(ns PJON.core
    (:require
        [clojure.core.async :as async]
        [byte-streams]
        [serial.util]
        [PJON.packet :as packet]
        [java-time]
        [serial.core]))

(def TIME_OUT 50) ;ms
(def BUF_SIZE 512)

(def serial-port "/dev/tty.wchusbserial1410")

(defn make-handler [c]
  (fn [stream]
    (do
      (async/put! c
                  (try
                    {:byte (.read stream)}
                    (catch java.io.IOException e {:error e}))))))

(defn open-port
 [serial-port c]
 (let [port (serial.core/open serial-port)]
  (serial.core/listen! port (make-handler c))
  port))

(defn- send-packet
 [port out-packet]
 (let [data (packet/make-packet out-packet)
       escaped (byte-array (concat [packet/SYM_START] data [packet/SYM_END]))]
   (byte-streams/print-bytes escaped)
   (serial.core/write port escaped))
 nil)

(defn- wait-for-byte-or-outgoing
 [c outgoing port]
 (async/<!!
   (async/go
     (async/alt!
      c ([_] _)
      outgoing ([out-packet] (send-packet port out-packet))))))

(defn- wait-for-byte
 [c]
 (async/<!!
   (async/go
     (async/alt!
      (async/timeout TIME_OUT) nil
      c ([_] _)))))

(defn- transport-loop
 [serial-port incoming outgoing]
 (let [c (async/chan)
       port (open-port serial-port c)
       B (byte-array BUF_SIZE)]
   (async/go-loop
     [i 0]

     (if (= i BUF_SIZE)
       (recur 0)
       (if-let [packet (packet/try-for-packet B i)]

         ;valid packet
         (do
           (serial.core/write port (byte 6))
           (async/put! incoming packet)
           (recur 0))

         ;not valid packet
         (if-let [result (if (= i 0)
                             (wait-for-byte-or-outgoing c outgoing port)
                             (wait-for-byte c))]

           (if (contains? result :byte)

             ;byte received
             (do
               (println (str "Byte received: " (:byte result)))
               (aset-byte B i (unchecked-byte (:byte result)))
               (recur (inc i)))

             ;error
             (do
               (async/put! incoming result)
               (async/close! incoming)))

           ;no byte received
           (recur 0)))))))


(defn -main
  []
  (let [incoming (async/chan)
        outgoing (async/chan)]
    (async/<!! (async/go
                 (transport-loop serial-port incoming outgoing)
                 (loop
                   [packet (async/<!! incoming)]
                   (println packet)
                   (if (contains? packet :data)
                     (do
                       (byte-streams/print-bytes (byte-array (:data packet)))
                       (async/put! outgoing {:receiver-id 11 :sender-id 10 :header #{:ack :tx-info} :data (byte-array [1 2 3 4 5 6 7 8 9 10])})
                       (recur (async/<!! incoming)))
                     (println (str "ERROR: " (:error packet)))))))))
