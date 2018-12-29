(ns PJON.core
    (:require
        [clojure.core.async :as async]
        [byte-streams]
        [serial.util]
        [PJON.packet :as packet]
        [java-time]
        [serial.core]))

(defn make-handler [c]
  (fn [stream]
      (try
        (async/put! c {:byte  (.read stream)})
        (catch java.io.IOException e {:error e}))))

(defn open-port
 [serial-port c]
 (let [port (serial.core/open serial-port)]
  (serial.core/listen! port (make-handler c))
  port))

(defn- send-packet
 [port out-packet]
 (let [data (packet/make-packet out-packet)
       escaped (byte-array (concat [packet/SYM_START] data [packet/SYM_END]))]
   (serial.core/write port escaped))
 (if (:ack (:header out-packet))
   {:sent out-packet}
   nil))

(defn- wait-for-byte-or-outgoing
 [c outgoing port]
 (async/<!!
   (async/go
     (async/alt!
      c ([_] _)
      outgoing ([out-packet] (send-packet port out-packet))))))

(defn- wait-for-byte
 [c timeout-ms]
 (async/<!!
   (async/go
     (async/alt!
      (async/timeout timeout-ms) nil
      c ([_] _)))))

(defn transport-loop
  "
  Three modes -
    :nil waiting for incoming bytes or outgoing packet
    :in - incoming bytes
    :out - packet sending

  "
 [serial-port
  incoming
  outgoing
  & {:keys [
            ts-response-time-out-ms
            ts-byte-time-out-ms
            ack-retry
            packet-max-length]
     :or {ts-response-time-out-ms 10
          ts-byte-time-out-ms 50
          ack-retry 10
          packet-max-length 50}}]

 (let [c (async/chan)
       port (open-port serial-port c)
       BUF_SIZE (* 4 packet-max-length)
       B (byte-array BUF_SIZE)]
   (async/go-loop
     [i 0
      modifier nil]

     (if (= i BUF_SIZE)

       ;buffer exceeded - reset
       (recur 0 nil)

       ; i < BUF_SIZE - continue
       (if-let [packet (packet/try-for-packet B i)]

         ;valid packet received
         (do
           (if (:ack (:header packet)) ;send ack
             (serial.core/write port (byte 6)))
           (async/put! incoming packet)
           (recur 0 nil))

         ;not valid packet - try to read data
         (if-let [result (if (= i 0)
                             ;only allow sending a packet when we're not already receiving
                             (wait-for-byte-or-outgoing c outgoing port)
                             (wait-for-byte c (if (nil? modifier) ts-byte-time-out-ms ts-response-time-out-ms)))]

           (condp #(contains? %2 %1) result

             ; packet sent which requires an ack
             :sent (do
                     ;(println "Packet sent - entering ack mode")
                     (recur 1 (:sent result)))

             ; byte received
             :byte (if (nil? modifier)

                     ; normal byte receiption - put it onto the buffer
                      (do
                        (aset-byte B i (unchecked-byte (:byte result)))
                        (recur (inc i) nil))

                      ; modifier - waiting for an ack
                      (if (= (byte 6) (:byte result))
                        (do
                          ;(println "Ack received")
                          (recur 0 nil))

                        (if (>= ack-retry i)
                          (do
                            ;(println "resending " i " " (:byte result) (char (:byte result)))
                            (async/<!! (async/timeout (* i 100)))
                            (send-packet port modifier)
                            (recur (inc i) modifier))
                          (do
                            (println "failed to receive ack")
                            (recur 0 nil)))))

             :error (do
                       (async/put! incoming result)
                       (async/close! incoming)))

           (if (nil? modifier)
             (recur 0 nil)
             (recur (inc i) modifier))))))))


;(defn -main
;  []
;  (let [incoming (async/chan)
;        outgoing (async/chan)))
;    (async/<!! (async/go
;                 (transport-loop serial-port incoming outgoing)
;                 (loop
;                   [packet (async/<!! incoming)]
;                   (println packet)
;                   (if (contains? packet :data)
;                     (do
;                       (byte-streams/print-bytes (byte-array (:data packet)))
;                       (async/put! outgoing {:receiver-id 11 :sender-id 10 :header #{:ack :tx-info} :data (byte-array [1 2 3 4 5 6 7 8 9 10])})
;                       (recur (async/<!! incoming))
;                     (println (str "ERROR: " (:error packet)))))))))))
