(ns jonure.core
    (:require
        [clojure.core.async :as async]
        [byte-streams]
        [serial.util]
        [jonure.packet :as packet]
        [java-time]
        [serial.core]))

(def TIME_OUT 50)
(def BYTE_TIME_OUT (java-time/millis TIME_OUT))
(def BUF_SIZE 512)

(def serial-port "/dev/tty.wchusbserial1410")

(defn make-handler [c]
  (fn [stream]
    (do
      (async/put! c
                  (try
                    [(java-time/instant) (.read stream)]
                    (catch java.io.IOException e [e nil]))))))

(defn open-port
 [serial-port c]
 (let [port (serial.core/open serial-port)]
  (serial.core/listen! port (make-handler c))
  port))

(defn has-timed-out
 [inst timeout]
 (java-time/after? (java-time/instant) (java-time/plus inst timeout)))



(defn- triage
 [c outgoing]
 (alt!
  (async/timeout TIME_OUT) :timeout
  c ([received] {:byte received})))

(defn- send-packet
 [port out-packet]
 nil)

(defn- wait-for-byte-or-outgoing
 [c outgoing port]
 (async/alt!
  (async/timeout TIME_OUT) nil
  c ([received] {:byte received})
  outgoing ([out-packet]) (send-packet port out-packet)))

(defn- wait-for-byte
 [c]
 (async/alt!
  (async/timeout TIME_OUT) :timeout
  c ([received] {:byte received})))

(defn- transport-loop
 [serial-port incoming outgoing]
 (let [c (async/chan)
       port (open-port serial-port c)
       B (byte-array BUF_SIZE)]
   (async/go-loop
     [i 0]
     (if-let [packet (packet/try-for-packet B i)]
       (do
         (serial.core/write port (byte 6))
         (async/put! incoming {:packet packet})
         (recur 0)))
     (if-let [result (if (= i 0)
                         (wait-for-byte-or-outgoing c outgoing port)
                         (wait-for-byte c))]
       (do
         (aset-byte B i (unchecked-byte (:byte result)))
         (recur (inc i)))
       (recur 0)))))




(defn create-channels
  [serial-port]
  (let [packets-received (async/chan)
        packets-to-send (async/chan)
        c (async/chan)
        port (open-port serial-port c)
        B (byte-array BUF_SIZE)]
    (async/go-loop
      [i 0
       prev-received (java-time/instant)
       [received-ts received-byte] (async/<!! c)]
      (if (= i 0)
        (java.util.Arrays/fill B (byte 0)))

      (if (nil? received-byte)
        (async/put! packets-received {:error received-ts} (fn [ret] (do (async/close! packets-received) (async/close! packets-to-send))))
        (if (has-timed-out prev-received BYTE_TIME_OUT)
          (do
            ;timeout - check for outgoing packets
            (recur 0 (java-time/instant) [received-ts received-byte]))
          (do
            (aset-byte B i (unchecked-byte received-byte))
            (let [packet (packet/try-for-packet B i)]
              (if
                 (or
                   (= (- BUF_SIZE 1) i)
                   (not (nil? packet)))
                 (do
                  (serial.core/write port (byte 6))
                  (async/put! packets-received {:packet packet})
                  ;packet received - check for outgoing
                  (recur 0 received-ts (async/<!! c)))
                 ;packet reception in progress - don't check for outgoing
                 (recur (inc i) received-ts (async/<!! c))))))))
    [packets-received packets-to-send]))


(defn -main
  []
  (let [[incoming outgoing] (create-channels serial-port)]
    (async/<!! (async/go-loop
                 [packet (async/<!! incoming)]
                 (println packet)
                 (if (contains? packet :packet)
                   (do
                     (byte-streams/print-bytes (:data (:packet packet)))
                     (recur (async/<!! incoming))))))))
