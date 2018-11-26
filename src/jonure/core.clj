(ns jonure.core
    (:require
        [clojure.core.async :as async]
        [byte-streams]
        [serial.util]
        [jonure.packet :as packet]
        [java-time]
        [serial.core]))

(def BYTE_TIME_OUT (java-time/millis 50))
(def BUF_SIZE 512)

(def serial-port "/dev/tty.wchusbserial1410")

(defn make-handler [c] (fn [stream] (do (async/put! c [(java-time/instant) (.read stream)]))))

(defn open-port
 [serial-port c]
 (let [port (serial.core/open serial-port)]
  (serial.core/listen! port (make-handler c))))

(defn has-timed-out
 [inst timeout]
 (java-time/after? (java-time/instant) (java-time/plus inst timeout)))


(defn -main
  []
  (async/go []
    (let [c (async/chan)
          buf-len BUF_SIZE
          port (open-port serial-port c)
          B (byte-array buf-len)]
      (loop [i 0
             prev-received (java-time/instant)
             [received-ts received-byte] (async/<!! c)]

        (if (= i 0)
          (java.util.Arrays/fill B (byte 0)))

        (if
          (has-timed-out prev-received BYTE_TIME_OUT)
          (recur 0 (java-time/instant) [received-ts received-byte])
          (do
            (aset-byte B i (unchecked-byte received-byte))
            (let [packet (packet/check-for-packet B i)]
              (if
                 (or
                   (= (- BUF_SIZE 1) i)
                   (not (nil? packet)))
                 (do
                  (byte-streams/print-bytes B)
                  (println packet)
                  (recur 0 received-ts (async/<!! c)))
                 (recur (inc i) received-ts (async/<!! c)))))))))

  (async/<!! (async/go-loop [n 0]
                          (async/<! (async/timeout 10000))
                          (recur (inc n)))))
