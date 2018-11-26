(ns jonure.crc
  (:require
    [clojure.test]))

; Original implementation Copyright Giovanni Blu Mitolo giorscarab@gmail.com 2018 from PJON

(defn calc-crc-bit
 [crc _byte]
 (let [result (bit-and (bit-xor crc _byte) 0x01)
       crc (bit-shift-right crc 1)]
  (if
    (= 0 result)
    crc
    (bit-xor crc 0x97))))


(defn crc8-roll
 [input_byte start_crc]
 (loop [i 8
        crc start_crc
        _byte input_byte]
  (if (= 0 i)
   crc
   (recur
     (dec i)
     (calc-crc-bit crc _byte)
     (bit-shift-right _byte 1)))))

(defn crc32-roll
 [crc]
 (loop [i 0
        crc crc]
    (if (= i 8)
      crc
      (recur (inc i) (if (bit-test crc 0)
                         (bit-xor (bit-shift-right crc 1) 0xEDB88320)
                         (bit-shift-right crc 1))))))

(defn crc8-compute
 ([input_bytes] (crc8-compute input_bytes (alength input_bytes)))
 ([input_bytes length]
  (loop [crc 0
         i 0]
    (if (= i length)
      (unchecked-byte crc)
      (recur
        (crc8-roll (aget input_bytes i) crc)
        (inc i))))))



(defn crc32-compute
  ([input_bytes] (crc32-compute input_bytes (alength input_bytes) 0))
  ([input_bytes length] (crc32-compute input_bytes length 0))
  ([input_bytes length previous_crc]
   (loop [x 0
          crc (bit-xor (long previous_crc) 0xFFFFFFFF)]
     ;(if (< 200 length)
     ;    (println (str "compute round " x " crc " crc))
     (if (= x length)
       (bit-xor crc 0xFFFFFFFF)
       (recur (inc x) (crc32-roll (bit-xor crc (bit-and (aget input_bytes x) 0xff))))))))



