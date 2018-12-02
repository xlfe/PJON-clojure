(ns jonure.util)

(defn bytes->num [data] (reduce bit-or (map-indexed (fn [i x] (bit-shift-left (bit-and x 0x0FF) (* 8 (- (count data) i 1)))) data)))

(defn copy-packet
 ([packet length] (copy-packet packet 0 length))
 ([packet start end] (byte-array (java.util.Arrays/copyOfRange packet start end))))


