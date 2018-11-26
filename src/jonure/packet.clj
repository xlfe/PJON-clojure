(ns jonure.packet
   (:require
     [byte-streams]
     [jonure.crc :as crc]))

(def SYM_START 0x95)
(def SYM_END 0xEA)
(def SYM_ESC 0xBB)

(defn bytes->num [data] (reduce bit-or (map-indexed (fn [i x] (bit-shift-left (bit-and x 0x0FF) (* 8 (- (count data) i 1)))) data)))

(defn copy-packet
 ([packet length] (copy-packet packet 0 length))
 ([packet start end] (byte-array (java.util.Arrays/copyOfRange packet start end))))

(def header-bits
  "
  Header spec v3.0 :-

  (note - 1 is MSB, 8 is LSB)

  1. PACKET ID bit informs if the packet contains (value 1) or not (value 0) a 2 bytes packet id
  2. EXT. LENGTH bit informs if the packet contains 1 (value 0) or 2 (value 1) bytes length
  3. CRC bit signals which CRC is used, CRC8 (value 0) or CRC32 (value 1)
  4. PORT bit informs if the packet contains a 2 bytes network service identifier (value 1) or not (value 0)
  5. ACK MODE bit signals synchronous (value 0) or asynchronous (value 1) acknowledgement mode
  6. ACK bit informs if acknowledgement is requested (value 1) or not (value 0)
  7. TX INFO bit informs if the sender info are included (value 1) or not (value 0)
  8. MODE bit informs if the packet is formatted in shared (value 1) or local mode (value 0)
 "
    [:has_packet_id
     :has_ext_length
     :has_crc32
     :has_port
     :async_ack
     :ack_requested
     :has_tx_info
     :mode_shared])

(defn packet-destination
 [packet]
 (merge {:dest_id (unchecked-byte (nth (:packet packet) 0))} packet))

(defn packet-header
  " Parse the first byte of the PJON packet"
  [packet]
  (let [header-map (zipmap header-bits (range 8))
        h (aget (:packet packet) 1)]
    (reduce-kv #(assoc %1 %2 (bit-test h (- 7 %3))) packet header-map)))

(defn packet-data-len
 [packet]
 (merge {:packet_len (if (:has_ext_length packet)
                      (bytes->num (take-last 2 (take 4 (:packet packet))))
                      (bytes->num [(aget (:packet packet) 2)]))}
        packet))



(defn packet-unescape
 [packet]
 (let [p (:packet packet)
       len (alength p)]
   (loop [src 0
          dst 0]
     (if
       (= src len)
       {:length dst :packet (copy-packet p dst)}
       (if
         (=
           (nth p src)
           (unchecked-byte SYM_ESC))
         (do
           (aset-byte p dst (unchecked-byte (bit-xor (nth p (+ src 1)) SYM_ESC)))
           (recur (+ src 2) (inc dst)))
         (do
           (if (not (= dst src))
             (aset-byte p dst (nth p src)))
           (recur (inc src) (inc dst))))))))

(defn packet-header-crc
 [packet]
 (let [p (:packet packet)
       meta-len (if (:has_ext_length packet) 4 3)
       packet-meta (copy-packet p meta-len)
       packet-crc (nth p meta-len)
       computed-crc (crc/crc8-compute packet-meta)]
   (merge packet {:header_crc_ok (= computed-crc packet-crc)})))

(defn packet-full-crc
 [packet]
 (let [p (:packet packet)
       crc-len (if (:has_crc32 packet) 4 1)
       data-len (- (alength p) crc-len)
       crc (bytes->num (take-last crc-len p))
       computed (crc/crc32-compute p data-len)]
   (if (not (= crc computed))
     (println (str
                "\npacket CRC " crc " " (java.lang.Long/toString (unchecked-long crc) 16)
                "\ncomputed CRC " computed " " (java.lang.Long/toString (unchecked-long computed) 16)
                "\ndata-len " data-len)))

   (merge packet {:packet_crc_ok (= crc computed)})))

(defn parse-packet
 [packet]
 (->
   {:packet packet}
   packet-unescape
   packet-header
   packet-destination
   packet-data-len
   packet-header-crc
   packet-full-crc))

(defn check-for-packet
 [p length]
 (if
   (and
     (< 5 length)
     (= (first p) (unchecked-byte SYM_START))
     (= (nth p length) (unchecked-byte SYM_END))
     (not (= (nth p (dec length)) (unchecked-byte SYM_ESC))))
   (parse-packet (copy-packet p 1 length))
   nil))
