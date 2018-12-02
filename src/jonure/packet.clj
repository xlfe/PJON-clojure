(ns jonure.packet
   (:require
     [byte-streams]
     [jonure.crc :as crc]))

(def SYM_START (unchecked-byte 0x95))
(def SYM_END  (unchecked-byte 0xEA))
(def SYM_ESC (unchecked-byte 0xBB))

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
    [:packet-id
     :ext-length
     :crc32
     :port
     :async-ack
     :ack
     :tx-info
     :shared-mode])

(defn packet-destination
 [packet]
 (merge packet {:dest-id (bytes->num [(nth (:packet packet) 0)])}))

(defn packet-header
  " Parse the first byte of the PJON packet"
  [packet]
  (let [header-map (zipmap header-bits (range 8))
        h (aget (:packet packet) 1)]
    (reduce-kv #(assoc %1 %2 (bit-test h (- 7 %3))) packet header-map)))

(defn packet-data-len
 [packet]
 (let [{:keys [ext-length packet-overhead]} packet
       data-len (if ext-length
                    (bytes->num (take-last 2 (take 4 (:packet packet))))
                    (bytes->num [(aget (:packet packet) 2)]))]
   (merge packet {:data-len (- data-len packet-overhead)})))



(defn packet-unescape
 [packet]
 (let [p (:packet packet)
       len (alength p)]
   (loop [src 0
          dst 0]
     (if
       (= src len)
       {:packet-len dst :packet (copy-packet p dst)}
       (if
         (=
           (nth p src)
           SYM_ESC)
         (do
           (aset-byte p dst (bit-xor (nth p (+ src 1)) SYM_ESC))
           (recur (+ src 2) (inc dst)))
         (do
           (if (not (= dst src))
             (aset-byte p dst (nth p src)))
           (recur (inc src) (inc dst))))))))

(defn packet-header-crc
 [packet]
 (let [p (:packet packet)
       meta-len (if (:ext-length packet) 4 3)
       packet-meta (copy-packet p meta-len)
       packet-crc (bytes->num [(nth p meta-len)])
       computed-crc (crc/crc8-compute packet-meta)]

   (if (not (= packet-crc computed-crc))
     (println (str
                "\npacket CRC " packet-crc " " (java.lang.Long/toString packet-crc) 16
                "\ncomputed CRC " computed-crc " " (java.lang.Long/toString computed-crc) 16
                "\nmeta-len " meta-len)))

   (merge packet {:header-crc-ok (= computed-crc packet-crc)})))

(defn packet-full-crc
 [packet]
 (let [p (:packet packet)
       crc-len (if (:crc32 packet) 4 1)
       data-len (- (alength p) crc-len)
       crc (bytes->num (take-last crc-len p))
       computed (if (:crc32 packet)
                    (crc/crc32-compute p data-len)
                    (crc/crc8-compute p data-len))]
   (if (not (= crc computed))
     (println (str
                "\npacket CRC " crc " " (java.lang.Long/toString crc) 16
                "\ncomputed CRC " computed " " (java.lang.Long/toString computed) 16
                "\ncrc-len " crc-len
                "\ndata-len " data-len)))

   (merge packet
          {:packet-crc-ok  (= crc computed)})))


(defn packet-tx-info
  [packet]
  (if (:tx-info packet)
    (let [tx-offset (if (:ext-length packet) 5 4)
          source-id (bytes->num [(aget (:packet packet) tx-offset)])]
      (merge packet {:source-id source-id}))
    packet))

(defn get-mid
  [data start len]
  (take-last len (take (+ start len) data)))


(defn packet-id
  "The packet will contain a packet ID if the PACKET_ID bit is set, or if both tx-info and async-ack are set"
 [packet]
 (if (or (:packet-id packet) (and (:tx-info packet) (:async-ack packet)))
   (let [packet-overhead (:packet-overhead packet)
         start (- packet-overhead 2 (if (:crc32 packet) 4 1) (if (:port packet) 2 0))
         packet-id      (bytes->num (get-mid (:packet packet) start 2))]
     (merge packet {:packet-id packet-id}))
   packet))

(defn packet-port
  [packet]
  (if (:port packet)
    (let [packet-overhead (:packet-overhead packet)
          offset (- packet-overhead 2 (if (:crc32 packet) 4 1))
          port (bytes->num (get-mid (:packet packet) offset 2))]
      (merge packet {:port port}))
    packet))


(defn packet-overhead
 [h]
 (merge h {:packet-overhead
           (+
             1 ;header
             1 ;header crc
             (if (true? (:shared-mode h))
               (if (true? (:tx-info h)) 10 5)
               (if (true? (:tx-info h)) 2 1))
             (if (true? (:ext-length h)) 2 1)
             (if (true? (:crc32 h)) 4 1)
             (if (true? (:port h)) 2 0)
             (if
               (or
                 (and (true? (:tx-info h)) (true? (:async-ack h)))
                 (true? (:packet-id h))) 2 0))}))



(defn parse-packet
 [packet]
 (->
   {:packet packet}
   packet-unescape

   packet-destination
   packet-header
   packet-overhead
   packet-header-crc

   packet-id
   packet-data-len
   packet-full-crc
   packet-port
   ;ack
   packet-tx-info)) ;tx-info
   ;shared-mode


(defn check-for-packet
 [p length]
 (if
   (and
     (< 5 length)
     (= (first p) SYM_START)
     (= (nth p length) SYM_END)
     (not (= (nth p (dec length)) SYM_ESC)))
   (parse-packet (copy-packet p 1 length))
   nil))
