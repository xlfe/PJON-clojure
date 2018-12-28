(ns jonure.packet
   (:require
     [byte-streams]
     [org.clojars.smee.binary.core :as bin]
     [jonure.util :as util]
     [jonure.binary :as jb]
     [jonure.crc :as crc]))

(def SYM_START (unchecked-byte 0x95)) ; -107 -> 46
(def SYM_END  (unchecked-byte 0xEA)) ; -22 -> 81
(def SYM_ESC (unchecked-byte 0xBB)) ; -69 -> 0

(defn header->packet-overhead
  [h]
  (+
    1 ;header
    1 ;header crc
    (if (:shared-mode h)
      (if (:tx-info h) 10 5)
      (if (:tx-info h) 2 1))
    (if (:ext-length h) 2 1)
    (if (:crc32 h) 4 1)
    (if (:port h) 2 0)
    (if (or (and (:tx-info h) (:async-ack h)) (:packet-id h)) 2 0)))


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


(defn header-start
 [_]
 (bin/ordered-map
   :receiver-id :ubyte
   :header (bin/bits (reverse header-bits))))


(defn header-rest
 [{:keys [header]}]
 (apply
   bin/ordered-map
   (concat
     [:packet-len (if (:ext-length header) :ushort :ubyte)
      :header-crc :ubyte]
     (if (:shared-mode header) [:receiver-bus-id (bin/repeated :byte :length 4)])
     (if (and (:shared-mode header) (:tx-info header)) [:sender-bus-id (bin/repeated :byte :length 4)])
     (if (:tx-info header) [:sender-id :ubyte])
     (if (or
           (and (:async-ack header) (:tx-info header))
           (:packet-id header))
         [:packet-id :ushort])
     (if (:port header) [:port :ushort]))))


(defn data-and-crc
 [{:keys [header packet-len]}]
 ;(println header packet-len (header->packet-overhead header))
 (bin/ordered-map
  :data (bin/repeated :ubyte :length (- packet-len (header->packet-overhead header)))
  :crc (if (:crc32 header) :uint :ubyte)))

(def packet-defn [header-start header-rest data-and-crc])

(defn decode-packet
 [data]
 (let [packet (bin/decode (jb/progressive packet-defn []) (java.io.ByteArrayInputStream. data))
       crc-len (if (:crc32 (:header packet)) 4 1)
       last-crc (util/bytes->num (take-last crc-len data))
       data-len (- (:packet-len packet) crc-len)
       computed (if (:crc32 (:header packet))
                    (crc/crc32-compute data data-len)
                    (crc/crc8-compute data data-len))]
   (assert (=
             (:header-crc packet)
             (crc/crc8-compute data (if (:ext-length (:header packet)) 4 3))))
   (if-not (= (:crc packet) computed)
     (println (str
                "\nparsed   crc: " (java.lang.Long/toHexString (:crc packet))
                "\ncomputed crc: " (java.lang.Long/toHexString computed)
                "\nbytes    crc: " (java.lang.Long/toHexString last-crc)
                "\ncrc-len     : " crc-len
                "\ndata-len    : " data-len)))
   (assert (= (:crc packet) computed))
   packet))




(defn escape-map
 [k v]
 (condp = v
   SYM_ESC (conj k SYM_ESC 0)
   SYM_START (conj k SYM_ESC 46)
   SYM_END (conj k SYM_ESC 81)
   (conj k v)))


(defn packet-escape
 [data]
 (byte-array (reduce escape-map [] data)))


(defn make-header
 [data]
 (apply (partial conj (:header data))
        (filter #(not (nil? %))
                [
                  (if (or
                        (contains? data :sender-bus-id)
                        (contains? data :receiver-bus-id)) :shared-mode)
                  (if (contains? data :sender-id) :tx-info)
                  (if (> (count (:data data)) 240) :ext-length)
                  (if (> (count (:data data)) 8) :crc32)
                  (if (contains? data :port) :port)
                  (if (contains? data :packet-id) :packet-id)])))

(defn make-packet
 [data]
 {:pre [(contains? data :header)
        (contains? data :receiver-id)
        (contains? data :data)]}
 (let [buffer (java.io.ByteArrayOutputStream.)
       data   (assoc data :header (make-header data))
       data   (assoc data :packet-len (+ (count (:data data)) (header->packet-overhead (:header data))))
       ;add placeholder crc values
       data   (assoc data :header-crc 0x00)
       data   (assoc data :crc 0x00000000)]

   (bin/encode (jb/progressive [] packet-defn) buffer data)
   (let [out-data (.toByteArray buffer)
         header-crc-pos  (if (:ext-length (:header data)) 4 3)
         header-crc (crc/crc8-compute out-data header-crc-pos)
         tail-crc-len (if (:crc32 (:header data)) 4 1)]
     (aset-byte out-data header-crc-pos (unchecked-byte header-crc))

     (let [tail-crc-start (- (count out-data) tail-crc-len)
           crc (if (:crc32 (:header data))
                   (crc/crc32-compute out-data tail-crc-start)
                   (crc/crc8-compute out-data tail-crc-start))
           crc-buf (java.io.ByteArrayOutputStream.)
           crc-encode (bin/encode (if (:crc32 (:header data)) :uint :ubyte) crc-buf crc)
           crc-bytes (.toByteArray crc-buf)]

       (java.lang.System/arraycopy crc-bytes 0 out-data tail-crc-start tail-crc-len)
       (packet-escape out-data)))))


(defn unescape-map
 [k v]
 (if (:esc k)
  {:val (conj (:val k) (bit-xor v SYM_ESC))}
  (if (= v SYM_ESC)
      (merge {:esc true} k)
      {:val (conj (:val k) v)})))


(defn packet-unescape
  [packet]
  (let [escaped (:val (reduce unescape-map {:val []} packet))]
    (byte-array escaped)))

(defn parse-packet
 [packet]
 (-> packet
   packet-unescape
   decode-packet))

(defn try-for-packet
 [p length]
 (if
   (and
     (< 5 length)
     (= (first p) SYM_START)
     (= (nth p length) SYM_END)
     (not (= (nth p (dec length)) SYM_ESC)))
   (try
     (parse-packet (util/copy-packet p 1 length))
     (catch java.io.EOFException e nil))
   nil))
