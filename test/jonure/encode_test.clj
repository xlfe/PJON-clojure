(ns jonure.encode-test
  (:require [clojure.test :refer :all]
            [jonure.core-test :as ct]
            [jonure.autogen-test :as ag]
            [jonure.crc :as crc]
            [byte-streams]
            [jonure.packet :as packet]
            [org.clojars.smee.binary.core :as bin]
            [jonure.core :refer :all]))


(defn rand-bool
  [_]
  (first (shuffle [true false])))


;(deftest test-packet-header-encode
;  (testing "packet header encode / decode"
;    (loop [i 0]
;      (let [header (reduce-kv #(assoc %1 %2 (rand-bool %3)) {} packet/header-map)
;            b (packet/pack-header header)
;            u (packet/packet-header {:packet (byte-array [0 b])}))
            ;t (bin/decode packet/dest-header (java.io.ByteArrayInputStream. (byte-array [10 b])))
            ;v (bin/decode packet/binary-packet (java.io.ByteArrayInputStream. (byte-array [10 b 1 2 3 4 5 6])))]
        ;(is (= header (select-keys u (keys header))))
        ;(is (= v {})))
      ;(if (> 100 i)
      ;  (recur (inc i))))

;(deftest test-full-packet-decode
;  (testing "full decode"
;    (is (= {}
;           (packet/parse-packet
                    ;ag/port_false-sync_ack_false-async_ack_false-tx_info_false-ext_len_false-crc32_false-packet_id_false-packet
                    ;ag/port_true-sync_ack_true-async_ack_true-tx_info_false-ext_len_false-crc32_true-packet_id_false-packet
                    ;ag/port_false-sync_ack_false-async_ack_true-tx_info_false-ext_len_false-crc32_true-packet_id_false-packet})))))))
                    ;ag/port_false-sync_ack_false-async_ack_false-tx_info_false-ext_len_false-crc32_true-packet_id_true-packet))))))
                    ;ag/port_true-sync_ack_true-async_ack_true-tx_info_true-ext_len_true-crc32_false-packet_id_true-packet})))))))
                    ;ct/long-packet}))))))))
