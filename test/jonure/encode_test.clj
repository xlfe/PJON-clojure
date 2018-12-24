(ns jonure.encode-test
  (:require [clojure.test :refer :all]
            [jonure.core-test :as ct]
            [jonure.crc :as crc]
            [byte-streams]
            [jonure.packet :as packet]
            [org.clojars.smee.binary.core :as bin]
            [jonure.core :refer :all]))


(defn rand-bool
  [_]
  (first (shuffle [true false])))


(deftest test-packet-header-encode
  (testing "packet header encode / decode"
    (loop [i 0]
      (let [header (reduce-kv #(assoc %1 %2 (rand-bool %3)) {} packet/header-map)
            b (packet/pack-header header)
            u (packet/packet-header {:packet (byte-array [0 b])})]
            ;t (bin/decode packet/dest-header (java.io.ByteArrayInputStream. (byte-array [10 b])))
            ;v (bin/decode packet/binary-packet (java.io.ByteArrayInputStream. (byte-array [10 b 1 2 3 4 5 6])))]
        (is (= header (select-keys u (keys header)))))
        ;(is (= v {})))
      (if (> 100 i)
        (recur (inc i))))))

(deftest test-full-packet-decode
  (testing "full decode"
    (is (= {}
           (bin/decode packet/binary-packet (java.io.ByteArrayInputStream. ct/long-packet))))))