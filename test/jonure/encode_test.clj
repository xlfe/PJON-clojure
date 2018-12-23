(ns jonure.encode-test
  (:require [clojure.test :refer :all]
            [jonure.crc :as crc]
            [jonure.packet :as packet]
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
        (is (= header (select-keys u (keys header)))))
      (if (> 100 i)
        (recur (inc i))))))
