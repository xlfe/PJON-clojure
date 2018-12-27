(ns jonure.binary
  (:require
    [org.clojars.smee.binary.core :as bin]))



(defn progressive
  "Progressivley decode/encode a bytestream"
  [decodes encodes]
  ;{:pre [(= (count decodes) (count encodes))]}
  (reify bin/BinaryIO
    (read-data  [_ big-in little-in]
      (reduce #(merge %1 (bin/read-data (bin/compile-codec (%2 %1)) big-in little-in)) {} decodes))
    (write-data [_ big-out little-out value]
      (reduce #(merge %1 (bin/write-data (bin/compile-codec (%2 %1)) big-out little-out value)) value encodes))
      ;(let [body   (if keep-header? (:body value) value)
      ;      header (cond (and keep-header? body->header)
      ;                   (body->header (:header value) (:body value))
      ;                   keep-header? (:header value)
      ;                   :else        (body->header body)
      ;      body-codec (header->body-codec header)
      ;  (write-data header-codec big-out little-out header)
      ;  (write-data body-codec big-out little-out body)))
    Object (toString [_] (str "<BinaryIO header,codec=" decodes ">"))))

