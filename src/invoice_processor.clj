(ns invoice-processor
  (:require [clojure.edn :as edn]))

;; PROBLEM # 1

(def invoice (edn/read-string (slurp "invoice.edn")))

(defn filter-invoice-items
  "Filter and iterates through the invoice/item key elements and receives validation as a parameter"
  [invoice predicate]
  (->> invoice
       :invoice/items
       (filter predicate)))

(defn has-iva-19?
  "Validate and obtain the items with iva equal to 19%"
  [item]
  (let [taxes (get item :taxable/taxes)]
    (and taxes
         (some #(and (= (:tax/category %) :iva) (= (:tax/rate %) 19)) taxes)
         (not (some #(and (= (:retention/category %) :ret_fuente) (= (:retention/rate %) 1))
                    (get item :retentionable/retentions))))))

(defn has-retention-1?
  "Validate and obtain the items with retention equal 1%"
  [item]
  (let [retentions (get item :retentionable/retentions)]
    (and retentions
         (some #(and (= (:retention/category %) :ret_fuente) (= (:retention/rate %) 1)) retentions)
         (not (some #(and (= (:tax/category %) :iva) (= (:tax/rate %) 19))
                    (get item :taxable/taxes))))))


(defn find-items
  "Function to find all the invoice items according the validation of iva and retention"
  [invoice]
  (filter-invoice-items invoice
                        #(or (has-iva-19? %)
                             (has-retention-1? %))))

;; Use
(def found-items (find-items invoice))
(print "found items:" found-items)
