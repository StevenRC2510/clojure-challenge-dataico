;;PROBLEM # 2

(ns invoice-validation
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s ]
            [clojure.string :as string]
            [invoice-spec :as test]))

(use 'clojure.instant)

(defn date-to-string
  "Function to transform vector date to string"
  [date-vec]
  (str (nth date-vec 2) "-" (nth date-vec 1) "-" (nth date-vec 0)))

(defn transform-date
  "Function to Transform date"
  [date-str]
  (let [date-vec (mapv #(Integer/parseInt %) (string/split date-str #"/"))]
    (date-to-string date-vec)))

(defn read-json-file
  "Function to Read files"
  [path]
  (with-open [reader (io/reader path)]
    (json/read reader :key-fn keyword)))

(defn validate-invoice [invoice-json]
  (let [data (read-json-file invoice-json)
        invoice (:invoice data)
        customer (:customer invoice)
        customer-name (str (:company_name customer))
        customer-email (str (:email customer))
        issue-date (:issue_date invoice)
        items (let [price (:price (nth (:items invoice) 0))
                    quantity (:quantity (nth (:items invoice) 0))
                    sku (:sku (nth (:items invoice) 0))
                    taxes (:taxes (nth (:items invoice) 0))
                    rate (:tax_rate (first taxes))]
                [{:invoice-item/price (double price)
                  :invoice-item/quantity (double quantity)
                  :invoice-item/sku (str sku)
                  :invoice-item/taxes  [{:tax/category :iva
                                         :tax/rate (double rate)}]}])
        result {:invoice/issue-date (read-instant-date (transform-date issue-date))
                :invoice/customer {:customer/name customer-name
                                   :customer/email customer-email}
                :invoice/items items}] (print (s/valid? ::test/invoice result))))

;; Invoice json data
(def invoice-data "invoice.json")
;; call the validation invoice function
 (validate-invoice invoice-data)