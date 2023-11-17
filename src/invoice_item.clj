(ns invoice-item)
(use 'clojure.test)

(defn- discount-factor [{:invoice-item/keys [discount-rate]
                         :or                {discount-rate 0}}]
  (- 1 (/ discount-rate 100.0)))

(defn subtotal
  [{:invoice-item/keys [precise-quantity precise-price discount-rate]
    :as                item
    :or                {discount-rate 0}}]
  (* precise-price precise-quantity (discount-factor item)))


;; PROBLEM # 3
(defn create-invoice-item
  "function to define Item with quantity, price and discount rate"
  [precise-quantity precise-price discount-rate]
  {:invoice-item/precise-quantity precise-quantity
   :invoice-item/precise-price    precise-price
   :invoice-item/discount-rate    discount-rate})


(defn expected
  "function to calculate expected data result"
  [precise-quantity precise-price discount-rate]
  (double (* precise-quantity precise-price (- 1 (/ discount-rate 100.0)))))

;test cases
(deftest Subtotal-without-discount
  (let [item {:invoice-item/precise-quantity 10
              :invoice-item/precise-price    2.99}
        expected-result (expected 10 2.99 0)]
    (is (= expected-result (subtotal item)))))

(deftest Subtotal-with-discount-10-percent
  (let [item (create-invoice-item 10 2.99 10)
        expected-result (expected 10 2.99 10)]
    (is (= expected-result (subtotal item)))))


(deftest Subtotal-with-multiple-items-and-discount-rates
  "Test comparing subtotals of four distinct items with varying discount rates, quantities, and prices."
  (let [items [(create-invoice-item 10 2.99 10)
               (create-invoice-item 20 3.99 20)
               (create-invoice-item 30 4.99 30)
               (create-invoice-item 40 5.99 40)]
        expected-results [(expected 10 2.99 10)
                          (expected 20 3.99 20)
                          (expected 30 4.99 30)
                          (expected 40 5.99 40)]
        result-count (count items)]
    (doseq [i (range result-count)]
      (let [item (nth items i)
            expected-result (nth expected-results i)]
        (is (= expected-result (subtotal item)))))))

(deftest subtotal-with-multiple-items-and-without-discount-rates
  "Test comparing subtotals of three distinct items with the same price, varying quantities, and no discount rates, each created using the function create-item."
  (let [items [(create-invoice-item 10 2.99 0)
               (create-invoice-item 20 2.99 0)
               (create-invoice-item 30 2.99 0)]
        expected-results [(expected 10 2.99 0)
                          (expected 20 2.99 0)
                          (expected 30 2.99 0)]
        result-count (count items)]
    (doseq [i (range result-count)]
      (let [item (nth items i)
            expected-result (nth expected-results i)]
        (is (= expected-result (subtotal item)))))))


(deftest subtotal-with-equal-items-and-different-discount-rates
  "Test comparing subtotals of three identical items with different discount rates, each created using the function create-item."
  (let [items [(create-invoice-item 10 2.99 10)
               (create-invoice-item 10 2.99 20)
               (create-invoice-item 10 2.99 30)]
        expected-results [(expected 10 2.99 10)
                          (expected 10 2.99 20)
                          (expected 10 2.99 30)]
        result-count (count items)]
    (doseq [i (range result-count)]
      (let [item (nth items i)
            expected-result (nth expected-results i)]
        (is (= expected-result (subtotal item)))))))


(deftest subtotal-with-equal-items-and-same-discount-rates
  "Test with four identical items, each having a 10% discount rate, created using the function create-item. Compare the subtotals of these items with each other."
  (let [items [(create-invoice-item 10 2.99 10)
               (create-invoice-item 10 2.99 10)
               (create-invoice-item 10 2.99 10)
               (create-invoice-item 10 2.99 10)]
        expected-results [(expected 10 2.99 10)
                          (expected 10 2.99 10)
                          (expected 10 2.99 10)
                          (expected 10 2.99 10)]
        [firstItem secondItem thirdItem fourItem] items
        [firstResult secondResult thirdResult fourResult] expected-results]
    (is (= firstResult (subtotal firstItem)))
    (is (= secondResult (subtotal secondItem)))
    (is (= thirdResult (subtotal thirdItem)))
    (is (= fourResult (subtotal fourItem)))
    (is (= firstResult secondResult))
    (is (= firstResult thirdResult))
    (is (= firstResult fourResult))))


(run-tests 'invoice-item)