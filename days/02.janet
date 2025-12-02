(defn- make-range [from to] {:from from :to to})

(def- product-id-range-grammar (peg/compile 
  ~{:main (* (some :product-range) "\n")
    :product-range (* (/ (* :product-id "-" :product-id) ,make-range) (? ","))
    :product-id (/ (<- :d+) ,scan-number)}))

(defn- is-id-part1-valid? [id]
  (let [id-str (string id) id-str-len (length id-str)]
    (and 
      (even? id-str-len) 
      (= 
        (string/slice id-str 0 (div id-str-len 2)) 
        (string/slice id-str (div id-str-len 2))))))

(defn- find-divisors [n]
  (filter |(int? (/ n $0)) (range n)))

(defn- chunk [str chunk-size]
  [;(seq [i :range [0 (div (length str) chunk-size)]] 
      (string/slice str (* i chunk-size) (* (+ i 1) chunk-size)))])

(defn- is-id-part2-valid? [id]
  (let [id-str (string id) id-str-len (length id-str)]
    (any? (map |(apply = (chunk id-str $0)) (find-divisors id-str-len)))))

(defn- iterate-product-id-ranges [product-id-ranges]
  (generate [id-range :in product-id-ranges id :range-to [(id-range :from) (id-range :to)]] id))

(with [f (file/open (get (dyn :args) 1))]
  (let [input (file/read f :all) product-id-ranges (peg/match product-id-range-grammar input)]
    (->>
      (iterate-product-id-ranges product-id-ranges)
      (filter is-id-part1-valid?)
      (sum)
      (printf "day 01 part 1: %d"))

    (->>
      (iterate-product-id-ranges product-id-ranges)
      (filter is-id-part2-valid?)
      (sum)
      (printf "day 01 part 2: %d"))))
