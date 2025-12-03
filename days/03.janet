(def- battery-bank-grammar (peg/compile
    ~{:main (* (some :bank))
      :bank (* (group (some :battery)) :newline?)
      :battery (/ (<- :d) ,scan-number)
      :newline? (? "\n")}))

(defn- update-joltage-span [span new-element]
  (label update
    (do
      (loop [i :range [0 (- (length span) 1)]]
        (when (< (get span i) (get span (+ i 1)))
          (return update [;(take i span) ;(take (- (length span) (+ i 1)) (drop (+ i 1) span)) new-element])
          ))

      (if (< (last span) new-element)
        [;(take (- (length span) 1) span) new-element]
        span))))

(defn- sum-joltage-span [span]
  (var accum 0)
  (loop [i :range [0 (length span)]]
    (set 
      accum
      (+ accum (* (get span i) (math/pow 10 (- (length span) (+ i 1)))))))
  accum)

(defn- collect-highest-joltage [span-size bank]
  (->
    (reduce update-joltage-span [;(take span-size bank)] (drop span-size bank))
    (sum-joltage-span)))

(with [f (file/open (get (dyn :args) 1))]
  (let [input (file/read f :all) battery-banks (peg/match battery-bank-grammar input)]
    (->> 
      battery-banks 
      (map |(collect-highest-joltage 2 $0)) 
      (sum) 
      (printf "day 03 part 1: %d"))

    (->>
      battery-banks
      (map |(collect-highest-joltage 12 $0))
      (sum)
      (printf "day 03 part 2: %d"))
    ))
