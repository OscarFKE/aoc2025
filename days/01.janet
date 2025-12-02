(defn- make-left [rotation-str] (- (scan-number rotation-str)))

(defn- make-right [rotation-str] (scan-number rotation-str))

(def- lock-rotation-grammar (peg/compile
  ~{:main (some :rotation)
    :rotation (* (+ :left :right) :newline?)
    :left (* "L" (/ (<- :d+) ,make-left))
    :right (* "R" (/ (<- :d+) ,make-right))
    :newline? (? "\n")}))

(defn- normalize-dial-position [dial-position]
  (% (if (pos? dial-position) dial-position (- 100 (% (math/abs dial-position) 100))) 100))

(defn- apply-rotations [starting-position rotations]
  (var dial-position starting-position)
  (seq [r :in rotations]
    (set dial-position (normalize-dial-position (+ dial-position r)))))

(defn- iterate-clicks [starting-position ending-position]
  # omit the starting position to avoid double counting zeros
  (let [start (min (+ starting-position 1) ending-position) 
        end (max (- starting-position 1) ending-position)]
    (seq [position :range-to [start end]]
      (normalize-dial-position position))))

(defn- apply-rotations-count-zeros [starting-position rotations]
  (var dial-position starting-position)
  (seq [r :in rotations]
    (let [prev-dial-position dial-position new-dial-position (+ dial-position r)]
      (set dial-position (normalize-dial-position new-dial-position))
      (sum (map |(if (zero? $0) 1 0) (iterate-clicks prev-dial-position new-dial-position))))))

(with [f (file/open (get (dyn :args) 1))]
  (let [input (file/read f :all) rotations (peg/match lock-rotation-grammar input)]
    (->> 
      (apply-rotations 50 rotations)
      (filter zero?)
      (length)
      (printf "day 01 part 1: %d"))

    (->>
      (apply-rotations-count-zeros 50 rotations)
      (sum)
      (printf "day 01 part 2: %d"))))
