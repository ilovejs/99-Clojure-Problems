;https://clojuredocs.org/clojure.core/when

;(for seq-exprs body-expr)
(for [x [0 1 2 3 4 5]
      :let [y (* x 3)]
      :when (even? y)]
  y)


(take 100 (for [x (range 100000000)
                y (range 1000000)
                :while (< y x)]
            ;body expression
            [x y]))


;cartesion multiple
(def digits (seq [1 2 3]))

(for [x1 digits x2 digits] (* x1 x2))
;;=> (1 2 3 2 4 6 3 6 9)

; produce a seq of all pairs drawn from two vectors
(for [x ['a 'b 'c]
      y [1 2 3]]
  [x y])
;;=> ([a 1] [a 2] [a 3] [b 1] [b 2] [b 3] [c 1] [c 2] [c 3])



;; produce a seq of the first three powers for a range of integers
(for [x (range 1 6)
      :let [y (* x x)
            z (* x x x)]]
  [x y z])
;;=> ([1 1 1] [2 4 8] [3 9 27] [4 16 64] [5 25 125])

;; find value in tuple
(for [[x y] '([:a 1] [:b 2] [:c 0])
      :when (= y 0)]
  x)

;; Demonstrating performance difference between :when and :while
;; Check your console
(time (dorun (for [x (range 1000) y (range 10000) :when (> x y)] [x y])))
;; "Elapsed time: 2898.908 msecs"

(time (dorun (for [x (range 1000) y (range 10000) :while (> x y)] [x y])))
;; "Elapsed time: 293.677 msecs" !!!



;; Demonstrating functional difference between :when and :while

(for [x (range 3) y (range 3) :when (not= x y)] [x y])
;;=> ([0 1] [0 2] [1 0] [1 2] [2 0] [2 1])

(for [x (range 3) y (range 3) :while (not= x y)] [x y])
;;=> ([1 0] [2 0] [2 1])


;clojure slice
(subvec [1 2 3 4 5 6 7] 2)
;[3 4 5 6 7]

