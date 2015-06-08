;; P21 (*) Insert an element at a given position into a list.
;; Example:
;; scala> insertAt('new', 1, List('a, 'b, 'c, 'd))

;; res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
(defn insertAt [element pos lst]
  (assoc lst pos element))

;replace
(assoc [1 2 3] 1 22)

;replace
(insertAt "new" 1 (vec '(\a \b \c \d)))


(defn insertAt-2 [element pos lst]
  (let [array (split-at pos lst)]
    (apply
       conj (second array) element (first array))))

;my version
(defn insertAt-2 [element pos lst]
  (let [[head tail] (split-at pos lst)]
    (concat head            ;since (\a)
            (cons element   ;here is the magic !!!!
            tail))))

(insertAt-2 "new" 3 '(\a \b \c \d))

;snippet

;concat string will turn it into char array
(concat [1 2] [3] [89] "lp" '(8 9))

;a k will be seperated
(concat "ak" '(\b \c))

;new structure only relates to underlying data structure
(conj '(\b \c) "new" '(\a))

;act like :: in haskell  cons elem tail
(cons "ak" '(\b \c))

;someone's solution using P17 split.
(defn split [n lst]
  (list
    (take n lst)
    (drop n lst)))

(defn insert-at [elem pos xs]
  "P21 (*) Insert an element at a given position into a list."
  (let [[head tail] (split pos xs)]    ;highlight, this make code more clean
      (print head)
      (concat head (cons elem tail))))



(insert-at "new" 3 '(\a \b \c \d))

(concat 1 2 3 4 5)

(split 3 (char-array "abcd"))

(split-at 3 (char-array "abcd"))
;=> [(\a) (\b \c)]



;P22 range
(defn my-range [start end]
  (loop [s  start
         e  end
         acc []]
    (if (> s e)
         acc
        (recur (inc s) e (conj acc s)))))

(my-range 1 5)

(take 3 (repeat 3 1))


;P23 random select 3 elements from a list.

;include library
;scala> removeAt(1, List('a, 'b, 'c, 'd))
;res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

(defn removeAt [i lst]
  (let [that (nth lst i)]
    (list
      (filter (fn [x]
               (if-not (= x that)
                 true
                 false)) lst)
     that)))

;wrong version, TODO: pass r out of loop's scope, doable ?
(defn randomSelect [n lst]
  (let [r []]
   (loop [t n
         cur lst]
    (if (> t 0)
      (let [index   (rand-int (dec (count cur)))
            [fr sr] (removeAt index cur)]
        (recur (dec t) fr)))) r))

;standard version
(defn randomSelect [n lst]
   (loop [t n
          cur lst
          r []]
    (if (= t 0)
      r
      (let [index   (rand-int (dec (count cur)))
            [fr sr] (removeAt index cur)]
        (println index fr " | " sr)
        (recur (dec t) fr (conj r sr))))))

;return a vector of randomly removed items
(randomSelect 3 (char-array "abcdefg"))

;test
(rand-int 7)


;P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;Example:
;scala> lotto(6, 49)
;res0: List[Int] = List(23, 1, 17, 33, 21, 37)


