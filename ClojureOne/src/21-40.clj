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

(defn lotto [n limit]
  (loop [i n
         r []]
    (if (= 0 i)
      r
      (recur (dec i) (conj r (rand-int limit))))))

;TODO: any fuzzy test tool for clojure?
(lotto 6 49)


;; P25 (*) Generate a random permutation of the elements of a list.
;; Hint: Use the solution of problem P23.
;; Example:

;; scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
;; res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

;use randomSelect function in P23
(defn randomPermute [lst]
  (randomSelect (count lst) lst))

;Smart move !! See the power of functional pattern ?
;element won't be taken twice.
(randomPermute (char-array "abcdef"))


;; P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
;; In how many ways can a committee of 3 be chosen from a group of 12 people?
;; We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
;; For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
;; Example:

;; scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
;; res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

;Integer to binary, length not fixed
(defn int2bin [i]
  (str (Integer/toBinaryString i)))
;2 returns 11

(int2bin 3)

(require '[clojure.pprint :refer (cl-format)])

;len = 6, number = 2
(cl-format nil "2r~6,'0',B" 2)

;Int to binary with given length
(defn int2binF [len n]
  (clojure.pprint/cl-format nil (str "~" len ",'0',B") n))

(int2binF 9 2)

(defn combinations [n lst]
  (loop [i (dec (count lst))]
    (if (= i 0)
       -1
      (do
        (println (int2binF n i))
        (recur (dec i))))))

(combinations 3 (char-array "abcdef"))



(defn combinations
    "Generate the combinations of a given size from a list"
    [k coll]
    (if (zero? k)
        '(())
    (if (empty? coll)
        nil
        (let
            [[head & tail] coll
             with-head     (combinations (dec k) tail)
             without-head  (combinations k tail)]

          (concat (map #(conj % head) with-head)
                  without-head)))))

(combinations 3 (char-array "abcdef"))

;something wrong
(defn comb [k l]
  (if (= 1 k) (map vector l)
      (apply concat
             (map-indexed
              #(map (fn [x] (conj x %2)) (comb (dec k) (drop (inc %1) l)) )))))

(comb 3 '(1 2))

;rewrite python code to clojure

;; def comb(sofar, rest, n):
;;     if n == 0:
;;         print sofar
;;     else:
;;         for i in range(len(rest)):
;;             comb(sofar + rest[i], rest[i+1:], n-1)

;; comb("", "abcde", 3)

(defn comb [sofar v n]
  (if (= n 0)
    (print sofar)
    (for [i (range 0 (count v))]
      (comb  (str sofar (nth v i))   ;don't it need to be recur ?
             (subvec v (inc i))
             (dec i)))))

(defn comb2 [sofar v n t]
  (if (= n 0)
    (print sofar)
    (for [i (range 0 (count v))]
      (recur (str sofar (nth v i))   ;don't it need to be recur ?
             (subvec v (inc i))
             (dec i)
             (inc t)))))

(comb "" [\a \b \c] 3 )

;; Corrent answer

;c(n,k) = c(n-1, k-1) + c(n-1,k)
;c(6,2) = c(5,1) + c(5,2) = 5 + 10 = 15

;total combination equals to if to choose first element, the rest have c(n-1,k-1) combinations.
;or if first element is not choosen, choose k from the rest n-1 element.
(defn combinations
  "P26 (**) Generate the combinations of K distinct objects chosen
  from the N elements of a list."
  [k n]
  (cond (= k 0) '(nil)
        (empty? n) nil
        :else (concat (map #(conj % (first n)) (combinations (dec k) (rest n)))
                      (combinations k (rest n)))))

(combinations 3 '(\a \b \c \d \e \f))

;http://codereview.stackexchange.com/questions/8930/enumerate-k-combinations-in-clojure-p26-from-99-problems
(defn subsets [n items]
  (cond
    (= n 0) '(())
    (empty? items) '()
    :else (concat (map #(cons (first items) %)
                    (subsets (dec n) (rest items)))
                  (subsets n (rest items)))))

;P27 a)
;; In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
;; Example:

;; scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
;; res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
(def group3 [lst]
  (

;; b)
;; Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

;; scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
;; res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...


;; Note that we do not want permutations of the group members;
;; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...).
;; However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
;; You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".

;P28



