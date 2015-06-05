 (ns haha)

;--sample answer
;https://github.com/AustinRochford/99-problems-clojure

;https://cursiveclojure.com/userguide/keybindings.html

(print "haha")
;in here ctl + shift + p

;1. Find the last element of a list.
 (last [1 2 3 4])

;2. Find the last but one element of a list.
 (defn secondLast [lst]
   (let [i (count lst)]
          (nth lst (- i 2))))
 (secondLast [1 2 3 4])
;count [1 2 3]

;3 Find the Kth element of a list.
 (nth [1 2 3] 3)                                            ;out of bound
 (nth [1 2 3] 2)

;4 Find the number of elements of a list.
(count [1 2 3])

;5 Reverse a list.
(reverse [1 2 3])
;TODO - why reverse return this type?
(= (reverse "123")
   '(\1 \2 \3))
;false

(true? (= (reverse "12321")
          '(\1 \2 \3 \2 \1)))
;true

;6 Find out whether a list is a palindrome.
;this is typical oo style, return value don't needed.
(defn palindrome-2 [lst]
   (if (= lst (reverse lst))
     1 0))

;TODO - how to use cursive to load to REPL
(defn palindrome [lst]
  (= lst (apply str (reverse lst))))

(palindrome "12321")

(if true 1 0)

;;how to convert back to string
(map concat (\1 \2 \3 \2 \1))

(= [1 2 3 2 1] (reverse [1 2 3 2 1]))                       ;true

(str 1 2)                                                   ;"12"
(concat "1" "2")                                            ;(\1 \2)

;other ppl's answer
(defn palindrome? [s]
  (= s (apply str (reverse s))))

;IMPORTANT to know
(apply str '(\a \b))                                        ;"ab"


;7. Flatten a nested list structure.
(flatten ((1,1), 2, (3, (5,8))))

;wrong
(flatten '( '(1,1), 2, '(3, '(5,8))))

(flatten '(1 (2,3), 2))

;right
(flatten '((1,1), 2, (3, (5,8))))
;(1 1 2 3 5 8)

;8. Eliminate consecutive duplicates of list elements.

;(defn pack [lst]
;  (take-while (partial
;                (fn [a b] (= a b))
;                )
;              lst))

; why partition-by take 1 arg ?
 ;(defn pack [lst]
 ;  (partition-by #(= %1 %2) lst))

 ;wrong too
;; (defn pack [lst]
;;   (cond
;;     (nil? lst) nil
;;     :else (let [head (first lst)]
;;             (cons
;;               head (recur
;;                      (drop-while #(= % head) (rest lst)))))))

(pack [1 1 2 3 4 5 6 5 5 1])



;TODO: fix - take-while's pred take 1 arg
(take-while (partial
              (fn [a b] (= a b))
               lst)
            lst)

;however
(take-while (partial > 10) (iterate inc 0))


;sample answer
(def compress
    "Eliminate consecutive duplicates of list elements"
    (comp
        (partial map first)
        (partial partition-by identity)))

(compress [1 1 2 3 4 5 6 5 5 1])

(def fat-compress (partial partition-by identity))

(fat-compress [1 1 2 3 4 5 6 5 5 1])

;or simple as this
(partition-by identity [1 1 2 3 4 5 6 5 5 1])



;9. Pack consecutive duplicates of list elements into sublists.
;pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
;List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

;char-array turn string into chars
(fat-compress (char-array "aaaabccaadeeee"))



;10. Run-length encoding of a list.
;; Use the result of problem P09 to implement the so-called run-length encoding data compression method.
;; Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.

;; Example:
;; scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
;; res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

(defn encode [lst]
  (map #(list (count %) (first %))
       (fat-compress lst)))

(encode (char-array "aaaabccaadeeee"))

;tips
(list 4 \a)



;11. Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
;; Only elements with duplicates are transferred as (N, E) terms.
;; Example:

;;scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
;;res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

(defn encodeModified [lst]
  (map #(let [cnt  (count %)
              head (first %)]
          (if (= cnt 1)
             head                        ;single element
            (list cnt head)))
       (fat-compress lst)))

(encodeModified (char-array "aaaabccaadeeee"))



;; P12 (**) Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
;; Example:

;; scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
;; res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

;https://github.com/pebrc/ninety-nine-clojure/blob/64d77d60f578e288ae676f5ac7d8657fa22fa519/src/ninety_nine_clojure/lists.clj#L119
(defn decode [lst]
  (map (fn [t] (#(repeat (first t) (second t)))) lst))

(decode '((4, \a), (1, \b), (2, \c), (2, \a), (1, \d), (4, \e)))

;test snippet
(def tl (list 4 \a))

(#(repeat %1 %2) (first tl) (second tl))

;n x
(repeat 2 3)
;this is wrong pattern !, pass in accumlator instead.
(let [r '()]
  (loop [i 4]
    (when (> i 0)
      (conj r i)
      (recur (dec i))))
  r)

(conj '() 2)


;How to access r from outside ?
(loop [i  4
       r '()]
  (when-not (< i 0)
    (recur (dec i)
           (conj r i))))


;This is like generator pattern !!
(def loop1
  (loop [x 10]
    (if (> x 0)
      (print x)
      ;x                       ;here you can add side-effect action; But does it returns ?
      (recur (- x 1)))))

(map str loop1)
(str 1)

(print loop1)

;loop when Pattern
;TODO: why if I change > to =, nothing get printed.
(loop [x 10]
    (when (> x 0)
      (println x)
      (recur (- x 1))))

;Why this print 10
(loop [x 10]
    (if (> x 0)
      (println x)
      (recur (- x 1))))

;official doc gives me:
(defn my-re-seq [re string]
  "Something like re-seq"
  (let [matcher (re-matcher re string)]

    (loop [match (re-find matcher) ;loop starts with 2 set arguments
           result []]
      (if-not match
        result
        (recur (re-find matcher)    ;loop with 2 new arguments
               (conj result match))))))

(my-re-seq #"\d" "0123456789")

;matcher object
(re-find (re-matcher #"\d" "0123456789"))


;Bravo !!!　r returns something.
(defn getNums []
  (loop [i  4
         r []]
    (if (<= i 0)
      r
      (recur (dec i)
             (cons i r)))))

(getNums)

;reverse action of 'reduce to single value'
(take 4 (iterate str 5))

(range 1 5)



;; P13 (**) Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method directly.

;; I.e. don't use other methods you've written (like P09's pack); do all the work directly.
;; Example:

;; scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
;; res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
(defn encodeDirect [lst]
  (let [x (first lst)
        xs (rest lst)]
    (take-while #())))

(defn encodeDirect [lst]
  (map #(list (count %) (first %))
    (partition-by identity lst)))

(encodeDirect (char-array "aaaabccaadeeee"))

;; P14 (*) Duplicate the elements of a list.
;; Example:
;; scala> duplicate(List('a, 'b, 'c, 'c, 'd))
;; res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
(defn duplicate [lst]
  (flatten
    (map #(repeat 2 %) lst)))

(duplicate (char-array "abccd"))

;; P15 (**) Duplicate the elements of a list a given number of times.
;; Example:
;; scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
;; res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
(defn duplicateN [n lst]
  (flatten
   (map #(repeat n %) lst)))

(duplicateN 3 (char-array "abccd"))

;; P16 (**) Drop every Nth element from a list.
;; Example:

;; scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

;; res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

(defn drop [N lst]
  (mapcat (fn [x]
            (if (< (count x) 2)
              )
  (partition-all N lst))

(drop 3 (char-array "abcdefghijk"))


;filter is by content not by index.

(partition-all 3 (char-array "abccdefghijkl"))


(nth (char-array "abcdefghijk") 3)



;java interpo
(def v ["one" "two" "three" "two"])
(.indexOf v "two")
(.indexOf v "foo")


;find index from vector
(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn find-thing [value coll]
  (first-index-of #(= % value) coll))

(find-thing "two" ["one" "two" "three" "two"]) ; 1
(find-thing "two" '("one" "two" "three")) ; 1

;; these answers are a bit silly
(find-thing "two" #{"one" "two" "three"}) ; 1
(find-thing "two" {"one" "two" "two" "three"}) ; nil



;; P17 (*) Split a list into two parts.
;; The length of the first part is given. Use a Tuple for your result.
;; Example:

;; scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
;; res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))



;; P18 (**) Extract a slice from a list.
;; Given two indices, I and K, the slice is the list containing the elements from and including the Ith element
;; up to but not including the Kth element of the original list. Start counting the elements with 0.
;; Example:

;; scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
;; res0: List[Symbol] = List('d, 'e, 'f, 'g)



;; P19 (**) Rotate a list N places to the left.
;; Examples:
;; scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
;; res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

;; scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
;; res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)



;; P20 (*) Remove the Kth element from a list.
;; Return the list and the removed element in a Tuple. Elements are numbered from 0.
;; Example:

;; scala> removeAt(1, List('a, 'b, 'c, 'd))
;; res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)



