(ns debug)

(defn comb [sofar v n]
   (cond (= n 0) (print sofar)
         :else   (loop [i (range 0 (count v))]
                    (comb (str sofar (nth v i))
                          (subvec v (inc i))
                          (dec i)))))

(comb "" [\a \b \c] 3)

(def v "abc")

(for [i (range 0 (count v))]
  (comb (str sofar (nth v i))   ;don't it need to be recur ?
        (subvec v (inc i))
        (dec i)))

;make a while loop in clj

(conj '(\a \b \c \d \e \f) \k)

(cons \k '(\a \b \c \d \e \f))

(str "" \a)

(subvec [1 2 3 4] 1)

(nth [1 2 3 4] 1)

;same as python range(len(lst))
(range 1 3)

