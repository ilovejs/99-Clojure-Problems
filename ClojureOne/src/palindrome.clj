(defn palindrome? [s]
  (loop [i 0
         j (dec (. s length))]
    (cond (>= i j) true
          (= (get s i) (get s j)) (recur (inc i) (dec j))
          :else false)))  ; when i < j and s[i] != s[j]

;get is a map function.

(let [grade 85]
  (cond
   (>= grade 90) "A"
   (>= grade 80) "B"
   (>= grade 70) "C"
   (>= grade 60) "D"
   :else "F"))

;Anther version
(defn my-reverse [s]
  (let [lst (list)]
    (reduce #(str %2 %1)
      (mapcat #(conj lst %1) s))))

(defn my-palindrome? [s]
  (= s (my-reverse s)))

(true? (my-palindrome? "21022012"))
