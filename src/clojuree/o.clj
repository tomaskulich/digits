(ns clojuree.o
  (:require [clojure.algo.generic.math-functions :as math :refer :all])
  (:require [clojure.math.combinatorics :as math-comb :refer :all])
)

(defn sum [x] (reduce + x))
(defn prob [x] (> x (rand)))
(defn mean [x] (float (/ (sum x) (count x))))
(defn disp [x] (sqrt (- (mean (map sqr x)) (sqr (mean x)))))
(defn diff [x y] (float (/ (abs (- (mean x) (mean y))) (+ (disp x) (disp y)))))
(defn fitness [xs] (sum (for [i (combinations xs 2)] (apply diff i))))

(def width 28)
(def height 28)

(defn doublemap [ifn]
  (let [widthr (range width)
        heightr (range height)]
    (let [f (fn [x y img] 
              (sum (for [i widthr j heightr] 
                 (ifn x y i j (get-in img [i j])))))]
      (fn [img] (vec (for [y heightr] (vec (for [x widthr] (f x y img))))))
    )
  )
)

(defn get-random-nfn [pool params nfs nps]
  (let [rand-num (fn [] 
         (cond 
           (prob 0.5) (rand-nth params)
           (and (not-empty pool) (prob 0.7)) (rand-nth pool)
           :else (rand-int width)))
       ]
    (cond 
       (prob 0.5) (let [v1 (rand-num) v2 (rand-num) fn (rand-nth nfs)] 
          `(~fn ~v1 ~v2))
       :else (let [v1 (rand-num) v2 (rand-num) v3 (rand-num) v4 (rand-num) pred (rand-nth nps)]
          `(if (~pred ~v1 ~v2) ~v3 ~v4))
    )
  )
)

(defn get-random-nfn-pool [pool & args]
  (let [curr (apply get-random-nfn pool args)
        _pool (if (> (count pool) 10000) (pop pool) pool)]
    (cons curr (lazy-seq (apply get-random-nfn-pool (conj _pool curr) args)))
  )
)

(def nfs ['+ '- '*])
(def nps ['> '< '=])
(def params ['x 'y 'i 'j 'c])
 
;; helpers
(defn buckets [imgs dgts f]
  ;(map (fn [i] (->> i last (map last) (map f))) (group-by first (map vector dgts imgs))))
  (map (fn [i] (map (comp f last) (last i))) (group-by first (map vector dgts imgs))))

(defn simplemap [ff] (fn [img] (sum (map sum img)))) ;((doublemap ff) img)))))
(defn calc-fitness [ff imgs dgts] (fitness (buckets imgs dgts ff)))

(def img1 (for [i (range 28)] (for [j (range 28)] (+ i j))))
(def img2 (for [i (range 28)] (for [j (range 28)] (* i j))))
(def img3 (for [i (range 28)] (for [j (range 28)] j)))
(def img4 (for [i (range 28)] (for [j (range 28)] i)))
(def imgs [img1 img2 img3 img4])
(def dgts [0 0 3 0])

(defn -main []
  (println (fitness [[1 100] [100 1]]))
  (println (buckets ["a" "b" "c" "d"] [0 0 3 0] identity))
  (defn ff [x y i j c] (if (< (if (= x c) y (if (= (if (< 6 i) 7 i) c) y (if (< 6 i) 7 i))) 18) 3 j))
;  (println (buckets imgs dgts))
  (println (buckets imgs dgts (simplemap ff)))
  (println (calc-fitness (simplemap ff) imgs dgts))
  ;(println (sum (map sum ((doublemap ff) img))))

  (println (clojure.string/join "\n" (take 10 (get-random-nfn-pool [] params nfs nps))))
  ;(print (slurp "blubber.txt"))
)
