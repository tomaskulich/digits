(ns clojuree.o)


(defn sum [x] (reduce + x))
(defn prob [x] (> x (rand)))

(def width 28)
(def height 28)

(defn doublemap [ifn]
  (let [widthr (range width)
        heightr (range height)]
    (let [f (fn [x y img] 
              (sum (for [i widthr j heightr] 
                 (ifn x y i j (get-in img [i j])))))]
      (fn [img] (vec (for [y heightr] (vec (for [x widthr] (f x y))))))
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

  
(defn -main []
  (println (clojure.string/join "\n" (take 10 (get-random-nfn-pool [] params nfs nps))))
  ;(print (slurp "blubber.txt"))
)
