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

(defn get-random-feature [numbers bools nfs nps]
  (let [rand-num (fn [] 
         (cond 
           (prob 0.5) (rand-nth numbers)))
       ]
    (cond 
       (prob 0.5) (let [v1 (rand-num) v2 (rand-num) fn (rand-nth nfs)] 
          '(fn v1 v2))
       :else (let [v1 (rand-num) v2 (rand-num) v3 (rand-num) v4 (rand-num) pred (rand-nth nps)]
          '(if (pred v1 v2) v3 v4))
    )
  )
)
  
(def nfs [+ - *])
(def nps [> < =])
(def numbers [x y i j])




  
(defn- main []
  (print (slurp "blubber.txt"))
)
