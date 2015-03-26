(ns clojuree.e)

(require '[clojure.math.numeric-tower :as math :refer :all])
(defn input [] (shuffle (range 10)))

(defn sum [l] (reduce + l))

(defn dist [s t]
  (sum (for [x (range 10)] (abs (- (.indexOf s x) (.indexOf t x)))))
)

;(def syms ['fn '> '< '+ '- '* '/ 'let 'if 'first 'rest 'quote 'a 'b 'c 'd 'e])
(def syms ['fn 'first 'rest 'last 'if 'x 'y])

(defn prob [x] (> x (rand)))

(defn get_random_elem []
    (cond
        (prob 0.5) (rand-nth syms)
        (prob 0.5) '[]
        :else '()
    )
)

(defn modify [fnl]
    (if (and (prob 0.5) (empty? fnl))
        (list (get_random_elem))
        (apply concat
            (for [elem fnl]
                (cond
                    (and (prob 0.5) (seq? elem)) (list (into '() (reverse (modify elem))))
                    (and (prob 0.5) (vector? elem)) (list (vec (modify elem)))
                    (prob 0.1) (list elem (get_random_elem))
                    (prob 0.1) '()
                    :else (list elem)
                )
            )
        )
    )
)

(defn score_factory [len_penalty test_cases]
    (fn [fnl]
        (sum (for [test_case test_cases]
            (let [source (:source test_case)
                  result (:result test_case)
                  _score
                    (try
                      (let [res (apply (eval fnl) [source])]
                        (if-not (instance? Number res) 1
                          (if-not (some #{res} source) 2
                            (+ 2 res)
                          )
                        )
                      )
                      (catch Exception e 0)
                    )
                  ]
                (- _score (* (count (str fnl)) (count (str fnl)) len_penalty) )
            )
        ))
    )
)


(defn select_factory [score randomness cnt]
  (fn [pp]
    (do
      (def rscore (into {} (for [elem pp] [elem, (- (+ (score elem) (* randomness (rand))))])))
      ;(println rscore)
      (take cnt (sort-by #(get rscore %) pp))
    )
  )
)


(defn modify_pop_factory [select]
    (fn [pp]
        (let [list_pp (into '() pp)]
              (let [news (take (count pp) (repeatedly #(modify (rand-nth list_pp))))]
                   (into #{} (select (concat pp news)))
              )
        )
    )
)


(defn stats_factory [score]
    (fn [pp]
        (hash-map
            :size (count pp)
            :avs  (/ (sum (for [elem pp] (score elem))) (count pp))
            :maxs (score (apply max-key (cons score pp)))
            :random3 (take 3 (reverse (shuffle pp)))
        )
    )
)


(defn tests []
  (do
    (let [score (score_factory 0 [{:source [1 2 3] :result 3}])]
        (println (= 3 (score '(fn [x] (first x)))))
        (println (= 5 (score '(fn [x] (last x)))))
        (println (= 2 (score '(fn [x] 123))))
        (println (= 1 (score '(fn [x] "ahoj"))))
        (println (= 0 (score '(just bullshit))))
    )
    (let [score (score_factory 0 [{:source [1 2 3] :result 3}
                                  {:source [4 5 6] :result 6}])]
        (println (= 9 (score '(fn [x] (first x)))))
        (println (= 13 (score '(fn [x] (last x)))))
        (println (= 4 (score '(fn [x] 123))))
        (println (= 2 (score '(fn [x] "ahoj"))))
        (println (= 0 (score '(just bullshit))))
    )
    (let [select (select_factory (fn [x] x) 0 10 )]
        (println (= (select (range 40)) (reverse (range 30 40))))
    )
  )
)

(defn -main []
  ;(tests)
  (def test_cases
    (for [i (range 10)]
      (let [l (+ 1 (rand-int 9))
            source (shuffle (range l))
            result (apply max source)
           ]
        {:source source :result result}
      )
    )
  )
  ; (let [score (score_factory 0.001 test_cases)]
  ;   (do
  ;       (println test_cases)
  ;       (println (score '(fn [x] (first x))))
  ;       (println (score '(fn [x] (last x))))
  ;       (println (score '(fn [x] (apply min x))))
  ;       (println (score '(fn [x] (apply max x))))
  ;       (println (score '(fn [x] bullcrap)))
  ;  )
  ;)

   (def init (take 20 (repeatedly #(modify '()))))
   (print init)
   ;(def init (conj init '(fn [r] (apply max r))))
   (def init (conj init '(fn [r] (apply max r))))


   (let [score (score_factory 0.00001 test_cases)
         select (select_factory score 0.2 20)
         modify_pop (modify_pop_factory select)
         stats (stats_factory score)
        ]

       (dotimes [n 10000]
           (def init (modify_pop init))
           (println (stats init))
       )
   )
)

(comment
(-main)
)


(comment
  (tests)
)

