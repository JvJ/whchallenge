(ns whchallenge.astar-test
  (:require [whchallenge.astar :as ast]
            [clojure.test :as t]))



(t/deftest neighbors
  (let [g1 (ast/read-grid
            (str "...\n"
                 "...\n"
                 "...\n"))
        g1m (ast/grid->map g1)

        g2 (ast/read-grid
            (str ".#.\n"
                 "#.#\n"
                 ".#.\n"))
        g2m (ast/grid->map g2)

        g3 (ast/read-grid
            (str "...\n"
                 "#.#\n"
                 "...\n"))
        g3m (ast/grid->map g3)

        g4 (ast/read-grid
            (str ".#.\n"
                 "...\n"
                 ".#.\n"))
        g4m (ast/grid->map g4)]

    (t/is (= [3 3]
             (ast/bounds g1)
             (ast/bounds g2)))

    (t/is (every? #(and (ast/in-bounds? (ast/bounds g1) %1)
                        (ast/in-bounds? (ast/bounds g2) %1))
                  (for [i (range 3) j (range 3)] [i j])))
    
    (t/is (= #{}
             (ast/neighbors g2m [1 1])))

    (t/is (= #{[0 1] [2 1]}
             (ast/neighbors g3m [1 1])))

    (t/is (= #{[1 0] [1 2]}
             (ast/neighbors g4m [1 1])))))

(t/deftest priority

  (let [;; Ten numbers shuffled in with keywords :a to :z
        nums (range 10)
        kws (map (comp keyword str char)
                 (range (int \A) (inc (int \Z))))
        rand-list (shuffle (concat nums kws))
        sorted-list (sort ast/cell-comp rand-list)
        sorted-nums (take-while number? sorted-list)
        sorted-kws (drop-while number? sorted-list)]
    
    (t/is (= nums sorted-nums))
    (t/is (= kws sorted-kws))))

(defn test-ns-hook
  []
  (neighbors))
