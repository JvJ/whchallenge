(ns whchallenge.astar
  "This namespace models the A* pathfinding
  algorithm.

  Any graph representation may be used as
  long as it has a valid heuristic and
  neighbors function."
  
  (:require [swiss.arrows :refer :all]
            [clojure.string :as st
             :refer [split split-lines]]
            [clojure.data.priority-map :as pm]))

(defn find-path
  "Given a map, a start-node, and an end-node, find
  the shortest path.
  
  The map must be able to act as 

  A valid map must provide the following keys in its metadata:
  :compare-nodes A standard clojure comparator for 2 cost values.
  :neighbors A function, takes [map, node], returns a set
  of neighbors to that node.
  :heuristic A function, takes [map, node1, node2], estimates the
  cost between those two nodes.
  :nodes Optional.  A function that can be applied to the map and
  return a sequence of all nodes in the map.  Defaults to 
  clojure.core/keys if not supplied.
  :default-g Optional.  The default \"infinite\" value of the
  g-score.  Defauts to nil if not supplied."
  
  [m start end]

  (let [;; Get params from metadata
        {:keys [heuristic compare-nodes
                neighbors default-g
                nodes default-g]}
        (meta m)

        nodes (or nodes keys)

        _ (when-not (and heuristic compare-nodes neighbors)
            (throw (Exception.
                    "Invalid metadata.  Requires :heuristic, :compare-nodes, and :neighbors.")))
        
        
        {:keys [came-from] :as result}
        (loop [{:keys [closed open-f came-from
                       g-score f-score]
                :as params}
               {:closed #{}
                :open-f (pm/priority-map-by
                         compare-nodes
                         start (heuristic m start end))
                :came-from {}
                :g-score (-<>> (nodes m)
                               (map #(-> [% default-g]))
                               (into {})
                               (assoc <> start 0))}]
          
          (let [[c _] (first open-f)]

            ;; Break-out conditions
            (if (or (empty? open-f) (= c end))
              ;; then
              params
              
              ;; else
              ;; Transduce over neighbors and re-loop
              (recur
               (reduce

                (fn
                  [{:keys [open-f closed g-score] :as acc} n]
                  (let [temp-g (+ (g-score c)
                                  (heuristic m c n))]
                    
                    (cond
                      (closed n) acc
                      (and (open-f n) (>= temp-g (g-score n))) acc
                      :else (-> acc
                                (update :open-f assoc n
                                        (+ temp-g (heuristic m n end)))
                                (update :came-from assoc n c)
                                (update :g-score assoc n temp-g)))))

                ;; Update the open and closed sets
                ;; before passing them in
                (-> params
                    (update :open-f dissoc c)
                    (update :closed conj c))

                ;; Transduce over c's neighbors
                (neighbors m c))))))]
    (->> (iterate came-from end)
         (take-while #(and % (not= start %)))
         (cons start)
         reverse)))


