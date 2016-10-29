(ns in-memory-graph-database.core
  (:gen-class)
   (require [cheshire.core :as json]
           [cheshire.parse :as parse]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Vertices
    [id label in out data])

(defrecord Edges
    [label in out data])

(def vertex-index (ref {}))

(def edges-array (ref []))

(defn generate-id
  "Generates unique ID"
  []
  (. clojure.lang.RT (nextID)))

(defn add-vertex-index
  [ids {:keys [id label data]}]
  (if (contains? @vertex-index ids)
    nil
    (dosync
     (alter vertex-index assoc ids (Vertices. ids label nil nil data)))))

(defn add-vertex
  [vertex]
  (let [id (if (nil? (:id vertex)) (generate-id) (:id vertex))]
    (add-vertex-index id vertex)))

(defn add-vertices
  [vertices]
  (do
    (map add-vertex vertices)))

(defn add-relation-to-vertex
  "Creates uni realtionship edge"
  [in out edge]
  (let [in-node (get @vertex-index in) out-node (get @vertex-index out)]
    (dosync
     (alter vertex-index assoc
            in (Vertices. (:id in-node) (:label in-node) (conj (:in in-node) edge) (:out in-node) (:data in-node))))
    (dosync
     (alter vertex-index assoc
            out (Vertices. (:id out-node) (:label out-node) (:in out-node) (conj (:out out-node) edge) (:data out-node))))))

(defn add-bidirectional-relation
  [in out edge edge2]
  (let [in-node (get @vertex-index in) out-node (get @vertex-index out)]
    (dosync
     (alter vertex-index assoc
            in (Vertices. (:id in-node) (:label in-node) (:in in-node) (conj (:out in-node) edge2) (:data in-node))))
    (dosync
     (alter vertex-index assoc
            out (Vertices. (:id out-node) (:label out-node) (:in out-node) (conj (:out out-node) edge) (:data out-node))))))

(defn add-edge
  [{:keys [label in out data] :as node}]
  (when (and (contains? @vertex-index in) (contains? @vertex-index out))
    (let [edge (Edges. label in out data)]
      (dosync (alter edges-array conj edge))
      (cond
       (contains? node :uni) (add-relation-to-vertex in out edge)
       :else (add-bidirectional-relation in out edge (Edges. label out in data))))))

(defn add-edges
  [edges]
  (map add-edge edges))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn delete-all
  []
  (dosync (ref-set vertex-index {}))
  (dosync (ref-set edges-array [])))

(defn delete-node
  [node]
  (dosync
   (alter @vertex-index dissoc node)))

(defn show-node-data
  []
  (println (json/generate-string @vertex-index {:pretty true})))

(defn show-edge-data
  []
  (println (json/generate-string @edges-array {:pretty true})))

(defn show-node
  [node]
  (println (json/generate-string node {:pretty true})))

(defn get-all-relationship
  []
  (println
   (set
    (concat

     (->> @vertex-index
         (map #(->> % second :in first :label))
         (filter #(not (nil? %))))
     (filter #(not (nil? %)) (map #(:label (first (:out (second %)))) @vertex-index))
     ))))

;;;;;;;;;;;;;;;;;;;;;;;Query;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-next-node
  [node relation]
  (if (string? node)
    (filter #(= (:label %) relation) (:out (get @vertex-index node)))
    (if (= (count node) 1)
      (filter #(= (:label %) relation) (:out (get @vertex-index (:in (first node)))))
      (map (fn [x] (filter (fn [y] (= (:label y) relation)) (:out (get @vertex-index (:in x))))) node))))

(def func-for-query-map
  (fn [x] (if (map? x)
            (get @vertex-index (:in x))
            (first (map (fn [y]  {(:out y) (get @vertex-index (:in y))}) x)))))

(defn query1
  [query]
  (show-node (map func-for-query-map (reduce get-next-node query))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
