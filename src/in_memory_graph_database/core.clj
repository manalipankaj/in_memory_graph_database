(ns in-memory-graph-database.core
  (:gen-class)
   (require [cheshire.core :as json]
           [cheshire.parse :as parse]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Vertices
    [id label in out properties])

(defrecord Edges
    [label in out properties])

(def vertex-index (atom {}))

(def edges-array (atom []))

(defn generate-id
  "Generates unique ID"
  []
  (. clojure.lang.RT (nextID)))

(defn add-vertex-index
  [id {:keys [id label properties]}]
  (if (contains? @vertex-index id)
    nil
    (swap! vertex-index assoc id (Vertices. id label nil nil properties))))

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
    (swap! vertex-index assoc 
           in (Vertices. (:id in-node) (:label in-node) (conj (:in in-node) edge) (:out in-node) (:properties in-node)))
    (swap! vertex-index assoc
           out (Vertices. (:id out-node) (:label out-node) (:in out-node) (conj (:out out-node) edge) (:properties out-node)))))

(defn add-bidirectional-relation
  [in out edge edge2]
  (let [in-node (get @vertex-index in) out-node (get @vertex-index out)] 
    (swap! vertex-index assoc 
           in (Vertices. (:id in-node) (:label in-node) (:in in-node) (conj (:out in-node) edge2) (:properties in-node)))
    (swap! vertex-index assoc
           out (Vertices. (:id out-node) (:label out-node) (:in out-node) (conj (:out out-node) edge) (:properties out-node)))))

(defn add-edge
  [{:keys [label in out properties] :as node}]
  (when (and (contains? @vertex-index in) (contains? @vertex-index out))
    (let [edge (Edges. label in out properties)] 
      (swap! edges-array conj edge)
      (cond 
       (contains? node :uni) (add-relation-to-vertex in out edge)
       :else (add-bidirectional-relation in out edge (Edges. label out in properties))))))

(defn add-edges
  [edges]
  (map add-edge edges))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn delete-all
  []
  (reset! vertex-index {})
  (reset! edges-array []))

;; (defn node-to-json-map
;;   [vertex-index-map]
;;   (vec (map (fn [x] (merge 
;;                      {:id (:id (second x))} 
;;                      {:label (:label (second x))} 
;;                      {:in (map #(:label %) (:in (second x)))}
;;                      {:out (map #(:label %) (:out (second x)))}
;;                      (:properties (second x)))) vertex-index-map)))

(defn show-node-data
  []
  (println (json/generate-string @vertex-index {:pretty true})))

(defn show-edge-data
  []
  (println (json/generate-string @edges-array {:pretty true})))

(defn get-all-relationship
  []
  (println
   (filter #(not (nil? %)) (map #(:label (first (:in (second %)))) @vertex-index))
   (filter #(not (nil? %)) (map #(:label (first (:out (second %)))) @vertex-index))))

(defn get-next-node
  [node relation]
  (if (string? node)
    (filter #(= (:label %) relation) (:out (get @vertex-index node)))
    (filter #(= (:label %) relation) (:out (get @vertex-index (:in (first node)))))))

(defn query1
  [query]
  (map #(:in %) (reduce get-next-node query)))
