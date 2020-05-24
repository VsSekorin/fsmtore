(ns fsmtore.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [union]]))

(def start "_start_")
(def fin "_end_")
(def e "")

(defn command? [line] (not (or (str/blank? line) (str/starts-with? line "#"))))

(defn get-data [filename]
  (with-open [rdr (io/reader filename)]
    (let [[[s] fins & lines] (map #(str/split % #"\s+") (filter command? (line-seq rdr)))]
     [(union #{[start e s]} (set (map #(vector % e fin) fins)) (set lines))
      (reduce #(conj %1 (first %2) (last %2)) #{} lines)])))

(defn edges [commands state id] (filter #(= state (id %)) commands))

(defn edge [commands f t] (first (filter #(and (= f (first %)) (= t (last %))) commands)))

(defn pairs [state commands]
  (for [[pv & _ :as p] (edges commands state last) [_ _ qv :as q] (edges commands state first)
        :when (and (not= pv state) (not= qv state))]
    [p q]))

(defn +edges [commands [pair & tail]]
  (let [[[pv pa s] [_ qa qv]] pair [_ a _ :as lp] (edge commands s s) [_ o _ :as pq] (edge commands pv qv)
        w (str (if pq "(") (cond (= pa e) qa (= qa e) pa :else (str pa (if lp (str a "*")) qa)) (if pq (str "+" o ")")))]
    (if pair (+edges (-> commands (disj pq) (conj [pv w qv])) tail) commands)))

(defn convert [[commands [state & tail]]]
  (let [prs (pairs state commands) ucs (apply disj (+edges commands prs) (apply concat prs))]
    (if state (convert [ucs tail]) commands)))

(defn print-res [commands] (let [[_ res _] (edge commands start fin)] (println res)))

(defn -main [filename & args] (-> (get-data filename) (convert) (print-res)))

