(ns advent-of-code-2017.day20
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;
; Part 1
;


(defn read-input
  ""
  [input-name]
  (clojure.string/split-lines (slurp (io/resource input-name))))

(defn parse-input-line
  ""
  [input-line]
  (let [cleaned-up
        (-> (str/replace input-line "=" " ")
            (str/replace "<" "[")
            (str/replace ">" "]")
            (str/replace "p" ":p")
            (str/replace "v" ":v")
            (str/replace "a" ":a"))
        edn-str (str "{" cleaned-up "}")
        xyz (edn/read-string edn-str)]
    xyz))

(defn parse-particles-buffer
  ""
  [input-name]
  (let [raw-buffer (read-input input-name)
        particles (map parse-input-line raw-buffer)
        indexed-particles (map-indexed (fn [index x] (assoc x :index index)) particles)]
    indexed-particles))

(defn update-particle
  "Updates particle velocity by the acceleration, then updates the position using the updated velocity."
  [particle]
  (let [{position     :p
         velocity     :v
         acceleration :a
         index        :index} particle
        updated-velocity (mapv + velocity acceleration)
        updated-position (mapv + position updated-velocity)]
    {:p updated-position :v updated-velocity :a acceleration :index index}))

(defn manhattan-distance
  ""
  [particle]
  (let [{position :p} particle]
    (reduce + (map #(Math/abs %) position))))

(defn find-closest-after
  ""
  [n input-name]
  (:index
    (first
      (sort-by #(manhattan-distance %)
               (last
                 (take n
                       (iterate #(map update-particle %)
                                (parse-particles-buffer input-name))))))))

(defn part1
  ""
  []
  (find-closest-after 1000 "day20"))

;
; Part 2
;

(defn find-collisions
  ""
  [particles]
  (into #{}
        (keys
          (filter #(> (count (second %)) 1)
                  (group-by identity
                            (map :p particles))))))

(defn update-particles-filtering-collisions
  ""
  [particles]
  (let [updated-particles (map update-particle particles)
        collisions (find-collisions updated-particles)
        filtered-particles (remove #(contains? collisions (:p %)) updated-particles)]
    filtered-particles))

(defn particle-count-after
  ""
  [n input-name]
  (count
    (last
      (take n
            (iterate update-particles-filtering-collisions
                     (parse-particles-buffer input-name))))))

(defn part2
  ""
  []
  (particle-count-after 1000 "day20"))