(ns tools.diagram-generator
  (:require [tools.sgf-parser    :as p]
            [gobook.latex.output :as lo]))

(defn subsequences-until? [pred col]
  (let [indices (keep-indexed (fn [i x] (when (pred x) i)) col)]
    (mapv #(-> (inc %) (take col) vec) indices)))

(defn variations-with-comments
  ([sgf]
   (vec (variations-with-comments [] sgf)))
  ([path root]
   (let [{:keys [nodes branches]} root
         comment-paths (map #(into path %) (subsequences-until? :comment nodes))
         base-path (into path nodes)
         downstream (mapcat
                     (partial variations-with-comments base-path)
                     branches)]
     (concat comment-paths downstream))))


(def strip-variation
  (partial mapv #(select-keys % [:black-move :white-move :add-white :add-black :comment])))


(defn variations-to-show [sgf]
  (->> sgf
       variations-with-comments
       (mapv strip-variation)))


(defonce t (p/parse-sgf (slurp "testdata/4corners.sgf")))

(defonce tv (variations-to-show t))

(def coords->char
  (zipmap
   (range)
   [\a \b \c \d \e \f \g \h \j \k \l \m \n \o \p \q \r \s \t]))

(defn to-psgoboard [v]
  (let [{:keys [stones moves comments]} (:variation v)]
    [:minipage ["0.5\\textwidth" :t]
     [:setcounter [:gomove 0]]
     (into [:psgoboard* []]
           (concat
            (map (fn [[[x y] color]] [:stone [color (coords->char x) (inc y)]])
                 stones)
            (map (fn [[[x y] color]] [:move [(coords->char x) (inc y)]])
                 moves)))
     (or comments "")]))

(defn latexize-variation [v]
  (->> v
       (map to-psgoboard)
       (apply lo/render-to-pdf)))

(defn latexize-book [edn]
  (latexize-variation (:content edn)))

(def tp (clojure.edn/read-string (slurp "testdata/book.edn")))
