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

(defn flatten-node [v]
  (let [{:keys [black-move white-move
                add-white add-black]} v
        move (or black-move white-move)]
    (concat
     (map (fn [[x y]] [:stone [:black (coords->char x) (inc y)]])
          add-black)
     (map (fn [[x y]] [:stone [:white (coords->char x) (inc y)]])
          add-white)
     (map (fn [[x y]] [:move [(coords-to-char x) (inc y)]])
          (when (and move (not= :pass move))
            [move])))))

(defn latexize-variation [v]
  (->> v
       (mapcat flatten-node)
       (into
        [:psgoboard []])
       (lo/render-to-pdf)))
