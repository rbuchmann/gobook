(ns gobook.latex.output
  (:require [gobook.latex.generator :as lg]))

(defn make-doc [& content]
  [[:documentclass [:book :a5paper]]
   [:usepackage [:psgo]]
   (into [:document []]
         content)])

(defn output-latex [& content]
  (apply lg/render-doc
         (apply make-doc content)))
