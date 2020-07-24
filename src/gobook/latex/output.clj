(ns gobook.latex.output
  (:require [gobook.latex.generator :as lg]
            [me.raynes.conch        :as conch :refer [programs]]
            [clojure.java.io        :refer [writer]]))

(defn make-doc [& content]
  [[:documentclass [:book :a4paper]]
   [:usepackage [:psgo]]
   (into [:document []
          [:setgounit ["0.3cm"]]]
         content)])

(defn output-latex [& content]
  (apply lg/render-doc
         (apply make-doc content)))

(programs latexmk)

(def outfile "./output/test.tex")

(defn render-to-pdf [& content]
  (with-open [outstream (writer outfile)]
    (binding [*out* outstream]
      (apply output-latex content)))
  (latexmk "-pdfps" "-output-directory=./output" outfile))

(def t [:psgoboard []
        [:stone [:white :a 1]]
        [:stone [:black :b 2]]
        [:stone [:white :c 3]]])
