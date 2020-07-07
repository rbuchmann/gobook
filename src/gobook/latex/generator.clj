(ns gobook.latex.generator
  (:require [clojure.string :as str]))

;; Overall structure and indentation helpers

(def ^:dynamic *indentation* 0)

(def indent-width 2)

(defn padding [n]
  (apply str (repeat n " ")))

(defmacro indented [& body]
  `(binding [*indentation* (+ *indentation* indent-width)]
     ~@body))

;; String generation helpers

(defn outln [s & args]
  (print (str (padding *indentation*)) (apply format s args) \newline))

(defn in-brackets [s]
  (when (not-empty s)
    (format "[%s]" s)))

(defn in-braces [s]
  (when (not-empty s)
    (format "{%s}" s)))

(defn stringify [x]
  (if (keyword? x)
    (name x)
    (str x)))

;; Rendering hiccups-style docs to latex strings

(declare render-node)

(defn render-args [args n-mandatory]
  (let [mandatory (take n-mandatory args)
        optional  (drop n-mandatory args)]
    (str (->> optional  (map stringify) (str/join ",") in-brackets)
         (->> mandatory (map stringify) (map in-braces) (apply str)))))

(defn environment-fn [tag n-mandatory]
  (fn [args children]
    (let [arg-str (render-args args n-mandatory)]
      (outln "\\begin{%s}%s" (name tag) arg-str)
      (indented
       (doseq [child children]
         (render-node child)))
      (outln "\\end{%s}" (name tag)))))

(defn cmd-fn [cmd n-mandatory]
  (fn [args _]
    (let [arg-str (render-args args n-mandatory)]
      (outln "\\%s%s" (name cmd) arg-str))))

;; Render functions

(def environments
  {:document  0
   :psgoboard 0})

(def commands
  {:documentclass 1
   :usepackage    1
   :stone         3})

(def render-fns
  (into {}
        (concat
         (for [[k v] environments]
           [k (environment-fn k v)])
         (for [[k v] commands]
           [k (cmd-fn k v)]))))

(defn render-node [[tag args & children]]
  (if-let [renderer (render-fns tag)]
    (renderer args children)
    (throw (RuntimeException. (format "No renderer for tag %s" tag)))))

(defn render-doc [& nodes]
  (doseq [node nodes]
    (render-node node)))
