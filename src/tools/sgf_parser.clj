(ns tools.sgf-parser
  (:require [instaparse.core :as insta]
            [clojure.string  :as str]))

(def sgf-grammar
  "
s = branch
branch = <ws> <'('> <ws> node* branch* <ws> <')'> <ws>
ws = #'\\s*'
node = <ws> <';'> <ws> prop*
prop = <ws> prop-name <ws> prop-value*
<prop-name> = #'\\w+'
<prop-value> = <ws> <'['> #'[^]]*' <']'> <ws>
")

(def sgf-parser (insta/parser sgf-grammar))

(def property-map
  {:pb :black
   :pw :white
   :c :comment
   :re :result
   :bt :black-team
   :wt :white-team
   :km :komi
   :ha :handicap
   :tm :time
   :ot :overtime
   :b  :black-move
   :w  :white-move
   :sz :board-size
   :ru :rules
   :ca :encoding
   :ff :sgf-version
   :dt :date
   :pc :played-at
   :ap :application
   :wl :white-time-left
   :bl :black-time-left
   :ab :add-black
   :aw :add-white
   :ae :add-empty
   :gm :game})

(defonce char-to-pos
  (->> (range 0 26)
       (map #(vector (char (+ % (int \a))) %))
       (into {})))

(defn parse-move [[x y :as s]]
  (case s
        ("" "tt") :pass
        (mapv char-to-pos [x y])))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-float [s]
  (Double/parseDouble s))

(defn parse-date [s]
  (->> (.split s "-")
       (map parse-int)
       (zipmap [:year :month :day])))

(def postprocess
  {#{:b :w :ab :aw :ae} parse-move
   #{:dt} parse-date
   #{:sz :tm :ha} parse-int
   #{:km} parse-float
   #{:gm} (fn [s] (if (= s "1")
                   :go
                   :unknown))})

(defn maybe-flatten [v]
  (cond-> v
    (= 1 (count v)) first))

(defn transform-node [[_ & props]]
  (into {}
        (for [[_ prop-name & prop-values] props]
          (let [prop-key (-> prop-name str/lower-case keyword)
                k (or (property-map prop-key) prop-key)
                processor (some (fn [[pred processor]]
                                  (when (pred prop-key)
                                    processor)) postprocess)
                v (mapv (or processor identity) prop-values)]
            [k (maybe-flatten v)]))))

(defn transform-branch [[_ & children]]
  (let [{:keys [node branch]} (group-by first children)
        all-nodes             (mapv transform-node node)
        all-branches          (mapv transform-branch branch)]
    {:nodes all-nodes
     :branches all-branches}))

(defn transform-result [tree]
  (let [[_ root] tree]
    (transform-branch root)))


(defn parse-sgf [sgf-string]
  (-> sgf-string
      sgf-parser
      transform-result))
