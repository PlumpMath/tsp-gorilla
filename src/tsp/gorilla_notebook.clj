;; gorilla-repl.fileformat = 1

;; **
;;; # The Travelling Salesman Problem in Clojure
;;; 
;;; This is a translation of Peter Norvig's greatly informative ipython [notebook](http://nbviewer.ipython.org/url/norvig.com/ipython/TSPv3.ipynb) to Clojure.
;;; 
;;; It uses [Gorilla REPL](http://gorilla-repl.org/).
;; **

;; **
;;; Let's begin by populating the namespace:
;; **

;; @@
(ns tsp.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]
            [gorilla-plot.core :as gp]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## All Tours Algorithm `alltours-tsp`
;;; Let's start with a guaranteed correct (but inefficient) algorithm.
;;; > _All Tours Algorithm_: Generate all possible tours of the cities, and choose the shortest tour.
;; **

;; @@
(defrecord City [x y])

(defn gen-cities [n & {:keys [w h s] :or {w 900 h 600 s 42}}]
  (for [i (range n)]
    (City. (rand-int w) (rand-int h))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/gen-cities</span>","value":"#'tsp.core/gen-cities"}
;; <=

;; @@
(defn alltours
  "Generate all possible tours."
  [cities]
  (combo/permutations cities))

(defn distance
  "Distance between two cities given as a city pair."
  [[a b]]
  (let [x-dist (- (:x a) (:x b))
        y-dist (- (:y a) (:y b))]
    (math/sqrt (+ (math/expt x-dist 2)
                  (math/expt y-dist 2)))))

(defn tour-length
  "Sum of distances between each pair of consecutive cities in the tour."
  [tour]
  (let [dests (concat (drop 1 tour) [(first tour)])
        city-pairs (map vector tour dests)]
    (apply + (map distance city-pairs))))

(defn shortest-tour
  "Choose tour with minimum tour length."
  [tours]
  (first (sort-by tour-length tours)))

(defn alltours-tsp
  "Generate all possible tours of the cities and choose shortest tour."
  [cities]
  (shortest-tour (alltours cities)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/alltours-tsp</span>","value":"#'tsp.core/alltours-tsp"}
;; <=

;; @@
(alltours [1 2 3])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 2 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 3 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[2 1 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[2 3 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[3 1 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[3 2 1]"}],"value":"([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])"}
;; <=

;; @@
(tour-length (alltours-tsp (gen-cities 3)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>1281.840307587906</span>","value":"1281.840307587906"}
;; <=

;; @@
(alltours-tsp (gen-cities 3))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>181</span>","value":"181"}],"value":"[:x 181]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>437</span>","value":"437"}],"value":"[:y 437]"}],"value":"#tsp.core.City{:x 181, :y 437}"},{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>559</span>","value":"559"}],"value":"[:x 559]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>279</span>","value":"279"}],"value":"[:y 279]"}],"value":"#tsp.core.City{:x 559, :y 279}"},{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>814</span>","value":"814"}],"value":"[:x 814]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>132</span>","value":"132"}],"value":"[:y 132]"}],"value":"#tsp.core.City{:x 814, :y 132}"}],"value":"(#tsp.core.City{:x 181, :y 437} #tsp.core.City{:x 559, :y 279} #tsp.core.City{:x 814, :y 132})"}
;; <=

;; @@
(defn city->vec [city]
  [(:x city) (:y city)])

(defn plot-tour
  "Plot the cities as circles and the tour as lines between them."
  [tour]
  (let [tourv (map city->vec (conj (vec tour) (first tour)))]
    (gp/compose (gp/list-plot tourv :joined true)
                (gp/list-plot tourv :symbol-size 70))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/plot-tour</span>","value":"#'tsp.core/plot-tour"}
;; <=

;; @@
(plot-tour (alltours-tsp (gen-cities 6)))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"392cc31e-4164-4b53-b448-585ca24a4c98","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"392cc31e-4164-4b53-b448-585ca24a4c98","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"392cc31e-4164-4b53-b448-585ca24a4c98","values":[{"x":110,"y":550},{"x":560,"y":319},{"x":793,"y":559},{"x":896,"y":9},{"x":673,"y":97},{"x":368,"y":18},{"x":110,"y":550}]},{"name":"a92ce844-ca3c-42cd-a935-d6fad1dd4d75","values":[{"x":110,"y":550},{"x":560,"y":319},{"x":793,"y":559},{"x":896,"y":9},{"x":673,"y":97},{"x":368,"y":18},{"x":110,"y":550}]}],"marks":[{"type":"line","from":{"data":"392cc31e-4164-4b53-b448-585ca24a4c98"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"a92ce844-ca3c-42cd-a935-d6fad1dd4d75"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"392cc31e-4164-4b53-b448-585ca24a4c98\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"392cc31e-4164-4b53-b448-585ca24a4c98\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"392cc31e-4164-4b53-b448-585ca24a4c98\", :values ({:x 110, :y 550} {:x 560, :y 319} {:x 793, :y 559} {:x 896, :y 9} {:x 673, :y 97} {:x 368, :y 18} {:x 110, :y 550})} {:name \"a92ce844-ca3c-42cd-a935-d6fad1dd4d75\", :values ({:x 110, :y 550} {:x 560, :y 319} {:x 793, :y 559} {:x 896, :y 9} {:x 673, :y 97} {:x 368, :y 18} {:x 110, :y 550})}), :marks ({:type \"line\", :from {:data \"392cc31e-4164-4b53-b448-585ca24a4c98\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"a92ce844-ca3c-42cd-a935-d6fad1dd4d75\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"}
;; <=

;; @@
(defn valid-tour
  "Check validity of tour for given cities."
  [tour cities]
  (and (= (set tour) (set cities))
       (= (count tour) (count cities))))

(defn plot-tsp
  "Apply a TSP algorithm to cities, plot the resulting tour, and print info."
  [algorithm cities]
  (let [tour (time (algorithm cities))]
    [(plot-tour tour)
     (println (count tour) "city tour with length"
              (tour-length tour) "for" (str algorithm))]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/plot-tsp</span>","value":"#'tsp.core/plot-tsp"}
;; <=

;; @@
(plot-tsp alltours-tsp (gen-cities 6))
;; @@
;; ->
;;; &quot;Elapsed time: 469.128519 msecs&quot;
;;; 6 city tour with length 1754.7168567189037 for tsp.core$alltours_tsp@3bb6034f
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310","values":[{"x":766,"y":68},{"x":383,"y":66},{"x":283,"y":158},{"x":570,"y":444},{"x":849,"y":541},{"x":794,"y":45},{"x":766,"y":68}]},{"name":"2afdb907-26b3-4ee5-a7a3-2db706c6fafc","values":[{"x":766,"y":68},{"x":383,"y":66},{"x":283,"y":158},{"x":570,"y":444},{"x":849,"y":541},{"x":794,"y":45},{"x":766,"y":68}]}],"marks":[{"type":"line","from":{"data":"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"2afdb907-26b3-4ee5-a7a3-2db706c6fafc"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310\", :values ({:x 766, :y 68} {:x 383, :y 66} {:x 283, :y 158} {:x 570, :y 444} {:x 849, :y 541} {:x 794, :y 45} {:x 766, :y 68})} {:name \"2afdb907-26b3-4ee5-a7a3-2db706c6fafc\", :values ({:x 766, :y 68} {:x 383, :y 66} {:x 283, :y 158} {:x 570, :y 444} {:x 849, :y 541} {:x 794, :y 45} {:x 766, :y 68})}), :marks ({:type \"line\", :from {:data \"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"2afdb907-26b3-4ee5-a7a3-2db706c6fafc\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310\", :values ({:x 766, :y 68} {:x 383, :y 66} {:x 283, :y 158} {:x 570, :y 444} {:x 849, :y 541} {:x 794, :y 45} {:x 766, :y 68})} {:name \"2afdb907-26b3-4ee5-a7a3-2db706c6fafc\", :values ({:x 766, :y 68} {:x 383, :y 66} {:x 283, :y 158} {:x 570, :y 444} {:x 849, :y 541} {:x 794, :y 45} {:x 766, :y 68})}), :marks ({:type \"line\", :from {:data \"b0fd2eee-d0e9-4bb9-ad9f-13f834b45310\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"2afdb907-26b3-4ee5-a7a3-2db706c6fafc\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]"}
;; <=

;; **
;;; ## All Non-Redundant Tours Algorithm (improved `alltours-tsp`)
;;; We said there are n! tours of n cities, and thus 6 tours of 3 cities:
;; **

;; @@
(alltours [1 2 3])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 2 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 3 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[2 1 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[2 3 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[3 1 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[3 2 1]"}],"value":"([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])"}
;; <=

;; **
;;; But this is redundant: (1, 2, 3), (2, 3, 1), and (3, 1, 2) are three ways of describing the same tour. So let's arbitrarily say that all tours must start with the first city in the set of cities. We'll just pull the first city out, and then tack it back on to all the permutations of the rest of the cities.
;; **

;; @@
(defn non-redundant-alltours
  "Returns a list of all tours with a fixed starting city."
  [cities]
  (map (partial cons (first cities))
       (combo/permutations (rest cities))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/non-redundant-alltours</span>","value":"#'tsp.core/non-redundant-alltours"}
;; <=

;; **
;;; We can verify that for 3 cities, there are only 2 (not 6) tours, and for 4 cities there are 6 tours (not 24).
;; **

;; @@
(non-redundant-alltours [1 2 3])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"(1 2 3)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"(1 3 2)"}],"value":"((1 2 3) (1 3 2))"}
;; <=

;; @@
(non-redundant-alltours [1 2 3 4])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"(1 2 3 4)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"(1 2 4 3)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"(1 3 2 4)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"(1 3 4 2)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"(1 4 2 3)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"(1 4 3 2)"}],"value":"((1 2 3 4) (1 2 4 3) (1 3 2 4) (1 3 4 2) (1 4 2 3) (1 4 3 2))"}
;; <=

;; **
;;; **Note:** We could say that there is only one tour of three cities, because [1, 2, 3] and [1, 3, 2] are in some sense the same tour, one going clockwise and the other counterclockwise. However, I choose not to do that, for two reasons. First, it would mean we can never handle maps where the distance from A to B is different from B to A. Second, it would complicate the code (if only by a line or two) while not saving much run time.
;;; 
;;; We can verify that calling alltours-tsp-imp works and gives the same tour with the same total distance. But it now runs faster:
;; **

;; @@
(defn alltours-tsp-imp                   ; Redefine with new alltours
  "Generate all possible tours of the cities and choose shortest tour."
  [cities]
  (shortest-tour (non-redundant-alltours cities)))

(let [cities (gen-cities 7)]
  [(plot-tsp alltours-tsp cities)      ; Time with old alltours
   (plot-tsp alltours-tsp-imp cities)]) ; New time
;; @@
;; ->
;;; &quot;Elapsed time: 1364.228467 msecs&quot;
;;; 7 city tour with length 2325.2281290279143 for tsp.core$alltours_tsp@3bb6034f
;;; &quot;Elapsed time: 178.749346 msecs&quot;
;;; 7 city tour with length 2325.2281290279143 for tsp.core$alltours_tsp_imp@1d5d6a4a
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"564b2405-b3d2-4aa5-ab60-c6a1341d5164","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"564b2405-b3d2-4aa5-ab60-c6a1341d5164","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"564b2405-b3d2-4aa5-ab60-c6a1341d5164","values":[{"x":86,"y":160},{"x":826,"y":68},{"x":850,"y":376},{"x":758,"y":597},{"x":390,"y":572},{"x":288,"y":308},{"x":23,"y":247},{"x":86,"y":160}]},{"name":"26c1505e-102e-4a62-a220-ee353ab503e4","values":[{"x":86,"y":160},{"x":826,"y":68},{"x":850,"y":376},{"x":758,"y":597},{"x":390,"y":572},{"x":288,"y":308},{"x":23,"y":247},{"x":86,"y":160}]}],"marks":[{"type":"line","from":{"data":"564b2405-b3d2-4aa5-ab60-c6a1341d5164"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"26c1505e-102e-4a62-a220-ee353ab503e4"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})} {:name \"26c1505e-102e-4a62-a220-ee353ab503e4\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})}), :marks ({:type \"line\", :from {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"26c1505e-102e-4a62-a220-ee353ab503e4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})} {:name \"26c1505e-102e-4a62-a220-ee353ab503e4\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})}), :marks ({:type \"line\", :from {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"26c1505e-102e-4a62-a220-ee353ab503e4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6","values":[{"x":86,"y":160},{"x":826,"y":68},{"x":850,"y":376},{"x":758,"y":597},{"x":390,"y":572},{"x":288,"y":308},{"x":23,"y":247},{"x":86,"y":160}]},{"name":"ff41c54f-98b5-42cf-9172-32e2299fa59d","values":[{"x":86,"y":160},{"x":826,"y":68},{"x":850,"y":376},{"x":758,"y":597},{"x":390,"y":572},{"x":288,"y":308},{"x":23,"y":247},{"x":86,"y":160}]}],"marks":[{"type":"line","from":{"data":"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"ff41c54f-98b5-42cf-9172-32e2299fa59d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})} {:name \"ff41c54f-98b5-42cf-9172-32e2299fa59d\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})}), :marks ({:type \"line\", :from {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"ff41c54f-98b5-42cf-9172-32e2299fa59d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})} {:name \"ff41c54f-98b5-42cf-9172-32e2299fa59d\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})}), :marks ({:type \"line\", :from {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"ff41c54f-98b5-42cf-9172-32e2299fa59d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]"}],"value":"[[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})} {:name \"26c1505e-102e-4a62-a220-ee353ab503e4\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})}), :marks ({:type \"line\", :from {:data \"564b2405-b3d2-4aa5-ab60-c6a1341d5164\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"26c1505e-102e-4a62-a220-ee353ab503e4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil] [#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})} {:name \"ff41c54f-98b5-42cf-9172-32e2299fa59d\", :values ({:x 86, :y 160} {:x 826, :y 68} {:x 850, :y 376} {:x 758, :y 597} {:x 390, :y 572} {:x 288, :y 308} {:x 23, :y 247} {:x 86, :y 160})}), :marks ({:type \"line\", :from {:data \"82bf46e5-05f7-4f3f-ab09-c72ec5f5c0b6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"ff41c54f-98b5-42cf-9172-32e2299fa59d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]]"}
;; <=

;; **
;;; ## Complexity of `alltours-tsp`
;;; 
;; **

;; @@

;; @@
