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

;; @@
(tour-length (alltours-tsp (gen-cities 3)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>634.6747913115241</span>","value":"634.6747913115241"}
;; <=

;; @@
(alltours-tsp (gen-cities 3))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>178</span>","value":"178"}],"value":"[:x 178]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>145</span>","value":"145"}],"value":"[:y 145]"}],"value":"#tsp.core.City{:x 178, :y 145}"},{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>693</span>","value":"693"}],"value":"[:x 693]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>260</span>","value":"260"}],"value":"[:y 260]"}],"value":"#tsp.core.City{:x 693, :y 260}"},{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>91</span>","value":"91"}],"value":"[:x 91]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>32</span>","value":"32"}],"value":"[:y 32]"}],"value":"#tsp.core.City{:x 91, :y 32}"}],"value":"(#tsp.core.City{:x 178, :y 145} #tsp.core.City{:x 693, :y 260} #tsp.core.City{:x 91, :y 32})"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"e047d70f-2572-4e5e-9678-396d20ca8bf4","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"e047d70f-2572-4e5e-9678-396d20ca8bf4","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"e047d70f-2572-4e5e-9678-396d20ca8bf4","values":[{"x":448,"y":471},{"x":149,"y":451},{"x":294,"y":138},{"x":817,"y":231},{"x":766,"y":271},{"x":674,"y":407},{"x":448,"y":471}]},{"name":"ba0adcb1-da81-4633-8c44-b221694528bd","values":[{"x":448,"y":471},{"x":149,"y":451},{"x":294,"y":138},{"x":817,"y":231},{"x":766,"y":271},{"x":674,"y":407},{"x":448,"y":471}]}],"marks":[{"type":"line","from":{"data":"e047d70f-2572-4e5e-9678-396d20ca8bf4"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"ba0adcb1-da81-4633-8c44-b221694528bd"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"e047d70f-2572-4e5e-9678-396d20ca8bf4\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"e047d70f-2572-4e5e-9678-396d20ca8bf4\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"e047d70f-2572-4e5e-9678-396d20ca8bf4\", :values ({:x 448, :y 471} {:x 149, :y 451} {:x 294, :y 138} {:x 817, :y 231} {:x 766, :y 271} {:x 674, :y 407} {:x 448, :y 471})} {:name \"ba0adcb1-da81-4633-8c44-b221694528bd\", :values ({:x 448, :y 471} {:x 149, :y 451} {:x 294, :y 138} {:x 817, :y 231} {:x 766, :y 271} {:x 674, :y 407} {:x 448, :y 471})}), :marks ({:type \"line\", :from {:data \"e047d70f-2572-4e5e-9678-396d20ca8bf4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"ba0adcb1-da81-4633-8c44-b221694528bd\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"}
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
;;; &quot;Elapsed time: 113.42818 msecs&quot;
;;; 6 city tour with length 1599.608460755042 for tsp.core$all_tours_tsp@55ec4168
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"c95a83bb-c179-4bef-8c9c-df0972944017","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c95a83bb-c179-4bef-8c9c-df0972944017","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"c95a83bb-c179-4bef-8c9c-df0972944017","values":[{"x":253,"y":538},{"x":415,"y":353},{"x":614,"y":144},{"x":467,"y":14},{"x":452,"y":20},{"x":61,"y":137},{"x":253,"y":538}]},{"name":"70b05175-3194-4778-9842-e8a6bec7d869","values":[{"x":253,"y":538},{"x":415,"y":353},{"x":614,"y":144},{"x":467,"y":14},{"x":452,"y":20},{"x":61,"y":137},{"x":253,"y":538}]}],"marks":[{"type":"line","from":{"data":"c95a83bb-c179-4bef-8c9c-df0972944017"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"70b05175-3194-4778-9842-e8a6bec7d869"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"c95a83bb-c179-4bef-8c9c-df0972944017\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c95a83bb-c179-4bef-8c9c-df0972944017\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"c95a83bb-c179-4bef-8c9c-df0972944017\", :values ({:x 253, :y 538} {:x 415, :y 353} {:x 614, :y 144} {:x 467, :y 14} {:x 452, :y 20} {:x 61, :y 137} {:x 253, :y 538})} {:name \"70b05175-3194-4778-9842-e8a6bec7d869\", :values ({:x 253, :y 538} {:x 415, :y 353} {:x 614, :y 144} {:x 467, :y 14} {:x 452, :y 20} {:x 61, :y 137} {:x 253, :y 538})}), :marks ({:type \"line\", :from {:data \"c95a83bb-c179-4bef-8c9c-df0972944017\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"70b05175-3194-4778-9842-e8a6bec7d869\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"c95a83bb-c179-4bef-8c9c-df0972944017\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c95a83bb-c179-4bef-8c9c-df0972944017\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"c95a83bb-c179-4bef-8c9c-df0972944017\", :values ({:x 253, :y 538} {:x 415, :y 353} {:x 614, :y 144} {:x 467, :y 14} {:x 452, :y 20} {:x 61, :y 137} {:x 253, :y 538})} {:name \"70b05175-3194-4778-9842-e8a6bec7d869\", :values ({:x 253, :y 538} {:x 415, :y 353} {:x 614, :y 144} {:x 467, :y 14} {:x 452, :y 20} {:x 61, :y 137} {:x 253, :y 538})}), :marks ({:type \"line\", :from {:data \"c95a83bb-c179-4bef-8c9c-df0972944017\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"70b05175-3194-4778-9842-e8a6bec7d869\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]"}
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

;; **
;;; We can verify that for 3 cities, there are only 2 (not 6) tours, and for 4 cities there are 6 tours (not 24).
;; **

;; @@
(non-redundant-alltours [1 2 3])
;; @@

;; @@
(non-redundant-alltours [1 2 3 4])
;; @@

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

;; **
;;; ## Complexity of `alltours-tsp`
;;; 
;; **

;; @@

;; @@
