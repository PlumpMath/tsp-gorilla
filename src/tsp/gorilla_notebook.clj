;; gorilla-repl.fileformat = 1

;; **
;;; # The Travelling Salesperson Problem in Clojure
;;;
;;; This is a translation of Peter Norvig's greatly informative ipython [notebook](http://nbviewer.ipython.org/url/norvig.com/ipython/TSPv3.ipynb) to Clojure.
;;;
;;; Consider the [Travelling Salesperson Problem](http://en.wikipedia.org/wiki/Traveling_salesman_problem):
;;; > Given a set of cities and the distances between each pair of cities, what is the shortest possible tour that visits each city exactly once, and returns to the starting city?
;;; In this notebook we will develop some solutions to the problem, and more generally show *how to think about solving a problem* like this.
;;;
;;;
;;; <a href="url"><img src="http://www.math.uwaterloo.ca/tsp/history/img/dantzig_big.jpg" align="left" height="270"></a>
;;;
;;; <br>(An example tour.)
;; **

;; **
;;; ###Understanding What We're Talking About (Vocabulary)
;;; Do we understand precisely what the problem is asking? Do we understand all the concepts that the problem talks about?  Do we understand them well enough to implement them in a programming language? Let's take a first pass:
;;;
;;; - **A set of cities**: We will need to represent a set of cities; Clojure's built in `vector` datatype is appropriate.
;;; - **Distance between each pair of cities**: If `a` and `b` are cities, this could be a function, `(distance [a b])`.  The resulting distance will be a real number.
;;; - **City**: All we have to know about an individual city is how far it is from other cities. We don't have to know its name, population, best restaurants, or anything else. So a city could be pair of (x, y) coordinates, if we are using straight-line distance on a plane.
;;; - **Tour**: A tour is a specified order in which to visit the cities; again, Clojure's `vector` can represent this simply. For example, given the set of cities `[a b c d]`, a tour might be the list `[b d a c]`, which means to travel from `b` to `d` to `a` to `c` and finally back to `b`.
;;; - **Shortest possible tour**: The shortest tour is the one whose tour length is the minimum of all tours.
;;; - **Tour length**: The sum of the distances between adjacent cities in the tour (including the last city to the first city). Probably  a function, `(tour-length tour)`.
;;; - **What is ...**: We can define a function to answer the question *what is the shortest possible tour?*  The function takes a set of cities as input and returns a tour as output. I will use the convention that any such function will have a name ending in the letters "`tsp`", the traditional abbreviation for Traveling Salesperson Problem.
;;;
;;; At this stage I have a rough sketch of how to attack the problem.  I don't have all the answers, and I haven't committed to specific representations for all the concepts, but I know what all the pieces are, and I don't see anything that stops me from proceeding."

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
;;; ### All Tours Algorithm `alltours-tsp`
;;; Let's start with a guaranteed correct (but inefficient) algorithm.
;;; > _All Tours Algorithm_: Generate all possible tours of the cities, and choose the shortest tour.
;;;
;;; In general our design philosophy is to first write an English description of the algorithm, then write Clojure code that closely mirrors the English description. This will probably require some auxilliary functions and data structures; just assume they exist; devlare them in a forward declaration, and eventually define them with the same design philosophy.
;;;
;;; Here is the start of the implementation. We declare the functions we can see we will need in the future, and we can refer to them in higher level functions without defining them ahead of time.
;; **

;; @@
(declare alltours-tsp
         alltours      ; Declared because alltours-tsp refers to it
         shortest-tour
         tour-length)  ; Same as above--we will implement them later

(defn alltours-tsp
  "Generate all possible tours of the cities and choose shortest tour."
  [cities]
  (shortest-tour (alltours cities)))

(defn shortest-tour
  "Choose tour with minimum tour length."
  [tours]
  (first (sort-by tour-length tours)))
;; @@

;; **
;;; ### Representing Tours
;;; A tour starts in one city, and then visits each of the other cities in order, before returning to the start city. A natural representation of a tour is a sequence of cities. For example [1 2 3] could represent a tour that starts in city 1, moves to 2, then 3, and finally returns to 1.
;;;
;;; **Note**: I considered using (1, 2, 3, 1) as the representation of this tour. I also considered an ordered list of edges between cities: ((1, 2), (2, 3), (3, 1)). In the end, I decided (1, 2, 3) was simplest.
;; **
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/alltours-tsp</span>","value":"#'tsp.core/alltours-tsp"}
;; <=

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
;;; ### All Non-Redundant Tours Algorithm (improved `alltours-tsp`)
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
;;; We can verify that calling alltours-imp-tsp works and gives the same tour with the same total distance. But it now runs faster:
;; **

;; @@
(defn alltours-imp-tsp                   ; Redefine with new alltours
  "Generate all possible tours of the cities and choose shortest tour."
  [cities]
  (shortest-tour (non-redundant-alltours cities)))

(let [cities (gen-cities 7)]
  [(plot-tsp alltours-tsp cities)      ; Time with old alltours
   (plot-tsp alltours-imp-tsp cities)]) ; New time
;; @@

;; **
;;; ### Complexity of `alltours-tsp`
;;;
;; **
