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
;;; 
;; **

;; **
;;; Let's begin by populating the namespace:
;; **

;; **
;;; ### All Tours Algorithm `alltours-tsp`
;;; Let's start with a guaranteed correct (but inefficient) algorithm.
;;; > _All Tours Algorithm_: Generate all possible tours of the cities, and choose the shortest tour.
;;; 
;;; In general our design philosophy is to first write an English description of the algorithm, then write Clojure code that closely mirrors the English description. This will probably require some auxilliary functions and data structures; just assume they exist; devlare them in a forward declaration, and eventually define them with the same design philosophy.
;;; 
;;; Here is the start of the implementation. We declare the functions we will need in the future, and we can refer to them in higher level functions without defining them ahead of time. This serves as a kind of TODO list for implementing later.
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

;; @@
(declare alltours      ; Declared because alltours-tsp refers to it
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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/shortest-tour</span>","value":"#'tsp.core/shortest-tour"}
;; <=

;; **
;;; ### Representing Tours
;;; A tour starts in one city, and then visits each of the other cities in order, before returning to the start city. A natural representation of a tour is a sequence of cities. For example [1 2 3] could represent a tour that starts in city 1, moves to 2, then 3, and finally returns to 1.
;;; 
;;; **Note**: Peter explains that he considered using [1 2 3 1] as the representation of this tour, as well as an ordered list of edges between cities: [[1 2] [2 3] [3 1]], but decided that [1 2 3] was simplest.
;; **

;; @@
(defn alltours
  "Generate all possible tours."
  [cities]
  (combo/permutations cities))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/alltours</span>","value":"#'tsp.core/alltours"}
;; <=

;; **
;;; For *n* cities there are *n*! permutations. Here are all 3! = 6 tours of 3 cities:
;; **

;; @@
(let [cities [1 2 3]]
  (alltours cities))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 2 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 3 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[2 1 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[2 3 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[3 1 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[3 2 1]"}],"value":"([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])"}
;; <=

;; **
;;; The length of a tour is the sum of the lengths of each segment in the tour; in other words, the sum of the distances between consecutive cities in the tour, including from the last city back to the first.
;; **

;; @@
(declare distance) ; Because it is referenced but not yet defined

(defn tour-length
  "Sum of distances between each pair of consecutive cities in the tour."
  [tour]
  (let [destinations (lazy-cat (drop 1 tour) (take 1 tour))
        city-pairs (map vector tour destinations)]
    (apply + (map distance city-pairs))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/tour-length</span>","value":"#'tsp.core/tour-length"}
;; <=

;; **
;;; This function binds `destinations` as a list of the tour cities rotated by 1. Then `city-pairs` is created by mapping through the `tour` and `destinations` lists and building a list of vectors. Given [1 2 3], `city-pairs` looks like [[1 2] [2 3] [3 1]].
;; **

;; **
;;; ### Representing Cities
;;; 
;; **

;; @@
(defn distance
  "Distance between two cities given as a city pair."
  [[a b]]
  (let [x-dist (- (:x a) (:x b))
        y-dist (- (:y a) (:y b))]
    (math/sqrt (+ (math/expt x-dist 2)
                  (math/expt y-dist 2)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tsp.core/distance</span>","value":"#'tsp.core/distance"}
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
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 2 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 3 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[2 1 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[2 3 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[3 1 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[3 2 1]"}],"value":"([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])"}
;; <=

;; @@
(tour-length (alltours-tsp (gen-cities 3)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>755.2423282328176</span>","value":"755.2423282328176"}
;; <=

;; @@
(alltours-tsp (gen-cities 3))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>103</span>","value":"103"}],"value":"[:x 103]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>379</span>","value":"379"}],"value":"[:y 379]"}],"value":"#tsp.core.City{:x 103, :y 379}"},{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>841</span>","value":"841"}],"value":"[:x 841]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>460</span>","value":"460"}],"value":"[:y 460]"}],"value":"#tsp.core.City{:x 841, :y 460}"},{"type":"list-like","open":"<span class='clj-record'>#tsp.core.City{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-unkown'>681</span>","value":"681"}],"value":"[:x 681]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-unkown'>67</span>","value":"67"}],"value":"[:y 67]"}],"value":"#tsp.core.City{:x 681, :y 67}"}],"value":"(#tsp.core.City{:x 103, :y 379} #tsp.core.City{:x 841, :y 460} #tsp.core.City{:x 681, :y 67})"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ccc4f5e2-b98c-444d-bbb6-f861dd1600c2","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ccc4f5e2-b98c-444d-bbb6-f861dd1600c2","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"ccc4f5e2-b98c-444d-bbb6-f861dd1600c2","values":[{"x":736,"y":358},{"x":842,"y":387},{"x":506,"y":63},{"x":80,"y":23},{"x":218,"y":193},{"x":147,"y":396},{"x":736,"y":358}]},{"name":"cf3e74e1-1a47-4219-a511-e009a9eee4ac","values":[{"x":736,"y":358},{"x":842,"y":387},{"x":506,"y":63},{"x":80,"y":23},{"x":218,"y":193},{"x":147,"y":396},{"x":736,"y":358}]}],"marks":[{"type":"line","from":{"data":"ccc4f5e2-b98c-444d-bbb6-f861dd1600c2"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"cf3e74e1-1a47-4219-a511-e009a9eee4ac"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ccc4f5e2-b98c-444d-bbb6-f861dd1600c2\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ccc4f5e2-b98c-444d-bbb6-f861dd1600c2\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"ccc4f5e2-b98c-444d-bbb6-f861dd1600c2\", :values ({:x 736, :y 358} {:x 842, :y 387} {:x 506, :y 63} {:x 80, :y 23} {:x 218, :y 193} {:x 147, :y 396} {:x 736, :y 358})} {:name \"cf3e74e1-1a47-4219-a511-e009a9eee4ac\", :values ({:x 736, :y 358} {:x 842, :y 387} {:x 506, :y 63} {:x 80, :y 23} {:x 218, :y 193} {:x 147, :y 396} {:x 736, :y 358})}), :marks ({:type \"line\", :from {:data \"ccc4f5e2-b98c-444d-bbb6-f861dd1600c2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"cf3e74e1-1a47-4219-a511-e009a9eee4ac\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"}
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
;;; &quot;Elapsed time: 352.173652 msecs&quot;
;;; 6 city tour with length 1947.8056323291223 for tsp.core$alltours_tsp@6eef14a8
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ee346748-e3b2-4060-a391-874619226232","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ee346748-e3b2-4060-a391-874619226232","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"ee346748-e3b2-4060-a391-874619226232","values":[{"x":768,"y":562},{"x":329,"y":572},{"x":102,"y":4},{"x":265,"y":184},{"x":385,"y":191},{"x":588,"y":368},{"x":768,"y":562}]},{"name":"550ea90f-fc7a-4f03-b16a-68b549ffe4a8","values":[{"x":768,"y":562},{"x":329,"y":572},{"x":102,"y":4},{"x":265,"y":184},{"x":385,"y":191},{"x":588,"y":368},{"x":768,"y":562}]}],"marks":[{"type":"line","from":{"data":"ee346748-e3b2-4060-a391-874619226232"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"550ea90f-fc7a-4f03-b16a-68b549ffe4a8"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ee346748-e3b2-4060-a391-874619226232\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ee346748-e3b2-4060-a391-874619226232\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"ee346748-e3b2-4060-a391-874619226232\", :values ({:x 768, :y 562} {:x 329, :y 572} {:x 102, :y 4} {:x 265, :y 184} {:x 385, :y 191} {:x 588, :y 368} {:x 768, :y 562})} {:name \"550ea90f-fc7a-4f03-b16a-68b549ffe4a8\", :values ({:x 768, :y 562} {:x 329, :y 572} {:x 102, :y 4} {:x 265, :y 184} {:x 385, :y 191} {:x 588, :y 368} {:x 768, :y 562})}), :marks ({:type \"line\", :from {:data \"ee346748-e3b2-4060-a391-874619226232\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"550ea90f-fc7a-4f03-b16a-68b549ffe4a8\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ee346748-e3b2-4060-a391-874619226232\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ee346748-e3b2-4060-a391-874619226232\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"ee346748-e3b2-4060-a391-874619226232\", :values ({:x 768, :y 562} {:x 329, :y 572} {:x 102, :y 4} {:x 265, :y 184} {:x 385, :y 191} {:x 588, :y 368} {:x 768, :y 562})} {:name \"550ea90f-fc7a-4f03-b16a-68b549ffe4a8\", :values ({:x 768, :y 562} {:x 329, :y 572} {:x 102, :y 4} {:x 265, :y 184} {:x 385, :y 191} {:x 588, :y 368} {:x 768, :y 562})}), :marks ({:type \"line\", :from {:data \"ee346748-e3b2-4060-a391-874619226232\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"550ea90f-fc7a-4f03-b16a-68b549ffe4a8\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]"}
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
;;; We can verify that calling alltours-imp-tsp works and gives the same tour with the same total distance. But it now runs faster:
;; **

;; @@
(defn alltours-imp-tsp
  "Generate all possible tours of the cities and choose shortest tour."
  [cities]
  (shortest-tour (non-redundant-alltours cities)))

(let [cities (gen-cities 7)]
  [(plot-tsp alltours-tsp cities)       ; Time with old alltours
   (plot-tsp alltours-imp-tsp cities)]) ; New time
;; @@
;; ->
;;; &quot;Elapsed time: 2555.903936 msecs&quot;
;;; 7 city tour with length 1959.22756687452 for tsp.core$alltours_tsp@6eef14a8
;;; &quot;Elapsed time: 132.35775 msecs&quot;
;;; 7 city tour with length 1959.22756687452 for tsp.core$alltours_imp_tsp@2aeba060
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"d47d29d0-97d0-49f5-b2e2-25ae980d2260","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"d47d29d0-97d0-49f5-b2e2-25ae980d2260","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"d47d29d0-97d0-49f5-b2e2-25ae980d2260","values":[{"x":472,"y":165},{"x":582,"y":2},{"x":762,"y":102},{"x":625,"y":247},{"x":421,"y":475},{"x":42,"y":407},{"x":37,"y":176},{"x":472,"y":165}]},{"name":"e9d7f880-9510-43fa-83aa-de5c2b02c9e9","values":[{"x":472,"y":165},{"x":582,"y":2},{"x":762,"y":102},{"x":625,"y":247},{"x":421,"y":475},{"x":42,"y":407},{"x":37,"y":176},{"x":472,"y":165}]}],"marks":[{"type":"line","from":{"data":"d47d29d0-97d0-49f5-b2e2-25ae980d2260"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"e9d7f880-9510-43fa-83aa-de5c2b02c9e9"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})} {:name \"e9d7f880-9510-43fa-83aa-de5c2b02c9e9\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})}), :marks ({:type \"line\", :from {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"e9d7f880-9510-43fa-83aa-de5c2b02c9e9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})} {:name \"e9d7f880-9510-43fa-83aa-de5c2b02c9e9\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})}), :marks ({:type \"line\", :from {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"e9d7f880-9510-43fa-83aa-de5c2b02c9e9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"dcc114ec-e526-4e19-b6f1-45137b95e7bc","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"dcc114ec-e526-4e19-b6f1-45137b95e7bc","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"dcc114ec-e526-4e19-b6f1-45137b95e7bc","values":[{"x":472,"y":165},{"x":582,"y":2},{"x":762,"y":102},{"x":625,"y":247},{"x":421,"y":475},{"x":42,"y":407},{"x":37,"y":176},{"x":472,"y":165}]},{"name":"779f53da-9c6b-4289-8c92-3f66c58a9730","values":[{"x":472,"y":165},{"x":582,"y":2},{"x":762,"y":102},{"x":625,"y":247},{"x":421,"y":475},{"x":42,"y":407},{"x":37,"y":176},{"x":472,"y":165}]}],"marks":[{"type":"line","from":{"data":"dcc114ec-e526-4e19-b6f1-45137b95e7bc"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"symbol","from":{"data":"779f53da-9c6b-4289-8c92-3f66c58a9730"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})} {:name \"779f53da-9c6b-4289-8c92-3f66c58a9730\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})}), :marks ({:type \"line\", :from {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"779f53da-9c6b-4289-8c92-3f66c58a9730\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})} {:name \"779f53da-9c6b-4289-8c92-3f66c58a9730\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})}), :marks ({:type \"line\", :from {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"779f53da-9c6b-4289-8c92-3f66c58a9730\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]"}],"value":"[[#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})} {:name \"e9d7f880-9510-43fa-83aa-de5c2b02c9e9\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})}), :marks ({:type \"line\", :from {:data \"d47d29d0-97d0-49f5-b2e2-25ae980d2260\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"e9d7f880-9510-43fa-83aa-de5c2b02c9e9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil] [#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})} {:name \"779f53da-9c6b-4289-8c92-3f66c58a9730\", :values ({:x 472, :y 165} {:x 582, :y 2} {:x 762, :y 102} {:x 625, :y 247} {:x 421, :y 475} {:x 42, :y 407} {:x 37, :y 176} {:x 472, :y 165})}), :marks ({:type \"line\", :from {:data \"dcc114ec-e526-4e19-b6f1-45137b95e7bc\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"symbol\", :from {:data \"779f53da-9c6b-4289-8c92-3f66c58a9730\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}} nil]]"}
;; <=

;; **
;;; ### Complexity of `alltours-tsp`
;;; 
;; **
