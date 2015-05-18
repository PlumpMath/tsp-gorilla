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

;; **
;;; For *n* cities there are *n*! permutations. Here are all 3! = 6 tours of 3 cities:
;; **

;; @@
(let [cities [1 2 3]]
  (alltours cities))
;; @@

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

;; @@
(defrecord City [x y])

(defn gen-cities [n & {:keys [w h s] :or {w 900 h 600 s 42}}]
  (for [i (range n)]
    (City. (rand-int w) (rand-int h))))
;; @@

;; @@
(alltours [1 2 3])
;; @@

;; @@
(tour-length (alltours-tsp (gen-cities 3)))
;; @@

;; @@
(alltours-tsp (gen-cities 3))
;; @@

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

;; @@
(plot-tour (alltours-tsp (gen-cities 6)))
;; @@

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

;; @@
(plot-tsp alltours-tsp (gen-cities 6))
;; @@

;; **
;;; ### All Non-Redundant Tours Algorithm (improved `alltours-tsp`)
;;; We said there are n! tours of n cities, and thus 6 tours of 3 cities:
;; **

;; @@
(alltours [1 2 3])
;; @@

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
