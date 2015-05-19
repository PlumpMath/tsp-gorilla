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
(declare alltours                       ; to be implemented later
         shortest-tour
         tour-length)

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
    (apply + (map (partial apply distance) city-pairs))))
;; @@

;; **
;;; This function binds `destinations` as a list of the tour cities rotated by 1. Then `city-pairs` is created by mapping through the `tour` and `destinations` lists and building a list of vectors. Given [1 2 3], `city-pairs` looks like [[1 2] [2 3] [3 1]].
;; **

;; **
;;; ### Representing Cities
;;; We determined that the only thing that matters about cities is the distance between them. But before we can decide about how to represent cities, and before we can define `(distance a b)`,  we have to make a choice. In the fully general version of the TSP, the \"distance\" between two cities could be anything: it could factor in the amount of time it takes to travel between cities, the twistiness of the road, or anything else. The `(distance a b)` might be different from `(distance b a)`. So the distances could be represented by a matrix `distance[A][B]`, where any entry in the matrix could be any (non-negative) numeric value.
;;; 
;;; But we will ignore the fully general TSP and concentrate on an important special case, the **Euclidean TSP**, where the distance between any two cities is the [Euclidean distance](http://en.wikipedia.org/wiki/Euclidean_distance), the straight-line distance between points in a two-dimensional plane. So a city can be represented by a two-dimensional point: a pair of *x* and *y* coordinates. We will use the record `City`, so that `(City. 300 0)` creates a city with x-coordinate of 300 and y coordinate of 0.  Then `(distance a b)` will be a function that uses the *x* and *y* coordinates to compute the distance between `a` and `b`.
;;; 
;;; ### Representing Points and Computing `distance`
;;; Ok, so a city can be represented as just a two-dimensional point. But how will we represent points? We will be defining a class `City` with the `defrecord` function. This class will have two fields, labeled `:x` and `:y`. They can then be accessed by `(:x some-city)` or `(:y some-city)`.
;;; 
;; **

;; @@
(defrecord City [x y])

(defn distance
  "Distance between two cities given as a city pair."
  [a b]
  (let [x-dist (- (:x a) (:x b))
        y-dist (- (:y a) (:y b))]
    (math/sqrt (+ (math/expt x-dist 2)
                  (math/expt y-dist 2)))))
;; @@

;; **
;;; Here is an example of computing the distance between two cities:
;; **

;; @@
(let [a (City. 3 0)
      b (City. 0 4)]
  (distance a b))
;; @@

;; **
;;; ### Random Sets of Cities
;;; The input to a TSP algorithm should be a list of cities. I can make a random list of *n* cities with the following function which takes the argument *n* for number of cities, and can optionally take width and height (otherwise putting in the default values of 900 and 600). The width and height represent the dimensions of the canvas on which these cities will be generated.
;; **

;; @@
(defn gen-cities
  "Make a set of n cities, each with random coordinates within a width by height rectangle."
  [n & {:keys [w h] :or {w 900 h 600}}]
  (for [i (range n)]
    (City. (rand-int w) (rand-int h))))
;; @@

;; **
;;; For example:
;; **

;; @@
(gen-cities 5)
;; @@

;; **
;;; Now we are ready to apply the `alltours-tsp` and `tour-length` functions to find the shortest tour and its length:
;; **

;; @@
(let [cities (gen-cities 6)]
  [(println "Shortest tour is:" (alltours-tsp cities))
   (println "The distance is:" (tour-length (alltours-tsp cities)))])
;; @@

;; **
;;; Quick, is that the right answer? I have no idea, and you probably can't tell either. But if we could *plot* the tour we'd understand it better and might be able to see at a glance if the tour is optimal.
;;; 
;;; ### Plotting Tours
;;; I define `plot-tour` to plot the cities (as circles) and the tour (as lines). `city->vec` is a helper which takes a city and returns its x and y coordinates inside a vector.
;; **

;; @@
(defn plot-tour
  "Plot the cities as circles and the tour as lines between them."
  [tour]
  (let [city->vec (fn [city] [(:x city) (:y city)])
        tourv (map city->vec (conj (vec tour) (first tour)))]
    (gp/compose (gp/list-plot tourv :joined true)
                (gp/list-plot tourv :symbol-size 50)
                (gp/list-plot [(first tourv)] :colour "#FF0000"))))

(plot-tour (alltours-tsp (gen-cities 6)))
;; @@

;; **
;;; That looks much better! To me, it looks like the shortest possible tour, although I don't have an easy way to prove it. Let's go one step further and define a function, `plot-tsp` that will take a TSP algorithm (such as `alltours-tsp`) and a set of cities, apply the algorithm to the cities to get a tour, plot the tour, and print information about the length of the tour and the time it took to find it:
;; **

;; @@
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
(defn fixed-head-alltours
  "Returns a list of all tours with a fixed starting city."
  [cities]
  (map (partial cons (first cities))
       (combo/permutations (rest cities))))
;; @@

;; **
;;; We can verify that for 3 cities, there are only 2 (not 6) tours, and for 4 cities there are 6 tours (not 24).
;; **

;; @@
(fixed-head-alltours [1 2 3])
;; @@

;; @@
(fixed-head-alltours [1 2 3 4])
;; @@

;; **
;;; **Note:** We could say that there is only one tour of three cities, because [1, 2, 3] and [1, 3, 2] are in some sense the same tour, one going clockwise and the other counterclockwise. However, I choose not to do that, for two reasons. First, it would mean we can never handle maps where the distance from A to B is different from B to A. Second, it would complicate the code (if only by a line or two) while not saving much run time.
;;; 
;;; We can verify that calling alltours-imp-tsp works and gives the same tour with the same total distance. But it now runs faster:
;; **

;; @@
(defn alltours-imp-tsp
  "Generate all possible tours of the cities and choose shortest tour."
  [cities]
  (shortest-tour (fixed-head-alltours cities)))

(let [cities (gen-cities 7)]
  [(plot-tsp alltours-tsp cities)       ; Time with old alltours
   (plot-tsp alltours-imp-tsp cities)]) ; New time
(time (alltours-imp-tsp (gen-cities 8)))
;; @@

;; **
;;; ### Complexity of `alltours-tsp`
;;; However, the improved algorithm is still to slow to run more than 9 cities on my machine. It takes about 1.25 seconds for 8 cities, 13 seconds for 9, and 160 seconds for 10. The following is Peter Norvig's table of expected times.
;;; <table>
;;; <tr><th>n <th>expected time for `alltours_tsp(Cities(n))`
;;; <tr><td>10<td>Covering 10! tours = 2 secs
;;; <tr><td>11<td>2 secs &times; 11! / 10! &approx; 22 secs
;;; <tr><td>12<td>2 secs &times; 12! / 10! &approx; 4 mins
;;; <tr><td>14<td>2 secs &times; 14! / 10! &approx; 13 hours
;;; <tr><td>16<td>2 secs &times; 16! / 10! &approx; 200 days
;;; <tr><td>18<td>2 secs &times; 18! / 10! &approx; 112 years
;;; <tr><td>25<td>2 secs &times; 25! / 10! &approx; <a href=\https://www.google.com/search?q=2+seconds*25!+%2F+10!+in+years\>270 billion years</a>
;;; </table>
;;; 
;;; There must be a better way...
;;; 
;;; ### Approximate Algorithms
;;; 
;;; What if we are willing to settle for a tour that is short, but not guaranteed to be shortest? Then we can save billions of years of compute time: we will show several approximate algorithms, which find tours that are typically within 10% of the shortest possible tour, and can handle thousands of cities in a few seconds.
;;; 
;;; So how do we me up with an approximate algorithm? Here are two general plans of how to create a tour:
;;; - **Nearest Neighbor Algorithm**: Make the tour go from a city to its nearest neighbor. Repeat.
;;; - **Greedy Algorithm**: Find the shortest distance between any two cities and include that edge in the tour. Repeat.
;;; 
;;; ### Nearest Neighbor Algorithm (`nn-tsp`)
;;; 
;;; Here is a description of the nearest neighbor algorithm:
;;; > Nearest Neighbor Algorithm: Start at any city; at each step extend the tour by moving from the previous city to its nearest neighbor that has not yet been visited.
;;; Often the best place for a tour to go next is to its nearest neighbor. But sometimes that neighbor would be better visited at some other point in the tour, so this algorithm is not guaranteed to find the shortest tour.
;;; 
;;; To implement the algorithm I need to represent all the noun phrases in the English description: "any city" (a city; arbitrarily the first city); "the tour" (a list of cities, initialy just the start city); "previous city" (the last element of tour, that is, tour[-1]); "nearest neighbor" (a function that, when given a city, A, and a list of other cities, finds the one with minimal distance from A); and "not yet been visited" (we will keep a set of unvisited cities; initially all cities but the start city are unvisited).
;;; 
;;; The algorithm (in more detail) is as follows:
;;; - Keep track of a partial tour (initially just a single start city) and a list of unvisited cities
;;; - While there are unvisited cities, do the following:
;;;     - find the unvisited city, C, that is nearest to the end city in the tour
;;;     - add C the end of the tour and remove it from the unvisited set
;; **

;; @@
(defn nearest-neighbor
  "Find the city in cities that is nearest to the current city."
  [current cities]
  (first (sort-by #(distance current %) cities)))

(defn nn-tsp
  "Start the tour at the first city; at each step extend the tour by moving from the previous city to its nearest neighbor that has not yet been visited."
  [cities]
  (loop [start (first cities)
         tour [start]
         unvisited (clojure.set/difference (set cities) (set [start]))]
    (if (empty? unvisited)
      tour
      (let [next (nearest-neighbor (last tour) unvisited)]
        (recur next
               (conj tour next)
               (clojure.set/difference unvisited (set [next])))))))
;; @@

;; **
;;; We can compare the algorithms. For reproducible results, we will define test-cities.
;; **

;; @@
(def test-cities-7
  '(#tsp.core.City{:x 275, :y 24} #tsp.core.City{:x 686, :y 174} #tsp.core.City{:x 678, :y 375} #tsp.core.City{:x 64, :y 579} #tsp.core.City{:x 224, :y 164} #tsp.core.City{:x 873, :y 444} #tsp.core.City{:x 631, :y 435} #tsp.core.City{:x 19, :y 109}))

(plot-tsp alltours-imp-tsp test-cities-7)

(plot-tsp nn-tsp test-cities-7)
;; @@

;; **
;;; We can compare the performance of these algorithms on multiple sets of cities.
;; **

;; @@
(defn length-ratio
  "The ratio of the tour lengths for nn-tsp and alltours-tsp."
  [cities]
  (/ (tour-length (nn-tsp cities))
     (tour-length (alltours-imp-tsp cities))))

(sort (for [i (range 11)] (length-ratio (gen-cities 7))))
;; @@

;; **
;;; The ratio of `1.0` means the two algorithms got the same (optimal) result. The other times, we can see that `nn-tsp` produces a longer tour, by anything up to 15% worse.
;;;
;;; But more important than that 0.6% (or even 15%) difference is that the nearest neighbor algorithm can quickly tackle problems that the all tours algorithm can't touch in the lifetime of the universe. Finding a tour of 1000 cities takes about a second:
;; **

;; @@
(plot-tsp nn-tsp (gen-cities 500))
;; @@

;; **
;;; Can we do better? Can we combine the speed of the nearest neighbor algorithm with the optimality of the all tours algorithm?
;;;
;;; Let's consider what went wrong with nn_tsp. Looking at plot_tsp(nn_tsp, Cities(100)), we see that near the end of the tour there are some very long edges between cities, because there are no remaining cities near by. In a way, this just seems like bad luck—the way we flow from neighbor to neighbor just happens to leave a few very-long edges. Just as with buying lottery tickets, we could improve our chance of winning by trying more often; in other words, by using the repetition strategy.
;;;
;;; ### Repeated NN Algorithm (`repeated-nn-tsp`)
;; **

;; @@
(defn nn-start-tsp
  [cities s]
  (loop [start s
         tour [start]
         unvisited (clojure.set/difference (set cities) (set [start]))]
    (if (empty? unvisited)
      tour
      (let [next (nearest-neighbor (last tour) unvisited)]
        (recur next
               (conj tour next)
               (clojure.set/difference unvisited (set [next])))))))

(defn repeated-nn-tsp
  "Repeat the nn-tsp algorithm starting from each city; return the shortest."
  [cities]
  (shortest-tour
   (for [start cities]
     (nn-start-tsp cities start))))
;; @@

;; @@
;; Compare nn-tsp and repeated-nn-tsp
(let [cities (gen-cities 100)]
  [(plot-tsp nn-tsp cities)
   (plot-tsp repeated-nn-tsp cities)])
;; @@

;; **
;;; We see that repeated_nn_tsp does indeed take longer to run, and yields a tour that is shorter.
;;;
;;; Let's try again with a smaller map that makes it easier to visualize the tours:
;; **

;; @@
(plot-tsp nn-tsp test-cities)
(plot-tsp repeated-nn-tsp test-cities)
(plot-tsp alltours-imp-tsp test-cities)
;; @@

;; **
;;; This time the repeated_nn_tsp gives us a tour that is better tha nn_tsp, but not quite optimal. So, it looks like repetition is helping. But if I want to tackle 1000 cities, I don't really want the run time to be 1000 times slower. I'd like a way to moderate the repetition—to repeat the nn_tsp starting from a sample of the cities but not all the cities.
;;;
;;; ### Sampled Repeated Nearest Neighbor Algorithm (improved `repeated-nn-tsp`)
;;; We can give `repeated-nn-tsp` an optional argument specifying the number of different cities to try starting from. We will implement the function sample to draw a random sample of the specified size from all the cities. (In addition, if the sample size, k is None or is larger than the population, then return the whole population.)
;; **


