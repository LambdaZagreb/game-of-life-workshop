(ns clj-gameoflife.core
  (:gen-class))

;; the solution presented here is a variation on Christophe Grand's solution
;; available at: http://clj-me.cgrand.net/2011/08/19/conways-game-of-life/
;;
;; (no peeking!)
;;
;; it's a truly beautiful demonstration of how choosing the right
;; representation of the problem, in combination with certain features of
;; Clojure and Lisps in general, can help create beautiful, succint solutions.

;; ----------------------------------------------------------------------------

;; let's define some interesting patterns to explore:

(def glider #{[2 0] [2 1] [2 2] [1 2] [0 1]})
(def light-spaceship #{[2 0] [4 0] [1 1] [1 2] [1 3] [4 3] [1 4] [2 4] [3 4]})

;; ----------------------------------------------------------------------------

;; step #0: define a create-empty-world function which will accept 2 arguments:
;;   - "w", as the width of the world,
;;   - "h", as the height of the world.
;;
;; as a result, it will return a list of vectors of empty strings ("").
;;
;; read about lists and vectors here:
;;   - http://clojure.org/data_structures#Data%20Structures-Lists%20%28IPersistentList%29
;;   - http://clojure.org/data_structures#Data%20Structures-Vectors%20%28IPersistentVector%29
;;
;; you will probably want to use the following functions:
;;   - "for": http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/for/
;;   - "vec": http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/vec/
;;   - "range": http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/range/

(defn create-empty-world
  [w h]
  (throw (Exception. "Not implemented.")))

;; ----------------------------------------------------------------------------

;; step #1: define a create-world function which will accept 3 arguments:
;;   - "w", as the width of the world,
;;   - "h", as the height of the world,
;;   - "cells", as a set of vectors defining coordinates of alive cells.
;;
;; as a result, it will return a vector of vectors of strings, where the string
;; will be:
;;   - dot (".") if the cell is dead;
;;   - "X" if the cell is alive.
;;
;; read about sets here:
;;   - http://clojure.org/data_structures#Data%20Structures-Sets
;;
;; if you feed the patterns defined at the start of this file to this function,
;; you should get a nice "visual" representation of the world as a return
;; value.

(defn create-world
  [w h cells]
  (throw (Exception. "Not implemented.")))

;; ----------------------------------------------------------------------------

;; step #2: define a "neighbours" function which will return a list of
;; vectors defining coordinates of all neighbors given a single argument:
;; a vector with the coordinates "x" and "y" defining the coordinates of
;; interest.
;;
;; you might want to read up on the ":when" argument of the "for"
;; function:
;; http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/for/
;;
;; it might help you avoid putting [x y] into the list along with the other
;; neighbours. (remember: we *only* want the neighbours in this list, not the
;; coordinate of interest itself!)
;;
;; finally, as an excercise in filtering, filter out all of the coordinates
;; with negative elements, as they're of no interest to us.
;;
;; you'll probably want to check out:
;;   - "filter": http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/filter/
;;
;; the "[x y]" parameter (i.e. a parameter as a vector) is a really nice
;; example of destructuring in Clojure. read up on the concept:
;;   - http://clojure.org/special_forms#Special%20Forms--Binding%20Forms%20%28Destructuring%29

(defn neighbours
  [[x y]]
  (throw (Exception. "Not implemented.")))

;; ----------------------------------------------------------------------------

;; step #3: using the "neighbours" function above, define a "step" function
;; which will take a set of vectors defining the coordinates of living cells,
;; and return a set of vectors defining *all* of the surviving and newly born
;; cells for the next iteration.
;;
;; there are many possible approaches, but one very clean and functional
;; approach does something along the lines of:
;;   - calculate the neighbours of each of the living cells;
;;   - "flatten" the collection of neighbours calculated above;
;;   - count the number of occurences of each neighbour, and then check
;;     whether each of the coordinates from the flattened collection has
;;     survived or needs to be born (depending on whether it already is
;;     alive or not).
;;
;; several functions might be useful if you decide to take this approach:
;;   - "mapcat": http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/mapcat/
;;   - "frequencies": http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/frequencies/
;;   - and, once again, the ":when" argument of "for"

(defn step
  [living-cells]
  (throw (Exception. "Not implemented.")))

;; ----------------------------------------------------------------------------

;; step #4: all together now! using the "step" and "create-world" functions
;; we've defined above, we can print out a certain iteration of the game!
;;
;; if you look carefully, you'll notice that the output of the "step" function
;; can be used as its input, generating the next iteration of the game.
;;
;; with this in mind, we can use Clojure's laziness and some cool functions in
;; the standard library to come up with a very elegant way of arriving at the
;; Nth iteration.
;;
;; you might want to read up on:
;;   - "iterate": http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/iterate/
;;   - "drop": http://conj.io/store/v0/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/drop/

(defn -main
  [& args]
  (throw (Exception. "Not implemented.")))
