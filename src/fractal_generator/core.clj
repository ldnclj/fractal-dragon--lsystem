(ns fractal-generator.core
  (:require [hiccup.core :as hiccup]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fractal generator for the dragon curve

;; An L-system or Lindenmayer system is a parallel rewriting system and a type of formal grammar.
;; https://en.wikipedia.org/wiki/L-system

;; An L-system consists of an alphabet of symbols that can be used to make strings,
;; a collection of production rules that expand each symbol into some larger string of symbols,
;; an initial "axiom" string from which to begin construction,
;; and a mechanism for translating the generated strings into geometric structures.

;; :F = forward
;; :+ =

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define starting patters
;; and something to rewrite patterns

(def start [:F :X])

(def start-y [:F :Y])

(defn rewrite
"Rewrite a pattern based on a given initial value"
 [i]
  (cond
    (= :X i) [:X :+ :Y :F :+]
    (= :Y i) [:- :F :X :- :Y]
    :true [i]))

(map rewrite start)
;; => ([:F] [:X :+ :Y :F :+])


(map rewrite start-y)
;; => ([:F] [:- :F :X :- :Y])

(mapcat rewrite start)
;; => (:F :X :+ :Y :F :+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tidy the output up a little bit

(defn dragon [segments iter]
  (if (zero? iter)
    segments
    (let [segs (vec (mapcat rewrite segments))]
      (recur segs (- iter 1)))))

(map (partial dragon start) (range 4))
;; => ([:F :X]
;;     [:F :X :+ :Y :F :+]
;;     [:F :X :+ :Y :F :+ :+ :- :F :X :- :Y :F :+]
;;     [:F :X :+ :Y :F :+ :+ :- :F :X :- :Y :F :+ :+ :- :F :X :+ :Y :F :+ :- :- :F :X :- :Y :F :+])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add some noise into the feedback loop

(def pi-by-2 (/ Math/PI 2))

(def vec-size 8)

(defn points [segments theta acc x y]
  (if (empty? segments)
    acc
    (let [s  (first segments)
          xs (rest segments)]
      (cond
        (or (= s :X) (= s :Y)) (recur xs, theta, acc, x, y)
        (= s :+) (recur xs, (+ theta pi-by-2), acc, x, y)
        (= s :-) (recur xs, (- theta pi-by-2), acc, x, y)
        (= s :F) (let [xx (+ x (* vec-size (Math/cos theta)))
                       yy (+ y (* vec-size (Math/sin theta)))
                       new-acc (conj acc (vector xx yy))]
                   (recur xs, theta, new-acc, xx, yy))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render the points using Hiccup SVG

(defn render-svg [plot-points]
  (let [width 1920
        height 1080
        xmlns "http://www.w3.org/2000/svg"
        style "stroke:#551a8b; fill:#00ff00;stroke-width:3"
        points (apply str (map #(str (first %) "," (second %) " ") plot-points))]
    (hiccup/html [:svg {:width width
                        :height height
                        :xmlns xmlns}
                  [:polyline {:points points
                              :style style}]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a patter and put it into an html file

(let [the-dragon  (dragon start 18)
      points      (points the-dragon 0 [] 1000 1000)
      html-points (render-svg points)]
  (spit "dragon50-purple-stroke-width-green-lots.html" html-points))
