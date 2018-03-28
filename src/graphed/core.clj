(ns graphed.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn create-image [x y]
  "a function to create a vector of vectors representing the image with width x and length y"
  (vec (for [a (range 0 y)]
         (vec (for [b (range 0 x)] \O)))))

(defn clear-image [image]
  "reset the colour of the image to O"
  (for [row image]
    (vec (for [index (range 0 (count row))]
           \O)))) 

(defn change-pixel [image x y colour]
  "change a pixel in the image to the specified colour"
  (assoc-in image [y x] colour))

(defn color-element-range [row x1 x2 colour]
  "update a row from element x1 to element x2 with the new colour passed in"
  (reduce #(assoc %1 %2 colour) row (range x1 (inc x2))))

(defn draw-horizontal-segment [image x1 x2 y colour]
  "color the image for the y row between the x1 and x2 elements inclusive"
  (if (and (< y (count image)) (>= y 0) (< x1 (count (get image y))) (< x2 (count (get image y))))
    (update image y color-element-range x1 x2 colour)
    (throw (ex-info "invalid input" {:image image :y y :x1 x1 :x2 x2 :colour colour}))))

(defn validate-node-is-in-image [x y image]
  "returns true if x y co-ordinate is in image else false"
  (let [max-y (count image)
        max-x (count (get image 0))]
    (and (>= x 0)
         (< x max-x)
         (>= y 0)
         (< y max-y))))
(defn draw-vertical-segment [image x y1 y2 colour]
  "color the image for the x column between the y1 and y2 elements inclusive"
  (if (and (validate-node-is-in-image x y1 image)
           (validate-node-is-in-image x y2 image))
    (reduce #(assoc-in %1 [%2 x] colour) image (range y1 (inc y2)))
    (do (println "invalid input: x y co-ordinates out of bounds of image")
        image)))

(defn add-valid-node [[x y] target-colour replacement-colour image node-list]
  "add node to node-list if it is within image and of the target colour"
  (if (and (validate-node-is-in-image x y image)
           (= (get-in image [y x]) target-colour))
    (cons [x y] node-list)
    node-list))

(defn get-neighbouring-nodes [image node target-colour replacement-colour]
  "get list of neighbouring nodes that are to have their colour replaced"
  (let [west-node [(dec (get node 0)) (get node 1)]
        east-node [(inc (get node 0)) (get node 1)]
        south-node [(get node 0) (dec (get node 1))]
        north-node [(get node 0) (inc (get node 1))]]
    (->> '()
         (add-valid-node west-node target-colour replacement-colour image)
         (add-valid-node east-node target-colour replacement-colour image)
         (add-valid-node south-node target-colour replacement-colour image)
         (add-valid-node north-node target-colour replacement-colour image))))

(defn process-node [image target-colour replacement-colour node-list]
  "process the node-list, updating the pixel for each node, and adding
   further neighbouring nodes for processing"
  (if (empty? node-list)
    image
    (let [node (first node-list)
          updated-image (change-pixel image (get node 0) (get node 1) replacement-colour)
          neighbouring-nodes  (get-neighbouring-nodes image node target-colour replacement-colour)
          new-node-list (concat (rest node-list) neighbouring-nodes)]
      (process-node updated-image target-colour replacement-colour new-node-list))))

(defn fill-region [image x y replacement-colour]
  "given the co-ordinate x y, change its colour, and all pixels which share a common side with it and each other and are the same colour,
  using flood-fill algorithm https://en.wikipedia.org/wiki/Flood_fill"
  (let [target-colour (get-in image [x y])]
    (cond
      (= target-colour replacement-colour) image
      :else (process-node image target-colour replacement-colour (list [x y])))))

(defn display-image [image]
  "print out the image"
  (do
    (println (apply str (for [row image]
                          (str (apply str row) "\n"))))
    image))

(defn print-error-message [message image]
  (do
    (println message)
    image))

(defn process-command [args image]
  "process the command line passed in"
  (cond
    (= (first args) "I") (if (= (count args) 3)
                           (create-image (Integer/parseInt (second args)) (Integer/parseInt (nth args 2)))
                           (print-error-message "invalid command line, format is: I M N to create MxN image" image))
    (= (first args) "C") (clear-image image)
    (= (first args) "L") (if (= (count args) 4)
                           (change-pixel image (dec (Integer/parseInt (second args))) (dec (Integer/parseInt (nth args 2))) (nth args 3))
                           (print-error-message "invalid command line, format is: L X Y C to change co-ords at X,Y to colour C" image))
    (= (first args) "V") (if (= (count args) 5)
                           (draw-vertical-segment image
                                                  (dec (Integer/parseInt (second args)))
                                                  (dec (Integer/parseInt (nth args 2)))
                                                  (dec (Integer/parseInt (nth args 3)))
                                                  (nth args 4))
                           (print-error-message "invalid command line, format is: V X Y1 Y2 C to change vertical line of pixels to colour C" image))
    (= (first args) "H") (if (= (count args) 5)
                           (draw-horizontal-segment image
                                                    (dec (Integer/parseInt (second args)))
                                                    (dec (Integer/parseInt (nth args 2)))
                                                    (dec (Integer/parseInt (nth args 3)))
                                                    (nth args 4))
                           (print-error-message "invalid command line, format is: H X1 X2 Y C to change vertical line of pixels to colour C" image))
    (= (first args) "F") (if (= (count args) 4)
                           (fill-region image
                                        (dec (Integer/parseInt (second args)))
                                        (dec (Integer/parseInt (nth args 2)))
                                        (nth args 3))
                           (print-error-message "invalid command line, format is: H X1 X2 Y C to change vertical line of pixels to colour C" image))
    (= (first args) "S") (display-image image)
    (= (first args) "X") (System/exit 0)
    :else (print-error-message (str "invalid command: " (first args)) image)))

(defn split-line-and-process [line image]
  "split line by whitespace and then call process-command with the args"
  (try
    (-> line
        (str/split  #"\s+")
        (process-command image))
    (catch Exception e
      (do
        (println (.toString e))
        image))))

(defn -main [& args]
  (reduce #(split-line-and-process %2 %1) '[] (line-seq (java.io.BufferedReader. *in*))))
