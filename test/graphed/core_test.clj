(ns graphed.core-test
  (:require [clojure.test :refer :all]
            [graphed.core :refer :all]))

(deftest create-image-test
  (is (= '[[\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O]](create-image 3 4))))

(deftest create-image-test-zero-size
  (is (= '[] (create-image 0 0))))

(deftest create-image-test-negative-x
  (is (= '[] (create-image -3 0))))

(deftest create-image-test-negative-y
  (is (= '[] (create-image -3 -3))))

(deftest create-image-test-negative-y
  (is (= '[] (create-image -3 -3))))

(deftest clear-image-test
  (is (= '([\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O]) (clear-image (create-image 3 4)))))

(deftest clear-image-test-2
  (is (= '([\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O]) (clear-image (create-image 3 5)))))

(deftest change-pixel-test-2nd-element-1st-row
  (is (= '[[\O \R \O] [\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O]] (change-pixel (create-image 3 5) 1 0 \R))))

(deftest change-pixel-test-1st-row-1st-element
  (is (= '[[\R \O \O] [\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O]] (change-pixel (create-image 3 5) 0 0 \R))))

(deftest color-vertical-segment-test
  (is (= '[[\O \O \O] [\R \O \O] [\R \O \O] [\O \O \O] [\O \O \O]] (draw-vertical-segment (create-image 3 5) 0 1 2 \R))))

(deftest color-vertical-segment-test-with-empty-image
  (is (= '[] (draw-vertical-segment '[] 0 1 2 \R))))


(deftest color-vertical-segment-test-1-element-range
  (is (= '[[\R \O \O] [\R \O \O] [\O \O \O] [\O \O \O] [\O \O \O]] (draw-vertical-segment (create-image 3 5) 0 0 1 \R))))

(deftest color-horizontal-segment-test
  (is (= '[[\O \O \O] [\R \R \R] [\O \O \O] [\O \O \O] [\O \O \O]] (draw-horizontal-segment (create-image 3 5) 0 2 1 \R))))

(deftest color-horizontal-segment-test-larger-image
  (is (= [[\O \O \O \O] [\O \O \O \O] [\O \R \R \O] [\O \O \O \O] [\O \O \O \O]] (draw-horizontal-segment (create-image 4 5) 1 2 2 \R))))

(deftest color-horizontal-segment-test-3
  (is (= [[\O \O \O] [\R \R \O] [\O \O \O] [\O \O \O]] (draw-horizontal-segment (create-image 3 4) 0 1 1 \R))))

(deftest get-neighbouring-nodes-test
  (is (= '([1 2] [1 0] [2 1] [0 1])(get-neighbouring-nodes (create-image 3 5) [1 1] \O \T))))

(deftest get-neighbouring-nodes-of-edge-node
  (is (=  '([0 2] [0 0] [1 1]) (get-neighbouring-nodes (create-image 3 5) [0 1] \O \T))))

(deftest get-neighbouring-nodes-of-corner-node
  (is (=  '([0 1] [1 0])(get-neighbouring-nodes (create-image 3 5) [0 0] \O \T))))

(deftest process-node-test
  (is (= '[[\T \T \T] [\T \T \T] [\T \T \T] [\T \T \T] [\T \T \T]] (process-node (create-image 3 5) \O \T '([2 2])))))

(deftest process-node-test-with-one-different-pixel
  (is (= '[[\T \R \T] [\T \T \T] [\T \T \T] [\T \T \T] [\T \T \T]] (process-node (change-pixel (create-image 3 5) 1 0 \R) \O \T '([2 2])))))

(deftest process-node-test-with-different-segment
  (is (= '[[\O \O \O] [\R \R \R] [\T \T \T] [\T \T \T] [\T \T \T]] (process-node (draw-horizontal-segment (create-image 3 5) 0 2 1 \R) \O \T '([2 2])))))

(deftest add-valid-node-test
  (is (= '([0 0] []) (add-valid-node [0 0] \O \R (create-image 3 5) [[]]))))

;; (deftest get-neighbouring-nodes )
(deftest fill-region-test
  (is (= '[[\R \R \R] [\R \R \R] [\R \R \R] [\R \R \R] [\R \R \R]] (fill-region (create-image 3 5) 1 1 \R))))

(deftest fill-region-test-where-target=replacement-colour
  (is (= '[[\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O]] (fill-region (create-image 3 5) 0 2 \O))))

(deftest fill-region-for-image-with-2-regions
  (is (= '[[\O \O \O] [\R \R \R] [\T \T \T] [\T \T \T] [\T \T \T]] (fill-region (draw-horizontal-segment (create-image 3 5) 0 2 1 \R) 0 2 \T))))

(deftest fill-region-for-image-with-2-regions-where-one-region-doesn't-partition-the-image
  (is (= '[[\T \T \T] [\R \R \T] [\T \T \T] [\T \T \T] [\T \T \T]] (fill-region (draw-horizontal-segment (create-image 3 5) 0 1 1 \R) 0 2 \T))))

(deftest fill-region-test-target=replacement-colour
  (is (= '[[\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O] [\O \O \O]] (fill-region (create-image 3 5) 0 2 \O))))

