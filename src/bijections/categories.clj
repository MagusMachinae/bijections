(ns bijections.categories)

(defrecord Function [procedure domain co-domain])

(defrecord Set [])

(defrecord Preorder [set relation])

(defrecord Relation [procedure domain co-domain])

(defrecord Poset [])

(defrecord CommutingSquare [top left right bottom])
