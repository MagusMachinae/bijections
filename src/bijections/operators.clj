(ns bijections.operators)

(defn resolve-arg-category [& args]
  (let [first-arg (first args)]
    (if (set? first-arg) 
      :set
      (type first-arg))))
       
(defmulti disjoint-union
  resolve-arg-category)

(defmulti cartesian-product
  resolve-arg-category)

(defmulti equaliser
  resolve-arg-category)

(defmulti coequaliser 
  resolve-arg-category)

(defmulti pullback 
  resolve-arg-category)

(defmulti pushout 
  resolve-arg-category)