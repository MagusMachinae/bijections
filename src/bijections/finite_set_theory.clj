(ns bijections.finite-set-theory
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.core.reducers :as reducers]
            [bijections.operators :as ops]
            [bijections.categories :as C]))

(defn function->fgraph [func]
  (into #{} (map (fn [x] [x ((:procedure func) x)])) (:domain func)))

(defn relation->rgraph [{:keys [procedure domain co-domain] :as _rel}]
  (into #{} (filter (partial apply procedure)) (map (fn [x y] [x y]) domain co-domain)))

(defn fgraph->procedure [fn-set]
  (fn)
  (map (fn []) funcset))

(defmethod ops/disjoint-union :set [& sets]
  (let [associated-sets (map (fn [set n] (into #{} (map (fn [element] [element n])) set)) sets (range))]
    (apply set/union associated-sets)))

(defn merge-step [classes]
  (if (empty? classes)
    #{}
    (let [first-class (first classes)
          rest-classes (rest classes)
          ;; Partition the remaining sets into overlapping vs disjoint
          overlapping  (filter #(seq (set/intersection first-class %)) rest-classes)
          disjoint     (filter #(empty? (set/intersection first-class %)) rest-classes)]
      (if (empty? overlapping)
        ;; No overlap: first-class is fully merged, keep it and process the rest
        (conj (merge-step disjoint) first-class)
        ;; Overlap found: union them together and re-run on the updated set list
        (recur (conj disjoint (apply set/union first-class overlapping)))))))

(defn set-coequaliser [set-1 set-2 & fns]
  (let [;; 1. For each e in set-1, build a set of its image targets: #{f1(e) f2(e) ...}
        image-pairs (into #{} (map (comp set (apply juxt fns))) set-1)

        ;; 2. Convert unglued set-2 elements into singleton sets: #{x}
        singletons  (into #{} (map set) (partition 1 set-2))

        ;; 3. Combine initial sets and run the transitive merge
        initial-classes (concat image-pairs singletons)]
    (merge-step initial-classes)))

(defmethod ops/cartesian-product :set [& sets]
  (into #{} (map (fn [element] (into #{} element))) (apply combo/cartesian-product sets)))

(defmethod ops/coequaliser :set [set-1 set-2 & fns]
  (let [glued-targets (into #{} (map (comp (partial into [] (interpose "~"))
                                           (apply juxt fns))
                                     set-1))
        raw-targets (disj (into #{} cat glued-targets) "~")]
    (set/union glued-targets
               (set/difference set-2 raw-targets))))

(defmethod ops/equaliser :set [set-1 set-2 first-fn & rest-fns]
  (filter (fn [element]
            (let [val (first-fn element)]
              (every? (fn [func] (= val (func element))) rest-fns)))
          set-1))

(defmethod ops/pushout :set [set-a & funcs-&-sets]
  (let [k (/ (count funcs-&-sets) 2)
        funcs (take k funcs-&-sets)
        sets (drop k funcs-&-sets)
        acc (map (fn [element] (interpose "~" (map (fn [func] (func element)) funcs))) set-a)]
    (reducers/fold
     (set/union acc (set/difference)))))

(defmethod ops/pullback :set [& funcs-&-sets]
  (let [length (count funcs-&-sets)
        begin (take (dec length) funcs-&-sets)
        last-set (last funcs-&-sets)
        half (/ (count begin) 2)
        sets (take half begin)
        fns (drop half begin)]
    (into #{}
          (filter (fn [element] (let [v (map (fn [f s] (f s)) fns element)]
                                  (every? true? (map = (take (dec (count v)) v) (rest v))))))
          (apply ops/cartesian-product sets))))

(defn function-set [src tgt]
  (let [xf (if)]
    (into #{} (map ops/cartesian-product src tgt))))

(defn inject [set-1 set-2]
  (cond
    (empty? set-1) #{}
    (= 1 (count set-1)) (map (fn [x] [[(first set-1) x]]) set-2)
    :else (map (fn [i] (map (fn [j] (conj [(first set-1) i] j)) (inject (rest set-1) (disj set-2 i)))) set-2)))

(defn injective-function-set [set-1 set-2]
  (into #{} (inject set-1 set-2)))

(defn foldr
  ([f init coll]
   ((reduce (fn [val-fn coll-element]
              (fn [acc] (val-fn (f coll-element acc))))
            identity
            coll)
    init)))

(defn powerset [set-coll]
  (into #{} (map set) (combo/subsets (seq set-coll))))

(defn h [st coll]
      (if (empty? (rest coll))
        (filter (fn [set-element] (<= (count set-element) (first coll))) (powerset st))
        (let [combo-set (powerset (first coll))]
          (map (fn [combination] (conj combination (h (dissoc combination) (rest coll)))) combo-set))))

(defn surjective-function-set [set-1 set-2]
  (map (fn [x] (reduce conj #{} (map (fn [y z] (map (fn [a] [a z]) y))
                                     x set-2)))
       (reduce conj #{} (map (partial h set-1)
                             (map (fn [x] (map - (conj (reverse x) (count set-1))
                                               (reverse (conj x 0))))
                                  (filter (fn [x] (<= x (dec (count set-2))))
                                          (powerset (range 1 (count set-1))))))))
  (foldr conj #{}
         (into #{}
               (comp
                (filter (fn [x] (<= x (dec (count set-2)))))
                (map (fn [x] (map - (conj (reverse x) (count set-1))
                                  (reverse (conj x 0)))))
                (map (partial h set-1))
                (map ops/cartesian-product))
               (powerset (range 1 (count set-1))))))

(defn powerset->boolean-ranged-fn-set [powerset]
  (let [biggest-set (apply max-key count powerset)]
    (into #{} (map (fn [s] (into #{} (map (fn [element] [element (contains? s element)])) biggest-set))) powerset)))

(defn there-exists [{:keys [domain co-domain procedure] :as _func} sbst]
  (if (set/subset? sbst domain)
    (into #{}
          (filter (fn [co-domain-element]
                    (some (fn [subset-element]
                            (= (procedure subset-element) co-domain-element))
                          sbst)))
          co-domain)
    nil))

(defn pre-image [{:keys [domain co-domain procedure] :as _func} sbst]
  (if (set/subset? sbst co-domain)
    (into #{}
          (filter (fn [domain-element]
                    (contains? sbst (procedure domain-element))))
          domain)
    nil))

(defn for-all [{:keys [domain co-domain procedure] :as _func} sbst]
  (if (set/subset? sbst domain)
    (into #{}
          (filter (fn [co-domain-element]
                    (set/subset? (pre-image procedure #{co-domain-element}) sbst)))
          co-domain)
    nil))

(defn existance-function [func]
  (C/->Function (fn [subset] (there-exists func subset))
                (powerset (:domain func))
                (powerset (:co-domain func))))

(defn pre-image-function [func]
  (C/->Function (fn [subset] (pre-image func subset))
                (powerset (:domain func))
                (powerset (:co-domain func))))

(defn for-all-function [func]
  (C/->Function (fn [subset] (for-all func subset))
                (powerset (:domain func))
                (powerset (:co-domain func))))

(defmacro implies [a b]
  `(if ~a (boolean ~b) true))

(defn injective? [{:keys [domain procedure] :as _func}]
  (every? (fn [element] (implies (= (procedure element)
                                    (procedure element))
                                 (= element element)))
          domain))

(defn surjective? [{:keys [domain co-domain procedure] :as _func}]
  (every? (fn [co-dom-el] (some (fn [dom-el] (= (procedure dom-el) co-dom-el)) domain))
          co-domain))

(defn bijective? [func]
  (and (injective? func)
       (surjective? func)))

(defn left-inverse-of? [{:keys [domain] :as func-1} func-2]
  (let [f (:procedure func-1)
        g (:procedure func-2)]
    (and (injective? func-1)
         (every? (fn [dom-el] (= dom-el (g (f dom-el)))) domain))))

(defn right-inverse-of? [{:keys [co-domain] :as func-1} func-2]
  (let [f (:procedure func-1)
        g (:procedure func-2)]
    (and (surjective? func-1)
         (every? (fn [co-dom-el] (= co-dom-el (f (g co-dom-el)))) co-domain))))

(defn inverse-of? [func-1 func-2]
  (and (left-inverse-of? func-1 func-2)
       (right-inverse-of? func-1 func-2)))

(defn idempotent? [{:keys [domain procedure]}]
  (every? (fn [element] (= (procedure element) (procedure (procedure element)))) domain))
