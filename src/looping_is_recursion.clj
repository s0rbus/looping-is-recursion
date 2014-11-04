(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base e]
                 (if (zero? e)
                   acc
                   (recur (* acc base) base (dec e))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [coll]
                (if (< (count coll) 2)
                  coll
                  (recur (rest coll))))]
    (first (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [c1 c2]
                (cond
                  (and (empty? c1) (empty? c2))
                     true
                  (or (empty? c1) (empty? c2))
                     false
                  (not (= (first c1) (first c2)))
                     false
                  :else (recur (rest c1) (rest c2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [n 0
         coll a-seq]
    (if (empty? coll)
      nil
      (if (pred (first coll))
        n
        (recur (inc n) (rest coll))))))

(defn avg [a-seq]
  (loop [acc 0
        len  0
        coll a-seq]
     (if (empty? coll)
        (if (> len 0)
           (/ acc len)
           nil)
        (recur (+ acc (first coll)) (inc len) (rest coll)))))

(defn parity [a-seq]
  (loop [acc #{}
         coll a-seq]
     (if (empty? coll)
        acc
        (recur (if (contains? acc (first coll)) (disj acc (first coll)) (conj acc (first coll))) (rest coll)))))

(defn fast-fibo [n]
   (loop [f0 0
          f1 1
          m (dec n)]
      (cond
         (= n 0)
            0
         (= n 1)
            1
         (= m 0)
           (+ f0 f1)
         :else
           (recur (+ f0 f1) f0 (dec m)))))

(defn cut-at-repetition [a-seq]
  (loop [s #{}
         v []
         coll a-seq]
        (if (or (= s (conj s (first coll))) (empty? coll))
          v
          (recur (conj s (first coll)) (conj v (first coll)) (rest coll)))))

