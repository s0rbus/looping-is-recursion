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
  ":(")

(defn find-first-index [pred a-seq]
  ":(")

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

