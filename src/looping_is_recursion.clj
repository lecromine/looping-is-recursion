(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp)
    1
  (let [helper (fn [acc exp]
                 (if (>= 1 exp)
                   acc
                   (recur (* base acc) (dec exp))))]
    (helper base exp))))

(defn last-element [a-seq]
  (let [helper (fn [n]
                 (if (= (count a-seq) n)
                   (get a-seq (- n 1))
                   (recur (inc n))))]
    (helper 0)))

(defn seq= [seq1 seq2]
  (if ((complement =) (count seq1) (count seq2))
    false
  (let [helper (fn [a-seq b-seq]
                 (cond
                   (and (empty? a-seq) (empty? b-seq)) true
                   ((complement =) (first a-seq) (first b-seq)) false
                   :else (recur (rest a-seq) (rest b-seq))))]
    (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) index
      :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [count 0
         sum 0
         seq a-seq]
    (if (empty? seq)
      (/ sum count)
      (recur (inc count) (+ (first seq) sum) (rest seq)))))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn parity [a-seq]
  (loop [set1 #{}
         seq1 a-seq]
    (if (empty? seq1)
      set1
      (recur (toggle set1 (first seq1))
             (rest seq1)))))

(defn fast-fibo [n]
  (loop [f0 0
         f1 1
         counter 0]
    (if (= counter n)
      f0
      (recur f1 (+ f0 f1) (inc counter)))))

(defn cut-at-repetition [a-seq]
  (loop [first-seq a-seq
         output []]
    (cond
      (empty? first-seq) output

      (contains? output (first first-seq)) (distinct (conj output (first first-seq)))
      :else (recur (rest first-seq) (conj output (first first-seq))))))



(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]

