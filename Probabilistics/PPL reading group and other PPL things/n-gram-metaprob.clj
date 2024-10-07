(ns ngram
    (:refer-clojure :only [])
    (:require
        [clojure.repl :refer :all]
        [metaprob.state :as state]
        [metaprob.trace :as trace]
        [metaprob.sequence :as sequence]
        [metaprob.builtin-impl :as impl]
        [metaprob.syntax :refer :all]
        [metaprob.builtin :refer :all]
        [metaprob.prelude :refer :all]
        [metaprob.distributions :refer :all]
        [metaprob.interpreters :refer :all]
        [metaprob.inference :refer :all]
        [metaprob.compositional :as comp]
        [metaprob.examples.gaussian :refer :all]
        [metaprob.tutorial.jupyter :refer :all]))

(enable-inline-viz)

(def normalize
    (gen [l]
        (clojure.core/let [sum (apply + l)]
        (map (gen [x] (/ x sum)) l))))

(def hidden-states '(Start N V))
(def vocabulary '(this means nothing))

(def transit-dist-Start '(0 0.7 0.3))
(def transit-dist-N '(0 0.3 0.7))
(def transit-dist-V '(0 0.9 0.1))
(def transit-dists
    (list transit-dist-Start transit-dist-N transit-dist-V))

(def observ-dist-Start '(0 0 0))
(def observ-dist-N '(0.3 0.1 0.5))
(def observ-dist-V '(0 0.8 0.2))
(def observ-dists
  (list observ-dist-Start observ-dist-N observ-dist-V))

(def dist-lookup
    "gets the distribution over states for a given state from the list of dists"
    (gen [state states dists]
    (if (= state (first states))
        (first dists)
    (dist-lookup state (rest states) (rest dists)))))

(def log2 (gen [x] (/ (Math/log x) (Math/log 2))))

(def logsumexp
    (gen [log-vals]
        (clojure.core/let [mx (apply clojure.core/max log-vals)]
        (+ mx (log2 (apply +
            (map (gen [z] (Math/pow 2 z))
            (map (gen [x] (- x mx)) log-vals))))))))


(def logscore-categorical
  "log probability of word 'outcome' given the vocabulary 'outcomes'
  and the list of categorical outcome probabilities 'params'"
  (gen
    [outcome outcomes params]
    (if (= outcome (first outcomes))
        (log2 (first params))
        (logscore-categorical outcome (rest outcomes) (rest params)))))


(def score-next-state-word
    "computes log probability of going from 'curr-state' to
    'next-state' and emmitting 'next-word'"
    (gen [curr-state next-state next-word t-dists o-dists]
      (+
        ;log prob of next-word being emitted by next-state
        (logscore-categorical
          next-word
          vocabulary
          (dist-lookup next-state hidden-states o-dists))
        ;log prob of next-state coming after curr-state
        (logscore-categorical
          next-state
          hidden-states
          (dist-lookup curr-state hidden-states t-dists)))))

(def compute-next-observation-marginal
  "gets the log marginal probability of next word being 'next-observation' given 'curr-state'"
  (gen [curr-state next-observation t-dists o-dists]
      (logsumexp (map
        (gen [next-state]
          (score-next-state-word curr-state next-state next-observation t-dists o-dists))
        ; rest hidden states is just (list N V) in this case
        (rest hidden-states))))

(def compute-next-observation-marginal
  "gets the log marginal probability of next word being 'next-observation' given 'curr-state'"
  (gen [curr-state next-observation t-dists o-dists]
  (logsumexp (map
    (gen [next-state]
      (score-next-state-word curr-state next-state next-observation t-dists o-dists))
    ; rest hidden states is just (list N V) in this case
    (rest hidden-states)))))

(def score-next-states-words
  "computes log prob of going from 'curr-state'
    to sequence of states 'next-states'
    and emmitting sequence of words 'next-words'"
  (gen [curr-state next-states next-words t-dists o-dists]
  (if (= (length next-states) (length next-words))
    (if (= (length next-words) 1)
      (score-next-state-word curr-state (first next-states) (first next-words) t-dists o-dists)
      ; add log prob at curr-state to the recursive call of this function
      ;  on the rest of the states and words
      (+
          (score-next-state-word
            curr-state (first next-states) (first next-words) t-dists o-dists)
          (score-next-states-words
            (first next-states) (rest next-states) (rest next-words) t-dists o-dists)))
    (print "next-states and next-words must be same length"))))

(def compute-next-words-marginal
  "gets the log marginal prob of sequence 'next-words' given 'curr-state'"
  (gen [curr-state next-words t-dists o-dists]
      (if (= (length next-words) 0)
        0
        (logsumexp (map
          (gen [next-state]
            (+
              (score-next-state-word curr-state next-state (first next-words) t-dists o-dists)
              (compute-next-words-marginal next-state (rest next-words) t-dists o-dists)))
          (rest hidden-states))))))

(def compute-hidden-prior
  "log prior probability 'list-of-states' given transition distributions 't-dists'."
  (gen [list-of-states t-dists]
      (if (= (length list-of-states) 1)
        (logscore-categorical
          (first list-of-states)
          hidden-states
          (dist-lookup 'Start hidden-states t-dists))
        (+
          (logscore-categorical
            (last list-of-states)
            hidden-states
            (dist-lookup (last (clojure.core/drop-last list-of-states)) hidden-states t-dists))
          (compute-hidden-prior (clojure.core/drop-last list-of-states) t-dists)))))

(def compute-likelihood-of-words
  "Likelihood (of hidden states) = conditional probability
  of the words in the list-of-words given the list-of-states"
  (gen [list-of-states list-of-words o-dists]
  (if (= (length list-of-states) (length list-of-words))
    (if (= (length list-of-states) 0)
      0
      (+
        (logscore-categorical
          (first list-of-words)
          vocabulary
          (dist-lookup (first list-of-states) hidden-states o-dists))
        (compute-likelihood-of-words (rest list-of-states) (rest list-of-words) o-dists)))
    (print "the two lists must be same length"))))

(def compute-hidden-posterior
  "computes log posterior probability of the 'list-of-states' given the observed 'list-of-words'"
  (gen [list-of-states list-of-words t-dists o-dists]
  ;; by bayes' rule posterior = prior * likelihood / marginal.
  ;; in the log domain, that is prior + likelihood - marginal
  ;; (Note, current-state is hardcoded to 'Start in the marginal function)
  (-
    (+
      (compute-hidden-prior list-of-states t-dists)
      (compute-likelihood-of-words list-of-states list-of-words o-dists))
    (compute-next-words-marginal 'Start list-of-words t-dists o-dists))))

(def compute-next-words-marginal-mem
"(like compute-next-words-marginal, but with memoization)
gets the log marginal prob of sequence 'next-words' given 'curr-state'"
  (clojure.core/memoize (gen [curr-state next-words t-dists o-dists]
    (if (= (length next-words) 0)
      0
      (logsumexp (map
        (gen [next-state]
          (+
            (score-next-state-word curr-state next-state (first next-words) t-dists o-dists)
            (compute-next-words-marginal-mem next-state (rest next-words) t-dists o-dists)))
        hidden-states))))))
