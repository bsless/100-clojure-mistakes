(ns bsless.100-clojure-mistakes
  (:require
   [clojure.test :refer [deftest is are]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; # Code and Project Organization

;; ## Forgetting immutability

(let [v []]
  (conj v 1)
  (conj v 2))

;; ## Unintended shadowing

(try
  (let [{:keys [name]} {:name "Alice"}]
    (name :foo))
  (catch Exception e
    (ex-message e)))

;; ## Utility namespaces

'my-repo.utils
'my-repo.common

;; vs

'my-repo.io
'my-repo.error

;; ## Using namespaces like folders

'my-app.repo.aws
'my-app.repo.aws.s3

;; vs

'my-app.aws
'my-app.s3

;; ## TOTALLY UNAMBIGUOUS aliases

'[clojure.string :as clojure-string]
'[some.kind.of.visibility.metrics :as visibility-metrics]

;; vs

'[clojure.string :as str]
'[some.kind.of.visibility.metrics :as metrics]

;; ## Using underscores in namespace names

'my.app.foo_bar

;; ## Not mapping test namespaces to implementation namespaces

'my.app.foo-bar
'my.app.test-foo-bar

;; vs

'my.app.foo-bar
'my.app.foo-bar-test

;; ## Having a constants namespace

'my.app.constants

;; RIP domain knowledge

;;; ### Having multiple constants namespaces

'my.app.foo.constants
'my.app.bar.constants

;; Remember, everything you def is constant. Locate defs close to their usage.

;; ## Refer

;; - refer all
;; - refer none

;; ## Not understanding defs

;; ### Not understanding that everything is constant

(def MY-CONSTANT 10)

;; Is just as constant as

(def my-constant 10)

;; ### Not understanding that everything in constant #2

(defonce a-constant 2)

;; `defonce` is evaluated only once, including on subsequent reevaluation of the form.
;; Use to ensure "heavy" operations only occur once

;; ### Not understanding that everything is constant #3

(def ^:constant b-constant 3)

;; The constant metadata influences compilation and inlines the form when it's referenced.
;; Rarely need to care about it unless dealing with specific performance optimizations

;; ## Misplaced doc strings

(defn wrong [x] "I'm misplaced" x)
(defn right "I'm in the right place" [x] x)

;; ## Misplaced type hints

;; ### Return type hints

(defn ^long badly-hinted [x] (inc x))

;; vs

(defn hinted-right ^long [x] (inc x))

(defn variadic-hinting
  (^long [x] (inc x))
  (^long [x y] (+ x y 1)))

;; ### Argument type hints that don't do anything

(defn read-some-input-stream
  [^java.io.InputStream is]
  (slurp is))

;; Type hints are relevant for java interop and unboxed maths

;; ## Not using linters

;; Plenty of the mistakes above are caught by clj-kondo

;; # Laziness

;; ## Doing lazy side effects

(def nothing-happens (map println (range 100)))

;; Mixes terribly with scoped resources

(def surprise
  (with-open [r (io/reader (io/file "deps.edn"))]
    (map #(str/split % #" ") (line-seq r))))

(vec (take 1 surprise))

(try
  (vec (take 2 surprise))
  (catch Exception e
    (ex-message e)))

;; # Control Structures

;; ## `for` is not a loop

(for [x (range 10)]
  (println x))

;; vs

(doseq [x (range 10)]
  (println x))

;; ## Nesting `for` and `doseq`

;; Let's imagine we want to produce the cartesian product of two sequences

(for [x (range 3)]
  (for [y (range 3)]
    [x y]))

;; Now what? apply concat?

(for [x (range 3)
      y (range 3)]
  [x y])

;; Much better

;; # Sequences

;; ## `flatten`

;; this doesn't even do what we want

(flatten
 (for [x (range 3)]
   (for [y (range 3)]
     [x y])))

(flatten
 (for [x (range 3)]
   (for [y (range 3)]
     {:x x :y y})))

;; Don't use `flatten` when apply concat would do

(apply concat
 (for [x (range 3)]
   (for [y (range 3)]
     [x y])))

;; Often, you don't need either

(for [x (range 3)
      y (range 3)]
  [x y])

;; or

(into
 []
 (mapcat (fn [x] (map (fn [y] [x y]) (range 3))))
 (range 3))

;; Remember `mapcat` exists

(apply concat (map range [1 2 3 4]))
(mapcat range [1 2 3 4])

;; # Strings

;; ## Calling `empty?` on string

(empty? "")
(empty? " ")

;; Use blank?

(str/blank? "")
(str/blank? " ")

;; ## Prefer string methods

(str/includes? "abcd" "c")
(some #{\c} "abcd")

;; ## Unoptimized string concatenation

(let [s "a"
      s (str s "b")
      s (str s "c")
      s (str s "d")]
  (str s "e"))

;; vs

(str "a" "b" "c" "d" "e")

;; Or

(reduce str [1 2 3 4])

(apply str [1 2 3 4])

;; Prefer a single call to `str` as it uses a single string builder.

;; # Predicates

;; ## Using `satisfies?`, ever

(defprotocol IFoo (-foo [x]))

(defn default-foo [x] x)

(defn do-foo [x]
  (if (satisfies? IFoo x)
    (-foo x)
    (default-foo x)))

;; Instead

(extend-protocol IFoo
  Object
  (-foo [x] (default-foo x))
  nil
  (-foo [x] (default-foo x)))

;; Then

(defn do-foo-correctly [x]
  (-foo x))

;; Don't forget to extend to `nil` as well as `Object` for catch-all default handling

;; ## `contains?` works on associative collections

(contains? [3] 3)
(contains? [3] 0)

;; Surprised?

(try
  (contains? (list 3) 0)
  (catch Exception e
    (ex-message e)))

;; # Data Types

;; ## Comparing types

(= (type "foo") String)

;; Use type predicates

(string? "foo")

;; Or instance checks

(instance? String "foo")

;; ## Using deftype when reify would do

(deftype Foo [x]
  IFoo
  (-foo [_] (println x)))

(defn make-foo-with-detype
  [x]
  (->Foo x))

;; vs

(defn make-foo-with-reify
  [x]
  (reify IFoo
    (-foo [_] x)))

;; ## Not understanding which type definition to use

;; https://github.com/cemerick/clojure-type-selection-flowchart

;; ## Mixing type implementation and cross functional implementation

;; Assume we want to use generic error handling around `-foo`.
;; Instead of ensuring this always happens in every implementation, never call directly into `-foo`

(defn make-a-foo
  [x]
  (reify IFoo
    (-foo [_]
      (try
        (println x)
        (catch Exception e
          ;; Good enough, right?
          (println e)
          nil)))))

;; vs

(defn foo
  [x]
  (try
    (-foo x)
    (catch Exception e
      ;; Good enough, right?
      (println e)
      nil)))

;; This also lets us prepare function arguments, ensure defaults, etc

(defprotocol IBar
  (-bar [x opts]))

(def default-bar-opts {:x 1})

(defn bar
  ([x]
   (bar x default-bar-opts))
  ([x opts]
   (-bar x (conj default-bar-opts opts))))

;; A good principle is to always wrap calls into protocol functions.
;; This prepares them for cross functional concern implementation and
;; hides implementation details.

;; # Macros

;; ## duplicate work

(defmacro my-or
  [x y]
  `(if ~x ~x ~y))

(macroexpand-1 '(my-or (transfer-money to from) fail))
;; => (if (transfer-money to from) (transfer-money to from) fail)

;; Oh no, we can accidentally duplicate work when using macros.
;; Use gensyms to capture vars that occur more than once

(defmacro my-correct-or
  [x y]
  `(let [x# ~x]
     (if x# x# ~y)))

(macroexpand-1 '(my-correct-or (transfer-money to from) fail))
;; => (clojure.core/let
;;     [x__12813__auto__ (transfer-money to from)]
;;     (if x__12813__auto__ x__12813__auto__ fail))

;; # Testing

;; Always `(= expected actual)`

;; Don't

(is (= (str/lower-case "ABC") "abx"))

;; Do

(is (= "abx" (str/lower-case "ABC")))

;; Your tooling and reports will thank you

;; ## def or let absolutely everything

;; Please don't

(deftest verbose-test
  (let [input1 "Abc"
        input2 "abc"
        input3 "ABC"
        expected "abc"]
    (is (= expected (str/lower-case input1)))
    (is (= expected (str/lower-case input2)))
    (is (= expected (str/lower-case input3)))))

;; ## Not using table tests

(deftest verbose-test
  (is (= "abc" (str/lower-case "Abc")))
  (is (= "abc" (str/lower-case "abc")))
  (is (= "abc" (str/lower-case "ABC"))))

;; Not really using `are` properly

(deftest verbose-test2
  (are [expected expr]
      (= expected expr)
    "abc" (str/lower-case "Abc")
    "abc" (str/lower-case "abc")
    "abc" (str/lower-case "ABC")))

;; Let `are` repeat code for you, get actually tabulated test

(deftest tabulated-test
  (are [expected expr]
      (= expected (str/lower-case expr))
    "abc" "Abc"
    "abc" "abc"
    "abc" "ABC"))

;; ## Avoid sleep in unit tests

;; Return something that can be waited on for synchronization

;; ## Having to mock System/currentTimeMillis

;; # Interop

;; ## Not turning on reflection warnings in EVERY NAMESPACE with interop

(set! *warn-on-reflection* true)

;; ## Being afraid of interop

;; > Java interop is idiomatic Clojure - Alex Miller

;; There doesn't have to be a pure Clojure implementation for
;; everything, not a Clojure wrapper for every Java client
