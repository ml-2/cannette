(import ./cppjan1-lib :prefix "")
(import ./cppjan1 :prefix "")

(each sym ['-> '->> 'when]
  (enable-macro sym))

(defmacro if-not [condition & args]
  ['if ['not condition] ;args])

(defmacro unless [condition & body]
  ['when ['not condition] ;body])

(defmacro dict [& args]
  (def context (or-syntax (dyn *macro-form*) nil))
  (unless (even? (length args))
    (cerr context "Expected an even number of arguments to dict"))
  (def result @[])
  (forv
   i 0 (length args)
   (array/push result (keep-syntax context ['set (args i) (args (inc i))]))
   (++ i))
  (def brak (tuple/brackets ;result))
  (tuple/setmap brak ;(if context (tuple/sourcemap context) [-1 -1]))
  brak)
