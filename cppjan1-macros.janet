(use ./cppjan1-lib ./cppjan1)

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
  (tuple/brackets ;result))

(defmacro class [& args]
  ['type (tuple/brackets (keep-syntax (dyn *macro-form*) [:class ;args]))])

(defmacro defstruct [name & args]
  (def struct-def (keep-syntax (dyn *macro-form*) ~(:struct ,name ,;args)))
  ~(def [typedef ,struct-def] ,name))

(defmacro -t [specs]
  ['type (if (tuple-b? specs) specs (tuple/brackets specs))])

(defmacro -t* [specs]
  ['type (if (tuple-b? specs) specs (tuple/brackets specs)) '(* _)])

(defmacro -d [name specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (if (nil? value)
    ['def specs-val name]
    ['def specs-val name value]))

(defmacro -d* [name specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (if (nil? value)
    ['def specs-val ['* name]]
    ['def specs-val ['* name] value]))

(defmacro -d& [name specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (if (nil? value)
    ['def specs-val ['& name]]
    ['def specs-val ['& name] value]))

(defmacro -f [name params specs]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  ['def specs-val ['fn name params]])
