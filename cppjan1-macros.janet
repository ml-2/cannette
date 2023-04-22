(use ./cppjan1-lib)
(import ./cppjan1 :as c)

(each sym ['-> '->> 'when]
  (c/enable-macro sym))

(c/defmacro if-not [condition & args]
  ['if ['not condition] ;args])

(c/defmacro unless [condition & body]
  ['when ['not condition] ;body])

(c/defmacro dict [& args]
  (def context (or-syntax (dyn *macro-form*) nil))
  (unless (even? (length args))
    (cerr context "Expected an even number of arguments to dict"))
  (def result @[])
  (forv i 0 (length args)
    (array/push result (keep-syntax context ['set (args i) (args (inc i))]))
    (++ i))
  (tuple/brackets ;result))

(c/defmacro class [& args]
  ['type (tuple/brackets (keep-syntax (dyn *macro-form*) [:class ;args]))])

(c/defmacro defstruct [name & args]
  (def struct-def (keep-syntax (dyn *macro-form*) ~(:struct ,name ,;args)))
  ~(def [typedef ,struct-def] ,name))

(c/defmacro -t
  `Shorthand for a type.`
  [specs]
  ['type (if (tuple-b? specs) specs (tuple/brackets specs))])

(c/defmacro *t
  `Shorthand for a pointer type.`
  [specs]
  ['type (if (tuple-b? specs) specs (tuple/brackets specs)) '(* _)])

(c/defmacro **t
  `Shorthand for a pointer to a pointer type.`
  [specs]
  ['type (if (tuple-b? specs) specs (tuple/brackets specs)) '(** _)])

(c/defmacro -d
  `Shorthand to define a variable.`
  [name specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (if (nil? value)
    ['def specs-val name]
    ['def specs-val name value]))

(c/defmacro *d
  `Shorthand to define a pointer variable.`
  [name specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (if (nil? value)
    ['def specs-val ['* name]]
    ['def specs-val ['* name] value]))

(c/defmacro **d
  `Shorthand to define a pointer to a pointer variable.`
  [name specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (if (nil? value)
    ['def specs-val ['** name]]
    ['def specs-val ['** name] value]))

(c/defmacro &d
  `Shorthand to define a reference variable.`
  [name specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (if (nil? value)
    ['def specs-val ['& name]]
    ['def specs-val ['& name] value]))

(c/defmacro &&d
  `Shorthand to define a double reference variable.`
  [name specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (if (nil? value)
    ['def specs-val ['&& name]]
    ['def specs-val ['&& name] value]))

(defn- fn-inner [name rest &opt pointers]
  (when (< (length rest) 2)
    (error "Too few arguments"))
  (var index 0)
  (def doc (if (string? (rest 0))
             (do (++ index) (rest 0))
             nil))
  (def params (rest index))
  (++ index)
  (when (<= (length rest) index)
    (error "Too few arguments"))
  (def specs (rest index))
  (++ index)
  (def body (slice rest index))
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (def name (if pointers [pointers name] name))
  (if (empty? body)
    ['def specs-val [:fn name params]] # TODO: Documentation for defs
    (do
      (def value (if doc
                   ['defn specs-val name doc params ;body]
                   ['defn specs-val name params ;body]))
      (keep-syntax (dyn *macro-form*) value))))

(c/defmacro -fn
  `Shorthand to define a function. If no forms are given, it will be a
  declaration. Cannot be used for functions with empty bodies.`
  [name & rest]
  (fn-inner name rest))

(c/defmacro *fn
  `Shorthand to define a function which returns a pointer. If no forms are
  given, it will be a declaration. Cannot be used for functions with empty
  bodies.`
  [name & rest]
  (fn-inner name rest '*))

(c/defmacro **fn
  `Shorthand to define a function which returns a pointer to a pointer. If no
  forms are given, it will be a declaration. Cannot be used for functions with
  empty bodies.`
  [name & rest]
  (fn-inner name rest '**))

(c/defmacro -f
  `Shorthand to declare a function pointer variable.`
  [name params specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (def f-val [:fn ['* name] params])
  (if (nil? value)
    ['def specs-val f-val]
    ['def specs-val f-val value]))

(c/defmacro *f
  `Define a function pointer variable to a function which returns a pointer.`
  [name params specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (def f-val ['* [:fn ['* name] params]])
  (if (nil? value)
    ['def specs-val f-val]
    ['def specs-val f-val value]))

(c/defmacro **f
  `Define a function pointer variable to a function which returns a pointer to a pointer.`
  [name params specs &opt value]
  (def specs-val (if (tuple-b? specs) specs (tuple/brackets specs)))
  (def f-val ['** [:fn ['* name] params]])
  (if (nil? value)
    ['def specs-val f-val]
    ['def specs-val f-val value]))
