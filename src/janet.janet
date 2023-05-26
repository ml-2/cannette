(use ./lib)
(import ./c :as c)

(defdyn-local cannette defjan-meta :private `Janet metadata array`)
(defn- defjan-meta []
  (when (not (c/get *defjan-meta*))
    (c/put *defjan-meta* @[]))
  (c/get *defjan-meta*))

(defdyn-local cannette abstract-types :private `Abstract types`)


(defdyn-local cannette defjan-hooks :private `Hooks to call on each metadata array`)
(defn- -defjan-hooks [] (or (dyn *defjan-hooks*) (setdyn *defjan-hooks* @[])))

(defn defjan-hooks
  `Returns the array of defjan hooks. Returns nil outside of cannette macros.`
  []
  (if (dyn *source-name*)
    (-defjan-hooks)
    (dyn *defjan-hooks*)))

(defn- -defjan-add-hook [func]
  (array/push (-defjan-hooks) func)
  'skip)

(c/defmacro defjan-add-hook
  `Adds a defjan hook in this fiber.`
  [func]
  (-defjan-add-hook (eval func)))

(c/defmacro project-name [name]
  (c/put :project-name name)
  'skip)

(def- basic-type?
  (to-set
   [:number :array :tuple :table :struct :string :cstring
    :symbol :keyword :buffer :fiber :function :cfunction
    :boolean :pointer :nat :integer :integer64 :size
    :indexed :bytes :dictionary]))

(def- optionable-type?
  (to-set
   [:number :array :tuple :table :struct :string :cstring
    :symbol :keyword :buffer :fiber :function :cfunction
    :boolean :pointer :nat :integer :integer64 :size]))

(def- basic-types-map
  '{:number (type [double])
    :array (type [JanetArray] (* _))
    :tuple (type [const Janet] (* _))
    :table (type [JanetTable] (* _))
    :struct (type [const JanetKV] (* _))
    :string (type [const uint8_t] (* _))
    :cstring (type [const char] (* _))
    :symbol (type [const uint8_t] (* _))
    :keyword (type [const uint8_t] (* _))
    :buffer (type [JanetBuffer] (* _))
    :fiber (type [JanetFiber] (* _))
    :function (type [JanetFunction] (* _))
    :cfunction (type [JanetCFunction] (* _))
    :boolean (type [int] _)
    :pointer (type [void] (* _))
    :nat (type [int32_t] _)
    :integer (type [int32_t] _)
    :integer64 (type [int64_t] _)
    :size (type [size_t] _)
    :indexed (type [JanetView] _)
    :bytes (type [JanetByteView] _)
    :dictionary (type [JanetDictView] _)})

(defn- parse-janet-types
  `Given a tuple of names followed by special type keywords, generates cannette
  code that safely extracts those types from argv. Optional arguments must
  provide a default value, and a rest argument is untyped and ignored.`
  [args context]
  (def names @[])

  (def extractor-calls @[])

  (var min 0)
  (var max 0)
  (var index 0)
  (var optional? false)
  (var rest nil)

  (forv i 0 (length args)
    (def elem (args i))
    (array/push names elem)
    (cond
      (= elem '&)
      (do (set max -1)
          (++ i)
          (when (< (inc i) (length args))
            (cerr context "Too many arguments after &"))
          (when (< i (length args))
            (array/push names (args i)))
          (break))
      (= elem '&opt)
      (set optional? true)
      (= (inc i) (length args))
      (cerr context "Expected a type associated with the name %v" elem)
      (do
        (++ i)
        (def type (args i))
        (when (and (not= type :_) (not= max -1))
          (cond (keyword? type)
                (cond
                  (= type :janet)
                  (array/push extractor-calls
                              ~(def [Janet] ,elem (if (< ,index argc)
                                                    (* (+ argv ,index))
                                                    (janet_wrap_nil))))
                  optional?
                  (cerr context "Optional arg must provide a default value at index %v" i)
                  (and (basic-type? type))
                  (array/push extractor-calls
                              (c/apply-type elem (keep-syntax context (basic-types-map type))
                                            ~(,(symbol (string "janet_get" type)) argv ,index)))
                  # else
                  (cerr context "Unknown type %v at index %v" type i))
                (tuple-p? type)
                (do
                  (def context (or-syntax type context))
                  (when (empty? type)
                    (cerr context "Empty type is not allowed at index %v" i))
                  (cond
                    (and optional? (optionable-type? (type 0)))
                    (do
                      (when (not= (length type) 2)
                        (cerr context "Optional type requires exactly one argument, its default value."))
                      (array/push
                       extractor-calls
                       (c/apply-type elem (keep-syntax context (basic-types-map (type 0)))
                                     ~(,(symbol (string "janet_opt" (type 0))) argv argc ,index ,(type 1)))))
                    (= (type 0) :abstract)
                    (do
                      (var type-call nil)
                      (var abstract-type nil)
                      (if (= (length type) 2)
                        (do
                          (def type-data (c/getn *abstract-types* (type 1)))
                          (unless type-data
                            (cerr context "Wrong number of arguments to :abstract %v" i))
                          (def c-type (type-data :c-type))
                          (if (symbol? c-type)
                            (set type-call ['type ~[,(type-data :c-type)] '(* _)])
                            (set type-call c-type))
                          (set abstract-type (type-data :var-name)))
                        (do
                          (unless (and (implies optional? (= (length type) 4))
                                       (implies (not optional?) (= (length type) 3)))
                            (cerr context "Wrong number of arguments to :abstract %v" i))
                          (set type-call (type 1))
                          (set type-call (if (symbol? type-call)
                                           ~(type ,[type-call])
                                             type-call))
                          (set abstract-type (type 2))))
                      (if optional?
                        (do
                          (def dflt (type 3))
                          (array/push
                           extractor-calls
                           (c/apply-type
                            elem type-call
                            ~(cast
                              ,type-call
                              (janet_optabstract argv argc ,index (& ,abstract-type) ,dflt)))))
                        (do
                          (array/push
                           extractor-calls
                           (c/apply-type
                            elem type-call
                            ~(cast
                              ,type-call
                              (janet_getabstract argv ,index (& ,abstract-type))))))))
                    (and optional? (basic-type? (type 0)))
                    (cerr context "Type %v cannot have a default value at index %v" (type 0) i)
                    # else
                    (cerr context "Unrecognized type %v at index %v" (type 0) i)))
                # else
                (cerr context "Special type must be either a keyword or parenthesized tuple at index %v" i)))
        (unless optional?
          (++ min))
        (unless (= max -1)
          (++ max))
        (++ index))))

  (def arity-calls (cond
                    (= min max)
                    [['janet-fixarity 'argc min]]
                    (and (= min 0) (= max -1))
                    []
                    # else
                    [['janet-arity 'argc min max]]))

  [names arity-calls extractor-calls])

(c/defmacro defjan
  `Generate a Janet function. All Janet bindings in a file are tracked and are
  inserted into the first call to the make-janet-cfuns macro. This will
  automatically insert an arity check and extract the values into variables
  based on the args list (which must be assigned special type keywords).`
  [janet-name c-name & rest]
  (def context (or-syntax (dyn *macro-form*) nil))
  (def metadata
    @{:c-name c-name
      :janet-name janet-name})

  (var args nil)
  (var doc nil)
  (var body-start 0)
  (while (< body-start (length rest))
    (def elem (rest body-start))
    (++ body-start)
    (cond
      (tuple-b? elem)
      (do (set args elem)
          (break))
      (keyword? elem)
      (put metadata elem true)
      (struct? elem)
      (merge-into metadata elem)
      (string? elem)
      (set doc elem)
      # else
      (cerr context "Invalid metadata type %v in janet definition" (type elem))))

  (def args (c/apply-macros args))

  (def [names arity-calls extractor-calls] (parse-janet-types args context))

  (put metadata :doc (string/format "%q\n\n%s" [janet-name ;names] (or doc "")))

  (def body (slice rest body-start))

  (each hook (-defjan-hooks)
    (try
      (hook metadata)
      ([err throwing-fiber]
       (def stack-trace @"")
       (with-dyns [*err* stack-trace]
         (debug/stacktrace throwing-fiber))
       (c/cerr context "Error in defjan hook: %q\nStack trace:\n%s" err stack-trace))))

  # When this metadata is set, don't generate arity checks nor extract the values.
  (def [arity-calls extractor-calls]
    (if (get metadata :skip-c-args-check)
      [[] []]
      [arity-calls extractor-calls]))

  # Update the doc with the changes from the hooks (if any)
  (set doc (if (string? (metadata :doc)) (metadata :doc) nil))

  (array/push (defjan-meta) metadata)

  ~(defn [static Janet] ,c-name ;,(if doc [doc] []) [(def [int32-t] argc) (def [Janet] (* argv))]
     ,;arity-calls
     ,;extractor-calls
     ,;body))

(c/defmacro make-janet-cfuns []
  (def ary @[])
  (each elem (defjan-meta)
    (array/push ary (tuple/brackets
                     (string (elem :janet-name))
                     (elem :c-name)
                     (elem :doc))))
  (array/push ary '[NULL NULL NULL])
  (array/clear (defjan-meta))
  ~(def [static const JanetReg] ([] cfuns) ,(tuple/brackets ;ary)))

(c/defmacro module-entry [&opt module-name]
  (def module-name (or module-name (c/get :project-name)))
  (when (not module-name)
    (c/cerr (dyn *macro-form*) "Expected a name for module-name or for the project-name to be set."))
  ~(defn [] JANET_MODULE_ENTRY [(def [JanetTable] (* env))]
     (janet_cfuns env ,(string module-name) cfuns)
     # TODO: register abstract types
     ))


(defn- typed-name [type auto-name given-name]
  (when (and (not given-name) (not (c/get :project-name)))
    (c/cerr (dyn *macro-form*)
            `Required either a variable name as argument or for project-name to be set`))
  (if (or (not given-name) (= given-name '_))
    (symbol (string (c/get :project-name) "_" type "_" auto-name))
    given-name))

(def abstract-type-keys
  '(.name .gc .gcmark .get
    .put .marshal .unmarshal
    .tostring .compare .hash
    .next .call .length .bytes))

(c/defmacro abstract-type [struct]
  (def result @[])
  (each key abstract-type-keys
    (array/push result key)
    (if (nil? (get struct key))
      (array/push result 'NULL)
      (array/push result (get struct key))))
  (eachk key struct
    (unless (index-of key abstract-type-keys)
      (errorf "Unknown abstract type key %v" key)))
  (c/keep-sourcemap (dyn *macro-form*) ~(dict ,;result)))

(c/defmacro defabstract [type-name c-type struct &opt var-name]
  (def name (typed-name type-name 'type var-name))
  (def c-type (c/apply-macros c-type))
  (c/putn *abstract-types*
     type-name @{:type-name type-name :c-type c-type :var-name name})
  ~(def [static const JanetAbstractType] ,name
     (,cannette/macro/abstract-type ,struct)))

(defn type-methods-hook
  `Keep track of all the methods on each abstract type.`
  [method-data]
  (def data (method-data :method))
  (when data
    (when (or (not (tuple? data)) (not= (length data) 2))
      (error "Expected a tuple of length 2 for :method metadata"))
    (def type (data 0))
    (when (c/getn *abstract-types* type :defined?)
      (error "Cannot define method after method array is already created"))

    (var ary (c/getn *abstract-types* type :methods))
    (when (not ary)
      (set ary @[])
      (c/putn *abstract-types* type :methods ary))
    (array/push ary method-data)))

(c/defmacro methods-array
  `Output a JanetMethod method array with all the methods for the given type.
  The type-methods-hook must be enabled for this function to work (otherwise it
  will output an empty array). Use (j/defjan-add-hook j/type-methods-hook) to
  enable the type-methods-hook.`
  [type &opt name]
  (when (and (not name) (not (c/get :project-name)))
    (c/cerr (dyn *macro-form*)
            `Required either a variable name as argument or for project-name to be set`))
  (when (c/getn *abstract-types* type :defined?)
    (c/cerr (dyn *macro-form*) "Cannot output same methods array twice"))

  (def result
    ~(def [static const JanetMethod]
       ([] ,(typed-name type 'methods name))
       ,(do
          (def arys @[])
          (each data (or (c/getn *abstract-types* type :methods) [])
            (def [_ name] (data :method))
            (array/push arys (tuple/brackets (string name) (data :c-name))))
          (array/push arys '[NULL NULL])
          (tuple/brackets ;arys))))
  (c/putn *abstract-types* type :defined? true)
  result)

(c/defmacro declget
  `Declare a getter function for the given abstract type. Set the name to _ or nil
  to automatically use the name [project-name]_[type]_get`
  [name type]
  (def name (typed-name type 'get name))
  ~(def [static int] (fn ,name [(*d data void) (-d key Janet) (*d out Janet)])))

(c/defmacro defget
  `Define a getter function for the given abstract type. Set the name to _ or nil
  to automatically use the name [project-name]_[type]_get. If the tuple is
  empty, automatically generates a body which searches the
  [project-name]_[type]_methods variable to find the appropriate method.`
  [name type & body]
  (def name (typed-name type 'get name))
  (def body
    (if (not (empty? body))
      body
      ~((if (not (janet-checktype key JANET-KEYWORD))
          (return 0))
        (return (janet-getmethod (janet-unwrap-keyword key) ,(typed-name type 'methods nil) out)))))
  ~(defn [static int] ,name [(*d data void) (-d key Janet) (*d out Janet)] ,;body))
