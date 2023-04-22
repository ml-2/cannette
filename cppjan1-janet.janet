(use ./cppjan1-lib)
(import ./cppjan1 :as c)

(defdyn-local cppjan1 defjan-meta :private `Janet metadata array`)
(defn- defjan-meta []
  (or (dyn *defjan-meta*) (setdyn *defjan-meta* @[])))

(defdyn-local cppjan1 defjan-hooks :private `Hooks to call on each metadata array`)
(defn- -defjan-hooks [] (or (dyn *defjan-hooks*) (setdyn *defjan-hooks* @[])))

(defn defjan-hooks
  `Returns the array of defjan hooks. Returns nil outside of cppjan macros.`
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
  `Given a tuple of names followed by special type keywords, generates cppjan
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
                      (unless (and (implies optional? (= (length type) 4))
                                   (implies (not optional?) (= (length type) 3)))
                        (cerr context "Wrong number of arguments to :abstract %v" i))
                      (def type-call (type 1))
                      (def type-call (if (symbol? type-call)
                                       ~(type ,[type-call])
                                         type-call))
                      (def abstract-type (type 2))
                      (if optional?
                        (do
                          (def dflt (type 3))
                          (array/push
                           extractor-calls
                           (c/apply-type
                            elem type-call
                            ~(cast
                              ,type-call
                              (janet_optabstract argv argc ,index ,abstract-type ,dflt)))))
                        (do
                          (array/push
                           extractor-calls
                           (c/apply-type
                            elem type-call
                            ~(cast
                              ,type-call
                              (janet_getabstract argv ,index ,abstract-type)))))))
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

  (def arity-call (if (= min max)
                    ['janet-fixarity 'argc min]
                    ['janet-arity 'argc min max]))

  [names arity-call extractor-calls])

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

  (def args ((c/apply-macros (or (dyn c/*source-name*) ":unknown") args) :code))

  (def [names arity-call extractor-calls] (parse-janet-types args context))

  (set doc (string/format "%q\n\n%s" [janet-name ;names] (or doc "")))
  (put metadata :doc doc)

  (def body (slice rest body-start))

  (each hook (-defjan-hooks)
    (hook metadata context))

  (array/push (defjan-meta) metadata)

  ~(defn [static Janet] ,c-name ;,(if doc [doc] []) [(def [int32-t] argc) (def [Janet] (* argv))]
     ,arity-call
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

(c/defmacro module-entry [module-name]
  ~(defn [] JANET_MODULE_ENTRY [(def [JanetTable] (* env))]
    (janet_cfuns env ,(string module-name) cfuns)))
