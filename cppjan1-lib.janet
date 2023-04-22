# Syntax #

(defn tuple-b? [t]
  (and (tuple? t) (= (tuple/type t) :brackets)))

(defn tuple-p? [tup]
  (and (tuple? tup) (= (tuple/type tup) :parens)))

(defn or-syntax [& contexts]
  (var result nil)
  (each elem contexts
    (when (and (tuple? elem) (not= (tuple/sourcemap elem) [-1 -1]))
      (set result elem)
      (break)))
  result)

(defn keep-sourcemap [original result]
  (when (and (tuple? original)
             (not= (tuple/sourcemap original) [-1 -1])
             (tuple? result))
    (tuple/setmap result ;(tuple/sourcemap original)))
  result)

# Logic #

(defmacro implies [b0 b1]
  ~(or (not ,b0) ,b1))

(defn iff [b0 b1]
  (and (implies b0 b1)
       (implies b1 b0)))

(defn xor [b0 b1]
  (not (iff b0 b1)))

# Strings and collections #

(def rep string/replace-all)
(def fmt string/format)

(defn contains? [col value]
  (var result false)
  (each elem col
    (when (= elem value)
      (set result true)
      (break)))
  result)

(defn to-set
  `Converts an array or tuple into a set (a table mapping keys to true).`
  [ary]
  (def result (table/new (length ary)))
  (each val ary
    (set (result val) true))
  result)

# Dynamics #

(defmacro defdyn-local
  `Generate a new dynamic :ns/name aliased to *ns/name* and *name*.`
  [ns name & more]
  ~(upscope
    (defdyn ,(symbol (string "*" ns "/" name "*")) ,;more)
    (def ,(symbol (string "*" name "*")) ,;more ,(symbol (string "*" ns "/" name "*")))))

# Public dynamics #

(defdyn-local cppjan source-name
  `Name of the source location of the file being generated.`)

(defdyn-local cppjan max-depth
  `How many recursive macro expansions to apply before giving up.`)

# Private dynamics #

(defdyn-local cppjan current-depth :private
   `How many recursive macro expansions have been applied so far.`)
(defn- depth [] (or (dyn *current-depth*) 0))
(defn- inc-depth [] (setdyn *current-depth* (inc (depth))))
(defn- set-depth [val] (setdyn *current-depth* val))

# Modules #

(defmacro re-export [name]
  (unless (symbol? name)
    (errorf "Expected a symbol, got %v" (type name)))
  (def doc (get (dyn name) :doc))
  (def macro (get (dyn name) :macro))
  (if doc
    ['def name {:macro macro} doc name]
    ['def name {:macro macro} name]))

# Emit all #

(defn emit-all [project]
  (each file-name (keys project)
    (def file-table (project file-name))

    (with
     [file (file/open file-name :w)]
     (:emit file-table file))))

# Errors #

(defn- emiterror [context message & params]
  (def source-name (or (dyn *source-name*) ":unknown-file"))
  (def msg (if (empty? params)
             message
             (fmt message ;params)))
  (print) # flush the stream
  (if (and (tuple? context) (not= (tuple/sourcemap context) [-1 -1]))
    (error (fmt "%s | In file %s near line and column %p"
                msg source-name (tuple/sourcemap context)))
    (error (fmt "%s | In file %s" msg source-name))))

(defmacro cerr [context message & params]
  # Call emiterror, but never tail call optimize this call (for a better stack
  # trace).
  ['do (keep-syntax (dyn *macro-form*) [emiterror context message ;params]) nil])

# Macros #

(defn- c-unquote [expr]
  (eval expr))

(def- special-names
  {'unquote c-unquote})

(defn- splice? [exp]
  (and (tuple? exp) (= (length exp) 2) (= (exp 0) 'splice)))

(defn- dosplice
  `If the argument is not a tuple, returns it. Otherwise applies splice
  operators in the given tuple and returns the result, preserving the tuple type
  and the sourcemap.`
  [exprs context]
  (when (not (tuple? exprs))
    (break exprs))
  (if-not (some splice? exprs)
    exprs
    (do (def result @[])
        (each exp exprs
          (if (splice? exp)
            (do
              (def to-splice (exp 1))
              (unless (or (tuple? to-splice) (array? to-splice))
                (cerr context "Cannot splice type %v" (type to-splice)))
              (for i 0 (length to-splice)
                (array/push result (to-splice i)))
              )
            (array/push result exp)))
        (keep-syntax! exprs result))))

(defn- fn? [f] (or (function? f) (cfunction? f)))

(defn- am-inner [cmacro exprs context]
  (when (or (idempotent? exprs) (symbol? exprs) (buffer? exprs))
    (break exprs))

  (when (not (tuple? exprs))
    (cerr context "Arrays, tables, and structs are not allowed in C code."))
  (when (empty? exprs)
    (break exprs))
  (def name (exprs 0))

  # If it is not a macro, recursively apply.
  (unless (and (= (tuple/type exprs) :parens)
               (or (fn? name)
                   (and (symbol? name)
                        (or (special-names name) (cmacro name)))))
    (break (dosplice (keep-syntax! exprs (map |(am-inner cmacro $ (or-syntax $ context)) exprs)) context)))

  (def macro-fun (if (fn? name)
                   name
                   (or (special-names name)
                       (cmacro name))))

  (def macro-form (dyn *macro-form*))
  (setdyn *macro-form* exprs)
  (var expanded nil)
  (try
    (set expanded (apply macro-fun (tuple/slice exprs 1)))
    ([err throwing-fiber]
     (def stack-trace @"")
     (with-dyns [*err* stack-trace]
       (debug/stacktrace throwing-fiber))

     (if (symbol? name)
       (cerr context "Error in macro \"%s\": %q\nStack trace:\n%s" name err stack-trace)
       (cerr context "Error in macro: %q\nStack trace:\n%s" err stack-trace))))
  (setdyn *macro-form* macro-form)

  (when (tuple? expanded)
    (var expanded+ nil)
    (def max-depth (or (dyn *max-depth*) 500))
    (def current-depth (depth))
    (var newcontext (or-syntax expanded context))

    (when (> (inc-depth) max-depth)
      (cerr context (string "More than " max-depth " recursive macro invocations")))

    (while (not= expanded (set expanded+ (am-inner cmacro expanded context)))
      (set expanded expanded+)
      (set newcontext (or-syntax expanded context))
      (when (> (inc-depth) max-depth)
        (cerr context (string "More than " max-depth " recursive macro invocations"))))
    (set-depth current-depth))
  (dosplice (keep-sourcemap exprs expanded) context))

(defmacro def-macros
  `Defines functions and macros for managing the macros of a given filetype (for
  example, :cpp or :xml). macro-kw is a keyword representing a prefix to use for
  the macros, filetype-kw is the filetype, and emitter is a function that takes
  a code struct and an optional value for *out* and outputs the generated code.`
  [macro-kw filetype-kw emitter]
  ~(upscope

    (defn add-to-project
      `Assigns the code to the given file-name in the project. The file name should
  be a string. The macros in the code must already have been expanded using
  apply-macros.`
      [project name expanded-code]
      (set (project name) expanded-code))

    (defn get-macro-name
      `Resolves the cppjan macro name.`
      [sym]
      (def parts (string/split "/" sym))
      (symbol (string/join (array/concat
                            (array/slice parts 0 (dec (length parts)))
                            @[,macro-kw (last parts)]) "/")))

    (defn get-macro
      `Returns the cppjan macro value if it exists, nil otherwise.`
      [sym]
      (def val (dyn (get-macro-name sym)))
      (if val (val :value) nil))

    (def- emitter
      `A prototype for code structs. It contains the :emit method.`
      {:emit ,emitter})

    (defdyn-local cppjan file-data :private
       `Metadata associated with the file for use with macros.`)
    (defn file-data [] (or (dyn *file-data*) (setdyn *file-data* @{})))

    (defn apply-macros [exprs]
      (def context (or-syntax exprs nil))
      (unless (dyn *source-name*)
        (cerr context "Apply-macros must only be called within a macro evaluation context"))
      (def known-symbols @{})
      (defn cmacro [symbol]
        (when (nil? (known-symbols symbol))
          (set (known-symbols symbol) (or (get-macro symbol) false)))
        (known-symbols symbol))

      (,am-inner cmacro exprs context))

    (defn apply-macros-to-file
      `Applies cppjan/xmljan macros to the given code and returns the result.
  Macro names are determined based on which bindings in the environment have the
  :cppjan/macro or :xmljan metadata set to true. A source-name string must be
  given for error messages to be able to properly display the error's source
  location.

  By default macros will be recursively expanded up to 500 times before giving
  an error. To change this limit, set the dynamic binding *max-depth* to a
  higher value.`
      [source-name exprs]
      (with-dyns [*source-name* source-name]
        (struct/with-proto
         emitter
         :code (apply-macros exprs)
         :source-name source-name
         :filetype ,filetype-kw
         :data (file-data))))

    (defmacro defile
      `Takes a cppjan project symbol, a symbol or string representing a file
  name in the project, optional definition metadata like in def, and finally
  code. The metadata is the same as what is allowed inside def statements.

  Runs cppjan macros on the code and sets the result into the project for the
  given name (as a string). If the name was a symbol, also defines that symbol
  to the expanded code.`
      [project name & more]
      (assert (or (symbol? name) (string? name)))
      (def meta (take-while |(not (tuple? $)) more))
      (def result (apply-macros-to-file
                   (or (dyn *current-file*) "unknown source")
                   (keep-syntax! more (tuple/slice more (length meta)))))
      (if (symbol? name)
        ~(upscope
          (def ,name ,;meta ',result)
          (set (,project ,(string name)) ,name))
          ~((set (,project ,(string name)) ,result))))

    (defn enable-macro
      `Sym must be a symbol which is defined as a function or macro in the
  environment. Enables this macro to be used as a cppjan macro.`
      [sym]
      (put (curenv) (symbol (string ,macro-kw "/" sym)) @{:value ((dyn sym) :value)}))

    (defmacro doc
      `Shows documentation for the given cppjan macro name.`
      [symbol]
      ['doc (get-macro-name symbol)])

    (defn putn [& args]
      (if (< (length args) 2)
        (error "Expected 2 arguments")
        (do
          (var tbl (file-data))
          (for i 0 (- (length args) 2)
            (def key (args i))
            (unless (get tbl key)
              (put tbl key @{}))
            (set tbl (get tbl key)))
          (def before-last (args (- (length args) 2)))
          (put tbl before-last (last args))
          nil)))

    (defn getn [& keys]
      (var tbl (file-data))
      (each key keys
        (set tbl (get tbl key)))
      tbl)

    # Dangerous Names #
    (def- janet-get get)
    (def- janet-put put)
    (defn get [key] (janet-get (file-data) key))
    (defn put [key value] (janet-put (file-data) key value))

    (defmacro defmacro-
      `Private version of cppjan's defmacro.`
      [name & more]
      (apply defn (symbol (string ,macro-kw "/" name)) :macro :private more))

    (defmacro defmacro
      `Same as the core defmacro, except that the :cppjan/macro or :xmljan/macro
  metadata is also set to true. This enables this macro to be used in
  cppjan/xmljan code.`
      [name & more]
      (apply defn (symbol (string ,macro-kw "/" name)) :macro more))))
