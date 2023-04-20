(import ./cppjan1-lib :prefix "")

# Macros #

(defdyn-local cppjan max-depth
  `How many recursive macro expansions to apply before giving up.`)

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
  [exprs]
  (when (not (tuple? exprs))
    (break exprs))
  (if-not (some splice? exprs)
    exprs
    (do (def result @[])
        (each exp exprs
          (if (splice? exp)
            (for i 0 (length (exp 1))
              (array/push result ((exp 1) i)))
            (array/push result exp)))
        (keep-syntax! exprs result))))

(defn- fn? [f] (or (function? f) (cfunction? f)))

(defn- am-inner [cmacro exprs]
  (when (or (idempotent? exprs) (symbol? exprs) (buffer? exprs))
    (break exprs))

  (when (not (tuple? exprs))
    (error "Arrays, tables, and structs are not allowed in C code."))
  (when (empty? exprs)
    (break exprs))
  (def name (exprs 0))

  # If it is not a macro, recursively apply.
  (unless (and (= (tuple/type exprs) :parens)
               (or (fn? name)
                   (and (symbol? name)
                        (or (special-names name) (cmacro name)))))
    (break (dosplice (keep-syntax! exprs (map |(am-inner cmacro $) exprs)))))

  (def macro-fun (if (fn? name)
                   name
                   (or (special-names name)
                       (cmacro name))))
  (var expanded (apply macro-fun (tuple/slice exprs 1)))
  (when (tuple? expanded)
    (var expanded+ nil)
    (def max-depth (or (dyn *max-depth*) 500))
    (var counter 0)
    (while (not= expanded (set expanded+ (am-inner cmacro expanded)))
      (set expanded expanded+)
      (when (> (++ counter) max-depth)
        (error (string "More than " max-depth " recursive macro invocations")))))
  (dosplice (keep-syntax exprs expanded)))

(defmacro def-macros [macro-kw filetype-kw]
  ~(upscope

    (defn add-to-project
      `Assigns the code to the given file-name in the project. The file name should
  be a string. The macros in the code must already have been expanded using
  apply-macros.`
      [project name expanded-code]
      (set (project name) expanded-code))

    (defn get-macro
      `Returns the cppjan macro value if it exists, nil otherwise.`
      [sym]
      (def parts (string/split "/" sym))
      (def val (dyn (symbol (string/join (array/concat
                                          (array/slice parts 0 (dec (length parts)))
                                          @[,macro-kw (last parts)]) "/"))))
      (if val (val :value) nil))

    (defn apply-macros
      `Applies cppjan/xmljan macros to the given code and returns the result.
  Macro names are determined based on which bindings in the environment have the
  :cppjan/macro or :xmljan metadata set to true. A source-name string must be
  given for error messages to be able to properly display the error's source
  location.

  By default macros will be recursively expanded up to 500 times before giving
  an error. To change this limit, set the dynamic binding :cppjan/max-depth to
  a higher value.`
      [source-name exprs]

      (def known-symbols @{})
      (defn cmacro [symbol]
        (when (nil? (known-symbols symbol))
          (set (known-symbols symbol) (or (get-macro symbol) false)))
        (known-symbols symbol))

      (with-dyns []
        {:code (,am-inner cmacro exprs)
         :source-name source-name
         :filetype ,filetype-kw}))

    (defmacro defile
      `Takes a cppjan project, a symbol or string representing a file name in the
  project, optional definition metadata like in def, and finally code. The
  metadata is the same as what is allowed inside def statements.

  Runs cppjan macros on the code and sets the result into the project for the
  given name (as a string). If the name was a symbol, also defines that symbol
  to the expanded code.`
      [project name & more]
      (assert (or (symbol? name) (string? name)))
      (def meta (take-while |(not (tuple? $)) more))
      (def result (apply-macros
                   (or (dyn *current-file*) "unknown source")
                   (keep-syntax! more (tuple/slice more (length meta)))))
      (if (symbol? name)
        ~(upscope
          (def ,name ,;meta ',result)
          (set (,project ,(string name)) ,name))
          ~((set (,project ,(string name)) ,result))))

    (defn enable-macro
      `Sym must be a symbol which is defined as a function or macro in the
  environment. Enables this macro to be used inside C code.`
      [sym]
      (put (curenv) (symbol (string ,macro-kw "/" sym)) @{:value ((dyn sym) :value)}))

    (defmacro defmacro-
      `Private version of cppjan's defmacro.`
      [name & more]
      (apply defn (symbol (string ,macro-kw "/" name)) :macro :private more))

    (defmacro defmacro
      `Same as the core defmacro, except that the :cppjan/macro or :xmljan/macro
  metadata is also set to true. This enables this macro to be used in
  cppjan/xmljan code.`
      [name & more]
      (apply defn (symbol (string ,macro-kw "/" name)) :macro more))
))
