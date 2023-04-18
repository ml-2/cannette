# Lib #

(defn- tuple-b? [t]
  (and (tuple? t) (= (tuple/type t) :brackets)))

(defn- tuple-p? [tup]
  (and (tuple? tup) (= (tuple/type tup) :parens)))

(defn- or-syntax [& contexts]
  (var result nil)
  (each elem contexts
    (when (and (tuple? elem) (not= (tuple/sourcemap elem) [-1 -1]))
      (set result elem)
      (break)))
  result)

(defn- implies [b0 b1]
  (or (not b0) b1))

(defn- iff [b0 b1]
  (and (implies b0 b1)
       (implies b1 b0)))

(defn- xor [b0 b1]
  (not (iff b0 b1)))

(def- rep string/replace-all)
(def- fmt string/format)

(def- ?dbqt (`"` 0))
(def- ?< (`<` 0))

# Dynamics #

(defmacro- defdyn-local
  `Generate a new dynamic :ns/name aliased to *ns/name* and *name*.`
  [ns name doc]
  ~(upscope
    (defdyn ,(symbol (string "*" ns "/" name "*")) ,doc)
    (def ,(symbol (string "*" name "*")) ,(symbol (string "*" ns "/" name "*")))))

# Public dynamics #

(defdyn-local cppjan source-name
  `Name of the source location of the file being generated.`)

# Private dynamics #

(defdyn-local cppjan indent `Current indent level for cppjan output.`)
(defn- indent [] (or (dyn *indent*) (setdyn *indent* @"")))

(defdyn-local cppjan needs-space
  `Used to temporarily store that a space will be needed if more is printed.`)
(defn- needs-space [] (setdyn *needs-space* true))
(defn- no-needs-space [] (setdyn *needs-space* false))
(defn- emit-space? []
  (when (dyn *needs-space*)
    (do (prin " ")
        (setdyn *needs-space* false))))

# Gencode #

(defn symbol-to-ident
  `Converts a lispified identifier to a C identifier. Basically just transforms
  - into _ and / into ::.`
  [sym]
  (symbol (rep "/" "::" (rep "-" "_" sym))))

(def symbol-expr-grammar
  (peg/compile
   ~{:main (* (any (* :symbol :infix-op)) :symbol -1)
     :infix-op (+ (* "." (constant :.)) (* "->" (constant :.->)))
     :symbol '(some (+ (range "az" "AZ" "09") "_" "/" (if-not "->" "-")))}))

(def ident-conversion-grammar
  (peg/compile
   ~{:main (% (* (any (* :symbol :infix-op)) :symbol -1))
     :infix-op '(+ "." "->")
     :symbol (some (+ '(range "az" "AZ" "09") '"_"
                       (* "/" (constant "::"))
                       (if-not "->" (* "-" (constant "_")))))}))

(defn symbol-expr
  `Parses an expression with -> and . as infix operators. The rest of the symbol
  must be composed of valid lispified C identifiers. If it fails to parse, the
  symbol is returned unmodified. For example, converts a symbol such as
  my-var.field->other-field into (.-> (. my-var field) other-field).`
  [sym]
  (def captures (peg/match symbol-expr-grammar sym))
  (if (not captures)
    sym
    (do
      (defn to-code [n]
        (if (= (+ n 1) (length captures))
          (symbol (captures n))

          [(symbol (captures (+ n 1)))
           (symbol (captures n))
           (to-code (+ n 2))]))
      (to-code 0))))

(def pointers-grammar
  (peg/compile ~(* (some "*") -1)))

(def pointers-op-grammar
  (peg/compile ~(* (some (+ "*" "&")) -1)))

(defn- cpperror [context message & params]
  (def source-name (or (dyn *source-name*) ":unknown-file"))
  (def msg (if (empty? params)
             message
             (fmt message ;params)))
  (print) # flush the stream
  (if context
    (error (fmt "%s | In file %s near line and column %p"
                msg source-name (tuple/sourcemap context)))
    (error (fmt "%s | In file %s" msg source-name))))

(defn- normalize-expr [expr context]
  (case (type expr)
    :nil [:symbol (symbol "nil")]
    :boolean [:symbol (symbol expr)]
    :number [:number expr]
    :tuple (if (empty? expr)
             (if (tuple-p? expr)
               (cpperror context "Empty call is not allowed")
               [:brackets expr])
             [:call expr])
    :string [:string expr]
    :symbol [:symbol expr]
    (cpperror context (fmt "Invalid expression %p" expr))))

(defn- as-symbol [expr]
  (case (type expr)
    :nil (symbol "nil")
    :boolean (symbol expr)
    :symbol expr
    :string (symbol expr)
    nil))

(defn- emit-indent []
  (prin (indent)))

(defn- emit-block-start []
  (print "{")
  (buffer/push-string (indent) "  "))

(defn- emit-block-end []
  (buffer/popn (indent) 2)
  (emit-indent)
  (prin "}"))

(var- emit-def nil)
(var- emit-statement-nonest-block nil)
(var- emit-expr nil)

(defn- emit-ident [ident context]
  (def converted (peg/match ident-conversion-grammar ident))
  (if converted
    (prin (converted 0))
    (prin ident)))

(defn- emit-@include [expr context]
  (unless (= (length expr) 2)
    (cpperror context "Expected 1 argument to @include"))
  (def include (as-symbol (expr 1)))
  (when (or (not include) (empty? include))
    (cpperror context "Expected symbol or string as argument to @include"))
  (prin "#include ")
  (if ({?dbqt true ?< true} (first include))
    (print include)
    (print `"` include `"`)))

(defn- emit-@def [expr context]
  # TODO
  (cpperror context "@def is unimplemented")
  )

(defn- emit-@defn [expr context]
  # TODO
  (cpperror context "@defn is unimplemented")
  )

(defn- emit-attributes [spec context]
  # TODO
  )

(defn- emit-template-type [expr context]
  (when (< (length expr) 2)
    (cpperror context "Expected at least one argument to <>"))
  (def name (expr 1))
  # TODO: Use normalize
  (unless (symbol? name)
    (cpperror context "Expected a symbol for the name"))
  (prin name)
  (prin "<")
  (var ctx context)
  (for i 2 (length expr)
    (def param (expr i))
    (set ctx (or-syntax param ctx))
    # TODO: Use normalize
    (cond
      (symbol? param)
      (emit-ident param context)
      (and (tuple-p? param) (not (empty? param)) (= (param 0) 'type))
      (emit-def param ctx)
      (cpperror ctx "Invalid template parameter %p" param))
    (unless (= (inc i) (length expr))
      (prin ", ")))
  (prin ">"))

(defn- emit-specifier [spec context]
  # TODO: Use normalize
  (cond
    (symbol? spec)
    (emit-ident spec context)
    (and (tuple-p? spec) (not (empty? spec)) (symbol? (spec 0)))
    (case (keyword (spec 0))
      :<> (emit-template-type spec context)
      :class nil # TODO
      :struct nil # TODO
      :enum nil # TODO
      :union nil # TODO
      :decltype nil # TODO
      (cpperror
       context
       "Unknown specifier type - use (<> %p parameters) if it was meant to be a template type" (spec 0)))
    (cpperror context "Invalid specifier %p" spec)))

(defn- emit-declarator [kind decl context]
  # kind can be :type or :def. For the former, the name should be _.
  (def normalized (normalize-expr decl context))
  (case (normalized 0)
    :string (cpperror context "Invalid declaration %p" decl)
    :number (cpperror context "Invalid declaration %p" decl)
    :brackets (cpperror context "Invalid declaration (tuple with brackets)")
    :symbol (if (and (symbol? decl) (= kind :type) (not= decl '_))
              (cpperror context "The declarator name of a type must be set to _")
              (when (= kind :def)
                (emit-space?)
                (emit-ident decl context)))
    :call (do
            (def name (decl 0))
            (unless (or (symbol? name) (tuple-b? name))
              (cpperror context "Invalid function name"))
            (cond
              (tuple-b? name)
              (cpperror context "Unimplemented") # TODO

              (peg/match pointers-grammar name)
              (do
                (emit-space?)
                (prin name)
                (when (< (length decl) 2)
                  (cpperror context "Wrong number of arguments to %p" name))
                (def inner-decl (decl 1))
                (for i 2 (length decl)
                  (def elem (decl i))
                  (cond (and (= (inc i) (length decl)) (tuple-b? elem))
                        (emit-attributes elem (or-syntax elem context))
                        (symbol? elem)
                        (emit-ident elem context)
                        (cpperror (or-syntax elem context) "Invalid metadata %p for pointer declarator" elem))
                  (if (= (inc i) (length decl))
                    (needs-space)
                    (prin " ")))
                (emit-declarator kind inner-decl (or-syntax inner-decl context))
                (no-needs-space))

              # TODO: Qualified pointer declarators

              (case (keyword name)
                :decl (do
                        (when (or (< (length decl) 2) (not (symbol? (decl 1))))
                          (cpperror context "Expected a name for decl"))
                        (emit-space?)
                        (prin (decl 1))
                        (when (= (length decl) 3)
                          (needs-space)
                          (emit-attributes (decl 2) (or-syntax (decl 2) context))
                          (no-needs-space)))
                :operator (cpperror context "Unimplemented declarator name %p" name) # TODO
                :... (cpperror context "Unimplemented declarator name %p" name) # TODO
                :& (cpperror context "Unimplemented declarator name %p" name) # TODO
                :&& (cpperror context "Unimplemented declarator name %p" name) # TODO
                :fn (do
                      (when (not= (length decl) 4)
                        (cpperror context "Expected 3 args to fn"))

                      (def fn-decl (decl 1))

                      (def params (decl 2))
                      (when (not (tuple-b? params))
                        (cpperror context "Expected brackets tuple for params"))

                      (def fn-info (decl 3))

                      (emit-declarator kind fn-decl (or-syntax fn-decl context))
                      (if (= params '[void])
                        (prin "(void)")
                        (do
                          (prin "(")
                          (for i 0 (length params)
                            (def param (params i))
                            (unless (and (tuple-p? param) (not (empty? param))
                                         (= 'def (param 0)))
                              (cpperror context "Expected a definition for a param"))
                            (emit-def param (or-syntax param params context))
                            (unless (= (inc i) (length params))
                              (prin ", ")))
                          (prin ")")))
                      # TODO: Emit fn-info
                      )
                (cpperror context "Unknown declarator name %p" name))))
     (cpperror
      context
      "Unknown normalized type %p - this is a bug in cppjan" (normalized 0))))

(def- uops
  '{+ "+" - "-" * "*" & "&" not "!" bnot "~" ++ "++" -- "--"})

(def- binops
  '{+ "+" - "-" * "*" / "/" % "%"
    and "&&" or "||" band "&" bor "|" bxor "^"
    << "<<" >> ">>" = "==" not= "!=" > ">" < "<" >= ">=" <= "<=" <=> "<=>"
   . "." .-> "->" .* ".*" .->* ".->*"})

(defn- emit-number [expr context]
  # TODO: By default numeric values, only allow integers before precision is lost.
  # TODO: For other numeric values, use something like (num/f 1.1) syntax.
  (cond
    (not= (math/round expr) expr)
    (cpperror context "Expected an integer (floating points are unimplemented)") # TODO
    (<= expr math/int-min)
    (cpperror context "Integer too small (floating points are unimplemented)")
    (>= expr math/int-max)
    (cpperror context "Integer too large (floating points are unimplemented)"))
  (prin expr))

(defn- emit-string [expr context]
  # TODO: Escape characters? Multi-strings?
  (prin `"` expr `"`))

(defn- emit-uop [expr context &opt with-parens?]
  (when with-parens? (prin "("))
  (prin (uops (expr 0)))
  (emit-expr (expr 1) (or-syntax (expr 1) context) true)
  (when with-parens? (prin ")")))

(defn- emit-binop [expr context &opt with-parens?]
  (when with-parens?
    (prin "("))
  (emit-expr (expr 1) (or-syntax (expr 1) context) true)
  (prin " " (binops (expr 0)) " ")
  (emit-expr (expr 2) (or-syntax (expr 2) context) true)
  (when with-parens?
    (prin ")")))

(varfn emit-expr [expr context &opt with-parens?]
  (def normalized (normalize-expr expr context))
  (case (normalized 0)
    :string (emit-string expr context)
    :number (emit-number expr context)
    :brackets (cpperror context "Bracket expressions are unimplemented") # TODO
    :symbol (prin expr)
    :call (do
            (def name (expr 0))
            (unless (symbol? name)
              (cpperror context "Expected symbol as function name"))
            # TODO: handle operators, function calls, and so on.
            # TODO: First, check if it is a *& pattern with a single argument.
            (cond
              (and (peg/match pointers-op-grammar name)
                   (implies (= name '*) (= (length expr) 2)))
              (if (not= (length expr) 2)
                (cpperror context "Expected exactly one argument to %p" name)
                (do
                  (prin "(" name)
                  (emit-expr (expr 1) (or-syntax (expr 1) context))
                  (prin ")")))
              (and (= (length expr) 2)
                   (uops name))
              (emit-uop expr context with-parens?)
              (binops name)
              (emit-binop expr context with-parens?)
              (case (keyword name)
                :set (do
                       # TODO: Validation
                       (emit-expr (expr 1) (or-syntax (expr 1) context))
                       (prin " = ")
                       (emit-expr (expr 2) (or-syntax (expr 2) context)))
                :cast (do
                        (unless (>= (length expr) 3)
                          (cpperror context "Expected at least 2 arguments to cast"))
                        (when with-parens?
                          (prin "("))
                        (prin "(")
                        (def type-call (if (= (length expr) 3)
                                         ['type (expr 1) '_]
                                         ['type ;(slice expr 1 (- (length expr) 1))]))
                        (emit-def type-call context)
                        (prin ")")
                        (emit-expr (last expr) (or-syntax (last expr) context))
                        (when with-parens?
                          (prin ")")))
                (do
                  # Regular function call
                  (emit-ident name context)
                  (prin "(")
                  (for i 1 (length expr)
                    (emit-expr (expr i) (or-syntax (expr i) context))
                    (unless (= (inc i) (length expr))
                      (prin ", ")))
                  (prin ")")))))
    (cpperror
     context
     "Unknown normalized type %p - this is a bug in cppjan" (normalized 0))))

(defn- emit-cond [expr context]
  (when (< (length expr) 2)
    (cpperror context "Conditional branch requires a condition"))
  (when (and (> (length expr) 4) (= (expr 0) 'if))
    (cpperror context "Too many arguments to if"))
  (forv
   i 1 (length expr)
   (cond
     (= i 1)
     (do
       (emit-indent)
       (prin "if (")
       (emit-expr (expr i) (or-syntax (expr i) context))
       (prin ") ")
       (emit-block-start)
       (++ i)
       (emit-statement-nonest-block (expr i) (or-syntax (expr i) context))
       (emit-block-end))
     (= (inc i) (length expr))
     (do
       (prin " else ")
       (emit-block-start)
       (emit-statement-nonest-block (expr i) (or-syntax (expr i) context))
       (emit-block-end))
     (do
       (prin " else if (")
       (emit-expr (expr i) (or-syntax (expr i) context))
       (prin ") ")
       (emit-block-start)
       (++ i)
       (emit-statement-nonest-block (expr i) (or-syntax (expr i) context))
       (emit-block-end))))
  (print))

(defn- emit-statement [expr context]
  (def normalized (normalize-expr expr context))
  (case (normalized 0)
    :string (cpperror context "Invalid statement %p" expr)
    :number (cpperror context "Invalid statement %p" expr)
    :brackets (cpperror context "Invalid statement (tuple with brackets)")
    :symbol nil # TODO: This is probably a macro.
    :call (do
            (def name (expr 0))
            (unless (symbol? name)
              (cpperror context "Expected symbol as function name"))
            (case (keyword name)
              :if (emit-cond expr (or-syntax expr context))
              :cond (emit-cond expr (or-syntax expr context))
              :def (do (emit-def expr context) (print ";"))
              :do (cpperror context "Unimplemented") # TODO: Emit a block
              :return (do
                        (unless (= (length expr) 2)
                          (cpperror context "Expected one argument to return"))
                        (emit-indent)
                        (prin "return" " ")
                        (emit-expr (expr 1) (or-syntax (expr 1) context))
                        (print ";"))
              # TODO: other builtins
              (do (emit-indent)
                  (emit-expr expr context)
                  (print ";"))))
    (cpperror
     context
     "Unknown normalized type %p - this is a bug in cppjan" (normalized 0))))

(varfn emit-statement-nonest-block [expr context]
  # Same as emit-statement, but does not emit curly braces if expression is a do
  # block. This is useful when the braces have alread been emitted, for example
  # when there is a do block in an if statement.
  (if (and (tuple-p? expr) (not (empty? expr)) (= (expr 0) 'do))
    (for i 1 (length expr)
      (emit-statement (expr i) (or-syntax (expr i) context)))
    (emit-statement expr context)))


(varfn emit-def [expr context]
  (def kind (keyword (expr 0)))

  (unless ({3 true 4 true} (length expr))
    (cpperror context "Wrong number of arguments to %s" kind))

  (def specifiers (expr 1))
  (def declarator (expr 2))

  (unless (tuple-b? specifiers)
    (cpperror (or-syntax specifiers context) "Expected bracketed tuple for specifiers list"))

  (when (= kind :def)
    (emit-indent))

  (def spec-ctx (or-syntax specifiers context))
  (for i 0 (length specifiers)
    (def specifier (specifiers i))
    (if (and (= i 0) (tuple-b? specifier))
      (emit-attributes specifier (or-syntax specifier spec-ctx))
      (emit-specifier specifier (or-syntax specifier spec-ctx)))
    (unless (= (inc i) (length specifiers))
      (prin " ")))

  (needs-space)
  (emit-declarator kind declarator (or-syntax declarator specifiers context))
  (no-needs-space)

  (when (= (length expr) 4)
    (when (= kind :type)
      (cpperror context "Expected 2 arguments to type, got 3"))
    (def init (expr 3))
    (prin " = ")
    (emit-expr init (or-syntax init declarator specifiers context))))

(defn- emit-defn [expr context]
  (when (< (length expr) 5)
    (cpperror context "Expected at least 4 arguments to defn"))
  (var index 0)
  (def specifiers (expr (++ index)))
  (def declarator (expr (++ index)))
  (def doc (if (string? (expr (inc index))) (expr (++ index)) nil))
  (def params 
    (if (< (length expr) (+ index 2))
      (cpperror context "Params missing for defn")
      (expr (++ index))))
  (def fn-info
    (if (< (length expr) (+ index 2))
      (cpperror context "Function info missing for defn")
      (expr (++ index))))


  (when doc
    (var first true)
    (def lines (string/split "\n" doc))
    (each line lines
      (if first
        (do (prin "/**") (set first false))
        (prin " **"))
      (when (not (empty? line))
        (prin " "))
      (if (= (length lines) 1)
        (prin line)
        (print line)))
    (unless first
      (print " **/")))

  (emit-def ['def
             specifiers
             ['fn declarator params fn-info]]
            context)
  (prin " ")
  (emit-block-start)
  (for i (inc index) (length expr)
    (def statement (expr i))
    (emit-statement statement (or-syntax statement context)))
  (emit-block-end)
  (print))

(defn- emit-toplevel [expr context]
  (def normalized (normalize-expr expr context))
  (case (normalized 0)
    :string (cpperror context "Invalid toplevel form %p" expr)
    :number (cpperror context "Invalid toplevel form %p" expr)
    :brackets (cpperror context "Invalid toplevel form (tuple with brackets)")
    :symbol nil # TODO: This is probably a macro.
    :call (do
            (def name (expr 0))
            (unless (symbol? name)
              (cpperror context "Expected symbol as function name %p" name))
            (case (keyword name)
              :@include (emit-@include expr (or-syntax expr context))
              :@def (emit-@def expr (or-syntax expr context))
              :@defn (emit-@defn expr (or-syntax expr context))
              :def (do (emit-def expr (or-syntax expr context))
                       (print ";"))
              :defn (emit-defn expr (or-syntax expr context))
              :upscope (for i 1 (length expr)
                         (emit-toplevel (expr i) (or-syntax (expr i) context)))
              (cpperror context "Unknown toplevel function") # TODO: Unknown toplevel function. Is probably a macro.
              ))
    (cpperror
     context
     "Unknown normalized type %p - this is a bug in cppjan" (normalized 0))))

(defn emit-code
  [code]
  (var context (or-syntax code nil))
  (each elem code
    (emit-toplevel elem (or-syntax elem context))
    (set context (or-syntax elem context))))

(defn emit
  [file-data &opt out]
  (with-dyns [*source-name* (file-data :source-name)
              *out* (or out (dyn out))]
    (print "/* " (dyn *source-name*) " */")
    (emit-code (file-data :code))))

(defn emit-to-string
  [file-data]
  (def out @"")
  (emit file-data out)
  (string out))

# Macros #

(defdyn-local cppjan max-depth
  `How many recursive macro expansions to apply before giving up.`)

(defn is-macro?
  `Returns true if the symbol is bound to a cppjan macro at the toplevel.`
  [symbol]
  (def sym (dyn symbol))
  (truthy? (and (table? sym) (sym :cppjan/macro))))

(defn- c-unquote [expr]
  (eval expr))

(def- special-names
  {'unquote c-unquote})

(defn- splice? [exp]
  (and (tuple? exp) (= (length exp) 2) (= (exp 0) 'splice)))

(defn dosplice
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

(defn- am-inner [cmacro? exprs]
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
                        (or (special-names name) (cmacro? name)))))
    (break (dosplice (keep-syntax! exprs (map |(am-inner cmacro? $) exprs)))))

  (def macro-fun (if (fn? name)
                   name
                   (or (special-names name)
                       ((dyn name) :value)
                       (((dyn name) :ref) 0))))
  (var expanded (apply macro-fun (tuple/slice exprs 1)))
  (when (tuple? expanded)
    (var expanded+ nil)
    (def max-depth (or (dyn *max-depth*) 500))
    (var counter 0)
    (while (not= expanded (set expanded+ (am-inner cmacro? expanded)))
      (set expanded expanded+)
      (when (> (++ counter) max-depth)
        (error (string "More than " max-depth " recursive macro invocations")))))
  (dosplice (keep-syntax exprs expanded)))

(defn apply-macros
  `Applies cppjan macros to the given code and returns the result. Macro names
  are determined based on which bindings in the environment have the
  :cppjan/macro metadata set to true. A source-name string must be given for
  error messages to be able to properly display the error's source location.

   By default macros will be recursively expanded up to 500 times before giving
   an error. To change this limit, set the dynamic binding :cppjan/max-depth to
   a higher value.`
  [source-name exprs]

  (def known-symbols @{})
  (defn cmacro? [symbol]
    (when (nil? (known-symbols symbol))
      (set (known-symbols symbol) (is-macro? symbol)))
    (known-symbols symbol))

  (with-dyns []
    {:code (am-inner cmacro? exprs)
     :source-name source-name}))

(defn add-to-project
  `Assigns the code to the given file-name in the project. The file name should
  be a string. The macros in the code must already have been expanded using
  apply-macros.`
  [project name expanded-code]
  (set (project name) expanded-code))

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
  (set ((dyn sym) :cppjan/macro) true))

(each sym ['-> '->>]
  (enable-macro sym))

(defmacro defmacro
  `Same as the core defmacro, except that the :cppjan/macro metadata is also set
  to true. This enables this macro to be used in cppjan code.`
  [name & more]
  (apply defn name :cppjan/macro :macro more))
