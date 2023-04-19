(import ./cppjan1-lib :prefix "")

# Lib #

(def- ?dbqt (`"` 0))
(def- ?< (`<` 0))

# Private dynamics #

(defdyn-local cppjan indent `Current indent level for cppjan output.`)
(defn- indent [] (or (dyn *indent*) (setdyn *indent* @"")))
(defn- indent+ [] (buffer/push-string (indent) "  "))
(defn- indent- [] (buffer/popn (indent) 2))

(defdyn-local cppjan needs-backslash
  `Indicate that newlines must be backslashed.`)
(defn needs-backslash? [] (dyn *needs-backslash*))
(defn needs-backslash [] (setdyn *needs-backslash* true))
(defn no-needs-backslash [] (setdyn *needs-backslash* false))

(defdyn-local cppjan line-length `Length of the current line`)
(defn- line-length [] (or (dyn *line-length*) (setdyn *line-length* 0)))
(defn- push-line-length [arg] (setdyn *line-length* (+ (line-length) (length arg))))
(defn- new-line-length [] (setdyn *line-length* 0))
(defn- cprin [& args]
  (each arg args
    (unless (or (string? arg) (buffer? arg) (symbol? arg) (keyword? arg))
      (errorf "Expected string-like, got %v" (type arg)))
    (push-line-length arg))
  (prin ;args))

(defn- cprint [& args]
  (new-line-length) # set line length to 0
  (if (needs-backslash?)
    (do
      (prin ;args)
      (print " \\"))
    (print ;args)))

(defdyn-local cppjan needs-space
  `Used to temporarily store that a space will be needed if more is printed.`)
(defn- needs-space [] (setdyn *needs-space* true))
(defn- no-needs-space [] (setdyn *needs-space* false))
(defn- emit-space? []
  (when (dyn *needs-space*)
    (do (cprin " ")
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

(defn- normalize-expr [expr context]
  (case (type expr)
    :nil [:symbol (symbol "nil")]
    :boolean [:symbol (symbol expr)]
    :number [:number expr]
    :tuple (if (empty? expr)
             (if (tuple-p? expr)
               (cerr context "Empty call is not allowed")
               [:brackets expr])
             [:call expr])
    :string [:string expr]
    :symbol [:symbol expr]
    (cerr context (fmt "Invalid expression %p" expr))))

(defn- as-symbol [expr]
  (case (type expr)
    :nil (symbol "nil")
    :boolean (symbol expr)
    :symbol expr
    :string (symbol expr)
    nil))

(defn- emit-indent []
  (cprin (indent)))

(defn- emit-block-start []
  (cprint "{")
  (indent+))

(defn- emit-block-end []
  (indent-)
  (emit-indent)
  (cprin "}"))

(var- emit-def nil)
(var- emit-defn nil)
(var- emit-statement nil)
(var- emit-statement-nonest-block nil)
(var- emit-expr nil)

(defn- emit-ident [ident context]
  (def converted (peg/match ident-conversion-grammar ident))
  (if converted
    (cprin (converted 0))
    (cprin ident)))

(defn- emit-@include [expr context]
  (unless (= (length expr) 2)
    (cerr context "Expected 1 argument to @include"))
  (def include (as-symbol (expr 1)))
  (when (or (not include) (empty? include))
    (cerr context "Expected symbol or string as argument to @include"))
  (cprin "#include ")
  (if ({?dbqt true ?< true} (first include))
    (cprint include)
    (cprint `"` include `"`)))

(defn- emit-@def [expr context]
  (cond
    (= (length expr) 2)
    (do
      (cprin "#define ")
      # TODO: validate
      (emit-ident (expr 1) (or-syntax (expr 1) context))
      (cprint))
    (= (length expr) 3)
    (do
      (cprin "#define ")
      (emit-ident (expr 1) (or-syntax (expr 1) context))
      (cprin " ")
      (needs-backslash)
      (emit-expr (expr 2) (or-syntax (expr 2) context))
      (no-needs-backslash)
      (cprint))
    (cerr context "Invalid number of arguments to @def")))

(defn- emit-@defn [expr context]
  # TODO
  (cerr context "@defn is unimplemented")
  )

(defn- emit-attributes [spec context]
  # TODO
  )

(defn- emit-template-type [expr context]
  (when (< (length expr) 2)
    (cerr context "Expected at least one argument to <>"))
  (def name (expr 1))
  # TODO: Use normalize
  (unless (symbol? name)
    (cerr context "Expected a symbol for the name"))
  (emit-ident name context)
  (cprin "<")
  (var ctx context)
  (var did-indent false)
  (for i 2 (length expr)
    (def param (expr i))
    (set ctx (or-syntax param ctx))
    # TODO: Use normalize
    (cond
      (symbol? param)
      (emit-ident param context)
      (and (tuple-p? param) (not (empty? param)) (= (param 0) 'type))
      (emit-def param ctx)
      (cerr ctx "Invalid template parameter %p" param))
    (unless (= (inc i) (length expr))
      (if (>= (line-length) 100)
        (do
          (unless did-indent
            (indent+)
            (set did-indent true))
          (cprint ",")
          (emit-indent))
        (cprin ", "))))
  (when did-indent
    (indent-))
  (cprin ">"))

(defn- emit-class-member [expr context]
  (def normalized (normalize-expr expr context))
  (case (normalized 0)
    :string (cerr context "Invalid statement %p" expr)
    :number (cerr context "Invalid statement %p" expr)
    :brackets (cerr context "Invalid statement (tuple with brackets)")
    :symbol (do (emit-indent) (emit-ident expr context) (cprint))
    :call (do
            (def name (expr 0))
            (unless (symbol? name)
              (cerr context "Expected symbol as function name"))
            (case (keyword name)
              :def (do (emit-def expr context true) (cprint ";"))
              :defn (emit-defn expr (or-syntax expr context))
              :do (cerr context "Unimplemented") # TODO: Emit a block
              :label (do
                       # TODO: validate
                       (var deindent? false)
                       (unless (empty? (indent))
                         (set deindent? true)
                         (indent-))
                       (for i 1 (length expr)
                         (emit-ident (expr i) context)
                         (unless (= (inc i) (length expr))
                           (cprin " ")))
                       (cprint ":")
                       (when deindent? (indent+)))
              :n! (cprint)
              :c (do
                   # TODO: validation
                   (cprin (expr 1)))
              # TODO: other builtins
              (cerr context "Unknown class member form")))
    (cerr
     context
     "Unknown normalized type %p - this is a bug in cppjan" (normalized 0))))

(defn- emit-class [spec context]
  (when (< (length spec) 3)
    (cerr context "Expected at least 2 arguments to class"))
  (def name (spec 1))
  (def info (spec 2))
  (cprin "class ")
  # TODO: Validate the name
  (emit-ident name context)

  (unless (tuple-b? info)
    (cerr (or-syntax info context) "Expected square brackets"))

  (each val info
    (cond (and (tuple-p? val) (not (empty? val)) (= (val 0) 'extends))
          (do
            (cprin " : ")
            (for i 1 (length val)
              (def superclass (val i))
              (unless (and (tuple-p? superclass) (not (empty? superclass)) (= (superclass 0) 'type))
                (cerr (or-syntax superclass context) "Expected a type for extends"))
              (emit-def superclass (or-syntax superclass context))))
          (cerr (or-syntax val context) "Unknown or unimplemented class info")))
  (unless (= (length spec) 3)
    (cprin " ")
    (emit-block-start)
    (for i 3 (length spec)
      (emit-class-member (spec i) (or-syntax (spec i) context))
      )
    (emit-block-end)
    ))

(defn- emit-specifier [spec context]
  # TODO: Use normalize
  (cond
    (symbol? spec)
    (emit-ident spec context)
    (and (tuple-p? spec) (not (empty? spec)) (symbol? (spec 0)))
    (case (keyword (spec 0))
      :<> (emit-template-type spec context)
      :class (emit-class spec context)
      :struct nil # TODO
      :enum nil # TODO
      :union nil # TODO
      :decltype nil # TODO
      (cerr
       context
       "Unknown specifier type - use (<> %p parameters) if it was meant to be a template type" (spec 0)))
    (cerr context "Invalid specifier %p" spec)))

(defn- emit-declarator [kind decl context]
  # kind can be :type or :def. For the former, the name should be _.
  (def normalized (normalize-expr decl context))
  (case (normalized 0)
    :string (cerr context "Invalid declaration %p" decl)
    :number (cerr context "Invalid declaration %p" decl)
    :brackets (cerr context "Invalid declaration (tuple with brackets)")
    :symbol (if (and (symbol? decl) (= kind :type) (not= decl '_))
              (cerr context "The declarator name of a type must be set to _")
              (when (= kind :def)
                (emit-space?)
                (emit-ident decl context)))
    :call (do
            (def name (decl 0))
            (unless (or (symbol? name) (tuple-b? name))
              (cerr context "Invalid function name"))
            (cond
              (tuple-b? name)
              (do
                (unless ({2 true 3 true} (length decl))
                  (cerr (or-syntax name context)
                        "Expected 1 or 2 arguments to array declarator, got %v" (length decl)))
                (cond (empty? name)
                      (do
                        (emit-declarator kind (decl 1) (or-syntax (decl 1) context))
                        (cprin "[]")
                        # TODO: Emit attributes
                        )
                      (cerr context "Arrays of non-fixed length are not implemented") # TODO
                      ))

              (peg/match pointers-grammar name)
              (do
                (emit-space?)
                (cprin name)
                (when (< (length decl) 2)
                  (cerr context "Wrong number of arguments to %p" name))
                (def inner-decl (decl 1))
                (for i 2 (length decl)
                  (def elem (decl i))
                  (cond (and (= (inc i) (length decl)) (tuple-b? elem))
                        (emit-attributes elem (or-syntax elem context))
                        (symbol? elem)
                        (emit-ident elem context)
                        (cerr (or-syntax elem context) "Invalid metadata %p for pointer declarator" elem))
                  (if (= (inc i) (length decl))
                    (needs-space)
                    (cprin " ")))
                (emit-declarator kind inner-decl (or-syntax inner-decl context))
                (no-needs-space))

              # TODO: Qualified pointer declarators

              (case (keyword name)
                :decl (do
                        (when (or (< (length decl) 2) (not (symbol? (decl 1))))
                          (cerr context "Expected a name for decl"))
                        (emit-space?)
                        (cprin (decl 1))
                        (when (= (length decl) 3)
                          (needs-space)
                          (emit-attributes (decl 2) (or-syntax (decl 2) context))
                          (no-needs-space)))
                :operator (cerr context "Unimplemented declarator name %p" name) # TODO
                :... (cerr context "Unimplemented declarator name %p" name) # TODO
                :& (do
                     (emit-space?)
                     (cprin "&")
                     # TODO: This is largely copied from the pointer code above
                     (when (< (length decl) 2)
                       (cerr context "Wrong number of arguments to %p" name))
                     (def inner-decl (decl 1))
                     (for i 2 (length decl)
                       (def elem (decl i))
                       (cond (and (= (inc i) (length decl)) (tuple-b? elem))
                             (emit-attributes elem (or-syntax elem context))
                             (symbol? elem)
                             (emit-ident elem context)
                             (cerr (or-syntax elem context) "Invalid metadata %p for pointer declarator" elem))
                       (if (= (inc i) (length decl))
                         (needs-space)
                         (cprin " ")))
                     (emit-declarator kind inner-decl (or-syntax inner-decl context))
                     (no-needs-space))
                :&& (cerr context "Unimplemented declarator name %p" name) # TODO
                :fn (do
                      (when (not= (length decl) 4)
                        (cerr context "Expected 3 args to fn"))

                      (def fn-decl (decl 1))

                      (def params (decl 2))
                      (when (not (tuple-b? params))
                        (cerr context "Expected brackets tuple for params"))

                      (def fn-info (decl 3))

                      (emit-declarator kind fn-decl (or-syntax fn-decl context))
                      (if (= params '[void])
                        (cprin "(void)")
                        (do
                          (cprin "(")
                          (var did-indent false)
                          (for i 0 (length params)
                            (def param (params i))
                            (unless (and (tuple-p? param) (not (empty? param))
                                         (= 'def (param 0)))
                              (cerr context "Expected a definition for a param"))
                            (emit-def param (or-syntax param params context))
                            (unless (= (inc i) (length params))
                              (if (>= (line-length) 100)
                                (do
                                  (unless did-indent
                                    (indent+)
                                    (set did-indent true))
                                  (cprint ",")
                                  (emit-indent))
                                (cprin ", "))))
                          (when did-indent (indent-))
                          (cprin ")")))

                      # TODO: Validate fn-info
                      (for i 0 (length fn-info)
                        # TODO: Emit other types of fn-info
                        (def info (fn-info i))
                        (def context (or-syntax info fn-info context))
                        (cond (and (tuple-p? info) (not (empty? info)) (= (info 0) 'init))
                              (do
                                (cprin " : ")
                                (for j 1 (length info)
                                  # TODO: Is this the correct way to do initialization lists?
                                  (emit-expr (info j) context)
                                  (unless (= (inc j) (length info))
                                    (cprin ", "))))
                              (cerr context "Invalid or unimplemented fn info") # TODO
                              )))
                (cerr context "Unknown declarator name %p" name))))
     (cerr
      context
      "Unknown normalized type %p - this is a bug in cppjan" (normalized 0))))

(def- uops
  '{+ "+" - "-" * "*" & "&" not "!" bnot "~" ++ "++" -- "--"})

(def- binops
  '{+ "+" - "-" * "*" / "/" % "%"
    and "&&" or "||" band "&" bor "|" bxor "^"
    << "<<" >> ">>" = "==" not= "!=" > ">" < "<" >= ">=" <= "<=" <=> "<=>"
   . "." .-> "->" .* ".*" .->* ".->*"})
(def- binops-nospace {'. true '.-> true})

(defn- emit-number [expr context]
  # TODO: By default numeric values, only allow integers before precision is lost.
  # TODO: For other numeric values, use something like (num/f 1.1) syntax.
  (cond
    (not= (math/round expr) expr)
    (cerr context "Expected an integer (floating points are unimplemented)") # TODO
    (<= expr math/int-min)
    (cerr context "Integer too small (floating points are unimplemented)")
    (>= expr math/int-max)
    (cerr context "Integer too large (floating points are unimplemented)"))
  (cprin (string expr)))

(defn- emit-string [expr context]
  # TODO: Escape characters? Multi-strings?
  (cprin `"` expr `"`))

(defn- emit-uop [expr context &opt with-parens?]
  (when with-parens? (cprin "("))
  (cprin (uops (expr 0)))
  (emit-expr (expr 1) (or-syntax (expr 1) context) true)
  (when with-parens? (cprin ")")))

(defn- emit-binop [expr context &opt with-parens?]
  (def sym (expr 0))
  (when with-parens?
    (cprin "("))
  (emit-expr (expr 1) (or-syntax (expr 1) context) true)
  (unless (binops-nospace sym)
    (cprin " "))
  (cprin (binops sym))
  (unless (binops-nospace sym)
    (cprin " "))
  (emit-expr (expr 2) (or-syntax (expr 2) context) true)
  (when with-parens?
    (cprin ")")))

(defn- emit-funargs [expr context]
  (cprin "(")
  # TODO: Find a way to factor out the did-indent stuff from the many places it appears.
  (var did-indent false)
  (for i 1 (length expr)
    (emit-expr (expr i) (or-syntax (expr i) context))
    (unless (= (inc i) (length expr))
      (if (>= (line-length) 100)
        (do
          (unless did-indent
            (indent+)
            (set did-indent true))
          (cprint ",")
          (emit-indent))
        (cprin ", "))))
  (when did-indent (indent-))
  (cprin ")"))

(varfn emit-expr [expr context &opt with-parens?]
  (def normalized (normalize-expr expr context))
  (case (normalized 0)
    :string (emit-string expr context)
    :number (emit-number expr context)
    :brackets (cerr context "Bracket expressions are unimplemented") # TODO
    :symbol (emit-ident expr context)
    :call (do
            (def name (expr 0))
            (cond
              (not (symbol? name))
              (do (emit-expr name (or-syntax name context))
                  (emit-funargs expr context))
              (and (peg/match pointers-op-grammar name)
                   (implies (= name '*) (= (length expr) 2)))
              (if (not= (length expr) 2)
                (cerr context "Expected exactly one argument to %p" name)
                (do
                  (when with-parens? (cprin "("))
                  (cprin name)
                  (emit-expr (expr 1) (or-syntax (expr 1) context) true)
                  (when with-parens? (cprin ")"))))
              (and (= (length expr) 2)
                   (uops name))
              (emit-uop expr context with-parens?)
              (binops name)
              (emit-binop expr context with-parens?)
              (case (keyword name)
                :set (do
                       # TODO: Validation
                       (emit-expr (expr 1) (or-syntax (expr 1) context) with-parens?)
                       (cprin " = ")
                       (emit-expr (expr 2) (or-syntax (expr 2) context)) with-parens?)
                :cast (do
                        (unless (>= (length expr) 3)
                          (cerr context "Expected at least 2 arguments to cast"))
                        (when with-parens?
                          (cprin "("))
                        (cprin "(")
                        (def type-call (if (= (length expr) 3)
                                         ['type (expr 1) '_]
                                         ['type ;(slice expr 1 (- (length expr) 1))]))
                        (emit-def type-call context)
                        (cprin ")")
                        (emit-expr (last expr) (or-syntax (last expr) context) with-parens?)
                        (when with-parens?
                          (cprin ")")))
                :new (do
                       # TODO: Validate
                       (when with-parens?
                         (cprin "("))
                       (cprin "new ")
                       (emit-expr (tuple/slice expr 1) (or-syntax (expr 1) context))
                       (when with-parens?
                         (cprin ")")))
                :<> (do
                      (emit-template-type expr context))
                :if (do
                      (when (not= (length expr) 4)
                        (cerr context "Expected exactly 3 arguments to if expression"))
                      (emit-expr (expr 1) (or-syntax (expr 1) context) true)
                      (cprin " ? ")
                      (emit-expr (expr 2) (or-syntax (expr 2) context) true)
                      (cprin " : ")
                      (emit-expr (expr 3) (or-syntax (expr 3) context) true))
                (do
                  # Regular function call
                  (emit-ident name context)
                  (emit-funargs expr context)))))
    (cerr
     context
     "Unknown normalized type %p - this is a bug in cppjan" (normalized 0))))

(defn- emit-cond [expr context]
  (when (< (length expr) 2)
    (cerr context "Conditional branch requires a condition"))
  (when (and (> (length expr) 4) (= (expr 0) 'if))
    (cerr context "Too many arguments to if"))
  (forv
   i 1 (length expr)
   (cond
     (= i 1)
     (do
       (emit-indent)
       (cprin "if (")
       (emit-expr (expr i) (or-syntax (expr i) context))
       (cprin ") ")
       (emit-block-start)
       (++ i)
       (emit-statement-nonest-block (expr i) (or-syntax (expr i) context))
       (emit-block-end))
     (= (inc i) (length expr))
     (do
       (cprin " else ")
       (emit-block-start)
       (emit-statement-nonest-block (expr i) (or-syntax (expr i) context))
       (emit-block-end))
     (do
       (cprin " else if (")
       (emit-expr (expr i) (or-syntax (expr i) context))
       (cprin ") ")
       (emit-block-start)
       (++ i)
       (emit-statement-nonest-block (expr i) (or-syntax (expr i) context))
       (emit-block-end))))
  (cprint))

(varfn emit-statement [expr context]
  (def normalized (normalize-expr expr context))
  (case (normalized 0)
    :string (cerr context "Invalid statement %p" expr)
    :number (cerr context "Invalid statement %p" expr)
    :brackets (cerr context "Invalid statement (tuple with brackets)")
    :symbol (cerr context "Constant statements are unimplemented") # TODO: This is probably a macro.
    :call (do
            (def name (expr 0))
            (if-not (symbol? name)
              (do (emit-indent)
                  (emit-expr name (or-syntax name context))
                  (emit-funargs expr context)
                  (cprint ";"))
              (case (keyword name)
                :if (emit-cond expr (or-syntax expr context))
                :cond (emit-cond expr (or-syntax expr context))
                :def (do (emit-def expr context true) (cprint ";"))
                :do (cerr context "Unimplemented") # TODO: Emit a block
                :return (do
                          (when (> (length expr) 2)
                            (cerr context "Expected zero or one argument to return"))
                          (emit-indent)
                          (cprin "return")
                          (when (= (length expr) 2)
                            (cprin " ")
                            (emit-expr (expr 1) (or-syntax (expr 1) context)))
                          (cprint ";"))
                :label (do
                         # TODO: validate
                         (var deindent? false)
                         (unless (empty? (indent))
                           (set deindent? true)
                           (indent-))
                         (emit-ident (expr 1) context)
                         (cprint ":")
                         (when deindent? (indent+)))
                :n! (cprint)
                :c (do
                     # TODO: validation
                     (cprin (expr 1)))
                # TODO: other builtins
                (do (emit-indent)
                    (emit-expr expr context)
                    (cprint ";")))))
    (cerr
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


(varfn emit-def [expr context &opt indent?]
  (def kind (keyword (expr 0)))

  (unless ({3 true 4 true} (length expr))
    (cerr context "Wrong number of arguments to %s" kind))

  (def specifiers (expr 1))
  (def declarator (expr 2))

  (unless (tuple-b? specifiers)
    (cerr (or-syntax specifiers context) "Expected bracketed tuple for specifiers list"))

  (when indent?
    (emit-indent))

  (def spec-ctx (or-syntax specifiers context))
  (for i 0 (length specifiers)
    (def specifier (specifiers i))
    (if (and (= i 0) (tuple-b? specifier))
      (emit-attributes specifier (or-syntax specifier spec-ctx))
      (emit-specifier specifier (or-syntax specifier spec-ctx)))
    (unless (= (inc i) (length specifiers))
      (cprin " ")))

  (needs-space)
  (emit-declarator kind declarator (or-syntax declarator specifiers context))
  (no-needs-space)

  (when (= (length expr) 4)
    (when (= kind :type)
      (cerr context "Expected 2 arguments to type, got 3"))
    (def init (expr 3))
    (if (and (tuple-p? init) (not (empty? init)) (= (init 0) 'constructor))
      # Constructor syntax
      (do
        (cprin "(")
        (var did-indent false)
        (for i 1 (length init)
          (emit-expr (init i) (or-syntax (init i) init context))
          (unless (= (inc i) (length init))
            (if (>= (line-length) 100)
              (do
                (unless did-indent
                  (indent+)
                  (set did-indent true))
                (cprint ",")
                (emit-indent))
              (cprin ", "))))
        (when did-indent (indent-))
        (cprin ")"))
      (do
        (cprin " = ")
        (emit-expr init (or-syntax init declarator specifiers context))))))

(varfn emit-defn [expr context]
  (when (< (length expr) 5)
    (cerr context "Expected at least 4 arguments to defn"))
  (var index 0)
  (def specifiers (expr (++ index)))
  (def declarator (expr (++ index)))
  (def doc (if (string? (expr (inc index))) (expr (++ index)) nil))
  (def params
    (if (< (length expr) (+ index 2))
      (cerr context "Params missing for defn")
      (expr (++ index))))
  (def fn-info
    (if (< (length expr) (+ index 2))
      (cerr context "Function info missing for defn")
      (expr (++ index))))


  (when doc
    (var first true)
    (def lines (string/split "\n" doc))
    (each line lines
      (if first
        (do (cprin "/**") (set first false))
        (cprin " **"))
      (when (not (empty? line))
        (cprin " "))
      (if (= (length lines) 1)
        (cprin line)
        (cprint line)))
    (unless first
      (cprint " **/")))

  (emit-def ['def
             specifiers
             ['fn declarator params fn-info]]
            context
            true)
  (cprin " ")
  (emit-block-start)
  (for i (inc index) (length expr)
    (def statement (expr i))
    (emit-statement statement (or-syntax statement context)))
  (emit-block-end)
  (cprint))

(defn- emit-toplevel [expr context]
  (def normalized (normalize-expr expr context))
  (case (normalized 0)
    :string (cerr context "Invalid toplevel form %p" expr)
    :number (cerr context "Invalid toplevel form %p" expr)
    :brackets (cerr context "Invalid toplevel form (tuple with brackets)")
    :symbol nil # TODO: This is probably a macro.
    :call (do
            (def name (expr 0))
            (unless (symbol? name)
              (cerr context "Expected symbol as function name %p" name))
            (case (keyword name)
              :@include (emit-@include expr (or-syntax expr context))
              :@def (emit-@def expr (or-syntax expr context))
              # TODO: Other kinds of @def for things that aren't expressions
              :@defn (emit-@defn expr (or-syntax expr context))
              :def (do (emit-def expr (or-syntax expr context) true)
                       (cprint ";"))
              :type (do (emit-def expr (or-syntax expr context) true)
                        (cprint ";"))
              :defn (emit-defn expr (or-syntax expr context))
              :upscope (for i 1 (length expr)
                         (emit-toplevel (expr i) (or-syntax (expr i) context)))
              :ifndef (do
                        # TODO: validation
                        (cprin "#ifndef" " ")
                        (emit-ident (expr 1) context)
                        (cprint)
                        (emit-toplevel (expr 2) (or-syntax (expr 2) context))
                        (when (= (length expr) 4)
                          (cprint "#else")
                          (emit-toplevel (expr 3) (or-syntax (expr 3) context)))
                        (cprin "#endif /* ")
                        (emit-ident (expr 1) context)
                        (cprint " */"))
              :n! (cprint)
              :c (do
                   # TODO: validation
                   (cprin (expr 1)))
              (cerr context "Unknown toplevel function") # TODO: Unknown toplevel function. Is probably a macro.
              ))
    (cerr
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
    (cprint "/* " (dyn *source-name*) " */")
    (emit-code (file-data :code))))

(defn emit-to-string
  [file-data]
  (def out @"")
  (emit file-data out)
  (string out))
