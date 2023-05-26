(use ./lib)

# Private dynamics #

(defdyn-local cannette/xml indent `Current indent level for cannette/xml output.`)
(defn- indent [] (or (dyn *indent*) (setdyn *indent* @"")))
(defn- indent+ [] (buffer/push-string (indent) "  "))
(defn- indent- [] (buffer/popn (indent) 2))

# Gencode #

(defn- emit-indent []
  (prin (indent)))

(defn- emit-str [str]
  (prin (->> str
             (rep "&" "&amp;")
             (rep "<" "&lt;")
             (rep ">" "&gt;")
             (rep "'" "&apos;")
             (rep `"` "&quot;"))))

(defn- emit-inner [xml context]
  (cond
    (tuple-p? xml)
    (do
      (when (empty? xml)
        (cerr context "Tags cannot be empty"))
      (unless (or (string? (xml 0)) (symbol? (xml 0)))
        (cerr context "Expected a symbol or string for the tag name"))
      (emit-indent)
      (prin "<")
      (emit-str (xml 0))
      (var index 1)
      (when (and (> (length xml) 1)
                 (tuple-b? (xml 1)))
        (++ index)
        (def attributes (xml 1))
        (def context (or-syntax attributes context))
        (unless (even? (length attributes))
          (cerr context "Attributes tuple must have an even number of elements"))
        (var first true)
        (indent+)
        (forv
         i 0 (length attributes)
         (if first
           (prin " ")
           (emit-indent))
         (def attribute (attributes i))
         (++ i)
         (def value (attributes i))
         (unless (or (string? attribute) (symbol? attribute))
           (cerr (or-syntax attribute context) "Attribute name must be a string or symbol"))
         (unless (or (string? value) (symbol? value))
           (cerr (or-syntax attribute context) "Attribute value must be a string or symbol"))
         (emit-str attribute)
         (prin `="`)
         (emit-str value)
         (prin `"`)
         (unless (= (inc i) (length attributes))
           (print))
         (set first false))
        (indent-))
      (if-not (< index (length xml))
        (print " />")
        (do
          (def single? (= (inc index) (length xml)))
          (if (and single? (or (string? (xml index)) (symbol? (xml index)))
                   (< (length (xml index)) 100)
                   (not (contains? (xml index) ("\n" 0))))
            (do
              (prin ">")
              (emit-str (xml index)))
            (do
              (print ">")
              (indent+)
              (for i index (length xml)
                (emit-inner (xml i) (or-syntax (xml i) context)))
              (indent-)
              (emit-indent)))
          (prin "</")
          (emit-str (xml 0))
          (print ">"))))
    (or (string? xml) (symbol? xml))
    (do (emit-indent) (emit-str xml) (print))
    (cerr context "Invalid type for xml %v" (type xml))))

(defn emit-xml [xml]
  (var context (or-syntax xml nil))
  (each elem xml
    (emit-inner elem (or-syntax elem context))
    (set context (or-syntax elem context))))

(defn emit [file-data &opt out]
  (with-dyns [*source-name* (file-data :source-name)
              *out* (or out (dyn out))]
    (print `<?xml version="1.0" encoding="UTF-8"?>`)
    (print "<!-- " (dyn *source-name*) " -->")
    (emit-xml (file-data :code))))

(defn emit-to-string
  [file-data]
  (def out @"")
  (emit file-data out)
  (string out))

(def emit-all emit-all) # Re-export

# Re-exports #

(re-export *source-name*)
(re-export *max-depth*)
(re-export keep-sourcemap)
(re-export cerr)
(re-export emit-all)

# Macros #

# Must be at end since it redefines important names like defmacro
(def-macros :cannette/xml/macro :xml emit)
