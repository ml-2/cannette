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

(defn implies [b0 b1]
  (or (not b0) b1))

(defn iff [b0 b1]
  (and (implies b0 b1)
       (implies b1 b0)))

(defn xor [b0 b1]
  (not (iff b0 b1)))

# Strings #

(def rep string/replace-all)
(def fmt string/format)

(defn contains? [col value]
  (var result false)
  (each elem col
    (when (= elem value)
      (set result true)
      (break)))
  result)

# Dynamics #

(defmacro defdyn-local
  `Generate a new dynamic :ns/name aliased to *ns/name* and *name*.`
  [ns name doc]
  ~(upscope
    (defdyn ,(symbol (string "*" ns "/" name "*")) ,doc)
    (def ,(symbol (string "*" name "*")) ,(symbol (string "*" ns "/" name "*")))))

# Public dynamics #

(defdyn-local emitlib source-name
  `Name of the source location of the file being generated.`)

# Errors #

(defn- emiterror [context message & params]
  (def source-name (or (dyn *source-name*) ":unknown-file"))
  (def msg (if (empty? params)
             message
             (fmt message ;params)))
  (print) # flush the stream
  (if context
    (error (fmt "%s | In file %s near line and column %p"
                msg source-name (tuple/sourcemap context)))
    (error (fmt "%s | In file %s" msg source-name))))

(defmacro cerr [& args]
  # Call emiterror, but never tail call optimize this call (for a better stack
  # trace).
  ['do (keep-syntax (dyn *macro-form*) [emiterror ;args]) nil])
