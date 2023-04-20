(import ./cppjan1 :prefix "")

(each sym ['-> '->> 'when]
  (enable-macro sym))

(defmacro if-not [condition & args]
  ['if ['not condition] ;args])

(defmacro unless [condition & body]
  ['when ['not condition] ;body])
