(import ./cppjan1-lib :prefix "")
(import ./cppjan1-emit :prefix "" :export true)
(import ./cppjan1-macros :as cm)

(cm/def-macros :cppjan/macro :cpp)

(each sym ['-> '->> 'when]
  (enable-macro sym))
