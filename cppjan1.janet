(import ./cppjan1-lib :prefix "")
(import ./cppjan1-emit :prefix "" :export true)
(import ./cppjan1-macros :as cm)

# Macros #

(def emit-all emit-all) # Re-export

# Must be at end since it redefines important names like defmacro
(cm/def-macros :cppjan/macro :cpp emit)
