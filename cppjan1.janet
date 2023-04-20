(import ./cppjan1-lib :prefix "")
(import ./cppjan1-emit :prefix "" :export true)
(import ./cppjan1-macros :as cm)

# Macros #

(cm/def-macros :cppjan/macro :cpp)
