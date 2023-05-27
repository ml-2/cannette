(declare-project
 :name "cannette"
 :description "C and C++ code generator")

(declare-source
 :source [
   "src/c.janet"
   "src/init.janet"
   "src/janet.janet"
   "src/lib.janet"
   "src/macros.janet"
   "src/xml.janet"
 ]
 :prefix "cannette")

