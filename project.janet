(declare-project
 :name "cannette"
 :description "C and C++ code generator")

(declare-source
 :source [
   "src/init.janet"
   "src/c.janet"
   "src/xml.janet"
   "src/lib.janet"
   "src/janet.janet"
 ]
 :prefix "cannette")

