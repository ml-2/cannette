(declare-project
 :name "cannette"
 :description "C and C++ code generator"
 :dev-dependencies [
    {:url "https://github.com/ianthehenry/judge.git"
     :tag "v2.5.0"}])

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

