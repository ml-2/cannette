(use judge)
(import cannette :as "c")

(defmacro gencode [source-name & code]
  ~(c/emit-to-string
    (c/apply-macros-to-file ,source-name ',code)))

(def name (dyn *current-file*))

(test (gencode name
               (def [typedef (:enum MyEnum [])] MyEnum))
      ```
/* test/c.janet */
/* THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT. */
typedef enum MyEnum MyEnum;

```)

(test (gencode name
               (def [typedef (:enum MyEnum [] ZERO ONE TWO THREE)] MyEnum))
      ```
/* test/c.janet */
/* THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT. */
typedef enum MyEnum {
  ZERO,
  ONE,
  TWO,
  THREE,
} MyEnum;

```)

(test (gencode name
               (def [typedef (:enum MyEnum [] (set FIVE 5) SIX (set TEN 10) ELEVEN)] MyEnum))
      ```
/* test/c.janet */
/* THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT. */
typedef enum MyEnum {
  FIVE = 5,
  SIX,
  TEN = 10,
  ELEVEN,
} MyEnum;

```)
