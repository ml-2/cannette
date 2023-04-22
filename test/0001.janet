(import /cppjan1 :as "c")

(c/defmacro include-all-splice [& names]
  (def stx (dyn *macro-form*))
  (defn f [name] (keep-syntax stx ~(@include ,name)))
  ~(splice ,(tuple/slice (map f names))))

(c/defmacro include-all-upscope [& names]
  (def stx (dyn *macro-form*))
  (defn f [name] (keep-syntax stx ~(@include ,name)))
  ~(upscope ,;(tuple/slice (map f names))))

(def name (dyn *current-file*))

(def basic-generate-include `
/* test/0001.janet */
#include <stdlib.h>
#include "0001.h"

`)

(defmacro gencode [source-name & code]
  ~(c/emit-to-string
    (c/apply-macros-to-file ,source-name ',code)))

(assert
 (= basic-generate-include
    (gencode name (include-all-splice <stdlib.h> ,(symbol "0001.h"))))
 "basic-generate-include-splice")

(assert
 (= basic-generate-include
    (gencode name (include-all-upscope <stdlib.h> ,(symbol "0001.h"))))
 "basic-generate-include-upscope")

(assert
 (=
  `
/* test/0001.janet */
/** Convert a git error into a janet panic. **/
static void throw_error(int error) {
  git_error *const e = git_error_last();
  if (e == NULL) {
    janet_panicf("Git error with code %d", janet_wrap_integer(error));
  } else {
    janet_panicf("Git error with code %d class %d: %s", (long)error, (long)e->klass, e->message);
  }
}

`
  (gencode
   name
   (defn [static void] throw-error
     `Convert a git error into a janet panic.`
     [(def [int] error)]
     (def [git-error] (* e const) (git-error-last))
     (if (= e NULL)
       (janet-panicf `Git error with code %d` (janet-wrap-integer error))
       (janet-panicf `Git error with code %d class %d: %s`
                     (cast (type [long]) error) (cast (type [long]) e->klass) e->message)))))
 "Relatively simple function.")

(assert (= (c/apply-type 'name '(type [uint8_t] (* _)))
           '(def [uint8_t] (* name))))

(assert (= (c/apply-type 'name '(type [uint8_t] (* (* _))))
           '(def [uint8_t] (* (* name)))))

(assert (= (c/apply-type 'name '(type [const char] (* (* _) const)))
           '(def [const char] (* (* name) const))))
