;;;; package.lisp

(defpackage #:random-sample
  (:use #:cl #:alexandria #:serapeum #:named-readtables)
  (:import-from #:infix-math #:$ #:^ #:over)
  (:export
   :random-sample
   :map-random-below
   :generate-index-array))
