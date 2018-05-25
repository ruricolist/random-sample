(defpackage :random-sample-tests
  (:use :cl :fiveam :random-sample :alexandria :serapeum)
  (:export :run-tests))
(in-package :random-sample-tests)

(defun run-tests ()
  (let ((5am::*num-trials* 1000))
    (run! 'random-sample)))

(def-suite random-sample)
(in-suite random-sample)

(test same-length
  (for-all ((pop-size (gen-integer :min 1 :max 1000)))
    (let ((seq (iota pop-size)))
      (is (set-equal seq (random-sample seq pop-size))))))

(test indices ()
  (for-all ((pop-size (gen-integer :min 1 :max 1000)))
    (let* ((sample-size (random pop-size))
           (indices
             (collecting
               (map-random-below #'collect
                                 sample-size
                                 pop-size))))
      (is-true (length= indices sample-size))
      (is-true (every #'< indices (rest indices)))
      (is (length= indices (nub indices)))
      (is-true (every (op (<= 0 _ pop-size)) indices))
      (is-true (every (op (< _ pop-size)) indices)))))

(test samples ()
  (for-all ((pop-size (gen-integer :min 1 :max 1000)))
    (let* ((pop (iota pop-size))
           (sample-size (random pop-size))
           (sample (random-sample pop sample-size)))
      (is-true (subsetp sample pop))
      (is-true (length= sample sample-size))
      (is-true (length= sample (nub sample))))))

(test vector-samples ()
  (for-all ((pop-size (gen-integer :min 1 :max 1000)))
    (let* ((pop (coerce (iota pop-size) 'vector))
           (sample-size (random pop-size))
           (sample (random-sample pop sample-size)))
      (is-true (every (op (find _ pop)) sample))
      (is-true (length= sample sample-size))
      (is-true (length= sample (nub sample))))))

(test large-range
  "Test that we can generate indices for ranges than double floats can
represent."
  (finishes
    (generate-index-array 50 (expt 2 128))))
