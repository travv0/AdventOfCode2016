(defun count-letters (s)
  (let ((letters '()))
    (loop for c across s do
      (cond ((digit-char-p c) (return letters))
            ((alpha-char-p c) (if (assoc c letters)
                                  (incf (cdr (assoc c letters)))
                                  (setf letters (acons c 1 letters))))))))

(defun make-checksum (letter-counts)
  (let ((letter-counts (sort-for-hash letter-counts)))
    (subseq (map 'string (lambda (a) (car a)) letter-counts) 0 5)))

(defun sort-for-hash (letter-counts)
  (let ((alpha-sorted (sort letter-counts (lambda (a b)
                                            (char< (car a) (car b))))))
    (stable-sort alpha-sorted
                 (lambda (a b)
                   (> (cdr a) (cdr b))))))

(defun real-room-p (s)
  (equal (make-checksum (count-letters s))
         (parse-checksum s)))

(defun parse-checksum (s)
  (subseq s (1+ (position #\[ s)) (position #\] s)))

(defun parse-sector-id (s)
  (parse-integer (subseq s (position-if 'digit-char-p s) (position #\[ s))))

(defun sum-of-real-rooms (filename)
  (let ((count 0))
    (with-open-file (file filename)
      (loop for room = (read-line file nil)
            while room do
              (when (real-room-p room)
                (setf count (+ count (parse-sector-id room))))))
    count))
