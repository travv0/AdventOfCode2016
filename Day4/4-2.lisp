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

(defun decrypt-room (room n)
  (let ((decrypted-room (make-array 0
                                    :element-type 'character
                                    :fill-pointer 0
                                    :adjustable t)))
    (loop for c across room do
      (cond ((alpha-char-p c)
             (let* ((adjusted-char-code (- (char-code c) (char-code #\a)))
                    (shifted-char-code (mod (+ adjusted-char-code n) 26))
                    (unadjusted-char-code (+ shifted-char-code (char-code #\a))))
               (vector-push-extend
                (code-char unadjusted-char-code)
                decrypted-room)))
            (t (vector-push-extend #\Space decrypted-room))))
    decrypted-room))

(defun parse-room-name (s)
  (subseq s 0 (1- (position-if 'digit-char-p s))))

(defun print-decrypted-names-with-sector-ids (filename)
  (with-open-file (file filename)
    (loop for room = (read-line file nil)
          while room do
            (let ((decrypted-room (decrypt-room (parse-room-name room)
                                                 (parse-sector-id room))))
              (format t "~d: ~a~%"
                      (parse-sector-id room)
                      decrypted-room)))))
