;;
;; IA Proj 3
;;

(defvar *grupo* "30")

(defun codifica-cnf-grafo(sfile nc)
  (interface sfile "grafo.cnf" nc))

(defun interface (sfile dfile nc)
  (let ((rfile (open sfile :if-does-not-exist nil))
        (wfile (open dfile :direction :output :if-exists :new-version :if-does-not-exist :create)))
    (if (null sfile)
        (wError wfile)
      (callParser rfile wfile nc))
    (close rfile)
    (close wfile)))

(defun callParser (rfile wfile nc)
  "Parse Caller"
  (wComent wfile)
  (parse rfile wfile nc))

(defun parse (rfile wfile nc)
  "Parse source file, and for each rule, take some action"
  (do ((line (read-line rfile nil) (read-line rfile nil 'the-end)))
      ((equal line 'the-end))
    (cond ((equal  (first (split-quoted line)) "p")
           (write-line (concatenate 'string
                         "p cnf "
                         (write-to-string (color-calculation
                                           (parse-integer (third (split-quoted line)))
                                           nc))
                         " "
                         (write-to-string (clause-calculation
                                           (parse-integer (third (split-quoted line)))
                                           nc
                                           (parse-integer (fourth (split-quoted line))))))
                       wfile)
           (restrict-colors-to-node
            (parse-integer (third (split-quoted line)))
            nc
            wfile)
           )
          ((equal  (first (split-quoted line)) "e")
           (restrict-colors-to-arc
            (parse-integer (second (split-quoted line)))
            (parse-integer (third (split-quoted line)))
            nc
            wfile)
           )
          )))

(defun wComent (wfile)
  "Write comentary"
  (write-line (concatenate 'string "c Ficheiro gerado por grupo " *grupo*) wfile))

(defun wError (wfile)
  "Write Error"
  (write-line "c Erro a abrir ficheiro de entrada" wfile))

(defun color-calculation (nn nc)
  "Number of colors calculation: nn * nc"
  (* nn nc))

(defun clause-calculation (nn nc n_arc)
  "colors C 2 * num_nos + num_nos + num_arcos * num_cores"
  (+ (+ (* (combination nc 2)
           nn)
        nn)
     (* n_arc nc)))

(defun restrict-colors-to-arc (node1 node2 nc wfile)
  "For each arc, restrict colors to each node"
  (let ((base1 (+ 1 (* (- node1 1) nc)))
        (base2 (+ 1 (* (- node2 1) nc)))
        (str1 ""))
    (loop for i from 0 to (- nc 1)
        do
          (setf str1 (concatenate 'string str1 (write-to-string (- 0 (+ base1 i))) " " (write-to-string (- 0 (+ base2 i))) " 0"))
          (write-line str1 wfile)
          (setf str1 ""))))

(defun restrict-colors-to-node (nn nc wfile)
  "For each node restrict one color"
  (let ((inc 1)
        (str1 ""))
    (loop for w from 1 to nn
        do
          (loop for i from inc to (* nc w)
              do
                (setf str1 (concatenate 'string str1 (write-to-string i) " ")))
          (setf str1 (concatenate 'string str1 "0"))
          (write-line str1 wfile)
          (setf str1 "")
          (loop for i from inc to (* nc w)
              do
                (loop for j from (+ 1 i) to (* nc w)
                    if (not (eq i j))
                    do
                      (setf str1 (concatenate 'string str1 (write-to-string (- 0 i)) " " (write-to-string (- 0 j)) " 0"))
                      (write-line str1 wfile)
                      (setf str1 "")))
          (setf inc (+ inc nc)))))

(defun combination (n p)
  "Matematic combination calculation"
  (if (or (eq n 0) (eq n 1))
      0
    (/ (factorial n) (* (factorial (- n p)) (factorial p)))))

(defun factorial (n)
  "Matematic factorial calculation"
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(defun split-quoted (str &optional max (ws '(#\Space)))
  "Split `string' along whitespace as defined by the sequence `ws',
but ignoring whitespace in quoted strings.  Whitespace which causes a
split is elided from the result.  The whole string will be split,
unless `max' is a non-negative integer, in which case the string will
be split into `max' tokens at most, the last one containing the whole
rest of the given `string', if any."
  (do ((i 0 (1+ i))
       (words '())
       (split-allowed-p t)
       (word '()))
      ((>= i (length str))
       (reverse (cons (coerce (reverse word) 'string) words)))
    (if (eql (elt str i) #\")
        (setf split-allowed-p (not split-allowed-p)))
    (if (eql (elt str i) #\\)
        (setf i (1+ i)))                ;advance past escape chars
    (if (and split-allowed-p
             (or (not max) (< (length words) (1- max)))
             (member (elt str i) ws))
        (progn
          (setf words (cons (coerce (reverse word) 'string) words))
          (setf word '()))
      (setf word (cons (elt str i) word)))))
