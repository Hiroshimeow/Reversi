; -*- coding: utf-8 -*-
(defconstant Board-Size 8)
(defconstant Board-Dimensions (list Board-Size Board-Size))
(defconstant Board-Sente 1)
(defconstant Board-Gote -1)
(defconstant Board-Empty 0)
(defconstant Board-Sente-String "*")
(defconstant Board-Gote-String "o")
(defconstant Board-Empty-String "-")
(defconstant Board-Initial 
  #2A((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 -1 1 0 0 0)
      (0 0 0 1 -1 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))
(defconstant Board-Directions '((0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1) (-1 0) (-1 1)))

(defparameter *q-table* (make-hash-table :test 'equal))
(defparameter *alpha* 0.1)
(defparameter *gamma* 0.99)
(defparameter *epsilon* 1.0)
(defparameter *epsilon-decay* 0.995)
(defparameter *min-epsilon* 0.1)
(defparameter *q-table-file* "q-table")

(defun initialize-board () (copy-board Board-Initial))

(defun print-board (board)
  (format t "R/C 0 1 2 3 4 5 6 7~%")
  (dotimes (row Board-Size)
    (format t "  ~d" row)
    (dotimes (col Board-Size)
      (format t " ~A" (symbol-turn (aref board row col))))
    (format t "~%")))

(defun change-turn (turn) (if (equal turn Board-Sente) Board-Gote Board-Sente))

(defun symbol-turn (turn)
  (cond ((equal turn Board-Sente) Board-Sente-String)
        ((equal turn Board-Gote) Board-Gote-String)
        (t Board-Empty-String)))

(defun ref-board (board row col) (aref board row col))

(defun set-board (board row col turn) (setf (aref board row col) turn))

(defun copy-board (board)
  (let ((new-board (make-array Board-Dimensions)))
    (dotimes (row Board-Size)
      (dotimes (col Board-Size)
        (set-board new-board row col (ref-board board row col))))
    new-board))

(defun board-scan (board row0 col0 row-inc col-inc turn)
  (do ((row (+ row0 row-inc) (+ row row-inc)) (col (+ col0 col-inc) (+ col col-inc)) (length 0 (1+ length)))
      ((or (< row 0) (= row Board-Size) (< col 0) (= col Board-Size)
           (equal (ref-board board row col) Board-Empty))
       0)
      (cond ((equal (ref-board board row col) turn) (return length)))))

(defun board-movable (board row col turn)
  (and (equal (ref-board board row col) Board-Empty)
       (dolist (dir Board-Directions nil)
         (cond ((> (board-scan board row col (car dir) (cadr dir) turn) 0) (return t))))))

(defun board-movable-any (board turn)
  (block roro
    (dotimes (row Board-Size nil)
      (dotimes (col Board-Size)
        (cond ((equal (board-movable board row col turn) t) (return-from roro t)))))))

(defun board-state (board turn)
  (cond ((equal (board-movable-any board turn) t) nil)
        ((equal (board-movable-any board (change-turn turn)) t) 'pass)
        (t (board-eval board))))

(defun board-eval (board)
  (let ((count 0) (ste 0))
    (dotimes (row Board-Size (/ ste count))
      (dotimes (col Board-Size)
        (setq ste (+ ste (ref-board board row col)))
        (cond ((not (equal 0 (ref-board board row col))) (setq count (1+ count))))))))

(defun game-result (board state)
  (print-board board)
  (format t "~%")
  (cond ((< 0 state) (format t "Game Won by * ~%"))
        ((> 0 state) (format t "Game Won by o ~%"))
        (t (format t "Draw ~%")))
  (let ((sco 0) (gco 0))
    (dotimes (row Board-Size (format t "Score is ~d - ~d" sco gco))
      (dotimes (col Board-Size)
        (cond ((equal 1 (ref-board board row col)) (setq sco (1+ sco)))
              ((equal -1 (ref-board board row col)) (setq gco (1+ gco))))))))

(defun get-q-value (state action)
  (gethash (list state action) *q-table* 0.0))

(defun set-q-value (state action value)
  (setf (gethash (list state action) *q-table*) value))

(defun choose-action (state actions)
  (if (< (random 1.0) *epsilon*)
      (nth (random (length actions)) actions)
    (let ((q-values (mapcar (lambda (action) (get-q-value state action)) actions)))
      (nth (position (reduce 'max q-values) q-values) actions))))

(defun train-q-learning (state action reward next-state next-actions)
  (let ((q-value (get-q-value state action))
        (next-max-q (if (null next-actions) 0 (reduce 'max (mapcar (lambda (a) (get-q-value next-state a)) next-actions)))))
    (set-q-value state action (+ q-value (* *alpha* (+ reward (* *gamma* next-max-q) (- q-value)))))))

(defun play-by-machine (board turn count)
  (let ((state (copy-board board))
        (actions (remove-if-not (lambda (action) (board-movable board (first action) (second action) turn))
                                (loop for row from 0 to (1- Board-Size) append
                                      (loop for col from 0 to (1- Board-Size) collect (list row col)))))
        action next-state reward)
    (setq action (choose-action state actions))
    (board-move board (first action) (second action) turn)
    (setq next-state (copy-board board))
    (setq reward (board-eval board))
    (train-q-learning state action reward next-state
                      (remove-if-not (lambda (action) (board-movable next-state (first action) (second action) (change-turn turn)))
                                     (loop for row from 0 to (1- Board-Size) append
                                           (loop for col from 0 to (1- Board-Size) collect (list row col)))))
    (setf *epsilon* (max *min-epsilon* (* *epsilon* *epsilon-decay*)))
    (format t "(~A) My move is ~d ~d.~%" count (first action) (second action))))

(defun othello ()
  (do ((board (initialize-board)) (turn Board-Sente (change-turn turn)) (count 1 (1+ count)) state)
      ((numberp (setq state (board-state board turn))) (game-result board state))
    (print-board board)
    (cond ((equal state 'pass) (format t "(~A) Pass.~%" count))
          (t (play-by-machine board turn count)))))

(defun board-move (board row col turn)
  (set-board board row col turn)
  (dolist (dir Board-Directions board)
    (let ((row0 row) (col0 col))
      (dotimes (count (board-scan board row0 col0 (car dir) (cadr dir) turn))
        (setq row0 (+ row0 (car dir)))
        (setq col0 (+ col0 (cadr dir)))
        (setf (aref board row0 col0) turn)))))

(defun save-q-table ()
  (with-open-file (stream *q-table-file* :direction :output :if-exists :supersede)
    (maphash (lambda (key value)
               (format stream "~S ~S~%" key value))
             *q-table*)))

(defun load-q-table ()
  (when (probe-file *q-table-file*)
    (with-open-file (stream *q-table-file* :direction :input)
      (loop
        (let ((state-action (read stream nil nil))
              (value (read stream nil nil)))
          (when (or (null state-action) (null value))
            (return))
          (setf (gethash (list state-action) *q-table*) value))))))

(defun display-q-table ()
  (maphash (lambda (key value)
             (format t "State: ~A~%Action: ~A~%Q-value: ~A~%~%"
                     (first key) (second key) value))
           *q-table*))


;; (defun main ()
;;   (load-q-table)
;;   (loop repeat 3000 do (othello))
;;   (save-q-table))

;; (main)
;; (display-q-table)

; Lisp側でのインタフェース
(defvar out-filename nil)
(defvar lock-out-filename nil)
(defvar in-filename nil)
(defvar lock-in-filename nil)

; *** Cからのデータ（盤面）をファイルから読み込む関数 ***
(defun read-board-from-file (board)
  (let ((eos (cons nil nil)) hand)
    (do ()
	((and (probe-file in-filename) (not (probe-file lock-in-filename))))
        (sleep 1))
    (with-open-file (i-file in-filename)
	(dotimes (row Board-Size)
	    (dotimes (col Board-Size)
		(setq hand (read i-file nil eos))
		(cond ((eq hand eos)
	           (set-board board 0 0 nil)
		   (return-from read-board-from-file board)))
		(set-board board row col hand))))
    (delete-file in-filename)))

; *** Cへのデータ（次の打ち手）をファイルへ書き出す関数 ***
(defun write-move-to-file (row col)
  (with-open-file (o-file lock-out-filename :direction :output)
     (print 'lock o-file))
  (with-open-file (o-file out-filename :direction :output)
     (format o-file "~A ~A" row col))
  (delete-file lock-out-filename))


; *** ファイル名称 ***
(defconstant Owner "h20074")

(defun initialize (sente-gote)
  (let ((saki-ato (if (equal sente-gote Board-Sente) "saki" "ato")))
    (setq out-filename
	  (format nil "/tmp/te.~A.~A" Owner saki-ato))
    (setq lock-out-filename
	  (format nil "/tmp/te.~A.~A.lock" Owner saki-ato))
    (setq in-filename
	  (format nil "/tmp/ban.~A.~A" Owner saki-ato))
    (setq lock-in-filename
	  (format nil "/tmp/ban.~A.~A.lock" Owner saki-ato))
))

;main-loop
(defun main-loop (sente-gote yomi-depth)
  (initialize sente-gote)
  (let ((board (make-board)) (turn sente-gote))
    (read-board-from-file board)
    (do ((count (cond ((equal turn board-Sente) 1) (t 2)))))
    (+ count 2)
   (1st (ref-board board 0 0) (ref-board board 0 0))
   (state (board-state board turn)) node)
  ((and (/= 1st Board-Sente) (/= 1st Board-Gote) (/= 1st Board-Empty)))
  (setq state (board-state board turn))
  (cond ((or (equal state 'pass) (numberp state))
         (write-move-to-file -1 0))
        (t (setq node (minimax (make-node :board board))
             turn count
             yomi-depth)

         (write-move-to-file (node-row node) (node-col node))))
  (read-board-from-file board))
 
