;; 盤面の大きさ
(defconstant Board-Size 8)

;; 盤面配列の次元
(defconstant Board-Dimensions (list Board-Size Board-Size))

;; 盤面配列で先手の石を示す数
(defconstant Board-Sente 1)

;; 盤面配列で後手の石を示す数
(defconstant Board-Gote -1)

;; 盤面配列で空きを示す数
(defconstant Board-Empty 0)

;; 先手の石を示す表示文字列
(defconstant Board-Sente-String "*")

;; 後手の石を示す表示文字列
(defconstant Board-Gote-String "o")

;; 空きを示す表示文字列
(defconstant Board-Empty-String "-")

;; 盤面の初期状態
(defconstant Board-Initial
  #2A((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 -1 1 0 0 0)
      (0 0 0 1 -1 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))

;; 手番turnを引数に取り、次の手番を返す
(defun change-turn (turn)
    (cond
           ((equal turn Board-Sente) Board-Gote)
        (t Board-Sente)))

;; 手番turnを引数にとり、対応する石の表示文字列を返す
(defun symbol-turn (turn)
    (cond
           ((equal turn Board-Sente) Board-Sente-String)
        ((equal turn Board-Gote) Board-Gote-String)
        (t Board-Empty-String)))

;; 新しい盤を生成する
(defun make-board () (make-array Board-Dimensions))

;; 盤面board上のrow行col列の石を参照する
(defun ref-board (board row col) (aref board row col))

;; 盤面board上のrow行col列の石に石turnを設定する
(defun set-board (board row col turn)
    (setf (aref board row col) turn))

;; 盤面boardをコピーした新しい盤面を生成して返す
(defun copy-board (board)
    (let ((new-board (make-board)))
         (dotimes (row Board-Size new-board)
          (dotimes (col Board-Size)
              (set-board new-board row col (ref-board board row col))))))

;; 盤面boardの状態を表示する
(defun print-board (board)
  (format t "R/C 0 1 2 3 4 5 6 7~%")
  (dotimes (row Board-Size)
    (format t " ~D " row)
    (dotimes (col Board-Size)
      (format t " ~A" (symbol-turn (ref-board board row col))))
    (format t "~%"))
  (values))

;; 8方向の定数
(defconstant Board-Directions
    '((0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1) (-1 0) (-1 1)))

;; 連続して並んでいる石の個数を数えて返す
(defun sample-scan (board row0 col0 row-inc col-inc)
    (do
           (
                      (row row0 (+ row row-inc))
           
                      (col col0 (+ col col-inc))
           
                      (length 0 (1+ length)))
           (
            (or
                           (< row 0)
                           (= row Board-Size)
                           (< col 0)
                           (= col Board-Size)
                (equal (ref-board board row col) Board-Empty))
            length)))

;; はさんで反転できる石の数を返す
(defun board-scan (board row0 col0 row-inc col-inc turn)
    (let
        (
            (row (+ row0 row-inc))  
            (col (+ col0 col-inc))
            (length 0))
        (loop
                      (if
                                     (or
                                         (< row 0)
                                         (= row Board-Size)
                                         (< col 0)
                                         (= col Board-Size)
                                      (equal (ref-board board row col) Board-Empty))
                
                       (return 0))
            
           
                      (if (equal (ref-board board row col) turn)
                       (return length))
                      (setq row (+ row row-inc))
                      (setq col (+ col col-inc))
                      (setq length (1+ length)))))

;; 石が置けるかどうか判定する
(defun board-movable (board row col turn)
    (and (equal (ref-board board row col) Board-Empty)
        (dolist (dir Board-Directions nil)
            (cond ((> (board-scan board row col (car dir) (cadr dir) turn) 0)
                   (return t))))))

;; 石が置けるかどうか判定
(defun board-movable-any (board turn)
    (dotimes (row Board-Size)
        (dotimes (col Board-Size)
            (if (board-movable board row col turn)
                (return-from board-movable-any t)))))

;; オセロゲームの基本的な流れを実現する
(defun othello-by-human ()
  (do
         (
                    (board (copy-board Board-Initial))
           
                    (turn Board-Sente (change-turn turn))
           
                    (count 1 (1+ count))
          state)
        
      (
                    (numberp (setq state (board-state board turn)))
           
                    (game-result board state))
        
      (print-board board)
       
      (cond
                 ((equal state 'pass) (format t "(~A) Pass.~%" count))
              (t (play-by-human board turn count)))))

;; 途中段階、パス、終局を判定
(defun board-state (board turn)
    (cond
        ((board-movable-any board turn) nil)
        ((board-movable-any board (change-turn turn)) 'pass)
        (t (board-eval board))))

;; 石を数える
(defun count-stone (board)
    (let (stone (num-sente 0) (num-gote 0))
        (dotimes (row Board-Size)
            (dotimes (col Board-Size)
                (setq stone (ref-board board row col))
                (cond
                    ((equal stone Board-Sente) (setq num-sente (1+ num-sente)))
                    ((equal stone Board-Gote) (setq num-gote (1+ num-gote))))))
        (list num-sente num-gote)))

;; 盤面の評価関数
(defun board-eval (board)
    (let (lis num-sente num-gote)
        (setq lis (count-stone board))
        (setq num-sente (first lis))
        (setq num-gote (second lis))
        (/ (- num-sente num-gote) (+ num-sente num-gote))))

;; 盤面を表示し、ゲームの結果を表示する
(defun game-result (board state)
    (let (lis)
         (print-board board)
         (cond
          ((zerop state) (format t "Draw~%"))
          ((< 0 state) (format t "Game won by ~A~%" Board-Sente-String))
          (t (format t "Game won by ~A~%" Board-Gote-String)))
         (setq lis (count-stone board))
         (format t "Score is ~D - ~D~%" (first lis) (second lis))))

;; 手番turnを引数に取り、プレイヤーに行を入力させる
(defun play-by-human (board turn count)
    (format t "(~A) Enter row and col for ~A: " count (symbol-turn turn))
    (do
           (
            (row (read) (read))
            (col (read) (read)))
        (
            (and
                           (board-number-check row col)
                (board-movable board row col turn))
            
            (board-move board row col turn))
        
        (format t "Re-enter row and col for ~A: " (symbol-turn turn))))

;; 盤面を反転させる
(defun reverse-stones (board row col row-inc col-inc count turn)
  (dotimes (i count)
    (set-board board (+ row (* row-inc (1+ i))) (+ col (* col-inc (1+ i))) turn)))

;; 石を置き、更新された盤面を返す
(defun board-move (board row col turn)
 (when (board-movable board row col turn)
   (set-board board row col turn)
   (dolist (dir Board-Directions)
     (let* ((row-inc (car dir))
            (col-inc (cadr dir))
            (count (board-scan board row col row-inc col-inc turn)))
       (when (> count 0)
         (reverse-stones board row

 col row-inc col-inc count turn))))
   board))

;; rowおよびcolの値が数値かどうか、盤面boardの範囲内であるかどうか判定する
(defun board-number-check (row col)
 (and (integerp row)
      (integerp col)
      (<= 0 row (- Board-Size 1))
      (<= 0 col (- Board-Size 1))))

;; ノードを表す構造体
(defstruct node
 row col
 board
 val)

;; ストラテジーウェイトの初期化
(defparameter *strategy-weights*
  #2A((45 -15 0.225 -1.05 -1.05 0.225 -15 45) 
      (-15 -22.5 -4.5 -4.5 -4.5 -4.5 -22.5 -15) 
      (0.225 -4.5 0 -1.5 -1.5 0 -4.5 0.225) 
      (-1.05 -4.5 -1.5 -1.5 -1.5 -1.5 -4.5 -1.05)
      (-1.05 -4.5 -1.5 -1.5 -1.5 -1.5 -4.5 -1.05) 
      (0.225 -4.5 0 -1.5 -1.5 0 -4.5 0.225) 
      (-15 -22.5 -4.5 -4.5 -4.5 -4.5 -22.5 -15) 
      (45 -15 0.225 -1.05 -1.05 0.225 -15 45)))

;; ノードの評価関数
(defun eval-node (node turn count)
  (let* ((board (node-board node))
         (early-game-weight (if (< count 20) 1.0 0.0))
         (mid-game-weight (if (and (>= count 20) (< count 40)) 1.0 0.0))
         (late-game-weight (if (>= count 40) 1.0 0.0)))
    (+ (* early-game-weight
          (+ (* 0.1 (position-score board count *strategy-weights*))
             (* 0.3 (corner-control-score board))
             (* 0.3 (mobility-score board 20))
             (* 0.3 (calculate-fixed-stones-score board turn *strategy-weights*))))
       (* mid-game-weight
          (+ (* 0.15 (position-score board count *strategy-weights*))
             (* 0.3 (corner-control-score board))
             (* 0.2 (mobility-score board 40))
             (* 0.35 (calculate-fixed-stones-score board turn *strategy-weights*))))
       (* late-game-weight
          (+ (* 0.2 (position-score board count *strategy-weights*))
             (* 0.3 (corner-control-score board))
             (* 0.1 (mobility-score board 20))
             (* 0.4 (calculate-fixed-stones-score board turn *strategy-weights*)))))))

;; ポジションスコアの計算
(defun position-score (board count weight)
  (loop for row below Board-Size sum
        (loop for col below Board-Size
              for stone = (ref-board board row col)
              when (not (eq stone Board-Empty))
              sum (* 3 stone (aref *strategy-weights* row col)))))

;; チェス盤上の柔軟性
(defun mobility-score (board weight)
  (loop for row below Board-Size sum
        (loop for col below Board-Size
              sum (cond ((board-movable board row col Board-Sente)
                         (* Board-Sente (+ weight (aref *strategy-weights* row col))))
                        ((board-movable board row col Board-Gote)
                         (* Board-Gote (+ weight (aref *strategy-weights* row col))))
                        (t 0)))))

;; コーナーを優先し、クロスセルを減らす
(defun corner-control-score (board)
  (loop for (row col diag-row diag-col) in '((0 0 1 1) (0 7 1 6) (7 0 6 1) (7 7 6 6))
        for player = (ref-board board row col)
        for diagonal-player = (ref-board board diag-row diag-col)
        sum (cond ((and (eq player Board-Empty)
                        (eq diagonal-player Board-Sente))
                   -500)
                  ((and (eq player Board-Empty)
                        (eq diagonal-player Board-Gote))
                   500)
                  (t 0))))

;; フラグKが反転する
(defun calculate-fixed-stones-score (board turn weights)
  (let ((count 0)
        (corners '((0 0 1 0 0 1) (7 7 0 -1 -1 0) (0 7 0 -1 1 0) (7 0 -1 0 0 1))))
    (dolist (corner corners)
      (destructuring-bind (row col row-inc1 col-inc1 row-inc2 col-inc2) corner
        (let ((stone (ref-board board row col)))
          (when (not (equal stone Board-Empty))
            (if (equal stone turn)
                (progn
                  (incf count (fixed-scan board row col row-inc1 col-inc1 turn))
                  (incf count (fixed-scan board row col row-inc2 col-inc2 turn)))
                (progn
                  (decf count (fixed-scan board row col row-inc1 col-inc1 stone))
                  (decf count (fixed-scan board row col row-inc2 col-inc2 stone))))))))
    (* 3 turn (aref weights 0 0) count)))

;; ボード上の指定された方向における連続する固定石の数をスキャンする
(defun fixed-scan (board start-row start-col row0 col0 turn)
  (loop for row = start-row then (+ row row0)
        for col = start-col then (+ col col0)
        for count from 0
        while (and (>= row 0) (< row Board-Size)
                   (>= col 0) (< col Board-Size)
                   (eq (aref board row col) turn))
        finally (return count)))

;; ノードの展開
(defun expand-node (node turn)
 (let((node-list nil) (board nil) (new-node (make-node)))
   (dotimes (row Board-Size (reverse node-list))
     (dotimes (col Board-Size)
      (cond((equal (board-movable (node-board node) row col turn) t)
            (setq board (copy-board (node-board node)))
            (board-move board row col turn)
            (setq new-node (make-node
                            :row row
                            :col col
                            :board board))
            (setq node-list (cons new-node node-list)))
           (t t))))))

;; 終局の状態の関数
(defun node-state (node turn count)
  (board-state (node-board node) turn))

;; ミニマックスアルゴリズムを実装する
(defun minimax (node turn count depth)
 (let ((state (node-state node turn count)))
     (cond ((numberp state) (setf (node-val node) state) node)
      ((zerop depth) (setf (node-val node) (eval-node node turn count)) node)
      ((equal state 'pass) (minimax node (change-turn turn) count depth))
      (t (minimax-children (expand-node node turn) turn count depth)))))

;; 子ノードの評価
(defun minimax-children (children turn count depth)
  (let ((new-turn (change-turn turn))
        (new-depth (1- depth)) (new-count (1+ count)))
   (do ((val nil) v (chil nil))
    ((null children) chil)
    (setq v (node-val (minimax (car children) new-turn new-count new-depth)))
    (cond ((or (null val) 
            (and (equal turn Board-Sente) (> v val))
            (and (equal turn Board-Gote) (< v val)))
           (setq val v)
           (setq chil (make-node :row (node-row (car children))
                       :col (node-col (car children))
                       :board (node-board (car children))
                       :val val))))
               
    (setq children (cdr children)))))

;; 人間とコンピュータが対戦するオセロゲーム関数
(defun othello (sente-gote yomi-depth)
  (do ((board (copy-board Board-Initial))
       (turn Board-Sente (change-turn turn))
       (count 1 (1+ count))
       state)
      ((numberp (setq state (board-state board turn)))
       (game-result board state))
      (print-board board)
      (cond ((equal state 'pass)
             (format t "(~A) Pass. ~%" count) (setq count (1- count)))
       ((or (equal sente-gote Board-Empty) (equal turn sente-gote))
        (play-by-machine board turn count yomi-depth))
       (t (play-by-human board turn count)))))

;; コンピュータが最良の手を打つ
(defun play-by-machine (board turn count depth)
  (let (node)
        (setq node (alpha-beta (make-node :board board) turn count depth nil nil))
    (board-move board (node-row node) (node-col node) turn)
    (format t "count ~A - ~A.~%"
            count
            (if (= turn 1) "a-b: *" "enemy: o"))
    (format t "(~A) My move is ~d ~d~%" count (node-row node) (node-col node))))

;; αβ法により最良の節点を求める
(defun alpha-beta (node turn count depth alpha beta)
  (let ((state (node-state node turn count)))
    (cond ((numberp state) (setf (node-val node) state) node)
          ((zerop depth) (setf (node-val node) (eval-node node turn count)) node)
          ((equal state 'pass) (alpha-beta node (change-turn turn) count depth alpha beta))
          (t
                    (let ((result (alpha-beta-children (expand-node node turn) turn count depth alpha beta)))
                     (when (and alpha beta)
                       (cond
                                       ((equal turn Board-Sente) (setf alpha (max alpha (or (node-val result) most-negative-fixnum))))
                         (t (setf beta (min beta (or (node-val result) most-positive-fixnum))))))
                     result)))))

;; 最善手の子節点を返す
(defun alpha-beta-children (children turn count depth alpha beta)
  (let ((new-turn (change-turn turn))
        (new-depth (1- depth)) (new-count (1+ count)))
    (do ((val nil) v (chil nil))
     ((null children) chil)
     (cond ((and (numberp alpha) (numberp beta))
            (cond ((<= alpha beta) (setq children nil))
             (t (setq v (node-val (alpha-beta (car children) new-turn new-count new-depth alpha beta))))))
           (t (setq v (node-val (alpha-beta (car children) new-turn new-count new-depth alpha beta)))))
     (cond ((or (and (equal turn Board-Gote) (null val))
             (and (equal turn Board-Gote) (< v val)))
            (setq val v)
            (setq alpha v)
            (setq chil (make-node :row (node-row (car children))
                        :col (node-col (car children))
                        :board (node-board (car children))
                        :val val)))
             
           ((or (and (equal turn Board-Sente) (null val))
             (and (equal turn Board-Sente) (> v val)))
            (setq val v)
            (setq beta v)
            (setq chil (make-node :row (node-row (car children))
                        :col (node-col (car children))
                        :board (node-board (car children))
                        :val val))))
               
     (setq children (cdr children)))))

;; ファイル入出力用の変数
(defvar out-filename nil)
(defvar lock-out-filename nil)
(defvar in-filename nil)
(defvar lock-in-filename nil)

;; 盤面の状態をファイルから読み込む関数
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

;; 手番をファイルに書き込む関数
(defun write-move-to-file (row col)
  (with-open-file (o-file lock-out-filename :direction :output)
     (print 'lock o-file))
  (with-open-file (o-file out-filename :direction :output)
     (format o-file "~A ~A" row col))
  (delete-file lock-out-filename))

;; 所有者の名前
(defconstant Owner "h20074")

;; 初期化関数
(defun initialize (sente-gote)
  (let ((saki-ato (if (equal sente-gote Board-Sente) "saki" "ato")))
    (setq out-filename
     (format nil "/tmp/te.~A.~A" Owner saki-ato))
    (setq lock-out-filename
     (format nil "/tmp/te.~A.~A.lock" Owner saki-ato))
    (setq in-filename
     (format nil "/tmp/ban.~A.~A" Owner saki-ato))
    (setq lock-in-filename
     (format nil "/tmp/ban.~A.~A.lock" Owner saki-ato))))

;; 深さの調整
(defun adjust-depth (count depth)
  (cond ((or (= count 50) (= count 51)) (+ depth 2))
        ((or (= count 40) (= count 41)) (+ depth 1))
        (t depth)))

;; メインループ
(defun main-loop (sente-gote yomi-depth)
  (initialize sente-gote)
  (let ((board (make-board)) (turn sente-gote))
    (read-board-from-file board)
    (do ((count (cond ((equal turn Board-Sente) 1) (t 2))
          (+ count 2))
         (1st (ref-board board 0 0) (ref-board board 0 0))
         (state (board-state board turn)) node)
     ((and (/= 1st Board-Sente) (/= 1st Board-Gote) (/= 1st Board-Empty)))
     (setq state (board-state board turn))
     (cond ((or (equal state 'pass) (numberp state))
            (write-move-to-file -1 0))
           (t (setq node (alpha-beta (make-node :board board)
                          turn count
                          (adjust-depth count yomi-depth) nil nil ))
            (write-move-to-file (node-row node) (node-col node))))
     (read-board-from-file board))))

(othello 0 2)



;; 戦略の重みを定義する2次元配列
(defvar *strategy-weights*
  #2A((45 -15 0.225 -1.05 -1.05 0.225 -15 45) 
      (-15 -22.5 -4.5 -4.5 -4.5 -4.5 -22.5 -15) 
      (0.225 -4.5 0 -1.5 -1.5 0 -4.5 0.225) 
      (-1.05 -4.5 -1.5 -1.5 -1.5 -1.5 -4.5 -1.05)
      (-1.05 -4.5 -1.5 -1.5 -1.5 -1.5 -4.5 -1.05) 
      (0.225 -4.5 0 -1.5 -1.5 0 -4.5 0.225) 
      (-15 -22.5 -4.5 -4.5 -4.5 -4.5 -22.5 -15) 
      (45 -15 0.225 -1.05 -1.05 0.225 -15 45)))

;; 損失関数：ゲーム結果の逆数の平均を計算
;; 注意：Rが0の場合、除算エラーが発生する可能性がある
(defun loss-function (games-results)
  (let ((N (length games-results)))
    (if (zerop N)
        0
        (/ (reduce #'+ (mapcar (lambda (R) (/ 1.0 R)) games-results))
           (float N)))))

;; 勾配を計算する関数
(defun gradient (games-results)
  (let ((N (length games-results))
        (gradients (make-array (array-dimensions *strategy-weights*) :initial-element 0.0)))
    (dolist (R games-results)
      (dotimes (i Board-Size)
        (dotimes (j Board-Size)
          (let ((partial-R-wij (/ 1.0 (expt R 2))))
            (incf (aref gradients i j) (/ partial-R-wij N))))))
    gradients))

;; 戦略の重みを更新する関数
(defun update-strategy-weights (gradients learning-rate)
  (dotimes (i Board-Size)
    (dotimes (j Board-Size)
      (incf (aref *strategy-weights* i j) (- (* learning-rate (aref gradients i j)))))))

;; 最適化ループを実行する関数
(defun run-optimization-loop (num-iterations learning-rate board state)
  (let ((games-results nil))
    (dotimes (iteration num-iterations)
     (othello 0 6)
      (let ((result (game-result board state))) 
        (push result games-results)
        (let ((loss (loss-function games-results))
              (gradients (gradient games-results)))
          (update-strategy-weights gradients learning-rate)
          (format t "Iteration: ~D, Loss: ~F~%" iteration loss))))))

;; (run-optimization-loop 200 0.1)


