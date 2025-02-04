; --------
; 例題1
; 盤面の大きさ
;(defconstant Board-Size 4)
(defconstant Board-Size 8)
; 盤面配列の次元
(defconstant Board-Dimensions (list Board-Size Board-Size))
; 盤面配列で先手の石を示す数
(defconstant Board-Sente 1)
; 盤面配列で後手の石を示す数
(defconstant Board-Gote -1)
; 盤面配列で空きを示す数
(defconstant Board-Empty 0)
; 先手の石を示す表示文字列
(defconstant Board-Sente-String "*")
; 後手の石を示す表示文字列
(defconstant Board-Gote-String "o")
; 空きを示す表示文字列
(defconstant Board-Empty-String "-")

; 盤面の初期状態
(defconstant Board-Initial
    ; 4×4
    ;; #2A((0 0 0 0) (0 -1 1 0) (0 1 -1 0) (0 0 0 0))
    ; 8x8
  #2A((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 -1 1 0 0 0)
      (0 0 0 1 -1 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))

; 例題2
; 手番turnを引数に取り、次の手番を返す
(defun change-turn (turn)
    (cond 
        ((equal turn Board-Sente) Board-Gote)
        (t Board-Sente)))

>(change-turn Board-Sente)

-1

>(change-turn Board-Gote)

1

; 練習1
; 手番turnを引数にとり、対応する石の表示文字列を返す
(defun symbol-turn (turn)
    (cond 
        ((equal turn Board-Sente) Board-Sente-String)
        ((equal turn Board-Gote) Board-Gote-String)
        (t Board-Empty-String)))

 >(symbol-turn Board-Sente)

"*"

>(symbol-turn Board-Empty)

"-"

; 例題3
; 新しい盤を生成する
(defun make-board () (make-array Board-Dimensions))
; 盤面board上のrow行col列の石を参照する
(defun ref-board (board row col) (aref board row col))
; 盤面board上のrow行col列の石に石turnを設定する
(defun set-board (board row col turn)
    (setf (aref board row col) turn))

; 例題4
; 盤面boardをコピーした新しい盤面を生成して返す
(defun copy-board (board)
    (let ((new-board (make-board)))
        ; new-boardをletの値として返す
        (dotimes (row Board-Size new-board)
            (dotimes (col Board-Size)
                (set-board new-board row col (ref-board board row col))
            )))) 

>(copy-board Board-Initial)

  #2A((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 -1 1 0 0 0)
      (0 0 0 1 -1 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0))


; 練習2
; 盤面boardの状態を表示する
(defun print-board (board)
  (format t "R/C 0 1 2 3 4 5 6 7~%")
  (dotimes (row Board-Size)
    (format t " ~D " row)
    (dotimes (col Board-Size)
      (format t " ~A" (symbol-turn (ref-board board row col))))
    (format t "~%"))
   (values))

>(print-board Board-Initial)
R/C 0 1 2 3 4 5 6 7
; -------------------
0 | - - - - - - - -
1 | - - - - - - - -
2 | - - - - - - - -
3 | - - - o * - - -
4 | - - - * o - - -
5 | - - - - - - - -
6 | - - - - - - - -
7 | - - - - - - - -

; 例題5
; 8方向の定数
(defconstant Board-Directions
    '((0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1) (-1 0) (-1 1)))

>Board-Directions

((0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1) (-1 0) (-1 1))

; 例題6
; 連続して並んでいる石の個数を数えて返す
(defun sample-scan (board row0 col0 row-inc col-inc)
    (do 
        (
            ; 変数rowをrow0からrow-incずつ変える
            (row row0 (+ row row-inc))
            ; 変数colをcol0からcol-incずつ変える
            (col col0 (+ col col-inc))
            ; 変数lengthを0から1ずつ増やす
            (length 0 (1+ length)))
        ; 盤面の外か石が置かれていなければ、lengthを返す
        (
            (or 
                (< row 0) 
                (= row Board-Size) 
                (< col 0) 
                (= col Board-Size)
                (equal (ref-board board row col) Board-Empty)
            )
            length)))

>(sample-scan Board-Initial 2 2 -1 -1)

2

>(sample-scan Board-Initial 2 2 0 1)

1

; 練習3 
; はさんで反転できる石の数を返す
(defun board-scan (board row0 col0 row-inc col-inc turn)
    (let
        (
            (row (+ row0 row-inc))   
            (col (+ col0 col-inc))
            (length 0))
        (loop
            ; 盤面の外か石が置かれていなければ、0を返す
            (if 
                (or 
                    (< row 0) 
                    (= row Board-Size) 
                    (< col 0) 
                    (= col Board-Size)
                    (equal (ref-board board row col) Board-Empty)
                )
                (return 0))
            ; 自分の石が置かれていれば、lengthを返す
            (if (equal (ref-board board row col) turn)
                (return length))
            ; 変数rowをrow0からrow-incずつ変える
            (setq row (+ row row-inc))
            ; 変数colをcol0からcol-incずつ変える
            (setq col (+ col col-inc))
            ; 変数lengthを0から1ずつ増やす
            (setq length (1+ length)))))

>(board-scan Board-Initial 1 0 0 1 Board-Sente)

1

>(board-scan Board-Initial 1 0 -1 -1 Board-Gote)

0

; 例題7
; 石が置けるかどうか判定する
(defun board-movable (board row col turn)
    (and (equal (ref-board board row col) Board-Empty)
        (dolist (dir Board-Directions nil)
            (cond ((> (board-scan board row col (car dir) (cadr dir) turn) 0)
            (return t))))))

>(board-movable Board-Initial 0 1 Board-Sente)

T

>(board-movable Board-Initial 0 1 Board-Gote)

NIL

; 練習4
; 石が置けるかどうか判定
(defun board-movable-any (board turn)
    (dotimes (row Board-Size)
        (dotimes (col Board-Size)
            (if (board-movable board row col turn)
                (return-from board-movable-any t)))))

>(board-movable-any Board-Initial Board-Sente)

T

; 例題8
; オセロゲームの基本的な流れを実現する
(defun othello-by-human ()
    (do 
        (
            ; 初期盤面の複製を作る
            (board (copy-board Board-Initial))
            ; 手番を交代する
            (turn Board-Sente (change-turn turn))
            ; 手数を増加させる
            (count 1 (1+ count))
            state)
        (
            ; 終局ならば
            (numberp (setq state (board-state board turn)))
            ; 終了画面
            (game-result board state))
        ; 盤面を表示
        (print-board board)
        ; パスの処理
        (cond 
            ((equal state 'pass) (format t "(~A) Pass.~%" count))
            ; 人間が石を置く
            (t (play-by-human board turn count)))))

; 練習5
; 途中段階、パス、終局を判定
(defun board-state (board turn)
    (cond
        ((board-movable-any board turn) nil)
        ((board-movable-any board (change-turn turn)) 'pass)
        (t (board-eval board))))

>(board-state Board-Initial Board-Sente)

NIL

>(board-state #2A((-1 -1 -1 1) (-1 -1 -1 0) (-1 1 -1 -1) (1 0 1 -1)) Board-Sente)

PASS

; 練習6
; 盤面の評価関数
(defun board-eval (board)
    (let (lis num-sente num-gote)
        (setq lis (count-stone board))
        (setq num-sente (first lis))
        (setq num-gote (second lis))
        (/ (- num-sente num-gote) (+ num-sente num-gote))))

>(board-eval Board-Initial)

0.0

; 練習7
; 石を数える
(defun count-stone (board)
    (let (stone (num-sente 0) (num-gote 0))
        (dotimes (row Board-Size)
            (dotimes (col Board-Size)
                (setq stone (ref-board board row col))
                (cond
                    ((equal stone Board-Sente) (setq num-sente (1+ num-sente)))
                    ((equal stone Board-Gote) (setq num-gote (1+ num-gote))))))
        (list num-sente num-gote)))

; 盤面を表示し、ゲームの結果を表示する
(defun game-result (board state)
    (let (lis)
        ; 盤面の表示
        (print-board board)
        ; 勝者の表示
        (cond
            ((zerop state) (format t "Draw~%"))
            ((< 0 state) (format t "Game won by ~A~%" Board-Sente-String))
            (t (format t "Game won by ~A~%" Board-Gote-String)))
        ; 石を数える
        (setq lis (count-stone board))
        ; スコアの表示
        (format t "Score is ~D - ~D~%" (first lis) (second lis))))

>(game-result #2A((-1 -1 -1 -1 -1 -1 1 -1)
    (1 1 1 1 1 1 1 -1)
    (-1 -1 -1 -1 1 -1 -1 -1)
    (-1 -1 -1 -1 -1 -1 -1 -1)
    (-1 -1 -1 -1 1 -1 1 -1)
    (-1 1 -1 -1 1 1 -1 -1)
    (-1 -1 -1 -1 -1 -1 -1 -1)
    (-1 -1 -1 -1 -1 -1 -1 1)) 7/15)

R/C 0 1 2 3 4 5 6 7
 0  o o o o o o * o
 1  * * * * * * * o
 2  o o o o * o o o
 3  o o o o o o o o
 4  o o o o * o * o
 5  o * o o * * o o
 6  o o o o o o o o
 7  o o o o o o o *
Game won by o
Score is 15 - 49
NIL

; 例題9
; 次の手を打ち、盤面を更新する
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

; 練習8
(defun reverse-stones (board row col row-inc col-inc count turn)
  (dotimes (i count)
    (set-board board (+ row (* row-inc (1+ i))) (+ col (* col-inc (1+ i))) turn)))

; 石を置き、更新された盤面を返す
(defun board-move (board row col turn)
  (when (board-movable board row col turn)
    (set-board board row col turn)
    (dolist (dir Board-Directions)
      (let* ((row-inc (car dir))
             (col-inc (cadr dir))
             (count (board-scan board row col row-inc col-inc turn)))
        (when (> count 0)
          (reverse-stones board row col row-inc col-inc count turn))))
    board))

; 練習9
; rowおよびcolの値が数値かどうか、盤面boardの範囲内であるかどうか判定する
(defun board-number-check (row col)
  (and (integerp row)
       (integerp col)
       (<= 0 row (- Board-Size 1))
       (<= 0 col (- Board-Size 1))))

; 練習１０
;;実行例
;; >(othello-by-human)
;; R/C0123
;;   0----
;;   1-o*-
;;   2-*o-
;;   3----
;; (1) Enter row and col for *: 1 0
;; R/C0123
;;   0----
;;   1***-
;;   2-*o-
;;   3----

;; ...
;; ...

;; (9) Enter row and col for *: 0 3
;; Re-enter row and col for *: 3 2
;; R/C0123
;;   0-oo-
;;   1*o**
;;   2*o**
;;   3***-
;; Game won by *
;; Score is 9 - 4
;; NIL

; 例題 10
(defstruct node
  row col ;この盤面に至るための手
  board ;ノードに対応する盤面
  val ;盤面の評価値
)

; --------
; 例題 11
(defun eval-node (node turn count) 0)

; --------
;練習 11
;; ノードの展開
(defun expand-node (node turn)
  (let ((result nil))
    (dotimes (row Board-Size (reverse result))
      (dotimes (col Board-Size)
	(cond ((eql t (board-movable (copy-board (node-board node)) row col turn))
	       (setq result (cons (make-node :row row :col col :board (board-move (copy-board (node-board node)) row col turn) :val nil) result))))))))

>(expand-node (make-node :board Board-Initial) Board-Sente)
#S(NODE ROW 0 COL 1 BOARD 
    #2A((1 0 0 0) (0 1 1 0) (0 0 0 0) (0 0 0 0)) VAL NIL)
#S(NODE ROW 1 COL 0 BOARD 
    #2A((0 0 0 0) (1 1 1 0) (0 0 0 0) (0 0 0 0)) VAL NIL)
#S(NODE ROW 2 COL 3 BOARD 
    #2A((0 0 0 0) (0 1 1 0) (0 0 0 1) (0 0 0 0)) VAL NIL)

; --------
; 例題 12	
;終局の状態の関数
(defun node-state (node turn count)
  (board-state (node-board node) turn))

> (node-state (make-node :board Board-Initial) Board-Sente 1)
NIL

; --------
;練習 12
(defun minimax (node turn count depth)
  (let ((state (node-state node turn count)))
    ;(setq count_min (1+ count_min))
    (cond ((numberp state) (setf (node-val node) state) node);終局
	  ((zerop depth) (setf (node-val node) (eval-node node turn count)) node);先読みの深さの限界
	  ((equal state 'pass) (minimax node (change-turn turn) count depth));Passされたときの処理
	  (t (minimax-children (expand-node node turn) turn count depth)))))

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

> (minimax (make-node :board Board-Initial) Board-Sente 1 2)
#S(NODE ROW 0 COL 1 BOARD #2A((0 1 0 0) (0 1 1 0) (0 1 -1 0) (0 0 0 0)) VAL 0)

; --------
; 例題 13
;人間とコンピュータが対戦するオセロゲーム関数
(defun othello (sente-gote yomi-depth)
  (do ((board (copy-board Board-Initial))
       (turn Board-Sente (change-turn turn))
       (count (1+ count)))
      (state)
    ((numberp (setq state (board-state board turn)))
     (game-result board state))
    (print-board board)
    (cond ((equal state 'pass)
           (format t "(A) Pass.~%" count) (setq count (1- count)))
          ((or (equal sente-gote Board-Empty) (equal turn sente-gote))
           (play-by-machine board turn count yomi-depth))
          (t (play-by-human board turn count)))))

; --------
;練習 13
(defun play-by-machine (board turn count depth)
  (let((node (make-node)))
    (setq node (minimax (make-node :board board) turn count depth))
    (board-move board (node-row node) (node-col node) turn);次に打つ手を設定
    (format t "(~A) My move is ~d ~d." count (node-row node) (node-col node))))

> (play-by-machine board Board-Sente 1 2)
(1) My move is 0 3.
NIL

; --------
;練習 14
> (othello Board-Empty 3)
R/C 0 1 2 3
  0 - - - -
  1 - o * -
  2 - * o -
  3 - - - -

(1) My move is 0 1

R/C 0 1 2 3
  0 - * - -
  1 - * * -
  2 - * o -
  3 - - - -

(2) My move is 3 2

R/C 0 1 2 3
  0 - * o -
  1 - * o -
  2 - * o -
  3 - - - -

(3) My move is 0 3

R/C 0 1 2 3
  0 - * * *
  1 - * * -
  2 - * o -
  3 - - - -

(4) My move is 0 0

R/C 0 1 2 3
  0 o * * *
  1 - o * -
  2 - * o -
  3 - - - -

(5) My move is 1 0

R/C 0 1 2 3
  0 o * * *
  1 * * * -
  2 - * o -
  3 - - - -

(6) My move is 2 0

R/C 0 1 2 3
  0 o * * *
  1 o * * -
  2 o o o -
  3 - - - -

(7) My move is 3 3

R/C 0 1 2 3
  0 o * * *
  1 o * * -
  2 o o * -
  3 - - - *

(8) My move is 1 3

R/C 0 1 2 3
  0 o * * *
  1 o o o o
  2 o o * -
  3 - - - *

(9) My move is 2 3

R/C 0 1 2 3
  0 o * * *
  1 o o * *
  2 o o * *
  3 - - - *

(11) My move is 3 1

R/C 0 1 2 3
  0 o * * *
  1 o * * *
  2 o * * *
  3 - * - *

(12) My move is 3 2

R/C 0 1 2 3
  0 o * * *
  1 o * * *
  2 o o * *
  3 - * o *

(12) My move is 3 0

R/C 0 1 2 3
  0 o * * *
  1 o * * *
  2 o * * *
  3 * * o *

Game won by *
Score is 12 - 4
NIL
; --------
;練習 15
; 番めんの大きさを８ｘ８に変更し、
;コンピュータ対コンピュータのオセロゲームの動作を確認
(defconstant Board-Size 8)

> Board-Size
8

> (othello Board-Empty 3)
;... 略
(57) My move is 1 1.
R/C 0 1 2 3 4 5 6 7
  0 - o o o o o o o
  1 o * - - * o o o
  2 o * * * * o o o
  3 o * o o o o o o
  4 o * * o o o o o
  5 o * o * o o o o
  6 o o * * * o o o
  7 o o o o o o o o
(58) My move is 0 0.
R/C 0 1 2 3 4 5 6 7
  0 o o o o o o o o
  1 o o - - * o o o
  2 o * o * * o o o
  3 o * o o o o o o
  4 o * * o o o o o
  5 o * o * o o o o
  6 o o * * * o o o
  7 o o o o o o o o
(59) My move is 1 2.
R/C 0 1 2 3 4 5 6 7
  0 o o o o o o o o
  1 o o * - * o o o
  2 o * * * * o o o
  3 o * * o o o o o
  4 o * * o o o o o
  5 o * o * o o o o
  6 o o * * * o o o
  7 o o o o o o o o
(60) My move is 1 3.
R/C 0 1 2 3 4 5 6 7
  0 o o o o o o o o
  1 o o o o o o o o
  2 o * o o o o o o
  3 o o * o o o o o
  4 o * * o o o o o
  5 o * o * o o o o
  6 o o * * * o o o
  7 o o o o o o o o

Game Won by o 
Score is 9 - 55
NIL
; --------
; 練習16
; αβ法により最良の節点を求める
(defun alpha-beta (node turn count depth alpha beta)
  ; ゲーム状態を取得
  (let ((state (node-state node turn count)))
    (cond ((numberp state) ; 状態が数値の場合、その値を節点に設定して返す
           (setf (node-val node) state) node)
          ((zerop depth) ; 深さがゼロの場合、節点の評価値を計算して設定
           (setf (node-val node) (eval-node node turn count)) node)
          ((equal state 'pass) ; パスの場合、ターンを変更して再帰的に探索
           (alpha-beta node (change-turn turn) count depth alpha beta))
          (t ; 子ノードをαβ法で探索し、最良の結果を取得
           (let ((result (alpha-beta-children (expand-node node turn) turn count depth alpha beta)))
             (when (and alpha beta) ; αとβが定義されている場合、値を更新
               (cond ((equal turn Board-Sente) ; 先手の場合、αを更新
                      (setf alpha (max alpha (or (node-val result) most-negative-fixnum))))
                     (t ; 後手の場合、βを更新
                      (setf beta (min beta (or (node-val result) most-positive-fixnum))))))
             result)))))

; 最善手の子節点を返す
(defun alpha-beta-children (children turn count depth alpha beta)
  (let ((new-turn (change-turn turn))
        (new-depth (1- depth)) (new-count (1+ count)))
    (do ((val nil) v (chil nil))
        ((null children) chil) ; 子ノードがなくなるまでループ
      (cond ((and (numberp alpha) (numberp beta))
             (cond ((<= alpha beta) ; αがβ以上なら、探索を打ち切る
                    (setq children nil))
                   (t ; 子ノードをαβ法で評価
                    (setq v (node-val (alpha-beta (car children) new-turn new-count new-depth alpha beta))))))
            (t ; 子ノードをαβ法で評価
             (setq v (node-val (alpha-beta (car children) new-turn new-count new-depth alpha beta)))))
      (cond ((or (and (equal turn Board-Gote) (null val))
                 (and (equal turn Board-Gote) (< v val))) ; 後手の場合、最小値を探す
             (setq val v)
             (setq alpha v)
             (setq chil (make-node :row (node-row (car children))
                                   :col (node-col (car children))
                                   :board (node-board (car children))
                                   :val val)))
            ((or (and (equal turn Board-Sente) (null val))
                 (and (equal turn Board-Sente) (> v val))) ; 先手の場合、最大値を探す
             (setq val v)
             (setq beta v)
             (setq chil (make-node :row (node-row (car children))
                                   :col (node-col (car children))
                                   :board (node-board (car children))
                                   :val val))))
      (setq children (cdr children))))) ; 次の子ノードに進む

; 練習17
; minimax と alpha-beta法の呼び出した回数確認
> (play-by-machine (copy-board Board-Initial) Board-Sente 1 5)
(1713 nodes checked.)
(182 nodes checked.)
(1) My move is 2 3

;; ------------------------------------------------------------------

;; eval-node評価関数
;; ストラテジーウェイトの初期化
;; この行列は、ボード上の各位置に対する重みを表する。
;; 高い正の値は有利な位置、負の値は不利な位置を示する。
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
;; 現在のボード状態とゲームの段階に基づいてノードを評価する。
(defun eval-node (node turn count)
  ;; ゲームの早期、中期、後期の重みを設定する。
　;; 各ゲーム段階に応じた評価値を計算して合計(全部100％)
  (let* ((board (node-board node))
         (early-game-weight (if (< count 20) 1.0 0.0))
         (mid-game-weight (if (and (>= count 20) (< count 40)) 1.0 0.0))
         (late-game-weight (if (>= count 40) 1.0 0.0)))

(+ (* early-game-weight
      ;; 初期段階ではポジションスコアが低めに評価される。移動の自由度は優先
      (+ (* 0.1 (position-score board count *strategy-weights*))
         (* 0.3 (corner-control-score board))
         (* 0.3 (mobility-score board 20))
         (* 0.3 (calculate-fixed-stones-score board turn *strategy-weights*))))
   (* mid-game-weight
      ;; 中期段階では移動の自由度が低め、ポジションスコアの影響が大きくなる
      (+ (* 0.15 (position-score board count *strategy-weights*))
         (* 0.3 (corner-control-score board))
         (* 0.2 (mobility-score board 40))
         (* 0.35 (calculate-fixed-stones-score board turn *strategy-weights*))))
   (* late-game-weight
      ;; 後期段階ではポジションスコアや固定石のスコアの影響がさらに大きくなる
      (+ (* 0.2 (position-score board count *strategy-weights*))
         (* 0.3 (corner-control-score board))
         (* 0.1 (mobility-score board 20))
         (* 0.4 (calculate-fixed-stones-score board turn *strategy-weights*)))))))

;; ポジションスコアの計算
;; ボード上の各位置の石のポジションスコアを計算する。
(defun position-score (board count weight)
  ;; 各行と列のスコアを計算して合計
  (loop for row below Board-Size sum
        (loop for col below Board-Size
              for stone = (ref-board board row col)
              when (not (eq stone Board-Empty))
              sum (* 3 stone (aref *strategy-weights* row col)))))

;; モビリティスコアの計算
;; ボード上の各位置の移動可能性を評価する。
(defun mobility-score (board weight)
  ;; ボード上の各位置での可動性を評価
  (loop for row below Board-Size sum
        (loop for col below Board-Size
              sum (cond ((board-movable board row col Board-Sente)
                         (* Board-Sente (+ weight (aref *strategy-weights* row col))))
                        ((board-movable board row col Board-Gote)
                         (* Board-Gote (+ weight (aref *strategy-weights* row col))))
                        (t 0)))))

;; コーナーコントロールスコアの計算
;; ボードのコーナーとその対角位置を評価する。
(defun corner-control-score (board)
  ;; コーナーとその対角位置を評価
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

;; 固定石スコアの計算
;; ボードのコーナーとその周辺の固定石スコアを計算する。
(defun calculate-fixed-stones-score (board turn weights)
  (let ((count 0)
        ;; コーナーとその周辺の座標を設定
        (corners '((0 0 1 0 0 1) (7 7 0 -1 -1 0) (0 7 0 -1 1 0) (7 0 -1 0 0 1))))
    ;; 各コーナーについて固定石をスキャンしてスコアを計算
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
    ;; スコアを計算して返す
    (* 3 turn (aref weights 0 0) count)))

;; 固定石のスキャン
;; ボード上の指定された方向における連続する固定石の数をスキャンする。
(defun fixed-scan (board start-row start-col row0 col0 turn)
  ;; ボードの範囲内で、指定された方向に向かって連続する石の数を数えます。
  (loop for row = start-row then (+ row row0)
        for col = start-col then (+ col col0)
        for count from 0

        ;; ボードの範囲内かつ指定されたターンの石である限りループ
        while (and (>= row 0) (< row Board-Size)　
                   (>= col 0) (< col Board-Size)
                   (eq (aref board row col) turn))
        finally (return count)))　;; finally　はFalseでも、最終的にcountを返す

(defvar out-filename nil)
(defvar lock-out-filename nil)
(defvar in-filename nil)
(defvar lock-in-filename nil)(defun read-board-from-file (board)
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
(defun write-move-to-file (row col)
  (with-open-file (o-file lock-out-filename :direction :output)
     (print 'lock o-file))
  (with-open-file (o-file out-filename :direction :output)
     (format o-file "~A ~A" row col))
  (delete-file lock-out-filename))

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
     (format nil "/tmp/ban.~A.~A.lock" Owner saki-ato))))

(defun adjust-depth (count depth)
  (cond ((or (= count 50) (= count 51)) (+ depth 2))
        ((or (= count 40) (= count 41)) (+ depth 1))
        (t depth)))

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
;; ------------------------------------------------------------------

評価関数の説明：
`eval-node` 関数は、現在のボード状態とゲームの段階に基づいてノードを評価する。この関数では、ゲームの早期、中期、後期に応じて異なる重みを設定し、それぞれの段階で適切な評価値を計算して合計する。

1. ゲームの早期:
   - `early-game-weight` (count < 20) に設定され、ポジションスコアは低めに評価されます。
   - 移動の自由度 (`mobility-score`) が高く評価されます。
   - ポジションスコア、コーナーコントロールスコア、移動の自由度、固定石スコアを計算して合計する。

2. ゲームの中期:
   - `mid-game-weight` (count < 40) に設定され、ポジションスコアの影響が大きくなります。
   - 移動の自由度の影響は少し減ります。
   - 各スコアを計算して合計する。

3. ゲームの後期:
   - `late-game-weight` (count >= 40) に設定され、ポジションスコアと固定石スコアの影響がさらに大きくなります。
   - 移動の自由度の影響は最も低くなります。
   - 各スコアを計算して合計する。

;; 計算機能
1. `position-score`:
   - ボード上の各石の位置に基づいてポジションスコアを計算する。
   - 各位置の重みと石の値を掛け合わせ、その合計を求めます。

2. `mobility-score`:
   - ボード上の各位置の移動可能性を評価する。
   - 各位置で移動可能な場合、その位置の重みとターンの値を掛け合わせ、その合計を求めます。

3. `corner-control-score`:
   - ボードのコーナーとその対角位置を評価する。
   - コーナーが空で対角位置に石がある場合、スコアに大きなペナルティまたはボーナスを与えます。

4. `calculate-fixed-stones-score`:
   - ボードのコーナーとその周辺の固定石スコアを計算する。
   - 各コーナーについて、連続する固定石の数をスキャンし、その合計スコアを計算する。

    4.1 `fixed-scan`:
    - ボード上の指定された方向における連続する固定石の数をスキャンする。
    - 指定された方向に向かって、連続する石の数を数え、その数を返する。
 
;; `eval-node` の実行例：
;指定された値を使用して特定のノードを作成する

;; 集計関数を作成し、「ボード」の状態を評価するスコアを出力する。
(defun evaluate-node (node)
  (let ((board (node-board node))
        (turn 1)
        (count 22))
    (let ((position-score-val (position-score board turn count *strategy-weights*))
          (evaluation (eval-node node turn count))
          (corner-control-score-val (corner-control-score board turn))
          (mobility-score-val (mobility-score board turn 20))
          (calculate-fixed-stones-score-val (calculate-fixed-stones-score board turn *strategy-weights*)))
      (format t "Position score: ~A~%" position-score-val)
      (format t "Mobility score: ~A~%" mobility-score-val)
      (format t "Fixed stones score: ~A~%" calculate-fixed-stones-score-val)
      (format t "Corner control score: ~A~%" corner-control-score-val)
      (format t "Eval-node score: ~A~%" evaluation)
      (eval-node node turn count))))

(defvar node1
  (make-node 
   :row 0 
   :col 0
   :board 
#2A(
(-1 -1 -1 -1 0 0 0 0) 
(0 0 0 0 0 0 -1 0)
(0 0 0 1 0 0 0 0) 
(0 0 -1 -1 1 0 0 0)
(0 0 0 1 -1 0 0 0) 
(0 0 0 0 1 0 0 1)
(0 0 0 -1 0 0 1 1) 
(0 0 0 1 0 0 0 1))
   :val 0))

(defvar node2
  (make-node 
   :row 0 
   :col 0
   :board 
#2A(
(-1 -1 -1 -1 0 0 0 0) 
(0 0 0 0 0 0 -1 0)
(0 -1 -1 1 0 0 0 0) 
(0 0 -1 -1 1 0 0 0)
(0 0 0 1 -1 0 0 0) 
(1 1 1 0 1 0 0 1)
(0 0 0 -1 0 0 1 1) 
(-1 1 1 1 0 0 0 1))
   :val 0))


;; 評価固有ノード関数を呼び出して実行し、評価値を出力する。
> (evaluate-node node1)

Position score: 6.0000076
Mobility score: -15.9
Fixed stones score: -90
Corner control score: 500
Eval-node score: 112.22

> (evaluate-node node2)

Position score: -119.399994
Mobility score: -5.799999
Fixed stones score: -270
Corner control score: 500
Eval-node score: 36.429993

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
;; 注意：部分導関数の計算が正しいか確認が必要
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
;; 注意：othello関数が定義されていないため、このコードは実行できない
(defun run-optimization-loop (num-iterations learning-rate)
  (let ((games-results nil))
    (dotimes (iteration num-iterations)
      (let ((result (othello 0 6)))  ;; othello関数が定義されていない
        (push result games-results)
        (let ((loss (loss-function games-results))
              (gradients (gradient games-results)))
          (update-strategy-weights gradients learning-rate)
          (format t "Iteration: ~D, Loss: ~F~%" iteration loss))))))

;; (run-optimization-loop 200 0.1)

> END

