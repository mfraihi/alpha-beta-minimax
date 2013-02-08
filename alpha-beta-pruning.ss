;; Mohammed AlFraihi
;; Minimax-bot
;;--------------------------------------------
;;  in this file, I implement a tronbot that runs the minimax-bot with alpha-beta pruning.
;; the function 'utility' defined below is my heuristic function that calculates the value
;; of a node. max-depth is defined below on line 9.

(load "tronapi.ss")
(define max-depth 8)

(define-tron-brain (minimax-bot
		    ("Mohammed" size orig-walls play ppos opos)
		    (walls orig-walls))
  (let ([new-walls (cons* ppos opos walls)])
    (define (safe? m)
      (and (not (member (get-pos m ppos size) new-walls)) #t))

    ;; This calls your minimax with these arguments
    ;; and your minimax should return a direction out of: (n s w e)
    ;; or #f if no safe move exists
    (let ([minimax-move (minimax size orig-walls ppos opos new-walls)])
      (play
       
       ;; Here, if minimax returned a direction, just play that,
       ;; otherwise choose a random direction because you are
       ;; going to crash :(
       (if minimax-move
	   minimax-move
	   (list-ref valid-moves (random (length valid-moves)))))
      new-walls)))

(define minimax
  (lambda (size walls ppos opos new-walls)

    (let* ((brd (make-board size walls ppos opos))
	   (node (make-node brd 'NULL 'NULL max-depth 0))
	   (children (get-children-max node))
	   (move (choose-move children -100 'NULL new-walls)))
      (if (not (equal? move 'NULL))
	  move
	  #f))))	  

;;takes care of choosing the move after each child node is evaluated
(define choose-move
  (lambda (children maximum operator walls)
    (if (null? children)
	operator
	(let ((value (max-value (car children))))
	  (if (and (> value maximum) (safe-move? (node-operator (car children)) (car children) walls))
	      (choose-move (cdr children) value (node-operator (car children)) walls)
	      (choose-move (cdr children) maximum operator walls))))))

;; checks if a chosen move is safe.
(define safe-move?
  (lambda (op node walls)
    (let ((brd (node-to-board node)))	  
      (if (not (member (my-pos brd) walls))
	  #t
	  #f))))

;; here comes the maximum-value calculation part. the functions max-value and min-value bounce to each other,
;; calculating the values of all child nodes until the maximum depth has been reached.
(define (max-value node)
  (let ((brd (node-to-board node))
        (depth (get-depth node))
	(maximum -1000))
    (if (or (terminal-test? brd)
            (= depth 0))
        (utility brd)
        (let loop ([children (get-children-max node)])
          (if (not (null? children))
	      (begin
		(let ((value (min-value (car children))))
		(if (> value maximum) (set! maximum value))
		(loop (cdr children))))
	      maximum)))))

;; min-value takes care of the MIN part of the minimax algorithm, stops and returns utility value
;; of a board when it reaches the maximum depth, or if it finds a terminal-state board.
(define (min-value node)
   (let ((brd (node-to-board node))
        (depth (get-depth node))
	(minimum 1000))
    (if (or (terminal-test? brd)
            (= depth 0))
        (utility brd)
        (let loop ([children (get-children-min node)])
          (if (not (null? children))
	      (begin
		(let ((value (max-value (car children))))
		(if (< value minimum) (set! minimum value)))
		(loop (cdr children))))
	  minimum))))

;; extracts the operator out of a given node
(define node-operator
  (lambda (node)
    (caddr node)))

;; used to find the operator (aka the direction that a bot has moved to from one board to another)
(define get-operator
  (lambda (b1 b2)
    (cond  [(eq? b1 b2)
	   'NULL]
	   [(equal? (my-pos b1) (my-pos b2))
	    (moved-to (op-pos b1) (op-pos b2))]
	   [(equal? (op-pos b1) (op-pos b2))
	    (moved-to (my-pos b1) (my-pos b2))])))

;; given 'node', this function extracts the board out of it and prints it.
(define print-brd
  (lambda (node)
    (let ((brd (node-to-board node)))
      (print-board (get-size brd) (get-walls brd) (my-pos brd) (op-pos brd)))))

;; get depth from node
(define get-depth
  (lambda (node) 
    (cadddr node)))

;; get cost fro node. (i.e. how much did it cost to reach this node so far)
(define get-cost
  (lambda (node)
    (car (reverse node))))

;; given a node, this function returns the board inside it.
(define node-to-board
  (lambda (node)
    (car node)))

;; checks if is a board has a terminal-scenario (i.e. of the bots has crashed, or both crashed).
(define (terminal-test? brd)
  (let ((walls (cadr brd))
        (ppos (caddr brd))
        (opos (cadddr brd)))
    (cond [(member ppos walls)
           #t]
          [(member opos walls)
           #t]
          [(eq? ppos opos)
           #t]
          (else #f))))

;; given size, walls, play position and opponent position, this function makes a board
(define (make-board size walls ppos opos)
  (list size walls ppos opos))

;; given a board, a parent node, an operator, depth, and path cost, this function creates a node.
(define make-node
  (lambda (brd parent operator depth path-cost)
    (list brd parent operator depth path-cost)))

;; gets the children of a given node. get-children-max & get-children-min
;; do the same thing, except that the move is for either my bot or the opponent's bot.
(define (get-children-max node)
  (let* ((parentBrd  (node-to-board node))
	 (children (map (lambda (pos) (make-board (get-size parentBrd) (cons (my-pos parentBrd) (get-walls parentBrd)) pos (op-pos parentBrd)))
			(get-moves (my-pos parentBrd) parentBrd))))
    (map (lambda (child) (make-node child node (get-operator parentBrd child) (sub1 (get-depth node)) (+ (get-cost node) (utility child)))) children)))

;; same as above, except it's the opponent's turn.
(define (get-children-min node)
  (let* ((parentBrd  (node-to-board node))
	 (children (map (lambda (pos) (make-board (get-size parentBrd) (cons (op-pos parentBrd) (get-walls parentBrd)) (my-pos parentBrd) pos))
			(get-moves (op-pos parentBrd) parentBrd))))
    (map (lambda (child) (make-node child node (get-operator parentBrd child) (sub1 (get-depth node)) (+ (get-cost node) (utility child)))) children)))
 
;;given a board, this function returns my position
(define (my-pos brd)
  (caddr brd))

;; given a board, this function returns the opponent's position
(define (op-pos brd)
  (cadddr brd))


;; given a board and a bot's position, it returns a list of coordinates
;; of all surronding grids.
(define (get-moves pos brd)
  (map (lambda (dir) (get-pos dir pos (get-size brd)))
       '(s e n w)))
   
;; my utility function, returns 100 if i win (opponent crashed), -100 if op won
;; and zero if draw. if none of the above cases, i calculate the move that brings
;; me closest to the opponents to attempt a block.              
(define (utility brd)
  (cond [(did-i-win brd)
         100]
         [(did-o-win brd)
         -100]
	 [(equal? (my-pos brd) (op-pos brd))
	  0]
         (else (- 100 (dist (my-pos brd) (op-pos brd))))))
	 
;; given a board, returns true of a i win, false otherwise.
(define (did-i-win brd)
  (if (member (cadddr brd) (cadr brd))
      #t
      #f))

;; given a board, returns true of opponent wins, false otherwise.
(define (did-o-win brd)
  (if (member (caddr brd) (cadr brd))
      #t
      #f))

;; get size of a board
(define (get-size brd)
  (car brd))

;; get walls of a board
(define (get-walls brd)
  (cadr brd))

;; given a point on the maze and a direction, this function returns the coordinates
;; of the new location.
(define move
  (lambda (pt dir)
    (cond [(eq? dir 'n)
	   (cons (car pt) (- (cdr pt) 1))]
	  [(eq? dir 's)
	   (cons (car pt) (+ (cdr pt) 1))]
	  [(eq? dir 'e)
	   (cons (+ (car pt) 1) (cdr pt))]
	  [(eq? dir 'w)
	   (cons (- (car pt) 1) (cdr pt))])))

;; given two positions, this function returns the direction a bot has moved to.
(define moved-to
  (lambda (pos1 pos2)
    (cond [(= (car pos1) (add1 (car pos2)))
	   'w]
	  [(= (car pos1) (sub1 (car pos2)))
	   'e]
	  [(= (cdr pos1) (add1 (cdr pos2)))
	   'n]
	  [(= (cdr pos1) (sub1 (cdr pos2)))
	   's])))

;; given two points, this function calculates the distance between them.
(define (dist p1 p2)
  (let ((x1 (car p1))
	(y1 (cdr p1))
	(x2 (car p2))
	(y2 (cdr p2)))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

;; square of a number
(define square
  (lambda (x)
    (* x x)))

;; checks if a list is empty
(define (empty? ls)
    (if (= 0 (length ls))
           #t
           #f))

;; A few functions that a started implementing  my solution with, but turned out
;;I eventually didn't need to use them. However, left here because a good amount
;; of time was spend writing them :)

;; (define (safe-moves brd directions safe)
;;   (let ((walls (cadr brd))
;;         (ppos  (caddr brd))
;;         (opos (cadddr brd)))
;;   (cond [(empty? directions)
;; 	 safe]
;; 	  [(not (member (move ppos (car directions)) (cons opos walls)))
;; 	   (safe-moves ppos opos walls (cdr directions) (cons (car directions) safe))]
;; 	  (else  (safe-moves ppos opos walls (cdr directions) safe)))))

;; (define (cbd ppos opos safe directions new) ;closest by distance
;;   (cond [(empty? directions)
;; 	 (min-in-list new '(n . 100))]
;; 	[(member (car directions) safe)
;; 	 (cbd ppos opos safe (cdr directions) (cons (cons (car directions) (dist (move ppos (car directions)) opos)) new))]
;; 	(else
;; 	 (cbd ppos opos safe (cdr directions) new))))

;; (define (free-space ppos walls directions total)
;;     (cond [(empty? directions)
;; 	   total]
;; 	  [(not (member (move ppos (car directions))  walls))
;; 	   (free-space (move ppos (car directions)) (cons (move ppos (car directions)) walls) directions (+ total 1))]
;; 	  (else
;; 	   (free-space ppos walls (cdr directions) total))))

;; (define (min-in-list ls lowest)
;;   (cond [(empty? ls)
;;       (car lowest)]
;; 	[(= (min (cdar ls) (cdr lowest)) (cdar ls))
;; 	 (min-in-list  (cdr ls) (car ls))]
;; 	(else (min-in-list (cdr ls) lowest))))   
   
;; here i define the size, walls, opos, ppos, board and a node.

(define size '(10 . 10))
(define walls (make-outer-walls size))
(define opos '(8 . 8))
(define ppos '(1 . 1))
(define b (make-board size walls ppos opos))
(define n (make-node b 'NULL 'NULL max-depth 0))
