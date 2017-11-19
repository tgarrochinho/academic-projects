;;
;; IA Proj 2
;;


;;
;;tipo posicao
;;

;construtor
(defun faz-posicao (l c)
  (cons l c))

;selectores
(defun posicao-linha (pos)
  (car pos))
(defun posicao-coluna (pos)
  (cdr pos))

;teste
(defun posicao= (pos1 pos2)
  (equal pos1 pos2))


;;
;; tipo obstaculo
;;
(defun faz-obstaculo (pos1 pos2)
  (cons pos1 pos2))

;;
;; Selectores
;;
(defun obstaculo-pos1 (obstaculo)
  (car obstaculo))

(defun obstaculo-pos2 (obstaculo)
  (cdr obstaculo))

;;
;; teste
;;

(defun obstaculos= (obs1 obs2)
  (or (and (posicao= (car obs1) (car obs2))
           (posicao= (cdr obs1) (cdr obs2)))
      (and (posicao= (car obs1) (cdr obs2))
           (posicao= (cdr obs1) (car obs2)))))

;;
;;tipo quadriculado
;;

;construtor
(defun faz-quadriculado (N obstaculos)
  (cons N obstaculos))

;selectores
(defun quadriculado-obstaculos (quadriculado)
  (cdr quadriculado))

(defun quadriculado-N (quadriculado)
  (car quadriculado))

;teste
(defun quadriculado-obstaculo? (quadriculado pos1 pos2)
  "Retorna T se na liga��o entre pos1 e pos2 existir um obst�culo"
  (let ((obstaculo-temp (faz-obstaculo pos1 pos2))
        (aux nil))
    (dolist (quad-obstaculo (quadriculado-obstaculos quadriculado))
      (setf aux  (or aux (obstaculos= quad-obstaculo obstaculo-temp))))
    aux)
)

;;
;;tipo estado
;;

;construtor
(defun faz-estado (As Ds quadriculado &optional (jogador 'D))
  (list jogador As Ds quadriculado))

;selectores
(defun estado-As (estado)
  (second estado))

(defun estado-Ds (estado)
  (third estado))

(defun estado-quadriculado (estado)
  (fourth estado))

(defun estado-jogador (estado)
  (first estado))

;opera��es de alto n�vel: estado-proximo-jogador estado-sucessores estado-numero-jogadas
(defun estado-proximo-jogador (estado)
  "Retorna o pr�ximo jogador a jogar"
  (if (eq (estado-jogador estado) 'A)
      'D
    'A)
)

(defun verifica-posicao-ocupado-obstaculo-e-limite (estado pos1 pos2)
  ;Verifica se pos2 nao esta ocupado, e se de pos1 para pos2 n�o � um obstaculo
  ;Verifica tambem se a posicao de movimento esta ou nao dentro dos limites do quadriculado
  ;Retorna t se alguma das verificacoes se verificar, nil caso contrario.
  (let ((res nil))
    (setf res
      (or
       (dolist (atacante (estado-As estado))
         (when (posicao= atacante pos2)
           (return t)))
       (dolist (defensor (estado-Ds estado))
         (when (posicao= defensor pos2)
           (return t)))
       (quadriculado-obstaculo? (estado-quadriculado estado) pos1 pos2)
      (if (or (> (posicao-linha pos2) (quadriculado-N (estado-quadriculado estado)))
              (> (posicao-coluna pos2) (quadriculado-N (estado-quadriculado estado)))
              (< (posicao-linha pos2) 0)
              (< (posicao-coluna pos2) 0))
          t
        nil)))
    res))

(defun estado-sucessores (estado)
  ;retorna a lista de estados sucessores
  (if (eq (estado-jogador estado) 'A)
      (estado-sucessores-As estado)
    (estado-sucessores-Ds estado))
)

(defun estado-sucessores-As (estado)
  (let ((atacantes (estado-As estado))
        (atacantes-aux (estado-As estado))
        (atacantes-acum ())
        (res ()))
    (dolist (atacante atacantes)
      (setf atacantes-aux (rest atacantes-aux))
      ; Verificar se posicao direita esta livre, se nao � obstaculo, nem fora dos limites
      (if (eq (verifica-posicao-ocupado-obstaculo-e-limite estado atacante (faz-posicao
                                                                     (posicao-linha atacante)
                                                                     (1+ (posicao-coluna atacante))))
              nil)
          ; Faz novo estado, mudando posicao para a direita
          (setf res
            (append res
                    (list (faz-estado (append
                                       (append atacantes-acum
                                               (list (faz-posicao
                                                      (posicao-linha atacante)
                                                      (1+ (posicao-coluna atacante)))))
                                       atacantes-aux)
                                      (estado-Ds estado)
                                      (estado-quadriculado estado)
                                      (estado-proximo-jogador estado))))))
      ; Verificar se posicao baixo esta livre, se nao � obstaculo, nem fora dos limites
      (if (eq (verifica-posicao-ocupado-obstaculo-e-limite estado atacante (faz-posicao
                                                                     (1+ (posicao-linha atacante))
                                                                     (posicao-coluna atacante)))
              nil)
          ; Faz novo estado, mudando posicao para a baixo
          (setf res
            (append res
                    (list (faz-estado (append
                                       (append atacantes-acum
                                               (list (faz-posicao
                                                      (1+ (posicao-linha atacante))
                                                      (posicao-coluna atacante))))
                                       atacantes-aux)
                                      (estado-Ds estado)
                                      (estado-quadriculado estado)
                                      (estado-proximo-jogador estado))))))
      (setf atacantes-acum (append atacantes-acum (list atacante))))
    res)
)

(defun estado-sucessores-Ds (estado)
  (let ((defensores (estado-Ds estado))
        (defensores-aux (estado-Ds estado))
        (defensores-acum ())
        (res ()))
    (dolist (defensor defensores)
      (setf defensores-aux (rest defensores-aux))
      ; Verificar se posicao esquerda esta livre, se nao � obstaculo, nem fora dos limites
      (if (eq (verifica-posicao-ocupado-obstaculo-e-limite estado defensor (faz-posicao
                                                                     (posicao-linha defensor)
                                                                     (1- (posicao-coluna defensor))))
              nil)
          ; Faz novo estado, mudando posicao para a esquerda
          (setf res
            (append res
                    (list (faz-estado (estado-As estado)
                                      (append
                                       (append defensores-acum
                                               (list (faz-posicao
                                                      (posicao-linha defensor)
                                                      (1- (posicao-coluna defensor)))))
                                       defensores-aux)
                                      (estado-quadriculado estado)
                                      (estado-proximo-jogador estado))))))
      ; Verificar se posicao cima esta livre, se nao � obstaculo, nem fora dos limites
      (if (eq (verifica-posicao-ocupado-obstaculo-e-limite estado defensor (faz-posicao
                                                                     (1- (posicao-linha defensor))
                                                                     (posicao-coluna defensor)))
              nil)
      ; Faz novo estado, mudando posicao para a cima
      (setf res
        (append res
                (list (faz-estado (estado-As estado)
                                  (append
                                   (append defensores-acum
                                           (list (faz-posicao
                                                  (1- (posicao-linha defensor))
                                                  (posicao-coluna defensor))))
                                   defensores-aux)
                                  (estado-quadriculado estado)
                                  (estado-proximo-jogador estado))))))
        (setf defensores-acum (append defensores-acum (list defensor))))
    res)
)

;estado-numero-jogadas
(defun estado-numero-jogadas (estado)
  "Retorna o n�mero de jogadas efectuadas pelo jogador atacante"
  (let ((res 0))
    (dolist (atacante (estado-As estado))
      (setf res (+ res
                   (+ (posicao-linha atacante)
                      (posicao-coluna atacante)))))
    res)
)

;;;
;;;Interface minimax.lisp
;;;

(defun game-successors (estado)
  ;recebe um estado e retorna uma lista de pares (<accao> . <estado>)
  ;conv�m que a ac��o seja o estado
  (let ((estado-sucrs (estado-sucessores estado))
        (res ()))
    (if (and (null estado-sucrs) (eq (estado-jogador estado) 'D))
        (let ((n-estado (faz-estado (estado-As estado)
                                    (estado-Ds estado)
                                    (estado-quadriculado estado)
                                    (estado-proximo-jogador estado))))
          (dolist (peca (estado-Ds estado))
            (setf res (append res
                              (list (cons n-estado n-estado))))))

      (dolist (estado-sucessor estado-sucrs)
        (setf res (append res (list (cons estado-sucessor estado-sucessor))))))
      res)
)

(defun terminal-values (estado)
  "Retorna a lista com os valores terminais do estado"
  (let ((valor (- (* 2
                     (* (quadriculado-N (estado-quadriculado estado))
                        (quadriculado-N (estado-quadriculado estado))))
                  (estado-numero-jogadas estado))))
    (list (- 0 valor) valor))
)

(defun game-over? (estado)
  ;identifica situa��es de fim de jogo
  ;1� situacao, quando o atacante chega � toca do defensor
  (or (dolist (atacante (estado-As estado))
        (when (and (eq (posicao-linha atacante) (quadriculado-N (estado-quadriculado estado)))
                   (eq (posicao-coluna atacante) (quadriculado-N (estado-quadriculado estado)))
                   (return t)
                   ))
        )
  ;2� situacao, quando o defensor bloqueia todos os atacantes
      (if (and (eq (estado-jogador estado) 'A) (null (estado-sucessores estado)))
          t))
)

(defun eval-fn-aleatorio (estado)
  ;fun��o de avalia��o heur�stica do estado
  (let ((valor (random (* 2
                          (* (quadriculado-N (estado-quadriculado estado))
                             (quadriculado-N (estado-quadriculado estado)))))))
    ;(if (eq (estado-jogador estado) 'A)
        (list valor (- 0 valor)))
      ;(list (- 0 valor) valor)))
)

(defun eval-fn (estado)
  ;fun��o de avalia��o heur�stica do estado
  (let ((movimentos (conta-movimentos-possiveis estado)))
    (if (eq (estado-jogador estado) 'A)
        (list movimentos (- movimentos))
      (list (- movimentos) movimentos)))
)

(defun conta-movimentos-possiveis (estado)
  (length (estado-sucessores (faz-estado (estado-As estado)
                                         (estado-Ds estado)
                                         (estado-quadriculado estado)
                                         (estado-proximo-jogador estado))))
)

;;;
;;; Interface jogador
;;;

(defun faz-jogador ()
  #'jogador-optimo)

(defun jogador-aleatorio (s)
  (minimax-cutoff-decision s
                           #'eval-fn-aleatorio
                           4))

(defun jogador-humano (s &aux original destino)
  (format t "Qual a posi��o que vai ser alterada? ")
  (setf original (read))
  (format t "Qual a posi��o destino? ")
  (setf destino (read))
  (if (eq (estado-proximo-jogador estado) 'A)
      (substitui-posicao-em-estado-A original estado destino)
    (substitui-posicao-em-estado-D original estado destino)))

(defun jogador-optimo (s)
  (if (eq (estado-jogador s) 'D)
      (minimax-cutoff-decision (faz-estado (estado-As s) (estado-Ds s) (estado-quadriculado s) 'A)
                           #'eval-fn
                           0)
    (minimax-cutoff-decision s
                             #'eval-fn
                             0)))

(defun jogo (jogador1 jogador2 quadriculado)
  (let* ((N (quadriculado-N quadriculado))
	 (estado (faz-estado (make-list N :initial-element (faz-posicao 0 0))
			     (make-list N :initial-element (faz-posicao N N))
			     quadriculado)))
    (format t "Jogo das suricatras~%" n)
    (escreve-estado estado)
    (loop
     (cond ((game-over? estado)
	    (format t "Jogador 1: Perdeu~%")
	    (return))
	   (t (setf estado (funcall jogador1 estado))
	      (escreve-estado estado)))
     (cond ((game-over? estado)
	    (format t "Jogador 2: Perdeu~%")
	    (return))
	   (t (setf estado (funcall jogador2 estado))
	      (escreve-estado estado))))))

(defun escreve-estado (estado)
  (let ((N (quadriculado-N (estado-quadriculado estado))))
    (format t "~%~%")
    (format t "  ")
    (dotimes (c N)
      (format t "~a    " c))
    (format t "~a~%" N)
    (dotimes (l N)
      (format t "~a " l)
      (dotimes (c N)
	(escreve-conteudo l c estado)
	(escreve-X l c l (+ c 1) estado))
      (escreve-conteudo l N estado)
      (terpri)
      (format t "  ")
      (dotimes (c N)
	(escreve-X2 l c (+ l 1) c estado))
      (escreve-X2 l N (+ l 1) N estado)
      (format t "~%"))
    (format t "~a " N)
    (dotimes (c N)
      (escreve-conteudo N c estado)
      (escreve-X N c N (+ c 1) estado))
    (escreve-conteudo N N estado)
    (terpri)
    ))

(defun escreve-conteudo (l c estado)
  (let ((As (count (cons l c) (estado-As estado) :test #'posicao=)))
    (if (> As 0)
	(format t "~a0" As)
      (let ((Ds (count (cons l c) (estado-Ds estado) :test #'posicao=)))
	(if (> Ds 0)
	    (format t "0~a" Ds)
	  (format t "  "))))))

(defun escreve-X (l1 c1 l2 c2 estado)
  (if (member (faz-obstaculo (faz-posicao l1 c1) (faz-posicao l2 c2))
	       (quadriculado-obstaculos (estado-quadriculado estado))
	      :test #'obstaculos=)
      (format t "-X-")
    (format t "---")))

(defun escreve-X2 (l1 c1 l2 c2 estado)
  (if (member  (faz-obstaculo (faz-posicao l1 c1) (faz-posicao l2 c2))
	       (quadriculado-obstaculos (estado-quadriculado estado))
	       :test #'obstaculos=)
      (format t "X    ")
    (format t "|    ")))

;;; The minimax decision procedure returns the optimal move in the game
;;; using exhaustive generation of the entire game tree.  Implementation
;;; uses the fact that the evaluation and utility functions return a list of
;;; values from the point of view of each player, with the "current" player
;;; first. Hence, rather than using #'min, we always use #'max for the
;;; current player.  A successor value is passed up the tree using
;;; right-rotation.  This works for any number of players.

;;; The notation "a+s" means an (action . state) pair.

;;;; Minimax with Cutoff

(defun minimax-cutoff-decision (state eval-fn limit)
  "Return the best action, according to backed-up evaluation down to LIMIT.
  After we search LIMIT levels seep, we use EVAL-FN to provide an estimate
  of the true value of a state; thus the action may not actually be best."
  (car (the-biggest
        #'(lambda (a+s)
            (first (right-rotate
                    (minimax-cutoff-value (cdr a+s) eval-fn (- limit 1)))))
        (game-successors state))))

(defun minimax-cutoff-value (state eval-fn limit)
  (cond ((game-over? state) (terminal-values state))
	((>= 0 limit) (funcall eval-fn state))
	(t (right-rotate
	    (the-biggest
	     #'(lambda (values) (first (right-rotate values)))
	     (mapcar #'(lambda (a+s)
			 (minimax-cutoff-value (cdr a+s) eval-fn
					       (- limit 1)))
		     (game-successors state)))))))

;;;
;;; De utilities.lisp
;;;

(defun right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun the-biggest (fn l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (let ((val (funcall fn x)))
	(when (> val best-val)
	  (setq best-val val)
	  (setq biggest x))))
    biggest))
