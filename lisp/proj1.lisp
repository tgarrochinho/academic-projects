;;
;; IA Proj 1
;;

;; Estrada

(defun faz-estrada (extremo1 extremo2 tempo nome)
  "Cria uma estrada"
  (list extremo1 extremo2 tempo nome))

(defun estrada-extremos (estrada)
  "Retorna lista com os extremos de uma estrada"
  (list (first estrada) (second estrada)))

(defun estrada-tempo (estrada)
  "Retorna o comprimento da estrada"
  (third estrada))

(defun estrada-nome (estrada)
  "Retrona o nome da estrada"
  (fourth estrada))

(defun estrada-p (obj)
  "Reconhecedor do tipo estrada"
  (and (listp obj)
       (= (length obj) 4)
       (symbolp (first obj))
       (symbolp (second obj))
       (numberp (third obj))
       (symbolp (fourth obj))))

(defun estrada-igual-p (estrada1 estrada2)
  "Testa duas estradas"
  (if (or (null estrada1) (null estrada2))
      nil
    (and
     ; extremos iguais
     (or (and (eq (first estrada1) (first estrada2))
              (eq (second estrada1) (second estrada2)))
         (and (eq (first estrada1) (second estrada2))
              (eq (second estrada1) (first estrada2))))
     ; comprimentos iguais
     (= (third estrada1) (third estrada2))
     (eq (fourth estrada1) (fourth estrada2)))))
;;

;; Coordenadas

(defun faz-coordenada (local x y)
  (list local x y))

(defun coordenada-local (coordenada)
  (first coordenada))

(defun coordenada-x (coordenada)
  (second coordenada))

(defun coordenada-y (coordenada)
  (third coordenada))
;;

;; Caminho

;
; Construtor

(defun faz-caminho (&rest l)
  "Constroi caminhos a partir das cidades recebidas como argumentos"
  l)

;
; Selectores

(defun caminho-primeira-cidade (caminho)
  "Retorna a primeira cidade do caminho"
  (first caminho))

(defun caminho-restantes-cidades (caminho)
  "Retorna o caminho que vem a seguir a primeira cidade"
  (rest caminho))

;
; Reconhecedor

(defun caminho-vazio-p (obj)
  "Reconhece o caminho vazio"
  (null obj))

;
; Teste

(defun caminho-igual-p (caminho1 caminho2)
  "Verifica se dois caminhos sao iguais"
  (if (or (null caminho1) (null caminho2))
      (and (null caminho1) (null caminho2))
    (and (eq (first caminho1) (first caminho2))
	 (caminho-igual-p (rest caminho1) (rest caminho2)))))

;
; Transformador de saida

(defun escreve-caminho (caminho)
  "Transformador de saida do tipo caminho"
  (format t "(")
  (escreve-caminho-aux caminho))

(defun escreve-caminho-aux (caminho)
  (cond ((null caminho) (format t ")"))
	(t (format t "~a" (first caminho))
	   (unless (null (rest caminho)) (format t " "))
	   (escreve-caminho-aux (rest caminho)))))
;;

;; Mapa

;
; Constructores

(defun cria-mapa (c e)
  "Constroi mapas dado um conjunto de cidades e estradas"
  (cons c e))

;
; Selectores

(defun mapa-cidades (mapa)
  "Retorna o conjunto de cidades de mapa"
  (first mapa))

(defun mapa-estradas (mapa)
  "Retorna o conjunto de estradas de mapa"
  (rest mapa))

;
; Reconhecedores

(defun mapa-vazio-p (obj)
  "Reconhece o mapa vazio"
  (and (null (first obj)) (null (rest obj))))

;(defun mapa-p (obj)
;  "Reconhece o tipo mapa"
;  )

;
; Testes

;(defun mapa-igual-p (mapa1 mapa2)
;  "Testa se dois mapas sao iguais"
;  )


; MAPA USADO PARA O PROJECTO!
(defvar *mapa* (cria-mapa
                (list 'A1-A9 'A2-A6 'Abrantes 'Albergaria 'Alcobaca 'Alenquer 'Almada 'Almeirim 'Alverca 'Aveiro 'Batalha 'Cascais 'CasteloBranco 'Chamusca 'Coimbra 'Constancia 'Coruche 'Elvas 'Ericeira 'Estremoz 'Evora 'Fatima 'FigueiraFoz 'FozGiraldo 'Fundao 'Guarda 'IC1-IC8 'IC8-N110 'IP2-A23 'Infantado 'Leiria 'Lisboa 'Loures 'Mafra 'Mira 'Montemor 'Montijo 'Mora 'N118-N119 'Obidos 'Oeiras 'Oleiros 'Palmela 'PampilhosaSerra 'Peniche 'Pombal 'PonteSor 'Portalegre 'Porto 'Queluz 'RioMaior 'Santarem 'Serta 'Setubal 'Sintra 'Tomar 'TorresNovas 'TorresVedras 'VendaPinheiro 'VilaVelhaRodao 'Viseu)
                (list
                 ;A1
                 (faz-estrada 'Lisboa 'Alverca 10 'A1)
                 (faz-estrada 'Alverca 'Alenquer 15 'A1)
                 (faz-estrada 'Alenquer 'Santarem 15 'A1)
                 (faz-estrada 'Santarem 'TorresNovas 15 'A1)
                 (faz-estrada 'TorresNovas 'Fatima 10 'A1)
                 (faz-estrada 'Fatima 'Leiria 8 'A1)
                 (faz-estrada 'Leiria 'Pombal 20 'A1)
                 (faz-estrada 'Pombal 'Coimbra 20 'A1)
                 (faz-estrada 'Coimbra 'Albergaria 25 'A1)
                 (faz-estrada 'Albergaria 'Porto 30 'A1)
                 ;A2
                 (faz-estrada 'Lisboa 'Almada 10 'A2)
                 (faz-estrada 'Almada 'Palmela 15 'A2)
                 (faz-estrada 'Palmela 'A2-A6 15 'A2)
                 ;A5
                 (faz-estrada 'Lisboa 'Oeiras 5 'A5)
                 (faz-estrada 'Oeiras 'Cascais 8 'A5)
                 (faz-estrada 'Cascais 'Sintra 15 'IC30)
                 (faz-estrada 'Sintra 'Queluz 18 'IC19)
                 (faz-estrada 'Queluz 'Lisboa 7 'IC19)
                 ;A6
                 (faz-estrada 'A2-A6 'Montemor 20 'A6)
                 (faz-estrada 'Montemor 'Evora 20 'A6)
                 (faz-estrada 'Evora 'Estremoz 15 'A6)
                 (faz-estrada 'Estremoz 'Elvas 25 'A6)
                 ;A8
                 (faz-estrada 'Lisboa 'Loures 8 'A8)
                 (faz-estrada 'Loures 'VendaPinheiro 7 'A8)
                 (faz-estrada 'VendaPinheiro 'TorresVedras 13 'A8)
                 (faz-estrada 'TorresVedras 'Obidos 15 'A8)
                 (faz-estrada 'Obidos 'Alcobaca 20 'A8)
                 (faz-estrada 'Alcobaca 'Leiria 15 'A8)
                 ;A9
                 (faz-estrada 'Oeiras 'Queluz 3 'A9)
                 (faz-estrada 'Queluz 'Loures 9 'A9)
                 (faz-estrada 'Loures 'A1-A9 8 'A9)
                 ;A12
                 (faz-estrada 'Lisboa 'Montijo 7 'A12)
                 (faz-estrada 'Montijo 'Palmela 10 'A12)
                 (faz-estrada 'Palmela 'Setubal 4 'A12)
                 ;A13
                 (faz-estrada 'Santarem 'Infantado 20 'A13)
                 (faz-estrada 'Infantado 'A2-A6 15 'A13)
                 ;A15
                 (faz-estrada 'Peniche 'Obidos 10 'A15)
                 (faz-estrada 'Obidos 'RioMaior 10 'A15)
                 (faz-estrada 'RioMaior 'Santarem 15 'A15)
                 ;A21
                 (faz-estrada 'Ericeira 'Mafra 7 'A21)
                 (faz-estrada 'Mafra 'VendaPinheiro 7 'A21)
                 ;A23
                 (faz-estrada 'TorresNovas 'Constancia 10 'A23)
                 (faz-estrada 'Constancia 'Abrantes 10 'A23)
                 (faz-estrada 'Abrantes 'IP2-A23 15 'A23)
                 (faz-estrada 'IP2-A23 'VilaVelhaRodao 15 'A23)
                 (faz-estrada 'VilaVelhaRodao 'CasteloBranco 25 'A23)
                 (faz-estrada 'CasteloBranco 'Fundao 25 'A23)
                 (faz-estrada 'Fundao 'Guarda 25 'A23)
                 ;A25
                 (faz-estrada 'Guarda 'Viseu 35 'A25)
                 (faz-estrada 'Viseu 'Albergaria 35 'A25)
                 (faz-estrada 'Albergaria 'Aveiro 15 'A25)
                 ;IP2
                 (faz-estrada 'Estremoz 'Portalegre 40 'IP2)
                 (faz-estrada 'Portalegre 'IP2-A23 25 'IP2)
                 ;IP3
                 (faz-estrada 'FigueiraFoz 'Coimbra 20 'IP3)
                 (faz-estrada 'Coimbra 'Viseu 80 'IP3)
                 ;IC1
                 (faz-estrada 'Leiria 'IC1-IC8 10 'IC1)
                 (faz-estrada 'IC1-IC8 'FigueiraFoz 10 'IC1)
                 (faz-estrada 'FigueiraFoz 'Mira 9 'IC1)
                 (faz-estrada 'Mira 'Aveiro 11 'IC1)
                 ;IC8
                 (faz-estrada 'IC1-IC8 'Pombal 7 'IC8)
                 (faz-estrada 'Pombal 'Serta 50 'IC8)
                 (faz-estrada 'Serta 'VilaVelhaRodao 50 'IC8)
                 ;N1
                 (faz-estrada 'Leiria 'Batalha 10 'N1)
                 (faz-estrada 'Batalha 'Alcobaca 10 'N1)
                 (faz-estrada 'Alcobaca 'RioMaior 15 'N1)
                 (faz-estrada 'RioMaior 'Alenquer 25 'N1)
                 ;N2
                 (faz-estrada 'Serta 'Abrantes 20 'N2)
                 (faz-estrada 'Abrantes 'PonteSor 20 'N2)
                 (faz-estrada 'PonteSor 'Mora 20 'N2)
                 (faz-estrada 'Mora 'Montemor 20 'N2)
                 ;N9
                 (faz-estrada 'Queluz 'Mafra 12 'N9)
                 (faz-estrada 'Mafra 'TorresVedras 10 'N9)
                 (faz-estrada 'TorresVedras 'Alenquer 15 'N9)
                 ;N110
                 (faz-estrada 'Tomar 'IC8-N110 20 'N110)
                 (faz-estrada 'IC8-N110 'Coimbra 25 'N110)
                 ;N112
                 (faz-estrada 'CasteloBranco 'FozGiraldo 15 'N112)
                 (faz-estrada 'FozGiraldo 'PampilhosaSerra 12 'N112)
                 (faz-estrada 'PampilhosaSerra 'Coimbra 25 'N112)
                 ;N113
                 (faz-estrada 'Fatima 'Tomar 10 'N113)
                 (faz-estrada 'Tomar 'Constancia 7 'N113)
                 ;N114
                 (faz-estrada 'Santarem 'Almeirim 4 'N114)
                 (faz-estrada 'Almeirim 'Coruche 16 'N114)
                 (faz-estrada 'Coruche 'Montemor 20 'N114)
                 ;N118
                 (faz-estrada 'N118-N119 'Almeirim 30 'N118)
                 (faz-estrada 'Almeirim 'Chamusca 8 'N118)
                 (faz-estrada 'Chamusca 'Abrantes 20 'N118)
                 ;N119
                 (faz-estrada 'PonteSor 'Portalegre 40 'N119)
                 (faz-estrada 'Coruche 'Infantado 15 'N119)
                 (faz-estrada 'Infantado 'N118-N119 7 'N119)
                 (faz-estrada 'N118-N119 'Montijo 8 'N119)
                 ;N234
                 (faz-estrada 'Mira 'Coimbra 17 'N234)
                 ;N238
                 (faz-estrada 'Tomar 'Serta 20 'N238)
                 (faz-estrada 'Serta 'Oleiros 12 'N238)
                 (faz-estrada 'Oleiros 'FozGiraldo 11 'N238)
                 (faz-estrada 'FozGiraldo 'Fundao 22 'N238)
                 ;N247
                 (faz-estrada 'Sintra 'Ericeira 15 'N247)
                 (faz-estrada 'Ericeira 'TorresVedras 15 'N247)
                 ;N251
                 (faz-estrada 'Estremoz 'Mora 40 'N251)
                 (faz-estrada 'Mora 'Coruche 20 'N251)
                 (faz-estrada 'Coruche 'A2-A6 25 'N251)
                 ;N349N113
                 (faz-estrada 'Chamusca 'TorresNovas 7 'N349N113)
                 (faz-estrada 'TorresNovas 'Leiria 30 'N349N113)
                 ;N351
                 (faz-estrada 'VilaVelhaRodao 'Oleiros 20 'N351)
                 (faz-estrada 'Oleiros 'PampilhosaSerra 11 'N351)
                 ;N356
                 (faz-estrada 'Batalha 'Fatima 10 'N356))))

(defvar *coordl* (list
                  (faz-coordenada	'A1-A9 	38.893972 -9.049988)
                  (faz-coordenada	'A2-A6 	38.603993 -8.650360)
                  (faz-coordenada	'ABRANTES 	39.466667 -8.200000)
                  (faz-coordenada	'ALBERGARIA 40.700000 -8.483333)
                  (faz-coordenada	'ALCOBACA 	39.550000 -8.983333)
                  (faz-coordenada	'ALENQUER 	39.050000 -9.000000)
                  (faz-coordenada	'ALMADA 	38.683333 -9.150000)
                  (faz-coordenada	'ALMEIRIM 	39.200000 -8.633333)
                  (faz-coordenada	'ALVERCA 	38.900000 -9.033333)
                  (faz-coordenada	'AVEIRO 	40.633333 -8.650000)
                  (faz-coordenada	'BATALHA 	39.650000 -8.833333)
                  (faz-coordenada	'CASCAIS 	38.700000 -9.416667)
                  (faz-coordenada	'CASTELOBRANCO  39.816667 -7.500000)
                  (faz-coordenada	'CHAMUSCA 	39.350000 -8.483333)
                  (faz-coordenada	'COIMBRA 	40.200000 -8.416667)
                  (faz-coordenada	'CONSTANCIA 39.466667 -8.333333)
                  (faz-coordenada	'CORUCHE 	38.950000 -8.516667)
                  (faz-coordenada	'ELVAS 	38.883333 -7.166667)
                  (faz-coordenada	'ERICEIRA 	38.983333 -9.416667)
                  (faz-coordenada	'ESTREMOZ 	38.850000 -7.583333)
                  (faz-coordenada	'EVORA 	38.566667 -7.900000)
                  (faz-coordenada	'FATIMA 	39.616667 -8.650000)
                  (faz-coordenada	'FIGUEIRAFOZ  40.150000 -8.866667)
                  (faz-coordenada	'FOZGIRALDO	40.000000 -7.716667)
                  (faz-coordenada	'FUNDAO 	40.133333 -7.500000)
                  (faz-coordenada	'GUARDA 	40.550000 -7.250000)
                  (faz-coordenada	'IC1-IC8 	39.939488 -8.673363)
                  (faz-coordenada	'IC8-N110 	39.917636 -8.375702)
                  (faz-coordenada	'IP2-A23 	40.568067 -7.237244)
                  (faz-coordenada	'INFANTADO 	38.833333 -8.850000)
                  (faz-coordenada	'LEIRIA 	39.750000 -8.800000)
                  (faz-coordenada	'LISBOA 	38.716667 -9.133333)
                  (faz-coordenada	'LOURES 	38.833333 -9.166667)
                  (faz-coordenada	'MAFRA 	38.933333 -9.333333)
                  (faz-coordenada	'MIRA 	39.533333 -8.716667)
                  (faz-coordenada	'MONTEMOR 	38.650000 -8.216667)
                  (faz-coordenada	'MONTIJO 	38.700000 -8.966667)
                  (faz-coordenada	'MORA 	38.933333 -8.166667)
                  (faz-coordenada	'N118-N119 	38.782458 -8.882103)
                  (faz-coordenada	'OBIDOS 	39.366667 -9.150000)
                  (faz-coordenada	'OEIRAS 	38.700000 -9.308333)
                  (faz-coordenada	'OLEIROS 	39.916667 -7.916667)
                  (faz-coordenada	'PALMELA 	38.566667 -8.900000)
                  (faz-coordenada	'PAMPILHOSASERRA 	40.050000 -7.950000)
                  (faz-coordenada	'PENICHE 	39.350000 -9.383333)
                  (faz-coordenada	'POMBAL 	39.916667 -8.633333)
                  (faz-coordenada	'PONTESOR 	39.250000 -8.016667)
                  (faz-coordenada	'PORTALEGRE 39.283333 -7.433333)
                  (faz-coordenada	'PORTO 	41.150000 -8.616667)
                  (faz-coordenada	'QUELUZ 	38.750000 -9.250000)
                  (faz-coordenada	'RIOMAIOR 	39.333333 -8.933333)
                  (faz-coordenada	'SANTAREM 	39.233333 -8.683333)
                  (faz-coordenada	'SERTA 	39.800000 -8.100000)
                  (faz-coordenada	'SETUBAL 	38.533333 -8.900000)
                  (faz-coordenada	'SINTRA 	38.800000 -9.383333)
                  (faz-coordenada	'TOMAR 	39.600000 -8.416667)
                  (faz-coordenada	'TORRESNOVAS  39.483333 -8.533333)
                  (faz-coordenada	'TORRESVEDRAS  39.100000 -9.266667)
                  (faz-coordenada	'VENDAPINHEIRO  38.916667 -9.233333)
                  (faz-coordenada	'VILAVELHARODAO  39.666667 -7.700000)
                  (faz-coordenada	'VISEU      40.650000 -7.916667)))

(defvar *mapa-cidades* (mapa-cidades *mapa*))
(defvar *mapa-estradas* (mapa-estradas *mapa*))
;;

; Codigo para usar as procuras

;; Funcao que cria os problemas
;;
;; (cria-problema <estado inicial>
;;                  <lista de operadores> -> estado-gera-sucessores
;;                  :objectivo? <funcao que testa o estado final>
;;                  :estado= <funcao de igualdade>
;;                  :hash <funcao que da o valor de hashing para um estado>
;;                  :custo <funcao que da o custo de geracao de um estado>
;;                  :qualidade
;;              <funcao que da a qualidade obtida com a geracao de um estado>
;;                  :heuristica <funcao que avalia um estado>
;;        :espaco-em-arvore? <T ou NIL>)

; -<estado>-
; cidade - cidade actual
; tempo - tempo ate essa cidade
; conjunto - mantem-se lista de cidades do conjunto (serve para testar se � objectivo)
; cobj - coordenadas local objectivo (par de coordenadas)
; canterior - cidade visitida anteriormente
(defstruct estado cidade tempo conjunto cobj canterior)

;; fazendo um defstruct ficam, automaticamente definidas as funcoes:
;; -> make-estado
;; -> estado-cidade
;; -> estado-tempo
;; -> estado-conjunto
;; -> estado-cobj
;; -> estado-canterior
;; -> estado-p
;; -> copy-estado

(defun cria-estado (&optional (c nil) (te 0) (co ()) (cob ()) (can nil))
  "cria-estado: -> estado"
  (make-estado :cidade c
               :tempo te
               :conjunto co
               :cobj cob
               :canterior can))

;Estado inicial
(defun cria-estado-inicial (problema)
  "Recebe um problema, contendo as cidades por onde temos de passar, criar estado inicial, � definir a primeira cidade desse problema como a nossa cidade de partida, o tempo inicial ao que queremos, neste caso a 0, e o conjunto com o conjunto dado na entrada menos a primeira cidade que � a de partida"
  (cria-estado (first problema) 0 (rest problema) (procura-coordenada (first (last problema))) nil))

;Procura-coordenada
(defun procura-coordenada (local)
  "Retorna par de coordenadas do local"
  (dolist (coordl *coordl*)
    (when (equal local (coordenada-local coordl))
      (return (cons (coordenada-x coordl) (coordenada-y coordl))))))

;Estado gera sucessores
(defun estado-gera-sucessores (estado)
  "estado-gera-sucessores: estado -> (estado)"
  (estado-gera-sucessores-aux estado *mapa-estradas*))

(defun estado-gera-sucessores-aux (estado mapa-estradas)
  (cond ((null mapa-estradas) ())
        ((and (equal (first (first mapa-estradas)) (estado-cidade estado))
              (not (equal (second (first mapa-estradas)) (estado-canterior estado))))
         (cons (cria-estado (second (first mapa-estradas))
                            (+ (estado-tempo estado) (third (first mapa-estradas)))
                            (remove (second (first mapa-estradas)) (estado-conjunto estado))
                            (estado-cobj estado)
                            (estado-cidade estado))
               (estado-gera-sucessores-aux estado (rest mapa-estradas))))
        ((and (equal (second (first mapa-estradas)) (estado-cidade estado))
              (not (equal (first (first mapa-estradas)) (estado-canterior estado))))
         (cons (cria-estado (first (first mapa-estradas))
                            (+ (estado-tempo estado) (third (first mapa-estradas)))
                            (remove (first (first mapa-estradas)) (estado-conjunto estado))
                            (estado-cobj estado)
                            (estado-cidade estado))
               (estado-gera-sucessores-aux estado (rest mapa-estradas))))
        (t (estado-gera-sucessores-aux estado (rest mapa-estradas)))))

;Estado objectivo
(defun estado-objectivo (estado)
  "estado-objectivo: estado -> bool"
   (null (estado-conjunto estado)))

;Estados iguais
(defun estados-iguais (estado1 estado2)
  "estados-iguais: estado estado -> bool"
  (and
   (equal (estado-cidade estado1) (estado-cidade estado2))
   (= (estado-tempo estado1) (estado-tempo estado2))
   (equal (estado-conjunto estado1) (estado-conjunto estado2))))

;Custo
(defun custo (estado)
  (estado-tempo estado))

(defun heuristica (estado)
  (calcula-distancia-linha-recta (estado-cobj estado) (procura-coordenada (estado-cidade estado))))

(defun calcula-distancia-linha-recta (ponto1 ponto2)
  (let ((lat1rad (/ (* pi (car ponto1)) 180))
        (lat2rad (/ (* pi (car ponto2)) 180))
        (long1rad (/ (* pi (cdr ponto1)) 180))
        (long2rad (/ (* pi (cdr ponto2)) 180)))
        (* 6371 (acos (+ (* (sin lat1rad) (sin lat2rad)) (* (cos lat1rad) (cos lat2rad) (cos (- long2rad long1rad))))))))


(defun imprime-estado (estado)
  "imprime-estado: estado ->"
  (format t "Cidade: ~a " (estado-cidade estado))
  (format t "Tempo: ~a " (estado-tempo estado))
  (format t "Conjunto: ~a~&" (estado-conjunto estado)))

(defun imprime-lista-estados (lista-estados)
  (cond ((null lista-estados) t)
        (t (imprime-estado (first lista-estados))
           (imprime-lista-estados (rest lista-estados)))))

(defun imprime-solucao (caminho tipo-procura tempo nos-expandidos nos-gerados)
  "imprime-solucao: tipo-procura lista-estados int int int -> "
  (format t "~& ~&Tipo de procura: ~a~&" tipo-procura)
  (format t "~&- - - - - - - - - - - - - - - - - - - - -~&")
  (imprime-lista-estados caminho)
  (format t "~&- - - - - - - - - - - - - - - - - - - - -~& ~&")
  (format t "Tempo gasto na procura: ~a segundos~&"
    (/ tempo internal-time-units-per-second))
  (format t "Nos expandidos: ~a~&" nos-expandidos)
  (format t "Nos gerados: ~a~&" nos-gerados)
  (format t " ~&- - - - - - - - - - - - - - - - - - - - -~&"))

;Criar problema com heuristica
(defun cria-problema-mapa (problema)
  "cria-problema-mapa: problema-mapa -> problema-procura"
  (let ((estado-inicial (cria-estado-inicial problema)))
    (cria-problema estado-inicial
                   (list #'estado-gera-sucessores)
                   :objectivo? #'estado-objectivo
                   ;:custo #'custo
                   :custo #'(lambda (estado) 0)
                   :heuristica #'heuristica
                   ;:espaco-em-arvore? nil
                   )))

(defun gera-solucao-caminho-3cidades (conj1 conj2 conj3)
  "Recebe 3 conjuntos, e junta-os de forma continua"
  (append (retira-cidades conj1) (retira-cidades (rest conj2)) (retira-cidades (rest conj3))))

(defun retira-cidades (conj)
  "Retira as cidades do conj e mete-as numa lista"
  (if (null conj)
      ()
    (cons (estado-cidade (first conj)) (retira-cidades (rest conj)))))

(defun procura-caminho (problema)
  "Recebe problema -> Lista com solucao"
  (procura-mapa-3cidades problema))


(defun procura-mapa-3cidades (problema &optional (tipo-procura 'a*)
                  (profundidade-maxima 10))
  (let ((c1c2 (procura (cria-problema-mapa
                        (list (first problema) (second problema)))
                       tipo-procura
                       :profundidade-maxima profundidade-maxima))
        (c2c3 ())
        (c3c2 ())
        (c1c3 (procura (cria-problema-mapa
                        (list (first problema) (third problema)))
                       tipo-procura
                       :profundidade-maxima profundidade-maxima))
        (c2c1 ())
        (c3c1 ())
        (sol ()))

    (cond ((>= (estado-tempo (first (last (first c1c2))))
              (estado-tempo (first (last (first c1c3)))))
           ; Mais rapido ir para c1c3 do que c1c2
           ; vou ter de ir para de c1c3 para c3c2 e finalmente c2c1
           (setf c3c2 (procura (cria-problema-mapa
                                (list (third problema) (second problema)))
                               tipo-procura
                               :profundidade-maxima profundidade-maxima))
           (setf c2c1 (procura (cria-problema-mapa
                                (list (second problema) (first problema)))
                               tipo-procura
                               :profundidade-maxima profundidade-maxima))
           (setf sol (gera-solucao-caminho-3cidades (first c1c3) (first c3c2) (first c2c1))))
          ; Mais rapido ir para c1c2 do que c1c3
          ; vou ter de ir de c1c2 para c2c3 e finalmente c3c1
          ((< (estado-tempo (first (last (first c1c2))))
              (estado-tempo (first (last (first c1c3)))))
           (setf c2c3 (procura (cria-problema-mapa
                                (list (second problema) (third problema)))
                               tipo-procura
                               :profundidade-maxima profundidade-maxima))
           (setf c3c1 (procura (cria-problema-mapa
                                (list (third problema) (first problema)))
                               tipo-procura
                               :profundidade-maxima profundidade-maxima))
           (setf sol (gera-solucao-caminho-3cidades (first c1c2) (first c2c3) (first c3c1)))))

    sol))
