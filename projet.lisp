


(clear-all)

;;; Function to place suitcases in the trunk

(defun place-valises(n-times &optional (draw-valises nil))

   (setf moyenne 0)
   (dotimes (i n-times)
      (setf compteur 1)
      (setf not-win t)
      (setf res nil)
      (setf state nil)
      (setf *valises* (create-valises)); Create suitcases
      (while not-win ; call the model until it wins
         ;; Reset all suitcases to layer 1
         (loop for valise in *valises*
            do (setf (slot-value valise 'couche) 1))
         (let ((choix-model (show-model-valises *valises* res state)) ; Show suitcases to the model and record the key pressed by the model
               (valises-deplacees-dimensions nil)) ; Flag to know if we moved suitcases due to dimensions
            
            ;; Handle model actions according to number of suitcases
            (when (string-equal "1" choix-model) (progn
               (setf compteur (+ compteur 1)) ;; increment counter
               ;; Put the last suitcase in layer 2
               (if (> (length *valises*) 0)
                  (progn
                     (setf (slot-value (nth (- (length *valises*) 1) *valises*) 'couche) 2)
                     (setf state "weight-problem"))))) ; Put the last suitcase in layer 2
            (when (string-equal "2" choix-model) (progn
               (setf compteur (+ compteur 1))
               ;; Put the second-to-last suitcase in layer 2
               (if (> (length *valises*) 1)
                  (progn
                     (setf (slot-value (nth (- (length *valises*) 2) *valises*) 'couche) 2)
                     (setf state "weight-problem-2"))))) ; Put the second-to-last suitcase in layer 2
            
            ;; Check if all suitcases fit on level 1 (dimensions)
            (let ((largeur-totale-niveau1 0))
               (loop for valise in *valises*
                  do (if (= (slot-value valise 'couche) 1)
                        (setf largeur-totale-niveau1 (+ largeur-totale-niveau1 (slot-value valise 'x)))))
               ;; If total width exceeds 12, move suitcases to level 2
               (when (> largeur-totale-niveau1 12)
                  (setf compteur (+ compteur 1))
                  (setf valises-deplacees-dimensions t)
                  ;; Move additional suitcases to level 2 until width <= 12
                  (loop for valise in *valises*
                     do (when (and (= (slot-value valise 'couche) 1) (> largeur-totale-niveau1 12))
                          (setf (slot-value valise 'couche) 2)
                          (setf largeur-totale-niveau1 (- largeur-totale-niveau1 (slot-value valise 'x)))))))
            
            (setf res "win") ;; By default we win
            (setf poids-tot-couche-1 0)
            (setf poids-tot-couche-2 0)
            (loop for valise in *valises* ; loop over suitcases chosen by the model
               do (if (= (slot-value valise 'couche) 1)
                     ;; If suitcase is in layer 1
                     (setf poids-tot-couche-1 (+ poids-tot-couche-1 (slot-value valise 'poids))) ; Add suitcase weight
                     ;; If suitcase is in layer 2
                     (setf poids-tot-couche-2 (+ poids-tot-couche-2 (slot-value valise 'poids))))) ; Add suitcase weight
            ;(setf choix-model "0")
            (if (> poids-tot-couche-2 poids-tot-couche-1)
               (progn
                  (setf res "lose") ; If suitcases in layer 2 are heavier -> lose
                  ;; If we moved suitcases due to dimensions, we still accept the result
                  ;; because dimensions are a physical constraint we cannot ignore
                  (when valises-deplacees-dimensions
                     (setf not-win nil)
                     (setf state "final")
                     (show-model-result res state)))
               (progn (setf not-win nil)
                      (unless (string-equal choix-model "0")(progn 
                        (setf state "final")
                        (show-model-result res state))))))
            (when draw-valises
            ;; Display all suitcases
            (loop for valise in *valises*
               do (print-valise valise))
               ;; Check if all suitcases are on layer 1
               (if (loop for valise in *valises*
                      always (= (slot-value valise 'couche) 1))
                  (progn
                     (setf nb 0)
                     (setf grandevalise (car *valises*))
                     (loop for valise in *valises*
                        do (if (= (slot-value valise 'categorie) 1)
                           (setf nb (+ nb 1))
                           (setf grandevalise valise)))
                     (if (>= nb 2)
                        (progn 
                           (draw2little)
                           (draw-valise grandevalise)
                        )
                        (progn
                           (format t "Niveau 1:~%")
                           (loop for valise in *valises*
                              do (when (= (slot-value valise 'couche) 1)
                                 (progn (draw-valise valise) (format t "~%"))))
                           (format t "~%Niveau 2:~%")
                           (loop for valise in *valises*
                              do (when (= (slot-value valise 'couche) 2)
                                 (draw-valise valise)))
                        ))
                  )
                  (progn 
                     (format t "Niveau 1:~%")
                     (loop for valise in *valises*
                        do (when (= (slot-value valise 'couche) 1)
                           (progn (draw-valise valise) (format t "~%"))))
                     (format t "~%Niveau 2:~%")
                     (loop for valise in *valises*
                        do (when (= (slot-value valise 'couche) 2)
                           (draw-valise valise)))))))

   (setf moyenne (+ moyenne compteur)))
   ;; Normaliser par le nombre moyen de valises (4.5 = (3+4+5+6)/4)
   (/ (/ moyenne n-times) 4.5))

(defun show-model-valises(valises &optional res state)
   (let ((nb-valises (length valises))
         (chunk-spec-list '()))
      ;; Add categories (c1, c2, c3, c4, c5, c6)
      (loop for i from 1 to 6
         do (if (<= i nb-valises)
               (setf chunk-spec-list (append chunk-spec-list 
                    (list (intern (format nil "C~d" i)) (slot-value (nth (- i 1) valises) 'categorie))))
               (setf chunk-spec-list (append chunk-spec-list 
                    (list (intern (format nil "C~d" i)) nil)))))
      ;; Add weights (p1, p2, p3, p4, p5, p6)
      (loop for i from 1 to 6
         do (if (<= i nb-valises)
               (setf chunk-spec-list (append chunk-spec-list 
                    (list (intern (format nil "P~d" i)) (slot-value (nth (- i 1) valises) 'poids))))
               (setf chunk-spec-list (append chunk-spec-list 
                    (list (intern (format nil "P~d" i)) nil)))))
      ;; Calculate total weight and balance
      (let ((total-weight 0)
            (max-weight 0)
            (min-weight 10))
         (loop for valise in valises
            do (let ((poids (slot-value valise 'poids)))
                 (setf total-weight (+ total-weight poids))
                 (if (> poids max-weight) (setf max-weight poids))
                 (if (< poids min-weight) (setf min-weight poids))))
         (let ((weight-balance (if (> (- max-weight min-weight) 2) "unbalanced" "balanced")))
            ;; Add other slots
            (setf chunk-spec-list (append chunk-spec-list 
                 (list 'total-weight total-weight 
                       'weight-balance weight-balance
                       'result res 'state state 'first-c nil 'second-c nil)))))
      
      (if (buffer-read 'goal) ; if there is a chunk in the goal buffer
         (mod-focus-fct chunk-spec-list)
         (goal-focus-fct (car (define-chunks-fct (list (append `(isa arrange-state) chunk-spec-list)))))))
   
   (run-full-time 10) 
   *model-action*)

(defun show-model-result(res state)
   (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
      (mod-focus-fct `(result ,res
                           state ,state))
      (goal-focus-fct (car (define-chunks-fct ; creates a new chunk and puts it in the goal
                             `((isa arrange-state result ,res
                                 state ,state))))))
                                 (run-full-time 10))

(defun run-blocks (blocks block-size)     
   (dotimes (i blocks)
      (setf retour (place-valises block-size)))
      retour)

;; Function to configure ACT-R parameters easily
(defun config-actr-params (&key (v nil) (esc nil) (ans 0.1) (bll 0.5) (ncnar nil) (rt 0) (pas nil) (show-focus t) (trace-detail 'low))
   "Configure ACT-R parameters. Use (config-actr-params) to see current values."
   (sgp :v v :esc esc :ans ans :bll bll :ncnar ncnar :rt rt :pas pas :show-focus show-focus :trace-detail trace-detail)
   (format t "~%ACT-R parameters updated:~%")
   (format t "  :v ~a, :esc ~a, :ans ~a, :bll ~a, :ncnar ~a, :rt ~a, :pas ~a, :show-focus ~a, :trace-detail ~a~%~%" 
           v esc ans bll ncnar rt pas show-focus trace-detail))

(defun show-learning (n &optional (graph t) (block-size 1000))
   (let ((points))
      (format t "~%=== Learning Graph Generation ===~%")
      (format t "Number of blocks: ~d~%" n)
      (format t "Experiences per block: ~d~%" block-size)
      (format t "Total experiences: ~d~%~%" (* n block-size))
      (dotimes (i n)
         (format t "Calculating block ~d/~d...~%" (+ i 1) n)
         (push (run-blocks 1 block-size) points)
         (format t "Block ~d/~d completed.~%~%" (+ i 1) n))
      (format t "=== Calculation completed ===~%")
      (format t "Generating graph...~%")
      (setf points (rev points))
      (when graph
         (draw-graph points))
      (format t "Graph displayed.~%")
      points))


(defun rev(l)
           (cond
             ((null l) '())
             (T (append (rev (cdr l)) (list (car l)))))) 

(defun draw-graph (points)
  (let ((w (open-exp-window "Data" :width 550 :height 460 :visible t)))
    (allow-event-manager w)
    (add-line-to-exp-window '(50 0) '(50 420) :color 'white :window "Data")
    (dotimes (i 11)
      (add-text-to-exp-window :x 5 :y (+ 5 (* i 40)) :width 35 :text (format nil "~3,1f" (* (- 1 (* i .1)) 3)) :window "Data")
      (add-line-to-exp-window (list 45 (+ 10 (* i 40))) (list 550 (+ 10 (* i 40))) :color 'white :window "Data"))
    
    (let ((x 50))
      (mapcar (lambda (a b) (add-line-to-exp-window (list x (floor (- 410 (* a 400))))
                                                  (list (incf x 25) (floor (- 410 (* b 400))))
                                                    :color 'blue :window "Data"))
        (butlast points) (cdr points)))
    (allow-event-manager w)))


(defvar *model-action* nil) ; La variable que le model devra remplir (liste de valise)

;; Variables globales pour place-valises
(defvar *valises* nil)
(defvar compteur 0)
(defvar grandevalise nil)
(defvar moyenne 0)
(defvar nb 0)
(defvar not-win t)
(defvar poids-tot-couche-1 0)
(defvar poids-tot-couche-2 0)
(defvar res nil)
(defvar state nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (if (eq win (current-device))
      (setf *model-action* (string key))
    (unless *human-action*
      (setf *human-action* (string key)))))

;;; Classe valise
(defclass valise()
   (poids
   categorie
   couche
   x
   y))

;;; permet d'afficher les infos d'une valise
(defgeneric print-valise (valise))
(defmethod print-valise ((la-valise valise))
   (format t "La valise pese: ~d, est de categorie ~d, mesure ~dx~d et est positionnee a la couche ~d~%" (slot-value la-valise 'poids) (slot-value la-valise 'categorie) (slot-value la-valise 'x) (slot-value la-valise 'y) (slot-value la-valise 'couche)))

;;; permet de dessiner une valise
(defgeneric draw-valise (valise))
(defmethod draw-valise ((la-valise valise))
   (format t " ")
   (dotimes (i (slot-value la-valise 'x))
      (format t "__")
   )
   (format t "~%")
   (dotimes (i (slot-value la-valise 'y))
      (format t "|")
      
      (dotimes (i (slot-value la-valise 'x))
         (format t "__")
      )
      (format t "|~%")
   )
)
(defun draw2little()
   (format t "Niveau 1: ~%")
   (format t " ______  ______~%")
   (format t "|______||______|~%")
   (format t "|______||______|~%")
   (format t "|______||______|~%")
)

(defun create-valises()
   ;; Creation of a variable number of suitcases (between 3 and 6)
   ;; random n gives between 0 and n-1, so random 4 gives 0,1,2,3
   ;; We want 3,4,5,6 so we do (+ 3 (random 4))
   (let ((nb-valises (+ 3 (random 4))) ; Random number between 3 and 6 (3,4,5,6)
         (valise-list '())) ; Empty list to start
      
      ;; Create the requested number of suitcases
      (dotimes (i nb-valises)
         (let ((nouvelle-valise (make-instance 'valise)))
            (setf (slot-value nouvelle-valise 'poids) (1+ (act-r-random 5))) ; random weight
            (setf (slot-value nouvelle-valise 'categorie) (1+ (act-r-random 3))) ; random category
            (setf (slot-value nouvelle-valise 'couche) 1) ; initial layer
            ;; Dimensions according to category
            (case (slot-value nouvelle-valise 'categorie)
               (1 (progn (setf (slot-value nouvelle-valise 'x) 3) (setf (slot-value nouvelle-valise 'y) 3)))
               (2 (progn (setf (slot-value nouvelle-valise 'x) 6) (setf (slot-value nouvelle-valise 'y) 2)))
               (3 (progn (setf (slot-value nouvelle-valise 'x) 6) (setf (slot-value nouvelle-valise 'y) 3))))
            (setf valise-list (append valise-list (list nouvelle-valise)))))
      valise-list)) ; return valise-list
   


;;; ACT-R Model: 


(define-model baggage-organization

(sgp :v nil :esc nil :ans 0.1 :bll 0.5 :ncnar nil :rt 0 :pas nil :show-focus t :trace-detail low)

(install-device (open-exp-window "" :visible nil))

(chunk-type arrange-state c1 c2 c3 c4 c5 c6 p1 p2 p3 p4 p5 p6 total-weight weight-balance first-c second-c result state)
(chunk-type first1 v1 v2 v3 result-first1 all-fit-level1)
(chunk-type first1-weight v1 v2 v3 p1 p2 p3 result-first1 all-fit-level1 weight-strategy)
(chunk-type first2 v4 v5 result-first2)

(chunk-type learned-info c1 c2 c3 c4 c5 c6 p1 p2 p3 p4 p5 p6 first-c second-c result)
(declare-buffer-usage goal arrange-state :all)

(define-chunks 

    (begin-model isa chunk)
    (remembering isa chunk) 
    (finish isa chunk) 
    (retrieving isa chunk) 
    (retrieving_2layers isa chunk) 
    (retrieving_2layers_2 isa chunk)
    (retrieving_2layers_3 isa chunk) 
    (comparing_weight isa chunk) 
    (comparing2 isa chunk)    

)


(add-dm
   (a ISA first1 v1 3 v2 1 v3 1 result-first1 113 all-fit-level1 true)
   (b ISA first1 v1 2 v2 2 v3 2 result-first1 222 all-fit-level1 false)
   (c ISA first1 v1 1 v2 1 v3 1 result-first1 111 all-fit-level1 true)
   (d ISA first1 v1 1 v2 3 v3 1 result-first1 113 all-fit-level1 true)
   (e ISA first1 v1 1 v2 1 v3 3 result-first1 113 all-fit-level1 true)
   (f ISA first1 v1 1 v2 1 v3 2 result-first1 112 all-fit-level1 true)
   ;; Additional chunks for problematic configurations (width > 12)
   (q ISA first1 v1 3 v2 2 v3 3 result-first1 323 all-fit-level1 false)
   (r ISA first1 v1 2 v2 3 v3 3 result-first1 233 all-fit-level1 false)
   (s ISA first1 v1 3 v2 3 v3 3 result-first1 333 all-fit-level1 false)
   (t ISA first1 v1 1 v2 3 v3 3 result-first1 133 all-fit-level1 false)
   (u ISA first1 v1 1 v2 2 v3 2 result-first1 122 all-fit-level1 false)
   (v ISA first1 v1 2 v2 2 v3 3 result-first1 223 all-fit-level1 false)
   (w ISA first1 v1 2 v2 3 v3 2 result-first1 232 all-fit-level1 false)
   (x ISA first1 v1 3 v2 2 v3 2 result-first1 322 all-fit-level1 false)
   ;; Additional chunks for valid configurations (width = 12)
   (y ISA first1 v1 1 v2 2 v3 1 result-first1 121 all-fit-level1 true)
   (z ISA first1 v1 2 v2 1 v3 1 result-first1 211 all-fit-level1 true)
   (aa ISA first1 v1 3 v2 1 v3 2 result-first1 312 all-fit-level1 false)
   (ab ISA first1 v1 3 v2 1 v3 3 result-first1 313 all-fit-level1 false)
   (ac ISA first1 v1 1 v2 3 v3 2 result-first1 132 all-fit-level1 false)
   (ad ISA first1 v1 2 v2 1 v3 2 result-first1 212 all-fit-level1 false)
   (ae ISA first1 v1 2 v2 1 v3 3 result-first1 213 all-fit-level1 false)
   (af ISA first1 v1 2 v2 3 v3 1 result-first1 231 all-fit-level1 false)
   (ag ISA first1 v1 3 v2 2 v3 1 result-first1 321 all-fit-level1 false)
   (ah ISA first1 v1 3 v2 3 v3 1 result-first1 331 all-fit-level1 false)
   (ai ISA first1 v1 3 v2 3 v3 2 result-first1 332 all-fit-level1 false)
   ;; Chunks first1-weight for parallel reasoning weight/category
   ;; Use of weight ranges: 1-2 (low), 3-4 (medium), 5 (high)
   ;; Format: (name ISA first1-weight v1 cat1 v2 cat2 v3 cat3 p1 weight1 p2 weight2 p3 weight3 result-first1 val all-fit-level1 bool weight-strategy strat)
   ;; Examples for common cases with unbalanced weights
   (aw1 ISA first1-weight v1 1 v2 1 v3 1 p1 1 p2 1 p3 5 result-first1 111 all-fit-level1 true weight-strategy "2layers")
   (aw2 ISA first1-weight v1 1 v2 1 v3 1 p1 5 p2 1 p3 1 result-first1 111 all-fit-level1 true weight-strategy "2layers")
   (aw3 ISA first1-weight v1 2 v2 2 v3 2 p1 1 p2 1 p3 5 result-first1 222 all-fit-level1 false weight-strategy "2layers")
   (g ISA first2 v4 3 v5 3 result-first2 33)
   (h ISA first2 v4 3 v5 2 result-first2 23)
   (j ISA first2 v4 2 v5 3  result-first2 23)
   (k ISA first2 v4 3  v5 1 result-first2 13)
   (l ISA first2 v4 1  v5 3 result-first2 13)
   (m ISA first2 v4 2 v5 1  result-first2 12)
   (n ISA first2 v4 1 v5 2  result-first2 12)
   (o ISA first2 v4 2 v5 2 result-first2 22)
   (p ISA first2 v4 1 v5  1 result-first2 11)
)
(p start
   =goal>
        isa arrange-state
        state nil
        c1 =a
        c2  =b
        c3  =c
        p1  =j
        p2  =d
        p3  =e
   ==>
   +retrieval> 
        isa learned-info
        c1 =a
        c2  =b
        c3  =c
        p1  =j
        p2  =d
        p3  =e
      - first-c nil
      - second-c nil    
   =goal>
        state remembering
)
(p remember-organization
    =goal>
       isa arrange-state
       state remembering
    =retrieval>
       isa learned-info
       first-c =val1
       second-c =val2
    ==>
    =goal>
       state finish
       first-c =val1
       second-c =val2
       result "win"
)
(p doesnt-remember-organization
    =goal>
       isa arrange-state
       state remembering
    ?retrieval>
       buffer  failure
    ==>
     =goal>
        state begin-model
)
(p begin-weight-aware
   =goal>
      c1 =a
      c2 =b
      c3 =c
      p1 =p1
      p2 =p2
      p3 =p3
      state begin-model
   ==>
   +retrieval> 
      isa first1-weight
      v1 =a
      v2 =b
      v3 =c
      p1 =p1
      p2 =p2
      p3 =p3
   =goal>
      state retrieving-weight
)
(p begin-weight-fallback
   =goal>
      c1 =a
      c2 =b
      c3 =c
      p1 =p1
      p2 =p2
      p3 =p3
      state begin-model
   ?retrieval>
      buffer failure
   ==>
   +retrieval> 
      isa first1
      v1 =a
      v2 =b
      v3 =c
   =goal>
      state retrieving
)
(p begin-weight-fallback-retrieving
   =goal>
      c1 =a
      c2 =b
      c3 =c
      state retrieving-weight
   ?retrieval>
      buffer failure
   ==>
   +retrieval> 
      isa first1
      v1 =a
      v2 =b
      v3 =c
   =goal>
      state retrieving
)
(p begin
   =goal>
      c1 =a
      c2 =b
      c3 =c
      state begin-model
      p1 nil
   ==>
   +retrieval> 
      isa first1
      v1 =a
      v2 =b
      v3 =c
   =goal>
      state retrieving
)
(p success_3bags-weight-balanced
   =retrieval>
      result-first1  =value
      all-fit-level1 true
   =goal> 
      state retrieving
      weight-balance "balanced"
   ==>
   =goal>
      first-c   =value
      second-c "vide"
      state "final"
      result "win"        
)
(p success_3bags-weight-unbalanced
   =retrieval>
      result-first1  =value
      all-fit-level1 true
   =goal> 
      state retrieving
      weight-balance "unbalanced"
      c1 =a
      c2 =b
   ==>
   +retrieval>
      isa first2
      v4 =a
      v5 =b
   =goal>
      first-c   =value
      second-c =c
      state comparing_weight
   +manual>
      cmd press-key
      key "1"
)
(p success_3bags-fallback
   =retrieval>
      result-first1  =value
      all-fit-level1 true
   =goal> 
      state retrieving
   ==>
   =goal>
      first-c   =value
      second-c "vide"
      state "final"
      result "win"        
)
(p fail-3bags-dimensions
   =retrieval>
      isa first1
      all-fit-level1 false
   =goal>
      isa arrange-state 
      state retrieving
      c1  =a
      c2  =b
   ==>
   +retrieval>
      isa first2
      v4   =a
      v5   =b
   =goal>
      state retrieving_2layers
)
(p fail-3bags-1     
   ?retrieval>
      buffer  failure
   =goal>
      isa arrange-state 
      state retrieving
      c1  =a
      c2  =b
   ==>
   +retrieval>
      isa first2
      v4   =a
      v5   =b
   =goal>
      state retrieving_2layers
)
(p car-trunk
	=retrieval>
      result-first2 =p
   =goal>
      isa arrange-state
      state retrieving_2layers
      c3    =q
	?manual>
      state free
   ==>
   =goal>
      first-c   =p
      second-c  =q
      state comparing_weight
   +manual>
      cmd press-key
      key "1"
)
(p fail-3bag-2
   =goal>
      result "lose"
      state "weight-problem"
      c2  =a
      c3  =b
   ==>
   +retrieval>
      v4   =a
      v5   =b
   =goal>
      state retrieving_2Layers_2    
)
(p car-trunk-2
   =retrieval>
      result-first2 =val
   =goal>
      state retrieving_2Layers_2  
      c1 =v 
   ?manual>
      state free
   ==>
   =goal>
      first-c =val  
      second-c =v
      state comparing2
   +manual>
      cmd press-key
      key "2"
)
(p fail-3bag-3
   ?retrieval>
      buffer  failure
   =goal>
      isa arrange-state 
      state "weight-problem-2"
      c1  =a
      c3  =b
   ==>
   +retrieval>
      v4   =a
      v5   =b
   =goal>
      state retrieving_2layers_3   
)
(p car-trunk-3
   =retrieval>
      result-first2 =val
   =goal>
      state retrieving_2layers_3
      c2 =v 
   ==>
   =goal>
      first-c =val  
      second-c =v
      state "final"
      result "win"
)
(p memorize
    =goal>
        state "final"
        result "win"
        c1 =a
        c2 =b
        c3 =c
        p1 =l
        p2 =d
        p3 =e
        first-c =f
        second-c =g 
    ?imaginal>
        state free    
    ==>
    =goal>
        state finish
    +imaginal>
        c1 =a
        c2 =b
        c3 =c
        p1 =l
        p2 =d
        p3 =e
        first-c =f
        second-c  =g
)
(p show-organization
   =goal>
      state finish
      result "win"
      first-c =org1
      second-c =org2
   ?manual>
      state free
    ==>
   +manual>
      cmd press-key
      key "0"
   !output! =org1
   !output! =org2
   =goal>
      state finish
      result nil
)
(p clear-new-imaginal-chunk
    ?imaginal>
        state free
        buffer full
    ==>
    -imaginal>
)

)