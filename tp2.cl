

;; Liste des tous les coups possibles
(setq liste_coups '(
                   (1 1)
                   (1 2)
                   (1 3)
                   (2 1)
                   (2 2)
                   (2 3)
                   )
      )


;; Fonctions de service

(defun coups_possibles (etat liste_coups)
  (let (liste)
    (dolist (x liste_coups liste)
      (when (and (equal (car x) (car etat)) (>= (cadr etat) (cadr x)))
          (setq liste (append liste (list x)))
        )
      )
    )
  )

(defun suivant (etat choix)
  (let (suiv joueur)
    (if (= (car etat) 1) (setq joueur 2) (setq joueur 1))
    (setq suiv (list joueur (- (cadr etat) (cadr choix))))
    )
  )

(defun calc_nb_coups (actions joueur)
  (let ((n 0))
    (dolist (x actions n)
      (when (equal joueur (car x))
        (setq n (+ 1 n))
        )
      )
    )
  )

(defun afficher (chemin actions)
  (mapcar #'(lambda (c a) 
              (format t "Le joueur ~S a la main. Il reste ~S allumette(s). ~&" (car c) (cadr c))
              (format t "Le joueur ~S enlève ~S allumette(s). ~&" (car a) (cadr a))
              )
    chemin actions)
  (format t "Le joueur ~S gagne !!~%~%" (caar (last actions)))
  )



;; Parcours en profondeur

(defun explore_profondeur (etat liste_coups vainqueur chemin actions)
 (let ((res) (temp))
  (if (and (equal (car etat) vainqueur) (= (cadr etat) 0))
      nil
    (progn
       (setq chemin (append chemin (list etat)))
      (if (and (not (equal (car etat) vainqueur)) (= (cadr etat) 0)) 
            (list (list chemin actions)) ;;on pourrait aussi afficher les solutions directement ici
        (dolist (x (coups_possibles etat liste_coups) res)
          (setq temp (explore_profondeur (suivant etat x) liste_coups vainqueur chemin (append actions (list x))))
          (when temp
            (setq res (append res temp)
              )
            )
          )
        )
      )
    )
   )
  )

(defun afficher_solutions_profondeur (etat liste_coups vainqueur)
  (let ((i 1)(min)(optimal)(sol))
    (setq sol (explore_profondeur etat liste_coups vainqueur () ()))
    (dolist (x sol)
      (format t "~%Solution ~S en ~S coups : ~&" i (calc_nb_coups (cadr x) vainqueur))
      (afficher (car x) (cadr x))
      (setq i (+ i 1))
      )
    (setq min (length (cadar sol)))  
    (setq optimal (car sol))
    (dolist (x sol)
      (when (< (length (cadr x)) min)
        (setq min (length (cadr x)))
        (setq optimal x)
        )
      )
    (format t "~%~%Solution optimale : en ~S coups : ~&" (calc_nb_coups (cadr optimal) vainqueur))
    (afficher (car optimal) (cadr optimal))        
    )
  )



;; parcours en largeur 

(defun explore_largeur (etat liste_coups vainqueur)
  (let ((temp)(file)(file_chemins)(temp_chemins)(file_actions)(temp_actions)(i 1))
    (setq file (list etat))
    (setq file_chemins (list (list etat)))
    (while file
      (setq temp (pop file))
      (setq temp_chemins (pop file_chemins))
      (setq temp_actions (pop file_actions))
      (if (and (not (equal vainqueur (car temp))) (= 0 (cadr temp)))
          (progn
            (if (= i 1)
                (format t "~%Solution 1 (optimale) en ~S coups : ~&" (calc_nb_coups temp_actions vainqueur))
              (format t "~%Solution ~S en ~S coups : ~&" i (calc_nb_coups temp_actions vainqueur))
              )
            (afficher temp_chemins temp_actions)
            (setq i (+ 1 i))
            )                  
        (dolist (a (coups_possibles temp liste_coups))          
          (setq x (suivant temp a))
          (setq file (append file (list x)))
          (setq file_actions (append file_actions (list (append temp_actions (list a)))))
          (setq file_chemins (append file_chemins (list (append temp_chemins (list x))))))
        )
      )
    )
  )
