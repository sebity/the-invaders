;;;; the-invaders.lisp

(in-package #:the-invaders)

(defparameter *data-root* (asdf:system-source-directory 'the-invaders))
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))

;;;; Game Params
(defparameter *game-width* 800)
(defparameter *game-height* 600)
(defparameter *game-state* 0) ; 0:menu/intro, 1:in game, 2:game over

(defparameter *level* nil)
(defparameter *space-w* 70)
(defparameter *space-h* 45)

(defparameter *pause* nil)

(defparameter *player* nil)
(defparameter *player-lives* 3)
(defparameter *player-level* 1)
(defparameter *player-shots* nil)
(defparameter *player-score* 0)
(defparameter *player-explosion* nil)

(defparameter *enemy* nil)
(defparameter *enemy-count* 0)
(defparameter *enemy-shots* nil)
(defparameter *enemy-direction* 'right)
(defparameter *enemy-explosion* nil)

(defparameter *enemy-move-delay* 60)
(defparameter *enemy-move-space* 10)

(defparameter *mothership* nil)
(defparameter *mothership-explosion* nil)

(defparameter *game-ticks* 0)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *mothership-engine* nil)
(defparameter *soundfx* nil)

(defparameter *ss-player* nil)
(defparameter *ss-enemy* nil)
(defparameter *ss-mothership* nil)
(defparameter *cells* nil)

;;;; GFX Params
(defparameter *gfx-ss-player* (merge-pathnames "spritesheet_player.png" *gfx-root*))
(defparameter *gfx-ss-enemy* (merge-pathnames "spritesheet_enemy.png" *gfx-root*))
(defparameter *gfx-ss-mothership* (merge-pathnames "spritesheet_mothership.png" *gfx-root*))
(defparameter *gfx-explosion-enemy* (merge-pathnames "explosion-1.png" *gfx-root*))
(defparameter *gfx-explosion-player* (merge-pathnames "explosion-2.png" *gfx-root*))
(defparameter *gfx-space-bg* (merge-pathnames "space-bg.jpg" *gfx-root*))
(defparameter *gfx-title-bg* (merge-pathnames "title-bg.jpg" *gfx-root*))
(defparameter *gfx-game-over-bg* (merge-pathnames "game-over-bg.jpg" *gfx-root*))

;;;; Font Params
(defparameter *terminus-ttf-12* 
  (make-instance 'SDL:ttf-font-definition
		 :size 12
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-18* 
  (make-instance 'SDL:ttf-font-definition
		 :size 18
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-24* 
  (make-instance 'SDL:ttf-font-definition
		 :size 24
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-32* 
  (make-instance 'SDL:ttf-font-definition
		 :size 32
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *ttf-font-small* nil)
(defparameter *ttf-font-normal* nil)
(defparameter *ttf-font-large* nil)
(defparameter *ttf-font-huge* nil)


;;;;;;;;;;;;;;;;;;;;;;;; STRUCTS/CLASSES ;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct player
  (x 0)
  (y 0))

(defstruct enemy
  (x 0)
  (y 0)
  (sprite 0))

(defstruct player-shot
  (x 0)
  (y 0)
  (dy 0))

(defstruct player-explosion
  (x 0)
  (y 0)
  (time 0))

(defstruct enemy
  (x 0)
  (y 0)
  (sprite 0))

(defstruct enemy-shot
  (x 0)
  (y 0)
  (dy 0))

(defstruct enemy-explosion
  (x 0)
  (y 0)
  (time 0))

(defstruct mothership
  (x 0)
  (y 0)
  (dx 0))

(defstruct mothership-explosion
  (x 0)
  (y 0)
  (time 0))

;;;;;;;;;;;;;;;;;;;;;;;; SLIME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CONTINUABLE macro

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))


;;;; UPDATE-SWANK function

(defun update-swank ()
  (continuable
   (let ((connection (or swank::*emacs-connection*
			 (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))


;;;;;;;;;;;;;;;;;;;;;;;; UTILS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; SQUARE function

(defun square (x)
  (* x x))


;;;; RANDOM-ELEMENT function

(defun random-element (lst)
  (elt lst (random (length lst))))





;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DRAW-TEXT function

(defun draw-text (string x y r g b &optional (font *ttf-font-normal*))
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)
			   :font font))


;;;; DRAW-BOX function

(defun draw-box (x y w h r g b)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* x y w h)
		:color (sdl:color :r r :g g :b b)))


;;;; DRAW-LINE function

(defun draw-line (x0 y0 x1 y1 r g b)
  (sdl:draw-line-* x0 y0 x1 y1
		   :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE function

(defun draw-circle (x y rad r g b)
  (sdl:draw-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE-FILLED function

(defun draw-circle-filled (x y rad r g b)
  (sdl:draw-filled-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-ELLIPSE-FILLED function

(defun draw-ellipse-filled (x y rx ry r g b)
  (sdl:draw-filled-ellipse-* x y rx ry
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-POLYGON function

(defun draw-polygon (vertices r g b)
  (sdl:draw-filled-polygon vertices :color (sdl:color :r r :g g :b b)))


;;;; PLAY-SOUND function

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))


;;;;;;;;;;;;;;;;;;;;;;;; ENEMY ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CREATE-ENEMY function

(defun create-enemy (dy)
  (setf *enemy* 'nil)

  (if (> dy 200)
      (setf dy 200))

  (loop for y below 5
     do (loop for x below 8
	   do (push (make-enemy :x (* x *space-w*) :y (+ (* y *space-h*) dy)
				:sprite y) *enemy*))))


;;;; DRAW-ENEMY function

(defun draw-enemy ()
  (loop for e in *enemy*
     do (sdl:draw-surface-at-* *ss-enemy* (enemy-x e) (enemy-y e) 
			       :cell (+ (enemy-sprite e) 
					(mod (/ (enemy-x e) 2) 10)))))
;			       :cell (aref *level* (enemy-y e) (enemy-x e)))))


;;;; UPDATE-ENEMY function

(defun update-enemy ()
  (setf *game-ticks* (incf *game-ticks*))
  (if (>= *game-ticks* *enemy-move-delay*)
      (progn (determine-enemy-position) 
	     (update-enemy-position)
	     (play-sound 1)
	     (setf *game-ticks* 0))))


;;;; DETERMINE-ENEMY-POSITION function

(defun determine-enemy-position ()
  (loop for e in *enemy*
     do (if (and (equalp *enemy-direction* 'right)
		 (>= (+ (enemy-x e) 50) *game-width*))
	    (setf *enemy-direction* 'down-and-left)
	    (if (and (equalp *enemy-direction* 'left)
		     (<= (enemy-x e) 0))
		(setf *enemy-direction* 'down-and-right)))))


;;; UPDATE-ENEMY-POSITION function

(defun update-enemy-position ()
  (cond ((equalp *enemy-direction* 'right)
	 (loop for e in *enemy*
	    do (setf (enemy-x e) (+ (enemy-x e) *enemy-move-space*))))

	((equalp *enemy-direction* 'left)
	 (loop for e in *enemy*
	    do (setf (enemy-x e) (+ (enemy-x e) (- *enemy-move-space*)))))

	((equalp *enemy-direction* 'down-and-right)
	 (loop for e in *enemy*
	    do (progn (setf (enemy-y e) (+ (enemy-y e) 20))
		      (setf *enemy-direction* 'right))))

	((equalp *enemy-direction* 'down-and-left)
	 (loop for e in *enemy*
	    do (progn (setf (enemy-y e) (+ (enemy-y e) 20))
		      (setf *enemy-direction* 'left)))))

  (enemy-hit-bottom)

  (dotimes (n (ceiling (/ *player-level* 5)))
    (if (< (random 100) (+ 20 *player-level*))
	(fire-enemy-shot))))


;;; ENEMY-HIT-BOTTOM function

(defun enemy-hit-bottom ()
  (loop for e in *enemy*
     do (if (> (+ (enemy-y e) 32) 540)
	    (setf *player-lives* 0))))


;;;; DETERMINE-ENEMY-SPEED function

(defun determine-enemy-speed ()
  (cond ((= (length *enemy*) 30) (setf *enemy-move-delay* 45))
	((= (length *enemy*) 20) (setf *enemy-move-delay* 30))
	((= (length *enemy*) 15) (setf *enemy-move-delay* 20))
	((= (length *enemy*) 10) (setf *enemy-move-delay* 15))
	((= (length *enemy*) 5) (setf *enemy-move-delay* 12))
	((= (length *enemy*) 3) (setf *enemy-move-delay* 9))
	((= (length *enemy*) 1) (setf *enemy-move-delay* 5))
	(t ())))


;;;; FIRE-ENEMY-SHOT function

(defun fire-enemy-shot ()
  (let ((enemy (random-element *enemy*)))
    (push (make-enemy-shot :x (+ (enemy-x enemy) 24) 
			   :y (+ (enemy-y enemy) 32)
			   :dy (+ (random 3) 3)) *enemy-shots*)
    (play-sound 2)))


;;;; DRAW-ENEMY-SHOT function

(defun draw-enemy-shot ()
  (loop for f in *enemy-shots*
     do (draw-box (enemy-shot-x f) (enemy-shot-y f) 2 10 255 0 0)))


;;;; UPDATE-ENEMY-SHOTS function

(defun update-enemy-shots ()
  (loop for f in *enemy-shots*
     do (progn (if (> (enemy-shot-y f) *game-height*)
		   (setf *enemy-shots* (remove f *enemy-shots*))
		   (setf (enemy-shot-y f) (+ (enemy-shot-y f) (enemy-shot-dy f))))
	       (enemy-shot-player f)))

  (if (<= *player-lives* 0)
      (change-game-state)))


;;;; ENEMY-SHOT-PLAYER function

(defun enemy-shot-player (s)
  (let ((p *player*))
    (if (and (<= (- (player-x p) 26) (enemy-shot-x s))
		 (>= (+ (player-x p) 26) (+ (enemy-shot-x s) 2))
		 (<= (player-y p) (enemy-shot-y s))
		 (>= (+ (player-y p) 32) (enemy-shot-y s)))
	    (progn (create-player-explosion)
		   (setf *player-lives* (decf *player-lives*))
		   (play-sound 4)
		   (setf (player-x p) 400)
		   (setf *enemy-shots* (remove s *enemy-shots*))))))


;;;; CREATE-ENEMY-EXPLOSION function

(defun create-enemy-explosion (x y)
  (push (make-enemy-explosion :x x :y y :time 6) *enemy-explosion*))


;;;; DRAW-ENEMY-EXPLOSION function

(defun draw-enemy-explosion ()
  (loop for e in *enemy-explosion*
     do (progn (setf (enemy-explosion-time e) (decf (enemy-explosion-time e)))
	       (if (zerop (enemy-explosion-time e))
		   (setf *enemy-explosion* (remove e *enemy-explosion*))
		   (sdl:draw-surface-at-* (sdl:load-image *gfx-explosion-enemy*)
					  (enemy-explosion-x e) (enemy-explosion-y e))))))


;;;;;;;;;;;;;;;;;;;;;;;; MOTHERSHIP ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DEPLOY-MOTHERSHIP function

(defun deploy-mothership ()
  (let ((chance (random 1000)))
    (if (and (= chance 1)
	     (not *mothership*)
	     (> (length *enemy*) 5)
	     (< (length *enemy*) 35))
	(create-mothership))))


;;;; CREATE-MOTHERSHIP function

(defun create-mothership ()
  (let ((entrance (random 2))
	(x -70)
	(dx 3))

    (if (= entrance 1)
	(progn (setf x (+ *game-width* 5))
	       (setf dx -3)))

    (setf *mothership* (make-mothership :x x :y 35 :dx dx)))
  (play-mothership-engine))


;;;; DRAW-MOTHERSHIP function

(defun draw-mothership ()
  (if *mothership*
      (let ((m *mothership*))
	(sdl:draw-surface-at-* *ss-mothership* (mothership-x m) (mothership-y m)
			       :cell (floor (mod *game-ticks* 9) 3)))))


;;;; UPDATE-MOTHERSHIP function

(defun update-mothership ()
  (if *mothership*
      (progn (let ((m *mothership*))
	       (setf (mothership-x m) (+ (mothership-x m) (mothership-dx m)))
	       (if (or (<= (mothership-x m) -75)
		       (>= (mothership-x m) (+ *game-width* 10)))
		   (setf *mothership* nil))))))


;;;; CREATE-MOTHERSHIP-EXPLOSION function

(defun create-mothership-explosion (m)
  (setf *mothership-explosion* (make-mothership-explosion :x (mothership-x m)
							  :y (mothership-y m)
							  :time 15)))


;;;; DRAW-MOTHERSHIP-EXPLOSION function

(defun draw-mothership-explosion ()
  (if *mothership-explosion*
      (progn (let ((m *mothership-explosion*))
	       (if (zerop (mothership-explosion-time m))
		   (setf *mothership-explosion* nil)
		   (progn (setf (mothership-explosion-time m)
				(decf (mothership-explosion-time m)))
			  (sdl:draw-surface-at-* (sdl:load-image *gfx-explosion-player*)
						 (mothership-explosion-x m)
						 (mothership-explosion-y m))))))))

;;;; PLAY-MOTHERSHIP-ENGINE function

(defun play-mothership-engine ()
  (sdl-mixer:play-music *mothership-engine*))


;;;; STOP-MOTHERSHIP-ENGINE function

(defun halt-mothership-engine ()
  (sdl-mixer:halt-music 100))


;;;;;;;;;;;;;;;;;;;;;;;; PLAYER ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CREATE-PLAYER function

(defun create-player ()
  (setf *player* (make-player :x 400 :y 540)))


;;;; DRAW-PLAYER-SHIP function

(defun draw-player-ship (p)
  (sdl:draw-surface-at-* *ss-player* (- (player-x p) 26) (player-y p) 
			       :cell (mod *game-ticks* 3)))

;;;; MOVE-PLAYER-SHIP function

(defun move-player-ship (p direction)
  (cond ((equalp direction 'left) (progn (setf (player-x p) (- (player-x p) 4))
					 (if (<= (player-x p) 26)
					     (setf (player-x p) 26))))

	((equalp direction 'right) (progn (setf (player-x p) (+ (player-x p) 4))
					  (if (>= (player-x p) (- *game-width* 26))
					      (setf (player-x p) (- *game-width* 26)))))))


;;;; FIRE-SHOT function

(defun fire-shot (p)
  (if (zerop (length *player-shots*))
      (progn (push (make-player-shot :x (player-x p) :y (player-y p) :dy -5) *player-shots*)
	     (play-sound 2))))


;;;; DRAW-SHOT function

(defun draw-shot ()
  (loop for f in *player-shots*
     do (draw-box (player-shot-x f) (player-shot-y f) 2 10 255 255 255)))


;;;; UPDATE-PLAYER_SHOTS function

(defun update-player-shots ()
  (loop for f in *player-shots*
     do (progn (if (<= (player-shot-y f) 0)
		   (setf *player-shots* (remove f *player-shots*))
		   (setf (player-shot-y f) (+ (player-shot-y f) (player-shot-dy f))))
	       (player-shot-enemy f)
	       (player-shot-mothership f))))


;;;; PLAYER-SHOT-HIT function

(defun player-shot-enemy (s)
  (loop for e in *enemy*
     do (if (and (<= (enemy-x e) (player-shot-x s))
		 (>= (+ (enemy-x e) 48) (+ (player-shot-x s) 2))
		 (<= (enemy-y e) (player-shot-y s))
		 (>= (+ (enemy-y e) 32) (player-shot-y s)))
	    (progn (create-enemy-explosion (enemy-x e) (enemy-y e))
		   (setf *enemy* (remove e *enemy*))
		   (play-sound 3)
		   (setf *player-shots* (remove s *player-shots*))
		   (setf *player-score* (+ *player-score* 10))
		   (determine-enemy-speed))))

  (if (end-of-level-p)
      (progn (calculate-score)
	     (new-level)
	     (play-sound 6))))


;;;; PLAYER-SHOT-MOTHERSHIP function

(defun player-shot-mothership (s)
  (if *mothership*
      (let ((m *mothership*))
	(if (and (<= (mothership-x m) (player-shot-x s))
		 (>= (+ (mothership-x m) 64) (+ (player-shot-x s) 2))
		 (<= (mothership-y m) (player-shot-y s))
		 (>= (+ (mothership-y m) 32) (player-shot-y s)))
	    (progn (create-mothership-explosion m)
		   (setf *player-score* (+ *player-score* (calculate-mothership-score m)))
		   (setf *mothership* nil)
		   (halt-mothership-engine)
		   (play-sound 5))))))


;;;; CREATE-PLAYER-EXPLOSION function

(defun create-player-explosion ()
  (push (make-player-explosion :x (player-x *player*)
			       :y (player-y *player*)
			       :time 20)
	*player-explosion*))


;;;; DRAW-PLAYER-EXPLOSION function

(defun draw-player-explosion ()
  (loop for p in *player-explosion*
     do	(progn (setf (player-explosion-time p) (decf (player-explosion-time p)))
	       (if (zerop (player-explosion-time p))
		   (setf *player-explosion* (remove p *player-explosion*))
		   (sdl:draw-surface-at-* (sdl:load-image *gfx-explosion-player*)
					  (player-explosion-x p) (player-explosion-y p))))))


;;;;;;;;;;;;;;;;;;;;;;;; LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-LEVEL function

(defun display-level ()
  (sdl:draw-surface-at-* (sdl:load-image *gfx-space-bg*) 0 0))


;;;; DRAW-GAME-UI function

(defun draw-game-ui ()
  (draw-text (format nil "Score: ~a" *player-score*) 20 5 255 255 255)
  (draw-text (format nil "Level: ~a" *player-level*) 380 5 255 255 255)
  (draw-text (format nil "Lives: ~a" *player-lives*) 700 5 255 255 255)
  (if (eql *pause* t)
      (draw-text "Paused" 
	     380 280 255 255 255 *ttf-font-large*)))


;;;; END-OF-LEVEL-P function

(defun end-of-level-p ()
  (if (zerop (length *enemy*))
      t
      nil))


;;;; CALCULATE-SCORE function

(defun calculate-score ()
  (setf *player-score* (+ *player-score* (* *player-lives* 100) (* *player-level* 100))))


;;;; CALCULATE-MOTHERSHIP-SCORE function

(defun calculate-mothership-score (m)
  (let ((mid (+ (mothership-x m) 32))
	(sect (/ *game-width* 4)))
    (cond ((<= mid sect)
	   (if (< (mothership-dx m) 0)
	       50
	       300))

	  ((<= mid (* sect 2))
	   (if (< (mothership-dx m) 0)
	       100
	       150))

	  ((<= mid (* sect 3))
	   (if (< (mothership-dx m) 0)
	       150
	       100))

	  (t (if (< (mothership-dx m) 0)
		 300
		 50)))))


;;;; NEW-LEVEL function

(defun new-level ()
  (setf *player-level* (incf *player-level*))
  (reset-level))


;;;; RESET-LEVEL function

(defun reset-level ()
  (let ((level *player-level*))
    (if (> level 15)
	(setf level 15))
    (create-enemy (+ 70 (* level 10)))
    (setf *enemy-move-delay* 60)
    (setf *enemy-direction* 'right)
    (setf *mothership* nil)
    (setf *player-shots* nil)
    (setf *enemy-shots* nil)
    (setf *enemy-explosion* nil)))

;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-END-GAME function

(defun display-end-game ()
  (sdl:draw-surface-at-* (sdl:load-image *gfx-game-over-bg*) 0 0)

  ;(draw-text "The Invaders" 310 20 255 255 255 *ttf-font-huge*)

  (draw-text "Game Over" 330 150 255 255 255 *ttf-font-huge*)

  (draw-text (format nil "Final Score: ~a" *player-score*) 280 250 255 255 0 *ttf-font-huge*)

  (draw-text "Press SPACE to Continue..." 290 570 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (sdl:draw-surface-at-* (sdl:load-image *gfx-title-bg*) 0 0)

  (draw-text "Press SPACE to Continue..." 290 570 255 255 255))


;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; PAUSE-GAME function

(defun pause-game ()
  (if (eql *pause* nil)
      (setf *pause* t)
      (setf *pause* nil)))


;;;; STATE-IN-PLAY function

(defun state-in-play ()
  
  (unless (eql *pause* t)
    (update-player-shots)
    (update-enemy)
    (update-mothership)
    (update-player-shots)
    (update-enemy-shots)
    (deploy-mothership))

  (display-level)
  (draw-player-ship *player*)
  (draw-enemy)
  (draw-mothership)
  (draw-shot)
  (draw-enemy-shot)
  (draw-player-explosion)
  (draw-enemy-explosion)
  (draw-mothership-explosion)
  (draw-game-ui))


;;;; CONTINUE-OPTION function

(defun continue-option ()
  (cond ((zerop *game-state*) (change-game-state))
	((= *game-state* 2) (change-game-state))
	(t ())))


;;;; CHANGE-GAME-STATE function

(defun change-game-state ()
  (cond ((zerop *game-state*) 
	 (progn (reset-game)
		(play-sound 6)
		(setf *game-state* 1)))

	((= *game-state* 1) (setf *game-state* 2))
	
	((= *game-state* 2) (setf *game-state* 0))
	
	(t ())))


;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)

  (cond ((= *game-state* 1) (state-in-play))

	((= *game-state* 2) (display-end-game))

	(t (display-menu)))

  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *pause* nil)
  (setf *player-level* 0)
  (setf *player-lives* 3)
  (setf *player-score* 0)
  (setf *player-shots* nil)
  (setf *player-explosion* nil)
  (setf *enemy-shots* nil)
  (setf *enemy-explosion* nil)
  (setf *mothership-explosion* nil)
  (new-level))



;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0)
  (create-player))


(defun load-sprite-sheet ()
  ; enemy sprite sheet
  (setf *ss-enemy* (sdl:load-image *gfx-ss-enemy*))
  
  (setf *cells* '((0 0 48 32) (48 0 48 32) (96 0 48 32) (144 0 48 32) (192 0 48 32)
		  (0 32 48 32) (48 32 48 32) (96 32 48 32) (144 32 48 32) (192 32 48 32)))

  (setf (sdl:cells *ss-enemy*) *cells*)

  ; player sprite sheet
  (setf *ss-player* (sdl:load-image *gfx-ss-player*))
  
  (setf *cells* '((0 0 52 32) (0 32 52 32) (0 64 52 32)))

  (setf (sdl:cells *ss-player*) *cells*)

  ; mothership sprite sheet
  (setf *ss-mothership* (sdl:load-image *gfx-ss-mothership*))
  
  (setf *cells* '((0 0 64 32) (0 32 64 32) (0 64 64 32)))

  (setf (sdl:cells *ss-mothership*) *cells*)
)


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 7))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    (setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "bass-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 1) (sdl-mixer:load-sample (sdl:create-path "bass-2.ogg" *audio-root*)))
    (setf (aref *soundfx* 2) (sdl-mixer:load-sample (sdl:create-path "laser-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 3) (sdl-mixer:load-sample (sdl:create-path "explode-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 4) (sdl-mixer:load-sample (sdl:create-path "explode-2.ogg" *audio-root*)))
    (setf (aref *soundfx* 5) (sdl-mixer:load-sample (sdl:create-path "explode-3.ogg" *audio-root*)))
    (setf (aref *soundfx* 6) (sdl-mixer:load-sample (sdl:create-path "level-up.ogg" *audio-root*)))
    (setf *mothership-engine* (sdl-mixer:load-music (sdl:create-path "mothership.ogg" *audio-root*)))
    (sample-finished-action)
    (sdl-mixer:allocate-channels 16)))


;;; SAMPLE-FINISHED-ACTION function

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))


;;;; CLEAN-UP function

(defun clean-up ()
  (when *mothership-engine*
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *mothership-engine*)
    (setf *mothership-engine* nil))

  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))

  (loop for s below (length *soundfx*)
     do (if (equal (aref *soundfx* s) 0)
	    t
	    (progn (sdl:free (aref *soundfx* s))
		   (setf (aref *soundfx* s) 0))))
  
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))


;;;; START function

(defun start ()
  (initialize-game)
  (reset-game)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height* :title-caption "The Invaders")
    (setf (sdl:frame-rate) 60)

    (setup-audio)

    (load-sprite-sheet)

    ;(sdl-mixer:play-music *music-intro* :loop t)

    (unless (sdl:initialise-default-font *terminus-ttf-18*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    (setf *ttf-font-small* (sdl:initialise-font *terminus-ttf-12*))
    (setf *ttf-font-normal* (sdl:initialise-font *terminus-ttf-18*))
    (setf *ttf-font-large* (sdl:initialise-font *terminus-ttf-24*))
    (setf *ttf-font-huge* (sdl:initialise-font *terminus-ttf-32*))
    
    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-p (if (= *game-state* 1)
					 (pause-game)))
			 (:sdl-key-q (if (= *game-state* 1)
					 (change-game-state)))
			 (:sdl-key-z (if (= *game-state* 1)
					 (fire-shot *player*)))
			 (:sdl-key-space (continue-option))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:idle ()
	     (when (sdl:get-key-state :sdl-key-left) (move-player-ship *player* 'left))
	     (when (sdl:get-key-state :sdl-key-right) (move-player-ship *player* 'right))
	     (render)))))
