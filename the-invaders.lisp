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
(defparameter *space-w* 80)
(defparameter *space-h* 50)

(defparameter *pause* nil)

(defparameter *player* nil)
(defparameter *player-lives* 3)
(defparameter *player-level* 1)
(defparameter *player-shots* nil)
(defparameter *player-score* 0)

(defparameter *enemy* nil)
(defparameter *enemy-count* 0)
(defparameter *enemy-shots* nil)
(defparameter *enemy-direction* 'right)

(defparameter *enemy-move-delay* 1)
(defparameter *enemy-move-space* 1)

(defparameter *game-ticks* 0)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *soundfx* nil)

(defparameter *ss-player* nil)
(defparameter *ss-enemy* nil)
(defparameter *cells* nil)

;;;; GFX Params
(defparameter *gfx-ss-player* (merge-pathnames "spritesheet_player.png" *gfx-root*))
(defparameter *gfx-ss-enemy* (merge-pathnames "spritesheet_enemy.png" *gfx-root*))
(defparameter *gfx-space-bg* (merge-pathnames "space-bg.jpg" *gfx-root*))

;;;; Font Params
(defparameter *terminus-ttf-12* 
  (make-instance 'SDL:ttf-font-definition
		 :size 12
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-18* 
  (make-instance 'SDL:ttf-font-definition
		 :size 18
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *termi111nus-ttf-24* 
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

(defstruct enemy-shot
  (x 0)
  (y 0)
  (dy 0))


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


;;;;;;;;;;;;;;;;;;;;;;;; MATHS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; SQUARE function

(defun square (x)
  (* x x))






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
		      (setf *enemy-direction* 'left))))))


;;;; DETERMINE-ENEMY-SPEED function

(defun determine-enemy-speed ()
  (cond ((= (length *enemy*) 30) (setf *enemy-move-delay* 20))
	((= (length *enemy*) 20) (setf *enemy-move-delay* 10))
	((= (length *enemy*) 10) (setf *enemy-move-delay* 8))
	((= (length *enemy*) 5) (setf *enemy-move-delay* 6))
	((= (length *enemy*) 2) (setf *enemy-move-delay* 4))
	((= (length *enemy*) 1) (setf *enemy-move-delay* 2))
	(t ())))


;;;; FIRE-ENEMY-SHOT function

(defun fire-enemy-shot (x y)
  (push (make-enemy-shot :x x :y y :dy 5) *enemy-shots*)
  (play-sound 2))


;;;; DRAW-ENEMY-SHOT function

(defun draw-enemy-shot ()
  (loop for f in *enemy-shots*
     do (draw-box (enemy-shot-x f) (enemy-shot-y f) 2 10 255 0 0)))


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
  (cond ((equalp direction 'left) (setf (player-x p) (- (player-x p) 4)))

	((equalp direction 'right) (setf (player-x p) (+ (player-x p) 4)))))


;;;; FIRE-SHOT function

(defun fire-shot (p)
  (push (make-player-shot :x (player-x p) :y (player-y p) :dy -5) *player-shots*)
  (play-sound 2))


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
	       (player-shot-enemy f))))


;;;; PLAYER-SHOT-HIT function

(defun player-shot-enemy (s)
  (loop for e in *enemy*
     do (if (and (<= (enemy-x e) (player-shot-x s))
		 (>= (+ (enemy-x e) 48) (+ (player-shot-x s) 2))
		 (<= (enemy-y e) (player-shot-y s))
		 (>= (+ (enemy-y e) 32) (player-shot-y s)))
	    (progn (setf *enemy* (remove e *enemy*))
		   (play-sound 3)
		   (setf *player-shots* (remove s *player-shots*))
		   (determine-enemy-speed))))

  (if (end-of-level-p)
      (new-level)))




;;;;;;;;;;;;;;;;;;;;;;;; LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-LEVEL function

(defun display-level ()
  (sdl:draw-surface-at-* (sdl:load-image *gfx-space-bg*) 0 0))


;;;; DRAW-GAME-UI function

(defun draw-game-ui ()
  (draw-text "Score:" 20 5 255 255 255)
  (draw-text "Level:" 380 5 255 255 255)
  (draw-text "Lives:" 700 5 255 255 255)
  (if (eql *pause* t)
      (draw-text "Paused" 
	     380 280 255 255 255 *ttf-font-large*)))


;;;; END-OF-LEVEL-P function

(defun end-of-level-p ()
  (if (zerop (length *enemy*))
      t
      nil))


;;;; NEW-LEVEL function

(defun new-level ()
  (setf *player-level* (incf *player-level*))
  (setf *enemy-move-delay* 30)
  (create-enemy (+ 30 (* *player-level* 20))))


;;;; RESET-LEVEL function

(defun reset-level ()
  )

;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-END-GAME function

(defun display-end-game ()
  (draw-text "The Invaders" 310 20 255 255 255 *ttf-font-huge*)

  (draw-text "Game Over" 20 100 255 255 255)

  (draw-text "Press SPACE to Continue..." 290 570 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (draw-text "The Invaders" 310 20 255 255 255 *ttf-font-huge*)

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
    (update-player-shots))

  (display-level)
  (draw-player-ship *player*)
  (draw-enemy)
  (draw-shot)
  (draw-enemy-shot)
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
  (setf *player-level* 0)
  (setf *player-shots* nil)
  (setf *player-score* 0)
  (setf *pause* nil)
  (setf *enemy-direction* 'right)
  (setf *enemy-move-delay* 30)
  (setf *enemy-move-space* 10)
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

  (setf (sdl:cells *ss-player*) *cells*))


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 4))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    (setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "bass-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 1) (sdl-mixer:load-sample (sdl:create-path "bass-2.ogg" *audio-root*)))
    (setf (aref *soundfx* 2) (sdl-mixer:load-sample (sdl:create-path "laser-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 3) (sdl-mixer:load-sample (sdl:create-path "explode-1.ogg" *audio-root*)))
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
  (when *music*
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *music*)
    (setf *music* nil))

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
