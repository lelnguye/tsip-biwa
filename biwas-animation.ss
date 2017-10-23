;; biwas-animation.ss
;; by tsip
;;
;; HTML5 animation for biwascheme


;; animation

;; Paul Irish's shim for scheme
(let* ((window (js-eval "window"))
       (anim-frame-f 
        (or (js-ref window "requestAnimationFrame")
            (js-ref window "webkitRequestAnimationFrame")
            (js-ref window "mozRequestAnimationFrame")
            (js-ref window "oRequestAnimationFrame")
            (js-ref window "msRequestAnimationFrame")
            (lambda (proc)
                (js-invoke window "setTimeout" (js-closure proc) (/ 1000 60))))))
    (js-set! window "requestAnimFrame" (lambda (proc) (js-call anim-frame-f (js-closure proc)))))

;; calls the requestAnimationFrame function on the given function
(define (call-animation-frame proc)
    (let* ((window (js-eval "window"))
           (animation-frame (js-ref window "requestAnimFrame")))
        (animation-frame proc)))


;; creates a list of animation frames for a sprite
(define (create-sprite 
            spritesheet image-width image-height num-frames startx starty sprite-width sprite-height)
    (let ((create-sprite-ls-f
            (lambda (x y nf f)
                (if (= nf 0)
                    '()
                    (let ((frame (list x y sprite-width sprite-height))
                          (add-x (+ x sprite-width))
                          (add-y (+ y sprite-height)))
                        (let ((next-x (if (>= add-x image-width) 0 add-x))
                              (next-y (if (>= add-x image-width) 
                                          (if (>= add-y image-height) 0 add-y)
                                          y)))
                            (cons frame (f next-x next-y (- nf 1) f))))))))
        (list spritesheet (create-sprite-ls-f startx starty num-frames create-sprite-ls-f))))

;; animates a sprite at a fixed position
;;
;; context - the HTML5 canvas context 
;; sprite - sprite to animate
;; x - position of sprite on the x-axis
;; y - position of sprite on the y-axis
;; width - width of sprite
;; height - height of sprite
;; speed - the speed of the animation 
;; flag - flag to tell the animation when to stop (flag is a procedure that takes no parameters)
(define (animate-fixed-sprite context sprite x y width height speed flag)
    (let* ((image-file (car sprite))
           (image (load-image image-file))
           (frames (cadr sprite))
           (frame-count (length frames))
           (ind 0))
        (letrec ((sprite-animation
                    (lambda ()
                        (let* ((i (mod ind frame-count))
                               (frame (list-ref frames i))
                               (frame-x (list-ref frame 0))
                               (frame-y (list-ref frame 1))
                               (frame-width (list-ref frame 2))
                               (frame-height (list-ref frame 3)))
                            (sleep (/ 1 (* speed frame-count)))
                            (clear-rect context x y width height)
                            (js-invoke context "drawImage" (car image) frame-x frame-y frame-width frame-height x y width height) 
                            (set! ind (+ i 1))
                            (if (flag)
                                (call-animation-frame sprite-animation))))))
            (call-animation-frame sprite-animation))))

;; animates a sprite that moves along a defined path
;;
;; context - the HTML5 canvas context
;; sprite - sprite to animate
;; width - sprite width
;; height - sprite height
;; path - a list of coordinates that determine the path the animation will take
;; speed - the speed of the animation
;; loop - tells the animation to keep on playing even after getting to the end of the path
(define (animate-moving-sprite context sprite width height path speed loop)
    (let* ((image-file (car sprite))
           (image (load-image image-file))
           (img (car image))
           (frames (cadr sprite))
           (frame-count (length frames))
           (frame-ind 0)
           (path-count (length path))
           (current 0)
           (path-ind 1)
           (path-mod 1)
           (get-modifier
            (lambda (n1 n2)
                (let ((diff (- n1 n2)))
                    (if (not (= diff 0)) (/ diff (abs diff)) 0))))
           (draw-frame-f
            (lambda (frame-x frame-y frame-width frame-height dest-x dest-y)
                (js-invoke context "drawImage" img frame-x frame-y frame-width frame-height dest-x dest-y width height))))
        (letrec* ((move-to-f
                    (lambda (current-location destination)
                        (let* ((i (mod frame-ind frame-count))
                               (frame (list-ref frames i))
                               (frame-x (list-ref frame 0))
                               (frame-y (list-ref frame 1))
                               (frame-width (list-ref frame 2))
                               (frame-height (list-ref frame 3))
                               (curr-x (car current-location))
                               (curr-y (cdr current-location))
                               (dest-x (car destination))
                               (dest-y (cdr destination))
                               (mod-x (get-modifier dest-x curr-x))
                               (mod-y (get-modifier dest-y curr-y))
                               (new-x (+ curr-x mod-x))
                               (new-y (+ curr-y mod-y)))
                            (if (or (not (= curr-x dest-x)) (not (= curr-y dest-y)))
                                (begin
                                    (sleep (/ 1 (* speed frame-count)))
                                    (clear-rect context curr-x curr-y width height)
                                    (draw-frame-f frame-x frame-y frame-width frame-height new-x new-y)
                                    (set! frame-ind (+ i 1))
                                    (move-to-f (coordinates new-x new-y) destination))))))
                  (loop-f
                    (lambda ()
                        (let* ((current-location (list-ref path current))
                               (destination (list-ref path path-ind))
                               (at-end? (or (= path-ind (- path-count 1)) (= path-ind 0))))
                            (begin
                                (console-log (format "moving from ~a to ~a" current-location destination)) 
                                (move-to-f current-location destination)
                                (if (or (not at-end?) (and at-end? loop))
                                    (begin
                                        (if (and at-end? loop)
                                            (set! path-mod (* path-mod -1)))
                                        (set! current path-ind)
                                        (set! path-ind (+ path-ind path-mod))
                                        (call-animation-frame loop-f))))))))
            (call-animation-frame loop-f))))

