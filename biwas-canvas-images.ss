;; biwas-canvas-images.ss
;; by tsip
;;
;; functions for drawing images to a HTML5 canvas

;; loads an image file from a file or URL
(define (load-image image-file-name)
    (let ((img-obj (list (js-new "Image") (list))))
        (js-set! (car img-obj) "src" image-file-name)
        (js-set! 
            (car img-obj) 
            "onload" 
            (js-closure 
                (lambda () 
                    (let ((loop-draw-inst
                            (lambda (ls f)
                                (if (not (null? ls))
                                    (let ((inst (car ls)))
                                            (inst) (f (cdr ls) f))))))
                        (loop-draw-inst (cadr img-obj) loop-draw-inst)))))
        img-obj))

(define (push-instruction! image inst)
    (let* ((instructions (cadr image))
           (instructions-count (length instructions)))
        (if (null? instructions)
            (set-car! (list-tail image 1) (cons inst instructions))
            (set-cdr! (list-tail instructions (- instructions-count 1)) (cons inst '())))))

(define (draw-image context image destX destY)
    (let* ((img (car image))
           (draw-inst (lambda () (js-invoke context "drawImage" img destX destY))))
        (push-instruction! image draw-inst)))
 
           
(define (draw-scaled-image context image destX destY width height)
    (let* ((img (car image))
           (draw-inst (lambda () (js-invoke context "drawImage" img destX destY width height))))
        (push-instruction! image draw-inst)))
   

(define (draw-slice context image sourceX sourceY sourceWidth sourceHeight destX destY width height)
    (let* ((img (car image))
           (draw-inst (lambda () (js-invoke context "drawImage" img sourceX sourceY sourceWidth sourceHeight destX destY width height))))
        (push-instruction! image draw-inst)))


