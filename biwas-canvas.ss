;; biwas-canvas.ss
;; by tsip
;;
;; biwascheme methods for working with HTML5 Canvas

;; creates an html5 canvas object
(define (get-canvas canvas-id)
    (js-eval (format "document.getElementById('~a')" canvas-id)))

(define (canvas-context canvas-id)
    (let ((js-str (format "var c = document.getElementById('~a'); c.getContext('2d')" canvas-id)))
        ;;(print js-str)
        (js-eval js-str)))

;; setter functions for the canvas context

(define (set-fill! context color)
    (js-set! context "fillStyle" color))

(define (set-stroke-style! context style)
    (js-set! context "strokeStyle" style))

(define (set-shadow-blur! context blur-level)
    (js-set! context "shadowBlur" blur-level))

(define (set-shadow-color! context color)
    (js-set! context "shadowColor" color))

(define (set-shadow-offset context X Y)
    (begin
        (js-set! context "shadowOffsetX" X)
        (js-set! context "shadowOffsetY" Y)))

(define (set-line-cap! context linecap)
    (js-set! context "lineCap" linecap))

(define (set-line-join! context line-join)
    (js-set! context "lineJoin" line-join))

(define (set-line-width! context width)
    (js-set! context "lineWidth" width))

(define (set-miter-limit context limit)
    (js-set! context "miterLimit" limit))


(define (set-font! context font)
    (js-set! context "font" font))


(define (set-text-align context align)
    (js-set! context "textAlign" align))

(define (set-global-alpha context alpha)
    (js-set! context "globalAlpha" alpha))

(define (set-global-composite-op context composite-op)
    (js-set! context "globalCompositeOperation" composite-op))

;; clears the contents of the canvas
(define (clear-canvas canvas)
    (let ((width (js-ref canvas "width"))
          (height (js-ref canvas "height"))
          (context (js-invoke canvas "getContext" "2d")))
        (js-invoke context "clearRect" 0 0 width height)))

(define (clear-rect context x y width height)
    (js-invoke context "clearRect" x y width height))
    

(define (save-context context) (js-invoke context "save"))
(define (restore-context context) (js-invoke context "restore"))

;; rectangle

(define (rectangle x y width height)
    (list (list x y width height) 1 "#000000" "#FFFFFF"))

(define (set-border-width! rect width)
    (set-car! (list-tail rect 1) width))

(define (set-border-color! rect color)
    (set-car! (list-tail rect 2) color))

(define (set-fill! rect fill)
    (set-car! (list-tail rect 3) fill))

;; coordinates 

(define (coordinates X Y) (cons X Y))

(define (polygon startX startY)
    (list (list (coordinates startX startY)) 1 "#000000" "#FFFFFF"))

(define (add-point! polygon X Y) 
    (let ((newCoord (cons (coordinates X Y) '()))
          (coords (car polygon)))
        (set-cdr! (list-tail coords (- (length coords) 1)) newCoord)))

(define (add-arc! polygon tanX1 tanY1 tanX2 tanY2 radius)
    (let ((arc (cons (list 'arc (coordinates tanX1 tanY1) (coordinates tanX2 tanY2) radius) '()))
          (coords (car polygon)))
        (set-cdr! (list-tail coords (- (length coords) 1)) arc)))

(define (add-bezier-curve! polygon cpX1 cpY1 cpX2 cpY2 endX endY)
    (let ((bcurve (cons (list 'bezier (coordinates cpX1 cpY1) (coordinates cpX2 cpY2) (coordinates endX endY)) '()))
          (coords (car polygon)))
        (set-cdr! (list-tail coords (- (length coords) 1)) bcurve)))

(define (add-quadratic-curve! polygon cpX cpY endX endY)
    (let ((qcurve (cons (list 'quadratic (coordinates cpX cpY) (coordinates endX endY)) '()))  
          (coords (car polygon)))
        (set-cdr! (list-tail coords (- (length coords) 1)) qcurve)))
    

(define (is-arc? a) (equal? 'arc (car a)))
(define (is-bezier? b) (equal? 'bezier (car b)))
(define (is-quadratic? q) (equal? 'quadratic (car q)))

;; circle

(define (circle center-x center-y radius)
    (list (list center-x center-y radius) 1 "#000000" "#FFFFFF"))

;; functions for drawing on the canvas

(define (draw-rectangle context rect)
    (let ((x (list-ref (car rect) 0))
          (y (list-ref (car rect) 1))
          (width (list-ref (car rect) 2))
          (height (list-ref (car rect) 3))
          (border-width (list-ref rect 1))
          (border-color (list-ref rect 2))
          (fill (list-ref rect 3)))
        (begin
            (js-set! context "fillStyle" fill)
            (js-invoke context "fillRect" x y width height)
            (js-set! context "lineWidth" border-width)
            (js-set! context "strokeStyle" border-color)
            (js-invoke context "strokeRect" x y width height))))

(define pi (js-eval "Math.PI"))

(define (draw-circle context circle)
    (let ((center-x (list-ref (car circle) 0))
          (center-y (list-ref (car circle) 1))
          (radius (list-ref (car circle) 2))
          (border-width (list-ref circle 1))
          (border-color (list-ref circle 2))
          (fill (list-ref circle 3)))
        (begin
            (js-set! context "fillStyle" fill)
            (js-invoke context "beginPath")
            (js-invoke context "arc" center-x center-y radius 0 (* 2 pi))
            (js-invoke context "closePath")
            (js-invoke context "fill")
            (js-set! context "lineWidth" border-width)
            (js-set! context "strokeStyle" border-color)
            (js-invoke context "stroke"))))


(define (draw-polygon context polygon)
    (let ((coords (list-ref polygon 0))
          (border-width (list-ref polygon 1))
          (border-color (list-ref polygon 2))
          (fill (list-ref polygon 3))
          (startPoint (caar polygon))
          (draw-arc
            (lambda (instruction)
                (let ((tan1 (list-ref instruction 1))
                      (tan2 (list-ref instruction 2))
                      (radius (list-ref instruction 3)))
                    (js-invoke context "arcTo" (car tan1) (cdr tan1) (car tan2) (cdr tan2) radius))))
          (draw-bezier
            (lambda (instruction)
                (let ((cp1 (list-ref instruction 1))
                      (cp2 (list-ref instruction 2))
                      (end (list-ref instruction 3)))
                    (js-invoke context "bezierCurveTo" (car cp1) (cdr cp1) (car cp2) (cdr cp2) (car end) (cdr end)))))
          (draw-quadratic
            (lambda (instruction)
                (let ((cp (list-ref instruction 1))
                      (end (list-ref instruction 2)))
                    (js-invoke context "quadraticCurveTo" (car cp) (cdr cp) (car end) (cdr end)))))
          (loop-coordinates-f
            (lambda (coords i draw-arc-f draw-bezier-f draw-quadratic-f f)
                (if (< i (length coords))
                    (let ((instruction (list-ref coords i)))
                        (if (is-arc? instruction)
                            (draw-arc-f instruction)
                            (if (is-bezier? instruction)
                                (draw-bezier-f instruction)
                                (if (is-quadratic? instruction)
                                    (draw-quadratic-f instruction)
                                    (js-invoke context "lineTo" (car instruction) (cdr instruction)))))
                        (f coords (+ i 1) draw-arc-f draw-bezier-f draw-quadratic-f f))))))
        (begin
            (js-set! context "fillStyle" fill)
            (js-invoke context "beginPath")
            (js-invoke context "moveTo" (car startPoint) (cdr startPoint)) 
            (loop-coordinates-f coords 1 draw-arc draw-bezier draw-quadratic loop-coordinates-f)
            (js-invoke context "closePath")
            (js-invoke context "fill")
            (js-set! context "lineWidth" border-width)
            (js-set! context "strokeStyle" border-color)
            (js-invoke context "stroke")))) 


;; text 

(define (draw-text-plain context text x y)
    (js-set! context "fillStyle" "black")
    (js-invoke context "fillText" text x y))

(define (draw-text-with-font context text font color x y)
    (js-set! context "fillStyle" color)
    (js-set! context "font" font)
    (js-invoke context "fillText" text x y))

