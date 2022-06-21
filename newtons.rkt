; #!usr/bin/env racket
#lang racket
(require trace)
; Newtons method:
; x_n+1 = x_n - (f(x_n) / f'(x_n))

; lim h-> 0 f'(x) = (f(x + h) - f(x))/h
; set h = 0.00001 for now
(define (deriv f x) 
    (/ (- (f (+ x 0.000001)) (f x)) 0.000001)
)
; f(x) = sin x
; (display (deriv sin 2.0))
; (newline)
; f(x) = sin x
; (display (deriv sin 2.0))
; (newline)

; f(x) = cos x - x
; (display (deriv (lambda (x) (- (cos x) x)) 2.0))
; (newline)


; Newtons method:
; x_n+1 = x_n - (f(x_n) / f'(x_n))
(define (newtons f n x0)
    (cond
        ((zero? n) x0)
        (else
            (let ((res (newtons f (- n 1) x0))) 
                (- 
                    res
                    (/ 
                        (f res)
                        (deriv f res)
                    )
                )
            )
        )
    )
) 

; (trace newtons)
; (display (newtons (lambda (x) (- (cos x) x)) 30 1))
; (newline)

; (display (newtons (lambda (x) (+ (expt x 5) (expt x 2) x 2)) 30 100))
; (newline)

; (display (newtons (lambda (x) (sin x)) 30 2))
; (newline)


; Newtons method on complex numbers
; 

(define TOLERANCE 0.000001)
(define NUM_ITERATIONS 20)
(define GRID_SIZE 100)
(define RESOLUTION 680)


(define BLACK 0)
(define RED 1)
(define GREEN 2)
(define BLUE 3)

(define COLORS '(BLACK RED GREEN BLUE))

; For every (x,y) in R^2, [bounded by 2.7 or so, in incremenets of 0.1]
; Conduct newtons method with (x,y) as the initial condition
; If the solution does not converge [within TOLERANCE] to an established root in n steps [n = 20]
; Paint the pixel black
; Else:
; Paint the pxiel the color of the root it converged to


; (define (itr-over-list min max fn)
;   (if (>= max min)
;     (begin (fn min) (loop (+ min 1) max (apply fn min)))
;     (void))
; )


; (define (get-element-recurse lst i j)
;     (cond 
;         ((null? lst) '())
;         ((= i j) (car lst))
;         (else (set! j (+ 1 j)) (get-element-recurse (cdr lst) i j))
;     )

; )


; (define (get-element lst i)
;     (get-element-recurse lst i 0)
; )



; (display (get-element '(a b c) 0))
; (loop 1 3 (display(get-element '(abc) 0)))

; (display (loop 0 10 (lambda (x) (+ 2 x))))

(define (within-threshold-to-roots z roots) 
    (begin
        (map 
            (lambda (x)
                (cond
                    (   (and (< (abs(real-part(- z x))) TOLERANCE) (< (abs(imag-part(- z x)))  TOLERANCE))
                        #t
                    )
                    (else #f) 
                )
            )
        roots)
    )
)

; (display (within-threshold-to-roots 3+2i '(3+2.00000000001i 4+1i 1+1i)))




(define (get-element-recurse lst i j)
    (cond 
        ((null? lst) '())
        ((= i j) (car lst))
        (else (set! j (+ 1 j)) (get-element-recurse (cdr lst) i j))
    )

)


(define (get-element lst i)
    (get-element-recurse lst i 0)
)


; return #f if there are no roots that meet threshold
; otherwise return the corresponding color
(define (get-color thresholds)
    (get-color-recurse thresholds 0)
)



(define (get-color-recurse thresholds j)
    (cond 
        ((null? thresholds) BLACK)
        ((eq? #t (car thresholds)) (get-element COLORS j))
        (else (set! j (+ 1 j)) (get-color-recurse (cdr thresholds) j))
    )

)

; For each (x,y) in grid
; paint the (x,y) coordinate
; (define (newtons-complex-grid f roots) 
;     ()
; )

; grid should be layed out like
; ((complex_num complex_num complex_num complex_num) (complex_num complex_num complex_num complex_num) (complex_num complex_num complex_num complex_num) )

; (define (paint-grid grid f roots)
;     ()
; )

; This function will tell you what color this complex number should be
(define (paint-newtons-complex f z roots) 
    (let ((res (paint-newtons-complex-recurse f NUM_ITERATIONS z roots)))
        (get-color (within-threshold-to-roots res roots))
        ; ((not (boolean? (get-color (within-threshold-to-roots z roots)))) (get-color (within-threshold-to-roots z roots)))
    )
)


(define (paint-newtons-complex-recurse f n z roots)
    (cond
        ((= 0 n) z)
        (else
            (set! z (paint-newtons-complex-recurse f (- n 1) z roots)) 
            (- z (/ (f z) (deriv f z)))
        )
    )
)
; (trace paint-newtons-complex-recurse)
; (display (paint-newtons-complex (lambda (x) (- (expt x 3) 1)) 0.5+0.5i '(1+0i -0.5+0.866025403i -0.5-0.866025403i)))

; (define newtons-complex)

; (define (generate-newtons-fractal) ())

(define (build-grid step xmin xmax ymin ymax) 
    (build-grid-recurse step xmin xmax ymin ymax xmin ymin)
)



(define (build-grid-recurse step xmin xmax ymin ymax x y)
    (cond 
        ((and (>= y ymax) (>= x xmax)) (list (make-rectangular x y))) ; End
        ((>= x xmax)
                ; (set! y (+ y step))
                ; (set! x xmin)
                (append (list (make-rectangular x y)) (build-grid-recurse step xmin xmax ymin ymax xmin (+ y step)) )
                ; (build-grid-recurse step xmin xmax ymin ymax x (+ y step)) 
                
            
        ) ; End line, increase y, reset x
        
        (else 

            (append (list (make-rectangular x y)) (build-grid-recurse step xmin xmax ymin ymax (+ x step) y))
            ; (display x)
                

        )
    ) ; Progress, add x+yi to list
)


; step = (xmax - xmin)/RES

(newline)

(define grid (build-grid (/ 4 RESOLUTION) -2 2 -2 2 ))
(define equation (lambda (x) (- (expt x 3) 1)))
(define roots '(1+0i -0.5+0.866025403i -0.5-0.866025403i))
(newline)

; (display (list? grid))
(newline)

; (display grid)

; (display
;     (map (lambda (x) (paint-newtons-complex equation x roots)) grid)
; )

(set! grid (map (lambda (x) (paint-newtons-complex equation x roots)) grid))

(with-output-to-file "out.txt"
    (lambda () (print grid)))

















; (build-grid 10)
; (display (vector-map (make-vector 10) (lambda (x) (vector-set! x make-vector 10)) ))
; 1-2-3-4-5-6-7-8-9-10||1-2-3-4-5-6-7-8-9-10
; (define (build-grid-recurse step xmin xmax ymin ymax x y lst)
;     (cond 
;         ((>= y ymax) '()) ; End
;         ((>= x xmax)
;             (begin 
;                 (+ y step)
;                 (set! x xmin)
;                 (append lst (list (make-rectangular x y)))
;                 (build-grid-recurse step xmin xmax ymin ymax x (+ y step) lst) 
                
;             )
;         ) ; End line, increase y, reset x
        
;         (else 
;             (cons(build-grid-recurse step xmin xmax ymin ymax (+ x step) y lst))
;         )
;     ) ; Progress, add x+yi to list
; )


; (define (build-grid-recurse step res xmin xmax ymin ymax x y lst)
;     (cond
;         ((>= y ymax) lst)
;         (#t (append (build-list res (lambda (x) (* x step))) (build-grid-recurse step res xmin) )
;     )
; )

; (trace build-grid-recurse)
(newline)
; (define grid (build-grid 0.1 -1 1 -1 1))
; (display (list? grid))
; (display (car grid))
; (map (lambda (x) (begin(display(x))(newline))) grid)

; (display
;     (build-list 5 
;         (lambda (x) 

;             ; (make-rectangular x x)
;         )
;     )
; )



















