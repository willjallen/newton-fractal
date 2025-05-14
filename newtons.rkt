#lang racket

(define TOLERANCE 0.000001)

; 4k is 4320 x 7680
; 16k is 8640 x 15360 
; Resolutions are inverted, (RES-Y, RES-X)
(define RESOLUTION-X 1920)
(define RESOLUTION-Y 1080)

(define XMIN -2)
(define XMAX 2)
(define YMIN -2)
(define YMAX 2)

(define colors '((255 0 0) (0 255 0) (0 0 255)))

(define FUNCTION (lambda (x) (exact->inexact(- (expt x 3) 1))))
(define DERIV (lambda (x) (exact->inexact(* 3 (expt x 2)))))
(define ROOTS '(1+0i -0.5+0.866025403i -0.5-0.866025403i))


; (define FUNCTION (lambda (x) (exact->inexact(+ (- (expt x 12) (* 6 (expt x 3))) 2))))
; (define DERIV (lambda (x) (exact->inexact(- (* 12 (expt x 11)) (* 18 (expt x 2))))))
; (define ROOTS '(0.69482 1.1908 -0.34741-0.60173i -0.34741+0.60173i -0.59542-1.0313i -0.59542+1.0313i -1.1658-0.40453i))

(define MAX-ITR 100)

; Generate an nxm vector of 0s
(define (gen-matrix m n)
    (build-vector m
        (lambda (row)
            (make-vector n 0)
        )
    )
)



; Find the incremenet step for a line of length max - min with d subdivisions
(define (find-step min-val max-val d)
    (exact->inexact( / (- max-val min-val) (- d 1)))
)

; Return the roots mapped with themselves, if z is within threshold
; Returns false otherwise
(define (within-threshold-to-roots z roots) 
    (map 
        (lambda (x)
            (cond
                (   
                    (and (< (abs(real-part(- z x))) TOLERANCE) (< (abs(imag-part(- z x)))  TOLERANCE))
                    x
                )
                (else #f) 
            )
        )
    roots)
)


; Set an nxm matrix to a complex plane
(define (make-complex-plane vec x y xstep ystep)

        (for ([i (in-range 0 (vector-length vec))])
            (for ([j (in-range 0 (vector-length (vector-ref vec i)))])
                    (vector-set! (vector-ref vec i) j (exact->inexact(make-rectangular (+ x (* xstep i)) (+ y (* ystep j)))))
                    ; (display i) (display ",") (display j) (display "    ") (display (make-rectangular (+ x (* xstep i)) (+ y (* ystep j)))) (newline)
                    ; (display (vector-ref vec 2)) (newline)
            )
        )
)



; (define (get-color-by-itr root itr max-itr)
;     (map (list-ref COLORS (index-of ROOTS root))
; )



; Given a function, f a beginning point z, aribtrary roots roots, a maximum number of iteration max-itr and the current iteration itr
; For 0 to itr
; Run newtons method
; Check for convergence
; If convergence, return color modified by iteration count
; If we are here, return black
(define (newtons-method-paint-by-iteration f df z roots max-itr itr)
    (cond 
        ((> itr max-itr)
            #f
        )

        ((not (empty? (filter (lambda (x) (not (boolean? x))) (within-threshold-to-roots z roots))))
            itr
        )

        (else
            (newtons-method-paint-by-iteration f df (- z (/ (f z) (df z))) roots max-itr (+ itr 1))
        )

    )

)



(display "Generating matrix")
(newline)
(define vector (gen-matrix RESOLUTION-X RESOLUTION-Y))

(display "Generating complex plane")
(newline)
(make-complex-plane vector XMIN YMIN (find-step XMIN XMAX RESOLUTION-X) (find-step YMIN YMAX RESOLUTION-Y))

; (display (vector-ref vector 0))

(display "Generating newton fractal (may take a long time)")
(newline)
(write-to-file 
    (vector-map (lambda (col) (vector-map (lambda (x) (newtons-method-paint-by-iteration FUNCTION DERIV x ROOTS MAX-ITR 0)) col)) vector)
    "out.txt" #:exists 'replace
)

(display "Finished")
