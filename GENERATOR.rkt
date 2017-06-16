#lang racket
(require graph 2htdp/universe 2htdp/image "listofnames.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; part of generation
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define RAWCOUNT 100)
(define SYSDENS (* 1.2 (expt (/ 1 RAWCOUNT) (/ 1 3))))
(define KPD 0.5)
(define MINNEIG 4)
(define MAXNEIG 6)
(define MAXNEIGONEPLANET 11)
(define (generate_system) 
(define c (build-list RAWCOUNT (lambda (x) (list x (list (random)
                                     (random)
                                     (random))))))


(define (disfromcenter a)
  (define len 0)
  (for-each (lambda (x) (set! len (+ len (sqr (- x 0.5))))) a)
  (< (sqrt len) 0.5))

(define (disfromplanet a b)
  (define len 0)
  (for-each (lambda (x)
              (set! len (+ len
                           (sqr (- (list-ref a x)
                                   (list-ref b x))))))
            (build-list 3 values))
  (< (sqrt len) SYSDENS))

(define system (filter-map (lambda (x) (and (disfromcenter (second x)) x)) c))

;;(length system)

(define (checkconects a)
  (define b (list))
  (for-each (lambda (x) (set! b (append b (connectone x (remove x a))))) a)
  b)

(define (connectone x a)
  (define b (list))
  (for-each (lambda (y) (if (disfromplanet (second x) (second y))
                            (set! b (append b (list (list (first x) (first y)))))
                            (set! b (append b (list)))))
            a)  
  b)

(define connect (checkconects system))
;;(printf "On average each planet has ~a neighbors\n" (exact->inexact (/ (length connect) (length system))))

;;(for-each (lambda (x) (printf "~a -> ~a;\n" (first x) (second x))) connect)
  (printf "attempt\n")

(define (checkallconnect a)
  (define maxneig true)
  (define gr (unweighted-graph/directed a))
  (for-each (lambda (x) (if (> (length (get-neighbors gr x)) MAXNEIGONEPLANET) (set! maxneig false) (void))) (get-vertices gr))
  (and maxneig (= 1 (length (scc gr)))))

(define b (list))
(for-each (lambda (x) (set! b (append b (list (first x) (second x))))) connect)
;;(length b)
;;(length connect)
;;(length system)
(set! b (remove-duplicates b))
;;(length b)
(if (and (= (length b) (length system))
         (> (length b) (* RAWCOUNT KPD))
         (< (/ (length connect) (length system)) MAXNEIG)
         (> (/ (length connect) (length system)) MINNEIG)
         (checkallconnect connect))
    (unweighted-graph/directed connect)
    (generate_system))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; big-bang component part
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define MS (empty-scene 1000 600 "white"))
(define SMAIN (empty-scene 600 600 "midnightblue"))
(define SNEIG (empty-scene 200 600 "midnightblue"))
(define SSTAT (empty-scene 200 600 "midnightblue"))
(define (render sta)
  (define b " ")
  (for-each (lambda (x) (set! b (string-append b (number->string x) " "))) (get-neighbors (state-graph sta) (state-planet sta)))
  (place-image
   (text b 60 "red")
   400 350
  (place-image
   (text "you can fly to planets:"
         60 "red")
   400 250
  (place-image
   (text (string-append "fly to planet "
                        (number->string (list-ref (get-neighbors (state-graph sta) (state-planet sta))
                                                  (state-options sta))))
         60 "red")
   400 450
  (place-image (text (string-append "now you on planet "
                                    (number->string (state-planet sta))) 60 "red")
   400 150 MS)))))

(define (planetname a) (list-ref listofnames a))

(define (renderneig sta)
  (define img (place-image (text "Nearest planets" 20 "green") 100 25 SNEIG))
  (define counter 75)
  (for-each (lambda (x) (set! img (place-image (text (planetname x) 20 "white") 100 counter img)) (set! counter (+ counter  50))) (get-neighbors (state-graph sta) (state-planet sta)))
  img)
(define (renderstat sta)
  (define img (place-image (text "Status" 20 "green") 100 25 SSTAT))
  (set! img (place-image (text "Healthy" 20 "white") 100 75 img))
  (set! img (place-image (text "In bag:" 20 "green") 100 125 img))
  (set! img (place-image (text "C21H23NO5 50 kg" 20 "white") 100 175 img))
  (set! img (place-image (text "Мивина 31 pieces" 20 "white") 100 225 img))
  (set! img (place-image (text "Socks 3 pieces" 20 "white") 100 275 img))
  (set! img (place-image (text "Tasks:" 20 "green") 100 325 img))
  (set! img (place-image (text "Kill Mother-in-law" 20 "white") 100 375 img))
  img)
(define (rendermain sta)
  (define img SMAIN)
  (set! img (place-image (text (string-append "The planet " (planetname (state-planet sta))) 50 "white") 300 50 img))
  (set! img (place-image (text (string-append "Fly to: " (planetname (list-ref (get-neighbors (state-graph sta) (state-planet sta)) (state-options sta)))) 20 "white") 300 125 img))
  img)
(define (render2 sta)
  (place-image (renderneig sta) 100 300 (place-image (renderstat sta) 900 300 (place-image (rendermain sta) 500 300 MS))))



(define (keypress sta ke) (cond
                            [(key=? ke "right") (if (= (+ (state-options sta) 1) (length (get-neighbors (state-graph sta) (state-planet sta))))
                                                    (state (state-planet sta) 0 (state-graph sta))
                                                    (state (state-planet sta) (+ 1 (state-options sta)) (state-graph sta)))]
                            [(key=? ke "left") (if (= (- (state-options sta) 1) -1)
                                                    (state (state-planet sta) (- (length (get-neighbors (state-graph sta) (state-planet sta))) 1) (state-graph sta))
                                                    (state (state-planet sta) (- (state-options sta) 1) (state-graph sta)))]
                            [(key=? ke " ") (state (list-ref (get-neighbors (state-graph sta) (state-planet sta)) (state-options sta)) 0 (state-graph sta))]
                            [else sta]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; main part
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define g (generate_system))
(printf (graphviz g))
(struct state (planet options graph))
(define sta (state (first (get-vertices g)) 0 g))
;;(define MAX (length (get-neighbors (state-graph sta) (state-planet sta))))
(big-bang sta (to-draw render2)
          (on-key keypress)
          )




