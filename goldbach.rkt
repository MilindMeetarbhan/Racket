;; (primes n) consumes a natural number and returns a list of primes that are less or equal to
;; that number
;;
;; Examples
(check-expect (primes 10) (list 2 3 5 7))
(check-expect (primes 3) (list 2 3))

;; primes: Nat -> (listof Nat) 
(define (primes n)
  (local [(define lst-of-num (build-list (- n 1) (lambda (x) (+ 2 x))))

          (define (remove-multiples n lst)
            (filter (lambda (z)
                      (not (= (remainder z n) 0))) lst))

          (define (build-primes lst acc)
            (cond [(empty? lst) acc]
                  [else (build-primes (remove-multiples (first lst) (rest lst))
                                      (append acc (list (first lst))))]))]
    (build-primes lst-of-num empty)))

;; Tests
(check-expect (primes 20) '(2 3 5 7 11 13 17 19))
(check-expect (primes 30) '(2 3 5 7 11 13 17 19 23 29))
(check-expect (primes 100) '( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))


;; (goldbach n) consumes an even natural number greater than 2 and produces a natural number
;; indicating the number of ways it can be written as the sum of two primes
;;
;; Examples

(check-expect (goldbach 10000) 127)
(check-expect (goldbach 14) 2)

;; goldbach: Nat -> Nat
(define (goldbach n)
  (local [(define prime-lst (primes n))
          
          (define (sub-n lst)
            (- n (first lst)))

          ;; prime? consumes a natural number n and checks whether it is in the list of primes
          ;; prime?: Nat -> Bool
          (define (prime? n)
            (member? n prime-lst))

          ;; (make-valid-pairs lst pairs) consumes a list of primes and an accumulator pairs,
          ;; and produces a list of all pairs of primes that can be summed up to n
          ;;
          ;; make-valid-pairs: (listof Nat) (listof Nat) -> (listof Nat)
          (define (make-valid-pairs lst pairs)
            (cond [(empty? lst) pairs]
                  [(and (>= (sub-n lst) (first lst))
                        (prime? (sub-n lst)))
                   (make-valid-pairs (rest lst) (cons (first lst) (cons (sub-n lst) pairs)))]
                  [else (make-valid-pairs (rest lst) pairs)]))
          
          (define number-of-ways
            (/ (length (make-valid-pairs prime-lst empty)) 2))
          ]
    number-of-ways))
