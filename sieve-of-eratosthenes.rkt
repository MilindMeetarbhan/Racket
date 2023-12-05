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
