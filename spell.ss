
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2017                              *
; *  Student Version        
; * Arifur Rahman                  *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "test-dictionary.ss")

;; -----------------------------------------------------
;; -----------------------------------------------------
;; HELPER FUNCTIONS
; Flatten the list 
(define flatten
        ( lambda (l)
  (cond ((null? l) '())
        ((pair? l) (append (flatten (car l)) (flatten (cdr l))))
        (else (list l)))))

; bitvector for an individual function in hashfunctionlist
(define gen-Vector
  (lambda (hfunc dict)
    (if (null? dict)
        '()
       (cons ( hfunc (car dict) ) (gen-vector hfunc (cdr dict)))
     )
 ))

;Generate the complete bitvector combining bitvectors of all hash functions
(define gen-VectorList
  (lambda (hlist dict)
    (if (null? hlist)
        '()
        (cons (gen-vector (car hlist)  dict) (gen-vectorlist (cdr hlist) dict))
        )
    ))

;Return a binary list showing if the the key of the word exists in the bitvector i.e word exists in dictionary
(define spell-checker
  (lambda (hflist word bitvec)
    (if (null? hflist)
        '()
        (flatten (list (gen-checklist ((car hflist) word) bitvec) (spell-checker (cdr hflist) word bitvec)))     
       )
    ))

;Generates a check for individual hash functions in the list if key is in the bitvector
(define gen-CheckList
  (lambda (key bitvec)
    (if (null? bitvec)
         (list 0)
           (if (= key (car bitvec))
               (list 1)
               (gen-checklist  key (cdr bitvec))
            )
      )    
    ))

;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (if (null? w)
        5387
       (+ (* 31 (key(cdr w))) (ctv(car w))))
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))     = 154238504134
;;   (key '(w a y))         = 160507203 
;;   (key '(r a i n b o w)) = 148230379423562

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number

(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (k)
      (modulo (key k) size))
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (k)
     (floor (* size (- (* (key k) A) (floor ( * (key k) A))))))
))

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 700224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))
(define hashfl-4 (list hash-1 hash-2))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))     ==> 53236
;;  (hash-1 '(w a y))         ==> 23124 
;;  (hash-1 '(r a i n b o w)) ==> 17039 
;;
;;  (hash-2 '(h e l l o))     ==> 25588 
;;  (hash-2 '(w a y))         ==> 42552 
;;  (hash-2 '(r a i n b o w)) ==> 70913 
;;
;;  (hash-3 '(h e l l o))     ==> 415458.0 
;;  (hash-3 '(w a y))         ==> 390702.0 
;;  (hash-4 '(r a i n b o w)) ==> 503286.0 
;;
;;  (hash-4 '(h e l l o))     ==> 533.0
;;  (hash-4 '(w a y))         ==> 502.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda ( hashfunctionlist dict)
    (lambda (word)
       (if(null? hashfunctionlist)
          '()
          (equal? (reduce * (spell-checker hashfunctionlist word (flatten (gen-vectorlist hashfunctionlist dict))) 1) 1)
          )
)))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;(checker-1 '(a r i f u r)); ==> #f
;;(checker-3 '(h e l l o)) ;==> #t