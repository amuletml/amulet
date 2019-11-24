(declare (uses library))
(import scheme)
(import (chicken fixnum))
(import (chicken base))

(define *big-mersenne-prime* 2147483647)
(define *buckets* 15)

(define (hash-string str)
  (let ((len (string-length str)))
    (letrec ((go (lambda (idx acc)
                   (if (fx< idx len)
                       (let ((ch (string-ref str idx)))
                         (go (fx+ 1 idx) (fxmod (fx+ (fx* acc 31)
                                                     (char->integer ch))
                                                *big-mersenne-prime*)))
                       acc))))
      (go 0 1))))

(define (make-record-storage)
  (make-vector *buckets* '()))

(define (record-storage-bucket table key)
  (let ((hash (hash-string key)))
    (vector-ref table (fxmod hash *buckets*))))

(define (record-storage-insert! table key value)
  (let ((hash (hash-string key)))
    (vector-set! table
                 (fxmod hash *buckets*)
                 (cons (cons key value)
                       (record-storage-bucket table key)))))

(define (string-assoc alist key)
  (cond
    ((null? alist) #f)
    ((and (pair? alist)
          (string=? (caar alist) key))
     (car alist))
    ((pair? alist) (string-assoc (cdr alist) key))
    (else (void))))

(define (record-storage-ref table key)
  (let ((hash (hash-string key))
        (bucket (record-storage-bucket table key)))
    (cdr (string-assoc bucket key))))

(define (copy-record-storage old-table)
  (let ((new-table (make-record-storage)))
    (vector-copy! old-table new-table *buckets*)
    new-table))

(define (@@#-20 f x) (f x))

(define error#-14 error)

(define (lazy#-15 x)
  (vector #f x))

(define (force#-16 lazy)
  (if (vector-ref lazy 0)
      (vector-ref lazy 1)
      (begin
        (let ((thunk (vector-ref lazy 1)))
          (vector-set! lazy 1 (lambda () (error "Loop when forcing thunk")))
          (vector-set! lazy 1 (thunk (void)))
          (vector-set! lazy 0 #t)
          (vector-ref lazy 1)))))

(define (Cons#-21 x) (vector 'Cons#-21 x))
(define Nil#-22 'Nil#-22)

(define (:=#-23 ref val)
  (vector-set! ref 0 val)
  (void))

(define (!#-24 ref)
  (vector-ref ref 0))

(define (ref#-25 val)
  (vector val))

(define (string_value#-26 val) val)
(define (int_value#-27 val) val)

(define (extend_row#-28 key val rec)
  (let ((new-rec (copy-record-storage rec)))
    (record-storage-insert! rec key val)
    rec))

(define (restrict_row#-29 key rec)
  (let ((new-rec (make-record-storage))
        (value (record-storage-ref rec key)))
    (record-storage-insert! new-rec "_1" value)
    (record-storage-insert! new-rec "_2" rec)
    new-rec))

(define ($KnownString#-30 val) val)
(define ($KnownInt#-31 val) val)
(define ($RowCons#-32 val) val)

(define $Refl#-38 'Refl)

(define $Refl#-38 'Refl)

(define (String#-43 x) (vector 'String#-43 x))
(define (:<>:#-44 x) (vector ':<>:#-44 x))
(define (:<#>:#-45 x) (vector ':<#>:#-45 x))
(define (ShowType#-46 x) (vector 'ShowType#-46 x))

(define ($Typeable#-50 x) (x))

(define ($TypeableApp#-53 pair)
  (let ((ta (record-storage-ref pair "_1"))
        (tb (record-storage-ref pair "_2")))
    (string-append "(" ta ") :$ (" tb ")")))

(define ($TypeableKK#-54 name finger)
  (string-append name "#" (number->string finger)))

(define ($TypeRep#-51 x)
  (string-append (record-storage-ref x "name")
                 "#"
                 (record-storage-ref x "fingerprint")))

(define (type_of#-48 x) (lambda () x))

(define (eq_type_rep#-52 tr_a tr_b keq kne)
  (if (string=? tr_a tr_b)
      ((keq 'Refl) (void))
      (kne (void))))
