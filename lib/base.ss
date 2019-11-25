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

(define (make-record-storage #!optional (n-buckets *buckets*))
  (cons n-buckets
        (make-vector n-buckets (cons 0 '()))))

(define (record-storage-bucket table hash)
  (vector-ref (cdr table) (fxmod hash (car table))))

(define (record-storage-set! table key value)
  (let* ((hash (hash-string key))
         (bucket (record-storage-bucket table hash))
         (n-buckets (car table))
         (n-entries (car bucket))
         (entries-alist (cdr bucket)))
    (cond
      ((> (+ 1 n-entries) n-buckets)
       ; If there are going to be more entires in this bucket than there
       ; are buckets in total
       (let* ((new-len (inexact->exact
                         (ceiling (* 1.5 n-buckets))))
              (new-table (vector-resize (cdr table) new-len (cons 0 '()))))
         ; Make a new table
         (vector-set! new-table (fxmod hash n-buckets) (cons 0 '()))
         ; Update the old table to have the new length and point to the
         ; new array
         (set-car! table new-len)
         (set-cdr! table new-table)
         ; Redistribute the entries already in this bucket
         (let loop ((alist entries-alist))
           (if (null? alist)
             (void)
             (begin
               (record-storage-set! (cons new-len new-table) (caar alist) (cdar alist))
               (loop (cdr alist)))))
         ; Insert the new entry into the new table
         (record-storage-set! (cons new-len new-table) key value)))
      (else
        ; Just add it to the association list lol
        (vector-set! (cdr table)
                     (fxmod hash n-buckets)
                     (cons (+ 1 n-entries)
                           (cons (cons key value)
                                 entries-alist)))))))

(define (string-assoc alist key)
  (cond
    ((null? alist) #f)
    ((and (pair? alist)
          (string=? (caar alist) key))
     (car alist))
    ((pair? alist) (string-assoc (cdr alist) key))
    (else (void))))

(define (record-storage-ref table key)
  (let* ((hash (hash-string key))
         (bucket (record-storage-bucket table hash)))
    (let ((elem (string-assoc (cdr bucket) key)))
      (when elem (cdr elem)))))

(define (copy-record-storage old-table #!optional (more-buckets 0))
  (let ((new-table (make-record-storage (+ (car old-table) more-buckets)))
        (old-length (car old-table))
        (old-vector (cdr old-table)))
    (if (= 0 more-buckets)
      (vector-copy old-vector (car new-table))
      (let loop ((idx 0))
        (unless (>= idx old-length)
          (map (lambda (p)
                 (record-storage-set! new-table (car p) (cdr p)))
               (cdr (vector-ref old-vector idx)))
          (loop (+ 1 idx)))))
    new-table))

(define (@@#-20 f x) (f x))

(define |id#-14| error) ; error

(define (|id#-15| x) ; lazy
  (vector #f x))

(define (|id#-16| lazy) ; force
  (if (vector-ref lazy 0)
      (vector-ref lazy 1)
      (begin
        (let ((thunk (vector-ref lazy 1)))
          (vector-set! lazy 1 (lambda () (error "Loop when forcing thunk")))
          (vector-set! lazy 1 (thunk (void)))
          (vector-set! lazy 0 #t)
          (vector-ref lazy 1)))))

(define (|id#-21| x) (vector '|id#-21| x))
(define |id#-22| (vector '|id#-22|))

(define (|id#-23| ref val) ; :=
  (vector-set! ref 0 val)
  (void))

(define (|id#-24| ref) ; !
  (vector-ref ref 0))

(define (|id#-25| val) ; ref
  (vector val))

(define (|id#-26| val) val)
(define (|id#-27| val) val)

(define (|id#-28| key val rec)
  (let ((new-rec (copy-record-storage rec 1)))
    (record-storage-set! rec key val)
    rec))

(define (|id#-29| key rec)
  (let ((new-rec (make-record-storage 2))
        (value (record-storage-ref rec key)))
    (record-storage-set! new-rec "_1" value)
    (record-storage-set! new-rec "_2" rec)
    new-rec))

(define ($|id#-30| val) val)
(define ($|id#-31| val) val)
(define ($|id#-32| val) val)

(define $|id#-38| 'Refl)

(define $|id#-38| 'Refl)

(define (|id#-43| x) (vector '|id#-43| x))
(define (:<>:#-44 x) (vector ':<>:#-44 x))
(define (:<#>:#-45 x) (vector ':<#>:#-45 x))
(define (|id#-46| x) (vector '|id#-46| x))

(define (|id#-50| x) (x (void)))

(define (|id#-53| pair)
  (let ((ta (record-storage-ref pair "_1"))
        (tb (record-storage-ref pair "_2")))
    (string-append "(" ta ") :$ (" tb ")")))

(define (|id#-54| name finger)
  (string-append name "#" (number->string finger)))

(define (|id#-51| x)
  (string-append (record-storage-ref x "name")
                 "#"
                 (number->string (record-storage-ref x "fingerprint"))))

(define (|id#-48| x) (lambda () x))

(define (|id#-52| tr_a tr_b keq kne)
  (if (string=? tr_a tr_b)
      ((keq 'Refl) (void))
      (kne (void))))
