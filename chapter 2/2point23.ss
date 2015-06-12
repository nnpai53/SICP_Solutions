(define (for-each-new proc some-list)
  (cond ((null? some-list) #t)
      ((proc (car some-list)) 
       (for-each-new proc (cdr some-list)))))

(for-each-new (lambda (x) (newline) (display x))
          (list 57 321 88))