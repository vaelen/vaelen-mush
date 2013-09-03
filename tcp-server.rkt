#lang racket
(provide (prefix-out tcp: serve))

(define (serve description port-number handler)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-number 5 #t))
    (define (start-server) 
      (printf "Starting ~a server on port ~a~n" description port-number)
      (server-loop))
    (define (server-loop)
      (accept-and-handle listener handler)
      (server-loop))
    (define t (thread start-server))
    (lambda ()
      (printf "Shutting down ~a server on port ~a~n" description port-number)
      (custodian-shutdown-all main-cust))))

(define (accept-and-handle listener handler)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handler in out)
              (close-input-port in)
              (close-output-port out)))))
  ; Watcher thread
  ;(thread (lambda ()
  ;          (sleep 10)
  ;          (custodian-shutdown-all cust))))

(define (handle-http in out)
  ; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))

(define (handle-telnet in out)
  (fprintf out "Hello, world!~n"))

(define (start-http)
  (serve "HTTP" 8080 handle-http))

(define (start-telnet)
  (serve "telnet" 2222 handle-telnet))
         
; (define stop-http (start-http))
; (define stop-telnet (start-telnet))