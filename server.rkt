#lang racket
(require "tcp-server.rkt")

(provide 
 (all-defined-out)
 (struct-out player)
 (struct-out room)
 (struct-out door)
 (struct-out command))

(struct player (name room in out) #:mutable)
(struct room (id name description inventory doors) #:mutable)
(struct door (names description) #:mutable)
(struct command (name description function) #:mutable)

(define players (make-hash))
(define rooms (make-hash))
(define commands (make-hash))

(define (get-player player-id)
  (hash-ref players player-id #f))

(define (get-room room-id)
  (hash-ref rooms room-id #f))

(define (get-command name)
  (hash-ref commands name #f))

(hash-set! rooms 'main (room 'main 
      "Entrance Hall" 
      "This is where new players appear before they enter the game."
      '()
      (list (door '("north" "n") "A path to the north leads to town."))))

(define (player-disconnected current-player)
  (cond
    [(player? current-player)
     (hash-remove! players (player-name current-player))
     (send-message-to-room (player-room current-player) (format "~a has logged out." (player-name current-player)))])
  #f)

(define (read-trimmed-line in current-player)
         (let ([line (read-line in)])
           (if (string? line)
               (string-trim line)
               (player-disconnected current-player))))

(define (parse-command-helper line current-player)
  ; TODO: Make this a more complicated parser
  (let ([args (string-split line)]
        [name ""]
        [cmd #f])
    (set! name (first args))
    (set! args (rest args))
    (set! cmd (or (get-command name) (get-command "noop")))
    (values cmd args)))

(define (parse-command current-player)
  (let ([line (read-trimmed-line (player-in current-player) current-player)])
    (cond 
      ; Command
      [(and (string? line) (not (equal? "" line))) (parse-command-helper line current-player)]
      ; Empty String
      [(string? line) (values (get-command "noop") '())]
      ; Disconnect
      [else (values(#f #f))])))

(define (login in out)
  (fprintf out "Name: ")
  (flush-output out)
  (let ([name (read-trimmed-line in #f)])
    (cond [(and (string? name) (not (equal? "" name)) (not (hash-has-key? players name))) name]
          [(not name) #f]
          [else (login in out)])))
  
(define (write-and-flush out message)
  (display message out)
  (flush-output out))

(define (move-player current-player new-room-id direction)
  (let ([old-room-id (player-room current-player)])
    (set-player-room! current-player new-room-id)
    (cond [direction
           (send-message-to-room old-room-id format("~a has gone ~a"))
           (send-message-to-room new-room-id format("~a has arrived."))]
          [else
           (send-message-to-room old-room-id format("~a has gone."))
           (send-message-to-room new-room-id format("~a has arrived."))])))

(define (generate-room-description current-room current-player)
  (format "~a~n~a~n~nExits:~n~a~n~a" 
          (room-name current-room) 
          (room-description current-room)
          (string-join (map (lambda (x) (format "\t~a:\t~a~n" (first (door-names x)) (door-description x))) (room-doors current-room)))
          (string-join (map (lambda (x) (format "~a is here.~n" (player-name x))) 
                            (filter (lambda (x) 
                                      (and (equal? (room-id current-room) (player-room x))
                                           (not (equal? (player-name current-player) (player-name x))))) 
                                    (hash-values players))))))

(define (display-room current-player room-id)
  (let ([current-room (get-room room-id)])
    (cond [(room? current-room)
            (send-message-to-player current-player (generate-room-description current-room current-player))]
           [else
            (send-message-to-player current-player "You have no idea where you are!")])))

(define (send-message-to-player current-player message)
    (if (port-closed? (player-out current-player))
        (player-disconnected current-player)
        (write-and-flush (player-out current-player) (format "~a~n" message))))

(define (send-message-to-room room-id message)
  (for ([current-player (hash-values players)])
    (cond [(equal? room-id (player-room current-player))
           (send-message-to-player current-player message)])))

(define (game-loop current-player)
  (fprintf (player-out current-player) "> ")
  (flush-output (player-out current-player))
  (let-values ([(cmd args) (parse-command current-player)])
    (cond 
      [(command? cmd) 
       ((command-function cmd) current-player args)
       (game-loop current-player)]
      [else #f])))

(define (handle-telnet in out)
  (let ([name (login in out)])
    (if name
        (let ([current-player (player name 'main in out)])
          (hash-set! players (player-name current-player) current-player)
          (fprintf out "Welcome, ~a!~n" (player-name current-player))
          (send-message-to-room (player-room current-player) (format "~a has logged on." (player-name current-player)))
          (display-room current-player (player-room current-player))
          (game-loop current-player))
        #f)))

(define (start-mush [port 2222] [title "mush"])
  (tcp:serve title port handle-telnet))

; TODO: Make the list of commands configurable
(hash-set! commands "noop"
          (command "noop"
                   "Do nothing."
                   (lambda (current-player args) #f)))

(hash-set! commands "say"
          (command "say"
                   "Say something to the room."
                   (lambda (current-player args)
                     (send-message-to-room (player-room current-player) (format "~a: ~a" (player-name current-player) (string-join args))))))
(hash-set! commands "s" (hash-ref commands "say"))
(hash-set! commands "'" (hash-ref commands "say"))
(hash-set! commands "\"" (hash-ref commands "say"))

(hash-set! commands "look"
          (command "look"
                   "Look around."
                   (lambda (current-player args)
                     (display-room current-player (player-room current-player)))))
(hash-set! commands "l" (hash-ref commands "look"))

(hash-set! commands "help"
          (command "h"
                   "Get help."
                   (lambda (current-player args)
                     (cond [(empty? args) (send-message-to-player current-player "Commands: say, look, go, who, help, quit")]
                           [else
                            (let ([cmd (get-command (first args))])
                              (cond [(command? cmd) (send-message-to-player current-player (format "~a: ~a" (command-name cmd) (command-description cmd)))]))]))))
(hash-set! commands "h" (hash-ref commands "help"))

(hash-set! commands "go"
          (command "go"
                   "Go somewhere."
                   (lambda (current-player args)
                     (send-message-to-player current-player "You can't go anywhere yet."))))
(hash-set! commands "g" (hash-ref commands "go"))
(hash-set! commands "walk" (hash-ref commands "go"))
(hash-set! commands "run" (hash-ref commands "go"))
(hash-set! commands "climb" (hash-ref commands "go"))

(hash-set! commands "quit"
          (command "quit"
                   "Log off."
                   (lambda (current-player args)
                     (send-message-to-player current-player "Goodbye!")
                     (close-input-port (player-in current-player))
                     (close-output-port (player-out current-player))
                     (player-disconnected current-player))))
(hash-set! commands "q" (hash-ref commands "quit"))

(hash-set! commands "who"
          (command "who"
                   "See who's online."
                   (lambda (current-player args)
                     (send-message-to-player 
                      current-player 
                      (format "Players Online:~nName\tLocation~n------------------------------~n~a" 
                              (string-join (map (lambda (x) 
                                                  (format "~a\t~a~n" 
                                                          (player-name x)
                                                          (room-name (get-room (player-room x))))) 
                                                (sort 
                                                 (hash-values players)
                                                 (lambda (x y) (string<? (player-name x) (player-name y)))))))))))
(hash-set! commands "w" (hash-ref commands "who"))

(define stop-mush (start-mush))