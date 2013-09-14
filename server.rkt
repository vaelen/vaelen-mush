#lang racket
(require racket/date)

(provide
 (all-defined-out)
 (struct-out player)
 (struct-out room)
 (struct-out door)
 (struct-out command))

; ########## Data Structures ##########

(struct player (name room in out remote-ip remote-port) #:mutable #:prefab)
(struct room (id name description inventory doors) #:mutable #:prefab)
(struct door (names description dest-room) #:mutable #:prefab)
(struct command (name description function) #:mutable #:prefab)

(define players (make-hash))
(define rooms (make-hash))
(define commands (make-hash))
(define world #hash(
                    ('players . players)
                    ('rooms . rooms)
                    ('commands . commands)))

(define (get-player player-id)
  (hash-ref players player-id #f))

(define (get-room room-id)
  (hash-ref rooms room-id #f))

(define (get-command name)
  (hash-ref commands name #f))

; ########## Logging Code ##########

(define LOG-PORT (current-output-port))
(define LOG-UTC #f)

(define (init-logging)
  ; TODO: Load from config file
  (date-display-format 'rfc2822))

(define (log message [level 'info])
    (fprintf LOG-PORT "[~a](~a) ~a~n" (date->string (current-date) #t) level message))

; ########## TCP Server Code ##########

(define (serve description port-number handler)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-number 5 #t))
    (define (start-server)
      (log (format "Starting ~a server on port ~a." description port-number))
      (server-loop))
    (define (server-loop)
      (accept-and-handle listener handler)
      (server-loop))
    (define t (thread start-server))
    (lambda ()
      (log (format "Shutting down ~a server on port ~a." description port-number))
      (custodian-shutdown-all main-cust)
      #t)))

(define (accept-and-handle listener handler)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (define-values (local-ip local-port remote-ip remote-port) (tcp-addresses in #t))
    (log (format "Incoming connection from ~a:~a." remote-ip remote-port))
    (thread (lambda ()
              (handler in out)
              (close-input-port in)
              (close-output-port out)
              (log (format "Connection from ~a:~a closed." remote-ip remote-port))))))

; ########## Game Server Code ##########

(define (player-connected current-player)
  (hash-set! players (player-name current-player) current-player)
  (log (format "Player '~a' has logged on from ~a:~a."
               (player-name current-player)
               (player-remote-ip current-player)
               (player-remote-port current-player)))
  (send-message-to-player current-player (format "Welcome, ~a!~n" (player-name current-player)))
  (send-message-to-room (player-room current-player) (format "~a has logged on." (player-name current-player))))

(define (player-disconnected current-player)
  (cond
    [(and (player? current-player) (hash-has-key? players (player-name current-player)))
     (hash-remove! players (player-name current-player))
     (log (format "Player '~a' has logged off from ~a:~a."
                  (player-name current-player)
                  (player-remote-ip current-player)
                  (player-remote-port current-player)))
     (send-message-to-room (player-room current-player) (format "~a has logged off." (player-name current-player)))])
  #f)

(define (read-trimmed-line in current-player)
  (if (port-closed? in)
      (player-disconnected current-player)
      (let ([line (read-line in)])
        (if (string? line)
            (string-trim line)
            (player-disconnected current-player)))))

(define (parse-command-helper line current-player)
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
      [else (values #f #f)])))

(define (login in out)
  (cond [(not (port-closed? out))
         (write-and-flush out "Name: ")
         (let ([name (read-trimmed-line in #f)])
           (cond [(and (string? name) (not (equal? "" name)) (not (hash-has-key? players name))) name]
                 [(not name) #f]
                 [else (login in out)]))]
        [else #f]))

(define (write-and-flush out message)
  (display message out)
  (flush-output out))

(define (move-player current-player new-room-id [direction #f] [source-direction #f])
  (let ([old-room-id (player-room current-player)])
    (set-player-room! current-player new-room-id)
    (display-room current-player (player-room current-player))
    (if direction
        (send-message-to-room old-room-id (format"~a has gone ~a." (player-name current-player) direction))
        (send-message-to-room old-room-id (format"~a has gone." (player-name current-player))))
    (if source-direction
        (send-message-to-room new-room-id (format"~a has arrived from the ~a." (player-name current-player) source-direction))
        (send-message-to-room new-room-id (format"~a has arrived." (player-name current-player))))))

(define (generate-room-description current-room current-player)
  (format "~a~n~a~n~nExits:~n~a~n~a"
          (room-name current-room)
          (room-description current-room)
          (string-join (map (lambda (x) (format "\t~a:\t~a~n"
                                                (first (door-names x))
                                                (door-description x)))
                            (room-doors current-room)))
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

(define (display-prompt current-player)
  (cond [(not (port-closed? (player-out current-player)))
         (let ([prompt-string "> "])
           (write-and-flush (player-out current-player) prompt-string))]
        [else #f]))

(define (game-loop current-player)
  (display-prompt current-player)
  (let-values ([(cmd args) (parse-command current-player)])
    (cond
      [(command? cmd)
       ((command-function cmd) current-player args)
       (game-loop current-player)]
      [else #f])))

(define (handle-telnet in out)
  (let ([name (login in out)])
    (if name
        (let-values ([(local-ip local-port remote-ip remote-port) (tcp-addresses in #t)])
          (let ([current-player (player name 0 in out remote-ip remote-port)])
            (player-connected current-player)
            (display-room current-player (player-room current-player))
            (game-loop current-player)))
          #f)))


(define stop-mush (lambda (x) #f))

(define (start-mush [port 2222] [title "mush"])
  (set! stop-mush (serve title port handle-telnet)))

; ########## Command Helper Functions ##########

(define (find-exit current-player direction)
  (let ([current-room (get-room (player-room current-player))]
        [lcase-direction (string-downcase direction)])
    (if (room? current-room)
        (findf
         (lambda (current-door)
           (findf
            (lambda (current-name)
              (equal? (string-downcase current-name) lcase-direction))
            (door-names current-door)))
         (room-doors current-room))
        #f)))

(define (find-entrance current-player exit)
  (let ([current-room (get-room (player-room current-player))]
        [new-room #f]
        [old-room-id #f])
    (if (and (room? current-room) (door? exit))
        (begin
          (set! old-room-id (room-id current-room))
          (set! new-room (get-room (door-dest-room exit)))
          (if (room? new-room)
              (findf
               (lambda (current-door)
                 (equal? old-room-id (door-dest-room current-door)))
               (room-doors new-room))
              #f))
        #f)))

; ########## Commands ##########

; TODO: Make the list of commands configurable
(hash-set! commands "noop"
          (command "noop"
                   "Do nothing."
                   (lambda (current-player args) #f)))

(hash-set! commands "say"
          (command "say"
                   "Say something to the room."
                   (lambda (current-player args)
                     (send-message-to-room
                      (player-room current-player)
                      (format "~a: ~a" (player-name current-player) (string-join args))))))
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
                              (cond [(command? cmd)
                                     (send-message-to-player current-player
                                                             (format "~a: ~a"
                                                                     (command-name cmd)
                                                                     (command-description cmd)))]))]))))
(hash-set! commands "h" (hash-ref commands "help"))

(hash-set! commands "go"
          (command "go"
                   "Go somewhere."
                   (lambda (current-player args)
                     (let ([exit (find-exit current-player (first args))]
                           [entrance #f])
                       (set! entrance (find-entrance current-player exit))
                       (if (door? exit)
                           (if (door? entrance)
                               (move-player
                                current-player
                                (door-dest-room exit)
                                (first (door-names exit))
                                (first (door-names entrance)))
                               (move-player
                                current-player
                                (door-dest-room exit)
                                (first (door-names exit))))
                           (send-message-to-player current-player "You can't go that way."))))))
(hash-set! commands "g" (hash-ref commands "go"))
(hash-set! commands "walk" (hash-ref commands "go"))
(hash-set! commands "run" (hash-ref commands "go"))
(hash-set! commands "climb" (hash-ref commands "go"))
(hash-set! commands "move" (hash-ref commands "go"))

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

; ########## Rooms ##########

; TODO: This should be configurable
(hash-set! rooms 0 (room 0
                         "Entrance Hall"
                         "This is where new players appear before they enter the game."
                         '()
                         (list (door '("north" "n") "A path to the north leads to town." 1))))

(hash-set! rooms 1 (room 1
                         "Empty Field"
                         "This large empty field contains a path that leads to and from town."
                         '()
                         (list
                          (door '("north" "n") "A path to the north leads to town." 2)
                          (door '("south" "s") "A path to the south leads to the entrance hall." 0))))

(hash-set! rooms 2 (room 2
                         "Town Gate"
                         "You are standing in front of the town gate.  A large wooden wall extends around the town in both directions.  The gate is shut tight."
                         '()
                         (list (door '("south" "s") "A path to the south leads to the entrance hall." 1))))

; ########## Data Storage and Retrieval ##########

; TODO: Lots to do here.  Eventually these methods will persist the state of the world to disk.
(define (save-world)
  (print-struct #t)
  (print-hash-table #t)
  (print-as-expression #f)
  (print-graph #t)
  (write world))

(define (load-world)
  (void))

; ########## Main ##########
(define (main)
  (init-logging)
  (start-mush)
  #t)

(void (main) (save-world))
