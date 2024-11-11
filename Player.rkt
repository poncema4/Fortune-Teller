(require 2htdp/image)
(require 2htdp/universe)

(define ESCN-LEN 700) (define ESCN-HEIGHT 200)
(define ESCN-COLOR 'darkblue) (define TXT-SIZE 36)
(define TXT-COLOR 'gold) (define TXT-SIZE2 16) (define TXT-COLOR2 'pink)

(define INSTR-STR "Press q to quit or any other key to get a fortune")
(define DONE-STR "Bye...")
(define DENIED-STR "Connection denied: name is already in use.")

(define ESCN-BCKGRND (rectangle ESCN-LEN ESCN-HEIGHT 'solid ESCN-COLOR))
(define INSTR-IMG (text INSTR-STR TXT-SIZE2 TXT-COLOR2))
(define BYE-IMG (text DONE-STR TXT-SIZE TXT-COLOR))
(define RJCT-IMG (text DENIED-STR TXT-SIZE TXT-COLOR))

(define E-SCN
(place-image INSTR-IMG (/ ESCN-LEN 2) (* 0.8 ESCN-HEIGHT) ESCN-BCKGRND))
(define E-SCN2
(place-image BYE-IMG (/ ESCN-LEN 2) (* 0.3 ESCN-HEIGHT) ESCN-BCKGRND))
(define E-SCN3
(place-image RJCT-IMG (/ ESCN-LEN 2) (* 0.3 ESCN-HEIGHT) ESCN-BCKGRND))

;; A world is a string of length less than 45.
;; Sample worlds
(define W1 "You will live long and prosper.")
(define W2 "Logic is the beginning of wisdom, not the end.")
(define W3 "The trial never ends")
(define W4 DONE-STR) (define W5 DENIED-STR)
(define INIT-WORLD "")

;; A game key, gk, is either: 1. "q" 2. any other key
;; sample gk
(define QUIT-GK "q")
(define OTHR-GK "z")
#| Template for functions on a gk
;; gk . . . → . . . Purpose:
(define (f-on-gk a-gk . . .)
(if (key=? a-gk QUIT-GK)
. . .
. . .))
;; Sample expressions for f-on-gk
(define QGK-VAL . . .) (define OGK-VAL . . .)
;; Tests using sample computations for f-on-gk
(check-expect (f-on-gk QUIT-GK . . .) QGK-VAL)
(check-expect (f-on-gk OTHR-GK . . .) OGK-VAL) . . .
;; Tests using sample values for f-on-gk
(check-expect (f-on-gk . . . . . .) QGK-VAL) . . . |#

;; world → image Purpose: Draw the given world
(define (draw-world a-world)
  (place-image (text a-world TXT-SIZE TXT-COLOR)
               (/ ESCN-LEN 2) (* 0.3 ESCN-HEIGHT) E-SCN))

;; Sample expressions for draw-world
(define W1-IMG (place-image (text W1 TXT-SIZE TXT-COLOR)
                            (/ ESCN-LEN 2) (* 0.3 ESCN-HEIGHT) E-SCN))
(define W2-IMG (place-image (text W2 TXT-SIZE TXT-COLOR)
                            (/ ESCN-LEN 2) (* 0.3 ESCN-HEIGHT) E-SCN))
(define W3-IMG (place-image (text W3 TXT-SIZE TXT-COLOR)
                            (/ ESCN-LEN 2) (* 0.3 ESCN-HEIGHT) E-SCN))

;; Tests using sample expressions for draw-world
(check-expect (draw-world W1) W1-IMG)
(check-expect (draw-world W2) W2-IMG)
(check-expect (draw-world W3) W3-IMG)

;; Tests using sample values for draw-world
(check-expect (draw-world "Insufficient facts always invite danger.")
              .)
(check-expect (draw-world "Boldly go where no one has gone before." )
              .)

;; world key → package
;;Purpose: Create next world after key event
(define (process-key a-world a-key)
  (if (key=? a-key "q")
      (make-package DONE-STR 'bye)
      (make-package a-world 'send-fortune)))

;; Sample expressions for process-key
(define W1Q-PK (make-package DONE-STR 'bye)) ;; "q"
(define W2Q-PK (make-package DONE-STR 'bye)) ;; "q"
(define W2*-PK (make-package W2 'send-fortune)) ;; "*"
(define W1a-PK (make-package W1 'send-fortune)) ;; "a"

;; Tests using sample computations for process-key
(check-expect (process-key W1 "q") W1Q-PK)
(check-expect (process-key W2 "q") W1Q-PK)
(check-expect (process-key W2 "*") W2*-PK)
(check-expect (process-key W1 "a") W1a-PK)

;; Tests using sample values for process-key
(check-expect (process-key W3 "q")
              (make-package DONE-STR 'bye))
(check-expect (process-key W1 "z")
              (make-package W1 'send-fortune))

;; Sample tpm
(define CDEN 'connection-denied)
(define WTPM W3)

#| ;; tpm . . . → . . .
;; Purpose:
(define (f-on-tpm a-tpm . . .)
(if (eq? a-tpm 'connection-denied)
. . .
. . .))
;; Sample expressions for f-on-tpm
(define CDEN-VAL . . .)
(define WTFP-VAL . . .)
;; Tests using sample computations for f-on-tpm
(check-expect (f-on-tpm CDEN . . .) CDEN-VAL)
(check-expect (f-on-tpm WTPM . . .) WTPM-VAL)
;; Tests using sample values for f-on-tpm
(check-expect (f-on-tpm . . . . . .) . . .)|#

;; world tpm → world
;; Purpose: Create new world after receiving the given to player message
(define (process-message a-world a-tpm)
  (if (eq? a-tpm 'connection-denied)
      DENIED-STR
      a-tpm))

;; Sample expression for process-message
(define PM-DN DENIED-STR)
(define PM-W2 "Warp speed now")
(define PM-W3 "This far and no further")

;; Test using sample computation for process-message
(check-expect (process-message INIT-WORLD DENIED-STR) PM-DN)
(check-expect (process-message W2 "Warp speed now") PM-W2)
(check-expect (process-message W3 "This far and no further") PM-W3)

;; Test using sample value for process-message
(check-expect (process-message "Make it so" "Resistance is futile")
              "Resistance is futile")
(check-expect (process-message "It will all work out"
                               "Time to let it go")
              "Time to let it go")

;; world → Boolean
;; Purpose: Determine if the simulation has ended
(define (finished? a-world)
  (or (string=? a-world DONE-STR)
      (string=? a-world DENIED-STR)))

;; Sample expressions for finished?
(define NFNSHD (string=? W1 DONE-STR))
(define FNSHD (string=? W4 DONE-STR))
(define DND (string=? W5 DENIED-STR))
(define NDND (string=? W3 DENIED-STR))

;; Tests using sample computations for finished?
(check-expect (finished? W1) NFNSHD)
(check-expect (finished? W4) FNSHD)
(check-expect (finished? W5) DND)
(check-expect (finished? W3) NDND)

;; Tests using sample values for finished?
(check-expect (finished? DONE-STR) #true)
(check-expect (finished? "Stop being highly illogical") #false)
(check-expect (finished? DENIED-STR) #true)

;; world → image Purpose: To draw the last world
;; ASSUMPTION: Given world is either W4 or W5
(define (draw-final-world a-world)
  (if (string=? DONE-STR a-world)
      E-SCN2
      E-SCN3))

;; Sample expression for draw-final-world
(define DFW1 E-SCN2) ;; W4 player quit world
(define DFW2 E-SCN3) ;; W5 connection denied world

;; Tests using sample computations for draw-final-world
(check-expect (draw-final-world W4) DFW1)
(check-expect (draw-final-world W5) DFW2)

;; Tests using sample values for draw-final-world
(check-expect (draw-final-world W4)
              .)
(check-expect (draw-final-world W5)
              .)

;; A name is a string or a symbol
;; name → world
(define (run a-name)
  (big-bang INIT-WORLD
           (on-draw draw-world) (on-key process-key)
           (stop-when finished? draw-final-world)
           (on-receive process-message)
           (register LOCALHOST) (name a-name)))
