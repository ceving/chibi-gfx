(begin

(load "./chibi-xlib.so")

(define-syntax ->
  (syntax-rules ()
    ((-> arg)
     (let ((stderr (current-error-port))
           (value arg))
       (display ";; " stderr)
       (display 'arg stderr)
       (display " -> " stderr)
       (display value stderr)
       (newline stderr)
       value))))

(define-syntax display*
  (syntax-rules ()
    ((display* first)
     (display first))
    ((display* first second)
     (begin (display first)
            (display second)))
    ((display* first second ...)
     (begin (display first)
            (display* second ...)))))

;; (put 'with-x-display 'scheme-indent-function 1)
;; (font-lock-add-keywords 'scheme-mode '(("with-x-display" . font-lock-keyword-face)))
(define-syntax with-x-display
  (syntax-rules ()
    ((with-x-display (display-name connection) body ...)
     (let ((connection #f))
       (dynamic-wind
           (lambda ()
             (set! connection (XOpenDisplay display-name)))
           (lambda ()
             body
             ...)
           (lambda ()
             (if connection
                 (XCloseDisplay connection))))))
    ((with-x-display (connection) body ...)
     (with-x-display (#f connection) body ...))))

;; (put 'with-return 'scheme-indent-function 1)
;; (font-lock-add-keywords 'scheme-mode '(("with-return" . font-lock-keyword-face)))
(define-syntax with-return
  (syntax-rules ()
    ((with-return (return) body ...)
     (call-with-current-continuation
      (lambda (return)
        body
        ...)))))

;; (put 'event-loop 'scheme-indent-function 1)
(define-syntax event-loop
  (syntax-rules (else)
    ((event-loop ((event get-event)) (condition . body) ... (else . else-body))
     (let loop ()
       (let ((event get-event))
         (cond 
          (condition . body)
          ...
          (else . else-body)))
       (loop)))
    ((event-loop ((event get-event)) (condition . body) ...)
     (event-loop ((event get-event))
       (condition . body)
       ...
       (else (error "unhandled event"))))))

;;(event-loop (XNextEvent disp)
;;   ((Expose) (display "expose\n")))

(define (XNextEvent xdisplay) (cadr (XNextEvent% xdisplay)))

;;(define (XButtonPressedEvent? event) (= (XEventType event) XButtonPress))


(with-x-display (xdisplay)
  (with-return (return)
    (let ((xwindow (XCreateSimpleWindow
                    xdisplay
                    (XRootWindow xdisplay (XDefaultScreen xdisplay))
                    0 0 800 600 1 0 0))
          (xwmdelwinmsg (XInternAtom xdisplay "WM_DELETE_WINDOW" 0))
	  )
      (XSelectInput xdisplay xwindow (+ XButtonPressMask XExposureMask))
      (XMapWindow xdisplay xwindow)
      (XSetWMProtocols xdisplay xwindow xwmdelwinmsg 1)
      (event-loop ((xevent (XNextEvent xdisplay)))
	((XExposeEvent? xevent) (display "expose\n"))
        ((XButtonPressedEvent? xevent) (display "button-press\n"))
        ((XClientMessageEvent? xevent) (return 0))
	(else (display* "unknown event: " xevent)))
      )))
)
