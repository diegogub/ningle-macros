(defpackage ningle-macros
  (:use :cl
        :ningle)
  (:export
    :define-routes
    :redirect
    :with-status
    :set-header
    :set-cookie
    :set-content-type
    :query-param
    :param
    :header)
  (:nicknames "nimac"))
(in-package :ningle-macros)

(defmacro print-route (route)
  `(format t "~A || ~A => ~A ~C" ,(getf route :method) ,(getf route :path) ,(getf route :handler) #\linefeed))

;; (print-route (:path "/testing" :method :GET))

(defmacro define-routes (app routes &key (verbose nil))
  "Defines a group of ningle routes and print them while defining them."
  (let ((printers '())
        (definitions '()))
    (when verbose
      (loop for r in routes
        do (push `(print-route ,r) printers))
      (push '(format t "~C" #\linefeed) printers))

    (loop for r in routes
      do (push `(setf (ningle:route ,app ,(getf r :path) :method ,(getf r :method)) ,(getf r :handler)) definitions))

    (when printers
        (setf definitions (append printers definitions)))

    (push 'progn definitions)))

;(define-routes (make-instance 'ningle:app) ((:path "/test" :method :POST :handler #'test) 
                       ;(:path "/ping" :method :GET :handler #'test)) :verbose t)

(defmacro redirect (response url) "Redirect to other url"
  `(progn 
    (setf (lack.response:response-headers ,response)
      (append (lack.response:response-headers ,response)
              (list :location ,url)))
    (setf (lack.response:response-status ,response) 302)
    "redirect"))

(defmacro with-status (res status &rest body)
  "Executes body with one HTTP status"
  `(progn (setf (lack.response:response-status ,res) ,status)
      ,@body))

(defmacro query-param (req param)
  "Gets query parameter from request"
  `(progn (cdr (assoc ,param (lack.request:request-query-parameters ,req) :test 'string=))))

(defmacro param (name parameters)
  "Gets any parameter from request"
  `(progn (cdr (assoc ,name ,parameters :test 'string=))))

(defmacro header (req name)
  "Read Header value"
  `(gethash ,name (lack.request:request-headers ,req)))

(defmacro get-ip (req &key (proxy nil)) 
  "Returns IP from request"
  (if proxy
    `(header ,req "x-forwarded-for")
   `(progn (lack.request:request-remote-addr ,req))))

(defmacro set-content-type (res content-type)
  `(setf (lack.response:response-headers ,res)
      (append (lack.response:response-headers ,res)
              (list :content-type ,content-type))))

(defmacro set-cookie (res value)
  `(setf (lack.response:response-headers ,res)
      (append (lack.response:response-headers ,res)
              (list :set-cookie ,value))))

(defmacro set-header (res header value)
  `(setf (lack.response:response-headers ,res)
      (append (lack.response:response-headers ,res)
              (list ,header ,value))))

(defmacro is-method (req m)
  "Checks request method"
    `(eql (lack.request:request-method ,req) ,m))

(defun real-ip (env)
  (let ((proxy-header (gethash "x-forwarded-for" (getf env :headers))))
    (if proxy-header
        proxy-header
        (getf env :REMOTE-ADDR))))

(defvar *maxminddb* "/srv/GeoIP2-City.mmdb")

(defvar *maxmind-mw*
  (lambda (app)
    (lambda (env)
      (let* ((geo (locate-ip *maxminddb* (real-ip env)))
             (country (cdr (assoc :country geo)))
             (city (cdr (assoc :city geo)))
             (continent(cdr (assoc :continent geo)))
             (timezone (cdr (assoc :timezone geo))))
        (when country
          (setf (gethash "x-country" (getf env :headers) ) country))

        (when city 
          (setf (gethash "x-city" (getf env :headers) ) city))

        (when continent
          (setf (gethash "x-continent" (getf env :headers) ) continent))

        (when timezone
          (setf (gethash "x-timezone" (getf env :headers) ) timezone)))
      (let ((res (funcall app env)))
        res))))
