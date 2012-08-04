;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol to provide an interface for financial queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  An attempt to use generic functions/interface-passing style to
;;;  provide a protocol suitable for cl-<service>-finance libraries.


;; TODO: provide an Endpoint Unsupported condition that can be raised.

(defpackage :cl-finance-query
  (:use :common-lisp )
  (:export
   :cl-finance-querier
   :with-querier
   :create
   :specialization-options
   :destroy

   :pricing
   :current-price

   :historical-pricing
   :historical-prices

   :historical-split
   :historical-splits

   :company-immutable
   :company-immutables

   :company-statistics
   :company-current-statistics
   :company-historical-statistics))
(in-package :cl-finance-query)

(defclass cl-finance-querier () ()
  (:documentation "Classes that implement a finance data-gathering API
  should inherit from this class and implement the methods on the class.

The contract *is* that the calling systems will call the defgenerics
without respecting the individual requirements of the particular
endpoint *except* in the case of SPECIALIZATION-OPTIONS, which will
take options specific for the endpoint.
 "))


(defgeneric destroy (querier) (:documentation "Generic finalizer for a querier"))

(defmacro with-querier ((obj-name type options)
                        &body body)
  ;; Prep a result variable
  (let ((result-name (gensym)))
    `(let ((,obj-name (make-instance ,type))
           (,result-name))
       ;; customize the object.
       (specialization-options ,obj-name ,options)
       ;; Pick up the result from the last form in progn
       (setf ,result-name
             (progn ,@body))
       ;; Destructor
       (destroy ,obj-name)
       ,result-name)))

(defgeneric specialization-options (object &rest specifics))

(defclass pricing ()
  (ask
   bid
   volume
   short-ratio
   symbol
   pegratio
   peratio
   ebitda
   year-high
   year-low
   days-high
   days-lo
   dividend-yield
   eps-estimate-next-quarter)
  (:documentation "A summary of information regarding pricing for a stock"))

(defgeneric current-price (reader symbol-or-symbol-list)
  (:documentation "Returns a PRICING object or list of PRICING objects"))

(defclass historical-pricing ()
    (date
     open
     high
     low
     close
     volume
     adjusted-close)
  (:documentation "Summary information for a given time period"))

(defgeneric historical-prices (reader
			  symbol-or-symbol-list
			  start-date
			  end-date)
  (:documentation "Returns a list of HISTORICAL-PRICING  objects"))


(defclass historical-split () (date split-ratio))

(defgeneric historical-splits (reader
			  symbol-or-symbol-list
			  start-date
			  end-date))

;; TODO
(defclass company-immutable () ())
(defgeneric company-immutables (reader
			   symbol-or-symbol-list))


(defclass company-statistics () ())
;; TODO
(defgeneric company-current-statistics (reader
				   symbol-or-symbol-list))

;; TODO
(defgeneric company-historical-statistics (reader
				      symbol-or-symbol-list
				      start-date
				      end-date))
