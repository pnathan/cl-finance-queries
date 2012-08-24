;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol to provide an interface for financial queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  An attempt to use generic functions/interface-passing style to
;;;  provide a protocol suitable for cl-<service>-finance libraries.


;; TODO: provide an Endpoint Unsupported condition that can be raised.

(defpackage :cl-finance-query
  (:use :common-lisp :defobject)
  (:export
   :cl-finance-querier
   :with-querier
   :create
   :specialization-options
   :destroy


   ;; Pricing object
   :pricing
   ;; Pricing accessors
   :name
   :ask
   :bid
   :volume
   :short-ratio
   :financial-symbol
   :peg-ratio
   :pe-ratio
   :ebitda
   :year-high
   :year-low
   :days-high
   :days-low
   :dividend-yield
   :eps-estimate-next-quarter
   ;; Function to get pricing
   :current-price

   ;; Historical-Pricing object
   :historical-pricing
   ;; Accessors
   :date
   :open-price
   :high
   :low
   :close-price
   :volume
   :adjusted-close
   ;; Function to get historical pricing
   :historical-prices

   ;; Historical-split object
   :historical-split
   ;; Accessors
   :date
   :split-ratio
   ;; Function to get historical pricing
   :historical-splits

   ;; Object
   :company-immutable
   ;; Getter
   :company-immutables

   ;; Object
   :company-statistics
   ;; Getters
   :company-current-statistics
   :company-historical-statistics))
(in-package :cl-finance-query)

(ql:quickload :defobject)
(use-package :defobject)

(defclass cl-finance-querier () ()
  (:documentation "Classes that implement a finance data-gathering API
  should inherit from this class and implement the methods on the class.

The contract *is* that the calling systems will call the defgenerics
without respecting the individual requirements of the particular
endpoint *except* in the case of SPECIALIZATION-OPTIONS, which will
take options specific for the endpoint.
 "))


(defgeneric destroy (querier)
  (:documentation "Generic finalizer for a querier"))

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

(defobject pricing
  (name
   ask
   bid
   volume
   short-ratio
   financial-symbol
   peg-ratio
   pe-ratio
   ebitda
   year-high
   year-low
   days-high
   days-low
   dividend-yield
   eps-estimate-next-quarter)
  :documentation "A summary of information regarding pricing for a stock"
  :undecorated t)

(defgeneric current-price (reader symbol-or-symbol-list)
  (:documentation "Returns a PRICING object or list of PRICING objects"))

(defobject historical-pricing
    (financial-symbol
     date
     open-price
     high
     low
     close-price
     volume
     adjusted-close)
  :documentation "Summary information for a given time period"
  :undecorated t)

(defgeneric historical-prices (reader
			  symbol-or-symbol-list
			  start-date
			  end-date)
  (:documentation "Returns a list of HISTORICAL-PRICING objects"))


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
