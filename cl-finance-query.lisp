;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol to provide an interface for financial queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  An attempt to use generic functions/interface-passing style to
;;;  provide a protocol suitable for cl-<service>-finance libraries.


;; TODO: provide an Endpoint Unsupported condition that can be raised.

(defpackage :cl-finance-query
  (:use :common-lisp )
  (:export
   :current-price
   :historical-prices
   :historical-splits
   :company-immutables
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

(defgeneric specialization-options (&rest specifics))

(defclass pricing ()
  (percent-change
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
