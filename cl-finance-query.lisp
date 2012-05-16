;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol to provide an interface for financial queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  An attempt to use generic functions/interface-passing style to
;;;  provide a protocol suitable for cl-<service>-finance libraries.


(defpackage :cl-finance-query
  (:use :common-lisp)
  (:export
   :current-price
   :historical-prices
   :historical-splits
   :company-immutables
   :company-current-statistics
   :company-historical-statistics))
(in-package :cl-finance-query)

(defun current-price (reader symbol-or-symbol-list &rest specifics)
  "`reader` denotes a funcallable to execute the 'current-price' from"
  (funcall reader symbol-or-symbol-list))

(defun historical-prices (reader symbol-or-symbol-list start-date end-date &rest specifics))
(defun historical-splits (reader symbol-or-symbol-list start-date end-date &rest specifics))
(defun company-immutables (reader symbol-or-symbol-list  &rest specifics))
(defun company-current-statistics (reader symbol-or-symbol-list  &rest specifics))
(defun company-historical-statistics (reader symbol-or-symbol-list start-date end-date &rest specifics))