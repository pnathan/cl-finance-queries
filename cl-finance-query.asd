

(asdf:defsystem #:cl-finance-query
  :components ((:file "cl-finance-query"))
  :depends-on (#:defobject)             ;pnathan personal project
  :name "cl-finance-query"
  :version "0.1"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :license "LLGPL"
  :description "Protocol for financial queries"
  :long-description "Protocol for financial queries. Designed so that
  various APIs can be shimmed into it such that users can trivially
  instantiate the API and send it to cl-finance-query without pain")
