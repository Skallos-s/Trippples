#lang racket

(require 2htdp/universe "client.rkt" "server.rkt")

(define (run)
  (launch-many-worlds (launch-client) (launch-client) (launch-server)))


(define (run1)
  (launch-many-worlds (launch-client) (launch-server)))

(define (run-quick)
  (launch-many-worlds (launch-client) (launch-client) (launch-server-quick-start)))


(define (run-quick1)
  (launch-many-worlds (launch-client) (launch-server-quick-start)))

(display LOCALHOST)