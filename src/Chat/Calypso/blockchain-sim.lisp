#|//! blockchain-sim.lisp - Blockchain Simulator for Chat

//
// Copyright (c) 2019 Stegos AG
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
|#

(in-package :chat-calypso.blockchain-sim)

;; -----------------------------------------------------------------
;; Blockchain in the cloud - simulator

(defvar *field-modulus*  *ed-r*)  ;; for now

;; For this purpose, TXINS are lists of UTXO hash ID's.
;; TXOUTS are human readable property lists containing a UTXO type.

(defclass blockchain ()
  ((subscribers  :accessor blockchain-subscribers  :initform nil)
   (utxos        :accessor blockchain-utxos        :initform (make-hash-table))
   ))

(defvar *blockchain*  nil)

(defun make-blockchain ()
  (let ((bc  (make-instance 'blockchain)))
    (ac:make-actor
     (um:dlambda
       (:transaction (txins txouts)
        (ac:pr (list :txins  txins
                     :txouts txouts))
        (when (validate-transaction txins txouts)
          ;; clear out spent utxos
          (dolist (txin txins) ;; a list of utxo ids
            (ignore-errors
              ;; simulation allows non hash items for utxo id
              ;; just ignore these (e.g., :zero-token-of-mine)
              (remhash (int txin) (blockchain-utxos bc))))
          
          ;; record new utxos
          (dolist (txout txouts)
            (destructuring-bind (utxo-id utxo) txout
              (setf (gethash (int utxo-id) (blockchain-utxos bc)) utxo)))
          
          ;; send utxos to subscribers - to simulate
          ;; their async reading of network traffic
          (dolist (subscriber (blockchain-subscribers bc))
            (ac:send subscriber :utxos txouts))))
       
       (:add-subscriber (subscriber)
        (pushnew subscriber (blockchain-subscribers bc)))
       
       (:remove-subscriber (subscriber)
        (setf (blockchain-subscribers bc)
              (delete subscriber (blockchain-subscribers bc))))

       (:tell-ready ()
        t)

       (:show ()
        (ac:pr
         (with-output-to-string (s)
           (describe bc s)))
        (inspect bc))
       
       (t (&rest msg)
          (ac:pr (format nil "Unknown blockchain message: ~S" msg)))
       
       )) ))

(defun init-blockchain ()
  (setf *blockchain* (make-blockchain)))

(defun validate-transaction (txins txouts)
  (declare (ignore txins txouts))
  t) ;; for now...

(defun publish-transaction (txins txouts)
  (ac:send *blockchain* :transaction txins txouts))

(defun subscribe (actor)
  (ac:send *blockchain* :add-subscriber actor))

