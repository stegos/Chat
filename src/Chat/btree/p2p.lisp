#|//! p2p.lisp - Chat Prototype and Simulator

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

(in-package :chat-btree.p2p)

(defclass payload ()
  ((from-nickname  :accessor payload-from-nickname  :initarg :from-nickname)
   (msg            :accessor payload-msg            :initarg :msg)
   (sig            :accessor payload-sig            :initarg :sig)
   ))

(defun make-signed-payload (&key from-nickname from-skey msg)
  (make-instance 'payload
                 :from-nickname from-nickname
                 :msg           msg
                 :sig           (generate-signature (list from-nickname msg) from-skey)))

(defun generate-signature (msg from-skey)
  ;; TODO - generate a Scnorr Signature
  (declare (ignore from-skey))
  (let* ((h  (hash/256 msg)))
    (declare (ignore h))
    :TODO))

;; --------------------------------------------------------------------------------------------

(defclass token-message ()
  ((to           :accessor token-to        :initarg :to)
   (amount       :accessor token-amount    :initarg :amount)))

(defclass p2p-message ()
  ((to           :accessor p2p-to           :initarg :to)  ;; pkey of recipient - will be cloaked
   (key-hint     :accessor p2p-key-hint     :initarg :key-hint)
   (encr-payload :accessor p2p-encr-payload :initarg :encr-payload)
   ))

(defclass group-message (bucket)
  ((to           :accessor group-message-to            :initarg :to)   ;; the pkey for the group owner - will be uncloaked
   (epoch        :accessor group-message-epoch         :initarg :epoch)
   (key-hint     :accessor group-message-key-hint      :initarg :key-hint)
   (encr-payload :accessor group-message-encr-payload  :initarg :encr-payload)
   ))

(defmethod generate-utxo ((msg token-message))
  ;; TODO - fix this up to be a real token UTXO
  (let* ((utxo (list
                :type   :token-utxo
                :to     (token-to msg)
                :amount (token-amount msg)))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))

(defmethod generate-utxo ((msg p2p-message))
  (let* ((utxo  (list
                 :type         :p2p-utxo
                 :to           (p2p-to msg)
                 :cloaked      t
                 :key-hint     (p2p-key-hint msg)
                 :encrypted-payload (p2p-encr-payload msg)
                 ))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))

(defmethod generate-utxo ((msg group-message))
  (let* ((utxo (list
                :type     :grp-msg-utxo
                :to       (group-message-to msg)
                :cloaked  nil
                :epoch    (group-message-epoch msg)
                ;; the following fields would be encrypted with the
                ;; group encryption key
                :key-hint       (group-message-key-hint msg)
                :encrypted-payload (group-message-encr-payload msg)
                ))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))

;; -----------------------------------------------------------------------------------------

(defun generate-token (&key to-pkey amount)
  (generate-utxo (make-instance 'token-message
                                :to     to-pkey
                                :amount amount)))

(defun generate-p2p (&key to-pkey from-nickname from-skey msg)
  (let* ((skey          (rfield-random))
         (encr-key      (int (edec:ed-mul (edec::ed-decompress-pt to-pkey) skey)))
         (key-hint      (int (edec:ed-nth-pt skey)))
         (payload       (make-signed-payload
                         :from-nickname from-nickname
                         :msg           msg
                         :from-skey     from-skey))
         (encr-payload  (encrypt-p2p-payload encr-key payload)))
    (generate-utxo (make-instance 'p2p-message
                                  :to           to-pkey
                                  :key-hint     key-hint
                                  :encr-payload encr-payload))
    ))

(defun send-p2p (&key txin to-pkey from-nickname from-skey msg)
  (let ((*pending-txins*  (list txin))
        (*pending-utxos*  nil))
    (generate-p2p
     :to-pkey       to-pkey
     :from-nickname from-nickname
     :from-skey     from-skey
     :msg           msg)
    (publish-transaction *pending-txins* *pending-utxos*)))

#|
  ;; TODO
(defun send-group-message (&key txin to-group-nickname from-nickname from-skey msg)
  (let ((group-pkey (find-group-pkey to-group-nickname)))
    (when node
      (let ((encr-skey  (find-group-encryption-key group-pkey)
      (let* ((group-pkey  (node-pkey node))
             (*pending-txins*  (list txin))
             (*pending-utxos*  nil)
             (encr-skey        (find-group-encryption-key to-group)
        (actual-encr-skey (rfield-random))
        (key-hint         (with-mod *field-modulus*
                            (m- actual-encr-key encr-key)))
        (payload          (make-signed-payload
                           :from-nickname from-nickname
                           :msg           msg
                           :from-skey     from-skey))
        (encr-paylaod    (encrypt-payload actual-encr-skey payload)))
    (generate-utxo (make-instance 'p2p-message
                                  :to           to-pkey
                                  :key-hint     key-hint
                                  :encr-payload encr-payload))
    (publish-transaction *pending-txins* *pending-utxos*)))
|#

(defun encrypt-p2p-payload (encr-skey payload)
  (let* ((h    (hash/256 encr-skey))
         (key  (subseq (bev-vec h) 0 16))
         (ivec (subseq (bev-vec h) 16)))
    (aes-encrypt key ivec payload)
    ;; TODO - remove this stuff
    (list ;; for now
     :from-nickname  (payload-from-nickname payload)
     :msg            (payload-msg payload)
     :sig            (payload-sig payload))))

(defun decrypt-p2p-payload (encr-skey encrypted-payload)
  (let* ((h    (hash/256 encr-skey))
         (key  (subseq (bev-vec h) 0 16))
         (ivec (subseq (bev-vec h) 16)))
    (aes-decrypt key ivec encrypted-payload)))

(defun aes-encrypt (key ivec msg)
  ;; TODO -- actual message encryption
  (declare (ignore key ivec))
  msg)

(defun aes-decrypt (key ivec msg)
  ;; TODO -- actual message decryption
  (declare (ignore key ivec))
  msg)

(defun validate-signature (hash sig pkey)
  (declare (ignore hash sig pkey))
  t) ;; for now - TODO

(defun decode-p2p-message (wallet skey utxo)
  (let* ((keying-hint       (getf utxo :key-hint))
         (encrypted-payload (getf utxo :encrypted-payload))
         (encr-key          (int (edec:ed-mul (edec::ed-decompress-pt keying-hint) skey)))
         (payload           (decrypt-p2p-payload encr-key encrypted-payload))
         (from-nickname     (getf payload :from-nickname))
         (node              (find-registry-with-nickname wallet from-nickname)))
    (when node
      (let* ((pkey  (node-pkey node))
             (msg   (getf payload :msg))
             (sig   (getf payload :sig))
             (h     (hash/256 from-nickname msg)))
        (when (validate-signature h sig pkey)
          (values pkey msg))
        ))))

(defun decode-group-message (wallet skey utxo)
  ;; TODO
  (declare (ignore wallet skey utxo)))

(defun handle-decoded-message (wallet from-pkey msg)
  ;; do whatever... TODO
  (declare (ignore wallet from-pkey msg))
  )

