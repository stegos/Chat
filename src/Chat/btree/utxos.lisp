#|//! utxos.lisp -- Base data structs for Chat

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

(in-package :chat-btree.utxos)

(defvar *field-modulus*  edec:*ed-r*)  ;; for now

(defun make-sim-keys (&rest seeds)
  ;; return skey, pkey
  (multiple-value-bind (skey pkey)
      (edec::make-deterministic-keys seeds)
    (values skey (int pkey))))

(defvar *pending-txins* nil)
(defvar *pending-utxos* nil)

(defgeneric generate-utxo (obj))

(defmethod generate-utxo :around (data)
  (let ((utxo (call-next-method)))
    (push utxo *pending-utxos*)
    utxo))

(defclass bucket ()
  ;; used for structs that will become TXOUT UTXO
  ;; and which need a retained UTXO ID
  ((utxo-id :accessor bucket-utxo-id  :initform nil)
   (node-id :accessor bucket-node-id  :initform (new-key) :initarg :node-id)
   ))

(defmethod generate-utxo :around ((bucket bucket))
  (let ((utxo (call-next-method)))
    (setf (bucket-utxo-id bucket) (car utxo))
    utxo))

;; registry-claim - represents a claimed nickname
(defclass registry-claim (bucket)
  (;; registry-claim-nickname - the nickname used in this claim
   (nickname  :accessor registry-claim-nickname  :initarg :nickname)
   ;; registry-claim-epoch - the current epoch of this claim.
   ;; Registry entries in the blockchain may be scavenged after an
   ;; expiration period. So these registry entries need to be
   ;; regenerated periodically to keep them alive.
   (epoch     :accessor registry-claim-epoch     :initform (get-universal-time) :initarg :epoch)
   ;; registry-claim-pkey -- the public key which stands for this nickname
   (pkey      :accessor registry-claim-pkey      :initarg :pkey)
   ;; registry-claim-skey -- the secret key for this nickname, needed
   ;; in order to decrypt p2p messages sent to this address
   (skey      :accessor registry-claim-skey      :initarg :skey)
   ))

(defmethod generate-utxo ((msg registry-claim))
  (let* ((utxo  (list
                 :type     :nickname-utxo
                 :to       (registry-claim-pkey msg)
                 :cloaked  nil
                 :node-id  (bucket-node-id msg)
                 :epoch    (registry-claim-epoch msg)
                 :nickname (registry-claim-nickname msg)))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))

(defclass wallet ()
  (;; wallet-pkey - the main public key for the wallet. Nicknames
   ;; claims are separate from this, so that assets remain protected.
   (pkey                :accessor wallet-pkey               :initarg :pkey)
   ;; wallet-skey - the main secret key for the wallet. All derived
   ;; keys used by the wallet incorporate this as secret knowledge for
   ;; generating keying hash seeds. Needed for decrypting items sent
   ;; to the public key.
   (skey                :accessor wallet-skey               :initarg :skey)
   ;; wallet-spendable - a list of UTXOs that were sent to us, and
   ;; which can be spent to remove them from the blockchain. We often
   ;; need junk TXINs to form transactions, and they can be taken from
   ;; this list.
   (spendable           :accessor wallet-spendable          :initform nil)
   ;; wallet-monitored-groups -- a list of groups that are being
   ;; monitored by this wallet
   (monitored-groups    :accessor wallet-monitored-groups   :initform nil)
   ;; wallet-owned-groups -- a list of groups managed by nickname
   ;; aliases of this wallet
   (owned-groups        :accessor wallet-owned-groups       :initform nil)
   ;; wallet-nicknames -- a list of nickname claims that represent
   ;; aliases of this wallet. Used for running groups or for
   ;; subscribing to groups, so that the main wallet identity remains
   ;; unknown and protected.
   (nicknames           :accessor wallet-nicknames          :initform nil) ;; list of registry-claim's
   ;; wallet-registry -- a local copy of all nickname registrations in
   ;; the blockchain
   (registry            :accessor wallet-registry           :initform (make-hash-table)) ;; keyed by nickname
   ;; wallet-nonce -- a local nonce used for deterministic randomness
   ;; generation
   (nonce               :accessor wallet-nonce              :initform (get-universal-time))
   ;;
   ;; wallet-last-result -- an indication of success/failure on last action attempted
   (result              :accessor wallet-result             :initform :ok)
   ))


;; Registry entries as they appear at the member nodes
(defclass unode ()
  ((epoch    :accessor node-epoch   :initarg :epoch)
   (node-id  :accessor node-id      :initarg :node-id)
   ))

(defclass unickname-node (unode)
  ((nickname :accessor node-nickname  :initarg :nickname)
   (pkey     :accessor node-pkey      :initarg :pkey)
   ))

(defun find-registry-with-nickname (wallet nickname)
  (gethash nickname (wallet-registry wallet)))

(defun store-registry-entry (wallet nickname node-id epoch pkey)
  ;; the registry is a record of what the world knows
  (let ((reg  nil))
    (cond
     ((setf reg (gethash nickname (wallet-registry wallet)))
      ;; registry already exists - just update its epoch and pkey
      (setf (node-pkey reg)  pkey
            (node-epoch reg) epoch))

     (t
      (setf (gethash nickname (wallet-registry wallet))
            (make-instance 'unickname-node
                           :nickname nickname
                           :node-id  node-id
                           :epoch    epoch
                           :pkey     pkey)))
     )))

(defun make-registry-claim (wallet nickname)
  ;; the claims are what I want - maybe world will agree, maybe not
  (or (find nickname (wallet-nicknames wallet)
            :key 'registry-claim-nickname)
      (multiple-value-bind (skey pkey)
          (make-sim-keys (wallet-skey wallet) :nickname nickname)
        (let ((claim (make-instance 'registry-claim
                                    :nickname nickname
                                    :pkey     pkey
                                    :skey     skey)))
          (push claim (wallet-nicknames wallet))
          claim))
      ))

(defmethod get-spendable-txin ((wallet wallet))
  (or (first (pop (wallet-spendable wallet))) ;; the utxo-id 
      ;; TODO -- fix this
      :zero-token-of-mine))

(defun rfield-random ()
  ;; Non-deterministic random
  (safe-field-random *field-modulus*))

(defun generate-utxo-id (utxo)
  ;; adds a bit of rondomness so that we don't repeat the same ID
  ;; during sim, when the utxo is struturally the same
  (hash/256 utxo (rfield-random)))

(defconstant +ptrs-per-bucket+ 16)

(defvar *wallet-state* nil) ;; shared dynamic binding TLS

(defun prefix-from-path (path)
  (reduce (lambda (ans ix)
            (+ (* +ptrs-per-bucket+ ans) ix))
          (reverse path)
          :initial-value 0))

(defun new-key (&optional path)
  ;; Use deterministic randomness to generate a new key.
  ;; If path is given, then be sure LSB bits of key match the path
  (let* ((dvsr   (expt +ptrs-per-bucket+ (length path)))
         (prefix (prefix-from-path path)))
    (labels ((key-match (key)
               (cond ((null path)
                      key)
                     ((= prefix (mod key dvsr))
                      key)
                     )))
      (um:nlet-tail iter ()
        (let ((key  (int (hash:hash-to-range *field-modulus*
                                             (wallet-skey *wallet-state*)
                                             (incf (wallet-nonce *wallet-state*))))))
          (if (key-match key)
              key
            (iter))
          ))
      )))


