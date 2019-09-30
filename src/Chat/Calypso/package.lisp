#|//! package.lisp - Chat Prototype and Simulator

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

(defpackage :chat-calypso.skip-chain
  (:use cl
   :core-crypto)
  (:export
   ))

(defpackage :chat-calypso.utxos
  (:use :cl
   :core-crypto)
  (:export
   :make-sim-keys

   :*field-modulus*
   :*wallet-state*
   :*pending-txins*
   :*pending-utxos*
   :+ptrs-per-bucket+
   :generate-utxo
   :generate-utxo-id
   :hash/fr
   :rfield-random
   :new-key
   :prefix-from-path
   
   ;; wallet state and registry
   :wallet
   :wallet-pkey
   :wallet-skey
   :wallet-spendable
   :wallet-monitored-groups
   :wallet-owned-groups
   :wallet-registry
   :wallet-nicknames
   :wallet-result
   
   :registry-claim
   :registry-claim-nickname
   :registry-claim-epoch
   :registry-claim-pkey
   :registry-claim-skey
   :make-registry-claim
   
   :unode
   :unickname-node
   :node-id
   :node-epoch
   :node-nickname
   :node-pkey

   :find-registry-with-nickname
   :store-registry-entry
   :get-spendable-txin
   
   :bucket
   :bucket-utxo-id
   :bucket-node-id
   :registry-claim
   :registry-claim-nickname
   :registry-claim-epoch
   :registry-claim-pkey
   :registry-claim-skey
   ))

(defpackage :chat-calypso.blockchain-sim
  (:use :cl
   :core-crypto
   :chat-calypso.utxos)
  (:export
   :*blockchain*
   :init-blockchain
   :subscribe
   :publish-transaction
   ))

(defpackage :chat-calypso.p2p
  (:use :cl
   :core-crypto
   :chat-calypso.utxos
   :chat-calypso.blockchain-sim)
  (:export
   :generate-p2p
   :send-p2p
   :decode-p2p-message
   :decode-group-message
   :handle-decoded-message
   :generate-token
   ))

(defpackage :chat-calypso.owner
  (:use :cl
   :core-crypto
   :chat-calypso.utxos
   :chat-calypso.blockchain-sim
   :chat-calypso.p2p)
  (:export
   ;; action handlers for group chat
   :start-group
   :add-member-to-group
   :add-members-to-group
   :remove-member-from-group
   :view-tree-for-group
   :handle-owned-chat-message
   :tear-down-group
   
   ;; for shared introspection of group state
   :group-claim
   :group-nickname
   :group-skey
   :owner-pkey
   
   ;; shared utility functions
   :index-at-level
   :days-
   :+days-history+
   :send-p2p

   :with-owned-group
   ))

(defpackage :chat-calypso.wallet-sim
  (:use :cl
   :core-crypto
   :chat-calypso.utxos
   :chat-calypso.blockchain-sim
   :chat-calypso.p2p
   :chat-calypso.owner)
  (:export
   :make-wallet
   ))

(defpackage :chat-calypso
  (:use :cl
   :core-crypto
   :chat-calypso.utxos
   :chat-calypso.blockchain-sim
   :chat-calypso.p2p
   :chat-calypso.owner
   :chat-calypso.wallet-sim)
  (:export
   :run
   :*print-bignum-abbrev*
   ))

