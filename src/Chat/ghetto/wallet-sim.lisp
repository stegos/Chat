#|//! wallet-sim.lisp - Wallet Simulator 

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


(in-package :chat-ghetto.wallet-sim)

;; -----------------------------------------------------------------
;; Wallets can subscribe to the blockchain, join group chat sessions,
;; and own group chat sessions and channels.
;;
;; For each group joined builds registry trees in memory from UTXOs in
;; transactions sent to blockchain by group owner.

;; --------------------------------------------------------------
;; Member State

(defclass monitored-group-state ()
  (;; member-claim - the registered identity by which we monitor this
   ;; group
   (claim         :accessor member-claim    :initarg :mon-claim)
   ;; group-registry - the registered identity used by the group
   (grp-reg       :accessor group-registry  :initarg :grp-reg)
   ;; group-top - the UTXO ID of the top node of the group keying tree
   (top           :accessor group-top       :initform nil)
   ;; group-mem - local copy of group keying tree
   (mem           :accessor group-mem       :initform (make-utxo-memory))
   ;; group-skey - the group access seecret key assigned to us for
   ;; deriving group encryption key
   (gskey         :accessor group-skey      :initform nil)
   ))


;; convenience accessors for indirect information
(defmethod member-pkey ((group monitored-group-state))
  (registry-claim-pkey (member-claim group)))

(defmethod member-nickname ((group monitored-group-state))
  (node-nickname (member-claim group)))

(defmethod group-nickname ((group monitored-group-state))
  (node-nickname (group-registry group)))

(defmethod group-pkey ((group monitored-group-state))
  (node-pkey (group-registry group)))

(defmethod group-epoch ((group monitored-group-state))
  (node-epoch (group-registry group)))

;; --------------------------------------------------------------
;; Pointers are all NODE ID's (int here), and pointer deref is by way
;; of indirection through hash table keyed by node-id.

(defun make-utxo-memory ()
  (make-hash-table))

(defmethod fetch ((state monitored-group-state) ix)
  (gethash (int ix) (group-mem state)))

(defmethod store ((state monitored-group-state) ix obj)
  (setf (gethash (int ix) (group-mem state)) obj))

(defmethod store-tospace (tospace ix obj)
  (setf (gethash (int ix) tospace) obj))

;; Memory of Trees
(defun store-tree-node (group tree-node)
  (store group (node-id tree-node) tree-node))

;; Reconstructed Trees from incoming UTXOs
(defclass ucoff-node (unode)
  ((coffs      :accessor node-coffs      :initarg :coffs)
   (children   :accessor node-children   :initarg :children)
   ))

(defclass uptr-node (unode)
  ((ptrs  :accessor node-ptrs :initarg :ptrs)
   ))

;; --------------------------------------------------------------

;; Owned Groups
(defun find-owned-group-with-nickname (wallet nickname)
  (find nickname (wallet-owned-groups wallet)
        :key 'group-nickname))

(defun find-owned-group-with-pkey (wallet pkey)
  (find pkey (wallet-owned-groups wallet)
        :key 'owner-pkey))

(defun discard-owned-group-with-nickname (wallet nickname)
  (setf (wallet-owned-groups wallet)
        (delete nickname (wallet-owned-groups wallet)
                :key 'group-nickname)))

(defun discard-owned-group (wallet group)
  (setf (wallet-owned-groups wallet)
        (delete group (wallet-owned-groups wallet))))

;; Monitored Groups
(defun find-monitored-group-with-group-nickname (wallet nickname)
  (find nickname (wallet-monitored-groups wallet)
        :key 'group-nickname))

(defun find-monitored-group-owned-by (wallet pkey)
  (find pkey (wallet-monitored-groups wallet)
        :key 'group-pkey))

(defun find-group-monitored-by-pkey (wallet pkey)
  (find pkey (wallet-monitored-groups wallet)
        :key 'member-pkey))
  
(defun discard-groups-monitored-by-nickname (wallet nickname)
  (setf (wallet-monitored-groups wallet)
        (delete nickname (wallet-monitored-groups wallet)
                :key 'member-nickname)))

(defun discard-monitored-group-with-group-nickname (wallet nickname)
  (setf (wallet-monitored-groups wallet)
        (delete nickname (wallet-monitored-groups wallet)
                :key 'group-nickmname)))

(defun discard-monitored-groups-referencing-claim (wallet claim)
  (setf (wallet-monitored-groups wallet)
        (mapcan (lambda (group)
                  (cond
                   ;; Q: is this group being monitored under the nickname?
                   ((eql claim (member-claim group))
                    (gen-p2p-resign-from-group (group-pkey group) claim)
                    nil)
                   
                   ;; Otherwise, retain this group monitoring
                   (t
                    (list group))
                   ))
                (wallet-monitored-groups wallet)) ))

(defun discard-monitored-group (wallet group)
  (setf (wallet-monitored-groups wallet)
        (delete group (wallet-monitored-groups wallet))))

;; Claims
(defun find-claim-with-nickname (wallet nickname)
  (find nickname (wallet-nicknames wallet)
        :key 'registry-claim-nickname))

(defun find-claim-with-pkey (wallet pkey)
  (find pkey (wallet-nicknames wallet)
        :key 'registry-claim-pkey))

(defun discard-claim-with-nickname (wallet nickname)
  (setf (wallet-nicknames wallet)
        (delete nickname (wallet-nicknames wallet)
                :key 'registry-claim-nickname)))

(defun discard-claim (wallet claim)
  (setf (wallet-nicknames wallet)
        (delete claim (wallet-nicknames wallet))))

;; Registry
(defun #1=find-registry-with-pkey (wallet pkey)
  (for-each-registry-entry
   wallet
   (lambda (registry-entry)
     (when (eql pkey (node-pkey registry-entry))
       (return-from #1# registry-entry))))
  nil)

(defun discard-registry-with-nickname (wallet nickname)
  (remhash nickname (wallet-registry wallet)))

(defun discard-registry-entry (wallet registry-entry)
  (remhash (node-id registry-entry) (wallet-registry wallet)))

(defun for-each-registry-entry (wallet fn)
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall fn v))
           (wallet-registry wallet)))

(defun collect-expired-registry-entries (wallet)
  (let ((expiration-date (days- (get-universal-time) +days-history+)))
    (um:accum accum-expired
      (for-each-registry-entry
       wallet
       (lambda (registry-entry)
         (when (<= (node-epoch registry-entry) expiration-date)
           (accum-expired registry-entry)))
       ))))

;; UTXO Generation
(defun gen-spend-and-regenerate-claim (claim)
  (push (bucket-utxo-id claim) *pending-txins*)
  (generate-utxo claim))

(defun gen-p2p-ask-for-group-skey (to-pkey claim)
  (generate-p2p
   :to-pkey       to-pkey
   :from-nickname (registry-claim-nickname claim)
   :from-skey     (registry-claim-skey claim)
   :msg           (list :request-group-key)
   ))

(defun gen-p2p-resign-from-group (to-pkey claim)
  (generate-p2p
   :to-pkey       to-pkey
   :from-nickname (registry-claim-nickname claim)
   :from-skey     (registry-claim-skey claim)
   :msg           (list :resign-from-group)
   ))

(defmacro with-new-registration (claim &body body)
  ;; UTXOs are generated in stack order so this macro ensures that the
  ;; claim utxo is generated last.  This is so that other parties will
  ;; have a registry to refer to when messages come across from the
  ;; new nickname
  `(progn
     ,@body
     (generate-utxo ,claim)))

;; --------------------------------------------------------------
;; wh-xxx - wallet handlers
;; Split out to separate defuns to make debugging easier

(defun wh-start-group (wallet nickname init-members)
  (setf (wallet-result wallet) :ok) ;; assume the best
  (cond
   ((find-owned-group-with-nickname wallet nickname)
    ;; group is already run by us
    )

   ((find-registry-with-nickname wallet nickname)
    (setf (wallet-result wallet) :nickname-already-taken))

   (t
    (let ((group (start-group wallet nickname init-members)))
      (push group (wallet-owned-groups wallet))
      (push (group-claim group) (wallet-nicknames wallet))))
   ))

(defun wh-join-group (wallet grp-nickname my-nickname)
  ;; for simulation prodding
  ;;
  ;; To join a group we must either alreay have a registered nickname
  ;; or else we must create it now. If already have, then we spend it
  ;; and regenerate it, along with our query for a group key.
  ;;
  (setf (wallet-result wallet) :ok) ;; assume the best
  (let ((reg   nil)
        (mon   nil)
        (me    (find-claim-with-nickname wallet my-nickname))
        (*pending-txins*  nil)
        (*pending-utxos*  nil))
    (cond
     ;; Q: is group-nickname one of ours?
     ((find-claim-with-nickname wallet grp-nickname)
      ;; can't subscribe to our own nickname
      (setf (wallet-result wallet) :cant-join-owned-group))

     ;; Q: does group exist?
     ((setf reg (find-registry-with-nickname wallet grp-nickname))
      (cond
       ;; Q: is group nickname already subscribed to by our wallet?
       ((setf mon (find-monitored-group-with-group-nickname wallet grp-nickname))
        ;; Already monitoring the group.  Ensure it is monitored under
        ;; my-nickname
        (cond
         ;; Q: is it already subscribed to under my-nickname?
         ((and me
               (eql me (member-claim mon)))
          ;; already registered to monitor group under my-nickname so
          ;; nothing to do here...
          )
         
         ;; Q: is my-nickanme already registered?
         (me
          ;; already registered my-nickname but need to switch
          ;; subscription from a previous nickname
          ;;
          ;; this entails:
          ;;   1. choose to spend my-nickname registry and reconstruct
          ;;   2. telling group owner to unregister my old nickname pkey
          ;;   3. switching out my-nickname in monitored-group-state
          ;;   4. asking group owner for new group skey
          
          ;; step 1 - choosing a handy zero token UTXO
          (gen-spend-and-regenerate-claim me)
          
          ;; step 2 - unregister old nickname
          (gen-p2p-resign-from-group (group-pkey mon) (member-claim mon))
          
          ;; step 3 - replace the monitoring identity in the monitored
          ;; group state
          (setf (member-claim mon) me
                (group-skey mon)   nil)
          
          ;; step 4 - request a group access skey
          (gen-p2p-ask-for-group-skey (group-pkey mon) me)
          
          (publish-transaction *pending-txins* *pending-utxos*))
         
         ;; otherwise - need to register my-nickname and then switch
         ;; subscription
         (t
          ;; my-nickname not yet registered
          ;;
          ;; This entails:
          ;;   0. find a junk UTXO to spend
          ;;   1. registering a new nickname for my-nickname - which has new pkey
          ;;   2. telling group owner to unregister my previous nickname
          ;;   3. switching out my-nickname in monitored-group-state
          ;;   4. asking group owner for new group skey
          (setf me (make-registry-claim wallet my-nickname))
          
          ;; step 0 - find a junk utxo to spend
          (push (get-spendable-txin wallet) *pending-txins*)
          
          ;; step 1 - create new registration for my-nickname
          (with-new-registration me
            
            ;; step 2 - unregister membership for old nickname
            (gen-p2p-resign-from-group (group-pkey mon) (member-claim mon))
            
            ;; step 3 - switch out monitoring identity in monitoring state
            (setf (member-claim mon) me
                  (group-skey mon)      nil)
            
            ;; step 4 - request new group access key
            (gen-p2p-ask-for-group-skey (group-pkey mon) me))
          
          (publish-transaction *pending-txins* *pending-utxos*))
         ))

       ;; -------------------------------------------
       ;; Otherwise - not yet monitoring group, so create a new monitoring state
       (t
        (setf mon (make-instance 'monitored-group-state
                                 :mon-claim  me
                                 :grp-reg    reg))
        (push mon (wallet-monitored-groups wallet))
        (cond
         ;; Q: is my-nickname already registered?
         (me
          ;; spend and regenerate our registry entry as a ready zero token to spend
          (gen-spend-and-regenerate-claim me)
          ;; ask for group access key
          (gen-p2p-ask-for-group-skey (node-pkey reg) me)

          (publish-transaction *pending-txins* *pending-utxos*))

         ;; Otherwise - register my-nickname then ask for group access key
         (t
          (setf me (make-registry-claim wallet my-nickname)
                (member-claim mon) me)

          (push (get-spendable-txin wallet) *pending-txins*)

          (with-new-registration me
            (gen-p2p-ask-for-group-skey (node-pkey reg) me))

          (publish-transaction *pending-txins* *pending-utxos*))
         ))))

     ;; --------------------------------------------------
     ;; Otherwise - no such group
     (t
      (setf (wallet-result wallet) :no-such-group))
     )))

(defun wh-register-nickname (wallet nickname)
  (setf (wallet-result wallet) :ok) ;; assume the best
  (cond
   ((find-claim-with-nickname wallet nickname)
    ;; already registered, or hopes to be
    )

   ((find-registry-with-nickname wallet nickname)
    ;; someone else owns this nickname
    (setf (wallet-result wallet) :someone-else-registered-nickname))

   (t
    ;; stake our claim to the nickname and hope world agrees
    (let ((claim (make-registry-claim wallet nickname))
          (*pending-txins* (list (get-spendable-txin wallet)))
          (*pending-utxos* nil))
      (generate-utxo claim)
      (publish-transaction *pending-txins* *pending-utxos*)))
   ))

(defun wh-transfer-group-to-new-owner (wallet group-nickname his-pkey)
  (setf (wallet-result wallet) :ok) ;; assume the best
  ;; transfer group ownership
  (um:when-let (group (find-owned-group-with-nickname wallet group-nickname))
    ;; group is one of my owned groups
    (um:when-let (reg (find-registry-with-pkey wallet his-pkey))
      ;; there is a registry entry with that new owner pkey
      (let ((claim nil))
        (cond
         ((setf claim (find-claim-with-pkey wallet his-pkey))
          ;; that new owner key is one of mine
          (unless (eql claim (group-claim group)) ;; unless no real change...
            ;; change of keying would also change nickname of group... can't do that
            (setf (wallet-result wallet) :cant-change-group-nickname)
            ))

         (t
          ;; not one of our keys - so it is a real change
          (setf claim (group-claim group))
          (with-owned-group (group)
            (let ((*pending-txins* (bucket-utxo-id (group-claim group)))
                  (*pending-utxos* nil))

              ;; +--------------------------------------------------
              ;; TODO -- send group state to new owner
              ;; Do it all here, before we start tossing things out
              ;; +--------------------------------------------------
              
              ;; -- Internal state update --
              
              ;; -- give up claim to group nickname 
              (discard-claim wallet claim)
              
              ;; -- remove group from our ownership
              (discard-owned-group wallet group)
              
              ;; -- if any subscriptions to other groups using our group
              ;; nickname as a member nickname then those subscriptions
              ;; will be cancelled here
              (discard-monitored-groups-referencing-claim wallet claim)
              
              ;; spend our group claim and generate a new one in new owner's pkey
              ;; to send public transfer of ownership to new owner
              (generate-utxo (make-instance 'registry-claim
                                            :node-id  (bucket-node-id claim)
                                            :pkey     his-pkey
                                            :nickname group-nickname))
              (publish-transaction *pending-txins* *pending-utxos*))))
         )))))

(defun wh-tear-down-registration (wallet nickname)
  (setf (wallet-result wallet) :ok)
  (um:when-let (claim (find-claim-with-nickname wallet nickname))
    ;; discard our claim record
    (discard-registry-with-nickname wallet nickname)
    (discard-claim wallet claim)
    
    (let ((*pending-txins*  (list (bucket-utxo-id claim)))
          (*pending-utxos*  nil))
      (um:when-let (group (find-owned-group-with-nickname wallet nickname))
        ;; remove all keying trees and group messages from the
        ;; blockchain
        (tear-down-group group)
        (discard-owned-group wallet group))

      ;; this also sends unsubscribe to group leaders
      (discard-monitored-groups-referencing-claim wallet claim)

      ;; if we haven't generated any UTXOs then we need to
      ;; construct one.
      (unless *pending-utxos*
        (generate-token
         :to-pkey  (wallet-pkey wallet)
         :amount   0))
      
      (publish-transaction *pending-txins* *pending-utxos*)
      )))

(defun wh-keying-info (wallet nickname)
  (um:when-let (group (find-monitored-group-with-group-nickname wallet nickname))
    (ac:pr :Keying
           (when (and (group-skey group) ;; we have been assigned a secret key
                      (tree-complete-p group))
             (list
              :epoch    (node-epoch (fetch group (group-top group)))
              :nickname (group-nickname group)
              :encr-key (find-encryption-key group)
              )))))

(defun make-wallet (&rest key-seeds)
  (multiple-value-bind (wallet-skey wallet-pkey)
      (apply 'make-sim-keys :wallet key-seeds)
    (let* ((wallet  (make-instance 'wallet
                                   :pkey  wallet-pkey
                                   :skey  wallet-skey))
           (me (ac:make-actor
                (um:dlambda
                  (:utxos (txouts)
                   (update-sim-member-trees wallet txouts))
                  
                  (:gc ()
                   (try-gc wallet))
                  
                  (:start-group (nickname &optional init-members)
                   (wh-start-group wallet nickname init-members))
                  
                  (:register-nickname (nickname)
                   (wh-register-nickname wallet nickname))
                  
                  (:transfer-group (grp-nickname new-owner-pkey)
                   (wh-transfer-group-to-new-owner wallet grp-nickname new-owner-pkey))
                  
                  (:tear-down-registration (nickname)
                   (wh-tear-down-registration wallet nickname))
                  
                  (:join-group (grp-nickname my-nickname)
                   (wh-join-group wallet grp-nickname my-nickname))
                  
                  ;; -----------------------------------------------------
                  ;; for watching behavior
                  
                  (:add-members (nickname pkeys)
                   (um:when-let (group (find-owned-group-with-nickname wallet nickname))
                     (add-members-to-group group pkeys)))
                  
                  (:remove-member (nickname pkey)
                   (um:when-let (group (find-owned-group-with-nickname wallet nickname))
                     (remove-member-from-group group pkey)))
                  
                  (:view-tree (nickname)
                   (um:when-let (group (find-owned-group-with-nickname wallet nickname))
                     (view-tree-for-group group)))
                  
                  (:keying-info (nickname)
                   ;; for simulation debug query
                   (wh-keying-info wallet nickname))
                  
                  (:show ()
                   (ac:pr (with-output-to-string (s)
                            (describe wallet) s))
                   (inspect wallet))
                  
                  (:tell-ready ()
                   t)
                  
                  (t (&rest msg)
                     (ac:pr (format nil "Unknown member message: ~S" msg)))
                  ))
               ))
      (ac:wrap-context me ((*wallet-state*  wallet)))
      (subscribe me)
      me)))

;; -------------------------------------------------------------------

(defmethod try-gc ((wallet wallet))
  (dolist (registry-entry (collect-expired-registry-entries wallet))
    (let ((nickname  (node-nickname registry-entry)))
      (discard-owned-group-with-nickname wallet nickname)
      (discard-monitored-group-with-group-nickname wallet nickname)
      (discard-claim-with-nickname wallet nickname)
      (discard-registry-entry wallet registry-entry)))
    (dolist (group (wallet-monitored-groups wallet))
      (try-gc group)))

(defmethod try-gc ((member monitored-group-state))
  ;; Trim out the old epoch from our indirection pointers table.
  (let ((to-space (make-utxo-memory)))
    (ignore-errors
      (labels ((descend (ptr)
                 (let* ((node     (fetch member ptr))
                        (children (node-children node)))
                   (store-tospace to-space ptr node)
                   (when children
                     (let ((ptrs  (fetch member children)))
                       (store-tospace to-space children ptrs)
                       (map nil #'descend (node-ptrs ptrs))))
                   )))
        (descend (group-top member))
        (setf (group-mem member) to-space) ;; be gone old epochs
        ))))

(defmethod tree-complete-p ((member monitored-group-state))
  ;; return true if we have a complete current state at our end
  ;; If we succeed, we will also have trimmed out the old epoch
  ;; from our indirection pointers table.
  (ignore-errors
    (labels ((descend (ptr)
               (let* ((node     (fetch member ptr))
                      (children (node-children node)))
                 (when children
                   (let ((ptrs  (fetch member children)))
                     (map nil #'descend (node-ptrs ptrs))))
                 )))
      (descend (group-top member))
      t)))

(defmethod find-encryption-key ((member monitored-group-state))
  ;; given a member group secret key, and the accumulated state of the
  ;; system obtained from watching utxo traffic, find the group
  ;; encryption key
  (let ((skey (group-skey member)))
    (labels
        ((eval-poly (node x)
           (reduce (lambda (coff ans)
                     (with-mod *ed-r*
                       (m+ coff (m* ans x))))
                   (node-coffs node)
                   :initial-value 0
                   :from-end t))
         
         (descend (top level)
           (let* ((node     (fetch member top))
                  (children (node-children node)))
             (if children
                 (let* ((ptr-node (fetch member children))
                        (ix       (index-at-level skey level))
                        (child    (aref (node-ptrs ptr-node) ix))
                        (subkey   (descend child (1+ level))))
                   (eval-poly node subkey))
               ;; else
               (eval-poly node skey)))))
      
      (descend (group-top member) 0)
      )))

(defmethod update-sim-member-trees ((wallet wallet) txouts)
  ;; respond to UTXO traffic
  (dolist (txout txouts)
    ;; while we are watching individual utxos here, the actual state
    ;; change on new epoch occurs all at once in the blockchain, not
    ;; in fragmentary form.
    ;;
    ;; Still, perhaps we need a way to verify that we have the whole
    ;; state on our end, because we may not read any but fragments at
    ;; a time. The epoch hasn't fully shifted for us until we have a
    ;; complete tree at our node.
    (destructuring-bind (utxo-id utxo) txout
      (declare (ignore utxo-id))
      (let ((recip  (int (getf utxo :to))))
        (ecase (getf utxo :type)
          (:nickname-utxo
           ;; this utxo should be the first to arrive among a batch of
           ;; registry utxos. This way we will have a group pkey to use
           ;; in the is-our-group-p tests that follow.
           (let* ((nickname  (getf utxo :nickname))
                  (node-id   (getf utxo :node-id))
                  (epoch     (getf utxo :epoch)))
             (store-registry-entry wallet nickname node-id epoch recip)
             
             ;; if nickname matches one I'm vying for...
             (um:when-let (my-claim (find-claim-with-nickname wallet nickname))
               ;; unless the pkeys match, I lost
               (unless (eql recip (registry-claim-pkey my-claim))
                 ;; so remove my claim to the nickname
                 (discard-claim wallet my-claim)
                 (discard-owned-group-with-nickname wallet nickname)
                 (discard-groups-monitored-by-nickname wallet nickname)))
             ))
          
          (:grp-ptr-utxo
           (um:when-let (group (find-monitored-group-owned-by wallet recip))
             (let ((node-id (getf utxo :node-id)))
               (store-tree-node group (make-instance 'uptr-node
                                                     :node-id node-id
                                                     :epoch   (getf utxo :epoch)
                                                     :ptrs    (getf utxo :ptrs)))
               )))
          
          (:grp-coffs-utxo
           (um:when-let (group (find-monitored-group-owned-by wallet recip))
             (let ((node-id (getf utxo :node-id)))
               (store-tree-node group (make-instance 'ucoff-node
                                                     :node-id  node-id
                                                     :epoch    (getf utxo :epoch)
                                                     :coffs    (getf utxo :coffs)
                                                     :children (getf utxo :children)))
               (when (getf utxo :top-p)
                 ;; note the pointer to the top of the tree
                 (setf (group-top group) node-id))
               )))
          
          (:grp-msg-utxo
           ;; UTXO represenets a group chat message
           (let ((group  nil))
             (cond
              ((setf group (find-owned-group-with-pkey wallet recip))
               ;; was a group message sent to one of my groups
               ;; so I must also retain for history removal later
               ;;
               ;; -- get the :epoch and decrypt the message payload
               ;; -- get nickname of sender
               ;; -- lookup pkey of sender
               ;; -- validate signature on the message
               ;; -- read the message text and do whatever...
               (handle-owned-chat-message group wallet txout))
              
              ((setf group (find-monitored-group-owned-by wallet recip))
               ;; -- get the :epoch and decrypt the message payload
               ;; -- get nickname of sender
               ;; -- lookup pkey of nickname
               ;; -- validate signature on the message
               ;; -- read the message text and do whatever...
               (handle-chat-message group wallet utxo)
               ))))
          
          (:p2p-utxo
           (let* ((group    nil)
                  (registry nil)
                  (skey   (cond ((eql recip (wallet-pkey wallet))
                                 (wallet-skey wallet))
                                ((setf registry (find-claim-with-pkey wallet recip))
                                 (registry-claim-skey registry))
                                )))
             (when skey
               (multiple-value-bind (from-pkey msg)
                   (decode-p2p-message wallet skey utxo)
                 (when from-pkey
                   (cond
                    ((eql recip (wallet-pkey wallet))
                     ;; message directed to me
                     ;;
                     (push txout (wallet-spendable wallet))
                     ;; do whatever else...
                     )
                    
                    ;; our group owner persona (pkey) is different from
                    ;; our wallet pkey
                    ((setf group (find-owned-group-with-pkey wallet recip))
                     ;; message directed to me as group owner
                     (push txout (wallet-spendable wallet))
                     (case (first msg)
                       (:request-group-key
                        (add-member-to-group group from-pkey))
                       (:resign-from-group
                        (remove-member-from-group group from-pkey))
                       ))
                    
                    ;; our group member persona (pkey) is different from
                    ;; our wallet pkey
                    ((setf group (find-group-monitored-by-pkey wallet recip))
                     ;; message directed to me as group member
                     ;; put this utxo in the spendables list for the wallet
                     (push txout (wallet-spendable wallet))
                     (case (first msg)
                       (:secret-grp-key
                        (setf (group-skey group) (second msg)))
                       ))))))))
          )))))

;; TODO -- finish this
(defun handle-chat-message (group wallet utxo)
  (ignore-errors
    (let* ((encr-key (find-encryption-key group)))
      (multiple-value-bind (from-pkey msg)
          (decode-group-message wallet encr-key utxo)
        (when from-pkey
          (handle-decoded-message wallet from-pkey msg))))))

