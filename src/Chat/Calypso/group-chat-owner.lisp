#|//! group-chat-owner.lisp -- Code to operate wallet as a group chat session owner

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

(in-package :chat-calypso.owner)

;; ------------------------------------------------

(defclass group-owner ()
  (;; group-claim - the registered identity by which we run this group
   (claim       :accessor group-claim       :initform nil :initarg :claim)
   ;;group-messages - the repo of group messages by epoch.  Since they
   ;;were all sent to use, we own them, but must delay spending until
   ;;expiry of the group epoch in group history
   (msg-history :accessor group-messages    :initform (make-hash-table)) ;; index by epoch
   ;; group-members - a hashmap relating group member pkeys to their
   ;; assigned group access skeys
   (members     :accessor group-members     :initform (make-hash-table))
   ;; group-history - the collection of group tree top nodes in the
   ;; history Ordered by epoch, so expiring epochs are nearest the
   ;; front of the list.
   (history     :accessor group-history     :initform nil)
   ;; group-top - points to the top tree node for the current epoch
   (top         :accessor group-top         :initform nil)
   ;; group-avail-skeys -- pregenerated group access keys for
   ;; available slots in the current keying tree
   (avail-skeys :accessor group-avail-skeys :initform nil)
   ))

(defvar *group-state*  nil) ;; shared dynamic binding TLS

(defmacro with-owned-group ((group) &body body)
  `(let ((*group-state*  ,group))
     ,@body))

;; ---------------------------------------------
;; derived info

(defmethod owner-pkey ((group group-owner))
  (registry-claim-pkey (group-claim group)))

(defmethod owner-skey ((group group-owner))
  (registry-claim-skey (group-claim group)))

(defmethod group-nickname ((group group-owner))
  (registry-claim-nickname (group-claim group)))

(defmethod group-epoch ((group group-owner))
  (registry-claim-epoch (group-claim group)))

(defmethod new-epoch ((group group-owner))
  (setf (registry-claim-epoch (group-claim group)) (get-universal-time)))

;; ----------------------------------------------

(defmethod leaf-bucket-p ((bucket bucket))
  nil)

(defclass ptr-bucket (bucket)
  ((ptrs  :accessor bucket-ptrs :initform (make-array +ptrs-per-bucket+) :initarg :ptrs)
   ))

(defclass coff-bucket (bucket)
  ((avail-keys :accessor bucket-avail-keys :initform nil :initarg :avail-keys)
   (alloc-keys :accessor bucket-alloc-keys :initform nil :initarg :alloc-keys)
   (key        :accessor bucket-key                      :initarg :key)
   (coffs      :accessor bucket-coffs                    :initarg :coffs)
   (parent     :accessor bucket-parent     :initform nil :initarg :parent)
   (epoch      :accessor bucket-epoch      :initform nil :initarg :epoch)
   (path       :accessor bucket-path       :initform nil :initarg :path)
   (top-p      :accessor bucket-top-p      :initform nil :initarg :top-p)
   (children   :accessor bucket-children   :initform nil :initarg :children)
   (readers    :accessor bucket-readers    :initform nil)
   ))

(defmethod leaf-bucket-p ((bucket coff-bucket))
  (null (bucket-children bucket)))


;; -------------------------------------------------------------------
;; For simulated UTXO construction - make human readable UTXOs

(defmethod generate-utxo ((bucket ptr-bucket))
  (let* ((utxo (list
                :type     :grp-ptr-utxo
                :to       (owner-pkey *group-state*)
                :cloaked  nil
                :node-id  (bucket-node-id bucket)
                :epoch    (group-epoch *group-state*)
                :ptrs     (map 'vector 'bucket-node-id (bucket-ptrs bucket))
                ))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))

(defmethod generate-utxo ((bucket coff-bucket))
  (let* ((utxo (list
                :type     :grp-coffs-utxo
                :to       (owner-pkey *group-state*)
                :cloaked  nil
                :node-id  (bucket-node-id bucket)
                :epoch    (group-epoch *group-state*)
                :top-p    (bucket-top-p bucket)
                :coffs    (bucket-coffs bucket)
                :children (when (bucket-children bucket)
                            (bucket-node-id (bucket-children bucket)))
                ))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))
  
;; ---------------------------------------------------------------------

(defun compute-coffs (key roots)
  ;; compute a key cloaking polynomial
  ;; Q(x) = key + P(x), where P(x) = 0 at roots
  ;; P(x) = a_rand * (x - root1)*(x - root2)*...*(x - rootN)
  (with-mod *field-modulus*
    (let* ((a-rand    (new-key))
           (polycoffs (reduce (lambda (ans coff)
                                (let* ((rowz (cons 0 ans))
                                     (rowc (append (mapcar (um:curry 'm* (m- coff)) ans) (list 0))))
                                  (mapcar 'm+ rowz rowc)))
                              roots
                              :initial-value (list 1)))
           (ascaled (mapcar (um:curry 'm* a-rand) polycoffs)))
      (cons (m+ key (car ascaled))
            (cdr ascaled))
      )))
         
(defun index-at-level (key level)
  ;; for a key residing at level, compute the child ptr index
  (let ((dvsr (expt +ptrs-per-bucket+ level)))
    (mod (truncate key dvsr) +ptrs-per-bucket+)))

(defun prealloc-keys (buckets &optional path)
  ;; Generate fresh skeys for all the buckets in the vector of buckets.
  ;; Keys need to reflect the path in their LSB, to match each bucket path.
  (let* ((level (length path)))
    (labels ((occupancy (bucket)
               ;; total number of keys present for this bucket
               (+ (length (bucket-avail-keys bucket))
                  (length (bucket-alloc-keys bucket)))))
      (um:nlet-tail iter ()
        (if (every (lambda (bucket)
                     (= +ptrs-per-bucket+ (occupancy bucket)))
                   buckets)
            ;; when all buckets are filled to capacity, we are
            ;; finished
            buckets
          (let* ((key  (new-key path))
                 (ix   (index-at-level key level))
                 (buck (aref buckets ix)))
            (when (< (occupancy buck) +ptrs-per-bucket+)
              (push key (bucket-avail-keys buck)))
            (iter))
          )))))

;; Code is written in purely functional form, to the extent that
;; once a bucket has been inserted into the tree, its ptrs can
;; no longer be mutated.
;;
;; Fresh copies of ptr buckets will be constructed
;; and the original tree will be pushed into the history
;; list before its top node is replaced in the global state.
;;
;; This helps to mimic the behavior of the registry UTXOs
;; saved in the blockchain.

(defun split-bucket (old-coffs)
  (let* ((path       (bucket-path old-coffs))
         (level      (length path))
         ;; form a replacement bucket to hold pointers
         (subtree-ptrs (make-instance 'ptr-bucket))
         (child-vec    (bucket-ptrs subtree-ptrs))
         ;; form a new coffs bucket with children to replace the old one
         (new-bucket (make-instance 'coff-bucket
                                    :path     path
                                    :top-p    (bucket-top-p old-coffs)
                                    :epoch    (group-epoch *group-state*)
                                    :key      (bucket-key old-coffs)
                                    :parent   (bucket-parent old-coffs)
                                    :children subtree-ptrs))
         (parent-roots  nil))
    ;; assign new coefficient buckets to each pointer in the replacement bucket.
    (loop for ix from 0 below +ptrs-per-bucket+ do
          (let* ((new-path (append path (list ix)))
                 (key      (new-key)))
            (push key parent-roots)
            (setf (aref child-vec ix)
                  (make-instance 'coff-bucket
                                 :path   new-path
                                 :key    key
                                 :parent new-bucket))))

    ;; fill in the coffs for the parent coff bucket
    (setf (bucket-coffs new-bucket)
          (compute-coffs (bucket-key new-bucket) parent-roots))
    
    ;; distribute the current keys into the subtree buckets
    (loop for pair in (bucket-alloc-keys old-coffs) do ;; pair = (pubkey . secrkey)
          (let* ((ix         (index-at-level (cdr pair) level))
                 (dst-bucket (aref child-vec ix)))
            (push pair (bucket-alloc-keys dst-bucket))))

    ;; fill out the subtree buckets with more prealloc secret keys
    (prealloc-keys child-vec path)

    (loop for bucket across child-vec do
          (let ((avail-keys (bucket-avail-keys bucket)))
            (setf (group-avail-skeys *group-state*)
                  (append avail-keys (group-avail-skeys *group-state*))
                  (bucket-coffs bucket)
                  (compute-coffs (bucket-key bucket)
                                 (append (mapcar 'cdr (bucket-alloc-keys bucket))
                                         avail-keys))
                  )))
    (update-parent new-bucket old-coffs)
    new-bucket))

(defmethod subtree-ptrs ((bucket coff-bucket))
  (bucket-ptrs (bucket-children bucket)))

(defun update-parent (new-bucket old-bucket)
  (let ((old-parent  (bucket-parent new-bucket)))
    (cond (old-parent
           
           ;; sanity checks
           (let ((pos (position old-bucket (subtree-ptrs old-parent))))
             ;; old bucket should be in the child vector of the parent node
             (assert pos)
             ;; position of pointer should match the tail of the path
             (assert (= pos (um:last1 (bucket-path old-bucket)))))

           ;; regenerate the parent bucket with a fresh pointer to the new node
           (let* ((child-vec (map 'vector (lambda (ptr)
                                            (if (eql ptr old-bucket)
                                                new-bucket
                                              ptr))
                                  (subtree-ptrs old-parent)))
                  (new-subtree (make-instance 'ptr-bucket
                                              :ptrs  child-vec
                                              ))
                  (new-parent  (make-instance 'coff-bucket
                                              :key      (bucket-key old-parent)
                                              :coffs    (bucket-coffs old-parent)
                                              :parent   (bucket-parent old-parent)
                                              :path     (bucket-path old-parent)
                                              :top-p    (bucket-top-p old-parent)
                                              :epoch    (group-epoch *group-state*)
                                              :children new-subtree)))
             ;; Adjust the back pointer in all subtrees to point to
             ;; new parent.
             ;;
             ;; This is internal state only, not present in UTXOs so
             ;; no need to rewrite the other child nodes to the
             ;; blockchain.
             (map nil (lambda (child)
                        (setf (bucket-parent child) new-parent))
                  child-vec)
             ;; update the parents along the path to top node
             (update-parent new-parent old-parent)))

          (t
           ;; we must be at top node
           (setf (group-top *group-state*) new-bucket))
          )))

;; ----------------------------------------------------------------
;; Locating Coefficient Buckets by key

(defun find-bucket (key &key (start (group-top *group-state*)) dum-probe)
  ;; Find coff bucket where key should be.

  ;; Key would normally be a skey, but for adding new members we pass
  ;; the pkey as a random selector when we need to split a leaf node
  ;; [dum-probe = t]
  (um:nlet-tail iter ((bucket start)
                      (level  0))
    (cond ((leaf-bucket-p bucket)
           (values bucket
                   (or dum-probe
                       (find key (bucket-avail-keys bucket))
                       (find key (bucket-alloc-keys bucket)
                             :key 'cdr))))

          (t 
           (let* ((ix  (index-at-level key level)))
             (iter (aref (subtree-ptrs bucket) ix)
                   (1+ level))))
          )))

(defmethod find-epoch-encr-skey (epoch)
  (let* ((top (find epoch (cons (group-top *group-state*)
                                (group-history *group-state*))
                    :key 'bucket-epoch)))
    (when top
      (bucket-key top))))

;; -----------------------------------------------------------------
;; System State Init

(defun make-top-bucket ()
  ;; do this just once to construct initial tree
  (let* ((key           (new-key))
         (pkey          (owner-pkey *group-state*))
         (avail-keys    (loop repeat 16 collect (new-key)))
         (coffs         (compute-coffs key avail-keys))
         (assoc         (cons pkey (pop avail-keys)))
         (alloc-keys    (list assoc)))
    (setf (group-avail-skeys *group-state*) avail-keys
          (gethash pkey (group-members *group-state*)) assoc
          (group-top *group-state*) (make-instance 'coff-bucket
                                                   :top-p      t
                                                   :epoch      (group-epoch *group-state*)
                                                   :key        key
                                                   :coffs      coffs
                                                   :alloc-keys alloc-keys
                                                   :avail-keys avail-keys))
    (generate-utxo (group-claim *group-state*))
    (generate-utxo (group-top   *group-state*))
    ))
          

;; -----------------------------------------------------------------------
;; Adding New Members

(defun tell-member-his-key (pkey skey)
  (generate-p2p
   :to-pkey       pkey
   :from-nickname (group-nickname *group-state*)
   :from-skey     (owner-skey *group-state*)
   :msg           (list :secret-grp-key skey)
   ))

(defun add-member-to-tree (pkey)
  (let ((pair (gethash pkey (group-members *group-state*))))
    (if pair
        ;; already a member - just tell him his key again...
        (tell-member-his-key pkey (cdr pair))
      
      ;; else - not already a member
      (um:nlet-tail iter ()
        (cond ((group-avail-skeys *group-state*)
               ;; we have available skeys, so peel one off and hand back to user.
               (let* ((skey   (pop (group-avail-skeys *group-state*)))
                      (assoc  (cons pkey skey)))
                 ;; update the internal state of the bucket assigned to the new member
                 (multiple-value-bind (bucket found) (find-bucket skey)
                   
                   ;; basic sanity checking
                   (assert found)
                   ;; the skey should also be in the list of available keys for this node
                   (assert (member skey (bucket-avail-keys bucket)))
                   
                   ;; update internal state
                   (push assoc (bucket-alloc-keys bucket)) ;; skey now allocated to member
                   (setf (gethash pkey (group-members *group-state*)) assoc)
                 
                   (setf (bucket-avail-keys bucket) ;; remove skey from available list
                         (delete skey (bucket-avail-keys bucket)))
                   
                   ;; tell the user his new group secret key
                   (tell-member-his-key pkey skey)
                   )))
              
              (t
               ;; We were out of available secret keys. So split a leaf
               ;; bucket and generate some more skeys
               
               ;; use pkey as probe key
               ;; this will randomly select a bucket to become split
               ;; and generate more available keys
               (new-epoch *group-state*)
               (split-bucket (find-bucket pkey :dum-probe t))
               (iter)) ;; try again
              )))))

;; When multiple operations are performed, there will be many
;; intermediate states that users will never have to see if we thin
;; out the changes to the minimum set of changes needed to bring the
;; system to the new state, before writing a bunch of state UTXOs

(defmacro with-thinned-utxos (&body body)
  `(do-with-thinned-utxos (lambda ()
                           ,@body)))

(defun do-with-thinned-utxos (fn)
  (let* ((sav-top  (group-top *group-state*))
         (*pending-utxos* nil))
    (funcall fn)
    (unless (eql (group-top *group-state*) sav-top)
      ;; if top node has not changed, then nothing has changed.
      (when sav-top
        (setf (group-history *group-state*)
              (nconc (group-history *group-state*) (list sav-top))))
      (generate-registry-utxos (group-top *group-state*) sav-top))
    (write-utxos)
    ))
       
(defun add-member (pkey)
  (with-thinned-utxos
   (add-member-to-tree pkey)))

(defun add-members (pkeys)
  (with-thinned-utxos
   (dolist (pkey pkeys)
     (add-member-to-tree pkey))
   ))

;; -----------------------------------------------------------------------
;; Removing Members

(defun remove-member-from-tree (pkey)
  (let ((pair  (gethash pkey (group-members *group-state*))))
    (when pair
      (new-epoch *group-state*)
      (remhash pkey (group-members *group-state*))
      (let* ((skey (cdr pair)))
        (multiple-value-bind (bucket found) (find-bucket skey)

          ;; sanity checks
          (assert found)
          ;; this should be a leaf bucket
          (assert (leaf-bucket-p bucket))
          ;; skey and pkey should exist in the list of allocated keys for the bucket
          (let ((alloc (find skey (bucket-alloc-keys bucket) :key 'cdr)))
            (assert alloc)
            (assert (eql pkey (car alloc))))

          ;; remove the key pair from the alloc list
          ;; invent a fresh available key
          ;; and reconstruct the leaf bucket with fresh keying
          (let* ((new-skey   (new-key (bucket-path bucket)))
                 (new-bucket (make-instance 'coff-bucket
                                            :path       (bucket-path bucket)
                                            :avail-keys (cons new-skey (bucket-avail-keys bucket))
                                            :alloc-keys (remove skey (bucket-alloc-keys bucket) :key 'cdr)
                                            :key        (new-key)
                                            :parent     (bucket-parent bucket) ;; temp
                                            :epoch      (group-epoch *group-state*)
                                            :top-p      (bucket-top-p bucket)
                                            )))
            ;; compute new polynomial for the new keying
            (setf (bucket-coffs new-bucket)
                  (compute-coffs (bucket-key new-bucket)
                                 (append (bucket-avail-keys new-bucket)
                                         (mapcar 'cdr (bucket-alloc-keys new-bucket)))))
            ;; make new skey available for new members
            (push new-skey (group-avail-skeys *group-state*))
            ;; fix up the path to the top node
            (rekey-parent new-bucket bucket)
            ))))
    ))

(defun rekey-parent (new-bucket old-bucket)
  (let ((old-parent  (bucket-parent new-bucket)))
    (cond (old-parent
           ;; basic sanity checks
           (let ((pos (position old-bucket (subtree-ptrs old-parent))))
             ;; the old bucket should be in the vector of children
             (assert pos)
             ;; and the last element of the old-bucket path should be the index
             ;; of the ptrs vector where the old-bucket is pointed to
             (assert (= pos (um:last1 (bucket-path old-bucket)))))

           ;; reconstruct the parent node with fresh keying and a new polynomial
           ;; pointing the parent to the new node
           (let* ((child-vec (map 'vector (lambda (ptr)
                                            (if (eql ptr old-bucket)
                                                new-bucket
                                              ptr))
                                  (subtree-ptrs old-parent)))
                  (new-subtree (make-instance 'ptr-bucket
                                              :ptrs  child-vec
                                              ))
                  (new-parent  (make-instance 'coff-bucket
                                              :key      (new-key)
                                              :parent   (bucket-parent old-parent)
                                              :path     (bucket-path old-parent)
                                              :top-p    (bucket-top-p old-parent)
                                              :epoch    (group-epoch *group-state*)
                                              :children new-subtree)))
             ;; Adjust the back pointer in all subtrees to point to
             ;; new parent.
             ;;
             ;; This is a mutating change to existing nodes in memory.
             ;; It is for internal state only, not present in UTXOs so
             ;; no need to rewrite the other child nodes to the
             ;; blockchain.
             (map nil (lambda (child)
                        (setf (bucket-parent child) new-parent))
                  child-vec)
             ;; compute new polynomial for new keying
             (setf (bucket-coffs new-parent)
                   (compute-coffs (bucket-key new-parent)
                                  (map 'list 'bucket-key child-vec)))
             ;; fixup parent path to top node
             (rekey-parent new-parent old-parent)))

          (t
           ;; we are at top node (node has no parent)
           ;; record this node as the top node
           (setf (group-top *group-state*) new-bucket))
          )))

(defun remove-member (pkey)
  (with-thinned-utxos
   (remove-member-from-tree pkey)))

(defun remove-members (pkeys)
  (with-thinned-utxos
   (dolist (pkey pkeys)
     (remove-member-from-tree pkey))
   ))

;; -----------------------------------------------------------------
;; Scanning Trees, Generating Registry UTXOs

(defun generate-registry-utxos (new-top old-top)
  (unless (eql new-top old-top)
    ;; in order to construct registry utxos on the blockchain, we must
    ;; spend our group claim utxo and reconstruct it
    (um:when-let (txin (bucket-utxo-id (group-claim *group-state*)))
      (push txin *pending-txins*))
    ;; generate the new structural registry utxos
    (dolist (bucket (collect-new-nodes new-top))
      (generate-utxo bucket))
    ;; push this one last, so that it becomes the first UTXO in a
    ;; registry change batch
    (generate-utxo (group-claim *group-state*))
    ))

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun scan-nodes (start &key (test 'true) (action 'identity))
  ;; general purpose in-order depth first scanning of trees
  (labels ((scanning (bucket)
             ;; depth-first traversal
             (when (funcall test bucket)
               (funcall action bucket)
               (let ((children (bucket-children bucket)))
                 (when children
                   (funcall action children)
                   (map 'nil #'scanning (bucket-ptrs children)))
                 ))))
    (scanning start)))

(defun collect-new-nodes (top-bucket)
  ;; Collect new nodes starting from top-bucket.
  ;; New nodes do not yet have UTXO ID assigned to them.
  ;;
  ;; Return list in reverse order so that we will generate utxo-ids
  ;; for leaf nodes before referring to them in higher nodes
  ;; during utxo generation
  (let ((ans  nil))
    (scan-nodes top-bucket
                :test (lambda (bucket)
                        (null (bucket-utxo-id bucket)))
                :action (lambda (bucket)
                          (push bucket ans)))
    ans))
  
;; ---------------------------------------------------------------------------------
;; Epoch reader marking, so we can do early trimming of registry if prior epoch
;; has been seen by all members

(defun mark-epoch (epoch pkey)
  ;; Called whenever a member tells me he read a group message,
  ;; or when I see a group message from him for the epoch.
  (let ((pair (gethash pkey (group-members *group-state*))))
    (when pair
      ;; ignore if he isn't a member making the claim
      (let ((top  (find epoch (cons (group-top *group-state*)
                                    (group-history *group-state*))
                        :key 'bucket-epoch)))
        (when top
          ;; just ignore the mention if no such epoch in history or current state
          (multiple-value-bind (bucket found) (find-bucket (cdr pair) :start top)
            ;; if not found, then he wasn't a member during that epoch
            ;; - so just ignore him
            (when found
              ;; record his pkey as a reader of this bucket
              (pushnew pair (bucket-readers bucket)
                       :key 'car)
              )))))))

(defun #1=fully-read-epoch-p (top)
  (scan-nodes top
              :action (lambda (bucket)
                        (when (and (leaf-bucket-p bucket)
                                   (< (length (bucket-readers bucket))
                                      (length (bucket-alloc-keys bucket))))
                          (return-from #1# nil))))
  t)

(defun collect-fully-read-epochs ()
  ;; Epochs in history that have been read by all members during that
  ;; epoch can be safely removed from registry history.
  
  ;; side effect trims history of all fully read epochs
  (multiple-value-bind (fully-read not-fully-read)
      (um:partition 'fully-read-epoch-p (group-history *group-state*)) ;; partition respects original order
    (setf (group-history *group-state*) not-fully-read)
    fully-read))

;; ---------------------------------------------------------------------------------
;; Writing UTXOs in a Transaction, also spending old history UTXOs as they expire

(defconstant +days-history+ 30)

(defun days- (date ndays)
  (- date (* ndays 86400))) ;; secs/day

(defun gather-expiring-nodes ()
  ;; History list is a list of prior top nodes ordered from early to
  ;; late epoch
  ;;
  ;; Collect together all epochs that have either been fully read or
  ;; else older than expiration age.  Collect the nodes in use by
  ;; those expiring epoch trees, then remove the nodes that are still
  ;; in use, either in current state or in newer history.  What is
  ;; left can be spent to remove from the registry.
  (let* ((fully-read  (collect-fully-read-epochs))
         (cutoff-date (days- (get-universal-time) +days-history+))
         (end   (position-if (um:rcurry '> cutoff-date) (group-history *group-state*)
                            :key 'bucket-epoch))
         (older (nconc fully-read (subseq (group-history *group-state*) 0 end)))
         (newer (cons (group-top *group-state*)
                      (setf (group-history *group-state*)
                            (and end
                                 (nthcdr end (group-history *group-state*)))) ))
         (nodes (make-hash-table)))
    (dolist (top older)
      ;; collect old messages for disposal too
      (let* ((epoch (bucket-epoch top))
             (msgs  (gethash epoch (group-messages *group-state*))))
        (remhash epoch (group-messages *group-state*))
        (setf *pending-txins* (nconc msgs *pending-txins*)))
      (scan-nodes top
                  :action (lambda (node)
                            (setf (gethash node nodes) node))
                  ))
    (dolist (top newer)
      (scan-nodes top
                  :action (lambda (node)
                            (remhash node nodes))
                  ))
    (setf *pending-txins* (nconc (collect-node-txins nodes)
                                 *pending-txins*))
    ))

(defun collect-node-txins (nodes)
  ;; Nodes should be a hash table of nodes with assigned utxo-ids. We
  ;; collect a list of those utxo ids to be used as txins.
    (um:accum acc
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (acc (bucket-utxo-id v)))
               nodes)))

(defun write-utxos ()
  (when *pending-utxos*
    (gather-expiring-nodes)
    (let ((txins  (or (shiftf *pending-txins* nil)
                      (list (get-spendable-txin *wallet-state*))))
          (txouts (shiftf *pending-utxos* nil)))
      ;; simulate writing UTXOs to the blockchain in a
      ;; transaction
      (publish-transaction txins txouts)
      )))

;; ----------------------------------------------------------------
;; Viewing Trees on Owner Node

(defmethod node-children ((bucket coff-bucket))
  (let ((children (bucket-children bucket)))
    (when children
      (coerce (bucket-ptrs children) 'list))))

(defun format-node-id (node-id)
  (vec-repr::short-str (hex-str node-id)))

(defmethod print-node ((bucket coff-bucket))
  (format nil "~A Coffs" (format-node-id (bucket-node-id bucket))))

(defun view-tree ()
  (capi:contain
   (make-instance 'capi:graph-pane
                  :layout-function :left-right
                  :interaction :single-selection
                  :roots (list (group-top *group-state*))
                  :children-function 'node-children
                  :print-function 'print-node
                  :action-callback (lambda (item intf)
                                     (declare (ignore intf))
                                     (inspect item))
                  )))

;; -----------------------------------------------------------------------------------
;; Prototyping Simulator Handlers

(defun start-group (wallet nickname init-members)
  (multiple-value-bind (grp-skey grp-pkey)
      (make-sim-keys (wallet-skey wallet) :group-owner nickname)
    (let* ((group-claim (make-instance 'registry-claim
                                       :nickname nickname
                                       :pkey     grp-pkey
                                       :skey     grp-skey))
           (new-group   (make-instance 'group-owner
                                       :claim group-claim))
           (*pending-txins*  (list (get-spendable-txin wallet)))
           (*pending-utxos*  nil))
      (with-owned-group (new-group)
        (make-top-bucket)
        (dolist (pkey init-members)
          (add-member-to-tree pkey))
        (publish-transaction *pending-txins* *pending-utxos*)
        new-group))))

(defmethod add-member-to-group ((group group-owner) pkey)
  (with-owned-group (group)
    (add-member pkey)))

(defmethod add-members-to-group ((group group-owner) pkeys)
  (with-owned-group (group)
    (add-members pkeys)))

(defmethod remove-member-from-group ((group group-owner) pkey)
  (with-owned-group (group)
    (remove-member pkey)))

(defmethod view-tree-for-group ((group group-owner))
  (with-owned-group (group)
    (view-tree)))

(defun stash-message (epoch msg)
  (let ((msgs (gethash epoch (group-messages *group-state*))))
    (push msg msgs)
    (setf (gethash epoch (group-messages *group-state*)) msgs)))

(defmethod handle-owned-chat-message ((group group-owner) wallet txout)
  ;;
  ;; Then retain in msg history by epoch
  (let* ((utxo              (second txout))
         (epoch             (getf utxo :epoch)))
    (with-owned-group (group)
      ;; since this is a group message,
      ;; and it was sent to me as group owner,
      ;; I have to retain message in history so that
      ;; other group members can read it too.
      ;;
      ;; (even if totally invalid message - since I must spend it later
      ;; to keep blockchain trimmed)
      (stash-message epoch txout)
      
      (um:when-let (skey (find-epoch-encr-skey epoch))
        ;; retain message in history
        ;; message was sent to me, so I have to spend it later
        ;; but we retain for other group members to view it too
        
        (multiple-value-bind (from-pkey msg)
            (decode-group-message wallet skey utxo)
          (when from-pkey
            ;; messge decoded properly
            (handle-decoded-message wallet from-pkey msg)
            ))))))

(defun tear-down-group (group)
  ;; remove the group entirely from the registry
  (with-owned-group (group)
    (let ((nodes nil))
      ;; collect all nodes from current state and history
      (dolist (top (cons (group-top group) (group-history group)))
        (let ((epoch (bucket-epoch top)))
          (setf *pending-txins* (nconc (gethash epoch (group-messages group))
                                       *pending-txins*))
          (remhash epoch (group-messages group)))
        (scan-nodes top
                    :action (lambda (node)
                              (setf (gethash node nodes) node))))
      (setf *pending-txins* (nconc (collect-node-txins nodes)
                                   *pending-txins*))
      )))

