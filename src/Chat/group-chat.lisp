

(defpackage :group-chat
  (:use :cl :lw :core-crypto)
  (:export
   ))

(in-package :group-chat)

;; ------------------------------------------------

(defconstant +ptrs-per-bucket+ 16)

(defvar *secret-seed*  :secret)
(defvar *nonce*        0)
(defvar *avail-skeys*  nil)
(defvar *top-bucket*   nil)
(defvar *history*      nil)
(defvar *members*      nil)
(defvar *epoch*        0)

(defvar *pending-utxos* nil)

(defvar *owner-pkey*   nil)
(defvar *grp-nickname* nil)
(defvar *grp-claim*    nil)
(defvar *msg-history*  nil)

;; ----------------------------------------------

(defclass bucket ()
  ((utxo-id :accessor bucket-utxo-id  :initform nil)
   ))

(defmethod leaf-bucket-p ((bucket bucket))
  nil)

(defclass group-claim (bucket)
  ((nickname  :accessor group-nickname  :initarg :nickname)
   (pkey      :accessor group-pkey      :initarg :pkey)
   ))

(defclass ptr-bucket (bucket)
  ((ptrs  :accessor bucket-ptrs :initform (make-array +ptrs-per-bucket+) :initarg :ptrs)
   ))

(defclass coff-bucket (bucket)
  ((avail-keys :accessor bucket-avail-keys :initform nil :initarg :avail-keys)
   (alloc-keys :accessor bucket-alloc-keys :initform nil :initarg :alloc-keys)
   (key        :accessor bucket-key                      :initarg :key)
   (coffs      :accessor bucket-coffs                    :initarg :coffs)
   (parent     :accessor bucket-parent     :initform nil :initarg :parent)
   (epoch      :accessor bucket-epoch      :initform *epoch*)
   (path       :accessor bucket-path       :initform nil :initarg :path)
   (top-p      :accessor bucket-top-p      :initform nil :initarg :top-p)
   (children   :accessor bucket-children   :initform nil :initarg :children)
   (readers    :accessor bucket-readers    :initform nil)
   ))

(defmethod leaf-bucket-p ((bucket coff-bucket))
  (null (bucket-children bucket)))


(defclass p2p-message ()
  ((to  :accessor p2p-to  :initarg :to)
   (msg :accessor p2p-msg :initarg :msg)
   ))

;; -------------------------------------------------------------------
;; For simulated UTXO construction - make human readable UTXOs

(defun generate-utxo-id (utxo)
  ;; adds a bit of rondomness so that we don't repeat the same ID
  ;; during sim, when the utxo is struturally the same
  (hash/256 utxo (rfield-random)))

(defmethod generate-utxo ((bucket ptr-bucket))
  (let* ((utxo (list
                :type     :grp-ptr-utxo
                :to       (hex *owner-pkey*)
                :cloaked  nil
                :ptrs     (map 'vector 'bucket-utxo-id (bucket-ptrs bucket))
                ))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))

(defmethod generate-utxo ((bucket coff-bucket))
  (let* ((utxo (list
                :type     :grp-coffs-utxo
                :to       (hex *owner-pkey*)
                :cloaked  nil
                :epoch    (when (bucket-top-p bucket) ;; only top coffs block carries epoch
                            (assert (eql *epoch* (bucket-epoch bucket)))
                            *epoch*)
                :coffs    (mapcar 'hex (bucket-coffs bucket))
                :children (when (bucket-children bucket)
                            (hex (bucket-utxo-id (bucket-children bucket))))
                ))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))
  
(defmethod generate-utxo :around ((bucket bucket))
  (let ((utxo (call-next-method)))
    (setf (bucket-utxo-id bucket) (car utxo))
    utxo))

(defmethod generate-utxo ((msg p2p-message))
  (let* ((utxo  (list
                 :type    :p2p-utxo
                 :to      (hex (p2p-to msg))
                 :cloaked t
                 :msg     (p2p-msg msg)
                 ))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))

(defmethod generate-utxo :around (data)
  (let ((utxo (call-next-method)))
    (push utxo *pending-utxos*)
    utxo))

(defmethod generate-utxo ((msg group-claim))
  (let* ((utxo  (list
                 :type     :grp-claim-utxo
                 :to       (hex (group-pkey msg))
                 :cloaked  nil
                 :nickname (group-nickname msg)))
         (id  (generate-utxo-id utxo)))
    (list id utxo)))

;; ---------------------------------------------------------------------

(defun rfield-random ()
  ;; Non-deterministic random
  (random-between 2 *ed-r*))

(defun hash/fr (&rest args)
  ;; Deterministic random value from hash of args
  ;; mapped to field Fr
  (let* ((h  (int (apply 'hash/256 args))))
    (um:while (>= h *ed-r*)
      (setf h (ash h -1)))
    h))

#|
(um:lc (hash/fr :secret ix)
       (ix <.. 0 16))
|#

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
        (let ((key  (hash/fr *secret-seed* *nonce*)))
          (incf *nonce*)
          (if (key-match key)
              key
            (iter))
          ))
      )))

(defun compute-coffs (key roots)
  ;; compute a key cloaking polynomial
  ;; Q(x) = key + P(x), where P(x) = 0 at roots
  ;; P(x) = a_rand * (x - root1)*(x - root2)*...*(x - rootN)
  (with-mod *ed-r*
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
            (setf *avail-skeys* (append avail-keys *avail-skeys*)
                  (bucket-coffs bucket)
                  (compute-coffs (bucket-key bucket)
                                 (append (mapcar 'cdr (bucket-alloc-keys bucket))
                                         avail-keys))
                  )))
    (update-parent new-bucket old-coffs)
    new-bucket))

(defun update-parent (new-bucket old-bucket)
  (let ((old-parent  (bucket-parent new-bucket)))
    (cond (old-parent
           
           ;; sanity checks
           (let ((pos (position old-bucket (bucket-ptrs (bucket-children old-parent)))))
             ;; old bucket should be in the child vector of the parent node
             (assert pos)
             ;; position of pointer should match the tail of the path
             (assert (= pos (um:last1 (bucket-path old-bucket)))))

           ;; regenerate the parent bucket with a fresh pointer to the new node
           (let* ((child-vec (map 'vector (lambda (ptr)
                                            (if (eql ptr old-bucket)
                                                new-bucket
                                              ptr))
                                  (bucket-ptrs (bucket-children old-parent))))
                  (new-subtree (make-instance 'ptr-bucket
                                              :ptrs  child-vec
                                              ))
                  (new-parent  (make-instance 'coff-bucket
                                              :key      (bucket-key old-parent)
                                              :coffs    (bucket-coffs old-parent)
                                              :parent   (bucket-parent old-parent)
                                              :path     (bucket-path old-parent)
                                              :top-p    (bucket-top-p old-parent)
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
           ;; we should be at top node
           (assert (bucket-top-p new-bucket))
           (setf *top-bucket* new-bucket))
          )))

;; ----------------------------------------------------------------
;; Locating Coefficient Buckets by key

(defun find-bucket (key &key (start *top-bucket*) dum-probe)
  ;; Find coff bucket where key should be.

  ;; Key would normally be a skey, but for adding new members we pass
  ;; the pkey as a random selector when we need to split a leaf node
  ;; [dum-probe = t]
  (um:nlet-tail iter ((bucket start)
                      (level  0))
    (cond ((null (bucket-children bucket))
           (values bucket
                   (or dum-probe
                       (find key (bucket-avail-keys bucket))
                       (find key (bucket-alloc-keys bucket)
                             :key 'cdr))))

          (t 
           (let* ((ix  (index-at-level key level)))
             (iter (aref (bucket-ptrs (bucket-children bucket)) ix)
                   (1+ level))))
          )))

;; -----------------------------------------------------------------
;; System State Init

(defun make-top-bucket ()
  ;; do this just once to construct initial tree

  ;; find a better way to start off the randomness and keep the nonce
  (setf *nonce*       (get-universal-time)
        *secret-seed* (int (hash/256 (rfield-random) *nonce*)))
  
  (let* ((key        (new-key))
         (avail-keys (loop repeat 16 collect (new-key)))
         (coffs      (compute-coffs key avail-keys))
         (assoc      (cons *owner-pkey* (pop avail-keys)))
         (alloc-keys (list assoc)))
    (setf *avail-skeys*  avail-keys
          *history*      nil
          *members*      (make-hash-table)
          (gethash *owner-pkey* *members*) assoc
          *epoch*       (get-universal-time)
          *top-bucket*  (make-instance 'coff-bucket
                                       :top-p t
                                       :key   key
                                       :coffs coffs
                                       :alloc-keys alloc-keys
                                       :avail-keys avail-keys)
          *grp-claim*    (make-instance 'group-claim
                                        :pkey     *owner-pkey*
                                        :nickname *grp-nickname*)
          *msg-history*  (make-hash-table))
    ;; construct the registry entry with cloaked key
    (let ((*pending-utxos* nil))
      (generate-registry-utxos *top-bucket* nil)
      (write-utxos))
    *top-bucket*))

;; -----------------------------------------------------------------------
;; Adding New Members

(defun tell-member-his-key (pkey skey)
  (generate-utxo (make-instance 'p2p-message
                                :to pkey
                                :msg (list :secret-grp-key (hex skey)))
                 ))

(defun add-member-to-tree (pkey)
  (let ((pair (gethash pkey *members*)))
    (if pair
        ;; already a member - just tell him his key again...
        (tell-member-his-key pkey (cdr pair))
      
      ;; else - not already a member
      (um:nlet-tail iter ()
        (cond (*avail-skeys*
               ;; we have available skeys, so peel one off and hand back to user.
               (let* ((skey   (pop *avail-skeys*))
                      (assoc  (cons pkey skey)))
                 ;; update the internal state of the bucket assigned to the new member
                 (multiple-value-bind (bucket found) (find-bucket skey)
                   
                   ;; basic sanity checking
                   (assert found)
                   ;; the skey should also be in the list of available keys for this node
                   (assert (member skey (bucket-avail-keys bucket)))
                   
                   ;; update internal state
                   (push assoc (bucket-alloc-keys bucket)) ;; skey now allocated to member
                   (setf (gethash pkey *members*) assoc)
                 
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
               (setf *epoch* (get-universal-time))
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
  (let* ((sav-top  *top-bucket*)
         (*pending-utxos* nil))
    (funcall fn)
    (unless (eql *top-bucket* sav-top)
      ;; if top node has not changed, then nothing has changed.
      (setf *history* (nconc *history* (list sav-top)))
      (generate-registry-utxos *top-bucket* sav-top))
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
  (let ((pair  (gethash pkey *members*)))
    (when pair
      (setf *epoch* (get-universal-time))
      (remhash pkey *members*)
      (let* ((skey (cdr pair)))
        (multiple-value-bind (bucket found) (find-bucket skey)

          ;; sanity checks
          (assert found)
          ;; this should be a leaf bucket
          (assert (null (bucket-children bucket)))
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
                                            :top-p      (bucket-top-p bucket)
                                            )))
            ;; compute new polynomial for the new keying
            (setf (bucket-coffs new-bucket)
                  (compute-coffs (bucket-key new-bucket)
                                 (append (bucket-avail-keys new-bucket)
                                         (mapcar 'cdr (bucket-alloc-keys new-bucket)))))
            ;; make new skey available for new members
            (push new-skey *avail-skeys*)
            ;; fix up the path to the top node
            (rekey-parent new-bucket bucket)
            ))))
    ))

(defun rekey-parent (new-bucket old-bucket)
  (let ((old-parent  (bucket-parent new-bucket)))
    (cond (old-parent
           ;; basic sanity checks
           (let ((pos (position old-bucket (bucket-ptrs (bucket-children old-parent)))))
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
                                  (bucket-ptrs (bucket-children old-parent))))
                  (new-subtree (make-instance 'ptr-bucket
                                              :ptrs  child-vec
                                              ))
                  (new-parent  (make-instance 'coff-bucket
                                              :key      (new-key)
                                              :parent   (bucket-parent old-parent)
                                              :path     (bucket-path old-parent)
                                              :top-p    (bucket-top-p old-parent)
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
           (assert (bucket-top-p new-bucket))
           ;; record this node as the top node
           (setf *top-bucket* new-bucket))
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
    (push (or (bucket-utxo-id *grp-claim*)
              :zero-token-of-mine)
          *pending-txins*)
    (generate-utxo *grp-claim*)
    ;; generate the new structural registry utxos
    (dolist (bucket (collect-new-nodes new-top))
      (generate-utxo bucket))
    ))

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
  (let ((pair (gethash pkey *members*)))
    (when pair
      ;; ignore if he isn't a member making the claim
      (let ((top  (find epoch (cons *top-bucket* *history*)
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

(defun fully-read-epoch-p (top)
  (scan-nodes top
              :action (lambda (bucket)
                        (when (and (leaf-bucket-p bucket)
                                   (< (length (bucket-readers bucket))
                                      (length (bucket-alloc-keys bucket))))
                          (return-from fully-read-epoch-p nil))))
  t)

(defun collect-fully-read-epochs ()
  ;; Epochs in history that have been read by all members during that
  ;; epoch can be safely removed from registry history.
  
  ;; side effect trims history of all fully read epochs
  (multiple-value-bind (fully-read not-fully-read)
      (um:partition 'fully-read-epoch-p *history*) ;; partition respects original order
    (setf *history* not-fully-read)
    fully-read))

;; ---------------------------------------------------------------------------
;; Message Handlers...

(defun nyi (&rest ignored)
  (declare (ignore ignored))
  (error "Not yet implemented"))

(defun on-receive-group-message (msg)
  ;; decrypt message
  ;; lookup nickname of sender in registry to find pkey, or exit
  ;; validate signature on message, or exit
  ;; get epoch from message
  (nyi msg)
  (let ((epoch 'dummy))
    (let ((epoch-list (gethash epoch *msg-history*)))
      (push msg epoch-list)
      (setf (gethash epoch *msg-history*) epoch-list))
    ))

(defun on-receive-p2p-message (msg)
  ;; decrypt message
  ;; lookup nickname of sender in registry to find pkey, or exit
  ;; validate signature on message, or exit
  (nyi msg)
  (let ((msg-type  'dummy)
        (pkey      'dummy)
        (epoch     'dummy))
    ;; spend at next opportunity to keep blockchain cleaned
    (push msg *pending-txins*)
    (case msg-type
      (:ask-for-member-skey         (add-member pkey))
      (:i-read-a-message-for-epoch  (mark-epoch epoch pkey))
      )))

;; ---------------------------------------------------------------------------------
;; Writing UTXOs in a Transaction, also spending old history UTXOs as they expire

(defvar *dbg-actions* nil) ;; for debug - shows human readable transactions

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
         (end   (position-if (um:rcurry '> cutoff-date) *history*
                            :key 'bucket-epoch))
         (older (nconc fully-read (subseq *history* 0 end)))
         (newer (cons *top-bucket*
                      (setf *history* (and end
                                           (nthcdr end *history*)))))
         (nodes (make-hash-table)))
    (dolist (top older)
      ;; collect old messages for disposal too
      (let* ((epoch (bucket-epoch top))
             (msgs  (gethash epoch *msg-history*)))
        (remhash epoch *msg-history*)
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

(defun make-transaction (txins txouts)
  (update-sim-member-trees txouts) ;; simulate members reading the utxos in the blockchain
  (setf *dbg-actions* (list :txins  txins
                            :txouts txouts)))

(defun write-utxos ()
  (when *pending-utxos*
    ;; to send structural changes to the registry
    ;; we must spend something from the same group owner keywe must spend the group claim
    ;; and regenrate it - proves ownership of the group
    (gather-expiring-nodes)
    (let ((txins  (shiftf *pending-txins* nil))
          (txouts (shiftf *pending-utxos* nil)))
      ;; simulate writing UTXOs to the blockchain in a
      ;; transaction
      (make-transaction txins txouts)
      )))

(defun tear-down-group ()
  ;; remove the group entirely from the registry
  (let ((nodes nil))
    ;; collect all nodes from current state and history
    (dolist (top (cons *top-bucket* *history*))
      (let ((epoch (bucket-epoch top)))
        (setf *pending-txins* (nconc (gethash epoch *msg-history*) *pending-txins*))
        (remhash epoch *msg-history*))
      (scan-nodes top
                  :action (lambda (node)
                            (setf (gethash node nodes) node))))
    (let ((*pending-utxos* nil)
          ;; spend the nodes
          (txins (cons (bucket-utxo-id *grp-claim*)
                       (nconc (collect-node-txins nodes)
                              (shiftf *pending-txins* nil)))))
      ;; send a private message to myself as sole txout
      (generate-utxo (make-instance 'p2p-message
                                    :to  *owner-pkey*
                                    :msg :group-teardown))
      (make-transaction txins *pending-utxos*)
      )))

(defun transfer-group-to-new-owner (his-pkey my-pkey my-skey)
  ;; send system state and my-skey to new owner via p2p encrypted message
  (declare (ignore my-pkey my-skey))
  (nyi)

  ;; spend our group claim and generate a new one in new owner's pkey
  ;; to send public transfer of ownership to new owner
  (let ((*pending-utxos* nil))
    (generate-utxo (make-instance 'group-claim
                                  :pkey     his-pkey
                                  :nickname *grp-nickname*))
    (make-transaction (bucket-utxo-id *grp-claim*)
                      *pending-utxos*)
    ))

;; -----------------------------------------------------------------------------------
;; Prototyping Simulator

(defun sim-group ()
  (multiple-value-bind (own-skey own-pkey)
      (edec::make-deterministic-keys :testing)
    (declare (ignore own-skey))

    ;; establish a chat group - just owner at first
    (let ((*owner-pkey* (int own-pkey))
          (*grp-nickname* :grp-test))
      (make-top-bucket)
      (pprint *dbg-actions*)
      ;; (view-tree)

      ;; add 15 more members, should happen without any registry changes
      (add-members (um:accum acc
                     (dotimes (ix 15)
                       (multiple-value-bind (mem-skey mem-pkey)
                           (edec::make-deterministic-keys (list :member ix))
                         (declare (ignore mem-skey))
                         (acc (int mem-pkey))))))
      (pprint *dbg-actions*)
      ;; (view-tree)
      ;; (break)

      ;; add one more member making 17 of us, should cause node splitting
      (multiple-value-bind (mem-skey mem-pkey)
          (edec::make-deterministic-keys (list :member 16))
        (declare (ignore mem-skey))
        (add-member (int mem-pkey))
        (pprint *dbg-actions*))
      ;; (view-tree)
      ;; (break)

      ;; add 1000 more members - should cause lots of node splitting
      ;; but because of batching operation, only one additional
      ;; history entry
      (add-members (um:accum acc
                     (dotimes (ix 1000)
                       (multiple-value-bind (mem-skey mem-pkey)
                           (edec::make-deterministic-keys (list :member (+ ix 17)))
                         (declare (ignore mem-skey))
                         (acc (int mem-pkey))))))
      ;; (pprint *dbg-actions*)
      ;; (break)

      ;; remove member 500 - watch registry rekeying
      (multiple-value-bind (mem-skey mem-pkey)
          (edec::make-deterministic-keys (list :member 500))
        (declare (ignore mem-skey))
        (remove-member (int mem-pkey))
        (pprint *dbg-actions*))
      
      )))

;; ----------------------------------------------------------------
;; Viewing Trees on Owner Node

(defmethod node-children ((bucket coff-bucket))
  (let ((children (bucket-children bucket)))
    (when children
      (coerce (bucket-ptrs children) 'list))))

(defun format-utxo-id (utxo-id)
  (vec-repr::short-str (hex-str utxo-id)))

(defmethod print-node ((bucket coff-bucket))
  (format nil "~A Coffs" (format-utxo-id (bucket-utxo-id bucket))))

(defun view-tree ()
  (capi:contain
   (make-instance 'capi:graph-pane
                  :layout-function :left-right
                  :interaction :single-selection
                  :roots (list *top-bucket*)
                  :children-function 'node-children
                  :print-function 'print-node
                  :action-callback (lambda (item intf)
                                     (declare (ignore intf))
                                     (inspect item))
                  )))

;; -----------------------------------------------------------------
;; Simulator Member - builds tree in memory from UTXOs sent by group owner
;; pointers are all UTXO ID's (int here), and pointer deref is by way of
;; indirection through hash table keyed by utxo id.

(defvar *sim-top*   nil)
(defvar *sim-ptrs*  (make-hash-table))
(defvar *mem-pkey*  nil)
(defvar *mem-skey*  nil)
(defvar *mem-gskey* nil)
(defvar *encr-key*  nil)

(defun init-sim-member ()
  (setf *sim-top*   nil
        *sim-ptrs*  (make-hash-table)
        *mem-pkey*  nil
        *mem-skey*  nil
        *mem-gskey* nil
        *encr-key*  nil))

(defun update-sim-member-trees (txouts)
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
      (ecase (getf utxo :type)
        (:grp-claim-utxo)
        (:grp-ptr-utxo
         (let ((ptrs (make-instance 'ptr-bucket
                                    :ptrs (map 'vector 'int (getf utxo :ptrs)))))
           (setf (gethash (int utxo-id) *sim-ptrs*) ptrs)
           ))
        (:grp-coffs-utxo
         (let* ((epoch (getf utxo :epoch))
                (coffs (make-instance 'coff-bucket
                                     :coffs (mapcar 'int (getf utxo :coffs))
                                     :children (let ((ptr (getf utxo :children)))
                                                 (when ptr
                                                   (int ptr)))
                                     :top-p    epoch)))
           (setf (gethash (int utxo-id) *sim-ptrs*) coffs)
           (when epoch
             (setf *sim-top* (int utxo-id)))
           ))
        (:p2p-utxo
         (let* ((to  (getf utxo :to))
                (msg (getf utxo :msg)))
         (when (and *mem-pkey*
                    (= (int to) *mem-pkey*)
                    (eql (car msg) :secret-grp-key))
           (setf *mem-gskey* (int (cadr msg)))
           )))
        ))))

(defun tree-complete-p ()
  ;; return true if we have a complete current state at our end
  ;; If we succeed, we will also have trimmed out the old epoch
  ;; from our indirection pointers table.
  (let ((new-ptr-table (make-hash-table)))
    (ignore-errors
      (labels ((descend (ptr)
                 (let* ((node     (fetch ptr))
                        (children (bucket-children node)))
                   (setf (gethash ptr new-ptr-table) node)
                   (when children
                     (let ((ptrs  (fetch children)))
                       (setf (gethash children new-ptr-table) ptrs)
                       (map nil #'descend (bucket-ptrs ptrs))))
                   )))
        (descend *sim-top*)
        (setf *sim-ptrs* new-ptr-table) ;; be gone old epochs
        t))))

(defun make-member ()
  ;; make member 500, ask for a group secret key, then derive the
  ;; current encryption secret key
  (multiple-value-bind (mem-skey mem-pkey)
      (edec::make-deterministic-keys (list :member 400))
    (setf *mem-pkey* (int mem-pkey)
          *mem-skey* (int mem-skey))
    (add-member *mem-pkey*) ;; this would normally be via 1-1 message exchange
    (assert (tree-complete-p))
    (setf *encr-key* (find-encryption-key *mem-gskey*))))

(defun fetch (hash-ix)
  ;; pointer deref
  (gethash hash-ix *sim-ptrs*))

(defun find-encryption-key (skey)
  ;; given a member group secret key for himself, and the accumulated
  ;; state of the system obtained from watching utxo traffic, find the
  ;; group encryption key
  (labels ((solve-poly (node x)
             (let ((coffs (bucket-coffs node)))
               (reduce (lambda (ans coff)
                         (with-mod *ed-r*
                           (m+ coff (m* ans x))))
                       (reverse coffs)
                       :initial-value 0)))
           (descend (top level)
             (let* ((node     (fetch top))
                    (children (bucket-children node)))
               (if children
                   (let* ((ptr-node (fetch children))
                          (ix       (index-at-level skey level))
                          (child    (aref (bucket-ptrs ptr-node) ix))
                          (subkey   (descend child (1+ level))))
                     (solve-poly node subkey))
                 ;; else
                 (solve-poly node skey)))))
    (descend *sim-top* 0)))

                   
(defun run ()
  ;; Final derived key should match that in the root block on owner
  ;; node. DoubleClick on root node in graph and see if key is as
  ;; reported here.
  (init-sim-member)
  (sim-group)
  (view-tree)
  (make-member))


