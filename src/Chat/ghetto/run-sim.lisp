#|//! run-sim.lisp -- Run the Chat Simulator

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

(in-package :chat-ghetto)

#|
(progn
  (setf *print-base* 16)
  (setf *print-level* 6))
|#

(defvar *member* nil)
(defvar *owner*  nil)

(defun view-tree ()
  (ac:send *owner* :view-tree :test-group))

(defun view-keying (group-nickname)
  (ac:send *member* :keying-info group-nickname))

(defun ask-owner-to-start-group (grp-nickname)
  (ac:send *owner* :start-group grp-nickname))

(defun ask-member-to-join-group (&key grp-nickname mem-nickname)
  (ac:send *member* :join-group grp-nickname mem-nickname))

(defun show-blockchain ()
  (ac:send *blockchain* :show))

(defun show-owner ()
  (ac:send *owner* :show))

(defun show-member ()
  (ac:send *member* :show))

(defun sync (actor)
  (ac:ask actor :tell-ready))

(defun run ()
  ;; Final derived key should match that in the root block on owner
  ;; node. DoubleClick on root node in graph and see if key is as
  ;; reported here.
  (let ((group-nickname :test-group))
    (init-blockchain)
    (setf *member* (make-wallet :member 2400))
    (setf *owner*  (make-wallet :member 0))

    (ask-owner-to-start-group group-nickname)
    (sleep 1) ;; give system time to absorb the announcement of the group
    
    ;; All of the following sends execute immediately and without pause
    ;; from the REPL CLI.  They become enqueued in the Actor mailboxes
    ;; of the recipients.
    ;;
    ;; So it does no good to ask the *member* for its keying info
    ;; printout after each batch of additions and removals. These
    ;; printouts would happen before any substantial change occurs to
    ;; the blockchain.
    ;;
    ;; Instead, we ask the *member* to show his keying info after each
    ;; batch of UTXOs arrives from the blockchain. There should be 4
    ;; reports due to the following 4 group operations by the owner.
    ;;

    ;; This should cause a keying of nil, since we just subscribed,
    ;; the the request sends a UTXO carrying a group key back to the member.
    (ask-member-to-join-group
     :grp-nickname group-nickname
     :mem-nickname :member-2400)
    (sleep 1) ;; give system time to absorb the exhchanges
    (show-member)
    ;; (break)

    (when t
      ;; Add 15 more members - there were already two, *owner* and
      ;; *member*.  This should split a node, and also produce a keying
      ;; info from the member.
      (ac:send *owner* :add-members group-nickname
               (um:lc (multiple-value-bind (skey pkey)
                          (make-sim-keys :member ix)
                        (declare (ignore skey))
                        pkey)
                      (ix <.. 0 15)))
      (view-tree)
      
      ;; Add 1000 more members - this happens in a batch mode and should
      ;; only publish a supertransaction with all of the net changes at
      ;; the end. There should be no intermediate states reported to the
      ;; blockchain registry. Hence on *member* update you should see only
      ;; one additional keying printout.
      ;;
      ;; Because the registry structure changes, the member will also
      ;; update its internal copy of the tree and again report its keying
      ;; to us. Since only group growth has occurred, the keying should
      ;; look the same as before but with a later epoch.
      (ac:send *owner* :add-members group-nickname
               (um:lc (multiple-value-bind (skey pkey)
                          (make-sim-keys :member ix)
                        (declare (ignore skey))
                        pkey)
                      (ix <.. 16 1016)))
      (view-tree)
    
      ;; remove member #500 Since rekeying must occur after this removal,
      ;; the member keying report should display changed keying and a
      ;; later epoch.
      (multiple-value-bind (skey pkey)
          (make-sim-keys :member 500)
        (declare (ignore skey))
        (ac:send *owner* :remove-member group-nickname pkey))
      (view-tree))))
  

#|
;; to compare with inspector from graph
(view-keying :test-group)

|#

#| -- moved to .lispworks startup file
;; ------------------------------------------------------
;; Allow abbreviated display of all large integers, without having to
;; wrap them in special functions - this affects all displays,
;; printouts, inspectors, debuggers, etc. Very nice to have.

(defvar *print-bignum-abbrev*  t)

#| I don't need no steenking digits, man...
e.g.,
(setf *print-base* 16)
(setf *print-bignum-abbrev* nil)
*ed-r* -> 1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF77965C4DFD307348944D45FD166C971
(setf *print-bignum-abbrev* t)
*ed-r* -> 1FFFFFF..166C971
|#

(defun abbrev-str (str)
  (let ((len (length str)))
    (if (< len 17)
        str
      (concatenate 'string
              (subseq str 0 7)
              ".."
              (subseq str (- len 7)))
      )))

(lw:defadvice
    ((method print-object (integer t))
     bignum-around-print-object :around)
    (x out-stream)
  (if (or *print-readably*
          (not *print-bignum-abbrev*))
      (lw:call-next-advice x out-stream)
    (let ((str (with-output-to-string (s)
                 (lw:call-next-advice x s))))
      (princ (abbrev-str str) out-stream))))
;; ------------------------------------------------------
|#    

;; --------------------------------------------------
#|
;; Verify proper behavior from (run) after it finishes
;; (user must wait about 15 seconds for completion - even though
;; the REPL appears to be finished immediately.
(run)
(show-owner)  -- should show it running the test group
              -- should have no spendables remaining
(show-member) -- should show it successfully monitoring the test-group
              -- should show its p2p message for group keying in the spendables list
(view-keying :test-group) -- should show a reasonable display with the current group encryption key
|#
