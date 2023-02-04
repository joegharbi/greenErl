;; -*- coding: utf-8 -*-

;; This file is part of RefactorErl.
;;
;; RefactorErl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; RefactorErl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
;;
;; The Original Code is RefactorErl.
;;
;; The Initial Developer of the Original Code is Eötvös Loránd University.
;; Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
;; are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
;; and Ericsson Hungary. All Rights Reserved.

(provide 'refactorerl-mode)
(require 'refactorerl-operations)
(require 'refactorerl-query)
; (require 'refactorerl-metrics)
(require 'refactorerl-flasher)
(eval-when-compile
  (require 'cl))

(require 'refactorerl-custom-undo-mode)

;; Minor mode definition

(setf refactorerl-mode-keybindings
      (append refactorerl-mode-keybindings
              '(("Q"    refactorerl-quit)
                ("S"    refactorerl-restart)
                ("a"    refactorerl-f)
                ("d"    refactorerl-drop-file)
                ("D"    refactorerl-debug-shell)

;                ("m1"    refactorerl-start-metrics)
;                ("m2"    refactorerl-stop-metrics)

                ("G"    refactorerl-draw-graph)
                ("L"    refactorerl-load-dir)
                ("C"    refactorerl-list-files)
                ("\C-u" refactorerl-update-status)
                ("ct"   refactorerl-cluster-agglom-mod)
                ("cg"   refactorerl-cluster-genetic-mod)
                ("U"    refactorerl-buffer-undo)
                ("R"    refactorerl-buffer-redo)
                ("TT"   refactorerl-toggle-test))))

(flet ((define-keybindings (bindings prefix)
         (loop for (suffix fun) in bindings
               do (let ((key (if (listp suffix)
                                 (car suffix)
                               (concat prefix suffix))))
                    (define-key refactorerl-mode-map key fun)))))

  (define-keybindings refactorerl-mode-keybindings "\C-c\C-r"))

(defun* menu-from-group (group-name &key enable)
  (let ((menu-group (cdr (assq group-name refactorerl-menu-groups))))
    (unless menu-group
      (error "Menu group %s not defined" group-name))

    (cons `(,(menu-group-label menu-group) :enable ,enable)
          (menu-group-children menu-group))))


(setf refactorerl-mode-menu
      `((("Server")
         ("Start server" refactorerl-restart :enable (lambda () (not (refac-processes-are-running t))))
         ("Stop server" refactorerl-quit :enable :server)
         "--"
         ("Start metrics" refactorerl-start-metrics :enable :metricsoff)
         ("Stop metrics" refactorerl-stop-metrics :enable :metricson)
         "--"
         ("Configure" refactorerl-configure)
         ("Select control buffer" refactorerl-control-buffer :enable :server))
        (("Files")
         ("Load directory" refactorerl-load-dir :enable (lambda () (and :server (not refactorerl-is-readonly))))
         "--"
         ("Database contents" refactorerl-list-files :enable :server)
;; TODO         ("Show file details" refactorerl-server-show-files :enable :server)
         ("Show parse errors" refactorerl-show-errors :enable :server)
         ("Draw graph" refactorerl-draw-graph :enable (lambda () (and :server (not refactorerl-is-readonly))))
         "--"
         ("Reset database"  refactorerl-server-reset-db :enable (lambda () (and :server (not refactorerl-is-readonly))))
         )
        "--"
        ;("Add file" refactorerl-add-file :enable :buffer-file)
        ("Add file" refactorerl-add-file :enable (lambda () (and :buffer-file (not refactorerl-is-readonly))))
        ("Drop file" refactorerl-drop-file :enable (lambda () (and :buffer-file (not refactorerl-is-readonly))))
        ("Update status" refactorerl-update-status :enable :buffer-file)
		("Undo (one step only)" refactorerl-undo :help "Steps back on the database" :enable (lambda () (not refactorerl-custom-undo-mode)))
        (("Undo" :enable (lambda () (not refactorerl-is-readonly)))
		 ("last change" refactorerl-buffer-undo :enable 
		 (lambda () refac-undo-available))
		 "--"
		)
		(("Redo" :enable (lambda () (not refactorerl-is-readonly)))
		 ("last change" refactorerl-buffer-redo :enable 
		 (lambda () refac-redo-available))
		 "--"
		)
        "--"
        ("Clustering" refactorerl-full-cluster :enable :only-question)
        "--"
        ,(menu-from-group 'query)
;       ,(menu-from-group 'metrics)
        "--"
        ("Module and function dependecies" refactorerl-depanal-funmod :enable (lambda () (and :server (not refactorerl-is-readonly))))
	    ("Function block dependency analysis" refactorerl-depanal-fblock-default :enable (lambda () (and :server (not refactorerl-is-readonly))))
        "--"
        (("Duplicated code analysis")
         ("Search in selected area... (Matrix)" refactorerl-dupcode-selected-mx)
         ("Search in selected area... (Suffix tree)" refactorerl-dupcode-selected-st)
         ("Search in selected area... (SW metrics)" refactorerl-dupcode-selected-swm)
         ("Search in selected area... (Matrix filtered)" refactorerl-dupcode-selected-mxf)
         ("Search in selected area... (Filtered suffix tree)" refactorerl-dupcode-selected-fst)
        "--"
         ("Advanced search" refactorerl-dupcode-advanced)
         ("Show duplicates" refactorerl-show-dupcode))
        "--"
        ,(menu-from-group 'func :enable (lambda () (not refactorerl-is-readonly)))
        ,(menu-from-group 'introelim :enable (lambda () (not refactorerl-is-readonly)))
        ,(menu-from-group 'move :enable (lambda () (not refactorerl-is-readonly)))
        ,(menu-from-group 'rename :enable :server-not-readonly)
        ,(menu-from-group 'upgrade :enable :server-not-readonly)))

(defun refactorerl-expand-enabler (enabler)
  (case enabler
    ((:buffer-state)
     `(and (refac-processes-are-running (not (eq refactorerl-server-type 'internal))) (eq refac-buffer-state :ok)))
    ((:buffer-file)
     `buffer-file-name)
    ((:server)
     `(and (refac-processes-are-running (not (eq refactorerl-server-type 'internal)))))
    ((:wrangler)
     `(and (not (eq refactorerl-wrangler-path 'disabled))))
    ((:only-question)
     `(and (refac-processes-are-running (not (eq refactorerl-server-type 'internal))) (not is-user-asked)))
    ((:metricson)
     `(and (refac-processes-are-running (not (eq refactorerl-server-type 'internal))) refac-buffer-state-metrics))
    ((:metricsoff)
     `(and (refac-processes-are-running (not (eq refactorerl-server-type 'internal))) (not refac-buffer-state-metrics)))
    ((:server-not-readonly)
     `(and (refac-processes-are-running (not (eq refactorerl-server-type 'internal))) (not refactorerl-is-readonly)))
    (otherwise
     (list enabler))))

(defun* refactorerl-easy-menu (name items &key enable)
  `(,name
    ,@(when (and (not (featurep 'xemacs)) enable) `(:active ,(refactorerl-expand-enabler enable)))
    ,@(loop for menu-item in items
            collect (if (consp menu-item)
                        (if (stringp (car menu-item))
                            (destructuring-bind (label callback &key enable help) menu-item
                              `[,label ,callback :active ,(if enable
                                                              (refactorerl-expand-enabler enable)
                                                            't)])
                          (destructuring-bind ((label &key enable help) &body items) menu-item
                            (refactorerl-easy-menu label items :enable enable)))
                      menu-item))))

(easy-menu-define refactorerl-mode-easymenu refactorerl-mode-map "Refactor menu"
  (refactorerl-easy-menu "Refactor" refactorerl-mode-menu))

(defvar refac-buffer-state nil
  "Status of the file:
 - `:off': not part of the active refactoring set
 - `:err': there is an error in the file
 - `:ok': ready for refactoring
 - nil: unknown state (e.g. during parsing)")

(defvar refac-buffer-state-is-off nil)
(defvar refac-buffer-state-is-err nil)
(defvar refac-buffer-state-is-ok nil)
(defvar refac-buffer-state-is-nil nil)
(defvar refac-buffer-state-metrics nil) ;removed when global state added to this property

(defvar is-user-asked nil)

(defun refac-set-buffer-state (new-state)
  "refac-buffer-state needs to be set with this function so that
refac-buffer-state-is-* can be kept in sync. This is needed
because in XEmacs, there is no dynamic modeline lighter, and
thus, we have to make do with the very simple conditional
modeline facility it offers (see also the :lighter of
refactorerl-mode)"
  (setf refac-buffer-state new-state)
  (setf refac-buffer-state-is-off (eq new-state :off))
  (setf refac-buffer-state-is-err (eq new-state :err))
  (setf refac-buffer-state-is-ok (eq new-state :ok))
  (setf refac-buffer-state-is-nil (not new-state)))

(defmacro* define-minor-mode* (name doc &key lighter map menu on off)
  (let ((manual-menu-add (featurep 'xemacs)))
    `(define-minor-mode ,name ,doc
       :lighter ,lighter
       :map map
       (if ,name
           (progn
             ,(when manual-menu-add
                    `(easy-menu-add ,menu))
             ,on)
           (progn
             ,(when manual-menu-add
                    `(easy-menu-remove ,menu))
             ,off)))))

(define-minor-mode* refactorerl-mode
  "Minor mode providing access to Erlang Refactorer operations.
\\<refactorerl-mode-map>
The first time this mode is activated the refactorer server is started as a
subprocess. It should be stopped before leaving Emacs (`\\[refactorerl-quit]').

Before using this minor mode, you must customize `refactorerl-base-path'.

Key bindings:
\\{refactorerl-mode-map}
"
  :lighter (" Refact"
            (refactorerl-is-readonly "[RO]")
            (refac-buffer-state-is-err ":error" "")
            (refac-buffer-state-is-off ":off" "")
            (refac-buffer-state-is-nil ":???" ""))
            ;(refac-buffer-state-metrics " Metrics" ""))
  :map refactorerl-mode-map
  :menu refactorerl-mode-easymenu
  :on (progn
        (make-local-variable 'refac-buffer-state)
        ;; (make-local-variable 'refac-ephemeral-overlays)
        (loop for var in '(refac-buffer-state-is-off
                           refac-buffer-state-is-ok
                           refac-buffer-state-is-err
                           refac-buffer-state-is-nil
                         ;  refac-buffer-state-metrics
						   )
              do (make-local-variable var))
        (refactorerl-start)
        ;;(refactorerl-update-status)

        (refactorerl-flasher t)

        (add-hook 'after-save-hook 'refac-file-saved t t)
        (add-hook 'post-command-hook 'refac-remove-overlays nil t))
  :off (progn
         (setq refactorerl-readonly-requested nil)
         (refac-remove-stacked-overlays)
		 (when refactorerl-custom-undo-mode
		  (refactorerl-custom-undo-mode))
         (refactorerl-flasher nil)
         (remove-hook 'after-save-hook 'refac-file-saved t)))

(defun refactorerl-to-readonly ()
  (setq refactorerl-is-readonly t)
  (mapc (lambda (buff)
            (with-current-buffer buff
                (when (and  (boundp 'erlang-mode) erlang-mode refactorerl-mode)
                    (toggle-readonly t))))
        (buffer-list)))

(defun refactorerl-to-normal ()
  (interactive)
  (when (and (boundp 'refactorerl-mode) refactorerl-mode)
    (setq refactorerl-readonly-requested nil)
    (refactorerl-restart)
    (if (not refactorerl-is-readonly)
      (mapc (lambda (buff)
            (with-current-buffer buff
                (when (and (boundp 'erlang-mode) erlang-mode refactorerl-mode (file-readable-p (buffer-name)))
                    (toggle-readonly 0))))
            (buffer-list))
      (error "Refactorerl server type requires read-only mode."))))
  
(defun refactorerl-readonly-mode ()
  (interactive)
  (setq refactorerl-readonly-requested t)
  (if (and (boundp 'refactorerl-mode) refactorerl-mode)
    (refactorerl-to-readonly)
    (refactorerl-mode)))
