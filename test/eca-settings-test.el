;;; eca-settings-test.el --- Tests for eca-settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for switching between the tabs of the settings panel.
;;; Code:

(require 'buttercup)
(require 'eca-settings)

(defun eca-settings-test--call-with-tabs (tabs fn)
  "Call FN with the buffers of settings TABS, in tab order.
TABS are (KEY . MAJOR-MODE) pairs, the only tabs registered while FN
runs.  Like the real tabs, each buffer enables its major mode, then
its tab-line.  The buffers are killed afterwards, and the selected
window shows back its previous buffer."
  (let ((eca-settings--tabs
         (mapcar (lambda (tab) (list :key (car tab) :label (car tab)))
                 tabs))
        (session (make-eca--session :id 1
                                    :workspace-folders '("/tmp/project")))
        (prev (window-buffer (selected-window)))
        (buffers '()))
    (spy-on 'eca-session :and-return-value session)
    (unwind-protect
        (progn
          (dolist (tab tabs)
            (with-current-buffer (eca-settings--create-buffer (car tab)
                                                              session)
              (setq buffers (append buffers (list (current-buffer))))
              (funcall (cdr tab))
              (eca-settings--setup-tab-line (car tab) session)))
          (funcall fn buffers))
      (when (buffer-live-p prev)
        (set-window-buffer (selected-window) prev))
      (mapc #'kill-buffer buffers))))

(defun eca-settings-test--press (keys buffers)
  "Press KEYS, then return the index in BUFFERS of the buffer shown."
  (execute-kbd-macro (kbd keys))
  (-elem-index (window-buffer (selected-window)) buffers))

(describe "eca-settings tab switching"

  (it "binds C-x <left>/<right> in settings buffers"
    (expect (lookup-key eca-settings-tabs-map (kbd "C-x <left>"))
            :to-be 'eca-settings-previous-tab)
    (expect (lookup-key eca-settings-tabs-map (kbd "C-x <right>"))
            :to-be 'eca-settings-next-tab)
    (expect (lookup-key eca-settings-tabs-map (kbd "C-x C-<left>"))
            :to-be 'eca-settings-previous-tab)
    (expect (lookup-key eca-settings-tabs-map (kbd "C-x C-<right>"))
            :to-be 'eca-settings-next-tab))

  (it "cycles through the tabs with C-x <left>/<right>, wrapping around"
    ;; Real command-loop dispatch, also from a tab running a major mode
    ;; of its own, like the global config one.  On Emacs 30,
    ;; `tab-line-mode-map' may shadow the keys with
    ;; `tab-line-switch-to-next-tab', which cycles the same tabs.
    (eca-settings-test--call-with-tabs
     '(("mcps" . eca-settings-mode)
       ("global-config" . js-mode)
       ("jobs" . eca-settings-mode))
     (lambda (buffers)
       (set-window-buffer (selected-window) (car buffers))
       (expect (eca-settings-test--press "C-x <right>" buffers) :to-equal 1)
       (expect (eca-settings-test--press "C-x <right>" buffers) :to-equal 2)
       (expect (eca-settings-test--press "C-x <right>" buffers) :to-equal 0)
       (expect (eca-settings-test--press "C-x <left>" buffers) :to-equal 2)
       (expect (eca-settings-test--press "C-x C-<left>" buffers) :to-equal 1)
       (expect (eca-settings-test--press "C-x C-<right>" buffers)
               :to-equal 2))))

  (it "falls back to previous/next-buffer without the tab-line"
    (spy-on 'previous-buffer)
    (spy-on 'next-buffer)
    (with-temp-buffer
      (setq-local tab-line-mode nil)
      (eca-settings-previous-tab 1)
      (eca-settings-next-tab 2)
      (expect 'previous-buffer :to-have-been-called-with 1)
      (expect 'next-buffer :to-have-been-called-with 2)))

  (it "leaves C-x <left>/<right> alone with eca-settings-tab-line off"
    (let ((eca-settings-tab-line nil))
      (eca-settings-test--call-with-tabs
       '(("mcps" . eca-settings-mode))
       (lambda (buffers)
         (with-current-buffer (car buffers)
           (expect (key-binding (kbd "C-x <left>"))
                   :to-be (global-key-binding (kbd "C-x <left>"))))))))

  (it "does not bind them in other buffers of the major mode of a tab"
    ;; Binding them with `local-set-key' would, as it modifies the
    ;; keymap shared by all the buffers of the major mode.
    (eca-settings-test--call-with-tabs
     '(("global-config" . js-mode))
     (lambda (_buffers)
       (with-temp-buffer
         (js-mode)
         (expect (key-binding (kbd "C-x <left>"))
                 :to-be (global-key-binding (kbd "C-x <left>"))))))))

(provide 'eca-settings-test)
;;; eca-settings-test.el ends here
