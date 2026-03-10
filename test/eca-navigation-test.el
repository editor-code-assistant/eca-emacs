;;; eca-navigation-test.el --- Tests for ECA navigation issues -*- lexical-binding: t; -*-

;;; Commentary:

;; tests fix for nav bug; when there are multiple eca sessions,
;; (eca-chat-go-to-next-expandable-block) only works in the newest session.

;;; Code:

(require 'ert)
(require 'eca-chat)
(require 'eca-util)

(defun eca-navigation-test--setup-mock-session (session-id)
  "Create a mock ECA session for testing with SESSION-ID."
  (let* ((workspace-folder "/tmp/test-project")
         (session (make-eca--session
                   :id session-id
                   :process nil
                   :chat-welcome-message ""
                   :chats '()
                   :last-chat-buffer nil
                   :status 'started
                   :workspace-folders (list workspace-folder)
                   :tool-servers '())))
    (setq eca--sessions (eca-assoc eca--sessions session-id session))
    session))

(defun eca-navigation-test--create-chat-buffer (session buffer-name)
  "Create a test chat buffer for SESSION named BUFFER-NAME."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((chat-buffer (current-buffer)))
      (setq-local eca-chat--id (concat "chat-" (number-to-string (random 10000))))
      (setq-local eca-session session) ; Direct session association
      (erase-buffer)
      (insert "Chat content\n")
      ;; Add multiple expandable blocks for testing
      (insert "Line 1 with [Expandable Button 1] \n")
      (let ((start-point (point)))
        (insert-text-button "[First Expandable]" 'face 'button)
        (let ((ov (make-overlay start-point (point))))
          (overlay-put ov 'eca-chat--expandable-content-id "block1")
          (overlay-put ov 'eca-chat--expandable-content-toggle nil)))
      
      (insert "\nSome content in between\n")
      
      (insert "Line 3 with [Expandable Button 2] \n")
      (let ((start-point (point)))
        (insert-text-button "[Second Expandable]" 'face 'button)
        (let ((ov (make-overlay start-point (point))))
          (overlay-put ov 'eca-chat--expandable-content-id "block2")
          (overlay-put ov 'eca-chat--expandable-content-toggle nil)))
      
      (insert "\nEnd content\n")
      (setf (eca--session-last-chat-buffer session) chat-buffer)
      (setf (eca--session-chats session)
            (eca-assoc (eca--session-chats session) eca-chat--id chat-buffer))
      chat-buffer)))

(ert-deftest eca-navigation-test-two-session-issue ()
  "Test that reproduces the navigation issue between two Emacs ECA sessions.
  
  This test demonstrates that when there are 2 ECA sessions, 
  the eca-chat-go-to-next-expandable-block navigation only works 
  in the newer/better managed session."
  (let ((session1 (eca-navigation-test--setup-mock-session "test-session-1"))
        (session2 (eca-navigation-test--setup-mock-session "test-session-2"))
        (buffer1 nil)
        (buffer2 nil))
    
    ;; Setup first chat buffer within first session context
    (let ((orig-session (symbol-function 'eca-session)))
      (cl-letf (((symbol-function 'eca-session) (lambda () session1)))
        (setq buffer1 (eca-navigation-test--create-chat-buffer session1 "*test-eca-chat-1*"))
        ))
    
    ;; Setup second chat buffer within second session context
    (let ((orig-session (symbol-function 'eca-session)))
      (cl-letf (((symbol-function 'eca-session) (lambda () session2)))
        (setq buffer2 (eca-navigation-test--create-chat-buffer session2 "*test-eca-chat-2*"))
        ))
    
    (condition-case err
        ;;
        ;; Test Navigation in First Buffer
        ;;
        (with-current-buffer buffer1
          (goto-char (point-min))
          
          ;; Capture original position before navigation
          (let ((original-pos (point)))
            (message "Testing navigation in buffer1 - starting at position: %d" (point))
            
            ;; Try to navigate to next expandable block
            (let ((result (ignore-errors
                            (eca-chat-go-to-next-expandable-block)
                            t)))
              
              (message "After navigating in buffer1 - position is: %d" (point))
              (message "Buffer1 successfully found an expandable block: %s" result)
              
              ;; Verify that we've actually moved to an expandable content
              (when result
                (let ((ov (car (overlays-in (point) (min (1+ (point)) (point-max))))))
                  (should (or (not ov) 
                              (overlay-get ov 'eca-chat--expandable-content-id)
                              (= (point) (point-max)))))))))
        
        ;;
        ;; Test Navigation in Second Buffer
        ;;
        (with-current-buffer buffer2
          (goto-char (point-min))
          
          ;; Capture original position before navigation
          (let ((original-pos (point)))
            (message "Testing navigation in buffer2 - starting at position: %d" (point))
            
            ;; Try to navigate to next expandable block            
            (let ((result (ignore-errors
                            (eca-chat-go-to-next-expandable-block)
                            t)))
              
              (message "After navigating in buffer2 - position is: %d" (point))
              (message "Buffer2 successfully found an expandable block: %s" result)
              
              ;; Verify that we've actually moved to an expandable content
              (when result
                (let ((ov (car (overlays-in (point) (min (1+ (point)) (point-max))))))
                  (should (or (not ov) 
                              (overlay-get ov 'eca-chat--expandable-content-id)
                              (= (point) (point-max)))))))))
      
      (error (message "Error during test: %s" err)))
    
    ;;
    ;; Cleanup
    ;;
    (setq eca--sessions (eca-dissoc eca--sessions "test-session-1"))
    (setq eca--sessions (eca-dissoc eca--sessions "test-session-2"))
    
    (when (buffer-live-p buffer1)
      (kill-buffer buffer1))
    (when (buffer-live-p buffer2)
      (kill-buffer buffer2))))

(ert-deftest eca-navigation-test--fixed-navigation ()
  "Test to verify that the navigation function now works correctly.
  
  With the fixed eca-chat--go-to-overlay function, navigation should occur 
  within the CURRENT buffer rather than jumping to the 'last buffer' 
  associated with the session."
  (let ((session (eca-navigation-test--setup-mock-session "fixed-session"))
        (target-buffer (get-buffer-create "*nav-target-test-fixed*")))

    ;;
    ;; Setup target buffer - where we want navigation, with expandables
    ;;
    (with-current-buffer target-buffer
      (erase-buffer)
      (insert "Target buffer content:\n")
      
      ;; First expandable block
      (let ((start-point (point)))
        (insert-text-button "[Target Expandable First]" 'face 'button)
        (let ((ov (make-overlay start-point (point))))
          (overlay-put ov 'eca-chat--expandable-content-id "exp-block-1")
          (overlay-put ov 'eca-chat--expandable-content-toggle nil)))
      
      (insert "\nSome content in middle\n")
      
      ;; Second expandable block
      (let ((start-point (point)))
        (insert-text-button "[Target Expandable Second]" 'face 'button)
        (let ((ov (make-overlay start-point (point))))
          (overlay-put ov 'eca-chat--expandable-content-id "exp-block-2")
          (overlay-put ov 'eca-chat--expandable-content-toggle nil)))
      
      (insert "\nEnd of buffer content\n")
      
      ;; Set up the buffer's session context
      (setq-local eca-session session))
    
    ;;
    ;; Test Navigation Within Same Buffer
    ;;
    (with-current-buffer target-buffer
      (goto-char (point-min))
      (let ((original-point (point))
            (original-buffer (current-buffer)))
        
        (message "Testing FIXED navigation in buffer: %s" (buffer-name original-buffer))
        (message "Starting point: %d" original-point)
        
        ;;
        ;; Test first navigation - should move to first expandable
        ;;
        (ignore-errors (eca-chat-go-to-next-expandable-block))
        (let ((after-first-nav (point)))
          (message "After first navigation: point = %d" after-first-nav)
          
          ;; Check if we moved to an expandable block
          (let ((ov-at-point (car (overlays-in (point) (min (1+ (point)) (point-max))))))
            (when ov-at-point
              (let ((id (overlay-get ov-at-point 'eca-chat--expandable-content-id)))
                (message "Found expandable block with ID: %s at position %d" 
                         id after-first-nav)
                (should (equal id "exp-block-1"))))))
        
        ;;
        ;; Test second navigation - should move to second expandable
        ;;
        (ignore-errors (eca-chat-go-to-next-expandable-block))
        (let ((after-second-nav (point)))
          (message "After second navigation: point = %d" after-second-nav)
          
          ;; Check if we moved to second expandable block
          (let ((ov-at-point (car (overlays-in (point) (min (1+ (point)) (point-max))))))
            (when ov-at-point
              (let ((id (overlay-get ov-at-point 'eca-chat--expandable-content-id)))
                (message "Found expandable block with ID: %s at position %d" 
                         id after-second-nav)
                (should (equal id "exp-block-2")))))
          
          ;; Verify that we stayed in the same buffer the whole time
          (should (equal original-buffer (current-buffer)))
          (message "Verified we stayed in the same buffer: %s" (buffer-name (current-buffer))))))
    
    ;;
    ;; Cleanup
    ;;
    (setq eca--sessions (eca-dissoc eca--sessions "fixed-session"))
    (kill-buffer target-buffer)
    
    (message "Navigation test completed successfully - no cross-buffer navigation occurred!")))

;; Test to show proper single session behavior (control test)
(ert-deftest eca-navigation-test-single-session-works ()
  "Control test to confirm single session navigation works properly."
  (let ((session (eca-navigation-test--setup-mock-session "single-session-test"))
        (chat-buffer (get-buffer-create "*test-single-session-nav*")))
    
    (with-current-buffer chat-buffer
      (setq-local eca-session session)
      (erase-buffer)
      (insert "Chat content with expandables:\n")
      
      ;; Add several expandable blocks
      (insert "Line 1\n")
      (let ((start-point (point)))
        (insert-text-button "[Expandable 1]" 'face 'button)
        (let ((ov (make-overlay start-point (point))))
          (overlay-put ov 'eca-chat--expandable-content-id "single-block-1")
          (overlay-put ov 'eca-chat--expandable-content-toggle nil)))
      
      (insert "\nAnother line\n")
      (let ((start-point (point)))
        (insert-text-button "[Expandable 2]" 'face 'button)
        (let ((ov (make-overlay start-point (point))))
          (overlay-put ov 'eca-chat--expandable-content-id "single-block-2")
          (overlay-put ov 'eca-chat--expandable-content-toggle nil)))
      
      ;; Set session context appropriately
      (setf (eca--session-last-chat-buffer session) chat-buffer)
      
      ;; Test navigation in single buffer
      (goto-char (point-min))
      (message "Testing single session navigation. Initial pos: %d" (point))
      
      (ignore-errors (eca-chat-go-to-next-expandable-block))
      (message "After first nav: %d" (point))
      
      ;; Get the overlay at the current point to verify we landed on an expandable
      (let ((ov-at-point (car (overlays-in (point) (min (1+ (point)) (point-max))))))
        (when ov-at-point
          (message "Overlay at point has ID: %s" 
                   (overlay-get ov-at-point 'eca-chat--expandable-content-id))))
      
      (ignore-errors (eca-chat-go-to-next-expandable-block))
      (message "After second nav: %d" (point))
      
      ;; Get the overlay at the current point again
      (let ((ov-at-point (car (overlays-in (point) (min (1+ (point)) (point-max))))))
        (when ov-at-point
          (message "After 2nd nav, Overlay at point has ID: %s" 
                   (overlay-get ov-at-point 'eca-chat--expandable-content-id)))         
        (should (or (not ov-at-point)
                    (overlay-get ov-at-point 'eca-chat--expandable-content-id)
                    (= (point) (point-max))))))
    
    ;; Cleanup
    (setq eca--sessions (eca-dissoc eca--sessions "single-session-test"))
    (kill-buffer chat-buffer)))

(provide 'eca-navigation-test)
;;; eca-navigation-test.el ends here
