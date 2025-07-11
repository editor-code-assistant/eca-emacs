;;; eca-process.el --- ECA (Editor Code Assistant) process -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) process.
;;
;;; Code:

(require 's)
(require 'f)

(require 'eca-util)
(require 'eca-api)

(defcustom eca-custom-command nil
  "The eca server command.
If not provided, download and start eca automatically."
  :group 'eca
  :risky t
  :type '(repeat string))

(defcustom eca-server-download-url
  (format "https://github.com/editor-code-assistant/eca/releases/latest/download/eca-native-static-%s.zip"
          (let ((arch (car (split-string system-configuration "-"))))
            (pcase system-type
              ('gnu/linux (concat "linux-"
                                  (cond
                                   ((string= "x86_64" arch) "amd64")
                                   (t arch))))
              ('darwin (concat "macos-"
                               (cond
                                ((string= "x86_64" arch) "amd64")
                                (t arch))))
              ('windows-nt "windows-amd64"))))
  "The URL to download eca server."
  :group 'eca
  :type 'string)

(defcustom eca-server-install-path
  (f-join (expand-file-name
           (locate-user-emacs-file "eca"))
          (if (eq system-type 'windows-nt)
              "eca.exe"
            "eca"))
  "Directory in which eca will be downloaded."
  :risky t
  :type 'directory
  :group 'eca)

(defconst eca-ext-pwsh-script "pwsh -noprofile -noninteractive \
-nologo -ex bypass -c Expand-Archive -Path '%s' -DestinationPath '%s'"
  "Pwsh script to unzip file.")

(defconst eca-ext-powershell-script "powershell -noprofile -noninteractive \
-nologo -ex bypass -command Expand-Archive -path '%s' -dest '%s'"
  "Powershell script to unzip file.")

(defconst eca-ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'"
  "Unzip script to unzip file.")

(defcustom eca-unzip-script (lambda ()
                              (cond ((and (eq system-type 'windows-nt)
                                          (executable-find "pwsh"))
                                     eca-ext-pwsh-script)
                                    ((and (eq system-type 'windows-nt)
                                          (executable-find "powershell"))
                                     eca-ext-powershell-script)
                                    ((executable-find "unzip") eca-ext-unzip-script)
                                    ((executable-find "pwsh") eca-ext-pwsh-script)
                                    (t nil)))
  "The script to unzip downloaded eca server."
  :group 'eca
  :type 'string)

(defvar eca-process--buffer-name "<eca>")
(defvar eca-process--stderr-buffer-name "<eca:stderr>")

(defun eca-process--download-and-store-path ()
  "Return the path of the download and store."
  (let* ((store-path eca-server-install-path)
         (download-path (concat store-path ".zip")))
    `(,download-path . ,store-path)))

(defun eca-process--uninstall-server ()
  "Remove downloaded server."
  (-let (((download-path . store-path) (eca-process--download-and-store-path)))
    (when (f-exists? download-path) (f-delete download-path))
    (when (f-exists? store-path) (f-delete store-path))))

(defun eca-process--download-server (on-downloaded)
  "Download eca server calling ON-DOWNLOADED when success."
  (-let ((url eca-server-download-url)
         ((download-path . store-path) (eca-process--download-and-store-path)))
    (make-thread
     (lambda ()
       (condition-case err
           (progn
             (when (f-exists? download-path) (f-delete download-path))
             (when (f-exists? store-path) (f-delete store-path))
             (eca-info "Downloading eca server to %s..." download-path)
             (mkdir (f-parent download-path) t)
             (let ((inhibit-message t))
               (url-copy-file url download-path))
             (unless eca-unzip-script
               (error "Unable to find `unzip' or `powershell' on the path, please customize `eca-unzip-script'"))
             (shell-command (format (funcall eca-unzip-script) download-path (f-parent store-path)))
             (eca-info "Downloaded eca successfully")
             (funcall on-downloaded))
         (error "Could not download eca server" err))))))

(defun eca-process--server-command ()
  "Return the command to start server."
  (or eca-custom-command
      (when-let* ((command (executable-find "eca")))
        (list command "server"))
      (list eca-server-install-path "server")))

(defun eca-process--parse-header (s)
  "Parse string S as a ECA (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'eca-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (s-trim-left (substring s (+ 1 pos))))
    (when (equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun eca-process--filter (handle-msg _proc raw-output)
  "Process filter to parse eca's stdout RAW-OUTPUT delivering to HANDLE-MSG."
  (let ((body-received 0)
        leftovers body-length body chunk)
    (setf chunk (if (s-blank? leftovers)
                    (encode-coding-string raw-output 'utf-8-unix t)
                  (concat leftovers (encode-coding-string raw-output 'utf-8-unix t))))
    (let (messages)
      (while (not (s-blank? chunk))
        (if (not body-length)
            ;; Read headers
            (if-let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
                ;; We've got all the headers, handle them all at once:
                (setf body-length (let* ((headers (mapcar #'eca-process--parse-header
                                                          (split-string
                                                           (substring-no-properties chunk
                                                                                    (or (string-match-p "Content-Length" chunk)
                                                                                        (error "Unable to find Content-Length header"))
                                                                                    body-sep-pos)
                                                           "\r\n")))
                                         (content-length (cdr (assoc "Content-Length" headers))))
                                    (if content-length
                                        (string-to-number content-length)
                                      ;; This usually means either the server or our parser is
                                      ;; screwed up with a previous Content-Length
                                      (error "No Content-Length header")))
                      body-received 0
                      leftovers nil
                      chunk (substring-no-properties chunk (+ body-sep-pos 4)))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf leftovers chunk
                    chunk nil))
          (let* ((chunk-length (string-bytes chunk))
                 (left-to-receive (- body-length body-received))
                 (this-body (if (< left-to-receive chunk-length)
                                (prog1 (substring-no-properties chunk 0 left-to-receive)
                                  (setf chunk (substring-no-properties chunk left-to-receive)))
                              (prog1 chunk
                                (setf chunk nil))))
                 (body-bytes (string-bytes this-body)))
            (push this-body body)
            (setf body-received (+ body-received body-bytes))
            (when (>= chunk-length left-to-receive)
              (condition-case err
                  (with-temp-buffer
                    (apply #'insert
                           (nreverse
                            (prog1 body
                              (setf leftovers nil
                                    body-length nil
                                    body-received nil
                                    body nil))))
                    (decode-coding-region (point-min)
                                          (point-max)
                                          'utf-8)
                    (goto-char (point-min))
                    (push (eca-api--json-read-buffer) messages))

                (error
                 (eca-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                           (concat leftovers raw-output)
                           err)))))))
      (mapc handle-msg
            (nreverse messages)))))

;; Public

(defun eca-process-start (on-start handle-msg)
  "Start the eca process calling ON-START after.
Call HANDLE-MSG for new msgs processed."
  (unless (process-live-p (eca--session-process eca--session))
    (let ((start-process-fn (lambda ()
                              (eca-info "Starting process...")
                              (setf (eca--session-process eca--session)
                                    (make-process
                                     :coding 'no-conversion
                                     :connection-type 'pipe
                                     :name "eca"
                                     :command (eca-process--server-command)
                                     :buffer eca-process--buffer-name
                                     :stderr (get-buffer-create eca-process--stderr-buffer-name)
                                     :filter (-partial #'eca-process--filter handle-msg)
                                     :sentinel (lambda (process exit-str)
                                                 (unless (process-live-p process)
                                                   (setq eca--session nil)
                                                   (eca-info "process has exited (%s)" (s-trim exit-str))))
                                     :file-handler t
                                     :noquery t))
                              (funcall on-start))))
      (if (f-exists? eca-server-install-path)
          (funcall start-process-fn)
        (eca-process--download-server (lambda ()
                                (funcall start-process-fn)))))))

(defun eca-process-running-p ()
  "Return non nil if eca process is running."
  (and eca--session
       (process-live-p (eca--session-process eca--session))))

(defun eca-process-stop ()
  "Stop the eca process if running."
  (kill-process (eca--session-process eca--session))
  (kill-buffer eca-process--buffer-name))

(defun eca-process-show-stderr ()
  "Open the eca process stderr buffer if running."
  (interactive)
  (with-current-buffer eca-process--stderr-buffer-name
    (if (window-live-p (get-buffer-window (buffer-name)))
        (select-window (get-buffer-window (buffer-name)))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun eca-install-server ()
  "Download the eca server if not downloaded."
  (interactive)
  (eca-process--download-server (lambda ())))

;;;###autoload
(defun eca-uninstall-server ()
  "Remove downloaded eca server if present."
  (interactive)
  (eca-process--uninstall-server)
  (eca-info "Server uninstalled!"))

(provide 'eca-process)
;;; eca-process.el ends here
