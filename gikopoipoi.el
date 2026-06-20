;;; gikopoipoi.el --- Gikopoipoi.net client -*- lexical-binding: t; coding: utf-8 -*-

;; Based on gikomacs by gyudon_addict◆hawaiiZtQ6
;; https://github.com/andrewmetzner/gikomacs
;;
;; Package-Requires: ((websocket "1.15"))
;; Keywords: games, chat, client
;; Version: 1.0

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'let-alist))

(require 'seq)
(require 'url)
(require 'url-http)
(require 'svg)
(require 'json)
(require 'cl-lib)
(require 'color)
(require 'thingatpt)
(require 'websocket)


;;; Custom Variables

(defgroup gikopoi nil
  "Gikopoipoi client for Emacs."
  :group 'applications)

(defcustom gikopoi-default-server "gikopoipoi.net"
  "The server to connect to without prompting, or nil."
  :group 'gikopoi
  :type '(choice (const nil) string))

(defcustom gikopoi-default-port 443
  "The port on the server to connect to."
  :group 'gikopoi
  :type 'natnum)

(defcustom gikopoi-prompt-port-p nil
  "Whether to prompt for the port of the server to connect to."
  :group 'gikopoi
  :type 'boolean)

(defcustom gikopoi-default-name nil
  "The name to use without prompting, or nil."
  :group 'gikopoi
  :type '(choice (const nil) string))

(defcustom gikopoi-default-character "giko"
  "The character to use without prompting, or nil."
  :group 'gikopoi
  :type '(choice (const nil) string))

(defcustom gikopoi-default-area "gikopoipoi"
  "The area to join without prompting, or nil."
  :group 'gikopoi
  :type '(choice (const nil) string))

(defcustom gikopoi-default-room nil
  "The room to join without prompting, or nil."
  :group 'gikopoi
  :type '(choice (const nil) string))

(defcustom gikopoi-default-password nil
  "The password to enter without prompting, or nil."
  :group 'gikopoi
  :type '(choice (const nil) string))

(defcustom gikopoi-prompt-password-p nil
  "Whether or not to prompt for a password when joining a Gikopoi server."
  :group 'gikopoi
  :type 'boolean)

(defcustom gikopoi-servers
  '(("gikopoipoi.net" "gikopoipoi" "gikopoi" "bar_giko")
    ("play.gikopoi.com" "for" "gen" "vip")
    ("gikopoi.hu" "int" "hun"))
  "List of connectable Gikopoipoi servers and their areas."
  :group 'gikopoi
  :type '(repeat (cons string (repeat string))))

(defcustom gikopoi-preferred-language 'en
  "The preferred language(s) of the client."
  :group 'gikopoi
  :type '(choice symbol (repeat symbol)))

(defcustom gikopoi-autoquote-format "> %s < "
  "The `format' string for `gikopoi-autoquote', taking a single string as its argument."
  :group 'gikopoi
  :type 'string)

(defcustom gikopoi-mention-regexp regexp-unmatchable
  "The regexp that matches against incoming messages, marking them with `gikopoi-mention-color'."
  :group 'gikopoi
  :type 'regexp)

(defcustom gikopoi-mention-color "red"
  "The face color to paint messages that match `gikopoi-mention-regexp', or nil to not."
  :group 'gikopoi
  :type '(choice (const nil) string))

(defcustom gikopoi-notif-position '(mode-line-modes . nil)
  "A cons where `gikopoi-notif' is prepended or appended to its car if the cdr is nil or t."
  :group 'gikopoi
  :type '(cons variable boolean))

(defcustom gikopoi-timestamp-interval 3600
  "The interval in seconds to print timestamps in the message buffer, or nil to not print them."
  :group 'gikopoi
  :type '(choice (const nil) number))

(defcustom gikopoi-time-format "* %a %b %d %Y %T GMT%z (%Z)\n"
  "A `format-time-string' string to format timestamps with."
  :group 'gikopoi
  :type 'string)

(defcustom gikopoi-msg-time-format "[%H:%M:%S] "
  "Timestamp prepended to each chat message."
  :group 'gikopoi
  :type 'string)

(defcustom gikopoi-logger nil
  "If non-nil, log chat to file."
  :group 'gikopoi
  :type 'boolean)

(defcustom gikopoi-log-directory (expand-file-name "~/.gikopoi-logs/")
  "Directory where chat logs are stored."
  :group 'gikopoi
  :type 'string)

(defcustom gikopoi-auto-start-reconnect-timer nil
  "If non-nil, start the periodic reconnect timer automatically on connect."
  :group 'gikopoi
  :type 'boolean)

(defcustom gikopoi-reconnect-timer-minutes 720
  "Minutes between periodic automatic reconnects."
  :group 'gikopoi
  :type 'natnum)

(defcustom gikopoi-auto-reconnect t
  "If non-nil, automatically reconnect when the connection drops."
  :group 'gikopoi
  :type 'boolean)

(defcustom gikopoi-reconnect-max-delay 300
  "Maximum seconds to wait between auto-reconnect attempts."
  :group 'gikopoi
  :type 'natnum)


;;; API

(defun gikopoi-version-of-server (server)
  (with-temp-buffer
    (url-insert-file-contents (format "https://%s/api/version" server))
    (number-at-point)))

(defun gikopoi-log-to-server (server message)
  (declare (indent 1))
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (url-request-data (encode-coding-string message 'utf-8)))
    (url-retrieve-synchronously (format "https://%s/api/client-log" server))))

(defun gikopoi-login (server area room name character password)
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string
                           (json-encode-alist
                            (cl-pairlis '(userName characterId areaId roomId password)
                                        (list name character area room password))) 'utf-8)))
    (with-temp-buffer
      (url-insert-file-contents (format "https://%s/api/login" server))
      (json-read-object))))

(defvar gikopoi--auto-move-on-join-p nil
  "When non-nil, auto-move to the busiest room after the first room list arrives.")

(defvar gikopoi-room-groups nil
  "Alist mapping room-id to area group-id, populated by `server-room-list'.")

(defun gikopoi--busiest-room-in-group (group)
  "Return the room-id with the most users in GROUP from the last room list."
  (let (best-id (best-count -1))
    (dolist (entry gikopoi-room-list-data)
      (let* ((id    (car entry))
             (count (string-to-number (aref (cadr entry) 2)))
             (grp   (cdr (assoc id gikopoi-room-groups))))
        (when (and (equal grp group) (> count best-count))
          (setq best-id id best-count count))))
    best-id))


;;; WebSocket

(defvar gikopoi-socket nil)

(defun gikopoi--ws-url (server port)
  "Build a WebSocket URL for SERVER and PORT.
Uses wss:// on port 443, ws:// otherwise."
  (if (= port 443)
      (format "wss://%s/socket.io/?EIO=4&transport=websocket" server)
    (format "ws://%s:%d/socket.io/?EIO=4&transport=websocket" server port)))

(defun gikopoi-socket-open (server port pid)
  (setq gikopoi-socket
        (websocket-open (gikopoi--ws-url server port)
                        :custom-header-alist `((private-user-id . ,pid)
                                               (perMessageDeflate . false))
                        :on-open  (lambda (sock) (websocket-send-text sock "40"))
                        :on-close (lambda (sock)
                                    (when (timerp gikopoi-socket-ping-timer)
                                      (cancel-timer gikopoi-socket-ping-timer))
                                    (when (and gikopoi-auto-reconnect
                                               (not gikopoi--deliberately-quit))
                                      (gikopoi--schedule-reconnect 5)))
                        :on-message #'gikopoi-socket-message-handler))
  (setf (websocket-client-data gikopoi-socket) (list server port pid))
  gikopoi-socket)

(defvar gikopoi-socket-ping-timer nil)

(defun gikopoi-socket-close ()
  (when (websocket-openp gikopoi-socket)
    (websocket-close gikopoi-socket))
  (when (timerp gikopoi-socket-ping-timer)
    (cancel-timer gikopoi-socket-ping-timer)))

(defvar gikopoi-socket-timeout nil)
(defvar gikopoi-reconnecting-p nil)
(defvar gikopoi--deliberately-quit nil)
(defvar gikopoi--reconnect-timer nil)
(defvar gikopoi--reconnect-current-delay 5)

(defun gikopoi--schedule-reconnect (delay)
  "Schedule a full re-login + reconnect in DELAY seconds, doubling on each failure."
  (when (timerp gikopoi--reconnect-timer)
    (cancel-timer gikopoi--reconnect-timer))
  (when (buffer-live-p gikopoi-message-buffer)
    (gikopoi-with-message-buffer
      (insert (format "%s* disconnected — retrying in %ds\n"
                      (format-time-string gikopoi-msg-time-format) delay))))
  (setq gikopoi--reconnect-current-delay delay
        gikopoi--reconnect-timer
        (run-at-time delay nil
                     (lambda ()
                       (condition-case err
                           (gikopoi-reconnect)
                         (error
                          (let ((next (min (* gikopoi--reconnect-current-delay 2)
                                           gikopoi-reconnect-max-delay)))
                            (gikopoi--schedule-reconnect next))))))))

(defvar gikopoi-socket-interval nil)
(defvar gikopoi-socket-tolerance 1)

(defun gikopoi-socket-message-handler (sock frame)
  (condition-case err
      (let (id payload)
        (with-temp-buffer
          (save-excursion
            (insert (websocket-frame-text frame)))
          (setq id (thing-at-point 'number))
          (forward-word)
          (setq payload (ignore-errors (json-read))))
        (cond
         ((eql id 0)
          (let-alist payload
            (setq gikopoi-socket-interval (/ .pingInterval 1000)
                  gikopoi-socket-timeout (/ .pingTimeout 1000))))
         ((eql id 2)
          (when (timerp gikopoi-socket-ping-timer)
            (cancel-timer gikopoi-socket-ping-timer))
          (websocket-send-text sock "3")
          (setq gikopoi-socket-ping-timer
                (run-at-time (+ gikopoi-socket-interval gikopoi-socket-tolerance) nil
                             (lambda ()
                               (unless gikopoi--deliberately-quit
                                 (gikopoi--schedule-reconnect 5))))))
         ((eql id 40) t)
         ((eql id 42) (gikopoi-event-handler payload))
         (t (message "Gikopoi: unrecognized packet %s %s" id payload))))
    (error
     (let ((msg (error-message-string err)))
       (unless (string-match-p "No usable sound device driver found" msg)
         (message "Gikopoi: websocket error: %s" msg))))))

(defun gikopoi-socket-emit (object)
  (websocket-send-text gikopoi-socket
                       (concat "42" (encode-coding-string (json-encode object) 'utf-8))))


;;; Server Events

(defmacro gikopoi-defevent (name args &rest body)
  "Define a handler for server event NAME."
  (declare (indent defun))
  (let (list-args)
    `(put ',name 'gikopoi-event-fn
          (lambda ,(mapcar (lambda (arg)
                             (if (consp arg)
                                 (caar (push (cons (gensym) arg) list-args))
                               arg)) args)
            (let ,(mapcan (lambda (larg)
                            (mapcar (lambda (arg)
                                      `(,arg (cdr (assq ',arg ,(car larg)))))
                                    (cdr larg))) list-args)
              ,@body)))))

(defmacro gikopoi-event-fn (name)
  `(get ,name 'gikopoi-event-fn))

(defun gikopoi-event-handler (event)
  (if-let ((fn (gikopoi-event-fn (intern-soft (aref event 0)))))
      (apply fn (cl-coerce (substring event 1) 'list))
    (message "Gikopoi: unhandled event %s" (aref event 0))))


;;; Client Emits

(defun gikopoi-change-room (room &optional door)
  (gikopoi-socket-emit `(user-change-room ((targetRoomId . ,room) (targetDoorId . ,door)))))

(defun gikopoi-ping ()
  (gikopoi-socket-emit '(user-ping)))

(defun gikopoi-send (message &optional endln)
  "Send MESSAGE to the server. If ENDLN, also send an empty string to clear bubble."
  (gikopoi-socket-emit `(user-msg ,message))
  (when endln
    (gikopoi-socket-emit '(user-msg ""))))

(defun gikopoi-move (direction)
  (gikopoi-socket-emit `(user-move ,direction)))

(defun gikopoi-bubble-position (direction)
  (gikopoi-socket-emit `(user-bubble-position ,direction)))

(defun gikopoi-room-list ()
  (gikopoi-socket-emit '(user-room-list)))


;;; Connecting

(defvar gikopoi-current-server nil)
(defvar gikopoi-current-user-id nil)
(defvar gikopoi-current-private-user-id nil)

(defun gikopoi-connect (server port area room name character password)
  (setq gikopoi--deliberately-quit nil)
  (when (timerp gikopoi--reconnect-timer)
    (cancel-timer gikopoi--reconnect-timer))
  (when (and (boundp 'gikopoi-socket) (websocket-openp gikopoi-socket))
    (gikopoi-socket-close))
  (let ((version (gikopoi-version-of-server server))
        (login (gikopoi-login server area room name character password)))
    (when (or (null login) (eq (alist-get 'isLoginSuccessful login) json-false))
      (error "Gikopoi: login unsuccessful: %s" (or (alist-get 'error login) "unknown")))
    (let-alist login
      (gikopoi-log-to-server server
        (string-join (list (format-time-string "%a %b %d %Y %T GMT%z (%Z)") .userId
                           "window.EXPECTED_SERVER_VERSION:" (number-to-string version)
                           "loginMessage.appVersion:" (number-to-string .appVersion)
                           "DIFFERENT:" (if (eql version .appVersion) "false" "true")) " "))
      (gikopoi-log-to-server server
        (string-join (list (format-time-string "%a %b %d %Y %T GMT%z (%Z)") .userId
                           (url-http-user-agent-string)) " "))
      (setq gikopoi-current-server server
            gikopoi-current-user-id .userId
            gikopoi-current-private-user-id .privateUserId)
      (gikopoi-socket-open server port .privateUserId))))


;;; Default Directory

(defconst gikopoi-default-directory
  (or (and load-file-name (file-name-directory load-file-name))
      default-directory))

(defvar gikopoi-site-directory nil)

(defun gikopoi-init-site-directory (server &rest _args)
  (let* ((sitedir (file-name-as-directory
                   (expand-file-name "sites" gikopoi-default-directory)))
         (nosearch (expand-file-name ".nosearch" sitedir))
         (sitedir (file-name-as-directory
                   (expand-file-name server sitedir))))
    (unless (file-exists-p nosearch) (make-empty-file nosearch t))
    (make-directory (expand-file-name "rooms" sitedir) t)
    (setq gikopoi-site-directory sitedir)))


;;; Sound Effects

(defconst gikopoi-login-sound
  (expand-file-name "login.au" gikopoi-default-directory))
(defconst gikopoi-message-sound
  (expand-file-name "message.au" gikopoi-default-directory))
(defconst gikopoi-mention-sound
  (expand-file-name "mention.au" gikopoi-default-directory))
(defconst gikopoi-disconnect-sound
  (expand-file-name "connection-lost.au" gikopoi-default-directory))
(defconst gikopoi-coin-sound
  (expand-file-name "ka-ching.au" gikopoi-default-directory))

(defun gikopoi-play-sound (file)
  (ignore-errors (play-sound-file file)))


;;; Language

(defvar gikopoi-lang-directory nil)
(defvar gikopoi-lang-alist nil)

(defun gikopoi-init-lang-alist (&rest _args)
  (setq gikopoi-lang-directory
        (file-name-as-directory (expand-file-name "langs" gikopoi-default-directory)))
  (let ((lang-file (expand-file-name
                    (symbol-name (or (car-safe gikopoi-preferred-language)
                                     gikopoi-preferred-language 'en))
                    gikopoi-lang-directory)))
    (when (file-exists-p lang-file)
      (with-temp-buffer
        (insert-file-contents lang-file)
        (setq gikopoi-lang-alist (read (current-buffer)))))))


;;; Logging

(defun gikopoi--log-to-file (text)
  (when gikopoi-logger
    (let* ((log-dir gikopoi-log-directory)
           (log-file (expand-file-name
                      (concat (format-time-string "%Y-%m-%d") ".txt") log-dir)))
      (unless (file-exists-p log-dir)
        (make-directory log-dir t))
      (with-temp-buffer
        (insert (or text ""))
        (append-to-file (point-min) (point-max) log-file)))))


;;; User List

(defun gikopoi-user-list-ignore-toggle ()
  "Toggle ignore on the user at point in the user list buffer."
  (interactive)
  (when-let ((user (gikopoi-user-by-id (tabulated-list-get-id))))
    (setf (gikopoi-user-ignored-p user) (not (gikopoi-user-ignored-p user))))
  (tabulated-list-revert))

(defvar gikopoi-user-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'gikopoi-user-list-ignore-toggle)
    map))

(define-minor-mode gikopoi-user-list-mode
  "Mode for the User List buffer."
  :group 'gikopoi
  :keymap gikopoi-user-list-mode-map)

(defvar gikopoi-user-list-buffer nil)

(defun gikopoi-init-user-list-buffer ()
  (setq gikopoi-user-list-buffer (get-buffer-create "*Gikopoi Users*"))
  (with-current-buffer gikopoi-user-list-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format
          `[("Name" 25 t)
            ("Status" 6 nil)
            ("ID" 10 t)])
    (tabulated-list-init-header)
    (add-hook 'tabulated-list-revert-hook
              (lambda ()
                (setq tabulated-list-entries (gikopoi--user-list-entries)))
              nil t)
    (gikopoi-user-list-mode)))

(defun gikopoi--user-list-entries ()
  (mapcar (lambda (user)
            (let ((id (gikopoi-user-id user)))
              (list id (vector (gikopoi-user-name user)
                               (string-join
                                (list (if (gikopoi-user-active-p user) "" "Zz")
                                      (if (gikopoi-user-ignored-p user) "I" ""))
                                " ")
                               (format "%s" id)))))
          (gikopoi-room-users gikopoi-current-room)))

(defun gikopoi-list-users ()
  "Show the user list buffer."
  (interactive)
  (unless (buffer-live-p gikopoi-user-list-buffer)
    (gikopoi-init-user-list-buffer))
  (with-current-buffer gikopoi-user-list-buffer
    (tabulated-list-revert))
  (unless (get-buffer-window gikopoi-user-list-buffer)
    (display-buffer gikopoi-user-list-buffer)))


;;; Room List

(defun gikopoi-room-list-change-entry ()
  (interactive)
  (gikopoi-change-room (tabulated-list-get-id)))

(defvar gikopoi-room-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gikopoi-room-list-change-entry)
    map))

(define-minor-mode gikopoi-room-list-mode
  "Mode for the Room List buffer."
  :group 'gikopoi
  :keymap gikopoi-room-list-mode-map)

(defvar gikopoi-room-list-buffer nil)

(defun gikopoi-init-room-list-buffer ()
  (setq gikopoi-room-list-buffer (get-buffer-create "*Gikopoi Rooms*"))
  (with-current-buffer gikopoi-room-list-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format
          `[("Room" 32 t)
            ("Area" 14 t)
            ("Users" 6 t)
            ("Streams" 0 nil)])
    (tabulated-list-init-header)
    (gikopoi-room-list-mode)))

(defvar gikopoi-room-list-data nil)
(defvar gikopoi--show-room-list-p nil)

(defun gikopoi-update-room-list (rooms)
  (seq-doseq (room rooms)
    (let-alist room
      (let* ((id .id)
             (count (number-to-string .userCount))
             (streams (string-join .streamers " "))
             (entry (assoc id gikopoi-room-list-data)))
        ;; track group for busiest-room lookup
        (setf (alist-get id gikopoi-room-groups nil nil #'equal) .group)
        (let-alist gikopoi-lang-alist
          (let* ((name (cdr (assoc id .room #'string-equal)))
                 (name (or (when (consp name) (cdr (assq 'sort_key name))) name id))
                 (area (cdr (assoc .group .area #'string-equal)))
                 (area (or (when (consp area) (cdr (assq 'sort_key area))) area .group)))
            (if (null entry)
                (push (list id (vector name area count streams)) gikopoi-room-list-data)
              (setf (aref (cadr entry) 2) count
                    (aref (cadr entry) 3) streams))))))))

(gikopoi-defevent server-room-list (rooms)
  (gikopoi-update-room-list rooms)
  ;; auto-move to busiest room on initial connect
  (when gikopoi--auto-move-on-join-p
    (setq gikopoi--auto-move-on-join-p nil)
    (when-let* ((grp  (ignore-errors (slot-value gikopoi-current-room 'group)))
                (best (gikopoi--busiest-room-in-group grp))
                (_ (not (equal best (gikopoi-room-id gikopoi-current-room)))))
      (gikopoi-change-room best)))
  ;; show room list only when user asked
  (when gikopoi--show-room-list-p
    (setq gikopoi--show-room-list-p nil)
    (when (buffer-live-p gikopoi-room-list-buffer)
      (with-current-buffer gikopoi-room-list-buffer
        (setq tabulated-list-entries gikopoi-room-list-data)
        (tabulated-list-revert))
      (unless (get-buffer-window gikopoi-room-list-buffer)
        (display-buffer gikopoi-room-list-buffer)))))

(defun gikopoi-list-rooms ()
  (interactive)
  (unless (buffer-live-p gikopoi-room-list-buffer)
    (gikopoi-init-room-list-buffer))
  (setq gikopoi--show-room-list-p t)
  (gikopoi-room-list))


;;; Message Buffer

(defvar gikopoi-message-buffer nil)
(defvar gikopoi--should-scroll-on-visible nil)
(defvar gikopoi--user-at-bottom-p t)

(defun gikopoi--at-bottom-p ()
  (let ((win (get-buffer-window gikopoi-message-buffer)))
    (and win (<= (- (point-max) (window-end win nil)) 1))))

(defun gikopoi--near-bottom-p ()
  (let ((win (get-buffer-window gikopoi-message-buffer)))
    (and win (<= (- (count-lines (window-end win t) (point-max))) (window-body-height win)))))

(defun gikopoi--prevent-scroll-jump ()
  (setq-local scroll-conservatively 101
              scroll-margin 0
              scroll-step 1))

(defun gikopoi-message-buffer-force-scroll ()
  (when-let ((win (get-buffer-window gikopoi-message-buffer)))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -1))))

(defun gikopoi-init-message-buffer (server &rest _args)
  (setq gikopoi-message-buffer (get-buffer-create "*Gikopoi*"))
  (with-current-buffer gikopoi-message-buffer
    (gikopoi-mode)
    (gikopoi-msg-mode)
    (goto-address-mode)
    (visual-line-mode 1)
    (setq buffer-read-only t)
    (gikopoi--prevent-scroll-jump))
  (display-buffer gikopoi-message-buffer)
  (when gikopoi--should-scroll-on-visible
    (gikopoi-message-buffer-force-scroll)
    (setq gikopoi--should-scroll-on-visible nil)))

(defmacro gikopoi-with-message-buffer (&rest body)
  (declare (indent defun))
  `(with-current-buffer gikopoi-message-buffer
     (let* ((win (get-buffer-window))
            (at-bottom (and win (gikopoi--at-bottom-p))))
       (save-excursion
         (goto-char (point-max))
         (let ((buffer-read-only nil))
           ,@body))
       (cond
        (win (when at-bottom (set-window-point win (point-max))))
        (t (setq gikopoi--should-scroll-on-visible t))))))

(defun gikopoi--update-user-scroll-status ()
  (when (and (get-buffer-window gikopoi-message-buffer)
             (eq (current-buffer) gikopoi-message-buffer))
    (setq gikopoi--user-at-bottom-p (gikopoi--near-bottom-p))))

(add-hook 'post-command-hook #'gikopoi--update-user-scroll-status)

(defun gikopoi-scroll-down-safely ()
  "Scroll down; snap to bottom if near it."
  (interactive)
  (when-let ((win (get-buffer-window gikopoi-message-buffer)))
    (with-selected-window win
      (scroll-up-command)
      (when (<= (- (point-max) (window-end win t)) (window-body-height win))
        (goto-char (point-max))
        (recenter -1)))))

(defun gikopoi-scroll-up-safely ()
  "Scroll up in the message buffer."
  (interactive)
  (when-let ((win (get-buffer-window gikopoi-message-buffer)))
    (with-selected-window win
      (scroll-down-command))))


;;; Interactive Commands

(defun gikopoi-move-left  (n) (interactive "p") (dotimes (_ n) (gikopoi-move "left")))
(defun gikopoi-move-right (n) (interactive "p") (dotimes (_ n) (gikopoi-move "right")))
(defun gikopoi-move-up    (n) (interactive "p") (dotimes (_ n) (gikopoi-move "up")))
(defun gikopoi-move-down  (n) (interactive "p") (dotimes (_ n) (gikopoi-move "down")))

(defun gikopoi-bubble-left  () (interactive) (gikopoi-bubble-position "left"))
(defun gikopoi-bubble-right () (interactive) (gikopoi-bubble-position "right"))
(defun gikopoi-bubble-up    () (interactive) (gikopoi-bubble-position "up"))
(defun gikopoi-bubble-down  () (interactive) (gikopoi-bubble-position "down"))

(defun gikopoi-send-blank () (interactive) (gikopoi-send ""))

(defun gikopoi-autoquote ()
  "Open the message prompt pre-filled with a quoted version of text at point."
  (interactive)
  (let ((quote (buffer-substring (point) (line-end-position))))
    (if (null (active-minibuffer-window))
        (minibuffer-with-setup-hook
            (lambda () (insert (format gikopoi-autoquote-format quote)))
          (call-interactively #'gikopoi-send-message))
      (select-window (active-minibuffer-window))
      (erase-buffer)
      (insert (format gikopoi-autoquote-format quote)))))

(defun gikopoi-minibuffer-complete ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (completion-in-region (car bounds) (cdr bounds) (gikopoi-user-names))))

(defvar gikopoi-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "TAB") #'gikopoi-minibuffer-complete)
    map))

(defun gikopoi-rula (room)
  (interactive (list (completing-read "Rula: " (mapcar #'car gikopoi-room-list-data))))
  (gikopoi-change-room room))

(defun gikopoi-send-message ()
  "Prompt for a message and send it.
#rula [room] changes room; #list opens user list; empty input clears bubble."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (case-fold-search nil)
        (message (read-from-minibuffer "" nil gikopoi-minibuffer-map)))
    (cond
     ((string-empty-p message) (gikopoi-send-blank))
     ((string-match "^#rula *" message)
      (let ((room (string-trim (substring message (match-end 0)))))
        (if (string-empty-p room)
            (gikopoi-list-rooms)
          (gikopoi-rula room))))
     ((string-match "^#list" message)
      (gikopoi-list-users))
     (t
      (gikopoi-send message)
      (when (get-buffer-window "*Gikopoi*")
        (select-window (get-buffer-window "*Gikopoi*")))))))

(defun gikopoi-open-minibuffer ()
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (call-interactively #'gikopoi-send-message)))

(defun gikopoi-ignore (name)
  (interactive (list (completing-read "Ignore: " (gikopoi-user-names))))
  (when-let ((user (gikopoi-user-by-name name)))
    (setf (gikopoi-user-ignored-p user) (not (gikopoi-user-ignored-p user)))
    (when (called-interactively-p 'interactive)
      (message "%s %s" (gikopoi-user-name user)
               (if (gikopoi-user-ignored-p user) "ignored" "un-ignored")))))


;;; Auto-ignore

(defvar gikopoi-auto-ignore-file
  (expand-file-name "auto-ignore.txt" gikopoi-default-directory)
  "File of newline-separated usernames to always ignore.")

(defvar gikopoi-auto-ignore-names nil)

(defun gikopoi--load-auto-ignore-list ()
  (when (file-exists-p gikopoi-auto-ignore-file)
    (with-temp-buffer
      (insert-file-contents gikopoi-auto-ignore-file)
      (split-string (string-trim (buffer-string)) "\n" t))))

(defun gikopoi--save-auto-ignore-list (names)
  (with-temp-file gikopoi-auto-ignore-file
    (dolist (name (delete-dups names))
      (insert name "\n"))))

(defun gikopoi-auto-ignore (name)
  "Toggle NAME in the persistent auto-ignore list."
  (interactive "sAuto-ignore username: ")
  (unless (file-exists-p gikopoi-auto-ignore-file)
    (with-temp-file gikopoi-auto-ignore-file))
  (let* ((names (gikopoi--load-auto-ignore-list))
         (already (member name names))
         (user (gikopoi-user-by-name name)))
    (if already
        (progn
          (setq names (delete name names))
          (when user (setf (gikopoi-user-ignored-p user) nil))
          (message "%s removed from auto-ignore" name))
      (push name names)
      (when user (setf (gikopoi-user-ignored-p user) t))
      (message "%s added to auto-ignore" name))
    (gikopoi--save-auto-ignore-list names)))

(defun gikopoi-load-auto-ignored-users ()
  "Apply the auto-ignore list to current room users."
  (interactive)
  (setq gikopoi-auto-ignore-names (gikopoi--load-auto-ignore-list))
  (when gikopoi-current-room
    (dolist (user (gikopoi-room-users gikopoi-current-room))
      (when (member (gikopoi-user-name user) gikopoi-auto-ignore-names)
        (setf (gikopoi-user-ignored-p user) t))))
  (message "Loaded %d auto-ignored users" (length gikopoi-auto-ignore-names)))

(defun gikopoi-init-auto-ignore (&rest _args)
  (if (and gikopoi-current-room (gikopoi-room-users gikopoi-current-room))
      (gikopoi-load-auto-ignored-users)
    (run-at-time 0.5 nil #'gikopoi-init-auto-ignore)))


;;; Quit / Reconnect

(defvar gikopoi-quit-functions
  (list #'gikopoi-socket-close
        (lambda () (gikopoi-notif-mode -1))))

(defun gikopoi-quit ()
  (interactive)
  (when (y-or-n-p "Disconnect from Gikopoipoi? ")
    (setq gikopoi--deliberately-quit t)
    (when (timerp gikopoi--reconnect-timer)
      (cancel-timer gikopoi--reconnect-timer))
    (run-hooks 'gikopoi-quit-functions)))

(defun gikopoi-quit-silent ()
  (setq gikopoi--deliberately-quit t)
  (when (timerp gikopoi--reconnect-timer)
    (cancel-timer gikopoi--reconnect-timer))
  (run-hooks 'gikopoi-quit-functions))

(defvar gikopoi--last-args nil)

(defun gikopoi-reconnect ()
  "Reconnect using the last connection arguments."
  (when gikopoi--last-args
    (message "Gikopoi: reconnecting...")
    (ignore-errors (gikopoi-quit-silent))
    (apply #'gikopoi gikopoi--last-args)))

(defvar gikopoi-reconnect-timer nil)

(defun gikopoi-start-reconnect-timer ()
  (interactive)
  (let ((seconds (* gikopoi-reconnect-timer-minutes 60)))
    (when gikopoi-reconnect-timer
      (cancel-timer gikopoi-reconnect-timer))
    (setq gikopoi-reconnect-timer
          (run-at-time seconds seconds #'gikopoi-reconnect))
    (message "Gikopoi: reconnect timer set for every %d minutes"
             gikopoi-reconnect-timer-minutes)))

(defun gikopoi-stop-reconnect-timer ()
  (interactive)
  (when gikopoi-reconnect-timer
    (cancel-timer gikopoi-reconnect-timer)
    (setq gikopoi-reconnect-timer nil)
    (message "Gikopoi: reconnect timer stopped")))

(defun gikopoi-maybe-start-reconnect-timer (&rest _args)
  (when gikopoi-auto-start-reconnect-timer
    (gikopoi-start-reconnect-timer)))


;;; Timestamps

(defvar gikopoi-timestamp-timer nil)

(defun gikopoi-print-single-timestamp (&rest _args)
  (gikopoi-with-message-buffer
    (insert (format-time-string gikopoi-time-format))))

(defun gikopoi-print-timestamps (&rest _args)
  (unless (null gikopoi-timestamp-interval)
    (setq gikopoi-timestamp-timer
          (run-at-time t gikopoi-timestamp-interval #'gikopoi-print-single-timestamp))
    (add-hook 'gikopoi-quit-functions
              (lambda () (cancel-timer gikopoi-timestamp-timer)))))


;;; Modes

(define-derived-mode gikopoi-mode fundamental-mode "Gikopoi"
  "Major mode for Gikopoi."
  :group 'gikopoi)

(let ((map gikopoi-mode-map))
  (define-key map (kbd "SPC")       #'gikopoi-open-minibuffer)
  (define-key map (kbd "RET")       #'gikopoi-send-blank)
  (define-key gikopoi-minibuffer-map (kbd "RET") #'exit-minibuffer)
  (define-key map (kbd "r")         #'gikopoi-rula)
  (define-key map (kbd "i")         #'gikopoi-ignore)
  (define-key map (kbd "c")         #'gikopoi-clear-mentions)
  (define-key map (kbd "R")         #'gikopoi-list-rooms)
  (define-key map (kbd "L")         #'gikopoi-list-users)
  (define-key map (kbd "Q")         #'gikopoi-quit)
  (define-key map (kbd "<left>")    #'gikopoi-move-left)
  (define-key map (kbd "<right>")   #'gikopoi-move-right)
  (define-key map (kbd "<up>")      #'gikopoi-move-up)
  (define-key map (kbd "<down>")    #'gikopoi-move-down)
  (define-key map (kbd "<C-left>")  #'gikopoi-bubble-left)
  (define-key map (kbd "<C-right>") #'gikopoi-bubble-right)
  (define-key map (kbd "<C-up>")    #'gikopoi-bubble-up)
  (define-key map (kbd "<C-down>")  #'gikopoi-bubble-down)
  (define-key map (kbd "<next>")    #'gikopoi-scroll-down-safely)
  (define-key map (kbd "C-v")       #'gikopoi-scroll-down-safely)
  (define-key map (kbd "<prior>")   #'gikopoi-scroll-up-safely)
  (define-key map (kbd "M-v")       #'gikopoi-scroll-up-safely))

(defvar gikopoi-unread-count 0)
(defvar gikopoi-notif-names nil)

(defun gikopoi-clear-mentions ()
  (interactive)
  (setq gikopoi-unread-count 0 gikopoi-notif-names nil)
  (when (called-interactively-p 'interactive)
    (force-mode-line-update)))

(defun gikopoi-notif-string ()
  (if (cl-plusp gikopoi-unread-count)
      (cl-labels ((shortest-unique (i string)
                    (let* ((prefix (substring string 0 i))
                           (completion (try-completion prefix gikopoi-notif-names)))
                      (cond ((member completion gikopoi-notif-names) prefix)
                            ((equal prefix completion) (shortest-unique (1+ i) string))
                            ((< (length prefix) (length completion))
                             (shortest-unique (length completion) string))))))
        (format " (%d)%s" gikopoi-unread-count
                (if gikopoi-notif-names
                    (format ",%s" (mapcar (lambda (x) (shortest-unique 1 x)) gikopoi-notif-names))
                  "")))
    ""))

(define-minor-mode gikopoi-notif-mode
  "Display unread Gikopoi message count in the mode line."
  :group 'gikopoi
  :global t)

(defvar gikopoi-msg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c q") #'gikopoi-autoquote)
    map))

(define-minor-mode gikopoi-msg-mode
  "Minor mode for the Gikopoi message buffer."
  :group 'gikopoi
  :keymap gikopoi-msg-mode-map)

(add-to-list 'minor-mode-alist
             '(gikopoi-msg-mode
               (:eval (format ": %s@%s"
                              (gikopoi-room-id gikopoi-current-room)
                              gikopoi-current-server))))

(add-to-list 'minor-mode-alist
             '(gikopoi-notif-mode (:eval (gikopoi-notif-string))))


;;; Users

(defclass gikopoi-user ()
  ((id             :initarg :id             :accessor gikopoi-user-id)
   (name           :initarg :name           :accessor gikopoi-user-name)
   (character-id   :initarg :character-id   :accessor gikopoi-user-character-id)
   (altp           :initarg :altp           :accessor gikopoi-user-alt-p)
   (position       :initarg :position       :accessor gikopoi-user-position)
   (direction      :initarg :direction      :accessor gikopoi-user-direction)
   (message        :initarg :message        :accessor gikopoi-user-last-message)
   (bubble-position :initarg :bubble-position :accessor gikopoi-user-bubble-position)
   (activep        :initarg :activep        :accessor gikopoi-user-active-p)
   (last-movement  :initarg :last-movement  :accessor gikopoi-user-last-movement)
   (voice-pitch    :initarg :voice-pitch    :accessor gikopoi-user-voice-pitch)
   (ignoredp       :initform nil            :accessor gikopoi-user-ignored-p)
   (name-color     :accessor gikopoi-user-name-color)
   (object         :accessor gikopoi-user-object)))

(cl-defmethod shared-initialize :after ((this gikopoi-user) initargs)
  (let* ((h (/ (mod (sxhash (slot-value this 'id)) 360.0) 360.0))
         (rgb (color-hsl-to-rgb h 0.6 0.7))
         (color (apply #'color-rgb-to-hex rgb)))
    (setf (slot-value this 'name-color) color
          (gikopoi-user-name this) (slot-value this 'name)))
  (when (equal gikopoi-current-user-id (slot-value this 'id))
    (setq gikopoi-current-user this)))

(cl-defmethod (setf gikopoi-user-name) (name (user gikopoi-user))
  (setf (slot-value user 'name)
        (propertize (if (string-empty-p name) "Spy" name)
                    'face `(:foreground ,(slot-value user 'name-color)))))

(defvar gikopoi-current-user nil)

(defun gikopoi-make-user (user-alist)
  (let-alist user-alist
    (apply #'make-instance 'gikopoi-user
           (list :id .id :name .name :character-id .characterId
                 :last-movement .lastMovement
                 :altp (eq .isAlternateCharacter t)
                 :position (cons .position.x .position.y)
                 :direction .direction :message .lastRoomMessage
                 :voice-pitch .voicePitch :bubble-position .bubblePosition
                 :activep (eq .isInactive json-false)))))

(defvar gikopoi-message-matched-p nil)

(cl-defmethod gikopoi-user-insert-message ((user gikopoi-user) msg)
  (unless (gikopoi-user-ignored-p user)
    (let ((text (concat (format-time-string gikopoi-msg-time-format) (or msg ""))))
      (gikopoi--log-to-file text)
      (if (eq user gikopoi-current-user)
          (gikopoi-clear-mentions)
        (when (setq gikopoi-message-matched-p
                    (string-match gikopoi-mention-regexp text))
          (unless (null gikopoi-mention-color)
            (put-text-property (match-beginning 0) (match-end 0)
                               'face `(:foreground ,gikopoi-mention-color) text)))
        (unless (get-buffer-window gikopoi-message-buffer 'visible)
          (cl-incf gikopoi-unread-count)
          (when gikopoi-message-matched-p
            (cl-pushnew (gikopoi-user-name user) gikopoi-notif-names :test #'equal))))
      (force-mode-line-update)
      (gikopoi-with-message-buffer (insert text)))))

(cl-defmethod gikopoi-user-msg ((user gikopoi-user) message &optional silentp)
  (setf (gikopoi-user-last-message user) message)
  (unless (string-empty-p (or message ""))
    (let* ((name (gikopoi-user-name user))
           (formatted (string-join
                       (mapcar (lambda (line) (format "%s: %s\n" name line))
                               (split-string message "\n"))
                       "")))
      (gikopoi-user-insert-message user formatted)
      (unless silentp
        (gikopoi-play-sound
         (if (and gikopoi-message-matched-p (not (eq user gikopoi-current-user)))
             gikopoi-mention-sound
           gikopoi-message-sound))))))

(cl-defmethod gikopoi-user-roleplay ((user gikopoi-user) message)
  (gikopoi-user-insert-message user (format "* %s %s\n" (gikopoi-user-name user) message)))

(cl-defmethod gikopoi-user-roll-die ((user gikopoi-user) base sum times)
  (gikopoi-user-insert-message user
    (format "[DICE] %s rolled %sx d%s → %s\n" (gikopoi-user-name user) times base sum)))

(cl-defmethod gikopoi-user-join ((user gikopoi-user) &optional from reconnectp)
  (unless (or reconnectp (gikopoi-user-ignored-p user))
    (gikopoi-user-insert-message user
      (format "* %s has entered the room%s"
              (gikopoi-user-name user)
              (if from (format " from %s\n" from) "\n")))
    (gikopoi-play-sound gikopoi-login-sound)))

(cl-defmethod gikopoi-user-leave ((user gikopoi-user) &optional for)
  (gikopoi-user-insert-message user
    (format "* %s has left the room%s"
            (gikopoi-user-name user)
            (if for (format " for %s\n" for) "\n"))))

(cl-defmethod (setf gikopoi-user-active-p) :after ((p (eql nil)) (user gikopoi-user))
  (gikopoi-user-insert-message user (format "* %s is away\n" (slot-value user 'name))))

(cl-defmethod gikopoi-user-move ((user gikopoi-user) position direction last-movement
                                 instantp spinwalkp)
  (setf (slot-value user 'position) position
        (slot-value user 'direction) direction
        (slot-value user 'last-movement) last-movement))

(defun gikopoi-user-by-id (id)
  (cl-find id (gikopoi-room-users gikopoi-current-room)
           :test #'equal :key #'gikopoi-user-id))

(defun gikopoi-user-by-name (name)
  (cl-find name (gikopoi-room-users gikopoi-current-room)
           :test #'equal :key #'gikopoi-user-name))

(defun gikopoi-user-names ()
  (mapcar #'gikopoi-user-name (gikopoi-room-users gikopoi-current-room)))

(gikopoi-defevent server-user-active (id)
  (setf (gikopoi-user-active-p (gikopoi-user-by-id id)) t))

(gikopoi-defevent server-user-inactive (id)
  (setf (gikopoi-user-active-p (gikopoi-user-by-id id)) nil))

(gikopoi-defevent server-move ((userId x y direction lastMovement isInstant shouldSpinwalk))
  (gikopoi-user-move (gikopoi-user-by-id userId) (cons x y) direction lastMovement
                     (eq isInstant t) (eq shouldSpinwalk t)))

(gikopoi-defevent server-bubble-position (id direction)
  (setf (gikopoi-user-bubble-position (gikopoi-user-by-id id)) direction))

(gikopoi-defevent server-name-changed (id name)
  (setf (gikopoi-user-name (gikopoi-user-by-id id)) name))

(gikopoi-defevent server-character-changed (id character-id altp)
  (let ((user (gikopoi-user-by-id id)))
    (setf (gikopoi-user-character-id user) character-id
          (gikopoi-user-alt-p user) (eq altp t))))

(gikopoi-defevent server-msg (id message)
  (gikopoi-user-msg (gikopoi-user-by-id id) message))

(gikopoi-defevent server-roleplay (id message)
  (gikopoi-user-roleplay (gikopoi-user-by-id id) message))

(gikopoi-defevent server-roll-die (id base sum arga &optional argb)
  (gikopoi-user-roll-die (gikopoi-user-by-id id) base sum (or argb arga)))


;;; Rooms

(defclass gikopoi-room-base-class ()
  ((id            :initarg :id            :accessor gikopoi-room-id)
   (assets        :initarg :assets)
   (users         :initarg :users         :accessor gikopoi-room-users)
   (instance-list :initarg :instance-list)
   (streams       :initarg :streams       :accessor gikopoi-room-streams)
   (stream-slot-count)
   (group         :initarg :group)))

(cl-defmethod make-instance ((class (subclass gikopoi-room-base-class)) &rest initargs
                             &key id instance-list &allow-other-keys)
  (if-let ((room (cl-find id (symbol-value instance-list)
                           :test #'equal :key #'gikopoi-room-id)))
      (progn (shared-initialize room initargs) room)
    (car (push (cl-call-next-method) (symbol-value instance-list)))))

(cl-defmethod shared-initialize :after ((this gikopoi-room-base-class) initargs)
  (setf (slot-value this 'users)
        (mapcar #'gikopoi-make-user (slot-value this 'users))))

(cl-defmethod gikopoi-room-add-user ((room gikopoi-room-base-class) user)
  (cl-pushnew user (slot-value room 'users) :test #'equal :key #'gikopoi-user-id)
  (when (member (gikopoi-user-name user) gikopoi-auto-ignore-names)
    (setf (gikopoi-user-ignored-p user) t)))

(cl-defmethod gikopoi-room-remove-user ((room gikopoi-room-base-class) user)
  (setf (slot-value room 'users) (delq user (slot-value room 'users))))

(defvar gikopoi-rooms nil)
(defvar gikopoi-current-room nil)
(defvar gikopoi-current-room-loading-p nil)

(gikopoi-defevent server-update-current-room-state ((currentRoom connectedUsers streams))
  (setq gikopoi-current-room-loading-p t)
  (let ((room (make-instance 'gikopoi-room-base-class
                             :id (alist-get 'id currentRoom)
                             :instance-list 'gikopoi-rooms
                             :group (alist-get 'group currentRoom)
                             :assets currentRoom
                             :users connectedUsers)))
    (setq gikopoi-current-room room))
  (when (fboundp 'gikopoi-load-auto-ignored-users)
    (gikopoi-load-auto-ignored-users))
  (unless gikopoi-reconnecting-p
    (dolist (user (gikopoi-room-users gikopoi-current-room))
      (unless (gikopoi-user-ignored-p user)
        (let ((last-msg (gikopoi-user-last-message user)))
          (when last-msg
            (gikopoi-user-msg user last-msg t))))))
  ;; request room list to populate counts (used by auto-move)
  (when gikopoi--auto-move-on-join-p
    (gikopoi-room-list)))

(gikopoi-defevent server-update-current-room-streams (streams)
  (setf (gikopoi-room-streams gikopoi-current-room) streams))

(gikopoi-defevent server-user-joined-room (user &optional from reconnectingp)
  (let ((user (gikopoi-make-user user)))
    (gikopoi-room-add-user gikopoi-current-room user)
    (gikopoi-user-join user from reconnectingp)))

(gikopoi-defevent server-user-left-room (id &optional for)
  (when-let ((user (gikopoi-user-by-id id)))
    (gikopoi-user-leave user for)
    (gikopoi-room-remove-user gikopoi-current-room user)))

(gikopoi-defevent special-events:server-add-shrine-coin (count)
  (gikopoi-play-sound gikopoi-coin-sound))


;;; Server Messages

(gikopoi-defevent server-stats ((userCount streamCount))
  (message "Gikopoi: %s users, %s streams" userCount streamCount))

(gikopoi-defevent server-system-message (code message)
  (gikopoi-with-message-buffer
    (insert (format "%s[SYSTEM] %s\n" (format-time-string gikopoi-msg-time-format) message))))

(gikopoi-defevent server-reject-movement ())
(gikopoi-defevent server-ok-to-stream ())
(gikopoi-defevent server-not-ok-to-stream (reason))
(gikopoi-defevent server-not-ok-to-take-stream (slot))


;;; Entry Point

(defun gikopoi-read-arglist ()
  (let* ((server
          (let ((input (completing-read
                        (format "Server (default %s): " gikopoi-default-server)
                        (mapcar #'car gikopoi-servers))))
            (if (string-empty-p input) gikopoi-default-server input)))
         (port
          (if gikopoi-prompt-port-p
              (let ((input (read-string (format "Port (default %s): " gikopoi-default-port))))
                (if (string-empty-p input) gikopoi-default-port (string-to-number input)))
            gikopoi-default-port))
         (area
          (let ((input (completing-read
                        (format "Area (default %s): " gikopoi-default-area)
                        (cdr (assoc server gikopoi-servers)))))
            (if (string-empty-p input) gikopoi-default-area input)))
         (room
          (let ((input (read-string (format "Room (default auto): "))))
            (if (string-empty-p input)
                (progn (setq gikopoi--auto-move-on-join-p t)
                       (or gikopoi-default-room "silo"))
              (progn (setq gikopoi--auto-move-on-join-p nil)
                     input))))
         (name
          (let* ((display (if (and gikopoi-default-name
                                   (string-match "#" gikopoi-default-name))
                              (replace-regexp-in-string "#.*" "#*****" gikopoi-default-name)
                            gikopoi-default-name))
                 (input (read-string (format "Name (default %s): " display))))
            (if (string-empty-p input) gikopoi-default-name input)))
         (character
          (let ((input (read-string (format "Character (default %s): " gikopoi-default-character))))
            (if (string-empty-p input) gikopoi-default-character input)))
         (password
          (if gikopoi-prompt-password-p
              (let ((input (read-passwd "Password: ")))
                (if (string-empty-p input) gikopoi-default-password input))
            gikopoi-default-password)))
    (list server port area room name character password)))

(defvar gikopoi-init-functions
  (list #'gikopoi-init-site-directory
        #'gikopoi-init-lang-alist
        #'gikopoi-connect
        #'gikopoi-init-message-buffer
        #'gikopoi-init-auto-ignore
        #'gikopoi-print-single-timestamp
        #'gikopoi-print-timestamps
        #'gikopoi-maybe-start-reconnect-timer)
  "Functions called in order to initialize the client.
Each is called with the args from `gikopoi-read-arglist'.")

;;;###autoload
(defun gikopoi (server port area room name character password)
  "Connect to a Gikopoipoi server.
With default settings, connects to gikopoipoi.net."
  (interactive (gikopoi-read-arglist))
  (setq gikopoi--last-args (list server port area room name character password))
  (run-hooks 'gikopoi-quit-functions)
  (run-hook-with-args 'gikopoi-init-functions
                      server port area room name character password))

(provide 'gikopoipoi)

;;; gikopoipoi.el ends here
