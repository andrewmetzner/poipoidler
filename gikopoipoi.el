;;; gikopoipoi.el --- Gikopoipoi.net chat client -*- lexical-binding: t; coding: utf-8 -*-

;; Based on gikomacs by gyudon_addict◆hawaiiZtQ6
;; https://github.com/andrewmetzner/gikomacs
;;
;; Package-Requires: ((emacs "28.1") (websocket "1.15"))
;; Keywords: games, chat, client
;; Version: 2.0

;;; Code:


;;; ── 1. Dependencies ───────────────────────────────────────────────────────

(eval-when-compile
  (require 'subr-x)
  (require 'let-alist))

(require 'cl-lib)
(require 'color)
(require 'json)
(require 'seq)
(require 'thingatpt)
(require 'url)
(require 'url-http)
(require 'websocket)


;;; ── 2. Customization ──────────────────────────────────────────────────────

(defgroup gikopoi nil
  "Client for Gikopoipoi.net and compatible servers."
  :group 'applications)

(defcustom gikopoi-default-server "gikopoipoi.net"
  "Server to connect to by default."
  :group 'gikopoi :type '(choice (const nil) string))

(defcustom gikopoi-default-port 443
  "Server port."
  :group 'gikopoi :type 'natnum)

(defcustom gikopoi-prompt-port-p nil
  "If non-nil, prompt for port on connect."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-servers
  '(("gikopoipoi.net"  "for" "gen")
    ("play.gikopoi.com" "for" "gen" "vip")
    ("gikopoi.hu"       "int" "hun"))
  "Alist of server → area IDs.  Only areas that the server accepts are listed."
  :group 'gikopoi :type '(repeat (cons string (repeat string))))

(defcustom gikopoi-default-name nil
  "Username; nil means prompt every time."
  :group 'gikopoi :type '(choice (const nil) string))

(defcustom gikopoi-default-character "giko"
  "Character sprite ID."
  :group 'gikopoi :type '(choice (const nil) string))

(defcustom gikopoi-default-area "for"
  "Area ID to join by default."
  :group 'gikopoi :type '(choice (const nil) string))

(defcustom gikopoi-default-room nil
  "Room ID to join by default; nil means prompt."
  :group 'gikopoi :type '(choice (const nil) string))

(defcustom gikopoi-default-password nil
  "Server password; nil means none."
  :group 'gikopoi :type '(choice (const nil) string))

(defcustom gikopoi-prompt-password-p nil
  "If non-nil, prompt for a password on connect."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-preferred-language 'en
  "Language used for room/area name display."
  :group 'gikopoi :type '(choice symbol (repeat symbol)))

(defcustom gikopoi-autoquote-format "> %s < "
  "Format string for `gikopoi-autoquote' (one %s = quoted text)."
  :group 'gikopoi :type 'string)

(defcustom gikopoi-mention-regexp regexp-unmatchable
  "Regexp matched against incoming messages to flag mentions."
  :group 'gikopoi :type 'regexp)

(defcustom gikopoi-mention-color "red"
  "Face colour for messages that match `gikopoi-mention-regexp'."
  :group 'gikopoi :type '(choice (const nil) string))

(defcustom gikopoi-notif-position '(mode-line-modes . nil)
  "Where to insert the unread-count notifier in the mode line."
  :group 'gikopoi :type '(cons variable boolean))

(defcustom gikopoi-timestamp-interval 3600
  "Seconds between full timestamps in the chat buffer; nil disables them."
  :group 'gikopoi :type '(choice (const nil) number))

(defcustom gikopoi-time-format "* %a %b %d %Y %T GMT%z (%Z)\n"
  "`format-time-string' pattern for full timestamps."
  :group 'gikopoi :type 'string)

(defcustom gikopoi-msg-time-format "[%H:%M:%S] "
  "Short timestamp prepended to each message."
  :group 'gikopoi :type 'string)

(defcustom gikopoi-logger nil
  "If non-nil, append chat to a daily log file."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-log-directory (expand-file-name "~/.gikopoi-logs/")
  "Directory for daily chat logs."
  :group 'gikopoi :type 'string)

(defcustom gikopoi-auto-reconnect t
  "If non-nil, reconnect automatically when the server drops the connection."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-reconnect-max-delay 300
  "Maximum seconds between auto-reconnect attempts (doubles each failure)."
  :group 'gikopoi :type 'natnum)

(defcustom gikopoi-auto-start-reconnect-timer nil
  "If non-nil, start the periodic reconnect timer on connect."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-reconnect-timer-minutes 720
  "Period in minutes for the optional periodic reconnect timer."
  :group 'gikopoi :type 'natnum)

(defcustom gikopoi-auto-clear-bubble t
  "If non-nil, automatically send a blank message to clear your speech bubble after chatting."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-anon-numbers t
  "If non-nil, append a number to anonymous users (e.g. Anonymous#2731).
The number is derived from the last 3 hex digits of the user's session ID,
matching the poipoi browser extension.  Toggle with \\[gikopoi-toggle-anon-numbers]."
  :group 'gikopoi :type 'boolean)


;;; ── 3. Global State ───────────────────────────────────────────────────────

(defun gikopoi--anon-number (id)
  "Derive a 0–4095 number from the last 3 hex chars of user ID.
Matches the algorithm used by the poipoi browser extension."
  (string-to-number (substring id -3) 16))

(defvar gikopoi-current-server  nil "Hostname of the active connection.")
(defvar gikopoi-current-user-id nil "Public user ID assigned by the server.")
(defvar gikopoi-current-private-user-id nil "Private user ID (used in WS header).")
(defvar gikopoi-current-user    nil "The local `gikopoi-user' object.")
(defvar gikopoi-current-room    nil "The active `gikopoi-room' object.")
(defvar gikopoi-current-room-loading-p nil "Non-nil while a room transition is in progress.")
(defvar gikopoi-rooms           nil "All `gikopoi-room' objects seen this session.")
(defvar gikopoi--server-user-count   0   "Last user count from server-stats.")
(defvar gikopoi--server-stream-count 0   "Last stream count from server-stats.")
(defvar gikopoi--stats-mode-line     ""  "Mode-line fragment updated by server-stats.")


;;; ── 4. Filesystem Defaults ────────────────────────────────────────────────

(defconst gikopoi-default-directory
  (or (and load-file-name (file-name-directory load-file-name))
      default-directory)
  "Directory containing this file; used to locate assets and langs.")

(defvar gikopoi-site-directory nil)

(defun gikopoi-init-site-directory (server &rest _)
  (let* ((sites   (expand-file-name "sites" gikopoi-default-directory))
         (nosrch  (expand-file-name ".nosearch" sites))
         (sitedir (file-name-as-directory (expand-file-name server sites))))
    (unless (file-exists-p nosrch) (make-empty-file nosrch t))
    (make-directory (expand-file-name "rooms" sitedir) t)
    (setq gikopoi-site-directory sitedir)))


;;; ── 5. Sound Effects ──────────────────────────────────────────────────────

(defconst gikopoi-login-sound      (expand-file-name "login.au"          gikopoi-default-directory))
(defconst gikopoi-message-sound    (expand-file-name "message.au"        gikopoi-default-directory))
(defconst gikopoi-mention-sound    (expand-file-name "mention.au"        gikopoi-default-directory))
(defconst gikopoi-disconnect-sound (expand-file-name "connection-lost.au" gikopoi-default-directory))
(defconst gikopoi-coin-sound       (expand-file-name "ka-ching.au"       gikopoi-default-directory))

(defun gikopoi-play-sound (file)
  (ignore-errors (play-sound-file file)))


;;; ── 6. Language / i18n ────────────────────────────────────────────────────

(defvar gikopoi-lang-directory nil)
(defvar gikopoi-lang-alist     nil)

(defun gikopoi-init-lang-alist (&rest _)
  (setq gikopoi-lang-directory
        (file-name-as-directory (expand-file-name "langs" gikopoi-default-directory)))
  (let ((file (expand-file-name
               (symbol-name (or (car-safe gikopoi-preferred-language)
                                gikopoi-preferred-language 'en))
               gikopoi-lang-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (setq gikopoi-lang-alist (read (current-buffer)))))))


;;; ── 7. Logging ────────────────────────────────────────────────────────────

(defun gikopoi--log-to-file (text)
  (when gikopoi-logger
    (let* ((dir  gikopoi-log-directory)
           (file (expand-file-name (concat (format-time-string "%Y-%m-%d") ".txt") dir)))
      (unless (file-exists-p dir) (make-directory dir t))
      (with-temp-buffer
        (insert (or text ""))
        (append-to-file (point-min) (point-max) file)))))


;;; ── 8. HTTP API ───────────────────────────────────────────────────────────

(defun gikopoi-api-version (server)
  "Return the integer app version reported by SERVER."
  (with-temp-buffer
    (url-insert-file-contents (format "https://%s/api/version" server))
    (number-at-point)))

(defun gikopoi-api-log (server message)
  "Send MESSAGE to SERVER's client-log endpoint (fire and forget)."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (url-request-data (encode-coding-string message 'utf-8)))
    (url-retrieve-synchronously (format "https://%s/api/client-log" server))))

(defun gikopoi-api-login (server area room name character password)
  "POST login credentials to SERVER; return the parsed JSON alist or signal an error."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string
          (json-encode `((userName    . ,name)
                         (characterId . ,character)
                         (areaId      . ,area)
                         (roomId      . ,room)
                         ,@(when (and password (not (string-empty-p password)))
                             `((password . ,password)))))
          'utf-8)))
    (let ((buf (url-retrieve-synchronously
                (format "https://%s/api/login" server) :silent t)))
      (unless buf (error "Gikopoi: login request failed (no response)"))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n" nil t)
            (condition-case err
                (json-read-object)
              (error
               (error "Gikopoi: login response parse error: %s"
                      (error-message-string err)))))
        (kill-buffer buf)))))


;;; ── 9. WebSocket / Socket.IO Protocol ────────────────────────────────────
;;;
;;; EIO=4 packet IDs:
;;;   0  – open  (server sends JSON handshake)
;;;   2  – ping  (server-initiated; we reply with "3")
;;;   3  – pong
;;;  40  – socket.io connect ack
;;;  42  – socket.io event  → ["eventName", arg1, arg2, …]

(defvar gikopoi-socket           nil)
(defvar gikopoi-socket-interval  nil)
(defvar gikopoi-socket-tolerance 1)
(defvar gikopoi-socket-ping-timer nil)
(defvar gikopoi-socket-timeout   nil)
(defvar gikopoi-reconnecting-p   nil)

;;; Reconnect machinery

(defvar gikopoi--deliberately-quit    nil "Non-nil when the user asked to disconnect.")
(defvar gikopoi--reconnect-timer      nil)
(defvar gikopoi--reconnect-delay      5   "Current delay for the next reconnect attempt.")
(defvar gikopoi--last-args            nil "Args from the last `gikopoi' call; used by reconnect.")

(defun gikopoi--schedule-reconnect (delay)
  "Schedule a full re-login + reconnect in DELAY seconds, with exponential back-off."
  (when (timerp gikopoi--reconnect-timer)
    (cancel-timer gikopoi--reconnect-timer))
  (when (buffer-live-p gikopoi-message-buffer)
    (gikopoi-with-message-buffer
      (insert (format "%s* disconnected — retrying in %ds\n"
                      (format-time-string gikopoi-msg-time-format) delay))))
  (setq gikopoi--reconnect-delay delay
        gikopoi--reconnect-timer
        (run-at-time delay nil
                     (lambda ()
                       (condition-case err
                           (gikopoi-reconnect)
                         (error
                          (gikopoi--schedule-reconnect
                           (min (* gikopoi--reconnect-delay 2)
                                gikopoi-reconnect-max-delay))))))))

;;; WebSocket open / close

(defun gikopoi--ws-url (server port)
  (if (= port 443)
      (format "wss://%s/socket.io/?EIO=4&transport=websocket" server)
    (format "ws://%s:%d/socket.io/?EIO=4&transport=websocket" server port)))

(defun gikopoi-socket-open (server port pid)
  (setq gikopoi-socket
        (websocket-open
         (gikopoi--ws-url server port)
         :custom-header-alist `((private-user-id . ,pid)
                                (perMessageDeflate . false))
         :on-open    (lambda (_sock) (websocket-send-text gikopoi-socket "40"))
         :on-close   (lambda (_sock)
                       (when (timerp gikopoi-socket-ping-timer)
                         (cancel-timer gikopoi-socket-ping-timer))
                       (when (and gikopoi-auto-reconnect
                                  (not gikopoi--deliberately-quit))
                         (gikopoi--schedule-reconnect 5)))
         :on-message #'gikopoi--ws-message-handler))
  (setf (websocket-client-data gikopoi-socket) (list server port pid))
  gikopoi-socket)

(defun gikopoi-socket-close ()
  (when (and gikopoi-socket (websocket-openp gikopoi-socket))
    (websocket-close gikopoi-socket))
  (when (timerp gikopoi-socket-ping-timer)
    (cancel-timer gikopoi-socket-ping-timer)))

(defun gikopoi-socket-emit (event &rest args)
  "Send EVENT with ARGS as a socket.io packet to the server."
  (if (and gikopoi-socket (websocket-openp gikopoi-socket))
      (websocket-send-text
       gikopoi-socket
       (concat "42" (encode-coding-string
                     (json-encode (apply #'vector event args)) 'utf-8)))
    (user-error "Gikopoi: not connected")))

;;; Low-level socket.io commands

(defun gikopoi-send (message)
  (gikopoi-socket-emit "user-msg" message))

(defun gikopoi-move (direction)
  (gikopoi-socket-emit "user-move" direction))

(defun gikopoi-bubble-position (direction)
  (gikopoi-socket-emit "user-bubble-position" direction))

(defun gikopoi-change-room (room-id &optional door-id)
  (gikopoi-socket-emit "user-change-room"
                       `((targetRoomId . ,room-id)
                         ,@(when door-id `((targetDoorId . ,door-id))))))

(defun gikopoi-room-list-request ()
  "Fetch the room list via REST API (the old user-room-list WS event is no longer served)."
  (let* ((server (or gikopoi-current-server "gikopoipoi.net"))
         (area   (or (nth 2 gikopoi--last-args) gikopoi-default-area "for"))
         (pid    gikopoi-current-private-user-id)
         (url    (format "https://%s/api/areas/%s/rooms" server area))
         (url-request-method "GET")
         (url-request-extra-headers
          (when pid `(("Authorization" . ,(format "Bearer %s" pid))))))
    (url-retrieve url
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Gikopoi: room list fetch failed: %s"
                                 (plist-get status :error))
                      (goto-char (point-min))
                      (re-search-forward "\r?\n\r?\n" nil t)
                      (condition-case err
                          (gikopoi--on-room-list (json-parse-buffer
                                                  :array-type  'list
                                                  :object-type 'alist))
                        (error
                         (message "Gikopoi: room list parse error: %s"
                                  (error-message-string err))))))
                  nil :silent :inhibit-cookies)))

(defun gikopoi-user-ping ()
  (gikopoi-socket-emit "user-ping"))

;;; Inbound message dispatch

(defun gikopoi--ws-message-handler (_sock frame)
  (condition-case err
      (let ((text (websocket-frame-text frame)))
        (unless (stringp text) (signal 'gikopoi-skip nil)) ; binary frame – ignore
        (let (id payload)
          (with-temp-buffer
            (insert text)
            (goto-char (point-min))
            (setq id (thing-at-point 'number))
            (forward-word)
            (setq payload (ignore-errors (json-read))))
          (pcase id
            (0  ; EIO handshake
             (let-alist payload
               (setq gikopoi-socket-interval (/ .pingInterval 1000)
                     gikopoi-socket-timeout  (/ .pingTimeout  1000))))
            (2  ; ping – reset watchdog timer, send pong
             (when (timerp gikopoi-socket-ping-timer)
               (cancel-timer gikopoi-socket-ping-timer))
             (websocket-send-text gikopoi-socket "3")
             (setq gikopoi-socket-ping-timer
                   (run-at-time (+ gikopoi-socket-interval gikopoi-socket-tolerance)
                                nil
                                (lambda ()
                                  (unless gikopoi--deliberately-quit
                                    (gikopoi--schedule-reconnect 5))))))
            (40 nil) ; socket.io connect ack
            (42 (when (vectorp payload) (gikopoi--dispatch-event payload)))
            (_  nil)))) ; unknown id – silently ignore
    (gikopoi-skip nil) ; binary frame sentinel – no message
    (error
     (let ((msg (error-message-string err)))
       (unless (or (string-match-p "No usable sound device driver" msg)
                   (string-match-p "not connected" msg))
         (message "Gikopoi: handler error: %s" msg))))))


;;; ── 10. Server Event System ───────────────────────────────────────────────
;;;
;;; Events arrive as socket.io packets: 42["eventName", arg1, arg2, …]
;;; After parsing, payload is a vector: ["eventName" arg1 arg2 …]
;;;
;;; `gikopoi-defevent' registers a handler on the symbol's property list.
;;; Handlers with a destructured first arg accept a single alist and
;;; extract named keys from it; otherwise each arg maps 1-to-1.

(defmacro gikopoi-defevent (name args &rest body)
  "Define a handler for server event NAME.
ARGS may include (KEY …) forms that destructure a single alist argument."
  (declare (indent defun))
  (let (alist-args)
    `(put ',name 'gikopoi-event-handler
          (lambda ,(mapcar (lambda (a)
                             (if (consp a)
                                 (caar (push (cons (gensym) a) alist-args))
                               a))
                           args)
            (let ,(mapcan
                   (lambda (entry)
                     (mapcar (lambda (key)
                               `(,key (cdr (assq ',key ,(car entry)))))
                             (cdr entry)))
                   alist-args)
              ,@(or body '(nil)))))))


(defun gikopoi--dispatch-event (payload)
  "Route PAYLOAD (a vector [\"eventName\" arg…]) to the matching handler."
  (let* ((event-name (aref payload 0))
         (name (and (stringp event-name) (intern-soft event-name)))
         (fn   (and name (get name 'gikopoi-event-handler))))
    (when (stringp event-name)
      (if fn
          (apply fn (cl-coerce (substring payload 1) 'list))
        (message "Gikopoi: unhandled event %s" event-name)))))


;;; ── 11. Domain Classes ────────────────────────────────────────────────────

;;; — User ——————————————————————————————————————————————————————————————————

(defclass gikopoi-user ()
  ((id              :initarg :id              :accessor gikopoi-user-id)
   (name            :initarg :name            :accessor gikopoi-user-name)
   (raw-name        :initform ""             :accessor gikopoi-user-raw-name)
   (character-id    :initarg :character-id    :accessor gikopoi-user-character-id)
   (alt-p           :initarg :alt-p           :accessor gikopoi-user-alt-p)
   (position        :initarg :position        :accessor gikopoi-user-position)
   (direction       :initarg :direction       :accessor gikopoi-user-direction)
   (last-message    :initarg :last-message    :accessor gikopoi-user-last-message)
   (bubble-position :initarg :bubble-position :accessor gikopoi-user-bubble-position)
   (active-p        :initarg :active-p        :accessor gikopoi-user-active-p)
   (last-movement   :initarg :last-movement   :accessor gikopoi-user-last-movement)
   (voice-pitch     :initarg :voice-pitch     :accessor gikopoi-user-voice-pitch)
   (ignored-p       :initform nil             :accessor gikopoi-user-ignored-p)
   (name-color      :accessor gikopoi-user-name-color))
  "A Gikopoi user in the current room.")

(cl-defmethod shared-initialize :after ((u gikopoi-user) _initargs)
  (let* ((h   (/ (mod (sxhash (slot-value u 'id)) 360.0) 360.0))
         (rgb (color-hsl-to-rgb h 0.6 0.7)))
    (setf (slot-value u 'name-color) (apply #'color-rgb-to-hex rgb)))
  ;; Preserve the raw server-supplied name before propertizing
  (setf (slot-value u 'raw-name) (or (slot-value u 'name) ""))
  (setf (gikopoi-user-name u) (slot-value u 'raw-name))
  ;; Track ourselves
  (when (equal gikopoi-current-user-id (slot-value u 'id))
    (setq gikopoi-current-user u)))

(cl-defmethod (setf gikopoi-user-name) (name (u gikopoi-user))
  (setf (slot-value u 'name)
        (propertize (if (string-empty-p name)
                        (if gikopoi-anon-numbers
                            (format "Anonymous#%d"
                                    (gikopoi--anon-number (slot-value u 'id)))
                          "Anonymous")
                      name)
                    'face `(:foreground ,(slot-value u 'name-color)))))

(defun gikopoi-toggle-anon-numbers ()
  "Toggle anonymous user numbering and refresh all names in the current room."
  (interactive)
  (setq gikopoi-anon-numbers (not gikopoi-anon-numbers))
  (when gikopoi-current-room
    (dolist (u (gikopoi-room-users gikopoi-current-room))
      (setf (gikopoi-user-name u) (gikopoi-user-raw-name u))))
  (message "Gikopoi: anon numbers %s" (if gikopoi-anon-numbers "on" "off")))

(defun gikopoi-make-user (alist)
  "Construct a `gikopoi-user' from a server-supplied ALIST."
  (let-alist alist
    (make-instance 'gikopoi-user
                   :id              .id
                   :name            .name
                   :character-id    .characterId
                   :alt-p           (eq .isAlternateCharacter t)
                   :position        (cons .position.x .position.y)
                   :direction       .direction
                   :last-message    .lastRoomMessage
                   :voice-pitch     .voicePitch
                   :bubble-position .bubblePosition
                   :active-p        (eq .isInactive json-false)
                   :last-movement   .lastMovement)))

(defun gikopoi-user-by-id (id)
  (cl-find id (gikopoi-room-users gikopoi-current-room)
           :test #'equal :key #'gikopoi-user-id))

(defun gikopoi-user-by-name (name)
  (cl-find name (gikopoi-room-users gikopoi-current-room)
           :test #'string= :key #'gikopoi-user-raw-name))

(defun gikopoi-user-names ()
  (mapcar #'gikopoi-user-name (gikopoi-room-users gikopoi-current-room)))

;;; — Room ——————————————————————————————————————————————————————————————————

(defclass gikopoi-room ()
  ((id      :initarg :id      :accessor gikopoi-room-id)
   (group   :initarg :group)
   (assets  :initarg :assets)
   (users   :initarg :users   :accessor gikopoi-room-users)
   (streams :initarg :streams :accessor gikopoi-room-streams))
  "A Gikopoi room, as reported by `server-update-current-room-state'.")

(cl-defmethod shared-initialize :after ((r gikopoi-room) _initargs)
  (setf (slot-value r 'users)
        (mapcar #'gikopoi-make-user (slot-value r 'users))))

(cl-defmethod gikopoi-room-add-user ((r gikopoi-room) (u gikopoi-user))
  (cl-pushnew u (slot-value r 'users) :test #'equal :key #'gikopoi-user-id)
  (when (member (gikopoi-user-raw-name u) gikopoi-auto-ignore-names)
    (setf (gikopoi-user-ignored-p u) t)))

(cl-defmethod gikopoi-room-remove-user ((r gikopoi-room) (u gikopoi-user))
  (setf (slot-value r 'users) (delq u (slot-value r 'users))))

(defun gikopoi--get-or-create-room (id group assets users)
  "Return existing room with ID or create a fresh one."
  (let ((existing (cl-find id gikopoi-rooms :test #'equal :key #'gikopoi-room-id)))
    (if existing
        (progn
          (shared-initialize existing (list :id id :group group :assets assets :users users))
          existing)
      (let ((r (make-instance 'gikopoi-room
                              :id id :group group :assets assets :users users)))
        (push r gikopoi-rooms)
        r))))


;;; ── 12. Message Display ───────────────────────────────────────────────────

(defvar gikopoi-message-matched-p nil)

(cl-defmethod gikopoi-user-insert-message ((u gikopoi-user) text)
  (unless (gikopoi-user-ignored-p u)
    (let ((line (concat (format-time-string gikopoi-msg-time-format) (or text ""))))
      (gikopoi--log-to-file line)
      (if (eq u gikopoi-current-user)
          (gikopoi-clear-mentions)
        (when (setq gikopoi-message-matched-p
                    (string-match gikopoi-mention-regexp line))
          (when gikopoi-mention-color
            (put-text-property (match-beginning 0) (match-end 0)
                               'face `(:foreground ,gikopoi-mention-color) line)))
        (unless (get-buffer-window gikopoi-message-buffer 'visible)
          (cl-incf gikopoi-unread-count)
          (when gikopoi-message-matched-p
            (cl-pushnew (gikopoi-user-name u) gikopoi-notif-names :test #'equal))))
      (force-mode-line-update)
      (gikopoi-with-message-buffer (insert line)))))

(cl-defmethod gikopoi-user-msg ((u gikopoi-user) message &optional silentp)
  (setf (gikopoi-user-last-message u) message)
  (unless (string-empty-p (or message ""))
    (let* ((name      (gikopoi-user-name u))
           (formatted (mapconcat (lambda (l) (format "%s: %s\n" name l))
                                 (split-string message "\n") "")))
      (gikopoi-user-insert-message u formatted)
      (unless silentp
        (gikopoi-play-sound
         (if (and gikopoi-message-matched-p (not (eq u gikopoi-current-user)))
             gikopoi-mention-sound
           gikopoi-message-sound))))))

(cl-defmethod gikopoi-user-roleplay ((u gikopoi-user) message)
  (gikopoi-user-insert-message
   u (format "* %s %s\n" (gikopoi-user-name u) message)))

(cl-defmethod gikopoi-user-roll-die ((u gikopoi-user) base sum times)
  (gikopoi-user-insert-message
   u (format "[DICE] %s rolled %sx d%s → %s\n"
             (gikopoi-user-name u) times base sum)))

(cl-defmethod gikopoi-user-join ((u gikopoi-user) &optional from reconnectp)
  (unless (or reconnectp (gikopoi-user-ignored-p u))
    (gikopoi-user-insert-message
     u (format "* %s has entered the room%s"
               (gikopoi-user-name u)
               (if from (format " from %s\n" from) "\n")))
    (gikopoi-play-sound gikopoi-login-sound)))

(cl-defmethod gikopoi-user-leave ((u gikopoi-user) &optional destination)
  (gikopoi-user-insert-message
   u (format "* %s has left the room%s"
             (gikopoi-user-name u)
             (if destination (format " for %s\n" destination) "\n"))))

(cl-defmethod (setf gikopoi-user-active-p) :after ((val (eql nil)) (u gikopoi-user))
  (gikopoi-user-insert-message
   u (format "* %s is away\n" (slot-value u 'name))))


;;; ── 13. Server Event Handlers ─────────────────────────────────────────────

;;; Room events

(gikopoi-defevent server-update-current-room-state ((currentRoom connectedUsers streams))
  (let ((prev-id (and gikopoi-current-room
                      (gikopoi-room-id gikopoi-current-room)))
        (new-id  (alist-get 'id currentRoom)))
    (setq gikopoi-current-room-loading-p t
          gikopoi-current-room
          (gikopoi--get-or-create-room
           new-id
           (alist-get 'group currentRoom)
           currentRoom
           connectedUsers))
    (when (fboundp 'gikopoi-load-auto-ignored-users)
      (gikopoi-load-auto-ignored-users))
    (when (null gikopoi-room-list-data)
      (gikopoi-room-list-request))
    (when (and prev-id (not (equal prev-id new-id)))
      (gikopoi-with-message-buffer
        (insert (format "%s* now in room: %s\n"
                        (format-time-string gikopoi-msg-time-format)
                        new-id)))
      (force-mode-line-update t)
      (gikopoi--refresh-user-list-buffer))
    (unless gikopoi-reconnecting-p
      (dolist (u (gikopoi-room-users gikopoi-current-room))
        (unless (gikopoi-user-ignored-p u)
          (when-let ((msg (gikopoi-user-last-message u)))
            (gikopoi-user-msg u msg t)))))))

(gikopoi-defevent server-update-current-room-streams (streams)
  (setf (gikopoi-room-streams gikopoi-current-room) streams)
  (force-mode-line-update))

(gikopoi-defevent server-user-joined-room (user &optional from reconnectingp)
  (let ((u (gikopoi-make-user user)))
    (gikopoi-room-add-user gikopoi-current-room u)
    (gikopoi-user-join u from reconnectingp)
    (gikopoi--refresh-user-list-buffer)))

(gikopoi-defevent server-user-left-room (id &optional destination)
  (when-let ((u (gikopoi-user-by-id id)))
    (gikopoi-user-leave u destination)
    (gikopoi-room-remove-user gikopoi-current-room u)
    (gikopoi--refresh-user-list-buffer)))

;;; User events

(gikopoi-defevent server-user-active (id)
  (when-let ((u (gikopoi-user-by-id id)))
    (setf (gikopoi-user-active-p u) t)))

(gikopoi-defevent server-user-inactive (id)
  (when-let ((u (gikopoi-user-by-id id)))
    (setf (gikopoi-user-active-p u) nil)))

(gikopoi-defevent server-move ((userId x y direction lastMovement isInstant shouldSpinwalk))
  (when-let ((u (gikopoi-user-by-id userId)))
    (setf (gikopoi-user-position      u) (cons x y)
          (gikopoi-user-direction     u) direction
          (gikopoi-user-last-movement u) lastMovement)))

(gikopoi-defevent server-bubble-position (id direction)
  (when-let ((u (gikopoi-user-by-id id)))
    (setf (gikopoi-user-bubble-position u) direction)))

(gikopoi-defevent server-character-changed (id character-id altp)
  (when-let ((u (gikopoi-user-by-id id)))
    (setf (gikopoi-user-character-id u) character-id
          (gikopoi-user-alt-p        u) (eq altp t))))

;;; Message events

(gikopoi-defevent server-msg (id message)
  (when-let ((u (gikopoi-user-by-id id)))
    (gikopoi-user-msg u message)
    (gikopoi--refresh-user-list-buffer)))

(gikopoi-defevent server-roleplay (id message)
  (when-let ((u (gikopoi-user-by-id id)))
    (gikopoi-user-roleplay u message)))

(gikopoi-defevent server-roll-die (id base sum arga &optional argb)
  (when-let ((u (gikopoi-user-by-id id)))
    (gikopoi-user-roll-die u base sum (or argb arga))))

;;; Server info events

(gikopoi-defevent server-stats ((userCount streamCount))
  (let ((n (if (stringp userCount)   (string-to-number userCount)   (or userCount 0)))
        (s (if (stringp streamCount) (string-to-number streamCount) (or streamCount 0))))
    (setq gikopoi--server-user-count   n
          gikopoi--server-stream-count s
          gikopoi--stats-mode-line
          (format " [%su%s]" n (if (> s 0) (format " %ss" s) "")))
    (force-mode-line-update)))

(gikopoi-defevent server-system-message (_code message)
  (gikopoi-with-message-buffer
    (insert (format "%s[SYSTEM] %s\n"
                    (format-time-string gikopoi-msg-time-format)
                    message))))

(gikopoi-defevent special-events:server-add-shrine-coin (_count)
  (gikopoi-play-sound gikopoi-coin-sound))

;;; No-op stubs for events we acknowledge but do nothing with

(gikopoi-defevent server-reject-movement ())
(gikopoi-defevent server-ok-to-stream ())
(gikopoi-defevent server-not-ok-to-stream (_reason))
(gikopoi-defevent server-not-ok-to-take-stream (_slot))
(gikopoi-defevent server-cant-log-you-in ())
(gikopoi-defevent server-update-chessboard (_state))
(gikopoi-defevent server-update-janken (_state))
(gikopoi-defevent server-chess-win (_id))
(gikopoi-defevent server-chess-quit ())


;;; ── 14. Room List ─────────────────────────────────────────────────────────

(defvar gikopoi-room-list-data nil
  "Tabulated-list entries for the room list buffer.
Each entry is (ROOM-ID [NAME AREA COUNT STREAMERS]).")

(defvar gikopoi--room-user-counts (make-hash-table :test #'equal)
  "Maps room ID (string) → integer user count from the last server-room-list.")

(defvar gikopoi--show-room-list-p nil)
(defvar gikopoi--join-busiest-p   nil)

(defvar gikopoi-room-list-buffer nil)

(defun gikopoi--streamer-display (streamers)
  "Return a display string from STREAMERS (array of strings or objects)."
  (when (and streamers (not (eq streamers 'json-null)) (not (eq streamers :null)))
    (let (names)
      (seq-doseq (s streamers)
        (cond ((stringp s) (push s names))
              ((listp s)
               (push (or (alist-get 'userName s)
                         (alist-get 'userId   s)
                         "?")
                     names))))
      (string-join (nreverse names) " "))))

(defun gikopoi-update-room-list (rooms)
  "Rebuild `gikopoi-room-list-data' and `gikopoi--room-user-counts' from ROOMS vector."
  (clrhash gikopoi--room-user-counts)
  (setq gikopoi-room-list-data nil)
  (seq-doseq (room rooms)
    (condition-case err
        (let-alist room
          (when .id
            (let* ((id         .id)
                   (room-group .group)
                   (n          (if (numberp .userCount) .userCount 0))
                   (count      (number-to-string n))
                   (streams    (or (gikopoi--streamer-display .streams) "")))
              (puthash id n gikopoi--room-user-counts)
              (let-alist gikopoi-lang-alist
                (let* ((name (cdr (assoc id .room #'string-equal)))
                       (name (or (and (consp name) (cdr (assq 'sort_key name))) name id))
                       (area (cdr (assoc room-group .area #'string-equal)))
                       (area (or (and (consp area) (cdr (assq 'sort_key area))) area room-group)))
                  (push (list id (vector name area count streams))
                        gikopoi-room-list-data))))))
      (error
       (message "Gikopoi: skipping bad room entry: %s" (error-message-string err)))))
  (setq gikopoi-room-list-data (nreverse gikopoi-room-list-data)))

(defun gikopoi--busiest-room-id ()
  "Return the room ID with the most users, excluding the current room."
  (let ((cur (and gikopoi-current-room (gikopoi-room-id gikopoi-current-room)))
        best-id best-n)
    (maphash (lambda (id n)
               (when (and (not (equal id cur))
                          (or (null best-n) (> n best-n)))
                 (setq best-id id best-n n)))
             gikopoi--room-user-counts)
    best-id))

(defun gikopoi--overall-busiest-room-id ()
  "Return the room ID with the most users, including the current room."
  (let (best-id best-n)
    (maphash (lambda (id n)
               (when (or (null best-n) (> n best-n))
                 (setq best-id id best-n n)))
             gikopoi--room-user-counts)
    best-id))

(defun gikopoi--on-room-list (rooms)
  "Process ROOMS list (from HTTP or WS) and update display / busiest-join logic."
  (gikopoi-update-room-list rooms)
  (cond
   (gikopoi--join-busiest-p
    (setq gikopoi--join-busiest-p nil)
    (let* ((cur     (and gikopoi-current-room (gikopoi-room-id gikopoi-current-room)))
           (overall (gikopoi--overall-busiest-room-id)))
      (if (equal cur overall)
          (gikopoi-with-message-buffer
            (insert (format "%s* already in the busiest room (%d users)\n"
                            (format-time-string gikopoi-msg-time-format)
                            (gethash cur gikopoi--room-user-counts 0))))
        (if-let ((id (gikopoi--busiest-room-id)))
            (progn
              (gikopoi-with-message-buffer
                (insert (format "%s* joining busiest room: %s (%d users)\n"
                                (format-time-string gikopoi-msg-time-format)
                                id (gethash id gikopoi--room-user-counts 0))))
              (gikopoi-change-room id))
          (message "Gikopoi: no other rooms with users found")))))
   (t
    (setq gikopoi--show-room-list-p nil)
    (gikopoi--refresh-room-list-buffer))))

(gikopoi-defevent server-room-list (rooms)
  (gikopoi--on-room-list rooms))

(defun gikopoi-room-list-change-entry ()
  (interactive)
  (gikopoi-change-room (tabulated-list-get-id)))

(defvar gikopoi-room-list-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'gikopoi-room-list-change-entry)
    m))

(define-minor-mode gikopoi-room-list-mode
  "Minor mode active in the *Gikopoi Rooms* buffer."
  :group 'gikopoi :keymap gikopoi-room-list-mode-map)

(defun gikopoi-init-room-list-buffer ()
  (setq gikopoi-room-list-buffer (get-buffer-create "*Gikopoi Rooms*"))
  (with-current-buffer gikopoi-room-list-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format
          [("Room" 32 t) ("Area" 14 t) ("Users" 6 t) ("Streams" 0 nil)])
    (tabulated-list-init-header)
    (add-hook 'tabulated-list-revert-hook
              (lambda ()
                (setq tabulated-list-entries gikopoi-room-list-data)
                (gikopoi-room-list-request))
              nil t)
    (gikopoi-room-list-mode)))

(defun gikopoi--refresh-room-list-buffer ()
  "Update *Gikopoi Rooms* buffer contents in place."
  (when (buffer-live-p gikopoi-room-list-buffer)
    (with-current-buffer gikopoi-room-list-buffer
      (setq tabulated-list-entries gikopoi-room-list-data)
      (tabulated-list-print t))))

(defun gikopoi-list-rooms ()
  "Show the room list buffer and request a fresh update from the server.
The list shows the number of users per room (N) and any live streamers.
Full user names per room are not available from the server room-list API."
  (interactive)
  (unless (buffer-live-p gikopoi-room-list-buffer)
    (gikopoi-init-room-list-buffer))
  ;; Show current data immediately, then refresh when the server responds
  (gikopoi--refresh-room-list-buffer)
  (pop-to-buffer gikopoi-room-list-buffer)
  (setq gikopoi--show-room-list-p t)
  (gikopoi-room-list-request))

(defun gikopoi-join-busiest-room ()
  "Teleport to the room with the most users (excluding the current one).
Requests a fresh room list from the server first."
  (interactive)
  (setq gikopoi--join-busiest-p t)
  (gikopoi-room-list-request))


;;; ── 15. Message Buffer ────────────────────────────────────────────────────

(defvar gikopoi-message-buffer          nil)
(defvar gikopoi--should-scroll-on-visit nil)

(defun gikopoi--at-bottom-p ()
  (when-let ((w (get-buffer-window gikopoi-message-buffer)))
    (<= (- (point-max) (window-end w nil)) 1)))

(defun gikopoi--near-bottom-p ()
  (when-let ((w (get-buffer-window gikopoi-message-buffer)))
    (<= (count-lines (window-end w t) (point-max))
        (window-body-height w))))

(defmacro gikopoi-with-message-buffer (&rest body)
  (declare (indent defun))
  `(when (buffer-live-p gikopoi-message-buffer)
     (with-current-buffer gikopoi-message-buffer
       (let* ((w        (get-buffer-window))
              (at-bot   (and w (gikopoi--at-bottom-p))))
         (save-excursion
           (goto-char (point-max))
           (let ((buffer-read-only nil)) ,@body))
         (if w
             (when at-bot (set-window-point w (point-max)))
           (setq gikopoi--should-scroll-on-visit t))))))

(defun gikopoi-message-buffer-scroll-to-end ()
  (when-let ((w (get-buffer-window gikopoi-message-buffer)))
    (with-selected-window w
      (goto-char (point-max))
      (recenter -1))))

(defun gikopoi--update-scroll-status ()
  (when (and gikopoi-message-buffer
             (eq (current-buffer) gikopoi-message-buffer)
             (get-buffer-window gikopoi-message-buffer))
    (setq gikopoi--user-at-bottom-p (gikopoi--near-bottom-p))))

(add-hook 'post-command-hook #'gikopoi--update-scroll-status)

(defvar gikopoi--user-at-bottom-p t)

(defun gikopoi-init-message-buffer (server &rest _)
  (setq gikopoi-message-buffer (get-buffer-create "*Gikopoi*"))
  (with-current-buffer gikopoi-message-buffer
    (gikopoi-mode)
    (gikopoi-msg-mode)
    (goto-address-mode)
    (visual-line-mode 1)
    (setq buffer-read-only t
          scroll-conservatively 101
          scroll-margin 0
          scroll-step 1))
  (display-buffer gikopoi-message-buffer)
  (when gikopoi--should-scroll-on-visit
    (gikopoi-message-buffer-scroll-to-end)
    (setq gikopoi--should-scroll-on-visit nil)))

(defun gikopoi-scroll-down () (interactive)
  (when-let ((w (get-buffer-window gikopoi-message-buffer)))
    (with-selected-window w
      (scroll-up-command)
      (when (<= (count-lines (window-end w t) (point-max)) (window-body-height w))
        (goto-char (point-max)) (recenter -1)))))

(defun gikopoi-scroll-up () (interactive)
  (when-let ((w (get-buffer-window gikopoi-message-buffer)))
    (with-selected-window w (scroll-down-command))))


;;; ── 16. User List Buffer ──────────────────────────────────────────────────

(defvar gikopoi-user-list-buffer nil)

(defun gikopoi-user-list-ignore-toggle ()
  (interactive)
  (when-let ((u (gikopoi-user-by-id (tabulated-list-get-id))))
    (setf (gikopoi-user-ignored-p u) (not (gikopoi-user-ignored-p u))))
  (tabulated-list-revert))

(defvar gikopoi-user-list-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "i") #'gikopoi-user-list-ignore-toggle)
    m))

(define-minor-mode gikopoi-user-list-mode
  "Minor mode active in the *Gikopoi Users* buffer."
  :group 'gikopoi :keymap gikopoi-user-list-mode-map)

(defun gikopoi--user-list-entries ()
  (mapcar (lambda (u)
            (list (gikopoi-user-id u)
                  (vector (gikopoi-user-name u)
                          (concat (if (gikopoi-user-active-p  u) "" "z")
                                  (if (gikopoi-user-ignored-p u) "I" ""))
                          (or (gikopoi-user-last-message u) ""))))
          (gikopoi-room-users gikopoi-current-room)))

(defun gikopoi--refresh-user-list-buffer ()
  (when (buffer-live-p gikopoi-user-list-buffer)
    (with-current-buffer gikopoi-user-list-buffer
      (tabulated-list-revert))))

(defun gikopoi-init-user-list-buffer ()
  (setq gikopoi-user-list-buffer (get-buffer-create "*Gikopoi Users*"))
  (with-current-buffer gikopoi-user-list-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format
          [("Name" 20 t) ("St" 2 nil) ("Last Message" 0 nil)])
    (tabulated-list-init-header)
    (add-hook 'tabulated-list-revert-hook
              (lambda () (setq tabulated-list-entries (gikopoi--user-list-entries)))
              nil t)
    (gikopoi-user-list-mode)))

(defun gikopoi-list-users ()
  "Show the user list buffer."
  (interactive)
  (unless (buffer-live-p gikopoi-user-list-buffer)
    (gikopoi-init-user-list-buffer))
  (with-current-buffer gikopoi-user-list-buffer (tabulated-list-revert))
  (unless (get-buffer-window gikopoi-user-list-buffer)
    (display-buffer gikopoi-user-list-buffer)))


;;; ── 17. Modes & Keybindings ───────────────────────────────────────────────

(defvar gikopoi-unread-count 0)
(defvar gikopoi-notif-names  nil)

(defun gikopoi-clear-mentions ()
  (interactive)
  (setq gikopoi-unread-count 0 gikopoi-notif-names nil)
  (when (called-interactively-p 'interactive) (force-mode-line-update)))

(defun gikopoi-notif-string ()
  (if (cl-plusp gikopoi-unread-count)
      (cl-labels ((unique-prefix (i s)
                    (if (>= i (length s))
                        s
                      (let* ((p   (substring s 0 i))
                             (cmp (try-completion p gikopoi-notif-names)))
                        (cond ((member cmp gikopoi-notif-names) p)
                              ((equal p cmp)                   (unique-prefix (1+ i) s))
                              ((< (length p) (length cmp))     (unique-prefix (length cmp) s))
                              (t                               s))))))
        (format " (%d)%s"
                gikopoi-unread-count
                (if gikopoi-notif-names
                    (concat "," (mapconcat (lambda (x) (unique-prefix 1 x))
                                          gikopoi-notif-names ","))
                  "")))
    ""))

(define-minor-mode gikopoi-notif-mode
  "Display unread Gikopoi count in the mode line."
  :group 'gikopoi :global t)

(define-derived-mode gikopoi-mode fundamental-mode "Gikopoi"
  "Major mode for the Gikopoi chat buffer."
  :group 'gikopoi)

(let ((m gikopoi-mode-map))
  (define-key m (kbd "SPC")       #'gikopoi-open-minibuffer)
  (define-key m (kbd "RET")       #'gikopoi-send-blank)
  (define-key m (kbd "r")         #'gikopoi-rula)
  (define-key m (kbd "R")         #'gikopoi-list-users)
  (define-key m (kbd "B")         #'gikopoi-join-busiest-room)
  (define-key m (kbd "i")         #'gikopoi-ignore)
  (define-key m (kbd "x")         #'gikopoi-block)
  (define-key m (kbd "L")         #'gikopoi-list-users)
  (define-key m (kbd "c")         #'gikopoi-clear-mentions)
  (define-key m (kbd "Q")         #'gikopoi-quit)
  (define-key m (kbd "<left>")    #'gikopoi-move-left)
  (define-key m (kbd "<right>")   #'gikopoi-move-right)
  (define-key m (kbd "<up>")      #'gikopoi-move-up)
  (define-key m (kbd "<down>")    #'gikopoi-move-down)
  (define-key m (kbd "<C-left>")  #'gikopoi-bubble-left)
  (define-key m (kbd "<C-right>") #'gikopoi-bubble-right)
  (define-key m (kbd "<C-up>")    #'gikopoi-bubble-up)
  (define-key m (kbd "<C-down>")  #'gikopoi-bubble-down)
  (define-key m (kbd "<next>")    #'gikopoi-scroll-down)
  (define-key m (kbd "C-v")       #'gikopoi-scroll-down)
  (define-key m (kbd "<prior>")   #'gikopoi-scroll-up)
  (define-key m (kbd "M-v")       #'gikopoi-scroll-up))

(defvar gikopoi-msg-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c q") #'gikopoi-autoquote)
    m))

(define-minor-mode gikopoi-msg-mode
  "Minor mode active in the *Gikopoi* message buffer."
  :group 'gikopoi :keymap gikopoi-msg-mode-map)

(add-to-list 'minor-mode-alist
             '(gikopoi-msg-mode
               (:eval (format ": %s@%s%s"
                              (and gikopoi-current-room
                                   (gikopoi-room-id gikopoi-current-room))
                              gikopoi-current-server
                              gikopoi--stats-mode-line))))

(add-to-list 'minor-mode-alist
             '(gikopoi-notif-mode (:eval (gikopoi-notif-string))))


;;; ── 18. Interactive Commands ──────────────────────────────────────────────

(defun gikopoi-move-left  (n) (interactive "p") (dotimes (_ n) (gikopoi-move "left")))
(defun gikopoi-move-right (n) (interactive "p") (dotimes (_ n) (gikopoi-move "right")))
(defun gikopoi-move-up    (n) (interactive "p") (dotimes (_ n) (gikopoi-move "up")))
(defun gikopoi-move-down  (n) (interactive "p") (dotimes (_ n) (gikopoi-move "down")))

(defun gikopoi-bubble-left  () (interactive) (gikopoi-bubble-position "left"))
(defun gikopoi-bubble-right () (interactive) (gikopoi-bubble-position "right"))
(defun gikopoi-bubble-up    () (interactive) (gikopoi-bubble-position "up"))
(defun gikopoi-bubble-down  () (interactive) (gikopoi-bubble-position "down"))

(defun gikopoi-send-blank () (interactive) (gikopoi-send ""))

(defvar gikopoi-minibuffer-map
  (let ((m (copy-keymap minibuffer-local-map)))
    (define-key m (kbd "TAB") #'gikopoi-minibuffer-complete)
    (define-key m (kbd "RET") #'exit-minibuffer)
    m))

(defun gikopoi-minibuffer-complete ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (completion-in-region (car bounds) (cdr bounds) (gikopoi-user-names))))

(defun gikopoi-autoquote ()
  "Pre-fill the message prompt with a quote of the text at point."
  (interactive)
  (let ((q (buffer-substring (point) (line-end-position))))
    (if (active-minibuffer-window)
        (progn (select-window (active-minibuffer-window))
               (erase-buffer)
               (insert (format gikopoi-autoquote-format q)))
      (minibuffer-with-setup-hook
          (lambda () (insert (format gikopoi-autoquote-format q)))
        (call-interactively #'gikopoi-send-message)))))

(defun gikopoi-rula (&optional room-id)
  "Teleport to ROOM-ID, or prompt with the room list.
Called with a string (e.g. via #rula) warps directly to that room ID."
  (interactive
   (list
    (progn
      (when (null gikopoi-room-list-data)
        (gikopoi-room-list-request))
      (let* ((candidates
              (mapcar (lambda (e)
                        (format "%s [%s]" (car e) (aref (cadr e) 2)))
                      gikopoi-room-list-data))
             (choice (completing-read "rula: " candidates nil nil)))
        (replace-regexp-in-string " \\[.*\\]$" "" choice)))))
  (when (and room-id (not (string-empty-p room-id)))
    (gikopoi-change-room room-id)))

(defun gikopoi-send-message ()
  "Prompt for a message.
  #rula [room]  – change room (opens room list if no room given)
  #list         – show user list
  empty input   – clears your speech bubble"
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (case-fold-search nil)
        (msg (read-from-minibuffer "" nil gikopoi-minibuffer-map)))
    (cond
     ((string-empty-p msg)
      (gikopoi-send-blank))
     ((string-match "^#rula *" msg)
      (let ((room (string-trim (substring msg (match-end 0)))))
        (if (string-empty-p room)
            (gikopoi-list-rooms)
          (gikopoi-rula room))))
     ((string-match "^#list" msg)
      (gikopoi-list-users))
     (t
      (gikopoi-send msg)
      (when gikopoi-auto-clear-bubble
        (gikopoi-send-blank))
      (when-let ((w (get-buffer-window "*Gikopoi*")))
        (select-window w))))))

(defun gikopoi-open-minibuffer ()
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (call-interactively #'gikopoi-send-message)))

(defun gikopoi-ignore (name)
  "Toggle client-side ignore for NAME.  Their messages are hidden locally only."
  (interactive (list (completing-read "Ignore/unignore user: " (gikopoi-user-names))))
  (when-let ((u (gikopoi-user-by-name name)))
    (setf (gikopoi-user-ignored-p u) (not (gikopoi-user-ignored-p u)))
    (when (called-interactively-p 'interactive)
      (message "Gikopoi: %s %s (local only)"
               (gikopoi-user-name u)
               (if (gikopoi-user-ignored-p u) "ignored" "un-ignored")))))

(defun gikopoi-block (name)
  "Block NAME server-side.  The server removes mutual visibility immediately.
This is stronger than ignore: neither you nor the blocked user can see each other.
There is no unblock in this session — reconnect to reset."
  (interactive (list (completing-read "Block user: " (gikopoi-user-names))))
  (when-let ((u (gikopoi-user-by-name name)))
    (let ((uid (gikopoi-user-id u)))
      (setf (gikopoi-user-ignored-p u) t)
      (gikopoi-socket-emit "user-block" uid)
      (message "Gikopoi: blocked %s (%s)" (gikopoi-user-name u) uid))))


;;; ── 19. Auto-ignore ───────────────────────────────────────────────────────

(defvar gikopoi-auto-ignore-file
  (expand-file-name "auto-ignore.txt" gikopoi-default-directory))

(defvar gikopoi-auto-ignore-names nil)

(defun gikopoi--load-auto-ignore ()
  (when (file-exists-p gikopoi-auto-ignore-file)
    (with-temp-buffer
      (insert-file-contents gikopoi-auto-ignore-file)
      (split-string (string-trim (buffer-string)) "\n" t))))

(defun gikopoi--save-auto-ignore (names)
  (with-temp-file gikopoi-auto-ignore-file
    (dolist (n (delete-dups names)) (insert n "\n"))))

(defun gikopoi-auto-ignore (name)
  "Toggle NAME in the persistent auto-ignore list."
  (interactive "sAuto-ignore username: ")
  (unless (file-exists-p gikopoi-auto-ignore-file)
    (with-temp-file gikopoi-auto-ignore-file))
  (let* ((names   (gikopoi--load-auto-ignore))
         (already (member name names))
         (u       (gikopoi-user-by-name name)))
    (if already
        (progn
          (setq names (delete name names))
          (when u (setf (gikopoi-user-ignored-p u) nil))
          (message "%s removed from auto-ignore" name))
      (push name names)
      (when u (setf (gikopoi-user-ignored-p u) t))
      (message "%s added to auto-ignore" name))
    (gikopoi--save-auto-ignore names)))

(defun gikopoi-load-auto-ignored-users ()
  "Apply the auto-ignore list to the current room."
  (interactive)
  (setq gikopoi-auto-ignore-names (gikopoi--load-auto-ignore))
  (when gikopoi-current-room
    (dolist (u (gikopoi-room-users gikopoi-current-room))
      (when (member (gikopoi-user-raw-name u) gikopoi-auto-ignore-names)
        (setf (gikopoi-user-ignored-p u) t))))
  (message "Loaded %d auto-ignored users" (length gikopoi-auto-ignore-names)))

(defun gikopoi-init-auto-ignore (&rest _)
  "Defer auto-ignore loading until the room is ready."
  (cond ((and gikopoi-current-room (gikopoi-room-users gikopoi-current-room))
         (gikopoi-load-auto-ignored-users))
        ((and gikopoi-socket (websocket-openp gikopoi-socket))
         (run-at-time 0.5 nil #'gikopoi-init-auto-ignore))))


;;; ── 20. Timestamps ────────────────────────────────────────────────────────

(defvar gikopoi-timestamp-timer nil)

(defun gikopoi-print-timestamp (&rest _)
  (gikopoi-with-message-buffer
    (insert (format-time-string gikopoi-time-format))))

(defun gikopoi--cancel-timestamp-timer ()
  (when (timerp gikopoi-timestamp-timer)
    (cancel-timer gikopoi-timestamp-timer)
    (setq gikopoi-timestamp-timer nil)))

(defun gikopoi-start-timestamps (&rest _)
  (unless (null gikopoi-timestamp-interval)
    (setq gikopoi-timestamp-timer
          (run-at-time t gikopoi-timestamp-interval #'gikopoi-print-timestamp))
    (cl-pushnew #'gikopoi--cancel-timestamp-timer gikopoi-quit-functions)))


;;; ── 21. Connection Management ─────────────────────────────────────────────

(defvar gikopoi-quit-functions
  (list #'gikopoi-socket-close
        (lambda () (gikopoi-notif-mode -1))
        (lambda () (setq gikopoi--stats-mode-line "") (force-mode-line-update)))
  "Hook run when disconnecting.  Each function is called with no arguments.")

(defun gikopoi-quit ()
  "Disconnect from Gikopoipoi (with confirmation)."
  (interactive)
  (when (y-or-n-p "Disconnect from Gikopoipoi? ")
    (setq gikopoi--deliberately-quit t)
    (when (timerp gikopoi--reconnect-timer) (cancel-timer gikopoi--reconnect-timer))
    (run-hooks 'gikopoi-quit-functions)))

(defun gikopoi-quit-silent ()
  "Disconnect silently (no confirmation; used internally by reconnect)."
  (setq gikopoi--deliberately-quit t)
  (when (timerp gikopoi--reconnect-timer) (cancel-timer gikopoi--reconnect-timer))
  (run-hooks 'gikopoi-quit-functions))

(defun gikopoi-reconnect ()
  "Reconnect using the same credentials as the last `gikopoi' call."
  (when gikopoi--last-args
    (message "Gikopoi: reconnecting…")
    (when gikopoi-current-room
      (setf (nth 3 gikopoi--last-args) (gikopoi-room-id gikopoi-current-room)))
    (ignore-errors (gikopoi-quit-silent))
    (apply #'gikopoi gikopoi--last-args)))

(defvar gikopoi-reconnect-timer nil)

(defun gikopoi-start-reconnect-timer ()
  "Start a periodic timer that reconnects every `gikopoi-reconnect-timer-minutes' minutes."
  (interactive)
  (let ((secs (* gikopoi-reconnect-timer-minutes 60)))
    (when gikopoi-reconnect-timer (cancel-timer gikopoi-reconnect-timer))
    (setq gikopoi-reconnect-timer (run-at-time secs secs #'gikopoi-reconnect))
    (message "Gikopoi: reconnect timer set for every %d min" gikopoi-reconnect-timer-minutes)))

(defun gikopoi-stop-reconnect-timer ()
  (interactive)
  (when gikopoi-reconnect-timer
    (cancel-timer gikopoi-reconnect-timer)
    (setq gikopoi-reconnect-timer nil)
    (message "Gikopoi: reconnect timer stopped")))

(defun gikopoi-maybe-start-reconnect-timer (&rest _)
  (when gikopoi-auto-start-reconnect-timer (gikopoi-start-reconnect-timer)))

(defun gikopoi-connect (server port area room name character password)
  "Establish an HTTP login and open the WebSocket to SERVER."
  (setq gikopoi--deliberately-quit nil
        gikopoi-room-list-data     nil)
  (when (timerp gikopoi--reconnect-timer) (cancel-timer gikopoi--reconnect-timer))
  (when (and gikopoi-socket (websocket-openp gikopoi-socket))
    (gikopoi-socket-close))
  (let* ((version (gikopoi-api-version server))
         (login   (gikopoi-api-login server area room name character password)))
    (when (or (null login) (eq (alist-get 'isLoginSuccessful login) json-false))
      (error "Gikopoi: login failed: %s" (or (alist-get 'error login) "unknown")))
    (let-alist login
      ;; Send session info to server log
      (ignore-errors
        (gikopoi-api-log server
          (format "%s %s EXPECTED_VERSION:%s ACTUAL:%s"
                  (format-time-string "%a %b %d %Y %T GMT%z") .userId
                  version .appVersion)))
      (setq gikopoi-current-server          server
            gikopoi-current-user-id         .userId
            gikopoi-current-private-user-id .privateUserId)
      (gikopoi-socket-open server port .privateUserId))))


;;; ── 22. Entry Point ───────────────────────────────────────────────────────

(defvar gikopoi-init-functions
  (list #'gikopoi-init-site-directory
        #'gikopoi-init-lang-alist
        #'gikopoi-connect
        #'gikopoi-init-message-buffer
        #'gikopoi-init-auto-ignore
        #'gikopoi-print-timestamp
        #'gikopoi-start-timestamps
        #'gikopoi-maybe-start-reconnect-timer
        (lambda (&rest _) (gikopoi-notif-mode 1)))
  "Functions called in sequence when connecting.
Each receives (server port area room name character password).")

(defun gikopoi-read-arglist ()
  "Interactively collect connection parameters."
  (let* ((server
          (let ((in (completing-read
                     (format "Server (default %s): " gikopoi-default-server)
                     (mapcar #'car gikopoi-servers))))
            (if (string-empty-p in) gikopoi-default-server in)))
         (port
          (if gikopoi-prompt-port-p
              (let ((in (read-string (format "Port (default %d): " gikopoi-default-port))))
                (if (string-empty-p in) gikopoi-default-port (string-to-number in)))
            gikopoi-default-port))
         (area
          (let ((in (completing-read
                     (format "Area (default %s): " gikopoi-default-area)
                     (cdr (assoc server gikopoi-servers)))))
            (if (string-empty-p in) gikopoi-default-area in)))
         (room
          (let ((in (read-string
                     (format "Room (default %s): " (or gikopoi-default-room "bar")))))
            (if (string-empty-p in) (or gikopoi-default-room "bar") in)))
         (name
          (let* ((disp (if (and gikopoi-default-name
                                (string-match "#" gikopoi-default-name))
                           (replace-regexp-in-string "#.*" "#*****" gikopoi-default-name)
                         gikopoi-default-name))
                 (in   (read-string (format "Name (default %s): " disp))))
            (if (string-empty-p in) gikopoi-default-name in)))
         (character
          (let ((in (read-string
                     (format "Character (default %s): " gikopoi-default-character))))
            (if (string-empty-p in) gikopoi-default-character in)))
         (password
          (if gikopoi-prompt-password-p
              (let ((in (read-passwd "Password: ")))
                (if (string-empty-p in) gikopoi-default-password in))
            gikopoi-default-password)))
    (list server port area room name character password)))

;;;###autoload
(defun gikopoi (server port area room name character password)
  "Connect to a Gikopoipoi server.
Interactively prompts for all parameters (defaults from defcustom)."
  (interactive (gikopoi-read-arglist))
  (setq gikopoi--last-args (list server port area room name character password))
  (run-hooks 'gikopoi-quit-functions)
  (run-hook-with-args 'gikopoi-init-functions
                      server port area room name character password))

;;; ── 23. Debug / Diagnostics ───────────────────────────────────────────────

(defvar gikopoi--debug-room-list-sent-at nil
  "Timestamp of the most recent `user-room-list' request.")

(defun gikopoi-debug-ping ()
  "Test HTTP round-trip latency to the current server's /api/version endpoint.
Reports elapsed time in milliseconds in the echo area and *Messages*."
  (interactive)
  (let ((server (or gikopoi-current-server
                    (read-string "Server: " gikopoi-default-server))))
    (message "Gikopoi-debug: pinging %s …" server)
    (let* ((t0  (current-time))
           (_   (with-temp-buffer
                  (condition-case err
                      (url-insert-file-contents (format "https://%s/api/version" server))
                    (error (message "Gikopoi-debug: ping error: %s" (error-message-string err))))))
           (ms  (round (* 1000 (float-time (time-subtract (current-time) t0))))))
      (message "Gikopoi-debug: /api/version round-trip = %d ms" ms))))

(defun gikopoi-debug-room-list-timing ()
  "Send a user-room-list request and report how long the server takes to respond.
One-shot: the next server-room-list reply is timed and reported."
  (interactive)
  (setq gikopoi--debug-room-list-sent-at (current-time))
  (let ((orig (get 'server-room-list 'gikopoi-event-handler)))
    (put 'server-room-list 'gikopoi-event-handler
         (lambda (&rest args)
           (let ((ms (round (* 1000 (float-time
                                     (time-subtract (current-time)
                                                    gikopoi--debug-room-list-sent-at))))))
             (message "Gikopoi-debug: server-room-list arrived in %d ms" ms))
           (put 'server-room-list 'gikopoi-event-handler orig) ; restore
           (apply orig args))))
  (message "Gikopoi-debug: sent user-room-list, waiting for reply…")
  (gikopoi-room-list-request))

(defun gikopoi-debug-status ()
  "Print a short summary of current connection state to *Messages*."
  (interactive)
  (message
   "Gikopoi-debug: server=%s  room=%s  socket=%s  room-list-data=%s  room-list-data-count=%d"
   (or gikopoi-current-server "nil")
   (and gikopoi-current-room (gikopoi-room-id gikopoi-current-room))
   (cond ((null gikopoi-socket) "nil")
         ((websocket-openp gikopoi-socket) "open")
         (t "closed"))
   (if gikopoi-room-list-data "loaded" "nil")
   (length gikopoi-room-list-data)))

(defun gikopoi-debug-room-list ()
  "Send user-room-list and dump the raw server response to *Gikopoi Room List Debug*."
  (interactive)
  (unless (and gikopoi-socket (websocket-openp gikopoi-socket))
    (user-error "Gikopoi: not connected"))
  (let ((buf (get-buffer-create "*Gikopoi Room List Debug*"))
        (orig (get 'server-room-list 'gikopoi-event-handler)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Room-list debug — %s\n" (format-time-string "%F %T")))
        (insert (format "Cached entries before request: %d\n\n" (length gikopoi-room-list-data)))
        (insert "Waiting for server-room-list response…\n")))
    (pop-to-buffer buf)
    (put 'server-room-list 'gikopoi-event-handler
         (lambda (rooms)
           (put 'server-room-list 'gikopoi-event-handler orig)
           (with-current-buffer (get-buffer-create "*Gikopoi Room List Debug*")
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               ;; ── raw arrival ──────────────────────────────────────────────
               (insert (format "\n=== server-room-list received ===\n"))
               (insert (format "Type:  %s\n"
                               (cond ((null rooms)   "nil")
                                     ((vectorp rooms) "vector")
                                     ((listp rooms)   "list")
                                     (t (format "%S" (type-of rooms))))))
               (insert (format "Count: %s\n"
                               (cond ((null rooms)   0)
                                     ((vectorp rooms) (length rooms))
                                     ((listp rooms)   (length rooms))
                                     (t "N/A"))))
               ;; ── first entry ──────────────────────────────────────────────
               (let ((first (cond ((and (vectorp rooms) (> (length rooms) 0)) (aref rooms 0))
                                  ((consp rooms) (car rooms)))))
                 (if first
                     (progn
                       (insert "\nFirst room entry (raw):\n")
                       (insert (format "  type: %s\n"
                                       (if (listp first) "alist" (format "%S" (type-of first)))))
                       (when (listp first)
                         (insert (format "  keys: %s\n"
                                         (mapcar #'car first))))
                       (insert (format "  data: %S\n" first)))
                   (insert "\nNo room entries in payload.\n")))
               ;; ── per-room parse trial ─────────────────────────────────────
               (insert "\n=== Parse trial ===\n")
               (let ((ok 0) (skipped 0) (errors nil))
                 (when (or (vectorp rooms) (listp rooms))
                   (seq-doseq (room rooms)
                     (condition-case err
                         (let-alist room
                           (if .id
                               (cl-incf ok)
                             (cl-incf skipped)))
                       (error
                        (push (error-message-string err) errors)))))
                 (insert (format "  ok:      %d\n" ok))
                 (insert (format "  no .id:  %d\n" skipped))
                 (insert (format "  errors:  %d\n" (length errors)))
                 (dolist (e (cl-remove-duplicates errors :test #'string=))
                   (insert (format "    - %s\n" e))))
               ;; ── run real handler ─────────────────────────────────────────
               (insert "\n=== Running normal handler ===\n")
               (condition-case err
                   (progn (apply orig (list rooms))
                          (insert (format "Done. gikopoi-room-list-data now has %d entries.\n"
                                          (length gikopoi-room-list-data))))
                 (error (insert (format "Handler error: %s\n" (error-message-string err)))))))))
    (gikopoi-room-list-request)))


(provide 'gikopoipoi)
;;; gikopoipoi.el ends here
