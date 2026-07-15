;;; gikopoipoi.el --- Gikopoipoi.net chat client -*- lexical-binding: t; coding: utf-8 -*-

;; Based on gikomacs by gyudon_addict◆hawaiiZtQ6
;; https://github.com/andrewmetzner/poipoidler
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

(defcustom gikopoi-occupied-room-color "green"
  "Face colour for occupied rooms in the `gikopoi-rula' completion list."
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

(defcustom gikopoi-auto-reconnect t
  "If non-nil, reconnect automatically when the server drops the connection."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-reconnect-max-delay 300
  "Maximum seconds between auto-reconnect attempts (doubles each failure)."
  :group 'gikopoi :type 'natnum)

(defcustom gikopoi-reconnect-max-attempts 10
  "Give up auto-reconnecting after this many consecutive failures.
Set to 0 to retry forever."
  :group 'gikopoi :type 'natnum)

(defcustom gikopoi-auto-start-reconnect-timer nil
  "If non-nil, start the periodic reconnect timer on connect."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-reconnect-timer-minutes 720
  "Period in minutes for the optional periodic reconnect timer."
  :group 'gikopoi :type 'natnum)

(defcustom gikopoi-restore-position-on-reconnect t
  "If non-nil, walk back to your last tile after a full re-login.
Most reconnects are a lightweight resume (see `gikopoi-reconnect') that
the server answers with your exact previous tile, so no walking is
needed. This only matters for the fallback case — a full HTTP re-login,
which always spawns at the room's default entry tile — where the tile
you were standing on is remembered and the client auto-walks you back
there once the room reloads, along a shortest path around walls."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-walk-step-interval 0.35
  "Seconds between successive moves while auto-walking back to a saved tile."
  :group 'gikopoi :type 'number)

(defcustom gikopoi-auto-clear-bubble nil
  "If non-nil, automatically send a blank message to clear your speech bubble after chatting."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-anon-numbers t
  "If non-nil, label anonymous (unnamed) users as \"Anonymous#N\".
The number is derived from the user id (see `gikopoi--anon-number');
toggle at runtime with `gikopoi-toggle-anon-numbers'."
  :group 'gikopoi :type 'boolean)

;;; ── 3. Global State ───────────────────────────────────────────────────────

(defun gikopoi--anon-number (id)
  "Derive a 0–4095 number from the last 3 hex chars of user ID.
Matches the algorithm used by the poipoi browser extension."
  (if (and (stringp id) (>= (length id) 3))
      (string-to-number (substring id -3) 16)
    0))

;; All connection-level state lives in one `gikopoi-session' object (the
;; singleton `gikopoi--session') so the whole connection can be inspected and
;; reasoned about as a unit.  The socket, reconnect, and stats fields are
;; declared here too even though they belong to later sections, keeping the
;; session model in one place.
(cl-defstruct (gikopoi-session (:constructor gikopoi--make-session)
                               (:conc-name gikopoi-ss-))
  "State of the active connection to a Gikopoi server.
Identity / room:
  SERVER            hostname of the active connection
  USER-ID           public user id assigned by the server
  PRIVATE-USER-ID   private user id (sent in the WS header)
  USER              the local `gikopoi-user' object
  ROOM              the active `gikopoi-room' object
  ROOM-LOADING-P    non-nil while a room transition is in progress
  ROOMS             all `gikopoi-room' objects seen this session
Server stats (from server-stats):
  USER-COUNT STREAM-COUNT STATS-MODE-LINE
WebSocket / Socket.IO:
  SOCKET SOCKET-INTERVAL SOCKET-TOLERANCE SOCKET-PING-TIMER SOCKET-TIMEOUT
  RECONNECTING-P
Reconnect machinery:
  DELIBERATELY-QUIT non-nil when the user asked to disconnect
  RECONNECT-TIMER   pending exponential-back-off reconnect timer
  RECONNECT-DELAY   current back-off delay in seconds
  RECONNECT-ATTEMPTS  consecutive failed reconnects since the last success
  LAST-ARGS         args from the last `gikopoi' call, reused on reconnect
  PERIODIC-RECONNECT-TIMER  the every-N-minutes reconnect timer
  SAVED-POSITION    (X . Y) tile to walk back to after a reconnect
  SAVED-ROOM        room id SAVED-POSITION belongs to"
  server user-id private-user-id user room room-loading-p rooms
  (user-count 0) (stream-count 0) (stats-mode-line "")
  socket socket-interval (socket-tolerance 1) socket-ping-timer socket-timeout
  reconnecting-p
  deliberately-quit reconnect-timer (reconnect-delay 5) (reconnect-attempts 0) last-args
  periodic-reconnect-timer
  saved-position saved-room)

(defvar gikopoi--session (gikopoi--make-session)
  "The singleton `gikopoi-session' holding all connection-level state.")


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

;; Socket and reconnect state live in `gikopoi--session' (see section 3).

(defun gikopoi--schedule-reconnect ()
  "Schedule the next reconnect attempt, backing off exponentially.
Gives up (leaving the connection dead until a manual `gikopoi-reconnect')
once `gikopoi-reconnect-max-attempts' consecutive failures have occurred."
  (when (timerp (gikopoi-ss-reconnect-timer gikopoi--session))
    (cancel-timer (gikopoi-ss-reconnect-timer gikopoi--session)))
  (cl-incf (gikopoi-ss-reconnect-attempts gikopoi--session))
  (if (and (> gikopoi-reconnect-max-attempts 0)
           (> (gikopoi-ss-reconnect-attempts gikopoi--session)
              gikopoi-reconnect-max-attempts))
      (when (buffer-live-p gikopoi-message-buffer)
        (gikopoi-with-message-buffer
          (insert (format "%s* giving up after %d failed reconnect attempts — use M-x gikopoi-reconnect to try again\n"
                          (format-time-string gikopoi-msg-time-format)
                          gikopoi-reconnect-max-attempts))))
    (let ((delay (gikopoi-ss-reconnect-delay gikopoi--session)))
      (when (buffer-live-p gikopoi-message-buffer)
        (gikopoi-with-message-buffer
          (insert (format "%s* disconnected — retrying in %ds (attempt %d%s)\n"
                          (format-time-string gikopoi-msg-time-format) delay
                          (gikopoi-ss-reconnect-attempts gikopoi--session)
                          (if (> gikopoi-reconnect-max-attempts 0)
                              (format "/%d" gikopoi-reconnect-max-attempts)
                            "")))))
      (setf (gikopoi-ss-reconnect-delay gikopoi--session)
            (min (* delay 2) gikopoi-reconnect-max-delay)
            (gikopoi-ss-reconnect-timer gikopoi--session)
            (run-at-time delay nil
                         (lambda ()
                           (condition-case err
                               (gikopoi-reconnect)
                             (error
                              (message "Gikopoi: reconnect error: %s" (error-message-string err))
                              (gikopoi--schedule-reconnect)))))))))

;;; WebSocket open / close

(defun gikopoi--ws-url (server port)
  (if (= port 443)
      (format "wss://%s/socket.io/?EIO=4&transport=websocket" server)
    (format "ws://%s:%d/socket.io/?EIO=4&transport=websocket" server port)))

(defun gikopoi-socket-open (server port pid)
  (setf (gikopoi-ss-socket gikopoi--session)
        (websocket-open
         (gikopoi--ws-url server port)
         :custom-header-alist `((private-user-id . ,pid)
                                (perMessageDeflate . false))
         :on-open    (lambda (_sock) (websocket-send-text (gikopoi-ss-socket gikopoi--session) "40"))
         :on-close   (lambda (_sock)
                       (when (timerp (gikopoi-ss-socket-ping-timer gikopoi--session))
                         (cancel-timer (gikopoi-ss-socket-ping-timer gikopoi--session)))
                       (when (and gikopoi-auto-reconnect
                                  (not (gikopoi-ss-deliberately-quit gikopoi--session)))
                         (gikopoi--schedule-reconnect)))
         :on-message #'gikopoi--ws-message-handler))
  (setf (websocket-client-data (gikopoi-ss-socket gikopoi--session)) (list server port pid))
  (gikopoi-ss-socket gikopoi--session))

(defun gikopoi-socket-close ()
  (when (and (gikopoi-ss-socket gikopoi--session) (websocket-openp (gikopoi-ss-socket gikopoi--session)))
    (websocket-close (gikopoi-ss-socket gikopoi--session)))
  (when (timerp (gikopoi-ss-socket-ping-timer gikopoi--session))
    (cancel-timer (gikopoi-ss-socket-ping-timer gikopoi--session))))

(defun gikopoi-socket-emit (event &rest args)
  "Send EVENT with ARGS as a socket.io packet to the server."
  (if (and (gikopoi-ss-socket gikopoi--session) (websocket-openp (gikopoi-ss-socket gikopoi--session)))
      (websocket-send-text
       (gikopoi-ss-socket gikopoi--session)
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
  (let* ((server (or (gikopoi-ss-server gikopoi--session) "gikopoipoi.net"))
         (area   (or (nth 2 (gikopoi-ss-last-args gikopoi--session)) gikopoi-default-area "for"))
         (pid    (gikopoi-ss-private-user-id gikopoi--session))
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

(defun gikopoi--room-list-fetch-synchronously ()
  "Fetch the room list via REST API and block until it's applied.
Unlike `gikopoi-room-list-request' (fire-and-forget, for the tabulated
room-list buffer's own revert cycle), this is for callers like
`gikopoi-rula' that need `gikopoi-room-list-data' to reflect the
current [num] counts *before* they build a candidate list — otherwise
the counts shown are whatever was cached from the last fetch, which
goes stale after people move between rooms or after a reconnect.
Returns `gikopoi-room-list-data', or the previous (stale) value with a
message if the request fails or times out, so `gikopoi-rula' still has
something to show while disconnected."
  (let* ((server (or (gikopoi-ss-server gikopoi--session) "gikopoipoi.net"))
         (area   (or (nth 2 (gikopoi-ss-last-args gikopoi--session)) gikopoi-default-area "for"))
         (pid    (gikopoi-ss-private-user-id gikopoi--session))
         (url    (format "https://%s/api/areas/%s/rooms" server area))
         (url-request-method "GET")
         (url-request-extra-headers
          (when pid `(("Authorization" . ,(format "Bearer %s" pid))))))
    (condition-case err
        (let ((buf (url-retrieve-synchronously url :silent :inhibit-cookies 5)))
          (unless buf (error "no response"))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n" nil t)
                (gikopoi--on-room-list (json-parse-buffer
                                        :array-type  'list
                                        :object-type 'alist)))
            (kill-buffer buf)))
      (error
       (message "Gikopoi: room list fetch failed (%s); showing last known counts"
                (error-message-string err)))))
  gikopoi-room-list-data)

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
               (setf (gikopoi-ss-socket-interval gikopoi--session) (/ .pingInterval 1000)
                     (gikopoi-ss-socket-timeout gikopoi--session)  (/ .pingTimeout  1000))))
            (2  ; ping – reset watchdog timer, send pong
             (when (timerp (gikopoi-ss-socket-ping-timer gikopoi--session))
               (cancel-timer (gikopoi-ss-socket-ping-timer gikopoi--session)))
             (websocket-send-text (gikopoi-ss-socket gikopoi--session) "3")
             (setf (gikopoi-ss-socket-ping-timer gikopoi--session)
                   (run-at-time (+ (gikopoi-ss-socket-interval gikopoi--session) (gikopoi-ss-socket-tolerance gikopoi--session))
                                nil
                                (lambda ()
                                  (unless (gikopoi-ss-deliberately-quit gikopoi--session)
                                    (gikopoi--schedule-reconnect))))))
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
  (when (equal (gikopoi-ss-user-id gikopoi--session) (slot-value u 'id))
    (setf (gikopoi-ss-user gikopoi--session) u)))

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
  (when (gikopoi-ss-room gikopoi--session)
    (dolist (u (gikopoi-room-users (gikopoi-ss-room gikopoi--session)))
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
  (cl-find id (gikopoi-room-users (gikopoi-ss-room gikopoi--session))
           :test #'equal :key #'gikopoi-user-id))

(defun gikopoi-user-by-name (name)
  (cl-find name (gikopoi-room-users (gikopoi-ss-room gikopoi--session))
           :test #'string= :key #'gikopoi-user-raw-name))

(defun gikopoi-user-names ()
  (mapcar #'gikopoi-user-name (gikopoi-room-users (gikopoi-ss-room gikopoi--session))))

;;; — Room ——————————————————————————————————————————————————————————————————

(defclass gikopoi-room ()
  ((id      :initarg :id      :accessor gikopoi-room-id)
   (group   :initarg :group)
   (assets  :initarg :assets  :accessor gikopoi-room-assets)
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
  (let ((existing (cl-find id (gikopoi-ss-rooms gikopoi--session) :test #'equal :key #'gikopoi-room-id)))
    (if existing
        (progn
          (shared-initialize existing (list :id id :group group :assets assets :users users))
          existing)
      (let ((r (make-instance 'gikopoi-room
                              :id id :group group :assets assets :users users)))
        (push r (gikopoi-ss-rooms gikopoi--session))
        r))))


;;; ── 12. Message Display ───────────────────────────────────────────────────

(defvar gikopoi-message-matched-p nil)

(cl-defmethod gikopoi-user-insert-message ((u gikopoi-user) text)
  (unless (gikopoi-user-ignored-p u)
    (let ((line (concat (format-time-string gikopoi-msg-time-format) (or text ""))))
      (if (eq u (gikopoi-ss-user gikopoi--session))
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
         (if (and gikopoi-message-matched-p (not (eq u (gikopoi-ss-user gikopoi--session))))
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
  (setf (gikopoi-ss-reconnect-attempts gikopoi--session) 0
        (gikopoi-ss-reconnect-delay gikopoi--session) 5)
  (let ((prev-id (and (gikopoi-ss-room gikopoi--session)
                      (gikopoi-room-id (gikopoi-ss-room gikopoi--session))))
        (new-id  (alist-get 'id currentRoom)))
    (setf (gikopoi-ss-room-loading-p gikopoi--session) t
          (gikopoi-ss-room gikopoi--session)
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
    (unless (gikopoi-ss-reconnecting-p gikopoi--session)
      (dolist (u (gikopoi-room-users (gikopoi-ss-room gikopoi--session)))
        (unless (gikopoi-user-ignored-p u)
          (when-let ((msg (gikopoi-user-last-message u)))
            (gikopoi-user-msg u msg t)))))
    (gikopoi--map-anim-reset)
    ;; Auto-open the map on first connect (deferred so the room settles first).
    (when (and gikopoi-map-auto-open (null prev-id)
               (not (gikopoi-ss-reconnecting-p gikopoi--session))
               (not (buffer-live-p (gikopoi-mv-buffer gikopoi--map))))
      (run-at-time 0.5 nil (lambda ()
                             (when (gikopoi-ss-room gikopoi--session) (gikopoi-show-map)))))
    ;; If we just reconnected, walk back to the tile we were on (one-shot).
    (when-let ((saved (gikopoi-ss-saved-position gikopoi--session)))
      (let ((same-room (equal (gikopoi-ss-saved-room gikopoi--session) new-id)))
        (setf (gikopoi-ss-saved-position gikopoi--session) nil
              (gikopoi-ss-saved-room gikopoi--session) nil)
        (when (and gikopoi-restore-position-on-reconnect same-room)
          (run-at-time 0.6 nil (lambda () (gikopoi--walk-to saved))))))
    (gikopoi--refresh-map-buffer)))

(gikopoi-defevent server-update-current-room-streams (streams)
  (setf (gikopoi-room-streams (gikopoi-ss-room gikopoi--session)) streams)
  (force-mode-line-update))

(gikopoi-defevent server-user-joined-room (user &optional from reconnectingp)
  (let ((u (gikopoi-make-user user)))
    (gikopoi-room-add-user (gikopoi-ss-room gikopoi--session) u)
    (gikopoi-user-join u from reconnectingp)
    (gikopoi--refresh-user-list-buffer)
    (gikopoi--refresh-map-buffer)))

(gikopoi-defevent server-user-left-room (id &optional destination)
  (when-let ((u (gikopoi-user-by-id id)))
    (gikopoi-user-leave u destination)
    (gikopoi-room-remove-user (gikopoi-ss-room gikopoi--session) u)
    (gikopoi--refresh-user-list-buffer)
    (gikopoi--refresh-map-buffer)))

;;; User events

(gikopoi-defevent server-user-active (id)
  (when-let ((u (gikopoi-user-by-id id)))
    (setf (gikopoi-user-active-p u) t)
    (gikopoi--refresh-user-list-buffer)
    (gikopoi--refresh-map-buffer)))

(gikopoi-defevent server-user-inactive (id)
  (when-let ((u (gikopoi-user-by-id id)))
    (setf (gikopoi-user-active-p u) nil)
    (gikopoi--refresh-user-list-buffer)
    (gikopoi--refresh-map-buffer)))

(gikopoi-defevent server-move ((userId x y direction lastMovement isInstant shouldSpinwalk))
  (when-let ((u (gikopoi-user-by-id userId)))
    (let ((old (gikopoi-user-position u)))
      (setf (gikopoi-user-position      u) (cons x y)
            (gikopoi-user-direction     u) direction
            (gikopoi-user-last-movement u) lastMovement)
      (when (and gikopoi-map-animate (not (eq isInstant t))
                 old (not (equal old (cons x y)))
                 (gikopoi--map-anim-visible-p))
        (gikopoi--map-start-walk userId old))
      (gikopoi--refresh-map-buffer)
      (gikopoi--refresh-user-list-buffer))))

(gikopoi-defevent server-bubble-position (id direction)
  (when-let ((u (gikopoi-user-by-id id)))
    (setf (gikopoi-user-bubble-position u) direction)
    (gikopoi--refresh-map-buffer)))

(gikopoi-defevent server-character-changed (id character-id altp)
  (when-let ((u (gikopoi-user-by-id id)))
    (setf (gikopoi-user-character-id u) character-id
          (gikopoi-user-alt-p        u) (eq altp t))))

;;; Message events

(gikopoi-defevent server-msg (id message)
  (when-let ((u (gikopoi-user-by-id id)))
    (gikopoi-user-msg u message)
    (gikopoi--refresh-user-list-buffer)
    (gikopoi--refresh-map-buffer)))      ; show/refresh the speech bubble at once

(gikopoi-defevent server-roleplay (id message)
  (when-let ((u (gikopoi-user-by-id id)))
    (gikopoi-user-roleplay u message)
    (gikopoi--refresh-map-buffer)))

(gikopoi-defevent server-roll-die (id base sum arga &optional argb)
  (when-let ((u (gikopoi-user-by-id id)))
    (gikopoi-user-roll-die u base sum (or argb arga))))

;;; Server info events

(gikopoi-defevent server-stats ((userCount streamCount))
  (let ((n (if (stringp userCount)   (string-to-number userCount)   (or userCount 0)))
        (s (if (stringp streamCount) (string-to-number streamCount) (or streamCount 0))))
    (setf (gikopoi-ss-user-count gikopoi--session)   n
          (gikopoi-ss-stream-count gikopoi--session) s
          (gikopoi-ss-stats-mode-line gikopoi--session)
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
(gikopoi-defevent server-cant-log-you-in ()
  ;; The server didn't recognize our private-user-id — this is the reply to
  ;; a lightweight socket-only resume (see `gikopoi-reconnect') when the
  ;; server no longer has a "ghost" for us (session >30min old, or the
  ;; server restarted). Fall back to a full HTTP re-login.
  (message "Gikopoi: session expired — doing a full re-login")
  (gikopoi--full-relogin))
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
  (let ((cur (and (gikopoi-ss-room gikopoi--session) (gikopoi-room-id (gikopoi-ss-room gikopoi--session))))
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
    (let* ((cur     (and (gikopoi-ss-room gikopoi--session) (gikopoi-room-id (gikopoi-ss-room gikopoi--session))))
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
                          (if-let ((p (gikopoi-user-position u)))
                              (format "%s,%s" (car p) (cdr p))
                            "")
                          (or (gikopoi-user-last-message u) ""))))
          (gikopoi-room-users (gikopoi-ss-room gikopoi--session))))

(defun gikopoi--refresh-user-list-buffer ()
  (when (buffer-live-p gikopoi-user-list-buffer)
    (with-current-buffer gikopoi-user-list-buffer
      (tabulated-list-revert))))

(defun gikopoi-init-user-list-buffer ()
  (setq gikopoi-user-list-buffer (get-buffer-create "*Gikopoi Users*"))
  (with-current-buffer gikopoi-user-list-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format
          [("Name" 20 t) ("St" 2 nil) ("XY" 7 nil) ("Last Message" 0 nil)])
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


;;; ── 16b. Room Map Buffer ──────────────────────────────────────────────────
;;;
;;; A top-down grid of the current room: static tiles come from the room
;;; `assets' (the raw `currentRoom' payload — size, blocked, sit, doors), and
;;; avatars are drawn from the live user positions.  Gikopoi's coordinate
;;; system puts the origin at the front-left with y increasing toward the back,
;;; so rows are rendered highest-y first (back of the room at the top).

(defcustom gikopoi-map-glyphs
  '((floor . "·") (blocked . "▓") (sit . "▒") (door . "+"))
  "Glyphs for the static tiles drawn in the room map."
  :group 'gikopoi
  :type '(alist :key-type symbol :value-type string))

(defconst gikopoi--map-dir-arrows
  '(("up" . "▲") ("down" . "▼") ("left" . "◀") ("right" . "▶"))
  "Avatar glyph per facing direction; `gikopoi--map-arrow' falls back to a dot.")

(defface gikopoi-map-floor   '((t :inherit shadow))
  "Face for empty floor tiles in the room map." :group 'gikopoi)
(defface gikopoi-map-blocked '((t :inherit shadow :weight bold))
  "Face for blocked/wall tiles in the room map." :group 'gikopoi)
(defface gikopoi-map-sit     '((t :inherit font-lock-comment-face))
  "Face for sittable tiles in the room map." :group 'gikopoi)
(defface gikopoi-map-door    '((t :inherit warning :weight bold))
  "Face for door/exit tiles in the room map." :group 'gikopoi)
(defface gikopoi-map-you     '((t :inherit highlight :weight bold))
  "Face for your own avatar in the room map." :group 'gikopoi)

(defcustom gikopoi-map-tile-colors
  '((door . "#b58900") (sit . "#3a5f7a"))
  "Background colours for special tiles, drawn as full-cell blocks.
The block is painted under any avatar standing on the tile, so a door or
warp stays visible when people are stacked on top of it.  Kinds without an
entry (e.g. `floor', `blocked') get no colour block."
  :group 'gikopoi
  :type '(alist :key-type symbol :value-type color))

(defun gikopoi--room-assets ()
  (and (gikopoi-ss-room gikopoi--session) (gikopoi-room-assets (gikopoi-ss-room gikopoi--session))))

(defun gikopoi--map-glyph (kind) (alist-get kind gikopoi-map-glyphs))

(defun gikopoi--map-arrow (dir)
  (or (cdr (assoc dir gikopoi--map-dir-arrows)) "●"))

(defun gikopoi--map-dimensions ()
  "Return (WIDTH . HEIGHT) of the current room in tiles, or nil."
  (when-let ((size (alist-get 'size (gikopoi--room-assets))))
    (cons (alist-get 'x size) (alist-get 'y size))))

(defun gikopoi--coords->set (vec)
  "Turn VEC, a sequence of {x,y} coord alists, into an `equal' hash of (X . Y)."
  (let ((h (make-hash-table :test 'equal)))
    (mapc (lambda (c)
            (puthash (cons (alist-get 'x c) (alist-get 'y c)) t h))
          (append vec nil))
    h))

(defun gikopoi--map-door-set ()
  "Hash (X . Y) → door id for every door in the current room."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (d (alist-get 'doors (gikopoi--room-assets)) h)
      (let ((c (cdr d)))
        (puthash (cons (alist-get 'x c) (alist-get 'y c)) (car d) h)))))

(defun gikopoi--map-user-set ()
  "Hash (X . Y) → list of users standing on that tile."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (u (gikopoi-room-users (gikopoi-ss-room gikopoi--session)) h)
      (when-let ((pos (gikopoi-user-position u)))
        (push u (gethash (cons (car pos) (cdr pos)) h))))))

(defun gikopoi--map-user-glyph (users)
  "Return the propertized avatar glyph for the USERS on a single tile."
  (let* ((me    (gikopoi--self-user))
         (you   (memq me users))
         (u     (if you me (car users)))
         (names (mapconcat #'gikopoi-user-raw-name users ", ")))
    (cond
     (you (propertize (gikopoi--map-arrow (gikopoi-user-direction u))
                      'face 'gikopoi-map-you
                      'help-echo (if (cdr users)
                                     (format "you (+%d here): %s" (1- (length users)) names)
                                   "you")))
     ((cdr users)                       ; more than one other user
      (propertize (if (< (length users) 10) (number-to-string (length users)) "+")
                  'face 'bold 'help-echo names))
     (t (propertize (gikopoi--map-arrow (gikopoi-user-direction u))
                    'face `(:foreground ,(gikopoi-user-name-color u))
                    'help-echo names)))))

(defun gikopoi--map-tile-glyph (kind door-id)
  "Return the propertized static glyph for a tile of KIND (`door', `sit', …)."
  (pcase kind
    ('door    (propertize (gikopoi--map-glyph 'door) 'face 'gikopoi-map-door
                          'help-echo (format "door: %s" door-id)))
    ('sit     (propertize (gikopoi--map-glyph 'sit) 'face 'gikopoi-map-sit))
    ('blocked (propertize (gikopoi--map-glyph 'blocked) 'face 'gikopoi-map-blocked))
    (_        (propertize (gikopoi--map-glyph 'floor) 'face 'gikopoi-map-floor))))

(defun gikopoi--map-cell (x y blocked sit doors users)
  "Return the 2-column propertized string for tile (X . Y).
Special tiles are painted as a full-cell colour block (see
`gikopoi-map-tile-colors'); an avatar is layered on top so the block — e.g.
a door or warp — stays visible even with people stacked on it."
  (let* ((key  (cons x y))
         (occ  (gethash key users))
         (kind (cond ((gethash key doors)   'door)
                     ((gethash key sit)     'sit)
                     ((gethash key blocked) 'blocked)
                     (t                     'floor)))
         (bg   (cdr (assq kind gikopoi-map-tile-colors)))
         (cell (concat (if occ
                           (gikopoi--map-user-glyph occ)
                         (gikopoi--map-tile-glyph kind (gethash key doors)))
                       " ")))
    (when bg
      ;; Paint the colour block so it wins over any background the avatar face
      ;; carries (e.g. `highlight' for you), while keeping the avatar's own
      ;; foreground colour — so a door/warp stays visible under stacked people.
      (add-face-text-property 0 (length cell) `(:background ,bg) nil cell))
    cell))

(defun gikopoi--map-swatch (glyph face kind)
  "Return a legend GLYPH with FACE, plus KIND's colour block (as on the map)."
  (let ((s (copy-sequence (propertize glyph 'face face)))
        (bg (cdr (assq kind gikopoi-map-tile-colors))))
    (when bg
      (add-face-text-property 0 (length s) `(:background ,bg) nil s))
    s))

(defun gikopoi--map-legend ()
  "Return the legend line describing the room-map glyphs."
  (concat
   "\n"
   (gikopoi--map-swatch (gikopoi--map-arrow "up") 'gikopoi-map-you nil) " you   "
   (gikopoi--map-swatch "●" 'font-lock-keyword-face nil)                " other   "
   (gikopoi--map-swatch "2" 'bold nil)                                  " stacked   "
   (gikopoi--map-swatch (gikopoi--map-glyph 'door) 'gikopoi-map-door 'door) " door   "
   (gikopoi--map-swatch (gikopoi--map-glyph 'sit)  'gikopoi-map-sit  'sit)  " sit   "
   (gikopoi--map-swatch (gikopoi--map-glyph 'blocked) 'gikopoi-map-blocked 'blocked)
   " wall\n"))

(defun gikopoi--render-map-ascii ()
  "Insert the ASCII top-down room map at point."
  (let ((dim (gikopoi--map-dimensions)))
    (if (not dim)
        (insert "No room map available.\n")
      (let* ((w       (car dim))
             (h       (cdr dim))
             (blocked (gikopoi--coords->set (alist-get 'blocked (gikopoi--room-assets))))
             (sit     (gikopoi--coords->set (alist-get 'sit     (gikopoi--room-assets))))
             (doors   (gikopoi--map-door-set))
             (users   (gikopoi--map-user-set)))
        (insert (propertize (format "%s  (%d×%d)\n"
                                     (gikopoi-room-id (gikopoi-ss-room gikopoi--session)) w h)
                            'face 'bold))
        (when-let* ((me (gikopoi--self-user))
                    (p  (gikopoi-user-position me)))
          (insert (format "you: (%s,%s) facing %s\n"
                          (car p) (cdr p)
                          (gikopoi-user-direction me))))
        (insert "\n")
        (cl-loop for y from (1- h) downto 0 do
                 (cl-loop for x from 0 below w do
                          (insert (gikopoi--map-cell x y blocked sit doors users)))
                 (insert "\n"))
        (insert (gikopoi--map-legend))))))

;;; ── 16b′. Auto-Walk (return to a saved tile) ──────────────────────────────
;;;
;;; A small reactive path-walker: given a target tile it emits one `user-move'
;;; per tick toward the target, recomputing a shortest path (BFS around walls)
;;; from the server-confirmed position each step, so it self-corrects and stops
;;; cleanly if the way is blocked.  Used to restore your position after a
;;; reconnect (see `gikopoi-restore-position-on-reconnect').

(defconst gikopoi--move-deltas
  '(("up" . (0 . 1)) ("down" . (0 . -1)) ("left" . (-1 . 0)) ("right" . (1 . 0)))
  "Grid delta (DX . DY) applied by each `user-move' DIRECTION.")

(defvar gikopoi--walk-target nil "Target (X . Y) the auto-walker is heading to, or nil.")
(defvar gikopoi--walk-timer nil "Repeating timer driving the auto-walker.")
(defvar gikopoi--walk-tries 0 "Moves emitted so far on the current walk (a safety bound).")
(defvar gikopoi--walk-moved nil "Non-nil once the current walk has emitted a move.")
(defvar gikopoi--walk-settle 0
  "Ticks left to wait for the server to settle our spawn before concluding arrival.
After a reconnect the server first echoes our old tile in the room state, then
relocates us to the entrance door a beat later.  We hold off declaring \"arrived\"
until either this counter runs out or our position actually changes.")
(defvar gikopoi-walk-max-steps 400
  "Hard cap on moves for one auto-walk, so a bad path can never loop forever.")

(defun gikopoi--self-user ()
  "Return our own `gikopoi-user' in the current room, resolved live by id.
Falls back to the session's cached USER.  Resolving by id each tick avoids
acting on a stale object left over from before a reconnect."
  (or (and (gikopoi-ss-user-id gikopoi--session)
           (gikopoi-user-by-id (gikopoi-ss-user-id gikopoi--session)))
      (gikopoi-ss-user gikopoi--session)))

(defun gikopoi--walk-next-dir (from to)
  "Return the DIRECTION string for the first step of a shortest FROM->TO path.
Paths run over in-bounds, non-blocked tiles of the current room.  Returns nil
if TO is unreachable or the room has no map."
  (when-let ((dim (gikopoi--map-dimensions)))
    (let* ((w       (car dim))
           (h       (cdr dim))
           (blocked (gikopoi--coords->set (alist-get 'blocked (gikopoi--room-assets))))
           (came    (make-hash-table :test 'equal))
           (queue   (list from)))
      (puthash from t came)
      (catch 'done
        (while queue
          (let ((cur (pop queue)))
            (when (equal cur to) (throw 'done nil))
            (dolist (d gikopoi--move-deltas)
              (let* ((delta (cdr d))
                     (nx    (+ (car cur) (car delta)))
                     (ny    (+ (cdr cur) (cdr delta)))
                     (np    (cons nx ny)))
                (unless (or (< nx 0) (< ny 0) (>= nx w) (>= ny h)
                            (gethash np blocked) (gethash np came))
                  (puthash np (cons cur (car d)) came)
                  (setq queue (nconc queue (list np)))))))))
      ;; Walk the parent chain back to FROM; the last direction is the first step.
      (let ((node to) (first-dir nil))
        (while (and (consp (gethash node came)) (not (equal node from)))
          (let ((pd (gethash node came)))
            (setq first-dir (cdr pd)
                  node      (car pd))))
        (and (equal node from) first-dir)))))

(defun gikopoi-walk-stop ()
  "Cancel any in-progress auto-walk."
  (interactive)
  (when (timerp gikopoi--walk-timer) (cancel-timer gikopoi--walk-timer))
  (setq gikopoi--walk-timer nil gikopoi--walk-target nil
        gikopoi--walk-tries 0 gikopoi--walk-moved nil gikopoi--walk-settle 0))

(defun gikopoi--walk-step ()
  "One tick of the auto-walker: step toward `gikopoi--walk-target' or stop."
  (let* ((me     (gikopoi--self-user))
         (pos    (and me (gikopoi-user-position me)))
         (target gikopoi--walk-target))
    (cond
     ((or (null target) (null pos)
          (not (and (gikopoi-ss-socket gikopoi--session)
                    (websocket-openp (gikopoi-ss-socket gikopoi--session)))))
      (gikopoi-walk-stop))
     ((equal pos target)
      ;; We appear to be there.  Right after a reconnect the server may still
      ;; relocate us to the door, so if we haven't moved yet wait out the settle
      ;; window before believing it.
      (if (and (not gikopoi--walk-moved) (> gikopoi--walk-settle 0))
          (setq gikopoi--walk-settle (1- gikopoi--walk-settle))
        (gikopoi-walk-stop)
        (message "Gikopoi: back at (%s,%s)" (car target) (cdr target))))
     ((> (setq gikopoi--walk-tries (1+ gikopoi--walk-tries)) gikopoi-walk-max-steps)
      (gikopoi-walk-stop)
      (message "Gikopoi: gave up walking back to (%s,%s)" (car target) (cdr target)))
     (t (let ((dir (gikopoi--walk-next-dir pos target)))
          (if dir
              (progn (setq gikopoi--walk-moved t)
                     (ignore-errors (gikopoi-move dir)))
            (gikopoi-walk-stop)
            (message "Gikopoi: no path back to (%s,%s)" (car target) (cdr target))))))))

(defun gikopoi--walk-to (target)
  "Begin auto-walking to TARGET, a (X . Y) tile in the current room."
  (gikopoi-walk-stop)
  (when (and target (gikopoi--self-user))
    (setq gikopoi--walk-target target
          gikopoi--walk-tries  0
          gikopoi--walk-moved  nil
          ;; ~1.5s of grace for the post-reconnect spawn to settle.
          gikopoi--walk-settle (max 1 (round (/ 1.5 (max 0.1 gikopoi-walk-step-interval))))
          gikopoi--walk-timer
          (run-at-time 0.1 (max 0.1 gikopoi-walk-step-interval) #'gikopoi--walk-step))))

;;; ── 16c. Graphical Map ────────────────────────────────────────────────────
;;;
;;; Composites the current room as one SVG — the gikopoi2 background, furniture
;;; objects and character sprites — using the same isometric projection as the
;;; JS client (`calculateRealCoordinates': origin + (x+y)*bw/2, (x-y)*bh/2),
;;; then shows it as an image.  Assets are downloaded on demand from the
;;; gikopoi2 repo and cached under `gikopoi-site-directory'.

(defcustom gikopoi-map-renderer 'auto
  "Which renderer the room map starts in.
  `auto'     – graphics on a GUI with SVG support, otherwise ASCII (default)
  `graphics' – always start in the composited-graphics map
  `ascii'    – always start in the ASCII grid map
Toggle at runtime with `gikopoi-map-toggle-graphics' (\\`t' in the map); this
setting only chooses the initial mode.  On a text terminal the map always
falls back to ASCII regardless of this setting."
  :group 'gikopoi
  :type '(choice (const :tag "Auto (graphics if supported)" auto)
                 (const :tag "Always graphics" graphics)
                 (const :tag "Always ASCII" ascii)))

(defcustom gikopoi-map-auto-open t
  "When non-nil, open the room map automatically on connecting to a server."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-map-asset-base-url
  "https://raw.githubusercontent.com/iccanobif/gikopoi2/master/public/"
  "Base URL for downloading room and character SVG assets."
  :group 'gikopoi :type 'string)

(defcustom gikopoi-map-default-zoom 1.0
  "Initial zoom level of the graphical map: higher shows less room, bigger.
The live zoom is the `zoom' slot of `gikopoi--map'; adjust it at runtime with
`gikopoi-map-zoom-in'/`-out' (\\`+' / \\`-' in the map)."
  :group 'gikopoi :type 'number)

(defcustom gikopoi-map-view-width 760
  "On-screen width, in pixels, of the graphical map window (the camera view).
The window keeps this size across rooms and zoom; big rooms scroll to follow
you rather than shrinking to fit."
  :group 'gikopoi :type 'integer)

(defcustom gikopoi-map-view-height 520
  "On-screen height, in pixels, of the graphical map window (the camera view)."
  :group 'gikopoi :type 'integer)

(defcustom gikopoi-map-follow t
  "When non-nil, the graphical-map camera centres on you in rooms bigger than
the view; otherwise it centres on the room."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-map-show-names t
  "When non-nil, draw each user's name above their sprite on the graphical map."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-map-show-bubbles t
  "When non-nil, draw speech bubbles for users' current messages on the map.
A bubble stays up as long as the user keeps that message active."
  :group 'gikopoi :type 'boolean)

(defconst gikopoi--map-block-w 80 "Default isometric block width (gikopoi2 BLOCK_WIDTH).")
(defconst gikopoi--map-block-h 40 "Default isometric block height (gikopoi2 BLOCK_HEIGHT).")
(defconst gikopoi--map-char-scale 0.5 "Character sprite scale (gikopoi2 default).")

(defcustom gikopoi-map-animate t
  "When non-nil, animate walking on the graphical map."
  :group 'gikopoi :type 'boolean)

(defcustom gikopoi-map-anim-fps 15
  "Frames per second for graphical-map walk animation."
  :group 'gikopoi :type 'number)

;; Asset caches are genuinely module-global (shared across every view and room),
;; so they stay as plain variables rather than moving into the map-view object.
(defvar gikopoi--asset-datauri-cache (make-hash-table :test 'equal)
  "Cache of asset relpath -> data: URI string.")
(defvar gikopoi--asset-dims-cache    (make-hash-table :test 'equal)
  "Cache of asset relpath -> (WIDTH . HEIGHT) in pixels.")

(cl-defstruct (gikopoi-map-view (:constructor gikopoi--make-map-view)
                                (:conc-name gikopoi-mv-))
  "Bundled state for the room-map view.
Groups what used to be scattered `gikopoi-map-*'/`gikopoi--map-*' globals into
one inspectable, resettable object — the singleton `gikopoi--map':
  BUFFER       the *Gikopoi Map* buffer, or nil
  GRAPHICAL    non-nil when drawing composited graphics rather than ASCII
  ZOOM         current zoom factor (1.0 = 100%)
  VSIZE        cached (W . H) px of the view, stable between real resizes
  FITTED-ROOM  room id the ASCII window was last fitted to
  ANIM         user-id -> walk-animation plist (:px :py :walking :phase)
  ANIM-TIMER   the running animation timer, or nil"
  buffer
  (graphical (pcase gikopoi-map-renderer
               ('graphics t) ('ascii nil)
               (_ (image-type-available-p 'svg))))
  (zoom gikopoi-map-default-zoom)
  vsize
  fitted-room
  (anim (make-hash-table :test 'equal))
  anim-timer)

(defvar gikopoi--map (gikopoi--make-map-view)
  "The singleton `gikopoi-map-view' holding all room-map view state.")

(defun gikopoi--map-camera (cw ch vw vh ccx ccy _rid)
  "Return camera origin (VX . VY) that simply follows the target (CCX . CCY).
Keeps you centred (clamped to the room), scrolling the room smoothly as you
walk — no dead-zone snapping, so it never jumps to re-centre after a move.
Rooms smaller than the view are centred and stay put."
  (cons (gikopoi--camera-clamp (- ccx (/ vw 2)) cw vw)
        (gikopoi--camera-clamp (- ccy (/ vh 2)) ch vh)))

(defun gikopoi--map-view-size ()
  "Return the target (WIDTH . HEIGHT) in pixels for the map image.
Fills the map window when it is displayed, so the room view is responsive;
falls back to `gikopoi-map-view-width'/`-height' before the window exists."
  (let ((win (and (buffer-live-p (gikopoi-mv-buffer gikopoi--map))
                  (get-buffer-window (gikopoi-mv-buffer gikopoi--map) t))))
    (if (and win (fboundp 'window-body-width))
        (cons (max 200 (- (window-body-width  win t) 6))
              (max 150 (- (window-body-height win t) 6)))
      (cons gikopoi-map-view-width gikopoi-map-view-height))))


(defun gikopoi--n (x) "Format number X for an SVG attribute." (format "%g" x))

(defun gikopoi--xml-escape (s)
  (replace-regexp-in-string
   "[<>&\"]" (lambda (m) (pcase m ("<" "&lt;") (">" "&gt;") ("&" "&amp;") ("\"" "&quot;")))
   (or s "")))

(defun gikopoi--asset-cache-dir ()
  (or gikopoi-site-directory
      (expand-file-name "assets" gikopoi-default-directory)))

(defun gikopoi--asset-file (relpath)
  "Return the local cached path of asset RELPATH, downloading it if missing.
Return nil when the asset cannot be fetched."
  (let ((local (expand-file-name relpath (gikopoi--asset-cache-dir))))
    (if (file-exists-p local)
        local
      (make-directory (file-name-directory local) t)
      (condition-case nil
          (progn
            (url-copy-file (concat gikopoi-map-asset-base-url relpath) local t)
            (if (and (file-exists-p local)
                     (> (file-attribute-size (file-attributes local)) 0))
                local
              (ignore-errors (delete-file local)) nil))
        (error (ignore-errors (delete-file local)) nil)))))

(defun gikopoi--asset-dims (file)
  "Return (WIDTH . HEIGHT) parsed from SVG FILE, or nil."
  (or (gethash file gikopoi--asset-dims-cache)
      (puthash
       file
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file nil 0 4000)
         (goto-char (point-min))
         (let (w h)
           (when (re-search-forward "<svg[^>]*>" nil t)
             (let ((tag (match-string 0)))
               (when (string-match "width=\"\\([0-9.]+\\)" tag)
                 (setq w (string-to-number (match-string 1 tag))))
               (when (string-match "height=\"\\([0-9.]+\\)" tag)
                 (setq h (string-to-number (match-string 1 tag))))
               (when (and (not (and w h))
                          (string-match
                           "viewBox=\"[-0-9.]+ +[-0-9.]+ +\\([0-9.]+\\) +\\([0-9.]+\\)" tag))
                 (setq w (or w (string-to-number (match-string 1 tag)))
                       h (or h (string-to-number (match-string 2 tag)))))))
           (and w h (cons w h))))
       gikopoi--asset-dims-cache)))

(defun gikopoi--asset-datauri (file)
  "Return a base64 data: URI embedding FILE (cached)."
  (or (gethash file gikopoi--asset-datauri-cache)
      (puthash
       file
       (let ((mime (if (string-suffix-p ".png" file) "image/png" "image/svg+xml")))
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert-file-contents-literally file)
           (concat "data:" mime ";base64,"
                   (base64-encode-string (buffer-string) t))))
       gikopoi--asset-datauri-cache)))

;;; --- Bulk asset download ----------------------------------------------------
;;;
;;; On-demand fetching (`gikopoi--asset-file') already grabs whatever a new
;;; room or character needs the first time it appears.  These commands let you
;;; pre-pull *all* static assets up front, or re-pull them when the upstream
;;; rooms/characters change.

(defcustom gikopoi-map-asset-manifest-url
  "https://api.github.com/repos/iccanobif/gikopoi2/git/trees/master?recursive=1"
  "GitHub tree API URL used to enumerate all downloadable static assets."
  :group 'gikopoi :type 'string)

(defconst gikopoi--dl-concurrency 8 "Parallel downloads for a bulk asset pull.")
(defvar gikopoi--dl-queue nil)
(defvar gikopoi--dl-total 0)
(defvar gikopoi--dl-done  0)
(defvar gikopoi--dl-active 0)

(defun gikopoi--asset-manifest ()
  "Return the list of asset relpaths (under public/) from the upstream repo."
  (with-current-buffer
      (url-retrieve-synchronously gikopoi-map-asset-manifest-url t t 30)
    (goto-char (point-min))
    (unless (re-search-forward "\n\n" nil t)
      (error "Gikopoi: bad response from asset manifest"))
    (let* ((data (json-parse-buffer :object-type 'alist :array-type 'list))
           (tree (alist-get 'tree data))
           (out  '()))
      (dolist (e tree (nreverse out))
        (let ((path (alist-get 'path e)))
          (when (and (stringp path)
                     (string-match-p "\\`public/\\(rooms\\|characters\\)/" path)
                     (string-match-p "\\.\\(svg\\|png\\)\\'" path))
            (push (substring path (length "public/")) out)))))))

(defun gikopoi--download-file-async (relpath callback)
  "Download RELPATH into the asset cache asynchronously, then call CALLBACK."
  (let ((local (expand-file-name relpath (gikopoi--asset-cache-dir)))
        (url   (concat gikopoi-map-asset-base-url relpath)))
    (make-directory (file-name-directory local) t)
    (condition-case nil
        (url-retrieve
         url
         (lambda (status)
           (unwind-protect
               (when (and (not (plist-get status :error))
                          (progn (goto-char (point-min))
                                 (re-search-forward "\n\n" nil t)))
                 (let ((coding-system-for-write 'binary))
                   (write-region (point) (point-max) local nil 'silent)))
             (kill-buffer (current-buffer))
             (funcall callback)))
         nil t t)
      (error (funcall callback)))))

(defun gikopoi--dl-next ()
  (if gikopoi--dl-queue
      (let ((relpath (pop gikopoi--dl-queue)))
        (cl-incf gikopoi--dl-active)
        (gikopoi--download-file-async
         relpath
         (lambda ()
           (cl-decf gikopoi--dl-active)
           (cl-incf gikopoi--dl-done)
           (when (zerop (% gikopoi--dl-done 25))
             (message "Gikopoi: downloading assets… %d/%d"
                      gikopoi--dl-done gikopoi--dl-total))
           (gikopoi--dl-next))))
    (when (zerop gikopoi--dl-active)
      (clrhash gikopoi--asset-datauri-cache)
      (clrhash gikopoi--asset-dims-cache)
      (message "Gikopoi: finished downloading %d assets." gikopoi--dl-done)
      (ignore-errors (gikopoi--refresh-map-buffer)))))

(defun gikopoi-map-download-assets (&optional force)
  "Download all gikopoi static room and character assets into the cache.
Without a prefix arg, only missing files are fetched.  With a prefix arg
\\[universal-argument] FORCE, re-download everything (use this when the
upstream rooms or characters have been updated)."
  (interactive "P")
  (when (and gikopoi--dl-queue (> gikopoi--dl-active 0))
    (user-error "Gikopoi: a download is already in progress"))
  (message "Gikopoi: fetching asset list…")
  (let* ((all (gikopoi--asset-manifest))
         (todo (if force
                   all
                 (cl-remove-if
                  (lambda (r) (file-exists-p (expand-file-name r (gikopoi--asset-cache-dir))))
                  all))))
    (if (null todo)
        (message "Gikopoi: all %d assets already cached." (length all))
      (setq gikopoi--dl-queue  todo
            gikopoi--dl-total  (length todo)
            gikopoi--dl-done   0
            gikopoi--dl-active 0)
      (message "Gikopoi: downloading %d assets (of %d)…" (length todo) (length all))
      (dotimes (_ (min gikopoi--dl-concurrency gikopoi--dl-total))
        (gikopoi--dl-next)))))

(defun gikopoi--img-el (file px py w h &optional mirror)
  "Return an SVG <image> element for FILE at (PX,PY) sized WxH, optionally MIRRORed."
  (let ((uri (gikopoi--asset-datauri file)))
    (if mirror
        (format "<g transform=\"translate(%s,0) scale(-1,1)\"><image x=\"0\" y=\"%s\" width=\"%s\" height=\"%s\" xlink:href=\"%s\"/></g>"
                (gikopoi--n (+ px w)) (gikopoi--n py) (gikopoi--n w) (gikopoi--n h) uri)
      (format "<image x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" xlink:href=\"%s\"/>"
              (gikopoi--n px) (gikopoi--n py) (gikopoi--n w) (gikopoi--n h) uri))))

(defun gikopoi--user-sprite-spec (u sit-set walking phase)
  "Return (SIDE STATE MIRROR) for user U, given the room's SIT-SET hash.
Mirrors the gikopoi2 client: back when facing up/left, mirrored when left/down.
When WALKING, alternate the two walk frames according to PHASE."
  (let* ((dir (or (gikopoi-user-direction u) "down"))
         (pos (gikopoi-user-position u))
         (sitting (and pos (gethash (cons (car pos) (cdr pos)) sit-set))))
    (list (if (member dir '("up" "left")) "back" "front")
          (cond (walking (if (cl-evenp (floor (/ phase 3.0))) "walking-1" "walking-2"))
                (sitting "sitting")
                (t       "standing"))
          (and (member dir '("left" "down")) t))))

;;; --- Walk animation ---------------------------------------------------------

(defun gikopoi--tile-feet (x y)
  "Return the sprite feet position (PX . PY) in background pixels for tile X,Y."
  (let* ((assets (gikopoi--room-assets))
         (origin (alist-get 'originCoordinates assets))
         (ox     (or (alist-get 'x origin) 0))
         (oy     (or (alist-get 'y origin) 0))
         (bw     (or (alist-get 'blockWidth assets) gikopoi--map-block-w))
         (bh     (or (alist-get 'blockHeight assets) gikopoi--map-block-h))
         (rx     (+ ox (/ (* (+ x y) bw) 2.0)))
         (ry     (+ oy (/ (* (- x y) bh) 2.0))))
    (cons (+ rx (/ bw 2.0)) ry)))

(defun gikopoi--current-user-feet ()
  "Return the local user's feet position (CX . FY) in background pixels, or nil.
Uses the animated position while walking so the camera follows smoothly."
  (when-let ((u (gikopoi--self-user))
             (pos (gikopoi-user-position u)))
    (let ((e (gethash (gikopoi-user-id u) (gikopoi-mv-anim gikopoi--map))))
      (if (and e (plist-get e :walking))
          (cons (plist-get e :px) (plist-get e :py))
        (gikopoi--tile-feet (car pos) (cdr pos))))))

(defun gikopoi--camera-clamp (v content region)
  "Clamp camera origin V so a REGION-sized view stays over CONTENT pixels.
If CONTENT is smaller than REGION, centre it (result may be negative)."
  (if (<= content region)
      (/ (- content region) 2.0)
    (max 0.0 (min (float v) (- content region)))))

(defun gikopoi--walk-speed ()
  "Walk speed in background pixels per millisecond, matching the gikopoi2 client."
  (let* ((assets (gikopoi--room-assets))
         (bw (or (alist-get 'blockWidth assets) gikopoi--map-block-w)))
    (* bw 0.0015
       (if (equal (gikopoi-room-id (gikopoi-ss-room gikopoi--session)) "long_st") 2 1))))

(defun gikopoi--map-anim-stop ()
  (when (gikopoi-mv-anim-timer gikopoi--map)
    (cancel-timer (gikopoi-mv-anim-timer gikopoi--map))
    (setf (gikopoi-mv-anim-timer gikopoi--map) nil)))

(defun gikopoi--map-anim-reset ()
  "Clear all walk-animation state and stop the timer (e.g. on room change)."
  (clrhash (gikopoi-mv-anim gikopoi--map))
  (gikopoi--map-anim-stop))

(defun gikopoi--map-anim-visible-p ()
  (and (gikopoi-mv-graphical gikopoi--map) (gikopoi-ss-room gikopoi--session)
       (buffer-live-p (gikopoi-mv-buffer gikopoi--map))
       (get-buffer-window (gikopoi-mv-buffer gikopoi--map) t)))

(defun gikopoi--map-start-walk (id old-pos)
  "Begin animating user ID walking away from OLD-POS (its previous tile)."
  (let* ((e    (gethash id (gikopoi-mv-anim gikopoi--map)))
         (seed (if (and e (plist-get e :walking))
                   (cons (plist-get e :px) (plist-get e :py))   ; continue mid-walk
                 (gikopoi--tile-feet (car old-pos) (cdr old-pos)))))
    (puthash id (list :px (car seed) :py (cdr seed) :walking t
                      :phase (if e (plist-get e :phase) 0))
             (gikopoi-mv-anim gikopoi--map)))
  (unless (gikopoi-mv-anim-timer gikopoi--map)
    (setf (gikopoi-mv-anim-timer gikopoi--map)
          (run-at-time 0 (/ 1.0 (max 1 gikopoi-map-anim-fps))
                       #'gikopoi--map-anim-tick))))

(defun gikopoi--map-anim-tick ()
  "Advance every walking sprite toward its tile, redraw, and stop when idle."
  (condition-case err
      (if (not (gikopoi--map-anim-visible-p))
          (gikopoi--map-anim-reset)
        (let ((moving nil)
              (step   (* (gikopoi--walk-speed) (/ 1000.0 (max 1 gikopoi-map-anim-fps)))))
          (maphash
           (lambda (id e)
             (when (plist-get e :walking)
               (let ((u (gikopoi-user-by-id id)))
                 (if (not u)
                     (remhash id (gikopoi-mv-anim gikopoi--map))
                   (let* ((pos  (gikopoi-user-position u))
                          (feet (gikopoi--tile-feet (car pos) (cdr pos)))
                          (px   (plist-get e :px)) (py (plist-get e :py))
                          (dx   (- (car feet) px)) (dy (- (cdr feet) py))
                          (dist (sqrt (+ (* dx dx) (* dy dy)))))
                     (if (<= dist step)
                         (setq e (plist-put (plist-put e :px (car feet))
                                            :py (cdr feet))
                               e (plist-put e :walking nil))
                       (setq e (plist-put e :px (+ px (* step (/ dx dist))))
                             e (plist-put e :py (+ py (* step (/ dy dist))))
                             e (plist-put e :phase (1+ (plist-get e :phase))))
                       (setq moving t))
                     (puthash id e (gikopoi-mv-anim gikopoi--map)))))))
           (gikopoi-mv-anim gikopoi--map))
          (gikopoi--map-draw)
          (unless moving (gikopoi--map-anim-stop))))
    (error (gikopoi--map-anim-stop)
           (message "Gikopoi anim error: %s" (error-message-string err)))))

(defun gikopoi--sprite-file (char-id side state alt)
  "Return the cached sprite file for CHAR-ID, trying alt/normal and svg/png."
  (when char-id
    (or (and alt (gikopoi--asset-file (format "characters/%s/%s-%s-alt.svg" char-id side state)))
        (gikopoi--asset-file (format "characters/%s/%s-%s.svg" char-id side state))
        (gikopoi--asset-file (format "characters/%s/%s-%s.png" char-id side state)))))

(defcustom gikopoi-map-bubble-opacity 0.97
  "Opacity of speech-bubble backgrounds on the graphical map (0..1)."
  :group 'gikopoi :type 'number)

(defun gikopoi--wrap-message (msg maxcols maxlines)
  "Split MSG into at most MAXLINES lines of at most MAXCOLS display columns."
  (let ((out '()))
    (catch 'done
      (dolist (para (split-string (or msg "") "[\r\n]+"))
        (let ((sline para))
          (if (string-empty-p sline)
              (when (>= (length out) maxlines) (throw 'done nil))
            (while (and sline (not (string-empty-p sline)))
              (when (>= (length out) maxlines) (throw 'done nil))
              (let ((i 0) (w 0))
                (while (and (< i (length sline))
                            (<= (+ w (char-width (aref sline i))) maxcols))
                  (setq w (+ w (char-width (aref sline i))) i (1+ i)))
                (setq i (max 1 i))
                (let ((seg (string-trim (substring sline 0 i))))
                  (unless (string-empty-p seg) (push seg out)))
                (setq sline (substring sline i)))))))
      nil)
    (or (nreverse out) (list ""))))

(defun gikopoi--text-px (s size)
  "Rough rendered width of string S at font SIZE, for sizing boxes to the text.
Per-character estimate for a bold sans-serif font, so boxes hug the text
instead of leaving whitespace on the right."
  (let ((u 0.0))
    (dolist (c (append s nil))
      (setq u (+ u (cond ((> c 127) 1.05)                                   ; CJK / wide
                         ((memq c '(?i ?l ?j ?I ?t ?f ?\. ?, ?' ?\; ?: ?! ?| ?\s)) 0.30)
                         ((memq c '(?m ?w ?M ?W ?@)) 0.92)
                         ((and (>= c ?A) (<= c ?Z)) 0.68)
                         ((and (>= c ?0) (<= c ?9)) 0.56)
                         (t 0.52))))) ; lowercase average
    (* u size)))

(defun gikopoi--name-tag-el (name cx top mine)
  "Return a gikopoi2-style name tag for NAME centred at CX, above head y TOP.
Bold text (red for MINE, else blue) on a translucent white background —
readable on any room, matching the JS client's `getNameImage'."
  (let* ((segs  (split-string name "◆"))
         (disp  (car segs))
         (trip  (cadr segs))
         (lines (delq nil (list (and (not (string-empty-p (or disp ""))) disp)
                                (and trip (concat "◆" trip)))))
         (lh 13)
         (h  (+ (* (length lines) lh) 3))
         ;; generous so the tag always covers the bold text end-to-end
         (w  (+ 6 (* 7.5 (apply #'max 1 (mapcar #'string-width lines)))))
         (bx (- cx (/ w 2)))
         (by (- top h 1))
         (color (if mine "red" "blue")))
    (concat
     "<g>"
     (format "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"#ffffff\" fill-opacity=\"0.5\"/>"
             (gikopoi--n bx) (gikopoi--n by) (gikopoi--n w) (gikopoi--n h))
     (let ((i 0) (acc ""))
       (dolist (ln lines acc)
         (setq acc (concat acc
                           (format "<text x=\"%s\" y=\"%s\" text-anchor=\"middle\" font-size=\"13\" font-weight=\"bold\" font-family=\"Arial,Helvetica,sans-serif\" fill=\"%s\">%s</text>"
                                   (gikopoi--n cx) (gikopoi--n (+ by (* lh i) (- lh 2)))
                                   color (gikopoi--xml-escape ln)))
               i (1+ i))))
     "</g>")))

(defun gikopoi--bubble-el (msg cx fy bpos)
  "Return a gikopoi2-style speech bubble for MSG.
CX/FY are the sprite centre-x and feet-y in background pixels; BPOS the bubble
side (\"up\"/\"down\"/\"left\"/\"right\").  The bubble is a borderless
translucent white box with a triangular tail toward the speaker, placed with
the same offsets the JS client uses."
  (let* ((lines  (gikopoi--wrap-message (string-trim msg) 35 5))
         (padx 4) (pady 3) (lh 15) (fs 13) (a 6)
         (tw     (apply #'max (mapcar (lambda (l) (gikopoi--text-px l fs)) lines)))
         (w      (+ tw (* 2 padx)))
         (h      (+ (* (length lines) lh) (* 2 pady)))
         ;; placement (drawBubbles): pos0 => right side, pos1 => lower
         (pos0   (member bpos '("up" "right")))
         (pos1   (member bpos '("down" "right")))
         (bx     (+ cx (if pos0 21 (- -21 w))))
         (by     (- fy (if pos1 62 (+ 70 h))))
         (fill   (format "fill=\"#ffffff\" fill-opacity=\"%s\"" gikopoi-map-bubble-opacity))
         ;; triangular tail at the corner nearest the avatar
         (tail   (pcase bpos
                   ("up"    (list (cons bx (- (+ by h) a)) (cons (+ bx a) (+ by h))
                                  (cons (- bx a) (+ (+ by h) a))))
                   ("down"  (list (cons (- (+ bx w) a) by) (cons (+ bx w) (+ by a))
                                  (cons (+ (+ bx w) a) (- by a))))
                   ("left"  (list (cons (- (+ bx w) a) (+ by h)) (cons (+ bx w) (- (+ by h) a))
                                  (cons (+ (+ bx w) a) (+ (+ by h) a))))
                   (_       (list (cons (+ bx a) by) (cons bx (+ by a))
                                  (cons (- bx a) (- by a)))))))
    (concat
     "<g>"
     ;; tail (drawn first, base tucked under the box)
     (format "<polygon points=\"%s\" %s/>"
             (mapconcat (lambda (p) (format "%s,%s" (gikopoi--n (car p)) (gikopoi--n (cdr p))))
                        tail " ")
             fill)
     ;; box
     (format "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" %s/>"
             (gikopoi--n bx) (gikopoi--n by) (gikopoi--n w) (gikopoi--n h) fill)
     ;; text lines
     (let ((i 0) (acc ""))
       (dolist (ln lines acc)
         (setq acc (concat acc
                           (format "<text x=\"%s\" y=\"%s\" font-size=\"%s\" font-family=\"'MS PGothic',sans-serif\" fill=\"#000000\">%s</text>"
                                   (gikopoi--n (+ bx padx))
                                   (gikopoi--n (+ by pady (* lh i) (- lh 3)))
                                   fs (gikopoi--xml-escape ln)))
               i (1+ i))))
     "</g>")))

(defun gikopoi--map-svg ()
  "Compose and return the current room as an SVG string, or nil if unavailable."
  (let* ((assets (gikopoi--room-assets))
         (bgurl  (alist-get 'backgroundImageUrl assets))
         (bgfile (and bgurl (gikopoi--asset-file bgurl))))
    (when bgfile
      (let* ((s      (or (alist-get 'scale assets) 1))
             (dims   (or (gikopoi--asset-dims bgfile) '(721 . 511)))
             (cw     (* s (car dims)))
             (ch     (* s (cdr dims)))
             (origin (alist-get 'originCoordinates assets))
             (ox     (or (alist-get 'x origin) 0))
             (oy     (or (alist-get 'y origin) 0))
             (bw     (or (alist-get 'blockWidth assets) gikopoi--map-block-w))
             (bh     (or (alist-get 'blockHeight assets) gikopoi--map-block-h))
             (sy     (or (alist-get 'y (alist-get 'size assets)) 0))
             (sit    (gikopoi--coords->set (alist-get 'sit assets)))
             (rid    (gikopoi-room-id (gikopoi-ss-room gikopoi--session)))
             (items   '())
             (parts   '())
             (bubbles '()))
        (cl-flet ((realx (x y) (+ ox (/ (* (+ x y) bw) 2.0)))
                  (realy (x y) (+ oy (/ (* (- x y) bh) 2.0))))
          ;; --- gather objects ---
          (dolist (o (append (alist-get 'objects assets) nil))
            (unless (eq (alist-get 'isHidden o) t)
              (let* ((url (alist-get 'url o))
                     (url (if (vectorp url) (and (> (length url) 0) (aref url 0)) url))
                     (off (alist-get 'offset o))
                     (osc (or (alist-get 'scale o) 1)))
                ;; positions/sizes are in raw background-pixel space; only the
                ;; background itself is scaled by the room `scale' (see canvas).
                (when url
                  (push (list :prio (+ (alist-get 'x o) 1 (- sy (alist-get 'y o)))
                              :type 'object :oscale osc
                              :relpath (concat "rooms/" rid "/" url)
                              :px (* (or (alist-get 'x off) 0) osc)
                              :py (* (or (alist-get 'y off) 0) osc))
                        items)))))
          ;; --- gather users ---
          (dolist (u (gikopoi-room-users (gikopoi-ss-room gikopoi--session)))
            (unless (gikopoi-user-ignored-p u)
              (when-let ((pos (gikopoi-user-position u)))
                (push (list :prio (+ (car pos) 1 (- sy (cdr pos)))
                            :type 'user :user u :x (car pos) :y (cdr pos))
                      items))))
          ;; --- painter's order: lower priority first, objects before users on tie ---
          (setq items
                (sort items
                      (lambda (a b)
                        (let ((pa (plist-get a :prio)) (pb (plist-get b :prio)))
                          (if (= pa pb)
                              (and (eq (plist-get a :type) 'object)
                                   (eq (plist-get b :type) 'user))
                            (< pa pb))))))
          ;; --- emit ---
          (dolist (it items)
            (pcase (plist-get it :type)
              ('object
               (when-let* ((file (gikopoi--asset-file (plist-get it :relpath)))
                           (d    (gikopoi--asset-dims file)))
                 (push (gikopoi--img-el file (plist-get it :px) (plist-get it :py)
                                        (* (plist-get it :oscale) (car d))
                                        (* (plist-get it :oscale) (cdr d)))
                       parts)))
              ('user
               (let* ((u       (plist-get it :user))
                      (x       (plist-get it :x)) (y (plist-get it :y))
                      (e       (gethash (gikopoi-user-id u) (gikopoi-mv-anim gikopoi--map)))
                      (walking (and e (plist-get e :walking)))
                      (spec    (gikopoi--user-sprite-spec
                                u sit walking (if e (plist-get e :phase) 0)))
                      (file    (gikopoi--sprite-file (gikopoi-user-character-id u)
                                                     (nth 0 spec) (nth 1 spec)
                                                     (gikopoi-user-alt-p u)))
                      (cx      (if walking (plist-get e :px) (+ (realx x y) (/ bw 2.0))))
                      (fy      (if walking (plist-get e :py) (realy x y)))
                      (ghost   (not (gikopoi-user-active-p u)))
                      (top     fy)
                      (sprite  nil))
                 (if-let ((d (and file (gikopoi--asset-dims file))))
                     (let* ((w  (* gikopoi--map-char-scale (car d)))
                            (hh (* gikopoi--map-char-scale (cdr d))))
                       (setq top (- fy hh)
                             sprite (gikopoi--img-el file (- cx (/ w 2)) top w hh (nth 2 spec))))
                   ;; fallback marker so positions still show without a sprite
                   (setq top (- fy 26)
                         sprite (format "<circle cx=\"%s\" cy=\"%s\" r=\"8\" fill=\"%s\" stroke=\"#000\"/>"
                                        (gikopoi--n cx) (gikopoi--n (- fy 8))
                                        (or (ignore-errors (gikopoi-user-name-color u)) "#ff4040"))))
                 ;; idle/away users render as a translucent ghost
                 (push (if ghost (format "<g opacity=\"0.5\">%s</g>" sprite) sprite) parts)
                 ;; name tag (gikopoi2 style: blue/red bold on translucent white)
                 (when gikopoi-map-show-names
                   (let ((label (substring-no-properties (or (gikopoi-user-name u) ""))))
                     (unless (string-empty-p label)
                       (push (gikopoi--name-tag-el
                              label cx top (eq u (gikopoi--self-user)))
                             parts))))
                 ;; speech bubble — persists as long as the user keeps the message
                 (when gikopoi-map-show-bubbles
                   (let ((msg (gikopoi-user-last-message u)))
                     (when (and msg (not (string-empty-p (string-trim msg))))
                       (push (gikopoi--bubble-el
                              msg cx fy (or (gikopoi-user-bubble-position u) "up"))
                             bubbles))))))))
          ;; camera: a fixed-size view (so the window keeps its size), zoomed
          ;; via the viewBox and centred on the local user in big rooms.
          (let* ((size (or (gikopoi-mv-vsize gikopoi--map) (gikopoi--map-view-size)))
                 (sw   (car size)) (sh (cdr size))
                 (zoom (max 0.1 (gikopoi-mv-zoom gikopoi--map)))
                 (vw   (/ sw zoom))
                 (vh   (/ sh zoom))
                 (feet (and gikopoi-map-follow (gikopoi--current-user-feet)))
                 (ccx  (if feet (car feet) (/ cw 2.0)))
                 (ccy  (if feet (- (cdr feet) 60) (/ ch 2.0)))
                 (cam  (gikopoi--map-camera cw ch vw vh ccx ccy rid))
                 (vx   (car cam))
                 (vy   (cdr cam)))
            (concat
             (format "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%s\" height=\"%s\" viewBox=\"%s %s %s %s\">"
                     sw sh
                     (gikopoi--n vx) (gikopoi--n vy) (gikopoi--n vw) (gikopoi--n vh))
             (format "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"%s\"/>"
                     (gikopoi--n vx) (gikopoi--n vy) (gikopoi--n vw) (gikopoi--n vh)
                     (or (alist-get 'backgroundColor assets) "#000000"))
             (gikopoi--img-el bgfile 0 0 cw ch)
             (mapconcat #'identity (nreverse parts) "")
             (mapconcat #'identity (nreverse bubbles) "")  ; bubbles above everyone
             "</svg>")))))))

(define-derived-mode gikopoi-map-mode special-mode "Gikopoi-Map"
  "Major mode for the Gikopoi room map buffer."
  :group 'gikopoi
  (setq truncate-lines t))

(define-key gikopoi-map-mode-map (kbd "g")   #'gikopoi-show-map)
(define-key gikopoi-map-mode-map (kbd "t")   #'gikopoi-map-toggle-graphics)
(define-key gikopoi-map-mode-map (kbd "D")   #'gikopoi-map-download-assets)
(define-key gikopoi-map-mode-map (kbd "+")   #'gikopoi-map-zoom-in)
(define-key gikopoi-map-mode-map (kbd "=")   #'gikopoi-map-zoom-in)
(define-key gikopoi-map-mode-map (kbd "-")   #'gikopoi-map-zoom-out)
(define-key gikopoi-map-mode-map (kbd "0")   #'gikopoi-map-zoom-reset)
(define-key gikopoi-map-mode-map (kbd "b")   #'gikopoi-map-cycle-bubble)
(define-key gikopoi-map-mode-map (kbd "c")   #'gikopoi-map-recenter)

;; Play from the map buffer too, not just the chat buffer: walk, chat, bubble.
(define-key gikopoi-map-mode-map (kbd "<left>")    #'gikopoi-move-left)
(define-key gikopoi-map-mode-map (kbd "<right>")   #'gikopoi-move-right)
(define-key gikopoi-map-mode-map (kbd "<up>")      #'gikopoi-move-up)
(define-key gikopoi-map-mode-map (kbd "<down>")    #'gikopoi-move-down)
(define-key gikopoi-map-mode-map (kbd "<C-left>")  #'gikopoi-bubble-left)
(define-key gikopoi-map-mode-map (kbd "<C-right>") #'gikopoi-bubble-right)
(define-key gikopoi-map-mode-map (kbd "<C-up>")    #'gikopoi-bubble-up)
(define-key gikopoi-map-mode-map (kbd "<C-down>")  #'gikopoi-bubble-down)
(define-key gikopoi-map-mode-map (kbd "SPC")       #'gikopoi-open-minibuffer)
(define-key gikopoi-map-mode-map (kbd "RET")       #'gikopoi-send-blank)
(define-key gikopoi-map-mode-map (kbd "r")         #'gikopoi-rula)
(define-key gikopoi-map-mode-map (kbd "R")         #'gikopoi-list-users)

(defun gikopoi-map-cycle-bubble ()
  "Cycle your own speech-bubble position: up -> right -> down -> left."
  (interactive)
  (let* ((order '("up" "right" "down" "left"))
         (cur   (and (gikopoi-ss-user gikopoi--session)
                     (gikopoi-user-bubble-position (gikopoi-ss-user gikopoi--session))))
         (next  (or (cadr (member cur order)) (car order))))
    (gikopoi-bubble-position next)
    (message "Bubble position: %s" next)))

(defun gikopoi-map-toggle-graphics ()
  "Toggle the room map between composited graphics and the ASCII grid.
On a text terminal (no SVG image support) the map always uses ASCII."
  (interactive)
  (if (and (not (gikopoi-mv-graphical gikopoi--map)) (not (image-type-available-p 'svg)))
      (message "Gikopoi: graphical map needs a GUI with SVG support; using ASCII")
    (setf (gikopoi-mv-graphical gikopoi--map) (not (gikopoi-mv-graphical gikopoi--map)))
    (gikopoi--map-draw)
    (gikopoi--map-fit-windows)           ; graphics/ASCII differ in size
    (message "Gikopoi map: %s" (if (gikopoi-mv-graphical gikopoi--map) "graphical" "ASCII"))))

(defun gikopoi-map-zoom (factor)
  "Multiply the graphical-map zoom by FACTOR and redraw.
Zooming lets big rooms (e.g. `silo') fit the window; `0' resets to 100%."
  (setf (gikopoi-mv-zoom gikopoi--map) (max 0.1 (min 6.0 (* (gikopoi-mv-zoom gikopoi--map) factor))))
  (gikopoi--map-draw)
  (gikopoi--map-fit-windows)
  (message "Gikopoi map zoom: %d%%" (round (* 100 (gikopoi-mv-zoom gikopoi--map)))))

(defun gikopoi-map-zoom-in ()    (interactive) (gikopoi-map-zoom 1.25))
(defun gikopoi-map-zoom-out ()   (interactive) (gikopoi-map-zoom 0.8))
(defun gikopoi-map-zoom-reset ()
  (interactive)
  (setf (gikopoi-mv-zoom gikopoi--map) 1.0)
  (gikopoi--map-draw) (gikopoi--map-fit-windows)
  (message "Gikopoi map zoom: 100%%"))

(defun gikopoi--map-graphical-p ()
  (and (gikopoi-mv-graphical gikopoi--map) (image-type-available-p 'svg)))

(defun gikopoi--map-draw ()
  "Render the room map into its buffer.
For the graphical map the image's `display' property is swapped in place — the
buffer is not erased and point/window-start don't move — so repeated redraws
during movement don't make the window shake or the view jump."
  (when (buffer-live-p (gikopoi-mv-buffer gikopoi--map))
    (with-current-buffer (gikopoi-mv-buffer gikopoi--map)
      (let ((inhibit-read-only t)
            (svg (and (gikopoi--map-graphical-p)
                      (condition-case err (gikopoi--map-svg)
                        (error (message "Gikopoi map: %s" (error-message-string err)) nil)))))
        (if (not svg)
            (progn (erase-buffer)
                   (gikopoi--render-map-ascii)
                   (goto-char (point-min)))
          (let ((img (create-image svg 'svg t :scale 1.0)))
            (if (and (> (buffer-size) 0)
                     (get-text-property (point-min) 'display))
                (put-text-property (point-min) (point-max) 'display img)
              (erase-buffer)
              (insert-image img))
            ;; Pin every map window to the top-left corner so a redraw can't
            ;; make the image jitter by scrolling to keep point in view.
            (goto-char (point-min))
            (dolist (w (get-buffer-window-list (gikopoi-mv-buffer gikopoi--map) nil t))
              (set-window-point  w (point-min))
              (set-window-hscroll w 0)
              (set-window-start  w (point-min) t))))))))

(defun gikopoi--map-fit-windows ()
  "Fit ASCII-map windows to their text.
Graphical windows are left as the user/`display-buffer' sized them — the image
is rendered to fill that size (see `gikopoi--map-view-size'), so resizing the
window resizes the room view rather than the other way around."
  (when (and (buffer-live-p (gikopoi-mv-buffer gikopoi--map)) (not (gikopoi--map-graphical-p)))
    (setf (gikopoi-mv-fitted-room gikopoi--map)
          (and (gikopoi-ss-room gikopoi--session) (gikopoi-room-id (gikopoi-ss-room gikopoi--session))))
    (run-at-time
     0 nil
     (lambda ()
       (when (buffer-live-p (gikopoi-mv-buffer gikopoi--map))
         (dolist (w (get-buffer-window-list (gikopoi-mv-buffer gikopoi--map) nil t))
           (let ((window-min-height 4))
             (fit-window-to-buffer w (- (frame-height) 4) 8))))))))

(defun gikopoi--refresh-map-buffer ()
  "Redraw the map if displayed; refit only the ASCII window when the room changed."
  (when (and (buffer-live-p (gikopoi-mv-buffer gikopoi--map))
             (get-buffer-window (gikopoi-mv-buffer gikopoi--map) t))
    (gikopoi--map-draw)
    (unless (or (gikopoi--map-graphical-p)
                (equal (gikopoi-mv-fitted-room gikopoi--map)
                       (and (gikopoi-ss-room gikopoi--session) (gikopoi-room-id (gikopoi-ss-room gikopoi--session)))))
      (gikopoi--map-fit-windows))))

(defun gikopoi--map-stabilize-window (win)
  "Drop fringes/scroll bars on WIN so the image size stays steady (no jitter)."
  (when (window-live-p win)
    (set-window-fringes win 0 0)
    (when (fboundp 'set-window-scroll-bars)
      (set-window-scroll-bars win 0 nil 0 nil))))

(defun gikopoi--map-on-size-change (&rest _)
  "Recompute the cached view size and re-render the map after a real resize."
  (when (and (gikopoi--map-graphical-p)
             (buffer-live-p (gikopoi-mv-buffer gikopoi--map))
             (get-buffer-window (gikopoi-mv-buffer gikopoi--map) t))
    (let ((sz (gikopoi--map-view-size)))
      (unless (equal sz (gikopoi-mv-vsize gikopoi--map))
        (setf (gikopoi-mv-vsize gikopoi--map) sz)
        (gikopoi--map-draw)))))

(add-hook 'window-size-change-functions #'gikopoi--map-on-size-change)

(defun gikopoi-map-recenter ()
  "Re-centre the graphical-map camera on you.
The camera already follows you every frame (see `gikopoi--map-camera'), so
this just forces an immediate redraw."
  (interactive)
  (gikopoi--map-draw))

(defun gikopoi-show-map ()
  "Display the current room map and everyone's position in it.
Opens in a properly sized window below the current one; the graphical map
fills that window.  Resize the window to resize the view."
  (interactive)
  (unless (gikopoi-ss-room gikopoi--session)
    (user-error "Gikopoi: not in a room"))
  (unless (buffer-live-p (gikopoi-mv-buffer gikopoi--map))
    (setf (gikopoi-mv-buffer gikopoi--map) (get-buffer-create "*Gikopoi Map*"))
    (with-current-buffer (gikopoi-mv-buffer gikopoi--map) (gikopoi-map-mode)))
  ;; Show first so the window exists, then lock its size and draw to fill it.
  (display-buffer
   (gikopoi-mv-buffer gikopoi--map)
   '((display-buffer-reuse-window display-buffer-below-selected)
     (window-height . 0.6)
     (preserve-size . (nil . t))))
  (let ((win (get-buffer-window (gikopoi-mv-buffer gikopoi--map) t)))
    (gikopoi--map-stabilize-window win)
    (setf (gikopoi-mv-vsize gikopoi--map) (gikopoi--map-view-size)))
  (gikopoi--map-draw)
  (gikopoi--map-fit-windows))


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
  (define-key m (kbd "m")         #'gikopoi-show-map)
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
                              (and (gikopoi-ss-room gikopoi--session)
                                   (gikopoi-room-id (gikopoi-ss-room gikopoi--session)))
                              (gikopoi-ss-server gikopoi--session)
                              (gikopoi-ss-stats-mode-line gikopoi--session)))))

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

;; TAB name completion.  We roll our own cycling completer rather than lean on
;; `completion-in-region', because the latter drives everything through
;; `completion-styles' — which split candidates on word/script boundaries and
;; so mangle Japanese (and other non-space-delimited) names, and which pop a
;; *Completions* buffer inside our recursive minibuffer.  Here the stem is the
;; whitespace-delimited token before point, matched as a plain case-insensitive
;; prefix (script-agnostic), and repeated TAB cycles through the candidates.

(defvar gikopoi--complete-beg nil
  "Buffer position where the token being completed begins.")
(defvar gikopoi--complete-cands nil
  "Candidate names for the in-progress TAB completion.")
(defvar gikopoi--complete-index 0
  "Index of the currently-inserted candidate in `gikopoi--complete-cands'.")

(defun gikopoi--complete-candidates (stem)
  "Room user names (unpropertized) whose prefix case-insensitively matches STEM.
An empty STEM matches every name."
  (let (out)
    (dolist (n (gikopoi-user-names))
      (let ((plain (substring-no-properties n)))
        (when (or (string-empty-p stem) (string-prefix-p stem plain t))
          (push plain out))))
    (nreverse out)))

(defun gikopoi--complete-insert ()
  "Replace the token from `gikopoi--complete-beg' to point with the current
candidate, and report the position within the candidate list."
  (delete-region gikopoi--complete-beg (point))
  (insert (nth gikopoi--complete-index gikopoi--complete-cands))
  (let ((n (length gikopoi--complete-cands)))
    (when (> n 1)
      (minibuffer-message " [%d/%d]" (1+ gikopoi--complete-index) n))))

(defun gikopoi-minibuffer-complete ()
  "Complete the name token before point against room user names.
Works for Japanese and other non-space-delimited scripts by matching a raw
case-insensitive prefix rather than `thing-at-point' word bounds.  Repeated
TAB cycles through the matching names."
  (interactive)
  (if (and (eq last-command this-command) gikopoi--complete-cands)
      ;; Repeated TAB: cycle to the next candidate, reusing the stored stem.
      (progn
        (setq gikopoi--complete-index
              (mod (1+ gikopoi--complete-index)
                   (length gikopoi--complete-cands)))
        (gikopoi--complete-insert))
    ;; Fresh completion: recompute the token and its candidates.
    (let* ((end  (point))
           (beg  (save-excursion (skip-chars-backward "^ \t\n") (point)))
           (stem (buffer-substring-no-properties beg end))
           (cands (gikopoi--complete-candidates stem)))
      (cond
       ((null cands)
        (setq gikopoi--complete-cands nil)
        (minibuffer-message "No matching name"))
       (t
        (setq gikopoi--complete-beg   beg
              gikopoi--complete-cands cands
              gikopoi--complete-index 0)
        (gikopoi--complete-insert))))))

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
      (gikopoi--room-list-fetch-synchronously)
      (let* ((candidates
              (mapcar (lambda (e)
                        (let* ((count (aref (cadr e) 2))
                               (label (format "%s [%s]" (car e) count)))
                          (if (and gikopoi-occupied-room-color
                                   (not (string= count "0")))
                              (propertize label 'face
                                          `(:foreground ,gikopoi-occupied-room-color))
                            label)))
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
  (interactive (list (read-from-minibuffer "Ignore/unignore user: " nil gikopoi-minibuffer-map)))
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
  (interactive (list (read-from-minibuffer "Block user: " nil gikopoi-minibuffer-map)))
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
  (when (gikopoi-ss-room gikopoi--session)
    (dolist (u (gikopoi-room-users (gikopoi-ss-room gikopoi--session)))
      (when (member (gikopoi-user-raw-name u) gikopoi-auto-ignore-names)
        (setf (gikopoi-user-ignored-p u) t))))
  (message "Loaded %d auto-ignored users" (length gikopoi-auto-ignore-names)))

(defun gikopoi-init-auto-ignore (&rest _)
  "Defer auto-ignore loading until the room is ready."
  (cond ((and (gikopoi-ss-room gikopoi--session) (gikopoi-room-users (gikopoi-ss-room gikopoi--session)))
         (gikopoi-load-auto-ignored-users))
        ((and (gikopoi-ss-socket gikopoi--session) (websocket-openp (gikopoi-ss-socket gikopoi--session)))
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
        (lambda () (setf (gikopoi-ss-stats-mode-line gikopoi--session) "") (force-mode-line-update)))
  "Hook run when disconnecting.  Each function is called with no arguments.")

(defun gikopoi-quit ()
  "Disconnect from Gikopoipoi (with confirmation)."
  (interactive)
  (when (y-or-n-p "Disconnect from Gikopoipoi? ")
    (setf (gikopoi-ss-deliberately-quit gikopoi--session) t)
    (when (timerp (gikopoi-ss-reconnect-timer gikopoi--session)) (cancel-timer (gikopoi-ss-reconnect-timer gikopoi--session)))
    (run-hooks 'gikopoi-quit-functions)))

(defun gikopoi-quit-silent ()
  "Disconnect silently (no confirmation; used internally by reconnect)."
  (setf (gikopoi-ss-deliberately-quit gikopoi--session) t)
  (when (timerp (gikopoi-ss-reconnect-timer gikopoi--session)) (cancel-timer (gikopoi-ss-reconnect-timer gikopoi--session)))
  (run-hooks 'gikopoi-quit-functions))

(defun gikopoi--full-relogin ()
  "Reconnect via a full HTTP re-login (fresh private-user-id, fresh spawn).
This is the fallback used when a lightweight resume isn't possible or was
refused by the server (see `gikopoi-reconnect' and `server-cant-log-you-in').
Since a fresh login always spawns at the room's default entry tile rather
than wherever we actually were, we remember our tile here and walk back to
it once the new room state settles (see `server-update-current-room-state'
and `gikopoi-restore-position-on-reconnect')."
  (when (gikopoi-ss-room gikopoi--session)
    (setf (nth 3 (gikopoi-ss-last-args gikopoi--session)) (gikopoi-room-id (gikopoi-ss-room gikopoi--session))))
  (when-let ((me (and gikopoi-restore-position-on-reconnect
                      (gikopoi-ss-user gikopoi--session))))
    (setf (gikopoi-ss-saved-position gikopoi--session) (gikopoi-user-position me)
          (gikopoi-ss-saved-room gikopoi--session)
          (and (gikopoi-ss-room gikopoi--session)
               (gikopoi-room-id (gikopoi-ss-room gikopoi--session)))))
  (ignore-errors (gikopoi-quit-silent))
  (apply #'gikopoi (gikopoi-ss-last-args gikopoi--session)))

(defun gikopoi-reconnect ()
  "Reconnect using the same credentials as the last `gikopoi' call.
Tries a lightweight resume first: reopen the raw WebSocket reusing our
existing private-user-id, the same thing the official web client's
socket.io auto-reconnect does. The server keeps a disconnected player (a
\"ghost\") alive together with its room and exact tile for a while after
the socket drops, so this restores our position exactly with no
client-side walking involved. If the server has since forgotten that
private-user-id (session expired, or the server restarted) it replies
with `server-cant-log-you-in', which falls back to `gikopoi--full-relogin'.
When we don't yet have a private-user-id at all (e.g. we never
successfully connected), that fallback runs directly."
  (interactive)
  (setf (gikopoi-ss-reconnect-attempts gikopoi--session) 0
        (gikopoi-ss-reconnect-delay gikopoi--session) 5)
  (when (gikopoi-ss-last-args gikopoi--session)
    (message "Gikopoi: reconnecting…")
    (if-let ((server (gikopoi-ss-server gikopoi--session))
             (pid    (gikopoi-ss-private-user-id gikopoi--session)))
        (let ((port (nth 1 (gikopoi-ss-last-args gikopoi--session))))
          (setf (gikopoi-ss-deliberately-quit gikopoi--session) nil)
          (when (timerp (gikopoi-ss-reconnect-timer gikopoi--session))
            (cancel-timer (gikopoi-ss-reconnect-timer gikopoi--session)))
          (when (and (gikopoi-ss-socket gikopoi--session)
                     (websocket-openp (gikopoi-ss-socket gikopoi--session)))
            (gikopoi-socket-close))
          (gikopoi-socket-open server port pid))
      (gikopoi--full-relogin))))

;; The periodic reconnect timer lives in `gikopoi--session' (see section 3).

(defun gikopoi-start-reconnect-timer ()
  "Start a periodic timer that reconnects every `gikopoi-reconnect-timer-minutes' minutes."
  (interactive)
  (let ((secs (* gikopoi-reconnect-timer-minutes 60)))
    (when (gikopoi-ss-periodic-reconnect-timer gikopoi--session) (cancel-timer (gikopoi-ss-periodic-reconnect-timer gikopoi--session)))
    (setf (gikopoi-ss-periodic-reconnect-timer gikopoi--session) (run-at-time secs secs #'gikopoi-reconnect))
    (message "Gikopoi: reconnect timer set for every %d min" gikopoi-reconnect-timer-minutes)))

(defun gikopoi-stop-reconnect-timer ()
  (interactive)
  (when (gikopoi-ss-periodic-reconnect-timer gikopoi--session)
    (cancel-timer (gikopoi-ss-periodic-reconnect-timer gikopoi--session))
    (setf (gikopoi-ss-periodic-reconnect-timer gikopoi--session) nil)
    (message "Gikopoi: reconnect timer stopped")))

(defun gikopoi-maybe-start-reconnect-timer (&rest _)
  (when gikopoi-auto-start-reconnect-timer (gikopoi-start-reconnect-timer)))

(defun gikopoi-connect (server port area room name character password)
  "Establish an HTTP login and open the WebSocket to SERVER."
  (setf (gikopoi-ss-deliberately-quit gikopoi--session) nil
        gikopoi-room-list-data     nil)
  (when (timerp (gikopoi-ss-reconnect-timer gikopoi--session)) (cancel-timer (gikopoi-ss-reconnect-timer gikopoi--session)))
  (when (and (gikopoi-ss-socket gikopoi--session) (websocket-openp (gikopoi-ss-socket gikopoi--session)))
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
      (setf (gikopoi-ss-server gikopoi--session)          server
            (gikopoi-ss-user-id gikopoi--session)         .userId
            (gikopoi-ss-private-user-id gikopoi--session) .privateUserId)
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
  (setf (gikopoi-ss-last-args gikopoi--session) (list server port area room name character password))
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
  (let ((server (or (gikopoi-ss-server gikopoi--session)
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
   (or (gikopoi-ss-server gikopoi--session) "nil")
   (and (gikopoi-ss-room gikopoi--session) (gikopoi-room-id (gikopoi-ss-room gikopoi--session)))
   (cond ((null (gikopoi-ss-socket gikopoi--session)) "nil")
         ((websocket-openp (gikopoi-ss-socket gikopoi--session)) "open")
         (t "closed"))
   (if gikopoi-room-list-data "loaded" "nil")
   (length gikopoi-room-list-data)))

(defun gikopoi-debug-room-list ()
  "Send user-room-list and dump the raw server response to *Gikopoi Room List Debug*."
  (interactive)
  (unless (and (gikopoi-ss-socket gikopoi--session) (websocket-openp (gikopoi-ss-socket gikopoi--session)))
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
