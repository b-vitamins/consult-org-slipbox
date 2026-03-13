;;; consult-org-slipbox.el --- Consult integration for org-slipbox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 org-slipbox contributors

;; Author: Ayan Das <bvits@riseup.net>
;; Maintainer: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (consult "3.3") (org-slipbox "0.2.0"))
;; Keywords: outlines, files, convenience
;; URL: https://github.com/b-vitamins/consult-org-slipbox

;; This file is not part of GNU Emacs.

;; consult-org-slipbox is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; consult-org-slipbox is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with consult-org-slipbox.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Consult-powered interactive commands for org-slipbox.  The package provides
;; direct commands for node, ref, file, occurrence, and related-note
;; workflows, plus an optional minor mode which overrides
;; `org-slipbox-node-read' and `org-slipbox-ref-read' and can install a
;; dedicated `consult-buffer' source for open slipbox note buffers.

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'org)
(require 'org-slipbox)
(require 'org-slipbox-buffer)
(require 'org-slipbox-files)
(require 'org-slipbox-metadata)
(require 'org-slipbox-node)
(require 'org-slipbox-node-insert)
(require 'org-slipbox-node-read)
(require 'org-slipbox-node-visit)
(require 'org-slipbox-search)
(require 'subr-x)

(defgroup consult-org-slipbox nil
  "Consult integration for org-slipbox."
  :group 'org
  :group 'convenience
  :prefix "consult-org-slipbox-")

(defcustom consult-org-slipbox-dynamic-throttle 0.08
  "Throttle used for dynamic Consult collections."
  :type 'number
  :group 'consult-org-slipbox)

(defcustom consult-org-slipbox-dynamic-debounce 0.02
  "Debounce used for dynamic Consult collections."
  :type 'number
  :group 'consult-org-slipbox)

(defcustom consult-org-slipbox-buffer-enabled t
  "When non-nil, expose open slipbox note buffers as a `consult-buffer' source.
This source is installed by `consult-org-slipbox-mode'."
  :type 'boolean
  :group 'consult-org-slipbox)

(defcustom consult-org-slipbox-buffer-narrow-key ?s
  "Narrow key used for the `consult-buffer' slipbox source."
  :type 'character
  :group 'consult-org-slipbox)

(defcustom consult-org-slipbox-buffer-after-buffers nil
  "When non-nil, place the slipbox source right after regular buffers."
  :type 'boolean
  :group 'consult-org-slipbox)

(defcustom consult-org-slipbox-search-limit 200
  "Maximum number of indexed occurrence hits to request."
  :type 'integer
  :group 'consult-org-slipbox)

(defcustom consult-org-slipbox-search-min-input 3
  "Minimum trimmed input length required for occurrence search."
  :type 'integer
  :group 'consult-org-slipbox)

(defcustom consult-org-slipbox-link-limit 200
  "Maximum number of link-style occurrences to request."
  :type 'integer
  :group 'consult-org-slipbox)

(defvar consult-org-slipbox-search-history nil
  "Minibuffer history for `consult-org-slipbox-search'.")

(defvar consult-org-slipbox-file-history nil
  "Minibuffer history for `consult-org-slipbox-file-find'.")

(defvar consult-org-slipbox-location-history nil
  "Minibuffer history for location-oriented org-slipbox commands.")

(defvar consult-buffer-sources)

(defconst consult-org-slipbox--commands
  '(consult-org-slipbox-node-find
    consult-org-slipbox-node-insert
    consult-org-slipbox-file-find
    consult-org-slipbox-ref-find
    consult-org-slipbox-search
    consult-org-slipbox-backlinks
    consult-org-slipbox-forward-links
    consult-org-slipbox-reflinks
    consult-org-slipbox-unlinked-references)
  "Public interactive commands provided by consult-org-slipbox.")

(defvar consult-org-slipbox--buffer-alist nil
  "Current mapping from slipbox `consult-buffer' labels to live buffers.")

(defun consult-org-slipbox--absolute-file (file-path)
  "Return FILE-PATH resolved against `org-slipbox-directory'."
  (expand-file-name file-path org-slipbox-directory))

(defun consult-org-slipbox--node-title (node)
  "Return the preferred display title for NODE."
  (or (plist-get node :title) ""))

(defun consult-org-slipbox--plist-sequence (value)
  "Normalize JSON-derived VALUE into an Emacs list."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun consult-org-slipbox--relative-file (record)
  "Return the relative indexed file path from RECORD."
  (or (plist-get record :file_path) ""))

(defun consult-org-slipbox--record-target (record)
  "Return a visit target plist normalized from RECORD."
  (pcase record
    ((pred (lambda (value) (plist-member value :file_path)))
     (list :file_path (plist-get record :file_path)
           :line (or (plist-get record :line)
                     (plist-get record :row)
                     1)
           :col (or (plist-get record :col) 1)))
    (_ nil)))

(defun consult-org-slipbox--visit-target (target &optional other-window)
  "Visit TARGET, optionally in OTHER-WINDOW."
  (let ((file (plist-get target :file_path))
        (line (or (plist-get target :line) 1))
        (col (or (plist-get target :col) 1)))
    (funcall (if other-window #'find-file-other-window #'find-file)
             (consult-org-slipbox--absolute-file file))
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (move-to-column (max 0 (1- col)))))

(defun consult-org-slipbox--preview-buffer (target open)
  "Return a preview buffer for TARGET using OPEN."
  (let ((buffer (funcall open (consult-org-slipbox--absolute-file
                               (plist-get target :file_path)))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (max 0 (1- (or (plist-get target :line) 1))))
      (move-to-column (max 0 (1- (or (plist-get target :col) 1)))))
    buffer))

(defun consult-org-slipbox--preview-state (extractor)
  "Return a preview state function using EXTRACTOR.
EXTRACTOR receives the selected candidate string and must return a target
plist accepted by `consult-org-slipbox--preview-buffer'."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview))
        (state (window-state-get)))
    (lambda (action cand)
      (pcase action
        ('exit
         (funcall preview 'exit nil)
         (ignore-errors (window-state-put state))
         (funcall open))
        (_
         (funcall preview
                  action
                  (and (eq action 'preview)
                       cand
                       (when-let ((target (funcall extractor cand)))
                         (consult-org-slipbox--preview-buffer target open)))))))))

(defun consult-org-slipbox--decorate-candidate
    (candidate value &optional target related-node group)
  "Attach VALUE, TARGET, RELATED-NODE, and GROUP metadata to CANDIDATE."
  (add-text-properties
   0
   (length candidate)
   `(consult--candidate ,value
                        consult-org-slipbox-target ,target
                        consult-org-slipbox-related-node ,related-node)
   candidate)
  (when group
    (put-text-property 0 1 'consult--prefix-group group candidate))
  candidate)

(defun consult-org-slipbox--candidate-value (candidate)
  "Return the structured value stored on CANDIDATE."
  (and (stringp candidate)
       (get-text-property 0 'consult--candidate candidate)))

(defun consult-org-slipbox--candidate-target (candidate)
  "Return the visit target stored on CANDIDATE."
  (and (stringp candidate)
       (get-text-property 0 'consult-org-slipbox-target candidate)))

(defun consult-org-slipbox--candidate-related-node (candidate)
  "Return the related node stored on CANDIDATE."
  (and (stringp candidate)
       (get-text-property 0 'consult-org-slipbox-related-node candidate)))

(defmacro consult-org-slipbox--with-origin-window (&rest body)
  "Run BODY from the original non-minibuffer window when available."
  (declare (indent 0) (debug t))
  `(let ((window (and (fboundp 'consult--original-window)
                      (consult--original-window))))
     (if (window-live-p window)
         (with-selected-window window
           ,@body)
       (progn ,@body))))

(defun consult-org-slipbox--candidate-lookup (selected candidates input _narrow)
  "Resolve SELECTED from CANDIDATES, falling back to INPUT."
  (or (consult--lookup-candidate selected candidates)
      input))

(defun consult-org-slipbox--format-time (mtime-ns)
  "Return MTIME-NS formatted for display."
  (when (and (integerp mtime-ns) (> mtime-ns 0))
    (format-time-string "%Y-%m-%d %H:%M"
                        (seconds-to-time (/ mtime-ns 1000000000.0)))))

(defun consult-org-slipbox--pluralize (n singular)
  "Return SINGULAR pluralized for count N."
  (if (= n 1)
      singular
    (concat singular "s")))

(defun consult-org-slipbox--buffer-visible-name (buffer)
  "Return a visible Consult label for slipbox note BUFFER."
  (let* ((file (or (buffer-file-name buffer) ""))
         (path (if (and (stringp org-slipbox-directory)
                        (not (string-empty-p org-slipbox-directory)))
                   (file-relative-name
                    file
                    (file-name-as-directory
                     (expand-file-name org-slipbox-directory)))
                 file))
         (title
          (with-current-buffer buffer
            (or (and (derived-mode-p 'org-mode)
                     (car (cdr (assoc "TITLE"
                                      (org-collect-keywords '("TITLE"))))))
                (file-name-base file)
                (buffer-name buffer)))))
    (if (string-empty-p title)
        path
      (format "%s | %s" title path))))

(defun consult-org-slipbox--note-buffer-p (buffer)
  "Return non-nil when BUFFER visits a file eligible for org-slipbox."
  (when-let ((file (buffer-file-name buffer)))
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (org-slipbox-file-p file)))))

(defun consult-org-slipbox--buffer-candidates ()
  "Return `consult-buffer' candidates for open slipbox note buffers."
  (let ((buffers
         (seq-filter #'consult-org-slipbox--note-buffer-p (buffer-list))))
    (setq consult-org-slipbox--buffer-alist
          (mapcar (lambda (buffer)
                    (cons (consult-org-slipbox--buffer-visible-name buffer) buffer))
                  buffers))
    (mapcar #'car consult-org-slipbox--buffer-alist)))

(defun consult-org-slipbox--candidate-buffer (candidate)
  "Return the live buffer associated with `consult-buffer' CANDIDATE."
  (cdr (assoc candidate consult-org-slipbox--buffer-alist)))

(defun consult-org-slipbox--buffer-state ()
  "Return a preview state function for the slipbox `consult-buffer' source."
  (let ((preview (consult--buffer-preview)))
    (lambda (action cand)
      (let ((buffer (and cand (consult-org-slipbox--candidate-buffer cand))))
        (funcall preview action buffer)
        (when (and buffer (eq action 'return))
          (consult--buffer-action buffer))))))

(defun consult-org-slipbox--buffer-annotation (candidate)
  "Return an annotation for the slipbox `consult-buffer' CANDIDATE."
  (when-let ((buffer (consult-org-slipbox--candidate-buffer candidate)))
    (string-join
     (delq nil
           (list (when (buffer-modified-p buffer) "modified")
                 (buffer-name buffer)))
     "  ")))

(defvar consult-org-slipbox-buffer-source nil
  "Consult source for open org-slipbox note buffers.")

(defun consult-org-slipbox--refresh-buffer-source ()
  "Refresh `consult-org-slipbox-buffer-source' from current customization."
  (setq consult-org-slipbox-buffer-source
        `(:name "Slipbox"
          :hidden nil
          :narrow ,consult-org-slipbox-buffer-narrow-key
          :category buffer
          :annotate ,#'consult-org-slipbox--buffer-annotation
          :state ,#'consult-org-slipbox--buffer-state
          :items ,#'consult-org-slipbox--buffer-candidates)))

(defun consult-org-slipbox--all-buffer-items ()
  "Return the default `consult-source-buffer' items."
  (consult--buffer-query
   :sort 'visibility
   :as #'buffer-name
   :predicate nil))

(defun consult-org-slipbox--non-slipbox-buffer-items ()
  "Return `consult-source-buffer' items excluding slipbox note buffers."
  (consult--buffer-query
   :sort 'visibility
   :as #'buffer-name
   :predicate (lambda (buffer)
                (not (consult-org-slipbox--note-buffer-p buffer)))))

(defun consult-org-slipbox--customize-source-buffer (remove-p)
  "Hide or show slipbox note buffers in `consult-source-buffer'.
When REMOVE-P is non-nil, exclude those buffers from the default source so
they only appear in `consult-org-slipbox-buffer-source'."
  (if remove-p
      (consult-customize
       consult-source-buffer
       :items #'consult-org-slipbox--non-slipbox-buffer-items)
    (consult-customize
     consult-source-buffer
     :items #'consult-org-slipbox--all-buffer-items)))

;;;###autoload
(defun consult-org-slipbox-buffer-setup ()
  "Install `consult-org-slipbox-buffer-source' into `consult-buffer-sources'."
  (interactive)
  (consult-org-slipbox--refresh-buffer-source)
  (consult-org-slipbox-buffer-teardown)
  (consult-org-slipbox--customize-source-buffer t)
  (if consult-org-slipbox-buffer-after-buffers
      (let* ((index (cl-position 'consult-source-buffer
                                 consult-buffer-sources
                                 :test #'equal))
             (tail (and index (nthcdr (1+ index) consult-buffer-sources))))
        (if index
            (setcdr (nthcdr index consult-buffer-sources)
                    (append (list 'consult-org-slipbox-buffer-source) tail))
          (add-to-list 'consult-buffer-sources
                       'consult-org-slipbox-buffer-source
                       'append)))
    (add-to-list 'consult-buffer-sources
                 'consult-org-slipbox-buffer-source
                 'append)))

;;;###autoload
(defun consult-org-slipbox-buffer-teardown ()
  "Remove `consult-org-slipbox-buffer-source' from `consult-buffer-sources'."
  (interactive)
  (setq consult-buffer-sources
        (delete 'consult-org-slipbox-buffer-source consult-buffer-sources))
  (consult-org-slipbox--customize-source-buffer nil))

(defun consult-org-slipbox--file-annotation (candidate)
  "Return the annotation string for file CANDIDATE."
  (let* ((record (consult-org-slipbox--candidate-value candidate))
         (path (consult-org-slipbox--relative-file record))
         (nodes (or (plist-get record :node_count) 0))
         (mtime (consult-org-slipbox--format-time (plist-get record :mtime_ns))))
    (string-join
     (delq nil
           (list (unless (string-empty-p path) path)
                 (format "%d %s" nodes
                         (consult-org-slipbox--pluralize nodes "node"))
                 mtime))
     "  ")))

(defun consult-org-slipbox--location-annotation (candidate)
  "Return a default annotation for location CANDIDATE."
  (when-let* ((node (consult-org-slipbox--candidate-related-node candidate))
              (title (consult-org-slipbox--node-title node))
              ((not (string-empty-p title))))
    (format " %s" title)))

(defun consult-org-slipbox--reflink-annotation (candidate)
  "Return the annotation for reflink CANDIDATE."
  (let* ((record (consult-org-slipbox--candidate-value candidate))
         (reference (plist-get record :matched_reference))
         (base (consult-org-slipbox--location-annotation candidate)))
    (string-join (delq nil (list base reference)) "  ")))

(defun consult-org-slipbox--unlinked-reference-annotation (candidate)
  "Return the annotation for unlinked-reference CANDIDATE."
  (let* ((record (consult-org-slipbox--candidate-value candidate))
         (match (plist-get record :matched_text))
         (base (consult-org-slipbox--location-annotation candidate)))
    (string-join (delq nil (list base match)) "  ")))

(defun consult-org-slipbox--node-candidates (input filter-fn sort)
  "Return Consult candidates for INPUT, FILTER-FN, and SORT."
  (cl-loop for (display . node) in (org-slipbox-node-completion-candidates
                                    input filter-fn sort)
           for index from 0
           collect
           (consult-org-slipbox--decorate-candidate
            (concat (copy-sequence display) (consult--tofu-encode index))
            node
            (consult-org-slipbox--record-target node)
            node)))

(defun consult-org-slipbox--ref-candidates (input filter-fn)
  "Return Consult ref candidates for INPUT and FILTER-FN."
  (cl-loop for (display . node) in (org-slipbox-ref-completion-candidates
                                    input filter-fn)
           for index from 0
           collect
           (consult-org-slipbox--decorate-candidate
            (concat (copy-sequence display) (consult--tofu-encode index))
            node
            (consult-org-slipbox--record-target node)
            node)))

(defun consult-org-slipbox--file-candidate-label (record)
  "Return the visible file label for RECORD."
  (let ((title (string-trim (or (plist-get record :title) "")))
        (path (consult-org-slipbox--relative-file record)))
    (if (string-empty-p title)
        path
      (format "%s | %s" title path))))

(defun consult-org-slipbox--file-candidates (input)
  "Return Consult file candidates for INPUT."
  (cl-loop for record in (org-slipbox-search-files input org-slipbox-node-read-limit)
           for index from 0
           collect
           (consult-org-slipbox--decorate-candidate
            (concat (consult-org-slipbox--file-candidate-label record)
                    (consult--tofu-encode index))
            record
            (consult-org-slipbox--record-target record)
            nil
            (consult-org-slipbox--relative-file record))))

(defun consult-org-slipbox--location-label (file row preview)
  "Return a Consult-style location label for FILE, ROW, and PREVIEW."
  (consult--format-file-line-match file row preview))

(defun consult-org-slipbox--occurrence-target (record)
  "Return a visit target plist for occurrence-like RECORD."
  (list :file_path (plist-get record :file_path)
        :line (plist-get record :row)
        :col (plist-get record :col)))

(defun consult-org-slipbox--location-candidate
    (record index &optional related-node annotation-key)
  "Return a location candidate for RECORD with INDEX.
RELATED-NODE is stored for follow-up actions.  ANNOTATION-KEY is retained on
the record plist for custom annotation functions."
  (let* ((file (plist-get record :file_path))
         (row (plist-get record :row))
         (preview (plist-get record :preview))
         (candidate
          (concat (consult-org-slipbox--location-label file row preview)
                  (consult--tofu-encode index))))
    (when annotation-key
      (setq record (plist-put (copy-sequence record)
                              :consult-org-slipbox-annotation-key
                              annotation-key)))
    (consult-org-slipbox--decorate-candidate
     candidate
     record
     (consult-org-slipbox--occurrence-target record)
     related-node
     file)))

(defun consult-org-slipbox--occurrence-candidates (input)
  "Return occurrence candidates for INPUT."
  (when (>= (length (string-trim input)) consult-org-slipbox-search-min-input)
    (cl-loop for record in (org-slipbox-search-occurrences
                            input consult-org-slipbox-search-limit)
             for index from 0
             collect
             (consult-org-slipbox--location-candidate
              record index (plist-get record :owning_node)))))

(defun consult-org-slipbox--backlink-candidates (records)
  "Return Consult backlink candidates from RECORDS."
  (cl-loop for record in records
           for index from 0
           collect
           (consult-org-slipbox--location-candidate
            (list :file_path (plist-get (plist-get record :source_node) :file_path)
                  :row (plist-get record :row)
                  :col (plist-get record :col)
                  :preview (plist-get record :preview))
            index
            (plist-get record :source_node))))

(defun consult-org-slipbox--forward-link-candidates (records)
  "Return Consult forward-link candidates from RECORDS."
  (cl-loop for record in records
           for index from 0
           collect
           (consult-org-slipbox--location-candidate
            (list :file_path (plist-get (plist-get record :destination_node) :file_path)
                  :row (plist-get record :row)
                  :col (plist-get record :col)
                  :preview (plist-get record :preview))
            index
            (plist-get record :destination_node))))

(defun consult-org-slipbox--reflink-candidates (records)
  "Return Consult reflink candidates from RECORDS."
  (cl-loop for record in records
           for index from 0
           collect
           (consult-org-slipbox--location-candidate
            (list :file_path (plist-get (plist-get record :source_node) :file_path)
                  :row (plist-get record :row)
                  :col (plist-get record :col)
                  :preview (plist-get record :preview)
                  :matched_reference (plist-get record :matched_reference))
            index
            (plist-get record :source_node)
            :matched_reference)))

(defun consult-org-slipbox--unlinked-reference-candidates (records)
  "Return Consult unlinked-reference candidates from RECORDS."
  (cl-loop for record in records
           for index from 0
           collect
           (consult-org-slipbox--location-candidate
            (list :file_path (plist-get (plist-get record :source_node) :file_path)
                  :row (plist-get record :row)
                  :col (plist-get record :col)
                  :preview (plist-get record :preview)
                  :matched_text (plist-get record :matched_text))
            index
            (plist-get record :source_node)
            :matched_text)))

(defun consult-org-slipbox--current-or-read-node (prompt)
  "Return the current indexed node, or read one using PROMPT."
  (or (org-slipbox-node-at-point)
      (consult-org-slipbox-node-read nil nil nil t prompt)))

(defun consult-org-slipbox--read-location
    (prompt candidates annotation-function &optional initial-input)
  "Read a location candidate with PROMPT from CANDIDATES.
ANNOTATION-FUNCTION annotates each candidate.  INITIAL-INPUT seeds the
minibuffer when non-nil."
  (when candidates
    (consult--read
     candidates
     :prompt prompt
     :initial initial-input
     :sort nil
     :require-match t
     :category 'org-slipbox-location
     :history 'consult-org-slipbox-location-history
     :group #'consult--prefix-group
     :annotate annotation-function
     :lookup #'consult--lookup-candidate
     :state (consult-org-slipbox--preview-state
             #'consult-org-slipbox--candidate-target))))

;;;###autoload
(defun consult-org-slipbox-node-read
    (&optional initial-input filter-fn sort-fn require-match prompt)
  "Read and return an indexed org-slipbox node with Consult preview.
INITIAL-INPUT seeds the minibuffer.  FILTER-FN filters indexed nodes.
SORT-FN names an engine-backed sort or provides a custom comparator.
REQUIRE-MATCH enforces an indexed selection.  PROMPT defaults to \"Node: \".
When REQUIRE-MATCH is nil and the user enters a new title, return a plist with
only `:title'."
  (let* ((prompt (or prompt "Node: "))
         (collection
          (consult--dynamic-collection
           (lambda (input)
             (consult-org-slipbox--node-candidates input filter-fn sort-fn))
           :min-input 0
           :throttle consult-org-slipbox-dynamic-throttle
           :debounce consult-org-slipbox-dynamic-debounce))
         (selection
          (consult--read
           collection
           :prompt prompt
           :initial initial-input
           :sort nil
           :require-match require-match
           :category 'org-slipbox-node
           :history 'org-slipbox-node-history
           :annotate #'org-slipbox-node-completion-annotation
           :lookup #'consult-org-slipbox--candidate-lookup
           :state (consult-org-slipbox--preview-state
                   #'consult-org-slipbox--candidate-target))))
    (cond
     ((and (listp selection) (plist-get selection :file_path)) selection)
     ((stringp selection)
      (and (not require-match)
           (not (string-empty-p selection))
           (list :title selection)))
     (t selection))))

;;;###autoload
(defun consult-org-slipbox-ref-read (&optional initial-input filter-fn prompt)
  "Read and return an indexed node selected through its ref.
INITIAL-INPUT seeds the minibuffer.  FILTER-FN filters nodes attached to refs.
PROMPT defaults to \"Ref: \"."
  (let* ((prompt (or prompt "Ref: "))
         (collection
          (consult--dynamic-collection
           (lambda (input)
             (consult-org-slipbox--ref-candidates input filter-fn))
           :min-input 0
           :throttle consult-org-slipbox-dynamic-throttle
           :debounce consult-org-slipbox-dynamic-debounce)))
    (consult--read
     collection
     :prompt prompt
     :initial initial-input
     :sort nil
     :require-match t
     :category 'org-slipbox-ref
     :history 'org-slipbox-ref-history
     :annotate #'org-slipbox-ref-completion-annotation
     :lookup #'consult--lookup-candidate
     :state (consult-org-slipbox--preview-state
             #'consult-org-slipbox--candidate-target))))

(defmacro consult-org-slipbox--with-read-overrides (&rest body)
  "Evaluate BODY with org-slipbox completion readers overridden."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'org-slipbox-node-read)
              #'consult-org-slipbox-node-read)
             ((symbol-function 'org-slipbox-ref-read)
              #'consult-org-slipbox-ref-read))
     ,@body))

;;;###autoload
(defun consult-org-slipbox-node-find (&optional other-window)
  "Find an org-slipbox node using Consult.
With OTHER-WINDOW, visit the result in another window."
  (interactive "P")
  (consult-org-slipbox--with-read-overrides
    (org-slipbox-node-find nil nil other-window)))

;;;###autoload
(defun consult-org-slipbox-node-insert ()
  "Insert an `id:' link to a selected org-slipbox node using Consult."
  (interactive)
  (consult-org-slipbox--with-read-overrides
    (org-slipbox-node-insert)))

(defun consult-org-slipbox--read-file (&optional initial-input)
  "Read and return an indexed file record.
INITIAL-INPUT seeds the minibuffer."
  (let ((collection
         (consult--dynamic-collection
          #'consult-org-slipbox--file-candidates
          :min-input 0
          :throttle consult-org-slipbox-dynamic-throttle
          :debounce consult-org-slipbox-dynamic-debounce)))
    (consult--read
     collection
     :prompt "File: "
     :initial initial-input
     :sort nil
     :require-match t
     :category 'org-slipbox-file
     :history 'consult-org-slipbox-file-history
     :annotate #'consult-org-slipbox--file-annotation
     :lookup #'consult--lookup-candidate
     :state (consult-org-slipbox--preview-state
             #'consult-org-slipbox--candidate-target))))

;;;###autoload
(defun consult-org-slipbox-file-find (&optional other-window initial-input)
  "Find an indexed org-slipbox file with Consult preview.
With OTHER-WINDOW, visit the selected file in another window.
INITIAL-INPUT seeds the minibuffer when called non-interactively."
  (interactive "P")
  (when-let ((record (consult-org-slipbox--read-file initial-input)))
    (consult-org-slipbox--visit-target
     (consult-org-slipbox--record-target record)
     other-window)
    record))

;;;###autoload
(defun consult-org-slipbox-ref-find (&optional other-window initial-input)
  "Find and visit an indexed ref-backed node with Consult preview.
With OTHER-WINDOW, visit the selected node in another window.
INITIAL-INPUT seeds the minibuffer when called non-interactively."
  (interactive "P")
  (let ((node (consult-org-slipbox-ref-read initial-input nil "Ref: ")))
    (when node
      (org-slipbox-node-visit node other-window)
      node)))

;;;###autoload
(defun consult-org-slipbox-search (&optional other-window initial-input)
  "Search indexed occurrence hits with Consult preview.
With OTHER-WINDOW, visit the selected occurrence in another window.
INITIAL-INPUT seeds the minibuffer when called non-interactively."
  (interactive "P")
  (let* ((collection
          (consult--dynamic-collection
           #'consult-org-slipbox--occurrence-candidates
           :min-input consult-org-slipbox-search-min-input
           :throttle consult-org-slipbox-dynamic-throttle
           :debounce consult-org-slipbox-dynamic-debounce))
         (selection
          (consult--read
           collection
           :prompt "Search slipbox: "
           :initial initial-input
           :sort nil
           :require-match t
           :category 'org-slipbox-occurrence
           :history '(:input consult-org-slipbox-search-history)
           :add-history (delq nil (list (thing-at-point 'symbol) isearch-string))
           :annotate #'consult-org-slipbox--location-annotation
           :group #'consult--prefix-group
           :lookup #'consult--lookup-candidate
           :state (consult-org-slipbox--preview-state
                   #'consult-org-slipbox--candidate-target))))
    (when selection
      (consult-org-slipbox--visit-target
       (consult-org-slipbox--occurrence-target selection)
       other-window)
      selection)))

;;;###autoload
(defun consult-org-slipbox-backlinks (&optional other-window)
  "Select a backlink occurrence for the current or chosen node.
With OTHER-WINDOW, visit the selected occurrence in another window."
  (interactive "P")
  (let* ((node (consult-org-slipbox--current-or-read-node "Backlinks for node: "))
         (records (consult-org-slipbox--plist-sequence
                   (plist-get
                    (org-slipbox-rpc-backlinks
                     (plist-get node :node_key)
                     consult-org-slipbox-link-limit)
                    :backlinks))))
    (unless records
      (user-error "No backlinks found"))
    (when-let ((selection
                (consult-org-slipbox--read-location
                 (format "Backlinks for %s: " (consult-org-slipbox--node-title node))
                 (consult-org-slipbox--backlink-candidates records)
                 #'consult-org-slipbox--location-annotation)))
      (consult-org-slipbox--visit-target
       (consult-org-slipbox--occurrence-target selection)
       other-window)
      selection)))

;;;###autoload
(defun consult-org-slipbox-forward-links (&optional other-window)
  "Select a forward-link occurrence for the current or chosen node.
With OTHER-WINDOW, visit the selected occurrence in another window."
  (interactive "P")
  (let* ((node (consult-org-slipbox--current-or-read-node "Forward links for node: "))
         (records (consult-org-slipbox--plist-sequence
                   (plist-get
                    (org-slipbox-rpc-forward-links
                     (plist-get node :node_key)
                     consult-org-slipbox-link-limit)
                    :forward_links))))
    (unless records
      (user-error "No forward links found"))
    (when-let ((selection
                (consult-org-slipbox--read-location
                 (format "Forward links for %s: " (consult-org-slipbox--node-title node))
                 (consult-org-slipbox--forward-link-candidates records)
                 #'consult-org-slipbox--location-annotation)))
      (consult-org-slipbox--visit-target
       (consult-org-slipbox--occurrence-target selection)
       other-window)
      selection)))

;;;###autoload
(defun consult-org-slipbox-reflinks (&optional other-window)
  "Select a reflink occurrence for the current or chosen node.
With OTHER-WINDOW, visit the selected occurrence in another window."
  (interactive "P")
  (let* ((node (consult-org-slipbox--current-or-read-node "Reflinks for node: "))
         (records (consult-org-slipbox--plist-sequence
                   (plist-get
                    (org-slipbox-rpc-reflinks
                     (plist-get node :node_key)
                     consult-org-slipbox-link-limit)
                    :reflinks))))
    (unless records
      (user-error "No reflinks found"))
    (when-let ((selection
                (consult-org-slipbox--read-location
                 (format "Reflinks for %s: " (consult-org-slipbox--node-title node))
                 (consult-org-slipbox--reflink-candidates records)
                 #'consult-org-slipbox--reflink-annotation)))
      (consult-org-slipbox--visit-target
       (consult-org-slipbox--occurrence-target selection)
       other-window)
      selection)))

;;;###autoload
(defun consult-org-slipbox-unlinked-references (&optional other-window)
  "Select an unlinked-reference occurrence for the current or chosen node.
With OTHER-WINDOW, visit the selected occurrence in another window."
  (interactive "P")
  (let* ((node (consult-org-slipbox--current-or-read-node
                "Unlinked references for node: "))
         (records (consult-org-slipbox--plist-sequence
                   (plist-get
                    (org-slipbox-rpc-unlinked-references
                     (plist-get node :node_key)
                     consult-org-slipbox-link-limit)
                    :unlinked_references))))
    (unless records
      (user-error "No unlinked references found"))
    (when-let ((selection
                (consult-org-slipbox--read-location
                 (format "Unlinked references for %s: "
                         (consult-org-slipbox--node-title node))
                 (consult-org-slipbox--unlinked-reference-candidates records)
                 #'consult-org-slipbox--unlinked-reference-annotation)))
      (consult-org-slipbox--visit-target
       (consult-org-slipbox--occurrence-target selection)
       other-window)
      selection)))

(defun consult-org-slipbox--node-action (candidate &optional other-window)
  "Visit the node stored on CANDIDATE, optionally in OTHER-WINDOW."
  (when-let ((node (consult-org-slipbox--candidate-related-node candidate)))
    (consult-org-slipbox--with-origin-window
      (org-slipbox-node-visit node other-window))
    node))

(defun consult-org-slipbox--location-action (candidate &optional other-window)
  "Visit the location stored on CANDIDATE, optionally in OTHER-WINDOW."
  (when-let ((target (consult-org-slipbox--candidate-target candidate)))
    (consult-org-slipbox--with-origin-window
      (consult-org-slipbox--visit-target target other-window))
    target))

(defun consult-org-slipbox--insert-node-action (candidate)
  "Insert a link to the node stored on CANDIDATE."
  (when-let ((node (consult-org-slipbox--candidate-related-node candidate)))
    (consult-org-slipbox--with-origin-window
        (let ((node-with-id
               (or (and (plist-get node :explicit_id) node)
                   (org-slipbox-rpc-ensure-node-id (plist-get node :node_key)))))
        (org-slipbox-node-insert-link
         node-with-id
         (org-slipbox-node-formatted node-with-id))
        node))))

(defun consult-org-slipbox--copy-node-id-action (candidate)
  "Copy the node ID from CANDIDATE."
  (when-let* ((node (consult-org-slipbox--candidate-related-node candidate))
              (explicit-id (or (plist-get node :explicit_id)
                               (plist-get
                                (org-slipbox-rpc-ensure-node-id
                                 (plist-get node :node_key))
                                :explicit_id))))
    (kill-new explicit-id)
    (message "Copied org-slipbox node ID %s" explicit-id)
    explicit-id))

(defun consult-org-slipbox--open-buffer-action (candidate)
  "Open the dedicated org-slipbox buffer for CANDIDATE."
  (when-let ((node (consult-org-slipbox--candidate-related-node candidate)))
    (consult-org-slipbox--with-origin-window
      (org-slipbox-buffer-display-dedicated node))
    node))

(with-eval-after-load 'embark
  (defvar embark-keymap-alist)
  (defvar-keymap consult-org-slipbox-node-actions-map
    :doc "Embark actions for org-slipbox node candidates."
    "RET" #'consult-org-slipbox--node-action
    "o"   (lambda (candidate)
            (interactive "sNode: ")
            (consult-org-slipbox--node-action candidate t))
    "i"   #'consult-org-slipbox--insert-node-action
    "b"   #'consult-org-slipbox--open-buffer-action
    "y"   #'consult-org-slipbox--copy-node-id-action)

  (defvar-keymap consult-org-slipbox-file-actions-map
    :doc "Embark actions for org-slipbox file candidates."
    "RET" #'consult-org-slipbox--location-action
    "o"   (lambda (candidate)
            (interactive "sFile: ")
            (consult-org-slipbox--location-action candidate t)))

  (defvar-keymap consult-org-slipbox-location-actions-map
    :doc "Embark actions for org-slipbox location candidates."
    "RET" #'consult-org-slipbox--location-action
    "o"   (lambda (candidate)
            (interactive "sLocation: ")
            (consult-org-slipbox--location-action candidate t))
    "b"   #'consult-org-slipbox--open-buffer-action)

  (add-to-list 'embark-keymap-alist
               '(org-slipbox-node . consult-org-slipbox-node-actions-map))
  (add-to-list 'embark-keymap-alist
               '(org-slipbox-ref . consult-org-slipbox-node-actions-map))
  (add-to-list 'embark-keymap-alist
               '(org-slipbox-file . consult-org-slipbox-file-actions-map))
  (add-to-list 'embark-keymap-alist
               '(org-slipbox-occurrence . consult-org-slipbox-location-actions-map))
  (add-to-list 'embark-keymap-alist
               '(org-slipbox-location . consult-org-slipbox-location-actions-map)))

;;;###autoload
(define-minor-mode consult-org-slipbox-mode
  "Toggle Consult integration for org-slipbox readers and buffer sources."
  :global t
  :lighter " cslip"
  (if consult-org-slipbox-mode
      (progn
        (when consult-org-slipbox-buffer-enabled
          (consult-org-slipbox-buffer-setup))
        (advice-add #'org-slipbox-node-read :override #'consult-org-slipbox-node-read)
        (advice-add #'org-slipbox-ref-read :override #'consult-org-slipbox-ref-read))
    (consult-org-slipbox-buffer-teardown)
    (advice-remove #'org-slipbox-node-read #'consult-org-slipbox-node-read)
    (advice-remove #'org-slipbox-ref-read #'consult-org-slipbox-ref-read)))

(provide 'consult-org-slipbox)

;;; consult-org-slipbox.el ends here
