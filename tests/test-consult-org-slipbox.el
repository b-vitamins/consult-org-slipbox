;;; test-consult-org-slipbox.el --- Tests for consult-org-slipbox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 org-slipbox contributors

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT coverage for consult-org-slipbox.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (package-root (expand-file-name ".." test-dir))
       (org-slipbox-root
        (or (getenv "ORG_SLIPBOX_DIR")
            (expand-file-name "../../org-slipbox" package-root))))
  (add-to-list 'load-path package-root)
  (add-to-list 'load-path org-slipbox-root))

(require 'consult-org-slipbox)

(ert-deftest consult-org-slipbox-test-node-read-returns-selected-node ()
  "Node reader should resolve Consult selections back to node plists."
  (let ((node '(:node_key "heading:alpha.org:3"
                :file_path "alpha.org"
                :title "Alpha"
                :line 3)))
    (cl-letf* (((symbol-function 'consult--dynamic-collection)
                (lambda (fun &rest _keys) fun))
               ((symbol-function 'org-slipbox-node-completion-candidates)
                (lambda (_input _filter _sort)
                  (list (cons (propertize "Alpha | alpha.org:3" 'node node) node))))
               ((symbol-function 'consult--read)
                (lambda (collection &rest plist)
                  (let* ((candidates (funcall collection "Alpha"))
                         (lookup (plist-get plist :lookup)))
                    (funcall lookup (car candidates) candidates "Alpha" nil)))))
      (should (equal (consult-org-slipbox-node-read) node)))))

(ert-deftest consult-org-slipbox-test-node-read-returns-new-title-when-allowed ()
  "Node reader should preserve new titles when matches are not required."
  (cl-letf* (((symbol-function 'consult--dynamic-collection)
              (lambda (fun &rest _keys) fun))
             ((symbol-function 'org-slipbox-node-completion-candidates)
              (lambda (_input _filter _sort) nil))
             ((symbol-function 'consult--read)
              (lambda (_collection &rest plist)
                (let ((lookup (plist-get plist :lookup)))
                  (funcall lookup "Fresh note" nil "Fresh note" nil)))))
    (should (equal (consult-org-slipbox-node-read nil nil nil nil)
                   '(:title "Fresh note")))))

(ert-deftest consult-org-slipbox-test-ref-read-returns-selected-node ()
  "Ref reader should resolve Consult selections back to node plists."
  (let ((node '(:node_key "heading:refs.org:5"
                :file_path "refs.org"
                :title "Smith"
                :line 5)))
    (cl-letf* (((symbol-function 'consult--dynamic-collection)
                (lambda (fun &rest _keys) fun))
               ((symbol-function 'org-slipbox-ref-completion-candidates)
                (lambda (_input _filter)
                  (list (cons (propertize "@smith2024"
                                          'org-slipbox-ref-node node)
                              node))))
               ((symbol-function 'consult--read)
                (lambda (collection &rest plist)
                  (let* ((candidates (funcall collection "@smith"))
                         (lookup (plist-get plist :lookup)))
                    (funcall lookup (car candidates) candidates "@smith" nil)))))
      (should (equal (consult-org-slipbox-ref-read) node)))))

(ert-deftest consult-org-slipbox-test-file-candidates-carry-records-and-annotation ()
  "File candidates should preserve indexed metadata for selection and display."
  (let* ((record '(:file_path "notes/alpha.org"
                   :title "Alpha"
                   :mtime_ns 1741881600000000000
                   :node_count 3))
         (candidate
          (cl-letf* (((symbol-function 'org-slipbox-search-files)
                      (lambda (_query _limit) (list record))))
            (car (consult-org-slipbox--file-candidates "alpha")))))
    (should (equal (consult-org-slipbox--candidate-value candidate) record))
    (should (equal (plist-get (consult-org-slipbox--candidate-target candidate) :file_path)
                   "notes/alpha.org"))
    (should (string-match-p "Alpha | notes/alpha\\.org" candidate))
    (let ((annotation (consult-org-slipbox--file-annotation candidate)))
      (should (string-match-p "notes/alpha\\.org" annotation))
      (should (string-match-p "3 nodes" annotation)))))

(ert-deftest consult-org-slipbox-test-file-find-visits-selected-file ()
  "File find should visit the selected indexed file."
  (let ((selected-target nil))
    (cl-letf* (((symbol-function 'consult-org-slipbox--read-file)
                (lambda (&optional _initial)
                  '(:file_path "notes/alpha.org"
                    :title "Alpha"
                    :mtime_ns 1
                    :node_count 1)))
               ((symbol-function 'consult-org-slipbox--visit-target)
                (lambda (target &optional other-window)
                  (setq selected-target (list target other-window)))))
      (consult-org-slipbox-file-find t)
      (should
       (equal selected-target
              (list '(:file_path "notes/alpha.org" :line 1 :col 1) t))))))

(ert-deftest consult-org-slipbox-test-search-visits-selected-occurrence ()
  "Occurrence search should visit the selected indexed hit."
  (let ((selected-target nil))
    (cl-letf* (((symbol-function 'consult--dynamic-collection)
                (lambda (fun &rest _keys) fun))
               ((symbol-function 'org-slipbox-search-occurrences)
                (lambda (_query _limit)
                  (list '(:file_path "notes/alpha.org"
                          :row 9
                          :col 5
                          :preview "Alpha preview"
                          :matched_text "Alpha"
                          :owning_node (:title "Alpha")))))
               ((symbol-function 'consult--read)
                (lambda (collection &rest plist)
                  (let* ((candidates (funcall collection "Alpha"))
                         (lookup (plist-get plist :lookup)))
                    (funcall lookup (car candidates) candidates "Alpha" nil))))
               ((symbol-function 'consult-org-slipbox--visit-target)
                (lambda (target &optional other-window)
                  (setq selected-target (list target other-window)))))
      (consult-org-slipbox-search t "Alpha")
      (should
       (equal selected-target
              (list '(:file_path "notes/alpha.org" :line 9 :col 5) t))))))

(ert-deftest consult-org-slipbox-test-backlinks-use-current-node-and-visit-selection ()
  "Backlink command should query the node at point and visit the chosen hit."
  (let ((visited nil))
    (cl-letf* (((symbol-function 'org-slipbox-node-at-point)
                (lambda (&optional _assert)
                  '(:node_key "heading:alpha.org:3" :title "Alpha")))
               ((symbol-function 'org-slipbox-rpc-backlinks)
                (lambda (node-key limit)
                  (should (equal node-key "heading:alpha.org:3"))
                  (should (= limit consult-org-slipbox-link-limit))
                  '(:backlinks
                    [(:source_node (:title "Beta"
                                   :file_path "beta.org")
                      :row 12
                      :col 6
                      :preview "Beta preview")])))
               ((symbol-function 'consult-org-slipbox--read-location)
                (lambda (_prompt candidates _annotate &optional _initial)
                  (consult-org-slipbox--candidate-value (car candidates))))
               ((symbol-function 'consult-org-slipbox--visit-target)
                (lambda (target &optional other-window)
                  (setq visited (list target other-window)))))
      (consult-org-slipbox-backlinks t)
      (should
       (equal visited
              (list '(:file_path "beta.org" :line 12 :col 6) t))))))

(ert-deftest consult-org-slipbox-test-buffer-candidates-list-open-slipbox-notes ()
  "Buffer candidates should list open Org note buffers under the slipbox root."
  (let* ((org-slipbox-directory (make-temp-file "consult-org-slipbox-root" t))
         (file (expand-file-name "notes/alpha.org" org-slipbox-directory))
         (buffer (generate-new-buffer " alpha")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-current-buffer buffer
            (setq buffer-file-name file)
            (org-mode)
            (insert "#+title: Alpha\n\nBody\n"))
          (should
           (equal (consult-org-slipbox--buffer-candidates)
                  '("Alpha | notes/alpha.org")))
          (should
           (eq (consult-org-slipbox--candidate-buffer "Alpha | notes/alpha.org")
               buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest consult-org-slipbox-test-buffer-setup-and-teardown-manage-source ()
  "Buffer setup and teardown should update `consult-buffer-sources'."
  (let ((consult-buffer-sources '(consult-source-buffer consult-source-recent-file))
        (consult-org-slipbox-buffer-after-buffers nil))
    (cl-letf (((symbol-function 'consult-customize) #'ignore))
      (consult-org-slipbox-buffer-setup)
      (should (equal consult-buffer-sources
                     '(consult-source-buffer
                       consult-source-recent-file
                       consult-org-slipbox-buffer-source)))
      (consult-org-slipbox-buffer-teardown)
      (should (equal consult-buffer-sources
                     '(consult-source-buffer consult-source-recent-file))))))

(ert-deftest consult-org-slipbox-test-buffer-setup-can-place-source-after-buffers ()
  "Buffer setup should honor `consult-org-slipbox-buffer-after-buffers'."
  (let ((consult-buffer-sources
         '(consult-source-buffer consult-source-recent-file consult-source-project-buffer))
        (consult-org-slipbox-buffer-after-buffers t))
    (cl-letf (((symbol-function 'consult-customize) #'ignore))
      (consult-org-slipbox-buffer-setup)
      (should (equal consult-buffer-sources
                     '(consult-source-buffer
                       consult-org-slipbox-buffer-source
                       consult-source-recent-file
                       consult-source-project-buffer)))
      (consult-org-slipbox-buffer-teardown))))

(ert-deftest consult-org-slipbox-test-node-find-wraps-org-slipbox-command ()
  "Direct node-find should route through the overridden reader."
  (let ((reader nil)
        (other-window nil))
    (cl-letf* (((symbol-function 'org-slipbox-node-find)
                (lambda (_initial _filter arg)
                  (setq reader (symbol-function 'org-slipbox-node-read)
                        other-window arg)
                  :ok)))
      (should (eq (consult-org-slipbox-node-find t) :ok))
      (should (eq reader #'consult-org-slipbox-node-read))
      (should other-window))))

(ert-deftest consult-org-slipbox-test-mode-overrides-readers ()
  "Minor mode should add and remove the consult reader overrides and source."
  (let ((consult-buffer-sources '(consult-source-buffer)))
    (cl-letf (((symbol-function 'consult-customize) #'ignore))
      (unwind-protect
          (progn
            (consult-org-slipbox-mode 1)
            (should (advice-member-p #'consult-org-slipbox-node-read #'org-slipbox-node-read))
            (should (advice-member-p #'consult-org-slipbox-ref-read #'org-slipbox-ref-read))
            (should (memq 'consult-org-slipbox-buffer-source consult-buffer-sources))
            (consult-org-slipbox-mode -1)
            (should-not (advice-member-p #'consult-org-slipbox-node-read #'org-slipbox-node-read))
            (should-not (advice-member-p #'consult-org-slipbox-ref-read #'org-slipbox-ref-read))
            (should-not (memq 'consult-org-slipbox-buffer-source consult-buffer-sources)))
        (consult-org-slipbox-mode -1)))))

(provide 'test-consult-org-slipbox)

;;; test-consult-org-slipbox.el ends here
