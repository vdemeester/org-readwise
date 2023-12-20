;;; org-readwise.el --- A readwise to org package -*- lexical-binding: t -*-

;; Copyright (C) 2023 Vincent Demeester

;; Author: Vincent Demeester <vincent@sbr.pm>
;; Version: 0.1
;; Package-Requires: ((emacs "27.2") (request "0.3.3") (dash "2.19.1"))
;; URL: https://github.com/vdemeester/org-readwise

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.
;;; Commentary:
;;
;; This package is a simple package to import readwise.io highlights as org-mode file.
;; It should evolve over time, but to start with it should include the following
;; - Synchronize readwise highlight in `org-mode'.
;; In the future, it should:
;;- Mimic readwise plugin, aka add entries under a date when synced, …

;;; Code:

(require 'request)
(require 'dash)

(defconst org-readwise-url "https://readwise.io/api/v2/" "URL for Readwise API.")

(defgroup org-readwise ()
  "Readwise integration for Emacs."
  ;; or :group 'org and :prefix org-readwise- ?
  :group 'readwise)

(defcustom org-readwise-sync-db-path (expand-file-name "~/.config/emacs/org-readwise-last-sync")
  "Path to the file where the last sync is stored."
  :type 'string
  :group 'readwise)

(defcustom org-readwise-api-token nil
  "Readwise API key."
  :type 'string
  :group 'readwise)

(defun org-readwise--save-last-sync ()
  "Save the most recent sync time."
  (let ((ts (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))
    (with-temp-file org-readwise-sync-db-path
      (erase-buffer)
      (insert ts))))

(defun org-readwise--get-last-sync ()
  "Retrieve the ISO 8601 timestamp of the last sync time."
  (if (file-exists-p org-readwise-sync-db-path)
      (with-temp-buffer
        (insert-file-contents org-readwise-sync-db-path)
        (buffer-string))
    nil))

;;;##autoload
(defun org-readwise--array-to-list (array)
  "Coerce ARRAY to a list."
  (-map #'identity array))

(defun org-readwise--fetch-highlights (api-key db-path &optional cursor since more)
  "Exhaustively fetch highlights from Readwise using API-KEY.
Saves each to the appropriate `org-mode' files.
API-KEY is the Readwise API key.
DB-PATH is the path to the file where the last sync time is stored.
CURSOR is the pagination cursor for fetching the next page of results.
SINCE is the ISO 8601 timestamp to limit highlights.
MORE indicates that there are more results to fetch."
  (let* ((params (append (when since `(("updatedAfter" . ,since)))
                         (when cursor `(("pageCursor" . ,cursor))))))
    (when more
      (request (concat org-readwise-url "export/")
        :type "GET"
        :params params
        :parser 'json-read
        :headers `(("Authorization" . ,(concat "Token " org-readwise-api-token)))
        :error (cl-function (lambda (_)
                              (message "Error fetching highlights from Readwise. Check your API key and try again.")))
        :success (cl-function (lambda (&key data &allow-other-keys)
                                (let-alist data
                                  (org-readwise--add-highlights (org-readwise--array-to-list .results))
                                  (if .nextPageCursor
                                      (org-readwise--fetch-highlights api-key db-path .nextPageCursor since 't)
                                    (org-readwise--save-last-sync)))))))))

;; ((user_book_id . 32654791) (title . A Practical Guide to
;; Distributed Scrum (Adobe Reader) (IBM Press)) (author . Elizabeth
;; Woodward, Steffan Surdek, and Matthew Ganis) (readable_title . A
;; Practical Guide to Distributed Scrum) (source . kindle)
;; (cover_image_url
;; . https://images-na.ssl-images-amazon.com/images/I/51CgeYpj0zL._SL200_.jpg)
;; (unique_url) (book_tags . []) (category . books) (document_note)
;; (readwise_url . https://readwise.io/bookreview/32654791)
;; (source_url) (asin . B003V4ATRQ) (highlights . [((id . 602597892)
;; (text . The ScrumMaster is a servant to the team.) (location . 766)
;; (location_type . location) (note . ) (color . yellow)
;; (highlighted_at . 2020-11-22T04:56:00Z) (created_at
;; . 2023-09-29T15:37:11.108Z) (updated_at . 2023-09-29T15:37:11.108Z)
;; (external_id) (end_location) (url) (book_id . 32654791) (tags . [])
;; (is_favorite . :json-false) (is_discard . :json-false)
;; (readwise_url . https://readwise.io/open/602597892)) ((id
;; . 602597893) (text . The Scrum Product Owner is the person who owns
;; the Product Backlog (a list of stakeholder needs for the product),
;; ranks the work items by priority, and defines the acceptance
;; conditions. The Product Owner is responsible for everything that
;; goes into the product release.) (location . 769) (location_type
;; . location) (note . ) (color . yellow) (highlighted_at
;; . 2020-11-22T04:56:00Z) (created_at . 2023-09-29T15:37:11.108Z)
;; (updated_at . 2023-09-29T15:37:11.108Z) (external_id)
;; (end_location) (url) (book_id . 32654791) (tags . []) (is_favorite
;; . :json-false) (is_discard . :json-false) (readwise_url
;; . https://readwise.io/open/602597893)) ((id . 602597894) (text
;; . User Stories typically follow the format “As a <role>, I need to
;; <goal> so that <business value>.”) (location . 791) (location_type
;; . location) (note . ) (color . yellow) (highlighted_at
;; . 2020-11-22T04:56:00Z) (created_at . 2023-09-29T15:37:11.109Z)
;; (updated_at . 2023-09-29T15:37:11.109Z) (external_id)
;; (end_location) (url) (book_id . 32654791) (tags . []) (is_favorite
;; . :json-false) (is_discard . :json-false) (readwise_url
;; . https://readwise.io/open/602597894)) ((id . 602597895) (text
;; . Although the Product Owner considers technical input from the
;; Architecture Owner, the Product Owner is ultimately responsible for
;; prioritization.) (location . 802) (location_type . location) (note
;; . ) (color . yellow) (highlighted_at . 2020-11-22T04:56:00Z)
;; (created_at . 2023-09-29T15:37:11.109Z) (updated_at
;; . 2023-09-29T15:37:11.109Z) (external_id) (end_location) (url)
;; (book_id . 32654791) (tags . []) (is_favorite . :json-false)
;; (is_discard . :json-false) (readwise_url
;; . https://readwise.io/open/602597895)) ((id . 602597896) (text
;; . For each user story, the team will detail all tasks, including
;; architecture review, development, testing, end user documentation,
;; user experience tasks, and others.) (location . 822) (location_type
;; . location) (note . ) (color . yellow) (highlighted_at
;; . 2020-11-22T04:56:00Z) (created_at . 2023-09-29T15:37:11.109Z)
;; (updated_at . 2023-09-29T15:37:11.109Z) (external_id)
;; (end_location) (url) (book_id . 32654791) (tags . []) (is_favorite
;; . :json-false) (is_discard . :json-false) (readwise_url
;; . https://readwise.io/open/602597896)) ((id . 602597897) (text
;; . What did your team do yesterday? • What will your team do today?
;; • What blockers have you met? • What blockers might you be throwing
;; into another team’s way?) (location . 1001) (location_type
;; . location) (note . ) (color . yellow) (highlighted_at
;; . 2020-11-22T04:56:00Z) (created_at . 2023-09-29T15:37:11.109Z)
;; (updated_at . 2023-09-29T15:37:11.109Z) (external_id)
;; (end_location) (url) (book_id . 32654791) (tags . []) (is_favorite
;; . :json-false) (is_discard . :json-false) (readwise_url
;; . https://readwise.io/open/602597897)) ((id . 602597898) (text
;; . Scrum puts challenges under a magnifying glass. As the image
;; under the glass grows larger, they scream for attention, and your
;; team’s performance will improve after they address the challenges
;; and correct dysfunctions.) (location . 1153) (location_type
;; . location) (note . ) (color . yellow) (highlighted_at
;; . 2020-11-22T04:56:00Z) (created_at . 2023-09-29T15:37:11.109Z)
;; (updated_at . 2023-09-29T15:37:11.109Z) (external_id)
;; (end_location) (url) (book_id . 32654791) (tags . []) (is_favorite
;; . :json-false) (is_discard . :json-false) (readwise_url
;; . https://readwise.io/open/602597898)) ((id . 602597899) (text
;; . Distributed teams heighten the need for clear, timely
;; communication between sites. Recognizing and confronting
;; communication inefficiencies can be critical to the success of the
;; project.) (location . 1157) (location_type . location) (note . )
;; (color . yellow) (highlighted_at . 2020-11-22T04:56:00Z)
;; (created_at . 2023-09-29T15:37:11.109Z) (updated_at
;; . 2023-09-29T15:37:11.109Z) (external_id) (end_location) (url)
;; (book_id . 32654791) (tags . []) (is_favorite . :json-false)
;; (is_discard . :json-false) (readwise_url
;; . https://readwise.io/open/602597899))]))

;; ((user_book_id . 32655789) (title . 40 questions to ask yourself
;; every year) (author . Steph Ango) (readable_title . 40 Questions to
;; Ask Yourself Every Year) (source . reader) (cover_image_url
;; . https://stephango.com/assets/card.png) (unique_url
;; . https://read.readwise.io/read/01hbgvztpthf825awbfyqb0px2)
;; (book_tags . []) (category . articles) (document_note)
;; (readwise_url . https://readwise.io/bookreview/32655789)
;; (source_url . https://stephango.com/40-questions) (asin)
;; (highlights . [((id . 602614418) (text . • What did you do this
;; year that you…) (location . 1376) (location_type . offset) (note
;; . Let's exctract a list from it (probably set a shorter list))
;; (color . ) (highlighted_at . 2023-09-29T16:23:35.949Z) (created_at
;; . 2023-09-29T16:22:57.764Z) (updated_at . 2023-09-29T16:23:35.986Z)
;; (external_id . 01hbgw065c5yghk3cr4t9h5dc4) (end_location) (url
;; . https://read.readwise.io/read/01hbgw065c5yghk3cr4t9h5dc4)
;; (book_id . 32655789) (tags . []) (is_favorite . :json-false)
;; (is_discard . :json-false) (readwise_url
;; . https://readwise.io/open/602614418))]))

;; (document_note . "\"Explorable Explanations\" discusses the concept
;; of active reading and how traditional reading tools like books and
;; websites can hinder it. The essay presents the author's umbrella
;; project, Explorable Explanations, which aims to change people's
;; relationship with text by enabling and encouraging truly active
;; reading. The project provides ideas such as reactive documents,
;; explorable examples, modeling, transparency, and contextual
;; information to help readers become engaged and start exploring. The
;; goal is to give control to the reader, encouraging them to ask
;; questions, verify assumptions, make connections, and follow their
;; own interests.")

(defun org-readwise--handle-entry (entry)
  "Handle each ENTRY."
  ;; (message "%s" entry)
  (let* ((title (alist-get 'title entry))
	 ;; (id (alist-get 'user_book_id entry))
	 (created_at (alist-get 'created_at entry))
	 ;; (author (alist-get 'author entry))
	 ;; (source (alist-get 'source entry))
	 (tags (alist-get 'book_tags entry))
	 (category (alist-get 'category entry))
	 ;; (readwise_url (alist-get 'readwise_url entry))
	 (document_note (alist-get 'document_note entry))
	 ;; (highlights (alist-get 'highlights entry))
	 )
    (message "title: %s" title)
    (message "created_at: %s" created_at)
    (message "category: %s" category)
    (message "tags: %s" tags)
    (message "notes: %s" document_note)
    ;; (message "entry: %s" entry)
    ;; TODO: switch case on category to know where to write file
    ;; - books
    ;; - articles
    ;; - ?
    ;; TODO: get creation date from highlights dates ?
    ;; TODO: call denote function to create file with title, date, tags, …
    ;; TODO: handle if there is no highlights
    ))

(defun org-readwise--add-highlights (highlights)
  "Add all new HIGHLIGHTS to `org-mode' notes."
  (mapc 'org-readwise--handle-entry highlights))

(provide 'org-readwise)
;;; org-readwise.el ends here
