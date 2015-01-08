;; eqauctions-trending - A buying and selling guide for project1999
;; Copyright (C) 2013 Matthew Carter
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; eqauctions-trending.lisp

(in-package #:eqauctions-trending)

;;; "eqauctions-trending" goes here. Hacks and glory await!

(defparameter *dsn* (list "localhost" "ahungry" "equser" "equser"))

(defmacro 1query (&rest rest)
  "Do queries in this manner to avoid DB going away"
  `(clsql:with-database (db *dsn* :database-type :mysql)
     (query ,@rest :database db)))

(defparameter *wtb* (make-hash-table :test 'equal))
(defparameter *wts* (make-hash-table :test 'equal))
(defparameter *cleaned-wtb* '())
(defparameter *cleaned-wts* '())

(ƒ find-wtb
   ~"(?i).*wtb\(.*?\)\(wts.*|$\)"~ → |"\\1"|)

(ƒ find-wts
   ~"(?i).*wts\(.*?\)\(wtb.*|$\)"~ → |"\\1"|)

(ƒ item-alias
   "fbss"    → "flowing black silk sash"
   "fungi"   → "fungus covered scaled tunic"
   "ruby bp" → "rubicite breastplate"
   α → α)

(defun find-items (listing hash-table)
  "Parse out a listing and find the items that are contained
in the sent in string.
Increment the hash table for items as needed."
  (do-matches-as-strings (item "\\w+['A-Za-z0-9 ]+" listing)
    (let ((item (item-alias (string-downcase (string-trim " " item)))))
      (incf (gethash item hash-table 0)))))

(defun clean-invalid-items (plist)
  "Run across all items and remove those which are not
a legitimate item"
  (remove-if-not
   (λ α → (get-item-from-db (getf α :name))) plist))

(defun parse-listing (listing)
  "Run the find-items for both wts and wtb."
  (find-items (find-wts listing) *wts*)
  (find-items (find-wtb listing) *wtb*))

(defun get-items (fn date-range)
  "Run through the eqItems database and parse it out,
using the chosen passed in parsing function."
  (mapcar (λ α → (funcall fn (car α)))
          (1query (format nil "SELECT DISTINCT(`listing`) FROM `eqAuction`
WHERE `date` > DATE_SUB(now(), INTERVAL ~a)" date-range))))

(defun get-item-from-db (item-name)
  "Query the eqItems table for the specified item-name
to make sure only real items are included in our results."
  (let ((cleaned-item-name (regex-replace-all "'" item-name "\\\\'")))
    (car (car (1query (format nil "SELECT `name` FROM `eqItems`
WHERE `name` = '~a'" cleaned-item-name) :field-names nil)))))

(defun hash-to-plist (hash-table)
  "Take in a hash table and return it as a sorted plist"
  (loop for k being the hash-keys in hash-table using (hash-value v)
     collect (list :name k
                   :total v
                   :demand (/ v (or (gethash k *wts*) .5)))));;(gethash k *wts*)))))

(defun sorted-plist (plist)
  "Sort the plist as needed - in this case by total items."
  (let ((new-list (copy-list plist)))
    (sort new-list #'> :key (λ α → (getf α :total)))))

(defun sorted-plist-by-demand (plist)
  "Sort the plist as needed - in this case by demand of items."
  (let ((new-list (copy-list plist)))
    (sort new-list #'> :key (λ α → (getf α :demand)))))

(defun load-and-parse-data (&optional date-range)
  "Load up the items from the DB into *wtb* and *wts* params,
sort the items by most common first"
  (setf *wts* (make-hash-table :test 'equal)
        *wtb* (make-hash-table :test 'equal))
  (get-items 'parse-listing (or date-range "1 MONTH"))
  (setf *cleaned-wtb*
        (clean-invalid-items (hash-to-plist *wtb*)))
  (setf *cleaned-wts*
        (clean-invalid-items (hash-to-plist *wts*))))

(defun print-trending-auctions (total-listings)
  "Print out the WTB and WTS top items - use this after
running load-and-parse-data or it will have nothing to
print out."
  (let ((sorted-wtb (sorted-plist *cleaned-wtb*))
        (sorted-wts (sorted-plist *cleaned-wts*)))
    (format t "~%WTB (total) ~60t WTS (total)~%")
    (dotimes (x total-listings)
      (let ((wtb (nth x sorted-wtb))
            (wts (nth x sorted-wts)))
        (format t "~a (~a) ~60t ~a (~a)~%"
                (getf wtb :name)
                (getf wtb :total)
                (getf wts :name)
                (getf wts :total))))))

(defun print-trending-auctions-by-demand (total-listings)
  "Print out the top in demand items"
  (let* ((by-demand (sorted-plist-by-demand *cleaned-wtb*))
         (total-listings (if (> total-listings (length by-demand))
                             (length by-demand)
                             total-listings)))
    (format t "~%WTB by Demand ~40t (wtb/wts ~52t → percent)~%")
    (dotimes (x total-listings)
      (let ((wtb (nth x by-demand)))
        (format t "~a ~40t (~a ~52t → ~a %) ~%"
                (getf wtb :name)
                (getf wtb :demand)
                (round (* 100 (float (getf wtb :demand)))))))))

(defun create-report (type total pretty-date)
  "Type should ideally be year,month,week - total should
be how many of the listing we want to print out"
  (with-open-file (*standard-output*
                   (format nil "/tmp/eqauctions-trending/~a-~a.txt"
                           type
                           pretty-date)
                   :direction :output
                   :if-exists :supersede)
    (load-and-parse-data (format nil "1 ~a" type))
    (format t "Top ~a for the last ~a (report created on: ~a)"
            total type
            pretty-date)
    (print-trending-auctions-by-demand total)
    (print-trending-auctions total)))

(defun main ()
  "Run the main functions necessary"
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore second minute hour day-of-week dst-p tz))
    (let ((pretty-date (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))
      (create-report "day" 100 pretty-date)
      (create-report "week" 100 pretty-date)
      (create-report "month" 100 pretty-date)
      (create-report "year" 100 pretty-date))))

(main)
