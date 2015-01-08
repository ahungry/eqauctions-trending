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

;;;; eqauctions-trending.asd

(asdf:defsystem #:eqauctions-trending
  :serial t
  :description "Find which items are trending"
  :author "Matthew Carter <m@ahungry.com>"
  :license "AGPLv3"
  :depends-on (#:cl-ppcre
               #:clsql
               #:cl-who
               #:hunchentoot
               #:parenscript
               #:glyphs)
  :components ((:file "package")
               (:file "eqauctions-trending")))

