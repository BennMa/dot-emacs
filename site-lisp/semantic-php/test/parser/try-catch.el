;;; try-catch.el --- Test cases for parsing of try-catch statements -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Joris Steyn

;; Author: Joris Steyn <jorissteyn@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

(require 'semantic-php)
(require 'ert)

(ert-deftest semantic-php-test-parser-try-catch()
  "Test parsing of try-catch constructs"
  (with-test-buffer
   "
try {
    $a = 1;
} catch (Exception $b) {
    $c = 1;
} catch (Exception $d) {
    $e = 1;
} finally {
    $f = 1;
}"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 3 tags) (should (equal "$d" tag-name)))
    (with-semantic-tag (nth 4 tags) (should (equal "$e" tag-name)))
    (with-semantic-tag (nth 5 tags) (should (equal "$f" tag-name))))))

(provide 'test/parser/try-catch)
;;; try-catch.el ends here
