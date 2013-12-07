;;; interval-list.el --- Interval list data structure for 1D selections

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 7 Dec 2013
;; Version: 0.1
;; Package-Requires: ((dash "2.4.0"))
;; Keywords: extensions, data structure
;; URL: https://github.com/Fuco1/interval-list

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a 1D selection model.  It provides methods
;; to add or remove intervals and merges or splits the overlapping
;; portions automatically.  The intervals are modeled as a list of
;; cons (beg . end)

;; See github readme at https://github.com/Fuco1/interval-list

;;; Code:

(require 'dash)

(defun intlist-add-interval (intlist begin end)
  (let* ((split (--split-with (<= (car it) begin) intlist))
         (splice (-concat (car split) (list (cons begin end)) (cadr split)))
         (backprop (-reduce-r-from (lambda (it acc)
                                     (if (>= (1+ (cdr it)) (caar acc))
                                         (cons (cons (car it) (max (cdar acc)
                                                                   (cdr it))) (cdr acc))
                                       (cons it acc)))
                                   (list (-last-item splice))
                                   (butlast splice)))
         (forwardprop (-reduce-from (lambda (acc it)
                                      (if (<= (1- (car it)) (cdar acc))
                                          (cons (cons (min (caar acc)
                                                           (car it)) (cdar acc)) (cdr acc))
                                        (cons it acc)))
                                    (list (car backprop))
                                    (cdr backprop))))
    (nreverse forwardprop)))



(defun intlist-remove-interval (intlist begin end)
  "Remove interval between BEGIN and END from intlist"
  (let* ((split-ends (-mapcat (lambda (it)
                                (cond
                                 ((and (= (car it) (cdr it))
                                       (= (car it) beg))
                                  nil)
                                 ((and (>= begin (car it))
                                       (<= begin (cdr it))
                                       (>= end (car it))
                                       (<= end (cdr it)))
                                  (list (cons (car it) (1- begin))
                                        (cons begin end)
                                        (cons (1+ end) (cdr it))))
                                 ((and (>= begin (car it))
                                       (<= begin (cdr it)))
                                  (list (cons (car it) (1- begin))
                                        (cons begin (cdr it))))
                                 ((and (>= end (car it))
                                       (<= end (cdr it)))
                                  (list (cons (car it) end)
                                        (cons (1+ end) (cdr it))))
                                 (t (list it))))
                              intlist)))

    (-remove (lambda (it)
               (and (>= (car it) begin)
                    (<= (cdr it) end))) split-ends)))

(cl-eval-when (eval)
  (ert-deftest intlist-add-to-empty ()
    (should (equal (intlist-add-interval nil 1 2) '((1 . 2)))))

  (ert-deftest intlist-add-to-nonempty ()
    (let ((cases '(((((1 . 2)) 1 4) . ((1 . 4)))
                   ((((1 . 2)) 2 4) . ((1 . 4)))
                   ((((1 . 2)) 3 4) . ((1 . 4)))
                   ((((3 . 4)) 1 2) . ((1 . 4)))
                   ((((3 . 4)) 1 3) . ((1 . 4)))
                   ((((3 . 4)) 1 4) . ((1 . 4)))
                   ((((1 . 4)) 2 3) . ((1 . 4)))
                   ((((1 . 4)) 3 5) . ((1 . 5)))
                   ((((3 . 6)) 1 4) . ((1 . 6)))
                   ((((3 . 6)) 2 7) . ((2 . 7)))
                   ((((1 . 2)) 4 5) . ((1 . 2) (4 . 5)))
                   ((((1 . 2) (4 . 5)) 7 8) . ((1 . 2) (4 . 5) (7 . 8)))
                   ((((1 . 2) (4 . 5)) 4 10) . ((1 . 2) (4 . 10)))
                   ((((1 . 2) (4 . 5)) 5 10) . ((1 . 2) (4 . 10)))
                   ((((1 . 2) (4 . 5)) 1 10) . ((1 . 10)))
                   ((((1 . 2) (4 . 5)) 2 10) . ((1 . 10)))
                   ((((1 . 2) (4 . 5)) 3 10) . ((1 . 10)))
                   ((((1 . 4) (8 . 10)) 6 9) . ((1 . 4) (6 . 10)))
                   ((((1 . 4) (8 . 10)) 6 11) . ((1 . 4) (6 . 11)))
                   ((((3 . 6) (10 . 12)) 7 8) . ((3 . 8) (10 . 12)))
                   ((((3 . 5) (10 . 12)) 7 8) . ((3 . 5) (7 . 8) (10 . 12)))
                   ((((3 . 6) (8 . 10)) 5 9) . ((3 . 10)))
                   ((((3 . 6) (8 . 10) (12 . 14)) 5 15) . ((3 . 15)))
                   ((((3 . 6) (8 . 10) (12 . 14)) 5 9) . ((3 . 10) (12 . 14)))
                   )))
      (cl-dolist (case cases)
        (should (equal (intlist-add-interval (caar case) (cadar case) (caddar case)) (cdr case))))))

  (ert-deftest intlist-remove-empty ()
    (should (equal (intlist-remove-interval nil 1 4) nil)))

  (ert-deftest intlist-remove-nonempty ()
    (let ((cases '(((((1 . 2)) 1 2) . nil)
                   ((((3 . 4)) 1 4) . nil)
                   ((((3 . 4)) 3 7) . nil)
                   ((((3 . 4)) 1 7) . nil)
                   ((((1 . 4)) 1 3) . ((4 . 4)))
                   ((((1 . 4)) 2 3) . ((1 . 1) (4 . 4)))
                   ((((1 . 4)) 3 3) . ((1 . 2) (4 . 4)))
                   ((((1 . 4)) 4 4) . ((1 . 3)))
                   ((((1 . 4)) 3 5) . ((1 . 2)))
                   ((((1 . 4) (6 . 9)) 3 6) . ((1 . 2) (7 . 9)))
                   ((((1 . 4) (6 . 9)) 4 10) . ((1 . 3)))
                   ((((1 . 4) (6 . 9) (12 . 15)) 4 13) . ((1 . 3) (14 . 15))))))
      (cl-dolist (case cases)
        (should (equal (intlist-remove-interval (caar case) (cadar case) (caddar case)) (cdr case)))))))

(provide 'interval-list)
;;; interval-list.el ends here
