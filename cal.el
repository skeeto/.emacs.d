;;; cal.el --- simple calendar functions

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; Version: 0.1

;;; Commentary:

;; `cal/insert' inserts a calendar that looks like so:

;;     August 2012
;;  S  M  T  W  T  F  S
;;           1  2  3  4
;;  5  6  7  8  9 10 11
;; 12 13 14 15 16 17 18
;; 19 20 21 22 23 24 25
;; 26 27 28 29 30 31

;;; Code:

(defvar cal/month-days  '(31 28 31 30 31 30 31 31 30 31 30 31))

(defvar cal/month-names
  '(" January"  "February"  "  March"   "  April"   "   May"    "  June"
    "  July"    " August"   "September" " October"  "November"  "December"))

(defun cal/day-of-week (year month day)
  "Return day of week number (0-7)."
  (let* ((Y (if (< month 3) (1- year) year))
         (m (1+ (mod (+ month 9) 12)))
         (y (mod Y 100))
         (c (/ Y 100)))
    (mod (+ day (floor (- (* 26 m) 2) 10) y (/ y 4) (/ c 4) (* -2 c)) 7)))

(defun cal/leap-day (year month)
  "Return the number of leap days to add to MONTH (0 or 1)."
  (if (and (= month 2)
           (or (= 0 (mod year 400))
               (and (> (mod year 100) 0) (= 0 (mod year 4))))) 1 0))

;;;###autoload
(defun cal/insert (year month)
  "Insert a calendar for the given YEAR and MONTH."
  (interactive "nYear (yyyy): \nnMonth (mm): \n")
  (let ((dow (cal/day-of-week year month 1)))
    (insert (format "    %s %d\n" (nth (1- month) cal/month-names) year))
    (insert "  S  M  T  W  T  F  S\n")
    (dotimes (i dow) (insert "   "))
    (dotimes (d (+ (nth (1- month) cal/month-days) (cal/leap-day year month)))
      (insert (format "% 3d" (1+ d)))
      (if (= 0 (mod (+ dow d 1) 7)) (insert "\n")))
    (insert "\n")))

;;; cal.el ends here
