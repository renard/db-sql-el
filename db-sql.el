;;; db-sql.el --- Connect to SQL server using tramp syntax

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, tramp, sql
;; Created: 2010-12-17
;; Last changed: 2013-06-27 11:37:31
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 
;; Connect to remote database using a tramp syntax.
;;
;; For this to work ssh should be correctly configured.

;;; Code:

(require 'sql)
(require 'tramp)
(require 'cl)

(defcustom db-sql-workdirs
  '((postgres "/sudo:postgres@%s:"))
  "ALIST defining the working directories used to connect to a
database.")


(defcustom db-sql-pathseparator "|"
  "Character used as field separator for tramp URLs.

URLs are formated as:

  /user@host:dbuser|dbpasswd|database
")



(dolist (type (mapcar 'car sql-product-alist))
  (fset (intern (concat "db-sql-" (symbol-name type)))
	`(lambda (&optional host)
	   "Connect to database on HOST.
See `db-sql' for further information."
	   (interactive)
	   (db-sql (quote ,type) host))))

;;;###autoload
(defun db-sql (&optional type host)
  "Connect to sql database as defined by TYPE on server HOST.

Database types are defined in `sql-product-alist'.

Wrapper functions such as `sql-db-postgres' or `sql-db-mysql' are
also defined for quick access to `db-sql'.

If some database access need a special work directory, it could
be defined in `db-sql-workdirs'."
  (interactive)
  (let* ((type (or type
		   (intern (completing-read
			    "SQL type: "
			    (mapcar #'(lambda(x)
					(symbol-name (car x)))
				    sql-product-alist)
			    nil t))))
	 (db-set (cdr (assoc type sql-product-alist)))
	 (host (or host
		   (read-file-name 
		    "Connect to: " "/" nil nil nil)))
	 (path host)
	 (path-v (or (ignore-errors (tramp-dissect-file-name path))
		       (tramp-dissect-file-name (concat "/:" path) 1)))
	 (host (tramp-file-name-real-host path-v))
	 (wd (cadr (assoc type db-sql-workdirs)))
	 (default-directory
	   (if wd
	       (format wd host)
	     (file-name-directory path)))
	 (tramp-path (split-string
		      (tramp-file-name-localname path-v)
		      db-sql-pathseparator))
	 (sql-user (or (car tramp-path) ""))
	 (sql-password (or (cadr tramp-path) "" ))
	 (database (or (caddr tramp-path) ""))
	 (func (intern (format "sql-comint-%s" type)))
	 (sql-buf (format "*SQL %s %s %s*" type host database)))
    ;; launch sql program on remote host.
    (message "(funcall %s %s)" func type)
    (switch-to-buffer 
     (funcall func type nil))
    (rename-buffer sql-buf)
    (set-process-sentinel (get-buffer-process (current-buffer))
			  '(lambda (proc status)
			     (when (eq (process-status proc) 'exit)
			       (kill-buffer (process-buffer proc)))))))

(provide 'db-sql)
