;;; db-sql.el --- Connect to SQL server using tramp syntax

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, tramp, sql
;; Created: 2010-12-17
;; Last changed: 2010-12-20 09:32:59
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'sql)
(require 'tramp)

(defcustom db-sql-workdirs
  '((postgres "/sudo:postgres@%s:"))
  "ALIST defining the working directories.")

(defun db-sql-ansi (&optional host)
  "Connect to Ansi database on HOST."
  (interactive)
  (db-sql 'ansi host))

(defun db-sql-db2 (&optional host)
  "Connect to DB2 database on HOST."
  (interactive)
  (db-sql 'db2 host))

(defun db-sql-informix (&optional host)
  "Connect to Informix database on HOST."
  (interactive)
  (db-sql 'informix host))

(defun db-sql-ingres (&optional host)
  "Connect to Ingres database on HOST."
  (interactive)
  (db-sql 'ingres host))

(defun db-sql-interbase (&optional host)
  "Connect to Interbase database on HOST."
  (interactive)
  (db-sql 'interbase host))

(defun db-sql-linter (&optional host)
  "Connect to Linter database on HOST."
  (interactive)
  (db-sql 'linter host))

(defun db-sql-ms (&optional host)
  "Connect to Ms database on HOST."
  (interactive)
  (db-sql 'ms host))

(defun db-sql-mysql (&optional host)
  "Connect to Mysql database on HOST."
  (interactive)
  (db-sql 'mysql host))

(defun db-sql-oracle (&optional host)
  "Connect to Oracle database on HOST."
  (interactive)
  (db-sql 'oracle host))

(defun db-sql-postgres (&optional host)
  "Connect to Postgres database on HOST."
  (interactive)
  (db-sql 'postgres host))

(defun db-sql-solid (&optional host)
  "Connect to Solid database on HOST."
  (interactive)
  (db-sql 'solid host))

(defun db-sql-sqlite (&optional host)
  "Connect to Sqlite database on HOST."
  (interactive)
  (db-sql 'sqlite host))

(defun db-sql-sybase (&optional host)
  "Connect to Sybase database on HOST."
  (interactive)
  (db-sql 'sybase host))

(defun db-sql (&optional type host)
  "Connect to sql database as defined by PATH."
  (interactive)
  (let* ((type (or type
		   (intern (completing-read
			    "SQL type: "
			    (mapcar '(lambda(x)
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
	 (database (file-name-nondirectory path))
	 (sql-buf (format "%s %s %s" type host database)))
    ;; launch sql program on remote host.
    (switch-to-buffer 
     (apply 'make-comint sql-buf
	    (eval (plist-get db-set :sqli-program))
	    nil
	    (append 
	     (eval (plist-get db-set :sqli-options))
	     (list database))))))

(provide 'db-sql)
