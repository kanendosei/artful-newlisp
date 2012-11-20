; @module CSV
; @author Jeff Ober <jeffober@gmail.com>, Kanen Flowers <kanendosei@gmail.com>
; @version 1.2
; @location https://raw.github.com/kanendosei/artful-newlisp/master/csv.lsp
;
; @description Functions for parsing CSV files (updated for newlisp 10)
;

(context 'CSV)

(setq *quote-char* "\"")
(setq *delimiter* ",")

(define (CSV:regex-token-quoted-string delimiter quote-char)
  (format "^(%s((?:[^%s]|%s%s)+?)%s)(%s|$)"
		  quote-char
		  quote-char
		  quote-char
		  quote-char
		  quote-char
		  delimiter))

(define (CSV:regex-token-atom delimiter quote-char)
  (format "^([^%s%s]+?)(?:%s|$)"
		  quote-char
		  delimiter
		  delimiter))

(define (CSV:regex-token-empty delimiter)
  (format "^%s(?:%s|$)"
		  delimiter
		  delimiter))

; @syntax (CSV:make-row-parser <str-delimiter> <str-quote-char>)
; @param <str-delimiter> column delimiter
; @param <str-quote-char> character denoting quoted strings
; <p>Returns a lambda that is able to parse a single row of CSV text.
; The created function returns a list of column values.</p>
; @example
; (setq parser (CSV:make-row-parser "," "\""))
; => parser function
;;
; (parser "foo,bar,baz,bat")
; => ("foo" "bar" "baz" "bat")
;;
; (setq parser (CSV:make-row-parser "|" "\""))
; => parser function
;;
; (parser "foo|bar|baz|bat")
; => ("foo" "bar" "baz" "bat")

(setq re-chars "|[]{}()<>?\^$*+!:.")

(define (CSV:make-row-parser delimiter quote-char)
  (if (find delimiter re-chars) (setq delimiter (string "\\" delimiter)))
  (if (find quote-char re-chars) (setq quote-char (string "\\" quote-char)))
  (letex ((re1 (regex-comp (regex-token-quoted-string delimiter quote-char)))
		  (re2 (regex-comp (regex-token-atom delimiter quote-char)))
		  (re3 (regex-comp (regex-token-empty delimiter))))
	(lambda (line)
	  (let ((re-1 re1) (re-2 re2) (re-3 re3))
		(let ((parser (lambda (ln , m)
						(cond
						 ((set 'm (regex re-1 ln 0x10000))
						  (cons $2 (parser ((+ (m 1) (m 2)) ln))))
						 ((set 'm (regex re-2 ln 0x10000))
						  (cons $1 (parser ((+ (m 1) (m 2)) ln))))
						 ((set 'm (regex re-3 ln 0x10000))
						  (cons {} (parser (1 ln))))
						 (true '())))))
		  (parser line))))))

; @syntax (CSV:parse-string <str-text> [<str-delimiter> [<str-quote-char>]])
; @param <str-text> the text to be parsed
; @param <str-delimiter> column delimiter
; @param <str-quote-char> character denoting quoted strings
; <p>Parses a string of text as a CSV file.  Returns a list with one element
; for each line; each element is a list of column values for each line in the
; text.</p>

(setq EOL-re (regex-comp "\n|\r"))

(define (CSV:parse-string str (delimiter *delimiter*) (quote-char *quote-char*))
  "Parses an entire string into a nested list of rows."
  (map (make-row-parser delimiter quote-char)
	   (parse str EOL-re 0x10000)))

; @syntax (CSV:parse-file <str-file> [<str-delimiter> [<str-quote-char>]])
; @param <str-file> the file to be read and parsed
; @param <str-delimiter> column delimiter
; @param <str-quote-char> character denoting quoted strings
; <p>Parses a CSV text file.  Returns a list with one element
; for each line; each element is a list of column values for each line in the
; text.  <parse-file> parses line by line, rather than processing the entire
; string at once, and is therefore more efficient than <parse-file>.</p>
; <p><b>Note:</b> at least some versions of MS Excel use a single \r for
; line endings, rather than a line feed or both.  newLISP's read-line will
; only treat \n or \r\n as line endings.  If all columns are lumped into one
; flat list, this may be the culprit.  In this case, use <parse-string> with
; <read-file> instead as the best alternative.</p>

(define (CSV:parse-file path (delimiter *delimiter*) (quote-char *quote-char*))
  (let ((f (open path "r"))
		(parser (make-row-parser delimiter quote-char))
		(rows '())
		(buff))
	(while (setq buff (read-line f))
	  (push (parser buff) rows -1))
	(close f)
	rows))

; @syntax (CSV:list->row <list-cols> <str-delimiter> <str-quote-char>)
; @param <str-delimiter> column delimiter; defaults to ","
; @param <str-quote-char> character denoting quoted strings; defaults to "\""
; @param <str-quote-char> character denoting quoted strings
; <p>Generates one row of CSV data from the values in <list-cols>.  Non-numeric
; elements are treated as quoted strings.</p>

(define (CSV:list->row lst (delimiter *delimiter*) (quote-char *quote-char*), (buff ""))
  (dolist (elt lst)
	(write-buffer buff (if (number? elt) (string elt)
						   (format "%s%s%s" quote-char (string elt) quote-char)))
	(write-buffer buff ","))
  buff)

; @syntax (CSV:list->csv <list-rows> <str-delimiter> <str-quote-char> <str-eol>)
; @param <list-rows> list of row sets (each is a list of values)
; @param <str-delimiter> column delimiter; defaults to ","
; @param <str-quote-char> character denoting quoted strings; defaults to "\""
; @param <str-eol> end of line character; defaults to "\n"
; <p>Generates CSV string of a list of column value sets.</p>

(define (CSV:list->csv lst (delimiter *delimiter*) (quote-char *quote-char*) (eol-str "\n"))
  (join (map (fn (r) (list->row r delimiter quote-char)) lst) eol-str))

(context MAIN)
