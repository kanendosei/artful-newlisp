; @module Json
; @author Jeff Ober <jeffober@gmail.com>, Kanen Flowers <kanendosei@gmail.com>
; @version 2.1
; @location https://raw.github.com/kanendosei/artful-newlisp/master/json.lsp
; @package  https://raw.github.com/kanendosei/artful-newlisp/master/json.qwerty
; @description JSON parser and encoder
; <p>Library for parsing JSON data and serializing lisp into JSON.</p>
;

(context 'Json)

; @syntax (Json:Lisp->Json <expr>)
; @param <expr> expression to be converted to JSON
; <p>Converts expression <expr> to JSON.  Association lists and
; contexts are converted into objects.  Other lists and arrays are
; converted into JSON arrays.</p>
; @example
; (Json:Lisp->Json '((a 1) (b 2)))
; => "{ 'A': 1, 'b': 2 }"
; (Json:Lisp->Json '(1 2 3 4 5))
; => "[1, 2, 3, 4, 5]"
(define (Lisp->Json lisp)
  (case (type-of lisp)
    ("boolean" (if lisp "true" "false"))
    ("quote" (Lisp->Json (eval lisp)))
    ("symbol" (format "'%s'" (name lisp)))
    ("string" (format "'%s'" (simple-escape lisp)))
    ("integer" (string lisp))
    ("float" (string lisp))
    ("list" (if (assoc? lisp)
                (format "{ %s }"
                        (join (map (fn (pair)
                                     (format "'%s': %s"
                                             (if (symbol? (pair 0))
                                                 (name (pair 0))
                                                 (string (pair 0)))
                                             (Lisp->Json (pair 1))))
                                   lisp)
                              ", "))
                (string "[" (join (map Lisp->Json lisp) ", ") "]")))
    ("array" (string "[" (join (map Lisp->Json lisp) ", ") "]"))
    ("context" (let ((values '()))
                 (dotree (s lisp)
                   (push (format "'%s': %s"
                                 (name s)
                                 (Lisp->Json (eval s)))
                         values -1))
                 (format "{ %s }" (join values ", "))))
    (true (throw-error (format "invalid Lisp->Json type: %s" lisp)))))

(define (simple-escape str)
  (replace {[\n\r]+} str {\n} 4)
  (replace {'} str {\'} 4)
  str)

; @syntax (Json:Json->Lisp <str-json>)
; @param <str-json> a valid JSON string
; <p>Parses a valid JSON string and returns a lisp structure.
; Arrays are converted to lists and objects are converted to
; assocation lists.</p>
; @example
; (Json:Json->Lisp "[1, 2, 3, 4]")
; => (1 2 3 4)
; (Json:Json->Lisp "{ 'x': 3, 'y': 4, 'z': [1, 2, 3] }")
; => (("x" 3) ("y" 4) ("z" (1 2 3)))
(define (Json->Lisp json)
  (first (lex (tokenize json))))

(define number-re (regex-comp {^([-+\deE.]+)} 1))
(define identifier-re (regex-comp {([$_a-zA-Z][$_a-zA-Z0-9]*)(.*)} 4))

(define (read-number json-text , matched n)
  "Reads in a number in any Javascript-permissible format and attempts to
  convert it to a newLISP float. If the number's absolute value is greater
  than 1e308 (defined as +/-INF in newLISP), the number is returned as a
  string."
  (setf json-text (trim json-text))
  (when (setf matched (regex number-re json-text 0x10000))
    (setf n (pop json-text 0 (matched 5)))
    (list (if (> (abs (float n)) 1e308) n (float n)) json-text)))

(define (read-string json-text , quot c escaped split-index str)
  (setf quot (pop json-text) str "")
  (catch
    (until (empty? (setf c (pop json-text)))
      (if (and (= c quot) (not escaped))
        (throw $idx)
        (write-buffer str c))
      (setf escaped (and (not $it) (= c "\\")))))
  (list str json-text))

(define (read-identifier json-text , matched)
  (setf json-text (trim json-text))
  (setf matched (regex identifier-re json-text 0x10000))
  (list (case (nth 3 matched)
          ("true" true) ("TRUE" true)
          ("false" nil) ("FALSE" nil)
          ("null" nil) ("NULL" nil)
          (true (nth 3 matched)))
        (nth 6 matched)))

(define (tokenize json-text (acc '()) , tok tail n)
  (setf json-text (trim json-text))
  (cond
    ((empty? json-text) acc)
    ((regex {^\s+} json-text 4)
     (tokenize (replace {^\s+} json-text "" 0) acc))
    ((regex number-re json-text 0x10000)
     (map set '(tok tail) (read-number json-text))
     (push tok acc -1)
     (tokenize tail acc))
    ((regex {^['"]} json-text)
     (map set '(tok tail) (read-string json-text))
     (push tok acc -1)
     (tokenize tail acc))
    ((regex [text]^[{}\[\]:,][/text] json-text)
     (setf tok (pop json-text))
     (case tok
       ("{" (push 'OPEN_BRACE acc -1))
       ("}" (push 'CLOSE_BRACE acc -1))
       ("[" (push 'OPEN_BRACKET acc -1))
       ("]" (push 'CLOSE_BRACKET acc -1))
       (":" (push 'COLON acc -1))
       ("," (push 'COMMA acc -1)))
     (tokenize json-text acc))
    (true
     (map set '(tok tail) (read-identifier json-text))
     (push tok acc -1)
     (tokenize tail acc))))

(define (lex tokens, (tree '()) (loc '(-1)) (depth 0) (mark 0))
  ; Note: mark is used to match colon-pairings' depth against the current
  ; depth to prevent commas in a paired value (e.g. foo: [...] or foo: {})
  ; from popping the stack.
  (unless (find (first tokens) '(OPEN_BRACKET OPEN_BRACE))
    (throw-error "A JSON object must be an object or array."))
  (dolist (tok tokens)
    (case tok
      (OPEN_BRACKET
        (inc depth)
        (push (list) tree loc)
        (push -1 loc))
      (OPEN_BRACE
        (inc depth)
        (push (list) tree loc)
        (push -1 loc))
      (CLOSE_BRACKET
        (dec depth)
        (pop loc))
      (CLOSE_BRACE
        (dec depth)
        (pop loc))
      (COLON
        (push (list (pop tree loc)) tree loc)
        (push -1 loc)
        (setf mark depth))
      (COMMA
        (when (= mark depth)
          (setf mark nil)
          (pop loc)))
      (true
        (push tok tree loc))))
  tree)

(context MAIN)
