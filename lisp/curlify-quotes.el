(require 'xah-replace-pairs)

(defun xah-replace-straight-quotes (φbegin φend)
  "Replace straight double quotes to curly ones, and others.
Works on current line or text selection.

Examples of changes:
 「\"…\"」 ⇒ 「“…”」
 「...」 ⇒ 「…」
 「I’m」 => 「I'm」
 「--」 ⇒ 「—」
 「~=」 ⇒ 「≈」

When called in lisp code, φbegin and φend are region begin/end positions.

WARNING: this command does not guarantee 100% correct conversion, because it's heuristics based. Also, if you use it in code, such as HTML, watch out for bad change of straight quotes such as in 「class=\"…\"」.

URL `http://ergoemacs.org/emacs/elisp_straight_curly_quotes.html'
Version 2015-08-22"
  ;; some examples for debug
  ;; do "‘em all -- done..."
  ;; I’am not
  ;; said "can’t have it, can’t, just can’t"
  ;; ‘I’ve can’t’
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))

  (let ( (case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region φbegin φend )
        ;; Note: order is important since this is huristic.
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          ;; dash and ellipsis etc
          ["--" " — "]
          ["—" " — "]
          ["..." "…"]
          [" :)" " ☺"]
          [" :(" " ☹"]
          [" ;)" " 😉"]
          ["e.g. " "➳ for example: "]
          ["~=" "≈"]
          ["  —  " " — "] ; rid of extra space in em-dash
          [" , " ", "]
          ;; fix GNU style ASCII quotes
          ["``" "“"]
          ["''" "”"]
          ;; "straight quote" ⇒ “double quotes”
          ["\n\"" "\n“"]
          [">\"" ">“"]
          ["(\"" "(“"]
          [" \"" " “"]
          ["\" " "” "]
          ["\"," "”,"]
          ["\"." "”."]
          ["\"?" "”?"]
          ["\";" "”;"]
          ["\":" "”:"]
          ["\")" "”)"]
          ["\"]" "”]"]
          [".\"" ".”"]
          [",\"" ",”"]
          ["!\"" "!”"]
          ["?\"" "?”"]
          ["\"<" "”<"]
          ["\"\n" "”\n"]
          ] )

        ;; fix straight double quotes by regex
        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["\\`\"" "“"]
          ])

        ;; fix single quotes to curly
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          [">\'" ">‘"]
          [" \'" " ‘"]
          ["\' " "’ "]
          ["\'," "’,"]
          [".\'" ".’"]
          ["!\'" "!’"]
          ["?\'" "?’"]
          ["(\'" "(‘"]
          ["\')" "’)"]
          ["\']" "’]"]
          ])

        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["\\bcan’t\\b" "can't"]
          ["\\bdon’t\\b" "don't"]
          ["\\bdoesn’t\\b" "doesn't"]
          ["\\bain’t\\b" "ain't"]
          ["\\bdidn’t\\b" "didn't"]
          ["\\baren’t\\b" "aren't"]
          ["\\bwasn’t\\b" "wasn't"]
          ["\\bweren’t\\b" "weren't"]
          ["\\bcouldn’t\\b" "couldn't"]
          ["\\bshouldn’t\\b" "shouldn't"]

          ["\\b’ve\\b" "'ve"]
          ["\\b’re\\b" "'re"]
          ["\\b‘em\\b" "'em"]
          ["\\b’ll\\b" "'ll"]
          ["\\b’m\\b" "'m"]
          ["\\b’d\\b" "'d"]
          ["\\b’s\\b" "'s"]
          ["s’ " "s' "]
          ["s’\n" "s'\n"]

          ["\"$" "”"]
          ])

        ;; fix back escaped quotes in code
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          ["\\”" "\\\""]
          ])

        ;; fix back. quotes in HTML code
        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["” \\([-a-z]+\\)="       "\" \\1="] ; any 「” some-thing=」
          ["=\”" "=\""]
          ["/” " "/\" "]
          ["\"\\([0-9]+\\)” "     "\"\\1\" "]
          ]
         )))))
