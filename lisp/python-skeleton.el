(auto-insert-mode 1)

(if (boundp 'derp)
    (setq auto-insert-alist derp)
  (defvar-local derp auto-insert-alist))

(push '(python-mode . (skeleton-insert
                                nil
         "#!/usr/bin/env pypy" \n
         ("import " "import " str \n)
         "def main():" \n
         > _ \n
         "\n"
         "if __name__ == '__main__':" \n
         > "main(" _ ")" \n))
      auto-insert-alist)

(provide 'python-skeleton)
