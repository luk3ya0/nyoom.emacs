;;; test.el -*- lexical-binding: t; -*-

(setq xx
      #s(hash-table
         size 1000
         test equal
         data (
               "33" 4
               "aa" 3
               "bb" 9
               "cc" 5 )))


;; test
(equal (gethash (format "%s" 33) xx ) 4)
