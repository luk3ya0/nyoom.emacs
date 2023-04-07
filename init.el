;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       (company                     ; the ultimate code completion backend
        +childframe)                ; ... when your children are better than you
       (vertico +icons)             ; the search engine of the future

       :ui
       doom                         ; what makes DOOM look the way it does
       doom-dashboard               ; a nifty splash screen for Emacs
       doom-quit                    ; DOOM quit-message prompts when you quit Emacs
       modeline
       minimap                      ; show a map of the code on the side
       ophints                      ; highlight the region an operation acts on
       (popup                       ; tame sudden yet inevitable temporary windows
        +all                        ; catch all popups that start with an asterix
        +defaults)                  ; default popup rules
       vc-gutter                    ; vcs diff in the fringe
       workspaces                   ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)           ; come to the dark side, we have cookies
       file-templates               ; auto-snippets for empty files
       fold                         ; (nigh) universal code folding
       (format +onsave)             ; automated prettiness
       multiple-cursors             ; editing in many places at once
       snippets                     ; my elves. They type so I don't have to

       :emacs
       (dired +icons)               ; making dired pretty [functional]
       electric                     ; smarter, keyword-based electric-indent
       (ibuffer +icons)             ; interactive buffer management
       undo                         ; persistent, smarter undo for your inevitable mistakes
       vc                           ; version-control and Emacs, sitting in a tree

       :term
       vterm                        ; the best terminal emulation in Emacs

       :checkers
       syntax                       ; tasing you for every semicolon you forget

       :tools
       direnv
       docker
       ;; biblio                    ; Writes a PhD for you (citation needed)
       ;; (debugger +lsp)           ; FIXME stepping through code, to help you add bugs
       (eval +overlay)              ; run code, run (also, repls)
       (lookup                      ; helps you navigate your code and documentation
        +dictionary                 ; dictionary/thesaurus is nice
        +docsets)                   ; ...or in Dash docsets locally
       ;; lsp                       ; Language Server Protocol
       (magit                       ; a git porcelain for Emacs
        +forge)                     ; interface with git forges
       pdf                          ; pdf enhancements
       rgb                          ; creating color strings
       ;; tree-sitter               ; Syntax and Parsing sitting in a tree

       :os
       (:if IS-MAC macos)           ; improve compatibility with macOS

       :lang
       ;; (cc +lsp +tree-sitter)    ; C/C++/Obj-C madness
       ;;clojure                    ; java with a lisp
       ;;common-lisp                ; if you've seen one lisp, you've seen them all
       ;;coq                        ; proofs-as-programs
       ;;crystal                    ; ruby at the speed of c
       ;;csharp                     ; unity, .NET, and mono shenanigans
       ;; data                      ; config/data formats
       ;; (dart +flutter)           ; paint ui and not much else
       ;;dhall
       ;;elixir                     ; erlang done right
       ;;elm                        ; care for a cup of TEA?
       emacs-lisp                   ; drown in parentheses
       ;;erlang                     ; an elegant language for a more civilized age
       ;;ess                        ; emacs speaks statistics
       ;;factor
       ;;faust                      ; dsp, but you get to keep your soul
       ;;fortran                    ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp                     ; ML stands for Microsoft's Language
       ;;fstar                      ; (dependent) types and (monadic) effects and Z3
       ;;gdscript                   ; the language you waited for
       ;; (go +lsp)                 ; the hipster dialect
       ;;(graphql +lsp)             ; Give queries a REST
       ;;(haskell +lsp)             ; a language that's lazier than I am
       ;;hy                         ; readability of scheme w/ speed of python
       ;;idris                      ; a language you can depend on
       ;; json                      ; At least it ain't XML
       ;; java                      ; the poster child for carpal tunnel syndrome
       ;; javascript                ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia                      ; a better, faster MATLAB
       ;; kotlin                    ; a better, slicker Java(Script)
       ;; (latex                    ; writing papers in Emacs has never been so fun
       ;;  ;;+fold                  ; fold the clutter away nicities
       ;;  +latexmk                 ; modern latex plz
       ;;+cdlatex                   ; quick maths symbols
       ;;lean                       ; for folks with too much to prove
       ;;ledger                     ; be audit you can be
       ;; (lua +lsp +fennel)        ; one-based indices? one-based indices
       ;; markdown                  ; writing docs for people to ignore
       ;;nim                        ; python + lisp at the speed of c
       ;;nix                        ; I hereby declare "nix geht mehr!"
       ;;ocaml                      ; an objective camel
       (org                         ; organize your plain life in plain text
        ;; +pretty                  ; yessss my pretties! (nice unicode symbols)
        ;;+dragndrop                ; drag & drop files/images into org buffers
        +hugo)                      ; use Emacs for hugo blogging
        ;; +noter                   ; enhanced PDF notetaking
        ;; +jupyter                 ; ipython/jupyter support for babel
        ;; +pandoc                  ; export-with-pandoc support
        ;; +gnuplot                 ; who doesn't like pretty pictures
        ;; +pomodoro                ; be fruitful with the tomato technique
        ;; +present                 ; using org-mode for presentations
        ;; +roam2)                  ; wander around notes
       ;; (python                   ; beautiful is better than ugly
       ;;  +lsp
       ;;  +pyright
       ;;  +tree-sitter
       ;;  +conda)
       ;; go
       ;;qt                         ; the 'cutest' gui framework ever
       ;;racket                     ; a DSL for DSLs
       ;;raku                       ; the artist formerly known as perl6
       ;;rest                       ; Emacs as a REST client
       ;;rst                        ; ReST in peace
       ;;(ruby +rails)              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;; (rust
       ;;  +lsp
       ;;  +tree-sitter)            ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala                      ; java, but good
       ;; sh                        ; she sells {ba,z,fi}sh shells on the C xor
       ;; swift                     ; who asked for emoji variables?
       ;; web                       ; the tubes
       ;; yaml                      ; JSON, but readable

       :config
       (default +bindings +smartparens))
