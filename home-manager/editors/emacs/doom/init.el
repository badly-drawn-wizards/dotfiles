;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom!

       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;; fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       pretty-code       ; replace bits of code with pretty symbols
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       (ligatures fira)
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       multiple-cursors  ; editing in many places at once
       parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree
       (undo +tree)

       :term
       vterm

       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       direnv
       lsp
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pdf               ; pdf enhancements
       terraform

       :lang
       (agda +local)              ; types of types of types of types...
       coq
       ocaml             ; an objective camel
       (cc +lsp)                ; C/C++/Obj-C madness
       clojure           ; java with a lisp
       (csharp +lsp)            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (haskell +lsp)  ; a language that's lazier than I am
       (java +lsp) ; the poster child for carpal tunnel syndrome
       ;; (web +lsp)
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       json
       (latex +viewers +ref)
       lean
       ledger            ; an accounting system in Emacs
       (lua +lsp)
       markdown          ; writing docs for people to ignore
       (nix +lsp +treesitter)               ; I hereby declare "nix geht mehr!"
       (org              ; organize your plain life in plain text
        +pomodoro        ; be fruitful with the tomato technique
        +present)        ; using org-mode for presentations
       plantuml          ; diagrams for confusing people more
       (purescript +lsp)        ; javascript, but functional
       (python +lsp)            ; beautiful is better than ugly
       racket            ; a DSL for DSLs
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       (go +lsp)

       :app
       irc               ; how neckbeards socialize

       :config
       (default +bindings +smartparens))
