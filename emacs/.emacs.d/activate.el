;;; activate.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Choose which modules to enable and how to enable them.
;; Many witty comments and code borrowed from Doom Emacs https://github.com/doomemacs/doomemacs
;;; Code:

(defvar shan--active-modules)

;;; Order slightly matters, maybe auto-ide should be part of core?
(setq shan--active-modules
      '(
        :tools
        auto-ide                  ; for every mode, two more heads sprout
        (lsp +dap +ui)            ; M-x vscode
        (format +apheleia)        ; so you can argue about formatting rules instead
        (vc +git +gutter)         ; version-control and Emacs, sitting in a tree
        eval                      ; run code, run (also, repls)
        lookup                    ; define-word: the dictionary you never knew you needed
        ;; (copilot +complete +chat) ; M$ Clippy, but for code

        :input
        os                        ; making Emacs feel native (it never will)

        :checkers
        (syntax +flycheck)        ; tasing you for every semicolon you forget
        (spell +flyspell +aspell) ; tasing you for misspelling mispelling
        (grammar +lsp)            ; tasing grammar mistake every you make

        :completion
        (company +childframe)     ; the only company that actually completes anything
        vertico                   ; company policy: complete everything, vertically
        snippets                  ; my elves. They type so I don't have to

        :editor
        ;; FIXME: breaks ivy and other tools
        ;; hungry-delete             ; the overzealous backspace
        ophints                   ; the buffer flinches when you paste
        multiple-cursors          ; why edit one line when you can break twenty?
        (parentheses +rainbow)    ; ((((((rainbow)))))), lisp devs see nothing unusual
        (zoom +text +window)      ; because sometimes you need to squint
        neotree                   ; exists purely to satisfy a code review

        :ui
        (color                    ; RGB goes brrr
         +todo                    ; NOTE: highlight TODO/FIXME/NOTE/HACK
         +whitespace              ; Pandora's box, but for trailing spaces
         +nums                    ; H16HL16H7 NUM3R1C L173R4L5
         +tokens)                 ; #ffb86c - orange you glad it's not monochrome?
        iconography               ; pictograms for the modern cave wall
        (theme +solaire)          ; YOU DIED - at least the theme was beautiful \[T]/
        modeline                  ; the bar at the bottom that knows more than you do
        dashboard                 ; emacs loaded 7.5 million packages in 42 years. here's a quote about perseverance.
        discoverability           ; you've been using Emacs for years and still don't know half the keys. this won't help.

        :lang
        ;;; Systems
        ;; (asm +mips)               ; talk directly to the metal
        (cc +lsp)                 ; C > C++ == 1
        (csharp +lsp)             ; Java, reinvented in Redmond by Pascal
        (java +lsp +dap)          ; speaking of, the poster child for carpal tunnel syndrome
        (go +lsp +dap)            ; the hipster dialect
        (rust +lsp +dap)          ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        (zig +lsp)                ; C, but simpler
        (swift +lsp)              ; who asked for emoji variables?
        (dart +lsp)               ; paint ui and not much else
        (kotlin +lsp)             ; a better, slicker Java(Script)
        (scala +lsp)              ; java, but good

        ;;; Functional
        (haskell +lsp)            ; a language that's lazier than I am
        (ocaml +lsp)              ; an objective camel
        (erlang +lsp)             ; an elegant language for a more civilized age
        (elixir +lsp)             ; erlang done right
        (clojure +lsp)            ; java with a lisp
        scheme                    ; a fully conniving family of lisps
        common-lisp               ; if you've seen one lisp, you've seen them all
        (racket +lsp)             ; a DSL for DSLs
        janet                     ; Fun fact: Janet is me!

        ;;; Scripting
        (python +lsp +dap)        ; beautiful is better than ugly
        (ruby +lsp)               ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        (perl +lsp)               ; write once, read never
        (lua +lsp)                ; one-based indices? one-based indices
        shell                     ; she sells {ba,z,fi}sh shells on the C xor
        fish                      ; friendly interactive shell, for real
        powershell                ; for when you must Windows

        ;;; Web
        (js +ts                   ; all(hope(abandon(ye(who(enter(here))))))
         +jsx +tsx                ; how would you React to HTML in your JS?
         +vue                     ; from my point of Vue, not like that
         +lsp +dap)
        (web                      ; the tubes
         +emmet                   ; div>ul>li*5 goes brrr
         +vtl                     ; velocity: fast name, slow death
         +lsp +dap)
        (astro +lsp)              ; where components go on vacation (islands, get it?)

        ;;; Data & Config
        json                      ; at least it ain't XML
        (yaml +lsp)               ; "JSON, but readable" - said no one debugging indentation
        toml                      ; INI files with a college degree
        csv                       ; spreadsheets for people who don't spreadsheet
        xml                       ; the enterprise strikes back
        (sql +lsp)                ; the lingua franca of databases
        (graphql +lsp)            ; give queries a REST
        (protobuf +lsp)           ; Google's binary Esperanto
        prisma                    ; ORM, schema, and the envy of ActiveRecord
        lookml                    ; SQL with extra steps and a Looker subscription
        conf                      ; .ini (and anything ending in rc) walked so .yaml could stumble

        ;;; Markup & Diagrams
        markdown                  ; writing docs for people to ignore
        mermaid                   ; turns out writing docs leads to drawing diagrams
        plantuml                  ; diagrams for confusing people more
        graphviz                  ; diagrams for confusing yourself even more
        gnuplot                   ; and a graph to prove nobody understood any of it

        ;;; Infrastructure
        (docker +lsp)             ; it works on my machine, so we ship the machine
        (terraform +lsp)          ; infrastructure as code (and code as infrastructure bugs)
        (jsonnet +lsp)            ; JSON, but with variables
        (nix +lsp)                ; I hereby declare "nix geht mehr!"
        helm                      ; kubectl apply -f {{ .Values.anxiety }}
        nginx                     ; the little engine("x") that could
        systemd                   ; the init system that ate Linux
        makefile                  ; building software with tabs since 1976
        git-modes                 ; git's own dotfiles

        ;;; Proof & Scientific
        lean                      ; for folks with too much to prove
        coq                       ; proofs-as-programs
        (r +lsp)                  ; emacs speaks statistics
        (julia +lsp)              ; a better, faster MATLAB
        jupyter                   ; attempting to tame notebooks with emacs (currently failing)
        netlogo                   ; agent-based modeling (no, not LLM agents)
        processing                ; creative coding sketchbook

        :misc
        (key-logger               ; what are you even typing?
         +freq                    ; how often you smash that key
         +commands)               ; and which ones you're smashing
        ;; sicp                      ; the wizard book
        ))

(provide 'activate)
;;; activate.el ends here
