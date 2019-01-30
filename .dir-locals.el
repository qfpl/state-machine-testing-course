(("coffee-state" . ((nil . ((dante-target . "coffee-state")))))
 (nil . ((dante-repl-command-line
          . ("nix-shell" "--run"
             (concat "cabal new-repl " dante-target " --builddir=dist/dante"))))))
