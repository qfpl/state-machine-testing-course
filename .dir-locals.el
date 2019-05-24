(("coffee-state" . ((nil . ((dante-target . "coffee-state")))))
 ("level00" . ((nil . ((dante-target . "level00")))))
 ("level01" . ((nil . ((dante-target . "level01")))))
 ("level02" . ((nil . ((dante-target . "level02")))))
 ("level03" . ((nil . ((dante-target . "level03")))))
 ("level04" . ((nil . ((dante-target . "level04")))))
 ("level05" . ((nil . ((dante-target . "level05")))))
 ("level06" . ((nil . ((dante-target . "level06")))))
 (nil . ((dante-repl-command-line
          . ("nix-shell" "--run"
             (concat "cabal new-repl " dante-target " --builddir=dist/dante"))))))
