;;; racket-pcomplete.el pcomplete for racket and raco

;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Copyright (c) 2013-2015 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.
;; URL: https://github.com/greghendershott/racket-mode

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

;;; Code:
(require 'pcomplete)

(defconst racket--raco-command-options
  '(("check-requires" . (("-k"
                          "--show-keep"
                          "-b"
                          "--show-bypass"
                          "-u"
                          "--show-uses")))
    ("ctool" . (("--cc"
                 "--ld"
                 "-x"
                 "-xform"
                 "--c-mods"
                 "--3m"
                 "--cgc"
                 "-n"
                 "--name"
                 "-d"
                 "--tool"
                 "--compiler"
                 "++ccf"
                 "--ccf"
                 "--ccf-clear"
                 "--ccf-show"
                 "--linker"
                 "++ldf"
                 "--ldf"
                 "--ldf-clear"
                 "--ldf-show"
                 "++ldl"
                 "--ldl-show"
                 "++cppf"
                 "--cppf"
                 "--cppf-clear"
                 "--cppf-show"
                 "++lib"
                 "-v"
                 "--vv")))
    ("decompile" . (("--force"
                     "--columns"
                     "--help"
                     "-h")))
    ("demodularize" . (("-e"
                        "--exclude-modules"
                        "-o"
                        "-g"
                        "--garbage-collect")))
    ("dependencies-graph" . (()))
    ("distribute" . (("--collects-path"
                      "++collects-copy"
                      "-v")))
    ("docs" . (()))
    ("exe" . (("-o"
               "--gui"
               "-l"
               "--launcher"
               "--config-path"
               "--collects-path"
               "--collects-dest"
               "--ico"
               "--icns"
               "--orig-exe"
               "--3m"
               "--cgc"
               "++aux"
               "++lib"
               "++exf"
               "--exf"
               "--exf-clear"
               "--exf-show"
               "-v"
               "--vv")))
    ("expand" . (("--columns")))
    ("link" . (("-l"
                "--list"
                "-n"
                "--name"
                "-d"
                "--root"
                "-D"
                "--static-root"
                "-x"
                "--version-regexp"
                "-r"
                "--remove"
                "-u"
                "--user"
                "-i"
                "--installation"
                "-f"
                "--file"
                "-v"
                "--version"
                "--repair")))
    ("make" . (("-j"
                "--disable-inline"
                "--disable-constant"
                "--no-deps"
                "-p"
                "--prefix"
                "--no-prim"
                "-v"
                "--vv")))
    ("pack" . (("--collect"
                "--plt-name"
                "--replace"
                "--at-plt"
                "--all-users"
                "--force-all-users"
                "--include-compiled"
                "++setup"
                "-v")))
    ("pkg" . (("install"
               "update"
               "remove"
               "show"
               "migrate"
               "create"
               "config"
               "catalog-show"
               "catalog-copy"
               "catalog-archive"
               "archive")))
    ("planet" . (("create"
                  "install"
                  "remove"
                  "show"
                  "clearlinks"
                  "fileinject"
                  "link"
                  "unlink"
                  "fetch"
                  "url"
                  "open"
                  "structure"
                  "print")))
    ("scribble" . (("--html"
                    "--htmls"
                    "--html-tree"
                    "--latex"
                    "--pdf"
                    "--dvipdf"
                    "--latex-section"
                    "--text"
                    "--markdown"
                    "--dest"
                    "--dest-name"
                    "--dest-base"
                    "++convert"
                    " ++style"
                    "--style"
                    "--prefix"
                    "++extra"
                    "--redirect-main"
                    "--redirect"
                    "+m"
                    "++main-xref-in"
                    "++xref-in"
                    "--info-out"
                    "++info-in"
                    "++arg"
                    "--quiet")))
    ("setup" . (("--only"
                 "-l"
                 "--pkgs"
                 "-P"
                 "--doc-index"
                 "--tidy"
                 "-c"
                 "--clean"
                 "-n"
                 "--no-zo"
                 "--trust-zos"
                 "-x"
                 "--no-launcher"
                 "-F"
                 "--no-foreign-libs"
                 "-i"
                 "--no-install"
                 "-I"
                 "--no-post-install"
                 "-d"
                 "--no-info-domain"
                 "-D"
                 "--no-docs"
                 "--doc-pdf"
                 "-K"
                 "--no-pkg-deps"
                 "--check-pkg-deps"
                 "--fix-pkg-deps"
                 "--unused-pkg-deps"
                 "-U"
                 "--no-user"
                 "--no-planet"
                 "--avoid-main"
                 "-j"
                 "--jobs"
                 "--workers"
                 "-v"
                 "--verbose"
                 "-m"
                 "--make-verbose"
                 "-r"
                 "--compile-verbose"
                 "--mode"
                 "--fail-fast"
                 "-p"
                 "--pause"
                 "-A"
                 "--force"
                 "-a"
                 "--all-users")))
    ("show-dependencies" . (("-c"
                             "--context"
                             "-f"
                             "--file"
                             "-m"
                             "--module-path"
                             "-x"
                             "--exclude"
                             "-X"
                             "--exclude-deps"
                             "-b")))
    ("test" . (("--collection"
                "-c"
                "--lib"
                "-l"
                "--package"
                "-p"
                "--modules"
                "-m"
                "--drdr"
                "--submodule"
                "-s"
                "--run-if-absent"
                "-r"
                "--no-run-if-absent"
                "-x"
                "--first-avail"
                "--direct"
                "--process"
                "--place"
                "--jobs"
                "-j"
                "--timeout"
                "--fresh-user"
                "--empty-stdin"
                "--quiet-program"
                "-Q"
                "--check-stderr"
                "-e"
                "++ignore-stderr"
                "--quiet"
                "-q"
                "--heartbeat"
                "--table"
                "-t")))
    ("unpack" . (("-l"
                  "--list"
                  "-c"
                  "--config"
                  "-f"
                  "--force")))))

(defconst racket--raco-pkg-command-options
  '(("install" . (("-t"
                   "--type"
                   "-n"
                   "--name"
                   "--checksum"
                   "--deps"
                   "--auto"
                   "--update-deps"
                   "--ignore-implies"
                   "--link"
                   "--static-link"
                   "--copy"
                   "--source"
                   "--binary"
                   "--binary-lib"
                   "--scope"
                   "-i"
                   "--installation"
                   "-u"
                   "--user"
                   "--scope-dir"
                   "--catalog"
                   "--skip-installed"
                   "--pkgs"
                   "--all-platforms"
                   "--force"
                   "--ignore-checksums"
                   "--strict-doc-conflicts"
                   "--no-cache"
                   "--no-setup"
                   "-j"
                   "--jobs"
                   "--fail-fast")))
    ("update" . (("-a"
                  "--all"
                  "--lookup"
                  "-t"
                  "--type"
                  "-n"
                  "--name"
                  "--checksum"
                  "--deps"
                  "--auto"
                  "--update-deps"
                  "--ignore-implies"
                  "--link"
                  "--static-link"
                  "--copy"
                  "--source"
                  "--binary"
                  "--binary-lib"
                  "--scope"
                  "-i"
                  "--installation"
                  "-u"
                  "--user"
                  "--scope-dir"
                  "--catalog"
                  "--all-platforms"
                  "--force"
                  "--ignore-checksums"
                  "--strict-doc-conflicts"
                  "--no-cache"
                  "--no-setup"
                  "-j"
                  "--jobs")))
    ("remove" . (("--demote"
                  "--force"
                  "--auto"
                  "--scope"
                  "-i"
                  "--installation"
                  "-u"
                  "--user"
                  "--scope-dir"
                  "--no-setup"
                  "-j"
                  "--jobs")))
    ("show" . (("-a"
                "--all"
                "-d"
                "--dir"
                "--scope"
                "-i"
                "--installation"
                "-u"
                "--user"
                "--scope-dir"
                "-v"
                "--version")))
    ("migrate" . (("--deps"
                   "--source"
                   "--binary"
                   "--binary-lib"
                   "--scope"
                   "-i"
                   "--installation"
                   "-u"
                   "--user"
                   "--scope-dir"
                   "--catalog"
                   "--all-platforms"
                   "--force"
                   "--ignore-checksums"
                   "--strict-doc-conflicts"
                   "--no-cache"
                   "--no-setup"
                   "-j"
                   "--jobs")))
    ("create" . (("--from-dir"
                  "--from-install"
                  "--format"
                  "--manifest"
                  "--as-is"
                  "--source"
                  "--binary"
                  "--binary-lib"
                  "--built"
                  "--dest")))
    ("config" . (("--set"
                  "--scope"
                  "-i"
                  "--installation"
                  "-u"
                  "--user"
                  "--scope-dir")))
    ("catalog-show" . (("--all"
                        "--only-names"
                        "--modules"
                        "--catalog"
                        "-v"
                        "--version")))
    ("catalog-copy" . (("--from-config"
                        "--force"
                        "--merge"
                        "--override"
                        "--relative"
                        "-v"
                        "--version")))
    ("catalog-archive" . (("--from-config"
                           "--state"
                           "-v"
                           "--version"
                           "--relative")))
    ("archive" . (("--include-deps"
                   "--exclude"
                   "--relative"))))
  "List of raco commands and options.")

(defconst racket--raco-commands
  (cons "help" (mapcar 'car racket--raco-command-options))
  "All raco commands.")

(defun racket--raco-get-options (command)
  "Get raco command's options."
  (cons "--help" (cadr (assoc command racket--raco-command-options))))

(defconst racket--raco-pkg-commands
  (mapcar 'car racket--raco-pkg-command-options)
  "All raco pkg commands.")

(defun racket--raco-pkg-get-options (command)
  "Get raco pkg command's options."
  (cons "--help" (cadr (assoc command racket--raco-pkg-command-options))))

;; TODO raco-planet-options
;; TODO pcomplete/racket

;;;###autoload
(defun pcomplete/raco ()
  (let ((command (nth 1 pcomplete-args)))
    (pcomplete-here* racket--raco-commands)
    (when (member command racket--raco-commands)
      (cond
       ((or (pcomplete-match "^-" 0)
            (pcomplete-match "^+" 0))
        (pcomplete-here (racket--raco-get-options command)))
       ((or (string= command "pkg")
            (string= command "planet"))
        (pcomplete-here (racket--raco-get-options command)))
       ((string= command "help")
        (pcomplete-here (remove "help" racket--raco-commands)))))
    (let ((subcommand (nth 2 pcomplete-args)))
      (when (and (string= command "pkg")
                 (member subcommand racket--raco-pkg-commands))
        (when (pcomplete-match "^-" 0)
          (pcomplete-here (racket--raco-pkg-get-options subcommand)))))
    (pcomplete-here (pcomplete-entries))))

(provide 'racket-pcomplete)
;;; racket-pcomplete.el ends here
