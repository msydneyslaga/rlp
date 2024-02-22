#!/usr/bin/env clisp

(let* ((paths (directory "dist-newstyle/build/*/*/rlp-*/build/"))
       (n (length paths)))
  (cond ((< 1 n) (error ">1 build directories found. run `cabal clean`."))
        ((< n 1) (error "no build directories found. this shouldn't happen lol"))
        (t       (format t "~A" (car paths)))))

