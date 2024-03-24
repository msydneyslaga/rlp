#!/usr/bin/env bb

(defn die [& msgs]
  (binding [*out* *err*]
    (run! println msgs))
  (System/exit 1))

(let [paths (map str (fs/glob "." "dist-newstyle/build/*/*/rlp-*/build"))
      n (count paths)]
  (cond (< 1 n) (die ">1 build directories found. run `cabal clean`.")
        (< n 1) (die "no build directories found. this shouldn't happen lol")
        :else   (-> (first paths) fs/real-path str println)))

