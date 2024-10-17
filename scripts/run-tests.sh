#!/bin/sh

set -e

echo ==== Test Docs ====
awk 'BEGIN { P=0 } { if ($0 ~ /^..begin_src *html/) { P=1 } else { if ($0 ~ /^..end_src/) { P=0 } ; if (P > 0) { print $0 } } } ' README.org > test.html

awk 'BEGIN { P=0 ; NAME="" } { if (tolower($0) ~ /^..name: /) {split($0,a,":") ; NAME=a[2] } else { if (tolower($0) ~ /^..begin_src *clojure.*-t/) { P=1 } else { if ($0 ~ /^..end_src/) { if (P > 0) { print "(if (not= *1 *2) (throw (new AssertionError (str \"FAIL" NAME " expected: \\n\" (pr-str *1) \"\\n\\nactual:\n\" (pr-str *2) \"\\n\\n\"))))\n\n" } ; P=0 ; NAME="" } ; if (P > 0) { print $0 } } } } ' `find . -iname '*.org'` | clojure -A:test >/dev/null

rm test.html

if [ "$1" = "all" ] ; then
    for v in 1.11 1.10 1.9 1.12 master ; do
        echo ==== Test Clojure $v ====
        clojure -M:test:runner:$v
        if [ $? -ne 0 ] ; then
            exit 1
        fi
    done
else
    if [ "$1" != "cljs" ] ; then
        echo ==== Test Clojure ====
        clojure -M:test:runner
    fi
fi

echo ==== Test ClojureScript ====
rm -rf cljs-test-runner-out/*
clojure -M:test:cljs-runner

