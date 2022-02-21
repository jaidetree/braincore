#!/usr/bin/env nbb

(ns format-key-file
  "
  Reads googleauth.private.json parses it, stringifies it, then prints
  it to console.

  Usage:
  ./scripts/format_key_file.cljs | base64 | pbcopy
  "
  (:require
   ["fs" :as fs]))

(-> (.readFileSync fs "googleauth.private.json" #js {:encoding "utf-8"})
    (js/JSON.parse)
    (js/JSON.stringify)
    (js/console.log))
