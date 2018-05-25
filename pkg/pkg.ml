#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let _cli= Conf.with_pkg "cli"

(* TODO generate man file, see opam config list | grep man*)

let opams =
  [Pkg.opam_file ~lint_deps_excluding:(Some ["odoc"]) "omktorrent.opam"]

let () =
  Pkg.describe "omktorrent" ~opams @@ fun c ->
  let cli = Conf.value c _cli in
  Ok [ Pkg.mllib ~api:["Omgtorrent"] "lib/omgtorrent.mllib";
       Pkg.test "test/alcotest_lib";
       Pkg.bin ~cond:cli "app/omktorrent"; ]
