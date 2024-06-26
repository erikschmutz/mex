open Printf
open Utils.Util
open Utils.Common
open Utils.Runtime
open Utils.ReadConfig
open Utils.Benchprog

module Alg1RunSys = RuntimeSystem (Main.Algorithm1)
open Alg1RunSys



let run = fun () -> 
   let count = int_of_string Sys.argv.(1) in
   let node = int_of_string Sys.argv.(2) in
   let info = take (count + 1) (readConfiguration "/Users/erikrehn/Desktop/skola/mex/mex/Settings.txt")
   in 
   if node <> -1 then (
      let bench_file = Sys.argv.(3) in
   let p = prog_of_bench bench_file in
   main info node p      
   ) else
      main info node Skip
      