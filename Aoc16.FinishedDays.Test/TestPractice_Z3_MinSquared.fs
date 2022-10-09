module Aoc16.FinishedDays.Test.TestPractice_Z3_MinSquared
open Aoc16.Practice_Z3_MinSquared

open Aoc16.Input
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()
    
[<Test>]
[<TestCase>]
let Test1() = 
    let sln1 = solve
    Assert.That(sln1, Is.EqualTo "abc")