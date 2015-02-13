[<AutoOpen>]
module Common

open System

///Contains useful infix operators
[<AutoOpen>]
module Operators = 

    ///Compare two strings, ignoring case
    let (===) x y = (String.Compare (x, y, StringComparison.OrdinalIgnoreCase)) = 0

