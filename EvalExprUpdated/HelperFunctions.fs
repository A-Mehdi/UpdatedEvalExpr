module HelperFunctions
open System.Numerics

let table = function
  | 0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8"
  | 9 -> "9"
  | 10 -> "A"
  | 11 -> "B"
  | 12 -> "C"
  | 13 -> "D"
  | 14 -> "E"
  | 15 -> "F"
  | _ -> ""

let table2 = function
  | '0' -> "0000"
  | '1' -> "1000"
  | '2' -> "0100"
  | '3' -> "1100"
  | '4' -> "0010"
  | '5' -> "1010"
  | '6' -> "0110"
  | '7' -> "1110"
  | '8' -> "0001"
  | '9' -> "1001"
  | 'A' -> "0101"
  | 'B' -> "1101"
  | 'C' -> "0011"
  | 'D' -> "1011"
  | 'E' -> "0111"
  | 'F' -> "1111"
  | _ -> ""

let table3 = function
  | '0' -> "000"
  | '1' -> "100"
  | '2' -> "010"
  | '3' -> "110"
  | '4' -> "001"
  | '5' -> "101"
  | '6' -> "011"
  | '7' -> "111"
  | _ -> ""



let rec rev_str (str : string) index res =
  if index = str.Length then res
  else rev_str (str) (index + 1) (res + string str.[str.Length - 1 - index])

let rec turnHextoBinary (str : string) index res =
  if index = str.Length then res
  else turnHextoBinary str (index + 1) (res + table2 str.[index])

let rec turnOctaltoBinary (str : string) index res =
  if index = str.Length then res
  else turnOctaltoBinary str (index + 1) (res + table3 str.[index])

let rec removeZero (str : string) =
  if str.[str.Length - 1] = '0' then removeZero str.[.. str.Length - 2]
  else str

let rec turnBinaryto128Bigint (str : string) index res =
  if index = str.Length then res
  else
    let sign = if (index = 127) then (-1I) else 1I
    let cur = BigInteger.Parse (string str.[index])
    let add = sign * (pown 2I (index)) * cur
    turnBinaryto128Bigint str (index + 1) (add + res)

let rec turnBinaryto256Bigint (str : string) index res =
  if index = str.Length then res
  else
    let sign = if (index = 255) then (-1I) else 1I
    let cur = BigInteger.Parse (string str.[index])
    let add = sign * (pown 2I (index)) * cur
    turnBinaryto256Bigint str (index + 1) (add + res)

let stringToBigint (str : string) =
  let final_str =
    match str.[0 .. 1] with
    | "0x" | "0X" ->
      let a = rev_str str.[2 ..] 0 ""
      let a = turnHextoBinary a 0 ""
      a
    | "0b" | "0B" ->
      let a = rev_str str.[2 ..] 0 ""
      a
    | "0o" | "0O" ->
      let a = rev_str str.[2 ..] 0 ""
      let a = turnOctaltoBinary a 0 ""
      let a = removeZero ("0o" + a)
      let a = a.[2 ..]
      let a = if (a = "") then "0" else a
      a
    | _ -> ""
  let rep = "0b"
  if final_str.Length <= 32 then
    let num = (rep + (rev_str final_str 0 ""))
    let value = int num
    bigint value
  elif final_str.Length <= 64 then
    let value = int64 (rep + (rev_str final_str 0 ""))
    bigint value
  elif final_str.Length <= 128 then
    let value = turnBinaryto128Bigint final_str 0 0I
    value
  elif final_str.Length <= 256 then
    let value = turnBinaryto256Bigint final_str 0 0I
    value
  else
    -1I

let calculateValue (str : string) =
  let rep = if (str.Length >= 2) then (str.[0 .. 1]) else ""
  if rep = "0x" || rep = "0X" || rep = "0o" || rep = "0O" ||
    rep = "0b" || rep = "oB" then
    stringToBigint (str)
  else
    BigInteger.Parse str
