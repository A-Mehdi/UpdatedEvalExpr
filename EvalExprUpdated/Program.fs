// Learn more about F# at http://fsharp.org

open System.Numerics
open FParsec
open HelperFunctions

let int128Max = 170141183460469231731687303715884105727I
let int128Min = -170141183460469231731687303715884105728I
let int256Max = (abs int128Min) * (abs int128Min) * 2I - 1I
let int256Min = -(int256Max) - 1I
let uint256Max = (int256Max + 1I) * 2I - 1I
let uint128Max = 340282366920938463463374607431768211455I
let int32Max = 2147483647I
let int32Min = -2147483648I
let uint32Max = bigint System.UInt32.MaxValue
let int64Max = 9223372036854775807I
let int64Min = -9223372036854775808I
let uint64Max = 18446744073709551615I
let floatMax = bigint System.Double.MaxValue
let floatMin = bigint System.Double.MinValue
let int8Min = -128I
let int8Max = 127I
let int16Min = -32768I
let int16Max = 32767I


module HelperFunctions =

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

type Numbers =
  | I8 of int8
  | UI8 of uint8
  | I16 of int16
  | UI16 of uint16
  | I32 of int
  | UI32 of uint32
  | I64 of int64
  | UI64 of uint64
  | I128 of BigInteger
  | UI128 of BigInteger
  | I256 of BigInteger
  | UI256 of BigInteger
  | F32 of float32
  | F64 of float
  | NError of string * int64

module Numbers =
  let getValue = function
    | I8 a -> string(a)
    | UI8 a -> string(a)
    | I16 a -> string(a)
    | UI16 a -> string(a)
    | I32 a -> string(a)
    | UI32 a -> string(a)
    | I64 a -> string(a)
    | UI64 a -> string(a)
    | I128 a -> string(a)
    | UI128 a -> string(a)
    | I256 a -> string(a)
    | UI256 a -> string(a)
    | F32 a -> string(a)
    | F64 a -> string(a)
    | NError (a, b) -> a

type Size =
  | B8
  | B16
  | B32
  | B64
  | B128
  | B256
  | BF32
  | BF64
  | SError

module Size =
  let getPriority = function
    | B8 -> 1
    | B16 -> 2
    | B32 -> 3
    | B64 -> 4
    | B128 -> 5
    | B256 -> 6
    | BF32 -> 7
    | BF64 -> 8
    | SError -> 9

type DataType =
  | Signed of Size
  | Unsigned of Size
  | Float of Size
  | CError of Size

module DataType =
  let getType = function
    | Signed _ -> 0
    | Unsigned _ -> 1
    | Float _ -> 2
    | CError _ -> 3

  let getIntegerRange = function
    | Signed B8 -> (-128I, 127I)
    | Unsigned B8 -> (0I, 255I)
    | Signed B16 -> (-32768I, 32767I)
    | Unsigned B16 -> (0I, 65535I)
    | Signed B32 -> (int32Min, int32Max)
    | Unsigned B32 -> (0I, uint32Max)
    | Signed B64 -> (int64Min, int64Max)
    | Unsigned B64 -> (0I, uint64Max)
    | Signed B128 -> (int128Min, int128Max)
    | Unsigned B128 -> (0I, uint128Max)
    | Signed B256 -> (int256Min, int256Max)
    | Unsigned B256 -> (0I, uint256Max)
    | _ -> (-1I, -1I)

  let getNextSignedInt = function
    | 1 -> Signed B16
    | 2 -> Signed B32
    | 3 -> Signed B64
    | 4 -> Signed B128
    | 5 -> Signed B256
    | _ -> CError SError

  let getSize = function
    | Signed a -> a
    | Unsigned a -> a
    | Float a -> a
    | CError _ -> SError

  let wrapValue dataType value =
    match dataType with
    | Signed B8 -> Signed B8, I8 (int8(value))
    | Unsigned B8 -> Unsigned B8, UI8 (uint8(value))
    | Signed B16 -> Signed B16, I16 (int16(value))
    | Unsigned B16 -> Unsigned B16, UI16 (uint16(value))
    | Signed B32 -> Signed B32, I32 (int32(value))
    | Unsigned B32 -> Unsigned B32, UI32 (uint32(value))
    | Signed B64 -> Signed B64, I64 (int64(value))
    | Unsigned B64 -> Unsigned B64, UI64 (uint64(value))
    | Signed B128 -> Signed B128, I128 value
    | Unsigned B128 -> Unsigned B128, UI128 value
    | Signed B256 -> Signed B256, I256 value
    | Unsigned B256 -> Unsigned B256, UI256 value
    | _ -> CError SError, NError ("Wrong Input", 1L)

module Operate =
  open Size
  open DataType
  open Numbers

  let getUpcast a b =
    let priority1, type1 = getPriority (getSize (fst a)), getType (fst a)
    let priority2, type2 = getPriority (getSize (fst b)), getType (fst b)
    if type1 = type2 then
      if priority1 > priority2 then (fst a) else (fst b)
    elif type1 = 2 || type2 = 2 then
      if priority1 > priority2 then (fst a) else (fst b)
    elif type1 = 3 || type2 = 3 then
      if priority1 > priority2 then (fst a) else (fst b)
    else
      let unsigned_val = if (type1 = 1) then a else b
      let signed_val = if (type1 = 0) then a else b
      let upR = getPriority (getSize (fst unsigned_val))
      let pR = getPriority (getSize (fst signed_val))
      let ur1, ur2 = getIntegerRange (fst unsigned_val)
      let r1, r2 = getIntegerRange (fst signed_val)
      let uvalue = BigInteger.Parse (getValue (snd unsigned_val))
      let svalue = BigInteger.Parse (getValue (snd signed_val))
      if pR >= upR && uvalue >= r1 && uvalue <= r2 then
        fst signed_val
      elif upR >= pR && svalue >= ur1 && svalue <= ur2 then
        fst unsigned_val
      else
        getNextSignedInt (max upR pR)

  let fixValue rep value =
    match rep with
    | Signed _ ->
      let r1, r2 = getIntegerRange rep
      if value >= r1 && value <= r2 then
        wrapValue rep value
      elif value > r2 then
        let cycle = abs r1 + r2 + 1I
        let remainder = value % cycle
        if remainder <= r2 then
          wrapValue rep remainder
        else
          let value = remainder - cycle
          wrapValue rep value
      else
        let cycle = abs r1 + r2 + 1I
        let remainder = value % cycle
        if remainder >= r1 then
          wrapValue rep remainder
        else
          let value = remainder + cycle
          wrapValue rep value
    | Unsigned _ ->
      let r1, r2 = getIntegerRange rep
      if value >= r1 && value <= r2 then
        wrapValue rep value
      elif value > r2 then
        let value = value % (r2 + 1I)
        wrapValue rep value
      else
        let value = value % (r2 + 1I)
        if value = 0I then
          wrapValue rep value
        else
          let value = r2 + value + 1I
          wrapValue rep value
    | _ -> CError SError, NError ("Wrong Input", 1L)

  let convertfromFloat rep (value : float) =
    match rep with
    | Signed B8 -> Signed B8, I8 (int8(value))
    | Unsigned B8 -> Unsigned B8, UI8 (uint8(value))
    | Signed B16 -> Signed B16, I16 (int16(value))
    | Unsigned B16 -> Unsigned B16, UI16 (uint16(value))
    | Signed B32 -> Signed B32, I32 (int(value))
    | Unsigned B32 -> Unsigned B32, UI32 (uint32(value))
    | Signed B64 -> Signed B64, I64 (int64(value))
    | Unsigned B64 -> Unsigned B64, UI64 (uint64(value))
    | Signed B128 -> Signed B128, I128 (bigint value)
    | Unsigned B128 -> fixValue (Unsigned B128) (bigint value)
    | Signed B256 -> Signed B256, I256 (bigint value)
    | Unsigned B256 -> fixValue (Unsigned B256) (bigint value)
    | Float BF32 -> Float BF32, F32 (float32(value))
    | Float BF64 -> Float BF64, F64 (float(value))
    | _ -> CError SError, NError ("Error", 1L)

  let convertFromBigint rep value =
    match rep with
    | Signed _ | Unsigned _ -> fixValue rep value
    | Float BF32 | Float BF64 ->
      if value >= floatMin && value <= floatMax then
        convertfromFloat rep (float value)
      else
        (CError SError, NError ("Out of range", 1L))
    | _ -> CError SError, NError ("Error", 1L)

  let convert curRep nextRep value =
    match curRep with
    | Signed _ | Unsigned _ ->
      let value = BigInteger.Parse value
      convertFromBigint nextRep value
    | Float _ -> convertfromFloat nextRep (float(value))
    | CError SError -> CError SError, NError (value, 1L)
    | _ -> CError SError, NError (value, 1L)

  let shift val1 val2 op (pos : Position) =
    match fst val2 with
    | Signed _ | Unsigned _ | Float _ ->
      let right_side = float (getValue (snd val2))
      if (right_side - float(right_side)) = 0.0 then
        let flag = getType (fst val2)
        let right_side =
          if (flag = 2) then (BigInteger.Parse (string(right_side)))
          else (BigInteger.Parse (getValue (snd val2)))
        let right_side = fixValue (Signed B32) right_side
        let right_side = int(getValue (snd right_side))
        match fst val1 with
        | Signed _ | Unsigned _ ->
          let left_side = BigInteger.Parse (getValue (snd val1))
          let result = op left_side right_side
          fixValue (fst val1) result
        | Float _ ->
          let left_side = float (getValue (snd val1))
          if left_side - float(int64(left_side)) = 0.0 then
            let left_side = BigInteger.Parse (string(left_side))
            let result = op left_side right_side
            fixValue (Signed B64) result
          else
            (CError SError,
              NError ("Left side of shift cannot be float", pos.Column))
        | CError _ -> val1
      else
        CError SError, NError ("Right side of shift must be int32", pos.Column)
    | CError _ -> val2

  let add x y =
    match x, y with
    | (CError _, _) , _ -> x
    | _, (CError _, _) -> y
    | _, _ ->
      let nextRep = getUpcast x y
      match nextRep with
      | Signed _ | Unsigned _ ->
        let val1 = BigInteger.Parse (getValue (snd x))
        let val2 = BigInteger.Parse (getValue (snd y))
        let result = val1 + val2
        fixValue nextRep result
      | Float BF32 ->
        Float BF32,
          F32 (float32 (getValue (snd x)) + float32 (getValue (snd y)))
      | Float BF64 ->
        Float BF64,
          F64 (float (getValue (snd x)) + float (getValue (snd y)))
      | CError _ ->
        let val1 = BigInteger.Parse (getValue (snd x))
        let val2 = BigInteger.Parse (getValue (snd y))
        fixValue (Unsigned B256) (val1 + val2)
      | _ -> CError SError, NError ("Add error", 1L)

  let sub x y =
    match x, y with
    | (CError _, _) , _ -> x
    | _, (CError _, _) -> y
    | _, _ ->
      let nextRep = getUpcast x y
      match nextRep with
      | Signed _ | Unsigned _ ->
        let val1 = BigInteger.Parse (getValue (snd x))
        let val2 = BigInteger.Parse (getValue (snd y))
        let result = val1 - val2
        fixValue nextRep result
      | Float BF32 ->
        Float BF32,
          F32 (float32 (getValue (snd x)) - float32 (getValue (snd y)))
      | Float BF64 ->
        Float BF64,
          F64 (float (getValue (snd x)) - float (getValue (snd y)))
      | CError _ ->
        let val1 = BigInteger.Parse (getValue (snd x))
        let val2 = BigInteger.Parse (getValue (snd y))
        let type1 = getType (fst x)
        if type1 = 1 then
          fixValue (Unsigned B256) (val1 - val2)
        else
          fixValue (Signed B256) (val1 - val2)
      | _ -> CError SError, NError ("Sub error", 1L)

  let mul x y =
    match x, y with
    | (CError _, _) , _ -> x
    | _, (CError _, _) -> y
    | _, _ ->
      let nextRep = getUpcast x y
      match nextRep with
      | Signed _ | Unsigned _ ->
        let val1 = BigInteger.Parse (getValue (snd x))
        let val2 = BigInteger.Parse (getValue (snd y))
        let result = val1 * val2
        fixValue nextRep result
      | Float BF32 ->
        Float BF32,
          F32 (float32 (getValue (snd x)) * float32 (getValue (snd y)))
      | Float BF64 ->
        Float BF64,
          F64 (float (getValue (snd x)) * float (getValue (snd y)))
      | CError _ ->
        let val1 = BigInteger.Parse (getValue (snd x))
        let val2 = BigInteger.Parse (getValue (snd y))
        fixValue (Signed B256) (val1 * val2)
      | _ -> CError SError, NError ("Mul error", 1L)

  let div x y (pos : Position) =
    match x, y with
    | (CError _, _) , _ -> x
    | _, (CError _, _) -> y
    | _, _ ->
      if float (getValue (snd y)) = 0.0 then
        CError SError, NError ("Cannot divide by zero", pos.Column)
      else
        let nextRep = getUpcast x y
        match nextRep with
        | Signed _ | Unsigned _ ->
          let val1 = BigInteger.Parse (getValue (snd x))
          let val2 = BigInteger.Parse (getValue (snd y))
          let result = val1 / val2
          fixValue nextRep result
        | Float BF32 ->
          Float BF32,
            F32 (float32 (getValue (snd x)) / float32 (getValue (snd y)))
        | Float BF64 ->
          Float BF64,
            F64 (float (getValue (snd x)) / float (getValue (snd y)))
        | CError _ ->
          let val1 = BigInteger.Parse (getValue (snd x))
          let val2 = BigInteger.Parse (getValue (snd y))
          fixValue (Signed B256) (val1 / val2)
        | _ -> CError SError, NError ("Div error", 1L)

  let modulo x y (pos : Position) =
    match x, y with
    | (CError _, _) , _ -> x
    | _, (CError _, _) -> y
    | _, _ ->
      if float (getValue (snd y)) = 0.0 then
        CError SError, NError ("Cannot divide by zero", pos.Column)
      else
        let nextRep = getUpcast x y
        match nextRep with
        | Signed _ | Unsigned _ ->
          let val1 = BigInteger.Parse (getValue (snd x))
          let val2 = BigInteger.Parse (getValue (snd y))
          let result = val1 % val2
          fixValue nextRep result
        | Float BF32 ->
          Float BF32,
            F32 (float32 (getValue (snd x)) % float32 (getValue (snd y)))
        | Float BF64 ->
          Float BF64,
            F64 (float (getValue (snd x)) % float (getValue (snd y)))
        | CError _ ->
          let val1 = BigInteger.Parse (getValue (snd x))
          let val2 = BigInteger.Parse (getValue (snd y))
          fixValue (Signed B256) (val1 % val2)
        | _ -> CError SError, NError ("Modulo error", 1L)


module Parser =
  open Operate
  open Numbers

  let numberformat =
    NumberLiteralOptions.AllowFraction |||
    NumberLiteralOptions.AllowFractionWOIntegerPart |||
    NumberLiteralOptions.AllowBinary |||
    NumberLiteralOptions.AllowHexadecimal |||
    NumberLiteralOptions.AllowMinusSign |||
    NumberLiteralOptions.AllowOctal |||
    NumberLiteralOptions.AllowSuffix

  let pV =
    numberLiteral numberformat "number" .>>. getPosition .>> spaces
    |>> fun (nl, pos) ->
          if nl.HasFraction then (Float BF64, F64 (float nl.String))
          else
            let value = calculateValue nl.String
            let ch1 = nl.SuffixChar1
            let ch2 = string (nl.SuffixChar1) + string (nl.SuffixChar2)
            let ch3 = ch2 + string (nl.SuffixChar3)
            if nl.SuffixLength = 1 && (ch1 = 'u' || ch1 = 'U') then
              if value >= 0I && value <= uint32Max then
                (Unsigned B32, UI32 (uint32 value))
              else
                (CError SError, NError ("Out of range", pos.Column))
            elif nl.SuffixLength = 1 && (ch1 = 'I' || ch1 = 'L') then
              if value >= int64Min && value <= int64Max then
                (Signed B64, I64 (int64 value))
              else
                (CError SError, NError ("Out of range", pos.Column))
            elif nl.SuffixLength = 2 && (ch2 = "uI" || ch2 = "UL") then
              if value >= 0I && value <= uint64Max then
                (Unsigned B64, UI64 (uint64 value))
              else
                (CError SError, NError ("Out of range", pos.Column))
            elif nl.SuffixLength = 2 && (ch2 = "II" || ch2 = "LL") then
              if value >= int128Min && value <= int128Max then
                (Signed B128, I128 (value))
              else
                (CError SError, NError ("Out of range", pos.Column))
            elif nl.SuffixLength = 3 && (ch3 = "uII" || ch3 = "ULL") then
              if value >= 0I && value <= uint128Max then
                (Unsigned B128, UI128 (value))
              else
                (CError SError, NError ("Out of range", pos.Column))
            elif value >= int32Min && value <= int32Max then
              (Signed B32, I32 (int nl.String))
            elif value >= int64Min && value <= int64Max then
              (Signed B64, I64 (int64 nl.String))
            elif value >= int128Min && value <= int128Max then
              (Signed B128, I128 (value))
            elif value >= int256Min && value <= int256Max then
              (Signed B256, I256 (value))
            elif value > int256Max && value <= uint256Max then
              (Unsigned B256, UI256 (value))
            else
              (CError SError, NError ("Out of range", pos.Column))

  let ws = spaces
  let str_ws s = pstring s >>. ws

  let opp =
    new OperatorPrecedenceParser<DataType*Numbers, Position, unit>()
  let expr = opp.ExpressionParser
  let term = pV <|> between (str_ws "(") (str_ws ")") expr

  do
    opp.TermParser <- term

    opp.AddOperator(
      InfixOperator("+", getPosition .>> ws, 2, Associativity.Left,
        fun x y -> (add x y))
    )

    opp.AddOperator(
      InfixOperator("-", getPosition .>> ws, 2, Associativity.Left,
        fun x y -> (sub x y))
    )
    opp.AddOperator(
      InfixOperator("/", getPosition .>> ws, 3, Associativity.Left, (),
        fun pos x y -> (div x y pos))
    )
    opp.AddOperator(
      InfixOperator("%", getPosition .>> ws, 3, Associativity.Left, (),
        fun pos x y -> (modulo x y pos))
    )
    opp.AddOperator(
      InfixOperator(">>", getPosition .>> ws, 1, Associativity.Left, (),
        fun pos x y -> shift x y (>>>) pos)
    )
    opp.AddOperator(
      InfixOperator("<<", getPosition .>> ws, 1, Associativity.Left, (),
        fun pos x y -> shift x y (<<<) pos)
    )
    opp.AddOperator(
      InfixOperator("*", getPosition .>> ws, 3, Associativity.Left,
        fun x y -> (mul x y))
    )
    opp.AddOperator(
      PrefixOperator("(int8)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Signed B8) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(uint8)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Unsigned B8) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(int16)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Signed B16) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(uint16)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Unsigned B16) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(int)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Signed B32) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(int32)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Signed B32) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(uint32)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Unsigned B32) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(int64)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Signed B64) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(uint64)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Unsigned B64) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(int128)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Signed B128) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(uint128)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Unsigned B128) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(int256)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Signed B256) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(uint256)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Unsigned B256) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(float32)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Float BF32) (getValue (snd x)))
    )
    opp.AddOperator(
      PrefixOperator("(float)", getPosition .>> ws, 4, true,
        fun x -> convert (fst x) (Float BF64) (getValue (snd x)))
    )

open Parser
open Numbers
open System

let final str =
  match run expr str with
    | Success ((CError _, NError (a, b)), _, _) ->
      let result = str + "\n"
      let space = (String.replicate (int(b) - 2) " ") + "^"
      let fin = result + space + "\n" + a
      printfn "%s" fin
      [|fin|]
    | Success (v, _, p) ->
      if p.Column <> int64 (str.Length + 1) then
        let result = str + "\n"
        let space = (String.replicate (int(p.Column) - 2) " ") + "^"
        let fin =
          result + space + "\n" + "Expecting: Digit, Suffix or Operator"
        [|fin|]
      else
        [|getValue (snd v)|]
    | Failure (v, _, _) ->
      [|v|]


let rec concatenate res arg flag =
  match arg with
  | [] -> (res, flag)
  | hd :: tail ->
    if hd = "" then

      concatenate res tail flag
    elif res = "" then
      concatenate (res + hd) tail flag
    else
      let last_char = res.[res.Length - 1]
      let first_char = hd.[0]
      if Char.IsDigit last_char && Char.IsDigit first_char then
        concatenate (res + " " + hd) tail (res.Length)
      else
        concatenate (res + hd) tail flag



[<EntryPoint>]
let main argv =
    let b = final "0x0235"
    let a = concatenate "" ["5"; "+"; "5"; "5"] 0
    printfn "%A" a
    printfn "%A" b
    0 // return an integer exit code
