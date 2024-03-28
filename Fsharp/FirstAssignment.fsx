// 1.1
let squareList (lst: int list) : int list =
    List.map (fun x -> x * x) lst

// 1.2
let filterEvenNumbers (lst: int list) : int list =
    List.filter (fun x -> x % 2 = 0) lst

// 1.3
let sumPositiveNumbers (lst: int list) : int =
    List.filter (fun x -> x > 0) lst
    |> List.sum

// 1.4
let capitalizeNames (lst: string list) : string list =
    List.map (fun name -> name.ToUpper()) lst

// 1.5
let filterStringsByLength (lst: string list) (n: int) : string list =
    List.filter (fun str -> String.length str > n) lst

// 1.6
let countDivisibleBy (lst: int list) (divisor: int) : int =
    List.filter (fun x -> x % divisor = 0) lst
    |> List.length

// 1.7
let findIndices (lst: 'a list) (element: 'a) : int list =
    lst
    |> List.mapi (fun index value -> if value = element then index else -1)
    |> List.filter (fun index -> index <> -1)

// 1.8
let concatenateStringsByLength (lst: string list) (n: int) : string =
    lst
    |> List.filter (fun str -> String.length str > n)
    |> List.reduce (fun acc str -> acc + str)

// 1.9
let findMaxValueTuple (lst: (int * int) list) : (int * int) option =
    match lst with
    | [] -> None
    | (id, value) :: rest ->
        let rec loop maxTuple maxVal remaining =
            match remaining with
            | [] -> Some maxTuple
            | (id, value) :: tail ->
                if value > maxVal then
                    loop (id, value) value tail
                else
                    loop maxTuple maxVal tail
        loop (id, value) value rest

// 1.10
let countOccurrences (lst: 'a list) : ('a * int) list =
    lst
    |> List.groupBy (fun x -> x)
    |> List.map (fun (key, values) -> (key, List.length values))

// 2.1
type TrafficLight =
    | Red
    | Yellow
    | Green

let getNextState (currentState: TrafficLight) : TrafficLight =
    match currentState with
    | Red -> Green
    | Yellow -> Red
    | Green -> Yellow

// 2.2
type ArithmeticOperation =
    | Add
    | Subtract
    | Multiply
    | Divide

let performArithmeticOperation (num1: float) (num2: float) (operation: ArithmeticOperation) : float =
    match operation with
    | Add -> num1 + num2
    | Subtract -> num1 - num2
    | Multiply -> num1 * num2
    | Divide -> num1 / num2

// 2.3
type Shape =
    | Circle of float
    | Rectangle of float * float
    | Square of float

let calculateArea (shape: Shape) : float =
    match shape with
    | Circle radius -> Math.PI * radius * radius
    | Rectangle width height -> width * height
    | Square side -> side * side

// 2.4
type TemperatureScale =
    | Celsius
    | Fahrenheit

let convertTemperature (temperature: float) (fromScale: TemperatureScale) (toScale: TemperatureScale) : float =
    match fromScale, toScale with
    | Celsius, Fahrenheit -> (temperature * 9.0 / 5.0) + 32.0
    | Fahrenheit, Celsius -> (temperature - 32.0) * 5.0 / 9.0
    | _ -> temperature

// 2.5
type JsonValue =
    | JsonObject of (string * JsonValue) list
    | JsonArray of JsonValue list
    | JsonString of string 
    | JsonNumber of float
    | JsonBoolean of bool

// Recursively prints a JSON value as a string.
// json is a parameter. It is the JSON value to be pretty printed.
// Returns a string representation of the pretty printed JSON value.
let rec prettyPrintJson (json: JsonValue) : string =
    let indent = "  "

    // Recursively generates the indentation string based on the level.
    // Returns the indentation string.
    let rec indentString (level: int) : string =
        String.replicate (level * 2) indent

    // Recursively pretty prints a JSON value with the specified indentation level.
    // Returns a string representation of the pretty printed JSON value.
    let rec prettyPrintValue (value: JsonValue) (level: int) : string =
        match value with
        | JsonObject properties ->
            let propertiesString =
                properties
                |> List.map (fun (key, value) -> indentString (level + 1) + "\"" + key + "\": " + prettyPrintValue value (level + 1))
                |> String.concat (",\n")
            "{\n" + propertiesString + "\n" + indentString level + "}"
        | JsonArray items ->
            let itemsString =
                items
                |> List.map (fun item -> indentString (level + 1) + prettyPrintValue item (level + 1))
                |> String.concat (",\n")
            "[\n" + itemsString + "\n" + indentString level + "]"
        | JsonString str -> "\"" + str + "\""
        | JsonNumber num -> string num
        | JsonBoolean true -> "true"
        | JsonBoolean false -> "false"

    prettyPrintValue json 0


// 3.1
let rec fibonacci (n: int) : int =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

// 3.2
let rec binarySearch (arr: 'a array) (target: 'a) (low: int) (high: int) : int option =
    if low > high then
        None
    else
        let mid = (low + high) / 2
        let midValue = arr.[mid]
        if midValue = target then
            Some mid
        elif midValue > target then
            binarySearch arr target low (mid - 1)
        else
            binarySearch arr target (mid + 1) high

let binarySearchArray (arr: 'a array) (target: 'a) : int option =
    binarySearch arr target 0 (Array.length arr - 1)

// 3.3
let rec mergeSort (lst: int list) : int list =
    match lst with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let splitIndex = lst.Length / 2
        let left = lst.[0..splitIndex-1]
        let right = lst.[splitIndex..]
        merge (mergeSort left) (mergeSort right)

and merge (left: int list) (right: int list) : int list =
    match left, right with
    | [], _ -> right
    | _, [] -> left
    | x::xs, y::ys ->
        if x <= y then
            x :: merge xs right
        else
            y :: merge left ys

// 3.4
let rec computeDepth (tree: 'a Tree) : int =
    match tree with
    | Leaf -> 0
    | Node (left, _, right) -> 1 + max (computeDepth left) (computeDepth right)

// 3.5
let rec isPalindrome (str: string) : bool =
    match str.Length with
    | 0 | 1 -> true
    | _ ->
        let firstChar = str.[0]
        let lastChar = str.[str.Length - 1]
        if firstChar = lastChar then
            isPalindrome (str.Substring(1, str.Length - 2))
        else
            false