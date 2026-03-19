open System

type BinaryTree<'T> =
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>
    | Empty

let rec readNatural () =
    let input = Console.ReadLine()
    let success, n = Int32.TryParse(input)

    if success && n > 0 then
        n
    else
        printf "Ошибка: Число не натуральное. Повторите ввод: "
        readNatural ()

let rec readChar () =
    let input = Console.ReadLine()
    let success, n = Char.TryParse(input)

    if success then
        n
    else
        printf "Ошибка: Введите ровно один символ: "
        readChar ()

let rec insert value tree =
    match tree with
    | Empty ->
        Node(value, Empty, Empty)
    | Node(v, left, right) ->
        if value < v then
            Node(v, insert value left, right)
        elif value > v then
            Node(v, left, insert value right)
        else
            tree

//Инфиксный обход
let rec printInOrder tree =
    match tree with
    | Empty -> ()
    | Node(data, left, right) ->
        printInOrder left
        printfn "Node %O" data
        printInOrder right

let rec mapTree f tree =
    match tree with
    | Empty -> Empty
    | Node(data, left, right) ->
        Node(f data, mapTree f left, mapTree f right)

let replaceLastChar (newChar: char) (str: string) =
    if String.IsNullOrEmpty(str) then
        str
    else
        str.Substring(0, str.Length - 1) + string newChar

let generateTree n =
    let rand = Random()
    let chars = "abcdefghijklmnopqrstuvwxyz"
    let randomString () =
        String.init (rand.Next(3, 8)) (fun _ -> string chars.[rand.Next(chars.Length)])
    
    let rec build count tree =
        if count = 0 then
            tree
        else
            build (count - 1) (insert (randomString()) tree)
    build n Empty

[<EntryPoint>]
let main argv =
    printf "Введите количество элемнетов: "
    let N = readNatural()

    let myTree = generateTree N
    printfn "Исходное дерево: "
    printInOrder myTree

    printfn "Введите символ для замены: "
    let myChar = readChar()

    let mappedTree = mapTree(replaceLastChar myChar) myTree
    printfn "Дерево после замены последнего символа: "
    printInOrder mappedTree
    0