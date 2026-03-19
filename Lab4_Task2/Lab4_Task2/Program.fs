open System

type BinaryTree<'T> =
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>
    | Empty

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

let rec readNatural () =
    let input = Console.ReadLine()
    let success, n = Int32.TryParse(input)

    if success && n > 0 then
        n
    else
        printf "Ошибка: Число не натуральное. Повторите ввод: "
        readNatural ()

//Инфиксный обход
let rec printInOrder tree =
    match tree with
    | Empty -> ()
    | Node(data, left, right) ->
        printInOrder left
        printfn "Node %O" data
        printInOrder right

let rec foldTree f state tree =
    match tree with
    | Empty -> state
    | Node(data, left, right) ->
        let state1 = foldTree f state left
        let state2 = f state1 data left right
        foldTree f state2 right

let sumLeaves tree =
    foldTree (fun acc data left right ->
        match left, right with
        | Empty, Empty when data % 2 = 0 -> acc + data
        | _ -> acc
    ) 0 tree

let generateTree n =
    let rand = Random()
    let rec build count tree =
        if count = 0 then
            tree
        else
            build (count - 1) (insert (rand.Next(1, 100)) tree)
    build n Empty

[<EntryPoint>]
let main argv =
    printf "Введите количество элементов: "
    let N = readNatural()

    let myTree = generateTree N
    printfn "Исходное дерево: "
    printInOrder myTree

    let sum = sumLeaves myTree
    printfn "Сумма четных значений в листьях: %i" sum
    0