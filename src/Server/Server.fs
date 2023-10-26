module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module Storage =
    let todos = ResizeArray()
    let exercices = ResizeArray()

    let addTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

    let addExercice exercice =
      exercices.Add  exercice

    let addAllExercices () =
      for lhs in 1..10 do
        for rhs in 1..10 do
          addExercice (Exercice.create Multiplication lhs rhs (lhs * rhs))

    do
        addTodo (Todo.create "Create new SAFE project")
        |> ignore

        addTodo (Todo.create "Write your app") |> ignore
        addTodo (Todo.create "Ship it !!!") |> ignore
        addAllExercices ()

// let todosApi =
let serverApi =
    {
      getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
      addTodo =
        fun todo ->
            async {
                return
                    match Storage.addTodo todo with
                    | Ok () -> todo
                    | Error e -> failwith e
            }
//             }

// let exercicesAPI = {
      getNextExercices = fun () -> async { return Storage.exercices |> List.ofSeq }
      newResult = fun exercice -> async { () }
  }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    // |> Remoting.fromValue todosApi
    // |> Remoting.fromValue exercicesAPI
    |> Remoting.fromValue serverApi 
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0