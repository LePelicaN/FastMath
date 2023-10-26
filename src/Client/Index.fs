module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
  Todos: Todo list;
  Exercices: Exercice list;
  Input: string
  InputResult: string
  PreviousResult: string option // Use strong type
  }

type Msg =
    | GotTodos of Todo list
    | GotExercices of Exercice list
    | SetInput of string
    | SetInputResult of string
    | AddTodo
    | ValidateResult of unit // TODO: have the result returned by the server?
    | SendResult
    | AddedTodo of Todo

//let todosApi =
let serverApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    // |> Remoting.buildProxy<ITodosApi>
    |> Remoting.buildProxy<IServerApi>

//let exercicesApi =
//    Remoting.createApi ()
//    |> Remoting.withRouteBuilder Route.builder
//    |> Remoting.buildProxy<IExercicesApi>

let init () : Model * Cmd<Msg> =
    let model = {
      Todos = [];
      Exercices = [];
      Input = ""
      InputResult = ""
      PreviousResult = None
      }

    //let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos
    let cmd = Cmd.OfAsync.perform serverApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos ->
      //let cmd = Cmd.OfAsync.perform exercicesApi.getNextExercices () GotExercices
      let cmd = Cmd.OfAsync.perform serverApi.getNextExercices () GotExercices
      { model with Todos = todos }, cmd
    | GotExercices exercices ->
      { model with Exercices = model.Exercices @ exercices }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | SetInputResult value -> { model with InputResult = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input

        //let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo
        let cmd = Cmd.OfAsync.perform serverApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | SendResult ->
        let result: Exercice = {model.Exercices.Head with Result = Correct}
        let cmd = Cmd.OfAsync.perform serverApi.newResult result ValidateResult
        { model with
            PreviousResult = (if int(model.InputResult) = model.Exercices.Head.ResultValue then Some "Good" else Some "KO")
            InputResult = ""
            Exercices = model.Exercices.Tail
          }, cmd
    | ValidateResult _ ->
        model, Cmd.none
    | AddedTodo todo -> { model with Todos = model.Todos @ [ todo ] }, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let operationBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Bulma.label [
                text.hasTextCentered
                prop.text "FastMath"
              ]
            if Option.isSome model.PreviousResult then
              Bulma.label [
                  text.hasTextCentered
                  prop.text (Option.get model.PreviousResult)
                ]
            if model.Exercices.IsEmpty then
              Bulma.label [
                  text.hasTextCentered
                  prop.text "No more exercices :("
                ]
            else
              Bulma.label [
                  text.hasTextCentered
                  prop.text model.Exercices.Head.Lhs
                ]
              Bulma.label [
                  text.hasTextCentered
                  let operatorSign =
                    match model.Exercices.Head.Operation with
                    | Operation.Addition -> "+"
                    | Operation.Substraction -> "-"
                    | Operation.Multiplication -> "*"
                    | Operation.Division -> "/"
                  prop.text operatorSign
                ]
              Bulma.label [
                  text.hasTextCentered
                  prop.text model.Exercices.Head.Rhs
                ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.InputResult
                            prop.placeholder "Result"
                            prop.onChange (fun x -> SetInputResult x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        // Not result but response perhaps?
                        prop.disabled (Exercice.isValid model.InputResult |> not)
                        prop.onClick (fun _ -> dispatch SendResult)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "FastMath"
                            ]
                            containerBox model dispatch
                            operationBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]