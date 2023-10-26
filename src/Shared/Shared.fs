namespace Shared

open System

type Operation =
  | Addition
  | Substraction
  | Multiplication
  | Division

type ExerciceResult =
  | NotDone
  | Correct
  | False

type Exercice = {
  Id: Guid;
  Operation: Operation;
  Lhs: int;
  Rhs: int;
  ResultValue: int;
  Result: ExerciceResult
  }

module Exercice =
    let isValid (result: string) =
        String.IsNullOrWhiteSpace result |> not
        // Check that it is a number

    let create (operation: Operation) (lhs: int) (rhs: int) (result: int) =
        { Id = Guid.NewGuid()
          Operation = operation
          Lhs = lhs
          Rhs = rhs
          ResultValue = result
          Result = NotDone          
        }

type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

//type ITodosApi = {
type IServerApi = {
      getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo>
//      }

//type IExercicesApi = {
      getNextExercices: unit -> Async<Exercice list>
      newResult: Exercice -> Async<unit>
      }