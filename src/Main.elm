module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias TodoItem =
    { id : Int
    , text : String
    , isDone : Bool
    }


type alias Model =
    { todoText : String
    , nextTodoId : Int
    , todos : List TodoItem
    }


init : Model
init =
    { todoText = ""
    , nextTodoId = 6
    , todos =
        [ { id = 0, text = "Buy milk", isDone = False }
        , { id = 1, text = "Make dinner", isDone = False }
        , { id = 2, text = "Clean a house", isDone = False }
        , { id = 3, text = "Wash clothes", isDone = False }
        , { id = 4, text = "Fix car", isDone = False }
        , { id = 5, text = "Watch TV", isDone = False }
        ]
    }


type Msg
    = Change String
    | Add
    | ToggleDone Int
    | Remove Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newTodoText ->
            { model | todoText = newTodoText }

        Add ->
            addTodo model

        ToggleDone todoId ->
            { model | todos = toggleDone model.todos todoId }

        Remove todoId ->
            { model | todos = removeTodo model.todos todoId }


addTodo : Model -> Model
addTodo model =
    if model.todoText == "" then
        model

    else
        { model
            | todos = model.todos ++ [ { id = model.nextTodoId, text = model.todoText, isDone = False } ]
            , todoText = ""
            , nextTodoId = model.nextTodoId + 1
        }


toggleDone : List TodoItem -> Int -> List TodoItem
toggleDone todos todoId =
    let
        toggleTodo todo =
            if todo.id == todoId then
                { todo | isDone = not todo.isDone }

            else
                todo
    in
    List.map toggleTodo todos


removeTodo : List TodoItem -> Int -> List TodoItem
removeTodo todos todoId =
    let
        remove todo =
            if todo.id == todoId then
                False

            else
                True
    in
    List.filter remove todos


view : Model -> Html Msg
view model =
    div [ class "todoList" ]
        (renderTodoInput model ++ renderTodos model)


renderTodoInput : Model -> List (Html Msg)
renderTodoInput model =
    [ div [ class "todoInput" ]
        [ input [ placeholder "Add todo..", value model.todoText, onInput Change ] []
        , button [ onClick Add ] [ text "Add" ]
        ]
    ]


renderTodos : Model -> List (Html Msg)
renderTodos model =
    List.map renderTodo model.todos


renderTodo : TodoItem -> Html Msg
renderTodo todo =
    div [ class "todoItem" ]
        [ div
            [ class
                (if todo.isDone then
                    "done"

                 else
                    "undone"
                )
            ]
            [ text (String.fromInt (todo.id + 1) ++ ". " ++ todo.text) ]
        , div [ class "todoActions" ]
            [ button [ class "checkTodo", onClick (ToggleDone todo.id) ] [ text "MARK" ]
            , button [ class "removeTodo", onClick (Remove todo.id) ] [ text "REMOVE" ]
            ]
        ]
