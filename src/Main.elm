module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Random
import Random.List exposing (shuffle)
import Tuple exposing (pair)

main = Browser.element
    { init = init_flags
    , view = view
    , subscriptions = \_ -> Sub.none
    , update = update
    }

type Direction
    = Left
    | Right

type alias Board = Array (Room, Bool)

type Room
    = Occupied
    | Unoccupied

type alias Model =
    { board : Board
    , history: List Board
    }

type Msg
    = SetBoard Board
    | ToggleRoom Int
    | Move
    | MoveDraculas (List Direction)
    | Reset

init_flags: () -> (Model, Cmd Msg)
init_flags _ = init

init = (init_model, Random.generate SetBoard <| random_board 5)

init_model : Model
init_model =
    { board = Array.empty
    , history = []
    }

random_board : Int -> Random.Generator Board
random_board n = 
    Random.int 1 (n-1)
    |>
    Random.map (\f -> (List.repeat f Occupied)++(List.repeat (n-f) Unoccupied))
    >>
    Random.andThen shuffle
    >>
    Random.map
        (  List.map (\r -> (r,False))
        >> Array.fromList
        )

nocmd m = (m, Cmd.none)

update msg model = case msg of
    SetBoard board -> { init_model | board = board } |> nocmd
    ToggleRoom i -> toggle_room i model |> nocmd
    Move -> move model
    MoveDraculas directions -> move_draculas directions model |> nocmd
    Reset -> init

num_selected model = (Array.length << Array.filter Tuple.second) model.board
num_draculas model = (Array.length << Array.filter ((==) Occupied) << Array.map Tuple.first) model.board

toggle_room : Int -> Model -> Model
toggle_room i model = 
    let
        max_selected = num_draculas model
    in
        { model | board = Array.indexedMap (\j -> \(r,selected) -> (r, if j==i && (num_selected model < max_selected || selected) then not selected else selected)) model.board }

move model = (peek model, Random.generate MoveDraculas (Random.list (Array.length model.board) (Random.map (\i -> if i==0 then Left else Right) (Random.int 0 1))))

peek : Model -> Model
peek model =
    let
        update_room (room,selected) = (if (room==Occupied)==selected then Unoccupied else Occupied, False)
        nboard = Array.fromList <| List.map update_room (Array.toList model.board)
    in
        { model | board = nboard, history = model.board::model.history }

move_draculas : List Direction -> Model -> Model
move_draculas directions model =
    let
        rooms = Array.toList <| Array.map (Tuple.first) model.board
        n = Array.length model.board
        step i d = 
            if i==0 then 1 else if i==n-1 then n-2 else case d of
                Left -> i-1
                Right -> i+1
        nrooms =
            List.foldl 
                (\(i,r,d) -> \b -> if r==Occupied then Array.set (step i d) Occupied b else b)
                (Array.repeat n Unoccupied) 
                (List.map3 (\i -> \r -> \d -> (i,r,d)) (List.range 0 (n-1)) rooms directions)

        q = Debug.log "nrooms" (nrooms, directions)
    in
        { model | board = Array.map (\r -> (r, False)) nrooms }

found_them_all : Model -> Bool
found_them_all model = (Array.toList >> List.map Tuple.first >> List.all ((==) Unoccupied)) model.board

view model = 
    div
        [ HA.id "game"]
        (  [table model]
        ++ (if found_them_all model then [finished_message model] else [show_status model])
        )

table model =
    Html.table []
    (  (List.map (view_old_board (found_them_all model)) (List.reverse model.history))
    ++ (if found_them_all model then [] else 
        [ view_current_board model.board
        , Html.tr 
            [] 
            [ Html.td 
                [HA.colspan (Array.length model.board)] 
                [ Html.button 
                    [ HE.onClick Move
                    , HA.disabled (num_selected model == 0)
                    ] 
                    [Html.text "peek"]
                ]
            ]
        ]
        )
--    ++ [Html.div [] [Html.text (Debug.toString model)]]
    )

view_current_board : Board -> Html Msg
view_current_board board =
    Html.tr
        [HA.class "current-board"]
        (List.indexedMap view_current_room (Array.toList board))

view_current_room : Int -> (Room, Bool) -> Html Msg
view_current_room i (room, selected) = 
    Html.td 
        [ HE.onClick (ToggleRoom i)
        , HA.attribute "role" "checkbox"
        , HA.attribute "aria-checked" (if selected then "true" else "false")
        , HA.attribute "aria-label" ("Look in room "++(String.fromInt i))
        , HA.attribute "tabindex" "0"
        , HA.class "door"
        ]
        [ Html.text (if selected then "🔑" else "🚪")]

show_status model =
    let
        n = num_draculas model
        text = if n==1 then "There is one Dracula in the castle." else "There are "++(String.fromInt n)++" Draculas in the castle."
    in
        Html.p [HA.id "status"] [Html.text text]

finished_message model = 
    Html.div
        []
        [ Html.p
            [HA.id "finished"]
            [Html.text "You found all the Draculas!"]
        , Html.button
            [ HA.id "restart"
            , HE.onClick Reset
            ]
            [Html.text "Start again"]
        ]

view_old_board : Bool -> Board -> Html Msg
view_old_board finished board =
    Html.tr
        []
        (List.map (view_old_room finished) (Array.toList board))

view_old_room : Bool -> (Room, Bool) -> Html Msg
view_old_room finished (room, selected) = 
    let
        symbol = case (room, selected) of
            (Occupied, True) -> "⚰️"
            (Unoccupied, True) -> "🦇"
            (Occupied, False) -> if finished then "🧛" else "🚪"
            (Unoccupied, False) -> "🚪"
    in
        Html.td [] [Html.text symbol]
