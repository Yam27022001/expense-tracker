module Main exposing(..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
    {
        budget : Int
    }


init : Model
init = 
    {
       budget = 0
    }  


--UPDATE
type Msg = Budget String


update : Msg -> Model -> Model
update msg model =

    case msg of 
        Budget budget -> 
            let 
                currentBudget = (Maybe.withDefault 0 (String.toInt budget))
            in
            { model | budget = currentBudget }

--VIEW

view : Model -> Html Msg
view model =
    div []
    [
       h1 [] [text "TRACK YOUR BUDGET"]
        , div[class "form1"] [
           h2 [][text "Please Enter Your Budget"] 
           , input [value (String.fromInt model.budget), onInput Budget] []
            ]
       ,div [ class "bug-exp-bal"]
        [
            div[] 
            [
                h2[] [text "Budget"]
                    , img[src "images/budget.jpg"][]
                , div[] [text (String.fromInt model.budget)]
            ]
        ]
    ]