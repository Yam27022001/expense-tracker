module Main exposing(..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MAIN


main =
  Browser.sandbox { init = init, update = update , view = view }


-- MODEL


type alias Model =
  {  budget : Int
    ,expense : Int
    ,balance : Int
  }


init : Model
init = 
    {
    budget = 0
    ,expense = 0
    ,balance = 0}  


--UPDATE


type Msg = Budget Int 


update : Msg -> Model -> Model
update msg model =
   case msg of 
    

-- VIEW


view : Model -> Html Msg
view model =
    div []
    [ h1 [] [text "Enter the Budget"]
    , input [value model.budget]
    , button [onClick Budget] [text "Calculate"]
    , div [] 
        [ h2 [] [text "Budget"]
        , div [] [  ( String.toInt model.budget) ]
        ]
    , div [] 
        [ h2 [] [text "Expense"]
        , div [] [ text ( model.expense) ]
        ]
    ,  div [] 
        [ h2 [] [text "Balance "]
        , div [] [ text ( model.balance) ]
        ]
    ]


    