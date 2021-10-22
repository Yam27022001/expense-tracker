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


type Msg = Budget String

-- whenever we have to make 

update : Msg -> Model -> Model
update msg model =
   
    case msg of 
        Budget budget -> 
            -- { model | budget = (String.toInt budget)}
             let 
                currentBudget = (Maybe.withDefault 0 (String.toInt budget))
            in
            { model | budget = currentBudget 
                    , balance = (currentBudget - model.expense)
            }
            
   
    

-- VIEW


view : Model -> Html Msg
view model =
    div []
    [
       h1 [] [text "Enter the Budget"]
       , input [value (String.fromInt model.budget), onInput Budget] []
       , button [] [text "Calculate"]
       , div[] 
            [
                h2[] [text "Budget"]
                , div[] [text (String.fromInt model.budget)]
            ]
        , div[] 
            [
                h2[] [text "Expense"]
                , div[] [text (String.fromInt model.expense)] 
            ]
        , div[] 
            [
                h2[] [text "Balance"]
                , div[] [text (String.fromInt model.balance)] 
            ]
    ]
    -- div []
    -- [ h1 [] [text "Enter the Budget"]
    -- , input [value model.budget] []
    -- , button [onClick Budget] [text "Calculate"]
    -- , div [] 
    --     [ h2 [] [text "Budget"]
    --     , div [] [ text (  model.budget) ]
    --     ]
    -- , div [] 
    --     [ h2 [] [text "Expense"]
    --     , div [] [ text ( String.fromInt model.expense) ]
    --     ]
    -- ,  div [] 
    --     [ h2 [] [text "Balance "]
    --     , div [] [ text ( String.fromInt model.balance) ]
    --     ]
    -- ]


    