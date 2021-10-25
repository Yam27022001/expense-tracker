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

-- whenever we have to make a new variable we have to use let in  block 

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
       h1 [] [text "TRACK YOUR BUDGET"]
       , div[ style "display" "flex"
            , style "flex-direction" "column"
            , style "border" "2px solid green"
            , style "padding-left" "10px"
            , style "padding-right" "10px"
            , style "padding-bottom" "10px"
            , style "width" "400px" 
            ] [
           h2 [style "font-weight" "normal"
               , style "font-size" "20px"][text "Please Enter Your Budget"] 
           , input [value (String.fromInt model.budget), onInput Budget] []
           , button [style "margin-top" "12px"
                    , style "border" "2px solid green"
                    , style "width" "100px"
                    , style "color" "green"] [text "Calculate"]
       ]
       ,div [style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "left"
            , style "text-align" "center"]
        [
            div[] 
            [
                h2[] [text "Budget"]
                , img[src "images/budget.jpg"
                  , style "width" "100px"
                  , style "height" "100px"
                ][]
                , div[] [text (String.fromInt model.budget)]
            ]
        , div[] 
            [
                h2[] [text "Expense"]
                , img [src "images/expense.png"  
                    , style "width" "100px"
                    , style "height" "100px"                              
                    ][]
                , div[] [text (String.fromInt model.expense)] 
            ]
        , div[] 
            [
                h2[] [text "Balance"]
                , img [src "images/balance.svg"
                    , style "width" "100px"
                    , style "height" "100px"
                    ][] 
                , div[] [text (String.fromInt model.balance)] 
            ]
        ]
        , div [style "display" "flex"
                , style "flex-direction" "column"
                , style "border" "2px solid red"
                , style "padding-left" "10px"
                , style "padding-right" "10px"
                , style "padding-bottom" "10px"
                , style "width" "400px" 
                , style "margin-top" "10px"
            ][
                h2 [style "font-weight" "normal"
                   , style "font-size" "20px"
                ] [text "Please Enter Your Expense"]
                , input [] []
                , h2 [style "font-size" "20px"
                    , style "font-weight" "normal"
                ] [text "Please Enter Your Expense Amount"]
                , input [] []
                , button [style "margin-top" "12px"
                    , style "border" "2px solid red"
                    , style "width" "100px"
                    , style "color" "red"
                ][text "Add"]
              
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


    