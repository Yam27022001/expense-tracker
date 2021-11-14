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
        ,expense : Int
        ,balance : Int
        ,tempExpenseItem : ExpenseItem
        ,expenseItems : List ExpenseItem
    }

type alias ExpenseItem = 
    {  
        expenseItem : String
        ,expenseAmount : Int
    }


init : Model
init = 
    {
       budget = 0
        ,expense = 0
        ,balance = 0
        ,tempExpenseItem = ExpenseItem ""  0
        ,expenseItems = []
    }  


--UPDATE
type Msg = 
    Budget String
    | UpdateExpenseItem String
    | UpdateExpenseAmount String
    | Add
        


update : Msg -> Model -> Model
update msg model =

    case msg of 
        Budget budget -> 
            let 
                currentBudget = (Maybe.withDefault 0 (String.toInt budget))
            in
            { model | budget = currentBudget 
                , balance = (currentBudget - model.expense)
            }
        
        UpdateExpenseItem name -> 
            let 
                currentexpenseItem = model.tempExpenseItem
                updatedExpenseItem = {currentexpenseItem | expenseItem = name}
            in
            { model | tempExpenseItem = updatedExpenseItem }

        UpdateExpenseAmount amount -> 
            let 
                currentexpenseItem = model.tempExpenseItem
                currentBudget = (Maybe.withDefault 0 (String.toInt amount))
                updatedExpenseItem = {currentexpenseItem | expenseAmount = currentBudget}
            in
            { model | tempExpenseItem = updatedExpenseItem }

        Add  ->
            {  model
                | expenseItems=  model.expenseItems ++ [ExpenseItem model.tempExpenseItem.expenseItem model.tempExpenseItem.expenseAmount]
                , expense = model.tempExpenseItem.expenseAmount
            }

toTableRow : ExpenseItem -> Html Msg
toTableRow expenseItem =
    tr []
        [ td [] [ text expenseItem.expenseItem ]
        , td [] [ text (String.fromInt expenseItem.expenseAmount)]
        ]   

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
            , div[] 
            [
                h2[] [text "Expense"]
                , img [src "images/expense.jpg"][]
                , div[] [text (String.fromInt model.expense)] 
            ]
            , div[] 
            [
                h2[] [text "Balance"]
                , img [src "images/balance.jpg"][] 
                , div[] [text (String.fromInt model.balance)] 
            ]
        ]
        , div [ class "form2"][
                h2 [] [text "Please Enter Your Expense"]
                , input [value model.tempExpenseItem.expenseItem , onInput UpdateExpenseItem] []  
                , h2 [] [text "Please Enter Your Expense Amount"]
                , input [value  (String.fromInt model.tempExpenseItem.expenseAmount ), onInput UpdateExpenseAmount] []
                , button [ onClick Add][text "Add"]

            ]
        , div [] [ 
             table
                []
                ( [ thead []
                    [ th [] [ text "Expense" ]
                    , th [] [ text "Amount" ]
                    ]
                  ]
                    ++ List.map toTableRow model.expenseItems  
                )
        ]
    ]