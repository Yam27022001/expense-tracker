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
        ,index : Int
        ,tempExpenseItem : ExpenseItem
        ,expenseItems : List ExpenseItem
    }

type alias ExpenseItem = 
    {   
        index :Int
        ,expenseItem : String
        ,expenseAmount : Int

    }


init : Model
init = 
    {
       budget = 0
        ,expense = 0
        ,balance = 0
        ,index = 0
        ,tempExpenseItem = ExpenseItem 0 ""  0
        ,expenseItems = []
    }  


--UPDATE
type Msg = 
    Budget String
    | UpdateExpenseItem String
    | UpdateExpenseAmount String
    | Add
    | Delete Int
        


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

       
        Add   -> 
            let
                updatedExpense = model.expense + model.tempExpenseItem.expenseAmount
                currentIndex = model.index + 1
                updatedExpenseItems =  model.expenseItems ++ [ExpenseItem model.tempExpenseItem.index model.tempExpenseItem.expenseItem model.tempExpenseItem.expenseAmount]
                updatedtempExpenseItem= ExpenseItem currentIndex "" 0
            in
            {  model
                | index= model.index + 1
                ,expenseItems = updatedExpenseItems
                , expense =  updatedExpense
                , balance = (model.budget - updatedExpense) 
                ,tempExpenseItem = updatedtempExpenseItem
            }
        
        Delete index ->
            let 
                updatedExpenseItems1 = List.filter (\expenseItem-> expenseItem.index /= index) model.expenseItems
                updatedExpenseItems2 = List.filter (\expenseItem-> expenseItem.index == index) model.expenseItems
            in

                case (List.head updatedExpenseItems2) of
                    Just x ->
                        let
                            z = model.expense - x.expenseAmount
                        in
                        {model 
                        |  expenseItems = updatedExpenseItems1 
                        , expense = z
                        , balance = model.budget - z}
                    Nothing ->
                        { model 
                        |  expenseItems = updatedExpenseItems1 }


toTableRow : ExpenseItem -> Html Msg
toTableRow expenseItem =
    tr []
        [ td [] [ text (String.fromInt expenseItem.index)]
        , td [] [ text expenseItem.expenseItem ]
        , td [] [ text (String.fromInt expenseItem.expenseAmount)]
        , td [] [
            button [onClick (Delete expenseItem.index)] [text "delete"]
        ]
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
                    [ th [] [text "S.no"]
                    , th [] [ text "Expense" ]
                    , th [] [ text "Amount" ]
                    ]
                  ]
                    ++ List.map toTableRow model.expenseItems  
                )
        ]
    ]