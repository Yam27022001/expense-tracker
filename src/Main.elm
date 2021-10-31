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
    ,tempExpenseItem : ExpenseItem 
    ,index : Int
    ,expenseItems : List ExpenseItem
    }

type alias ExpenseItem = 
 {  
      index : Int
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
    | Add
    | UpdateExpenseItem String
    | UpdateExpenseAmount String
    | Delete Int 
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

        UpdateExpenseItem name -> 
            -- { model | budget = (String.toInt budget)}
             let 
                currentexpenseItem = model.tempExpenseItem
                updatedExpenseItem = {currentexpenseItem | expenseItem = name}
                dummy = Debug.log "tuple" currentexpenseItem
                dummy1 = Debug.log "tuple" name
            in
             
            { model | tempExpenseItem = updatedExpenseItem
                    
            }

        UpdateExpenseAmount amount -> 
            -- { model | budget = (String.toInt budget)}
             let 
                currentexpenseItem = model.tempExpenseItem
                currentBudget = (Maybe.withDefault 0 (String.toInt amount))
                updatedExpenseItem = {currentexpenseItem | expenseAmount = currentBudget}
              
            in
            { model | tempExpenseItem = updatedExpenseItem}
        Add   -> 
            let
                dummy = Debug.log "dump tuple" model.tempExpenseItem

                updatedExpense = model.expense + model.tempExpenseItem.expenseAmount
                currentIndex = model.index + 1
                dummy1 = Debug.log "dump" currentIndex
                updatedExpenseItems =  model.expenseItems ++ [ExpenseItem model.tempExpenseItem.index model.tempExpenseItem.expenseItem model.tempExpenseItem.expenseAmount]
                updatedtempExpenseItem= ExpenseItem currentIndex "" 0
                _ = Debug.log "dump" updatedExpenseItems


            in 
          
            {  model
                | index= model.index + 1
                ,expenseItems = updatedExpenseItems
                , expense =  updatedExpense
                , balance = (model.budget - updatedExpense) 
                ,tempExpenseItem = updatedtempExpenseItem
            }
            

        Delete index ->

           {model 
            |  expenseItems = List.filter (\expenseItem-> expenseItem.index /= index) model.expenseItems
           }


        
            


toTableRow : ExpenseItem -> Html Msg
toTableRow expenseItem =
    tr []
        [ td [] [ text (String.fromInt expenseItem.index)]
        , td [] [ text expenseItem.expenseItem ]
        , td [] [ text (String.fromInt expenseItem.expenseAmount) ]
        , td [] [
            button [onClick (Delete expenseItem.index)] [text "delete"]
        ]
        ]   

-- VIEW


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
                , img [src "images/expense.png"][]
                , div[] [text (String.fromInt model.expense)] 
            ]
        , div[] 
            [
                h2[] [text "Balance"]
                , img [src "images/balance.svg"][] 
                , div[] [text (String.fromInt model.balance)] 
            ]
        ]
        , div [ class "form2"][
                h2 [] [text "Please Enter Your Expense"]
                , input [value model.tempExpenseItem.expenseItem , onInput UpdateExpenseItem] []  --modal or expenseItem
                , h2 [] [text "Please Enter Your Expense Amount"]
                , input [value  (String.fromInt model.tempExpenseItem.expenseAmount ), onInput UpdateExpenseAmount] []
                
                , button [ onClick Add][text "Add"]
        
             ]
        , div [] [ 
             table
                []
                ([ thead []
                    [ th [] [text "S.no"]
                    , th [] [ text "Expense"]
                    , th [] [ text "Amount" ]
                    
                    ]
                ]
                    ++ List.map toTableRow model.expenseItems  --tries to combine that Html msg with a List.
                    
                )
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


    