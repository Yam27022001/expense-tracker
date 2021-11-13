module Main exposing(..)

import Browser
import Array
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
     ,editable : Bool
     
 }


init : Model
init = 
    {
    budget = 0
    ,expense = 0
    ,balance = 0
    ,index = 0
    ,tempExpenseItem = ExpenseItem 0 ""  0 False
    ,expenseItems = []
    }  


--UPDATE


type Msg = 
    Budget String
    | Add
    | UpdateExpenseItem String
    | UpdateExpenseAmount String
    | Delete Int 
    | Edit Int 
    | CancelEdit Int
    | UpdateAmount Int String
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
                updatedExpenseItems =  model.expenseItems ++ [ExpenseItem model.tempExpenseItem.index model.tempExpenseItem.expenseItem model.tempExpenseItem.expenseAmount True]
                updatedtempExpenseItem= ExpenseItem currentIndex "" 0 False
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
            let 
                updatedExpenseItems1 = List.filter (\expenseItem-> expenseItem.index /= index) model.expenseItems
                dummy2 = Debug.log "dump" updatedExpenseItems1
                updatedExpenseItems2 = List.filter (\expenseItem-> expenseItem.index == index) model.expenseItems
                dummy3 = Debug.log "dump1" updatedExpenseItems2
                myArray = Array.fromList updatedExpenseItems2
                dummy_ = Debug.log "dump2" myArray
                item = Array.get 0 (myArray)
                dummy4 = Debug.log "dump3" item
                actualExpense = Maybe.withDefault item
                -- dummy5 = Debug.log "dump4" actualExpense
              

                
               
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
                        {model 
                        |  expenseItems = updatedExpenseItems1 }
        
        Edit index ->
            let
                updatedExpenseItems2 = List.filter (\expenseItem-> expenseItem.index == index) model.expenseItems
                listHead = List.take index model.expenseItems
                listTail = List.drop (index + 1) model.expenseItems
            in
                case (List.head updatedExpenseItems2) of
                    Just x ->
                        let
                            expenseItem = ExpenseItem x.index x.expenseItem x.expenseAmount True
                            updatedExpenseItems = listHead ++ [expenseItem] ++ listTail
                        in
                        {model 
                        |  expenseItems = updatedExpenseItems
                        , expense = model.expense
                        , balance = model.balance}


                    Nothing ->
                        model
           
            
        CancelEdit index ->
            let
                updatedExpenseItems2 = List.filter (\expenseItem-> expenseItem.index == index) model.expenseItems
                listHead = List.take index model.expenseItems
                listTail = List.drop (index + 1) model.expenseItems
            in
                case (List.head updatedExpenseItems2) of
                    Just x ->
                        let
                            expenseItem = ExpenseItem x.index x.expenseItem x.expenseAmount False
                            updatedExpenseItems = listHead ++ [expenseItem] ++ listTail
                        in
                        {model 
                        |  expenseItems = updatedExpenseItems
                        , expense = model.expense
                        , balance = model.balance}


                    Nothing ->
                        model
        
        UpdateAmount index value ->
            let
                updatedExpenseItems3 = List.filter (\expenseItem-> expenseItem.index == index) model.expenseItems
                listHead = List.take index model.expenseItems
                listTail = List.drop (index + 1) model.expenseItems
                currentBudget = (Maybe.withDefault 0 (String.toInt value))
                
            in
                case (List.head updatedExpenseItems3) of
                    Just x ->
                        let
                            expenseItem = ExpenseItem x.index x.expenseItem currentBudget x.editable
                            updatedExpenseItems = listHead ++ [expenseItem] ++ listTail
                        in
                        {model 
                        |  expenseItems = updatedExpenseItems
                        , expense = model.expense
                        , balance = model.balance }


                    Nothing ->
                        model 



        
            


toTableRow : ExpenseItem -> Html Msg
toTableRow expenseItem =
    if expenseItem.editable == True then
        toTableRowEdit expenseItem
    else 
        toTableRowNonEdit expenseItem 

toTableRowEdit : ExpenseItem -> Html Msg
toTableRowEdit expenseItem =
    tr []
        [ td [] [ text (String.fromInt expenseItem.index)]
        , td [] [ text expenseItem.expenseItem ]
        , td [] [ text (String.fromInt expenseItem.expenseAmount) ]
        , td [] [
            input[type_ "text", value (String.fromInt expenseItem.expenseAmount) , onInput (UpdateAmount expenseItem.index) ] []
            ,button [onClick (CancelEdit expenseItem.index)] [text "Cancel"]
            , button [] [text "Submit"]
        ]
        ]   

toTableRowNonEdit : ExpenseItem -> Html Msg
toTableRowNonEdit expenseItem =
    tr []
        [ td [] [ text (String.fromInt expenseItem.index)]
        , td [] [ text expenseItem.expenseItem ]
        , td [] [ text (String.fromInt expenseItem.expenseAmount) ]
        , td [] [
            button [onClick (Delete expenseItem.index)] [text "delete"]
            , button [onClick (Edit expenseItem.index)] [text "edit"]
        ]
        ]   

validateInput : Model -> Html Msg
validateInput model = 
    if model.budget  == 0  then 
    div[style "color" "red"][text "Budget Field should be greater than zero"]
    else
    div [] []


-- VIEW


view : Model -> Html Msg
view model =
    div []
    [
       h1 [] [text "TRACK YOUR BUDGET"]
       , div[class "form1"] [
           h2 [][text "Please Enter Your Budget"] 
           , input [value (String.fromInt model.budget), onInput Budget] []
           , validateInput model
           
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
                , input [value model.tempExpenseItem.expenseItem , onInput UpdateExpenseItem] []  --model or expenseItem
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


    