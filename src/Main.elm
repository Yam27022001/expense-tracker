module Main exposing(..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder,field,map3,string,int)
import Json.Encode as Encode
import Http
import Task





-- MAIN
main:Program () Model Msg
main =
  Browser.element { init = init, update = update, view = view , subscriptions = \_ -> Sub.none}


-- MODEL

type alias Model =
    {
        budget : Int
        ,expense : Int
        ,balance : Int
        ,index : Int
        ,tempExpenseItem : ExpenseItem
        ,expenseItems : List ExpenseItem
        -- ,errorMessage : Maybe String
    }

type alias ExpenseItem = 
    {   
        index :Int
        ,expenseItem : String
        ,expenseAmount : Int
        -- ,editable : Bool
        }


init : () -> ( Model, Cmd Msg )
init _ = 
    ({
       budget = 0
        ,expense = 0
        ,balance = 0
        ,index = 0
        ,tempExpenseItem = ExpenseItem 0 ""  0 
        ,expenseItems = []
    --     , errorMessage = Nothing
    }  
      ,Cmd.none
            )
        


--UPDATE
type Msg = 
    Budget String
    | UpdateExpenseItem String
    | UpdateExpenseAmount String
    | Add
    | Delete Int
    | Edit Int 
    | CancelEdit Int
    | SendHttpRequest
    | DataReceived (Result Http.Error (List ExpenseItem))
    | ExpenseCreated (Result Http.Error ExpenseItem)
    -- | UpdateAmount Int String
    
        


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

    case msg of 
        Budget budget -> 
            let 
                currentBudget = (Maybe.withDefault 0 (String.toInt budget))
            in
            ({ model | budget = currentBudget 
                , balance = (currentBudget - model.expense)
            }
            ,Cmd.none
            )
        
        UpdateExpenseItem name -> 
            let 
                currentexpenseItem = model.tempExpenseItem
                updatedExpenseItem = {currentexpenseItem | expenseItem = name}
            in
            ({ model | tempExpenseItem = updatedExpenseItem } ,Cmd.none)
            

        UpdateExpenseAmount amount -> 
            let 
                currentexpenseItem = model.tempExpenseItem
                currentBudget = (Maybe.withDefault 0 (String.toInt amount))
                updatedExpenseItem = {currentexpenseItem | expenseAmount = currentBudget}
            in
            ({ model | tempExpenseItem = updatedExpenseItem } ,Cmd.none
            )

       
        Add   -> 
            (model, httppostCommand model.tempExpenseItem) 
            -- let
            --     updatedExpense = model.expense + model.tempExpenseItem.expenseAmount
            --     currentIndex = model.index + 1
            --     -- updatedExpenseItems =  model.expenseItems ++ [ExpenseItem model.tempExpenseItem.index model.tempExpenseItem.expenseItem model.tempExpenseItem.expenseAmount ]
            --     -- updatedtempExpenseItem= ExpenseItem currentIndex "" 0 
            -- in
            -- ({  model
            --     | index= model.index + 1
            --     ,expenseItems = updatedExpenseItems
            --     , expense =  updatedExpense
            --     , balance = (model.budget - updatedExpense) 
            --     ,tempExpenseItem = updatedtempExpenseItem
            -- } ,Cmd.none
            -- )
        
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
                        ({model 
                        |  expenseItems = updatedExpenseItems1 
                        , expense = z
                        , balance = model.budget - z} ,Cmd.none
                        )
                    Nothing ->
                        ({ model 
                        |  expenseItems = updatedExpenseItems1 } ,Cmd.none
                        )

        Edit index ->
            let
                updatedExpenseItems2 = List.filter (\expenseItem-> expenseItem.index == index) model.expenseItems
                listHead = List.take index model.expenseItems
                listTail = List.drop (index + 1) model.expenseItems
            in
                case (List.head updatedExpenseItems2) of
                    Just x ->
                        let
                            expenseItem = ExpenseItem x.index x.expenseItem x.expenseAmount 
                            updatedExpenseItems = listHead ++ [expenseItem] ++ listTail
                        in
                        ({model 
                        |  expenseItems = updatedExpenseItems
                        , expense = model.expense
                        , balance = model.balance} ,Cmd.none
                        )


                    Nothing ->
                        (model ,Cmd.none
                         )

        CancelEdit index ->
            let
                updatedExpenseItems2 = List.filter (\expenseItem-> expenseItem.index == index) model.expenseItems
                listHead = List.take index model.expenseItems
                listTail = List.drop (index + 1) model.expenseItems
            in
                case (List.head updatedExpenseItems2) of
                    Just x ->
                        let
                            expenseItem = ExpenseItem x.index x.expenseItem x.expenseAmount 
                            updatedExpenseItems = listHead ++ [expenseItem] ++ listTail
                        in
                        ({model 
                        |  expenseItems = updatedExpenseItems
                        , expense = model.expense
                        , balance = model.balance} ,Cmd.none
                        )


                    Nothing ->
                        (model ,Cmd.none
                        )

        SendHttpRequest ->
            ( model, httpCommand )

        
        DataReceived (Ok expenseItems) ->
            let
                _ = Debug.log "A" expenseItems
            in
            ({model | expenseItems = expenseItems}
               , Cmd.none
            )
        
        DataReceived (Err httpError) ->
            let
                _ = Debug.log "A" httpError
            in
            (model
               , Cmd.none
            )    
            
        ExpenseCreated (Ok tempExpenseItem) -> 
            let
                _ = Debug.log "A" tempExpenseItem
                newCmd =  Task.succeed SendHttpRequest
                    |> Task.perform identity
            in
            (model, newCmd)

              
             

        ExpenseCreated (Err httpError) ->

            ( model 
            , Cmd.none
            )

        -- UpdateAmount index value ->
        --     let
        --         updatedExpenseItems3 = List.filter (\expenseItem-> expenseItem.index == index) model.expenseItems
        --         listHead = List.take index model.expenseItems
        --         listTail = List.drop (index + 1) model.expenseItems
        --         currentBudget = (Maybe.withDefault 0 (String.toInt value))
        --     in
        --         case (List.head updatedExpenseItems3) of
        --             Just x ->
        --                 let
        --                     expenseItem = ExpenseItem x.index x.expenseItem currentBudget x.editable
        --                     updatedExpenseItems = listHead++[expenseItem]++listTail
        --                 in
        --                 {model 
        --                 |  expenseItems = updatedExpenseItems
        --                 , expense = model.expense
        --                 , balance = model.balance }


        --             Nothing ->
        --                 model             

-- toTableRow : ExpenseItem -> Html Msg
-- toTableRow expenseItem =
--     if expenseItem.editable == True then
--         toTableRowEdit expenseItem
--     else 
--         toTableRowNonEdit expenseItem 
httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://localhost:4000/api/show_expenses"
        , expect = Http.expectJson DataReceived (Json.Decode.field "data" (Json.Decode.list expenseDecoder))
        }

expenseDecoder : Decoder ExpenseItem
expenseDecoder =
    map3 ExpenseItem
        (field "id" Json.Decode.int)
        (field "item" Json.Decode.string)
        (field "amount" Json.Decode.int)

httppostCommand: ExpenseItem -> Cmd Msg
httppostCommand expenseItem =
    Http.post
        { url = "http://localhost:4000/api/add_expense"
        , body = Http.jsonBody (newExpenseEncoder expenseItem)
        , expect = Http.expectJson ExpenseCreated expenseDecoder
        }

newExpenseEncoder: ExpenseItem -> Encode.Value
newExpenseEncoder expenseItem =
    Encode.object[("expense", Encode.object
    [ 
     ("item", Encode.string expenseItem.expenseItem)
    ,("amount", Encode.int expenseItem.expenseAmount)
    ])]


toTableRowEdit : ExpenseItem -> Html Msg
toTableRowEdit expenseItem =
    tr []
        [ td [] [ text (String.fromInt expenseItem.index)]
        , td [] [ text expenseItem.expenseItem ]
        , td [] [ text (String.fromInt expenseItem.expenseAmount) ]
        , td [] [
            input[type_ "text",  Html.Attributes.value ( String.fromInt expenseItem.expenseAmount) 
            -- , onInput (UpdateAmount expenseItem.index) 
            ] []
            ,button [onClick (CancelEdit expenseItem.index)] [text "Cancel"]
            , button [] [text "Submit"]
        ]
        ]   

toTableRowNonEdit : ExpenseItem -> Html Msg
toTableRowNonEdit expenseItem =
    tr []
        [ td [] [ text (String.fromInt expenseItem.index)]
        , td [] [ text expenseItem.expenseItem ]
        , td [] [ text (String.fromInt expenseItem.expenseAmount)]
        , td [] [
            button [onClick (Delete expenseItem.index)] [text "delete"]
            , button [onClick (Edit expenseItem.index)] [text "edit"]
        ]
        ]   
viewExpenseItem : ExpenseItem -> Html Msg
viewExpenseItem expenseItem  =
    tr []
        [ td []
            [ text (String.fromInt expenseItem.index) ]
        , td []
            [ text expenseItem.expenseItem ]
        , td []
            [ text  (String.fromInt expenseItem.expenseAmount) ]
        ]

--VIEW

view : Model -> Html Msg
view model =
    div []
    [
       h1 [] [text "TRACK YOUR BUDGET"]
        , div[class "form1"] [
           h2 [][text "Please Enter Your Budget"] 
           , input [Html.Attributes.value (String.fromInt model.budget), onInput Budget] []
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
                , input [Html.Attributes.value model.tempExpenseItem.expenseItem , onInput UpdateExpenseItem] []  
                , h2 [] [text "Please Enter Your Expense Amount"]
                , input [Html.Attributes.value (String.fromInt model.tempExpenseItem.expenseAmount ), onInput UpdateExpenseAmount] []
                , button [ onClick Add][text "Add"]

            ]
        ,div []
            [ button [ onClick SendHttpRequest ]
                [ text "Get data from server" ]
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
                    -- ++ List.map toTableRow model.expenseItems  
                    ++ List.map viewExpenseItem model.expenseItems
                )
        ]
    ]