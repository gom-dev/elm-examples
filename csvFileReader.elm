import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)
import Task
import Random
import Debug
import FileReader exposing (..)
import Csv exposing (..)

import Json.Decode as Json exposing (Value, andThen)

main : Program Never
main =
  App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type alias Model =
  { selected : Files        -- csv files
  , message : String
  , emailList : List String -- email list parsed from model.fileContent
  , itemList : List Item      
  , itemCount : Int         -- used to create item id
  }

type alias Item =
  { name : String
  , count : Int
  , winners : List String
  , uid : Int
  }

type alias Files =
  List NativeFile

type Msg
  = FileSelect Files
  | FileDataSucceed String
  | FileDataFail FileReader.Error
  | AddEmptyItem
  | UpdateItemName Int String
  | UpdateItemCount Int String
  | RemoveItem Int
  | Pick String Int Int
  | Winner Int (Cmd Msg) Int

init : (Model, Cmd Msg)
init =
  (initModel, Cmd.none)

initModel : Model
initModel =
  Model
    [] -- csv files
    "CSV 파일을 선택해주세요!"
    []
    [] -- no item
    0

newItem : Int -> Item
newItem uid =
  Item "" 1 [] uid

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FileSelect files ->
      case files of
        [] ->
          initModel ! []
        _ ->
          { model | selected = files} ! List.map readTextFile files 
    FileDataSucceed str ->
      let
        parsedList = Csv.parse str
        emailList = List.filter (\email -> email /= "") (List.map extractEmail parsedList.records)
        newMessage = (Basics.toString (List.length emailList)) ++ "개의 이메일이 추가되었습니다 :)"
      in
        { model | emailList = emailList, message = newMessage } ! []
    FileDataFail err ->
      { model | message = FileReader.toString err } ! []
    AddEmptyItem ->
      let
        uid = model.itemCount + 1
      in
        { model 
          | itemCount = uid
          , itemList = (model.itemList ++ [newItem uid])
        }
          ! []
    UpdateItemName uid newName ->
      let
        updateItemName item =
          if item.uid == uid then
            { item | name = newName }
          else
            item
      in
        { model | itemList = List.map updateItemName model.itemList } ! []
    UpdateItemCount uid newCount ->
      let
        updateItemCount item =
          let
            newCountInt = toInt newCount
          in
            if item.uid == uid then
              { item 
              | count =
                case newCountInt of
                  Result.Ok n ->
                    n
                  Result.Err err -> -- ignore err msg; set to 0
                    0
              }
            else
              item
      in
        { model | itemList = List.map updateItemCount model.itemList } ! []
    RemoveItem uid->
      { model
        | itemList = List.filter (\item -> item.uid /= uid) model.itemList
      }  
        ! []
    Pick name uid count ->
      let
        maxIdx = (List.length model.emailList - 1)
      in
        if List.length model.selected == 0 then
          { model | message = "파일을 먼저 선택해주세요!" } ! []
        else if name == "" then
          { model | message = "추첨하려는 경품 이름을 입력해주세요!"} ! []
        else if List.length model.emailList < count then
          { model | message = "주어진 이메일 개수가 경품수보다 적습니다!" } ! []
        else
          model ! 
          [(Random.generate
            (Winner uid (winnerCmdList uid (maxIdx-1) (count-1)))
            (Random.int 0 maxIdx))
          ]
    Winner uid nextCmd winnerIdx ->
      let
        winner = takeEmailFromList model.emailList winnerIdx
        newEmailList = emailListWithout model.emailList winnerIdx
        addItemWinner item =
          if item.uid == uid then
            { item | winners = (winner :: item.winners) }
          else
            item
      in
        { model
          | emailList = newEmailList
          , itemList = List.map addItemWinner model.itemList
        }
          ! [nextCmd] -- Winner or Cmd.none

emailListWithout : List String -> Int -> List String
emailListWithout list idx =
  (List.take idx list) ++ (List.drop (idx+1) list)

takeEmailFromList : List String -> Int -> String
takeEmailFromList list idx =
  let
    restList = List.drop idx list
  in
    case List.head restList of
      Just winner ->
        winner
      Nothing ->
        "" -- never happen

winnerCmdList : Int -> Int -> Int -> (Cmd Msg)
winnerCmdList uid maxIdx count =
  if count == 0 then
    Cmd.none
  else
    (Random.generate (Winner uid (winnerCmdList uid (maxIdx-1) (count-1))) (Random.int 0 maxIdx))

view : Model -> Html Msg
view model =
  div [style [("padding", "30px")]]
    [ h1 [] [text "Random gift"]
    , input
      [ type' "file"
      , onchange FileSelect
      , disabled (List.length model.selected > 0)
      , multiple False
      , accept ".csv"
      ] []
    , div [style [("padding", "10px")]] [ text model.message ]
    , button [ onClick AddEmptyItem ] [ text "경품추가하기" ]
    , div [] ( List.map itemDiv model.itemList )
    ]

itemDiv : Item -> Html Msg
itemDiv item = 
  div [style [("padding", "10px 0")]]
    [ button
        [ style inputStyle
        , onClick (RemoveItem item.uid)
        , disabled (List.length item.winners /= 0)
        , title "추첨 후에는 삭제가 불가능합니다"
        ] [ text "삭제" ]
    , input
        [ style inputStyle
        , type' "text"
        , placeholder "경품이름"
        , value item.name
        , disabled (List.length item.winners /= 0)
        , onInput (UpdateItemName item.uid)
        ] []
    , input
        [ style (("width", "30px")::inputStyle)
        , type' "number"
        , placeholder "0"
        , disabled (List.length item.winners /= 0)
        , value (Basics.toString item.count)
        , onInput (UpdateItemCount item.uid)
        ] []
    , button
      [ onClick (Pick item.name item.uid item.count)
      , disabled (List.length item.winners /= 0)
      , title "경품 종류당 한번만 추첨할 수 있습니다"
      ] [text "추첨"]
    , winnerDiv item.winners
    ]

winnerDiv : List String -> Html Msg
winnerDiv winners =
  case winners of
    [] ->
      div [] []
    _ ->
      div [] (List.map (\winner -> (div [] [text winner])) winners)

inputStyle : List (String, String)
inputStyle = [("margin-right", "5px")]

onchange : (List NativeFile -> a) -> Attribute a
onchange action =
  on
    "change"
      (Json.object1 (\v -> action v) parseSelectedFiles)

readTextFile : NativeFile -> Cmd Msg
readTextFile fileValue =
    readAsTextFile fileValue.blob
        |> Task.perform FileDataFail FileDataSucceed

extractEmail : List String -> String
extractEmail line =
  case List.head line of
    Just email ->
      email
    Nothing ->
      ""

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none