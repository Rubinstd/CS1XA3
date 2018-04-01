module ElmTron exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Keyboard as Key



type alias Position = (Float,Float)

type Direction = Up | Down | Left | Right

type alias Bike = { head : Position,
                     tail : List Position,
                     direction : Direction,
                     score : Int }

type Model = GameOff
                 | GamePause Bike Bike
                 | GameOn Bike Bike


type Msg = Tick Float
          | KeyMsg Key.KeyCode


init = (GameOff,Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case model of
          GameOff ->
            case msg of
                (KeyMsg 32) -> (GameOn (initBike1 0) (initBike2 0), Cmd.none)
                _           -> (model, Cmd.none)
          (GamePause bike1 bike2) ->
            case msg of
                (KeyMsg 32) -> (GameOn (initBike1 bike1.score) (initBike2 bike2.score), Cmd.none)
                (KeyMsg 82) -> (GameOff, Cmd.none)
                _           -> (model, Cmd.none)
          (GameOn bike1 bike2) -> case msg of
            (KeyMsg keycode) -> case keycode of
              82 -> (GameOff, Cmd.none)
              _ ->      let
                          newDirection1 = changeDirection1 keycode bike1.direction
                          newBike1 = {bike1 | direction = newDirection1}
                          newDirection2 = changeDirection2 keycode bike2.direction
                          newBike2 = {bike2 | direction = newDirection2}
                      in ((GameOn newBike1 newBike2), Cmd.none)
            Tick _ -> let
                  newHead1 = moveBike bike1.head bike1.direction
                  newTail1 = bike1.tail ++ [bike1.head]
                  newBike1 = {bike1 | head = newHead1, tail = newTail1}
                  newHead2 = moveBike bike2.head bike2.direction
                  newTail2 = bike2.tail ++ [bike2.head]
                  newBike2 = {bike2 | head = newHead2, tail = newTail2}
                  gameState1 = (gameOver newHead1 (newHead2::(newTail1 ++ newTail2)))
                  gameState2 = (gameOver newHead2 (newHead1::(newTail2 ++ newTail1)))
                in if gameState1 && gameState2 then (GamePause newBike1 newBike2, Cmd.none)
                else if gameState1 then
                    let
                      winBike = {newBike2 | score = newBike2.score + 1 }
                    in (GamePause newBike1 winBike, Cmd.none)
                else if gameState2 then
                  let
                    winBike = {newBike1 | score = newBike1.score + 1 }
                  in (GamePause winBike newBike2, Cmd.none)
                else
                  ((GameOn newBike1 newBike2), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    GameOff ->
      Key.downs KeyMsg
    GamePause _ _ ->
      Key.downs KeyMsg
    GameOn _ _ ->
      Sub.batch
        [ Key.downs KeyMsg
        , Anim.times Tick
        ]

view : Model -> Html.Html Msg
view model = let
      bg = rect [x "0",y "0", width (toString boardDimension), height (toString boardDimension), rx "0", ry "0", fill "black"][]
      sb = [rect [x "700",y "0", width "300", height "200", rx "0", ry "0", fill "darkslategrey"] [],
            rect [x "700",y "200", width "300", height "500", rx "0", ry "0", fill "dimgrey"] [],
            text_ [x "850", y "30", fill "darkturquoise", fontFamily "Impact",textAnchor "middle",fontSize "20"] [text "Scores:"],
            text_ [x "850", y "230", fill "darkturquoise", fontFamily "Impact",textAnchor "middle",fontSize "20"] [text "Rules:"],
            text_ [x "850", y "260", fill "white", fontFamily "Arial",textAnchor "middle"] [text "Drive your bike around!"],
            text_ [x "850", y "290", fill "white", fontFamily "Arial",textAnchor "middle"] [text "Careful! Don't collide with:"],
            text_ [x "850", y "320", fill "white", fontFamily "Arial",textAnchor "middle"] [text "Walls"],
            text_ [x "850", y "350", fill "white", fontFamily "Arial",textAnchor "middle"] [text "Your tail"],
            text_ [x "850", y "380", fill "white", fontFamily "Arial",textAnchor "middle"] [text "Or the enemies tail"],
            text_ [x "850", y "410", fill "darkturquoise", fontFamily "Impact",textAnchor "middle",fontSize "20"] [text "Controls:"],
            text_ [x "850", y "440", fill "white", fontFamily "Arial",textAnchor "middle"] [text "Player 1 (red) moves with W,A,S,D"],
            text_ [x "850", y "470", fill "white", fontFamily "Arial",textAnchor "middle"] [text "Player 2 (blue) moves with Arrow Keys"],
            text_ [x "850", y "500", fill "white", fontFamily "Arial",textAnchor "middle"] [text "Hit R to reset the Game"]]
      content = case model of
          GameOff -> [text_ [x "350", y "325", fill "lightgrey", fontFamily "Impact",textAnchor "middle",fontSize "60"] [text "Welcome to ElmTron"],
                      text_ [x "353", y "325", fill "darkturquoise", fontFamily "Impact",textAnchor "middle",fontSize "60"] [text "Welcome to ElmTron"],
                      text_ [x "350", y "375", fill "lightgrey", fontFamily "Impact",textAnchor "middle",fontSize "40"] [text "Press Space Bar To Start The Game!"],
                      text_ [x "353", y "375", fill "darkturquoise", fontFamily "Impact",textAnchor "middle",fontSize "40"] [text "Press Space Bar To Start The Game!"],
                      text_ [x "850", y "60", fill "white", fontFamily "Arial",textAnchor "middle"] [text ("Player 1 - 0")],
                      text_ [x "850", y "90", fill "white", fontFamily "Arial",textAnchor "middle"] [text ("Player 2 - 0")]]
          GamePause bike1 bike2 -> [text_ [x "850", y "60", fill "white", fontFamily "Arial",textAnchor "middle"] [text ("Player 1 - " ++ (toString bike1.score))],
                                    text_ [x "850", y "90", fill "white", fontFamily "Arial",textAnchor "middle"] [text ("Player 2 - " ++ (toString bike2.score))],
                                    text_ [x "350", y "350", fill "lightgrey", fontFamily "Impact", textAnchor "middle",fontSize "40"] [text "Press Space Bar to start the next round!"],
                                    text_ [x "353", y "350", fill "darkturquoise", fontFamily "Impact", textAnchor "middle",fontSize "40"] [text "Press Space Bar to start the next round!"]]
          GameOn bike1 bike2 ->  let
                sb = [text_ [x "850", y "60", fill "white", fontFamily "Arial",textAnchor "middle"] [text ("Player 1 - " ++ (toString bike1.score))],
                      text_ [x "850", y "90", fill "white", fontFamily "Arial",textAnchor "middle"] [text ("Player 2 - " ++ (toString bike2.score))]]
                head1 = rect [x (toString (Tuple.first bike1.head)),
                             y (toString (Tuple.second bike1.head)),
                             width (toString bikeDimension),
                             height (toString bikeDimension),
                             rx "0",
                             ry "0",
                             fill "red"] []
                tail1 = List.map (\ coord -> (rect [ x (toString (Tuple.first coord)),
                                                    y (toString (Tuple.second coord)),
                                                    width (toString bikeDimension),
                                                    height (toString bikeDimension),
                                                    fill "red" ] [])) bike1.tail
                head2 = rect [x (toString (Tuple.first bike2.head)),
                             y (toString (Tuple.second bike2.head)),
                             width (toString bikeDimension),
                             height (toString bikeDimension),
                             rx "0",
                             ry "0",
                             fill "blue"] []
                tail2 = List.map (\ coord -> (rect [ x (toString (Tuple.first coord)),
                                                    y (toString (Tuple.second coord)),
                                                    width (toString bikeDimension),
                                                    height (toString bikeDimension),
                                                    fill "blue" ] [])) bike2.tail
            in sb++(head1::tail1)++(head2::tail2)
     in Html.div [] [svg [width (toString (boardDimension+300)),height (toString boardDimension)] (bg::(sb++content))]

{- Main -}
main : Program Never Model Msg
main = Html.program
       {init = init,
        update = update,
        view   = view,
        subscriptions = subscriptions }

{- Functions for the game -}

initBike1 : Int -> Bike
initBike1 score =
  let head = (24, 24)
      tail = []
  in { head=head, tail=tail, direction=Right, score = score }

initBike2 : Int -> Bike
initBike2 score=
    let head = (boardDimension-24, boardDimension-24)
        tail = []
    in { head=head, tail=tail, direction=Left, score = score }

changeDirection1 keyCode dir = case dir of
                Left -> case keyCode of
                  83 -> Up
                  87 -> Down
                  _ -> dir
                Right -> case keyCode of
                  83 -> Up
                  87 -> Down
                  _ -> dir
                Up -> case keyCode of
                  65 -> Left
                  68 -> Right
                  _ -> dir
                Down -> case keyCode of
                  65 -> Left
                  68 -> Right
                  _ -> dir

changeDirection2 keyCode dir = case dir of
                Left -> case keyCode of
                  40 -> Up
                  38 -> Down
                  _ -> dir
                Right -> case keyCode of
                  40 -> Up
                  38 -> Down
                  _ -> dir
                Up -> case keyCode of
                  37 -> Left
                  39 -> Right
                  _ -> dir
                Down -> case keyCode of
                  37 -> Left
                  39 -> Right
                  _ -> dir


moveBike : (Float,Float) -> Direction -> Position
moveBike (x,y) d = case d of
              Up -> (x,y+bikeDimension)
              Down -> (x,y-bikeDimension)
              Left -> (x-bikeDimension,y)
              Right -> (x+bikeDimension,y)

bikeDimension = 4

boardDimension = 700

gameOver : Position -> List Position -> Bool
gameOver newHead newTail = List.any ((==) newHead) newTail
                        || (Tuple.first newHead <= (0 + bikeDimension))
                        || (Tuple.first newHead >= (boardDimension - bikeDimension))
                        || (Tuple.second newHead <= (0 + bikeDimension))
                        || (Tuple.second newHead >= (boardDimension - bikeDimension))


{-

overlapText = Svg.style [("color","white"),
                     ("position", "absolute"),
                     ("top","50%"),
                     ("left","50%")
                     ("transform","translate(-50%, -50%)")]



-}

{-
Resources:
https://stackoverflow.com/questions/39058189/how-to-iterate-over-a-list-in-an-elm-view -> Drawing the tail
https://github.com/theburningmonk/elm-snake -> Ideas for each bike logic (driving, tail drawing ish, collisions, etc.)

-}
