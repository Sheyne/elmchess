module Main exposing (main)

import Browser
import Html exposing (Html, table, tr, td, text, div, h3)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

type alias Model =
    { fromLoc : Maybe Point
    , grid : Board
    , turn : Color
    , castlingAvailable : (Bool, Bool)
    , enPassant : (Maybe Board)
    , moves : (List (Point, Point, Square)) }
    
initialModel : Model
initialModel =
    { fromLoc = Nothing
    , grid = initialBoard
    , turn = White
    , castlingAvailable = (True, True)
    , enPassant = Nothing
    , moves = [] }


type Piece = King | Queen | Bishop | Knight | Rook | Pawn
type Color = White | Black
type Square = Filled Color Piece | Unfilled

show : Square -> String
show s = case s of
    Unfilled              -> ""
    (Filled White King)   -> "♔"
    (Filled White Queen)  -> "♕"
    (Filled White Rook)   -> "♖"
    (Filled White Bishop) -> "♗"
    (Filled White Knight) -> "♘"
    (Filled White Pawn)   -> "♙"
    (Filled Black King)   -> "♚"
    (Filled Black Queen)  -> "♛"
    (Filled Black Rook)   -> "♜"
    (Filled Black Bishop) -> "♝"
    (Filled Black Knight) -> "♞"
    (Filled Black Pawn)   -> "♟"

type alias Board = List (List Square)

whiteBackRow : List Square
whiteBackRow = [ Filled White Rook
               , Filled White Knight
               , Filled White Bishop
               , Filled White King
               , Filled White Queen
               , Filled White Bishop
               , Filled White Knight
               , Filled White Rook]

blackBackRow : List Square
blackBackRow = List.map (\x -> case x of 
                            Unfilled -> Unfilled
                            Filled _ p -> Filled Black p) whiteBackRow

initialBoard : Board
{----}
initialBoard = [ blackBackRow 
               , List.repeat 8 (Filled Black Pawn)] 
               ++ List.repeat 4 (List.repeat 8 Unfilled) ++ 
               [ List.repeat 8 (Filled White Pawn)
               , whiteBackRow]
--}
{--   
initialBoard = emptyBoard
               |> putAt {x=4, y=4} (Filled White Rook)
--}

           
emptyBoard : Board
emptyBoard = List.repeat 8 (List.repeat 8 Unfilled)

putAt : Point -> Square -> Board -> Board
putAt loc piece board = List.indexedMap (\y r -> 
                                    List.indexedMap (\x v -> 
                                    if loc == {x=x, y=y} then 
                                        piece
                                    else v) 
                                    r) board

isUnfilled : Square -> Bool
isUnfilled x = case x of
                Unfilled -> True
                Filled _ _ -> False

doesntCross : Point -> Point -> Point -> Board -> Bool
doesntCross start end direction board = let 
                                            next = { x = start.x + direction.x
                                                   , y = start.y + direction.y }
                                        in next == end
                                           || ((isUnfilled (pieceAt next board))
                                               && doesntCross next end direction board)
                  
              
testCrosses = let
                  board = emptyBoard
                          |> putAt {x=4, y=4} (Filled White Rook)
                          
              in [ (Test "Can't step over"
                          ((doesntCross {x=3, y=4} {x=5, y=4} {x=1, y=0} board)
                           |> not))
                 , (Test "Can't step over"
                          ((doesntCross {x=1, y=4} {x=5, y=4} {x=1, y=0} board)
                           |> not))
                 , (Test "Can start moving"
                          (doesntCross {x=4, y=4} {x=5, y=4} {x=1, y=0} board))
                 , (Test "Can capture"
                          (doesntCross {x=3, y=4} {x=4, y=4} {x=1, y=0} board))]

signum p = if p == 0 then 0 else if p > 0 then 1 else -1
deltasigns : Point -> Point -> ((Int, Int), (Int, Int))
deltasigns p1 p2 = ((abs (p1.x - p2.x), abs (p1.y - p2.y)),
                    (signum (p2.x - p1.x), signum (p2.y - p1.y)))


testDeltasigns = [ (TEqual "one unit"
                    (deltasigns {x=4, y=6} {x=5, y=5}) ((1, 1), (1, -1))) 
                 , (TEqual "large delta"
                    (deltasigns {x=5,y=7} {x=1,y=6}) ((4, 1), (-1, -1)))]

value : Piece -> Int
value p = case p of
     King -> 100
     Queen -> 8
     Bishop -> 3
     Knight -> 3
     Rook -> 4
     Pawn -> 1

score : Square -> Int
score s = case s of
            Unfilled -> 0
            Filled c p -> if c == White then 1 * (value p) else -1 * (value p)

evaluate : Board -> Int
evaluate board = List.foldr (\line rst -> rst + (
                                List.foldr (\c acc -> score c + acc) 0 line)) 0 board

isValidPieceMove : Point -> Point -> Color -> Piece -> Square -> Board -> Bool
isValidPieceMove src dst c p destpiece board = 
    let
        ((dx, dy), (sx, sy)) = deltasigns src dst
    in case p of
        King -> (dx <= 1 && dy <= 1)
        Queen -> (dx == dy || dx == 0 || dy == 0) && doesntCross src dst {x=sx,y=sy} board
        Bishop -> (dx == dy) && doesntCross src dst {x=sx,y=sy} board
        Knight -> (dx == 1 && dy == 2) || (dx == 2 && dy == 1)
        Rook -> (dx == 0 || dy == 0) && doesntCross src dst {x=sx,y=sy} board
        Pawn -> (dx == 0 && dy == 1
                    && (isUnfilled destpiece)
                    && if c == White then sy == -1 else sy == 1)
                 || (dx == 1 && dy == 1
                    && (not (isUnfilled destpiece))
                    && if c == White then sy == -1 else sy == 1)
                 || (dx == 0 && dy == 2 
                    && ((src.y == 1 && c == Black)
                        || (src.y == 6 && c == White))
                    && (isUnfilled destpiece)
                    && if c == White then sy == -1 else sy == 1)

isValidMoveModelFree : Point -> Point -> Color -> Piece -> Board -> Bool
isValidMoveModelFree source dest c p board = 
            let 
                dsquare = pieceAt dest board
            in  source /= dest
                && (case dsquare of 
                         Unfilled -> True
                         Filled cdest _ -> cdest /= c)
                && (isValidPieceMove source dest c p dsquare board)


isValidMove : Model -> Point -> Point -> (Maybe Board)
isValidMove model source dest = case pieceAt source model.grid of
                                    Filled c p -> if c == model.turn then
                                                        if isValidMoveModelFree source dest c p model.grid then
                                                            Just model.grid
                                                        else case model.enPassant of
                                                            Nothing -> Nothing
                                                            Just board -> if isValidMoveModelFree source dest c p board then
                                                                             model.enPassant
                                                                          else Nothing
                                                      else Nothing

                                                       
                                    Unfilled   -> Nothing 


type alias Point = 
    { x : Int
    , y : Int }

type Msg
    = MoveFrom Point
    | Move Point Point

index : Int -> (List a) -> (Maybe a)
index idx l = List.head (List.drop idx l)

pieceAt : Point -> Board -> Square
pieceAt {x, y} board = (index y board)
                       |> Maybe.andThen (index x)
                       |> Maybe.withDefault Unfilled

makeMove : Point -> Point -> Square -> Board -> Board
makeMove source dest movingPiece board = List.indexedMap (\y r -> 
                                             List.indexedMap (\x v -> 
                                             if dest == {x=x, y=y} then 
                                                 movingPiece
                                             else if source == {x=x, y=y} then Unfilled
                                             else v) 
                                             r) board

update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveFrom loc ->
            case (pieceAt loc model.grid) of
                Filled color _ -> if color == model.turn then
                    { model | fromLoc = Just loc } else model
                Unfilled -> model

        Move source dest ->
            case isValidMove model source dest of
                Just board ->
                    let 
                        movingPiece = pieceAt source board
                        epLoc = case movingPiece of
                                        Filled _ Pawn -> 
                                            if abs (source.y - dest.y) == 2 then
                                                Just {x=source.x, y=(source.y + dest.y) // 2}
                                            else
                                                Nothing
                                        _ -> Nothing
                    in
                      { model | grid = makeMove source dest movingPiece board
                              , fromLoc = Nothing
                              , turn = if model.turn == White then Black else White
                              , moves = (source, dest, movingPiece) :: model.moves
                              , enPassant = Maybe.andThen (\x -> Just (makeMove source x movingPiece board)) epLoc }
                Nothing -> { model | fromLoc = Nothing }
             


view : Model -> Html Msg
view model =
    div [] [ text ("It is " ++ (if model.turn == White then "white" else "black") ++ "'s turn")
           , table [style "border-spacing" "0px"] (List.indexedMap (row model) model.grid)
           , div [] [ text "Point differential: "
                    , text (String.fromInt (evaluate model.grid))]
           , showMoves model.moves
           , case model.enPassant of
               Nothing -> div [] []
               Just a -> table [style "border-spacing" "0px"] (List.indexedMap (row model) a)
           , showAllTests ]

row : Model -> Int -> (List Square) -> Html Msg
row model y r = tr [] (List.indexedMap (\x c -> cell model {x=x, y=y} c) r)

type DispSquare = Valid | Selected | FreeSquare

isNotNothing : Maybe a -> Bool
isNotNothing m = case m of
                    Nothing -> False
                    Just x -> True

cell : Model -> Point -> Square -> Html Msg
cell model loc label = td [] [
               div (( case model.fromLoc of
                           Just fl -> onClick (Move fl loc)
                           Nothing -> onClick (MoveFrom loc)) 
                      :: style "width"  "1.5em"
                      :: style "height" "1.5em"
                      :: style "font-size" "20px"
                      :: style "text-align" "center"
                      :: style "font-family" "\"arial unicode ms\",\"Lucida Sans Unicode\",sans-serif"  
                      :: (let
                              c = if (modBy 2 (loc.x + loc.y)) == 0 then White else Black
                          in
                              model.fromLoc
                              
                              |> Maybe.andThen (\fl -> if loc == fl then
                                                          (Just Selected)
                                                       else if isNotNothing (isValidMove model fl loc) then
                                                          (Just Valid)
                                                       else
                                                          (Just FreeSquare))
                              |> Maybe.withDefault FreeSquare
                              |> \i -> case i of
                                           FreeSquare -> if c == White then 
                                                             [ style "background" "#eee"
                                                             , style "border" ".25em solid #eee"]
                                                         else
                                                             [ style "background" "#aaa"
                                                             , style "border" ".25em solid #aaa"]
                                           Valid -> if c == White then 
                                                        [ style "background" "#99b"
                                                        , style "border" ".25em solid #99b "]
                                                    else
                                                        [ style "background" "#99b"
                                                        , style "border" ".25em solid #99b"]
                                           Selected -> if c == White then 
                                                           [ style "background" "#eee"
                                                           , style "border" ".25em solid #888"]
                                                       else
                                                           [ style "background" "#aaa"
                                                           , style "border" ".25em solid #888"]))
                   [text (show label)]]

showMoves : (List (Point, Point, Square)) -> Html Msg
showMoves moves = table [] (List.foldl (\x rst -> (showMove x) :: rst) [] moves)

showMove : (Point, Point, Square) -> Html Msg
showMove (s, d, p) = tr [] [td [] [ text (show p)
                                  , text " "
                                  , text (showPoint s)
                                  , text " → "
                                  , text (showPoint d)]]

showPoint : Point -> String
showPoint {x, y} = (String.fromChar (Char.fromCode (x + (Char.toCode 'a')))) ++ String.fromInt (8 - y)

type Test a = Test String Bool | TEqual String a a

allTests = [ ("Test crosses", testCrosses)
           , ("Test deltasigns", testDeltasigns) ]

showAllTests : Html Msg
showAllTests = div [] (List.map showTestList allTests)

showTestList : (String, (List (Test a))) -> Html Msg
showTestList (name, tests) = div [] [ h3 [] [text name]
                                    , table [] (List.map showTest tests)]

showTest : (Test a) -> Html Msg
showTest t = let
                (name, success, msg) = case t of
                                        (Test n v) -> 
                                            (n, v, if v then "succeeded" else "failed")
                                        (TEqual n x y) -> (n, x == y, Debug.toString(x) ++ " == " ++ Debug.toString(y))
             in tr [style "color" (if success then "green" else "red")]
                                    [ td [] [ text name ]
                                    , td [] [ text ":" ]
                                    , td [] [ text (msg)]]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

