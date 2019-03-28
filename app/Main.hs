module Main where

import Control.Monad
import Data.Ratio ((%))
import Data.String (String)
import Lib
import Mon
import System.Environment (getArgs)
import System.IO

data Token
  = NumberToken Int
  | AddToken
  | SubToken
  | DivToken
  | MulToken
  | MovetoToken
  | LinetoToken
  | ClosepathToken
  | TranslateToken
  | RotateToken

parse :: String -> Token
parse s =
  case s of
    "add" -> AddToken
    "sub" -> SubToken
    "div" -> DivToken
    "mul" -> MulToken
    "moveto" -> MovetoToken
    "lineto" -> LinetoToken
    "closepath" -> ClosepathToken
    "translate" -> TranslateToken
    "rotate" -> RotateToken
    s -> NumberToken (read s :: Int)

data Environment = Environment
  { stack :: [R]
  , picture :: Picture
  , currentPoint :: R2
  , currentTransform :: Transform
  , currentPath :: [R2] -- początek ścieżki
  }

data ExecError =
  ExecError

execSingle :: Environment -> Token -> Either ExecError Environment
execSingle env@(Environment st pi poi tra pat) t =
  case t of
    NumberToken n -> Right env {stack = (toInteger n % 1) : st}
    AddToken ->
      case st of
        h1:h2:t -> Right $ Environment ((h2 + h1) : t) pi poi tra pat
        _ -> Left ExecError
    SubToken ->
      case st of
        h1:h2:t -> Right $ Environment ((h2 - h1) : t) pi poi tra pat
        _ -> Left ExecError
    MulToken ->
      case st of
        h1:h2:t -> Right $ Environment ((h1 * h2) : t) pi poi tra pat
        _ -> Left ExecError
    DivToken ->
      case st of
        0:h2:t -> Left ExecError
        h1:h2:t -> Right $ Environment ((h2 / h1) : t) pi poi tra pat
        _ -> Left ExecError
    -- zdejmuje dwie współrzędne ze stosu i ustawia bieżący punkt na te współrzędne;
    -- rozpoczyna nową bieżącą ścieżkę składającą się z jednego punktu;
    MovetoToken ->
      case st of
        h1:h2:t -> Right env {currentPoint = (h1, h2), currentPath = [(h1, h2)]}
        _ -> Left ExecError
    -- zdejmuje dwie współrzędne ze stosu i dodaje do bieżącej ścieżki (i do rysunku) odcinek od bieżącego punktu;
    -- do punktu o tych współrzędnych, odpowiednio przesuwając bieżący punkt.
    LinetoToken ->
      case st of
        h1:h2:t ->
          Right env {picture = pi & line (h1, h2) poi, currentPoint = (h1, h2), currentPath = pat ++ [(h1, h2)]}
        _ -> Left ExecError
    -- dodaje do bieżącej ścieżki (i do ryzunku) odcinek od bieżącego punktu do pierwszego punktu ścieżki,
    -- odpowiednio przesuwając bieżący punkt. Jeżeli bieżąca ścieżka jest pusta lub złożona z jednego punktu,
    -- closepath nie ma żadnych efektów
    ClosepathToken ->
      case pat of
        h:_:t -> Right env {picture = pi & line poi h, currentPoint = h, currentPath = pat ++ [h]}
        _ -> Right env
    TranslateToken ->
      case st of
        h1:h2:t -> Right env {currentTransform = translate (Vec (h2, h1)) >< tra}
        _ -> Left ExecError
    RotateToken ->
      case st of
        h:t -> Right env {currentTransform = rotate h >< tra}
        _ -> Left ExecError

-- analogicznie do bieżącej ścieżki, istnieje pojęcie bieżącej transformacji (początkowo identyczność).
-- W efekcie operacji specyfikującej transformację,bieżąca transformacja staje się złożeniem transformacji
-- dotychczasowej i wyspecyfikowanej.W momencie wykonywania dowolnej operacji używającej współrzędnych,
-- podlegają one przekształceniu przez bieżącą transformację.
execMany :: Environment -> [Token] -> Either ExecError Environment
execMany = foldM execSingle

main :: IO ()
main = do
  args <- getArgs -- n
  postKryptProgram <- getContents
  let postKryptWords = words postKryptProgram
  print postKryptWords
  print args
  print "XD"
  print "XDDD"

-- cz B funkcja zmieniajaca Picture w stringi na wyjście
prologue = "300 400 translate\n"

epilogue = "stroke showpage"

printPicture :: Picture -> String
printPicture p = prologue ++ printPictureAux p ++ epilogue

printPictureAux :: Picture -> String
printPictureAux (Picture []) = ""
printPictureAux (Picture (((x, y), (z, t)):tail)) =
  show x ++ " " ++ show y ++ " moveto " ++ show z ++ " " ++ show t ++ " lineto\n" ++ printPictureAux (Picture tail)