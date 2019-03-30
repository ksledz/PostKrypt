-- by Kamila Śledź, ks386105
module Main where

import Control.Monad
import Data.Ratio ((%))
import Data.String (String)
import Lib
import Mon
import System.Environment (getArgs)
import System.IO
import Text.Read (readEither)

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
  | ErrorToken

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
    s ->
      let res = readEither s :: Either String Int
       in case res of
            Right r -> NumberToken r
            Left r -> ErrorToken

data Environment = Environment
  { stack :: [R]
  , picture :: Picture
  , currentPoint :: R2
  , currentTransform :: Transform
  , currentPath :: [R2]
  }

data ExecError =
  ExecError

execSingle :: Environment -> Token -> Either ExecError Environment
execSingle env@(Environment st pi poi tra pat) t =
  case t of
    ErrorToken -> Left ExecError
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
        0:_:t -> Left ExecError
        h1:h2:t -> Right $ Environment ((h2 / h1) : t) pi poi tra pat
        _ -> Left ExecError
    MovetoToken ->
      case st of
        y:x:t ->
          let transformed = trR2 tra (x, y)
           in Right env {currentPoint = transformed, currentPath = [transformed]}
        _ -> Left ExecError
    LinetoToken ->
      case st of
        y:x:t ->
          let transformed = trR2 tra (x, y)
           in Right
                env
                  {picture = pi & line poi transformed, currentPoint = transformed, currentPath = pat ++ [transformed]}
        _ -> Left ExecError
    ClosepathToken ->
      case pat of
        h:_:t -> Right env {picture = pi & line poi h, currentPoint = h, currentPath = pat ++ [h]}
        _ -> Right env
    TranslateToken ->
      case st of
        y:x:t -> Right env {currentTransform = translate (Vec (x, y)) >< tra}
        _ -> Left ExecError
    RotateToken ->
      case st of
        h:t -> Right env {currentTransform = rotate h >< tra}
        _ -> Left ExecError

execMany :: Environment -> [Token] -> Either ExecError Environment
execMany = foldM execSingle

main :: IO ()
main = do
  args <- getArgs
  let n =
        case args of
          h:t -> readEither h :: Either String Int
          _ -> Right 1
  case n of
    Left sth -> putStr "wrong argument, please enter a numeric value :("
    Right sth -> do
      postKryptProgram <- getContents
      let postKryptWords = words postKryptProgram
      let tokens = map parse postKryptWords
      let env = Environment [] (Picture []) (0, 0) (Transform []) []
      let result = execMany env tokens
      case result of
        Left ExecError -> putStr (prologue ++ errorMessage ++ epilogue)
        Right (Environment st pi poi tra pat) ->
          let xd = printPicture pi sth
           in putStr xd

prologue = "300 400 translate\n"

epilogue = "stroke showpage"

errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show \n"

printPicture :: Picture -> Int -> String
printPicture p i =
  let inted = renderScaled i p
   in prologue ++ printPictureAux inted ++ epilogue

printPictureAux :: IntRendering -> String
printPictureAux [] = ""
printPictureAux (((x, y), (z, t)):tail) =
  show x ++ " " ++ show y ++ " moveto " ++ show z ++ " " ++ show t ++ " lineto\n" ++ printPictureAux tail