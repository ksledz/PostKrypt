module Main where

import           Data.String (String)
import           Lib

main :: IO ()
main = print "XD!"

-- cz B funkcja zmieniajaca Picture w stringi an wyjście

prologue = "300 400 translate\n"

epilogue = "stroke showpage"

printPicture :: Picture -> String
printPicture p = prologue ++ printPictureAux p ++ epilogue

printPictureAux :: Picture -> String
printPictureAux (Picture[]) = ""
printPictureAux (Picture(((x, y), (z, t)):tail)) =
  show x ++ " " ++ show y ++ " moveto " ++ show z ++ " " ++ show t ++ " lineto\n" ++
  (printPictureAux $Picture tail)
