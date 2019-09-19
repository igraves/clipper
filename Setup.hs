import Distribution.Simple
import System.Environment

main = do
         args <- getArgs
         case last args of 
               "copy" -> defaultMain
               "register" -> defaultMain
               _      -> defaultMainArgs $ ["--with-gcc=/usr/bin/g++"] ++ args


