{-# LANGUAGE StandaloneDeriving #-}
module Main
       ( main
       ) where

import Prelude

import TW.Ast
import TW.Check
import qualified TW.CodeGen.Haskell as HS
import qualified TW.CodeGen.Flow as Flow
import TW.Loader
import TW.Parser
import TW.Types

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  T.putStrLn "Loading type modules"
  allModules <- loadModules ["typedefs"] [ModuleName ["Pos", "Wallet", "Web", "ClientTypes"], ModuleName ["Pos", "Wallet", "Web", "Error"]]
  T.putStrLn "All modules loaded"
  case allModules of
    Left err -> fail err
    Right ok ->
      case checkModules ok of
        Left err -> fail err
        Right readyModules -> do
          mapM_ (T.putStrLn . Flow.makeModule) readyModules
          mapM_ (T.putStrLn . HS.makeModule) readyModules
