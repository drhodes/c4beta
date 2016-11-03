{-# LANGUAGE ConstrainedClassMethods #-}
module Types where

import Data.List
import Language.C
import Language.C.Analysis.AstAnalysis
import Language.C.Syntax.AST
import Language.C.Analysis.TravMonad
import Language.C.System.GCC
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.List as DL
import Data.Monoid
import qualified BetaCpu.Types as BT
  
data RegPool = RegPool { rpoolInUse :: [BT.Reg]
                       , rpoolFree :: [BT.Reg]
                       } deriving (Show, Eq)
                  
class Compile a where
  compile :: (Show a, MonadCError m, Monad m) => RegPool -> a -> m BT.AsmEdit
