{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Scope where

import Data.List
import Language.C
import Language.C.Analysis.AstAnalysis
import Language.C.Syntax.AST
import Language.C.Analysis.TravMonad
import Language.C.System.GCC
import Language.C.Analysis.DefTable
import Language.C.Data.Ident
import Language.C.Analysis.SemRep
import qualified Language.C.Analysis.DeclAnalysis as DA
import qualified Language.C.Analysis.NameSpaceMap as NSM
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.List as DL
import Types
import qualified RegPool as RP
import Language.C.Data.Error
import qualified BetaCpu.Types as BT 
import BetaCpu.Instructions
import qualified BetaCpu.ToBeta as TB 
import qualified BetaCpu.Util as BCU
import qualified Data.Map as DM



--

withNewScope f = do
  preserveRegs
  f
  restoreRegs

