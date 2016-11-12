{-# LANGUAGE ConstrainedClassMethods #-}
module Types where

import Language.C.Analysis.TravMonad
import qualified BetaCpu.Types as BT
import Control.Monad
import qualified Data.List as DL
import qualified Data.Map as DM
import Language.C.Data.Error
import Data.Functor.Identity
import Control.Monad.Trans.Writer.Lazy
import Language.C.Analysis.SemRep
import qualified Language.C.Data.Ident as Ident
import qualified Language.C.Data.Position as Position
import qualified Language.C.Data.Name as Name
import qualified Language.C.Analysis.NameSpaceMap as NSM

data RegPool = RegPool { poolInUse :: [BT.Reg]
                       , poolFree :: [BT.Reg]
                       } deriving (Show, Eq)


data Pile = Pile { pileGlobalDecls :: GlobalDecls
                 --, pileLocals :: NSM.NameSpaceMap Ident.Ident BT.NReg
                 , pileTestBool :: Bool
                 }


class Compile a where
  compile :: a -> Trav Pile (BT.AsmEdit, Maybe BT.NReg)


sizeOfGlobalDecls (GlobalDecls objs tags tdefs) =  ( (DM.size objs,  "objects")
                                                   , (DM.size tags, "tags")
                                                   , (DM.size tdefs, "typedefs"))


objKeysOfGlobalDecls (GlobalDecls objs _ _) = DM.keys objs

testingIdent s = Ident.mkIdent Position.nopos s (Name.Name 0)


identInGlobals :: String -> Trav Pile Bool
identInGlobals s = do
  (GlobalDecls objs _ _) <- liftM pileGlobalDecls getUserState
  let ident = testingIdent s
  return $ DM.member ident objs



compileSeq :: (Compile a) => [a] -> Trav Pile BT.AsmEdit
compileSeq xs = do
  (code, _) <- liftM DL.unzip $ mapM (compile) xs
  return $ sequence_ code

compileOne x = liftM fst $ compile x


compileExpr x = do
  (code, register) <- compile x
  case register of
    Just reg -> return (code, reg)
    Nothing -> internalErr "Expression doesn't return a virtual register"
 
retReg = (BT.NR "ret" BT.R0) 

done edit = return (edit, Nothing)
