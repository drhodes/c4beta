{-# LANGUAGE ConstrainedClassMethods #-}
module Types where

import Language.C.Analysis.TravMonad
import qualified BetaCpu.Types as BT
import Control.Monad
import qualified Data.List as DL
import Language.C.Data.Error
import Data.Functor.Identity
import Control.Monad.Trans.Writer.Lazy
import Language.C.Analysis.SemRep

data RegPool = RegPool { poolInUse :: [BT.Reg]
                       , poolFree :: [BT.Reg]
                       } deriving (Show, Eq)


                                  
-- class Compile a where
--   compile :: (Show a, MonadTrav m, MonadCError m, Monad m, MonadSymtab m)
--              => RegPool        -- register pool
--              -> a              -- ast node
--              -> Trav (BT.AsmEdit, Maybe BT.NReg)   -- writer monad for accumulating assembly.

class Compile a where
  compile :: a -> Trav GlobalDecls (BT.AsmEdit, Maybe BT.NReg)


compileSeq :: (Compile a) => [a] -> Trav GlobalDecls BT.AsmEdit
compileSeq xs = do
  (code, _) <- liftM DL.unzip $ mapM (compile) xs
  return $ sequence_ code

compileOne x = liftM fst $ compile x


compileExpr x = do
  (c, r) <- compile x
  case r of
    Just reg -> return (c, reg)
    Nothing -> internalErr "Expression doesn't return a virtual register"
 
retReg = (BT.NR "ret" BT.R0) 

done edit = return (edit, Nothing)
