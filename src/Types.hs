{-# LANGUAGE ConstrainedClassMethods #-}
module Types where

import Language.C.Analysis.TravMonad
import qualified BetaCpu.Types as BT
import Control.Monad
import qualified Data.List as DL
import Language.C.Data.Error

data RegPool = RegPool { poolInUse :: [BT.Reg]
                       , poolFree :: [BT.Reg]
                       } deriving (Show, Eq)
                  
class Compile a where
  compile :: (Show a, MonadCError m, Monad m)
             => RegPool        -- register pool
             -> a              -- ast node
             -> m (BT.AsmEdit, Maybe BT.NReg)   -- writer monad for accumulating assembly.

compileSeq :: (Compile a, MonadCError m, Show a)
           => RegPool
           -> [a]
           -> m BT.AsmEdit
compileSeq rp xs = do (code, _) <- liftM DL.unzip $ mapM (compile rp) xs
                      return $ sequence_ code


compileOne rp x = liftM fst $ compile rp x


compileExpr rp x = do
  (c, r) <- compile rp x
  case r of
    Just reg -> return (c, reg)
    Nothing -> internalErr "Expression doesn't return a virtual register"
 
retReg = (BT.NR "ret" BT.R0) 
