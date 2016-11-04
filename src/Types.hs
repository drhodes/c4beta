{-# LANGUAGE ConstrainedClassMethods #-}
module Types where

import Language.C.Analysis.TravMonad
import qualified BetaCpu.Types as BT
import Control.Monad

data RegPool = RegPool { poolInUse :: [BT.Reg]
                       , poolFree :: [BT.Reg]
                       } deriving (Show, Eq)
                  
class Compile a where
  compile :: (Show a, MonadCError m, Monad m)
             => RegPool        -- register pool
             -> a              -- ast node
             -> m BT.AsmEdit   -- writer monad for accumulating assembly.

compileSeq rp x = liftM sequence_ (mapM (compile rp) x)
