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

data Label = Label String deriving (Eq)

instance Show Label where show (Label s) = s

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 |
           R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19 | R20 |
           R21 | R22 | R23 | R24 | R25 | R26 | BP | LP | SP | R30 | R31
         deriving (Show, Eq, Enum)

-- crtn = pop R1 <>
--        move BP SP <>
--        pop BP <>
--        pop LP <>
--        jmp LP 

jmp2 r1 r2 = [Inst2Regs JMP r1 r2]
jmp1 r = jmp2 r R31


pop rc = [ ld3 rc
         , subc SP 4 SP]

addc = Inst3 ADDC
bf = Inst2 BF
cmove = addc R31
beq ra label = Inst3Label BEQ ra label R31
-- add = Inst3
-- move ra rc = add ra R31 rc

br :: Label -> BetaAsm
br label = beq R31 label

subc ra lit rc = Inst3 SUBC ra (IntConstant $ cInteger lit) rc

ld3 :: Reg -> BetaAsm
ld3 r = Inst3 LD SP (IntConstant $ cInteger (-4)) r
  
data RegPool = RegPool { rpoolInUse :: [Reg]
                       , rpoolFree :: [Reg]
                       } deriving (Show, Eq)
                  
data Constant = IntConstant CInteger
              deriving (Eq)

                                              
instance Show Constant where
  show (IntConstant n) = show n

data Mneumonic = ADDC
               | BF
               | BEQ
               | SUBC
               | JMP
               | ADD
               | LD
               deriving (Show, Eq)
                               
data BetaAsm = Inst3 Mneumonic Reg Constant Reg
             | Inst3Label Mneumonic Reg Label Reg               
             | Inst2 Mneumonic Reg Label
             | Inst2Regs Mneumonic Reg Reg
             | Lbl Label
             deriving (Eq)

instance (Show BetaAsm) where
  show (Inst3 m r1 c r2) = concat [ show m
                                  , "("
                                  , show r1, ", "
                                  , show c, ", "
                                  , show r2, ")"
                                  ]
  show (Inst2 m x y) = show m ++ "(" ++ show x ++ ", " ++ show y ++ ")"
  show (Inst3Label m r1 lbl r2) = concat [ show m
                                         , "("
                                         , show r1, ", "
                                         , show lbl, ", "
                                         , show r2, ")"
                                         ]
  show (Inst2Regs m r1 r2) = concat [ show m
                                    , "("
                                    , show r1, ", "
                                    , show r2, ")"
                                    ]
  show (Lbl l) = show l ++ ":"


               
class Compile a where
  compile :: (Show a, MonadCError m, Monad m) => RegPool -> a -> m [BetaAsm]
