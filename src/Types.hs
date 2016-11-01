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

data Label = Label String deriving (Eq)

instance Show Label where show (Label s) = s

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 |
           R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19 | R20 |
           R21 | R22 | R23 | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31
         deriving (Show, Eq, Enum)

data RegPool = RegPool { rpoolInUse :: [Reg]
                       , rpoolFree :: [Reg]
                       } deriving (Show, Eq)
                  
data Constant = IntConstant CInteger deriving (Eq)
instance Show Constant where
  show (IntConstant n) = show n

data Mneumonic = ADDC
               | BF
               deriving (Show, Eq)
                               
data BetaAsm = Inst3 Mneumonic Reg Constant Reg               
             | Inst2 Mneumonic Reg Label
             | Lbl Label
             deriving (Eq)

addc = Inst3 ADDC
bf = Inst2 BF
cmove = addc R31

instance (Show BetaAsm) where
  show (Inst3 m r1 c r2) = concat [ show m
                                  , "("
                                  , show r1, ", "
                                  , show c, ", "
                                  , show r2, ")"
                                  ]
  show (Inst2 m x y) = show m ++ "(" ++ show x ++ ", " ++ show y ++ ")"
  show (Lbl l) = show l ++ ":"


               
class Compile a where
  compile :: (Show a, Monad m) => RegPool -> a -> m [BetaAsm]
