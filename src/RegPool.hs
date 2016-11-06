module RegPool where

import Control.Monad.Writer
import Types
import BetaCpu.Types
import BetaCpu.Instructions
import qualified BetaCpu.ToBeta as TB 
import qualified BetaCpu.Util as BCU
import qualified Data.List as DL
import qualified Data.Map as DM

new = RegPool [] [R1 .. R26]



--data RegAlloc = RegAlloc { raRegMap :: DM.Map Reg 

-- these should be encapsulated
spill = undefined
fill = undefined

request = undefined
release = undefined

-- isolate blocks of instructions available regs are from R1 to R20.
-- the first go at this, allocate all virtual registers without any
-- concern of running out.

-- partition the instruction stream into [Instructions]
isStartBlock (AsmBlockStart _) = True
isStartBlock _ = False
isStopBlock (AsmBlockStop _) = True
isStopBlock _ = False

partitionStream [] _ = []
partitionStream prog inBlock =
  if not inBlock
  then let code = DL.takeWhile (not . isStartBlock) prog
           rest = drop (length code) prog
       in (False, code) : partitionStream rest True
  else let code = DL.takeWhile (not . isStopBlock) prog
           rest = drop (length code) prog
       in (True, code) : partitionStream rest False

resolveReg regMap r@(VirtReg _) =
  case DM.lookup r regMap of
    Just r' -> r'
    Nothing -> error $ "resolveReg can't resolve register in: " ++ (show (regMap, r))
resolveReg _ r = r

replaceRegs regMap asmInst = 
  let f = resolveReg regMap
  in case asmInst of
    (AsmInst (Inst m r1 r2 r3 s)) ->
      Just $ AsmInst $ Inst m (f r1) (f r2) (f r3) s
      
    (AsmInst (LitInst m r1 lit r2 s)) ->
      Just $ AsmInst $ LitInst m (f r1) lit (f r2) s
      
    (AsmInst (LabelInst m r1 label r2 s)) ->
      Just $ AsmInst $ LabelInst m (f r1) label (f r2) s
      
    (AsmInst (JmpInst m r1 r2)) ->
      Just $ AsmInst $ JmpInst m (f r1) (f r2)

    (AsmPush r1 s) ->
      Just $ AsmPush (f r1) s

    (AsmPop r1) ->
      Just $ AsmPop (f r1) 

allocateBlock :: (Bool, [Asm]) -> [Asm]
allocateBlock (False, prog) = prog
allocateBlock (True, prog) =
  let allRegs = DL.nub $ filter isVirtReg $ concat $ map regsFromAsm prog
      regMap = DM.fromList $ zip allRegs (map (NR "") [R1 .. R26])
      boiler inst = case replaceRegs regMap inst of
        Just x -> x
        Nothing -> error $ "Allocate block fails to find a virtual register in: " ++ show (allRegs, regMap, inst)
        
      f inst@(AsmInst _) = boiler inst        
      f inst@(AsmPush _ _) = boiler inst
      f inst@(AsmPop _) = boiler inst
      f x = x
  in map f prog


allocateRegs :: AsmEdit -> AsmEdit
allocateRegs asm =
  let (_, (AsmProg program)) = runWriter asm
      parts = partitionStream program False
      allo'd = concat $ map allocateBlock parts
  in sequence_ $ map tell allo'd

