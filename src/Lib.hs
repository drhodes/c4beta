{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib where

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

debugOn = True
debugEnter msg = if debugOn
                 then docs $ "dbg | " ++ msg
                 else docs "//"


------------------------------------------------------------------
--processFile :: CLanguage -> [String] -> FilePath -> IO ()
processFile lang cppOpts file =
  do hPutStr stderr $ file ++ ": "
     let outFile = file ++ ".uasm"
     result <- parseCFile (newGCC "gcc") Nothing cppOpts file
     
     case result of
       Left err -> hPutStrLn stderr ('\n' : show err)
       Right tu -> do
         let temp = runTrav (Pile emptyGlobalDecls True) (body tu lang) 
         case temp of
           Left err  -> do
             mapM_ print err
             --hPutStrLn stderr ('\n' : concatMap show errs)
           Right (asm, globalDecls) -> do
             --print $ show $ userState globalDecls
             TB.toBetaEdit (RP.allocateRegs asm) >>= BCU.dump
             BCU.run (RP.allocateRegs asm)
               
------------------------------------------------------------------
body :: CTranslUnit -> CLanguage -> Trav Pile BT.AsmEdit
body tu lang = do modifyOptions (\opts -> opts { language = lang })
                  gdecs <- analyseAST tu
                  modifyUserState (\_ -> Pile gdecs False)
                  cTranslUnit tu
------------------------------------------------------------------
setupStack ws@(BT.Words space) = do
  allocate ws "allocation for setup stack"
  br (BT.Label "__start") "branching to start code"
  
  let bytes = space * 4
  skip bytes
  
  lbl "__start"
  cmove 0x100 BT.sp "this is setting the stack pointer"
  br (BT.Label "__function_main") "start the show"

------------------------------------------------------------------
cTranslUnit :: CTranslationUnit NodeInfo -> Trav Pile BT.AsmEdit
cTranslUnit (CTranslUnit decls info) = do
  -- nts, RP.new here isn't going to cut it, the register pool logic has nonlocal span.
  --(xs, _) <- liftM DL.unzip $ mapM (compile RP.new) decls
  code <- compileSeq decls

  return $ do BCU.newline
              setupStack (BT.Words 1024)              
              docs "assembly generated by c4beta"
              code

regFromInfo (NodeInfo _ _ name) = BT.VirtReg $ show (nameId name)
  
------------------------------------------------------------------
instance Compile (CExternalDeclaration NodeInfo) where
  compile (CDeclExt decl) = compile decl
  compile decl@(CFDefExt funDef) = compile funDef
  compile (CAsmExt strLit info) = undefined

------------------------------------------------------------------
functionNameFromDeclr (CDeclr (Just ident) _ _ _ _) = identToString ident
functionNameFromDeclr _ = ""

------------------------------------------------------------------
instance Compile (CFunctionDef NodeInfo) where
  compile func@(CFunDef decSpecs declarator declarations stmt info) = do
    specs <- compileSeq decSpecs
    dector <- compileOne declarator
    -- decls <- compileSeq declarations  for old style C
    block <- compileOne stmt

    analyseFunDef func
    
    let funcName = functionNameFromDeclr declarator
    let numArgs = length declarations
    table <- liftM identDecls getDefTable
    let zxcv = NSM.localNames table
    
    us <- liftM (pileGlobalDecls) getUserState
    testBool <- liftM pileTestBool getUserState
    gdecs <- liftM pileGlobalDecls getUserState
    
    done $ do docs "-------------------------------------------------------"
              lbl $ "__function_" ++ funcName
              docs "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
              docs $ "SIZE OF GLOBALS: " ++ (show (sizeOfGlobalDecls gdecs))
              --docs $ "table namespace map: " ++ (
              -- docs $ (show $ length zxcv)
              blockStart funcName              
              push BT.lp "entry pushing linkage pointer"
              push BT.bp "entry pushing base pointer"
              move BT.sp BT.bp "set base pointer"
              allocate (BT.Words numArgs) "entry seq"
              --pushSome regs "preserve registers by pushing them on the stack"
              -- decls
              dector
              block 
              blockStop funcName
    
------------------------------------------------------------------
instance Compile (CDeclarationSpecifier NodeInfo) where
  compile (CStorageSpec spec) = error "undefined: cDecSpec (CStorageSpec spec)"
  compile (CTypeSpec typeSpec) = compile typeSpec 
  compile (CTypeQual typeQual) = error "undefined: cDecSpec (CTypeQual typeQual)"

------------------------------------------------------------------
instance Compile (CTypeSpecifier NodeInfo) where
  compile x = done $ debugEnter "Compile (CTypeSpecifier NodeInfo)"

  

------------------------------------------------------------------
instance Compile (CDeclarator NodeInfo) where
  compile (CDeclr ident derivedDeclarators strLits attrs info) = do
    ddectors <- compileSeq derivedDeclarators
    done $ do debugEnter "Compile (CDeclarator NodeInfo)"
              ddectors
                
  -- CDecl [CDeclarationSpecifier a] [(Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a))] a
  -- CDeclr (Maybe Ident) [CDerivedDeclarator a] (Maybe (CStringLiteral a)) [CAttribute a] a

------------------------------------------------------------------
instance Compile (CDerivedDeclarator NodeInfo) where
  compile (CPtrDeclr quals info) = error "undefined: CPtrDeclr"
  compile (CArrDeclr quals arraySize info) = error "undefined: CArrDeclr"
  
  -- * old style parameter lists have the form @Left (parameter-names)@
  compile (CFunDeclr (Left idents) attrs info) = do
    ats <- compileSeq attrs
    done $ do docs $ show idents
              docs $ show ats
    
  -- * New style parameter lists have the form @Right (declarations, isVariadic)@
  compile (CFunDeclr (Right (decls, isVariadic)) attrs info) = do
    -- ats <- compileSeq attrs
    mapM_ (compile) decls
    ds <- compileSeq decls
    done $ do docs ". CDerivedDeclarator.CFunDeclr"
              docs $ show decls
              ds
  
------------------------------------------------------------------
instance (Compile (CAttribute NodeInfo)) where
  compile (CAttr ident exprs info) = do
    xs <- compileSeq exprs    
    done $ xs --do docs $ ". Cattribute.Cattr"
              -- docs $ show ident
              -- docs $ show xs

------------------------------------------------------------------
instance Compile CDecl where
  compile decl@(CDecl declSpecs triples info) = do
    dspecs <- compileSeq declSpecs
    trips <- compileSeq triples
    done $ do docs ". CDecl.CDecl"
              dspecs
              trips
    
  --   --fail "cDecl (CDecl declSpecs triples info) = undefined"

------------------------------------------------------------------
instance (Compile ( Maybe (CDeclarator NodeInfo)
                  , Maybe (CInitializer NodeInfo)
                  , Maybe (CExpression NodeInfo))) where
  compile (mdecl, minit, mexpr) = do
    md <- case mdecl of Just mdecl' -> compileOne mdecl'
                        Nothing -> return $ docs "no mdecl"
    -- what is minit?
    mi <- case minit of Just minit' -> compileOne minit'
                        Nothing -> return $ docs "no minit"
                        
    (me, reg) <- case mexpr of Just mexpr' -> compile mexpr'
                               Nothing -> return (docs "no mexpr", Nothing)
    
    let code = do docs "compiling triple"
                  md
                  mi
                  me
                  
    return (code, reg)
  
------------------------------------------------------------------
instance (Compile (CInitializer NodeInfo)) where
  compile (CInitExpr expr info) = do
    (code, rx) <- compileExpr expr
    return (code, Just rx)
  
  compile (CInitList initList info) = error ("undefined  (CInitList initList info)")
  
------------------------------------------------------------------
instance Compile (CCompoundBlockItem NodeInfo) where
  compile (CBlockStmt stmt) = compile stmt
  compile (CBlockDecl decl) = compile decl
-- GNU C, not implemented yet.
  compile (CNestedFunDef funcDef) = error "undefined: blockStmt (CNestedFunDef funcDef)"


------------------------------------------------------------------
instance Compile (CStatement NodeInfo) where
  -- zxcv zxcv zxcv zxcv
  
  compile (CCompound [] blockItems info) = do
    xs <- compileSeq blockItems -- fail "undefined: cStmt (CCompound [] blockItems info)"
    done $ if DL.null xs
           then docs "empty statement block"
           else xs



  -- | compound statement @CCompound localLabels blockItems at@
  compile (CCompound localLabels blockItems info) = do
    xs <- compileSeq blockItems
    done $ if DL.null xs
           then docs "empty statement block"
           else do docs $ "localLabels: " ++ show localLabels
                   xs

  -- if statement
  compile (CIf expr stmt1 Nothing info) = do
    (cond, rx) <- compileExpr expr
    clause1 <- compileOne stmt1
    done $ do docs "if statment without else clause"
              cond
              bf rx (BT.Label "Lendif") "branch"
              clause1
              lbl "Lendif"

  -- if statement with else clause
  compile (CIf expr stmt1 (Just stmt2) info) = do

    (cond, rx) <- compileExpr expr

    clause1 <- compileOne stmt1
    clause2 <- compileOne stmt2

    let code = do cond
                  bf rx (BT.Label "Lelse") ""
                  clause1
                  br (BT.Label "Lendif") ""
                  lbl "Lelse"
                  clause2
                  lbl "Lendif"
    return (code, Nothing)

  -- returning, this means we're in the Callee.
  compile (CReturn Nothing info) = do
    done $ do docs "returning"
              docs "EXIT SEQUENCE"
              move BT.bp BT.sp "deallocate space for locals"
              pop BT.bp
              pop BT.lp
              jmp1 BT.lp

  compile (CReturn (Just expr) a) = do
    (code, rx) <- compileExpr expr
    done $ do docs "returning"
              -- docs $ show code
              move rx BT.retReg "convention moves the value into R0"
              docs "EXIT SEQUENCE"
              move BT.bp BT.sp "deallocate space for locals"
              pop BT.bp
              pop BT.lp
              jmp1 BT.lp


  compile x = fail $ show x


-- preserveAllRegisters = do
--   st R1


instance Compile (CExpression NodeInfo) where
  compile (CComma expr info) = fail "cExpr CComma expr info = "
  compile (CAssign binop e1 e2 info) = fail "cExpr CAssign binop e1 e2 info ="
  compile (CCond e1 Nothing e2 info) = fail "cExpr CCond e1 Nothing e2 info ="
  compile (CCond e1 (Just e2) e3 info) = fail "cExpr CCond e1 (Just e2) e3 info ="

  -- cExpr CBinary CBinaryOp (CExpression a) (CExpression a) a
  -- cExpr CCast (CDeclaration a) (CExpression a) a
  -- cExpr CUnary CUnaryOp (CExpression a) a
  -- cExpr CSizeofExpr (CExpression a) a
  -- cExpr CSizeofType (CDeclaration a) a
  -- cExpr CAlignofExpr (CExpression a) a
  -- cExpr CAlignofType (CDeclaration a) a
  -- cExpr CComplexReal (CExpression a) a
  -- cExpr CComplexImag (CExpression a) a
  -- cExpr CIndex (CExpression a) (CExpression a) a
  compile (CCall expr exprs info) = do
    (code, rx) <- compileExpr expr
    let funcName = "__function_" ++ (TB.toBetaString code)

    (codes, rxs) <- liftM DL.unzip $ mapM (compileExpr) exprs
    return ( do docs $ "ENTRY SEQ FOR: " ++ (show code)
                docs $ "pushing registers: " ++ (show rxs)
                sequence_ codes
                mapM (flip push "pushing args") (reverse rxs)
                br2 (BT.Label funcName) BT.lp ("branching to " ++ funcName)
                deallocate (BT.Words $ length codes) ("deallocating " ++ funcName)
           , Just rx)

  -- cExpr CMember (CExpression a) Ident Bool a

  compile c@(CVar ident info) = do
    -- there's a lot more going on here
    -- need to find the memory this ident is referring to
    -- could be local or global.
    let code = do asmIdent $ identToString ident
        rx = regFromInfo info

    return (code, Just rx)

  compile c@(CConst const) = compile const
  -- --integer, character, floating point and string constants
  -- cExpr CCompoundLit (CDeclaration a) (CInitializerList a) a
  -- cExpr C99 compound literal
  -- cExpr CStatExpr (CStatement a) a
  -- cExpr GNU C compound statement as expr
  -- cExpr CLabAddrExpr Ident a
  -- cExpr GNU C address of label
  -- cExpr CBuiltinExpr (CBuiltinThing a)
  --builtin expressions, see CBuiltin
  compile x = fail $ "Haven't implemented an expression rule: " ++ show x

instance Compile (CConstant NodeInfo) where
  compile (CIntConst n info) = do
    let c = fromIntegral $ getCInteger n :: Int
        rx = regFromInfo info
        code = cmove c rx "move constant to register"

    return (code, Just rx)

  compile (CCharConst char _) = fail $ "Haven't implmented: cConstant chars " ++ show char
  compile (CFloatConst float _) = fail  $ "Haven't implmented: cConstant floats " ++ show float
  compile (CStrConst str _) = fail $ "Haven't implmented: cConstant strings " ++ show str

main :: IO ()
main =
  do args <- getArgs
     let (cppOpts, files) = partition (isPrefixOf "-") args
     mapM_ (processFile GNU99 cppOpts) files
