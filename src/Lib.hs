{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib where

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
import Types
import qualified RegPool as RP
import Language.C.Data.Error

processFile :: CLanguage -> [String] -> FilePath -> IO ()
processFile lang cppOpts file =
  do hPutStr stderr $ file ++ ": "
     let outFile = file ++ ".uasm"
     result <- parseCFile (newGCC "gcc") Nothing cppOpts file
     
     case result of
       Left err -> hPutStrLn stderr ('\n' : show err)
       Right tu -> case runTrav_ (body tu lang) of
                     Left errs -> hPutStrLn stderr ('\n' : concatMap show errs)
                     Right (asmProgram, errs) -> do
                       let prog = concat $ DL.intersperse "\n" (map show asmProgram)
                       putStrLn outFile
                       writeFile outFile (prog ++ "\n")
                     

body :: CTranslUnit -> CLanguage -> Trav s [BetaAsm]
body tu lang = do modifyOptions (\opts -> opts { language = lang })
                  analyseAST tu
                  cTranslUnit tu



--cTranslUnit :: (Monad m, Show t) => CTranslationUnit t -> m [BetaAsm]
cTranslUnit (CTranslUnit decls info) = liftM join $ mapM (compile RP.new) decls

instance Compile (CExternalDeclaration NodeInfo) where
  compile rp (CDeclExt decl) = undefined
  compile rp (CFDefExt funDef) = compile rp funDef
  compile rp (CAsmExt strLit info) = undefined

instance Compile (CFunctionDef NodeInfo) where
-- cFunDef (CFunDef [CDeclarationSpecifier a] (CDeclarator a) [CDeclaration a] (CStatement a) a
  compile rp (CFunDef decSpecs declarator declarations stmt info) = do
    --specs <- mapM compile decSpecs
    compile rp stmt
    --fail $  "undefined: cFunDef (CFunDef decSpecs declarator declarations stmt info)"

instance Compile (CDeclarationSpecifier NodeInfo) where
  compile rp (CStorageSpec spec) = error "undefined: cDecSpec (CStorageSpec spec)"
  compile rp (CTypeSpec typeSpec) = error "undefined: cDecSpec (CTypeSpec typeSpec)"
  compile rp (CTypeQual typeQual) = error "undefined: cDecSpec (CTypeQual typeQual)"


instance Compile (CDeclarator NodeInfo) where
  compile rp (CDeclr ident derivedDeclarators strLits attrs info) =
    error "undefined: cDeclr (CDeclr ident derivedDeclarators strLits attrs info)"
  -- CDecl [CDeclarationSpecifier a] [(Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a))] a
  -- CDeclr (Maybe Ident) [CDerivedDeclarator a] (Maybe (CStringLiteral a)) [CAttribute a] a

instance Compile (CDeclaration NodeInfo) where
  compile rp (CDecl declSpecs triples info) =
    error "cDecl (CDecl declSpecs triples info) = undefined"

instance Compile (CCompoundBlockItem NodeInfo) where
  compile rp (CBlockStmt stmt) = compile rp stmt
  compile rp (CBlockDecl decl) = compile rp decl
-- GNU C, not implemented yet.
  compile rp (CNestedFunDef funcDef) = error "undefined: blockStmt (CNestedFunDef funcDef)"

lbl x = Label x
label x = [Lbl $ Label x]


instance Compile (CStatement NodeInfo) where
  compile rp (CCompound [] blockItems info) = do
    --recordError "asdf"
    -- recordError (userErr $ "the sun set with a red light " ++ show info)
    -- recordError (userErr "large beans attacked the camp")
    -- throwTravError (userErr "no prisoners were taken")
    -- errs <- getErrors
    -- fail $ show errs
    xs <- mapM (compile rp) blockItems -- fail "undefined: cStmt (CCompound [] blockItems info)"
    return (concat xs)
  
  compile _ (CCompound idents blockItems info) =
    fail "undefined: cStmt (CCompound idents blockItems info)"

  compile rp (CIf expr stmt1 Nothing info) = do
    cond <- compile rp expr
    ss <- compile rp stmt1
    
    return $ concat [ cond
                    , [bf R1 (Label "endif") ]
                    , ss 
                    , [Lbl $ Label "endif"] ]
      
  compile rp (CIf expr stmt1 (Just stmt2) info) = do
    cond <- compile rp expr
    s1 <- compile rp stmt1 
    s2 <- compile rp stmt2
   
    return $ concat [ cond
                    , [bf R1 (Label "Lelse")]
                    , s1
                    , [br $ lbl "Lendif"]
                    , label "Lelse"
                    , s2
                    , label "Lendif"
                    ]

  compile _ (CReturn Nothing info) = return []
  compile _ (CReturn (Just expr) a) = return []
  compile _ x = fail $ show x

instance Compile (CExpression NodeInfo) where
  compile rp (CComma expr info) = fail "cExpr CComma expr info = "
  compile rp (CAssign binop e1 e2 info) = fail "cExpr CAssign binop e1 e2 info ="
  compile rp (CCond e1 Nothing e2 info) = fail "cExpr CCond e1 Nothing e2 info ="
  compile rp (CCond e1 (Just e2) e3 info) = fail "cExpr CCond e1 (Just e2) e3 info ="

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
  -- cExpr CCall (CExpression a) [CExpression a] a
  -- cExpr CMember (CExpression a) Ident Bool a
  -- cExpr CVar Ident a
  compile rp c@(CConst const) = compile rp const
  -- --integer, character, floating point and string constants
  -- cExpr CCompoundLit (CDeclaration a) (CInitializerList a) a
  -- cExpr C99 compound literal
  -- cExpr CStatExpr (CStatement a) a
  -- cExpr GNU C compound statement as expr
  -- cExpr CLabAddrExpr Ident a
  -- cExpr GNU C address of label
  -- cExpr CBuiltinExpr (CBuiltinThing a)
  --builtin expressions, see CBuiltin
  compile _ x = fail $ show x


instance Compile (CConstant NodeInfo) where
  compile rp (CIntConst n _) = return [cmove (IntConstant n) R1]
  compile rp (CCharConst char _) = fail $ "Haven't implmented: cConstant chars " ++ show char
  compile rp (CFloatConst float _) = fail  $ "Haven't implmented: cConstant floats " ++ show float
  compile ro (CStrConst str _) = fail $ "Haven't implmented: cConstant strings " ++ show str
  
main :: IO ()
main =
  do args <- getArgs
     let (cppOpts, files) = partition (isPrefixOf "-") args
     mapM_ (processFile GNU99 cppOpts) files




