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

processFile :: CLanguage -> [String] -> FilePath -> IO ()
processFile lang cppOpts file =
  do hPutStr stderr $ file ++ ": "
     let outFile = file ++ ".uasm"
     result <- parseCFile (newGCC "gcc") Nothing cppOpts file
     
     case result of
       Left err -> hPutStrLn stderr ('\n' : show err)
       Right tu -> case runTrav_ (body tu) of
                     Left errs -> hPutStrLn stderr ('\n' : concatMap show errs)
                     Right (asmProgram, errs) -> do
                       let prog = concat $ DL.intersperse "\n" (map show asmProgram)
                       putStrLn outFile
                       writeFile outFile prog
                     
  where body tu = do modifyOptions (\opts -> opts { language = lang })
                     analyseAST tu
                     cTranslUnit tu


cTranslUnit :: (Monad m, Show t) => CTranslationUnit t -> m [BetaAsm]
cTranslUnit (CTranslUnit decls info) =  liftM join $ mapM cExtDecl decls

cExtDecl (CDeclExt decl) = undefined
cExtDecl (CFDefExt funDef) = cFunDef funDef
cExtDecl (CAsmExt strLit info) = undefined

-- cFunDef (CFunDef [CDeclarationSpecifier a] (CDeclarator a) [CDeclaration a] (CStatement a) a
cFunDef (CFunDef decSpecs declarator declarations stmt info) = do
  --specs <- mapM cDecSpec decSpecs
  cStmt stmt
  --fail $  "undefined: cFunDef (CFunDef decSpecs declarator declarations stmt info)"

cDecSpec (CStorageSpec spec) = error "undefined: cDecSpec (CStorageSpec spec)"
cDecSpec (CTypeSpec typeSpec) =error "undefined: cDecSpec (CTypeSpec typeSpec)"
cDecSpec (CTypeQual typeQual) =error "undefined: cDecSpec (CTypeQual typeQual)"


-- CDeclr (Maybe Ident) [CDerivedDeclarator a] (Maybe (CStringLiteral a)) [CAttribute a] a
cDeclr (CDeclr ident derivedDeclarators strLits attrs info) =
  error "undefined: cDeclr (CDeclr ident derivedDeclarators strLits attrs info)"


-- CDecl [CDeclarationSpecifier a] [(Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a))] a
cDecl (CDecl declSpecs triples info) = error "cDecl (CDecl declSpecs triples info) = undefined"

blockStmt (CBlockStmt stmt) = cStmt stmt
blockStmt (CBlockDecl decl) = cDecl decl

-- GNU C, not implemented yet.
blockStmt (CNestedFunDef funcDef) = error "undefined: blockStmt (CNestedFunDef funcDef)"


--cStmt :: Monad m => CStatement t -> m [BetaAsm]

cStmt (CCompound [] blockItems info) = do
  xs <- mapM blockStmt blockItems -- fail "undefined: cStmt (CCompound [] blockItems info)"
  return (concat xs)
  
cStmt (CCompound idents blockItems info) = fail "undefined: cStmt (CCompound idents blockItems info)"

cStmt (CIf expr stmt1 Nothing info) = do
  c <- cExpr expr
  -- if xs represents true, then 
  ss <- cStmt stmt1
  
  return $ concat [ c
                  , [bf R1 (Label "endif") ]
                  , ss 
                  , [Lbl $ Label "endif"] ]
-- 
cStmt (CIf expr stmt1 (Just stmt2) info) = error "undefined: cStmt (CIf expr stmt1 (Just stmt2) info)"

cStmt (CReturn Nothing info) = return []
cStmt (CReturn (Just expr) a) = return []
cStmt x = error (show x)

-- cIf = do
--   rx <- cExpr expr
--   bf rx len


cExpr (CComma expr info) = fail "cExpr CComma expr info = "
cExpr (CAssign binop e1 e2 info) =fail "cExpr CAssign binop e1 e2 info ="
cExpr (CCond e1 Nothing e2 info) =fail "cExpr CCond e1 Nothing e2 info ="
cExpr (CCond e1 (Just e2) e3 info) =fail "cExpr CCond e1 (Just e2) e3 info ="

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
cExpr c@(CConst const) = cConstant const
-- --integer, character, floating point and string constants
-- cExpr CCompoundLit (CDeclaration a) (CInitializerList a) a
-- cExpr C99 compound literal
-- cExpr CStatExpr (CStatement a) a
-- cExpr GNU C compound statement as expr
-- cExpr CLabAddrExpr Ident a
-- cExpr GNU C address of label
-- cExpr CBuiltinExpr (CBuiltinThing a)
--builtin expressions, see CBuiltin
cExpr x = fail $ show x

cConstant (CIntConst n _) = return [cmove (IntConstant n) R1]
cConstant (CCharConst char _) = fail $ "Haven't implmented: cConstant chars " ++ show char
cConstant (CFloatConst float _) = fail  $ "Haven't implmented: cConstant floats " ++ show float
cConstant (CStrConst str _) = fail $ "Haven't implmented: cConstant strings " ++ show str
  
main :: IO ()
main =
  do args <- getArgs
     let (cppOpts, files) = partition (isPrefixOf "-") args
     mapM_ (processFile GNU99 cppOpts) files




