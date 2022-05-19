{-# LANGUAGE NamedFieldPuns #-}
module DeriveOverField (
  deriveOverField
 , deriveOverFieldWithOptions
 , deriveOverAllFields
) where

import Language.Haskell.TH
import Data.Maybe
import Control.Monad
import qualified Data.Char as Char

-- | Derive an over function for a nested field
-- overSomeField functions are useful as a composable setter that does not have as much overhead as lenses
-- Given:
--
-- @
-- data MyType =
--   ParentType {
--     someField :: ChildType
--   , someOtherField :: Int
--   }
--   deriving (Show)
--
-- data MyType2=
--   MyType2
--     {
--      someField2 :: Int
--     , someOtherField2 :: Int
--     }
--     deriving (Show)
-- @
--
-- then:
--
-- @

-- $(deriveOverField 'someField)
-- @
--
-- will give:
--
-- @
-- overSomefield :: (ChildType -> Childtype) -> Parenttype -> Parenttype
-- overSomefield modifyFieldFn parentType = parentType {someField = modifyFieldFn (someField parentType)}
-- @
deriveOverField :: Name -> Q [Dec]
deriveOverField = deriveOverFieldWithOptions OverField

-- | deriveOverField with configurable function names 
deriveOverFieldWithOptions :: DeriveOverFnNameOption -> Name  -> Q [Dec]
deriveOverFieldWithOptions fnNameOption fieldName= do
  myType <- newName "parentType"
  modifyFieldFn <- newName "modifyFieldFn"
  fieldType <- reifyType fieldName
  case fieldType of
    AppT (AppT ArrowT parentType) childType ->
      let moduleNameAndBaseName n = fromMaybe "" (nameModule n) <> "Module" <> (upperFirst . nameBase) n
          overName = 
            case fnNameOption of
              OverField -> mkName $ "over" <> (upperFirst . normalizeName . nameBase) fieldName
              OverModuleField -> mkName . normalizeName $ "over" <> moduleNameAndBaseName fieldName
          -- AST for the type signature: (ChildType -> Childtype) -> Parenttype -> Parenttype
          typeSig = SigD overName (AppT (AppT ArrowT (AppT (AppT ArrowT childType) childType)) (AppT (AppT ArrowT parentType) parentType))
          -- AST for the fn definition: overSometypeSomeField modifyFieldFn parentType = parentType {someField = modifyFieldFn (someField parentType)}
          functionDefinition =
            FunD overName [Clause [VarP modifyFieldFn, VarP myType] (NormalB (RecUpdE (VarE myType) [(fieldName, AppE (VarE modifyFieldFn) (ParensE (AppE (VarE fieldName) (VarE myType))))])) []]
       in do
         pure [typeSig, functionDefinition]
    _ -> error "Only simple constructors supported"
  where
    upperFirst :: String -> String
    upperFirst [] = []
    upperFirst (c : cs) = Char.toUpper c : cs
    normalizeName :: String -> String
    normalizeName = filter Char.isAlphaNum

-- | Derive all fields for a type
-- (Will only derive top level fields)
-- Note:
--  - Currently not implemented for newtype
deriveOverAllFields :: DeriveOverFnNameOption -> Name -> Q [Dec]
deriveOverAllFields fnNameOption typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC _ rawVarbangs] _) ->
      let fieldNames = (getVarBangName <$> rawVarbangs)
       in join <$> mapM (deriveOverFieldWithOptions fnNameOption) fieldNames
    _ -> reportError "Invalid type input to deriveOverAllFields: input must be a data type constructor." >> pure []
  where
    getVarBangName (n, _, _) = n

-- | Set the naming convention for generated functions. Useful if there are conflicts
data DeriveOverFnNameOption
  = 
    OverField
    -- ^ function name will be overField if field is a field of Mytype
  | OverModuleField
    -- ^ function name will be overMymoduleField if field is a field of the type Mytype
