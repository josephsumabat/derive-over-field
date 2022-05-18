module DeriveOverField (deriveOverField) where

import Language.Haskell.TH

-- | Derive an over function for a nested field
-- Given:
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
-- $(deriveOverField 'someField)
--
-- will give:
--
-- overSometypeFieldsomeField :: (ChildType -> Childtype) -> Parenttype -> Parenttype 
-- overSometypeFieldsomeField modifyFieldFn parentType = parentType {someField = modifyFieldFn (someField parentType)}
deriveOverField :: Name -> Q [Dec]
deriveOverField fieldName = do
  let myType = mkName "parentType"
      modifyFieldFn = mkName "modifyFieldFn"
  fieldType  <- reifyType fieldName
  case fieldType of
    AppT (AppT ArrowT parentType@(ConT parentTypeName)) childType ->
      let 
          overType = mkName ("over" <> nameBase parentTypeName <> "Field" <> nameBase fieldName) 
          typeSig = SigD overType (AppT (AppT ArrowT (AppT (AppT ArrowT childType) childType)) (AppT (AppT ArrowT parentType) parentType))
          functionDefinition =
            FunD overType [Clause [VarP modifyFieldFn,VarP myType] (NormalB (RecUpdE (VarE myType) [(fieldName,AppE (VarE modifyFieldFn) (ParensE (AppE (VarE fieldName) (VarE myType))))])) []] in
      pure [typeSig, functionDefinition]
    _ -> error "Invalid type input to deriveOverField: input must be a field function"
