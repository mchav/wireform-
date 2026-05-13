-- | Backward compatibility shim re-exporting name conversion utilities.
module Proto.CodeGen.Types (
  hsTypeName,
  hsFieldName,
  hsEnumCon,
  hsModuleName,
  hsScopedTypeName,
  hsScopedFieldName,
  hsScopedEnumCon,
  genTypeDecls,
  genEnumDecl,
  genOneofDecl,
) where

import Data.Text (Text)
import Prettyprinter (Doc)
import Proto.CodeGen (
  hsModuleName,
  hsTypeName,
  scopedFieldName,
  scopedTypeName,
  snakeToCamel,
  snakeToPascal,
 )
import Proto.IDL.AST (EnumDef, MessageDef, OneofDef)


hsFieldName :: Text -> Text
hsFieldName = snakeToCamel


hsEnumCon :: Text -> Text -> Text
hsEnumCon _enumName = snakeToPascal


hsScopedTypeName :: [Text] -> Text -> Text
hsScopedTypeName parents name = scopedTypeName (parents <> [name])


hsScopedFieldName :: [Text] -> Text -> Text
hsScopedFieldName = scopedFieldName


hsScopedEnumCon :: [Text] -> Text -> Text -> Text
hsScopedEnumCon scope _enumName valName =
  case scope of
    [] -> snakeToPascal valName
    _ -> scopedTypeName scope <> "'" <> snakeToPascal valName


genTypeDecls :: MessageDef -> [Doc ann]
genTypeDecls _ = []


genEnumDecl :: EnumDef -> Doc ann
genEnumDecl _ = mempty


genOneofDecl :: Text -> OneofDef -> Doc ann
genOneofDecl _ _ = mempty
