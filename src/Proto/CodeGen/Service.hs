-- | Code generation for gRPC service definitions.
--
-- Generates Haskell service interfaces from proto service definitions:
--
-- * A record type for the service with one field per RPC method
-- * A client record with functions for calling each method
-- * Type aliases for request/response types
-- * Method metadata (method names, streaming modes)
--
-- The generated code is transport-agnostic: it defines the interface
-- but does not depend on any specific HTTP/2 or gRPC library.
-- Users wire up the transport layer by providing implementations.
module Proto.CodeGen.Service
  ( genServiceDecls
  , genServiceModule
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

import Data.Char (toLower, toUpper)
import Proto.AST

-- | Generate all declarations for a service definition.
genServiceDecls :: Maybe Text -> [Text] -> ServiceDef -> [Doc ann]
genServiceDecls pkg scope svc =
  [ mempty
  , genServiceDoc pkg svc
  , genServerType scope svc
  , mempty
  , genClientType scope svc
  , mempty
  , genMethodInfos scope svc
  , mempty
  , genServiceMeta scope svc
  ]

-- | Generate a complete module for a service.
genServiceModule :: Maybe Text -> Text -> ServiceDef -> Doc ann
genServiceModule pkg prefix svc =
  let modName = prefix <> "." <> hsModuleName' (fromMaybe "" pkg) <> "." <> hsTypeName' (svcName svc)
  in vsep
    [ pretty ("{-# LANGUAGE OverloadedStrings #-}" :: Text)
    , pretty ("-- | Generated gRPC service interface for " :: Text) <> pretty (svcName svc)
    , pretty ("module " :: Text) <> pretty modName <> pretty (" where" :: Text)
    , mempty
    , pretty ("import Data.ByteString (ByteString)" :: Text)
    , pretty ("import Data.Text (Text)" :: Text)
    , pretty ("import Proto.Encode (MessageEncode, encodeMessage)" :: Text)
    , pretty ("import Proto.Decode (MessageDecode, decodeMessage)" :: Text)
    , mempty
    , vsep (genServiceDecls pkg [] svc)
    ]

genServiceDoc :: Maybe Text -> ServiceDef -> Doc ann
genServiceDoc pkg svc =
  let qualified = maybe (svcName svc) (\p -> p <> "." <> svcName svc) pkg
  in vsep
    [ pretty ("-- | gRPC service @" :: Text) <> pretty qualified <> pretty ("@." :: Text)
    , pretty ("--" :: Text)
    , pretty ("-- Methods:" :: Text)
    , vsep (fmap (\r ->
        pretty ("-- * " :: Text) <> pretty (rpcName r) <>
        pretty (" (" :: Text) <> streamLabel (rpcInputStr r) <>
        pretty (rpcInput r) <> pretty (" -> " :: Text) <>
        streamLabel (rpcOutputStr r) <> pretty (rpcOutput r) <>
        pretty (")" :: Text)) (svcRpcs svc))
    ]

streamLabel :: StreamQualifier -> Doc ann
streamLabel NoStream  = mempty
streamLabel Streaming = pretty ("stream " :: Text)

-- | Generate the server handler record type.
genServerType :: [Text] -> ServiceDef -> Doc ann
genServerType scope svc =
  let tyN = svcTypeName scope svc <> "Server"
  in vsep
    [ pretty ("-- | Server handler record for @" :: Text) <> pretty (svcName svc) <> pretty ("@." :: Text)
    , pretty ("-- Each field is a handler function for one RPC method." :: Text)
    , pretty ("-- Implement all fields to create a server." :: Text)
    , pretty ("data " :: Text) <> pretty tyN <> pretty (" m = " :: Text) <> pretty tyN
    , indent 2 (braceFields (fmap (genServerField scope) (svcRpcs svc)))
    ]

genServerField :: [Text] -> RpcDef -> Doc ann
genServerField scope rpc =
  let fname = lowerFirst' (snakeToCamel' (rpcName rpc)) <> "Handler"
  in pretty (escapeReserved' fname) <+> pretty ("::" :: Text) <+> genRpcServerType scope rpc

genRpcServerType :: [Text] -> RpcDef -> Doc ann
genRpcServerType _ rpc = case (rpcInputStr rpc, rpcOutputStr rpc) of
  (NoStream, NoStream) ->
    pretty (hsTypeName' (rpcInput rpc)) <+> pretty ("-> m" :: Text) <+> pretty (hsTypeName' (rpcOutput rpc))
  (Streaming, NoStream) ->
    pretty ("m " :: Text) <> pretty (hsTypeName' (rpcInput rpc)) <+>
    pretty ("-> m" :: Text) <+> pretty (hsTypeName' (rpcOutput rpc))
  (NoStream, Streaming) ->
    pretty (hsTypeName' (rpcInput rpc)) <+>
    pretty ("-> (" :: Text) <> pretty (hsTypeName' (rpcOutput rpc)) <+>
    pretty ("-> m ()) -> m ()" :: Text)
  (Streaming, Streaming) ->
    pretty ("m " :: Text) <> pretty (hsTypeName' (rpcInput rpc)) <+>
    pretty ("-> (" :: Text) <> pretty (hsTypeName' (rpcOutput rpc)) <+>
    pretty ("-> m ()) -> m ()" :: Text)

-- | Generate the client record type.
genClientType :: [Text] -> ServiceDef -> Doc ann
genClientType scope svc =
  let tyN = svcTypeName scope svc <> "Client"
  in vsep
    [ pretty ("-- | Client stub record for @" :: Text) <> pretty (svcName svc) <> pretty ("@." :: Text)
    , pretty ("-- Each field is a function for calling one RPC method." :: Text)
    , pretty ("data " :: Text) <> pretty tyN <> pretty (" m = " :: Text) <> pretty tyN
    , indent 2 (braceFields (fmap (genClientField scope) (svcRpcs svc)))
    ]

genClientField :: [Text] -> RpcDef -> Doc ann
genClientField scope rpc =
  let fname = lowerFirst' (snakeToCamel' (rpcName rpc))
  in pretty (escapeReserved' fname) <+> pretty ("::" :: Text) <+> genRpcClientType scope rpc

genRpcClientType :: [Text] -> RpcDef -> Doc ann
genRpcClientType = genRpcServerType

-- | Generate method metadata.
genMethodInfos :: [Text] -> ServiceDef -> Doc ann
genMethodInfos scope svc =
  let tyN = svcTypeName scope svc
  in vsep
    [ pretty ("-- | Method metadata for @" :: Text) <> pretty (svcName svc) <> pretty ("@." :: Text)
    , pretty ("data " :: Text) <> pretty tyN <> pretty ("Method" :: Text)
    , indent 2 (vsep (zipWith (\pfx r -> pfx <+>
        pretty (hsTypeName' (rpcName r) <> "Method")) seps (svcRpcs svc)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Ord, Enum, Bounded)" :: Text))
    , mempty
    , pretty (T.toLower tyN <> "MethodName :: " :: Text) <> pretty tyN <> pretty ("Method -> Text" :: Text)
    , vsep (fmap (\r ->
        pretty (T.toLower tyN <> "MethodName " :: Text) <>
        pretty (hsTypeName' (rpcName r) <> "Method" :: Text) <+>
        pretty ("= \"" :: Text) <> pretty (rpcName r) <> pretty ("\"" :: Text)
        ) (svcRpcs svc))
    ]
  where
    seps = pretty ("=" :: Text) : repeat (pretty ("|" :: Text))

-- | Generate ProtoService metadata instance.
genServiceMeta :: [Text] -> ServiceDef -> Doc ann
genServiceMeta scope svc =
  let tyN = svcTypeName scope svc
      fullName = T.intercalate "." (scope <> [svcName svc])
  in vsep
    [ pretty ("-- | Fully-qualified service name: @" :: Text) <> pretty fullName <> pretty ("@" :: Text)
    , pretty (T.toLower tyN <> "ServiceName :: Text" :: Text)
    , pretty (T.toLower tyN <> "ServiceName = \"" :: Text) <> pretty fullName <> pretty ("\"" :: Text)
    , mempty
    , pretty ("-- | All method paths for @" :: Text) <> pretty (svcName svc) <> pretty ("@." :: Text)
    , pretty (T.toLower tyN <> "MethodPaths :: [Text]" :: Text)
    , pretty (T.toLower tyN <> "MethodPaths = " :: Text) <>
      pretty (T.pack (show (fmap (\r -> "/" <> fullName <> "/" <> rpcName r) (svcRpcs svc))))
    ]

svcTypeName :: [Text] -> ServiceDef -> Text
svcTypeName scope svc = T.intercalate "'" (fmap hsTypeName' (scope <> [svcName svc]))

braceFields :: [Doc ann] -> Doc ann
braceFields [] = pretty ("{ }" :: Text)
braceFields (f:fs) =
  vsep (pretty ("{ " :: Text) <> f : fmap (\x -> pretty (", " :: Text) <> x) fs) <>
  line <> pretty ("}" :: Text)

hsTypeName' :: Text -> Text
hsTypeName' t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing        -> t

hsModuleName' :: Text -> Text
hsModuleName' = T.intercalate (T.singleton '.') . fmap capitalize . T.splitOn (T.singleton '.')
  where
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> t

snakeToCamel' :: Text -> Text
snakeToCamel' t =
  let parts = T.splitOn (T.singleton '_') t
  in case parts of
    []     -> t
    (p:ps) -> T.concat (lowerFirst' p : fmap titleCase ps)

lowerFirst' :: Text -> Text
lowerFirst' s = case T.uncons s of
  Just (c, rest) -> T.cons (toLower c) (T.toLower rest)
  Nothing        -> s

titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing        -> s

escapeReserved' :: Text -> Text
escapeReserved' t
  | t `elem` reserved = t <> "'"
  | otherwise = t
  where
    reserved :: [Text]
    reserved = [ "type", "class", "data", "default", "deriving", "do", "else"
               , "if", "import", "in", "infix", "infixl", "infixr", "instance"
               , "let", "module", "newtype", "of", "then", "where", "case" ]
