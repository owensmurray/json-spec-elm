{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.Elm (
  elmDefs,
  Definitions,
  HasType(..),
) where


import Bound (Scope(Scope), Var(B, F), toScope)
import Control.Monad.Writer (MonadWriter(tell), Writer, execWriter)
import Data.JsonSpec (Specification(JsonArray, JsonBool, JsonDateTime,
  JsonEither, JsonInt, JsonLet, JsonNullable, JsonNum, JsonObject,
  JsonRef, JsonString, JsonTag))
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.TypeLits (ErrorMessage((:$$:), (:<>:)), KnownSymbol, Symbol,
  TypeError, symbolVal)
import Language.Elm.Definition (Definition)
import Language.Elm.Expression ((|>), Expression, bind, if_)
import Language.Elm.Name (Constructor, Qualified)
import Language.Elm.Type (Type)
import Prelude (Applicative(pure), Foldable(foldl), Functor(fmap),
  Maybe(Just, Nothing), Monad((>>)), Semigroup((<>)), Show(show), ($),
  (++), (.), (<$>), Int, error, reverse, zip)
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.TypeLits as Lits
import qualified Language.Elm.Definition as Def
import qualified Language.Elm.Expression as Expr
import qualified Language.Elm.Name as Name
import qualified Language.Elm.Pattern as Pat
import qualified Language.Elm.Type as Type


elmDefs
  :: forall spec. (HasType spec)
  => Proxy (spec :: Specification)
  -> Set Definition
elmDefs _ =
  execWriter $ typeOf @spec >> decoderOf @spec


class Record (spec :: [(Symbol, Specification)]) where
  recordDefs :: forall v. Definitions [(Name.Field, Type v)]
  recordEncoders :: Definitions [(Text, Name.Field, Expression Void)]
  recordDecoders :: Definitions [(Text, Expression Void)]
instance Record '[] where
  recordDefs = pure []
  recordEncoders = pure []
  recordDecoders = pure []
instance
    ( HasType spec
    , KnownSymbol name
    , Record more
    )
  =>
    Record ( '(name, spec) : more )
  where
    recordDefs = do
      type_ <- typeOf @spec
      moreFields <- recordDefs @more
      pure $ (fieldName (sym @name), type_) : moreFields
    recordEncoders = do
      encoder <- encoderOf @spec
      moreFields <- recordEncoders @more
      pure $ (sym @name, fieldName (sym @name), encoder) : moreFields
    recordDecoders = do
      dec <- decoderOf @spec
      more <- recordDecoders @more
      pure $ ( sym @name , dec) : more


class HasType (spec :: Specification) where
  typeOf :: forall v. Definitions (Type v)
  decoderOf :: Definitions (Expression Void)
  encoderOf :: Definitions (Expression Void)
instance HasType JsonString where
  typeOf = pure "String.String"
  decoderOf = pure "Json.Decode.string"
  encoderOf = pure "Json.Encode.string"
instance HasType JsonNum where
  typeOf = pure "Basics.Float"
  decoderOf = pure "Json.Decode.float"
  encoderOf = pure "Json.Encode.float"
instance HasType JsonInt where
  typeOf = pure "Basics.Int"
  decoderOf = pure "Json.Decode.int"
  encoderOf = pure "Json.Encode.int"
instance {- HasType (JsonObject fields) -}
    ( Record fields
    , BaseFields (Reverse fields)
    , Lambda (LambdaDepth (Reverse fields))
    )
  =>
    HasType (JsonObject fields)
  where
    typeOf = Type.Record <$> recordDefs @fields
    decoderOf = do
        decoders <- recordDecoders @fields
        pure $
          foldl
            (\expr (_, decoder) ->
              expr |> ("Json.Decode.andThen" `a`
                Expr.Lam (toScope (
                  "Json.Decode.map"
                    `a` Expr.Var (B ())
                    `a` bind Expr.Global absurd decoder
                ))
              )
            )
            ("Json.Decode.succeed" `a` lambda)
            decoders
      where
        lambda =
          lam . Expr.Record . reverse $
            [ (name, var)
            | (name, var) <- baseFields @(Reverse fields)
            ]
    encoderOf = do
        fields <- recordEncoders @fields
        pure $
          Expr.Lam . toScope $
            "Json.Encode.object" `a`
            Expr.List
              [ Expr.apps "Basics.," [
                Expr.String jsonField,
                Expr.bind Expr.Global absurd encoder `a`
                  (Expr.Proj elmField `a` Expr.Var var)
                ]
              | (jsonField, elmField, encoder) <- fields
              ]
      where
        var :: Bound.Var () a
        var = B ()
instance (HasType spec) => HasType (JsonArray spec) where
  typeOf = do
    elemType <- typeOf @spec
    pure $ "Basics.List" `ta` elemType
  decoderOf = do
    dec <- decoderOf @spec
    pure $ "Json.Decode.list" `a` dec
  encoderOf = do
    encoder <- encoderOf @spec
    pure $ "Json.Encode.list" `a` encoder
instance HasType JsonBool where
  typeOf = pure "Basics.Bool"
  decoderOf = pure "Json.Decode.bool"
  encoderOf =
    pure "Json.Encode.bool"
instance (HasType spec) => HasType (JsonNullable spec) where
  typeOf = do
    type_ <- typeOf @spec
    pure $ "Maybe.Maybe" `ta` type_
  decoderOf = do
    dec <- decoderOf @spec
    pure $ a "Json.Decode.nullable" dec
  encoderOf = do
    encoder <- encoderOf @spec
    pure $
      Expr.Lam . toScope $
        Expr.apps
          "Maybe.withDefault"
          [ "Json.Decode.null"
          , Expr.apps
              "Maybe.map"
              [ Expr.bind Expr.Global absurd encoder
              , Expr.Var (B ())
              ]
          ]
instance (KnownSymbol const) => HasType (JsonTag const) where
  typeOf = pure "Basics.()"
  decoderOf =
    pure $
      "Json.Decode.string"
      |> Expr.apps "Json.Decode.andThen"
          [ Expr.Lam . toScope $
              if_
                (
                  Expr.apps
                    "Basics.=="
                    [ Expr.Var (B ())
                    , Expr.String (sym @const)
                    ]
                )
                (a "Json.Decode.succeed" "Basics.()")
                (a "Json.Decode.fail" (Expr.String "Tag mismatch"))
          ]
  encoderOf =
    pure $
      "Basics.always" `a`
        ("Json.Encode.string" `a` Expr.String (sym @const))
instance HasType JsonDateTime where
  typeOf = pure "Time.Posix"
  decoderOf = pure "Iso8601.decoder"
  encoderOf = pure "Iso8601.encode"
instance (KnownSymbol name) => HasType (JsonRef name) where
  typeOf =
    pure
    . Type.Global
    . localName
    $ sym @name
  decoderOf =
    pure . Expr.Global $ decoderName @name
  encoderOf =
    pure . Expr.Global $ encoderName @name
instance (HasType spec) => HasType (JsonLet '[] spec) where
  typeOf = typeOf @spec
  decoderOf = decoderOf @spec
  encoderOf = encoderOf @spec
instance {- HasType (JsonLet ( def : more ) spec) -}
    ( HasDef def
    , HasType (JsonLet more spec)
    )
  =>
    HasType (JsonLet ( def : more ) spec)
  where
    typeOf = do
      defs @def
      typeOf @(JsonLet more spec)
    decoderOf = do
      defs @def
      decoderOf @(JsonLet more spec)
    encoderOf = do
      defs @def
      encoderOf @(JsonLet more spec)
instance {- HasType (JsonEither left right) -}
    ( TypeError
        ( Lits.Text "Elm doesn't support anonymous sum types, so if you "
          :<>: Lits.Text "want to use (possibly nested) `JsonEither` "
          :<>: Lits.Text "you must give it a name using `JsonLet`, e.g:"
          :$$: Lits.Text ""
          :$$: Lits.Text "> JsonLet"
          :$$: Lits.Text ">   '[ '( \"MySum\""
          :$$: Lits.Text ">       , JsonEither"
          :$$: Lits.Text ">           ( JsonEither"
          :$$: Lits.Text ">               JsonInt"
          :$$: Lits.Text ">               JsonString"
          :$$: Lits.Text ">           )"
          :$$: Lits.Text ">           ( JsonEither"
          :$$: Lits.Text ">               JsonFloat"
          :$$: Lits.Text ">               JsonBool"
          :$$: Lits.Text ">           )"
          :$$: Lits.Text ">       )"
          :$$: Lits.Text ">    ]"
          :$$: Lits.Text ">    (JsonRef \"MySum\")"
          :$$: Lits.Text ""
          :$$: Lits.Text "This will produce the Elm type"
          :$$: Lits.Text ""
          :$$: Lits.Text "> type MySum"
          :$$: Lits.Text ">   = MySum_1 Int"
          :$$: Lits.Text ">   | MySum_2 String"
          :$$: Lits.Text ">   | MySum_3 Float"
          :$$: Lits.Text ">   | MySum_4 Bool"
          :$$: Lits.Text ""

        )
    )
  =>
    HasType (JsonEither left right)
  where
    typeOf = error "undefinable"
    decoderOf = error "undefinable"
    encoderOf = error "undefinable"


type family LambdaDepth (record :: [k]) where
  LambdaDepth '[] = Void
  LambdaDepth (a : more) =
    Bound.Var () (LambdaDepth more)


class Lambda depth where
  lam :: Expression depth -> Expression Void
instance {-# OVERLAPS #-} Lambda (Bound.Var () Void) where
  lam e = Expr.Lam (toScope e)
instance (Lambda deeper) => Lambda (Bound.Var () deeper) where
  lam e = lam (Expr.Lam (toScope e))


class BaseFields (record :: [(Symbol, Specification)]) where
  baseFields :: [(Name.Field, Expression (LambdaDepth record))]
instance BaseFields '[] where
  baseFields = []
instance {- BaseFields ('(name, spec) : more) -}
    (BaseFields more, KnownSymbol name)
  =>
    BaseFields ('(name, spec) : more)
  where
    baseFields =
        (fieldName (sym @name), Expr.Var (B ())) :
        [ (name, b var)
        | (name, var) <- baseFields @more
        ]
      where
        b = bind Expr.Global (Expr.Var . F)


type family Reverse (l :: [k]) where
  Reverse '[] = '[]
  Reverse (a : more) = Concat (Reverse more) '[a]


type family Concat (a :: [k]) (b :: [k]) where
  Concat '[] b = b
  Concat (a : more) b =
    a : Concat more b


class HasDef (def :: (Symbol, Specification)) where
  defs :: Definitions ()
instance {-# OVERLAPS #-}
    ( KnownSymbol name
    , SumDef (JsonEither left right)
    )
  =>
    HasDef '(name, JsonEither left right)
  where
    defs = do
        branches <- sumDef @(JsonEither left right)
        let
          constructors :: [(Constructor, [Scope Int Type Void])]
          constructors =
            [ ( Name.Constructor (constructorName n)
              , [Scope type_]
              )
            | (n, type_) <- zip [(1 :: Int) ..] branches
            ]
        decoders <- sumDecoders @(JsonEither left right)
        encoders <- sumEncoders @(JsonEither left right)
        tell . Set.fromList $
          [ Def.Type (localName name) 0 constructors
          , Def.Constant
              (decoderName @name)
              0
              (Scope ("Json.Decode.Decoder" `ta` Type.Global (localName name)))
              (
                "Json.Decode.oneOf"
                `a`
                Expr.List
                  [ "Json.Decode.map"
                    `a` Expr.Global (localName (constructorName n))
                    `a` dec
                  | (n, dec) <-  zip [(1 :: Int) ..] decoders
                  ]
              )
          , Def.Constant
              (encoderName @name)
              0
              (
                toScope $
                  Type.Fun
                    (Type.Global (localName name))
                    "Json.Encode.Value"
              )
              (
                Expr.Lam . toScope $
                  Expr.Case
                    (Expr.Var (B ()))
                    [ ( Pat.Con (localName (constructorName n)) [Pat.Var 0]
                      , toScope $
                        fmap absurd encoder `a`
                          Expr.Var (B (0 :: Int))
                      )
                    | (n, encoder) <- zip [1..] encoders
                    ]
              )
          ]
      where
        constructorName :: Int -> Text
        constructorName n = name <> "_" <> showt n

        name :: Text
        name = sym @name
instance (HasType spec, KnownSymbol name) => HasDef '(name, spec) where
  defs = do
    type_ <- typeOf @spec
    dec <- decoderOf @spec
    enc <- encoderOf @spec
    tell . Set.fromList $
      [ Def.Alias
          (localName (sym @name))
          0
          (Scope type_)
      , Def.Constant
          (decoderName @name)
          0
          ( Scope
              (
                "Json.Decode.Decoder" `ta`
                  Type.Global (localName (sym @name))
              )
          )
          dec
      , Def.Constant
          (encoderName @name)
          0
          ( Scope
              ( Type.Fun
                  (Type.Global $ localName (sym @name))
                  "Json.Encode.Value"
              )
          )
          enc
      ]


class SumDef (spec :: Specification) where
  sumDef :: forall v. Definitions [Type v]
  sumDecoders :: Definitions [Expression Void]
  sumEncoders :: Definitions [Expression Void]
instance {-# OVERLAPS #-}
    (SumDef (JsonEither a b), SumDef (JsonEither c d))
  =>
    SumDef (JsonEither (JsonEither a b) (JsonEither c d))
  where
    sumDef = do
      left <- sumDef @(JsonEither a b)
      right <- sumDef @(JsonEither c d)
      pure $ left ++ right
    sumDecoders = do
      left <- sumDecoders @(JsonEither a b)
      right <- sumDecoders @(JsonEither c d)
      pure (left ++ right)
    sumEncoders = do
      left <- sumEncoders @(JsonEither a b)
      right <- sumEncoders @(JsonEither c d)
      pure (left ++ right)
instance {-# OVERLAPS #-}
    (SumDef (JsonEither a b), HasType right)
  =>
    SumDef (JsonEither (JsonEither a b) right)
  where
    sumDef = do
      left <- sumDef @(JsonEither a b)
      right <- typeOf @right
      pure $ left ++ [right]
    sumDecoders = do
      left <- sumDecoders @(JsonEither a b)
      right <- decoderOf @right
      pure $ left ++ [right]
    sumEncoders = do
      left <- sumEncoders @(JsonEither a b)
      right <- encoderOf @right
      pure $ left ++ [right]
instance {-# OVERLAPS #-}
    (SumDef (JsonEither c d), HasType left)
  =>
    SumDef (JsonEither left (JsonEither c d))
  where
    sumDef = do
      left <- typeOf @left
      right <- sumDef @(JsonEither c d)
      pure $ left : right
    sumDecoders = do
      left <- decoderOf @left
      right <- sumDecoders @(JsonEither c d)
      pure $ left : right
    sumEncoders = do
      left <- encoderOf @left
      right <- sumEncoders @(JsonEither c d)
      pure $ left : right
instance
    (HasType left, HasType right)
  =>
    SumDef (JsonEither left right)
  where
    sumDef = do
      left <- typeOf @left
      right <- typeOf @right
      pure [left, right]
    sumDecoders = do
      left <- decoderOf @left
      right <- decoderOf @right
      pure [left, right]
    sumEncoders = do
      left <- encoderOf @left
      right <- encoderOf @right
      pure [left, right]


localName :: Text -> Qualified
localName =
  Name.Qualified ["Api", "Data"]


type Definitions = Writer (Set Definition)


sym :: forall a b. (KnownSymbol a, IsString b) => b
sym = fromString $ symbolVal (Proxy @a)


showt :: (Show a, IsString b) => a -> b
showt = fromString . show


lower :: Text -> Text
lower txt =
  case Text.uncons txt of
    Nothing -> txt
    Just (c, more) -> Text.cons (Char.toLower c) more


decoderName :: forall name. (KnownSymbol name) => Qualified
decoderName = localName (lower (sym @name) <> "Decoder")

encoderName :: forall name. (KnownSymbol name) => Qualified
encoderName = localName (lower (sym @name) <> "Encoder")


fieldName :: Text -> Name.Field
fieldName specName =
  Name.Field $
    case specName of
      "type" -> "type_"
      other -> Text.replace "-" "_" other


a :: Expression v -> Expression v -> Expression v
a = Expr.App


ta :: Type v -> Type v -> Type v
ta = Type.App


