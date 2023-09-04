{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.HashMap.Strict (HashMap)
import Data.JsonSpec (Specification(JsonArray, JsonDateTime, JsonEither,
  JsonInt, JsonLet, JsonObject, JsonRef, JsonString, JsonTag))
import Data.JsonSpec.Elm (Named, elmDefs)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Language.Elm.Name (Module)
import Language.Elm.Pretty (modules)
import Prelude (Functor(fmap), Semigroup((<>)), ($), (.), IO)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.IO (stderr)
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

main :: IO ()
main =
  hspec $ do
    describe "thing" $ do
      it "works" $
        let
          actual :: HashMap Module Text
          actual =
            fmap ((<> "\n") . renderStrict . layoutPretty defaultLayoutOptions)
            . modules
            . Set.toList
            $ elmDefs (Proxy @TestSpec)

          expected :: HashMap Module Text
          expected =
            HM.singleton
              ["Api", "Data"]
              ( Text.unlines
                  [ "module Api.Data exposing"
                  , "    ( dashboardDecoder"
                  , "    , dashboardEncoder"
                  , "    , inviteDecoder"
                  , "    , inviteEncoder"
                  , "    , Invite(..)"
                  , "    , Dashboard"
                  , "    )"
                  , ""
                  , "import Iso8601"
                  , "import Json.Decode"
                  , "import Json.Encode"
                  , "import Time"
                  , ""
                  , ""
                  , "dashboardDecoder : Json.Decode.Decoder Dashboard"
                  , "dashboardDecoder ="
                  , "    Json.Decode.succeed (\\a b c -> { proposals = a, credits = b, user = c }) |>"
                  , "    Json.Decode.andThen (\\a -> Json.Decode.map a (Json.Decode.list (Json.Decode.succeed (\\b c -> { key = b"
                  , "    , value = c }) |>"
                  , "    Json.Decode.andThen (\\b -> Json.Decode.map b Json.Decode.string) |>"
                  , "    Json.Decode.andThen (\\b -> Json.Decode.map b (Json.Decode.succeed (\\c d e f g h i j -> { name = c"
                  , "    , owner = d"
                  , "    , availability = e"
                  , "    , description = f"
                  , "    , venue = g"
                  , "    , invites = h"
                  , "    , created_at = i"
                  , "    , attachments = j }) |>"
                  , "    Json.Decode.andThen (\\c -> Json.Decode.map c Json.Decode.string) |>"
                  , "    Json.Decode.andThen (\\c -> Json.Decode.map c Json.Decode.string) |>"
                  , "    Json.Decode.andThen (\\c -> Json.Decode.map c (Json.Decode.list (Json.Decode.succeed (\\d e -> { interval = d"
                  , "    , users = e }) |>"
                  , "    Json.Decode.andThen (\\d -> Json.Decode.map d (Json.Decode.succeed (\\e f -> { startInclusive = e"
                  , "    , endExclusive = f }) |>"
                  , "    Json.Decode.andThen (\\e -> Json.Decode.map e Iso8601.decoder) |>"
                  , "    Json.Decode.andThen (\\e -> Json.Decode.map e Iso8601.decoder))) |>"
                  , "    Json.Decode.andThen (\\d -> Json.Decode.map d (Json.Decode.list Json.Decode.string))))) |>"
                  , "    Json.Decode.andThen (\\c -> Json.Decode.map c Json.Decode.string) |>"
                  , "    Json.Decode.andThen (\\c -> Json.Decode.map c Json.Decode.string) |>"
                  , "    Json.Decode.andThen (\\c -> Json.Decode.map c (Json.Decode.list inviteDecoder)) |>"
                  , "    Json.Decode.andThen (\\c -> Json.Decode.map c Iso8601.decoder) |>"
                  , "    Json.Decode.andThen (\\c -> Json.Decode.map c (Json.Decode.list Json.Decode.string))))))) |>"
                  , "    Json.Decode.andThen (\\a -> Json.Decode.map a Json.Decode.int) |>"
                  , "    Json.Decode.andThen (\\a -> Json.Decode.map a Json.Decode.string)"
                  , ""
                  , ""
                  , "dashboardEncoder : Dashboard -> Json.Encode.Value"
                  , "dashboardEncoder a ="
                  , "    Json.Encode.object [ (\"proposals\" , Json.Encode.list (\\b -> Json.Encode.object [ (\"key\" , Json.Encode.string b.key)"
                  , "    , (\"value\" , (\\c -> Json.Encode.object [ (\"name\" , Json.Encode.string c.name)"
                  , "    , (\"owner\" , Json.Encode.string c.owner)"
                  , "    , (\"availability\" , Json.Encode.list (\\d -> Json.Encode.object [ (\"interval\" , (\\e -> Json.Encode.object [ (\"startInclusive\" , Iso8601.encode e.startInclusive)"
                  , "    , (\"endExclusive\" , Iso8601.encode e.endExclusive) ]) d.interval)"
                  , "    , (\"users\" , Json.Encode.list Json.Encode.string d.users) ]) c.availability)"
                  , "    , (\"description\" , Json.Encode.string c.description)"
                  , "    , (\"venue\" , Json.Encode.string c.venue)"
                  , "    , (\"invites\" , Json.Encode.list inviteEncoder c.invites)"
                  , "    , (\"created-at\" , Iso8601.encode c.created_at)"
                  , "    , (\"attachments\" , Json.Encode.list Json.Encode.string c.attachments) ]) b.value) ]) a.proposals)"
                  , "    , (\"credits\" , Json.Encode.int a.credits)"
                  , "    , (\"user\" , Json.Encode.string a.user) ]"
                  , ""
                  , ""
                  , "inviteDecoder : Json.Decode.Decoder Invite"
                  , "inviteDecoder ="
                  , "    Json.Decode.oneOf [ Json.Decode.map InviteUser (Json.Decode.succeed (\\a b -> { type_ = a"
                  , "    , username = b }) |>"
                  , "    Json.Decode.andThen (\\a -> Json.Decode.map a (Json.Decode.string |>"
                  , "    Json.Decode.andThen (\\b -> if b == \"discord-user\" then"
                  , "        Json.Decode.succeed ()"
                  , ""
                  , "    else"
                  , "        Json.Decode.fail \"Tag mismatch\"))) |>"
                  , "    Json.Decode.andThen (\\a -> Json.Decode.map a Json.Decode.string))"
                  , "    , Json.Decode.map InviteGuild (Json.Decode.succeed (\\a b -> { type_ = a"
                  , "    , guild = b }) |>"
                  , "    Json.Decode.andThen (\\a -> Json.Decode.map a (Json.Decode.string |>"
                  , "    Json.Decode.andThen (\\b -> if b == \"discord-server\" then"
                  , "        Json.Decode.succeed ()"
                  , ""
                  , "    else"
                  , "        Json.Decode.fail \"Tag mismatch\"))) |>"
                  , "    Json.Decode.andThen (\\a -> Json.Decode.map a (Json.Decode.succeed (\\b c -> { id = b"
                  , "    , name = c }) |>"
                  , "    Json.Decode.andThen (\\b -> Json.Decode.map b Json.Decode.string) |>"
                  , "    Json.Decode.andThen (\\b -> Json.Decode.map b Json.Decode.string)))) ]"
                  , ""
                  , ""
                  , "inviteEncoder : Invite -> Json.Encode.Value"
                  , "inviteEncoder a ="
                  , "    case a of"
                  , "        InviteUser b ->"
                  , "            (\\c -> Json.Encode.object [ (\"type\" , always (Json.Encode.string \"discord-user\") c.type_)"
                  , "            , (\"username\" , Json.Encode.string c.username) ]) b"
                  , ""
                  , "        InviteGuild b ->"
                  , "            (\\c -> Json.Encode.object [ (\"type\" , always (Json.Encode.string \"discord-server\") c.type_)"
                  , "            , (\"guild\" , (\\d -> Json.Encode.object [ (\"id\" , Json.Encode.string d.id)"
                  , "            , (\"name\" , Json.Encode.string d.name) ]) c.guild) ]) b"
                  , ""
                  , ""
                  , "type Invite "
                  , "    = InviteUser { type_ : (), username : String }"
                  , "    | InviteGuild { type_ : (), guild : { id : String, name : String } }"
                  , ""
                  , ""
                  , "type alias Dashboard  ="
                  , "    { proposals : List { key : String"
                  , "    , value : { name : String"
                  , "    , owner : String"
                  , "    , availability : List { interval : { startInclusive : Time.Posix"
                  , "    , endExclusive : Time.Posix }"
                  , "    , users : List String }"
                  , "    , description : String"
                  , "    , venue : String"
                  , "    , invites : List Invite"
                  , "    , created_at : Time.Posix"
                  , "    , attachments : List String } }"
                  , "    , credits : Int"
                  , "    , user : String }"
                  ]
              )
        in do
          TIO.hPutStrLn stderr "==========================================\n\n"
          TIO.hPutStrLn stderr (fromMaybe "" (HM.lookup ["Api", "Data"] actual))
          TIO.hPutStrLn stderr "\n\n==========================================\n\n"
          actual `shouldBe` expected


{-
  This spec is copied from an as-yet uncompleted personal project. I
  just used it because it is fairly complex. Probably something known
  to be exhaustive is in order.
-}
type TestSpec =
  JsonLet
    '[
      '("Dashboard", JsonObject '[
        '("proposals", JsonArray (
          JsonObject '[
            '("key", JsonString),
            '("value", JsonObject '[
              '("name", JsonString),
              '("owner", JsonString),
              '("availability", JsonArray (
                JsonObject '[
                  '("interval", JsonObject '[
                    '("startInclusive", JsonDateTime),
                    '("endExclusive", JsonDateTime)
                  ]),
                  '("users", JsonArray JsonString)
                ]
              )),
              '("description", JsonString),
              '("venue", JsonString),
              '("invites", (
                JsonLet '[
                  '("Invite",
                    JsonEither
                      (Named "InviteUser" (JsonObject '[
                        '("type", JsonTag "discord-user"),
                        '("username", JsonString)
                      ]))
                      (Named "InviteGuild" (JsonObject '[
                        '("type", JsonTag "discord-server"),
                        '("guild", JsonObject '[
                          '("id", JsonString),
                          '("name", JsonString)
                         ])
                      ]))
                  )
                ]
                (JsonArray (JsonRef "Invite")))),
              '("created-at", JsonDateTime),
              '("attachments", JsonArray JsonString)
            ])
          ]
        )),
        '("credits", JsonInt),
        '("user", JsonString)
       ])
    ] ( JsonRef "Dashboard")

