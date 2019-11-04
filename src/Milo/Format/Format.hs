module Milo.Format.Format (docToString, displayJson) where

import Data.Aeson                             (Value)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import Data.Aeson.Encode.Pretty               as Pretty

displayJson :: Value -> String
displayJson = T.unpack . T.decodeUtf8 . LBS.toStrict . Pretty.encodePretty' (Pretty.defConfig { Pretty.confIndent = Pretty.Spaces 2 })

docToString :: ANSI.Doc -> String
docToString doc = ANSI.displayS (ANSI.renderPretty 0.4 80 doc) ""
  