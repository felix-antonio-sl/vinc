{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_vinc_chatbot (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "vinc_chatbot"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Chatbot informativo para VINC 8% 2024"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
