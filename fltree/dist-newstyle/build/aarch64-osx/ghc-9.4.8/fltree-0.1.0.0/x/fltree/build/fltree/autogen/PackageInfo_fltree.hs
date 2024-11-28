{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_fltree (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "fltree"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Pretty-printing n-ary rooted trees"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
