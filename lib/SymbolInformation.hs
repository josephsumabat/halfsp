{-# OPTIONS_GHC -Wno-deprecations #-}

module SymbolInformation where

import Data.Text (Text)
import Language.LSP.Types (List, Location, SymbolInformation (..), SymbolKind, SymbolTag)

mkSymbolInformation ::
  Text ->
  SymbolKind ->
  Maybe (List SymbolTag) ->
  Location ->
  Maybe Text ->
  SymbolInformation
mkSymbolInformation name kind tags location containerName =
  SymbolInformation
    { _name = name,
      _kind = kind,
      _tags = tags,
      _deprecated = Nothing,
      _location = location,
      _containerName = containerName
    }
