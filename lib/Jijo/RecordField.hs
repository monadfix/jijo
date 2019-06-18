{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Jijo.RecordField where

import GHC.TypeLits

newtype Field (prefix :: Symbol) (name :: Symbol) ty = Field ty
