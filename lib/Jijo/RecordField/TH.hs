{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell utilities for 'Field'.
module Jijo.RecordField.TH
  ( module Jijo.RecordField,
    makeRecBuilder
  ) where

import Data.List (foldl')
import Control.Monad (when)
import qualified Language.Haskell.TH as TH
import Data.Coerce
import Jijo.RecordField

-- | Generate a smart constructor for a record that expects every field to be
-- wrapped in the 'Field' newtype.
--
-- Consider this record:
-- @
-- data User =
--   MkUser { _userId :: UUID,
--            _userName :: Text,
--            _userAddr :: Text
--          }
-- @
--
-- @'makeRecBuilder' \"_user\" ''User@ generates the following function:
--
-- @
-- recUser ::
--   'Field' \"_user\" \"Id\"   UUID ->
--   'Field' \"_user\" \"Name\" Text ->
--   'Field' \"_user\" \"Addr\" Text ->
--   User
-- recUser = 'coerce' MkUser
-- @
makeRecBuilder :: String -> TH.Name -> TH.DecsQ
makeRecBuilder prefixStr tyName = do
  info <- TH.reify tyName
  dec <- case info of
    TH.TyConI dec -> pure dec
    _ -> fail "Not a type declaration"
  (dCon, dCxt, dTyVarBndrs) <- case dec of
    TH.DataD cxt _ tyVarBndrs _ [con] _ -> pure (con, cxt, tyVarBndrs)
    TH.NewtypeD cxt _ tyVarBndrs _ con _ -> pure (con, cxt, tyVarBndrs)
    _ -> fail "Not a single-constructor type"
  let
    tyParam (TH.PlainTV tvName)         = TH.VarT tvName
    tyParam (TH.KindedTV tvName tvKind) = TH.VarT tvName `TH.SigT` tvKind
    ty = foldl' TH.AppT (TH.ConT tyName) (map tyParam dTyVarBndrs)
  (conName, vbts) <- case dCon of
    TH.RecC conName vbts -> pure (conName, vbts)
    _ -> fail "Not a record type"
  let
    prefixLen = length prefixStr
    mkField (fieldName, _, fieldTy) = do
      let
        fieldNameStr = TH.nameBase fieldName
        (prefixStr', strippedFieldNameStr) = splitAt prefixLen fieldNameStr
      when (prefixStr' /= prefixStr) $
        TH.reportError $
          "Field name " ++ fieldNameStr ++
          " does not have the expected prefix " ++ prefixStr
      return (strippedFieldNameStr, fieldTy)
  fields <- traverse mkField vbts
  let
    builderName = TH.mkName ("rec" ++ TH.nameBase conName)
    mkFieldArg (fieldNameStr, fieldTy) =
        TH.ConT ''Field `TH.AppT`
        TH.LitT (TH.StrTyLit prefixStr) `TH.AppT`
        TH.LitT (TH.StrTyLit fieldNameStr) `TH.AppT`
        fieldTy
    mkConArg (_, fieldTy) = fieldTy
    argFunTy vbt r = TH.ArrowT `TH.AppT` mkFieldArg vbt `TH.AppT` r
    conFunTy vbt r = TH.ArrowT `TH.AppT` mkConArg vbt `TH.AppT` r
    builderTy :: TH.Type
    builderTy = TH.ForallT dTyVarBndrs dCxt (foldr argFunTy ty fields)
    builderExp :: TH.Exp
    builderExp = TH.AppE (TH.VarE 'coerce) (TH.SigE (TH.ConE conName) (foldr conFunTy ty fields))
  return
    [ TH.SigD builderName builderTy,
      TH.FunD builderName [TH.Clause [] (TH.NormalB builderExp) []] ]
