{-# LANGUAGE TemplateHaskell #-}

module Jijo.RecordField.TH
  ( module Jijo.RecordField,
    makeRecBuilder
  ) where

import Data.List (foldl')
import qualified Language.Haskell.TH as TH
import qualified Data.Char as Char
import Data.Coerce
import Jijo.RecordField

makeRecBuilder :: TH.Name -> TH.DecsQ
makeRecBuilder tyName = do
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
    prefixStr = toPrefix (TH.nameBase tyName)
    prefixLen = length prefixStr
    builderName = TH.mkName ("rec" ++ TH.nameBase conName)
    mkFieldArg (fieldName, _, fieldTy) =
        TH.ConT ''Field `TH.AppT`
        TH.LitT (TH.StrTyLit prefixStr) `TH.AppT`
        TH.LitT (TH.StrTyLit nameStr) `TH.AppT`
        fieldTy
      where
        -- TODO: check prefix validity
        nameStr = drop prefixLen (TH.nameBase fieldName)
    mkConArg (_, _, fieldTy) = fieldTy
    argFunTy vbt r = TH.ArrowT `TH.AppT` mkFieldArg vbt `TH.AppT` r
    conFunTy vbt r = TH.ArrowT `TH.AppT` mkConArg vbt `TH.AppT` r
    builderTy :: TH.Type
    builderTy = TH.ForallT dTyVarBndrs dCxt (foldr argFunTy ty vbts)
    builderExp :: TH.Exp
    builderExp = TH.AppE (TH.VarE 'coerce) (TH.SigE (TH.ConE conName) (foldr conFunTy ty vbts))
  return
    [ TH.SigD builderName builderTy,
      TH.FunD builderName [TH.Clause [] (TH.NormalB builderExp) []] ]

toPrefix :: String -> String
toPrefix (x:xs) = '_' : Char.toLower x : xs
toPrefix [] = []
