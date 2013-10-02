{-# LANGUAGE TemplateHaskell #-}

module Graphics.HsPlot.TH (
  deriveFromField, enumToField, nameToField
)
where

import Control.Applicative (pure)
import Control.Monad (mzero)
import Data.List (elemIndex)
import Language.Haskell.TH
import System.IO.Unsafe

import Data.Csv (Field, FromField, Parser, parseField)

import qualified Data.ByteString.Char8 as BS8 (pack)

enumToField :: Enum a => [Field] -> Field -> Parser a
enumToField n s = case elemIndex s n of
                    Just x -> pure $ toEnum x
                    Nothing -> mzero

nameToField :: Name -> Exp
nameToField n = AppE (VarE 'BS8.pack) (LitE (StringL $ nameBase n))

data T1

deriveFromField :: Name -> Q [Dec]
deriveFromField t = do
  TyConI (DataD _ _ _ cs _) <- reify t
  let names = ListE $ map (\(NormalC n _) -> nameToField n) cs
      etf = AppE (VarE 'enumToField) names
  d <- [d| instance FromField T1 where
             parseField = enumToField []
       |]
  let [InstanceD [] (AppT s (ConT _)) [ValD p _ []]] = d
  return [InstanceD [] (AppT s (ConT t)) [ValD p (NormalB etf) []]]
