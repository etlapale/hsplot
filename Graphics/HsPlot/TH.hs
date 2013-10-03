{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Graphics.HsPlot.TH (
  deriveFromField, deriveFromRecordOffset, enumToField, nameToField
)
where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Data.List (elemIndex)
import Language.Haskell.TH
import System.IO.Unsafe

import qualified Data.Vector as V

import Data.Csv ((.!), Field, FromField, FromRecord, Parser, parseField, parseRecord)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

enumToField :: Enum a => [Field] -> Field -> Parser a
enumToField ns n =
  case (BS8.filter notSpace n) `elemIndex` ns of
    Just x -> pure $ toEnum x
    Nothing -> mzero
  where notSpace ' ' = False
        notSpace _   = True

nameToField :: Name -> Exp
nameToField n = AppE (VarE 'BS8.pack) (LitE (StringL $ nameBase n))

deriveFromField :: Name -> Q [Dec]
deriveFromField t = do
  TyConI (DataD _ _ _ cs _) <- reify t
  let clauses = map makeClause cs ++ [wildClause]
  return [InstanceD [] (AppT (ConT ''FromField) (ConT t))
                        [FunD 'parseField clauses]]
  where makeClause (NormalC n _) =
          Clause [LitP (StringL $ nameBase n)]
                 (NormalB (AppE (VarE 'pure) (ConE n)))
                 []
        wildClause = Clause [WildP] (NormalB (VarE 'mzero)) []

deriveFromRecordOffset :: Name -> Integer -> Q [Dec]
deriveFromRecordOffset t o = do
  TyConI (DataD _ _ _ [RecC t' fs] _) <- reify t
  -- Make sure every field is an instance of FromField
  ffs <- mapM ensureFromField fs
  -- Create the FromRecord instance
  v <- newName "v"
  let recLen = fromIntegral $ length fs
  let infixE = buildInfix t' v (recLen - o)
      d = InstanceD [] (AppT (ConT ''FromRecord) (ConT t))
                       [FunD 'parseRecord [Clause [VarP v] (GuardedB [normalGuard, badLengthGuard]) []]]
      normalGuard = (NormalG (InfixE (Just (AppE (VarE 'V.length) (VarE v))) (VarE '(==)) (Just (LitE (IntegerL $ recLen + o)))), infixE)
      badLengthGuard = (NormalG (VarE 'otherwise), VarE 'mzero)
  return $ concat $ ffs ++ [[d]]
  where ensureFromField (_,_,ConT n) = do
          isinst <- isInstance ''FromField [ConT n]
          if isinst
          then return []
          else deriveFromField n
        ensureFromField _ = return []  -- Not yet supported
        buildInfix t v 0 = InfixE (Just (ConE t)) (VarE '(<$>)) $ buildJust v 0
        buildInfix t v i = InfixE (Just $ buildInfix t v (i-1)) (VarE '(<*>)) $ buildJust v i
        buildJust v i  = Just (InfixE (Just (VarE v))
                                      (VarE '(.!))
                                      (Just (LitE (IntegerL $ i + o))))
