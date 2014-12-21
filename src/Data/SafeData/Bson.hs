{-# LANGUAGE OverloadedStrings #-}

module Data.SafeData.Bson
  ( toValue
  , toDocument
  ) where

import qualified Data.Bson            as B
import           Data.Bson            (Field ((:=)), (=:))
import qualified Data.HashMap.Strict  as H
import qualified Data.SafeCopy        as S
import qualified Data.Scientific      as SF
import qualified Data.Text            as T
import qualified Data.Vector          as V

toValue :: S.Value -> B.Value
toValue (S.BValue v)   = B.Bool v
toValue (S.CValue v)   = B.String $ T.singleton v
toValue (S.SValue v)   = B.String $ T.pack v
toValue (S.DValue v)   = B.Float v
toValue (S.FValue v)   = B.Float $ realToFrac v
toValue (S.IValue v)   = B.Float $ fromInteger $ toInteger v
toValue (S.I8Value v)  = B.Float $ fromInteger $ toInteger v
toValue (S.I16Value v) = B.Float $ fromInteger $ toInteger v
toValue (S.I32Value v) = B.Float $ fromInteger $ toInteger v
toValue (S.I64Value v) = B.Float $ fromInteger $ toInteger v
toValue (S.BIValue v)  = undefined
toValue (S.OrdValue v) = undefined
toValue (S.WValue v)   = undefined
toValue (S.W8Value v)  = undefined
toValue (S.W16Value v) = undefined
toValue (S.W32Value v) = undefined
toValue (S.W64Value v) = undefined
toValue (S.UValue v)   = undefined
toValue (S.BSValue v)  = undefined
toValue (S.BSLValue v) = undefined

toValue (S.Array v)    = B.Array $ map toValue v

toDocument (S.Object ty cstr version vs)
                      = [ "_type"    := B.String (T.pack ty)
                        , "_cstr"    := B.Float  (fromInteger $ toInteger cstr)
                        , "_version" := B.Float  (fromInteger $ toInteger version)
                        ]
                        ++ map (\(k, v) -> T.pack k := toValue v) vs
toDocument _ = error "Expected an Object"
