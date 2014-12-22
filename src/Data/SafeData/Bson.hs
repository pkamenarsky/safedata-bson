{-# LANGUAGE OverloadedStrings #-}

module Data.SafeData.Bson where

import qualified Data.Bson            as B
import           Data.Bson            ((=:))
import qualified Data.SafeData        as S
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Data.Word

instance B.Val Word8 where
  val                = B.Float . fromInteger . toInteger
  cast' (B.Float v)  = Just $ fromInteger $ round v
  cast' _            = Nothing

instance B.Val S.Value where
  val (S.BValue v)   = B.Bool v
  val (S.CValue v)   = B.String $ T.singleton v
  val (S.SValue v)   = B.String $ T.pack v
  val (S.DValue v)   = B.Float v
  val (S.FValue v)   = B.Float $ realToFrac v
  val (S.IValue v)   = B.Float $ fromInteger $ toInteger v
  val (S.I8Value v)  = B.Float $ fromInteger $ toInteger v
  val (S.I16Value v) = B.Float $ fromInteger $ toInteger v
  val (S.I32Value v) = B.Float $ fromInteger $ toInteger v
  val (S.I64Value v) = B.Float $ fromInteger $ toInteger v
  val (S.BIValue v)  = undefined
  val (S.OrdValue v) = undefined
  val (S.WValue v)   = undefined
  val (S.W8Value v)  = undefined
  val (S.W16Value v) = undefined
  val (S.W32Value v) = undefined
  val (S.W64Value v) = undefined
  val (S.UValue v)   = undefined
  val (S.BSValue v)  = undefined
  val (S.BSLValue v) = undefined

  val (S.Array v)    = B.Array $ map B.val v

  val (S.Object ty cstr version vs)
                     = B.Doc $ [ "_type"    =: ty
                               , "_cstr"    =: cstr
                               , "_version" =: version
                               ]
                               ++ map (\(k, v) -> T.pack k =: v) vs

  cast' (B.Bool v) = Just $ S.BValue v
