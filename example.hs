{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import Data.ProtoBlob
import ProtoDB.Writer
import ProtoDB.Types

main = BL.writeFile "dbs/sin.db" $ Data.ProtoBlob.runPutBlob $ ProtoDB.Writer.writeDB $ ProtoDB.Writer.WritableDB "y = sin(x) Sample" [WritableField "Point Description" ProtoDB.Types.ProtoStringType  [], WritableField "x" ProtoDB.Types.ProtoRealType  [], WritableField "y" ProtoDB.Types.ProtoRealType  []] $ concat [[ProtoStringCell $ ProtoString Nothing, ProtoRealCell $ ProtoReal $ Just x, ProtoRealCell $ ProtoReal $ Just $ sin x] |  x <- [0.01,0.02..7.0] ]

