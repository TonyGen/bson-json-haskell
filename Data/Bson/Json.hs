-- | ToJSON and FromJSON instances for BSON Documents and Values

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Data.Bson.Json where

import qualified Data.Bson as B
import qualified Data.Aeson as J
import qualified Data.Attoparsec.Number as J (Number(..))
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UString as U
import qualified Data.ByteString.Char8 as S
import Control.Monad
import Control.Applicative

instance J.ToJSON B.Document where
	toJSON fields = J.object $ map toJSONPair fields
		where toJSONPair (label B.:= value) = (T.decodeUtf8 $ U.toByteString label, J.toJSON value)

instance J.FromJSON B.Document where
	parseJSON (J.Object dict) = mapM parseJSONPair $ M.toList dict where
		parseJSONPair (label, value) = fmap (U.fromByteString_ (T.encodeUtf8 label) B.:=) $ J.parseJSON value
	parseJSON x = fail $ "BSON document expects to be deriving from a JSON Object, not " ++ show x

instance J.ToJSON B.Value where
	toJSON v = case v of
		B.Float double -> J.Number $ J.D double
		B.String string -> J.String $ T.decodeUtf8 $ U.toByteString string
		B.Doc document -> J.Object obj where J.Object obj = J.toJSON document
		B.Array values -> J.Array $ V.fromList $ map J.toJSON values
		B.Bool bool -> J.Bool bool
		B.Null -> J.Null
		B.Int32 int -> J.Number $ J.I $ fromIntegral int
		B.Int64 int -> J.Number $ J.I $ fromIntegral int
		B.Bin binary -> J.toJSON binary
		B.Fun function -> J.toJSON function
		B.Uuid uuid -> J.toJSON uuid
		B.Md5 md5 -> J.toJSON md5
		B.UserDef userDefined -> J.toJSON userDefined
		B.ObjId objectId -> J.toJSON objectId
		B.UTC utcTime -> J.toJSON utcTime
		B.RegEx regex -> J.toJSON regex
		B.JavaScr javascript -> J.toJSON javascript
		B.Sym symbol -> J.toJSON symbol
		B.Stamp mongoStamp -> J.toJSON mongoStamp
		B.MinMax minMaxKey -> J.toJSON minMaxKey

instance J.FromJSON B.Value where
	parseJSON v = case v of
		J.Object _ -> msum [
			B.Bin <$> J.parseJSON v,
			B.Fun <$> J.parseJSON v,
			B.Uuid <$> J.parseJSON v,
			B.Md5 <$> J.parseJSON v,
			B.UserDef <$> J.parseJSON v,
			B.ObjId <$> J.parseJSON v,
			B.RegEx <$> J.parseJSON v,
			B.JavaScr <$> J.parseJSON v,
			B.Sym <$> J.parseJSON v,
			B.Stamp <$> J.parseJSON v,
			B.MinMax <$> J.parseJSON v,
			B.Doc <$> J.parseJSON v ]
		J.Array vec -> B.Array <$> mapM J.parseJSON (V.toList vec)
		J.String text -> pure $ case J.fromJSON v of
			J.Success utcTime -> B.UTC utcTime
			J.Error _ -> B.String $ U.fromByteString_ $ T.encodeUtf8 text
		J.Number num -> pure $ case num of
			J.I int -> B.val int
			J.D double -> B.Float double
		J.Bool bool -> pure $ B.Bool bool
		J.Null -> pure B.Null

instance J.ToJSON B.Binary where
	toJSON (B.Binary byteString) = J.object [("#_BSON_Binary", J.String $ T.pack $ S.unpack byteString)]

instance J.FromJSON B.Binary where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_Binary", J.String text)] <- return $ M.toList dict
			return $ B.Binary $ S.pack $ T.unpack text
		fail' = fail $ "BSON Binary expects object with special field #_BSON_Binary, not " ++ show v

instance J.ToJSON B.Function where
	toJSON (B.Function byteString) = J.object [("#_BSON_Function", J.String $ T.pack $ S.unpack byteString)]

instance J.FromJSON B.Function where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_Function", J.String text)] <- return $ M.toList dict
			return $ B.Function $ S.pack $ T.unpack text
		fail' = fail $ "BSON Function expects object with special field #_BSON_Function, not " ++ show v

instance J.ToJSON B.UUID where
	toJSON (B.UUID byteString) = J.object [("#_BSON_UUID", J.String $ T.pack $ S.unpack byteString)]

instance J.FromJSON B.UUID where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_UUID", J.String text)] <- return $ M.toList dict
			return $ B.UUID $ S.pack $ T.unpack text
		fail' = fail $ "BSON UUID expects object with special field #_BSON_UUID, not " ++ show v

instance J.ToJSON B.MD5 where
	toJSON (B.MD5 byteString) = J.object [("#_BSON_MD5", J.String $ T.pack $ S.unpack byteString)]

instance J.FromJSON B.MD5 where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_MD5", J.String text)] <- return $ M.toList dict
			return $ B.MD5 $ S.pack $ T.unpack text
		fail' = fail $ "BSON MD5 expects object with special field #_BSON_MD5, not " ++ show v

instance J.ToJSON B.UserDefined where
	toJSON (B.UserDefined byteString) = J.object [("#_BSON_UserDefined", J.String $ T.pack $ S.unpack byteString)]

instance J.FromJSON B.UserDefined where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_UserDefined", J.String text)] <- return $ M.toList dict
			return $ B.UserDefined $ S.pack $ T.unpack text
		fail' = fail $ "BSON UserDefined expects object with special field #_BSON_UserDefined, not " ++ show v

instance J.ToJSON B.Regex where
	toJSON (B.Regex pattern options) = J.object [("#_BSON_Regex", J.object [
		("pattern", J.String $ T.decodeUtf8 $ U.toByteString pattern),
		("options", J.String $ T.decodeUtf8 $ U.toByteString options)] )]

instance J.FromJSON B.Regex where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_Regex", J.Object dict2)] <- return $ M.toList dict
			[("pattern", J.String pattern), ("options", J.String options)] <- return $ M.toList dict2
			return $ B.Regex (U.fromByteString_ $ T.encodeUtf8 pattern) (U.fromByteString_ $ T.encodeUtf8 options)
		fail' = fail $ "BSON Regex expects object with special field #_BSON_Regex, not " ++ show v

instance J.ToJSON B.Javascript where
	toJSON (B.Javascript environment code) = J.object [("#_BSON_Javascript", J.object [
		("environment", J.toJSON environment),
		("code", J.String $ T.decodeUtf8 $ U.toByteString code)] )]

instance J.FromJSON B.Javascript where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_Javascript", J.Object dict2)] <- return $ M.toList dict
			[("environment", J.Object env), ("code", J.String code)] <- return $ M.toList dict2
			flip B.Javascript (U.fromByteString_ $ T.encodeUtf8 code) <$> J.parseJSON (J.Object env)
		fail' = fail $ "BSON Javascript expects object with special field #_BSON_Javascript, not " ++ show v

instance J.ToJSON B.Symbol where
	toJSON (B.Symbol string) = J.object [("#_BSON_Symbol", J.String $ T.decodeUtf8 $ U.toByteString string)]

instance J.FromJSON B.Symbol where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_MD5", J.String text)] <- return $ M.toList dict
			return $ B.Symbol $ U.fromByteString_ $ T.encodeUtf8 text
		fail' = fail $ "BSON Symbol expects object with special field #_BSON_Symbol, not " ++ show v

instance J.ToJSON B.MongoStamp where
	toJSON (B.MongoStamp int) = J.object [("#_BSON_MongoStamp", J.Number $ J.I $ fromIntegral int)]

instance J.FromJSON B.MongoStamp where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_MongoStamp", J.Number (J.I int))] <- return $ M.toList dict
			return $ B.MongoStamp $ fromIntegral int
		fail' = fail $ "BSON MongoStamp expects object with special field #_BSON_MongoStamp, not " ++ show v

instance J.ToJSON B.MinMaxKey where
	toJSON x = J.object [("#_BSON_MinMaxKey", J.String $ case x of B.MinKey -> "MinKey"; B.MaxKey -> "MaxKey")]

instance J.FromJSON B.MinMaxKey where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_MinMaxKey", J.String text)] <- return $ M.toList dict
			case text of "MinKey" -> return B.MinKey; "MaxKey" -> return B.MaxKey; _ -> mzero
		fail' = fail $ "BSON MinMaxKey expects object with special field #_BSON_MinMaxKey, not " ++ show v

instance J.ToJSON B.ObjectId where
	toJSON x = J.object [("#_BSON_ObjectId", J.String $ T.pack $ show x)]

instance J.FromJSON B.ObjectId where
	parseJSON v = go `mplus` fail' where
		go = do
			J.Object dict <- return v
			[("#_BSON_ObjectId", J.String text)] <- return $ M.toList dict
			return $ read $ T.unpack text
		fail' = fail $ "BSON ObjectId expects object with special field #_BSON_ObjectId, not " ++ show v
