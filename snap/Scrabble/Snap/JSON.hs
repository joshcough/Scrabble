{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-
Copyright (c)2012, Ozgun Ataman

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Ozgun Ataman nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

-- | This code is taken from https://hackage.haskell.org/package/snap-extras-0.1.7
--   due to issues getting stack to compile it. Hopefully this can eventually be pulled out.
module Scrabble.Snap.JSON where

import           Control.Monad
import           Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import           Data.ByteString.Lazy.Char8 (fromChunks)
import           Data.Int
import           Safe
import           Snap.Core

-- | Demand the presence of JSON in the body assuming it is not larger
-- than 50000 bytes.
reqJSON :: (MonadSnap m, A.FromJSON b) => m b
reqJSON = reqBoundedJSON 50000

-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body with a size up to N
-- bytes. If parsing fails for any reson, request is terminated early
-- and a server error is returned.
reqBoundedJSON
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m a
reqBoundedJSON n = do
  res <- getBoundedJSON n
  case res of
    Left e -> badReq $ B.pack e
    Right a -> return a

-------------------------------------------------------------------------------
-- | Try to parse request body as JSON with a default max size of
-- 50000.
getJSON :: (MonadSnap m, A.FromJSON a) => m (Either String a)
getJSON = getBoundedJSON 50000

-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- A.decode `fmap` readRequestBody n
  return $ case bodyVal of
    Nothing -> Left "Can't find JSON data in POST body"
    Just v -> case A.fromJSON v of
                A.Error e -> Left e
                A.Success a -> Right a

-------------------------------------------------------------------------------
-- | Get JSON data from the given Param field
getJSONField
    :: (MonadSnap m, FromJSON a)
    => B.ByteString
    -> m (Either String a)
getJSONField fld = do
  val <- getParam fld
  return $ case val of
    Nothing -> Left $ "Cant find field " ++ B.unpack fld
    Just val' ->
      case A.decode (fromChunks . return $ val') of
        Nothing -> Left $ "Can't decode JSON data in field " ++ B.unpack fld
        Just v ->
          case A.fromJSON v of
            A.Error e -> Left e
            A.Success a -> Right a

-------------------------------------------------------------------------------
-- | Force the JSON value from field. Similar to 'getJSONField'
reqJSONField
    :: (MonadSnap m, FromJSON a)
    => B.ByteString
    -> m a
reqJSONField fld = do
  res <- getJSONField fld
  case res of
    Left e -> badReq $ B.pack e
    Right a -> return a

-------------------------------------------------------------------------------
-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . encode $ a

-------------------------------------------------------------------------------
-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> B.ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith

-------------------------------------------------------------------------------
-- | Finish early with error code 400
badReq :: MonadSnap m => B.ByteString -> m b
badReq = finishEarly 400

-------------------------------------------------------------------------------
-- | Finish early with error code 404
notFound :: MonadSnap m => B.ByteString -> m b
notFound = finishEarly 404

-------------------------------------------------------------------------------
-- | Finish early with error code 500
serverError :: MonadSnap m => B.ByteString -> m b
serverError = finishEarly 500

-------------------------------------------------------------------------------
-- | Mark response as 'text/plain'
plainResponse :: MonadSnap m => m ()
plainResponse = modifyResponse $ setHeader "Content-Type" "text/plain"

-------------------------------------------------------------------------------
-- | Mark response as 'application/json'
jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"

-------------------------------------------------------------------------------
-- | Mark response as 'application/javascript'
jsResponse :: MonadSnap m => m ()
jsResponse = modifyResponse $ setHeader "Content-Type" "application/javascript"

------------------------------------------------------------------------------
-- | Easier debug logging into error log. First argument is a
-- category/namespace and the second argument is anything that has a
-- 'Show' instance.
easyLog :: (Show t, MonadSnap m) => String -> t -> m ()
easyLog k v = logError . B.pack $ ("[Debug] " ++ k ++ ": " ++ show v)

-------------------------------------------------------------------------------
-- | Alternate version of getParam that considers empty string Nothing
getParam' :: MonadSnap m => B.ByteString -> m (Maybe B.ByteString)
getParam' = return . maybe Nothing f <=< getParam
    where f "" = Nothing
          f x = Just x

-------------------------------------------------------------------------------
-- | Require that a parameter is present or terminate early.
reqParam :: (MonadSnap m) => B.ByteString -> m B.ByteString
reqParam s = do
  p <- getParam s
  maybe (badReq $ B.concat ["Required parameter ", s, " is missing."]) return p

-------------------------------------------------------------------------------
-- | Read a parameter from request. Be sure it is readable if it's
-- there, or else this will raise an error.
readParam :: (MonadSnap m, Read a) => B.ByteString -> m (Maybe a)
readParam k = fmap (readNote "readParam failed" . B.unpack) `fmap` getParam k

-------------------------------------------------------------------------------
-- | Try to read a parameter from request. Computation may fail
-- because the param is not there, or because it can't be read.
readMayParam :: (MonadSnap m, Read a) => B.ByteString -> m (Maybe a)
readMayParam k = do
  p <- getParam k
  return $ readMay . B.unpack =<< p
