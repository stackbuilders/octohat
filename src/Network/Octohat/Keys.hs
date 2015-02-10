{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.Keys (fingerprintFor) where

import qualified Data.List as L
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Crypto.Hash.MD5 as MD5 -- eww

import Network.Octohat.Types

type RSAPublicKey = T.Text
type Fingerprint  = T.Text

fingerprintFor :: PublicKey -> PublicKeyFingerprint
fingerprintFor PublicKey{..} = PublicKeyFingerprint publicKeyId (digestToHex publicKey)

digestToHex :: RSAPublicKey -> Fingerprint
digestToHex = T.concat          .
              L.intersperse ":" .
              T.chunksOf 2      .
              T.toLower         .
              TE.decodeUtf8     .
              B16.encode        .
              MD5.hash          .
              B64.decodeLenient .
              TE.encodeUtf8     .
              T.replace "ssh-rsa " ""
