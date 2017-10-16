{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.UploadImage(uploadimage) where

import qualified Crypto.Hash.MD5 as MD5
import           Data.Aeson hiding (encode)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8 (lines, unlines, writeFile)
import           Data.HVect
import           Prelude (init)
import           Protolude hiding (writeFile)
import           System.Directory (createDirectoryIfMissing)
import           Web.Spock

import           Database.Persist
import           DatabaseTypes
import           DefaultResponses
import           ServerTypes
import           Types
import           Utilities

uploadimage :: ListContains n (Entity User) xs => ApiAction (HVect xs) ()
uploadimage = do
  request <- unlines . init . drop 4 . lines <$> body
  let name = toS . encode $ MD5.hash request
  let folder = (<>) "images/" . intersperse '/' . take 6 $ name
  let path = folder <> "/" <> name <> ".jpg"
  liftIO $ createDirectoryIfMissing True ("static/" <> folder)
  liftIO $ writeFile ("static/" <> path) request
  text . toS $ path
