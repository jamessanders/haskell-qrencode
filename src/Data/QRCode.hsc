{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- | Haskell bindings for libqrencode. <http://fukuchi.org/works/qrencode/index.en.html>
--
--   Libqrencode is a C library for encoding data in a QR Code symbol, a kind of 2D symbology 
--   that can be scanned by handy terminals such as a mobile phone with CCD. The capacity of 
--   QR Code is up to 7000 digits or 4000 characters, and is highly robust.

module Data.QRCode (encodeByteString, 
                    encodeString,
                    getQRCodeVersion,
                    getQRCodeWidth,
                    getQRCodeString,
                    toMatrix,
                    QREncodeLevel (..), 
                    QREncodeMode (..)) where

import Control.Monad
import Data.ByteString (ByteString, unpack, useAsCString, packCString)
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

#include <qrencode.h>

data QREncodeLevel = QR_ECLEVEL_L 
                   | QR_ECLEVEL_M 
                   | QR_ECLEVEL_Q 
                   | QR_ECLEVEL_H

data QREncodeMode  = QR_MODE_NUM        -- ^ Numeric mode
                   | QR_MODE_AN         -- ^ Alphabet-numeric mode
                   | QR_MODE_EIGHT      -- ^ 8-bit data mode
                   | QR_MODE_KANJI      -- ^ Kanji (shift-jis) mode

convertQREncodeLevel :: QREncodeLevel -> CInt
convertQREncodeLevel QR_ECLEVEL_L = #const QR_ECLEVEL_L
convertQREncodeLevel QR_ECLEVEL_M = #const QR_ECLEVEL_M
convertQREncodeLevel QR_ECLEVEL_Q = #const QR_ECLEVEL_Q
convertQREncodeLevel QR_ECLEVEL_H = #const QR_ECLEVEL_H

convertQREncodeMode :: QREncodeMode -> CInt
convertQREncodeMode QR_MODE_NUM       = #const QR_MODE_NUM   
convertQREncodeMode QR_MODE_AN        = #const QR_MODE_AN    
convertQREncodeMode QR_MODE_EIGHT     = #const QR_MODE_8     
convertQREncodeMode QR_MODE_KANJI     = #const QR_MODE_KANJI 

data QRcode = QRcode { 
      getQRCodeVersion :: Int,
      getQRCodeWidth   :: Int,
      getQRCodeString  :: ByteString
    } deriving (Show, Read)

data QRcodeStruct = QRcodeStruct {
      c_version :: CInt,
      c_width   :: CInt,
      c_data    :: CString
    } deriving (Show)

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable QRcodeStruct where

    alignment _ = #{alignment QRcode}

    sizeOf _ = #{size QRcode}

    peek ptr = do
      version <- #{peek QRcode, version} ptr
      width   <- #{peek QRcode, width} ptr
      data'   <- #{peek QRcode, data} ptr
      return $ QRcodeStruct version width data'

    poke ptr (QRcodeStruct version width data') = do
      #{poke QRcode, version} ptr version
      #{poke QRcode, width} ptr width
      #{poke QRcode, data} ptr data'


foreign import ccall unsafe "QRcode_encodeString" 
    c_encodeString :: CString -- string
                   -> CInt    -- version
                   -> CInt    -- level
                   -> CInt    -- hint
                   -> CInt    -- casesensitive
                   -> IO (Ptr QRcodeStruct)

-- | create a QR code from a ByteString
encodeByteString :: ByteString    -- ^ String to encode
                 -> Maybe Int     -- ^ Version (auto if Nothing)
                 -> QREncodeLevel -- ^ Encode Level
                 -> QREncodeMode  -- ^ Encode Mode
                 -> Bool          -- ^ Case-sensative
                 -> IO QRcode     
encodeByteString str version level mode casesensitive = 
    useAsCString str $ \s-> encoder s version level mode casesensitive

-- | create a QR code from a String
encodeString :: String        -- ^ String to encode
             -> Maybe Int     -- ^ Version (auto if Nothing)
             -> QREncodeLevel -- ^ Encode Level
             -> QREncodeMode  -- ^ Encode Mode
             -> Bool          -- ^ Case-sensative
             -> IO QRcode
encodeString str version  level mode casesensitive = 
    newCAString str >>= \s-> encoder s version level mode casesensitive
    
encoder :: CString -> Maybe Int -> QREncodeLevel -> QREncodeMode -> Bool -> IO QRcode
encoder cstr ver level mode casesensitive = do
  let l = convertQREncodeLevel level
  let m = convertQREncodeMode mode
  c_qr <- join $ fmap peek $ c_encodeString cstr (fromIntegral $ fromMaybe 0 ver) l m (b2i casesensitive) 
  let version = fromIntegral (c_version c_qr)                           
  let width   = fromIntegral (c_width   c_qr)                           
  str <- packCString (c_data c_qr) 
  return (QRcode version width str)                                     
  where
    b2i True  = 1                                   
    b2i False = 0                                   

-- | Convert a QRcode to a matrix of ones and zeros (1 = On, 0 = Off)
toMatrix :: QRcode -> [[Word8]]
toMatrix (QRcode _ width str) = 
    regroup . map tobin . unpack $ str
    where
      tobin c = c .&. 1                               
      regroup [] = []                               
      regroup x = take width x : regroup (drop width x)   

