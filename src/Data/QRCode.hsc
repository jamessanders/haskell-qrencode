{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Data.QRCode (encodeString, 
                    getQRCodeVersion,
                    getQRCodeWidth,
                    getQRCodeString,
                    EncodeLevel (..), 
                    EncodeMode (..)) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Control.Monad
import Data.ByteString.Internal

#include <qrencode.h>

data EncodeLevel = LEVEL_L | LEVEL_M | LEVEL_Q | LEVEL_H
data EncodeMode  = MODE_NUM | MODE_AN | MODE_EIGHT | MODE_KANJI | MODE_STRUCTURE

convertEncodeLevel :: EncodeLevel -> CInt
convertEncodeLevel LEVEL_L = #const QR_ECLEVEL_L
convertEncodeLevel LEVEL_M = #const QR_ECLEVEL_M
convertEncodeLevel LEVEL_Q = #const QR_ECLEVEL_Q
convertEncodeLevel LEVEL_H = #const QR_ECLEVEL_H

convertEncodeMode :: EncodeMode -> CInt
convertEncodeMode MODE_NUM       = #const QR_MODE_NUM   
convertEncodeMode MODE_AN        = #const QR_MODE_AN    
convertEncodeMode MODE_EIGHT     = #const QR_MODE_8     
convertEncodeMode MODE_KANJI     = #const QR_MODE_KANJI 
convertEncodeMode MODE_STRUCTURE = #const QR_MODE_STRUCTURE

data QRcode = QRcode { 
      getQRCodeVersion :: Int,
      getQRCodeWidth   :: Int,
      getQRCodeString  :: [[Word8]] 
    } deriving (Show, Read)

data Struct = Struct {
      c_version :: CInt,
      c_width :: CInt,
      c_data :: CString
    } deriving (Show)

getStr (Struct _ _ st) = peekCAString st


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Struct where
    alignment _ = #{alignment QRcode}
    sizeOf _ = #{size QRcode}
    peek ptr = do
      version <- #{peek QRcode, version} ptr
      width   <- #{peek QRcode, width} ptr
      data'   <- #{peek QRcode, data} ptr
      return $ Struct version width data'
    poke ptr (Struct version width data') = do
      #{poke QRcode, version} ptr version
      #{poke QRcode, width} ptr width
      #{poke QRcode, data} ptr data'

foreign import ccall unsafe "QRcode_encodeString" 
    c_encodeString :: CString -- string
                   -> CInt    -- version
                   -> CInt    -- level
                   -> CInt    -- hint
                   -> CInt    -- casesensitive
                   -> IO (Ptr Struct)

encodeString str level mode = do
    s <- newCAString str
    let l = convertEncodeLevel level
    let m = convertEncodeMode mode
    c_qr <- join $ fmap peek $ c_encodeString s 0 l m 1
    let version = fromIntegral (c_version c_qr)
    let width   = fromIntegral (c_width   c_qr)
    str <- fmap (regroup width . walkS) $ peekCAString (c_data c_qr)
    return (QRcode version width str)

walkS = map tobin 
tobin c = (c2w c) .&. 1 

regroup _ [] = []
regroup l x = take l x : regroup l (drop l x) 