{-# LANGUAGE ForeignFunctionInterface #-}

module Doubler where

import Foreign.C.Types

foreign export ccall doubleLongLong_hs :: CLLong -> CLLong
doubleLongLong_hs :: CLLong -> CLLong
doubleLongLong_hs x = x * x
