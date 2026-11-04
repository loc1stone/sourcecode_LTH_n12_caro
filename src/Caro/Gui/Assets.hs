{-# LANGUAGE TemplateHaskell #-}
-- | Nhúng trang giao diện @gui/index.html@ vào chương trình LÚC BIÊN DỊCH
-- (Template Haskell), nên chương trình chạy ở đâu cũng có sẵn giao diện, không
-- phụ thuộc đường dẫn tệp. Sửa @gui/index.html@ rồi biên dịch lại là xong.
module Caro.Gui.Assets (indexHtml) where

import Language.Haskell.TH (litE, runIO, stringL)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.IO

indexHtml :: String
indexHtml = $(do
  let path = "gui/index.html"
  addDependentFile path
  txt <- runIO $ do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    s <- hGetContents h
    length s `seq` hClose h
    pure s
  litE (stringL txt))
