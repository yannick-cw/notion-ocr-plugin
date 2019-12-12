{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ElmGen.Generate
  ( gen
  )
where

import           Http.Api
import           Servant.Elm                    ( DefineElm(DefineElm)
                                                , Proxy(Proxy)
                                                , defElmImports
                                                , defElmOptions
                                                , generateElmModuleWith
                                                )

gen :: IO ()
gen = (print "Generating...") *> generateElmModuleWith
  defElmOptions
  ["Generated", "OcrApi"]
  defElmImports
  "plug/src"
  [DefineElm (Proxy :: Proxy InitState), DefineElm (Proxy :: Proxy SyncState)]
  (Proxy :: Proxy OcrApi)
