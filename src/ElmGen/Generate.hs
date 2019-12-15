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
                                                , ElmOptions
                                                , urlPrefix
                                                , UrlPrefix(..)
                                                )

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8081" }


gen :: IO ()
gen = putStrLn "Generating..." *> generateElmModuleWith
  myElmOpts
  ["Generated", "OcrApi"]
  defElmImports
  "plug/src"
  [DefineElm (Proxy :: Proxy InitState), DefineElm (Proxy :: Proxy SyncState)]
  (Proxy :: Proxy OcrApi)
