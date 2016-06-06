{-|
Module      : Mellon.Web.Server.DocsAPI
Description : A documentation-annotated REST web service for @mellon-core@ controllers
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

This module extends the standard 'MellonAPI' with a documentation
resource, in case you want to provide on-line documentation for the
API. In every other way, it is identical to the 'MellonAPI' service.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Web.Server.DocsAPI
         ( -- * Types
           DocsAPI

           -- * Servant / WAI functions
         , docsAPI
         , docsApp
         , docsServer
         ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Mellon.Controller (Controller)
import Network.HTTP.Types (ok200)
import Network.Wai (Application, responseLBS)
import Servant ((:>), (:<|>)(..), Raw, Proxy(..), Server, serve)
import Servant.Docs (DocIntro(..), docsWithIntros, markdown)

import Mellon.Web.Server.API (MellonAPI, mellonAPI, server)

-- | Extends 'MellonAPI' with a documentation resource.
--
-- The documentation resource is available via the @GET /docs@ method.
type DocsAPI = MellonAPI :<|> "docs" :> Raw

-- | A 'Proxy' for 'DocsAPI', exported in order to make it possible to
-- extend the API.
docsAPI :: Proxy DocsAPI
docsAPI = Proxy

docsBS :: ByteString
docsBS = encodeUtf8 . pack . markdown $ docsWithIntros [intro] mellonAPI
  where
    intro = DocIntro "Mellon API" []

-- | A 'Server' which serves the 'DocsAPI' on the given 'Controller'.
--
-- Normally you will just use 'docsApp', but this function is exported so
-- that you can extend/wrap 'DocsAPI'.
docsServer :: Controller d -> Server DocsAPI
docsServer cc = server cc  :<|> serveDocs
  where
    serveDocs _ respond =
      respond $ responseLBS ok200 [plain] docsBS

    plain = ("Content-Type", "text/plain")

-- | A WAI 'Network.Wai.Application' which runs the server, using the
-- given 'Controller'
docsApp :: Controller d -> Application
docsApp = serve docsAPI . docsServer
