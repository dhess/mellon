{-# LANGUAGE DataKinds #-}

-- | This module extends the standard 'Mellon.Server.API.MellonAPI' with a
-- documentation resource, in case you want to provide on-line
-- documentation for the API. In every other way, it is identical to
-- the 'Mellon.Server.API.MellonAPI' server.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Server.Docs
         ( DocsAPI
         , app
         , docsAPI
         , server
         ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Mellon.Controller as MC
import qualified Mellon.Server.API as API
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Docs

-- | Extends 'Mellon.Server.API.MellonAPI' with a documentation
-- resource.
--
-- The documentation resource is available via the @GET /docs@ method.
type DocsAPI = API.MellonAPI :<|> "docs" :> Raw

-- | A 'Proxy' for 'DocsAPI', exported in order to make it possible to
-- extend the API.
docsAPI :: Proxy DocsAPI
docsAPI = Proxy

docsBS :: ByteString
docsBS = encodeUtf8 . pack . markdown $ docsWithIntros [intro] API.mellonAPI
  where
    intro = DocIntro "Mellon API" []

-- | A 'Server' which serves the 'DocsAPI' on the given
-- 'Mellon.Controller.Concurrent.ConcurrentControllerCtx' instance.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'DocsAPI'.
server :: MC.ConcurrentControllerCtx -> Server DocsAPI
server cc = (API.server cc)  :<|> serveDocs
  where
    serveDocs _ respond =
      respond $ responseLBS ok200 [plain] docsBS

    plain = ("Content-Type", "text/plain")

-- | An 'Network.Wai.Application' which runs the server, using the given
-- 'Mellon.Controller.Concurrent.ConcurrentControllerCtx' instance for
-- the controller.
app :: MC.ConcurrentControllerCtx -> Application
app = serve docsAPI . server
