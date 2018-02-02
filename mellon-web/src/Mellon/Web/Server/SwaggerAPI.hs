{-|
Module      : Mellon.Web.Server.SwaggerAPI
Description : A Swagger-enhanced REST web service for @mellon-core@ controllers
Copyright   : (c) 2018, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixofticg.com>
Stability   : experimental
Portability : non-portable

This module extends the standard 'MellonAPI' with a Swagger resource.
In every other way, it is identical to the 'MellonAPI' service.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Web.Server.SwaggerAPI
         ( -- * Types
           SwaggerAPI

           -- * Servant / WAI functions
         , swaggerAPI
         , swaggerApp
         , swaggerServer

           -- * Swagger meta-data
         , mellonSwagger

           -- * Convenience functions
         , writeSwaggerJSON
         ) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as C8 (writeFile)
import Data.Swagger
       (Swagger, URL(..), description, info, license, title, url, version)
import Mellon.Controller (Controller)
import Network.Wai (Application)
import Servant ((:<|>)(..), Proxy(..), Server, serve)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

import Mellon.Web.Server.API (MellonAPI, mellonAPI, server)

-- | Extends 'MellonAPI' with a Swagger resource.
type SwaggerAPI = MellonAPI :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | A 'Proxy' for 'SwaggerAPI', exported in order to make it possible
-- to extend the API further.
swaggerAPI :: Proxy SwaggerAPI
swaggerAPI = Proxy

-- | A 'Server' which serves the 'SwaggerAPI' on the given
-- 'Controller'.
--
-- Normally you will just use 'swaggerApp', but this function is
-- exported so that you can extend/wrap 'SwaggerAPI'.
swaggerServer :: Controller d -> Server SwaggerAPI
swaggerServer cc = server cc  :<|> swaggerSchemaUIServer mellonSwagger

-- | A WAI 'Network.Wai.Application' which runs the service, using the
-- given 'Controller'
swaggerApp :: Controller d -> Application
swaggerApp = serve swaggerAPI . swaggerServer

mellonSwagger :: Swagger
mellonSwagger = toSwagger mellonAPI
  & info.title .~ "Mellon API"
  & info.version .~ "1.0"
  & info.description ?~ "Control physical access devices"
  & info.license ?~ ("BSD3" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = C8.writeFile "swagger.json" (encodePretty mellonSwagger)
