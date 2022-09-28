{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains simple example of how to setup a counter using domain-driven.
module Main where

import           Control.Monad.Except
import           Data.Aeson
import           DomainDriven
import           DomainDriven.Persistance.ForgetfulInMemory
import           DomainDriven.Server
import           Model
import           Network.Wai.Handler.Warp       ( run )
import           Prelude
import           RIO                     hiding ( Handler )
import           Servant
import           Servant.OpenApi

$(mkServer storeActionConfig ''StoreAction)

main :: IO ()
main = do
    -- Pick a persistance model to create the domain model
    m <- createForgetful applyStoreEvent mempty
    -- Now we can supply the ActionRunner to the generated server and run it as any other
    -- Servant server.
    run 8888 $ serve (Proxy @StoreActionApi) $ hoistServer
        (Proxy @StoreActionApi)
        (Handler . ExceptT . try)
        (storeActionServer $ runAction m handleStoreAction)
