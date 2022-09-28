{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
module Model where


import           Control.Monad.IO.Class
import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON
                                                , ToJSONKey
                                                )
import           Data.OpenApi                   ( ToParamSchema
                                                , ToSchema
                                                )
import           Data.String                    ( IsString )
import           Data.Typeable
import           DomainDriven
import           DomainDriven.Config
import           GHC.Generics                   ( Generic )
import           Prelude
import           RIO
import qualified RIO.Map                                      as M
import qualified RIO.Text                                     as T
import           RIO.Text                       ( Text )
import           Servant

------------------------------------------------------------------------------------------
-- Defining the types we need                                                           --
-- `HasFieldName` defines the name the type will get in the request body.               --
------------------------------------------------------------------------------------------
newtype ItemKey = ItemKey UUID
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSONKey, ToJSONKey, FromJSON, ToJSON, ToSchema, HasFieldName
                , ToParamSchema, HasParamName)
    deriving newtype (FromHttpApiData, ToHttpApiData)
newtype Quantity = Quantity Int
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)
newtype ItemName = ItemName Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)
    deriving newtype (IsString)
newtype Price = Price Int
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)

data ItemInfo = ItemInfo
    { key             :: ItemKey
    , name            :: ItemName
    , quantity        :: Quantity
    , orderedQuantity :: Quantity -- ^ Ordered from supplier
    , price           :: Price
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | The store actions
-- `method` is `Verb` from servant without the returntype, `a`, applied
data StoreAction method a where
    BuyItem    ::ItemKey -> Quantity -> StoreAction Cmd ()
    ListItems ::StoreAction Query [ItemInfo]
    Search ::Text -> StoreAction Query [ItemInfo]
    ItemAction ::ItemKey -> ItemAction method a -> StoreAction method a
    AdminAction ::AdminAction method a -> StoreAction method a
    deriving HasApiOptions

data ItemAction method a where
    ItemStockQuantity ::ItemAction Query Quantity
    ItemPrice ::ItemAction Query Price
    deriving HasApiOptions

data AdminAction method a where
    Order    ::ItemKey -> Quantity -> AdminAction CbCmd ()
    Restock    ::ItemKey -> Quantity -> AdminAction Cmd ()
    AddItem    ::ItemName -> Quantity -> Price -> AdminAction Cmd ItemKey
    RemoveItem ::ItemKey -> AdminAction Cmd ()
    deriving HasApiOptions

-- | The event
-- Store state of the store is fully defined by
-- `foldl' applyStoreEvent mempty listOfEvents`
data StoreEvent
    = BoughtItem ItemKey Quantity
    | Ordered ItemKey Quantity
    | Restocked ItemKey Quantity
    | AddedItem ItemKey ItemName Price
    | RemovedItem ItemKey
    deriving stock (Show, Eq, Generic, Typeable)
    deriving (FromJSON, ToJSON) via (NamedJsonFields StoreEvent)


type StoreModel = M.Map ItemKey ItemInfo

------------------------------------------------------------------------------------------
-- Action handlers                                                                      --
------------------------------------------------------------------------------------------
handleStoreAction
    :: (MonadIO m, MonadThrow m)
    => MonadThrow m => ActionHandler StoreModel StoreEvent m StoreAction
handleStoreAction = \case
    BuyItem iKey quantity' -> Cmd $ \m -> do
        let available = maybe 0 quantity $ M.lookup iKey m
        when (available < quantity') $ throwM err422 { errBody = "Out of stock" }
        pure (const (), [BoughtItem iKey quantity'])
    ListItems -> Query $ pure . M.elems
    Search t  -> Query $ \m -> do
        let matches :: ItemInfo -> Bool
            matches (ItemInfo _ (ItemName n) _ _ _) =
                T.toUpper t `T.isInfixOf` T.toUpper n
        pure . filter matches $ M.elems m
    ItemAction iKey cmd -> handleItemAction iKey cmd
    AdminAction cmd     -> handleAdminAction cmd

handleAdminAction
    :: forall m
     . (MonadIO m, MonadThrow m)
    => MonadIO m => ActionHandler StoreModel StoreEvent m AdminAction
handleAdminAction = \case
    Order iKey q -> CbCmd $ \runTransaction -> do
        m <- runTransaction $ \m -> pure (const m, [])
        when (M.notMember iKey m) $ throwM err404
        -- Simulate making an external API call that takes 2s.
        -- It is important that we do not do this in a normal Cmd
        -- as this will block any other command from runnning during
        -- this time.
        let orderItems :: ItemKey -> Quantity -> m ()
            orderItems _ _ = liftIO $ threadDelay 2000000
        orderItems iKey q
        -- Note that since this whole command is not running in a transaction it is
        -- possible that the item was removed from the inventory while we were making the
        -- external API call. We ignore it here, but in a real world situation you may
        -- want to handle this.
        runTransaction $ \_ -> pure (const (), [Ordered iKey q])

    Restock iKey q -> Cmd $ \m -> do
        when (M.notMember iKey m) $ throwM err404
        pure (const (), [Restocked iKey q])
    AddItem name' quantity' price -> Cmd $ \_ -> do
        iKey <- ItemKey <$> mkId
        pure (const iKey, [AddedItem iKey name' price, Restocked iKey quantity'])
    RemoveItem iKey -> Cmd $ \m -> do
        when (M.notMember iKey m) $ throwM err404
        pure (const (), [RemovedItem iKey])

handleItemAction
    :: forall m
     . (MonadIO m, MonadThrow m)
    => ItemKey
    -> ActionHandler StoreModel StoreEvent m ItemAction
handleItemAction iKey = \case
    ItemStockQuantity -> Query $ \m -> do
        i <- getItem m
        pure $ quantity i
    ItemPrice -> Query $ \m -> do
        i <- getItem m
        pure $ price i
  where
    getItem :: StoreModel -> m ItemInfo
    getItem = maybe (throwM err404) pure . M.lookup iKey
------------------------------------------------------------------------------------------
-- Event handler                                                                        --
------------------------------------------------------------------------------------------
applyStoreEvent :: StoreModel -> Stored StoreEvent -> StoreModel
applyStoreEvent m (Stored e _ _) = case e of
    Ordered iKey q ->
        M.update (\ii -> Just ii { orderedQuantity = orderedQuantity ii + q }) iKey m
    BoughtItem iKey q -> M.update (\ii -> Just ii { quantity = quantity ii - q }) iKey m
    Restocked iKey q -> M.update (\ii -> Just ii { quantity = quantity ii + q }) iKey m
    AddedItem iKey name' price -> M.insert iKey (ItemInfo iKey name' 0 0 price) m
    RemovedItem iKey -> M.delete iKey m



------------------------------------------------------------------------------------------
-- Grab the config for each GADT
------------------------------------------------------------------------------------------

$(mkServerConfig "storeActionConfig")

-- $(pure []) -- Avoid a strange TH bug. Remove it and the apiOptionsMap will be empty
--
-- apiOptionsMap :: M.Map String ApiOptions
-- apiOptionsMap = $(getApiOptionsMap)

--
-- app :: (WriteModel p, Model p ~ StoreModel, Event p ~ StoreEvent) => Port -> p -> IO ()
-- app port wm = do
--     putStrLn $ "Starting server on port: " <> show port
--     BL.writeFile "/tmp/store_schema.json" . encode . toOpenApi $ Proxy @StoreActionApi
--     run port $ serve (Proxy @StoreActionApi)
--                      (storeActionServer $ runAction wm handleStoreAction)
--
-- forgetfulApp :: Port -> IO ()
-- forgetfulApp p = app p =<< createForgetful applyStoreEvent mempty
