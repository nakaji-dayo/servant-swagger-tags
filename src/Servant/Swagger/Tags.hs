-- move lobrary
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.Swagger.Tags where

import           Control.Lens
import qualified Data.Set        as S
import           Data.Swagger
import qualified Data.Text       as Text
import           Data.Typeable   (Typeable)
import           GHC.TypeLits    (KnownSymbol, Symbol, symbolVal)
import           Servant         hiding (Context)
import           Servant.Client
import           Servant.Mock
import           Servant.Swagger

data Tags (sym :: Symbol)
    deriving (Typeable)

instance HasClient m api => HasClient m (Tags tags :> api) where
  type Client m (Tags tags :> api) = Client m api
  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

instance HasServer api ctx => HasServer (Tags tags :> api) ctx where
  type ServerT (Tags tags :> api) m = ServerT api m
  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

instance (KnownSymbol tags, HasSwagger api) => HasSwagger (Tags tags :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
    & allOperations.tags %~ S.union (S.fromList [Text.pack (symbolVal (Proxy :: Proxy tags))])

instance HasMock api context => HasMock (Tags t :> api) context where
    mock _ = mock (Proxy :: Proxy api)
