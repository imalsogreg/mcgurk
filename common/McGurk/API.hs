{-# language TypeOperators #-}
{-# language DataKinds     #-}

module McGurk.API where

import Data.Proxy
import Servant.API
import McGurk.Types

type Api =
           "rooms" :> Get '[JSON] [RoomNumber]
      :<|>
           "run-lecture" :> Raw
      :<|>
           "browser-socket" :> Capture "room" RoomNumber :> Raw
      :<|>
           "respond" :> Capture "room" RoomNumber
                     :> ReqBody '[JSON] Respond
                     :> Post '[] ()


apiProxy :: Proxy Api
apiProxy = Proxy
