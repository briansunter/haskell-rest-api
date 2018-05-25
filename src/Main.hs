{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Data.Aeson
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Text (Text)
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Network.HTTP.Types
import Data.Proxy
import Data.Time.Clock (UTCTime,getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime,utcTimeToPOSIXSeconds)
import Database.Persist
import Database.Persist.Sqlite (runSqlite,runMigration,SqlPersist,toSqlKey,Key(..))
import Database.Persist.Sql(SqlBackend)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Control.Monad.IO.Class
import Data.Maybe (isJust,fromJust,Maybe)
import GHC.Int (Int64(..))
import Servant.Checked.Exceptions

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Deck json
    name Text
    description Text
    createdAt  UTCTime default=CURRENT_TIME
    deriving Show

Card json
    deckId DeckId Eq
    question Text
    answer Text
    timesCorrect Int
    lastAsked UTCTime
    createdAt UTCTime default=CURRENT_TIME
    deriving Show
|]

data DeckPost = DeckPost { name :: !Text,
                           description :: !Text}

instance FromJSON DeckPost where
  parseJSON (Object v) =
    DeckPost <$> v .:  "name"
             <*> v .: "description"

type FlashCardAPI = "decks" :> Get '[JSON] [Entity Deck]
                  :<|> "decks" :> ReqBody '[JSON] DeckPost :> Post '[JSON] (Entity Deck)
                  :<|> "decks" :> Capture "id" Int64 :> Throws DeckNotFoundError :> Get '[JSON] (Entity Deck)

data DeckNotFoundError = DeckNotFoundError deriving (Eq, Read, Show)

instance ToJSON DeckNotFoundError where
  toJSON :: DeckNotFoundError -> Value
  toJSON = toJSON . show

instance ErrStatus DeckNotFoundError where
  toErrStatus :: DeckNotFoundError -> Status
  toErrStatus _ = status404

getDeckByIdH :: Int64 -> Handler (Envelope '[DeckNotFoundError] (Entity Deck))
getDeckByIdH deckId = do
  mDeck <- liftIO $ getDeckById deckId
  case mDeck of
    Nothing -> pureErrEnvelope DeckNotFoundError
    Just deck -> pureSuccEnvelope deck

server :: ServerT FlashCardAPI Handler
server = allDecksH  :<|> postDecksH :<|> getDeckByIdH
  where allDecksH = liftIO getDecks
        postDecksH deck = liftIO $ insertDeck deck

asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = id

getDecks :: IO [Entity Deck]
getDecks =  runSqlite "flashcards.sqlite" . asSqlBackendReader $ do
  decks <- selectList [] [Asc DeckCreatedAt]
  return decks

insertDeck :: DeckPost -> IO (Entity Deck)
insertDeck deckPost = runSqlite "flashcards.sqlite" . asSqlBackendReader $ do
  createdNowDeck <-  liftIO $ deckPostToDeck deckPost
  insertedDeckId <- insert createdNowDeck
  insertedDeck <- getJust insertedDeckId
  return (Entity insertedDeckId insertedDeck)

deckPostToDeck :: DeckPost -> IO Deck
deckPostToDeck (DeckPost name description ) = do
  currentTime <- getCurrentTime
  return $ Deck name description currentTime

getDeckById :: Int64 -> IO (Maybe (Entity Deck))
getDeckById i = do
  runSqlite "flashcards.sqlite" . asSqlBackendReader $ do
    let key = toSqlKey i :: Key Deck
    mdeck <- get key
    return $ fmap (Entity key) mdeck

flashCardAPI :: Proxy FlashCardAPI
flashCardAPI = Proxy

app :: Application
app = serve flashCardAPI server

main = do
  runSqlite "flashcards.sqlite" $ do
    runMigration migrateTables
  run 8081 app
