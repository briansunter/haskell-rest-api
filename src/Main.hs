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

import Data.Aeson
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
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Control.Monad.IO.Class
import Data.Maybe (isJust,fromJust,Maybe)
import GHC.Int (Int64(..))

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
                  :<|> "decks" :> Capture "id" Int64 :> Get '[JSON] (Entity Deck)

server :: Server FlashCardAPI
server = allDecksH  :<|> postDecksH :<|> getDeckByIdH
  where allDecksH = liftIO getDecks
        postDecksH deck = liftIO $ insertDeck deck
        getDeckByIdH deckId = liftIO $ getDeckById deckId

getDecks :: IO [Entity Deck]
getDecks =  runSqlite "flashcards.sqlite" $ do
  decks <- selectList [] [Asc DeckCreatedAt]
  return decks

insertDeck :: DeckPost -> IO (Entity Deck)
insertDeck deckPost = runSqlite "flashcards.sqlite" $ do
  createdNowDeck <-  liftIO $ deckPostToDeck deckPost
  insertedDeckId <- insert createdNowDeck
  insertedDeck <- getJust insertedDeckId
  return (Entity insertedDeckId insertedDeck)

deckPostToDeck :: DeckPost -> IO Deck
deckPostToDeck (DeckPost name description ) = do
  currentTime <- getCurrentTime
  return $ Deck name description currentTime

getDeckById :: Int64 -> IO (Entity Deck)
getDeckById i = do
  runSqlite "flashcards.sqlite" $ do
    let key = toSqlKey i :: Key Deck
    deck <- get key
    return $ (Entity key (fromJust deck))

flashCardAPI :: Proxy FlashCardAPI
flashCardAPI = Proxy

app :: Application
app = serve flashCardAPI server

main = do
  runSqlite "flashcards.sqlite" $ do
    runMigration migrateTables
  run 8081 app
