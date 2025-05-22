{-# LANGUAGE  OverloadedStrings #-}
module Websocket.Connect where

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Control.Concurrent 
import Control.Monad 

webSocketProxy :: String -> Int -> IO (T.Text -> IO ())
webSocketProxy host port = do
    putStrLn $ "Connecting to WebSocket server at " ++ host ++ ":" ++ show port
    messageVar <- newMVar Nothing 
    _ <- forkIO $ WS.runClient host port "/" (app messageVar)
    pure $ \msg -> modifyMVar_ messageVar (const $ pure $ Just msg)
  where
    app :: MVar (Maybe T.Text) -> WS.ClientApp ()
    app messageVar conn = do
        putStrLn "Connected to WebSocket server."
        
        _ <- forkIO $ forever $ do
            msg <- WS.receiveData conn :: IO T.Text
            putStrLn $ "Received: " ++ T.unpack msg
        
        forever $ do
            msgToSend <- readMVar messageVar
            case msgToSend of
                Just msg -> do
                    WS.sendTextData conn msg
                    modifyMVar_ messageVar (const $ pure Nothing) 
                Nothing -> pure () 


