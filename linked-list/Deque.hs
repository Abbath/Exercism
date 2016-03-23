module Deque where 

import Control.Concurrent.STM

data Element a = Element 
      { elementLeft  :: TVar (Maybe (Element a))
      , _elementValue :: a 
      , elementRight :: TVar (Maybe (Element a))
      }
      
singE :: a -> STM (Element a)
singE x = Element <$> newTVar Nothing <*> pure x <*> newTVar Nothing

newE :: Maybe (Element a) -> a -> Maybe (Element a) -> STM (Element a)
newE r x l = Element <$> newTVar r <*> pure x <*> newTVar l 

type Storage a = (Element a,Element a)
data Deque a = Deque (TVar (Maybe (Element a, Element a)))

mkDeque :: IO (Deque a)
mkDeque = Deque <$> newTVarIO Nothing

push :: Deque a -> a -> IO ()
push (Deque s) x = atomically $ do
    mt <- readTVar s
    z <- case mt of
      Nothing -> singE x >>= \e -> return (e,e)
      Just (l,r@(Element _ _ rr)) -> do
        n <- newE (Just r) x Nothing
        writeTVar rr $ Just n
        return (l,n)
    writeTVar s (Just z)
    
unshift :: Deque a -> a -> IO ()
unshift (Deque s) x = atomically $ do
    mt <- readTVar s
    z <- case mt of
      Nothing -> singE x >>= \e -> return (e,e)
      Just (l@(Element ll _ _),_) -> do
        n <- newE Nothing x (Just l)
        writeTVar ll $ Just n
        return (n,l)
    writeTVar s (Just z)

pop :: Deque a -> IO (Maybe a)
pop (Deque s) = atomically $ do
    mt <- readTVar s
    case mt of
      Nothing -> return Nothing
      Just (l,Element rl rx _) -> do
          rv <- readTVar rl
          case rv of
            Nothing -> {- r==l -} writeTVar s Nothing
            Just r' -> writeTVar s (Just (l,r'))
          return $ Just rx
          
shift :: Deque a -> IO (Maybe a)
shift (Deque s) = atomically $ do 
    mt <- readTVar s
    case mt of 
         Nothing -> return Nothing
         Just (Element _ lx lr, r) -> do
             lv <- readTVar lr
             case lv of 
                  Nothing -> writeTVar s Nothing
                  Just l' -> writeTVar s (Just (l',r))
             return $ Just lx