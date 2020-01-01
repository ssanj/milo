{-# LANGUAGE ScopedTypeVariables   #-}

module Milo.Cursored (
    -- * functions
    unfoldWith
  
    -- * data types
  , Cursor(..)
  , CursorState(..)
  
) where

-- | Wrapper around a cursor of some sort
newtype Cursor a = Cursor a deriving Eq

-- | An ADT to capture the three states of a cursor:
data CursorState a 
  = NewCursor           -- ^ Initial request - there is no cursor; the iteration will start at this state
  | GoCursor (Cursor a) -- ^ A state of having a cursor; the iteration will continue in this state
  | StopCursor          -- ^ The final cursor state; the iteration will stop at this state

-- | A Generic function that unfolds a computation that has some form of 'Cursor'; such as a paginated REST API.
unfoldWith :: forall m a b c. Applicative m 
           => 
           (a -> (b, CursorState c))        -- ^ a function that can extract a "result" and the next 'CursorState' from some response 'a'
           -> (Maybe (Cursor c) -> m a)     -- ^ a function that takes an optional 'Cursor' and returns a response 'a' in the effect 'm'
           -> (CursorState c)               -- ^ an initial 'CursorState'
           ->  m (Maybe (b, CursorState c)) -- ^ the optional pair of the result type 'b' with the next 'CursorState' in the effect 'm'
  
unfoldWith extractPayload iterateWith NewCursor = 
  let resultM :: m a = iterateWith Nothing
  in  Just . extractPayload <$> resultM
unfoldWith extractPayload iterateWith (GoCursor c) = 
  let resultM :: m a = iterateWith (Just c)
  in  Just . extractPayload <$> resultM
unfoldWith _ _ StopCursor = pure Nothing 