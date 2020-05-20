
type BytePile = Pile CChar

-- | Add to a bytepile within a StateT monad transformer with a lens to the bytepile within the state.
-- updating the state along the way.
addToBytePileState :: (Storable t, Show t, MonadState s m, Monad m, MonadIO m)
                   => Lens' s BytePile
                   -> t
                   -> m Int
addToBytePileState lens object =
  do pile <- use lens
     (pile', ref) <- liftIO $ addToBytePile "addToPileState" pile object
     lens .= pile'
     return ref

-- | Add any type with an instance of Storable to a pile or bytes.
addToBytePile' :: forall t. (Storable t, Show t) => String -> BytePile -> t -> IO BytePile
addToBytePile' message pile@(Pile cursor size startPtr) item =
  let end = cursor + sizeOf item
  in
  if end < size
  then
    do --putStrLn $ "aboutToPoke" ++ show pile
       poke (startPtr `plusPtr` cursor) item
       return $ Pile end size startPtr
  else
    do
       e <- extendPile message pile
       addToBytePile' message e item

-- | Add any type with an instance of Storable to a pile or bytes. Return the position where it started (in bytes)
addToBytePile :: forall t. (Storable t, Show t) => String -> BytePile -> t -> IO (BytePile, Int)
addToBytePile message pile@(Pile cursor size startPtr) item =
    do --putStrLn $ "addToBytePile " ++ message ++ show item
       pile' <- addToBytePile' message pile item
       return (pile', cursor)

-- | Determine if an item can be added to a bytepile without going over size limit.
canAddToBytePile :: (Storable t) => BytePile -> Int -> t -> Bool
canAddToBytePile (Pile cursor _ _) limit item = cursor + sizeOf item < limit

-- | Add a vector of any storable type to a byte piles. Return the start position in bytes.
addVectorToBytePile :: forall t . (Storable t, Show t) => BytePile -> VS.Vector t -> IO (BytePile, Reference CChar)
addVectorToBytePile pile@(Pile cursor size startPtr) vector =
  let vlen =  VS.length vector
      end = cursor + vlen
      vecSize = vlen * sizeOf (undefined :: t)
  in
  if end < size
  then do let cursorPtr = startPtr `plusPtr` (cursor * sizeOf (undefined :: t))
          VS.unsafeWith vector $ \ptr -> copyBytes cursorPtr ptr vecSize
          return $ (Pile end size startPtr, Ref $ fromIntegral cursor)
  else do e <- extendPile "addVectorToBytePile" pile
          addVectorToBytePile e vector
