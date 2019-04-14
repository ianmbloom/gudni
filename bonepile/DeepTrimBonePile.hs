-- Bonpile from deeptrim branch

-- | Return a horizontal order for two points
compareHorizontal :: Ord s => Point2 s -> Point2 s -> Ordering
compareHorizontal a b = compare (a ^. pX) (b ^. pX)

-- | Label a curve (that is not horizontally convex with a horizontal direction.
direction :: Ord s => Triple (Point2 s) -> (Ordering, [Triple (Point2 s)])
direction (V3 a control b) = (compareHorizontal a b, [V3 a control b])

-- | Combine labeled curves that have the same horizontal order.
groupDirections :: [(Ordering , [a])] -> [(Ordering, [a])]
groupDirections ((ac, as):(bc, bs):ss)= if ac == bc
                                        then groupDirections ((ac, as ++ bs):ss)
                                        else (ac, as):groupDirections((bc, bs):ss)
groupDirections ss                    = ss

-- | If a direction group spans the loop of the curve move it from the end to the begining of the sequence
connectLoop :: Show a => [(Ordering, [a])] -> [(Ordering, [a])]
connectLoop ss = let ((ac, as), mid, (bc, bs)) = takeFirstLast ss
                 in  if ac == bc && ac /= EQ
                     then (ac, bs ++ as):mid
                     else ss

-- | Remove intermedate parts of a sequence of vertical curves to make one long vertical curve.
trimEQ :: Show p => (Ordering, [Triple p]) -> (Ordering, [Triple p])
trimEQ (EQ, [a]) = (EQ, [a])
trimEQ (EQ, ss ) = let (V3 a b _ ,_, V3 _ _ c) = takeFirstLast ss
                   in (EQ,[V3 a b c])
trimEQ x         = x

-- | If a direction group that has a direction is surrounded by vertical groups attach them like wings.
attachEQ :: [(Ordering, [Triple p])] -> [(Maybe (Triple p),(Ordering,[Triple p]), Maybe (Triple p))]
attachEQ ((EQ,[a]):x:y@(EQ,[b]):xs) = (Just a , x, Just b ):attachEQ(y:xs)
attachEQ ((EQ,[a]):x:           xs) = (Just a , x, Nothing):attachEQ   xs
attachEQ ((EQ,_  ):             []) = []
attachEQ (         x:y@(EQ,[b]):xs) = (Nothing, x, Just b ):attachEQ(y:xs)
attachEQ (         x:           xs) = (Nothing, x, Nothing):attachEQ   xs
attachEQ []                         = []

-- | Get the vertical value from a curve segment.
vertical :: (Num s, Ord s, Iota s) => Maybe (Triple (Point2 s)) -> Y s
vertical (Just (V3 a _ b)) = a ^. pY - b ^. pY
vertical _                 = 0


-- Reverse a curve section.
flipTriple :: Triple a -> Triple a
flipTriple (V3 a x b) = V3 b x a

concatSimples' :: [V3 p] -> (p, [(p, p)])
concatSimples' (V3 a b c:[]) = (c, [(a, b)])
concatSimples' (V3 a b c:xs) = let (right, rest) = concatSimples' xs
                                in  (right, (a,b):rest)
concatSimples' [] = error "concatSimples' encountered empty list"

concatSimples :: [V3 p] -> (V3 p, [(p, p)])
concatSimples (V3 left control right:[]) = (V3 right left control, [])
concatSimples (V3 left control _    :xs) = let (right, rest) = concatSimples' xs
                                            in  (V3 right left control, rest)
concatSimples [] = error "concatSimples encountered empty list"

buildStrand' :: (Num s, Ord s, Iota s) => Maybe (Triple (Point2 s)) -> [Triple (Point2 s)] -> Maybe (Triple (Point2 s)) -> [Triple (Point2 s)]
buildStrand' start xs end =
  let s =  if vertical start < 0
           then  -- X -----> X
                 -- ↑
                 -- X
                 start
           else  -- X
                 -- ↓
                 -- X -----> X
                 Nothing
      e =  if vertical end > 0
           then  -- X -----> X
                 --          ↓
                 --          X
                 end
           else  --          X
                 --          ↑
                 -- X <----- X
                 Nothing
   in maybeToList s ++ xs ++ maybeToList e

-- | Build a strand from a directional section and possible flanking vertical sections
buildStrand :: (Num s, Ord s, Iota s) => (Maybe (Triple (Point2 s)), (Ordering, [Triple (Point2 s)]), Maybe (Triple (Point2 s))) -> [Triple (Point2 s)]
buildStrand (start, (LT, xs), end) = buildStrand' start                                            xs                  end
buildStrand (start, (GT, xs), end) = buildStrand' (flipTriple <$> end) (reverse . map flipTriple $ xs) (flipTriple <$> start)
buildStrand (start, (EQ, xs), end) = error "buildStrand'' encountered EQ section."

-- | Split Strands that are longer than the sectionsize into parts.
splitLong :: (Show p) => Int -> [p] -> [[p]]
splitLong sectionSize section = let triplesPerSection = sectionSize `div` 2
                                in  splitLong' triplesPerSection section

splitLong' :: (Show p) => Int -> [p] -> [[p]]
splitLong' triplesPerSection (triples) = let (part, rest) = splitAt triplesPerSection triples
                                         in  if length triples > triplesPerSection
                                             then part:splitLong' triplesPerSection rest
                                             else [triples]

-- | If a direction group spans the loop of the curve move it from the end to the begining of the sequence
wrapEQ :: (Show p) => [(Ordering, [Triple p])] -> [(Ordering, [Triple p])]
wrapEQ xs = let (f, mid, l) = takeFirstLast xs
            in  case f of
                (EQ, _) -> xs ++ [f]
                _       -> case l of
                             (EQ, _) -> l:xs
                             _       -> xs

-- | Build strands from a series of order labeled sections.
buildFromDirections :: CurveTable
                    -> Int
                    -> [(Ordering, [Triple (Point2 SubSpace)])]
                    -> [Strand]
buildFromDirections table sectionSize ss =
  if length ss > 1
  then
        map (Strand . PVect .
             makeCurveStrand table .
             concatSimples) .
        concatMap (splitLong sectionSize) .
        --tr "buildStrand" .
        map buildStrand .
        --tr "attachEQ" .
        attachEQ .
        --tr "trimEQ" .
        map trimEQ .
        --tr "connectLoop" .
        connectLoop $ ss
  else []

rotateVectorLeft :: VS.Vector a -> Int -> VS.Vector a
rotateVectorLeft vs i = VS.drop i vs (VS.++) VS.take i vs

-- | A larger close-enough value.
smidge :: (Num s, Iota s) => s
smidge = iota * 100

{-
instance Storable a => Storable (Range a) where
  sizeOf    (Range _ len) = error "sizeofRange" -- tc "sizeOfRange" $ sizeOf ((undefined :: a)) * tc "len" len
  alignment (Range _ len) = error "alignmentRange" -- tc "alignment" $ alignment (undefined :: a)
  peek ptr                = error "peek not available for Range"
  poke ptr  (Range f len) = error "pokeRange" -- numLoop 0 (len-1) (\i -> poke (ptr `plusPtr` i) (f i))
-}
