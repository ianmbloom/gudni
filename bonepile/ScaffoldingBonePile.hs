data Alignment = AlignOver | AlignMin | AlignMax | AlignNext deriving (Show)

data Align = Align
  { alignHorizontal :: Alignment
  , alignVertical   :: Alignment
  }

align :: (Num (SpaceOf a), Ord (SpaceOf a), HasBox a) => Align -> a -> a -> (Point2 (SpaceOf a), Point2 (SpaceOf a))
align (Align aH aV) over under =
        let (h0, h1) = case aH of
                  AlignOver -> (0,0)
                  AlignMax  -> let maxWidth = max (widthOf over) (widthOf under)
                               in (maxWidth - widthOf over, maxWidth - widthOf under)
                  AlignMin  -> (0,0)
                  AlignNext -> (0, widthOf over)
            (v0, v1) = case aV of
                  AlignOver -> (0,0)
                  AlignMax  -> let maxHeight = max (heightOf over) (heightOf under)
                               in (maxHeight - heightOf over, maxHeight - heightOf under)
                  AlignMin  -> (0,0)
                  AlignNext -> (0, heightOf over)
            trans0 = makePoint h0 v0
            trans1 = makePoint h1 v1
        in  (trans0, trans1)

type Label = String

data Scaffolding a = Coupler
                   | Brace (Point2 SubSpace) (Scaffolding a)
                   | Joint [(Label,Scaffolding a)]
                   | Transom (Transom a) (Scaffolding a)

data Transom a = Transom1 (   [Label]) (   (Point2 SubSpace) -> a)
               | Transom2 (V2 [Label]) (V2 (Point2 SubSpace) -> a)
               | Transom3 (V3 [Label]) (V3 (Point2 SubSpace) -> a)
               | Transom4 (V4 [Label]) (V4 (Point2 SubSpace) -> a)

class Leafable t a => Scaffoldable t a where
  buildScaffolding :: t -> Scaffolding a

class Leafable t a where
  makeLeaf :: t -> a

{-
putGlyph :: (Transformable a, Leafable (Glyph SubSpace) a) => Glyph SubSpace -> Point2 SubSpace -> a
putGlyph glyph p = tTranslate p . makeLeaf $ glyph

instance Leafable (Glyph SubSpace) CompoundTree where
  makeLeaf glyph = SLeaf . RawGlyph $ glyph

instance (Transformable a, Leafable (Glyph SubSpace) a) => Scaffoldable Glyph a where
  buildScaffolding glyph = Transom (Transom1 ["base"] (putGlyph glyph))
                                   (Joint [("base"  ,                                            Coupler)
                                          ,("width" , Brace (makePoint (glyphAdvanceWidth glyph) 0) Coupler)
                                          ,("height", Brace (makePoint 0 (glyphHeight glyph))       Coupler)]
                                   )
-}

findPoint :: Scaffolding t -> [Label] -> Point2 SubSpace
findPoint scaffold [] =
  case scaffold of
    Coupler -> Point2 0 0
    Brace p rest -> p + findPoint rest []
    Transom _ rest -> findPoint rest []
    Joint xs -> error "not enough labels to traverse scaffold"
findPoint scaffold (l:ls) =
  case scaffold of
    Coupler -> error "too many labels to traverse scaffold"
    Brace p rest -> p + findPoint rest (l:ls)
    Transom _ rest -> findPoint rest (l:ls)
    Joint xs -> case find ((==l) . fst) xs of
                  Just (_, found) -> findPoint found ls
                  Nothing -> error $ "label " ++ l ++ "does not match any in joint."

connect :: Scaffolding a -> [Label] -> Scaffolding a -> [Label] -> Scaffolding a
connect s0 ls0 s1 ls1 =
  let p0 = findPoint s0 ls0
      p1 = findPoint s1 ls1
  in  Joint [("left",s0),("right",Brace (p1 - p0) s1)]
