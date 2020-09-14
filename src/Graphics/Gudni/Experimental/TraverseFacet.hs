

insideBox :: Point2 s
          -> Box s
          -> Bool
insideBox p box =
       box ^. leftSide   <= p ^. pX
    && box ^. topSide    <= p ^. pY
    && box ^. rightSide  >  p ^. pX
    && box ^. bottomSide >  p ^. pY

couldContain :: Point2 s -> Facet_ s -> Bool
couldContain p facet = insideBox p . boxOf $ facet

limit :: Space s => s
limit = 1 / 16

sizeLimit :: Facet_ s -> Bool
sizeLimit facet =
  let box = boxOf facet
  in heighBox box > limit || widthBox box > limit

traverseFacet :: Point2 s -> Facet_ s -> [Facet_ s]
traverseFacet p = go
  where
  go = concatMap go .
       filter sizeLimit .
       filter (couldContain p) .
       subdivideFacet
