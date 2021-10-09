
runApplicationThresholdOpenCL :: ( Show s
                                 , Model s
                                 , HasStyle s
                                 , Show (TokenOf (StyleOf s))
                                 , SpaceOf (StyleOf s) ~ SubSpace
                                 )
                              => s
                              -> IO ()
runApplicationThresholdOpenCL state =
    do (rasterizer :: RasterState) <- setupRasterizer
       startApplication rasterizer state

runApplicationDagOpenCL :: ( Show s
                           , Model s
                           , HasStyle s
                           , Show (TokenOf (StyleOf s))
                           , SpaceOf (StyleOf s) ~ SubSpace
                           )
                        => s
                        -> IO ()
runApplicationDagOpenCL state =
    do (rasterizer :: DagOpenCLState) <- setupRasterizer
       startApplication rasterizer state

goto :: (DagConstraints (SpaceOf i) m) => (FabricTagId, Maybe (Box (SpaceOf i))) -> SerialMonad i glyphType m (x(SpaceOf i))
goto (tagId, mBox) =
   do -- currentId <- currentPointer
      addReturnTag
      addStackerTag tagId
      -- addStackerTag currentId
      return ([], mBox)
      --srMBox .= mBox

buildCombine :: x
buildCombine ty aFab bFab =
    do  addReturnTag
        addBinaryTag ty
        encodeFabric bFab
        bJump <- currentPointer
        addReturnTag
        encodeFabric aFab
        aJump <- currentPointer
        return $ (aJump, bJump) (combineBoxes a b)

simplifyFabric :: (s ~ SpaceOf i)
               => Fabric i -> x
simplifyFabric =
    go
    where
    go :: Fabric i -> (DependentShape s, Either (Fabric j) ([FabricTagId], Maybe (Box s)))
    go fabric =
        case fabric of
            FBinary ty above below ->
                do  bChild <- go below
                    aChild <- go above
                    let melder = ty ^. proxMeld
                    combineChildren aChild bChild
            FUnaryPost post child ->
                do  go child
            FUnaryPre pre child ->
                do  go child
            FLeaf leaf ->
                case leaf of
                  FShape     shape     -> (Just shape, Left $ FLeaf (FConst insideShape :: FSubstance i))
                  FSubstance substance -> (Nothing,    Left $ FLeaf substance)
            FVar var -> (Nothing, Left $ FVar var)
            FDefine var body applied ->
               do addReturnTag
                  appliedTop <- go applied
                  withVar var body appliedTop

call :: ( DagConstraints (SpaceOf i) m
        , Show item
        , Ord item
        )
     => item
     -> Lens' (SerialState i glyphType) (M.Map item (Maybe (Box (SpaceOf i)), FabricTagId))
     -> SerialMonad i glyphType m ()
call item itemMapLens =
    do itemMap <- use itemMapLens
       case M.lookup item itemMap of
           Just child -> addStackerTag ch
           Nothing -> error $ "item not found " ++ show item

combineIsConfined ty =
    case ty of
        FMask -> True
        _     -> False

simpleCombine ty (FLeaf (FSubstance (FConst i))) (FLeaf (FSubstance (FConst j))) =
    FLeaf $ FSubstance $ applyCombine ty i j
simpleCombine ty a b =
    FBinary ty (FLeaf a) (FLeaf b)

withVar :: ( DagConstraints (SpaceOf i) m
           , Ord (FVarName i)
           )
        => FVarName i
        -> (Maybe (Box (SpaceOf i)), Either a b)
        -> SerialMonad i glyphType m ()
        -> SerialMonad i glyphType m ()
withVar varName (mBox, _) code =
    do codePointer <- currentPointer
       varMap <- use srVarMap
       let (mOld, varMap') = insertLookup varName (codePointer, mBox) varMap
       srVarMap .= varMap'
       h <- code
       case mOld of
           Nothing  -> srVarMap %= M.delete varName
           Just old -> srVarMap %= M.insert varName old
       return h
