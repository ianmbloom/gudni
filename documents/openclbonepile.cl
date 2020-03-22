accumulatePictureReference :: PictureMemoryReference -> StateT (Pile PictureMemoryReference) IO PictMemId
accumulatePictureReference memory =
  do refPile <- get
     let currentId = refPile ^. pileCursor
     refPile <- liftIO $ addToPile "refPile" refPile memory
     return (fromIntegral currentId)

collectPictureReferences :: PictureMemoryMap -> IO (PictureIdMap, Pile PictureMemoryReference)
collectPictureReferences mapping =
   do refPile <- newPile
      runStateT (mapM accumulatePictureReference mapping) refPile


inline HEADER makeHeaderIndex(int shapeBit) {
    return THRESHOLDENABLE | shapeBit;
}

inline HEADER setHeaderActiveFacet ( HEADER header
                                   ,
                                   , bool enable
                                   ) {
    HEADER enableShape = enable ? THRESHOLDENABLE : THRESHOLDDISABLE;
    return (header & PERSIST_AND_SLOPE_MASK) | enableShape | newBit;
}

inline HEADER disableHeader  (HEADER h) {return h & DISABLEMASK;  }


inline HEADER setShapeBit ( HEADER header
                          , HEADER newBit
                          ) {
    return (header & WITHOUT_PAYLOAD) | newBit;
}

-- sHAPETAGsUBSTANCETYPEsHIFT      = 30 :: Int

// A shape is a reference to the slice of the geometry heap
// or a reference to the facet depending on the shapeTag
typedef struct Shape
  { ITEMTAG shapeTag;
    Slice    shapeSlice;
  } Shape;

  -- | Make an itemTag
  itemInfoToTag :: ItemInfo -> ItemTag
  itemInfoToTag itemInfo =
    case itemInfo of
       ShapeInfo substance combine (SubstanceTagId substanceId) ->
           let combineFlag =
                   case combine of
                       CompoundAdd      -> sHAPETAGcOMPOUNDtYPEaDD
                       CompoundSubtract -> sHAPETAGcOMPOUNDtYPEsUBTRACT
           in  if unSubstanceTagId groupId <= mAXsHAPEID
               then ItemTag (iTEMtAGiSsHAPE .|. combineFlag .|. (substanceId .&. sHAPETAGsUBSTANCEIDbITMASK))
               else error "shapeID out of bounds"
       FacetInfo (FacetId facetId) ->
          ItemTag (iTEMtAGiSfACET .|. (facetId .&. iTEMtAGsUBSTANCEIDbITMASK))

instance StorableM (Shape GeoReference) where
  sizeOfM _ = do sizeOfM (undefined :: ItemInfo)
                 sizeOfM (undefined :: GeoReference   )
  alignmentM _ = do alignmentM (undefined :: ItemInfo)
                    alignmentM (undefined :: GeoReference   )
  peekM = do shapeInfo <- peekM
             geoRef <- peekM
             return (Shape shapeInfo geoRef)
  pokeM (Shape shapeInfo geoRef) = do pokeM shapeInfo
                                      pokeM geoRef

instance Storable (Shape GeoReference) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

-- DEPRECATED
-- -- | SubstanceType defines a flag for the rendering kernel to select a coloring algorithm.
-- data SubstanceType = SubstanceSolidColor | SubstancePicture  deriving (Show, Ord, Eq)


-- | Build a shape with the supplied metadata and representation type.
makeShape :: HasSpace rep
          => SubstanceTagId
          -> Substance (NamedTexture (SpaceOf rep))
          -> Compound
          -> rep
          -> Shape rep
makeShape substanceId substance combineType rep = Shape (ItemInfo (substanceToSubstanceType substance) combineType substanceId) rep


{-
findPicture ::  (M.Map PictureName (Either Picture PictureMemoryReference), Pile Word8) -> PictureFacet PictureName s
            ->  IO ((M.Map PictureName (Either Picture PictureMemoryReference), Pile Word8), PictureFacet PictureMemoryReference s)
findPicture (mapping, pictPile) usage =
    let name = pictSource usage
    in
    case M.lookup name mapping of
        Nothing -> error "cannot find picture name in picture map."
        Just eitherPicture ->
            case eitherPicture of
                Right foundReference ->
                      return ((mapping, pictPile), usage {pictSource = foundReference})
                Left picture ->
                  do  let size = pictureSize picture
                          memory = PictureMemory size (pictPile ^. pileCursor)
                          pVector = pictureData picture
                      (pictPile', _) <- addVectorToPile pictPile pVector
                      let mapping' = M.insert name (Right memory) mapping
                          newUsage = usage {pictSource = memory}
                      return ((mapping', pictPile'), newUsage)


-- | Create a vector of raw bytes and list of picture memory offsets that the rasterizer
-- can use to reference images. (Rickety)
-}
{-
makePictData :: (Show s, Storable (PictureFacet PictureMemoryReference s))
             => PictureMap
             -> [PictureFacet PictureName s]
             -> IO (Pile Word8, Pile (PictureFacet PictureMemoryReference s))
makePictData mapping usages =
  do  dataPile <- newPile
      ((_, pictDataPile), usages') <- mapAccumM findPicture (M.map Left mapping, dataPile) usages
      usagePile <- listToPile usages'
      return (pictDataPile, usagePile)
-}

-- Increment for the next usage.
suCurrentPictureUsage += 1
-- return a Substance with the right usage id.
return . Texture . fromIntegral $ current


instance StorableM (PictureFacet PictUsageId SubSpace) where
  sizeOfM _ =
    do sizeOfM (undefined :: HardFacet_ SubSpace TextureSpace)
       sizeOfM (undefined :: PictUsageId  )
  alignmentM _ =
    do alignmentM (undefined :: HardFacet_ SubSpace TextureSpace)
       alignmentM (undefined :: PictUsageId  )
  peekM = do facet  <- peekM
             pictId <- peekM
             return (PictureFacet pictId facet)
  pokeM (PictureFacet pictId facet) =
    do  pokeM facet
        pokeM pictId

instance Storable (PictureFacet PictUsageId SubSpace) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV


  // LayerFlags

  #define LAYERFLAGS             ulong
  #define LAYERFLAGSBITS         64
  #define LAYERFLAGSCARRYSHIFT   63
  #define LAYERFLAGSCARRYMASK    0x1
  #define LAYERFLAGSSECTIONSHIFT 6    // the amount to shift to get the section from the total bits
  #define LAYERFLAGSSECTIONBITS  0x3F

  #define EMPTY_LAYERFLAGS       0x0
  #define COMPLETE_MASK          0xFFFFFFFFFFFFFFFF
  // The layer index of top layer (the visible one) can be determined by counting the leading zeros in the shapestack value using clz

  inline void clearLayerFlags(LAYERFLAGS *stack) {
      for (int i = 0; i < LAYERFLAGSSECTIONS; i++) {
          stack[i] = EMPTY_LAYERFLAGS;
      }
  }

  inline LAYERFLAGS ignoreLayers(LAYERFLAGS section, int ignoreBits) {
      //DEBUG_IF(printf("ignoreBits %i (COMPLETE_MASK << ignoreBits) %lX (~(COMPLETE_MASK << ignoreBits)) %lX ignoreLayers %lX (LAYERFLAGSBITS - clz(ignoreLayers)) - 1 %i \n", ignoreBits, (COMPLETE_MASK << ignoreBits), (~(COMPLETE_MASK << ignoreBits)), ignoreLayers, (LAYERFLAGSBITS - clz(ignoreLayers)) - 1 );)
      return ignoreBits >= LAYERFLAGSBITS ? section : (~(COMPLETE_MASK << ignoreBits)) & section;
  }

  inline int findSectionTop(LAYERFLAGS section) {
      // find the top set bit
      return LAYERFLAGSBITS - clz(section);
  }

  inline int findTop(PMEM LAYERFLAGS *layerFlags, int ignoreAbove) {
      int ignoreSection = (ignoreAbove >> LAYERFLAGSSECTIONSHIFT);
      int ignoreBits    = ignoreAbove & LAYERFLAGSSECTIONBITS;
      //DEBUG_IF(printf("findTop ignoreAbove %i iS %i ignoreBits %i ", ignoreAbove, ignoreSection, ignoreBits);)
      LAYERFLAGS section = ignoreLayers(layerFlags[ignoreSection], ignoreBits);
      while (section == EMPTY_LAYERFLAGS && ignoreSection > 0) {
        ignoreSection -= 1;
        section = layerFlags[ignoreSection];
      }
      int sectionBits = findSectionTop(section);
      return (ignoreSection << LAYERFLAGSSECTIONSHIFT) + sectionBits - 1;
  }


  inline LAYERFLAGS setSectionBit(LAYERID bit)                     {return (((ulong)0x1) << bit);} // create a shape by shifting a bit to the right position
  inline LAYERFLAGS flipSectionBit(LAYERID bit, LAYERFLAGS section) {return (section ^ setSectionBit(bit));} // if shape on is true, toggle a shape bit

  inline void flipBit(LAYERID layer, PMEM LAYERFLAGS *layerFlags) {
      int section = layer >> LAYERFLAGSSECTIONSHIFT;
      int bit     = layer & LAYERFLAGSSECTIONBITS;
      //DEBUG_IF(printf("flipBit section %i bit %i before layerFlags[section] %lx ", section, bit, layerFlags[section]);)
      layerFlags[section] = flipSectionBit(bit, layerFlags[section]);
      //DEBUG_IF(printf(" after layerFlags[section] %lx\n", layerFlags[section]);)
  }


  //////////////////////////////////////////////////////////////////////////////////////////////
  // Functions for Exporting Debugging Information in human readable forms.
  //////////////////////////////////////////////////////////////////////////////////////////////

  // Debug Functions
  void showTraversal(Traversal t);
  void showTree(            Traversal t
               , GMEM       float4 *tree
               ,            int     treeSize
               ) ;
  void showPixelBuffer(__local uint *pixelBuffer, int sectionWidth, int height);
  void showTileInfoAlignment (int tileIndex, GMEM TileInfo *tileHeap);
  void showSubstance(Substance substance);
  void showPictUsage(CMEM PictUse *use, int i);
  void showShapeIndices(ShapeState *shS, int numItems);
  void showSubstances(GMEM Substance *substances, int numSubstances);
  void showPictUsages(CMEM PictUse *uses, int numPicts);
  void showShapeRefs(GMEM REF *shapeRefs, int numItems);
  void showLayers(LAYERENTRY *layers);
  void showData (GMEM uchar *pictData, int width, int height);
  void showThresholdHeader( HEADER header);
  void showThresholdGeo (THRESHOLD threshold);

  void showThreshold( HEADER header
                    , THRESHOLD threshold
                    );


  void showThresholds (PMEM   ThresholdQueue *tQ);
  void showActiveThresholds(PMEM ThresholdQueue *tQ, int num);

  // Output for parsings by GudniTraceVisualizer
  float infiniteZero(float x);
  void sliceHs (Slice slice);
  void tileInfoHs (TileInfo tileInfo);
  void boxHs (int4 box);
  void colorHs (COLOR color);
  void colorStateHs (ColorState colorState);
  void thresholdHs(int i, THRESHOLD threshold, HEADER header);
  void thresholdListHs ( PMEM ThresholdQueue tQ );
  void thresholdQueueHs (ThresholdQueue tQ);
  inline void shapeIndexHs (int i, int shapeIndex);
  void shapeStateHs (ShapeState shapeState);
  void parseStateHs (ParseState pS);
  void tileStateHs (TileState tileS);
  void traversalHs(Traversal t);

  void showTraversal(Traversal t) {
    printf("travLeft %v2f travControl %v2f travRight %v2f travXPos %f travIndex %i\n"
         ,t.travLeft   ,t.travControl   ,t.travRight   ,t.travXPos ,t.travIndex     );
  }

  void showTree(            Traversal t
               , GMEM       float4 *tree
               ,            int     treeSize
               ) {
      printf("right   %v2f\n", t.travRight);
      printf("left    %v2f\n", t.travLeft);
      printf("control %v2f\n", t.travControl);
      for (int i = 0; i < treeSize; i++) {
          printf("%02i    %v2f\n", i, tree[i].xy);
          printf("        %v2f\n",    tree[i].zw);
      }
  }

  void showPixelBuffer(__local uint *pixelBuffer, int sectionWidth, int height) {
      printf ("PixelBuffer: sectionWidth %2i height: %2i\n", sectionWidth, height);
      for (int y = 0; y < height; y++) {
          for (int x = 0 ; x < sectionWidth; x++) {
             printf(" %08x ", pixelBuffer[pos2(x,y,sectionWidth)]);
          }
          printf("\n");
      }
  }

  void showThresholdHeader( HEADER header
                          ) {
      //printf("Raw: %08x ",header);
      if (headerPositiveSlope(header)) {
        printf("[+] ");
      }
      else {
        printf("[-] ");
      }
      if      (headerPersistTop(header)   ) {printf("pTop ");}
      else if (headerPersistBottom(header)) {printf("pBot ");}
      else                                  {printf("pNon ");}
      printf(" sIx:%03i ", headerLayerId(header));
  }

  void showThresholdGeo (THRESHOLD threshold) {
    //printf("tTop: %2.2f tBottom: %2.2f tLeft: %2.2f tRight: %2.2f "
    printf("tLeft: %f tTop: %f tRight: %f tBottom: %f height: %f"
          , tLeft(threshold)
          , tTop(threshold)
          , tRight(threshold)
          , tBottom(threshold)
          , tHeight(threshold)
          );
  }

  void showThreshold( HEADER header,
                      THRESHOLD threshold) {
      showThresholdHeader(header);
      showThresholdGeo(threshold);
  }

  void showThresholds (PMEM ThresholdQueue *tQ) {
      printf ("Thresholds numThresholds %2i \n", tQ->qSlice.sLength);
      for (int t = 0; t < tQ->qSlice.sLength; t++) {
          if (t < MAXTHRESHOLDS) {
          printf("t: %2i ", t);
          showThreshold( getHeader(tQ, t)
                       , getThreshold(tQ, t)
                       );
          printf("\n");
          }
          else {printf("out of bounds\n\n\n");}
      }
  }

  void showActiveThresholds(PMEM ThresholdQueue *tQ, int num) {
      printf ("------Thresholds numThresholds %2i------- \n", num);
      for (int t = 0; t < num; t++) {
        printf("t: %2i ", t);
        showThreshold( getHeader(tQ, t)
                     , getThreshold(tQ, t));
        printf("\n");
      }
  }

  void showTileInfoAlignment (int gTileIndex, GMEM TileInfo *tileHeap) {
      //printf ("%i :            threadDelta %2i %2hi,%2hi\n", gTileIndex, (long)&tileHeap[gTileIndex].threadDelta - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].threadDelta.x / FIXEDFACTORINT, tileHeap[gTileIndex].threadDelta.y / FIXEDFACTORINT);
      //printf ("%i :     tileMaxThresholds %2i %i   \n", gTileIndex, (long)&tileHeap[gTileIndex].tileMaxThresholds     - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].tileMaxThresholds     );
      printf ("%i : tileInfo.tileBox       %2i %2v4i  \n", gTileIndex, (long)&tileHeap[gTileIndex].tileBox    - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].tileBox     );
      printf ("%i : tileInfo.tileHDepth    %2i %i     \n", gTileIndex, (long)&tileHeap[gTileIndex].tileHDepth - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].tileHDepth  );
      printf ("%i : tileInfo.tileVDepth    %2i %i     \n", gTileIndex, (long)&tileHeap[gTileIndex].tileVDepth - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].tileVDepth  );
      printf ("%i : tileInfo.tileColumnAl..%2i %i     \n", gTileIndex, (long)&tileHeap[gTileIndex].tileColumnAllocation - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].tileColumnAllocation  );
      printf ("%i : tileInfo.tileShapeSlice.sStart  %2i %hi    \n", gTileIndex, (long)&tileHeap[gTileIndex].tileShapeSlice.sStart  - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex+1].tileShapeSlice.sStart  );
      printf ("%i : tileInfo.tileShapeSlice.sLength %2i %hi    \n", gTileIndex, (long)&tileHeap[gTileIndex].tileShapeSlice.sLength - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex+1].tileShapeSlice.sLength );
      printf ("%i :              alignment %2i        \n", gTileIndex, (long)&tileHeap[gTileIndex+1]       - (long)&tileHeap[gTileIndex]);
  }



  void showSubstance(Substance substance) {
      printf ("     substanceColor %2i  %2.2v4f \n", (long) &substance.substanceColor - (long)&substance, substance.substanceColor);
  }

  void showPictUsage(CMEM PictUse *uses, int i) {
      printf (" pictTranslate %2i  %2.2v2f \n", (long)&uses[i].pictTranslate - (long)&uses[i], uses[i].pictTranslate);
      printf ("      pictSize %2i  %v2i    \n", (long)&uses[i].pictSize      - (long)&uses[i], uses[i].pictSize     );
      printf (" pictMemOffset %2i  %i      \n", (long)&uses[i].pictMemOffset - (long)&uses[i], uses[i].pictMemOffset);
      printf ("     pictScale %2i  %f      \n", (long)&uses[i].pictScale     - (long)&uses[i], uses[i].pictScale    );
      printf ("     alignment %2i          \n", (long)&uses[i+1]             - (long)&uses[i]);
  }

  void showShapeIndices( ShapeState *shS
                       ,        int  numItems
                       ) {
      int num = min(numItems, MAXSHAPE);
      printf("Substance indices %i\n", num);
      for (int i = 0; i < num; i++) {
          printf("        [%02i] -> %02i\n", i, shS->itemTagStack[i]);
      }
  }

  void showSubstances(GMEM Substance *substances, int numSubstances) {
    printf("substances: num = %i\n", numSubstances);
    for (int n = 0; n < numSubstances; n++) {
      printf(" %i\n", n);
      showSubstance(substances[n]);
      //printf ("      alignment %2i \n", (long) &substances[n+1] - (long)&substances[n]);
    }
  }

  void showPictUsages(CMEM PictUse *uses, int numPicts) {
    printf("pictUsages: num = %i\n", numPicts);
    for (int n = 0; n < numPicts; n++) {
      showPictUsage(uses, n);
    }
  }

  void showShapeRefs(GMEM REF *shapeRefs, int numItems) {
    printf("shapeRefs: numItems = %i\n", numItems);
    for (int n = 0; n < numItems; n ++ ) {
        printf ("shapeRef %2i:%2i\n", n, shapeRefs[n]);
      }
  }

  /*
  void showShapes(GMEM Shape *shapeHeap, ITEMTAGID itemStart, GMEM Substance *substances, int numItems) {
      for (int n = 0; n < numItems; n ++ ) {
          Shape shape = shapeHeap[itemStart + n];
          printf("%i: ", n);
          if (shapeTagIsAdd(shape.shapeTag)) {
              printf("add ");
          }
          else if (shapeTagIsSubtract(shape.shapeTag)) {
              printf ("sub ");
          }
          else if (shapeTagIsContinue(shape.shapeTag)) {
              printf("con ");
          }
          else {
              printf("    ");
          }
          printf("isSolid %i ",shapeTagIsSolidColor(shape.shapeTag));
          printf("start %i num %i tag %lX \n", shape.shapeSlice.sStart, shape.shapeSlice.sLength, shape.shapeTag);
      }
  }
  */

  void showLayers(LAYERENTRY *layers, int numLayers) {
    for (int i = 0; i < numLayers; i++) {
      printf(" %016lX", layerFlags[i]);
    }
  }

  void showData (GMEM uchar *pictData, int width, int height) {
      for (int y = 0; y < height; y ++) {
        for (int x = 0; x < width; x++ ) {
            printf ("%2.2v4f ", getPicturePixel(pictData, width, x, y));
        }
        printf ("\n");
      }
  }

  ////////////////////////////////////////////////
  // Functions for exporting debugging information
  // parsable by GudniTraceVisualizer.hs
  ////////////////////////////////////////////////

  float infiniteZero(float x) {return isinf(x) ? 0 : x;} // default infinite values to zero to prevent parse error.

  void sliceHs (Slice slice) {
      printf("SliceHs\n");
      printf("{ sStart  = %i\n", slice.sStart);
      printf(", sLength = %i\n", slice.sLength);
      printf("}\n");
  }

  void tileInfoHs (TileInfo tileInfo) {
      printf("TileInfoHs\n");
      printf("{ tileBox = "); boxHs(tileInfo.tileBox); printf("\n");
      printf(", tileHDepth = %i\n", tileInfo.tileHDepth);
      printf(", tileVDepth = %i\n", tileInfo.tileVDepth);
      printf(", tileShapeSlice = "); sliceHs(tileInfo.tileShapeSlice); printf("\n");
      printf("}\n");
  }

  void boxHs (int4 box) {
      printf("(%i, %i, %i, %i)",box.x,box.y,box.z,box.w);
  }

  void colorHs( COLOR color) {
    printf ("ColorHs\n");
    printf ("{ redChan   = %f\n", color.s0);
    printf (", greenChan = %f\n", color.s1);
    printf (", blueChan  = %f\n", color.s2);
    printf (", alphaChan = %f\n", color.s3);
    printf ("} \n");
  }

  void colorStateHs (ColorState colorState) {
      printf("ColorStateHs\n");
      printf("{ csBackgroundColor = ");colorHs(colorState.csBackgroundColor);printf("\n");
      printf(", csPictureData = %i\n", colorState.csPictureData);
      printf(", csPictureRefs = %i\n", colorState.csPictureRefs);
      printf(", absolutePosition = (%f, %f)\n", colorState.absolutePosition.x, colorState.absolutePosition.y);
      printf("}\n");
  }

  void thresholdHs(int i, THRESHOLD threshold, HEADER header) {
    printf("ThresholdHs\n");
    printf("{ thrIndex         = %i\n", i                                );
    printf(", thrTop           = %f\n", infiniteZero (tTop(threshold)   ));
    printf(", thrBottom        = %f\n", infiniteZero (tBottom(threshold)));
    printf(", thrLeft          = %f\n", infiniteZero (tLeft(threshold)  ));
    printf(", thrRight         = %f\n", infiniteZero (tRight(threshold) ));
    printf(", thrPositiveSlope = %i\n", headerPositiveSlope(header)      );
    printf(", thrTopPersist    = %i\n", headerPersistTop(header)         );
    printf(", thrBottomPersist = %i\n", headerPersistBottom(header)      );
    printf(", thrShapeIndex    = %i\n", headerLayerId(header)           );
    printf("}\n");
  }

  void thresholdListHs ( PMEM ThresholdQueue tQ ) {
    for (int t = 0; t < tQ.qSlice.sLength; t++) {
        if (t==0) {printf ("[");} else {printf (",");}
        thresholdHs(t, getThreshold(&tQ, t), getHeader(&tQ, t));
        printf ("\n");
    }
    printf("]");
  }

  void thresholdQueueHs (ThresholdQueue tQ) {
      printf("ThresholdStateHs\n");
      printf("{ thresholds = "); thresholdListHs(tQ); printf("\n");
      //printf(", qSlice.sStart = %i\n", tQ.qSlice.sStart);
      //printf(", qSlice.sLength = %i\n", tQ.qSlice.sLength);
      printf("}\n");
  }

  inline void layerFlagsSectionHs (LAYERFLAGS section) {
    printf(" %lu\n", section);
  }

  inline void shapeIndexHs (int i, int shapeIndex) {
    printf("(%i,%i)\n", i, shapeIndex);
  }

  void shapeStateHs (ShapeState shapeState) {
        int num = min((int)shapeState.layerCount, MAXSHAPE);
        printf("ShapeStateHs\n");
        printf("{ layerCount = %i\n", num);
        printf(", itemTagStack = \n");
        printf("[ ");shapeIndexHs(0,shapeState.itemTagStack[0]);
        for (int i = 1; i < num; i++) {
            printf(", ");shapeIndexHs(i,shapeState.itemTagStack[i]);
        }
        printf("]\n");
        printf(", layerFlags = \n");
        printf("[");layerFlagsSectionHs(shapeState.layerFlags[LAYERFLAGSSECTIONS - 1]);
        for (int i = LAYERFLAGSSECTIONS - 2; i >= 0; i--) {
            printf(",");layerFlagsSectionHs(shapeState.layerFlags[i]);
        }
        printf("]\n");
        printf("}\n");
  }


  void parseStateHs (ParseState pS) {
      printf("ParseStateHs\n");
      printf("{ currentThreshold = %i\n",   pS.currentThreshold                  );
      //printf(", numActive = %i\n",          pS.numActive                         );
      printf(", sectionStart = (%f, %f)\n", pS.sectionStart.x, pS.sectionStart.y );
      printf(", sectionEnd = (%f, %f)\n",   pS.sectionEnd.x, pS.sectionEnd.y     );
      //printf(", pixelY = %f\n",             pS.pixelY                            );
      printf(", accColorArea = (%f,%f,%f,%f,%f,%f,%f,%f)\n"
              , pS.accColorArea.s0
              , pS.accColorArea.s1
              , pS.accColorArea.s2
              , pS.accColorArea.s3
              , pS.accColorArea.s4
              , pS.accColorArea.s5
              , pS.accColorArea.s6
              , pS.accColorArea.s7
              );
      //printf(", sectionCount = %i\n",      pS.sectionCount     );
      //printf(", frameNumber = %i\n",       pS.frameNumber      );
      //printf(", buildCount = %i\n",        pS.buildCount       );
      //printf(", randomFieldCursor = %i\n", pS.randomFieldCursor);
      //printf(", randomField = %i\n",       pS.randomField      );
      printf("}\n");
  }

  void tileStateHs (TileState tileS) {
      printf("TileStateHs\n");
      printf("{ tileItemStart = %i\n"     ,  tileS.tileItemStart);
      printf(", tileNumItems = %i\n"      ,  tileS.tileNumItems);
      printf(", tileSize = (%i,%i)\n"     ,  tileS.tileSize.x, tileS.tileSize.y);
      printf(", bitmapSize = (%i,%i)\n"   ,  tileS.bitmapSize.x,  tileS.bitmapSize.y);
      printf(", internalDelta = (%i,%i)\n",  tileS.internalDelta.x,  tileS.internalDelta.y);
      printf(", threadDelta = (%i,%i)\n"  ,  tileS.threadDelta.x, tileS.threadDelta.y);
      printf(", tileIndex = %i\n"         ,  tileS.tileIndex);
      printf(", intHeight = %i\n"         ,  tileS.intHeight);
      printf(", floatHeight = %f\n"       ,  tileS.floatHeight);
      printf(", threadUnique = %i\n"      ,  tileS.threadUnique);
      printf(", column = %i\n"            ,  tileS.column);
      printf("}\n");
  }

  void traversalHs(Traversal t) {
      printf("TraversalHs\n");
      printf("{ travLeftControl = (%f, %f, %f, %f)\n"
            , t.travLeftControl.x
            , t.travLeftControl.y
            , t.travLeftControl.z
            , t.travLeftControl.w
            );
      printf(", travRight = (%f, %f)\n", t.travRight.x, t.travRight.y);
      printf(", travXPos = %f\n", t.travXPos);
      printf(", travIndex = %i\n", t.travIndex);
      printf("}\n");
  }

  // determine the current color based on the layer stack
  COLOR compositeLayers( PMEM    ShapeState *shS
                       , PMEM    ColorState *cS
                       ) {
      LOCALSUBSTANCE currentSubstance;
      COLOR baseColor = TRANSPARENT_COLOR;
      COLOR nextColor;
      LOCALSUBSTANCE lastSubstance = NULLINDEX;
      bool lastIsSet = false;
      LAYERID topLayer = 0;
      bool done = false;
      while (!done) {
          bool shouldComposite = true;
          topLayer = findTopLayer(shS, topLayer);
          DEBUG_IF(printf("topLayer %i ", topLayer);/*showLayerFlags(shS->layerFlags);*/printf("\n");)
          if (topLayer > shS->layerCount) {
              nextColor = cS->csBackgroundColor;
              done = true;
              shouldComposite = true;
              lastIsSet = true;
          }
          else {
              currentSubstance = layerSubstance(shS, topLayer);
              SUBSTANCETAG substanceTag = shS->substanceTagStack[currentSubstance];
              //DEBUG_IF(printf("topLayer %i substanceId %i lastSubstance %i ",
              //                 topLayer,   substanceId,   lastSubstance   );)
              lastIsSet = false; // layerIsAdditive(shS, topLayer);
              shouldComposite = (bool) (currentSubstance != lastSubstance);
              if (shouldComposite) {
                   //nextColor = (COLOR) (0.0,0.5,0.4,1.0);
                   nextColor = readColor ( cS
                                         , substanceTag
                                         );
              }
              // shouldComposite = true; // delete this
              lastSubstance = currentSubstance;
              topLayer += 1;
              done = false;
          }
          DEBUG_IF(printf("done %i lastIsSet %i shouldComposite %i baseColor %2.2v4f nextColor %2.2v4f \n",
                           done,   lastIsSet,   shouldComposite,   baseColor,        nextColor          );)
          if (shouldComposite) {
              if (lastIsSet) {
                  baseColor = composite(baseColor, nextColor);
                  if (OPAQUE(baseColor)) {
                     done = true;
                  }
              }
          }
          // done = true; // delete this
      }
      return baseColor;
      //return (COLOR)(0.0,0.0,0.5,1.0);




      LMEM addressParts[THREADSPERJOB];
      if (columnThread == 0) {
          int address = parallelScanInt( addressParts
                                       , inUse[blockThread]
                                       , blockDepth
                                       , columnThread
                                       );
          if (inUse[blockThread]) {
              outputBlockIds[address] = blockIds[blockThread];
              outputCanMerge[address] = canMerge[blockThread];
          }
          if(blockThread == 0) {
            outputSize[0] = addressParts[THREADSPERJOB-1]
          }
      }

      __kernel void checkSplitKernel
          ( GMEM      Slice *qSliceHeap
          , GMEM   TileInfo *tileHeap
          , GMEM      Slice *checkBlockSlices
          , GMEM        int *blockIdHeap
          ,             int  columnDepth
          ,            int2  bitmapSize
          ,             int  frameNumber
          ,             int  jobIndex
          ,             int  jobOffset
          , GMEM        int *maximumsPerTile
          ) {
          // This kernel is indexed by tile.
          int   tileId  = INDEX + jobOffset; // the sequential number of the tile in the current workgroup.
          int   columnThread = COLUMNTHREAD;
          LMEM  int parts[THREADSPERBLOCK];
          GMEM TileInfo *tileInfo = getTileInfo(tileHeap, tileId);
          TileState tileS;
          initTileState ( &tileS
                        ,  tileInfo
                        ,  bitmapSize
                        ,  tileId
                        ,  columnDepth
                        ,  columnThread
                        );
          // All we are doing is taking the sum of all the queue sizes for each column
          // of the blocks associated with the tile.
          int totalThresholds = 0;
          if (isActiveThread(&tileS, tileInfo)) {
              // Each tile has a slice of blocks.
              Slice blockSlice = checkBlockSlices[tileId];
              for (int i = 0; i < blockSlice.sLength; i++) {
                 // Each blockId is stored in an array, referenced from the slice.
                 int blockId = blockIdHeap[blockSlice.sStart + i];
                 Slice qSlice = loadQueueSlice(qSliceHeap, blockId, columnDepth, columnThread);
                 totalThresholds += qSlice.sLength;
              }
          }
          // Now we take the maximum sum from every column and store it.
          int max = parallelMaxInt( parts + (tileId << columnDepth)
                                  , totalThresholds
                                  , columnDepth
                                  , columnThread
                                  );

      }


      void parallelMax ( LMEM int *parts
                       ,      int  totalThresholdsPerColumn
                       ,      int  tileIndex
                       ,      int  columnDepth
                       ,      int  columnThread
                       , GMEM int *maximumsPerTile
                       ) {
         // Copy from global to local memory
         parts[columnThread] = totalThresholdsPerColumn;
         // Loop for computing localSums : divide WorkGroup into 2 parts
         for (int stride = 1<<(columnDepth - 1); stride>0; stride = stride >> 1) {
             // Waiting for each 2x2 addition into given workgroup
             barrier(CLK_LOCAL_MEM_FENCE);

             // Add elements 2 by 2 between local_id and local_id + stride
             if (columnThread < stride) {
                 parts[columnThread] = max(parts[columnThread], parts[columnThread + stride]);
             }
         }
         // Write result into partialSums[nWorkGroups]
         if (columnThread == 0) {
             maximumsPerTile[tileIndex] = parts[0];
         }
      }

void initColumnState ( PMEM  ColumnState *columnState
                     ,          TileInfo  tileInfo
                     ,              int2  bitmapSize
                     ,               int  blockId
                     ,               int  columnDepth
                     ,               int  columnThread
                     ) {
    columnState->columnThread  = columnThread;
    // 2^hDepth = the width of the tile (before being cropped)
    int hDepth = (int)tileInfo.tileHDepth;
    // 2^vDepth = the height of the tile (before being cropped)
    int vDepth = (int)tileInfo.tileVDepth;
    // The precropped tile has (2^hDepth)*(2^vDepth) pixels or 2^(hDepth + vDepth)
    // 2^columnDepth = the total number of threads available.
    // So the number of pixels per thread is 2^(hDepth + vDepth) / 2^(columnDepth)
    // aka the total number of pixels divided by the number threads.
    // which we refer to as the thread depth.
    // 2^threadDepth = 2^(hDepth + vDepth) / 2^(columnDepth) so:
    int threadDepth = max(0, vDepth + hDepth - columnDepth);
    // The precropped thread height is the height of the vertical column of pixels covered by the thread (2^threadDepth)
    int precroppedHeight = 1 << threadDepth;
    // The internal x coordinate of the start of the thread, that is in pixels relative to the top left corner of the tile.
    // is just the rightmost n bits of the columnThreadId, where n is hDepth
    // This is equivalent to internalX = columnThread % (uncropped tileWidth which equals 2^hDepth)
    int internalX = bitmaskN((int)hDepth) & columnThread;
    // The internal y coordinate of the start of the thread, that is in pixels relative to the top left corner of the tile
    // is just the tileId / (uncropped widthOf tile) * precroppedHeight
    int internalY = (columnThread >> hDepth) << threadDepth;
    // The columnDelta is the topleft corner of the column. The is equivalent to the start point of the thread relative to the output image.
    columnState->columnDelta   = (int2)(internalX, internalY) + boxLeftTop(tileInfo.tileBox);
    // The actual height of the thread after being cropped by the bitmap boundaries.
    columnState->intHeight     = min(precroppedHeight, bitmapSize.y-columnState->columnDelta.y);
    columnState->floatHeight   = convert_float(columnState->intHeight);
}


__kernel void mergeTileKernel
    ( GMEM       int4 *tileHeap
    , GMEM  THRESHOLD *thresholdHeap
    , GMEM     HEADER *headerHeap
    , GMEM      Slice *qSliceHeap
    , GMEM      Slice *mergeBlockSlices
    , GMEM        int *blockIds
    ,             int  columnDepth
    ,            int2  bitmapSize
    ,             int  frameNumber
    ,             int  jobIndex
    ,             int  jobOffset
    ) {
    // This kernel is indexed by each slice of blocks that need to be merged into one block.
    // The destination tiles are indexed in the same way.
    int sliceId      = jobOffset + INDEX; // the sequential number of the tile in the current workgroup.
    int columnThread = COLUMNTHREAD;
    Slice blockSlice = mergeBlockSlices[sliceId];
    int4 tileBox     = tileHeap[sliceId];
    float4 columnBox = initColumnBox(tileBox, bitmapSize, columnThread);
    if (isActiveThread(columnBox, bitmapSize)) {
        ThresholdQueue tQDestination;
        initThresholdQueue(&tQDestination,initQueueSlice());
        for (int i = 0; i < blockSlice.sLength; i++) {
            int blockIdSource = blockIds[blockSlice.sStart + 1];
            Slice qSliceSource = loadQueueSlice(qSliceHeap, blockIdSource, columnDepth, columnThread);
            ThresholdQueue tQSource;
            initThresholdQueue(&tQSource, qSliceSource);
            loadThresholdQueue(&tQSource, thresholdHeap, headerHeap, blockIdSource, columnDepth, columnThread);
            while (queueSize(&tQSource)) {
              THRESHOLD threshold = getThreshold(&tQSource, 0);
              HEADER       header = getHeader(&tQSource, 0);
              popTop(&tQSource);
              pushThreshold(&tQDestination, header, threshold);
            }
        }
        int blockIdDestination = blockIds[blockSlice.sStart];
        saveThresholdQueue( &tQDestination
                          ,  thresholdHeap
                          ,  headerHeap
                          ,  blockIdDestination
                          ,  columnDepth
                          ,  columnThread
                          );
        storeQueueSlice( qSliceHeap
                       , tQDestination.qSlice
                       , blockIdDestination
                       , columnDepth
                       , columnThread
                       );
    }
}
