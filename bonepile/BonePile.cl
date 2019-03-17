



float findSlicePoint ( PMEM ThresholdState *tS
                     ,                 int  limit
                     ) {
    THRESHOLD current = tS->thresholds[limit];
    int cursor = limit + 1;
    float slicePoint = tBottom(current);
    bool done = false;
    while (!done && cursor < tS->numThresholds) {
        THRESHOLD next = tS->thresholds[cursor];
        if (tTop(next) > tTop(current)) {
            slicePoint = min(slicePoint, tTop(next));
            done = true;
        }
        else {
            slicePoint = min(tBottom(next), slicePoint);
        }
      cursor += 1;
    }
    return slicePoint;
}



void splitThresholds ( PMEM ThresholdState *tS
                     , PMEM     ParseState *pS
                     ) {
    float lastSlicePoint = FLT_MAX;
    for (int limit = 0; limit < tS->numThresholds - 1; limit++) {
        HEADER currentHeader = tS->thresholdHeaders[limit];
        THRESHOLD current = tS->thresholds[limit];
        if (lastSlicePoint <= tTop(current)) {
            lastSlicePoint = FLT_MAX;
        }
        float slicePoint = min(findSlicePoint(tS, limit), lastSlicePoint);
        DEBUG_IF(printf("limit %i lSP %f fSP %f ",limit, lastSlicePoint, slicePoint);showThreshold(currentHeader,current);printf("\n");)
        if (slicePoint < tBottom(current) && slicePoint > tTop(current)) {
            HEADER splitHeader;
            THRESHOLD split;
            splitThreshold( &currentHeader
                          , &current
                          , &splitHeader
                          , &split
                          ,  slicePoint
                          );
            DEBUG_IF(printf("               current ");showThreshold(currentHeader,current);printf("\n");)
            DEBUG_IF(printf("               split   ");showThreshold(splitHeader,split);printf("\n");)
            tS->thresholdHeaders[limit] = currentHeader;
            tS->thresholds[limit] = current;
            if (tKeep(splitHeader,split)) {
                slotThreshold(tS, pS, splitHeader, split);
            }

        }
        lastSlicePoint = tBottom(current);
    }
}
















    int topMask;
    COLOR color = TRANSPARENT_COLOR;
    COLOR nextColor;
    bool moreBelow;
    SHAPEINDEX lastShape = 0xFFFFFFFFFFFFFFFF;
    bool lastIsContinue = true;
    DEBUG_IF(printf("determineColor maskStack: %lX \n", maskStack);)
    do {
        bool skip = false;
        topMask = FIND_TOP(maskStack);
        DEBUG_IF(printf("topMask %i \n", topMask);)
        if (topMask < 0) {
            nextColor = cS->csBackgroundColor;
        }
        else {
            int ref = pS->maskIndices[topMask];
            MASKINDEX maskIndex = maskRefs[ref];
            Mask mask = maskHeap[maskIndex];
            SHAPEINDEX shapeId = shapeTagShapeIndex(mask.maskShapeTag);
            SHAPETAG combineType = shapeTagCombineType(mask.maskShapeTag);
            if (shapeId != lastShape) {
                nextColor = readColor ( cS
                                      , shapes
                                      , shapeId
                                      , shapeTagIsSolidColor(mask.maskShapeTag)
                                      );
                lastShape = shapeId;
                lastIsContinue = shapeTagIsContinue(mask.maskShapeTag);
            }
            else {
              skip = !lastIsContinue;
            }
        }
        //DEBUG_IF(printf("color: %2.2v4f nextColor: %2.2v4f \n", color, nextColor);)
        if (!skip) {
            color = composite(color, nextColor);
            moreBelow = NOT_OPAQUE(color) && topMask >= 0;
            if (moreBelow) {
                maskStack = REMOVE_SHAPE(topMask, maskStack); // remove that shape in case we loop back
            }
        }
        else {
          moreBelow = topMask >= 0;
        }

    } while (moreBelow);
    //DEBUG_IF(printf("color %2.2v4f \n", color);)
    return color;
}


inline void passBrushTop(               HEADER  header
                        , __private ParseState *pS
                        );

inline void passBrushBottom(               HEADER  header
                           , __private ParseState *pS
                           );

inline void passBrush(               HEADER  header
                     , __private ParseState *pS
                     );

inline void passBrushTop(               HEADER  header
                        , __private ParseState *pS
                        ) {
    if (headerPersistTop(header)) {
        passBrush(header, pS);
    }
}

inline void passBrushBottom(               HEADER  header
                           , __private ParseState *pS
                           ) {
    if (headerPersistBottom(header)) {
        passBrush(header, pS);
    }
}

inline void passBrush(               HEADER  header
                     , __private ParseState *pS
                     ) {
    pS->initialBrush = flipBrush(pS->initialBrush, header);
}


        HEADER  initialBrush;       // holds the brush state of thresholds added above the top of the tile.

// Brush Values

// A 'brush' refers to an 8 bit value that stores what a threshold represents with regards to on the fly rasterization
// of compound shapes. A subshape can either be addititive, subractive or invert the previous shape.
// As new shapes are added to the threshold pile the compoundCount is increased, this determines what bit will be set in the brush value.
// The bottom for bits are additive shapes, the top four bits are subtractive.
// Because we need one bit for each shape and the bit either needs to be in the top or the bottom half of the byte,
// brush values allow 4 mask shapes to properly be combined before the kernel must pass over the thresholds in local memory and compress down the brush
// values using the function compressBrushValues.
#define MASKTAG     ulong

inline HEADER getHeaderBrush(HEADER h) {return h & SHAPEBIT_MASK;} // get the brush style of the threshold
inline HEADER setHeaderBrush(HEADER h, HEADER brush) {return (h & WITHOUT_SHAPEBIT) | INBRUSHMODE | brush;} //(*thresholdHeader).s1 = brush;}   // set the brush style of the threshold

#define MAXCOMPOUND 14
#define REMOVE_HIGH_HALF_BYTE    0x3FFF

// This means most simple jobs with fewer than 4 compounds per column and fewer than 8 shapes per tile can be rasterized very fast and more complex jobs can
// be rasterized with just a few extra passes over local memory.

// the bits in the lower bytes represent additive shapes.
#define NOBRUSH                  0x0
#define NEWBRUSH                 0x1
// Bits in the lower half of the byte represent additive shapes.
#define ADDBRUSH(compoundCount) (0x1 << compoundCount)
// Bits in the higher bytes represent subtractive shapes.
#define SUBBRUSH(compoundCount) (0x1 << (compoundCount + MAXCOMPOUND));
// The rasterizer then traverses the thresholds by xoring it's mask value with each brush value.
// It can determine weather it is inside or outside of the compound mask by comparing the value of the higher bits to the
// lower bits.
inline bool brushValueIsOn(HEADER v) {
    HEADER b = v & SHAPEBIT_MASK;
    return ((b & REMOVE_HIGH_HALF_BYTE) > (b >> MAXCOMPOUND));
}

inline HEADER flipBrush(original,modifier) {
    return ((original & WITHOUT_SHAPEBIT) | ((original & SHAPEBIT_MASK) ^ (modifier & SHAPEBIT_MASK)));
}

#define BRUSHSTYLE_CONTINUE 0x1 // add new thresholds to the same brush level.
#define BRUSHSTYLE_ADD      0x2 // the next shape is additive
#define BRUSHSTYLE_SUB      0x3 // the next shape is subtractive

// make a brush value from a shapetag and compound index
inline HEADER makeBrush(MASKTAG style, int compoundIndex) {
    HEADER brush;
    //DEBUG_IF(printf("style: %X\n",style);)
    switch (style) {
        case BRUSHSTYLE_SUB:
            brush = SUBBRUSH(compoundIndex);
            break;
        default:
            brush = ADDBRUSH(compoundIndex);
            break;
    }
    return brush | INBRUSHMODE;
}

// Reset counters for tracking multiple compounds
void resetCompoundState ( __private  ParseState *pS
                        ,                   int *compoundCount
                        ) {
  pS->initialBrush = NOBRUSH;
  *compoundCount  = 0; // number of masks so far the certain compound shape
}


// Advance to the next compound shape
void nextCompound(__private ThresholdState  *tS
                 ,__private      ParseState *pS
                 ,                     Mask  mask
                 ,__private             int *compoundCount
                 ,               SHAPEINDEX  shapeIndex
                 ) {
    if (mask.maskCombineType != BRUSHSTYLE_CONTINUE) { // if this is not a continuation, make it a new compound shape
        (*compoundCount)++;
        if (*compoundCount >= MAXCOMPOUND) { // this means once compoundCount reaches 4, compress the compounds and start from 1
            compressBrushes(tS, pS);
            *compoundCount = 1; // start from left shifting the brush 1 since 0 is still taken.
        }
    }
}


// compress a brush stack value
void compressBrushes( __private ThresholdState *tS
                    , __private     ParseState *pS
                    ) {
    HEADER currentBrushV = pS->initialBrush;
    HEADER currentBrushH = currentBrushV;
    bool isOn = brushValueIsOn(currentBrushV);
    pS->initialBrush = setHeaderBrush(pS->initialBrush, (HEADER) isOn);
    //DEBUG_IF(printf("pS->initialBrush %X pS->vertShapeStack %X isOn %i\n", pS->initialBrush, pS->vertShapeStack, isOn);)
    HEADER lastPersistHeader = 0x0;
    for (int cursor = 0; cursor < tS->numThresholds; cursor++) {
        HEADER h = tS->thresholdHeaders[cursor];
        //DEBUG_IF(printf("cursor %i h: %X currentBrushV %X currentBrushH %X isOn %i\n", cursor, getHeaderBrush(h), currentBrushV, currentBrushH, isOn);)
        if (headerInBrushMode(h)) {
            if (headerPersistEither(h)) {
                if (headerPersistBottom(lastPersistHeader)) {
                    //DEBUG_IF(printf("headerPersistBottom(lastPersistHeader)\n");)
                    currentBrushV = flipBrush(currentBrushV,lastPersistHeader);
                }
                if (headerPersistTop(h)) {
                    //DEBUG_IF(printf("headerPersistTop(h)\n");)
                    currentBrushV = flipBrush(currentBrushV,h);
                }
                lastPersistHeader = h;
                currentBrushH = currentBrushV;
                isOn = brushValueIsOn(currentBrushV);
            }
            currentBrushH = flipBrush(currentBrushH, h);
            bool nextIsOn = brushValueIsOn(currentBrushH);
            bool enable = isOn ^ nextIsOn;
            //DEBUG_IF(printf("cursor %i h: %X currentBrushV %X currentBrushH %X isOn %i nextIsOn %i ---> enable %i\n", cursor, getHeaderBrush(h), currentBrushV, currentBrushH, isOn, nextIsOn, enable);)
            isOn = nextIsOn;
            tS->thresholdHeaders[cursor] = setHeaderBrush(h, (HEADER)enable);
        }
    }
}


void convertBrushesToShapeBits ( __private ThresholdState *tS
                               , __private     ParseState *pS
                               ,               SHAPEINDEX  shapeIndex
                               ) {
    HEADER currentBrushV = pS->initialBrush;
    HEADER currentBrushH = currentBrushV;
    bool isOn = brushValueIsOn(currentBrushV);
    //DEBUG_IF(printf("convert shapeIndex %i pS->vertShapeStack %lX isOn %i\n", shapeIndex, pS->vertShapeStack, isOn);)
    updateShapeStack(shapeIndex, &(pS->vertShapeStack), isOn);
    //DEBUG_IF(printf("convert shapeIndex %i pS->vertShapeStack %lX isOn %i\n", shapeIndex, pS->vertShapeStack, isOn);)
    HEADER lastPersistHeader = 0x0;
    for (int cursor = 0; cursor < tS->numThresholds; cursor++) {
        HEADER h = tS->thresholdHeaders[cursor];
        //DEBUG_IF(printf("cursor %i h: %X currentBrushV %X currentBrushH %X isOn %i\n", cursor, getHeaderBrush(h), currentBrushV, currentBrushH, isOn);)
        if (headerInBrushMode(h)) {
            if (headerPersistEither(h)) {
                if (headerPersistBottom(lastPersistHeader)) {
                    //DEBUG_IF(printf("headerPersistBottom(lastPersistHeader)\n");)
                    currentBrushV = flipBrush(currentBrushV,lastPersistHeader);
                }
                if (headerPersistTop(h)) {
                    //DEBUG_IF(printf("headerPersistTop(h)\n");)
                    currentBrushV = flipBrush(currentBrushV,h);
                }
                lastPersistHeader = h;
                currentBrushH = currentBrushV;
                isOn = brushValueIsOn(currentBrushV);
            }
            currentBrushH = flipBrush(currentBrushH, h);
            bool nextIsOn = brushValueIsOn(currentBrushH);
            bool enable = isOn ^ nextIsOn;
            //DEBUG_IF(printf("cursor %i h: %X currentBrushV %X currentBrushH %X isOn %i nextIsOn %i ---> enable %i\n", cursor, getHeaderBrush(h), currentBrushV, currentBrushH, isOn, nextIsOn, enable);)
            isOn = nextIsOn;
            tS->thresholdHeaders[cursor] = setEnableBit(h, shapeIndex, enable);
        }
    }
}











            float cSlope = thresholdInvertedSlope(currentHeader, current);
            float nSlope = thresholdInvertedSlope(nextHeader, next);
            if (cSlope > nSlope) {
              DEBUG_IF(printf("-------- checkIntersection --------\n");)
              //DEBUG_IF(printf("current cSlope %f topX %f ", cSlope, tTopX(currentHeader, current));showThreshold(currentHeader, current);printf("\n");)
              //DEBUG_IF(printf("next    nSlope %f topX %f ", nSlope, tTopX(nextHeader,    next   ));showThreshold(nextHeader, next);printf("\n");)
              //float c = tTopX(currentHeader, current) - (cSlope * tTop(current));
              //float d = tTopX(nextHeader,    next   ) - (nSlope * tTop(next)   );
              //// see wikipedia line-line intersection for the derivation of this equation.
              //// https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
              //float intersectY = (cSlope * d - nSlope * c) / (cSlope - nSlope);
              //DEBUG_IF(printf("cSlope %f > nSlope %f  c %f d %f intersectY %f\n", cSlope, nSlope, c, d, intersectY);)


void fourWaySplit( __private     HEADER *aTopHeader
                 , __private  THRESHOLD *aTop
                 , __private     HEADER *aBottomHeader
                 , __private  THRESHOLD *aBottom
                 , __private     HEADER *bTopHeader
                 , __private  THRESHOLD *bTop
                 , __private     HEADER *bBottomHeader
                 , __private  THRESHOLD *bBottom
                 ,                HALF   splitY
                 );

void fourWaySplit(    HEADER *aTopHeader
                 , THRESHOLD *aTop
                 ,    HEADER *aBottomHeader
                 , THRESHOLD *aBottom
                 ,    HEADER *bTopHeader
                 , THRESHOLD *bTop
                 ,    HEADER *bBottomHeader
                 , THRESHOLD *bBottom
                 ,      HALF  splitY
                 ) {
    HALF splitX = encodeHalf(thresholdIntersectX(*aTopHeader, *aTop, decodeHalf(splitY)));
    divideThreshold( aTopHeader
                   , aTop
                   , aBottomHeader
                   , aBottom
                   , splitX
                   , splitY
                   );
    divideThreshold( bTopHeader
                   , bTop
                   , bBottomHeader
                   , bBottom
                   , splitX
                   , splitY
                   );
    //DEBUG_IF(printf("4WaySplit\n    ");\
    //         showThreshold(*aTopHeader, *aTop);\
    //         printf("\n    ");\
    //         showThreshold(*aBottomHeader, *aBottom);\
    //         printf("\n    ");\
    //         showThreshold(*bTopHeader, *bTop);\
    //         printf("\n    ");\
    //         showThreshold(*bBottomHeader, *bBottom);\
    //         printf("\n");\
    //        )
}

void simpleStore( __private ThresholdState *tS
                ,          HEADER  thresholdHeader
                ,            THRESHOLD  threshold
                ,                      int  cursor
                ) {
    tS->thresholdHeaders[cursor] = thresholdHeader;
    tS->thresholds[cursor] = threshold;
}

void simpleStore( __private ThresholdState *tS
                ,                   HEADER  thresholdHeader
                ,                THRESHOLD  threshold
                ,                      int  cursor
                );



inline void attemptToStore ( __private  ThresholdState *tS
                           , __private      ParseState *pS
                           ,                    HEADER  thresholdHeader
                           ,                 THRESHOLD  threshold
                           ,                       int  cursor
                           );

void holdIntersecting( __private  ThresholdState *tS
                     , __private      ParseState *pS
                     , __private          HEADER *newHeader
                     , __private       THRESHOLD *new
                     , __private          HEADER *oldHeader
                     , __private       THRESHOLD *old
                     , __private             int *holding
                     ) {
    float topDiff       = decodeHalf(tTopX(*newHeader, *new))    - decodeHalf(tTopX(*oldHeader, *old)   );
    float bottomDiff    = decodeHalf(tBottomX(*newHeader, *new)) - decodeHalf(tBottomX(*oldHeader, *old));
    float segmentTop    = decodeHalf(tTop(*new));
    float segmentHeight = decodeHalf(tBottom(*new)) - segmentTop;
    float ratio = fabs(topDiff) / (fabs(topDiff) + fabs(bottomDiff));
    float splitY = (ratio * segmentHeight) + segmentTop;
       HEADER newSplitHeader; // this is used to temporarily store the bottom half of a threshold header.
    THRESHOLD newSplit;       // this is used to temporarily store the bottom half of a threshold.
       HEADER oldSplitHeader; // this is used to temporarily store the bottom half of a threshold header.
    THRESHOLD oldSplit;       // this is used to temporarily store the bottom half of a threshold.
    // split the old threshold first
    fourWaySplit(  newHeader
                ,  new
                , &newSplitHeader
                , &newSplit
                ,  oldHeader
                ,  old
                , &oldSplitHeader
                , &oldSplit
                ,  encodeHalf(splitY)
                );
    //                     new   old
    //                       *   *
    //                        \ /
    //                         *
    //        first queued->  / \ <- second queued
    //                       *   *
    // replace the old threshold with the top of the trimmed version of the old threshold.
    // queue the bottom portion of the old threshold.
    attemptToHold(tS, pS, oldSplitHeader, oldSplit, holding);
    // queue the bottom of the new threshold.
    attemptToHold(tS, pS, newSplitHeader, newSplit, holding);
    // the top of the new threshold is still in &newHeader so keep it there for now and since done is false
    // compare it to the next threshold at thresholds[cursor-2]
}

int  splitAndHold ( __private  ThresholdState *tS
                  , __private      ParseState *pS
                  , __private          HEADER *toSplitHeader
                  , __private       THRESHOLD *toSplit
                  , __private             int *holding
                  ,                      HALF  y
                  ) {
    HEADER toHoldHeader;
    THRESHOLD toHold;
    splitThreshold(  toSplitHeader
                  ,  toSplit
                  , &toHoldHeader
                  , &toHold
                  ,  y
                  );
    return attemptToHold(tS, pS, toHoldHeader, toHold, holding);
}


void insideStore( __private  ThresholdState *tS
                , __private      ParseState *pS
                ,                    HEADER  thresholdHeader
                ,                 THRESHOLD  threshold
                ,                       int  cursor
                ) {
    if (cursor < MAXTHRESHOLDS && pS->shapesAvailable) {
        tS->thresholdHeaders[cursor] = thresholdHeader;
        tS->thresholds[cursor] = threshold;
    }
    else {
        adjustToExclude(pS, threshold);
    }
}

inline void attemptToStore ( __private  ThresholdState *tS
                           , __private      ParseState *pS
                           ,                    HEADER  thresholdHeader
                           ,                 THRESHOLD  threshold
                           ,                       int  cursor
                           ) {
    insideStore( tS
               , pS
               , thresholdHeader
               , threshold
               , cursor
               );
    //DEBUG_IF(printf("STORE %i ", cursor);)//showThresholdGeo(threshold);printf("rBot %f \n",decodeHalf(pS->renderEnd.y));)
}

inline int  attemptToHold ( __private ThresholdState *tS
                          , __private     ParseState *pS
                          ,                   HEADER  thresholdHeader
                          ,                THRESHOLD  threshold
                          ,                      int *holding
                          ) {
    int end = tS->numThresholds + (*holding) + 1; // this is the location of the slot after the last slot holding a threshold
    insideStore( tS
               , pS
               , thresholdHeader
               , threshold
               , end
               );
    //if (end < MAXTHRESHOLDS) {
      *holding += 1;
    //}
    return end;
    //DEBUG_IF(printf("HOLD %i", end);)//showThresholdGeo(threshold);printf("rBot %f \n",decodeHalf(pS->renderEnd.y));)
}

void slotThreshold ( __private ThresholdState *tS
                   , __private     ParseState *pS
                   ,                   HEADER  newHeader
                   ,                THRESHOLD  new
                   ) {
    //DEBUG_IF(printf("slot --> new ");showThresholdGeo(new);printf("\n");)
    int cursor = tS->numThresholds;
    bool done = false;
    while (cursor > 0 && !done) {
        HEADER oldHeader;
        THRESHOLD old;
        oldHeader = tS->thresholdHeaders[cursor-1];
        old = tS->thresholds[cursor-1];
        HEADER store;
        THRESHOLD store;
        DEBUG_IF(printf("while --> cursor %i\n", cursor);)
        DEBUG_IF(printf("before --> new ");showThresholdGeo(new);printf("\n");)
        //DEBUG_IF(printf("before --> old ");showThresholdGeo(old);printf("\n");)
        bool oldChanged = false;
        float topCompare    = fixNegativeZero(decodeHalf(tTop(new)   ) - decodeHalf(tTop(old)   ));
        float bottomCompare = fixNegativeZero(decodeHalf(tBottom(new)) - decodeHalf(tBottom(old)));
        float topBotCompare = fixNegativeZero(decodeHalf(tTop(new)   ) - decodeHalf(tBottom(old)));
        float botTopCompare = fixNegativeZero(decodeHalf(tBottom(new)) - decodeHalf(tTop(old)   ));
        DEBUG_IF(printf("        topCompare %f bottomCompare %f topBotCompare %f botTopCompare %f %i \n", topCompare, bottomCompare, topBotCompare, botTopCompare,fabs(bottomCompare) < 0.0000001  );)
        if(fabs(topCompare) <= MINCROP) {
            //DEBUG_IF(printf("++ (fabs(topCompare) <= MINCROP)\n");)
            new = setTop(new, tTop(old));
        }
        if(fabs(bottomCompare) <= MINCROP) {
            //DEBUG_IF(printf("++ (fabs(bottomCompare) <= MINCROP)\n");)
            new = setBottom(new, tBottom(old));
        }
        if ((topBotCompare < 0) && (topBotCompare > (-MINCROP))) {
            //      |
            //  * ~ |
            //  | ~ *
            //  |
            // new old
            new = setTop(new,tBottom(old));
            DEBUG_IF(printf("        ++ ((topBotCompare < 0) && (topBotCompare > (-MINCROP)))\n");)
        }
        if ((botTopCompare > 0) && (botTopCompare < MINCROP)) {
            //  |
            //  | ~ *
            //  * ~ |
            //      |
            // new old
            DEBUG_IF(printf("        ++ ((botTopCompare > 0) && (botTopCompare < MINCROP))\n");)
            new = setBottom(new, tTop(old));
        }
        if ((bottomCompare > MINCROP) && (topBotCompare < (-MINCROP))) {
           //  *   |
           //  |   |
           //  |   *
           //  |
           //  *
           // new old
           int hold = splitAndHold(tS, pS, &newHeader, &new, holding, tBottom(old));
           DEBUG_IF(printf("        %i ((bottomCompare > MINCROP) && (topBotCompare < (-MINCROP)))\n", hold);)
        }
        if ((bottomCompare < (-MINCROP)) && (botTopCompare > MINCROP)) {
            //  |   *
            //  |   |
            //  *   |
            //      |
            //      *
            // new old
            int hold = splitAndHold(tS, pS, &oldHeader, &old, holding, tBottom(new));
            DEBUG_IF(printf("        %i ((bottomCompare < (-MINCROP)) && (botTopCompare > MINCROP))\n", hold);)
            oldChanged = true;
        }
        if ((topCompare > MINCROP) && (topBotCompare < (-MINCROP))) {
            //      *
            //      |
            //  *   |
            //  |   |
            //  |   *
            //  |
            // new old
            int hold = splitAndHold(tS, pS, &oldHeader, &old, holding, tTop(new));
            DEBUG_IF(printf("        %i ((topCompare > MINCROP) && (topBotCompare < (-MINCROP)))\n", hold);)
            oldChanged = true;
        }
        if ((topCompare < (-MINCROP)) && (botTopCompare > MINCROP)) {
            //  *
            //  |
            //  |   *
            //  |   |
            //  *   |
            // new old
            int hold = splitAndHold(tS, pS, &newHeader, &new, holding, tTop(old));
            DEBUG_IF(printf("        %i ((topCompare < (-MINCROP)) && (botTopCompare > MINCROP))\n", hold);)
        }
        if (tTop(new) == tTop(old) && tBottom(new) == tBottom(old)) {
            //DEBUG_IF(printf("tTop(new) == tTop(old)\n");)
            //DEBUG_IF(printf("tTopX(newHeader, new) %f tTopX(oldHeader, old) %f\n", decodeHalf(tTopX(newHeader, new)), decodeHalf(tTopX(oldHeader, old)));)
            //DEBUG_IF(printf("tBottomX(newHeader, new) %f tBottomX(oldHeader, old) %f\n", decodeHalf(tBottomX(newHeader, new)), decodeHalf(tBottomX(oldHeader, old)));)
            HALF newTopX    = tTopX(newHeader, new);
            HALF newBottomX = tBottomX(newHeader, new);
            HALF oldTopX    = tTopX(oldHeader, old);
            HALF oldBottomX = tBottomX(oldHeader, old);
            bool newTopIsLeft = halfLT(newTopX, oldTopX);
            bool newBottomIsLeft = halfLT(newBottomX, oldBottomX);
            if (newTopIsLeft != newBottomIsLeft && newTopX != oldTopX && newBottomX != oldBottomX) {
                //DEBUG_IF(printf("intersecting\n");)
                holdIntersecting( tS
                                , pS
                                , &newHeader
                                , &new
                                , &oldHeader
                                , &old
                                , holding
                                );
                oldChanged = true;
            }
            done = !newTopIsLeft;
            //DEBUG_IF(printf("newIsBefore %i\n", newIsBefore);)
        }
        else {
          done = halfGTE(tTop(new),tBottom(old));
        }
        if (oldChanged) {
            simpleStore(tS, oldHeader, old, cursor-1);
        }
        cursor = done ? cursor : cursor - 1;
        DEBUG_IF(printf("end ----> cursor %i done %i\n", cursor, done);)
    }
    DEBUG_IF(printf("after --> cursor %i done %i numThresholds %i\n", cursor, done, tS->numThresholds);)
    //DEBUG_IF(printf("after --> new ");showThresholdGeo(new);printf("\n");)
    //DEBUG_IF(printf("after --> old ");showThresholdGeo(old);printf("\n");)
    float height = fixNegativeZero(decodeHalf(tBottom(new)) - decodeHalf(tTop(new)));
    if (headerPersistEither(newHeader) || height > MINCROP) {
          int target = cursor;
          cursor = tS->numThresholds - 1;
          while (cursor >= target) {
              //DEBUG_IF(printf("Move: cursor %i\n", cursor);)
              if (cursor+1 < MAXTHRESHOLDS) {
                tS->thresholdHeaders[cursor+1] = tS->thresholdHeaders[cursor];
                tS->thresholds[cursor+1] = tS->thresholds[cursor];
              }
              else {
                adjustToExclude(pS, tS->thresholds[cursor]);
              }
              cursor -= 1;
          }
          attemptToStore(tS, pS, newHeader, new, target);
          tS->numThresholds += 1;
      }
}

      /*
      if (halfGT(tBottom(new), tBottom(old))) {
          //DEBUG_IF(printf("splitOffBottom\n");)
          splitAndHold(tS, pS, &newHeader, &new, holding, tBottom(old));
      }
      if (halfLT(tTop(new), tTop(old)) && halfGT(tBottom(new),tTop(old))) {
          //DEBUG_IF(printf("splitOffTop\n");)
          splitAndHold(tS, pS, &newHeader, &new, holding, tTop(old));
      }
      //DEBUG_IF(printf("----> new ");showThreshold(newHeader, new);printf("\n");)
      //DEBUG_IF(printf("----> old ");showThreshold(oldHeader, old);printf("\n");)
      if (halfGT(tBottom(old), tBottom(new)) && halfLT(tTop(old), tBottom(new))) {
          //DEBUG_IF(printf("splitOffBottom\n");)
          splitAndHold(tS, pS, &oldHeader, &old, holding, tBottom(new));
      }
      if (halfLT(tTop(old), tTop(new)) && halfGT(tBottom(old),tTop(new))) {
          //DEBUG_IF(printf("splitOffTop\n");)
          splitAndHold(tS, pS, &oldHeader, &old, holding, tTop(new));
      }
      //DEBUG_IF(printf("----> new ");showThreshold(newHeader, new);printf("\n");)
      //DEBUG_IF(printf("----> old ");showThreshold(oldHeader, old);printf("\n");)
      */



void splitAgainstOther ( __private  ThresholdState *tS
                       , __private      ParseState *pS
                       , __private          HEADER *splitHeader
                       , __private       THRESHOLD *split
                       , __private       THRESHOLD  other
                       , __private             int *holding
                       );

void splitAgainstOther ( __private  ThresholdState *tS
                       , __private      ParseState *pS
                       , __private          HEADER *splitHeader
                       , __private       THRESHOLD *split
                       , __private       THRESHOLD  other
                       , __private             int *holding
                       ) {
    //DEBUG_IF(printf("splitAgainstOther\n    ");\
    //         showThresholdGeo(*split);\
    //         printf("\n    ");\
    //         showThresholdGeo( other);\
    //         printf("\n");\
    //        )
    if (halfGT(tBottom(*split), tBottom( other)) && halfLT(tTop(*split), tBottom(other))) {
    //    DEBUG_IF(printf("splitOffBottom\n");)
        splitAndHold(tS, pS, splitHeader, split, holding, tBottom(other));
    }
    if (halfLT(tTop(*split), tTop(other)) && halfGT(tBottom(*split),tTop(other))) {
    //    DEBUG_IF(printf("splitOffTop\n");)
        splitAndHold(tS, pS, splitHeader, split, holding, tTop(other));
    }
}

inline float decodeHalf(HALF x) {
  return vload_half(0,(__private half*)&x));
}

inline float2 decodeHalf2(HALF2 x) {
  return (float2)(0,0); // vload_half2(0,(__private half*)&x);
}

inline float4 decodeHalf4(HALF4 x) {
  return (float4)(0,0,0,0); //vload_half4(0,(__private half*)&x);
}

inline HALF encodeHalf(float x) {
    //HALF var;
    //vstore_half(x, 0, (__private half *)&var);
    return 0; //var;
}

inline bool halfGT(HALF a, HALF b) {
  return decodeHalf(a) > decodeHalf(b);
}

inline bool halfGTE(HALF a, HALF b) {
  return decodeHalf(a) >= decodeHalf(b);
}

inline bool halfLT(HALF a, HALF b) {
  return decodeHalf(a) < decodeHalf(b);
}

inline bool halfLTE(HALF a, HALF b) {
  return decodeHalf(a) <= decodeHalf(b);
}

inline HALF halfMin(HALF a, HALF b) {
  return halfLT(a,b) ? a : b;
}

inline HALF halfMax(HALF a, HALF b) {
  return halfGT(a,b) ? a : b;
}


void buildThresholds ( __private  ThresholdState *tS
                     , __private      ParseState *pS
                     ,                SHAPEINDEX  shapeIndex
                     ,                     BRUSH  brush
                     , GMEM               float2 *strandHeap
                     ,                       int  currentSize
                     ,                      Mask  mask
                     ,                     float  xPos_L
                     ,                    float2  right_L
                     ,                    float4  leftControl_L
                     ,                     float  xPos_R
                     ,                    float2  right_R
                     ,                    float4  leftControl_R
                     ,                      bool  touchingLeftBorder
                     ) {
    //DEBUG_IF(printf("before xPos_L %f left_L: %v2f control_L: %v2f right_L: %v2f xPos_R %f left_R: %v2f control_R: %v2f right_R: %v2f\n" \
    //               ,        xPos_L,   left_L,      control_L,      right_L,      xPos_R,   left_R,      control_R,      right_R       );)
    float y_L; // this is the y value
    if (eqT(leftX_L,rightX_L)) { // the left segment is vertical
        if (!eqT(leftY_L,rightY_L)  // vertical segments with no height are ignored
            && xPos_L > LEFTBORDER  // if the vertical segment is inside the pixel.
            ) {
            // add the threshold to the current state or buffer.
            addThreshold (  tS
                         ,  left_L
                         ,  right_L
                         ,  shapeIndex
                         ,  brush
                         ,  pS
                         ,  false
                         ,  0
                         );
        }
        y_L = rightY_L; // the left side of the center threshold is the top of the left wing.
    }
    else {
        if (eqT(leftX_L,xPos_L)) { // if the left side is the left edge of the strand, don't bifurcate.
          y_L = leftY_L;
        }
        else { // bifurcate the curve to find the left side of the segment
          bifurcateCurve(xPos_L, &leftControl_L, &right_L);
          y_L = yIntercept(left_L, right_L, xPos_L);
        }
    }
    float y_R;
    if ( eqT(leftX_R,  rightX_R) ) { // this means there is a vertical threshold on the right
        //DEBUG_IF(printf("(leftY_R %f != rightY_R %f) %i\n", leftY_R, rightY_R, leftY_R != rightY_R);)
        if (   !eqT(leftY_R, rightY_R) // vertical segments with no height are ignored
            && xPos_R < RIGHTBORDER // if the vertical segment is inside the pixel.
           ) {
             // add the threshold to the current state or buffer.
             addThreshold (  tS
                          ,  right_R
                          ,  left_R
                          ,  shapeIndex
                          ,  brush
                          ,  pS
                          ,  false
                          ,  1
                          );
        }
        y_R = leftY_R;  // the right side of the center threshold is the top of the right wing.
    }
    else {
        if (eqT(rightX_R,xPos_R)) { // if the right side is the right side of the strand, don't bifurcate.
          y_R = rightY_R;
        }
        else { // bifurcate the curve to find the right side of the center segment
          bifurcateCurve(xPos_R, &leftControl_R, &right_R);
          y_R = yIntercept(left_R, right_R, xPos_R);
        }
    }
    //DEBUG_IF(printf("after xPos_L %f left_L: %v2hlf control_L: %v2hlf right_L: %v2hlf xPos_R %f left_R: %v2hlf control_R: %v2hlf right_R: %v2hlf\n"\
    //                 ,       xPos_L,   left_L,        control_L,        right_L,        xPos_R,   left_R,        control_R,        right_R        );)
    //DEBUG_IF(printf("y_L %f  y_R: %f\n", y_L, y_R);)
    //DEBUG_IF(printf("(rightX_R %f == xPos_R %f) %i \n", rightX_R, xPos_R, (rightX_R == xPos_R));)
    // add the center threshold
    addThreshold (  tS
                 ,  (float2) (xPos_L, y_L)
                 ,  (float2) (xPos_R, y_R)
                 ,  shapeIndex
                 ,  brush
                 ,  pS
                 ,  touchingLeftBorder
                 ,  2
                 );
}




void addToLeft ( __private  ThresholdState *tS
               , __private      ParseState *pS
               , __private          HEADER *newHeader
               , __private       THRESHOLD *new
               , __private          HEADER *oldHeader
               , __private       THRESHOLD *old
               ,                       int  cursor
               ,                       int *holding
               ,                      bool *done
               ,                      bool  newIsLeft
               ) {

    splitAgainstOther(tS, pS, newHeader, new, old, holding, false);
    if (halfLT(tTop(*old),tBottom(*new)) && halfLT(tTop(*new),tBottom(*old))) {
        splitAgainstOther(tS, pS, oldHeader, old, new, holding, true);
    }
    *done = newIsLeft == halfGT(tTop(*new), tTop(*old));
    DEBUG_IF(printf("newIsLeft %i == halfGT(tTop(*new), tTop(*old)) %i : %i \n", newIsLeft, halfGT(tTop(*new), tTop(*old)), newIsLeft == halfGT(tTop(*new), tTop(*old)));)
                                       // done is true if both are true or both are false.
    int placeForOld = *done ? cursor-1 : cursor; // position to store old threshold after modification.
    attemptToStore(tS, pS, *oldHeader, *old, placeForOld);
}


void leftOrRightInside ( __private  ThresholdState *tS
                       , __private      ParseState *pS
                       ,                    HEADER *newHeader
                       ,             THRESHOLD *new
                       ,                    HEADER *oldHeader
                       ,             THRESHOLD *old
                       ,                       int  cursor
                       , __private             int *holding
                       , __private            bool *newIsLeft
                       ) {
    // Determine which threshold is on the left and if the intersect.
    // If either the tops or bottoms are equal we do some extra math but it doesn't matter.
    bool newIsHigher = halfLTE(tTop(*new), tTop(*old));
    HEADER *intersectTopHeader = newIsHigher ? newHeader : oldHeader; // Which threshold header do we need to calculate the xIntersect on.
    THRESHOLD   *intersectTop = newIsHigher ? new : old; // Which threshold do we need to calculate the xIntersect on.
    float cutTop = decodeHalf(newIsHigher ? tTop(*old) : tTop(*new)); // Which top do we find the intersect on.
    float intersectTopX = thresholdIntersectX(*intersectTopHeader, *intersectTop, cutTop); // Calculate the intersect.
    float topDiff = newIsHigher ? intersectTopX - decodeHalf(tTopX(*oldHeader, *old)) // Compare the intersect to the proper x position.
                                : decodeHalf(tTopX(*newHeader, *new)) - intersectTopX;
    bool newIsLower = halfGTE(tBottom(*new), tBottom(*old)); // Which threshold header do we need to calculate the xIntersect on.
    HEADER *intersectBottomHeader = newIsLower ? newHeader : oldHeader; // Which threshold header do we need to calculate the xIntersect on.
    THRESHOLD *intersectBottom = newIsLower ? new : old; // Which threshold do we need to calculate the xIntersect on.
    float cutBottom = decodeHalf(newIsLower ? tBottom(*old) : tBottom(*new)); // Which top do we find the intersect on.
    float intersectBottomX = thresholdIntersectX(*intersectBottomHeader, *intersectBottom, cutBottom);
    float bottomDiff = newIsLower ? intersectBottomX - decodeHalf(tBottomX(*oldHeader, *old))
                                  : decodeHalf(tBottomX(*newHeader, *new)) - intersectBottomX;
    float segmentHeight = cutBottom - cutTop;
    topDiff    = (fabs(topDiff)    < 0.00001f) ? 0.0f : topDiff; // eliminate signed zero
    bottomDiff = (fabs(bottomDiff) < 0.00001f) ? 0.0f : bottomDiff; // eliminate signed zero
    //DEBUG_IF(printf("topDiff: %f bottomDiff: %f multiplied: %f \n", topDiff, bottomDiff, topDiff * bottomDiff);)
    if (topDiff * bottomDiff < 0) { // if topDiff and bottomDiff are both positive or both negative then this should be false.
        //DEBUG_IF(printf("      intersecting\n");)
        addIntersecting( tS
                       , pS
                       , newHeader
                       , new
                       , oldHeader
                       , old
                       , topDiff
                       , bottomDiff
                       , cutTop
                       , segmentHeight
                       , cursor
                       , holding
                       );
    }
    *newIsLeft = topDiff <= 0;
}

void leftOrRight( __private  ThresholdState *tS
                , __private      ParseState *pS
                ,                    HEADER *newHeader
                ,                 THRESHOLD *new
                ,                    HEADER *oldHeader
                ,                 THRESHOLD *old
                ,                       int  cursor
                , __private             int *holding
                , __private            bool *done
                ) {
  //DEBUG_IF(printf("  leftOrRight\n");)
  bool newIsLeft = true;
  if (halfLTE(tRight(*new),tLeft(*old))) {
    DEBUG_IF(printf("    tRight(*new)<=tLeft(*old)\n");)
    newIsLeft = true;
  }
  else if (halfLTE(tRight(*old),tLeft(*new))) {
    DEBUG_IF(printf("    tRight(*old)<=tLeft(*new)\n");)
    newIsLeft = false;
  }
  else {
    //DEBUG_IF(printf("    leftOrRightInside\n");)
    leftOrRightInside (  tS
                      ,  pS
                      ,  newHeader
                      ,  new
                      ,  oldHeader
                      ,  old
                      ,  cursor
                      ,  holding
                      , &newIsLeft
                      );
  }
  //DEBUG_IF(printf("newIsLeft %i\n", newIsLeft);)
  //if (tS->trigger) {
  addToLeft( tS
           , pS
           , newHeader
           , new
           , oldHeader
           , old
           , cursor
           , holding
           , done
           , newIsLeft
           );
   //}
}


















// This is just a bonepile of unused openCL code.

                DEBUG_IF(printf("        ***************************\n");\
                         printf("        ***************************\n");\
                         printf("        ****buildThrehsoldArray****\n");\
                         printf("        ***************************\n");\
                         printf("        ***************************\n");\
                        )


void addToLeft ( __private  ThresholdState *tS
               , __private      ParseState *pS
               , __private THRESHOLDHEADER *newHeader
               , __private       THRESHOLD *new
               , __private THRESHOLDHEADER *oldHeader
               , __private       THRESHOLD *old
               ,                       int  cursor
               ,                       int *holding
               ,                      bool *done
               ,                      bool  newIsLeft
               ) {

    THRESHOLDHEADER *leftHeader  = newIsLeft ? newHeader : oldHeader;
          THRESHOLD *left        = newIsLeft ? new : old;
    THRESHOLDHEADER *rightHeader = newIsLeft ? oldHeader : newHeader;
          THRESHOLD *right       = newIsLeft ? old : new;
    DEBUG_IF(printf("addToLeft cursor %i\n   left: ", cursor);\
             showThreshold(*leftHeader, *left);\
             printf("\n  right: ");\
             showThreshold(*rightHeader, *right);\
             printf("\n");\
             )
    THRESHOLDHEADER splitLeftHeader;  // this is used to temporarily store the bottom half of a threshold header.
          THRESHOLD splitLeft;        // this is used to temporarily store the bottom half of a threshold.
    THRESHOLDHEADER splitRightHeader; // this is used to temporarily store the bottom half of a threshold header.
          THRESHOLD splitRight;       // this is used to temporarily store the bottom half of a threshold.
    bool bottomTest = tBottom(*left) > tBottom(*right);
    bool topTest    = tTop(*left)    > tTop(*right);
    if (bottomTest) {
        // this has to happen first because it will affect the bottoms but not the tops for the
        // subsequent comparison.
        //  left    right
        //  |       |
        //  * - - - *
        //  |
        //  *
        // split the left side
        splitThreshold(  leftHeader
                      ,  left
                      , &splitLeftHeader
                      , &splitLeft
                      ,  tBottom(*right)
                      );
        attemptToHold(tS, pS, splitLeftHeader, encodeThreshold(splitLeft), holding);
    }
    if (topTest) {
        //  left    right
        //          *
        //          |
        //  * - - - *
        //  |       | held
        splitThreshold(  rightHeader
                      ,  right
                      , &splitRightHeader
                      , &splitRight
                      ,  tTop(*left)
                      );
        attemptToHold(tS, pS, splitRightHeader, encodeThreshold(splitRight), holding);
    }
    *done = topTest == newIsLeft; // equivalance not (exclusive or)
    int place = *done ? cursor-1 : cursor;
    DEBUG_IF(printf("place %i\n", place);)
    attemptToStore(tS, pS, *oldHeader, encodeThreshold(*old), place);

}

/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////

// A section is a vertical rectangular portion of a pixel
// We process it by scanning across the active thresholds from left to right
// Incidentally you can implement LCD subpixel rendering by just dividing this function into
// three parts for RGB portions of pixels https://en.wikipedia.org/wiki/Subpixel_rendering
inline float4 processSection( __private ThresholdState *tS
                            , __constant         Shape *shapes
                            ,               BRUSHSTACK  localBrushStack
                            ,               SHAPESTACK  localShapeStack
                            , __private     ColorState *cS
                            , __private     ParseState *pS
                            ,                    float  x
                            ) {
    float4 accColor = TRANSPARENT_COLOR; // start with color zero
    float lastX = LEFTBORDER; // start on the left side of the pixel.

    //DEBUG_IF(printf("processSection0 pS->activeStart: %i pS->activeEnd: %i pS->sectionStart: %v2f pS->sectionEnd: %v2f localShapeStack %i localBrushStack %02v8x \n"\
    //                               , pS->activeStart,    pS->activeEnd,    pS->sectionStart,      pS->sectionEnd,      localShapeStack,   localBrushStack         );)
    int i;
    for (i = pS->activeStart; i < pS->activeEnd; i++) { // iterate over every active threshold
        if (DECODE_THRESHOLD_BOTTOM(tS->thresholds[i]) > pS->sectionStart.y) { // check that the threshold hasn't been cleaned from the list by advanceSectn
            // find the mid
            float midX = thresholdMidX(decodeThreshold(tS->thresholds[i]), tS->thresholdHeaders[i], pS->sectionStart.y, pS->sectionEnd.y); // find the midpoint of the threshold when bound by the section
            float currentX = clamp(RIGHTBORDER,LEFTBORDER, midX); // clamp it to prevent geometric inaccuracies from half floats
            DEBUG_IF(printf("             processSection i: %i currentX: %f ", i, currentX);\
                     showThreshold( tS->thresholdHeaders[i]\
                                  , decodeThreshold(tS->thresholds[i]));\
                     printf("\n");)
            // determine the color of the current horizontal section
            float4 color = determineBufferedColor( i - pS->activeStart
                                                 , localShapeStack
                                                 , cS
                                                 , shapes
                                                 );
            // add that color to the accumulated value using the width of the section as a multiplier
            DEBUG_IF(printf("currentX: %f lastX: %f (currentX - lastX): %f\n", currentX, lastX, currentX - lastX);)
            DEBUG_SHOW_SECTION(currentX)
            accColor += color * (currentX - lastX);
            //DEBUG_IF(printf("(currentX - lastX) %f color: %2.2v4hlf accColor: %2.2v4hlf \n" \
            //               , (currentX - lastX),   color,           accColor            );)
            // affect the stack state with the threshold header. (This is only persistent within this vertical section.)
            //DEBUG_IF(printf("ps ");)
            passHeader(  tS->thresholdHeaders[i]
                      , &localBrushStack  // these changes are just local to this function.
                      , &localShapeStack);
            lastX = currentX;
        }
    }
    //DEBUG_IF(printf("processSection5 pS->activeStart: %i pS->activeEnd: %i pS->sectionStart: %v2f pS->sectionEnd: %v2f localShapeStack %i localBrushStack %02v8x \n"\
    //                               , pS->activeStart,    pS->activeEnd,    pS->sectionStart,      pS->sectionEnd,      localShapeStack,   localBrushStack         );)
    // finish off the far right section (this could be the only section.)
    float4 color = determineBufferedColor( i - pS->activeStart
                                         , localShapeStack
                                         , cS
                                         , shapes
                                         );
    DEBUG_IF(printf("RIGHTBORDER: %f lastX: %f (RIGHTBORDER - lastX): %f\n", RIGHTBORDER, lastX, RIGHTBORDER - lastX);)
    DEBUG_SHOW_SECTION(RIGHTBORDER)
    accColor += color * (RIGHTBORDER - lastX);
    //DEBUG_IF(printf("processSection6 pS->sectionStart: %v2f (RIGHTBORDER - lastX) %f color: %2.2v4hlf accColor * (sBot- sTop): %2.2v4hlf \n"\
    //                               , pS->sectionStart,      (RIGHTBORDER - lastX),   color,           accColor * (pS->sectionEnd.y - pS->sectionStart.y));)
    return accColor * (pS->sectionEnd.y - pS->sectionStart.y);
}
/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////


// Advance to the next horizontal section passing over persistant shape stack changes and adjusting the list of active thresholds
float advanceSection( __private  ThresholdState  *tS
                    , __private      ParseState  *pS
                    , __private          float    sectionLine
                    ,                     float   x
                    ) {
    while ((pS->activeEnd < tS->numThresholds) && (DECODE_THRESHOLD_TOP(tS->thresholds[pS->activeEnd]) <= sectionLine)) {
        passHeaderTop(tS->thresholdHeaders[pS->activeEnd], pS);
        pS->activeEnd += 1;
        *anyChange = true;
    }
    //DEBUG_IF(printf("advanceSectn0 pS->activeStart %i pS->activeEnd %i sectionLine %2.2f tS->numThresholds %i\n" \
    //                               , pS->activeStart,   pS->activeEnd,   sectionLine,      tS->numThresholds   );)
    while ((pS->activeStart < pS->activeEnd) && (DECODE_THRESHOLD_BOTTOM(tS->thresholds[pS->activeStart]) <= sectionLine)) {
        passHeaderBottom(tS->thresholdHeaders[pS->activeStart], pS);
        pS->activeStart += 1;
        *anyChange = true;
    }
    //DEBUG_IF(printf("advanceSectn1 pS->activeStart %i pS->activeEnd %i sectionLine %v2f tS->numThresholds %i\n" \
    //                               , pS->activeStart,   pS->activeEnd,   sectionLine,     tS->numThresholds   );)
    // clean any thresholds that are above the new section but stuck behind one that still interacts with it.
    for (int i = pS->activeStart + 1; i < pS->activeEnd; i++) {
        if (DECODE_THRESHOLD_BOTTOM(tS->thresholds[i]) <= sectionLine) {
            passHeaderBottom(tS->thresholdHeaders[i], pS);
            tS->thresholds[i] = NEUTRALHALFTHRESHOLD; // neutralize threshold
            tS->thresholdHeaders[i] = NEUTRALHEADER;
            *anyChange = true;
        }
    }
    //DEBUG_IF(printf("advanceSectn2 *activeStart %i *activeEnd %i sectionLine %2.2f numThresholds %i\n" \
    //                               , *activeStart,   *activeEnd,   sectionLine,      numThresholds   );)
    // determine the bottom of the next section.
    float nextBottom = (pS->activeEnd < tS->numThresholds) ? DECODE_THRESHOLD_TOP(tS->thresholds[pS->activeEnd]) : 55;
    for (int i = pS->activeStart; i < pS->activeEnd; i++) {
        float bottom = DECODE_THRESHOLD_BOTTOM(tS->thresholds[i]);
        if (bottom > sectionLine) {
          nextBottom = min(bottom, nextBottom);
        }
    }
    //DEBUG_IF(printf("advanceSectn3 *activeStart %i *activeEnd %i sectionLine %2.2f numThresholds %i\n" \
    //                               , *activeStart,   *activeEnd,   sectionLine,      numThresholds   );)
    return nextBottom;
    //DEBUG_IF(printf("advanceSectn4 *activeStart %i *activeEnd %i sectionLine %2.2f numThresholds %i nextBottom %f\n" \
    //                               , *activeStart,   *activeEnd,   sectionLine,      numThresholds,   nextBottom );)

}

/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
        float nextThreshold;                                                       \
        /*DEBUG_IF {showThresholds(thresholdHeaders, thresholds, numThresholds);}*/\
        if (thresholdCursor < numThresholds) {                                     \
            nextThreshold = advanceThreshold (  thresholds                         \
                                             , &thresholdCursor                    \
                                             ,  numThresholds                      \
                                             ,  height                             \
                                             ,  0                                  \
                                             );                                    \
        }                                                                          \
        else {                                                                     \
          nextThreshold = (float)height + 1;                                       \
        }                                                                          \
        float nextPixel = 0;                                                       \
        float nextTail = parseActive(  thresholdHeaders                            \
                                    ,  thresholds                                  \
                                    ,  thresholdCursor                             \
                                    , &activeStart                                 \
                                    ,  numThresholds                               \
                                    ,  brushStack                                  \
                                    ,  shapeStack                                  \
                                    ,  x                                           \
                                    ,  0                                           \
                                    ,  height                                      \
                                    );                                             \

inline void parseThresholdAbove(           THRESHOLDHEADER  thresholdHeader
                               ,           THRESHOLD        threshold
                               , __private BRUSHSTACK_TYPE *brushStack
                               , __private SHAPESTACK_TYPE *shapeStack
                               ,           float            y
                               ) {
    float slope = GET_THRESHOLD_SLOPE(threshold);
    if ((GET_THRESHOLD_TOP(threshold)    <= y && slope >  0) ||
            // y |-o-----|
            //   | .\    |
            // y |---\---|
            //   | ...\  |
            //   | ....\ |
        (GET_THRESHOLD_BOTTOM(threshold) <= y && slope <= 0)
            //   |   /.. |
            //   |  /... |
            // y |-o---- |
            //   | ..... |
            // y |-------|
        ) {
        parseHeader(thresholdHeader, brushStack, shapeStack);
        //DEBUG_IF {printf(" PARSED ");}
    }
}

inline void parseThreshold(           THRESHOLDHEADER  thresholdHeader
                          ,           THRESHOLD        threshold
                          , __private BRUSHSTACK_TYPE *brushStack
                          , __private SHAPESTACK_TYPE *shapeStack
                          ,           float            y
                          ) {
    if ((GET_THRESHOLD_TOP(threshold)    == y && GET_THRESHOLD_SLOPE(threshold) >  0) ||
            // y |-o----|
            //   | .\   |
            //   | ..\  |
        (GET_THRESHOLD_BOTTOM(threshold) == y && GET_THRESHOLD_SLOPE(threshold) <= 0)
            //   |   /. |
            //   |  /.. |
            // y |-o----|
        ) {
        DEBUG_IF {printf("          PARSED shapeStack: %04x brushStack: %02v8x ", *shapeStack, *brushStack); showThresholdHeader(thresholdHeader); showThreshold(threshold); printf("\n");}
        parseHeader(thresholdHeader, brushStack, shapeStack);
    }
}

inline bool isActive( THRESHOLD threshold
                    , float     y
                    ) {
    return (y < GET_THRESHOLD_BOTTOM(threshold));
}

inline float advanceThreshold ( __local   THRESHOLD  *thresholds
                              , __private int        *thresholdCursor
                              ,           int         numThresholds
                              ,           int         height
                              ,           float       yLimit
                              ) {
    float nextThreshold = (float)height + 1;
    if (*thresholdCursor < numThresholds) {
        nextThreshold = GET_THRESHOLD_TOP(thresholds[*thresholdCursor]);
        while ((*thresholdCursor + 1 < numThresholds ) &&
               ((nextThreshold < yLimit) ||
                (GET_THRESHOLD_TOP(thresholds[*thresholdCursor + 1]) == nextThreshold)
               )
              ) {
            *thresholdCursor += 1;
            nextThreshold = min(GET_THRESHOLD_TOP(thresholds[*thresholdCursor + 1]), nextThreshold);
        }
    }
    return nextThreshold;
}

// parse the current activeStart and return the minimum tail intersection.
inline float parseActive( __local   THRESHOLDHEADER *thresholdHeaders
                        , __local   THRESHOLD       *thresholds
                        ,           int             thresholdCursor
                        , __private int             *activeStart
                        ,           int             numThresholds
                        , __private BRUSHSTACK_TYPE *brushStack
                        , __private SHAPESTACK_TYPE *shapeStack
                        ,           float           x
                        ,           float           yNext
                        ) {
    int firstActive = thresholdCursor;
    float nextTail = FLT_MAX;
    if (*activeStart == thresholdCursor) {
        return nextTail;
    }
    else {
        for (int walkBack = *activeStart; walkBack <= thresholdCursor; walkBack++) {
              if (walkBack < numThresholds) {
                  THRESHOLD threshold = thresholds[walkBack];
                  THRESHOLDHEADER thresholdHeader = thresholdHeaders[walkBack];
                  //DEBUG_IF {printf("          parseActive0 yNext: %2.2f thresholdCursor: %i walkBack: %i firstActive: %i *activeStart: %i\n", yNext, thresholdCursor, walkBack, firstActive, *activeStart);}
                  //DEBUG_IF {printf("            "); showThresholdHeader(thresholdHeader); showThreshold(threshold); printf("\n");}
                  if (isActive(threshold, yNext)) {
                      firstActive = min(walkBack, firstActive);
                      nextTail = min(nextTail, GET_THRESHOLD_BOTTOM(threshold));
                  }
                  parseThreshold(thresholdHeader, threshold, brushStack, shapeStack, yNext);
              }
        }
        *activeStart = firstActive;
        return nextTail;
    }
}


inline float4 processPixel( __local   THRESHOLDHEADER *thresholdHeaders
                          , __local   THRESHOLD       *thresholds
                          , __local   HALF4           *colorStack
                          , __private BRUSHSTACK_TYPE *brushStack
                          , __private SHAPESTACK_TYPE *shapeStack
                          , __private float           *nextThreshold
                          , __private float           *nextTail
                          ,           float            nextPixel
                          , __private int             *thresholdCursor
                          , __private int             *activeStart
                          ,           int              numThresholds
                          ,           float            x
                          ,           int              height
                          ) {
    float4 color;
    float4 accColor = TRANSPARENT_COLOR; // accumulated color
    float  yTop  = nextPixel - PIXELHEIGHT;
    float  yNext = min (*nextThreshold, *nextTail);
    DEBUG_IF {printf("processPixel0 yTop: %2.2f yNext: %2.2f *nextThreshold: %2.2f *nextTail: %2.2f nextPixel: %2.2f *thresholdCursor: %i *activeStart: %i numThresholds: %i accColor: %2.2v4f\n"
                     ,              yTop,       yNext,       *nextThreshold,       *nextTail,       nextPixel,       *thresholdCursor,    *activeStart,    numThresholds,    accColor         );}
    while (yNext <= nextPixel) {
        color = processHorizontal(  thresholdHeaders
                                 ,  thresholds
                                 ,  colorStack
                                 , *brushStack
                                 , *shapeStack
                                 , *thresholdCursor
                                 ,  numThresholds
                                 , *activeStart
                                 ,  x
                                 ,  yTop
                                 ,  yNext
                                 );
        //color    = determineColor(*shapeStack, colorStack) * (yNext - yTop);
        accColor += color;
        DEBUG_IF {printf("processPixel1 yTop: %2.2f yNext: %2.2f *nextThreshold: %2.2f *nextTail: %2.2f nextPixel: %2.2f *thresholdCursor: %i *activeStart: %i numThresholds: %i accColor: %2.2v4f\n"
                         ,              yTop,       yNext,       *nextThreshold,       *nextTail,       nextPixel,       *thresholdCursor,    *activeStart,    numThresholds,    accColor         );}
        if (*nextThreshold < *nextTail) {
            advanceThreshold ( thresholds
                             , thresholdCursor
                             , numThresholds
                             , height
                             , yNext
                             );
        }
        DEBUG_IF {printf("processPixel2 yTop: %2.2f yNext: %2.2f *nextThreshold: %2.2f *nextTail: %2.2f nextPixel: %2.2f *thresholdCursor: %i *activeStart: %i numThresholds: %i accColor: %2.2v4f\n"
                         ,              yTop,       yNext,       *nextThreshold,       *nextTail,       nextPixel,       *thresholdCursor,    *activeStart,    numThresholds,    accColor         );}
        *nextTail = parseActive(  thresholdHeaders
                               ,  thresholds
                               , *thresholdCursor
                               ,  activeStart
                               ,  numThresholds
                               ,  brushStack
                               ,  shapeStack
                               ,  x
                               ,  yNext
                               ,  height
                               );
        yTop  = yNext;
        yNext = min (*nextThreshold, *nextTail);
        //DEBUG_IF {printf("shapeStack: %04x brushStack: %02v8x\n", *shapeStack, as_uchar8(*brushStack));}
        DEBUG_IF {printf("processPixel3 yTop: %2.2f yNext: %2.2f *nextThreshold: %2.2f *nextTail: %2.2f nextPixel: %2.2f *thresholdCursor: %i *activeStart: %i numThresholds: %i accColor: %2.2v4f\n"
                         ,              yTop,       yNext,       *nextThreshold,       *nextTail,       nextPixel,       *thresholdCursor,    *activeStart,    numThresholds,    accColor         );}
    } // while
    // complete the color value
    color = processHorizontal(  thresholdHeaders
                             ,  thresholds
                             ,  colorStack
                             , *brushStack
                             , *shapeStack
                             , *thresholdCursor
                             ,  numThresholds
                             , *activeStart
                             ,  x
                             ,  yTop
                             ,  nextPixel
                             );
    //color = determineColor(*shapeStack, colorStack) * (nextPixel - yTop);
    DEBUG_IF {printf("processPixelX yTop: %2.2f yNext: %2.2f *nextThreshold: %2.2f *nextTail: %2.2f nextPixel: %2.2f *thresholdCursor: %i *activeStart: %i numThresholds: %i -accColor: %2.2v4f\n"
                     ,              yTop,       yNext,       *nextThreshold,       *nextTail,       nextPixel,       *thresholdCursor,    *activeStart,    numThresholds,     accColor + color );}
    return accColor + color;
    //return (float4)(0.3,0.7,0.9,1.0);
}










        advanceSection(  thresholdHeaders                              \
                      ,  thresholds                                    \
                      ,  colorStack                                    \
                      ,  brushStack                                    \
                      ,  shapeStack                                    \
                      , &activeStart                                   \
                      , &thresholdCursor                               \
                      , &sectionTop                                    \
                      , &sectionBottom                                 \
                      ,  numThresholds                                 \
                      );                                               \
        for (float y = 0; y < height; y += 1.0f ) {                    \
            float4 color =  processPixel( thresholdHeaders             \
                                        , thresholds                   \
                                        , colorStack                   \
                                        , brushStack                   \
                                        , shapeStack                   \
                                        , &activeStart                 \
                                        , &thresholdCursor             \
                                        , &sectionTop                  \
                                        , &sectionBottom               \
                                        , x                            \
                                        , y                            \
                                        , numThresholds                \
                                        );                             \
            writerCode                                                 \
            pixPos += tileWidth;                                       \
            barrier (CLK_LOCAL_MEM_FENCE);                             \
        }                                                              \

__kernel void fillBackgroundTile (      COLOR  backgroundColor
                                 , GMEM  uint *out
                                 ,        int  bitmapWidth
                                 ,        int  bitmapHeight
                                 ,        int  gridWidth
                                 ,        int  tileWidth
                                 ,        int  tileHeight
                                 ,        int  computeDepth
                                 ) {
    int   tileIndex = INDEX;
    int   column = COLUMN;
    TileState tileS;
    initTileState ( &tileS
                  ,  tileIndex
                  ,  0x0
                  ,  bitmapWidth
                  ,  bitmapHeight
                  ,  column
                  ,  computeDepth
                  );
    COLOR color = backgroundColor;
    if (tileS.tileDeltaX + column < tileS.bitmapWidth) {
        fillOutBuffer ( &tileS
                      ,  out
                      ,  color
                      );
    }
}

__kernel void checkContinuations (GMEM Continuation *continuations
                                 ,              int  numColumns
                                 , GMEM        bool *localSums
                                 , GMEM         int *returnVal
                                 ) {
    uint local_id = get_global_id(0);
    uint group_size = get_global_size(0);
    Continuation c = getContinuation(continuations, local_id);
    // Copy from global to local memory
    localSums[local_id] = c.contIsContinued;;
    //if (local_id == 0) {
    //  printf("group_size %i", group_size);
    //}
    // Loop for computing localSums : divide WorkGroup into 2 parts
    for (uint stride = group_size/2; stride>0; stride /= 2) {
        // Waiting for each 2x2 addition into given workgroup
        barrier(CLK_LOCAL_MEM_FENCE);

        // Add elements 2 by 2 between local_id and local_id + stride
        if (local_id < stride)
          localSums[local_id] = localSums[local_id] || localSums[local_id + stride];
    }

    // Write result into partialSums[nWorkGroups]
    if (local_id == 0) {
        *returnVal = localSums[0];
    }
}

// Since the continuation array must be allocated by the host and the size of a continuation depends on the size of half2 and bool
// We just use an alignment as large as the largest possible size_of(Continuation) rather than force the host code to try and
// determine the right size for this array we can just waste a little space and get on with our lives.
#define CONTINUATION_ALIGN 40

typedef struct Continuation {
    SPACE2 contRenderStart;         // 4
    SPACE2 contRenderEnd;           // 4
    float8 contAccColorArea;       // 32
    int  contYInt;                 // 4
    bool contIsContinued;          // 2
    bool contInHorizontalPass;     // 2
    bool contNeedHorizontalPass;   // 2
} Continuation;

inline Continuation newContinuation(void);

inline Continuation getContinuationForTile(           TileState *tileS
                                          ,  GMEM  Continuation *continuations
                                          );

inline Continuation getContinuation( GMEM Continuation *continuations
                                   ,               int  columnIndex
                                   );

Continuation makeContinuation( PMEM ThresholdState *tS
                             , PMEM     ParseState *pS
                             ,                int  yInt
                             ,               bool  isContinued
                             );

void setContinuation(          TileState *tileS
                    , GMEM  Continuation *continuations
                    ,       Continuation  c
                    );

inline Continuation newContinuation() {
    Continuation c;
    c.contRenderStart = (SPACE2)(LEFTBORDER,0);
    c.contRenderEnd  = (SPACE2)(RIGHTBORDER,0); // the lowest vertical position that can be properly rendered with the current list of thresholds.
    c.contAccColorArea = (float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0));
    c.contYInt = 0;
    c.contIsContinued = false;
    c.contInHorizontalPass = false;
    c.contNeedHorizontalPass = false;
    return c;
}

inline Continuation getContinuationForTile( PMEM    TileState *tileS
                                          , GMEM Continuation *continuations
                                          ) {
    return getContinuation(continuations, tileS->tileIndex * tileS->tileSize.x + tileS->column);
}

inline Continuation getContinuation( GMEM Continuation *continuations
                                   ,               int  columnIndex
                                   ) {
    return *((GMEM Continuation *)(continuations + columnIndex * CONTINUATION_ALIGN));
}


Continuation makeContinuation( PMEM ThresholdState *tS
                             , PMEM     ParseState *pS
                             ,                int  yInt
                             ,               bool  isContinued
                             ) {
    Continuation c;
    c.contRenderStart  = tS->renderStart;
    c.contRenderEnd    = tS->renderEnd;
    c.contInHorizontalPass   = tS->inHorizontalPass;
    c.contNeedHorizontalPass = tS->needHorizontalPass;

    c.contAccColorArea = pS->accColorArea;
    c.contYInt         = yInt;
    c.contIsContinued = isContinued;
    return c;
}


void setContinuation(            TileState *tileS
                    , GMEM    Continuation *continuations
                    ,         Continuation  c
                    ) {
    GMEM Continuation *p = ((GMEM Continuation *)(continuations + (tileS->tileIndex * tileS->tileSize.x + tileS->column)*CONTINUATION_ALIGN));
    *p = c;
}


void showContinuation(Continuation c) {
    printf ("        contRenderStart %2i %v2f \n" , (long) &c.contRenderStart        - (long)&c, c.contRenderStart        );
    printf ("          contRenderEnd %2i %v2f \n" , (long) &c.contRenderEnd          - (long)&c, c.contRenderEnd          );
    printf ("           contAccColor %2i %v8f \n" , (long) &c.contAccColorArea       - (long)&c, c.contAccColorArea       );
    printf ("               contYInt %2i %i   \n" , (long) &c.contYInt               - (long)&c, c.contYInt               );
    printf ("        contIsContinued %2i %i   \n" , (long) &c.contIsContinued        - (long)&c, c.contIsContinued        );
    printf ("   contInHorizontalPass %2i %i   \n" , (long) &c.contInHorizontalPass   - (long)&c, c.contInHorizontalPass   );
    printf (" contNeedHorizontalPass %2i %i   \n" , (long) &c.contNeedHorizontalPass - (long)&c, c.contNeedHorizontalPass );
}


float arbitraryIntersect( HEADER currentHeader
                        , THRESHOLD current
                        , HEADER nextHeader
                        , THRESHOLD next
                        ) {

    float aC = tBottom(current)-tTop(current);
    // B = x1-x2
    float bC = tBottomX(currentHeader, current)-tTopX(currentHeader,current);

    // A = y2-y1
    float aN = tBottom(next)-tTop(next);
    // B = x1-x2
    float bN = tBottomX(nextHeader, next)-tTopX(nextHeader, next);
    // det = A1*B2 - A2*B1
    float det = aC*bN - aN*bC;
    float intersectY = FLT_MAX; // will be clamped to bottom.
    if(fabs(det) > 0.001){
        // C = Ax1+By1
        //float cC = aC * tBottomX(currentHeader, current) + bC * tTop(current);
        // C = Ax1+By1
        //float cN = aN * tBottomX(nextHeader, next) + bN * tTop(next);
        intersectY = 999; //(aC*cN - aN*cC)/det;
        //DEBUG_IF(printf("det %f intersectY %f slicePoint %f\n", det, intersectY, slicePoint);)
    }
    return clamp(intersectY, tTop(current),tBottom(current));
}

// value used to fudge geometry errors when comparing geometric values
#define TOLERANCE 0.002
// equality with the fudge factor
inline bool eqT( float a, float b) {
  return fabs(a - b) <= TOLERANCE;
}

inline void updateShapeStack(         SHAPEBIT  shapeBit
                            , PMEM  SHAPESTACK *shapeStack
                            );


////////////////////////////////////////////////
// ShapeStack testing Functions
////////////////////////////////////////////////

void fillShapeStack(SHAPESTACK *stack, SHAPESTACK value);

void testShapeStack(void);
void testIgnoreBits(void);
void testDeleteBit(void);

void testShapeStack(void) {
    DEBUG_IF(printf("clz(0x8000000000000000) %i, clz(0x00000000) %i\n", clz((ulong)0x8000000000000000), clz((ulong)0x0));)
    __private SHAPESTACK stack[SHAPESTACKSECTIONS];
    for (int bit = MAXSHAPE; bit >= 0; bit -= 1) {
        clearShapeStack(stack);
        flipBit(bit,   stack);
        //flipBit(bit+1, stack);
        DEBUG_IF(printf(" bit: %i top: %i ", bit, findTop(stack, 512));showShapeStack(stack);printf("\n");)
    }
}

void testIgnoreBits(void) {
  for (int ignoreAbove = 512; ignoreAbove >= 0; ignoreAbove--) {
      DEBUG_IF( int ignoreSection = ignoreAbove >> SHAPESTACKSECTIONSHIFT; \
                int ignoreBits    = ignoreAbove & SHAPESTACKSECTIONBITS; \
                printf("ignoreAbove: %i ignoreSection %i ignoreBits %i ",ignoreAbove,ignoreSection,ignoreBits); \
                printf("(COMPLETE_MASK << ignoreBits) %016lX (~(COMPLETE_MASK << ignoreBits)) %016lX \n", (COMPLETE_MASK << ignoreBits), (~(COMPLETE_MASK << ignoreBits)) ); \
      )
  }
}

#define DELETEBITTESTVALUE 0xAAAAAAAAAAAAAAAA

void fillShapeStack(SHAPESTACK *stack, SHAPESTACK value) {
  for (int i = 0; i < SHAPESTACKSECTIONS; i ++) {
    stack[i] = value;
  }
}

void testDeleteBit(void) {
  __private SHAPESTACK stack[SHAPESTACKSECTIONS];
  for (int i = 0; i < MAXSHAPE + 2; i++) {
    fillShapeStack(stack, DELETEBITTESTVALUE);
    deleteBit(stack, i);
    DEBUG_IF(printf("test deleteBit i %i ", i);)
    for (int i = SHAPESTACKSECTIONS - 1; i >= 0; i--) {
      DEBUG_IF(printf(" %016lX", stack[i]);)
    }
    DEBUG_IF(printf("\n");)
  }
}

inline SHAPESTACK carryBitSet(SHAPESTACK carryBit) {
    return carryBit << SHAPESTACKCARRYSHIFT;
}

inline SHAPESTACK shiftSection(SHAPESTACK carryBit, SHAPESTACK shapeStack) {
  return carryBitSet(carryBit) | (shapeStack >> 1);
}

inline SHAPESTACK getCarryBit(SHAPESTACK section) {
  return section & SHAPESTACKCARRYMASK;
}

inline SHAPESTACK deleteSectionBit(SHAPESTACK carryBit, SHAPESTACK shapeStack, SHAPEBIT shapeBit) {
    SHAPESTACK breakMask = COMPLETE_MASK << shapeBit;
    //DEBUG_IF(printf("shapeStack %lX shapeBit %i ----> !breakMask %lX (shapeStack >> 1) & (breakMask)) %lX shapeStack & !breakMask %lX\n", shapeStack, shapeBit, ~breakMask,(shapeStack >> 1) & breakMask, shapeStack & ~breakMask);)
    return ((shiftSection(carryBit, shapeStack)) & breakMask) | (shapeStack & (~breakMask));
}

inline void deleteBit(PMEM SHAPESTACK *shapeStack, SHAPEBIT shapeBit) {
    int section = shapeBit >> SHAPESTACKSECTIONSHIFT;
    int bit     = shapeBit & SHAPESTACKSECTIONBITS;
    SHAPESTACK carryBit = EMPTY_SHAPESTACK;
    for (int i = SHAPESTACKSECTIONS - 1; i > section; i--) {
      SHAPESTACK nextCarryBit = getCarryBit(shapeStack[i]);
      shapeStack[i] = shiftSection(carryBit, shapeStack[i]);
      carryBit = nextCarryBit;
    }
    shapeStack[section] = deleteSectionBit(carryBit, shapeStack[section], bit);
}

void removeLastShape( PMEM  ThresholdState *tS
                    , PMEM      ShapeState *shS
                    ,           GEO_ENTRY   shapeIndex
                    ) {
    SHAPEBIT removeBit = tS->numThresholds == 0 ? 0 : headerShapeBit(getHeader(tS, tS->numThresholds - 1));
    int shiftAmount = 0;
    for (int cursor = 0; cursor < tS->numThresholds; cursor ++) {
        HEADER currentHeader = getHeader(tS, cursor);
        THRESHOLD current = getThreshold(tS, cursor);
        SHAPEBIT currentBit = headerShapeBit(currentHeader);
        if (currentBit > removeBit) {
            currentBit -= 1;
        }
        if (headerShapeBit(currentHeader) == removeBit)  {
            shiftAmount += 1;
            adjustToExclude(tS, current);
        }
        else {
            setHeader(tS, cursor-shiftAmount, setShapeBit(currentHeader,currentBit));
            setThreshold(tS, cursor-shiftAmount, current);
        }
    }
    tS->numThresholds = max(0, tS->numThresholds - shiftAmount);
    for (int i = removeBit; i < MAXSHAPE - 1; i++) {
        shS->shapeIndices[i] = shS->shapeIndices[i+1];
    }
    //DEBUG_IF(printf("removeBit %i shapeBit %i\n", removeBit, shapeBit);)
    //DEBUG_IF(printf("before deleteBit ");showShapeStack(shS->shapeStack);printf("\n");)
    deleteBit(shS->shapeStack, removeBit);
    //DEBUG_IF(printf("after  deleteBit ");showShapeStack(shS->shapeStack);printf("\n");)
    if (removeBit < MAXSHAPE) {
        shS->shapeIndices[shS->shapeBits] = shapeIndex;
    }
}


            if (shS->shapeBits >= MAXSHAPE) {
                shS->shapeBits -= 1;
                removeLastShape(tS,shS,shapeIndex);
            }
            else {
               shS->shapeIndices[shS->shapeBits] = shapeIndex;
            }
