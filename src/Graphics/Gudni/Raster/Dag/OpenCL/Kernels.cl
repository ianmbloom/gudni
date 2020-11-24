// 1 Space for haskell defined macros
// 2 So the line numbers are correct
// 3  -----------------------------------------------------------------------------
// 4  -- |
// 5  -- Module      :  Graphics.Gudni.Raster.Dag.OpenCL.Kernel.cl
// 6  -- Copyright   :  (c) Ian Bloom 2019
// 7  -- License     :  BSD-style (see the file libraries/base/LICENSE)
// 8  --
// 9  -- Maintainer  :  Ian Bloom
// 10 -- Stability   :  experimental
// 11 -- Portability :  portable
// 12 --
// 13 -- OpenCL kernel for rendering a bitmap from provided data buffers.
// 14
// 15
// 16
// 17
// 18
// 19
// 20
// 21
// 22
// 23
// 24
// 25
// 26
// 27
// 28
// 29
// 30
// 31
// 32
// 33
// 34
// 35
// 36
// 37
// 38
// 39
// 40
// 41
// 42
// 43
// 44
// 45
// 46
// 47
// 48
// 49
// 50
// 51
// 52
// 53
// 54
// 55
// 56
// 57
// 58
// 59
// 60
// 61
// 62
// 63
// 64
// 65
// 66
// 67
// 68
// 69
// 70
// 71
// 72
// 73
// 74
// 75
// 76
// 77
// 78
// 79
// 80
// 81
// 82
// 83
// 84
// 85
// 86
// 87
// 88
// 89
// 90
// ---------------- Macros, Type definitions and type accessors -----------------------------------

#ifdef DEBUG_OUTPUT
#define DEBUG_IF(statement) if (get_global_id(0) == DEBUG0 && get_global_id(1) == DEBUG1) {statement} // on the fly debugging output
#else
#define DEBUG_IF(statement)
#endif

#ifdef DEBUG_OUTPUT
#define DEBUG_LT(statement) if (get_global_id(0) < DEBUG0 && get_global_id(1) < DEBUG1) {statement} // on the fly debugging output
#else
#define DEBUG_LT(statement)
#endif

#ifdef cl_amd_printf
#pragma OPENCL EXTENSION cl_amd_printf : enable
#endif

// ---------------------------------- Memory Types ----------------------------------
#define GMEM __global
#define LMEM __local
#define PMEM __private
#define CMEM __constant

// ---------------------------------- Types ----------------------------------
#define Ref_          uint
#define Ref2_         uint2
#define Stage_        int

#define ShapeId_      Ref_
#define ShapeId2_     Ref2_
#define StorageId_    Ref_
#define ConfineTagId_ Ref_
#define DecoTagId_    Ref_
#define FabricTagId_  Ref_
#define FabricLimits_ ShapeId2_
#define TreeRootId_   Ref_
#define TreeRoot_     Ref2_
#define PrimTagId_    Ref_
#define TransformId_  Ref_
#define BezierId_     Ref_
#define FacetId_      Ref_
#define BoxId_        Ref_
#define ShapeId_      Ref_
#define Index_        Ref_

#define Tag_          ulong

#define PrimTag_      Tag_
#define FabricTag_    Tag_

#define Space_      float
#define As_Space_   as_float
#define Space2_     float2
#define As_Space2_  as_float2
#define Space3_     float4
#define As_Space3_  as_float4
#define Space4_     float4
#define As_Space4_  as_float4
#define Space6_     float8
#define As_Space6_  as_float8
#define Space8_     float8
#define As_Space8_  as_float8
#define Space9_     float16
#define As_Space9_  as_float16
#define Space12_    float16
#define As_Space12_ as_float16

#define Affine_ Space9_
#define BezTri_ Space12_
#define Tri_    Space6_
#define Tile_   int4

#define Point2_    Space2_
#define Bezier_    Space8_
#define Box_       Space4_
#define Loc2_      int2
#define StackRange_ short2

// ---------------------------------- Constants ----------------------------------
#define LARGE_PRIME 282917
#define MAXCHANNELFLOAT 255.0f // maximum value for converting from float color value
#define RANDOMFIELDMASK RANDOMFIELDSIZE - 1
#define ZEROPOINT (Point2_)(0,0)

// ---------------------------------- Color ----------------------------------
#define Color_    float4
#define HsvColor_ Color_

// A slice is a section of array indexes.
#define Slice_ Ref2_

inline Ref_ sliceStart (Slice_ s) {return s.x;}
inline Ref_ sliceLength(Slice_ s) {return s.y;}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Tag -------------------

typedef struct ConfineTag {
          PrimTagId_ confineTagPrimTagId ;
               uint  confineTagHorizontal;
              Space_ confineTagCut       ;
              Space_ confineTagOverhang  ;
       ConfineTagId_ confineTagLessCut   ;
       ConfineTagId_ confineTagMoreCut   ;
     } ConfineTag;

typedef struct DecoTag
    {     Space_ decoTagCut       ;
            bool decoTagHorizontal;
          Slice_ decoTagCrossings ;
      DecoTagId_ decoTagLessCut   ;
      DecoTagId_ decoTagMoreCut   ;
    } DecoTag;

// ---------------------------------- Structure Types  ------------------------------------


// Store the state of the entire dag.
typedef struct Dag
    { GMEM     Space_  *dagPrimBezierHeap  ;
      GMEM     Space_  *dagPrimFacetHeap   ;
      GMEM     Space_  *dagPrimBoxHeap     ;
      GMEM   PrimTag_  *dagPrimTagHeap     ;
      GMEM FabricTag_  *dagFabricTagHeap   ;
      GMEM       char  *dagFabricHeap      ;
      GMEM  TreeRoot_  *dagTreeRootHeap    ;
      GMEM ConfineTag  *dagTreeConfineHeap ;
      GMEM    DecoTag  *dagTreeDecoHeap    ;
      GMEM   ShapeId_  *dagCrossingHeap    ;
      GMEM     Space_  *dagPictHeap        ;
      GMEM      float  *dagRandomField     ;
                  int   dagRandomCursor    ;
    } Dag;

typedef struct ShapeStack {
    ShapeId_  stVector[SHAPESTACKSIZE];
         int  stSize;
} ShapeStack;

typedef struct BzStack {
    PMEM Bezier_  bezStack[BEZIERSTACKSIZE];
    PMEM    Box_  boxStack[BEZIERSTACKSIZE];
             int  bezStackTop;
} BzStack;

typedef struct PictUse {
    int2 pictSize;      // size of the bitmap
     int pictMemOffset; // starting point of the pixel data in the memory buffer
} PictUse;

typedef struct TreeStack {
     ConfineTagId_ trStack[CONFINETREESTACKSIZE];
               int trSize                       ;
} TreeStack;

typedef struct TraverseState {
     FabricTagId_  tsFabricTagIds[FABRICSTACKSIZE];
      StackRange_  tsRanges      [FABRICSTACKSIZE];
           Stage_  tsStages      [FABRICSTACKSIZE];
          Point2_  tsRays        [FABRICSTACKSIZE];
              int  tsSize                         ;
           Color_  tsColorStack  [COLORSTACKSIZE] ;
              int  tsColorSize                    ;
       ShapeStack  tsShapeStack                   ;
        TreeStack  tsTreeStack                    ;
          BzStack  tsBzStack                      ;
} TraverseState;

// ------------------- functions from Graphics.Gudni.Figure.Facet.Type -----------------------

typedef struct Facet {
  BezTri_ facetOutput ;
     Tri_ facetInput  ;
} Facet;

// -------- Prototypes ------

inline bool crossesBezTriAlong(  Space_  limit
                              , BzStack *bzStack
                              ,    bool  axis
                              ,  Space_  start
                              ,  Space_  baseline
                              ,  Space_  end
                              , BezTri_  t
                              );

inline Space_ getRandom(Dag *dag);

void showDecoTagHeap(int size, GMEM DecoTag  *treeDecoHeap);
void showDecoTag(DecoTag tag);
void showConfineTagHeap(int size, GMEM ConfineTag  *treeConfineHeap);
void showConfineTag(ConfineTag tag);
void showShapeStack(ShapeStack *stack);
void showFabricStack(Dag *dag, TraverseState *state);
void showColor(Color_ color);
void showColorStack(TraverseState *state);
void showStackInRange(StackRange_ range, ShapeStack *stack);
void showBezier(Bezier_ bez);
void showAffine(Affine_ a);
void showFabricTagType(FabricTag_ tag);
// ------------------------------------------ Box ------------------------------------------

inline Space_  taxiDistance(Point2_ v0, Point2_ v1) {return fabs(v1.x - v0.x) + fabs(v1.y - v0.y);}
inline Point2_ mid(Point2_ a, Point2_ b) {return (a + b) / 2;}

#define boxLeft(box)   box.s0
#define boxTop(box)    box.s1
#define boxRight(box)  box.s2
#define boxBottom(box) box.s3

#define minBox(box) box.xy
#define maxBox(box) box.zw

inline Space_  getLeft   (Box_ box) {return boxLeft(box)  ;}
inline Space_  getTop    (Box_ box) {return boxTop(box)   ;}
inline Space_  getRight  (Box_ box) {return boxRight(box) ;}
inline Space_  getBottom (Box_ box) {return boxBottom(box);}
inline Box_    setLeft   (Box_ box, Space_ value) {boxLeft(box)   = value; return box;}
inline Box_    setTop    (Box_ box, Space_ value) {boxTop(box)    = value; return box;}
inline Box_    setRight  (Box_ box, Space_ value) {boxRight(box)  = value; return box;}
inline Box_    setBottom (Box_ box, Space_ value) {boxBottom(box) = value; return box;}

inline Box_ makeBox(Space_ l, Space_ t, Space_ r, Space_ b) {return (Box_)(l, t, r, b);}
inline Point2_ sizeBox  (Box_ box) {return    (maxBox(box) - minBox(box)) ;}
inline Point2_ centerBox(Box_ box) {return mid(minBox(box),  maxBox(box)) ;}

// ------------------- functions from Graphics.Gudni.Raster.Dag.Fabric.Tag -------------------

#define FabricNodeType_    FabricTag_
#define FabricNodeSubType_ FabricTag_
#define FabricSubType_     FabricTag_
#define FabricData_        FabricTag_
#define FabricHigh_        StorageId_
#define FabricLow_         StorageId_

inline FabricHigh_ fromFabHigh(FabricTag_ i) {return (FabricHigh_) ((i & FABRICTAGHIGHIDBITMASK) >> FABRICTAGHIGHIDSHIFT);}
inline FabricLow_  fromFabLow (FabricTag_ i) {return (FabricLow_ ) (i & FABRICTAGLOWIDBITMASK)                           ;}
inline FabricData_ fromFabData       (FabricTag_ tag) {return tag & FABRICTAGDATABITMASK ;}
inline FabricData_ fromSubstanceData (FabricTag_ tag) {return tag & FABRICTAGDATABITMASK ;}
inline FabricNodeType_ fabTagNodeType(FabricTag_ tag) {return tag & FABRICTYPEBITMASK;}

inline bool matchNodeType(FabricNodeType_ ty, FabricTag_ fabricTag) {return fabTagNodeType(fabricTag) == ty;}

inline bool fabTagIsLeaf     (FabricTag_ tag) {return matchNodeType(FABRICISLEAF     , tag);}
inline bool fabTagIsUnaryPre (FabricTag_ tag) {return matchNodeType(FABRICISUNARYPRE , tag);}
inline bool fabTagIsUnaryPost(FabricTag_ tag) {return matchNodeType(FABRICISUNARYPOST, tag);}
inline bool fabTagIsBinaryOp (FabricTag_ tag) {return matchNodeType(FABRICISBINARY   , tag);}

inline FabricNodeSubType_ fabTagSubType(FabricTag_ fabricTag) {return fabricTag & FABRICSUBTYPEBITMASK;}

inline bool matchSubType(FabricSubType_ match, FabricTag_ fabricTag) {return fabTagSubType(fabricTag) == match;}

inline FabricData_ substanceTagRef(FabricTag_ tag) {return tag & FABRICTAGDATABITMASK;}

inline bool fabTagIsConstant (FabricTag_ tag) {return matchSubType(FABRICISCONSTANT , tag);}
inline bool fabTagIsTexture  (FabricTag_ tag) {return matchSubType(FABRICISTEXTURE  , tag);}
inline bool fabTagIsLinear   (FabricTag_ tag) {return matchSubType(FABRICISLINEAR   , tag);}
inline bool fabTagIsQuadrance(FabricTag_ tag) {return matchSubType(FABRICISQUADRANCE, tag);}

inline bool fabTagIsTree             (FabricTag_ tag) {return matchSubType(FABRICISTREE             , tag);}
inline bool fabTagIsTransformAffine  (FabricTag_ tag) {return matchSubType(FABRICISTRANSFORMAFFINE  , tag);}
inline bool fabTagIsTransformFacet   (FabricTag_ tag) {return matchSubType(FABRICISTRANSFORMFACET   , tag);}
inline bool fabTagIsTransformConvolve(FabricTag_ tag) {return matchSubType(FABRICISTRANSFORMCONVOLVE, tag);}

inline bool fabTagIsSqrt  (FabricTag_ tag) {return matchSubType(FABRICISSQRT  , tag);}
inline bool fabTagIsInvert(FabricTag_ tag) {return matchSubType(FABRICISINVERT, tag);}
inline bool fabTagIsCos   (FabricTag_ tag) {return matchSubType(FABRICISCOS   , tag);}
inline bool fabTagIsSin   (FabricTag_ tag) {return matchSubType(FABRICISSIN   , tag);}
inline bool fabTagIsClamp (FabricTag_ tag) {return matchSubType(FABRICISCLAMP , tag);}


inline bool fabTagIsComposite (FabricTag_ tag) {return matchSubType(FABRICISCOMPOSITE  , tag);}
inline bool fabTagIsMult      (FabricTag_ tag) {return matchSubType(FABRICISMULT       , tag);}
inline bool fabTagIsAdd       (FabricTag_ tag) {return matchSubType(FABRICISADD        , tag);}
inline bool fabTagIsFloatOr   (FabricTag_ tag) {return matchSubType(FABRICISFLOATOR    , tag);}
inline bool fabTagIsFloatXor  (FabricTag_ tag) {return matchSubType(FABRICISFLOATXOR   , tag);}
inline bool fabTagIsMin       (FabricTag_ tag) {return matchSubType(FABRICISMIN        , tag);}
inline bool fabTagIsMax       (FabricTag_ tag) {return matchSubType(FABRICISMAX        , tag);}
inline bool fabTagIsHSVAdjust (FabricTag_ tag) {return matchSubType(FABRICISHSVADJUST  , tag);}
inline bool fabTagIsTranparent(FabricTag_ tag) {return matchSubType(FABRICISTRANSPARENT, tag);}

inline TreeRootId_  fabTagTreeId      (FabricTag_ tag) {return fromFabHigh(tag);}
inline TransformId_ fabTagTransformId (FabricTag_ tag) {return fromFabHigh(tag);}
inline FabricData_  fabTagSubstanceRef(FabricTag_ tag) {return fromFabData(tag);}
inline FabricTagId_ fabTagAboveId     (FabricTag_ tag) {return fromFabHigh(tag);}
inline FabricTagId_ fabTagChildId     (FabricTag_ tag) {return fromFabLow (tag);}
#define fabTagBelowId fabTagChildId

inline ShapeId_ fromHighLimit(FabricTag_ fabricTag) {return (ShapeId_) (fabricTag >> 32        );}
inline ShapeId_ fromLowLimit (FabricTag_ fabricTag) {return (ShapeId_) (fabricTag & NULLSHAPEID);}


// ------------------- functions matching Graphics.Gudni.Principle.Point -------------------

#define HORIZONTALAXIS true
#define VERTICALAXIS   false

inline Space_ along  (bool axis, Point2_ p) {return axis ? p.x : p.y;}
inline Space_ athwart(bool axis, Point2_ p) {return axis ? p.y : p.x;}

inline Point2_ pointAlongAxis(bool axis, Space_ parentLine, Space_ parentCut) {
    if (axis) { return (Point2_)(parentCut,  parentLine); }
    else      { return (Point2_)(parentLine, parentCut ); }
}

// ------------------- functions matching Graphics.Gudni.Figure.Bezier.Type -------------------

inline Point2_ bzStart  (Bezier_ bez) {return bez.s01;}
inline Point2_ bzControl(Bezier_ bez) {return bez.s23;}
inline Point2_ bzEnd    (Bezier_ bez) {return bez.s45;}

bool inline shouldSubdivideBezier(Space_ tolerance, Bezier_ bez) {
    Point2_ midPoint = mid(bzStart(bez), bzEnd(bez));
    Space_ tDistance = taxiDistance(midPoint, bzControl(bez));
    return tDistance > tolerance;
}

// ------------------- functions matching Graphics.Gudni.Figure.Bezier.Cross -------------------


inline Bezier_ makeBez ( Point2_ start
                       , Point2_ control
                       , Point2_ end
                       ) {
    return (Bezier_)(start, control, end, ZEROPOINT);
}

inline Point2_ lerp(Space_ t, Point2_ a, Point2_ b) {
  return t * a + (1 - t) * b;
}

inline Bezier_ insideBezier(Space_ t, Bezier_ bez) {
  Point2_ mid0    = lerp(t, bzControl(bez), bzStart(bez)  );
  Point2_ mid1    = lerp(t, bzEnd(bez)    , bzControl(bez));
  Point2_ onCurve = lerp(t, mid1, mid0);
  return makeBez(mid0, onCurve, mid1);
}

inline void splitBezier( Space_ t
                       , Bezier_ bez
                       , Bezier_ *less
                       , Bezier_ *more
                       ) {
    Bezier_ m = insideBezier(t, bez);
    *less = makeBez(bzStart(bez), bzStart(m), bzControl(m));
    *more = makeBez(bzControl(m), bzEnd(m),   bzEnd(bez)  );
}

inline bool isKnobThreshold( Space_ threshold
                           , bool axis
                           , Bezier_ bez
                           ) {
    Space_ a = fabs(athwart(axis,bzStart  (bez)) - athwart(axis, bzControl(bez)));
    Space_ b = fabs(athwart(axis,bzControl(bez)) - athwart(axis, bzEnd    (bez)));
    Space_ c = fabs(athwart(axis,bzStart  (bez)) - athwart(axis, bzEnd    (bez)));
    return fabs ((a + b) - c) > threshold;
}

inline bool isKnobAbsolute(    bool axis
                          , Bezier_ bez
                          ) {
   return isKnobThreshold(0,axis,bez);
}

inline void initBezierStack (PMEM BzStack *stack) {
   stack->bezStackTop = 0;
}

inline void pushBezier( PMEM BzStack *stack
                      ,      Bezier_  bez
                      ,         Box_  box
                      ) {
    if (stack->bezStackTop < BEZIERSTACKSIZE - 1) {
        stack->bezStack[stack->bezStackTop] = bez;
        stack->boxStack[stack->bezStackTop] = box;
        stack->bezStackTop += 1;
    }
    else {
        DEBUG_IF(printf("error bezier Stack overrun.\n");)
    }
}

inline Bezier_ popBezier( PMEM BzStack *stack
                        ,      Bezier_ *bez
                        ,         Box_ *box
                        ) {
    Bezier_ ret;
    if (stack->bezStackTop > 0) {
        stack->bezStackTop -= 1;
        *bez = stack->bezStack[stack->bezStackTop];
        *box = stack->boxStack[stack->bezStackTop];
    }
    else {
      DEBUG_IF(printf("error bezier Stack underrun.\n");)
    }
    return ret;
}

inline bool emptyBezierStack ( PMEM BzStack *stack
                             ) {
    return stack->bezStackTop <= 0;
}

inline bool bezierSlopeLTEZero(bool axis, Bezier_ bez) {
    Space_ alo = along  (axis, bzEnd(bez)) - along  (axis, bzStart(bez));
    Space_ ath = athwart(axis, bzEnd(bez)) - athwart(axis, bzStart(bez));
    return ((alo > 0) != (ath > 0)) || (alo == 0);
}

inline bool isVertical  (bool axis) {return axis == VERTICALAXIS  ;}
inline bool isHorizontal(bool axis) {return axis == HORIZONTALAXIS;}

inline Space_ minAthwart(bool axis, Box_ box) {return athwart(axis, minBox(box));}
inline Space_ maxAthwart(bool axis, Box_ box) {return athwart(axis, maxBox(box));}
inline Space_ minAlong  (bool axis, Box_ box) {return along  (axis, minBox(box));}
inline Space_ maxAlong  (bool axis, Box_ box) {return along  (axis, maxBox(box));}

inline bool outsideOfRange (   bool axis
                           , Space_ start
                           , Space_ baseline
                           , Space_ end
                           ,   Box_ box
                           ) {
    return baseline >  maxAthwart(axis, box) ||
           baseline <= minAthwart(axis, box) ||
           start >     maxAlong  (axis, box) ||
           end   <=    minAlong  (axis, box) ;
}

inline Space_ bezHoriMin(Bezier_ bez) {return  min( min( bzStart(bez).x, bzControl(bez).x), bzEnd(bez).x); }
inline Space_ bezVertMin(Bezier_ bez) {return  min( min( bzStart(bez).y, bzControl(bez).y), bzEnd(bez).y); }
inline Space_ bezHoriMax(Bezier_ bez) {return  max( max( bzStart(bez).x, bzControl(bez).x), bzEnd(bez).x); }
inline Space_ bezVertMax(Bezier_ bez) {return  max( max( bzStart(bez).y, bzControl(bez).y), bzEnd(bez).y); }

inline Box_ boxOfBezier(Bezier_ bez) {
  return (Box_) ( bezHoriMin(bez)
                , bezVertMin(bez)
                , bezHoriMax(bez)
                , bezVertMax(bez)
                );
}

inline bool crossesBezierAlong (  Space_  limit
                               , BzStack *bzStack
                               ,    bool  axis
                               ,  Space_  start
                               ,  Space_  baseline
                               ,  Space_  end
                               , Bezier_  bez
                               ) {
    Box_ box;
    Bezier_ lessBez;
    Bezier_ moreBez;
    bool returnValue = false;
    if (start != end) {
        if (start > end) {
            Space_ temp = start;
            start = end;
            end = temp;
        }
        bool done = false;
        returnValue = false;
        box = boxOfBezier(bez);
        if (!outsideOfRange(axis, start, baseline, end, box)) {
            initBezierStack(bzStack);
            while (!done || !emptyBezierStack(bzStack)) {
                if (done) {
                    popBezier(bzStack, &bez, &box);
                }
                // DEBUG_IF(printf("      ret %i ", returnValue);showBezier(bez);printf("\n");)
                splitBezier(0.5, bez, &lessBez, &moreBez);
                Space_ size = max (maxAlong(axis, box) - minAlong(axis, box), maxAthwart(axis, box) - minAthwart(axis, box));
                bool   slopeLTEZero = bezierSlopeLTEZero(axis, bez);
                bool   offBaseline = baseline != maxAthwart(axis, box);
                bool   isK = isKnobAbsolute(axis, bez) || isKnobAbsolute(!axis, bez);
                if  ( (size >= limit &&
                      (
                       // curve size remains greater than the limit
                       offBaseline &&
                       (start > minAlong(axis, box) || end <= maxAlong(axis, box)) // and the start or end points are somewhere inside curve limits
                      ))
                      || isK
                    ) {
                    // or the curve creates a knob, meaning there could be more than one cross point
                    // must split
                    box = boxOfBezier(moreBez);
                    if (!outsideOfRange(axis, start, baseline, end, box)) {
                        pushBezier(bzStack, moreBez, box);
                    }
                    box = boxOfBezier(lessBez);
                    if (!outsideOfRange(axis, start, baseline, end, box)) {
                        bez = lessBez;
                        done = false;
                    }
                    else {
                        done = true;
                    }
                }
                else {
                    bool   barrierMin = slopeLTEZero || (!slopeLTEZero && (offBaseline && isVertical(axis)));
                    Space_ barrierPos = barrierMin ? minAlong(axis, box) : maxAlong(axis, box);
                    bool   startLTE   = slopeLTEZero || (!slopeLTEZero && (offBaseline || isHorizontal(axis)));
                    if (startLTE) {
                        returnValue = returnValue != (start <= barrierPos && end >  barrierPos);
                    }
                    else {
                        returnValue = returnValue != (start <  barrierPos && end >= barrierPos);
                    }
                    done = true;
                }
            }
        }
        // else {
        //    DEBUG_IF(printf("out   ret %i ", returnValue);showBezier(bez);printf("\n");)
        // }
    }
    return returnValue;
}

// --------------------- function from Graphics.Gudni.Raster.Dag.Primitive.Tag ---------------

#define PrimType_ PrimTag_

inline StorageId_ fromPrimStorage(PrimTag_ tag) {return (StorageId_) tag >> PRIMTAGSTORAGEIDSHIFT ; }
inline   ShapeId_ fromPrimShapeId(PrimTag_ tag) {return (ShapeId_)   tag &  PRIMTAGFABRICIDBITMASK; }
inline  PrimType_ primTagType    (PrimTag_ tag) {return              tag &  PRIMTAGTYPEBITMASK    ; }

inline       bool primTagIsBezier(PrimTag_ tag) {return primTagType(tag) == PRIMTAGISBEZIER   ; }
inline       bool primTagIsFacet (PrimTag_ tag) {return primTagType(tag) == PRIMTAGISFACET    ; }
inline       bool primTagIsRect  (PrimTag_ tag) {return primTagType(tag) == PRIMTAGISRECTANGLE; }
inline       bool primTagIsElipse(PrimTag_ tag) {return primTagType(tag) == PRIMTAGISELIPSE   ; }
inline  BezierId_ primTagBezierId(PrimTag_ tag) {return (BezierId_)fromPrimStorage(tag); }
inline   FacetId_ primTagFacetId (PrimTag_ tag) {return ( FacetId_)fromPrimStorage(tag); }
inline     BoxId_ primTagBoxId   (PrimTag_ tag) {return (   BoxId_)fromPrimStorage(tag); }
inline   ShapeId_ primTagShapeId (PrimTag_ tag) {return ( ShapeId_)fromPrimShapeId(tag); }

// --------------------- function from Graphics.Gudni.Figure.Substance.Color -------------------

#define rCh(rgba) rgba.s0
#define gCh(rgba) rgba.s1
#define bCh(rgba) rgba.s2
#define aCh(rgba) rgba.s3

#define hCh(hsva) hsva.s0
#define sCh(hsva) hsva.s1
#define vCh(hsva) hsva.s2
// aCh is the same as for rgba

inline bool isOpaque(Color_ color) {return aCh(color) >= (1-OPAQUETHRESHOLD);}
inline bool isClear (Color_ color) {return aCh(color) <= (  OPAQUETHRESHOLD);}

inline HsvColor_ rgbToHsv(Color_ rgba) {
    Space_ mx = max(max(rCh(rgba),gCh(rgba)),bCh(rgba));
    Space_ mn = min(min(rCh(rgba),gCh(rgba)),bCh(rgba));
    Space_ h = mx;
    Space_ s = mx;
    Space_ v = mx;

    Space_ d = mx - mn;
    s = mx == 0 ? 0 : d / mx;

    if (mx == mn) {
        h = 0; // achromatic
    } else {
        if      (mx == rCh(rgba))   { h = (gCh(rgba) - bCh(rgba)) / d + (gCh(rgba) < bCh(rgba) ? 6 : 0);}
        else if (mx == gCh(rgba))   { h = (bCh(rgba) - rCh(rgba)) / d + 2                              ;}
        else  /*(mx == bCh(rgba))*/ { h = (rCh(rgba) - gCh(rgba)) / d + 4                              ;}
        h = h / 6;
    }
    return (HsvColor_) (h, s, v, aCh(rgba));
}

inline Color_ hsvToRgb(HsvColor_ hsva) {
    int i = floor(hCh(hsva) * 6);
    Space_ f = hCh(hsva) * 6 - (Space_)i;
    Space_ p = vCh(hsva) * (1 - sCh(hsva));
    Space_ q = vCh(hsva) * (1 - f * sCh(hsva));
    Space_ t = vCh(hsva) * (1 - (1 - f) * sCh(hsva));
    Space_ v = vCh(hsva);
    Color_ ret;
    switch (i % 6) {
        case 0: ret = (Color_) (v, t, p, aCh(hsva)); break;
        case 1: ret = (Color_) (q, v, p, aCh(hsva)); break;
        case 2: ret = (Color_) (p, v, t, aCh(hsva)); break;
        case 3: ret = (Color_) (p, q, v, aCh(hsva)); break;
        case 4: ret = (Color_) (t, p, v, aCh(hsva)); break;
        case 5: ret = (Color_) (v, p, q, aCh(hsva)); break;
    }
    return ret;
}

inline Color_ composite(Color_ foreground, Color_ background) {
  float alphaOut = aCh(foreground) + aCh(background) * (1.0f - aCh(foreground));
  if (alphaOut > 0) {
     Color_ color = ((foreground * aCh(foreground)) + (background * aCh(background) * (1.0f - aCh(foreground)))) / alphaOut;
     return (Color_) (rCh(color),gCh(color),bCh(color),alphaOut);
  }
  else {
    return CLEARBLACK;
  }
}

inline Color_ adjustHsva(HsvColor_ amountHsva, Color_ rgba) {
  HsvColor_ hsva = rgbToHsv(rgba);
  Space_ temp;
  hsva = (HsvColor_) ( fract(hCh(hsva) + hCh(amountHsva),&temp) // should rotate around 1.0
                     , clamp(sCh(hsva) + sCh(amountHsva), (Space_)0, (Space_)1)
                     , clamp(sCh(hsva) + sCh(amountHsva), (Space_)0, (Space_)1)
                     , aCh(hsva)
                     );
  return hsvToRgb(hsva);
}

inline Color_ transparent(Space_ amount, Color_ rgba) {
  aCh(rgba) = amount;
  return rgba;
}


// ------------------- functions from Graphics.Gudni.Figure.Facet.Traverse -------------------

#define t0(t) t.s01
#define t1(t) t.s23
#define t2(t) t.s45

inline Tri_ rotateTri1(Tri_ t) {return t.s23450167;}
inline Tri_ rotateTri2(Tri_ t) {return t.s45012367;}

inline Tri_ sideTri(Tri_ i) {return 0.5f * ((Tri_)(t0(i),t0(i),t0(i),ZEROPOINT) + i);}

inline Tri_ sideTriIndex(int i, Tri_ tri) {
    Tri_ ret;
    switch(i) {
        case 0: ret = sideTri (           tri ); break;
        case 1: ret = sideTri (rotateTri2(tri)); break;
        case 2: ret = sideTri (rotateTri1(tri)); break;
    }
    return ret;
}

inline Tri_ centerTri(Tri_ i) {return 0.5f * (rotateTri1(i) + rotateTri2(i));}

#define p0(t) t.s01
#define c0(t) t.s23
#define p1(t) t.s45
#define c1(t) t.s67
#define p2(t) t.s89
#define c2(t) t.sab

inline BezTri_ rotateBezTri1(BezTri_ t) {return t.s456789ab0123cdef;}
inline BezTri_ rotateBezTri2(BezTri_ t) {return t.s89ab01234567cdef;}

#define Point2Pair_ Space4_

inline Point2Pair_ side0(BezTri_ i) {
    Point2_ aP0 = 0.5f * (p0(i) + p0(i));
    Point2_ aC0 = 0.5f * (c0(i) + p0(i));
    return (Point2Pair_) (aP0, aC0);
}

inline Point2Pair_ side1(BezTri_ i) {
    Point2_ aP1 = 0.25f * (p1(i) + p0(i) + c0(i) + c0(i));
    Point2_ aC1 = 0.25f * (c1(i) + p0(i) + c0(i) + c2(i));
    return (Point2Pair_) (aP1, aC1);
}

inline Point2Pair_ side2(BezTri_ i) {
  Point2_ aP2 = 0.25f * (p2(i) + p0(i) + c2(i) + c2(i));
  Point2_ aC2 = 0.5f  * (c2(i) + p0(i));
  return (Point2Pair_)(aP2, aC2);
}

inline BezTri_ sideTriangle(BezTri_ bezTri) {
    return (BezTri_) ( side0(bezTri)
                     , side1(bezTri)
                     , side2(bezTri)
                     , (Point2Pair_)(ZEROPOINT,ZEROPOINT)
                     );
}

inline BezTri_ flipSidesBezTri(BezTri_ i) {
    return (BezTri_)( p0(i), c1(i)
                    , p1(i), c2(i)
                    , p2(i), c0(i)
                    , ZEROPOINT, ZEROPOINT
                    );
}

inline Space4_ innerSide (BezTri_ t) {
    return (Space4_)(p1(t),c1(t));
}

inline BezTri_ centerBezTri(BezTri_ t) {
    return flipSidesBezTri(
        (BezTri_)( innerSide(sideTriangle(              t ))
                 , innerSide(sideTriangle(rotateBezTri1(t)))
                 , innerSide(sideTriangle(rotateBezTri2(t)))
                 , (Point2Pair_) (ZEROPOINT, ZEROPOINT)
                 )
    );
}

inline BezTri_ sideBezTri(int i, BezTri_ t) {
    BezTri_ ret;
    switch (i) {
        case 0: ret = sideTriangle(              t ); break;
        case 1: ret = sideTriangle(rotateBezTri1(t)); break;
        case 2: ret = sideTriangle(rotateBezTri2(t)); break;
    }
    return ret;
}

inline bool insideBezierTri(Space_ limit, BzStack *bzStack, Point2_ p, BezTri_ t) {
    return crossesBezTriAlong(limit, bzStack, VERTICALAXIS, MAXFLOAT, p.x, p.y, t);
}

inline Bezier_ bezierBezTri0 (BezTri_ t) { return makeBez(p0(t),c0(t),p1(t)); }
inline Bezier_ bezierBezTri1 (BezTri_ t) { return makeBez(p1(t),c1(t),p2(t)); }
inline Bezier_ bezierBezTri2 (BezTri_ t) { return makeBez(p2(t),c2(t),p0(t)); }

inline bool shouldSubdivideFacet(Space_ tolerance, Facet t) {
    return
    shouldSubdivideBezier(tolerance, bezierBezTri0(t.facetOutput)) ||
    shouldSubdivideBezier(tolerance, bezierBezTri1(t.facetOutput)) ||
    shouldSubdivideBezier(tolerance, bezierBezTri2(t.facetOutput));
}

inline Facet traverseFacet(Space_ limit, BzStack *bzStack, Facet facet, Point2_ point) {
    bool found = false;
    int i = 0;
    while (!found && i<3) {
        BezTri_ sideOut = sideBezTri(i,facet.facetOutput);
        if (insideBezierTri(limit, bzStack, point, sideOut)) {
            facet.facetOutput = sideOut;
            facet.facetInput  = sideTriIndex(i, facet.facetInput);
            found = true;
        }
    }
    if (!found) {
        facet.facetOutput = centerBezTri(facet.facetOutput);
        facet.facetInput  = centerTri(facet.facetInput);
    }
    return facet;
}

inline Facet traverseFacetUntil(Space_ limit, Space_ flatness, BzStack *bzStack, Facet facet, Point2_ point) {
    while (shouldSubdivideFacet(flatness, facet)) {
        facet = traverseFacet(limit, bzStack, facet, point);
    }
    return facet;
}

// --------------------- function from Graphics.Gudni.Raster.Dag.Primitive.Cross ---------------

inline bool rectIncludesPoint(Box_ boundary, Point2_ p) {
    return p.x >  maxBox(boundary).x ||
           p.y >  maxBox(boundary).y ||
           p.x <= minBox(boundary).x ||
           p.y <= minBox(boundary).y ;
}

inline Space_ quadrance(Point2_ p) {return (p.x * p.x) + (p.y * p.y);}

inline bool ellipseIncludesPoint(Box_ boundary, Point2_ p) {
    Point2_ center = centerBox(boundary);
    Point2_ size   = sizeBox  (boundary);
    if (size.x > 0 && size.y > 0) {
        Point2_ adjustedDist = (p - center) / (size / 2);
        return rectIncludesPoint(boundary, p) && (quadrance(adjustedDist) >= 4);
    }
    else {
        return false;
    }
}

bool crossesBezTriAlong(  Space_  limit
                       , BzStack *bzStack
                       ,    bool  axis
                       ,  Space_  start
                       ,  Space_  baseline
                       ,  Space_  end
                       ,  BezTri_ t
                       ) {
    return
    crossesBezierAlong(limit, bzStack, axis, start, baseline, end, bezierBezTri0(t)) !=
    crossesBezierAlong(limit, bzStack, axis, start, baseline, end, bezierBezTri1(t)) !=
    crossesBezierAlong(limit, bzStack, axis, start, baseline, end, bezierBezTri2(t));
}

inline bool crossesFacetAlong (  Space_ limit
                              , BzStack *bzStack
                              ,    bool axis
                              ,  Space_ start
                              ,  Space_ baseline
                              ,  Space_ end
                              ,  Facet t
                              ) {
    return crossesBezTriAlong(limit, bzStack, axis, start, baseline, end, t.facetOutput);
}

inline bool crossesRectAlong( bool axis
                            , Space_ start
                            , Space_ baseline
                            , Space_ end
                            , Box_ rect
                            ) {
  Point2_ s = pointAlongAxis(axis, start, baseline);
  Point2_ e = pointAlongAxis(axis, end  , baseline);
  return rectIncludesPoint(rect, s) !=
         rectIncludesPoint(rect, e) ;
}

inline bool crossesEllipseAlong(   bool axis
                               , Space_ start
                               , Space_ baseline
                               , Space_ end
                               ,   Box_ rect
                               ) {
  Point2_ s = pointAlongAxis(axis, start, baseline);
  Point2_ e = pointAlongAxis(axis, end  , baseline);
  return ellipseIncludesPoint(rect, s) !=
         ellipseIncludesPoint(rect, e) ;
}

inline Bezier_ loadBezier(Dag *dag, PrimTag_ tag) {
    BezierId_ bezierId = primTagBezierId(tag);
    Bezier_ bez = *((Bezier_ *)(dag->dagPrimBezierHeap + (bezierId * BEZIERSIZEINFLOATS)));
    bez.s67 = (Space2_)(0,0);
    return bez;
}

inline Facet loadFacet(Dag *dag, PrimTag_ tag) {
    FacetId_ facetId = primTagFacetId(tag);
    return *((Facet *)(dag->dagPrimFacetHeap + (facetId * FACETSIZEINFLOATS)));
}

inline Box_ loadBox(Dag *dag, PrimTag_ tag) {
    BoxId_ rectId = primTagBoxId(tag);
    return *((Box_ *)(dag->dagPrimBoxHeap + (rectId * BOXSIZEINFLOATS)));
}

inline bool crossesPrimAlong(   Space_  limit
                            ,  BzStack *bzStack
                            ,     bool  axis
                            ,      Dag *dag
                            ,   Space_  start
                            ,   Space_  baseline
                            ,   Space_  end
                            , PrimTag_  tag
                            ) {
   bool ret;
   if (primTagIsBezier(tag)) {
       Bezier_ bez = loadBezier(dag, tag);
       ret = crossesBezierAlong(limit, bzStack, axis, start, baseline, end, bez);
       // DEBUG_IF(printf("primTagIsBezier ret %i axis %i start %f baseline %f end %f ",ret,axis,start,baseline,end);showBezier(bez);printf("\n");)
   }
   else if (primTagIsFacet(tag)) {
       ret = crossesFacetAlong(limit, bzStack, axis, start, baseline, end, loadFacet(dag, tag));
   }
   else if (primTagIsRect(tag)) {
       ret = crossesRectAlong(axis, start, baseline, end, loadBox(dag, tag));
   }
   else { // primTagIsElipse
       ret = crossesEllipseAlong(axis, start, baseline, end, loadBox(dag, tag));
   }
   return ret;
}

inline Point2_ interimPoint(Point2_ start, Point2_ end) {return (Point2_)(start.x, end.y);}

inline bool crossesPrim(   Space_  limit
                       ,  BzStack *bzStack
                       ,      Dag *dag
                       , PrimTag_  primTag
                       ,  Point2_  start
                       ,  Point2_  end
                       ) {
    Point2_ iP = interimPoint(start, end);
    return  crossesPrimAlong (limit, bzStack, VERTICALAXIS  , dag, start.y, start.x,  iP.y, primTag) !=
            crossesPrimAlong (limit, bzStack, HORIZONTALAXIS, dag,    iP.x,    iP.y, end.x, primTag)    ;
}

// ------------------- functions from Graphics.Gudni.Figure.Facet.Barycentric -------------------

inline Point2_ project( Point2_ u
                      , Point2_ v
                      ) {
  Space_ uq = quadrance(u);
  return (uq > 0) ? ((dot(v, u) / quadrance(u)) * u) : 0;
}

inline Point2_ inverseTriangle(Facet f, Point2_ p) {
    Point2_ o  = p0(f.facetOutput)     ;
    Point2_ ou = p1(f.facetOutput) - o ;
    Point2_ ov = p2(f.facetOutput) - o ;
    Point2_ pT = p - o                 ;
    Point2_ pu = project(pT, ou)       ;
    Point2_ pv = project(pT, ov)       ;
    Point2_ to = t0(f.facetInput)      ;
    Point2_ tu = t1(f.facetInput) - to ;
    Point2_ tv = t2(f.facetInput) - to ;
    return  to + (pu * tu + pv * tv)   ;
}

inline Point2_ inverseFacet(Space_ limit, Space_ flatness, BzStack *bzStack, Facet facet, Point2_ ray) {
    return inverseTriangle(traverseFacetUntil(limit, flatness, bzStack, facet, ray), ray);
}

// ------------------- functions from Graphics.Gudni.Figure.Principle.Affine -------------------

// Other orientation
// inline Space3_ matrixProduct9x3(Space9_ a, Space3_ b) {
//    return (Space3_) ( a.s0 * b.s0 + a.s1 * b.s1 + a.s2 * b.s2
//                     , a.s3 * b.s0 + a.s4 * b.s1 + a.s5 * b.s2
//                     , a.s6 * b.s0 + a.s7 * b.s1 + a.s8 * b.s2
//                     );
//   }
// }

inline Space2_ matrixProduct9x3Part(Space9_ a, Space3_ b) {
   return (Space2_) ( a.s0 * b.s0 + a.s1 * b.s1 + a.s2 * b.s2
                    , a.s3 * b.s0 + a.s4 * b.s1 + a.s5 * b.s2
                    );
}

inline Point2_ applyAffine(Affine_ a, Point2_ p) {
  Space3_ row = (Space4_) (p.x, p.y, 1, 0);
  return matrixProduct9x3Part(a, row);
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.Fabric.Storage -------------------

inline Affine_ loadAffine(Dag *dag, FabricTag_ tag) {
    TransformId_ transformId = fabTagTransformId(tag);
    return *((Affine_ *)(dag->dagFabricHeap + transformId));
}

inline Space_ loadConvolve(Dag *dag, FabricTag_ tag) {
    TransformId_ transformId = fabTagTransformId(tag);
    return *((Space_ *)(dag->dagFabricHeap + transformId));
}

// A picture reference is a reference to bitmap data that can be the substance of a shape.

inline Color_ loadSubstanceColor(        Dag *dag
                                , FabricTag_  tag
                                ) {
    return *((Color_ *)(dag->dagFabricHeap + fabTagSubstanceRef(tag)));
}

inline PictUse loadPictUse(        Dag *dag
                          , FabricTag_  tag
                          ) {
    return *((PictUse*)(dag->dagFabricHeap + fabTagSubstanceRef(tag)));
}

// ------------------- Graphics.Gudni.Raster.Dag.Fabric.Substance.Query -------------------

inline Color_ emptyQuery () {return CLEARBLACK ;}
inline Color_ insideShape() {return OPAQUEWHITE;}


inline Color_ getPixel(Dag *dag, PictUse use, Loc2_ p) {
     if (p.x >= 0 &&
         p.y >= 0 &&
         p.x < use.pictSize.x &&
         p.y < use.pictSize.y) {
         int w = use.pictSize.x;
         return *(dag->dagPictHeap + use.pictMemOffset) + mul24(p.y, w) + p.x;
     }
     else {
         return emptyQuery();
     }
}

inline Color_ querySubstanceColor(Dag *dag, FabricTag_ tag, Point2_ ray) {
    Color_ ret;
    if (fabTagIsConstant(tag)) {
        ret = loadSubstanceColor(dag, tag);
    }
    else if (fabTagIsTexture(tag)) {
        ret = getPixel(dag, loadPictUse(dag, tag), (Loc2_)floor(ray));
    }
    else if (fabTagIsLinear(tag)) {
        ret = (Color_) ray.y;
    }
    else if (fabTagIsQuadrance(tag)) {
        ret = (Color_)quadrance(ray);
    }
    return ret;
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.Fabric.Ray.Class -------------------

inline Color_ applyFilter(Dag *dag, FabricTag_ tag, Color_ color) {
    Color_ ret;
    if      (fabTagIsSqrt  (tag)) {ret = sqrt(color)                        ;}
    else if (fabTagIsInvert(tag)) {ret = 1-color                            ;}
    else if (fabTagIsCos   (tag)) {ret = cos(color)                         ;}
    else if (fabTagIsSin   (tag)) {ret = sin(color)                         ;}
    else if (fabTagIsClamp (tag)) {ret = clamp(color,CLEARBLACK,OPAQUEWHITE);}
    else                     {ret = color                              ;}
    return ret;
}

inline Point2_ rayApplyTransform(Space_ limit, Space_ flatness, Dag *dag, BzStack *bzStack, FabricTag_ tag, Point2_ ray) {
    Point2_ ret;
    if (fabTagIsTransformAffine(tag)) {
        Affine_ affine = loadAffine(dag, tag);
        DEBUG_IF(showAffine(affine);)
        ret =applyAffine(affine, ray);
    }
    else if (fabTagIsTransformFacet(tag)) {
        ret = inverseFacet(limit, flatness, bzStack, loadFacet(dag, tag),  ray );
    }
    else if (fabTagIsTransformConvolve(tag)) {
        ret = ray + ((Point2_)(getRandom(dag),getRandom(dag)) * loadConvolve(dag, tag));
    }
    return ret;
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.Fabric.Combine.Query -------------------

inline Color_ traverseCombine(FabricTag_ tag, Color_ a, Color_ b) {
    Color_ ret;
    if      (fabTagIsComposite (tag)) {ret = composite(a,b)        ;}
    else if (fabTagIsMult      (tag)) {ret = a * b                 ;}
    else if (fabTagIsAdd       (tag)) {ret = a + b                 ;}
    else if (fabTagIsFloatOr   (tag)) {ret = a + b - (a * b)       ;}
    else if (fabTagIsFloatXor  (tag)) {ret = a + b - (2 * (a * b)) ;}
    else if (fabTagIsMin       (tag)) {ret = min(a,b)              ;}
    else if (fabTagIsMax       (tag)) {ret = max(a,b)              ;}
    else if (fabTagIsHSVAdjust (tag)) {ret = adjustHsva(a,b)       ;}
    else if (fabTagIsTranparent(tag)) {ret = transparent(aCh(a),b) ;}
    return ret;
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Decorate -------------------

inline void insertShapeId(ShapeStack *stack, int i, ShapeId_ shapeId) {
    for (int j = min(SHAPESTACKSIZE-1,stack->stSize); j > i; j--) {
        stack->stVector[j] = stack->stVector[j-1];
    }
    stack->stVector[i] = shapeId;
    stack->stSize = min(SHAPESTACKSIZE,stack->stSize+1);
}

inline void deleteShapeId(ShapeStack *stack, int i) {
    for (int j = i; j < stack->stSize-1; j++) {
        stack->stVector[j] = stack->stVector[j+1];
    }
    stack->stSize -= 1;
}

inline void toggleShapeActive( ShapeStack *stack
                             ,   ShapeId_  newShapeId
                             ) {
    bool done = false;
    int i = 0;
    while (i < stack->stSize && !done) {
        ShapeId_ oldShapeId = stack->stVector[i];
        if (newShapeId == oldShapeId) {
            // it is the same exact tag so delete it from the stack
            deleteShapeId(stack,i);
            done = true;
        }
        else if (newShapeId < oldShapeId) {
            insertShapeId(stack,i,newShapeId);
            done = true;
        }
        i++;
    }
    if (!done) { // if we reach the bottom just insert on the end.
        insertShapeId(stack,i,newShapeId);
    }
}

inline void combineShapeStacks (        Dag *dag
                               , ShapeStack *stack
                               ,    DecoTag  tree
                               ) {
    for ( uint i = sliceStart(tree.decoTagCrossings)
        ; i < (sliceStart(tree.decoTagCrossings) + sliceLength(tree.decoTagCrossings))
        ; i ++
        ) {
        toggleShapeActive(stack, dag->dagCrossingHeap[(int)i]);
    }
}

inline Point2_ traverseDecorateTree(        Dag *dag
                                   , ShapeStack *shapeStack
                                   ,    Point2_  ray
                                   , DecoTagId_  treeId
                                   ) {
    bool done = false;
    Point2_ anchor;
    Space_ parentCut  = (-MAXFLOAT);
    Space_ parentLine = (-MAXFLOAT);
    int count = 10;
    bool axis = false; // is vertical
    while (!done && count > 0) {
        if (treeId == NULLDECOTAGID) {
            //DEBUG_IF(printf("nullDecoTagId\n");)
            anchor = pointAlongAxis(axis, parentCut, parentLine);
            done = true;
        }
        else {
            //DEBUG_IF(printf("decoTreeId %i\n", treeId);)
            DecoTag tree = dag->dagTreeDecoHeap[(int)treeId];
            //axis = tree.decoTagHorizontal;
            Space_ cut = tree.decoTagCut;
            combineShapeStacks(dag, shapeStack, tree);
            parentCut  = parentLine;
            parentLine = cut;
            if (athwart(axis, ray) < cut) {
                treeId = tree.decoTagLessCut;
            }
            else {
                treeId = tree.decoTagMoreCut;
            }
        }
        count -= 1;
        axis = !axis;
    }
    //DEBUG_IF(printf("decorated anchor %v2f ", anchor);showShapeStack(shapeStack);)
    return anchor;
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Query -------------------

inline void modifyItemStackIfCrossed(     Space_  limit
                                    ,        Dag *dag
                                    ,    BzStack *bzStack
                                    , ShapeStack *stack
                                    ,    Point2_  start
                                    ,    Point2_  end
                                    , PrimTagId_  primTagId
                                    ) {
    PrimTag_ primTag = dag->dagPrimTagHeap[(int)primTagId];
    if (crossesPrim(limit, bzStack, dag, primTag, start, end)) {
        toggleShapeActive (stack, primTagShapeId(primTag));
    }
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Traverse -------------------

inline void initTreeStack(TreeStack *stack) {
    stack->trSize = 0;
}

inline ConfineTagId_ popTreeStack(TreeStack *stack) {
    stack->trSize -= 1;
    return stack->trStack[stack->trSize];
}

inline void pushTreeStack(TreeStack *stack, ConfineTagId_ t) {
    stack->trStack[stack->trSize] = t;
    stack->trSize += 1;
}

inline ConfineTagId_ emptyTreeStack(TreeStack *stack) {
    return stack->trSize == 0;
}

inline Box_ boxAroundPoints(Point2_ a, Point2_ b) {
    return (Box_) (min(a, b), max(a,b));
}

inline void traverseCTBox(        Space_  limit
                         ,           Dag *dag
                         ,       BzStack *bzStack
                         ,    ShapeStack *shapeStack
                         ,     TreeStack *spine
                         ,       Point2_  anchor
                         ,       Point2_  ray
                         , ConfineTagId_  treeId
                         ) {
    initTreeStack(spine);
    Box_ box = boxAroundPoints(anchor, ray);
    pushTreeStack(spine, treeId);
    while (!emptyTreeStack(spine)) {
        treeId = popTreeStack(spine);
        // DEBUG_IF(printf("confineTreeId %i\n", treeId);)
        // DEBUG_IF(showShapeStack(shapeStack);)
        if (treeId != NULLCONFINETAGID) {
            ConfineTag tree = dag->dagTreeConfineHeap[(int)treeId];
            bool axis = tree.confineTagHorizontal == 1;
            if (athwart(axis, maxBox(box)) > tree.confineTagCut) {
                pushTreeStack(spine, tree.confineTagMoreCut);
            }
            if (athwart(axis, minBox(box)) <= tree.confineTagOverhang) {
                pushTreeStack(spine, tree.confineTagLessCut);
            }
            modifyItemStackIfCrossed(limit, dag, bzStack, shapeStack, anchor, ray, tree.confineTagPrimTagId);
        }
    }
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Query -------------------

inline ConfineTagId_ rootConfineTagId(TreeRoot_ root) {return root.x;}
inline DecoTagId_    rootDecoTagId   (TreeRoot_ root) {return root.y;}

inline void queryConfineTreePoint(     Space_   limit
                                 ,        Dag  *dag
                                 ,    BzStack  *bzStack
                                 , ShapeStack  *shapeStack
                                 ,  TreeStack  *spine
                                 ,  TreeRoot_   root
                                 ,     Point2_  ray
                                 ) {
    Point2_ anchor = traverseDecorateTree(dag, shapeStack, ray, rootDecoTagId(root));
    traverseCTBox(limit, dag, bzStack, shapeStack, spine, anchor, ray, rootConfineTagId(root));
    // DEBUG_IF(printf("anchor %v2f ", anchor);showShapeStack(shapeStack);)
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.Fabric.Traverse -------------------

inline void initShapeStack(ShapeStack *shapeStack) {
  shapeStack->stSize = 0;
}

#define RANGEBITMASK  0x7FFF
#define RANGEMAYBEBIT 0x8000



inline   bool rangeMaybe(StackRange_ mRange) {return ((mRange.x & RANGEMAYBEBIT) == RANGEMAYBEBIT  ) ;}
inline Index_ rangeMin  (StackRange_ mRange) {return (Index_) (mRange.x & RANGEBITMASK) ;}
inline Index_ rangeMax  (StackRange_ mRange) {return (Index_) (mRange.y                ) ;}

inline StackRange_ makeStackRange(bool maybe, Index_ rangeMin, Index_ rangeMax) {
  short mn = maybe ? (RANGEMAYBEBIT | (short) rangeMin) : (short) rangeMin;
  short mx = (short) rangeMax;
  return ((StackRange_)(mn, mx));
}

inline StackRange_ shapeStackRange(ShapeStack *stack) {
   return makeStackRange(true, 0, stack->stSize);
}

inline StackRange_ justRange(Index_ mn, Index_ mx) {
  return makeStackRange (true, mn, mx);
}

inline StackRange_ nothingRange() {
  return makeStackRange (false, 0, 0);
}

inline bool hasRange(StackRange_ range) {
    return (rangeMaybe(range) && (rangeMax(range) > rangeMin(range)))
           || (!rangeMaybe(range));
}

inline int findSubt( ShapeStack *stack
                   , ShapeId_ cutPoint
                   , int minLimit
                   , int i
                   ) {
    ShapeId_ x = stack->stVector[i-1];
    while ( i > minLimit || x >= cutPoint ) {
        i = i - 1;
        x = stack->stVector[i];
    }
    return i;
}

inline void initTraverseState(TraverseState *state) {
    state->tsSize = 0;
    state->tsColorSize = 0;
    initShapeStack(&state->tsShapeStack);
}

inline bool traverseStop( FabricTag_ fabricTag
                        ,     Color_ aboveQ
                        ) {
    if (fabTagIsComposite(fabricTag)) {
        return isOpaque(aboveQ);
    }
    else if (fabTagIsMult(fabricTag)) {
        // DEBUG_IF(printf("traverse stop aboveQ %v4f iClear(aboveQ) \n", aboveQ, isClear(aboveQ));)
        return isClear (aboveQ);
    }
    else {
        return false;
    }
}

inline FabricTag_ loadFabricTag     (Dag *dag, FabricTagId_ fabricTagId) {return            dag->dagFabricTagHeap[(int)fabricTagId    ]; }
inline ShapeId_   loadFabricCutPoint(Dag *dag, FabricTagId_ fabricTagId) {return (ShapeId_) dag->dagFabricTagHeap[(int)fabricTagId + 1]; }
inline TreeRoot_  loadTreeRoot      (Dag *dag, TreeRootId_  treeId     ) {return            dag->dagTreeRootHeap [(int)treeId]         ; }

inline void pushFabric( TraverseState *state
                      ,  FabricTagId_  fabricTagId
                      ,   StackRange_  mRange
                      ,        Stage_  stage
                      ,       Point2_  ray
                      ) {
    state->tsFabricTagIds[state->tsSize] = fabricTagId;
    state->tsRanges      [state->tsSize] = mRange;
    state->tsStages      [state->tsSize] = stage;
    state->tsRays        [state->tsSize] = ray;
    state->tsSize += 1;
}

inline void popFabric( TraverseState *state
                     ,  FabricTagId_ *fabricTagId
                     ,   StackRange_ *mRange
                     ,        Stage_ *stage
                     ,       Point2_ *ray
                     ) {
    state->tsSize -= 1;
    *fabricTagId = state->tsFabricTagIds[state->tsSize];
    *mRange      = state->tsRanges      [state->tsSize];
    *stage       = state->tsStages      [state->tsSize];
    *ray         = state->tsRays        [state->tsSize];
}

inline bool emptyFabric(TraverseState *state
                       ) {
    return state->tsSize == 0;
}

inline void pushColor( TraverseState *state
                     , Color_ color
                     ) {
    state->tsColorStack[state->tsColorSize] = color;
    state->tsColorSize += 1;
}

inline Color_ popColor( TraverseState *state
                      ) {
    state->tsColorSize -= 1;
    return state->tsColorStack[state->tsColorSize];
}

inline ShapeId_ limitMax(ShapeId2_ limits) {return limits.y;}

#define FIRSTSTAGE  0
#define SECONDSTAGE 1
#define THIRDSTAGE  2

inline void splitWithCutPoint ( TraverseState *state
                              ,      ShapeId_  cutPoint
                              ,   StackRange_  mRange
                              ,   StackRange_ *aboveRange
                              ,   StackRange_ *belowRange
                              ) {
    if (!rangeMaybe(mRange)) {
        *aboveRange = nothingRange();
        *belowRange = nothingRange();
    }
    else {
        if (cutPoint == NULLSHAPEID) {
            *aboveRange = mRange;
            *belowRange = mRange;
        }
        else {
            Index_ cutIndex = findSubt(&(state->tsShapeStack), cutPoint, rangeMin(mRange),rangeMax(mRange));
            *aboveRange = justRange(rangeMin(mRange), cutIndex);
            *belowRange = justRange(cutIndex, rangeMax(mRange));
        }
    }
}

inline Color_ traverseFabric(       Space_  limit
                            ,       Space_  flatness
                            ,TraverseState *state
                            ,          Dag *dag
                            ,      Point2_  ray
                            , FabricTagId_  tagId
                            ) {
    initTraverseState(state);
    bool done = false;
    Stage_ stage = FIRSTSTAGE;
    Color_ aboveQ, belowQ;
    StackRange_ aboveRange, belowRange, mRange;
    while (!done || !emptyFabric(state)) {
        DEBUG_IF(printf("START\n");)
        // DEBUG_IF(printf("!done %i || !emptyFabric(state) %i\n ",!done, !emptyFabric(state));)
        if (done) {
             popFabric(state, &tagId, &mRange, &stage, &ray);
             DEBUG_IF(printf("pop -> tagId %i mRange %i, %i, %i stage %i ray %v2f\n", tagId, rangeMaybe(mRange), rangeMin(mRange), rangeMax(mRange), stage, ray);)
             done = false;
        } // if done
        if (!hasRange(mRange)) {
          //DEBUG_IF(printf("not has range tagId %i mRange %i, %i, %i ray %v2f\n", tagId, rangeMaybe(mRange), rangeMin(mRange), rangeMax(mRange), ray);)
          DEBUG_IF(printf("not has range\n");)
          pushColor(state, emptyQuery());
          done = true;
        }
        else {
            if (tagId == NULLFABRICTAGID) {
                DEBUG_IF(printf("tagId null mRange %i, %i, %i stage %i ray %v2f\n", rangeMaybe(mRange), rangeMin(mRange), rangeMax(mRange), stage, ray);)
                pushColor(state, insideShape());
                done = true;
             }
             else {
                 DEBUG_IF(printf("%i \n ", tagId);)
                 FabricTag_ tag = loadFabricTag(dag, tagId);
                 //DEBUG_IF(showFabricTagType(tag);)
                 //DEBUG_IF(printf("mRange %i, %i, %i stage %i ray %v2f\n", rangeMaybe(mRange), rangeMin(mRange), rangeMax(mRange), stage, ray);)
                 if (fabTagIsLeaf(tag)) {
                     pushColor(state, querySubstanceColor(dag, tag, ray));
                     done = true;
                 } // fabTagIsLeaf
                 else if (fabTagIsUnaryPre(tag)) {
                     if (fabTagIsTree(tag)) {
                         TreeRoot_ root = loadTreeRoot(dag, fabTagTreeId(tag));
                         queryConfineTreePoint(limit, dag, &(state->tsBzStack), &(state->tsShapeStack), &(state->tsTreeStack), root, ray);
                         // go
                         tagId = fabTagChildId(tag);
                         mRange = shapeStackRange(&(state->tsShapeStack));
                         stage = FIRSTSTAGE;
                         // ray = ray
                     }
                     else { // must be transformer
                         // go
                         tagId  = fabTagChildId(tag);
                         mRange = nothingRange();
                         stage  = FIRSTSTAGE;
                         ray    = rayApplyTransform(limit, flatness, dag, &(state->tsBzStack), tag, ray);
                     }
                 } // fabTagIsUnaryPre
                 else if (fabTagIsUnaryPost(tag)) {
                     switch (stage) {
                         case FIRSTSTAGE:
                             pushFabric(state, tagId, nothingRange(), SECONDSTAGE, ray);
                             //go
                             tagId = fabTagChildId(tag);
                             // mRange = mRange
                             // stage = stage
                             // ray = ray
                             break;
                         case SECONDSTAGE:
                             pushColor(state, applyFilter(dag, tag, popColor(state)));
                             done = true;
                             break;
                      } // switch stage
                 } // fabTagIsUnaryPost
                 else if (fabTagIsBinaryOp(tag)) {
                     ShapeId_ cutPoint = loadFabricCutPoint(dag, tagId);
                     switch (stage) {
                         case FIRSTSTAGE:
                             //DEBUG_IF(printf("%i FIRSTSTAGE\n", tagId);)
                             splitWithCutPoint(state, cutPoint, mRange, &aboveRange, &belowRange);
                             pushFabric(state, tagId, belowRange, SECONDSTAGE, ray);
                             //go
                             tagId = fabTagAboveId(tag);
                             mRange = aboveRange;
                             // stage = stage
                             // ray = ray
                             break;
                         case SECONDSTAGE:
                             aboveQ = popColor(state);
                             pushColor(state, aboveQ);
                             //DEBUG_IF(printf("%i SECONDSTAGE aboveQ ", tagId);showColor(aboveQ);printf("\n");)
                             if (traverseStop(tag, aboveQ)) {
                                 done = true;
                             }
                             else {
                                 pushFabric(state, tagId, nothingRange(), THIRDSTAGE, ray);
                                 //go
                                 tagId = fabTagBelowId(tag);
                                 // mRange = mRange
                                 stage = FIRSTSTAGE;
                                 // ray = ray
                             }
                             break;
                         case THIRDSTAGE:
                             belowQ = popColor(state);
                             aboveQ = popColor(state);
                             DEBUG_IF(printf("%i THIRDSTAGE aboveQ ", tagId);showColor(aboveQ);printf("\n");)
                             DEBUG_IF(printf("              belowQ "       );showColor(belowQ);printf("\n");)
                             pushColor(state, traverseCombine(tag, aboveQ, belowQ));
                             done = true;
                             break;
                     } // switch stage
                 } // fabTagIsBinaryOp
            } // else fabricTagId != NULLFABRICTAGID
        } // else hasRange
        DEBUG_IF(printf("LOOP\n");)
        //DEBUG_IF(showColorStack(state);)
        //DEBUG_IF(showFabricStack(dag, state);)
    } // while
    DEBUG_IF(printf("DONE\n");)
    return popColor(state);
}

// ------------------- Run Kernel ---------------------

inline Space_ getRandom(Dag *dag) {
    Space_ value = dag->dagRandomField[dag->dagRandomCursor];
    dag->dagRandomCursor = (dag->dagRandomCursor + as_int(value) + 1) & RANDOMFIELDMASK;
    return value;
}

inline uint colorToSolidPixel_Word32_BGRA (Color_ color) {
  return as_uint(convert_uchar4((Color_)(bCh(color),gCh(color),rCh(color), 1.0) * MAXCHANNELFLOAT));
}

inline void writePixelGlobal (        int2  pos
                             ,        int2  bitmapSize
                             ,      Color_  color
                             , GMEM   uint *out
                             ) {
    uint colorWord = colorToSolidPixel_Word32_BGRA(color);
    int outPos = mul24(pos.y, bitmapSize.x) + pos.x;
    out[outPos] = colorWord;
}

inline void initDag
    ( PMEM        Dag  *dag             //
    , GMEM      float  *primBezierHeap  //
    , GMEM      float  *primFacetHeap   //
    , GMEM      float  *primBoxHeap     //
    , GMEM    PrimTag_ *primTagHeap     //
    , GMEM  FabricTag_ *fabricTagHeap   //
    , GMEM       char  *fabricHeap      //
    , GMEM  TreeRoot_  *treeRootHeap    //
    , GMEM ConfineTag  *treeConfineHeap //
    , GMEM    DecoTag  *treeDecoHeap    //
    , GMEM   ShapeId_  *crossingHeap    //
    , GMEM     Space_  *pictHeap        //
    , GMEM      float  *randomHeap      //
    ,            long   thread          //
    ) {
    dag->dagPrimBezierHeap  = primBezierHeap ;
    dag->dagPrimFacetHeap   = primFacetHeap  ;
    dag->dagPrimBoxHeap     = primBoxHeap    ;
    dag->dagPrimTagHeap     = primTagHeap    ;
    dag->dagFabricTagHeap   = fabricTagHeap  ;
    dag->dagFabricHeap      = fabricHeap     ;
    dag->dagTreeRootHeap    = treeRootHeap   ;
    dag->dagTreeConfineHeap = treeConfineHeap;
    dag->dagTreeDecoHeap    = treeDecoHeap   ;
    dag->dagCrossingHeap    = crossingHeap   ;
    dag->dagPictHeap        = pictHeap       ;
    dag->dagRandomField      = randomHeap     ;
    dag->dagRandomCursor    = thread & RANDOMFIELDMASK;
}

__kernel void traverseDagKernel
    ( GMEM       float  *primBezierHeap  //
    , GMEM       float  *primFacetHeap   //
    , GMEM       float  *primBoxHeap     //
    , GMEM    PrimTag_  *primTagHeap     //
    , GMEM  FabricTag_  *fabricTagHeap   //
    , GMEM        char  *fabricHeap      //
    , GMEM   TreeRoot_  *treeRootHeap    //
    , GMEM  ConfineTag  *treeConfineHeap //
    , GMEM     DecoTag  *treeDecoHeap    //
    , GMEM    ShapeId_  *crossingPile    //
    , GMEM      Space_  *pictHeap        //
    , GMEM       float  *randomHeap      //
    ,              int   dagRoot         //
    ,            Tile_   tile            //
    ,              int   columnDepth     //
    ,             int2   bitmapSize      //
    ,              int   frameCount      //
    , GMEM       uint   *target          //
    ) {
    int2  pos = (int2)( get_global_id(0) + boxLeft(tile)
                      , get_global_id(1) + boxTop(tile)
                      );
    //DEBUG_IF(printf("---pos %v2i get_global_id(0) %i get_global_id(1) %i get_local_id(0) %i get_local_id(1) %i\n", pos, get_global_id(0) ,get_global_id(1), get_local_id(0), get_local_id(1));)
    float2 ray = convert_float2(pos);
    long thread = (bitmapSize.x * (bitmapSize.y * frameCount + pos.x)) + pos.x;
    Dag dag;
    TraverseState state;
    if (pos.x < bitmapSize.x && pos.y < bitmapSize.y) {
        initDag( &dag
               ,  primBezierHeap     //
               ,  primFacetHeap      //
               ,  primBoxHeap        //
               ,  primTagHeap        //
               ,  fabricTagHeap      //
               ,  fabricHeap         //
               ,  treeRootHeap       //
               ,  treeConfineHeap    //
               ,  treeDecoHeap       //
               ,  crossingPile       //
               ,  pictHeap           //
               ,  randomHeap         //
               ,  thread             //
               );
        // DEBUG_IF(printf("sizeOf (DecoTag) %i\n", sizeof(DecoTag));)
        // DEBUG_IF(showDecoTagHeap(7, treeDecoHeap);)
        // DEBUG_IF(printf("sizeOf (ConfineTag) %i\n", sizeof(ConfineTag));)
        // DEBUG_IF(showConfineTagHeap(7, treeConfineHeap);)
        // DEBUG_IF(printf("----------inTraverseDagKernel-----------%i %i\n", get_global_id(0),get_global_id(1));)
        Color_ color = traverseFabric(  CROSSSPLITLIMIT
                                     ,  TAXICABFLATNESS
                                     , &state
                                     , &dag
                                     ,  ray
                                     ,  dagRoot
                                     );
        DEBUG_IF(printf("pos %v2i color %v4f bitmapSize %v2i \n", pos, color, bitmapSize);)
        writePixelGlobal ( pos
                         , bitmapSize
                         , color
                         , target
                         );
    }
}

void showDecoTagHeap(int size, GMEM DecoTag  *treeDecoHeap) {
  for (int i = 0; i < size; i++) {
     showDecoTag(treeDecoHeap[i]);
  }
}

void showDecoTag(DecoTag tag) {
    printf ("  decoTagCut       %2i %f  \n" , (long) &tag.decoTagCut       - (long)&tag, tag.decoTagCut         );
    printf ("  decoTagCrossings %2i %v2i\n" , (long) &tag.decoTagCrossings - (long)&tag, tag.decoTagCrossings   );
    printf ("  decoTagLessCut   %2i "       , (long) &tag.decoTagLessCut   - (long)&tag); if (tag.decoTagLessCut == NULLDECOTAGID) {printf("null\n");} else {printf("%i  \n", tag.decoTagLessCut);}
    printf ("  decoTagMoreCut   %2i "       , (long) &tag.decoTagMoreCut   - (long)&tag); if (tag.decoTagMoreCut == NULLDECOTAGID) {printf("null\n");} else {printf("%i  \n", tag.decoTagMoreCut);}
}

void showConfineTagHeap(int size, GMEM ConfineTag  *treeConfineHeap) {
  for (int i = 0; i < size; i++) {
     showConfineTag(treeConfineHeap[i]);
  }
}

void showConfineTag(ConfineTag tag) {
    printf ("  confineTagPrimTagId %2i %i  \n" , (long) &tag.confineTagPrimTagId - (long)&tag, tag.confineTagPrimTagId );
    printf ("  confineTagCut       %2i %f  \n" , (long) &tag.confineTagCut       - (long)&tag, tag.confineTagCut       );
    printf ("  confineTagOverhang  %2i %f  \n" , (long) &tag.confineTagOverhang  - (long)&tag, tag.confineTagOverhang  );
    printf ("  confineTagLessCut   %2i "       , (long) &tag.confineTagLessCut   - (long)&tag);if (tag.confineTagLessCut == NULLCONFINETAGID) {printf("null\n");} else {printf("%i  \n", tag.confineTagLessCut);}
    printf ("  confineTagMoreCut   %2i "       , (long) &tag.confineTagMoreCut   - (long)&tag);if (tag.confineTagMoreCut == NULLCONFINETAGID) {printf("null\n");} else {printf("%i  \n", tag.confineTagMoreCut);}
}

/*
void showStackRange(StackRange_ s) {
    printf("rangeMaybe mRange %i, %i, %i\n", rangeMaybe(mRange), rangeMin(mRange), rangeMax(mRange)
}
*/

void showShapeStack(ShapeStack *stack) {
  printf("shapeStack\n");
  for (int i = stack->stSize - 1; i >= 0; i--){
      printf("%i== %i \n",i, stack->stVector[i]);
  }
}

void showFabricStack(Dag *dag, TraverseState *state) {
  FabricTag_ tag;
  printf("fabricStack %i \n", state->tsSize);
  for (int i = state->tsSize - 1; i >= 0; i--) {
      tag = loadFabricTag(dag, state->tsFabricTagIds[i]);
      printf("%i>>  %i ", i, state->tsFabricTagIds[i]);
      // showFabricTagType(tag);
      printf(" mRange %i, %i, %i stage %i ray %v2f \n"
            , rangeMaybe(state->tsRanges[i])
            , rangeMin(state->tsRanges[i])
            , rangeMax(state->tsRanges[i])
            , state->tsStages[i]
            , state->tsRays[i]
            );
  }
}

void showColor(Color_ color) {
  printf("%2.2f,%2.2f,%2.2f,%2.2f", color.s0, color.s1, color.s2, color.s3);
}

void showColorStack(TraverseState *state) {
  printf("colorStack %i\n", state->tsColorSize);
  for (int i = state->tsColorSize - 1; i >= 0; i--) {
    printf("    %i}}\n"); //  %v4f\n",i,state->tsColorStack[i]);
  }
}

void showStackInRange(StackRange_ r, ShapeStack *stack) {
    printf("stackRange ");
    if (rangeMaybe(r)) {
        printf("\n");
        for (Ref_ i = rangeMin(r); i < rangeMax(r); i++) {
            printf("    %2i -> %i \n", i, stack->stVector[i]);
        }
    }
    else {
       printf("None\n");
    }
}

void showBezier(Bezier_ bez) {
  printf("Bez %3.3f, %3.3f : %3.3f, %3.3f : %3.3f, %3.3f", bzStart(bez).x,   bzStart(bez).y
                                                         , bzControl(bez).x, bzControl(bez).y
                                                         , bzEnd(bez).x,     bzEnd(bez).y
                                                         );
}

void showAffine(Affine_ a) {
  printf("AffineO\n   %3.3f %3.3f %3.3f\n   %3.3f %3.3f %3.3f\n   %3.3f %3.3f %3.3f\n",a.s0,a.s1,a.s2,a.s3,a.s4,a.s5,a.s6,a.s7,a.s8);
}

void showFabricTagType(FabricTag_ tag) {
       if (fabTagIsConstant         (tag)) {printf("CONSTANT   ");}
  else if (fabTagIsTexture          (tag)) {printf("TEXTURE    ");}
  else if (fabTagIsLinear           (tag)) {printf("LINEAR     ");}
  else if (fabTagIsQuadrance        (tag)) {printf("QUADRANCE  ");}
  else if (fabTagIsTree             (tag)) {printf("TREE       ");}
  else if (fabTagIsTransformAffine  (tag)) {printf("AFFINE     ");}
  else if (fabTagIsTransformFacet   (tag)) {printf("FACET      ");}
  else if (fabTagIsTransformConvolve(tag)) {printf("CONVOLVE   ");}
  else if (fabTagIsSqrt             (tag)) {printf("SQRT       ");}
  else if (fabTagIsInvert           (tag)) {printf("INVERT     ");}
  else if (fabTagIsCos              (tag)) {printf("COS        ");}
  else if (fabTagIsSin              (tag)) {printf("SIN        ");}
  else if (fabTagIsClamp            (tag)) {printf("CLAMP      ");}
  else if (fabTagIsComposite        (tag)) {printf("COMPOSITE  ");}
  else if (fabTagIsMult             (tag)) {printf("MULT       ");}
  else if (fabTagIsAdd              (tag)) {printf("ADD        ");}
  else if (fabTagIsFloatOr          (tag)) {printf("FLOATOR    ");}
  else if (fabTagIsFloatXor         (tag)) {printf("FLOATXOR   ");}
  else if (fabTagIsMin              (tag)) {printf("MIN        ");}
  else if (fabTagIsMax              (tag)) {printf("MAX        ");}
  else if (fabTagIsHSVAdjust        (tag)) {printf("HSVADJUST  ");}
  else if (fabTagIsTranparent       (tag)) {printf("TRANSPARENT");}
}
