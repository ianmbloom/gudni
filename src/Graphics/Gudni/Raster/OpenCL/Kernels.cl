// 1 Space for haskell defined macros
// 2 So the line numbers are correct
// 3  -----------------------------------------------------------------------------
// 4  -- |
// 5  -- Module      :  Graphics.Gudni.Raster.OpenCL.Kernel.cl
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

#ifdef dEBUGoUTPUT
#define dEBUGiF(statement) if (get_global_id(0) == dEBUG0 && get_global_id(1) == dEBUG1) {statement} // on the fly debugging output
#ifdef cl_amd_printf
#pragma OPENCL EXTENSION cl_amd_printf : enable
#endif
#else
#define dEBUGiF(statement)
#endif

#ifdef dEBUGoUTPUT
#define dEBUGlT(statement) if (get_global_id(0) < dEBUG0 && get_global_id(1) < dEBUG1) {statement} // on the fly debugging output
#ifdef cl_amd_printf
#pragma OPENCL EXTENSION cl_amd_printf : enable
#endif
#else
#define dEBUGlT(statement)
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

#define StorageId_    Ref_
#define ConfineTagId_ Ref_
#define DecoTagId_    Ref_
#define FabricTagId_  Ref_
#define FabricLimits_ Ref2_
#define PrimTagId_    Ref_
#define TransformId_  Ref_
#define BezierId_     Ref_
#define FacetId_      Ref_
#define BoxId_        Ref_
#define Index_        Ref_

#define PrimTag_      ulong
#define FabricTag_    uint

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

// ---------------------------------- Constants ----------------------------------
#define MAXCHANNELFLOAT 255.0f // maximum value for converting from float color value
#define ZEROPOINT (Point2_)(0,0)

// ---------------------------------- Color ----------------------------------
#define Color_    float4
#define HsvColor_ Color_

// A slice is a section of array indexes.
#define Slice_ Ref2_

inline Ref_ sliceStart (Slice_ s) {return s.x;}
inline Ref_ sliceLength(Slice_ s) {return s.y;}

// ------------------- random number generator ---------------------
typedef struct{ uint2 x; uint2 c; } mwc64xvec2_state_t;

ulong MWC_AddMod64(ulong a, ulong b, ulong M);
ulong MWC_MulMod64(ulong a, ulong b, ulong M);
ulong MWC_PowMod64(ulong a, ulong e, ulong M);
uint2 MWC_SkipImpl_Mod64(uint2 curr, ulong A, ulong M, ulong distance);
uint2 MWC_SeedImpl_Mod64(ulong A, ulong M, uint vecSize, uint vecOffset, ulong streamBase, ulong streamGap);
void MWC64XVEC2_Step(mwc64xvec2_state_t *s);
void MWC64XVEC2_Skip(mwc64xvec2_state_t *s, ulong distance);
void MWC64XVEC2_SeedStreams(mwc64xvec2_state_t *s, ulong baseOffset, ulong perStreamOffset);
uint2 MWC64XVEC2_NextUint2(mwc64xvec2_state_t *s);

// ------------------- functions matching Graphics.Gudni.Raster.ConfineTree.Type -------------------

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


// Store the entire dag.
typedef struct Dag
    { GMEM       Space_  *dagPrimBezierHeap  ;
      GMEM       Space_  *dagPrimFacetHeap   ;
      GMEM       Space_  *dagPrimBoxHeap     ;
      GMEM     PrimTag_  *dagPrimTagHeap     ;
      GMEM   FabricTag_  *dagFabricTagHeap   ;
      GMEM         char  *dagFabricHeap      ;
      GMEM   ConfineTag  *dagTreeConfineHeap ;
      GMEM      DecoTag  *dagTreeDecoHeap    ;
      GMEM FabricTagId_  *dagCrossingHeap    ;
      GMEM       Space_  *dagPictHeap        ;
     mwc64xvec2_state_t   dagRandomSeed      ;
    } Dag;

typedef struct PictUse {
    int2 pictSize;      // size of the bitmap
     int pictMemOffset; // starting point of the pixel data in the memory buffer
} PictUse;

typedef struct FabricStack {
   FabricTagId_ fsStack[fABRICsTACKsIZE];
        Point2_ fsRayStack[fABRICsTACKsIZE];
           int  fsSize;
} FabricStack;

typedef struct AnswerStack {
         Color_ asStack[aNSWERsTACKsIZE];
            int asSize;
} AnswerStack;

typedef struct TreeStack {
     ConfineTagId_ trStack[cONFINEsTACKsIZE];
               int trSize;
} TreeStack;

typedef struct BzStack {
    PMEM Bezier_  bezStack[bEZIERsTACKsIZE];
    PMEM    Box_  boxStack[bEZIERsTACKsIZE];
             int  bezStackTop;
} BzStack;


typedef struct TraverseState {
      FabricStack  tSFabricStack;
      AnswerStack  tSAnswerStack;
        TreeStack  tSTreeStack  ;
          BzStack  tSBzStack    ;
} TraverseState;

// ------------------- boilerplate for various stack types -----------------

// ----- FabricStack
inline void resetFabricStack(FabricStack *stack) {
    stack->fsSize = 0;
}

inline void popFabricStack(FabricStack *stack, FabricTagId_ *codePointer, Point2_ *ray) {
    stack->fsSize -= 1;
    *codePointer = stack->fsStack[stack->fsSize];
    *ray = stack->fsRayStack[stack->fsSize];
}

inline void pushFabricStack(FabricStack *stack, FabricTagId_ tagId, Point2_ ray) {
    stack->fsStack[stack->fsSize] = tagId;
    stack->fsRayStack[stack->fsSize] = ray;
    stack->fsSize += 1;
}

inline bool emptyFabricStack(FabricStack *stack) {
    return stack->fsSize <= 0;
}

// ----- AnswerStack
inline void resetAnswerStack(AnswerStack *stack) {
    stack->asSize = 0;
}

inline Color_ popAnswerStack(AnswerStack *stack) {
    stack->asSize -= 1;
    return stack->asStack[stack->asSize];
}

inline void pushAnswerStack(AnswerStack *stack, Color_ answer) {
    stack->asStack[stack->asSize] = answer;
    stack->asSize += 1;
}

inline bool emptyAnswerStack(AnswerStack *stack) {
    return stack->asSize == 0;
}

// ----- TreeStack

inline void resetTreeStack(TreeStack *stack) {
    stack->trSize = 0;
}

inline ConfineTagId_ popTreeStack(TreeStack *stack) {
    stack->trSize -= 1;
    return stack->trStack[stack->trSize];
}

inline void pushTreeStack(TreeStack *stack, ConfineTagId_ confineId) {
    stack->trStack[stack->trSize] = confineId;
    stack->trSize += 1;
}

inline bool emptyTreeStack(TreeStack *stack) {
    return stack->trSize == 0;
}

// ----- Init all

inline void resetTraverseState(TraverseState *tS) {
  resetFabricStack(&(tS->tSFabricStack));
  resetAnswerStack(&(tS->tSAnswerStack));
}

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

inline Space2_ getRandom2(Dag *dag);

void showBezierHeap(int size, Dag *dag);
void showDecoTagHeap(int size, GMEM DecoTag  *treeDecoHeap);
void showDecoTag(DecoTag tag);
void showConfineTagHeap(int size, GMEM ConfineTag  *treeConfineHeap);
void showConfineTag(ConfineTag tag);
void showFabricStack(Dag *dag, TraverseState *state);
void showAnswer(Color_ color);
void showAnswerStack(TraverseState *state);
void showBezier(Bezier_ bez);
void showAffine(Affine_ a);

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

// ------------------- functions from Graphics.Gudni.Raster.Fabric.Tag -------------------

#define FabricNodeType_    FabricTag_
#define FabricNodeSubType_ FabricTag_
#define FabricSubType_     FabricTag_
#define FabricData_        FabricTag_

inline FabricNodeType_ fabTagNodeType(FabricTag_ tag) {return (tag & fABRICtYPEbITMASK)           ;}
inline bool matchNodeType (FabricNodeSubType_ ty, FabricTag_ tag) {return fabTagNodeType(tag) == ty;}
inline      FabricTag_ fabTagSubType (FabricTag_ tag) {return tag & fABRICtAGdATAbITMASK        ;}
inline      FabricTag_ fromFabData   (FabricTag_ tag) {return tag & fABRICtAGdATAbITMASK        ;}

inline DecoTagId_    fabTagDecoId      (FabricTag_ tag) {return fromFabData(tag);}
inline ConfineTagId_ fabTagConfineId   (FabricTag_ tag) {return fromFabData(tag);}
inline FabricTagId_  fabTagStackerId   (FabricTag_ tag) {return fromFabData(tag);}
inline TransformId_  fabTagTransformId (FabricTag_ tag) {return fromFabData(tag);}
inline Ref_          fabTagSubstanceRef(FabricTag_ tag) {return fromFabData(tag);}

inline bool fabTagIsReturn(FabricTag_ tag) {return matchNodeType(fABRICiSrETURN, tag);}
/*
inline bool matchSubType (FabricTag_ match,  FabricTag_ tag) {return fabTagSubType(fabricTag) == match;}
inline bool fabTagIsConstant   (FabricTag_ tag) {return matchNodeType(fABRICiScONSTANT   , tag);}
inline bool fabTagIsTexture    (FabricTag_ tag) {return matchNodeType(fABRICiStEXTURE    , tag);}
inline bool fabTagIsFunction   (FabricTag_ tag) {return matchNodeType(fABRICiSfUNCTION   , tag);}
inline bool fabTagIsBinary     (FabricTag_ tag) {return matchNodeType(fABRICiSbINARY     , tag);}
inline bool fabTagIsUnaryPost  (FabricTag_ tag) {return matchNodeType(fABRICiSuNARYpOST  , tag);}
inline bool fabTagIsDecoTree   (FabricTag_ tag) {return matchNodeType(fABRICiSdECOtREE   , tag);}
inline bool fabTagIsConfineTree(FabricTag_ tag) {return matchNodeType(fABRICiScONFINEtREE, tag);}
inline bool fabTagIsStacker    (FabricTag_ tag) {return matchNodeType(fABRICiSsTACKER    , tag);}
inline bool fabTagIsAffine     (FabricTag_ tag) {return matchNodeType(fABRICiSaFFINE     , tag);}
inline bool fabTagIsFacet      (FabricTag_ tag) {return matchNodeType(fABRICiSfACET      , tag);}
inline bool fabTagIsConvolve   (FabricTag_ tag) {return matchNodeType(fABRICiScONVOLVE   , tag);}

inline bool fabTagIsLinear    (FabricTag_ tag) {return matchSubType(fABRICiSlINEAR   , tag);}
inline bool fabTagIsQuadrance (FabricTag_ tag) {return matchSubType(fABRICiSqUADRANCE, tag);}

inline bool fabTagIsSqrt  (FabricTag_ tag) {return matchSubType(fABRICiSsQRT  , tag);}
inline bool fabTagIsInvert(FabricTag_ tag) {return matchSubType(fABRICiSiNVERT, tag);}
inline bool fabTagIsCos   (FabricTag_ tag) {return matchSubType(fABRICiScOS   , tag);}
inline bool fabTagIsSin   (FabricTag_ tag) {return matchSubType(fABRICiSsIN   , tag);}
inline bool fabTagIsClamp (FabricTag_ tag) {return matchSubType(fABRICiScLAMP , tag);}

inline bool fabTagIsComposite (FabricTag_ tag) {return matchSubType (fABRICiScOMPOSITE  , tag);}
inline bool fabTagIsMult      (FabricTag_ tag) {return matchSubType (fABRICiSmULT       , tag);}
inline bool fabTagIsAdd       (FabricTag_ tag) {return matchSubType (fABRICiSaDD        , tag);}
inline bool fabTagIsFloatOr   (FabricTag_ tag) {return matchSubType (fABRICiSfLOAToR    , tag);}
inline bool fabTagIsFloatXor  (FabricTag_ tag) {return matchSubType (fABRICiSfLOATxOR   , tag);}
inline bool fabTagIsMin       (FabricTag_ tag) {return matchSubType (fABRICiSmIN        , tag);}
inline bool fabTagIsMax       (FabricTag_ tag) {return matchSubType (fABRICiSmAX        , tag);}
inline bool fabTagIsHsvAdjust (FabricTag_ tag) {return matchSubType (fABRICiShSVaDJUST  , tag);}
inline bool fabTagIsTranparent(FabricTag_ tag) {return matchSubType (fABRICiStRANSPARENT, tag);}
*/

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
    if (stack->bezStackTop < bEZIERsTACKsIZE - 1) {
        stack->bezStack[stack->bezStackTop] = bez;
        stack->boxStack[stack->bezStackTop] = box;
        stack->bezStackTop += 1;
    }
    else {
        dEBUGiF(printf("error bezier Stack overrun.\n");)
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
      dEBUGiF(printf("error bezier Stack underrun.\n");)
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

inline bool crossesBezierAlong (    bool  debugFlag
                               ,  Space_  limit
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
                splitBezier(0.5, bez, &lessBez, &moreBez);
                Space_ size = max (maxAlong(axis, box) - minAlong(axis, box), maxAthwart(axis, box) - minAthwart(axis, box));
                bool   slopeLTEZero = bezierSlopeLTEZero(axis, bez);
                bool   offBaseline = baseline != maxAthwart(axis, box);
                bool   isK = isKnobAbsolute(axis, bez) || isKnobAbsolute(!axis, bez);
                bool   insideLimits = start > minAlong(axis, box) || end <= maxAlong(axis, box);
                // dEBUGiF(if (debugFlag) {printf("    go bez ");showBezier(bez);printf(" isK %i offBaseline %i insideLimits %i size %f limit %f ret %i\n", isK, offBaseline, insideLimits, size, limit, returnValue);})
                if  ( (size >= limit &&
                      (
                       // curve size remains greater than the limit
                       offBaseline && insideLimits
                       // and the start or end points are somewhere inside curve limits
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
        } // if outsideOfRange init
        else {
           //dEBUGiF(if(debugFlag) {printf("out   ret %i ", returnValue);showBezier(bez);printf("\n");})
        }
    }
    return returnValue;
}

// --------------------- function from Graphics.Gudni.Raster.ConfineTree.Primitive.Tag ---------------

#define PrimType_ PrimTag_

inline   StorageId_ fromPrimStorage(PrimTag_ tag) {return   (StorageId_) (tag >> pRIMtAGsTORAGEiDsHIFT ); }
inline FabricTagId_ fromPrimTagId  (PrimTag_ tag) {return (FabricTagId_) (tag &  pRIMtAGfABRICiDbITMASK); }
inline    PrimType_ primTagType    (PrimTag_ tag) {return                (tag &  pRIMtAGtYPEbITMASK    ); }

inline         bool primTagIsBezier(PrimTag_ tag) {return primTagType(tag) == pRIMtAGiSbEZIER   ; }
inline         bool primTagIsFacet (PrimTag_ tag) {return primTagType(tag) == pRIMtAGiSfACET    ; }
inline         bool primTagIsRect  (PrimTag_ tag) {return primTagType(tag) == pRIMtAGiSrECTANGLE; }
inline         bool primTagIsElipse(PrimTag_ tag) {return primTagType(tag) == pRIMtAGiSeLIPSE   ; }
inline    BezierId_ primTagBezierId(PrimTag_ tag) {return (BezierId_)fromPrimStorage(tag); }
inline     FacetId_ primTagFacetId (PrimTag_ tag) {return ( FacetId_)fromPrimStorage(tag); }
inline       BoxId_ primTagBoxId   (PrimTag_ tag) {return (   BoxId_)fromPrimStorage(tag); }
inline FabricTagId_ primTagFabricTagId (PrimTag_ tag) {return (FabricTagId_)fromPrimTagId(tag); }

// --------------------- function from Graphics.Gudni.Figure.Substance.Color -------------------

#define rCh(rgba) rgba.s0
#define gCh(rgba) rgba.s1
#define bCh(rgba) rgba.s2
#define aCh(rgba) rgba.s3

#define hCh(hsva) hsva.s0
#define sCh(hsva) hsva.s1
#define vCh(hsva) hsva.s2
// aCh is the same as for rgba

inline bool isOpaque(Color_ color) {return aCh(color) >= (1-oPAQUEtHRESHOLD);}
inline bool isClear (Color_ color) {return aCh(color) <= (  oPAQUEtHRESHOLD);}

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
    return cLEARbLACK;
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

// --------------------- function from Graphics.Gudni.Raster.ConfineTree.Primitive.Cross ---------------

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
    crossesBezierAlong(false, limit, bzStack, axis, start, baseline, end, bezierBezTri0(t) ) !=
    crossesBezierAlong(false, limit, bzStack, axis, start, baseline, end, bezierBezTri1(t) ) !=
    crossesBezierAlong(false, limit, bzStack, axis, start, baseline, end, bezierBezTri2(t) );
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
    // dEBUGiF(printf("bezierId %i\n", bezierId);)
    Bezier_ bez = *((Bezier_ *)(dag->dagPrimBezierHeap + (bezierId * bEZIERsIZEiNfLOATS)));
    bez.s67 = (Space2_)(0,0); // not needed but I hate having random data in registers.
    return bez;
}

inline Bezier_ loadBezierId(Dag *dag, int bezierId) {
    Bezier_ bez = *((Bezier_ *)(dag->dagPrimBezierHeap + (bezierId * bEZIERsIZEiNfLOATS)));
    bez.s67 = (Space2_)(0,0); // not needed but I hate having random data in registers.
    return bez;
}

inline Facet loadFacet(Dag *dag, PrimTag_ tag) {
    FacetId_ facetId = primTagFacetId(tag);
    return *((Facet *)(dag->dagPrimFacetHeap + (facetId * fACETsIZEiNfLOATS)));
}

inline Box_ loadBox(Dag *dag, PrimTag_ tag) {
    BoxId_ rectId = primTagBoxId(tag);
    return *((Box_ *)(dag->dagPrimBoxHeap + (rectId * bOXsIZEiNfLOATS)));
}

inline bool crossesPrimAlong(     bool  debugFlag
                            ,   Space_  limit
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
       ret = crossesBezierAlong(debugFlag, limit, bzStack, axis, start, baseline, end, bez);
       //dEBUGiF(if (debugFlag) {printf("primTagIsBezier ret %i axis %i start %f baseline %f end %f ",ret,axis,start,baseline,end);showBezier(bez);printf("\n");})
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

inline bool crossesPrim(     bool debugFlag
                       ,   Space_  limit
                       ,  BzStack *bzStack
                       ,      Dag *dag
                       , PrimTag_  primTag
                       ,  Point2_  start
                       ,  Point2_  end
                       ) {
    Point2_ iP = interimPoint(start, end);
    return  crossesPrimAlong (debugFlag, limit, bzStack, VERTICALAXIS  , dag, start.y, start.x,  iP.y, primTag) !=
            crossesPrimAlong (debugFlag, limit, bzStack, HORIZONTALAXIS, dag,    iP.x,    iP.y, end.x, primTag)    ;
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

// ------------------- functions matching Graphics.Gudni.Raster.Fabric.Storage -------------------

inline FabricTag_ loadFabricTag(Dag *dag, FabricTagId_ tagId) {
    return dag->dagFabricTagHeap[tagId];
}

inline Affine_ loadAffine(Dag *dag, FabricTag_ tag) {
    TransformId_ transformId = fabTagTransformId(tag);
    return *((Affine_ *)(dag->dagFabricHeap + transformId));
}

inline Space_ loadConvolve(Dag *dag, FabricTag_ tag) {
    TransformId_ transformId = fabTagTransformId(tag);
    return *((Space_ *)(dag->dagFabricHeap + transformId));
}

// A picture reference is a reference to bitmap data that can be the substance of a shape.

inline Color_ constantQuery(        Dag *dag
                           , FabricTag_  tag
                           ) {
    return *((Color_ *)(dag->dagFabricHeap + fabTagSubstanceRef(tag)));
}

inline PictUse loadPictUse(        Dag *dag
                          , FabricTag_  tag
                          ) {
    return *((PictUse*)(dag->dagFabricHeap + fabTagSubstanceRef(tag)));
}

// ------------------- Graphics.Gudni.Raster.Fabric.Substance.Query -------------------

inline Color_ outsideShape() {return cLEARbLACK ;}
inline Color_ insideShape () {return oPAQUEwHITE;}

inline Color_ getPixel(Dag *dag, PictUse use, Loc2_ p) {
     if (p.x >= 0 &&
         p.y >= 0 &&
         p.x < use.pictSize.x &&
         p.y < use.pictSize.y) {
         int w = use.pictSize.x;
         return *(dag->dagPictHeap + use.pictMemOffset) + mul24(p.y, w) + p.x;
     }
     else {
         return outsideShape();
     }
}

inline Color_ textureQuery(        Dag *dag
                          , FabricTag_  tag
                          ,    Point2_  ray
                          ) {
    return getPixel(dag, loadPictUse(dag, tag), (Loc2_)floor(ray));
}

inline Color_ linearQuery(Point2_ ray) {
    return (Color_) ray.y;
}

inline Color_ quadranceQuery(Point2_ ray) {
    return (Color_)quadrance(ray);
}


// ------------------- functions matching Graphics.Gudni.Raster.Fabric.Ray.Class -------------------

inline Color_ applyFilter(FabricTag_ tag, Color_ color) {
    Color_ ret;
    switch (fabTagSubType(tag)) {
        case fABRICiSsQRT  : ret = sqrt(color)                        ; break;
        case fABRICiSiNVERT: ret = 1-color                            ; break;
        case fABRICiScOS   : ret = cos(color)                         ; break;
        case fABRICiSsIN   : ret = sin(color)                         ; break;
        case fABRICiScLAMP : ret = clamp(color,cLEARbLACK,oPAQUEwHITE); break;
        default:             ret = color                              ; break;
    }
    return ret;

}

// ------------------- functions matching Graphics.Gudni.Raster.Fabric.Combine.Apply -------------------

inline Color_ applyCombine(FabricTag_ tag, Color_ a, Color_ b) {
    Color_ ret;
    switch (fabTagSubType(tag)) {
        case fABRICiScOMPOSITE  : ret = composite(a,b)        ; break;
        case fABRICiSmULT       : ret = a * b                 ; break;
        case fABRICiSaDD        : ret = a + b                 ; break;
        case fABRICiSfLOAToR    : ret = a + b - (a * b)       ; break;
        case fABRICiSfLOATxOR   : ret = a + b - (2 * (a * b)) ; break;
        case fABRICiSmIN        : ret = min(a,b)              ; break;
        case fABRICiSmAX        : ret = max(a,b)              ; break;
        case fABRICiShSVaDJUST  : ret = adjustHsva(a,b)       ; break;
        case fABRICiStRANSPARENT: ret = transparent(aCh(a),b) ; break;
    }
    return ret;
}

// ------------------- functions matching Graphics.Gudni.Raster.ConfineTree.Decorate -------------------

inline void insertTagId(FabricStack *stack, int i, FabricTagId_ tagId, Point2_ ray) {
    for (int j = min(fABRICsTACKsIZE-1,stack->fsSize); j > i; j--) {
        stack->fsStack[j] = stack->fsStack[j-1];
    }
    stack->fsStack[i] = tagId;
    stack->fsRayStack[stack->fsSize] = ray;
    stack->fsSize = min(fABRICsTACKsIZE,stack->fsSize+1);
}

inline void deleteTagId(FabricStack *stack, int i) {
    for (int j = i; j < stack->fsSize-1; j++) {
        stack->fsStack[j] = stack->fsStack[j+1];
    }
    stack->fsSize -= 1;
}

inline bool sameRay(Point2_ a, Point2_ b) {
    return (a.x == b.x) && (a.y == b.y); //  all(isequal(a,b));
}
inline void toggleItemActive(  FabricStack *stack
                             , FabricTagId_  newFabId
                             ,      Point2_  newRay
                             ) {
    bool done = false;
    int i = stack->fsSize;
    dEBUGiF(printf("toggleItemActive i %i newFabId %i newRay %v2f\n",i, newFabId, newRay);)
    while (i > 0 && !done) {
        FabricTagId_ oldFabId = stack->fsStack[i-1];
        Point2_      oldRay   = stack->fsRayStack[i-1];
        bool same = sameRay(newRay,oldRay);
        dEBUGiF(printf("toggleItemActive i %i oldFabId %i newRay %v2f \n",i, oldFabId, oldRay);)
        if (newFabId == oldFabId && same) {
            // it is the same exact tag so delete it from the stack
            deleteTagId(stack,i-1);
            done = true;
        }
        else if (newFabId > oldFabId || !same) {
            insertTagId(stack,i,newFabId, newRay);
            done = true;
        }
        i--;
    }
    if (!done) { // if we reach the bottom just insert on the end.
        dEBUGiF(printf("toggleItemActive !done i %i\n",i);)
        insertTagId(stack,i,newFabId, newRay);

    }

}

inline void combineFabricStacks (         Dag *dag
                                , FabricStack *stack
                                ,     DecoTag  tree
                                ,     Point2_  ray
                                ) {
    dEBUGiF(printf("combineFabricStacks start %i length %i \n", sliceStart(tree.decoTagCrossings), sliceLength(tree.decoTagCrossings));)
    for ( uint i = sliceStart(tree.decoTagCrossings)
        ; i < (sliceStart(tree.decoTagCrossings) + sliceLength(tree.decoTagCrossings))
        ; i ++
        ) {
       toggleItemActive(stack, dag->dagCrossingHeap[(int)i], ray);
    }
}

inline Point2_ traverseDecorateTree(         Dag *dag
                                   , FabricStack *fabricStack
                                   ,     Point2_  ray
                                   ,  DecoTagId_  decoTreeId
                                   ) {
    bool done = false;
    Point2_ anchor;
    Space_ parentCut  = (-MAXFLOAT);
    Space_ parentLine = (-MAXFLOAT);
    bool axis = false; // is vertical
    while (!done) {
        if (decoTreeId == nULLdECOtAGiD) {
            dEBUGiF(printf("        nullDecoTagId\n");)
            anchor = pointAlongAxis(axis, parentCut, parentLine);
            done = true;
        }
        else {
            DecoTag tree = dag->dagTreeDecoHeap[(int)decoTreeId];
            axis = tree.decoTagHorizontal == 1;
            Space_ cut = tree.decoTagCut;
            combineFabricStacks(dag, fabricStack, tree, ray);
            parentCut  = parentLine;
            parentLine = cut;
            if (athwart(axis, ray) < cut) {
                decoTreeId = tree.decoTagLessCut;
            }
            else {
                decoTreeId = tree.decoTagMoreCut;
            }
            dEBUGiF(printf("        decoTreeId %i\n", decoTreeId);)
        }
        axis = !axis;
    }
    return anchor;
}

// ------------------- functions matching Graphics.Gudni.Raster.ConfineTree.Query -------------------

inline void modifyItemStackIfCrossed(      Space_  limit
                                    ,         Dag *dag
                                    ,     BzStack *bzStack
                                    , FabricStack *stack
                                    ,     Point2_  start
                                    ,     Point2_  ray
                                    ,  PrimTagId_  primTagId
                                    ) {
    PrimTag_ primTag = dag->dagPrimTagHeap[(int)primTagId];
    bool debugFlag = primTagId == 0;
    bool crosses = crossesPrim(debugFlag, limit, bzStack, dag, primTag, start, ray);
    //dEBUGiF(printf("crossesPrim %i primFabricTagId %i result %i\n",primTagId, primTagFabricTagId(primTag), crosses);)
    if (crosses) {
        toggleItemActive (stack, primTagFabricTagId(primTag), ray);
    }
}

// ------------------- functions matching Graphics.Gudni.Raster.ConfineTree.Traverse -------------------

inline Box_ boxAroundPoints(Point2_ a, Point2_ b) {
    return (Box_) (min(a, b), max(a,b));
}

inline void traverseCTBox(        Space_  limit
                         ,           Dag *dag
                         ,       BzStack *bzStack
                         ,   FabricStack *fabricStack
                         ,     TreeStack *spine
                         ,       Point2_  anchor
                         ,       Point2_  ray
                         , ConfineTagId_  confineTreeId
                         ) {
    Box_ box = boxAroundPoints(anchor, ray);
    resetTreeStack(spine);
    pushTreeStack(spine, confineTreeId);
    while (!emptyTreeStack(spine)) {
        confineTreeId = popTreeStack(spine);
        //dEBUGiF(printf("confineTreeId %i \n", confineTreeId);)
        if (confineTreeId != nULLcONFINEtAGiD) {
            ConfineTag tree = dag->dagTreeConfineHeap[(int)confineTreeId];
            bool axis = tree.confineTagHorizontal == 1;
            if (athwart(axis, maxBox(box)) > tree.confineTagCut) {
                pushTreeStack(spine, tree.confineTagMoreCut);
            }
            if (athwart(axis, minBox(box)) <= tree.confineTagOverhang) {
                pushTreeStack(spine, tree.confineTagLessCut);
            }
            modifyItemStackIfCrossed( limit
                                    , dag
                                    , bzStack
                                    , fabricStack
                                    , anchor
                                    , ray
                                    , tree.confineTagPrimTagId
                                    );
        }
    }
}

// ------------------- functions matching Graphics.Gudni.Raster.ConfineTree.Query -------------------

inline void queryConfineTreePoint(        Space_  limit
                                 ,           Dag *dag
                                 , TraverseState *tS
                                 ,       BzStack *bzStack
                                 ,   FabricStack *fabricStack
                                 ,     TreeStack *spine
                                 ,    DecoTagId_  decoId
                                 , ConfineTagId_  confineId
                                 ,       Point2_  ray
                                 ) {
    dEBUGiF(printf("beforeDecorate\n");)
    dEBUGiF(showFabricStack(dag,tS);)
    Point2_ anchor = traverseDecorateTree(dag, fabricStack, ray, decoId);
    dEBUGiF(printf("afterDecorate\n");)
    dEBUGiF(showFabricStack(dag,tS);)
    traverseCTBox(limit, dag, bzStack, fabricStack, spine, anchor, ray, confineId);
    dEBUGiF(printf("afterTraverseCT\n");)
    dEBUGiF(showFabricStack(dag,tS);)
}

// ------------------- functions matching Graphics.Gudni.Raster.Fabric.Traverse -------------------

inline void pushAnswer( TraverseState *tS
                      , Color_ color
                      ) {
    pushAnswerStack(&(tS->tSAnswerStack),color);
}

inline Color_ popAnswer( TraverseState *tS
                       ) {
    if (emptyAnswerStack(&(tS->tSAnswerStack))) {
        return outsideShape();
    }
    else {
        return popAnswerStack(&(tS->tSAnswerStack));
    }
}

inline bool emptyFabric(TraverseState *tS) {
    return emptyFabricStack(&(tS->tSFabricStack));
}

inline void popFabric(TraverseState *tS, FabricTagId_ *codePointer, Point2_ *ray) {
    popFabricStack(&(tS->tSFabricStack), codePointer, ray);
}

inline void pushFabric(TraverseState *tS, FabricTagId_ codePointer, Point2_ ray) {
    pushFabricStack(&(tS->tSFabricStack), codePointer, ray);
}

inline void rayTraverseTree(       Space_  limit
                           ,          Dag  *dag
                           , TraverseState *tS
                           ,    DecoTagId_  decoId
                           , ConfineTagId_  confineId
                           ,       Point2_  ray
                           ) {
      queryConfineTreePoint(  limit
                           ,  dag
                           ,  tS
                           , &(tS->tSBzStack)
                           , &(tS->tSFabricStack)
                           , &(tS->tSTreeStack)
                           ,  decoId
                           ,  confineId
                           ,  ray
                           );
}

inline Color_ traverseFabric( Space_ limit
                            , Space_ flatness
                            , Dag *dag
                            , TraverseState *tS
                            , FabricTagId_ codePointer
                            , Point2_ ray
                            ) {
    resetTraverseState(tS);
    bool done = false;
    Color_ aboveAnswer, belowAnswer;
    FabricTag_ confineTag;
    while (!done) {
         FabricTag_ tag = loadFabricTag(dag, codePointer);
         dEBUGiF(printf("after load codePointer %i tag %x\n", codePointer, tag);)
         if (fabTagIsReturn(tag)) {
            if (!emptyFabric(tS)) {
                popFabric(tS, &codePointer, &ray);
                dEBUGiF(printf("after return codePointer %i tag %x\n", codePointer, tag);)
                dEBUGiF(showFabricStack(dag,tS);)
            }
            else {
               done = true;
            }
         }
         else {
             dEBUGiF(printf("beforeMarshall codePointer %i tag %x\n", codePointer, tag);)
             dEBUGiF(showFabricStack(dag,tS);)
             switch (fabTagNodeType(tag)) {
                 case fABRICiScONSTANT:
                      pushAnswer(tS, constantQuery(dag, tag));
                      break;
                 case fABRICiStEXTURE:
                      // pushAnswer(tS, textureQuery(dag, tag, ray));
                      break;
                 case fABRICiSfUNCTION:
                      // switch (fabTagSubType(tag)) {
                      //    case fABRICiSlINEAR:
                      //         pushAnswer(tS, linearQuery(ray));
                      //         break;
                      //    case fABRICiSqUADRANCE:
                      //         pushAnswer(tS, quadranceQuery(ray));
                      //         break;
                      // }
                      break;
                 case fABRICiSbINARY:
                      belowAnswer = popAnswer(tS);
                      aboveAnswer = popAnswer(tS);
                      pushAnswer(tS, applyCombine(tag, aboveAnswer, belowAnswer));
                      break;
                 case fABRICiSuNARYpOST:
                      // aboveAnswer = popAnswer(tS);
                      // pushAnswer(tS, applyFilter(tag, aboveAnswer));
                      break;
                 case fABRICiSdECOtREE:
                      codePointer -= 1;
                      confineTag = loadFabricTag(dag, codePointer);
                      rayTraverseTree(limit, dag, tS, fabTagDecoId(tag), fabTagConfineId(confineTag),ray);
                      break;
                 case fABRICiSsTACKER:
                      //pushFabric(tS, fabTagStackerId(tag), ray);
                      toggleItemActive(&(tS->tSFabricStack),fabTagStackerId(tag),ray);
                      break;
                 case fABRICiSaFFINE:
                      ray = applyAffine(loadAffine(dag, tag), ray);
                      break;
                 case fABRICiSfACET:
                      // ray = inverseFacet(limit, flatness, &(tS->tSBzStack), loadFacet(dag, tag),  ray);
                      break;
                 case fABRICiScONVOLVE:
                      // ray = ray + (getRandom2(dag) * loadConvolve(dag, tag));
                      break;
                 default:
                     dEBUGiF(printf("MARSHALL DEFAULT codePointer %i tag %x\n", codePointer, tag);)
                     break;
             }
             dEBUGiF(printf("afterMarshall codePointer %i tag %x\n", codePointer, tag);)
             dEBUGiF(showFabricStack(dag,tS);)
             codePointer -= 1;
             done = false;
         }
    }
    return popAnswer(tS);
}

// ------------------- Run Kernel ---------------------

inline Space2_ getRandom2(Dag *dag) {
    uint2 point = MWC64XVEC2_NextUint2(&dag->dagRandomSeed);
    Space2_ ret;
    ret.x = (Space_)((double)point.x / (double)0xFFFFFFFF);
    ret.y = (Space_)((double)point.y / (double)0xFFFFFFFF);
    dEBUGiF(printf("getRandom point %v2i %v2f \n", point, ret);)
    return ret;
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
    ( PMEM          Dag  *dag             //
    , GMEM        float  *primBezierHeap  //
    , GMEM        float  *primFacetHeap   //
    , GMEM        float  *primBoxHeap     //
    , GMEM      PrimTag_ *primTagHeap     //
    , GMEM    FabricTag_ *fabricTagHeap   //
    , GMEM         char  *fabricHeap      //
    , GMEM   ConfineTag  *treeConfineHeap //
    , GMEM      DecoTag  *treeDecoHeap    //
    , GMEM FabricTagId_  *crossingHeap    //
    , GMEM       Space_  *pictHeap        //
    ,             ulong   thread          //
    ) {
    dag->dagPrimBezierHeap  = primBezierHeap ;
    dag->dagPrimFacetHeap   = primFacetHeap  ;
    dag->dagPrimBoxHeap     = primBoxHeap    ;
    dag->dagPrimTagHeap     = primTagHeap    ;
    dag->dagFabricTagHeap   = fabricTagHeap  ;
    dag->dagFabricHeap      = fabricHeap     ;
    dag->dagTreeConfineHeap = treeConfineHeap;
    dag->dagTreeDecoHeap    = treeDecoHeap   ;
    dag->dagCrossingHeap    = crossingHeap   ;
    dag->dagPictHeap        = pictHeap       ;
    MWC64XVEC2_SeedStreams(&dag->dagRandomSeed, thread, 0);
}

__kernel void traverseDagKernel
    ( GMEM        float  *primBezierHeap   //
    , GMEM        float  *primFacetHeap    //
    , GMEM        float  *primBoxHeap      //
    , GMEM     PrimTag_  *primTagHeap      //
    , GMEM   FabricTag_  *fabricTagHeap    //
    , GMEM         char  *fabricHeap       //
    , GMEM   ConfineTag  *treeConfineHeap  //
    , GMEM      DecoTag  *treeDecoHeap     //
    , GMEM FabricTagId_  *crossingPile     //
    , GMEM       Space_  *pictHeap         //
    ,               int   startCodePointer //
    ,             Tile_   tile             //
    ,               int   columnDepth      //
    ,              int2   bitmapSize       //
    ,               int   samplesPerPixel  //
    ,               int   frameCount       //
    ,               int   cx               //
    ,               int   cy               //
    // , LMEM FabricTagId_  *fabricStackPool  //
    // , LMEM      Point2_  *rayStackPool     //
    //, LMEM      Color_   *samplePool     //
    , GMEM          uint *target           //
    ) {
    int2  pos = (int2)( get_global_id(0) + boxLeft(tile)
                      , get_global_id(1) + boxTop(tile)
                      );
    // int sample = get_global_id(2);
    //dEBUGiF(printf("---pos %v2i get_global_id(0) %i get_global_id(1) %i get_local_id(0) %i get_local_id(1) %i\n", pos, get_global_id(0) ,get_global_id(1), get_local_id(0), get_local_id(1));)
    Dag dag;
    TraverseState tS;
    Color_ accColor = cLEARbLACK;
    if (pos.x < bitmapSize.x && pos.y < bitmapSize.y) {
         // dEBUGiF(printf("sizeOf (DecoTag) %i\n", sizeof(DecoTag));)
         // dEBUGiF(showDecoTagHeap(7, treeDecoHeap);)
         // dEBUGiF(printf("sizeOf (ConfineTag) %i\n", sizeof(ConfineTag));)
         // dEBUGiF(showConfineTagHeap(7, treeConfineHeap);)
         // dEBUGiF(printf("----------inTraverseDagKernel-----------%i %i\n", get_global_id(0),get_global_id(1));)
         // dEBUGiF(showBezierHeap(60,&dag);)
         for (int sample = 0; sample < samplesPerPixel; sample++) {
             long thread = (samplesPerPixel * (bitmapSize.x * (bitmapSize.y * frameCount + pos.x)) + pos.x) + sample;
             initDag( &dag             //
                    ,  primBezierHeap  //
                    ,  primFacetHeap   //
                    ,  primBoxHeap     //
                    ,  primTagHeap     //
                    ,  fabricTagHeap   //
                    ,  fabricHeap      //
                    ,  treeConfineHeap //
                    ,  treeDecoHeap    //
                    ,  crossingPile    //
                    ,  pictHeap        //
                    ,  thread          //
                    );
             float2 ray = convert_float2(pos) + getRandom2(&dag);
             dEBUGiF(printf("sample %i ray %v2f \n", sample, ray);)
             accColor += traverseFabric(  cROSSsPLITlIMIT
                                       ,  tAXICABfLATNESS
                                       , &dag
                                       , &tS
                                       ,  startCodePointer
                                       ,  ray
                                       );
         } // for sample
         dEBUGiF(printf("pos %v2i accColor %v4f bitmapSize %v2i \n", pos, accColor, bitmapSize);)
         if (pos.x == dEBUG0 || pos.y == dEBUG1) {accColor = composite((Color_)(0.0,0.0,1.0,0.25),accColor);}
         // else {accColor=(Color_)(0.0,0.5,0.5,1.0);}
         writePixelGlobal ( pos
                          , bitmapSize
                          , accColor/(float)samplesPerPixel
                          , target
                          );
    }
    //barrier(CLK_LOCAL_MEM_FENCE);
}

/*
Random number code below is part of MWC64X by David Thomas, dt10@imperial.ac.uk
This is provided under BSD, full license is with the main package.
See http://www.doc.ic.ac.uk/~dt10/research
http://cas.ee.ic.ac.uk/people/dt10/research/rngs-gpu-mwc64x.html#source_code
*/
#ifndef dt10_mwc64x_skip_cl
#define dt10_mwc64x_skip_cl

// Pre: a<M, b<M
// Post: r=(a+b) mod M
ulong MWC_AddMod64(ulong a, ulong b, ulong M)
{
	ulong v=a+b;
	if( (v>=M) || (v<a) )
		v=v-M;
	return v;
}

// Pre: a<M,b<M
// Post: r=(a*b) mod M
// This could be done more efficently, but it is portable, and should
// be easy to understand. It can be replaced with any of the better
// modular multiplication algorithms (for example if you know you have
// double precision available or something).
ulong MWC_MulMod64(ulong a, ulong b, ulong M)
{
	ulong r=0;
	while(a!=0){
		if(a&1)
			r=MWC_AddMod64(r,b,M);
		b=MWC_AddMod64(b,b,M);
		a=a>>1;
	}
	return r;
}


// Pre: a<M, e>=0
// Post: r=(a^b) mod M
// This takes at most ~64^2 modular additions, so probably about 2^15 or so instructions on
// most architectures
ulong MWC_PowMod64(ulong a, ulong e, ulong M)
{
	ulong sqr=a, acc=1;
	while(e!=0){
		if(e&1)
			acc=MWC_MulMod64(acc,sqr,M);
		sqr=MWC_MulMod64(sqr,sqr,M);
		e=e>>1;
	}
	return acc;
}

uint2 MWC_SkipImpl_Mod64(uint2 curr, ulong A, ulong M, ulong distance)
{
	ulong m=MWC_PowMod64(A, distance, M);
	ulong x=curr.x*(ulong)A+curr.y;
	x=MWC_MulMod64(x, m, M);
	return (uint2)((uint)(x/A), (uint)(x%A));
}

uint2 MWC_SeedImpl_Mod64(ulong A, ulong M, uint vecSize, uint vecOffset, ulong streamBase, ulong streamGap)
{
	// This is an arbitrary constant for starting LCG jumping from. I didn't
	// want to start from 1, as then you end up with the two or three first values
	// being a bit poor in ones - once you've decided that, one constant is as
	// good as any another. There is no deep mathematical reason for it, I just
	// generated a random number.
	enum{ MWC_BASEID = 4077358422479273989UL };

	ulong dist=streamBase + (get_global_id(0)*vecSize+vecOffset)*streamGap;
	ulong m=MWC_PowMod64(A, dist, M);

	ulong x=MWC_MulMod64(MWC_BASEID, m, M);
	return (uint2)((uint)(x/A), (uint)(x%A));
}

#endif

#ifndef dt10_mwc64xvec2_rng_cl
#define dt10_mwc64xvec2_rng_cl

//! Represents the state of a particular generator


enum{ MWC64XVEC2_A = 4294883355U };
enum{ MWC64XVEC2_M = 18446383549859758079UL };

void MWC64XVEC2_Step(mwc64xvec2_state_t *s)
{
	uint2 X=s->x, C=s->c;

	uint2 Xn=MWC64XVEC2_A*X+C;
	// Note that vector comparisons return -1 for true, so we have to do this negation
	// I would hope that the compiler would do something sensible if possible...
	uint2 carry=as_uint2(-(Xn<C));
	uint2 Cn=mad_hi((uint2)MWC64XVEC2_A,X,carry);

	s->x=Xn;
	s->c=Cn;
}

void MWC64XVEC2_Skip(mwc64xvec2_state_t *s, ulong distance)
{
	uint2 tmp0=MWC_SkipImpl_Mod64((uint2)(s->x.s0,s->c.s0), MWC64XVEC2_A, MWC64XVEC2_M, distance);
	uint2 tmp1=MWC_SkipImpl_Mod64((uint2)(s->x.s1,s->c.s1), MWC64XVEC2_A, MWC64XVEC2_M, distance);
	s->x=(uint2)(tmp0.x, tmp1.x);
	s->c=(uint2)(tmp0.y, tmp1.y);
}

void MWC64XVEC2_SeedStreams(mwc64xvec2_state_t *s, ulong baseOffset, ulong perStreamOffset)
{
	uint2 tmp0=MWC_SeedImpl_Mod64(MWC64XVEC2_A, MWC64XVEC2_M, 2, 0, baseOffset, perStreamOffset);
	uint2 tmp1=MWC_SeedImpl_Mod64(MWC64XVEC2_A, MWC64XVEC2_M, 2, 1, baseOffset, perStreamOffset);
	s->x=(uint2)(tmp0.x, tmp1.x);
	s->c=(uint2)(tmp0.y, tmp1.y);
}

//! Return a 32-bit integer in the range [0..2^32)
uint2 MWC64XVEC2_NextUint2(mwc64xvec2_state_t *s)
{
	uint2 res=s->x ^ s->c;
	MWC64XVEC2_Step(s);
	return res;
}

#endif

void showBezierHeap(int size, Dag *dag) {
    printf("BezierHeap \n");
    for (int i = 0; i < size; i++) {
        Bezier_ bez = loadBezierId(dag, i);
        printf("    %i: ",i);showBezier(bez);printf("\n");
    }
}

void showDecoTagHeap(int size, GMEM DecoTag *treeDecoHeap) {
  for (int i = 0; i < size; i++) {
     showDecoTag(treeDecoHeap[i]);
  }
}

void showDecoTag(DecoTag tag) {
    printf ("  decoTagCut       %2i %f  \n" , (long) &tag.decoTagCut       - (long)&tag, tag.decoTagCut         );
    printf ("  decoTagCrossings %2i %v2i\n" , (long) &tag.decoTagCrossings - (long)&tag, tag.decoTagCrossings   );
    printf ("  decoTagLessCut   %2i "       , (long) &tag.decoTagLessCut   - (long)&tag); if (tag.decoTagLessCut == nULLdECOtAGiD) {printf("null\n");} else {printf("%i  \n", tag.decoTagLessCut);}
    printf ("  decoTagMoreCut   %2i "       , (long) &tag.decoTagMoreCut   - (long)&tag); if (tag.decoTagMoreCut == nULLdECOtAGiD) {printf("null\n");} else {printf("%i  \n", tag.decoTagMoreCut);}
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
    printf ("  confineTagLessCut   %2i "       , (long) &tag.confineTagLessCut   - (long)&tag);if (tag.confineTagLessCut == nULLcONFINEtAGiD) {printf("null\n");} else {printf("%i  \n", tag.confineTagLessCut);}
    printf ("  confineTagMoreCut   %2i "       , (long) &tag.confineTagMoreCut   - (long)&tag);if (tag.confineTagMoreCut == nULLcONFINEtAGiD) {printf("null\n");} else {printf("%i  \n", tag.confineTagMoreCut);}
}

void showFabricStack(Dag *dag, TraverseState *tS) {
    printf("fabricStack %i\n", tS->tSFabricStack.fsSize);
    for (int i = tS->tSFabricStack.fsSize - 1; i >= 0; i--) {
        FabricTagId_ tagId = tS->tSFabricStack.fsStack[i];
        printf("        i %i >> tagId %i tag %x ray %v2f \n", i, tagId, loadFabricTag(dag, tagId), tS->tSFabricStack.fsRayStack[i]);
    }
    showAnswerStack(tS);
}

void showAnswer(Color_ color) {
  printf("%2.2f,%2.2f,%2.2f,%2.2f", color.s0, color.s1, color.s2, color.s3);
}

void showAnswerStack(TraverseState *tS) {
  printf("answerStack %i\n", tS->tSAnswerStack.asSize);
  for (int i = tS->tSAnswerStack.asSize - 1; i >= 0; i--) {
    printf("    %i}}} %v4f\n",i,tS->tSAnswerStack.asStack[i]);
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
