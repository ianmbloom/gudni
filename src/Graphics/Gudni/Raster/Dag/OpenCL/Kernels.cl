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
// ---------------- Macros, Type definitions and type accessors -----------------------------------

#ifdef DEBUG_OUTPUT
#define DEBUG_IF(statement) if (get_global_id(1) == DEBUGCOLUMNTHREAD && get_global_id(0) == DEBUGINDEX) {statement} // on the fly debugging output
#else
#define DEBUG_IF(statement)
#endif

#ifdef cl_amd_printf
#pragma OPENCL EXTENSION cl_amd_printf : enable
#endif

// Memory Types
#define GMEM __global
#define LMEM __local
#define PMEM __private
#define CMEM __constant


#define ShapeId_       uint
#define ShapeId2_      uint2
#define StorageId_     uint
#define ConfineTagId_  uint
#define FabricTagId_   uint
#define PrimTag_       ulong
#define FabricTag_     ulong
#define SubstanceTag_  FabricTag_

#define Affine_ float9;
#define BezTri_ float12;
#define Tri_    float6;
#define Filter_ uint;

#define Space_     float
#define As_Space_  as_float
#define Space2_    float2
#define As_Space2_ as_float2
#define Space4_    float4
#define As_Space4_ as_float4
#define Space8_    float8
#define As_Space8_ as_float8

#define Point2_    Space2_
#define Bezier_    Space8_

// Constants
#define LARGE_PRIME 282917
#define PIXELHEIGHT 1.0f
#define COLUMNSPACING 1.0f
#define MAXCHANNELUCHAR 0xFF   // maximum value for converting from uchar color value
#define MAXCHANNELFLOAT 255.0f // maximum value for converting from float color value

// Color
#define Color_ float4
#define RED(color)   color.s0 // red   channel of float4 color
#define GREEN(color) color.s1 // green channel of float4 color
#define BLUE(color)  color.s2 // blue  channel of float4 color
#define ALPHA(color) color.s3 // alpha channel of float4 color
#define OPAQUE(color) ALPHA(color) == 1.0f // determine if the color is not opaque
#define TRANSPARENT_COLOR (Color_)(0.0f,0.0f,0.0f,0.0f) // constant clear pixel
#define TRANSPARENT_COLOR_ZERO_AREA ((float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0)))

//  ---------------------------------- Structure Types  ------------------------------------

// A slice is a section of array indexes.
typedef struct Slice
    { REF sStart;
      REF sLength;
    } Slice;

// Store the state of the entire dag.
typedef struct Dag
    {     float2 *dagPrimBezierHeap  ; //
    ,     float2 *dagPrimFacetHeap   ; //
    ,     float2 *dagPrimBoxHeap     ; //
    ,   PrimTag_ *dagPrimTagHeap     ; //
    , FabricTag_ *dagFabricTagHeap   ; //
    ,      char  *dagFabricHeap      ; //
    ,   Confine  *dagTreeConfineHeap ; //
    ,      Deco  *dagTreeDecoHeap    ; //
    ,  ShapeId_  *dagCrossingPile    ; //
    ,     uchar  *dagPictHeap        ; //
    ,     float  *dagRandomHeap      ; //
    } Dag;

// ------------------------------------------ Box ------------------------------------------

#define Box_ FLOAT4
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
inline Point2_ sizeBox  (Box_ box) {return (maxBox(box) - minBox(box))    ;}
inline Point2_ centerBox(Box_ box) {return (minBox(box) + maxBox(box)) / 2;}

// ------------------- functions matching Graphics.Gudni.Principle.Point -------------------

#define HORIZONTALAXIS True
#define VERTICALAXIS   False

inline Space_ along  (bool axis, Point2_ p) {return axis ? p.x : p.y;}
inline Space_ athwart(bool axis, Point2_ p) {return axis ? p.y : p.x;}

pointAlongAxis(bool axis, Space_ parentLine, Space_ parentCut) {
   if (axis) { return (Point2_)(parentCut,  parentLine); }
   else      { return (Point2_)(parentLine, parentCut ); }
}

// ------------------- functions matching Graphics.Gudni.Figure.Bezier.Type -------------------

inline Point2_ bzStart  (Bezier_ bez) {return bez.s0s1;}
inline Point2_ bzControl(Bezier_ bez) {return bez.s2s3;}
inline Point2_ bzEnd    (Bezier_ bez) {return bez.s4s5;}
inline Point2_ taxiDistance(Point2_ v0, Point2_ v1) {return abs(v1.x - v0.x) + abs(v1.y - v0.y);}
inline Point2_ mid(Point2_ a, Point2_ b) {return (a + b) / 2;}

bool inline shouldSubdivideBezier(Space_ tolerance, Bezier_ bez) {
    Point2_ midPoint = mid(bezStart(bez),bzEnd(bez));
    Space_ tDistance = taxiDistance(midPoint,bzControl(bez));
    return tDistance > tolerance;
}

// ------------------- functions matching Graphics.Gudni.Figure.Bezier.Cross -------------------

inline Space_ bezAlongMin(  bool axis, Bezier_ bez) { min( min( along(  axis, bzStart(bez)), along(  axis, bzControl(bez))), along(  axis, bzEnd    (bez))); }
inline Space_ bezAlongMax(  bool axis, Bezier_ bez) { max( max( along(  axis, bzStart(bez)), along(  axis, bzControl(bez))), along(  axis, bzEnd    (bez))); }
inline Space_ bezAthwartMin(bool axis, Bezier_ bez) { min( min( athwart(axis, bzStart(bez)), athwart(axis, bzControl(bez))), athwart(axis, bzEnd    (bez))); }
inline Space_ bezAthwartMax(bool axis, Bezier_ bez) { max( max( athwart(axis, bzStart(bez)), athwart(axis, bzControl(bez))), athwart(axis, bzEnd    (bez))); }

Bezier_ makeBez ( Point2_ start
                , Point2_ control
                , Point2_ end
                ) {
    return (Bezier_)(start, control, end);
}

inline void splitBezier( Space_ t
                       , Bezier_ bez
                       , Bezier_ *less
                       , Bezier_ *more
                       ) {
    Bezier_ m = insideBezier(t,bez);
    *less = makeBez(bzStart(bez), bzStart(m), bzControl(m));
    *more = makeBez(bzControl(m), bzEnd(m),   bzEnd(bez)  );
}

inline Bezier_ insideBezier(Space_ t, Bezier_ bez) {
  Space_ mid0    = lerp(t, bzControl(bez), bzStart(bez)  );
  Space_ mid1    = lerp(t, bzEnd(bez)    , bzControl(bez));
  Space_ onCurve = lerp t mid1    mid0
  return makeBez(mid0, onCurve, mid1);
}

bool isKnobThreshold( Space_ threshold
                    , bool axis
                    , Bezier bez
                    ) {
    Space_ a = abs (athwart(axis,bzStart  (bez)) - athwart(axis, bzControl(bez)));
    Space_ b = abs (athwart(axis,bzControl(bez)) - athwart(axis, bzEnd    (bez)));
    Space_ c = abs (athwart(axis,bzStart  (bez)) - athwart(axis, bzEnd    (bez)));
    return abs ((a + b) - c) > threshold;
}

#define BEZIERSTACKSIZE 2;

typedef struct BezierStack
   { PMEM Bezier_  bezStack[BEZIERSTACKSIZE];
              int  bezStackTop  ;
   } BezierStack;

inline void initBezierStack (PMEM BezierStack *stack) {
   stack->bezierStackTop = 0;
}

inline void pushBezier( PMEM BezierStack *stack
                      ,          Bezier_  bez
                      ) {
    if (stack->bezStackTop < BEZIERSTACKSIZE - 1) {
        stack->bezStack[stack->bezStackTop] = bez;
        stack->bezStackTop ++;
    }
    else {
        printf("error bezier Stack overrun.");
    }
}

inline Bezier_ popBezier( PMEM BezierStack *stack
                        ) {
    if (stack->bezStackTop > 0) {
        Bezier_ bez = stack->bezStack[stack->bezStackTop];
        stack->bezStackTop--;
        return bez;
    }
    else {
      printf("error bezier Stack underrun.");
    }
}

inline bool emptyBezierStack ( PMEM BezierStack *stack) {
    return stack->bezStackTop <= 0;
}

inline bool crossesBezierAlong (    bool  axis
                               ,  Space_  start
                               ,  Space_  baseline
                               ,  Space_  end
                               , Bezier_  bez
                               ) {
     BezierStack stack;
     initBezierStack(&stack);
     if (start == end) {
       return false;
     }
     else {
         if (start > end) {
             Space_ temp = start;
             start = end;
             end = temp;
         }
         bool returnVal = true;
         pushBezStack(&stack, bez);
         while (!emptyBezierStack(&stack)) {
             bez = popBezStack(stack, &stackSize);
             Space_ minAthwart = bezAthwartMin(axis, bez);
             Space_ maxAthwart = bezAthwartMax(axis, bez);
             Space_ minAlong   = bezAlongMin  (axis, bez);
             Space_ maxAlong   = bezAlongMax  (axis, bez);
             Bezier_ lessBez;
             Bezier_ moreBez;
             splitBezier(0.5, bez, &lessBez, &moreBez);
             if ( baseline >  maxAthwart ||
                  baseline <= minAthwart ||
                  start >  maxAlong      ||
                  end   <= minAlong
                ) {
                 // segment is totally outside the range of curve
                 returnValue = returnValue != true;
             }
             else { Space_ size = max (maxAlong - minAlong, maxAthwart - minAthwart);
                 bool  slopeLTEZero = bezierSlopeLTEZero axis bez;
                 bool  offBaseline = baseline /= maxAthwart;
                 bool  isK = isKnobAbsolute axis bez || isKnobAbsolute (!axis) bez;
                 if  ( size >= crossSplitLimit &&
                       (
                        // curve size remains greater than the limit
                        offBaseline &&
                        (start > minAlong || end <= maxAlong) // and the start or end points are somewhere inside curve limits
                       )
                       || isK
                       ) { // or the curve creates a knob, meaning there could be more than one cross point
                           // must split
                      pushBezStack(stack, stackSize, moreBez);
                      bez = lessBez;
                 }
                 else {
                     bool   barrierMin = slopeLTEZero || (not slopeLTEZero && (offBaseline && isVertical axis));
                     Space_ barrierPos = barrierMin ? minAlong : maxAlong;
                     bool   startLTE   = slopeLTEZero || (not slopeLTEZero && (offBaseline || isHorizontal axis));
                     if (startLTE) {
                         returnValue = returnValue != (start <= barrierPos && end >  barrierPos);
                     }
                     else {
                         returnValue = returnValue != (start <  barrierPos && end >= barrierPos);
                     }
                 }
             }
         }
         return returnVal;
     }
}

// --------------------- function from Graphics.Gudni.Raster.Dag.Primitive.Tag ---------------

#define PrimType_ PrimTag_

inline StorageId_ fromPrimStorage(PrimTag_ tag) {return (StorageId_) tag >> PRIMTAGSTORAGEIDSHIFT ; }
inline ShapeId_   fromPrimShapeId(PrimTag_ tag) {return (ShapeId_)   tag &  PRIMTAGFABRICIDBITMASK; }
inline PrimType_  primTagType    (PrimTag_ tag) {return              tag &  PRIMTAGTYPEBITMASK    ; }

inline      bool primTagIsBezier(PrimTag_ tag) {return primTagType(tag) == PRIMTAGISBEZIER   ; }
inline      bool primTagIsFacet (PrimTag_ tag) {return primTagType(tag) == PRIMTAGISFACET    ; }
inline      bool primTagIsRect  (PrimTag_ tag) {return primTagType(tag) == pRIMtAGiSrECTANGLE; }
inline      bool primTagIsElipse(PrimTag_ tag) {return primTagType(tag) == pRIMtAGiSeLIPSE   ; }
inline BezierId_ primTagBezierId(PrimTag_ tag) {return fromPrimStorage(tag); }
inline FacetId_  primTagFacetId (PrimTag_ tag) {return fromPrimStorage(tag); }
inline BoxId_    primTagBoxId   (PrimTag_ tag) {return fromPrimStorage(tag); }
inline ShapeId_  primTagShapeId (PrimTag_ tag) {return fromPrimShapeId(tag); }

// --------------------- function from Graphics.Gudni.Raster.Dag.Primitive.Cross ---------------

inline bool rectIncludesPoint(Box_ boundary, Point2_ p) {
    return
    p.x >  maxBox(boundary).x ||
    p.y >  maxBox(boundary).y ||
    p.x <= minBox(boundary).x ||
    p.y <= minBox(boundary).y ;
}

inline Point2_ quadrance(Point2_ p) {return (p.x^2) + (p.y^2);}

inline bool ellipseIncludesPoint(Box_ boundary, Point2_ p) {
    Point2_ center = centerBox(boundary);
    Point2_ size   = sizeBox  (boundary);
    if (size.x > 0) && (size.y > 0)) {
        Point2_ adjustedDist = (p - center) / (size / 2);
        return rectIncludesPoint(boundary, p) && (quadrance(adjustedDist) >= 4);
    }
    else {
        return false;
    }
}

inline bool crossesRectAlong( bool axis
                            , Space_ start
                            , Space_ baseline
                            , Space_ end
                            , Box_ rect ) P
  Point2_ s = pointAlongAxis(axis, start, baseline);
  Point2_ e = pointAlongAxis(axis, end  , baseline);
  return rectIncludesPoint rect s /=
         rectIncludesPoint rect e ;
}

inline bool crossesEllipseAlong(   bool axis
                               , Space_ start
                               , Space_ baseline
                               , Space_ end
                               ,   Box_ rect
                               ) {
  Point2_ s = pointAlongAxis(axis, start, baseline);
  Point2_ e = pointAlongAxis(axis, end  , baseline);
  return ellipseIncludesPoint rect s /=
         ellipseIncludesPoint rect e ;
}

inline bool crossesPrimAlong(      bool  axis
                            ,    Space_  start
                            ,    Space_  baseline
                            ,    Space_  end
                            ,       Dag *dag
                            ,  PrimTag_  primTag
                            ) {
   switch (primTagType(tag)) {
       case  PRIMTAGISBEZIER:
             BezierId_ bezierId = primTagBezierId(tag);
             Bezier_ bez = dag->dagPrimBezierHeap[bezierId];
             return crossesBezierAlong(axis, start, baseline, end, bez);
             break;
       case  PRIMTAGISFACET:
             FacetId_ facetId = primTagFacetId(tag);
             Facet_ facet = dag->dagPrimFacetHeap[facetId];
             return crossesFacetAlong(axis, start, baseline, end, facet);
             break;
       case  PRIMTAGISRECTANGLE:
             BoxId_ rectId = primTagBoxId(tag);
             Box_ rect = dag->dagPrimBoxHeap[rectId];
             return crossesRectAlong(axis, start, baseline, end, rect);
             break;
       case  PRIMTAGISELIPSE:
             BoxId_ ellipseId = primTagBoxId(tag)
             Box_ ellipse = dag->dagPrimBoxHeap[ellipseId];
             return crossesEllipseAlong(axis, start, baseline, end, ellipse);
             break;
   }
}

inline Point2_ interimPoint(Point2_ start, Point2_ end) {(Point2_)(start.x, end.y);}

inline bool crossesPrim(      Dag *dag
                       , PrimTag_  primTag
                       ,  Point2_  start
                       ,  Point2_  end
                       ) {
    Point2_ iP = interimPoint(start, end);
    return  crossesPrimAlong (VERTICALAXIS  , start.y, start.x,  iP.y, primTag) /=
            crossesPrimAlong (HORIZONTALAXIS,    iP.x,    iP.y, end.x, primTag)    ;
}

// ------------------- functions from Graphics.Gudni.Figure.Facet.Traverse -------------------

#define t0(t) t.s0s1
#define t1(t) t.s2s3
#define t2(t) t.s4s5

inline Tri_ rotateTri1(Tri_ t) {return t.s2s3s4s5s0s1;}
inline Tri_ rotateTri2(Tri_ t) {return t.s4s5s0s1s2s3;}

inline Tri_ sideTri(Tri_ i) {return 0.5 * ((Tri_)(t0(i))/*,t0(i),t0(i))*/ + i);}

inline Tri_ sideTriIndex(int i, Tri_ tri) {
    switch(i) {
        case 0: return sideTri (           tri ); break;
        case 1: return sideTri (rotateTri2(tri)); break;
        case 2: return sideTri (rotateTri1(tri)); break;
    }
}

inline Tri_ centerTri(Tri_ i) {return 0.5 * (rotateTri1(i) + rotateTri2(i));}

#define p0(t) t.s0s1
#define c0(t) t.s2s3
#define p1(t) t.s4s5
#define c1(t) t.s6s7
#define p2(t) t.s8s9
#define c2(t) t.s10s11

inline BezTri_ flipSidesBezTri(BezTri_ i) {
    (BezTri_)( p0(i), c1(i)
             , p1(i), c2(i)
             , p2(i), c0(i)
             );
}

Space4_ innerSide (BezTri_ t) {
    return (Space4_)(p1(t),c1(t));
}

inline BezTri_ centerBezTri(BezTri_ t) {
    return flipSidesBezTri(
        (BezTri_)( innerSide(sideTriangle(           t ))
                 , innerSide(sideTriangle(rotateTri1(t)))
                 , innerSide(sideTriangle(rotateTri2(t)))
                 )
    );
}

inline BezTri_ sideBezTri(int i, BezTri_ t) {
    switch (i) {
        case 0: return sideTriangle(           t ); break;
        case 1: return sideTriangle(rotateTri1(t)); break;
        case 2: return sideTriangle(rotateTri2(t)); break;
    }
}


inline Bezier_ bezierBezTri0 (BezTri_ t) { return (Bezier_)(p0(t),c0(t),p1(t)); }
inline Bezier_ bezierBezTri1 (BezTri_ t) { return (Bezier_)(p1(t),c1(t),p2(t)); }
inline Bezier_ bezierBezTri2 (BezTri_ t) { return (Bezier_)(p2(t),c2(t),p0(t)); }

inline bool insideBezierTri(Point2_ p, BezTri_ t) {
    return
    crossesBezierAlong(VERTICALAXIS, MAXFLOAT, p.x, p.y, bezierBezTri0(t)) !=
    crossesBezierAlong(VERTICALAXIS, MAXFLOAT, p.x, p.y, bezierBezTri1(t)) !=
    crossesBezierAlong(VERTICALAXIS, MAXFLOAT, p.x, p.y, bezierBezTri2(t));
}

inline bool shouldSubdivideFacet(Space_ tolerance, Facet_ t) {
    return
    shouldSubdivideBezier(tolerance, bezierBezTri0(t.facetOutput)) ||
    shouldSubdivideBezier(tolerance, bezierBezTri1(t.facetOutput)) ||
    shouldSubdivideBezier(tolerance, bezierBezTri2(t.facetOutput));
}

inline Facet_ traverseFacet(Facet_ facet, Point2_ point) {
    bool found = false;
    int i = 0;
    while (!found && i<3) {
        BezTri_ sideOut = sideBezTri(i,facet.facetOutput);
        if (insideBezierTri(point, sideOut)) {
            facet.facetOutput = sideOut;
            facet.facetInput  = sideTri(i,facet.facetInput);
            found = true;
        }
    }
    if (!found) {
        facet.facetOutput = centerBezTri(facet.facetOutput);
        facet.facetInput  = centerTri(facet.facetInput);
    }
    return facet;
}

inline Facet_ traverseFacetUntil(Space_ threshold, Point2_ point, Facet_ facet) {
    bool done = false;
    while (!done) {
        facet = traverseFacet(point, facet);
        done = !shouldSubdivideFacet(threshold, facet);
    }
    return facet;
}

// ------------------- functions from Graphics.Gudni.Figure.Facet.Barycentric -------------------

inline Point2_ inverseFacet(Facet_ facet, Point2_ ray) {
    return inverseTriangle(traverseFacet(facet, ray), ray);
}

inline Point2_ inverseTriangle(Facet_ f, Point2_ p) {
    Point2_ o  = p0(f.facetOutput)     ;
    Point2_ ou = p1(f.facetOutput) - o ;
    Point2_ ov = p2(f.facetOutput) - o ;
    Point2_ pT = p - o                 ;
    Point2_ pu = project pT ou         ;
    Point2_ pv = project pT ov         ;
    Point2_ to = t0(f.facetInput)      ;
    Point2_ tu = t1(f.facetInput) - to ;
    Point2_ tv = t2(f.facetInput) - to ;
    return  to + (pu * tu + pv * tv)   ;
}

// ------------------- functions from Graphics.Gudni.Figure.Principle.Affine -------------------

// Other orientation
// inline Space3_ matrixProduct9x3(Space9_ a, Space3_ b) {
//    return (Space3_) ( b.s0 * (a.s0 + a.s1 + a.s2)
//                     , b.s1 * (a.s3 + a.s4 + a.s5)
//                     , b.s2 * (a.s6 + a.s7 + a.s8)
//                     );
//   }
//
// inline Space3_ matrixProduct9x3Part(Space9_ a, Space3_ b) {
//    return (Space2_) ( b.s0 * (a.s0 + a.s1 + a.s2)
//                     , b.s1 * (a.s3 + a.s4 + a.s5)
//                     );
//   }

inline Space2_ matrixProduct9x3Part(Space9_ a, Space3_ b) {
   return (Space2_) ( b.s0 * (a.s0 + a.s3 + a.s6)
                    , b.s1 * (a.s1 + a.s4 + a.s7)
                    );
}

Point2_ applyAffine(Affine_ a, Point2_ p) {
  Space3_ row = (Space3_) (x, y, 1);
  return matrixProduct9x3Part(a, row);
}

// ------------------- functions from Graphics.Gudni.Raster.Dag.Fabric.Tag -------------------

#define FabricNodeType_    FabricTag_
#define FabricNodeSubType_ FabricTag_
#define FabricSubType_     FabricTag_
#define FabricData_        FabricTag_
#define FabricHigh_        StorageId_
#define FabricLow_         StorageId_

inline FabricHigh_ fromFabHigh(FabricTag_ i) {return (FabricHigh_) ((i & FABRICTAGHIGHIDBITMASK) >> FABRICTAGHIGHIDSHIFT);}
inline FabricLow_  fromFabLow (FabricTag_ i) {return (FabricLow_ ) (i & FABRICTAGLOWIDBITMASK)                           ;}
inline FabricData_ fromFabData       (FabricTag_ tag) {return tag & FABRICTAGDATABITMASK   ;}
inline FabricData_ fromSubstanceData (FabricTag_ tag) {return tag & FABRICTAGDATABITMASK   ;}
inline FabricNodeType_ fabTagNodeType(FabricTag_ tag) {return tag & FABRICNODETYPEBITMASK;}

inline bool matchNodeType(FabricNodeType_ ty, FabricTag_ fabricTag) {return fabTagNodeType(fabricTag) == ty;}

inline bool fabTagIsLeaf     (FabricTag_ tag) {return matchNodeType fABRICiSlEAF     ;}
inline bool fabTagIsUnaryPre (FabricTag_ tag) {return matchNodeType fABRICiSuNARYpRE ;}
inline bool fabTagIsUnaryPost(FabricTag_ tag) {return matchNodeType fABRICiSuNARYpOST;}
inline bool fabTagIsBinaryOp (FabricTag_ tag) {return matchNodeType fABRICiSbINARY   ;}

inline FabricNodeSubType_ fabTagNodeSubType(FabricTag_ fabricTag) {return fabricTag & FABRICNODESUBTYPEBITMASK;}

inline bool matchSubType(FabricSubType_ match, FabricTag_ fabricTag) {return fabTagNodeSubType(fabricTag) == match;}

inline FabricData_ substanceTagRef(FabricTag_ tag) {return tag & FABRICTAGDATABITMASK;}

inline bool fabTagIsConstant (FabricTag_ tag) {return matchSubType(fABRICiScONSTANT , tag);}
inline bool fabTagIsTexture  (FabricTag_ tag) {return matchSubType(fABRICiStEXTURE  , tag);}
inline bool fabTagIsLinear   (FabricTag_ tag) {return matchSubType(fABRICiSlINEAR   , tag);}
inline bool fabTagIsQuadrance(FabricTag_ tag) {return matchSubType(fABRICiSqUADRANCE, tag);}

inline bool fabTagIsTree             (FabricTag_ tag) {return matchSubType(fABRICiStREE             , tag);}
inline bool fabTagIsTransformAffine  (FabricTag_ tag) {return matchSubType(fABRICiStRANSFORMaFFINE  , tag);}
inline bool fabTagIsTransformFacet   (FabricTag_ tag) {return matchSubType(fABRICiStRANSFORMfACET   , tag);}
inline bool fabTagIsTransformConvolve(FabricTag_ tag) {return matchSubType(fABRICiStRANSFORMcONVOLVE, tag);}

inline bool fabTagIsSqrt  (FabricTag_ tag) {return matchSubType(fABRICiSsQRT  , tag);}
inline bool fabTagIsInvert(FabricTag_ tag) {return matchSubType(fABRICiSiNVERT, tag);}
inline bool fabTagIsCos   (FabricTag_ tag) {return matchSubType(fABRICiScOS   , tag);}
inline bool fabTagIsSin   (FabricTag_ tag) {return matchSubType(fABRICiSsIN   , tag);}

inline bool fabTagIsComposite(FabricTag_ tag) {return matchSubType(fABRICiScOMPOSITE, tag);}
inline bool fabTagIsMult     (FabricTag_ tag) {return matchSubType(fABRICiSmULT     , tag);}
inline bool fabTagIsAdd      (FabricTag_ tag) {return matchSubType(fABRICiSaDD      , tag);}
inline bool fabTagIsFloatOr  (FabricTag_ tag) {return matchSubType(fABRICiSfLOAToR  , tag);}
inline bool fabTagIsFloatXor (FabricTag_ tag) {return matchSubType(fABRICiSfLOATxOR , tag);}
inline bool fabTagIsMin      (FabricTag_ tag) {return matchSubType(fABRICiSmIN      , tag);}
inline bool fabTagIsMax      (FabricTag_ tag) {return matchSubType(fABRICiSmAX      , tag);}
inline bool fabTagIsSaturate (FabricTag_ tag) {return matchSubType(fABRICiSsATURATE , tag);}

inline RootRef_     fabTagTreeId      (FabricTag_ tag) {return fromFabHigh(tag);}
inline TransformId_ fabTagTransformId (FabricTag_ tag) {return fromFabHigh(tag);}
inline FabricData_  fabTagSubstanceRef(FabricTag_ tag) {return fromFabData(tag);}
inline FabricTagId_ fabTagAboveId    (FabricTag_ tag) {return fromFabHigh(tag);}
inline FabricTagId_ fabTagChildId     (FabricTag_ tag) {return fromFabLow (tag);}

inline ShapeId_ fromHighLimit(FabricTag_ fabricTag) {return (ShapeId_) (fabricTag >> 32        );}
inline ShapeId_ fromLowLimit (FabricTag_ fabricTag) {return (ShapeId_) (fabricTag & NULLSHAPEID);}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.Fabric.Storage -------------------

inline Affine_ loadAffine(Dag *dag, SubstanceTag_ tag) {
    int transformId = fabTagTransformId(tag);
    return *((Affine_ *)(dag->dagFabricHeap + transformId));
}

inline Facet_ loadFacet(Dag *dag, SubstanceTag_ tag) {
    int transformId = fabTagTransformId(tag);
    return *((Facet_ *)(dag->dagFabricHeap + tranformId));
}

inline Filter_ loadFilter(Dag *dag, SubstanceTag_ tag) {
    int transformId = fabTagTransformId(tag);
    return *((Filter_ *)(dag->dagFabricHeap + transformId));
}

inline Space_ loadConvolve(Dag *dag, SubstanceTag_ tag) {
    int transformId = fabTagTransformId(tag);
    return *((Space_ *)(dag->dagFabricHeap + transformId));
}

// A picture reference is a reference to bitmap data that can be the substance of a shape.
typedef struct PictUse
  { int2 pictSize;      // size of the bitmap
     int pictMemOffset; // starting point of the pixel data in the memory buffer
  } PictUse;

inline void showSubstanceTag(Dag *dag, SubstanceTag_ tag) {
    showSubstanceCase(dag, tag);
    if (substanceTagIsConstance tag) {
        printf("color %2.2v4f\n", loadSubstanceColor(dag, tag));
    }
    if (substanceTagIsTexture  tag) {
        PictUse_ pRef = loadPictUse(dag, tag);
        printf(" size %v2i offset %i\n", pRef.pictSize, pRef.pictMemOffset);
    }
}

inline Color_ loadSubstanceColor(Dag *dag, SubstanceTag_ tag) {
    return *((Color_ *)(dag->dagFabricHeap + substanceTagDescription(tag)));
}

inline PictUse_ loadPictUse(Dag *dag, SubstanceTag_ tag) {
    return *((PictUse_ *)(dag->dagFabricHeap + substanceTagDescription(tag)));
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.Fabric.Ray.Class -------------------

inline Color_ applyFilter(Dag *dag, SubstanceTag_ tag, Color_ color) {
    switch(substanceTagCase) {
        default: return color;
    }
}


inline Point2_ applyTransform(Dag *dag, SubstanceTag_ tag, Point2_ ray) {
    switch(fabTagCase(tag)) {
          case FABRICISTRANSFORMAFFINE:
               Affine_ affine = loadAffine(dag, tag);
               return applyAffine(affine, ray);
               break;
          case FABRICISTRANSFORMFACET:
               Facet_ facet = loadFacet(dag, tag);
               return inverseFacet(facet,  ray );
               break;
          case FABRICISTRANSFORMCONVOLVE:
               Space_ scale = loadConvolve(dag, tag);
               Space_ x = getRandom(dag);
               Space_ y = getRandom(dag);
               return ray + ((Point2_)(x,y) * scale);
               break;
    }
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.Fabric.Combine.Query -------------------

inline Color_ traverseCombine(Color_ a, Color_ b) {
    switch (fabTagCase(fabricTag)) {
         case FABRICISCOMPOSITE: return composite(a,b)       ; break;
         case FABRICISMULT:      return a * b                ; break;
         case FABRICISADD:       return a + b                ; break;
         case FABRICISFLOATOR:   return a + b - (a * b)      ; break;
         case FABRICISFLOATXOR:  return a + b - (2 * (a * b)); break;
    }
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.Fabric.Traverse -------------------

typedef struct ShapeStack
    { ShapeId_ *stVector[SHAPESTACKSIZE];
           int stSize;
    } ShapeStack;

typedef struct StackRange
    { bool  srMaybe;
       int  srMinIndex;
       int  srMaxIndex;
    } StackRange;

#define Limits_ = ShapeId2_

inline Limits_ srLimits(StackRange *s) {
   return (Limits_)( s->srStack[s -> srMinIndex]
                   , s->srStack[s -> srMaxIndex]
                   );
}

inline void showStackRange (StackRange *s) {
    printf("stackRange ");
    if (s->srMaybe) {
        printf("\n");
        for (int i = s->srMinIndex; i < s->srMaxIndex; i++) {
            printf("    %2i -> %i \n", i, s->srStack[i]);
        }
    }
    else {
       printf("None\n");
    }
}

inline bool makeStackRange(ShapeStack *stack, StackRange *range) {
   range->srMaybe    = True; // if this is false then there are no shapes so parse every node.
   range->srMinIndex = 0;
   range->srMaxIndex = stack->stSize;
}

inline bool hasRange(ShapeRange *range) {
    return range->srMaybe && (range->srMaxIndex > range->srMinIndex);
}

#define limitMin(l) l.x
#define limitMax(l) l.y

#define indexMin(i) i.x
#define indexMax(i) i.y

inline int findSubt( ShapeStack *stack
                   , ShapeId_ cutPoint
                   , int i
                   ) {
    ShapeId_ x = stack->stVector[i-1];
    while ( i > 0 || x >= cutPoint ) {
        i = i - 1;
        x = stack->stVector[i];
    }
    return i;
}

inline Color_ emptyQuery () {return CLEARBLACK ;}
inline Color_ insideShape() {return OPAQUEWHITE;}

typedef struct TraverseState
    {  StackRange  tsRanges[FABRICSTACKSIZE]      ;
      FabricTagId  tsFabricTagIds[FABRICSTACKSIZE];
           Stage_  tsFabricStages[FABRICSTACKSIZE];
              int  tsSize                         ;
            Color  tsColorStack[COLORSTACKSIZE]   ;
              int  tsColorSize                    ;
           Point2  tsRayStack[RAYSTACKSIZE]       ;
              int  tsRaySize                      ;
    } TraverseState;

inline void initTraverseState(TraverseState *state) {
    state->tsSize = 0;
    state->tsColorSize = 0;
    state->tsRaySize = 0;
}

bool traverseStop( FabricTag_ fabricTag
                 ,     Color_ aboveQ
                 ) {
    switch (fabricTagCase(fabricTag)) {
        case FABRICISCOMPOSITE: return isOpaque(aboveQ); break;
        case FABRICISMULT:      return isClear (aboveQ); break;
        default:                return False           ; break;
     }
}

inline FabricTag_    loadFabricTag(Dag *dag, FabricTagId_ fabricTagId) { dag->dagFabricTagHeap[fabricTagId    ]; }
inline FabricLimits_ loadLimits   (Dag *dag, FabricTagId_ fabricTagId) { dag->dagFabricTagHeap[fabricTagId + 1]; }

inline void pushFabric( TraverseState *state
                      ,  FabricTagId_  fabricTagId
                      ,       Limits_  limits
                      ,        Stage_  stage
                      ) {
    state->tsFabricTagIds[state->tsSize] = fabricTagId;
    state->tsRanges      [state->tsSize] = limits;
    state->tsStages      [state->tsSize] = stage;
    state->tsSize += 1;
}

inline void popFabric( TraverseState *state
                     ,  FabricTagId_ *fabricTagId
                     ,       Limits_ *limits
                     ,        Stage_ *stage
                     ) {
    *fabricTagId = state->tsFabricTagIds[state->tsSize];
    *limits      = state->tsRanges      [state->tsSize];
    *stage       = state->tsStages      [state->tsSize];
    state->tsSize -= 1;
}

inline bool emptyFabric(TraverseState *state
                       ) {
    return state->tsSize > 0;
}

inline void pushColor( TraverseState *state
                     , Color_ color
                     ) {
    state->tsColorStack[state->tsColorSize];
    state->tsColorSize += 1;
}

inline Color_ popColor( TraverseState *state
                      ) {
    Color_ color = state->tsColorStack[state->tsColorSize];
    state->tsColorSize -= 1;
    return color;
}

#define FIRSTSTAGE  0
#define SECONDSTAGE 1
#define THIRDSTAGE  2

#define Indices_ Limits_

Color_ traverseFabric(           Dag *dag
                     ,       Point2_  ray
                     ,  FabricTagId_  fabricTagId
                     ) {
    TraverseState state;
    initTraverseState(&state);
    Indices_ indices = (Indices_) (0, 0);
    FabricStage_ fabricStage = FIRSTSTAGE;
    pushFabric(state, fabricTagId, indices, fabricStage);
    while (!emptyFabric(state)) {
        popFabric(state, &fabricTagId, &indices, &fabricStage);
        if (fabricTagId == NULLFABRICTAGID) {
            pushColor(state, hasRange(indices) ? emptyQuery() : insideShape());
        }
        else {
            FabricTag_ tag = loadFabricTag(dag, fabricTagId);
            if (fabTagIsLeaf(tag)) {
                if (fabTagIsSubstance(tag)) {
                    pushColor(&state, fromSubstance(dag, fabTagSubstanceTag(tag), ray));
                    break;
                }
            }
            else if (fabTagIsUnaryPre(tag)) {
                if (fabTagIsTree(tag)) {
                    shapeStack   = traverseTree(dag, &state, ray, treeId);
                    Range_ shapeStackRange = makeStackRange(shapeStack, shapeStackRange);
                    pushFabric(&state, fabTagChildId (tag), limits, (Range_)(0, NULLSHAPEID), FIRSTSTAGE);
                    break;
                }
                else {
                    ShapeId_ belowId = fabTagChildId (tag);
                    switch (fabricStage) {
                       case FIRSTSTAGE:
                           pushRay(&state, ray);
                           ray = applyTransform(dag, tag, ray);
                           pushFabric(&state, fabricTagId, noRange, SECONDSTAGE);
                           pushFabric(&state, fabTagChildId(tag), shapeRange,
                           break;
                       case SECONDSTAGE:
                           ray = popRay(&state);
                           break;
                     }
                }
            }
            else if (fabTagIsUnaryPost(tag)) {
                ShapeId_ belowId = fabTagChildId (tag);
                switch (fabricStage) {
                   case FIRSTSTAGE:
                       pushFabric(&state, fabricTagId, noRange, SECONDSTAGE);
                       pushFabric(&state, fabTagChildId(tag), shapeRange,
                       break;
                   case SECONDSTAGE:
                       Color_ color = popColor(&state);
                       pushColor(&state, applyFilter(dag, tag, color));
                }
            }
            else if (fabTagIsBinaryOp(tag)) {
                FabricLimits_ limits  = loadLimits(dag, fabricTagId);
                switch (fabricStage) {
                    case FIRSTSTAGE:
                        //pushRange (state, trimRangeMin(midCut, mStackRange), midCut, indicesMax);
                        int cutIndex = findSubt(state->tsShapeStack, limitMax(limits), indexMax(indices));
                        pushFabric(&state, fabricTagId              , (Indices_) (indexMin(indices), cutIndex         ), SECONDSTAGE);
                        pushFabric(&state, fabTagAboveId(tag), (Indices_) (cutIndex         , indexMax(indices)), FIRSTSTAGE );
                        break;
                    case SECONDSTAGE:
                        Color_ aboveQ = popColor(state);
                        if (traverseStop(tag, aboveQ)) { // attempt to short circuit the combination based on the first argument.
                            pushColor(state, aboveQ);
                        }
                        else  {
                            FabricTagId_ belowId = fabTagChildId (tag);
                            pushFabric(&state, belowId, indices, FIRSTSTAGE);
                            pushColor(&state, aboveQ);
                            pushFabric(&state, fabricTagId, limits, EMPTYRANGE, THIRDSTAGE);
                        }
                        break;
                    case THIRDSTAGE:
                        Color_ belowQ = popColor(&state);
                        Color_ aboveQ = popColor(&state);
                        traverseCombine(tag, aboveQ, belowQ);
                        break;
                }
            }
        }
    }
    return popColor(&state)
}

inline void traverseTree(          Dag *dag
                        ,     Traverse *state
                        ,      Point2_  ray
                        , ConfineTreeId treeId
                        ) {
        lift $ queryConfineTreePoint loadPrimS cTree dTree (rayToPoint ray)
    do  (confineTree, decoTree) <- loadTree( confineTreeId
        overTree ({-trP "confineTree"-} confineTree) ({-trP "decoTree"-} decoTree) ray

// ------------------- Graphics.Gudni.Raster.Dag.Fabric.Substance.Query -------------------

inline Color_ loadPixel_Word32_RGBA(uchar4 pixel) {
  return convert_float4(pixel)/ MAXCHANNELFLOAT;
}

inline PictUse loadPictUse(           Dag *dag
                          , SUBSTANCETAG_  tag
                          ) {
    return *((PictUse*)(cS->csDescriptions + substanceTagDescriptionRef(tag)));
}

inline Color_ getPixel(Dag *dag, MemRef *pRef, int2 p) {
     if (p.x >= 0 &&
         p.y >= 0 &&
         p.x < pRef.pictSize.x &&
         p.y < pRef.pictSize.y) {
         int w = pRef.pictSize.x;
         GMEM uchar4 *pixelPointer = ((GMEM uchar4 *)dag->pixelPile + pRef.pictMemOffset) + mul24(y, w) + x;
         return loadPixel_Word32_RGBA(*pixelPointer);
     }
     else {
         return emptyQuery();
     }
}

inline Color_ querySubstanceColor(Dag *dag, SubstanceTag_ substance, Point2_ ray) {
    switch (substance) {
        case SUBSTANCEISCONSTANT:
            return loadSubstanceColor(dag, substance);
            break;
        case SUBSTANCEISTEXTURE:
            PictUse_ pictUse = loadPictUse(dag, substance);
            int2 pixel = floor(ray);
            getPixel(dag, memRef, pixel);
            break;
        case SUBSTANCEISLINEAR:
            return (Color_) ray.y;
            break;
        case SUBSTANCEISQUADRANCE:
            Space_ v = quadrance(ray);
            return (Color_) v;
            break;
    };
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Tag -------------------

typedef struct ConfineTag
     {    PrimTagId_ confineTagPrimTagId ;
     ,        Space_ confineTagCut       ;
     ,        Space_ confineTagOverhang  ;
     , ConfineTagId_ confineTagLessCut   ;
     , ConfineTagId_ confineTagMoreCut   ;
     } ConfineTag;

typedef struct DecoTag
    {     Space_ decoTagCut       ;
    ,     Slice_ decoTagCrossings ;
    , DecoTagId_ decoTagLessCut   ;
    , DecoTagId_ decoTagMoreCut   ;
    } DecoTag;

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Decorate -------------------

inline void insertShapeId(ShapeStack *stack, int i, ITEMTAG newThresholdTag) {
    for (int j = min(MAXLAYERS-1,stack->stSize); j > i; j--) {
        stack->stVector[j] = stack->stVector[j-1];
    }
    stack->stVector[i] = newThresholdTag;
    stack->stSize = min(MAXLAYERS,stack->stSize+1);
}

inline void deleteShapeId(ShapeStack *stack, int i) {
    for (int j = i; j < stack->stSize-1; j++) {
        stack->stVector[j] = stack->stVector[j+1];
    }
    stack->stSize--;
}

inline void toggleShapeActive( ShapeStack *stack
                             , ShapeId_ newShapeId
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
    for ( int i = tree.decoCrossings.sStart
        ; i < (tree.decoCrossings.sStart + tree.decoCrossings.sLength)
        ; i ++
        ) {
        toggleShapeActive(stack, dag->dagCrossingHeap[i]);
    }
}

traverseDecorateTree(        Dag *dag
                    , ShapeStack *stack
                    ,    Point2_  point
                    , DecoTagId_  treeId
                    ) {
    bool done = false;
    Space_ parentCut  = (-MAXFLOAT);
    Space_ parentLine = (-MAXFLOAT)
    bool axis = VERTICALAXIS;
    Point2_ anchor;
    while (!done) {
        if (tagId == NULLDECOTAGID) {
            anchor = pointAlongAxis(!axis, parentCut, parentLine);
        }
        else {
            DecoTag tree = dag->dagTreeDeco
            Space_ cut = tree.decoCut
            combineShapeStacks(dag, stack, tree);
            parentCut  = parentLine;
            parentLine = cut;
            if (athwart(axis, point) < cut) {
                treeId = tree.decoLessCut;
            }
            else {
                treeId = tree.decoMoreCut;
            }
        }
        axis = !axis;
    }
    return anchor;
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Traverse -------------------

#define TREEITEM uint2

typedef struct TreeStack
    { ConfineTreeId trStack[CONFINETREESTACKSIZE];
                int trSize                       ;
    } TreeStack;

inline ConfineTreeId popTreeStack(TreeStack *stack) {
    ConfineTreeId t = stack->trStack[stack->trSize];
    stack->trSize -= 1;
    return t;
}

inline void pushTreeStack(TreeStack *stack, ConfineTreeId t) {
    stack->trStack[stack->trSize] = t;
    stack->trSize += 1;
}

inline ConfineTreeId emptyTreeStack(TreeStack *stack) {
    return tStack->trSize > 0;
}

inline Box_ boxAroundPoints(Box_ a, Box_ b) {
    (Box_) ( min(a.x, b.x)
           , min(a.y, b.y)
           , max(a.x, b.x)
           , max(a.y, b.y));
}

inline treeItemAxis(TREEITEM item) {return           (bool) item.x;}
inline treeItemId  (TREEITEM item) {return (ConfineTreeId_) item.y;}

void traverseCTBox(           Dag *dag
                  ,    ShapeStack *shapeStack
                  ,       Point2_  anchor
                  ,       Point2_  point
                  , ConfineTreeId  treeId
                  ) {
    TreeStack tStack;
    Box_ box = boxAroundPoints(anchor, point);
    bool axis = VERTICALAXIS;
    pushTreeStack(&tStack, axis, treeId);
    while (!done ) {
        TREEITEM item = popTreeStack(tStack);
        axis   = treeItemAxis(item);
        treeId = treeItemId  (item);
        go axis mTree =
          case mTree of
          if (treeId != NULLCONFINEID) {
              if (athwart(axis, maxBox(box)) > tree.confineCut) {
                  pushTreeStack(&tStack, axis, tree.confineMoreCut);
              }
              if (athwart(axis, minBox(box)) <= tree.confineOverhang) {
                  pushTreeStack(&tStack, axis, tree.confineLessCut);
              }
              modifyItemStackIfCrossed(dag, shapeStack, anchor, point, tree.confinePrimTagId);
    }
}

// ------------------- functions matching Graphics.Gudni.Raster.Dag.ConfineTree.Query -------------------

inline void modifyItemStackIfCrossed(        Dag *dag
                                    , ShapeStack *stack
                                    ,    Point2_  start
                                    ,    Point2_  end
                                    , PrimTagId_  primTagId
                                    ) {
    PrimTag_ primTag = dag->dagPrimTagHeap[primTagId];
    if (crossesPrim(dag, primTag, start, end) {
        toggleShapeActive (stack, primTagShapeId(primShapeId));
    }
}

inline void queryConfineTreePoint(           Dag  *dag
                                 ,    ShapeStack  *stack
                                 , ConfineTreeId_  confineTreeId
                                 ,    DecoTreeId_  decoTreeId
                                 ,        Point2_  point) {
    Point2_ anchor = traverseDecorateTree(dag, stack, point, conTreeId);
    traverseCTBox(dag, stack, anchor, point, decoTreeId);
}

// ------------------- Run Kernel ---------------------

void initRandomField( Dag *dag
                    , CMEM float *randomField
                    , int blockId
                    , int columnDepth
                    , int columnThread
                    ) {
    // find a random starting point in the field passed on the absolute start position of the columnThread.
    int uniqueStart       = (int) ((((long)blockId) << columnDepth + columnThread) * LARGE_PRIME) & RANDOMFIELDMASK;
    dag->randomFieldCursor = uniqueStart;
    dag->randomField       = randomField;
}

float getRandom(Dag *dag) {
    dag->randomFieldCursor = (dag->randomFieldCursor + 1) & RANDOMFIELDMASK;
    return dag->randomField[dag->randomFieldCursor];
}

void writePixelGlobal (        int2  columnDelta
                      ,        int2  bitmapSize
                      ,       COLOR  color
                      , GMEM   uint *out
                      ,         int  y
                      ) {
    uint colorWord = colorToSolidPixel_Word32_BGRA(color);
    //DEBUG_IF(printf("y %i color %2.2v4f colorWord %x columnDelta %v2i \n", y, color, colorWord, columnDelta);)
    int outPos = mul24(columnDelta.y + y, bitmapSize.x) + columnDelta.x;
    out[outPos] = colorWord;
}

inline void initDag
    (     float2 *primBezierHeap  //
    ,     float2 *primFacetHeap   //
    ,     float2 *primBoxHeap     //
    ,   PrimTag_ *primTagHeap     //
    , FabricTag_ *fabricTagHeap   //
    ,      char  *fabricHeap      //
    ,   Confine  *treeConfineHeap //
    ,      Deco  *treeDecoHeap    //
    ,  ShapeId_  *crossingHeap    //
    ,     uchar  *pictHeap        //
    ,     float  *randomHeap      //
    ,       Dag  *dag             //
    ,       int   thread          //
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
    dag->dagRandomHeap      = randomHeap     ;
    dag->dagRandomCursor    = initRandomCursor(randomHeap, thread);
}

__kernel void traverseDagKernel
    (     float2 *primBezierHeap  //
    ,     float2 *primFacetHeap   //
    ,     float2 *primBoxHeap     //
    ,   PrimTag_ *primTagHeap     //
    , FabricTag_ *fabricTagHeap   //
    ,      char  *fabricHeap      //
    ,   Confine  *treeConfineHeap //
    ,      Deco  *treeDecoHeap    //
    ,  ShapeId_  *crossingPile    //
    ,     uchar  *pictHeap        //
    ,     float  *randomHeap      //
    ,       int   dagRoot         //
    ,     Tile_   tile            //
    ,       int   columnDepth     //
    ,      int2   bitmapSize      //
    ,       int   frameCount      //
    , GMEM uint  *target          //
    ) {
    int   xPos = get_global_id(0);
    int   yPos = get_global_id(1);
    float2 ray = (float2)( (float)xPos+boxLeft(tile)
                         , (float)yPos+boxTop(tile)
                         );
    Dag dag;
    initDag(&dagprimBezierHeap  //
           , primFacetHeap      //
           , primBoxHeap        //
           , primTagHeap        //
           , fabricTagHeap      //
           , fabricHeap         //
           , treeConfineHeap    //
           , treeDecoHeap       //
           , crossingPile       //
           , pictHeap           //
           , randomHeap         //
           );
    Color_ color = traverseFabric( &dag
                                 ,  ray
                                 ,  dagRoot
                                 );
    writePixelGlobal ( columnDelta
                     , bitmapSize
                     , color
                     , out
                     , yInt
                     );
}
