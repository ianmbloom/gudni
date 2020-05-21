// 1 Space for haskell defined macros
// 2 So the line numbers are correct
// 3  -----------------------------------------------------------------------------
// 4  -- |
// 5  -- Module      :  Graphics.Gudni.OpenCL.Kernel.cl
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

#define MAXTHRESHOLDMASK (MAXTHRESHOLDS - 1)

#define MINCROP 0.2f

#ifdef DEBUG_OUTPUT
#define DEBUG_IF(statement) if (get_global_id(1) == DEBUGCOLUMNTHREAD && get_global_id(0) == DEBUGINDEX) {statement} // on the fly debugging output
#else
#define DEBUG_IF(statement)
#endif


#ifdef cl_amd_printf
#pragma OPENCL EXTENSION cl_amd_printf : enable
#endif

#define SPACE     float
#define AS_SPACE  as_float
#define SPACE2    float2
#define AS_SPACE2 as_float2
#define SPACE4    float4
#define AS_SPACE4 as_float4

// Constants
#define LARGE_PRIME 282917
#define PIXELHEIGHT 1.0f
#define COLUMNSPACING 1.0f
#define MAXCHANNELUCHAR 0xFF   // maximum value for converting from uchar color value
#define MAXCHANNELFLOAT 255.0f // maximum value for converting from float color value

// Color
#define COLOR float4
#define RED(color)   color.s0 // red   channel of float4 color
#define GREEN(color) color.s1 // green channel of float4 color
#define BLUE(color)  color.s2 // blue  channel of float4 color
#define ALPHA(color) color.s3 // alpha channel of float4 color
#define OPAQUE(color) ALPHA(color) == 1.0f // determine if the color is not opaque
#define TRANSPARENT_COLOR (COLOR)(0.0f,0.0f,0.0f,0.0f) // constant clear pixel
#define TRANSPARENT_COLOR_ZERO_AREA ((float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0)))

// An ITEMTAGIDREF refers to the index of an itemId in the local heap.
#define ITEMTAGIDREF uint
// An ITEMTAGID refers to the index of an itemtag in the heap.
#define ITEMTAGID  uint

// An Item tag links to either a shape or a texture facet.
// See Graphics.Gudni.Raster.Constants for the bit layouts of ITEMTAG type.
#define ITEMTAG ulong
// A FacetID refers to the index of a facet in the facet heap.
#define FACETID   uint
#define NOFACET  0xFFFFFFFF
// An outline id refers to the index of a geoRef in the geoRefHeap.
#define SHAPEID   uint

#define SUBSTANCETAGID uint

inline bool itemTagIsFacet(ITEMTAG tag)    {return (tag & ITEMTAG_ISFACET_BITMASK) == ITEMTAG_ISFACET;}
inline bool itemTagIsShape(ITEMTAG tag)    {return (tag & ITEMTAG_ISFACET_BITMASK) == ITEMTAG_ISSHAPE;}
inline SUBSTANCETAGID itemTagSubstanceTagId(ITEMTAG tag) {return (int) ((tag & ITEMTAG_SUBSTANCE_ID_BITMASK) >> ITEMTAG_SUBSTANCE_ID_SHIFT);}
inline FACETID itemTagFacetId(ITEMTAG tag) {return (uint) (tag & ITEMTAG_REFERENCE_BITMASK);}
inline SHAPEID itemTagShapeId(ITEMTAG tag) {return (uint) (tag & ITEMTAG_REFERENCE_BITMASK);} // this is the same but here for consistency.
inline bool itemTagIsAdd(ITEMTAG tag)      {return (tag & ITEMTAG_COMPOUND_BITMASK) == ITEMTAG_COMPOUND_ADD;     }
inline bool itemTagIsSubtract(ITEMTAG tag) {return (tag & ITEMTAG_COMPOUND_BITMASK) == ITEMTAG_COMPOUND_SUBTRACT;}

//  A substance tag determines the type of a substance and contains a pointer to the substance description.
#define SUBSTANCETAG ulong

inline bool substanceTagType(SUBSTANCETAG tag) {return (tag & SUBSTANCETAG_TYPE_BITMASK) >> 56;}
inline bool substanceTagIsSolidColor(SUBSTANCETAG tag) {return (tag & SUBSTANCETAG_TYPE_BITMASK) == SUBSTANCETAG_TYPE_SOLID_COLOR;}
inline bool substanceTagIsTexture(SUBSTANCETAG tag)    {return (tag & SUBSTANCETAG_TYPE_BITMASK) == SUBSTANCETAG_TYPE_TEXTURE;}
inline bool substanceTagIsRadial(SUBSTANCETAG tag)    {return (tag & SUBSTANCETAG_TYPE_BITMASK) == SUBSTANCETAG_TYPE_RADIAL;}
inline bool substanceTagIsLinear(SUBSTANCETAG tag)    {return (tag & SUBSTANCETAG_TYPE_BITMASK) == SUBSTANCETAG_TYPE_LINEAR;}

inline SUBSTANCETAG substanceTagDescriptionRef(SUBSTANCETAG tag)      {return (tag & SUBSTANCETAG_REF_BITMASK);}



// A shape bit is the number of each shape assigned as it is added to a tilethread, each shape number corresponds to a bit in the shape stack
// so the number of possible shape bits is limited to the size (in bits) of the shape stack.
#define LAYERID    uint

#define LOCALSUBSTANCE uint

// Memory Types

#define GMEM __global
#define LMEM __local
#define PMEM __private
#define CMEM __constant

// Threshold Typedef

// A threshold is a small line segment that represents a transition into or out of a shape.
// if the threshold slope points downward
// and the left side crosses the left pixel border
// the shape transition made at the top of the threshold will be persistent.
// conversely, if the threshold slope points upward
// and the left side crosses the left pixel border
// the shape transition made at the bottom of the threshold will be persistent.
// top persistence looks like this.
// y o-----|
//   |.\   | slope pointing downward (positive)
//   |..\  |
//   |...\ |
// bottom persistence looks like this:
//   |  /..|
//   | /...|
//   |/....| slope pointing upward (negative)
// y o-----|
#define ITEMTAG        ulong   // the threshold-itemTag includes identifying information
#define THRESHOLD     SPACE4 // the stored version of a threshold encoded in floats

// ITEMTAG base type 64 bit ulong
// Bits | 1 bit  | 1 bit       | 1 bit    | 1 bit    | 32 bit  | 28 bit
// Type | bool   | bool        | bool     |          | uint    | uint
// Desc | slope  | persistence | isFacet  | reserved | facetId | substanceId
//      | sign   |             |          |          |         |
//
// slope sign - determines the sign of the slope of the threshold.
// persitent - determines if the threshold intersects with the left side of the pixel.
// isFacet - determines if the threshold defines a shape border or a texture facet border
// facetId - determines which facet the threshold is a border for.
// substanceId - either determines which substance the theshold is a facet border for.
// layerId - determines which layer the threshold defines a shape border for.
#define PERSIST_AND_SLOPE_MASK (ITEMTAG_SLOPE_BITMASK|ITEMTAG_PERSIST_BITMASK) // & to get leftmost and second to leftmost bit
#define PERSIST_TOP            (ITEMTAG_SLOPE_POSITIVE|ITEMTAG_PERSISTANT) // positive slope and persist
#define PERSIST_BOTTOM         (ITEMTAG_SLOPE_NEGATIVE|ITEMTAG_PERSISTANT) // not positive slopw and persist

#define UNPERSISTMASK          (~ITEMTAG_PERSIST_BITMASK) // everything but the persist bit

// & has a lower precedence than !=
inline bool   itemTagPositiveSlope(ITEMTAG tag) {return (tag & ITEMTAG_SLOPE_POSITIVE) != 0;} // determine if the threshold has a positive slope
inline bool   itemTagPersistTop   (ITEMTAG tag) {return (tag & PERSIST_AND_SLOPE_MASK) == PERSIST_TOP;   } // determine if the top of the threshold affects the persistant state of the shapestack
inline bool   itemTagPersistBottom(ITEMTAG tag) {return (tag & PERSIST_AND_SLOPE_MASK) == PERSIST_BOTTOM;} // determine if the bottom of the threshold affects the persistant state of the shapestack
inline bool   itemTagPersistEither(ITEMTAG tag) {return (tag & ITEMTAG_PERSIST_BITMASK) == ITEMTAG_PERSISTANT;  } // determine if either top or bottom of the threshold affects the persistant state of the shapestack
inline ITEMTAG unPersist          (ITEMTAG tag) {return tag & UNPERSISTMASK;}

#define VERTICALSLOPE      FLT_MAX // float value used to indicate vertical threshold slope.

inline THRESHOLD makeThreshold(SPACE top, SPACE bottom, SPACE left, SPACE right) {
  return (THRESHOLD)(top, bottom, left, right);
}

inline SPACE  tTop     (THRESHOLD t) {return t.s0;}
inline SPACE  tBottom  (THRESHOLD t) {return t.s1;}
inline SPACE  tLeft    (THRESHOLD t) {return t.s2;}
inline SPACE  tRight   (THRESHOLD t) {return t.s3;}
inline SPACE2 tStart   (ITEMTAG tag, THRESHOLD t) { return (SPACE2)(tLeft(t),itemTagPositiveSlope(tag) ? tTop(t) : tBottom(t));}
inline SPACE2 tEnd     (ITEMTAG tag, THRESHOLD t) { return (SPACE2)(tRight(t),itemTagPositiveSlope(tag) ? tBottom(t) : tTop(t));}
inline SPACE  tTopX    (ITEMTAG tag, THRESHOLD t) { return itemTagPositiveSlope(tag) ? tLeft(t)  : tRight(t);}
inline SPACE  tBottomX (ITEMTAG tag, THRESHOLD t) { return itemTagPositiveSlope(tag) ? tRight(t) : tLeft(t);}
inline SPACE  tHeight  (THRESHOLD t) {return tBottom(t) - tTop(t);}

inline bool tIsHorizontal (THRESHOLD t) {return tTop(t) == tBottom(t);}
inline bool tKeep(ITEMTAG tag, THRESHOLD t) { return (itemTagPersistEither(tag)) || (tHeight(t) >= MINCROP);}

inline THRESHOLD setTop   (THRESHOLD t, SPACE value) {t.s0 = value; return t;}
inline THRESHOLD setBottom(THRESHOLD t, SPACE value) {t.s1 = value; return t;}
inline THRESHOLD setLeft  (THRESHOLD t, SPACE value) {t.s2 = value; return t;}
inline THRESHOLD setRight (THRESHOLD t, SPACE value) {t.s3 = value; return t;}

inline float thresholdInvertedSlope ( ITEMTAG thresholdTag
                                    , THRESHOLD t
                                    ) {
    float slopeSign = itemTagPositiveSlope(thresholdTag) ? 1 : -1;
    return (tIsHorizontal(t)) ? VERTICALSLOPE
                              : (tRight(t) - tLeft(t)) / (tBottom(t) - tTop(t)) * slopeSign;
}

// Tesselation
#define T 0.5f // halfway value for tesselating curves.

// REF is a reference to an array with max index 32768
#define REF int

//  ---------------------------------- Structure Types  ------------------------------------

// A slice is a section of array indexes.
typedef struct Slice
  { REF sStart;
    REF sLength;
  } Slice;

#define boxLeft(box)   box.x
#define boxTop(box)    box.y
#define boxRight(box)  box.z
#define boxBottom(box) box.w

#define boxLeftTop(box) box.xy
#define boxRightBottom(box) box.zw

// A picture reference is a reference to bitmap data that can be the substance of a shape.
typedef struct PictUse
  {   int2 pictSize;      // size of the bitmap
       int pictMemOffset; // starting point of the pixel data in the memory buffer
  } PictUse;

typedef struct RadialGradient
  { float2 gradientCenter      ;
    float  gradientInnerRadius ;
    float  gradientOuterRadius ;
    COLOR  gradientInnerColor ;
    COLOR  gradientOuterColor;
  } RadialGradient;

 typedef struct LinearGradient
   { float2 gradientStart     ;
     float2 gradientEnd       ;
     COLOR  gradientStartColor;
     COLOR  gradientEndColor  ;
   } LinearGradient;

// A hard facet pointing to a specific picture.
typedef struct HardFacet
  { float2 facetP0;
    float2 facetP1;
    float2 facetP2;
    float2 facetT0;
    float2 facetT1;
    float2 facetT2;
  } HardFacet;

// PointQuery
typedef struct PointQuery
  { SPACE2 queryLocation;
    int queryTileId;
    int queryId;
  } PointQuery;

// The color state structure tracks the current color information during a scan through thresholds
typedef struct ColorState {
    GMEM SUBSTANCETAG *csSubstanceTagHeap;        // access to the substanceTagHeap
               COLOR   csBackgroundColor;         // background color
    GMEM       uchar  *csPictureData;             // global image information
    GMEM       uchar  *csDescriptions;            // global heap of substance descriptions
                int2   absolutePosition;          // the absolute position of the current pixel.
  } ColorState;

// the threshold queue stores references to the threshold buffers pointers to the start and end of the queue.
typedef struct ThresholdQueue {
               PMEM   ITEMTAG thresholdTags[MAXTHRESHOLDS]; // array of threshold thresholdTag
               PMEM THRESHOLD thresholds[MAXTHRESHOLDS];       // array of threshold geometry
                        Slice qSlice;       // slice, the position of the top of the queue.
} ThresholdQueue;

inline int cycleLocation(int i) {
  // return (i + MAXTHRESHOLDS) & MAXTHRESHOLDMASK;
  return i > MAXTHRESHOLDS ? i - MAXTHRESHOLDS : i;
  // this makes the position cyclic if MAXTHRESHOLDS is a power of 2
  // and MAXTHRESHOLDMASK = MAXTHRESHOLDS - 1
}

inline int tSLocation(ThresholdQueue *tQ, int i) {
  return cycleLocation(tQ->qSlice.sStart + i);
}

inline THRESHOLD getThreshold(ThresholdQueue *tQ, int index) {
    return tQ->thresholds[tSLocation(tQ, index)];
}

inline void setThreshold(ThresholdQueue *tQ, int index, THRESHOLD set) {
    tQ->thresholds[tSLocation(tQ,index)] = set;
}

inline ITEMTAG getThresholdTag(ThresholdQueue *tQ, int index) {
    return tQ->thresholdTags[tSLocation(tQ,index)];
}

inline void setThresholdTag(ThresholdQueue *tQ, int index, ITEMTAG set) {
    tQ->thresholdTags[tSLocation(tQ,index)] = set;
}

inline void pushTopSlot(ThresholdQueue *tQ) {
    tQ->qSlice.sStart = cycleLocation(tQ->qSlice.sStart - 1);
    tQ->qSlice.sLength += 1;
}

inline void popTop(ThresholdQueue *tQ) {
    tQ->qSlice.sStart = cycleLocation(tQ->qSlice.sStart + 1);
    tQ->qSlice.sLength -= 1;
}

inline REF queueSize(ThresholdQueue *tQ) {
    return tQ->qSlice.sLength;
}


#define RANDOMFIELDMASK RANDOMFIELDSIZE - 1

#define RANDOM_POS int

typedef struct ShapeState {
    ITEMTAG itemTagStack[MAXLAYERS];
        int itemCount;
} ShapeState;

inline void deleteItemId(ShapeState *shS, int i) {
  for (int j = i; j < shS->itemCount-1; j++) {
    shS->itemTagStack[j] = shS->itemTagStack[j+1];
  }
  shS->itemCount--;
}

inline void insertItem(ShapeState *shS, int i, ITEMTAG newThresholdTag) {
  for (int j = min(MAXLAYERS-1,shS->itemCount); j > i; j--) {
    shS->itemTagStack[j] = shS->itemTagStack[j-1];
  }
  shS->itemTagStack[i] = newThresholdTag;
  shS->itemCount = min(MAXLAYERS,shS->itemCount+1);
}

void showShapeState(ShapeState *shS, ColorState *cS);

inline void toggleItemActive(ShapeState *shS, ITEMTAG newItemTag) {
    bool done = false;
    int i = 0;
    while (i < shS->itemCount && !done) {
        ITEMTAG oldItemTag = shS->itemTagStack[i];
        if (itemTagSubstanceTagId(newItemTag) == itemTagSubstanceTagId(oldItemTag)) {
            // it is the same exact tag so delete it from the stack
            deleteItemId(shS,i);
            done = true;
        }
        else if (itemTagSubstanceTagId(newItemTag) < itemTagSubstanceTagId(oldItemTag)) {
            insertItem(shS,i,newItemTag);
            done = true;
        }
        i++;
    }
    if (!done) { // if we reach the bottom just insert on the end.
        insertItem(shS,i,newItemTag);
    }
}

typedef struct ParseState {
               int  currentThreshold;   // the current threshold bordering the section
               int  numActive;          // the next threshold that is not currently active.
            SPACE2  sectionStart;       // the top of the current vertical section being processed
            SPACE2  sectionEnd;         // the bottom of the current vertical section being processed
             float  pixelY;             // the bottom of the current pixel.
            float8  accColorArea;       // accumulated color and area.

               int  sectionCount;
               int  frameCount;
               int  buildCount;
        RANDOM_POS  randomFieldCursor;
    CMEM     float *randomField;
  } ParseState;

typedef struct Traversal {
    float4  travLeftControl;
    float2  travRight;
    float   travXPos;
    int     travIndex;
} Traversal;

// accessor functions for geometry data for variables used in geometry tree traversals
#define travLeft       travLeftControl.xy
#define travLeftX      travLeftControl.x
#define travLeftY      travLeftControl.y
#define travControl    travLeftControl.zw
#define travControlX   travLeftControl.z
#define travControlY   travLeftControl.w
#define travRightX     travRight.x
#define travRightY     travRight.y

// Function Prototypes

float intersectCurve( Traversal t
                    );

void bifurcateCurve( Traversal *t
                   );

void addLineSegment ( PMEM  ThresholdQueue *tQ
                    ,               float2  left
                    ,               float2  right
                    ,              ITEMTAG  itemTag
                    ,               float4  columnBox
                    );

void addThreshold ( PMEM ThresholdQueue *tQ
                  ,              float4  columnBox
                  ,              ITEMTAG  newThresholdTag
                  ,           THRESHOLD  newThreshold
                  );

bool traverseTree ( GMEM    float2 *strandHeap
                  ,            int  currentSize
                  ,          float4 columnBox
                  ,      Traversal *l
                  ,      Traversal *r
                  );

void searchTree(      Traversal *trav
               , GMEM    float4 *tree
               ,            int  treeSize
               ,           bool  isLeft
               );

void spawnThresholds ( PMEM  ThresholdQueue *tQ
                     ,               float4  columnBox
                     ,              ITEMTAG  itemTag
                     ,            Traversal *l
                     ,            Traversal *r
                     );

bool checkInRange ( Traversal *t
                  ,    float4  columnBox
                  );

inline THRESHOLD lineToThreshold ( float2  left
                                 , float2  right
                                 );

inline ITEMTAG lineToThresholdTag (    float2  left
                                  ,    float2  right
                                  ,   ITEMTAG  itemTag
                                  ,    float4  columnBox
                                  );

float thresholdIntersectX(    ITEMTAG thresholdTag
                         , THRESHOLD threshold
                         ,     float y
                         );

SPACE thresholdMidXLow( THRESHOLD t
                      ,    ITEMTAG tag
                      ,     SPACE yTop
                      ,     SPACE yBottom
                      ,     SPACE clampHigh
                      ,     SPACE clampLow
                      );

inline void divideThreshold( PMEM     ITEMTAG *thresholdTagTop
                           , PMEM   THRESHOLD *thresholdTop
                           , PMEM     ITEMTAG *thresholdTagBottom
                           , PMEM   THRESHOLD *thresholdBottom
                           ,            SPACE  splitX
                           ,            SPACE  splitY
                           );

inline void splitThreshold( PMEM     ITEMTAG *topThresholdTag
                          , PMEM  THRESHOLD *top
                          , PMEM     ITEMTAG *bottomThresholdTag
                          , PMEM  THRESHOLD *bottom
                          , PMEM      SPACE  splitY
                          );

int countActive ( PMEM ThresholdQueue *tQ
                ,               float *nextTop
                );

float nextSlicePoint ( PMEM ThresholdQueue *tQ
                     ,               float  slicePoint
                     ,                 int  numActive
                     );

void sliceActive( PMEM ThresholdQueue *tQ
                ,               float  slicePoint
                ,                 int  numActive
                );

float splitNext ( PMEM ThresholdQueue *tQ
                , PMEM     ParseState *pS
                );

inline bool thresholdIsAbove( ITEMTAG newThresholdTag
                            , THRESHOLD new
                            , ITEMTAG oldThresholdTag
                            , THRESHOLD old);

bool isAboveLast( PMEM ThresholdQueue *tQ
                  ,            ITEMTAG  newThresholdTag
                  ,         THRESHOLD  new
                  );

int findMatchAbove(PMEM ThresholdQueue *tQ
                  ,             float4  columnBox
                  ,            ITEMTAG  newThresholdTag
                  ,          THRESHOLD  newThreshold
                  );

void pushThreshold( PMEM ThresholdQueue *tQ
                  ,                   ITEMTAG  newThresholdTag
                  ,                THRESHOLD  new
                  );

void insertThreshold( PMEM ThresholdQueue *tQ
                    ,              ITEMTAG  newThresholdTag
                    ,           THRESHOLD  new
                    );

void deleteThreshold( PMEM ThresholdQueue *tQ
                    ,                 int  i
                    );

inline void passThreshold( PMEM ShapeState *shS
                         ,         ITEMTAG  thresholdTag
                         );

inline void passThresholdTop( PMEM ShapeState *shS
                            ,         ITEMTAG  thresholdTag
                            );

inline void passThresholdBottom( PMEM ShapeState *shS
                               ,         ITEMTAG  thresholdTag
                               );

inline void passThresholdPersistent( PMEM ShapeState *shS
                                   ,         ITEMTAG  thresholdTag
                                   );

COLOR readColor ( PMEM ColorState *cS
                , SUBSTANCETAG tag
                , FACETID currentFacet
                );

COLOR compositeLayers( PMEM    ShapeState *shS
                     , PMEM    ColorState *cS
                     );

void verticalAdvance( PMEM ThresholdQueue *tQ
                    , PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    ,              float4  columnBox
                    );

void horizontalAdvance ( PMEM ThresholdQueue *tQ
                       , PMEM     ParseState *pS
                       ,              float4  columnBox
                       );

void writePixelGlobal (        int2  columnDelta
                      ,        int2  bitmapSize
                      ,       COLOR  color
                      , GMEM   uint *out
                      ,         int  y
                      );

void buildThresholdArray ( PMEM  ThresholdQueue *tQ
                         , GMEM          float4 *geometryHeap
                         , GMEM       HardFacet *facetHeap
                         , GMEM         ITEMTAG *itemTagHeap
                         , GMEM       ITEMTAGID *itemTagIdHeap
                         ,         ITEMTAGIDREF  itemStart
                         ,                  int  progress
                         ,                  int  bacthSize
                         ,               float4  columnBox
                         );

void initRandomField( ParseState *pS
                    , CMEM float *randomField
                    , int blockId
                    , int columnDepth
                    , int columnThread
                    );

float getRandom(ParseState *pS);

inline Slice initQueueSlice();

void initThresholdQueue( PMEM ThresholdQueue  *tQ
                       ,               Slice   qSlice);

int thresholdBlockStart( int   blockId
                       , int   columnDepth
                       );

void loadThresholdQueue( PMEM ThresholdQueue  *tQ
                       , GMEM      THRESHOLD  *thresholdHeap
                       , GMEM         ITEMTAG  *thresholdTagHeap
                       ,                 int   blockId
                       ,                 int   columnDepth
                       ,                 int   columnThread
                       );

void saveThresholdQueue( PMEM ThresholdQueue  *tQ
                       , GMEM      THRESHOLD  *thresholdHeap
                       , GMEM         ITEMTAG  *thresholdTagHeap
                       ,                 int   blockId
                       ,                 int   columnDepth
                       ,                 int   columnThread
                       );

void initShapeState (PMEM    ShapeState *shS
                    );

int queueSliceStart (            int   blockId
                    ,            int   columnDepth
                    ,            int   columnThread
                    );

Slice loadQueueSlice( GMEM     Slice  *qSliceHeap
                    ,            int   blockId
                    ,            int   columnDepth
                    ,            int   columnThread
                    );

void storeQueueSlice( GMEM     Slice  *qSliceHeap
                    ,          Slice   qSlice
                    ,            int   blockId
                    ,            int   columnDepth
                    ,            int   columnThread
                    );

void initParseState ( PMEM ParseState *pS
                    ,             int  frameCount
                    , CMEM      float *randomField
                    , int blockId
                    , int columnDepth
                    , int columnThread
                    , float4 columnBox
                    );

float4 getTileBox (int  tileIndex);



bool pointTouchesThread( float4 columnBox
                       , SPACE2 point
                       );

bool isActiveThread (  float4 columnBox
                    ,    int2 bitmapSize
                    );

float8 sectionColor ( PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    , PMEM     ColorState *cS
                    );

void calculatePixel ( PMEM ThresholdQueue *tQ
                    , PMEM     ShapeState *shS
                    , PMEM     ParseState *pS
                    , PMEM     ColorState *cS
                    ,              float4  columnBox
                    );


void sortThresholdArray ( PMEM  ThresholdQueue *tQ);

void prepThresholdArray( PMEM ThresholdQueue *tQ
                       , PMEM     ShapeState *shS
                       ,              float4  columnBox
                       );

int parallelMaxInt(LMEM  int *parts
                  ,      int  x
                  ,      int  columnDepth
                  ,      int  columnThread
                  );

int parallelSumInt(LMEM  int *parts
                  ,      int  x
                  ,      int  columnDepth
                  ,      int  columnThread
                  );


int parallelScanInt (LMEM  int *parts
                    ,      int  x
                    ,      int  columnDepth
                    ,      int  columnThread
                    ,      int *finalLength
                    );

bool checkAdjacency( GMEM int4 *tileHeap
                   , GMEM  int *blockPtrs
                   ,       int  columnDepth
                   ,       int  i
                   ,       int  sizeLimit
                   ,      int4  prevTile
                   ,      int4  nextTile
                   );

bool compareTiles(int4 a, int4 b);

void mergeBlocks ( GMEM       int4 *tileHeap
                 , GMEM  THRESHOLD *thresholdHeap
                 , GMEM     ITEMTAG *thresholdTagHeap
                 , GMEM      Slice *qSliceHeap
                 , GMEM       int  *blockPtrs
                 , GMEM      bool  *activeFlags
                 ,            int   indexDst
                 ,            int   indexSrc
                 ,            int   columnDepth
                 ,            int   columnThread
                 , LMEM       int  *parts
                 );

void splitThresholdQueue( ThresholdQueue *tQSource
                        , ThresholdQueue *tQA
                        ,         float4  columnBoxA
                        , ThresholdQueue *tQB
                        ,         float4  columnBoxB
                        );

void splitTileVertical(      int4  source
                      , PMEM int4 *top
                      , PMEM int4 *bot
                      );

void copyBlock
    ( GMEM       int4 *tileHeapDst
    , GMEM  THRESHOLD *thresholdHeapDst
    , GMEM     ITEMTAG *thresholdTagHeapDst
    , GMEM      Slice *qSliceHeapDst
    , GMEM       int4 *tileHeapSrc
    , GMEM  THRESHOLD *thresholdHeapSrc
    , GMEM     ITEMTAG *thresholdTagHeapSrc
    , GMEM      Slice *qSliceHeapSrc
    ,             int  blockIdDst
    ,             int  blockIdSrc
    ,             int  columnThread
    ,             int  columnDepth
    );

 void renderThresholdArray ( PMEM  ThresholdQueue *tQ
                           , PMEM      ShapeState *shS
                           , GMEM         ITEMTAG *itemTagHeap
                           , GMEM    SUBSTANCETAG *substanceTagHeap
                           , GMEM       HardFacet *facetHeap
                           , GMEM           uchar *pictureData
                           , GMEM           uchar *descriptionHeap
                           , CMEM           float *randomField
                           ,                COLOR  backgroundColor
                           ,                 int2  bitmapSize
                           ,                  int  frameCount
                           ,                  int  blockId
                           ,                  int  columnDepth
                           ,                  int  columnThread
                           ,               float4  columnBox
                           ,                 int2  columnDelta
                           , GMEM            uint *out
                           );

SUBSTANCETAG identifyPoint ( PMEM  ThresholdQueue *tQ
                           , PMEM      ShapeState *shS
                           , GMEM       HardFacet *facetHeap
                           , GMEM    SUBSTANCETAG *substanceTagHeap
                           ,                  int  frameCount
                           ,                  int  queryId
                           ,               SPACE2  point
                           ,               float4  columnBox
                           );

void initColorState ( PMEM   ColorState *init
                    , GMEM SUBSTANCETAG *substanceTagHeap
                    ,             COLOR  backgroundColor
                    , GMEM        uchar *pictureData
                    , GMEM        uchar *descriptionHeap
                    ,              int2  absolutePosition
                    );

// Debug Functions

void showSubstanceTag(SUBSTANCETAG tag);
void showItemTag(ITEMTAG tag);

void showThresholdTag( ITEMTAG thresholdTag);
void showThresholdBox (THRESHOLD threshold);
void showThreshold( ITEMTAG thresholdTag,
                    THRESHOLD threshold);
void showThresholds (PMEM ThresholdQueue *tQ);
void showActiveThresholds (PMEM ThresholdQueue *tQ, int num);
// ------------------- Inline Function Bodies -----------------------------
// get the position of a pixel in a 2d bitmap
inline int pos2 (x, y, width) {
  return x + (mul24 (y, width));
}

// get the y position of the intersection between a vertical line and a non-vertical line
inline float yIntercept (float2 e0, float2 e1, float x) {
    return (((e1.y - e0.y) / (e1.x - e0.x)) * (x - e0.x)) + e0.y;
}

// get the y position of the intersection between a vertical line and a non-vertical line with slope
inline float yInterceptSlope (float2 e, float slope, float x) {
    return (slope * (x - e.x)) + e.y;
}

// get the intersection of a horizontal line and a non horizontal line (with slope > 0)
inline float xInterceptSlope (float2 e, float slope, float y) {
    return ((y - e.y) / slope) + e.x;
}

inline float xInterceptInvertedSlope (float2 e, float invertedSlope, float y) {
    return ((y - e.y) * invertedSlope) + e.x;
}

// make a uint pixel value from components
inline uint makePixelWord32 (float r, float g, float b) {
  return as_uint(convert_uchar4((COLOR)(b, g, r, 1.0) * MAXCHANNELFLOAT));
}

// make an opaque uint pixel from a float4 color
inline uint colorToSolidPixel_Word32_BGRA (COLOR color) {
  return as_uint(convert_uchar4((COLOR)(BLUE(color),GREEN(color),RED(color), 1.0) * MAXCHANNELFLOAT));
}

// load a uint color into a float4 color
inline COLOR loadPixel_Word32_BGRA(uint pixel) {
  uchar4 p = as_uchar4(pixel);
  return convert_float4((uchar4)(p.s2, p.s1, p.s0, MAXCHANNELUCHAR)) / MAXCHANNELFLOAT;
}

inline COLOR loadPixel_Word32_RGBA(uchar4 pixel) {
  return convert_float4(pixel)/ MAXCHANNELFLOAT;
}

inline COLOR getPicturePixel(GMEM uchar *pictData, int w, int x, int y) {
     GMEM uchar4 *pixelPointer = ((GMEM uchar4 *)pictData) + mul24(y, w) + x;
     //DEBUG_IF(printf("getPicturePixel x %i y %i w %i \n", x, y, w);)
     return loadPixel_Word32_RGBA(*pixelPointer);
}


// get the midpoint of a line segment.
inline float2 midPoint (float2 v0, float2 v1) {
  return ((1-T) * v0) + (T * v1);
}

// get the taxicab distance between two points
inline float taxiDistance (float2 v0, float2 v1) {
  return fabs(v1.x - v0.x) + fabs(v1.y - v0.y);
}

inline float fixNegativeZero(float x) {
    return fabs(x) < 0.000001 ? 0 : x;
}

// composite two colors where the background color may also be transparent.
inline COLOR composite(COLOR foreground, COLOR background) {
  float alphaOut = ALPHA(foreground) + ALPHA(background) * (1.0f - ALPHA(foreground));
  if (alphaOut > 0) {
     COLOR color = ((foreground * ALPHA(foreground)) + (background * ALPHA(background) * (1.0f - ALPHA(foreground)))) / alphaOut;
     return (COLOR) (RED(color),GREEN(color),BLUE(color),alphaOut);
  }
  else {
    return TRANSPARENT_COLOR;
  }
}

// composite two colors where the background is forced to be opaque.
inline COLOR compositeSolid(COLOR foreground, COLOR background) {
  COLOR color = foreground * ALPHA(foreground) + background * (1 - ALPHA(foreground));
  return (COLOR)(RED(color),GREEN(color),BLUE(color),1.0f);
}

inline float isVerticalThreshold(THRESHOLD t) {
  return tLeft(t)==tRight(t);
}

// find an x intercept of a threshold
float thresholdIntersectX( ITEMTAG thresholdTag
                         , THRESHOLD threshold
                         ,         float y
                         ) {
    if (isVerticalThreshold(threshold)) {
        //DEBUG_IF(printf("isVertical\n");)
        return tLeft(threshold);
    }
    else {
        return xInterceptInvertedSlope( tStart(thresholdTag, threshold)
                                      , thresholdInvertedSlope(thresholdTag, threshold)
                                      , y );
    }
}

// the x position of the midpoint of a threshold within a segment
SPACE thresholdMidXLow( THRESHOLD t
                      , ITEMTAG tag
                      , SPACE yTop
                      , SPACE yBottom
                      , SPACE clampLow
                      , SPACE clampHigh
                      ) {
    float yMid = yTop + ((yBottom - yTop) * 0.5f);
    float x = thresholdIntersectX(tag,t,yMid);
    return x >= clampHigh ? clampLow : max(clampLow, x); // like clamp but defaults to low value
}

inline void divideThreshold( PMEM     ITEMTAG *thresholdTagTop
                           , PMEM  THRESHOLD *thresholdTop
                           , PMEM     ITEMTAG *thresholdTagBottom
                           , PMEM  THRESHOLD *thresholdBottom
                           ,                 SPACE  splitX
                           ,                 SPACE  splitY
                           ) {
    if (itemTagPositiveSlope(*thresholdTagTop)) {
        /* *      *
            \      \
             \      *
              \      \
               *      * */
        *thresholdBottom = makeThreshold(splitY, tBottom(*thresholdTop), splitX, tRight(*thresholdTop));
        *thresholdTop    = makeThreshold(tTop(*thresholdTop), splitY, tLeft(*thresholdTop), splitX);
        // thresholdTagTop is unchanged.
        *thresholdTagBottom    = unPersist(*thresholdTagTop);
    }
    else {
        /*     *     *
              /     /
             /     *
            /     /
           *     *     */
        *thresholdBottom = makeThreshold(splitY, tBottom(*thresholdTop), tLeft(*thresholdTop), splitX);
        *thresholdTop    = makeThreshold(tTop(*thresholdTop), splitY, splitX, tRight(*thresholdTop));
        *thresholdTagBottom    = *thresholdTagTop;
        *thresholdTagTop       = unPersist(*thresholdTagTop);
    }
}

inline void splitThreshold(  PMEM     ITEMTAG *topThresholdTag
                           , PMEM  THRESHOLD *top
                           , PMEM     ITEMTAG *bottomThresholdTag
                           , PMEM  THRESHOLD *bottom
                           , PMEM           SPACE   splitY
                           ) {
    SPACE splitX = thresholdIntersectX(*topThresholdTag, *top, splitY);
    divideThreshold( topThresholdTag
                   , top
                   , bottomThresholdTag
                   , bottom
                   , splitX
                   , splitY
                   );
    //DEBUG_IF(printf("splitThreshold\n    ");\
    //         showThreshold(*topThresholdTag, *top);\
    //         printf("\n    ");\
    //         showThreshold(*bottomThresholdTag,*bottom);\
    //         printf("\n");\
    //        )
}

// Count active thresholds. The active thresholds are the
// first threshold in the queue and the following thresholds
// that share the same top.
int countActive ( PMEM ThresholdQueue *tQ
                ,               float *nextTop
                ) {
    // Get the y position of the top of the first threshold in the queue.
    float topOfFirst = tTop(getThreshold(tQ, 0));
    bool done = false;
    // The first threshold is always part of the active thresholds.
    int  numActive = 1;
    // Collect all of the following thresholds that have the same top.
    while (!done && numActive < queueSize(tQ)) {
        // Get the next threshold.
        THRESHOLD next = getThreshold(tQ, numActive);
        // If the next top is greater than the first top.
        if (tTop(next) > topOfFirst) {
            // Stop collecting.
            done = true;
            // Return the top y position of the threshold that is not collected.
            *nextTop = tTop(next);
        }
        else {
            numActive += 1;
        }
    }
    // Return the number of thresholds collected.
    return numActive;
}

// Find the lowest bottom value among the active thresholds
// excluding horizontal thresholds.
float nextSlicePoint ( PMEM ThresholdQueue *tQ
                     ,               float  slicePoint
                     ,                 int  numActive
                     ) {
    // Get the y position of the top of the first threshold in the queue.
    float topOfFirst = tTop(getThreshold(tQ, 0));
    // For each active threshold find the bottom with the minimum y value.
    for (int i = 0; i < numActive; i++) { // TODO: i could start at 1
        float bottom = tBottom(getThreshold(tQ, i));
        if (topOfFirst < bottom) { // check that it's not a horizontal threshold
            // if it's not horizontal, keep it if it's a lower value.
            slicePoint = min(slicePoint, bottom);
        }
    }
    return slicePoint;
}

// Split all active thresholds that extend beyond the splitPoint and insert both parts back into the
// queue.
void sliceActive( PMEM ThresholdQueue *tQ
                ,               float  slicePoint
                ,                 int  numActive
                ) {
    // For each active threshold
    for (int cursor = 0; cursor < numActive; cursor++) {
        ITEMTAG currentThresholdTag = getThresholdTag(tQ, cursor);
        THRESHOLD current = getThreshold(tQ, cursor);
        //DEBUG_IF(printf("cursor %i slicePoint %f ",cursor, slicePoint);showThreshold(currentThresholdTag,current);printf("\n");)
        // If the slicePoint cuts the threshold.
        if (tTop(current) < slicePoint && slicePoint < tBottom(current)) {
            ITEMTAG splitThresholdTag;
            THRESHOLD split;
            // split the thresold into to.
            splitThreshold( &currentThresholdTag
                          , &current
                          , &splitThresholdTag
                          , &split
                          ,  slicePoint
                          );
            //DEBUG_IF(printf("               current ");showThreshold(currentThresholdTag,current);printf("\n");)
            //DEBUG_IF(printf("               split   ");showThreshold(splitThresholdTag,split);printf("\n");)
            // store the top part of the split threshold back in the same place.
            setThresholdTag(tQ, cursor, currentThresholdTag);
            setThreshold(tQ, cursor, current);
            // if the bottom part of the split threshold is worth keeping (large enough or persistent)
            if (tKeep(splitThresholdTag,split)) {
                // reinsert it back into the queue.
                insertThreshold(tQ, splitThresholdTag, split);
                //DEBUG_IF(printf("split and insert\n");showThresholds(tQ);)
            }
        }
    }
}

// Count the active thresholds, find the next slicepoint, slice everything, return the slicePoint.
float splitNext( PMEM ThresholdQueue *tQ
               , PMEM     ParseState *pS
               ) {
    // the initial slice point is the maximum possible value.
    float slicePoint = FLT_MAX;
    // now count the active thresholds and get the first possible slice point which will be
    // next top beyond the active thresholds.
    pS->numActive = countActive(tQ, &slicePoint);
    // now find the next potential slicePoint, which will be any bottom that has a lower
    // value than the next top.
    slicePoint = min(slicePoint, nextSlicePoint(tQ, slicePoint, pS->numActive));
    // Now chop any active thresholds that extend across the slicePoint.
    sliceActive(tQ, slicePoint, pS->numActive);
    // And keep the slicePoint to define the vertical section we are working on.
    return slicePoint;
}

// Compare to thresholds and determine if a is below b in the sort order.
inline bool thresholdIsBelow( ITEMTAG aThresholdTag
                            , THRESHOLD a
                            , ITEMTAG bThresholdTag
                            , THRESHOLD b) {
    return (tTop(a) > tTop(b)) ||
                (
                    (tTop(a) == tTop(b)) &&
                        (
                            (tTopX(aThresholdTag, a) > tTopX(bThresholdTag, b)) ||
                            (
                                (tTopX(aThresholdTag, a) == tTopX(bThresholdTag, b)) &&
                                (thresholdInvertedSlope(aThresholdTag, a) > thresholdInvertedSlope(bThresholdTag, b))
                            )
                        )
                );
}

int findMatchAbove(PMEM ThresholdQueue *tQ
                  ,             float4  columnBox
                  ,            ITEMTAG  newThresholdTag
                  ,          THRESHOLD  newThreshold
                  ) {
    int  result = -1;
    if (tBottom(newThreshold) <= boxTop(columnBox)) {
        if (itemTagPersistEither(newThresholdTag)) {
            // this threshold is above the render area so check if there is a matching threshold that can be cancelled out.
            bool done = false;
            int  i = 0;
            while (!done && i < queueSize(tQ)) {
                ITEMTAG   checkTag = getThresholdTag(tQ, i);
                THRESHOLD checkThreshold = getThreshold(tQ, i);
                if (itemTagSubstanceTagId(checkTag) == itemTagSubstanceTagId(newThresholdTag)) {
                    if (tBottom(checkThreshold) <= boxTop(columnBox)) { // && persistEither(checkTag) unneccessary since non persistent can never be added.
                      done = true;
                      result = i;
                    }
                    // else the threshold is entirely above the top of the render area and not persistant it can be ignored.
                }
                else {
                    done = true;
                    // exit the loop without setting result.
                }
                i++;
           }
        }
    }
    return result;
}

void pushThreshold( PMEM ThresholdQueue *tQ
                  ,             ITEMTAG  newThresholdTag
                  ,           THRESHOLD  newThreshold
                  ) {
    if (queueSize(tQ) < MAXTHRESHOLDS) {
       pushTopSlot(tQ);
       setThresholdTag(tQ,0,newThresholdTag);
       setThreshold(tQ,0,newThreshold);
    }
}


void insertThreshold( PMEM ThresholdQueue *tQ
                    ,                   ITEMTAG  newThresholdTag
                    ,                THRESHOLD  new
                    ) {
    if (queueSize(tQ) < MAXTHRESHOLDS) {
        pushTopSlot(tQ);
        int cursor = 0;
        bool isBelow = true;
        while (cursor < (queueSize(tQ) - 1) && isBelow) {
            ITEMTAG oldThresholdTag = getThresholdTag(tQ, cursor + 1);
            THRESHOLD old = getThreshold(tQ, cursor + 1);
            isBelow = thresholdIsBelow(newThresholdTag, new, oldThresholdTag, old);
            if (isBelow) {
                setThresholdTag(tQ, cursor, oldThresholdTag);
                setThreshold(tQ, cursor, old);
                cursor += 1;
            }
        }
        setThresholdTag(tQ, cursor, newThresholdTag);
        setThreshold(tQ, cursor, new);
    }
}

void deleteThreshold( PMEM ThresholdQueue *tQ
                    ,                 int  i
                    ) {
    for (int cursor = i; cursor > 0; cursor--) {
        setThresholdTag(tQ, cursor, getThresholdTag(tQ, cursor-1));
        setThreshold(tQ, cursor, getThreshold(tQ, cursor-1));
    }
    popTop(tQ);
}

// create a threshold from a larger line segment and identifying information.
inline THRESHOLD lineToThreshold ( float2  left
                                 , float2  right
                                 ) {
    float tTop    = fmin(left.y,right.y);
    float tBottom = fmax(left.y,right.y);
    return makeThreshold(tTop, tBottom, left.x, right.x);
}

inline ITEMTAG lineToThresholdTag (    float2  left
                                 ,    float2  right
                                 ,   ITEMTAG  itemTag
                                 ,    float4  columnBox
                                 ) {
    bool  positiveSlope = left.y <= right.y;
    bool  notVertical = left.x != right.x;
    bool  touchingLeftBorder = left.x == boxLeft(columnBox);
    bool  isPersistent = notVertical && touchingLeftBorder;
    ITEMTAG slopeBit = positiveSlope ? ITEMTAG_SLOPE_POSITIVE : ITEMTAG_SLOPE_NEGATIVE;
    ITEMTAG persistantBit = isPersistent ? ITEMTAG_PERSISTANT : ITEMTAG_NONPERSISTANT;
    return slopeBit | persistantBit | itemTag;
}

// determine if a threshold should be added to the stack,
// if it's thresholdTag information should be pre-parsed (because it's above the render area)
// or if it should be ignored (below the render area, or a horizontal threshold that can be bypassed)
void addLineSegment ( PMEM  ThresholdQueue *tQ
                    ,               float2  left
                    ,               float2  right
                    ,              ITEMTAG  itemTag
                    ,               float4  columnBox
                    ) {
    THRESHOLD newThreshold = lineToThreshold( left
                                            , right
                                            );
    //DEBUG_IF(printf("left %2.6f %2.6f right %2.6f %2.6f\n", left.x, left.y, right.x, right.y);)
    ITEMTAG newThresholdTag = lineToThresholdTag( left
                                                , right
                                                , itemTag
                                                , columnBox
                                                );
    addThreshold(tQ, columnBox, newThresholdTag, newThreshold);
}

void addThreshold ( PMEM  ThresholdQueue *tQ
                  ,               float4  columnBox
                  ,               ITEMTAG  newThresholdTag
                  ,            THRESHOLD  newThreshold
                  ) {
    //DEBUG_IF(printf("beg add enclosed%i add %i ", *enclosedByStrand, addType);showThreshold(newThresholdTag, newThreshold);printf("\n");)
    //DEBUG_IF(printf("original ");showThreshold(newThresholdTag, newThreshold);printf("\n");)
    // in the beggining the slot at position numThresholds is free, we are either at the end of the list or just picked up the top
    // threshold from the holding queue
    if (tKeep(newThresholdTag, newThreshold)) {
        // horizontal thresholds that have no persistance can be ignored.
        //DEBUG_IF(printf("mid add enclosed %i add %i ", *enclosedByStrand, addType);showThreshold(newThresholdTag, newThreshold);printf("\n");)
        if ( ( tTop(newThreshold) <= boxBottom(columnBox) &&
               tLeft(newThreshold) < boxRight(columnBox) &&
               tBottom(newThreshold) > boxTop(columnBox) ) ||
             ( itemTagPersistEither(newThresholdTag) && tBottom(newThreshold) <= boxTop(columnBox) )
           ) {
                // if the threshold is entirely below the bottom of the render area is can be ignored
                // otherwise add it to the threshold array, and see if the bottom of the render area needs to be adjusted because the threshold array is not
                // large enough.g
                //DEBUG_IF(printf("  ADD");)

                //DEBUG_IF(printf("************** %016lx",newThresholdTag);)
                int match = findMatchAbove(tQ, columnBox, newThresholdTag, newThreshold);
                //DEBUG_IF(printf("addThreshold match %i ", match);showThreshold(newThresholdTag, newThreshold);printf("\n");)
                if (match >= 0) {
                  //DEBUG_IF(printf("DELETE ");showThreshold(getThresholdTag(tQ,match),getThreshold(tQ,match));printf("\n");)
                  deleteThreshold(tQ, match);
                }
                else {
                  pushThreshold(tQ, newThresholdTag, newThreshold);
                }
        }
        else {
            //DEBUG_IF(printf("  OUT");)
        }
    }
    else {
      //DEBUG_IF(printf("  NON");)
    }
    //DEBUG_IF(printf(" qSlice.sLength %i qSlice.sStart %i \n", tQ->qSlice.sLength, tQ->qSlice.sStart);)
    //tS->addThresholdCount -= 1; // This is just for debugging, it can be removed.
}


// -------------------- Functions for traversing geometry data heaps -------------------------

// find the intersectin with a curve after bifurcation by making new private addresses for leftControl and right.
float intersectCurve( Traversal t
                    ) {
    bifurcateCurve(&t);
    return (t.travLeftX == t.travRightX) ? min(t.travLeftY,t.travRightY) : yIntercept(t.travLeft, t.travRight, t.travXPos);
}

// adjust leftControl and right value by tesselating into the curve.
void bifurcateCurve( Traversal *t
                   ) {
    // if control == left then there is no control point, it's interpreted as linear.
    if (!(t->travLeftX == t->travControlX && t->travLeftY == t->travControlY)) { // if control is actually a control point (we indicate it's not by making it equal to left
        // bifurcate the curve until flatness is below threshold
        while (true) {
            float2 leftMid  = midPoint(t->travLeft, t->travControl);
            float2 rightMid = midPoint(t->travControl, t->travRight);
            float2 onCurve  = midPoint(leftMid,    rightMid);
            float  flatness = taxiDistance(t->travControl, onCurve);
            if (flatness > TAXICAB_FLATNESS) {
                if (t->travXPos < onCurve.x) { // bifurcate curve to left
                    t->travControl = leftMid;
                    t->travRight   = onCurve;
                }
                else { // bifurcate curve to right
                    t->travLeft    = onCurve;
                    t->travControl = rightMid;
                }
            } // if (flatness > TAXICAB_FLATNESS)
            else { // curve is flat enough
                break;
            }
        } // while
    } // if (!(dr_left == dr_control))
}

// After determining the position in the strand tree of the left and right border of the pixel
// Determine what thresholds need to be added to the buffer.
// This is generally on horizontal or angled threshold called the center,
// and potentially two vertical thresholds called the wings
void spawnThresholds ( PMEM  ThresholdQueue *tQ
                     ,               float4  columnBox
                     ,              ITEMTAG  itemTag
                     ,            Traversal *l
                     ,            Traversal *r
                     ) {
    float y_L; // this is the y value
    //DEBUG_IF(printf("spawnThresholds\n");)
    if (l->travLeftX >= boxLeft(columnBox)) { // if the left side is the left edge of the strand, don't bifurcate.
      y_L = l->travLeftY;
    }
    else { // bifurcate the curve to find the left side of the segment
      y_L = intersectCurve(*l);
    }
    bool leftWing;
    if ((l->travRightX < boxRight(columnBox)) && (l->travRightX > boxLeft(columnBox))) {
        // add the threshold to the current state or buffer.
        addLineSegment (  tQ
                       ,  (float2) (l->travXPos, y_L)
                       ,  l->travRight
                       ,  itemTag
                       ,  columnBox
                       );
        leftWing = true;
    }
    else {
        leftWing = false;
    }
    float y_R;
    if (r->travRightX <= boxRight(columnBox)) { // if the right side is the right side of the strand, don't bifurcate.
      y_R = r->travRightY;
    }
    else { // bifurcate the curve to find the right side of the center segment
      y_R = intersectCurve(*r);
    }
    bool rightWing;
    if ((r->travLeftX > boxLeft(columnBox)) && (r->travLeftX < boxRight(columnBox)) && (l->travIndex != r->travIndex)) {
        addLineSegment (  tQ
                       ,  r->travLeft
                       ,  (float2) (r->travXPos, y_R)
                       ,  itemTag
                       ,  columnBox
                       );
        rightWing = true;
    }
    else {
        rightWing = false;
    }
    if (l->travRightX < r->travLeftX || (!leftWing && !rightWing)) {
        float2 bridge_L = leftWing  || (l->travLeftX  == l->travRightX) ? l->travRight : (float2) (l->travXPos, y_L);
        float2 bridge_R = rightWing || (r->travLeftX  == r->travRightX) ? r->travLeft  : (float2) (r->travXPos, y_R);
        addLineSegment (  tQ
                       ,  bridge_L
                       ,  bridge_R
                       ,  itemTag
                       ,  columnBox
                       );
    }
}

// move to the next position in the tree based on the relationship between an XPosition and the current
// node in the tree. (test is either <= or < depending on if the search is biased to the left or the right.
void searchTree( Traversal *trav
               , GMEM float4 *tree
               , int treeSize
               , bool isLeft
               ){
    trav->travIndex = 0;
    while (trav->travIndex < treeSize) {
        float4 currentTree = tree[trav->travIndex];
        if ((trav->travXPos < currentTree.x) ||
            (isLeft && trav->travXPos == currentTree.x)) {
            /* notice his is not <= for the left traversal.*/
            trav->travRight = currentTree.xy;
            trav->travIndex = (trav->travIndex << 1) + 1; /* go Left  */
        }
        else { /*if (rightSide !test currentTree.x)*/
            trav->travLeftControl = currentTree;
            trav->travIndex = (trav->travIndex << 1) + 2; /* go Right */
        }
    }
}

// check if a section of a strand crosses a columnThread
bool checkInRange ( Traversal *t
                  , float4 columnBox
                  ) {
    return (t->travLeftX <= boxRight(columnBox) && t->travRightX > boxLeft(columnBox));
}

// A strand is a list of on curve and off curve coordinates pairs rearranged in an order that allows easy access to the left and right boundaries of the strand
// followed by a complete-binary-tree of the internal coordinate pairs.
// The order is designed to facilitate as many 64bit memory accesses as possible and determine the horizontal range of the curve with the minumum loads
// Because of this there are a lot of unused bits in the first 64 bits.
// Size refers to the number of 64 bits chunks that the loading function must skip ahead to get to the next tree so it includes the thresholdTag.
// In memory the format the thresholdTag should appear this way:
// | 64 bit              | 64 bit            | 64 bit          | 64 bit                | 64 bit            | ...
// | 32 bit   | 32 bit   | 32 bit  | 32 bit  | 32 bit | 32bit  | 32 bit    | 32bit     | 32 bit  | 32 bit  | 32 bit          | 32bit           | ...
// | uint     | uint     | float   | float   | float  | float  | float     | float     | float   | float   | float           | float           | ...
// | size + 1 | reserved | right.x | right.y | left.x | left.y | control.x | control.y | tree0.x | tree0.y | tree0 control.x | tree0 control.y | ...
// |                     | ending curve point| leftmost curve point                    | complete binary tree of point control point pairs     | rest of tree
// | strandHeap + 0      | strandHeap + 1    | strandHeap + 2                          | strandHeap + 4                                        | strandHeap + 6

bool traverseTree( GMEM    float2 *strandHeap
                 ,            int  currentSize
                 ,         float4  columnBox
                 ,      Traversal *l
                 ,      Traversal *r
                 ) {
    int    treeSize    =  (currentSize - 4) / 2; // take the total size in 64 bit parts and subtract the itemTag and the left and right points. Divide by 2 becasue each left+control is 128 bytes
    l->travRight       = *((GMEM float2 *)(strandHeap + 1)); // load the rightmost point of the strand.
    l->travLeftControl = *((GMEM float4 *)(strandHeap + 2)); // load the leftmost point of the strand and it's control point.
    GMEM float4 *tree =   (GMEM float4 *)(strandHeap + 4); // Move to tree portion of strand array.
    //DEBUG_IF(printf("l->travRight: %v2f l->travLeft: %v2f l->travControl: %v2f\n", l->travLeft, l->travControl, l->travRight);)
    bool inRange = checkInRange(l,columnBox);
    //DEBUG_IF(printf("inRange %i\n", inRange);)
    if (inRange) { // determine if the strand interacts at all with this columnThread.
        //DEBUG_IF(printf("--TREE--\n");showTree(*((float4 *)(strandHeap + 2)), *((float2 *)(strandHeap + 1)), tree, treeSize);)
        // now execute two tree traversals for the left and right borders of the pixel.
        *r = *l;
        // if the left side of the strand is within the pixel, treat the left side border as the tree traversal axis
        // otherwise treat the left border of the pixel as the axis.
        l->travXPos = max(boxLeft(columnBox), l->travLeftX);
        // do the same for the right side.
        r->travXPos = min(boxRight(columnBox), l->travRightX);
        searchTree(l, tree, treeSize, true);  // traverse the tree biased to the left
        searchTree(r, tree, treeSize, false); // traverse the tree biased to the right
        //DEBUG_IF(printf("i_L %i i_R %i\n", i_L, i_R);)
        // build up to three thresholds based on the tree traversals
    }
    return inRange;
}

inline COLOR getColorDescription(PMEM ColorState *cS
                                ,SUBSTANCETAG tag
                                ) {
    return *((COLOR*)(cS->csDescriptions + substanceTagDescriptionRef(tag)));
}

inline PictUse getPictUseDescription(PMEM ColorState *cS
                                    ,SUBSTANCETAG tag
                                    ) {
    return *((PictUse*)(cS->csDescriptions + substanceTagDescriptionRef(tag)));
}

inline RadialGradient getRadialDescription(PMEM ColorState *cS
                                          ,SUBSTANCETAG tag
                                          ) {
    return *((RadialGradient*)(cS->csDescriptions + substanceTagDescriptionRef(tag)));
}

inline LinearGradient getLinearDescription(PMEM ColorState *cS
                                          ,SUBSTANCETAG tag
                                          ) {
    return *((LinearGradient*)(cS->csDescriptions + substanceTagDescriptionRef(tag)));
}

inline float smooth(float edge0, float edge1, float x) {
  // Scale, bias and saturate x to 0..1 range
  x = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
  // Evaluate polynomial
  return x * x * (3 - 2 * x);
}

// read a color value depending on the substance and absolute position.
COLOR readColor ( PMEM ColorState *cS
                , SUBSTANCETAG tag
                , FACETID currentFacet
                ) {
    if (substanceTagIsSolidColor(tag)) {
        return getColorDescription(cS, tag);
    }
    else if (substanceTagIsTexture(tag)) { // its a picture reference
        PictUse pRef = getPictUseDescription(cS, tag);
        float scale = 1.0f;//pRef.pictScale;
        scale = scale < 0.0000001 ? 0.0000001 : scale;
        //DEBUG_IF(printf("scale %f \n", scale);)
        int2 relativePosition = convert_int2((convert_float2(cS->absolutePosition) / scale) /* - (pRef.pictTranslate*/);
        if (relativePosition.x >= 0 &&
            relativePosition.y >= 0 &&
            relativePosition.x < pRef.pictSize.x &&
            relativePosition.y < pRef.pictSize.y) {

            return getPicturePixel( cS->csPictureData + pRef.pictMemOffset
                                  , pRef.pictSize.x
                                  , relativePosition.x
                                  , relativePosition.y);
        }
        else {
            return TRANSPARENT_COLOR;
        }
    }
    else if (substanceTagIsRadial(tag)) {
      RadialGradient grad = getRadialDescription(cS, tag);
      float2 relativePosition = convert_float2(cS->absolutePosition);
      float dist = fast_distance(grad.gradientCenter, relativePosition);
      float ratio = smoothstep(grad.gradientInnerRadius, grad.gradientOuterRadius, dist);
      return (grad.gradientInnerColor * ratio) + (grad.gradientOuterColor * (1 - ratio));
    }
    else if (substanceTagIsLinear(tag)) {
      LinearGradient grad = getLinearDescription(cS, tag);
      float2 relativePosition = convert_float2(cS->absolutePosition);
      float startDist = taxiDistance(grad.gradientStart, relativePosition);
      //float2 endDist   = taxiDistance(grad.gradientEnd  , relativePosition);
      float w = taxiDistance(grad.gradientStart, grad.gradientEnd);
      float ratio = smoothstep(0,w,startDist);
      // float dist = distance(grad.gradientStart, grad.gradientEnd);
      return (grad.gradientStartColor * ratio) + (grad.gradientEndColor * (1 - ratio));
      //return (COLOR)(1.0,0,0,1);
    }
}

inline COLOR compositeLayer ( PMEM    ShapeState *shS
                            , PMEM    ColorState *cS
                            , COLOR   color
                            , SUBSTANCETAG substanceTag
                            , FACETID currentFacet
                            ) {
    COLOR nextColor = readColor ( cS
                                , substanceTag
                                , currentFacet
                                );
    //DEBUG_IF(printf("=========== composite layer color %2.2v4f nextcolor %2.2v4f", color, nextColor); showSubstanceTag(substanceTag);printf("\n");)
    return composite(color, nextColor);
}

COLOR compositeLayers( PMEM    ShapeState *shS
                     , PMEM    ColorState *cS
                     ) {
   COLOR color = TRANSPARENT_COLOR;
   SUBSTANCETAG prevSubstanceTag = NOSUBSTANCETAG;
   int layer = 0;
   FACETID currentFacet = NOFACET;
   while (layer < shS->itemCount && !(OPAQUE(color))) {
     ITEMTAG currentItemTag = shS->itemTagStack[layer];
     SUBSTANCETAG currentSubstanceTag = cS->csSubstanceTagHeap[itemTagSubstanceTagId(currentItemTag)];
     if (itemTagIsFacet(currentItemTag)) {
       currentFacet = itemTagFacetId(currentItemTag);
     }
     else {
        if (prevSubstanceTag != currentSubstanceTag) {
          bool topIsAdditive = itemTagIsAdd(currentItemTag);
          color = topIsAdditive ? compositeLayer (shS, cS, color, currentSubstanceTag, currentFacet) : color;
          currentFacet = NOFACET;
        }
     }
     layer += 1;
     prevSubstanceTag = currentSubstanceTag;
  }
  //DEBUG_IF(printf("=========== final color %2.2v4f ---------------------\n", color);)
  color = composite(color, cS->csBackgroundColor);
  return color;
}

inline void passThreshold( PMEM ShapeState *shS
                         ,         ITEMTAG  thresholdTag
                         ) {
    toggleItemActive(shS, thresholdTag);
}

inline void passThresholdTop( PMEM ShapeState *shS
                            ,         ITEMTAG  thresholdTag
                            ) {
    if (itemTagPersistTop(thresholdTag)) {
        passThreshold(shS, thresholdTag);
    }
}

inline void passThresholdBottom( PMEM ShapeState *shS
                               ,         ITEMTAG  thresholdTag
                               ) {
    if (itemTagPersistBottom(thresholdTag)) {
        passThreshold(shS, thresholdTag);
    }
}

inline void passThresholdPersistent( PMEM ShapeState *shS
                                   ,         ITEMTAG  thresholdTag
                                   ) {
    if (itemTagPersistEither(thresholdTag)) {
        passThreshold(shS, thresholdTag);
    }
}

// Parse all of the current shapes adding as many thresholds as possible.
// Return the bottom of the rendering area which is the bottom of the tile if everything fits and the last
// complete section if it doesn't.
void buildThresholdArray ( PMEM  ThresholdQueue *tQ
                         , GMEM          float4 *geometryHeap
                         , GMEM       HardFacet *facetHeap
                         , GMEM         ITEMTAG *itemTagHeap
                         , GMEM       ITEMTAGID *itemTagIdHeap
                         ,         ITEMTAGIDREF  itemStart
                         ,                  int  progress
                         ,                  int  batchSize
                         ,               float4  columnBox
                         ) {
    //DEBUG_IF(printf("~~~~~~~~~~~~~~~~~ batchSize %i ~~~~~~~~~~~~~~", batchSize );)
    for (int n = 0; n < batchSize; n++) { // iterate over every item in the current tile.
        int itemIndex = itemStart + progress + n;
        ITEMTAGID itemTagId = itemTagIdHeap[itemIndex]; // get the current itemTagId
        ITEMTAG   itemTag   = itemTagHeap[itemTagId]; // get the current itemTag itself
        //DEBUG_IF(printf("n %i id %i ",n, itemTagId);showItemTag(itemTag);printf("\n");)
        if (itemTagIsShape(itemTag)) {
             // if you don't shift the shape to the tile size there will be accuracy errors with height floating point geometric values
            int strandRef = itemTagShapeId(itemTag);
            //DEBUG_IF(printf("strandRef %i itemTagShapeId(itemTag) %i itemTagSubstanceTagId(itemTag) %i\n", strandRef, itemTagShapeId(itemTag), itemTagSubstanceTagId(itemTag));)
            if (queueSize(tQ) + 3 < MAXTHRESHOLDS) { // TODO: don't need this if generating in batches.
                GMEM float2 *strandHeap = (GMEM float2 *)&geometryHeap[strandRef];
                uint2 strandHeader = *((GMEM uint2 *)strandHeap);
                uint  currentSize = strandHeader.x; // size of current strand being parsed.
                Traversal left;
                Traversal right;
                // search the tree on the left and right sides of the pixel (or strand) and return 2 traversal result structures.
                bool inRange = traverseTree(  strandHeap
                                           ,  currentSize
                                           ,  columnBox
                                           , &left
                                           , &right
                                           );
                //DEBUG_IF(printf("currentSize %i inRange %i\n", currentSize, inRange);)
                if (inRange) {
                    spawnThresholds (  tQ
                                    ,  columnBox
                                    ,  itemTag
                                    , &left
                                    , &right
                                    );
                }
            }
        }
        else { // itemTagIsFacet
           //if (lastSubstance == (int)itemTagItemId(itemTag)) {
             // only add a facet if it has the same substance as the most recently added shape.
           //}
        }
    } // for n
}

void initRandomField( ParseState *pS
                    , CMEM float *randomField
                    , int blockId
                    , int columnDepth
                    , int columnThread
                    ) {
  // find a random starting point in the field passed on the absolute start position of the columnThread.
  int uniqueStart       = (int) ((((long)blockId) << columnDepth + columnThread) * LARGE_PRIME) & RANDOMFIELDMASK;
  pS->randomFieldCursor = uniqueStart;
  pS->randomField       = randomField;
}

float getRandom(ParseState *pS) {
    pS->randomFieldCursor = (pS->randomFieldCursor + 1) & RANDOMFIELDMASK;
    float random = pS->randomField[pS->randomFieldCursor];
    return random;
}

inline Slice initQueueSlice() {
    Slice qSlice;
    qSlice.sStart  = MAXTHRESHOLDS;
    qSlice.sLength = 0;
    return qSlice;
}

void initThresholdQueue( PMEM ThresholdQueue  *tQ
                       ,               Slice   qSlice) {
    tQ->qSlice = qSlice;
}

int thresholdBlockStart(  int   blockId
                        , int   columnDepth
                        ) {
    return ((blockId << columnDepth) * MAXTHRESHOLDS) ;
}

void loadThresholdQueue( PMEM ThresholdQueue  *tQ
                       , GMEM      THRESHOLD  *thresholdHeap
                       , GMEM         ITEMTAG  *thresholdTagHeap
                       ,                 int   blockId
                       ,                 int   columnDepth
                       ,                 int   columnThread
                       ) {
    int blockStart = thresholdBlockStart(blockId, columnDepth);
    //DEBUG_IF(printf("blockId %i columnDepth %i columnThread %i blockStart %i\n", blockId, columnDepth, columnThread, blockStart);)
    GMEM ITEMTAG *thresholdTags = thresholdTagHeap + blockStart;
    for (int i = 0; i<MAXTHRESHOLDS; i++) {
        tQ->thresholdTags[i] = thresholdTags[(i<<columnDepth) + columnThread];
    }
    GMEM THRESHOLD *thresholds = thresholdHeap + thresholdBlockStart(blockId, columnDepth);
    for (int i = 0; i<MAXTHRESHOLDS; i++) {
        tQ->thresholds[i] = thresholds[((i<<columnDepth) + columnThread)];
    }
    //DEBUG_IF(printf("done thresholds\n");)
}

void saveThresholdQueue( PMEM ThresholdQueue  *tQ
                       , GMEM      THRESHOLD  *thresholdHeap
                       , GMEM         ITEMTAG  *thresholdTagHeap
                       ,                 int   blockId
                       ,                 int   columnDepth
                       ,                 int   columnThread
                       ) {
    int blockStart = thresholdBlockStart(blockId, columnDepth);
    //DEBUG_IF(printf("blockId %i columnDepth %i columnThread %i blockStart %i\n", blockId, columnDepth, columnThread, blockStart);)
    GMEM THRESHOLD *thresholds = thresholdHeap + blockStart;
    //if ((columnThread & 0xFFFFFFFC) == columnThread) {
    for (int i = 0; i<MAXTHRESHOLDS; i++) {
        thresholds[((i<<columnDepth) + columnThread)] = tQ->thresholds[i];
    }
    //}

    GMEM ITEMTAG *thresholdTags = thresholdTagHeap + blockStart;
    //if ((columnThread % 4) == 0) {
    for (int i = 0; i<MAXTHRESHOLDS; i++) {
        thresholdTags[((i<<columnDepth) + columnThread)] = tQ->thresholdTags[i];
    }
   //}

}

void initShapeState (PMEM    ShapeState *shS
                    ) {
  shS->itemCount = 0;
}

int queueSliceStart ( int   blockId
                    , int   columnDepth
                    , int   columnThread
                    ) {
    return (blockId << columnDepth) + columnThread;
}

Slice loadQueueSlice( GMEM     Slice  *qSliceHeap
                    ,            int   blockId
                    ,            int   columnDepth
                    ,            int   columnThread
                    ) {
    Slice qSlice = qSliceHeap[queueSliceStart(blockId, columnDepth, columnThread)];
    //DEBUG_IF(printf("qSliceHeap %p columnThread %i qSlice.sStart %i qSlice.sLength %i\n", qSliceHeap, columnThread, qSlice.sStart, qSlice.sLength);)
    return qSlice;
}


void storeQueueSlice( GMEM     Slice  *qSliceHeap
                    ,          Slice   qSlice
                    ,            int   blockId
                    ,            int   columnDepth
                    ,            int   columnThread
                    ) {
    qSliceHeap[queueSliceStart(blockId, columnDepth, columnThread)] = qSlice;
}

void initParseState ( PMEM ParseState *pS
                    ,             int  frameCount
                    , CMEM      float *randomField
                    , int blockId
                    , int columnDepth
                    , int columnThread
                    , float4 columnBox
                    ) {
    pS->currentThreshold = 0;
    pS->numActive        = 0; // the next threshold that is not currently active.
    pS->accColorArea = (float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0));
    // if we go below render bottom we must rebuild the threshold list.
    pS->sectionStart = (SPACE2)(boxLeft(columnBox), boxTop(columnBox)); // the top of the current vertical section being processed
    pS->sectionEnd   = (SPACE2)(boxRight(columnBox),boxTop(columnBox)); // the bottom of the current vertical section being processed
    pS->sectionCount = 0;

    pS->frameCount = frameCount;
    pS->buildCount = 0;
    initRandomField(pS, randomField, blockId, columnDepth, columnThread);
}

// Create a binary value where the rightmost n bits are set to 1.
inline int bitmaskN(int n) {return (1 << n) - 1;}



// Threads are arranged into the tile in several horizontal rows depending on the relative number of
// availble compute threads vs the size of the tile. In small tiles, it can go all the way to one pixel per thread.
// This allows the system to adapt to different levels of detail in different tiles.
// The threads are organized into horizontal rows.
inline float4 initColumnBox( int4  tileBox
                           , int2  bitmapSize
                           ,  int  columnThread
                           ) {

    // The columnBox is the floating point box surrounding the pixels to be rendered.
    // the bottom is cropped to the size of the bitmap.
    return convert_float4((int4) ( boxLeft(tileBox) + columnThread
                                 , boxTop(tileBox)
                                 , boxLeft(tileBox) + columnThread + 1.0f
                                 , min(boxBottom(tileBox),bitmapSize.y)
                                 ));
}

bool pointTouchesThread(      float4  columnBox
                       ,      SPACE2  point
                       ) {
    return (point.x >= boxLeft(columnBox)    ) &&
           (point.x <  boxRight(columnBox)   ) &&
           (point.y >= boxTop(columnBox)     ) &&
           (point.y <  boxBottom(columnBox)  );

}

bool isActiveThread ( float4 columnBox
                    ,   int2 bitmapSize
                    ) {
    return (boxLeft(columnBox) < (float)bitmapSize.x) &&
           (boxTop (columnBox) < (float)bitmapSize.y);
}

float8 sectionColor ( PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    , PMEM     ColorState *cS
                    ) {
    float area = (pS->sectionEnd.x - pS->sectionStart.x) * (pS->sectionEnd.y - pS->sectionStart.y);
    if (area > 0.001) {
        //DEBUG_IF(printf("sectionColor sectionStart %v2f sectionEnd %v2f \n", pS->sectionStart, pS->sectionEnd);showShapeState(shS, cS);)
        COLOR color;
        color = compositeLayers( shS
                               , cS
                               );
        float random = getRandom(pS);
        float4 adjustedArea = (float4) (area + (area * random * STOCHASTIC_FACTOR));
        return (float8)(color * adjustedArea, adjustedArea);
    }
    else { // don't waste time calculating the color of insignificant sections.
        //DEBUG_IF(printf("no time sectionStart %v2f sectionEnd %v2f \n", pS->sectionStart, pS->sectionEnd);showShapeState(shS, cS);)
        return (float8)(TRANSPARENT_COLOR, (float4)0);
    }
}

void verticalAdvance( PMEM ThresholdQueue *tQ
                    , PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    ,              float4  columnBox
                    ) {
    if (pS->sectionEnd.x == boxRight(columnBox)) {
        // DEBUG_IF(printf("---------- Vertical Advance -------------- \n");)
        // Start by undoing all of the state changes from the horizontal traversal.
        // Occasionally a threshold gets skipped because they are out of order.
        // pass these.
        // Revert all horizontal border crossing from the last vertical advances.
        for (int i = 0; i < pS->numActive; i++) {
            passThreshold(shS, getThresholdTag(tQ, i));
        }
        //DEBUG_IF(printf("<-rev");)
        // Next break is the next natural break point (either the bottom of the pixel or the bottom of the render area.
        float nextBreak = min(boxBottom(columnBox), pS->pixelY);
        // The activeBottom is the bottom of the current group of horizontally adjacent thresholds or max float if none are left.
        float activeBottom = queueSize(tQ) > 0 ? tBottom(getThreshold(tQ, 0)) : FLT_MAX;
        // If the last section ended at the bottom of the current group.
        if (activeBottom == pS->sectionEnd.y) {
            // then pass over the bottom of all the active thresholds.
            //DEBUG_IF(printf("pS->numActive %i\n", pS->numActive);)
            while(pS->numActive > 0) {
                passThresholdBottom(shS, getThresholdTag(tQ, 0));
                popTop(tQ);
                pS->numActive -= 1;
            }

        }
        float nextBottom;
        if (pS->numActive > 0) {
            // if thresholds are still active, select between the next break and the bottom of the active thresholds.
            nextBottom = min(activeBottom,nextBreak);
        }
        else {
            // first find the top of the next threshoxld.
            // nextTop is either the top of the next available threshold of max float.
            float nextTop = pS->numActive < queueSize(tQ) ? tTop(getThreshold(tQ, pS->numActive)) : FLT_MAX;
            // nextBottom is the bottom of the next group of horizontally adjacent thresholds or the top of it depending on if it starts immediately.
            if (nextTop > pS->sectionEnd.y) {
                // there is a vertical gap. active thresholds stays empty and we add the gap to the accumulator.
                nextBottom = min(nextBreak, nextTop);
            }
            else {
                // there is no gap
                // extend active thresholds to include anything adjacent to the current section.
                nextBottom = min(nextBreak,splitNext(tQ,pS));
                //DEBUG_IF(printf("splitNext \n");showThresholds(tQ);)
                // pass over any leading horizontal thresholds (non persistance horizontal are never added).
                while (pS->numActive > 0 && tIsHorizontal(getThreshold(tQ, 0))) {
                    passThresholdTop(shS, getThresholdTag(tQ, 0));
                    popTop(tQ);
                    pS->numActive -= 1;
                }
                for (int i = 0; i < pS->numActive; i++) {
                    passThresholdTop(shS, getThresholdTag(tQ, i));
                }
            }
        }
        //DEBUG_IF(printf(">>> activeBottom %f nextBottom: %f \n", activeBottom, nextBottom);)
        // advance the section start vertically to the end of the current section. (in the beggining sectionStart.y == sectionEnd.y already so nothing happens.)
        pS->sectionStart.y = pS->sectionEnd.y;
        // advance the bottom of the section to either the next break or the bottom of the adjacent thresholds depending on what is next.
        pS->sectionEnd.y = nextBottom;
        // set the horizontal borders of the section to the far left to await a horizontal advance.
        pS->sectionStart.x = pS->sectionEnd.x = boxLeft(columnBox);
        // reset the cursor
        pS->currentThreshold = 0;
    }
}

void horizontalAdvance( PMEM ThresholdQueue *tQ
                      , PMEM     ParseState *pS
                      ,              float4  columnBox
                      ) {
    // find the right side of the current section.
    SPACE nextX;
    //DEBUG_IF(printf("do ..  pS->sectionEnd %v2f nextX %f\n", pS->sectionEnd, nextX);)
    if (pS->currentThreshold < pS->numActive) {
        //DEBUG_IF(printf("pS->currentThreshold < pS->numActive\n");)
        // find the midpoint of the threshold when bound by the section
        nextX = thresholdMidXLow( getThreshold(tQ, pS->currentThreshold)
                                , getThresholdTag(tQ, pS->currentThreshold)
                                , pS->sectionStart.y
                                , pS->sectionEnd.y
                                , boxLeft(columnBox)
                                , boxRight(columnBox)
                                );
        //DEBUG_IF(printf("midX %f\n", nextX);)
    }
    else {
        //DEBUG_IF(printf("else: nextX = boxRight(columnBox)\n");)
        nextX = boxRight(columnBox);
    }
    //DEBUG_IF(printf("nextX %f\n", nextX);)
    pS->sectionStart.x = pS->sectionEnd.x;
    pS->sectionEnd.x   = nextX;
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

// "Who creates an engine that can properly render an image with more than 128 horizontal thresholds in
// one pixel?" You might ask. And we reply "We do, that's who."

void calculatePixel ( PMEM ThresholdQueue *tQ
                    , PMEM     ShapeState *shS
                    , PMEM     ParseState *pS
                    , PMEM     ColorState *cS
                    ,              float4  columnBox
                    ) {
    //DEBUG_IF(printf("                                              pixelY: %f \n", pS->pixelY);)
    while (((pS->sectionEnd.x < boxRight(columnBox)) || (pS->sectionEnd.y < pS->pixelY))/*&& count > 0*/) { // process all sections that do not reach the bottom of the pixel.
        //DEBUG_IF(printf("loop        sectionStart %v2f sectionEnd %v2f \n", pS->sectionStart, pS->sectionEnd);)
        //DEBUG_IF(printf("beforeV cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);showShapeState(shS, cS);)
        verticalAdvance(tQ, pS, shS, columnBox);
        //DEBUG_IF(printf("afterV  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);showShapeState(shS, cS);)
        horizontalAdvance(tQ, pS, columnBox);
        //DEBUG_IF(printf("afterH  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);showShapeState(shS, CS);)
        //if (pS->pixelY < 16.0f) {
        //    DEBUG_IF(printf("pixelY: %f \n", pS->pixelY);showShapeState(shS, cS);)
        //    DEBUG_IF(printf("-------------- Active ---------------\n");showActiveThresholds(tQ,pS->numActive);)
        //}
        float8 colorArea = sectionColor( pS
                                       , shS
                                       , cS
                                       );
        pS->accColorArea += colorArea;
        //DEBUG_IF(printf("accColor %v8f\n", pS->accColorArea);)
        if (pS->currentThreshold < pS->numActive) {
            passThreshold(shS, getThresholdTag(tQ, pS->currentThreshold));
        }
        pS->currentThreshold += 1;
        pS->sectionCount += 1;
        //DEBUG_IF(printf("atEnd  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
    } // while (((pS->sectionEnd.x < boxRight(columnBox)) || (pS->sectionEnd.y < pS->pixelY)))
    //DEBUG_IF(printf("pixelDone\n");)=
}

// create an initial color state.
void initColorState ( PMEM   ColorState *init
                    , GMEM SUBSTANCETAG *substanceTagHeap
                    ,             COLOR  backgroundColor
                    , GMEM        uchar *pictureData
                    , GMEM        uchar *descriptionHeap
                    ,              int2  absolutePosition
                    ) {
  init->csSubstanceTagHeap = substanceTagHeap;
  init->csBackgroundColor = backgroundColor;
  init->csPictureData  = pictureData;
  init->csDescriptions = descriptionHeap;
  init->absolutePosition = absolutePosition;
}


inline bool swapIfAbove( PMEM ThresholdQueue *tQ
                       ,                 int  i
                       ) {
    ITEMTAG    aThresholdTag = getThresholdTag(   tQ, i    );
    THRESHOLD a       = getThreshold(tQ, i    );
    ITEMTAG    bThresholdTag = getThresholdTag(   tQ, i + 1);
    THRESHOLD b       = getThreshold(tQ, i + 1);
    bool swap =
        (tTop(a) > tTop(b)) ||
            (
                (tTop(a) == tTop(b)) &&
                    (
                        (tTopX(aThresholdTag, a) > tTopX(bThresholdTag, b)) ||
                        (
                            (tTopX(aThresholdTag, a) == tTopX(bThresholdTag, b)) &&
                            (thresholdInvertedSlope(aThresholdTag, a) > thresholdInvertedSlope(bThresholdTag, b))
                        )
                    )
            );
    if (swap) {
        setThresholdTag(   tQ, i    , bThresholdTag );
        setThreshold(tQ, i    , b       );
        setThresholdTag(   tQ, i + 1, aThresholdTag );
        setThreshold(tQ, i + 1, a       );
    }
    return !swap; // if true then tripwire activated
}

void sortThresholdArray( PMEM  ThresholdQueue *tQ
                       ) {
      // The selection sort algorithm
      bool done = false;
      // k = last item to be checked
      int k = ((int)queueSize(tQ)); // Check all items
      while (!done) {
         //DEBUG_IF(printf("k=%i\n",k);)
         done = true; // Set tripwire
         // Sort the unsort part a[k..n] (n = a.length)
         for (int i = 0 ; i < k-1 ; i++ ) {
            bool swapIf = swapIfAbove(tQ, i);
            done = done && swapIf;
            // if (get_global_id(1)==0) {printf("inside ------------ k %i i %i done %i \n",k, i, done);}
         }
         k--;  // Shorten the number of pairs checked.
         //DEBUG_IF(printf("kloop ------------ k %i done %i \n",k, done);showThresholds(tQ);)
      }
      //DEBUG_IF(printf("sort done\n");)
 }


void prepThresholdArray( PMEM ThresholdQueue *tQ
                       , PMEM     ShapeState *shS
                       ,              float4  columnBox
                       ) {
     // count all thresholds where the top is less than the top of the
     // threadColumn.
     int numAbove;
     bool done = false;
     while (!done && numAbove < queueSize(tQ)) {
         if (tTop(getThreshold(tQ,numAbove)) < boxTop(columnBox)) {
             numAbove++;
         }
         else {
             done = true;
         }
     }
      // Slice all active thresholds that pass over the zero mark.
     sliceActive(tQ,boxTop(columnBox),numAbove);
     // Pass over all active thresholds
     while(numAbove > 0) {
         passThresholdPersistent(shS, getThresholdTag(tQ,0));
         popTop(tQ);
         numAbove -= 1;
     }
}

 void renderThresholdArray ( PMEM  ThresholdQueue *tQ
                           , PMEM      ShapeState *shS
                           , GMEM         ITEMTAG *itemTagHeap
                           , GMEM    SUBSTANCETAG *substanceTagHeap
                           , GMEM       HardFacet *facetHeap
                           , GMEM           uchar *pictureData
                           , GMEM           uchar *descriptionHeap
                           , CMEM           float *randomField
                           ,                COLOR  backgroundColor
                           ,                 int2  bitmapSize
                           ,                  int  frameCount
                           ,                  int  blockId
                           ,                  int  columnDepth
                           ,                  int  columnThread
                           ,               float4  columnBox
                           ,                 int2  columnDelta
                           , GMEM            uint *out
                           ) {
    ParseState pS;
    initParseState( &pS
                  ,  frameCount
                  ,  randomField
                  ,  blockId
                  ,  columnDepth
                  ,  columnThread
                  ,  columnBox
                  );
    ColorState cS;
    initColorState( &cS
                  ,  substanceTagHeap
                  ,  backgroundColor
                  ,  pictureData
                  ,  descriptionHeap
                  ,  columnDelta
                  );
    // fix thresholds that start above
    //DEBUG_IF(printf("before prep\n");showThresholds(tQ);)
    prepThresholdArray(tQ,shS,columnBox);
    //DEBUG_IF(printf("after prep\n");showThresholds(tQ);)
    int yInt = -1;
    for (pS.pixelY = boxTop(columnBox)+1.0f; pS.pixelY <= boxBottom(columnBox); pS.pixelY += PIXELHEIGHT) { // y is the bottom of the current pixel.
        yInt += 1;
        calculatePixel (  tQ
                       ,  shS
                       , &pS
                       , &cS
                       ,  columnBox
                       );
        // write the accumulated color information to the pixel buffer.
        float4 color = pS.accColorArea.s0123 / pS.accColorArea.s4567;
        writePixelGlobal ( columnDelta
                         , bitmapSize
                         , color
                         , out
                         , yInt
                         );
        pS.accColorArea = (float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0));
        pS.sectionStart = (SPACE2)(boxLeft(columnBox),pS.pixelY);
        cS.absolutePosition += (int2)(0,1);
    } // for y

}

int parallelMaxInt (LMEM  int *parts
                   ,      int  x
                   ,      int  depth
                   ,      int  i
                   ) {
   //DEBUG_IF(printf("depth %i depth-1 %i 1<<(depth - 1) %i\n", depth, depth-1, 1<<(depth - 1));)
   // Initial value into local memory
   parts[i] = x;
   // Loop for computing localSums : divide WorkGroup into 2 parts
   barrier(CLK_LOCAL_MEM_FENCE);
   for (int stride = 1<<(depth - 1); stride > 0; stride = stride >> 1) {
       // Max elements 2 by 2 between local_id and local_id + stride
       if (i < stride) {
          parts[i] = max(parts[i],parts[i+stride]);
       }
       barrier(CLK_LOCAL_MEM_FENCE);
   }
   // Write result into partialSums[nWorkGroups]
   return parts[0];
}

int parallelSumInt (LMEM  int *parts
                   ,      int  x
                   ,      int  depth
                   ,      int  i
                   ) {
   //DEBUG_IF(printf("depth %i depth-1 %i 1<<(depth - 1) %i\n", depth, depth-1, 1<<(depth - 1));)
   // Initial value into local memory
   parts[i] = x;
   // Loop for computing localSums : divide WorkGroup into 2 parts
   barrier(CLK_LOCAL_MEM_FENCE);
   for (int stride = 1<<(depth - 1); stride > 0; stride = stride >> 1) {
       // Max elements 2 by 2 between local_id and local_id + stride
       if (i < stride) {
          parts[i] = parts[i] + parts[i+stride];
       }
       barrier(CLK_LOCAL_MEM_FENCE);
   }
   // Write result into partialSums[nWorkGroups]
   return parts[0];
}

int parallelScanInt ( LMEM int *parts
                    ,      int  x
                    ,      int  columnDepth
                    ,      int  i
                    ,      int *finalLength
                    ) {
    parts[i] = x;
    barrier(CLK_LOCAL_MEM_FENCE);
    for (int stride = 1; stride <= (1 << (columnDepth-1)); stride = stride << 1) {
        int j = i - stride;
        if (j >= 0) {
            parts[i] = parts[i] + parts[j];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    //DEBUG_IF(printf("(1 << columnDepth) - 1 = %i parts[(1 << columnDepth) - 1] %i\n",(1 << columnDepth) - 1,parts[(1 << columnDepth) - 1]);)
    *finalLength = parts[(1 << columnDepth) - 1];
    barrier(CLK_LOCAL_MEM_FENCE);
    return parts[i];
}

bool checkAdjacency( GMEM int4 *tileHeap
                   , GMEM  int *blockPtrs
                   ,       int  columnDepth
                   ,       int  i
                   ,       int  sizeLimit
                   ,      int4  prevTile
                   ,      int4  nextTile
                   ) {
    //DEBUG_IF(printf("sizeLimit %i i %i blockPtrs[i+1] %i\n", sizeLimit   , i, blockPtrs[i+1]);)
    int4 prevTile0 = i <= 0             ? prevTile : tileHeap[blockPtrs[i-1]];
    int4 nextTile0 = i >= sizeLimit - 1 ? nextTile : tileHeap[blockPtrs[i+1]];
    int4 currentTile = tileHeap[blockPtrs[i]];
    bool beforeMatches = compareTiles(prevTile0, currentTile);
    bool afterMatches  = compareTiles(nextTile0, currentTile);
    //DEBUG_IF(printf("prevTile0 %v4i nextTile0 %v4i currentTile %v4i beforeMatches %i afterMatches %i !(beforeMatches || afterMatches) %i\n"
    //               , prevTile0     ,nextTile0     ,currentTile     ,beforeMatches   ,afterMatches   ,!(beforeMatches || afterMatches));   )
    return !(beforeMatches || afterMatches);
}

bool compareTiles(int4 a, int4 b) {
    return  a.x == b.x &&
            a.y == b.y &&
            a.z == b.z &&
            a.w == b.w;
}

void mergeBlocks ( GMEM       int4  *tileHeap
                 , GMEM  THRESHOLD  *thresholdHeap
                 , GMEM    ITEMTAG  *thresholdTagHeap
                 , GMEM      Slice  *qSliceHeap
                 , GMEM        int  *blockPtrs
                 , GMEM       bool  *activeFlags
                 ,             int   indexDst
                 ,             int   indexSrc
                 ,             int   columnDepth
                 ,             int   columnThread
                 , LMEM        int  *parts
                 ) {
   int blockIdDest = blockPtrs[indexDst];
   int blockIdSrc  = blockPtrs[indexSrc];
   barrier(CLK_LOCAL_MEM_FENCE);
   //if (get_global_id(1) == 0) {printf("dest (%i,%i,%i) src (%i,%i,%i) tileHeap[blockIdDest] %v4i tileHeap[blockIdSrc] %v4i compareTiles %i\n",
   //                                indexDst,   blockIdDest,   activeFlags[indexDst],
   //                                indexSrc,   blockIdSrc,    activeFlags[indexSrc] ,
   //                                tileHeap[blockIdDest],
   //                                tileHeap[blockIdSrc],
   //                                compareTiles(tileHeap[blockIdDest],tileHeap[blockIdSrc]));
   //}
   int4 tileDst = tileHeap[blockIdDest];
   int4 tileSrc = tileHeap[blockIdSrc];
   if (compareTiles(tileDst,tileSrc) // do the blocks refer to the same tile.
       && activeFlags[indexDst]   // are both the destination and source blocks active
       && activeFlags[indexSrc]
       ) { // initial test to see if blocks are at all viable to merge.
         // now do the work to calculate
         int lengthA = loadQueueSlice(qSliceHeap, blockIdDest, columnDepth, columnThread).sLength;
         int lengthB = loadQueueSlice(qSliceHeap, blockIdSrc,  columnDepth, columnThread).sLength;
         //DEBUG_IF(printf("checking blockIdDest %i blockIdSrc %i lengthA %i lengthB %i\n",blockIdDest, blockIdSrc, lengthA, lengthB);)
         int maxSize = parallelMaxInt( parts
                                     , lengthA + lengthB
                                     , columnDepth
                                     , columnThread
                                     );
         //if (get_global_id(1) == 0) {printf("dest (%i,%i,%i) src (%i,%i,%i) maxSize %i\n",
         //                                indexDst, blockIdDest, activeFlags[indexDst],
         //                                indexSrc, blockIdSrc,  activeFlags[indexSrc],
         //                                maxSize);
         //}
         //DEBUG_IF(printf("columnThread %i maxSize %i \n",columnThread,maxSize);)
         if (maxSize < MAXTHRESHOLDS || (boxBottom(tileSrc) - boxTop(tileSrc)) <= 1) {
             Slice qSliceSource = loadQueueSlice(qSliceHeap, blockIdSrc, columnDepth, columnThread);
             ThresholdQueue tQSource;
             initThresholdQueue(&tQSource, qSliceSource);
             loadThresholdQueue(&tQSource, thresholdHeap, thresholdTagHeap, blockIdSrc, columnDepth, columnThread);

             Slice qSliceDest = loadQueueSlice(qSliceHeap, blockIdDest, columnDepth, columnThread);
             ThresholdQueue tQDest;
             initThresholdQueue(&tQDest, qSliceDest);
             loadThresholdQueue(&tQDest, thresholdHeap, thresholdTagHeap, blockIdDest, columnDepth, columnThread);
             while (queueSize(&tQSource) > 0 && queueSize(&tQDest) < MAXTHRESHOLDS) {
                 THRESHOLD threshold = getThreshold(&tQSource, 0);
                 ITEMTAG    itemTag    = getThresholdTag(&tQSource, 0);
                 popTop(&tQSource);
                 pushThreshold(&tQDest, itemTag, threshold);
             }
             saveThresholdQueue( &tQDest
                               ,  thresholdHeap
                               ,  thresholdTagHeap
                               ,  blockIdDest
                               ,  columnDepth
                               ,  columnThread
                               );
             storeQueueSlice( qSliceHeap
                            , tQDest.qSlice
                            , blockIdDest
                            , columnDepth
                            , columnThread
                            );
             if (columnThread == 0) {
                 activeFlags[indexSrc] = false;
             }
             //if (columnThread == 0) {
             //  printf("actually merged blockIdDest %i blockIdSrc %i queueSize %i\n",blockIdDest,blockIdSrc,queueSize(&tQDest));
             //}
         }
    }
}

__kernel void mergeBlockKernel
    ( GMEM       int4 *tileHeap
    , GMEM  THRESHOLD *thresholdHeap
    , GMEM    ITEMTAG *thresholdTagHeap
    , GMEM      Slice *qSliceHeap
    , GMEM        int *blockPtrs
    , GMEM       bool *activeFlags
    ,             int  columnDepth
    ,             int  numActive
    ,             int  strideExp
    ,             int  strideOffset
    , LMEM        int *parts
    ) {
    int stride       = 1 << strideExp;
    int strideMask   = 0xFFFFFFFF << (strideExp + 1);
    int blockThread  = get_global_id(0);
    int columnThread = get_global_id(1);
    // This kernel is indexed by each block Pointer.
    // Loop for computing localSums : divide WorkGroup into 2 parts
    // Merge elements 2 by 2 between local_id and local_id + stride
    // Merge elements 2 by 2 between local_id and local_id + stride
    bool onStride = (blockThread & strideMask) == blockThread;
    int  indexDst = blockThread + strideOffset;
    int  indexSrc = indexDst + stride;
    if (onStride // start point is on a stride.
       && (indexDst >= 0)                       // check that the destination point is range.
       && (indexSrc < numActive)       // check that the stride point is in range.
       ) {
        mergeBlocks( tileHeap
                   , thresholdHeap
                   , thresholdTagHeap
                   , qSliceHeap
                   , blockPtrs
                   , activeFlags
                   , indexDst
                   , indexSrc
                   , columnDepth
                   , columnThread
                   , parts
                   );
    }
}

__kernel void totalThresholdsKernel
    ( GMEM      Slice *qSliceHeap
    , GMEM        int *blockPtrs
    , GMEM       bool *activeFlags
    ,             int  columnDepth
    , LMEM        int *parts
    , GMEM        int *totals
    ) {
    int blockThread  = get_global_id(0);
    int columnThread = get_global_id(1);
    int total = 0;
    if (activeFlags[blockThread]) {
      ThresholdQueue tQ;
      int blockId = blockPtrs[blockThread];
      Slice qSlice = loadQueueSlice(qSliceHeap, blockId, columnDepth, columnThread);
      initThresholdQueue(&tQ, qSlice);
      total = parallelSumInt( parts
                            , queueSize(&tQ)
                            , columnDepth
                            , columnThread
                            );
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    if (columnThread == 0) {
      totals[blockThread] = total;
    }
}

__kernel void collectMergedBlocksKernel
    ( GMEM   int4 *tileHeap
    , GMEM   int  *blockPtrs
    , GMEM   bool *activeFlags
    ,        int   blockDepth
    , GMEM   int  *outputNumActive
    , GMEM   int4 *outputTiles
    , LMEM   int  *parts
    ) {
    int blockThread = get_global_id(1);
    int currentBlockId = blockPtrs[blockThread];
    int numActive, availableLength;
    int address;
    int isActive = activeFlags[blockThread];
    barrier(CLK_GLOBAL_MEM_FENCE);

    address = parallelScanInt ( parts
                              , isActive
                              , blockDepth
                              , blockThread
                              ,&numActive
                              );
    if (isActive) {
       //DEBUG_IF(printf("not available currentBlockId %i activeFlags[blockThread] %i address %i numActive %i\n",currentBlockId,activeFlags[blockThread],address, numActive);)
       blockPtrs[address - 1] = currentBlockId;
       activeFlags[   address - 1] = true;
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    address = parallelScanInt ( parts
                              ,!isActive
                              , blockDepth
                              , blockThread
                              ,&availableLength
                              );
    if (!isActive) {
       //DEBUG_IF(printf("available currentBlockId %i activeFlags[blockThread] %i address %i numActive %i\n",currentBlockId, activeFlags[blockThread],address, numActive);)
       blockPtrs[address + numActive - 1] = currentBlockId;
       activeFlags[   address + numActive - 1] = false;
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    if (blockThread == 0) {
       *outputNumActive = numActive;
       outputTiles[0] = (numActive > 0) ? tileHeap[blockPtrs[0]]             : NULLTILE;
       outputTiles[1] = (numActive > 0) ? tileHeap[blockPtrs[numActive - 1]] : NULLTILE;
       //DEBUG_IF(printf("numActive %i blockPtrs[0] %i blockPtrs[numActive - 1] %i\noutputTiles[0] %v4i tileHeap[blockPtrs[0]] %v4i outputTiles[1] %v4i tileHeap[blockPtrs[numActive - 1]] %v4i \n",
       //                 numActive,   blockPtrs[0],   blockPtrs[numActive - 1],    outputTiles[0],     tileHeap[blockPtrs[0]],     outputTiles[1],     tileHeap[blockPtrs[numActive - 1]]       );)
       //DEBUG_IF(printf("(numActive > 0) ? tileHeap[blockPtrs[0]]               : NULLTILE %v4i \n", (numActive > 0) ? tileHeap[blockPtrs[0]]: NULLTILE);)
    }
}

__kernel void collectRenderBlocksKernel
    ( GMEM   int4 *tileHeap
    , GMEM   int  *blockPtrs
    , GMEM   bool *activeFlags
    ,        int   numActive
    , GMEM  int4*  sideTiles
    ,        int   blockDepth
    ,        int   bufferSize
    ,        int   frameCount
    , GMEM   int  *outputNumActive
    , GMEM   int  *outputNumToRender
    , LMEM   int  *parts
    ) {
    // The blockId and the tileId are the same for this kernel.
    int blockThread    = get_global_id(1);
    int currentBlockId = blockPtrs[blockThread];
    int numToRender, splitLength;
    int address;
    bool isActive = activeFlags[blockThread];
    int4 prevTile = sideTiles[0];
    int4 nextTile = sideTiles[1];
    // If the block has no adjacent blocks with the same tile assignment then
    // it is singular and ready to render.
    barrier(CLK_GLOBAL_MEM_FENCE);
    bool isSingular =
           checkAdjacency( tileHeap
                         , blockPtrs
                         , blockDepth
                         , blockThread
                         , numActive
                         , prevTile
                         , nextTile
                         );
    // Collect all the blocks ready to split into the thresholdBuffers
    address = parallelScanInt ( parts
                              , isActive && (!isSingular) && (blockThread < numActive)
                              , blockDepth
                              , blockThread
                              ,&splitLength
                              );
    if ((!isSingular) && (blockThread < numActive)) {
       //DEBUG_IF(printf("isSingular %i blockThread %i address %i numActive %i\n", isSingular, blockThread, address, numActive);)
       blockPtrs[address - 1] = currentBlockId;
       activeFlags[   address - 1] = true;
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    // Collect all the blocks ready to render into a vector
    address = parallelScanInt ( parts
                              , isActive && isSingular && (blockThread < numActive)
                              , blockDepth
                              , blockThread
                              ,&numToRender
                              );
     if (isSingular && (blockThread < numActive)) {
         //DEBUG_IF(printf("isSingular %i blockThread %i address %i numActive %i\n", isSingular, blockThread, address, numActive);)
         blockPtrs[address + splitLength - 1] = currentBlockId;
         activeFlags[   address + splitLength - 1] = false;
     }
     barrier(CLK_GLOBAL_MEM_FENCE);
     if (blockThread == 0) {
         *outputNumActive  = splitLength;
         *outputNumToRender = numToRender;
     }
}

__kernel void initializeSectionKernel
    ( GMEM         int  *blockPtrs
    , GMEM        bool  *activeFlags
    ) {
    int  blockThread = get_global_id(0);
    // This kernel is indexed onces for each block of thresholds to generate.
    // The blockId and the tileId are the same for this kernel.
    blockPtrs[blockThread]   = blockThread;
    activeFlags[blockThread] = false;
}

__kernel void initializeBlockKernel
    ( GMEM        int4  *tileHeap
    , GMEM       Slice  *qSliceHeap
    , GMEM         int  *blockPtrs
    , GMEM        bool  *activeFlags
    ,              int   blockThread
    ,             int4   tile
    ,              int   columnDepth
    ) {
    // This kernel is indexed onces for each block of thresholds to generate.
    // The blockId and the tileId are the same for this kernel.
    int  blockId  = blockPtrs[blockThread];
    // Each column in the block has an individual thread.
    int  columnThread = get_global_id(1);
    storeQueueSlice(qSliceHeap, initQueueSlice(), blockId, columnDepth, columnThread);
    if (columnThread == 0) {
      activeFlags[blockThread] = true;
      tileHeap[blockId] = tile;
    }
}

__kernel void generateThresholdsKernel
    ( GMEM      float4  *geometryHeap
    , GMEM   HardFacet  *facetHeap
    , GMEM     ITEMTAG  *itemTagHeap
    , GMEM   ITEMTAGID  *itemTagIdHeap
    // Each itemSlice points to a list of itemTagIds in the heap.
    ,              int   blockPtr
    ,              int   itemStart
    ,              int   itemProgress
    ,              int   batchSize
    ,              int   columnDepth
    ,             int2   bitmapSize
    ,              int   frameCount
    // These buffers represent each buffer to be generated they are indexed by blockId
    , GMEM        int4  *tileHeap
    , GMEM   THRESHOLD  *thresholdHeap
    , GMEM      ITEMTAG *thresholdTagHeap
    , GMEM       Slice  *qSliceHeap
    , GMEM         int  *blockPtrs
    , GMEM        bool  *activeFlags
    , LMEM         int  *parts
    , GMEM         int  *outputMaxQueue
    ) {
    // Each column in the block has an individual thread.
    int  columnThread = get_global_id(1);
    // There is one tileBox for each block (even though these may be duplicates that represent the same tile.)
    int blockId = blockPtrs[blockPtr];
    int4 tileBox = tileHeap[blockId];
    // Now initial the tile info for every block.
    float4 columnBox = initColumnBox(tileBox, bitmapSize, columnThread);
    DEBUG_IF(printf("=========================== generate tile %v4i itemStart %i itemProgress %i batchSize %i blockPtr %i blockId %i \n", tileBox, itemStart, itemProgress, batchSize, blockPtr, blockId);/*showThresholds(&tQ);*/)
    ThresholdQueue tQ;
    initThresholdQueue(&tQ, initQueueSlice()); // needed for parallel max queue size
    if (isActiveThread(columnBox, bitmapSize)) {
        Slice qSlice = loadQueueSlice(qSliceHeap, blockId, columnDepth, columnThread);
        initThresholdQueue(&tQ, qSlice);
        loadThresholdQueue(&tQ, thresholdHeap, thresholdTagHeap, blockId, columnDepth, columnThread);
        // There is one slice from the itemHeap for every block.
        buildThresholdArray ( &tQ
                            ,  geometryHeap
                            ,  facetHeap
                            ,  itemTagHeap
                            ,  itemTagIdHeap
                            ,  itemStart
                            ,  itemProgress
                            ,  batchSize
                            ,  columnBox
                            );
        storeQueueSlice(qSliceHeap, tQ.qSlice, blockId, columnDepth, columnThread);
        saveThresholdQueue(&tQ, thresholdHeap, thresholdTagHeap, blockId, columnDepth, columnThread);
        //if (columnThread == 0) {printf("G---> queueSize %i blockThread %i\n",queueSize(&tQ), blockThread);}// showThresholds(&tQ);)
    }
    //if (columnThread == 0) {printf("A---> queueSize %i \n",queueSize(&tQ));}
    int mxQ = parallelMaxInt ( parts
                             , queueSize(&tQ)
                             , columnDepth
                             , columnThread
                             );
    if (columnThread == 0) {
        activeFlags[blockPtr] = true;
        *outputMaxQueue = mxQ;

        //printf("B---> mxQ %i queueSize %i\n", mxQ, queueSize(&tQ));
    }
    DEBUG_IF(printf("=========================== done generate tile %v4i\n", tileBox);/*showThresholds(&tQ);*/)
}

void splitThresholdQueue( ThresholdQueue *tQSource
                        , ThresholdQueue *tQA
                        ,         float4  columnBoxA
                        , ThresholdQueue *tQB
                        ,         float4  columnBoxB
                        ) {
    while (queueSize(tQSource) > 0) {
        THRESHOLD threshold = getThreshold(tQSource, 0);
        ITEMTAG    thresholdTag    = getThresholdTag(tQSource, 0);
        popTop(tQSource);
        addThreshold(tQA, columnBoxA, thresholdTag, threshold);
        addThreshold(tQB, columnBoxB, thresholdTag, threshold);
    }
}

void splitTileVertical(      int4  source
                      , PMEM int4 *top
                      , PMEM int4 *bot
                      ) {
  int split = (boxTop(source) + boxBottom(source)) / 2;
  (*top) = source;
  boxBottom((*top)) = split;
  (*bot) = source;
  boxTop((*bot)) = split;
}

void copyBlock
    ( GMEM       int4 *tileHeapDst
    , GMEM  THRESHOLD *thresholdHeapDst
    , GMEM    ITEMTAG *thresholdTagHeapDst
    , GMEM      Slice *qSliceHeapDst
    , GMEM       int4 *tileHeapSrc
    , GMEM  THRESHOLD *thresholdHeapSrc
    , GMEM    ITEMTAG *thresholdTagHeapSrc
    , GMEM      Slice *qSliceHeapSrc
    ,             int  blockIdDst
    ,             int  blockIdSrc
    ,             int  columnThread
    ,             int  columnDepth
    ) {
    if (columnThread == 0) {
      tileHeapDst[blockIdDst] = tileHeapSrc[blockIdSrc];
    }
    Slice qSliceSrc = loadQueueSlice(qSliceHeapSrc, blockIdSrc, columnDepth, columnThread);
    ThresholdQueue tQ;
    initThresholdQueue(&tQ, qSliceSrc);
    loadThresholdQueue(&tQ, thresholdHeapSrc, thresholdTagHeapSrc, blockIdSrc, columnDepth, columnThread);

    saveThresholdQueue( &tQ
                      ,  thresholdHeapDst
                      ,  thresholdTagHeapDst
                      ,  blockIdDst
                      ,  columnDepth
                      ,  columnThread
                      );
    storeQueueSlice( qSliceHeapDst
                   , tQ.qSlice
                   , blockIdDst
                   , columnDepth
                   , columnThread
                   );
    //if (columnThread == 0) {
    //    printf("copyBlock blockIdSrc %i blockIdDst %i queueSize %i\n", blockIdSrc, blockIdDst, queueSize(&tQ));
    //}
}


__kernel void splitBlocksKernel
    ( GMEM       int4 *tileHeapSrc
    , GMEM  THRESHOLD *thresholdHeapSrc
    , GMEM    ITEMTAG *thresholdTagHeapSrc
    , GMEM      Slice *qSliceHeapSrc
    , GMEM        int *blockIdPointersSrc
    , GMEM       bool *activeFlagsSrc
    ,             int  offsetSrc
    , GMEM       int4 *tileHeapDst
    , GMEM  THRESHOLD *thresholdHeapDst
    , GMEM    ITEMTAG *thresholdTagHeapDst
    , GMEM      Slice *qSliceHeapDst
    , GMEM        int *blockIdPointersDst
    , GMEM       bool *activeFlagsDst
    ,             int  offsetDst
    ,             int  columnDepth
    ,            int2  bitmapSize
    ,             int  numActive
    ) {
    // This kernel is indexed be each pair of tiles that need to be split.
    int blockThread  = get_global_id(0);
    int columnThread = get_global_id(1);
    //DEBUG_IF(printf("in splitTile blockThread %i jobStep %i numActive %i \n", blockThread, jobStep, numActive);)

    if (blockThread < numActive) {

        int   blockIdSrc   = blockIdPointersSrc[offsetSrc + blockThread];
        int   blockIdDst   = blockIdPointersDst[offsetDst + blockThread];
        //DEBUG_IF(printf("splitTile blockThread %i columnThread %i blockIdSrc %i blockIdDst %i \n", blockThread, columnThread, blockIdSrc, blockIdDst);)
        // The blockIdHeap is an array of pairs of blockPtrs, the first id is the source and top destination.
        // The second id is the bottom destination.
        // in this case each tileId is the same as each blockId the tiles
        // represent the destination tile for the thresholds once split.
        int4 tileSource = tileHeapSrc[blockIdSrc];
        //DEBUG_IF(printf("splitTile tileSource %v4i \n", tileSource);)
        int4 tileTop, tileBot;
        splitTileVertical(tileSource, &tileTop, &tileBot);
        //DEBUG_IF(printf("splitTile tileSource %v4i tileTop %v4i tileBot %v4i\n", tileSource, tileTop, tileBot);)
        barrier(CLK_GLOBAL_MEM_FENCE);
        tileHeapSrc[blockIdSrc] = tileTop;
        tileHeapDst[blockIdDst] = tileBot;
        activeFlagsDst[offsetDst + blockThread] = true;


        float4 columnBoxSource = initColumnBox(tileSource, bitmapSize, columnThread);
        float4 columnBoxTop    = initColumnBox(tileTop,    bitmapSize, columnThread);
        float4 columnBoxBot    = initColumnBox(tileBot,    bitmapSize, columnThread);

        if (isActiveThread(columnBoxSource, bitmapSize)) {
            // Load the source thresholds from blockIdA, this will become the top block after the split.
            Slice qSliceSrc = loadQueueSlice(qSliceHeapSrc, blockIdSrc, columnDepth, columnThread);
            ThresholdQueue tQSrc;
            initThresholdQueue(&tQSrc, qSliceSrc);
            loadThresholdQueue(&tQSrc, thresholdHeapSrc, thresholdTagHeapSrc, blockIdSrc, columnDepth, columnThread);

            // Top queue will be copied back into the source block.
            ThresholdQueue tQTop;
            initThresholdQueue(&tQTop, initQueueSlice());
            // The bottom queue will be copied into the destination block.
            ThresholdQueue tQBot;
            initThresholdQueue(&tQBot, initQueueSlice());

            splitThresholdQueue( &tQSrc
                               , &tQTop
                               ,  columnBoxTop
                               , &tQBot
                               ,  columnBoxBot
                               );
            saveThresholdQueue( &tQTop
                              ,  thresholdHeapSrc
                              ,  thresholdTagHeapSrc
                              ,  blockIdSrc
                              ,  columnDepth
                              ,  columnThread
                              );
            storeQueueSlice( qSliceHeapSrc
                           , tQTop.qSlice
                           , blockIdSrc
                           , columnDepth
                           , columnThread
                           );
            saveThresholdQueue( &tQBot
                              ,  thresholdHeapDst
                              ,  thresholdTagHeapDst
                              ,  blockIdDst
                              ,  columnDepth
                              ,  columnThread
                              );
            storeQueueSlice( qSliceHeapDst
                           , tQBot.qSlice
                           , blockIdDst
                           , columnDepth
                           , columnThread
                           );
            //if (columnThread == 0) {
            //  printf("split tileTop %v4i blockIdSrc %i queueSizeSrc %i tileBot %v4i blockIdDst %i queueSizeDst %i \n"
            //        ,       tileTop,     blockIdSrc,   queueSize(&tQTop), tileBot,  blockIdDst,   queueSize(&tQBot) );
            //}

        }
    }
}

__kernel void combineSectionKernel
    ( GMEM       int4 *tileHeapDst
    , GMEM  THRESHOLD *thresholdHeapDst
    , GMEM    ITEMTAG *thresholdTagHeapDst
    , GMEM      Slice *qSliceHeapDst
    , GMEM        int *blockIdPointersDst
    , GMEM       bool *activeFlagsDst
    ,             int  numActiveDst
    , GMEM       int4 *tileHeapSrc
    , GMEM  THRESHOLD *thresholdHeapSrc
    , GMEM    ITEMTAG *thresholdTagHeapSrc
    , GMEM      Slice *qSliceHeapSrc
    , GMEM        int *blockIdPointersSrc
    , GMEM       bool *activeFlagsSrc
    ,             int  numActiveSrc
    ,             int  blocksPerSection
    ,             int  columnDepth
    , GMEM        int *outputNumActiveDst
    , GMEM        int *outputNumActiveSrc
    ) {
    int blockThread = get_global_id(0);
    int columnThread = get_global_id(1);
    int available = min(blocksPerSection - numActiveDst, numActiveSrc);
    int tempId;
    if (blockThread < available) {
       int blockIdDst = blockIdPointersDst[blockThread + numActiveDst];
       int blockIdSrc = blockIdPointersSrc[blockThread];
       copyBlock ( tileHeapDst
                 , thresholdHeapDst
                 , thresholdTagHeapDst
                 , qSliceHeapDst
                 , tileHeapSrc
                 , thresholdHeapSrc
                 , thresholdTagHeapSrc
                 , qSliceHeapSrc
                 , blockIdDst
                 , blockIdSrc
                 , columnThread
                 , columnDepth
                 );
       activeFlagsDst[blockThread + numActiveDst] = true;
       activeFlagsSrc[blockThread + available     ] = false;
       tempId = blockIdPointersSrc[blockThread];
       blockIdPointersSrc[blockThread] = blockIdPointersSrc[blockThread + available];
       blockIdPointersSrc[blockThread + available] = tempId;
    }
    if (blockThread == 0 && columnThread == 0) {
        *outputNumActiveDst = numActiveDst + available;
        *outputNumActiveSrc = numActiveSrc - available;
    }
}

__kernel void sortThresholdsKernel
    ( GMEM       int4 *tileHeap
    , GMEM  THRESHOLD *thresholdHeap
    , GMEM    ITEMTAG *thresholdTagHeap
    , GMEM      Slice *qSliceHeap
    , GMEM        int *blockPtrs
    ,             int  numActive
    ,             int  numToRender
    ,             int  columnDepth
    ,            int2  bitmapSize
    ,             int  frameCount
    ) {
    int   blockThread = numActive + get_global_id(0);
    int   columnThread = get_global_id(1);
    int   blockId  = blockPtrs[blockThread]; // the sequential number of the tile in the current workgroup.
    int4  tileBox = tileHeap[blockId];
    float4 columnBox = initColumnBox(tileBox, bitmapSize, columnThread);
    if (blockThread < (numToRender + numActive) && isActiveThread(columnBox, bitmapSize)) {
        Slice qSlice = loadQueueSlice(qSliceHeap, blockId, columnDepth, columnThread);
        ThresholdQueue tQ;
        initThresholdQueue(&tQ, qSlice);
        loadThresholdQueue(&tQ, thresholdHeap, thresholdTagHeap, blockId, columnDepth, columnThread);
        //DEBUG_IF(printf("before------------\n");showThresholds(&tQ);)
        sortThresholdArray(&tQ);
        //DEBUG_IF(printf("after------------\n");showThresholds(&tQ);)
        saveThresholdQueue(&tQ, thresholdHeap, thresholdTagHeap, blockId, columnDepth, columnThread);
        // we don't need to store the qSlice here because it hasn't changed.
    }
}

__kernel void renderThresholdsKernel
    ( // Parameters passed between kernels
      GMEM         int4 *tileHeap
    , GMEM    THRESHOLD *thresholdHeap
    , GMEM      ITEMTAG *thresholdTagHeap
    , GMEM        Slice *qSliceHeap
    , GMEM          int *blockPtrs
    ,               int  numActive
    ,               int  numToRender
      // Constant parameters for every kernel
    , GMEM      ITEMTAG *itemTagHeap
    , GMEM SUBSTANCETAG *substanceTagHeap
    , GMEM    HardFacet *facetHeap
    , GMEM        uchar *pictureData
    , GMEM        uchar *descriptionHeap
    , CMEM        float *randomField
    ,             COLOR  backgroundColor
    ,               int  columnDepth
    ,              int2  bitmapSize
    ,               int  frameCount
    , GMEM         uint *out
    ) {
     int   blockThread = numActive + get_global_id(0);
     int   columnThread = get_global_id(1);
     int   blockId  = blockPtrs[blockThread]; // the sequential number of the tile in the current workgroup.
     int4  tileBox = tileHeap[blockId];
     float4 columnBox = initColumnBox(tileBox, bitmapSize, columnThread);
     int2   columnDelta = (int2)(boxLeft(tileBox) + columnThread, boxTop(tileBox));
     if (blockThread < (numToRender + numActive) && isActiveThread(columnBox, bitmapSize)) {
        Slice qSlice = loadQueueSlice(qSliceHeap, blockId, columnDepth, columnThread);
        //DEBUG_IF(printf("qSliceHeap %p columnThread %i qSlice.sStart %i qSlice.sLength %i\n", qSliceHeap, columnThread, qSlice.sStart, qSlice.sLength);)
        ThresholdQueue tQ;
        initThresholdQueue(&tQ, qSlice);
        loadThresholdQueue(&tQ, thresholdHeap, thresholdTagHeap, blockId, columnDepth, columnThread);
        ShapeState shS;
        initShapeState(&shS);
        //DEBUG_IF(printf("blockId %i columnBox %2.6v4f\n",blockId, columnBox);)
        DEBUG_IF(printf("=========================== render tile %v4i queueSize %i blockThread %i blockId %i \n", tileBox, queueSize(&tQ), blockThread, blockId);/*showThresholds(&tQ);*/)
        renderThresholdArray ( &tQ
                             , &shS
                             ,  itemTagHeap
                             ,  substanceTagHeap
                             ,  facetHeap
                             ,  pictureData
                             ,  descriptionHeap
                             ,  randomField
                             ,  backgroundColor
                             ,  bitmapSize
                             ,  frameCount
                             ,  blockId
                             ,  columnDepth
                             ,  columnThread
                             ,  columnBox
                             ,  columnDelta
                             ,  out
                             );
        DEBUG_IF(printf("=========================== done render tile %v4i\n", tileBox);/*showThresholds(&tQ);*/)
    }
}

SUBSTANCETAG identifyPoint ( PMEM  ThresholdQueue *tQ
                           , PMEM      ShapeState *shS
                           , GMEM       HardFacet *facetHeap
                           , GMEM    SUBSTANCETAG *substanceTagHeap
                           ,                  int  frameCount
                           ,                  int  queryId
                           ,               SPACE2  point
                           ,               float4  columnBox
                           ) {
     for (int i = 0; i < queueSize(tQ); i++) {
       if (tBottom(getThreshold(tQ,i))<=point.y) {
           passThreshold(shS, getThresholdTag(tQ,i));
         }
     }
     if (shS->itemCount == 0) {
         return NOSUBSTANCETAG;
     }
     else {
         return substanceTagHeap[itemTagSubstanceTagId(shS->itemTagStack[0])];
     }
}

__kernel void pointQueryKernel
   ( GMEM         int4 *tileHeap
   , GMEM    THRESHOLD *thresholdHeap
   , GMEM      ITEMTAG *thresholdTagHeap
   , GMEM        Slice *qSliceHeap
   , GMEM    HardFacet *facetHeap
   , GMEM SUBSTANCETAG *substanceTagHeap
   ,               int  columnDepth
   ,              int2  bitmapSize
   ,               int  frameCount
   ,               int  jobStep
   ,               int  jobOffset
   ,               int  numQueries
   , GMEM   PointQuery *queryHeap
   , GMEM SUBSTANCETAG *queryResults
   ) {
    int  blockId      = get_global_id(0);
    int  columnThread = get_global_id(1);
    // WRONG
    int4 tileBox = tileHeap[blockId];
    float4 columnBox = initColumnBox(tileBox, bitmapSize, columnThread);
    //int2   columnDelta = (int2)(boxLeft(tileBox) + columnThread, boxTop(tileBox));
    for (int i = 0; i < numQueries; i++) {
        PointQuery query = queryHeap[i];
        if (blockId == 0 && columnThread == 0) {
          // Just one thread in the tile defaults the EMPTYSUBSTANCETAG.
          queryResults[i] = NOSUBSTANCETAG;
        }
        if (isActiveThread(columnBox, bitmapSize) && pointTouchesThread(columnBox,query.queryLocation)) {
            Slice qSlice = loadQueueSlice(qSliceHeap, blockId, columnDepth, columnThread);
            ThresholdQueue tQ;
            initThresholdQueue(&tQ, qSlice);
            loadThresholdQueue(&tQ, thresholdHeap, thresholdTagHeap, blockId, columnDepth, columnThread);
            ShapeState shS;
            initShapeState(&shS);
            //DEBUG_IF(printf("render shapeState\n");)
            //DEBUG_IF(showShapeState(&shS);)
            //DEBUG_IF(showThresholds(&tQ);)
            SUBSTANCETAG substanceTag = identifyPoint ( &tQ
                                                      , &shS
                                                      ,  facetHeap
                                                      ,  substanceTagHeap
                                                      ,  frameCount
                                                      ,  query.queryId
                                                      ,  query.queryLocation
                                                      ,  columnBox
                                                      );
            //DEBUG_IF(printf("******** substanceId %i\n", substanceTagId);)
            if (substanceTag != NOSUBSTANCETAG) {
               queryResults[i] = substanceTag; // this should only happen for one thread.
            }
        }
    }
}


void showShapeState(ShapeState *shS, ColorState *cS) {
   printf("--- Item Layers --- %i \n",shS->itemCount);
   for (int i = 0; i < shS->itemCount; i++) {
     ITEMTAG itemTag = shS->itemTagStack[i];
     SUBSTANCETAG substanceTag = cS->csSubstanceTagHeap[itemTagSubstanceTagId(itemTag)];
     printf("%i ", i);showItemTag(itemTag);showSubstanceTag(substanceTag);
     if (substanceTagIsSolidColor(substanceTag)) {
         printf("color %2.2v4f", getColorDescription(cS, substanceTag));
     } else if (substanceTagIsTexture(substanceTag)) { // its a picture reference
         PictUse pRef = getPictUseDescription(cS, substanceTag);
         printf("pictSize %03v2i offset %08x", pRef.pictSize, pRef.pictMemOffset);
     }
     printf("\n");
   }
}

void showSubstanceTag(SUBSTANCETAG tag) {
  if (tag == NOSUBSTANCETAG) {
    printf("NOSUBSTANCETAG ");
  }
  else {
    printf("type %i ref %i ", substanceTagType(tag), substanceTagDescriptionRef(tag));
  }
}

void showItemTag(ITEMTAG tag) {
  if (itemTagPositiveSlope(tag)) {
    printf("[+] ");
  }
  else {
    printf("[-] ");
  }
  if      (itemTagPersistTop(tag)   ) {printf("pTop ");}
  else if (itemTagPersistBottom(tag)) {printf("pBot ");}
  else                                {printf("pNon ");}
  printf("isShape %i add %i substanceid %02i refid %02i ", itemTagIsShape(tag), itemTagIsAdd(tag), itemTagSubstanceTagId(tag), itemTagFacetId(tag));
}

void showThresholdTag( ITEMTAG itemTag
                     ) {

    if      (itemTagPersistTop(itemTag)   ) {printf("pTop ");}
    else if (itemTagPersistBottom(itemTag)) {printf("pBot ");}
    else                                    {printf("pNon ");}
    printf("itemTagSubstanceTagId %06i ", itemTagSubstanceTagId(itemTag));
}

void showThresholdBox (THRESHOLD threshold) {
  //printf("tTop: %2.2f tBottom: %2.2f tLeft: %2.2f tRight: %2.2f "
  printf("tLeft: %f tTop: %f tRight: %f tBottom: %f height: %f"
        , tLeft(threshold)
        , tTop(threshold)
        , tRight(threshold)
        , tBottom(threshold)
        , tHeight(threshold)
        );
}

void showThreshold( ITEMTAG itemTag,
                    THRESHOLD threshold) {
    showItemTag(itemTag);
    showThresholdBox(threshold);
}

void showThresholds (PMEM ThresholdQueue *tQ) {
    printf ("Thresholds numThresholds %2i \n", queueSize(tQ));
    showActiveThresholds(tQ,queueSize(tQ));
}

void showActiveThresholds (PMEM ThresholdQueue *tQ, int num) {
    for (int t = 0; t < num; t++) {
        if (t < MAXTHRESHOLDS) {
        printf("t: %2i ", t);
        showThreshold( getThresholdTag(tQ, t)
                     , getThreshold(tQ, t)
                     );
        printf("\n");
        }
        else {printf("out of bounds\n\n\n");}
    }
}
