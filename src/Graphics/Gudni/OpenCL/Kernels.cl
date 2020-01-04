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
// ---------------- Macros, Type definitions and type accessors -----------------------------------

#define MAXTHRESHOLDMASK (MAXTHRESHOLDS - 1)

#define MINCROP 0.2f
// Debugging
#define INDEX get_global_id(0)
#define COLUMN get_global_id(1)

#ifdef DEBUG_OUTPUT
#define DEBUG_IF(statement) if (COLUMN == DEBUGCOLUMN && INDEX == DEBUGINDEX) {statement} // on the fly debugging output
#else
#define DEBUG_IF(statement)
#endif

#ifdef DEBUG_TRACE
#define DEBUG_TRACE_BEGIN if (COLUMN == DEBUGCOLUMN && INDEX == DEBUGINDEX) {printf("[ListStart\n");}
#define DEBUG_TRACE_ITEM(statement)  if (COLUMN == DEBUGCOLUMN && INDEX == DEBUGINDEX) {printf(", "); statement printf("\n");} // debugging output for parsing by TraceVisualizer
#define DEBUG_TRACE_END if (COLUMN == DEBUGCOLUMN && INDEX == DEBUGINDEX) {printf("]\n");}
#else
#define DEBUG_TRACE_BEGIN
#define DEBUG_TRACE_ITEM(statement)
#define DEBUG_TRACE_END
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
#define LARGE_PRIME 7919
#define PIXELHEIGHT 1.0f
#define COLUMNSPACING 1.0f
#define MAXCHANNELUCHAR 0xFF   // maximum value for converting from uchar color value
#define MAXCHANNELFLOAT 255.0f // maximum value for converting from float color value

// Sides of the pixel
#define LEFTBORDER  0.0f
#define RIGHTBORDER COLUMNSPACING

// Color
#define COLOR float4
#define RED(color)   color.s0 // red   channel of float4 color
#define GREEN(color) color.s1 // green channel of float4 color
#define BLUE(color)  color.s2 // blue  channel of float4 color
#define ALPHA(color) color.s3 // alpha channel of float4 color
#define OPAQUE(color) ALPHA(color) == 1.0f // determine if the color is not opaque
#define TRANSPARENT_COLOR (COLOR)(0,0,0,0) // constant clear pixel
#define TRANSPARENT_COLOR_ZERO_AREA ((float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0)))

// An ITEMTAGID refers to the index of an item tag heap.
#define ITEMTAGID  uint
// An Item tag links to either a shape or a texture facet.
// See Graphics.Gudni.Raster.Constants for the bit layouts of ITEMTAG type.
#define ITEMTAG ulong
// A FacetID refers to the index of a facet in the facet heap.
#define FACETID   uint
// An outline id refers to the index of a geoRef in the geoRefHeap.
#define SHAPEID   uint

#define SUBSTANCEID uint

inline bool itemTagIsFacet(ITEMTAG tag)    {return (tag & ITEMTAG_ISFACET_BITMASK) == ITEMTAG_ISFACET;}
inline bool itemTagIsShape(ITEMTAG tag)  {return (tag & ITEMTAG_ISFACET_BITMASK) == ITEMTAG_ISSHAPE;}
inline bool itemTagIsAdd(ITEMTAG tag)      {return (tag & ITEMTAG_COMPOUNDTYPE_BITMASK) == ITEMTAG_COMPOUNDTYPE_ADD;}
inline bool itemTagIsSubtract(ITEMTAG tag) {return (tag & ITEMTAG_COMPOUNDTYPE_BITMASK) == ITEMTAG_COMPOUNDTYPE_SUBTRACT;}
inline SUBSTANCEID itemTagSubstanceId(ITEMTAG tag) {return (uint) ((tag & ITEMTAG_SUBSTANCE_ID_BITMASK) >> ITEMTAG_SUBSTANCE_ID_SHIFT);}
inline FACETID itemTagFacetId(ITEMTAG tag) {return (uint) (tag & ITEMTAG_ITEM_ID_BITMASK);}
inline SHAPEID itemTagShapeId(ITEMTAG tag) {return (uint) (tag & ITEMTAG_ITEM_ID_BITMASK);} // this is the same but here for consistency.

// A SUBSTANCETAGID refers to the index of a substance tag in the substancetag heap.
#define SUBSTANCETAGID uint
//  A substance tag determines the type of a substance and contains a pointer to the substance description.
#define SUBSTANCETAG ulong

inline bool substanceTagType(SUBSTANCETAG tag) {return (tag & SUBSTANCETAG_TYPE_BITMASK) >> 56;}
inline bool substanceTagIsSolidColor(SUBSTANCETAG tag) {return (tag & SUBSTANCETAG_TYPE_BITMASK) == SUBSTANCETAG_TYPE_SOLID_COLOR;}
inline bool substanceTagIsTexture(SUBSTANCETAG tag)    {return (tag & SUBSTANCETAG_TYPE_BITMASK) == SUBSTANCETAG_TYPE_TEXTURE;}
inline SUBSTANCETAG substanceTagColorId(SUBSTANCETAG tag)      {return (tag & SUBSTANCETAG_REF_BITMASK);}
inline SUBSTANCETAG substanceTagTextureMemId(SUBSTANCETAG tag) {return (tag & SUBSTANCETAG_REF_BITMASK);} // this is the same but here for consistency.

// A shape bit is the number of each shape assigned as it is added to a column, each shape number corresponds to a bit in the shape stack
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
#define HEADER        ulong   // the threshold-header includes identifying information
#define THRESHOLD     SPACE4 // the stored version of a threshold encoded in floats

// HEADER base type 64 bit ulong
// Bits | 1 bit  | 1 bit      | 1 bit    | 1 bit    | 32 bit  | 28 bit
// Type | bool   | bool       | bool     |          | uint    | uint
// Desc | slope  | persistent | isFacet  | reserved | facetId | substanceId (if facet threshold)
//      | sign   |            |          |          |         | or layerId (if shape threshold)
// slope sign - determines the sign of the slope of the threshold.
// persitent - determines if the threshold intersects with the left side of the pixel.
// isFacet - determines if the threshold defines a shape border or a texture facet border
// facetId - determines which facet the threshold is a border for.
// substanceId - either determines which substance the theshold is a facet border for.
// layerId - determines which layer the threshold defines a shape border for.
#define POSITIVE_SLOPE_MASK    0x8000000000000000 // & to get the leftmost bit
#define PERSIST_AND_SLOPE_MASK 0xC000000000000000 // & to get leftmost and second to leftmost bit
#define PERSIST_TOP            0xC000000000000000 // positive slope and persist
#define PERSIST_BOTTOM         0x4000000000000000 // not positive slopw and persist

#define POSITIVE_SLOPE         0x8000000000000000
#define NEGATIVE_SLOPE         0x0000000000000000

#define PERSIST                0x4000000000000000 // & to isolate the persist bit
#define NONPERSIST             0x0000000000000000 // just zero
#define UNPERSISTMASK          0xBFFFFFFFFFFFFFFF // everything but the persist bit

#define FACETFLAGMASK          0x2000000000000000 // & to determine if the threshold changes active facet
#define FACETFLAGSET           0x2000000000000000 // or with this to create a header with the facet flag set
#define FACETFLAGNOTSET        0x0000000000000000 // or with this to create a header with the facet flag not set
#define WITHOUT_PAYLOAD        0xF000000000000000 // & to get control bits without the payload
#define JUST_PAYLOAD           0x0FFFFFFFFFFFFFFF // & to get right 60 bit payload

#define FACETIDMASK            0x0FFFFFFFF0000000 // & and shift to get the facets shapeId
#define NOFACET                0x0FFFFFFFF        // this value is reserved to indicate that a substance is outside of all facets.
#define FACETIDSHIFT           28               // amount to shift the shapeId
#define LAYERIDMASK            0x000000000FFFFFFF // & to get the layer id

// & has a lower precedence than !=
inline     bool headerPositiveSlope(HEADER h) {return (h & POSITIVE_SLOPE_MASK) != 0;} // determine if the threshold has a positive slope
inline     bool headerPersistTop   (HEADER h) {return (h & PERSIST_AND_SLOPE_MASK) == PERSIST_TOP;   } // determine if the top of the threshold affects the persistant state of the shapestack
inline     bool headerPersistBottom(HEADER h) {return (h & PERSIST_AND_SLOPE_MASK) == PERSIST_BOTTOM;} // determine if the bottom of the threshold affects the persistant state of the shapestack
inline     bool headerPersistEither(HEADER h) {return (h & PERSIST) != 0;  } // determine if either top or bottom of the threshold affects the persistant state of the shapestack
inline HEADER unPersist      (HEADER h) {return h & UNPERSISTMASK;}

inline    bool headerIsFacet(HEADER h) {return (h & FACETFLAGMASK) != 0;}
inline FACETID headerFacetId(HEADER h) {return (h & FACETIDMASK) >> FACETIDSHIFT;}

inline LAYERID headerLayerId(HEADER h) {return  h & LAYERIDMASK;} // get the index of the shape that corresponds to the threshold
inline LOCALSUBSTANCE headerLocalSubstance (HEADER h) {return  h & LAYERIDMASK;} // get the local substance index for the header (same as above just for consistency.)



#define VERTICALSLOPE      FLT_MAX // float value used to indicate vertical threshold slope.

inline THRESHOLD makeThreshold(SPACE top, SPACE bottom, SPACE left, SPACE right) {
  return (THRESHOLD)(top, bottom, left, right);
}

inline SPACE  tTop     (THRESHOLD t) {return t.s0;}
inline SPACE  tBottom  (THRESHOLD t) {return t.s1;}
inline SPACE  tLeft    (THRESHOLD t) {return t.s2;}
inline SPACE  tRight   (THRESHOLD t) {return t.s3;}
inline SPACE2 tStart   (HEADER h, THRESHOLD t) { return (SPACE2)(tLeft(t),headerPositiveSlope(h) ? tTop(t) : tBottom(t));}
inline SPACE2 tEnd     (HEADER h, THRESHOLD t) { return (SPACE2)(tRight(t),headerPositiveSlope(h) ? tBottom(t) : tTop(t));}
inline SPACE  tTopX    (HEADER h, THRESHOLD t) { return headerPositiveSlope(h) ? tLeft(t)  : tRight(t);}
inline SPACE  tBottomX (HEADER h, THRESHOLD t) { return headerPositiveSlope(h) ? tRight(t) : tLeft(t);}
inline SPACE  tHeight  (THRESHOLD t) {return tBottom(t) - tTop(t);}

inline bool tIsHorizontal (THRESHOLD t) {return tTop(t) == tBottom(t);}
inline bool tKeep(HEADER h, THRESHOLD t) { return (headerPersistEither(h)) || (tHeight(t) >= MINCROP);}

inline THRESHOLD setTop   (THRESHOLD t, SPACE value) {t.s0 = value; return t;}
inline THRESHOLD setBottom(THRESHOLD t, SPACE value) {t.s1 = value; return t;}
inline THRESHOLD setLeft  (THRESHOLD t, SPACE value) {t.s2 = value; return t;}
inline THRESHOLD setRight (THRESHOLD t, SPACE value) {t.s3 = value; return t;}

inline float thresholdInvertedSlope ( HEADER header
                                    , THRESHOLD t
                                    ) {
    float slopeSign = headerPositiveSlope(header) ? 1 : -1;
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

inline int getGeometryStart(Slice ref) {return ref.sStart;}  // The first part of a shape slice refers to the position in the geometry buffer
inline int getNumStrands(Slice ref)    {return ref.sLength;} // The second part refers to the number of strands that define the shape
                                                              // (the actual size of the strands is at the beginning of each strand heap)
inline int boxLeft  (int4 box) {return box.x;}
inline int boxTop   (int4 box) {return box.y;}
inline int boxRight (int4 box) {return box.z;}
inline int boxBottom(int4 box) {return box.w;}
inline int2 boxLeftTop     (int4 box) {return box.xy;}
inline int2 boxRightBottom (int4 box) {return box.zw;}

// Initial information about a tile.
typedef struct TileInfo
  { int4  tileBox    // boundaries of tile
  ; short tileHDepth // logarithmic horizontal depth in tree (tileHDepth ^ 2 == tile width)
  ; short tileVDepth // logarithmic vertical   depth in tree (tileVDepth ^ 2 == tile height)
  ; int   tileColumnAllocation // unique number of the column in the entire image
  ; Slice tileShapeSlice // beggining and length of shape records for the tile.
  ;} TileInfo;

// A substance contains information about the substance of a group of combined shapes.
typedef struct Substance
  { COLOR  substanceColor; // this is either the solid color of the shape or a reference to a picture ref.
  } Substance;

// A picture reference is a reference to bitmap data that can be the substance of a shape.
typedef struct PictUse
  {   int2 pictSize;      // size of the bitmap
       int pictMemOffset; // starting point of the pixel data in the memory buffer
  } PictUse;

// A hard facet pointing to a specific picture.
typedef struct HardFacet
  { float2 facetP0;
    float2 facetP1;
    float2 facetP2;
    float2 facetT0;
    float2 facetT1;
    float2 facetT2;
  } HardFacet;

// The color state structure tracks the current color information during a scan through thresholds
typedef struct ColorState {
               COLOR   csBackgroundColor;         // background color
     GMEM      uchar  *csPictureData;             // global image information
     GMEM    PictUse  *csPictureRefs;             // global list of image references
     GMEM    COLOR    *csSolidColors;               // global list of solidColors
                int2   absolutePosition;          // the absolute position of the current pixel.
  } ColorState;

#define RENDERSTART 0
#define RENDEREND tileS->floatHeight

// the threshold queue stores references to the threshold buffers pointers to the start and end of the queue.
typedef struct ThresholdQueue {
               GMEM    HEADER *thresholdHeaders; // array of threshold header
               GMEM THRESHOLD *thresholds;       // array of threshold geometry
                        Slice  qSlice;       // slice, the position of the top of the queue.
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

inline HEADER getHeader(ThresholdQueue *tQ, int index) {
    return tQ->thresholdHeaders[tSLocation(tQ,index)];
}

inline void setHeader(ThresholdQueue *tQ, int index, HEADER set) {
    tQ->thresholdHeaders[tSLocation(tQ,index)] = set;
}

inline void pushTopSlot(ThresholdQueue *tQ) {
    tQ->qSlice.sStart = cycleLocation(tQ->qSlice.sStart - 1);
    tQ->qSlice.sLength += 1;
}

inline void popTop(ThresholdQueue *tQ) {
  tQ->qSlice.sStart = cycleLocation(tQ->qSlice.sStart + 1);
  tQ->qSlice.sLength -= 1;
}

#define RANDOMFIELDMASK RANDOMFIELDSIZE - 1

#define RANDOM_POS int

// LAYERENTRY base type 16 bit ushort
#define LAYERENTRY ushort
// LAYERENTRY bit layout
// Bits | 1 bit  | 1 bit      | 14 bit
// Type | bool   | bool       | uint
// Desc | active | additive   | local substance
//      |        |            |
// active - this bit changes during parsing and determines whether the layer is currently inside a shape.
// additive - this bit determines whether the layer is additive or subtractive and does not change during parsing.
// local substance - these bits determine which substance in the substanceTagStack the layer references.

#define LAYERACTIVEMASK            0x8000
#define LAYERISACTIVE              0x8000
#define LAYERNOTACTIVEBITMASK      0x7FFF
#define LAYERADDITIVEMASK          0x4000
#define LAYERISADDITIVE            0x4000
#define LAYERISSUBTRACTIVE         0x0000
#define LAYERLOCALSUBSTANCEBITMASK 0x3FFF

inline bool layerEntryIsActive (LAYERENTRY entry) {return (entry & LAYERACTIVEMASK) == LAYERISACTIVE;}
inline LAYERENTRY layerEntrySetActive(LAYERENTRY entry) {return (entry | LAYERISACTIVE);}
inline LAYERENTRY layerEntrySetNotActive(LAYERENTRY entry) {return (entry & LAYERNOTACTIVEBITMASK);}
// inline LAYERENTRY layerEntryToggleActive(LAYERENTRY entry) {return (((~entry) & LAYERACTIVEMASK)) | (entry & LAYERNOTACTIVEBITMASK);}
inline LAYERENTRY layerEntryToggleActive(LAYERENTRY entry) {return (entry ^ LAYERACTIVEMASK);}
inline bool layerEntryIsAdditive(LAYERENTRY entry) {return ((entry & LAYERADDITIVEMASK) == LAYERISADDITIVE);}
inline bool layerEntryIsSubtractive(LAYERENTRY entry) {return ((entry & LAYERADDITIVEMASK) == LAYERISSUBTRACTIVE);}
inline LOCALSUBSTANCE layerEntryLocalSubstance(LAYERENTRY entry) {return (LOCALSUBSTANCE) (entry & LAYERLOCALSUBSTANCEBITMASK);}
inline LAYERENTRY createLayerEntry(bool isAdditive, LOCALSUBSTANCE substance) {
  LAYERENTRY additiveBit = isAdditive ? LAYERISADDITIVE : LAYERISSUBTRACTIVE;
  return additiveBit | ((LAYERENTRY)substance);
  }

typedef struct ShapeState {
            LAYERID layerCount;                      // the number of shapes and thus layers added to the column.
         LAYERENTRY layerStack[MAXLAYERS];            // a stack of layer entries
     LOCALSUBSTANCE substanceCount;                  // the number of substances that have been added to the tile.
       SUBSTANCETAG substanceTagStack[MAXLAYERS];     // a stack of all substances in the column
            FACETID substanceActiveFacet[MAXLAYERS];  // a mapping each substance to the active facet of that texture or -1 for no facet
} ShapeState;

inline void clearLayerFlags(LAYERENTRY *stack, int numLayers) {
    for (int i = 0; i < numLayers; i++) {
        stack[i] = layerEntrySetNotActive(stack[i]);
    }
}

inline LAYERID findTopLayer(ShapeState *shS, LAYERID ignoreAbove) {
   LAYERID i = ignoreAbove;
   while (i < shS->layerCount && (!layerEntryIsActive(shS->layerStack[i]))) {
     i++;
   }
   return i;
}

inline void toggleLayerActive(ShapeState *shS, int layer) {
  shS->layerStack[layer] = layerEntryToggleActive(shS->layerStack[layer]);
}

inline LOCALSUBSTANCE layerSubstance(ShapeState *shS, int layer) {
  return layerEntryLocalSubstance(shS->layerStack[layer]);
}

inline bool layerIsAdditive(ShapeState *shS, LAYERID layer) {
  return layerEntryIsAdditive(shS->layerStack[layer]);
}

typedef struct ParseState {
               int  currentThreshold;   // the current threshold bordering the section
               int  numActive;          // the next threshold that is not currently active.
            SPACE2  sectionStart;       // the top of the current vertical section being processed
            SPACE2  sectionEnd;         // the bottom of the current vertical section being processed
             float  pixelY;             // the bottom of the current pixel.
            float8  accColorArea;       // accumulated color and area.

               int  sectionCount;
               int  frameNumber;
               int  buildCount;
        RANDOM_POS  randomFieldCursor;
    CMEM     float *randomField;
  } ParseState;

typedef struct TileState {
         ITEMTAGID  tileItemStart;
               int  tileNumItems;
               int  threadId;
              int2  tileSize;
              int2  bitmapSize;
              int2  internalDelta;
              int2  threadDelta;
               int  tileIndex;
               int  intHeight;
             float  floatHeight;
               int  threadUnique;
               int  column;
} TileState;

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
                    , PMEM       TileState *tileS
                    ,               float2  left
                    ,               float2  right
                    ,             LAYERID  layer
                    ,                  int  addType
                    ,                 bool *thresholdWasAdded
                    ,                 bool *enclosedByStrand
                    );

void addThreshold ( PMEM ThresholdQueue *tQ
                  , PMEM      TileState *tileS
                  ,              HEADER  newHeader
                  ,           THRESHOLD  newThreshold
                  ,                 int  addType
                  ,                bool *thresholdWasAdded
                  ,                bool *enclosedByStrand
                  );

bool traverseTree ( GMEM    float2 *strandHeap
                  ,            int  currentSize
                  ,          float2 threadDelta
                  ,      Traversal *l
                  ,      Traversal *r
                  );

void searchTree(      Traversal *trav
               , GMEM    float4 *tree
               ,            int  treeSize
               ,         float4  threadDelta4
               ,           bool  isLeft
               );

void spawnThresholds ( PMEM  ThresholdQueue *tQ
                     , PMEM       TileState *tileS
                     ,              LAYERID  layer
                     ,            Traversal *l
                     ,            Traversal *r
                     ,                 bool *thresholdWasAdded
                     ,                 bool *enclosedByStrand
                     );

bool checkInRange ( Traversal *t
                  );

inline THRESHOLD lineToThreshold ( float2  left
                                 , float2  right
                                 );

inline HEADER lineToHeader ( float2 left
                           , float2 right
                           , bool isFacet
                           , LAYERID shapeId
                           , SHAPEID facetId
                           );

float thresholdIntersectX(    HEADER header
                         , THRESHOLD threshold
                         ,     float y
                         );

SPACE thresholdMidXLow( THRESHOLD t
                      ,    HEADER h
                      ,     SPACE yTop
                      ,     SPACE yBottom
                      ,     SPACE clampHigh
                      ,     SPACE clampLow
                      );

inline void divideThreshold( PMEM     HEADER *headerTop
                           , PMEM  THRESHOLD *thresholdTop
                           , PMEM     HEADER *headerBottom
                           , PMEM  THRESHOLD *thresholdBottom
                           ,           SPACE  splitX
                           ,           SPACE  splitY
                           );

inline void splitThreshold( PMEM     HEADER *topHeader
                          , PMEM  THRESHOLD *top
                          , PMEM     HEADER *bottomHeader
                          , PMEM  THRESHOLD *bottom
                          , PMEM      SPACE  splitY
                          );

void trimThresholdTop( PMEM    HEADER *header
                     , PMEM THRESHOLD *threshold
                     ,          SPACE  splitY
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

inline bool thresholdIsAbove( HEADER newHeader
                            , THRESHOLD new
                            , HEADER oldHeader
                            , THRESHOLD old);

bool isAboveLast( PMEM ThresholdQueue *tQ
                  ,            HEADER  newHeader
                  ,         THRESHOLD  new
                  );

void pushThreshold( PMEM ThresholdQueue *tQ
                  ,                   HEADER  newHeader
                  ,                THRESHOLD  new
                  );

void insertThreshold( PMEM ThresholdQueue *tQ
                    ,              HEADER  newHeader
                    ,           THRESHOLD  new
                    );

inline void passHeader( PMEM ShapeState *shS
                      ,          HEADER  thresholdHeader
                      );

inline void passHeaderTop( PMEM ShapeState *shS
                         ,          HEADER  header
                         );

inline void passHeaderBottom( PMEM ShapeState *shS
                            ,          HEADER  header
                            );

COLOR readColor ( PMEM ColorState *cS
                , SUBSTANCETAG tag
                );

COLOR compositeLayers( PMEM    ShapeState *shS
                     , PMEM    ColorState *cS
                     );

void verticalAdvance( PMEM ThresholdQueue *tQ
                    , PMEM      TileState *tileS
                    , PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    );

void horizontalAdvance ( PMEM ThresholdQueue *tQ
                       , PMEM     ParseState *pS
                       );

void writePixelGlobal ( PMEM TileState *tileS
                      ,          COLOR  color
                      , GMEM       uint *out
                      ,            int  y
                      );

void buildThresholdArray ( PMEM       TileState *tileS
                         , PMEM  ThresholdQueue *tQ
                         , PMEM      ShapeState *shS
                         , GMEM          float4 *geometryHeap
                         , GMEM       HardFacet *facetHeap
                         , GMEM           Slice *geoRefHeap
                         , GMEM         ITEMTAG *itemTagHeap
                         , GMEM    SUBSTANCETAG *substanceTagHeap
                         ,            ITEMTAGID  itemStart
                         ,            ITEMTAGID  numItems
                         ,               float2  threadDelta
                         );

void initRandomField( ParseState *pS
                    , TileState *tileS
                    , CMEM float *randomField
                    );

float getRandom(ParseState *pS);

inline Slice initQueueSlice();

void initThresholdQueue( PMEM ThresholdQueue  *tQ
                       , PMEM       TileState *tileS
                       , GMEM      THRESHOLD  *thresholdHeap
                       , GMEM         HEADER  *headerHeap
                       ,               Slice   qSlice
                       );

void initShapeState (PMEM    ShapeState *shS
                    );

ShapeState loadShapeState( GMEM ShapeState  *shapeStateHeap
                         , PMEM  TileState  *tileS
                         );

void storeShapeState( GMEM ShapeState  *shapeStateHeap
                    , PMEM ShapeState  *shS
                    , PMEM  TileState  *tileS
                    );

Slice loadQueueSlice( GMEM     Slice  *qSliceHeap
                    , PMEM TileState  *tileS
                    );

void storeQueueSlice( GMEM     Slice  *qSliceHeap
                    ,          Slice   qSlice
                    , PMEM TileState  *tileS
                    );

void initParseState ( PMEM ParseState *pS
                    , PMEM  TileState *tileS
                    ,             int  frameNumber
                    , CMEM      float *randomField
                    );

GMEM TileInfo *getTileInfo ( GMEM  TileInfo *tileHeap
                           ,  int  tileIndex
                           );

void initTileState ( PMEM  TileState *tileS
                   , GMEM   TileInfo *tileInfo
                   ,            int2  bitmapSize
                   ,             int  column
                   ,             int  jobIndex
                   ,             int  computeDepth
                   );

bool isActiveThread ( PMEM TileState *tileS);

float8 sectionColor ( PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    , PMEM     ColorState *cS
                    );

void calculatePixel ( PMEM      TileState *tileS
                    , PMEM ThresholdQueue *tQ
                    , PMEM     ShapeState *shS
                    , PMEM     ParseState *pS
                    , PMEM     ColorState *cS
                    );


bool swapIfAbove( PMEM ThresholdQueue *tQ
                ,                 int  i
                ,                bool  done
                );

void sortThresholdArray ( PMEM  ThresholdQueue *tQ);

void renderThresholdArray ( PMEM       TileState *tileS
                          , PMEM  ThresholdQueue *tQ
                          , PMEM      ShapeState *shS
                          , GMEM       HardFacet *facetHeap
                          , GMEM           uchar *pictureData
                          , GMEM         PictUse *pictureRefs
                          , GMEM           COLOR *solidColors
                          , CMEM           float *randomField
                          ,                COLOR  backgroundColor
                          ,                  int  frameNumber
                          , GMEM            uint *out
                          );

void initColorState ( PMEM  ColorState *init
                    ,            COLOR  backgroundColor
                    , GMEM       uchar *pictureData
                    , GMEM     PictUse *pictureRefs
                    , GMEM       COLOR *solidColors
                    ,             int2  absolutePosition
                    );


// Debug Functions

void showShapeState( ShapeState *shS);
void showLayerEntry(LAYERENTRY entry);
void showSubstanceTag(SUBSTANCETAG tag);
void showItemTag(ITEMTAG tag);

void showThresholdHeader( HEADER header);
void showThresholdGeo (THRESHOLD threshold);
void showThreshold( HEADER header,
                    THRESHOLD threshold);
void showThresholds (PMEM ThresholdQueue *tQ);
void tileStateHs (TileState tileS);

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
  //if (alphaOut > 0) {
     COLOR color = ((foreground * ALPHA(foreground)) + (background * ALPHA(background) * (1.0f - ALPHA(foreground)))) / alphaOut;
     return (COLOR) (RED(color),GREEN(color),BLUE(color),alphaOut);
  //}
  //else {
  //  return TRANSPARENT_COLOR;
  //}
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
float thresholdIntersectX( HEADER header
                         , THRESHOLD threshold
                         ,         float y
                         ) {
    if (isVerticalThreshold(threshold)) {
        //DEBUG_IF(printf("isVertical\n");)
        return tLeft(threshold);
    }
    else {
        return xInterceptInvertedSlope( tStart(header, threshold)
                                      , thresholdInvertedSlope(header, threshold)
                                      , y );
    }
}

// the x position of the midpoint of a threshold within a segment
SPACE thresholdMidXLow( THRESHOLD t
                     , HEADER h
                     , SPACE yTop
                     , SPACE yBottom
                     , SPACE clampLow
                     , SPACE clampHigh
                     ) {
    float yMid = yTop + ((yBottom - yTop) * 0.5f);
    float x = thresholdIntersectX(h,t,yMid);
    return x >= clampHigh ? clampLow : max(clampLow, x); // like clamp but defaults to low value
}

inline void divideThreshold( PMEM     HEADER *headerTop
                           , PMEM  THRESHOLD *thresholdTop
                           , PMEM     HEADER *headerBottom
                           , PMEM  THRESHOLD *thresholdBottom
                           ,                 SPACE  splitX
                           ,                 SPACE  splitY
                           ) {
    if (headerPositiveSlope(*headerTop)) {
        /* *      *
            \      \
             \      *
              \      \
               *      * */
        *thresholdBottom = makeThreshold(splitY, tBottom(*thresholdTop), splitX, tRight(*thresholdTop));
        *thresholdTop    = makeThreshold(tTop(*thresholdTop), splitY, tLeft(*thresholdTop), splitX);
        // headerTop is unchanged.
        *headerBottom    = unPersist(*headerTop);
    }
    else {
        /*     *     *
              /     /
             /     *
            /     /
           *     *     */
        *thresholdBottom = makeThreshold(splitY, tBottom(*thresholdTop), tLeft(*thresholdTop), splitX);
        *thresholdTop    = makeThreshold(tTop(*thresholdTop), splitY, splitX, tRight(*thresholdTop));
        *headerBottom    = *headerTop;
        *headerTop       = unPersist(*headerTop);
    }
}

inline void splitThreshold(  PMEM     HEADER *topHeader
                           , PMEM  THRESHOLD *top
                           , PMEM     HEADER *bottomHeader
                           , PMEM  THRESHOLD *bottom
                           , PMEM           SPACE   splitY
                           ) {
    SPACE splitX = thresholdIntersectX(*topHeader, *top, splitY);
    divideThreshold( topHeader
                   , top
                   , bottomHeader
                   , bottom
                   , splitX
                   , splitY
                   );
    //DEBUG_IF(printf("splitThreshold\n    ");\
    //         showThreshold(*topHeader, *top);\
    //         printf("\n    ");\
    //         showThreshold(*bottomHeader,*bottom);\
    //         printf("\n");\
    //        )
}


void trimThresholdTop( PMEM        HEADER *header
                     , PMEM     THRESHOLD *threshold
                     ,                    SPACE  splitY
                     ) {
    SPACE splitX = thresholdIntersectX(*header, *threshold, splitY);
    if (headerPositiveSlope(*header)) {
        /* *
            \
             \      *
              \      \
               *      * */
        *threshold= makeThreshold(splitY, tBottom(*threshold), splitX, tRight(*threshold));
        // headerTop is unchanged.
        *header    = unPersist(*header);
    }
    else {
        /*     *
              /
             /     *
            /     /
           *     *     */
        *threshold = makeThreshold(splitY, tBottom(*threshold), tLeft(*threshold), splitX);
    }
}

int countActive ( PMEM ThresholdQueue *tQ
                ,               float *nextTop
                ) {
    float top = tTop(getThreshold(tQ, 0));
    bool notDone = true;
    int  numActive = 1;
    while (notDone && numActive < tQ->qSlice.sLength) {
        THRESHOLD next = getThreshold(tQ, numActive);
        if (tTop(next) > top) {
            notDone = false;
            *nextTop = tTop(next);
        }
        else {
            numActive += 1;
        }
    }
    return numActive;
}

float nextSlicePoint ( PMEM ThresholdQueue *tQ
                     ,               float  slicePoint
                     ,                 int  numActive
                     ) {
    float top = tTop(getThreshold(tQ, 0));
    for (int i = 0; i < numActive; i++) {
        float bottom = tBottom(getThreshold(tQ, i));
        if (top < bottom) { // it's not a horizontal threshold
            slicePoint = min(slicePoint, bottom);
        }
    }
    return slicePoint;
}

void sliceActive( PMEM ThresholdQueue *tQ
                ,               float  slicePoint
                ,                 int  numActive
                ) {
    for (int cursor = 0; cursor < numActive; cursor++) {
        HEADER currentHeader = getHeader(tQ, cursor);
        THRESHOLD current = getThreshold(tQ, cursor);
        //DEBUG_IF(printf("cursor %i slicePoint %f ",cursor, slicePoint);showThreshold(currentHeader,current);printf("\n");)
        if (tTop(current) < slicePoint && slicePoint < tBottom(current)) {
            HEADER splitHeader;
            THRESHOLD split;
            splitThreshold( &currentHeader
                          , &current
                          , &splitHeader
                          , &split
                          ,  slicePoint
                          );
            //DEBUG_IF(printf("               current ");showThreshold(currentHeader,current);printf("\n");)
            //DEBUG_IF(printf("               split   ");showThreshold(splitHeader,split);printf("\n");)
            setHeader(tQ, cursor, currentHeader);
            setThreshold(tQ, cursor, current);
            if (tKeep(splitHeader,split)) {
                insertThreshold(tQ, splitHeader, split);
                //DEBUG_IF(printf("split and insert\n");showThresholds(tQ);)
            }
        }
    }
}

float splitNext( PMEM ThresholdQueue *tQ
               , PMEM     ParseState *pS
               ) {
    float slicePoint = FLT_MAX;
    pS->numActive = countActive(tQ, &slicePoint);
    slicePoint = min(slicePoint, nextSlicePoint(tQ, slicePoint, pS->numActive));
    sliceActive(tQ, slicePoint, pS->numActive);
    return slicePoint;
}

inline bool thresholdIsBelow( HEADER aHeader
                            , THRESHOLD a
                            , HEADER bHeader
                            , THRESHOLD b) {
    return (tTop(a) > tTop(b)) ||
                (
                    (tTop(a) == tTop(b)) &&
                        (
                            (tTopX(aHeader, a) > tTopX(bHeader, b)) ||
                            (
                                (tTopX(aHeader, a) == tTopX(bHeader, b)) &&
                                (thresholdInvertedSlope(aHeader, a) > thresholdInvertedSlope(bHeader, b))
                            )
                        )
                );
}

void pushThreshold( PMEM ThresholdQueue *tQ
                  ,                   HEADER  newHeader
                  ,                THRESHOLD  new
                  ) {
    pushTopSlot(tQ);
    setHeader(tQ,0,newHeader);
    setThreshold(tQ,0,new);
}

void insertThreshold( PMEM ThresholdQueue *tQ
                    ,                   HEADER  newHeader
                    ,                THRESHOLD  new
                    ) {
    pushTopSlot(tQ);
    int cursor = 0;
    bool isBelow = true;
    while (cursor < (tQ->qSlice.sLength - 1) && isBelow) {
        HEADER oldHeader = getHeader(tQ, cursor + 1);
        THRESHOLD old = getThreshold(tQ, cursor + 1);
        isBelow = thresholdIsBelow(newHeader, new, oldHeader, old);
        if (isBelow) {
            setHeader(tQ, cursor, oldHeader);
            setThreshold(tQ, cursor, old);
            cursor += 1;
        }
    }
    setHeader(tQ, cursor, newHeader);
    setThreshold(tQ, cursor, new);
}

inline bool full(ThresholdQueue *tQ) {
  return tQ->qSlice.sLength >= MAXTHRESHOLDS;
}

// create a threshold from a larger line segment and identifying information.
inline THRESHOLD lineToThreshold ( float2  left
                                 , float2  right
                                 ) {
    float tTop    = fmin(left.y,right.y);
    float tBottom = fmax(left.y,right.y);
    return makeThreshold(tTop, tBottom, left.x, right.x);
}

inline HEADER defaultShapeHeader(LAYERID layerId) {
  return (HEADER)layerId;
}

inline HEADER lineToHeader ( float2 left
                           , float2 right
                           , bool isFacet
                           , LAYERID layerId
                           , SHAPEID facetId
                           ) {
    bool  positiveSlope = left.y <= right.y;
    bool  notVertical = left.x != right.x;
    bool  touchingLeftBorder = left.x == LEFTBORDER;
    bool  isPersistent = notVertical && touchingLeftBorder;
    HEADER slopeBit = positiveSlope ? POSITIVE_SLOPE : NEGATIVE_SLOPE;
    HEADER persistantBit = isPersistent ? PERSIST : NONPERSIST;
    HEADER facetBit = isFacet ? FACETFLAGSET : FACETFLAGNOTSET;
    HEADER facetIdBits = ((HEADER) facetId) << FACETIDSHIFT;
    return slopeBit | persistantBit | facetBit | facetIdBits | (HEADER) layerId;
}

inline HEADER lineToFacetHeader ( float2 left
                                , float2 right
                                , FACETID facetId
                                , LAYERID layer
                                ) {
    return lineToHeader(left,right,true,layer,facetId);
}

inline HEADER lineToShapeHeader ( LAYERID layer
                                ,  float2 left
                                ,  float2 right
                                ) {
    return lineToHeader (left, right,false,layer,0);
}

// determine if a threshold should be added to the stack,
// if it's header information should be pre-parsed (because it's above the render area)
// or if it should be ignored (below the render area, or a horizontal threshold that can be bypassed)
void addLineSegment ( PMEM  ThresholdQueue *tQ
                    , PMEM       TileState *tileS
                    ,               float2  left
                    ,               float2  right
                    ,              LAYERID  layer
                    ,                  int  addType
                    ,                 bool *thresholdWasAdded
                    ,                 bool *enclosedByStrand
                    ) {
    //DEBUG_IF(printf("--------------- addLineSegment %i left: %v2f right: %v2f addType: %i\n", tS->addThresholdCount, left, right, addType);)
    THRESHOLD newThreshold = lineToThreshold( left
                                            , right
                                            );
    HEADER newHeader = lineToShapeHeader( layer
                                        , left
                                        , right
                                        );
    addThreshold(tQ, tileS, newHeader, newThreshold, addType, thresholdWasAdded, enclosedByStrand);
}

void addThreshold ( PMEM  ThresholdQueue *tQ
                  , PMEM       TileState *tileS
                  ,               HEADER  newHeader
                  ,            THRESHOLD  newThreshold
                  ,                  int  addType
                  ,                 bool *thresholdWasAdded
                  ,                 bool *enclosedByStrand
                  ) {
    //DEBUG_IF(printf("beg add enclosed%i add %i ", *enclosedByStrand, addType);showThreshold(newHeader, newThreshold);printf("\n");)
    //DEBUG_IF(printf("original ");showThreshold(newHeader, newThreshold);printf("\n");)
    // in the beggining the slot at position numThresholds is free, we are either at the end of the list or just picked up the top
    // threshold from the holding queue
    //DEBUG_IF(printf("addThreshold  ");showThreshold(newHeader, newThreshold);)
    if (tKeep(newHeader, newThreshold)) {
        // horizontal thresholds that have no persistance can be ignored.
        *enclosedByStrand = *enclosedByStrand ||
                             ((tTop(newThreshold)    <= RENDERSTART) && headerPersistTop(newHeader)   ) ||
                             ((tBottom(newThreshold) <= RENDERSTART) && headerPersistBottom(newHeader));
        //DEBUG_IF(printf("mid add enclosed %i add %i ", *enclosedByStrand, addType);showThreshold(newHeader, newThreshold);printf("\n");)
        if ((tTop(newThreshold) < RENDEREND) && (tBottom(newThreshold) > RENDERSTART) && tLeft(newThreshold) < RIGHTBORDER) {
            if (tTop(newThreshold) <= RENDERSTART) {
                trimThresholdTop(&newHeader,&newThreshold, RENDERSTART);
            }
            if (tRight(newThreshold) <= LEFTBORDER) {
                *enclosedByStrand = true;
                //DEBUG_IF(printf(" LEFT");)
            }
            else {
                // if the threshold is entirely below the bottom of the render area is can be ignored
                // otherwise add it to the threshold array, and see if the bottom of the render area needs to be adjusted because the threshold array is not
                // large enough.g
                //DEBUG_IF(printf("  ADD");)
                *thresholdWasAdded = true;
                //DEBUG_IF(printf("************** %016lx",newHeader);)
                pushThreshold(tQ, newHeader, newThreshold);
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
                     , PMEM       TileState *tileS
                     ,              LAYERID  layer
                     ,            Traversal *l
                     ,            Traversal *r
                     ,                 bool *thresholdWasAdded
                     ,                 bool *enclosedByStrand
                     ) {
    float y_L; // this is the y value
    //DEBUG_IF(printf("spawnThresholds len %i count %i \n", tQ->qSlice.sLength);)
    if (l->travLeftX >= LEFTBORDER) { // if the left side is the left edge of the strand, don't bifurcate.
      y_L = l->travLeftY;
    }
    else { // bifurcate the curve to find the left side of the segment
      y_L = intersectCurve(*l);
    }
    bool leftWing;
    if ((l->travRightX < RIGHTBORDER) && (l->travRightX > LEFTBORDER)) {
        // add the threshold to the current state or buffer.
        addLineSegment (  tQ
                       ,  tileS
                       ,  (float2) (l->travXPos, y_L)
                       ,  l->travRight
                       ,  layer
                       ,  0
                       ,  thresholdWasAdded
                       ,  enclosedByStrand
                       );
        leftWing = true;
    }
    else {
        leftWing = false;
    }
    float y_R;
    if (r->travRightX <= RIGHTBORDER) { // if the right side is the right side of the strand, don't bifurcate.
      y_R = r->travRightY;
    }
    else { // bifurcate the curve to find the right side of the center segment
      y_R = intersectCurve(*r);
    }
    bool rightWing;
    if ((r->travLeftX > LEFTBORDER) && (r->travLeftX < RIGHTBORDER) && (l->travIndex != r->travIndex)) {
        addLineSegment (  tQ
                       ,  tileS
                       ,  r->travLeft
                       ,  (float2) (r->travXPos, y_R)
                       ,  layer
                       ,  1
                       ,  thresholdWasAdded
                       ,  enclosedByStrand
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
                       ,  tileS
                       ,  bridge_L
                       ,  bridge_R
                       ,  layer
                       ,  2
                       ,  thresholdWasAdded
                       ,  enclosedByStrand
                       );
    }
}

// move to the next position in the tree based on the relationship between an XPosition and the current
// node in the tree. (test is either <= or < depending on if the search is biased to the left or the right.
void searchTree( Traversal *trav
               , GMEM float4 *tree
               , int treeSize
               , float4 threadDelta4
               , bool isLeft
               ){
    trav->travIndex = 0;
    while (trav->travIndex < treeSize) {
        float4 currentTree = tree[trav->travIndex] - threadDelta4;
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

// check if a section of a strand crosses a column
bool checkInRange ( Traversal *t
                  ) {
    return (t->travLeftX <= RIGHTBORDER && t->travRightX > LEFTBORDER);
}

// A strand is a list of on curve and off curve coordinates pairs rearranged in an order that allows easy access to the left and right boundaries of the strand
// followed by a complete-binary-tree of the internal coordinate pairs.
// The order is designed to facilitate as many 64bit memory accesses as possible and determine the horizontal range of the curve with the minumum loads
// Because of this there are a lot of unused bits in the first 64 bits.
// Size refers to the number of 64 bits chunks that the loading function must skip ahead to get to the next tree so it includes the header.
// In memory the format the header should appear this way:
// | 64 bit                            | 64 bit            | 64 bit          | 64 bit                | 64 bit            | ...
// | 16 bit   | 8 bit   | 8 bit        | 32 bit  | 32 bit  | 32 bit | 32bit  | 32 bit    | 32bit     | 32 bit  | 32 bit  | 32 bit          | 32bit           | ...
// | ushort   | uchar   | uchar        | float   | float   | float  | float  | float     | float     | float   | float   | float           | float           | ...
// | size + 1 | packing | continuation | right.x | right.y | left.x | left.y | control.x | control.y | tree0.x | tree0.y | tree0 control.x | tree0 control.y | ...
// |                                   | ending curve point| leftmost curve point                    | complete binary tree of point control point pairs     | rest of tree
// | strandHeap + 0                    | strandHeap + 1    | strandHeap + 2                          | strandHeap + 4                                        | strandHeap + 6

bool traverseTree( GMEM    float2 *strandHeap
                 ,            int  currentSize
                 ,         float2  threadDelta
                 ,      Traversal *l
                 ,      Traversal *r
                 ) {
    int    treeSize    =  (currentSize - 4) / 2; // take the total size in 64 bit parts and subtract the header and the left and right points. Divide by 2 becasue each left+control is 128 bytes
    float4 threadDelta4  =  (float4)(threadDelta, threadDelta); // create a double vector that can be used to offset reads of two simultaneous reads
    l->travRight       = *((GMEM float2 *)(strandHeap + 1)) - threadDelta; // load the rightmost point of the strand.
    l->travLeftControl = *((GMEM float4 *)(strandHeap + 2)) - threadDelta4; // load the leftmost point of the strand and it's control point.
    GMEM float4 *tree =   (GMEM float4 *)(strandHeap + 4); // Move to tree portion of strand array.
    //DEBUG_IF(printf("l->travLeftX: %f l->travRightX: %f \n", l->travLeftX, l->travRightX);)
    bool inRange = checkInRange(l);
    if (inRange) { // determine if the strand interacts at all with this column.
        //DEBUG_IF(printf("--TREE--\n");showTree(*((float4 *)(strandHeap + 2)), *((float2 *)(strandHeap + 1)), tree, treeSize);)
        // now execute two tree traversals for the left and right borders of the pixel.
        *r = *l;
        // if the left side of the strand is within the pixel, treat the left side border as the tree traversal axis
        // otherwise treat the left border of the pixel as the axis.
        l->travXPos = max(LEFTBORDER, l->travLeftX);
        // do the same for the right side.
        r->travXPos = min(RIGHTBORDER, l->travRightX);
        searchTree(l, tree, treeSize, threadDelta4, true);  // traverse the tree biased to the left
        searchTree(r, tree, treeSize, threadDelta4, false); // traverse the tree biased to the right
        //DEBUG_IF(printf("i_L %i i_R %i\n", i_L, i_R);)
        // build up to three thresholds based on the tree traversals
    }
    return inRange;
}

// read a color value depending on the substance and absolute position.
COLOR readColor ( PMEM ColorState *cS
                , SUBSTANCETAG tag
                ) {
    if (substanceTagIsSolidColor(tag)) {
        return cS->csSolidColors[substanceTagColorId(tag)];
    } else { // its a picture reference
        uint pictId = substanceTagTextureMemId(tag);
        PictUse pRef = cS->csPictureRefs[pictId];
        float scale = 1.0f;//pRef.pictScale;
        scale = scale < 0.0000001 ? 0.0000001 : scale;
        //DEBUG_IF(printf("scale %f \n", scale);)
        int2 relativePosition = convert_int2((convert_float2(cS->absolutePosition) / scale) /* - (pRef.pictTranslate*/);
        //DEBUG_IF(printf("pictId %i pRef.pictSize %v2i pRef.memOffset %i relativePosition %v2i \n", pictId, pRef.pictSize, pRef.pictMemOffset, relativePosition);)
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
}

#define NULLINDEX 0xFFFFFFFF

// determine the current color based on the layer stack
COLOR compositeLayers( PMEM    ShapeState *shS
                     , PMEM    ColorState *cS
                     ) {
    COLOR color = TRANSPARENT_COLOR;
    LAYERID topLayer = findTopLayer(shS,0);
    bool subtractionOnOrAbove = false;
    LOCALSUBSTANCE lastSubstance = NULLINDEX;
    while (topLayer < shS->layerCount && !(OPAQUE(color))) {
        LAYERENTRY currentLayer = shS->layerStack[topLayer];
        LOCALSUBSTANCE currentSubstance = layerEntryLocalSubstance(currentLayer);
        bool isSubtractive = layerEntryIsSubtractive(currentLayer);
        subtractionOnOrAbove =  ((currentSubstance == lastSubstance) && (subtractionOnOrAbove || isSubtractive))
                             || ((currentSubstance != lastSubstance) && (!isSubtractive));
        float multiplier = subtractionOnOrAbove ? 0.0f : 1.0f;
        SUBSTANCETAG substanceTag = shS->substanceTagStack[currentSubstance];
        COLOR nextColor = readColor ( cS
                                    , substanceTag
                                    );
        color = composite(color, nextColor); // * multiplier);
        topLayer = findTopLayer(shS,topLayer+1);
        lastSubstance = currentSubstance;
    }
    color = composite(color, cS->csBackgroundColor);
    return color;
}

inline void passHeader( PMEM ShapeState *shS
                      ,          HEADER  header
                      ) {
    if (headerIsFacet(header)) {
      LOCALSUBSTANCE currentSubstance = headerLocalSubstance(header);
      FACETID facetId = headerFacetId(header);
      FACETID current = shS->substanceActiveFacet[currentSubstance];
      FACETID newId = current =! facetId ? facetId : NOFACET;
      // this works because we don't know the order that a facetId will be reached
      // we might be exiting the current facet or entering a new one first so this
      // ignores exits that occur after we enter a new facet
      shS->substanceActiveFacet[currentSubstance] = newId;
    }
    else {
      LAYERID layer = headerLayerId(header);
      toggleLayerActive(shS, layer);
    }
}

inline void passHeaderTop( PMEM ShapeState *shS
                         ,          HEADER  header
                         ) {
    if (headerPersistTop(header)) {
        passHeader(shS, header);
    }
}

inline void passHeaderBottom( PMEM ShapeState *shS
                            ,          HEADER  header
                            ) {
    if (headerPersistBottom(header)) {
        passHeader(shS, header);
    }
}

// Parse all of the current shapes adding as many thresholds as possible.
// Return the bottom of the rendering area which is the bottom of the tile if everything fits and the last
// complete section if it doesn't.
void buildThresholdArray ( PMEM       TileState *tileS
                         , PMEM  ThresholdQueue *tQ
                         , PMEM      ShapeState *shS
                         , GMEM          float4 *geometryHeap
                         , GMEM       HardFacet *facetHeap
                         , GMEM           Slice *geoRefHeap
                         , GMEM         ITEMTAG *itemTagHeap
                         , GMEM    SUBSTANCETAG *substanceTagHeap
                         ,            ITEMTAGID  itemStart
                         ,            ITEMTAGID  numItems
                         ,               float2  threadDelta
                         ) {
    bool thresholdWasAdded;
    int lastSubstance = -1;
    for (ITEMTAGID n = 0; n < numItems /*&& shS->layerCount < MAXLAYERS*/; n++) { // iterate over every item in the current tile.
        ITEMTAGID itemIndex = itemStart + n;
        ITEMTAG itemTag = itemTagHeap[itemIndex]; // get the current shape.
        thresholdWasAdded = false;
        //DEBUG_IF(printf("n %i ",n);showItemTag(itemTag);printf("\n");)
        if (itemTagIsShape(itemTag)) {
             // if you don't shift the shape to the tile size there will be accuracy errors with height floating point geometric values
            Slice geoRef = geoRefHeap[itemTagShapeId(itemTag)];
            GMEM float2 *strandHeap = (GMEM float2 *)&geometryHeap[getGeometryStart(geoRef)];
            bool enclosedByShape = false;
            for (int currentStrand = 0; currentStrand < getNumStrands(geoRef); currentStrand++) {
                uchar4 header = *((GMEM uchar4 *)strandHeap);
                ushort currentSize = as_ushort(header.xy); // size of current strand being parsed.
                bool enclosedByStrand = false; // is the start of the render area inside of the shape.
                Traversal left;
                Traversal right;
                // search the tree on the left and right sides of the pixel (or strand) and return 2 traversal result structures.
                bool inRange = traverseTree(  strandHeap
                                           ,  currentSize
                                           ,  threadDelta
                                           , &left
                                           , &right
                                           );
                if (inRange) {
                    spawnThresholds (  tQ
                                    ,  tileS
                                    ,  shS->layerCount
                                    , &left
                                    , &right
                                    , &thresholdWasAdded
                                    , &enclosedByStrand
                                    );
                }
                strandHeap += currentSize;
                enclosedByShape = enclosedByShape != enclosedByStrand; // using not equal as exclusive or.
            } // for currentStrand
            if (thresholdWasAdded || enclosedByShape) {
                if (shS->layerCount < MAXLAYERS) {
                   shS->layerStack[shS->layerCount] = createLayerEntry(itemTagIsAdd(itemTag),shS->substanceCount);
                   if (enclosedByShape) {
                       passHeader(shS, defaultShapeHeader(shS->layerCount)); // a default header has no flags for geometry and just changes the shapeState for a threshold above the boundaries.
                       //DEBUG_IF(printf("passHeader\n");showShapeState(shS);)
                   }
                   shS->layerCount += 1;
                }
                if (lastSubstance != (int)itemTagSubstanceId(itemTag)) {
                  // add this new substance.
                  SUBSTANCETAG tag = substanceTagHeap[itemTagSubstanceId(itemTag)];
                  //DEBUG_IF(printf("subCount %i itemTagSub %i ",shS->substanceCount,itemTagSubstanceId(itemTag));showSubstanceTag(tag);printf("\n");)
                  shS->substanceTagStack[shS->substanceCount] = tag;
                  shS->substanceCount += 1;
                }
            }
            // tS->lastShape  = n;
        }
        else { // itemTagIsFacet
           if (lastSubstance == (int)itemTagSubstanceId(itemTag)) {
             // only add a facet if it has the same substance as the most recently added shape.
           }
        }
    }
}

void initRandomField( ParseState *pS
                    , TileState *tileS
                    , CMEM float *randomField) {
  // find a random starting point in the field passed on the absolute start position of the column.
  int start = tileS->threadUnique & RANDOMFIELDMASK;
  pS->randomFieldCursor = as_uint(randomField[start]) & RANDOMFIELDMASK;
  //if (tileS->tileIndex==0) {printf("tileS->tileIndex %i tileS->column %i start %i pS->randomFieldCursor %i tileS->threadUnique %i\n"
  //                                 ,tileS->tileIndex   ,tileS->column   ,start   ,pS->randomFieldCursor,   tileS->threadUnique);}
  pS->randomField = randomField;
}

float getRandom(ParseState *pS) {
    pS->randomFieldCursor = (pS->randomFieldCursor + 1) & RANDOMFIELDMASK;
    float random = pS->randomField[pS->randomFieldCursor];
    //DEBUG_IF(printf("random %f\n", random);)
    return random;
}

inline Slice initQueueSlice() {
    Slice qSlice;
    qSlice.sStart  = MAXTHRESHOLDS;
    qSlice.sLength = 0;
    return qSlice;
}

void initThresholdQueue( PMEM ThresholdQueue  *tQ
                       , PMEM       TileState *tileS
                       , GMEM      THRESHOLD  *thresholdHeap
                       , GMEM         HEADER  *headerHeap
                       ,               Slice   qSlice
                       ) {
    tQ->thresholdHeaders = headerHeap    + (tileS->threadId * MAXTHRESHOLDS);
    tQ->thresholds       = thresholdHeap + (tileS->threadId * MAXTHRESHOLDS);
    tQ->qSlice           = qSlice;
}

void initShapeState (PMEM    ShapeState *shS
                    ) {
  shS->layerCount = 0;
  shS->substanceCount = 0;
}

ShapeState loadShapeState( GMEM ShapeState  *shapeStateHeap
                         , PMEM  TileState  *tileS
                         ) {
    return shapeStateHeap[tileS->threadId];
}

void storeShapeState( GMEM ShapeState  *shapeStateHeap
                    , PMEM ShapeState  *shS
                    , PMEM  TileState  *tileS
                    ) {
    shapeStateHeap[tileS->threadId] = *shS;
}

Slice loadQueueSlice( GMEM     Slice  *qSliceHeap
                    , PMEM TileState  *tileS
                    ) {
    return qSliceHeap[tileS->threadId];
}


void storeQueueSlice( GMEM     Slice  *qSliceHeap
                    ,          Slice   qSlice
                    , PMEM TileState  *tileS
                    ) {
    qSliceHeap[tileS->threadId] = qSlice;
}

void initParseState ( PMEM ParseState *pS
                    , PMEM  TileState *tileS
                    ,             int  frameNumber
                    , CMEM      float *randomField
                    ) {
    pS->currentThreshold = 0;
    pS->numActive        = 0; // the next threshold that is not currently active.
    pS->accColorArea = (float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0));
    // if we go below render bottom we must rebuild the threshold list.
    pS->sectionStart = (SPACE2)(LEFTBORDER, RENDERSTART); // the top of the current vertical section being processed
    pS->sectionEnd   = (SPACE2)(RIGHTBORDER,RENDERSTART); // the bottom of the current vertical section being processed
    pS->sectionCount = 0;

    pS->frameNumber = frameNumber;
    pS->buildCount = 0;
    initRandomField(pS,tileS,randomField);
}

GMEM TileInfo *getTileInfo ( GMEM  TileInfo *tileHeap
                           , int  tileIndex
                           ) {
    return (&tileHeap[tileIndex]);
}

inline int bitmaskN(int n) {return (1 << n) - 1;}

void initTileState ( PMEM  TileState *tileS
                   , GMEM   TileInfo *tileInfo
                   ,            int2  bitmapSize
                   ,             int  column
                   ,             int  jobIndex
                   ,             int  computeDepth
                   ) {
    tileS->tileItemStart = tileInfo->tileShapeSlice.sStart;
    tileS->tileNumItems  = tileInfo->tileShapeSlice.sLength;
    tileS->bitmapSize    = bitmapSize;
    tileS->threadId      = tileInfo->tileColumnAllocation + column;
    tileS->column        = column;
    int hDepth = (int)tileInfo->tileHDepth;
    int vDepth = (int)tileInfo->tileVDepth;
    int diffDepth = max(0, vDepth - (computeDepth - hDepth));
    int desiredHeight = 1 << diffDepth;
    int internalX = bitmaskN((int)hDepth) & column;
    int internalY = (column >> hDepth) << diffDepth;
    // DEBUG_IF(printf("computeDepth %i diffDepth %i internalX %i internalY %i\n",computeDepth, diffDepth,internalX,internalY);)
    tileS->internalDelta = (int2)(internalX, internalY);
    tileS->threadDelta   = tileS->internalDelta + boxLeftTop(tileInfo->tileBox); // the threadDelta is the internal delta + the topleft corner of the tileBox.
    tileS->threadUnique  = (tileInfo->tileColumnAllocation + (1 << computeDepth) + jobIndex * (1 << (computeDepth + computeDepth))) * LARGE_PRIME; // unique integer for the thread within the group.
    tileS->intHeight     = min( desiredHeight, tileS->bitmapSize.y-tileS->threadDelta.y);
    tileS->floatHeight   = convert_float( tileS->intHeight);
    tileS->tileSize.x    = boxRight(tileInfo->tileBox)  - boxLeft(tileInfo->tileBox);
    tileS->tileSize.y    = boxBottom(tileInfo->tileBox) - boxTop(tileInfo->tileBox);
}

bool isActiveThread ( PMEM TileState *tileS) {
      return (tileS->internalDelta.y < tileS->tileSize.y) && (tileS->threadDelta.x < tileS->bitmapSize.x) && (tileS->threadDelta.y < tileS->bitmapSize.y);
}

float8 sectionColor ( PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    , PMEM     ColorState *cS
                    ) {
    COLOR color = compositeLayers( shS
                                 , cS
                                 );
    float random = getRandom(pS);
    float area = (pS->sectionEnd.x - pS->sectionStart.x) * (pS->sectionEnd.y - pS->sectionStart.y);
    float4 adjustedArea = (float4) (area + (area * random * STOCHASTIC_FACTOR));
    return (float8)(color * adjustedArea, adjustedArea);
}

void verticalAdvance( PMEM ThresholdQueue *tQ
                    , PMEM      TileState *tileS
                    , PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    ) {
    if (pS->sectionEnd.x == RIGHTBORDER) {
        //DEBUG_IF(printf("---------- Vertical Advance -------------- \n");)
        // Start by undoing all of the state changes from the horizontal traversal.
        // Occasionally a threshold gets skipped because they are out of order.
        // pass these.
        // Revert all horizontal border crossing from the lass vertical advances.
        //DEBUG_IF(printf("rev->");)
        for (int i = 0; i < pS->numActive; i++) {
            //DEBUG_IF(printf("back %i %i\n",i,headerLayerId(getHeader(tQ, i)));)
            passHeader(shS, getHeader(tQ, i));
        }
        //DEBUG_IF(printf("<-rev");)
        // Next break is the next natural break point (either the bottom of the pixel or the bottom of the render area.
        float nextBreak = min(RENDEREND, pS->pixelY);
        // The activeBottom is the bottom of the current group of horizontally adjacent thresholds or max float if none are left.
        float activeBottom = tQ->qSlice.sLength > 0 ? tBottom(getThreshold(tQ, 0)) : FLT_MAX;
        // If the last section ended at the bottom of the current group.
        if (activeBottom == pS->sectionEnd.y) {
            // then pass over the bottom of all the active thresholds.
            //DEBUG_IF(printf("pS->numActive %i\n", pS->numActive);)
            while(pS->numActive > 0) {
                //DEBUG_IF(if (headerPersistBottom(getHeader(tQ, i))) {printf("bott %i %i\n",i,headerLayerId(getHeader(tQ, i)));})
                passHeaderBottom(shS, getHeader(tQ, 0));
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
            // first find the top of the next threshold.
            // nextTop is either the top of the next available threshold of max float.
            float nextTop = pS->numActive < tQ->qSlice.sLength ? tTop(getThreshold(tQ, pS->numActive)) : FLT_MAX;
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
                    //DEBUG_IF(if (headerPersistTop(getHeader(tQ, pS->numActive))) {printf("topH %i %i\n",pS->numActive,headerLayerId(getHeader(tQ, pS->numActive)));})
                    passHeaderTop(shS, getHeader(tQ, 0));
                    popTop(tQ);
                    pS->numActive -= 1;
                }
                for (int i = 0; i < pS->numActive; i++) {
                    if (tTop(getThreshold(tQ, i)) > RENDERSTART) { // TODO: Can probably get rid of this check.
                        //DEBUG_IF(if (headerPersistTop(getHeader(tQ, i))) {printf("top  %i %i\n", i, headerLayerId(getHeader(tQ, i)));})
                        passHeaderTop(shS, getHeader(tQ, i));
                    }
                }
            }
        }
        //DEBUG_IF(printf(">>> activeBottom %f nextBottom: %f \n", activeBottom, nextBottom);)
        // advance the section start vertically to the end of the current section. (in the beggining sectionStart.y == sectionEnd.y already so nothing happens.)
        pS->sectionStart.y = pS->sectionEnd.y;
        // advance the bottom of the section to either the next break or the bottom of the adjacent thresholds depending on what is next.
        pS->sectionEnd.y = nextBottom;
        // set the horizontal borders of the section to the far left to await a horizontal advance.
        pS->sectionStart.x = pS->sectionEnd.x = LEFTBORDER;
        // reset the cursor
        pS->currentThreshold = 0;
        //DEBUG_IF(showLayerFlags(shS->layerFlags); printf("  ");
        //         showActiveThresholds(tQ,pS->numActive);)

    }
}

void horizontalAdvance( PMEM ThresholdQueue *tQ
                      , PMEM     ParseState *pS
                      ) {
    // find the right side of the current section.
    SPACE nextX;
    //DEBUG_IF(printf("do ..  pS->sectionEnd %v2f nextX %f\n", pS->sectionEnd, nextX);)
    if (pS->currentThreshold < pS->numActive) {
        //DEBUG_IF(printf("pS->currentThreshold < pS->numActive\n");)
        // find the midpoint of the threshold when bound by the section
        nextX = thresholdMidXLow( getThreshold(tQ, pS->currentThreshold)
                                , getHeader(tQ, pS->currentThreshold)
                                , pS->sectionStart.y
                                , pS->sectionEnd.y
                                , LEFTBORDER
                                , RIGHTBORDER
                                );
        //DEBUG_IF(printf("midX %f\n", nextX);)
    }
    else {
        //DEBUG_IF(printf("else: nextX = RIGHTBORDER\n");)
        nextX = RIGHTBORDER;
    }
    //DEBUG_IF(printf("nextX %f\n", nextX);)
    pS->sectionStart.x = pS->sectionEnd.x;
    pS->sectionEnd.x   = nextX;
}

void writePixelGlobal ( PMEM TileState *tileS
                      ,          COLOR  color
                      , GMEM      uint *out
                      ,            int  y
                      ) {
    uint colorWord = colorToSolidPixel_Word32_BGRA(color);
    //DEBUG_IF(printf("write %3i    %2.2v4f    %x\n", y, color,colorWord);)
    int outPos = (mul24(tileS->threadDelta.y + y, tileS->bitmapSize.x)) + tileS->threadDelta.x;
    out[outPos] = colorWord;
}

#define DEBUG_SECTION DEBUG_IF(float area = (pS->sectionEnd.x - pS->sectionStart.x) * (pS->sectionEnd.y - pS->sectionStart.y);\
                               printf("next %3i cr %i ae %i area %f c %2.2v8f sStart %2.2v2f sEnd %2.2v2f accC %2.2v8f" \
                                     , pS->sectionCount \
                                     , pS->currentThreshold \
                                     , pS->numActive \
                                     , area \
                                     , colorArea \
                                     , pS->sectionStart \
                                     , pS->sectionEnd \
                                     , pS->accColorArea); \
                               showLayerFlags(shS->layerFlags); \
                               printf("\n"); \
                               )

// "Who creates an engine that can properly render an image with more than 128 horizontal thresholds in
// one pixel?" You might ask. And we reply "We do, that's who."

void calculatePixel ( PMEM      TileState *tileS
                    , PMEM ThresholdQueue *tQ
                    , PMEM     ShapeState *shS
                    , PMEM     ParseState *pS
                    , PMEM     ColorState *cS
                    ) {
    //DEBUG_IF(printf("                                              pixelY: %f \n", pS->pixelY);)
    while (((pS->sectionEnd.x < RIGHTBORDER) || (pS->sectionEnd.y < pS->pixelY))/*&& count > 0*/) { // process all sections that do not reach the bottom of the pixel.
        //DEBUG_IF(printf("loop        sectionStart %v2f sectionEnd %v2f \n", pS->sectionStart, pS->sectionEnd);)
        //DEBUG_IF(printf("beforeV cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
        verticalAdvance(tQ, tileS, pS, shS);
        //DEBUG_IF(printf("afterV  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
        horizontalAdvance(tQ, pS);
        //DEBUG_IF(printf("afterH  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
        float8 colorArea = sectionColor( pS
                                       , shS
                                       , cS
                                       );
        pS->accColorArea += colorArea;
        //DEBUG_IF(printf("accColor %v8f\n", pS->accColorArea);)
        DEBUG_TRACE_ITEM(parseStateHs(*pS);)
        if (pS->currentThreshold < pS->numActive) {
            //DEBUG_IF(printf("pass %i %i\n",pS->currentThreshold,headerLayerId(getHeader(tQ, pS->currentThreshold)));)
            passHeader(shS, getHeader(tQ, pS->currentThreshold));
        }
        pS->currentThreshold += 1;
        pS->sectionCount += 1;
        //DEBUG_IF(printf("atEnd  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
    } // while (((pS->sectionEnd.x < RIGHTBORDER) || (pS->sectionEnd.y < pS->pixelY)))
    //DEBUG_IF(printf("pixelDone\n");)=
}

// create an initial color state.
void initColorState( PMEM   ColorState *init
                   ,             COLOR  backgroundColor
                   , GMEM        uchar *pictureData
                   , GMEM      PictUse *pictureRefs
                   , GMEM        COLOR *solidColors
                   ,              int2  pos
                   ) {
  init->csBackgroundColor = backgroundColor;
  init->csPictureData = pictureData;
  init->csPictureRefs = pictureRefs;
  init->csSolidColors = solidColors;
  init->absolutePosition = pos;
}


bool swapIfAbove( PMEM ThresholdQueue *tQ
                ,                 int  i
                ,                bool  done
                ) {
    HEADER    aHeader = getHeader(   tQ, i    );
    THRESHOLD a       = getThreshold(tQ, i    );
    HEADER    bHeader = getHeader(   tQ, i + 1);
    THRESHOLD b       = getThreshold(tQ, i + 1);
    bool swap =
        (tTop(a) > tTop(b)) ||
            (
                (tTop(a) == tTop(b)) &&
                    (
                        (tTopX(aHeader, a) > tTopX(bHeader, b)) ||
                        (
                            (tTopX(aHeader, a) == tTopX(bHeader, b)) &&
                            (thresholdInvertedSlope(aHeader, a) > thresholdInvertedSlope(bHeader, b))
                        )
                    )
            );
     if (swap) {
         setHeader(   tQ, i    , bHeader );
         setThreshold(tQ, i    , b       );
         setHeader(   tQ, i + 1, aHeader );
         setThreshold(tQ, i + 1, a       );
         done = false; // tripwire activated !!!
     }
     return done;
}

void sortThresholdArray( PMEM  ThresholdQueue *tQ
                       ) {
      // The selection sort algorithm
      bool done = false;
      // k = last item to be checked
      int k = (int)tQ->qSlice.sLength; // Check all items
      while (!done) {
         done = true;             // Set tripwire
         // Sort the unsort part a[k..n] (n = a.length)
         for (int i = 0 ; i < k-1 ; i ++ ) {
            done = swapIfAbove(tQ, i, done);
         }
         k--;       // Shorten the number of pairs checked.
      }
 }

 void renderThresholdArray ( PMEM       TileState *tileS
                           , PMEM  ThresholdQueue *tQ
                           , PMEM      ShapeState *shS
                           , GMEM       HardFacet *facetHeap
                           , GMEM           uchar *pictureData
                           , GMEM         PictUse *pictureRefs
                           , GMEM           COLOR *solidColors
                           , CMEM           float *randomField
                           ,                COLOR  backgroundColor
                           ,                  int  frameNumber
                           , GMEM            uint *out
                           ) {
    //DEBUG_IF(printf("INDEX %i gTileIndex %i numItems %i \n", INDEX, gTileIndex, tileS->numItems);)
    //barrier (CLK_LOCAL_MEM_FENCE);
    ParseState pS;
    initParseState( &pS
                  ,  tileS
                  ,  frameNumber
                  ,  randomField
                  );
    ColorState cS;
    initColorState( &cS
                  ,  backgroundColor
                  ,  pictureData
                  ,  pictureRefs
                  ,  solidColors
                  ,  tileS->threadDelta
                  );
    int yInt = -1;
    for (pS.pixelY = 1.0f; pS.pixelY <= tileS->floatHeight; pS.pixelY += PIXELHEIGHT) { // y is the bottom of the current pixel.
        yInt += 1;
        calculatePixel (  tileS
                       ,  tQ
                       ,  shS
                       , &pS
                       , &cS
                       );
        // write the accumulated color information to the pixel buffer.
        float4 color = pS.accColorArea.s0123 / pS.accColorArea.s4567;
        writePixelGlobal ( tileS
                         , color
                         , out
                         , yInt
                         );
        pS.accColorArea = (float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0));
        pS.sectionStart = (SPACE2)(LEFTBORDER,pS.pixelY);
        cS.absolutePosition += (int2)(0,1);
    } // for y
    DEBUG_TRACE_END
}

__kernel void generateThresholds( GMEM      float4  *geometryHeap
                                , GMEM       Slice  *geoRefHeap
                                , GMEM   HardFacet  *facetHeap
                                , GMEM     ITEMTAG  *itemTagHeap
                                , GMEM SUBSTANCETAG *substanceTagHeap
                                , GMEM     TileInfo  *tileHeap
                                ,              int2   bitmapSize
                                ,               int   computeDepth
                                ,               int   frameNumber
                                ,               int   jobIndex
                                , GMEM    THRESHOLD  *thresholdHeap
                                , GMEM       HEADER  *headerHeap
                                , GMEM   ShapeState  *shapeStateHeap
                                , GMEM        Slice  *qSliceHeap
                                ) {
    int   tileIndex  = INDEX; // the sequential number of the tile in the current workgroup.
    int   column     = COLUMN;
    //DEBUG_IF(printf("sizeof(ThresholdQueue)=%i\n", sizeof(ThresholdQueue));)
    //DEBUG_IF(printf("sizeof(ShapeState)=%i\n", sizeof(ShapeState));)
    GMEM TileInfo *tileInfo = getTileInfo(tileHeap, tileIndex);
    TileState tileS;
    initTileState ( &tileS
                  ,  tileInfo
                  ,  bitmapSize
                  ,  column
                  ,  jobIndex
                  ,  computeDepth
                  );
    if (isActiveThread(&tileS)) {
        DEBUG_TRACE_BEGIN
        ThresholdQueue tQ;
        initThresholdQueue(&tQ, &tileS, thresholdHeap, headerHeap, initQueueSlice());
        ShapeState shS;
        initShapeState(&shS);
        buildThresholdArray ( &tileS
                            , &tQ
                            , &shS
                            ,  geometryHeap
                            ,  facetHeap
                            ,  geoRefHeap
                            ,  itemTagHeap
                            ,  substanceTagHeap
                            ,  tileS.tileItemStart
                            ,  tileS.tileNumItems
                            ,  convert_float2(tileS.threadDelta)
                            );
        //DEBUG_IF(showLayerFlags(shS.layerFlags);)
        //DEBUG_IF(showThresholds(&tQ);)
        //DEBUG_TRACE_ITEM(thresholdStateHs(&tQ);)
        storeShapeState(shapeStateHeap, &shS, &tileS);
        //DEBUG_IF(showShapeState(&shS);)
        //DEBUG_IF(printf("qSliceHeap %p column %i tQ.qSlice.sStart %i tQ.qSlice.sLength %i\n", qSliceHeap, column, tQ.qSlice.sStart, tQ.qSlice.sLength);)
        storeQueueSlice(qSliceHeap, tQ.qSlice, &tileS);
    }
}

__kernel void sortThresholds( GMEM  THRESHOLD *thresholdHeap
                            , GMEM     HEADER *headerHeap
                            , GMEM      Slice *qSliceHeap
                            , GMEM   TileInfo *tileHeap
                            ,            int2  bitmapSize
                            ,             int  computeDepth
                            ,             int  frameNumber
                            ,             int  jobIndex
                            ) {
    int   tileIndex  = INDEX; // the sequential number of the tile in the current workgroup.
    int   column     = COLUMN;
    GMEM TileInfo *tileInfo = getTileInfo(tileHeap, tileIndex);
    TileState tileS;
    initTileState ( &tileS
                  ,  tileInfo
                  ,  bitmapSize
                  ,  column
                  ,  jobIndex
                  ,  computeDepth
                  );
    if (isActiveThread(&tileS)) {
        DEBUG_TRACE_BEGIN
        Slice qSlice = loadQueueSlice(qSliceHeap, &tileS);
        //DEBUG_IF(printf("qSliceHeap %p column %i qSlice.sStart %i qSlice.sLength %i\n", qSliceHeap, column, qSlice.sStart, qSlice.sLength);)
        ThresholdQueue tQ;
        initThresholdQueue(&tQ, &tileS, thresholdHeap, headerHeap, qSlice);

        //DEBUG_IF(printf("before------------\n");showThresholds(&tQ);)
        sortThresholdArray(&tQ);
        //DEBUG_IF(printf("after------------\n");showThresholds(&tQ);)
    }
}

__kernel void renderThresholds( GMEM    THRESHOLD *thresholdHeap
                              , GMEM       HEADER *headerHeap
                              , GMEM   ShapeState *shapeStateHeap
                              , GMEM        Slice *qSliceHeap
                              , GMEM    HardFacet *facetHeap
                              , GMEM        uchar *pictureData
                              , GMEM      PictUse *pictureRefs
                              , GMEM        COLOR *solidColors
                              , CMEM        float *randomField
                              , GMEM     TileInfo *tileHeap
                              ,             COLOR  backgroundColor
                              ,              int2  bitmapSize
                              ,               int  computeDepth
                              ,               int  frameNumber
                              ,               int  jobIndex
                              , GMEM         uint *out
                              ) {
    int   tileIndex  = INDEX; // the sequential number of the tile in the current workgroup.
    int   column     = COLUMN;
    GMEM TileInfo *tileInfo = getTileInfo(tileHeap, tileIndex);
    //DEBUG_IF(showTileInfoAlignment(0,tileInfo);)
    TileState tileS;
    initTileState ( &tileS
                  ,  tileInfo
                  ,  bitmapSize
                  ,  column
                  ,  jobIndex
                  ,  computeDepth
                  );
    DEBUG_IF(tileStateHs(tileS);)
    if (isActiveThread(&tileS)) {
        DEBUG_TRACE_BEGIN
        Slice qSlice = loadQueueSlice(qSliceHeap, &tileS);
        //DEBUG_IF(printf("qSliceHeap %p column %i qSlice.sStart %i qSlice.sLength %i\n", qSliceHeap, column, qSlice.sStart, qSlice.sLength);)
        ThresholdQueue tQ;
        initThresholdQueue(&tQ, &tileS, thresholdHeap, headerHeap, qSlice);
        ShapeState shS = loadShapeState(shapeStateHeap, &tileS);
        DEBUG_IF(printf("render shapeState\n");)
        DEBUG_IF(showShapeState(&shS);)
        //DEBUG_IF(showThresholds(&tQ);)
        //DEBUG_TRACE_ITEM(thresholdStateHs(&tQ);)
        renderThresholdArray ( &tileS
                             , &tQ
                             , &shS
                             ,  facetHeap
                             ,  pictureData
                             ,  pictureRefs
                             ,  solidColors
                             ,  randomField
                             ,  backgroundColor
                             ,  frameNumber
                             ,  out
                             );
    }
}

void showShapeState(ShapeState *shS) {
   printf("--- Layers --- \n");
   for (LAYERID i = 0; i < shS->layerCount; i++) {
     printf("%i ",i);showLayerEntry(shS->layerStack[i]);printf("\n");
   }
   printf("--- SubstanceTags --- \n");
   for (LOCALSUBSTANCE i = 0; i < shS->substanceCount; i++) {
     printf("%i ",i);showSubstanceTag(shS->substanceTagStack[i]);printf(" facet %i \n",shS->substanceActiveFacet[i]);
   }
}

void showLayerEntry(LAYERENTRY entry) {
  printf("act %i add %i substance %i", layerEntryIsActive(entry), layerEntryIsAdditive(entry), layerEntryLocalSubstance(entry));
}

void showSubstanceTag(SUBSTANCETAG tag) {
  printf("type %i ref %i ", substanceTagType(tag), substanceTagTextureMemId(tag));
}

void showItemTag(ITEMTAG tag) {
  printf("isFacet %i add %i substanceid %i facetid %i", itemTagIsFacet(tag), itemTagIsAdd(tag), itemTagSubstanceId(tag), itemTagFacetId(tag));
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
    printf(" layerId:%03i localSubstance: %03i ", headerLayerId(header), headerLocalSubstance(header));
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
