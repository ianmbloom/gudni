// 1 Space for haskell defined macros
// 2 So the line numbers are correct
// 3  -----------------------------------------------------------------------------
// 4  -- |
// 5  -- Module      :  Graphics.Gudni.OpenCL.CallKernel
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

#define MAXBUILDS 2 // number of times to rebuild the list of thresholds in one kernel.
#define COLORBUFFERSIZE 4
#define MINCROP 0.2f
// Debugging
#define DEBUGCOLUMN 0 // Determines the column for DEBUG_IF macro
#define DEBUGINDEX  0 // Determines the index for DEBUG_IF macro
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

// A geo entry refers to an entry in the global table of shape geometry. A single geometry entry can be referenced by multiple tiles
#define GEO_ENTRY  uint
// The shape tag includes 4 bit combination style value, the 4 bit texture type and the 28 bit substance identifier.
#define SHAPETAG ulong
// A substance id is the reference to a group of shapes that share color/texture information.
#define SUBSTANCEID ulong
// A shape bit is the number of each shape assigned as it is added to a column, each shape number corresponds to a bit in the shape stack
// so the number of possible shape bits is limited to the size (in bits) of the shape stack.
#define SHAPEBIT    uint

// Is Compound determines if the shape is a complex compound shape. If it is zero the shape is assumed to be made up of continuation
// shapes.

inline bool     shapeTagIsSolidColor (SHAPETAG tag) {return (tag & SHAPETAG_SUBSTANCETYPE_BITMASK) == SHAPETAG_SUBSTANCETYPE_SOLIDCOLOR;}
inline SHAPETAG shapeTagSubstanceType(SHAPETAG tag) {return (tag & SHAPETAG_SUBSTANCETYPE_BITMASK) >> SHAPETAG_SUBSTANCETYPE_SHIFT;}

inline int  shapeTagCompound  (SHAPETAG tag) {return (tag & SHAPETAG_COMPOUNDTYPE_BITMASK) >> SHAPETAG_COMPOUNDTYPE_SHIFT;   }
inline bool shapeTagIsAdd     (SHAPETAG tag) {return (tag & SHAPETAG_COMPOUNDTYPE_BITMASK) == SHAPETAG_COMPOUNDTYPE_ADD;     }
inline bool shapeTagIsSubtract(SHAPETAG tag) {return (tag & SHAPETAG_COMPOUNDTYPE_BITMASK) == SHAPETAG_COMPOUNDTYPE_SUBTRACT;}
inline bool shapeTagIsContinue(SHAPETAG tag) {return (tag & SHAPETAG_COMPOUNDTYPE_BITMASK) == SHAPETAG_COMPOUNDTYPE_CONTINUE;}

inline  SUBSTANCEID shapeTagSubstanceId(SHAPETAG tag) {return (tag & SHAPETAG_SUBSTANCEID_BITMASK);}

// Memory Types

#define TMEM __local   // memory type used to store threshold header and threshold buffers
#define SMEM __global  // memory type used to store shape references and strandheap geometry etc.

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
#define HEADER        uint   // the threshold-header includes identifying information
#define THRESHOLD     SPACE4 // the stored version of a threshold encoded in floats

// ThresholdHeader 16it,  uchar2
// Bits | 1 bit  | 1 bit      | 1 bit   | 1 bit | 28 bit
// Type | bool   | bool       | bool    | bool  | uint
// Desc | slope  | persistent | enabled | brush | shapeBit or
//      | sign   |            |         | mode  | brush
// slope sign - determines the sign of the slope of the threshold.
// persitent - determines if the threshold intersects with the left side of the pixel.
// threshold enable - determines if the threshold has been disabled by when converted from brush to shapeindex
// in brush mode - determines if the payload is the shapeBit or defining a compound shape.

#define POSITIVE_SLOPE_MASK    0x80000000 // leftmost bit
#define PERSIST_AND_SLOPE_MASK 0xC0000000 // leftmost and second to leftmost bit
#define PERSIST_TOP            0xC0000000 // positive slope and persist
#define PERSIST_BOTTOM         0x40000000 // not positive slopw and persist

#define POSITIVE_SLOPE         0x80000000
#define NEGATIVE_SLOPE         0x00000000

#define PERSIST                0x40000000 // isolate the persist bit
#define NONPERSIST             0x00000000 // just zero
#define UNPERSISTMASK          0xBFFFFFFF // everything but the persist bit

#define THRESHOLDENABLE        0x20000000 // determine if the threshold has been deactivated by brushes.
#define THRESHOLDDISABLE       0x00000000
#define DISABLEMASK            0xDFFFFFFF // AND with a header to disable the header.

#define SHAPEBIT_MASK          0x0FFFFFFF // right 29 bits including the brushmode
#define WITHOUT_SHAPEBIT       0xF0000000 // bits without shape index

// & has a lower precedence than !=
inline     bool headerPositiveSlope(HEADER h) {return (h & POSITIVE_SLOPE_MASK) != 0;} // determine if the threshold has a positive slope
inline     bool headerPersistTop   (HEADER h) {return (h & PERSIST_AND_SLOPE_MASK) == PERSIST_TOP;   } // determine if the top of the threshold affects the persistant state of the shapestack
inline     bool headerPersistBottom(HEADER h) {return (h & PERSIST_AND_SLOPE_MASK) == PERSIST_BOTTOM;} // determine if the bottom of the threshold affects the persistant state of the shapestack
inline     bool headerPersistEither(HEADER h) {return (h & PERSIST) != 0;  } // determine if either top or bottom of the threshold affects the persistant state of the shapestack
inline SHAPEBIT headerShapeBit     (HEADER h) {return  h & SHAPEBIT_MASK;} // get the index of the shape that corresponds to the threshold

inline HEADER unPersist      (HEADER h) {return h & UNPERSISTMASK;}
inline HEADER disableHeader  (HEADER h) {return h & DISABLEMASK;  }
inline   bool headerIsEnabled(HEADER h) {return (h & THRESHOLDENABLE) != 0;}

inline HEADER setEnableBit ( HEADER header
                           , HEADER newBit
                           , bool enable
                           ) {
    HEADER enableShape = enable ? THRESHOLDENABLE : THRESHOLDDISABLE;
    return (header & PERSIST_AND_SLOPE_MASK) | enableShape | newBit;
}

inline HEADER setShapeBit ( HEADER header
                          , HEADER newBit
                          ) {
    return (header & WITHOUT_SHAPEBIT) | newBit;
}

inline HEADER makeHeaderIndex(int shapeBit) {
    return THRESHOLDENABLE | shapeBit;
}

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
#define TAXICAB_FLATNESS 0.25f // minimum taxicab distance between (relative to the pixel size) where curve tesselation terminates

// ShapeStack

#define SHAPESTACK             ulong
#define SHAPESTACKBITS         64
#define SHAPESTACKCARRYSHIFT   63
#define SHAPESTACKCARRYMASK    0x1
#define SHAPESTACKSECTIONSHIFT 6    // the amount to shift to get the section from the total bits
#define SHAPESTACKSECTIONBITS  0x3F
#define SHAPESTACKSECTIONS     8

#define EMPTY_SHAPESTACK       0x0
#define COMPLETE_MASK          0xFFFFFFFFFFFFFFFF
// The shape index of top shape (the visible one) can be determined by counting the leading zeros in the shapestack value using clz

inline void clearShapeStack(SHAPESTACK *stack) {
    for (int i = 0; i < SHAPESTACKSECTIONS; i++) {
        stack[i] = EMPTY_SHAPESTACK;
    }
}

inline SHAPESTACK ignoreStack(SHAPESTACK section, int ignoreBits) {
    //DEBUG_IF(printf("ignoreBits %i (COMPLETE_MASK << ignoreBits) %lX (~(COMPLETE_MASK << ignoreBits)) %lX ignoreStack %lX (SHAPESTACKBITS - clz(ignoreStack)) - 1 %i \n", ignoreBits, (COMPLETE_MASK << ignoreBits), (~(COMPLETE_MASK << ignoreBits)), ignoreStack, (SHAPESTACKBITS - clz(ignoreStack)) - 1 );)
    return ignoreBits >= SHAPESTACKBITS ? section : (~(COMPLETE_MASK << ignoreBits)) & section;
}

inline int findSectionTop(SHAPESTACK section) {
    // find the top set bit
    return SHAPESTACKBITS - clz(section);
}

inline int findTop(PMEM SHAPESTACK *shapeStack, int ignoreAbove) {
    int ignoreSection = (ignoreAbove >> SHAPESTACKSECTIONSHIFT);
    int ignoreBits    = ignoreAbove & SHAPESTACKSECTIONBITS;
    //DEBUG_IF(printf("findTop ignoreAbove %i iS %i ignoreBits %i ", ignoreAbove, ignoreSection, ignoreBits);)
    SHAPESTACK section = ignoreStack(shapeStack[ignoreSection], ignoreBits);
    while (section == EMPTY_SHAPESTACK && ignoreSection > 0) {
      ignoreSection -= 1;
      section = shapeStack[ignoreSection];
    }
    int sectionBits = findSectionTop(section);
    return (ignoreSection << SHAPESTACKSECTIONSHIFT) + sectionBits - 1;
}


inline SHAPESTACK setSectionBit(SHAPEBIT bit)                     {return (((ulong)0x1) << bit);} // create a shape by shifting a bit to the right position
inline SHAPESTACK flipSectionBit(SHAPEBIT bit, SHAPESTACK section) {return (section ^ setSectionBit(bit));} // if shape on is true, toggle a shape bit

inline void flipBit(SHAPEBIT shapeBit, PMEM SHAPESTACK *shapeStack) {
    int section = shapeBit >> SHAPESTACKSECTIONSHIFT;
    int bit     = shapeBit & SHAPESTACKSECTIONBITS;
    //DEBUG_IF(printf("flipBit section %i bit %i before shapeStack[section] %lx ", section, bit, shapeStack[section]);)
    shapeStack[section] = flipSectionBit(bit, shapeStack[section]);
    //DEBUG_IF(printf(" after shapeStack[section] %lx\n", shapeStack[section]);)
}

// REF is a reference to an array with max index 32768
#define REF uint

//  ---------------------------------- Structure Types  ------------------------------------

// A slice is a section of array indexes.
typedef struct Slice
  { REF sStart;
    REF sLength;
  } Slice;

// A shape is a reference to shape data with information for on-the-fly translation, scaling, and shape combination.
typedef struct Shape
  { SHAPETAG shapeTag;
    Slice    shapeSlice;
  } Shape;

inline int getGeometryStart(Shape shape) {return shape.shapeSlice.sStart;}  // The first part of a shape slice refers to the position in the geometry buffer
inline int getNumStrands(Shape shape)    {return shape.shapeSlice.sLength;} // The second part refers to the number of strands that define the shape
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
  ; int   tileHDepth // logarithmic horizontal depth in tree (tileHDepth ^ 2 == tile width)
  ; int   tileVDepth // logarithmic vertical   depth in tree (tileVDepth ^ 2 == tile height)
  ; Slice tileShapeSlice // beggining and length of shape records for the tile.
  ;} TileInfo;

// A substance contains information about the substance of a group of combined shapes.
typedef struct Substance
  { COLOR  substanceColor; // this is either the solid color of the shape or a reference to a picture ref.
  } Substance;

// A picture reference is a reference to bitmap data that can be the substance of a shape.
typedef struct PictureUse
  { int2 pictTranslate; // translation vector in pixel units
    int2 pictSize;      // size of the bitmap
    int  pictMemOffset; // starting point of the pixel data in the memory buffer
  } PictureUse;

/*
#define COLORBUFFERMODULOMASK = 0x3 // must be adjusted if COLORBUFFERSIZE is changed
#define COLORBUFFERMODULO(index) index & COLORBUFFERMODULOMASK
#define COLORBUFFERINDEX(offset, index) COLORBUFFERMODULO(offset + index)
*/
// The color state structure tracks the current color information during a scan through thresholds
typedef struct ColorState {
               COLOR   csBackgroundColor;         // background color
     GMEM      uchar  *csPictureData;             // global image information
     CMEM PictureUse  *csPictureRefs;             // global list of image references
                bool   csIsConstant;              // determine is the color state changes after a pixel move
                int2   absolutePosition;          // the absolute position of the current pixel.
  } ColorState;

// the threshold state stores references to the threshold buffers and their size.
typedef struct ThresholdState {
    TMEM    HEADER  thresholdHeaders[MAXTHRESHOLDS]; // array of threshold header
    TMEM THRESHOLD  thresholds[MAXTHRESHOLDS];       // array of threshold geometry
               int  thresholdStart;                  // the position of the top of the stack.
               int  numThresholds;                   // number of thresholds in buffers
            SPACE2  renderStart;
            SPACE2  renderEnd;          // the lowest vertical position that can be properly rendered with the current list of thresholds.
                                        // if we go below render bottom we must rebuild the threshold list.
               int  thresholdWasAdded;  // used to determine if a shape ever interacted with the column as it's being added.
               int  slotThresholdCount;
               int  addThresholdCount;
  } ThresholdState;

inline int cycleLocation(int i) {
  return (i + MAXTHRESHOLDS) & MAXTHRESHOLDMASK;
  // this makes the position cyclic if MAXTHRESHOLDS is a power of 2
  // and MAXTHRESHOLDMASK = MAXTHRESHOLDS - 1
}

inline int tSLocation(ThresholdState *tS, int i) {
  return cycleLocation(tS->thresholdStart + i);
}

inline THRESHOLD getThreshold(ThresholdState *tS, int index) {
    return tS->thresholds[tSLocation(tS, index)];
}

inline void setThreshold(ThresholdState *tS, int index, THRESHOLD set) {
    tS->thresholds[tSLocation(tS,index)] = set;
}

inline HEADER getHeader(ThresholdState *tS, int index) {
    return tS->thresholdHeaders[tSLocation(tS,index)];
}

inline void setHeader(ThresholdState *tS, int index, HEADER set) {
    tS->thresholdHeaders[tSLocation(tS,index)] = set;
}

inline void pushTopSlot(ThresholdState *tS) {
    tS->thresholdStart = cycleLocation(tS->thresholdStart - 1);
    tS->numThresholds += 1;
}

inline void popTop(ThresholdState *tS) {
  tS->thresholdStart = cycleLocation(tS->thresholdStart + 1);
  tS->numThresholds -= 1;
  //DEBUG_IF(printf("popTop\n");)
}

#define RANDOMFIELDMASK RANDOMFIELDSIZE - 1

#define RANDOM_POS int

typedef struct ShapeState {
                SHAPEBIT  shapeBits;                 // the number of shapes assigned to a bit in the shapeStack.
      LMEM   SUBSTANCEID  shapeIndices[MAXSHAPE];     // a mapping from shape bit positions in the shapeStacks to shapeIndices in the tile.
      PMEM SHAPESTACK shapeStack[SHAPESTACKSECTIONS]; // the current shape Stack. (Each bit represents the presence of a shape in the stack.)
} ShapeState;

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
         GEO_ENTRY  tileShapeStart;
               int  tileNumShapes;
              int2  tileSize;
              int2  bitmapSize;
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

void addLineSegment ( PMEM  ThresholdState *tS
                    ,               float2  left
                    ,               float2  right
                    ,              SHAPEBIT  shapeBit
                    ,                  int  addType
                    ,                 bool *enclosedByStrand
                    );

void addThreshold ( PMEM ThresholdState *tS
                  ,              HEADER  newHeader
                  ,           THRESHOLD  newThreshold
                  ,                 int  addType
                  ,                bool *enclosedByStrand
                  );

bool traverseTree ( SMEM    float2 *strandHeap
                  ,            int  currentSize
                  ,           Shape shape
                  ,          float2 threadDelta
                  ,      Traversal *l
                  ,      Traversal *r
                  );

void searchTree(      Traversal *trav
               , SMEM    float4 *tree
               ,            int  treeSize
               ,         float4  threadDelta4
               ,           bool  isLeft
               );

void spawnThresholds ( PMEM  ThresholdState *tS
                     ,               HEADER  headerPayload
                     ,            Traversal *l
                     ,            Traversal *r
                     ,                 bool *enclosedByStrand
                     );

bool checkInRange ( Traversal *t
                  );

inline THRESHOLD lineToThreshold ( float2  left
                                 , float2  right
                                 );

inline HEADER lineToHeader (  SHAPEBIT shapeBit
                           ,   float2 left
                           ,   float2 right
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

inline void adjustToExclude( PMEM ThresholdState *pS
                           ,           THRESHOLD  threshold
                           );

int countActive ( PMEM ThresholdState *tS
                ,               float *nextTop
                );

float nextSlicePoint ( PMEM ThresholdState *tS
                     ,               float  slicePoint
                     ,                 int  numActive
                     );

void sliceActive( PMEM ThresholdState *tS
                ,               float  slicePoint
                ,                 int  numActive
                );

float splitNext ( PMEM ThresholdState *tS
                , PMEM     ParseState *pS
                );

inline bool thresholdIsAbove( HEADER newHeader
                            , THRESHOLD new
                            , HEADER oldHeader
                            , THRESHOLD old);

void removeLastThreshold ( PMEM ThresholdState *tS
                         );

bool isAboveLast( PMEM ThresholdState *tS
                  ,            HEADER  newHeader
                  ,         THRESHOLD  new
                  );
void insertThreshold( PMEM ThresholdState *tS
                    ,              HEADER  newHeader
                    ,           THRESHOLD  new
                    );

void slotThreshold ( PMEM ThresholdState *tS
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
                , SMEM  Substance *substances
                ,     SUBSTANCEID  substanceId
                ,            bool  isSolidColor
                );

COLOR determineColor( PMEM   ShapeState *shS
                    , PMEM   ColorState *cS
                    , SMEM    Substance *substances
                    , SMEM        Shape *shapeHeap
                    ,         GEO_ENTRY  shapeStart
                    );

void verticalAdvance( PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    );

void horizontalAdvance ( PMEM ThresholdState *tS
                       , PMEM     ParseState *pS
                       );

void writePixelGlobal ( PMEM TileState *tileS
                      ,          COLOR  color
                      , GMEM       uint *out
                      ,            int  y
                      );

 void removeLastShape( PMEM  ThresholdState *tS
                     , PMEM      ShapeState *shS
                     ,            GEO_ENTRY  shapeIndex
                     );

void buildThresholdArray ( PMEM  ThresholdState *tS
                         , PMEM      ShapeState *shS
                         , SMEM          float4 *geometryHeap
                         , SMEM       Substance *substances
                         , SMEM           Shape *shapeHeap
                         ,            GEO_ENTRY  shapeStart
                         ,            GEO_ENTRY  numShapes
                         ,               float2  threadDelta
                         );

void resetParser (PMEM ParseState *pS);

void nextRenderArea ( PMEM ThresholdState *tS
                    , PMEM ParseState *pS
                    ,           SPACE  height
                    );

void initRandomField( ParseState *pS
                    , TileState *tileS
                    , CMEM float *randomField
                    );

float getRandom(ParseState *pS);

void initThresholdState(ThresholdState *tS);
void resetThresholdState(ThresholdState *tS);
void resetShapeState(ShapeState *shS);

void initShapeState (PMEM    ShapeState *shS
                    );

void initParseState (PMEM     ParseState *pS
                    ,PMEM ThresholdState *tS
                    ,PMEM      TileState *tileS
                    ,                int  frameNumber
                    ,     CMEM     float *randomField
                    );

bool initTileState ( PMEM  TileState *tileS
                   ,             int  tileIndex
                   , SMEM   TileInfo *tileHeap
                   ,            int2  bitmapSize
                   ,             int  column
                   ,             int  jobIndex
                   ,             int  computeDepth
                   );

float8 sectionColor ( PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    , PMEM     ColorState *cS
                    , SMEM      Substance *substances
                    , SMEM          Shape *shapeHeap
                    ,           GEO_ENTRY  shapeStart
                    );

void calculatePixel ( PMEM      TileState *tileS
                    , PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    , PMEM     ColorState *cS
                    , SMEM         float4 *geometryHeap
                    , SMEM      Substance *substances
                    , SMEM          Shape *shapeHeap
                    );

void renderPixelBuffer ( PMEM   TileState *tileS
                       , SMEM      float4 *geometryHeap
                       , GMEM       uchar *pictureData
                       , CMEM  PictureUse *pictureRefs
                       , SMEM   Substance *substances
                       , SMEM       Shape *shapeHeap
                       ,            COLOR  backgroundColor
                       ,              int  frameNumber
                       , GMEM        uint *out
                       , CMEM       float *randomField
                       );

void initColorState ( PMEM  ColorState *init
                    ,            COLOR  backgroundColor
                    , GMEM       uchar *pictureData
                    , CMEM  PictureUse *pictureRefs
                    ,             int2  absolutePosition
                    );

void rebuildColorState ( PMEM  ThresholdState *tS
                       , PMEM      ParseState *pS
                       , PMEM      ColorState *cS
                       , SMEM       Substance *substances
                       );

void fillOutBuffer (       TileState *tileS
                   , GMEM       uint *out
                   ,           COLOR  color
                   );

// Debug Functions
void showTraversal(Traversal t);
void showTree(            Traversal t
             , SMEM       float4 *tree
             ,            int     treeSize
             ) ;
void showPixelBuffer(__local uint *pixelBuffer, int sectionWidth, int height);
void showTileSliceAlignment (int tileIndex, SMEM Slice *tileHeap);
void showShape (Shape shape);
void showSubstance(Substance substance);
void showShapeIndices(ShapeState *shS, int numShapes);
void showShapeColors(     ShapeState *shS
                   , SMEM  Substance *substances
                   , SMEM      Shape *shapeHeap
                   ,             int  numShapes
                   );
void showSubstances(SMEM Substance *substances, int numSubstances);
void showShapeRefs(SMEM REF *shapeRefs, int numShapes);
void showShapes (SMEM Shape *shapeHeap, GEO_ENTRY shapeStart, SMEM Substance *substances, int numShapes);
void showShapeStack(SHAPESTACK *shapeStack);
void showShapeAlignment (SMEM Shape *shapeHeap, GEO_ENTRY shapeStart, SMEM Substance *substances, int numShapes);
void showData (GMEM uchar *pictData, int width, int height);
void showThresholdHeader( HEADER header);
void showThresholdGeo (THRESHOLD threshold);

void showThreshold( HEADER header
                  , THRESHOLD threshold
                  );


void showThresholds (PMEM   ThresholdState *tS);
void showActiveThresholds(PMEM ThresholdState *tS, int num);

// Output for parsings by GudniTraceVisualizer
float infiniteZero(float x);
void sliceHs (Slice slice);
void shapeHs (Shape shape);
void tileInfoHs (TileInfo tileInfo);
void boxHs (int4 box);
void colorHs (COLOR color);
void colorStateHs (ColorState colorState);
void thresholdHs(int i, THRESHOLD threshold, HEADER header);
void thresholdListHs ( PMEM ThresholdState tS );
void thresholdStateHs (ThresholdState tS);
inline void shapeStackSectionHs (SHAPESTACK section);
inline void shapeIndexHs (int i, int shapeIndex);
void shapeStateHs (ShapeState shapeState);
void parseStateHs (ParseState pS);
void tileStateHs (TileState tileS);
void traversalHs(Traversal t);

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
     DEBUG_IF(printf("getPicturePixel x %i y %i w %i \n", x, y, w);)
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

inline void adjustToExclude( PMEM ThresholdState *tS
                           ,           THRESHOLD  threshold
                           ) {
    if (tBottom(threshold) > tS->renderStart.y) {
        // We don't have enough space to store the threshold,
        // so we need to trim the render area while ensuring
        // that each buildRenderArray call, makes some progress
        // (otherwise an infinite loop can occur).
        SPACE top = tTop(threshold);
        SPACE next;
        if (top > tS->renderStart.y) { // top > tS->renderStart.y
            // If top is greater than renderStart.y we can vertically trim the render area and still make progress.
            // This is the most common occurance.
            next = top;
        }
        else {
            // prevent an infinite loop by skipping to the bottom.
            next = tBottom(threshold);
        }
        tS->renderEnd.y = min(tS->renderEnd.y, next);
        //DEBUG_IF(printf("trim area nex %f \n", next);)
    }
}

int countActive ( PMEM ThresholdState *tS
                ,               float *nextTop
                ) {
    float top = tTop(getThreshold(tS, 0));
    bool notDone = true;
    int numActive = 1;
    while (notDone && numActive < tS->numThresholds) {
        THRESHOLD next = getThreshold(tS, numActive);
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

float nextSlicePoint ( PMEM ThresholdState *tS
                     ,               float  slicePoint
                     ,                 int  numActive
                     ) {
    float top = tTop(getThreshold(tS, 0));
    for (int i = 0; i < numActive; i++) {
        float bottom = tBottom(getThreshold(tS, i));
        if (top < bottom) { // it's not a horizontal threshold
            slicePoint = min(slicePoint, bottom);
        }
    }
    return slicePoint;
}

void sliceActive( PMEM ThresholdState *tS
                ,               float  slicePoint
                ,                 int  numActive
                ) {
    for (int cursor = 0; cursor < numActive; cursor++) {
        HEADER currentHeader = getHeader(tS, cursor);
        THRESHOLD current = getThreshold(tS, cursor);
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
            setHeader(tS, cursor, currentHeader);
            setThreshold(tS, cursor, current);
            if (tKeep(splitHeader,split)) {
                slotThreshold(tS, splitHeader, split);
            }
        }
    }
}

float splitNext( PMEM ThresholdState *tS
               , PMEM     ParseState *pS
               ) {
    float slicePoint = FLT_MAX;
    pS->numActive = countActive(tS, &slicePoint);
    slicePoint = min(slicePoint, nextSlicePoint(tS, slicePoint, pS->numActive));
    sliceActive(tS, slicePoint, pS->numActive);
    return slicePoint;
}


inline bool thresholdIsAbove( HEADER newHeader
                            , THRESHOLD new
                            , HEADER oldHeader
                            , THRESHOLD old) {
    return  (tTop(new) < tTop(old)) ||
            (
                (tTop(new) == tTop(old)) &&
                    (
                        (tTopX(newHeader, new) < tTopX(oldHeader, old)) ||
                        (
                            (tTopX(newHeader, new) == tTopX(oldHeader, old)) &&
                            (thresholdInvertedSlope(newHeader, new) < thresholdInvertedSlope(oldHeader, old))
                        )
                    )
            );
}

void removeLastThreshold ( PMEM ThresholdState *tS
                         ) {
    THRESHOLD last = getThreshold(tS, tS->numThresholds - 1);
    adjustToExclude(tS, last);
    tS->numThresholds -= 1;
}

bool isAboveLast( PMEM ThresholdState *tS
                  ,                   HEADER  newHeader
                  ,                THRESHOLD  new
                  ) {
    if (tS->numThresholds > 0) {
      HEADER lastHeader = getHeader(tS, tS->numThresholds - 1);
      THRESHOLD last = getThreshold(tS, tS->numThresholds - 1);
      return thresholdIsAbove(newHeader, new, lastHeader, last);
    }
    else {
      return false;
    }
}

void insertThreshold( PMEM ThresholdState *tS
                    ,                   HEADER  newHeader
                    ,                THRESHOLD  new
                    ) {
    pushTopSlot(tS);
    int cursor = 0;
    bool isAbove = false;
    while (cursor < (tS->numThresholds - 1) && !isAbove) {
        HEADER oldHeader = getHeader(tS, cursor + 1);
        THRESHOLD old = getThreshold(tS, cursor + 1);
        isAbove = thresholdIsAbove(newHeader, new, oldHeader, old);
        if (!isAbove) {
            setHeader(tS, cursor, oldHeader);
            setThreshold(tS, cursor, old);
            cursor += 1;
        }
    }
    setHeader(tS, cursor, newHeader);
    setThreshold(tS, cursor, new);
}

inline bool full(ThresholdState *tS) {
  return tS->numThresholds >= MAXTHRESHOLDS;
}

void slotThreshold ( PMEM ThresholdState *tS
                   ,              HEADER  newHeader
                   ,           THRESHOLD  new
                   ) {
    tS->slotThresholdCount += 1;
    if (full(tS) && isAboveLast(tS, newHeader, new)) {
        removeLastThreshold(tS);
    }
    if (full(tS)) {
        adjustToExclude(tS, new);
    }
    else {
        tS->thresholdWasAdded = true;
        insertThreshold(tS, newHeader, new);

    }
}

// create a threshold from a larger line segment and identifying information.
inline THRESHOLD lineToThreshold ( float2  left
                                 , float2  right
                                 ) {
    float tTop    = fmin(left.y,right.y);
    float tBottom = fmax(left.y,right.y);
    return makeThreshold(tTop, tBottom, left.x, right.x);
}

inline HEADER lineToHeader ( SHAPEBIT shapeBit
                           ,  float2 left
                           ,  float2 right
                           ) {
    bool  positiveSlope = left.y <= right.y;
    bool  notVertical = left.x != right.x;
    bool  touchingLeftBorder = left.x == LEFTBORDER;
    bool  isPersistent = notVertical && touchingLeftBorder;
    HEADER slopeBit = positiveSlope ? POSITIVE_SLOPE : NEGATIVE_SLOPE;
    HEADER persistantBit = isPersistent ? PERSIST : NONPERSIST;
    return slopeBit | persistantBit | shapeBit;
}

// determine if a threshold should be added to the stack,
// if it's header information should be pre-parsed (because it's above the render area)
// or if it should be ignored (below the render area, or a horizontal threshold that can be bypassed)
void addLineSegment ( PMEM  ThresholdState *tS
                    ,               float2  left
                    ,               float2  right
                    ,             SHAPEBIT  shapeBit
                    ,                  int  addType
                    ,                 bool *enclosedByStrand
                    ) {
    //DEBUG_IF(printf("--------------- addLineSegment %i left: %v2f right: %v2f addType: %i\n", tS->addThresholdCount, left, right, addType);)
    THRESHOLD newThreshold = lineToThreshold( left
                                            , right
                                            );
    HEADER newHeader = lineToHeader( shapeBit
                                   , left
                                   , right
                                   );
    addThreshold(tS, newHeader, newThreshold, addType, enclosedByStrand);
}

void addThreshold ( PMEM  ThresholdState *tS
                  ,               HEADER  newHeader
                  ,            THRESHOLD  newThreshold
                  ,                  int  addType
                  ,                 bool *enclosedByStrand
                  ) {
    //DEBUG_IF(printf("beg add enclosed%i add %i ", *enclosedByStrand, addType);showThreshold(newHeader, newThreshold);printf("\n");)
    //DEBUG_IF(printf("original ");showThreshold(newHeader, newThreshold);printf("\n");)
    // in the beggining the slot at position numThresholds is free, we are either at the end of the list or just picked up the top
    // threshold from the holding queue
    //DEBUG_IF(printf("addThreshold %i ", tS->addThresholdCount);showThreshold(newHeader, newThreshold);)
    if (tKeep(newHeader, newThreshold)) {
        // horizontal thresholds that have no persistance can be ignored.
        *enclosedByStrand = *enclosedByStrand ||
                             ((tTop(newThreshold)    <= tS->renderStart.y) && headerPersistTop(newHeader)   ) ||
                             ((tBottom(newThreshold) <= tS->renderStart.y) && headerPersistBottom(newHeader));
        //DEBUG_IF(printf("mid add enclosed %i add %i ", *enclosedByStrand, addType);showThreshold(newHeader, newThreshold);printf("\n");)
        if ((tTop(newThreshold) < tS->renderEnd.y) && (tBottom(newThreshold) > tS->renderStart.y) && tLeft(newThreshold) < RIGHTBORDER) {
            if (tTop(newThreshold) <= tS->renderStart.y) {
                trimThresholdTop(&newHeader,&newThreshold, tS->renderStart.y);
            }
            if (tRight(newThreshold) <= tS->renderStart.x) {
                *enclosedByStrand = true;
                //DEBUG_IF(printf(" LEFT");)
            }
            else {
                // if the threshold is entirely below the bottom of the render area is can be ignored
                // otherwise add it to the threshold array, and see if the bottom of the render area needs to be adjusted because the threshold array is not
                // large enough.g
                //if (tS->addThresholdCount > 0) {
                    //DEBUG_IF(printf("  ADD");)
                    slotThreshold(tS, newHeader, newThreshold);
                //}
                //else {
                //  DEBUG_IF(printf(" SKIP");)
                //}
            }
        }
        //else {
        //    DEBUG_IF(printf("  OUT");)
        //}
    }
    //else {
    //  DEBUG_IF(printf("  NON");)
    //}
    //DEBUG_IF(printf(" numThresholds %i start %i \n", tS->numThresholds, tS->thresholdStart);)
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
void spawnThresholds ( PMEM  ThresholdState *tS
                     ,              SHAPEBIT  shapeBit
                     ,            Traversal *l
                     ,            Traversal *r
                     ,                 bool *enclosedByStrand
                     ) {
    float y_L; // this is the y value
    if (l->travLeftX >= LEFTBORDER) { // if the left side is the left edge of the strand, don't bifurcate.
      y_L = l->travLeftY;
    }
    else { // bifurcate the curve to find the left side of the segment
      y_L = intersectCurve(*l);
    }
    bool leftWing;
    if ((l->travRightX < RIGHTBORDER) && (l->travRightX > LEFTBORDER)) {
        // add the threshold to the current state or buffer.
        addLineSegment (  tS
                       ,  (float2) (l->travXPos, y_L)
                       ,  l->travRight
                       ,  shapeBit
                       ,  0
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
        addLineSegment (  tS
                       ,  r->travLeft
                       ,  (float2) (r->travXPos, y_R)
                       ,  shapeBit
                       ,  1
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
        addLineSegment (  tS
                       ,  bridge_L
                       ,  bridge_R
                       ,  shapeBit
                       ,  2
                       ,  enclosedByStrand
                       );
    }
}

// move to the next position in the tree based on the relationship between an XPosition and the current
// node in the tree. (test is either <= or < depending on if the search is biased to the left or the right.
void searchTree( Traversal *trav
               , SMEM float4 *tree
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

bool traverseTree( SMEM    float2 *strandHeap
                 ,            int  currentSize
                 ,          Shape  shape
                 ,         float2  threadDelta
                 ,      Traversal *l
                 ,      Traversal *r
                 ) {
    int    treeSize    =  (currentSize - 4) / 2; // take the total size in 64 bit parts and subtract the header and the left and right points. Divide by 2 becasue each left+control is 128 bytes
    float4 threadDelta4  =  (float4)(threadDelta, threadDelta); // create a double vector that can be used to offset reads of two simultaneous reads
    l->travRight       = *((SMEM float2 *)(strandHeap + 1)) - threadDelta; // load the rightmost point of the strand.
    l->travLeftControl = *((SMEM float4 *)(strandHeap + 2)) - threadDelta4; // load the leftmost point of the strand and it's control point.
    SMEM float4 *tree =   (SMEM float4 *)(strandHeap + 4); // Move to tree portion of strand array.
    //DEBUG_IF(printf("shape: %i strand: %i -------------\n", shapeIndex, strandIndex);)
    //DEBUG_IF(printf("l->travLeftX: %f l->travRightX: %f \n", l->travLeftX, l->travRightX);)
    bool inRange = checkInRange(l);
    if (inRange) { // determine if the strand interacts at all with this column.
        //DEBUG_IF(showShape(shape);printf("--TREE--\n");showTree(*((float4 *)(strandHeap + 2)), *((float2 *)(strandHeap + 1)), tree, treeSize);)
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
                , SMEM  Substance *substances
                ,     SUBSTANCEID  substanceId
                ,            bool  isSolidColor
                ) {
    Substance substance = substances[substanceId];
    //DEBUG_IF(showShape(shape);)
    if (isSolidColor) {
        return substance.substanceColor;
    } else { // its a picture reference
        uint pictId = (as_uint4(substance.substanceColor)).x;
        PictureUse pRef = cS->csPictureRefs[pictId];
        int2 relativePosition = cS->absolutePosition - pRef.pictTranslate;
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

#define NULLINDEX 0xFFFFFFFFFFFFFFFF

// determine the current color based on the shape stack
COLOR determineColor( PMEM    ShapeState *shS
                    , PMEM    ColorState *cS
                    , SMEM     Substance *substances
                    , SMEM         Shape *shapeHeap
                    ,          GEO_ENTRY  shapeStart
                    ) {
    int topBit = MAXSHAPE;
    COLOR baseColor = TRANSPARENT_COLOR;
    COLOR nextColor;
    bool done = false;
    SUBSTANCEID lastId = NULLINDEX;
    bool lastIsContinue = true;
    bool lastIsSet = false;
    while (!done) {
        SUBSTANCEID substanceId = NULLINDEX;
        bool shouldComposite = true;
        topBit = findTop(shS->shapeStack, topBit);
        //DEBUG_IF(printf("topBit %i ", topBit);showShapeStack(shS->shapeStack);printf("\n");)
        if (topBit < 0) {
            nextColor = cS->csBackgroundColor;
            done = true;
            shouldComposite = true;
            lastIsSet = true;
        }
        else {
            int referenceFromBit = shS->shapeIndices[topBit];
            SHAPETAG tag = shapeHeap[referenceFromBit].shapeTag;
            substanceId = shapeTagSubstanceId(tag);
            //DEBUG_IF(printf("topBit %i substanceId %i lastId %i ",
            //                 topBit,   substanceId,   lastId   );)
            if (substanceId == lastId) {
                if (lastIsContinue) {
                    if (!shapeTagIsContinue(tag)) {
                        lastIsSet = shapeTagIsAdd(tag);
                        lastIsContinue = false;
                    }
                    else {
                        lastIsSet = !lastIsSet;
                    }
                }
                shouldComposite = false;
            }
            if (substanceId != lastId) {
                nextColor = readColor ( cS
                                      , substances
                                      , substanceId
                                      , shapeTagIsSolidColor(tag)
                                      );
                shouldComposite = true;
                lastIsSet = shapeTagIsAdd(tag) || shapeTagIsContinue(tag);
            }
            lastId = substanceId;

        }
        //DEBUG_IF(printf("done %i lastIsContinue %i lastIsSet %i shouldComposite %i baseColor %2.2v4f nextColor %2.2v4f \n",
        //                 done,   lastIsContinue,   lastIsSet,   shouldComposite,   baseColor,        nextColor          );)
        if (shouldComposite) {
            if (lastIsSet) {
                baseColor = composite(baseColor, nextColor);
                if (OPAQUE(baseColor)) {
                   done = true;
                }
            }
        }
    }
    return baseColor;
}

inline void passHeader( PMEM ShapeState *shS
                      ,          HEADER  header
                      ) {
    flipBit(headerShapeBit(header), shS->shapeStack);
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
void buildThresholdArray ( PMEM  ThresholdState *tS
                         , PMEM      ShapeState *shS
                         , SMEM          float4 *geometryHeap
                         , SMEM       Substance *substances
                         , SMEM           Shape *shapeHeap
                         ,            GEO_ENTRY  shapeStart
                         ,            GEO_ENTRY  numShapes
                         ,               float2  threadDelta
                         ) {
    for (GEO_ENTRY n = 0; n < numShapes && shS->shapeBits < MAXSHAPE; n++) { // iterate over every shape in the current shape.
        GEO_ENTRY shapeIndex = shapeStart + n;
        tS->thresholdWasAdded = false;
        Shape shape = shapeHeap[shapeIndex]; // get the current shape.
         // if you don't shift the shape to the tile size there will be accuracy errors with height floating point geometric values
        SMEM float2 *strandHeap = (SMEM float2 *)&geometryHeap[getGeometryStart(shape)];
        bool enclosedByShape = false;
        for (int currentStrand = 0; currentStrand < getNumStrands(shape); currentStrand++) {
            uchar4 header = *((SMEM uchar4 *)strandHeap);
            ushort currentSize = as_ushort(header.xy); // size of current strand being parsed.
            bool enclosedByStrand = false; // is the start of the render area inside of the shape.
            Traversal left;
            Traversal right;
            // search the tree on the left and right sides of the pixel (or strand) and return 2 traversal result structures.
            bool inRange = traverseTree(  strandHeap
                                       ,  currentSize
                                       ,  shape
                                       ,  threadDelta
                                       , &left
                                       , &right
                                       );
            if (inRange) {
                spawnThresholds (  tS
                                ,  shS->shapeBits
                                , &left
                                , &right
                                , &enclosedByStrand
                                );
            }
            strandHeap += currentSize;
            enclosedByShape = enclosedByShape != enclosedByStrand; // using not equal as exclusive or.
        } // for currentStrand
        if (enclosedByShape) {
            passHeader(shS, shS->shapeBits);
        }
        if (tS->thresholdWasAdded || enclosedByShape) {
            if (shS->shapeBits < MAXSHAPE) {
               shS->shapeIndices[shS->shapeBits] = shapeIndex;
            }
            shS->shapeBits += 1;
        }
    }
}

void resetParser ( PMEM ParseState *pS
                 ) {
    pS->currentThreshold = 0;
    pS->numActive        = 0; // the next threshold that is not currently active.
}

void nextRenderArea ( PMEM ThresholdState *tS
                    , PMEM ParseState     *pS
                    ,               SPACE  height
                    ) {
    tS->renderStart.y = tS->renderEnd.y;
    tS->renderEnd.y = height;
    tS->renderStart.x  = tS->renderEnd.x != RIGHTBORDER ? tS->renderEnd.x : LEFTBORDER;  // finish horizontal sweep.
    tS->renderEnd.x    = RIGHTBORDER;
}

void initRandomField( ParseState *pS
                    , TileState *tileS
                    , CMEM float *randomField) {
  // find a random starting point in the field passed on the absolute start position of the column.
  int start = tileS->threadUnique & RANDOMFIELDMASK;
  pS->randomFieldCursor = (as_uint(randomField[start]) & RANDOMFIELDMASK);
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

void initThresholdState(ThresholdState *tS) {
    tS->renderStart  = (SPACE2)(LEFTBORDER,0);
    tS->renderEnd    = (SPACE2)(RIGHTBORDER,0); // the lowest vertical position that can be properly rendered with the current list of thresholds.
    resetThresholdState(tS);
}

void resetThresholdState(ThresholdState *tS) {
    tS->numThresholds = 0;
    tS->thresholdStart = MAXTHRESHOLDS;
    tS->slotThresholdCount = 0;
    tS->thresholdWasAdded = false;
    tS->addThresholdCount = INT_MAX; // maximum thresholds to add before stopping (for debugging).
}

void resetShapeState(ShapeState *shS) {
  shS->shapeBits = 0;
  clearShapeStack(shS->shapeStack);
}

void initShapeState (PMEM    ShapeState *shS
                    ) {
    resetShapeState(shS);
}


void initParseState (PMEM     ParseState *pS
                    ,PMEM ThresholdState *tS
                    ,PMEM    TileState  *tileS
                    ,               int  frameNumber
                    ,    CMEM     float *randomField
                    ) {
    resetParser(pS);
    pS->accColorArea = (float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0));
    // if we go below render bottom we must rebuild the threshold list.
    pS->sectionStart = (SPACE2)(LEFTBORDER,0); // the top of the current vertical section being processed
    pS->sectionEnd   = (SPACE2)(tS->renderEnd.x, tS->renderEnd.y); // the bottom of the current vertical section being processed
    pS->sectionCount = 0;

    pS->frameNumber = frameNumber;
    pS->buildCount = 0;
    initRandomField(pS,tileS,randomField);
    DEBUG_TRACE_BEGIN
}

inline int bitmaskN(int n) {return (1 << n) - 1;}

bool initTileState ( PMEM  TileState *tileS
                   ,             int  tileIndex
                   , SMEM   TileInfo *tileHeap
                   ,            int2  bitmapSize
                   ,             int  column
                   ,             int  jobIndex
                   ,             int  computeDepth
                   ) {
    TileInfo tileInfo    = tileHeap[tileIndex];
    tileS->tileShapeStart = tileInfo.tileShapeSlice.sStart;
    tileS->tileNumShapes = tileInfo.tileShapeSlice.sLength;
    int hDepth = tileInfo.tileHDepth;
    int vDepth = tileInfo.tileVDepth;
    int diffDepth = max(0, vDepth - (computeDepth - hDepth));
    int desiredHeight = 1 << diffDepth;
    tileS->tileSize.x    = boxRight(tileInfo.tileBox)  - boxLeft(tileInfo.tileBox);
    tileS->tileSize.y    = boxBottom(tileInfo.tileBox) - boxTop(tileInfo.tileBox);
    tileS->bitmapSize    = bitmapSize;
    int2 internalDelta = (int2)(bitmaskN(hDepth) & column, (column >> hDepth) << diffDepth);
    tileS->threadDelta   = internalDelta + boxLeftTop(tileInfo.tileBox); // the threadDelta is the internal delta + the topleft corner of the tileBox.
    tileS->tileIndex     = tileIndex;
    tileS->column        = column;
    tileS->threadUnique  = (column + tileIndex * (1 << computeDepth) + jobIndex * (1 << (computeDepth + computeDepth))) * LARGE_PRIME; // unique integer for the thread within the group.
    tileS->intHeight     = min( desiredHeight, tileS->bitmapSize.y-tileS->threadDelta.y);
    tileS->floatHeight   = convert_float( tileS->intHeight);
    //DEBUG_IF(printf("column %i computeDepth %i hDepth %i vDepth %i diffDepth %i x %i y %i height %i floatHeight %f \n"\
    //        , column,   computeDepth,   hDepth,   vDepth, diffDepth, tileS->threadDelta.x, tileS->threadDelta.y, desiredHeight,tileS->floatHeight);)
    return (internalDelta.y < tileS->tileSize.y) && (tileS->threadDelta.x < tileS->bitmapSize.x) && (tileS->threadDelta.y < tileS->bitmapSize.y);
}

float8 sectionColor ( PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    , PMEM     ColorState *cS
                    , SMEM      Substance *substances
                    , SMEM          Shape *shapeHeap
                    ,           GEO_ENTRY  shapeStart
                    ) {
    COLOR color = determineColor( shS
                                , cS
                                , substances
                                , shapeHeap
                                , shapeStart
                                );
    float random = getRandom(pS);
    float area = (pS->sectionEnd.x - pS->sectionStart.x) * (pS->sectionEnd.y - pS->sectionStart.y);
    float4 adjustedArea = (float4) (area + (area * random * STOCHASTIC_FACTOR));
    return (float8)(color * adjustedArea, adjustedArea);
}

void verticalAdvance( PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    ) {
    if (pS->sectionEnd.x == tS->renderEnd.x) {
        //DEBUG_IF(printf("---------- Vertical Advance -------------- \n");)
        // Start by undoing all of the state changes from the horizontal traversal.
        // Occasionally a threshold gets skipped because they are out of order.
        // pass these.
        // Revert all horizontal border crossing from the lass vertical advances.
        //DEBUG_IF(printf("rev->");)
        for (int i = 0; i < pS->numActive; i++) {
            //DEBUG_IF(printf("back %i %i\n",i,headerShapeBit(getHeader(tS, i)));)
            passHeader(shS, getHeader(tS, i));
        }
        //DEBUG_IF(printf("<-rev");)
        // Next break is the next natural break point (either the bottom of the pixel or the bottom of the render area.
        float nextBreak = min(tS->renderEnd.y, pS->pixelY);
        // The activeBottom is the bottom of the current group of horizontally adjacent thresholds or max float if none are left.
        float activeBottom = tS->numThresholds > 0 ? tBottom(getThreshold(tS, 0)) : FLT_MAX;
        // If the last section ended at the bottom of the current group.
        if (activeBottom == pS->sectionEnd.y) {
            // then pass over the bottom of all the active thresholds.
            //DEBUG_IF(printf("pS->numActive %i\n", pS->numActive);)
            while(pS->numActive > 0) {
                //DEBUG_IF(if (headerPersistBottom(getHeader(tS, i))) {printf("bott %i %i\n",i,headerShapeBit(getHeader(tS, i)));})
                passHeaderBottom(shS, getHeader(tS, 0));
                popTop(tS);
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
            float nextTop = pS->numActive < tS->numThresholds ? tTop(getThreshold(tS, pS->numActive)) : FLT_MAX;
            // nextBottom is the bottom of the next group of horizontally adjacent thresholds or the top of it depending on if it starts immediately.
            if (nextTop > pS->sectionEnd.y) {
                // there is a vertical gap. active thresholds stays empty and we add the gap to the accumulator.
                nextBottom = min(nextBreak, nextTop);
            }
            else {
                // there is no gap
                // extend active thresholds to include anything adjacent to the current section.
                nextBottom = min(nextBreak,splitNext(tS,pS));
                //DEBUG_IF(printf("splitNext \n");showThresholds(tS);)
                // pass over any leading horizontal thresholds (non persistance horizontal are never added).
                while (pS->numActive > 0 && tIsHorizontal(getThreshold(tS, 0))) {
                    //DEBUG_IF(if (headerPersistTop(getHeader(tS, pS->numActive))) {printf("topH %i %i\n",pS->numActive,headerShapeBit(getHeader(tS, pS->numActive)));})
                    passHeaderTop(shS, getHeader(tS, 0));
                    popTop(tS);
                    pS->numActive -= 1;
                }
                for (int i = 0; i < pS->numActive; i++) {
                    if (tTop(getThreshold(tS, i)) > tS->renderStart.y) { // TODO: Can probably get rid of this check.
                        //DEBUG_IF(if (headerPersistTop(getHeader(tS, i))) {printf("top  %i %i\n", i, headerShapeBit(getHeader(tS, i)));})
                        passHeaderTop(shS, getHeader(tS, i));
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
        //DEBUG_IF(showShapeStack(shS->shapeStack); printf("  ");
        //         showActiveThresholds(tS,pS->numActive);)

    }
}

void horizontalAdvance( PMEM ThresholdState *tS
                      , PMEM     ParseState *pS
                      ) {
    // find the right side of the current section.
    SPACE nextX;
    //DEBUG_IF(printf("do ..  pS->sectionEnd %v2f nextX %f\n", pS->sectionEnd, nextX);)
    if (pS->currentThreshold < pS->numActive) {
        //DEBUG_IF(printf("pS->currentThreshold < pS->numActive\n");)
        // find the midpoint of the threshold when bound by the section
        nextX = thresholdMidXLow( getThreshold(tS, pS->currentThreshold)
                                , getHeader(tS, pS->currentThreshold)
                                , pS->sectionStart.y
                                , pS->sectionEnd.y
                                , LEFTBORDER
                                , RIGHTBORDER
                                );
        //DEBUG_IF(printf("midX %f\n", nextX);)
    }
    else {
        if (pS->sectionEnd.y == tS->renderEnd.y) {
            //DEBUG_IF(printf("pS->sectionEnd.y == tS->renderEnd.y: nextX = tS->renderEnd.x\n");)
            nextX = tS->renderEnd.x; // unless we can't render the entire horizontal section, this will be RIGHTBORDER.
        }
        else {
            //DEBUG_IF(printf("else: nextX = RIGHTBORDER\n");)
            nextX = RIGHTBORDER;
        }
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
                               showShapeStack(shS->shapeStack); \
                               printf("\n"); \
                               )

// "Who creates an engine that can properly render an image with more than 128 horizontal thresholds in
// one pixel?" You might ask. And we reply "We do, that's who."

void calculatePixel ( PMEM      TileState *tileS
                    , PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    , PMEM     ShapeState *shS
                    , PMEM     ColorState *cS
                    , SMEM         float4 *geometryHeap
                    , SMEM      Substance *substances
                    , SMEM          Shape *shapeHeap
                    ) {
    //DEBUG_IF(printf("                                              pixelY: %f \n", pS->pixelY);)
    bool done = false;
    int count = 100;
    while (((pS->sectionEnd.x < RIGHTBORDER) || (pS->sectionEnd.y < pS->pixelY)) && !done /*&& count > 0*/) { // process all sections that do not reach the bottom of the pixel.
        count -= 1;
        DEBUG_IF(printf("loop        sectionStart %v2f sectionEnd %v2f \n", pS->sectionStart, pS->sectionEnd);)
        //DEBUG_IF(printf("loop        renderStart  %v2f renderEnd  %v2f \n", pS->renderStart,  pS->renderEnd );)
        if (pS->sectionEnd.x == tS->renderEnd.x && pS->sectionEnd.y == tS->renderEnd.y) {
                // If the section bottom is below the area we can properly render.
                // Rebuild the threshold list and reset the processing state.
                // TODO: This should really happen if ANY thread reaches the end of the render area. Otherwise different threads could trigger
                // a rebuild out of sync.
                //DEBUG_IF(printf("============== buildThresholdArray ===============\n");)
                if (pS->buildCount < MAXBUILDS) {
                    resetParser(pS);
                    nextRenderArea(tS, pS, tileS->floatHeight);
                    resetThresholdState(tS);
                    resetShapeState(shS);
                    buildThresholdArray( tS
                                       , shS
                                       , geometryHeap
                                       , substances
                                       , shapeHeap
                                       , tileS->tileShapeStart
                                       , tileS->tileNumShapes
                                       , convert_float2(tileS->threadDelta)
                                       );
                    pS->buildCount += 1;
                }
                else {
                    done = true;
                }
                DEBUG_IF(showShapeStack(shS->shapeStack);)
                DEBUG_IF(showThresholds(tS);)
                DEBUG_TRACE_ITEM(thresholdStateHs(*tS);)
                //DEBUG_TRACE_ITEM(shapeStateHs(*shS);)
            pS->sectionStart = tS->renderStart;
            pS->sectionStart.x = tS->renderStart.x;
            pS->sectionStart.y = tS->renderStart.y;
            pS->sectionEnd.x   = tS->renderEnd.x;
            pS->sectionEnd.y   = tS->renderStart.y;

        }
        //done = true; // this is for debugging only.
        if (!done) {
            //DEBUG_IF(printf("beforeV cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
            verticalAdvance(tS, pS, shS);
            //DEBUG_IF(printf("afterV  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
            horizontalAdvance(tS, pS);
            //DEBUG_IF(printf("afterH  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
            float8 colorArea = sectionColor( pS
                                           , shS
                                           , cS
                                           , substances
                                           , shapeHeap
                                           , tileS->tileShapeStart
                                           );
            pS->accColorArea += colorArea;
            DEBUG_IF(printf("accColor %v8f", pS->accColorArea);)
            DEBUG_TRACE_ITEM(parseStateHs(*pS);)
            if (pS->currentThreshold < pS->numActive) {
                //DEBUG_IF(printf("pass %i %i\n",pS->currentThreshold,headerShapeBit(getHeader(tS, pS->currentThreshold)));)
                passHeader(shS, getHeader(tS, pS->currentThreshold));
            }
            pS->currentThreshold += 1;
            pS->sectionCount += 1;
            //DEBUG_IF(printf("atEnd  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
        }
    } // while (((pS->sectionEnd.x < RIGHTBORDER) || (pS->sectionEnd.y < pS->pixelY)))
    //DEBUG_IF(printf("pixelDone\n");)=
}

// create an initial color state.
void initColorState( PMEM   ColorState *init
                   ,             COLOR  backgroundColor
                   , GMEM        uchar *pictureData
                   , CMEM   PictureUse *pictureRefs
                   ,              int2  pos
                   ) {
  init->csBackgroundColor = backgroundColor;
  init->csPictureData = pictureData;
  init->csPictureRefs = pictureRefs;
  init->absolutePosition = pos;
}

void renderPixelBuffer ( PMEM   TileState *tileS
                       , SMEM      float4 *geometryHeap
                       , GMEM       uchar *pictureData
                       , CMEM  PictureUse *pictureRefs
                       , SMEM   Substance *substances
                       , SMEM       Shape *shapeHeap
                       ,            COLOR  backgroundColor
                       ,              int  frameNumber
                       , GMEM        uint *out
                       ,CMEM        float *randomField
                       ) {
    //DEBUG_IF(printf("INDEX %i gTileIndex %i numShapes %i \n", INDEX, gTileIndex, tileS->numShapes);)
    //barrier (CLK_LOCAL_MEM_FENCE);
    ThresholdState tS;
    initThresholdState(&tS);
    ShapeState shS;
    initShapeState(&shS);
    ParseState pS;
    initParseState(&pS, &tS, tileS, frameNumber, randomField);
    ColorState cS;
    initColorState( &cS
                  ,  backgroundColor
                  ,  pictureData
                  ,  pictureRefs
                  ,  tileS->threadDelta
                  );
    int yInt = -1;
    for (pS.pixelY = 1.0f; pS.pixelY <= tileS->floatHeight; pS.pixelY += PIXELHEIGHT) { // y is the bottom of the current pixel.
        yInt += 1;
        calculatePixel (  tileS
                       , &tS
                       , &pS
                       , &shS
                       , &cS
                       ,  geometryHeap
                       ,  substances
                       ,  shapeHeap
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
    //DEBUG_IF(printf("slotThresholdCount = %i\n",pS.slotThresholdCount);)
}

__kernel void multiTileRaster ( SMEM     float4 *geometryHeap
                              , SMEM  Substance *substanceHeap
                              , GMEM      uchar *pictureData
                              , CMEM PictureUse *pictureRefs
                              , CMEM      float *randomField
                              , SMEM      Shape *shapeHeap
                              , SMEM   TileInfo *tileHeap
                              ,           COLOR  backgroundColor
                              ,            int2  bitmapSize
                              ,             int  computeDepth
                              ,             int  frameNumber
                              ,             int  jobIndex
                              , GMEM        uint *out
                              ) {
    int   tileIndex  = INDEX; // the sequential number of the tile in the current workgroup.
    int   column     = COLUMN;
    TileState tileS;
    bool threadActive = initTileState ( &tileS
                                      ,  tileIndex
                                      ,  tileHeap
                                      ,  bitmapSize
                                      ,  column
                                      ,  jobIndex
                                      ,  computeDepth
                                      );
    //testShapeStack();
    //testDeleteBit();
    //DEBUG_IF(showShapes(shapeHeap, tileS.tileShapeStart, substanceHeap, tileS.tileNumShapes);)
    if (threadActive && tileS.tileNumShapes == 0) {
        fillOutBuffer (&tileS
                      , out
                      , backgroundColor
                      );
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    if (threadActive && tileS.tileNumShapes > 0) {
        renderPixelBuffer ( &tileS
                          ,  geometryHeap
                          ,  pictureData
                          ,  pictureRefs
                          ,  substanceHeap
                          ,  shapeHeap
                          ,  backgroundColor
                          ,  frameNumber
                          ,  out
                          ,  randomField
                          );
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
}

// when there are no shapes in the tile fill it with the background color.
void fillOutBuffer ( PMEM TileState *tileS
                   , GMEM      uint *out
                   ,          COLOR  color
                   ) {
    uint pixel = colorToSolidPixel_Word32_BGRA(color);
    int outPos = mul24(tileS->threadDelta.y, tileS->bitmapSize.x) + tileS->threadDelta.x;
    for (int y = 0; y < tileS->intHeight; y++) {
        out[outPos] = pixel;
        outPos += tileS->bitmapSize.x;
    }
}


//////////////////////////////////////////////////////////////////////////////////////////////
// Functions for Exporting Debugging Information in human readable forms.
//////////////////////////////////////////////////////////////////////////////////////////////

void showTraversal(Traversal t) {
  printf("travLeft %v2f travControl %v2f travRight %v2f travXPos %f travIndex %i\n"
       ,t.travLeft   ,t.travControl   ,t.travRight   ,t.travXPos ,t.travIndex     );
}

void showTree(            Traversal t
             , SMEM       float4 *tree
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
    printf(" sIx:%03i ", headerShapeBit(header));
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

void showThresholds (PMEM ThresholdState *tS) {
    printf ("Thresholds numThresholds %2i \n", tS->numThresholds);
    for (int t = 0; t < tS->numThresholds; t++) {
        if (t < MAXTHRESHOLDS) {
        printf("t: %2i ", t);
        showThreshold( getHeader(tS, t)
                     , getThreshold(tS, t)
                     );
        printf("\n");
        }
        else {printf("out of bounds\n\n\n");}
    }
}

void showActiveThresholds(PMEM ThresholdState *tS, int num) {
    printf ("------Thresholds numThresholds %2i------- \n", num);
    for (int t = 0; t < num; t++) {
      printf("t: %2i ", t);
      showThreshold( getHeader(tS, t)
                   , getThreshold(tS, t));
      printf("\n");
    }
}

void showTileSliceAlignment (int gTileIndex, SMEM Slice *tileHeap) {
    //printf ("%i :            threadDelta %2i %2hi,%2hi\n", gTileIndex, (long)&tileHeap[gTileIndex].threadDelta - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].threadDelta.x / FIXEDFACTORINT, tileHeap[gTileIndex].threadDelta.y / FIXEDFACTORINT);
    //printf ("%i :     tileMaxThresholds %2i %i   \n", gTileIndex, (long)&tileHeap[gTileIndex].tileMaxThresholds     - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].tileMaxThresholds     );
    printf ("%i : tileShapeSlice.sStart  %2i %hi    \n", gTileIndex, (long)&tileHeap[gTileIndex].sStart  - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex+1].sStart  );
    printf ("%i : tileShapeSlice.sLength %2i %hi    \n", gTileIndex, (long)&tileHeap[gTileIndex].sLength - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex+1].sLength );
    printf ("%i :              alignment %2i        \n", gTileIndex, (long)&tileHeap[gTileIndex+1]       - (long)&tileHeap[gTileIndex]);
}

void showSubstance(Substance substance) {
    printf ("     substanceColor %2i  %2.2v4f \n", (long) &substance.substanceColor - (long)&substance, substance.substanceColor);
}

void showShapeIndices( ShapeState *shS
                     ,        int  numShapes
                     ) {
    int num = min(numShapes, MAXSHAPE);
    printf("Substance indices %i\n", num);
    for (int i = 0; i < num; i++) {
        printf("        [%02i] -> %02i\n", i, shS->shapeIndices[i]);
    }
}

void showShapeColors(     ShapeState *shS
                   , SMEM  Substance *substances
                   , SMEM      Shape *shapeHeap
                   ,             int  numShapes
                   ) {
    int num = min(numShapes, MAXSHAPE);
    printf("Substance indices %i\n", num);
    for (int i = 0; i < num; i++) {
        Shape shape = shapeHeap[shS->shapeIndices[i]];
        SHAPETAG tag = shape.shapeTag;
        SUBSTANCEID substanceId = shapeTagSubstanceId(tag);
        Substance substance = substances[substanceId];
        COLOR color = substance.substanceColor;
        printf("        [%02i] -> %02i (%2.2v4f)\n", i, shS->shapeIndices[i], color);
    }
}


void showSubstances(SMEM Substance *substances, int numSubstances) {
  printf("substances: num = %i\n", numSubstances);
  for (int n = 0; n < numSubstances; n++) {
    printf(" %i\n", n);
    showSubstance(substances[n]);
    //printf ("      alignment %2i \n", (long) &substances[n+1] - (long)&substances[n]);
  }
}

void showShapeRefs(SMEM REF *shapeRefs, int numShapes) {
  printf("shapeRefs: numShapes = %i\n", numShapes);
  for (int n = 0; n < numShapes; n ++ ) {
      printf ("shapeRef %2i:%2i\n", n, shapeRefs[n]);
    }
}

void showShapes(SMEM Shape *shapeHeap, GEO_ENTRY shapeStart, SMEM Substance *substances, int numShapes) {
    for (int n = 0; n < numShapes; n ++ ) {
        Shape shape = shapeHeap[shapeStart + n];
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

void showShapeStack(SHAPESTACK *shapeStack) {
  for (int i = SHAPESTACKSECTIONS - 1; i >= 0; i--) {
    printf(" %016lX", shapeStack[i]);
  }
}

void showShape(Shape shape) {
    printf (" shapeTag %2i %lX \n"    , (long) &shape.shapeTag      - (long)&shape, shape.shapeTag      );
    printf ("  shapeSlice.s %2i %i \n"     , (long) &shape.shapeSlice.sStart  - (long)&shape, shape.shapeSlice.sStart  );
    printf ("  shapeSlice.l %2i %i \n"     , (long) &shape.shapeSlice.sLength - (long)&shape, shape.shapeSlice.sLength );
}

void showShapeAlignment (SMEM Shape *shapeHeap, GEO_ENTRY shapeStart, SMEM Substance *substances, int numShapes) {
    printf("Shape Alignment: numShapes = %i\n", numShapes);
    for (int n = 0; n < numShapes; n ++ ) {
        printf ("%2i ----------------------------------------------------------------\n",n );
        Shape shape = shapeHeap[shapeStart + n];
        showShape(shape);
        printf ("       alignment %2i \n"        , (long) &shapeHeap[shapeStart + n + 1]                 - (long)&shapeHeap[shapeStart + n] );
        showSubstance(substances[shapeTagSubstanceId(shape.shapeTag)]);
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

void shapeHs (Shape shape) {
    printf("ShapeHs\n");
    printf("{ shapeTag = %u\n", shape.shapeTag);
    printf(", ");sliceHs(shape.shapeSlice);
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
    printf(", csIsConstant = %i\n", colorState.csIsConstant);
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
  printf(", thrShapeIndex    = %i\n", headerShapeBit(header)           );
  printf("}\n");
}

void thresholdListHs ( PMEM ThresholdState tS ) {
  for (int t = 0; t < tS.numThresholds; t++) {
      if (t==0) {printf ("[");} else {printf (",");}
      thresholdHs(t, getThreshold(&tS, t), getHeader(&tS, t));
      printf ("\n");
  }
  printf("]");
}

void thresholdStateHs (ThresholdState tS) {
    printf("ThresholdStateHs\n");
    printf("{ thresholds = "); thresholdListHs(tS); printf("\n");
    //printf(", thresholdStart = %i\n", tS.thresholdStart);
    //printf(", numThresholds = %i\n", tS.numThresholds);
    printf(", renderStart = (%f, %f)\n", tS.renderStart.x, tS.renderStart.y);
    printf(", renderEnd = (%f, %f)\n", tS.renderEnd.x, tS.renderEnd.y);
    //printf(", thresholdWasAdded  = %i\n", tS.thresholdWasAdded);
    //printf(", slotThresholdCount = %i\n", tS.slotThresholdCount);
    //printf(", addThresholdCount  = %i\n", tS.addThresholdCount);
    printf("}\n");
}

inline void shapeStackSectionHs (SHAPESTACK section) {
  printf(" %lu\n", section);
}

inline void shapeIndexHs (int i, int shapeIndex) {
  printf("(%i,%i)\n", i, shapeIndex);
}

void shapeStateHs (ShapeState shapeState) {
      int num = min((int)shapeState.shapeBits, MAXSHAPE);
      printf("ShapeStateHs\n");
      printf("{ shapeBits = %i\n", num);
      printf(", shapeIndices = \n");
      printf("[ ");shapeIndexHs(0,shapeState.shapeIndices[0]);
      for (int i = 1; i < num; i++) {
          printf(", ");shapeIndexHs(i,shapeState.shapeIndices[i]);
      }
      printf("]\n");
      printf(", shapeStack = \n");
      printf("[");shapeStackSectionHs(shapeState.shapeStack[SHAPESTACKSECTIONS - 1]);
      for (int i = SHAPESTACKSECTIONS - 2; i >= 0; i--) {
          printf(",");shapeStackSectionHs(shapeState.shapeStack[i]);
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
    printf("{ tileShapeStart = %i\n",   tileS.tileShapeStart);
    printf(", tileNumShapes = %i\n",    tileS.tileNumShapes);
    printf(", tileSize = %i\n",         tileS.tileSize);
    printf(", bitmapSize = (%i,%i)\n",  tileS.bitmapSize.x,  tileS.bitmapSize.y);
    printf(", threadDelta = (%i,%i)\n", tileS.threadDelta.x, tileS.threadDelta.y);
    printf(", tileIndex = %i\n",        tileS.tileIndex);
    printf(", intHeight = %i\n",        tileS.intHeight);
    printf(", floatHeight = %i\n",      tileS.floatHeight);
    printf(", threadUnique = %i\n",     tileS.threadUnique);
    printf(", column = %i\n",           tileS.column);
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
