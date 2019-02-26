// ---------------- Macros, Type definitions and type accessors -----------------------------------

#define STOCHASTIC_FACTOR 0.3f // relative amount of variability in an edge.
#define MAXTHRESHOLDS 512 // the size of the threshold header and threshold geometry buffers (must be a power of 2)
#define MAXTHRESHOLDMASK 511 // = MAXTHRESHOLDS - 1
#define MAXSHAPE 511 // total number of shapes per build. must be one less than the number of bits available.
#define MAXBUILDS 1 // number of times to rebuild the list of thresholds in one kernel.
#define COLORBUFFERSIZE 4
#define MINCROP 0.2f
// Debugging
#define DEBUGCOLUMN 0 // Determines the column for DEBUG_IF macro
#define DEBUGINDEX  0 // Determines the index for DEBUG_IF macro
#define INDEX get_global_id(0)
#define COLUMN get_global_id(1)
#define DEBUG_IF(statement)   //if (COLUMN == DEBUGCOLUMN && INDEX == DEBUGINDEX) {statement} // useful for tracing a specific tile and column
#define DEBUG_HS(statement)   // if (pS->debugEnabled) {statement}
#define DEBUG_HSP(statement)  // if (pS.debugEnabled) {statement}

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
// The shape tag includes 4 bit combination style value, the 4 bit texture type and the 28 bit group identifier.
#define SHAPETAG ulong
// A group id is the reference to a group of shapes that share color/texture information.
#define GROUPID ulong
// A shape bit is the number of each shape assigned as it is added to a column, each shape number corresponds to a bit in the shape stack
// so the number of possible shape bits is limited to the size (in bits) of the shape stack.
#define SHAPEBIT    uint

// Is Compound determines if the shape is a complex compound shape. If it is zero the shape is assumed to be made up of continuation
// shapes.

// Bits 31 - 30
#define SHAPETAG_SUBSTANCETYPE_BITMASK    0xC000000000000000
#define SHAPETAG_SUBSTANCETYPE_SOLIDCOLOR 0x8000000000000000
#define SHAPETAG_SUBSTANCETYPE_PICTURE    0x4000000000000000
#define SHAPETAG_SUBSTANCETYPE_SHIFT      30

inline bool     shapeTagIsSolidColor(SHAPETAG tag)  {return (tag & SHAPETAG_SUBSTANCETYPE_BITMASK) == SHAPETAG_SUBSTANCETYPE_SOLIDCOLOR;}
inline SHAPETAG shapeTagSubstanceType(SHAPETAG tag) {return (tag & SHAPETAG_SUBSTANCETYPE_BITMASK) >> SHAPETAG_SUBSTANCETYPE_SHIFT;}

// Bits 29 - 28
#define SHAPETAG_COMBINETYPE_BITMASK      0x3000000000000000
#define SHAPETAG_COMBINETYPE_CONTINUE     0x1000000000000000
#define SHAPETAG_COMBINETYPE_ADD          0x2000000000000000
#define SHAPETAG_COMBINETYPE_SUBTRACT     0x3000000000000000
#define SHAPETAG_COMBINETYPE_SHIFT        28

inline int  shapeTagCombineType(SHAPETAG tag) {return (tag & SHAPETAG_COMBINETYPE_BITMASK) >> SHAPETAG_COMBINETYPE_SHIFT;   }
inline bool shapeTagIsAdd(SHAPETAG tag)       {return (tag & SHAPETAG_COMBINETYPE_BITMASK) == SHAPETAG_COMBINETYPE_ADD;     }
inline bool shapeTagIsSubtract(SHAPETAG tag)  {return (tag & SHAPETAG_COMBINETYPE_BITMASK) == SHAPETAG_COMBINETYPE_SUBTRACT;}
inline bool shapeTagIsContinue(SHAPETAG tag)  {return (tag & SHAPETAG_COMBINETYPE_BITMASK) == SHAPETAG_COMBINETYPE_CONTINUE;}

// Bits 27 - 0
#define SHAPETAG_GROUPID_BITMASK          0x0FFFFFFFFFFFFFFF

inline  GROUPID shapeTagGroupId(SHAPETAG tag) {return (tag & SHAPETAG_GROUPID_BITMASK);}

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

#define POSITIVE_SLOPE_MASK     0x80000000 // leftmost bit
#define PERSIST_AND_SLOPE_MASK  0xC0000000 // leftmost and second to leftmost bit
#define PERSIST_TOP             0xC0000000 // positive slope and persist
#define PERSIST_BOTTOM          0x40000000 // not positive slopw and persist

#define POSITIVE_SLOPE          0x80000000
#define NEGATIVE_SLOPE          0x00000000

#define PERSIST                 0x40000000 // isolate the persist bit
#define NONPERSIST              0x00000000 // just zero
#define UNPERSISTMASK           0xBFFFFFFF // everything but the persist bit

#define THRESHOLDENABLE         0x20000000 // determine if the threshold has been deactivated by brushes.
#define THRESHOLDDISABLE        0x00000000
#define DISABLEMASK             0xDFFFFFFF // AND with a header to disable the header.

#define SHAPEBIT_MASK           0x0FFFFFFF // right 29 bits including the brushmode
#define WITHOUT_SHAPEBIT        0xF0000000 // bits without shape index

// & has a lower precedence than !=
inline       bool headerPositiveSlope(HEADER h) {return (h & POSITIVE_SLOPE_MASK) != 0;} // determine if the threshold has a positive slope
inline       bool headerPersistTop   (HEADER h) {return (h & PERSIST_AND_SLOPE_MASK) == PERSIST_TOP;   } // determine if the top of the threshold affects the persistant state of the shapestack
inline       bool headerPersistBottom(HEADER h) {return (h & PERSIST_AND_SLOPE_MASK) == PERSIST_BOTTOM;} // determine if the bottom of the threshold affects the persistant state of the shapestack
inline       bool headerPersistEither(HEADER h) {return (h & PERSIST) != 0;  } // determine if either top or bottom of the threshold affects the persistant state of the shapestack
inline   SHAPEBIT headerShapeBit     (HEADER h) {return  h & SHAPEBIT_MASK;} // get the index of the shape that corresponds to the threshold

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

#define SHAPESHACK             ulong
#define SHAPESHACKBITS         64
#define SHAPESHACKCARRYSHIFT   63
#define SHAPESHACKCARRYMASK    0x1
#define SHAPESHACKSECTIONSHIFT 6    // the amount to shift to get the section from the total bits
#define SHAPESHACKSECTIONBITS  0x3F
#define SHAPESHACKSECTIONS     8

#define EMPTY_SHAPESHACK       0x0
#define COMPLETE_MASK         0xFFFFFFFFFFFFFFFF
// The shape index of top shape (the visible one) can be determined by counting the leading zeros in the shapestack value using clz

inline void clearShapeStack(SHAPESHACK *stack) {
    for (int i = 0; i < SHAPESHACKSECTIONS; i++) {
        stack[i] = EMPTY_SHAPESHACK;
    }
}

inline SHAPESHACK ignoreStack(SHAPESHACK section, int ignoreBits) {
    //DEBUG_IF(printf("ignoreBits %i (COMPLETE_MASK << ignoreBits) %lX (~(COMPLETE_MASK << ignoreBits)) %lX ignoreStack %lX (SHAPESHACKBITS - clz(ignoreStack)) - 1 %i \n", ignoreBits, (COMPLETE_MASK << ignoreBits), (~(COMPLETE_MASK << ignoreBits)), ignoreStack, (SHAPESHACKBITS - clz(ignoreStack)) - 1 );)
    return ignoreBits >= SHAPESHACKBITS ? section : (~(COMPLETE_MASK << ignoreBits)) & section;
}

inline int findSectionTop(SHAPESHACK section) {
    // find the top set bit
    return SHAPESHACKBITS - clz(section);
}

inline int findTop(PMEM SHAPESHACK *shapeStack, int ignoreAbove) {
    int ignoreSection = (ignoreAbove >> SHAPESHACKSECTIONSHIFT);
    int ignoreBits    = ignoreAbove & SHAPESHACKSECTIONBITS;
    //DEBUG_IF(printf("findTop ignoreAbove %i iS %i ignoreBits %i ", ignoreAbove, ignoreSection, ignoreBits);)
    SHAPESHACK section = ignoreStack(shapeStack[ignoreSection], ignoreBits);
    while (section == EMPTY_SHAPESHACK && ignoreSection > 0) {
      ignoreSection -= 1;
      section = shapeStack[ignoreSection];
    }
    int sectionBits = findSectionTop(section);
    return (ignoreSection << SHAPESHACKSECTIONSHIFT) + sectionBits - 1;
}


inline SHAPESHACK setSectionBit(SHAPEBIT bit)                     {return (((ulong)0x1) << bit);} // create a shape by shifting a bit to the right position
inline SHAPESHACK flipSectionBit(SHAPEBIT bit, SHAPESHACK section) {return (section ^ setSectionBit(bit));} // if shape on is true, toggle a shape bit

inline void flipBit(SHAPEBIT shapeBit, PMEM SHAPESHACK *shapeStack) {
    int section = shapeBit >> SHAPESHACKSECTIONSHIFT;
    int bit     = shapeBit & SHAPESHACKSECTIONBITS;
    //DEBUG_IF(printf("flipBit section %i bit %i before shapeStack[section] %lx ", section, bit, shapeStack[section]);)
    shapeStack[section] = flipSectionBit(bit, shapeStack[section]);
    //DEBUG_IF(printf(" after shapeStack[section] %lx\n", shapeStack[section]);)
}

inline SHAPESHACK carryBitSet(SHAPESHACK carryBit) {
    return carryBit << SHAPESHACKCARRYSHIFT;
}

inline SHAPESHACK shiftSection(SHAPESHACK carryBit, SHAPESHACK shapeStack) {
  return carryBitSet(carryBit) | (shapeStack >> 1);
}

inline SHAPESHACK getCarryBit(SHAPESHACK section) {
  return section & SHAPESHACKCARRYMASK;
}

inline SHAPESHACK deleteSectionBit(SHAPESHACK carryBit, SHAPESHACK shapeStack, SHAPEBIT shapeBit) {
    SHAPESHACK breakMask = COMPLETE_MASK << shapeBit;
    //DEBUG_IF(printf("shapeStack %lX shapeBit %i ----> !breakMask %lX (shapeStack >> 1) & (breakMask)) %lX shapeStack & !breakMask %lX\n", shapeStack, shapeBit, ~breakMask,(shapeStack >> 1) & breakMask, shapeStack & ~breakMask);)
    return ((shiftSection(carryBit, shapeStack)) & breakMask) | (shapeStack & (~breakMask));
}

inline void deleteBit(PMEM SHAPESHACK *shapeStack, SHAPEBIT shapeBit) {
    int section = shapeBit >> SHAPESHACKSECTIONSHIFT;
    int bit     = shapeBit & SHAPESHACKSECTIONBITS;
    SHAPESHACK carryBit = EMPTY_SHAPESHACK;
    for (int i = SHAPESHACKSECTIONS - 1; i > section; i--) {
      SHAPESHACK nextCarryBit = getCarryBit(shapeStack[i]);
      shapeStack[i] = shiftSection(carryBit, shapeStack[i]);
      carryBit = nextCarryBit;
    }
    shapeStack[section] = deleteSectionBit(carryBit, shapeStack[section], bit);
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

// A group contains information about the substance of a group of combined shapes.
typedef struct Group
  { COLOR     groupColor; // this is either the solid color of the shape or a reference to a picture ref.
  } Group;

// A picture reference is a reference to bitmap data that can be the substance of a shape.
typedef struct PictureRef
  { int2 pictTranslate; // translation vector in pixel units
    int  pictScale;     // scaling amount.
    int  pictMemOffset; // starting point of the pixel data in the memory buffer
    int2 pictSize;      // size of the bitmap
  } PictureRef;


/*
#define COLORBUFFERMODULOMASK = 0x3 // must be adjusted if COLORBUFFERSIZE is changed
#define COLORBUFFERMODULO(index) index & COLORBUFFERMODULOMASK
#define COLORBUFFERINDEX(offset, index) COLORBUFFERMODULO(offset + index)
#define COLORBUFFERELEMENT(cS, index) cS->csColors[COLORBUFFERMODULO(cS->csOffset, index)]
*/
// The color state structure tracks the current color information during a scan through thresholds
typedef struct ColorState {
               COLOR   csColors[COLORBUFFERSIZE]; // private array should correspond to the first 4 active substances
               COLOR   csBackgroundColor;         // background color
     GMEM      uchar  *csPictureData;             // global image information
     CMEM PictureRef  *csPictureRefs;             // global list of image references
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
              bool  needHorizontalPass; // did we overrun the number of simultaneous horizontal thresholds.
              bool  inHorizontalPass;   // special mode where we handle too many horizontal thresholds.
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

#define RANDOMFIELD_SIZE 4096
#define RANDOMFIELD_MASK 0xFFF

#define RANDOM_POS int

typedef struct ParseState {
    LMEM   GROUPID  shapeIndices[MAXSHAPE];        // a mapping from shape bit positions in the shapeStacks to shapeIndices in the tile.
    PMEM SHAPESHACK shapeStack[SHAPESHACKSECTIONS]; // the current shape Stack. (Each bit represents the presence of a shape in the stack.)
               int  currentThreshold;   // the current threshold bordering the section
               int  numActive;          // the next threshold that is not currently active.
            SPACE2  sectionStart;       // the top of the current vertical section being processed
            SPACE2  sectionEnd;         // the bottom of the current vertical section being processed
             float  pixelY;             // the bottom of the current pixel.
            float8  accColorArea;       // accumulated color and area.

               int  sectionCount;

               int  frameNumber;
               int  buildCount;
              bool  debugEnabled;
               int  passCount;
        RANDOM_POS  randomFieldCursor;
    CMEM     float *randomField;
  } ParseState;

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

typedef struct TileState {
    int  tileWidth;
    int  tileHeight;
    int  bitmapWidth;
    int  bitmapHeight;
    int  tileDeltaX;
    int  tileDeltaY;
    int  gridWidth;
    int  tileIndex;
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

bool traverseTree( SMEM                float2 *strandHeap
                 ,                        int  currentSize
                 ,                       Shape  shape
                 ,                      float2 tileDelta
                 ,                  Traversal *l
                 ,                  Traversal *r
                 );

void searchTree(   Traversal *trav
               , SMEM float4 *tree
               ,         int  treeSize
               ,      float4  tileDelta4
               ,        bool  isLeft
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
                     ,      SPACE yTop
                     ,      SPACE yBottom
                     ,      SPACE clampHigh
                     ,      SPACE clampLow
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

float arbitraryIntersect(    HEADER currentHeader
                        , THRESHOLD current
                        ,    HEADER nextHeader
                        , THRESHOLD next
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
                  ,                   HEADER  newHeader
                  ,                THRESHOLD  new
                  );
void insertThreshold( PMEM ThresholdState *tS
                    ,                   HEADER  newHeader
                    ,                THRESHOLD  new
                    );

void slotThreshold ( PMEM ThresholdState *tS
                   ,              HEADER  newHeader
                   ,           THRESHOLD  new
                   );

inline void passHeader(           HEADER thresholdHeader
                      , PMEM ParseState *pS
                      );

inline void passHeaderTop(           HEADER  thresholdHeader
                         , PMEM ParseState *pS
                         );

inline void passHeaderBottom(           HEADER  thresholdHeader
                            , PMEM ParseState *pS
                            );

inline void updateShapeStack(         SHAPEBIT  shapeBit
                            , PMEM  SHAPESHACK *shapeStack
                            );

COLOR readColor ( PMEM ColorState *cS
                , SMEM      Group *groups
                ,         GROUPID  shapeId
                ,            bool  isSolidColor
                );

COLOR determineColor( PMEM   ParseState *pS
                    , PMEM   ColorState *cS
                    , SMEM        Group *groups
                    , SMEM        Shape *shapeHeap
                    , SMEM    GEO_ENTRY *shapeRefs
                    );

void verticalAdvance( PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    );

void horizontalAdvance ( PMEM ThresholdState *tS
                       , PMEM     ParseState *pS
                       );

void writePixelGlobal ( PMEM TileState *tileS
                      ,          COLOR  color
                      , GMEM       uint *out
                      ,            int  column
                      ,            int  y
                      );

inline void moveToTile (              int   tileIndex
                       , SMEM       Slice  *tileHeap
                       , SMEM   GEO_ENTRY  *shapeRefHeap
                       , SMEM   GEO_ENTRY **shapeRefs
                       ,        GEO_ENTRY  *numShapes
                       );

 void removeLastShape( PMEM  ThresholdState *tS
                     , PMEM      ParseState *pS
                     ,             SHAPEBIT  shapeBit
                     ,            GEO_ENTRY  shapeIndex
                     );

void buildThresholdArray ( PMEM  ThresholdState *tS
                         , PMEM      ParseState *pS
                         , SMEM          float4 *geometryHeap
                         , SMEM           Group *groups
                         , SMEM           Shape *shapeHeap
                         , SMEM       GEO_ENTRY *shapeRefs
                         ,            GEO_ENTRY  numShapes
                         ,               float2  point
                         );

void resetParser (PMEM ParseState *pS
                 );

void nextRenderArea ( PMEM ThresholdState *tS
                    , PMEM ParseState *pS
                    ,           SPACE  height
                    );

void initRandomField( ParseState *pS
                    , TileState *tileS
                    , CMEM float *randomField
                    );

float getRandom(ParseState *pS);

void initThresholdState(ThresholdState *tS, Continuation cont);
void resetThresholdState(ThresholdState *tS);

void initParseState ( PMEM  ParseState *pS
                    , PMEM  ThresholdState *tS
                    , PMEM  TileState  *tileS
                    ,              int  frameNumber
                    ,             bool  debugEnabled
                    ,     Continuation  cont
                    ,              int  passCount
                    ,CMEM        float *randomField
                    );

void initTileState ( PMEM TileState *tileS
                   ,                 int  tileId
                   ,                 int  tileWidth
                   ,                 int  tileHeight
                   ,                 int  bitmapWidth
                   ,                 int  bitmapHeight
                   ,                 int  gridWidth
                   ,                 int  tileIndex
                   ,                 int  column
                   );

inline Continuation newContinuation(void);

inline Continuation getContinuationForTile(       TileState *tileS
                                          ,  GMEM      char *continuations
                                          );

inline Continuation getContinuation( GMEM  char *continuations
                                   ,        int  columnIndex
                                   );

Continuation makeContinuation( PMEM ThresholdState *tS
                             , PMEM     ParseState *pS
                             ,                int  yInt
                             ,               bool  isContinued
                             );

void setContinuation(        TileState *tileS
                    , GMEM        char *continuations
                    ,      Continuation c
                    );

float8 sectionColor ( PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    , PMEM     ColorState *cS
                    , SMEM          Group *groups
                    , SMEM           Shape *shapeHeap
                    , SMEM      GEO_ENTRY *shapeRefs
                    );

bool calculatePixel ( PMEM      TileState *tileS
                    , PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    , PMEM     ColorState *cS
                    , SMEM         float4 *geometryHeap
                    , SMEM          Group *groups
                    , SMEM           Shape *shapeHeap
                    , SMEM      GEO_ENTRY *shapeRefs
                    ,           GEO_ENTRY  numShapes
                    ,               SPACE  height
                    ,               float  x
                    );

Continuation renderPixelBuffer ( PMEM  TileState *tileS
                               , SMEM     float4 *geometryHeap
                               , GMEM      uchar *pictureData
                               , CMEM PictureRef *pictureRefs
                               , SMEM      Group *groups
                               , SMEM      Shape *shapeHeap
                               , SMEM  GEO_ENTRY *shapeRefs
                               ,       GEO_ENTRY  numShapes
                               ,           COLOR  backgroundColor
                               ,           float  floatHeight
                               ,             int  column
                               ,             int  frameNumber
                               , GMEM       uint *out
                               ,            bool  debugEnabled
                               ,             int  passCount
                               ,    Continuation  cont
                               , CMEM      float *randomField
                               );

void initColorState ( PMEM  ColorState *init
                    ,            COLOR  backgroundColor
                    , GMEM       uchar *pictureData
                    , CMEM  PictureRef *pictureRefs
                    ,             int2  absolutePosition
                    );

void rebuildColorState ( PMEM  ThresholdState *tS
                       , PMEM      ParseState *pS
                       , PMEM      ColorState *cS
                       , SMEM           Group *groups
                       );

void fillOutBuffer (       TileState *tileS
                   , GMEM       uint *out
                   ,           COLOR  color
                   );

// Debug Functions
void showTree(            Traversal t
             , SMEM       float4 *tree
             ,            int     treeSize
             ) ;
void showPixelBuffer(__local uint *pixelBuffer, int sectionWidth, int height);
void showTileSliceAlignment (int tileIndex, SMEM Slice *tileHeap);
void showShape (Shape shape);
void showGroup(Group group);
void showShapeIndices(ParseState *pS, int numShapes);
void showShapeColors(      ParseState *pS
                   , SMEM      Group *groups
                   , SMEM       Shape *shapeHeap
                   ,             int  numShapes
                   );
void showGroups(SMEM Group *groups, int numGroups);
void showContinuation(Continuation c);
void showShapeRefs(SMEM REF *shapeRefs, int numShapes);
void showShapes (SMEM Shape *shapeHeap, SMEM GEO_ENTRY *shapeRefs, SMEM Group *groups, int numShapes);
void showShapeStack(SHAPESHACK *shapeStack);
void showShapeStackHs(SHAPESHACK *shapeStack);
void showShapeAlignment (SMEM Shape *shapeHeap, SMEM GEO_ENTRY *shapeRefs, SMEM Group *groups, int numShapes);
void showData (GMEM uchar *pictData, int width, int height);
void showThresholdHeader( HEADER header);
void showThresholdGeo (THRESHOLD threshold);

void showThreshold( HEADER header
                  , THRESHOLD threshold
                  );

float infiniteZero(float x);
void showThresholds (PMEM   ThresholdState *tS);

void showActiveThresholds(PMEM ThresholdState *tS, int num);
void showColorHs( COLOR color );

void testShapeStack(void);
void testIgnoreBits(void);
void testDeleteBit(void);

void showColorStateHs (ColorState *cS
                      ,       int  numActive
                      );
void showSectionHs(    int numActive
                  ,  SPACE2 sectionStart
                  ,  SPACE2 sectionEnd
                  ,  float lastX
                  ,  float currentX
                  , COLOR color
                  , ThresholdState *tS
                  , ColorState *cS
                  );
void showThresholdHs(int i, THRESHOLD threshold, HEADER header);
void showThresholdsHs ( PMEM ThresholdState *tS
                      , int num
                      );
void showThresholdStateHs ( PMEM ThresholdState *tS
                          , ParseState *pS
                          , SMEM      Group *groups
                          , SMEM       Shape *shapeHeap
                          ,             int  numShapes
                          );
void showColorsHs ( PMEM ThresholdState *tS
                  , ParseState *pS
                  , SMEM      Group *groups
                  , SMEM       Shape *shapeHeap
                  ,             int  num
                  );
void showParseStateHs(  PMEM ThresholdState *tS
                     ,           ParseState *pS
                     , SMEM           Group *groups
                     , SMEM            Shape *shapeHeap
                     ,                COLOR  color
                     );


// ------------------- Inline Function Bodies -----------------------------

// value used to fudge geometry errors when comparing geometric values
#define TOLERANCE 0.002
// equality with the fudge factor
inline bool eqT( float a, float b) {
  return fabs(a - b) <= TOLERANCE;
}

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
    //DEBUG_IF(printf("FAILED %i : ", pS->inHorizontalPass);)
    if (tBottom(threshold) > tS->renderStart.y) {
        if (tS->inHorizontalPass) {
                tS->renderEnd.x = min(tS->renderEnd.x, tLeft(threshold));
            //DEBUG_IF(printf("trim area renderStart  %v2f renderEnd  %v2f \n", tS->renderStart,  tS->renderEnd);)
        }
        else {
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
                // Otherwise a single vertical section has > MAXTHRESHOLDS overlapping the section.
                // So must horizontally trim the render area.
                // First vertically trim the render area to the bottom of the threshold.
                next = tBottom(threshold);
                tS->needHorizontalPass = true;
            }
            tS->renderEnd.y = min(tS->renderEnd.y, next);
            //DEBUG_IF(printf("trim area nex %f \n", next);)
        }
    }
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
    //DEBUG_IF(printf("--------------- addThreshold %i left: %v2f right: %v2f addType: %i\n", tS->addThresholdCount, left, right, addType);)
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
        if ((tTop(newThreshold) < tS->renderEnd.y) && (tBottom(newThreshold) > tS->renderStart.y)) {
            if (tS->inHorizontalPass) {
                // find the midpoint of the threshold when bound by the section
                //DEBUG_IF(printf("before modified ");showThreshold(newHeader, newThreshold);printf("\n");)
                SPACE midX = thresholdMidXLow( newThreshold
                                             , newHeader
                                             , tS->renderStart.y
                                             , tS->renderEnd.y
                                             , tS->renderStart.x
                                             , tS->renderEnd.x
                                             );
                //DEBUG_IF(printf ("midX %f >>>", midX );)
                newThreshold = makeThreshold(tS->renderStart.y, tS->renderEnd.y, midX, midX);
                //DEBUG_IF(printf("modified ");showThreshold(newHeader, newThreshold);printf("\n");)
            }
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
                //DEBUG_IF(printf("inHori %i need %i \n", tS->inHorizontalPass, tS->needHorizontalPass);)
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
    //DEBUG_IF(printf("before xPos_L %f left_L: %v2f control_L: %v2f right_L: %v2f\n" \
    //               ,        xPos_L,   left_L,      control_L,      right_L       ); \
    //         printf("       xPos_R %f left_R: %v2f control_R: %v2f right_R: %v2f\n" \
    //               ,        xPos_R,   left_R,      control_R,      right_R       ); )
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
               , float4 tileDelta4
               , bool isLeft
               ){
    trav->travIndex = 0;
    while (trav->travIndex < treeSize) {
        float4 currentTree = tree[trav->travIndex] - tileDelta4;
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
                 ,         float2  tileDelta
                 ,      Traversal *l
                 ,      Traversal *r
                 ) {
    int    treeSize    =  (currentSize - 4) / 2; // take the total size in 64 bit parts and subtract the header and the left and right points. Divide by 2 becasue each left+control is 128 bytes
    float4 tileDelta4  =  (float4)(tileDelta, tileDelta); // create a double vector that can be used to offset reads of two simultaneous reads
    l->travRight       = *((SMEM float2 *)(strandHeap + 1)) - tileDelta; // load the rightmost point of the strand.
    l->travLeftControl = *((SMEM float4 *)(strandHeap + 2)) - tileDelta4; // load the leftmost point of the strand and it's control point.
    SMEM float4 *tree =   (SMEM float4 *)(strandHeap + 4); // Move to tree portion of strand array.
    //DEBUG_IF(printf("shape: %i strand: %i -------------\n", shapeIndex, strandIndex);)
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
        searchTree(l, tree, treeSize, tileDelta4, true);  // traverse the tree biased to the left
        searchTree(r, tree, treeSize, tileDelta4, false); // traverse the tree biased to the right
        //DEBUG_IF(printf("i_L %i i_R %i\n", i_L, i_R);)
        // build up to three thresholds based on the tree traversals
    }
    return inRange;
}

// read a color value depending on the substance and absolute position.
COLOR readColor ( PMEM ColorState *cS
                , SMEM      Group *groups
                ,         GROUPID  shapeId
                ,            bool  isSolidColor
                ) {
    Group group = groups[shapeId];
    //DEBUG_IF(showShape(shape);)
    if (isSolidColor) {
        return group.groupColor;
    } else { // its a picture reference
        uint pictId = (as_uint4(group.groupColor)).x;
        PictureRef pRef = cS->csPictureRefs[pictId];
        int2 relativePosition = cS->absolutePosition - pRef.pictTranslate;
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
COLOR determineColor( PMEM    ParseState *pS
                    , PMEM    ColorState *cS
                    , SMEM         Group *groups
                    , SMEM          Shape *shapeHeap
                    , SMEM     GEO_ENTRY *shapeRefs
                    ) {
    int topBit = MAXSHAPE;
    COLOR baseColor = TRANSPARENT_COLOR;
    COLOR nextColor;
    bool done = false;
       GROUPID lastId = NULLINDEX;
    bool lastIsContinue = true;
    bool lastIsSet = false;
    while (!done) {
           GROUPID shapeId = NULLINDEX;
        bool shouldComposite = true;
        topBit = findTop(pS->shapeStack, topBit);
        //DEBUG_IF(printf("topBit %i ", topBit);showShapeStack(pS->shapeStack);printf("\n");)
        if (topBit < 0) {
            nextColor = cS->csBackgroundColor;
            done = true;
            shouldComposite = true;
            lastIsSet = true;
        }
        else {
            int referenceFromBit = pS->shapeIndices[topBit];
            GEO_ENTRY shapeIndex = shapeRefs[referenceFromBit];
            SHAPETAG tag = shapeHeap[shapeIndex].shapeTag;
            shapeId = shapeTagGroupId(tag);
            //DEBUG_IF(printf("topBit %i shapeId %i lastId %i ",
            //                 topBit,   shapeId,   lastId   );)
            if (shapeId == lastId) {
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
            if (shapeId != lastId) {
                nextColor = readColor ( cS
                                      , groups
                                      , shapeId
                                      , shapeTagIsSolidColor(tag)
                                      );
                shouldComposite = true;
                lastIsSet = shapeTagIsAdd(tag) || shapeTagIsContinue(tag);
            }
            lastId = shapeId;

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

inline void passHeader(          HEADER  header
                      , PMEM ParseState *pS
                      ) {
    //DEBUG_IF(printf(":%i", headerShapeBit(header));)
    flipBit(headerShapeBit(header), pS->shapeStack);
}

inline void passHeaderTop(               HEADER  header
                         , PMEM ParseState *pS
                         ) {
    if (headerPersistTop(header)) {
        //DEBUG_IF(printf("T");)
        passHeader(header, pS);
        //DEBUG_IF(printf("\n");)
    }
}

inline void passHeaderBottom(               HEADER  header
                            , PMEM ParseState *pS
                            ) {
    if (headerPersistBottom(header)) {
        //DEBUG_IF(printf("B");)
        passHeader(header, pS);
        //DEBUG_IF(printf("\n");)
    }
}

// Move to the correct shapes for the current tile.
inline void moveToTile (              int   tileIndex
                       , SMEM       Slice  *tileHeap
                       , SMEM   GEO_ENTRY  *shapeRefHeap
                       , SMEM   GEO_ENTRY **shapeRefs
                       ,        GEO_ENTRY  *numShapes
                       ) {
    Slice shapeSlice =  tileHeap[tileIndex];
          *shapeRefs = &shapeRefHeap[shapeSlice.sStart];
          *numShapes =  shapeSlice.sLength;
}

#define DEBUG_BUILDTHRESHOLDARRAY \
            DEBUG_HS(printf(",\n");showThresholdStateHs(tS, pS, groups, shapeHeap, shapeBit);)

#define LIMITSHAPE 0

void removeLastShape( PMEM  ThresholdState *tS
                    , PMEM      ParseState *pS
                    ,             SHAPEBIT  shapeBit
                    ,           GEO_ENTRY   shapeIndex
                    ) {
    SHAPEBIT removeBit = tS->numThresholds == 0 ? 0 : headerShapeBit(getHeader(tS, tS->numThresholds - 1));
    int shiftAmount = 0;
    //DEBUG_IF(if(shapeIndex > LIMITSHAPE-2) {printf("before RemoveBit %i\n", removeBit);showThresholds(tS);printf("\n");})
    //DEBUG_IF(printf("before removeBit shapeBit %i removeBit %i tS->numThresholds %i\n", shapeBit, removeBit, tS->numThresholds);)
    for (int cursor = 0; cursor < tS->numThresholds; cursor ++) {
        HEADER currentHeader = getHeader(tS, cursor);
        THRESHOLD current = getThreshold(tS, cursor);
        SHAPEBIT currentBit = headerShapeBit(currentHeader);
        if (currentBit > removeBit) {
            currentBit -= 1;
        }
        if (headerShapeBit(currentHeader) == removeBit)  {
            shiftAmount += 1;
            //DEBUG_IF(if(shapeIndex > LIMITSHAPE-2) {printf("%i DELETE cursor %i shiftAmount %i current: ", tS->addThresholdCount, cursor, shiftAmount);showThreshold(currentHeader, current);printf("\n");})
            adjustToExclude(tS, current);
        }
        else {
            //DEBUG_IF(if(shapeIndex > LIMITSHAPE-2) {printf("%i MOVE   cursor %i shiftAmount %i current: ", tS->addThresholdCount, cursor, shiftAmount);showThreshold(currentHeader, current);printf("\n");})
            setHeader(tS, cursor-shiftAmount, setShapeBit(currentHeader,currentBit));
            setThreshold(tS, cursor-shiftAmount, current);
        }
    }
    tS->numThresholds = max(0, tS->numThresholds - shiftAmount);
    for (int i = removeBit; i < MAXSHAPE - 1; i++) {
        pS->shapeIndices[i] = pS->shapeIndices[i+1];
    }
    //DEBUG_IF(printf("removeBit %i shapeBit %i\n", removeBit, shapeBit);)
    //DEBUG_IF(printf("before deleteBit ");showShapeStack(pS->shapeStack);printf("\n");)
    deleteBit(pS->shapeStack, removeBit);
    //DEBUG_IF(printf("after  deleteBit ");showShapeStack(pS->shapeStack);printf("\n");)
    if (removeBit < MAXSHAPE) {
        pS->shapeIndices[shapeBit] = shapeIndex;
    }
}

// Parse all of the current shapes adding as many thresholds as possible.
// Return the bottom of the rendering area which is the bottom of the tile if everything fits and the last
// complete section if it doesn't.
void buildThresholdArray ( PMEM  ThresholdState *tS
                         , PMEM      ParseState *pS
                         , SMEM          float4 *geometryHeap
                         , SMEM           Group *groups
                         , SMEM           Shape *shapeHeap
                         , SMEM       GEO_ENTRY *shapeRefs
                         ,            GEO_ENTRY  numShapes
                         ,               float2  point
                         ) {
    SHAPEBIT shapeBit = 0;
    for (GEO_ENTRY shapeIndex = 0; shapeIndex < numShapes; shapeIndex++) { // iterate over every shape in the current shape.
        tS->thresholdWasAdded = false;
        Shape shape = shapeHeap[shapeRefs[shapeIndex]]; // get the current shape.
         // if you don't shift the shape to the tile size there will be accuracy errors with height floating point geometric values
        SMEM float2 *strandHeap = (SMEM float2 *)&geometryHeap[getGeometryStart(shape)];
        bool enclosedByShape = false;
        for (uint currentStrand = 0; currentStrand < getNumStrands(shape); currentStrand++) {
            uchar4 header = *((SMEM uchar4 *)strandHeap);
            ushort currentSize = as_ushort(header.xy);
            bool enclosedByStrand = false;
            Traversal left;
            Traversal right;
            bool inRange = traverseTree(  strandHeap
                                       ,  currentSize
                                       ,  shape
                                       ,  point
                                       , &left
                                       , &right
                                       );
            if (inRange) {
                //DEBUG_IF(printf("shapeBit %i shapeIndex %i\n",shapeBit, shapeIndex);)
                spawnThresholds (  tS
                                ,  shapeBit
                                , &left
                                , &right
                                , &enclosedByStrand
                                );
            }
            //DEBUG_IF(showThresholds(tS);)
            strandHeap += currentSize;
            enclosedByShape = enclosedByShape != enclosedByStrand; // using not equal as exclusive or.
        } // for currentStrand
        //barrier (CLK_LOCAL_MEM_FENCE); // in case rebuffer occurs
        if (enclosedByShape) {
            //DEBUG_IF(printf("enclosedByShape %i shapeBit %i \n", enclosedByShape, shapeBit);)
            passHeader(shapeBit, pS);
            //DEBUG_IF(printf("pS->shapeStack %lX \n", pS->shapeStack);)
        }
        //DEBUG_IF(printf("before remove pS->thresholdWasAdded %i enclosedByShape %i shapeBit %i\n", pS->thresholdWasAdded, enclosedByShape, shapeBit);showThresholds(tS);)
        //DEBUG_IF(if(shapeIndex > LIMITSHAPE-2) {printf("before shapeBit %i\n",shapeBit);showShapeColors(pS, groups, shapeHeap, shapeBit);})
        if (tS->thresholdWasAdded || enclosedByShape) {
            if (shapeBit >= MAXSHAPE) {
                shapeBit -= 1;
                removeLastShape(tS,pS,shapeBit, shapeIndex);
            }
            else {
               //DEBUG_IF(if(shapeIndex > LIMITSHAPE-2) {printf("replace shapeBit %i\n", shapeBit);})
               pS->shapeIndices[shapeBit] = shapeIndex;
            }
            shapeBit += 1;
            //DEBUG_IF(if(shapeIndex > LIMITSHAPE-2) {printf("after RemoveBit\n");
            //                              showShapeColors(pS, groups, shapeHeap, shapeBit);
            //                              showThresholds(tS);
            //                             })
        }
    }
    //DEBUG_IF(printf("shapeColors %i\n",shapeBit);showShapeColors(pS, groups, shapeHeap, shapeBit);)
    DEBUG_BUILDTHRESHOLDARRAY
    //DEBUG_IF(showShapeIndices(pS, shapeBit);)
}

void resetParser ( PMEM ParseState *pS
                 ) {
    clearShapeStack(pS->shapeStack);
    pS->currentThreshold = 0;
    pS->numActive        = 0; // the next threshold that is not currently active.
}

void nextRenderArea ( PMEM ThresholdState *tS
                    , PMEM ParseState     *pS
                    ,               SPACE  height
                    ) {
    if (tS->renderEnd.x == RIGHTBORDER) {
        tS->inHorizontalPass = false;
    }
    if (!(tS->needHorizontalPass || tS->inHorizontalPass)) {
        tS->renderStart.y = tS->renderEnd.y;
    }
    if (!tS->inHorizontalPass) {
        tS->renderEnd.y = tS->needHorizontalPass ? min(tS->renderEnd.y, pS->pixelY)
                                                 : height;
    }
    tS->renderStart.x  = tS->renderEnd.x != RIGHTBORDER ? tS->renderEnd.x : LEFTBORDER;  // finish horizontal sweep.
    tS->renderEnd.x    = RIGHTBORDER;

    if (tS->needHorizontalPass) {
        tS->needHorizontalPass = false;
        tS->inHorizontalPass   = true;
    }
}

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

inline Continuation getContinuationForTile( PMEM TileState *tileS
                                          , GMEM      char *continuations
                                          ) {
    return getContinuation(continuations, tileS->tileIndex * tileS->tileWidth + tileS->column);
}

inline Continuation getContinuation( GMEM char *continuations
                                   ,       int  columnIndex
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
                    , GMEM            char *continuations
                    ,          Continuation c
                    ) {
    GMEM Continuation *p = ((GMEM Continuation *)(continuations + (tileS->tileIndex * tileS->tileWidth + tileS->column)*CONTINUATION_ALIGN));
    *p = c;
}

void initRandomField( ParseState *pS
                    , TileState *tileS
                    , CMEM float *randomField) {
  // find a random starting point in the field passed on the absolute start position of the column.
  int start = (tileS->column + (tileS->tileIndex * tileS->tileWidth)) & RANDOMFIELD_MASK;
  pS->randomFieldCursor = (as_uint(randomField[start]) & RANDOMFIELD_MASK);
  DEBUG_IF(printf("start %i pS->randomFieldCursor %i tileS->column %i tileS->tileIndex %i tileS->tileWidth %i\n"
                  ,start   ,pS->randomFieldCursor,   tileS->column,   tileS->tileIndex,   tileS->tileWidth );)
  pS->randomField = randomField;
}

float getRandom(ParseState *pS) {
    pS->randomFieldCursor = (pS->randomFieldCursor + 1) & RANDOMFIELD_MASK;
    float random = pS->randomField[pS->randomFieldCursor];
    DEBUG_IF(printf("random %f\n", random);)
    return random;
}

void initThresholdState(ThresholdState *tS, Continuation cont) {
    tS->renderStart  = cont.contRenderStart;
    tS->renderEnd    = cont.contRenderEnd; // the lowest vertical position that can be properly rendered with the current list of thresholds.
    tS->inHorizontalPass = cont.contInHorizontalPass;
    tS->needHorizontalPass = cont.contNeedHorizontalPass;
    resetThresholdState(tS);
}

void resetThresholdState(ThresholdState *tS) {
    tS->numThresholds = 0;
    tS->thresholdStart = MAXTHRESHOLDS;
    tS->slotThresholdCount = 0;
    tS->thresholdWasAdded = false;
    tS->addThresholdCount = INT_MAX; // 400; // /*INT_MAX; (passCount == 0) ? 400 : 400; //1024 + 512 + 256 + 64 + 32 + 8 + 4
}


void initParseState (PMEM ParseState *pS
                    ,PMEM ThresholdState *tS
                    ,PMEM TileState  *tileS
                    ,            int  frameNumber
                    ,           bool  debugEnabled
                    ,   Continuation  cont
                    ,            int  passCount
                    , CMEM     float *randomField
                    ) {
    resetParser(pS);
    pS->accColorArea = cont.contAccColorArea;
    // if we go below render bottom we must rebuild the threshold list.
    pS->sectionStart = (SPACE2)(LEFTBORDER,0); // the top of the current vertical section being processed
    pS->sectionEnd   = (SPACE2)(tS->renderEnd.x, tS->renderEnd.y); // the bottom of the current vertical section being processed
    pS->sectionCount = 0;

    pS->frameNumber = frameNumber;
    pS->buildCount = 0;
    pS->debugEnabled = debugEnabled;

    pS->passCount = passCount;
    initRandomField(pS,tileS,randomField);
    DEBUG_HS(printf("[ListStart\n");)
}

void initTileState ( PMEM TileState *tileS
                   ,                 int  tileId
                   ,                 int  tileWidth
                   ,                 int  tileHeight
                   ,                 int  bitmapWidth
                   ,                 int  bitmapHeight
                   ,                 int  gridWidth
                   ,                 int  tileIndex
                   ,                 int  column
                   ) {
    tileS->tileWidth    = tileWidth;
    tileS->tileHeight   = tileHeight;
    tileS->bitmapWidth  = bitmapWidth;
    tileS->bitmapHeight = bitmapHeight;
    tileS->tileDeltaX   = (tileId % gridWidth) * tileWidth;  // x value of the top left corner of the tile
    tileS->tileDeltaY   = (tileId / gridWidth) * tileHeight; // y value of the top left corner of the tile
    tileS->gridWidth    = gridWidth;
    tileS->tileIndex    = tileIndex;
    tileS->column       = column;
}

#define DEBUG_NEXTSECTION DEBUG_HS(printf(",\n");showParseStateHs(tS, pS, groups, shapeHeap, color);)

float8 sectionColor ( PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    , PMEM     ColorState *cS
                    , SMEM               Group *groups
                    , SMEM                Shape *shapeHeap
                    , SMEM           GEO_ENTRY *shapeRefs
                    ) {
    COLOR color = determineColor( pS
                                , cS
                                , groups
                                , shapeHeap
                                , shapeRefs
                                );
    DEBUG_NEXTSECTION
    float random = getRandom(pS);
    float area = (pS->sectionEnd.x - pS->sectionStart.x) * (pS->sectionEnd.y - pS->sectionStart.y);
    float4 adjustedArea = (float4) (area + (area * random * STOCHASTIC_FACTOR));
    //DEBUG_IF(printf("         color: %2.2v4f  area %f \n", color, area);)
    return (float8)(color * adjustedArea, adjustedArea);
}

#define DEBUG_SHOW_SECTION(rightSide) DEBUG_HS(\
  printf(", "); \
  showSectionHs( pS->numActive     \
               , pS->sectionStart  \
               , pS->sectionEnd    \
               , lastX             \
               , rightSide         \
               , color             \
               , tS                \
               , cS                \
               );)

#define DEBUG_VERTICAL DEBUG_IF(printf("\nvert %3i cr %i ae %i sStart %2.2v2f sEnd %2.2v2f" \
                                     , pS->sectionCount \
                                     , pS->currentThreshold \
                                     , pS->numActive \
                                     , pS->sectionStart \
                                     , pS->sectionEnd \
                                     ); \
                               showShapeStack(pS->shapeStack); \
                               printf("\n"); \
                               )

void verticalAdvance( PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    ) {
    if (pS->sectionEnd.x == tS->renderEnd.x) {
        //DEBUG_VERTICAL
        //DEBUG_IF(printf("---------- Vertical Advance -------------- \n");)
        // Start by undoing all of the state changes from the horizontal traversal.
        // Occasionally a threshold gets skipped because they are out of order.
        // pass these.
        // Revert all horizontal border crossing from the lass vertical advances.
        //DEBUG_IF(printf("rev->");)
        for (int i = 0; i < pS->numActive; i++) {
            //DEBUG_IF(printf("back %i %i\n",i,headerShapeBit(getHeader(tS, i)));)
            passHeader(getHeader(tS, i), pS);
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
                passHeaderBottom(getHeader(tS, 0), pS);
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
                    passHeaderTop(getHeader(tS, 0), pS);
                    popTop(tS);
                    pS->numActive -= 1;
                }
                for (int i = 0; i < pS->numActive; i++) {
                    if (tTop(getThreshold(tS, i)) > tS->renderStart.y) { // TODO: Can probably get rid of this check.
                        //DEBUG_IF(if (headerPersistTop(getHeader(tS, i))) {printf("top  %i %i\n", i, headerShapeBit(getHeader(tS, i)));})
                        passHeaderTop(getHeader(tS, i), pS);
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
        //DEBUG_IF(showShapeStack(pS->shapeStack); printf("  ");
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
                      ,            int  column
                      ,            int  y
                      ) {
    uint colorWord = colorToSolidPixel_Word32_BGRA(color);
    //DEBUG_IF(printf("write %3i    %2.2v4f    %x\n", y, color,colorWord);)
    if (column < tileS->tileWidth) {
        int outPos = (mul24(tileS->tileDeltaY + y, tileS->bitmapWidth)) + tileS->tileDeltaX + column;
        out[outPos] = colorWord;
    }
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
                               showShapeStack(pS->shapeStack); \
                               printf("\n"); \
                               )

// "Who creates an engine that can properly render an image with more than 128 horizontal thresholds in
// one pixel?" You might ask. And we reply "We do, that's who."

bool calculatePixel ( PMEM      TileState *tileS
                    , PMEM ThresholdState *tS
                    , PMEM     ParseState *pS
                    , PMEM     ColorState *cS
                    , SMEM         float4 *geometryHeap
                    , SMEM          Group *groups
                    , SMEM           Shape *shapeHeap
                    , SMEM      GEO_ENTRY *shapeRefs
                    ,           GEO_ENTRY  numShapes
                    ,               SPACE  height
                    ,               float  x
                    ) {
    //DEBUG_IF(printf("                                              pixelY: %f \n", pS->pixelY);)
    //DEBUG_IF(printf("           y: %f floatHeight: %f eEnd: %i sTop: %f sBot: %f sStack %i\n",\
    //                            y, floatHeight, numActive, sectionStart, sectionEnd, shapeStack);)
    bool done = false;
    int count = 100;
    while (((pS->sectionEnd.x < RIGHTBORDER) || (pS->sectionEnd.y < pS->pixelY)) && !done /*&& count > 0*/) { // process all sections that do not reach the bottom of the pixel.
        count -= 1;
        //DEBUG_IF(printf("loop        sectionStart %v2f sectionEnd %v2f \n", pS->sectionStart, pS->sectionEnd);)
        //DEBUG_IF(printf("loop        renderStart  %v2f renderEnd  %v2f \n", pS->renderStart,  pS->renderEnd );)
        if (pS->sectionEnd.x == tS->renderEnd.x && pS->sectionEnd.y == tS->renderEnd.y) {
            do {
                // If the section bottom is below the area we can properly render.
                // Rebuild the threshold list and reset the processing state.
                // TODO: This should really happen if ANY thread reaches the end of the render area. Otherwise different threads could trigger
                // a rebuild out of sync.
                //DEBUG_IF(printf("============== buildThresholdArray inHorizontalPass %i needHorizontalPass %i ===============\n", pS->inHorizontalPass, pS->needHorizontalPass);)
                if (pS->buildCount < MAXBUILDS) {
                    resetParser(pS);
                    nextRenderArea(tS, pS, height);
                    resetThresholdState(tS);
                    //DEBUG_IF(printf("============== afterNext area      inHorizontalPass %i ===============\n", pS->inHorizontalPass);)
                    //DEBUG_IF(printf("before      sectionStart %v2f sectionEnd %v2f \n", pS->sectionStart, pS->sectionEnd);)
                    //DEBUG_IF(printf("before      renderStart  %v2f renderEnd  %v2f \n", tS->renderStart,  tS->renderEnd );)
                    buildThresholdArray( tS
                                       , pS
                                       , geometryHeap
                                       , groups
                                       , shapeHeap
                                       , shapeRefs
                                       , numShapes
                                       , (float2)(tileS->tileDeltaX + x, tileS->tileDeltaY)
                                       );
                    pS->buildCount += 1;
                }
                else {
                    done = true;
                }
                //DEBUG_IF(showThresholds(tS);)
                //DEBUG_IF(printf("after      sectionStart %v2f sectionEnd %v2f \n", pS->sectionStart, pS->sectionEnd);)
                //DEBUG_IF(printf("after      renderStart  %v2f renderEnd  %v2f \n", tS->renderStart,  tS->renderEnd );)
                //DEBUG_IF(printf("============== buildThresholdArray COMPLETE inHorizontalPass %i needHorizontalPass %i ==============\n", pS->inHorizontalPass, pS->needHorizontalPass);)
            } while (tS->needHorizontalPass);
            pS->sectionStart = tS->renderStart;
            if (tS->inHorizontalPass) {
                pS->sectionEnd.x = tS->renderStart.x;
                pS->sectionEnd.y = tS->renderEnd.y;
                pS->numActive = tS->numThresholds;
            }
            else {
                pS->sectionStart.x = tS->renderStart.x;
                pS->sectionStart.y = tS->renderStart.y;
                pS->sectionEnd.x   = tS->renderEnd.x;
                pS->sectionEnd.y   = tS->renderStart.y;
            }
        }
        //done = true; // this is for debugging only.
        if (!done) {
            //DEBUG_IF(printf("beforeV cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
            verticalAdvance(tS, pS);
            //DEBUG_IF(printf("afterV  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
            horizontalAdvance(tS, pS);
            //DEBUG_IF(printf("afterH  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
            float8 colorArea = sectionColor( tS
                                           , pS
                                           , cS
                                           , groups
                                           , shapeHeap
                                           , shapeRefs
                                           );
            pS->accColorArea += colorArea;
            //DEBUG_SECTION
            if (pS->currentThreshold < pS->numActive) {
                //DEBUG_IF(printf("pass %i %i\n",pS->currentThreshold,headerShapeBit(getHeader(tS, pS->currentThreshold)));)
                passHeader(getHeader(tS, pS->currentThreshold), pS);
            }
            pS->currentThreshold += 1;
            pS->sectionCount += 1;
            //DEBUG_IF(printf("atEnd  cr %i ae %i sectionStart %v2f sectionEnd %v2f \n", pS->currentThreshold, pS->numActive, pS->sectionStart, pS->sectionEnd);)
        }
    } // while (((pS->sectionEnd.x < RIGHTBORDER) || (pS->sectionEnd.y < pS->pixelY)))
    //DEBUG_IF(printf("pixelDone\n");)
    return done;
}

// create an initial color state.
void initColorState( PMEM   ColorState *init
                   ,             COLOR  backgroundColor
                   , GMEM        uchar *pictureData
                   , CMEM   PictureRef *pictureRefs
                   ,              int2  pos
                   ) {
  init->csBackgroundColor = backgroundColor;
  init->csPictureData = pictureData;
  init->csPictureRefs = pictureRefs;
  init->absolutePosition = pos;
}

#define DEBUG_COLOR_STATE DEBUG_HS(printf(",\n"); showColorStateHs(cS, pS->numActive); printf("\n");)


Continuation renderPixelBuffer ( PMEM   TileState *tileS
                               , SMEM      float4 *geometryHeap
                               , GMEM       uchar *pictureData
                               , CMEM  PictureRef *pictureRefs
                               , SMEM       Group *groups
                               , SMEM       Shape *shapeHeap
                               , SMEM   GEO_ENTRY *shapeRefs
                               ,        GEO_ENTRY  numShapes
                               ,            COLOR  backgroundColor
                               ,            float  floatHeight
                               ,              int  column
                               ,              int  frameNumber
                               , GMEM        uint *out
                               ,             bool  debugEnabled
                               ,              int  passCount
                               ,     Continuation  cont
                               ,CMEM        float *randomField
                               ) {
    //DEBUG_IF(printf("INDEX %i gTileIndex %i numShapes %i \n", INDEX, gTileIndex, numShapes);)
    //barrier (CLK_LOCAL_MEM_FENCE);
    float x = ((float) column * COLUMNSPACING); // actual x position we are transecting
    ThresholdState tS;
    initThresholdState(&tS, cont);
    ParseState pS;
    initParseState(&pS, &tS, tileS, frameNumber, debugEnabled, cont, passCount, randomField);
    ColorState cS;
    initColorState( &cS
                  ,  backgroundColor
                  ,  pictureData
                  ,  pictureRefs
                  // initial absolutPosition
                  ,  (int2)( tileS->tileDeltaX + column
                           , tileS->tileDeltaY
                           )
                  );
    int yInt = -1;
    int startY = cont.contYInt;
    cont = newContinuation();
    bool isContinued = false;
    for (pS.pixelY = 1.0f; pS.pixelY <= floatHeight; pS.pixelY += PIXELHEIGHT) { // y is the bottom of the current pixel.
        yInt += 1;
        //DEBUG_IF(printf("yInt %i startY %i\n", yInt, startY);)
        if (yInt >= startY) {
            if (!isContinued) {
                isContinued = calculatePixel (  tileS
                                             , &tS
                                             , &pS
                                             , &cS
                                             ,  geometryHeap
                                             ,  groups
                                             ,  shapeHeap
                                             ,  shapeRefs
                                             ,  numShapes
                                             ,  floatHeight
                                             ,  x
                                             );
                if (isContinued) {
                    cont = makeContinuation(&tS, &pS, yInt, true);
                }
            }
            if (!isContinued) {
              // write the accumulated color information to the pixel buffer.
              float4 color = pS.accColorArea.s0123 / pS.accColorArea.s4567;
              writePixelGlobal ( tileS
                               , color
                               , out
                               , column
                               , yInt
                               );
            }
            //else {
              //DEBUG_IF(printf("skip   %i    %2.2v4f\n\n", yInt, pS.accColor);)
            //}
            pS.accColorArea = (float8)(TRANSPARENT_COLOR,(float4)(0,0,0,0));
            pS.sectionStart = (SPACE2)(LEFTBORDER,pS.pixelY);
            cS.absolutePosition += (int2)(0,1);
        }
    } // for y
    DEBUG_HSP(printf("]\n");)
    //DEBUG_IF(printf("slotThresholdCount = %i\n",pS.slotThresholdCount);)
    return cont;
}

__kernel void multiTileRaster ( SMEM     float4 *geometryHeap
                              , SMEM      Shape *shapeHeap
                              , SMEM        REF *shapeRefHeap
                              , SMEM      Group *groupHeap
                              , SMEM      Slice *tileHeap
                              , GMEM      uchar *pictureData
                              , CMEM PictureRef *pictureRefs
                              , CMEM      float *randomField
                              ,           COLOR  backgroundColor
                              , GMEM        uint *out
                              ,             int  bitmapWidth
                              ,             int  bitmapHeight
                              ,             int  gridWidth
                              ,             int  tileWidth
                              ,             int  tileHeight
                              ,             int  frameNumber
                              ,            int2  userCursor
                              , GMEM       char *continuations
                              ,             int  passCount
                              , SMEM       uint *tileIndices
                              ) {
    // INDEX is the sequential number of the tile in the current workgroup.
    int   tileId     = tileIndices[INDEX]; // tileId is the number of the tile across the entire window.
    int   column     = COLUMN;
    TileState tileS;
    initTileState ( &tileS
                  ,  tileId
                  ,  tileWidth
                  ,  tileHeight
                  ,  bitmapWidth
                  ,  bitmapHeight
                  ,  gridWidth
                  ,  INDEX
                  ,  column
                  );
    bool debugEnabled = COLUMN == DEBUGCOLUMN && INDEX == DEBUGINDEX;
    //(userCursor.y >= tileS.tileDeltaY) &&
    //(userCursor.y < tileS.tileDeltaY + tileS.tileHeight) &&
    //(userCursor.x == tileS.tileDeltaX + column);
    //DEBUG_IF(printf("userCursor %v2i debugEnabled %i \n", userCursor, debugEnabled);)
    float floatHeight = convert_float( min( tileS.tileHeight
                                          , tileS.bitmapHeight-tileS.tileDeltaY
                                          )
                                      );
    //testShapeStack();
    //testDeleteBit();
    SMEM GEO_ENTRY *shapeRefs;
    GEO_ENTRY numShapes;
    Continuation c = passCount == 0 ? newContinuation() : getContinuationForTile(&tileS, continuations);
    if (tileS.tileDeltaX + column < tileS.bitmapWidth) {
        //printf("tileId %i column %i tileDeltaX %i tileDeltaY %i absolutePosition %v2i \n", tileId, column, tileDeltaX, tileDeltaY, absolutePosition);
        moveToTile(INDEX, tileHeap, shapeRefHeap, &shapeRefs, &numShapes);
        //DEBUG_IF(showShapes(shapeHeap, shapeRefs, shapeHeap, numShapes);)
        if (numShapes == 0) {
            fillOutBuffer (&tileS
                          , out
                          , backgroundColor
                          );
        }
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    if (tileS.tileDeltaX + column < tileS.bitmapWidth /*&& COLUMN == DEBUGCOLUMN && INDEX < 2*/) {
        if (numShapes > 0) {
            //DEBUG_IF(printf("before\n");showContinuation(c);)
             if (passCount == 0 || c.contIsContinued) {
                 c = renderPixelBuffer ( &tileS
                                       ,  geometryHeap
                                       ,  pictureData
                                       ,  pictureRefs
                                       ,  groupHeap
                                       ,  shapeHeap
                                       ,  shapeRefs
                                       ,  numShapes
                                       ,  backgroundColor
                                       ,  floatHeight
                                       ,  column
                                       ,  frameNumber
                                       ,  out
                                       ,  debugEnabled
                                       ,  passCount
                                       ,  c
                                       ,  randomField
                                       );
             }
            //DEBUG_IF(printf("raster\n");showContinuation(c);)
        }
    }
    setContinuation(&tileS, continuations, c);
    barrier(CLK_GLOBAL_MEM_FENCE);
}

/*
__kernel void getIdsAtPoint ( SMEM     float4 *geometryHeap
                            , SMEM      Shape *shapeHeap
                            , SMEM        REF *shapeRefHeap
                            , SMEM      Group *groupHeap
                            , SMEM      Slice *tileHeap
                            , GMEM        uint *out
                            ,             int  bitmapWidth
                            ,             int  bitmapHeight
                            ,             int  gridWidth
                            ,             int  tileWidth
                            ,             int  tileHeight
                            ,             int  frameNumber
                            , SMEM       uint *tileIndices
                            , SMEM     float2 *inputPoints
                            ) {
    // INDEX is the sequential number of the tile in the current workgroup.
    int    tileId = tileIndices[INDEX]; // tileId is the number of the tile across the entire window.
    float2 point  = inputPoints[INDEX];
    TileState tileS;
    initTileState ( &tileS
                  ,  tileId
                  ,  tileWidth
                  ,  tileHeight
                  ,  bitmapWidth
                  ,  bitmapHeight
                  ,  gridWidth
                  ,  INDEX
                  ,  0 // column is not used
                  );
    SMEM GEO_ENTRY *shapeRefs;
    GEO_ENTRY numShapes;
    moveToTile(INDEX, tileHeap, shapeRefHeap, &shapeRefs, &numShapes);
    barrier(CLK_GLOBAL_MEM_FENCE);
    float x = point.x - tileS->tileDeltaX
    float y = point.y - tileS->tileDeltaY
    ThresholdState tS;
    ParseState pS;
    initParseStatePoint(&pS, tileS, frameNumber, false, cont, passCount, randomField);
    buildThresholdArray( tS
                       , pS
                       , geometryHeap
                       , groups
                       , shapeHeap
                       , shapeRefs
                       , numShapes
                       , x
                       );
    barrier(CLK_GLOBAL_MEM_FENCE);
}
*/
// when there are no groups in the tile fill it with the background color.
void fillOutBuffer ( PMEM TileState *tileS
                   , GMEM      uint *out
                   ,          COLOR  color
                   ) {
    //int width  = min(tileWidth,  bitmapWidth  - tileDeltaX);
    int height = min(tileS->tileHeight, tileS->bitmapHeight - tileS->tileDeltaY);
    uint pixel = colorToSolidPixel_Word32_BGRA(color);
    int outPos = (tileS->tileDeltaY * tileS->bitmapWidth) + tileS->tileDeltaX + COLUMN;
    for (int y = 0; y < height; y++) {
        out[outPos] = pixel;
        outPos += tileS->bitmapWidth;
    }
}

__kernel void fillBackgroundTile (      COLOR  backgroundColor
                                 , GMEM  uint *out
                                 ,        int  bitmapWidth
                                 ,        int  bitmapHeight
                                 ,        int  gridWidth
                                 ,        int  tileWidth
                                 ,        int  tileHeight
                                 , SMEM  uint *tileIndices
                                 ) {
    int   tileId = tileIndices[INDEX];
    int   column = COLUMN;
    TileState tileS;
    initTileState ( &tileS
                  ,  tileId
                  ,  tileWidth
                  ,  tileHeight
                  ,  bitmapWidth
                  ,  bitmapHeight
                  ,  gridWidth
                  ,  INDEX
                  ,  column
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

void testShapeStack(void) {
    DEBUG_IF(printf("clz(0x8000000000000000) %i, clz(0x00000000) %i\n", clz((ulong)0x8000000000000000), clz((ulong)0x0));)
    __private SHAPESHACK stack[SHAPESHACKSECTIONS];
    for (int bit = MAXSHAPE; bit >= 0; bit -= 1) {
        clearShapeStack(stack);
        flipBit(bit,   stack);
        //flipBit(bit+1, stack);
        DEBUG_IF(printf(" bit: %i top: %i ", bit, findTop(stack, 512));showShapeStack(stack);printf("\n");)
    }
}

void testIgnoreBits(void) {
  for (int ignoreAbove = 512; ignoreAbove >= 0; ignoreAbove--) {
      DEBUG_IF( int ignoreSection = ignoreAbove >> SHAPESHACKSECTIONSHIFT; \
                int ignoreBits    = ignoreAbove & SHAPESHACKSECTIONBITS; \
                printf("ignoreAbove: %i ignoreSection %i ignoreBits %i ",ignoreAbove,ignoreSection,ignoreBits); \
                printf("(COMPLETE_MASK << ignoreBits) %016lX (~(COMPLETE_MASK << ignoreBits)) %016lX \n", (COMPLETE_MASK << ignoreBits), (~(COMPLETE_MASK << ignoreBits)) ); \
      )
  }
}

#define DELETEBITTESTVALUE 0xAAAAAAAAAAAAAAAA

void fillShapeStack(SHAPESHACK *stack, SHAPESHACK value) {
  for (int i = 0; i < SHAPESHACKSECTIONS; i ++) {
    stack[i] = value;
  }
}

void testDeleteBit(void) {
  __private SHAPESHACK stack[SHAPESHACKSECTIONS];
  for (int i = 0; i < MAXSHAPE + 2; i++) {
    fillShapeStack(stack, DELETEBITTESTVALUE);
    deleteBit(stack, i);
    DEBUG_IF(printf("test deleteBit i %i ", i);)
    for (int i = SHAPESHACKSECTIONS - 1; i >= 0; i--) {
      DEBUG_IF(printf(" %016lX", stack[i]);)
    }
    DEBUG_IF(printf("\n");)
  }
}

void showColorStateHs (ColorState *cS
                      ,       int  numActive
                      ) {
  printf ("ColorState\n");
  printf ("{ csColors = [ ");
  for (int i = 0; i < COLORBUFFERSIZE && i < (numActive + 1); i ++) {
     if (i>0) {printf (",");}
     showColorHs(cS->csColors[i]);
     printf ("\n");
  }
  printf ("] \n } \n");
}

void showColorHs( COLOR color) {
  printf ("ColorTuple\n");
  printf ("{ redChan   = %f\n", color.s0);
  printf (", greenChan = %f\n", color.s1);
  printf (", blueChan  = %f\n", color.s2);
  printf (", alphaChan = %f\n", color.s3);
  printf ("} \n");
}

float infiniteZero(float x) {return isinf(x) ? 0 : x;}


//////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////

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
  printf("tTop: %f tBottom: %f tLeft: %f tRight: %f height: %f"
        , tTop(threshold)
        , tBottom(threshold)
        , tLeft(threshold)
        , tRight(threshold)
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
    //printf ("%i :             tileDelta %2i %2hi,%2hi\n", gTileIndex, (long)&tileHeap[gTileIndex].tileDelta - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].tileDelta.x / FIXEDFACTORINT, tileHeap[gTileIndex].tileDelta.y / FIXEDFACTORINT);
    //printf ("%i :     tileMaxThresholds %2i %i   \n", gTileIndex, (long)&tileHeap[gTileIndex].tileMaxThresholds     - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex].tileMaxThresholds     );
    printf ("%i : tileShapeSlice.sStart  %2i %hi    \n", gTileIndex, (long)&tileHeap[gTileIndex].sStart  - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex+1].sStart  );
    printf ("%i : tileShapeSlice.sLength %2i %hi    \n", gTileIndex, (long)&tileHeap[gTileIndex].sLength - (long)&tileHeap[gTileIndex], tileHeap[gTileIndex+1].sLength );
    printf ("%i :              alignment %2i        \n", gTileIndex, (long)&tileHeap[gTileIndex+1]       - (long)&tileHeap[gTileIndex]);
}

void showGroup(Group group) {
    printf ("     groupColor %2i  %2.2v4f \n", (long) &group.groupColor - (long)&group, group.groupColor);
}

void showShapeIndices( ParseState *pS
                     ,        int  numShapes
                     ) {
    int num = min(numShapes, MAXSHAPE);
    printf("Group indices %i\n", num);
    for (int i = 0; i < num; i++) {
        printf("        [%02i] -> %02i\n", i, pS->shapeIndices[i]);
    }
}

void showShapeColors(      ParseState *pS
                   , SMEM      Group *groups
                   , SMEM      Shape *shapeHeap
                   ,             int  numShapes
                   ) {
    int num = min(numShapes, MAXSHAPE);
    printf("Group indices %i\n", num);
    for (int i = 0; i < num; i++) {
        Shape shape = shapeHeap[pS->shapeIndices[i]];
        SHAPETAG tag = shape.shapeTag;
        GROUPID shapeId = shapeTagGroupId(tag);
        Group group = groups[shapeId];
        COLOR color = group.groupColor;
        printf("        [%02i] -> %02i (%2.2v4f)\n", i, pS->shapeIndices[i], color);
    }
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

void showGroups(SMEM Group *groups, int numGroups) {
  printf("groups: num = %i\n", numGroups);
  for (int n = 0; n < numGroups; n++) {
    printf(" %i\n", n);
    showGroup(groups[n]);
    //printf ("      alignment %2i \n", (long) &groups[n+1] - (long)&groups[n]);
  }
}

void showShapeRefs(SMEM REF *shapeRefs, int numShapes) {
  printf("shapeRefs: numShapes = %i\n", numShapes);
  for (int n = 0; n < numShapes; n ++ ) {
      printf ("shapeRef %2i:%2i\n", n, shapeRefs[n]);
    }
}

void showShapes(SMEM Shape *shapeHeap, SMEM GEO_ENTRY *shapeRefs, SMEM Group *groups, int numShapes) {
    for (int n = 0; n < numShapes; n ++ ) {
        Shape shape = shapeHeap[shapeRefs[n]];
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
        printf("%i - %i tag: %lX \n", shape.shapeSlice.sStart, shape.shapeSlice.sLength, shape.shapeTag);
    }
}

void showShapeStack(SHAPESHACK *shapeStack) {
  for (int i = SHAPESHACKSECTIONS - 1; i >= 0; i--) {
    printf(" %016lX", shapeStack[i]);
  }
}

void showShape(Shape shape) {
    printf (" shapeTag %2i %lX \n"    , (long) &shape.shapeTag      - (long)&shape, shape.shapeTag      );
    printf ("  shapeSlice.s %2i %i \n"     , (long) &shape.shapeSlice.sStart  - (long)&shape, shape.shapeSlice.sStart  );
    printf ("  shapeSlice.l %2i %i \n"     , (long) &shape.shapeSlice.sLength - (long)&shape, shape.shapeSlice.sLength );
}

void showShapeAlignment (SMEM Shape *shapeHeap, SMEM GEO_ENTRY *shapeRefs, SMEM Group *groups, int numShapes) {
    printf("Shape Alignment: numShapes = %i\n", numShapes);
    for (int n = 0; n < numShapes; n ++ ) {
        printf ("%2i ----------------------------------------------------------------\n",n );
        Shape shape = shapeHeap[shapeRefs[n]];
        showShape(shape);
        printf ("       alignment %2i \n"        , (long) &shapeHeap[n+1]                 - (long)&shapeHeap[n] );
        showGroup(groups[shapeTagGroupId(shape.shapeTag)]);
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

/////////////////////////////////////////////
/////////////////////////////////////////////

void showShapeStackHs(SHAPESHACK *shapeStack) {
  printf("ShapeStack {msList = [");
  for (int i = SHAPESHACKSECTIONS - 1; i >= 0; i--) {
    printf(" %lu", shapeStack[i]);
    if (i > 0) {printf(",");} else {printf("]");}
  }
  printf("}");
}

void showThresholdHs(int i, THRESHOLD threshold, HEADER header) {
  printf("Threshold \n");
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


void showThresholdsHs ( PMEM ThresholdState *tS
                      , int num
                      ) {
  for (int t = 0; t < num; t++) {
      if (t>0) {printf (",");}
      showThresholdHs(t, getThreshold(tS, t), getHeader(tS, t));
      printf ("\n");
  }
}
void showThresholdStateHs (PMEM ThresholdState *tS
                          , ParseState *pS
                          , SMEM      Group *groups
                          , SMEM       Shape *shapeHeap
                          ,             int  numShapes
                          ) {
  printf("ThresholdState\n");
  printf("{ tsRenderStart      = (%v2f)\n", tS->renderStart      );
  printf(", tsRenderEnd        = (%v2f)\n", tS->renderEnd        );
  printf(", tsThresholds = [\n");
  showThresholdsHs(tS, tS->numThresholds);
  printf("]\n");
  printf(", tsColors = [\n");
  int num = min(numShapes, MAXSHAPE);
  showColorsHs ( tS
               , pS
               , groups
               , shapeHeap
               , num
               );
  printf("]\n");
  printf("}\n");
}

void showColorsHs ( PMEM ThresholdState *tS
                  , ParseState *pS
                  , SMEM      Group *groups
                  , SMEM       Shape *shapeHeap
                  ,             int  num
                  ) {
  for (int i = 0; i < num; i++) {
      if (i>0) {printf(", ");}
      Shape shape = shapeHeap[pS->shapeIndices[i]];
      SHAPETAG tag = shape.shapeTag;
      GROUPID shapeId = shapeTagGroupId(tag);
      Group group = groups[shapeId];
      COLOR color = group.groupColor;
      showColorHs(color);printf("\n");
  }
}

void showParseStateHs( PMEM ThresholdState *tS
                      , ParseState *pS
                      , SMEM      Group *groups
                      , SMEM       Shape *shapeHeap
                      ,            COLOR color
                      ) {
  printf("ParseState\n");
  printf("{ psCurrentThreshold = %i\n"    , pS->currentThreshold );
  printf(", psNumActive        = %i\n"    , pS->numActive        );

  printf(", psSectionStart     = (%v2f)\n", pS->sectionStart     );
  printf(", psSectionEnd       = (%v2f)\n", pS->sectionEnd       );
  printf(", psPixelY           = %f\n", pS->pixelY);
  printf(", psSectionColor = "); showColorHs(color); printf("\n");
  printf(", psShapeStack = "); showShapeStackHs(pS->shapeStack); printf("\n");
  printf(", psThresholds = ["); showThresholdsHs(tS, pS->numActive);printf("]\n");
  printf("}\n");
}
