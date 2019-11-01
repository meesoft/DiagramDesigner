
(********************************************************)
(*                                                      *)
(*  Codebot Class Library at codebot.org/delphi         *)
(*  This package available at codebot.org/gdiplus       *)
(*                                                      *)
(*  Email me at sysrpl@gmail.com using the subject      *)
(*  of "gdiplus" with comments, suggestions, or         *)
(*  enhancements                                        *)
(*                                                      *)
(*  1.00.02 Open Source Released 2007                   *)
(*                                                      *)
(********************************************************)

unit GdiPlus;

interface

{$I CODEBOT.INC}
{$MINENUMSIZE 4}

uses
  Windows, ActiveX;

{ GDI+ types }

type
  Status = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );
  TStatus = Status;

  ARGB   = DWORD;
  TARGB = ARGB;
  PARGB  = ^TARGB;
  ARGB64 = Int64;

const
  ALPHA_SHIFT = 24;
  RED_SHIFT   = 16;
  GREEN_SHIFT = 8;
  BLUE_SHIFT  = 0;
  ALPHA_MASK  = (TARGB($FF) shl ALPHA_SHIFT);

type
  PixelFormat = Integer;
  TPixelFormat = PixelFormat;

const
  PixelFormatIndexed     = $00010000;
  PixelFormatGDI         = $00020000;
  PixelFormatAlpha       = $00040000;
  PixelFormatPAlpha      = $00080000;
  PixelFormatExtended    = $00100000;
  PixelFormatCanonical   = $00200000;

  PixelFormatUndefined      = 0;
  PixelFormatDontCare       = 0;

  PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat16bppGrayScale = (4  or (16 shl 8) or PixelFormatExtended);
  PixelFormat16bppRGB555    = (5  or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppRGB565    = (6  or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  PixelFormat24bppRGB       = (8  or (24 shl 8) or PixelFormatGDI);
  PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha  or PixelFormatCanonical or PixelFormatExtended);
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha  or PixelFormatPAlpha or PixelFormatExtended);
  PixelFormatMax            = 15;

type
  TColor = record
    case Integer of
      1: (C: TARGB);
      2: (B, G, R, A: Byte);
  end;

  TColorMatrix = array [0..4, 0..4] of Single;
  PColorMatrix = ^TColorMatrix;

  TColorRow = array[0..4] of Single;
  PColorRow = ^TColorRow;

  TColorTransform = record
    Gamma: Single;
    Brightness: Single;
    Contrast: Single;
    Saturation: Single;
    Opacity: Single;
    Greyscale: Boolean;
    Negative: Boolean;
  end;

  TSizeF = record
    Width: Single;
    Height: Single;
  end;
  PSizeF = ^TSizeF;

  TSizeI = record
    Width: Integer;
    Height: Integer;
  end;
  PSizeI = ^TSizeI;

  TPointF = record
    X: Single;
    Y: Single;
  end;
  PPointF = ^TPointF;

  TPointI = record
    X: Integer;
    Y: Integer;
  end;
  PPointI = ^TPointI;

  TRectF = record
    case Boolean of
      False: (
        X: Single;
        Y: Single;
        Width: Single;
        Height: Single);
      True: (
        Point: TPointF;
        Size: TSizeF;
      );
  end;
  PRectF = ^TRectF;

  TRectI = record
    case Boolean of
      False: (
        X: Integer;
        Y: Integer;
        Width: Integer;
        Height: Integer);
      True: (
        Point: TPointI;
        Size: TSizeI;
      );
  end;
  PRectI = ^TRectI;

function NewColor(R, G, B: Byte): TColor; overload;
function NewColor(R, G, B, A: Byte): TColor; overload;
function NewColor(ColorRef: TColorRef): TColor; overload;

function NewColorMatrix: TColorMatrix; overload;
function NewColorMatrix(Opacity: Single): TColorMatrix; overload;
function NewColorMatrix(R, G, B, A: Byte): TColorMatrix; overload;
function NewColorMatrix(R, G, B, A: Single): TColorMatrix; overload;
function NewColorMatrix(Color: TColor): TColorMatrix; overload;
function NewColorMatrix(ColorRef: TColorRef): TColorMatrix; overload;

function NewColorTransform: TColorTransform;

{ Color operations }

procedure ColorFill(C: PSingle; Data: array of Single);
function ColorMultiply(const A, B: TColorMatrix): TColorMatrix;
function ColorBrightness(const M: TColorMatrix; B: Single): TColorMatrix;
function ColorContrast(const M: TColorMatrix; C: Single): TColorMatrix;
function ColorSaturate(const M: TColorMatrix; S: Single): TColorMatrix;
function ColorOpacity(const M: TColorMatrix; O: Single): TColorMatrix;
function ColorGreyscale(const M: TColorMatrix): TColorMatrix;
function ColorNegative(const M: TColorMatrix): TColorMatrix;
function ColorTransform(const Transform: TColorTransform): TColorMatrix;

function NewSize(Width, Height: Integer): TSizeI; overload;
function NewSize(Width, Height: Single): TSizeF; overload;
function NewSize(const Point: TPointI): TSizeI; overload;
function NewSize(const Point: TPointF): TSizeF; overload;

function NewPoint(X, Y: Integer): TPointI; overload;
function NewPoint(X, Y: Single): TPointF; overload;
function NewPoint(const Size: TSizeI): TPointI; overload;
function NewPoint(const Size: TSizeF): TPointF; overload;

function NewPoint(const Point: TPoint): TPointI; overload;

function NewRect(Width, Height: Integer): TRectI; overload;
function NewRect(Width, Height: Integer; Scale: Single): TRectI; overload;
function NewRect(X, Y, Width, Height: Integer): TRectI; overload;
function NewRect(const Point: TPointI; Width, Height: Integer): TRectI; overload;
function NewRect(const Point: TPointI; const Size: TSizeI): TRectI; overload;
function NewRect(X, Y: Integer; const Size: TSizeI): TRectI; overload;
function NewRect(Width, Height: Single): TRectF; overload;
function NewRect(X, Y, Width, Height: Single): TRectF; overload;
function NewRect(const Point: TPointF; Width, Height: Single): TRectF; overload;
function NewRect(const Point: TPointF; const Size: TSizeF): TRectF; overload;
function NewRect(X, Y: Integer; const Size: TSizeF): TRectF; overload;

function NewRect(const Rect: TRect): TRectI; overload;

function OffsetRect(const Rect: TRectI; X, Y: Integer): TRectI; overload;
function OffsetRect(const Rect: TRectF; X, Y: Single): TRectF; overload;
function InflateRect(const Rect: TRectI; X, Y: Integer): TRectI; overload;
function InflateRect(const Rect: TRectF; X, Y: Single): TRectF; overload;

type
{$IFDEF D6_UP}
  PaletteFlags = (
    PaletteFlagsHasAlpha = $0001,
    PaletteFlagsGrayScale = $0002,
    PaletteFlagsHalftone = $0004
  );
  TPaletteFlags = PaletteFlags;
{$ELSE}
  PaletteFlags = Integer;
  
const
  PaletteFlagsHasAlpha = $0001;
  PaletteFlagsGrayScale = $0002;
  PaletteFlagsHalftone = $0004;

type
  TPaletteFlags = PaletteFlags;
{$ENDIF}

  ColorPalette = packed record
    Flags: UINT;
    Count: UINT;
    Entries: array [0..0] of ARGB
  end;
  TColorPalette = ColorPalette;
  PColorPalette = ^TColorPalette;

{$IFDEF D6_UP}
  QualityMode = (
    QualityModeInvalid = -1,  
    QualityModeDefault = 0,
    QualityModeLow = 1,       
    QualityModeHigh = 2
  );
  TQualityMode = QualityMode;
{$ELSE}
  QualityMode = Integer;

const
  QualityModeInvalid = -1;  
  QualityModeDefault = 0;
  QualityModeLow = 1;      
  QualityModeHigh = 2;     
{$ENDIF}

type
  CompositingMode = (
    CompositingModeSourceOver,
    CompositingModeSourceCopy
  );
  TCompositingMode = CompositingMode;

{$IFDEF D6_UP}
  CompositingQuality = (
    CompositingQualityInvalid = Ord(QualityModeInvalid),         
    CompositingQualityDefault = Ord(QualityModeDefault),         
    CompositingQualityHighSpeed = Ord(QualityModeLow),       
    CompositingQualityHighQuality = Ord(QualityModeHigh),     
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );
  TCompositingQuality = CompositingQuality;
{$ELSE}
  CompositingQuality = Integer;

const
  CompositingQualityInvalid = QualityModeInvalid;         
  CompositingQualityDefault = QualityModeDefault;         
  CompositingQualityHighSpeed = QualityModeLow;       
  CompositingQualityHighQuality = QualityModeHigh;
  CompositingQualityGammaCorrected = 3;
  CompositingQualityAssumeLinear = 4;    

type
  TCompositingQuality = CompositingQuality;
{$ENDIF}

  Unit_ = (
    UnitWorld,
    UnitDisplay,
    UnitPixel,
    UnitPoint,
    UnitInch,
    UnitDocument,
    UnitMillimeter
  );
  TUnit = Unit_;

{$IFDEF D6_UP}
  MetafileFrameUnit = (
    MetafileFrameUnitPixel = Ord(UnitPixel),     
    MetafileFrameUnitPoint = Ord(UnitPoint),     
    MetafileFrameUnitInch = Ord(UnitInch),
    MetafileFrameUnitDocument = Ord(UnitDocument),  
    MetafileFrameUnitMillimeter = Ord(UnitMillimeter),
    MetafileFrameUnitGdi
  );
  TMetafileFrameUnit = MetafileFrameUnit;
{$ELSE}
  MetafileFrameUnit = Integer;

const
  MetafileFrameUnitPixel = 2;     
  MetafileFrameUnitPoint = 3;     
  MetafileFrameUnitInch = 4;      
  MetafileFrameUnitDocument = 5;  
  MetafileFrameUnitMillimeter = 6;
  MetafileFrameUnitGdi = 7;       

type
  TMetafileFrameUnit = MetafileFrameUnit;
{$ENDIF}

  CoordinateSpace = (
    CoordinateSpaceWorld,
    CoordinateSpacePage,
    CoordinateSpaceDevice
  );
  TCoordinateSpace = CoordinateSpace;

  WrapMode = (
    WrapModeTile,
    WrapModeTileFlipX,
    WrapModeTileFlipY,
    WrapModeTileFlipXY,
    WrapModeClamp
  );
  TWrapMode = WrapMode;

  HatchStyle = (
    HatchStyleHorizontal,
    HatchStyleVertical,
    HatchStyleForwardDiagonal,
    HatchStyleBackwardDiagonal,
    HatchStyleCross,
    HatchStyleDiagonalCross,
    HatchStyle05Percent,
    HatchStyle10Percent,
    HatchStyle20Percent,
    HatchStyle25Percent,
    HatchStyle30Percent,
    HatchStyle40Percent,
    HatchStyle50Percent,
    HatchStyle60Percent,
    HatchStyle70Percent,
    HatchStyle75Percent,
    HatchStyle80Percent,
    HatchStyle90Percent,
    HatchStyleLightDownwardDiagonal,
    HatchStyleLightUpwardDiagonal,
    HatchStyleDarkDownwardDiagonal,
    HatchStyleDarkUpwardDiagonal,
    HatchStyleWideDownwardDiagonal,
    HatchStyleWideUpwardDiagonal,
    HatchStyleLightVertical,
    HatchStyleLightHorizontal,
    HatchStyleNarrowVertical,
    HatchStyleNarrowHorizontal,
    HatchStyleDarkVertical,
    HatchStyleDarkHorizontal,
    HatchStyleDashedDownwardDiagonal,
    HatchStyleDashedUpwardDiagonal,
    HatchStyleDashedHorizontal,
    HatchStyleDashedVertical,
    HatchStyleSmallConfetti,
    HatchStyleLargeConfetti,
    HatchStyleZigZag,
    HatchStyleWave,
    HatchStyleDiagonalBrick,
    HatchStyleHorizontalBrick,
    HatchStyleWeave,
    HatchStylePlaid,
    HatchStyleDivot,
    HatchStyleDottedGrid,
    HatchStyleDottedDiamond,
    HatchStyleShingle,
    HatchStyleTrellis,
    HatchStyleSphere,
    HatchStyleSmallGrid,
    HatchStyleSmallCheckerBoard,
    HatchStyleLargeCheckerBoard,
    HatchStyleOutlinedDiamond,
    HatchStyleSolidDiamond,
    HatchStyleTotal
  );

const
  HatchStyleLargeGrid = HatchStyleCross;
  HatchStyleMin = HatchStyleHorizontal;      
  HatchStyleMax = HatchStyleSolidDiamond;      

type
  THatchStyle = HatchStyle;

  DashStyle = (
    DashStyleSolid,
    DashStyleDash,
    DashStyleDot,
    DashStyleDashDot,
    DashStyleDashDotDot,
    DashStyleCustom
  );
  TDashStyle = DashStyle;

{$IFDEF D6_UP}
  DashCap = (
    DashCapFlat = 0,            
    DashCapRound = 2,           
    DashCapTriangle = 3
  );
  TDashCap = DashCap;
{$ELSE}
  DashCap = Integer;

const
  DashCapFlat = 0;            
  DashCapRound = 2;           
  DashCapTriangle = 3;        

type
  TDashCap = DashCap;
{$ENDIF}

{$IFDEF D6_UP}
  LineCap = (
    LineCapFlat = 0,
    LineCapSquare = 1,          
    LineCapRound = 2,           
    LineCapTriangle = 3,
    LineCapNoAnchor = $10,        
    LineCapSquareAnchor = $11,    
    LineCapRoundAnchor = $12,     
    LineCapDiamondAnchor = $13,   
    LineCapArrowAnchor = $14,
    LineCapCustom = $FF,          
    LineCapAnchorMask = $F0      
  );
  TLineCap = LineCap;
{$ELSE}
  LineCap = Integer;
  const
    LineCapFlat = 0;            
    LineCapSquare = 1;
    LineCapRound = 2;           
    LineCapTriangle = 3;        
    LineCapNoAnchor = $10;        
    LineCapSquareAnchor = $11;
    LineCapRoundAnchor = $12;     
    LineCapDiamondAnchor = $13;   
    LineCapArrowAnchor = $14;     
    LineCapCustom = $FF;
    LineCapAnchorMask = $F0;      

type
  TLineCap = LineCap;
{$ENDIF}

  CustomLineCapType = (
    CustomLineCapTypeDefault,
    CustomLineCapTypeAdjustableArrow
  );
  TCustomLineCapType = CustomLineCapType;

  LineJoin = (
    LineJoinMiter,
    LineJoinBevel,
    LineJoinRound,
    LineJoinMiterClipped
  );
  TLineJoin = LineJoin;

{$IFDEF D6_UP}
  {$Z1}
  PathPointType = (
    PathPointTypeStart = $00,          
    PathPointTypeLine = $01,           
    PathPointTypeBezier = $03,         
    PathPointTypePathTypeMask = $07,   
    PathPointTypeDashMode = $10,       
    PathPointTypePathMarker = $20,     
    PathPointTypeCloseSubpath = $80,
    PathPointTypeBezier3 = $03        
  );
  TPathPointType = PathPointType;
  {$Z4}
{$ELSE}
  PathPointType = Byte;

const
  PathPointTypeStart : Byte = $00;         
  PathPointTypeLine : Byte = $01;
  PathPointTypeBezier : Byte = $03;        
  PathPointTypePathTypeMask : Byte = $07;  
  PathPointTypeDashMode : Byte = $10;      
  PathPointTypePathMarker : Byte = $20;    
  PathPointTypeCloseSubpath : Byte = $80;  
  PathPointTypeBezier3 : Byte = $03;       

type
  TPathPointType = PathPointType;
{$ENDIF}

  WarpMode = (
    WarpModePerspective,
    WarpModeBilinear
  );
  TWarpMode = WarpMode;

  LinearGradientMode = (
    LinearGradientModeHorizontal,
    LinearGradientModeVertical,
    LinearGradientModeForwardDiagonal,
    LinearGradientModeBackwardDiagonal
  );
  TLinearGradientMode = LinearGradientMode;

  CombineMode = (
    CombineModeReplace,
    CombineModeIntersect,
    CombineModeUnion,
    CombineModeXor,
    CombineModeExclude,
    CombineModeComplement
  );
  TCombineMode = CombineMode;

  ImageType = (
    ImageTypeUnknown,
    ImageTypeBitmap,
    ImageTypeMetafile
  );
  TImageType = ImageType;

{$IFDEF D6_UP}
  InterpolationMode = (
    InterpolationModeInvalid = Ord(QualityModeInvalid),         
    InterpolationModeDefault = Ord(QualityModeDefault),         
    InterpolationModeLowQuality = Ord(QualityModeLow),      
    InterpolationModeHighQuality = Ord(QualityModeHigh),     
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );
  TInterpolationMode = InterpolationMode;
{$ELSE}

  InterpolationMode = Integer;

const
  InterpolationModeInvalid = QualityModeInvalid;            
  InterpolationModeDefault = QualityModeDefault;            
  InterpolationModeLowQuality = QualityModeLow;         
  InterpolationModeHighQuality = QualityModeHigh;        
  InterpolationModeBilinear = 3;           
  InterpolationModeBicubic = 4;            
  InterpolationModeNearestNeighbor = 5;    
  InterpolationModeHighQualityBilinear = 6;
  InterpolationModeHighQualityBicubic = 7; 

type
  TInterpolationMode = InterpolationMode;
{$ENDIF}

  PenAlignment = (
    PenAlignmentCenter,
    PenAlignmentInset
  );
  TPenAlignment = PenAlignment;

  BrushType = (
   BrushTypeSolidColor,
   BrushTypeHatchFill,
   BrushTypeTextureFill,
   BrushTypePathGradient,
   BrushTypeLinearGradient
  );
  TBrushType = BrushType;

{$IFDEF D6_UP}
  PenType = (
   PenTypeSolidColor = Ord(BrushTypeSolidColor),      
   PenTypeHatchFill = Ord(BrushTypeHatchFill),
   PenTypeTextureFill = Ord(BrushTypeTextureFill),     
   PenTypePathGradient = Ord(BrushTypePathGradient),    
   PenTypeLinearGradient = Ord(BrushTypeLinearGradient),  
   PenTypeUnknown = -1
  );
  TPenType = PenType;
{$ELSE}
  PenType = Integer;

const
  PenTypeSolidColor = 0;      
  PenTypeHatchFill = 1;       
  PenTypeTextureFill = 2;     
  PenTypePathGradient = 3;    
  PenTypeLinearGradient = 4;  
  PenTypeUnknown = -1;         

type
  TPenType = PenType;
{$ENDIF}

  MatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend
  );
  TMatrixOrder = MatrixOrder;

  GenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace
  );
  TGenericFontFamily = GenericFontFamily;

type
  FontStyle = Integer;

const
  FontStyleRegular = Integer(0);
  FontStyleBold = Integer(1);      
  FontStyleItalic = Integer(2);    
  FontStyleBoldItalic = Integer(3);
  FontStyleUnderline = Integer(4); 
  FontStyleStrikeout = Integer(8); 

type
  TFontStyle = FontStyle;

{$IFDEF D6_UP}
  SmoothingMode = (
    SmoothingModeInvalid = Ord(QualityModeInvalid),    
    SmoothingModeDefault = Ord(QualityModeDefault),    
    SmoothingModeHighSpeed = Ord(QualityModeLow),  
    SmoothingModeHighQuality = Ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias
  );
  TSmoothingMode = SmoothingMode;
{$ELSE}
  SmoothingMode = Integer;

const
  SmoothingModeInvalid = QualityModeInvalid;    
  SmoothingModeDefault = QualityModeDefault;    
  SmoothingModeHighSpeed = QualityModeLow;  
  SmoothingModeHighQuality = QualityModeHigh;
  SmoothingModeNone = 3;       
  SmoothingModeAntiAlias = 4;  

type
  TSmoothingMode = SmoothingMode;
{$ENDIF}

{$IFDEF D6_UP}
  PixelOffsetMode = (
    PixelOffsetModeInvalid = Ord(QualityModeInvalid),
    PixelOffsetModeDefault = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed = Ord(QualityModeLow),  
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,
    PixelOffsetModeHalf
  );
  TPixelOffsetMode = PixelOffsetMode;
{$ELSE}
  PixelOffsetMode = Integer;

const
  PixelOffsetModeInvalid = QualityModeInvalid;    
  PixelOffsetModeDefault = QualityModeDefault;    
  PixelOffsetModeHighSpeed = QualityModeLow;  
  PixelOffsetModeHighQuality = QualityModeHigh;
  PixelOffsetModeNone = 3;
  PixelOffsetModeHalf = 4;       

type
  TPixelOffsetMode = PixelOffsetMode;
{$ENDIF}

  TextRenderingHint = (
    TextRenderingHintSystemDefault,
    TextRenderingHintSingleBitPerPixelGridFit,
    TextRenderingHintSingleBitPerPixel,
    TextRenderingHintAntiAliasGridFit,
    TextRenderingHintAntiAlias,
    TextRenderingHintClearTypeGridFit
  );
  TTextRenderingHint = TextRenderingHint;

  MetafileType = (
    MetafileTypeInvalid,
    MetafileTypeWmf,
    MetafileTypeWmfPlaceable,
    MetafileTypeEmf,
    MetafileTypeEmfPlusOnly,
    MetafileTypeEmfPlusDual
  );
  TMetafileType = MetafileType;

{$IFDEF D6_UP}
  EmfType = (
    EmfTypeEmfOnly = Ord(MetafileTypeEmf),
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)
  );
  TEmfType = EmfType;
{$ELSE}
  EmfType = Integer;

const
  EmfTypeEmfOnly = Ord(MetafileTypeEmf);
  EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly);
  EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual);

type
  TEmfType = EmfType;
{$ENDIF}

  ObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap
  );
  TObjectType = ObjectType;

const
  ObjectTypeMax = ObjectTypeCustomLineCap;
  ObjectTypeMin = ObjectTypeBrush;

type
  ColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );
  TColorMatrixFlags = ColorMatrixFlags;

  ColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny
  );
  TColorAdjustType = ColorAdjustType;

type
{$IFDEF D6_UP}
  ImageFlags = (
    ImageFlagsNone = 0,               
    ImageFlagsScalable = $0001,           
    ImageFlagsHasAlpha = $0002,
    ImageFlagsHasTranslucent = $0004,     
    ImageFlagsPartiallyScalable = $0008,  
    ImageFlagsColorSpaceRGB = $0010,      
    ImageFlagsColorSpaceCMYK = $0020,     
    ImageFlagsColorSpaceGRAY = $0040,     
    ImageFlagsColorSpaceYCBCR = $0080,    
    ImageFlagsColorSpaceYCCK = $0100,     
    ImageFlagsHasRealDPI = $1000,         
    ImageFlagsHasRealPixelSize = $2000,
    ImageFlagsReadOnly = $00010000,           
    ImageFlagsCaching = $00020000            
  );
  TImageFlags = ImageFlags;
{$ELSE}
  ImageFlags = Integer;

const
  ImageFlagsNone = 0;
  ImageFlagsScalable = $0001;
  ImageFlagsHasAlpha = $0002;
  ImageFlagsHasTranslucent = $0004;
  ImageFlagsPartiallyScalable = $0008;
  ImageFlagsColorSpaceRGB = $0010;
  ImageFlagsColorSpaceCMYK = $0020;
  ImageFlagsColorSpaceGRAY = $0040;
  ImageFlagsColorSpaceYCBCR = $0080;
  ImageFlagsColorSpaceYCCK = $0100;
  ImageFlagsHasRealDPI = $1000;
  ImageFlagsHasRealPixelSize = $2000;
  ImageFlagsReadOnly = $00010000;
  ImageFlagsCaching = $00020000;
  
type
  TImageFlags = ImageFlags;
{$ENDIF}

{$IFDEF D6_UP}
  RotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone = 1,  
    Rotate180FlipNone = 2, 
    Rotate270FlipNone = 3, 
    RotateNoneFlipX = 4,   
    Rotate90FlipX = 5,
    Rotate180FlipX = 6,
    Rotate270FlipX = 7,    
    RotateNoneFlipY = Rotate180FlipX,
    Rotate90FlipY = Rotate270FlipX,     
    Rotate180FlipY = RotateNoneFlipX,    
    Rotate270FlipY = Rotate90FlipX,    
    RotateNoneFlipXY = Rotate180FlipNone,  
    Rotate90FlipXY = Rotate270FlipNone,    
    Rotate180FlipXY = RotateNoneFlipNone,   
    Rotate270FlipXY = Rotate90FlipNone   
  );
  TRotateFlipType = RotateFlipType;
{$ELSE}
  RotateFlipType = (
    RotateNoneFlipNone, 
    Rotate90FlipNone,   
    Rotate180FlipNone,  
    Rotate270FlipNone,  
    RotateNoneFlipX,    
    Rotate90FlipX,      
    Rotate180FlipX,     
    Rotate270FlipX      
  );

const
  RotateNoneFlipY = Rotate180FlipX;
  Rotate90FlipY = Rotate270FlipX;
  Rotate180FlipY = RotateNoneFlipX;
  Rotate270FlipY = Rotate90FlipX;
  RotateNoneFlipXY = Rotate180FlipNone;
  Rotate90FlipXY = Rotate270FlipNone;
  Rotate180FlipXY = RotateNoneFlipNone;
  Rotate270FlipXY = Rotate90FlipNone;
  
type
  TRotateFlipType = RotateFlipType;
{$ENDIF}

  EncoderParameter = packed record
    Guid: TGUID;
    NumberOfValues: ULONG;
    Type_: ULONG;
    Value: Pointer;
  end;
  TEncoderParameter = EncoderParameter;
  PEncoderParameter = ^TEncoderParameter;

  EncoderParameters = packed record
    Count: UINT;
    Parameter: array[0..0] of TEncoderParameter;
  end;
  TEncoderParameters = EncoderParameters;
  PEncoderParameters = ^TEncoderParameters;

  PropertyItem = record
    id: PROPID;         
    length: ULONG;      
    type_: WORD;        
    value: Pointer;     
  end;
  TPropertyItem = PropertyItem;
  PPropertyItem = ^TPropertyItem;

const
  PropertyTagTypeByte: Integer = 1;
  PropertyTagTypeASCII: Integer = 2;     
  PropertyTagTypeShort: Integer = 3;     
  PropertyTagTypeLong: Integer = 4;      
  PropertyTagTypeRational: Integer = 5;  
  PropertyTagTypeUndefined: Integer = 7; 
  PropertyTagTypeSLONG: Integer = 9;
  PropertyTagTypeSRational: Integer = 10;

  PropertyTagExifIFD                        = $8769;
  PropertyTagGpsIFD                         = $8825;
  PropertyTagNewSubfileType                 = $00FE;
  PropertyTagSubfileType                    = $00FF;
  PropertyTagImageWidth                     = $0100;
  PropertyTagImageHeight                    = $0101;
  PropertyTagBitsPerSample                  = $0102;
  PropertyTagCompression                    = $0103;
  PropertyTagPhotometricInterp              = $0106;
  PropertyTagThreshHolding                  = $0107;
  PropertyTagCellWidth                      = $0108;
  PropertyTagCellHeight                     = $0109;
  PropertyTagFillOrder                      = $010A;
  PropertyTagDocumentName                   = $010D;
  PropertyTagImageDescription               = $010E;
  PropertyTagEquipMake                      = $010F;
  PropertyTagEquipModel                     = $0110;
  PropertyTagStripOffsets                   = $0111;
  PropertyTagOrientation                    = $0112;
  PropertyTagSamplesPerPixel                = $0115;
  PropertyTagRowsPerStrip                   = $0116;
  PropertyTagStripBytesCount                = $0117;
  PropertyTagMinSampleValue                 = $0118;
  PropertyTagMaxSampleValue                 = $0119;
  PropertyTagXResolution                    = $011A;
  PropertyTagYResolution                    = $011B;
  PropertyTagPlanarConfig                   = $011C;
  PropertyTagPageName                       = $011D;
  PropertyTagXPosition                      = $011E;
  PropertyTagYPosition                      = $011F;
  PropertyTagFreeOffset                     = $0120;
  PropertyTagFreeByteCounts                 = $0121;
  PropertyTagGrayResponseUnit               = $0122;
  PropertyTagGrayResponseCurve              = $0123;
  PropertyTagT4Option                       = $0124;
  PropertyTagT6Option                       = $0125;
  PropertyTagResolutionUnit                 = $0128;
  PropertyTagPageNumber                     = $0129;
  PropertyTagTransferFuncition              = $012D;
  PropertyTagSoftwareUsed                   = $0131;
  PropertyTagDateTime                       = $0132;
  PropertyTagArtist                         = $013B;
  PropertyTagHostComputer                   = $013C;
  PropertyTagPredictor                      = $013D;
  PropertyTagWhitePoint                     = $013E;
  PropertyTagPrimaryChromaticities          = $013F;
  PropertyTagColorMap                       = $0140;
  PropertyTagHalftoneHints                  = $0141;
  PropertyTagTileWidth                      = $0142;
  PropertyTagTileLength                     = $0143;
  PropertyTagTileOffset                     = $0144;
  PropertyTagTileByteCounts                 = $0145;
  PropertyTagInkSet                         = $014C;
  PropertyTagInkNames                       = $014D;
  PropertyTagNumberOfInks                   = $014E;
  PropertyTagDotRange                       = $0150;
  PropertyTagTargetPrinter                  = $0151;
  PropertyTagExtraSamples                   = $0152;
  PropertyTagSampleFormat                   = $0153;
  PropertyTagSMinSampleValue                = $0154;
  PropertyTagSMaxSampleValue                = $0155;
  PropertyTagTransferRange                  = $0156;
  PropertyTagJPEGProc                       = $0200;
  PropertyTagJPEGInterFormat                = $0201;
  PropertyTagJPEGInterLength                = $0202;
  PropertyTagJPEGRestartInterval            = $0203;
  PropertyTagJPEGLosslessPredictors         = $0205;
  PropertyTagJPEGPointTransforms            = $0206;
  PropertyTagJPEGQTables                    = $0207;
  PropertyTagJPEGDCTables                   = $0208;
  PropertyTagJPEGACTables                   = $0209;
  PropertyTagYCbCrCoefficients              = $0211;
  PropertyTagYCbCrSubsampling               = $0212;
  PropertyTagYCbCrPositioning               = $0213;
  PropertyTagREFBlackWhite                  = $0214;
  PropertyTagICCProfile                     = $8773;
  PropertyTagGamma                          = $0301;
  PropertyTagICCProfileDescriptor           = $0302;
  PropertyTagSRGBRenderingIntent            = $0303;
  PropertyTagImageTitle                     = $0320;
  PropertyTagCopyright                      = $8298;

  PropertyTagResolutionXUnit                = $5001;
  PropertyTagResolutionYUnit                = $5002;
  PropertyTagResolutionXLengthUnit          = $5003;
  PropertyTagResolutionYLengthUnit          = $5004;
  PropertyTagPrintFlags                     = $5005;
  PropertyTagPrintFlagsVersion              = $5006;
  PropertyTagPrintFlagsCrop                 = $5007;
  PropertyTagPrintFlagsBleedWidth           = $5008;
  PropertyTagPrintFlagsBleedWidthScale      = $5009;
  PropertyTagHalftoneLPI                    = $500A;
  PropertyTagHalftoneLPIUnit                = $500B;
  PropertyTagHalftoneDegree                 = $500C;
  PropertyTagHalftoneShape                  = $500D;
  PropertyTagHalftoneMisc                   = $500E;
  PropertyTagHalftoneScreen                 = $500F;
  PropertyTagJPEGQuality                    = $5010;
  PropertyTagGridSize                       = $5011;
  PropertyTagThumbnailFormat                = $5012;
  PropertyTagThumbnailWidth                 = $5013;
  PropertyTagThumbnailHeight                = $5014;
  PropertyTagThumbnailColorDepth            = $5015;
  PropertyTagThumbnailPlanes                = $5016;
  PropertyTagThumbnailRawBytes              = $5017;
  PropertyTagThumbnailSize                  = $5018;
  PropertyTagThumbnailCompressedSize        = $5019;
  PropertyTagColorTransferFunction          = $501A;
  PropertyTagThumbnailData                  = $501B;

  PropertyTagThumbnailImageWidth            = $5020;
  PropertyTagThumbnailImageHeight           = $5021;
  PropertyTagThumbnailBitsPerSample         = $5022;
  PropertyTagThumbnailCompression           = $5023;
  PropertyTagThumbnailPhotometricInterp     = $5024;
  PropertyTagThumbnailImageDescription      = $5025;
  PropertyTagThumbnailEquipMake             = $5026;
  PropertyTagThumbnailEquipModel            = $5027;
  PropertyTagThumbnailStripOffsets          = $5028;
  PropertyTagThumbnailOrientation           = $5029;
  PropertyTagThumbnailSamplesPerPixel       = $502A;
  PropertyTagThumbnailRowsPerStrip          = $502B;
  PropertyTagThumbnailStripBytesCount       = $502C;
  PropertyTagThumbnailResolutionX           = $502D;
  PropertyTagThumbnailResolutionY           = $502E;
  PropertyTagThumbnailPlanarConfig          = $502F;
  PropertyTagThumbnailResolutionUnit        = $5030;
  PropertyTagThumbnailTransferFunction      = $5031;
  PropertyTagThumbnailSoftwareUsed          = $5032;
  PropertyTagThumbnailDateTime              = $5033;
  PropertyTagThumbnailArtist                = $5034;
  PropertyTagThumbnailWhitePoint            = $5035;
  PropertyTagThumbnailPrimaryChromaticities = $5036;
  PropertyTagThumbnailYCbCrCoefficients     = $5037;
  PropertyTagThumbnailYCbCrSubsampling      = $5038;
  PropertyTagThumbnailYCbCrPositioning      = $5039;
  PropertyTagThumbnailRefBlackWhite         = $503A;
  PropertyTagThumbnailCopyRight             = $503B;
  PropertyTagLuminanceTable                 = $5090;
  PropertyTagChrominanceTable               = $5091;
  PropertyTagFrameDelay                     = $5100;
  PropertyTagLoopCount                      = $5101;
  PropertyTagPixelUnit                      = $5110;
  PropertyTagPixelPerUnitX                  = $5111;
  PropertyTagPixelPerUnitY                  = $5112;
  PropertyTagPaletteHistogram               = $5113;
  PropertyTagExifExposureTime               = $829A;
  PropertyTagExifFNumber                    = $829D;
  PropertyTagExifExposureProg               = $8822;
  PropertyTagExifSpectralSense              = $8824;
  PropertyTagExifISOSpeed                   = $8827;
  PropertyTagExifOECF                       = $8828;
  PropertyTagExifVer                        = $9000;
  PropertyTagExifDTOrig                     = $9003;
  PropertyTagExifDTDigitized                = $9004;
  PropertyTagExifCompConfig                 = $9101;
  PropertyTagExifCompBPP                    = $9102;
  PropertyTagExifShutterSpeed               = $9201;
  PropertyTagExifAperture                   = $9202;
  PropertyTagExifBrightness                 = $9203;
  PropertyTagExifExposureBias               = $9204;
  PropertyTagExifMaxAperture                = $9205;
  PropertyTagExifSubjectDist                = $9206;
  PropertyTagExifMeteringMode               = $9207;
  PropertyTagExifLightSource                = $9208;
  PropertyTagExifFlash                      = $9209;
  PropertyTagExifFocalLength                = $920A;
  PropertyTagExifMakerNote                  = $927C;
  PropertyTagExifUserComment                = $9286;
  PropertyTagExifDTSubsec                   = $9290;
  PropertyTagExifDTOrigSS                   = $9291;
  PropertyTagExifDTDigSS                    = $9292;
  PropertyTagExifFPXVer                     = $A000;
  PropertyTagExifColorSpace                 = $A001;
  PropertyTagExifPixXDim                    = $A002;
  PropertyTagExifPixYDim                    = $A003;
  PropertyTagExifRelatedWav                 = $A004;
  PropertyTagExifInterop                    = $A005;
  PropertyTagExifFlashEnergy                = $A20B;
  PropertyTagExifSpatialFR                  = $A20C;
  PropertyTagExifFocalXRes                  = $A20E;
  PropertyTagExifFocalYRes                  = $A20F;
  PropertyTagExifFocalResUnit               = $A210;
  PropertyTagExifSubjectLoc                 = $A214;
  PropertyTagExifExposureIndex              = $A215;
  PropertyTagExifSensingMethod              = $A217;
  PropertyTagExifFileSource                 = $A300;
  PropertyTagExifSceneType                  = $A301;
  PropertyTagExifCfaPattern                 = $A302;
  PropertyTagGpsVer                         = $0000;
  PropertyTagGpsLatitudeRef                 = $0001;
  PropertyTagGpsLatitude                    = $0002;
  PropertyTagGpsLongitudeRef                = $0003;
  PropertyTagGpsLongitude                   = $0004;
  PropertyTagGpsAltitudeRef                 = $0005;
  PropertyTagGpsAltitude                    = $0006;
  PropertyTagGpsGpsTime                     = $0007;
  PropertyTagGpsGpsSatellites               = $0008;
  PropertyTagGpsGpsStatus                   = $0009;
  PropertyTagGpsGpsMeasureMode              = $000A;
  PropertyTagGpsGpsDop                      = $000B;
  PropertyTagGpsSpeedRef                    = $000C;
  PropertyTagGpsSpeed                       = $000D;
  PropertyTagGpsTrackRef                    = $000E;
  PropertyTagGpsTrack                       = $000F;
  PropertyTagGpsImgDirRef                   = $0010;
  PropertyTagGpsImgDir                      = $0011;
  PropertyTagGpsMapDatum                    = $0012;
  PropertyTagGpsDestLatRef                  = $0013;
  PropertyTagGpsDestLat                     = $0014;
  PropertyTagGpsDestLongRef                 = $0015;
  PropertyTagGpsDestLong                    = $0016;
  PropertyTagGpsDestBearRef                 = $0017;
  PropertyTagGpsDestBear                    = $0018;
  PropertyTagGpsDestDistRef                 = $0019;
  PropertyTagGpsDestDist                    = $001A;

  ImageFormatUndefined: TGUID = '{B96B3CA9-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatMemoryBMP: TGUID = '{B96B3CAA-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatBMP: TGUID = '{B96B3CAB-0728-11D3-9D7B-0000F81EF32E}';       
  ImageFormatEMF: TGUID = '{B96B3CAC-0728-11D3-9D7B-0000F81EF32E}';       
  ImageFormatWMF: TGUID = '{B96B3CAD-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatJPEG: TGUID = '{B96B3CAE-0728-11D3-9D7B-0000F81EF32E}';      
  ImageFormatPNG: TGUID = '{B96B3CAF-0728-11D3-9D7B-0000F81EF32E}';       
  ImageFormatGIF: TGUID = '{B96B3CB0-0728-11D3-9D7B-0000F81EF32E}';       
  ImageFormatTIFF: TGUID = '{B96B3CB1-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatEXIF: TGUID = '{B96B3CB2-0728-11D3-9D7B-0000F81EF32E}';      
  ImageFormatIcon: TGUID = '{B96B3CB5-0728-11D3-9D7B-0000F81EF32E}';

  FrameDimensionTime: TGUID = '{6AEDBD6D-3FB5-418A-83A6-7F45229DC872}';
  FrameDimensionResolution: TGUID = '{84236F7B-3BD3-428F-8DAB-4EA1439CA315}';
  FrameDimensionPage: TGUID = '{7462DC86-6180-4C7E-8E3F-EE7333A7A483}';

  FormatIDImageInformation: TGUID = '{E5836CBE-5EEF-4F1D-ACDE-AE4C43B608CE}';
  FormatIDJpegAppHeaders: TGUID = '{1C4AFDCD-6177-43CF-ABC7-5F51AF39EE85}';

  EncoderCompression: TGUID = '{E09D739D-CCD4-44EE-8EBA-3FBF8BE4FC58}';
  EncoderColorDepth: TGUID = '{66087055-AD66-4C7C-9A18-38A2310B8337}';
  EncoderScanMethod: TGUID = '{3A4E2661-3109-4E56-8536-42C156E7DCFA}';
  EncoderVersion: TGUID = '{24D18C76-814A-41A4-BF53-1C219CCCF797}';
  EncoderRenderMethod: TGUID = '{6D42C53A-229A-4825-8BB7-5C99E2B9A8B8}';
  EncoderQuality: TGUID = '{1D5BE4B5-FA4A-452D-9CDD-5DB35105E7EB}';
  EncoderTransformation: TGUID = '{8D0EB2D1-A58E-4EA8-AA14-108074B7B6F9}';
  EncoderLuminanceTable: TGUID = '{EDB33BCE-0266-4A77-B904-27216099E717}';
  EncoderChrominanceTable: TGUID = '{F2E455DC-09B3-4316-8260-676ADA32481C}';
  EncoderSaveFlag: TGUID = '{292266FC-AC40-47BF-8CFC-A85B89A655DE}';

  CodecIImageBytes: TGUID = '{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}';

type
  IImageBytes = Interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    function CountBytes(out pcb: UINT): HRESULT; stdcall;
    function LockBytes(cb: UINT; ulOffset: ULONG; out ppvBytes: pointer): HRESULT; stdcall;
    function UnlockBytes(pvBytes: Pointer; cb: UINT; ulOffset: ULONG): HRESULT; stdcall;
  end;

  ImageCodecInfo = packed record
    Clsid: TGUID;             
    FormatID: TGUID;
    CodecName: PWChar;
    DllName: PWChar;           
    FormatDescription: PWChar; 
    FilenameExtension: PWChar; 
    MimeType: PWChar;
    Flags: DWORD;             
    Version: DWORD;           
    SigCount: DWORD;
    SigSize: DWORD;           
    SigPattern: PByte;
    SigMask: PByte;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;

{$IFDEF D6_UP}
  ImageCodecFlags = (
    ImageCodecFlagsEncoder            = $00000001,
    ImageCodecFlagsDecoder            = $00000002,
    ImageCodecFlagsSupportBitmap      = $00000004,
    ImageCodecFlagsSupportVector      = $00000008,
    ImageCodecFlagsSeekableEncode     = $00000010,
    ImageCodecFlagsBlockingDecode     = $00000020,
    ImageCodecFlagsBuiltin            = $00010000,
    ImageCodecFlagsSystem             = $00020000,
    ImageCodecFlagsUser               = $00040000
  );
  TImageCodecFlags = ImageCodecFlags;
{$ELSE}
  ImageCodecFlags = Integer;

const
  ImageCodecFlagsEncoder            = $00000001;
  ImageCodecFlagsDecoder            = $00000002;
  ImageCodecFlagsSupportBitmap      = $00000004;
  ImageCodecFlagsSupportVector      = $00000008;
  ImageCodecFlagsSeekableEncode     = $00000010;
  ImageCodecFlagsBlockingDecode     = $00000020;
  ImageCodecFlagsBuiltin            = $00010000;
  ImageCodecFlagsSystem             = $00020000;
  ImageCodecFlagsUser               = $00040000;

type
  TImageCodecFlags = ImageCodecFlags;
{$ENDIF}

const
  BmpFormat = 'image/bmp';
  GifFormat = 'image/gif';
  PngFormat = 'image/png';
  JpgFormat = 'image/jpeg';
  TifFormat = 'image/tiff';

function GetEncoderClsid(const Format: WideString; var Clsid: TGUID): Boolean;

type
  ImageLockMode = Integer;

const
  ImageLockModeRead         = $0001;
  ImageLockModeWrite        = $0002;
  ImageLockModeUserInputBuf = $0004;

type
  TImageLockMode = ImageLockMode;

type
  BitmapData = record
    Width: Integer;
    Heigh: Integer;
    Strid: Integer;
    Pixel: TPixelFormat;
    Scan0: Pointer;
    Reser: Integer;
  end;
  TBitmapData = BitmapData;
  PBitmapData = ^TBitmapData;

  TImageAbort = function: BOOL; stdcall;
  TDrawImageAbort = TImageAbort;
  TGetThumbnailImageAbort = TImageAbort;

  DebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );
  TDebugEventLevel = DebugEventLevel;

  DebugEventProc = procedure(level: TDebugEventLevel; message: PChar); stdcall;
  TDebugEventProc = DebugEventProc;
  NotificationHookProc = function(out token: ULONG): Status; stdcall;
  TNotificationHookProc = NotificationHookProc;
  NotificationUnhookProc = procedure(token: ULONG); stdcall;
  TNotificationUnhookProc = NotificationUnhookProc;

  GdiplusStartupInput = packed record
    GdiplusVersion: ULONG;
    DebugEventCallback: TDebugEventProc;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs: BOOL;
  end;
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  GdiplusStartupOutput = packed record
    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

{ Native objects }

type
  PGdiplusBase = Pointer;
  PGraphics = Pointer;
  PPen = Pointer;
  PBrush = Pointer;
  PMatrix = Pointer;
  PBitmap = Pointer;
  PMetafile = Pointer;
  PFontFamily = Pointer;
  PGraphicsPath = Pointer;
  PRegion = Pointer;
  PImage = Pointer;
  PHatchBrush = Pointer;
  PSolidBrush = Pointer;
  PLinearGradientBrush = Pointer;
  PPathGradientBrush = Pointer;
  PFont = Pointer;
  PFontCollection = Pointer;
  PInstalledFontCollection = Pointer;
  PPrivateFontCollection = Pointer;
  PImageAttributes = Pointer;
  PCachedBitmap = Pointer;

  TGdiHandle = Pointer;

{ GDI+ interfaces }

{$IFNDEF D6_UP}
  IInterface = IUnknown;
{$ENDIF}

  IGdiplusBase = interface;
  IGraphics = interface;
  IPen = interface;
  IBrush = interface;
  IMatrix = interface;
  IBitmap = interface;
  IMetafile = interface;
  ICachedBitmap = interface;
  IFontFamily = interface;
  IGraphicsPath = interface;
  IRegion = interface;
  IImage = interface;
  IHatchBrush = interface;
  ISolidBrush = interface;
  ILinearGradientBrush = interface;
  IPathGradientBrush = interface;
  IFont = interface;
  IFontCollection = interface;
  IInstalledFontCollection = interface;
  IPrivateFontCollection = interface;
  IImageAttributes = interface;

{ IDisposable }

  IDisposable = interface
    ['{D8D6DA2C-A863-4269-917F-7559DE72F3D7}']
    procedure Dispose;
  end;

{ IGdiplusBase }

  IGdiplusBase = interface
    ['{04C06AB7-3A3D-486B-9645-E52451C38734}']
    function GetHandle: TGdiHandle;
    procedure SetHandle(Value: TGdiHandle);
    function GetLastResult: TStatus;
    property Handle: TGdiHandle read GetHandle write SetHandle;
    property LastResult: TStatus read GetLastResult;
  end;

{ IGraphics }

  IGraphics = interface(IGdiplusBase)
    ['{E158B1C4-38A7-4E95-BE2A-AB2C8AEA5A02}']
    function GetInterpolationMode: TInterpolationMode;
    procedure SetInterpolationMode(Value: TInterpolationMode);
    function GetSmoothingMode: TSmoothingMode;
    procedure SetSmoothingMode(Value: TSmoothingMode);
    function GetHDC: HDC;
    procedure ReleaseHDC(DC: HDC);
    function SetRenderingOrigin(X, Y: Integer): TStatus;
    function GetRenderingOrigin(out X, Y: Integer): TStatus;
    function DrawCachedBitmap(CachedBitmap: ICachedBitmap; X, Y: Integer): TStatus;
    function DrawImage(Image: IImage; const Point: TPointF): TStatus; overload;
    function DrawImage(Image: IImage; X, Y: Single): TStatus; overload;
    function DrawImage(Image: IImage; const Rect: TRectF): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, Width, Height: Single): TStatus; overload;
    function DrawImage(Image: IImage; const point: TPointI): TStatus; overload;
    function DrawImage(Image: IImage; X, Y: Integer): TStatus; overload;
    function DrawImage(Image: IImage; const Rect: TRectI): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointI; Count: Integer): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single;
      SrcUnit: TUnit = UnitPixel): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth,
      SrcHeight: Integer; SrcUnit: TUnit = UnitPixel): TStatus; overload;
    function DrawImage(Image: IImage; const DestRect: TRectF; SrcX, SrcY,
      SrcWidth, SrcHeight: Single; SrcUnit: TUnit = UnitPixel;
      ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer;
      SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit = UnitPixel;
      ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; const DestRect: TRectI; SrcX, SrcY,
      SrcWidth, SrcHeight: Integer; SrcUnit: TUnit = UnitPixel;
      ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointI;
      Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TUnit = UnitPixel;
      ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    property InterpolationMode: TInterpolationMode read GetInterpolationMode write SetInterpolationMode;
    property SmoothingMode: TSmoothingMode read GetSmoothingMode write SetSmoothingMode;
  end;

{ IImage }

  IImage = interface(IGdiplusBase)
    ['{2E22481B-C9D8-4200-9DCD-FBA1032D37F3}']
    function GetImageType: TImageType;
    function GetBounds(out SrcRect: TRectF; out SrcUnit: TUnit): TStatus;
    function GetEncoderParameterListSize(const ClsidEncoder: TGUID): Cardinal;
    function GetEncoderParameterList(const ClsidEncoder: TGUID; Size: Cardinal;
      Buffer: PEncoderParameters): TStatus;
    function GetFlags: Cardinal;
    function GetFrameCount(const DimensionID: TGUID): Integer;
    function GetFrameDimensionsCount: Integer;
    function GetFrameDimensionsList(DimensionIDs: PGUID; Count: Integer): TStatus;
    function GetPixelFormat: TPixelFormat;
    function GetPaletteSize: Integer;
    function GetPalette(Palette: PColorPalette; Size: Integer): TStatus;
    function SetPalette(Palette: PColorPalette): TStatus;
    function GetThumbnailImage(ThumbWidth, ThumbHeight: Integer;
      Callback: TGetThumbnailImageAbort = nil; CallbackData: pointer = nil): IImage;
    function GetPhysicalDimension: TSizeF;
    function GetPropertyCount: Integer;
    function GetPropertyIdList(NumOfProperty: Integer; List: PPropID): TStatus;
    function GetPropertyItemSize(PropId: TPropID): Cardinal;
    function GetPropertyItem(PropId: TPropID; PropSize: Cardinal; Buffer: PPropertyItem): TStatus;
    function GetPropertySize(out TotalBufferSize, NumProperties : Integer): TStatus;
    function GetAllPropertyItems(TotalBufferSize, NumProperties: Integer;
      AllItems: PPropertyItem): TStatus;
    function SetPropertyItem(const Item: TPropertyItem): TStatus;
    function GetRawFormat: TGUID;
    function GetHorizontalResolution: Single;
    function GetVerticalResolution: Single;
    function GetSize: TSizeI;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function FromFile(const FileName: WideString; ICM: Boolean =  False): IImage;
    function FromStream(Stream: IStream; ICM: Boolean =  False): IImage;
    function Clone: IImage;
    function RemovePropertyItem(PropId: TPropID): TStatus;
    function RotateFlip(RotateFlipType: TRotateFlipType): TStatus;
    function Save(const FileName: WideString; const ClsidEncoder: TGUID;
      EncoderParams: PEncoderParameters = nil): TStatus; overload;
    function Save(Stream: IStream; const ClsidEncoder: TGUID;
      EncoderParams: PEncoderParameters  = nil): TStatus; overload;
    function SaveAdd(EncoderParams: PEncoderParameters): TStatus; overload;
    function SaveAdd(NewImage: IImage; EncoderParams: PEncoderParameters): TStatus; overload;
    function SelectActiveFrame(const DimensionID: TGUID; FrameIndex: Integer): TStatus;
    property PhysicalDimension: TSizeF read GetPhysicalDimension;
    property PropertyCount: Integer read GetPropertyCount;
    property PixelFormat: TPixelFormat read GetPixelFormat;
    property RawFormat: TGUID read GetRawFormat;
    property ImageType: TImageType read GetImageType;
    property Size: TSizeI read GetSize;
    property HorizontalResolution: Single read GetHorizontalResolution;
    property VerticalResolution: Single read GetVerticalResolution;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

{ IBitmap }

  IBitmap = interface(IImage)
    ['{878C66B0-EE43-45AA-80AB-3E76BAC96C76}']
    function GetPixel(X, Y: Integer): TColor;
    procedure SetPixel(X, Y: Integer; Value: TColor);
    function Clone(Rect: TRectI; Format: TPixelFormat): IBitmap; overload;
    function Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): IBitmap; overload;
    function Clone(Rect: TRectF; Format: TPixelFormat): IBitmap; overload;
    function Clone(X, Y, Width, Height: Single; Format: TPixelFormat): IBitmap; overload;
    function LockBits(const Rect: TRectI; Flags: UINT; Format: TPixelFormat;
      var LockedBitmapData: TBitmapData): TStatus;
    function UnlockBits(const LockedBitmapData: TBitmapData): TStatus;
    function SetResolution(DpiX, DpiY: Single): TStatus;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
  end;

  IMetafile = interface(IImage)
    ['{D4D419B9-C507-49F1-BEFE-96051EEC29D7}']
  end;

  ICachedBitmap = interface(IGdiplusBase)
    ['{FC97A8B4-8CA4-4D6A-A0EF-A7DE31EFEC66}']
  end;

  IPen = interface(IGdiplusBase)
    ['{9B368246-3B48-4FD1-A302-909019A54BAB}']
  end;

  IBrush = interface(IGdiplusBase)
    ['{383CBD35-05F5-4A75-852E-E9FA6018401C}']
  end;

  IMatrix = interface(IGdiplusBase)
    ['{50A4FB69-C6D7-456B-AEE8-B697E1467587}']
  end;

  IFontFamily = interface(IGdiplusBase)
    ['{5A07052B-97EB-41BD-9E6C-52201FDAFC96}']
  end;

  IGraphicsPath = interface(IGdiplusBase)
    ['{35E36885-44FB-4CF8-8CD1-ABCD90C0735E}']
  end;

  IRegion = interface(IGdiplusBase)
    ['{04F03B52-302D-4A4E-B660-C709FE7E1B0A}']
  end;

  IHatchBrush = interface(IGdiplusBase)
    ['{2BA66149-82C1-4439-8A13-CA8025B6D3B8}']
  end;

  ISolidBrush = interface(IGdiplusBase)
    ['{AFC70D88-FE18-4512-8CAA-1787A9FED815}']
  end;

  ILinearGradientBrush = interface(IGdiplusBase)
    ['{5DD6631A-F36A-40EF-AD1E-9E5850D5CF3E}']
  end;

  IPathGradientBrush = interface(IGdiplusBase)
    ['{F70F6F59-90C4-4E2D-A10C-BBE84D54A093}']
  end;

  IFont = interface(IGdiplusBase)
    ['{4FC93D5D-5873-4048-BC6C-597AA47C06B5}']
  end;

  IFontCollection = interface(IGdiplusBase)
    ['{2DDA98CD-B2B3-43DC-B7B9-C9A5D6E46C09}']
  end;

  IInstalledFontCollection = interface(IGdiplusBase)
    ['{74267DB8-9217-4A23-9BC9-CE948B4A96CC}']
  end;

  IPrivateFontCollection = interface(IGdiplusBase)
    ['{1D334CF3-911C-488D-8BEC-9012339D9ADA}']
  end;

{ IImageAttributes }

  IImageAttributes = interface(IGdiplusBase)
    ['{2B824EF6-641F-4401-85DA-447125B3FB27}']
    function Clone: IImageAttributes;
    function SetColorMatrix(const Matrix: TColorMatrix;
      Flags: TColorMatrixFlags = ColorMatrixFlagsDefault;
      Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetGamma(Gamma: Single; Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearGamma(Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  end;

{ Constructors }

{ ICM should be defaulted to false }

function NewGraphics(DC: HDC): IGraphics; overload;
function NewGraphics(DC: HDC; Device: THandle): IGraphics; overload;
function NewGraphics(Wnd: HWND; ICM: Boolean): IGraphics; overload;
function NewGraphics(Image: IImage): IGraphics; overload;

function NewImage(const FileName: string; ICM: Boolean = False): IImage; overload;
function NewImage(Stream: IStream; ICM: Boolean = False): IImage; overload;

function NewBitmap(const FileName: string; ICM: Boolean = False): IBitmap; overload;
function NewBitmap(Stream: IStream; ICM: Boolean = False): IBitmap; overload;
function NewBitmap(Width, Height: Integer; Format: TPixelFormat = PixelFormat32bppARGB): IBitmap; overload;
function NewBitmap(Width, Height: Integer; Target: IGraphics): IBitmap; overload;
function NewBitmap(Width, Height, Stride: Integer; Format: TPixelFormat; Scan0: PByte): IBitmap; overload;
function NewBitmap(var BitmapInfo: TBitmapInfo; BitmapData: Pointer): IBitmap; overload;
function NewBitmap(Bitmap: HBITMAP; Palette: HPALETTE): IBitmap; overload;
function NewBitmap(Icon: HICON): IBitmap; overload;
function NewBitmap(Module: HMODULE; const ResName: string): IBitmap; overload;

function NewCachedBitmap(Bitmap: IBitmap; Graphics: IGraphics): ICachedBitmap; overload;

function NewImageAttributes: IImageAttributes; overload;
function NewImageAttributes(Transform: TColorTransform): IImageAttributes; overload;

{ GDI+ API }

function GdipAlloc(size: ULONG): Pointer; stdcall;
procedure GdipFree(ptr: Pointer); stdcall;
function GdiplusStartup(out token: ULONG; input: PGdiplusStartupInput;
  output: PGdiplusStartupOutput): Status; stdcall;
procedure GdiplusShutdown(token: ULONG); stdcall;

{ PGraphics }

function GdipDeleteGraphics(graphics: PGraphics): TStatus; stdcall;
function GdipCreateFromHDC(hdc: HDC;
  out graphics: PGraphics): TStatus; stdcall;
function GdipCreateFromHDC2(hdc: HDC; hDevice: THandle;
  out graphics: PGraphics): TStatus; stdcall;
function GdipCreateFromHWND(hwnd: HWND;
  out graphics: PGraphics): TStatus; stdcall;
function GdipCreateFromHWNDICM(hwnd: HWND;
  out graphics: PGraphics): TStatus; stdcall;
function GdipSetInterpolationMode(graphics: PGraphics;
  interpolationMode: TInterpolationMode): TStatus; stdcall;
function GdipGetInterpolationMode(graphics: PGraphics;
  var interpolationMode: TInterpolationMode): TStatus; stdcall;
function GdipSetSmoothingMode(graphics: PGraphics;
  smoothingMode: TSmoothingMode): TStatus; stdcall;
function GdipGetSmoothingMode(graphics: PGraphics;
  var smoothingMode: TSmoothingMode): TStatus; stdcall;
function GdipGetDC(graphics: PGraphics; var hdc: HDC): TStatus; stdcall;
function GdipReleaseDC(graphics: PGraphics; hdc: HDC): TStatus; stdcall;
function GdipSetRenderingOrigin(graphics: PGraphics;
  x, y: Integer): TStatus; stdcall;
function GdipGetRenderingOrigin(graphics: PGraphics;
  var x, y: Integer): TStatus; stdcall;
function GdipDrawImage(graphics: PGraphics; image: PImage; x: Single;
  y: Single): TStatus; stdcall;
function GdipDrawImageI(graphics: PGraphics; image: PImage; x: Integer;
  y: Integer): TStatus; stdcall;
function GdipDrawImageRect(graphics: PGraphics; image: PImage; x: Single;
  y: Single; width: Single; height: Single): TStatus; stdcall;
function GdipDrawImageRectI(graphics: PGraphics; image: PImage; x: Integer;
  y: Integer; width: Integer; height: Integer): TStatus; stdcall;
function GdipDrawImagePoints(graphics: PGraphics; image: PImage;
  dstpoints: PPointF; count: Integer): TStatus; stdcall;
function GdipDrawImagePointsI(graphics: PGraphics; image: PImage;
  dstpoints: PPointI; count: Integer): TStatus; stdcall;
function GdipDrawImagePointRect(graphics: PGraphics; image: PImage;
  x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
  srcheight: Single; srcUnit: TUnit): TStatus; stdcall;
function GdipDrawImagePointRectI(graphics: PGraphics; image: PImage;
  x: Integer; y: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer;
  srcheight: Integer; srcUnit: TUnit): TStatus; stdcall;
function GdipDrawImageRectRect(graphics: PGraphics; image: PImage;
  dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single;
  srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single;
  srcUnit: TUnit; ImageAttributes: PImageAttributes;
  callback: TDrawImageAbort; callbackData: Pointer): TStatus; stdcall;
function GdipDrawImageRectRectI(graphics: PGraphics; image: PImage;
  dstx: Integer; dsty: Integer; dstwidth: Integer; dstheight: Integer;
  srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer;
  srcUnit: TUnit; imageAttributes: PImageAttributes;
  callback: TDrawImageAbort; callbackData: Pointer): TStatus; stdcall;
function GdipDrawImagePointsRect(graphics: PGraphics; image: PImage;
  points: PPointF; count: Integer; srcx: Single; srcy: Single;
  srcwidth: Single; srcheight: Single; srcUnit: TUnit;
  imageAttributes: PImageAttributes; callback: TDrawImageAbort;
  callbackData: Pointer): TStatus; stdcall;
function GdipDrawImagePointsRectI(graphics: PGraphics; image: PImage;
  points: PPointI; count: Integer; srcx: Integer; srcy: Integer;
  srcwidth: Integer; srcheight: Integer; srcUnit: TUnit;
  imageAttributes: PImageAttributes; callback: TDrawImageAbort;
  callbackData: Pointer): TStatus; stdcall;

{ PImage }

function GdipDisposeImage(image: PImage): TStatus; stdcall;
function GdipLoadImageFromStream(stream: IStream;
  out image: PImage): TStatus; stdcall;
function GdipLoadImageFromFile(filename: PWChar;
  out image: PImage): TStatus; stdcall;
function GdipLoadImageFromStreamICM(stream: IStream;
  out image: PImage): TStatus; stdcall;
function GdipLoadImageFromFileICM(filename: PWChar;
  out image: PImage): TStatus; stdcall;
function GdipCloneImage(image: PImage;
  out cloneImage: PImage): TStatus; stdcall;
function GdipSaveImageToFile(image: PImage; filename: PWChar;
  const clsidEncoder: TGUID;
  encoderParams: PEncoderParameters): TStatus; stdcall;
function GdipSaveImageToStream(image: PImage; stream: IStream;
  const clsidEncoder: TGUID;
  encoderParams: PEncoderParameters): TStatus; stdcall;
function GdipSaveAdd(image: PImage;
  encoderParams: PEncoderParameters): TStatus; stdcall;
function GdipSaveAddImage(image: PImage; newImage: PImage;
  encoderParams: PEncoderParameters): TStatus; stdcall;
function GdipGetImageGraphicsContext(image: PImage;
  out graphics: PGraphics): TStatus; stdcall;
function GdipGetImageBounds(image: PImage; var srcRect: TRectF;
  var srcUnit: TUnit): TStatus; stdcall;
function GdipGetImageDimension(image: PImage;
  var width, height: Single): TStatus; stdcall;
function GdipGetImageType(image: PImage;
  var imageType: TImageType): TStatus; stdcall;
function GdipGetImageWidth(image: PImage;
  var width: Integer): TStatus; stdcall;
function GdipGetImageHeight(image: PImage;
  var height: Integer): TStatus; stdcall;
function GdipGetImageHorizontalResolution(image: PImage;
  var resolution: Single): TStatus; stdcall;
function GdipGetImageVerticalResolution(image: PImage;
  var resolution: Single): TStatus; stdcall;
function GdipGetImageFlags(image: PImage;
  var flags: UINT): TStatus; stdcall;
function GdipGetImageRawFormat(image: PImage;
  var format: TGUID): TStatus; stdcall;
function GdipGetImagePixelFormat(image: PImage;
  out format: TPixelFormat): TStatus; stdcall;
function GdipGetImageThumbnail(image: PImage; thumbWidth: UINT;
  thumbHeight: UINT; out thumbImage: PImage;
  callback: TGetThumbnailImageAbort; callbackData: Pointer): TStatus; stdcall;
function GdipGetEncoderParameterListSize(image: PImage;
  const clsidEncoder: TGUID; out size: UINT): TStatus; stdcall;
function GdipGetEncoderParameterList(image: PImage; const clsidEncoder: TGUID;
  size: UINT; buffer: PEncoderParameters): TStatus; stdcall;
function GdipImageGetFrameDimensionsCount(image: PImage;
  var count: Integer): TStatus; stdcall;
function GdipImageGetFrameDimensionsList(image: PImage; dimensionIDs: PGUID;
  count: Integer): TStatus; stdcall;
function GdipImageGetFrameCount(image: PImage; const dimensionID: TGUID;
  var count: Integer): TStatus; stdcall;
function GdipImageSelectActiveFrame(image: PImage; const dimensionID: TGUID;
  frameIndex: Integer): TStatus; stdcall;
function GdipImageRotateFlip(image: PImage;
  rfType: TRotateFlipType): TStatus; stdcall;
function GdipGetImagePalette(image: PImage; palette: PColorPalette;
  size: Integer): TStatus; stdcall;
function GdipSetImagePalette(image: PImage;
  palette: PColorPalette): TStatus; stdcall;
function GdipGetImagePaletteSize(image: PImage;
  var size: Integer): TStatus; stdcall;
function GdipGetPropertyCount(image: PImage;
  var numOfProperty: Integer): TStatus; stdcall;
function GdipGetPropertyIdList(image: PImage; numOfProperty: UINT;
  list: PPROPID): TStatus; stdcall;
function GdipGetPropertyItemSize(image: PImage; propId: PROPID;
  var size: UINT): TStatus; stdcall;
function GdipGetPropertyItem(image: PImage; propId: PROPID; propSize: UINT;
  buffer: PPropertyItem): TStatus; stdcall;
function GdipGetPropertySize(image: PImage; var totalBufferSize,
  numProperties: Integer): TStatus; stdcall;
function GdipGetAllPropertyItems(image: PImage; totalBufferSize: UINT;
  numProperties: Integer; allItems: PPropertyItem): TStatus; stdcall;
function GdipRemovePropertyItem(image: PImage;
  propId: PROPID): TStatus; stdcall;
function GdipSetPropertyItem(image: PImage;
  const item: TPropertyItem): TStatus; stdcall;
function GdipImageForceValidation(image: PImage): TStatus; stdcall;

{ PBitmap}

function GdipCreateBitmapFromStream(Stream: IStream;
  out Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromFile(FileName: PWideChar;
  out Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromStreamICM(Stream: IStream;
  out Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromFileICM(FileName: PWideChar;
  var Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromScan0(Width: Integer; Height: Integer;
  Stride: Integer; Format: TPixelFormat; Scan0: PByte;
  out Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromGraphics(Width: Integer; Height: Integer;
  Target: PGraphics; out Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromGdiDib(var BitmapInfo: TBitmapInfo;
  BitmapData: Pointer; out Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromHBITMAP(BM: HBITMAP; Pal: HPALETTE;
  out Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromHICON(Icon: HICON;
  out Bitmap: PBitmap): TStatus; stdcall;
function GdipCreateBitmapFromResource(hInstance: HMODULE;
  lpBitmapName: PWideChar; out bitmap: PBitmap): TStatus; stdcall;
function GdipCloneBitmapArea(x: Single; y: Single; width: Single;
  height: Single; format: TPixelFormat; srcBitmap: PBitmap;
  out dstBitmap: PBitmap): TStatus; stdcall;
function GdipCloneBitmapAreaI(x, y: Integer; width: Integer;
  height: Integer; format: TPixelFormat; srcBitmap: PBitmap;
  out dstBitmap: PBitmap): TStatus; stdcall;
function GdipBitmapLockBits(bitmap: PBitmap; const rect: TRectI; flags: UINT;
  format: TPixelFormat; var lockedBitmapData: TBitmapData): TStatus; stdcall;
function GdipBitmapUnlockBits(bitmap: PBitmap;
  const lockedBitmapData: TBitmapData): TStatus; stdcall;
function GdipBitmapGetPixel(bitmap: PBitmap; x, y: Integer;
  var color: TColor): TStatus; stdcall;
function GdipBitmapSetPixel(bitmap: PBitmap; x, y: Integer;
  color: TColor): TStatus; stdcall;
function GdipBitmapSetResolution(bitmap: PBitmap; xdpi: Single;
  ydpi: Single): TStatus; stdcall;

{ PCachedBitmap }

function GdipDeleteCachedBitmap(cachedBitmap: PCachedBitmap): TStatus; stdcall;
function GdipCreateCachedBitmap(bitmap: PBitmap; graphics: PGraphics;
  out cachedBitmap: PCachedBitmap): TStatus; stdcall;
function GdipDrawCachedBitmap(graphics: PGraphics;
  cachedBitmap: PCachedBitmap; x, y: Integer): TStatus; stdcall;

{ PImageAttributes }

function GdipDisposeImageAttributes(imageattr: PImageAttributes): TStatus; stdcall;
function GdipCreateImageAttributes(out imageattr: PImageAttributes): TStatus; stdcall;
function GdipCloneImageAttributes(imageattr: PImageAttributes;
  out cloneImageattr: PImageAttributes): TStatus; stdcall;
function GdipSetImageAttributesColorMatrix(imageattr: PImageAttributes;
  adjust: TColorAdjustType; enableFlag: BOOL; colorMatrix,
  grayMatrix: PColorMatrix; flags: TColorMatrixFlags): TStatus; stdcall;
function GdipSetImageAttributesGamma(imageattr: PImageAttributes;
  adjust: TColorAdjustType; enableFlag: BOOL; gamma: Single): TStatus; stdcall;

function GdipGetImageEncodersSize(out numEncoders: Integer;
  out size: Integer): TStatus; stdcall;
function GdipGetImageEncoders(numEncoders: Integer; size: Integer;
  encoders: PImageCodecInfo): TStatus; stdcall;

implementation

const
  gdipluslib = 'gdiplus.dll';

function GdipAlloc; external gdipluslib;
procedure GdipFree; external gdipluslib;
function GdiplusStartup; external gdipluslib;
procedure GdiplusShutdown; external gdipluslib;
function GdipDeleteGraphics; external gdipluslib;
function GdipCreateFromHDC; external gdipluslib;
function GdipCreateFromHDC2; external gdipluslib;
function GdipCreateFromHWND; external gdipluslib;
function GdipCreateFromHWNDICM; external gdipluslib;
function GdipSetInterpolationMode; external gdipluslib;
function GdipGetInterpolationMode; external gdipluslib;
function GdipSetSmoothingMode; external gdipluslib;
function GdipGetSmoothingMode; external gdipluslib;
function GdipGetDC; external gdipluslib;
function GdipReleaseDC; external gdipluslib;
function GdipSetRenderingOrigin; external gdipluslib;
function GdipGetRenderingOrigin; external gdipluslib;
function GdipDrawImage; external gdipluslib;
function GdipDrawImageI; external gdipluslib;
function GdipDrawImageRect; external gdipluslib;
function GdipDrawImageRectI; external gdipluslib;
function GdipDrawImagePoints; external gdipluslib;
function GdipDrawImagePointsI; external gdipluslib;
function GdipDrawImagePointRect; external gdipluslib;
function GdipDrawImagePointRectI; external gdipluslib;
function GdipDrawImageRectRect; external gdipluslib;
function GdipDrawImageRectRectI; external gdipluslib;
function GdipDrawImagePointsRect; external gdipluslib;
function GdipDrawImagePointsRectI; external gdipluslib;
function GdipDisposeImage; external gdipluslib;
function GdipLoadImageFromStream; external gdipluslib;
function GdipLoadImageFromFile; external gdipluslib;
function GdipLoadImageFromStreamICM; external gdipluslib;
function GdipLoadImageFromFileICM; external gdipluslib;
function GdipCloneImage; external gdipluslib;
function GdipSaveImageToFile; external gdipluslib;
function GdipSaveImageToStream; external gdipluslib;
function GdipSaveAdd; external gdipluslib;
function GdipSaveAddImage; external gdipluslib;
function GdipGetImageGraphicsContext; external gdipluslib;
function GdipGetImageBounds; external gdipluslib;
function GdipGetImageDimension; external gdipluslib;
function GdipGetImageType; external gdipluslib;
function GdipGetImageWidth; external gdipluslib;
function GdipGetImageHeight; external gdipluslib;
function GdipGetImageHorizontalResolution; external gdipluslib;
function GdipGetImageVerticalResolution; external gdipluslib;
function GdipGetImageFlags; external gdipluslib;
function GdipGetImageRawFormat; external gdipluslib;
function GdipGetImagePixelFormat; external gdipluslib;
function GdipGetImageThumbnail; external gdipluslib;
function GdipGetEncoderParameterListSize; external gdipluslib;
function GdipGetEncoderParameterList; external gdipluslib;
function GdipImageGetFrameDimensionsCount; external gdipluslib;
function GdipImageGetFrameDimensionsList; external gdipluslib;
function GdipImageGetFrameCount; external gdipluslib;
function GdipImageSelectActiveFrame; external gdipluslib;
function GdipImageRotateFlip; external gdipluslib;
function GdipGetImagePalette; external gdipluslib;
function GdipSetImagePalette; external gdipluslib;
function GdipGetImagePaletteSize; external gdipluslib;
function GdipGetPropertyCount; external gdipluslib;
function GdipGetPropertyIdList; external gdipluslib;
function GdipGetPropertyItemSize; external gdipluslib;
function GdipGetPropertyItem; external gdipluslib;
function GdipGetPropertySize; external gdipluslib;
function GdipGetAllPropertyItems; external gdipluslib;
function GdipRemovePropertyItem; external gdipluslib;
function GdipSetPropertyItem; external gdipluslib;
function GdipImageForceValidation; external gdipluslib;
function GdipCreateBitmapFromStream; external gdipluslib;
function GdipCreateBitmapFromFile; external gdipluslib;
function GdipCreateBitmapFromStreamICM; external gdipluslib;
function GdipCreateBitmapFromFileICM; external gdipluslib;
function GdipCreateBitmapFromScan0; external gdipluslib;
function GdipCreateBitmapFromGraphics; external gdipluslib;
function GdipCreateBitmapFromGdiDib; external gdipluslib;
function GdipCreateBitmapFromHBITMAP; external gdipluslib;
function GdipCreateBitmapFromHICON; external gdipluslib;
function GdipCreateBitmapFromResource; external gdipluslib;
function GdipCloneBitmapArea; external gdipluslib;
function GdipCloneBitmapAreaI; external gdipluslib;
function GdipBitmapLockBits; external gdipluslib;
function GdipBitmapUnlockBits; external gdipluslib;
function GdipBitmapGetPixel; external gdipluslib;
function GdipBitmapSetPixel; external gdipluslib;
function GdipBitmapSetResolution; external gdipluslib;
function GdipDeleteCachedBitmap; external gdipluslib;
function GdipCreateCachedBitmap; external gdipluslib;
function GdipDrawCachedBitmap; external gdipluslib;
function GdipDisposeImageAttributes; external gdipluslib;
function GdipCreateImageAttributes; external gdipluslib;
function GdipCloneImageAttributes; external gdipluslib;
function GdipSetImageAttributesColorMatrix; external gdipluslib;
function GdipSetImageAttributesGamma; external gdipluslib;
function GdipGetImageEncodersSize; external gdipluslib;
function GdipGetImageEncoders; external gdipluslib;

function NewColor(R, G, B: Byte): TColor; overload;
begin
  Result.C := $FF000000 or (R shl RED_SHIFT) or (G shl GREEN_SHIFT) or B;
end;

function NewColor(R, G, B, A: Byte): TColor; overload;
begin
  Result.C := (A shl ALPHA_SHIFT) or (R shl RED_SHIFT) or (G shl GREEN_SHIFT) or B;
end;

function NewColor(ColorRef: TColorRef): TColor; overload;
var
  A: Byte;
begin
  if Integer(ColorRef) < 0 then
    ColorRef := GetSysColor(ColorRef and $000000FF);
  Result.C := ColorRef or $FF000000;
  A := Result.B;
  Result.B := Result.R;
  Result.R := A;
end;

function NewColorMatrix: TColorMatrix;
begin
  FillChar(Result, SizeOf(Result), #0);
  Result[0, 0] := 1;
  Result[1, 1] := 1;
  Result[2, 2] := 1;
  Result[3, 3] := 1;
  Result[4, 4] := 1;
end;

function NewColorMatrix(Opacity: Single): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[3, 3] := Opacity;
end;

function NewColorMatrix(R, G, B, A: Byte): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[0, 0] := R / $FF;
  Result[1, 1] := G / $FF;
  Result[2, 2] := B / $FF;
  Result[3, 3] := A / $FF;
end;

function NewColorMatrix(R, G, B, A: Single): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[0, 0] := R;
  Result[1, 1] := G;
  Result[2, 2] := B;
  Result[3, 3] := A;
end;

function NewColorMatrix(Color: TColor): TColorMatrix;
begin
  with Color do
    Result := NewColorMatrix(R, G, B, A);
end;

function NewColorMatrix(ColorRef: TColorRef): TColorMatrix;
begin
  with NewColor(ColorRef) do
    Result := NewColorMatrix(R, G, B, A);
end;

procedure ColorFill(C: PSingle; Data: array of Single);
var
  I: Integer;
begin
  for I := Low(Data) to High(Data) do
  begin
    C^ := Data[I];
    Inc(C);
  end;
end;

function ColorMultiply(const A, B: TColorMatrix): TColorMatrix;
var
  Col, Row: TColorRow;
  X, Y, Z: Integer;
  S: single;
begin
  for X := Low(Result[0]) to High(Result[0]) do
  begin
    for Y := Low(Col) to High(Col) do
      Col[Y] := A[Y, X];
    for Z := Low(Col) to High(Col) do
    begin
      Row := TColorRow(B[Z]);
      S := 0;
      for Y := Low(Row) to High(Row) do
        S := S + Row[Y] * Col[Y];
      Result[Z, X] := S;
    end;
  end;
end;

function ColorBrightness(const M: TColorMatrix; B: Single): TColorMatrix;
begin
  Result := NewColorMatrix;
  ColorFill(@Result[4, 0], [B, B, B, 0, 1]);
  Result := ColorMultiply(Result, M);
end;

function ColorContrast(const M: TColorMatrix; C: Single): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[0, 0] := C;
  Result[1, 1] := C;
  Result[2, 2] := C;
  ColorFill(@Result[4, 0], [0.001, 0.001, 0.001, 0, 0]);
  Result := ColorMultiply(Result, M);
end;

function ColorSaturate(const M: TColorMatrix; S: Single): TColorMatrix;
var
  C, R, G, B: Single;
begin
  C := 1 - S;
  R := 0.3086 * C;
  G := 0.6094 * C;
  B := 0.0820 * C;
  Result := NewColorMatrix;
  ColorFill(@Result[0, 0], [R + S, R, R]);
  ColorFill(@Result[1, 0], [G, G + S, G]);
  ColorFill(@Result[2, 0], [B, B, B + S]);
  Result := ColorMultiply(Result, M);
end;

function ColorOpacity(const M: TColorMatrix; O: Single): TColorMatrix;
begin
  Result := ColorMultiply(NewColorMatrix(O), M);
end;

function ColorGreyscale(const M: TColorMatrix): TColorMatrix;
begin
  Result := NewColorMatrix;
  ColorFill(@Result[0, 0], [0.30, 0.30, 0.30]);
  ColorFill(@Result[1, 0], [0.59, 0.59, 0.59]);
  ColorFill(@Result[2, 0], [0.11, 0.11, 0.11]);
  Result := ColorMultiply(Result, M);
end;


function ColorNegative(const M: TColorMatrix): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[0, 0] := -1;
  Result[1, 1] := -1;
  Result[2, 2] := -1;
  Result := ColorMultiply(Result, M);
end;

function NewColorTransform: TColorTransform;
begin
  with Result do
  begin
    Gamma := 1;
    Brightness := 0;
    Contrast := 1;
    Saturation := 1;
    Opacity := 1;
    Greyscale := False;
    Negative := False;
  end;
end;

function ColorTransform(const Transform: TColorTransform): TColorMatrix;
begin
  Result := NewColorMatrix;
  if Transform.Brightness <> 0 then
    Result := ColorBrightness(Result, Transform.Brightness);
  if Transform.Contrast <> 1 then
    Result := ColorContrast(Result, Transform.Contrast);
  if Transform.Opacity <> 1 then
    Result := ColorOpacity(Result, Transform.Opacity);
  if Transform.Saturation <> 1 then
    Result := ColorSaturate(Result, Transform.Saturation);
  if Transform.Greyscale then
    Result := ColorGreyscale(Result);
  if Transform.Negative then
    Result := ColorNegative(Result);
end;

function NewSize(Width, Height: Integer): TSizeI;
begin
  Result.Width := Width; Result.Height := Height;
end;

function NewSize(Width, Height: Single): TSizeF;
begin
  Result.Width := Width; Result.Height := Height;
end;

function NewSize(const Point: TPointI): TSizeI;
begin
  Result.Width := Point.X; Result.Height := Point.Y;
end;

function NewSize(const Point: TPointF): TSizeF;
begin
  Result.Width := Point.X; Result.Height := Point.Y;
end;

function NewPoint(X, Y: Integer): TPointI;
begin
  Result.X := X; Result.Y := Y;
end;

function NewPoint(X, Y: Single): TPointF;
begin
  Result.X := X; Result.Y := Y;
end;

function NewPoint(const Size: TSizeI): TPointI;
begin
  Result.X := Size.Width; Result.Y := Size.Height;
end;

function NewPoint(const Size: TSizeF): TPointF;
begin
  Result.X := Size.Width; Result.Y := Size.Height;
end;

function NewPoint(const Point: TPoint): TPointI; overload;
begin
  Result.X := Point.X; Result.Y := Point.Y;
end;

function NewRect(Width, Height: Integer): TRectI;
begin
  Result.X := 0; Result.Y := 0; Result.Width := Width; Result.Height := Height;
end;

function NewRect(Width, Height: Integer; Scale: Single): TRectI;
begin
  Result.X := 0; Result.Y := 0;
  Result.Width := Round(Width * Scale); Result.Height := Round(Height * Scale);
end;

function NewRect(X, Y, Width, Height: Integer): TRectI;
begin
  Result.X := X; Result.Y := Y; Result.Width := Width; Result.Height := Height;
end;

function NewRect(const Point: TPointI; Width, Height: Integer): TRectI;
begin
  Result.X := Point.X; Result.Y := Point.Y; Result.Width := Width; Result.Height := Height;
end;

function NewRect(const Point: TPointI; const Size: TSizeI): TRectI;
begin
  Result.X := Point.X; Result.Y := Point.Y; Result.Width := Size.Width; Result.Height := Size.Height;
end;

function NewRect(X, Y: Integer; const Size: TSizeI): TRectI;
begin
  Result.X := X; Result.Y := Y; Result.Width := Size.Width; Result.Height := Size.Height;
end;

function NewRect(Width, Height: Single): TRectF;
begin
  Result.X := 0; Result.Y := 0; Result.Width := Width; Result.Height := Height;
end;

function NewRect(X, Y, Width, Height: Single): TRectF;
begin
  Result.X := X; Result.Y := Y; Result.Width := Width; Result.Height := Height;
end;

function NewRect(const Point: TPointF; Width, Height: Single): TRectF;
begin
  Result.X := Point.X; Result.Y := Point.Y; Result.Width := Width; Result.Height := Height;
end;

function NewRect(const Point: TPointF; const Size: TSizeF): TRectF;
begin
  Result.X := Point.X; Result.Y := Point.Y; Result.Width := Size.Width; Result.Height := Size.Height;
end;

function NewRect(X, Y: Integer; const Size: TSizeF): TRectF;
begin
  Result.X := X; Result.Y := Y; Result.Width := Size.Width; Result.Height := Size.Height;
end;

function NewRect(const Rect: TRect): TRectI;
begin
  Result.X := Rect.Left; Result.Y := Rect.Top;
  Result.Width := Rect.Right - Rect.Left; Result.Height := Rect.Bottom - Rect.Top;
end;

function OffsetRect(const Rect: TRectI; X, Y: Integer): TRectI;
begin
  Result.X := Rect.X + X; Result.Y := Rect.Y + Y;
  Result.Width := Rect.Width; Result.Height := Rect.Height;
end;

function OffsetRect(const Rect: TRectF; X, Y: Single): TRectF;
begin
  Result.X := Rect.X + X; Result.Y := Rect.Y + Y;
  Result.Width := Rect.Width; Result.Height := Rect.Height;
end;

function InflateRect(const Rect: TRectI; X, Y: Integer): TRectI;
begin
  Result.X := Rect.X + X; Result.Y := Rect.Y + Y;
  Result.Width := Rect.Width + X * 2; Result.Height := Rect.Height + Y * 2;
end;

function InflateRect(const Rect: TRectF; X, Y: Single): TRectF;
begin
  Result.X := Rect.X + X; Result.Y := Rect.Y + Y;
  Result.Width := Rect.Width + X * 2; Result.Height := Rect.Height + Y * 2;
end;

function GetEncoderClsid(const Format: WideString; var Clsid: TGUID): Boolean;
var
  Num, Size: Integer;
  Codecs, Item: PImageCodecInfo;
  I: Integer;
begin
  Result := False;
  GdipGetImageEncodersSize(Num, Size);
  GetMem(Codecs, Size);
  GdipGetImageEncoders(Num, Size, Codecs);
  Item := Codecs;
  for I := 0 to Num - 1 do
  begin
    if Format = Item.MimeType then
    begin
      Clsid := Item.Clsid;
      Result := True;
      Break;
    end;
    Inc(Item);
  end;
  FreeMem(Codecs);
end;

{ GDI+ handle management }

type
  PHandleLink = ^THandleLink;
  THandleLink = record
    Handle: TGdiHandle;
    RefCount: Integer;
    Next: PHandleLink;
  end;

var
  HandleLink: PHandleLink;
  HandleName: array[0..$20] of Char;

procedure AddRefHandle(Handle: TGdiHandle);
var
  Mutex: THandle;
  A, B: PHandleLink;
begin
  if Handle = nil then Exit;
  Mutex := CreateMutex(nil, False, HandleName);
  WaitForSingleObject(Mutex, INFINITE);
  try
    A := HandleLink;
    B := A;
    while B <> nil do
    begin
      A := B;
      if A.Handle = Handle then
      begin
        Inc(A.RefCount);
        Exit;
      end;
      B := A.Next;
    end;
    New(B);
    B.Handle := Handle;
    B.RefCount := 1;
    B.Next := nil;
    if A = nil then
      HandleLink := B
    else
      A.Next := B;
  finally
    ReleaseMutex(Mutex);
    CloseHandle(Mutex);
  end;
end;

function ReleaseHandle(Handle: TGdiHandle): Boolean;
var
  Mutex: THandle;
  A, B: PHandleLink;
begin
  Result := False;
  if Handle = nil then Exit;
  Mutex := CreateMutex(nil, False, HandleName);
  WaitForSingleObject(Mutex, INFINITE);
  try
    A := HandleLink;
    B := A;
    while A <> nil do
    begin
      Result := A.Handle = Handle;
      if Result then
      begin
        Dec(A.RefCount);
        if A.RefCount = 0 then
        begin
          if A = HandleLink then
            HandleLink := A.Next
          else
            B.Next := A.Next;
          Dispose(A);
        end;
        Result := True;
        Break;
      end;
      B := A;
      A := B.Next;
    end;
  finally
    ReleaseMutex(Mutex);
    CloseHandle(Mutex);
  end;
end;

{ GDI+ interfaces }

type
  TGdiplusBase = class(TInterfacedObject, IGdiplusBase, IDisposable)
  protected
    FHandle: TGdiHandle;
    FLastResult: TStatus;
    procedure DestroyHandle(Handle: TGdiHandle); virtual;
    function GetHandle: TGdiHandle;
    procedure SetHandle(Value: TGdiHandle);
    function GetLastResult: TStatus;
    procedure Dispose;
  public
    constructor Create(Handle: Pointer);
    destructor Destroy; override;
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

constructor TGdiplusBase.Create(Handle: Pointer);
begin
  inherited Create;
  SetHandle(Handle);
end;

destructor TGdiplusBase.Destroy;
begin
  SetHandle(nil);
  inherited Destroy;
end;

procedure TGdiplusBase.DestroyHandle(Handle: TGdiHandle);
begin
end;

type
  TInterfacedHack = class
    RefCount: Integer;
  end;

class function TGdiplusBase.NewInstance: TObject;
begin
  Result := InitInstance(GdipAlloc(ULONG(InstanceSize)));
  TInterfacedHack(Result).RefCount := 1;
end;

procedure TGdiplusBase.FreeInstance;
begin
  CleanupInstance;
  GdipFree(Self);
end;

function TGdiplusBase.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TGdiplusBase.SetHandle(Value: Pointer);
begin
  if Value <> FHandle then
  try
    if ReleaseHandle(FHandle) then
      DestroyHandle(FHandle);
    AddRefHandle(Value);
  finally
    FHandle := Value;
  end;
end;

function TGdiplusBase.GetLastResult: TStatus;
begin
  Result := FLastResult;
end;

procedure TGdiplusBase.Dispose;
begin
  SetHandle(nil);
end;

type
  TGraphics = class(TGdiplusBase, IGraphics)
  protected
    procedure DestroyHandle(Handle: TGdiHandle); override;
  public
    constructor Create(DC: HDC); overload;
    constructor Create(DC: HDC; Device: THandle); overload;
    constructor Create(Wnd: HWND; ICM: Boolean); overload;
    constructor Create(Image: IImage); overload;
    function GetInterpolationMode: TInterpolationMode;
    procedure SetInterpolationMode(Value: TInterpolationMode);
    function GetSmoothingMode: TSmoothingMode;
    procedure SetSmoothingMode(Value: TSmoothingMode);
    function GetHDC: HDC;
    procedure ReleaseHDC(DC: HDC);
    function SetRenderingOrigin(X, Y: Integer): TStatus;
    function GetRenderingOrigin(out X, Y: Integer): TStatus;
    function DrawCachedBitmap(CachedBitmap: ICachedBitmap; X, Y: Integer): TStatus;
    function DrawImage(Image: IImage; const Point: TPointF): TStatus; overload;
    function DrawImage(Image: IImage; X, Y: Single): TStatus; overload;
    function DrawImage(Image: IImage; const Rect: TRectF): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, Width, Height: Single): TStatus; overload;
    function DrawImage(Image: IImage; const point: TPointI): TStatus; overload;
    function DrawImage(Image: IImage; X, Y: Integer): TStatus; overload;
    function DrawImage(Image: IImage; const Rect: TRectI): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointI; Count: Integer): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single;
      SrcUnit: TUnit = UnitPixel): TStatus; overload;
    function DrawImage(Image: IImage; const DestRect: TRectF; SrcX, SrcY,
      SrcWidth, SrcHeight: Single; SrcUnit: TUnit = UnitPixel;
      ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer;
      SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit = UnitPixel;
      ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth,
      SrcHeight: Integer; SrcUnit: TUnit = UnitPixel): TStatus; overload;
    function DrawImage(Image: IImage; const DestRect: TRectI; SrcX, SrcY,
      SrcWidth, SrcHeight: Integer; SrcUnit: TUnit = UnitPixel;
      ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointI;
      Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TUnit = UnitPixel;
      ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
  end;

function NewGraphics(DC: HDC): IGraphics;
begin
  Result := TGraphics.Create(DC);
end;

function NewGraphics(DC: HDC; Device: THandle): IGraphics;
begin
  Result := TGraphics.Create(DC, Device);
end;

function NewGraphics(Wnd: HWND; ICM: Boolean): IGraphics;
begin
  Result := TGraphics.Create(Wnd, ICM);
end;

function NewGraphics(Image: IImage): IGraphics;
begin
  Result := TGraphics.Create(Image);
end;

procedure TGraphics.DestroyHandle(Handle: TGdiHandle);
begin
  FLastResult := GdipDeleteGraphics(Handle);
end;

constructor TGraphics.Create(DC: HDC);
var
  H: TGdiHandle;
begin
  H := nil;
  FLastResult := GdipCreateFromHDC(DC, H);
  inherited Create(H);
end;

constructor TGraphics.Create(DC: HDC; Device: THandle);
var
  H: TGdiHandle;
begin
  H := nil;
  FLastResult := GdipCreateFromHDC2(DC, Device, H);
  inherited Create(H);
end;

constructor TGraphics.Create(Wnd: HWND; ICM: Boolean);
var
  H: TGdiHandle;
begin
  H := nil;
  if ICM then
    FLastResult := GdipCreateFromHWNDICM(Wnd, H)
  else
    FLastResult := GdipCreateFromHWND(Wnd, H);
  inherited Create(H);
end;

constructor TGraphics.Create(Image: IImage);
var
  H: TGdiHandle;
begin
  H := nil;
  if Image <> nil then
    FLastResult := GdipGetImageGraphicsContext(Image.Handle, H);
  inherited Create(H);
end;

function TGraphics.GetInterpolationMode: TInterpolationMode;
begin
  FLastResult := GdipGetInterpolationMode(FHandle, Result);
end;

procedure TGraphics.SetInterpolationMode(Value: TInterpolationMode);
begin
  FLastResult := GdipSetInterpolationMode(FHandle, Value);
  if FLastResult <> Ok then
    FLastResult := Ok;
end;

function TGraphics.GetSmoothingMode: TSmoothingMode;
begin
  Result := SmoothingModeInvalid;
  FLastResult := GdipGetSmoothingMode(FHandle, Result);
end;

procedure TGraphics.SetSmoothingMode(Value: TSmoothingMode);
begin
  FLastResult := GdipSetSmoothingMode(FHandle, Value);
end;

function TGraphics.GetHDC: HDC;
begin
  FLastResult := GdipGetDC(FHandle, Result);
end;

procedure TGraphics.ReleaseHDC(DC: HDC);
begin
  FLastResult := GdipReleaseDC(FHandle, DC);
end;

function TGraphics.SetRenderingOrigin(X, Y: Integer): TStatus;
begin
  FLastResult := GdipSetRenderingOrigin(FHandle, X, Y);
  Result := FLastResult;
end;

function TGraphics.GetRenderingOrigin(out X, Y: Integer): TStatus;
begin
  FLastResult := GdipGetRenderingOrigin(FHandle, X, Y);
  Result := FLastResult;
end;

function TGraphics.DrawCachedBitmap(CachedBitmap: ICachedBitmap; X, Y: Integer): TStatus;
var
  C: PCachedBitmap;
begin
  if CachedBitmap <> nil then C := CachedBitmap.Handle else C := nil;
  FLastResult := GdipDrawCachedBitmap(FHandle, C, X, Y);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; const Point: TPointF): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImage(FHandle, I, Point.X, Point.Y);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; X, Y: Single): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImage(FHandle, I, X, Y);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; const Rect: TRectF): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  with Rect do
    FLastResult := GdipDrawImageRect(FHandle, I, X, Y, Width, Height);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; X, Y, Width, Height: Single): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImageRect(FHandle, I, X, Y, Width, Height);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; const point: TPointI): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImageI(FHandle, I, Point.X, Point.Y);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; X, Y: Integer): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImageI(FHandle, I, X, Y);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; const Rect: TRectI): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  with Rect do
    FLastResult := GdipDrawImageRectI(FHandle, I, X, Y, Width, Height);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; X, Y, Width, Height: Integer): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImageRectI(FHandle, I, X, Y, Width, Height);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImagePoints(FHandle, I, DestPoints, Count);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; DestPoints: PPointI; Count: Integer): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImagePointsI(FHandle, I, DestPoints, Count);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single;
  SrcUnit: TUnit = UnitPixel): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImagePointRect(FHandle, I, X, Y, SrcX, SrcY,
    SrcWidth, SrcHeight, SrcUnit);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth,
  SrcHeight: Integer; SrcUnit: TUnit = UnitPixel): TStatus;
var
  I: PImage;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  FLastResult := GdipDrawImagePointRectI(FHandle, I, X, Y,
    SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; const DestRect: TRectF; SrcX, SrcY,
  SrcWidth, SrcHeight: Single; SrcUnit: TUnit = UnitPixel;
  ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
  CallbackData: Pointer = nil): TStatus;
var
  I: PImage;
  H: PImageAttributes;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  if ImageAttributes <> nil then
    H := ImageAttributes.Handle
  else
    H := nil;
  with DestRect do
    FLastResult := GdipDrawImageRectRect(FHandle, I, X, Y, Width, Height,
      SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit, H, Callback, CallbackData);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer;
  SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit = UnitPixel;
  ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
  CallbackData: Pointer = nil): TStatus;
var
  I: PImage;
  H: PImageAttributes;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  if ImageAttributes <> nil then
    H := ImageAttributes.Handle
  else
    H := nil;
  FLastResult := GdipDrawImagePointsRect(FHandle, I, DestPoints, Count,
      SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit, H, Callback, CallbackData);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; const DestRect: TRectI; SrcX, SrcY,
  SrcWidth, SrcHeight: Integer; SrcUnit: TUnit = UnitPixel;
  ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
  CallbackData: Pointer = nil): TStatus;
var
  I: PImage;
  H: PImageAttributes;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  if ImageAttributes <> nil then
    H := ImageAttributes.Handle
  else
    H := nil;
  with DestRect do
    FLastResult := GdipDrawImageRectRectI(FHandle, I, X, Y, Width, Height,
      SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit, H, Callback, CallbackData);
  Result := FLastResult;
end;

function TGraphics.DrawImage(Image: IImage; DestPoints: PPointI;
  Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TUnit = UnitPixel;
  ImageAttributes: IImageAttributes = nil; Callback: TDrawImageAbort = nil;
  CallbackData: Pointer = nil): TStatus;
var
  I: PImage;
  H: PImageAttributes;
begin
  if Image <> nil then I := Image.Handle else I := nil;
  if ImageAttributes <> nil then
    H := ImageAttributes.Handle
  else
    H := nil;
  FLastResult := GdipDrawImagePointsRectI(FHandle, I, DestPoints, Count,
      SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit, H, Callback, CallbackData);
  Result := FLastResult;
end;

type
  TImage = class(TGdiplusBase, IImage)
  protected
    procedure DestroyHandle(Handle: TGdiHandle); override;
  public
    function GetImageType: TImageType;
    function GetBounds(out SrcRect: TRectF; out SrcUnit: TUnit): TStatus;
    function GetEncoderParameterListSize(const ClsidEncoder: TGUID): Cardinal;
    function GetEncoderParameterList(const ClsidEncoder: TGUID; Size: Cardinal;
      Buffer: PEncoderParameters): TStatus;
    function GetFlags: Cardinal;
    function GetFrameCount(const DimensionID: TGUID): Integer;
    function GetFrameDimensionsCount: Integer;
    function GetFrameDimensionsList(DimensionIDs: PGUID; Count: Integer): TStatus;
    function GetPixelFormat: TPixelFormat;
    function GetPaletteSize: Integer;
    function GetPalette(Palette: PColorPalette; Size: Integer): TStatus;
    function SetPalette(Palette: PColorPalette): TStatus;
    function GetThumbnailImage(ThumbWidth, ThumbHeight: Integer;
      Callback: TGetThumbnailImageAbort = nil; CallbackData: pointer = nil): IImage;
    function GetPhysicalDimension: TSizeF;
    function GetPropertyCount: Integer;
    function GetPropertyIdList(NumOfProperty: Integer; List: PPropID): TStatus;
    function GetPropertyItemSize(PropId: TPropID): Cardinal;
    function GetPropertyItem(PropId: TPropID; PropSize: Cardinal; Buffer: PPropertyItem): TStatus;
    function GetPropertySize(out TotalBufferSize, NumProperties : Integer): TStatus;
    function GetAllPropertyItems(TotalBufferSize, NumProperties: Integer;
      AllItems: PPropertyItem): TStatus;
    function SetPropertyItem(const Item: TPropertyItem): TStatus;
    function GetRawFormat: TGUID;
    function GetHorizontalResolution: Single;
    function GetVerticalResolution: Single;
    function GetSize: TSizeI;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function FromFile(const FileName: WideString; ICM: Boolean =  False): IImage;
    function FromStream(Stream: IStream; ICM: Boolean =  False): IImage;
    function Clone: IImage;
    function RemovePropertyItem(PropId: TPropID): TStatus;
    function RotateFlip(RotateFlipType: TRotateFlipType): TStatus;
    function Save(const FileName: WideString; const ClsidEncoder: TGUID;
      EncoderParams: PEncoderParameters = nil): TStatus; overload;
    function Save(Stream: IStream; const ClsidEncoder: TGUID;
      EncoderParams: PEncoderParameters  = nil): TStatus; overload;
    function SaveAdd(EncoderParams: PEncoderParameters): TStatus; overload;
    function SaveAdd(NewImage: IImage; EncoderParams: PEncoderParameters): TStatus; overload;
    function SelectActiveFrame(const DimensionID: TGUID; FrameIndex: Integer): TStatus;
  end;

function ImageFromHandle(Handle: Pointer): TImage; forward;

function NewImage(const FileName: string; ICM: Boolean = False): IImage;
var
  I: IImage;
begin
  I := TImage.Create(nil);
  Result := I.FromFile(FileName, ICM);
end;

function NewImage(Stream: IStream; ICM: Boolean = False): IImage;
var
  I: IImage;
begin
  I := TImage.Create(nil);
  Result := I.FromStream(Stream, ICM);
end;

procedure TImage.DestroyHandle(Handle: TGdiHandle);
begin
  FLastResult := GdipDisposeImage(Handle);
end;

function TImage.GetImageType: TImageType;
begin
  FLastResult := GdipGetImageType(FHandle, Result);
end;

function TImage.GetBounds(out SrcRect: TRectF; out SrcUnit: TUnit): TStatus;
begin
  FLastResult := GdipGetImageBounds(FHandle, SrcRect, SrcUnit);
  Result := FLastResult;
end;

function TImage.GetEncoderParameterListSize(const ClsidEncoder: TGUID): Cardinal;
begin
  FLastResult := GdipGetEncoderParameterListSize(FHandle, ClsidEncoder, Result);
end;

function TImage.GetEncoderParameterList(const ClsidEncoder: TGUID; Size: Cardinal;
  Buffer: PEncoderParameters): TStatus;
begin
  FLastResult := GdipGetEncoderParameterList(FHandle, ClsidEncoder, Size, Buffer);
  Result := FLastResult;
end;

function TImage.GetFlags: Cardinal;
begin
  FLastResult := GdipGetImageFlags(FHandle, Result);
end;

function TImage.GetFrameCount(const DimensionID: TGUID): Integer;
begin
  FLastResult := GdipImageGetFrameCount(FHandle, DimensionID, Result);
end;

function TImage.GetFrameDimensionsCount: Integer;
begin
  FLastResult := GdipImageGetFrameDimensionsCount(FHandle, Result);
end;

function TImage.GetFrameDimensionsList(DimensionIDs: PGUID; Count: Integer): TStatus;
begin
  FLastResult := GdipImageGetFrameDimensionsList(FHandle, DimensionIDs, Count);
  Result := FLastResult;
end;

function TImage.GetPixelFormat: TPixelFormat;
begin
  FLastResult := GdipGetImagePixelFormat(FHandle, Result);
end;

function TImage.GetPaletteSize: Integer;
begin
  FLastResult := GdipGetImagePaletteSize(FHandle, Result);
end;

function TImage.GetPalette(Palette: PColorPalette; Size: Integer): TStatus;
begin
  FLastResult := GdipGetImagePalette(FHandle, Palette, Size);
  Result := FLastResult;
end;

function TImage.SetPalette(Palette: PColorPalette): TStatus;
begin
  FLastResult := GdipSetImagePalette(FHandle, Palette);
  Result := FLastResult;
end;

function TImage.GetThumbnailImage(ThumbWidth, ThumbHeight: Integer;
  Callback: TGetThumbnailImageAbort = nil; CallbackData: pointer = nil): IImage;
var
  I: PImage;
  O: TImage;
begin
  I := nil;
  FLastResult := GdipGetImageThumbnail(FHandle, ThumbWidth, ThumbHeight, I,
    Callback, CallbackData);
  Result := nil;
  if (FLastResult = Ok) and (I <> nil) then
  begin
    O := ImageFromHandle(I);
    if O <> nil then
      Result := O
    else
      GdipDisposeImage(I);
  end
  else if I <> nil then
    GdipDisposeImage(I);
end;

function TImage.GetPhysicalDimension: TSizeF;
begin
  with Result do
  begin
    Width := 0;
    Height := 0;
    FLastResult := GdipGetImageDimension(FHandle, Width, Height);
  end;
end;

function TImage.GetPropertyCount: Integer;
begin
  FLastResult := GdipGetPropertyCount(FHandle, Result);
end;

function TImage.GetPropertyIdList(NumOfProperty: Integer; List: PPropID): TStatus;
begin
  FLastResult := GdipGetPropertyIdList(FHandle, NumOfProperty, List);
  Result := FLastResult;
end;

function TImage.GetPropertyItemSize(PropId: TPropID): Cardinal;
begin
  FLastResult := GdipGetPropertyItemSize(FHandle, PropId, Result);
end;

function TImage.GetPropertyItem(PropId: TPropID; PropSize: Cardinal; Buffer: PPropertyItem): TStatus;
begin
  FLastResult := GdipGetPropertyItem(FHandle, PropId, PropSize, Buffer);
  Result := FLastResult;
end;

function TImage.GetPropertySize(out TotalBufferSize, NumProperties : Integer): TStatus;
begin
  FLastResult := GdipGetPropertySize(FHandle, TotalBufferSize, NumProperties);
  Result := FLastResult;
end;

function TImage.GetAllPropertyItems(TotalBufferSize, NumProperties: Integer;
  AllItems: PPropertyItem): TStatus;
begin
  FLastResult := GdipGetAllPropertyItems(FHandle, TotalBufferSize, NumProperties, AllItems);
  Result := FLastResult;
end;

function TImage.SetPropertyItem(const Item: TPropertyItem): TStatus;
begin
  FLastResult := GdipSetPropertyItem(FHandle, Item);
  Result := FLastResult;
end;

function TImage.GetRawFormat: TGUID;
begin
  FLastResult := GdipGetImageRawFormat(FHandle, Result);
end;

function TImage.GetHorizontalResolution: Single;
begin
  FLastResult := GdipGetImageHorizontalResolution(FHandle, Result);
end;

function TImage.GetVerticalResolution: Single;
begin
  FLastResult := GdipGetImageVerticalResolution(FHandle, Result);
end;

function TImage.GetSize: TSizeI;
begin
  with Result do
  begin
    Width := GetWidth;
    Height := GetHeight;
  end;    
end;

function TImage.GetWidth: Integer;
begin
  FLastResult := GdipGetImageWidth(FHandle, Result);
end;

function TImage.GetHeight: Integer;
begin
  FLastResult := GdipGetImageHeight(FHandle, Result);
end;

function TImage.FromFile(const FileName: WideString; ICM: Boolean =  False): IImage;
var
  I: PImage;
  O: TImage;
begin
  I := nil;
  if ICM then
    FLastResult := GdipLoadImageFromFileICM(PWChar(FileName), I)
  else
    FLastResult := GdipLoadImageFromFile(PWChar(FileName), I);
  Result := nil;
  if (FLastResult = Ok) and (I <> nil) then
  begin
    O := ImageFromHandle(I);
    if O <> nil then
      Result := O
    else
      GdipDisposeImage(I);
  end
  else if I <> nil then
    GdipDisposeImage(I);
end;

function TImage.FromStream(Stream: IStream; ICM: Boolean =  False): IImage;
var
  I: PImage;
  O: TImage;
begin
  I := nil;
  if ICM then
    FLastResult := GdipLoadImageFromStreamICM(Stream, I)
  else
    FLastResult := GdipLoadImageFromStream(Stream, I);
  Result := nil;
  if (FLastResult = Ok) and (I <> nil) then
  begin
    O := ImageFromHandle(I);
    if O <> nil then
      Result := O
    else
      GdipDisposeImage(I);
  end
  else if I <> nil then
    GdipDisposeImage(I);
end;

function TImage.Clone: IImage;
var
  I: PImage;
  O: TImage;
begin
  I := nil;
  FLastResult := GdipCloneImage(FHandle, I);
  Result := nil;
  if (FLastResult = Ok) and (I <> nil) then
  begin
    O := ImageFromHandle(I);
    if O <> nil then
      Result := O
    else
      GdipDisposeImage(I);
  end
  else if I <> nil then
    GdipDisposeImage(I);
end;

function TImage.RemovePropertyItem(PropId: TPropID): TStatus;
begin
  FLastResult := GdipRemovePropertyItem(FHandle, PropID);
  Result := FLastResult;
end;

function TImage.RotateFlip(RotateFlipType: TRotateFlipType): TStatus;
begin
  GdipImageRotateFlip(FHandle, RotateFlipType);
  Result := FLastResult;
end;

function TImage.Save(const FileName: WideString; const ClsidEncoder: TGUID;
  EncoderParams: PEncoderParameters = nil): TStatus;
begin
  FLastResult := GdipSaveImageToFile(FHandle, PWChar(FileName), ClsidEncoder,
    EncoderParams);
  Result := FLastResult;
end;

function TImage.Save(Stream: IStream; const ClsidEncoder: TGUID;
  EncoderParams: PEncoderParameters  = nil): TStatus;
begin
  FLastResult := GdipSaveImageToStream(FHandle, Stream, ClsidEncoder,
    EncoderParams);
  Result := FLastResult;
end;

function TImage.SaveAdd(EncoderParams: PEncoderParameters): TStatus;
begin
  FLastResult := GdipSaveAdd(FHandle, EncoderParams);
  Result := FLastResult;
end;

function TImage.SaveAdd(NewImage: IImage; EncoderParams: PEncoderParameters): TStatus;
var
  I: PImage;
begin
  I := nil;
  if NewImage <> nil then
    I := NewImage.Handle;
  FLastResult := GdipSaveAddImage(FHandle, I, EncoderParams);
  Result := FLastResult;
end;

function TImage.SelectActiveFrame(const DimensionID: TGUID; FrameIndex: Integer): TStatus;
begin
  FLastResult := GdipImageSelectActiveFrame(FHandle, DimensionID, FrameIndex);
  Result := FLastResult;
end;

type
  TBitmap = class(TImage, IBitmap)
  public
    constructor Create(const FileName: WideString; ICM: Boolean); overload;
    constructor Create(Stream: IStream; ICM: Boolean); overload;
    constructor Create(Width, Height: Integer;
      Format: TPixelFormat = PixelFormat32bppARGB); overload;
    constructor Create(Width, Height: Integer; Target: IGraphics); overload;
    constructor Create(Width, Height, Stride: Integer;
      Format: TPixelFormat; Scan0: PByte); overload;
    constructor Create(var BitmapInfo: TBitmapInfo; BitmapData: Pointer); overload;
    constructor Create(Bitmap: HBITMAP; Palette: HPALETTE); overload;
    constructor Create(Icon: HICON); overload;
    constructor Create(Module: HMODULE; const ResName: WideString); overload;
    function GetPixel(X, Y: Integer): TColor;
    procedure SetPixel(X, Y: Integer; Value: TColor);
    function Clone(Rect: TRectI; Format: TPixelFormat): IBitmap; overload;
    function Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): IBitmap; overload;
    function Clone(Rect: TRectF; Format: TPixelFormat): IBitmap; overload;
    function Clone(X, Y, Width, Height: Single; Format: TPixelFormat): IBitmap; overload;
    function LockBits(const Rect: TRectI; Flags: UINT; Format: TPixelFormat;
      var LockedBitmapData: TBitmapData): TStatus;
    function UnlockBits(const LockedBitmapData: TBitmapData): TStatus;
    function SetResolution(DpiX, DpiY: Single): TStatus;
  end;

function NewBitmap(const FileName: string; ICM: Boolean = False): IBitmap;
begin
  Result := TBitmap.Create(FileName, ICM);
end;

function NewBitmap(Stream: IStream; ICM: Boolean = False): IBitmap;
begin
  Result := TBitmap.Create(Stream, ICM);
end;

function NewBitmap(Width, Height: Integer; Format: TPixelFormat = PixelFormat32bppARGB): IBitmap;
begin
  Result := TBitmap.Create(Width, Height, Format);
end;

function NewBitmap(Width, Height: Integer; Target: IGraphics): IBitmap;
begin
  Result := TBitmap.Create(Width, Height, Target);
end;

function NewBitmap(Width, Height, Stride: Integer; Format: TPixelFormat; Scan0: PByte): IBitmap;
begin
  Result := TBitmap.Create(Width, Height, Stride, Format, Scan0);
end;

function NewBitmap(var BitmapInfo: TBitmapInfo; BitmapData: Pointer): IBitmap;
begin
  Result := TBitmap.Create(BitmapInfo, BitmapData);
end;

function NewBitmap(Bitmap: HBITMAP; Palette: HPALETTE): IBitmap;
begin
  Result := TBitmap.Create(Bitmap, Palette);
end;

function NewBitmap(Icon: HICON): IBitmap;
begin
  Result := TBitmap.Create(Icon);
end;

function NewBitmap(Module: HMODULE; const ResName: string): IBitmap;
begin
  Result := TBitmap.Create(Module, ResName);
end;

constructor TBitmap.Create(const FileName: WideString; ICM: Boolean);
var
  H: TGdiHandle;
begin
  H := nil;
  if ICM then
    FLastResult := GdipCreateBitmapFromFileICM(PWideChar(FileName), H)
  else
    FLastResult := GdipCreateBitmapFromFile(PWideChar(FileName), H);
  inherited Create(H);
end;

constructor TBitmap.Create(Stream: IStream; ICM: Boolean);
var
  H: TGdiHandle;
begin
  H := nil;
  if ICM then
    FLastResult := GdipCreateBitmapFromStreamICM(Stream, H)
  else
    FLastResult := GdipCreateBitmapFromStream(Stream, H);
  inherited Create(H);
end;

constructor TBitmap.Create(Width, Height: Integer;
  Format: TPixelFormat = PixelFormat32bppARGB);
var
  H: TGdiHandle;
begin
  H := nil;
  FLastResult := GdipCreateBitmapFromScan0(Width, Height, 0, Format, nil, H);
  inherited Create(H);
end;

constructor TBitmap.Create(Width, Height: Integer; Target: IGraphics);
var
  G: PGraphics;
  H: TGdiHandle;
begin
  H := nil;
  if Target <> nil then G := Target.Handle else G := nil;
  FLastResult := GdipCreateBitmapFromGraphics(Width, Height, G, H);
  inherited Create(H);
end;

constructor TBitmap.Create(Width, Height, Stride: Integer;
  Format: TPixelFormat; Scan0: PByte);
var
  H: TGdiHandle;
begin
  H := nil;
  FLastResult := GdipCreateBitmapFromScan0(Width, Height, Stride, Format, Scan0, H);
  inherited Create(H);
end;

constructor TBitmap.Create(var BitmapInfo: TBitmapInfo; BitmapData: Pointer);
var
  H: TGdiHandle;
begin
  H := nil;
  FLastResult := GdipCreateBitmapFromGdiDib(BitmapInfo, BitmapData, H);
  inherited Create(H);
end;

constructor TBitmap.Create(Bitmap: HBITMAP; Palette: HPALETTE);
var
  H: TGdiHandle;
begin
  H := nil;
  FLastResult := GdipCreateBitmapFromHBITMAP(Bitmap, Palette, H);
  inherited Create(H);
end;

constructor TBitmap.Create(Icon: HICON);
var
  H: TGdiHandle;
begin
  H := nil;
  FLastResult := GdipCreateBitmapFromHICON(Icon, H);
  inherited Create(H);
end;

constructor TBitmap.Create(Module: HMODULE; const ResName: WideString);
var
  H: TGdiHandle;
begin
  H := nil;
  FLastResult := GdipCreateBitmapFromResource(Module, PWideChar(ResName), H);
  inherited Create(H);
end;

function TBitmap.GetPixel(X, Y: Integer): TColor;
begin
  FLastResult := GdipBitmapGetPixel(FHandle, X, Y, Result);
end;

procedure TBitmap.SetPixel(X, Y: Integer; Value: TColor);
begin
  FLastResult := GdipBitmapSetPixel(FHandle, X, Y, Value);
end;

function TBitmap.Clone(Rect: TRectI; Format: TPixelFormat): IBitmap;
begin
  with Rect do
    Result := Clone(X, Y, Width, Height, Format);
end;

function TBitmap.Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): IBitmap;
var
  B: PBitmap;
begin
  B := nil;
  FLastResult := GdipCloneBitmapAreaI(X, Y, Width, Height, Format, FHandle, B);
  if (FLastResult = Ok) and (B <> nil) then
    Result := TBitmap.Create(B)
  else
    Result := nil;
end;

function TBitmap.Clone(Rect: TRectF; Format: TPixelFormat): IBitmap;
begin
  with Rect do
    Result := Clone(X, Y, Width, Height, Format);
end;

function TBitmap.Clone(X, Y, Width, Height: Single; Format: TPixelFormat): IBitmap;
var
  B: PBitmap;
begin
  B := nil;
  FLastResult := GdipCloneBitmapArea(X, Y, Width, Height, Format, FHandle, B);
  if (FLastResult = Ok) and (B <> nil) then
    Result := TBitmap.Create(B)
  else
    Result := nil;
end;

function TBitmap.LockBits(const Rect: TRectI; Flags: UINT; Format: TPixelFormat;
  var LockedBitmapData: TBitmapData): TStatus;
begin
  FLastResult := GdipBitmapLockBits(FHandle, Rect, Flags, Format, LockedBitmapData);
  Result := FLastResult;
end;

function TBitmap.UnlockBits(const LockedBitmapData: TBitmapData): TStatus;
begin
  FLastResult := GdipBitmapUnlockBits(FHandle, LockedBitmapData);
  Result := FLastResult;
end;

function TBitmap.SetResolution(DpiX, DpiY: Single): TStatus;
begin
  FLastResult := GdipBitmapSetResolution(FHandle, DpiX, DpiY);
  Result := FLastResult;
end;

type
  TMetafile = class(TImage, IMetafile)
  end;

function ImageFromHandle(Handle: Pointer): TImage; 
var
  ImageType: TImageType;
begin
  Result := nil;
  if (Handle <> nil) and (GdipGetImageType(Handle, ImageType) = Ok) then
    case ImageType of
      ImageTypeBitmap: Result := TBitmap.Create(Handle);
      ImageTypeMetafile: Result := TMetafile.Create(Handle);
    end;
end;

type
  TCachedBitmap = class(TGdiplusBase, ICachedBitmap)
  protected
    procedure DestroyHandle(Handle: TGdiHandle); override;
  public
    constructor Create(Bitmap: IBitmap; Graphics: IGraphics);
  end;

function NewCachedBitmap(Bitmap: IBitmap; Graphics: IGraphics): ICachedBitmap;
begin
  Result := TCachedBitmap.Create(Bitmap, Graphics);
end;

procedure TCachedBitmap.DestroyHandle(Handle: TGdiHandle);
begin
  FLastResult := GdipDeleteCachedBitmap(Handle);
end;

constructor TCachedBitmap.Create(Bitmap: IBitmap; Graphics: IGraphics);
var
  B, G, H: TGdiHandle;
begin
  if Bitmap <> nil then B := Bitmap.Handle else B := nil;
  if Graphics <> nil then G := Graphics.Handle else G := nil;
  H := nil;
  FLastResult := GdipCreateCachedBitmap(B, G, H);
  inherited Create(H);
end;

type
  TImageAttributes = class(TGdiplusBase, IImageAttributes)
  protected
    procedure DestroyHandle(Handle: TGdiHandle); override;
    function Clone: IImageAttributes;
    function SetColorMatrix(const Matrix: TColorMatrix;
      Flags: TColorMatrixFlags = ColorMatrixFlagsDefault;
      Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetGamma(Gamma: Single; Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearGamma(Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
  public
    constructor Create; overload;
  end;

function NewImageAttributes: IImageAttributes;
begin
  Result := TImageAttributes.Create;
end;

function NewImageAttributes(Transform: TColorTransform): IImageAttributes;
begin
  Result := TImageAttributes.Create;
  Result.SetColorMatrix(ColorTransform(Transform));
  if Transform.Gamma <> 1 then
    Result.SetGamma(Transform.Gamma);
end;

procedure TImageAttributes.DestroyHandle(Handle: TGdiHandle);
begin
  FLastResult := GdipDisposeImageAttributes(Handle);
end;

constructor TImageAttributes.Create;
var
  H: PImageAttributes;
begin
  H := nil;
  FLastResult := GdipCreateImageAttributes(H);
  inherited Create(H);
end;

function TImageAttributes.Clone: IImageAttributes;
var
  H: PImageAttributes;
begin
  H := nil;
  FLastResult := GdipCloneImageAttributes(FHandle, H);
  if (FLastResult = Ok) and (H <> nil) then
    Result := TImageAttributes.Create(H)
  else
    Result := nil;
end;

function TImageAttributes.SetColorMatrix(const Matrix: TColorMatrix;
  Flags: TColorMatrixFlags = ColorMatrixFlagsDefault;
  Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  FLastResult := GdipSetImageAttributesColorMatrix(FHandle,
    Adjust, True, @Matrix, nil, Flags);
  Result := FLastResult;
end;

function TImageAttributes.SetGamma(Gamma: Single;
  Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  FLastResult := GdipSetImageAttributesGamma(FHandle, Adjust, True, Gamma);
  Result := FLastResult;
end;

function TImageAttributes.ClearGamma(Adjust: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  FLastResult := GdipSetImageAttributesGamma(FHandle, Adjust, False, 0.0);
  Result := FLastResult;
end;

{ Startup/Shutdown routines }

var
  StartupInput: TGDIPlusStartupInput;
  GdiplusToken: ULONG;

procedure Startup;
var
  S: string;
begin
  Str(GetCurrentProcessId, S);
  S := 'GdiHandleMutex' + S;
  FillChar(HandleName, SizeOf(HandleName), #0);
  Move(PChar(@S[1])^, HandleName, Length(S) + 1);
  if IsLibrary then Exit;
  StartupInput.GdiplusVersion := 1;
  GdiplusStartup(GdiplusToken, @StartupInput, nil);
end;

procedure Shutdown;
begin
  if IsLibrary then Exit;
  GdiplusShutdown(GdiplusToken);
end;

initialization
  Startup;
finalization
  Shutdown;
end.
