////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// FloatMapUtils.pas - TFloatMap processing
// ----------------------------------------
// Version:   2006-12-26
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
unit FloatMapUtils;

interface

uses
  Windows, Classes, SysUtils, Math, SortUtils, FloatMap,
  LinarBitmap, MathUtils, MatrixMath, ExpressionEval, Graphics, Types;

type
  TFloatPlanes = array of TFloatMap;
  TFloatPlanes32 = array of TFloatMap32;

// Set image values from expression in x,y
procedure CreateMapFromExpression(Image: TFloatMap; const Expression: string; Defines: TSymbolTable=nil);
procedure UpdateMapWithExpression(Image: TFloatMap; Expression: string; Defines: TSymbolTable=nil; Source: TFloatMap=nil);
procedure MedianFilterFloat(Image: TFloatMap; SquareSize: Integer=3);
procedure FloatMapDillation(Image: TFloatMap; SquareSize: Integer=3);
// Apply square FIR filter with equal values
procedure SmoothFilterFloat(Image: TFloatMap; SquareSize: Integer=3; NewImage: TFloatMap=nil);
// Create gaussian filter kernel
// Precision determines how much the Gauss bell must decay before truncating, i.e. how big the matrix is
function CreateGaussianKernel(const Variance: Double; const Precision: Double=1e-7): TMatrix;
// First-order IIR filters
type TFilterDirection = (fdBoth,fdHorizontal,fdVertical);
procedure IIRLowpassFilterFloat(Image: TFloatMap; Lookback: Double; Direction: TFilterDirection=fdBoth); overload;
procedure IIRLowpassFilterFloat(Image: TFloatMap32; Lookback: Single; Direction: TFilterDirection=fdBoth); overload;
procedure IIRHighpassFilterFloat(Image: TMatrix; Lookback: Double; Direction: TFilterDirection=fdBoth; HorzResult: TFloatMap=nil; VertResult: TFloatMap=nil);
procedure IIREdgeDetectFilterFloat(Image: TMatrix; Lookback: Double; Direction: TFilterDirection; HorzResult: TFloatMap=nil; VertResult: TFloatMap=nil); overload;
procedure IIREdgeDetectFilterFloat(Image: TFloatMap32; Lookback: Single; Direction: TFilterDirection; HorzResult: TFloatMap32=nil; VertResult: TFloatMap32=nil); overload;
procedure IIRMaxFilterFloat(Image: TFloatMap32; Lookback: Single);
procedure IIRMinFilterFloat(Image: TFloatMap32; Lookback: Single);
procedure IIRMaxFilterFloatEdge(Image,EdgePlane: TFloatMap32; Lookback: Single);
procedure IIRMinFilterFloatEdge(Image,EdgePlane: TFloatMap32; Lookback: Single);
// Resizing
procedure BilinearResizeFloatMap(Image: TFloatMap; NewWidth,NewHeight: Integer; NewImage: TFloatMap=nil);
procedure DownsampleFloat(Image: TFloatMap; Scale: Integer; NewImage: TFloatMap=nil; XShift: Integer=0; YShift: Integer=0);
procedure ResizeHalfFloat(Image: TFloatMap; NewImage: TFloatMap=nil);
procedure PixelResizeFloat(Image: TFloatMap; NewImage: TFloatMap; ScaleFactor: Integer);
procedure TileFloatMap(Image: TFloatMap; XCount, YCount: Integer; NewImage: TFloatMap=nil);
// Rotation
procedure RotateFloatMap270(Image: TFloatMap);
procedure FlipFloatMap(Image: TFloatMap);
procedure MirrorFloatMap(Image: TFloatMap);
// Color space conversion
procedure ConvertToGrayScaleFloat(Image: TLinarBitmap; DestPlane: TFloatMap32);
procedure SplitColorPlanesFloat(Image: TLinarBitmap; Plane1,Plane2,Plane3: TFloatMap; const T: TMatrix4x4); overload;
procedure SplitColorPlanesFloat(Image: TLinarBitmap; Plane1,Plane2,Plane3: TFloatMap32; const T: TMatrix4x4); overload;
procedure CombineColorPlanesFloat(Plane1,Plane2,Plane3: TFloatMap; Image: TLinarBitmap; const T: TMatrix4x4); overload;
procedure CombineColorPlanesFloat(Plane1,Plane2,Plane3: TFloatMap32; Image: TLinarBitmap; const T: TMatrix4x4); overload;
procedure ConvertColorSpace(SrcPlane1,SrcPlane2,SrcPlane3,DestPlane1,DestPlane2,DestPlane3: TFloatMap; const T: TMatrix4x4); overload;
procedure ConvertColorSpace(SrcPlane1,SrcPlane2,SrcPlane3,DestPlane1,DestPlane2,DestPlane3: TFloatMap32; const T: TMatrix4x4); overload;

implementation

uses BitmapConversion, MultiCoreProcessing;

procedure MedianFilterFloat(Image: TFloatMap; SquareSize: Integer);
var
  List, NewMap : PFloatArray;
  X, Y, DX, DY, DMax, DMin, XA, XB, YA, YB  : Integer;
  Count : DWord;
  CenterPix, ScanPix : PFloat;
begin
  DMin:=-SquareSize div 2;
  DMax:=SquareSize+DMin-1;
  with Image do
  begin
    GetMem(List,Sqr(SquareSize)*SizeOf(Float));
    GetMem(NewMap,Size*SizeOf(Float));
    try
      CenterPix:=@NewMap^;
      for Y:=0 to Height-1 do
      begin
        if Assigned(ProgressUpdate) then ProgressUpdate(Y*100 div (Height-1));
        YA:=Y+DMin; if YA<0 then YA:=0;
        YB:=Y+DMax; if YB>=Height then YB:=Height-1;
        for X:=0 to Width-1 do
        begin
          Count:=0;
          XA:=X+DMin; if XA<0 then XA:=0;
          XB:=X+DMax; if XB>=Width then XB:=Width-1;
          for DY:=YA to YB do
          begin
            ScanPix:=@Map^[DY*Width+XA];
            for DX:=XA to XB do
            begin
              List^[Count]:=ScanPix^;
              Inc(Count);
              Inc(ScanPix);
            end;
          end;
          CenterPix^:=QuickSelectFloat(List^,0,Count-1,Count div 2);

          Inc(CenterPix);
        end;
      end;
      FreeMem(Map);
      Map:=NewMap;
    finally
      FreeMem(List);
    end;
  end;
end;

procedure FloatMapDillation(Image: TFloatMap; SquareSize: Integer=3);
var
  NewMap : PFloatArray;
  X, Y, DX, DY, DMax, DMin, XA, XB, YA, YB  : Integer;
  CenterPix, ScanPix : PFloat;
  Max : Float;
begin
  DMin:=-SquareSize div 2;
  DMax:=SquareSize+DMin-1;
  with Image do
  begin
    GetMem(NewMap,Size*SizeOf(Float));
    try
      CenterPix:=@NewMap^;
      for Y:=0 to Height-1 do
      begin
        YA:=Y+DMin; if YA<0 then YA:=0;
        YB:=Y+DMax; if YB>=Height then YB:=Height-1;
        for X:=0 to Width-1 do
        begin
          Max:=NegInfinity;
          XA:=X+DMin; if XA<0 then XA:=0;
          XB:=X+DMax; if XB>=Width then XB:=Width-1;
          for DY:=YA to YB do
          begin
            ScanPix:=@Map^[DY*Width+XA];
            for DX:=XA to XB do
            begin
              if ScanPix^>Max then Max:=ScanPix^;
              Inc(ScanPix);
            end;
          end;
          CenterPix^:=Max;

          Inc(CenterPix);
        end;
      end;
    finally
      FreeMem(Map);
      Map:=NewMap;
    end;
  end;
end;

procedure SmoothFilterFloat(Image: TFloatMap; SquareSize: Integer; NewImage: TFloatMap);
var
  NewMap : PFloatArray;
  X, Y, DX, DY, DMax, DMin, XA, XB, YA, YB  : Integer;
  Count : DWord;
  CenterPix, ScanPix : PFloat;
  Sum : Float;
  ReplaceImage : Boolean;
begin
  ReplaceImage:=(NewImage=nil) or (NewImage=Image);
  DMin:=-SquareSize div 2;
  DMax:=SquareSize+DMin-1;
  with Image do
  begin
    if ReplaceImage then GetMem(NewMap,Size*SizeOf(Float))
    else
    begin
      NewImage.New(Width,Height);
      NewMap:=NewImage.Map;
    end;

    CenterPix:=@NewMap^;
    for Y:=0 to Height-1 do
    begin
      if Assigned(ProgressUpdate) then ProgressUpdate(Y*100 div (Height-1));
      YA:=Y+DMin; if YA<0 then YA:=0;
      YB:=Y+DMax; if YB>=Height then YB:=Height-1;
      for X:=0 to Width-1 do
      begin
        Count:=0; Sum:=0;
        XA:=X+DMin; if XA<0 then XA:=0;
        XB:=X+DMax; if XB>=Width then XB:=Width-1;
        for DY:=YA to YB do
        begin
          ScanPix:=@Map^[DY*Width+XA];
          for DX:=XA to XB do
          begin
            Sum:=Sum+ScanPix^;
            Inc(Count);
            Inc(ScanPix);
          end;
        end;
        CenterPix^:=Sum/Count;
        Inc(CenterPix);
      end;
    end;
    if ReplaceImage then
    begin
      FreeMem(Map);
      Map:=NewMap;
    end;
  end;
end;


procedure ResizeHalfFloat(Image,NewImage: TFloatMap);
type
  GrayList = packed array[0..1] of Float;
var
  OutPix : PFloat;
  DrawPix1, DrawPix2 : ^GrayList;
  OverwriteSource : Boolean;
  X, Y : Integer;
begin
  OverwriteSource:=(NewImage=Image) or (NewImage=nil);
  if OverwriteSource then NewImage:=TFloatMap.Create;
  NewImage.New(Image.Width div 2,Image.Height div 2);
  for Y:=0 to NewImage.Height-1 do
  begin
    OutPix:=NewImage.ScanLine[Y];
    DrawPix1:=Image.ScanLine[Y*2];
    DrawPix2:=Image.ScanLine[Y*2+1];
    for X:=1 to NewImage.Width do
    begin
      OutPix^:=(DrawPix1^[0]+DrawPix2^[0]+DrawPix1^[1]+DrawPix2^[1])/4;
      Inc(OutPix);
      Inc(DrawPix1); Inc(DrawPix2);
    end;
  end;
  if OverwriteSource then
  begin
    Image.TakeOver(NewImage);
    NewImage.Free;
  end;
end;

procedure PixelResizeFloat(Image,NewImage: TFloatMap; ScaleFactor: Integer);
var
  X, Y, I : Integer;
  Pix, NewPix : PFloat;
begin
  Assert(ScaleFactor>1);
  NewImage.New(Image.Width*ScaleFactor,Image.Height*ScaleFactor);
  for Y:=0 to Image.Height-1 do
  begin
    Pix:=Image.ScanLine[Y];
    NewPix:=NewImage.ScanLine[Y*ScaleFactor];
    for X:=1 to Image.Width do
    begin
      for I:=1 to ScaleFactor do
      begin
        NewPix^:=Pix^;
        Inc(NewPix);
      end;
      Inc(Pix);
    end;
    for I:=1 to ScaleFactor-1 do
      Move(NewImage.ScanLine[Y*ScaleFactor]^,NewImage.ScanLine[Y*ScaleFactor+I]^,NewImage.BytesPerLine);
  end;
end;

type
  TFloatArray1 = array[0..0] of Float;
  PFloatArray1 = ^TFloatArray1;

procedure DownsampleFloat(Image: TFloatMap; Scale: Integer; NewImage: TFloatMap; XShift,YShift: Integer);
var
  OutPix : PFloat;
  SrcPix : PFloatArray;
  OverwriteSource : Boolean;
  X, Y, IX, IY : Integer;
  Sum, Normalize : Double;
begin
  Assert((Scale>1) and (XShift>=0) and (YShift>=0));
  Normalize:=1/Sqr(Scale);
  OverwriteSource:=(NewImage=Image) or (NewImage=nil);
  if OverwriteSource then NewImage:=TFloatMap.Create;
  NewImage.New((Image.Width-XShift) div Scale,(Image.Height-YShift) div Scale);
  for Y:=0 to NewImage.Height-1 do
  begin
    OutPix:=NewImage.ScanLine[Y];
    for X:=0 to NewImage.Width-1 do
    begin
      Sum:=0;
      for IY:=0 to Scale-1 do
      begin
        SrcPix:=Pointer(Image.Pixel[X*Scale+XShift,Y*Scale+YShift+IY]);
        for IX:=0 to Scale-1 do Sum:=Sum+SrcPix[IX];
      end;
      OutPix^:=Sum*Normalize;
      Inc(OutPix);
    end;
  end;
  if OverwriteSource then
  begin
    Image.TakeOver(NewImage);
    NewImage.Free;
  end;
end;

procedure TileFloatMap(Image: TFloatMap; XCount, YCount: Integer; NewImage: TFloatMap);
var
  OverwriteSource : Boolean;
  X, Y : Integer;
begin
  OverwriteSource:=(NewImage=Image) or (NewImage=nil);
  if OverwriteSource then NewImage:=TFloatMap.Create;
  try
    NewImage.New(Image.Width*XCount,Image.Height*YCount);
    for Y:=0 to YCount-1 do
      for X:=0 to XCount-1 do
        NewImage.Paste(Image,X*Image.Width,Y*Image.Height);
    if OverwriteSource then Image.TakeOver(NewImage);
  finally
    if OverwriteSource then NewImage.Free;
  end;
end;

procedure BilinearResizeFloatMap(Image: TFloatMap; NewWidth,NewHeight: Integer; NewImage: TFloatMap);

  procedure SetupScaling(Size, NewSize: Integer; out Offset, Step: Double);
  var
    Scale : Double;
  begin
    if NewSize<=1 then
    begin
      Offset:=0.5;
      Step:=0;
    end
    else
    if NewSize>=Size then // >=100%
    begin
      Offset:=0;
      Step:=(Size-1)/(NewSize-1);
    end
    else // 50% - 100%
    begin
      Scale:=NewSize/Size;
      if Scale>=0.5 then
      begin
        Offset:=1-Scale;
        Step:=(Size-1-2*Offset)/(NewSize-1);
      end
      else // <50%
      begin
        Offset:=0.5;
        Step:=(Size-2)/(NewSize-1);;
      end;
    end;
  end;

var
  XOffset, YOffset, DX, DY : Double;

  procedure ProcessLine(NY: Integer);
  var
    NX, TruncX, TruncY : Integer;
    NewPix : PDouble;
    Line : PDoubleArray;
    X, Y, FracX, FracY : Double;
  begin
    Y:=NY*DY+YOffset;
    with Image do
    begin
      NewPix:=NewImage.ScanLine[NY];
      X:=XOffset;
      TruncY:=Trunc(Y);
      if TruncY>=Height-1 then
      begin
        TruncY:=Height-2;
        FracY:=1;
      end
      else FracY:=Frac(Y);

      Line:=@Map^[TruncY*Width];
      for NX:=0 to NewImage.Width-1 do
      begin
        TruncX:=Trunc(X);
        FracX:=Frac(X);
        NewPix^:=Line^[TruncX]*(1-FracX)*(1-FracY)       + Line^[TruncX+1]*(  FracX)*(1-FracY) +
                 Line^[TruncX+Width]*(1-FracX)*(  FracY) + Line^[TruncX+Width+1]*(  FracX)*(  FracY);
        X:=X+DX;
        Inc(NewPix);
      end;
    end;
  end;

var
  OverwriteSource : Boolean;
begin
  OverwriteSource:=(NewImage=Image) or (NewImage=nil);
  if OverwriteSource then
    if (Image.Width=NewWidth) and (Image.Height=NewHeight) then Exit
    else NewImage:=TFloatMap.Create;
  NewImage.New(NewWidth,NewHeight);

  SetupScaling(Image.Width,NewImage.Width,XOffset,DX);
  SetupScaling(Image.Height,NewImage.Height,YOffset,DY);
  ParallelFor(0,NewImage.Height-1,@ProcessLine);

  if OverwriteSource then
  begin
    Image.TakeOver(NewImage);
    NewImage.Free;
  end;
end;

procedure IIRLowpassFilterFloat(Image: TFloatMap; Lookback: Double; Direction: TFilterDirection);
var
  LookBack1 : Double;

  procedure ProcessLineHorz(Y: Integer);
  var
    X : Integer;
    Last : Double;
    Pix : PFloat;
  begin
    with Image do
    begin
      Pix:=ScanLine[Y];
      Last:=Pix^;
      for X:=1 to Width do
      begin
        Last:=(Last*LookBack+Pix^)*LookBack1;
        Pix^:=Last;
        Inc(Pix);
      end;
      for X:=Width downto 1 do
      begin
        Dec(Pix);
        Last:=(Last*LookBack+Pix^)*LookBack1;
        Pix^:=Last;
      end;
    end;
  end;

  procedure ProcessLineVert(X: Integer);
  var
    Y : Integer;
    Last : Double;
    Pix : PFloat;
  begin
    with Image do
    begin
      Pix:=@Map^[X];
      Last:=Pix^;
      for Y:=1 to Height do
      begin
        Last:=(Last*LookBack+Pix^)*LookBack1;
        Pix^:=Last;
        Inc(Pix,Width);
      end;
      for Y:=Height downto 1 do
      begin
        Dec(Pix,Width);
        Last:=(Last*LookBack+Pix^)*LookBack1;
        Pix^:=Last;
      end;
    end;
  end;


begin
  if Lookback<=0 then Exit;
  if Direction=fdBoth then LookBack:=LookBack/4
  else LookBack:=LookBack/2;
  LookBack1:=1/(LookBack+1);
  with Image do
  begin
    if Direction in [fdBoth,fdHorizontal] then ParallelFor(0,Height-1,@ProcessLineHorz,16);
    if Direction in [fdBoth,fdVertical] then ParallelFor(0,Width-1,@ProcessLineVert,16);
  end;
end;

procedure IIRLowpassFilterFloat(Image: TFloatMap32; Lookback: Single; Direction: TFilterDirection);
var
  LookBack1 : Single;

  procedure ProcessLineHorz(Y: Integer);
  var
    X : Integer;
    Last : Single;
    Pix : PSingle;
  begin
    with Image do
    begin
      Pix:=ScanLine[Y];
      Last:=Pix^;
      for X:=1 to Width do
      begin
        Last:=(Last*LookBack+Pix^)*LookBack1;
        Pix^:=Last;
        Inc(Pix);
      end;
      for X:=Width downto 1 do
      begin
        Dec(Pix);
        Last:=(Last*LookBack+Pix^)*LookBack1;
        Pix^:=Last;
      end;
    end;
  end;

  procedure ProcessLineVert(X: Integer);
  var
    Y : Integer;
    Last : Single;
    Pix : PSingle;
  begin
    with Image do
    begin
      Pix:=@Map^[X];
      Last:=Pix^;
      for Y:=1 to Height do
      begin
        Last:=(Last*LookBack+Pix^)*LookBack1;
        Pix^:=Last;
        Inc(Pix,Width);
      end;
      for Y:=Height downto 1 do
      begin
        Dec(Pix,Width);
        Last:=(Last*LookBack+Pix^)*LookBack1;
        Pix^:=Last;
      end;
    end;
  end;

begin
  if Lookback<=0 then Exit;
  if Direction=fdBoth then LookBack:=LookBack/4
  else LookBack:=LookBack/2;
  LookBack1:=1/(LookBack+1);
  with Image do
  begin
    if Direction in [fdBoth,fdHorizontal] then ParallelFor(0,Height-1,@ProcessLineHorz,16);
    if Direction in [fdBoth,fdVertical] then ParallelFor(0,Width-1,@ProcessLineVert,16);
  end;
end;

//==============================================================================================================================

procedure IIRMaxFilterFloat(Image: TFloatMap32; Lookback: Single);
var
  X, Y, I : Integer;
  Last1, Last2, LookBack1 : Single;
  Pix1, Pix2 : PSingle;
begin
  if Lookback<=0 then Exit;
  LookBack:=LookBack/4;
  LookBack1:=1/(LookBack+1);
  for I:=1 to 2 do
  with Image do
  begin
    for Y:=0 to Height-1 do
    begin
      Pix1:=Pixel[0,Y];
      Pix2:=Pixel[Width-1,Y];
      Last1:=Pix1^;
      Last2:=Pix2^;
      for X:=1 to Width do
      begin
        Last1:=(Last1*LookBack+Pix1^)*LookBack1;
        if Last1>Pix1^ then Pix1^:=Last1
        else Last1:=Pix1^;
        Inc(Pix1);
        Last2:=(Last2*LookBack+Pix2^)*LookBack1;
        if Last2>Pix2^ then Pix2^:=Last2
        else Last2:=Pix2^;
        Dec(Pix2);
      end;
    end;
    for X:=0 to Image.Width-1 do
    begin
      Pix1:=Pixel[X,0];
      Pix2:=Pixel[X,Height-1];
      Last1:=Pix1^;
      Last2:=Pix2^;
      for Y:=1 to Height do
      begin
        Last1:=(Last1*LookBack+Pix1^)*LookBack1;
        if Last1>Pix1^ then Pix1^:=Last1
        else Last1:=Pix1^;
        Inc(Pix1,Width);
        Last2:=(Last2*LookBack+Pix2^)*LookBack1;
        if Last2>Pix2^ then Pix2^:=Last2
        else Last2:=Pix2^;
        Dec(Pix2,Width);
      end;
    end;
  end;
end;

//==============================================================================================================================

procedure IIRMinFilterFloat(Image: TFloatMap32; Lookback: Single);
var
  X, Y, I : Integer;
  Last1, Last2, LookBack1 : Single;
  Pix1, Pix2 : PSingle;
begin
  if Lookback<=0 then Exit;
  LookBack:=LookBack/4;
  LookBack1:=1/(LookBack+1);
  for I:=1 to 2 do
  with Image do
  begin
    for Y:=0 to Height-1 do
    begin
      Pix1:=Pixel[0,Y];
      Pix2:=Pixel[Width-1,Y];
      Last1:=Pix1^;
      Last2:=Pix2^;
      for X:=1 to Width do
      begin
        Last1:=(Last1*LookBack+Pix1^)*LookBack1;
        if Last1<Pix1^ then Pix1^:=Last1
        else Last1:=Pix1^;
        Inc(Pix1);
        Last2:=(Last2*LookBack+Pix2^)*LookBack1;
        if Last2<Pix2^ then Pix2^:=Last2
        else Last2:=Pix2^;
        Dec(Pix2);
      end;
    end;
    for X:=0 to Image.Width-1 do
    begin
      Pix1:=Pixel[X,0];
      Pix2:=Pixel[X,Height-1];
      Last1:=Pix1^;
      Last2:=Pix2^;
      for Y:=1 to Height do
      begin
        Last1:=(Last1*LookBack+Pix1^)*LookBack1;
        if Last1<Pix1^ then Pix1^:=Last1
        else Last1:=Pix1^;
        Inc(Pix1,Width);
        Last2:=(Last2*LookBack+Pix2^)*LookBack1;
        if Last2<Pix2^ then Pix2^:=Last2
        else Last2:=Pix2^;
        Dec(Pix2,Width);
      end;
    end;
  end;
end;

// EdgePlane must be 0 at edges and 1 at smooth areas
procedure IIRMinFilterFloatEdge(Image,EdgePlane: TFloatMap32; Lookback: Single);
var
  X, Y, I : Integer;
  Last1, Last2, LocalLookBack : Single;
  Pix1, Pix2 : PSingle;
  EdgePix1, EdgePix2 : PSingle;
begin
  Assert(Image.Size=EdgePlane.Size);
  if Lookback<=0 then Exit;
  LookBack:=LookBack/(4*5);
  for I:=1 to 8 do // Run many weak iterations with edge filter to avoid unwanted artifacts
  with Image do
  begin
    for Y:=0 to Height-1 do
    begin
      Pix1:=Pixel[0,Y];
      Pix2:=Pixel[Width-1,Y];
      Last1:=Pix1^;
      Last2:=Pix2^;
      EdgePix1:=EdgePlane.Pixel[0,Y];
      EdgePix2:=EdgePlane.Pixel[Width-1,Y];
      for X:=1 to Width do
      begin
        LocalLookBack:=LookBack*EdgePix1^;
        Last1:=(Last1*LocalLookBack+Pix1^)/(LocalLookBack+1);
        if Last1<Pix1^ then Pix1^:=Last1
        else Last1:=Pix1^;
        Inc(Pix1);
        Inc(EdgePix1);
        LocalLookBack:=LookBack*EdgePix2^;
        Last2:=(Last2*LocalLookBack+Pix2^)/(LocalLookBack+1);
        if Last2<Pix2^ then Pix2^:=Last2
        else Last2:=Pix2^;
        Dec(Pix2);
        Dec(EdgePix2);
      end;
    end;
    for X:=0 to Image.Width-1 do
    begin
      Pix1:=Pixel[X,0];
      Pix2:=Pixel[X,Height-1];
      Last1:=Pix1^;
      Last2:=Pix2^;
      EdgePix1:=EdgePlane.Pixel[X,0];
      EdgePix2:=EdgePlane.Pixel[X,Height-1];
      for Y:=1 to Height do
      begin
        LocalLookBack:=LookBack*EdgePix1^;
        Last1:=(Last1*LocalLookBack+Pix1^)/(LocalLookBack+1);
        if Last1<Pix1^ then Pix1^:=Last1
        else Last1:=Pix1^;
        Inc(Pix1,Width);
        Inc(EdgePix1,Width);
        LocalLookBack:=LookBack*EdgePix2^;
        Last2:=(Last2*LocalLookBack+Pix2^)/(LocalLookBack+1);
        if Last2<Pix2^ then Pix2^:=Last2
        else Last2:=Pix2^;
        Dec(Pix2,Width);
        Dec(EdgePix2,Width);
      end;
    end;
  end;
end;

//==============================================================================================================================

// EdgePlane must be 0 at edges and 1 at smooth areas
procedure IIRMaxFilterFloatEdge(Image,EdgePlane: TFloatMap32; Lookback: Single);
var
  X, Y, I : Integer;
  Last1, Last2, LocalLookBack : Single;
  Pix1, Pix2 : PSingle;
  EdgePix1, EdgePix2 : PSingle;
begin
  Assert(Image.Size=EdgePlane.Size);
  if Lookback<=0 then Exit;
  LookBack:=LookBack/(4*5);
  for I:=1 to 8 do // Run many weak iterations with edge filter to avoid unwanted artifacts
  with Image do
  begin
    for Y:=0 to Height-1 do
    begin
      Pix1:=Pixel[0,Y];
      Pix2:=Pixel[Width-1,Y];
      Last1:=Pix1^;
      Last2:=Pix2^;
      EdgePix1:=EdgePlane.Pixel[0,Y];
      EdgePix2:=EdgePlane.Pixel[Width-1,Y];
      for X:=1 to Width do
      begin
        LocalLookBack:=LookBack*EdgePix1^;
        Last1:=(Last1*LocalLookBack+Pix1^)/(LocalLookBack+1);
        if Last1>Pix1^ then Pix1^:=Last1
        else Last1:=Pix1^;
        Inc(Pix1);
        Inc(EdgePix1);
        LocalLookBack:=LookBack*EdgePix2^;
        Last2:=(Last2*LocalLookBack+Pix2^)/(LocalLookBack+1);
        if Last2>Pix2^ then Pix2^:=Last2
        else Last2:=Pix2^;
        Dec(Pix2);
        Dec(EdgePix2);
      end;
    end;
    for X:=0 to Image.Width-1 do
    begin
      Pix1:=Pixel[X,0];
      Pix2:=Pixel[X,Height-1];
      Last1:=Pix1^;
      Last2:=Pix2^;
      EdgePix1:=EdgePlane.Pixel[X,0];
      EdgePix2:=EdgePlane.Pixel[X,Height-1];
      for Y:=1 to Height do
      begin
        LocalLookBack:=LookBack*EdgePix1^;
        Last1:=(Last1*LocalLookBack+Pix1^)/(LocalLookBack+1);
        if Last1>Pix1^ then Pix1^:=Last1
        else Last1:=Pix1^;
        Inc(Pix1,Width);
        Inc(EdgePix1,Width);
        LocalLookBack:=LookBack*EdgePix2^;
        Last2:=(Last2*LocalLookBack+Pix2^)/(LocalLookBack+1);
        if Last2>Pix2^ then Pix2^:=Last2
        else Last2:=Pix2^;
        Dec(Pix2,Width);
        Dec(EdgePix2,Width);
      end;
    end;
  end;
end;

//==============================================================================================================================

procedure IIRHighpassFilterFloat(Image: TMatrix; Lookback: Double; Direction: TFilterDirection; HorzResult,VertResult: TFloatMap);
var
  X, Y : Integer;
  RunAvg, LookBack1 : Double;
  LocalResult : Boolean;
  Pix, NewPix : PFloat;
begin
  LocalResult:=(HorzResult=nil) and (VertResult=nil);
  if LocalResult then
  begin
    if Direction in [fdBoth,fdHorizontal] then HorzResult:=TFloatMap.Create;
    if Direction in [fdBoth,fdVertical] then VertResult:=TFloatMap.Create;
  end;

  if Direction in [fdBoth,fdHorizontal] then HorzResult.New(Image.Width,Image.Height);
  if Direction in [fdBoth,fdVertical] then VertResult.New(Image.Width,Image.Height);
  if Lookback<=0 then
  begin
    if Direction in [fdBoth,fdHorizontal] then HorzResult.Clear;
    if Direction in [fdBoth,fdVertical] then VertResult.Clear;
  end
  else
  with Image do
  begin
    LookBack:=LookBack/4;
    LookBack1:=1/(LookBack+1);
    if Direction in [fdBoth,fdHorizontal] then
      for Y:=0 to Height-1 do
      begin
        Pix:=ScanLine[Y];
        NewPix:=HorzResult.ScanLine[Y];
        RunAvg:=Pix^;
        for X:=1 to Width do
        begin
          RunAvg:=(RunAvg*LookBack+Pix^)*LookBack1;
          NewPix^:=Pix^-RunAvg;
          Inc(Pix); Inc(NewPix);
        end;
        Dec(NewPix);
        RunAvg:=NewPix^;
        for X:=Width downto 1 do
        begin
          RunAvg:=(RunAvg*LookBack+NewPix^)*LookBack1;
          NewPix^:=NewPix^-RunAvg;
          Dec(NewPix);
        end;
      end;
    if Direction in [fdBoth,fdVertical] then
      for X:=0 to Image.Width-1 do
      begin
        Pix:=@Map^[X];
        NewPix:=@VertResult.Map^[X];
        RunAvg:=Pix^;
        for Y:=1 to Height do
        begin
          RunAvg:=(RunAvg*LookBack+Pix^)*LookBack1;
          NewPix^:=Pix^-RunAvg;
          Inc(Pix,Width);
          Inc(NewPix,Width);
        end;
        Dec(NewPix,Width);
        RunAvg:=NewPix^;
        for Y:=Height downto 1 do
        begin
          RunAvg:=(RunAvg*LookBack+NewPix^)*LookBack1;
          NewPix^:=NewPix^-RunAvg;
          Dec(NewPix,Width);
        end;
      end;
  end;
  if LocalResult then
  begin
    if Direction=fdHorizontal then Image.TakeOver(HorzResult)
    else if Direction=fdVertical then Image.TakeOver(VertResult)
    else
    begin
      Image.TakeOver(HorzResult);
      Image.Add(VertResult);
    end;
    HorzResult.Free;
    VertResult.Free;
  end;
end;

//==============================================================================================================================

procedure IIREdgeDetectFilterFloat(Image: TMatrix; Lookback: Double; Direction: TFilterDirection; HorzResult,VertResult: TFloatMap);
var
  X, Y : Integer;
  Last, LookBack1 : Double;
  Pix, EdgePix : PFloat;
  LineBack : TFloatDynArray;
  LocalResult : Boolean;
begin
  LocalResult:=(HorzResult=nil) and (VertResult=nil);
  if LocalResult then
  begin
    if Direction in [fdBoth,fdHorizontal] then HorzResult:=TFloatMap.Create;
    if Direction in [fdBoth,fdVertical] then VertResult:=TFloatMap.Create;
  end;

  if Direction in [fdBoth,fdHorizontal] then HorzResult.New(Image.Width,Image.Height);
  if Direction in [fdBoth,fdVertical] then VertResult.New(Image.Width,Image.Height);
  if Lookback<=0 then
  begin
    if Direction in [fdBoth,fdHorizontal] then HorzResult.Clear;
    if Direction in [fdBoth,fdVertical] then VertResult.Clear;
  end
  else with Image do
  begin
    LookBack1:=1/(LookBack+1);
    if Direction in [fdBoth,fdHorizontal] then // Horizontal
    begin
      SetLength(LineBack,Width);
      for Y:=0 to Height-1 do
      begin
        Pix:=Pixel[Width-1,Y];
        Last:=Pix^;
        for X:=Width-1 downto 0 do // Scan back (right) and store result in LineBack
        begin
          Last:=(Last*LookBack+Pix^)*LookBack1;
          LineBack[X]:=Last;
          Dec(Pix);
        end;
        Inc(Pix);
        Last:=Pix^;
        EdgePix:=HorzResult.Pixel[0,Y];
        for X:=0 to Width-1 do // Scan forward (left) and compute edge values as difference between forward and back smoothing
        begin
          Last:=(Last*LookBack+Pix^)*LookBack1;
          EdgePix^:=Abs(Last-LineBack[X]);
          Inc(Pix);
          Inc(EdgePix);
        end;
      end;
    end;
    if Direction in [fdBoth,fdVertical] then // Vertical
    begin
      SetLength(LineBack,Height);
      for X:=0 to Image.Width-1 do
      begin
        Pix:=Pixel[X,Height-1];
        Last:=Pix^;
        for Y:=Height-1 downto 0 do // Scan back (up) and store result in LineBack
        begin
          Last:=(Last*LookBack+Pix^)*LookBack1;
          LineBack[Y]:=Last;
          Dec(Pix,Width);
        end;
        Inc(Pix,Width);
        Last:=Pix^;
        EdgePix:=VertResult.Pixel[X,0];
        for Y:=0 to Height-1 do // Scan forward (down) and compute edge values as difference between forward and back smoothing
        begin
          Last:=(Last*LookBack+Pix^)*LookBack1;
          EdgePix^:=Abs(Last-LineBack[Y]);
          Inc(Pix,Width);
          Inc(EdgePix,Width);
        end;
      end;
    end;
  end;
  if LocalResult then
  begin
    if Direction=fdHorizontal then Image.TakeOver(HorzResult)
    else if Direction=fdVertical then Image.TakeOver(VertResult)
    else
    begin
      Image.TakeOver(HorzResult);
      Image.Add(VertResult);
    end;
    HorzResult.Free;
    VertResult.Free;
  end;
end;                   

procedure IIREdgeDetectFilterFloat(Image: TFloatMap32; Lookback: Single; Direction: TFilterDirection; HorzResult,VertResult: TFloatMap32);
var
  X, Y : Integer;
  Last, LookBack1 : Single;
  Pix, EdgePix : PSingle;
  LineBack : TSingleDynArray;
  LocalResult : Boolean;
begin
  LocalResult:=(HorzResult=nil) and (VertResult=nil);
  if LocalResult then
  begin
    if Direction in [fdBoth,fdHorizontal] then HorzResult:=TFloatMap32.Create;
    if Direction in [fdBoth,fdVertical] then VertResult:=TFloatMap32.Create;
  end;

  if Direction in [fdBoth,fdHorizontal] then HorzResult.New(Image.Width,Image.Height);
  if Direction in [fdBoth,fdVertical] then VertResult.New(Image.Width,Image.Height);
  if Lookback<=0 then
  begin
    if Direction in [fdBoth,fdHorizontal] then HorzResult.Clear;
    if Direction in [fdBoth,fdVertical] then VertResult.Clear;
  end
  else with Image do
  begin
    LookBack1:=1/(LookBack+1);
    if Direction in [fdBoth,fdHorizontal] then // Horizontal
    begin
      SetLength(LineBack,Width);
      for Y:=0 to Height-1 do
      begin
        Pix:=Pixel[Width-1,Y];
        Last:=Pix^;
        for X:=Width-1 downto 0 do // Scan back (right) and store result in LineBack
        begin
          Last:=(Last*LookBack+Pix^)*LookBack1;
          LineBack[X]:=Last;
          Dec(Pix);
        end;
        Inc(Pix);
        Last:=Pix^;
        EdgePix:=HorzResult.Pixel[0,Y];
        for X:=0 to Width-1 do // Scan forward (left) and compute edge values as difference between forward and back smoothing
        begin
          Last:=(Last*LookBack+Pix^)*LookBack1;
          EdgePix^:=Abs(Last-LineBack[X]);
          Inc(Pix);
          Inc(EdgePix);
        end;
      end;
    end;
    if Direction in [fdBoth,fdVertical] then // Vertical
    begin
      SetLength(LineBack,Height);
      for X:=0 to Image.Width-1 do
      begin
        Pix:=Pixel[X,Height-1];
        Last:=Pix^;
        for Y:=Height-1 downto 0 do // Scan back (up) and store result in LineBack
        begin
          Last:=(Last*LookBack+Pix^)*LookBack1;
          LineBack[Y]:=Last;
          Dec(Pix,Width);
        end;
        Inc(Pix,Width);
        Last:=Pix^;
        EdgePix:=VertResult.Pixel[X,0];
        for Y:=0 to Height-1 do // Scan forward (down) and compute edge values as difference between forward and back smoothing
        begin
          Last:=(Last*LookBack+Pix^)*LookBack1;
          EdgePix^:=Abs(Last-LineBack[Y]);
          Inc(Pix,Width);
          Inc(EdgePix,Width);
        end;
      end;
    end;
  end;
  if LocalResult then
  begin
    if Direction=fdHorizontal then Image.TakeOver(HorzResult)
    else if Direction=fdVertical then Image.TakeOver(VertResult)
    else
    begin
      Image.TakeOver(HorzResult);
      Image.Add(VertResult);
    end;
    HorzResult.Free;
    VertResult.Free;
  end;
end;                   

//==============================================================================================================================

procedure CreateMapFromExpression(Image: TFloatMap; const Expression: string; Defines: TSymbolTable);
var
  XSymbol, YSymbol : TSymbolValue;
  X, Y : Integer;
  Pix : PFloat;
begin
  Val(Expression,X,Y);
  if Y=0 then Image.Clear(X)
  else
  begin
    if Defines=nil then Defines:=DefaultDefines;
    Image.Clear;
    Pix:=@Image.Map^[0];
    XSymbol:=TSymbolValue(Defines.Define('x',TSymbolValue.Create));
    YSymbol:=TSymbolValue(Defines.Define('y',TSymbolValue.Create));
    try
      for Y:=0 to Image.Height-1 do
      begin
        YSymbol.Value:=Y;
        for X:=0 to Image.Width-1 do
        begin
          XSymbol.Value:=X;
          Pix^:=EvaluateExpression(Expression,Defines);
          Inc(Pix);
        end;
        if Assigned(ProgressUpdate) then ProgressUpdate(Y*100 div Image.Height);
      end;
    finally
      Defines.Remove('x');
      Defines.Remove('y');
    end;
  end;
  if Assigned(ProgressUpdate) then ProgressUpdate(100);
end;

type
  TGetPixelFunction = class(TSymbolicFunction)
    public
      Image : TFloatMap;
      constructor Create(Image: TFloatMap); reintroduce;
      function Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat; override;
      function VarName: string; override;
      function Help: string; override;
    end;

constructor TGetPixelFunction.Create(Image: TFloatMap);
begin
  inherited Create;
  Self.Image:=Image;
  SymbolType:=stSymbolicFunction;
end;

function TGetPixelFunction.Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat;
var
  Start, Level : Integer;
  XExpr, YExpr : string;
  X, Y : Double;
  XInt, YInt : Integer;
  XFrac, YFrac : Double;
begin
  // Extract X expression
  Start:=Pos;
  Level:=0;
  while (Pos<=Length(Expression)) and ((Level<>0) or (Expression[Pos]<>',')) do
  begin
    if Expression[Pos]='(' then Inc(Level)
    else if Expression[Pos]=')' then
    begin
      Dec(Level);
      if Level<0 then raise EExpressionError.Create(Format(rsSExpected,[',']),Pos);
    end;
    Inc(Pos);
  end;
  if (Pos>Length(Expression)) then raise EExpressionError.Create(Format(rsSExpected,[',']),Pos);
  XExpr:=Copy(Expression,Start,Pos-Start);
  Inc(Pos);
  // Extract Y expression
  Start:=Pos;
  while (Pos<=Length(Expression)) and ((Level<>0) or (Expression[Pos]<>')')) do
  begin
    if Expression[Pos]='(' then Inc(Level)
    else if Expression[Pos]=')' then Dec(Level);
    Inc(Pos);
  end;
  if (Pos>Length(Expression)) then raise EExpressionError.Create(Format(rsSExpected,[',']),Pos);
  YExpr:=Copy(Expression,Start,Pos-Start);
  // Look up pixel using bilinear interpolation
  X:=EvaluateExpression(XExpr,Table);
  Y:=EvaluateExpression(YExpr,Table);
  XInt:=Floor(X); XFrac:=X-XInt;
  YInt:=Floor(Y); YFrac:=Y-YInt;
  Result:=Image.Extrapolate(XInt,YInt)*(1-XFrac)*(1-YFrac)+
          Image.Extrapolate(XInt+1,YInt)*(XFrac)*(1-YFrac)+
          Image.Extrapolate(XInt,YInt+1)*(1-XFrac)*(YFrac)+
          Image.Extrapolate(XInt+1,YInt+1)*(XFrac)*(YFrac);
end;

function TGetPixelFunction.Help: string;
begin
  Result:='';
end;

function TGetPixelFunction.VarName: string;
begin
  Result:='x,y';
end;


type
  TUpdateMapWithExpressionProcess = class(TMultiCoreProcess)
    protected
      Source, Dest : TFloatMap;
      Expression: string;
      GlobalDefines: TSymbolTable;
      GetPixelFunction : TGetPixelFunction;
      procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
    end;

procedure TUpdateMapWithExpressionProcess.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  XSymbol, YSymbol, VSymbol : TSymbolValue;
  X, Y : Integer;
  Pix : PFloat;
  Defines : TSymbolTable;
  ThreadData : PThreadDataStartStop;
begin
  Defines:=TSymbolTable.Create(False);
  try
    Defines.Assign(GlobalDefines);
    Defines.Define('Width',TSymbolValue.Create(Dest.Width));
    Defines.Define('Height',TSymbolValue.Create(Dest.Height));
    XSymbol:=TSymbolValue(Defines.Define('x',TSymbolValue.Create));
    YSymbol:=TSymbolValue(Defines.Define('y',TSymbolValue.Create));
    VSymbol:=TSymbolValue(Defines.Define('p',TSymbolValue.Create));
    try
      ThreadData:=Data;
      for Y:=ThreadData.Start to ThreadData.Stop do
      begin
        Pix:=Dest.ScanLine[Y];
        YSymbol.Value:=Y;
        for X:=0 to Dest.Width-1 do
        begin
          XSymbol.Value:=X;
          VSymbol.Value:=Pix^;
          Pix^:=EvaluateExpression(Expression,Defines);
          Inc(Pix);
        end;
        if (Thread=nil) and Assigned(ProgressUpdate) then ProgressUpdate(Y*100 div (ThreadData.Stop+1))
        else if ProcessAborted then Exit;
      end;
    except
      on E: EExpressionError do
      begin
        Insert('][',Expression,E.ErrorPosition+1);
        raise Exception.Create(E.Message+' "'+Expression+'"');
      end;
    end;
  finally
    for X:=0 to Defines.Count-1 do
      if GlobalDefines.IndexOfObject(Defines.Objects[X])>=0 then Defines.Objects[X]:=nil;
    Defines.Free;
  end;
end;

// (not thread safe - be careful about Defines)
procedure UpdateMapWithExpression(Image: TFloatMap; Expression: string; Defines: TSymbolTable; Source: TFloatMap);
var
  X, Y : Integer;
  Process : TUpdateMapWithExpressionProcess;
begin
  Val(Expression,X,Y);
  if Y=0 then Image.Clear(X)
  else
  begin
    if Source=nil then Source:=Image;
    if Defines=nil then Defines:=DefaultDefines;
    Process:=TUpdateMapWithExpressionProcess.Create;
    try
      Process.GetPixelFunction:=TGetPixelFunction(Defines.Define('P',TGetPixelFunction.Create(TFloatMap.Create(Source))));
      Process.Source:=Source;
      Process.Dest:=Image;
      Process.GlobalDefines:=Defines;
      Process.Expression:=Expression;
      Process.ExecuteStartStop(0,Image.Height-1,16,Process.NumberOfCores);
    finally
      Process.GetPixelFunction.Image.Free;
      Process.Free;
    end;
  end;
  if Assigned(ProgressUpdate) then ProgressUpdate(100);
end;

//==============================================================================================================================

function CreateGaussianKernel(const Variance,Precision: Double): TMatrix;
var
  HalfFilterSize : Integer;
  X, Y : Integer;
  Pix : PFloat;
  InvSpread : Double;
begin
  HalfFilterSize:=Ceil(Sqrt(-2*Variance*Ln(Precision))); // Auto determine matrix size
  InvSpread:=-0.5/Variance;
  Result:=TMatrix.Create(HalfFilterSize*2+1,HalfFilterSize*2+1);
  try
    Pix:=@Result.Map^[0];
    for Y:=-HalfFilterSize to HalfFilterSize do
      for X:=-HalfFilterSize to HalfFilterSize do
      begin
        Pix^:=Exp((Sqr(X)+Sqr(Y))*InvSpread);
        Inc(Pix);
      end;
    Result.Normalize;
  except
    Result.Free;
    raise;
  end;
end;

//==============================================================================================================================

procedure ConvertToGrayScaleFloat(Image: TLinarBitmap; DestPlane: TFloatMap32);
var
  Pix : ^RGBRec;
  GrayPix : PSingle;
  P : Integer;
begin
  if (Image.PixelFormat=pf8bit) and Image.IsGrayScale then
    DestPlane.Assign(Image)
  else if Image.PixelFormat=pf24bit then
  begin
    DestPlane.New(Image.Width,Image.Height);
    Pointer(Pix):=Image.Map;
    Pointer(GrayPix):=DestPlane.Map;
    for P:=1 to DestPlane.Size do
    begin
      GrayPix^:=Pix^.R*0.299+
                Pix^.G*0.587+
                Pix^.B*0.114;
      Inc(Pix);
      Inc(GrayPix);
    end;
  end
  else Assert(False,rsInvalidPixelFormat);
end;

procedure SplitColorPlanesFloat(Image: TLinarBitmap; Plane1,Plane2,Plane3: TFloatMap; const T: TMatrix4x4);
var
  Pix : ^RGBRec;
  Pix1, Pix2, Pix3 : PFloat;
  P : Integer;
begin
  Plane1.New(Image.Width,Image.Height);
  Plane2.New(Image.Width,Image.Height);
  Plane3.New(Image.Width,Image.Height);
  Pointer(Pix):=Image.Map;
  Pointer(Pix1):=Plane1.Map;
  Pointer(Pix2):=Plane2.Map;
  Pointer(Pix3):=Plane3.Map;
  for P:=1 to Plane1.Size do
  begin
    ColorTransform(Pix^.R,Pix^.G,Pix^.B,Pix1^,Pix2^,Pix3^,T);
    Inc(Pix);
    Inc(Pix1); Inc(Pix2); Inc(Pix3);
  end;
end;

procedure SplitColorPlanesFloat(Image: TLinarBitmap; Plane1,Plane2,Plane3: TFloatMap32; const T: TMatrix4x4);

  procedure ProcessLine(Y: Integer);
  var
    Pix : ^RGBRec;
    Pix1, Pix2, Pix3 : PSingle;
    X : Integer;
  begin
    Pointer(Pix):=Image.ScanLine[Y];
    Pointer(Pix1):=Plane1.ScanLine[Y];
    Pointer(Pix2):=Plane2.ScanLine[Y];
    Pointer(Pix3):=Plane3.ScanLine[Y];
    for X:=1 to Image.Width do
    begin
      ColorTransform(Pix^.R,Pix^.G,Pix^.B,Pix1^,Pix2^,Pix3^,T);
      Inc(Pix);
      Inc(Pix1); Inc(Pix2); Inc(Pix3);
    end;
  end;

begin
  Assert(Image.PixelFormat=pf24bit);
  Plane1.New(Image.Width,Image.Height);
  Plane2.New(Image.Width,Image.Height);
  Plane3.New(Image.Width,Image.Height);
  ParallelFor(0,Image.Height-1,@ProcessLine);
end;

procedure CombineColorPlanesFloat(Plane1,Plane2,Plane3: TFloatMap; Image: TLinarBitmap; const T: TMatrix4x4);

  procedure ProcessLine(Y: Integer);
  var
    Pix : ^RGBRec;
    Pix1, Pix2, Pix3 : PFloat;
    X : Integer;
  begin
    Pointer(Pix):=Image.ScanLine[Y];
    Pointer(Pix1):=Plane1.ScanLine[Y];
    Pointer(Pix2):=Plane2.ScanLine[Y];
    Pointer(Pix3):=Plane3.ScanLine[Y];
    for X:=1 to Image.Width do
    begin
      ColorTransform(Pix1^,Pix2^,Pix3^,Pix^.R,Pix^.G,Pix^.B,T);
      Inc(Pix);
      Inc(Pix1); Inc(Pix2); Inc(Pix3);
    end;
  end;

begin
  Image.New(Plane1.Width,Plane1.Height,pf24bit);
  ParallelFor(0,Image.Height-1,@ProcessLine);
end;

procedure CombineColorPlanesFloat(Plane1,Plane2,Plane3: TFloatMap32; Image: TLinarBitmap; const T: TMatrix4x4);

  procedure ProcessLine(Y: Integer);
  var
    Pix : ^RGBRec;
    Pix1, Pix2, Pix3 : PSingle;
    X : Integer;
  begin
    Pointer(Pix):=Image.ScanLine[Y];
    Pointer(Pix1):=Plane1.ScanLine[Y];
    Pointer(Pix2):=Plane2.ScanLine[Y];
    Pointer(Pix3):=Plane3.ScanLine[Y];
    for X:=1 to Image.Width do
    begin
      ColorTransform(Pix1^,Pix2^,Pix3^,Pix^.R,Pix^.G,Pix^.B,T);
      Inc(Pix);
      Inc(Pix1); Inc(Pix2); Inc(Pix3);
    end;
  end;

begin
  Image.New(Plane1.Width,Plane1.Height,pf24bit);
  ParallelFor(0,Image.Height-1,@ProcessLine);
end;

procedure ConvertColorSpace(SrcPlane1,SrcPlane2,SrcPlane3,DestPlane1,DestPlane2,DestPlane3: TFloatMap; const T: TMatrix4x4); overload;
var
  SrcPix1, SrcPix2, SrcPix3 : PFloat;
  DestPix1, DestPix2, DestPix3 : PFloat;
  P : Integer;
begin
  DestPlane1.New(SrcPlane1.Width,SrcPlane1.Height);
  DestPlane2.New(SrcPlane1.Width,SrcPlane1.Height);
  DestPlane3.New(SrcPlane1.Width,SrcPlane1.Height);
  Pointer(SrcPix1):=SrcPlane1.Map;
  Pointer(SrcPix2):=SrcPlane2.Map;
  Pointer(SrcPix3):=SrcPlane3.Map;
  Pointer(DestPix1):=DestPlane1.Map;
  Pointer(DestPix2):=DestPlane2.Map;
  Pointer(DestPix3):=DestPlane3.Map;
  for P:=1 to SrcPlane1.Size do
  begin
    ColorTransform(SrcPix1^,SrcPix2^,SrcPix3^,DestPix1^,DestPix2^,DestPix3^,T);
    Inc(SrcPix1); Inc(SrcPix2); Inc(SrcPix3);
    Inc(DestPix1); Inc(DestPix2); Inc(DestPix3);
  end;
end;

procedure ConvertColorSpace(SrcPlane1,SrcPlane2,SrcPlane3,DestPlane1,DestPlane2,DestPlane3: TFloatMap32; const T: TMatrix4x4); overload;
var
  SrcPix1, SrcPix2, SrcPix3 : PSingle;
  DestPix1, DestPix2, DestPix3 : PSingle;
  P : Integer;
begin
  DestPlane1.New(SrcPlane1.Width,SrcPlane1.Height);
  DestPlane2.New(SrcPlane1.Width,SrcPlane1.Height);
  DestPlane3.New(SrcPlane1.Width,SrcPlane1.Height);
  Pointer(SrcPix1):=SrcPlane1.Map;
  Pointer(SrcPix2):=SrcPlane2.Map;
  Pointer(SrcPix3):=SrcPlane3.Map;
  Pointer(DestPix1):=DestPlane1.Map;
  Pointer(DestPix2):=DestPlane2.Map;
  Pointer(DestPix3):=DestPlane3.Map;
  for P:=1 to SrcPlane1.Size do
  begin
    ColorTransform(SrcPix1^,SrcPix2^,SrcPix3^,DestPix1^,DestPix2^,DestPix3^,T);
    Inc(SrcPix1); Inc(SrcPix2); Inc(SrcPix3);
    Inc(DestPix1); Inc(DestPix2); Inc(DestPix3);
  end;
end;

procedure RotateFloatMap270(Image: TFloatMap);
var
  Original : TFloatMap;
begin
  Original:=TFloatMap.Create;
  try
    Original.TakeOver(Image);
    Image.New(Original.Height,Original.Width);
    UpdateMapWithExpression(Image,'P(y,'+IntToStr(Image.Width-1)+'-x)',nil,Original);
  finally
    Original.Free;
  end;
end;

procedure FlipFloatMap(Image: TFloatMap);
begin
  UpdateMapWithExpression(Image,'P(x,'+IntToStr(Image.Height-1)+'-y)')
end;

procedure MirrorFloatMap(Image: TFloatMap);
begin
  UpdateMapWithExpression(Image,'P('+IntToStr(Image.Width-1)+'-x,y)')
end;

end.

