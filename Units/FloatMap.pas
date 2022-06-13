////////////////////////////////////////////////////////////////////////////////
//
// FloatMap.pas - Floating point and complex number bitmap classes
// ---------------------------------------------------------------
// Version:   2004-04-10
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Last changes:
//
unit FloatMap;

interface

uses Windows, SysUtils, Graphics, StreamUtils, Complex, LinarBitmap,
  MemUtils, Math, MathUtils, Consts, Types,
  SysConst, MultiCoreProcessing, Streams;

{$IFDEF EnableInlining}
{$INLINE AUTO}
{$ENDIF}

type
  TBaseFloatMap = class(TStreamClass)
    protected
      FByteSize : Integer;
      FWidth, FHeight : Integer;
      FBytesPerLine : Integer;
      FSize : LongInt; // Number of pixels
    public
      StreamFormat : (smMAP,smMatlab,smText);
      property Width: Integer read fWidth;
      property Height: Integer read fHeight;
      property BytesPerLine: Integer read fBytesPerLine;
      property ByteSize: Integer read FByteSize;
      // Number of elements
      property Size: Integer read fSize;

      constructor Create(Other: TObject); overload;
      destructor Destroy; override;
      procedure Dispose; virtual;
   end;

  TMappingMethod = (mmLinear,mmSqrt,mm16bitLinear,mmLog);

  TFloatMap = class(TBaseFloatMap)
    protected
      function GetScanLine(Y: Integer): Pointer; {$IFDEF EnableInlining} inline; {$ENDIF}
      function GetPixel(X,Y: Integer): PFloat; {$IFDEF EnableInlining} inline; {$ENDIF}
    public
      Map : PFloatArray;

      property ScanLine[Y: Integer]: Pointer read GetScanLine;
      property Pixel[X,Y: Integer]: PFloat read GetPixel; default;
      // Get element value or extrapolate by repeating edge pixels if X,Y is outside
      function Extrapolate(X,Y: Integer): Float;
      function Interpolate(X,Y: Single): Float;

      constructor Create(X,Y: Integer); overload;

      // Create new image
      procedure New(X,Y: Integer);
      procedure TakeOver(Other: TFloatMap);
      procedure Assign(Other: TObject); override;
      procedure AssignTo(Other: TObject); override;
      procedure ResizeCanvas(XSiz,YSiz: Integer; XPos: Integer=0; YPos: Integer=0; Background: Float=0);
      procedure Paste(Source: TFloatMap; X,Y: Integer);

      // Release image memory
      procedure Dispose; override;
      procedure DeleteLine(Y: Integer);

      // Clear map
      procedure Clear(Value: Float); overload;
      procedure Clear; overload;

      procedure GetStat(out Min,Max: Float);

      // Convert to TLinearBitmap
      procedure MakeBitmap(Image: TLinearBitmap; Method: TMappingMethod=mmLinear; const Min: Float=1.0e10; const Max: Float=-1.0e10);
    end;

  TFloatMap32 = class(TBaseFloatMap)
    protected
      function GetScanLine(Y: Integer): Pointer; {$IFDEF EnableInlining} inline; {$ENDIF}
      function GetPixel(X,Y: Integer): PSingle; {$IFDEF EnableInlining} inline; {$ENDIF}
    public
      Map : PSingleArray;

      property ScanLine[Y: Integer]: Pointer read GetScanLine;
      property Pixel[X,Y: Integer]: PSingle read GetPixel; default;
      function GetPixelInterpolate(X,Y: Single): Single;

      constructor Create(X,Y: Integer); overload;
      // Create new image
      procedure New(X,Y: Integer);
      // Release image memory
      procedure Dispose; override;

      procedure TakeOver(Other: TFloatMap32);
      procedure Assign(Other: TObject); override;
      procedure AssignTo(Other: TObject); override;
      procedure MakeBitmap(Image: TLinearBitmap; Min,Max: Single);

      procedure Clear;
      procedure Multiply(Value: Single);
      procedure SubtractFrom(Value: Single);
      procedure Add(Other: TFloatMap32); overload;
      procedure Add(Value: Single); overload;
      procedure Min(Other: TFloatMap32); overload;
      procedure Max(Other: TFloatMap32); overload;
      procedure EnsureRange(Min,Max: Single);
      procedure Transpose;

      function Min: Single; overload;
      function Max: Single; overload;
      function Mean: Single;
    end;
      
var
  cfImageAnalyzerMap : Word = 0;

implementation

//==============================================================================================================================
// TBaseFloatMap
//==============================================================================================================================

constructor TBaseFloatMap.Create(Other: TObject);
begin
  Create;
  Assign(Other);
end;

destructor TBaseFloatMap.Destroy;
begin
  Dispose;
  inherited Destroy;
end;

procedure TBaseFloatMap.Dispose;
begin
  FByteSize:=0;
  FWidth:=0;
  FHeight:=0;
  FBytesPerLine:=0;
  FSize:=0;
end;

//==============================================================================================================================
// TFloatMap32
//==============================================================================================================================

constructor TFloatMap32.Create(X, Y: Integer);
begin
  Create;
  New(X,Y);
end;

procedure TFloatMap32.Dispose;
begin
  inherited Dispose;
  FreeAndNilData(Map);
end;

procedure TFloatMap32.TakeOver(Other: TFloatMap32);
begin
  FreeAndNilData(Map);
  Map:=Other.Map;
  Other.Map:=nil;
  FWidth:=Other.Width;
  FHeight:=Other.Height;
  FSize:=Other.Size;
  FBytesPerLine:=Other.BytesPerLine;
  FByteSize:=Other.ByteSize;
  Other.Dispose;
end;

procedure TFloatMap32.Assign(Other: TObject);
var
  I : Integer;
  Pix : PByte;
begin
  if Other is TFloatMap then
  begin
    New(TBaseFloatMap(Other).Width,TBaseFloatMap(Other).Height);
    for I:=0 to Size-1 do
      Map^[I]:=TFloatMap32(Other).Map^[I];
  end
  else if Other is TFloatMap32 then
  begin
    New(TBaseFloatMap(Other).Width,TBaseFloatMap(Other).Height);
    Move(TFloatMap(Other).Map^,Map^,ByteSize);
  end
  else if Other is TLinearBitmap then
  begin
    Assert(TLinearBitmap(Other).PixelFormat=pf8bit);
    New(TLinearBitmap(Other).Width,TLinearBitmap(Other).Height);
    Pix:=@TLinearBitmap(Other).Map^;
    for I:=0 to Size-1 do
    begin
      Map^[I]:=Pix^;
      Inc(Pix);
    end;
  end
end;

procedure TFloatMap32.AssignTo(Other: TObject);
var
  I : Integer;
  SrcPix : PSingle;
begin
  if Other is TLinearBitmap then
  begin
    TLinearBitmap(Other).New(Width,Height,pf8bit);
    SrcPix:=Pointer(Map);
    for I:=0 to Size-1 do
    begin
      if SrcPix^>=255 then TLinearBitmap(Other).Map^[I]:=255
      else if SrcPix^<=0 then TLinearBitmap(Other).Map^[I]:=0
      else TLinearBitmap(Other).Map^[I]:=Round(SrcPix^);
      Inc(SrcPix);
    end;
  end
  else inherited;
end;

procedure TFloatMap32.MakeBitmap(Image: TLinearBitmap; Min, Max: Single);
var
  I : Integer;
  SrcPix : PSingle;
  Scale : Single;
begin
  Scale:=255/(Max-Min);
  Image.New(Width,Height,pf8bit);
  SrcPix:=Pointer(Map);
  for I:=0 to Size-1 do
  begin
    if SrcPix^>=Max then Image.Map^[I]:=255
    else if SrcPix^<=Min then Image.Map^[I]:=0
    else Image.Map^[I]:=Round((SrcPix^-Min)*Scale);
    Inc(SrcPix);
  end;
end;

procedure TFloatMap32.Clear;
begin
  ZeroMem(Map^,ByteSize);
end;

procedure TFloatMap32.Multiply(Value: Single);
var
  I : Integer;
begin
  if Value=0 then Clear
  else for I:=0 to Size-1 do Map^[I]:=Map^[I]*Value;
end;

procedure TFloatMap32.SubtractFrom(Value: Single);
var
  I : Integer;
begin
  if Value=0 then Clear
  else for I:=0 to Size-1 do Map^[I]:=Value-Map^[I];
end;

procedure TFloatMap32.EnsureRange(Min,Max: Single);
var
  I : Integer;
begin
  for I:=0 to Size-1 do
    if Map^[I]<Min then Map^[I]:=Min
    else if Map^[I]>Max then Map^[I]:=Max;
end;

procedure TFloatMap32.Add(Other: TFloatMap32);
var
  I : Integer;
begin
  Assert((Other.Width=Width) and (Other.Height=Height));
  for I:=0 to Size-1 do
    Map^[I]:=Map^[I]+Other.Map^[I];
end;

procedure TFloatMap32.Add(Value: Single);
var
  I : Integer;
begin
  if Value<>0 then
    for I:=0 to Size-1 do
      Map^[I]:=Map^[I]+Value;
end;

procedure TFloatMap32.Min(Other: TFloatMap32);
var
  I : Integer;
begin
  Assert((Other.Width=Width) and (Other.Height=Height));
  for I:=0 to Size-1 do
    Map^[I]:=Math.Min(Map^[I],Other.Map^[I]);
end;

procedure TFloatMap32.Max(Other: TFloatMap32);
var
  I : Integer;
begin
  Assert((Other.Width=Width) and (Other.Height=Height));
  for I:=0 to Size-1 do
    Map^[I]:=Math.Max(Map^[I],Other.Map^[I]);
end;

procedure TFloatMap32.Transpose;

  procedure TransposeSquareInline;
  var
    R, C, E1, E2 : Integer;
    Swap : Single;
  begin
    for R:=1 to Height-1 do
    begin
      E1:=R*Width;
      for C:=0 to R-1 do
      begin
        E2:=C*Height+R;
        Swap:=Map[E1];
        Map[E1]:=Map[E2];
        Map[E2]:=Swap;
        Inc(E1);
      end;
    end;
  end;

  procedure TransposeInline;
  var
    R, C, RE2, CE2, E1, E2, S : Integer;
    Swap : Single;
    Done : array of Byte;
  begin
    SetLength(Done,(Size+7) div 8);
    S:=0;
    for R:=0 to Height-1 do
      for C:=0 to Width-1 do
      begin
        if Done[S div 8] and (1 shl (S mod 8))=0 then 
        begin
          Swap:=Map[S];
          E1:=S;
          E2:=C*Height+R;
          while E2<>S do
          begin
            Map[E1]:=Map[E2];
            Done[E1 div 8]:=Done[E1 div 8] or (1 shl (E1 mod 8));
            E1:=E2;
            RE2:=E2 div Width;
            CE2:=E2 mod Width;
            E2:=CE2*Height+RE2;
          end;
          Map[E1]:=Swap;
          Done[E1 div 8]:=Done[E1 div 8] or (1 shl (E1 mod 8));
        end;
        Inc(S);
      end;
  end;

  procedure TransposeInlineParallel(R: Integer);
  var
    C, RE2, CE2, E1, E2, S : Integer;
    Swap : Single;
    Done : Boolean;
  begin
    S:=R*Width;
    for C:=0 to Width-1 do
    begin
      E2:=C*Height+R;
      Done:=False;
      while E2<>S do
      begin
        if E2<S then
        begin
          Done:=True;
          Break;
        end;
        RE2:=E2 div Width;
        CE2:=E2 mod Width;
        E2:=CE2*Height+RE2;
      end;

      if not Done then
      begin
        Swap:=Map[S];
        E1:=S;
        E2:=C*Height+R;
        while E2<>S do
        begin
          Map[E1]:=Map[E2];
          E1:=E2;
          RE2:=E2 div Width;
          CE2:=E2 mod Width;
          E2:=CE2*Height+RE2;
        end;
        Map[E1]:=Swap;
      end;

      Inc(S);
    end;
  end;

  procedure TransposeCopy;
  var
    Old : PSingleArray;
    R, C : Integer;
  begin
    Old:=Map;
    GetMem(Map,ByteSize);
    for R:=0 to Height-1 do for C:=0 to Width-1 do Map[C+R*Width]:=Old[R+C*Height];
    FreeMem(Old);
  end;

begin
  if Width=Height then TransposeSquareInline
  else
  begin
    SwapDWords(FWidth,FHeight);
    FBytesPerLine:=Width*SizeOf(Single);
    if (FWidth<>1) and (FHeight<>1) then
      if ByteSize<1024*1024*64 then TransposeCopy
      //else if TMultiCoreProcess.NumberOfCores>1 then ParallelFor(0,Height-1,@TransposeInlineParallel)
      else TransposeInline;
  end;
end;

function TFloatMap32.Mean: Single;
var
  I : Integer;
begin
  Result:=0;
  for I:=0 to Size-1 do
    Result:=Result+Map^[I];
  if Size>0 then Result:=Result/Size;
end;

function TFloatMap32.Min: Single;
var
  I : Integer;
begin
  Result:=Infinity;
  for I:=0 to Size-1 do
    if Map^[I]<Result then Result:=Map^[I];
end;

function TFloatMap32.Max: Single;
var
  I : Integer;
begin
  Result:=-Infinity;
  for I:=0 to Size-1 do
    if Map^[I]>Result then Result:=Map^[I];
end;

function TFloatMap32.GetScanLine(Y: Integer): Pointer;
begin
  Assert((Y>=0) and (Y<Height));
  Result:=@Map^[Y*Width];
end;

function TFloatMap32.GetPixel(X, Y: Integer): PSingle;
begin
  Result:=@Map^[Y*Width+X];
end;

function TFloatMap32.GetPixelInterpolate(x,y: Single): Single;
var
  ix, iy : Integer;
  ScanLine : PSingleArray;
begin
  if x<0 then x := 0;
  if y < 0 then y := 0;
  ix := Trunc(x);
  iy := Trunc(y);
  if ix >= Width - 1 then
  begin
    ix := Width - 2;
    x := 1;
  end
  else x := x - ix;
  if iy >= Height - 1 then
  begin
    iy := Height - 2;
    y := 1;
  end
  else y := y - iy;
  ScanLine:=@Map^[IY*Width];
  Result := ScanLine^[ix] * (1 - x) * (1 - y) + ScanLine[ix + 1] * (x) * (1 - y) +
            ScanLine[Width + ix] * (1 - x) * (y) + ScanLine[Width + ix + 1] * (x) * (y);
end;

procedure TFloatMap32.New(X, Y: Integer);
begin
  if (X=Width) and (Y=Height) then Exit;
  FreeAndNilData(Map);
  if Int64(X)*Int64(Y)*SizeOf(Single)>2000000000 then raise EOutOfMemory.Create(SOutOfMemory);
  FSize:=X*Y;
  FWidth:=X;
  FHeight:=Y;
  FBytesPerLine:=Width*SizeOf(Single);
  FByteSize:=Size*SizeOf(Single);
  try
    GetMem(Map,FByteSize);
  except
    Dispose;
    raise;
  end;
end;

//==================================================================================================
// TFloatMap
//==================================================================================================

constructor TFloatMap.Create(X,Y: Integer);
begin
  Create;
  New(X,Y);
end;

function TFloatMap.GetScanLine(Y: Integer): Pointer;
begin
  Assert((Y>=0) and (Y<Height));
  Result:=@Map^[Y*Width];
end;

function TFloatMap.GetPixel(X,Y: Integer): PFloat;
begin
  Result:=@Map^[Y*Width+X];
end;

function TFloatMap.Extrapolate(X,Y: Integer): Float;
begin
  if Y<=0 then
  begin
    if X<=0 then Result:=Map^[0]
    else if X<Width then Result:=Map^[X]
    else Result:=Map^[Width-1];
  end
  else if Y<Height then
  begin
    if X<=0 then Result:=Map^[Y*Width]
    else if X<Width then Result:=Map^[Y*Width+X]
    else Result:=Map^[(Y+1)*Width-1];
  end
  else // Y>=Height
  begin
    if X<=0 then Result:=Map^[Size-Width+1]
    else if X<Width then Result:=Map^[Size-Width+1+X]
    else Result:=Map^[Size-1];
  end;
  Assert(not IsNan(Result),Format('NaN at %d,%d',[X,Y]));
end;

function TFloatMap.Interpolate(x,y: Single): Float;
var
  ix, iy : Integer;
  ScanLine : PFloatArray;
begin
  if x<0 then x := 0;
  if y < 0 then y := 0;
  ix := Trunc(x);
  iy := Trunc(y);
  if ix >= Width - 1 then
  begin
    ix := Width - 2;
    x := 1;
  end
  else x := x - ix;
  if iy >= Height - 1 then
  begin
    iy := Height - 2;
    y := 1;
  end
  else y := y - iy;
  ScanLine:=@Map^[IY*Width];
  Result := ScanLine^[ix] * (1 - x) * (1 - y) + ScanLine[ix + 1] * (x) * (1 - y) +
            ScanLine[Width + ix] * (1 - x) * (y) + ScanLine[Width + ix + 1] * (x) * (y);
end;

// Create new map
procedure TFloatMap.New(X,Y: Integer);
begin
  if (X=Width) and (Y=Height) then Exit;
  FreeAndNilData(Map);
  if Int64(X)*Int64(Y)*SizeOf(Float)>2000000000 then
    raise EOutOfMemory.Create(SOutOfMemory);
  FSize:=X*Y;
  FWidth:=X;
  FHeight:=Y;
  FBytesPerLine:=Width*SizeOf(Float);
  FByteSize:=Size*SizeOf(Float);
  try
    GetMem(Map,FByteSize);
  except
    Dispose;
    raise;
  end;
end;

procedure TFloatMap.TakeOver(Other: TFloatMap);
begin
  FreeAndNilData(Map);
  Map:=Other.Map;
  Other.Map:=nil;
  FWidth:=Other.Width;
  FHeight:=Other.Height;
  FSize:=Other.Size;
  FBytesPerLine:=Other.BytesPerLine;
  FByteSize:=Other.ByteSize;
  Other.Dispose;
end;

procedure TFloatMap.Assign(Other: TObject);
var
  I : Integer;
  Pix : PByte;
  Pix16 : PWord absolute Pix;
  Pix24 : PRGBRec absolute Pix;
  Value : PFloat;
  Real : TLinearBitmap;
begin
  if Other is TFloatMap then
  begin
    New(TBaseFloatMap(Other).Width,TBaseFloatMap(Other).Height);
    Move(TFloatMap(Other).Map^,Map^,ByteSize);
  end
  else if Other is TFloatMap32 then
  begin
    New(TBaseFloatMap(Other).Width,TBaseFloatMap(Other).Height);
    for I:=0 to Size-1 do
      Map^[I]:=TFloatMap32(Other).Map^[I];
  end
  else if Other is TLinearBitmap then
  begin
    Real:=TLinearBitmap(Other);
    New(Real.Width,Real.Height);
    Pix:=@Real.Map^;
    Value:=@Map^;
    if Real.PixelFormat=pf16bit then // 16 bit
      for I:=1 to Size do
      begin
        Value^:=Pix16^;
        Inc(Value);
        Inc(Pix16);
      end
    else if Real.PixelFormat=pf8bit then // 8 bit
      for I:=1 to Size do
      begin
        Value^:=Pix^;
        Inc(Value);
        Inc(Pix);
      end
    else if Real.PixelFormat=pf24bit then // 24 bit
      for I:=1 to Size do
      begin
        Value^:=(2*Pix24^.R+3*Pix24^.G+Pix24^.B)/6;
        Inc(Value);
        Inc(Pix24);
      end
    else
      raise Exception.Create(rsInvalidPixelFormat);
  end
  else inherited;
end;

procedure TFloatMap.AssignTo(Other: TObject);
var
  I : Integer;
  Pix : PByte;
begin
  if Other is TLinearBitmap then
  begin
    TLinearBitmap(Other).New(Width,Height,pf8bit);
    Pix:=@TLinearBitmap(Other).Map^;
    for I:=0 to Size-1 do
    begin
      if Map^[I]>=255 then Pix^:=255
      else if Map^[I]<=0 then Pix^:=0
      else Pix^:=Round(Map^[I]);
      Inc(Pix);
    end;
  end
  else inherited;
end;

// Release image map
procedure TFloatMap.Dispose;
begin
  inherited;
  FreeAndNilData(Map);
end;

procedure TFloatMap.Clear(Value: Float);
var P : Integer;
begin
  for P:=0 to Size-1 do Map^[P]:=Value;
end;

procedure TFloatMap.Clear;
begin
  ZeroMem(Map^,ByteSize);
end;

procedure TFloatMap.DeleteLine(Y: Integer);
begin
  Move(Map^[(Y+1)*Width],Map^[Y*Width],(Height-Y-1)*BytesPerLine);
  Dec(FHeight);
  Dec(fSize,Width);
  Dec(fByteSize,BytesPerLine);
end;

procedure TFloatMap.GetStat(out Min,Max: Float);
var
  I : Integer;
  P : PFloat;
begin
  Min:=MaxFloat;
  Max:=-MaxFloat;
  P:=@Map^;
  for I:=1 to Size do
  begin
    if P^>Max then Max:=P^;
    if P^<Min then Min:=P^;
    Inc(P);
  end;
end;

procedure TFloatMap.MakeBitmap(Image: TLinearBitmap; Method: TMappingMethod; const Min, Max: Float);
var
  MinValue, MaxValue, Divisor : Float;
  P : Integer;
  A : Float;
  Value : PFloat;
  Pix : PByte;
  Pix16 : PWord absolute Pix;
begin
  if Method=mm16bitLinear then Image.New(Width,Height,pf16bit)
  else
  begin
    Image.New(Width,Height,pf8bit);
    Image.Palette^:=GrayPal;
  end;
  if Min>Max then GetStat(MinValue,MaxValue)
  else
  begin
    MinValue:=Min;
    MaxValue:=Max;
  end;
  if IsInfinite(MaxValue) then
  begin
    Image.Clear(0);
    Exit;
  end;

  if Abs(MaxValue-MinValue)<0.00000001 then MinValue:=MaxValue-1;
  Divisor:=MaxValue-MinValue;
  if Divisor=0 then Divisor:=1;

  Pix:=Pointer(Image.Map);
  Value:=Pointer(Map);
  if Image.PixelFormat=pf8bit then
    for P:=1 to Size do
    begin
      A:=(Value^-MinValue)/Divisor;
      case Method of
        mmLinear : A:=A*255;
        mmSqrt   : if A>0 then A:=Sqrt(A)*255 else A:=0;
      end;
      if A>255 then A:=255
      else if A<0 then A:=0;
      Pix^:=Round(A);
      Inc(Value);
      Inc(Pix);
    end
  else // 16 bit
    for P:=1 to Size do
    begin
      A:=$ffff*(Value^-MinValue)/Divisor;
      if A>$ffff then A:=$ffff
      else if A<0 then A:=0;
      Pix16^:=Round(A);
      Inc(Value);
      Inc(Pix16);
    end
end;

procedure TFloatMap.ResizeCanvas(XSiz,YSiz,XPos,YPos: Integer; Background: Float);
var
  OldMap : PByteArray;
  Y, XStart, OldBytesPerLine, LineMove, Bottom : Integer;
begin
  if (XSiz<1) or (YSiz<1) then
  begin
    Dispose;
    Exit;
  end;
  if XSiz<1 then
  begin
    New(XSiz,YSiz);
    Clear(Background);
    Exit;
  end;
  if (XSiz=Width) and (YSiz=Height) and (XPos=0) and (YPos=0) then Exit;

  OldBytesPerLine:=BytesPerLine;
  fBytesPerLine:=XSiz*SizeOf(Float);
  fSize:=XSiz*YSiz;
  fByteSize:=fSize*SizeOf(Float);
  OldMap:=Pointer(Map);
  GetMem(Map,fByteSize);

  XPos:=XPos*SizeOf(Float);
  if XPos>=0 then XStart:=0
  else
  begin
    XStart:=-XPos;
    XPos:=0;
  end;
  LineMove:=OldBytesPerLine-XStart;
  if LineMove+XPos>BytesPerLine then LineMove:=BytesPerLine-XPos;
  Bottom:=Min(Height-1,YSiz-YPos-1);
  fWidth:=XSiz; fHeight:=YSiz;
  Clear(Background);
  if LineMove>0 then for Y:=Max(0,-YPos) to Bottom do
    Move(OldMap^[Y*OldBytesPerLine+XStart],PByteArray(Map)^[(Y+YPos)*BytesPerLine+XPos],LineMove);
  FreeMem(OldMap);
end;

procedure TFloatMap.Paste(Source: TFloatMap; X,Y: Integer);
var
  CopySize : Integer;
  DestRect : TRect;
  SrcLine, DestLine : PFloat;
begin
  if not IntersectRect(DestRect,Rect(0,0,Width,Height),Bounds(X,Y,Source.Width,Source.Height)) then Exit;
  SrcLine:=Source.Pixel[Max(0,-X),Max(0,-Y)];
  DestLine:=Pixel[DestRect.Left,DestRect.Top];
  CopySize:=(DestRect.Right-DestRect.Left)*SizeOf(Float);
  for Y:=DestRect.Top to DestRect.Bottom-1 do
  begin
    Move(SrcLine^,DestLine^,CopySize);
    Inc(SrcLine,Source.Width);
    Inc(DestLine,Width);
  end;
end;

end.

