unit UniDIB;
{TUniDIB version 1.21}

{by Vit Kovalcik}

interface

uses Monitor, Windows;

const
  C_MaxAllowedBPP=6;
  C_AllowedBPP:array [1..C_MaxAllowedBPP] of Byte = (1,4,8,16,24,32);

const
  SBU_NONE=0; {see SetPixel VARIABLE}
  SBU_RED=1;
  SBU_GREEN=2;
  SBU_BLUE=3;

type
  PLogPalette256 = ^TLogPalette256;
  TLogPalette256 = record
    palVersion: Word;
    palNumEntries: Word;
    palEntry: array[0..255] of TPaletteEntry;
  end;

type
  TBitmapInfo256 = record
    bmiHeader : TBITMAPINFOHEADER;
    bmiColors : array[0..255] of TRGBQUAD;
  end;

type
  TSetPixelProc = procedure (X,Y,Value:Integer) of object;
  TGetPixelFunc = function (X,Y:Integer):Integer of object;
  TSetSeqPixelProc = procedure (Value:Integer) of object;
  TGetSeqPixelFunc = function:Integer of object;

type
  TUniDIB = class(TMonitorObject)
  protected
    FBMInfo:TBitmapInfo256;           {Informations about this bitmap}
    FHandle:HBITMAP;                  {Handle of this bitmap}
    FDC:HDC;                          {DC (compatible with the current screen)}
    FBits:Pointer;                    {Pointer to array of bits}
    FPalHandle:HPALETTE;              {Handle of palette}
    FActPointer:Pointer;              {Pointer to next pixel (for sequential access)}
    FDWordWidth:Cardinal;             {Width of row in bytes aligned to Double Word}    

    XActX:Integer;                    {Actual X coordinate (for sequential access)}
    XSize:Integer;                    {Size of complete array of bits}
    XUsage:UINT;                      {DIB_PAL_COLORS or DIB_RGB_COLORS}
    XClrCount:Integer;                {Number of colors in 1,4 and 8 bits modes}
    XSelPalette:HPALETTE;
    procedure SetPixel1 (X,Y,Value:Integer);
    procedure SetPixel4 (X,Y,Value:Integer);
    procedure SetPixel8 (X,Y,Value:Integer);
    procedure SetPixel16 (X,Y,Value:Integer);
    procedure SetPixel16R (X,Y,Value:Integer);
    procedure SetPixel16G (X,Y,Value:Integer);
    procedure SetPixel16B (X,Y,Value:Integer);
    procedure SetPixel24 (X,Y,Value:Integer);
    procedure SetPixel32 (X,Y,Value:Integer);
    function GetPixel1 (X,Y:Integer):Integer;
    function GetPixel4 (X,Y:Integer):Integer;
    function GetPixel8 (X,Y:Integer):Integer;
    function GetPixel16 (X,Y:Integer):Integer;
    function GetPixel16R (X,Y:Integer):Integer;
    function GetPixel16G (X,Y:Integer):Integer;
    function GetPixel16B (X,Y:Integer):Integer;    
    function GetPixel24 (X,Y:Integer):Integer;
    function GetPixel32 (X,Y:Integer):Integer;
    procedure SetSeqPixel1 (Value:Integer);
    procedure SetSeqPixel4 (Value:Integer);
    procedure SetSeqPixel8 (Value:Integer);
    procedure SetSeqPixel16 (Value:Integer);
    procedure SetSeqPixel16R (Value:Integer);
    procedure SetSeqPixel16G (Value:Integer);
    procedure SetSeqPixel16B (Value:Integer);
    procedure SetSeqPixel24 (Value:Integer);
    procedure SetSeqPixel32 (Value:Integer);
    function GetSeqPixel1:Integer;
    function GetSeqPixel4:Integer;
    function GetSeqPixel8:Integer;
    function GetSeqPixel16:Integer;
    function GetSeqPixel16R:Integer;
    function GetSeqPixel16G:Integer;
    function GetSeqPixel16B:Integer;
    function GetSeqPixel24:Integer;
    function GetSeqPixel32:Integer;
  public
    SetPixel:TSetPixelProc;
{In the procedure SetPixel depends the "Value" on ABPP specified
 in constructor.
 ABPP - 1,4,8 - simply set the Value to color from palette
        16,24 - 3 bytes are used, one for red, second for green and
                third for blue element of the desired color
        32    - strange number of bits - in Win32 Developer's References
                is written that 4th byte is NOT used (?)
                I don't know much about it yet.
                (It seems to be same as 24 bit depth, but with 4 bytes)

 ****!!!!!!*****
 Next thing is that coordinates [0,0] are NOT at the upper left corner,
 but at the LOWER left one.
 ****!!!!!!*****}
    GetPixel:TGetPixelFunc;
{procedures for sequential access to pixels}
       SetSeqPixel:TSetSeqPixelProc;
       GetSeqPixel:TGetSeqPixelFunc;
       procedure Seek (X,Y:Integer);
         {Sets actual pointer to the pixel [X,Y]
          This pointer is pointing to the next pixel to operate with.}
    constructor Create (AWidth,AHeight:LongInt;ABPP:Byte;SByteUse:Byte);
      {ABPP - bits per pixel (up to 32)
              If this is not some of allowed values (see C_AllowedBPP),
              it is rounded to the nearest upper one.
       SByte- use of 16th bit in 2 bytes
              used only when ABPP is 16
              SBU_NONE  - unused
              SBU_RED   - red element of pixel have
                          6 (SIX) bits; it is not visible, but it
                          is more accurate for image conversions
              SBU_GREEN - same as above, but for green component
              SBU_BLUE  - and for blue}
    destructor Destroy; override;
    procedure DIBtoScreen(DC:hDC);
      {After this procedure you should call Invalidate or
       something alike.}
    procedure SetPalette(Pal:TLogPalette256);
    procedure Clear;
    procedure DrawHorizLine(X1,X2,Y,Col:Integer);
    procedure DrawVertLine (X,Y1,Y2,Col:Integer);
    procedure DrawLine (X1,Y1,X2,Y2:integer; Col:Integer);
    procedure FillPolygon (Poly:Array of TPoint; FillCol:Integer);
    procedure CaptureScreen;
      {Captures actual screen content to this DIB}
    property ActPointer:Pointer read FActPointer write FActPointer;      
    property Bits:Pointer read FBits;
    property DC:HDC read FDC;
    property DWordWidth:Cardinal read FDWordWidth;
    property Handle:HBITMAP read FHandle;
    property Height:LongInt read FBMInfo.bmiHeader.biHeight;
    property Width:LongInt read FBMInfo.bmiHeader.biWidth;
    function IndexColor: Boolean;
    property PaletteHandle:HPalette read FPalHandle;
  end;

implementation

{$WARNINGS OFF}

procedure TUniDIB.SetPixel1 (X,Y,Value:Integer);assembler;
// EAX = Self
// EBX = ?    (It is needed after end of procedure !)
// ECX = Y
// EDX = X
asm
  push ebx
  push eax
  mov  ebx,x           //EBX:=X
  mov  eax,[eax].FDWordWidth  //EAX:=FDWordWidth*8*Y
  shl  eax,1
  shl  eax,1
  shl  eax,1
  mul  Y
{8 pixels are stored byte; first pixel is the most
 important bit in byte}
  xor  ebx,7           //=>EBX:=(EBX div 8) + (7-EBX mod 8)
  add  ebx,eax         //EBX:=EBX+EAX
  pop  eax
  mov  eax,[eax].FBits
  cmp  Value,0
  jz   @@2
@@1:
  bts  [eax],ebx       //Set this bit
  jmp  @@3
@@2:
  btr  [eax],ebx       //Clear this bit
@@3:
  pop  ebx
end;

function TUniDIB.GetPixel1 (X,Y:Integer):Integer;assembler;
// EAX = Self
// EBX = ? (!!)
// ECX = Y
// EDX = X
// Result = EAX
asm
  push ebx
  push eax
  mov  ebx,x           //EBX:=X
  mov  eax,[eax].FDWordWidth  //EAX:=FDWordWidth*8*Y
  shl  eax,1
  shl  eax,1
  shl  eax,1  
  mul  y
  xor  ebx,7           //EBX:=(EBX div 8) + (7-EBX mod 8)
  add  ebx,eax         //EBX:=EBX+EAX
  pop  eax
  mov  eax,[eax].FBits
  bt   [eax],ebx       //Copy bit to CF
  sbb  eax,eax         //Substract with borrow - EAX:=EAX-CF
  and  eax,1           //Return only one bit
  pop  ebx
end;

procedure TUniDIB.SetPixel4 (X,Y,Value:Integer);
var A:Byte;
    Z:Integer;
begin
  Z:=integer(FBits)+y*FDWordWidth+X shr 1;
  A:=pByte(Z)^;
  If X and 1=0 then
    pByte(Z)^:=(A AND $0F) OR (Value SHL 4)
  else
    pByte(Z)^:=(A AND $F0) OR Value;
end;

function TUniDIB.GetPixel4 (X,Y:Integer):Integer;
begin
  Result:=pByte(integer(FBits)+y*FDWordWidth+X shr 1)^;
  If X and 1>0 then
    Result:=Result AND $F
  else
    Result:=Result SHR 4 AND $F;
end;

procedure TUniDIB.SetPixel8 (X,Y,Value:Integer);
begin
  pByte ( integer(FBits) + y*FDWordWidth + x )^ := Value;
end;

function TUniDIB.GetPixel8 (X,Y:Integer):Integer;
begin
  Result := pByte ( integer(FBits) + y*FDWordWidth + x )^;
end;

procedure TUniDIB.SetPixel16 (X,Y,Value:Integer);
begin
  pWord(integer(FBits)+y*FDWordWidth+x shl 1)^:=Word((Value AND $1F0000 SHR 6) OR (Value AND $001F00 SHR 3) OR (Value AND $00001F));
end;

function TUniDIB.GetPixel16 (X,Y:Integer):Integer;
var Z:Word;
begin
  Z:=pWord(integer(FBits)+y*FDWordWidth+x shl 1)^;
  Result:=(Z AND $7C00 SHL 6) OR (Z AND $3E0 SHL 3) OR (Z AND $001F);
end;

procedure TUniDIB.SetPixel16R (X,Y,Value:Integer);
begin
  pWord(integer(FBits)+y*FDWordWidth+x shl 1)^:=Word((Value AND $3E0000 SHR 7) OR (Value AND $001F00 SHR 3) OR (Value AND $00001F) OR (Value AND $010000 SHR 1));
end;

function TUniDIB.GetPixel16R (X,Y:Integer):Integer;
var Z:Word;
begin
  Z:=pWord(integer(FBits)+y*FDWordWidth+x shl 1)^;
  Result:=(Z AND $7C00 SHL 7) OR (Z AND $3E0 SHL 3) OR (Z AND $001F) OR (Z AND $8000 SHL 1);
end;

procedure TUniDIB.SetPixel16G (X,Y,Value:Integer);
begin
  pWord(integer(FBits)+y*FDWordWidth+x shl 1)^:=Word((Value AND $1F0000 SHR 6) OR (Value AND $003E00 SHR 4) OR (Value AND $00001F) OR (Value AND $000100 SHL 7));
end;

function TUniDIB.GetPixel16G (X,Y:Integer):Integer;
var Z:Word;
begin
  Z:=pWord(integer(FBits)+y*FDWordWidth+x shl 1)^;
  Result:=(Z AND $7C00 SHL 6) OR (Z AND $3E0 SHL 4) OR (Z AND $001F) OR (Z AND $8000 SHR 7);
end;

procedure TUniDIB.SetPixel16B (X,Y,Value:Integer);
var Z:Word;
begin
  Z:=Word((Value AND $1F0000 SHR 6) OR (Value AND $001F00 SHR 3) OR (Value AND $00003E SHR 1) OR (Value AND $000001 SHL 15));
  pWord(integer(FBits)+y*FDWordWidth+x shl 1)^:=Z;
end;

function TUniDIB.GetPixel16B (X,Y:Integer):Integer;
var Z:Word;
begin
  Z:=pWord(integer(FBits)+y*FDWordWidth+x shl 1)^;
  Result:=(Z AND $7C00 SHL 6) OR (Z AND $3E0 SHL 3) OR (Z AND $001E SHL 1) OR (Z AND $8000 SHR 1);
end;

procedure TUniDIB.SetPixel24 (X,Y,Value:Integer);
var Z:Integer;
begin
  Z:=integer(FBits)+y*FDWordWidth+x+x+x;
  pByte(Z)^:=Value AND $FF;
  pByte(integer(Z)+1)^:=Value SHR 8 AND $FF;
  pByte(integer(Z)+2)^:=Value SHR 16 AND $FF;
{It can't be something like :
    pInteger (Z)^:=pInteger(Z)^ AND $FF000000 OR Value;
 because there would be an EAccessViolation exception,
 if   X=Width-1  and  Y=Height-1  (only in some cases) !}
end;

function TUniDIB.GetPixel24 (X,Y:Integer):Integer;
var Z:Integer;
begin
  Z:=integer(FBits)+y*FDWordWidth+x+x+x;
  Result:=pByte(Z)^ OR (pByte(integer(Z)+1)^ SHL 8) OR (pByte(integer(Z)+2)^ shl 16);
end;

procedure TUniDIB.SetPixel32 (X,Y,Value:Integer);
begin
  pInteger(integer(FBits)+y*FDWordWidth+x shl 2)^:=Value;
end;

function TUniDIB.GetPixel32 (X,Y:Integer):Integer;
begin
  Result:=pInteger(integer(FBits)+y*FDWordWidth+x shl 2)^;
end;

procedure TUniDIB.SetSeqPixel1 (Value:Integer);assembler;
// EAX = Self
// EBX = ?    (It is needed after end of procedure !)
// EDX = Value ?
asm
  mov  ecx,[eax].XActX
  inc  [eax].XActX
  mov  eax,[eax].FActPointer
  xor  ecx,7
  cmp  Value,0
  jz   @@2
@@1:
  bts  [eax],ecx
  jmp  @@3
@@2:
  btr  [eax],ecx
@@3:
end;

function TUniDIB.GetSeqPixel1:Integer;assembler;
// EAX = Self
// EBX = ? (!!)
// Result = EAX
asm
  mov  ecx,[eax].XActX
  inc  [eax].XActX
  mov  eax,[eax].FActPointer
  xor  ecx,7
  bt   [eax],ecx       //Copy bit to CF
  sbb  eax,eax         //Substract with borrow - EAX:=EAX-CF
  and  eax,1           //Return only one bit
end;

procedure TUniDIB.SetSeqPixel4 (Value:Integer);
var A:Byte;
    Z:Integer;
begin
  Z:=Integer (FActPointer)+XActX shr 1;
  A:=pByte(Z)^;
  If XActX and 1=0 then
    pByte(Z)^:=(A AND $0F) OR (Value SHL 4)
  else
    pByte(Z)^:=(A AND $F0) OR Value;
  Inc (XActX);
end;

function TUniDIB.GetSeqPixel4:Integer;
begin
  Result:=pByte(Integer(FActPointer)+XActX shr 1)^;
  If XActX and 1=1 then
    Result:=Result AND $F
  else
    Result:=Result SHR 4 AND $F;
  Inc (XActX);
end;

procedure TUniDIB.SetSeqPixel8 (Value:Integer);
begin
  pByte (FActPointer)^ := Value;
  FActPointer:=Pointer(Integer(FActPointer)+1);
end;

function TUniDIB.GetSeqPixel8:Integer;
begin
  Result := pByte(FActPointer)^;
  FActPointer:=Pointer(Integer(FActPointer)+1);
end;

procedure TUniDIB.SetSeqPixel16 (Value:Integer);
begin
  pWord(FActPointer)^:=Word((Value AND $1F0000 SHR 6) OR (Value AND $001F00 SHR 3) OR (Value AND $00001F));
  FActPointer:=Pointer(Integer(FActPointer)+2);
end;

function TUniDIB.GetSeqPixel16:Integer;
var Z:Word;
begin
  Z:=pWord(FActPointer)^;
  Result:=(Z AND $7C00 SHL 6) OR (Z AND $3E0 SHL 3) OR (Z AND $001F);
  FActPointer:=Pointer(Integer(FActPointer)+2);
end;

procedure TUniDIB.SetSeqPixel16R (Value:Integer);
begin
  pWord(FActPointer)^:=Word((Value AND $3E0000 SHR 7) OR (Value AND $001F00 SHR 3) OR (Value AND $00001F) OR (Value AND $010000 SHR 1));
  FActPointer:=Pointer(Integer(FActPointer)+2);
end;

function TUniDIB.GetSeqPixel16R:Integer;
var Z:Word;
begin
  Z:=pWord(FActPointer)^;
  Result:=(Z AND $7C00 SHL 7) OR (Z AND $3E0 SHL 3) OR (Z AND $001F) OR (Z AND $8000 SHL 1);
  FActPointer:=Pointer(Integer(FActPointer)+2);
end;

procedure TUniDIB.SetSeqPixel16G (Value:Integer);
begin
  pWord(FActPointer)^:=Word((Value AND $1F0000 SHR 6) OR (Value AND $003E00 SHR 4) OR (Value AND $00001F) OR (Value AND $000100 SHL 7));
  FActPointer:=Pointer(Integer(FActPointer)+2);
end;

function TUniDIB.GetSeqPixel16G :Integer;
var Z:Word;
begin
  Z:=pWord(FActPointer)^;
  Result:=(Z AND $7C00 SHL 6) OR (Z AND $3E0 SHL 4) OR (Z AND $001F) OR (Z AND $8000 SHR 7);
  FActPointer:=Pointer(Integer(FActPointer)+2);
end;

procedure TUniDIB.SetSeqPixel16B (Value:Integer);
begin
  pWord(FActPointer)^:=Word((Value AND $1F0000 SHR 6) OR (Value AND $001F00 SHR 3) OR (Value AND $00003E SHR 1) OR (Value AND $000001 SHL 15));
  FActPointer:=Pointer(Integer(FActPointer)+2);
end;

function TUniDIB.GetSeqPixel16B:Integer;
var Z:Word;
begin
  Z:=pWord(FActPointer)^;
  Result:=(Z AND $7C00 SHL 6) OR (Z AND $3E0 SHL 3) OR (Z AND $001E SHL 1) OR (Z AND $8000 SHR 1);
  FActPointer:=Pointer(Integer(FActPointer)+2);
end;

procedure TUniDIB.SetSeqPixel24 (Value:Integer);
begin
  pByte(FActPointer)^:=Value AND $FF;
  pByte(Integer(FActPointer)+1)^:=Value SHR 8 AND $FF;
  pByte(integer(FActPointer)+2)^:=Value SHR 16 AND $FF;
  FActPointer:=Pointer(Integer(FActPointer)+3);
end;

function TUniDIB.GetSeqPixel24:Integer;
begin
  Result:=pByte(FActPointer)^ OR (pByte(integer(FActPointer)+1)^ SHL 8) OR (pByte(integer(FActPointer)+2)^ shl 16);
  FActPointer:=Pointer(Integer(FActPointer)+3);
end;

procedure TUniDIB.SetSeqPixel32 (Value:Integer);
begin
  pInteger(FActPointer)^:=Value;
  FActPointer:=Pointer (Integer(FActPointer)+4);
end;

function TUniDIB.GetSeqPixel32 :Integer;
begin
  Result:=pInteger(FActPointer)^;
  FActPointer:=Pointer (Integer(FActPointer)+4);
end;

procedure TUniDIB.Seek (X,Y:Integer);
begin
  XActX:=X;
  FActPointer:=Pointer(Integer(FBits)+Y*FDWordWidth+X*(FBMInfo.bmiHeader.biBitCount shr 3));
end;

constructor TUniDIB.Create (AWidth,AHeight:LongInt;ABPP:Byte;SByteUse:Byte);
var A:Integer;
begin
  inherited Create;
  FDC := CreateCompatibleDC(0);
  with FBMInfo.bmiHeader do
  begin
    biSize:=SizeOf(TBitmapInfoHeader);
    biPlanes:=1;
    A:=1;
    while (A<C_MaxAllowedBPP) AND (ABPP>C_AllowedBPP[A]) do
      Inc (A);
    biBitCount:=C_AllowedBPP[A];
    biCompression:=BI_RGB;
    If AWidth<=0 then
      biWidth:=1
    else
      biWidth:=AWidth;
    If AHeight=0 then
      biHeight:=1
    else
      biHeight:=AHeight;
    biSizeImage:=0;
    biXPelsPerMeter:=0;
    biYPelsPerMeter:=0;
    biClrUsed:=0;
    biClrImportant:=0;
  end;

  If ABPP<=8 then
  begin
    XClrCount:=1 shl FBMInfo.bmiHeader.biBitCount;
      {=> XClrCount:=2^FBMInfo.bmiHeader.biBitCount;}
    XUsage:=DIB_PAL_COLORS;
  end
  else
    XUsage:=DIB_RGB_COLORS;

  FDWordWidth:=((AWidth*FBMInfo.bmiHeader.biBitCount+31) shr 5)shl 2;
  XSize:=FDWordWidth*Abs(AHeight);
  FHandle := CreateDIBSection(FDC,pBitmapInfo(@FBMInfo)^,
     XUsage,FBits,0,0);
  Case FBMInfo.bmiHeader.biBitCount of
    1:begin
        SetPixel:=SetPixel1;
        GetPixel:=GetPixel1;
        SetSeqPixel:=SetSeqPixel1;
        GetSeqPixel:=GetSeqPixel1;
      end;
    4:begin
        SetPixel:=SetPixel4;
        GetPixel:=GetPixel4;
        SetSeqPixel:=SetSeqPixel4;
        GetSeqPixel:=GetSeqPixel4;
      end;
    8:begin
        SetPixel:=SetPixel8;
        GetPixel:=GetPixel8;
        SetSeqPixel:=SetSeqPixel8;
        GetSeqPixel:=GetSeqPixel8;
      end;
    16:begin
         Case SByteUse of
           SBU_NONE:begin
               SetPixel:=SetPixel16;
               GetPixel:=GetPixel16;
               SetSeqPixel:=SetSeqPixel16;
               GetSeqPixel:=GetSeqPixel16;
             end;
           SBU_RED:begin
               SetPixel:=SetPixel16R;
               GetPixel:=GetPixel16R;
               SetSeqPixel:=SetSeqPixel16R;
               GetSeqPixel:=GetSeqPixel16R;
             end;
           SBU_GREEN:begin
               SetPixel:=SetPixel16G;
               GetPixel:=GetPixel16G;
               SetSeqPixel:=SetSeqPixel16G;
               GetSeqPixel:=GetSeqPixel16G;
             end;
           SBU_BLUE:begin
               SetPixel:=SetPixel16B;
               GetPixel:=GetPixel16B;
               SetSeqPixel:=SetSeqPixel16B;
               GetSeqPixel:=GetSeqPixel16B;
             end;
        end;
      end;
    24:begin
         SetPixel:=SetPixel24;
         GetPixel:=GetPixel24;
         SetSeqPixel:=SetSeqPixel24;
         GetSeqPixel:=GetSeqPixel24;
      end;
    32:begin
         SetPixel:=SetPixel32;
         GetPixel:=GetPixel32;
         SetSeqPixel:=SetSeqPixel32;
         GetSeqPixel:=GetSeqPixel32;
      end;
  end;
  SelectObject(FDC, FHandle);
end;

destructor TUniDIB.Destroy;
begin
  DeleteObject (FHandle);
  DeleteDC (FDC);
  inherited Destroy;
end;

procedure TUniDIB.DIBtoScreen(DC:hDC);
var Pal:HPalette;
begin
  BitBlt(DC,0,0,FBMInfo.bmiHeader.biWidth,Abs(FBMInfo.bmiHeader.biHeight),FDC,0,0,SRCCOPY);
  If XUsage=DIB_PAL_COLORS then
  begin
    Pal:=SelectPalette(DC,FPalHandle,false);
    RealizePalette (DC);
    SelectPalette (DC,Pal,true);
  end;
end;

procedure TUniDIB.Clear;
var
   pDWord : pLongInt;
   i   : integer;
begin
  pDWord := FBits;
  for i := 1 to XSize shr 2 do
  begin
    pLongInt(pDWord)^ := $00000000;
    inc(pDWord); {!! pDWord:=pDWord+ 4 }
                                   {===}
  end;
end;

procedure TUniDIB.DrawHorizLine(X1,X2,Y:Integer; Col:Integer);
var X,T:Integer;
    P:Pointer;
begin
  If X2>X1 then
  begin
    X:=X1;
    X1:=X2;
    X2:=X;
  end;
  T:=XActX;
  P:=FActPointer;
  Seek (X1,Y);
  For X:=X1 to X2 do
    SetSeqPixel (Col);
  XActX:=T;
  FActPointer:=P;
end;

procedure TUniDIB.DrawVertLine (X,Y1,Y2,Col:Integer);
var Y:Integer;
begin
  If Y1>Y2 then
  begin
    Y:=Y1;
    Y1:=Y2;
    Y2:=Y;
  end;
  For Y:=Y1 to Y2 do
    SetPixel (X,Y,Col);
end;

procedure TUniDIB.DrawLine(x1,y1,x2,y2:integer; Col:Integer);
var
   lp1              : integer;
   x,y              : integer;
   dy,dx,step,delta : integer;
begin
  dx:=x2-x1;
  dy:=y2-y1;
  { case nought }
  if (dy=0) and (dx=0) then
     SetPixel(x1,y1,Col)
  { case one }
  else
    if dy=0 then
    begin
      DrawHorizLine(x1,x2,y1,Col);
      exit;
    end
    { case two }
    else
      if dx=0 then
      begin
        DrawVertLine(x1,y1,y2,Col);
        exit;
      end
      { case three }
      else
        if (abs(dx)>abs(dy)) then
        begin
          if dy>0 then
            step:= 1
          else
          begin
            step:=-1;
            dy:=-dy;
          end;
          delta:=dy shr 1;
          if dx>=0 then
          begin
            y:=y1;
            for lp1:=x1 to x2 do
            begin
              SetPixel(lp1,y,Col);
              delta:=delta+dy;
              if delta>dx then
              begin
                y:=y+step;
                delta:=delta-dx;
              end;
            end;
          end
          else
          begin { dx<0 }
            y:=y2;
            dx:=-dx;
            dy:=-dy;
            for lp1:=x2 to x1 do
            begin
              SetPixel(lp1,y,Col); delta:=delta-dy;
              if delta>dx then
              begin
                y:=y-step;
                delta:=delta-dx;
              end;
            end;
          end;
        end
        else
        begin  { dy>dx }
          if dx>0 then
            step:= 1
          else
          begin
            step:=-1;
            dx:=-dx;
          end;
          delta:=dx shr 1;
          if dy>=0 then
          begin
            x:=x1;
            for lp1:=y1 to y2 do
            begin
              SetPixel(x,lp1,Col);
              delta:=delta+dx;
              if delta>dy then
              begin
                x:=x+step;
                delta:=delta-dy;
              end;
            end;
          end
          else
          begin { dy<0 }
            x:=x2;
            dy:=-dy;
            dx:=-dx;
            for lp1:=y2 to y1 do
            begin
              SetPixel(x,lp1,Col);
              delta:=delta-dx;
              if delta>dy then
              begin
                x:=x-step;
                delta:=delta-dy;
              end;
            end;
          end;
        end;
end;

procedure TUniDIB.SetPalette(Pal:TLogPalette256);
var A:Byte;
    Colors:Array [0..255] of TRGBQuad;
begin
  If XUsage<>DIB_PAL_COLORS then
    Exit;
  For A:=0 to XClrCount-1 do
  begin
    Colors[A].rgbRed:=Pal.palEntry[A].peRed;
    Colors[A].rgbGreen:=Pal.palEntry[A].peGreen;
    Colors[A].rgbBlue:=Pal.palEntry[A].peBlue;
    Colors[A].rgbReserved:=Pal.palEntry[A].peFlags;
  end;
  Pal.palVersion:=$300;
  Pal.palNumEntries:=XClrCount;
  SelectPalette (FDC,XSelPalette,false);
  DeleteObject (FPalHandle);
  FPalHandle:=CreatePalette(PLogPalette(@Pal)^);
  SetDIBColorTable(FDC,0,XClrCount,Colors);
  XSelPalette:=SelectPalette (FDC,FPalHandle,False);
end;

procedure TUniDIB.FillPolygon(poly:array of TPoint; fillcol:Integer);
var
   loop1                   : integer;
   yval,ymax,ymin          : integer;
   yval0,yval1,yval2,yval3 : integer;
   ydifl,ydifr             : integer;
   xval0,xval1,xval2,xval3 : integer;
   xleft,xright            : integer;
   mu                      : integer;
   minvertex               : integer;
   vert0,vert1,vert2,vert3 : integer;
   n                       : integer; {number of points}
begin
  ymax:=-999999; ymin:=999999;
  { get top & bottom scan lines to work with }
  n := High(poly);
  minvertex:=0;
  for loop1:=0 to n-1 do
  begin
    yval:=poly[loop1].y;
    if yval>ymax then ymax:=yval;
    if yval<ymin then begin ymin:=yval; minvertex:=loop1; end;
  end;
  vert0 := minvertex;      vert1 :=(minvertex+1) mod n;
  vert2 := minvertex;      vert3 :=(minvertex-1) mod n;
  yval0 := poly[vert0].y; yval1 := poly[vert1].y;
  yval2 := poly[vert2].y; yval3 := poly[vert3].y;
  ydifl := yval1-yval0;    ydifr := yval3-yval2;
  xval0 := poly[vert0].x; xval1 := poly[vert1].x;
  xval2 := poly[vert2].x; xval3 := poly[vert3].x;

  for loop1:=ymin to ymax do
  begin
    {intersection on left hand side }
    mu:=(loop1-yval0);
    if mu>ydifl then
    begin
      vert0:=vert1; vert1:=(vert1+1) mod n;
      yval0 := poly[vert0].y; yval1 := poly[vert1].y;
      xval0 := poly[vert0].x; xval1 := poly[vert1].x;
      ydifl := yval1-yval0;
      mu:=(loop1-yval0)
    end;
    if ydifl<>0 then
        xleft:=xval0 - (mu*integer(xval0-xval1) div ydifl)
    else
        xleft:=xval0;

    {intersection on right hand side }
    if ydifr<>0 then
        mu:=(loop1-yval2)
    else
        mu:=ydifr;
    if mu>ydifr then
    begin
      vert2:=vert3; vert3:=(vert3-1) mod n;
      yval2 := poly[vert2].y; yval3 := poly[vert3].y;
      xval2 := poly[vert2].x; xval3 := poly[vert3].x;
      ydifr := yval3-yval2;
      if ydifr<>0 then
          mu:=(loop1-yval2)
      else
          mu:=ydifr;
    end;
    if ydifr<>0 then
        xright:=xval2 + (mu*integer(xval3-xval2) div ydifr)
    else
        xright:=xval2;
    DrawHorizLine(xleft,xright,loop1,fillcol);
  end;
end;

procedure TUniDIB.CaptureScreen;
var DC:HDC;
    A,B:Integer;
begin
{ DC:=GetDC(GetDesktopWindow);}
  DC:=GetDC(0);
  A:=FBMInfo.bmiHeader.biWidth;
  If GetDeviceCaps (DC,HORZRES)<A then
    A:=GetDeviceCaps (DC,HORZRES);
  B:=Abs(FBMInfo.bmiHeader.biHeight);
  If GetDeviceCaps (DC,VERTRES)<B then
    B:=GetDeviceCaps (DC,VERTRES);
  BitBlt(FDC,0,0,A,B,DC,0,0, SRCCOPY);
{  If GetDeviceCaps (DC,RASTERCAPS) AND RC_PALETTE>0 then
  begin
    A:=GetDeviceCaps (DC,NUMCOLORS);
    GetPaletteEntries (DC,0,A,LP.PalEntry);
    SetPalette (LP);
  end;}
  ReleaseDC(0,DC);
{  ReleaseDC(H,DC);}
end;

function TUniDIB.IndexColor: Boolean;
begin
  Result:=XUsage=DIB_PAL_COLORS;
end;

{$WARNINGS ON}

end.
