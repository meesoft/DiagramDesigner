unit DIBTools;
{TUniDIB - advanced functions}

{by Vit Kovalcik}
{Update 2005-10-02 by Michael Vinther: Stream support, removed PCX support}

interface

uses  Monitor,UniDIB,Classes,Windows,SysUtils;

{$R-}

const UDIBNoError=0;
      UDIBUndefError=1;
      UDIBFileOpenError=2;
      UDIBReadError=3;
      UDIBWriteError=4;
      UDIBBadFile=5;

const Std2ColPalette:Array [0..1] of TPaletteEntry=
   ((peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$ff;peGreen:$ff;peBlue:$ff));

const Std16ColPalette:Array [0..15] of TPaletteEntry=
   ((peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$aa),
    (peRed:$00;peGreen:$aa;peBlue:$00),
    (peRed:$00;peGreen:$aa;peBlue:$aa),
    (peRed:$aa;peGreen:$00;peBlue:$00),
    (peRed:$aa;peGreen:$00;peBlue:$aa),
    (peRed:$aa;peGreen:$55;peBlue:$00),
    (peRed:$aa;peGreen:$aa;peBlue:$aa),
    (peRed:$55;peGreen:$55;peBlue:$55),
    (peRed:$55;peGreen:$55;peBlue:$ff),
    (peRed:$55;peGreen:$ff;peBlue:$55),
    (peRed:$55;peGreen:$ff;peBlue:$ff),
    (peRed:$ff;peGreen:$55;peBlue:$55),
    (peRed:$ff;peGreen:$55;peBlue:$ff),
    (peRed:$ff;peGreen:$ff;peBlue:$55),
    (peRed:$ff;peGreen:$ff;peBlue:$ff));

const Std256ColPalette:Array [0..255] of TPaletteEntry=
   ({EGA Palette}
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$aa),
    (peRed:$00;peGreen:$aa;peBlue:$00),
    (peRed:$00;peGreen:$aa;peBlue:$aa),
    (peRed:$aa;peGreen:$00;peBlue:$00),
    (peRed:$aa;peGreen:$00;peBlue:$aa),
    (peRed:$aa;peGreen:$55;peBlue:$00),
    (peRed:$aa;peGreen:$aa;peBlue:$aa),
    (peRed:$55;peGreen:$55;peBlue:$55),
    (peRed:$55;peGreen:$55;peBlue:$ff),
    (peRed:$55;peGreen:$ff;peBlue:$55),
    (peRed:$55;peGreen:$ff;peBlue:$ff),
    (peRed:$ff;peGreen:$55;peBlue:$55),
    (peRed:$ff;peGreen:$55;peBlue:$ff),
    (peRed:$ff;peGreen:$ff;peBlue:$55),
    (peRed:$ff;peGreen:$ff;peBlue:$ff),
    {Grey palette}
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$14;peGreen:$14;peBlue:$14),
    (peRed:$20;peGreen:$20;peBlue:$20),
    (peRed:$2c;peGreen:$2c;peBlue:$2c),
    (peRed:$38;peGreen:$38;peBlue:$38),
    (peRed:$45;peGreen:$45;peBlue:$45),
    (peRed:$51;peGreen:$51;peBlue:$51),
    (peRed:$61;peGreen:$61;peBlue:$61),
    (peRed:$71;peGreen:$71;peBlue:$71),
    (peRed:$82;peGreen:$82;peBlue:$82),
    (peRed:$92;peGreen:$92;peBlue:$92),
    (peRed:$a2;peGreen:$a2;peBlue:$a2),
    (peRed:$b6;peGreen:$b6;peBlue:$b6),
    (peRed:$cb;peGreen:$cb;peBlue:$cb),
    (peRed:$e3;peGreen:$b3;peBlue:$b3),
    (peRed:$ff;peGreen:$ff;peBlue:$ff),
    {rest of palette}
    (peRed:$00;peGreen:$00;peBlue:$ff),
    (peRed:$41;peGreen:$00;peBlue:$ff),
    (peRed:$7d;peGreen:$00;peBlue:$ff),
    (peRed:$be;peGreen:$00;peBlue:$ff),
    (peRed:$ff;peGreen:$00;peBlue:$ff),
    (peRed:$ff;peGreen:$00;peBlue:$be),
    (peRed:$ff;peGreen:$00;peBlue:$7d),
    (peRed:$ff;peGreen:$00;peBlue:$41),
    (peRed:$ff;peGreen:$00;peBlue:$00),
    (peRed:$ff;peGreen:$41;peBlue:$00),
    (peRed:$ff;peGreen:$7d;peBlue:$00),
    (peRed:$ff;peGreen:$be;peBlue:$00),
    (peRed:$ff;peGreen:$ff;peBlue:$00),
    (peRed:$be;peGreen:$ff;peBlue:$00),
    (peRed:$7d;peGreen:$ff;peBlue:$00),
    (peRed:$41;peGreen:$ff;peBlue:$00), {16}
    (peRed:$00;peGreen:$ff;peBlue:$00),
    (peRed:$00;peGreen:$ff;peBlue:$41),
    (peRed:$00;peGreen:$ff;peBlue:$7d),
    (peRed:$00;peGreen:$ff;peBlue:$be),
    (peRed:$00;peGreen:$ff;peBlue:$ff),
    (peRed:$00;peGreen:$be;peBlue:$ff),
    (peRed:$00;peGreen:$7d;peBlue:$ff),
    (peRed:$00;peGreen:$41;peBlue:$ff),
    (peRed:$7d;peGreen:$7d;peBlue:$ff),
    (peRed:$9e;peGreen:$7d;peBlue:$ff),
    (peRed:$be;peGreen:$7d;peBlue:$ff),
    (peRed:$df;peGreen:$7d;peBlue:$ff),
    (peRed:$ff;peGreen:$7d;peBlue:$ff),
    (peRed:$ff;peGreen:$7d;peBlue:$df),
    (peRed:$ff;peGreen:$7d;peBlue:$be),
    (peRed:$ff;peGreen:$7d;peBlue:$9e), {32}
    (peRed:$ff;peGreen:$7d;peBlue:$7d),
    (peRed:$ff;peGreen:$9e;peBlue:$7d),
    (peRed:$ff;peGreen:$be;peBlue:$7d),
    (peRed:$ff;peGreen:$df;peBlue:$7d),
    (peRed:$ff;peGreen:$ff;peBlue:$7d),
    (peRed:$df;peGreen:$ff;peBlue:$7d),
    (peRed:$be;peGreen:$ff;peBlue:$7d),
    (peRed:$9e;peGreen:$ff;peBlue:$7d),
    (peRed:$7d;peGreen:$ff;peBlue:$7d),
    (peRed:$7d;peGreen:$ff;peBlue:$9e),
    (peRed:$7d;peGreen:$ff;peBlue:$be),
    (peRed:$7d;peGreen:$ff;peBlue:$df),
    (peRed:$7d;peGreen:$ff;peBlue:$ff),
    (peRed:$7d;peGreen:$df;peBlue:$ff),
    (peRed:$7d;peGreen:$be;peBlue:$ff),
    (peRed:$7d;peGreen:$9e;peBlue:$ff), {48}
    (peRed:$b6;peGreen:$b6;peBlue:$ff),
    (peRed:$c7;peGreen:$b6;peBlue:$ff),
    (peRed:$db;peGreen:$b6;peBlue:$ff),
    (peRed:$eb;peGreen:$b6;peBlue:$ff),
    (peRed:$ff;peGreen:$b6;peBlue:$ff),
    (peRed:$df;peGreen:$b6;peBlue:$eb),
    (peRed:$ff;peGreen:$b6;peBlue:$db),
    (peRed:$ff;peGreen:$b6;peBlue:$c7),
    (peRed:$ff;peGreen:$b6;peBlue:$b6),
    (peRed:$ff;peGreen:$c7;peBlue:$b6),
    (peRed:$ff;peGreen:$db;peBlue:$b6),
    (peRed:$ff;peGreen:$eb;peBlue:$b6),
    (peRed:$ff;peGreen:$ff;peBlue:$b6),
    (peRed:$eb;peGreen:$ff;peBlue:$b6),
    (peRed:$db;peGreen:$ff;peBlue:$b6),
    (peRed:$c7;peGreen:$ff;peBlue:$b6), {64}
    (peRed:$b6;peGreen:$df;peBlue:$b6),
    (peRed:$b6;peGreen:$ff;peBlue:$c7),
    (peRed:$b6;peGreen:$ff;peBlue:$db),
    (peRed:$b6;peGreen:$ff;peBlue:$eb),
    (peRed:$b6;peGreen:$ff;peBlue:$ff),
    (peRed:$b6;peGreen:$eb;peBlue:$ff),
    (peRed:$b6;peGreen:$db;peBlue:$ff),
    (peRed:$b6;peGreen:$c7;peBlue:$ff),
    (peRed:$00;peGreen:$00;peBlue:$71),
    (peRed:$1c;peGreen:$00;peBlue:$71),
    (peRed:$38;peGreen:$00;peBlue:$71),
    (peRed:$55;peGreen:$00;peBlue:$71),
    (peRed:$71;peGreen:$00;peBlue:$71),
    (peRed:$71;peGreen:$00;peBlue:$55),
    (peRed:$71;peGreen:$00;peBlue:$38),
    (peRed:$71;peGreen:$00;peBlue:$1c), {80}
    (peRed:$71;peGreen:$00;peBlue:$00),
    (peRed:$71;peGreen:$1c;peBlue:$00),
    (peRed:$71;peGreen:$38;peBlue:$00),
    (peRed:$71;peGreen:$55;peBlue:$00),
    (peRed:$71;peGreen:$71;peBlue:$00),
    (peRed:$55;peGreen:$71;peBlue:$00),
    (peRed:$38;peGreen:$71;peBlue:$00),
    (peRed:$1c;peGreen:$71;peBlue:$00),
    (peRed:$00;peGreen:$71;peBlue:$00),
    (peRed:$00;peGreen:$71;peBlue:$1c),
    (peRed:$00;peGreen:$71;peBlue:$38),
    (peRed:$00;peGreen:$71;peBlue:$55),
    (peRed:$00;peGreen:$71;peBlue:$71),
    (peRed:$00;peGreen:$55;peBlue:$71),
    (peRed:$00;peGreen:$38;peBlue:$71),
    (peRed:$00;peGreen:$1c;peBlue:$71), {96}
    (peRed:$38;peGreen:$38;peBlue:$71),
    (peRed:$45;peGreen:$38;peBlue:$71),
    (peRed:$55;peGreen:$38;peBlue:$71),
    (peRed:$61;peGreen:$38;peBlue:$71),
    (peRed:$71;peGreen:$38;peBlue:$71),
    (peRed:$71;peGreen:$38;peBlue:$61),
    (peRed:$71;peGreen:$38;peBlue:$55),
    (peRed:$71;peGreen:$38;peBlue:$45),
    (peRed:$71;peGreen:$38;peBlue:$38),
    (peRed:$71;peGreen:$45;peBlue:$38),
    (peRed:$71;peGreen:$55;peBlue:$38),
    (peRed:$71;peGreen:$61;peBlue:$38),
    (peRed:$71;peGreen:$71;peBlue:$38),
    (peRed:$61;peGreen:$71;peBlue:$38),
    (peRed:$55;peGreen:$71;peBlue:$38),
    (peRed:$45;peGreen:$71;peBlue:$38), {112}
    (peRed:$38;peGreen:$71;peBlue:$38),
    (peRed:$38;peGreen:$71;peBlue:$45),
    (peRed:$38;peGreen:$71;peBlue:$55),
    (peRed:$38;peGreen:$71;peBlue:$61),
    (peRed:$38;peGreen:$71;peBlue:$71),
    (peRed:$38;peGreen:$61;peBlue:$71),
    (peRed:$38;peGreen:$55;peBlue:$71),
    (peRed:$38;peGreen:$45;peBlue:$71),
    (peRed:$51;peGreen:$51;peBlue:$71),
    (peRed:$59;peGreen:$51;peBlue:$71),
    (peRed:$61;peGreen:$51;peBlue:$71),
    (peRed:$69;peGreen:$51;peBlue:$71),
    (peRed:$71;peGreen:$51;peBlue:$71),
    (peRed:$71;peGreen:$51;peBlue:$69),
    (peRed:$71;peGreen:$51;peBlue:$61),
    (peRed:$71;peGreen:$51;peBlue:$59), {128}
    (peRed:$71;peGreen:$51;peBlue:$51),
    (peRed:$71;peGreen:$59;peBlue:$51),
    (peRed:$71;peGreen:$61;peBlue:$51),
    (peRed:$71;peGreen:$69;peBlue:$51),
    (peRed:$71;peGreen:$71;peBlue:$51),
    (peRed:$69;peGreen:$71;peBlue:$51),
    (peRed:$61;peGreen:$71;peBlue:$51),
    (peRed:$59;peGreen:$71;peBlue:$51),
    (peRed:$51;peGreen:$71;peBlue:$51),
    (peRed:$51;peGreen:$71;peBlue:$59),
    (peRed:$51;peGreen:$71;peBlue:$61),
    (peRed:$51;peGreen:$71;peBlue:$69),
    (peRed:$51;peGreen:$71;peBlue:$71),
    (peRed:$51;peGreen:$69;peBlue:$71),
    (peRed:$51;peGreen:$61;peBlue:$71),
    (peRed:$51;peGreen:$59;peBlue:$71), {144}
    (peRed:$00;peGreen:$00;peBlue:$41),
    (peRed:$10;peGreen:$00;peBlue:$41),
    (peRed:$20;peGreen:$00;peBlue:$41),
    (peRed:$30;peGreen:$00;peBlue:$41),
    (peRed:$41;peGreen:$00;peBlue:$41),
    (peRed:$41;peGreen:$00;peBlue:$30),
    (peRed:$41;peGreen:$00;peBlue:$20),
    (peRed:$41;peGreen:$00;peBlue:$10),
    (peRed:$41;peGreen:$00;peBlue:$00),
    (peRed:$41;peGreen:$10;peBlue:$00),
    (peRed:$41;peGreen:$20;peBlue:$00),
    (peRed:$41;peGreen:$30;peBlue:$00),
    (peRed:$41;peGreen:$41;peBlue:$00),
    (peRed:$30;peGreen:$41;peBlue:$00),
    (peRed:$20;peGreen:$41;peBlue:$00),
    (peRed:$10;peGreen:$41;peBlue:$00), {160}
    (peRed:$00;peGreen:$41;peBlue:$00),
    (peRed:$00;peGreen:$41;peBlue:$10),
    (peRed:$00;peGreen:$41;peBlue:$20),
    (peRed:$00;peGreen:$41;peBlue:$30),
    (peRed:$00;peGreen:$41;peBlue:$41),
    (peRed:$00;peGreen:$30;peBlue:$41),
    (peRed:$00;peGreen:$20;peBlue:$41),
    (peRed:$00;peGreen:$10;peBlue:$41),
    (peRed:$20;peGreen:$20;peBlue:$41),
    (peRed:$28;peGreen:$20;peBlue:$41),
    (peRed:$30;peGreen:$20;peBlue:$41),
    (peRed:$38;peGreen:$20;peBlue:$41),
    (peRed:$41;peGreen:$20;peBlue:$41),
    (peRed:$41;peGreen:$20;peBlue:$38),
    (peRed:$41;peGreen:$20;peBlue:$30),
    (peRed:$41;peGreen:$20;peBlue:$28), {176}
    (peRed:$41;peGreen:$20;peBlue:$20),
    (peRed:$41;peGreen:$28;peBlue:$20),
    (peRed:$41;peGreen:$30;peBlue:$20),
    (peRed:$41;peGreen:$38;peBlue:$20),
    (peRed:$41;peGreen:$41;peBlue:$20),
    (peRed:$38;peGreen:$41;peBlue:$20),
    (peRed:$30;peGreen:$41;peBlue:$20),
    (peRed:$28;peGreen:$41;peBlue:$20),
    (peRed:$20;peGreen:$41;peBlue:$20),
    (peRed:$20;peGreen:$41;peBlue:$28),
    (peRed:$20;peGreen:$41;peBlue:$30),
    (peRed:$20;peGreen:$41;peBlue:$38),
    (peRed:$20;peGreen:$41;peBlue:$41),
    (peRed:$20;peGreen:$38;peBlue:$41),
    (peRed:$20;peGreen:$30;peBlue:$41),
    (peRed:$20;peGreen:$28;peBlue:$41),
    (peRed:$2c;peGreen:$2c;peBlue:$41),
    (peRed:$30;peGreen:$2c;peBlue:$41), {192}
    (peRed:$34;peGreen:$2c;peBlue:$41),
    (peRed:$3c;peGreen:$2c;peBlue:$41),
    (peRed:$41;peGreen:$2c;peBlue:$41),
    (peRed:$41;peGreen:$2c;peBlue:$3c),
    (peRed:$41;peGreen:$2c;peBlue:$34),
    (peRed:$41;peGreen:$2c;peBlue:$30),
    (peRed:$41;peGreen:$2c;peBlue:$2c),
    (peRed:$41;peGreen:$30;peBlue:$2c),
    (peRed:$41;peGreen:$34;peBlue:$2c),
    (peRed:$41;peGreen:$3c;peBlue:$2c),
    (peRed:$41;peGreen:$41;peBlue:$2c),
    (peRed:$3c;peGreen:$41;peBlue:$2c),
    (peRed:$34;peGreen:$41;peBlue:$2c),
    (peRed:$30;peGreen:$41;peBlue:$2c),
    (peRed:$2c;peGreen:$41;peBlue:$2c),
    (peRed:$2c;peGreen:$41;peBlue:$30), {208}
    (peRed:$2c;peGreen:$41;peBlue:$34),
    (peRed:$2c;peGreen:$41;peBlue:$3c),
    (peRed:$2c;peGreen:$41;peBlue:$41),
    (peRed:$2c;peGreen:$3c;peBlue:$41),
    (peRed:$2c;peGreen:$34;peBlue:$41),
    (peRed:$2c;peGreen:$30;peBlue:$41),
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$00),
    (peRed:$00;peGreen:$00;peBlue:$00)  {224}
    );

function UDIBLoadBMP (Stream: TStream; var ADIB:TUniDIB):Integer;

var UpsideDownDIB:Boolean;
  {You can set this to True if you want to load file and
   have bitmap with point [0,0] in upper left corner}

implementation

{$WARNINGS OFF}

{-------------}
{TBufferedFile}
{-------------}

const MinBufferSize=65536;
      VeryMuch=1000000000;

type TBuffer=Array [0..VeryMuch-1] of Byte;
     PBuffer=^TBuffer;
     TOpenMode=(omRead,omWrite);

type TBufferedFile=class(TMonitorObject)
     protected
       FOffset:Int64;
       FPos:Integer;
       FOpenMode:TOpenMode;
       FBufSize:Integer;
       FFilePos:Integer;
       FMaxPos:Integer;
       procedure SetBufSize (Value:Integer);
       function GetFilePos:Integer;
     public
       Str:TStream;
       Buffer:PBuffer;
       property Pos:Integer read FPos write FPos; {position of next read byte}
       property BufSize:Integer read FBufSize write SetBufSize;
       property FilePos:Integer read GetFilePos;
       property MaxPos:Integer read FMaxPos;
       procedure PreReadBytes (ACount:Integer);
         {This function makes sure that next <ACount> bytes are in buffer}
         {Count must be lower than or equal to BufSize !!!}
       procedure ReadFromPos (APos:Integer);
       function ReadByte:byte;
       constructor Create (Stream: TStream);
       destructor Destroy; override;
     end;

procedure TBufferedFile.SetBufSize (Value:Integer);
var A:Integer;
begin
  If Value<MinBufferSize then
    A:=MinBufferSize
  else
    A:=Value;
  If (A>0) AND (FBufSize>0) AND
     ((FMaxPos=-1) OR (FMaxPos-FPos+1<=A)) then
  begin
    If (FPos>0) then
    begin
      Move (Buffer[FPos],Buffer[0],FMaxPos-FPos+1);
      Dec (FMaxPos,FPos);
      FPos:=0;
    end;
    ReallocMem (Buffer,A);
    FBufSize:=A;
  end;
end;

constructor TBufferedFile.Create (Stream: TStream);
begin
  inherited Create;
  Buffer:=nil;
  Str:=Stream;
  FOffset:=Str.Position;
  GetMem (Buffer,MinBufferSize);
  FBufSize:=MinBufferSize;
  FMaxPos:=-1;
  FPos:=0;
end;

procedure TBufferedFile.PreReadBytes (ACount:Integer);
begin
  If (ACount>0) AND (FMaxPos-FPos+1<ACount) AND
     (BufSize-FMaxPos+FPos>ACount) AND (BufSize>=ACount) AND
     (Str.Position<Str.Size) then
  begin
    If FPos>FMaxPos then
    begin
      FPos:=0;
      FMaxPos:=-1;
    end;
    If FPos>0 then
    begin
      Move (Buffer[FPos],Buffer[0],FMaxPos-FPos+1);
      Dec (FMaxPos,FPos);
    end;
    If FMaxPos>=0 then
      Inc (FMaxPos,Str.Read (Buffer[FMaxPos+1],BufSize-FMaxPos-1))
    else
      FMaxPos:=Str.Read (Buffer^,BufSize)-1;
    FPos:=0;
  end;
end;

destructor TBufferedFile.Destroy;
begin
  ReallocMem (Buffer,0);
  inherited Destroy;
end;

procedure TBufferedFile.ReadFromPos (APos:Integer);
begin
  FMaxPos:=-1;
  FPos:=0;
  Str.Seek (FOffset+APos,soFromBeginning);
  PreReadBytes (FMaxPos+1);
end;

function TBufferedFile.ReadByte:byte;
begin
  If FPos>FMaxPos then
    PreReadBytes (FBufSize);
  Result:=Buffer[FPos];
  Inc (FPos);
end;

function TBufferedFile.GetFilePos:Integer;
begin
  Result:=Str.Position-FOffset-FMaxPos+FPos;
end;

{------------}
{UniDIB Tools}
{------------}

function UDIBLoadBMP (Stream: TStream; var ADIB:TUniDIB):Integer;
var BFile:TBufferedFile;
    Pal:TLogPalette256;
    BmpOffset:Integer;
    A,B,C:Integer;
    Width,Height:Integer;
    ByteWidth:Integer;
    Planes,Bits:Integer;
    W,Z:Word;
    Compr:Integer;
    PalEn:Integer;
    Rev:Boolean;

  procedure PixelSeek (X,Y:Integer);
  begin
    If Rev then
      ADIB.Seek (X,Height-Y-1)
    else
      ADIB.Seek (X,Y);
  end;

begin
  If ADIB<>nil then
    ADIB.Free;
  ADIB:=nil;
  BFile:=nil;
  try
    BFile:=TBufferedFile.Create (Stream);
  except
    Result:=UDIBFileOpenError;
    BFile.Free;
    Exit;
  end;
  try
    BFile.PreReadBytes (78); {Max. size of BitmapHeader + BitmapInfoHeader
                              (This size is used in OS/2 Bitmap version 2.x)}
    Move (BFile.Buffer[0],W,2);
    Case W of
      0:{Microsoft Windows Bitmap version 1 (Windows 1.x and 2.x)}
        begin
          Width:=BFile.Buffer[2]+BFile.Buffer[3] shl 8;
          Height:=BFile.Buffer[4]+BFile.Buffer[5] shl 8;
          ByteWidth:=BFile.Buffer[6]+BFile.Buffer[7] shl 8;
          Planes:=BFile.Buffer[8];
          Bits:=BFile.Buffer[9];
          BFile.Pos:=10;
          BmpOffset:=10;
          Compr:=0;
          If UpsideDownDIB then
            B:=-Abs(Height)
          else
            B:=Abs(Height);
          ADIB:=TUniDIB.Create (Width,B,Bits,SBU_NONE);
          Rev:=A<>Height;          
          Case Bits of
            8:Move (Std256ColPalette,Pal.palEntry[0],SizeOf(TPaletteEntry) shl 4);
            4:Move (Std16ColPalette,Pal.palEntry[0],SizeOf(TPaletteEntry) shl 4);
            1:Move (Std2ColPalette,Pal.palEntry[0],SizeOf(TPaletteEntry) shl 4);
          end;
          If Bits<=8 then
            ADIB.SetPalette (Pal);
        end;
      Ord('B')+Ord('M') shl 8:
        {Microsoft Windows Bitmap version 3 (Windows 3.x and higher)
         or OS/2 Bitmap version 1.x or 2.x}
        begin
          Move (BFile.Buffer[10],BmpOffset,4);
          Move (BFile.Buffer[14],A,4); {Header size}
          Case A of
            12,64: {OS/2 Bitmap version 1.x or 2.x}
              begin
                Width:=BFile.Buffer[18]+BFile.Buffer[19] shl 8;
                Height:=BFile.Buffer[20]+BFile.Buffer[21] shl 8;
                Planes:=BFile.Buffer[22]+BFile.Buffer[23] shl 8;
                Bits:=BFile.Buffer[24]+BFile.Buffer[25] shl 8;
                If A=64 then
                begin
                  Move (BFile.Buffer[26],Compr,4);
                  Move (BFile.Buffer[30],PalEn,4);
                end
                else
                begin
                  Compr:=0;
                  PalEn:=0;
                end;
              end;
            40: {Microsoft Windows Bitmap version 3}
              begin
                Move (BFile.Buffer[18],Width,4);
                Move (BFile.Buffer[22],Height,4);
                Planes:=BFile.Buffer[26]+BFile.Buffer[27] shl 8;
                Bits:=BFile.Buffer[28]+BFile.Buffer[29] shl 8;
                Move (BFile.Buffer[30],Compr,4);
                Move (BFile.Buffer[46],PalEn,4);
              end;
            else
              {Bad file or new version}
              begin
                Result:=UDIBBadFile;
                BFile.Free;
                Exit;
              end;
          end; {Case A of}
          BFile.Pos:=14+A;
          If PalEn=0 then
            PalEn:=1 SHL Bits;
          ByteWidth:=((Width*Bits+31) shr 5)shl 2;
          If UpsideDownDIB then
            A:=-Abs(Height)
          else
            A:=Abs(Height);
          ADIB:=TUniDIB.Create (Width,A,Bits,SBU_NONE);
          Rev:=A<>Height;
          If Bits<=8 then
            {palette present}
          begin
            B:=BFile.Pos+4*PalEn;
            For A:=0 to PalEn-1 do
            begin
              Pal.palEntry[A].peBlue:=BFile.ReadByte;
              Pal.palEntry[A].peGreen:=BFile.ReadByte;
              Pal.palEntry[A].peRed:=BFile.ReadByte;
              If BmpOffset<B then
              {OS/2 Bitmap has different palette}
                Pal.palEntry[A].peFlags:=0
              else
                Pal.palEntry[A].peFlags:=BFile.ReadByte;
            end;
            ADIB.SetPalette (Pal);
          end;
        end; {W='BM'}
      else
      {Bad file}
      begin
        Result:=UDIBBadFile;
        BFile.Free;
        Exit;
      end;
    end; {Case}
    If Planes<>1 then
    {Bad file or maybe new version}
    begin
      Result:=UDIBBadFile;
      BFile.Free;
      ADIB.Free;
      ADIB:=nil;
      Exit;
    end;
    If BmpOffset<BFile.MaxPos then
      BFile.Pos:=BmpOffset
    else
      BFile.ReadFromPos (BmpOffset);
    BFile.BufSize:=ByteWidth*3+10;
    Height:=Abs(Height);
    Case Compr of
      0, {No compression}
      3: {BITFIELDS - I don't know much about it yet}
        begin
          A:=0;
          while A<Height do
          begin
            PixelSeek (0,A);
            BFile.PreReadBytes (ByteWidth+3);
            Move (BFile.Buffer[BFile.Pos],ADIB.ActPointer^,ByteWidth);
            BFile.Pos:=BFile.Pos+((ByteWidth+3) and not 3);
            Inc (A);
          end;
        end;
      1: {RLE8 compression}
        begin
          A:=0;
          PixelSeek (0,A);
          while A<Height do
          begin
            W:=BFile.ReadByte;
            If W=0 then
            begin
              W:=BFile.ReadByte;
              Case W of
                0:{End of line}
                  begin
                    Inc (A);
                    PixelSeek (0,A);
                  end;
                1:{End of bitmap}
                  Break;
                2:{Delta}
                  begin
                    W:=BFile.ReadByte;
                    Inc (A,BFile.ReadByte);
                    PixelSeek (W,A);
                  end;
                else
                begin
                  BFile.PreReadBytes (W+1);
                  Move (BFile.Buffer[BFile.Pos],ADIB.ActPointer^,W);
                  BFile.Pos:=BFile.Pos+((W+1) and not 1);
                  ADIB.ActPointer:=Pointer(Integer(ADIB.ActPointer)+W);
                end;
              end; {Case W of}
            end {If W=0}
            else
              {consecutive pixels}
            begin
              B:=BFile.ReadByte;
              FillChar (ADIB.ActPointer^,W,B);
              ADIB.ActPointer:=Pointer(Integer(ADIB.ActPointer)+W);
            end;
          end;
        end;
      2: {RLE4 compression}
        begin
          A:=0;
          PixelSeek (0,0);
          while A<Height do
          begin
            W:=BFile.ReadByte;
            If W=0 then
            begin
              W:=BFile.ReadByte;
              Case W of
                0:{End of line}
                  begin
                    Inc (A);
                    PixelSeek (0,A);
                  end;
                1:{End of bitmap}
                  Break;
                2:{Delta}
                  begin
                    W:=BFile.ReadByte;
                    Inc (A,BFile.ReadByte);
                    PixelSeek (W,A);
                  end;
                else
                begin
                  For Z:=1 to W do
                  begin
                    If Z and 1=1 then
                    begin
                      B:=BFile.ReadByte;
                      C:=B and $F;
                      B:=B shr 4;
                      ADIB.SetSeqPixel (B);
                    end
                    else
                      ADIB.SetSeqPixel (C);
                  end;
                end;
                If (W shr 1) and 1=1 then
                  BFile.ReadByte;
              end; {Case W of}
            end {If W=0}
            else
              {consecutive pixels}
            begin
              B:=BFile.ReadByte;
              C:=B and $F;
              B:=B shr 4;
              For Z:=1 to W do
                If Z and 1=1 then
                  ADIB.SetSeqPixel (B)
                else
                  ADIB.SetSeqPixel (C);
            end;
          end;
        end;
      else
        {Bad file or unknown compression}
        begin
          Result:=UDIBBadFile;
          BFile.Free;
          ADIB.Free;
          ADIB:=nil;
          Exit;
        end;
    end; {Case}
    BFile.Free;
    Result:=UDIBNoError;
  except
    on EReadError do
      Result:=UDIBReadError
    else
      Result:=UDIBUndefError;
    ADIB.Free;
    ADIB:=nil;
    BFile.Free;
  end;
end;

{$WARNINGS ON}

initialization
  UpsideDownDIB:=False;
end.
