////////////////////////////////////////////////////////////////////////////////
//
// CompLZ77.pas - LZ77 compression unit
// ------------------------------------
// Changed:   2003-03-09
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Contains:
//   (TStreamClass)
//      TLZ77StreamClass
//   (TFilterStream)
//      TLZ77Stream
//      TLZ77Stream2
//  See below for description
//
//  Changes to the compression parameters will only affect new streams created
//  after the change.
//
// Last changes:
//   LZ77 compression moved to TLZ77StreamClass from StreamUtils unit

unit CompLZ77;

interface

uses Streams, BufStream, SysUtils, StreamUtils;

resourcestring
 rs_BadCompFormat = 'Bad compression parameters';
 rs_WriteDenied   = 'Stream not open for write';

var
 MaxLookAheadBits   : Integer = 4;   // LZ77Stream:  3-4 bits, default must be 4
                                     // LZ77Stream2: 3-5 bits

 LookBackBufferSize : Integer = 0;   // LZ77Stream2: Max 2^(16-MaxLookAheadBits) bytes, 0=max

type
  // TLZ77Stream
  //
  // 256 or 512 bytes look back and 8 or 16 bytes look ahead
  //
  // Compression format:
  //
  // If MaxLookAheadBits = 4 (16 bytes look ahead) and LookBackBufferSize = 256:
  // if A>0: A+1      = Numbers of chars to be copied from look back buffer
  //         IHi+ILo  = Index in look back buffer
  // else:   IHi+1    = Number of following uncompressed bytes
  //
  //                LSB                         MSB
  // Startcode     | A | A | A | A |IHi|IHi|IHi|IHi|
  //
  // if A>0:       |ILo|ILo|ILo|ILo|
  //
  // else:         (IHi+1) * Ucompressed byte
  TLZ77Stream = class(TFilterStream)
                 private
                  LookAheadBits, LookAheadSize, LookBackSize, NotCompSize : Integer;

                  LookAhead : array[0..15] of Byte;
                  NotComp   : array[0..31] of Byte;
                  LALen, NotCompLen, LookBackPos : Integer;

                  CodeCheck : Byte;
                  NewBlock : Boolean;

                  FilByte : Byte;
                  FilBits : Integer;

                  function Process: Boolean;
                  function WriteBits(Str: Byte; Count: Integer): Boolean;
                  function ReadBits(var Str: Byte; Count: Integer): Boolean;

                 public
                  { The look back buffer is filled by #0's by default. When compressing
                  fx text files, you might achive better compression by filling it with
                  #32's. Changes to LookBack should be made before the first call to
                  Read or Write}
                  LookBack : PByteArray;

                  constructor Create(NextStream: TBaseStream);
                  destructor Destroy; override;

                  function Write(var Buf; Count: Integer): Integer; override;
                  function Read(var Buf; Count: Integer): Integer; override;

                  // Fill look back buffer with zeros
                  procedure ResetLookBack; virtual;

                  // Write data in look-ahead and reset 1-byte read/write buffer
                  procedure Flush; override;

                  // Return value is >0 if there is available data
                  function Available: integer; override;
                end;



 // TLZ77Stream2
 //
 // 3 bytes to 8kb look back and 9, 17 or 33 bytes look ahead
 //
 // Compression format:
 //
 // If MaxLookAheadBits = 5 (33 bytes look-ahead) and LookBackBufferSize =2048:
 // if A>0: A+2      = Numbers of chars to be copied from look back buffer
 //         IHi+ILo  = Index in look back buffer
 // else:   IHi+1    = Number of following uncompressed bytes
 //
 //                LSB                         MSB
 // Startcode     | A | A | A | A | A |IHi|IHi|IHi|
 //
 // if A>0:       |ILo|ILo|ILo|ILo|ILo|ILo|ILo|ILo|
 //
 // else:         (IHi+1) * Ucompressed byte
 TLZ77Stream2 = class(TFilterStream)
                private
                 LookAheadBits, LookAheadSize, LookBackSize, NotCompSize : Integer;

                 LookAhead : array[0..33] of Byte;
                 NotComp   : array[0..31] of Byte;
                 LALen, NotCompLen, LookBackPos : Integer;

                 CodeCheck : Byte;
                 NewBlock : Boolean;

                 function Process: Boolean;

                public
                 { The look back buffer is filled by #0's by default. When compressing
                 fx text files, you might achive better compression by filling it with
                 #32's. Changes to LookBack should be made before the first call to
                 Read or Write}
                 LookBack : PByteArray;

                 constructor Create(NextStream: TBaseStream);
                 destructor Destroy; override;

                 function Write(var Buf; Count: Integer): Integer; override;
                 function Read(var Buf; Count: Integer): Integer; override;

                 // Fill look back buffer with zeros
                 procedure ResetLookBack; virtual;

                 // Write data in look-ahead
                 procedure Flush; override;

                 // Return value is >0 if there is available data
                 function Available: integer; override;
               end;

  TLZ77StreamClass = class(TStreamClass)
    public
      procedure SaveToCompressedFile(const FileName: string);
      procedure LoadFromCompressedFile(const FileName: string);
    end;

 EBadFormat = class(EInOutError);

// Open LZ77 compressed, buffered file. Makes it unnessacary to include other stream units
 const
  fmRead  = 0;
  fmWrite = 1;
 function OpenLZ77File(Name: string; Mode: Integer): TLZ77Stream;

implementation

uses MemUtils;

//=======================================================================================================
//                TLZ77Stream
//=======================================================================================================

//-----------------------------------------------------------------------------------
// Create a new LZ77 stream.
 constructor TLZ77Stream.Create;
 begin
  inherited Create(NextStream);
  if (MaxLookAheadBits<3) or (MaxLookAheadBits>4) then raise EBadFormat.Create( rs_BadCompFormat );

  LookAheadBits:=MaxLookAheadBits;
  if Next.CanWrite then
  begin
   fCanWrite:=True;
   NotCompSize:=1 shl (8-LookAheadBits);
   LookAheadSize:=1 shl LookAheadBits;
   LALen:=0; NotCompLen:=0;
   FilByte:=0; FilBits:=0;
  end
  else if Next.CanRead then
  begin
   fCanRead:=True;
   CodeCheck:=1 shl LookAheadBits-1;
   NewBlock:=True;
  end;
  LookBackSize:=16 shl (8-LookAheadBits);
  LookBackPos:=0;
  GetMem(LookBack,LookBackSize);
  ResetLookBack;
 end;

//-----------------------------------------------------------------------------------
// Flush and free memory
 destructor TLZ77Stream.Destroy;
 begin
  Flush;
  FreeMem(LookBack);
  inherited;
 end;

//-----------------------------------------------------------------------------------

 procedure TLZ77Stream.ResetLookBack;
 begin
   //FillChar(LookBack^,LookBackSize,0);
   ZeroMem(LookBack^,LookBackSize);
 end;

//-----------------------------------------------------------------------------------
                                           { 4/8 }
 function TLZ77Stream.WriteBits(Str: Byte; Count: Integer): Boolean;
 begin
  FilByte:=FilByte or (Str shl FilBits);
  Inc(FilBits,Count);
  if FilBits=8 then
  begin
   Result:=Next.Write(FilByte,1)=1;
   FilBits:=0; FilByte:=0;
  end
  else if FilBits=12 then
  begin
   Result:=Next.Write(FilByte,1)=1;
   FilBits:=4;
   FilByte:=Str shr 4;
  end
  else Result:=True;
 end;
                                              { 4/8 }
 function TLZ77Stream.ReadBits(var Str: Byte; Count: Integer): Boolean;
 begin
  if FilBits=0 then
  begin
   if Count=8 then
   begin
    Result:=Next.Read(Str,1)=1;
    Exit;
   end;
   Result:=Next.Read(FilByte,1)=1;
   FilBits:=8;
  end
  else Result:=True;
  if Count=8 then {4+4}
  begin
   Str:=FilByte shr 4;
   Result:=Next.Read(FilByte,1)=1;
   Str:=Str or (FilByte shl 4);
   FilBits:=4;
  end
  else  {Count=4}
  begin
   Dec(FilBits,4);
   if FilBits=0 then Str:=FilByte shr 4
   else Str:=FilByte and $f;
  end;
 end;
 {$WARNINGS OFF}
 function TLZ77Stream.Process: Boolean;
 var
  LBPos, CurP, Found, F, Best : Integer;
  BestP : Word;
  OutB : Byte;
 begin
  {Find LookAhead in LookBack^}
  Best:=1;
  LBPos:=FastLocateByte(LookBack^,0,LookBackSize,LookAhead[0]);
  while LBPos<>-1 do
  begin {If first char of LookAhead is found}
   CurP:=LBPos;
   Found:=LALen-1;
   for F:=1 to Found do {How much of LookAhead exists after LBPos?}
   begin
    Inc(CurP); if CurP=LookBackSize then CurP:=0;
    if LookBack^[CurP]<>LookAhead[F] then
    begin
     Found:=F;
     Break;
    end;
   end;
   if Found>Best then {If this is the best string so far}
   begin
    Best:=Found; BestP:=LBPos;
   end;
   LBPos:=FastLocateByte(LookBack^,LBPos+1,LookBackSize,LookAhead[0]);
  end;

  if Best>1 then {If it pays off to save length+index}
  begin
   if NotCompLen<>0 then {Write uncompressed block}
   begin
    OutB:=(NotCompLen-1) shl LookAheadBits; WriteBits(OutB,8);
    for CurP:=0 to NotCompLen-1 do WriteBits(NotComp[CurP],8);
    NotCompLen:=0;
   end;
   OutB:=(Best-1) or ((BestP shr 4) shl LookAheadBits);
   WriteBits(OutB,8);
   Result:=WriteBits(BestP and $f,4);
   {Move Best bytes from LookAhead to LookBack^}
   for CurP:=0 to Best-1 do
   begin
    Inc(LookBackPos); if LookBackPos=LookBackSize then LookBackPos:=0;
    LookBack^[LookBackPos]:=LookAhead[CurP];
   end;
   Dec(LALen,Best); Move(LookAhead[Best],LookAhead,LALen);
  end
  else {Not compressable}
  begin
   if NotCompLen=NotCompSize then {Write previous uncompressed block}
   begin
    OutB:=(NotCompLen-1) shl LookAheadBits; WriteBits(OutB,8);
    for CurP:=0 to NotCompLen-1 do Result:=WriteBits(NotComp[CurP],8);
    NotCompLen:=0;
   end
   else Result:=True;
   NotComp[NotCompLen]:=LookAhead[0];
   Inc(NotCompLen);
   {Move 1 byte from LookAhead to LookBack^}
   Inc(LookBackPos); if LookBackPos=LookBackSize then LookBackPos:=0;
   LookBack^[LookBackPos]:=LookAhead[0];
   Dec(LALen); Move(LookAhead[1],LookAhead,LALen);
  end;
 end;
 {$WARNINGS ON}

 function TLZ77Stream.Write(var Buf; Count: Integer): Integer;
 var Get : Integer;
 begin
  if not fCanWrite then raise EInOutError.Create( rs_WriteDenied );
  Result:=0;
  while Result<Count do
  begin
   Get:=LookAheadSize-LALen;
   if Count-Result<Get then Get:=Count-Result;
   Move(TByteArray(Buf)[Result],LookAhead[LALen],Get); Inc(LALen,Get); {Fill look ahead buffer}

   if LALen=LookAheadSize then
   begin
    if not Process then Exit;
   end;
   Inc(Result,Get);
  end;
 end;

 function TLZ77Stream.Read(var Buf; Count: Integer): Integer;
 var
  LBPos : Word;
  CurP, Get : Integer;
  StartCode : Byte;
 begin
  if not fCanRead then raise EInOutError.Create( rs_ReadDenied );
  Result:=0;
  while Result<Count do
  begin
   if LALen=0 then
   begin
    ReadBits(StartCode,8);
    LALen:=StartCode and CodeCheck;
    if LALen=0 then {Not compressed block}
    begin
     if FilBits=0 then LALen:=next.Read(NotComp,StartCode shr LookAheadBits+1) else
     for CurP:=0 to StartCode shr LookAheadBits do
       if ReadBits(NotComp[CurP],8) then Inc(LALen);
     if LALen=0 then Exit;
    end
    else
    begin
     Inc(LALen);
     LBPos:=0;
     if not ReadBits(Byte(Pointer(@LBPos)^),4) then begin LALen:=0; Exit; end;
     LBPos:=LBPos or ((StartCode shr LookAheadBits) shl 4);
     for CurP:=0 to LALen-1 do {Copy from look back to NotComp}
     begin
      NotComp[CurP]:=LookBack^[LBpos];
      Inc(LBPos); if LBPos=LookBackSize then LBPos:=0;
     end;
    end;
   end;

   Get:=Count-Result;
   if Get>LALen then Get:=LALen;
   Move(NotComp,TByteArray(Buf)[Result],Get); Inc(Result,Get);
   {Move Get bytes from NotComp to LookBack^}
   for CurP:=0 to Get-1 do
   begin
    Inc(LookBackPos); if LookBackPos=LookBackSize then LookBackPos:=0;
    LookBack^[LookBackPos]:=NotComp[CurP];
   end;
   Dec(LALen,Get); Move(NotComp[Get],NotComp,LALen);
  end;
 end;

//-----------------------------------------------------------------------------------
// Write data in look-ahead and reset 1-byte read/write buffer
 procedure TLZ77Stream.Flush;
 var
  OutB : Byte;
  CurP : Integer;
 begin
  if fCanWrite then
  begin
   while LALen<>0 do if not Process then raise EInOutError.CreateFmt( rs_WriteError, [''] );
   if NotCompLen<>0 then {Write uncompressed block}
   begin
    OutB:=(NotCompLen-1) shl LookAheadBits; WriteBits(OutB,8);
    for CurP:=0 to NotCompLen-1 do WriteBits(NotComp[CurP],8);
    NotCompLen:=0;
   end;
   if (FilBits<>0) and (Next.Write(FilByte,1)<>1) then raise EInOutError.CreateFmt( rs_WriteError, [''] );
  end;
  FilBits:=0;
 end;

//-----------------------------------------------------------------------------------
// Return value is >0 if there is available data
 function TLZ77Stream.Available;
 begin
  Available:=Next.Available+LALen;
 end;


//-----------------------------------------------------------------------------------
// Open LZ77 compressed, buffered file
 function OpenLZ77File(Name: string; Mode: Integer): TLZ77Stream;
 begin
   if Mode=fmRead then Result:=TLZ77Stream.Create(TBufferedStream.Create(-1,0,TFileStream.Create(Name,[fsRead,fsShareRead])))
   else Result:=TLZ77Stream.Create(TBufferedStream.Create(0,-1,TFileStream.Create(Name,fsRewrite+[fsShareRead])));
 end;


//=======================================================================================================
//                TLZ77Stream2
//=======================================================================================================

//-----------------------------------------------------------------------------------
// Create a new LZ77 stream.
 constructor TLZ77Stream2.Create;
 begin
  inherited Create(NextStream);

  if LookBackBufferSize=0 then LookBackSize:=256 shl (8-LookAheadBits)
  else LookBackSize:=LookBackBufferSize;

  if (MaxLookAheadBits<3) or (MaxLookAheadBits>5) or
     (LookBackSize<3) or (LookBackSize>256 shl (8-LookAheadBits))
     then raise EBadFormat.Create( rs_BadCompFormat );

  LookAheadBits:=MaxLookAheadBits;
  if Next.CanWrite then
  begin
   fCanWrite:=True;
   NotCompSize:=1 shl (8-LookAheadBits);
   LookAheadSize:=1 shl LookAheadBits+1;
   LALen:=0; NotCompLen:=0;
  end
  else if Next.CanRead then
  begin
   fCanRead:=True;
   CodeCheck:=1 shl LookAheadBits-1;
   NewBlock:=True;
  end;
  if LookBackBufferSize=0 then LookBackSize:=256 shl (8-LookAheadBits)
  else LookBackSize:=LookBackBufferSize;
  LookBackPos:=0; //LookBackSize-1;
  GetMem(LookBack,LookBackSize);
  ResetLookBack;
 end;

//-----------------------------------------------------------------------------------
// Flush and free memory
 destructor TLZ77Stream2.Destroy;
 begin
  Flush;
  if LookBack<>nil then FreeMem(LookBack);
  inherited;
 end;

//-----------------------------------------------------------------------------------

 procedure TLZ77Stream2.ResetLookBack;
 begin
   //FillChar(LookBack^,LookBackSize,0);
   ZeroMem(LookBack^,LookBackSize);
 end;

//-----------------------------------------------------------------------------------

 function TLZ77Stream2.Process: Boolean;

 var
  LBPos, CurP, F, Found, Best : Integer;
  BestP : Word;
  OutB : Byte;
 begin
  {Find LookAhead in LookBack^}
  Best:=2;
//  LBPos:=FastLocate(LookBack^,0,LookBackSize,LookAhead[0]);
  LBPos:=FastLocate2Bytes(LookBack^,0,LookBackSize-1,TWordArray(Pointer(@LookAhead)^)[0]);
  while LBPos<>-1 do
  begin {If first 2 bytes of LookAhead is found}
   CurP:=LBPos+1;
   Found:=LALen-1;
   for F:=2 to Found do {How much of LookAhead exists after LBPos?}
   begin
    Inc(CurP); if CurP=LookBackSize then CurP:=0;
    if LookBack^[CurP]<>LookAhead[F] then
    begin
     Found:=F;
     Break;
    end;
   end;
   if Found>Best then {If this is the best string so far}
   begin
    Best:=Found; BestP:=LBPos;
   end;
//   LBPos:=FastLocate(LookBack^,LBPos+1,LookBackSize,LookAhead[0]);
   LBPos:=FastLocate2Bytes(LookBack^,LBPos+1,LookBackSize-1,TWordArray(Pointer(@LookAhead)^)[0]);
  end;

  if Best>2 then {If it pays off to save length+index}
  begin
   if NotCompLen<>0 then {Write uncompressed block}
   begin
    OutB:=(NotCompLen-1) shl LookAheadBits; Next.Write(OutB,1);
    Next.Write(NotComp,NotCompLen);
    NotCompLen:=0;
   end;
   OutB:=(Best-2) or (Hi(BestP) shl LookAheadBits);
   Next.Write(OutB,1);
   Result:=Next.Write(BestP,1)=1;
   {Move Best bytes from LookAhead to LookBack^}
   for CurP:=0 to Best-1 do
   begin
    Inc(LookBackPos); if LookBackPos=LookBackSize then LookBackPos:=0;
    LookBack^[LookBackPos]:=LookAhead[CurP];
   end;
   Dec(LALen,Best); Move(LookAhead[Best],LookAhead,LALen);
  end
  else
  begin
   if NotCompLen=NotCompSize then {Write previous uncompressed block}
   begin
    OutB:=(NotCompLen-1) shl LookAheadBits; Next.Write(OutB,1);
    Result:=Next.Write(NotComp,NotCompLen)=NotCompLen;
    NotCompLen:=0;
   end
   else Result:=True;
   NotComp[NotCompLen]:=LookAhead[0];
   Inc(NotCompLen);
   {Move 1 byte from LookAhead to LookBack^}
   Inc(LookBackPos); if LookBackPos=LookBackSize then LookBackPos:=0;
   LookBack^[LookBackPos]:=LookAhead[0];
   Dec(LALen); Move(LookAhead[1],LookAhead,LALen);
  end;
 end;

 function TLZ77Stream2.Write(var Buf; Count: Integer): Integer;
 var Get : Integer;
 begin
  if not fCanWrite then raise EInOutError.CreateFmt( rs_WriteError, [rs_WriteDenied] );
  Result:=0;
  while Result<Count do
  begin
   Get:=LookAheadSize-LALen;
   if Count-Result<Get then Get:=Count-Result;
   Move(TByteArray(Buf)[Result],LookAhead[LALen],Get); Inc(LALen,Get); {Fill look ahead buffer}
   if LALen=LookAheadSize then
   begin
    if not Process then Exit;
   end;
   Inc(Result,Get);
  end;
 end;

 function TLZ77Stream2.Read(var Buf; Count: Integer): Integer;
 var
  LBPos : Word;
  CurP, Get : Integer;
  StartCode : Byte;
 begin
  if not fCanRead then raise EInOutError.CreateFmt(rs_ReadError,[rs_ReadDenied]);
  Result:=0;
  while Result<Count do
  begin
   if LALen=0 then
   begin
    Next.Read(StartCode,1);
    LALen:=StartCode and CodeCheck;
    if LALen=0 then {Not compressed block}
    begin
     LALen:=Next.Read(LookAhead,(StartCode shr LookAheadBits)+1);
     if LALen=0 then Exit;
    end
    else
    begin
     Inc(LALen,2);
     LBPos:=0;
     if Next.Read(LBPos,1)<>1 then begin LALen:=0; Exit; end;
     LBPos:=LBPos or ((StartCode shr LookAheadBits) shl 8);
     for CurP:=0 to LALen-1 do {Copy from look back to look ahead}
     begin
      LookAhead[CurP]:=LookBack^[LBpos];
      Inc(LBPos); if LBPos=LookBackSize then LBPos:=0;
     end;
    end;
   end;

   Get:=Count-Result;
   if Get>LALen then Get:=LALen;
   Move(LookAhead,TByteArray(Buf)[Result],Get); Inc(Result,Get);
   {Move Get bytes from LookAhead to LookBack^}
   for CurP:=0 to Get-1 do
   begin
    Inc(LookBackPos); if LookBackPos=LookBackSize then LookBackPos:=0;
    LookBack^[LookBackPos]:=LookAhead[CurP];
   end;
   Dec(LALen,Get); Move(LookAhead[Get],LookAhead,LALen);
  end;
 end;

//-----------------------------------------------------------------------------------
// Write data in look-ahead and reset 1-byte read/write buffer
 procedure TLZ77Stream2.Flush;
 var OutB : Byte;
 begin
  if fCanWrite then
  begin
   while LALen<>0 do if not Process then raise EInOutError.CreateFmt( rs_WriteError, [''] );
   if NotCompLen<>0 then {Write uncompressed block}
   begin
    OutB:=(NotCompLen-1) shl LookAheadBits;
    if (Next.Write(OutB,1)<>1) or (Next.Write(NotComp,NotCompLen)<>NotCompLen) then
     raise EInOutError.CreateFmt( rs_WriteError, [''] );
    NotCompLen:=0;
   end;
  end;
 end;

//-----------------------------------------------------------------------------------
// Return value is >0 if there is available data
 function TLZ77Stream2.Available;
 begin
  Available:=Next.Available+LALen;
 end;

//=======================================================================================================
// TLZ77StreamClass
//=======================================================================================================
procedure TLZ77StreamClass.SaveToCompressedFile(const FileName: string);
var
  Stream : TFilterStream;
begin
  Stream:=OpenLZ77File(FileName,fmWrite);
  try
    SaveToStream(Stream);
  finally
    Stream.FreeAll;
  end;
end;

procedure TLZ77StreamClass.LoadFromCompressedFile(const FileName: string);
var
  Stream : TFilterStream;
begin
  Stream:=OpenLZ77File(FileName,fmRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.FreeAll;
  end;
end;

end.

