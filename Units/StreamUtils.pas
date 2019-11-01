////////////////////////////////////////////////////////////////////////////////
//
// StreamsUtils.pas - Stream reading/writing utilities
// ---------------------------------------------------
// Version:   2005-10-30
// Maintain:  Michael Vinther  |  mv@logicnet·dk
//
// Last changes:
//   LZ77 compression moved to TLZ77StreamClass in CompLZ77 unit
//
unit StreamUtils;

interface

uses SysUtils, Classes, Streams, BufStream, MemUtils;

procedure SaveString(const Str: string; Stream: TBaseStream);
procedure SaveString16(const Str: string; Stream: TBaseStream);
procedure LoadString(var Str: string; Stream: TBaseStream); overload;
procedure LoadString16(var Str: string; Stream: TBaseStream); overload;
function LoadString(Stream: TBaseStream): string; overload;
function LoadString16(Stream: TBaseStream): string; overload;
procedure SaveStringList(List: TStrings; Stream: TBaseStream);
procedure LoadStringList(List: TStrings; Stream: TBaseStream);
//procedure SaveListView(List: TListView; Stream: TBaseStream);
//procedure LoadListView(List: TListView; Stream: TBaseStream);

type
  TStreamClass = class(TAssignObject)
    public
      procedure SaveToStream(Stream: TBaseStream); virtual;
      procedure LoadFromStream(Stream: TBaseStream); overload; virtual;
      procedure LoadFromStream(Stream: TBaseStream; StreamVersion: Integer); overload; virtual;
      procedure SaveToFile(const FileName: string); virtual;
      procedure LoadFromFile(const FileName: string); virtual;
    end;

implementation

procedure SaveString(const Str: string; Stream: TBaseStream);
var
  Len : Integer;
begin
  Len:=Length(Str);
  Stream.Write(Len,SizeOf(Len));
  Stream.Write(PChar(@Str[1])^,Len);
end;

procedure SaveString16(const Str: string; Stream: TBaseStream);
var
  Len : Integer;
begin
  Len:=Length(Str);
  if Len>High(Word) then Len:=High(Word);
  Stream.Write(Len,2);
  Stream.Write(PChar(@Str[1])^,Len);
end;

procedure LoadString(var Str: string; Stream: TBaseStream);
var
  Len : Integer;
begin
  if Stream.Read(Len,SizeOf(Len))<SizeOf(Len) then Len:=0;
  SetLength(Str,Len);
  Stream.Read(Str[1],Len);
end;

procedure LoadString16(var Str: string; Stream: TBaseStream); overload;
var
  Len : Word;
begin
  if Stream.Read(Len,2)<2 then Len:=0;
  SetLength(Str,Len);
  Stream.Read(Str[1],Len);
end;

function LoadString(Stream: TBaseStream): string; overload;
begin
  LoadString(Result,Stream);
end;

function LoadString16(Stream: TBaseStream): string;
begin
  LoadString16(Result,Stream);
end;

procedure SaveStringList(List: TStrings; Stream: TBaseStream);
var
  Len : Integer;
  Str : string;
begin
  if Assigned(List) then
  begin
    Str:=List.Text;
    Len:=Length(Str);
    Stream.Write(Len,SizeOf(Len));
    Stream.Write(Str[1],Len);
  end
  else
  begin
    Len:=0;
    Stream.Write(Len,SizeOf(Len));
  end;
end;

procedure LoadStringList(List: TStrings; Stream: TBaseStream);
var
  Len : Integer;
  Str : string;
begin
  Stream.Read(Len,SizeOf(Len));
  SetLength(Str,Len);
  Stream.Read(Str[1],Len);
  List.Text:=Str;
end;

{procedure SaveListView(List: TListView; Stream: TBaseStream);
var
  I, C : Integer;
  Check : Boolean;
begin
  I:=List.Items.Count;
  Stream.Write(I,SizeOf(I));
  for I:=0 to I-1 do with List.Items[I] do
  begin
    if List.Checkboxes then
    begin
      Check:=Checked;
      Stream.Write(Check,SizeOf(Check));
    end;
    SaveString(Caption,Stream);
    C:=SubItems.Count;
    Stream.Write(C,SizeOf(C));
    for C:=0 to C-1 do SaveString(SubItems[C],Stream);
  end;
end;

procedure LoadListView(List: TListView; Stream: TBaseStream);
var
  I, C : Integer;
  Check : Boolean;
  Str : string;
begin
  List.Items.BeginUpdate;
  try
    Stream.Read(I,SizeOf(I));
    for I:=0 to I-1 do with List.Items.Add do
    begin
      if List.Checkboxes then
      begin
        Stream.Read(Check,SizeOf(Check));
        Checked:=Check;
      end;
      LoadString(Str,Stream);
      Caption:=Str;
      Stream.Read(C,SizeOf(C));
      for C:=0 to C-1 do
      begin
        LoadString(Str,Stream);
        SubItems.Add(Str);
      end;
    end;
  finally
    List.Items.EndUpdate;
  end;
end;}

//=====================================================================================================
// TStreamClass
//=====================================================================================================
procedure TStreamClass.SaveToStream(Stream: TBaseStream);
begin
  Assert(False,ClassName+'.SaveToStream not implemented');
end;

procedure TStreamClass.LoadFromStream(Stream: TBaseStream);
begin
  Assert(False,ClassName+'.LoadFromStream not implemented');
end;

procedure TStreamClass.LoadFromStream(Stream: TBaseStream; StreamVersion: Integer);
begin
  LoadFromStream(Stream);
end;

procedure TStreamClass.SaveToFile(const FileName: string);
var
  Stream : TFilterStream;
begin
  Stream:=OpenBufferedFile(FileName,fmWrite);
  try
    SaveToStream(Stream);
  finally
    Stream.FreeAll;
  end;
end;

procedure TStreamClass.LoadFromFile(const FileName: string);
var
  Stream : TFilterStream;
begin
  Stream:=OpenBufferedFile(FileName,fmRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.FreeAll;
  end;
end;

end.

