//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Settings.pas - Program setting module
// -------------------------------------
// Version:   2005-12-18
// Maintain:  Michael Vinther   |   mv@logicnet·dk
//
// Last changes:
//
unit Settings;

interface

uses Monitor, Windows, Classes, Forms, SysUtils, Registry;

type
  TProgramSetup = class(TMonitorObject)
    public
      function ValueExists(const Name: string): Boolean; virtual; abstract;
      procedure DeleteValue(const Name: string); virtual; abstract;

      // Write data
      procedure WriteString(const Name,Value: string); virtual; abstract;
      procedure WriteInteger(const Name: string; Value: Integer); virtual; abstract;
      procedure WriteDouble(const Name: string; Value: Double); virtual; abstract;
      procedure WriteBoolean(const Name: string; Value: Boolean); virtual; abstract;
      procedure WriteBinary(const Name: string; var Value; Size: Integer); virtual; abstract;

      // Get data and return true if value exists
      function ReadString(const Name: string; var Value: string): Boolean; virtual; abstract;
      function ReadInteger(const Name: string; var Value: Integer): Boolean; virtual; abstract;
      function ReadDouble(const Name: string; var Value: Double): Boolean; virtual; abstract;
      function ReadBoolean(const Name: string; var Value: Boolean): Boolean; virtual; abstract;
      function ReadBinary(const Name: string; var Value; BufferSize: Integer): Integer; virtual; abstract; // Return size

      // Get data or raise exception if not value does not exist
      function GetString(const Name: string): string; overload; virtual;
      function GetInteger(const Name: string): Integer; overload; virtual;
      function GetBoolean(const Name: string): Boolean; overload; virtual;

      // Get data or return Default if not value does not exist
      function GetString(const Name,Default: string): string; overload;
      function GetInteger(const Name: string; Default: Integer): Integer; overload;
      function GetDouble(const Name: string; const Default: Double): Double; overload;
      function GetBoolean(const Name: string; Default: Boolean): Boolean; overload;
    end;

  TProgramSetupRegistry = class(TProgramSetup)
    private
      Reg : TRegistry;
      FProgramKey : string;
    public
      // ProgramKey should be 'Software\MeeSoft\Program name';
      constructor Create(const ProgramKey: string; ReadOnly: Boolean=False);
      destructor Destroy; override;

      property ProgramKey: string read FProgramKey;

      function DataType(const Name: string): TRegDataType;
      function ValueExists(const Name: string): Boolean; override;
      procedure DeleteValue(const Name: string); override;

      // Write data to registry
      procedure WriteString(const Name,Value: string); override;
      procedure WriteInteger(const Name: string; Value: Integer); override;
      procedure WriteDouble(const Name: string; Value: Double); override;
      procedure WriteBoolean(const Name: string; Value: Boolean); override;
      procedure WriteBinary(const Name: string; var Value; Size: Integer); override;

      // Get data or raise exception if not value does not exist
      function GetString(const Name: string): string; overload; override;
      function GetInteger(const Name: string): Integer; overload; override;
      function GetBoolean(const Name: string): Boolean; overload; override;

      // Get data from registry and return true if value exists
      function ReadString(const Name: string; var Value: string): Boolean; override;
      function ReadInteger(const Name: string; var Value: Integer): Boolean; override;
      function ReadDouble(const Name: string; var Value: Double): Boolean; override;
      function ReadBoolean(const Name: string; var Value: Boolean): Boolean; override;
      function ReadBinary(const Name: string; var Value; BufferSize: Integer): Integer; override; // Return size
    end;

  TProgramSetupIni = class(TProgramSetup)
    private
      Strings : TStringList;
      FFileName : string;
    public
      constructor Create(const FileName: string);
      destructor Destroy; override;

      property FileName: string read FFileName;

      function ValueExists(const Name: string): Boolean; override;
      procedure DeleteValue(const Name: string); override;

      // Write data to disk
      procedure WriteData;

      // Write data to local cache
      procedure WriteString(const Name,Value: string); override;
      procedure WriteInteger(const Name: string; Value: Integer); override;
      procedure WriteDouble(const Name: string; Value: Double); override;
      procedure WriteBoolean(const Name: string; Value: Boolean); override;
      procedure WriteBinary(const Name: string; var Value; Size: Integer); override;

      // Get data and return true if value exists
      function ReadString(const Name: string; var Value: string): Boolean; override;
      function ReadInteger(const Name: string; var Value: Integer): Boolean; override;
      function ReadDouble(const Name: string; var Value: Double): Boolean; override;
      function ReadBoolean(const Name: string; var Value: Boolean): Boolean; override;
      function ReadBinary(const Name: string; var Value; BufferSize: Integer): Integer; override; // Return size
    end;

var
  Setup : TProgramSetup = nil;

// ProgramKey should be 'Software\MeeSoft\Program name'
procedure CreateSetup(const ProgramKey: string);
// Delete program key and free and nil Setup
procedure DestoySetupAndCleanup;

// For use in OnShow and OnDestroy. Assume CreateSetup was called
procedure SaveWindowPos(Form: TForm);
function RestoreWindowPos(Form: TForm): Boolean; // True if position found in registry

implementation

uses StringUtils, Math;

procedure SaveWindowPos(Form: TForm);
var
  Pos : TSmallPoint;
  Maximized : Boolean;
begin
  Maximized:=Form.WindowState=wsMaximized;
  Setup.WriteBoolean('IsMaximized',Maximized);
  if not Maximized then
  begin
    Pos.X:=Form.Left;
    Pos.Y:=Form.Top;
    Setup.WriteInteger('TopLeft',Integer(Pos));
    Pos.X:=Form.Width;
    Pos.Y:=Form.Height;
    Setup.WriteInteger('WidthHeight',Integer(Pos));
  end;
end;

function RestoreWindowPos(Form: TForm): Boolean;
var
  Pos : TSmallPoint;
  DesktopRect : TRect;
begin
  Result:=False;
  if Setup.GetBoolean('IsMaximized',False) then
  begin
    Form.WindowState:=wsMaximized;
    Result:=True;
  end
  else
  begin
    DesktopRect:=Screen.DesktopRect;
    if Setup.ReadInteger('TopLeft',Integer(Pos)) and
       InRange(Pos.X,DesktopRect.Left-4,DesktopRect.Right) and InRange(Pos.Y,DesktopRect.Top-4,DesktopRect.Bottom) then
    begin
      Form.Left:=Pos.X;
      Form.Top:=Pos.Y;
      Result:=True;
    end;
    if Setup.ReadInteger('WidthHeight',Integer(Pos)) then
    begin
      Form.Width:=Pos.X;
      Form.Height:=Pos.Y;
    end;
    if Form.Left+Form.Width<DesktopRect.Left then Form.Left:=0;
    if Form.Top+Form.Height<DesktopRect.Top then Form.Top:=0;
  end;
end;

procedure CreateSetup(const ProgramKey: string);
begin
  Setup.Free;
  Setup:=TProgramSetupRegistry.Create(ProgramKey);
end;

procedure DestoySetupAndCleanup;
var
  LocalSetupReg : TProgramSetupRegistry;
begin
  LocalSetupReg:=Setup as TProgramSetupRegistry;
  Setup:=nil;
  try
    LocalSetupReg.Reg.CloseKey;
    LocalSetupReg.Reg.DeleteKey(LocalSetupReg.ProgramKey);
  finally
    LocalSetupReg.Destroy;
  end;
end;

//==============================================================================================================================
// TProgramSetup
//==============================================================================================================================

function TProgramSetup.GetBoolean(const Name: string): Boolean;
begin
  if not ReadBoolean(Name,Result) then raise Exception.Create(ClassName+': Value not found: '+Name);
end;

function TProgramSetup.GetInteger(const Name: string): Integer;
begin
  if not ReadInteger(Name,Result) then raise Exception.Create(ClassName+': Value not found: '+Name);
end;

function TProgramSetup.GetString(const Name: string): string;
begin
  if not ReadString(Name,Result) then raise Exception.Create(ClassName+': Value not found: '+Name);
end;

function TProgramSetup.GetString(const Name,Default: string): string;
begin
  if not ReadString(Name,Result) then Result:=Default;
end;

function TProgramSetup.GetInteger(const Name: string; Default: Integer): Integer;
begin
  if not ReadInteger(Name,Result) then Result:=Default;
end;

function TProgramSetup.GetBoolean(const Name: string; Default: Boolean): Boolean;
begin
  if not ReadBoolean(Name,Result) then Result:=Default;
end;

function TProgramSetup.GetDouble(const Name: string; const Default: Double): Double;
begin
  if not ReadDouble(Name,Result) then Result:=Default;
end;

//==============================================================================================================================
// TProgramSetupRegistry
//==============================================================================================================================

constructor TProgramSetupRegistry.Create(const ProgramKey: string; ReadOnly: Boolean);
begin
  inherited Create;
  FProgramKey:=ProgramKey;
  if ReadOnly then Reg:=TRegistry.Create(KEY_READ)
  else Reg:=TRegistry.Create(KEY_READ or KEY_WRITE);
  Reg.OpenKey(ProgramKey,not ReadOnly);
end;

destructor TProgramSetupRegistry.Destroy;
begin
  Reg.Free;
  inherited Destroy;
end;

procedure TProgramSetupRegistry.WriteString(const Name,Value: string);
begin
  Reg.WriteString(Name,Value);
end;

procedure TProgramSetupRegistry.WriteInteger(const Name: string; Value: Integer);
begin
  Reg.WriteInteger(Name,Value);
end;

procedure TProgramSetupRegistry.WriteBoolean(const Name: string; Value: Boolean);
begin
  Reg.WriteBool(Name,Value);
end;

procedure TProgramSetupRegistry.WriteDouble(const Name: string; Value: Double);
begin
  WriteBinary(Name,Value,SizeOf(Value));
end;

procedure TProgramSetupRegistry.WriteBinary(const Name: string; var Value; Size: Integer);
begin
  Reg.WriteBinaryData(Name,Value,Size);
end;

function TProgramSetupRegistry.ReadString(const Name: string; var Value: string): Boolean;
begin
  Result:=Reg.GetDataType(Name)=rdString;
  if Result then Value:=Reg.ReadString(Name);
end;

function TProgramSetupRegistry.ReadInteger(const Name: string; var Value: Integer): Boolean;
begin
  Result:=Reg.GetDataType(Name)=rdInteger;
  if Result then Value:=Reg.ReadInteger(Name);
end;

function TProgramSetupRegistry.ReadBoolean(const Name: string; var Value: Boolean): Boolean;
begin
  Result:=Reg.GetDataType(Name)=rdInteger;
  if Result then Value:=Reg.ReadBool(Name);
end;

function TProgramSetupRegistry.ReadDouble(const Name: string; var Value: Double): Boolean;
begin
  Result:=ReadBinary(Name,Value,SizeOf(Value))=SizeOf(Value);
end;

function TProgramSetupRegistry.ReadBinary(const Name: string; var Value; BufferSize: Integer): Integer;
begin
  try
    Result:=Reg.ReadBinaryData(Name,Value,BufferSize);
  except
     Result:=0;
  end;
end;

function TProgramSetupRegistry.GetString(const Name: string): string;
begin
  Result:=Reg.ReadString(Name);
end;

function TProgramSetupRegistry.GetInteger(const Name: string): Integer;
begin
  Result:=Reg.ReadInteger(Name);
end;

function TProgramSetupRegistry.GetBoolean(const Name: string): Boolean;
begin
  Result:=Reg.ReadBool(Name);
end;

function TProgramSetupRegistry.DataType(const Name: string): TRegDataType;
begin
  Result:=Reg.GetDataType(Name);
end;

function TProgramSetupRegistry.ValueExists(const Name: string): Boolean;
begin
  Result:=Reg.ValueExists(Name);
end;

procedure TProgramSetupRegistry.DeleteValue(const Name: string);
begin
  Reg.DeleteValue(Name);
end;

//==============================================================================================================================
// TProgramSetupIni
//==============================================================================================================================

constructor TProgramSetupIni.Create(const FileName: string);
begin
  inherited Create;
  FFileName:=FileName;
  Strings:=TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
  except
  end;
end;

destructor TProgramSetupIni.Destroy;
begin
  try
    WriteData;
  except
    // Too bad
  end;
  Strings.Free;
  inherited;
end;

procedure TProgramSetupIni.WriteData;
begin
  Strings.SaveToFile(FileName);
end;

function TProgramSetupIni.ValueExists(const Name: string): Boolean;
begin
  Result:=Strings.IndexOfName(Name)>=0;
end;

function TProgramSetupIni.ReadBinary(const Name: string; var Value; BufferSize: Integer): Integer;
var
  Data : string;
  I : Integer;
begin
  Data:=Strings.Values[Name];
  Result:=0;
  I:=1;
  while (Result<BufferSize) and (I<=Length(Data)) do
  begin
    if Data[I]='\' then
    begin
      Inc(I);
      TByteArray(Value)[Result]:=Byte(Data[I])-65;
    end
    else TByteArray(Value)[Result]:=Byte(Data[I]);
    Inc(I);
    Inc(Result);
  end;
end;

function TProgramSetupIni.ReadBoolean(const Name: string; var Value: Boolean): Boolean;
var
  I: Integer;
begin
  I:=Strings.IndexOfName(Name);
  Result:=I>=0;
  if Result then Value:=Strings.ValueFromIndex[I]<>'0';
end;

function TProgramSetupIni.ReadDouble(const Name: string; var Value: Double): Boolean;
var
  I: Integer;
begin
  I:=Strings.IndexOfName(Name);
  Result:=I>=0;
  if Result then
  begin
    Val(Strings.ValueFromIndex[I],Value,I);
    if I<>0 then Result:=False;
  end;
end;

function TProgramSetupIni.ReadInteger(const Name: string; var Value: Integer): Boolean;
var
  I: Integer;
begin
  I:=Strings.IndexOfName(Name);
  Result:=I>=0;
  if Result then
  begin
    Val(Strings.ValueFromIndex[I],Value,I);
    if I<>0 then Result:=False;
  end;
end;

function TProgramSetupIni.ReadString(const Name: string; var Value: string): Boolean;
var
  I: Integer;
begin
  I:=Strings.IndexOfName(Name);
  Result:=I>=0;
  if Result then Value:=Strings.ValueFromIndex[I];
end;

procedure TProgramSetupIni.WriteBinary(const Name: string; var Value; Size: Integer);
var
  Data : string;
  I, P : Integer;
begin
  SetLength(Data,Size*2);
  P:=0;
  for I:=0 to Size-1 do
  begin
    if TByteArray(Value)[I] in [0..31,Byte('\')] then
    begin
      Inc(P);
      Data[P]:='\';
      Inc(P);
      Data[P]:=Char(TByteArray(Value)[I]+65);
    end
    else
    begin
      Inc(P);
      Data[P]:=Char(TByteArray(Value)[I]);
    end;
  end;
  Strings.Values[Name]:=Copy(Data,1,P);
end;

procedure TProgramSetupIni.WriteBoolean(const Name: string; Value: Boolean);
begin
  Strings.Values[Name]:=IntToStr(Integer(Value));
end;

procedure TProgramSetupIni.WriteDouble(const Name: string; Value: Double);
begin
  Strings.Values[Name]:=GetStr(Value);
end;

procedure TProgramSetupIni.WriteInteger(const Name: string; Value: Integer);
begin
  Strings.Values[Name]:=IntToStr(Value);
end;

procedure TProgramSetupIni.WriteString(const Name, Value: string);
begin
  Strings.Values[Name]:=Value;
end;

procedure TProgramSetupIni.DeleteValue(const Name: string);
begin
  Strings.Values[Name]:='';
end;

initialization

finalization
  FreeAndNil(Setup);
end.

