///////////////////////////////////////////////////////////////////////////////////////////////
//
// FileUtils.pas
// --------------------------
// Changed:   2004-12-11
// Maintain:  Michael Vinther   |   mv@logicnet·dk
//
// Last change:
//   MaskCompare removed
//   RemoveBackslash does not change root string, e.g. "C:\"
//
unit FileUtils;

interface

uses Windows, SysUtils, ShellAPI, Classes, MemUtils, Messages, Registry, Forms, Shlobj;

var
  ProgramPath : string; // Path to program executable including '\'
  TempPath    : string; // Windows temporary file path, call InitTempPath before using

const
  faFile      = faReadOnly or faHidden or faSysFile or faArchive;
  faFileOrDir = faFile or faDirectory;

// Set TempPath
function InitTempPath: string;
// Get name for temporary file
function GetTempFileName: string;

// Get special Windows folders
function GetSpecialFolderLocation(CSIDL: Integer): string;
// Append text line to file
procedure WriteLog(const FileName, LogLine: string);
// Execute file. Failure if Result<=32
function ExecuteFile(const FileName: string; const Params: string=''; const DefaultDir: string=''; ShowCmd: Integer=SW_SHOW): THandle;
// Execute command line and optionally wait for process
procedure ExecuteCommandLine(const Command,StartupDir: string; WaitForProcess: Boolean);
// Return file name without extension and .
function RemoveFileExt(const FileName: string): string;
// Extract file extension without .
function ExtractFileExtNoDot(const FileName: string): string;
function ExtractFileExtNoDotUpper(const FileName: string): string;
// Return path name with a \ as the last character
function ForceBackslash(const PathName: string): string;
// Remove \ from end of path if not drive root e.g. "C:\"
function RemoveBackslash(const PathName: string): string;
// Return true if Path is not an absolute file/folder name
function IsRelativePath(const Path: string): Boolean;
// Set modified timestamp of file or directory
function SetFileTimeStamp(const FileName: string; TimeStamp: Integer): Boolean;
// Get modified timestamp of file or directory
function GetFileTimeStamp(const FileName: string): TDateTime;
// Get size of file
function GetFileSize(const FileName: string): Int64;
function SearchRecFileSize64(const SearchRec: TSearchRec): Int64;
// Return true if file conatins Text
function FileContains(const FileName: string; Text: string; CaseSensitive: Boolean; ExceptionOnError: Boolean=True): Boolean;
// If recursive, Search must end with \ and all files will be included
procedure GetDirList(const Search: string; List: TStrings; Recursive: Boolean=False);
// Delete file(s).
// Default is flags will recycle: FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI
// Multiple files can be seperated by #0, wildcards are allowed, full path must be specified
function DeleteFileEx(FileName: string; Flags: FILEOP_FLAGS=0): Boolean;
// Move file(s).
// Default is flags will overwrite: FOF_NOCONFIRMMKDIR or FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI
// Multiple files can be seperated by #0, wildcards are allowed, full path must be specified
function MoveFile(Source,Dest: string; Flags: FILEOP_FLAGS=0): Boolean;
function CopyFile(Source,Dest: string; CanOverwrite: Boolean): Boolean;
// Copy file(s).
// Default is flags will overwrite: FOF_NOCONFIRMMKDIR or FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI
// Multiple files can be seperated by #0, wildcards are allowed, full path must be specified
function CopyFileEx(Source,Dest: string; Flags: FILEOP_FLAGS=0): Boolean;

function MakeValidFileName(const Str: string): string;

// Show standard Windows dialogs
procedure ShowFileProperties(const FileName: string);
procedure ShowSearchDialog(const Directory: string);

function GetParameterFileName: string;

// Read/write complete file contents as string
function LoadFileAsString(const FileName: string): string;
procedure CreateFileFromString(const FileName,Data: string);

// Get file dropped by WM_DROPFILES
function GetDroppedFile(const Msg: TWMDropFiles; Index: Integer=0): string;

procedure RegisterFileFormat(Extension,AppID: string; Description: string=''; Executable: string=''; IconIndex: Integer=0; EditAction: Boolean=False);
function IsFormatRegistered(Extension,AppID: string): Boolean;

implementation

uses StringUtils;

function MakeValidFileName(const Str: string): string;
const
  FileNameChars = ['A'..'Z','0'..'9','.','_','~','-','@','='];
  ReplaceChar = '_';
var
  I : Integer;
begin
  Result:=Str;
  for I:=1 to Length(Result) do
    if not (UpCase(Result[I]) in FileNameChars) then Result[I]:=ReplaceChar;
  if Result='' then Result:='-';
  if Result[1]='.' then Result:=ReplaceChar+Result;
  if Length(Result)>255 then SetLength(Result,255);
end;

procedure GetDirList(const Search: string; List: TStrings; Recursive: Boolean);
var
  SRec : TSearchRec;
  E : Integer;
  Path, Filter : string;
begin
  List.BeginUpdate;
  Path:=ExtractFilePath(Search);
  if Recursive then
  begin
    Filter:=ExtractFileName(Search);
    E:=FindFirst(Path+'*.*',faFileOrDir,SRec);
  end
  else
  begin
    E:=FindFirst(Search,faFile,SRec);
  end;
  try
    while E=0 do
    begin
      if SRec.Attr and faDirectory<>0 then
      begin
        if Recursive  and (SRec.Name<>'.') and (SRec.Name<>'..') then
          GetDirList(Path+SRec.Name+'\'+Filter,List,True);
      end
      else if (Filter='') or MaskCompare(SRec.Name,Filter) then
        List.Add(Path+SRec.Name);
      E:=FindNext(SRec);
    end;
  finally
    FindClose(SRec);
    List.EndUpdate;
  end;
end;

procedure WriteLog(const FileName, LogLine: string);
var Log : TextFile;
begin
  try
    Assign(Log,FileName);
    {$I-} Append(Log); {$I+}
    if IOResult<>0 then Rewrite(Log);
    try
      WriteLn(Log,LogLine);
    finally
      CloseFile(Log);
    end;
  except
  end;
end;

// Execute file. Failure if Result<=32
function ExecuteFile(const FileName,Params,DefaultDir: string; ShowCmd: Integer): THandle;
begin
  Result:=ShellExecute(0,nil,
                       PChar(FileName),
                       PChar(Params),
                       PChar(DefaultDir),ShowCmd);
end;

procedure ExecuteCommandLine(const Command,StartupDir: string; WaitForProcess: Boolean);
var
  StartupInfo : TStartupInfo;
  ProcessInformation : TProcessInformation;
begin
  FillChar(StartupInfo,SizeOf(StartupInfo),0);
  FillChar(ProcessInformation,SizeOf(ProcessInformation),0);
  if not CreateProcess(nil,PChar(Command),nil,nil,False,NORMAL_PRIORITY_CLASS,nil,PChar(StartupDir),StartupInfo,ProcessInformation) then
    RaiseLastOSError;
  if WaitForProcess then WaitForSingleObject(ProcessInformation.hProcess,INFINITE);
end;

function ExtractFileExtNoDot(const FileName: string): string;
var I : Integer;
begin
  I:=LastDelimiter('.\:',FileName);
  if (I>0) and (FileName[I]='.') then Result:=Copy(FileName,I+1,MaxInt)
  else Result:='';
end;

function ExtractFileExtNoDotUpper(const FileName: string): string;
var I : Integer;
begin
  I:=LastDelimiter('.\:',FileName);
  if (I>0) and (FileName[I]='.') then Result:=UpperCase(Copy(FileName,I+1,MaxInt))
  else Result:='';
end;

function RemoveFileExt(const FileName: string): string;
var P : Integer;
begin
  for P:=Length(FileName) downto 1 do
    if FileName[P]='\' then Break
    else if FileName[P]='.' then
    begin
      Result:=Copy(FileName,1,P-1);
      Exit;
    end;
  Result:=FileName;
end;

function ForceBackslash(const PathName: string): string;
begin
  Result:=PathName;
  if (Result<>'') and not (Result[Length(Result)] in ['\','/']) then Result:=Result+'\';
end;

function RemoveBackslash(const PathName: string): string;
begin
  Result:=PathName;
  if (Length(Result)>1) and
     (Result[Length(Result)] in ['\','/']) and
     ((Length(Result)<>3) or (Result[2]<>':')) then SetLength(Result,Length(Result)-1);
end;

function GetFileSize(const FileName: string): Int64;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result:=-1;
  Handle:=FindFirstFile(PChar(FileName),FindData);
  if Handle<>INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
    begin
      Result:=FindData.nFileSizeLow or (Int64(FindData.nFileSizeHigh) shl 32);
    end;
  end;
end;

// Get modified timestamp of file or directory
function GetFileTimeStamp(const FileName: string): TDateTime;
var
  SearchRec : TSearchRec;
begin
  Result:=-1;
  if FindFirst(FileName,0,SearchRec)=0 then
  begin
    FindClose(SearchRec);
    Result:=FileDateToDateTime(SearchRec.Time);
  end;
end;

// Set modified timestamp of file or directory
function SetFileTimeStamp(const FileName: string; TimeStamp: Integer): Boolean;
var
  Handle : THandle;
  LocalFileTime, FileTime: TFileTime;
begin
  Handle:=CreateFile(PChar(FileName),GENERIC_WRITE,FILE_SHARE_WRITE or FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS,0);
  Result:=Handle<>0;
  if Result then
  begin
    Result:=DosDateTimeToFileTime(LongRec(TimeStamp).Hi,LongRec(TimeStamp).Lo,LocalFileTime) and
            LocalFileTimeToFileTime(LocalFileTime,FileTime) and
            SetFileTime(Handle,nil,nil,@FileTime);
    CloseHandle(Handle);
  end;
end;

function DeleteFileEx(FileName: string; Flags: FILEOP_FLAGS): Boolean;
var
  fos : TSHFileOpStruct;
begin
  if FileName='' then
  begin
    Result:=False;
    Exit;
  end;
  if FileName[Length(FileName)]<>#0 then FileName:=FileName+#0;
  ZeroMem(fos,SizeOf(fos));
  with fos do
  begin
    Wnd:=Application.Handle;
    wFunc:=FO_DELETE;
    pFrom:=PChar(FileName);
    if Flags=0 then fFlags:=FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI
    else fFlags:=Flags;
  end;
  Result:=SHFileOperation(fos)=0;
end;

function MoveFile(Source,Dest: string; Flags: FILEOP_FLAGS=0): Boolean;
var
  fos : TSHFileOpStruct;
begin
  if (Source='') or (Dest='') then
  begin
    Result:=False;
    Exit;
  end;
  if Source[Length(Source)]<>#0 then Source:=Source+#0;
  if Dest[Length(Dest)]<>#0 then Dest:=Dest+#0;
  ZeroMem(fos,SizeOf(fos));
  with fos do
  begin
    Wnd:=Application.Handle;
    wFunc:=FO_MOVE;
    pFrom:=PChar(Source);
    pTo:=PChar(Dest);
    if Flags=0 then fFlags:=FOF_NOCONFIRMMKDIR or FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI
    else fFlags:=Flags;
  end;
  Result:=SHFileOperation(fos)=0;
end;

function CopyFileEx(Source,Dest: string; Flags: FILEOP_FLAGS=0): Boolean;
var
  fos : TSHFileOpStruct;
begin
  if (Source='') or (Dest='') then
  begin
    Result:=False;
    Exit;
  end;
  if Source[Length(Source)]<>#0 then Source:=Source+#0;
  if Dest[Length(Dest)]<>#0 then Dest:=Dest+#0;
  ZeroMem(fos,SizeOf(fos));
  with fos do
  begin
    Wnd:=Application.Handle;
    wFunc:=FO_COPY;
    pFrom:=PChar(Source);
    pTo:=PChar(Dest);
    if Flags=0 then fFlags:=FOF_NOCONFIRMMKDIR or FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI
    else fFlags:=Flags;
  end;
  Result:=SHFileOperation(fos)=0;
end;

function CopyFile(Source,Dest: string; CanOverwrite: Boolean): Boolean;
begin
  Result:=Windows.CopyFile(PChar(Source),PChar(Dest),not CanOverwrite);
end;

function GetParameterFileName: string;
var
  I : Integer;
begin
  Result:=ParamStr(1);
  for I:=2 to ParamCount do Result:=Result+' '+ParamStr(I);
end;

procedure ShowFileProperties(const FileName: string);
var
  SEI : SHELLEXECUTEINFO;
begin
 ZeroMem(SEI,SizeOf(SEI));
 with SEI do
 begin
   cbSize:=SizeOf(SEI);
   fMask:=SEE_MASK_NOCLOSEPROCESS or SEE_MASK_INVOKEIDLIST or SEE_MASK_FLAG_NO_UI;
   Wnd:=Application.Handle;
   lpVerb:='properties';
   lpFile:=PChar(FileName);
  end;
  if not ShellExecuteEx(@SEI) then RaiseLastOSError;
end;

procedure ShowSearchDialog(const Directory: string);
begin
  if not ShellExecute(Application.Handle, 'find', PChar(RemoveBackslash(Directory)), nil, nil, 0)<=32 then RaiseLastOSError;
end;

// Get name of file dropped
// Use DragQueryFile(Msg.Drop,$FFFFFFFF,nil,0) to get number of files and DragFinish(Msg.Drop) when done
function GetDroppedFile(const Msg: TWMDropFiles; Index: Integer): string;
begin
  SetLength(Result,DragQueryFile(Msg.Drop,Index,nil,0));
  DragQueryFile(Msg.Drop,Index,@Result[1],Length(Result)+1);
end;

function SearchRecFileSize64(const SearchRec: TSearchRec): Int64;
begin
  Result:=(Int64(SearchRec.FindData.nFileSizeHigh) shl 32) or SearchRec.FindData.nFileSizeLow
end;

function IsFormatRegistered(Extension,AppID: string): Boolean;
var
  Reg : TRegistry;
  P : Integer;
  Str : string;
begin
  Extension:=LowerCase(Extension);
  if Extension[1]<>'.' then Extension:='.'+Extension;
  repeat
    P:=Pos(' ',AppID);
    if P=0 then Break;
    Delete(AppID,P,1);
  until False;
  Reg:=TRegistry.Create;
  try
    // Add HKEY_CLASSES_ROOT\.<Extension>\(Default) = <AppID>
    Reg.RootKey:=HKEY_CLASSES_ROOT;
    Result:=Reg.OpenKey(Extension,False);
    if Result then
    begin
      try
        Str:=Reg.ReadString('');
        Result:=Copy(Str,1,Pos('.',Str)-1)=AppID;
      except
        Result:=False;
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure RegisterFileFormat(Extension,AppID,Description,Executable: string; IconIndex: Integer; EditAction: Boolean);
var
  Reg: TRegistry;
  P : Integer;
  IsIcon : Boolean;
begin
  // Complete Extension
  Extension:=LowerCase(Extension);
  if Extension[1]<>'.' then Extension:='.'+Extension;
  // Find Executable
  if Executable='' then Executable:=ParamStr(0);
  // Chech if icon
  IsIcon:=(Extension='.ico') or (Extension='.cur') or (Extension='.ani');
  // Complete AppID
  if AppID<>'' then
  begin
    repeat
      P:=Pos(' ',AppID);
      if P=0 then Break;
      Delete(AppID,P,1);
    until False;
    if (Description='') and not IsIcon then AppID:=AppID+'.'+IntToStr(IconIndex)
    else AppID:=AppID+Extension;
  end
  else if IsIcon then Exit; // Can't figure out unassociating an icon

  Reg:=TRegistry.Create;
  try
    // Add HKEY_CLASSES_ROOT\.<Extension>\(Default) = <AppID>
    Reg.RootKey:=HKEY_CLASSES_ROOT;
    Reg.OpenKey(Extension,True);
    Reg.WriteString('',AppID);
    Reg.CloseKey;
    if AppID<>'' then
    begin
      // Now create an association for that file type
      // This adds HKEY_CLASSES_ROOT\<AppID>\(Default) = <Description>
      Reg.OpenKey(AppID,True);
      Reg.WriteString('',Description);
      Reg.CloseKey;

      if IconIndex>=0 then
      begin
        // Now write the default icon for my file type
        // This adds HKEY_CLASSES_ROOT\<AppID>\DefaultIcon\(Default) = <Executable>,<IconIndex>
        Reg.OpenKey(AppID+'\DefaultIcon', True);
        if IsIcon then Reg.WriteString('','"%1"')
        else Reg.WriteString('','"'+Executable+'",'+IntToStr(IconIndex));
        Reg.CloseKey;
      end;

      // Now write the open action in explorer
      {Reg.OpenKey(AppID+'\Shell\Open',True);
      Reg.WriteString('', '&Open');
      Reg.CloseKey;}

      // Write what application to open it with
      // This adds HKEY_CLASSES_ROOT\<AppID>\Shell\Open\Command\ (Default) = "<Executable>" "%1"
      if EditAction then Reg.OpenKey(AppID+'\Shell\Edit\Command',True)
      else Reg.OpenKey(AppID+'\Shell\Open\Command',True);
      Reg.WriteString('', '"'+Executable+'" "%1"');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  // Finally, we want the Windows Explorer to realize we added our file type
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

function InitTempPath: string;
begin
  SetLength(TempPath,GetTempPath(0,nil));
  SetLength(TempPath,GetTempPath(Length(TempPath)+1,@TempPath[1]));
  TempPath:=ForceBackslash(TempPath);
  Result:=TempPath;
end;

// Get name for temporary file
function GetTempFileName: string;
var
  Path, FileName : array[0..MAX_PATH] of Char;
begin
  Windows.GetTempPath(MAX_PATH,@Path);
  Windows.GetTempFileName(@Path,'Popims',0,@FileName);
  Result:=FileName;
end;

// Return true if file conatins Text
function FileContains(const FileName: string; Text: string; CaseSensitive,ExceptionOnError: Boolean): Boolean;
const
  BufferSize = 8*1024;
type
  TBuffer = array[0..BufferSize-1] of Char;
var
  Stream : TFileStream;
  Buffer, NextBuffer : ^TBuffer;
  NextBufferGot, Got : Integer;
  P, CheckedChars : Integer;
  Handle : THandle;
begin
  if Text='' then
  begin
    Result:=True;
    Exit;
  end;
  if Length(Text)>BufferSize then raise Exception.Create('Text too long');
  if not CaseSensitive then Text:=AnsiUpperCase(Text);
  Result:=False;
  try
    //Stream:=TFileStream.Create(FileName,fmOpenRead,fmShareDenyNone);
    Handle:=CreateFile(PChar(FileName),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
    if Handle=INVALID_HANDLE_VALUE then RaiseLastOSError;
    Stream:=TFileStream.Create(Handle);
    try
      New(Buffer);
      New(NextBuffer);
      try
        Got:=0;
        NextBufferGot:=-1;
        repeat
          if NextBufferGot=-1 then // Read new block
          begin
            Got:=Stream.Read(Buffer^,BufferSize);
            if Got<Length(Text) then Break;
            if not CaseSensitive then CharUpperBuff(Pointer(Buffer),Got);
          end
          else // Get already read block
          begin
            if NextBufferGot<Length(Text) then Break;
            Got:=NextBufferGot;
            NextBufferGot:=-1;
            SwapDWords(Buffer,NextBuffer);
          end;
          P:=-1;
          repeat
            P:=FastLocateByte(Buffer^,P+1,Got,Byte(Text[1]));
            if P=-1 then Break;
            if Got-P>=Length(Text) then // Text could be contained in buffer
            begin
              if CompareMem(@Buffer^[P],@Text[1],Length(Text)) then
              begin
                Result:=True;
                Break;
              end;
            end
            else // Text reaches outside buffer
            begin
              CheckedChars:=Got-P;
              if CompareMem(@Buffer^[P],@Text[1],CheckedChars) then
              begin
                if NextBufferGot=-1 then
                begin
                  NextBufferGot:=Stream.Read(NextBuffer^,BufferSize);
                  if not CaseSensitive then CharUpperBuff(Pointer(NextBuffer),NextBufferGot);
                end;
                if NextBufferGot<Length(Text)-CheckedChars then Continue; 
                if CompareMem(@NextBuffer^[0],@Text[CheckedChars+1],Length(Text)-CheckedChars) then
                begin
                  Result:=True;
                  Break;
                end;
              end;
            end;                                          
          until False;                                                
        until Result;
      finally
        Dispose(Buffer);
        Dispose(NextBuffer);
      end;
    finally
      Stream.Free;
    end;
  except
    if ExceptionOnError then raise;
  end;
end;

function LoadFileAsString(const FileName: string): string;
var
  F : TFileStream;
begin
  F:=TFileStream.Create(FileName,fmOpenRead	or fmShareDenyWrite);
  try
    SetLength(Result,F.Size);
    F.Read(Result[1],Length(Result));
  finally
    F.Free;
  end;
end;

procedure CreateFileFromString(const FileName,Data: string);
var
  F : TFileStream;
begin
  F:=TFileStream.Create(FileName,fmCreate	or fmShareDenyWrite);
  try
    F.Write(Data[1],Length(Data));
  finally
    F.Free;
  end;
end;

// Get special Windows folders
function GetSpecialFolderLocation(CSIDL: Integer): string;
begin
  SetLength(Result,MAX_PATH);
  if SHGetSpecialFolderPath(0,@Result[1],CSIDL,False) then
  begin
    SetLength(Result,StrLen(PChar(Result)));
    Result:=ForceBackslash(Result);
  end
  else RaiseLastOSError;
end;

// Return true if Path is not an absolute file/folder name
function IsRelativePath(const Path: string): Boolean;
begin
  Result:=(Length(Path)<2) or ((Path[2]<>':') and (Copy(Path,1,2)<>'\\'));
end;

var P : Integer;
initialization
  ProgramPath:=ParamStr(0); P:=Length(ProgramPath);
  while (P>0) and (ProgramPath[P]<>'\') do Dec(P);
  SetLength(ProgramPath,P);
end.

