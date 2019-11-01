unit VersionInfo;
// VersionInfo
// -----------
// Version: 2004-07-16
// Maintain: Rune
//
// Unit for retrieving version info from a file.
// The record VersionInfo contains all info about the file.
// Call getVersInfo to retrieve info about any file (if no info was available or
// an error occurs while reading the info, an exception is raised).
// On inclusion of this unit, the structure ThisApp is filled with info from the
// currently executing file (read from paramstr(0)).
//
// History:
// 2000-07-??: Completely rewritten internals - now uses Win32 API
// 2000-08-01: Support for languages different from danish
// 2000-08-04: fix for error including in non-version-info apps

interface
uses
  Windows, SysUtils;
                      

type
 TVersion = packed record
                   Minor,
                   Major,
                   Build,
                   Rel  : word;
                 end;
 TVersionInfo = record
   CompanyName: string;
   FileDescription: string;
   FileVersion: TVersion;
//   sFileVersion: string;
   InternalName: string;
   LegalCopyright: string;
   OriginalFilename: string;
   ProductName: string;
   ProductVersion: TVersion;
//   sProductVersion: string;
 end;

var
  ThisApp : TVersionInfo;
  VersionStr : string;

function GetVersInfo(const FileName: string): TVersionInfo;

implementation

type
  str2 = string[2];
const
 hexval : array[0..15] of char = '0123456789ABCDEF';
function char2hex(c: char): str2;
begin
 char2hex:= hexval[(byte(c) and $F0) shr 04]+
            hexval[(byte(c) and $0F) shr 00];
end;



resourcestring
  rs_readfixedfileinfo = 'An error occurred while reading the FixedFileInfo structure from the file.';
  rs_readlanginfo = 'An error occurred while reading the language/charset description in the file.';
  rs_noversioninfo = 'No version-info was associated with the requested file "%s"';


type
 TFixedFileInfo = packed record
   dwSignature: dword; //=0xFEEFO4BD
   dwStrucVersion: dword; // >0x029
   FileVersion: TVersion;
   ProductVersion: TVersion;
   dwFileFlagsMask: dword;
   dwFileFlags: dword;
   dwFileOS: dword;
   dwFileType: dword;
   dwFileSubtype: dword;
   dwFileDateMS: dword;
   dwFileDateLS: dword;
 end;

function GetVersInfo(const FileName: string): TVersionInfo;
const
 val_signature = $FEEF04BD;
var
 p: pointer;
 {$IFDEF VERSION10}
 sz: integer;
 dummy: integer;
 {$ELSE}
 sz: DWord;
 dummy: DWord;
 {$ENDIF}
 fixed: ^TFixedFileInfo;
 pp: pointer;
 pc: pChar;
 val_stringinfo: string;
 val_infocomp: string;
 val_infointernal: string;
 val_infofdesc: string;
 val_infofvers: string;
 val_infocopy: string;
 val_infoorg: string;
 val_infoprodname: string;
 val_infoprodvers: string;
begin
 sz:=GetFileVersionInfoSize(pChar(filename), dummy);
 if sz=0 then
   raise Exception.CreateFmt(rs_noversioninfo,[filename]);
 if sz>0 then
 begin
   getmem(p, sz);
   GetFileVersionInfo(pChar(filename), 0, sz, p);
   // use VerQueryValue to retrieve the FixedFileInfo structure:
   VerQueryValue(p,pChar('\\'),pointer(fixed),sz);
   if (sz<>sizeof(TFixedFileInfo)) or (fixed^.dwSignature<>val_signature) then
   begin
     dispose(p);
     raise Exception.Create(rs_readfixedfileinfo);
   end;
   result.FileVersion:=fixed^.FileVersion;
   result.ProductVersion:=fixed^.ProductVersion;
   // calculate correct look-up strings for VerQueryValue
   VerQueryValue(p,'\\VarFileInfo\\Translation',pointer(pc),sz);
   if (sz<4) then
     raise Exception.Create(rs_readlanginfo);
   val_stringinfo:= '\\StringFileInfo\\'+char2hex((pc+1)^)+char2hex((pc+0)^)+char2hex((pc+3)^)+char2hex((pc+2)^)+'\\';
   val_infocomp:= val_stringinfo+'CompanyName';
   val_infointernal:= val_stringinfo+'InternalName';
   val_infofdesc:= val_stringinfo+'FileDescription';
   val_infofvers:= val_stringinfo+'FileVersion';
   val_infocopy:= val_stringinfo+'LegalCopyright';
   val_infoorg:= val_stringinfo+'OriginalFilename';
   val_infoprodname:= val_stringinfo+'ProductName';
   val_infoprodvers:= val_stringinfo+'ProductVersion';
   // copy the strings from buffer to record
   VerQueryValue(p,pChar(val_infocomp),pp,sz);
   if sz>0 then begin setlength(result.CompanyName,sz-1); move(pp^,result.CompanyName[1],sz-1); end;
   VerQueryValue(p,pChar(val_infointernal),pp,sz);
   if sz>0 then begin setlength(result.InternalName,sz-1); move(pp^,result.InternalName[1],sz-1); end;
   VerQueryValue(p,pChar(val_infofdesc),pp,sz);
   if sz>0 then begin setlength(result.FileDescription,sz-1); move(pp^,result.FileDescription[1],sz-1); end;
//   VerQueryValue(p,pChar(val_infofvers),pp,sz);
//   if sz>0 then begin setlength(result.sFileVersion,sz-1); move(pp^,result.sFileVersion[1],sz-1); end;
   VerQueryValue(p,pChar(val_infocopy),pp,sz);
   if sz>0 then begin setlength(result.LegalCopyright,sz-1); move(pp^,result.LegalCopyright[1],sz-1); end;
   VerQueryValue(p,pChar(val_infoorg),pp,sz);
   if sz>0 then begin setlength(result.OriginalFilename,sz-1); move(pp^,result.OriginalFilename[1],sz-1); end;
   VerQueryValue(p,pChar(val_infoprodname),pp,sz);
   if sz>0 then begin setlength(result.ProductName,sz-1); move(pp^,result.ProductName[1],sz-1); end;
//   VerQueryValue(p,pChar(val_infoprodvers),pp,sz);
//   if sz>0 then begin setlength(result.sProductVersion,sz-1); move(pp^,result.sProductVersion[1],sz-1); end;
   dispose(p);
 end;
end;
                   

initialization
  try
    ThisApp:=GetVersInfo(ParamStr(0));
    VersionStr:=format('%d.%d.%d %d',[ThisApp.productversion.major,ThisApp.productversion.minor,ThisApp.productversion.build,ThisApp.productversion.rel]);
  except
    VersionStr:='';
  end;
end.

