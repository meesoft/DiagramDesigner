///////////////////////////////////////////////////////////////////////////////////////////////
//
// StringUtils.pas
// --------------------------
// Changed:   2003-10-23
// Maintain:  Michael Vinther   |   mv@logicnet·dk
//
// Last change:
//   RemLeadSpace added
//
unit StringUtils;

interface

uses Windows, SysUtils;

const
  TabChar = #9;

// Case sensitive compare with mask
const
  SingleWildcard = '?';
  MultiWildcard = '*';
function MaskCompare(Str,Mask: string): Boolean;

procedure ReplaceChar(var Str: string; Old,New: Char);
function RemTailSpace(const S: string): string;
function RemLeadSpace(const S: string): string;
function RemLeadTailSpace(const S: string): string;
// Always use . as decimal separator
function GetStr(const F: Extended; W,D: Integer): string; overload;
function GetStr(const F: Extended): string; overload;
function StrToFloatPeriod(const Str: string): Extended;
// Float to string uning ingeneering format
function FormatFloatEng(const Value: Extended; const Format: string=''): string;
function IntToStrLeadZero(Value,Width: Integer): string;
function IntToStrThousandSeparator(const Value: Int64): string;
// Fill with PadChar until string is at least MinLength
function PadStringRight(const Str: string; PadChar: Char; MinLength: Integer): string;
function PadStringLeft(const Str: string; PadChar: Char; MinLength: Integer): string;
// Double all occurences of & in the string
function DoubleAcceleratorMarker(const Str: string): string;
// Like standard Pos, but returns last occurence
function LastPos(const Substr: string; Str: string): Integer;
function StartsWith(const Substr,Str: string): Boolean;
function EndsWith(const Substr,Str: string): Boolean;
function StripTrailing3Dots(const Str: string): string;
function NextIndexOf(Ch: Char; const Str: string; From: Integer): Integer;
function PrevIndexOf(Ch: Char; const Str: string; From: Integer): Integer;

implementation

uses Math, MathUtils, SysConst;

function DoubleAcceleratorMarker(const Str: string): string;
var
  I : Integer;
begin
  Result:=Str;
  for I:=Length(Result) downto 1 do if Result[I]='&' then Insert('&',Result,I);
end;

function GetStr(const F: Extended; W,D: Integer): string;
begin
  Str(F:W:D,Result);
end;

function GetStr(const F: Extended): string; 
begin
  Str(F,Result);
  if Result[1]=' ' then Delete(Result,1,1);
end;

function RemTailSpace(const S: string): string;
var P: Integer;
begin
  P:=Length(S);
  while (P>0) and (S[P]=' ') do Dec(P);
  Result:=S;
  SetLength(Result,P);
end;

function StrToFloatPeriod(const Str: string): Extended;
var
  Code : Integer;
begin
  Val(Str,Result,Code);
  if Code<>0 then raise EConvertError.CreateFmt(SInvalidFloat,[Str]);
end;

function RemLeadSpace(const S: string): string;
var P: Integer;
begin
  Result:=S;
  P:=1;
  while (P<=Length(Result)) and (Result[P]=' ') do Inc(P);
  Delete(Result,1,P-1);
end;

function RemLeadTailSpace(const S: string): string;
var P: Integer;
begin
  Result:=S;
  P:=Length(Result);
  while (P>0) and (Result[P]=' ') do Dec(P);
  SetLength(Result,P);
  P:=1;
  while (P<=Length(Result)) and (Result[P]=' ') do Inc(P);
  Delete(Result,1,P-1);
end;

function MaskCompare(Str,Mask: string): Boolean;
var
  P : Integer;
begin
  Result:=True;
  for P:=1 to Length(Mask) do case Mask[P] of
    SingleWildcard : if P>Length(Str) then
                     begin
                       Result:=False;
                       Exit;
                     end;
    MultiWildcard  : begin
                       if P=Length(Mask) then Exit;
                       Delete(Mask,1,P);
                       Delete(Str,1,P-1);
                       repeat
                         if MaskCompare(Str,Mask) then Exit; // Suitable substring found, return true
                         Delete(Str,1,1);
                       until Length(Str)=0;
                       Result:=False;
                       Exit;
                     end;
    else if (P>Length(Str)) or (Str[P]<>Mask[P]) then
    begin
      Result:=False;
      Exit;
    end;
  end;
  if Length(Mask)<>Length(Str) then Result:=False;
end;

procedure ReplaceChar(var Str: string; Old,New: Char);
var
  P : Integer;
begin
  for P:=1 to Length(Str) do if Str[P]=Old then Str[P]:=New;
end;

function RepeatChar(Ch: Char; Count: Integer): string;
begin
  SetLength(Result,Count);
  FillChar(Result[1],Count,Ch);
end;

function IntToStrLeadZero(Value,Width: Integer): string;
begin
  if Value>=0 then
  begin
    Result:=IntToStr(Value);
    Result:=RepeatChar('0',Width-Length(Result))+Result;
  end
  else
  begin
    Result:=IntToStr(-Value);
    Result:='-'+RepeatChar('0',Width-Length(Result)-1)+Result;
  end
end;

function IntToStrThousandSeparator(const Value: Int64): string;
var I : Integer;
begin
  Result:=IntToStr(Value);
  I:=Length(Result)-2;
  while I>1 do
  begin
    Insert(ThousandSeparator,Result,I);
    Dec(I,3);
  end;
end;

function LastPos(const Substr: string; Str: string): Integer;
var
  P : Integer;
begin
  if Length(Substr)=1 then Result:=LastDelimiter(Substr,Str)
  else
  begin                                
    Result:=Pos(Substr,Str);
    if Result>0 then
    begin
      Delete(Str,1,Result);
      repeat
        P:=Pos(Substr,Str);
        if P=0 then Exit;
        Inc(Result,P);
        Delete(Str,1,P);
      until False;
    end;
  end;
end;

function StartsWith(const Substr,Str: string): Boolean;
begin
  Result:=Copy(Str,1,Length(Substr))=Substr;
end;

function EndsWith(const Substr,Str: string): Boolean;
begin
  Result:=Copy(Str,Length(Str)-Length(Substr)+1,MaxInt)=Substr;
end;

function StripTrailing3Dots(const Str: string): string;
begin
  if EndsWith('...',Str) then Result:=Copy(Str,1,Length(Str)-3)
  else Result:=Str;
end;

function NextIndexOf(Ch: Char; const Str: string; From: Integer): Integer;
begin
  for Result:=From+1 to Length(Str) do
    if Str[Result]=Ch then Exit;
  Result:=0;
end;

function PrevIndexOf(Ch: Char; const Str: string; From: Integer): Integer;
begin
  for Result:=From-1 downto 1 do
    if Str[Result]=Ch then Exit;
  Result:=0;
end;

function FormatFloatEng(const Value: Extended; const Format: string): string;
var
  E : Integer;
begin
  if Value=0 then Result:='0'
  else
  begin
    E:=Floor(Log10(Abs(Value)));
    if E<0 then E:=FloorInt(E-2,3)
    else E:=FloorInt(E,3);
    Result:=FormatFloat(Format,Value*Power(10,-E));
    if E<>0 then Result:=Result+'e'+IntToStr(E);
  end;
end;

// Fill with PadChar until string is at least MinLength
function PadStringRight(const Str: string; PadChar: Char; MinLength: Integer): string;
begin
  SetLength(Result,Max(0,MinLength-Length(Str)));
  FillChar(Result[1],Length(Result),PadChar);
  Result:=Str+Result;
end;

function PadStringLeft(const Str: string; PadChar: Char; MinLength: Integer): string;
begin
  SetLength(Result,Max(0,MinLength-Length(Str)));
  FillChar(Result[1],Length(Result),PadChar);
  Result:=Result+Str;
end;

end.

