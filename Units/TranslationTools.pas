unit TranslationTools;

interface

uses Windows, SysUtils, Classes, Forms, Graphics, StringLists;

type
  TTranslationManager = class
    private
      ResourceIDList : TStringIntegerList;
      Translations : TStringIntegerList;
      ResourceStringOffset : Integer;
      ReplaceCharSet : TFontCharset;
      procedure LoadDRCFile(const FileName: string);
      procedure InsertTranslation(const StrOriginal, StrTranslation, StrID : string);
      procedure LoadTabDelimitedTranslation(const FileName: string);
      procedure DecodeTranslationStrings;
    public
      destructor Destroy; override;
      function TranslateString(ResStringRec: PResStringRec): string; overload;
      function TranslateString(const Str,ID: string): string; overload;
      function TranslateString(const Str: string; FormClass: TClass; ID: string; FormProperty: Boolean): string; overload;
      procedure TranslateForm(FormOrFrame: TScrollingWinControl);
      procedure TranslateComponent(Component: TComponent; FormOrFrame: TScrollingWinControl);
    end;

var
  TranslationManager : TTranslationManager = nil;

procedure LoadTranslation(const LanguageFile: string; DRCFile: string='');

function DecodeResourceString(const Str: string): string;
function EncodeResourceString(const Str: string): string;

function EncodeDFMString(const Str: string): string;
function DecodeDFMString(const Str: string): string;

implementation

uses FastCodePatch, TypInfo, FileUtils;

function LoadResString(ResStringRec: PResStringRec): string;
begin
  if Assigned(ResStringRec) then
    Result:=TranslationManager.TranslateString(ResStringRec);
end;

procedure LoadTranslation(const LanguageFile: string; DRCFile: string);
var
  DoPatch : Boolean;
begin
  DoPatch:=False;
  if TranslationManager=nil then
  begin
    DoPatch:=True;
    TranslationManager:=TTranslationManager.Create;
  end;
  try
    if DRCFile='' then DRCFile:=RemoveFileExt(ParamStr(0))+'.drc';
    TranslationManager.LoadDRCFile(DRCFile);
    TranslationManager.LoadTabDelimitedTranslation(LanguageFile);
    TranslationManager.DecodeTranslationStrings;
    if DoPatch then FastCodeAddressPatch(@System.LoadResString,@LoadResString);
  except
    FreeAndNil(TranslationManager);
    raise;
  end;
end;

function DecodeResourceString(const Str: string): string;
var
  P : Integer;
begin
  Result:=Str;
  P:=1;
  repeat
    if Result[P]='\' then
    begin
      Delete(Result,P,1);
      case Result[P] of
        't' : Result[P]:=#9;
        'n' : Result[P]:=#10;
        'r' : Result[P]:=#13;
      end;
    end
    else Inc(P);
  until P>=Length(Result);
end;

function EncodeResourceString(const Str: string): string;
var
  P : Integer;
begin
  Result:=Str;
  for P:=Length(Result) downto 1 do
    case Result[P] of
      '\',
      '"' : Insert('\',Result,P);
      #9  : begin
              Result[P]:='t';
              Insert('\',Result,P);
            end;
      #10 : begin
              Result[P]:='n';
              Insert('\',Result,P);
            end;
      #13 : begin
              Result[P]:='r';
              Insert('\',Result,P);
            end;
    end;
end;

function DecodeDFMString(const Str: string): string;
var
  I : Integer;
begin
  Result:=Str;
  for I:=Length(Result) downto 1 do if Result[I]='#' then
  begin
    try
      Result[I]:=Char(StrToInt(Copy(Result,I+1,3)));
    except
      on E: Exception do raise Exception.Create('Error decoding "'+Result+'"'#13+E.Message);
    end;
    Delete(Result,I+1,3);
  end;
end;

// Replace [#0..#31,'#'] with hex ASCII code
function EncodeDFMString(const Str: string): string;
var
  I : Integer;
begin
  Result:=Str;
  for I:=Length(Result) downto 1 do if Result[I] in [#0..#31,'#'] then
  begin
    Insert('$'+IntToHex(Byte(Result[I]),2),Result,I+1);
    Result[I]:='#';
  end;
end;

//==============================================================================================================================
// TTranslationManager
//==============================================================================================================================

destructor TTranslationManager.Destroy;
begin
  ResourceIDList.Free;
  Translations.Free;
  inherited;
end;

procedure TTranslationManager.LoadDRCFile(const FileName: string);
var
  DRCFile : TextFile;
  Str : string;
  P, StringIndex, PrevIndex : Integer;
begin
  FreeAndNil(ResourceIDList);
  FreeAndNil(Translations);
  ReplaceCharSet:=0;
  try
    AssignFile(DRCFile,FileName);
    Reset(DRCFile);
    try
      // Read resource IDs
      ResourceStringOffset:=0;
      ResourceIDList:=TStringIntegerList.Create;
      repeat
        ReadLn(DRCFile,Str);
        if Copy(Str,1,8)='#define ' then
        begin
          Delete(Str,1,8);
          P:=Pos(' ',Str);
          ResourceIDList.AddValue(Copy(Str,1,P-1),StrToInt(Copy(Str,P+1,MaxInt)));
          Continue;
        end;
      until (Str='BEGIN') or Eof(DRCFile);
      ResourceStringOffset:=ResourceIDList.Value[0];
      // Read original (English) texts
      PrevIndex:=0;
      Translations:=TStringIntegerList.Create;
      for StringIndex:=0 to ResourceIDList.Count-1 do
      begin
        if StringIndex>0 then
          for P:=PrevIndex+2 to ResourceIDList.Value[StringIndex] do
            Translations.Add('');
        PrevIndex:=ResourceIDList.Value[StringIndex];
        ReadLn(DRCFile,Str);
        Assert(Str[1]=#9);
        P:=Pos(','#9,Str);
        Assert(Copy(Str,2,P-2)=ResourceIDList[StringIndex]);
        ResourceIDList.Value[StringIndex]:=Translations.Add(Copy(Str,P+3,Length(Str)-P-3));
      end;
    finally
      CloseFile(DRCFile);
    end;
  except
    on E: Exception do
      raise Exception.Create('Error reading "'+FileName+'": '+E.Message+#13+Str);
  end;
end;

procedure TTranslationManager.InsertTranslation(const StrOriginal, StrTranslation, StrID: string);
var
  I : Integer;
begin
  I:=ResourceIDList.IndexOf(StrID);
  if I>=0 then // Resource string
  begin
    //Assert(Translations[ResourceIDList.Value[I]]=StrOriginal,Translations[ResourceIDList.Value[I]]+'<>'+StrOriginal);
    I:=ResourceIDList.Value[I];
    Translations[I]:=StrTranslation;
    Translations.Value[I]:=1;
  end
  else // Component text
  begin
    ResourceIDList.AddValue(StrID,Translations.AddValue(StrTranslation,2));
  end;
end;

procedure TTranslationManager.LoadTabDelimitedTranslation(const FileName: string);
var
  TranslationFile : TextFile;
  Str, StrOriginal, StrTranslation, StrID : string;
  P, LineIndex : Integer;
begin
  try
    ResourceIDList.Sorted:=True;
    AssignFile(TranslationFile,FileName);
    Reset(TranslationFile);
    try
      LineIndex:=0;
      repeat
        ReadLn(TranslationFile,Str);
        Inc(LineIndex);
        if Str='' then Continue;
        P:=Pos(#9,Str);
        StrOriginal:=Copy(Str,1,P-1);
        Delete(Str,1,P);
        P:=Pos(#9,Str);
        if P=0 then
        begin
          if StrOriginal<>'DefaultCharset' then
            raise Exception.Create('Missing string ID: '+Str+' (line '+IntToStr(LineIndex)+')');
          ReplaceCharSet:=StrToInt(Str);
          Continue;
        end;
        StrTranslation:=Copy(Str,1,P-1);
        if (StrTranslation<>'') or (Pos(':',Str)<>0) then
        begin
          Delete(Str,1,P);
          repeat
            P:=Pos(#9,Str);
            if P=0 then P:=MaxInt;
            StrID:=Copy(Str,1,P-1);
            Delete(Str,1,P);
            if StrTranslation='' then
            begin
              // For component texts, insert the original string in case of no translation to make sure that
              // the correct text is used in case of form inheritance 
              if Pos(':',StrID)<>0 then InsertTranslation(StrOriginal,StrOriginal,StrID);
            end
            else InsertTranslation(StrOriginal,StrTranslation,StrID);
          until Str='';
        end;
      until Eof(TranslationFile);
    finally
      CloseFile(TranslationFile);
    end;
  except
    on E: Exception do
      raise Exception.Create('Error reading "'+FileName+'": '+E.Message+#13+Str);
  end;
end;

function TTranslationManager.TranslateString(ResStringRec: PResStringRec): string;
begin
  if Self=nil then Result:=System.LoadResString(ResStringRec)
  else Result:=Translations[ResStringRec^.Identifier-ResourceStringOffset];
end;

function TTranslationManager.TranslateString(const Str,ID: string): string;
var
  I : Integer;
begin
  I:=ResourceIDList.IndexOf(ID);
  if I<0 then Result:=Str
  else Result:=Translations[ResourceIDList.Value[I]];
end;

function TTranslationManager.TranslateString(const Str: string; FormClass: TClass; ID: string; FormProperty: Boolean): string;
var
  I : Integer;
begin
  repeat
    I:=ResourceIDList.IndexOf(FormClass.ClassName+': '+ID);
    if I>=0 then Break;
    if FormProperty then Delete(ID,1,Length(FormClass.ClassName)-1);
    FormClass:=FormClass.ClassParent;
    if FormProperty then ID:=Copy(FormClass.ClassName,2,MaxInt)+ID;
  until not FormClass.InheritsFrom(TCustomForm);
  if I<0 then Result:=Str
  else Result:=Translations[ResourceIDList.Value[I]];
end;

procedure TTranslationManager.TranslateComponent(Component: TComponent; FormOrFrame: TScrollingWinControl);
var
  I : Integer;
  PropList : PPropList;
  Str : string;
  Obj : TObject;
begin
  // Translate string properties
  I:=GetPropList(Component,PropList);
  try
    for I:=0 to I-1 do
    begin
      if PropList^[I].PropType^.Kind=tkLString then
      begin
        if (PropList^[I].Name<>'Name') and IsStoredProp(Component,PropList^[I]) then
        begin
          Str:=GetStrProp(Component,PropList^[I]);
          if Str<>'' then
            SetStrProp(Component,PropList^[I],TranslateString(Str,FormOrFrame.ClassType,Component.Name+'.'+PropList^[I].Name,Component=FormOrFrame));
        end;
      end
      else if PropList^[I].PropType^.Kind=tkClass then
      begin                                 
        Obj:=GetObjectProp(Component,PropList^[I]);
        if Obj is TStrings then
        begin
          if (TStrings(Obj).Count>0) and IsStoredProp(Component,PropList^[I]) then
            TStrings(Obj).Text:=TranslateString(TStrings(Obj).Text,FormOrFrame.ClassType,Component.Name+'.'+PropList^[I].Name+'.Strings',False);
        end
        else if Obj is TFont then
        begin
          if (ReplaceCharSet<>0) and (TFont(Obj).Charset<>SYMBOL_CHARSET) then
            TFont(Obj).Charset:=ReplaceCharSet;
        end;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
  // Translate child components
  for I:=0 to Component.ComponentCount-1 do
    TranslateComponent(Component.Components[I],FormOrFrame);
end;

procedure TTranslationManager.TranslateForm(FormOrFrame: TScrollingWinControl);
begin
  TranslateComponent(FormOrFrame,FormOrFrame);
end;

procedure TTranslationManager.DecodeTranslationStrings;
var
  I : Integer;
begin
  for I:=0 to Translations.Count-1 do
    case Translations.Value[I] of
      1 : Translations[I]:=DecodeResourceString(DecodeDFMString(Translations[I]));
      2 : Translations[I]:=DecodeDFMString(Translations[I]);
    end;
end;

initialization
finalization
  TranslationManager.Free;
end.

