////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// LanguageSelector.pas - Delphi language resource selector
// --------------------------------------------------------
// Version:   2004-12-15
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Last changes:
//
//
// Use in project file:
//  Application.Title := 'Program name';
//
//  CreateSetup('Software\MeeSoft\Program name');
//  LoadSelectedLanguage(False);
//
//  Application.Initialize;
//  Application.CreateForm(TMainForm, MainForm);
//  ...
//
unit LanguageSelector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Settings, StdCtrls, StyleForm, FileUtils, VersionInfo, Monitor;

resourcestring
  rsProgramMustBeRestartedForChangesToTakeEffect = 'Program must be restarted for changes to take effect.';

const
  NativeLanguage = '0409_English';

type
  TLanguageItem = class(TMonitorObject)
  public
    LanguageFile : string;
    constructor Create(const LanguageFile: string);
  end;

  TLanguageSelectorForm = class(TStyleForm)
    ListBox: TListBox;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    class procedure Execute(ShowIfOnlyOne: Boolean);
  end;

procedure LoadSelectedLanguage(ReinitializeActiveForms: Boolean);

procedure ReinitializeForms;
function LoadNewResourceModule(const FileName: string): Boolean;
procedure SetResourceHInstance(NewInstance: Longint);
procedure RegisterInitProc(Proc: TProcedure);

var
  LanguageFile : string = NativeLanguage;
  ProjectIsTranslated : Boolean = False;

implementation

uses
  LanguagesDEPfix,
  TranslationTools;

{$R *.dfm}

var
  InitProcList : array of TProcedure = nil;

procedure LocalInitProc;
var
  I : Integer;
begin
  for I:=0 to High(InitProcList) do InitProcList[I];
end;

procedure RegisterInitProc(Proc: TProcedure);
var
  OldInitProc : Pointer;
begin
  SetLength(InitProcList,Length(InitProcList)+1);
  InitProcList[High(InitProcList)]:=Proc;
  OldInitProc:=InitProc;
  InitProc:=@LocalInitProc;
  if Assigned(OldInitProc) and (OldInitProc<>InitProc) then RegisterInitProc(OldInitProc);
end;

type
  TAsInheritedReader = class(TReader)
    public
      procedure ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer); override;
    end;

procedure TAsInheritedReader.ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer);
begin
  inherited ReadPrefix(Flags, AChildPos);
  Include(Flags, ffInherited);
end;

procedure SetResourceHInstance(NewInstance: Longint);
var
  CurModule: PLibModule;
begin
  CurModule:=LibModuleList;
  while CurModule<>nil do
  begin
    if CurModule.Instance=HInstance then
    begin
      if CurModule.ResInstance<>CurModule.Instance then
        FreeLibrary(CurModule.ResInstance);
      CurModule.ResInstance:=NewInstance;
      Exit;
    end;
    CurModule:=CurModule.Next;
  end;
end;

function LoadNewResourceModule(const FileName: string): Boolean;
var
  NewInst: Longint;
begin
  NewInst:=LoadLibraryEx(PChar(FileName),0,LOAD_LIBRARY_AS_DATAFILE);
  Result:=NewInst<>0;
  if Result then SetResourceHInstance(NewInst);
end;

function InternalReloadComponentRes(const ResName: string; HInst: THandle; var Instance: TComponent): Boolean;
var
  HRsrc: THandle;
  ResStream: TResourceStream;
  AsInheritedReader: TAsInheritedReader;
begin
  if HInst=0 then HInst:=HInstance;
  HRsrc:=FindResource(HInst,PChar(ResName),RT_RCDATA);
  Result:=HRsrc<>0;
  if not Result then Exit;
  ResStream:=TResourceStream.Create(HInst,ResName,RT_RCDATA);
  try
    AsInheritedReader:=TAsInheritedReader.Create(ResStream,4096);
    try
      Instance:=AsInheritedReader.ReadRootComponent(Instance);
    finally
      AsInheritedReader.Free;
    end;
  finally
    ResStream.Free;
  end;
  Result:=True;
end;

function ReloadInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  begin
    Result:=False;
    if (ClassType=TComponent) or (ClassType=RootAncestor) then Exit;
    Result:=InitComponent(ClassType.ClassParent);
    Result:=InternalReloadComponentRes(ClassType.ClassName,FindResourceHInstance(
      FindClassHInstance(ClassType)),Instance) or Result;
  end;

begin
  Result:=InitComponent(Instance.ClassType);
end;

procedure ReinitializeForms;
var
  I, Count : Integer;
  Form : TForm;
begin
  Count:=Screen.FormCount;
  for I:=0 to Count-1 do
  begin
    Form:=Screen.Forms[I];
    ReloadInheritedComponent(Form,TForm);
  end;
end;

procedure LoadSelectedLanguage(ReinitializeActiveForms: Boolean);
var
  ResourceFile : string;
  ResourceOk : Boolean;
begin
  if Setup=nil then raise Exception.Create('Setup not assigned');
  LanguageFile:=Setup.GetString('Language',NativeLanguage);
  if LanguageFile<>NativeLanguage then
  begin
    try
      ResourceFile:=ProgramPath+LanguageFile+'.lrs';
      if FileExists(ResourceFile+'.txt') then
      begin
        Assert(not ReinitializeActiveForms);
        ResourceFile:=ResourceFile+'.txt';
        TranslationTools.LoadTranslation(ResourceFile);
        ResourceOk:=True;
      end
      else
      begin
        with GetVersInfo(ResourceFile) do
          if (ThisApp.FileVersion.Major=FileVersion.Major) and (ThisApp.FileVersion.Minor=FileVersion.Minor) then
          begin
            ResourceOk:=LoadNewResourceModule(ResourceFile);
            if not ResourceOk then MessageDlg('Error loading language file: '+SysErrorMessage(GetLastError),mtError);
          end
          else
          begin
            MessageDlg('Language file from other program version selected.',mtError);
            ResourceOk:=False;
          end
      end;
    except
      on E: Exception do
      begin
        ResourceOk:=False;
        if FileExists(ResourceFile) then MessageDlg('Error loading language file: '+E.Message,mtError);
      end;
    end;
    if ResourceOk then
    begin
      ProjectIsTranslated:=True;
      if ReinitializeActiveForms then ReinitializeForms;
    end
    else
    begin
      Setup.WriteString('Language',NativeLanguage);
      LanguageFile:=NativeLanguage;
    end;
  end;
end;

//==============================================================================================================================
// TLanguageItem
//==============================================================================================================================
constructor TLanguageItem.Create(const LanguageFile: string);
begin
  inherited Create;
  Self.LanguageFile:=LanguageFile;
end;

//==============================================================================================================================
// TLanguageSelectorForm
//==============================================================================================================================
class procedure TLanguageSelectorForm.Execute(ShowIfOnlyOne: Boolean);
begin
  if Setup=nil then raise Exception.Create('Setup not assigned');
  with Create(nil,GetActiveFormHandle) do
  try
    if ShowIfOnlyOne or (ListBox.Items.Count>1) then ShowModal;
  finally
    Free;
  end;
end;

function LanguageName(LocaleID: Integer): string; overload;
begin
  try
    Result:=Languages.NameFromLocaleID[LocaleID];
  except
    Result:='Locale '+IntToHex(LocaleID,4);
  end;
end;

function LanguageName(const LanguageFileName: string): string; overload;
begin
  try
    Result:=Languages.NameFromLocaleID[StrToInt('$'+Copy(LanguageFileName,1,4))];
  except
    Result:=LanguageFileName;
  end;
end;

procedure TLanguageSelectorForm.FormCreate(Sender: TObject);
var
  I : Integer;
  Str : string;
begin
  Font.Color:=clWindowText;
  UseBackgroundTheme:=True;
  with ListBox do
  begin
    GetDirList(ProgramPath+'*.lrs*',Items);
    for I:=Items.Count-1 downto 0 do
    begin
      Str:=ExtractFileName(Items[I]);
      SetLength(Str,Pos('.',Str)-1);
      Items.Objects[I]:=TLanguageItem.Create(Str);
      Items[I]:=LanguageName(Str);
    end;
    Items.AddObject(LanguageName(NativeLanguage),TLanguageItem.Create(NativeLanguage));
    Sorted:=True;

    Str:=LanguageName(Setup.GetString('Language',''));
    if Str='' then Str:=LanguageName(GetThreadLocale);
    ItemIndex:=Items.IndexOf(Str);
    if ItemIndex=-1 then ItemIndex:=Items.IndexOf(LanguageName($0409));
  end;
end;

procedure TLanguageSelectorForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Language : string;
begin
  if ModalResult=mrOk then with ListBox do
  begin
    Language:=(Items.Objects[ItemIndex] as TLanguageItem).LanguageFile;
    if Language<>Setup.GetString('Language','') then
    begin
      Setup.WriteString('Language',Language);
      LoadSelectedLanguage(False);
      if ProjectIsTranslated then MessageDlg(rsProgramMustBeRestartedForChangesToTakeEffect,mtInformation);
    end;
  end;
end;

procedure TLanguageSelectorForm.OKButtonClick(Sender: TObject);
begin
  if ListBox.ItemIndex=-1 then MessageDlg('No language selected',mtError)
  else ModalResult:=mrOk;
end;

procedure TLanguageSelectorForm.FormDestroy(Sender: TObject);
var
  I : Integer;
begin
  with ListBox do
    for I:=0 to Items.Count-1 do Items.Objects[I].Free;
end;

end.

