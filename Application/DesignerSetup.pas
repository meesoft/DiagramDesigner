unit DesignerSetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  DiagramBase, StdCtrls, ValueEdits, Math, StyleForm, Settings, StringUtils,
  ExtCtrls, Buttons, MathUtils;

resourcestring
  rsMm = 'mm';
  rsCm = 'cm';
  rsInches = '"';
  rsPoints = 'points';
  rsDots = 'dots';
                        
type
  TDisplayUnits = (duMM,duCM,duInch,duPoints,du300,du600);

const
  DisplayUnitSize   : array[TDisplayUnits] of Double = (DesignerDPmm,DesignerDPmm*10,DesignerDPI,
                                                        DesignerDPpoint,DesignerDPI/300,DesignerDPI/600);
  DisplayUnitName   : array[TDisplayUnits] of PResStringRec = (@rsMm, @rsCm, @rsInches, @rsPoints, @rsDots, @rsDots);
  DisplayUnitFormat : array[TDisplayUnits] of string = ('0.0','0.00','0.00','0','0','0');

type
  TDesignerSetup = object
                     DisplayUnits           : TDisplayUnits;
                     Grid                   : TPoint;
                     ShowGrid               : Boolean;
                     ShowMargins            : Boolean;
                     ReversePrint           : Boolean;
                     PrintAsBitmap          : Boolean;
                     UndoHistory            : Integer;
                     ClipboardMetafileScale : Integer;
                     Antialiasing           : Boolean;
                     AutoConnectToLinks     : Boolean;
                     DefaultGroupLinks      : Boolean;
                     DefaultLinks           : string;
                     DefaultGroupAnchors    : Boolean;
                     DictionaryPath         : string;
                     PrintScaling           : Double;
                     procedure LoadSettings(Setup: TProgramSetup);
                     procedure SaveSettings(Setup: TProgramSetup);
                     function GetDefaultLinks: TFloatPointArray;
                   end;

  TDesignerSetupForm = class(TStyleForm)
    GridBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    GridXEdit: TFloatEdit;
    GridYEdit: TFloatEdit;
    Button1: TButton;
    Button2: TButton;
    ShowGridBox: TCheckBox;
    Label3: TLabel;
    UndoHistoryEdit: TIntegerEdit;
    Label4: TLabel;
    ClipboardScaleEdit: TIntegerEdit;
    Label5: TLabel;
    UnitsBox: TComboBox;
    LanguageButton: TBitBtn;
    DictionaryPathButton: TButton;
    Bevel1: TBevel;
    FileFormatAssociationsButton: TButton;
    Label7: TLabel;
    PrintScalingEdit: TFloatEdit;
    AntialiasingBox: TCheckBox;
    ShowMarginsBox: TCheckBox;
    DefaultGroupAnchorsBox: TCheckBox;
    DefaultGroupLinksBox: TCheckBox;
    PrintAsBitmapBox: TCheckBox;
    Bevel2: TBevel;
    LockGridBox: TCheckBox;
    AutoConnectToLinksBox: TCheckBox;
    EditDefaultLinksButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UnitsBoxChange(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure DictionaryPathButtonClick(Sender: TObject);
    procedure FileFormatAssociationsButtonClick(Sender: TObject);
    procedure GridXEditChangeValue(Sender: TObject);
    procedure GridYEditChangeValue(Sender: TObject);
    procedure EditDefaultLinksButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Options : TDesignerSetup;
    class function Execute(var AOptions : TDesignerSetup): Boolean;
  end;

function LinksToString(List: TFloatPointArray): string;
function LinksFromString(Str: string): TFloatPointArray;

resourcestring
  rsGridS = 'Grid (%s)';
  rsDictionariesDicDic = 'Dictionaries (*.dic)|*.dic';

implementation

uses LanguageSelector, FileUtils, SpellChecker, Main, FormatAssociation,
  FormatAssociationRegister, WinAPIUtils, LinkEditor, TranslationTools;

{$R *.dfm}

function LinksToString(List: TFloatPointArray): string;
var
  I : Integer;
begin
  for I:=0 to High(List) do
    Result:=Result+FormatFloat('0.0000',List[I].X)+'  '+FormatFloat('0.0000',List[I].Y)+#13;
end;

function LinksFromString(Str: string): TFloatPointArray;
var
  I : Integer;
  X, Y : Float;
  Found : Boolean;
begin
  repeat
    I:=Pos(' ',Str);
    if I=0 then Break;
    X:=StrToFloat(Trim(Copy(Str,1,I)));
    Delete(Str,1,I);
    I:=Pos(#13,Str);
    if I=0 then Break;
    Y:=StrToFloat(Trim(Copy(Str,1,I)));
    Delete(Str,1,I);
    Found:=False;
    for I:=0 to High(Result) do
      if (Result[I].X=X) and (Result[I].Y=Y) then
      begin
        Found:=True;
        Break;
      end;
    if not Found then
    begin
      I:=Length(Result);
      SetLength(Result,I+1);
      Result[I].X:=X;
      Result[I].Y:=Y;
    end;
  until Str='';
end;

//==============================================================================================================================
// TDesignerSetup
//==============================================================================================================================
procedure TDesignerSetup.LoadSettings(Setup : TProgramSetup);
begin
  Grid:=Point(Max(1,Setup.GetInteger('GridX',DesignerDPmm)),
              Max(1,Setup.GetInteger('GridY',DesignerDPmm)));
  ShowGrid:=Setup.GetBoolean('ShowGrid',False);
  ShowMargins:=Setup.GetBoolean('ShowMargins',False);
  ReversePrint:=Setup.GetBoolean('ReversePrint',False);
  PrintAsBitmap:=Setup.GetBoolean('PrintAsBitmap',True);
  DefaultGroupLinks:=Setup.GetBoolean('DefaultGroupLinks',True);
  DefaultGroupAnchors:=Setup.GetBoolean('DefaultGroupAnchors',True);
  AutoConnectToLinks:=Setup.GetBoolean('AutoConnectToLinks',True);
  UndoHistory:=Setup.GetInteger('UndoHistory',5);
  ClipboardMetafileScale:=Max(1,Setup.GetInteger('ClipboardScale',1));
  Antialiasing:=Setup.GetBoolean('Antialiasing',True);
  DisplayUnits:=TDisplayUnits(Setup.GetInteger('DisplayUnits',Integer(duMM)));
  DictionaryPath:=Setup.GetString('DictionaryPath',ProgramPath);
  DefaultLinks:=Setup.GetString('DefaultLinks','');
  ReplaceChar(DefaultLinks,';',#13);
  PrintScaling:=Max(1e-6,Setup.GetDouble('PrintScaling',1));
end;

procedure TDesignerSetup.SaveSettings(Setup : TProgramSetup);
begin
  Setup.WriteInteger('GridX',Grid.X);
  Setup.WriteInteger('GridY',Grid.Y);
  Setup.WriteBoolean('ShowGrid',ShowGrid);
  Setup.WriteBoolean('ShowMargins',ShowMargins);
  Setup.WriteBoolean('ReversePrint',ReversePrint);
  Setup.WriteBoolean('PrintAsBitmap',PrintAsBitmap);
  Setup.WriteBoolean('DefaultGroupLinks',DefaultGroupLinks);
  Setup.WriteBoolean('DefaultGroupAnchors',DefaultGroupAnchors);
  Setup.WriteBoolean('AutoConnectToLinks',AutoConnectToLinks);
  Setup.WriteInteger('UndoHistory',UndoHistory);
  Setup.WriteInteger('ClipboardScale',ClipboardMetafileScale);
  Setup.WriteBoolean('Antialiasing',Antialiasing);
  Setup.WriteInteger('DisplayUnits',Integer(DisplayUnits));
  Setup.WriteString('DictionaryPath',DictionaryPath);
  Setup.WriteString('DefaultLinks',StringReplace(DefaultLinks,#13,';',[rfReplaceAll]));
  Setup.WriteDouble('PrintScaling',PrintScaling);
end;

function TDesignerSetup.GetDefaultLinks: TFloatPointArray;
begin
  try
    Result:=LinksFromString(DefaultLinks);
    if Length(Result)=0 then Abort;
  except
    SetLength(Result,4);
    Result[0]:=FloatPoint(0.5,0);
    Result[1]:=FloatPoint(0.5,1);
    Result[2]:=FloatPoint(0,0.5);
    Result[3]:=FloatPoint(1,0.5);
  end;
end;

//==============================================================================================================================
// TDesignerSetupForm
//==============================================================================================================================
class function TDesignerSetupForm.Execute(var AOptions: TDesignerSetup): Boolean;
begin
  with Create(nil,GetActiveFormHandle) do
  try
    Options:=AOptions;

    ShowGridBox.Checked:=Options.ShowGrid;
    ShowMarginsBox.Checked:=Options.ShowMargins;
    DefaultGroupAnchorsBox.Checked:=Options.DefaultGroupAnchors;
    DefaultGroupLinksBox.Checked:=Options.DefaultGroupLinks;
    AutoConnectToLinksBox.Checked:=Options.AutoConnectToLinks;
    UndoHistoryEdit.Value:=Options.UndoHistory;
    ClipboardScaleEdit.Value:=Options.ClipboardMetafileScale;
    AntialiasingBox.Checked:=Options.Antialiasing;
    PrintScalingEdit.Value:=Options.PrintScaling;
    PrintAsBitmapBox.Checked:=Options.PrintAsBitmap;
    UnitsBox.ItemIndex:=Integer(Options.DisplayUnits);

    Result:=ShowModal=mrOk;
    if Result then AOptions:=Options;
  finally
    Free;
  end;
end;

procedure TDesignerSetupForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult=mrOk then
  begin
    Options.Grid.X:=Max(1,Round(GridXEdit.Value*DisplayUnitSize[Options.DisplayUnits]));
    Options.Grid.Y:=Max(1,Round(GridYEdit.Value*DisplayUnitSize[Options.DisplayUnits]));
    Options.ShowGrid:=ShowGridBox.Checked;
    Options.ShowMargins:=ShowMarginsBox.Checked;
    Options.DefaultGroupAnchors:=DefaultGroupAnchorsBox.Checked;
    Options.DefaultGroupLinks:=DefaultGroupLinksBox.Checked;
    Options.AutoConnectToLinks:=AutoConnectToLinksBox.Checked;
    Options.UndoHistory:=UndoHistoryEdit.Value;
    Options.ClipboardMetafileScale:=ClipboardScaleEdit.Value;
    Options.Antialiasing:=AntialiasingBox.Checked;
    Options.PrintScaling:=PrintScalingEdit.Value;
    Options.PrintAsBitmap:=PrintAsBitmapBox.Checked;
  end;
end;

procedure TDesignerSetupForm.FormShow(Sender: TObject);
begin
  GridBox.Caption:=Format(rsGridS,[TranslationManager.TranslateString(DisplayUnitName[Options.DisplayUnits])]);
  GridXEdit.FormatString:=DisplayUnitFormat[Options.DisplayUnits];
  GridXEdit.Max:=1001/DisplayUnitSize[Options.DisplayUnits]*DesignerDPmm;
  GridXEdit.Value:=Options.Grid.X/DisplayUnitSize[Options.DisplayUnits];
  GridYEdit.FormatString:=GridXEdit.FormatString;
  GridYEdit.Max:=GridXEdit.Max;
  GridYEdit.Value:=Options.Grid.Y/DisplayUnitSize[Options.DisplayUnits];
  GridXEdit.SelectAll;
  LockGridBox.Checked:=GridXEdit.Value=GridYEdit.Value;
  SetElevationRequiredState(FileFormatAssociationsButton);
end;

procedure TDesignerSetupForm.UnitsBoxChange(Sender: TObject);
begin
  Options.Grid.X:=Round(GridXEdit.Value*DisplayUnitSize[Options.DisplayUnits]);
  Options.Grid.Y:=Round(GridYEdit.Value*DisplayUnitSize[Options.DisplayUnits]);
  Options.DisplayUnits:=TDisplayUnits(UnitsBox.ItemIndex);
  FormShow(nil);
end;

procedure TDesignerSetupForm.LanguageButtonClick(Sender: TObject);
begin
  TLanguageSelectorForm.Execute(True);
end;

procedure TDesignerSetupForm.DictionaryPathButtonClick(Sender: TObject);
var
  Dictionary : string;
begin
  Dictionary:=Options.DictionaryPath;
  if OpenFileDialog(Dictionary,rsDictionariesDicDic) then
  begin
    Options.DictionaryPath:=ExtractFilePath(Dictionary);
    FreeAndNil(SpellCheckForm);
  end;
end;

procedure TDesignerSetupForm.FileFormatAssociationsButtonClick(Sender: TObject);
begin
  if WindowsIsVistaOrLater and not ProcessHasAdministratorPrivileges then
  try
    Enabled:=False;
    RunAsAdministrator(ParamStr(0),'**',True) // In Vista, run admin process
  finally
    Enabled:=True;
  end
  else
  begin
    Application.Title:=MainForm.ApplicationTitle;
    TFormatAssociateRegisterForm.Execute(rsDiagramFileFilter+'|'+rsTemplatePaletteFilter+'|',[0]);
    Application.Title:=MainForm.Caption;
  end;
end;
  
procedure TDesignerSetupForm.GridXEditChangeValue(Sender: TObject);
begin
  if LockGridBox.Checked then GridYEdit.Value:=GridXEdit.Value;
end;

procedure TDesignerSetupForm.GridYEditChangeValue(Sender: TObject);
begin
  if LockGridBox.Checked then GridXEdit.Value:=GridYEdit.Value;
end;

procedure TDesignerSetupForm.EditDefaultLinksButtonClick(Sender: TObject);
begin
  with TLinkEditorForm.Create(Self) do
  try
    List:=Options.GetDefaultLinks;
    if ShowModal=mrOk then Options.DefaultLinks:=LinksToString(List);
  finally
    Free;
  end;
end;

end.

