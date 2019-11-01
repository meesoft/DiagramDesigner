program DiagramDesigner;

uses
  FastMM4,
  Forms,
  SysUtils,
  ImageDLLLoader in '..\UNITS\ImageDLLLoader.pas',
  Main in 'Main.pas' {MainForm},
  DiagramBase in 'DiagramBase.pas',
  LinarBitmap in '..\UNITS\LinarBitmap.pas',
  ShapeObject in 'ShapeObject.pas',
  TemplateObjects in 'TemplateObjects.pas',
  ExpressionPlot in '..\UNITS\ExpressionPlot.pas' {ExpressionPlotForm},
  ExpressionHelp in '..\UNITS\ExpressionHelp.pas' {ExpressionEvalForm},
  ExpressionEval in '..\UNITS\ExpressionEval.pas',
  DesignerSetup in 'DesignerSetup.pas' {DesignerSetupForm},
  FormatAssociation in '..\Units\FormatAssociation.pas' {FormatAssociateForm},
  PropertyEditor in 'PropertyEditor.pas' {PropertyEditorForm},
  LineObject in 'LineObject.pas',
  PictureObject in 'PictureObject.pas',
  EventUtils in '..\UNITS\EventUtils.pas',
  GroupObject in 'GroupObject.pas',
  RearrangePages in 'RearrangePages.pas' {RearrangePagesForm},
  LinkEditor in 'LinkEditor.pas' {LinkEditorForm},
  ObjectTree in 'ObjectTree.pas',
  PleaseSupport in '..\Units\PleaseSupport.pas' {PleaseSupportForm},
  FlowchartObject in 'FlowchartObject.pas',
  PageProperties in 'PageProperties.pas' {PagePropertiesForm},
  LanguageSelector in '..\Units\LanguageSelector.pas' {LanguageSelectorForm},
  Settings in '..\UNITS\Settings.pas',
  About in '..\UNITS\About.pas' {AboutBox},
  PasteSpecial in 'PasteSpecial.pas' {PasteSpecialForm},
  ColorDialog in '..\UNITS\ColorDialog.pas',
  StyleForm in '..\UNITS\StyleForm.pas',
  SlideShow in 'SlideShow.pas' {SlideShowForm},
  SynSpellCheck in '..\LIBRARY\synspellcheck\SynSpellCheck.pas',
  SpellChecker in 'SpellChecker.pas' {SpellCheckForm},
  MathUtils in '..\UNITS\MathUtils.pas',
  ICOLoader in '..\UNITS\ICOLoader.pas' {IconSelectionForm},
  ValueEdits in '..\UNITS\ValueEdits.pas',
  ThemedBackground in '..\UNITS\ThemedBackground.pas',
  PanelFrame in '..\UNITS\PanelFrame.pas',
  TextEditor in 'TextEditor.pas' {TextEditorForm},
  MemUtils in '..\UNITS\MemUtils.pas',
  TranslationTools in '..\UNITS\TranslationTools.pas',
  QuickMenuSearch in '..\UNITS\QuickMenuSearch.pas' {QuickMenuSearchForm},
  QuickActionSearch in 'QuickActionSearch.pas' {QuickActionSearchForm},
  FormatAssociationRegister in 'FormatAssociationRegister.pas' {FormatAssociateRegisterForm},
  WinAPIUtils in '..\UNITS\WinAPIUtils.pas',
  FastBitmap in '..\UNITS\FastBitmap.pas',
  BitmapGammaInterpolation in '..\UNITS\BitmapGammaInterpolation.pas',
  DiagramAntialiasingDrawing in 'DiagramAntialiasingDrawing.pas',
  DiagramTextSearch in 'DiagramTextSearch.pas',
  FileUtils in '..\UNITS\FileUtils.pas',
  PrintDiagram in 'PrintDiagram.pas',
  DialogsEx in '..\UNITS\DialogsEx.pas',
  DiagramExport in 'DiagramExport.pas',
  StringUtils in '..\UNITS\StringUtils.pas',
  WMFLoader in '..\UNITS\WMFLoader.pas',
  FormScalingUtils in '..\UNITS\FormScalingUtils.pas',
  TextObject in 'TextObject.pas',
  StreamUtils in '..\UNITS\StreamUtils.pas';

{$R *.res}
{$R manifest-invoker.res}

begin
  Application.Title := 'Diagram Designer';
  if FileExists(ProgramPath+'DiagramDesigner.ini') then Setup:=TProgramSetupIni.Create(ProgramPath+'DiagramDesigner.ini')
  else CreateSetup('Software\MeeSoft\DiagramDesigner');
  LoadSelectedLanguage(False);
  if ParamStr(1)='**' then
  begin
    TFormatAssociateRegisterForm.Execute(rsDiagramFileFilter+'|'+rsTemplatePaletteFilter+'|',[0]);
    Exit;
  end;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

