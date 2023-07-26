unit Main;

{
* Check bug reports

* texture for shapes

* Better text position for 90° connectors

* SVG / VML export

* "replace text" function.

* grid size as a property of a page, now with different diagram you have to guess the grid size.

* backgound image for rectangle/ellipse

* I would like to have the rectangle not with a solid line but with dasch.

* Apply i Properties

* Resize page by dragging

* Flip direction on end arrow icons

}

//auto full zoom http://meesoft.logicnet.dk/support/viewtopic.php?pid=6280#p6280

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls,
  ActnMenus, StyleForm, About, ShellAPI, ComCtrls, PanelFrame, Math,
  ExtCtrls, DiagramBase, MathUtils, ImgList, TemplateObjects, StdCtrls, FileUtils,
  DesignerSetup, StringUtils, Consts, Menus, MemStream, Settings, Contnrs,
  StringLists, DialogsEx, Printers, ValueEdits, PropertyEditor, Buttons, FileCtrl,
  MMSystem, XPMan, DiagramTextSearch, PrintDiagram, Tabs;

resourcestring        
  rsIWouldLikeToThank = 'I would like to thank %s and others for making their software libraries available.';

const
  LibraryCreators =
    'Anders Melander, '+     // GIFImage
    'Brad Stowers, '+        // TdfsGradientForm
    'Gabriel Corneanu, '+    // JPEGEx
    'Jacob Dybala, '+        // TSynSpellCheck
    'Pierre le Riche, ' +    // FastMM 
    'Rune Møller Barnkob, '+ // CRC32
    'Vit Kovalcik';          // UniDIB

const
  PluginName         = 'DiagramReader.dll';
  ImageAnalyzer      = 'Analyzer.exe';
  RecentFileListSize = 5;

  sbpHint     = 0;
  sbpPage     = 1;
  sbpLayer    = 2;
  sbpMousePos = 3;

type
  TMouseMode = (mmEdit,mmZoom,mmHand,mmDrawObject);

  TMainForm = class(TStyleForm)
    AboutAction: TAction;
    ActionMainMenuBar: TActionMainMenuBar;
    ActionManager: TActionManager;
    AddTemplateAction: TAction;
    BringToFrontAction: TAction;
    Bringtofront1: TMenuItem;
    CanvasFrame: TDoubleBufferedPanel;
    Convert1: TMenuItem;
    Converttometafile1: TMenuItem;
    Converttopolygon1: TMenuItem;
    Copy1: TMenuItem;
    CopyAction: TAction;
    Cut1: TMenuItem;
    CutAction: TAction;
    Delete1: TMenuItem;
    DeleteAction: TAction;
    DeleteTemplateAction: TAction;
    Deletepage1: TMenuItem;
    Deletetemplateobject1: TMenuItem;
    DiagramFontAction: TAction;
    DrawArrowAction: TAction;
    DrawConnectorAction: TAction;
    DrawCurveAction: TAction;
    DrawLineAction: TAction;
    DrawPanel: TPanel;
    EditTextAction: TAction;
    Edittext1: TMenuItem;
    ExitAction: TAction;
    ExportAction: TAction;
    ExpressionEvaluator: TAction;
    Group1: TMenuItem;
    GroupAction: TAction;
    ImageList: TImageList;
    InsertPictureAction: TAction;
    InternetHelpAction: TAction;
    LoadTemplateAction: TAction;
    LoadTemplatePaletteItem: TMenuItem;
    MakeMetafileAction: TAction;
    MakePolygonAction: TAction;
    MouseEditAction: TAction;
    MoveCanvasAction: TAction;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    NewAction: TAction;
    NewPageAction: TAction;
    Newpage1: TMenuItem;
    OpenAction: TAction;
    OpenNewAction: TAction;
    OptionsAction: TAction;
    PageMenu: TPopupMenu;
    PagePropertiesAction: TAction;
    Paste1: TMenuItem;
    PasteAction: TAction;
    PopupMenu: TPopupMenu;
    PrintAction: TAction;
    PrintPreviewAction: TAction;
    Properties1: TMenuItem;
    PropertiesAction: TAction;
    RedoAction: TAction;
    ReloadAction: TAction;
    ReorderPagesAction: TAction;
    Reorderpages1: TMenuItem;
    ResentFilesMenu: TPopupMenu;
    SaveAction: TAction;
    SaveAsAction: TAction;
    SaveButton: TToolButton;
    SaveTemplateAction: TAction;
    SaveTemplatePaletteItem: TMenuItem;
    ScrollBarX: TScrollBar;
    ScrollBarY: TScrollBar;
    SelectAllAction: TAction;
    SendToBackAction: TAction;
    Sendtoback1: TMenuItem;
    ShowTreeAction: TAction;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    SupportAction: TAction;
    TemplateFrame: TDoubleBufferedPanel;
    TemplatePopupMenu: TPopupMenu;
    TemplatePropertiesAction: TAction;
    TemplatePropertiesItem: TMenuItem;
    TemplateScrollBox: TScrollBox;
    ToolBar1: TToolBar;
    ToolButtonRedo: TToolButton;
    Separator3: TToolButton;
    ToolButtonZoom: TToolButton;
    ToolButtonMoveCanvas: TToolButton;
    ToolButtonMouseEdit: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButtonOpen: TToolButton;
    Separator4: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonCut: TToolButton;
    Separator1: TToolButton;
    ToolButtonCopy: TToolButton;
    ToolButtonPaste: TToolButton;
    Separator2: TToolButton;
    ToolButtonUndo: TToolButton;
    TreeView: TTreeView;
    UndoAction: TAction;
    UngroupAction: TAction;
    ZoomAction: TAction;
    ZoomBox: TComboBox;
    Ungroup1: TMenuItem;
    EditLayer1Action: TAction;
    EditStencilAction: TAction;
    EditLayer2Action: TAction;
    EditLayer3Action: TAction;
    HelpFileAction: TAction;
    PasteSpecialAction: TAction;
    TemplateToPageAction: TAction;
    PageToTemplateAction: TAction;
    TemplatesToPageItem: TMenuItem;
    PageToTemplateItem: TMenuItem;
    N5: TMenuItem;
    SetLayerColorAction: TAction;
    ConnectLinksAction: TAction;
    Separator5: TToolButton;
    LineColorAction: TAction;
    FillColorAction: TAction;
    LineWidthAction: TAction;
    LineWidthButton: TSpeedButton;
    LineColorButton: TSpeedButton;
    FillColorButton: TSpeedButton;
    DrawRectangleAction: TAction;
    DrawEllipseAction: TAction;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    TextColorAction: TAction;
    TextColorButton: TSpeedButton;
    SlideShowAction: TAction;
    AlignCenterHAction: TAction;
    AlignRightAction: TAction;
    AlignLeftAction: TAction;
    AlignMenu: TMenuItem;
    Left1: TMenuItem;
    Center1: TMenuItem;
    Right1: TMenuItem;
    AlignTopAction: TAction;
    AlignBottomAction: TAction;
    op1: TMenuItem;
    Bottom1: TMenuItem;
    AlignCenterVAction: TAction;
    N6: TMenuItem;
    Center2: TMenuItem;
    SpellCheckAction: TAction;
    FlipLRAction: TAction;
    FlipUDAction: TAction;
    Rotate90Action: TAction;
    Rotate1: TMenuItem;
    Mirror1: TMenuItem;
    Flip1: TMenuItem;
    About1: TMenuItem;
    N7: TMenuItem;
    Rotate180Action: TAction;
    Rotate270Action: TAction;
    Rotate1801: TMenuItem;
    Addtemplate1: TMenuItem;
    RotateAction: TAction;
    Anyangle1: TMenuItem;
    AlignPageAction: TAction;
    N8: TMenuItem;
    Page1: TMenuItem;
    InheritLayerAction: TAction;
    FileMenuAction: TAction;
    EditMenuAction: TAction;
    DiagramMenuAction: TAction;
    HelpMenuAction: TAction;
    ObjectMenuAction: TAction;
    TemplatePaletteMenuAction: TAction;
    EditLayerMenuAction: TAction;
    RotateMenuAction: TAction;
    AlignMenuAction: TAction;
    DrawTextAction: TAction;
    ToolButton2: TToolButton;
    LoadTemplatePaletteMenu: TMenuItem;
    DefaultPaletteItem: TMenuItem;
    N9: TMenuItem;
    LayerMenu: TPopupMenu;
    Layer1Item: TMenuItem;
    Layer2Item: TMenuItem;
    Layer3Item: TMenuItem;
    GlobalStencilItem: TMenuItem;
    N10: TMenuItem;
    CornerRadiusAction: TAction;
    CornerRadiusButton: TSpeedButton;
    CheckForUpdatesAction: TAction;
    LineStartButton: TSpeedButton;
    LineEndButton: TSpeedButton;
    LineStartAction: TAction;
    LineEndAction: TAction;
    ObjectShadowsAction: TAction;
    FindTextAction: TAction;
    FindDialog: TFindDialog;
    GradientColorButton: TSpeedButton;
    GradientColorAction: TAction;
    LayerTabSet: TTabSet;
    PageTabSet: TTabSet;
    RightPanel: TPanel;
    TemplatePanel: TPanel;
    TemplateComboBox: TComboBox;
    LoadTemplateButton: TSpeedButton;
    AlignDistributeVAction: TAction;
    AlignDistributeHAction: TAction;
    Distribute1: TMenuItem;
    Distribute2: TMenuItem;
    GridEdit: TFloatEdit;
    AutoLineBreakAction: TAction;
    LineStartEndMenu: TPopupMenu;
    SolidConnectorLabelsAction: TAction;
    CutConnectorLabelsAction: TAction;
    OverlayConnectorLabelsAction: TAction;
    ConnectorLabelStyleMenuAction: TAction;
    procedure AboutActionExecute(Sender: TObject);
    procedure AddTemplateActionExecute(Sender: TObject);
    procedure BringToFrontActionExecute(Sender: TObject);
    procedure CanvasFrameDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CanvasFrameDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure CanvasFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CanvasFrameMouseMove(Sender: TObject; Shift: TShiftState; MX,MY: Integer);
    procedure CanvasFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CanvasFramePaint(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure DeleteTemplateActionExecute(Sender: TObject);
    procedure DiagramFontActionExecute(Sender: TObject);
    procedure DrawObjectActionExecute(Sender: TObject);
    procedure DrawPanelResize(Sender: TObject);
    procedure EditTextActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure ExportActionExecute(Sender: TObject);
    procedure ExpressionEvaluatorExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure GroupActionExecute(Sender: TObject);
    procedure InsertPictureActionExecute(Sender: TObject);
    procedure InternetHelpActionExecute(Sender: TObject);
    procedure LoadTemplateActionExecute(Sender: TObject);
    procedure MakeMetafileActionExecute(Sender: TObject);
    procedure MakePolygonActionExecute(Sender: TObject);
    procedure MouseEditActionExecute(Sender: TObject);
    procedure MoveCanvasActionExecute(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure NewPageActionExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
    procedure OpenNewActionExecute(Sender: TObject);
    procedure OptionsActionExecute(Sender: TObject);
    procedure PageMenuPopup(Sender: TObject);
    procedure PagePropertiesActionExecute(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure PrintActionExecute(Sender: TObject);
    procedure PrintPreviewActionExecute(Sender: TObject);
    procedure PropertiesActionExecute(Sender: TObject);
    procedure ReloadActionExecute(Sender: TObject);
    procedure ReorderPagesActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure SaveTemplateActionExecute(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure SendToBackActionExecute(Sender: TObject);
    procedure ShowTreeActionExecute(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StatusBarResize(Sender: TObject);
    procedure SupportActionExecute(Sender: TObject);
    procedure TemplateFrameDblClick(Sender: TObject);
    procedure TemplateFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TemplateFramePaint(Sender: TObject);
    procedure TemplatePopupMenuPopup(Sender: TObject);
    procedure TemplatePropertiesActionExecute(Sender: TObject);
    procedure TemplateScrollBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UndoRedoExecute(Sender: TObject);
    procedure UngroupActionExecute(Sender: TObject);
    procedure ZoomActionExecute(Sender: TObject);
    procedure ZoomBoxClick(Sender: TObject);
    procedure SetLayerActionExecute(Sender: TObject);
    procedure HelpFileActionExecute(Sender: TObject);
    procedure ResentFilesMenuPopup(Sender: TObject);
    procedure TemplateToPageActionExecute(Sender: TObject);
    procedure PageToTemplateActionExecute(Sender: TObject);
    procedure SetLayerColorActionExecute(Sender: TObject);
    procedure ConnectLinksActionExecute(Sender: TObject);
    procedure ZoomBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditBoxEnter(Sender: TObject);
    procedure TreeViewEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TreeViewExit(Sender: TObject);
    procedure CanvasFrameDblClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeViewDblClick(Sender: TObject);
    procedure LineWidthActionExecute(Sender: TObject);
    procedure LineColorActionExecute(Sender: TObject);
    procedure FillColorActionExecute(Sender: TObject);
    procedure TextColorActionExecute(Sender: TObject);
    procedure TreeViewEnter(Sender: TObject);
    procedure SlideShowActionExecute(Sender: TObject);
    procedure AlignActionExecute(Sender: TObject);
    procedure SpellCheckActionExecute(Sender: TObject);
    procedure RotateActionExecute(Sender: TObject);
    procedure InheritLayerActionExecute(Sender: TObject);
    procedure LoadTemplatePaletteMenuClick(Sender: TObject);
    procedure LoadTemplatePaletteItemClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CornerRadiusActionExecute(Sender: TObject);
    procedure CheckForUpdatesActionExecute(Sender: TObject);
    procedure TemplateFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure LineStartActionExecute(Sender: TObject);
    procedure LineEndActionExecute(Sender: TObject);
    procedure ObjectShadowsActionExecute(Sender: TObject);
    procedure TemplateFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FindTextActionExecute(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure GradientColorActionExecute(Sender: TObject);
    procedure FillColorButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LineColorButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GradientColorButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LayerTabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure PageTabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure TemplateComboBoxDropDown(Sender: TObject);
    procedure TemplateComboBoxChange(Sender: TObject);
    procedure GridEditChangeValue(Sender: TObject);
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AutoLineBreakActionExecute(Sender: TObject);
    procedure TreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ConnectorLabelStyleActionExecute(Sender: TObject);
  private
    // File
    FFileName : string;
    PalettePath : string;
    SavedToFile : Boolean;
    Templates : TTemplateSheet;
    PageActions : TObjectList;
    RecentFileList : TStringList;
    TextSearch : TDiagramTextSearch;
    DiagramPrinter : TDiagramPrinter;
    // Painting
    CanvasInfo, TemplateCanvasInfo : TCanvasInfo;
    ZBuffer : TBitmap;
    FActivePage, FActiveLayer : Integer;
    FRepaint, FUpdateControlStates, RepaintTemplate : Boolean;
    FScreenScale, Zoom : Double;
    ShowLinkPoints : Boolean;
    PrinterMargins : TRect;
    // Mouse
    MousePos, LastMousePos : TPoint;
    SelectStart, SelectEnd : TPoint;
    FActiveObject, TemplateObject, DontFocusObject : TBaseObject;
    ActiveObjectHandle : Integer;
    FMouseMode, FPrevMouseMode : TMouseMode;
    DrawObject : TBaseObjectClass;
    DrawAction : TAction;
    LastPointClickTime : DWord;
    // Clipboard
    Clipboard : TMemStream;
    // Undo/redo
    History : TStringObjectList;
    ObjectMoved : Boolean;
    UndoCount, RedoCount : Integer;
    DefaultProperties : TPropertyObject;

    procedure SetActiveObject(ActiveObject: TBaseObject);
    procedure SelectPageActionExecute(Sender: TObject);
    procedure UpdateScrollBars;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure SetControlStates;
    procedure WMDestroyClipboard(var Msg: TWMDestroyClipboard); message WM_DESTROYCLIPBOARD;
    procedure WMRenderFormat(var Msg: TWMRenderFormat); message WM_RENDERFORMAT;
    procedure WMRenderAllFormats(var Msg: TWMRenderAllFormats); message WM_RENDERALLFORMATS;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure SetMouseMode(MouseMode: TMouseMode);
    procedure ShowHint(Sender: TObject);
    procedure ResetHistory;
    procedure AddToHistory;
    procedure SetFileName(const FileName: string);
    procedure AddRecentFile(const FileName: string);
    procedure UpdateRecentFileList;
    procedure LoadRecentFileClick(Sender: TObject);
    procedure LoadTemplatePalette(PaletteName: string);
    procedure UpdateObjectTree;
    procedure UpdateStyleButtons;
    procedure SetActivePageIndex(Index: Integer);
    procedure SetActiveLayerIndex(Index: Integer);
    procedure RemoveMenuActions(Client: TActionClient);
    procedure HandleLineStartEndAction(PropType: TObjectProperty; Delta: Integer);
    procedure LineStartEndItemClick(Sender: TObject);
    procedure UpdatePrinterMargins;
    procedure UpdatePageTabs;
    procedure SetGridValue;
    function TreeViewDragObject(Source: TObject; X, Y: Integer; Apply: Boolean): Boolean;
  public
    ApplicationTitle : string;
    AnalyzerPath, PicturePath : string;
    Options : TDesignerSetup;
    Modified : Integer;
    Diagram : TDiagramContainer;
    destructor Destroy; override;
    procedure InitializeDrawing;
    property ActivePageIndex: Integer read FActivePage write SetActivePageIndex;
    property ActiveLayerIndex: Integer read FActiveLayer write SetActiveLayerIndex;
    property MouseMode: TMouseMode read FMouseMode write SetMouseMode;
    property FileName: string read FFileName write SetFileName;
    property ActiveObject: TBaseObject read FActiveObject write SetActiveObject;
    property ScreenScale: Double read FScreenScale;
    function GetActivePage: TDiagramPage;
    function GetActiveLayer: TDiagramLayer;
    procedure UpdateDrawing;
    procedure UpdateControlStates;
    procedure UpdateTemplates;
    procedure StoreUndoPoint(Title: string; UpdateTree: Boolean);
  end;

var
  MainForm : TMainForm;

resourcestring
  rsRename = 'rename';
  rsEditProperties = 'edit properties';
  rsOpen = 'Open';
  rsDiagramModifiedSaveBeforeClosing = 'Diagram modified, save before closing?';
  rsReloadAndLoseChanges = 'Reload and lose changes?';
  rsUndo = 'Undo';
  rsRedo = 'Redo';
  rsUnsupportedClipboardFormat = 'Unsupported clipboard format';
  rsKnownImageFormats = 'Known image formats';
  rsAddPoint = 'add point';
  rsAddS = 'add %s';
  rsMove = 'move';
  rsEdit = 'Edit';
  rsBitmap = 'Bitmap';
  rsText = 'Text';
  rsNoObjectsInActiveLayer = 'No objects in active layer';
  rsOverwriteTemplatePaletteContentsWithObjectsFromActiveLayer = 'Overwrite template palette contents with objects from active layer?';
  rsLeftRightClickToAddPoint = 'Left+right click to add point';
  rs1_4Points = '¼ points';
  rsLineWidth = 'Line width';
  rsCornerRadius = 'Corner radius';
  rsSize = 'size';
  rsNoDictionariesFound = 'No dictionaries installed or dictionary path not set in the Options dialog.';
  rsRotationAngle = 'Rotation angle';
  rsRotate = 'Rotate';
  rsRelativePageNumber1PreviousPage = 'Relative page number (-1 = previous page):';
  rsLayerNumber = 'Layer number:';
  rsNoFilesFound = 'No files found';
  rsLayerD = 'Layer %d';
  rsUseMouseWheelToChange = 'Turn mouse wheel to change';
  rsLeftClickToSet = 'Left click to set';
  rsRightClickToClear = 'Right click to clear';
  rsUseMouseWheelToChangeSize = 'Turn mouse wheel to change size';
  rsTextNotFound = 'Text not found';
  rsEditConnectorHint = 'Hold Ctrl to draw horizontal or vertical lines only. Alt to ignore grid. Shift to ignore links.';
  rsEditShapeHint = 'Hold Ctrl to keep height and width equal. Alt to ignore grid. Shift to scale around center.';
  rsPage = 'Page';

implementation

uses
  ImageDLLLoader, WMFLoader, PNGLoader, BMPLoader, JPEGLoader, PCXLoader, ICOLoader, GIFLoader, CR2Loader, 
  ExpressionHelp, ExpressionEval, Types, MemUtils, EventUtils, Cursors,
  ShapeObject, DynamicLists, FormatAssociation, LineObject, LinarBitmap,
  PictureObject, GroupObject, RearrangePages, ObjectTree, PleaseSupport, ColorDialog,
  PageProperties, LanguageSelector, PasteSpecial, SlideShow, VersionInfo,
  SpellChecker, StrUtils, ThemedBackground, TextEditor, QuickActionSearch,
  FormatAssociationRegister, WinAPIUtils, FastBitmap, 
  DiagramAntialiasingDrawing, DiagramExport, TranslationTools, FormScalingUtils, TextObject;

{$R *.dfm}

function ItemCaption(Action: TAction): string;
var
  I : Integer;
begin
  Result:=Action.Caption;
  I:=Length(Result);
  while (I>0) and (Result[I]='.') do Dec(I);
  SetLength(Result,I);
  I:=Pos('&',Result);
  if (I<>0) and (Result[I+1]<>' ') then Delete(Result,I,1);
end;

procedure UpdateLineWidthButton(Button: TSpeedButton; Width: Integer);
begin
  if Button.Tag<>Width then
  with Button.Glyph.Canvas do
  begin
    Button.Tag:=Width;
    Brush.Style:=bsSolid;
    Brush.Color:=clFuchsia;
    FillRect(Rect(0,0,16,15));
    Width:=Round(Width*(4/DesignerDPpoint));
    if Width<1 then
    begin
      Width:=1;
      Brush.Color:=$a0a0a0;
    end
    else
    begin
      if Width>15 then Width:=15;
      Brush.Color:=clBlack;
    end;
    FillRect(Bounds(0,7-Width div 2,16,Width));
  end;
end;

procedure UpdateColorButton(Button: TSpeedButton; Color: TColor);
begin
  if Button.Tag<>Color then
  with Button.Glyph.Canvas do
  begin
    Button.Tag:=Color;
    if Color=clNone then
    begin
      Brush.Color:=clFuchsia;
      FillRect(Rect(1,9,15,15));
      Pen.Color:=clDkGray;
      MoveTo(1,9);
      LineTo(15,14);
      MoveTo(1,14);
      LineTo(15,9);
    end
    else
    begin
      if Color=clFuchsia then Color:=clFuchsia-1;
      Brush.Color:=Color;
      FillRect(Rect(1,9,15,15));
    end;
  end;
end;

procedure UpdateCornerRadiusButton(Button: TSpeedButton; Radius: Integer);
var
  Diameter : Integer;
begin
  if Button.Tag<>Radius then
  with Button.Glyph.Canvas do
  begin
    Button.Tag:=Radius;
    Diameter:=Min(16,Radius div 250);
    Radius:=Diameter div 2;
    Brush.Color:=clFuchsia;
    FillRect(Rect(0,0,12,12));
    MoveTo(0,8);
    LineTo(9-Radius,8);
    MoveTo(8,0);
    LineTo(8,9-Radius);
    Arc(8-Diameter,8-Diameter,9,9,8-Radius,1000,1000,8-Radius);
  end;
end;

type
  TConnectorGlypDraw = class(TBaseConnectorObject);

procedure UpdateLineStartEndButton(Button: TSpeedButton; Marker,Direction: Integer);
var
  Size : Integer;
  Obj : TConnectorGlypDraw;
  CanvasInfo : TCanvasInfo;
  Pos, Dir : TPoint;
begin
  if Button.Tag<>Lo(Marker) then
  begin
    Button.Tag:=Lo(Marker);
    Size:=Button.Glyph.Width;

    if Lo(Marker) in [leStop,leCircle,leBall] then Pos:=Point(Size div 2,Size div 2)
    else Pos:=Point(Size div 2-7*Direction,Size div 2);
    Dir:=Point(Size div 2+9*Direction,Size div 2);

    with Button.Glyph.Canvas do
    begin
      Brush.Color:=clFuchsia;
      FillRect(Rect(0,0,Size,Size));
      with Pos do MoveTo(X,Y);
      with Dir do LineTo(X,Y);
    end;

    CanvasInfo.Scale.X:=96/DesignerDPI; // This is the only part that is used in DrawLineEnd

    Obj:=TConnectorGlypDraw.Create;
    Obj.FFillColor:=clWhite;
    Obj.FLineWidth:=DefaultLineWidth*3 div 2;
    Obj.DrawLineEnd(Button.Glyph.Canvas,CanvasInfo,Pos,Dir,Lo(Marker) or $300);
    Obj.Free;
  end;
end;

//==============================================================================================================================
// Initialization
//==============================================================================================================================
procedure TMainForm.FormCreate(Sender: TObject);
begin
  UseBackgroundTheme:=False;
  ApplicationTitle:=Application.Title;
  Caption:=ApplicationTitle;
  Diagram:=TDiagramContainer.Create;
  Templates:=TTemplateSheet.Create;
  FScreenScale:=Screen.PixelsPerInch/DesignerDPI;
  if FScreenScale<=0 then FScreenScale:=96/DesignerDPI;
  LastMousePos.X:=Low(Integer);
  SelectStart.X:=Low(Integer);
  AddIdleEvent(Self,ApplicationIdle);
  History:=TStringObjectList.Create;
  RecentFileList:=TStringList.Create;
  RecentFileList.Delimiter:='|';
  Application.OnActivate:=FormActivate;
  DefaultProperties:=TPropertyObject.Create;
  Zoom:=1;
  CanvasInfo.Scale:=FloatPoint(1,1);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(Setup) then
  begin
    Options.SaveSettings(Setup);
    Setup.WriteInteger('TreeWidth',TreeView.Width);
    Setup.WriteInteger('TemplateWidth',RightPanel.Width);
    Setup.WriteString('RecentFileList',RecentFileList.DelimitedText);
    Setup.WriteString('PalettePath',PalettePath);
    SaveWindowPos(Self);
  end;
  RemoveIdleEvent(Self);
  DiagramPrinter.Free;
  ZBuffer.Free;
  Templates.Free;
  History.Free;
  RecentFileList.Free;
  PageActions.Free;
  DefaultProperties.Free;
  TextSearch.Free;
end;

destructor TMainForm.Destroy;
begin
  inherited;
  Diagram.Free; // Only free after WM_RENDERALLFORMATS
end;

procedure TMainForm.RemoveMenuActions(Client: TActionClient);
begin
  if Client is TActionClientItem then
    with TActionClientItem(Client) do
      if Assigned(Action) and not Assigned(Action.OnExecute) then
        Action:=nil;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  I : Integer;
  Str, Ext : string;
  MenuItem : TMenuItem;
begin
  Set8087CW(DefaultFPUControlWord);

  if ProjectIsTranslated then DefaultDefines.Reset;
  ThemedBackground.Enable;
  SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  SetSystemFont(Font);

  with TProgramSetupRegistry.Create('Software\MeeSoft\ImageAnalyzer') do
  try
    if ReadString('AnalyzerPath',AnalyzerPath) and FileExists(AnalyzerPath+ImageAnalyzer) then
    begin
      PicturePath:=GetString('ImagePath','');
      if GetFileSize(ProgramPath+PluginName)<>GetFileSize(AnalyzerPath+PluginName) then
        CopyFile(ProgramPath+PluginName,AnalyzerPath+PluginName,True);
    end
    else
    begin
      AnalyzerPath:=ProgramPath;
      WriteString('AnalyzerPath',AnalyzerPath);
    end;
  finally
    Free;
  end;
  ImageDLLLoader.Default.FindDLLs(AnalyzerPath);
  RestoreWindowPos(Self);
  Options.LoadSettings(Setup);
  TreeView.Width:=Max(20,Setup.GetInteger('TreeWidth',TreeView.Width));
  RightPanel.Width:=Max(20,Setup.GetInteger('TemplateWidth',RightPanel.Width));
  ShowTreeAction.Checked:=TreeView.Width>1;
  RecentFileList.DelimitedText:=Setup.GetString('RecentFileList','');
  SetGridValue;

  // Update help menu
  HelpFileAction.Visible:=FileExists(ProgramPath+'DiagramDesigner.chm');
  if HelpFileAction.Visible then
  begin
    HelpFileAction.ShortCut:=InternetHelpAction.ShortCut;
    InternetHelpAction.ShortCut:=0;
  end;

  // Prepare templates
  TemplateCanvasInfo.Scale:=FloatPoint(ScreenScale*0.5,ScreenScale*0.5);
  TemplateCanvasInfo.DrawMode:=dmRender;
  TemplateCanvasInfo.DefaultFont:=DrawPanel.Font;
  try
    LoadTemplatePalette(Setup.GetString('PaletteName',ProgramPath+'Flowchart.ddt'))
  except
    LoadTemplatePalette('');
    PalettePath:=Setup.GetString('PalettePath',ProgramPath);
  end;
  DrawPanel.Color:=ColorToRGB(ActionMainMenuBar.ColorMap.SelectedColor);
  TemplateScrollBox.Color:=ColorTone(DrawPanel.Color,clWhite,1,2);

  EnableVistaViewStyle(TreeView);

  // Build line start/end style menu
  LineStartButton.Glyph.Width:=ImageList.Width;
  LineStartButton.Glyph.Height:=ImageList.Height;
  for I:=Low(LineEnds) to High(LineEnds) do
  begin
    MenuItem:=TMenuItem.Create(LineStartEndMenu);
    MenuItem.Caption:=LoadResString(LineEndNames[I]);
    MenuItem.Tag:=LineEnds[I];
    MenuItem.OnClick:=LineStartEndItemClick;
    LineStartEndMenu.Items.Add(MenuItem);
    UpdateLineStartEndButton(LineStartButton,LineEnds[I],1);
    MenuItem.ImageIndex:=ImageList.AddMasked(LineStartButton.Glyph,clFuchsia);
  end;

  // Prepare new drawing
  CanvasInfo.Scale:=FloatPoint(ScreenScale*1,ScreenScale*1);
  CanvasInfo.DrawMode:=dmEditing;
  CanvasInfo.DefaultFont:=DrawPanel.Font;
  ZBuffer:=TBitmap.Create;
  ZBuffer.PixelFormat:=pf32bit;
  UpdatePrinterMargins;

  // Prepare default property buttons
  DefaultProperties.Properties[opLineWidth]:=DefaultLineWidth;
  DefaultProperties.Properties[opLineColor]:=clBlack;
  DefaultProperties.Properties[opFillColor]:=clWhite;
  DefaultProperties.Properties[opGradientColor]:=clNone;
  DefaultProperties.Properties[opTextColor]:=clBlack;
  DefaultProperties.Properties[opCornerRadius]:=0;
  DefaultProperties.Properties[opLineStart]:=leNone or $200;
  DefaultProperties.Properties[opLineEnd]:=leArrow3 or $200;
  UpdateStyleButtons;
  ZoomBox.Items.Add(rsPage);

  DrawPanelResize(nil);
  DragAcceptFiles(Handle,True);
  RegisterDesignerClipboardFormat;
  Application.OnHint:=ShowHint;

  LoadTemplateButton.Caption:='';
  AlignCenterVAction.Hint:=AlignCenterHAction.Hint;
  AlignLeftAction.Hint:=AlignCenterHAction.Hint;
  AlignRightAction.Hint:=AlignCenterHAction.Hint;
  AlignTopAction.Hint:=AlignCenterHAction.Hint;
  AlignBottomAction.Hint:=AlignCenterHAction.Hint;
  AlignPageAction.Hint:=AlignCenterHAction.Hint;
  AlignDistributeHAction.Hint:=AlignCenterHAction.Hint;
  AlignDistributeVAction.Hint:=AlignCenterHAction.Hint;
  Rotate90Action.Hint:=RotateAction.Hint;
  Rotate180Action.Hint:=RotateAction.Hint;
  Rotate270Action.Hint:=RotateAction.Hint;
  // Copy menu action captions to empty hints
  for I:=ActionManager.ActionCount-1 downto 0 do
    with TAction(ActionManager.Actions[I]) do
      if Hint='' then Hint:=StripHotkey(StripTrailing3Dots(Caption));
  with DrawLineAction do Hint:=Hint+#13+rsLeftRightClickToAddPoint;
  with DrawArrowAction do Hint:=Hint+#13+rsLeftRightClickToAddPoint;
  with DrawConnectorAction do Hint:=Hint+#13+rsLeftRightClickToAddPoint;
  with DrawCurveAction do Hint:=Hint+#13+rsLeftRightClickToAddPoint;
  with LineWidthAction do Hint:=Hint+#13+rsLeftClickToSet+#13+rsUseMouseWheelToChange;
  with LineColorAction do Hint:=Hint+#13+rsLeftClickToSet+#13+rsRightClickToClear;
  with FillColorAction do Hint:=Hint+#13+rsLeftClickToSet+#13+rsRightClickToClear;
  with GradientColorAction do Hint:=Hint+#13+rsLeftClickToSet+#13+rsRightClickToClear+#13+rsUseMouseWheelToChange;
  with TextColorAction do Hint:=Hint+#13+rsLeftClickToSet;
  with CornerRadiusAction do Hint:=Hint+#13+rsLeftClickToSet+#13+rsUseMouseWheelToChange;
  with LineStartAction do Hint:=Hint+#13+rsLeftClickToSet+#13+rsUseMouseWheelToChangeSize;
  with LineEndAction do Hint:=Hint+#13+rsLeftClickToSet+#13+rsUseMouseWheelToChangeSize;
  // Remove dummy actions used for translation of menu captions
  ActionManager.ActionBars.IterateClients(ActionManager.ActionBars,RemoveMenuActions);
  // Auto assign menu hotkeys
  ActionManager.ActionBars.AutoHotKeys:=True;
  DrawAction:=DrawConnectorAction;

  if Diagram.Count=0 then
  begin
    UpdateRecentFileList;
    if ParamCount>0 then
    begin
      Str:=ParamStr(1);
      if Str='*' then
      begin
        NewActionExecute(nil);
        if WindowsIsVistaOrLater and not ProcessHasAdministratorPrivileges then
        begin
          RunAsAdministrator(ParamStr(0),'*',False); // In Vista, run admin process
          Close;
        end
        else
        begin
          TLanguageSelectorForm.Execute(False);
          TFormatAssociateRegisterForm.Execute(rsDiagramFileFilter+'|'+rsTemplatePaletteFilter+'|',[0],True,True);
        end;
      end
      else
      begin
        Ext:=ExtractFileExtNoDotUpper(Str);
        if Ext='DDT' then
        begin
          Str:=ExpandFileName(Str);
          NewActionExecute(nil);
          LoadTemplatePalette(Str);
        end
        else if WMFLoader.Default.CanLoad(EXT) or
                JPEGLoader.Default.CanLoad(EXT) or
                PNGLoader.Default.CanLoad(EXT) or
                BMPLoader.Default.CanLoad(EXT) then
        begin
          NewActionExecute(nil);
          PicturePath:=Str;
          InsertPictureActionExecute(nil);
          if ExtractFileExtNoDotUpper(ParamStr(2))='DDT' then
          begin
            LoadTemplatePalette(ExpandFileName(ParamStr(2)));
          end;
        end
        else
        begin
          try
            if not FileExists(Str) then Str:=GetParameterFileName;
            FileName:=ExpandFileName(Str);
            OpenActionExecute(nil);
          except
            NewActionExecute(nil);
            raise;
          end;
          if ExtractFileExtNoDotUpper(ParamStr(2))='DDT' then
          begin
            LoadTemplatePalette(ExpandFileName(ParamStr(2)));
          end;
        end;
      end;
    end
    else NewActionExecute(nil);
  end;
  CanvasFrame.Visible:=True;
end;

procedure TMainForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  if FUpdateControlStates then SetControlStates;
end;

procedure TMainForm.SetControlStates;
begin
  if Assigned(ActiveObject) and not ActiveObject.Selected then
  begin
    ActiveObject.Selected:=True;
    UpdateDrawing;
  end;
  FUpdateControlStates:=False;
  // File
  ReloadAction.Enabled:=SavedToFile;
  if Assigned(SaveButton) then SaveButton.Enabled:=Modified<>0;
  // Edit
  DeleteAction.Enabled:=Assigned(ActiveObject);
  CutAction.Enabled:=Assigned(ActiveObject);
  CopyAction.Enabled:=Assigned(ActiveObject);
  PasteAction.Enabled:=True;
  UndoAction.Enabled:=UndoCount>0;
  RedoAction.Enabled:=RedoCount>0;
  // Object
  EditTextAction.Enabled:=(ActiveObject is TTextObject) or
                          ((ActiveObject is TGroupObject) and (TGroupObject(ActiveObject).TextObject<>nil)) or
                          (Assigned(TreeView.Selected) and (TObject(TreeView.Selected.Data) is TTextObject));
  PropertiesAction.Enabled:=Assigned(ActiveObject) or Assigned(TreeView.Selected);
  BringToFrontAction.Enabled:=Assigned(ActiveObject) or Assigned(TreeView.Selected);
  SendToBackAction.Enabled:=Assigned(ActiveObject) or Assigned(TreeView.Selected);
  GroupAction.Enabled:=Assigned(ActiveObject);
  UngroupAction.Enabled:=ActiveObject is TGroupObject;
  MakeMetafileAction.Enabled:=Assigned(ActiveObject);
  MakePolygonAction.Enabled:=(ActiveObject is TGroupObject) or (ActiveObject is TCurveLineObject);
  AddTemplateAction.Enabled:=Assigned(ActiveObject);
  AlignLeftAction.Enabled:=Assigned(ActiveObject);
  AlignCenterHAction.Enabled:=Assigned(ActiveObject);
  AlignRightAction.Enabled:=Assigned(ActiveObject);
  AlignTopAction.Enabled:=Assigned(ActiveObject);
  AlignCenterVAction.Enabled:=Assigned(ActiveObject);
  AlignBottomAction.Enabled:=Assigned(ActiveObject);
  AlignDistributeVAction.Enabled:=Assigned(ActiveObject);
  AlignDistributeHAction.Enabled:=Assigned(ActiveObject);
  AlignPageAction.Enabled:=Assigned(ActiveObject);
  FlipLRAction.Enabled:=Assigned(ActiveObject);
  FlipUDAction.Enabled:=Assigned(ActiveObject);
  Rotate90Action.Enabled:=Assigned(ActiveObject);
  Rotate180Action.Enabled:=Assigned(ActiveObject);
  Rotate270Action.Enabled:=Assigned(ActiveObject);
  RotateAction.Enabled:=Assigned(ActiveObject);
end;

procedure TMainForm.UpdateControlStates;
begin
  FUpdateControlStates:=True;
end;

procedure TMainForm.SetMouseMode(MouseMode: TMouseMode);
begin
  FMouseMode:=MouseMode;
  ZoomAction.Checked:=False;
  MoveCanvasAction.Checked:=False;
  MouseEditAction.Checked:=False;
  if Assigned(DrawAction) and (MouseMode<>mmDrawObject) then DrawAction.Checked:=False;
  case MouseMode of
    mmZoom       : begin
                     ZoomAction.Checked:=True;
                     CanvasFrame.Cursor:=crZoom;
                   end;
    mmHand       : begin
                     MoveCanvasAction.Checked:=True;
                     CanvasFrame.Cursor:=crHand;
                   end;
    mmEdit       : begin
                     MouseEditAction.Checked:=True;
                     CanvasFrame.Cursor:=crDefault;
                   end;
    mmDrawObject : CanvasFrame.Cursor:=crCross;
  end;
  if MouseMode=mmZoom then CanvasFrame.PopupMenu:=nil
  else CanvasFrame.PopupMenu:=PopupMenu;
  if MouseMode<>mmEdit then ShowLinkPoints:=False;
end;

procedure TMainForm.ShowHint(Sender: TObject);
var
  Str : string;
  I : Integer;
begin
  Monitor.ClassType; // Hopefully reinitialize the monitor handler to avoid rare AVs
  Str:=Application.Hint;
  for I:=Length(Str)-1 downto 2 do
    if Str[I]=#13 then
    begin
      Str[I]:=' ';
      if Str[I-1]<>'.' then Insert('.',Str,I);
      if Str[Length(Str)]<>'.' then Str:=Str+'.';
    end;
  StatusBar.Panels[0].Text:=Str;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin                           
  if Active then ActiveControl:=nil;
end;

//==============================================================================================================================
// File
//==============================================================================================================================
procedure TMainForm.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ResetHistory;
begin
  History.Clear;
  UndoCount:=0;
  RedoCount:=0;
  UpdateControlStates;
end;

procedure TMainForm.InitializeDrawing;
begin
  Modified:=0;
  UpdatePageTabs;
  ObjectShadowsAction.Checked:=Diagram.ObjectShadows;
  AutoLineBreakAction.Checked:=Diagram.AutoLineBreak;
  SolidConnectorLabelsAction.Checked:=Diagram.ConnectorLabelStyle=clsSolid;
  CutConnectorLabelsAction.Checked:=Diagram.ConnectorLabelStyle=clsCut;
  OverlayConnectorLabelsAction.Checked:=Diagram.ConnectorLabelStyle=clsOverlay;
  FActivePage:=-1; SetActivePageIndex(0);
  ZoomBox.Text:=IntToStr(Setup.GetInteger('Zoom',100));
  ZoomBoxClick(nil);
  ScrollBarX.Position:=-8;
  ScrollBarY.Position:=-8;
end;

procedure TMainForm.SetFileName(const FileName: string);
begin
  FFileName:=FileName;
  if FileName='' then Caption:=ApplicationTitle
  else Caption:=RemoveFileExt(ExtractFileName(FileName))+' - '+ApplicationTitle;
  Application.Title:=Caption;
end;

procedure TMainForm.NewActionExecute(Sender: TObject);
begin
  if CloseQuery then
  begin
    Diagram.New;
    FileName:='';
    InitializeDrawing;
    if Assigned(Sender) then PagePropertiesActionExecute(nil);
    CanvasFrame.Visible:=True;
  end;
end;

procedure TMainForm.OpenActionExecute(Sender: TObject);
var
  NewFile : string;
begin
  NewFile:=FileName;
  if (Sender=nil) or (CloseQuery and OpenPictureDialog(NewFile,rsDiagramFileFilter)) then
  begin
    Screen.Cursor:=crHourGlass;
    try
      UpdateControlStates;
      FileName:='';
      ActiveObject:=nil;
      UpdateDrawing;
      try
        Diagram.LoadFromFile(NewFile);
      finally
        if Diagram.DefaultFontName<>'' then
        begin
          DrawPanel.Font.Name:=Diagram.DefaultFontName;
          DrawPanel.Font.Size:=Diagram.DefaultFontSize;
          DrawPanel.Font.Style:=TFontStyles(Byte(Diagram.DefaultFontStyle));
          DrawPanel.Font.Charset:=Diagram.DefaultFontCharSet;
        end
        else
        begin
          DrawPanel.Font.Name:='Arial';
          DrawPanel.Font.Size:=10;
          DrawPanel.Font.Style:=[];
          DrawPanel.Font.Charset:=1;
        end;
        FileName:=NewFile;
        AddRecentFile(FileName);
        InitializeDrawing;
      end;
      SavedToFile:=True;
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
  ActiveControl:=nil;
end;

procedure TMainForm.OpenNewActionExecute(Sender: TObject);
var
  NewFile : string;
begin
  NewFile:=FileName;
  if OpenFileDialog(NewFile,rsDiagramFileFilter) then
    if ExecuteFile(ParamStr(0),'"'+NewFile+'"')<=32 then RaiseLastOSError;
end;

procedure TMainForm.SaveActionExecute(Sender: TObject);
begin
  if SavedToFile and (FileName<>'') then
  try
    Screen.Cursor:=crHourGlass;
    try
      Diagram.SaveToFile(FileName);
      Modified:=0;
      UpdateControlStates;
    finally
      Screen.Cursor:=crDefault;
    end;
  except
    Application.HandleException(Self);
    SaveAsActionExecute(Sender);
  end
  else SaveAsActionExecute(Sender);
end;

procedure TMainForm.SaveAsActionExecute(Sender: TObject);
begin
  if SaveFileDialog(FFileName,rsDiagramFileFilter) then
  begin
    FileName:=FFileName;
    SavedToFile:=True;
    try
      SaveActionExecute(Sender);
      AddRecentFile(FileName);
    except
      SavedToFile:=False;
      raise;
    end;
  end;
  ActiveControl:=nil;
end;

procedure TMainForm.ExportActionExecute(Sender: TObject);
begin
  with TExportDiagramDialog.Create(Self) do
  try
    Options:=Options+[ofOverwritePrompt];
    Execute(ExtractFilePath(FileName),Diagram,ActivePageIndex,DrawPanel.Font,Self.Options);
  finally
    Free;
  end;
  ActiveControl:=nil;
end;

procedure TMainForm.UpdateRecentFileList;
var
  I : Integer;
  NewItem : TMenuItem;
begin
  for I:=RecentFileList.Count-1 downto 0 do
  begin
    if I>=RecentFileListSize then
    begin
      if Assigned(RecentFileList.Objects[I]) then RecentFileList.Objects[I].Free;
      RecentFileList.Delete(I);
    end
    else if RecentFileList.Objects[I]=nil then
    begin
      NewItem:=TMenuItem.Create(ResentFilesMenu);
      NewItem.Caption:=IntToStr(RecentFileList.Count-I)+' '+DoubleAcceleratorMarker(ExtractFileName(RemoveFileExt(RecentFileList[I])));
      NewItem.Hint:=rsOpen+' '+RecentFileList[I];
      NewItem.OnClick:=LoadRecentFileClick;
      RecentFileList.Objects[I]:=NewItem;
      ResentFilesMenu.Items.Add(NewItem);
    end
    else with TMenuItem(RecentFileList.Objects[I]) do
    begin
      Caption:=IntToStr(RecentFileList.Count-I)+' '+ExtractFileName(RemoveFileExt(RecentFileList[I]));
      Hint:=rsOpen+' '+RecentFileList[I];
    end;
  end;
end;

procedure TMainForm.AddRecentFile(const FileName: string);
var
  I : Integer;
begin
  I:=RecentFileList.IndexOf(FileName);
  if I=-1 then
  begin
    RecentFileList.Insert(0,FileName);
    UpdateRecentFileList;
  end
  else RecentFileList.Move(I,0);
end;

procedure TMainForm.ResentFilesMenuPopup(Sender: TObject);
var
  I : Integer;
  Exists : Boolean;
begin
  for I:=0 to RecentFileList.Count-1 do
    if Assigned(RecentFileList.Objects[I]) then
    begin
      if RecentFileList[I]='' then Exists:=False
      else if (RecentFileList[I][1]<>'\') and
              (TDriveType(GetDriveType(PChar(Copy(RecentFileList[I],1,3)))) in [dtFixed,dtRAM]) then
          Exists:=FileExists(RecentFileList[I])
      else Exists:=True;
      TMenuItem(RecentFileList.Objects[I]).Enabled:=Exists;
    end;
end;

procedure TMainForm.LoadRecentFileClick(Sender: TObject);
var
  I : Integer;
begin
  I:=RecentFileList.IndexOfObject(Sender);
  if I>=0 then
  begin
    if RecentFileList[I]=FileName then ReloadActionExecute(nil)
    else if CloseQuery then
    begin
      FileName:=RecentFileList[I];
      OpenActionExecute(nil);
    end;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Modified<>0 then
    case MessageDlg(rsDiagramModifiedSaveBeforeClosing,mtConfirmation,[mbYes,mbNo,mbCancel]) of
      mrYes    : begin
                   SaveActionExecute(nil);
                   CanClose:=Modified=0;
                 end;
      mrNo     : CanClose:=True;
      mrCancel : CanClose:=False;
    end;
  ActiveControl:=nil;
end;

procedure TMainForm.ReloadActionExecute(Sender: TObject);
begin
  if (Modified=0) or (MessageDlg(rsReloadAndLoseChanges,mtConfirmation,mbYesNo)=mrYes) then
    OpenActionExecute(nil);
  ActiveControl:=nil; 
end;

procedure TMainForm.OptionsActionExecute(Sender: TObject);
begin
  if TDesignerSetupForm.Execute(Options) then
  begin                                      
    UpdateScrollBars;
    UpdatePrinterMargins;
    UpdateDrawing;
    UpdateControlStates;
    UpdateTemplates;
    if (Options.UndoHistory>0) and (History.Count=0) then AddToHistory;
    SetGridValue;
  end;
  ActiveControl:=nil;
end;

procedure TMainForm.SetGridValue;
begin
  GridEdit.Hint:=Format(rsGridS,[TranslationManager.TranslateString(DisplayUnitName[Options.DisplayUnits])]);
  GridEdit.FormatString:=DisplayUnitFormat[Options.DisplayUnits];
  GridEdit.Max:=1001/DisplayUnitSize[Options.DisplayUnits]*DesignerDPmm;
  if Options.Grid.X=Options.Grid.Y then GridEdit.Value:=Options.Grid.X/DisplayUnitSize[Options.DisplayUnits]
  else GridEdit.Text:='<>';
end;

procedure TMainForm.GridEditChangeValue(Sender: TObject);
begin
  if GridEdit.Valid then
  begin
    Options.Grid.X:=Max(1,Round(GridEdit.Value*DisplayUnitSize[Options.DisplayUnits]));
    Options.Grid.Y:=Options.Grid.X;
    UpdateDrawing;
  end;
end;

procedure TMainForm.ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE : ActiveControl:=nil;
    VK_F10    : FormKeyDown(nil,Key,Shift);
  end;
end;

procedure TMainForm.PrintActionExecute(Sender: TObject);
begin
  if DiagramPrinter=nil then DiagramPrinter:=TDiagramPrinter.Create;
  DiagramPrinter.Options:=Options;
  DiagramPrinter.Diagram:=Diagram;
  DiagramPrinter.ActivePage:=GetActivePage;
  DiagramPrinter.FileName:=FileName;
  DiagramPrinter.DrawPanelFont:=DrawPanel.Font;
  DiagramPrinter.Print;
  UpdatePrinterMargins;
  ActiveControl:=nil;
end;

procedure TMainForm.PrintPreviewActionExecute(Sender: TObject);
begin
  PrintPreviewAction.Checked:=not PrintPreviewAction.Checked;
  UpdateDrawing;
end;

procedure TMainForm.SlideShowActionExecute(Sender: TObject);
begin
  with TSlideShowForm.Create(nil) do
  try
    Page:=ActivePageIndex;
    CanvasInfo.DefaultFont:=Self.CanvasInfo.DefaultFont;
    Slides:=Diagram;
    Antialiasing:=Options.Antialiasing;
    ShowModal;
    ActivePageIndex:=Page;
  finally
    Free;
  end;
  ActiveControl:=nil;
end;

//==============================================================================================================================
// Templates
//==============================================================================================================================

procedure TMainForm.DeleteTemplateActionExecute(Sender: TObject);
begin
  if TemplateObject<>nil then
  begin
    Templates.Delete(Templates.IndexOf(TemplateObject));
    TemplateObject:=nil;
    Templates.UpdateHeight;
    UpdateTemplates;
  end;
end;

procedure TMainForm.LoadTemplateActionExecute(Sender: TObject);
var
  PaletteName : string;
begin
  PaletteName:=PalettePath;
  if OpenPictureDialog(PaletteName,rsTemplatePaletteFilter) then
    LoadTemplatePalette(PaletteName);
  ActiveControl:=nil;
end;

procedure TMainForm.SaveTemplateActionExecute(Sender: TObject);
var
  PaletteName : string;
begin
  PaletteName:=PalettePath;
  if SaveFileDialog(PaletteName,rsTemplatePaletteFilter) then
  begin
    PalettePath:=ExtractFilePath(PaletteName);
    Templates.SaveToFile(PaletteName);
  end;
  ActiveControl:=nil;
end;

procedure TMainForm.TemplateToPageActionExecute(Sender: TObject);
begin
  NewActionExecute(nil);
  if Modified<>0 then Exit;
  ActiveLayerIndex:=0;
  Modified:=1;
  Templates.CopyToPage(GetActivePage);
  ResetHistory;
  AddToHistory;
  UpdateDrawing;
  UpdateScrollBars;
  UpdateObjectTree;
end;

procedure TMainForm.PageToTemplateActionExecute(Sender: TObject);
begin
  if GetActiveLayer.Count=0 then MessageDlg(rsNoObjectsInActiveLayer,mtError)
  else if MessageDlg(rsOverwriteTemplatePaletteContentsWithObjectsFromActiveLayer,mtConfirmation,mbYesNo)=mrYes then
  begin
    Templates.Clear;
    Templates.AddCopy(GetActiveLayer);
    Templates.UpdateHeight;
    UpdateTemplates;
  end;
end;

//==============================================================================================================================
// Edit
//==============================================================================================================================

procedure TMainForm.AddToHistory;
var
  LayerImage : TMemStream;
begin
  if Options.UndoHistory>0 then
  begin
    LayerImage:=TMemStream.Create;
    try
      GetActiveLayer.SaveToStream(LayerImage);
    except
      LayerImage.Free;
      raise;
    end;
    History.AddObject('',LayerImage);
  end;
end;

procedure TMainForm.StoreUndoPoint(Title: string; UpdateTree: Boolean);
var
  I : Integer;
begin
  if RedoCount<>0 then
  begin
    for I:=1 to Min(RedoCount,History.Count) do History.Delete(History.Count-1);
    RedoCount:=0;
  end;
  if Options.UndoHistory>0 then
  begin
    Inc(UndoCount);
    while UndoCount>Options.UndoHistory do
    begin
      Dec(UndoCount);
      History.Delete(0);
    end;
    Title:=StripTrailing3Dots(Title);
    History[History.Count-1]:=Title;
    UndoAction.Hint:=rsUndo+' '+Title;
    RedoAction.Hint:=rsRedo;
    UndoAction.Caption:=UndoAction.Hint;
    RedoAction.Caption:=RedoAction.Hint;
    AddToHistory;
  end;
  ObjectMoved:=False;
  Inc(Modified);
  if Modified<=0 then Modified:=UndoCount+1;
  UpdateControlStates;
  UpdateDrawing;
  if UpdateTree then UpdateObjectTree;
end;

procedure TMainForm.UndoRedoExecute(Sender: TObject);
var
  LayerImage : TMemStream;
  Delta : Integer;
begin
  ActiveObject:=nil;
  TreeView.Selected:=nil;
  if Sender=RedoAction then
  begin
    if RedoCount=0 then Exit;
    Delta:=1
  end
  else
  begin
    if UndoCount=0 then Exit;
    Delta:=-1;
  end;
  Inc(UndoCount,Delta);
  Dec(RedoCount,Delta);
  LayerImage:=TMemStream(History.Objects[UndoCount]);
  Inc(Modified,Delta);
  with GetActiveLayer do
  begin
    Clear;
    LayerImage.Position:=0;
    LoadFromStream(LayerImage,CurrentFileVersion);
  end;

  if UndoCount>0 then UndoAction.Hint:=rsUndo+' '+History[UndoCount-1]
  else UndoAction.Hint:=rsUndo;

  if RedoCount>0 then RedoAction.Hint:=rsRedo+' '+History[UndoCount]
  else RedoAction.Hint:=rsRedo;

  UndoAction.Caption:=UndoAction.Hint;
  RedoAction.Caption:=RedoAction.Hint;

  UpdateDrawing;
  UpdateControlStates;
  UpdateObjectTree;
end;

procedure TMainForm.SelectAllActionExecute(Sender: TObject);
begin
  with GetActiveLayer do if Count>0 then
  begin
    SelectAll;
    if ActiveObject=nil then ActiveObject:=Objects[Count-1];
    UpdateDrawing;
  end;
end;

procedure TMainForm.DeleteActionExecute(Sender: TObject);
begin
  ActiveObject:=nil;
  GetActiveLayer.DeleteSelected;
  StoreUndoPoint(DeleteAction.Caption,True);
end;

procedure TMainForm.CutActionExecute(Sender: TObject);
begin
  CopyActionExecute(nil);
  DeleteActionExecute(nil);
end;

procedure TMainForm.CopyActionExecute(Sender: TObject);
begin
  if ActiveObject=nil then Exit;
  if not OpenClipboard(Handle) then RaiseLastOSError;
  try
    if not EmptyClipboard then RaiseLastOSError;
    ActiveObject.Selected:=True;

    if Assigned(Clipboard) then Clipboard.Reset
    else Clipboard:=TMemStream.Create;
    GetActiveLayer.SaveSelected(Clipboard);

    SetClipboardData(DesignerClipboardFormat,0);
    SetClipboardData(CF_ENHMETAFILE,0);
    SetClipboardData(CF_DIB,0);
  finally
    CloseClipboard;
  end;
end;

procedure TMainForm.WMDestroyClipboard(var Msg: TWMDestroyClipboard);
begin
  FreeAndNil(Clipboard);
  Msg.Result:=0;
end;

procedure TMainForm.WMRenderFormat(var Msg: TWMRenderFormat);
const
  MetafileMargin = 1;
var
  DataHandle : THandle;
  PaletteHandle : HPalette;
  Data : PInteger;
  Metafile : TMetafile;
  Bitmap : TFastBitmap;
  MetafileCanvas : TMetafileCanvas;
  MetafileCanvasInfo : TCanvasInfo;
  Objects : TDiagramLayer;
  Bounds : TRect;
  Format : Word;
  Shadow : TPoint;
  ExtraSize : Double;
begin
  if Clipboard=nil then Exit;
  DataHandle:=0;
  if Msg.Format=DesignerClipboardFormat then // Diagram designer format
  begin
    DataHandle:=GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE,Clipboard.Size+4);
    Data:=GlobalLock(DataHandle);
    try
      Data^:=Clipboard.Size;
      Inc(Data);
      Clipboard.Position:=0;
      Clipboard.Read(Data^,Clipboard.Size);
    finally
      GlobalUnLock(DataHandle);
    end;
  end
  else if Msg.Format=CF_ENHMETAFILE then // Enhanced metafile format
  begin
    Objects:=TDiagramLayer.Create;
    try                     
      Clipboard.Position:=0;
      Objects.LoadFromStream(Clipboard,CurrentFileVersion);
      {if (Objects.Count=1) and (Objects.Objects[0] is TMetafileObject) then // Copy metafiles "raw"
      begin
        TMetafile(Objects.Objects[0].Properties[opMetafile]).SaveToClipboardFormat(Format,DataHandle,PaletteHandle);
        Assert(Format=Msg.Format);
        SetClipboardData(CF_PALETTE,PaletteHandle);
      end
      else}
      begin
        Bounds:=Objects.GetBounds;
        ZeroMem(MetafileCanvasInfo,SizeOf(MetafileCanvasInfo));
        with MetafileCanvasInfo do
        begin
          Scale:=FloatPoint(ScreenScale*Options.ClipboardMetafileScale,ScreenScale*Options.ClipboardMetafileScale);
          DefaultFont:=DrawPanel.Font;
          Offset:=Point(Round(-Bounds.Left*Scale.X)+MetafileMargin,Round(-Bounds.Top*Scale.Y)+MetafileMargin);
          Container:=Diagram;
        end;

        Metafile:=TMetafile.Create;
        try
          if Diagram.ObjectShadows then ExtraSize:=ShadowOffset
          else ExtraSize:=0;
          Metafile.Width:=Round((Bounds.Right-Bounds.Left+ExtraSize)*MetafileCanvasInfo.Scale.X)+MetafileMargin*2;
          Metafile.Height:=Round((Bounds.Bottom-Bounds.Top+ExtraSize)*MetafileCanvasInfo.Scale.Y)+MetafileMargin*2;
          MetafileCanvas:=TMetafileCanvas.CreateWithComment(Metafile,0,ApplicationTitle,'');
          try
            if Diagram.ObjectShadows then
            begin
              MetafileCanvasInfo.DrawMode:=dmShadow;
              Shadow:=RoundPoint(ShadowOffset*MetafileCanvasInfo.Scale.X,ShadowOffset*MetafileCanvasInfo.Scale.Y);
              MetafileCanvasInfo.Offset:=VectorAdd(MetafileCanvasInfo.Offset,Shadow);
              Objects.DrawShadow(MetafileCanvas,MetafileCanvasInfo);
              MetafileCanvasInfo.Offset:=VectorSubtract(MetafileCanvasInfo.Offset,Shadow);
            end;
            MetafileCanvasInfo.DrawMode:=dmRender;
            Objects.Draw(MetafileCanvas,MetafileCanvasInfo);
          finally
            MetafileCanvas.Free;
          end;
          Metafile.SaveToClipboardFormat(Format,DataHandle,PaletteHandle);
          Assert(Format=Msg.Format);
          SetClipboardData(CF_PALETTE,PaletteHandle);
        finally
          Metafile.Free;
        end;
      end;
    finally
      Objects.Free;
    end;
  end
  else if Msg.Format=CF_DIB then // Bitmap
  begin
    Objects:=TDiagramLayer.Create;
    try
      Clipboard.Position:=0;
      Objects.LoadFromStream(Clipboard,CurrentFileVersion);
      Bounds:=Objects.GetBounds;
      ZeroMem(MetafileCanvasInfo,SizeOf(MetafileCanvasInfo));
      with MetafileCanvasInfo do
      begin
        Scale:=FloatPoint(ScreenScale*Options.ClipboardMetafileScale,
                          ScreenScale*Options.ClipboardMetafileScale);
        DrawMode:=dmRender;
        DefaultFont:=DrawPanel.Font;
        Offset:=Point(Round(-Bounds.Left*Scale.X)+MetafileMargin,Round(-Bounds.Top*Scale.Y)+MetafileMargin);
        Container:=Diagram;
      end;
      if Diagram.ObjectShadows then ExtraSize:=ShadowOffset
      else ExtraSize:=0;
      Bitmap:=TFastBitmap.Create(Round((Bounds.Right-Bounds.Left+ExtraSize)*MetafileCanvasInfo.Scale.X)+MetafileMargin*2,
                                 Round((Bounds.Bottom-Bounds.Top+ExtraSize)*MetafileCanvasInfo.Scale.Y)+MetafileMargin*2,
                                 pf32bit);
      try
        DrawToBitmap(Objects,Bitmap,MetafileCanvasInfo,Options.Antialiasing);
        Bitmap.PixelFormat:=pf24bit;
        with TLinearBitmap.Create(Bitmap) do
        try
          CopyToClipboard(False);
        finally
          Free;
        end;
      finally
        Bitmap.Free;
      end;
    finally
      Objects.Free;
    end;
  end;
  if DataHandle<>0 then SetClipboardData(Msg.Format,DataHandle);
end;

procedure TMainForm.WMRenderAllFormats(var Msg: TWMRenderAllFormats);
var
  RenderMsg : TWMRenderFormat;
  LocalClipboard : TMemStream;
begin
  if Assigned(Clipboard) and OpenClipboard(Handle) then
  try
    LocalClipboard:=Clipboard;
    Clipboard:=nil;
    EmptyClipboard;
    Clipboard:=LocalClipboard;
    RenderMsg.Format:=DesignerClipboardFormat;
    WMRenderFormat(RenderMsg);
    RenderMsg.Format:=CF_ENHMETAFILE;
    WMRenderFormat(RenderMsg);
    RenderMsg.Format:=CF_DIB;
    WMRenderFormat(RenderMsg);
    FreeAndNil(Clipboard);
    Msg.Result:=0;
  finally
    CloseClipboard;
  end;
end;

procedure TMainForm.PasteActionExecute(Sender: TObject);

  procedure PasteDesignerObject;
  var
    DataHandle : THandle;
    Data : PInteger;
    Stream : TMemBlockStream;
    Size : Integer;
    Center : TPoint;
  begin
    DataHandle:=GetClipboardData(DesignerClipboardFormat);
    Data:=GlobalLock(DataHandle);
    if Data=nil then RaiseLastOSError;
    try
      Size:=Data^;
      Inc(Data);
      Stream:=TMemBlockStream.Create(Data,Size);
      try
        ActiveObject:=nil;
        with GetActiveLayer do
        begin
          DeselectAll;
          LoadSelected(Stream);

          with Options do if (Grid.X>1) or (Grid.Y>1) then // Move pasted objects one grid step
            MoveSelected(Grid.X,Grid.Y,-1,NoGrid,[]);

          // If pasted outside screen, move in
          Center:=CenterPoint(GetSelectedBounds);
          if not PtInRect(CanvasInfo.ObjectRect(Rect(0,0,CanvasFrame.Width,CanvasFrame.Height)),Center) then
            MoveSelected(MousePos.X-Center.X,MousePos.Y-Center.Y,0,Options.Grid,[]);

          ActiveObject:=Objects[Count-1];
        end;
      finally
        Stream.Free;
      end;
    finally
      GlobalUnlock(DataHandle);
    end;
  end;

  procedure PasteBitmap;
  var
    Image : TLinearBitmap;
  begin
    CloseClipboard;
    Image:=TLinearBitmap.Create;
    try
      Image.GetFromClipboard;
      ActiveObject:=TBitmapObject.CreateNew(Image);
      ActiveObject.Position:=Bounds(FloorInt(MousePos.X,Options.Grid.X),FloorInt(MousePos.Y,Options.Grid.Y),
                                    Round((Image.Width-1)*DesignerDPI/Screen.PixelsPerInch),
                                    Round((Image.Height-1)*DesignerDPI/Screen.PixelsPerInch));
    finally
      Image.Free;
    end;
    if Options.DefaultGroupLinks then ActiveObject.Links:=Options.GetDefaultLinks;
    with GetActiveLayer do
    begin
      DeselectAll;
      Add(ActiveObject);
      ActiveObject.Selected:=True;
    end;
  end;

  procedure PasteMetafile;
  var
    Metafile : TMetafile;
  begin
    Metafile:=TMetafile.Create;
    try
      Metafile.LoadFromClipboardFormat(CF_ENHMETAFILE,GetClipboardData(CF_ENHMETAFILE),GetClipboardData(CF_PALETTE));
      ActiveObject:=TMetafileObject.CreateNew(Metafile);
      ActiveObject.Position:=Bounds(FloorInt(MousePos.X,Options.Grid.X),FloorInt(MousePos.Y,Options.Grid.Y),
                                    Round(Metafile.MMWidth*DesignerDPmm/100),
                                    Round(Metafile.MMHeight*DesignerDPmm/100));
    finally
      Metafile.Free;
    end;
    if Options.DefaultGroupLinks then ActiveObject.Links:=Options.GetDefaultLinks;
    with GetActiveLayer do
    begin
      DeselectAll;
      Add(ActiveObject);
      ActiveObject.Selected:=True;
    end;
  end;

  procedure PasteText;
  var
    DataHandle : THandle;
    Data : PInteger;
    Text : string;
  begin
    DataHandle:=GetClipboardData(CF_TEXT);
    Data:=GlobalLock(DataHandle);
    if Data=nil then RaiseLastOSError;
    try
      Text:=PChar(Data);
    finally
      GlobalUnlock(DataHandle);
    end;
    ReplaceChar(Text,#13,'\');
    ReplaceChar(Text,#10,'n');
    ActiveObject:=TTextObject.CreateNew(Text);
    with CanvasFrame.BitmapCanvas do
    begin
      Font:=DrawPanel.Font;
      ActiveObject.Position:=Bounds(FloorInt(MousePos.X,Options.Grid.X),FloorInt(MousePos.Y,Options.Grid.Y),
                                    Round(TextWidth(Text)/ScreenScale),
                                    Round(TextHeight(Text)/ScreenScale));
    end;
    with GetActiveLayer do
    begin
      DeselectAll;
      Add(ActiveObject);
      ActiveObject.Selected:=True;
    end;
  end;

var
  Format : Integer;
begin
  if not OpenClipboard(Handle) then RaiseLastOSError;
  try
    if Sender=PasteSpecialAction then // Let user choose format
    with TPasteSpecialForm.Create(nil,Handle) do
    try
      if IsClipboardFormatAvailable(DesignerClipboardFormat) then ListBox.AddItem(ApplicationTitle,TObject(DesignerClipboardFormat));
      if IsClipboardFormatAvailable(CF_ENHMETAFILE) then ListBox.AddItem(rsWindowsMetafile,TObject(CF_ENHMETAFILE));
      if IsClipboardFormatAvailable(CF_DIB) then ListBox.AddItem(rsBitmap,TObject(CF_DIB));
      if IsClipboardFormatAvailable(CF_TEXT) then ListBox.AddItem(rsText,TObject(CF_TEXT));
      if ListBox.Items.Count>0 then                                     
      begin
        ListBox.ItemIndex:=0;
        if ShowModal<>mrOk then Abort;
        Format:=Integer(ListBox.Items.Objects[ListBox.ItemIndex]);
      end
      else Format:=0;
    finally
      Free;
    end
    else // Find a format that we know
    begin
      Format:=0;
      repeat
        Format:=EnumClipboardFormats(Format);
      until (Format=DesignerClipboardFormat) or (Format in [0,CF_DIB,CF_BITMAP,CF_METAFILEPICT,CF_ENHMETAFILE,CF_TEXT]);
    end;
    if Format=DesignerClipboardFormat then PasteDesignerObject
    else if Format in [CF_DIB,CF_BITMAP] then PasteBitmap
    else if Format in [CF_METAFILEPICT,CF_ENHMETAFILE] then PasteMetafile
    else if Format in [CF_TEXT] then PasteText
    else raise Exception.Create(rsUnsupportedClipboardFormat);
  finally
    CloseClipboard;
  end;
  UpdateDrawing;
  StoreUndoPoint(PasteAction.Caption,True);
  ActiveControl:=nil; 
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  DroppedFile, Ext : string;
begin
  try
    DroppedFile:=GetDroppedFile(Msg,0);
  finally
    DragFinish(Msg.Drop);
  end;
  Ext:=ExtractFileExtNoDotUpper(DroppedFile);
  If Ext='DDD' then
  begin
    if CloseQuery then
    begin
      FileName:=DroppedFile;
      OpenActionExecute(nil);
    end;
  end
  else if Ext='DDT' then
  begin
    LoadTemplatePalette(DroppedFile);
  end
  else
  begin
    PicturePath:=DroppedFile;
    InsertPictureActionExecute(nil);
  end;
  Application.BringToFront;
  SetFocus;
end;

procedure TMainForm.InsertPictureActionExecute(Sender: TObject);
var
  Ext : string;
  Image : TLinearBitmap;
  Metafile : TMetafile;
begin
  if (Sender=nil) or OpenPictureDialog(PicturePath,rsKnownImageFormats+' ('+BitmapLoaders.GetLoadFilter) then
  try
    Screen.Cursor:=crHourGlass;
    Ext:=ExtractFileExtNoDotUpper(PicturePath);
    if (Ext='EMF') or (Ext='WMF') then // Metafile
    begin
      Metafile:=TMetafile.Create;
      try
        Metafile.LoadFromFile(PicturePath);
        ActiveObject:=TMetafileObject.CreateNew(Metafile);
        ActiveObject.Position:=Bounds(MousePos.X,MousePos.Y,  
                                      Round(Metafile.MMWidth*DesignerDPmm/100),
                                      Round(Metafile.MMHeight*DesignerDPmm/100));
      finally
        Metafile.Free;
      end;
    end
    else // Bitmap
    begin
      ICOLoader.Default.Mode:=mShowDialog;
      PNGLoader.Default.ExtraInfo:=True;
      Image:=TLinearBitmap.Create;
      try
        Image.LoadFromFile(PicturePath);
        if (PNGLoader.Default.AlphaChannel=nil) and (PNGLoader.Default.AlphaPalette<>nil) then
          PNGLoader.Default.MakeAlphaChannelFromAlphaPalette(Image)
        else if (PNGLoader.Default.AlphaChannel=nil) and (PNGLoader.Default.TransparentColor>=0) then
          PNGLoader.Default.AlphaChannel:=CreateAlphaChannelFromColorKey(Image,PNGLoader.Default.TransparentColor);
        ActiveObject:=TBitmapObject.CreateNew(Image,PNGLoader.Default.AlphaChannel);
        ActiveObject.Position:=Bounds(MousePos.X,MousePos.Y,
                                      Image.Width*DesignerDPI div Screen.PixelsPerInch,
                                      Image.Height*DesignerDPI div Screen.PixelsPerInch);
      finally
        Image.Free;
        PNGLoader.Default.ExtraInfo:=False;
        PNGLoader.Default.NewImage;
        ICOLOader.Default.Mode:=mLoadFirst;
      end;
    end;
    if Options.DefaultGroupLinks then ActiveObject.Links:=Options.GetDefaultLinks;
    ActiveObject.Name:=ActiveObject.Name+' '+RemoveFileExt(ExtractFileName(PicturePath));
    with GetActiveLayer do
    begin
      DeselectAll;
      Add(ActiveObject);
      ActiveObject.Selected:=True;
    end;
    StoreUndoPoint(Format(rsAddS,[ActiveObject.Name]),True);
  finally                                         
    Screen.Cursor:=crDefault;
  end;
  ActiveControl:=nil;
end;

procedure TMainForm.InheritLayerActionExecute(Sender: TObject);
var
  RelativePageIndex, LayerIndex : Integer;
  Caption : string;
begin
  RelativePageIndex:=-1;
  Caption:=StripHotKey(StripTrailing3Dots(InheritLayerAction.Caption));
  if (not IntegerQuery(Caption,rsRelativePageNumber1PreviousPage,
                       RelativePageIndex,-ActivePageIndex,Diagram.Count-ActivePageIndex-1,200)) then Exit;
  LayerIndex:=1;
  if (not IntegerQuery(Caption,rsLayerNumber,
                       LayerIndex,1,Diagram.Pages[ActivePageIndex+RelativePageIndex].Count,140)) then Exit;

  ActiveObject:=TInheritedLayerObject.CreateNew(RelativePageIndex,LayerIndex-1,Diagram.Pages[ActivePageIndex+RelativePageIndex]);
  with GetActiveLayer do
  begin
    DeselectAll;
    Insert(0,ActiveObject);
  end;
  StoreUndoPoint(Caption,True);
  ActiveControl:=nil;
end;

//==============================================================================================================================
// Other
//==============================================================================================================================
procedure TMainForm.AboutActionExecute(Sender: TObject);
begin
  InfoLabelText:=Format(rsIWouldLikeToThank,[LibraryCreators]);
  Application.Title:=ApplicationTitle;
  ShowAboutBox;
  SetFileName(FileName);
  ActiveControl:=nil;
end;

procedure TMainForm.HelpFileActionExecute(Sender: TObject);
begin
  if ExecuteFile(ProgramPath+'DiagramDesigner.chm')<=32 then RaiseLastOSError;
end;

procedure TMainForm.InternetHelpActionExecute(Sender: TObject);
begin
  ExecuteFile(InternetHelpAction.Hint);
end;

procedure TMainForm.SupportActionExecute(Sender: TObject);
begin
  Application.Title:=ApplicationTitle;
  ShowPleaseSupportForm(SupportAction.Hint,True);
  SetFileName(FileName);
  ActiveControl:=nil;
end;

procedure TMainForm.ExpressionEvaluatorExecute(Sender: TObject);
begin
  ShowExpressionEvaluator;
  ActiveControl:=nil;
end;

procedure TMainForm.ZoomBoxClick(Sender: TObject);
var
  Z : Double;
  P : Integer;
  Str : string;
begin
  if ZoomBox.ItemIndex=ZoomBox.Items.Count-1 then
  begin
    if Sender=ZoomBox then NotifyOnIdle(ZoomBoxClick,ZoomBox)
    else
    begin
      Zoom:=Floor(Min((CanvasFrame.Width-16)/(GetActivePage.Width*ScreenScale),
                      (CanvasFrame.Height-16)/(GetActivePage.Height*ScreenScale))*100)/100;
      CanvasInfo.Scale:=FloatPoint(ScreenScale*Zoom,ScreenScale*Zoom);
      UpdateScrollBars;
      ScrollBarX.Position:=-8;
      ScrollBarY.Position:=-8;
      ZoomBox.ItemIndex:=-1;
      ZoomBox.Text:=IntToStr(Round(Zoom*100))+'%';
    end;
  end
  else
  begin
    Str:=ZoomBox.Text;
    P:=Pos('%',Str);
    if P=0 then P:=MaxInt;
    Z:=EvaluateExpression(Copy(ZoomBox.Text,1,P-1))/100;
    if (Z<0.0001) or (Z>100) then raise Exception.Create(SInvalidNumber);
    if P=MaxInt then ZoomBox.Text:=RemLeadTailSpace(Str)+'%';
    ZoomBox.SelLength:=0;
    Zoom:=Z;
    CanvasInfo.Scale:=FloatPoint(ScreenScale*Zoom,ScreenScale*Zoom);
    UpdateScrollBars;
  end;
end;

procedure TMainForm.ZoomBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE : ActiveControl:=nil;
    VK_RETURN : ZoomBoxClick(Sender);
    VK_F10    : FormKeyDown(nil,Key,Shift);
  end;
end;

procedure TMainForm.EditBoxEnter(Sender: TObject);
begin
  GetActiveLayer.DeselectAll;
  ActiveObject:=nil;
end;

//==============================================================================================================================
// Drawing
//==============================================================================================================================
procedure TMainForm.UpdateDrawing;
begin
  FRepaint:=True;
  CanvasFrame.Invalidate;
end;

procedure TMainForm.UpdateTemplates;
begin
  RepaintTemplate:=True;
  TemplateFrame.Invalidate;
end;

procedure TMainForm.UpdateScrollBars;
var
  ScreenPoint : TPoint;
begin
  CanvasInfo.Offset:=Origo;
  ScreenPoint:=CanvasInfo.CanvasPoint(Point(-2*DesignerDPI,-2*DesignerDPI));
  ScrollBarX.Min:=ScreenPoint.X;
  ScrollBarY.Min:=ScreenPoint.Y;
  with GetActivePage do
    ScreenPoint:=CanvasInfo.CanvasPoint(Point(Width+2*DesignerDPI,Height+2*DesignerDPI));
  ScrollBarX.Max:=Max(0,ScreenPoint.X-CanvasFrame.Width);
  ScrollBarY.Max:=Max(0,ScreenPoint.Y-CanvasFrame.Height);
  UpdateDrawing;
end;

procedure TMainForm.DrawPanelResize(Sender: TObject);
begin
  TreeView.Left:=0;
  if Assigned(ZBuffer) then
  begin
    CanvasFrame.Width:=DrawPanel.ClientWidth-ScrollBarY.Width;
    CanvasFrame.Height:=DrawPanel.ClientHeight-ScrollBarX.Height-1;
    ZBuffer.Width:=CanvasFrame.Width;
    ZBuffer.Height:=CanvasFrame.Height;
    if Diagram.Count>0 then UpdateScrollBars;
  end;
  ScrollBarX.Top:=CanvasFrame.Height;
  ScrollBarX.Width:=DrawPanel.Width-PageTabSet.Width-LayerTabSet.Width;
  PageTabSet.Top:=ScrollBarX.Top;
  LayerTabSet.Top:=ScrollBarX.Top;
  LayerTabSet.Left:=PageTabSet.Width+ScrollBarX.Width;

  ScrollBarY.Left:=CanvasFrame.Width;
  ScrollBarY.Height:=CanvasFrame.Height;
end;

procedure TMainForm.ScrollBarChange(Sender: TObject);
begin
  UpdateDrawing;
end;

procedure TMainForm.UpdatePrinterMargins;
var
  DC : THandle;
  PrinterDPI, PrinterOffset, PrintSize : TPoint;
begin
  PrinterMargins:=Rect(0,0,0,0);
  try
    DC:=Printer.Handle;
    PrinterDPI:=Point(GetDeviceCaps(DC,LOGPIXELSX),GetDeviceCaps(DC,LOGPIXELSY));
    PrinterOffset:=Point(GetDeviceCaps(DC,PHYSICALOFFSETX),GetDeviceCaps(DC,PHYSICALOFFSETY));
    PrintSize:=Point(GetDeviceCaps(DC,HORZRES),GetDeviceCaps(DC,VERTRES));
    PrinterMargins:=RoundBounds(PrinterOffset.X/PrinterDPI.X*DesignerDPI/Options.PrintScaling,
                                PrinterOffset.Y/PrinterDPI.Y*DesignerDPI/Options.PrintScaling,
                                PrintSize.X/PrinterDPI.Y*DesignerDPI/Options.PrintScaling,
                                PrintSize.Y/PrinterDPI.X*DesignerDPI/Options.PrintScaling);

    UpdateDrawing;
  except
    // May fail if there is no printer
  end;
end;

procedure TMainForm.CanvasFramePaint(Sender: TObject);
var
  Pos : TPoint;
  ScreenRect : TRect;
  I : Integer;
  MovingObject : TBaseObject;
begin
  if FRepaint then
  with CanvasFrame do
  begin
    FRepaint:=False;
    with ZBuffer.Canvas do
    begin
      Brush.Color:=$ffffff;
      FillRect(Rect(0,0,Width,Height));
    end;
    CanvasInfo.Offset:=Point(-ScrollBarX.Position,
                             -ScrollBarY.Position);
    BitmapCanvas.Brush.Color:=DrawPanel.Color;
    BitmapCanvas.Brush.Style:=bsSolid;
    BitmapCanvas.FillRect(Rect(0,0,Width,Height));

    if PrintPreviewAction.Checked then // Preview mode
    begin
      // Draw paper
      CanvasInfo.DrawMode:=dmPreview;
      GetActivePage.DrawPaper(BitmapCanvas,CanvasInfo);
      // Draw shadows
      if Diagram.ObjectShadows then
      begin
        CanvasInfo.ZBuffer:=nil;
        CanvasInfo.DrawMode:=dmShadow;
        GetActivePage.DrawShadow(BitmapCanvas,CanvasInfo);
        CanvasInfo.DrawMode:=dmPreview;
      end;
      // Draw objects
      if ActiveLayerIndex=-1 then CanvasInfo.ZBuffer:=ZBuffer.Canvas
      else CanvasInfo.ZBuffer:=nil;
      Diagram.Stencil.Draw(BitmapCanvas,CanvasInfo);
      with GetActivePage do
        for I:=0 to Count-1 do
        begin
          if I=ActiveLayerIndex then CanvasInfo.ZBuffer:=ZBuffer.Canvas
          else CanvasInfo.ZBuffer:=nil;
          Layers[I].Draw(BitmapCanvas,CanvasInfo);
        end;
      CanvasInfo.ZBuffer:=ZBuffer.Canvas;
    end
    else // Edit mode
    begin
      // Draw paper
      CanvasInfo.DrawMode:=dmEditing;
      GetActivePage.DrawPaper(BitmapCanvas,CanvasInfo);
      if Options.ShowMargins and (PrinterMargins.Right>PrinterMargins.Left) then
        GetActivePage.DrawMargins(BitmapCanvas,CanvasInfo,PrinterMargins);
      // Draw grid
      if Options.ShowGrid and
         (Options.Grid.X*CanvasInfo.Scale.X>=6) and (Options.Grid.Y*CanvasInfo.Scale.Y>=6) then
      begin
        ScreenRect:=CanvasInfo.ObjectRect(Rect(0,0,CanvasFrame.Width,CanvasFrame.Height));
        ScreenRect.Left:=FloorInt(ScreenRect.Left,Options.Grid.X);
        Pos.Y:=FloorInt(ScreenRect.Top,Options.Grid.Y);
        while Pos.Y<ScreenRect.Bottom do
        begin
          Pos.X:=ScreenRect.Left;
          while Pos.X<ScreenRect.Right do
          begin
            with CanvasInfo.CanvasPoint(Pos) do BitmapCanvas.Pixels[X,Y]:=0;
            Inc(Pos.X,Options.Grid.X);
          end;
          Inc(Pos.Y,Options.Grid.Y);
        end;
      end;
      // Draw shadows
      if Diagram.ObjectShadows then
      begin
        CanvasInfo.ZBuffer:=nil;
        CanvasInfo.DrawMode:=dmShadow;
        GetActivePage.DrawShadow(BitmapCanvas,CanvasInfo);
        CanvasInfo.DrawMode:=dmEditing;
      end;
      // Draw objects
      if ActiveLayerIndex=-1 then // Stencil active
      begin
        CanvasInfo.ZBuffer:=ZBuffer.Canvas;
        Diagram.Stencil.Draw(BitmapCanvas,CanvasInfo);
      end
      else // Page layer active
      begin
        CanvasInfo.DrawMode:=dmPreview;
        CanvasInfo.ZBuffer:=nil;
        Diagram.Stencil.Draw(BitmapCanvas,CanvasInfo);
        with GetActivePage do
          for I:=0 to ActiveLayerIndex do
          begin
            if I=ActiveLayerIndex then
            begin
              CanvasInfo.ZBuffer:=ZBuffer.Canvas;
              if ShowLinkPoints then CanvasInfo.DrawMode:=dmEditDrag
              else CanvasInfo.DrawMode:=dmEditing;
            end;
            Layers[I].Draw(BitmapCanvas,CanvasInfo);
          end;
      end;
    end;
    // Draw select boxes
    MovingObject:=nil;
    if ObjectMoved then MovingObject:=ActiveObject;
    GetActiveLayer.DrawSelected(BitmapCanvas,CanvasInfo,MovingObject);
  end;
end;

procedure TMainForm.TemplateFramePaint(Sender: TObject);
//var TemplateBitmap : TFastBitmap;
begin
  if RepaintTemplate then
  begin
    RepaintTemplate:=False;
    TemplateFrame.Width:=Ceil(Templates.Width*TemplateCanvasInfo.Scale.X);
    TemplateFrame.Height:=Ceil(Templates.Height*TemplateCanvasInfo.Scale.X);
    {if Options.Antialiasing>1 then
    begin
      TemplateBitmap:=TFastBitmap.Create(TemplateFrame.Width,TemplateFrame.Height,pf32bit,TemplateScrollBox.Color);
      try
        DrawAntialiased(Templates,TemplateBitmap,TemplateCanvasInfo,Options.Antialiasing,TemplateScrollBox.Color);
        TemplateFrame.BitmapCanvas.Draw(0,0,TemplateBitmap);
      finally
        TemplateBitmap.Free;
      end;
    end
    else}
    begin
      with TemplateFrame, BitmapCanvas do
      begin
        Brush.Color:=TemplateScrollBox.Color;
        Brush.Style:=bsSolid;
        FillRect(Rect(0,0,Width,Height));
      end;
      Templates.Draw(TemplateFrame.BitmapCanvas,TemplateCanvasInfo);
    end;
  end;
end;

//==============================================================================================================================
// Mouse/keyboard events
//==============================================================================================================================
procedure TMainForm.TemplateScrollBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TemplateObject:=nil;
end;

procedure TMainForm.TemplatePopupMenuPopup(Sender: TObject);
begin
  TemplatePropertiesAction.Enabled:=Assigned(TemplateObject);
  DeleteTemplateAction.Enabled:=Assigned(TemplateObject);
  if Assigned(TemplateObject) then TemplatePropertiesAction.Caption:=TemplateObject.Name
  else TemplatePropertiesAction.Caption:=TemplatePropertiesAction.Hint;
end;

procedure TMainForm.TemplateFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  ObjectUnderCursor : TBaseObject;
begin
  if Shift=[] then
  begin
    ObjectUnderCursor:=Templates.GetObjectAt(X,Y,TemplateCanvasInfo,2);
    if ObjectUnderCursor<>nil then StatusBar.Panels[0].Text:=ObjectUnderCursor.Name
    else StatusBar.Panels[0].Text:=TemplateScrollBox.Hint;
  end;
end;

procedure TMainForm.TemplateFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TemplateObject:=Templates.GetObjectAt(X,Y,TemplateCanvasInfo,2);
  if Button=mbRight then
  begin
    with TemplateFrame.ClientToScreen(Point(X,Y)) do TemplatePopupMenu.Popup(X,Y);
  end
  else if Assigned(TemplateObject) then
  begin
    StatusBar.Panels[0].Text:=TemplateObject.Name;
    TemplateFrame.BeginDrag(False,4);
  end;
end;

procedure TMainForm.TemplateFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ActiveControl:=nil;
end;

procedure TMainForm.CanvasFrameDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=Source=TemplateFrame;
end;

procedure TMainForm.CanvasFrameDragDrop(Sender, Source: TObject; X,Y: Integer);
var
  DestPos : TPoint;
begin
  ActiveObject:=TemplateObject.CreateCopy;
  TemplateObject:=nil;
  with GetActiveLayer do
  begin
    DeselectAll;
    Add(ActiveObject);
    ActiveObject.Selected:=True;
  end;
  DestPos:=CanvasInfo.ObjectPoint(Point(X,Y));
  ActiveObject.Move(DestPos.X-ActiveObject.Left-ActiveObject.Width div 2,
                    DestPos.Y-ActiveObject.Top-ActiveObject.Height div 2,0,Options.Grid,[]);
  StoreUndoPoint(Format(rsAddS,[ActiveObject.Name]),True);
  ActiveControl:=nil;
end;

procedure TMainForm.UpdateStyleButtons;
var
  Color : TColor;
begin
  if Assigned(ActiveObject) and (opLineWidth in ActiveObject.ValidProperties) then
    UpdateLineWidthButton(LineWidthButton,ActiveObject.Properties[opLineWidth])
  else UpdateLineWidthButton(LineWidthButton,DefaultProperties.Properties[opLineWidth]);

  if Assigned(ActiveObject) and (opLineColor in ActiveObject.ValidProperties) then
    UpdateColorButton(LineColorButton,ActiveObject.Properties[opLineColor])
  else UpdateColorButton(LineColorButton,DefaultProperties.Properties[opLineColor]);

  if Assigned(ActiveObject) and (opFillColor in ActiveObject.ValidProperties) then
    UpdateColorButton(FillColorButton,ActiveObject.Properties[opFillColor])
  else UpdateColorButton(FillColorButton,DefaultProperties.Properties[opFillColor]);

  if Assigned(ActiveObject) and (opGradientColor in ActiveObject.ValidProperties) then
    Color:=ActiveObject.Properties[opGradientColor]
  else Color:=DefaultProperties.Properties[opGradientColor];
  if Color<>clNone then
  begin
    if Color and Integer($80000000)=0 then ImageList.GetBitmap(53,GradientColorButton.Glyph)
    else ImageList.GetBitmap(52,GradientColorButton.Glyph);
    GradientColorButton.Tag:=-1;
  end;
  UpdateColorButton(GradientColorButton,Color and clNone);

  if Assigned(ActiveObject) and (opTextColor in ActiveObject.ValidProperties) then
    UpdateColorButton(TextColorButton,ActiveObject.Properties[opTextColor])
  else UpdateColorButton(TextColorButton,DefaultProperties.Properties[opTextColor]);

  if Assigned(ActiveObject) and (opCornerRadius in ActiveObject.ValidProperties) then
    UpdateCornerRadiusButton(CornerRadiusButton,ActiveObject.Properties[opCornerRadius])
  else UpdateCornerRadiusButton(CornerRadiusButton,DefaultProperties.Properties[opCornerRadius]);

  if Assigned(ActiveObject) and (opLineStart in ActiveObject.ValidProperties) then
    UpdateLineStartEndButton(LineStartButton,ActiveObject.Properties[opLineStart],1)
  else UpdateLineStartEndButton(LineStartButton,DefaultProperties.Properties[opLineStart],1);

  if Assigned(ActiveObject) and (opLineEnd in ActiveObject.ValidProperties) then
    UpdateLineStartEndButton(LineEndButton,ActiveObject.Properties[opLineEnd],-1)
  else UpdateLineStartEndButton(LineEndButton,DefaultProperties.Properties[opLineEnd],-1);
end;

procedure TMainForm.SetActiveObject(ActiveObject: TBaseObject);
begin
  if Assigned(ActiveObject) then ActiveObject.Selected:=True;
  if FActiveObject<>ActiveObject then
  begin
    FActiveObject:=ActiveObject;
    UpdateControlStates;
    UpdateDrawing;
    if TreeView.Items.Count>0 then
    begin
      if Assigned(ActiveObject) then TreeView.Selected:=TTreeNode(ActiveObject.TreeNode)
      else TreeView.Selected:=nil;
    end;
    UpdateStyleButtons;
  end;
end;

procedure TMainForm.CanvasFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I : Integer;
  Hit : TBaseObject;
  NewConnector : TBaseConnectorObject;
begin
  if Button=mbMiddle then
  begin
    FPrevMouseMode:=MouseMode;
    MouseMode:=mmHand;
  end;
  MousePos:=CanvasInfo.ObjectPoint(Point(X,Y));
  LastMousePos:=MousePos;
  case MouseMode of
    mmEdit :
      if (Button=mbRight) and (ssLeft in Shift) and (ActiveObjectHandle>0) then // Add point
      begin
        I:=LastPointClickTime;
        LastPointClickTime:=TimeGetTime;
        if LastPointClickTime-DWord(I)<100 then Exit;
        if (ActiveObject is TCurveLineObject) then
        begin
          CanvasFrame.PopupMenu:=nil;

          if ((ActiveObjectHandle=1) and TCurveLineObject(ActiveObject).HasLinkObject(1)) or
             ((ActiveObjectHandle=TCurveLineObject(ActiveObject).PointCount) and TCurveLineObject(ActiveObject).HasLinkObject(2)) then
          begin
            if ActiveObjectHandle=1 then TCurveLineObject(ActiveObject).DisconnectLink(1)
            else TCurveLineObject(ActiveObject).DisconnectLink(2);
            Exit;
          end;

          ActiveObjectHandle:=TCurveLineObject(ActiveObject).AddPoint(ActiveObjectHandle);
          StoreUndoPoint(rsAddPoint,True);
        end
        else if (ActiveObject is TBaseConnectorObject) and
                (Int64(TBaseConnectorObject(ActiveObject).P1)<>Int64(TBaseConnectorObject(ActiveObject).P2)) then
        begin
          CanvasFrame.PopupMenu:=nil;

          if TBaseConnectorObject(ActiveObject).HasLinkObject(ActiveObjectHandle) then // Disconnect link if dragging a connection
          begin
            TBaseConnectorObject(ActiveObject).DisconnectLink(ActiveObjectHandle);
            Exit;
          end;

          if ObjectMoved then StoreUndoPoint(rsMove,False);
          GetActiveLayer.DeselectAll;
          if FRepaint then Update;
          NewConnector:=ActiveObject.CreateCopy as TBaseConnectorObject;

          GetActiveLayer.Add(NewConnector);
          with NewConnector.Position do
          begin
            if ActiveObjectHandle=1 then
            begin
              TopLeft:=TBaseConnectorObject(ActiveObject).P1;
              TBaseConnectorObject(ActiveObject).Properties[opLineStart]:=leNone;
              NewConnector.MakeLink(1,ActiveObject,0);
            end
            else
            begin
              TopLeft:=TBaseConnectorObject(ActiveObject).P2;
              TBaseConnectorObject(ActiveObject).Properties[opLineEnd]:=leNone;
              NewConnector.MakeLink(1,ActiveObject,1);
            end;
            BottomRight:=Point(RoundInt(MousePos.X,Options.Grid.X),RoundInt(MousePos.Y,Options.Grid.Y));
          end;
          ActiveObject:=NewConnector;
          ActiveObjectHandle:=2;
          UpdateObjectTree;
        end;
      end
      else
      begin
        if FRepaint then Update;
        I:=ZBuffer.Canvas.Pixels[X,Y];
        if I=$ffffff then
        begin // Begin rectangle select
          SelectStart:=Point(X,Y);
          SelectEnd:=SelectStart;
          with CanvasFrame.Canvas do
          begin
            Pen.Style:=psSolid;
            Pen.Color:=clBlack;
            Brush.Style:=bsSolid;
            Brush.Color:=clWhite;
          end;
        end
        else // Object hit
        begin
          ObjectMoved:=False;
          Hit:=GetActiveLayer.Objects[I and $ffff];
          if ssShift in Shift then // Shift pressed
          begin
            if Hit.Selected then
            begin
              Hit.Selected:=False;
              ActiveObject:=GetActiveLayer.LastSelected;
              UpdateDrawing;
            end
            else ActiveObject:=Hit;
            LastMousePos.X:=Low(Integer);
          end
          else if Button=mbRight then
          begin
            LastMousePos.X:=Low(Integer);
            if not Hit.Selected then GetActiveLayer.DeselectAll;
            ActiveObject:=Hit;
            ActiveObjectHandle:=0;
          end
          else
          begin
            ActiveObjectHandle:=I shr 16;
            if (ActiveObjectHandle<>0) or not Hit.Selected then GetActiveLayer.DeselectAll;
            ActiveObject:=Hit;
          end;
        end;
      end;
    mmDrawObject :
      begin
        ShowLinkPoints:=True;
        ObjectMoved:=False;
        GetActiveLayer.DeselectAll;
        if FRepaint then Update;
        ActiveObject:=DrawObject.CreateNew(DefaultProperties);
        GetActiveLayer.Add(ActiveObject);
        with ActiveObject.Position do
        begin
          TopLeft:=MousePos;
          BottomRight:=TopLeft;
        end;
        MouseMode:=mmEdit;
        ActiveObjectHandle:=1;
        FRepaint:=False;
        CanvasFrameMouseMove(nil,[ssLeft],X,Y);
        if (ActiveObject is TShapeObject) or (ActiveObject.ClassType=TTextObject) then ActiveObjectHandle:=4
        else ActiveObjectHandle:=2;
        UpdateObjectTree;
      end;
    mmHand :
      SelectStart:=Point(X,Y);
    mmZoom :
      begin
         if Button=mbLeft then Zoom:=Min(16,Zoom*2)
         else if Button=mbRight then Zoom:=Max(Zoom/2,1/16);
         CanvasInfo.Scale:=FloatPoint(ScreenScale*Zoom,ScreenScale*Zoom);
         ZoomBox.Text:=IntToStr(Round(Zoom*100))+'%';
         UpdateScrollBars;
         with CanvasInfo.CanvasPoint(MousePos) do
         begin
           ScrollBarX.Position:=X-CanvasFrame.Width div 2;
           ScrollBarY.Position:=Y-CanvasFrame.Height div 2;
         end;
      end;
  end;
end;

procedure TMainForm.CanvasFrameDblClick(Sender: TObject);
var
  Text, ClassName : string;
  P, Code : Integer;
  WinHandle : THandle;
begin
  if FUpdateControlStates then SetControlStates;
  if (GetActiveLayer.SelectCount>1) and (ActiveObject<>nil) then
  begin
    GetActiveLayer.DeselectAll;
    ActiveObject.Selected:=True;
    UpdateDrawing;
  end;
  if ActiveObject is TTextObject then
  begin
    Text:=PString(TTextObject(ActiveObject).Properties[opText])^;
    P:=Pos('\A',Text);
    while P<>0 do
    begin
      if (P=1) or (Text[P-1]<>'\') then // Execute link
      begin
        Text:=Copy(Text,P+2,MaxInt);

        Val(Text,P,Code); // Link to page number
        if Code=0 then
        begin
          ActivePageIndex:=EnsureRange(P-1,0,Diagram.Count-1);
          Exit;
        end;

        if ExtractFileExtNoDotUpper(Text)='DDD' then // Link to other document, see if it is open
        begin
          ClassName:=Application.ClassName;
          WinHandle:=FindWindow(PChar(ClassName),PChar(RemoveFileExt(ExtractFileName(Text))+' - '+ApplicationTitle)); // Find old instance
          if WinHandle<>0 then
          begin
            if GetWindowLong(WinHandle,GWL_STYLE) and WS_VISIBLE<>0 then // Only bring to front if visible
            begin
              if IsIconic(WinHandle) then ShowWindow(WinHandle,SW_RESTORE)
              else SetForegroundWindow(WinHandle);
            end;
            Exit;
          end;
        end;
        if ExecuteFile(Text,'',ExtractFilePath(FileName))<=32 then RaiseLastOSError;
        Exit;
      end;
      P:=PosEx('\A',Text,P+1)
    end;

    EditTextAction.Execute; // Edit text
  end
  else PropertiesAction.Execute;
end;

procedure TMainForm.CanvasFrameMouseMove(Sender: TObject; Shift: TShiftState; MX, MY: Integer);
var
  I : Integer;
  Offset : TPoint;

  procedure DrawFocusRect(X1,Y1,X2,Y2: Integer);
  begin
    if X1>X2 then SwapDWords(X1,X2);
    if Y1>Y2 then SwapDWords(Y1,Y2);
    CanvasFrame.Canvas.DrawFocusRect(Rect(X1,Y1,X2+1,Y2+1));
  end;

  function FindLink: Boolean;
  var
    SY, SX, X1, X2, I : Integer;
    Value : PInteger;
    LastValue, Dist, BestDist, BestIndex, LinkSearchBox : Integer;
    Obj, BestObject : TBaseObject;
    LinkPos : TPoint;
  begin
    Result:=False;
    if (ssShift in Shift) or not Options.AutoConnectToLinks then Exit; // Ignore links
    if ssAlt in Shift then LinkSearchBox:=0
    else LinkSearchBox:=4;
    X1:=Max(0,MX-LinkSearchBox);
    X2:=Min(CanvasFrame.Width-1,MX+LinkSearchBox);
    LastValue:=-1;
    BestIndex:=0;
    BestObject:=nil;
    BestDist:=2*Sqr(LinkSearchBox)+1;
    with GetActiveLayer do
    for SY:=Max(0,MY-LinkSearchBox) to Min(CanvasFrame.Height-1,MY+LinkSearchBox) do
    begin
      Value:=@PIntegerArray(ZBuffer.ScanLine[SY])^[X1];
      for SX:=X1 to X2 do
      begin
        if Value^<>LastValue then
        begin
          LastValue:=Value^;
          if LastValue<>$ffffff then
          begin
            Obj:=Objects[Swap(LastValue shr 8)];
            if (Obj<>ActiveObject) and
               ((Sender=nil) or not (Obj is TBaseConnectorObject)) then // Don't attach to other connectors while dragging
              for I:=0 to High(Obj.Links) do
              begin
                LinkPos:=Obj.GetLinkPosition(I);
                with CanvasInfo.CanvasPoint(LinkPos) do
                begin
                  Dist:=Sqr(X-MX)+Sqr(Y-MY);
                  if Dist<BestDist then
                  begin
                    BestDist:=Dist;
                    BestObject:=Obj;
                    BestIndex:=I;
                    Offset:=Point(LinkPos.X-LastMousePos.X,LinkPos.Y-LastMousePos.Y);
                    Result:=True;
                  end;
                end;
              end;
          end;
        end;
        Inc(Value);
      end;
    end;
    if Result then
    begin
      if BestObject is TBaseConnectorObject then
      begin
        // If connecting to another connector then check if it already has a linked object and redirect the reference
        TBaseConnectorObject(BestObject).GetLink(BestIndex+1,Obj,I);
        if (Obj<>nil) and not (Obj is TBaseConnectorObject) then
        begin
          BestObject:=Obj;
          BestIndex:=I;
        end;
      end;
      Result:=TBaseConnectorObject(ActiveObject).MakeLink(ActiveObjectHandle,BestObject,BestIndex);
    end;
  end;

var
  CanvasMousePos : TPoint;
  MemStream : TMemStream;
  Units : string;
begin
  {$IFOPT D-}
  try
  {$ENDIF}
    if FRepaint then Update;
    CanvasMousePos:=Point(MX,MY);
    MousePos:=CanvasInfo.ObjectPoint(CanvasMousePos);
    case MouseMode of
      mmEdit :
        if ssLeft in Shift then
        begin
          if SelectStart.X<>Low(Integer) then // Rectangle select
          begin
            DrawFocusRect(SelectStart.X,SelectStart.Y,SelectEnd.X,SelectEnd.Y);
            SelectEnd:=CanvasMousePos;
            DrawFocusRect(SelectStart.X,SelectStart.Y,SelectEnd.X,SelectEnd.Y);
          end
          else if (LastMousePos.X<>Low(Integer))  then // Move/resize
          begin
            if not ((ActiveObjectHandle>0) and (ActiveObject is TBaseConnectorObject) and FindLink) then
            begin
              if not ObjectMoved and (ActiveObjectHandle=0) and (ssCtrl in Shift) then // Quick copy with Ctrl key
              begin
                MemStream:=TMemStream.Create;
                try
                  with GetActiveLayer do
                  begin
                    SaveSelected(MemStream);
                    MemStream.Position:=0;
                    DeselectAll;
                    GetActiveLayer.LoadSelected(MemStream);
                    ActiveObject:=Objects[Count-1];
                  end;
                finally
                  MemStream.Free;
                end;
                ObjectMoved:=True;
                UpdateObjectTree
              end;

              Offset.X:=MousePos.X-LastMousePos.X;
              Offset.Y:=MousePos.Y-LastMousePos.Y;
              if ssAlt in Shift then // Alt pressed, don't use grid
                Offset:=GetActiveLayer.MoveSelected(Offset.X,Offset.Y,
                                                    ActiveObjectHandle,NoGrid,Shift)
              else if (Offset.X<>0) or (Offset.Y<>0) then // Snap to grid
                Offset:=GetActiveLayer.MoveSelected(Offset.X,Offset.Y,
                                                    ActiveObjectHandle,Options.Grid,Shift);
            end;
            if (Offset.X<>0) or (Offset.Y<>0) then
            begin
              LastMousePos:=OffsetPoint(LastMousePos,Offset);
              ShowLinkPoints:=True;
              UpdateDrawing;
              ObjectMoved:=True;
            end;
          end;
        end
        else
        begin
          I:=ZBuffer.Canvas.Pixels[MX,MY];
          // Set cursor
          //  1 5 2
          //  7 0 8
          //  3 6 4
          if (ssShift in Shift) or (I=$ffffff) then // Select
            CanvasFrame.Cursor:=crDefault
          else if (GetActiveLayer.Objects[I and $ffff] is TCurveLineObject) and (I>$ffff) then
            CanvasFrame.Cursor:=crMove
          else
            case I shr 16 of
              0   : CanvasFrame.Cursor:=crSize;
              1,4 : CanvasFrame.Cursor:=crSizeNWSE;
              2,3 : CanvasFrame.Cursor:=crSizeNESW;
              5,6 : CanvasFrame.Cursor:=crSizeNS;
              7,8 : CanvasFrame.Cursor:=crSizeWE;
              9   : CanvasFrame.Cursor:=crRotate;
              else  CanvasFrame.Cursor:=crDefault;
            end;
          // Set status bar text
          if I<>$ffffff then
          begin
            StatusBar.Panels[sbpHint].Text:=GetActiveLayer.Objects[I and $ffff].Name;
            CanvasFrame.Hint:=GetActiveLayer.Objects[I and $ffff].Hint;
          end
          else
          begin
            StatusBar.Panels[sbpHint].Text:='';
            CanvasFrame.Hint:='';
          end;
        end;

      mmHand : if ([ssLeft,ssMiddle]*Shift<>[]) and (SelectStart.X<>Low(Integer)) then
        begin
          ScrollBarX.Position:=ScrollBarX.Position-MX+SelectStart.X;
          ScrollBarY.Position:=ScrollBarY.Position-MY+SelectStart.Y;
          SelectStart:=CanvasMousePos;
        end;
    end;
    with Options do
    begin
      Units:=' '+TranslationManager.TranslateString(DisplayUnitName[DisplayUnits]);
      StatusBar.Panels[sbpMousePos].Text:=
        'x='+FormatFloat(DisplayUnitFormat[DisplayUnits],MousePos.X/DisplayUnitSize[DisplayUnits])+Units+
        '  y='+FormatFloat(DisplayUnitFormat[DisplayUnits],MousePos.Y/DisplayUnitSize[DisplayUnits])+Units;
      if ObjectMoved and (ActiveObjectHandle>0) then
      begin
        if ActiveObject is TStraightLineObject then
          StatusBar.Panels[sbpHint].Text:=ActiveObject.Name+' (L='+FormatFloat(DisplayUnitFormat[DisplayUnits],TStraightLineObject(ActiveObject).Length/DisplayUnitSize[DisplayUnits])+Units+
                                          ListSeparator+' A='+FormatFloat('0.0',TStraightLineObject(ActiveObject).Angle/Pi*180)+'°)  '+
                                          rsEditConnectorHint
        else if ActiveObject is TConnectorObject then
          StatusBar.Panels[sbpHint].Text:=ActiveObject.Name+'. '+rsEditConnectorHint

        else if ActiveObject is TCurveLineObject then
          StatusBar.Panels[sbpHint].Text:=ActiveObject.Name+'. '+rsLeftRightClickToAddPoint
        else if ActiveObject is TShapeObject then
          StatusBar.Panels[sbpHint].Text:=ActiveObject.Name+' (W='+FormatFloat(DisplayUnitFormat[DisplayUnits],ActiveObject.Width/DisplayUnitSize[DisplayUnits])+Units+
                                          ListSeparator+' H='+FormatFloat(DisplayUnitFormat[DisplayUnits],ActiveObject.Height/DisplayUnitSize[DisplayUnits])+Units+')'+'  '+
                                          rsEditShapeHint;
      end;
    end;
  {$IFOPT D-}
  except
    on E: EAccessViolation do ;
  end;
  {$ENDIF}
end;

procedure TMainForm.CanvasFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbMiddle then
  begin
    SelectStart.X:=Low(Integer);
    MouseMode:=FPrevMouseMode;
    Exit;
  end;
  if (Button=mbRight) and (ssLeft in Shift) then Exit;
  if (Button=mbLeft) and (MouseMode=mmEdit) then
  begin
    if SelectStart.X<>Low(Integer) then
    begin
      if FRepaint then Update;
      if not (ssShift in Shift) then GetActiveLayer.DeselectAll;
      ActiveObject:=GetActiveLayer.Select(NormalizeRect(Rect(SelectStart.X,SelectStart.Y,SelectEnd.X,SelectEnd.Y)),ZBuffer,CanvasInfo);
      UpdateDrawing;
    end
    else if ObjectMoved then
    begin
      // Final move event to allow connectors to link to connectors
      if ActiveObject is TBaseConnectorObject then
        CanvasFrameMouseMove(nil,[ssLeft]+Shift,X,Y);
      GetActiveLayer.NotifyMovementDone;
      StoreUndoPoint(rsMove,False);
      if (ActiveObject is TTextObject) and TTextObject(ActiveObject).EditTextAfterPlace then
        EditTextActionExecute(nil);
    end;
    if ShowLinkPoints then
    begin
      ShowLinkPoints:=False;
      UpdateDrawing;
    end;
    CanvasFrame.PopupMenu:=PopupMenu;
  end;
  LastMousePos.X:=Low(Integer);
  SelectStart.X:=Low(Integer);
  ActiveControl:=nil;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  function ToolbarControlHit(Control: TControl): Boolean;
  begin
    Result:=Control.Visible and Control.Enabled and (MousePos.X>=Control.Left) and (MousePos.X<Control.Left+Control.Width);
  end;

var
  Key : Word;
  ToolbarPos : TPoint;
begin
  if not (ActiveControl=TreeView) then
  begin
    Handled:=True;
    ToolbarPos:=ToolBar1.ClientToScreen(Point(0,ToolBar1.Height));
    if MousePos.Y<ToolbarPos.Y then
    begin
      if MousePos.Y>=ToolbarPos.Y-Toolbar1.Height then // Toolbar hit
      begin
        MousePos.X:=MousePos.X-ToolbarPos.X;
        if ToolbarControlHit(ZoomBox) or ToolbarControlHit(ToolButtonZoom) then
        begin
          if ZoomBox.ItemIndex<0 then ZoomBox.ItemIndex:=ZoomBox.Items.IndexOf(ZoomBox.Text);
          ZoomBox.SetFocus;
          Handled:=False;
        end
        else if ToolbarControlHit(LineWidthButton) then
          LineWidthActionExecute(Pointer(WheelDelta))
        else if ToolbarControlHit(CornerRadiusButton) then
          CornerRadiusActionExecute(Pointer(WheelDelta))
        else if ToolbarControlHit(LineEndButton) then
          LineEndActionExecute(Pointer(WheelDelta))
        else if ToolbarControlHit(LineStartButton) then
          LineStartActionExecute(Pointer(WheelDelta))
        else if ToolbarControlHit(LineColorButton) then
          LineColorActionExecute(nil)
        else if ToolbarControlHit(FillColorButton) then
          FillColorActionExecute(nil)
        else if ToolbarControlHit(GradientColorButton) then
          GradientColorActionExecute(Pointer(WheelDelta));
      end;
      GridEdit.FormMouseWheelSetFocusControl(Sender,Shift,WheelDelta,MousePos,Handled);
    end
    else if ssCtrl in Shift then // Zoom in/out
    begin
      if WheelDelta>0 then Key:=VK_ADD
      else Key:=VK_SUBTRACT;
      FormKeyDown(Self,Key,[]);
    end
    else // Scroll page
    begin
      MousePos:=ScreenToClient(MousePos);
      if MousePos.X>RightPanel.Left then with TemplateScrollBox.VertScrollBar do Position:=Position-WheelDelta div 2
      else ScrollBarY.Position:=ScrollBarY.Position-WheelDelta div 2;
    end;
  end;
end;

procedure TMainForm.StatusBarResize(Sender: TObject);
begin
  StatusBar.Panels[sbpMousePos].Width:=185;
  StatusBar.Panels[0].Width:=StatusBar.ClientWidth-StatusBar.Panels[1].Width-StatusBar.Panels[2].Width-StatusBar.Panels[3].Width;
end;

procedure TMainForm.ZoomActionExecute(Sender: TObject);
begin
  MouseMode:=mmZoom;
end;

procedure TMainForm.MoveCanvasActionExecute(Sender: TObject);
begin
  MouseMode:=mmHand;
end;

procedure TMainForm.MouseEditActionExecute(Sender: TObject);
begin
  MouseMode:=mmEdit;
end;

procedure TMainForm.DrawObjectActionExecute(Sender: TObject);
begin
  if Sender=DrawLineAction then DrawObject:=TStraightLineObject
  else if Sender=DrawArrowAction then DrawObject:=TArrowObject
  else if Sender=DrawConnectorAction then DrawObject:=TConnectorObject
  else if Sender=DrawCurveAction then DrawObject:=TCurveLineObject
  else if Sender=DrawRectangleAction then DrawObject:=TRectangleObject
  else if Sender=DrawTextAction then DrawObject:=TTextObject
  else if Sender=DrawEllipseAction then DrawObject:=TEllipseObject
  else Assert(False);
  if Assigned(DrawAction) and (DrawAction<>Sender) then
  begin
    DrawAction.ShortCut:=0;
    DrawAction.Checked:=False;
  end;
  DrawAction:=(Sender as TAction);
  DrawAction.ShortCut:=ShortCut(VK_F5,[]);
  MouseMode:=mmDrawObject;
  DrawAction.Checked:=False;
  DrawAction.Checked:=True;
  if DrawObject.InheritsFrom(TBaseConnectorObject) then
  begin
    ShowLinkPoints:=True;
    UpdateDrawing;
  end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure KeyboardMove(DX,DY: Integer);
  var
    Grid : TPoint;
  begin
    Grid:=Options.Grid;
    if (ssAlt in Shift) or (Grid.X<=1) then Grid.X:=Round(1/CanvasInfo.Scale.X);
    if (ssAlt in Shift) or (Grid.Y<=1) then Grid.Y:=Round(1/CanvasInfo.Scale.Y);
    GetActiveLayer.MoveSelected(Grid.X*DX,Grid.Y*DY,0,Point(1,1),[]);
    UpdateDrawing;
  end;

var
  OldMouseMode : TMouseMode;
begin
  case Key of
    VK_ESCAPE   :   if MouseMode=mmEdit then
                    begin
                      GetActiveLayer.DeselectAll;
                      ActiveObject:=nil;
                    end
                    else MouseMode:=mmEdit;
    VK_SPACE    :   if MouseMode<>mmHand then
                    begin
                      FPrevMouseMode:=MouseMode;
                      MouseMode:=mmHand;
                    end;
    VK_ADD,
    VK_OEM_PLUS :   if [ssLeft,ssRight]*Shift=[] then
                    begin
                      OldMouseMode:=MouseMode;
                      MouseMode:=mmZoom;
                      with CanvasInfo.CanvasPoint(MousePos) do CanvasFrameMouseDown(nil,mbLeft,[],X,Y);
                      MouseMode:=OldMouseMode;
                    end;
    VK_SUBTRACT,
    VK_OEM_MINUS :  if [ssLeft,ssRight]*Shift=[] then
                    begin
                      OldMouseMode:=MouseMode;
                      MouseMode:=mmZoom;
                      with CanvasInfo.CanvasPoint(MousePos) do CanvasFrameMouseDown(nil,mbRight,[],X,Y);
                      MouseMode:=OldMouseMode;
                    end;
    Word('1')..
    Word('9')     : if (ssCtrl in Shift) and (Key-Word('1')<Diagram.Count) then
                    SetActivePageIndex(Key-Word('1'));
    Word('0')     : if (ssCtrl in Shift) and (Diagram.Count>=10) then
                    SetActivePageIndex(9);
    VK_NEXT       : if ActivePageIndex<Diagram.Count-1 then ActivePageIndex:=ActivePageIndex+1;
    VK_PRIOR      : if ActivePageIndex>0 then ActivePageIndex:=ActivePageIndex-1;
    VK_HOME       : if ssCtrl in Shift then ActivePageIndex:=0;
    VK_END        : if ssCtrl in Shift then ActivePageIndex:=Diagram.Count-1;
    VK_DOWN       : if ssCtrl in Shift then KeyboardMove(0,1)
                    else with ScrollBarY do Position:=Position+SmallChange;
    VK_UP         : if ssCtrl in Shift then KeyboardMove(0,-1)
                    else with ScrollBarY do Position:=Position-SmallChange;
    VK_RIGHT      : if ssCtrl in Shift then KeyboardMove(1,0)
                    else with ScrollBarX do Position:=Position+SmallChange;
    VK_LEFT       : if ssCtrl in Shift then KeyboardMove(-1,0)
                    else with ScrollBarX do Position:=Position-SmallChange;
    VK_APPS,
    VK_F10        : begin
                      TQuickActionSearchForm.Execute(ActionManager);
                      Key:=0;
                    end;
  end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE    : MouseMode:=FPrevMouseMode;
  end;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  TQuickActionSearchForm.CheckKey(ActionManager,Key,[]);
end;

procedure TMainForm.TreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE   : begin
                    ActiveControl:=nil;
                    FormKeyDown(nil,Key,Shift);
                  end;
    VK_F10,
    Word('0')..
    Word('9')   : FormKeyDown(nil,Key,Shift);
  end;
end;

procedure TMainForm.TreeViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node : TTreeNode;
begin
  if Button=mbRight then
  begin
    Node:=TreeView.GetNodeAt(X,Y);
    if Assigned(Node) then
    begin
      TreeView.Selected:=Node;
      SetControlStates;
    end;
  end;
end;

procedure TMainForm.TreeViewDblClick(Sender: TObject);
begin
  PropertiesAction.Execute;
end;

procedure TMainForm.LineWidthActionExecute(Sender: TObject);
var
  LineWidth : EvalFloat;
  PropertyObject : TBaseObject;
  ChangeDefault : Boolean;
begin
  ChangeDefault:=ActiveObject=nil;
  if Assigned(ActiveObject) and (opLineWidth in ActiveObject.ValidProperties) then PropertyObject:=ActiveObject
  else PropertyObject:=DefaultProperties;
  LineWidth:=PropertyObject.Properties[opLineWidth]*(4/DesignerDPpoint);

  if (Sender<>nil) and (Abs(Integer(Sender))<1000) then
  begin
    LineWidth:=EnsureRange(LineWidth+Sign(Integer(Sender)),0,100);
    StatusBar.Panels[0].Text:=rsLineWidth+': '+FloatToStrF(LineWidth,ffFixed,5,1)+' '+rs1_4Points;
  end
  else if not FloatQuery(rsLineWidth,rsLineWidth+': ('+rs1_4Points+')',LineWidth,0,100,120,1) then Exit;

  ActiveControl:=nil;
  if ChangeDefault then
  begin
    ActiveObject:=nil;
    GetActiveLayer.DeselectAll;
    DefaultProperties.Properties[opLineWidth]:=Round(LineWidth/(4/DesignerDPpoint));
  end
  else
  begin
    GetActiveLayer.SetMemberProperty(opLineWidth,Round(LineWidth/(4/DesignerDPpoint)),False);
    StoreUndoPoint(rsEditProperties,False);
  end;
  UpdateStyleButtons;
end;

procedure TMainForm.CornerRadiusActionExecute(Sender: TObject);
var
  CornerRadius : EvalFloat;
  PropertyObject : TBaseObject;
  ChangeDefault : Boolean;
begin
  ChangeDefault:=ActiveObject=nil;
  if Assigned(ActiveObject) and (opCornerRadius in ActiveObject.ValidProperties) then PropertyObject:=ActiveObject
  else PropertyObject:=DefaultProperties;
  CornerRadius:=PropertyObject.Properties[opCornerRadius]/DisplayUnitSize[Options.DisplayUnits];
  if (Sender<>nil) and (Abs(Integer(Sender))<1000) then
  begin
    CornerRadius:=EnsureRange(CornerRadius+Sign(Integer(Sender)),0,100);
    StatusBar.Panels[0].Text:=rsCornerRadius+': '+FloatToStrF(CornerRadius,ffFixed,5,2)+' '+
      TranslationManager.TranslateString(DisplayUnitName[Options.DisplayUnits]);
  end
  else if not FloatQuery(rsCornerRadius,rsCornerRadius+': ('+TranslationManager.TranslateString(DisplayUnitName[Options.DisplayUnits])+')',CornerRadius,0,1e100,120,1) then Exit;
  ActiveControl:=nil;
  if ChangeDefault then
  begin
    ActiveObject:=nil;
    GetActiveLayer.DeselectAll;
    DefaultProperties.Properties[opCornerRadius]:=Round(CornerRadius*DisplayUnitSize[Options.DisplayUnits]);
  end
  else
  begin
    GetActiveLayer.SetMemberProperty(opCornerRadius,Round(CornerRadius*DisplayUnitSize[Options.DisplayUnits]),False);
    StoreUndoPoint(rsEditProperties,False);
  end;
  UpdateStyleButtons;
end;

procedure TMainForm.LineColorActionExecute(Sender: TObject);
var
  Color : TColor;
  PropertyObject : TBaseObject;
  ChangeDefault : Boolean;
begin
  ChangeDefault:=ActiveObject=nil;
  if Assigned(ActiveObject) and (opLineColor in ActiveObject.ValidProperties) then PropertyObject:=ActiveObject
  else PropertyObject:=DefaultProperties;
  Color:=PropertyObject.Properties[opLineColor];
  if (Sender=nil) or ShowColorDialog(Color) then
  begin
    if Sender=nil then Color:=clNone; 
    ActiveControl:=nil;
    if ChangeDefault then
    begin
      ActiveObject:=nil;
      GetActiveLayer.DeselectAll;
      DefaultProperties.Properties[opLineColor]:=Color;
    end
    else
    begin
      GetActiveLayer.SetMemberProperty(opLineColor,Color,False);
      StoreUndoPoint(rsEditProperties,False);
    end;
    UpdateStyleButtons;
  end;
end;

procedure TMainForm.FillColorActionExecute(Sender: TObject);
var
  Color : TColor;
  PropertyObject : TBaseObject;
  ChangeDefault : Boolean;
begin
  ChangeDefault:=ActiveObject=nil;
  if Assigned(ActiveObject) and (opFillColor in ActiveObject.ValidProperties) then PropertyObject:=ActiveObject
  else PropertyObject:=DefaultProperties;
  Color:=PropertyObject.Properties[opFillColor];
  if (Sender=nil) or ShowColorDialog(Color) then
  begin
    if Sender=nil then Color:=clNone;
    ActiveControl:=nil;
    if ChangeDefault then
    begin
      ActiveObject:=nil;
      GetActiveLayer.DeselectAll;
      DefaultProperties.Properties[opFillColor]:=Color;
    end
    else if Assigned(ActiveObject) then
    begin
      GetActiveLayer.SetMemberProperty(opFillColor,Color,False);
      StoreUndoPoint(rsEditProperties,False);
    end;
    UpdateStyleButtons;
  end;
end;

procedure TMainForm.GradientColorActionExecute(Sender: TObject);
var
  GradientColor, FillColor, Direction : TColor;
  PropertyObject : TBaseObject;
  ChangeDefault : Boolean;
begin
  ChangeDefault:=ActiveObject=nil;
  if Assigned(ActiveObject) and (opGradientColor in ActiveObject.ValidProperties) then PropertyObject:=ActiveObject
  else PropertyObject:=DefaultProperties;
  FillColor:=PropertyObject.Properties[opFillColor];
  GradientColor:=PropertyObject.Properties[opGradientColor] and clNone;
  Direction:=PropertyObject.Properties[opGradientColor] and $80000000;
  if (Integer(Sender)<1000) or ShowColorDialog(GradientColor) then
  begin
    if Sender=nil then GradientColor:=clNone
    else
    begin
      if (Integer(Sender)<1000) and (GradientColor<>clNone) then
      begin
        if (Direction=0) xor (Integer(Sender)<0) then SwapDWords(GradientColor,FillColor);
        Direction:=Direction xor Integer($80000000);
      end;
      GradientColor:=GradientColor or Direction;
    end;
    ActiveControl:=nil;
    if ChangeDefault then
    begin
      ActiveObject:=nil;
      GetActiveLayer.DeselectAll;
      DefaultProperties.Properties[opFillColor]:=FillColor;
      DefaultProperties.Properties[opGradientColor]:=GradientColor;
    end
    else if Assigned(ActiveObject) then
    begin
      GetActiveLayer.SetMemberProperty(opFillColor,FillColor,False);
      GetActiveLayer.SetMemberProperty(opGradientColor,GradientColor,False);
      StoreUndoPoint(rsEditProperties,False);
    end;
    UpdateStyleButtons;
  end;
end;

procedure TMainForm.TextColorActionExecute(Sender: TObject);
var
  Color : TColor;
  PropertyObject : TBaseObject;
  ChangeDefault : Boolean;
begin
  ChangeDefault:=ActiveObject=nil;
  if Assigned(ActiveObject) and (opTextColor in ActiveObject.ValidProperties) then PropertyObject:=ActiveObject
  else PropertyObject:=DefaultProperties;
  Color:=PropertyObject.Properties[opTextColor];
  if ShowColorDialog(Color) then
  begin
    ActiveControl:=nil;
    if ChangeDefault then
    begin
      ActiveObject:=nil;
      GetActiveLayer.DeselectAll;
      DefaultProperties.Properties[opTextColor]:=Color;
    end
    else
    begin
      GetActiveLayer.SetMemberProperty(opTextColor,Color,False);
      StoreUndoPoint(rsEditProperties,False);
    end;
    UpdateStyleButtons;
  end;
end;

procedure TMainForm.HandleLineStartEndAction(PropType: TObjectProperty; Delta: Integer);
var
  ChangeDefault : Boolean;
  PropertyObject : TBaseObject;
  Marker, I : Integer;
  Found : Boolean;
begin
  ChangeDefault:=ActiveObject=nil;
  if Assigned(ActiveObject) and (PropType in ActiveObject.ValidProperties) then PropertyObject:=ActiveObject
  else PropertyObject:=DefaultProperties;
  Marker:=PropertyObject.Properties[PropType];

  if (Delta<>0) and (Abs(Delta)<1000) then // Wheel
  begin
    Marker:=(Marker and $00ff) + ForceInRange(Hi(Marker)+Sign(Delta),1,100) shl 8;
  end
  else if (Delta>=2000) and (Delta<2256) then // Set
  begin
    Marker:=(Delta-2000) or (Marker and $ff00);
    if Hi(Marker)=0 then Marker:=Marker or $200; // Set default size 2
  end
  else // Toggle
  begin
    Found:=False;
    for I:=Low(LineEnds) to High(LineEnds)-1 do
      if Lo(Marker)=LineEnds[I] then
      begin
        Marker:=LineEnds[I+1] or (Marker and $ff00);
        if Hi(Marker)=0 then Marker:=Marker or $200; // Set default size 2
        Found:=True;
        Break;
      end;
    if not Found then Marker:=Marker and $ff00; // Reset to None
  end;

  // Update status bar
  for I:=Low(LineEnds) to High(LineEnds) do
    if Lo(Marker)=LineEnds[I] then
    begin
      StatusBar.Panels[0].Text:=LoadResString(LineEndNames[I])+', '+rsSize+'='+IntToStr(Hi(Marker));
      Break;
    end;

  ActiveControl:=nil;
  if ChangeDefault then
  begin
    ActiveObject:=nil;
    GetActiveLayer.DeselectAll;
    DefaultProperties.Properties[PropType]:=Marker;
  end
  else
  begin
    GetActiveLayer.SetMemberProperty(PropType,Marker,False);
    StoreUndoPoint(rsEditProperties,False);
  end;
  UpdateStyleButtons;
end;

procedure TMainForm.LineStartActionExecute(Sender: TObject);
begin
  if Abs(Integer(Sender))<=1000 then HandleLineStartEndAction(opLineStart,Integer(Sender))
  else
  begin
    LineStartEndMenu.Tag:=Integer(opLineStart);
    with LineStartButton.ClientToScreen(Point(0,20)) do LineStartEndMenu.Popup(X,Y);
  end;
end;

procedure TMainForm.LineEndActionExecute(Sender: TObject);
begin
  if Abs(Integer(Sender))<=1000 then HandleLineStartEndAction(opLineEnd,Integer(Sender))
  else
  begin
    LineStartEndMenu.Tag:=Integer(opLineEnd);
    with LineEndButton.ClientToScreen(Point(0,20)) do LineStartEndMenu.Popup(X,Y);
  end;
end;

procedure TMainForm.LineStartEndItemClick(Sender: TObject);
begin
  HandleLineStartEndAction(TObjectProperty(LineStartEndMenu.Tag),(Sender as TMenuItem).Tag+2000);
end;

//==============================================================================================================================
// Object
//==============================================================================================================================
procedure TMainForm.EditTextActionExecute(Sender: TObject);
var
  Str : string;
  Obj : TBaseObject;
  PreviewObj : TTextObject;
  ActiveLayer : TDiagramLayer;
  I : Integer;
begin
  if Assigned(ActiveObject) then Obj:=ActiveObject
  else Obj:=TBaseObject(TreeView.Selected.Data);
  if Obj is TGroupObject then PreviewObj:=TGroupObject(Obj).TextObject
  else PreviewObj:=Obj as TTextObject;
  Str:=PString(Obj.Properties[opText])^;
  if TTextEditorForm.Execute(Str,PreviewObj) then
  begin
    ActiveLayer:=GetActiveLayer;
    if ActiveLayer.IndexOf(Obj)=-1 then
    begin
      if Obj is TGroupObject then PString(TGroupObject(Obj).TextObject.Properties[opText])^:=Str
      else (Obj as TTextObject).SetTextAndName(Str);
    end
    else
      for I:=0 to ActiveLayer.Count-1 do
      begin
        Obj:=ActiveLayer.Objects[I];
        if Obj.Selected then
        begin
          if Obj is TTextObject then TTextObject(Obj).SetTextAndName(Str)
          else if (Obj is TGroupObject) and (TGroupObject(Obj).TextObject<>nil) then
            PString(TGroupObject(Obj).TextObject.Properties[opText])^:=Str;
        end;
      end;
    StoreUndoPoint(ItemCaption(EditTextAction),True);
  end;
  ActiveControl:=nil; 
end;

procedure TMainForm.TreeViewEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
  DeleteAction.Enabled:=False;
  PasteAction.Enabled:=False;
end;

procedure TMainForm.TreeViewExit(Sender: TObject);
begin
  UpdateControlStates;
end;

procedure TMainForm.TreeViewEdited(Sender: TObject; Node: TTreeNode; var S: String);
begin
  UpdateControlStates;
  TBaseObject(Node.Data).Name:=S;
  StoreUndoPoint(rsRename,False);
end;

procedure TMainForm.BringToFrontActionExecute(Sender: TObject);
begin
  if ActiveObject=nil then // Move object in group
    (TObject(TreeView.Selected.Parent.Data) as TGroupObject).Group.MoveObjectToFront(TreeView.Selected.Data)
  else
    GetActiveLayer.MoveSelectedToFront;
  StoreUndoPoint(BringToFrontAction.Caption,True);
end;

procedure TMainForm.SendToBackActionExecute(Sender: TObject);
begin
  if ActiveObject=nil then // Move object in group
    (TObject(TreeView.Selected.Parent.Data) as TGroupObject).Group.MoveObjectToBack(TreeView.Selected.Data)
  else
    GetActiveLayer.MoveSelectedToBack;
  StoreUndoPoint(SendToBackAction.Caption,True);
end;

procedure TMainForm.PropertiesActionExecute(Sender: TObject);
var
  Modified : Boolean;
  Node : TTreeNode;
begin
  if GetActiveLayer.SelectCount>1 then Modified:=TPropertyEditorForm.Execute(GetActiveLayer,Options)
  else if GetActiveLayer.SelectCount=1 then Modified:=TPropertyEditorForm.Execute(ActiveObject,Options,False)
  else
  begin
    Node:=TreeView.Selected;
    Modified:=TPropertyEditorForm.Execute(TBaseObject(Node.Data),Options,Node.Parent<>nil);
    if Modified then
    while Assigned(Node.Parent) do
    begin
      Node:=Node.Parent;
      with TObject(Node.Data) as TGroupObject do UpdatePosition;
    end;
  end;
  if Modified then StoreUndoPoint(rsEditProperties,True);
  ActiveControl:=nil;
  UpdateStyleButtons; 
end;

procedure TMainForm.GroupActionExecute(Sender: TObject);
begin
  if GetActiveLayer.SelectCount>1 then
  begin
    ActiveObject:=TGroupObject.CreateFromSelection(GetActiveLayer,Options.DefaultGroupAnchors);
    if Options.DefaultGroupLinks then
    begin
      ActiveObject.Links:=Options.GetDefaultLinks;
      ActiveObject.Properties[opBoundsOptions]:=1;
    end;
    with GetActiveLayer do
    begin
      DeleteSelected;
      Add(ActiveObject);
    end;
    StoreUndoPoint(GroupAction.Caption,True);
  end;
end;

procedure TMainForm.UngroupActionExecute(Sender: TObject);
var
  I, J, ObjectCount : Integer;
begin
  with GetActiveLayer do
  begin
    // Dissolve all groups
    for I:=Count-1 downto 0 do
      if Objects[I].Selected and (Objects[I] is TGroupObject) then
      begin
        ObjectCount:=Count;
        TGroupObject(Objects[I]).DissolveGroup(GetActiveLayer);
        Delete(I);
        for J:=ObjectCount-1 to Count-1 do Move(J,I+J-(ObjectCount-1));
        ResetLinkIndices;
        if I<Count then ActiveObject:=Objects[I] else ActiveObject:=nil; // Check in case the group was empty (which should not be possible)
      end;
  end;
  StoreUndoPoint(UngroupAction.Caption,True);
end;

procedure TMainForm.MakePolygonActionExecute(Sender: TObject);
var
  I : Integer;
  Polygon : TPolygonObject;
begin
  with GetActiveLayer do
  begin
    ActiveObject:=nil;
    Polygon:=nil;
    for I:=0 to Count-1 do if Objects[I].Selected then
    begin
      if Objects[I] is TGroupObject then
        Polygon:=TGroupObject(Objects[I]).CreatePolygon
      else if Objects[I] is TCurveLineObject then
        Polygon:=TCurveLineObject(Objects[I]).CreatePolygon
      else Continue;
      Objects[I].Free;
      Objects[I]:=Polygon;
      if ActiveObject=nil then ActiveObject:=Polygon
      else Polygon.Selected:=True;
    end;
  end;
  StoreUndoPoint(MakePolygonAction.Caption,True);
end;

procedure TMainForm.MakeMetafileActionExecute(Sender: TObject);
const
  MetafileMargin = 1;
var
  Metafile : TMetafile;
  MetafileCanvas : TMetafileCanvas;
  MetafileCanvasInfo : TCanvasInfo;
  Objects : TDiagramLayer;
  Bounds : TRect;
  Clipboard : TMemStream;
  ObjectName : string;
  SelCount, D : Integer;
begin
  SelCount:=GetActiveLayer.SelectCount;
  if (SelCount<=1) and (ActiveObject is TMetafileObject) then Exit;
  if SelCount=1 then ObjectName:=ActiveObject.Name;
  ActiveObject:=nil;
  Clipboard:=TMemStream.Create;
  try
    GetActiveLayer.SaveSelected(Clipboard);
    Objects:=TDiagramLayer.Create;
    try
      Clipboard.Position:=0;
      Objects.LoadFromStream(Clipboard,CurrentFileVersion);
      Bounds:=Objects.GetBounds;
      ZeroMem(MetafileCanvasInfo,SizeOf(MetafileCanvasInfo));
      with MetafileCanvasInfo do
      begin
        Scale:=FloatPoint(1200/DesignerDPI,1200/DesignerDPI);
        DrawMode:=dmRender;
        DefaultFont:=DrawPanel.Font;
        Offset:=Point(Round(-Bounds.Left*Scale.X)+MetafileMargin,Round(-Bounds.Top*Scale.Y)+MetafileMargin);
      end;
      Metafile:=TMetafile.Create;
      try
        Metafile.Width:=Round((Bounds.Right-Bounds.Left)*MetafileCanvasInfo.Scale.X)+MetafileMargin*2;
        Metafile.Height:=Round((Bounds.Bottom-Bounds.Top)*MetafileCanvasInfo.Scale.Y)+MetafileMargin*2;
        MetafileCanvas:=TMetafileCanvas.Create(Metafile,0);
        try
          Objects.Draw(MetafileCanvas,MetafileCanvasInfo);
        finally
          MetafileCanvas.Free;
        end;
        GetActiveLayer.DeleteSelected;
        ActiveObject:=TMetafileObject.CreateNew(Metafile);
        if ObjectName<>'' then ActiveObject.Name:=ObjectName;
      finally
        Metafile.Free;
      end;
    finally
      Objects.Free;
    end;
  finally
    Clipboard.Free;
  end;
  GetActiveLayer.Add(ActiveObject);
  D:=Round(MetafileMargin*DesignerDPI/1200);
  InflateRect(Bounds,D,D);
  ActiveObject.Position:=Bounds;
  StoreUndoPoint(MakeMetafileAction.Caption,True);
end;

procedure TMainForm.AddTemplateActionExecute(Sender: TObject);
begin
  Templates.AddTemplate(GetActiveLayer,ActiveObject);
  UpdateTemplates;
end;

procedure TMainForm.TemplateFrameDblClick(Sender: TObject);
begin
  TemplateFrame.EndDrag(False); // Doesn't work - what to do?
  TemplatePropertiesActionExecute(nil);
end;
                      
procedure TMainForm.TemplatePropertiesActionExecute(Sender: TObject);
begin
  if Assigned(TemplateObject) and TPropertyEditorForm.Execute(TemplateObject,Options,False) then UpdateTemplates;
  Templates.UpdateHeight;
  ActiveControl:=nil;
end;

procedure TMainForm.Splitter1Moved(Sender: TObject);
begin
  ShowTreeAction.Checked:=TreeView.Width>1;
  if not ShowTreeAction.Checked then TreeView.Items.Clear
  else if TreeView.Items.Count=0 then UpdateObjectTree
end;

procedure TMainForm.ShowTreeActionExecute(Sender: TObject);
begin
  ShowTreeAction.Checked:=not ShowTreeAction.Checked;
  if ShowTreeAction.Checked then
  begin
    TreeView.Width:=130;
    UpdateObjectTree;
  end
  else
  begin
    TreeView.Width:=1;
    TreeView.Items.Clear;
  end;
end;

procedure TMainForm.UpdateObjectTree;
var
  FocusObject : TBaseObject;
begin
  if ShowTreeAction.Checked then
  begin
    FocusObject:=nil;
    if Assigned(ActiveObject) then FocusObject:=ActiveObject
    else if Assigned(TreeView.Selected) then FocusObject:=TBaseObject(TreeView.Selected.Data);
    TreeView.Selected:=nil;
    BuildTree(TreeView,GetActiveLayer);
    if Assigned(FocusObject) then TreeView.Selected:=TTreeNode(FocusObject.TreeNode)
    else TreeView.Selected:=nil;
  end;
end;

procedure TMainForm.TreeViewEnter(Sender: TObject);
begin
  if (ActiveObject=nil) and (TreeView.Items.Count>0) then // Don't focus first object when entering tree
    DontFocusObject:=TBaseObject(TreeView.Items[0].Data);
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) and (ActiveObject<>TBaseObject(Node.Data)) then
  begin
    if TBaseObject(Node.Data)=DontFocusObject then Node.Selected:=False
    else
    begin
      GetActiveLayer.DeselectAll;
      if Node.Parent=nil then ActiveObject:=TBaseObject(Node.Data)
      else
      begin
        ActiveObject:=nil;
        TreeView.Selected:=Node;
      end;
    end;
  end;
  DontFocusObject:=nil;
end;

procedure TMainForm.AlignActionExecute(Sender: TObject);
var
  Bounds : TRect;
begin
  with GetActiveLayer do
  begin
    if SelectCount=1 then // Align to page
    begin
      with GetActivePage do
      if Options.ShowMargins and (PrinterMargins.Right>PrinterMargins.Left) then
        Bounds:=Rect(PrinterMargins.Left,PrinterMargins.Top,Min(PrinterMargins.Right,Width),Min(PrinterMargins.Bottom,Height))
      else Bounds:=Rect(0,0,Width,Height);
    end
    else Bounds:=GetSelectedBounds; // Align to selection
    if Sender=AlignLeftAction then AlignSelected(amLeft,Bounds)
    else if Sender=AlignCenterHAction then AlignSelected(amCenterHorz,Bounds)
    else if Sender=AlignRightAction then AlignSelected(amRight,Bounds)
    else if Sender=AlignDistributeHAction then AlignSelected(amDistributeHorz,Bounds)
    else if Sender=AlignTopAction then AlignSelected(amTop,Bounds)
    else if Sender=AlignCenterVAction then AlignSelected(amCenterVert,Bounds)
    else if Sender=AlignBottomAction then AlignSelected(amBottom,Bounds)
    else if Sender=AlignDistributeVAction then AlignSelected(amDistributeVert,Bounds)
    else if Sender=AlignPageAction then AlignSelected(amFill,Bounds)
    else Assert(False,'Not implemented');
  end;
  StoreUndoPoint((Sender as TAction).Caption,False);
end;

procedure TMainForm.RotateActionExecute(Sender: TObject);
var
  Center : TPoint;
  Angle : EvalFloat;
begin
  Center:=CenterPoint(GetActiveLayer.GetSelectedBounds);
  if Sender=FlipLRAction then GetActiveLayer.RotateSelected(0,True,False,Center)
  else if Sender=FlipUDAction then GetActiveLayer.RotateSelected(0,False,True,Center)
  else if Sender=Rotate90Action then GetActiveLayer.RotateSelected(-90/180*Pi,False,False,Center)
  else if Sender=Rotate180Action then GetActiveLayer.RotateSelected(-180/180*Pi,False,False,Center)
  else if Sender=Rotate270Action then GetActiveLayer.RotateSelected(-270/180*Pi,False,False,Center)
  else
  begin
    Angle:=45;
    if FloatQuery(rsRotate,rsRotationAngle+':',Angle,-1000,1000) then
      GetActiveLayer.RotateSelected(-Angle/180*Pi,False,False,Center)
    else Abort;
  end;
  StoreUndoPoint((Sender as TAction).Caption,False);
end;

//==============================================================================================================================
// Diagram
//==============================================================================================================================
function TMainForm.GetActivePage: TDiagramPage;
begin
  Assert((FActivePage>=0) and (FActivePage<Diagram.Count));
  Result:=Diagram.Pages[FActivePage];
end;

procedure TMainForm.SetActivePageIndex(Index: Integer);
var
  I : Integer;
begin
  if Index<>FActivePage then
  begin
    Assert((Index>=0) and (Index<Diagram.Count));
    FActivePage:=Index;
    CanvasInfo.PageIndex:=Index;
    CanvasInfo.Container:=Diagram;
    for I:=GetActivePage.Count-1 downto 0 do
      if (I=0) or (GetActivePage.Layers[I].Count>0) then
      begin
        ActiveLayerIndex:=I;
        Break;
      end;
    with GetActivePage do StatusBar.Panels[sbpPage].Text:=GetName(Index);
    if Index>=PageTabSet.Tabs.Count then UpdatePageTabs;
    PageTabSet.TabIndex:=Index;
    UpdateScrollBars;
  end;
end;

procedure TMainForm.SetActiveLayerIndex(Index: Integer);

  function LayerAction(LayerIndex: Integer): TAction;
  begin
    case LayerIndex of
      -1 : Result:=EditStencilAction;
      0  : Result:=EditLayer1Action;
      1  : Result:=EditLayer2Action;
      2  : Result:=EditLayer3Action;
      else
      begin
        Assert(False);
        Result:=nil;
      end;
    end;
  end;

var
  I : Integer;
begin
  with GetActivePage do if Index>=Count then
    for I:=Count to Index do Layers[Add(TDiagramLayer.Create)].DrawColor:=-1;
  LayerAction(FActiveLayer).Checked:=False;
  ResetHistory;
  ActiveControl:=nil;
  ActiveObject:=nil;
  FActiveLayer:=Index;
  LayerAction(Index).Checked:=True;
  AddToHistory;
  GetActiveLayer.DeselectAll;
  if Index<0 then
  begin
    StatusBar.Panels[sbpLayer].Text:=EditStencilAction.Caption;
    LayerTabSet.TabIndex:=0;
  end
  else
  begin
    StatusBar.Panels[sbpLayer].Text:=Format(rsLayerD,[Index+1]);
    LayerTabSet.TabIndex:=Index+1;
  end;
  UpdateObjectTree;
  UpdateDrawing;
end;

function TMainForm.GetActiveLayer: TDiagramLayer;
begin
  if FActiveLayer=-1 then Result:=Diagram.Stencil
  else
  begin
    Assert((FActivePage>=0) and (FActivePage<Diagram.Count));
    Assert((FActiveLayer>=0) and (FActiveLayer<Diagram.Pages[FActivePage].Count));
    Result:=Diagram.Pages[FActivePage].Layers[FActiveLayer];
  end;
end;

procedure TMainForm.StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Left : Integer;
begin
  // Check if page menu
  Left:=StatusBar.Panels[0].Width;
  if (X>Left) and (X<Left+StatusBar.Panels[sbpPage].Width) then
    PageMenu.Popup(Self.Left+Left+2,Top+Height-StatusBar.Height)
  else
  begin
    // Check if layer menu
    Inc(Left,StatusBar.Panels[sbpPage].Width);
    if (X>Left) and (X<Left+StatusBar.Panels[sbpLayer].Width) then
      LayerMenu.Popup(Self.Left+Left+2,Top+Height-StatusBar.Height)
  end;
end;

procedure TMainForm.NewPageActionExecute(Sender: TObject);
var
  NewPage : TDiagramPage;
begin
  NewPage:=TDiagramPage.Create;
  NewPage.New(GetActivePage);
  Diagram.Insert(ActivePageIndex+1,NewPage);
  UpdatePageTabs;
  SetActivePageIndex(ActivePageIndex+1);
  Modified:=1;
end;

procedure TMainForm.SetLayerActionExecute(Sender: TObject);
var
  Layer : Integer;
begin
  Layer:=(Sender as TAction).Tag;
  if Layer<>ActiveLayerIndex then ActiveLayerIndex:=Layer;
end;

procedure TMainForm.LayerTabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin
  Dec(NewTab);
  if NewTab<>ActiveLayerIndex then ActiveLayerIndex:=NewTab;
end;

procedure TMainForm.SetLayerColorActionExecute(Sender: TObject);
var
  Color : TColor;
begin
  Color:=clBlack;
  if ShowColorDialog(Color) then with GetActiveLayer do
  begin
    SelectAll;
    SetMemberProperty(opLineColor,Color,True);
    SetMemberProperty(opFillColor,Color,True);
    SetMemberProperty(opGradientColor,Color,True);
    SetMemberProperty(opTextColor,Color,True);
  end;
  StoreUndoPoint(SetLayerColorAction.Caption,False);
  ActiveControl:=nil;
end;

procedure TMainForm.SelectPageActionExecute(Sender: TObject);
begin
  with Sender as TComponent do SetActivePageIndex(Tag);
end;

procedure TMainForm.PageMenuPopup(Sender: TObject);
var
  I : Integer;
  PageItem : TMenuItem;
  PageName : string;
begin
  if Assigned(PageActions) then PageActions.Clear
  else PageActions:=TObjectList.Create;
  for I:=0 to Diagram.Count-1 do
  begin
    PageItem:=TMenuItem.Create(nil);
    PageActions.Add(PageItem);
    PageName:=Diagram.Pages[I].GetName(I);
    with PageItem do
    begin
      Caption:=PageName;
      Hint:=rsEdit+' '+PageName;
      Tag:=I;
      OnClick:=SelectPageActionExecute;
      if I<9 then ShortCut:=Menus.ShortCut(Byte('1')+I,[ssCtrl])
      else if I=9 then ShortCut:=Menus.ShortCut(Byte('0'),[ssCtrl]);
      if I=ActivePageIndex then Checked:=True;
    end;
    PageMenu.Items.Add(PageItem);
  end;
end;

procedure TMainForm.DiagramFontActionExecute(Sender: TObject);
var
  UpdateFont : Boolean;
  FontDialog : TFontDialog;
begin
  FontDialog:=TFontDialog.Create(nil);
  with FontDialog do
  try
    MinFontSize:=5;
    MaxFontSize:=255;
    Font:=DrawPanel.Font;
    Font.Color:=DefaultProperties.Properties[opTextColor];
    Options:=[fdLimitSize,fdEffects];
    SetApplicationHandleForDialog(0,FontDialog);
    try
      UpdateFont:=Execute;
    finally
      RestoreApplicationHandle;
    end;
    if UpdateFont then
    begin
      Diagram.DefaultFontName:=Font.Name;
      Diagram.DefaultFontSize:=Font.Size;
      Diagram.DefaultFontStyle:=Byte(Font.Style);
      Diagram.DefaultFontCharSet:=Font.Charset;
      DefaultProperties.Properties[opTextColor]:=Font.Color;
      CanvasInfo.DefaultFont.Name:=Diagram.DefaultFontName;
      CanvasInfo.DefaultFont.Size:=Diagram.DefaultFontSize;
      CanvasInfo.DefaultFont.Style:=TFontStyles(Byte(Diagram.DefaultFontStyle));
      CanvasInfo.DefaultFont.Charset:=Diagram.DefaultFontCharSet;
      ResetHistory;
      Modified:=1;
      AddToHistory;
      UpdateDrawing;
      UpdateStyleButtons;
    end;
  finally
    Free;
  end;
  ActiveControl:=nil;
end;

procedure TMainForm.PagePropertiesActionExecute(Sender: TObject);
begin
  if TPagePropertiesForm.Execute(GetActivePage,Diagram,Options,Assigned(Sender)) then
  begin
    ResetHistory;
    Modified:=1;
    AddToHistory;
    UpdatePrinterMargins;
    UpdateDrawing;
    UpdateScrollBars;
    with GetActivePage do StatusBar.Panels[sbpPage].Text:=GetName(ActivePageIndex);
  end;
  ActiveControl:=nil;
end;

procedure TMainForm.ReorderPagesActionExecute(Sender: TObject);
var
  Page : Integer;
  DiagramModified : Boolean;
begin
  Page:=TRearrangePagesForm.Execute(Diagram,ActivePageIndex,DiagramModified);
  UpdatePageTabs;
  FActivePage:=-1; SetActivePageIndex(Page);
  if DiagramModified then Modified:=1;
  ActiveControl:=nil;
end;

procedure TMainForm.UpdatePageTabs;
var
  I : Integer;
begin
  for I:=0 to Diagram.Count-1 do
    if I>=PageTabSet.Tabs.Count then PageTabSet.Tabs.Add(IntToStr(I+1));
  while PageTabSet.Tabs.Count>Diagram.Count do PageTabSet.Tabs.Delete(PageTabSet.Tabs.Count-1); 
end;

procedure TMainForm.PageTabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin
  ActivePageIndex:=NewTab;
end;

procedure TMainForm.ConnectLinksActionExecute(Sender: TObject);

  function ConnectLinks(ConnectAnything: Boolean): Boolean;
  var
    I, J, LinkIndex : Integer;
    P : TPoint;
  begin
    Result:=False;
    with GetActiveLayer do
      for I:=0 to Count-1 do
        if ConnectAnything or not (Objects[I] is TBaseConnectorObject) then with Objects[I] do
          for LinkIndex:=0 to High(Links) do
          begin
            P:=GetLinkPosition(LinkIndex);
            for J:=0 to Count-1 do
              if (I<>J) and (Objects[J] is TBaseConnectorObject) then
                with TBaseConnectorObject(Objects[J]) do
                  if not HasLinkObject(1) and (P1.X=P.X) and (P1.Y=P.Y) then
                  begin
                    MakeLink(1,Objects[I],LinkIndex);
                    Result:=True;
                  end
                  else if not HasLinkObject(2) and (P2.X=P.X) and (P2.Y=P.Y) then
                  begin
                    MakeLink(2,Objects[I],LinkIndex);
                    Result:=True;
                  end;
        end;
  end;

var
  MadeNewLink : Boolean;
begin
  Screen.Cursor:=crHourGlass;
  try
    // First connect to objects that are not connectors
    MadeNewLink:=ConnectLinks(False);
      // Now connect anything
    MadeNewLink:=ConnectLinks(True) or MadeNewLink;
    if MadeNewLink then StoreUndoPoint(ConnectLinksAction.Caption,False);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TMainForm.SpellCheckActionExecute(Sender: TObject);
begin
  if SpellCheckForm=nil then SpellCheckForm:=TSpellCheckForm.Create(Application);
  if SpellCheckForm.LanguageBox.Items.Count=0 then
  begin
    FreeAndNil(SpellCheckForm);
    MessageDlg(rsNoDictionariesFound,mtError);
  end
  else if SpellCheckForm.ShowModal=mrOk then
  begin
    UpdateObjectTree;
    ResetHistory; // Undo history can only handle one layer and the spell checker might modify more so we should to clear it
    AddToHistory;
    Modified:=1;
    UpdateControlStates;
  end;            
  ActiveControl:=nil;
end;

procedure TMainForm.LoadTemplatePaletteItemClick(Sender: TObject);
begin
  if Sender=DefaultPaletteItem then LoadTemplatePalette('')
  else LoadTemplatePalette((Sender as TMenuItem).Hint);
  ActiveControl:=nil;
end;

procedure TMainForm.LoadTemplatePaletteMenuClick(Sender: TObject);
var
  DirList : TStringList;
  Item : TMenuItem;
  I : Integer;
begin
  DirList:=TStringList.Create;
  try
    GetDirList(ForceBackslash(PalettePath)+'*.ddt',DirList);
    for I:=LoadTemplatePaletteMenu.Count-1 downto 2 do LoadTemplatePaletteMenu.Items[I].Free;
    if DirList.Count=0 then
    begin
      Item:=TMenuItem.Create(LoadTemplatePaletteMenu);
      Item.Caption:=rsNoFilesFound;
      Item.Enabled:=False;
      LoadTemplatePaletteMenu.Add(Item);
    end
    else
    begin
      for I:=0 to DirList.Count-1 do
      begin
        Item:=TMenuItem.Create(LoadTemplatePaletteMenu);
        Item.Caption:=RemoveFileExt(ExtractFileName(DirList[I]));
        Item.Hint:=DirList[I];
        Item.OnClick:=LoadTemplatePaletteItemClick;
        LoadTemplatePaletteMenu.Add(Item);
      end;
    end;
  finally
    DirList.Free;
  end;
end;

procedure TMainForm.TemplateComboBoxDropDown(Sender: TObject);
var
  I : Integer;
  Prev : string;
begin
  Prev:=TemplateComboBox.Text;
  TemplateComboBox.Items.Text:=StripHotkey(DefaultPaletteItem.Caption);
  GetDirList(ForceBackslash(PalettePath)+'*.ddt',TemplateComboBox.Items);
  for I:=0 to TemplateComboBox.Items.Count-1 do
  begin
    TemplateComboBox.Items[I]:=RemoveFileExt(ExtractFileName(TemplateComboBox.Items[I]));
    if TemplateComboBox.Items[I]=Prev then TemplateComboBox.ItemIndex:=I;
  end;
end;

procedure TMainForm.TemplateComboBoxChange(Sender: TObject);
begin
  if TemplateComboBox.ItemIndex=0 then LoadTemplatePalette('')
  else LoadTemplatePalette(PalettePath+TemplateComboBox.Text+'.ddt');
end;

procedure TMainForm.LoadTemplatePalette(PaletteName: string);
var
  I : Integer;
begin
  if PaletteName='' then
  begin
    Templates.MakeDefault;
    Setup.DeleteValue('PaletteName');
    PaletteName:=StripHotkey(DefaultPaletteItem.Caption);
  end
  else
  begin
    Templates.LoadFromFile(PaletteName);
    Setup.WriteString('PaletteName',PaletteName);
    PalettePath:=ExtractFilePath(PaletteName);
    PaletteName:=RemoveFileExt(ExtractFileName(PaletteName));
  end;
  UpdateTemplates;

  I:=TemplateComboBox.Items.IndexOf(PaletteName);
  if I>=0 then TemplateComboBox.ItemIndex:=I
  else
  begin
    TemplateComboBox.Items.Text:=PaletteName;
    TemplateComboBox.ItemIndex:=0;
  end;
end;

procedure TMainForm.CheckForUpdatesActionExecute(Sender: TObject);
begin
  ExecuteFile('http://meesoft.logicnet.dk/DiagramDesigner/CheckForUpdates.php?Version='+
              IntToStrLeadZero(ThisApp.FileVersion.Major,3)+
              IntToStrLeadZero(ThisApp.FileVersion.Minor,3)+
              IntToStrLeadZero(ThisApp.FileVersion.Build,3));
  ActiveControl:=nil;
end;

procedure TMainForm.ObjectShadowsActionExecute(Sender: TObject);
begin
  Diagram.ObjectShadows:=ObjectShadowsAction.Checked;
  UpdateDrawing;
  Inc(Modified);
  ActiveControl:=nil;
  UpdateControlStates;
end;

procedure TMainForm.AutoLineBreakActionExecute(Sender: TObject);
begin
  Diagram.AutoLineBreak:=AutoLineBreakAction.Checked;
  UpdateDrawing;
  Inc(Modified);
  ActiveControl:=nil;
  UpdateControlStates;
end;

procedure TMainForm.ConnectorLabelStyleActionExecute(Sender: TObject);
begin
  Diagram.ConnectorLabelStyle:=TConnectorLabelStyle((Sender as TAction).Tag);
  UpdateDrawing;
  Inc(Modified);
  ActiveControl:=nil;
  UpdateControlStates;
end;

procedure TMainForm.FindTextActionExecute(Sender: TObject);
begin
  FreeAndNil(TextSearch);
  FindDialog.Execute;
end;

procedure TMainForm.FindDialogFind(Sender: TObject);
begin
  if (TextSearch<>nil) and
     ((TextSearch.FindText<>FindDialog.FindText) or
      (TextSearch.CaseSensitive<>(frMatchCase in FindDialog.Options))) then FreeAndNil(TextSearch);
  if TextSearch=nil then TextSearch:=TDiagramTextSearch.Create(Diagram,FindDialog.FindText,frMatchCase in FindDialog.Options);
  if not TextSearch.FindNext then
  begin
    FreeAndNil(TextSearch);
    MessageDlg(rsTextNotFound,mtInformation);
  end;
end;

procedure TMainForm.PopupMenuPopup(Sender: TObject);
begin
  if FUpdateControlStates then SetControlStates;
end;

procedure TMainForm.FillColorButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then FillColorActionExecute(nil);
end;

procedure TMainForm.LineColorButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then LineColorActionExecute(nil);
end;

procedure TMainForm.GradientColorButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then GradientColorActionExecute(nil);
end;

function TMainForm.TreeViewDragObject(Source: TObject; X, Y: Integer; Apply: Boolean): Boolean;
var
  SourceNode, TargetNode : TTreeNode;
  SourceObject, TargetObject : TBaseObject;
  SourceIndex, TargetIndex : Integer;
  SourceGroup, TargetGroup : TBaseObjectList;
  Layer : TDiagramLayer;
begin
  Result:=False;
  if Source<>TreeView then Exit;
  SourceNode:=TreeView.Selected;
  TargetNode:=TreeView.GetNodeAt(X,Y);
  if (SourceNode=nil) or (TargetNode=nil) or (SourceNode=TargetNode) then Exit;
  SourceObject:=TObject(SourceNode.Data) as TBaseObject;
  TargetObject:=TObject(TargetNode.Data) as TBaseObject;
  Layer:=GetActiveLayer;
  Layer.IndexAndParentOf(SourceObject,SourceIndex,SourceGroup);
  if SourceGroup=nil then Exit;
  Layer.IndexAndParentOf(TargetObject,TargetIndex,TargetGroup);
  if TargetGroup<>SourceGroup then Exit;
  Result:=True;
  if not Apply then Exit;
  SourceGroup.Move(SourceIndex,TargetIndex);
  SourceGroup.ResetLinkIndices;
  StoreUndoPoint(rsMove,True);
end;

procedure TMainForm.TreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=TreeViewDragObject(Source,X,Y,False);
end;

procedure TMainForm.TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  TreeViewDragObject(Source,X,Y,True);
end;

end.

