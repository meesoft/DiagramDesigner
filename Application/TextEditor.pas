unit TextEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StyleForm, StdCtrls, ComCtrls, PanelFrame, ShapeObject, DiagramBase,
  Consts, TextObject;

resourcestring
  rsBold = 'Bold';
  rsItalic = 'Italic';
  rsUnderline = 'Underline';
  rsOverline = 'Overline';
  rsSuperscript = 'Superscript';
  rsSubscript = 'Subscript';
  rsStrikeout = 'Strikeout';
  rsSymbolFont = 'Symbol font';
  rsSetFontByName = 'Set font by name';
  rsFont = 'Font';
  rsThreeDigitFontSize = 'Three-digit font size';
  rsNewLine = 'New line';
  rsPageNumber = 'Page number';
  rsPageCount = 'Page count';
  rsPageTitle = 'Page title';
  rsHTMLFormattedColor = 'HTML formatted color';
  rsDiagramOrInternetLink = 'Diagram or Internet link';
  rsPopupNote = 'Popup note';
  rsSeparatorLine = 'Separator line';
  rsDisableEscapeChars = 'Disable escape chars';

type
  TTextEditorForm = class(TStyleForm)
    RichEdit: TRichEdit;
    OKButton: TButton;
    CancelButton: TButton;
    Label1: TLabel;
    LabelCode1: TLabel;
    LabelDescription1: TLabel;
    Label2: TLabel;
    LabelCode2: TLabel;
    LabelDescription2: TLabel;
    Preview: TDoubleBufferedPanel;
    ClipPanelFrame: TPanelFrame;
    Label3: TLabel;
    HighlightSyntaxBox: TCheckBox;
    procedure RichEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RichEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewPaint(Sender: TObject);
    procedure LabelCodeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HighlightSyntaxBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    PreviewObject, SourceObject : TTextObject;
    CanvasInfo : TCanvasInfo;
    function ObjectText: string;
  public
    class function Execute(var AText: string; ASourceObject: TTextObject): Boolean;
  end;

implementation

uses Main, MathUtils, StringUtils, ColorDialog, ValueEdits, LinarBitmap, Settings;

{$R *.dfm}

{ TTextEditorForm }

class function TTextEditorForm.Execute(var AText: string; ASourceObject: TTextObject): Boolean;
var
  InString : Boolean;
  I : Integer;
  Str : string;
begin
  with Create(nil) do
  try
    InString:=False;
    Str:=AText;
    I:=1;
    while I<Length(Str) do
    begin
      if (Str[I]='"') and (I>1) and (InString or (Str[I-1]='\')) then InString:=not InString;
      if not InString and (Str[I]='\') then
        case Str[I+1] of
          'n' : begin
                  Str[I]:=#13;
                  Str[I+1]:=#10;
                  Inc(I);
                end;
          '_' : begin
                  Insert(#13#10,Str,I+2);
                  Inc(I,3);
                end;
        end;
      Inc(I);
    end;
    SourceObject:=ASourceObject;
    RichEdit.Lines.Text:=Str;
    RichEdit.SelStart:=0;
    RichEdit.SelLength:=Length(Str);
    Str:=PString(SourceObject.Properties[opText])^;
    Result:=ShowModal=mrOk;
    SourceObject.Properties[opText]:=Integer(@Str);
    if Result then AText:=ObjectText
    else MainForm.UpdateDrawing;
  finally
    Free;
  end;
end;

procedure TTextEditorForm.FormCreate(Sender: TObject);

  procedure AddFormatCode1(const Code,Description: string);
  begin
    with LabelCode1 do Caption:=Caption+Code+#13;
    with LabelDescription1 do Caption:=Caption+Description+#13;
  end;

  procedure AddFormatCode2(const Code,Description: string);
  begin
    with LabelCode2 do Caption:=Caption+Code+#13;
    with LabelDescription2 do Caption:=Caption+Description+#13;
  end;

begin
  UseBackgroundTheme:=True;
  // Create help
  LabelDescription1.Caption:='';
  LabelCode1.Caption:='';
  LabelDescription2.Caption:='';
  LabelCode2.Caption:='';
  AddFormatCode1('\B ... \b',rsBold);              AddFormatCode2('\I ... \i',rsItalic);
  AddFormatCode1('\U ... \u',rsUnderline);         AddFormatCode2('\O ... \o',rsOverline);
  AddFormatCode1('\H ... \h',rsSuperscript);       AddFormatCode2('\L ... \l',rsSubscript);
  AddFormatCode1('\T ... \t',rsStrikeout);         AddFormatCode2('\S ... \s',rsSymbolFont);
  AddFormatCode1('\p',rsPageNumber);               AddFormatCode2('\c',rsPageCount);
  AddFormatCode1('\P',rsPageTitle);                AddFormatCode2('\A',rsDiagramOrInternetLink);
  AddFormatCode1('\_',rsSeparatorLine);            AddFormatCode2('\n',rsNewLine);
  AddFormatCode1('\N',rsPopupNote);                AddFormatCode2('\\','\');
  AddFormatCode1('\"'+rsFont+'"',rsSetFontByName); AddFormatCode2('\@ ... \@',rsDisableEscapeChars);
  AddFormatCode1('\###',rsThreeDigitFontSize);
  AddFormatCode1('\C######',rsHTMLFormattedColor);

  // Prepare controls
  Label2.Top:=LabelCode1.BoundsRect.Bottom;
  HighlightSyntaxBox.Top:=Label2.Top-2;
  RichEdit.Top:=Label2.BoundsRect.Bottom+4;
  ClientHeight:=RichEdit.BoundsRect.Bottom+OKButton.Height+10;
  RichEdit.Anchors:=[akLeft,akTop,akRight,akBottom];
  Preview.Top:=ClientHeight-Preview.Height;
  OKButton.Top:=ClientHeight-OKButton.Height-4;
  CancelButton.Top:=OKButton.Top;
  Constraints.MinHeight:=Height;
  Constraints.MinWidth:=Width;
  OKButton.Hint:=SmkcCtrl+SmkcEnter;
  ClientWidth:=LabelDescription2.Left+LabelDescription2.Width+8;
  RichEdit.Width:=ClientWidth-8;
  CancelButton.Left:=ClientWidth-8-CancelButton.Width;
  OKButton.Left:=CancelButton.Left-8-OKButton.Width;
  Preview.Width:=OKButton.Left-8;
  // Prepare preview
  PreviewObject:=TTextObject.Create;
  PreviewObject.Position:=Rect(0,0,Round(Preview.Width/MainForm.ScreenScale),0);
  PreviewObject.Properties[opTextXAlign]:=-1;
  CanvasInfo.DefaultFont:=MainForm.DrawPanel.Font;
  CanvasInfo.Offset.Y:=Preview.Height div 2;
  CanvasInfo.Scale:=FloatPoint(MainForm.ScreenScale,MainForm.ScreenScale);;
  CanvasInfo.PageIndex:=MainForm.ActivePageIndex;
  CanvasInfo.Container:=MainForm.Diagram;
end;

procedure TTextEditorForm.FormDestroy(Sender: TObject);
begin
  PreviewObject.Free;
end;

procedure TTextEditorForm.FormShow(Sender: TObject);
begin
  HighlightSyntaxBox.Checked:=Setup.GetBoolean('HighlightSyntax',HighlightSyntaxBox.Checked);
end;

procedure TTextEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Setup.WriteBoolean('HighlightSyntax',HighlightSyntaxBox.Checked);
end;

function TTextEditorForm.ObjectText: string;
var
  I : Integer;
begin
  Result:=RichEdit.Lines[0];
  for I:=1 to RichEdit.Lines.Count-1 do
    if EndsWith('\_',RichEdit.Lines[I-1]) then Result:=Result+RichEdit.Lines[I]
    else Result:=Result+'\n'+RichEdit.Lines[I];
end;

procedure TTextEditorForm.RichEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  SelStart, SelLength : Integer;
begin
  case Key of
    VK_RETURN : if ssCtrl in Shift then
                begin
                  Key:=0;
                  ModalResult:=mrOk;
                end;
    VK_ESCAPE : ModalResult:=mrCancel;
    Word('B'),
    Word('I'),
    Word('H'),
    Word('L'),
    Word('U') : if ssCtrl in Shift then
                begin
                  SelStart:=RichEdit.SelStart;
                  SelLength:=RichEdit.SelLength;
                  if SelLength=0 then
                  begin
                    RichEdit.SelText:='\'+Char(Key)+'\'+LowerCase(Char(Key));
                    RichEdit.SelStart:=SelStart+2;
                  end
                  else
                  begin
                    if EndsWith(#13#10,RichEdit.SelText) then
                    begin
                      Dec(SelLength,2);
                      RichEdit.SelLength:=SelLength;
                    end;
                    RichEdit.SelText:='\'+Char(Key)+RichEdit.SelText+'\'+LowerCase(Char(Key));
                    RichEdit.SelStart:=SelStart+2;
                    RichEdit.SelLength:=SelLength;
                  end;
                  Key:=0;
                end;
  end;
end;

procedure TTextEditorForm.RichEditChange(Sender: TObject);
var
  Str : string;
  I, CodeLength, SelStart, SelLength : Integer;
begin
  if PreviewObject=nil then Exit;

  // Update preview
  Str:=ObjectText;
  PreviewObject.Properties[opText]:=Integer(@Str);
  Preview.Invalidate;
  SourceObject.Properties[opText]:=Integer(@Str);
  MainForm.UpdateDrawing;

  // Update syntax highlighting
  if HighlightSyntaxBox.Checked then
  begin
    SelStart:=RichEdit.SelStart;
    SelLength:=RichEdit.SelLength;
    Str:=RichEdit.Text;
    ClipPanelFrame.BoundsRect:=RichEdit.BoundsRect; // Prevent painting while working
    ClipPanelFrame.Visible:=True;
    RichEdit.SelectAll;
    RichEdit.SelAttributes.Color:=clWindowText;
    I:=Pos('\',Str);
    if I>0 then
      repeat
        if Str[I]='\' then
        begin
          case Str[I+1] of
            '0'..'9' : if not (Str[I+2] in ['0'..'9']) then CodeLength:=2
                       else if not (Str[I+3] in ['0'..'9']) then CodeLength:=3
                       else CodeLength:=4;
            '"'      : begin
                         CodeLength:=2;
                         while not (Str[I+CodeLength] in [#0,'"']) do Inc(CodeLength);
                         Inc(CodeLength);
                       end;
            'A','N'  : CodeLength:=Length(Str);
            'C'      : CodeLength:=8;
            else CodeLength:=2;
          end;
          RichEdit.SelStart:=I-1;
          RichEdit.SelLength:=CodeLength;
          RichEdit.SelAttributes.Color:=$ff8080;
          Inc(I,CodeLength);
        end
        else Inc(I);
      until I>Length(Str);
    RichEdit.SelStart:=SelStart;
    RichEdit.SelLength:=SelLength;
    ClipPanelFrame.Visible:=False;
  end;
end;

procedure TTextEditorForm.PreviewPaint(Sender: TObject);
begin
  Preview.Clear(Color,False);
  PreviewObject.Draw(Preview.BitmapCanvas,CanvasInfo,0);
end;

procedure TTextEditorForm.LabelCodeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Codes : TStringList;
  CodeLabel : TLabel;
  Code : string;
  I : Integer;
begin
  if (Sender=LabelCode1) or (Sender=LabelDescription1) then CodeLabel:=LabelCode1
  else CodeLabel:=LabelCode2;
  Codes:=TStringList.Create;
  try
    Codes.Delimiter:=#13;
    Codes.Text:=CodeLabel.Caption;
    Y:=Y*(Codes.Count+1) div CodeLabel.Height;
    if Y>=Codes.Count then Exit;
    Code:=Codes[Y];

    if Code='\C######' then // Color
    begin
      I:=CanvasInfo.DefaultFont.Color;
      if not ShowColorDialog(TColor(I)) then Exit;
      Code:='\C'+IntToHex(RGB2BGR(I),6);
    end
    else if Code='\###' then // Size
    begin
      I:=CanvasInfo.DefaultFont.Size;
      if not IntegerQuery(rsThreeDigitFontSize,'',I,1,999) then Exit;
      Code:='\'+IntToStrLeadZero(I,3);
    end;

    if RichEdit.SelLength=0 then RichEdit.SelText:=Code
    else
    begin
      if (Length(Code)=9) and (Code[3]=' ') then
      begin
        if EndsWith(#13#10,RichEdit.SelText) then RichEdit.SelLength:=RichEdit.SelLength-2;
        RichEdit.SelText:=Copy(Code,1,2)+RichEdit.SelText+Copy(Code,8,2)
      end
      else
        RichEdit.SelText:=Code+RichEdit.SelText;
    end;
  finally
    Codes.Free;
  end;
end;

procedure TTextEditorForm.HighlightSyntaxBoxClick(Sender: TObject);
var
  Str : string;
begin
  Str:=RichEdit.Text;
  RichEdit.Text:='';
  RichEdit.Text:=Str;
  RichEditChange(nil);
end;

end.

