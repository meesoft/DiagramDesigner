unit SpellChecker;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, SynSpellCheck, Dialogs, StyleForm, Consts;

type
  TSpellCheckForm = class(TStyleForm)
    CloseButton: TButton;
    StartButton: TButton;
    IgnoreAllButton: TButton;
    AddButton: TButton;
    LookUpLabel: TLabel;
    WordEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ObjectLabel: TLabel;
    TextLabel: TLabel;
    ListBox: TListBox;
    Label4: TLabel;
    ReplaceButton: TButton;
    Label5: TLabel;
    LanguageBox: TComboBox;
    ActiveLayerOnlyBox: TCheckBox;
    Bevel2: TBevel;
    SkipSymbolsBox: TCheckBox;
    StopButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IgnoreAllButtonClick(Sender: TObject);
    procedure WordEditChange(Sender: TObject);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure LanguageBoxChange(Sender: TObject);
  private
    { Private declarations }
    Modified, CheckInProgress : Boolean;
    ContinueAction : (caDont,caReplace,caIgnore,caStop);
    SpellChecker : TSynSpellCheck;
    procedure LoadDictionary;
  public
    { Public declarations }
  end;

var
  SpellCheckForm : TSpellCheckForm=nil;

implementation

uses Settings, Main, Math, DiagramBase,
  GroupObject, ShapeObject, StrUtils, TextObject;

{$R *.dfm}

resourcestring
  rsNotInDictionary = 'Not in dictionary:';
  rsStart = '&Start';
  rsSpellCheckComplete = 'Spell check complete.';

procedure TSpellCheckForm.FormCreate(Sender: TObject);
begin
  SpellChecker:=TSynSpellCheck.Create(Self);
  SpellChecker.DictionaryPath:=MainForm.Options.DictionaryPath;
  SpellChecker.GetDictionaryList(LanguageBox.Items);
  SpellChecker.Algorithm:=haSoundEx;
end;

procedure TSpellCheckForm.FormDestroy(Sender: TObject);
begin
  if SpellChecker.Modified then SpellChecker.SaveUserDictionary;
end;

procedure TSpellCheckForm.FormShow(Sender: TObject);
var
  Language : string;
begin
  Language:=Setup.GetString('Dictionary','');
  if Language='' then Language:=Setup.GetString('Language','');
  LanguageBox.ItemIndex:=Max(0,LanguageBox.Items.IndexOf(Language));
  SkipSymbolsBox.Checked:=Setup.GetBoolean('SkipSymbols',True);
  ActiveLayerOnlyBox.Checked:=Setup.GetBoolean('ActiveLayerOnly',False);

  WordEdit.Text:='';
  StartButton.Caption:=rsStart;
  Modified:=False;
  ActiveControl:=StartButton;
end;

procedure TSpellCheckForm.CloseButtonClick(Sender: TObject);
begin
  ContinueAction:=caStop;
  if Modified then ModalResult:=mrOK
  else ModalResult:=mrCancel;
end;

procedure TSpellCheckForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Setup.WriteString('Dictionary',LanguageBox.Text);
  Setup.WriteBoolean('SkipSymbols',SkipSymbolsBox.Checked);
  Setup.WriteBoolean('ActiveLayerOnly',ActiveLayerOnlyBox.Checked);
end;

procedure TSpellCheckForm.LoadDictionary;
begin
  if SpellChecker.Dictionary<>LanguageBox.Text then
  begin
    if SpellChecker.Modified then SpellChecker.SaveUserDictionary;
    SpellChecker.LoadDictionary(LanguageBox.Text);
  end;
end;

function IsSymbol(const Word: string): Boolean;
var
  I : Integer;
  Upper : string;
begin
  // Only one letter
  Result:=Length(Word)<=1;
  if Result then Exit;
  // Word with number
  for I:=1 to Length(Word) do if Word[I] in ['0'..'9'] then
  begin
    Result:=True;
    Exit;
  end;
  // Has upper case letters
  Upper:=AnsiUpperCase(Word);
  for I:=2 to Length(Word) do if Word[I]=Upper[I] then
  begin
    Result:=True;
    Exit;
  end;
end;

function IsNumber(const Word: string): Boolean;
var
  I : Integer;
begin
  Result:=True;
  for I:=1 to Length(Word) do if not (Word[I] in ['0'..'9']) then
  begin
    Result:=False;
    Break;
  end;
end;

procedure TSpellCheckForm.StartButtonClick(Sender: TObject);

  procedure CheckObjectList(List: TBaseObjectList);
  var
    I, P : Integer;
    Text, Word : string;

    procedure CheckWord(Obj: TTextObject);
    begin
      if Word<>'' then
      begin
        if not IsNumber(Word) and
           not (SkipSymbolsBox.Checked and IsSymbol(Word)) and
           not SpellChecker.IsDictWord(Word) and
           not SpellChecker.IsSkipWord(Word) then
        begin
          LookUpLabel.Caption:=rsNotInDictionary;
          StartButton.Caption:=SIgnoreButton;
          WordEdit.Text:=Word;
          IgnoreAllButton.Enabled:=True;
          StopButton.Enabled:=True;
          AddButton.Enabled:=True;
          ReplaceButton.Enabled:=False;
          ObjectLabel.Caption:=Obj.Name;
          TextLabel.Caption:=Text;
          SpellChecker.GetSuggestions(Word,ListBox.Items);
          ContinueAction:=caDont;
          Screen.Cursor:=crDefault;
          repeat
            Application.ProcessMessages;
            Sleep(40);
          until ContinueAction<>caDont;
          Screen.Cursor:=crHourGlass;

          case ContinueAction of
            caStop    : Abort;
            caReplace : begin
                          Text:=StuffString(Text,P-Length(Word),Length(Word),WordEdit.Text);
                          Inc(P,Length(WordEdit.Text)-Length(Word));
                          Obj.SetTextAndName(Text);
                          MainForm.UpdateDrawing;
                          Modified:=True; 
                        end;
          end;
        end;
        Word:='';
      end;
    end;

  begin
    Update;
    for I:=0 to List.Count-1 do
      if List.Objects^[I] is TGroupObject then CheckObjectList(TGroupObject(List.Objects^[I]).Group)
      else if List.Objects^[I] is TTextObject then 
      begin
        Text:=PString(List.Objects^[I].Properties[opText])^;
        Word:='';
        P:=1;
        while P<=Length(Text) do
        begin
          if Text[P]='\' then
          begin
            CheckWord(TTextObject(List.Objects^[I]));
            Inc(P);
            case Text[P] of
              '0'..'9' : while Text[P+1] in ['0'..'9'] do Inc(P); // Text size
              '"'      : repeat // Font name
                           Inc(P);
                         until Text[P] in [#0,'"'];
              'S'      : begin // Symbol
                           P:=PosEx('\s',Text,P+1);
                           if P=0 then Break
                           else Inc(P);
                         end;
            end;
          end
          else if Text[P] in [#0..#47,#58..#64,#91..#96,#123..#137,#139,#145..#153,#155,#163..#191,#215,#247] then
            CheckWord(TTextObject(List.Objects^[I]))
          else Word:=Word+Text[P];
          Inc(P);
        end;
        CheckWord(TTextObject(List.Objects^[I]));
      end;
  end;

var
  I, P : Integer;
  LookUp : string;
begin
  if CheckInProgress then ContinueAction:=caIgnore
  else
  begin
    Screen.Cursor:=crHourGlass;
    try
      LookUp:=LookUpLabel.Caption;
      LanguageBox.Enabled:=False;
      ActiveLayerOnlyBox.Enabled:=False;
      Update;
      LoadDictionary;
      CheckInProgress:=True;
      try
        if ActiveLayerOnlyBox.Checked then // Active layer only
          CheckObjectList(MainForm.GetActiveLayer)

        else with MainForm do // Complete diagram
        begin
          CheckObjectList(Diagram.Stencil);
          for P:=0 to Diagram.Count-1 do
          begin
            ActivePageIndex:=P;
            Update;
            with Diagram.Pages[P] do
              for I:=0 to Count-1 do
                CheckObjectList(Layers[I]);
          end;
        end;

        MessageDlg(rsSpellCheckComplete,mtInformation);
      finally
        CheckInProgress:=False;
        ObjectLabel.Caption:='';
        TextLabel.Caption:='';
        LookUpLabel.Caption:=LookUp;
        StartButton.Caption:=rsStart;
        ReplaceButton.Enabled:=False;
        IgnoreAllButton.Enabled:=False;
        AddButton.Enabled:=False;
        StopButton.Enabled:=False;
        LanguageBox.Enabled:=True;
        ActiveLayerOnlyBox.Enabled:=True;
      end;
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TSpellCheckForm.IgnoreAllButtonClick(Sender: TObject);
begin
  SpellChecker.AddSkipWord(WordEdit.Text);
  ContinueAction:=caIgnore;
end;

procedure TSpellCheckForm.WordEditChange(Sender: TObject);
begin
  IgnoreAllButton.Enabled:=False;
  if CheckInProgress then
  begin
    ReplaceButton.Enabled:=True;
    AddButton.Enabled:=False;
  end
  else
  begin
    Screen.Cursor:=crAppStart;
    try
      LoadDictionary;
      SpellChecker.GetSuggestions(WordEdit.Text,ListBox.Items);
      ListBox.ItemIndex:=ListBox.Items.IndexOf(WordEdit.Text);
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TSpellCheckForm.ReplaceButtonClick(Sender: TObject);
begin
  if ReplaceButton.Enabled then ContinueAction:=caReplace;
end;

procedure TSpellCheckForm.AddButtonClick(Sender: TObject);
begin
  SpellChecker.AddDictWord(WordEdit.Text);
  ContinueAction:=caIgnore;
end;

procedure TSpellCheckForm.ListBoxClick(Sender: TObject);
begin
  if ListBox.ItemIndex>=0 then WordEdit.Text:=ListBox.Items[ListBox.ItemIndex];
end;

procedure TSpellCheckForm.StopButtonClick(Sender: TObject);
begin
  ContinueAction:=caStop;
end;

procedure TSpellCheckForm.LanguageBoxChange(Sender: TObject);
begin
  if WordEdit.Text<>'' then WordEditChange(nil);
end;

end.

