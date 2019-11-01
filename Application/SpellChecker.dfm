object SpellCheckForm: TSpellCheckForm
  Left = 439
  Top = 182
  BorderStyle = bsDialog
  Caption = 'Spell check'
  ClientHeight = 307
  ClientWidth = 353
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    353
    307)
  PixelsPerInch = 96
  TextHeight = 13
  object LookUpLabel: TLabel
    Left = 8
    Top = 8
    Width = 42
    Height = 13
    Caption = '&Look up:'
    FocusControl = WordEdit
  end
  object Label2: TLabel
    Left = 8
    Top = 52
    Width = 34
    Height = 13
    Caption = 'Object:'
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 24
    Height = 13
    Caption = 'Text:'
  end
  object ObjectLabel: TLabel
    Left = 56
    Top = 52
    Width = 191
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
  end
  object TextLabel: TLabel
    Left = 56
    Top = 72
    Width = 191
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 61
    Height = 13
    Caption = 'Suggestions:'
  end
  object Label5: TLabel
    Left = 8
    Top = 263
    Width = 51
    Height = 13
    Caption = 'Language:'
  end
  object Bevel2: TBevel
    Left = 0
    Top = 228
    Width = 353
    Height = 79
    Align = alBottom
    Shape = bsTopLine
  end
  object CloseButton: TButton
    Left = 256
    Top = 191
    Width = 88
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Cancel = True
    Caption = '&Close'
    TabOrder = 5
    OnClick = CloseButtonClick
  end
  object StartButton: TButton
    Left = 256
    Top = 11
    Width = 88
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Default = True
    TabOrder = 0
    OnClick = StartButtonClick
  end
  object IgnoreAllButton: TButton
    Left = 256
    Top = 39
    Width = 88
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Ignore all'
    Enabled = False
    TabOrder = 1
    OnClick = IgnoreAllButtonClick
  end
  object AddButton: TButton
    Left = 256
    Top = 71
    Width = 88
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Add'
    Enabled = False
    TabOrder = 2
    OnClick = AddButtonClick
  end
  object WordEdit: TEdit
    Left = 8
    Top = 24
    Width = 239
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    OnChange = WordEditChange
  end
  object ListBox: TListBox
    Left = 8
    Top = 120
    Width = 239
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 7
    OnClick = ListBoxClick
    OnDblClick = ReplaceButtonClick
  end
  object ReplaceButton: TButton
    Left = 256
    Top = 119
    Width = 88
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Replace'
    Enabled = False
    TabOrder = 3
    OnClick = ReplaceButtonClick
  end
  object LanguageBox: TComboBox
    Left = 8
    Top = 279
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 9
    OnChange = LanguageBoxChange
  end
  object ActiveLayerOnlyBox: TCheckBox
    Left = 8
    Top = 240
    Width = 337
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Check active layer &only'
    TabOrder = 8
  end
  object SkipSymbolsBox: TCheckBox
    Left = 164
    Top = 280
    Width = 181
    Height = 17
    Hint = 'Skip words with numbers, single letters and upper case words'
    Caption = 'Skip symbols'
    TabOrder = 10
  end
  object StopButton: TButton
    Left = 256
    Top = 163
    Width = 88
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 4
    OnClick = StopButtonClick
  end
end
