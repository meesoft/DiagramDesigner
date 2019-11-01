object TextEditorForm: TTextEditorForm
  Left = 329
  Top = 511
  Width = 461
  Height = 293
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Edit text'
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    445
    254)
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 144
    Top = 8
    Width = 300
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    BiDiMode = bdLeftToRight
    Caption = 'Note that closing tags are not required'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 128
    Height = 13
    Caption = 'Text formatting codes:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LabelCode1: TLabel
    Left = 8
    Top = 26
    Width = 57
    Height = 13
    Cursor = crHandPoint
    Hint = 'Click to apply format to selection'
    Caption = 'LabelCode1'
    Transparent = True
    OnMouseDown = LabelCodeMouseDown
  end
  object LabelDescription1: TLabel
    Left = 100
    Top = 26
    Width = 85
    Height = 13
    Caption = 'LabelDescription1'
    Transparent = True
    OnMouseDown = LabelCodeMouseDown
  end
  object Label2: TLabel
    Left = 8
    Top = 68
    Width = 99
    Height = 13
    Caption = 'Enter object text:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LabelCode2: TLabel
    Left = 208
    Top = 26
    Width = 57
    Height = 13
    Cursor = crHandPoint
    Hint = 'Click to apply format to selection'
    Caption = 'LabelCode2'
    Transparent = True
    OnMouseDown = LabelCodeMouseDown
  end
  object LabelDescription2: TLabel
    Left = 300
    Top = 26
    Width = 85
    Height = 13
    Caption = 'LabelDescription2'
    Transparent = True
    OnMouseDown = LabelCodeMouseDown
  end
  object RichEdit: TRichEdit
    Left = 4
    Top = 86
    Width = 445
    Height = 136
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
    WordWrap = False
    OnChange = RichEditChange
    OnKeyDown = RichEditKeyDown
  end
  object OKButton: TButton
    Left = 293
    Top = 229
    Width = 75
    Height = 25
    Hint = ' '
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 373
    Top = 229
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Preview: TDoubleBufferedPanel
    Left = 4
    Top = 228
    Width = 278
    Height = 33
    RenderThemeBackground = True
    OnPaint = PreviewPaint
    Anchors = [akLeft, akRight, akBottom]
    UseDockManager = False
    TabOrder = 3
  end
  object ClipPanelFrame: TPanelFrame
    Left = 228
    Top = 64
    Width = 100
    Height = 41
    UseDockManager = False
    TabOrder = 4
    Visible = False
  end
  object HighlightSyntaxBox: TCheckBox
    Left = 332
    Top = 66
    Width = 109
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Highlight syntax'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = HighlightSyntaxBoxClick
  end
end
