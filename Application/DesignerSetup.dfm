object DesignerSetupForm: TDesignerSetupForm
  Left = 2480
  Top = 143
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 330
  ClientWidth = 405
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    405
    330)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 288
    Width = 405
    Height = 42
    Align = alBottom
    Shape = bsTopLine
  end
  object Label3: TLabel
    Left = 148
    Top = 59
    Width = 83
    Height = 13
    Caption = 'Undo history size:'
  end
  object Label4: TLabel
    Left = 148
    Top = 35
    Width = 114
    Height = 13
    Caption = 'Clipboard metafile scale:'
  end
  object Label5: TLabel
    Left = 20
    Top = 111
    Width = 27
    Height = 13
    Caption = 'Units:'
  end
  object Label7: TLabel
    Left = 148
    Top = 11
    Width = 60
    Height = 13
    Caption = 'Print scaling:'
  end
  object GridBox: TGroupBox
    Left = 16
    Top = 10
    Width = 121
    Height = 96
    Caption = 'GridBox'
    TabOrder = 2
    object Label1: TLabel
      Left = 12
      Top = 23
      Width = 10
      Height = 13
      Caption = 'X:'
    end
    object Label2: TLabel
      Left = 12
      Top = 45
      Width = 10
      Height = 13
      Caption = 'Y:'
    end
    object Bevel2: TBevel
      Left = 92
      Top = 28
      Width = 15
      Height = 25
      Shape = bsFrame
      Style = bsRaised
    end
    object GridXEdit: TFloatEdit
      Left = 32
      Top = 20
      Width = 65
      Height = 21
      Alignment = taRightJustify
      OnChangeValue = GridXEditChangeValue
      TabOrder = 0
      Max = 1000.000000000000000000
      SpinIncrement = 1.000000000000000000
      FormatString = '0.##'
    end
    object GridYEdit: TFloatEdit
      Left = 32
      Top = 42
      Width = 65
      Height = 21
      Alignment = taRightJustify
      OnChangeValue = GridYEditChangeValue
      TabOrder = 1
      Max = 1000.000000000000000000
      SpinIncrement = 1.000000000000000000
      FormatString = '0.##'
    end
    object ShowGridBox: TCheckBox
      Left = 12
      Top = 71
      Width = 105
      Height = 17
      Caption = 'Show grid'
      TabOrder = 2
    end
    object LockGridBox: TCheckBox
      Left = 100
      Top = 32
      Width = 13
      Height = 17
      TabOrder = 3
      OnClick = GridXEditChangeValue
    end
  end
  object Button1: TButton
    Left = 322
    Top = 300
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Button2: TButton
    Left = 242
    Top = 300
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object UndoHistoryEdit: TIntegerEdit
    Left = 344
    Top = 56
    Width = 53
    Height = 21
    Hint = 'Maximum number of undos/redos'
    Anchors = [akTop, akRight]
    TabOrder = 6
    SpinIncrement = 1
  end
  object ClipboardScaleEdit: TIntegerEdit
    Left = 344
    Top = 32
    Width = 53
    Height = 21
    Hint = 'Size of design when pasting from clipboard to other application'
    Anchors = [akTop, akRight]
    TabOrder = 5
    Max = 16
    Min = 1
    SpinIncrement = 1
    Value = 1
  end
  object UnitsBox: TComboBox
    Left = 16
    Top = 127
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = UnitsBoxChange
    Items.Strings = (
      'Millimeters'
      'Centimeters'
      'Inches'
      'points'
      '300 DPI dots'
      '600 DPI dots')
  end
  object LanguageButton: TBitBtn
    Left = 148
    Top = 122
    Width = 247
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Set program language'
    TabOrder = 8
    OnClick = LanguageButtonClick
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000FF00FFFF00FF
      FF00FFFF00FFB6B2B29992928C84848B8282918A8AADABABFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFDDD8D8A99B9BDBD4D4EBE7E7E3D6D6D9
      C1C1CEBEBEB7A7A7786C6CDFDFDFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      D7C6C6E8DFDEFFFFFFE5DEDFB69899B08788D3BCBCE7DCDCB5A0A0D8D8D8FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDFD2D2C1B6BAC5B0B0AF847F9B
      6760A38988B5ABACE3E0E0FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFD3D0D1A179657D4529732E1E5500005500005619178B8B8BFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFBAB4B6B776486E3210006820540000FF
      2B15FF0000E20000AA00003F3F3FFF00FFFF00FFFF00FFFF00FFFF00FFDCDADB
      BC896D7D6C1B009C31009C31006820A8401FFF6331FE4220FE00000031004F4F
      4FFF00FFFF00FFFF00FFFF00FFB2A0A1D2844B0AA6314CE83163FF3120BD3100
      3410FF762071691A007610006300004200AAAAAAFF00FFFF00FFFF00FFC1A29C
      44902F94E899AAFF9941DE310A8010706C0554750015A526009C310063000058
      00394F39FF00FFFF00FFFF00FFC8A9A2009B3154BC7574DD74006300FFAC10FF
      CE3100630098ED9810AC31006300006300002100FF00FFFF00FFFF00FFC5AAAC
      FF6331C6E99955A725A98810FFB02BFFC82BFF9C0082843D5FB44810AC31006F
      0A002100FF00FFFF00FFFF00FFC8AAAFE1996AC6965354802620AA318C682BFF
      B02BFFBD20FF7C1AC6632610861000590A727C72FF00FFFF00FFFF00FFDDD0D0
      CDAAAB53BB74DDFEDD31CE3120AA31A88710FE9B00FFA930FF6331FF6331AB44
      23ACACACFF00FFFF00FFFF00FFFF00FFCCB4B6A5A58C83CB96DEFFDE65DE652C
      A342007631C57C1BFF6331FF6331C88E8EFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFD7C2C3A0A38E54BC757CD2926FDE6F20BD31547C31FF6331FFCBBBFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFF0EAEAC8B2B484B49000
      9C31009C3155BD75FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
  end
  object DictionaryPathButton: TButton
    Left = 148
    Top = 92
    Width = 247
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Set dictionary path'
    TabOrder = 7
    OnClick = DictionaryPathButtonClick
  end
  object FileFormatAssociationsButton: TButton
    Left = 148
    Top = 152
    Width = 247
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'File format associations'
    TabOrder = 9
    OnClick = FileFormatAssociationsButtonClick
  end
  object PrintScalingEdit: TFloatEdit
    Left = 344
    Top = 8
    Width = 53
    Height = 21
    Hint = 'Size of print relative to design size'
    Anchors = [akTop, akRight]
    TabOrder = 4
    Max = 100.000000000000000000
    Min = 0.010000000000000000
    SpinIncrement = -2.000000000000000000
    Value = 1.000000000000000000
    FormatString = '0.##'
  end
  object AntialiasingBox: TCheckBox
    Left = 16
    Top = 160
    Width = 121
    Height = 17
    Hint = 'Antialiasing for slide show and bitmap export'
    Caption = 'Antialiasing'
    TabOrder = 10
  end
  object ShowMarginsBox: TCheckBox
    Left = 16
    Top = 180
    Width = 125
    Height = 17
    Hint = 'Show printer margins on page in designer'
    Caption = 'Show printer margins'
    TabOrder = 11
  end
  object DefaultGroupAnchorsBox: TCheckBox
    Left = 16
    Top = 220
    Width = 345
    Height = 17
    Hint = 
      'Automatically set anchors when grouping objects so that the grou' +
      'p can be scaled'
    Caption = 'Create default anchors for groups'
    TabOrder = 13
  end
  object DefaultGroupLinksBox: TCheckBox
    Left = 16
    Top = 240
    Width = 345
    Height = 17
    Hint = 'Automatically create link points when grouping objects'
    Caption = 'Create default link points for groups and images'
    TabOrder = 14
  end
  object PrintAsBitmapBox: TCheckBox
    Left = 16
    Top = 200
    Width = 345
    Height = 17
    Hint = 
      'Render all objects to a bit and print the bitmap. This may be ne' +
      'cessary with some printers to handle transparency and gradient f' +
      'ill correctly.'
    Caption = 'Print as bitmap'
    TabOrder = 12
  end
  object AutoConnectToLinksBox: TCheckBox
    Left = 16
    Top = 260
    Width = 345
    Height = 17
    Hint = 
      'Automatically snap connectors to object link points when draggin' +
      'g'
    Caption = 'Auto connect to link points'
    TabOrder = 15
  end
  object EditDefaultLinksButton: TButton
    Left = 304
    Top = 240
    Width = 91
    Height = 17
    Caption = 'Edit defaults'
    TabOrder = 16
    OnClick = EditDefaultLinksButtonClick
  end
end
