object PagePropertiesForm: TPagePropertiesForm
  Left = 1140
  Top = 242
  BorderStyle = bsDialog
  Caption = 'Page properties'
  ClientHeight = 183
  ClientWidth = 297
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 44
    Width = 31
    Height = 13
    Caption = 'Width:'
  end
  object Label2: TLabel
    Left = 12
    Top = 72
    Width = 34
    Height = 13
    Caption = 'Height:'
  end
  object UnitLabel1: TLabel
    Left = 136
    Top = 44
    Width = 51
    Height = 13
    Caption = 'UnitLabel1'
  end
  object UnitLabel2: TLabel
    Left = 136
    Top = 72
    Width = 51
    Height = 13
    Caption = 'UnitLabel2'
  end
  object Label3: TLabel
    Left = 12
    Top = 12
    Width = 23
    Height = 13
    Caption = 'Title:'
  end
  object OKBtn: TButton
    Left = 213
    Top = 6
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object CancelBtn: TButton
    Left = 213
    Top = 38
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object WidthEdit: TFloatEdit
    Left = 72
    Top = 40
    Width = 61
    Height = 21
    Alignment = taRightJustify
    OnChangeValue = ChangeValue
    TabOrder = 1
    Max = 1E100
    Min = 0.000000000100000000
    Value = 0.000000000100000000
    FormatString = '0.##'
  end
  object HeightEdit: TFloatEdit
    Left = 72
    Top = 68
    Width = 61
    Height = 21
    Alignment = taRightJustify
    OnChangeValue = ChangeValue
    TabOrder = 2
    Max = 1E100
    Min = 0.000000000100000000
    Value = 0.000000000100000000
    FormatString = '0.##'
  end
  object PrinterButton: TButton
    Left = 8
    Top = 126
    Width = 181
    Height = 25
    Caption = '&Get printer page format'
    TabOrder = 4
    OnClick = PrinterButtonClick
  end
  object FlipButton: TButton
    Left = 9
    Top = 98
    Width = 180
    Height = 25
    Caption = '&Flip'
    TabOrder = 3
    OnClick = FlipButtonClick
  end
  object ApplyToAllBox: TCheckBox
    Left = 12
    Top = 160
    Width = 277
    Height = 17
    Caption = '&Apply to all pages'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object NameEdit: TEdit
    Left = 72
    Top = 8
    Width = 117
    Height = 21
    TabOrder = 0
    Text = 'NameEdit'
    OnChange = ChangeValue
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 260
    Top = 68
  end
end
