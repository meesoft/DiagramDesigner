object IconSelectionForm: TIconSelectionForm
  Left = 388
  Top = 522
  BorderStyle = bsDialog
  Caption = 'Select icon to import from file'
  ClientHeight = 185
  ClientWidth = 296
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    296
    185)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 152
    Width = 284
    Height = 29
    Anchors = [akLeft, akTop, akBottom]
    AutoSize = False
    Caption = 'Note that writing of files with multiple icons is not supported.'
    Transparent = True
    WordWrap = True
  end
  object OkButton: TButton
    Left = 213
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 213
    Top = 44
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object SelectBox: TComboBox
    Left = 8
    Top = 8
    Width = 193
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = SelectBoxChange
  end
  object PanelFrame: TDoubleBufferedPanel
    Left = 8
    Top = 40
    Width = 193
    Height = 73
    BevelOuter = bvLowered
    UseDockManager = False
    TabOrder = 3
  end
  object TransparentButton: TButton
    Left = 8
    Top = 120
    Width = 113
    Height = 25
    Caption = 'Transparent color'
    TabOrder = 4
    OnClick = TransparentButtonClick
  end
  object ColorPanel: TPanel
    Left = 128
    Top = 120
    Width = 73
    Height = 25
    BevelOuter = bvLowered
    TabOrder = 5
    OnClick = TransparentButtonClick
  end
end
