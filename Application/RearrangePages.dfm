object RearrangePagesForm: TRearrangePagesForm
  Left = 570
  Top = 254
  BorderStyle = bsDialog
  Caption = 'Rearrange pages'
  ClientHeight = 251
  ClientWidth = 298
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = PageListBoxKeyDown
  OnShow = UpdateControlStates
  DesignSize = (
    298
    251)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 213
    Top = 222
    Width = 79
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnKeyDown = PageListBoxKeyDown
  end
  object UpButton: TButton
    Left = 213
    Top = 8
    Width = 79
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '5'
    Font.Charset = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Webdings'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = MoveButtonClick
    OnKeyDown = PageListBoxKeyDown
  end
  object DownButton: TButton
    Left = 213
    Top = 36
    Width = 79
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '6'
    Font.Charset = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Webdings'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = MoveButtonClick
    OnKeyDown = PageListBoxKeyDown
  end
  object DeleteButton: TButton
    Left = 213
    Top = 72
    Width = 79
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Delete'
    TabOrder = 3
    OnClick = DeleteButtonClick
    OnKeyDown = PageListBoxKeyDown
  end
  object RenameButton: TButton
    Left = 213
    Top = 100
    Width = 79
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Rename'
    TabOrder = 4
    OnClick = RenameButtonClick
    OnKeyDown = PageListBoxKeyDown
  end
  object PageListBox: TListBox
    Left = 8
    Top = 8
    Width = 198
    Height = 237
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = UpdateControlStates
    OnDblClick = PageListBoxDblClick
    OnKeyDown = PageListBoxKeyDown
  end
end
