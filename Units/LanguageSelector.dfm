object LanguageSelectorForm: TLanguageSelectorForm
  Left = 446
  Top = 217
  BorderStyle = bsDialog
  Caption = 'Select program language'
  ClientHeight = 168
  ClientWidth = 317
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    317
    168)
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TListBox
    Left = 8
    Top = 8
    Width = 220
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = OKButtonClick
  end
  object OKButton: TButton
    Left = 235
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 235
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
