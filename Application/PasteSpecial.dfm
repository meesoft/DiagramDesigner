object PasteSpecialForm: TPasteSpecialForm
  Left = 401
  Top = 204
  BorderStyle = bsDialog
  Caption = 'Paste special'
  ClientHeight = 164
  ClientWidth = 342
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    342
    164)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 239
    Height = 146
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 111
    Height = 13
    Caption = 'Select clipboard format:'
    Transparent = True
  end
  object OKBtn: TButton
    Left = 258
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 258
    Top = 38
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ListBox: TListBox
    Left = 20
    Top = 40
    Width = 215
    Height = 104
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBoxDblClick
  end
end
