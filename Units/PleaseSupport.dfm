object PleaseSupportForm: TPleaseSupportForm
  Left = 445
  Top = 301
  BorderStyle = bsDialog
  Caption = 'Please support %s'
  ClientHeight = 281
  ClientWidth = 391
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    391
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 373
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'If you like this software then please support my work and the fu' +
      'ture development of the program, the documentation and the websi' +
      'te.'#13'Without your support keeping the website running is only an ' +
      'expense for me.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 112
    Width = 108
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = 'What can I do?'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 132
    Width = 252
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = #183' The easiest is to make a donation'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 220
    Width = 154
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Click here to see how'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label5: TLabel
    Left = 16
    Top = 152
    Width = 248
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = #183' Help improve the documentation'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 16
    Top = 172
    Width = 298
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = #183' Translate the program to your language'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 16
    Top = 192
    Width = 124
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = #183' Develop plugins'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object CloseButton: TButton
    Left = 308
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Continue'
    Default = True
    ModalResult = 2
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 104
    Top = 252
    Width = 201
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Show this message on startup'
    TabOrder = 1
  end
end
