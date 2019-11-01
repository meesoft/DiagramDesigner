object SlideShowForm: TSlideShowForm
  Left = 2013
  Top = 324
  BorderStyle = bsNone
  Caption = 'Slide show'
  ClientHeight = 478
  ClientWidth = 692
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu
  Position = poDefault
  WindowState = wsMaximized
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SlidePanel: TPanelFrame
    Left = 0
    Top = 0
    Width = 281
    Height = 245
    OnPaint = SlidePanelPaint
    UseDockManager = False
    TabOrder = 0
    OnMouseDown = FormMouseDown
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 8
    Top = 8
    object NextSlideItem: TMenuItem
      Caption = 'Next'
      ShortCut = 34
      OnClick = NextSlideItemClick
    end
    object PreviousSlideItem: TMenuItem
      Caption = 'Previous'
      ShortCut = 33
      OnClick = PreviousSlideItemClick
    end
    object LastSlideItem: TMenuItem
      Caption = 'Last'
      ShortCut = 35
      OnClick = LastSlideItemClick
    end
    object FirstSlideItem: TMenuItem
      Caption = 'First'
      ShortCut = 36
      OnClick = FirstSlideItemClick
    end
    object GoToPageItem: TMenuItem
      Caption = 'Go to page'
      ShortCut = 71
      OnClick = GoToPageItemClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CloseItem: TMenuItem
      Caption = 'Close'
      ShortCut = 27
      OnClick = CloseItemClick
    end
  end
  object RenderTimer: TTimer
    Interval = 2000
    OnTimer = RenderTimerTimer
    Left = 40
    Top = 8
  end
end
