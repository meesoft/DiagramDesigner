unit PrintDiagram;

interface

uses
  Windows, Controls, Printers, Classes, SysUtils, Dialogs, Forms, Graphics, Math,
  DesignerSetup, DiagramBase;

resourcestring
  rsPrintScalingSetToD = 'Print scaling set to %d%%';

type
  TDiagramPrinter = class
  private
    PrintDialog : TPrintDialog;
  public
    Options : TDesignerSetup;
    Diagram : TDiagramContainer;
    ActivePage : TDiagramPage;
    FileName: string;
    DrawPanelFont : TFont;
    procedure Print;
    destructor Destroy; override;
  end;

implementation

uses
  Types, StyleForm, FileUtils, MemUtils, MathUtils, ExtCtrls;

var DisableAutoPrinterOrientation : Boolean = False;

procedure PrintBitmap(Bitmap: TBitmap; const DestPos: TPoint);
var
  Info: PBitmapInfo;
  InfoSize: DWORD;
  Image: Pointer;
  ImageSize: DWORD;
  BitmapHandle : HBITMAP;
  DIBWidth, DIBHeight: LongInt;
begin
  BitmapHandle := Bitmap.Handle;
  GetDIBSizes(BitmapHandle, InfoSize, ImageSize);
  Info := AllocMem(InfoSize);
  try
    Image := HeapAlloc(GetProcessHeap,0,ImageSize);
    try
      GetDIB(BitmapHandle, 0, Info^, Image^);
      with Info^.bmiHeader do
      begin
        DIBWidth := biWidth;
        DIBHeight := biHeight;
      end;
      StretchDIBits(Printer.Canvas.Handle,
                    DestPos.X, DestPos.Y, DIBWidth, DIBHeight,
                    0, 0, DIBWidth, DIBHeight,
                    Image, Info^, DIB_RGB_COLORS, SRCCOPY);
    finally
      HeapFree(GetProcessHeap,0,Image);
    end;
  finally
    FreeMem(Info);
  end;
end;

destructor TDiagramPrinter.Destroy;
begin
  PrintDialog.Free;
end;

procedure TDiagramPrinter.Print;
var
  PrinterCanvasInfo : TCanvasInfo;
  DC : THandle;
  I, Y, SegmentHeight : Integer;
  PageSize : TPoint;
  First, DoPrint : Boolean;
  AutoOrientation : TPrinterOrientation;
  Bitmap : TBitmap;
begin
  if (Options.PrintScaling<>1) and (MessageDlg(Format(rsPrintScalingSetToD,[Round(100*Options.PrintScaling)]),mtWarning,[mbOK,mbCancel])<>mrOK) then Exit;

  AutoOrientation:=poPortrait;
  if not DisableAutoPrinterOrientation then
  try
    with ActivePage do
    if Height>Width then AutoOrientation:=poPortrait
    else if Width>Height then AutoOrientation:=poLandscape
    else AutoOrientation:=Printer.Orientation;
    Printer.Orientation:=AutoOrientation;
  except
    DisableAutoPrinterOrientation:=True;
  end;
  if PrintDialog=nil then PrintDialog:=TPrintDialog.Create(Application.MainForm);
  PrintDialog.MinPage:=1;
  PrintDialog.MaxPage:=Diagram.Count;
  PrintDialog.Options:=[poPageNums,poWarning];
  SetApplicationHandleForDialog(0,PrintDialog); 
  try
    DoPrint:=PrintDialog.Execute;
  finally
    RestoreApplicationHandle;
  end;
  if DoPrint then
  try
    Screen.Cursor:=crHourGlass;
    Printer.Title:=RemoveFileExt(ExtractFileName(FileName));
    Printer.BeginDoc;
    try
      DC:=Printer.Handle;
      ZeroMem(PrinterCanvasInfo,SizeOf(TCanvasInfo));
      with PrinterCanvasInfo do
      begin
        Scale:=FloatPoint(GetDeviceCaps(DC,LOGPIXELSX)/DesignerDPI*Options.PrintScaling,
                          GetDeviceCaps(DC,LOGPIXELSY)/DesignerDPI*Options.PrintScaling);
        Offset:=Point(-GetDeviceCaps(DC,PHYSICALOFFSETX),-GetDeviceCaps(DC,PHYSICALOFFSETY));
        DefaultFont:=DrawPanelFont;
        Container:=Diagram;
      end;
      First:=True;
      for I:=0 to Diagram.Count-1 do
      begin
        if Options.ReversePrint then PrinterCanvasInfo.PageIndex:=Diagram.Count-1-I
        else PrinterCanvasInfo.PageIndex:=I;
        PrinterCanvasInfo.Container:=Diagram;

        if (PrintDialog.PrintRange=prAllPages) or InRange(PrinterCanvasInfo.PageIndex+1,PrintDialog.FromPage,PrintDialog.ToPage) then
        begin
          if First then First:=False
          else Printer.NewPage;

          if Options.PrintAsBitmap then
          begin
            // Render page to bitmap and print the bitmap
            PrinterCanvasInfo.DisableFontSmoothing:=True;
            PageSize:=Point(GetDeviceCaps(DC,HORZRES),GetDeviceCaps(DC,VERTRES));
            SegmentHeight:=Min(PageSize.Y,20*1024*1024 div (3*PageSize.X));
            Y:=0;
            PrinterCanvasInfo.Offset.Y:=0;
            while Y<PageSize.Y do
            begin
              Bitmap:=TBitmap.Create;
              try
                Bitmap.PixelFormat:=pf24bit;
                Bitmap.Width:=PageSize.X;
                Bitmap.Height:=SegmentHeight;
                if Diagram.ObjectShadows then
                begin
                  PrinterCanvasInfo.DrawMode:=dmShadow;
                  Diagram.Pages[PrinterCanvasInfo.PageIndex].DrawShadow(Bitmap.Canvas,PrinterCanvasInfo);
                end;
                PrinterCanvasInfo.DrawMode:=dmRender;
                Diagram.Pages[PrinterCanvasInfo.PageIndex].Draw(Bitmap.Canvas,PrinterCanvasInfo);

                PrintBitmap(Bitmap,Point(0,Y));
              finally
                Bitmap.Free;
              end;
              Dec(PrinterCanvasInfo.Offset.Y,SegmentHeight);
              Inc(Y,SegmentHeight);
            end;
          end
          else
          begin
            // Render directly to printer canvas
            if Diagram.ObjectShadows then
            begin
              PrinterCanvasInfo.DrawMode:=dmShadow;
              Diagram.Pages[PrinterCanvasInfo.PageIndex].DrawShadow(Printer.Canvas,PrinterCanvasInfo);
            end;
            PrinterCanvasInfo.DrawMode:=dmRender;
            Diagram.Pages[PrinterCanvasInfo.PageIndex].Draw(Printer.Canvas,PrinterCanvasInfo);
          end;
        end;
      end;
    finally
      Printer.EndDoc;
    end;
    try
      if not DisableAutoPrinterOrientation and (AutoOrientation<>Printer.Orientation) then
        DisableAutoPrinterOrientation:=True;
    except
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

end.
