unit DiagramExport;

interface

uses
  Classes, SysUtils, DialogsEx, DiagramBase, StdCtrls, ExtCtrls, Graphics, DesignerSetup, ValueEdits;

resourcestring
  rsExportFileDPI = 'Export file DPI:';
  rsDisableFontAntialiasing = 'Disable font antialiasing';
  rsExportAllPages = 'Export all pages';
  rsAutoCrop = 'Crop to page contents';

type
  TExportDiagramDialog = class(TSaveDialogButtons)
  protected
    AllPagesBox : TCheckBox;
    DisableAntialiasingBox : TCheckBox;
    AutoCropBox : TCheckBox;
    ResolutionEdit : TFloatEdit;
    procedure AddControls(var NextTop: Integer); override;
  public
    procedure Execute(const Path: string; Diagram: TDiagramContainer; ActivePageIndex: Integer; ADefaultFont: TFont; const Options: TDesignerSetup); reintroduce;
  end;

implementation

uses
  Math, Types, Controls, Forms, FastBitmap, LinarBitmap, MemUtils, MathUtils,
  DiagramAntialiasingDrawing, StyleForm, FileUtils, Dialogs, StringUtils,
  Settings;

procedure TExportDiagramDialog.AddControls(var NextTop: Integer);
begin
  inherited;

  ResolutionEdit:=TFloatEdit.Create(FPanel);
  with ResolutionEdit do
  begin
    Parent:=FPanel;
    Left:=4;
    Top:=NextTop;
    Value:=Setup.GetInteger('ExportDPI',Screen.PixelsPerInch);
    Min:=64;
    Max:=2400;
    Inc(NextTop,Height+16);
  end;

  DisableAntialiasingBox:=TCheckBox.Create(FPanel);
  with DisableAntialiasingBox do
  begin
    Parent:=FPanel;
    Left:=4;
    Top:=NextTop;
    Width:=FPanel.Width-4;
    Caption:=rsDisableFontAntialiasing;
    Inc(NextTop,Height+8);
  end;

  AutoCropBox:=TCheckBox.Create(FPanel);
  with AutoCropBox do
  begin
    Parent:=FPanel;
    Left:=4;
    Top:=NextTop;
    Width:=FPanel.Width-4;
    Caption:=rsAutoCrop;
    Inc(NextTop,Height+8);
  end;

  AllPagesBox:=TCheckBox.Create(FPanel);
  with AllPagesBox do
  begin
    Parent:=FPanel;
    Left:=4;
    Top:=NextTop;
    Width:=FPanel.Width-4;
    Caption:=rsExportAllPages;
    Inc(NextTop,Height+8);
  end;
end;

procedure TExportDiagramDialog.Execute(const Path: string; Diagram: TDiagramContainer; ActivePageIndex: Integer; ADefaultFont: TFont; const Options: TDesignerSetup);
var
  ExportName, ExportType : string;
  ExportCanvasInfo : TCanvasInfo;
  Canvas : TCanvas;
  Bitmap : TFastBitmap;
  Metafile : TMetafile;
  Image : TLinearBitmap;
  MinPage, MaxPage, Layer : Integer;
  LayerRect, CropRect : TRect;
begin
  Filter:=BitmapLoaders.GetSaveFilter;
  SetupSaveDialogFilter(Self,Path+'.emf');
  ButtonCaptions:='@'+rsExportFileDPI;
  FilterIndex:=Setup.GetInteger('ExportFilterIndex',FilterIndex);
  if inherited Execute then
  try
    Screen.Cursor:=crHourGlass;
    Setup.WriteInteger('ExportFilterIndex',FilterIndex);
    Setup.WriteInteger('ExportDPI',Round(ResolutionEdit.Value));
    ZeroMem(ExportCanvasInfo,SizeOf(TCanvasInfo));
    with ExportCanvasInfo do
    begin
      Scale:=FloatPoint(ResolutionEdit.Value/DesignerDPI,ResolutionEdit.Value/DesignerDPI);
      DrawMode:=dmRender;
      DefaultFont:=ADefaultFont;
      Container:=Diagram;
    end;
    ExportType:=ExtractFileExtNoDotUpper(FileName);

    if AllPagesBox.Checked then
    begin
      MinPage:=0;
      MaxPage:=Diagram.Count-1;
    end
    else
    begin
      MinPage:=ActivePageIndex;
      MaxPage:=ActivePageIndex;
    end;
    for ActivePageIndex:=MinPage to MaxPage do
    begin
      if AutoCropBox.Checked then // Auto crop, determine bounds of page contents
      begin
        CropRect:=Diagram.Stencil.GetBounds;
        for Layer:=0 to Diagram.Pages[ActivePageIndex].Count-1 do
        begin
          LayerRect:=Diagram.Pages[ActivePageIndex].Layers[Layer].GetBounds;
          if IsZeroRect(CropRect) then CropRect:=LayerRect
          else if not IsZeroRect(LayerRect) then IntersectRect(CropRect,CropRect,LayerRect);
        end;
        Dec(CropRect.Left,Ceil(1/ExportCanvasInfo.Scale.X));
        Dec(CropRect.Top,Ceil(1/ExportCanvasInfo.Scale.Y));
        Inc(CropRect.Right,Ceil(1/ExportCanvasInfo.Scale.X));
        Inc(CropRect.Bottom,Ceil(1/ExportCanvasInfo.Scale.Y));
        if Diagram.ObjectShadows then
        begin
          Inc(CropRect.Right,Round(ShadowOffset));
          Inc(CropRect.Bottom,Round(ShadowOffset));
        end;
        ExportCanvasInfo.Offset:=RoundPoint(-CropRect.Left*ExportCanvasInfo.Scale.X,-CropRect.Top*ExportCanvasInfo.Scale.Y);
      end
      else CropRect:=Rect(0,0,Diagram.Pages[ActivePageIndex].Width,Diagram.Pages[ActivePageIndex].Height);
      ExportCanvasInfo.PageIndex:=ActivePageIndex;

      if MinPage<MaxPage then ExportName:=RemoveFileExt(FileName)+IntToStrLeadZero(ActivePageIndex+1,4)+ExtractFileExt(FileName)
      else ExportName:=FileName;
      if (ExportType='EMF') or (ExportType='WMF') then
      begin
        ExportCanvasInfo.DisableFontSmoothing:=True;
        Metafile:=TMetafile.Create;
        try
          Metafile.Enhanced:=ExportType='EMF';
          Metafile.Width:=Round((CropRect.Right-CropRect.Left)*ExportCanvasInfo.Scale.X);
          Metafile.Height:=Round((CropRect.Bottom-CropRect.Top)*ExportCanvasInfo.Scale.Y);
          Canvas:=TMetafileCanvas.Create(Metafile,0);
          try
            if Diagram.ObjectShadows then
            begin
              ExportCanvasInfo.DrawMode:=dmShadow;
              Diagram.Pages[ActivePageIndex].DrawShadow(Canvas,ExportCanvasInfo);
              ExportCanvasInfo.DrawMode:=dmRender;
            end;
            Diagram.Pages[ActivePageIndex].Draw(Canvas,ExportCanvasInfo);
          finally
            Canvas.Free;
          end;
          Metafile.SaveToFile(ExportName);
        finally
          Metafile.Free;
        end;
      end
      else
      begin
        ExportCanvasInfo.DisableFontSmoothing:=DisableAntialiasingBox.Checked;
        Bitmap:=TFastBitmap.Create(Round((CropRect.Right-CropRect.Left)*ExportCanvasInfo.Scale.X),
                                   Round((CropRect.Bottom-CropRect.Top)*ExportCanvasInfo.Scale.Y),
                                   pf32bit);
        try
          DrawToBitmap(Diagram.Pages[ActivePageIndex],Bitmap,ExportCanvasInfo,Options.Antialiasing and not DisableAntialiasingBox.Checked);
          Bitmap.PixelFormat:=pf24bit;
          Image:=TLinearBitmap.Create(Bitmap);
          try
            FreeAndNil(Bitmap);
            Image.SaveToFile(ExportName);
          finally
            Image.Free;
          end;
        finally
          Bitmap.Free;
        end;
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

end.
