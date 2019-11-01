library DiagramReader;

uses
  FastMM4,
  Windows,
  SysUtils,
  Math,
  Types,
  Graphics,
  MemUtils,
  MathUtils,
  AnalyzerPlugins in '..\UNITS\AnalyzerPlugins.pas',
  DiagramBase in 'DiagramBase.pas',
  LineObject in 'LineObject.pas',
  PictureObject in 'PictureObject.pas',
  ShapeObject in 'ShapeObject.pas',
  GroupObject in 'GroupObject.pas',
  FormsDummy in '..\UNITS\FormsDummy.pas',
  TemplateObjects in 'TemplateObjects.pas';

{$R *.RES}

function LibGetLoadFilter: PChar; stdcall;
begin
  Result:=PChar(rsDiagramFileFilter+'|'+rsTemplatePaletteFilter);
end;

function LibLoadScaleImage(FileName,FileType: PChar; var Image: TImageContainer): Integer; stdcall;
var
  Design : TDiagramContainer;
  Y, Scale : Integer;
  Bitmap, ScaleBitmap : TBitmap;
  CanvasInfo : TCanvasInfo;
  Templates : TTemplateSheet;
  Background : TRectangleObject;
  AutoSize : Boolean;
  BackgroundColor : TColor;
  Size : TPoint;
begin
  AutoSize:=(Image.Width<0) or (Image.Height<0);
  if not AutoSize then Size:=Point(Image.Width,Image.Height);

  Result:=0;
  Design:=TDiagramContainer.Create;
  try
    try
      // Load design
      if FileType='DDT' then
      begin
        Templates:=TTemplateSheet.Create;
        try
          Templates.LoadFromFile(FileName);
          Design.Add(TDiagramPage.Create);
          Design.Pages[0].Add(TDiagramLayer.Create);
          Templates.CopyToPage(Design.Pages[0]);
        finally
          Templates.Free;
        end;
        BackgroundColor:=clBtnFace;
      end
      else
      begin
        Design.LoadFromFile(FileName);
        if AutoSize then BackgroundColor:=clWhite
        else BackgroundColor:=$faffff;
      end;
      // Create background rectangle
      Background:=TRectangleObject.CreateNew;
      Background.Properties[opFillColor]:=BackgroundColor;
      Background.Properties[opLineColor]:=BackgroundColor;
      if Design.Count>0 then Background.Position:=Rect(0,0,Design.Pages[0].Width,Design.Pages[0].Height)
      else Result:=3;
      Design.Stencil.Insert(0,Background);
    except
      if FileExists(FileName) then Result:=3
      else Result:=2;
    end;
    ZeroMem(Image,SizeOf(Image));
    if Result=0 then
    begin
      ZeroMem(CanvasInfo,SizeOf(CanvasInfo));
      CanvasInfo.DrawMode:=dmPreview;
      CanvasInfo.DefaultFont:=TFont.Create;
      try
        if Design.DefaultFontName='' then
        begin
          CanvasInfo.DefaultFont.Name:='Arial';
          CanvasInfo.DefaultFont.Size:=10;
          CanvasInfo.DefaultFont.Style:=[];
        end
        else
        begin
          CanvasInfo.DefaultFont.Name:=Design.DefaultFontName;
          CanvasInfo.DefaultFont.Size:=Design.DefaultFontSize;
          CanvasInfo.DefaultFont.Style:=TFontStyles(Byte(Design.DefaultFontStyle));
          CanvasInfo.DefaultFont.Charset:=Design.DefaultFontCharSet;
        end;
        CanvasInfo.Container:=Design;
        Scale:=1;
        if AutoSize then // Scale using screen DPI
        begin
          CanvasInfo.Scale:=FloatPoint(GetDeviceCaps(GetDC(0),LOGPIXELSX)/DesignerDPI,GetDeviceCaps(GetDC(0),LOGPIXELSY)/DesignerDPI);
          Image.PixelFormat:=$03;
        end
        else // Use specified size
        begin
          if Size.X<40 then Scale:=4
          else if Size.X<200 then Scale:=2; // Do antialiasing
          CanvasInfo.Scale.X:=Scale*Min(Size.X/Design.Pages[0].Width,Size.Y/Design.Pages[0].Height);
          CanvasInfo.Scale.Y:=CanvasInfo.Scale.X;
          Image.PixelFormat:=$04;
        end;
        Image.Width:=Ceil(Design.Pages[0].Width*CanvasInfo.Scale.X);
        Image.Height:=Ceil(Design.Pages[0].Height*CanvasInfo.Scale.Y);
        if (Image.Width=0) or (Image.Height=0) then Result:=3
        else
        begin
          Bitmap:=TBitmap.Create;
          try
            Bitmap.PixelFormat:=pf24bit;
            Bitmap.Width:=Image.Width;
            Bitmap.Height:=Image.Height;
            Design.Pages[0].Draw(Bitmap.Canvas,CanvasInfo);
            if not AutoSize then
            begin
              Image.Width:=Size.X;
              Image.Height:=Size.X;
              ScaleBitmap:=TBitmap.Create;
              ScaleBitmap.PixelFormat:=pf32bit;
              ScaleBitmap.Width:=Image.Width;
              ScaleBitmap.Height:=Image.Height;
              if Scale>1 then SetStretchBltMode(ScaleBitmap.Canvas.Handle,STRETCH_HALFTONE);
              StretchBlt(ScaleBitmap.Canvas.Handle,
                         (ScaleBitmap.Width-Bitmap.Width div Scale) div 2,
                         (ScaleBitmap.Height-Bitmap.Height div Scale) div 2,
                         Bitmap.Width div Scale,Bitmap.Height div Scale,
                         Bitmap.Canvas.Handle,0,0,Bitmap.Width,Bitmap.Height,
                         SRCCOPY);
              Bitmap.Free;
              Bitmap:=ScaleBitmap;
            end;
            Image.BytesPerLine:=Image.Width*Image.PixelFormat;
            GetMem(Image.Map,Image.Height*Image.BytesPerLine);
            for Y:=0 to Image.Height-1 do Move(Bitmap.ScanLine[Y]^,Image.Map^[Y*Image.BytesPerLine],Image.BytesPerLine);
          finally
            Bitmap.Free;
          end;
        end;
      finally
        CanvasInfo.DefaultFont.Free;
      end;
    end;
  finally
    Design.Free;
  end;
end;

function LibLoadImage(FileName,FileType: PChar; var Image: TImageContainer): Integer; stdcall;
begin
  Image.Width:=-1;
  Image.Height:=-1;
  result:=LibLoadScaleImage(FileName,FileType,Image);
end;

function LibFreeImage(Image: PImageContainer): Integer; stdcall;
begin
  if Assigned(Image) then
  begin
    if Assigned(Image.Map) then FreeMem(Image.Map);
    if Assigned(Image.Palette) then FreeMem(Image.Palette);
    Result:=0;
  end
  else Result:=1;
end;

exports
  LibGetLoadFilter name '?LibGetLoadFilter@@YGPADXZ',
  LibGetLoadFilter name '?LibGetSaveFilter@@YGPADXZ', // To compensate for bug in old ImageDLLDoader
  LibLoadImage name '?LibLoadImage@@YGHPAD0PAUTImageContainer@@@Z',
  LibLoadScaleImage,
  LibFreeImage name '?LibFreeImage@@YGHPAUTImageContainer@@@Z';
end.

