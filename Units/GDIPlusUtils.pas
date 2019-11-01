unit GDIPlusUtils;

interface

uses
  SysUtils, Graphics, GdiPlus, LinarBitmap;

procedure GDIplusBitmapToLinearBitmap(Source: IBitmap; Dest: TLinearBitmap);

implementation

procedure GDIplusBitmapToLinearBitmap(Source: IBitmap; Dest: TLinearBitmap);
const
  PalSize = SizeOf(TColorPalette)+255*4;
var
  BmData : BitmapData;
  Y : Integer;
  Pal : PColorPalette;
begin
  if (Source.PixelFormat=PixelFormat8bppIndexed) and (Source.GetPaletteSize=PalSize) then
  begin
    Dest.New(Source.Width,Source.Height,pf8bit);
    Source.LockBits(NewRect(Dest.Width,Dest.Height),0,PixelFormat8bppIndexed,BmData);
    try
      for Y:=0 to Dest.Height-1 do
        Move(PByteArray(BmData.Scan0)^[Y*BmData.Strid],Dest.ScanLine[Y]^,Dest.BytesPerLine);
    finally
      Source.UnlockBits(BmData);
    end;
    GetMem(Pal,PalSize);
    try
      if Source.GetPalette(Pal,PalSize)=Ok then
        for Y:=0 to 255 do Dest.Palette^[Y]:=GetRGBRec(Pal^.Entries[Y])
      else
        Dest.Palette^:=GrayPal;
    finally
      FreeMem(Pal);
    end;
  end
  else
  begin
    Dest.New(Source.Width,Source.Height,pf24bit);
    Source.LockBits(NewRect(Dest.Width,Dest.Height),0,PixelFormat24bppRGB,BmData);
    try
      for Y:=0 to Dest.Height-1 do
        Move(PByteArray(BmData.Scan0)^[Y*BmData.Strid],Dest.ScanLine[Y]^,Dest.BytesPerLine);
    finally
      Source.UnlockBits(BmData);
    end;
  end;
end;

end.
