/////////////////////////////////////////////////////////////////////////////////////////////////////
//
// ICOLoader.pas - Windows icon and cursor coding/decoding
// -------------------------------------------------------
// Version:   2004-03-13
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
// Last changes:
//   AllwaysLoadFirst default true
//
unit ICOLoader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IconList,
  LinarBitmap, Streams, StdCtrls, ExtCtrls, PanelFrame, MemUtils, Math
{$IFDEF UseStyleForm}, StyleForm; {$ELSE}; type TStyleForm = TForm; {$ENDIF}

type
  TIconSelectionForm = class(TStyleForm)
    OkButton: TButton;
    CancelButton: TButton;
    Label1: TLabel;
    SelectBox: TComboBox;
    PanelFrame: TDoubleBufferedPanel;
    TransparentButton: TButton;
    ColorPanel: TPanel;
    procedure FormShow(Sender: TObject);
    procedure SelectBoxChange(Sender: TObject);
    procedure TransparentButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DialogColor : TColor;
    IconList : TIconList;
  end;

  TICOLoader = class(TBitmapLoader)
     public
       TransparentColor : TColor;
       Mode : (mLoadFirst, mLoadAll, mShowDialog);
       CursorHotSpot    : TPoint;

       constructor Create;

       function CanLoad(const Ext: string): Boolean; override;
       function CanSave(const Ext: string): Boolean; override;

       function GetLoadFilter: string; override;

       procedure LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
       procedure SaveToStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
     end;

const
  MagentaPix24 : RGBRec = (B:255;G:0;R:255);

var
  Default : TICOLoader;

implementation

uses ColorDialog;

{$R *.DFM}

resourcestring
  rsWindowsIcon = 'Windows icon';
  rsWindowsCursor = 'Windows cursor';
  rsNoIconsInFile = 'No icons in file';
  rsImageIsTooLargeForIcon = 'Image is too large for icon';

//==============================================================================================================================
// TICOLoader
//==============================================================================================================================
constructor TICOLoader.Create;
begin
  inherited Create;
  TransparentColor:=$ff00ff; // Magenta
end;

function TICOLoader.CanLoad(const Ext: string): Boolean;
begin
  Result:=(Ext='ICO') or (Ext='CUR');
end;

function TICOLoader.CanSave(const Ext: string): Boolean;
begin
  Result:=(Ext='ICO') or (Ext='CUR');
end;

function TICOLoader.GetLoadFilter: string;
begin
  Result:=rsWindowsIcon+' (*.ico)|*.ico|'+rsWindowsCursor+' (*.cur)|*.cur';
end;

procedure TICOLoader.LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
const
  Spacing = 4;
var
  List : TIconList;
  IconIndex, I, X, Y : Integer;
  AllBitmap : TBitmap;
begin
  List:=TIconList.Create;
  try
    try
      List.LoadFromStream(Stream);
    except
      if List.Count=0 then raise;
    end;
    if List.Count=0 then raise ELinearBitmap.Create(rsNoIconsInFile);
    if Mode=mLoadAll then
    begin
      Y:=0;
      X:=0;
      for I:=0 to List.Count-1 do
      begin
        X:=Max(X,List.Icons[I].Width);
        Inc(Y,List.Icons[I].Height+Spacing);
      end;
      AllBitmap:=TBitmap.Create;
      try
        AllBitmap.PixelFormat:=pf24bit;
        AllBitmap.Canvas.Lock;
        AllBitmap.Canvas.Brush.Color:=TransparentColor;
        AllBitmap.Width:=X;
        AllBitmap.Height:=Y-Spacing;
        Y:=0;
        for I:=0 to List.Count-1 do
        begin
          List.Icons[I].Draw(0,Y,AllBitmap.Canvas);
          Inc(Y,List.Icons[I].Height+Spacing);
        end;
        Bitmap.Assign(AllBitmap);
        AllBitmap.Canvas.Unlock;
      finally
        AllBitmap.Free;
      end;
    end
    else
    begin
      if Mode=mLoadFirst then IconIndex:=0
      else with TIconSelectionForm.Create(Application) do
      try
        IconList:=List;
        DialogColor:=Self.TransparentColor;
        if ShowModal<>mrOk then Abort;
        Self.TransparentColor:=DialogColor;
        IconIndex:=SelectBox.ItemIndex;
      finally
        Free;
      end;
      if List.ListType=ltCursor then CursorHotSpot:=List.Icons[IconIndex].HotSpot;
      List.Icons[IconIndex].AssignTo(Bitmap,TransparentColor);
    end;
  finally
    List.Free;
  end;
end;

procedure TICOLoader.SaveToStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
var
  List : TIconList;
  I, LastColor, ColorCount : Integer;
  X : DWord;
  TR, TG, TB : Byte;
  Pix8, MaskPtr, Dest : PByte;
  Pix24 : PRGBRec;
  FlipTransparentColor : TColor;
begin
  if (Bitmap.Width>255) or (Bitmap.Height>255) then raise Exception.Create(rsImageIsTooLargeForIcon);

  List:=TIconList.Create;
  try
    if Ext='ICO' then List.ListType:=ltIcon
    else List.ListType:=ltCursor;

    if Bitmap.PixelFormat=pf24bit then
    begin
      FlipTransparentColor:=RGB2BGR(TransparentColor);
      List.AddIcon(Bitmap.Width,Bitmap.Height,24);
      with List.Icons[0] do
      begin
        ZeroMem(Mask^,MaskSize);
        for I:=0 to Height-1 do
        begin
          Pix24:=@Image^[I*BytesPerLine];
          Move(Bitmap.ScanLine[Height-1-I]^,Pix24^,Bitmap.BytesPerLine);
          MaskPtr:=@Mask^[I*MaskBytesPerLine];
          for X:=0 to Width-1 do
          begin
            if PColor(Pix24)^ and $ffffff=FlipTransparentColor then
            begin
              Pix24^:=BlackPix24;
              PByte(DWord(MaskPtr)+X shr 3)^:=PByte(DWord(MaskPtr)+X shr 3)^ or (128 shr (X and 7));
            end;
            Inc(Pix24);
          end;
        end;
      end;
    end
    else
    begin
      TR:=TransparentColor and $ff; TG:=(TransparentColor shr 8) and $ff; TB:=(TransparentColor shr 16) and $ff;

      // Find number of colors used
      LastColor:=1;
      with Bitmap do for I:=0 to Size-1 do if (Map^[I]>LastColor) then
        with Bitmap.Palette^[Map^[I]] do
          if (R<>TR) or (G<>TG) or (B<>TB) then LastColor:=Bitmap.Map^[I];
      ColorCount:=LastColor+1;

      {if ColorCount<=2 then
      begin
        List.AddIcon(Bitmap.Width,Bitmap.Height,1);
        ColorCount:=2;
      end
      else} if ColorCount<=16 then
      begin
        List.AddIcon(Bitmap.Width,Bitmap.Height,4);
        ColorCount:=16;
        with List.Icons[0] do
        begin
          ZeroMem(Mask^,MaskSize);
          ZeroMem(Image^,ImageSize);
          for I:=0 to Height-1 do
          begin
            Pix8:=Bitmap.ScanLine[Height-1-I];
            Dest:=@Image^[I*BytesPerLine];
            MaskPtr:=@Mask^[I*MaskBytesPerLine];
            for X:=0 to Width-1 do
            begin
              with Bitmap.Palette^[Pix8^] do if (R=TR) and (G=TG) and (B=TB) then
                PByte(DWord(MaskPtr)+X shr 3)^:=PByte(DWord(MaskPtr)+X shr 3)^ or (128 shr (X and 7))
              else
                Dest^:=Dest^ or (Pix8^ shl ((not X and 1) shl 2));

              if X and 1=1 then Inc(Dest);
              Inc(Pix8);
            end;
          end;
        end;
      end
      else // 256 colors
      begin
        List.AddIcon(Bitmap.Width,Bitmap.Height,8);
        ColorCount:=256;
        with List.Icons[0] do
        begin
          ZeroMem(Mask^,MaskSize);
          for I:=0 to Height-1 do
          begin
            Pix8:=@Image^[I*BytesPerLine];
            Move(Bitmap.ScanLine[Height-1-I]^,Pix8^,Bitmap.BytesPerLine);
            MaskPtr:=@Mask^[I*MaskBytesPerLine];
            for X:=0 to Width-1 do
            begin
              with Bitmap.Palette^[Pix8^] do if (R=TR) and (G=TG) and (B=TB) then
              begin
                Pix8^:=0;
                PByte(DWord(MaskPtr)+X shr 3)^:=PByte(DWord(MaskPtr)+X shr 3)^ or (128 shr (X and 7));
              end;
              Inc(Pix8);
            end;
          end;
        end;
      end;

      with List.Icons[0] do // Convert palette
      begin
        MakeLogPalette(Bitmap.Palette^,Palette^,ColorCount);
        for I:=0 to ColorCount-1 do with Palette^[I] do
          if (peRed=TR) and (peGreen=TG) and (peBlue=TB) then DWord(Palette^[I]):=0;
      end;
    end;

    List.Icons[0].HotSpot:=CursorHotSpot;
    List.SaveToStream(Stream);
  finally
    List.Free;
  end;
end;

//==============================================================================================================================
// TIconSelectionForm
//==============================================================================================================================
procedure TIconSelectionForm.FormShow(Sender: TObject);
var
  TypeStr : string;
  I : Integer;
begin
  {$IFDEF UseStyleForm}
  UseBackgroundTheme:=True;
  {$ENDIF}
  SelectBox.Clear;
  case IconList.ListType of
    ltIcon   : TypeStr:='Icon';
    ltCursor : TypeStr:='Cursor';
  end;
  for I:=0 to IconList.Count-1 do
    SelectBox.Items.Add(Format('%s %d x %d %d bit',[TypeStr,IconList.Icons[I].Width,IconList.Icons[I].Height,
                                                    IconList.Icons[I].BitsPerPixel]));
  SelectBox.ItemIndex:=0;
  SelectBoxChange(Self);
end;

procedure TIconSelectionForm.SelectBoxChange(Sender: TObject);
begin
  PanelFrame.Width:=Min(IconList.Icons[SelectBox.ItemIndex].Width+2,193);
  PanelFrame.Height:=Min(IconList.Icons[SelectBox.ItemIndex].Height+2,73);
  with PanelFrame.BitmapCanvas do
  begin
    Brush.Color:=DialogColor;
    FillRect(ClipRect);
  end;
  IconList.Icons[SelectBox.ItemIndex].Draw(0,0,PanelFrame.BitmapCanvas);
  PanelFrame.Invalidate;
  SetColorPanelColor(ColorPanel,DialogColor);
end;

procedure TIconSelectionForm.TransparentButtonClick(Sender: TObject);
begin
  if ShowColorDialog(DialogColor) then SelectBoxChange(Self);
end;

initialization
  Default:=TICOLoader.Create;
  LinarBitmap.AddLoader(Default);
  TPicture.RegisterFileFormat('cur',rsWindowsCursor,TIcon);
finalization
  Default.Free;
end.

