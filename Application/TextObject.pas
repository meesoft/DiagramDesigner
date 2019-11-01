unit TextObject;

interface

uses
  Windows, SysUtils, Classes, Graphics, Types, Math, Streams, DiagramBase;

const
  DefaultLineWidth = 3*DesignerDPpoint div 4;

type
  TTextAlign = -1..3;
            // -1  Left
            //  0  Block left
            //  1  Right
            //  2  Center
            //  3  Block right

  PFontStyles = ^TFontStyles;

  TTextObject = class(TBaseObject)
    protected
      AADraw : Boolean; // If True, DrawAntialiasing calls Draw with Text=''. Should be determined in Draw when DrawMode=dmPreAntialiasing.
      FText : string;
      FTextXAlign : TTextAlign;
      FTextYAlign : TTextAlign;
      FTextColor  : TColor;
      FMargin : Integer;
      FEditTextAfterPlace : Boolean;
      Angle : Single;
      CurrentTextRect : TRect; // Only valid after call to draw
      CanvasTextMargin : Integer; // Hint for auto line breaking in Draw
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
    public
      constructor CreateNew(PropertyObject: TBaseObject=nil); overload; override;
      constructor CreateNew(const Text: string); reintroduce; overload;
      function ValidProperties: TObjectProperties; override;
      function Hint: string; override;
      function EditTextAfterPlace: Boolean;
      procedure Rotate(const DAngle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint); override;
      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      procedure DrawAntialiasing(Canvas: TCanvas; const CanvasInfo: TCanvasInfo); override;
      class function Identifier: Integer; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
      procedure SetTextAndName(const Text: string);
    end;

function CleaupText(const Text: string; IncludePopup: Boolean=False): string;

implementation

uses
  WinAPIUtils, StrUtils, StreamUtils, StringUtils, LinarBitmap;

function CleaupText(const Text: string; IncludePopup: Boolean): string;
var
  I, P : Integer;
begin
  Result:=Text;
  I:=Pos('\',Result);
  if I>0 then
    repeat
      if Result[I]='\' then
        case Result[I+1] of
          '0'..'9' : if not (Result[I+2] in ['0'..'9']) then Delete(Result,I,2)
                     else if not (Result[I+3] in ['0'..'9']) then Delete(Result,I,3)
                     else Delete(Result,I,4);
          '"'      : begin
                       P:=I+2;
                       while not (Result[P] in [#0,'"']) do Inc(P);
                       Delete(Result,I,P-I+1);
                     end;
          '\'      : begin
                       Delete(Result,I,1);
                       Inc(I);
                     end;
          'A'      : Delete(Result,I,MaxInt);
          'N'      : if IncludePopup or (I=1) then Delete(Result,I,2)
                     else Delete(Result,I,MaxInt);
          'C'      : Delete(Result,I,8);
          '@'      : begin
                       Delete(Result,I,2);
                       P:=PosEx('\@',Result,I);
                       if P=0 then Break;
                       I:=P;
                       Delete(Result,I,2);
                     end;
          else Delete(Result,I,2);
        end
      else Inc(I);
    until I>Length(Result);
end;

//==============================================================================================================================
// TTextObject
//==============================================================================================================================
constructor TTextObject.CreateNew(const Text: string);
begin
  Create;
  FMargin:=DesignerDPpoint*2;
  SetTextAndName(Text);
end;

constructor TTextObject.CreateNew(PropertyObject: TBaseObject);
begin
  CreateNew('abc');
  if Assigned(PropertyObject) then Assign(PropertyObject);
  FEditTextAfterPlace:=True;
end;

function TTextObject.EditTextAfterPlace: Boolean;
begin
  Result:=FEditTextAfterPlace;
  FEditTextAfterPlace:=False;
end;

function TTextObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opText,opPosition,opTextXAlign,opTextYAlign,opMargin,opTextColor,opAngle];
end;

function TTextObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opText            : Result:=Integer(@FText);
    opTextXAlign      : Result:=FTextXAlign;
    opTextYAlign      : Result:=FTextYAlign;
    opBlockAlignOnly  : Result:=0;
    opTextColor       : Result:=FTextColor;
    opPosition        : Result:=Integer(@FPosition);
    opMargin          : Result:=FMargin;
    opAngle           : Result:=Integer(@Angle);
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TTextObject.SetProperty(Index: TObjectProperty; Value: Integer);
begin
  case Index of
    opText            : FText:=PString(Value)^;
    opTextXAlign      : FTextXAlign:=Value;
    opTextYAlign      : FTextYAlign:=Value;
    opTextColor       : FTextColor:=Value;
    opPosition        : begin
                          Position:=PRect(Value)^;
                          NotifyMovement;
                        end;
    opMargin          : FMargin:=Value;
    opAngle           : begin
                          Angle:=PSingle(Value)^;
                          //RotateHandlePos.X:=Nan;
                        end;
    else inherited SetProperty(Index,Value);
  end;
end;

procedure TTextObject.Rotate(const DAngle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint);
begin
  inherited;
  if opAngle in ValidProperties then Angle:=Angle+DAngle;
end;

function TTextObject.Hint: string;
var
  P : Integer;
begin
  P:=Pos('\',FText);
  if P=0 then Exit;
  Inc(P);
  while P<Length(FText) do
  begin
    if FText[P] in ['A','N'] then
    begin
      Result:=StringReplace(Copy(FText,P+1,MaxInt),'\n',#13,[rfReplaceAll,rfIgnoreCase]);
      Break;
    end;
    while P<Length(FText) do
    begin
      Inc(P);
      if FText[P]='\' then Break;
    end;
    Inc(P);
  end;
end;

procedure TTextObject.SetTextAndName(const Text: string);

  function NameFromText(const Text: string): string;
  begin
    Result:=RemLeadSpace(CleaupText(Text));
    if Length(Result)>16 then SetLength(Result,16);
    Result:=RemTailSpace(Result);
    if Result='' then Result:=ExtractObjectName(ClassName);
  end;

begin
  if (Name='') or (Name=ExtractObjectName(ClassName)) or (Name=NameFromText(FText)) then
    Name:=NameFromText(Text);
  FText:=Text;
end;

procedure TTextObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);
var
  TextSize : TSize;
  X, Y, IntAngle : Integer;
  Str : string;
  LineWidths : array[0..255] of Integer;
  CanvasRect : TRect;

  procedure ParseText(DoDraw: Boolean);
  var
    P, I, PX, PY, Size, ScriptStartX, OverlineStartX, OverlineMaxY, LineHeight, LineY, Collect : Integer;
    Line : Byte;
    TextMetric : TTextMetric;

    procedure AlignLine;
    begin
      if IntAngle mod 180 in [46..134] then
        case FTextXAlign of
          -1 : X:=CanvasRect.Top+Round(FMargin*CanvasInfo.Scale.X);
           1 : X:=CanvasRect.Bottom-Round(FMargin*CanvasInfo.Scale.X)-LineWidths[Line];
           2 : X:=CanvasRect.Top+(CanvasRect.Bottom-CanvasRect.Top-LineWidths[Line]) div 2;
           3 : X:=CanvasRect.Bottom-(CanvasRect.Bottom-CanvasRect.Top-TextSize.CX) div 2-LineWidths[Line];
          else X:=CanvasRect.Top+(CanvasRect.Bottom-CanvasRect.Top-TextSize.CX) div 2;
        end
      else
        case FTextXAlign of
          -1 : X:=CanvasRect.Left+Round(FMargin*CanvasInfo.Scale.X);
           1 : X:=CanvasRect.Right-Round(FMargin*CanvasInfo.Scale.X)-LineWidths[Line];
           2 : X:=CanvasRect.Left+(CanvasRect.Right-CanvasRect.Left-LineWidths[Line]) div 2;
           3 : X:=CanvasRect.Right-(CanvasRect.Right-CanvasRect.Left-TextSize.CX) div 2-LineWidths[Line];
          else X:=CanvasRect.Left+(CanvasRect.Right-CanvasRect.Left-TextSize.CX) div 2;
        end;
    end;

    procedure NewLine(Height: Integer);
    begin
      TextSize.CX:=Max(TextSize.CX,PX-X);
      LineWidths[Line]:=PX-X;
      Inc(Line);
      if DoDraw then AlignLine;
      PX:=X;
      Inc(PY,Height);
      LineY:=PY;
      Inc(TextSize.CY,LineHeight);
    end;

    procedure Write(const Str: string);
    var
      P, LineStart, PrevSpace, WidthAtPrevSpace, Width, MaxWidth : Integer;
    begin
      with Canvas do
      begin
        Width:=TextWidth(Str);
        if (CanvasInfo.Container<>nil) and (CanvasInfo.Container.AutoLineBreak) then
        begin
          if IntAngle in [46..134] then
            MaxWidth:=CanvasRect.Bottom-CanvasRect.Top-CanvasTextMargin-Round(FMargin*CanvasInfo.Scale.Y)
          else
            MaxWidth:=CanvasRect.Right-CanvasRect.Left-CanvasTextMargin-Round(FMargin*CanvasInfo.Scale.X);
          if (PX-X+Width>MaxWidth) and (MaxWidth>0) then
          begin
            LineStart:=1;
            Width:=0;
            PrevSpace:=0;
            WidthAtPrevSpace:=0;
            for P:=1 to Length(Str) do
            begin
              if Str[P]=' ' then
              begin
                PrevSpace:=P;
                WidthAtPrevSpace:=Width+TextWidth(' ');
              end;
              Inc(Width,TextWidth(Str[P]));
              if (Str[P+1]<>' ') and (Str[P] in ['-',':','\','/','>','<','+','*']) then
              begin
                PrevSpace:=P;
                WidthAtPrevSpace:=Width;
              end;
              if (PrevSpace>0) and (PX-X+Width>MaxWidth) then
              begin
                if DoDraw then
                begin
                  if CanvasInfo.DisableFontSmoothing then DisableFontSmoothing(Font);
                  TextOut(PX,PY,Copy(Str,LineStart,PrevSpace-LineStart+1));
                end;
                Inc(PX,WidthAtPrevSpace);
                NewLine(LineHeight);
                LineStart:=PrevSpace+1;
                PrevSpace:=0;
                Dec(Width,WidthAtPrevSpace);
              end;
            end;
            P:=Length(Str);
            if DoDraw and (P>=LineStart) then
            begin
              if CanvasInfo.DisableFontSmoothing then DisableFontSmoothing(Font);
              TextOut(PX,PY,Copy(Str,LineStart,P-LineStart+1));
            end;
            Inc(PX,Width);
            Exit;
          end;
        end;

        if DoDraw then
        begin
          if CanvasInfo.DisableFontSmoothing then DisableFontSmoothing(Font);
          TextOut(PX,PY,Str);
        end;
        Inc(PX,Width);
      end;
    end;

    procedure WriteSymbol(const Str: string);
    var
      PrevFont : string;
    begin
      with Canvas do
      begin
        PrevFont:=Font.Name;
        Font.Name:='Symbol';
        Write(Str);
        Font.Name:=PrevFont;
      end;
    end;

  begin
    Line:=0;
    with Canvas do
    begin
      Size:=CanvasInfo.DefaultFont.Size;
      Font:=CanvasInfo.DefaultFont;
      Font.Height:=-Round(Size*DesignerDPpoint*CanvasInfo.Scale.Y);
      if DoDraw then
      begin
        Font.Color:=FTextColor;
        AlignLine;
        SetTextAlign(Handle,TA_BASELINE);
        GetTextMetrics(Handle,TextMetric);
      end;

      ScriptStartX:=Low(Integer);
      OverlineStartX:=Low(Integer);
      OverlineMaxY:=0;
      PX:=X;
      PY:=Y+TextMetric.tmAscent;
      LineY:=PY;
      LineHeight:=TextHeight('A');
      TextSize.CY:=LineHeight;
      P:=Pos('\',FText);
      Collect:=P-1;
      repeat
        if FText[P]='\' then
        begin
          if Collect>0 then
          begin
            Write(Copy(FText,P-Collect,Collect));
            Collect:=0;
          end;
          Inc(P);
          case FText[P] of
            'A',
            'N' : P:=MaxInt-1; // Link or hint, skip rest of string
            'I' : Font.Style:=Font.Style+[fsItalic]; // Italic
            'i' : Font.Style:=Font.Style-[fsItalic];
            'B' : Font.Style:=Font.Style+[fsBold]; // Bold
            'b' : Font.Style:=Font.Style-[fsBold];
            'U' : Font.Style:=Font.Style+[fsUnderline]; // Underline
            'u' : Font.Style:=Font.Style-[fsUnderline];
            'T' : Font.Style:=Font.Style+[fsStrikeOut]; // Strikeout
            't' : Font.Style:=Font.Style-[fsStrikeOut];
            'L' : begin // Subscript
                    PY:=LineY+Round(Size*(DesignerDPpoint*0.18)*CanvasInfo.Scale.Y);
                    Font.Height:=-Round(Size*(DesignerDPpoint*0.6)*CanvasInfo.Scale.Y);
                    if ScriptStartX=Low(Integer) then ScriptStartX:=PX
                    else PX:=ScriptStartX;
                  end;
            'H' : begin // Superscript
                    PY:=LineY-Round(Size*(DesignerDPpoint*0.55)*CanvasInfo.Scale.Y);
                    Font.Height:=-Round(Size*(DesignerDPpoint*0.6)*CanvasInfo.Scale.Y);
                    if ScriptStartX=Low(Integer) then ScriptStartX:=PX
                    else PX:=ScriptStartX;
                    if OverlineStartX<>Low(Integer) then OverlineMaxY:=Min(OverlineMaxY,PY+Round(Font.Height*0.95));
                  end;
            'l',
            'h' : begin
                    PY:=LineY;
                    Font.Height:=-Round(Size*DesignerDPpoint*CanvasInfo.Scale.Y);
                    ScriptStartX:=Low(Integer);
                  end;
            'O' : if OverlineStartX=Low(Integer) then
                  begin
                    OverlineStartX:=PX;
                    OverlineMaxY:=PY+Round(Font.Height*0.95);
                    if DoDraw then
                    begin
                      Pen.Color:=Font.Color;
                      Pen.Style:=psSolid;
                      Pen.Width:=-Font.Height div 16;
                    end;
                  end;
            'o' : if OverlineStartX=Low(Integer) then Write('•')
                  else
                  begin
                    if DoDraw then
                    begin
                      MoveTo(OverlineStartX,OverlineMaxY);
                      LineTo(PX,OverlineMaxY);
                    end;
                    OverlineStartX:=Low(Integer);
                  end;
            'S' : Font.Name:='Symbol'; // Symbol font
            's' : Font.Name:=CanvasInfo.DefaultFont.Name;
            'n' : NewLine(LineHeight); // New line
            'p' : begin // Page number
                    Str:=IntToStr(CanvasInfo.PageIndex+1);
                    Write(Str);
                  end;
            'c' : begin // Pages count
                    if CanvasInfo.Container=nil then Str:='?'
                    else Str:=IntToStr(CanvasInfo.Container.Count);
                    Write(Str);
                  end;
            'P' : with CanvasInfo do if Assigned(Container) then // Page name
                  begin
                    Str:=Container.Pages[PageIndex].GetName(PageIndex);
                    Write(Str);
                  end;
            '0'..
            '9' : begin // Font size
                    I:=P;
                    if FText[P+1] in ['0'..'9'] then // 2 digits
                    begin
                      Inc(P);
                      if FText[P+1] in ['0'..'9'] then Inc(P); // 3 digits
                    end;
                    Size:=StrToInt(Copy(FText,I,P-I+1));

                    Font.Height:=-Round(Size*DesignerDPpoint*CanvasInfo.Scale.Y);
                    if OverlineStartX<>Low(Integer) then OverlineMaxY:=Min(OverlineMaxY,PY+Round(Font.Height*0.95));
                    if PX=X then // Update line height if start of new line
                    begin
                      if DoDraw then
                      begin
                        I:=TextMetric.tmAscent;
                        GetTextMetrics(Handle,TextMetric);
                        I:=TextMetric.tmAscent-I;
                        Inc(PY,I);
                        Inc(LineY,I);
                      end;
                      Dec(TextSize.CY,LineHeight);
                      LineHeight:=TextHeight('A');
                      Inc(TextSize.CY,LineHeight);
                    end;
                  end;
            'C' : begin // Text color
                    Font.Color:=RGB2BGR(StrToIntDef('$'+Copy(FText,P+1,6),clBlack));
                    Inc(P,6);
                  end;
            '"' : begin // Font name
                    Inc(P);
                    I:=P;
                    while not (FText[P] in [#0,'"']) do Inc(P);
                    Font.Name:=Copy(FText,I,P-I);
                  end;
            '@' : begin // Unformatted string
                    Inc(P);
                    I:=PosEx('\@',FText,P);
                    if I=0 then
                    begin
                      Write(Copy(FText,P,MaxInt));
                      P:=Length(FText);
                    end
                    else
                    begin
                      Write(Copy(FText,P,I-P));
                      P:=I+1;
                    end;
                  end;
            '_' : begin // Separator line
                    if DoDraw then
                    begin
                      Pen.Style:=psSolid;
                      if opLineWidth in ValidProperties then Pen.Width:=Round(Properties[opLineWidth]*CanvasInfo.Scale.X)
                      else Pen.Width:=-Font.Height div 8;
                      if opLineColor in ValidProperties then Pen.Color:=Properties[opLineColor]
                      else Pen.Color:=Font.Color;
                      if PX>X then I:=LineHeight div 4
                      else I:=-LineHeight*2 div 4;
                      MoveTo(CanvasRect.Left,LineY+I);
                      LineTo(CanvasRect.Right,LineY+I);
                    end;
                    if PX>X then NewLine(LineHeight*6 div 4)
                    else NewLine(LineHeight*3 div 4);
                  end;
            '.' : Write('·');
            '#' : WriteSymbol('¨');
            '+' : Write('±');
            '*' : Write('×');
            '''': Write('°');
            '=' : WriteSymbol('¹');
            '~' : WriteSymbol('»');
            '>' : WriteSymbol('³');
            '<' : WriteSymbol('£');
            '/' : WriteSymbol('Ö');
            '-' : WriteSymbol('¸');
            '§' : WriteSymbol('¥');
            else Write(FText[P]); // Write "\" in case of "\\"
          end
        end
        else Inc(Collect);
        Inc(P);
      until P>Length(FText);
      if Collect>0 then Write(Copy(FText,P-Collect,Collect));

      if (OverlineStartX<>Low(Integer)) and DoDraw then // If overline active
      begin
        MoveTo(OverlineStartX,OverlineMaxY);
        LineTo(PX,OverlineMaxY);
      end;

      LineWidths[Line]:=PX-X;
      TextSize.CX:=Max(TextSize.CX,PX-X);
    end;
  end;

var
  Trans : XFORM;
  PrevMode : Integer;
begin
  inherited;
  if Ftext<>'' then
  begin
    CanvasRect:=CanvasInfo.CanvasRect(Position);
    IntAngle:=Round(Abs(Angle)*(180/Pi)) mod 180;

    // Prepare for rotation
    PrevMode:=0;
    if Angle<>0 then
    begin
      with Trans, CanvasRect do
      begin
        eM11:=Cos(Angle);
        eM12:=Sin(Angle);
        eM21:=-eM12;
        eM22:=eM11;
        eDx:=Left+Width*CanvasInfo.Scale.X*0.5;
        eDy:=Top+Height*CanvasInfo.Scale.Y*0.5;
        OffsetRect(CanvasRect,Round(-eDX),Round(-eDY));
      end;
      PrevMode:=SetGraphicsMode(Canvas.Handle,GM_ADVANCED);
      SetWorldTransform(Canvas.Handle,Trans);
    end;

    Canvas.Brush.Style:=bsClear;
    // Align text
    TextSize.CX:=0;
    if (FTextXAlign<>-1) or (FTextYAlign<>-1) then ParseText(False);
    case FTextYAlign of
      -1 : if IntAngle mod 180 in [46..134] then Y:=CanvasRect.Left+Round(FMargin*CanvasInfo.Scale.Y)
           else Y:=CanvasRect.Top+Round(FMargin*CanvasInfo.Scale.Y);
       1 : if IntAngle mod 180 in [46..134] then Y:=CanvasRect.Right-Round(FMargin*CanvasInfo.Scale.Y)-TextSize.CY
           else Y:=CanvasRect.Bottom-Round(FMargin*CanvasInfo.Scale.Y)-TextSize.CY;
      else Y:=CanvasRect.Top+((CanvasRect.Bottom-CanvasRect.Top)-TextSize.CY) div 2;
    end;
    // Do actual drawing
    ParseText(True);
    // Determine bounding box
    case FTextXAlign of
      -1 : CurrentTextRect:=Bounds(X,Y,TextSize.CX+1,TextSize.CY+1);
       1 : CurrentTextRect:=Bounds(CanvasRect.Right-Round(FMargin*CanvasInfo.Scale.X)-TextSize.CX,Y,TextSize.CX+1,TextSize.CY+1);
      else CurrentTextRect:=Bounds(CanvasRect.Left+(CanvasRect.Right-CanvasRect.Left-TextSize.CX) div 2,Y,TextSize.CX+1,TextSize.CY+1);
    end;

    // Restore after rotation
    if Angle<>0 then
    begin
      ModifyWorldTransform(Canvas.Handle,Trans,MWT_IDENTITY);
      SetGraphicsMode(Canvas.Handle,PrevMode);
    end;

    // Draw rectangle in Z-buffer
    if Assigned(CanvasInfo.ZBuffer) then with CanvasInfo.ZBuffer do
    begin
      Brush.Color:=Index;
      if Angle<>0 then
      begin
        PrevMode:=SetGraphicsMode(Handle,GM_ADVANCED);
        SetWorldTransform(Handle,Trans);
        FillRect(CurrentTextRect);
        ModifyWorldTransform(Handle,Trans,MWT_IDENTITY);
        SetGraphicsMode(Handle,PrevMode);
      end
      else FillRect(CurrentTextRect);
    end;
  end
  else if Assigned(CanvasInfo.ZBuffer) then
  begin
    with CanvasInfo.CanvasPoint(CenterPoint(Position)) do CanvasInfo.ZBuffer.Pixels[X,Y]:=Index;
  end;
end;

procedure TTextObject.DrawAntialiasing(Canvas: TCanvas; const CanvasInfo: TCanvasInfo);
var
  Text : string;
begin
  inherited;
  if AADraw then
  begin
    Text:=FText;
    FText:='';
    Draw(Canvas,CanvasInfo,0);
    FText:=Text;
  end;
end;

class function TTextObject.Identifier: Integer;
begin
  Result:=otTextObject;
end;

procedure TTextObject.SaveToStream(Stream: TBaseStream);
begin
  inherited;
  SaveString(FText,Stream);
  Stream.Write(FTextXAlign,1);
  Stream.Write(FTextYAlign,1);
  Stream.Write(FTextColor,4);
  Stream.Write(FMargin,4);
  Stream.Write(Angle,4);
end;

procedure TTextObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
var
  Align : ShortInt;
  I : Integer;
begin
  inherited;
  LoadString(FText,Stream);
  if FileVersion<8 then // Change \U to \H
  begin
    I:=1;
    while I<Length(FText) do
    begin
      if FText[I]='\' then
      begin
        Inc(I);
        case FText[I] of
          'U' : FText[I]:='H';
          'u' : FText[I]:='h';
        end;
      end;
      Inc(I);
    end;
  end;

  Stream.Read(Align,1); FTextXAlign:=Align;
  Stream.Read(Align,1); FTextYAlign:=Align;
  Stream.Read(FTextColor,4);
  if FileVersion>=11 then Stream.Read(FMargin,4)
  else FMargin:=DesignerDPpoint*2;
  if (FileVersion<26) and (FTextXAlign=2) and (FTextXAlign=2) then FMargin:=0;
  if FileVersion>=22 then Stream.Read(Angle,4)
end;

end.
