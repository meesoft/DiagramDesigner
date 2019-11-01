unit ExpressionPlot;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExpressionEval,
  ValueEdits, PanelFrame, StdCtrls, ExtCtrls, Clipbrd, Math
{$IFDEF UseStyleForm}, StyleForm; {$ELSE}; type TStyleForm = TForm; {$ENDIF}

type
  TExpressionPlotForm = class(TStyleForm)
    TopPanel: TPanel;
    MinEdit: TFloatEdit;
    MaxEdit: TFloatEdit;
    Label1: TLabel;
    Label2: TLabel;
    CloseButton: TButton;
    PlotPanel: TDoubleBufferedPanel;
    CopyButton: TButton;
    Label3: TLabel;
    procedure DoPlot(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure PlotPanelMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    MaxF, MinF : EvalFloat;
    PlotArea : TRect;
  public
    { Public declarations }
    PlotExpression : string;
    PlotSymbol : TSymbolValue;
    SymbolTable : TSymbolTable;
    procedure Plot(Canvas: TCanvas);
  end;

  TPlotFunction = class(TSymbolicFunction)
    private
      Active : Boolean;
    public
      xMin, xMax : EvalFloat;
      constructor Create; override;
      function Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat; override;
      function VarName: string; override;
      function Help: string; override;
    end;

function CeilDigits(const Value: Extended; Digits: Integer): Extended;

resourcestring
  rsPlotGraphOfExpressionInX = 'Plot graph of expression in x';

implementation

{$R *.DFM}

//=======================================================================================================
// TPlotFunction
//=======================================================================================================
constructor TPlotFunction.Create;
begin
  inherited Create;
  SymbolType:=stSymbolicFunction;
  xMin:=-10;
  xMax:=10;
end;

function TPlotFunction.Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat;
var
  Symbolic : string;
  xSymbol : TSymbolValue;
  Start : Integer;
  Level, VarStart, I : Integer;
  ExistingSymbol : TSymbol;
  VarName : string;
begin
  if Active then raise EExpressionError.Create(rsRecursiveReference);
  Active:=True;
  try
    Start:=Pos;
    Level:=0;
    while (Pos<=Length(Expression)) and ((Level<>0) or (Expression[Pos]<>',')) do
    begin
      if Expression[Pos]='(' then Inc(Level)
      else if Expression[Pos]=')' then
      begin
        Dec(Level);
        if Level<0 then raise EExpressionError.Create(Format(rsSExpected,[',']),Pos);
      end;
      Inc(Pos);
    end;
    if (Pos>Length(Expression)) then raise EExpressionError.Create(Format(rsSExpected,[',']),Pos);
    Symbolic:=Copy(Expression,Start,Pos-Start);
    Inc(Pos);
    while (Pos<=Length(Expression)) and (Expression[Pos] in SeparatorChars) do Inc(Pos);
    VarStart:=Pos;
    if not (Expression[Pos] in TextSymbolStartChars) then raise EExpressionError.Create(rsNameExpected,Pos);
    Inc(Pos);
    while Expression[Pos] in TextSymbolChars do Inc(Pos);
    VarName:=Copy(Expression,VarStart,Pos-VarStart);
    if VarName='' then raise EExpressionError.Create(rsNameExpected,Pos);

    xSymbol:=TSymbolValue.Create;
    I:=Table.IndexOf(VarName);
    if I=-1 then
    begin
      ExistingSymbol:=nil;
      Table.AddObject(VarName,xSymbol);
    end
    else
    begin
      ExistingSymbol:=TSymbol(Table.Objects[I]);
      Table.Objects[I]:=xSymbol;
    end;
    try
      with TExpressionPlotForm.Create(nil) do
      try
        UseBackgroundTheme:=False;
        Caption:=Caption+' '+Symbolic;
        PlotExpression:=Symbolic;
        PlotSymbol:=xSymbol;
        MinEdit.Value:=xMin;
        MaxEdit.Value:=xMax;
        SymbolTable:=Table;
        ShowModal;
        xMin:=MinEdit.Value;
        xMax:=MaxEdit.Value;
      finally
        Free;
      end;
    finally
      if Assigned(ExistingSymbol) then Table.Define(VarName,ExistingSymbol)
      else Table.Remove(VarName);
    end;
  finally
    Active:=False;
  end;
  raise EExpressionError.Create('Plot');
end;

function TPlotFunction.VarName: string;
begin
  Result:='expression,x';
end;

function TPlotFunction.Help: string;
begin
  Result:=rsPlotGraphOfExpressionInX;
end;

//=======================================================================================================
// TExpressionPlotForm
//=======================================================================================================

function FloatToShortStr(const Value: Extended): string;
begin
  Result:=FloatToStrF(Value,ffGeneral,5,4)
end;

function CeilDigits(const Value: Extended; Digits: Integer): Extended;
var
  S : Extended;
begin
  if Value=0 then Result:=0
  else
  begin
    S:=Power(10,Ceil(Log10(Abs(Value)))-Digits);
    Result:=Ceil(Value/S)*S;
  end;
end;

procedure TExpressionPlotForm.FormCreate(Sender: TObject);
begin
  Font.Color:=clWindowText;
  ClientWidth:=Max(ClientWidth,CloseButton.Left+CloseButton.Width+Label1.Left);
  ClientHeight:=(ClientWidth-TopPanel.Height)*3 div 4;
  CopyButton.Height:=MinEdit.Height;
  CloseButton.Height:=MinEdit.Height;
end;

procedure TExpressionPlotForm.Plot(Canvas: TCanvas);
const
  Inf : EvalFloat = 1/0;
  NaN : EvalFloat = 6465465465e3221;
var
  X, Y : Integer;
  FList : array of EvalFloat;
  F, Scale : EvalFloat;
  PosKnown, LastPosKnown : Boolean;
  MaxRect, MinRect, LabelRect, DummyRect : TRect;
  Str : string;
  S : Extended;
begin
  Screen.Cursor:=crHourGlass;
  try
    Canvas.Font:=Font;
    with Canvas do
    begin
      PlotArea:=PlotPanel.BitmapCanvas.ClipRect;
      Dec(PlotArea.Right);
      Inc(PlotArea.Top);
      Dec(PlotArea.Bottom,TextHeight('A')*3 div 2);
      Inc(PlotArea.Left,TextWidth('AAAAAAAAAAA'));

      //plot(x,x)

      // Print MinEdit.Value label
      Brush.Style:=bsClear;
      Str:=FloatToShortStr(MinEdit.Value);
      with TextExtent(Str) do MinRect:=Bounds(PlotArea.Left,PlotArea.Bottom+CY div 4,CX,CY);
      TextOut(MinRect.Left,MinRect.Top,Str);
      // Print MaxEdit.Value label
      Str:=FloatToShortStr(MaxEdit.Value);
      with TextExtent(Str) do MaxRect:=Bounds(PlotArea.Right-CX+1,PlotArea.Bottom+CY div 4,CX,CY);
      TextOut(MaxRect.Left,MaxRect.Top,Str);

      SetLength(FList,PlotArea.Right-PlotArea.Left);
      Scale:=(MaxEdit.Value-MinEdit.Value)/High(FList);

      Pen.Width:=1;
      Pen.Color:=clGray;
      if (MinEdit.Value<0) and (MaxEdit.Value>0) and (Scale<>0) then // Draw vertical axis |
      begin
        X:=PlotArea.Left+Round(-MinEdit.Value/Scale);
        MoveTo(X,PlotArea.Top);
        LineTo(X,PlotArea.Bottom);
        with TextExtent('0') do LabelRect:=Bounds(X-CX div 2,PlotArea.Bottom+CY div 4,CX,CY);
        if not IntersectRect(DummyRect,LabelRect,MaxRect) and not IntersectRect(DummyRect,LabelRect,MinRect) then
          TextOut(LabelRect.Left,LabelRect.Top,'0');
      end;

      MaxF:=-Inf;
      MinF:=Inf;
      for X:=0 to High(FList) do // Evaluate function
      begin
        PlotSymbol.Value:=X*Scale+MinEdit.Value;
        try
          F:=EvaluateExpression(PlotExpression,SymbolTable);
          if (F<MinF) and (F<>-Inf) then MinF:=F;
          if (F>MaxF) and (F<>Inf) then MaxF:=F;
        except
          F:=NaN;
        end;
        FList[X]:=F;
      end;

      if ((MinF=0) and (MaxF=0)) or (MaxF<MinF) then
      begin
        MaxF:=1;
        MinF:=-1;           
      end
      else
      begin
        if MaxF=MinF then S:=Power(10,Ceil(Log10(Abs(MaxF)))-2)
        else S:=Power(10,Ceil(Log10(MaxF-MinF))-2);
        try
          MaxF:=Ceil(MaxF/S)*S;
          MinF:=Floor(MinF/S)*S;
        except
          // Ignore overflow
        end;
      end;

      // Print MaxF label
      Str:=FloatToShortStr(MaxF);
      with TextExtent(Str) do MaxRect:=Bounds(PlotArea.Left-CX-4,PlotArea.Top-1,CX,CY);
      TextOut(MaxRect.Left,MaxRect.Top,Str);
      // Print MinF label
      Str:=FloatToShortStr(MinF);
      with TextExtent(Str) do MinRect:=Bounds(PlotArea.Left-CX-4,PlotArea.Bottom-CY,CX,CY);
      TextOut(MinRect.Left,MinRect.Top,Str);

      try
        if MinF=MaxF then Scale:=1
        else Scale:=(PlotArea.Bottom-PlotArea.Top-1)/(MaxF-MinF);
      except
        Scale:=1;
      end;
      if (MinF<0) and (MaxF>0) then // Draw horizontal axis -
      begin
        Y:=PlotArea.Bottom-1-Round(-MinF*Scale);
        MoveTo(PlotArea.Left,Y);
        LineTo(PlotArea.Right,Y);
        with TextExtent('0') do LabelRect:=Bounds(PlotArea.Left-CX-4,Y-CY div 2,CX,CY);
        if not IntersectRect(DummyRect,LabelRect,MaxRect) and not IntersectRect(DummyRect,LabelRect,MinRect) then
          TextOut(LabelRect.Left,LabelRect.Top,'0');
      end;
      Pen.Color:=clBlack;
      with PlotArea do
      begin
        MoveTo(Left-3,Top);
        LineTo(Right,Top);
        LineTo(Right,Bottom+4);
        MoveTo(Left,Top);
        LineTo(Left,Bottom+4);
        MoveTo(Left-3,Bottom);
        LineTo(Right,Bottom);
      end;
      Pen.Color:=clBlue;
      LastPosKnown:=False;

      for X:=0 to High(FList) do // Plot function
      begin
        PosKnown:=True;
        if FList[X]=NaN then
        begin
          Y:=0;
          PosKnown:=False;
        end
        else if FList[X]=-Inf then
        begin
          Y:=0;
          if X>0 then LastPosKnown:=(FList[X]<>FList[X-1]) and (FList[X-1]<>NaN);
        end
        else if FList[X]=Inf then
        begin
          Y:=PlotArea.Bottom-PlotArea.Top-1;
          if X>0 then LastPosKnown:=(FList[X]<>FList[X-1]) and (FList[X-1]<>NaN);
        end
        else Y:=Round((FList[X]-MinF)*Scale);

        if PosKnown then
        begin
          if LastPosKnown then LineTo(PlotArea.Left+X,PlotArea.Bottom-1-Y)
          else MoveTo(PlotArea.Left+X,PlotArea.Bottom-1-Y);
        end
        else if LastPosKnown then LineTo(PenPos.X,PenPos.Y);
        LastPosKnown:=PosKnown;
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TExpressionPlotForm.DoPlot(Sender: TObject);
begin
  with PlotPanel.BitmapCanvas do
  begin
    Brush.Style:=bsSolid;
    Brush.Color:=clBtnFace;
    FillRect(ClipRect);
  end;
  if MinEdit.Valid and MaxEdit.Valid and (MinEdit.Value<MaxEdit.Value) then Plot(PlotPanel.BitmapCanvas);
  PlotPanel.Invalidate;
end;

procedure TExpressionPlotForm.CopyButtonClick(Sender: TObject);
var
  Metafile : TMetafile;
  Canvas : TMetafileCanvas;
begin
  Metafile:=TMetafile.Create;
  try
    Metafile.Width:=PlotPanel.BackBuffer.Width;
    Metafile.Height:=PlotPanel.BackBuffer.Height;
    Canvas:=TMetafileCanvas.Create(Metafile,0);
    try
      Plot(Canvas);
    finally
      Canvas.Free;
    end;
    Clipboard.Assign(Metafile);
  finally;
    Metafile.Free;
  end;
end;

procedure TExpressionPlotForm.PlotPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Dec(X,2); Dec(Y,2);
  if PtInRect(PlotArea,Point(X,Y)) then
    PlotPanel.Hint:=FloatToShortStr((X-PlotArea.Left)/(PlotArea.Right-PlotArea.Left-1)*(MaxEdit.Value-MinEdit.Value)+MinEdit.Value)+'; '+
                    FloatToShortStr((PlotArea.Bottom-Y-1)/(PlotArea.Bottom-PlotArea.Top-1)*(MaxF-MinF)+MinF)
  else PlotPanel.Hint:='';
end;

initialization
  RegisterStandardSymbol('plot',TPlotFunction);
end.

