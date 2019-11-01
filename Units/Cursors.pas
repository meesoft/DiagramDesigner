///////////////////////////////////////////////////////////////////////////////////////////////
//
// Cursors.pas - Mouse cursor resources
// ------------------------------------
// Version:   2003-06-20
// Maintain:  Michael Vinther  |  mv@logicnet·dk
//
unit Cursors;

interface

const         
  crBoxSelect     = 1;
  crMove          = 2;
  crDrawRectangle = 3;
  crHand          = 4;
  crZoom          = 5;
  crCross         = 6;
  crPen           = 7;
  crRotate        = 8;

implementation

uses Forms, Windows;

{$R CURSORS.RES}

procedure LoadCursors;
begin
  Screen.Cursors[crBoxSelect]    :=LoadCursor(HInstance,'CUR_BOXSELECT');
  Screen.Cursors[crMove]         :=LoadCursor(HInstance,'CUR_MOVE');
  Screen.Cursors[crZoom]         :=LoadCursor(HInstance,'CUR_ZOOM');
  Screen.Cursors[crDrawRectangle]:=LoadCursor(HInstance,'CUR_DRAWRECTANGLE');
  Screen.Cursors[crHand]         :=LoadCursor(HInstance,'CUR_HAND');
  Screen.Cursors[crCross]        :=LoadCursor(HInstance,'CUR_COLORSELECT');
  Screen.Cursors[crPen]          :=LoadCursor(HInstance,'CUR_PEN');
  Screen.Cursors[crRotate]       :=LoadCursor(HInstance,'CUR_ROTATE');
end;

initialization
  LoadCursors;
end.

