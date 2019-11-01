////////////////////////////////////////////////////////////////////////////////
//
// Complex.pas - Complex number handling
// -------------------------------------
// Version:   2005-08-14
// Maintain:  Michael Vinther   |   mv@logicnet·dk
//
unit Complex;

interface

uses MathUtils;

type
  TComplex = object
               Re, Im : Float;
               function Modulus: Double;
               function Angle: Double;
               function Power: Double;

               function Conjugate: TComplex;
               function Product(const Other: TComplex): TComplex;
               function Add(const Other: TComplex): TComplex;
               function Scale(const Value: Float): TComplex;
               procedure ScaleThis(const Value: Float);
             end;
  PComplex = ^TComplex;

  TComplexArray = array[0..0] of TComplex; // SizeOf(TComplexArray) must be equal to SizeOf(TComplex)
  PComplexArray = ^TComplexArray;

const ComplexZero : TComplex = (Re:0;Im:0);

function ComplexVal(const Re,Im: Float): TComplex; overload;
function ComplexVal(const Pt: TFloatPoint): TComplex; overload;

implementation

function ComplexVal(const Re,Im: Float): TComplex;
begin
  Result.Re:=Re;
  Result.Im:=Im;
end;

function ComplexVal(const Pt: TFloatPoint): TComplex;
begin
  Result.Re:=Pt.X;
  Result.Im:=Pt.Y;
end;

function TComplex.Modulus: Double;
begin
  Result:=Sqrt(Sqr(Re)+Sqr(Im));
end;

function TComplex.Power: Double;
begin
  Result:=Sqr(Re)+Sqr(Im);
end;

function TComplex.Angle: Double;
begin
  if Re=0 then
  begin
    if Im>=0 then Result:=Pi/2
    else Result:=-Pi/2;
  end
  else if Re>0 then
  begin
    if Im>=0 then Result:=arctan(Im/Re)
    else Result:=-arctan(-Im/Re);
  end
  else
  begin
    if Im>=0 then Result:=Pi-arctan(-Im/Re)
    else Result:=arctan(Im/Re)-Pi;
  end
end;

function TComplex.Conjugate: TComplex;
begin
  Result.Re:=Re;
  Result.Im:=-Im;
end;

function TComplex.Product(const Other: TComplex): TComplex;
begin
  Result.Re:=Re*Other.Re-Im*Other.Im;
  Result.Im:=Re*Other.Im+Im*Other.Re;
end;

function TComplex.Add(const Other: TComplex): TComplex;
begin
  Result.Re:=Re+Other.Re;
  Result.Im:=Im+Other.Im;
end;

function TComplex.Scale(const Value: Float): TComplex;
begin
  Result.Re:=Re*Value;
  Result.Im:=Im*Value;
end;

procedure TComplex.ScaleThis(const Value: Float);
begin
  Re:=Re*Value;
  Im:=Im*Value;
end;

end.

