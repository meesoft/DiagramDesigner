///////////////////////////////////////////////////////////////////////////////////////////////
//
// ExpressionEval.pas - Math expression evaluator
// ----------------------------------------------
// Version:   2003-12-25
// Maintain:  Michael Vinther  |  mv@logicnet·dk
//
// Last change:
//   Help, frac function
//   Translation
//
unit ExpressionEval;

interface

uses
  Classes, SysConst, SysUtils, StringLists, Math;

type
  EvalFloat = Extended;
  PEvalFloat = ^EvalFloat;

  TFloatFunction = function(const X: EvalFloat): EvalFloat;
  TSingleFunction = function(X: Single): Single;
  TDoubleFunction = function(const X: Double): Double;

  EExpressionError = class(Exception)
    public
      ErrorPosition : Integer;
      constructor Create(Msg: string; Position: Integer=0);
    end;

  TSymbol = class
    public
      SymbolType : (stFunction,stValue,stExpression,stSymbolicFunction);
    end;
  PSymbol = ^TSymbol;

  TSymbolTable = class(TStringObjectList)
    public
      // If false, undefined symbols are evaluated as 0
      ExceptionIfUndefined : Boolean;
      // Set StandardSymbols=True to define pi, sin, cos...
      constructor Create(StandardSymbols: Boolean=True);
      // Empty and add standard symbols
      procedure Reset;
      // Define new symbol, overwrite existing with same name
      function Define(const Name: string; Symbol: TSymbol): TSymbol;
      // Get symbol by name
      function Symbol(const Name: string; ExceptIfNotExists: Boolean=False): TSymbol;
      // Remove symbol by name
      procedure Remove(const Name: string);
    end;

  TSymbolValue = class(TSymbol)
    private
      FValue : EvalFloat;
    public
      constructor Create(const Value: EvalFloat=0); overload;
      constructor Create(Value: Boolean); overload;
      property Value: EvalFloat read FValue write FValue;
    end;
  TParameterValue = class(TSymbolValue);

  TSymbolFunction = class(TSymbol)
    public
      function Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat; virtual; abstract;
    end;

  TSymbolFunctionWithDescription = class(TSymbolFunction)
    private
      FDescription : string;
    public
      property Description: string read FDescription;
    end;

  TSymbolicFunction = class(TSymbol)
    public
      constructor Create; virtual;
      function Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat; virtual; abstract;
      function VarName: string; virtual; abstract;
      function Help: string; virtual; abstract;
    end;
  TSymbolicFunctionClass = class of TSymbolicFunction;

  TSymbolExpression = class(TSymbol)
    private
      FExpression : string;
      DefinedAt : Integer;
      Active : Boolean;
    public
      constructor Create(const Expression: string; Pos: Integer=1);
      function Evaluate(Table: TSymbolTable): EvalFloat; virtual;
      property Expression: string read FExpression write FExpression;
    end;

  TStandardFunction = class(TSymbolFunctionWithDescription)
    private
      F : TFloatFunction;
    public
      constructor Create(Func: TFloatFunction; const Description: string='');
      function Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat; override;
    end;

  TStandardFunctionS = class(TSymbolFunctionWithDescription)
    private
      F : TSingleFunction;
    public
      constructor Create(Func: TSingleFunction; const Description: string='');
      function Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat; override;
    end;

  TStandardFunctionD = class(TSymbolFunctionWithDescription)
    private
      F : TDoubleFunction;
    public
      constructor Create(Func: TDoubleFunction; const Description: string='');
      function Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat; override;
    end;

  TExpressionFunction = class(TSymbolFunction)
    private
      FExpression, FVarName : string;
      DefinedAt : Integer;
      Active : Boolean;
    public
      constructor Create(const VarName,Expression: string; Pos: Integer);
      function Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat; override;
      property Expression: string read FExpression;
      property VarName: string read FVarName;
    end;

  TOptimizationTask = (otArgMin,otMin,otRoot,otSolve);
  TOptimizationFunction = class(TSymbolicFunction)
    private
      Task : TOptimizationTask;
    public
      constructor Create(ATask: TOptimizationTask); reintroduce;
      function Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat; override;
      function VarName: string; override;
      function Help: string; override;
    end;

  TIfFunction = class(TSymbolicFunction)
    public
      //constructor Create
      function Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat; override;
      function VarName: string; override;
      function Help: string; override;
    end;


var
  DefaultDefines : TSymbolTable = nil;

// Math parser
function EvaluateExpression(const Expression: string; Defines: TSymbolTable=nil; MustReturnValue: Boolean=True): EvalFloat; overload;
// Add standard symbols to symbol table
procedure AddStandardSymbols(Table: TSymbolTable);
// Register new standard symbol
procedure RegisterStandardSymbol(const SymbolName: string; SymbolClassType: TSymbolicFunctionClass);

// Change first , to .
function ValStr(const Str: string): string;
// Remove SeparatorChars from beginning and end af string
function RemLeadTailSeparators(const S: string): string;

const
  NewSentenceChar = ';';

  SeparatorChars = [#9,#10,#13,' '];

  TextSymbolStartChars = ['A'..'Z','_','a'..'z'];
  TextSymbolChars = TextSymbolStartChars+['0'..'9','.'];

  ValueStartChars = ['$','0'..'9','.',',','-'];

resourcestring
  rsSExpected = '%s expected';
  rsNameExpected = 'Name expected';
  rsUndefinedSymbol = 'Undefined symbol:';
  rsOrExpected = '= or ( expected';
  rsInvalidNumberFormat = 'Invalid number format';
  rsOperatorExpected = 'Operator expected';
  rsUnexpectedCharacter = 'Unexpected character:';
  rsValueExpected = 'Value expected';
  rsExpressionIsEmpty = 'Expression is empty';
  rsRecursiveReference = 'Recursive reference';
  rsMax16BitsInBinaryNumber = 'Max 16 bits in binary number';
  rsInvalidBinaryNumber = 'Invalid binary number';
  rsUndefined = 'Undefined';
  rsUnableToEvaluateGuess = 'Unable to evaluate guess:';
  rsNoSolutionFoundTryChangingStartingGuess = 'No solution found, try changing starting guess';
  rsGuessUndefined = 'Guess undefined';
  rsSNotFound = '%s not found';
  rsExtraFound = 'Extra = found';
  rsFindXThatMinimizesExpression = 'Find x that minimizes expression';
  rsMinimumValueForExpressionInX = 'Minimum value for expression in x';
  rsRootOfExpressionInX = 'Root of expression in x';
  rsSolveEquationInX = 'Solve equation in x';
  rsBase10Logarithm = 'Base 10 logarithm';
  rsBase2Logarithm = 'Base 2 logarithm';
  rsBaseELogarithm = 'Base e logarithm';
  rsSquareRoot = 'Square root';
  rsRoundsUpTowardPositiveInfinity = 'Rounds up toward positive infinity';
  rsRoundsTowardNegativeInfinity = 'Rounds toward negative infinity';
  rsRoundToNearestInteger = 'Round to nearest integer';
  rsFractionalPartOfRealNumber = 'Fractional part of real number';
  rsRandomNumberIn0X = 'Random number in [0;x[';
  rsGaussianRandomNumberStdDevX = 'Gaussian random number, StdDev=x';
  rsConvertBinaryNumber = 'Convert binary number';
  rsConditionalValue = 'Conditional value';
  
implementation

{$IFNDEF VER130}
uses Types, MathUtils;
{$ENDIF}

var
  OperatorPriority : array[Char] of Integer;

procedure BuildOperators;
const
  Operators =                                            '|'+'&'+'='+'#'+'>'+'<'+'+'+'-'+'/'+'*'+'%'+'^';
  Priorities : array[1..Length(Operators)] of Integer = ( 1 , 1 , 2 , 2 , 2 , 2 , 3 , 3 , 4 , 4 , 4 , 5);
var
  I : Integer;
begin
  for I:=1 to Length(Operators) do OperatorPriority[Operators[I]]:=Priorities[I];
end;

function EvaluateExpression(const Expression: string; Defines: TSymbolTable; MustReturnValue: Boolean): EvalFloat;

var
  Len : Integer;
  Pos : Integer;

  procedure SkipCharacter(Ch: Char);
  begin
    if Expression[Pos]<>Ch then raise EExpressionError.Create(Format(rsSExpected,[Ch]),Pos);
    Inc(Pos);
  end;

  procedure SkipSeperators;
  begin
    while (Pos<=Len) and (Expression[Pos] in SeparatorChars) do Inc(Pos);
  end;

  function GetValue({; AllowComma: Boolean=True}): EvalFloat;
  var T, V : Integer;
  begin
    T:=Pos;
    Inc(Pos);
    if Expression[T]='$' then // Hex
    begin
      while (Pos<=Len) and (Expression[Pos] in ['0'..'9','A'..'F','a'..'f']) do Inc(Pos);
      Val(Copy(Expression,T,Pos-T),V,T);
      Result:=V;
    end
    else //if AllowComma then
    begin
      while (Pos<=Len) and
            ((Expression[Pos] in ['0'..'9','.',',','e','E']) or
             ((Expression[Pos] in ['+','-']) and (Expression[Pos-1] in ['e','E']))) // 3e-4
             do Inc(Pos);
      Val(ValStr(Copy(Expression,T,Pos-T)),Result,T);
    end
    {else
    begin
      while (Expression[Pos] in ['0'..'9','.','E','e']) and (Pos<=Len) do Inc(Pos);
      Val(Copy(Expression,T,Pos-T),Result,T);
    end{};
    if T<>0 then raise EExpressionError.Create(rsInvalidNumberFormat,Pos);
  end;

  function Evaluate: EvalFloat; forward;

  function EvaluateIdentifier: EvalFloat;
  var
    Start, I : Integer;
    Symbol : TSymbol;
  begin
    Start:=Pos;
    Inc(Pos);
    while (Expression[Pos] in TextSymbolChars) and (Pos<=Len) do Inc(Pos);
    I:=Defines.IndexOf(Copy(Expression,Start,Pos-Start));
    if I>=0 then
    begin
      Symbol:=TSymbol(Defines.Objects[I]);
      case Symbol.SymbolType of
        stValue      : Result:=TSymbolValue(Symbol).Value;
        stFunction   : begin
                         SkipCharacter('(');
                         try
                           Result:=TSymbolFunction(Symbol).Evaluate(Evaluate,Defines);
                         except
                           on Error: Exception do raise EExpressionError.Create(Error.Message,Pos-1);
                         end
                       end;
        stSymbolicFunction :
                       begin
                         SkipCharacter('(');
                         SkipSeperators;
                         try
                           Result:=TSymbolicFunction(Symbol).Evaluate(Expression,Pos,Defines);
                         except
                           on EExpressionError do raise;
                           on Error: Exception do raise EExpressionError.Create(Error.Message,Pos-1);
                         end;
                         SkipSeperators;
                         SkipCharacter(')');
                       end;
        stExpression : Result:=TSymbolExpression(Symbol).Evaluate(Defines);

        else raise EExpressionError.Create('Unknown symbol type: '+Defines[I],Pos);
      end
    end
    else if Defines.ExceptionIfUndefined then raise EExpressionError.Create(rsUndefinedSymbol+' '+Copy(Expression,Start,Pos-Start),Pos)
    else Result:=0;
  end;

  procedure DefineSymbol;
  var
    Name, VarName : string;
    Start : Integer;
  begin
    SkipSeperators;
    if not (Expression[Pos] in TextSymbolStartChars) then raise EExpressionError.Create(rsNameExpected,Pos);
    Start:=Pos;
    Inc(Pos);
    while (Expression[Pos] in TextSymbolChars) and (Pos<=Len) do Inc(Pos);
    Name:=Copy(Expression,Start,Pos-Start);
    SkipSeperators;
    case Expression[Pos] of
      '=' : begin
              Inc(Pos);
              Defines.Define(Name,TSymbolValue.Create(Evaluate));
            end;
      '(' : begin
              Inc(Pos);
              SkipSeperators;
              if Expression[Pos]=')' then // Expression
              begin
                Inc(Pos);
                SkipSeperators;
                SkipCharacter('=');
                SkipSeperators;
                Start:=Pos;
                while (Expression[Pos]<>NewSentenceChar) and (Pos<=Len) do Inc(Pos);
                Inc(Pos);
                Defines.Define(Name,TSymbolExpression.Create(Copy(Expression,Start,Pos-Start),Start));
              end
              else // User function
              begin
                Start:=Pos;
                if not (Expression[Pos] in TextSymbolStartChars) then raise EExpressionError.Create(rsNameExpected,Pos);
                Inc(pos);
                while (Expression[Pos] in TextSymbolChars) and (Pos<=Len) do Inc(Pos);
                VarName:=Copy(Expression,Start,Pos-Start);
                SkipSeperators;
                SkipCharacter(')');
                SkipSeperators;
                SkipCharacter('=');
                SkipSeperators;
                Start:=Pos;
                while (Expression[Pos]<>NewSentenceChar) and (Pos<=Len) do Inc(Pos);
                Inc(Pos);
                Defines.Define(Name,TExpressionFunction.Create(VarName,Copy(Expression,Start,Pos-Start),Start));
              end;
            end;
    else raise EExpressionError.Create(rsOrExpected,Pos);
    end;
  end;

  function Evaluate: EvalFloat;
  var
    Stack : array[0..10] of record
                              Operator : Char;
                              Value    : EvalFloat;
                            end;
    StackTop : Integer;

    procedure ProcessStack(MinPriority: Integer);
    var
      V1, V2 : PEvalFloat;
    begin
      while OperatorPriority[Stack[StackTop].Operator]>=MinPriority do
      begin
        V1:=@Stack[StackTop-1].Value;
        V2:=@Stack[StackTop].Value;
        case Stack[StackTop].Operator of
          '+' : V1^:=V1^+V2^;
          '-' : V1^:=V1^-V2^;
          '*' : V1^:=V1^*V2^;
          '/' : V1^:=V1^/V2^;
          '%' : V1^:=Frac(V1^/V2^)*V2^;
          '^' : V1^:=Power(V1^,V2^);
          '=' : V1^:=Byte(SameValue(V1^,V2^));
          '#' : V1^:=Byte(not SameValue(V1^,V2^));
          '>' : V1^:=Byte(CompareValue(V1^,V2^)=GreaterThanValue);
          '<' : V1^:=Byte(CompareValue(V1^,V2^)=LessThanValue);
          '|' : V1^:=Byte((V1^<>0) or (V2^<>0));
          '&' : V1^:=Byte((V1^<>0) and (V2^<>0));
        end;
        if IsNan(V1^) then raise EExpressionError.Create(SInvalidOp,Pos);
        Dec(StackTop);
      end;
    end;

  var
    Operator : Char;
  begin
    StackTop:=0;
    Stack[0].Operator:=' ';
    Stack[0].Value:=0;
    Operator:='+';
    SkipSeperators;
    repeat
      if Expression[Pos]=':' then // Define
      begin
        Inc(Pos);
        DefineSymbol;
        SkipSeperators;
        Continue;
      end;

      if OperatorPriority[Expression[Pos]]=0 then
      begin
        if StackTop<>0 then
          raise EExpressionError.Create(rsOperatorExpected,Pos);
      end
      else
      begin
        Operator:=Expression[Pos];
        {if (Operator='/') and (Expression[Pos+1]='/') then // Line comment
        begin
          Inc(Pos,2);
          while (Pos<=Len) and (Expression[Pos]<>#13) do Inc(Pos);
          SkipSeperators;
          Continue;
        end;}
        if (StackTop=0) and not (Operator in ['+','-']) then
          raise EExpressionError.Create(rsUnexpectedCharacter+' '+Expression[Pos],Pos);
        Inc(Pos);
        SkipSeperators;
      end;

      if Expression[Pos] in TextSymbolStartChars then // Name
      begin
        Result:=EvaluateIdentifier;
      end
      else if Expression[Pos] in ValueStartChars then // Number
      begin
        Result:=GetValue;
      end
      else if Expression[Pos]='(' then // ( )
      begin
        Inc(Pos);
        Result:=Evaluate;
      end
      else
      begin
        if Pos>Len then raise EExpressionError.Create(rsValueExpected,Pos);
        raise EExpressionError.Create(rsUnexpectedCharacter+' '+Expression[Pos],Pos);
      end;
      ProcessStack(OperatorPriority[Operator]);
      Inc(StackTop);
      Stack[StackTop].Operator:=Operator;
      Stack[StackTop].Value:=Result;
      SkipSeperators;
    until (Pos>Len) or (Expression[Pos] in [')',NewSentenceChar]);
    if StackTop=0 then
    begin
      if MustReturnValue then raise EExpressionError.Create(rsValueExpected,Pos);
      Result:=NaN;
    end
    else
    begin
      ProcessStack(1);
      Result:=Stack[0].Value;
    end;
    Inc(Pos);
  end;

var
  PrevFPUControlWord : Word;
begin
  Len:=Length(Expression);
  if Len=0 then raise EExpressionError.Create(rsExpressionIsEmpty);
  if Defines=nil then Defines:=DefaultDefines;
  if OperatorPriority['+']=0 then BuildOperators;
  Pos:=1;
  PrevFPUControlWord:=Get8087CW;
  SetExceptionMask([exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision, exInvalidOp]);
  SetPrecisionMode(pmExtended);
  try
    Result:=Evaluate;
  finally
    Set8087CW(PrevFPUControlWord);
  end;
end;

function ValStr(const Str: string): string;
var I : Integer;
begin
  Result:=Str;
  I:=Pos(',',Result);
  if I<>0 then Result[I]:='.';
end;

function RemLeadTailSeparators(const S: string): string;
var P: Integer;
begin
  Result:=S;
  P:=Length(Result);
  while (P>0) and (Result[P] in SeparatorChars) do Dec(P);
  SetLength(Result,P);
  P:=1;
  while (P<=Length(Result)) and (Result[P] in SeparatorChars) do Inc(P);
  Delete(Result,1,P-1);
end;

//=======================================================================================================
// EExpressionError
//=======================================================================================================
constructor EExpressionError.Create(Msg: string; Position: Integer);
begin
  ErrorPosition:=Position;
  inherited Create(Msg);
end;

//=======================================================================================================
// TSymbolTable
//=======================================================================================================

constructor TSymbolTable.Create(StandardSymbols: Boolean);
begin
  inherited Create;
  Sorted:=True;
  Duplicates:=dupIgnore;
  AddStandardSymbols(Self);
end;

procedure TSymbolTable.Reset;
begin
  Clear;
  AddStandardSymbols(Self);
end;

function TSymbolTable.Define(const Name: string; Symbol: TSymbol): TSymbol;
var
  I : Integer;
begin
  I:=IndexOf(Name);
  if I<0 then AddObject(Name,Symbol) // New symbol
  else
  begin // Replace symbol
    Objects[I].Free;
    Objects[I]:=Symbol;
  end;
  Result:=Symbol;
end;

function TSymbolTable.Symbol(const Name: string; ExceptIfNotExists: Boolean): TSymbol;
var
  I : Integer;
begin
  I:=IndexOf(Name);
  if I<0 then
  begin
    if ExceptIfNotExists then raise EExpressionError.Create(rsUndefinedSymbol+' '+Name);
    Result:=nil;
  end
  else Result:=TSymbol(Objects[I]);
end;

procedure TSymbolTable.Remove(const Name: string);
var
  I : Integer;
begin
  I:=IndexOf(Name);
  if I<0 then raise EExpressionError.Create(rsUndefinedSymbol+' '+Name);
  Delete(I);
end;

//=======================================================================================================
// TSymbolValue
//=======================================================================================================
constructor TSymbolValue.Create(const Value: EvalFloat);
begin
  inherited Create;
  SymbolType:=stValue;
  FValue:=Value;
end;

constructor TSymbolValue.Create(Value: Boolean); 
begin
  Create(Byte(Value));
end;

//=======================================================================================================
// TSymbolicFunction
//=======================================================================================================
constructor TSymbolicFunction.Create;
begin
  inherited Create;
  SymbolType:=stSymbolicFunction;
end;

//=======================================================================================================
// TSymbolExpression
//=======================================================================================================
constructor TSymbolExpression.Create(const Expression: string; Pos: Integer);
var I : Integer;
begin
  inherited Create;
  SymbolType:=stExpression;
  DefinedAt:=Pos;
  FExpression:=RemLeadTailSeparators(Expression);
  if FExpression='' then raise EExpressionError.Create(rsExpressionIsEmpty,Pos)
  else if FExpression[Length(FExpression)]=';' then SetLength(FExpression,Length(FExpression)-1);
  for I:=1 to Length(FExpression) do if FExpression[I] in SeparatorChars then FExpression[I]:=' ';
end;

function TSymbolExpression.Evaluate(Table: TSymbolTable): EvalFloat;
begin
  if Active then raise EExpressionError.Create(rsRecursiveReference);
  Active:=True;
  try
    Result:=EvaluateExpression(FExpression,Table);
  finally
    Active:=False;
  end;
end;

//=======================================================================================================
// TExpressionFunction
//=======================================================================================================
constructor TExpressionFunction.Create(const VarName,Expression: string; Pos: Integer);
var I : Integer;
begin
  inherited Create;
  SymbolType:=stFunction;
  DefinedAt:=Pos;
  FVarName:=VarName;
  FExpression:=RemLeadTailSeparators(Expression);
  if FExpression='' then raise EExpressionError.Create(rsExpressionIsEmpty,Pos)
  else if FExpression[Length(FExpression)]=';' then SetLength(FExpression,Length(FExpression)-1);
  for I:=1 to Length(FExpression) do if FExpression[I] in SeparatorChars then FExpression[I]:=' ';
end;

function TExpressionFunction.Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat;
var
  I : Integer;
  ExistingSymbol : TSymbol;
begin
  if Active then raise EExpressionError.Create(rsRecursiveReference);
  I:=Table.IndexOf(VarName);
  if I=-1 then
  begin
    ExistingSymbol:=nil;
    Table.AddObject(VarName,TSymbolValue.Create(Param));
  end
  else
  begin
    ExistingSymbol:=TSymbol(Table.Objects[I]);
    Table.Objects[I]:=TSymbolValue.Create(Param);
  end;
  Active:=True;
  try
    Result:=EvaluateExpression(Expression,Table);
  finally
    Active:=False;
    if Assigned(ExistingSymbol) then Table.Define(VarName,ExistingSymbol)
    else Table.Remove(VarName);
  end;
end;

//=======================================================================================================
// TStandardFunctions
//=======================================================================================================
constructor TStandardFunction.Create(Func: TFloatFunction; const Description: string);
begin
  inherited Create;
  FDescription:=Description;
  SymbolType:=stFunction;
  F:=Func;
end;

function TStandardFunction.Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat;
begin
  Result:=F(Param);
end;

constructor TStandardFunctionS.Create(Func: TSingleFunction; const Description: string);
begin
  inherited Create;
  FDescription:=Description;
  SymbolType:=stFunction;
  F:=Func;
end;

constructor TStandardFunctionD.Create(Func: TDoubleFunction; const Description: string);
begin
  inherited Create;
  FDescription:=Description;
  SymbolType:=stFunction;
  F:=Func;
end;

function TStandardFunctionD.Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat;
begin
  Result:=F(Param);
end;

function TStandardFunctionS.Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat;
begin
  Result:=F(Param);
end;

function FSin(const X: EvalFloat): EvalFloat;
begin
  Result:=Sin(X);
end;

function FCos(const X: EvalFloat): EvalFloat;
begin
  Result:=Cos(X);
end;

function FArcTan(const X: EvalFloat): EvalFloat;
begin
  Result:=ArcTan(X);
end;

function FSqrt(const X: EvalFloat): EvalFloat;
begin
  Result:=Sqrt(X);
end;

function FExp(const X: EvalFloat): EvalFloat;
begin
  Result:=Exp(X);
end;

function FLn(const X: EvalFloat): EvalFloat;
begin
  Result:=Ln(X);
end;

function FCeil(const X: EvalFloat): EvalFloat;
begin
  Result:=Ceil(X);
end;

function FFloor(const X: EvalFloat): EvalFloat;
begin
  Result:=Floor(X);
end;

function FRound(const X: EvalFloat): EvalFloat;
begin
  Result:=Round(X);
end;

function FFrac(const X: EvalFloat): EvalFloat;
begin
  Result:=Frac(X);
end;

function FAbs(const X: EvalFloat): EvalFloat;
begin
  Result:=Abs(X);
end;

function FSign(const X: EvalFloat): EvalFloat;
begin
  Result:=Sign(X);
end;

function FRand(const X: EvalFloat): EvalFloat;
begin
  Result:=Random*X;
end;

function FRandN(const X: EvalFloat): EvalFloat;
begin
  Result:=RandG(0,X);
end;

function FBin(const X: EvalFloat): EvalFloat;
var
  I, B : Integer;
  S : string;
begin
  Str(X:0:0,S);
  if Length(S)>16 then raise Exception.Create(rsMax16BitsInBinaryNumber);
  B:=0;
  for I:=1 to Length(S) do
  begin
    if not (S[I] in ['0','1']) then raise Exception.Create(rsInvalidBinaryNumber);
    B:=(B shl 1) or Integer((S[I]='1'));
  end;
  Result:=B;
end;

function FFactorial(const X: EvalFloat): EvalFloat;
var
  I : Integer;
begin
  if (X<0) or (Abs(X-Round(X))>1e-6) then raise Exception.Create(rsUndefined);
  Result:=1;
  for I:=Round(X) downto 2 do Result:=Result*I;
end;

{$IFDEF VER130}
  function ArcCos(const X: Extended): Extended;
  begin
    Result := ArcTan2(Sqrt(1 - X*X), X);   
  end;

  function ArcSin(const X: Extended): Extended;
  begin
    Result := ArcTan2(X, Sqrt(1 - X*X))
  end;

  function Tan(const X: Extended): Extended;
  {  Tan := Sin(X) / Cos(X) }
  asm
          FLD    X
          FPTAN
          FSTP   ST(0)      { FPTAN pushes 1.0 after result }
          FWAIT
  end;

  function CoTan(const X: Extended): Extended;
  { CoTan := Cos(X) / Sin(X) = 1 / Tan(X) }
  asm
          FLD   X
          FPTAN
          FDIVRP
          FWAIT
  end;

  function CoshSinh(X: Extended; Factor: Double): Extended;
  begin
    Result := Exp(X) / 2;
    Result := Result + Factor / Result;
  end;

  function Cosh(const X: Extended): Extended;
  begin
    Result := CoshSinh(X, 0.25)
  end;

  function Sinh(const X: Extended): Extended;
  begin
    Result := CoshSinh(X, -0.25)
  end;

  const
    MaxTanhDomain = 5678.22249441322; // Ln(MaxExtended)/2

  function Tanh(const X: Extended): Extended;
  begin
    if X > MaxTanhDomain then
      Result := 1.0
    else if X < -MaxTanhDomain then
      Result := -1.0
    else
    begin
      Result := Exp(X);
      Result := Result * Result;
      Result := (Result - 1.0) / (Result + 1.0)
    end;
  end;

  function ArcCosh(const X: Extended): Extended;
  begin
    Result := Ln(X + Sqrt((X - 1) / (X + 1)) * (X + 1));
  end;

  function ArcSinh(const X: Extended): Extended;
  begin
    Result := Ln(X + Sqrt((X * X) + 1));
  end;

  function ArcTanh(const X: Extended): Extended;
  begin
    if X=1 then
      Result := 1/0
    else if X=-1 then
      Result := -1/0
    else
      Result := 0.5 * Ln((1 + X) / (1 - X));
  end;

  function Log10(const X: Extended): Extended;
    { Log.10(X) := Log.2(X) * Log.10(2) }
  asm
          FLDLG2     { Log base ten of 2 }
          FLD     X
          FYL2X
          FWAIT
  end;

  function Log2(const X: Extended): Extended;
  asm
          FLD1
          FLD     X
          FYL2X
          FWAIT
  end;
{$ENDIF}

//=======================================================================================================
// TOptimizationFunction
//=======================================================================================================

const OptimizationPrefix = 'num.';

constructor TOptimizationFunction.Create(ATask: TOptimizationTask);
begin
  inherited Create;
  Task:=ATask;
end;

function TOptimizationFunction.Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat;
var
  Symbolic : string;
  xSymbol : TSymbolValue;
  Start : Integer;

  function Minimize(FindZero,EvaluateOutput: Boolean): EvalFloat;
  var
    x, F, d, Fnew : EvalFloat;
    I : Integer;
    xNew : ^EvalFloat;
  begin
    x:=xSymbol.Value;
    try
      F:=EvaluateExpression(Symbolic,Table);
      if FindZero then F:=Abs(F);
    except
      on Error: EExpressionError do raise EExpressionError.Create(rsUnableToEvaluateGuess+' '+Error.Message,Start+Error.ErrorPosition);
    end;
    d:=1;
    xNew:=@xSymbol.Value;
    for I:=1 to 5000 do
    begin
      xNew^:=x+d;
      if xNew^=x then Break; // d less than working precision
      try
        Fnew:=EvaluateExpression(Symbolic,Table);
        if FindZero then Fnew:=Abs(Fnew);
      except
        Fnew:=F;
      end;
      if Fnew<F then
      begin
        x:=xNew^;
        d:=d*1.1;
        F:=Fnew;
      end
      else
      begin
        xNew^:=x-d;
        try
          Fnew:=EvaluateExpression(Symbolic,Table);
          if FindZero then Fnew:=Abs(Fnew);
        except
          Fnew:=F;
        end;
        if Fnew<F then
        begin
          x:=xNew^;
          d:=-1.1*d; // Try other direction first
          F:=Fnew;
        end
        else d:=d/2;
      end
    end;
    if EvaluateOutput then Result:=F
    else if FindZero and (F>1e-5) then raise EExpressionError.Create(rsNoSolutionFoundTryChangingStartingGuess,Pos)
    else Result:=x;
  end;

var
  Level, VarStart, I : Integer;
  ExistingSymbol : TSymbol;
  VarName : string;
begin
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

  ExistingSymbol:=Table.Symbol(OptimizationPrefix+'Guess');
  if not (ExistingSymbol is TSymbolValue) then raise EExpressionError.Create(rsGuessUndefined,Pos);
  xSymbol:=TSymbolValue.Create(TSymbolValue(ExistingSymbol).Value);
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
    Result:=0;
    case Task of
      otArgMin : Result:=Minimize(False,False);
      otMin    : Result:=Minimize(False,True);
      otRoot   : Result:=Minimize(True,False);
      otSolve  : begin
                   I:=System.Pos('=',Symbolic);
                   if I=0 then raise EExpressionError.Create(Format(rsSNotFound,['=']),VarStart-2);
                   Symbolic:='('+Copy(Symbolic,1,I-1)+')-('+Copy(Symbolic,I+1,High(Integer))+')';
                   if System.Pos('=',Symbolic)<>0 then raise EExpressionError.Create(rsExtraFound,Start+I);
                   Dec(Start,3);
                   Result:=Minimize(True,False);
                 end;
    end;
    if Abs(Result)<1e-100 then Result:=0
    else if Result>1e100 then Result:=1/0 // INF
    else if Result<-1e100 then Result:=-1/0; // -INF
  finally
    if Assigned(ExistingSymbol) then Table.Define(VarName,ExistingSymbol)
    else Table.Remove(VarName);
  end;
end;

function TOptimizationFunction.VarName: string;
begin
  if Task=otSolve then Result:='equation,x'
  else Result:='expression,x';
end;

function TOptimizationFunction.Help: string;
begin
  case Task of
    otArgMin : Result:=rsFindXThatMinimizesExpression;
    otMin    : Result:=rsMinimumValueForExpressionInX;
    otRoot   : Result:=rsRootOfExpressionInX;
    otSolve  : Result:=rsSolveEquationInX;
  end;
end;

//=======================================================================================================
// TIfFunction
//=======================================================================================================

function TIfFunction.Evaluate(const Expression: string; var Pos: Integer; Table: TSymbolTable): EvalFloat;
var
  Level, CondStart, TrueValStart, FalseValStart : Integer;
begin
  CondStart:=Pos;    
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
  Inc(Pos);
  TrueValStart:=Pos;
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
  Inc(Pos);
  FalseValStart:=Pos;
  while (Pos<=Length(Expression)) and (Level>=0) do
  begin
    if Expression[Pos]='(' then Inc(Level)
    else if Expression[Pos]=')' then Dec(Level);
    Inc(Pos);
  end;
  if Pos=FalseValStart then raise EExpressionError.Create(Format(rsSExpected,[')']),Pos);
  Dec(Pos);
  if EvaluateExpression(Copy(Expression,CondStart,TrueValStart-CondStart-1),Table)<>0 then
  try
    Result:=EvaluateExpression(Copy(Expression,TrueValStart,FalseValStart-TrueValStart-1),Table)
  except
    on E: EExpressionError do
      raise EExpressionError.Create(E.Message,TrueValStart+E.ErrorPosition-1);
  end
  else
  try
    Result:=EvaluateExpression(Copy(Expression,FalseValStart,Pos-FalseValStart),Table);
  except
    on E: EExpressionError do
      raise EExpressionError.Create(E.Message,FalseValStart+E.ErrorPosition-1);
  end
end;

function TIfFunction.Help: string;
begin
  Result:=rsConditionalValue;
end;

function TIfFunction.VarName: string;
begin
  Result:='condition,trueVal,falseVal';
end;

//=======================================================================================================
var
  ExtraStandardSymbols : array of record
                           Name : string;
                           ClassType : TSymbolicFunctionClass;
                         end;

procedure RegisterStandardSymbol(const SymbolName: string; SymbolClassType: TSymbolicFunctionClass);
begin
  SetLength(ExtraStandardSymbols,Length(ExtraStandardSymbols)+1);
  with ExtraStandardSymbols[High(ExtraStandardSymbols)] do
  begin
    Name:=SymbolName;
    ClassType:=SymbolClassType;
  end;
  if DefaultDefines<>nil then DefaultDefines.AddObject(SymbolName,SymbolClassType.Create);
end;

procedure AddStandardSymbols(Table: TSymbolTable);
var
  I : Integer;
begin
  Table.AddObject('pi',TSymbolValue.Create(Pi));
  Table.AddObject('e',TSymbolValue.Create(Exp(1)));
  Table.AddObject('inf',TSymbolValue.Create(1/0));
  Table.AddObject('kb',TSymbolValue.Create(1 shl 10));
  Table.AddObject('Mb',TSymbolValue.Create(1 shl 20));
  Table.AddObject('Gb',TSymbolValue.Create(1 shl 30));

  Table.AddObject('sin',TStandardFunction.Create(FSin));
  Table.AddObject('cos',TStandardFunction.Create(FCos));
  Table.AddObject('arctan',TStandardFunction.Create(FArcTan));
  Table.AddObject('arccos',TStandardFunction.Create(ArcCos));
  Table.AddObject('arcsin',TStandardFunction.Create(ArcSin));
  Table.AddObject('tan',TStandardFunction.Create(Tan));
  Table.AddObject('cot',TStandardFunction.Create(Cotan));

  Table.AddObject('sinh',TStandardFunction.Create(Sinh));
  Table.AddObject('cosh',TStandardFunction.Create(Cosh));
  Table.AddObject('tanh',TStandardFunction.Create(Tanh));
  Table.AddObject('arccosh',TStandardFunction.Create(ArcCosh));
  Table.AddObject('arcsinh',TStandardFunction.Create(ArcSinh));
  Table.AddObject('arctanh',TStandardFunction.Create(ArcTanh));

  Table.AddObject('log10',TStandardFunction.Create(Log10,rsBase10Logarithm));
  Table.AddObject('log2',TStandardFunction.Create(Log2,rsBase2Logarithm));

  Table.AddObject('ln',TStandardFunction.Create(FLn,rsBaseELogarithm));
  Table.AddObject('sqrt',TStandardFunction.Create(FSqrt,rsSquareRoot));
  Table.AddObject('exp',TStandardFunction.Create(FExp,'e^x'));
  Table.AddObject('fac',TStandardFunction.Create(FFactorial,'x!'));

  Table.AddObject('abs',TStandardFunction.Create(FAbs,'|x|'));
  Table.AddObject('sign',TStandardFunction.Create(FSign));
  Table.AddObject('ceil',TStandardFunction.Create(FCeil,rsRoundsUpTowardPositiveInfinity));
  Table.AddObject('floor',TStandardFunction.Create(FFloor,rsRoundsTowardNegativeInfinity));
  Table.AddObject('round',TStandardFunction.Create(FRound,rsRoundToNearestInteger));
  Table.AddObject('frac',TStandardFunction.Create(FFrac,rsFractionalPartOfRealNumber));

  Table.AddObject('rand',TStandardFunction.Create(FRand,rsRandomNumberIn0X));
  Table.AddObject('randn',TStandardFunction.Create(FRandN,rsGaussianRandomNumberStdDevX));

  Table.AddObject('bin',TStandardFunction.Create(FBin,rsConvertBinaryNumber));

  Table.AddObject(OptimizationPrefix+'argmin',TOptimizationFunction.Create(otArgMin));
  Table.AddObject(OptimizationPrefix+'min',TOptimizationFunction.Create(otMin));
  Table.AddObject(OptimizationPrefix+'root',TOptimizationFunction.Create(otRoot));
  Table.AddObject(OptimizationPrefix+'solve',TOptimizationFunction.Create(otSolve));
  Table.AddObject(OptimizationPrefix+'Guess',TParameterValue.Create(0.500000001));

  Table.AddObject('if',TIfFunction.Create);

  for I:=0 to High(ExtraStandardSymbols) do
    with ExtraStandardSymbols[I] do Table.AddObject(Name,ClassType.Create);
end;

initialization
  DefaultDefines:=TSymbolTable.Create;
  DefaultDefines.ExceptionIfUndefined:=True;
finalization
  DefaultDefines.Free;
end.

