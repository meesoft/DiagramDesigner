unit FastcodeCeilUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Fastcode
 *
 * The Initial Developer of the Original Code is Fastcode
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow, Dennis Christensen
 *
 * ***** END LICENSE BLOCK ***** *)

//Last update 15/2 2005
//Version : 1.0
//Only plain function calls supported

interface

function CeilFastcodeP4P(const X : Extended) : Int64; overload;
function CeilFastcodeP4P(const X : Double) : Int64; overload;
function CeilFastcodeP4P(const X : Single) : Int64; overload;
function CeilFastcodeP4N(const X : Extended) : Int64; overload;
function CeilFastcodeP4N(const X : Double) : Int64; overload;
function CeilFastcodeP4N(const X : Single) : Int64; overload;
function CeilFastcodePMB(const X : Extended) : Int64; overload;
function CeilFastcodePMB(const X : Double) : Int64; overload;
function CeilFastcodePMB(const X : Single) : Int64; overload;
function CeilFastcodeP3(const X : Extended) : Int64; overload;
function CeilFastcodeP3(const X : Double) : Int64; overload;
function CeilFastcodeP3(const X : Single) : Int64; overload;
function CeilFastcodeAMD64(const X : Extended) : Int64; overload;
function CeilFastcodeAMD64(const X : Double) : Int64; overload;
function CeilFastcodeAMD64(const X : Single) : Int64; overload;
function CeilFastcodeXP(const X : Extended) : Int64; overload;
function CeilFastcodeXP(const X : Double) : Int64; overload;
function CeilFastcodeXP(const X : Single) : Int64; overload;
function CeilFastcodeBlended(const X : Extended) : Int64; overload;
function CeilFastcodeBlended(const X : Double) : Int64; overload;
function CeilFastcodeBlended(const X : Single) : Int64; overload;
function CeilFastcodeRTL(const X : Extended) : Int64; overload;
function CeilFastcodeRTL(const X : Double) : Int64; overload;
function CeilFastcodeRTL(const X : Single) : Int64; overload;
function CeilFastcodePascal(const X : Extended) : Int64; overload;
function CeilFastcodePascal(const X : Double) : Int64; overload;
function CeilFastcodePascal(const X : Single) : Int64; overload;

implementation

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 4 Prescott
//Instructionset(s): IA32
//Original Name:     CeilExtendedJOH

function CeilFastcodeP4P(const X : Extended) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 4 Prescott
//Instructionset(s): IA32
//Original Name:     CeilDoubleJOH

function CeilFastcodeP4P(const X : Double) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            Dennis Kjaer Christensen
//Optimized for:     Intel Pentium 4 Prescott
//Instructionset(s): IA32
//Original Name:     CeilSingleDKC1

function CeilFastcodeP4P(const X : Single) : Int64;
asm
 fnstcw [esp-12].Word         //Get current controlword
 mov    ax, [esp-12]          //into eax
 or     ax, 0000100000000000B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 and    ax, 1111101111111111B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 mov    [esp-8], ax
 fldcw  word ptr [esp-8]
 fld    X
 fistp  qword ptr [esp-8]
 mov    eax, [esp-8]
 mov    edx, [esp-4]
 fldcw  word ptr [esp-12]     //Restore controlword
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 4 Northwood
//Instructionset(s): IA32
//Original Name:     CeilExtendedJOH

function CeilFastcodeP4N(const X : Extended) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 4 Northwood
//Instructionset(s): IA32
//Original Name:     CeilDoubleJOH

function CeilFastcodeP4N(const X : Double  ) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            Dennis Kjaer Christensen
//Optimized for:     Intel Pentium 4 Northwood
//Instructionset(s): IA32
//Original Name:     CeilSingleDKC1

function CeilFastcodeP4N(const X : Single) : Int64;
asm
 fnstcw [esp-12].Word         //Get current controlword
 mov    ax, [esp-12]          //into eax
 or     ax, 0000100000000000B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 and    ax, 1111101111111111B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 mov    [esp-8], ax
 fldcw  word ptr [esp-8]
 fld    X
 fistp  qword ptr [esp-8]
 mov    eax, [esp-8]
 mov    edx, [esp-4]
 fldcw  word ptr [esp-12]     //Restore controlword
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium M Banias
//Instructionset(s): IA32
//Original Name:     CeilExtendedJOH

function CeilFastcodePMB(const X : Extended) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium M Banias
//Instructionset(s): IA32
//Original Name:     CeilDoubleJOH

function CeilFastcodePMB(const X : Double) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium M Banias
//Instructionset(s): IA32
//Original Name:     CeilSingleJOH

function CeilFastcodePMB(const X : Single) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 3
//Instructionset(s): IA32
//Original Name:     CeilExtendedJOH

function CeilFastcodeP3(const X : Extended) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 3
//Instructionset(s): IA32
//Original Name:     CeilDoubleJOH

function CeilFastcodeP3(const X : Double  ) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 3
//Instructionset(s): IA32
//Original Name:     CeilSingleJOH

function CeilFastcodeP3(const X : Single) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     AMD 64 Opteron, FX, XP
//Instructionset(s): IA32
//Original Name:     CeilExtendedJOH

function CeilFastcodeAMD64(const X : Extended) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            Dennis Kjaer Christensen
//Optimized for:     AMD 64 Opteron, FX, XP
//Instructionset(s): IA32
//Original Name:     CeilDoubleDKC1

function CeilFastcodeAMD64(const X : Double) : Int64;
asm
 fnstcw [esp-12].Word         //Get current controlword
 mov    ax, [esp-12]          //into eax
 or     ax, 0000100000000000B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 and    ax, 1111101111111111B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 mov    [esp-8], ax
 fldcw  word ptr [esp-8]
 fld    X
 fistp  qword ptr [esp-8]
 mov    eax, [esp-8]
 mov    edx, [esp-4]
 fldcw  word ptr [esp-12]     //Restore controlword
end;

//Author:            Dennis Kjaer Christensen
//Optimized for:     AMD 64 Opteron, FX, XP
//Instructionset(s): IA32
//Original Name:     CeilSingleDKC1

function CeilFastcodeAMD64(const X : Single) : Int64;
asm
 fnstcw [esp-12].Word         //Get current controlword
 mov    ax, [esp-12]          //into eax
 or     ax, 0000100000000000B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 and    ax, 1111101111111111B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 mov    [esp-8], ax
 fldcw  word ptr [esp-8]
 fld    X
 fistp  qword ptr [esp-8]
 mov    eax, [esp-8]
 mov    edx, [esp-4]
 fldcw  word ptr [esp-12]     //Restore controlword
end;

//Author:            John O'Harrow
//Optimized for:     AMD Athlon XP
//Instructionset(s): IA32
//Original Name:     CeilExtendedJOH

function CeilFastcodeXP(const X : Extended) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     AMD Athlon XP
//Instructionset(s): IA32
//Original Name:     CeilDoubleJOH

function CeilFastcodeXP(const X : Double  ) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            Dennis Kjaer Christensen
//Optimized for:     AMD Athlon XP
//Instructionset(s): IA32
//Original Name:     CeilSingleDKC1

function CeilFastcodeXP(const X : Single) : Int64;
asm
 fnstcw [esp-12].Word         //Get current controlword
 mov    ax, [esp-12]          //into eax
 or     ax, 0000100000000000B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 and    ax, 1111101111111111B //Bit 10-11 is rounding and bit10 = 0 & bit11 = 1 is round down
 mov    [esp-8], ax
 fldcw  word ptr [esp-8]
 fld    X
 fistp  qword ptr [esp-8]
 mov    eax, [esp-8]
 mov    edx, [esp-4]
 fldcw  word ptr [esp-12]     //Restore controlword
end;

//Author:            John O'Harrow
//Optimized for:     Blended
//Instructionset(s): IA32
//Original Name:     CeilExtendedJOH

function CeilFastcodeBlended(const X : Extended) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Blended
//Instructionset(s): IA32
//Original Name:     CeilDoubleJOH

function CeilFastcodeBlended(const X : Double  ) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     Blended
//Instructionset(s): IA32
//Original Name:     CeilSingleJOH

function CeilFastcodeBlended(const X : Single) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     RTL
//Instructionset(s): IA32
//Original Name:     CeilExtendedJOH

function CeilFastcodeRTL(const X : Extended) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     RTL
//Instructionset(s): IA32
//Original Name:     CeilDoubleJOH

function CeilFastcodeRTL(const X : Double  ) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            John O'Harrow
//Optimized for:     RTL
//Instructionset(s): IA32
//Original Name:     CeilSingleJOH

function CeilFastcodeRTL(const X : Single) : Int64;
var
  OldCW, NewCW : Word;
asm
  fnstcw OldCW     {Save Current Control Word}
  mov    ax, OldCW
  and    ax, $FBFF {Clear Bit 10}
  or     ax, $0B00 {Set Bits 8, 9 and 11}
  mov    NewCW, ax {Bits 8/9 = Precision, Bits 10/11 = Rounding Mode}
  fldcw  NewCW     {Set Round to +ve Infinity Mode with 64 Bit Precision}
  fld    X
  fistp  Result
  fldcw  OldCW     {Restore Original Control Word}
end;

//Author:            Dennis Kjaer Christensen
//Instructionset(s): IA32
//Original Name:     CeilExtendedDKCPas4

function CeilFastcodePascal(const X: Extended): Int64;
begin
 Result := Trunc(X);
 if ((X > 0) and (X - Result <> 0)) then
  Inc(Result);
end;

//Author:            Dennis Kjaer Christensen
//Instructionset(s): IA32
//Original Name:     CeilDoubleDKCPas4

function CeilFastcodePascal(const X: Double): Int64;
begin
 Result := Trunc(X);
 if ((X > 0) and (X - Result <> 0)) then
  Inc(Result);
end;

//Author:            Dennis Kjaer Christensen
//Instructionset(s): IA32
//Original Name:     CeilSingleDKCPas4

function CeilFastcodePascal(const X: Single): Int64;
begin
 Result := Trunc(X);
 if ((X > 0) and (X - Result <> 0)) then
  Inc(Result);
end;

end.
