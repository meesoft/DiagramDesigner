unit FastCodeStrComp;

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
 * Portions created by the Initial Developer are Copyright (C) 2002-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Charalabos Michael <chmichael@creationpower.com>
 * John O'Harrow <john@elmcrest.demon.co.uk>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I FastCode.inc}

type
  FastCodeStrCompFunction = function(const Str1, Str2: PChar): Integer;

{Functions shared between Targets}
function FastCodeStrCompRTL   (const Str1, Str2: PChar): Integer;
{Functions not shared between Targets}
function FastcodeStrCompPascal(const Str1, Str2: PChar): Integer;

const
  Version = '0.3';

  FastCodeStrCompP4P    : FastCodeStrCompFunction = FastCodeStrCompRTL;
  FastCodeStrCompP4N    : FastCodeStrCompFunction = FastCodeStrCompRTL;
  FastCodeStrCompPMD    : FastCodeStrCompFunction = FastCodeStrCompRTL;
  FastCodeStrCompPMB    : FastCodeStrCompFunction = FastCodeStrCompRTL;
  FastCodeStrCompAMD64  : FastCodeStrCompFunction = FastCodeStrCompRTL;
  FastCodeStrCompXP     : FastCodeStrCompFunction = FastCodeStrCompRTL;
  FastCodeStrCompBlended: FastCodeStrCompFunction = FastCodeStrCompRTL;

procedure StrCompStub;

implementation

uses
  SysUtils;

//Author:            Aleksandr Sharahov
//Optimized for:     Intel P4 Prescott
//Instructionset(s): IA32
//Original name:     StrCompSha7

function FastCodeStrCompRTL(const Str1, Str2: PChar): Integer;
asm
   sub   eax, edx
   jz    @ret
@loop:
   movzx ecx, [eax+edx]
   cmp   cl, [edx]
   jne   @stop
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+1]
   cmp   cl, [edx+1]
   jne   @stop1
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+2]
   cmp   cl, [edx+2]
   jne   @stop2
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+3]
   cmp   cl, [edx+3]
   jne   @stop3
   add   edx, 4
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx]
   cmp   cl, [edx]
   jne   @stop
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+1]
   cmp   cl, [edx+1]
   jne   @stop1
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+2]
   cmp   cl, [edx+2]
   jne   @stop2
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+3]
   cmp   cl, [edx+3]
   jne   @stop3
   add   edx, 4
   test  cl, cl
   jnz   @loop
@eq:
   xor   eax, eax
@ret:
   ret
@stop3:
   add   edx, 1
@stop2:
   add   edx, 1
@stop1:
   add   edx, 1
@stop:
   mov   eax, ecx
   movzx edx, [edx]
   sub   eax, edx
end;

//Author:            Aleksandr Sharahov
//Optimized for:     Blended / Pascal
//Instructionset(s): IA32
//Original name:     StrCompShaPas5

function FastcodeStrCompPascal(const Str1, Str2: PChar): Integer;
var
  dist: integer;
  p: pchar;
label
  make, make1, make2;
begin;
   dist:=str1-str2;
   p:=str2;
   repeat;
     Result:=shortint(p[dist]);
     if (Result=0) or (chr(Result)<>p[0]) then goto make;
     Result:=shortint(p[dist+1]);
     if (Result=0) or (chr(Result)<>p[1]) then goto make1;
     Result:=shortint(p[dist+2]);
     if (Result=0) or (chr(Result)<>p[2]) then goto make2;
     Result:=shortint(p[dist+3]);
     inc(p,4);
     until (Result=0) or (chr(Result)<>p[-1]);
   dec(p,3);
make2:
   inc(p);
make1:
   inc(p);
make:
   Result:=byte(Result)-byte(p[0]);
end;

procedure StrCompStub;
asm
  call SysUtils.StrComp;
end;

end.
