unit FastcodeCompareMem;

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
 * Contributor(s):
 * Charalabos Michael <chmichael@creationpower.com>
 * John O'Harrow <john@elmcrest.demon.co.uk>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I FastCode.inc}

type
  FastCodeCompareMemFunction = function(const p1, p2: pointer; const length: integer): boolean;

{Functions shared between Targets}
function FastCodeCompareMemRTL   (const p1, p2: pointer; const length: integer): boolean;
{Functions not shared between Targets}
function FastCodeCompareMemPascal(const p1, p2: pointer; const length: integer): boolean;

const
  Version = '0.3';

  FastCodeCompareMemP4P    : FastCodeCompareMemFunction = FastCodeCompareMemRTL;
  FastCodeCompareMemP4N    : FastCodeCompareMemFunction = FastCodeCompareMemRTL;
  FastCodeCompareMemPMD    : FastCodeCompareMemFunction = FastCodeCompareMemRTL;
  FastCodeCompareMemPMB    : FastCodeCompareMemFunction = FastCodeCompareMemRTL;
  FastCodeCompareMemAMD64  : FastCodeCompareMemFunction = FastCodeCompareMemRTL;
  FastCodeCompareMemXP     : FastCodeCompareMemFunction = FastCodeCompareMemRTL;
  FastCodeCompareMemBlended: FastCodeCompareMemFunction = FastCodeCompareMemRTL;

procedure CompareMemStub;

implementation

uses
  SysUtils;

//Author:            Aleksandr Sharahov
//Optimized for:     Intel P4 Prescott
//Instructionset(s): IA32
//Original name:     FastCodeCompareMemMem_Sha_IA32_4

function FastCodeCompareMemRTL(const p1, p2: pointer; const length: integer): boolean;
asm
   add   eax, ecx
   add   edx, ecx
   xor   ecx, -1
   add   eax, -8
   add   edx, -8
   add   ecx, 9
   push  ebx
   jg    @Dword
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   lea   ebx, [eax+ecx]
   add   ecx, 4
   and   ebx, 3
   sub   ecx, ebx
   jg    @Dword
@DwordLoop:
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   mov   ebx, [eax+ecx+4]
   cmp   ebx, [edx+ecx+4]
   jne   @Ret0
   add   ecx, 8
   jg    @Dword
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   mov   ebx, [eax+ecx+4]
   cmp   ebx, [edx+ecx+4]
   jne   @Ret0
   add   ecx, 8
   jle   @DwordLoop
@Dword:
   cmp   ecx, 4
   jg    @Word
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   add   ecx, 4
@Word:
   cmp   ecx, 6
   jg    @Byte
   movzx ebx, word ptr [eax+ecx]
   cmp   bx, [edx+ecx]
   jne   @Ret0
   add   ecx, 2
@Byte:
   cmp   ecx, 7
   jg    @Ret1
   movzx ebx, byte ptr [eax+7]
   cmp   bl, [edx+7]
   jne   @Ret0
@Ret1:
   mov   eax, 1
   pop   ebx
   ret
@Ret0:
   xor   eax, eax
   pop   ebx
end;

//Author:            Aleksandr Sharahov
//Optimized for:     Blended / Pascal
//Instructionset(s): IA32
//Original name:     FastCodeCompareMemMem_Sha_Pas_4

{$UNDEF SaveQ} {$IFOPT Q+} {$Q-} {$DEFINE SaveQ} {$ENDIF}
{$UNDEF SaveR} {$IFOPT R+} {$R-} {$DEFINE SaveR} {$ENDIF}
function FastCodeCompareMemPascal(const p1, p2: pointer; const length: integer): boolean;
var
  q1, q2: pIntegerArray;
  c: cardinal;
label
  Ret0;
begin;
  c:=length;
  c:=c+cardinal(p1)-8;
  q1:=p1;
  q2:=p2;
  if c>=cardinal(q1) then begin;
    if q1[0]<>q2[0] then goto Ret0;
    inc(cardinal(q1),4);
    inc(cardinal(q2),4);
    dec(cardinal(q2),cardinal(q1));
    cardinal(q1):=cardinal(q1) and -4;
    inc(cardinal(q2),cardinal(q1));
    //if c>=cardinal(q1) then repeat;
    //replaced: compiler creates a copy of cardinal(q1) for statement above
    if pchar(c)>=q1 then repeat;
      if q1[0]<>q2[0] then goto Ret0;
      if q1[1]<>q2[1] then goto Ret0;
      inc(cardinal(q1),8);
      inc(cardinal(q2),8);
      if c<cardinal(q1) then break;
      if q1[0]<>q2[0] then goto Ret0;
      if q1[1]<>q2[1] then goto Ret0;
      inc(cardinal(q1),8);
      inc(cardinal(q2),8);
      until c<cardinal(q1);
    end;
  c:=c-cardinal(q1)+8;
  if integer(c)>=4 then begin;
    if q1[0]<>q2[0] then goto Ret0;
    inc(cardinal(q1),4);
    inc(cardinal(q2),4);
    c:=c-4;
    end;
  if integer(c)>=2 then begin;
    if pword(q1)^<>pword(q2)^ then goto Ret0;
    inc(cardinal(q1),2);
    inc(cardinal(q2),2);
    c:=c-2;
    end;
  if integer(c)>=1 then if pchar(q1)^<>pchar(q2)^ then goto Ret0;
  Result:=true;
  exit;
Ret0:
  Result:=false;
  end;
{$IFDEF SaveQ} {$Q+} {$UNDEF SaveQ} {$ENDIF}
{$IFDEF SaveR} {$R+} {$UNDEF SaveR} {$ENDIF}

procedure CompareMemStub;
asm
  call SysUtils.CompareMem;
end;

end.
