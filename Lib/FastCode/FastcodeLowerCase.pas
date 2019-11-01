unit FastcodeLowerCase;

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
  FastCodeLowerCaseFunction = function(const s: string): string;

{Functions shared between Targets}
function FastCodeLowerCaseRTL   (const s: string): string;
{Functions not shared between Targets}
function FastCodeLowerCasePascal(const s: string): string;

const
  Version = '0.3';

  FastCodeLowerCaseP4P    : FastCodeLowerCaseFunction = FastCodeLowerCaseRTL;
  FastCodeLowerCaseP4N    : FastCodeLowerCaseFunction = FastCodeLowerCaseRTL;
  FastCodeLowerCasePMD    : FastCodeLowerCaseFunction = FastCodeLowerCaseRTL;
  FastCodeLowerCasePMB    : FastCodeLowerCaseFunction = FastCodeLowerCaseRTL;
  FastCodeLowerCaseAMD64  : FastCodeLowerCaseFunction = FastCodeLowerCaseRTL;
  FastCodeLowerCaseXP     : FastCodeLowerCaseFunction = FastCodeLowerCaseRTL;
  FastCodeLowerCaseBlended: FastCodeLowerCaseFunction = FastCodeLowerCaseRTL;

procedure LowerCaseStub;

implementation

uses
  SysUtils;

//Author:            Aleksandr Sharahov
//Optimized for:     Intel P4 Prescott
//Instructionset(s): IA32
//Original name:     LowerCaseShaAsm6_d

function FastCodeLowerCaseRTL(const s: string): string;
asm
       push  ebx
       push  esi
       push  edi
       mov   esi, eax          // s
       mov   eax, edx
       test  esi, esi
       jz    @Nil
       mov   edx, [esi-4]      // Length(s)
       mov   edi, eax          // @Result
       test  edx, edx
       jle   @Nil
       mov   ecx, [eax]
       mov   ebx, edx
       test  ecx, ecx
       jz    @Realloc          // Jump if Result not allocated
       test  edx, 3
       jnz   @Length3
       xor   edx, [ecx-4]
       cmp   edx, 3
       jbe   @TestRef
       jmp   @Realloc
@Length3:
       or    edx, 2
       xor   edx, [ecx-4]
       cmp   edx, 1
       ja    @Realloc
@TestRef:
       cmp   [ecx-8], 1
       je    @LengthOK         // Jump if Result RefCt=1
@Realloc:
       mov   edx, ebx
       or    edx, 3
       call  System.@LStrSetLength
@LengthOK:
       mov   edi, [edi]        // Result
       mov   [edi-4], ebx      // Correct Result length
       mov   byte ptr [ebx+edi], 0
       add   ebx, -1
       and   ebx, -4
       mov   eax, [ebx+esi]

@Loop: mov   ecx, eax
       or    eax, $80808080    // $C1..$DA
       mov   edx, eax
       sub   eax, $5B5B5B5B    // $66..$7F
       xor   edx, ecx          // $80
       or    eax, $80808080    // $E6..$FF
       sub   eax, $66666666    // $80..$99
       and   eax, edx          // $80
       shr   eax, 2            // $20
       xor   eax, ecx          // Lower
       mov   [ebx+edi], eax
       mov   eax, [ebx+esi-4]
       sub   ebx, 4
       jge   @Loop

       pop   edi
       pop   esi
       pop   ebx
       ret

@Nil:  pop   edi
       pop   esi
       pop   ebx
       jmp    System.@LStrClr   // Result:=''
end;

//Author:            Aleksandr Sharahov
//Optimized for:     Blended / Pascal
//Instructionset(s): IA32
//Original name:     LowerCaseShaPas6_d

{$UNDEF SaveQ} {$IFOPT Q+} {$Q-} {$DEFINE SaveQ} {$ENDIF}
{$UNDEF SaveR} {$IFOPT R+} {$R-} {$DEFINE SaveR} {$ENDIF}
function FastCodeLowerCasePascal(const s: string): string;
var
  ch1, ch2, ch3, dist: integer;
  p, q: pInteger;
label
  Realloc, LengthOK;
begin;
  p:=pointer(@pchar(pointer(s))[-4]);
  ch1:=0;
  if integer(p)<>-4 then ch1:=p^;
  if ch1=0 then Result:=''
  else begin;
    q:=pointer(Result);
    if q=nil then goto Realloc;
    if ch1 and 3=0
    then if ch1 xor pInteger(pchar(q)-4)^ > 3
         then goto Realloc
         else
    else if (ch1 or 2) xor pInteger(pchar(q)-4)^ > 1
         then goto Realloc;
    if (pInteger(pchar(q)-8)^=1) then goto LengthOK;
Realloc:
    SetLength(Result,ch1 or 3);
    q:=pointer(Result);
LengthOK:
    pchar(q)[ch1]:=#0;                // Terminator
    dec(q); q^:=ch1;                  // Correct Result length
    dist:=(pointer(Result)-pchar(p)) shr 2;
    q:=@pchar(p)[(ch1+3) and -4];
    ch1:=q^;
    repeat;
      ch2:=ch1;
      ch1:=ch1 or integer($80808080); // $C1..$DA
      ch3:=ch1;
      ch1:=ch1 - $5B5B5B5B;           // $66..$7F
      dec(q);
      ch1:=ch1 or integer($80808080); // $E6..$FF
      ch3:=ch3 xor ch2;               // $80
      ch1:=ch1 - $66666666;           // $80..$99
      ch3:=ch3 and ch1;               // $80
      ch1:=q^;
      ch3:=ch3 shr 2;                 // $20
      ch3:=ch3 xor ch2;               // Lower
      pIntegerArray(q)[dist]:=ch3;
      until cardinal(q)<=cardinal(p);
    end;
end;
{$IFDEF SaveQ} {$Q+} {$UNDEF SaveQ} {$ENDIF}
{$IFDEF SaveR} {$R+} {$UNDEF SaveR} {$ENDIF}

procedure LowerCaseStub;
asm
  call SysUtils.LowerCase;
end;

end.
