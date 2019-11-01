unit FastCodeCompareText;

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
  FastCodeCompareTextFunction = function(const S1, S2: string): Integer;

{Functions shared between Targets}
function FastCodeCompareTextRTL   (const S1, S2: string): Integer;
{Functions not shared between Targets}
function FastCodeCompareTextP4N   (const S1, S2: string): Integer;
function FastCodeCompareTextPascal(const S1, S2: string): Integer;

const
  Version = '0.3';

  FastCodeCompareTextP4P    : FastCodeCompareTextFunction = FastCodeCompareTextRTL;
  FastCodeCompareTextPMD    : FastCodeCompareTextFunction = FastCodeCompareTextRTL;
  FastCodeCompareTextPMB    : FastCodeCompareTextFunction = FastCodeCompareTextRTL;
  FastCodeCompareTextAMD64  : FastCodeCompareTextFunction = FastCodeCompareTextRTL;
  FastCodeCompareTextXP     : FastCodeCompareTextFunction = FastCodeCompareTextRTL;
  FastCodeCompareTextBlended: FastCodeCompareTextFunction = FastCodeCompareTextRTL;

procedure CompareTextStub;

implementation

uses
  SysUtils;

//Author:            Aleksandr Sharahov
//Optimized for:     Intel P4 Prescott
//Instructionset(s): IA32
//Original name:     CompareTextShaAsm3_d

function FastCodeCompareTextRTL(const S1, S2: string): Integer;
asm
         test  eax, eax
         jz    @nil1
         test  edx, edx
         jnz   @ptrok

@nil2:   mov   eax, [eax-4]
         ret
@nil1:   test  edx, edx
         jz    @nil0
         sub   eax, [edx-4]
@nil0:   ret

@ptrok:  push  edi
         push  ebx
         xor   edi, edi
         mov   ebx, [eax-4]
         mov   ecx, ebx
         sub   ebx, [edx-4]
         adc   edi, -1
         push  ebx
         and   ebx, edi
         mov   edi, eax
         sub   ebx, ecx
         jge   @len

@lenok:  sub   edi, ebx
         sub   edx, ebx

@loop:   mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         cmp   eax, ecx
         jne   @byte0
@same:   add   ebx, 4
         jl    @loop

@len:    pop   eax
         pop   ebx
         pop   edi
         ret

@loop2:  mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         cmp   eax, ecx
         je    @same

@byte0:  cmp   al, cl
         je    @byte1

         and   eax, $FF
         and   ecx, $FF
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @up0a
         sub   eax, 'a'-'A'
@up0a:   cmp   cl, 'z'-'a'
         ja    @up0c
         sub   ecx, 'a'-'A'
@up0c:   sub   eax, ecx
         jnz   @done

         mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]

@byte1:  cmp   ah, ch
         je    @byte2

         and   eax, $FF00
         and   ecx, $FF00
         sub   eax, 'a'*256
         sub   ecx, 'a'*256
         cmp   ah, 'z'-'a'
         ja    @up1a
         sub   eax, ('a'-'A')*256
@up1a:   cmp   ch, 'z'-'a'
         ja    @up1c
         sub   ecx, ('a'-'A')*256
@up1c:   sub   eax, ecx
         jnz   @done

         mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]

@byte2:  add   ebx, 2
         jnl   @len2
         shr   eax, 16
         shr   ecx, 16
         cmp   al, cl
         je    @byte3

         and   eax, $FF
         and   ecx, $FF
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @up2a
         sub   eax, 'a'-'A'
@up2a:   cmp   cl, 'z'-'a'
         ja    @up2c
         sub   ecx, 'a'-'A'
@up2c:   sub   eax, ecx
         jnz   @done

         movzx eax, word ptr [ebx+edi]
         movzx ecx, word ptr [ebx+edx]

@byte3:  cmp   ah, ch
         je    @byte4

         and   eax, $FF00
         and   ecx, $FF00
         sub   eax, 'a'*256
         sub   ecx, 'a'*256
         cmp   ah, 'z'-'a'
         ja    @up3a
         sub   eax, ('a'-'A')*256
@up3a:   cmp   ch, 'z'-'a'
         ja    @up3c
         sub   ecx, ('a'-'A')*256
@up3c:   sub   eax, ecx
         jnz   @done

@byte4:  add   ebx, 2
         jl    @loop2
@len2:   pop   eax
         pop   ebx
         pop   edi
         ret

@done:   pop   ecx
         pop   ebx
         pop   edi
end;

//Author:            Aleksandr Sharahov
//Optimized for:     Intel P4 Northwood
//Instructionset(s): IA32
//Original name:     CompareTextShaAsm4_d

function FastCodeCompareTextP4N(const S1, S2: string): Integer;
asm
         test  eax, eax
         jz    @nil1
         test  edx, edx
         jnz   @ptrok

@nil2:   mov   eax, [eax-4]
         ret
@nil1:   test  edx, edx
         jz    @nil0
         sub   eax, [edx-4]
@nil0:   ret

@ptrok:  push  edi
         push  ebx
         xor   edi, edi
         mov   ebx, [eax-4]
         mov   ecx, ebx
         sub   ebx, [edx-4]
         adc   edi, -1
         push  ebx
         and   ebx, edi
         mov   edi, eax
         sub   ebx, ecx        //ebx := -min(Length(s1),Length(s2))
         jge   @len

@lenok:  sub   edi, ebx
         sub   edx, ebx

@loop:   mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         xor   eax, ecx
         jne   @differ
@same:   add   ebx, 4
         jl    @loop

@len:    pop   eax
         pop   ebx
         pop   edi
         ret

@loop2:  mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         xor   eax, ecx
         je    @same
@differ: test  eax, $DFDFDFDF  //$00 or $20
         jnz   @find
         add   eax, eax        //$00 or $40
         add   eax, eax        //$00 or $80
         test  eax, ecx
         jnz   @find
         and   ecx, $5F5F5F5F  //$41..$5A
         add   ecx, $3F3F3F3F  //$80..$99
         and   ecx, $7F7F7F7F  //$00..$19
         add   ecx, $66666666  //$66..$7F
         test  ecx, eax
         jnz   @find
         add   ebx, 4
         jl    @loop2

@len2:   pop   eax
         pop   ebx
         pop   edi
         ret

@loop3:  add   ebx, 1
         jge   @len2
@find:   movzx eax, [ebx+edi]
         movzx ecx, [ebx+edx]
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @upa
         sub   eax, 'a'-'A'
@upa:    cmp   cl, 'z'-'a'
         ja    @upc
         sub   ecx, 'a'-'A'
@upc:    sub   eax, ecx
         jz    @loop3

@found:  pop   ecx
         pop   ebx
         pop   edi
end;

//Author:            Aleksandr Sharahov
//Optimized for:     Blended / Pascal
//Instructionset(s): IA32
//Original name:     CompareTextShaPas5_b

{$UNDEF SaveQ} {$IFOPT Q+} {$Q-} {$DEFINE SaveQ} {$ENDIF}
{$UNDEF SaveR} {$IFOPT R+} {$R-} {$DEFINE SaveR} {$ENDIF}
function FastCodeCompareTextPascal(const S1, S2: string): Integer;
var
  c1, c2, d, q, save: integer;
  p: pIntegerArray;
label
  past, find;
begin;
  d:=integer(@pchar(pointer(s1))[-4]);
  c1:=0;
  c2:=0;
  p:=@pchar(pointer(s2))[-4];
  if d<>-4 then c1:=pinteger(d)^;          //c1 = length of s1
  if p<>pointer(-4) then c2:=pinteger(p)^; //c2 = length of s2
  d:=(d-integer(p)) shr 2;                 //d = distance(s1-s2) div 4
  q:=c1;
  c1:=c1-c2;
  if c1>0 then q:=c2;                      //q = min length
  save:=c1;                    //save result for equal data
  if q<=0 then begin;
    Result:=c1;
    exit;
    end;
  q:=q+integer(p);

  repeat;
    c1:=p[d+1];                            //dword from s1
    c2:=p[1];                              //dword from s2
    inc(integer(p),4);
    c1:=c1 xor c2;
    if c1<>0 then begin;                   //test the difference
      //all bits of each byte must be 0, except bit5 (weight $20)
      if (c1 and integer($DFDFDFDF))<>0 then goto find;

      //bit5 can be 1 for letters only
      c1:=c1 + c1;                         //$00 or $40
      c1:=c1 + c1;                         //$00 or $80
      if (c1 and c2)<>0 then goto find;    //if not letter
      c2:=c2 and $5F5F5F5F;                //$41..$5A
      c2:=c2   + $3F3F3F3F;                //$80..$99
      c2:=c2 and $7F7F7F7F;                //$00..$19
      c2:=c2   + $66666666;                //$66..$7F
      if (c1 and c2)<>0 then goto find;    //if not letter
      end;
    until cardinal(p)>=cardinal(q);
past:
  Result:=save;
  exit;

  repeat; //find mismatched characters
    if cardinal(p)>=cardinal(q+4) then goto past;
find:
    c1:=byte(p[d]);
    c2:=byte(p[0]);
    inc(integer(p));
    c1:=c1-ord('a');
    c2:=c2-ord('a');
    if cardinal(c1)<=ord('z')-ord('a') then c1:=c1-(ord('a')-ord('A'));
    if cardinal(c2)<=ord('z')-ord('a') then c2:=c2-(ord('a')-ord('A'));
    until c1<>c2;
  Result:=c1-c2;
  end;
{$IFDEF SaveQ} {$Q+} {$UNDEF SaveQ} {$ENDIF}
{$IFDEF SaveR} {$R+} {$UNDEF SaveR} {$ENDIF}

procedure CompareTextStub;
asm
  call SysUtils.CompareText;
end;

end.
