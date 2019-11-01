unit FastcodeFillCharUnit;

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
 * Contributor(s): John O'Harrow, Chris Grant, Dennis Christensen, Pierre Le Riche
 *
 * ***** END LICENSE BLOCK ***** *)

//Version : 1.1
//Only plain function calls supported
//Last update 25/2 2005

interface

procedure FillCharFastcodeP4P(var Dest; count: Integer; Value: Char);
procedure FillCharFastcodeP4N(var Dest; count: Integer; Value: Char);
procedure FillCharFastcodePMB(var Dest; count: Integer; Value: Char);
procedure FillCharFastcodeP3(var Dest; count: Integer; Value: Char);
procedure FillCharFastcodeAMD64(var Dest; count: Integer; Value: Char);
procedure FillCharFastcodeXP(var Dest; count: Integer; Value: Char);
procedure FillCharFastcodeBlended(var Dest; count: Integer; Value: Char);
procedure FillCharFastcodeRTL(var Dest; count: Integer; Value: Char);
procedure FillCharFastcodePascal(var Dest; count: Integer; Value: Char);

implementation

uses
 SysUtils;

//Author:            Dennis Kjaer Christensen
//Date:              22/12 2003
//Optimized for:     Intel Pentium 4 Prescott
//Instructionset(s): IA32, MMX, SSE, SSE2
//Original Name:     FillCharDKC_SSE2_10

procedure FillCharFastcodeP4P(var Dest; count: Integer; Value: Char);
asm
   test edx,edx
   jle  @Exit2
   //case Count of
   cmp  edx,31
   jnbe @CaseElse
   jmp  dword ptr [edx*4+@Case1JmpTable]
 @CaseCount0 :
   ret
 @CaseCount1 :
   mov  [eax],cl
   ret
 @CaseCount2 :
   mov  ch,cl
   mov  [eax],cx
   ret
 @CaseCount3 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cl
   ret
 @CaseCount4 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   ret
 @CaseCount5 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cl
   ret
 @CaseCount6 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   ret
 @CaseCount7 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cl
   ret
 @CaseCount8 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   ret
 @CaseCount9 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cl
   ret
 @CaseCount10 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   ret
 @CaseCount11 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cl
   ret
 @CaseCount12 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   ret
 @CaseCount13 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cl
   ret
 @CaseCount14 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   ret
 @CaseCount15 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cl
   ret
 @CaseCount16 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   ret
 @CaseCount17 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cl
   ret
 @CaseCount18 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   ret
 @CaseCount19 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   mov  [eax+18],cl
   ret
 @CaseCount20 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   ret
 @CaseCount21 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cl
   ret
 @CaseCount22 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   ret
 @CaseCount23 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cl
   ret
 @CaseCount24 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   ret
 @CaseCount25 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cl
   ret
 @CaseCount26 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   ret
 @CaseCount27 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cl
   ret
 @CaseCount28 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   ret
 @CaseCount29 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cl
   ret
 @CaseCount30 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   ret
 @CaseCount31 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   mov   [eax+30],cl
   ret
   nop
   nop
   nop
   nop
   nop
 @CaseElse :
   //Need at least 32 bytes here. Max 16 for alignment and 16 for loop
   push    esi
   push    edi
   //Broadcast value
   mov     ch, cl
   movd    xmm0, ecx
   pshuflw xmm0, xmm0, 0
   pshufd  xmm0, xmm0, 0
   //Fill first 16 non aligned bytes
   movdqu  [eax],xmm0
   //StopP2 := P + Count;
   lea     ecx,[eax+edx]
   //16 byte Align
   mov     edi,eax
   and     edi,$F
   mov     esi,16
   sub     esi,edi
   add     eax,esi
   sub     edx,esi
   //I := 0;
   xor     esi,esi
   sub     edx,15
   cmp     edx,1048576
   ja      @Repeat4
 @Repeat1 :
   movdqa  [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat1
   jmp     @Repeat4End
   nop
   nop
 @Repeat4 :
   movntdq [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat4
 @Repeat4End :
   //Fill the rest
   movdqu [ecx-16],xmm0
 @Exit1 :
   pop   edi
   pop   esi
 @Exit2 :
   ret
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop

@Case1JmpTable:
 dd @CaseCount0
 dd @CaseCount1
 dd @CaseCount2
 dd @CaseCount3
 dd @CaseCount4
 dd @CaseCount5
 dd @CaseCount6
 dd @CaseCount7
 dd @CaseCount8
 dd @CaseCount9
 dd @CaseCount10
 dd @CaseCount11
 dd @CaseCount12
 dd @CaseCount13
 dd @CaseCount14
 dd @CaseCount15
 dd @CaseCount16
 dd @CaseCount17
 dd @CaseCount18
 dd @CaseCount19
 dd @CaseCount20
 dd @CaseCount21
 dd @CaseCount22
 dd @CaseCount23
 dd @CaseCount24
 dd @CaseCount25
 dd @CaseCount26
 dd @CaseCount27
 dd @CaseCount28
 dd @CaseCount29
 dd @CaseCount30
 dd @CaseCount31

end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 4 Northwood
//Instructionset(s): IA32, MMX, SSE, SSE2
//Original Name:     FillCharJOH_SSE2

procedure FillCharFastcodeP4N(var Dest; count: Integer; Value: Char);
asm {Size = 161 Bytes}
  cmp       edx, 32
  mov       ch, cl                {Copy Value into both Bytes of CX}
  jl        @@Small
  sub       edx, 16
  movd      xmm0, ecx
  pshuflw   xmm0, xmm0, 0
  pshufd    xmm0, xmm0, 0
  movups    [eax], xmm0           {Fill First 16 Bytes}
  movups    [eax+edx], xmm0       {Fill Last 16 Bytes}
  mov       ecx, eax              {16-Byte Align Writes}
  and       ecx, 15
  sub       ecx, 16
  sub       eax, ecx
  add       edx, ecx
  add       eax, edx
  neg       edx
  cmp       edx, -512*1024
  jb        @@Large
@@Loop:
  movaps    [eax+edx], xmm0       {Fill 16 Bytes per Loop}
  add       edx, 16
  jl        @@Loop
  ret
@@Large:
  movntdq    [eax+edx], xmm0      {Fill 16 Bytes per Loop}
  add       edx, 16
  jl        @@Large
  ret
@@Small:
  test      edx, edx
  jle       @@Done
  mov       [eax+edx-1], cl       {Fill Last Byte}
  and       edx, -2               {No. of Words to Fill}
  neg       edx
  lea       edx, [@@SmallFill + 60 + edx * 2]
  jmp       edx
  nop                             {Align Jump Destinations}
  nop
@@SmallFill:
  mov       [eax+28], cx
  mov       [eax+26], cx
  mov       [eax+24], cx
  mov       [eax+22], cx
  mov       [eax+20], cx
  mov       [eax+18], cx
  mov       [eax+16], cx
  mov       [eax+14], cx
  mov       [eax+12], cx
  mov       [eax+10], cx
  mov       [eax+ 8], cx
  mov       [eax+ 6], cx
  mov       [eax+ 4], cx
  mov       [eax+ 2], cx
  mov       [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;

//Author:            Dennis Kjaer Christensen
//Date:              22/12 2003
//Optimized for:     Intel Pentium M Banias
//Instructionset(s): IA32, MMX, SSE, SSE2
//Original Name:     FillCharDKC_SSE2_10

procedure FillCharFastcodePMB(var Dest; count: Integer; Value: Char);
asm
   test edx,edx
   jle  @Exit2
   //case Count of
   cmp  edx,31
   jnbe @CaseElse
   jmp  dword ptr [edx*4+@Case1JmpTable]
 @CaseCount0 :
   ret
 @CaseCount1 :
   mov  [eax],cl
   ret
 @CaseCount2 :
   mov  ch,cl
   mov  [eax],cx
   ret
 @CaseCount3 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cl
   ret
 @CaseCount4 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   ret
 @CaseCount5 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cl
   ret
 @CaseCount6 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   ret
 @CaseCount7 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cl
   ret
 @CaseCount8 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   ret
 @CaseCount9 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cl
   ret
 @CaseCount10 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   ret
 @CaseCount11 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cl
   ret
 @CaseCount12 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   ret
 @CaseCount13 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cl
   ret
 @CaseCount14 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   ret
 @CaseCount15 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cl
   ret
 @CaseCount16 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   ret
 @CaseCount17 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cl
   ret
 @CaseCount18 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   ret
 @CaseCount19 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   mov  [eax+18],cl
   ret
 @CaseCount20 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   ret
 @CaseCount21 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cl
   ret
 @CaseCount22 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   ret
 @CaseCount23 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cl
   ret
 @CaseCount24 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   ret
 @CaseCount25 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cl
   ret
 @CaseCount26 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   ret
 @CaseCount27 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cl
   ret
 @CaseCount28 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   ret
 @CaseCount29 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cl
   ret
 @CaseCount30 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   ret
 @CaseCount31 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   mov   [eax+30],cl
   ret
   nop
   nop
   nop
   nop
   nop
 @CaseElse :
   //Need at least 32 bytes here. Max 16 for alignment and 16 for loop
   push    esi
   push    edi
   //Broadcast value
   mov     ch, cl
   movd    xmm0, ecx
   pshuflw xmm0, xmm0, 0
   pshufd  xmm0, xmm0, 0
   //Fill first 16 non aligned bytes
   movdqu  [eax],xmm0
   //StopP2 := P + Count;
   lea     ecx,[eax+edx]
   //16 byte Align
   mov     edi,eax
   and     edi,$F
   mov     esi,16
   sub     esi,edi
   add     eax,esi
   sub     edx,esi
   //I := 0;
   xor     esi,esi
   sub     edx,15
   cmp     edx,1048576
   ja      @Repeat4
 @Repeat1 :
   movdqa  [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat1
   jmp     @Repeat4End
   nop
   nop
 @Repeat4 :
   movntdq [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat4
 @Repeat4End :
   //Fill the rest
   movdqu [ecx-16],xmm0
 @Exit1 :
   pop   edi
   pop   esi
 @Exit2 :
   ret
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop

@Case1JmpTable:
 dd @CaseCount0
 dd @CaseCount1
 dd @CaseCount2
 dd @CaseCount3
 dd @CaseCount4
 dd @CaseCount5
 dd @CaseCount6
 dd @CaseCount7
 dd @CaseCount8
 dd @CaseCount9
 dd @CaseCount10
 dd @CaseCount11
 dd @CaseCount12
 dd @CaseCount13
 dd @CaseCount14
 dd @CaseCount15
 dd @CaseCount16
 dd @CaseCount17
 dd @CaseCount18
 dd @CaseCount19
 dd @CaseCount20
 dd @CaseCount21
 dd @CaseCount22
 dd @CaseCount23
 dd @CaseCount24
 dd @CaseCount25
 dd @CaseCount26
 dd @CaseCount27
 dd @CaseCount28
 dd @CaseCount29
 dd @CaseCount30
 dd @CaseCount31

end;

//Author:            Dennis Kjaer Christensen
//Date:              16/9 2003
//Optimized for:     Intel Pentium 3
//Instructionset(s): IA32, MMX, SSE
//Original Name:     FillCharDKC_SSE_9

procedure FillCharFastcodeP3(var Dest; count: Integer; Value: Char);
asm
   test edx,edx
   jle  @Exit2
   //case Count of
   cmp  edx,31
   jnbe @CaseElse
   jmp  dword ptr [edx*4+@Case1JmpTable]
 @CaseCount0 :
   ret
 @CaseCount1 :
   mov  [eax],cl
   ret
 @CaseCount2 :
   mov  ch,cl
   mov  [eax],cx
   ret
 @CaseCount3 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cl
   ret
 @CaseCount4 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   ret
 @CaseCount5 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cl
   ret
 @CaseCount6 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   ret
 @CaseCount7 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cl
   ret
 @CaseCount8 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   ret
 @CaseCount9 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cl
   ret
 @CaseCount10 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   ret
 @CaseCount11 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cl
   ret
 @CaseCount12 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   ret
 @CaseCount13 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cl
   ret
 @CaseCount14 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   ret
 @CaseCount15 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cl
   ret
 @CaseCount16 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   ret
 @CaseCount17 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cl
   ret
 @CaseCount18 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   ret
 @CaseCount19 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   mov  [eax+18],cl
   ret
 @CaseCount20 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   ret
 @CaseCount21 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cl
   ret
 @CaseCount22 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   ret
 @CaseCount23 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cl
   ret
 @CaseCount24 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   ret
 @CaseCount25 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cl
   ret
 @CaseCount26 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   ret
 @CaseCount27 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cl
   ret
 @CaseCount28 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   ret
 @CaseCount29 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cl
   ret
 @CaseCount30 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   ret
 @CaseCount31 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   mov   [eax+30],cl
   ret
 @CaseElse :
   //Need at least 24 bytes here. Max 8 for alignment and 16 for loop
   push  ebx
   push  esi
   push  edi
   //P := PChar(@Dest);
   mov   esi,eax
   //StopP2 := P + Count;
   lea   ebx,[edx+eax]
   //8 byte Align
 @Repeat3 :
   mov    [esi],cl
   add    esi,1
   sub    edx,1     //Decrement count
   mov    edi,esi
   and    edi,7
   test   edi,edi
   jnz    @Repeat3
   //Broadcast value
   mov    ch, cl
   movd   mm0, ecx
   pshufw mm0, mm0, 0
   //I := 0;
   xor    eax,eax
   sub    edx,15
   cmp    edx,256000
   ja     @Repeat4
 @Repeat1 :
   movq   [esi+eax],  mm0
   movq   [esi+eax+8],mm0
   add    eax,16
   cmp    eax,edx
   jl     @Repeat1
   jmp    @Repeat4End
 @Repeat4 :
   movntq [esi+eax],  mm0
   movntq [esi+eax+8],mm0
   add    eax,16
   cmp    eax,edx
   jl     @Repeat4
 @Repeat4End :
   emms
   //Fill the rest if any
   add   esi,eax
   cmp   esi,ebx
   jnb   @Exit1
 @Repeat2 :
   mov   [esi],cl
   add   esi,1
   cmp   esi,ebx
   jb    @Repeat2
 @Exit1 :
   pop   edi
   pop   esi
   pop   ebx
 @Exit2 :
   ret
   nop
   nop
   nop
   nop
   nop

@Case1JmpTable:
 dd @CaseCount0
 dd @CaseCount1
 dd @CaseCount2
 dd @CaseCount3
 dd @CaseCount4
 dd @CaseCount5
 dd @CaseCount6
 dd @CaseCount7
 dd @CaseCount8
 dd @CaseCount9
 dd @CaseCount10
 dd @CaseCount11
 dd @CaseCount12
 dd @CaseCount13
 dd @CaseCount14
 dd @CaseCount15
 dd @CaseCount16
 dd @CaseCount17
 dd @CaseCount18
 dd @CaseCount19
 dd @CaseCount20
 dd @CaseCount21
 dd @CaseCount22
 dd @CaseCount23
 dd @CaseCount24
 dd @CaseCount25
 dd @CaseCount26
 dd @CaseCount27
 dd @CaseCount28
 dd @CaseCount29
 dd @CaseCount30
 dd @CaseCount31

end;

//Author:            Dennis Kjaer Christensen
//Date:              22/12 2003
//Optimized for:     AMD 64, Opteron, FX, Athlon 64
//Instructionset(s): IA32, MMX, SSE, SSE2
//Original Name:     FillCharDKC_SSE2_10

procedure FillCharFastcodeAMD64(var Dest; count: Integer; Value: Char);
asm
   test edx,edx
   jle  @Exit2
   //case Count of
   cmp  edx,31
   jnbe @CaseElse
   jmp  dword ptr [edx*4+@Case1JmpTable]
 @CaseCount0 :
   ret
 @CaseCount1 :
   mov  [eax],cl
   ret
 @CaseCount2 :
   mov  ch,cl
   mov  [eax],cx
   ret
 @CaseCount3 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cl
   ret
 @CaseCount4 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   ret
 @CaseCount5 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cl
   ret
 @CaseCount6 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   ret
 @CaseCount7 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cl
   ret
 @CaseCount8 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   ret
 @CaseCount9 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cl
   ret
 @CaseCount10 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   ret
 @CaseCount11 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cl
   ret
 @CaseCount12 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   ret
 @CaseCount13 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cl
   ret
 @CaseCount14 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   ret
 @CaseCount15 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cl
   ret
 @CaseCount16 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   ret
 @CaseCount17 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cl
   ret
 @CaseCount18 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   ret
 @CaseCount19 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   mov  [eax+18],cl
   ret
 @CaseCount20 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   ret
 @CaseCount21 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cl
   ret
 @CaseCount22 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   ret
 @CaseCount23 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cl
   ret
 @CaseCount24 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   ret
 @CaseCount25 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cl
   ret
 @CaseCount26 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   ret
 @CaseCount27 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cl
   ret
 @CaseCount28 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   ret
 @CaseCount29 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cl
   ret
 @CaseCount30 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   ret
 @CaseCount31 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   mov   [eax+30],cl
   ret
   nop
   nop
   nop
   nop
   nop
 @CaseElse :
   //Need at least 32 bytes here. Max 16 for alignment and 16 for loop
   push    esi
   push    edi
   //Broadcast value
   mov     ch, cl
   movd    xmm0, ecx
   pshuflw xmm0, xmm0, 0
   pshufd  xmm0, xmm0, 0
   //Fill first 16 non aligned bytes
   movdqu  [eax],xmm0
   //StopP2 := P + Count;
   lea     ecx,[eax+edx]
   //16 byte Align
   mov     edi,eax
   and     edi,$F
   mov     esi,16
   sub     esi,edi
   add     eax,esi
   sub     edx,esi
   //I := 0;
   xor     esi,esi
   sub     edx,15
   cmp     edx,1048576
   ja      @Repeat4
 @Repeat1 :
   movdqa  [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat1
   jmp     @Repeat4End
   nop
   nop
 @Repeat4 :
   movntdq [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat4
 @Repeat4End :
   //Fill the rest
   movdqu [ecx-16],xmm0
 @Exit1 :
   pop   edi
   pop   esi
 @Exit2 :
   ret
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop

@Case1JmpTable:
 dd @CaseCount0
 dd @CaseCount1
 dd @CaseCount2
 dd @CaseCount3
 dd @CaseCount4
 dd @CaseCount5
 dd @CaseCount6
 dd @CaseCount7
 dd @CaseCount8
 dd @CaseCount9
 dd @CaseCount10
 dd @CaseCount11
 dd @CaseCount12
 dd @CaseCount13
 dd @CaseCount14
 dd @CaseCount15
 dd @CaseCount16
 dd @CaseCount17
 dd @CaseCount18
 dd @CaseCount19
 dd @CaseCount20
 dd @CaseCount21
 dd @CaseCount22
 dd @CaseCount23
 dd @CaseCount24
 dd @CaseCount25
 dd @CaseCount26
 dd @CaseCount27
 dd @CaseCount28
 dd @CaseCount29
 dd @CaseCount30
 dd @CaseCount31

end;

//Author:            John O'Harrow
//Optimized for:     AMD Athlon XP
//Instructionset(s): IA32, MMX, SSE
//Original Name:     FillCharJOH_SSE

procedure FillCharFastcodeXP(var Dest; count: Integer; Value: Char);
asm {Size = 161 Bytes}
  cmp       edx, 32
  mov       ch, cl                {Copy Value into both Bytes of CX}
  jl        @@Small
  sub       edx, 16
  mov       [eax], cx             {Fill First 4 Bytes}
  mov       [eax+2], cx
  movss     xmm0, [eax]           {Set each byte of XMM0 to Value}
  shufps    xmm0, xmm0, 0
  movups    [eax], xmm0           {Fill First 16 Bytes}
  movups    [eax+edx], xmm0       {Fill Last 16 Bytes}
  mov       ecx, eax              {16-Byte Align Writes}
  and       ecx, 15
  sub       ecx, 16
  sub       eax, ecx
  add       edx, ecx
  add       eax, edx
  neg       edx
  cmp       edx, -512*1024
  jb        @@Large
@@Loop:
  movaps    [eax+edx], xmm0       {Fill 16 Bytes per Loop}
  add       edx, 16
  jl        @@Loop
  ret
@@Large:
  movntps   [eax+edx], xmm0       {Fill 16 Bytes per Loop}
  add       edx, 16
  jl        @@Large
  ret
@@Small:
  test      edx, edx
  jle       @@Done
  mov       [eax+edx-1], cl       {Fill Last Byte}
  and       edx, -2               {No. of Words to Fill}
  neg       edx
  lea       edx, [@@SmallFill + 60 + edx * 2]
  jmp       edx
  nop                             {Align Jump Destinations}
  nop
@@SmallFill:
  mov       [eax+28], cx
  mov       [eax+26], cx
  mov       [eax+24], cx
  mov       [eax+22], cx
  mov       [eax+20], cx
  mov       [eax+18], cx
  mov       [eax+16], cx
  mov       [eax+14], cx
  mov       [eax+12], cx
  mov       [eax+10], cx
  mov       [eax+ 8], cx
  mov       [eax+ 6], cx
  mov       [eax+ 4], cx
  mov       [eax+ 2], cx
  mov       [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;

//Author:            Pierre Le Riche
//Optimized for:     Blended
//Instructionset(s): IA32, MMX
//Original Name:     FillCharPLRMMX1

{MMX instruction set. Size = 240 + 4 * 40 = 400}
procedure FillCharFastcodeBlended(var Dest; Count: Integer; Value: Char);
asm
  {Copy the fill character into ch}
  mov ch, cl
  {Big or small fill?}
  cmp edx, 39
  ja @BigFill
  {Jump to the correct handler}
  jmp [edx * 4 + @JumpTable]
@BigFill:
  cmp edx, 0
  jl @DoneFill
  {Get the values in mm0}
  movd mm0, ecx
  punpcklwd mm0, mm0
  punpckldq mm0, mm0
  {Store the first qword}
  movq [eax], mm0
  {qword align eax}
  add edx, eax
  add eax, 8
  and eax, -8
  sub edx, eax
  {Fill 32 bytes}
@Fill32Loop:
  {Subtract 32 from edx so long}
  sub edx, 32
  {Fill 32 bytes}
  movq [eax], mm0
  movq [eax + 8], mm0
  movq [eax + 16], mm0
  movq [eax + 24], mm0
  add eax, 32
  cmp edx, 32
  jae @Fill32Loop
  {Exit mmx state}
  emms
  {Do the rest of the bytes}
  jmp [edx * 4 + @JumpTable]
@DoneFill:
  ret
  nop
  nop
  nop //align branch targets
@Fill38:
  mov [eax + 36], cx
@Fill36:
  mov [eax + 34], cx
@Fill34:
  mov [eax + 32], cx
@Fill32:
  mov [eax + 30], cx
@Fill30:
  mov [eax + 28], cx
@Fill28:
  mov [eax + 26], cx
@Fill26:
  mov [eax + 24], cx
@Fill24:
  mov [eax + 22], cx
@Fill22:
  mov [eax + 20], cx
@Fill20:
  mov [eax + 18], cx
@Fill18:
  mov [eax + 16], cx
@Fill16:
  mov [eax + 14], cx
@Fill14:
  mov [eax + 12], cx
@Fill12:
  mov [eax + 10], cx
@Fill10:
  mov [eax + 8], cx
@Fill8:
  mov [eax + 6], cx
@Fill6:
  mov [eax + 4], cx
@Fill4:
  mov [eax + 2], cx
@Fill2:
  mov [eax], cx
  ret
@Fill39:
  mov [eax + 37], cx
@Fill37:
  mov [eax + 35], cx
@Fill35:
  mov [eax + 33], cx
@Fill33:
  mov [eax + 31], cx
@Fill31:
  mov [eax + 29], cx
@Fill29:
  mov [eax + 27], cx
@Fill27:
  mov [eax + 25], cx
@Fill25:
  mov [eax + 23], cx
@Fill23:
  mov [eax + 21], cx
@Fill21:
  mov [eax + 19], cx
@Fill19:
  mov [eax + 17], cx
@Fill17:
  mov [eax + 15], cx
@Fill15:
  mov [eax + 13], cx
@Fill13:
  mov [eax + 11], cx
@Fill11:
  mov [eax + 9], cx
@Fill9:
  mov [eax + 7], cx
@Fill7:
  mov [eax + 5], cx
@Fill5:
  mov [eax + 3], cx
@Fill3:
  mov [eax + 1], cx
@Fill1:
  mov [eax], cl
  ret
  nop //dword align jump table
@JumpTable:
  dd @DoneFill
  dd @Fill1, @Fill2, @Fill3, @Fill4, @Fill5, @Fill6, @Fill7, @Fill8, @Fill9
  dd @Fill10, @Fill11, @Fill12, @Fill13, @Fill14, @Fill15, @Fill16, @Fill17
  dd @Fill18, @Fill19, @Fill20, @Fill21, @Fill22, @Fill23, @Fill24, @Fill25
  dd @Fill26, @Fill27, @Fill28, @Fill29, @Fill30, @Fill31, @Fill32, @Fill33
  dd @Fill34, @Fill35, @Fill36, @Fill37, @Fill38, @Fill39
end;

//Author:            John O'Harrow
//Optimized for:     RTL
//Instructionset(s): IA32
//Original Name:     FillCharJOH_FPU

procedure FillCharFastcodeRTL(var Dest; count: Integer; Value: Char);
asm {Size = 153 Bytes}
  cmp   edx, 32
  mov   ch, cl                    {Copy Value into both Bytes of CX}
  jl    @@Small
  mov   [eax  ], cx               {Fill First 8 Bytes}
  mov   [eax+2], cx
  mov   [eax+4], cx
  mov   [eax+6], cx
  sub   edx, 16
  fld   qword ptr [eax]
  fst   qword ptr [eax+edx]       {Fill Last 16 Bytes}
  fst   qword ptr [eax+edx+8]
  mov   ecx, eax
  and   ecx, 7                    {8-Byte Align Writes}
  sub   ecx, 8
  sub   eax, ecx
  add   edx, ecx
  add   eax, edx
  neg   edx
@@Loop:
  fst   qword ptr [eax+edx]       {Fill 16 Bytes per Loop}
  fst   qword ptr [eax+edx+8]
  add   edx, 16
  jl    @@Loop
  ffree st(0)
  ret
  nop
  nop
  nop
@@Small:
  test  edx, edx
  jle   @@Done
  mov   [eax+edx-1], cl       {Fill Last Byte}
  and   edx, -2               {No. of Words to Fill}
  neg   edx
  lea   edx, [@@SmallFill + 60 + edx * 2]
  jmp   edx
  nop                             {Align Jump Destinations}
  nop
@@SmallFill:
  mov   [eax+28], cx
  mov   [eax+26], cx
  mov   [eax+24], cx
  mov   [eax+22], cx
  mov   [eax+20], cx
  mov   [eax+18], cx
  mov   [eax+16], cx
  mov   [eax+14], cx
  mov   [eax+12], cx
  mov   [eax+10], cx
  mov   [eax+ 8], cx
  mov   [eax+ 6], cx
  mov   [eax+ 4], cx
  mov   [eax+ 2], cx
  mov   [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;

//Author:            Chris Grant
//Optimized for:     Pascal
//Instructionset(s): IA32
//Original Name:     FillCharCJGPas5

procedure FillCharFastcodePascal(var Dest; count: Integer; Value: Char);
var
  I, J, K : Integer;
  P    : Pointer;
  Label P01, P02, P03, P04, P05, P06, P07, P08, P09, P10, P11, P12;
begin
  if Count > 0 then
    begin
      P := @Dest;
      If Count >= 12 then
        begin
          J := Byte(Value);
          J := J or (J shl  8);
          J := J or (J shl 16);

          PInteger(P)^ := J;
          PInteger(Integer(P) + Count - 4)^ := J;

          I := Count shr 2;

          if Count >= 256 then
            begin
              if count < 448 then
                begin
                  PIntegerArray(P)[1] := J;
                  PIntegerArray(P)[2] := J;
                  PIntegerArray(P)[3] := J;

                  repeat
                    Dec(I,4);
                    PIntegerArray(P)[I]   := J;
                    PIntegerArray(P)[I+1] := J;
                    PIntegerArray(P)[I+2] := J;
                    PIntegerArray(P)[I+3] := J;
                  until I < 4;
                end
              else
                begin
                  I := Count;
                  K := (Integer(P) and 3) - 4;
                  Dec(I, 16);
                  Dec(PByte(P), K);
                  Inc(I, K);
                  Inc(PByte(P), I);
                  PintegerArray(P)[0] := J;
                  PintegerArray(P)[1] := J;
                  PintegerArray(P)[2] := J;
                  PintegerArray(P)[3] := J;
                  repeat
                    PintegerArray(Integer(P)-I)[0] := J;
                    PintegerArray(Integer(P)-I)[1] := J;
                    PintegerArray(Integer(P)-I)[2] := J;
                    PintegerArray(Integer(P)-I)[3] := J;
                    Dec(I, 16);
                  until I <= 0;
                end
             end
          else
            begin
              repeat
                Dec(I,2);
                PIntegerArray(P)[I] := J;
                PIntegerArray(P)[I+1] := J;
              until I < 2;
            end
        end
      else
        begin
          case Count of
            1:  goto P01;
            2:  goto P02;
            3:  goto P03;
            4:  goto P04;
            5:  goto P05;
            6:  goto P06;
            7:  goto P07;
            8:  goto P08;
            9:  goto P09;
            10: goto P10;
            11: goto P11;
            12: goto P12;
          end;
          P12: PByteArray(P)[11] := Byte(Value);
          P11: PByteArray(P)[10] := Byte(Value);
          P10: PByteArray(P)[09] := Byte(Value);
          P09: PByteArray(P)[08] := Byte(Value);
          P08: PByteArray(P)[07] := Byte(Value);
          P07: PByteArray(P)[06] := Byte(Value);
          P06: PByteArray(P)[05] := Byte(Value);
          P05: PByteArray(P)[04] := Byte(Value);
          P04: PByteArray(P)[03] := Byte(Value);
          P03: PByteArray(P)[02] := Byte(Value);
          P02: PByteArray(P)[01] := Byte(Value);
          P01: PByteArray(P)[00] := Byte(Value);

        end
    end;
end;

end.
