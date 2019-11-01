unit FastcodeMoveUnit;

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
 * Contributor(s): John O'Harrow, Dennis Kjaer Christensen
 *
 * ***** END LICENSE BLOCK ***** *)

//Version : 1.0
//Only plain function calls supported
//Last update 20/2 2005

interface

{$D-}

procedure MoveFastcodeP4P(const Source; var Dest; Count : Integer);
procedure MoveFastcodeP4N(const Source; var Dest; Count : Integer);
procedure MoveFastcodePMB(const Source; var Dest; Count : Integer);
procedure MoveFastcodeAMD64(const Source; var Dest; Count : Integer);
procedure MoveFastcodeXP(const Source; var Dest; Count : Integer);
procedure MoveFastcodeBlended(const Source; var Dest; Count : Integer);
procedure MoveFastcodeRTL(const Source; var Dest; Count : Integer);
procedure MoveFastcodePascal(const Source; var Dest; Count : Integer);

implementation

//Author:            Dennis Kjaer Christensen
//Date:              26/2 2004
//Optimized for:     P4 Prescott
//Instructionset(s): IA32, MMX, SSE, SSE2, SSE3
//Original name:     MoveDKCSSE3_1

procedure MoveFastcodeP4P(const Source; var Dest; Count : Integer);
const
 L2CACHESIZE : Cardinal = 1000000;
 SPLIT1 : Cardinal = 150;

asm
     //Exit if Count is negative
     test    ecx, ecx
     js      @Exit
     //Detect the need for rewerse move in overlapped case
     cmp     eax, edx                   // if (DestAddress > SourceAddress) then
     jnb     @ForwardMove
     push    ebx
     mov     ebx, edx
     sub     ebx, eax                   // (DestAddress - SourceAddress)
     cmp     ebx, edx                   // if ((DestAddress - SourceAddress) < Count) then
     pop     ebx
     jb      @RewMove
@L2:
@L1:
@ForwardMove:
     cmp     ecx, 55
     jnbe    @ForwardCaseElse
     jmp     dword ptr [ecx*4+@Case1JmpTable]
@ForwardCaseCount0:
     jmp     @Exit
@ForwardCaseCount1:
     mov     cl, [eax]
     mov     [edx], cl
     jmp     @Exit
@ForwardCaseCount2:
     mov     cx, [eax]
     mov     [edx], cx
     jmp     @Exit
@ForwardCaseCount3:
     mov     cl, [eax]
     mov     [edx], cl
     mov     cl, [eax+1]
     mov     [edx+1], cl
     mov     cl, [eax+2]
     mov     [edx+2], cl
     jmp     @Exit
@ForwardCaseCount4:
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@ForwardCaseCount5:
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     cl, [eax+4]
     mov     [edx+4], cl
     jmp     @Exit
@ForwardCaseCount6:
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     cl, [eax+5]
     mov     [edx+5], cl
     jmp     @Exit
@ForwardCaseCount7:
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     cl, [eax+5]
     mov     [edx+5], cl
     mov     cl, [eax+6]
     mov     [edx+6], cl
     jmp     @Exit
@ForwardCaseCount8:
     movq    xmm0, [eax]
     movq    [edx], xmm0
     jmp     @Exit
@ForwardCaseCount9:
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     cl, [eax+8]
     mov     [edx+8], cl
     jmp     @Exit
@ForwardCaseCount10 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     cl, [eax+8]
     mov     [edx+8], cl
     mov     cl, [eax+9]
     mov     [edx+9], cl
     jmp     @Exit
@ForwardCaseCount11 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     cl, [eax+8]
     mov     [edx+8], cl
     mov     cl, [eax+9]
     mov     [edx+9], cl
     mov     cl, [eax+10]
     mov     [edx+10], cl
     jmp     @Exit
@ForwardCaseCount12:
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     jmp     @Exit
@ForwardCaseCount13 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     cl, [eax+12]
     mov     [edx+12], cl
     jmp     @Exit
@ForwardCaseCount14 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     cl, [eax+13]
     mov     [edx+13], cl
     jmp     @Exit
@ForwardCaseCount15 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     cl, [eax+13]
     mov     [edx+13], cl
     mov     cl, [eax+14]
     mov     [edx+14], cl
     jmp     @Exit
@ForwardCaseCount16 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     jmp     @Exit
@ForwardCaseCount17 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     cl, [eax+16]
     mov     [edx+16], cl
     jmp     @Exit
@ForwardCaseCount18 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     cl, [eax+16]
     mov     [edx+16], cl
     mov     cl, [eax+17]
     mov     [edx+17], cl
     jmp     @Exit
@ForwardCaseCount19 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     cl, [eax+16]
     mov     [edx+16], cl
     mov     cl, [eax+17]
     mov     [edx+17], cl
     mov     cl, [eax+18]
     mov     [edx+18], cl
     jmp     @Exit
@ForwardCaseCount20 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     jmp     @Exit
@ForwardCaseCount21 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     cl, [eax+20]
     mov     [edx+20], cl
     jmp     @Exit
@ForwardCaseCount22 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     cl, [eax+21]
     mov     [edx+21], cl
     jmp     @Exit
@ForwardCaseCount23 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     cl, [eax+21]
     mov     [edx+21], cl
     mov     cl, [eax+22]
     mov     [edx+22], cl
     jmp     @Exit
@ForwardCaseCount24 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     jmp     @Exit
@ForwardCaseCount25 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     mov     cl, [eax+24]
     mov     [edx+24], cl
     jmp     @Exit
@ForwardCaseCount26 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     mov     cl, [eax+24]
     mov     [edx+24], cl
     mov     cl, [eax+25]
     mov     [edx+25], cl
     jmp     @Exit
@ForwardCaseCount27 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     mov     cl, [eax+24]
     mov     [edx+24], cl
     mov     cl, [eax+25]
     mov     [edx+25], cl
     mov     cl, [eax+26]
     mov     [edx+26], cl
     jmp     @Exit
@ForwardCaseCount28 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     jmp     @Exit
@ForwardCaseCount29 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     cl, [eax+28]
     mov     [edx+28], cl
     jmp     @Exit
@ForwardCaseCount30 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     cl, [eax+29]
     mov     [edx+29], cl
     jmp     @Exit
@ForwardCaseCount31 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     cl, [eax+29]
     mov     [edx+29], cl
     mov     cl, [eax+30]
     mov     [edx+30], cl
     jmp     @Exit
@ForwardCaseCount32 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     jmp     @Exit
@ForwardCaseCount33 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     cl, [eax+32]
     mov     [edx+32], cl
     jmp     @Exit
@ForwardCaseCount34 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     cl, [eax+32]
     mov     [edx+32], cl
     mov     cl, [eax+33]
     mov     [edx+33], cl
     jmp     @Exit
@ForwardCaseCount35 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     cl, [eax+32]
     mov     [edx+32], cl
     mov     cl, [eax+33]
     mov     [edx+33], cl
     mov     cl, [eax+34]
     mov     [edx+34], cl
     jmp     @Exit
@ForwardCaseCount36 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     jmp     @Exit
@ForwardCaseCount37 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     cl, [eax+36]
     mov     [edx+36], cl
     jmp     @Exit
@ForwardCaseCount38 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     cl, [eax+37]
     mov     [edx+37], cl
     jmp     @Exit
@ForwardCaseCount39 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     cl, [eax+37]
     mov     [edx+37], cl
     mov     cl, [eax+38]
     mov     [edx+38], cl
     jmp     @Exit
@ForwardCaseCount40 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     jmp     @Exit
@ForwardCaseCount41 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     mov     cl, [eax+40]
     mov     [edx+40], cl
     jmp     @Exit
@ForwardCaseCount42 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     mov     cl, [eax+40]
     mov     [edx+40], cl
     mov     cl, [eax+41]
     mov     [edx+41], cl
     jmp     @Exit
@ForwardCaseCount43 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     mov     cl, [eax+40]
     mov     [edx+40], cl
     mov     cl, [eax+41]
     mov     [edx+41], cl
     mov     cl, [eax+42]
     mov     [edx+42], cl
     jmp     @Exit
@ForwardCaseCount44 :
     movdqu  xmm0, [eax]
     movdqu  [edx], xmm0
     movdqu  xmm2, [eax+16]
     movdqu  [edx+16], xmm2
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     jmp     @Exit
@ForwardCaseCount45 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     cl, [eax+44]
     mov     [edx+44], cl
     jmp     @Exit
@ForwardCaseCount46 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     cl, [eax+45]
     mov     [edx+45], cl
     jmp     @Exit
@ForwardCaseCount47 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     cl, [eax+45]
     mov     [edx+45], cl
     mov     cl, [eax+46]
     mov     [edx+46], cl
     jmp     @Exit
@ForwardCaseCount48 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     movq    xmm5, [eax+40]
     movq    [edx+40], xmm5
     jmp     @Exit
@ForwardCaseCount49 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     movq    xmm5, [eax+40]
     movq    [edx+40], xmm5
     mov     cl, [eax+48]
     mov     [edx+48], cl
     jmp     @Exit
@ForwardCaseCount50 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     movq    xmm5, [eax+40]
     movq    [edx+40], xmm5
     mov     cl, [eax+48]
     mov     [edx+48], cl
     mov     cl, [eax+49]
     mov     [edx+49], cl
     jmp     @Exit
@ForwardCaseCount51 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     movq    xmm5, [eax+40]
     movq    [edx+40], xmm5
     mov     cl, [eax+48]
     mov     [edx+48], cl
     mov     cl, [eax+49]
     mov     [edx+49], cl
     mov     cl, [eax+50]
     mov     [edx+50], cl
     jmp     @Exit
@ForwardCaseCount52 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     cl, [eax+48]
     mov     [edx+48], cl
     mov     cl, [eax+49]
     mov     [edx+49], cl
     mov     cl, [eax+50]
     mov     [edx+50], cl
     mov     cl, [eax+51]
     mov     [edx+51], cl
     jmp     @Exit
@ForwardCaseCount53 :
     movq    xmm0, [eax]
     movq    [edx], xmm0
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm4, [eax+32]
     movq    [edx+32], xmm4
     movq    xmm5, [eax+40]
     movq    [edx+40], xmm5
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     cl, [eax+52]
     mov     [edx+52], cl
     jmp     @Exit
@ForwardCaseCount54 :
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     ecx, [eax+$04]
     mov     [edx+$04], ecx
     mov     ecx, [eax+$08]
     mov     [edx+$08], ecx
     mov     ecx, [eax+$0c]
     mov     [edx+$0c], ecx
     mov     ecx, [eax+$10]
     mov     [edx+$10], ecx
     mov     ecx, [eax+$14]
     mov     [edx+$14], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     cl, [eax+53]
     mov     [edx+53], cl
     jmp     @Exit
@ForwardCaseCount55 :
     movdqu  xmm0, [eax]
     movdqu  [edx], xmm0
     movdqu  xmm1, [eax+16]
     movdqu  [edx+16], xmm1
     movdqu  xmm3, [eax+32]
     movdqu  [edx+32], xmm3
     mov     ecx,[eax+48]
     mov     [edx+48],ecx
     mov     cl,[eax+52]
     mov     [edx+52],cl
     mov     cl,[eax+53]
     mov     [edx+53],cl
     mov     cl,[eax+54]
     mov     [edx+54],cl
     jmp     @Exit
 @ForwardCaseElse:
     cmp     ecx, SPLIT1
     ja      @Else9
     push    ebx                     // Pop is done before jmp to exit1
     push    esi                     // Pop is done before jmp to exit1
     sub     ecx, 7
     xor     esi,esi
 @L11:
     mov     ebx, [eax+esi]
     mov     [edx+esi], ebx
     mov     ebx, [eax+esi+4]
     mov     [edx+esi+4], ebx
     add     esi, 8
     cmp     esi, ecx
     jb      @L11
     add     ecx, 7
     sub     esi, 1
 @L3:
     mov     bl, [eax+esi]
     mov     [edx+esi], bl
     add     esi, 1
     cmp     esi, ecx
     jb      @L3
     pop     esi
     pop     ebx
     jmp     @Exit1
 @Else9:
     push    ebx                     // Pop is done before jmp to exit1
     push    edi                     // Pop is done before jmp to exit1
     push    esi                     // Pop is done before jmp to exit1
     //Align destination
     mov     ebx, edx
     and     ebx, $0f
     mov     esi, 16
     sub     esi, ebx
     test    esi, esi
     jz      @L222
     xor     edi, edi
 @L111:
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jae     @ByteMoveExit
     mov     bl, [eax+edi]
     mov     [edx+edi], bl
     add     edi, 1
     cmp     edi, esi
     jb      @L111
 @ByteMoveExit :
     add     eax, edi                // SrcB is aligned now - "SrcI := SrcI + ByteNo1;" Not valid Pascal
     add     edx, edi                // DstB is aligned now - "DstI := DstI + ByteNo1;" Not valid Pascal
     sub     ecx, edi                // Count := Count - ByteNo1;
 @L222:
     //SourceAddress2 := Cardinal(@SrcB[0]);
     //if (SourceAddress2 mod 16 = 0) then //Both source and destination are 16 byte aligned
     mov     ebx, eax
     and     ebx, $0f
     jnz     @ElseIf1
     cmp     ecx, L2CACHESIZE
     jnl     @Else2
     sub     ecx, 47
     xor     ebx, ebx
 @L1111:
     movaps  xmm0, [eax+ebx]
     movaps  [edx+ebx], xmm0
     movaps  xmm1, [eax+ebx+16]
     movaps  [edx+ebx+16], xmm1
     movaps  xmm2, [eax+ebx+32]
     movaps  [edx+ebx+32], xmm2
     add     ebx, 48
     cmp     ebx, ecx
     jb      @L1111
     add     ecx, 47
     // Small moves after big 16 byte aligned moves
     sub     ebx,1
     mov     edi, ecx
@L123:
     mov     cl, [eax+ebx]           // DstB[ByteNo] := SrcB[ByteNo];
     mov     [edx+ebx], cl
     add     ebx, 1
     cmp     ebx, edi
     jb      @L123                   // until(ByteNo >= EndOfByteMoves);
 @L234:
     pop     esi
     pop     edi
     pop     ebx
     jmp     @Exit1
 @Else2:
     sub     ecx,127
     xor     ebx, ebx
 @L10:
     movdqa  xmm0, [eax+ebx]
     movntdq [edx+ebx], xmm0
     movdqa  xmm1, [eax+ebx+16]
     movntdq [edx+ebx+16], xmm1
     movdqa  xmm2, [eax+ebx+32]
     movntdq [edx+ebx+32], xmm2
     movdqa  xmm3, [eax+ebx+48]
     movntdq [edx+ebx+48], xmm3
     movdqa  xmm4, [eax+ebx+64]
     movntdq [edx+ebx+64], xmm4
     movdqa  xmm5, [eax+ebx+80]
     movntdq [edx+ebx+80], xmm5
     movdqa  xmm6, [eax+ebx+96]
     movntdq [edx+ebx+96], xmm6
     movdqa  xmm7, [eax+ebx+112]
     movntdq [edx+ebx+112], xmm7
     add     ebx, 128                 //  4, 8, 12, 16, 20, 24, 28, 32
     cmp     ebx, ecx
     jb      @L10
     add     ecx, 127
     sfence
     mov     edi, ecx
     sub     ebx, 1
 @ByteMove1:
     mov     cl, [eax+ebx]           // DstB[ByteNo] := SrcB[ByteNo];
     mov     [edx+ebx], cl
     add     ebx, 1
     cmp     ebx, edi
     jb      @ByteMove1                   // until(ByteNo >= EndOfByteMoves);
     pop     esi
     pop     edi
     pop     ebx
     jmp     @Exit1
     //else if (SourceAddress2 mod 8 = 0) then //Both source and destination are at least 8 byte aligned
 @ElseIf1:
     cmp     ecx, L2CACHESIZE
     ja      @ElseDennis
     mov     ebx, eax
     and     ebx, $07
     test    ebx, ebx
     jnz     @Else1
     xor     ebx, ebx
     sub     ecx, 31
 @L100:
     movq    xmm0, [eax+ebx]
     movq    [edx+ebx], xmm0
     movq    xmm1, [eax+ebx+8]
     movq    [edx+ebx+8], xmm1
     movq    xmm2, [eax+ebx+16]
     movq    [edx+ebx+16], xmm2
     movq    xmm3, [eax+ebx+24]
     movq    [edx+ebx+24], xmm3
     add     ebx, 32
     cmp     ebx, ecx
     jb      @L100
     add     ecx, 31
 @L200:
     add     eax, ebx
     add     edx, ebx
     sub     ecx, ebx
     pop     esi
     pop     edi
     pop     ebx
     jmp     dword ptr [ecx*4+@Case1JmpTable]
 @Else1:
     sub     ecx, 31
     xor     ebx, ebx
 @L1000:
     //movdqu  xmm0, [eax+ebx]
     db $F2 db $0F db $F0 db $04 db $03
     movdqa  [edx+ebx], xmm0
     //movdqu  xmm1, [eax+ebx+16]
     db $F2 db $0F db $F0 db $4C db $03 db $10
     movdqa  [edx+ebx+16], xmm1
     add     ebx, 32
     cmp     ebx, ecx
     jb      @L1000
     add     ecx, 31
     sub     ebx, 1
     mov     edi, ecx
 @ByteMove2:
     mov     cl, [eax+ebx]
     mov     [edx+ebx], cl
     add     ebx, 1
     cmp     ebx, edi
     jb      @ByteMove2
     pop     esi
     pop     edi
     pop     ebx
     jmp     @Exit1
 @ElseDennis:
     sub     ecx,127
     xor     ebx, ebx
 @Yps3:
     //movdqu  xmm0, [eax+ebx]
     db $F2 db $0F db $F0 db $04 db $03
     movntdq [edx+ebx], xmm0
     //movdqu  xmm1, [eax+ebx+16]
     db $F2 db $0F db $F0 db $4C db $03 db $10
     movntdq [edx+ebx+16], xmm1
     //movdqu  xmm2, [eax+ebx+32]
     db $F2 db $0F db $F0 db $54 db $03 db $20
     movntdq [edx+ebx+32], xmm2
     //movdqu  xmm3, [eax+ebx+48]
     db $F2 db $0F db $F0 db $5C db $03 db $30
     movntdq [edx+ebx+48], xmm3
     //movdqu  xmm4, [eax+ebx+64]
     db $F2 db $0F db $F0 db $64 db $03 db $40
     movntdq [edx+ebx+64], xmm4
     //movdqu  xmm5, [eax+ebx+80]
     db $F2 db $0F db $F0 db $6C db $03 db $50
     movntdq [edx+ebx+80], xmm5
     //movdqu  xmm6, [eax+ebx+96]
     db $F2 db $0F db $F0 db $74 db $03 db $60
     movntdq [edx+ebx+96], xmm6
     //movdqu  xmm7, [eax+ebx+112]
     db $F2 db $0F db $F0 db $7C db $03 db $70
     movntdq [edx+ebx+112], xmm7
     add     ebx, 128                 //  4, 8, 12, 16, 20, 24, 28, 32
     cmp     ebx, ecx
     jb      @Yps3
     add     ecx, 127
     sfence
     mov     edi, ecx
     sub     ebx, 1
 @ByteMove7:
     mov     cl, [eax+ebx]           // DstB[ByteNo] := SrcB[ByteNo];
     mov     [edx+ebx], cl
     add     ebx, 1
     cmp     ebx, edi
     jb      @ByteMove7                   // until(ByteNo >= EndOfByteMoves);
     pop     esi
     pop     edi
     pop     ebx
     jmp     @Exit1
 @ForwardCaseEnd:
 @RewMove:
     cmp     ecx,$37
     jnbe    @Case1Else
     jmp     dword ptr [ecx*4+@Case2JmpTable]
 @RewerseCaseCount0:
     jmp     @Exit
 @RewerseCaseCount1:
     mov     cl, [eax]
     mov     [edx], cl
     jmp     @Exit
 @RewerseCaseCount2:
     mov     cl, [eax+1]
     mov     [edx+1], cl
     mov     cl, [eax]
     mov     [edx], cl
     jmp     @Exit
 @RewerseCaseCount3:
     mov     cl, [eax+2]
     mov     [edx+2], cl
     mov     cl, [eax+1]
     mov     [edx+1], cl
     mov     cl, [eax]
     mov     [edx], cl
     jmp     @Exit
 @RewerseCaseCount4:
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
 @RewerseCaseCount5:
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
 @RewerseCaseCount6:
     mov     cl, [eax+5]
     mov     [edx+5], cl
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
 @RewerseCaseCount7:
     mov     cl, [eax+6]
     mov     [edx+6], cl
     mov     cl, [eax+5]
     mov     [edx+5], cl
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
 @RewerseCaseCount8:
     movq    xmm0, [eax]
     movq    [edx], xmm0
     jmp     @Exit
 @RewerseCaseCount9:
     mov     cl, [eax+8]
     mov     [edx+8], cl
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
 @RewerseCaseCount10:
     mov     cl, [eax+9]
     mov     [edx+9], cl
     mov     cl, [eax+8]
     mov     [edx+8], cl
     movq    xmm0, [eax]
     movq    [edx], xmm0
     jmp     @Exit
 @RewerseCaseCount11:
     mov     cl, [eax+10]
     mov     [edx+10], cl
     mov     cl, [eax+9]
     mov     [edx+9], cl
     mov     cl, [eax+8]
     mov     [edx+8], cl
     movq    xmm0, [eax]
     movq    [edx], xmm0
     jmp     @Exit
 @RewerseCaseCount12 :
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
 @RewerseCaseCount13 :
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount14 :
     mov     cl, [eax+13]
     mov     [edx+13], cl
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount15 :
     mov     cl, [eax+14]
     mov     [edx+14], cl
     mov     cl, [eax+13]
     mov     [edx+13], cl
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount16 :
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm0, [eax]
     movq    [edx], xmm0
     jmp     @Exit
@RewerseCaseCount17 :
     mov     cl, [eax+16]
     mov     [edx+16], cl
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm0, [eax]
     movq    [edx], xmm0
     jmp     @Exit
@RewerseCaseCount18 :
     mov     cl, [eax+17]
     mov     [edx+17], cl
     mov     cl, [eax+16]
     mov     [edx+16], cl
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount19 :
     mov     cl, [eax+18]
     mov     [edx+18], cl
     mov     cl, [eax+17]
     mov     [edx+17], cl
     mov     cl, [eax+16]
     mov     [edx+16], cl
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount20 :
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount21 :
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount22 :
     mov     cl, [eax+21]
     mov     [edx+21], cl
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount23 :
     mov     cl, [eax+22]
     mov     [edx+22], cl
     mov     cl, [eax+21]
     mov     [edx+21], cl
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount24 :
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount25 :
     mov     cl, [eax+24]
     mov     [edx+24], cl
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount26 :
     mov     cl, [eax+25]
     mov     [edx+25], cl
     mov     cl, [eax+24]
     mov     [edx+24], cl
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount27 :
     mov     cl, [eax+26]
     mov     [edx+26], cl
     mov     cl, [eax+25]
     mov     [edx+25], cl
     mov     cl, [eax+24]
     mov     [edx+24], cl
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount28 :
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount29 :
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount30 :
     mov     cl, [eax+29]
     mov     [edx+29], cl
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount31 :
     mov     cl, [eax+30]
     mov     [edx+30], cl
     mov     cl, [eax+29]
     mov     [edx+29], cl
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount32 :
     movq    xmm3, [eax+24]
     movq    [edx+24], xmm3
     movq    xmm2, [eax+16]
     movq    [edx+16], xmm2
     movq    xmm1, [eax+8]
     movq    [edx+8], xmm1
     movq    xmm0, [eax]
     movq    [edx], xmm0
     jmp     @Exit
@RewerseCaseCount33 :
     mov     cl, [eax+32]
     mov     [edx+32], cl
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount34 :
     mov     cl, [eax+33]
     mov     [edx+33], cl
     mov     cl, [eax+32]
     mov     [edx+32], cl
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount35 :
     mov     cl, [eax+34]
     mov     [edx+34], cl
     mov     cl, [eax+33]
     mov     [edx+33], cl
     mov     cl, [eax+32]
     mov     [edx+32], cl
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount36 :
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount37 :
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount38 :
     mov     cl, [eax+37]
     mov     [edx+37], cl
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount39 :
     mov     cl, [eax+38]
     mov     [edx+38], cl
     mov     cl, [eax+37]
     mov     [edx+37], cl
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount40 :
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount41 :
     mov     cl, [eax+40]
     mov     [edx+40], cl
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount42 :
     mov     cl, [eax+41]
     mov     [edx+41], cl
     mov     cl, [eax+40]
     mov     [edx+40], cl
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount43 :
     mov     cl, [eax+42]
     mov     [edx+42], cl
     mov     cl, [eax+41]
     mov     [edx+41], cl
     mov     cl, [eax+40]
     mov     [edx+40], cl
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount44 :
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount45 :
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount46 :
     mov     cl, [eax+45]
     mov     [edx+45], cl
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount47 :
     mov     cl, [eax+46]
     mov     [edx+46], cl
     mov     cl, [eax+45]
     mov     [edx+45], cl
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount48 :
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount49 :
     mov     cl, [eax+48]
     mov     [edx+48], cl
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount50 :
     mov     cl, [eax+49]
     mov     [edx+49], cl
     mov     cl, [eax+48]
     mov     [edx+48], cl
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount51 :
     mov     cl, [eax+50]
     mov     [edx+50], cl
     mov     cl, [eax+49]
     mov     [edx+49], cl
     mov     cl, [eax+48]
     mov     [edx+48], cl
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount52 :
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount53 :
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount54 :
     mov     cl, [eax+53]
     mov     [edx+53], cl
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
@RewerseCaseCount55 :
     mov     cl, [eax+54]
     mov     [edx+54], cl
     mov     cl, [eax+53]
     mov     [edx+53], cl
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     ecx, [eax+44]
     mov     [edx+44], ecx
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     ecx, [eax+36]
     mov     [edx+36], ecx
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     ecx, [eax+28]
     mov     [edx+28], ecx
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     ecx, [eax+20]
     mov     [edx+20], ecx
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     ecx, [eax+12]
     mov     [edx+12], ecx
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     jmp     @Exit
 @Case1Else:
     //Align destination
     push    edi
     push    esi
     push    ebx
     mov     edi, edx
     add     edi, ecx
     and     edi, $0f
     test    edi, edi
     jz      @Aligned
     mov     esi, ecx
     sub     esi, edi
     sub     ecx, 1
     sub     esi, 1
 @L15:
     mov     bl, [eax+ecx]
     mov     [edx+ecx], bl
     sub     ecx, 1
     cmp     ecx, esi
     ja      @L15
     add     ecx, 1
 @Aligned :
     sub     ecx, 32
 @L35:
     //movdqu  xmm0, [eax+ecx+16]
     db $F2 db $0F db $F0 db $44 db $01 db $10
     movdqa  [edx+ecx+16], xmm0
     //movdqu  xmm0, [eax+ecx]
     db $F2 db $0F db $F0 db $04 db $01
     movdqa  [edx+ecx], xmm0
     sub     ecx, 32
     jns     @L35
     add     ecx, 32
     pop     ebx
     pop     esi
     pop     edi
     jmp     dword ptr [ecx*4+@Case2JmpTable]
 @L25:
     pop     ebx
     pop     esi
     pop     edi
 @Exit1:
 @Exit:
     ret

@Case1JmpTable:
 dd @ForwardCaseCount0
 dd @ForwardCaseCount1
 dd @ForwardCaseCount2
 dd @ForwardCaseCount3
 dd @ForwardCaseCount4
 dd @ForwardCaseCount5
 dd @ForwardCaseCount6
 dd @ForwardCaseCount7
 dd @ForwardCaseCount8
 dd @ForwardCaseCount9
 dd @ForwardCaseCount10
 dd @ForwardCaseCount11
 dd @ForwardCaseCount12
 dd @ForwardCaseCount13
 dd @ForwardCaseCount14
 dd @ForwardCaseCount15
 dd @ForwardCaseCount16
 dd @ForwardCaseCount17
 dd @ForwardCaseCount18
 dd @ForwardCaseCount19
 dd @ForwardCaseCount20
 dd @ForwardCaseCount21
 dd @ForwardCaseCount22
 dd @ForwardCaseCount23
 dd @ForwardCaseCount24
 dd @ForwardCaseCount25
 dd @ForwardCaseCount26
 dd @ForwardCaseCount27
 dd @ForwardCaseCount28
 dd @ForwardCaseCount29
 dd @ForwardCaseCount30
 dd @ForwardCaseCount31
 dd @ForwardCaseCount32
 dd @ForwardCaseCount33
 dd @ForwardCaseCount34
 dd @ForwardCaseCount35
 dd @ForwardCaseCount36
 dd @ForwardCaseCount37
 dd @ForwardCaseCount38
 dd @ForwardCaseCount39
 dd @ForwardCaseCount40
 dd @ForwardCaseCount41
 dd @ForwardCaseCount42
 dd @ForwardCaseCount43
 dd @ForwardCaseCount44
 dd @ForwardCaseCount45
 dd @ForwardCaseCount46
 dd @ForwardCaseCount47
 dd @ForwardCaseCount48
 dd @ForwardCaseCount49
 dd @ForwardCaseCount50
 dd @ForwardCaseCount51
 dd @ForwardCaseCount52
 dd @ForwardCaseCount53
 dd @ForwardCaseCount54
 dd @ForwardCaseCount55

@Case2JmpTable:
 dd @RewerseCaseCount0
 dd @RewerseCaseCount1
 dd @RewerseCaseCount2
 dd @RewerseCaseCount3
 dd @RewerseCaseCount4
 dd @RewerseCaseCount5
 dd @RewerseCaseCount6
 dd @RewerseCaseCount7
 dd @RewerseCaseCount8
 dd @RewerseCaseCount9
 dd @RewerseCaseCount10
 dd @RewerseCaseCount11
 dd @RewerseCaseCount12
 dd @RewerseCaseCount13
 dd @RewerseCaseCount14
 dd @RewerseCaseCount15
 dd @RewerseCaseCount16
 dd @RewerseCaseCount17
 dd @RewerseCaseCount18
 dd @RewerseCaseCount19
 dd @RewerseCaseCount20
 dd @RewerseCaseCount21
 dd @RewerseCaseCount22
 dd @RewerseCaseCount23
 dd @RewerseCaseCount24
 dd @RewerseCaseCount25
 dd @RewerseCaseCount26
 dd @RewerseCaseCount27
 dd @RewerseCaseCount28
 dd @RewerseCaseCount29
 dd @RewerseCaseCount30
 dd @RewerseCaseCount31
 dd @RewerseCaseCount32
 dd @RewerseCaseCount33
 dd @RewerseCaseCount34
 dd @RewerseCaseCount35
 dd @RewerseCaseCount36
 dd @RewerseCaseCount37
 dd @RewerseCaseCount38
 dd @RewerseCaseCount39
 dd @RewerseCaseCount40
 dd @RewerseCaseCount41
 dd @RewerseCaseCount42
 dd @RewerseCaseCount43
 dd @RewerseCaseCount44
 dd @RewerseCaseCount45
 dd @RewerseCaseCount46
 dd @RewerseCaseCount47
 dd @RewerseCaseCount48
 dd @RewerseCaseCount49
 dd @RewerseCaseCount50
 dd @RewerseCaseCount51
 dd @RewerseCaseCount52
 dd @RewerseCaseCount53
 dd @RewerseCaseCount54
 dd @RewerseCaseCount55

end;

//Author:            John O'Harrow

{-------------------------------------------------------------------------}
{Perform Forward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure SmallForwardMove_3;
asm
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd01,@@Fwd02,@@Fwd03,@@Fwd04,@@Fwd05,@@Fwd06,@@Fwd07,@@Fwd08
  dd      @@Fwd09,@@Fwd10,@@Fwd11,@@Fwd12,@@Fwd13,@@Fwd14,@@Fwd15,@@Fwd16
  dd      @@Fwd17,@@Fwd18,@@Fwd19,@@Fwd20,@@Fwd21,@@Fwd22,@@Fwd23,@@Fwd24
  dd      @@Fwd25,@@Fwd26,@@Fwd27,@@Fwd28,@@Fwd29,@@Fwd30,@@Fwd31,@@Fwd32
  dd      @@Fwd33,@@Fwd34,@@Fwd35,@@Fwd36
@@Fwd36:
  mov     ecx,[eax-36]
  mov     [edx-36],ecx
@@Fwd32:
  mov     ecx,[eax-32]
  mov     [edx-32],ecx
@@Fwd28:
  mov     ecx,[eax-28]
  mov     [edx-28],ecx
@@Fwd24:
  mov     ecx,[eax-24]
  mov     [edx-24],ecx
@@Fwd20:
  mov     ecx,[eax-20]
  mov     [edx-20],ecx
@@Fwd16:
  mov     ecx,[eax-16]
  mov     [edx-16],ecx
@@Fwd12:
  mov     ecx,[eax-12]
  mov     [edx-12],ecx
@@Fwd08:
  mov     ecx,[eax-8]
  mov     [edx-8],ecx
@@Fwd04:
  mov     ecx,[eax-4]
  mov     [edx-4],ecx
  ret
@@Fwd35:
  mov     ecx,[eax-35]
  mov     [edx-35],ecx
@@Fwd31:
  mov     ecx,[eax-31]
  mov     [edx-31],ecx
@@Fwd27:
  mov     ecx,[eax-27]
  mov     [edx-27],ecx
@@Fwd23:
  mov     ecx,[eax-23]
  mov     [edx-23],ecx
@@Fwd19:
  mov     ecx,[eax-19]
  mov     [edx-19],ecx
@@Fwd15:
  mov     ecx,[eax-15]
  mov     [edx-15],ecx
@@Fwd11:
  mov     ecx,[eax-11]
  mov     [edx-11],ecx
@@Fwd07:
  mov     ecx,[eax-7]
  mov     [edx-7],ecx
  mov     ecx,[eax-4] 
  mov     [edx-4],ecx
  ret
@@Fwd03:
  movzx   ecx, word ptr [eax-3]
  mov     [edx-3],cx
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1],cl
  ret
@@Fwd34:
  mov     ecx,[eax-34]
  mov     [edx-34],ecx
@@Fwd30:
  mov     ecx,[eax-30]
  mov     [edx-30],ecx
@@Fwd26:
  mov     ecx,[eax-26]
  mov     [edx-26],ecx
@@Fwd22:
  mov     ecx,[eax-22]
  mov     [edx-22],ecx
@@Fwd18:
  mov     ecx,[eax-18]
  mov     [edx-18],ecx
@@Fwd14:
  mov     ecx,[eax-14]
  mov     [edx-14],ecx
@@Fwd10:
  mov     ecx,[eax-10]
  mov     [edx-10],ecx
@@Fwd06:
  mov     ecx,[eax-6]
  mov     [edx-6],ecx
@@Fwd02:
  movzx   ecx, word ptr [eax-2]
  mov     [edx-2],cx
  ret
@@Fwd33:
  mov     ecx,[eax-33]
  mov     [edx-33],ecx
@@Fwd29:
  mov     ecx,[eax-29]
  mov     [edx-29],ecx
@@Fwd25:
  mov     ecx,[eax-25]
  mov     [edx-25],ecx
@@Fwd21:
  mov     ecx,[eax-21]
  mov     [edx-21],ecx
@@Fwd17:
  mov     ecx,[eax-17]
  mov     [edx-17],ecx
@@Fwd13:
  mov     ecx,[eax-13]
  mov     [edx-13],ecx
@@Fwd09:
  mov     ecx,[eax-9]
  mov     [edx-9],ecx
@@Fwd05:
  mov     ecx,[eax-5]
  mov     [edx-5],ecx
@@Fwd01:
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1],cl
@@Done:
end; {SmallForwardMove}

{-------------------------------------------------------------------------}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure SmallBackwardMove_3;
asm
  jmp     dword ptr [@@BwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@BwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Bwd01,@@Bwd02,@@Bwd03,@@Bwd04,@@Bwd05,@@Bwd06,@@Bwd07,@@Bwd08
  dd      @@Bwd09,@@Bwd10,@@Bwd11,@@Bwd12,@@Bwd13,@@Bwd14,@@Bwd15,@@Bwd16
  dd      @@Bwd17,@@Bwd18,@@Bwd19,@@Bwd20,@@Bwd21,@@Bwd22,@@Bwd23,@@Bwd24
  dd      @@Bwd25,@@Bwd26,@@Bwd27,@@Bwd28,@@Bwd29,@@Bwd30,@@Bwd31,@@Bwd32
  dd      @@Bwd33,@@Bwd34,@@Bwd35,@@Bwd36
@@Bwd36:
  mov     ecx,[eax+32]
  mov     [edx+32],ecx
@@Bwd32:
  mov     ecx,[eax+28]
  mov     [edx+28],ecx
@@Bwd28:
  mov     ecx,[eax+24]
  mov     [edx+24],ecx
@@Bwd24:
  mov     ecx,[eax+20]
  mov     [edx+20],ecx
@@Bwd20:
  mov     ecx,[eax+16]
  mov     [edx+16],ecx
@@Bwd16:
  mov     ecx,[eax+12]
  mov     [edx+12],ecx
@@Bwd12:
  mov     ecx,[eax+8]
  mov     [edx+8],ecx
@@Bwd08:
  mov     ecx,[eax+4]
  mov     [edx+4],ecx
@@Bwd04:
  mov     ecx,[eax]
  mov     [edx],ecx
  ret
@@Bwd35:
  mov     ecx,[eax+31]
  mov     [edx+31],ecx
@@Bwd31:
  mov     ecx,[eax+27]
  mov     [edx+27],ecx
@@Bwd27:
  mov     ecx,[eax+23]
  mov     [edx+23],ecx
@@Bwd23:
  mov     ecx,[eax+19]
  mov     [edx+19],ecx
@@Bwd19:
  mov     ecx,[eax+15]
  mov     [edx+15],ecx
@@Bwd15:
  mov     ecx,[eax+11]
  mov     [edx+11],ecx
@@Bwd11:
  mov     ecx,[eax+7]
  mov     [edx+7],ecx
@@Bwd07:
  mov     ecx,[eax+3]
  mov     [edx+3],ecx
  mov     ecx,[eax]
  mov     [edx],ecx
  ret
@@Bwd03:
  movzx   ecx, word ptr [eax+1]
  mov     [edx+1],cx
  movzx   ecx, byte ptr [eax]
  mov     [edx],cl
  ret
@@Bwd34:
  mov     ecx,[eax+30]
  mov     [edx+30],ecx
@@Bwd30:
  mov     ecx,[eax+26]
  mov     [edx+26],ecx
@@Bwd26:
  mov     ecx,[eax+22]
  mov     [edx+22],ecx
@@Bwd22:
  mov     ecx,[eax+18]
  mov     [edx+18],ecx
@@Bwd18:
  mov     ecx,[eax+14]
  mov     [edx+14],ecx
@@Bwd14:
  mov     ecx,[eax+10]
  mov     [edx+10],ecx
@@Bwd10:
  mov     ecx,[eax+6]
  mov     [edx+6],ecx
@@Bwd06:
  mov     ecx,[eax+2]
  mov     [edx+2],ecx
@@Bwd02:
  movzx   ecx, word ptr [eax]
  mov     [edx],cx
  ret
@@Bwd33:
  mov     ecx,[eax+29]
  mov     [edx+29],ecx
@@Bwd29:
  mov     ecx,[eax+25]
  mov     [edx+25],ecx
@@Bwd25:
  mov     ecx,[eax+21]
  mov     [edx+21],ecx
@@Bwd21:
  mov     ecx,[eax+17]
  mov     [edx+17],ecx
@@Bwd17:
  mov     ecx,[eax+13]
  mov     [edx+13],ecx
@@Bwd13:
  mov     ecx,[eax+9]
  mov     [edx+9],ecx
@@Bwd09:
  mov     ecx,[eax+5]
  mov     [edx+5],ecx
@@Bwd05:
  mov     ecx,[eax+1]
  mov     [edx+1],ecx
@@Bwd01:
  movzx   ecx, byte ptr[eax]
  mov     [edx],cl
@@Done:
end; {SmallBackwardMove}

const
  SMALLMOVESIZE = 36;

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure AlignedFwdMoveSSE_3(const Source; var Dest; Count: Integer);
const
  Prefetch = 512;
asm
  push    esi
  mov     esi,eax             {ESI = Source}
  mov     eax,ecx             {EAX = Count}
  and     eax,-128            {EAX = No of Bytes to Block Move}
  add     esi,eax
  add     edx,eax
  shr     eax,3               {EAX = No of QWORD's to Block Move}
  neg     eax
  cmp     eax, -(32*1024)     {Count > 256K}
  jl      @Large
@Small: {Count<=256K}
  test    esi,15              {Check if Both Source/Dest Aligned}
  jnz     @SmallUnaligned
@SmallAligned:                {Both Source and Dest 16-Byte Aligned}
@SmallAlignedLoop:
  movaps  xmm0,[esi+8*eax]
  movaps  xmm1,[esi+8*eax+16]
  movaps  xmm2,[esi+8*eax+32]
  movaps  xmm3,[esi+8*eax+48]
  movaps  [edx+8*eax],xmm0
  movaps  [edx+8*eax+16],xmm1
  movaps  [edx+8*eax+32],xmm2
  movaps  [edx+8*eax+48],xmm3
  movaps  xmm4,[esi+8*eax+64]
  movaps  xmm5,[esi+8*eax+80]
  movaps  xmm6,[esi+8*eax+96]
  movaps  xmm7,[esi+8*eax+112]
  movaps  [edx+8*eax+64],xmm4
  movaps  [edx+8*eax+80],xmm5
  movaps  [edx+8*eax+96],xmm6
  movaps  [edx+8*eax+112],xmm7
  add     eax,16
  js      @SmallAlignedLoop
  jmp     @Remainder
@SmallUnaligned:              {Source Not 16-Byte Aligned}
@SmallUnalignedLoop:
  movups  xmm0,[esi+8*eax]
  movups  xmm1,[esi+8*eax+16]
  movups  xmm2,[esi+8*eax+32]
  movups  xmm3,[esi+8*eax+48]
  movaps  [edx+8*eax],xmm0
  movaps  [edx+8*eax+16],xmm1
  movaps  [edx+8*eax+32],xmm2
  movaps  [edx+8*eax+48],xmm3
  movups  xmm4,[esi+8*eax+64]
  movups  xmm5,[esi+8*eax+80]
  movups  xmm6,[esi+8*eax+96]
  movups  xmm7,[esi+8*eax+112]
  movaps  [edx+8*eax+64],xmm4
  movaps  [edx+8*eax+80],xmm5
  movaps  [edx+8*eax+96],xmm6
  movaps  [edx+8*eax+112],xmm7
  add     eax,16
  js      @SmallUnalignedLoop
  jmp     @Remainder
@Large: {Count>256K}
  test    esi,15              {Check if Both Source/Dest Aligned}
  jnz     @LargeUnaligned
@LargeAligned:                {Both Source and Dest 16-Byte Aligned}
@LargeAlignedLoop:
  prefetchnta  [esi+8*eax+Prefetch]
  prefetchnta  [esi+8*eax+Prefetch+64]
  movaps  xmm0,[esi+8*eax]
  movaps  xmm1,[esi+8*eax+16]
  movaps  xmm2,[esi+8*eax+32]
  movaps  xmm3,[esi+8*eax+48]
  movntps [edx+8*eax],xmm0
  movntps [edx+8*eax+16],xmm1
  movntps [edx+8*eax+32],xmm2
  movntps [edx+8*eax+48],xmm3
  movaps  xmm4,[esi+8*eax+64]
  movaps  xmm5,[esi+8*eax+80]
  movaps  xmm6,[esi+8*eax+96]
  movaps  xmm7,[esi+8*eax+112]
  movntps [edx+8*eax+64],xmm4
  movntps [edx+8*eax+80],xmm5
  movntps [edx+8*eax+96],xmm6
  movntps [edx+8*eax+112],xmm7
  add     eax,16
  js      @LargeAlignedLoop
  sfence
  jmp     @Remainder
@LargeUnaligned:              {Source Not 16-Byte Aligned}
@LargeUnalignedLoop:
  prefetchnta  [esi+8*eax+Prefetch]
  prefetchnta  [esi+8*eax+Prefetch+64]
  movups  xmm0,[esi+8*eax]
  movups  xmm1,[esi+8*eax+16]
  movups  xmm2,[esi+8*eax+32]
  movups  xmm3,[esi+8*eax+48]
  movntps [edx+8*eax],xmm0
  movntps [edx+8*eax+16],xmm1
  movntps [edx+8*eax+32],xmm2
  movntps [edx+8*eax+48],xmm3
  movups  xmm4,[esi+8*eax+64]
  movups  xmm5,[esi+8*eax+80]
  movups  xmm6,[esi+8*eax+96]
  movups  xmm7,[esi+8*eax+112]
  movntps [edx+8*eax+64],xmm4
  movntps [edx+8*eax+80],xmm5
  movntps [edx+8*eax+96],xmm6
  movntps [edx+8*eax+112],xmm7
  add     eax,16
  js      @LargeUnalignedLoop
  sfence
@Remainder:
  and     ecx,$7F {ECX = Remainder (0..112 - Multiple of 16)}
  jz      @Done
  add     esi,ecx
  add     edx,ecx
  neg     ecx
@RemainderLoop:
  movups  xmm0,[esi+ecx]
  movaps  [edx+ecx],xmm0
  add     ecx,16
  jnz     @RemainderLoop
@Done:
  pop     esi
end; {AlignedFwdMoveSSE}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_SSE_3;
const
  LARGESIZE = 2048;
asm
  cmp     ecx,LARGESIZE
  jge     @FwdLargeMove
  cmp     ecx,SMALLMOVESIZE+32
  movups  xmm0,[eax]
  jg      @FwdMoveSSE
  movups  xmm1,[eax+16]
  movups  [edx],xmm0
  movups  [edx+16],xmm1
  add     eax,ecx
  add     edx,ecx
  sub     ecx,32
  jmp     SmallForwardMove_3
@FwdMoveSSE:
  push    ebx
  mov     ebx,edx
  {Align Writes}
  add     eax,ecx
  add     ecx,edx
  add     edx,15
  and     edx,-16
  sub     ecx,edx
  add     edx,ecx
  {Now Aligned}
  sub     ecx,32
  neg     ecx
@FwdLoopSSE:
  movups  xmm1,[eax+ecx-32]
  movups  xmm2,[eax+ecx-16]
  movaps  [edx+ecx-32],xmm1
  movaps  [edx+ecx-16],xmm2
  add     ecx,32
  jle     @FwdLoopSSE
  movups  [ebx],xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx,32
  pop     ebx
  jmp     SmallForwardMove_3
@FwdLargeMove:
  push    ebx
  mov     ebx,ecx
  test    edx,15
  jz      @FwdLargeAligned
  {16 byte Align Destination}
  mov     ecx,edx
  add     ecx,15
  and     ecx,-16
  sub     ecx,edx
  add     eax,ecx
  add     edx,ecx
  sub     ebx,ecx
  {Destination now 16 Byte Aligned}
  call    SmallForwardMove_3
  mov     ecx,ebx
@FwdLargeAligned:
  and     ecx,-16
  sub     ebx,ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    AlignedFwdMoveSSE_3
  pop     ecx
  pop     eax
  pop     edx
  add     ecx,ebx
  add     eax,ecx
  add     edx,ecx
  mov     ecx,ebx
  pop     ebx
  jmp     SmallForwardMove_3
end; {Forwards_SSE}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_SSE_3;
asm
  cmp     ecx,SMALLMOVESIZE+32
  jg      @BwdMoveSSE
  sub     ecx,32
  movups  xmm1,[eax+ecx]
  movups  xmm2,[eax+ecx+16]
  movups  [edx+ecx],xmm1
  movups  [edx+ecx+16],xmm2
  jmp     SmallBackwardMove_3
@BwdMoveSSE:
  push    ebx
  movups  xmm0,[eax+ecx-16] {Last 16 Bytes}
  {Align Writes}
  lea     ebx,[edx+ecx]
  and     ebx,15
  sub     ecx,ebx
  add     ebx,ecx
  {Now Aligned}
  sub     ecx,32
@BwdLoop:
  movups  xmm1,[eax+ecx]
  movups  xmm2,[eax+ecx+16]
  movaps  [edx+ecx],xmm1
  movaps  [edx+ecx+16],xmm2
  sub     ecx,32
  jge     @BwdLoop
  movups  [edx+ebx-16],xmm0  {Last 16 Bytes}
  add     ecx,32
  pop     ebx
  jmp     SmallBackwardMove_3
end; {Backwards_SSE}

//Author:            John O'Harrow
//Optimized for:     Intel Pentium 4 Northwood
//Instructionset(s): IA32, MMX, SSE
//Original Name:     MoveJOH_SSE_3

procedure MoveFastcodeP4N(const Source; var Dest; Count : Integer);
asm
  cmp     ecx,SMALLMOVESIZE
  ja      @Large
  cmp     eax,edx
  lea     eax,[eax+ecx]
  jle     @SmallCheck
@SmallForward:
  add     edx,ecx
  jmp     SmallForwardMove_3
@SmallCheck:
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     eax,ecx
  jmp     SmallBackwardMove_3
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax,edx
  jg      Forwards_SSE_3
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  push    eax
  add     eax,ecx
  cmp     eax,edx
  pop     eax
  jg      Backwards_SSE_3 {Source/Dest Overlap}
  jmp     Forwards_SSE_3
@Done:
end; {MoveJOH_SSE}

//Author:            John O'Harrow
//Optimized for:     Intel Pentium M Banias
//Instructionset(s): IA32, MMX, SSE
//Original Name:     MoveJOH_SSE_3

procedure MoveFastcodePMB(const Source; var Dest; Count : Integer);
asm
  cmp     ecx,SMALLMOVESIZE
  ja      @Large
  cmp     eax,edx
  lea     eax,[eax+ecx]
  jle     @SmallCheck
@SmallForward:
  add     edx,ecx
  jmp     SmallForwardMove_3
@SmallCheck:
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     eax,ecx
  jmp     SmallBackwardMove_3
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax,edx
  jg      Forwards_SSE_3
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  push    eax
  add     eax,ecx
  cmp     eax,edx
  pop     eax
  jg      Backwards_SSE_3 {Source/Dest Overlap}
  jmp     Forwards_SSE_3
@Done:
end; {MoveJOH_SSE}

//Author:            John O'Harrow
//Optimized for:     AMD FX, Opteron, Athlon 64
//Instructionset(s): IA32, MMX, SSE
//Original Name:     MoveJOH_SSE_3

procedure MoveFastcodeAMD64(const Source; var Dest; Count : Integer);
asm
  cmp     ecx,SMALLMOVESIZE
  ja      @Large
  cmp     eax,edx
  lea     eax,[eax+ecx]
  jle     @SmallCheck
@SmallForward:
  add     edx,ecx
  jmp     SmallForwardMove_3
@SmallCheck:
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     eax,ecx
  jmp     SmallBackwardMove_3
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax,edx
  jg      Forwards_SSE_3
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  push    eax
  add     eax,ecx
  cmp     eax,edx
  pop     eax
  jg      Backwards_SSE_3 {Source/Dest Overlap}
  jmp     Forwards_SSE_3
@Done:
end; {MoveJOH_SSE}

//Author:            Dennis Kjaer Christensen
//Optimized for:     AMD Ahtlon XP
//Instructionset(s): IA32, MMX, SSE
//Original name:     MoveDKCSSE_1

procedure MoveFastcodeXP(const Source; var Dest; Count : Integer);
asm
     //Exit if Count is negative
     test    ecx, ecx
     js      @Exit
     //Detect the need for rewerse move in overlapped case
     cmp     eax, edx                   // if (DestAddress > SourceAddress) then
     jnb     @ForwardMove
     push    ebx
     mov     ebx, edx
     sub     ebx, eax                   // (DestAddress - SourceAddress)
     cmp     ebx, edx                   // if ((DestAddress - SourceAddress) < Count) then
     pop     ebx
     jb      @RewMove
@ForwardMove:
     cmp     ecx, 55
     jnbe    @ForwardCaseElse
     jmp     dword ptr [ecx*4+@Case1JmpTable]
@ForwardCaseCount1:
     mov     cl, [eax]
     mov     [edx], cl
@ForwardCaseCount0:
     ret
@ForwardCaseCount2:
     mov     cl, [eax]
     mov     [edx], cl
     mov     cl, [eax+1]
     mov     [edx+1], cl
     ret
@ForwardCaseCount3:
     mov     cl, [eax]
     mov     [edx], cl
     mov     cl, [eax+1]
     mov     [edx+1], cl
     mov     cl, [eax+2]
     mov     [edx+2], cl
     ret
@ForwardCaseCount4:
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
@ForwardCaseCount5:
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     cl, [eax+4]
     mov     [edx+4], cl
     ret
@ForwardCaseCount6:
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     cl, [eax+5]
     mov     [edx+5], cl
     ret
@ForwardCaseCount7:
     mov     ecx, [eax]
     mov     [edx], ecx
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     cl, [eax+5]
     mov     [edx+5], cl
     mov     cl, [eax+6]
     mov     [edx+6], cl
     ret
@ForwardCaseCount8:
     movq    mm0, [eax]
     movq    [edx], mm0
     emms
     ret
@ForwardCaseCount9:
     movq    mm0, [eax]
     movq    [edx], mm0
     mov     cl, [eax+8]
     mov     [edx+8], cl
     emms
     ret
@ForwardCaseCount10 :
     movq    mm0, [eax]
     movq    [edx], mm0
     mov     cl, [eax+8]
     mov     [edx+8], cl
     mov     cl, [eax+9]
     mov     [edx+9], cl
     emms
     ret
@ForwardCaseCount11 :
     movq    mm0, [eax]
     movq    [edx], mm0
     mov     cl, [eax+8]
     mov     [edx+8], cl
     mov     cl, [eax+9]
     mov     [edx+9], cl
     mov     cl, [eax+10]
     mov     [edx+10], cl
     emms
     ret
@ForwardCaseCount12:
     movq    mm0, [eax]
     movq    [edx], mm0
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     emms
     ret
@ForwardCaseCount13 :
     movq    mm0, [eax]
     movq    [edx], mm0
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     cl, [eax+12]
     mov     [edx+12], cl
     emms
     ret
@ForwardCaseCount14 :
     movq    mm0, [eax]
     movq    [edx], mm0
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     cl, [eax+13]
     mov     [edx+13], cl
     emms
     ret
@ForwardCaseCount15 :
     movq    mm0, [eax]
     movq    [edx], mm0
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     cl, [eax+13]
     mov     [edx+13], cl
     mov     cl, [eax+14]
     mov     [edx+14], cl
     emms
     ret
@ForwardCaseCount16 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    [edx],   mm0
     movq    [edx+8], mm1
     emms
     ret
@ForwardCaseCount17 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    [edx],   mm0
     movq    [edx+8], mm1
     mov     cl, [eax+16]
     mov     [edx+16], cl
     emms
     ret
@ForwardCaseCount18 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    [edx],   mm0
     movq    [edx+8], mm1
     mov     cl, [eax+16]
     mov     [edx+16], cl
     mov     cl, [eax+17]
     mov     [edx+17], cl
     emms
     ret
@ForwardCaseCount19 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    [edx],   mm0
     movq    [edx+8], mm1
     mov     cl, [eax+16]
     mov     [edx+16], cl
     mov     cl, [eax+17]
     mov     [edx+17], cl
     mov     cl, [eax+18]
     mov     [edx+18], cl
     emms
     ret
@ForwardCaseCount20 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    [edx],   mm0
     movq    [edx+8], mm1
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     emms
     ret
@ForwardCaseCount21 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    [edx],   mm0
     movq    [edx+8], mm1
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     cl, [eax+20]
     mov     [edx+20], cl
     emms
     ret
@ForwardCaseCount22 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    [edx],   mm0
     movq    [edx+8], mm1
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     cl, [eax+21]
     mov     [edx+21], cl
     emms
     ret
@ForwardCaseCount23 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    [edx],   mm0
     movq    [edx+8], mm1
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     cl, [eax+21]
     mov     [edx+21], cl
     mov     cl, [eax+22]
     mov     [edx+22], cl
     emms
     ret
@ForwardCaseCount24 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     emms
     ret
@ForwardCaseCount25 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     mov     cl, [eax+24]
     mov     [edx+24], cl
     emms
     ret
@ForwardCaseCount26 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     mov     cl, [eax+24]
     mov     [edx+24], cl
     mov     cl, [eax+25]
     mov     [edx+25], cl
     emms
     ret
@ForwardCaseCount27 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     mov     cl, [eax+24]
     mov     [edx+24], cl
     mov     cl, [eax+25]
     mov     [edx+25], cl
     mov     cl, [eax+26]
     mov     [edx+26], cl
     emms
     ret
@ForwardCaseCount28 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     emms
     ret
@ForwardCaseCount29 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     cl, [eax+28]
     mov     [edx+28], cl
     emms
     ret
@ForwardCaseCount30 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     cl, [eax+29]
     mov     [edx+29], cl
     emms
     ret
@ForwardCaseCount31 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     cl, [eax+29]
     mov     [edx+29], cl
     mov     cl, [eax+30]
     mov     [edx+30], cl
     emms
     ret
@ForwardCaseCount32 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     emms
     ret
@ForwardCaseCount33 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     mov     cl, [eax+32]
     mov     [edx+32], cl
     emms
     ret
@ForwardCaseCount34 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     mov     cl, [eax+32]
     mov     [edx+32], cl
     mov     cl, [eax+33]
     mov     [edx+33], cl
     emms
     ret
@ForwardCaseCount35 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     mov     cl, [eax+32]
     mov     [edx+32], cl
     mov     cl, [eax+33]
     mov     [edx+33], cl
     mov     cl, [eax+34]
     mov     [edx+34], cl
     emms
     ret
@ForwardCaseCount36 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     emms
     ret
@ForwardCaseCount37 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     cl, [eax+36]
     mov     [edx+36], cl
     emms
     ret
@ForwardCaseCount38 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     cl, [eax+37]
     mov     [edx+37], cl
     emms
     ret
@ForwardCaseCount39 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     cl, [eax+37]
     mov     [edx+37], cl
     mov     cl, [eax+38]
     mov     [edx+38], cl
     emms
     ret
@ForwardCaseCount40 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     emms
     ret
@ForwardCaseCount41 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     mov     cl, [eax+40]
     mov     [edx+40], cl
     emms
     ret
@ForwardCaseCount42 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     mov     cl, [eax+40]
     mov     [edx+40], cl
     mov     cl, [eax+41]
     mov     [edx+41], cl
     emms
     ret
@ForwardCaseCount43 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     mov     cl, [eax+40]
     mov     [edx+40], cl
     mov     cl, [eax+41]
     mov     [edx+41], cl
     mov     cl, [eax+42]
     mov     [edx+42], cl
     emms
     ret
@ForwardCaseCount44 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     emms
     ret
@ForwardCaseCount45 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     cl, [eax+44]
     mov     [edx+44], cl
     emms
     ret
@ForwardCaseCount46 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     cl, [eax+45]
     mov     [edx+45], cl
     emms
     ret
@ForwardCaseCount47 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     cl, [eax+45]
     mov     [edx+45], cl
     mov     cl, [eax+46]
     mov     [edx+46], cl
     emms
     ret
@ForwardCaseCount48 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    mm5, [eax+40]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     movq    [edx+40], mm5
     emms
     ret
@ForwardCaseCount49 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    mm5, [eax+40]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     movq    [edx+40], mm5
     mov     cl, [eax+48]
     mov     [edx+48], cl
     emms
     ret
@ForwardCaseCount50 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    mm5, [eax+40]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     movq    [edx+40], mm5
     mov     cl, [eax+48]
     mov     [edx+48], cl
     mov     cl, [eax+49]
     mov     [edx+49], cl
     emms
     ret
@ForwardCaseCount51 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    mm5, [eax+40]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     movq    [edx+40], mm5
     mov     cl, [eax+48]
     mov     [edx+48], cl
     mov     cl, [eax+49]
     mov     [edx+49], cl
     mov     cl, [eax+50]
     mov     [edx+50], cl
     emms
     ret
@ForwardCaseCount52 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    mm5, [eax+40]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     movq    [edx+40], mm5
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     emms
     ret
@ForwardCaseCount53 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    mm5, [eax+40]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     movq    [edx+40], mm5
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     cl, [eax+52]
     mov     [edx+52], cl
     emms
     ret
@ForwardCaseCount54 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    mm5, [eax+40]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     movq    [edx+40], mm5
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     cl, [eax+53]
     mov     [edx+53], cl
     emms
     ret
@ForwardCaseCount55 :
     movq    mm0, [eax]
     movq    mm1, [eax+8]
     movq    mm2, [eax+16]
     movq    mm3, [eax+24]
     movq    mm4, [eax+32]
     movq    mm5, [eax+40]
     movq    [edx],    mm0
     movq    [edx+8],  mm1
     movq    [edx+16], mm2
     movq    [edx+24], mm3
     movq    [edx+32], mm4
     movq    [edx+40], mm5
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     cl, [eax+53]
     mov     [edx+53], cl
     mov     cl,[eax+54]
     mov     [edx+54],cl
     emms
     ret
     nop
     nop
     nop
     nop
     nop
     nop
     nop
     nop
 @ForwardCaseElse:
     cmp     ecx, 800
     jnle    @Else9
     push    ebx                     // Pop is done before jmp to exit1
     push    edi                     // Pop is done before jmp to exit1
     push    esi                     // Pop is done before jmp to exit1
     mov     edi, ecx
     shr     edi, 5
     shl     edi, 5
     xor     ebx, ebx
 @L11:
     movq    mm0, [eax+ebx]
     movq    mm1, [eax+ebx+8]
     movq    mm2, [eax+ebx+16]
     movq    mm3, [eax+ebx+24]
     movq    [edx+ebx], mm0
     movq    [edx+ebx+8], mm1
     movq    [edx+ebx+16], mm2
     movq    [edx+ebx+24], mm3
     add     ebx, 32
     cmp     ebx, edi
     jb      @L11
     emms
     add     eax, edi
     add     edx, edi
     sub     ecx, edi  //Remaining moves
     pop     esi
     pop     edi
     pop     ebx
     jmp     dword ptr [ecx*4+@Case1JmpTable]
     nop
     nop
     nop
     nop
     nop
 @Else9:
     push    ebx                     // Pop is done before jmp to exit1
     push    edi                     // Pop is done before jmp to exit1
     push    esi                     // Pop is done before jmp to exit1
     //Align destination
     xor     edi, edi                // ByteNo1 := 0;
 @L111:
     mov     bl, [eax+edi]           // DstB[ByteNo1] := SrcB[ByteNo1];
     mov     [edx+edi], bl
     add     edi, 1                  // Inc(ByteNo1);
     mov     ebx, edx                // edx is destination pointer
     add     ebx, edi
     and     ebx, $0f
     test    ebx, ebx
     jnz     @L111                   // until((SrcAddress2 mod 16) = 0);
     add     eax, edi                // SrcB is aligned now - "SrcI := SrcI + ByteNo1;" Not valid Pascal
     add     edx, edi                // DstB is aligned now - "DstI := DstI + ByteNo1;" Not valid Pascal
     sub     ecx, edi                // Count := Count - ByteNo1;
     mov     esi, ecx
 @L222:
     //SourceAddress2 := Cardinal(@SrcB[0]);
     //if (SourceAddress2 mod 16 = 0) then
     mov     ebx, eax
     and     ebx, $0f
     jnz     @ElseIf1
     //Both source and destination are 16 byte aligned
     cmp     ecx, 240000            // if Count < 1M then
     jnl     @Else2
     shr     ecx,4
     shl     ecx,4
     xor     ebx, ebx
 @L1111:
     movq    mm0, [eax+ebx]
     movq    mm1, [eax+ebx+8]
     movq    [edx+ebx],   mm0
     movq    [edx+ebx+8], mm1
     add     ebx, 16
     cmp     ebx, ecx
     jb      @L1111
     jmp     @Fedtmule
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
 @Else2:
     shr     ecx, 7                  // NoOfIntLoops  := Count div 16;    16, 32, 48, 64, 80, 96, 112, 128
     shl     ecx, 7                  // NoOfIntMoves := NoOfIntLoops * 4;  4,  8, 12, 16, 20, 24,  28,  32
     xor     ebx, ebx
 @L10:
     movaps  xmm0, [eax+ebx]
     movaps  xmm1, [eax+ebx+16]
     movaps  xmm2, [eax+ebx+32]
     movaps  xmm3, [eax+ebx+48]
     movaps  xmm4, [eax+ebx+64]
     movaps  xmm5, [eax+ebx+80]
     movaps  xmm6, [eax+ebx+96]
     movaps  xmm7, [eax+ebx+112]
     movntps [edx+ebx],     xmm0
     movntps [edx+ebx+16],  xmm1
     movntps [edx+ebx+32],  xmm2
     movntps [edx+ebx+48],  xmm3
     movntps [edx+ebx+64],  xmm4
     movntps [edx+ebx+80],  xmm5
     movntps [edx+ebx+96],  xmm6
     movntps [edx+ebx+112], xmm7
     add     ebx, 128
     cmp     ebx, ecx
     jb      @L10
 @L20:
     jmp     @Fedtmule
     nop
     nop
     nop
     nop
     nop
     //else if (SourceAddress2 mod 8 = 0) then
 @ElseIf1:
     //Source is at least 8 byte aligned and destination is at least 16 byte aligned
     mov     ebx, eax
     and     ebx, $07
     test    ebx, ebx
     jnz     @Else1
     //Destination 16 byte aligned, Source unaligned
     shr     ecx, 4               // NoOfIntLoops  := Count div 16;
     shl     ecx, 4               // NoOfIntMoves := NoOfIntLoops * 4;
     xor     ebx, ebx
 @L100:
     movq    mm0, [eax+ebx]
     movq    mm1, [eax+ebx+8]
     movq    [edx+ebx],   mm0
     movq    [edx+ebx+8], mm1
     add     ebx, 16
     cmp     ebx, ecx
     jb      @L100
 @L200:
     jmp     @Fedtmule
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
     nop
 @Else1:
     //Destination 16 byte aligned. Source unaligned
     shr     ecx, 4
     shl     ecx, 4
     xor     ebx, ebx
 @L1000:
     movq    mm0, [eax+ebx]
     movq    mm1, [eax+ebx+8]
     movq    [edx+ebx],   mm0
     movq    [edx+ebx+8], mm1
     add     ebx, 16
     cmp     ebx, ecx
     jb      @L1000
     nop
     nop
     nop
     nop
     nop
     nop
     nop
 @Fedtmule:
     // Small moves after big 16 byte destination aligned moves
     add     eax, ecx
     add     edx, ecx
     sub     esi, ecx  //Remaining moves
     mov     ecx, esi
     cmp     ecx, 55
     ja      @L1239
     pop     esi
     pop     edi
     pop     ebx
     emms
     jmp     dword ptr [ecx*4+@Case1JmpTable]
     nop
     nop
     nop
     nop
     nop
 @L1239:
     add     ecx, eax
 @L123:
     mov     bl, [eax]          // DstB[ByteNo] := SrcB[ByteNo];
     mov     [edx], bl
     inc     eax
     inc     edx
     cmp     eax, ecx
     jb      @L123                  // until(ByteNo >= EndOfByteMoves);
     pop     esi
     pop     edi
     pop     ebx
     emms
     ret
 @RewMove:
     cmp     ecx,64
     jnbe    @Case1Else
     jmp     dword ptr [ecx*4+@Case2JmpTable]
 @RewerseCaseCount1:
     mov     cl, [eax]
     mov     [edx], cl
 @RewerseCaseCount0:
     ret
 @RewerseCaseCount2:
     mov     cl, [eax+1]
     mov     [edx+1], cl
     mov     cl, [eax]
     mov     [edx], cl
     ret
 @RewerseCaseCount3:
     mov     cl, [eax+2]
     mov     [edx+2], cl
     mov     cl, [eax+1]
     mov     [edx+1], cl
     mov     cl, [eax]
     mov     [edx], cl
     ret
 @RewerseCaseCount4:
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount5:
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount6:
     mov     cl, [eax+5]
     mov     [edx+5], cl
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount7:
     mov     cl, [eax+6]
     mov     [edx+6], cl
     mov     cl, [eax+5]
     mov     [edx+5], cl
     mov     cl, [eax+4]
     mov     [edx+4], cl
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount8:
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount9:
     mov     cl, [eax+8]
     mov     [edx+8], cl
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount10:
     mov     cl, [eax+9]
     mov     [edx+9], cl
     mov     cl, [eax+8]
     mov     [edx+8], cl
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount11:
     mov     cl, [eax+10]
     mov     [edx+10], cl
     mov     cl, [eax+9]
     mov     [edx+9], cl
     mov     cl, [eax+8]
     mov     [edx+8], cl
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount12 :
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
 @RewerseCaseCount13 :
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
@RewerseCaseCount14 :
     mov     cl, [eax+13]
     mov     [edx+13], cl
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
@RewerseCaseCount15 :
     mov     cl, [eax+14]
     mov     [edx+14], cl
     mov     cl, [eax+13]
     mov     [edx+13], cl
     mov     cl, [eax+12]
     mov     [edx+12], cl
     mov     ecx, [eax+8]
     mov     [edx+8], ecx
     mov     ecx, [eax+4]
     mov     [edx+4], ecx
     mov     ecx, [eax]
     mov     [edx], ecx
     ret
@RewerseCaseCount16 :
     movq    mm0, [eax+8]
     movq    mm1, [eax]
     movq    [edx+8], mm0
     movq    [edx],   mm1
     emms
     ret
@RewerseCaseCount17 :
     mov     cl, [eax+16]
     mov     [edx+16], cl
     movq    mm0, [eax+8]
     movq    mm1, [eax]
     movq    [edx+8], mm0
     movq    [edx],   mm1
     emms
     ret
@RewerseCaseCount18 :
     mov     cl, [eax+17]
     mov     [edx+17], cl
     mov     cl, [eax+16]
     mov     [edx+16], cl
     movq    mm0, [eax+8]
     movq    mm1, [eax]
     movq    [edx+8], mm0
     movq    [edx],   mm1
     emms
     ret
@RewerseCaseCount19 :
     mov     cl, [eax+18]
     mov     [edx+18], cl
     mov     cl, [eax+17]
     mov     [edx+17], cl
     mov     cl, [eax+16]
     mov     [edx+16], cl
     movq    mm0, [eax+8]
     movq    mm1, [eax]
     movq    [edx+8], mm0
     movq    [edx],   mm1
     emms
     ret
@RewerseCaseCount20 :
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     movq    mm0, [eax+8]
     movq    mm1, [eax]
     movq    [edx+8], mm0
     movq    [edx],   mm1
     emms
     ret
@RewerseCaseCount21 :
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     movq    mm0, [eax+8]
     movq    mm1, [eax]
     movq    [edx+8], mm0
     movq    [edx],   mm1
     emms
     ret
@RewerseCaseCount22 :
     mov     cl, [eax+21]
     mov     [edx+21], cl
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     movq    mm0, [eax+8]
     movq    mm1, [eax]
     movq    [edx+8], mm0
     movq    [edx],   mm1
     emms
     ret
@RewerseCaseCount23 :
     mov     cl, [eax+22]
     mov     [edx+22], cl
     mov     cl, [eax+21]
     mov     [edx+21], cl
     mov     cl, [eax+20]
     mov     [edx+20], cl
     mov     ecx, [eax+16]
     mov     [edx+16], ecx
     movq    mm0, [eax+8]
     movq    mm1, [eax]
     movq    [edx+8], mm0
     movq    [edx],   mm1
     emms
     ret
@RewerseCaseCount24 :
     movq    mm0, [eax+16]
     movq    mm1, [eax+8]
     movq    mm2, [eax]
     movq    [edx+16], mm0
     movq    [edx+8],  mm1
     movq    [edx],    mm2
     emms
     ret
@RewerseCaseCount25 :
     mov     cl, [eax+24]
     mov     [edx+24], cl
     movq    mm0, [eax+16]
     movq    mm1, [eax+8]
     movq    mm2, [eax]
     movq    [edx+16], mm0
     movq    [edx+8],  mm1
     movq    [edx],    mm2
     emms
     ret
@RewerseCaseCount26 :
     mov     cl, [eax+25]
     mov     [edx+25], cl
     mov     cl, [eax+24]
     mov     [edx+24], cl
     movq    mm0, [eax+16]
     movq    mm1, [eax+8]
     movq    mm2, [eax]
     movq    [edx+16], mm0
     movq    [edx+8],  mm1
     movq    [edx],    mm2
     emms
     ret
@RewerseCaseCount27 :
     mov     cl, [eax+26]
     mov     [edx+26], cl
     mov     cl, [eax+25]
     mov     [edx+25], cl
     mov     cl, [eax+24]
     mov     [edx+24], cl
     movq    mm0, [eax+16]
     movq    mm1, [eax+8]
     movq    mm2, [eax]
     movq    [edx+16], mm0
     movq    [edx+8],  mm1
     movq    [edx],    mm2
     emms
     ret
@RewerseCaseCount28 :
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     movq    mm0, [eax+16]
     movq    mm1, [eax+8]
     movq    mm2, [eax]
     movq    [edx+16], mm0
     movq    [edx+8],  mm1
     movq    [edx],    mm2
     emms
     ret
@RewerseCaseCount29 :
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     movq    mm0, [eax+16]
     movq    mm1, [eax+8]
     movq    mm2, [eax]
     movq    [edx+16], mm0
     movq    [edx+8],  mm1
     movq    [edx],    mm2
     emms
     ret
@RewerseCaseCount30 :
     mov     cl, [eax+29]
     mov     [edx+29], cl
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     movq    mm0, [eax+16]
     movq    mm1, [eax+8]
     movq    mm2, [eax]
     movq    [edx+16], mm0
     movq    [edx+8],  mm1
     movq    [edx],    mm2
     emms
     ret
@RewerseCaseCount31 :
     mov     cl, [eax+30]
     mov     [edx+30], cl
     mov     cl, [eax+29]
     mov     [edx+29], cl
     mov     cl, [eax+28]
     mov     [edx+28], cl
     mov     ecx, [eax+24]
     mov     [edx+24], ecx
     movq    mm0, [eax+16]
     movq    mm1, [eax+8]
     movq    mm2, [eax]
     movq    [edx+16], mm0
     movq    [edx+8],  mm1
     movq    [edx],    mm2
     emms
     ret
@RewerseCaseCount32 :
     movq    mm0, [eax+24]
     movq    mm1, [eax+16]
     movq    mm2, [eax+8]
     movq    mm3, [eax]
     movq    [edx+24], mm0
     movq    [edx+16], mm1
     movq    [edx+8],  mm2
     movq    [edx],    mm3
     emms
     ret
@RewerseCaseCount33 :
     mov     cl, [eax+32]
     mov     [edx+32], cl
     movq    mm0, [eax+24]
     movq    mm1, [eax+16]
     movq    mm2, [eax+8]
     movq    mm3, [eax]
     movq    [edx+24], mm0
     movq    [edx+16], mm1
     movq    [edx+8],  mm2
     movq    [edx],    mm3
     emms
     ret
@RewerseCaseCount34 :
     mov     cl, [eax+33]
     mov     [edx+33], cl
     mov     cl, [eax+32]
     mov     [edx+32], cl
     movq    mm0, [eax+24]
     movq    mm1, [eax+16]
     movq    mm2, [eax+8]
     movq    mm3, [eax]
     movq    [edx+24], mm0
     movq    [edx+16], mm1
     movq    [edx+8],  mm2
     movq    [edx],    mm3
     emms
     ret
@RewerseCaseCount35 :
     mov     cl, [eax+34]
     mov     [edx+34], cl
     mov     cl, [eax+33]
     mov     [edx+33], cl
     mov     cl, [eax+32]
     mov     [edx+32], cl
     movq    mm0, [eax+24]
     movq    mm1, [eax+16]
     movq    mm2, [eax+8]
     movq    mm3, [eax]
     movq    [edx+24], mm0
     movq    [edx+16], mm1
     movq    [edx+8],  mm2
     movq    [edx],    mm3
     emms
     ret
@RewerseCaseCount36 :
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     movq    mm0, [eax+24]
     movq    mm1, [eax+16]
     movq    mm2, [eax+8]
     movq    mm3, [eax]
     movq    [edx+24], mm0
     movq    [edx+16], mm1
     movq    [edx+8],  mm2
     movq    [edx],    mm3
     emms
     ret
@RewerseCaseCount37 :
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     movq    mm0, [eax+24]
     movq    mm1, [eax+16]
     movq    mm2, [eax+8]
     movq    mm3, [eax]
     movq    [edx+24], mm0
     movq    [edx+16], mm1
     movq    [edx+8], mm2
     movq    [edx], mm3
     emms
     ret
@RewerseCaseCount38 :
     mov     cl, [eax+37]
     mov     [edx+37], cl
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     movq    mm0, [eax+24]
     movq    mm1, [eax+16]
     movq    mm2, [eax+8]
     movq    mm3, [eax]
     movq    [edx+24], mm0
     movq    [edx+16], mm1
     movq    [edx+8],  mm2
     movq    [edx],    mm3
     emms
     ret
@RewerseCaseCount39 :
     mov     cl, [eax+38]
     mov     [edx+38], cl
     mov     cl, [eax+37]
     mov     [edx+37], cl
     mov     cl, [eax+36]
     mov     [edx+36], cl
     mov     ecx, [eax+32]
     mov     [edx+32], ecx
     movq    mm0, [eax+24]
     movq    mm1, [eax+16]
     movq    mm2, [eax+8]
     movq    mm3, [eax]
     movq    [edx+24], mm0
     movq    [edx+16], mm1
     movq    [edx+8],  mm2
     movq    [edx],    mm3
     emms
     ret
@RewerseCaseCount40 :
     movq    mm0, [eax+32]
     movq    mm1, [eax+24]
     movq    mm2, [eax+16]
     movq    mm3, [eax+8]
     movq    mm4, [eax]
     movq    [edx+32], mm0
     movq    [edx+24], mm1
     movq    [edx+16], mm2
     movq    [edx+8],  mm3
     movq    [edx],    mm4
     emms
     ret
@RewerseCaseCount41 :
     mov     cl, [eax+40]
     mov     [edx+40], cl
     movq    mm0, [eax+32]
     movq    mm1, [eax+24]
     movq    mm2, [eax+16]
     movq    mm3, [eax+8]
     movq    mm4, [eax]
     movq    [edx+32], mm0
     movq    [edx+24], mm1
     movq    [edx+16], mm2
     movq    [edx+8],  mm3
     movq    [edx],    mm4
     emms
     ret
@RewerseCaseCount42 :
     mov     cl, [eax+41]
     mov     [edx+41], cl
     mov     cl, [eax+40]
     mov     [edx+40], cl
     movq    mm0, [eax+32]
     movq    mm1, [eax+24]
     movq    mm2, [eax+16]
     movq    mm3, [eax+8]
     movq    mm4, [eax]
     movq    [edx+32], mm0
     movq    [edx+24], mm1
     movq    [edx+16], mm2
     movq    [edx+8],  mm3
     movq    [edx],    mm4
     emms
     ret
@RewerseCaseCount43 :
     mov     cl, [eax+42]
     mov     [edx+42], cl
     mov     cl, [eax+41]
     mov     [edx+41], cl
     mov     cl, [eax+40]
     mov     [edx+40], cl
     movq    mm0, [eax+32]
     movq    mm1, [eax+24]
     movq    mm2, [eax+16]
     movq    mm3, [eax+8]
     movq    mm4, [eax]
     movq    [edx+32], mm0
     movq    [edx+24], mm1
     movq    [edx+16], mm2
     movq    [edx+8],  mm3
     movq    [edx],    mm4
     emms
     ret
@RewerseCaseCount44 :
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     movq    mm0, [eax+32]
     movq    mm1, [eax+24]
     movq    mm2, [eax+16]
     movq    mm3, [eax+8]
     movq    mm4, [eax]
     movq    [edx+32], mm0
     movq    [edx+24], mm1
     movq    [edx+16], mm2
     movq    [edx+8],  mm3
     movq    [edx],    mm4
     emms
     ret
@RewerseCaseCount45 :
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     movq    mm0, [eax+32]
     movq    mm1, [eax+24]
     movq    mm2, [eax+16]
     movq    mm3, [eax+8]
     movq    mm4, [eax]
     movq    [edx+32], mm0
     movq    [edx+24], mm1
     movq    [edx+16], mm2
     movq    [edx+8],  mm3
     movq    [edx],    mm4
     emms
     ret
@RewerseCaseCount46 :
     mov     cl, [eax+45]
     mov     [edx+45], cl
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     movq    mm0, [eax+32]
     movq    mm1, [eax+24]
     movq    mm2, [eax+16]
     movq    mm3, [eax+8]
     movq    mm4, [eax]
     movq    [edx+32], mm0
     movq    [edx+24], mm1
     movq    [edx+16], mm2
     movq    [edx+8],  mm3
     movq    [edx],    mm4
     emms
     ret
@RewerseCaseCount47 :
     mov     cl, [eax+46]
     mov     [edx+46], cl
     mov     cl, [eax+45]
     mov     [edx+45], cl
     mov     cl, [eax+44]
     mov     [edx+44], cl
     mov     ecx, [eax+40]
     mov     [edx+40], ecx
     movq    mm0, [eax+32]
     movq    mm1, [eax+24]
     movq    mm2, [eax+16]
     movq    mm3, [eax+8]
     movq    mm4, [eax]
     movq    [edx+32], mm0
     movq    [edx+24], mm1
     movq    [edx+16], mm2
     movq    [edx+8],  mm3
     movq    [edx],    mm4
     emms
     ret
@RewerseCaseCount48 :
     movq    mm0, [eax+40]
     movq    mm1, [eax+32]
     movq    mm2, [eax+24]
     movq    mm3, [eax+16]
     movq    mm4, [eax+8]
     movq    mm5, [eax]
     movq    [edx+40], mm0
     movq    [edx+32], mm1
     movq    [edx+24], mm2
     movq    [edx+16], mm3
     movq    [edx+8],  mm4
     movq    [edx],    mm5
     emms
     ret
@RewerseCaseCount49 :
     mov     cl, [eax+48]
     mov     [edx+48], cl
     movq    mm0, [eax+40]
     movq    mm1, [eax+32]
     movq    mm2, [eax+24]
     movq    mm3, [eax+16]
     movq    mm4, [eax+8]
     movq    mm5, [eax]
     movq    [edx+40], mm0
     movq    [edx+32], mm1
     movq    [edx+24], mm2
     movq    [edx+16], mm3
     movq    [edx+8],  mm4
     movq    [edx],    mm5
     emms
     ret
@RewerseCaseCount50 :
     mov     cl, [eax+49]
     mov     [edx+49], cl
     mov     cl, [eax+48]
     mov     [edx+48], cl
     movq    mm0, [eax+40]
     movq    mm1, [eax+32]
     movq    mm2, [eax+24]
     movq    mm3, [eax+16]
     movq    mm4, [eax+8]
     movq    mm5, [eax]
     movq    [edx+40], mm0
     movq    [edx+32], mm1
     movq    [edx+24], mm2
     movq    [edx+16], mm3
     movq    [edx+8],  mm4
     movq    [edx],    mm5
     emms
     ret
@RewerseCaseCount51 :
     mov     cl, [eax+50]
     mov     [edx+50], cl
     mov     cl, [eax+49]
     mov     [edx+49], cl
     mov     cl, [eax+48]
     mov     [edx+48], cl
     movq    mm0, [eax+40]
     movq    mm1, [eax+32]
     movq    mm2, [eax+24]
     movq    mm3, [eax+16]
     movq    mm4, [eax+8]
     movq    mm5, [eax]
     movq    [edx+40], mm0
     movq    [edx+32], mm1
     movq    [edx+24], mm2
     movq    [edx+16], mm3
     movq    [edx+8],  mm4
     movq    [edx],    mm5
     emms
     ret
@RewerseCaseCount52 :
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     movq    mm0, [eax+40]
     movq    mm1, [eax+32]
     movq    mm2, [eax+24]
     movq    mm3, [eax+16]
     movq    mm4, [eax+8]
     movq    mm5, [eax]
     movq    [edx+40], mm0
     movq    [edx+32], mm1
     movq    [edx+24], mm2
     movq    [edx+16], mm3
     movq    [edx+8],  mm4
     movq    [edx],    mm5
     emms
     ret
@RewerseCaseCount53 :
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     movq    mm0, [eax+40]
     movq    mm1, [eax+32]
     movq    mm2, [eax+24]
     movq    mm3, [eax+16]
     movq    mm4, [eax+8]
     movq    mm5, [eax]
     movq    [edx+40], mm0
     movq    [edx+32], mm1
     movq    [edx+24], mm2
     movq    [edx+16], mm3
     movq    [edx+8],  mm4
     movq    [edx],    mm5
     emms
     ret
@RewerseCaseCount54 :
     mov     cl, [eax+53]
     mov     [edx+53], cl
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     movq    mm0, [eax+40]
     movq    mm1, [eax+32]
     movq    mm2, [eax+24]
     movq    mm3, [eax+16]
     movq    mm4, [eax+8]
     movq    mm5, [eax]
     movq    [edx+40], mm0
     movq    [edx+32], mm1
     movq    [edx+24], mm2
     movq    [edx+16], mm3
     movq    [edx+8],  mm4
     movq    [edx],    mm5
     emms
     ret
@RewerseCaseCount55 :
     mov     cl, [eax+54]
     mov     [edx+54], cl
     mov     cl, [eax+53]
     mov     [edx+53], cl
     mov     cl, [eax+52]
     mov     [edx+52], cl
     mov     ecx, [eax+48]
     mov     [edx+48], ecx
     movq    mm0, [eax+40]
     movq    mm1, [eax+32]
     movq    mm2, [eax+24]
     movq    mm3, [eax+16]
     movq    mm4, [eax+8]
     movq    mm5, [eax]
     movq    [edx+40], mm0
     movq    [edx+32], mm1
     movq    [edx+24], mm2
     movq    [edx+16], mm3
     movq    [edx+8],  mm4
     movq    [edx],    mm5
     emms
     ret
@RewerseCaseCount56 :
     movq    mm0, [eax+48]
     movq    mm1, [eax+40]
     movq    mm2, [eax+32]
     movq    mm3, [eax+24]
     movq    mm4, [eax+16]
     movq    mm5, [eax+8]
     movq    mm6, [eax]
     movq    [edx+48], mm0
     movq    [edx+40], mm1
     movq    [edx+32], mm2
     movq    [edx+24], mm3
     movq    [edx+16], mm4
     movq    [edx+8],  mm5
     movq    [edx],    mm6
     emms
     ret
@RewerseCaseCount57 :
     mov     cl, [eax+56]
     mov     [edx+56], cl
     movq    mm0, [eax+48]
     movq    mm1, [eax+40]
     movq    mm2, [eax+32]
     movq    mm3, [eax+24]
     movq    mm4, [eax+16]
     movq    mm5, [eax+8]
     movq    mm6, [eax]
     movq    [edx+48], mm0
     movq    [edx+40], mm1
     movq    [edx+32], mm2
     movq    [edx+24], mm3
     movq    [edx+16], mm4
     movq    [edx+8],  mm5
     movq    [edx],    mm6
     emms
     ret
@RewerseCaseCount58 :
     mov     cl, [eax+57]
     mov     [edx+57], cl
     mov     cl, [eax+56]
     mov     [edx+56], cl
     movq    mm0, [eax+48]
     movq    mm1, [eax+40]
     movq    mm2, [eax+32]
     movq    mm3, [eax+24]
     movq    mm4, [eax+16]
     movq    mm5, [eax+8]
     movq    mm6, [eax]
     movq    [edx+48], mm0
     movq    [edx+40], mm1
     movq    [edx+32], mm2
     movq    [edx+24], mm3
     movq    [edx+16], mm4
     movq    [edx+8],  mm5
     movq    [edx],    mm6
     emms
     ret
@RewerseCaseCount59 :
     mov     cl, [eax+58]
     mov     [edx+58], cl
     mov     cl, [eax+57]
     mov     [edx+57], cl
     mov     cl, [eax+56]
     mov     [edx+56], cl
     movq    mm0, [eax+48]
     movq    mm1, [eax+40]
     movq    mm2, [eax+32]
     movq    mm3, [eax+24]
     movq    mm4, [eax+16]
     movq    mm5, [eax+8]
     movq    mm6, [eax]
     movq    [edx+48], mm0
     movq    [edx+40], mm1
     movq    [edx+32], mm2
     movq    [edx+24], mm3
     movq    [edx+16], mm4
     movq    [edx+8],  mm5
     movq    [edx],    mm6
     emms
     ret
@RewerseCaseCount60 :
     mov     ecx, [eax+56]
     mov     [edx+56], ecx
     movq    mm0, [eax+48]
     movq    mm1, [eax+40]
     movq    mm2, [eax+32]
     movq    mm3, [eax+24]
     movq    mm4, [eax+16]
     movq    mm5, [eax+8]
     movq    mm6, [eax]
     movq    [edx+48], mm0
     movq    [edx+40], mm1
     movq    [edx+32], mm2
     movq    [edx+24], mm3
     movq    [edx+16], mm4
     movq    [edx+8],  mm5
     movq    [edx],    mm6
     emms
     ret
@RewerseCaseCount61 :
     mov     cl, [eax+60]
     mov     [edx+60], cl
     mov     ecx, [eax+56]
     mov     [edx+56], ecx
     movq    mm0, [eax+48]
     movq    mm1, [eax+40]
     movq    mm2, [eax+32]
     movq    mm3, [eax+24]
     movq    mm4, [eax+16]
     movq    mm5, [eax+8]
     movq    mm6, [eax]
     movq    [edx+48], mm0
     movq    [edx+40], mm1
     movq    [edx+32], mm2
     movq    [edx+24], mm3
     movq    [edx+16], mm4
     movq    [edx+8],  mm5
     movq    [edx],    mm6
     emms
     ret
@RewerseCaseCount62 :
     mov     cl, [eax+61]
     mov     [edx+61], cl
     mov     cl, [eax+60]
     mov     [edx+60], cl
     mov     ecx, [eax+56]
     mov     [edx+56], ecx
     movq    mm0, [eax+48]
     movq    mm1, [eax+40]
     movq    mm2, [eax+32]
     movq    mm3, [eax+24]
     movq    mm4, [eax+16]
     movq    mm5, [eax+8]
     movq    mm6, [eax]
     movq    [edx+48], mm0
     movq    [edx+40], mm1
     movq    [edx+32], mm2
     movq    [edx+24], mm3
     movq    [edx+16], mm4
     movq    [edx+8],  mm5
     movq    [edx],    mm6
     emms
     ret
@RewerseCaseCount63 :
     mov     cl, [eax+62]
     mov     [edx+62], cl
     mov     cl, [eax+61]
     mov     [edx+61], cl
     mov     cl, [eax+60]
     mov     [edx+60], cl
     mov     ecx, [eax+56]
     mov     [edx+56], ecx
     movq    mm0, [eax+48]
     movq    mm1, [eax+40]
     movq    mm2, [eax+32]
     movq    mm3, [eax+24]
     movq    mm4, [eax+16]
     movq    mm5, [eax+8]
     movq    mm6, [eax]
     movq    [edx+48], mm0
     movq    [edx+40], mm1
     movq    [edx+32], mm2
     movq    [edx+24], mm3
     movq    [edx+16], mm4
     movq    [edx+8],  mm5
     movq    [edx],    mm6
     emms
     ret
@RewerseCaseCount64 :
     movq    mm0, [eax+56]
     movq    mm1, [eax+48]
     movq    mm2, [eax+40]
     movq    mm3, [eax+32]
     movq    mm4, [eax+24]
     movq    mm5, [eax+16]
     movq    mm6, [eax+8]
     movq    mm7, [eax]
     movq    [edx+56], mm0
     movq    [edx+48], mm1
     movq    [edx+40], mm2
     movq    [edx+32], mm3
     movq    [edx+24], mm4
     movq    [edx+16], mm5
     movq    [edx+8],  mm6
     movq    [edx],    mm7
     emms
     ret
     nop
     nop
     nop
 @Case1Else:
     push    ebx
     push    edi
 @L15:
     sub     ecx, 1
     mov     bl, [eax+ecx]
     mov     [edx+ecx], bl
     mov     edi, edx               // ecx is destination pointer
     add     edi, ecx
     and     edi, $07
     test    edi, edi
     jnz     @L15                    // until((SrcAddress2 mod 16) = 0);
     sub     ecx, 16
     mov     ebx, ecx
 @L67:
     movq    mm0, [eax+ecx+8]
     movq    [edx+ecx+8], mm0
     movq    mm1, [eax+ecx]
     movq    [edx+ecx], mm1
     sub     ecx, 16
     jns     @L67
     emms
     add     ecx,16
     pop     edi
     pop     ebx
     jmp     dword ptr [ecx*4+@Case2JmpTable]
 @Exit:
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

@Case1JmpTable:
 dd @ForwardCaseCount0
 dd @ForwardCaseCount1
 dd @ForwardCaseCount2
 dd @ForwardCaseCount3
 dd @ForwardCaseCount4
 dd @ForwardCaseCount5
 dd @ForwardCaseCount6
 dd @ForwardCaseCount7
 dd @ForwardCaseCount8
 dd @ForwardCaseCount9
 dd @ForwardCaseCount10
 dd @ForwardCaseCount11
 dd @ForwardCaseCount12
 dd @ForwardCaseCount13
 dd @ForwardCaseCount14
 dd @ForwardCaseCount15
 dd @ForwardCaseCount16
 dd @ForwardCaseCount17
 dd @ForwardCaseCount18
 dd @ForwardCaseCount19
 dd @ForwardCaseCount20
 dd @ForwardCaseCount21
 dd @ForwardCaseCount22
 dd @ForwardCaseCount23
 dd @ForwardCaseCount24
 dd @ForwardCaseCount25
 dd @ForwardCaseCount26
 dd @ForwardCaseCount27
 dd @ForwardCaseCount28
 dd @ForwardCaseCount29
 dd @ForwardCaseCount30
 dd @ForwardCaseCount31
 dd @ForwardCaseCount32
 dd @ForwardCaseCount33
 dd @ForwardCaseCount34
 dd @ForwardCaseCount35
 dd @ForwardCaseCount36
 dd @ForwardCaseCount37
 dd @ForwardCaseCount38
 dd @ForwardCaseCount39
 dd @ForwardCaseCount40
 dd @ForwardCaseCount41
 dd @ForwardCaseCount42
 dd @ForwardCaseCount43
 dd @ForwardCaseCount44
 dd @ForwardCaseCount45
 dd @ForwardCaseCount46
 dd @ForwardCaseCount47
 dd @ForwardCaseCount48
 dd @ForwardCaseCount49
 dd @ForwardCaseCount50
 dd @ForwardCaseCount51
 dd @ForwardCaseCount52
 dd @ForwardCaseCount53
 dd @ForwardCaseCount54
 dd @ForwardCaseCount55

@Case2JmpTable:
 dd @RewerseCaseCount0
 dd @RewerseCaseCount1
 dd @RewerseCaseCount2
 dd @RewerseCaseCount3
 dd @RewerseCaseCount4
 dd @RewerseCaseCount5
 dd @RewerseCaseCount6
 dd @RewerseCaseCount7
 dd @RewerseCaseCount8
 dd @RewerseCaseCount9
 dd @RewerseCaseCount10
 dd @RewerseCaseCount11
 dd @RewerseCaseCount12
 dd @RewerseCaseCount13
 dd @RewerseCaseCount14
 dd @RewerseCaseCount15
 dd @RewerseCaseCount16
 dd @RewerseCaseCount17
 dd @RewerseCaseCount18
 dd @RewerseCaseCount19
 dd @RewerseCaseCount20
 dd @RewerseCaseCount21
 dd @RewerseCaseCount22
 dd @RewerseCaseCount23
 dd @RewerseCaseCount24
 dd @RewerseCaseCount25
 dd @RewerseCaseCount26
 dd @RewerseCaseCount27
 dd @RewerseCaseCount28
 dd @RewerseCaseCount29
 dd @RewerseCaseCount30
 dd @RewerseCaseCount31
 dd @RewerseCaseCount32
 dd @RewerseCaseCount33
 dd @RewerseCaseCount34
 dd @RewerseCaseCount35
 dd @RewerseCaseCount36
 dd @RewerseCaseCount37
 dd @RewerseCaseCount38
 dd @RewerseCaseCount39
 dd @RewerseCaseCount40
 dd @RewerseCaseCount41
 dd @RewerseCaseCount42
 dd @RewerseCaseCount43
 dd @RewerseCaseCount44
 dd @RewerseCaseCount45
 dd @RewerseCaseCount46
 dd @RewerseCaseCount47
 dd @RewerseCaseCount48
 dd @RewerseCaseCount49
 dd @RewerseCaseCount50
 dd @RewerseCaseCount51
 dd @RewerseCaseCount52
 dd @RewerseCaseCount53
 dd @RewerseCaseCount54
 dd @RewerseCaseCount55
 dd @RewerseCaseCount56
 dd @RewerseCaseCount57
 dd @RewerseCaseCount58
 dd @RewerseCaseCount59
 dd @RewerseCaseCount60
 dd @RewerseCaseCount61
 dd @RewerseCaseCount62
 dd @RewerseCaseCount63
 dd @RewerseCaseCount64

end;

//Author:            John O'Harrow
//Date:              23/6-03
//Optimized for:     Blended
//Instructionset(s): IA32, MMX
//Original Name:     MoveJOH_MMX

procedure MoveFastcodeBlended(const Source; var Dest; Count : Integer);
const
  TABLESIZE =  36;
  LARGESIZE = 512;
asm
  cmp   eax,edx
  jng   @Check
@ForwardMove:
  cmp   ecx,TABLESIZE
  jg    @FwdNotSmall
  add   eax,ecx
@ForwardMove2:
  cmp   ecx,0
  jle   @Done {For Compatibility with Delphi's move for Count <= 0}
  add   edx,ecx
  jmp   dword ptr [@FwdJumpTable+ecx*4]
@Check:
  je    @Done {For Compatibility with Delphi's move for Source=Dest}
@CheckOverlap:
  add   eax,ecx
  cmp   eax,edx
  jg    @BackwardCheck {Source/Dest Overlap}
@NoOverlap:
  cmp   ecx,TABLESIZE
  jle   @ForwardMove2 {Source already incremented by Count}
  sub   eax,ecx {Restore Original Source}
@FwdNotSmall:
  cmp   ecx,LARGESIZE
  jge   @FwdLargeMove
  {Count > TABLESIZE and Count < LARGESIZE}
  cmp   ecx,72 {Size at which using MMX becomes worthwhile}
  jl    @FwdMoveNonMMX
  @FwdMoveMMX:
  push  ebx
  mov   ebx,edx
  movq  mm0,[eax] {First 8 Characters}
  {QWORD Align Writes}
  add   eax,ecx
  add   ecx,edx
  add   edx,7
  and   edx,-8
  sub   ecx,edx
  add   edx,ecx
  {Now QWORD Aligned}
  sub   ecx,32
  neg   ecx
@FwdLoopMMX:
  movq  mm1,[eax+ecx-32]
  movq  mm2,[eax+ecx-24]
  movq  mm3,[eax+ecx-16]
  movq  mm4,[eax+ecx- 8]
  movq  [edx+ecx-32],mm1
  movq  [edx+ecx-24],mm2
  movq  [edx+ecx-16],mm3
  movq  [edx+ecx- 8],mm4
  add   ecx,32
  jle   @FwdLoopMMX
  movq  [ebx],mm0 {First 8 Characters}
  emms
  pop   ebx
  neg   ecx
  add   ecx,32
  jmp   dword ptr [@FwdJumpTable+ecx*4]
@FwdMoveNonMMX:
  push  edi
  push  ebx
  push  edx
  mov   edi,[eax]
  {DWORD Align Reads}
  add   edx,ecx
  add   ecx,eax
  add   eax,3
  and   eax,-4
  sub   ecx,eax
  add   eax,ecx
  {Now DWORD Aligned}
  sub   ecx,32
  neg   ecx
@FwdLoop:
  mov   ebx,[eax+ecx-32]
  mov   [edx+ecx-32],ebx
  mov   ebx,[eax+ecx-28]
  mov   [edx+ecx-28],ebx
  mov   ebx,[eax+ecx-24]
  mov   [edx+ecx-24],ebx
  mov   ebx,[eax+ecx-20]
  mov   [edx+ecx-20],ebx
  mov   ebx,[eax+ecx-16]
  mov   [edx+ecx-16],ebx
  mov   ebx,[eax+ecx-12]
  mov   [edx+ecx-12],ebx
  mov   ebx,[eax+ecx-8]
  mov   [edx+ecx-8],ebx
  mov   ebx,[eax+ecx-4]
  mov   [edx+ecx-4],ebx
  add   ecx,32
  jle   @FwdLoop
  pop   ebx {Orig EDX}
  mov   [ebx],edi
  neg   ecx
  add   ecx,32
  pop   ebx
  pop   edi
  jmp   dword ptr [@FwdJumpTable+ecx*4]
@FwdLargeMove:
  push  ebx
  mov   ebx,ecx
  test  edx,15
  jz    @FwdAligned
  {16 byte Align Destination}
  mov   ecx,edx
  add   ecx,15
  and   ecx,-16
  sub   ecx,edx
  add   eax,ecx
  add   edx,ecx
  sub   ebx,ecx
  {Destination now 16 Byte Aligned}
  call  dword ptr [@FwdJumpTable+ecx*4]
@FwdAligned:
  mov   ecx,ebx
  and   ecx,-16
  sub   ebx,ecx {EBX = Remainder}
  push  esi
  push  edi
  mov   esi,eax          {ESI = Source}
  mov   edi,edx          {EDI = Dest}
  mov   eax,ecx          {EAX = Count}
  and   eax,$FFFFFFC0    {EAX = No of Bytes to Blocks Moves}
  and   ecx,$3F          {ECX = Remaining Bytes to Move (0..63)}
  add   esi,eax
  add   edi,eax
  shr   eax,3            {EAX = No of QWORD's to Block Move}
  neg   eax
@MMXcopyloop:
  movq  mm0,[esi+eax*8   ]
  movq  mm1,[esi+eax*8+ 8]
  movq  mm2,[esi+eax*8+16]
  movq  mm3,[esi+eax*8+24]
  movq  mm4,[esi+eax*8+32]
  movq  mm5,[esi+eax*8+40]
  movq  mm6,[esi+eax*8+48]
  movq  mm7,[esi+eax*8+56]
  movq  [edi+eax*8   ],mm0
  movq  [edi+eax*8+ 8],mm1
  movq  [edi+eax*8+16],mm2
  movq  [edi+eax*8+24],mm3
  movq  [edi+eax*8+32],mm4
  movq  [edi+eax*8+40],mm5
  movq  [edi+eax*8+48],mm6
  movq  [edi+eax*8+56],mm7
  add   eax,8
  jnz   @MMXcopyloop
  emms                   {Empty MMX State}
  add   ecx,ebx
  shr   ecx,2
  rep   movsd
  mov   ecx,ebx
  and   ecx,3
  rep   movsb
  pop   edi
  pop   esi
  pop   ebx
  ret
@BackwardCheck: {Overlapping Source/Dest}
  sub   eax,ecx {Restore Original Source}
  cmp   ecx,TABLESIZE
  jle   @BwdRemainder
@BwdNotSmall:
  push  ebx
@BackwardMove:
  cmp   ecx,72 {Size at which using MMX becomes worthwhile}
  jl    @BwdMove
@BwdMoveMMX:
  movq  mm0,[eax+ecx-8] {Get Last QWORD}
  {QWORD Align Writes}
  lea   ebx,[edx+ecx]
  and   ebx,7
  sub   ecx,ebx
  add   ebx,ecx
  {Now QWORD Aligned}
  sub   ecx,32
@BwdLoopMMX:
  movq  mm1,[eax+ecx   ]
  movq  mm2,[eax+ecx+ 8]
  movq  mm3,[eax+ecx+16]
  movq  mm4,[eax+ecx+24]
  movq  [edx+ecx+24],mm4
  movq  [edx+ecx+16],mm3
  movq  [edx+ecx+ 8],mm2
  movq  [edx+ecx   ],mm1
  sub   ecx,32
  jge   @BwdLoopMMX
  movq  [edx+ebx-8], mm0 {Last QWORD}
  emms
  add   ecx,32
  pop   ebx
@BwdRemainder:
  jmp   dword ptr [@BwdJumpTable+ecx*4]
@BwdMove:
  push  edi
  push  ecx
  mov   edi,[eax+ecx-4] {Get Last DWORD}
  {DWORD Align Writes}
  lea   ebx,[edx+ecx]
  and   ebx,3
  sub   ecx,ebx
  {Now DWORD Aligned}
  sub   ecx,32
@BwdLoop:
  mov   ebx,[eax+ecx+28]
  mov   [edx+ecx+28],ebx
  mov   ebx,[eax+ecx+24]
  mov   [edx+ecx+24],ebx
  mov   ebx,[eax+ecx+20]
  mov   [edx+ecx+20],ebx
  mov   ebx,[eax+ecx+16]
  mov   [edx+ecx+16],ebx
  mov   ebx,[eax+ecx+12]
  mov   [edx+ecx+12],ebx
  mov   ebx,[eax+ecx+8]
  mov   [edx+ecx+8],ebx
  mov   ebx,[eax+ecx+4]
  mov   [edx+ecx+4],ebx
  mov   ebx,[eax+ecx]
  mov   [edx+ecx],ebx
  sub   ecx,32
  jge   @BwdLoop
  pop   ebx
  add   ecx,32
  mov   [edx+ebx-4],edi {Last DWORD}
  pop   edi
  pop   ebx
  jmp   dword ptr [@BwdJumpTable+ecx*4]
  nop; nop; nop
@FwdJumpTable:
  dd    @Done {Removes need to test for zero size move}
  dd    @Fwd01,@Fwd02,@Fwd03,@Fwd04,@Fwd05,@Fwd06,@Fwd07,@Fwd08
  dd    @Fwd09,@Fwd10,@Fwd11,@Fwd12,@Fwd13,@Fwd14,@Fwd15,@Fwd16
  dd    @Fwd17,@Fwd18,@Fwd19,@Fwd20,@Fwd21,@Fwd22,@Fwd23,@Fwd24
  dd    @Fwd25,@Fwd26,@Fwd27,@Fwd28,@Fwd29,@Fwd30,@Fwd31,@Fwd32
  dd    @Fwd33,@Fwd34,@Fwd35,@Fwd36
@BwdJumpTable:
  dd    @Done {Removes need to test for zero size move}
  dd    @Bwd01,@Bwd02,@Bwd03,@Bwd04,@Bwd05,@Bwd06,@Bwd07,@Bwd08
  dd    @Bwd09,@Bwd10,@Bwd11,@Bwd12,@Bwd13,@Bwd14,@Bwd15,@Bwd16
  dd    @Bwd17,@Bwd18,@Bwd19,@Bwd20,@Bwd21,@Bwd22,@Bwd23,@Bwd24
  dd    @Bwd25,@Bwd26,@Bwd27,@Bwd28,@Bwd29,@Bwd30,@Bwd31,@Bwd32
  dd    @Bwd33,@Bwd34,@Bwd35,@Bwd36
@Fwd36:
  mov   ecx,[eax-36]
  mov   [edx-36],ecx
@Fwd32:
  mov   ecx,[eax-32]
  mov   [edx-32],ecx
@Fwd28:
  mov   ecx,[eax-28]
  mov   [edx-28],ecx
@Fwd24:
  mov   ecx,[eax-24]
  mov   [edx-24],ecx
@Fwd20:
  mov   ecx,[eax-20]
  mov   [edx-20],ecx
@Fwd16:
  mov   ecx,[eax-16]
  mov   [edx-16],ecx
@Fwd12:
  mov   ecx,[eax-12]
  mov   [edx-12],ecx
@Fwd08:
  mov   ecx,[eax-8]
  mov   [edx-8],ecx
@Fwd04:
  mov   ecx,[eax-4]
  mov   [edx-4],ecx
  ret
@Fwd35:
  mov   ecx,[eax-35]
  mov   [edx-35],ecx
@Fwd31:
  mov   ecx,[eax-31]
  mov   [edx-31],ecx
@Fwd27:
  mov   ecx,[eax-27]
  mov   [edx-27],ecx
@Fwd23:
  mov   ecx,[eax-23]
  mov   [edx-23],ecx
@Fwd19:
  mov   ecx,[eax-19]
  mov   [edx-19],ecx
@Fwd15:
  mov   ecx,[eax-15]
  mov   [edx-15],ecx
@Fwd11:
  mov   ecx,[eax-11]
  mov   [edx-11],ecx
@Fwd07:
  mov   ecx,[eax-7]
  mov   [edx-7],ecx
@Fwd03:
  mov   cx,[eax-3]
  mov   [edx-3],cx
  mov   cl,[eax-1]
  mov   [edx-1],cl
  ret
@Fwd34:
  mov   ecx,[eax-34]
  mov   [edx-34],ecx
@Fwd30:
  mov   ecx,[eax-30]
  mov   [edx-30],ecx
@Fwd26:
  mov   ecx,[eax-26]
  mov   [edx-26],ecx
@Fwd22:
  mov   ecx,[eax-22]
  mov   [edx-22],ecx
@Fwd18:
  mov   ecx,[eax-18]
  mov   [edx-18],ecx
@Fwd14:
  mov   ecx,[eax-14]
  mov   [edx-14],ecx
@Fwd10:
  mov   ecx,[eax-10]
  mov   [edx-10],ecx
@Fwd06:
  mov   ecx,[eax-6]
  mov   [edx-6],ecx
@Fwd02:
  mov   cx,[eax-2]
  mov   [edx-2],cx
  ret
@Fwd33:
  mov   ecx,[eax-33]
  mov   [edx-33],ecx
@Fwd29:
  mov   ecx,[eax-29]
  mov   [edx-29],ecx
@Fwd25:
  mov   ecx,[eax-25]
  mov   [edx-25],ecx
@Fwd21:
  mov   ecx,[eax-21]
  mov   [edx-21],ecx
@Fwd17:
  mov   ecx,[eax-17]
  mov   [edx-17],ecx
@Fwd13:
  mov   ecx,[eax-13]
  mov   [edx-13],ecx
@Fwd09:
  mov   ecx,[eax-9]
  mov   [edx-9],ecx
@Fwd05:
  mov   ecx,[eax-5]
  mov   [edx-5],ecx
@Fwd01:
  mov   cl,[eax-1]
  mov   [edx-1],cl
  ret
@Bwd36:
  mov   ecx,[eax+32]
  mov   [edx+32],ecx
@Bwd32:
  mov   ecx,[eax+28]
  mov   [edx+28],ecx
@Bwd28:
  mov   ecx,[eax+24]
  mov   [edx+24],ecx
@Bwd24:
  mov   ecx,[eax+20]
  mov   [edx+20],ecx
@Bwd20:
  mov   ecx,[eax+16]
  mov   [edx+16],ecx
@Bwd16:
  mov   ecx,[eax+12]
  mov   [edx+12],ecx
@Bwd12:
  mov   ecx,[eax+8]
  mov   [edx+8],ecx
@Bwd08:
  mov   ecx,[eax+4]
  mov   [edx+4],ecx
@Bwd04:
  mov   ecx,[eax]
  mov   [edx],ecx
  ret
@Bwd35:
  mov   ecx,[eax+31]
  mov   [edx+31],ecx
@Bwd31:
  mov   ecx,[eax+27]
  mov   [edx+27],ecx
@Bwd27:
  mov   ecx,[eax+23]
  mov   [edx+23],ecx
@Bwd23:
  mov   ecx,[eax+19]
  mov   [edx+19],ecx
@Bwd19:
  mov   ecx,[eax+15]
  mov   [edx+15],ecx
@Bwd15:
  mov   ecx,[eax+11]
  mov   [edx+11],ecx
@Bwd11:
  mov   ecx,[eax+7]
  mov   [edx+7],ecx
@Bwd07:
  mov   ecx,[eax+3]
  mov   [edx+3],ecx
@Bwd03:
  mov   cx,[eax+1]
  mov   [edx+1],cx
  mov   cl,[eax]
  mov   [edx],cl
  ret
@Bwd34:
  mov   ecx,[eax+30]
  mov   [edx+30],ecx
@Bwd30:
  mov   ecx,[eax+26]
  mov   [edx+26],ecx
@Bwd26:
  mov   ecx,[eax+22]
  mov   [edx+22],ecx
@Bwd22:
  mov   ecx,[eax+18]
  mov   [edx+18],ecx
@Bwd18:
  mov   ecx,[eax+14]
  mov   [edx+14],ecx
@Bwd14:
  mov   ecx,[eax+10]
  mov   [edx+10],ecx
@Bwd10:
  mov   ecx,[eax+6]
  mov   [edx+6],ecx
@Bwd06:
  mov   ecx,[eax+2]
  mov   [edx+2],ecx
@Bwd02:
  mov   cx,[eax]
  mov   [edx],cx
  ret
@Bwd33:
  mov   ecx,[eax+29]
  mov   [edx+29],ecx
@Bwd29:
  mov   ecx,[eax+25]
  mov   [edx+25],ecx
@Bwd25:
  mov   ecx,[eax+21]
  mov   [edx+21],ecx
@Bwd21:
  mov   ecx,[eax+17]
  mov   [edx+17],ecx
@Bwd17:
  mov   ecx,[eax+13]
  mov   [edx+13],ecx
@Bwd13:
  mov   ecx,[eax+9]
  mov   [edx+9],ecx
@Bwd09:
  mov   ecx,[eax+5]
  mov   [edx+5],ecx
@Bwd05:
  mov   ecx,[eax+1]
  mov   [edx+1],ecx
@Bwd01:
  mov   cl,[eax]
  mov   [edx],cl
@Done:
end;

//Author:            John O'Harrow
//Optimized for:     RTL Replacement
//Instructionset(s): IA32
//Original name:     MoveJOH_IA32_2

procedure SmallForwardMove;
asm
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd01,@@Fwd02,@@Fwd03,@@Fwd04,@@Fwd05,@@Fwd06,@@Fwd07,@@Fwd08
  dd      @@Fwd09,@@Fwd10,@@Fwd11,@@Fwd12,@@Fwd13,@@Fwd14,@@Fwd15,@@Fwd16
  dd      @@Fwd17,@@Fwd18,@@Fwd19,@@Fwd20,@@Fwd21,@@Fwd22,@@Fwd23,@@Fwd24
  dd      @@Fwd25,@@Fwd26,@@Fwd27,@@Fwd28,@@Fwd29,@@Fwd30,@@Fwd31,@@Fwd32
  dd      @@Fwd33,@@Fwd34,@@Fwd35,@@Fwd36
@@Fwd36:
  mov     ecx,[eax-36]
  mov     [edx-36],ecx
@@Fwd32:
  mov     ecx,[eax-32]
  mov     [edx-32],ecx
@@Fwd28:
  mov     ecx,[eax-28]
  mov     [edx-28],ecx
@@Fwd24:
  mov     ecx,[eax-24]
  mov     [edx-24],ecx
@@Fwd20:
  mov     ecx,[eax-20]
  mov     [edx-20],ecx
@@Fwd16:
  mov     ecx,[eax-16]
  mov     [edx-16],ecx
@@Fwd12:
  mov     ecx,[eax-12]
  mov     [edx-12],ecx
@@Fwd08:
  mov     ecx,[eax-8]
  mov     [edx-8],ecx
@@Fwd04:
  mov     ecx,[eax-4]
  mov     [edx-4],ecx
  ret
@@Fwd35:
  mov     ecx,[eax-35]
  mov     [edx-35],ecx
@@Fwd31:
  mov     ecx,[eax-31]
  mov     [edx-31],ecx
@@Fwd27:
  mov     ecx,[eax-27]
  mov     [edx-27],ecx
@@Fwd23:
  mov     ecx,[eax-23]
  mov     [edx-23],ecx
@@Fwd19:
  mov     ecx,[eax-19]
  mov     [edx-19],ecx
@@Fwd15:
  mov     ecx,[eax-15]
  mov     [edx-15],ecx
@@Fwd11:
  mov     ecx,[eax-11]
  mov     [edx-11],ecx
@@Fwd07:
  mov     ecx,[eax-7]
  mov     [edx-7],ecx
@@Fwd03:
  mov     cx,[eax-3]
  mov     [edx-3],cx
  mov     cl,[eax-1]
  mov     [edx-1],cl
  ret
@@Fwd34:
  mov     ecx,[eax-34]
  mov     [edx-34],ecx
@@Fwd30:
  mov     ecx,[eax-30]
  mov     [edx-30],ecx
@@Fwd26:
  mov     ecx,[eax-26]
  mov     [edx-26],ecx
@@Fwd22:
  mov     ecx,[eax-22]
  mov     [edx-22],ecx
@@Fwd18:
  mov     ecx,[eax-18]
  mov     [edx-18],ecx
@@Fwd14:
  mov     ecx,[eax-14]
  mov     [edx-14],ecx
@@Fwd10:
  mov     ecx,[eax-10]
  mov     [edx-10],ecx
@@Fwd06:
  mov     ecx,[eax-6]
  mov     [edx-6],ecx
@@Fwd02:
  mov     cx,[eax-2]
  mov     [edx-2],cx
  ret
@@Fwd33:
  mov     ecx,[eax-33]
  mov     [edx-33],ecx
@@Fwd29:
  mov     ecx,[eax-29]
  mov     [edx-29],ecx
@@Fwd25:
  mov     ecx,[eax-25]
  mov     [edx-25],ecx
@@Fwd21:
  mov     ecx,[eax-21]
  mov     [edx-21],ecx
@@Fwd17:
  mov     ecx,[eax-17]
  mov     [edx-17],ecx
@@Fwd13:
  mov     ecx,[eax-13]
  mov     [edx-13],ecx
@@Fwd09:
  mov     ecx,[eax-9]
  mov     [edx-9],ecx
@@Fwd05:
  mov     ecx,[eax-5]
  mov     [edx-5],ecx
@@Fwd01:
  mov     cl,[eax-1]
  mov     [edx-1],cl
@@Done:
  ret
end; {SmallForwardMove}

procedure SmallBackwardMove;
asm
  jmp     dword ptr [@@BwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@BwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Bwd01,@@Bwd02,@@Bwd03,@@Bwd04,@@Bwd05,@@Bwd06,@@Bwd07,@@Bwd08
  dd      @@Bwd09,@@Bwd10,@@Bwd11,@@Bwd12,@@Bwd13,@@Bwd14,@@Bwd15,@@Bwd16
  dd      @@Bwd17,@@Bwd18,@@Bwd19,@@Bwd20,@@Bwd21,@@Bwd22,@@Bwd23,@@Bwd24
  dd      @@Bwd25,@@Bwd26,@@Bwd27,@@Bwd28,@@Bwd29,@@Bwd30,@@Bwd31,@@Bwd32
  dd      @@Bwd33,@@Bwd34,@@Bwd35,@@Bwd36
@@Bwd36:
  mov     ecx,[eax+32]
  mov     [edx+32],ecx
@@Bwd32:
  mov     ecx,[eax+28]
  mov     [edx+28],ecx
@@Bwd28:
  mov     ecx,[eax+24]
  mov     [edx+24],ecx
@@Bwd24:
  mov     ecx,[eax+20]
  mov     [edx+20],ecx
@@Bwd20:
  mov     ecx,[eax+16]
  mov     [edx+16],ecx
@@Bwd16:
  mov     ecx,[eax+12]
  mov     [edx+12],ecx
@@Bwd12:
  mov     ecx,[eax+8]
  mov     [edx+8],ecx
@@Bwd08:
  mov     ecx,[eax+4]
  mov     [edx+4],ecx
@@Bwd04:
  mov     ecx,[eax]
  mov     [edx],ecx
  ret
@@Bwd35:
  mov     ecx,[eax+31]
  mov     [edx+31],ecx
@@Bwd31:
  mov     ecx,[eax+27]
  mov     [edx+27],ecx
@@Bwd27:
  mov     ecx,[eax+23]
  mov     [edx+23],ecx
@@Bwd23:
  mov     ecx,[eax+19]
  mov     [edx+19],ecx
@@Bwd19:
  mov     ecx,[eax+15]
  mov     [edx+15],ecx
@@Bwd15:
  mov     ecx,[eax+11]
  mov     [edx+11],ecx
@@Bwd11:
  mov     ecx,[eax+7]
  mov     [edx+7],ecx
@@Bwd07:
  mov     ecx,[eax+3]
  mov     [edx+3],ecx
@@Bwd03:
  mov     cx,[eax+1]
  mov     [edx+1],cx
  mov     cl,[eax]
  mov     [edx],cl
  ret
@@Bwd34:
  mov     ecx,[eax+30]
  mov     [edx+30],ecx
@@Bwd30:
  mov     ecx,[eax+26]
  mov     [edx+26],ecx
@@Bwd26:
  mov     ecx,[eax+22]
  mov     [edx+22],ecx
@@Bwd22:
  mov     ecx,[eax+18]
  mov     [edx+18],ecx
@@Bwd18:
  mov     ecx,[eax+14]
  mov     [edx+14],ecx
@@Bwd14:
  mov     ecx,[eax+10]
  mov     [edx+10],ecx
@@Bwd10:
  mov     ecx,[eax+6]
  mov     [edx+6],ecx
@@Bwd06:
  mov     ecx,[eax+2]
  mov     [edx+2],ecx
@@Bwd02:
  mov     cx,[eax]
  mov     [edx],cx
  ret
@@Bwd33:
  mov     ecx,[eax+29]
  mov     [edx+29],ecx
@@Bwd29:
  mov     ecx,[eax+25]
  mov     [edx+25],ecx
@@Bwd25:
  mov     ecx,[eax+21]
  mov     [edx+21],ecx
@@Bwd21:
  mov     ecx,[eax+17]
  mov     [edx+17],ecx
@@Bwd17:
  mov     ecx,[eax+13]
  mov     [edx+13],ecx
@@Bwd13:
  mov     ecx,[eax+9]
  mov     [edx+9],ecx
@@Bwd09:
  mov     ecx,[eax+5]
  mov     [edx+5],ecx
@@Bwd05:
  mov     ecx,[eax+1]
  mov     [edx+1],ecx
@@Bwd01:
  mov     cl,[eax]
  mov     [edx],cl
@@Done:
  ret
end; {SmallBackwardMove}

procedure Forwards_IA32;
asm
  push    ebx
  mov     ebx,edx
  fild    qword ptr [eax]
  add     eax,ecx {QWORD Align Writes}
  add     ecx,edx
  add     edx,7
  and     edx,-8
  sub     ecx,edx
  add     edx,ecx {Now QWORD Aligned}
  sub     ecx,16
  neg     ecx
@FwdLoop:
  fild    qword ptr [eax+ecx-16]
  fistp   qword ptr [edx+ecx-16]
  fild    qword ptr [eax+ecx-8]
  fistp   qword ptr [edx+ecx-8]
  add     ecx,16
  jle     @FwdLoop
  fistp   qword ptr [ebx]
  neg     ecx
  add     ecx,16
  pop     ebx
  jmp     SmallForwardMove
end;

procedure Backwards_IA32;
asm
  push    ebx
  fild    qword ptr [eax+ecx-8]
  lea     ebx,[edx+ecx] {QWORD Align Writes}
  and     ebx,7
  sub     ecx,ebx
  add     ebx,ecx {Now QWORD Aligned, EBX = Original Length}
  sub     ecx,16
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fild    qword ptr [eax+ecx+8]
  fistp   qword ptr [edx+ecx+8]
  fistp   qword ptr [edx+ecx]
  sub     ecx,16
  jge     @BwdLoop
  fistp   qword ptr [edx+ebx-8]
  add     ecx,16
  pop     ebx
  jmp     SmallBackwardMove
end;

procedure MoveFastcodeRTL(const Source; var Dest; Count : Integer);
asm
  cmp     ecx,SMALLMOVESIZE
  ja      @Large
  cmp     eax,edx
  lea     eax,[eax+ecx]
  jle     @SmallCheck
@SmallForward:
  add     edx,ecx
  jmp     SmallForwardMove
@SmallCheck:
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     eax,ecx
  jmp     SmallBackwardMove
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax,edx
  jg      Forwards_IA32
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  push    eax
  add     eax,ecx
  cmp     eax,edx
  pop     eax
  jg      Backwards_IA32 {Source/Dest Overlap}
  jmp     Forwards_IA32
@Done:
end; {MoveJOH_IA32}

//Author:            John O'Harrow
//Optimized for:     Pascal
//Instructionset(s): IA32
//Original name:     MoveJOH_PAS_3

procedure MoveFastcodePascal(const Source; var Dest; Count : Integer);
var
  S, D       : Cardinal;
  Temp, C, I : Integer;
  L          : PInteger;
begin
  S := Cardinal(@Source);
  D := Cardinal(@Dest);
  if S = D then
    Exit;
  if Count <= 4 then
    case Count of
      1 : PByte(@Dest)^ := PByte(S)^;
      2 : PWord(@Dest)^ := PWord(S)^;
      3 : if D > S then
            begin
              PByte(Integer(@Dest)+2)^ := PByte(S+2)^;
              PWord(@Dest)^ := PWord(S)^;
            end
          else
            begin
              PWord(@Dest)^ := PWord(S)^;
              PByte(Integer(@Dest)+2)^ := PByte(S+2)^;
            end;
      4 : PInteger(@Dest)^ := PInteger(S)^
      else Exit; {Count <= 0}
    end
  else
    if D > S then
      begin
        Temp := PInteger(S)^;
        I := Integer(@Dest);
        C := Count - 4;
        L := PInteger(Integer(@Dest) + C);
        Inc(S, C);
        repeat
          L^ := PInteger(S)^;
          if Count <= 8 then
            Break;
          Dec(Count, 4);
          Dec(S, 4);
          Dec(L);
        until False;
        PInteger(I)^ := Temp;
      end
    else
      begin
        C := Count - 4;
        Temp := PInteger(S + Cardinal(C))^;
        I := Integer(@Dest) + C;
        L := @Dest;
        repeat
          L^ := PInteger(S)^;
          if Count <= 8 then
            Break;
          Dec(Count, 4);
          Inc(S, 4);
          Inc(L);
        until False;
        PInteger(I)^ := Temp;
      end;
end; {MoveJOH_PAS}

end.
