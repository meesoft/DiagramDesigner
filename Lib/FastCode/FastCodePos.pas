unit FastCodePos;

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
  FastCodePosFunction = function(const SubStr: AnsiString; const Str: AnsiString): Integer;

{Functions shared between Targets}
function FastCodePosRTL    (const SubStr: AnsiString; const Str: AnsiString): Integer;
function FastCodePosBlended(const SubStr: AnsiString; const Str: AnsiString): Integer;
function FastCodePosXP     (const SubStr: AnsiString; const Str: AnsiString): Integer;
{Functions not shared between Targets}
function FastCodePosP4N   (const SubStr: AnsiString; const Str: AnsiString): Integer;
function FastCodePosPMD   (const SubStr: AnsiString; const Str: AnsiString): Integer;
function FastCodePosPascal(const SubStr: AnsiString; const Str: AnsiString): Integer;


const
  Version = '0.3';

  FastCodePosP4P  : FastCodePosFunction = FastCodePosBlended;
  FastCodePosPMB  : FastCodePosFunction = FastCodePosBlended;
  FastCodePosAMD64: FastCodePosFunction = FastCodePosXP;

function PosStub(const substr: AnsiString; const s: AnsiString): Integer;

implementation

//Author:            Aleksandr Sharahov
//Optimized for:     Intel P4 Prescott
//Instructionset(s): IA32
//Original name:     PosShaAsm4

function FastCodePosRTL(const SubStr: AnsiString; const Str: AnsiString): Integer;
asm
       push  ebx
       push  esi
       add   esp, -16
       test  edx, edx
       jz    @NotFound
       test  eax, eax
       jz    @NotFound
       mov   esi, [edx-4] //Length(Str)
       mov   ebx, [eax-4] //Length(Substr)
       cmp   esi, ebx
       jl    @NotFound
       test  ebx, ebx
       jle   @NotFound
       dec   ebx
       add   esi, edx
       add   edx, ebx
       mov   [esp+8], esi
       add   eax, ebx
       mov   [esp+4], edx
       neg   ebx
       movzx ecx, byte ptr [eax]
       mov   [esp], ebx
       jnz   @FindString

       sub   esi, 2
       mov   [esp+12], esi

@FindChar2:
       cmp   cl, [edx]
       jz    @Matched0ch
       cmp   cl, [edx+1]
       jz    @Matched1ch
       add   edx, 2
       cmp   edx, [esp+12]
       jb    @FindChar4
       cmp   edx, [esp+8]
       jb    @FindChar2
@NotFound:
       xor   eax, eax
       jmp   @Exit0ch

@FindChar4:
       cmp   cl, [edx]
       jz    @Matched0ch
       cmp   cl, [edx+1]
       jz    @Matched1ch
       cmp   cl, [edx+2]
       jz    @Matched2ch
       cmp   cl, [edx+3]
       jz    @Matched3ch
       add   edx, 4
       cmp   edx, [esp+12]
       jb    @FindChar4
       cmp   edx, [esp+8]
       jb    @FindChar2
       xor   eax, eax
       jmp   @Exit0ch

@Matched2ch:
       add   edx, 2
@Matched0ch:
       inc   edx
       mov   eax, edx
       sub   eax, [esp+4]
@Exit0ch:
       add   esp, 16
       pop   esi
       pop   ebx
       ret

@Matched3ch:
       add   edx, 2
@Matched1ch:
       add   edx, 2
       xor   eax, eax
       cmp   edx, [esp+8]
       ja    @Exit1ch
       mov   eax, edx
       sub   eax, [esp+4]
@Exit1ch:
       add   esp, 16
       pop   esi
       pop   ebx
       ret

@FindString4:
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @Test1
       cmp   cl, [edx+2]
       jz    @Test2
       cmp   cl, [edx+3]
       jz    @Test3
       add   edx, 4
       cmp   edx, [esp+12]
       jb    @FindString4
       cmp   edx, [esp+8]
       jb    @FindString2
       xor   eax, eax
       jmp   @Exit1

@FindString:
       sub   esi, 2
       mov   [esp+12], esi
@FindString2:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @Test1
@AfterTest1:
       add   edx, 2
       cmp   edx, [esp+12]
       jb    @FindString4
       cmp   edx, [esp+8]
       jb    @FindString2
       xor   eax, eax
       jmp   @Exit1

@Test3:
       add   edx, 2
@Test1:
       mov   esi, [esp]
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTest1
       add   esi, 2
       jl    @Loop1
       add   edx, 2
       xor   eax, eax
       cmp   edx, [esp+8]
       ja    @Exit1
@RetCode1:
       mov   eax, edx
       sub   eax, [esp+4]
@Exit1:
       add   esp, 16
       pop   esi
       pop   ebx
       ret

@Test2:
       add   edx,2
@Test0:
       mov   esi, [esp]
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @AfterTest0
       add   esi, 2
       jl    @Loop0
       inc   edx
@RetCode0:
       mov   eax, edx
       sub   eax, [esp+4]
       add   esp, 16
       pop   esi
       pop   ebx
end;

//Author:            Aleksandr Sharahov
//Optimized for:     Intel P4 Northwood
//Instructionset(s): IA32
//Original name:     PosShaAsm3

function FastCodePosBlended(const SubStr: AnsiString; const Str: AnsiString): Integer;
asm
       push  ebx
       push  esi
       add   esp, -16
       test  eax, eax
       jz    @NotFound
       test  edx, edx
       jz    @NotFound
       mov   ebx, [eax-4]
       mov   esi, [edx-4]
       cmp   esi, ebx
       jl    @NotFound
       test  ebx, ebx
       jle   @NotFound
       dec   ebx
       add   esi, edx
       add   edx, ebx
       mov   [esp+8], esi
       add   eax, ebx
       mov   [esp+4], edx
       neg   ebx
       movzx ecx, byte ptr [eax]
       mov   [esp], ebx
       jnz   @FindString

       sub   esi, 2
       mov   [esp+12], esi

@FindChar2:
       cmp   cl, [edx]
       jz    @Matched0
       cmp   cl, [edx+1]
       jz    @Matched1
       add   edx, 2
       cmp   edx, [esp+12]
       jb    @FindChar4
       cmp   edx, [esp+8]
       jb    @FindChar2
@NotFound:
       xor   eax, eax
       jmp   @Exit
@FindChar4:
       cmp   cl, [edx]
       jz    @Matched0
       cmp   cl, [edx+1]
       jz    @Matched1
       cmp   cl, [edx+2]
       jz    @Matched2
       cmp   cl, [edx+3]
       jz    @Matched3
       add   edx, 4
       cmp   edx, [esp+12]
       jb    @FindChar4
       cmp   edx, [esp+8]
       jb    @FindChar2
       xor   eax, eax
       jmp   @Exit

@FindString:
       sub   esi, 2
       mov   [esp+12], esi
@FindString2:
       cmp   cl, [edx]
       jz    @Test0
@NotMatched0:
       cmp   cl, [edx+1]
       jz    @Test1
@NotMatched1:
       add   edx, 2
       cmp   edx, [esp+12]
       jb    @FindString4
       cmp   edx, [esp+8]
       jb    @FindString2
       xor   eax, eax
       jmp   @Exit

@FindString4:
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @Test1
       cmp   cl, [edx+2]
       jz    @Test2
       cmp   cl, [edx+3]
       jz    @Test3
       add   edx, 4
       cmp   edx, [esp+12]
       jb    @FindString4
       cmp   edx, [esp+8]
       jb    @FindString2
       xor   eax, eax
       jmp   @Exit

@Test3:
       add   edx,2
@Test1:
       mov   esi, [esp]
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @NotMatched1
       add   esi, 2
       jl    @Loop1
@Matched1:
       add   edx, 2
       xor   eax, eax
       cmp   edx, [esp+8]
       ja    @Exit1
@RetCode1:
       mov   eax, edx
       sub   eax, [esp+4]
@Exit1:
       add   esp, 16
       pop   esi
       pop   ebx
       ret
@Matched3:
       add   edx, 4
       xor   eax, eax
       cmp   edx, [esp+8]
       jbe   @RetCode1
       jmp   @Exit1

@Matched2:
       add   edx, 3
       jmp   @RetCode0
@Test2:
       add   edx,2
@Test0:
       mov   esi, [esp]
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @NotMatched0
       add   esi, 2
       jl    @Loop0
@Matched0:
       inc   edx
@RetCode0:
       mov   eax, edx
       sub   eax, [esp+4]
@Exit:
       add   esp, 16
       pop   esi
       pop   ebx
end;

//Author:            John O'Harrow
//Optimized for:     AMD Athlon 64, Opteron, FX
//Instructionset(s): IA32
//Original name:     PosJOH_SSE_3

function FastCodePosXP(const SubStr: AnsiString; const Str: AnsiString): Integer;
asm
  test      eax, eax
  jz        @@NotFoundExit   {Exit if SubStr = ''}
  test      edx, edx
  jz        @@NotFound       {Exit if Str = ''}
  mov       ecx, [edx-4]     {Length(Str)}
  cmp       [eax-4], 1       {Length SubStr = 1?}
  je        @@SingleChar     {Yes - Exit via CharPos}
  jl        @@NotFound       {Exit if Length(SubStr) < 1}
  sub       ecx, [eax-4]     {Subtract Length(SubStr), -ve handled by CharPos}
  push      esi              {Save Registers}
  push      edi
  push      ebx
  push      ebp
  push      edx              {Save Start Address of Str}
  mov       ebx, [eax]       {BL = 1st Char of SubStr}
  mov       esi, eax         {Start Address of SubStr}
  lea       edi, [ecx+1]     {Initial Remainder Count}
@@StrLoop:
  mov       eax, ebx         {AL  = 1st char of SubStr}
  mov       ecx, edi         {Remaining Length}
  mov       ebp, edx         {Save Start Position}
  call      @@CharPos        {Search for 1st Character}
  jz        @@StrExit        {Exit with Zero Result if 1st Char Not Found}
  mov       ecx, [esi-4]     {Length SubStr}
  lea       edx, [ebp+eax]   {Update Start Position}
  sub       ecx, 1           {Remaining Characters to Compare}
  sub       edi, eax         {Update Remaining Length for Next Loop}
@@StrCheck:
  mov       ax, [edx+ecx-2]  {Compare Next 2 Chars of SubStr and Str}
  cmp       ax, [esi+ecx-1]
  jne       @@StrLoop        {Different - Return to First Character Search}
  sub       ecx, 2
  jg        @@StrCheck       {Check each Remaining Character}
  sub       edx, [esp]
  mov       eax, edx
@@StrExit:
  pop       edx              {Restore Registers}
  pop       ebp
  pop       ebx
  pop       edi
  pop       esi
  ret
@@NotFound:
  xor       eax, eax         {Return 0}
@@NotFoundExit:
  ret
@@SingleChar:
  mov       al, [eax]        {Search Character}
{Return Position of Character AL within a String of Length ECX starting}
{at Address EDX.  If Found, Return Index in EAX and Clear Zero Flag,   }
{otherwise Return 0 in EAX and Set Zero Flag.  Changes EAX, ECX and EDX}
@@CharPos:
  cmp       ecx, 8
  jg        @@NotSmall
@@Small:
  push      ecx
  or        ecx, ecx
  jle       @@SmallNotFound      {Exit if Length <= 0}
  cmp       al, [edx]
  jz        @@Found
  sub       ecx, 1
  jz        @@SmallNotFound
  cmp       al, [edx+1]
  jz        @@Found
  sub       ecx, 1
  jz        @@SmallNotFound
  cmp       al, [edx+2]
  jz        @@Found
  sub       ecx, 1
  jz        @@SmallNotFound
  cmp       al, [edx+3]
  jz        @@Found
  sub       ecx, 1
  jz        @@SmallNotFound
  cmp       al, [edx+4]
  jz        @@Found
  sub       ecx, 1
  jz        @@SmallNotFound
  cmp       al, [edx+5]
  jz        @@Found
  sub       ecx, 1
  jz        @@SmallNotFound
  cmp       al, [edx+6]
  jz        @@Found
  sub       ecx, 1
  jz        @@SmallNotFound
  cmp       al, [edx+7]
  jz        @@Found
@@SmallNotFound:
  pop       ecx
  xor       eax, eax
  ret
@@Found:
  pop       eax
  sub       eax, ecx
  add       eax, 1
  ret

@@NotSmall:
  MOV       AH, AL
  ADD       EDX, ECX
  MOVD      MM0, EAX
  PSHUFW    MM0, MM0, 0
  PUSH      ECX
  NEG       ECX
@@First8:
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare Next 8 Bytes}
  PMOVMSKB  EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
  JGE       @@Last8
@@Align:
  LEA       EAX, [EDX+ECX]
  AND       EAX, 7
  SUB       ECX, EAX
@@Loop:                      {Loop Unrolled 2X}
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare Next 8 Bytes}
  PMOVMSKB  EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
  JL        @@loop
@@Last8:
  PCMPEQB   MM0, [EDX-8]
  POP       ECX              {Original Length}
  PMOVMSKB  EAX, MM0
  TEST      EAX, EAX
  JNZ       @@Matched2
  EMMS
  RET                        {Finished}
@@Matched:                   {Set Result from 1st Match in EcX}
  POP       EDX              {Original Length}
  ADD       ECX, EDX
@@Matched2:
  EMMS
  BSF       EDX, EAX
  LEA       EAX, [EDX+ECX-7]
end;

//Author:            Aleksandr Sharahov
//Optimized for:     Blended
//Instructionset(s): IA32
//Original name:     PosShaAsm5

function FastCodePosP4N(const SubStr: AnsiString; const Str: AnsiString): Integer;
asm
       push  ebx
       push  esi
       add   esp, -16
       test  edx, edx
       jz    @NotFound
       test  eax, eax
       jz    @NotFound
       mov   ebx, [eax-4]
       mov   esi, [edx-4]
       cmp   esi, ebx
       jl    @NotFound
       test  ebx, ebx
       jle   @NotFound
       dec   ebx
       add   esi, edx
       add   edx, ebx
       mov   [esp+8], esi
       add   eax, ebx
       mov   [esp+4], edx
       neg   ebx
       movzx ecx, byte ptr [eax]
       mov   [esp], ebx
       jnz   @FindString

@FindChar:
       cmp   cl, [edx]
       jz    @Matched0
       cmp   cl, [edx+1]
       jz    @Matched1
       add   edx, 2
       cmp   edx, [esp+8]
       jb    @FindChar
@NotFound:
       xor   eax, eax
       jmp   @Exit

@FindString:
       sub   esi, 2
       mov   [esp+12], esi
@FindString2:
       cmp   cl, [edx]
       jz    @Test0
@NotMatched0:
       cmp   cl, [edx+1]
       jz    @Test1
@NotMatched1:
       add   edx, 2
       cmp   edx, [esp+12]
       jb    @FindString4
       cmp   edx, [esp+8]
       jb    @FindString2
       xor   eax, eax
       jmp   @Exit

@FindString4:
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @Test1
       cmp   cl, [edx+2]
       jz    @Test2
       cmp   cl, [edx+3]
       jz    @Test3
       add   edx, 4
       cmp   edx, [esp+12]
       jb    @FindString4
       cmp   edx, [esp+8]
       jb    @FindString2
       xor   eax, eax
       jmp   @Exit

@Test3:
       add   edx,2
@Test1:
       mov   esi, [esp]
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @NotMatched1
       add   esi, 2
       jl    @Loop1
@Matched1:
       add   edx, 2
       cmp   edx, [esp+8]
       jbe   @RetCode
       xor   eax, eax
       jmp   @Exit

@Test2:
       add   edx,2
@Test0:
       mov   esi, [esp]
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @NotMatched0
       add   esi, 2
       jl    @Loop0
@Matched0:
       inc   edx
@RetCode:
       mov   eax, edx
       sub   eax, [esp+4]
@Exit:
       add   esp, 16
       pop   esi
       pop   ebx
end;

//Author:            John O'Harrow
//Optimized for:     Intel Pentium M Dothan
//Instructionset(s): IA32
//Original name:     PosJOH_SSE2_3

function FastCodePosPMD(const SubStr: AnsiString; const Str: AnsiString): Integer;
asm
  test      eax, eax
  jz        @NotFoundExit    {Exit if SurStr = ''}
  test      edx, edx
  jz        @NotFound        {Exit if Str = ''}
  mov       ecx, [edx-4]     {Length(Str)}
  cmp       [eax-4], 1       {Length SubStr = 1?}
  je        @SingleChar      {Yes - Exit via CharPos}
  jl        @NotFound        {Exit if Length(SubStr) < 1}
  sub       ecx, [eax-4]     {Subtract Length(SubStr)}
  jl        @NotFound        {Exit if Length(SubStr) > Length(Str)}
  push      esi              {Save Registers}
  push      edi
  push      ebx
  push      ebp
  mov       esi, eax         {Start Address of SubStr}
  lea       edi, [ecx+1]     {Initial Remainder Count}
  mov       eax, [eax]       {AL = 1st Char of SubStr}
  mov       ebp, edx         {Start Address of Str}
  mov       ebx, eax         {Maintain 1st Search Char in BL}
@StrLoop:
  mov       eax, ebx         {AL  = 1st char of SubStr}
  mov       ecx, edi         {Remaining Length}
  push      edx              {Save Start Position}
  call      @CharPos         {Search for 1st Character}
  pop       edx              {Restore Start Position}
  test      eax, eax         {Result = 0?}
  jz        @StrExit         {Exit if 1st Character Not Found}
  mov       ecx, [esi-4]     {Length SubStr}
  add       edx, eax         {Update Start Position for Next Loop}
  sub       edi, eax         {Update Remaining Length for Next Loop}
  sub       ecx, 1           {Remaining Characters to Compare}
@StrCheck:
  mov       ax, [edx+ecx-2]  {Compare Next Char of SubStr and Str}
  cmp       ax, [esi+ecx-1]
  jne       @StrLoop         {Different - Return to First Character Search}
  sub       ecx, 2
  jg        @StrCheck        {Check each Remaining Character}
  mov       eax, edx         {All Characters Matched - Calculate Result}
  sub       eax, ebp
@StrExit:
  pop       ebp              {Restore Registers}
  pop       ebx
  pop       edi
  pop       esi
  ret
@NotFound:
  xor       eax, eax         {Return 0}
@NotFoundExit:
  ret
@SingleChar:
  mov       al, [eax]        {Search Character}
@CharPos:
  PUSH      EBX
  MOV       EBX, EAX
  CMP       ECX, 16
  JL        @@Small
@@NotSmall:
  MOV       AH, AL           {Fill each Byte of XMM1 with AL}
  MOVD      XMM1, EAX
  PSHUFLW   XMM1, XMM1, 0
  PSHUFD    XMM1, XMM1, 0
@@First16:
  MOVUPS    XMM0, [EDX]      {Unaligned}
  PCMPEQB   XMM0, XMM1       {Compare First 16 Characters}
  PMOVMSKB  EAX, XMM0
  TEST      EAX, EAX
  JNZ       @@FoundStart     {Exit on any Match}
  CMP       ECX, 32
  JL        @@Medium         {If Length(Str) < 32, Check Remainder}
@@Align:
  SUB       ECX, 16          {Align Block Reads}
  PUSH      ECX
  MOV       EAX, EDX
  NEG       EAX
  AND       EAX, 15
  ADD       EDX, ECX
  NEG       ECX
  ADD       ECX, EAX
@@Loop:
  MOVAPS    XMM0, [EDX+ECX]  {Aligned}
  PCMPEQB   XMM0, XMM1       {Compare Next 16 Characters}
  PMOVMSKB  EAX, XMM0
  TEST      EAX, EAX
  JNZ       @@Found          {Exit on any Match}
  ADD       ECX, 16
  JLE       @@Loop
@Remainder:
  POP       EAX              {Check Remaining Characters}
  ADD       EDX, 16
  ADD       EAX, ECX         {Count from Last Loop End Position}
  JMP       DWORD PTR [@@JumpTable2-ECX*4]

@@NullString:
  XOR       EAX, EAX         {Result = 0}
  RET

@@FoundStart:
  BSF       EAX, EAX         {Get Set Bit}
  POP       EBX
  ADD       EAX, 1           {Set Result}
  RET

@@Found:
  POP       EDX
  BSF       EAX, EAX         {Get Set Bit}
  ADD       EDX, ECX
  POP       EBX
  LEA       EAX, [EAX+EDX+1] {Set Result}
  RET

@@Medium:
  ADD       EDX, ECX         {End of String}
  MOV       EAX, 16          {Count from 16}
  JMP       DWORD PTR [@@JumpTable1-64-ECX*4]

@@Small:
  ADD       EDX, ECX         {End of String}
  XOR       EAX, EAX         {Count from 0}
  JMP       DWORD PTR [@@JumpTable1-ECX*4]

  nop; nop                   {Aligb Jump Tables}

@@JumpTable1:
  DD        @@NotFound, @@01, @@02, @@03, @@04, @@05, @@06, @@07
  DD        @@08, @@09, @@10, @@11, @@12, @@13, @@14, @@15, @@16

@@JumpTable2:
  DD        @@16, @@15, @@14, @@13, @@12, @@11, @@10, @@09, @@08
  DD        @@07, @@06, @@05, @@04, @@03, @@02, @@01, @@NotFound

@@16:
  ADD       EAX, 1
  CMP       BL, [EDX-16]
  JE        @@Done
@@15:
  ADD       EAX, 1
  CMP       BL, [EDX-15]
  JE        @@Done
@@14:
  ADD       EAX, 1
  CMP       BL, [EDX-14]
  JE        @@Done
@@13:
  ADD       EAX, 1
  CMP       BL, [EDX-13]
  JE        @@Done
@@12:
  ADD       EAX, 1
  CMP       BL, [EDX-12]
  JE        @@Done
@@11:
  ADD       EAX, 1
  CMP       BL, [EDX-11]
  JE        @@Done
@@10:
  ADD       EAX, 1
  CMP       BL, [EDX-10]
  JE        @@Done
@@09:
  ADD       EAX, 1
  CMP       BL, [EDX-9]
  JE        @@Done
@@08:
  ADD       EAX, 1
  CMP       BL, [EDX-8]
  JE        @@Done
@@07:
  ADD       EAX, 1
  CMP       BL, [EDX-7]
  JE        @@Done
@@06:
  ADD       EAX, 1
  CMP       BL, [EDX-6]
  JE        @@Done
@@05:
  ADD       EAX, 1
  CMP       BL, [EDX-5]
  JE        @@Done
@@04:
  ADD       EAX, 1
  CMP       BL, [EDX-4]
  JE        @@Done
@@03:
  ADD       EAX, 1
  CMP       BL, [EDX-3]
  JE        @@Done
@@02:
  ADD       EAX, 1
  CMP       BL, [EDX-2]
  JE        @@Done
@@01:
  ADD       EAX, 1
  CMP       BL, [EDX-1]
  JE        @@Done
@@NotFound:
  XOR       EAX, EAX
@@Done:
  POP       EBX
end;

//Author:            Aleksandr Sharahov
//Optimized for:     Blended / Pascal
//Instructionset(s): IA32
//Original name:     PosShaPas3

function FastCodePosPascal(const SubStr: AnsiString; const Str: AnsiString): Integer;
var
  len, lenSub: integer;
  ch: char;
  w: word;
  p, pSub, pStart, pStop: pchar;
label
  Ret, Ret0, Ret1, Next0, Next1, Found0, Found1;
begin;
  p:=pointer(Str);
  pSub:=pointer(SubStr);

  if (p=nil) or (pSub=nil) then begin;
    Result:=0;
    exit;
    end;

  len:=pinteger(p-4)^;
  lenSub:=pinteger(pSub-4)^;
  if (len<lenSub) or (lenSub<=0) then begin;
    Result:=0;
    exit;
    end;

  lenSub:=lenSub-1;
  pStop:=p+len;
  p:=p+lenSub;
  pSub:=pSub+lenSub;
  pStart:=p;

  ch:=pSub[0];

  if lenSub=0 then begin;
    repeat;
      w:=pword(p)^;
      p:=p+2;
      if ch=chr(Lo(w)) then goto Ret0;
      if ch=chr(Hi(w)) then goto Ret1;
      until p>=pStop;
    Result:=0;
    exit;
    end;

  lenSub:=-lenSub;
  repeat;
    w:=pword(p)^;
    p:=p+2;
    if ch=chr(Lo(w)) then goto Found0;
Next0:
    if ch=chr(Hi(w)) then goto  Found1;
Next1:
    until p>=pStop;
  Result:=0;
  exit;

Found1:
  len:=lenSub;
  repeat;
    if pword(psub+len)^<>pword(p+len-1)^ then goto Next1;
    len:=len+2;
    until len>=0;
Ret1:
  if p<=pStop then goto Ret;
  Result:=0;
  exit;
Found0:
  len:=lenSub;
  repeat;
    if pword(psub+len)^<>pword(p+len-2)^ then goto Next0;
    len:=len+2;
    until len>=0;
Ret0:
  dec(p);
Ret:
  Result:=p-pStart;
end;

function PosStub(const substr: AnsiString; const s: AnsiString): Integer;
asm
{$IFDEF DELPHI9}
  call System.Pos;
{$ELSE}
  call System.@LStrPos;
{$ENDIF}
end;

end.
