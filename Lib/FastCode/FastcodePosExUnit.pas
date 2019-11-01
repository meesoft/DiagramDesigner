unit FastcodePosExUnit;

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
 * Contributor(s): Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)

//Version : 0.1
//Only plain function calls supported
//Last update 24/5 2005

interface

function PosExFastcodeP4P(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExFastcodeP4N(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExFastcodePMD(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExFastcodePMB(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExFastcodeAMD64(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExFastcodeXP(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExFastcodeBlended(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExFastcodeRTL(const SubStr, S: string; Offset: Integer = 1): Integer;
function PosExFastcodePascal(const SubStr, S: string; Offset: Integer = 1): Integer;

implementation

//Author:            Aleksandr Sharahov
//Date:              25/5 2004
//Optimized for:     Intel P4 Prescott
//Instructionset(s): IA32
//Original name:     PosEx_Sha_IA32_3

function PosExFastcodeP4P(const SubStr, S: string; Offset: Integer = 1): Integer;
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first char at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last char at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last char of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last char of Substr

@SmallLoop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTest1:
       add   edx, 8
       cmp   edx, [esp+8]
       jae   @EndSmall
@MainLoop:
       cmp   cl, [edx-6]
       jz    @Test6
       cmp   cl, [edx-5]
       jz    @Test5
       cmp   cl, [edx-4]
       jz    @Test4
       cmp   cl, [edx-3]
       jz    @Test3
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jz    @Test1
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @TestT
       add   edx, 8
       cmp   edx, [esp+8]
       jb    @MainLoop
@EndSmall:
       add   edx, -6
       cmp   edx, [esp+8]
       jb    @SmallLoop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret

@Test6:
       add   edx, -2
@Test4:
       add   edx, -2
@Test2:
       add   edx, -2
@Test0:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found0
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @AfterTest0
       cmp   esi, -2
       jge   @Found0
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+2]
       jnz   @AfterTest0
       add   esi, 4
       jl    @Loop0
@Found0:
       mov   eax, [esp+4]
       add   edx, 1
       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       ret

@Test5:
       add   edx, -2
@Test3:
       add   edx, -2
@Test1:
       add   edx, -2
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found1
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTest1
       cmp   esi, -2
       jge   @Found1
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTest1
       add   esi, 4
       jl    @Loop1
@Found1:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       end;

//Author:            Aleksandr Sharahov
//Date:              25/5 2004
//Optimized for:     Intel P4 Northwood
//Instructionset(s): IA32
//Original name:     PosEx_Sha_IA32_3

function PosExFastcodeP4N(const SubStr, S: string; Offset: Integer = 1): Integer;
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first char at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last char at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last char of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last char of Substr

@SmallLoop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTest1:
       add   edx, 8
       cmp   edx, [esp+8]
       jae   @EndSmall
@MainLoop:
       cmp   cl, [edx-6]
       jz    @Test6
       cmp   cl, [edx-5]
       jz    @Test5
       cmp   cl, [edx-4]
       jz    @Test4
       cmp   cl, [edx-3]
       jz    @Test3
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jz    @Test1
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @TestT
       add   edx, 8
       cmp   edx, [esp+8]
       jb    @MainLoop
@EndSmall:
       add   edx, -6
       cmp   edx, [esp+8]
       jb    @SmallLoop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret

@Test6:
       add   edx, -2
@Test4:
       add   edx, -2
@Test2:
       add   edx, -2
@Test0:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found0
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @AfterTest0
       cmp   esi, -2
       jge   @Found0
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+2]
       jnz   @AfterTest0
       add   esi, 4
       jl    @Loop0
@Found0:
       mov   eax, [esp+4]
       add   edx, 1
       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       ret

@Test5:
       add   edx, -2
@Test3:
       add   edx, -2
@Test1:
       add   edx, -2
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found1
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTest1
       cmp   esi, -2
       jge   @Found1
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTest1
       add   esi, 4
       jl    @Loop1
@Found1:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       end;

//Author:
//Date:              25/5 2004
//Optimized for:     Intel Pentium M Dothan
//Instructionset(s): IA32
//Original name:

//function PosExFastcodePMD(const SubStr, S: string; Offset: Integer = 1): Integer;

//Author:
//Date:              25/5 2004
//Optimized for:     Intel Pentium M Banias
//Instructionset(s): IA32
//Original name:

//function PosExFastcodePMB(const SubStr, S: string; Offset: Integer = 1): Integer;

//Author:            Aleksandr Sharahov
//Date:              25/5 2004
//Optimized for:     AMD Athlon 64, Opteron, FX
//Instructionset(s): IA32
//Original name:     PosEx_Sha_IA32_3

function PosExFastcodeAMD64(const SubStr, S: string; Offset: Integer = 1): Integer;
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first char at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last char at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last char of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last char of Substr

@SmallLoop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTest1:
       add   edx, 8
       cmp   edx, [esp+8]
       jae   @EndSmall
@MainLoop:
       cmp   cl, [edx-6]
       jz    @Test6
       cmp   cl, [edx-5]
       jz    @Test5
       cmp   cl, [edx-4]
       jz    @Test4
       cmp   cl, [edx-3]
       jz    @Test3
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jz    @Test1
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @TestT
       add   edx, 8
       cmp   edx, [esp+8]
       jb    @MainLoop
@EndSmall:
       add   edx, -6
       cmp   edx, [esp+8]
       jb    @SmallLoop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret

@Test6:
       add   edx, -2
@Test4:
       add   edx, -2
@Test2:
       add   edx, -2
@Test0:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found0
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @AfterTest0
       cmp   esi, -2
       jge   @Found0
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+2]
       jnz   @AfterTest0
       add   esi, 4
       jl    @Loop0
@Found0:
       mov   eax, [esp+4]
       add   edx, 1
       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       ret

@Test5:
       add   edx, -2
@Test3:
       add   edx, -2
@Test1:
       add   edx, -2
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found1
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTest1
       cmp   esi, -2
       jge   @Found1
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTest1
       add   esi, 4
       jl    @Loop1
@Found1:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       end;

//Author:            Aleksandr Sharahov
//Date:              25/5 2004
//Optimized for:     AMD Athlon XP
//Instructionset(s): IA32
//Original name:     PosEx_Sha_IA32_3

function PosExFastcodeXP(const SubStr, S: string; Offset: Integer = 1): Integer;
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first char at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last char at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last char of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last char of Substr

@SmallLoop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTest1:
       add   edx, 8
       cmp   edx, [esp+8]
       jae   @EndSmall
@MainLoop:
       cmp   cl, [edx-6]
       jz    @Test6
       cmp   cl, [edx-5]
       jz    @Test5
       cmp   cl, [edx-4]
       jz    @Test4
       cmp   cl, [edx-3]
       jz    @Test3
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jz    @Test1
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @TestT
       add   edx, 8
       cmp   edx, [esp+8]
       jb    @MainLoop
@EndSmall:
       add   edx, -6
       cmp   edx, [esp+8]
       jb    @SmallLoop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret

@Test6:
       add   edx, -2
@Test4:
       add   edx, -2
@Test2:
       add   edx, -2
@Test0:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found0
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @AfterTest0
       cmp   esi, -2
       jge   @Found0
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+2]
       jnz   @AfterTest0
       add   esi, 4
       jl    @Loop0
@Found0:
       mov   eax, [esp+4]
       add   edx, 1
       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       ret

@Test5:
       add   edx, -2
@Test3:
       add   edx, -2
@Test1:
       add   edx, -2
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found1
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTest1
       cmp   esi, -2
       jge   @Found1
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTest1
       add   esi, 4
       jl    @Loop1
@Found1:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       end;

//Author:            Aleksandr Sharahov
//Date:              25/5 2004
//Optimized for:     Blended
//Instructionset(s): IA32
//Original name:     PosEx_Sha_IA32_3

function PosExFastcodeBlended(const SubStr, S: string; Offset: Integer = 1): Integer;
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first char at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last char at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last char of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last char of Substr

@SmallLoop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTest1:
       add   edx, 8
       cmp   edx, [esp+8]
       jae   @EndSmall
@MainLoop:
       cmp   cl, [edx-6]
       jz    @Test6
       cmp   cl, [edx-5]
       jz    @Test5
       cmp   cl, [edx-4]
       jz    @Test4
       cmp   cl, [edx-3]
       jz    @Test3
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jz    @Test1
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @TestT
       add   edx, 8
       cmp   edx, [esp+8]
       jb    @MainLoop
@EndSmall:
       add   edx, -6
       cmp   edx, [esp+8]
       jb    @SmallLoop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret

@Test6:
       add   edx, -2
@Test4:
       add   edx, -2
@Test2:
       add   edx, -2
@Test0:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found0
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @AfterTest0
       cmp   esi, -2
       jge   @Found0
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+2]
       jnz   @AfterTest0
       add   esi, 4
       jl    @Loop0
@Found0:
       mov   eax, [esp+4]
       add   edx, 1
       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       ret

@Test5:
       add   edx, -2
@Test3:
       add   edx, -2
@Test1:
       add   edx, -2
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found1
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTest1
       cmp   esi, -2
       jge   @Found1
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTest1
       add   esi, 4
       jl    @Loop1
@Found1:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       end;

//Author:            Aleksandr Sharahov
//Date:              25/5 2004
//Optimized for:     RTL Replacement
//Instructionset(s): IA32
//Original name:     PosEx_Sha_IA32_4

function PosExFastcodeRTL(const SubStr, S: string; Offset: Integer = 1): Integer;
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first char at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last char at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last char of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last char of Substr

@Loop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTestT:
       add   edx, 4
       cmp   edx, [esp+8]
       jb   @Continue
@EndLoop:
       add   edx, -2
       cmp   edx, [esp+8]
       jb    @Loop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret
@Continue:
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jnz   @Loop
@Test1:
       add   edx,  1
@Test2:
       add   edx, -2
@Test0:
       add   edx, -1
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found
@String:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTestT
       cmp   esi, -2
       jge   @Found
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTestT
       add   esi, 4
       jl    @String
@Found:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       end;

//Author:            Aleksandr Sharahov
//Date:              25/5 2004
//Optimized for:     Blended / Pascal
//Instructionset(s): IA32
//Original name:     PosEx_Sha_Pas_2

function PosExFastcodePascal(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  len, lenSub: integer;
  ch: char;
  p, pSub, pStart, pStop: pchar;
label
  Loop0, Loop4,
  TestT, Test0, Test1, Test2, Test3, Test4,
  AfterTestT, AfterTest0,
  Ret, Exit;
begin;
  pSub:=pointer(SubStr);
  p:=pointer(S);

  if (p=nil) or (pSub=nil) or (Offset<1) then begin;
    Result:=0;
    goto Exit;
    end;

  lenSub:=pinteger(pSub-4)^-1;
  len:=pinteger(p-4)^;
  if (len<lenSub+Offset) or (lenSub<0) then begin;
    Result:=0;
    goto Exit;
    end;

  pStop:=p+len;
  p:=p+lenSub;
  pSub:=pSub+lenSub;
  pStart:=p;
  p:=p+Offset+3;

  ch:=pSub[0];
  lenSub:=-lenSub;
  if p<pStop then goto Loop4;
  p:=p-4;
  goto Loop0;

Loop4:
  if ch=p[-4] then goto Test4;
  if ch=p[-3] then goto Test3;
  if ch=p[-2] then goto Test2;
  if ch=p[-1] then goto Test1;
Loop0:
  if ch=p[0] then goto Test0;
AfterTest0:
  if ch=p[1] then goto TestT;
AfterTestT:
  p:=p+6;
  if p<pStop then goto Loop4;
  p:=p-4;
  if p<pStop then goto Loop0;
  Result:=0;
  goto Exit;

Test3: p:=p-2;
Test1: p:=p-2;
TestT: len:=lenSub;
  if lenSub<>0 then repeat;
    if (psub[len]<>p[len+1])
    or (psub[len+1]<>p[len+2]) then goto AfterTestT;
    len:=len+2;
    until len>=0;
  p:=p+2;
  if p<=pStop then goto Ret;
  Result:=0;
  goto Exit;

Test4: p:=p-2;
Test2: p:=p-2;
Test0: len:=lenSub;
  if lenSub<>0 then repeat;
    if (psub[len]<>p[len])
    or (psub[len+1]<>p[len+1]) then goto AfterTest0;
    len:=len+2;
    until len>=0;
  inc(p);
Ret:
  Result:=p-pStart;
Exit:
  end;

end.
