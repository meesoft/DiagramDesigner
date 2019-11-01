unit FastCode;

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

uses
  FastCodeCPUID,
  FastCodePatch,
  FastCodePos,
  FastCodeLowerCase,
  FastCodeUpperCase,
  FastCodeCompareText,
  FastCodeStrComp,
  FastCodeStrCopy,
  FastCodeFillChar,
  FastCodeCompareStr,
  FastcodeMoveUnit,
  FastCodeCompareMem;

implementation

uses
  Windows, SysUtils;

var
  FastCodePos         : FastCodePosFunction;
  FastCodeLowerCase   : FastCodeLowerCaseFunction;
  FastCodeUpperCase   : FastCodeUpperCaseFunction;
  FastCodeCompareText : FastCodeCompareTextFunction;
  FastCodeStrComp     : FastCodeStrCompFunction;
  FastCodeStrCopy     : FastCodeStrCopyFunction;
  FastcodeFillChar    : FastCodeFillCharFunction;
  FastcodeCompareStr  : FastCodeCompareStrFunction;
  FastcodeCompareMem  : FastCodeCompareMemFunction;
{  FastcodeAnsiStringReplace: function (const S, OldPattern, NewPattern: AnsiString;
    Flags: TReplaceFlags): AnsiString;}

initialization
{$IFDEF FastCodeCPUID}
  case FastCodeTarget of
           fctBlended: begin
                         FastCodePos         := FastCodePosBlended;
                         FastcodeLowerCase   := FastCodeLowerCaseBlended;
                         FastcodeUpperCase   := FastCodeUpperCaseBlended;
                         FastCodeCompareText := FastCodeCompareTextBlended;
                         FastCodeStrComp     := FastCodeStrCompBlended;
                         FastCodeStrCopy     := FastCodeStrCopyBlended;
                         FastcodeFillChar    := FastcodeFillCharBlended;
                         FastCodeCompareStr  := FastCodeCompareStrBlended;
                         FastCodeCompareMem  := FastCodeCompareMemBlended;
                       end;
                fctP3: begin
                         FastCodePos         := FastCodePosBlended;
                         FastCodeLowerCase   := FastCodeLowerCaseBlended;
                         FastCodeUpperCase   := FastCodeUpperCaseBlended;
                         FastCodeCompareText := FastCodeCompareTextBlended;
                         FastCodeStrComp     := FastCodeStrCompBlended;
                         FastCodeStrCopy     := FastCodeStrCopyBlended;
                         FastcodeFillChar    := FastcodeFillCharBlended;
                         FastCodeCompareStr  := FastCodeCompareStrBlended;
                         FastCodeCompareMem  := FastCodeCompareMemBlended;
                       end;
                fctPM: begin
                         FastCodePos         := FastCodePosPMD;
                         FastCodeLowerCase   := FastCodeLowerCasePMD;
                         FastCodeUpperCase   := FastCodeUpperCasePMD;
                         FastCodeCompareText := FastCodeCompareTextPMD;
                         FastCodeStrComp     := FastCodeStrCompPMD;
                         FastCodeStrCopy     := FastCodeStrCopyPMD;
                         FastcodeFillChar    := FastcodeFillCharPMD;
                         FastCodeCompareStr  := FastCodeCompareStrPMD;
                         FastCodeCompareMem  := FastCodeCompareMemPMD;
                       end;
                fctP4: begin
                         FastCodePos         := FastCodePosP4N;
                         FastCodeLowerCase   := FastCodeLowerCaseP4N;
                         FastCodeUpperCase   := FastCodeUpperCaseP4N;
                         FastCodeCompareText := FastCodeCompareTextP4N;
                         FastCodeStrComp     := FastCodeStrCompP4N;
                         FastCodeStrCopy     := FastCodeStrCopyP4N;
                         FastcodeFillChar    := FastcodeFillCharP4N;
                         FastCodeCompareStr  := FastCodeCompareStrP4N;
                         FastCodeCompareMem  := FastCodeCompareMemP4N;
                       end;
           fctP4_SSE3: begin
                         FastCodePos         := FastCodePosP4P;
                         FastCodeLowerCase   := FastCodeLowerCaseP4P;
                         FastCodeUpperCase   := FastCodeUpperCaseP4P;
                         FastCodeCompareText := FastCodeCompareTextP4P;
                         FastCodeStrComp     := FastCodeStrCompP4P;
                         FastCodeStrCopy     := FastCodeStrCopyP4P;
                         FastcodeFillChar    := FastcodeFillCharP4P;
                         FastCodeCompareStr  := FastCodeCompareStrP4P;
                         FastCodeCompareMem  := FastCodeCompareMemP4P;
                       end;
             fctP4_64: begin
                         FastCodePos         := FastCodePosP4P;
                         FastCodeLowerCase   := FastCodeLowerCaseP4P;
                         FastCodeUpperCase   := FastCodeUpperCaseP4P;
                         FastCodeCompareText := FastCodeCompareTextP4P;
                         FastCodeStrComp     := FastCodeStrCompP4P;
                         FastCodeStrCopy     := FastCodeStrCopyP4P;
                         FastcodeFillChar    := FastcodeFillCharP4P;
                         FastCodeCompareStr  := FastCodeCompareStrP4P;
                         FastCodeCompareMem  := FastCodeCompareMemP4P;
                       end;
                fctK7: begin
                         FastCodePos         := FastCodePosBlended;
                         FastCodeLowerCase   := FastCodeLowerCaseBlended;
                         FastCodeUpperCase   := FastCodeUpperCaseBlended;
                         FastCodeCompareText := FastCodeCompareTextBlended;
                         FastCodeStrComp     := FastCodeStrCompBlended;
                         FastCodeStrCopy     := FastCodeStrCopyBlended;
                         FastcodeFillChar    := FastcodeFillCharBlended;
                         FastCodeCompareStr  := FastCodeCompareStrBlended;
                         FastCodeCompareMem  := FastCodeCompareMemBlended;
                       end;
            fctK7_SSE: begin
                         FastCodePos         := FastCodePosXP;
                         FastCodeLowerCase   := FastCodeLowerCaseXP;
                         FastCodeUpperCase   := FastCodeUpperCaseXP;
                         FastCodeCompareText := FastCodeCompareTextXP;
                         FastCodeStrComp     := FastCodeStrCompXP;
                         FastCodeStrCopy     := FastCodeStrCopyXP;
                         FastcodeFillChar    := FastcodeFillCharXP;
                         FastCodeCompareStr  := FastCodeCompareStrXP;
                         FastCodeCompareMem  := FastCodeCompareMemXP;
                       end;
                fctK8: begin
                         FastCodePos         := FastCodePosAMD64;
                         FastCodeLowerCase   := FastCodeLowerCaseAMD64;
                         FastCodeUpperCase   := FastCodeUpperCaseAMD64;
                         FastCodeCompareText := FastCodeCompareTextAMD64;
                         FastCodeStrComp     := FastCodeStrCompAMD64;
                         FastCodeStrCopy     := FastCodeStrCopyAMD64;
                         FastcodeFillChar    := FastcodeFillCharAMD64;
                         FastCodeCompareStr  := FastCodeCompareStrAMD64;
                         FastCodeCompareMem  := FastCodeCompareMemAMD64;
                       end;
           fctK8_SSE3: begin
                         FastCodePos         := FastCodePosAMD64;
                         FastCodeLowerCase   := FastCodeLowerCaseAMD64;
                         FastCodeUpperCase   := FastCodeUpperCaseAMD64;
                         FastCodeCompareText :=  FastCodeCompareTextAMD64;
                         FastCodeStrComp     := FastCodeStrCompAMD64;
                         FastCodeStrCopy     := FastCodeStrCopyAMD64;
                         FastcodeFillChar    := FastcodeFillCharAMD64;
                         FastCodeCompareStr  := FastCodeCompareStrAMD64;
                         FastCodeCompareMem  := FastCodeCompareMemAMD64;
                       end;
                  else begin
                         FastCodePos         := FastCodePosRTL;
                         FastCodeLowerCase   := FastCodeLowerCaseRTL;
                         FastCodeUpperCase   := FastCodeUpperCaseRTL;
                         FastCodeCompareText := FastCodeCompareTextRTL;
                         FastCodeStrComp     := FastCodeStrCompRTL;
                         FastCodeStrCopy     := FastCodeStrCopyRTL;
                         FastcodeFillChar    := FastcodeFillCharRTL;
                         FastCodeCompareStr  := FastCodeCompareStrRTL;
                         FastCodeCompareMem  := FastCodeCompareMemRTL;
                       end;
  end;
{$ENDIF}

{$IFDEF FastCodeRTL}
  FastCodePos         := FastCodePosRTL;
  FastCodeLowerCase   := FastCodeLowerCaseRTL;
  FastCodeUpperCase   := FastCodeUpperCaseRTL;
  FastCodeCompareText := FastCodeCompareTextRTL;
  FastCodeStrComp     := FastCodeStrCompRTL;
  FastCodeStrCopy     := FastCodeStrCopyRTL;
  FastcodeFillChar    := FastcodeFillCharRTL;
  FastCodeCompareStr  := FastCodeCompareStrRTL;
  FastCodeCompareMem  := FastCodeCompareMemRTL;
{$ENDIF}

{$IFDEF FastCodeBlended}
  FastCodePos         := FastCodePosBlended;
  FastCodeLowerCase   := FastCodeLowerCaseBlended;
  FastCodeUpperCase   := FastCodeUpperCaseBlended;
  FastCodeCompareText := FastCodeCompareTextBlended;
  FastCodeStrComp     := FastCodeStrCompBlended;
  FastCodeStrCopy     := FastCodeStrCopyBlended;
  FastcodeFillChar    := FastcodeFillCharBlended;
  FastCodeCompareStr  := FastCodeCompareStrBlended;
  FastCodeCompareMem  := FastCodeCompareMemBlended;
{$ENDIF}

{$IFDEF FastCodePascal}
  FastCodePos         := FastCodePosPascal;
  FastCodeLowerCase   := FastCodeLowerCasePascal;
  FastCodeUpperCase   := FastCodeUpperCasePascal;
  FastCodeCompareText := FastCodeCompareTextPascal;
  FastCodeStrComp     := FastCodeStrCompPascal;
  FastCodeStrCopy     := FastCodeStrCopyPascal;
  FastcodeFillChar    := FastcodeFillCharPascal;
  FastCodeCompareStr  := FastCodeCompareStrPascal;
  FastCodeCompareMem  := FastCodeCompareMemPascal;
{$ENDIF}

  FastCodeAddressPatch(FastCodeGetAddress(@PosStub)        , @FastCodePos);
  FastCodeAddressPatch(FastCodeGetAddress(@LowerCaseStub)  , @FastCodeLowerCase);
  FastCodeAddressPatch(FastCodeGetAddress(@UpperCaseStub)  , @FastCodeUpperCase);
  FastCodeAddressPatch(FastCodeGetAddress(@CompareTextStub), @FastCodeCompareText);
  FastCodeAddressPatch(FastCodeGetAddress(@StrCompStub)    , @FastCodeStrComp);
  FastCodeAddressPatch(FastCodeGetAddress(@StrCopyStub)    , @FastCodeStrCopy);
  FastCodeAddressPatch(FastCodeGetAddress(@FillCharStub)   , @FastCodeFillChar);
  FastCodeAddressPatch(FastCodeGetAddress(@CompareStrStub) , @FastCodeCompareStr);
  FastCodeAddressPatch(FastCodeGetAddress(@CompareMemStub) , @FastCodeCompareMem);
                   
  if isMMX in CPU.InstructionSupport then FastCodeAddressPatch(@Move,@MoveFastcodeBlended);
end.

