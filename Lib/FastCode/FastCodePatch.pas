unit FastCodePatch;

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
 *
 * ***** END LICENSE BLOCK ***** *)

interface

function FastCodeGetAddress(AStub: Pointer): Pointer;
procedure FastCodeAddressPatch(const ASource, ADestination: Pointer);
procedure FastCodeReplaceVirtualMethod(OldClass,NewClass: TClass; MethodVMTOffset : Integer);

implementation

uses
  Windows;

type
  PJump = ^TJump;
  TJump = packed record
    OpCode: Byte;
    Distance: Pointer;
  end;

function FastCodeGetAddress(AStub: Pointer): Pointer;
begin
  if PBYTE(AStub)^ = $E8 then
  begin
    Inc(Integer(AStub));
    Result := Pointer(Integer(AStub) + SizeOf(Pointer) + PInteger(AStub)^);
  end
  else
    Result := nil;
end;

procedure FastCodeAddressPatch(const ASource, ADestination: Pointer);
var
  NewJump: PJump;
  OldProtect: Cardinal;
begin
  if VirtualProtect(ASource, SizeOf(TJump), PAGE_EXECUTE_READWRITE, OldProtect) then
  try
    NewJump := PJump(ASource);
    NewJump.OpCode := $E9;
    NewJump.Distance := Pointer(Integer(ADestination) - Integer(ASource) - 5);
  finally
    FlushInstructionCache(GetCurrentProcess, ASource, SizeOf(TJump));
  end;
end;

procedure FastCodeReplaceVirtualMethod(OldClass,NewClass: TClass; MethodVMTOffset : Integer);
var
  OldProtect : Cardinal;
begin
  if VirtualProtect(PInteger(Integer(OldClass)+MethodVMTOffset), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
  try
    PInteger(Integer(OldClass)+MethodVMTOffset)^:=PInteger(Integer(NewClass)+MethodVMTOffset)^;
  finally
    FlushInstructionCache(GetCurrentProcess, PInteger(Integer(OldClass)+MethodVMTOffset), 4);
  end;
end;

end.
