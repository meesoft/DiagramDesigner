// FastCode RTL Replacement definitions //

{.$DEFINE FastcodeRTL}
{.$DEFINE FastCodeBlended}
{$DEFINE FastCodeCPUID}
{.$DEFINE FastCodePascal}

{Default to FastcodeRTL if No Fastcode Compile Option Selected}
{$IFNDEF FastCodeBlended}
  {$IFNDEF FastCodePascal}
    {$IFNDEF FastCodeCPUID}
      {$DEFINE FastcodeRTL}        
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{Ensure Only One Fastcode Compile Option (Best) is Selected}
{$IFDEF FastCodeCPUID}
  {$UNDEF FastCodeBlended}
  {$UNDEF FastcodeRTL}
  {$UNDEF FastCodePascal}
{$ENDIF}
{$IFDEF FastCodeBlended}
  {$UNDEF FastcodeRTL}
  {$UNDEF FastCodePascal}
{$ENDIF}
{$IFDEF FastcodeRTL}
  {$UNDEF FastCodePascal}
{$ENDIF}

// Delphi Version definitions //

{$IFDEF VER120}
  {$DEFINE Delphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi9}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi9}
  {$DEFINE Delphi10}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$D-}

