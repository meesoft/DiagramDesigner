////////////////////////////////////////////////////////////////////////////////
// TSynSpellCheck 2.00
//
// Copyright (c) 2002 Jacob Dybala a.k.a. "m3Rlin". All rights reserved.
//
// E-Mail: jacobdybala@synspellcheck.prv.pl
// WWW   : http://www.synspellcheck.prv.pl/ SynSpellCheck Home
//         http://www.delphifaq.net/        Merlin's Delphi Forge
//
// Elf hash algorithm
//   Copyright (c) 1998-2002 Scalabium
//   <http://www.scalabium.com/faq/dct0136.htm>
// SoundEx algorithm
//   Copyright (c) 1995-2001 Borland Software Corporation
// Metaphone Phonetic Hash Algorithm
//   Copyright (c) Tom White <wcstom@yahoo.com>
// Word differences algorithm JHCMP...
//   Copyright (c) Josef Hampl
//
// Created : Jan-10-2002
// Modified: Aug-31-2002
////////////////////////////////////////////////////////////////////////////////
// All dictionaries are located in the 'Program Files\Common\SynSpell' folder.
// This is to limit the number of copies of the same dictionary on a single
// computer to one file.
//
// Dictionaries are flat text files with a single word in each line. All words
// MUST be lowercase. The dictionaries are case insensitive.
//
// Available dictionaries:
// * czech.1-0-0.dic     - 21,177  words [197 Kb]
// * danish.1-0-0.dic    - 339,207 words [4.31 Mb]
// * dutch.1-0-0.dic     - 176,800 words [2.03 Mb]
// * english.1-1-4.dic   - 73,403  words [762 Kb]
// * german.1-0-0.dic    - 88,566  words [1.16 Mb]
// * italian.1-0-0.dic   - 60,453  words [607 Kb]
// * japanese.1-0-0.dic  - 115,524 words [1 Mb]
// * latin.1-0-0.dic     - 77,107  words [905 Kb]
// * norwegian.1-0-0.dic - 61,847  words [636 Kb]
// * polish.1-0-3.dic    - 3,925   words [36 Kb]
// * russian.1-0-0.dic   - 39,412  words [407 Kb]
// * spanish.1-0-0.dic   - 59,167  words [554 Kb]
// * turkish.1-0-0.dic   - 26,123  words [237 Kb]
////////////////////////////////////////////////////////////////////////////////
// Changes:
//
// 2.00 (Michael Vinther)
//   - Removed SynEdit connection
// 1.30 (Contributed in large by Jan Fiala)
//   * Many, many minor adjustments, optimizations.
//   * Rewritten SetApostrophes().
//   + New word suggestion algorithm: haDiff. Finds words based on differences.
//     haSoundex and haMetaphone *may* be removed in upcoming versions.
//   + New action added: ACTION_UNDO.
//   + New function: GetDictionaryDir(). This allows users to specify their own
//     paths.
//   + Dutch (compiled by Arno Verhoeven) dictionary added.
//
// 1.24 Released privately to certain users.
//   * Bug Fix: PChar and string incompatiblity. Fixed.
//
// 1.23 Released privately to certain users.
//   * Minor code adjustments.
//   + New dictionaries: Norwegian and Spanish.
//
// 1.22
//   * Bug Fix: The Apostrophes property did not allow changing. Fixed.
//     Submitted by R.K. Wittye.
//   * Bug Fix: ClearDictWords did not properly dispose of words creating a
//     rather large memory leak. Fixed. Submitted by Ascher Stefan.
//   * English and Polish dictionaries updated.
//   + Added Value field to TWordRec record. Each word is assigned an Elf value
//     and is checked by it. Major speed optimization. Suggested by Jan Fiala
//     (CRC32).
//
// 1.21
//   * Bug Fix: %ProgramFilesDir%\Common Files was read instead of %CommonFilesDir%.
//     This created problems on non-English versions of Windows. The directory
//     was not found. Fixed.
//   * English and Polish dictionaries updated.
//
// 1.20
//   * FindWord() routine rewritten to make use of cache array. Other functions
//     have only been slightly modified yet no functions have been broken.
//   * LoadDictionary() routine now converts all words to lowercase.
//   * LoadSkipList() does not add the words one-by-one any more. They are
//     assigned in whole.
//   * FSkipList is now cleared when a dictionary is closed.
//   * SaveSkipList() now removes all empty lines before saving to file.
//   + Added cache array to speed up word checks.
//   + ENoDictionaryLoaded is now thrown when no dictionary has been loaded.
//
// 1.19
//   * Bug Fix: Word underlining would also draw on gutter when the word was
//     partially scrolled under it. Fixed.
//   * SoundexLength property converted to HashLength.
//   * PaintUnderLine() code modified to directly color pixels instead of drawing
//     lines.
//   * Dictionary updates: English (1.1.2), Polish (1.1.1). The Polish word list
//     has been *significantly* reduced due to the fact that this word list is
//     being started all over to include words with non-latin characters.
//   + New option: sscoTrimApostrophes.
//   + New properties: Busy and UnderlineStyle (to mimic Corel Wordperfect
//     auto spell checking).
//   + MetaPhone algorithm has been finally implemented. In beta stage (works,
//     but slow on big lists).
//   + AddDictSkipList(), AddDictWordList() routines added.
//   + New dictionaries: German (by Ascher Stefan) and Russian.
//
// 1.18
//   * Bug Fix: OnSkipWord event did not return proper ASkipAll value. Fixed.
//   * Bug Fix: GetDictionaryList() included all copies of dictionaries for a
//     specific language instead of newest. Fixed.
//   * DupeString() has been corrected with proper compiler conditional.
//   * Minor code changes to always pass lowercase words to FindWord().
//   * English dictionary updated to version 1.1.0.
//   * Updated component demo.
//   + New option: sscoMaintainCase. Idea suggested by Jeff Rafter.
//   + New event: OnAddWord.
//   + Added support for words with apostrophes. Idea by Stefan van As.
//   + GetDictionaryList() now returns a sorted list.
//
// 1.17
//   * SelectWordAtCursor() made public.
//   + Added support for localized languages and numbers.
//
// 1.16
//   * Bug Fix: Compiler conditional around SoundEx() routines was broken.
//     Fixed.
//   * Bug Fix: sscoSelectWord did not work when set to False. Fixed.
//   + SelectWordAtCursor() routine added. Contributed by Stefan van As.
//
// 1.15
//   * Bug Fix: PenColor property did not work. Fixed by Jeff Corbets.
//   * Bug Fix: OnAbort event was not called when spell checking was aborted.
//     Fixed.
//   * TSoundEx class has been removed in favor of Borland implementation of
//     SoundEx() function.
//   * Minor code modifications.
//   + Added support for dashed words.
//   + New option: sscoGoUp.
//   + New property: SoundExLength.
//
// 1.14
//   * Bug Fix: If the editor had no text and sscoHourGlass was set the cursor
//     did not revert to it's previous value. Fixed by Jeff Corbets.
//
// 1.13
//   * Bug Fix: When empty lines in base dictionary and user dictionary were
//     added to word list and raised AV when attempting to calculate word hash.
//     Fixed.
//
// 1.12
//   * Bug Fix: GetSuggestions did not properly support words with uppercase
//     characters. Fixed. Found by Jeff Rafter.
//   + Added Metaphon algorithm for word hashes. Not working, just skeleton for
//     now.
//
// 1.11
//   + Added support for multiple editors: AddEditor() and RemoveEditor().
//
// 1.10 (code contributed by Ricardo Cardona)
//   * Bug Fix: When not highlighter was selected and sscoAutoSpellCheck was set
//     in Options the component generated an AV. Fixed.
//   * New property: CheckAttribs.
//   * Improved code for underlining unknown words.
//
// 1.09
//   * Bug Fix: FWordList did not free memory when the component was destroyed.
//     It just cleared the word and hash lists. Fixed.
//
// 1.08
//   * Bug Fix: FindWord() function was case sensitive. Fix contributed by
//     Gerald Nunn.
//   + New events: OnDictClose and OnDictLoad.
//   + New options: sscoAutoSpellCheck (contributed by Ricardo Cardona),
//     sscoIgnoreWordsWithNumbers and sscoIgnoreSingleChars.
//   + New property: PenColor.
//   + Added support for Java documentation.
//
// 1.07
//   * Bug Fix: When spell checking the last word under certain conditions the
//     component would enter an infinite loop. Fixed.
//
// 1.06
//   * Bug Fix: When correcting words in OnCheckWord event the word would not be
//     replaced but added to the beginning of the old one. Fixed.
//   + New dictionary: Danish.
//   + New property: OpenDictionary.
//   + New option: sscoSelectWord.
//
// 1.05
//   + New events: OnCorrectWord and OnSkipWord.
//   + Demo added.
//
// 1.04
//   * Bug Fix: Would not compile under Delphi 6 due to duplicate resource
//     error. Fixed.
//   * GetDictionaryList() now searches for file that match the correct naming
//     scheme - name.major-minor-revision.dic, where major, minor and revision
//     are single characters.
//   + New dictionaries: Italian, Latin, Japanese, Polish, Spanish (Thanks to
//     Ricardo Cardona), and Turkish.
//   + New routines: CloseDictionary(), GetWordCount().
//   + New property: Dictionary.
//   - Removed {$IFDEF SYN_WIN32} directive from GetDictionaryList(). The
//     routines are available under Kylix also.
//   - Removed Version parameter from LoadDictionary.
//
// 1.03
//   + Added /usr/local/SynSpell dir under Linux as the default dictionary
//     directory.
//   + Added Language property.
//   + %ProgramFiles%\Common Files\SynSpell is now dynamically read from system
//     Registry.
//   + Added user dictionary.
//   + Added GetDictionaryList().
//
// 1.02
//   * Bug Fix: When the word list was cleared, the SoundEx list still hogged up
//     the memory =) Fixed.
//   * Bug Fix: When a word was deleted from the dictionary, the SoundEx hash
//     remained undeleted. Therefor, after deleting a word the whole SoundEx
//     hash list after the deleted word was wrong (1 up).
//   * Bug Fix: Suggestions were not passed in ASuggestions in OnCheckWord
//     event. Fixed.
//   * Bug Fix: DeleteSkipWord() fixed to delete form skip word list, not word
//     list ;-)
//   * Bug Fix: editor did not update when searching for words, the screen
//     would "blur". Fixed.
//   * GetSuggestions() changed from procedure to function to return number or
//     words in list.
//   * FWordList is now type of TList instead of TStringList.
//   + If no AAction is specified in the OnCheckWord event, then ACTION_SKIP is
//     default.
//   + Now double words are automatically ignored in FWordList.
//   + Added sscoSuggestWords option.
//   + Added OnAbort event.
//   + Added support for HTML Text.
//   - Removed unsupported options from Options property.
//
// 1.01
//   + Added Options property (support for selecting unknown words in editor,
//     spell checking from cursor, and spell checking only selection, hour glass
//     cursor during spell check, removing cursor visibility during spell
//     check).
//   + Added word suggestion list.
////////////////////////////////////////////////////////////////////////////////

unit SynSpellCheck;

interface

{$DEFINE SYN_WIN32}
{$DEFINE SYN_DELPHI_4_UP}

uses
  Classes,
  Graphics,
  Windows,
  Controls,
  Forms,
  Registry,
  Math,
  SysUtils,
  StrUtils;

type
  THashLength    = 1..16;
  TSoundExLength = 1..8;

  TLanguageRec = record
    Name,
    Version: string;
  end;

  PWordRec = ^TWordRec;
  TWordRec = record
    Word,
    Hash : string;
    Value: Integer;
    User : Boolean;
  end;

  TSynSpellCheck  = class;
  TUnderlineStyle = (usCorelWordPerfect, usMicrosoftWord);

  { Procedure types }
  TOnAddWord = procedure(Sender: TObject; AWord: string) of object;
  TOnCheckWord = procedure(Sender: TObject; AWord: string;
    ASuggestions: TStringList; var ACorrectWord: string; var AAction: Integer;
    const AUndoEnabled: Boolean = True) of object;
  TOnCorrectWord = procedure(Sender: TObject; AWord, ACorrectWord: string)
    of object;
  TOnSkipWord = procedure(Sender: TObject; AWord: string; ASkipAll: Boolean)
    of object;

  { Sets }
  HashAlgorithms       = (haSoundEx, haDiff);

  TSynSpellCheck = class(TComponent)
  private
    FAnsi2Ascii        : array[128..255] of Char;
    FCacheArray        : array[0..255] of array [0..1] of Cardinal;
    FModified,
    FOpenDictionary,
    FUseUserDictionary : Boolean;
    FHashAlgorithm     : HashAlgorithms;
    FMaxWordLength     : Integer;
    FDictionary,
    FDictPath,
    FUserFileName,
    FUserDictPath      : string;
    FHashLength        : THashLength;
    FOnAddWord         : TOnAddWord;
    FLanguage          : TLanguageRec;
    FEditors,
    FPlugins,
    FWordList          : TList;
    FOnAbort,
    FOnDictClose,
    FOnDictLoad,
    FOnDone,
    FOnStart           : TNotifyEvent;
    FOnCheckWord       : TOnCheckWord;
    FOnCorrectWord     : TOnCorrectWord;
    FOnSkipWord        : TOnSkipWord;
    FSkipList          : TStringList;
    { Functions }
    function FindWord(sWord: string): Integer;
    function GetDefaultDictionaryDir: string;
    function GetDictionaryDir: string;
    function GetUserDictionaryDir: string;    
    { Procedures }
    procedure CalculateCacheArray;
    procedure InitializeAnsi2Ascii;                                             //Fiala
    procedure SetSkipList(Value: TStringList);
    procedure SortWordList;
    procedure SetHashAlgorithm(const Value: HashAlgorithms);
    procedure SetHashLength(const Value: THashLength);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Functions }
    function Ansi2Ascii(const sString: string): string;
    function DictionaryExists(Language: string; Path: string = ''): Boolean;
    function GetNewestDictionary(Language: string): string;
    function GetSuggestions(const Word: string; SuggestionList: TStrings): Integer;
    function GetWordCount: Integer;
    function GetWordFromASCIIWord(sWord: string): string;
    function IsDictWord(Word: string): Boolean;
    function IsSkipWord(Word: string): Boolean;
    { Procedures }
    procedure AddDictWord(Word: string);
    procedure AddDictWordList(WordList: TStringList);
    procedure AddSkipWord(Word: string);
    procedure AddSkipWordList(WordList: TStringList);
    procedure ClearDictWords;
    procedure ClearSkipWords;
    procedure CloseDictionary;
    procedure DeleteDictWord(Word: string);
    procedure DeleteSkipWord(Word: string);
    procedure FixLists;
    procedure GetDictionaryList(tslList: TStrings);
    procedure LoadDictionary(Language: string; FileName: string = '');
    procedure LoadSkipList(FileName: string);
    procedure SaveSkipList(FileName: string);
    procedure SaveUserDictionary;
  published
    { Properties }
    property Algorithm: HashAlgorithms read FHashAlgorithm write SetHashAlgorithm default haDiff;
    property Dictionary: string read FDictionary;
    property DictionaryPath: string read GetDictionaryDir write FDictPath;
    property HashLength: THashLength read FHashLength write SetHashLength
      default 4;
    property Language: TLanguageRec read FLanguage;
    property Modified: Boolean read FModified write FModified default False;
    property OpenDictionary: Boolean read FOpenDictionary;
    property SkipList: TStringList read FSkipList write SetSkipList;
    property UserDirectory: string read GetUserDictionaryDir write FUserDictPath;
    property UseUserDictionary: Boolean read FUseUserDictionary write FUseUserDictionary default True;
    { Events }
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnAddWord: TOnAddWord read FOnAddWord write FOnAddWord;
    property OnCheckWord: TOnCheckWord read FOnCheckWord write FOnCheckWord;
    property OnCorrectWord: TOnCorrectWord read FOnCorrectWord
      write FOnCorrectWord;
    property OnDictClose: TNotifyEvent read FOnDictClose write FOnDictClose;
    property OnDictLoad: TNotifyEvent read FOnDictLoad write FOnDictLoad;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property OnSkipWord: TOnSkipWord read FOnSkipWord write FOnSkipWord;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

  ENoDictionaryLoaded = class(EExternal);
  resourcestring SNoDictionaryLoaded = 'No dictionary is loaded.';

function ElfHash(const Value: string): Integer;
function TrimEx(const sWord: string; const chChar: Char): string;

const
  //////////////////////////////////////////////////////////////////////////////
  // Action constants
  //////////////////////////////////////////////////////////////////////////////
  ACTION_ABORT   = -1;
  ACTION_SKIP    = 0;
  ACTION_SKIPALL = 1;
  ACTION_CORRECT = 2;
  ACTION_ADD     = 3;
  ACTION_UNDO    = -2;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SynEdit', [TSynSpellCheck]);
end;

function ContainsNumbers(sWord: string): Boolean;
var
  iI: Integer;
begin
  Result := False;
  for iI := 1 to Length(sWord) do
    if sWord[iI] in ['1'..'9', '0'] then begin
      Result := True;
      Break;
    end;
end;

function ElfHash(const Value: string): Integer;
var
  iI, iJ: Integer;
begin
  Result := 0;
  for iI := 1 to Length(Value) do begin
    Result := (Result shl 4) + Ord(Value[iI]);
    iJ := Result and $F0000000;
    if (iJ <> 0) then
      Result := Result xor (iJ shr 24);
    Result := Result and (not iJ);
  end;
end;

{ TSynSpellCheck }

function TSynSpellCheck.GetDefaultDictionaryDir: string;
begin
  Result := 'C:\Program Files\Common Files\SynSpell\';
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion') then begin
        if ValueExists('CommonFilesDir') then
          Result := ReadString('CommonFilesDir') + '\SynSpell\';
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function TSynSpellCheck.GetDictionaryDir: string;
begin
  if FDictPath <> '' then
    Result := IncludeTrailingBackslash(FDictPath)
  else
    Result := IncludeTrailingBackslash(GetDefaultDictionaryDir);
end;

function TSynSpellCheck.GetUserDictionaryDir;
begin
{$ifdef SYN_WIN32}
  if FUserDictPath <> '' then
    Result := IncludeTrailingBackslash(FUserDictPath)
  else
    Result := GetDictionaryDir;
{$else}
  if FUserDictPath <> '' then
    Result := FUserDictPath
  else
    Result := Result := '/usr/local/SynSpell/';
{$endif}
end;

function IsNumber(const PWord: PChar): Boolean;
var
  iI: Integer;
begin
  Result := True;
  for iI := 1 to StrLen(PWord) do
    if not ((PWord + iI)[1] in ['0'..'9']) then begin
      Result := False;
      Break;
    end;
end;

function SortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TWordRec(Item1^).Word, TWordRec(Item2^).Word);
end;

function TrimEx(const sWord: string; const chChar: Char): string;
var
  iI, iLength: Integer;
begin
  iLength := Length(sWord);
  iI := 1;
  while (iI <= iLength) and (sWord[iI] <= chChar) do
    Inc(iI);
  if iI > iLength then
    Result := ''
  else begin
    while sWord[iLength] = chChar do
      Dec(iLength);
    Result := Copy(sWord, iI, iLength - iI + 1);
  end;
end;

{ TSynSpellCheck }

constructor TSynSpellCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModified := False;
  FHashAlgorithm  := haDiff;
  FHashLength     := 4;
  FMaxWordLength  := 0;
  FUseUserDictionary := True;
  //////////////////////////////////////////////////////////////////////////////
  // Lists
  //////////////////////////////////////////////////////////////////////////////
  FEditors  := TList.Create;
  FPlugins  := TList.Create;
  FWordList := TList.Create;
  FSkipList := TStringList.Create;
  FSkipList.Duplicates := dupIgnore;
end;

destructor TSynSpellCheck.Destroy;
begin
  CloseDictionary;
  //////////////////////////////////////////////////////////////////////////////
  // Free used memory
  //////////////////////////////////////////////////////////////////////////////
  FEditors.Free;
  FPlugins.Free;
  FSkipList.Free;
  FWordList.Free;
  inherited;
end;

function TSynSpellCheck.Ansi2Ascii(const sString: string): string;
var
  iI: Integer;
begin
  if FAnsi2Ascii[128]=#0 then InitializeAnsi2Ascii;
  Result := sString;
  for iI := 1 to Length(sString) do
    if Result[iI] > #127 then
      Result[iI] := FAnsi2Ascii[Ord(Result[iI])];
end;

function TSynSpellCheck.DictionaryExists(Language: string;
{$IFDEF SYN_DELPHI_4_UP}
  Path: string = ''
{$ELSE}
  Path: string
{$ENDIF}
): Boolean;
var
  sTemp : string;
  srTemp: TSearchRec;
begin
  if Trim(Path) = '' then
    sTemp := GetDictionaryDir // Search in shared dictionary directory
  else
    sTemp := Path;            // Search in user specified directory
  Result := (FindFirst(sTemp + Language + '.?-?-?.dic', faAnyFile, srTemp) = 0);
end;

function TSynSpellCheck.GetNewestDictionary(Language: string): string;
var
  srDict : TSearchRec;
  tslTemp: TStringList;
begin
  tslTemp := TStringList.Create;
  if FindFirst(GetDictionaryDir + Language + '.?-?-?.dic', faAnyFile,
    srDict) = 0 then begin
    if Pos('.user.', srDict.Name) = 0 then
      tslTemp.Add(AnsiLowerCase(srDict.Name));
    while FindNext(srDict) = 0 do begin
      if Pos('.user.', srDict.Name) = 0 then
        tslTemp.Add(AnsiLowerCase(srDict.Name));
    end;
  end;
  with tslTemp do begin
    if Count > 0 then begin
      Sort;
      Result := Strings[Count - 1];
    end;
    Free;
  end;
  SysUtils.FindClose(srDict);
end;

function TSynSpellCheck.GetWordCount: Integer;
begin
  Result := FWordList.Count;
end;

// Returns word from word without diacritic
function TSynSpellCheck.GetWordFromASCIIWord(sWord: string): string;
var
  iI, iJ, iLength: Integer;
  sLower, sTemp  : string;

  function CorrectCase(const AsWord: string; const Word: string): string;
  var
    s1, s2, s3, s4: string;
    iX: Integer;
  begin
    s1 := AnsiUpperCase(AsWord);
    s2 := AnsiLowerCase(AsWord);
    s3 := AnsiUpperCase(Word);
    s4 := AnsiLowerCase(Word);
    Result := Word;
    for iX := 1 to Length(Word) do begin
      if s1[iX] = AsWord[iX] then
        Result[iX] := s3[iX]
      else if s2[iX] = AsWord[iX] then
        Result[iX] := s4[iX];
    end;
  end;

begin
  if FAnsi2Ascii[128]=#0 then InitializeAnsi2Ascii;

  // Are there any words at all starting with this letter?
  sLower := AnsiLowerCase(sWord);
  if FCacheArray[Ord(sLower[1])][1] = 0 then
    Exit;
  if FindWord(sLower) <> -1 then
    Exit;
  iLength := Length(sLower);
  for iI := FCacheArray[Ord(sLower[1])][0] to FCacheArray[Ord(sLower[1])][1] do begin
    sTemp := PWordRec(FWordList.Items[iI])^.Word;
    if iLength = Length(sTemp) then begin
      // Remove diacritic in dictionary and try find word
      if Ansi2Ascii(sTemp) = sLower then begin
        Result := CorrectCase(sWord, sTemp);
        Exit;
      end;
    end;
  end;

  // Not found in base, first char has diacritic, we must continue search
  for iI := 128 to 254 do begin
    // Some optimalization
    if FAnsi2Ascii[iI] = sLower[1] then
      for iJ := FCacheArray[iI][0] to FCacheArray[iI][1] do begin
        sTemp := PWordRec(FWordList.Items[iJ])^.Word;
        if iLength = Length(sTemp) then
          // Remove diacritic in dictionary and try find word
          if Ansi2Ascii(sTemp) = sLower then begin
            Result := CorrectCase(sWord, sTemp);
            Exit;
          end;
      end;
   end;
end;

procedure TSynSpellCheck.InitializeAnsi2Ascii;
begin
  FAnsi2Ascii[128] := #128;
  FAnsi2Ascii[129] := #129;
  FAnsi2Ascii[130] := #130;
  FAnsi2Ascii[131] := #131;
  FAnsi2Ascii[132] := #132;
  FAnsi2Ascii[133] := #133;
  FAnsi2Ascii[134] := #134;
  FAnsi2Ascii[135] := #135;
  FAnsi2Ascii[136] := #136;
  FAnsi2Ascii[137] := #137;
  FAnsi2Ascii[138] := #083;
  FAnsi2Ascii[139] := #139;
  FAnsi2Ascii[140] := #083;
  FAnsi2Ascii[141] := #084;
  FAnsi2Ascii[142] := #090;
  FAnsi2Ascii[143] := #090;
  FAnsi2Ascii[144] := #144;
  FAnsi2Ascii[145] := #145;
  FAnsi2Ascii[146] := #146;
  FAnsi2Ascii[147] := #147;
  FAnsi2Ascii[148] := #148;
  FAnsi2Ascii[149] := #149;
  FAnsi2Ascii[150] := #150;
  FAnsi2Ascii[151] := #151;
  FAnsi2Ascii[152] := #152;
  FAnsi2Ascii[153] := #153;
  FAnsi2Ascii[154] := #115;
  FAnsi2Ascii[155] := #155;
  FAnsi2Ascii[156] := #115;
  FAnsi2Ascii[157] := #116;
  FAnsi2Ascii[158] := #122;
  FAnsi2Ascii[159] := #122;
  FAnsi2Ascii[160] := #32;
  FAnsi2Ascii[161] := #161;
  FAnsi2Ascii[162] := #162;
  FAnsi2Ascii[163] := #076;
  FAnsi2Ascii[164] := #164;
  FAnsi2Ascii[165] := #065;
  FAnsi2Ascii[166] := #166;
  FAnsi2Ascii[167] := #167;
  FAnsi2Ascii[168] := #168;
  FAnsi2Ascii[169] := #169;
  FAnsi2Ascii[170] := #083;
  FAnsi2Ascii[171] := #171;
  FAnsi2Ascii[172] := #172;
  FAnsi2Ascii[173] := #173;
  FAnsi2Ascii[174] := #174;
  FAnsi2Ascii[175] := #090;
  FAnsi2Ascii[176] := #176;
  FAnsi2Ascii[177] := #177;
  FAnsi2Ascii[178] := #178;
  FAnsi2Ascii[179] := #179;
  FAnsi2Ascii[180] := #180;
  FAnsi2Ascii[181] := #181;
  FAnsi2Ascii[182] := #182;
  FAnsi2Ascii[183] := #183;
  FAnsi2Ascii[184] := #184;
  FAnsi2Ascii[185] := #097;
  FAnsi2Ascii[186] := #115;
  FAnsi2Ascii[187] := #187;
  FAnsi2Ascii[188] := #076;
  FAnsi2Ascii[189] := #189;
  FAnsi2Ascii[190] := #108;
  FAnsi2Ascii[191] := #122;
  FAnsi2Ascii[192] := #082;
  FAnsi2Ascii[193] := #065;
  FAnsi2Ascii[194] := #065;
  FAnsi2Ascii[195] := #065;
  FAnsi2Ascii[196] := #065;
  FAnsi2Ascii[197] := #076;
  FAnsi2Ascii[198] := #067;
  FAnsi2Ascii[199] := #067;
  FAnsi2Ascii[200] := #067;
  FAnsi2Ascii[201] := #069;
  FAnsi2Ascii[202] := #069;
  FAnsi2Ascii[203] := #069;
  FAnsi2Ascii[204] := #069;
  FAnsi2Ascii[205] := #073;
  FAnsi2Ascii[206] := #073;
  FAnsi2Ascii[207] := #068;
  FAnsi2Ascii[208] := #068;
  FAnsi2Ascii[209] := #078;
  FAnsi2Ascii[210] := #078;
  FAnsi2Ascii[211] := #079;
  FAnsi2Ascii[212] := #079;
  FAnsi2Ascii[213] := #079;
  FAnsi2Ascii[214] := #079;
  FAnsi2Ascii[215] := #215;
  FAnsi2Ascii[216] := #082;
  FAnsi2Ascii[217] := #085;
  FAnsi2Ascii[218] := #085;
  FAnsi2Ascii[219] := #085;
  FAnsi2Ascii[220] := #085;
  FAnsi2Ascii[221] := #089;
  FAnsi2Ascii[222] := #084;
  FAnsi2Ascii[223] := #223;
  FAnsi2Ascii[224] := #114;
  FAnsi2Ascii[225] := #097;
  FAnsi2Ascii[226] := #097;
  FAnsi2Ascii[227] := #097;
  FAnsi2Ascii[228] := #097;
  FAnsi2Ascii[229] := #108;
  FAnsi2Ascii[230] := #099;
  FAnsi2Ascii[231] := #099;
  FAnsi2Ascii[232] := #099;
  FAnsi2Ascii[233] := #101;
  FAnsi2Ascii[234] := #101;
  FAnsi2Ascii[235] := #101;
  FAnsi2Ascii[236] := #101;
  FAnsi2Ascii[237] := #105;
  FAnsi2Ascii[238] := #105;
  FAnsi2Ascii[239] := #100;
  FAnsi2Ascii[240] := #100;
  FAnsi2Ascii[241] := #110;
  FAnsi2Ascii[242] := #110;
  FAnsi2Ascii[243] := #111;
  FAnsi2Ascii[244] := #111;
  FAnsi2Ascii[245] := #111;
  FAnsi2Ascii[246] := #111;
  FAnsi2Ascii[247] := #247;
  FAnsi2Ascii[248] := #114;
  FAnsi2Ascii[249] := #117;
  FAnsi2Ascii[250] := #117;
  FAnsi2Ascii[251] := #117;
  FAnsi2Ascii[252] := #117;
  FAnsi2Ascii[253] := #121;
  FAnsi2Ascii[254] := #116;
  FAnsi2Ascii[255] := #255;
end;


procedure TSynSpellCheck.AddDictWord(Word: string);
var
  AWordItem: PWordRec;

  { Return list position for insert new word }
  function GetInsertPos(const Word: string): Integer;
  var
    iI: Integer;
  begin
    Result := -1;
    // If not any words at all starting with this letter, we find next word
    if FCacheArray[Ord(Word[1])][1] = 0 then begin
      for iI := Ord(Word[1]) + 1 to 255 do
        if FCacheArray[iI][1] <> 0 then begin
          Result := FCacheArray[iI][0];
          Break;
        end;
    end else
      // Words with this letter exists, we find right pos
      for iI := FCacheArray[Ord(Word[1])][0] to Succ(FCacheArray[Ord(Word[1])][1]) do
        if PWordRec(FWordList.Items[iI])^.Word > Word then begin
          Result := iI;
          Break;
        end;
  end;

begin
  if Trim(Word) = '' then Exit;
  Word := AnsiLowerCase(Word);
  if FindWord(Word) = -1 then
  begin
    New(AWordItem);
    FMaxWordLength := Max(FMaxWordLength, Length(Word));
    AWordItem^.Word  := Word;
    AWordItem^.User  := True;
    AWordItem^.Value := ElfHash(Word);
    if FHashAlgorithm = haSoundEx then
      AWordItem^.Hash := SoundEx(Word, FHashLength);
    // Quickest way is insert one word than add and than sort whole list
    FWordList.Insert(GetInsertPos(Word), AWordItem);
    CalculateCacheArray; // Calculate cache array to speed up searches
    FModified := True;
    if Assigned(FOnAddWord) then
      FOnAddWord(Self, Word);
  end;
end;

procedure TSynSpellCheck.AddDictWordList(WordList: TStringList);
var
  iI: Integer;
begin
  for iI := 0 to WordList.Count - 1 do
    AddDictWord(WordList.Strings[iI]);
end;

procedure TSynSpellCheck.AddSkipWord(Word: string);
begin
  if Trim(Word) <> '' then
    FSkipList.Add(AnsiLowerCase(Word));
end;

procedure TSynSpellCheck.AddSkipWordList(WordList: TStringList);
var
  iI: Integer;
begin
  for iI := 0 to WordList.Count - 1 do
    AddSkipWord(WordList.Strings[iI]);
end;

procedure TSynSpellCheck.CalculateCacheArray;
var
  chOld,
  chNew: Char;
  iI   : Integer;
begin
  if FWordList.Count = 0 then
    Exit;

  chOld := TWordRec(FWordList.Items[0]^).Word[1];
  chNew := chOld;
  FCacheArray[Ord(chOld)][0] := 0;                                              //Fiala
  FCacheArray[Ord(chOld)][1] := 0;                                              //Fiala
  for iI := 0 to FWordList.Count - 1 do
    if chOld <> TWordRec(FWordList.Items[iI]^).Word[1] then begin
      chNew := TWordRec(FWordList.Items[iI]^).Word[1];
      FCacheArray[Ord(chOld)][1] := iI - 1; // Last occurence of previous letter
      FCacheArray[Ord(chNew)][0] := iI;     // First occurence of new letter
      chOld := chNew;
    end;
  // Last occurence of last letter
  FCacheArray[Ord(chNew)][1] := FWordList.Count - 1;
end;

procedure TSynSpellCheck.ClearDictWords;
var
  iI       : Integer;
  AWordItem: PWordRec;
begin
  for iI := 0 to FWordList.Count - 1 do
  begin
    AWordItem := FWordList.Items[iI];                                           //Fiala
    Dispose(AWordItem);                                                         //Fiala
  end;
  FWordList.Clear;
end;

procedure TSynSpellCheck.ClearSkipWords;
begin
  FSkipList.Clear;
end;

procedure TSynSpellCheck.CloseDictionary;
var
  iI       : Integer;
begin
  if FOpenDictionary then
  begin
    for iI := 0 to 255 do begin
      FCacheArray[iI][0] := 0;
      FCacheArray[iI][1] := 0;
    end;
    ClearDictWords;
    FSkipList.Clear;
    FOpenDictionary := False;
    if Assigned(FOnDictClose) then
      FOnDictClose(Self);
  end;
end;

procedure TSynSpellCheck.DeleteDictWord(Word: string);
begin
  Dispose(PWordRec(FWordList.Items[FindWord(AnsiLowerCase(Word))]^));
end;

procedure TSynSpellCheck.DeleteSkipWord(Word: string);
begin
  with FSkipList do
    Delete(IndexOf(AnsiLowerCase(Word)));
end;

function TSynSpellCheck.IsDictWord(Word: string): Boolean;
begin
  Result := (FindWord(AnsiLowerCase(Word)) <> -1);
end;

function TSynSpellCheck.IsSkipWord(Word: string): Boolean;
begin
  Result := (FSkipList.IndexOf(AnsiLowerCase(Word)) <> -1);
end;

procedure TSynSpellCheck.FixLists;
var
  iI: Integer;
begin
  for iI := 0 to FSkipList.Count - 1 do
    FSkipList.Strings[iI] := AnsiLowerCase(FSkipList.Strings[iI]);
end;

procedure TSynSpellCheck.GetDictionaryList(tslList: TStrings);
var
  srDics: TSearchRec;

  procedure AddDictionary;
  var
    sLanguage: string;
  begin
    sLanguage := Copy(srDics.Name, 1, Pos('.', srDics.Name) - 1);
    if (tslList.IndexOf(sLanguage) = -1) and (Pos('.user.', srDics.Name) = 0) then
      tslList.Add(sLanguage);
  end;

var
  iI: Integer;
begin
  tslList.BeginUpdate;
  try
    if FindFirst(GetDictionaryDir + '*.?-?-?.dic', faAnyFile, srDics) = 0 then begin
      AddDictionary;
      while FindNext(srDics) = 0 do
        AddDictionary;
    end;
    SysUtils.FindClose(srDics);
    for iI := 0 to tslList.Count - 1 do
      tslList.Strings[iI] := UpCase(tslList.Strings[iI][1]) +
        Copy(tslList.Strings[iI], 2, Length(tslList.Strings[iI]));
  finally
    tslList.EndUpdate;
  end;
end;

function TSynSpellCheck.GetSuggestions(const Word: string; SuggestionList: TStrings): Integer;
var
  iI, iLength: Integer;
  sHash, sWord  : string;
begin
  Result := 0;
  if Assigned(SuggestionList) then
  begin
    SuggestionList.Clear;
    ////////////////////////////////////////////////////////////////////////////
    // Select algorithm
    ////////////////////////////////////////////////////////////////////////////
    if FHashAlgorithm = haSoundEx then
      sHash := SoundEx(Word, FHashLength);
    iLength := Length(Word);
    for iI := 0 to FWordList.Count - 1 do
      if (TWordRec(FWordList.Items[iI]^).Hash = sHash) and (Abs(iLength - Length(TWordRec(FWordList.Items[iI]^).Word)) < 2) then
      begin
        sWord := TWordRec(FWordList.Items[iI]^).Word;
        //////////////////////////////////////////////////////////////////////
        // Maintain case for uppercase and capitalized words.
        //////////////////////////////////////////////////////////////////////
        if AnsiUpperCase(Word) = Word then
          sWord := AnsiUpperCase(sWord)
        else if AnsiUpperCase(Word[1])[1] = Word[1] then
          sWord[1] := AnsiUpperCase(sWord[1])[1];
        SuggestionList.Add(sWord);
      end;
    Result := SuggestionList.Count;
  end;
end;

procedure TSynSpellCheck.LoadDictionary(Language,FileName: string);
var
  AWordItem: PWordRec;
  sLine,
  sName    : string;
  fOut     : TextFile;

  procedure AddNewWord(sWord: string; IsUser: Boolean);
  begin
      New(AWordItem);
      with AWordItem^ do begin
        Word := sWord;
        User := IsUser;
      end;
      if FHashAlgorithm = haSoundEx then
        AWordItem^.Hash := SoundEx(sWord, FHashLength);
      FWordList.Add(AWordItem);
  end;

begin
  CloseDictionary;
  FMaxWordLength := 0;
  if Trim(FileName) = '' then
  begin
    sName       := GetDictionaryDir + GetNewestDictionary(Language);
    FDictionary := Language;
  end
  else
  begin
    sName       := FileName;
    FDictionary := FileName;
  end;
  AssignFile(fOut, sName);
  Reset(fOut);
  while not Eof(fOut) do
  begin
    ReadLn(fOut, sLine);
    if Trim(sLine) <> '' then
    begin
      FMaxWordLength := Max(FMaxWordLength, Length(sLine));
      AddNewWord(sLine, False);
    end;
  end;
  CloseFile(fOut);
  sName := ExtractFileName(sName);
  with FLanguage do begin
    Name    := Copy(sName, 1, Pos('.', sName) - 1);
    Version := Copy(sName, Pos('.', sName) + 1, 5);
  end;
  FUserFileName := FLanguage.Name + '.user.dic';
  //////////////////////////////////////////////////////////////////////////////
  // Load user's dictionary if present
  //////////////////////////////////////////////////////////////////////////////
  FModified := False;
  if FUseUserDictionary then begin
    if FUserDictPath = '' then
      FUserDictPath := GetUserDictionaryDir;
    sName := IncludeTrailingBackslash(FUserDictPath) + FUserFileName;
    if FileExists(sName) then begin
      AssignFile(fOut, sName);
      Reset(fOut);
      while not Eof(fOut) do begin
        ReadLn(fOut, sLine);
        FMaxWordLength := Max(FMaxWordLength, Length(sLine));
        if Trim(sLine) <> '' then
          AddNewWord(sLine, True);
      end;
      CloseFile(fOut);
      FModified := False;
    end;
  end;

  SortWordList;        // Sort the whole word list
  CalculateCacheArray; // Calculate cache array to speed up searches
  FOpenDictionary := True;
  if Assigned(FOnDictLoad) then
    FOnDictLoad(Self);
end;

function TSynSpellCheck.FindWord(sWord: string): Integer;
{var
  iI, iHash: Integer;
begin
  Result := -1;
  // Are there any words at all starting with this letter?
  if FCacheArray[Ord(sWord[1])][1] = 0 then
    Exit;

  iHash := ElfHash(sWord);
  for iI := FCacheArray[Ord(sWord[1])][0] to FCacheArray[Ord(sWord[1])][1] do
    if PWordRec(FWordList.Items[iI])^.Value = iHash then begin
//    if CompareStr(PWordRec(FWordList.Items[iI])^.Word, sWord) = 0 then begin
      Result := iI;
      Exit;
    end;}
var
  L, H, I, C: Integer;
begin
  Result := -1;
  // Are there any words at all starting with this letter?
  if FCacheArray[Ord(sWord[1])][1] = 0 then                                     //Fiala
    Exit;
  L := FCacheArray[Ord(sWord[1])][0];
  H := FCacheArray[Ord(sWord[1])][1];
  while L <= H do
  begin
    I := (L + H) shr 1;
    { here must be CompareStr not AnsiCompareStr, because dictionary is ASCII sorted }
    C := CompareStr(PWordRec(FWordList.Items[I])^.Word, sWord);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then
        Result := I;
    end;
  end;
end;

procedure TSynSpellCheck.LoadSkipList(FileName: string);
begin
  if FileExists(FileName) then 
    FSkipList.LoadFromFile(FileName);
end;

procedure TSynSpellCheck.SaveSkipList(FileName: string);
var
  iI: Integer;
begin
  for iI := 0 to FSkipList.Count -1 do
    if Trim(FSkipList.Strings[iI]) = '' then
      FSkipList.Delete(iI); 
  FSkipList.SaveToFile(FileName);
end;

procedure TSynSpellCheck.SaveUserDictionary;
var
  iI : Integer;
  fIn: TextFile;
begin
  if not DirectoryExists(ExtractFileDir(FUserDictPath)) then
    if not ForceDirectories(ExtractFileDir(FUserDictPath)) then
      Exit;
  AssignFile(fIn, IncludeTrailingBackslash(FUserDictPath) + FUserFileName);
  Rewrite(fIn);
  for iI := 0 to FWordList.Count - 1 do
    if TWordRec(FWordList.Items[iI]^).User then
      WriteLn(fIn, TWordRec(FWordList.Items[iI]^).Word);
  CloseFile(fIn);
  FModified := False;
end;

procedure TSynSpellCheck.SetHashAlgorithm(const Value: HashAlgorithms);
var
  iI       : Integer;
  AWordItem: PWordRec;
begin
  if Value <> FHashAlgorithm then begin
    FHashAlgorithm := Value;
    if FWordList.Count > 0 then
      for iI := 0 to FWordList.Count - 1 do begin
        AWordItem := FWordList.Items[iI];
        if FHashAlgorithm = haSoundEx then
          AWordItem^.Hash := SoundEx(AWordItem^.Word, FHashLength)
      end;
  end;
end;

procedure TSynSpellCheck.SetHashLength(const Value: THashLength);
var
  iI       : Integer;
  AWordItem: PWordRec;
begin
  if FHashLength <> Value then begin
    ////////////////////////////////////////////////////////////////////////////
    // Soundex hashes are supported up to 8 characters long.
    ////////////////////////////////////////////////////////////////////////////
    FHashLength := Value;
    if FWordList.Count > 0 then
      for iI := 0 to FWordList.Count - 1 do begin
        AWordItem := FWordList.Items[iI];
        if FHashAlgorithm = haSoundEx then
          AWordItem^.Hash := SoundEx(AWordItem^.Word, FHashLength);
      end;
  end;
end;

procedure TSynSpellCheck.SetSkipList(Value: TStringList);
begin
  SkipList.Assign(Value);
end;

procedure TSynSpellCheck.SortWordList;
begin
  FWordList.Sort(SortFunc);
end;

end.

