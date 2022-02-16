unit Mainu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Inifiles;

const
  KeyEvent = WM_USER + 1;
  MAXFILENAME = 255;

type
  TKeyForm = class (TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    procedure KeyEventHandler(var Msg: TMessage); message KeyEvent;
    procedure LoadSounds(folder: string);
    function GetSetNames(list: TStrings): TStrings;
  end;

  PWaveHdr = ^TWaveHdr;
  {$EXTERNALSYM wavehdr_tag}
  wavehdr_tag = record
    lpData: PChar;              { pointer to locked data buffer }
    dwBufferLength: DWORD;      { length of data buffer }
    dwBytesRecorded: DWORD;     { used for input only }
    dwUser: DWORD;              { for client's use }
    dwFlags: DWORD;             { assorted flags (see defines) }
    dwLoops: DWORD;             { loop control counter }
    lpNext: PWaveHdr;           { reserved for driver }
    reserved: DWORD;            { reserved for driver }
  end;
  TWaveHdr = wavehdr_tag;
  {$EXTERNALSYM WAVEHDR}
  WAVEHDR = wavehdr_tag;

  PWaveFormat = ^TWaveFormat;
  TWaveFormat = record
    wFormatTag: Word;         { format type }
    nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWord;  { sample rate }
    nAvgBytesPerSec: DWord; { for buffer estimation }
    nBlockAlign: Word;      { block size of data }
  end;

  TPCMWaveFormat = record
    wf: TWaveFormat;
    wBitsPerSample: Word;
  end;
  PPCMWaveFormat = ^TPCMWaveFormat;

  TMixWave = record
	  pcm: TPCMWAVEFORMAT;
	  wh: TWAVEHDR;
    szWaveFilename: array [0..MAXFILENAME] of char;
  end;
  PMixWave = ^TMixWave;

var
  KeyForm: TKeyForm;
  DownSounds: array of PMixWave;
  UpSounds: array of PMixWave;

function ScanDir: TStrings;

implementation


function SetKeyHook: Longint; stdcall; external 'Key.dll';
function DelKeyHook: Longint; stdcall; external 'Key.dll';

procedure InitWaveOut; cdecl; external 'waveout.dll';
procedure DestroyWaveOut; cdecl; external 'waveout.dll';
procedure PlayWave(Sound: PMixWave); cdecl; external 'waveout.dll';
function LoadWave(FileName: String): PMixWave; cdecl; external 'waveout.dll';

{$R *.dfm}

var DirList: TStrings;

procedure TKeyForm.FormCreate(Sender: TObject);
begin
  InitWaveOut;
  DirList := ScanDir;
  Listbox1.Items := KeyForm.GetSetNames(DirList);
  KeyForm.LoadSounds(DirList.Strings[0]);
  SetKeyHook;
end;

procedure TKeyForm.FormDestroy(Sender: TObject);
begin
  DelKeyHook;
  DestroyWaveOut;
end;

procedure TKeyForm.KeyEventHandler(var Msg: TMessage);
var KeyState: integer;
    Sound: PMixWave;
begin
  KeyState := Msg.LParam shr 30;
  if (KeyState = 0) then
  begin
    Sound := DownSounds[Random(Length(DownSounds))];
    PlayWave(Sound);
  end;

  if (KeyState = 3) then
  begin
    Sound := UpSounds[Random(Length(UpSounds))];
    PlayWave(Sound);
  end;
end;

function ScanDir(): TStrings;
var
  Path: String;
  SR: TSearchRec;
  DirList: TStrings;
begin
  Path := '.\sound\';
  DirList := TStringList.Create;
  try
    if FindFirst(Path + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') then
        begin DirList.Add(SR.Name) end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    result := dirlist;
  finally
  end;
end;

function TKeyForm.GetSetNames(list: TStrings): TStrings;
var i: integer;
  ini: TIniFile;
  setname: string;
begin
  result := TStringList.Create;
  for i := 0 to list.count - 1 do
  begin
    ini := TInifile.Create('.\sound\' + list.Strings[i] + '\config.ini');
    setname := ini.ReadString('about', 'name', list.Strings[i]);
    result.Add(setname);
  end;
end;

procedure TKeyForm.LoadSounds(folder: string);
var ini: TIniFile;
  list: TStrings;
  i: integer;
  s: PMixWave;
begin
  GetMem(s, sizeof(TMixWave));
  list := TStringList.Create;
  ini := TInifile.Create('.\sound\' + folder + '\config.ini');
  ini.ReadSection('down', list);
  if list.count > 0 then
  begin
    SetLength(DownSounds, list.count);
    for i := 0 to list.Count - 1 do
    begin
      DownSounds[i] := LoadWave('.\sound\' + folder + '\' + ini.ReadString('down', list[i], ''));
    end;
  end;

  ini.ReadSection('up', list);
  if list.count > 0 then
  begin
    SetLength(UpSounds, list.count);
    for i := 0 to list.Count - 1 do
    begin
      UpSounds[i] := LoadWave('.\sound\' + folder + '\' + ini.ReadString('up', list[i], ''));
    end;
  end;
end;

procedure TKeyForm.ListBox1Click(Sender: TObject);
begin
  keyform.loadsounds(DirList.Strings[listbox1.ItemIndex]);
end;

end.
