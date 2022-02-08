unit Mainu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WaveMix, Inifiles;

const
  KeyEvent = WM_USER + 1;

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

var
  KeyForm: TKeyForm;
  WaveMix: TWaveMix;
  WaveFileKeyDown, WaveFileKeyUp: PMixWave;
  DownSounds: array of PMixWave;
  UpSounds: array of PMixWave;

function ScanDir: TStrings;

implementation


function SetKeyHook: Longint; stdcall; external 'Key.dll';
function DelKeyHook: Longint; stdcall; external 'Key.dll';

{$R *.dfm}

var i: integer = 0;
var DirList: TStrings;

procedure TKeyForm.FormCreate(Sender: TObject);
begin
  WaveMix := TWaveMix.Create();
  WaveMix.Channels := $FF;
  WaveMix.Activated := true;
  DirList := ScanDir;
  Listbox1.Items := KeyForm.GetSetNames(DirList);
  KeyForm.LoadSounds(DirList.Strings[0]);
  SetKeyHook;
end;

procedure TKeyForm.FormDestroy(Sender: TObject);
begin
  DelKeyHook;
  WaveMix.Activated := false;
  WaveMix.Destroy();
end;

procedure TKeyForm.KeyEventHandler(var Msg: TMessage);
var KeyState: integer;
begin
  inc(i);
  KeyState := Msg.LParam shr 30;
  if (KeyState = 0) then
  begin
    WaveFileKeyDown := DownSounds[Random(Length(DownSounds))];
    WaveMix.Play(i mod 8, WaveFileKeyDown, nil, WMIX_USELRUCHANNEL or WMIX_HIGHPRIORITY, 0);
  end;

  if (KeyState = 3) then
  begin
    WaveFileKeyUp := UpSounds[Random(Length(UpSounds))];
    WaveMix.Play(i mod 8, WaveFileKeyUp, nil, WMIX_USELRUCHANNEL or WMIX_HIGHPRIORITY, 0);
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
begin
  list := TStringList.Create;
  ini := TInifile.Create('.\sound\' + folder + '\config.ini');
  ini.ReadSection('down', list);
  if list.count > 0 then
  begin
    SetLength(DownSounds, list.count);
    for i := 0 to list.Count - 1 do
    begin
      DownSounds[i] := WaveMix.OpenFromFile('.\sound\' + folder + '\' + ini.ReadString('down', list[i], ''));
    end;
  end;

  ini.ReadSection('up', list);
  if list.count > 0 then
  begin
    SetLength(UpSounds, list.count);
    for i := 0 to list.Count - 1 do
    begin
      UpSounds[i] := WaveMix.OpenFromFile('.\sound\' + folder + '\' + ini.ReadString('up', list[i], ''));
    end;
  end;
end;

procedure TKeyForm.ListBox1Click(Sender: TObject);
begin
  keyform.loadsounds(DirList.Strings[listbox1.ItemIndex]);
end;

end.
