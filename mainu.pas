unit Mainu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WaveMix;

const
  KeyEvent = WM_USER + 1;

type
  TKeyForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure KeyEventHandler(var Msg : TMessage); message KeyEvent;
  end;

var
  KeyForm: TKeyForm;
  WaveMix: TWaveMix;
  WaveFileKeyDown, WaveFileKeyUp: PMixWave;

implementation


function SetKeyHook : Longint; stdcall; external 'Key.dll';
function DelKeyHook : Longint; stdcall; external 'Key.dll';

{$R *.dfm}

var i: integer = 0; 

procedure TKeyForm.FormCreate(Sender: TObject);
begin
 WaveMix := TWaveMix.Create();
 WaveFileKeyDown := WaveMix.OpenFromFile('keydown.wav');
 WaveFileKeyUp := WaveMix.OpenFromFile('keyup.wav');
 WaveMix.Channels := $FF;
 WaveMix.Activated := true;
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
  if (KeyState = 0) then begin
    WaveMix.Play(i mod 8, WaveFileKeyDown, nil, WMIX_USELRUCHANNEL or WMIX_HIGHPRIORITY, 0);
  end;

  if (KeyState = 3) then begin
    WaveMix.Play(i mod 8, WaveFileKeyUp, nil, WMIX_USELRUCHANNEL or WMIX_HIGHPRIORITY, 0);
  end;
end;

end.
