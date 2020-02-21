library Key;

uses
  WinTypes,
  WinProcs,
  Messages,
  MMSystem;

const
  KeyEvent = WM_USER + 1;

var
  HookHandle   : hHook;

function KeyHook(nCode: integer; WParam: Word; LParam: LongInt): Longint; stdcall;
var KeyState: Integer;
begin
 KeyState := lParam shr 30;

 if (nCode = HC_ACTION) then begin
  if (KeyState = 0) then 
    sndPlaySound('keydown.wav', SND_ASYNC or SND_FILENAME);
  if (KeyState = 3) then 
    sndPlaySound('keyup.wav', SND_ASYNC or SND_FILENAME);
 end;

 Result := CallNextHookEx(HookHandle, nCode, WParam, LParam);
end;

procedure SetKeyHook; stdcall;
begin
 if HookHandle <> 0 then exit;
 HookHandle      := SetWindowsHookEx(WH_KEYBOARD, @KeyHook, HInstance, 0);
end;

procedure DelKeyHook; stdcall;
begin
  if HookHandle <> 0 then begin
    UnhookWindowsHookEx(HookHandle);
    HookHandle := 0;
  end;
end;

exports
  SetKeyHook,
  DelKeyHook;

begin
 HookHandle := 0;
end.

