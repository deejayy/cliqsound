library Key;

uses
  WinTypes,
  WinProcs,
  Messages;

const
  KeyEvent = WM_USER + 1;

var
  HookHandle   : hHook;

function KeyHook(nCode: integer; WParam: Word; LParam: LongInt): Longint; stdcall;
var LogWindowHandle: THandle;
begin
 if (nCode = HC_ACTION) then begin
    LogWindowHandle := FindWindow('TKeyForm', nil);
    if LogWindowHandle <> 0 then
      SendMessage(LogWindowHandle, KeyEvent, wParam, lParam);
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

