unit Mainu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TKeyForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  KeyForm: TKeyForm;

implementation


function SetKeyHook : Longint; stdcall; external 'Key.dll';
function DelKeyHook : Longint; stdcall; external 'Key.dll';

{$R *.dfm}

procedure TKeyForm.FormCreate(Sender: TObject);
begin
 SetKeyHook;
end;

procedure TKeyForm.FormDestroy(Sender: TObject);
begin
 DelKeyHook;
end;

end.
