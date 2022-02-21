library waveout;

uses WaveMixStripped;

var i: integer;
    FHandle: THandle;
    FPlayParams: TMixPlayParams;

procedure InitWaveOut;
begin
  FHandle := WaveMixInit;

  with FPlayParams do
  begin
    Size := sizeof(TMixPlayParams);
    hMixSession := FHandle;
  end;

  WaveMixActivate(FHandle, true);
  WaveMixOpenChannel(FHandle, 0, WMIX_ALL);
end;

function LoadWave(FileName: string): PMixWave;
begin
  Result := WaveMixOpenWave(FHandle, Pointer(FileName), 0, WMIX_FILE);
end;

procedure PlayWave(LSound: PMixWave);
begin
  FPlayParams.iChannel := i mod 8;
  FPlayParams.lpMixWave := LSound;
  FPlayParams.dwFlags := WMIX_USELRUCHANNEL or WMIX_HIGHPRIORITY;
  FPlayParams.wLoops := 0;

  WaveMixPlay(@FPlayParams);
end;

procedure DestroyWaveOut;
begin
  WaveMixActivate(FHandle, false);
  WaveMixCloseSession(FHandle);
end;

exports
  InitWaveOut,
  DestroyWaveOut,
  PlayWave,
  LoadWave;

begin
end.
