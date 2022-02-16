library waveout;

uses WaveMixStripped;

var MyWaveMix: TWaveMix;
    i: integer;

procedure InitWaveOut;
begin
  MyWaveMix := TWaveMix.Create();
  MyWaveMix.Channels := $FF;
  MyWaveMix.Activated := true;
end;

procedure DestroyWaveOut;
begin
  MyWaveMix.Activated := false;
  MyWaveMix.Destroy();
end;

procedure PlayWave(LSound: PMixWave);
begin
  MyWaveMix.Play(i mod 8, LSound, WMIX_USELRUCHANNEL or WMIX_HIGHPRIORITY, 0);
end;

function LoadWave(FileName: string): PMixWave;
begin
  Result := MyWaveMix.OpenFromFile(FileName);
end;

exports
  InitWaveOut,
  DestroyWaveOut,
  PlayWave,
  LoadWave;

begin
end.
