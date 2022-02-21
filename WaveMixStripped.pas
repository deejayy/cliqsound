unit WaveMixStripped;

interface

uses SysUtils, Windows, MMSystem;

const
  WMIX_FILE = $0001;            {WMixOpen: from disk file}
  WMIX_RESOURCE = $0002;        {WMixOpen: from system resource file}
  WMIX_MEMORY = $0004;          {WMixOpen: from memory}
  WMIX_OPENSINGLE = $0000;      {WMixChannel: open one Play channel}
  WMIX_OPENALL = $0001;         {WMixChannel: open ALL eight channels}
  WMIX_OPENCOUNT = $0002;       {WMixChannel: open # channels}
  WMIX_ALL = $0001;             {WMixFlush & Close: do ALL channels}
  WMIX_NOREMIX = $0002;         {WMixFlush: don't remix sound data}
  WMIX_CHANNELS = $0001;        {WMixConfigure: set channels}
  WMIX_SAMPLINGRATE = $0002;    {WMixConfigure: set playback rate}
  WMIX_QUEUEWAVE = $0000;       {WMixPlay: play when previous finishes}
  WMIX_CLEARQUEUE = $0001;      {WMixPlay: play sound immediately}
  WMIX_USELRUCHANNEL = $0002;   {WMixPlay: play on next available chan}
  WMIX_HIGHPRIORITY = $0004;    {WMixPlay: remix play buffer immediately}
  WMIX_WAIT = $0008;            {WMixPlay: hold until next 'Play'}

  MAXFILENAME = 255;

type
  EWaveMixError = class (Exception);

   {$A-}
  PWaveFormat = ^TWaveFormat;
  TWaveFormat = record
    wFormatTag: Word;         { format type }
    nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWord;  { sample rate }
    nAvgBytesPerSec: DWord; { for buffer estimation }
    nBlockAlign: Word;      { block size of data }
  end;

  TMixConfig = record         {Storage record for WMixConfigure}
    wSize: word;          {Record size}
    dwFlags: longint;      {Flags (see below). Must OR with both...}
    wChannels: word;      {1(mono) or 2(stereo) And ...}
    wSamplingRate: word;      {11, 22, or 44 kHz}
  end;
  PMixConfig = ^TMixConfig;

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

  TMixPlayParams = record         {Storage record for WMixPlay}
    Size: Word;          {Record size}
    hMixSession: THandle;    {WaveMix session handle}
    iChannel: Integer;    {Play on channel(0 to 7)}
    lpMixWave: PMixWave;      {Memory pointer to Wave file}
    hWndNotify: HWND;        {Handle to send MM_WOM_DONE message to}
    dwFlags: Longint;      {Play flags (see below)}
    wLoops: Word;         {# of times to loop ($FFFF = indefinite)}
  end;
  PMixPlayParams = ^TMixPlayParams;

   {$A+}


   // function prototypes
function WaveMixInit: THandle;
function WaveMixConfigureInit(lpConfig: PMIXCONFIG): THandle;
procedure WaveMixCloseSession(hMixSession: THandle);
procedure WaveMixActivate(hMixSession: THandle; fActivate: Boolean);
procedure WaveMixOpenChannel(hMixSession: THandle; iChannel: Integer; dwFlags: DWord);
procedure WaveMixCloseChannel(hMixSession: THandle; iChannel: Integer; dwFlags: DWord);
function WaveMixOpenWave(hMixSession: THandle; szWaveFilename: PChar; hInst: THandle; dwFlags: DWord): PMixWave;
procedure WaveMixFreeWave(hMixSession: THandle; lpMixWave: PMIXWAVE);
procedure WaveMixFlushChannel(hMixSession: THandle; iChannel: Integer; dwFlags: DWord);
procedure WaveMixPlay(lpMixPlayParams: PMIXPLAYPARAMS);


const
  SILENCE = $80;
  MAXCHANNELS = 16;
  MINWAVEBLOCKS = 2;
  MAXWAVEBLOCKS = 6;
  BYTESPERSAMPLE = 1;
  BITSPERSAMPLE = 8;
  BITSPERBYTE = 8;
  MONO = 1;
  MINWAVEBLOCKLEN = 512;
  MAXWAVEBLOCKLEN = 4096;
  MAXQUEUEDWAVES = 100;
  MAGICNO = $5432;
  SAMPLESPERSEC = 11025;
  WMIX_CONFIG_CHANNELS = 1;
  WMIX_CONFIG_SAMPLINGRATE = 2;
  _MAX_PATH = 200;
  DEFAULT_NTWAVEBLOCKLEN = 2048;
  DEFAULT_REMIXALGORITHM = 1;    // ResetRemix()                */
  DEFAULT_GOODWAVPOS = 0;    // use timeGetTime()           */
  DEFAULT_WAVEBLOCKS = 3;    // number of ping-pong buffers */
  DEFAULT_WAVEBLOCKLEN = 0;    // 0 forces code to figure DMA size */
  DEFAULT_SAMPLESPERSEC = 44;   // 44 khz                    */

  gszAppName: string = 'WavMix32';
  gszDefault: string = 'default';
  gszMemError: string = 'Unable to allocate memory for waveform data.  ' +
    'Try making more memory available by closing other applications.';
  gszIniFile: array [0..12] of char = 'WAVEMIX.INI';

  gpFormat: TPCMWAVEFORMAT = (wf:
    (wFormatTag: WAVE_FORMAT_PCM;
    nChannels: MONO;
    nSamplesPerSec: SAMPLESPERSEC;
    nAvgBytesPerSec: SAMPLESPERSEC * BYTESPERSAMPLE * MONO;
    nBlockAlign: BYTESPERSAMPLE);
    wBitsPerSample: BITSPERSAMPLE);

type
  PSAMPLE = PCHAR;
  PSAMPLE16 = ^Smallint;
  HPINT = ^Word;
  PDWord = ^DWord;

  PCHANNELNODE = ^CHANNELNODE;
  CHANNELNODE = record
    next: PCHANNELNODE;
    PlayParams: TMIXPLAYPARAMS;
    lpMixWave: PMIXWAVE;
    dwNumSamples: DWord;
    dwStartPos: DWord; // this is the MyWaveOutGetPosition() or dwEndPos+1 of previous wave depending on whether high priority or getting queued
    dwEndPos: DWord; // this is the end pos: dwStartPos+dwNumSamples*(1+numloops)
    lpPos: PSAMPLE; // this is pointer to current position within data, with current mixer play is equal to wh.lpData
    lpEnd: PSAMPLE; // this is pointer to last sample of actual wave data
  end;

  fnRemix = procedure(dwRemixSamplePos: DWord; pCD: PCHANNELNODE);
  fnSampleAdjust = function(dwSamplePos: DWord; dwAdjustAmt: DWord): DWord;
  GLOBALS = record
    wMagic1: Word;
    hWndApp: HWND; //=NULL;
    fShow: Boolean;
    hWaveOut: HWAVEOUT;
    fActive: Boolean; //=FALSE;
    wDeviceID: UINT;  //=(UINT)WAVE_MAPPER;
    szDevicePName: array [0..MAXPNAMELEN] of char;
    aChannel: array [0..MAXCHANNELS - 1] of PCHANNELNODE;
    iChannels: Integer; //=0;
    MRUChannel: array [0..MAXCHANNELS - 1] of DWord;
    dwMRU: DWord;
    pcm: TPCMWAVEFORMAT;
    dwWaveBlockLen: DWord;
    iNumWaveBlocks: Integer;

    dwCurrentSample: DWord;
    dwBaseTime: DWord;

    fGoodGetPos: Boolean; // =TRUE;
    dwWaveOutPos: DWord;
    pfnRemix: fnRemix;
    pfnSampleAdjust: fnSampleAdjust;
    pWaitList: PCHANNELNODE; // points to last node in wait list, last node points to first node
                        //BUGBUG: do I want to hand WaveMixFlush??? No for now
    wMagic2: Word;
  end;
  PGLOBALS = ^GLOBALS;

  PXWAVEHDR = ^XWAVEHDR;
  XWAVEHDR = record
    wh: TWAVEHDR;  // NOTE: wh must be first field in structure since pointer will get passed as WAVEHDR to waveOut functions */
    fAvailable: Boolean;
    dwWavePos: DWord;
    g: PGLOBALS;
    QNext: PXWAVEHDR;
  end;

  PLAYQUEUE = record
    first: PXWAVEHDR;
    last: PXWAVEHDR;
  end;
  PPLAYQUEUE = ^PLAYQUEUE;

procedure InitLibrary;
function MixerPlay(lpXWH: PXWAVEHDR; fWriteBlocks: Boolean): Boolean;
function MyWaveOutGetPosition(hWaveOut: HWAVEOUT; fGoodGetPos: Boolean): DWord;
procedure MyWaveOutReset(hWaveOut: HWAVEOUT);


implementation

var
  gfShow: Boolean = TRUE;
  giDebug: Integer;
   //int giNumWaveBlocks=3;   // number of ping pong wave buffers
  gaWaveBlock: array [0..MAXWAVEBLOCKS - 1] of PXWAVEHDR;
  gPlayQueue: PLAYQUEUE;
  gaChannelNodes: array [0..MAXQUEUEDWAVES - 1] of CHANNELNODE;
  gpFreeChannelNodes: PCHANNELNODE;
  gsz: string;
  g, gActiveSession: PGLOBALS;
  gfCount: Boolean;


function mmioFOURCC(ch0, ch1, ch2, ch3: Char): DWord;
begin
  Result := Ord(ch0) + (Ord(ch1) shl 8) + (Ord(ch2) shl 16) + (Ord(ch3) shl 24);
end;

function SessionToGlobalDataPtr(hMixSession: THandle): PGLOBALS;
var
  pG: PGLOBALS;
begin
  pG := PGLOBALS(hMixSession);

  if ((pG = nil) or (pG^.wMagic1 <> MAGICNO) or (pG^.wMagic2 <> MAGICNO)) then
  begin raise EWaveMixError.Create('Invalid Session handle') end;

  Result := pG;
end;

function MyWaveOutGetPosition(hWaveOut: HWAVEOUT; fGoodGetPos: Boolean): DWord;
var
  mmt: TMMTIME;
  t1: DWord;
  t: Comp;
begin
  if (fGoodGetPos = True) then
  begin
    mmt.wType := TIME_BYTES;
    waveOutGetPosition(hWaveOut, @mmt, sizeof(TMMTIME));
    Result := g^.pfnSampleAdjust(mmt.sample, g^.dwWaveOutPos);
  end
  else
  begin
    // this code is potentially not that accurate.  If a sound board does not play
    // at exactly the sampling rate.  I think for the most part thought that the
    // drift will be negligible, especially for short sounds.  If this is not the
    // case then this function will have to be made more accurate.
    // animation)
    t1 := timeGetTime - g^.dwBaseTime;
    t := t1;
    t := (t * g^.pcm.wf.nAvgBytesPerSec) / 1000;
    t1 := Trunc(t);  // may need to and with 0xfffffffc  to align channels
    if ((g^.pcm.wf.nBlockAlign > 1) and ((t1 and 1) <> 0)) then  // return values must be block aligned or will screw up stereo
    begin Dec(t1) end;
    Result := t1;
  end;
end;

function AddFactor(dwSample: DWord; dwAdjustAmt: DWord): DWord;
begin
  Result := dwSample + dwAdjustAmt;
end;

function SubFactor(dwSample: DWord; dwAdjustAmt: DWord): DWord;
begin
  Result := dwSample - dwAdjustAmt;
end;

procedure SetWaveOutPosition(dwSamplePos: DWord);
var
  mmt: TMMTIME;
begin

  //  for fGoodGetPos */
  mmt.wType := TIME_BYTES;
  if (g^.hWaveOut <> 0) then
  begin
    waveOutGetPosition(g^.hWaveOut, @mmt, sizeof(TMMTIME));
    mmt.sample := mmt.sample;
  end
  else
  begin mmt.sample := 0 end;

  if (mmt.sample >= dwSamplePos) then
  begin
    g^.dwWaveOutPos := mmt.sample - dwSamplePos;
    g^.pfnSampleAdjust := SubFactor;
  end
  else
  begin
    g^.dwWaveOutPos := dwSamplePos - mmt.sample;
    g^.pfnSampleAdjust := AddFactor;
  end;

  // for fGoodGetPos=FALSE */

    // this method is subject to drift if the sound card does not play at
    // exactly 11025 Hz.
  g^.dwBaseTime := timeGetTime - (dwSamplePos * 1000) div g^.pcm.wf.nAvgBytesPerSec;
    // bugbug: if stereo gets screwed up may need to force dwBaseTime to be block aligned

  g^.dwCurrentSample := dwSamplePos;
end;

procedure MyWaveOutReset(hWaveOut: HWAVEOUT);
var
  // if don't do this, this will screw up the current pos time in channel blocks.
  dwTmp: DWord;

begin
  dwTmp := MyWaveOutGetPosition(hWaveOut, g^.fGoodGetPos);
  waveOutReset(hWaveOut);
  SetWaveOutPosition(dwTmp);
end;

// cmixit: function that actually mixes wave data.  It must be 286 compatible
procedure cmixit(lpDest: PSAMPLE; rgWaveSrc: array of PSAMPLE; iNumWaves: Integer; wLen: Word);
var
  i, iSum: Integer;
  Ctr: Word;
begin
  // special case ctr == 1: there is only one channel with data
  // so just copy it directly to the wave buffer
  if (iNumWaves = 1) then
  begin Move(rgWaveSrc[0]^, lpDest^, wLen) end
  else // iNumWaves >= 2 */
  begin
    // BUGBUG: this code should be optimized but should remain in C
    ctr := 0;
    while (wLen <> 0) do
    begin
      iSum := 128;
      for i := 0 to iNumWaves - 1 do
      begin iSum := iSum + Word((rgWaveSrc[i] + ctr)^) - 128 end;

      if (iSum < 0) then
      begin iSum := 0 end;
      if (iSum > 255) then
      begin iSum := 255 end;
      lpDest^ := Chr(iSum);
      Inc(lpDest);
      Inc(Ctr);
      Dec(wLen);
    end;
  end;
end;

procedure InitChannelNodes;
var
  i: Integer;
begin
  for i := 0 to MAXQUEUEDWAVES - 3 do
  begin gaChannelNodes[i].next := @gaChannelNodes[i + 1] end;
  gaChannelNodes[MAXQUEUEDWAVES - 1].next := nil;
  gpFreeChannelNodes := @gaChannelNodes[0];
end;

function GetChannelNode: PCHANNELNODE;
var
  pNode: PCHANNELNODE;
begin
  if (gpFreeChannelNodes = nil) then
  begin
    Result := nil;
    exit;
  end;

  pNode := gpFreeChannelNodes;
  gpFreeChannelNodes := gpFreeChannelNodes^.next;
  pNode^.next := nil;
  Result := pNode;
end;

procedure FreeChannelNode(pNode: PCHANNELNODE);
begin
  if (pNode = nil) then
  begin exit end;

  pNode^.next := gpFreeChannelNodes;
  gpFreeChannelNodes := pNode;
end;

procedure FreeWaveBlocks(hWaveOut: HWAVEOUT);
var
  i: Integer;
begin
  for i := 0 to MAXWAVEBLOCKS - 1 do
  begin
    if (gaWaveBlock[i] = nil) then
    begin continue end;
    waveOutUnprepareHeader(hWaveOut, PWAVEHDR(gaWaveBlock[i]), sizeof(TWAVEHDR));
    GlobalFreePtr(gaWaveBlock[i]);
    gaWaveBlock[i] := nil;
  end;
end;

procedure AllocWaveBlocks(hWaveOut: HWAVEOUT);
var
  i, rslt: Integer;
begin
  // create the waveform data blocks that we will actually be using to send to the waveform device */
  i := 0;
  repeat
    gaWaveBlock[i] := PXWAVEHDR(GlobalAllocPtr(GMEM_FIXED or GMEM_SHARE or GMEM_NODISCARD, sizeof(XWAVEHDR) + MAXWAVEBLOCKLEN));
    if (gaWaveBlock[i] = nil) then
    begin
      while (i > 0) do
      begin
        Dec(i);
        GlobalFreePtr(gaWaveBlock[i]);
        gaWaveBlock[i] := nil;
      end;

      raise EWaveMixError.Create(gszMemError);
    end;
    gaWaveBlock[i]^.wh.lpData := PChar(gaWaveBlock[i]) + sizeof(XWAVEHDR);
    gaWaveBlock[i]^.wh.dwBufferLength := g^.dwWaveBlockLen;
    gaWaveBlock[i]^.wh.dwFlags := 0;
    gaWaveBlock[i]^.wh.dwLoops := 0;
    gaWaveBlock[i]^.fAvailable := TRUE;
    gaWaveBlock[i]^.dwWavePos := 0;

    Inc(i);
  until (i >= MAXWAVEBLOCKS);

  for i := 0 to MAXWAVEBLOCKS - 1 do
  begin
    rslt := waveOutPrepareHeader(hWaveOut, PWAVEHDR(gaWaveBlock[i]), sizeof(TWAVEHDR));
    if (rslt <> 0) then
    begin
      FreeWaveBlocks(hWaveOut);
      raise EWaveMixError.Create('Unable to prepare wave header #'
        + IntToStr(rslt));
    end;
    gaWaveBlock[i]^.wh.dwFlags := gaWaveBlock[i]^.wh.dwFlags or WHDR_DONE;
  end;
end;

function GetWaveBlock: PXWAVEHDR;
var
  i: Integer;
begin
  // this is kinda inefficient, may want to reimplement with pointer list */
  // but since we only have ~4 wave blocks what the hell */
  for i := 0 to g^.iNumWaveBlocks - 1 do
  begin if (gaWaveBlock[i]^.fAvailable = True) then
    begin break end end;
  if (i >= g^.iNumWaveBlocks) then // no blocks available now, wait till the wave driver is done with one */
  begin
    Result := nil;
    exit;
  end;

  gaWaveBlock[i]^.fAvailable := FALSE;

  // need to reset these two fields since Remix() can modify them */
  gaWaveBlock[i]^.wh.dwBufferLength := g^.dwWaveBlockLen;
  gaWaveBlock[i]^.wh.lpData := PChar(gaWaveBlock[i]) + sizeof(XWAVEHDR);
  gaWaveBlock[i]^.g := gActiveSession;

  Result := gaWaveBlock[i];
end;

procedure ReleaseWaveBlock(lpXWH: PXWAVEHDR);
begin
  lpXWH^.fAvailable := TRUE;
end;

// return pointer to the removed node */
function RemoveFromPlayingQueue(lpXWH: PXWAVEHDR): PXWAVEHDR;
var
  lp, lpPrev: PXWAVEHDR;
begin
  // if queue is empty do nothing */
  if (gPlayQueue.first = nil) then
  begin
    Result := nil;
    exit;
  end;

  // check if at head of list */
  if (lpXWH = gPlayQueue.first) then
  begin
    gPlayQueue.first := lpXWH^.QNext;

    // check if the queue is now empty */
    if (gPlayQueue.first = nil) then
    begin gPlayQueue.last := nil end;
  end
  else // traverse looking for node to remove */
  begin
    lpPrev := gPlayQueue.first;
    lp := gPlayQueue.first^.QNext;
    while ((lp <> nil) and (lp <> lpXWH)) do
    begin
      lpPrev := lp;
      lp := lp^.QNext;
    end;

    if (lp = nil) then // error we couldn't find the specified element */
    begin
      Result := nil;
      exit;
    end;

    // remove node from list */
    lpPrev^.QNext := lp^.QNext;
    if (lpPrev^.qnext = lpPrev) then
    begin MessageBeep(1) end;
    if (lp = gPlayQueue.last) then
    begin gPlayQueue.last := gPlayQueue.first end;
  end;

  lpXWH^.QNext := nil;
  Result := lpXWH;
end;

procedure DestroyPlayQueue;
begin
  while (gPlayQueue.first <> nil) do
  begin
    ReleaseWaveBlock(gPlayQueue.first);
    RemoveFromPlayingQueue(gPlayQueue.first);
  end;
end;

procedure ReleaseWaveDevice(pG: PGLOBALS);
begin
  if ((pG^.fActive = False) or (pG^.hWaveOut = 0)) then
  begin exit end;

  MyWaveOutReset(pG^.hWaveOut);
  DestroyPlayQueue;
  FreeWaveBlocks(pG^.hWaveOut);
  waveOutClose(pG^.hWaveOut);
  pG^.hWaveOut := 0;
  DestroyWindow(pG^.hWndApp);
  pG^.hWndApp := 0;
end;

procedure GetWaveDevice;
var
  uErr: Word;
begin
  if (g^.hWaveOut = 0) then
  begin
    g^.hWndApp := CreateWindow(Pointer(gszAppName), '', WS_DISABLED, 0, 0, 0, 0, 0,
      0, HInstance, nil);
    if (g^.hWndApp = 0) then
    begin raise EWaveMixError.Create('Failed to create callback window') end;

    uErr := waveOutOpen(PHWAVEOUT(@(g^.hWaveOut)), g^.wDeviceID,
      PWAVEFORMATEX(@(g^.pcm)), DWord(g^.hWndApp), DWord(nil),
      CALLBACK_WINDOW);
    if (uErr <> 0) then
    begin
      DestroyWindow(g^.hWndApp);
      g^.hWndApp := 0;
      raise EWaveMixError.Create('Failed to open waveform output device #'
        + IntToStr(uErr));
    end;

    try
      AllocWaveBlocks(g^.hWaveOut);
    except
      on Exception do
      begin
            // error msg already displayed in AllocWaveBlocks */
        waveOutClose(g^.hWaveOut);
        g^.hWaveOut := 0;
        DestroyWindow(g^.hWndApp);
        raise;
      end;
    end;
  end;
end;

procedure WaveMixActivate(hMixSession: THandle; fActivate: Boolean);
begin
  SessionToGlobalDataPtr(hMixSession);

  if (fActivate = True) then
  begin
    // Bugfix for Utopia:  don't allow the wave device to be removed from an application
    // that has already allocated it.  Instead return an Error.  This will permit the
    // application to release it when it is ready to. (what happens if an APP hangs while
    // it has the device allocated? Should probably reboot at that point anyway)
    //
    if (gActiveSession <> nil) then
    begin
      // the wavemixer has already beeen allocated to a device,
      // stealing it asynchrounously is dangerous and can cause
      // GP Faults.
      if (g <> gActiveSession) then
      begin raise EWaveMixError.Create('Resource is already allocated to a ' +
          'process') end
      else
      begin exit end;
    end;
    gActiveSession := g;

    // Maximize our chances of getting device by killing any current system sounds
    // e.g. SoundBits
    sndPlaySound(nil, SND_SYNC);

    GetWaveDevice;

    g^.fActive := TRUE;
    SetWaveOutPosition(g^.dwCurrentSample);
    while (MixerPlay(GetWaveBlock, True) = True) do
    begin end;
  end
  else
  begin
    if (g^.fActive = True) then
    begin g^.dwCurrentSample := MyWaveOutGetPosition(g^.hWaveOut, g^.fGoodGetPos) end;
    ReleaseWaveDevice(g);
    g^.fActive := FALSE;
    if (g = gActiveSession) then
    begin gActiveSession := nil end;
  end;
end;

procedure WaveMixOpenChannel(hMixSession: THandle; iChannel: Integer; dwFlags: DWord);
begin
  g := SessionToGlobalDataPtr(hMixSession);

  if (dwFlags > WMIX_OPENCOUNT) then
  begin raise EWaveMixError.Create('Invalid Flags') end;

  if ((dwFlags = WMIX_OPENSINGLE) and (iChannel >= MAXCHANNELS)) then
  begin raise EWaveMixError.Create('Invalid channel number') end;

  if ((dwFlags = WMIX_OPENCOUNT) and ((iChannel > MAXCHANNELS) or (iChannel < 1))) then
  begin raise EWaveMixError.Create('Invalid number of channels') end;

  case dwFlags of
    WMIX_OPENSINGLE:
    begin
      if (g^.aChannel[iChannel] <> PCHANNELNODE(-1)) then
      begin raise EWaveMixError.Create('The channel has already been opened') end;

      g^.aChannel[iChannel] := nil;
      Inc(g^.iChannels);
    end;

    WMIX_OPENALL:
    begin
      iChannel := MAXCHANNELS;
      while (iChannel > 0) do
      begin
        Dec(iChannel);
        if (g^.aChannel[iChannel] = PCHANNELNODE(-1)) then
        begin
          g^.aChannel[iChannel] := nil;
          Inc(g^.iChannels);
        end;
      end;
    end;

    WMIX_OPENCOUNT:
    begin
      while (iChannel > 0) do
      begin
        Dec(iChannel);
        if (g^.aChannel[iChannel] = PCHANNELNODE(-1)) then
        begin
          g^.aChannel[iChannel] := nil;
          Inc(g^.iChannels);
        end;
      end;
    end;

  else
  begin raise EWaveMixError.Create('Invalid flag') end;
  end;
end;

function AddToPlayingQueue(lpXWH: PXWAVEHDR): PXWAVEHDR;
begin
  // since add to the end of queue this node should not point to any other */
  lpXWH^.QNext := nil;

  // if queue is empty then both first and last pointers point to this node */
  if (gPlayQueue.first = nil) then
  begin
    gPlayQueue.first := lpXWH;
    gPlayQueue.last := lpXWH;
  end
  else // add to end of queue */
  begin
    gPlayQueue.last^.QNext := lpXWH;
    if (gPlayQueue.last^.QNext = gPlayQueue.last) then
    begin MessageBeep(1) end;
    gPlayQueue.last := lpXWH;
  end;
  Result := gPlayQueue.first;
end;

function MixerPlay(lpXWH: PXWAVEHDR; fWriteBlocks: Boolean): Boolean;
var
  i, j, uMaxChannel: DWord;
  lpDest: PSAMPLE;
  rgpCD: array [0..MAXCHANNELS - 1] of PCHANNELNODE;
  pCD: PCHANNELNODE;
  dwBlkCopyPos, dwOffset, dwSoonest, dwNumSamples, dwBlkEndCopyPos: DWord;
  //LPSAMPLE rgpCDdata[MAXCHANNELS];   // made static since assembly version was assuming ds
  rgpCDdata: array [0..MAXCHANNELS - 1] of PSAMPLE;
  wBytesToCopy: Word;
  dwSamplesRemaining: DWord;
begin
  if (lpXWH = nil) then
  begin
    Result := FALSE;
    exit;
  end;

  // setup array to point to only the channels that have data on them */
  dwSoonest := $7FFFFFFF;
  uMaxChannel := 0;
  for j := 0 to MAXCHANNELS - 1 do
  begin
    if (g^.aChannel[j] <> PCHANNELNODE(-1)) then
    begin
      // advance past waves already played: should at most be one or two traversels per channel */
      pCD := g^.aChannel[j];
      while ((pCD <> nil) and (pCD^.dwEndPos <= g^.dwCurrentSample)) do
      begin pCD := pCD^.next end;

      if (pCD = nil) then
      begin continue end;
      if (pCD^.dwStartPos < dwSoonest) then
      begin dwSoonest := pCD^.dwStartPos end;
      rgpCD[uMaxChannel] := pCD;
      Inc(uMaxChannel);
    end;
  end;

  {* BUGBUG: I tried playing silence when we were out of data to avoid
  **       activating the mute circuitry in Windows Sound System when
  **       sounds aren't playing, but I got a clicking sound so I took
  **       out the code to do that
  *}
  if (uMaxChannel = 0) then
  begin
    // BUGBUG: verify that this should happen here
    if (fWriteBlocks = True) then
    begin ReleaseWaveBlock(lpXWH) end;
    Result := False;
    exit;
  end;

  lpDest := PSAMPLE(lpXWH^.wh.lpData);
  wBytesToCopy := WORD(g^.dwWaveBlockLen);               // BUGBUG: wBytesToCopy should be wSamplesToCopy
  dwBlkCopyPos := g^.dwCurrentSample;

  while (wBytesToCopy > 0) do
  begin
    if (dwBlkCopyPos < dwSoonest) then // then have to fill the block with silence */
    begin
      if (dwBlkCopyPos + wBytesToCopy < dwSoonest) then
      begin dwNumSamples := wBytesToCopy end
      else
      begin dwNumSamples := dwSoonest - dwBlkCopyPos end;
      FillChar(lpDest^, dwNumSamples, SILENCE);
      Inc(lpDest, dwNumSamples);
      dwBlkCopyPos := dwBlkCopyPos + dwNumSamples;
      wBytesToCopy := wBytesToCopy - dwNumSamples;
      continue;
    end;

    // now know that some data starts here - find the shortest amount of data and set pointers to the data */
    // BUGBUG: have to deal with:
    //            x - waves that end during this block,
    //            x - waves that have data for the entire block,
    //            x - waves that start playing sometime in the future
    //            x - loops, combined with all of the above
    dwBlkEndCopyPos := dwBlkCopyPos + wBytesToCopy;

    {* j will be the index of the array used to contain pointers to actual wave data
    ** as well as the count of how many waves will be mixed
    *}
    j := 0;
    for i := 0 to uMaxChannel - 1 do
    begin
      if (rgpCD[i]^.dwStartPos > dwBlkCopyPos) then // file to play in the future (eg. queued waves)*/
      begin
        if (rgpCD[i]^.dwStartPos < dwBlkEndCopyPos) then
        begin dwBlkEndCopyPos := rgpCD[i]^.dwStartPos end;
        continue;  // no more checks needed on this channel */
      end;

      if (rgpCD[i]^.dwEndPos < dwBlkEndCopyPos) then // check for waves that end during this period */
      begin dwBlkEndCopyPos := rgpCD[i]^.dwEndPos end;

      dwOffset := dwBlkCopyPos - rgpCD[i]^.dwStartPos;  // find offset of current sample in the wave data */

      if (rgpCD[i]^.PlayParams.wLoops <> 0) then
      begin
        dwOffset := dwOffset mod rgpCD[i]^.dwNumSamples;  // unsigned eax div on 386 takes 38/41 clocks depend on reg or mem,
        // find the distance from the current offset until the end of actual wave data */
        dwSamplesRemaining := rgpCD[i]^.dwNumSamples - dwOffset;
        if (dwBlkCopyPos + dwSamplesRemaining < dwBlkEndCopyPos) then
        begin dwBlkEndCopyPos := dwBlkCopyPos + dwSamplesRemaining end;
      end;

      // now have valid data to mix, so setup the wave data pointer */
      rgpCDdata[j] := rgpCD[i]^.lpPos + dwOffset;
      Inc(j);
    end;

    if (j <> 0) then // then have data we want to mix now */
    begin
      // now mix the data! */
      dwNumSamples := dwBlkEndCopyPos - dwBlkCopyPos;
      cMixit(lpDest, rgpCDdata, j, WORD(dwNumSamples));
    end
    else
    begin continue end;

    // update variables that keep track of how much data we have left to copy */
    Inc(lpDest, dwNumSamples);
    wBytesToCopy := wBytesToCopy - WORD(dwNumSamples);
    dwBlkCopyPos := dwBlkCopyPos + dwNumSamples;
    dwSoonest := $7FFFFFFF;

    {* now go through the channels and skip by any we are done with
    ** note: we are only going through the channels that had data when we entered MixerPlay
    *}
    j := 0;
    while (j < uMaxChannel) do
    begin
      while (rgpCD[j] <> nil) do
      begin
        if (rgpCD[j]^.dwEndPos <= dwBlkCopyPos) then
        begin rgpCD[j] := rgpCD[j]^.next end
        else
        begin break end;
      end;

      if (rgpCD[j] = nil) then // remove this channel from the list */
      begin
        Dec(uMaxChannel);
        rgpCD[j] := rgpCD[uMaxChannel];

        if (uMaxChannel <> 0) then
        begin continue end  // recheck index j again since it now contains a different channel
        else
        begin break end; // no more valid data on any of the channels
      end;
      if (rgpCD[j]^.dwStartPos < dwSoonest) then
      begin dwSoonest := rgpCD[j]^.dwStartPos end;

      Inc(j);
    end;
  end;

  lpXWH^.dwWavePos := g^.dwCurrentSample;
  Inc(g^.dwCurrentSample, g^.dwWaveBlockLen div BYTESPERSAMPLE);

  if (fWriteBlocks = True) then
  begin
    AddToPlayingQueue(lpXWH);

    // now play the block */
    if (waveOutWrite(g^.hWaveOut, PWAVEHDR(lpXWH), sizeof(TWAVEHDR)) <> 0) then
    begin
      ReleaseWaveBlock(lpXWH);
      RemoveFromPlayingQueue(lpXWH);
      raise EWaveMixError.Create('Failed to write block to device');
    end;
  end;

  Result := True;
end;

procedure FreePlayedBlocks;
var
  i: Integer;
  dwPos: DWord;
  pCD: PCHANNELNODE;

begin
  // bugbug: need to use the waveOutGetPosition that is provided by the sound device
  //         otherwise it is possible that the position determined using timeGetTime()
  //         will be less than the length of the wave.  That can cause this function
  //         to think that the wave is not yet done and so it won't release it.  Since
  //         the multimedia device will not post anymore messages (it has already posted
  //         one for each block) we will never inform the user that the wave has finished
  //         playing. (ARCADE bug #268, #300, #301)
  dwPos := MyWaveOutGetPosition(g^.hWaveOut, TRUE);

  for i := 0 to MAXCHANNELS - 1 do
  begin
    pCD := g^.aChannel[i];
    if ((pCD = nil) or (pCD = PCHANNELNODE(-1))) then
    begin continue end;

    while ((pCD <> nil) and (dwPos >= pCD^.dwEndPos)) do
    begin
      g^.aChannel[i] := pCD^.next;

      if (pCD^.PlayParams.hWndNotify <> 0) then
      begin PostMessage(pCD^.PlayParams.hWndNotify, MM_WOM_DONE, i, LPARAM(pCD^.lpMixWave)) end;
      FreeChannelNode(pCD);
      pCD := g^.aChannel[i];
    end;
  end;
end;

procedure WaveMixPump;
var
  lpXWH: PXWAVEHDR;

begin
  // to fix bug where WaveMixPump GPFaults because app called WaveMixPump before
  // it called WaveMixActivate, or called WaveMixPump after WaveMixInit failed.
  // Second part of bugfix in WaveMixActivate()
  // (Actual hang is in FreePlayedBlocks)
  g := gActiveSession;
  if (g = nil) then
  begin exit end;

  // first go through and remove blocks that have the done bit set from playing queue */
  lpXWH := gPlayQueue.first;
  while (lpXWH <> nil) do
  begin
    if ((lpXWH^.wh.dwFlags and WHDR_DONE) <> 0) then
    begin
      RemoveFromPlayingQueue(lpXWH);
      ReleaseWaveBlock(lpXWH);
      lpXWH := gPlayQueue.first;  // need to reset after altering list to avoid potential problems
    end
    else
    begin lpXWH := lpXWH^.QNext end;
  end;

  // now free up channel blocks that have completed (ie. physically played) */
  FreePlayedBlocks;

  // if no data on any of the channels, reset the position tracking variables */
  //BUGBUG: implement this code; This code implemented in WaveMixPlay()
  while (MixerPlay(GetWaveBlock, TRUE)) do  // then fill up the queue again */
  begin end;
end;

function WndProc(hWnd: HWND; msg: DWord; wParam: Word; lParam: Longint): Longint; stdcall;
begin
  case msg of
    MM_WOM_DONE:
    begin
      if (PXWAVEHDR(lParam)^.g <> gActiveSession) then
      begin g := gActiveSession end;

            // mark the block as usable again and mix the next batch */
      WaveMixPump;
      Result := 0;
    end;
  else
  begin Result := DefWindowProc(hWnd, msg, wParam, lParam) end;
  end;
end;

{*
  this code will "Remix" the wave output without doing a waveOutReset.  This is necessary
  for systems in which the waveOutReset cause hardware clicks or mute circutry or whatever.
  The SLIMY version of this code remixes the sounds directly into the buffers that have
  already been submitted to waveOutWrite.  This is faster than just mixing when the next block
  becomes available, but it does not work so well because many sound cards suck the submitted
  data into a DMA buffer and so modifying the data in those buffers has no effect.  The NOSLIMY
  version works well when the waveblocks are short (eg. ~512-600 bytes) and there are few of them
  (eg <=3 blks) Doing this will cause blocks to be mixed and submitted often and so response time
  is good.
*}
procedure NoResetRemix(dwRemixSamplePos: DWord; pCD: PCHANNELNODE);
var
  dwOffset: DWord;

begin
  if (pCD <> nil) then // */
  begin
    dwOffset := g^.dwCurrentSample - pCD^.dwStartPos;
    pCD^.dwStartPos := pCD^.dwStartPos + dwOffset;
    pCD^.dwEndPos := pCD^.dwEndPos + dwOffset;
    if (pCD^.dwStartPos > pCD^.dwEndPos) then  // if we wrapped the end beyond last possible position, fix it!
    begin pCD^.dwEndPos := $7FFFFFFF end;
  end;

  WaveMixPump;
end;

procedure ResetRemix(dwRemixSamplePos: DWord; pCD: PCHANNELNODE);
var
  lpXWH: PXWAVEHDR;
  dwTmp: DWord;
begin
  g^.dwCurrentSample := dwRemixSamplePos;

  // destroy the old queue */
  DestroyPlayQueue;

  {* this is sort of slimy, but necessary for speed reasons:
  ** remix the waves into blocks that have been submitted to the wave driver
  ** BUGBUG: will this cause problems in NT?
  *}

  lpXWH := GetWaveBlock;
  while (lpXWH <> nil) do
  begin try
      if (MixerPlay(lpXWH, FALSE) = False) then
      begin
        ReleaseWaveBlock(lpXWH);
        break;
      end;
      AddToPlayingQueue(lpXWH);
    finally
      lpXWH := GetWaveBlock;
    end end;

  {* Now after all the mixing, reset the wave driver, resubmit the blocks and
  ** restart the wave driver
  ** Note: we must save the currentSample because myWaveOutReset will reset it
  **       and since we have already remixed the data we don't want to go through
  **       it again.
  *}
  dwTmp := g^.dwCurrentSample;
  MyWaveOutReset(g^.hWaveOut);
  g^.dwCurrentSample := dwTmp;  // need to reset since MyWaveOutReset will update it
  waveOutPause(g^.hWaveOut);

  // now submit all the blocks in the play queue*/
  lpXWH := gPlayQueue.first;
  while (lpXWH <> nil) do
  begin
    if (waveOutWrite(g^.hWaveOut, PWAVEHDR(lpXWH), sizeof(TWAVEHDR)) <> 0) then
    begin
      ReleaseWaveBlock(lpXWH);
      RemoveFromPlayingQueue(lpXWH);
      raise EWaveMixError.Create('Failed to write block to device');
    end;
    lpXWH := lpXWH^.QNext;
  end;

  waveOutRestart(g^.hWaveOut);
end;

procedure ResetWavePosIfNoChannelData;
var
  pCD: PCHANNELNODE;
  i: Integer;
begin
  // if the playqueue is empty we want to reset our global waveoutposition back to 0 so we
  // don't have to worry about the waveout position wrapping back to zero and messing up all
  // sounds still playing (this would take 13.5 hours of playing at 44.1khz to happen, but
  // we want to be as robust as possible.  However, we need to verify that all the channels
  // are empty too.  Otherwise resetting the waveoutpostion will cause these sounds to stay
  // on the channel and play again sometime in the future.

  if (gPlayQueue.first <> nil) then
  begin exit end;

  for i := 0 to MAXCHANNELS - 1 do
  begin
    pCD := g^.aChannel[i];
    if ((pCd <> nil) and (pCD <> PCHANNELNODE(-1))) then
    begin exit end;
  end;

  SetWaveOutPosition(0);
end;

procedure WaveMixPlay(lpMixPlayParams: PMIXPLAYPARAMS);
var
  pTmp, pCD: PCHANNELNODE;
  dwWavePos: DWord;
  iChannel: Integer;
  fRemix, fPause: Boolean;
  iLRU: Integer;
  pCD1: PCHANNELNODE;
begin
  fRemix := FALSE;
  fPause := FALSE;

  if (lpMixPlayParams = nil) then
  begin raise EWaveMixError.Create('nil parameters pointer passed to WaveMixPlay') end;

  g := SessionToGlobalDataPtr(lpMixPlayParams^.hMixSession);

  if (lpMixPlayParams^.lpMixWave = nil) then
  begin raise EWaveMixError.Create('nil wave pointer passed to WaveMixPlay!') end;

  if (g^.fActive = False) then
  begin raise EWaveMixError.Create('Wave device not allocated, call '
      + 'WaveMixActivate(hMixSession,TRUE)') end;

  if (g^.iChannels = 0) then
  begin raise EWaveMixError.Create('You must open a channel before you can play '
      + 'a wave') end;

  if ((lpMixPlayParams^.dwFlags and WMIX_USELRUCHANNEL) <> 0) then
  begin
    // find the lease recently used channel */
    iLRU := 0;
    for iChannel := 0 to MAXCHANNELS - 1 do
    begin
      if (g^.aChannel[iChannel] = PCHANNELNODE(-1)) then
      begin continue end;

      if (g^.aChannel[iLRU] = nil) then // we found an empty channel go ahead and use it.
      begin break end;

      if ((iChannel <> iLRU) and (g^.MRUChannel[iChannel] < g^.MRUChannel[iLRU])) then
      begin iLRU := iChannel end;
    end;

    iChannel := iLRU;
    lpMixPlayParams^.iChannel := iChannel;  // return channel back to calling app so can use other APIs
  end
  else
  begin iChannel := lpMixPlayParams^.iChannel end;

  Inc(g^.dwMRU);
  g^.MRUChannel[iChannel] := g^.dwMRU;

  if (g^.aChannel[iChannel] = PCHANNELNODE(-1)) then
  begin raise EWaveMixError.Create('The specified channel was not open') end;

  pCD := GetChannelNode;
  if (pCD = nil) then
  begin raise EWaveMixError.Create('Not enough memory') end;

  Move(lpMixPlayParams^, pCD^.PlayParams, sizeof(TMIXPLAYPARAMS));
  pCD^.lpMixWave := pCD^.PlayParams.lpMixWave;
  pCD^.dwNumSamples := pCD^.lpMixWave^.wh.dwBufferLength div BYTESPERSAMPLE;
  pCD^.lpPos := PSAMPLE(pCD^.lpMixWave^.wh.lpData);
  pCD^.lpEnd := pCD^.lpPos + pCD^.dwNumSamples - 1;
  pCD^.PlayParams.iChannel := iChannel;

  // add to end of play wait list */
  if (g^.pWaitList <> nil) then
  begin
    pCD^.next := g^.pWaitList^.next;
    g^.pWaitList^.next := pCD;
    g^.pWaitList := pCD;
  end
  else
  begin
    g^.pWaitList := pCD;
    pCD^.next := pCD;
  end;

  if ((pCD^.PlayParams.dwFlags and WMIX_WAIT) <> 0) then
  begin exit end;

  {* if there is no sound current playing reset our wave position counters
  ** to avoid potential overflows.
  *}
  ResetWavePosIfNoChannelData;

  dwWavePos := MyWaveOutGetPosition(g^.hWaveOut, g^.fGoodGetPos);

  while (g^.pWaitList <> nil) do
  begin
    // remove the first node from the list */
    pCD := g^.pWaitList^.next;
    if (pCD = g^.pWaitList) then // then was the only one in list */
    begin g^.pWaitList := nil end
    else
    begin g^.pWaitList^.next := pCD^.next end;
    pCD^.next := nil;

    iChannel := pCD^.PlayParams.iChannel;

    if ((pCD^.PlayParams.dwFlags and WMIX_CLEARQUEUE) <> 0) then
    begin
      // replace the stuff in the old channel with this new wave */
      pCD1 := g^.aChannel[iChannel];
      while (pCD1 <> nil) do
      begin
        pTmp := pCD1^.next;
        FreeChannelNode(pCD1);
        pCD1 := pTmp;
      end;
      g^.aChannel[iChannel] := pCD;

      // if there was already sound playing we must remix the output */
      if (gPlayQueue.first <> nil) then
      begin fRemix := TRUE end;

      if ((pCD^.PlayParams.dwFlags and WMIX_HIGHPRIORITY) <> 0) then
      begin pCD^.dwStartPos := dwWavePos end
      else
      begin pCD^.dwStartPos := g^.dwCurrentSample end;
    end
    else // queue the wave on this channel */
    begin
      if (g^.aChannel[iChannel] <> nil) then
      begin
        pTmp := g^.aChannel[iChannel];
        while (pTmp^.Next <> nil) do
        begin pTmp := pTmp^.Next end;
        pTmp^.next := pCD;

        if ((pCD^.PlayParams.dwFlags and WMIX_HIGHPRIORITY) <> 0) then
        begin pCD^.dwStartPos := pTmp^.dwEndPos end  // start pos = end pos of previous in queue: don't do +1 */
        else
        if (g^.dwCurrentSample > pTmp^.dwEndPos) then
        begin pCd^.dwStartPos := g^.dwCurrentSample end
        else                            // or MixerPlay will need to handle a single sample copy */
        begin pCd^.dwStartPos := pTmp^.dwEndPos end;
      end
      else // queue wave onto an empty channel */
      begin
        g^.aChannel[iChannel] := pCD;
        if ((pCD^.PlayParams.dwFlags and WMIX_HIGHPRIORITY) <> 0) then
        begin pCD^.dwStartPos := dwWavePos end
        else
        begin pCD^.dwStartPos := g^.dwCurrentSample end;
      end;

      if (g^.dwCurrentSample > pCD^.dwStartPos) then
      begin fRemix := TRUE end;
    end;

    if (pCD^.PlayParams.wLoops = $FFFF) or (pCD^.dwStartPos = $7FFFFFFF) then
    begin pCD^.dwEndPos := $7FFFFFFF end
    else
    begin pCD^.dwEndPos := pCD^.dwStartPos + (pCD^.PlayParams.wLoops + 1) * pCD^.dwNumSamples - 1 end;

  end; // while (g->pWaitList) */

  if (fRemix = True) then
  begin g^.pfnRemix(pCD^.dwStartPos, pCD) end
  else
  begin
    {* if we are not already playing we should pause before submitting data or we
    ** may not be able to do it fast enough and will cause a hicup
    *}
    if (gPlayQueue.first = nil) then
    begin
      fPause := TRUE;
      waveOutPause(g^.hWaveOut);
    end;
    while (MixerPlay(GetWaveBlock, TRUE) = True) do
    begin end;

    if (fPause = True) then
    begin waveOutRestart(g^.hWaveOut) end;
  end;
end;

procedure WaveMixFlushChannel(hMixSession: THandle; iChannel: Integer; dwFlags: DWord);
var
  pCD, pTmp: PCHANNELNODE;
  iLast: Integer;
  fRemix: Boolean; // don't remix if that channel didn't have data on it. */
begin
  fRemix := False;

  g := SessionToGlobalDataPtr(hMixSession);

  if ((dwFlags and WMIX_ALL) <> 0) then
  begin
    iChannel := 0;
    iLast := MAXCHANNELS;
  end
  else
  begin
    if ((iChannel < 0) or (iChannel >= MAXCHANNELS)) then
    begin raise EWaveMixError.Create('Invalid channel number') end;

    if (g^.aChannel[iChannel] = PCHANNELNODE(-1)) then
    begin raise EWaveMixError.Create('The specified channel was not open') end;

    iLast := iChannel + 1;
  end;

  Dec(iChannel);
  while (iChannel < iLast - 1) do
  begin
    Inc(iChannel);

    pCD := g^.aChannel[iChannel];
    if (pCD = PCHANNELNODE(-1)) then
    begin continue end;
    g^.aChannel[iChannel] := nil;

    while (pCD <> nil) do
    begin
      pTmp := pCD^.next;
      FreeChannelNode(pCD);
      pCD := pTmp;
      fRemix := TRUE;
    end;
  end;

  if ((fRemix = True) and ((dwFlags and WMIX_NOREMIX) = 0) and (g^.fActive = True)) then
  begin g^.pfnRemix(MyWaveOutGetPosition(g^.hWaveOut, g^.fGoodGetPos), nil) end;
end;

procedure WaveMixCloseChannel(hMixSession: THandle; iChannel: Integer; dwFlags: DWord);
var
  iLast: Integer;
begin
  g := SessionToGlobalDataPtr(hMixSession);

  {* flush the channel and let WaveMixFlushChannel do all the
  ** error checking for us
  *}
  WaveMixFlushChannel(hMixSession, iChannel, dwFlags or WMIX_NOREMIX);

  if ((dwFlags and WMIX_ALL) <> 0) then
  begin
    iChannel := 0;
    iLast := MAXCHANNELS;
  end
  else
  begin iLast := iChannel + 1 end;

  while (iChannel < iLast) do
  begin
    if (g^.aChannel[iChannel] <> PCHANNELNODE(-1)) then
    begin
      g^.aChannel[iChannel] := PCHANNELNODE(-1);
      Dec(g^.iChannels);
    end;
    Inc(iChannel);
  end;
end;

procedure WaveMixFreeWave(hMixSession: THandle; lpMixWave: PMIXWAVE);
var
  i: Integer;
  pCD, pPrev: PCHANNELNODE;
  g: PGLOBALS;
begin
  g := SessionToGlobalDataPtr(hMixSession);

  if (lpMixWave = nil) then
  begin raise EWaveMixError.Create('The given pointer was not valid') end;

  for i := 0 to MAXCHANNELS - 1 do
  begin
    if (g^.aChannel[i] = PCHANNELNODE(-1)) then
    begin continue end;

    pPrev := nil;
    pCD := g^.aChannel[i];

    while (pCD <> nil) do
    begin
      if (pCD^.lpMixWave = lpMixWave) then
      begin
        if (pPrev = nil) then // then at head of list */
        begin
          pCD := pCD^.next;
          FreeChannelNode(g^.aChannel[i]);
          g^.aChannel[i] := pCD;
        end
        else
        begin
          pPrev^.next := pCD^.next;
          FreeChannelNode(pCD);
          pCD := pPrev^.next;
        end;
      end
      else
      begin
        pPrev := pCD;
        pCD := pCD^.next;
      end;
    end;
  end;

  if (lpMixWave^.wh.lpData <> nil) then
  begin GlobalFreePtr(lpMixWave^.wh.lpData) end;
  GlobalFreePtr(lpMixWave);
end;

// dwNumSamples should be double if the file is stereo */
function BitsPerSampleAlign(lpInData: PChar; nInBPS: Word; nOutBPS: Word; dwDataSize: PDWord): PChar;
var
  lpOutData, lpB: PChar;
  lpW: HPINT;
  dwNumSamples: DWord;
  nInBytesPerSample, nOutBytesPerSample: Word;
begin
  if (nInBPS = nOutBPS) then
  begin
    Result := lpInData;
    exit;
  end;

  // if not 8 or 16 then ADPCM (compressed) format which we don't support */
  if (((nInBPS <> 8) and (nInBPS <> 16)) or ((nOutBPS <> 8) and (nOutBPS <> 16))) then
  begin
    GlobalFreePtr(lpInData);
    raise EWaveMixError.Create('File format not supported');
  end;

  nInBytesPerSample := nInBPS div BITSPERBYTE;
  nOutBytesPerSample := nOutBPS div BITSPERBYTE;
  dwNumSamples := dwDataSize^ div nInBytesPerSample;
  dwDataSize^ := dwNumSamples * nOutBytesPerSample;

  lpOutData := GlobalAllocPtr(GMEM_MOVEABLE or GMEM_SHARE or GMEM_NODISCARD, dwDataSize^);
  if (lpOutData = nil) then
  begin
    GlobalFreePtr(lpInData);
    raise EWaveMixError.Create(gszMemError);
  end;

  if (nInBytesPerSample > nOutBytesPerSample) then // convert from 16 bit to eight bit */
  begin
    lpB := lpOutData;
    lpW := HPINT(lpInData);
    While (dwNumSamples > 0) do
    begin
      lpB^ := chr(Integer(lpW^) div 256 + 128);
      Inc(lpW);
      Inc(lpB);
      Dec(dwNumSamples);
    end;
  end
  else // convert from 8 bit to 16 bit */
  begin
    lpB := lpInData;
    lpW := HPINT(lpOutData);
    while (dwNumSamples > 0) do
    begin
      lpW^ := (Shortint(lpB^) - 128) * 256;
      Inc(lpB);
      Inc(lpW);
      Dec(dwNumSamples);
    end;
  end;

  GlobalFreePtr(lpInData);
  Result := lpOutData;
end;

function ChannelAlign(lpInData: PChar; nInChannels: Word; nOutChannels: Word; nBytesPerSample: Word; dwDataSize: PDWord): PChar;
var
  lpOutData: PSAMPLE;
  lpB1, lpB2: PSAMPLE;
  lpW1, lpW2: HPINT;
  dwNumSamples: DWord;
begin
  if (nInChannels = nOutChannels) then
  begin
    Result := lpInData;
    exit;
  end;

  dwNumSamples := dwDataSize^ div nBytesPerSample div nInChannels;
  dwDataSize^ := dwNumSamples * nBytesPerSample * nOutChannels;

  lpOutData := GlobalAllocPtr(GMEM_MOVEABLE or GMEM_SHARE or GMEM_NODISCARD, dwDataSize^);
  if (lpOutData = nil) then
  begin
    GlobalFreePtr(lpInData);
    raise EWaveMixError.Create(gszMemError);
  end;

  // BUGBUG: this should really be more general to allow you to convert from stereo to 4 channel */
  if (nInChannels < nOutChannels) then // convert from mono to stereo */
  begin
    if (nBytesPerSample = 1) then  // eight bit */
    begin
      lpB1 := lpInData;
      lpB2 := lpOutData;
      while (dwNumSamples > 0) do
      begin
        lpB2^ := lpB1^;
        Inc(lpB2);
        lpB2^ := lpB1^;
        Inc(lpB2);
        Inc(lpB1);
        Dec(dwNumSamples);
      end;
    end
    else
    begin
      lpW1 := HPINT(lpInData);
      lpW2 := HPINT(lpOutData);
      while (dwNumSamples > 0) do
      begin
        lpW2^ := lpW1^;
        Inc(lpW2);
        lpW2^ := lpW1^;
        Inc(lpW2);
        Inc(lpW1);
        Dec(dwNumSamples);
      end;
    end;
  end
  else // convert from stereo to mono */
  begin
    if (nBytesPerSample = 1) then  // eight bit */
    begin
      lpB1 := PSAMPLE(lpInData);
      lpB2 := lpOutData;
      while (dwNumSamples > 0) do
      begin
        // mix the two channels */
        // note: lpB1 and lpB2 must be pointers to unsigned chars or normalized
        // numbers above 0 (eg. 140) are negative numbers and so the averaging
        // gets confused
        lpB2^ := Chr((Integer(lpB1^) + Integer((lpB1 + 1)^)) div 2);
        Inc(lpB2);
        Inc(lpB1, 2);
        Dec(dwNumSamples);
      end;
    end
    else // 16 bit */
    begin
      lpW1 := HPINT(lpInData);
      lpW2 := HPINT(lpOutData);
      while (dwNumSamples > 0) do
      begin
        lpW2^ := Integer(Longint(lpW1^) + Longint(HPINT(PChar(lpW1) + 2)^)) div 2;
        Inc(lpW2);
        Inc(lpW1, 2);
        Dec(dwNumSamples);
      end;
    end;
  end;

  GlobalFreePtr(lpInData);
  Result := lpOutData;
end;

procedure AvgSample(lpOutData, lpInData: PChar; nSkip, nBytesPerSample, nChannels: Integer);
var
  lpTmp: PChar;
  i, j: Integer;
  lpWOutData: HPINT;
  lpWInData: HPINT;
  lpWTmp: HPINT;
  sum: Longint;
begin
  if (nBytesPerSample = 1) then // 8 bit samples
  begin
    for i := 0 to nChannels - 1 do
    begin
      lpTmp := lpInData;
      Inc(lpInData);
      sum := 0;
      for j := 0 to nSkip - 1 do
      begin
        Inc(sum, BYTE(lpTmp^) - 128);
        Inc(lpTmp, nChannels);
      end;
      sum := sum div nSkip;
      lpOutData^ := Chr(sum + 128);
      Inc(lpOutData);
    end;
  end
  else // 16 bit samples
  begin
    lpWOutData := HPINT(lpOutData);
    lpWInData := HPINT(lpInData);

    for i := 0 to nChannels - 1 do
    begin
      lpWTmp := lpWInData;
      Inc(lpWInData);
      sum := 0;
      for j := 0 to nSkip - 1 do
      begin
        Inc(sum, lpWTmp^);
        Inc(lpWTmp, nChannels);
      end;
      sum := sum div nSkip;
      lpWOutData^ := Integer(sum);
    end;
  end;
end;

// this routine interpolates the samples along the different channels
procedure RepSample(lpOutData, lpInData: PChar; nRep, nBytesPerSample, nChannels: Integer);
var
  lpTmp: PChar;
  diff, val: Char;
  i, j:  Integer;
  lpWIn, lpWOut, lpWTmp: HPINT;
  diffw, valw: Integer;
begin
  if (nBytesPerSample = 1) then  // 8 bit samples
  begin
    // bugbug: this code is supposed to interpolate the samples
    //         that it is going to be
    lpTmp := lpOutData;

    for i := 0 to nChannels - 1 do
    begin
      lpOutData := lpTmp;
      Inc(lpTmp);
      diff := Chr((BYTE((lpInData + nChannels)^) - BYTE(lpInData^)) div nRep);
      //diff = 0;  // hack to remove interpolation
      val := lpInData^;
      lpOutData^ := lpInData^;
      Inc(lpOutData, nChannels);

      for j := 1 to nRep - 1 do
      begin
        val := Chr(Ord(val) + Ord(diff));
        lpOutData^ := val;
        Inc(lpOutData, nChannels);
      end;

      Inc(lpInData);
    end;
  end
  else // 16 bit samples
  begin
    lpWIn := HPINT(lpInData);
    lpWOut := HPINT(lpOutData);
    lpWTmp := lpWOut;

    for i := 0 to nChannels - 1 do
    begin
      lpWOut := lpWTmp;
      Inc(lpWTmp);
      diffw := (HPINT(PChar(lpWOut) + nChannels * 2)^ - lpWIn^) div nRep;
      valw := lpWIn^;
      lpWOut^ := lpWIn^;
      Inc(lpWOut, nChannels);

      for j := 1 to nRep - 1 do
      begin
        Inc(valw, diffw);
        lpWOut^ := valw;
        Inc(lpWOut, nChannels);
      end;
      Inc(lpWIn);
    end;
  end;
end;

function SamplesPerSecAlign(lpInData: PChar; nInSamplesPerSec, nOutSamplesPerSec: DWord; nBytesPerSample, nChannels: Word; dwDataSize: PDWord): PChar;
var
  nRep, nSkip, i, n: Integer;
  SampleSize: DWord;
  dwNumSamples, dwNewNumSamples, dw: DWord;
  lpOutData, lpOutSave, lpInSave: PSAMPLE;
  lpTmp: PSAMPLE;
begin
  if (nInSamplesPerSec = nOutSamplesPerSec) then
  begin
    Result := lpInData;
    exit;
  end;

  SampleSize := nBytesPerSample * nChannels;
  dwNumSamples := dwDataSize^ div SampleSize;

  if (nOutSamplesPerSec > nInSamplesPerSec) then
  begin
    // then need to add in extra samples
    nRep := Integer(nOutSamplesPerSec div nInSamplesPerSec);
    nSkip := 0;
    dwNewNumSamples := dwNumSamples * nRep;
  end
  else  // replace the sample with the average of nSkip samples
  begin
    nRep := 0;
    nSkip := Integer((nInSamplesPerSec + (nOutSamplesPerSec div 2))
      div nOutSamplesPerSec);
    dwNewNumSamples := dwNumSamples div nSkip;
  end;

  dwDataSize^ := dwNewNumSamples * SampleSize;

  lpOutData := GlobalAllocPtr(GMEM_MOVEABLE or GMEM_SHARE or GMEM_NODISCARD, Longint(dwDataSize^));
  if (lpOutData = nil) then
  begin
    GlobalFreePtr(lpInData);
    raise EWaveMixError.Create(gszMemError);
  end;

  lpInSave := lpInData;
  lpOutSave := lpOutData;

  if (nRep > 0) then
  begin
    dw := dwNumSamples - 1;
    while (dw > 0) do
    begin
      RepSample(lpOutData, lpInData, nRep, nBytesPerSample, nChannels);  // this routine should interpolate the samples
      lpOutData := lpOutData + nRep * SampleSize;
      lpInData := lpInData + SampleSize;
      Dec(dw);
    end;
    // up sample last sample without filtering */
    for n := 0 to nRep - 1 do
    begin
      lpTmp := lpInData;
      for i := 0 to SampleSize - 1 do
      begin
        lpOutData^ := lpTmp^;
        Inc(lpOutData);
        Inc(lpTmp);
      end;
    end;
  end
  else
  begin
    dw := dwNewNumSamples - 1;
    while (dw > 0) do
    begin
      AvgSample(lpOutData, lpInData, nSkip, nBytesPerSample, nChannels);
      lpOutData := lpOutData + SampleSize;
      lpInData := lpInData + nSkip * SampleSize;
      Dec(dw);
    end;
    // just copy the last sample
    for i := 0 to SampleSize - 1 do
    begin
      lpOutData^ := lpInData^;
      Inc(lpOutData);
      Inc(lpInData);
    end;
  end;

  GlobalFreePtr(lpInSave);
  Result := lpOutSave;
end;

function WaveFormatConvert(lpOutWF, lpInWF: PPCMWAVEFORMAT; lpInData: PChar; dwDataSize: PDWord): PChar;
begin
  // if wave formats are the same just return the input buffer */
  if ((lpInWF^.wf.nChannels = lpOutWF^.wf.nChannels) and
    (lpInWF^.wf.nSamplesPerSec = lpOutWF^.wf.nSamplesPerSec) and
    (lpInWF^.wBitsPerSample = lpOutWF^.wBitsPerSample)) then
  begin
    Result := lpInData;
    exit;
  end;

  // block align the data, eg. convert from 16 bit samples to eight bit samples or vice versa */
  lpInData := BitsPerSampleAlign(lpInData, lpInWF^.wBitsPerSample, lpOutWF^.wBitsPerSample, dwDataSize);
  if (lpInData = nil) then
  begin
    Result := nil;
    exit;
  end;

  // channel align the data, e.g. convert from stereo to mono or vice versa */
  lpInData := ChannelAlign(lpInData, lpInWF^.wf.nChannels, lpOutWF^.wf.nChannels, lpOutWF^.wBitsPerSample div BITSPERBYTE, dwDataSize);
  if (lpInData = nil) then
  begin
    Result := nil;
    exit;
  end;

  // SamplesPerSec align the data, e.g. convert from 44.1kHz to 11.025 kHz or vice versa*/
  lpInData := SamplesPerSecAlign(lpInData, lpInWF^.wf.nSamplesPerSec, lpOutWF^.wf.nSamplesPerSec, lpOutWF^.wBitsPerSample div BITSPERBYTE, lpOutWF^.wf.nChannels, dwDataSize);
  if (lpInData = nil) then
  begin
    Result := nil;
    exit;
  end;

  Result := lpInData;
end;

function WaveMixOpenWave(hMixSession: THandle; szWaveFilename: PChar; hInst: THandle; dwFlags: DWord): PMixWave;
var
  mmckinfoParent: TMMCKINFO;
  mmckinfoSubchunk: TMMCKINFO;
  dwDataSize: DWord;
  hWaveOutTmp: HWAVEOUT;
  hdmmio: HMMIO;
  lpMix:  PMIXWAVE;
  lpData: PChar;
  wDeviceID: UInt;
  hdRsrc: HRSRC;
begin
  hdmmio := 0;
  lpData := nil;

  g := SessionToGlobalDataPtr(hMixSession);
  if (g <> nil) then
  begin
    wDeviceID := g^.wDeviceID
  end
  else
  begin
    wDeviceID := WAVE_MAPPER
  end;

  waveOutOpen(@hWaveOutTmp, wDeviceID, PWAVEFORMATEX(@(g^.pcm)), DWord(nil), 0, WAVE_FORMAT_QUERY);
  lpMix := PMIXWAVE(GlobalAllocPtr(GMEM_FIXED or GMEM_ZEROINIT or GMEM_NODISCARD, sizeof(TMIXWAVE)));
  hdmmio := mmioOpen(szWaveFilename, nil, MMIO_READ or MMIO_ALLOCBUF);
  mmckinfoParent.fccType := mmioFOURCC('W', 'A', 'V', 'E');
  mmckinfoSubchunk.ckid := mmioFOURCC('f', 'm', 't', ' ');
  mmioDescend(hdmmio, PMMCKINFO(@mmckinfoParent), nil, MMIO_FINDRIFF);
  mmioDescend(hdmmio, @mmckinfoSubchunk, @mmckinfoParent, MMIO_FINDCHUNK);
  mmioRead(hdmmio, PChar(@(lpMix^.pcm)), sizeof(TPCMWAVEFORMAT));
  mmioAscend(hdmmio, @mmckinfoSubchunk, 0);
  mmckinfoSubchunk.ckid := mmioFOURCC('d', 'a', 't', 'a');
  mmioDescend(hdmmio, @mmckinfoSubchunk, @mmckinfoParent, MMIO_FINDCHUNK);
  dwDataSize := mmckinfoSubchunk.cksize;
  lpData := GlobalAllocPtr(GMEM_MOVEABLE or GMEM_SHARE or GMEM_NODISCARD, dwDataSize);
  mmioRead(hdmmio, PChar(lpData), dwDataSize);
  lpData := WaveFormatConvert(@(g^.pcm), @(lpMix^.pcm), lpData, @dwDataSize);
  mmioClose(hdmmio, 0);
  lpMix^.wh.lpData := lpData;
  lpMix^.wh.dwBufferLength := dwDataSize;
  lpMix^.wh.dwFlags := 0;
  lpMix^.wh.dwLoops := 0;
  lpMix^.wh.dwUser := 0;
  StrLCopy(lpMix^.szWaveFilename, szWaveFilename, MAXFILENAME);
  Result := lpMix;
end;

function FigureOutDMABufferSize(iDefBufferSize: Integer; lpWaveFormat: PWAVEFORMAT): DWord;
var
  dwDMALen: DWord;
begin
  // if len has been set in ini file for this device - use it */
  dwDMALen := DWord(GetPrivateProfileInt(g^.szDevicePName, 'WaveBlockLen', 0, gszIniFile));
  if (dwDMALen <> 0) then
  begin
    if (dwDMALen < MINWAVEBLOCKLEN) then
    begin dwDMALen := MINWAVEBLOCKLEN end
    else
    if (dwDMALen > MAXWAVEBLOCKLEN) then
    begin dwDMALen := MAXWAVEBLOCKLEN end;
    Result := dwDMALen;
    exit;
  end;

  // if len has been set in ini file for the default device - use it */
  dwDMALen := DWord(iDefBufferSize);
  if (dwDMALen <> 0) then
  begin
    if (dwDMALen < MINWAVEBLOCKLEN) then
    begin dwDMALen := MINWAVEBLOCKLEN end
    else
    if (dwDMALen > MAXWAVEBLOCKLEN) then
    begin dwDMALen := MAXWAVEBLOCKLEN end;
    Result := dwDMALen;
    exit;
  end;

  {* if we are running on NT don't want to use our technique of flooding the wave output to
  ** try to figure out the DMA size since the way NT protects the hardware makes this inaccurate
  *}
  Result := DEFAULT_NTWAVEBLOCKLEN * (lpWaveFormat^.nSamplesPerSec div 11025);
end;

function WaveMixConfigureInit(lpConfig: PMIXCONFIG): THandle;
var
  caps: TWAVEOUTCAPS;
  i, iDevices: Integer;
  u: DWord;
  iDefRemix, iDefGoodWavePos, iDefWaveBlocks, iDefWaveBlockLen: Integer;
  iDefSamplesPerSec, iSamplesPerSec: Integer;
begin
  InitLibrary;

   // checks for previous initialization
  if (g <> nil) then
  begin raise EWaveMixError.Create('Session already initialized') end;

  // if a size is passed in which we don't understand, then don't use the passed in info
  // it may have been created with another version
  if ((lpConfig <> nil) and (lpConfig^.wSize <> sizeof(TMIXCONFIG))) then
  begin lpConfig := nil end;

  iDevices := waveOutGetNumDevs;
  if (iDevices = 0) then
  begin raise EWaveMixError.Create('No wave output devices available') end;

  g := PGLOBALS(LocalAlloc(LPTR, sizeof(GLOBALS)));
  if (g = nil) then
  begin raise EWaveMixError.Create(gszMemError) end;

  g^.wMagic1 := MAGICNO;
  g^.wMagic2 := MAGICNO;

  for i := 0 to MAXCHANNELS - 1 do
  begin g^.aChannel[i] := PCHANNELNODE(-1) end;

  Move(gpFormat, g^.pcm, sizeof(TPCMWAVEFORMAT));  // default wave format */

  iDefRemix := GetPrivateProfileInt(Pointer(gszDefault), 'Remix', DEFAULT_REMIXALGORITHM, gszIniFile);
  iDefGoodWavePos := GetPrivateProfileInt(Pointer(gszDefault), 'GoodWavePos', DEFAULT_GOODWAVPOS, gszIniFile);
  iDefWaveBlocks := GetPrivateProfileInt(Pointer(gszDefault), 'WaveBlocks', DEFAULT_WAVEBLOCKS, gszIniFile);
  iDefWaveBlockLen := GetPrivateProfileInt(Pointer(gszDefault), 'WaveBlockLen', DEFAULT_WAVEBLOCKLEN, gszIniFile) and $FFFC; // force DWord align
  iDefSamplesPerSec := GetPrivateProfileInt(Pointer(gszDefault), 'SamplesPerSec', DEFAULT_SAMPLESPERSEC, gszIniFile);


  StrCopy(g^.szDevicePName, Pointer(gszDefault));
  g^.wDeviceID := DWord(GetPrivateProfileInt('general', 'WaveOutDevice', WAVE_MAPPER, gszIniFile));
  if (g^.wDeviceID >= DWord(iDevices)) then // make sure we don't use an invalid ID number
  begin g^.wDeviceID := DWord(WAVE_MAPPER) end;

  // BUGBUG: sometimes this function returns an error even if there is a device installed
  //         so just act as if I don't know what card it is and don't worry about it.
  u := waveOutGetDevCaps(g^.wDeviceID, @caps, sizeof(TWAVEOUTCAPS));
  if (u <> 0) then
  begin StrCopy(caps.szPname, 'Unknown Device') end;

  if ((caps.dwSupport and WAVECAPS_SYNC) <> 0) then
  begin
    LocalFree(HLOCAL(g));
    g := nil;
    raise EWaveMixError.Create(caps.szPname + ' is a syncronous (blocking) '
      + 'wave output device.  This will not permit audio to play while other '
      + 'applications are running');
  end;

  if (GetPrivateProfileInt('not compatible', caps.szPname, 0, gszIniFile) <> 0) then
  begin
    LocalFree(HLOCAL(g));
    g := nil;
    raise EWaveMixError.Create(caps.szPname + ' is not compatible with '
      + 'the realtime wavemixer');
  end;

  StrCopy(g^.szDevicePName, caps.szPname);

  if (GetPrivateProfileInt(g^.szDevicePName, 'Remix', iDefRemix, gszIniFile) = 2) then
  begin g^.pfnRemix := NoResetRemix end
  else
  begin g^.pfnRemix := ResetRemix end;

  g^.fGoodGetPos := Boolean(GetPrivateProfileInt(g^.szDevicePName, 'GoodWavePos', iDefGoodWavePos, gszIniFile));
  g^.iNumWaveBlocks := GetPrivateProfileInt(g^.szDevicePName, 'WaveBlocks', iDefWaveBlocks, gszIniFile);
  if (g^.iNumWaveBlocks < MINWAVEBLOCKS) then
  begin g^.iNumWaveBlocks := MINWAVEBLOCKS end
  else
  if (g^.iNumWaveBlocks > MAXWAVEBLOCKS) then
  begin g^.iNumWaveBlocks := MAXWAVEBLOCKS end;

  if ((lpConfig <> nil) and ((lpConfig^.dwFlags and WMIX_CONFIG_CHANNELS) <> 0)) then
  begin
    if ((lpConfig^.wChannels > 1) and (caps.wChannels > 1)) then
    begin
      g^.pcm.wf.nChannels := 2;
      g^.pcm.wf.nBlockAlign := 2;  // number of bytes for one sample including both channels
    end;
  end;

  iSamplesPerSec := GetPrivateProfileInt(g^.szDevicePName, 'SamplesPerSec', iDefSamplesPerSec, gszIniFile);

  // application wants to play at a specific sampling rate */
  if ((lpConfig <> nil) and ((lpConfig^.dwFlags and WMIX_CONFIG_SAMPLINGRATE) <> 0)) then
  begin
    case lpConfig^.wSamplingRate of
      11:
      begin iSamplesPerSec := 11 end;
      22:
      begin iSamplesPerSec := 22 end;
      44:
      begin iSamplesPerSec := 44 end;
    end;
  end;

  // adjust the wave format if the sampling rate is no longer set to the default */
  case iSamplesPerSec of
    11:
    begin g^.pcm.wf.nAvgBytesPerSec := 11025 * BYTESPERSAMPLE * g^.pcm.wf.nChannels end;
    22:
    begin
      g^.pcm.wf.nSamplesPerSec := 22050;
      g^.pcm.wf.nAvgBytesPerSec := 22050 * BYTESPERSAMPLE * g^.pcm.wf.nChannels;
    end;
    44:
    begin
      g^.pcm.wf.nSamplesPerSec := 44100;
      g^.pcm.wf.nAvgBytesPerSec := 44100 * BYTESPERSAMPLE * g^.pcm.wf.nChannels;
    end;
  else
  begin g^.pcm.wf.nAvgBytesPerSec := 11025 * BYTESPERSAMPLE * g^.pcm.wf.nChannels end;
  end;

  g^.dwWaveBlockLen := FigureOutDMABufferSize(iDefWaveBlockLen, @(g^.pcm.wf));

  Result := THandle(g);
end;

function WaveMixInit: THandle;
var
  config: TMIXCONFIG;
begin
  FillChar(config, sizeof(TMIXCONFIG), 0);
  config.wSize := sizeof(TMIXCONFIG);

  Result := WaveMixConfigureInit(@config);
end;

procedure WaveMixCloseSession(hMixSession: THandle);
begin
  g := SessionToGlobalDataPtr(hMixSession);

  WaveMixActivate(hMixSession, FALSE);
  WaveMixCloseChannel(hMixSession, 0, WMIX_ALL);

  // null out all the data so we can catch references to it after we have freed it */
  FillChar(g^, sizeof(GLOBALS), 0);
  g := nil;

  LocalFree(HLOCAL(hMixSession));
end;

procedure InitLibrary;
var
  wc: TWNDCLASS;
  rt: Integer;
begin
  giDebug := GetPrivateProfileInt('general', 'debug', 0, gszIniFile);

  wc.hCursor := LoadCursor(0, IDC_ARROW);
  wc.hIcon := 0;
  wc.lpszMenuName := nil;
  wc.lpszClassName := Pointer(gszAppName);
  wc.hbrBackground := GetStockObject(LTGRAY_BRUSH);
  wc.hInstance := HInstance;
  wc.style := 0;
  wc.lpfnWndProc := @WndProc;
  wc.cbWndExtra := 0;
  wc.cbClsExtra := 0;

  rt := RegisterClass(wc);
  if ((rt = 0) and (GetLastError <> 1410) and (GetLastError <> 0)) then
  begin raise EWaveMixError.Create('Error registering class #'
      + IntToStr(GetLastError)) end;

  InitChannelNodes;  // BUGBUG: need to do this for each session
end;

initialization
  gfShow := False;
  giDebug := 0;
  gPlayQueue.first := nil;
  gPlayQueue.last := nil;
  gsz := 'Thanks Carlos Barbosa for the component, made into a simple class by DeeJayy';  //general purpose buffer for preparing strings
  gfCount := False;
  g := nil;

end.
