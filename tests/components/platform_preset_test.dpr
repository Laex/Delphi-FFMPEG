program platform_preset_test;

{$IFDEF FPC}
{$MODE Delphi}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  {$ELSE}
  System.Classes,
  System.SysUtils,
  {$ENDIF}
  uFFEncoder,
  uFFWriter,
  uFFFrameFilter,
  uFFTranscodePreset;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

procedure TestPreset(APreset: TFFTranscodePreset; const AExpectedCodec, AExpectedFormat: string;
  AExpectAudioTranscode: Boolean; const AExpectedFilter: string);
var
  Encoder: TFFEncoder;
  Writer: TFFWriter;
  Filter: TFFFrameFilter;
  Job: TFFTranscodeJob;
begin
  Encoder := TFFEncoder.Create(nil);
  Writer := TFFWriter.Create(nil);
  Filter := TFFFrameFilter.Create(nil);
  Job := TFFTranscodeJob.Create(nil);
  try
    Job.Encoder := Encoder;
    Job.Writer := Writer;
    Job.FrameFilter := Filter;
    Job.Preset := APreset;
    Job.ApplyPreset;

    if Encoder.CodecName <> AExpectedCodec then
      Fail(Format('%s: expected codec %s, got %s',
        [TFFTranscodePresetHelper.DisplayName(APreset), AExpectedCodec, Encoder.CodecName]));
    if Writer.FormatName <> AExpectedFormat then
      Fail(Format('%s: expected format %s, got %s',
        [TFFTranscodePresetHelper.DisplayName(APreset), AExpectedFormat, Writer.FormatName]));
    if AExpectAudioTranscode and not Encoder.TranscodeAudio then
      Fail(TFFTranscodePresetHelper.DisplayName(APreset) + ': TranscodeAudio expected');
    if (AExpectedFilter <> '') and (Filter.FilterDescription <> AExpectedFilter) then
      Fail(Format('%s: expected filter %s, got %s',
        [TFFTranscodePresetHelper.DisplayName(APreset), AExpectedFilter, Filter.FilterDescription]));

    WriteLn('OK ', TFFTranscodePresetHelper.DisplayName(APreset));
  finally
    Job.Free;
    Filter.Free;
    Writer.Free;
    Encoder.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG platform preset test');
  TestPreset(ftpYouTube_1080p, 'libx264', 'mp4', True, '');
  TestPreset(ftpWeb_Preview_480p, 'libx264', 'mp4', True, 'scale=854:480');
  WriteLn('PASS: platform presets OK');
end.
