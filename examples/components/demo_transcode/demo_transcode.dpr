program demo_transcode;

uses
  Vcl.Forms,
  uDemoTranscodeForm in 'uDemoTranscodeForm.pas' {FormDemoTranscode};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDemoTranscode, FormDemoTranscode);
  Application.Run;
end.
