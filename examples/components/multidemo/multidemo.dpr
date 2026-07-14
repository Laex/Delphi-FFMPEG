program multidemo;

uses
  Vcl.Forms,
  uMultiDemoForm in 'uMultiDemoForm.pas' {FormMultiDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMultiDemo, FormMultiDemo);
  Application.Run;
end.
