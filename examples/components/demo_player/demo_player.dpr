program demo_player;

uses
  Vcl.Forms,
  uDemoPlayerForm in 'uDemoPlayerForm.pas' {FormDemoPlayer};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDemoPlayer, FormDemoPlayer);
  Application.Run;
end.
