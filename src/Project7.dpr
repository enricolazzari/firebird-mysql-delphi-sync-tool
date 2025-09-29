program Project7;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {FRM_Atualizar_BD};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFRM_Atualizar_BD, FRM_Atualizar_BD);
  Application.Run;
end.
