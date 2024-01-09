program VCLTest;
  {$warn COMPARING_SIGNED_UNSIGNED off}
  {$warn COMBINING_SIGNED_UNSIGNED off}
  {$warn COMBINING_SIGNED_UNSIGNED64 off}
  {$warn IMPLICIT_STRING_CAST off}
  {$warn IMPLICIT_STRING_CAST_LOSS off}
  {$warn GARBAGE off}
  {$warn WIDECHAR_REDUCED off}
  {$warn SYMBOL_DEPRECATED off}
  {$warn DUPLICATE_CTOR_DTOR off}
  {$warn PRIVATE_PROPACCESSOR off}
  
uses
  Vcl.Forms,
  MainForm in 'Code\MainForm.pas' {Form1},
  ErrorManager in 'Code\ErrorManager.pas',
  ModelProcessing in 'Code\ModelProcessing.pas',
  ShapeNodeColor in 'Code\ShapeNodeColor.pas',
  T3DViewManager in 'Code\T3DViewManager.pas',
  CastleControlManager in 'Code\CastleControlManager.pas',
  IModelProcessing in 'Code\Interfaces\IModelProcessing.pas',
  I_T3DViewManager in 'Code\Interfaces\I_T3DViewManager.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
