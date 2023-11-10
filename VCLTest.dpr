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
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
