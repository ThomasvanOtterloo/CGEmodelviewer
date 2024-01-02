unit CastleControlManager;

interface

uses
  T3DViewManager,Vcl.CastleControl, SysUtils, Vcl.Dialogs, System.Classes, vcl.Controls,VCL.Forms
  ;

function GetCastleControl: TCastleControl;
function FindCastleControls(o: TObject): TArray<TCastleControl>;
procedure InitializeGLWin(Form: TForm);


implementation
var
  CastleControl: TCastleControl;
  IsCastleControlOnForm: boolean;
  GLWin: TCastleControl;
  GLView: TCastleApp;

function GetCastleControl: TCastleControl;
begin

  CastleControl.Container.DesignUrl :=
    'castle-data:/test_3d.castle-user-interface';

result := CastleControl;
end;



function FindCastleControls(o: TObject): TArray<TCastleControl>;
var
  I: Integer;
begin
  Result := TArray<TCastleControl>.Create();
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TCastleControl then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := TCastleControl(Components[I]);
    end;
  end;
end;



procedure InitializeGLWin(Form: TForm);
var
  CastleControls: TArray<TCastleControl>;
begin
  CastleControls := FindCastleControls(Form);
  IsCastleControlOnForm := Length(CastleControls) > 0;

  if IsCastleControlOnForm then
    GLWin := CastleControls[0]
  else
    GLWin := TCastleControl.Create(Form);

  GLWin.Parent := Form;
  GLWin.Align := alClient;


  //

   GLView := TCastleApp.Create(GLWin);
  GLWin.Container.View := GLView;

end;

end.
