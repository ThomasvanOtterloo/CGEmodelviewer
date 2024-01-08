unit CastleControlManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls,

  CastleShapes, CastleCameras, X3DNodes, X3DLoad, CastleTransform,
  CastleBoxes, CastleSceneCore, X3DLoadInternalUtils, CastleUIControls,
  CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleUtils, CastleTriangles,
  CastleRectangles, CastleColors, CastleScene, CastleViewport,
  Vcl.CastleControl,

  Math, ErrorManager, ModelProcessing, T3DViewManager;

function GetCastleControl: TCastleControl;
function FindCastleControls(Form: TForm): TArray<TCastleControl>;
function InitializeGLWin(Form: TForm): TCastleControl;
procedure SetGLWinView(GLView: TCastleApp);

implementation

var
  IsCastleControlOnForm: boolean;
  GLWin: TCastleControl;
  GLView: TCastleApp;

function GetCastleControl: TCastleControl; // third. Getting CastleControl
begin
  GLWin.Container.DesignUrl := 'castle-data:/test_3d.castle-user-interface';

  result := GLWin;
end;

function InitializeGLWin(Form: TForm): TCastleControl; // first
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
  result := GLWin;

end;


procedure SetGLWinView(GLView: TCastleApp);
begin
    GLWin.Container.View := GLView;
end;


function FindCastleControls(Form: TForm): TArray<TCastleControl>; // second
var
  I: Integer;
begin
  result := TArray<TCastleControl>.Create();
  for I := 0 to Form.ComponentCount - 1 do
  begin
    if Form.Components[I] is TCastleControl then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result) - 1] := TCastleControl(Form.Components[I]);
    end;
  end;

end;

end.
