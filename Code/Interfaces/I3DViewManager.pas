unit I3DViewManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, ModelProcessing,

  CastleShapes, CastleCameras, X3DNodes, X3DLoad, CastleTransform,
  CastleBoxes, CastleSceneCore, X3DLoadInternalUtils, CastleUIControls,
  CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleUtils, CastleTriangles,
  CastleRectangles, CastleColors, CastleScene, CastleViewport,
  Vcl.CastleControl;

type
  I_3DViewManager = interface
    procedure SetBoolCamOrbitIsActive(boolInput: boolean);
    procedure SetBoolMainNav(boolInput: boolean);
    function GetBoolMainNav: boolean;
    procedure CalculateNewCameraPos(ModelProcessing: TModelProcessing;
      ModelScene: TCastleScene);
    function Motion(const Event: TInputMotion): boolean;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean);
    procedure UpdateMainCameraPosition(const ModelCenter: TVector3;
      const DistanceToModel, AngleU: Single);
    procedure ErrorNavSetup(Sender: TObject);
    function Press(const Event: TInputPressRelease): boolean;
  end;

implementation

end.
