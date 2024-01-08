unit Unit3;

interface

uses
  DUnitX.TestFramework, TestFramework,
  Delphi.Mocks,
  MainForm, T3DViewManager, ShapeNodeColor,ModelProcessing,CastleControlManager,

  //vlc
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls,  Vcl.CastleControl,

  //cge:
   CastleShapes, CastleCameras, X3DNodes, X3DLoad, CastleTransform,
  CastleBoxes, CastleSceneCore, X3DLoadInternalUtils, CastleUIControls,
  CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleUtils, CastleTriangles,
  CastleRectangles, CastleColors, CastleScene, CastleViewport

  ;


type
  TestTModelProcessing = class(TTestcase)
 private
    FModelProcessing: TModelProcessing;
    FShapeList: TShapeList;
    FBox1, FBox2: TBox3D;
   protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestCalculateSumBbox;
  end;

implementation

procedure TestTModelProcessing.SetUp;
begin
  // Initialize the TModelProcessing instance
  FModelProcessing := TModelProcessing.Create(nil);

  // Set up mock ShapeList and bounding boxes
  FShapeList := TShapeList.Create(true);
  FBox1 := TBox3D.Create(Point3D(0, 0, 0), Point3D(1, 1, 1));
  FBox2 := TBox3D.Create(Point3D(1, 1, 1), Point3D(2, 2, 2));



  // Here, you would need to add mock shapes to FShapeList
  // and set their bounding boxes to FBox1 and FBox2
end;

procedure TestTModelProcessing.TearDown;
begin
  FModelProcessing.Free;
  FShapeList.Free;
end;

procedure TestTModelProcessing.TestCalculateSumBbox;
var
  ExpectedResult, ActualResult: TBox3D;
begin
  // Define the expected result
  ExpectedResult := FBox1 + FBox2; // This assumes your + operator on TBox3D is defined correctly

  // Call the method
  ActualResult := FModelProcessing.CalculateSumBbox(FShapeList);

  // Check if the actual result matches the expected result
  CheckTrue(ActualResult.Equals(ExpectedResult), 'CalculateSumBbox did not return the expected result');
end;

initialization
  RegisterTest(TestTModelProcessing.Suite);
end.
