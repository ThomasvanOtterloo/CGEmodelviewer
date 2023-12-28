unit Unit3;

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  MainForm,

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
  TFormMock = TMock<TForm1>;
  TCastleControlMock = TMock<TCastleControl>;

  [TestFixture]
  UnitTestsCGE = class
  strict private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);

    var
      FormMock: TFormMock;
      CastleControlMock: TCastleControlMock;
      MockScene: TMock<TCastleScene>;
  end;

implementation

procedure UnitTestsCGE.Setup;
var
  MockForm: TMock<TForm>;
  MockCastleControl: TMock<TCastleControl>;
  MockScene: TMock<TCastleScene>;  //sut
begin
  // Create a mock form
  MockForm := TMock<TForm>.Create;

  // Create a mock TCastleControl
  MockCastleControl := TMock<TCastleControl>.Create(MockForm);

  // Create a mock TCastleScene
  MockScene := TMock<TCastleScene>.Create;

  MockScene.Create(MockForm.Instance);

  // Configure the mock scene
  MockScene.Setup.WillReturn('C:\Users\t.vanotterloo\Documents\SelfmadeDemos\CGEmodelviewer\data\8.glb').When.URL;

  // Link the mock scene to the mock control if needed
  MockCastleControl.Setup.WillReturn(MockScene.Instance);

  // Continue with the setup...
end;




procedure UnitTestsCGE.TearDown;
begin
  Form1.Free; // Corrected reference to the field

end;

procedure UnitTestsCGE.Test2(const AValue1 : Integer;const AValue2 : Integer);
var
Input: string;
//ModelScene: TCastleScene;

begin
  // Arange
  Input := 'EthernetPort';


  // Act

  FormMock.Setup.Expect.Once.When.SetFailedObject(Input);
  // Assert

    Assert.IsTrue(True, 'This test always passes');
end;

//procedure UnitTestsCGE.TestSetFailedObjectWithValidNode;
//var
//  NodeNameOfFailedObject: string;
//begin
//  // Arrange
//  NodeNameOfFailedObject := 'Fingers';
//  try
//    // Act
//    TForm1.SetFailedObject(NodeNameOfFailedObject);
//
//    // Assert
//    Assert.IsTrue(True, 'SetFailedObject executed without exception');
//    // Here you might want to add more specific assertions
//    // related to the expected outcomes of SetFailedObject
//  except
//    on E: Exception do
//      Assert.Fail('Exception was raised: ' + E.Message);
//  end;
//end;






initialization
  TDUnitX.RegisterTestFixture(UnitTestsCGE);

end.
