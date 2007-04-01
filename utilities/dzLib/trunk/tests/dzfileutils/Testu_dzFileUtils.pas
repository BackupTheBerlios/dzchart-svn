unit Testu_dzFileUtils;

interface

uses
  Windows,
  Classes,
  SysUtils,
  TestFramework,
  u_dzFileUtils;

type
  TestTFileSystem = class(TTestCase)
  private
    FCallbackCount: integer;
    procedure ProgressContinue(_Status: TCopyProgressStatus;
      var _Continue: TCopyProgressStatus.TProgressResult);
    procedure ProgressCancel(_Status: TCopyProgressStatus;
      var _Continue: TCopyProgressStatus.TProgressResult);
  protected
    procedure SetUp; override;
  published
    procedure TestCopyFileWithProgressContinue;
    procedure TestCopyFileWithProgressCancel;
    procedure TestMoveFileWithProgressCancel;
  end;

implementation


{ TestTFileSystem }

procedure TestTFileSystem.ProgressCancel(_Status: TCopyProgressStatus;
  var _Continue: TCopyProgressStatus.TProgressResult);
begin
  _Continue := prCancel;
  Inc(FCallbackCount);
end;

procedure TestTFileSystem.ProgressContinue(_Status: TCopyProgressStatus;
  var _Continue: TCopyProgressStatus.TProgressResult);
begin
  _Continue := prContinue;
  Inc(FCallbackCount);
end;

procedure TestTFileSystem.SetUp;
begin
  inherited;
  FCallbackCount := 0;
end;

procedure TestTFileSystem.TestCopyFileWithProgressContinue;
const
  SOURCEFILE = 'testdata\sourcefile.txt';
  DESTFILE = 'testdata\destfile.txt';
var
  Res: TFileSystem.TCopyFileWithProgressResult;
begin
  Res := TFileSystem.CopyFileWithProgress(SOURCEFILE, DESTFILE, ProgressContinue);
  Check(Res = cfwOK, 'aborted or error');
  CheckTrue(FileExists(DESTFILE), 'destination file missing');
  CheckEquals(2, FCallbackCount, 'Callback count wrong');
end;

procedure TestTFileSystem.TestMoveFileWithProgressCancel;
const
  SOURCEFILE = 'testdata\sourcefile.txt';
  DESTFILE = 'testdata\destfile.txt';
var
  Res: TFileSystem.TCopyFileWithProgressResult;
begin
  Res := TFileSystem.MoveFileWithProgress(SOURCEFILE, DESTFILE, ProgressCancel, []);
  Check(Res = cfwOK, 'aborted expected');
end;

procedure TestTFileSystem.TestCopyFileWithProgressCancel;
const
  SOURCEFILE = 'testdata\sourcefile.txt';
  DESTFILE = 'testdata\destfile.txt';
var
  Res: TFileSystem.TCopyFileWithProgressResult;
begin
  Res := TFileSystem.CopyFileWithProgress(SOURCEFILE, DESTFILE, ProgressCancel, []);
  Check(Res = cfwAborted, 'aborted expected');
  CheckEquals(1, FCallbackCount, 'Callback count wrong');
end;

initialization
  RegisterTest(TestTFileSystem.Suite);
end.

