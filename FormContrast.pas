unit FormContrast;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, ImageProcessing;

type

  { TFormContrast }

  TFormContrast = class(TForm)
    ApplyBtn: TButton;
    CancelBtn: TButton;
    Image1: TImage;
    Label1: TLabel;
    TrackBarPercent: TTrackBar;
    procedure ApplyBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBarPercentChange(Sender: TObject);
  private
    FSourceMatrix: RGB_MATRIX;
    FImageHeight: Integer;
    FImageWidth: Integer;
    FResultMatrix: RGB_MATRIX;
    procedure UpdatePreview;
  public
    procedure SetSourceImage(const AMatrix: RGB_MATRIX; AHeight, AWidth: Integer);
    function GetResultMatrix: RGB_MATRIX;
  end;

var
  FormContrastForm: TFormContrast;

implementation

{$R *.lfm}

{ TFormContrast }

procedure TFormContrast.FormCreate(Sender: TObject);
begin
  TrackBarPercent.Min := 0;
  TrackBarPercent.Max := 49; // 0..49 -> 0.00..0.49 (paso 0.01)
  TrackBarPercent.Position := 1; // 0.01 por defecto
  Label1.Caption := 'Percentil: 0.01';

  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.Center := True;
end;

procedure TFormContrast.SetSourceImage(const AMatrix: RGB_MATRIX; AHeight, AWidth: Integer);
var
  tmpBmp: TBitmap;
begin
  FImageHeight := AHeight;
  FImageWidth := AWidth;
  SetLength(FSourceMatrix, AWidth, AHeight, 3);
  FSourceMatrix := Copy(AMatrix);

  tmpBmp := TBitmap.Create;
  try
    tmpBmp.PixelFormat := pf24bit;
    tmpBmp.SetSize(AWidth, AHeight);
    ImageProcessing.CopyMatrixToImage(AHeight, AWidth, FSourceMatrix, tmpBmp);
    Image1.Picture.Assign(tmpBmp);
  finally
    tmpBmp.Free;
  end;

  UpdatePreview;
end;

procedure TFormContrast.TrackBarPercentChange(Sender: TObject);
begin
  Label1.Caption := 'Percentil: ' + FormatFloat('0.00', TrackBarPercent.Position / 100);
  // actualizar preview (con retardo no necesario aquí)
  UpdatePreview;
end;

procedure TFormContrast.UpdatePreview;
var
  p: Double;
  tmpBmp: TBitmap;
begin
  if (FImageWidth = 0) or (FImageHeight = 0) then Exit;
  p := TrackBarPercent.Position / 100.0;
  ImageProcessing.ApplyContrastMatrix(FSourceMatrix, FResultMatrix, FImageHeight, FImageWidth, 1, p);

  tmpBmp := TBitmap.Create;
  try
    tmpBmp.PixelFormat := pf24bit;
    tmpBmp.SetSize(FImageWidth, FImageHeight);
    ImageProcessing.CopyMatrixToImage(FImageHeight, FImageWidth, FResultMatrix, tmpBmp);
    Image1.Picture.Assign(tmpBmp);
  finally
    tmpBmp.Free;
  end;
end;

procedure TFormContrast.ApplyBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormContrast.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TFormContrast.GetResultMatrix: RGB_MATRIX;
begin
  Result := FResultMatrix;
end;

end.
