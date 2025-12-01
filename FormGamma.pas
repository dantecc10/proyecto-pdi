unit FormGamma;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, ImageProcessing;

type

  { TFormGammaCorrection }

  TFormGammaCorrection = class(TForm)
    Aplicar: TButton;
    Cancelar: TButton;
    Edit1: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    TrackBarGamma: TTrackBar;
    Timer1: TTimer;
    procedure AplicarClick(Sender: TObject);
    procedure CancelarClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBarGammaChange(Sender: TObject);
  private
    FSourceMatrix: RGB_MATRIX;
    FImageHeight: Integer;
    FImageWidth: Integer;
    FResultMatrix: RGB_MATRIX;
    FPendingPreview: Boolean;
    procedure PreviewGamma;
    function GetGammaValue: Double;
  public
    procedure SetSourceImage(const AMatrix: RGB_MATRIX; AHeight, AWidth: Integer);
    function GetResultMatrix: RGB_MATRIX;
  end;

var
  FormGammaCorrection: TFormGammaCorrection;

implementation

{$R *.lfm}

{ TFormGammaCorrection }

procedure TFormGammaCorrection.FormCreate(Sender: TObject);
begin
  // Configurar TrackBar para gamma (0.1 a 5.0, multiplicado por 100)
  TrackBarGamma.Min := 10;      // 0.1
  TrackBarGamma.Max := 500;     // 5.0
  TrackBarGamma.Position := 100; // 1.0 (sin cambios)
  
  Label2.Caption := 'Gamma: 1.00';
  Edit1.Text := '1.00';
  
  // Configurar botones
  Aplicar.Caption := 'Aplicar';
  Cancelar.Caption := 'Cancelar';
  
  // Configurar imagen
  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.Center := True;
  
  // Configurar temporizador
  FPendingPreview := False;
  Timer1.Interval := 150; // 150ms de retraso
  Timer1.Enabled := False;
end;

procedure TFormGammaCorrection.SetSourceImage(const AMatrix: RGB_MATRIX; AHeight, AWidth: Integer);
var
  tmpBmp: TBitmap;
begin
  FImageHeight := AHeight;
  FImageWidth := AWidth;
  
  // Copiar la matriz de origen
  SetLength(FSourceMatrix, AWidth, AHeight, 3);
  FSourceMatrix := Copy(AMatrix);
  
  // Mostrar imagen original
  tmpBmp := TBitmap.Create;
  try
    tmpBmp.PixelFormat := pf24bit;
    tmpBmp.SetSize(AWidth, AHeight);
    ImageProcessing.CopyMatrixToImage(AHeight, AWidth, FSourceMatrix, tmpBmp);
    Image1.Picture.Assign(tmpBmp);
  finally
    tmpBmp.Free;
  end;
  
  // Mostrar preview inicial
  PreviewGamma;
end;

function TFormGammaCorrection.GetGammaValue: Double;
begin
  Result := TrackBarGamma.Position / 100.0;
end;

procedure TFormGammaCorrection.PreviewGamma;
var
  tmpBmp: TBitmap;
  gammaVal: Double;
  x, y, k: Integer;
begin
  if (FImageWidth = 0) or (FImageHeight = 0) then
    Exit;
    
  gammaVal := GetGammaValue;
  
  // Copiar matriz fuente manualmente (más rápido que Copy)
  SetLength(FResultMatrix, FImageWidth, FImageHeight, 3);
  for x := 0 to FImageWidth - 1 do
    for y := 0 to FImageHeight - 1 do
      for k := 0 to 2 do
        FResultMatrix[x, y, k] := FSourceMatrix[x, y, k];
  
  // Aplicar gamma
  ImageProcessing.ApplyGammaCorrection(FResultMatrix, FImageHeight, FImageWidth, gammaVal);
  
  // Mostrar preview
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

procedure TFormGammaCorrection.TrackBarGammaChange(Sender: TObject);
var
  gammaVal: Double;
begin
  gammaVal := GetGammaValue;
  Label2.Caption := 'Gamma: ' + FormatFloat('0.00', gammaVal);
  Edit1.Text := FormatFloat('0.00', gammaVal);
  
  // Marcar que hay un preview pendiente y reiniciar el timer
  FPendingPreview := True;
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TFormGammaCorrection.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if FPendingPreview then
  begin
    FPendingPreview := False;
    PreviewGamma;
  end;
end;

procedure TFormGammaCorrection.Edit1Change(Sender: TObject);
var
  gammaVal: Double;
  newPos: Integer;
begin
  // Intentar convertir el texto a número
  if TryStrToFloat(Edit1.Text, gammaVal) then
  begin
    // Limitar rango
    if gammaVal < 0.1 then
      gammaVal := 0.1
    else if gammaVal > 5.0 then
      gammaVal := 5.0;
      
    newPos := Round(gammaVal * 100);
    if newPos <> TrackBarGamma.Position then
    begin
      TrackBarGamma.Position := newPos;
      Label2.Caption := 'Gamma: ' + FormatFloat('0.00', gammaVal);
      
      // Usar el timer también para cambios desde Edit
      FPendingPreview := True;
      Timer1.Enabled := False;
      Timer1.Enabled := True;
    end;
  end;
end;

procedure TFormGammaCorrection.AplicarClick(Sender: TObject);
begin
  // Aplicar y cerrar con resultado OK
  ModalResult := mrOk;
end;

procedure TFormGammaCorrection.CancelarClick(Sender: TObject);
begin
  // Cancelar y cerrar
  ModalResult := mrCancel;
end;

function TFormGammaCorrection.GetResultMatrix: RGB_MATRIX;
begin
  Result := FResultMatrix;
end;

end.
