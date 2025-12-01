unit FormBinarize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, ImageProcessing;

type

  { TFormBinarization }

  TFormBinarization = class(TForm)
    Aplicar: TButton;
    Cancelar: TButton;
    Metodo: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Umbral: TTrackBar;
    procedure AplicarClick(Sender: TObject);
    procedure CancelarClick(Sender: TObject);
    procedure MetodoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UmbralChange(Sender: TObject);
  private
    FSourceMatrix: RGB_MATRIX;
    FImageHeight: Integer;
    FImageWidth: Integer;
    FResultMatrix: RGB_MATRIX;
    procedure PreviewBinarization;
  public
    procedure SetSourceImage(const AMatrix: RGB_MATRIX; AHeight, AWidth: Integer);
    function GetResultMatrix: RGB_MATRIX;
    function WasApplied: Boolean;
  end;

var
  FormBinarization: TFormBinarization;

implementation

{$R *.lfm}

{ TFormBinarization }

procedure TFormBinarization.FormCreate(Sender: TObject);
begin
  // Configurar ComboBox con los modos de binarización
  Metodo.Items.Clear;
  Metodo.Items.Add('Promedio RGB');
  Metodo.Items.Add('Luma (Luminancia)');
  Metodo.Items.Add('Canal R');
  Metodo.Items.Add('Canal G');
  Metodo.Items.Add('Canal B');
  Metodo.ItemIndex := 0;
  
  // Configurar TrackBar
  Umbral.Min := 0;
  Umbral.Max := 255;
  Umbral.Position := 128;
  
  Label2.Caption := 'Umbral: ' + IntToStr(Umbral.Position);
  
  // Configurar botones
  Aplicar.Caption := 'Aplicar';
  Cancelar.Caption := 'Cancelar';
  
  // Configurar imagen
  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.Center := True;
end;

procedure TFormBinarization.SetSourceImage(const AMatrix: RGB_MATRIX; AHeight, AWidth: Integer);
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
  PreviewBinarization;
end;

procedure TFormBinarization.PreviewBinarization;
var
  tmpBmp: TBitmap;
  modeIndex: Integer;
  threshold: Byte;
begin
  if (FImageWidth = 0) or (FImageHeight = 0) then
    Exit;
    
  modeIndex := Metodo.ItemIndex;
  if modeIndex < 0 then
    modeIndex := 0;
    
  threshold := Byte(Umbral.Position);
  
  // Aplicar binarización en matriz temporal
  ImageProcessing.BinarizeMatrix(FSourceMatrix, FResultMatrix, FImageHeight, FImageWidth, modeIndex, threshold);
  
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

procedure TFormBinarization.UmbralChange(Sender: TObject);
begin
  Label2.Caption := 'Umbral: ' + IntToStr(Umbral.Position);
  PreviewBinarization;
end;

procedure TFormBinarization.MetodoChange(Sender: TObject);
begin
  PreviewBinarization;
end;

procedure TFormBinarization.AplicarClick(Sender: TObject);
begin
  // Aplicar y cerrar con resultado OK
  ModalResult := mrOk;
end;

procedure TFormBinarization.CancelarClick(Sender: TObject);
begin
  // Cancelar y cerrar
  ModalResult := mrCancel;
end;

function TFormBinarization.GetResultMatrix: RGB_MATRIX;
begin
  Result := FResultMatrix;
end;

function TFormBinarization.WasApplied: Boolean;
begin
  // Si el formulario se cerró con mrOk, se aplicó
  Result := (ModalResult = mrOk);
end;

end.
