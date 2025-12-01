unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, StdCtrls, ExtDlgs, TAGraph, TASeries, TAChartUtils, Math,
  ImageProcessing, FormHistogram, FormBinarize, FormGamma;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    Abrir: TMenuItem;
    Grises: TMenuItem;
    Guardar: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Binarizacion: TMenuItem;
    Restaurar: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    Shape1: TShape;

    procedure FormCreate(Sender: TObject);
    procedure AbrirClick(Sender: TObject);
    procedure GrisesClick(Sender: TObject);
    procedure BinarizacionClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseLeave(Sender: TObject; Shift: TShiftState; X, Y:
      Integer);
    procedure Label1Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure RestaurarClick(Sender: TObject);
  end;

var
  Form1: TForm1;
  IMG_HEIGHT, IMG_WIDTH, COLOR_MODE: Integer;
  MATRIX, ORIGINAL_MATRIX: RGB_MATRIX;
  CONVERTED_HSV_MATRIX: HSV_MATRIX;
  GRAY_SCALE_VALUES: GRAY_SCALE_MATRIX;
  CONVERTED_GRAY_MATRIX: RGB_MATRIX;
  BMAP: TBitmap;
  IMAGE_CONTAINER_HEIGHT, IMAGE_CONTAINER_WIDTH: Integer;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BMAP := TBitmap.Create;
  Image1.Stretch := True;
  Image1.Proportional := False;
  Image1.Center := True;
  IMAGE_CONTAINER_HEIGHT := Image1.Height;
  IMAGE_CONTAINER_WIDTH := Image1.Width;
end;

procedure TForm1.AbrirClick(Sender: TObject);
begin
  // Configurar el diálogo antes de abrirlo
  OpenDialog1.Title := 'Seleccionar imagen';
  OpenDialog1.Filter := 'Archivos de imagen|*.bmp;*.jpg;*.jpeg;*.png|Bitmap (*.bmp)|*.bmp;*.BMP|Todos los archivos|*.*';
  OpenDialog1.FilterIndex := 1;
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  OpenDialog1.Options := [ofFileMustExist, ofEnableSizing, ofViewDetail];

  if OpenDialog1.Execute then
  begin
    SetLength(MATRIX, 0, 0, 0);
    SetLength(ORIGINAL_MATRIX, 0, 0, 0);
    SetLength(CONVERTED_GRAY_MATRIX, 0, 0, 0);
    SetLength(CONVERTED_HSV_MATRIX, 0, 0, 0);
    Image1.Enabled := True;
    BMAP.LoadFromFile(OpenDialog1.FileName);
    IMG_HEIGHT := BMAP.Height;
    IMG_WIDTH := BMAP.Width;
    if BMAP.PixelFormat <> pf24bit then
      BMAP.PixelFormat := pf24bit;
    StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
    SetLength(MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    SetLength(ORIGINAL_MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    
    // Usar funciones del módulo ImageProcessing
    ImageProcessing.CopyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, MATRIX);
    ImageProcessing.CopyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, ORIGINAL_MATRIX);
    Image1.Picture.Assign(BMAP);
    ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
    COLOR_MODE := 1;
  end;
end;

procedure TForm1.GrisesClick(Sender: TObject);
begin
  // Usar función del módulo ImageProcessing
  ImageProcessing.MediumRangeGrayScale(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_GRAY_MATRIX);
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, CONVERTED_GRAY_MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  COLOR_MODE := 2;
end;

procedure TForm1.BinarizacionClick(Sender: TObject);
var
  BinarizeForm: TFormBinarization;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear y mostrar el formulario de binarización
  BinarizeForm := TFormBinarization.Create(Self);
  try
    // Pasar la imagen actual al formulario
    BinarizeForm.SetSourceImage(MATRIX, IMG_HEIGHT, IMG_WIDTH);

    // Si el usuario hace clic en "Aplicar" (mrOk), actualizar la matriz
    if BinarizeForm.ShowModal = mrOk then
    begin
      // Obtener el resultado directamente
      MATRIX := BinarizeForm.GetResultMatrix;

      // Actualizar la imagen en el formulario principal
      ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
      Image1.Picture.Assign(BMAP);
      ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
    end;
  finally
    BinarizeForm.Free;
  end;
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
  if (X < 0) or (X >= IMG_WIDTH) or (Y < 0) or (Y >= IMG_HEIGHT) then Exit;

  // Actualizar paneles solo si existen
  if StatusBar1.Panels.Count > 1 then
    StatusBar1.Panels[1].Text := IntToStr((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH);
  if StatusBar1.Panels.Count > 2 then
    StatusBar1.Panels[2].Text := IntToStr((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT);

  if (Length(MATRIX) > 0) and (Length(MATRIX[0]) > 0) then
  begin
    if StatusBar1.Panels.Count > 4 then

      // StatusBar1.Panels[4].Text := IntToStr(MATRIX[X, Y, 0]) + ',' + IntToStr(MATRIX[X, Y, 1]) + ',' + IntToStr(MATRIX[X, Y, 2]);
      StatusBar1.Panels[4].Text := IntToStr(MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 0]) + ',' + IntToStr(MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 1]) + ',' + IntToStr(MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 2]);
  end;

    if (Length(CONVERTED_HSV_MATRIX) > 0) and (Length(CONVERTED_HSV_MATRIX[0]) > 0) then
  begin
    if StatusBar1.Panels.Count > 8 then
      StatusBar1.Panels[8].Text := IntToStr(CONVERTED_HSV_MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 0]) + ', ' + IntToStr(CONVERTED_HSV_MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 1]) + ', ' + IntToStr(CONVERTED_HSV_MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 2]);
  end;

  // Mostrar color (solo si hay matrices válidas)
  Label1.Visible := True;
  Shape1.Visible := True;
  if COLOR_MODE = 1 then // Modo RGB
  begin
    if (Length(MATRIX) > 0) and (Length(MATRIX[0]) > 0) then
      Shape1.Brush.Color := RGBToColor(MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 0], MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 1], MATRIX[((X * IMG_WIDTH) div IMAGE_CONTAINER_WIDTH), ((Y * IMG_HEIGHT) div IMAGE_CONTAINER_HEIGHT), 2])
    else
      Shape1.Brush.Color := clBtnFace;
  end
  else if COLOR_MODE = 2 then // Modo escala de grises
  begin
    if (Length(CONVERTED_GRAY_MATRIX) > 0) and (Length(CONVERTED_GRAY_MATRIX[0]) > 0) then
      Shape1.Brush.Color := RGBToColor(CONVERTED_GRAY_MATRIX[X, Y, 0], CONVERTED_GRAY_MATRIX[X, Y, 1], CONVERTED_GRAY_MATRIX[X, Y, 2])
    else
      Shape1.Brush.Color := clBtnFace;
  end;
end;

procedure TForm1.Image1MouseLeave(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  // Ocultar información cuando el mouse sale de la imagen
  Label1.Visible := False;
  Shape1.Visible := False;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin

end;

procedure TForm1.MenuItem4Click(Sender: TObject);
var
  HistForm: TFormHist;
  histData: THistogramData;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;
  // Calcular el histograma usando la función del módulo ImageProcessing
  ImageProcessing.GenerateHistogram(IMG_HEIGHT, IMG_WIDTH, MATRIX, histData);
  // Crear y mostrar el formulario del histograma
  HistForm := TFormHist.Create(Self);
  try
    HistForm.ShowHistogram(histData);
    HistForm.ShowModal;
  finally
    HistForm.Free;
  end;
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
var
  GammaForm: TFormGammaCorrection;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear y mostrar el formulario de corrección gamma
  GammaForm := TFormGammaCorrection.Create(Self);
  try
    // Pasar la imagen actual al formulario
    GammaForm.SetSourceImage(MATRIX, IMG_HEIGHT, IMG_WIDTH);

    // Si el usuario hace clic en "Aplicar" (mrOk), actualizar la matriz
    if GammaForm.ShowModal = mrOk then
    begin
      // Obtener el resultado directamente
      MATRIX := GammaForm.GetResultMatrix;

      // Actualizar la imagen en el formulario principal
      ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
      Image1.Picture.Assign(BMAP);
      ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
    end;
  finally
    GammaForm.Free;
  end;
end;

procedure TForm1.RestaurarClick(Sender: TObject);
begin
  // Restaurar la imagen a su estado original cargado desde archivo
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Copiar la matriz original a la matriz actual y actualizar la imagen
  MATRIX := ORIGINAL_MATRIX;
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
  COLOR_MODE := 1; // Volver al modo color original
end;

end.
