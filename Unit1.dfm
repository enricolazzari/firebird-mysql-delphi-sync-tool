object FRM_Atualizar_BD: TFRM_Atualizar_BD
  Left = 0
  Top = 0
  Caption = 'FRM_Atualizar_BD'
  ClientHeight = 231
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 208
    Top = 132
    Width = 65
    Height = 22
    Caption = 'Atualizar'
    OnClick = SpeedButton1Click
  end
  object LabelTempoRestante: TLabel
    Left = 208
    Top = 104
    Width = 101
    Height = 13
    Caption = 'LabelTempoRestante'
  end
  object LabelRegistro: TLabel
    Left = 208
    Top = 72
    Width = 65
    Height = 13
    Caption = 'LabelRegistro'
  end
  object ProgressBar1: TProgressBar
    Left = 208
    Top = 160
    Width = 241
    Height = 25
    TabOrder = 0
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=bitcomteste'
      'Password=006023leBD2025!'
      'User_Name=root'
      'DriverID=MySQL')
    Connected = True
    Left = 376
    Top = 72
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 456
    Top = 80
  end
  object FDTransaction1: TFDTransaction
    Connection = FDConnection1
    Left = 432
    Top = 24
  end
  object DataSource1: TDataSource
    DataSet = ibqueryestrutura
    Left = 112
    Top = 80
  end
  object IBDatabase1: TIBDatabase
    Connected = True
    DatabaseName = 'C:\pdv bitcom\banco pdv\PDV.FDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey'
      '')
    LoginPrompt = False
    ServerType = 'IBServer'
    Left = 80
    Top = 32
  end
  object ibqueryestrutura: TIBQuery
    Database = IBDatabase1
    BufferChunks = 1000
    CachedUpdates = False
    ParamCheck = True
    Left = 56
    Top = 80
  end
  object IBDataSet1: TIBDataSet
    Database = IBDatabase1
    BufferChunks = 1000
    CachedUpdates = False
    ParamCheck = True
    UniDirectional = False
    Left = 56
    Top = 136
  end
  object DataSource2: TDataSource
    DataSet = IBDataSet1
    Left = 112
    Top = 136
  end
  object IBTransaction1: TIBTransaction
    DefaultDatabase = IBDatabase1
    Left = 80
    Top = 184
  end
end
