unit Unit1;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, IBX.IBCustomDataSet, IBX.IBQuery,
  IBX.IBDatabase, Vcl.ComCtrls, System.IniFiles, Vcl.Buttons, System.RegularExpressions,
  Generics.Collections, Vcl.StdCtrls, System.StrUtils, Vcl.ExtCtrls, Vcl.Mask,
  Vcl.DBCtrls;
type
  TFRM_Atualizar_BD = class(TForm)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDTransaction1: TFDTransaction;
    DataSource1: TDataSource;
    IBDatabase1: TIBDatabase;
    ibqueryestrutura: TIBQuery;
    IBDataSet1: TIBDataSet;
    DataSource2: TDataSource;
    ProgressBar1: TProgressBar;
    SpeedButton1: TSpeedButton;
    IBTransEstrutura: TIBTransaction;
    LabelTempoRestante: TLabel;
    LabelRegistro: TLabel;
    TrayIcon1: TTrayIcon;
    Timer1: TTimer;
    ComboBox1: TComboBox;
    BtnAplicar: TButton;
    function ExisteCampoTabela(pNomeCampo, pNomeTabela: String): Boolean;
    function ExisteCampoTabelaMySQL(const pNomeTabela,
      pNomeCampo: String): Boolean;
    function ExisteTabela(pNomeTabela: String): Boolean;
    function ObterTipoMySQLCorreto(const pNomeTabela, pNomeCampo: string): Integer;
    procedure geralog;
    procedure VersaoExe;
    procedure SincronizaTabela(const Tabela: string);
    procedure CriaCampoNaTabelaMySQL(pNomeCampo, pNomeTabela, pTipo: String);
    procedure CriaCampoAtualizadoNaTabelaMySQL(pNomeCampo, pNomeTabela, pTipo,
      pValorDef: String);
    procedure CriaCampoNaTabelaFirebird(pNomeCampo, pNomeTabela, pTipo,
      pValorDef: String);
    procedure CriaCampoExcluidoNaTabelaFirebird(pNomeCampo, pNomeTabela,
      pTipo: String);
    procedure CriaTabelaFirebird(pNomeTabela: String);
    procedure CriaTriggerChar(const Tabela: string);
    procedure Executa_Script_6;
    procedure CarregarParametrosMySQL(Conn: TFDConnection;
      const ArquivoIni: string);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CriaTriggerExclusao(const Tabela: string);
    procedure BtnAplicarClick(Sender: TObject);
  private
    FCacheValidacao: TDictionary<string, Boolean>;
    FCacheTipos: TDictionary<string, Integer>;
    FCacheCampos: TDictionary<string, TStringList>;
    FLogBuffer: TStringList;
    FRegexValidador: TRegEx;
    FTotalRegistros: Integer;
    FContadorGlobal: Integer;
    FStartTime: TDateTime;
    function ValidarIdentificador(const Nome: string): Boolean;
    function FBTypeToDelphiType(FBType: Integer): TFieldType;
    function GetCacheKeyCampo(const Tabela, Campo: string): string; inline;
    function GetCacheKeyTipo(const Tabela, Campo: string): string; inline;
    function GetPrimaryKeys(const Tabela: string): TStringList;
    procedure FlushLog;
    function ExisteTabelaMySQL(const pNomeTabela: string): Boolean;
    function FirebirdTypeToMySQL(FBType: Integer; FBSubType, FBLength,
      FBScale: Integer): string;
    procedure CriarTabelaMySQL(const NomeTabela: string);
    procedure VerificarEstruturaLogExclusoes;
    procedure InicializarCaches;
    procedure LiberarCaches;
    procedure EnsureConnections;
    procedure EnsureTransaction;
  public
    mensagem: String;
    versaosistema: String;
    Ja_Executou: Boolean;
    destructor Destroy; override;
  end;
var
  FRM_Atualizar_BD: TFRM_Atualizar_BD;
implementation
{$R *.dfm}
destructor TFRM_Atualizar_BD.Destroy;
begin
  LiberarCaches;
  inherited;
end;
procedure TFRM_Atualizar_BD.InicializarCaches;
begin
  FCacheValidacao := TDictionary<string, Boolean>.Create;
  FCacheTipos := TDictionary<string, Integer>.Create;
  FCacheCampos := TDictionary<string, TStringList>.Create;
  FLogBuffer := TStringList.Create;
  FRegexValidador := TRegEx.Create('^[A-Za-z0-9_]+$');
end;
procedure TFRM_Atualizar_BD.LiberarCaches;
var
  Lista: TStringList;
begin
  if Assigned(FCacheValidacao) then
    FCacheValidacao.Free;
  if Assigned(FCacheTipos) then
    FCacheTipos.Free;
  if Assigned(FCacheCampos) then
  begin
    for Lista in FCacheCampos.Values do
      Lista.Free;
    FCacheCampos.Free;
  end;
  if Assigned(FLogBuffer) then
  begin
    FlushLog;
    FLogBuffer.Free;
  end;
end;
function TFRM_Atualizar_BD.GetCacheKeyCampo(const Tabela, Campo: string): string;
begin
  Result := UpperCase(Tabela) + '.' + UpperCase(Campo);
end;
function TFRM_Atualizar_BD.GetCacheKeyTipo(const Tabela, Campo: string): string;
begin
  Result := UpperCase(Tabela) + '.TYPE.' + UpperCase(Campo);
end;
procedure TFRM_Atualizar_BD.EnsureConnections;
begin
  if not FDConnection1.Connected then
    FDConnection1.Connected := True;
  if not IBDatabase1.Connected then
    IBDatabase1.Connected := True;
end;
procedure TFRM_Atualizar_BD.EnsureTransaction;
begin
  if not IBTransEstrutura.InTransaction then
    IBTransEstrutura.StartTransaction;
end;
function TFRM_Atualizar_BD.ValidarIdentificador(const Nome: string): Boolean;
var
  CacheKey: string;
begin
  CacheKey := 'VALID_' + Nome;
  if FCacheValidacao.TryGetValue(CacheKey, Result) then
    Exit;
  Result := FRegexValidador.IsMatch(Nome);
  FCacheValidacao.Add(CacheKey, Result);
end;
function TFRM_Atualizar_BD.FBTypeToDelphiType(FBType: Integer): TFieldType;
begin
  case FBType of
    7: Result := ftSmallint;
    8: Result := ftInteger;
    10: Result := ftFloat;
    12: Result := ftDate;
    13: Result := ftTime;
    14, 37: Result := ftString;
    35: Result := ftDateTime;
  else
    Result := ftString;
  end;
end;
procedure TFRM_Atualizar_BD.FormCreate(Sender: TObject);
begin
  InicializarCaches;
  IBTransEstrutura := TIBTransaction.Create(Self);
  IBTransEstrutura.DefaultDatabase := IBDatabase1;
  ibqueryestrutura.Transaction := IBTransEstrutura;
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add('5 minutos');
  ComboBox1.Items.Add('10 minutos');
  ComboBox1.Items.Add('15 minutos');
  ComboBox1.Items.Add('20 minutos');
  ComboBox1.ItemIndex := 0;

  ComboBox1.ItemIndex := 0;
end;
function TFRM_Atualizar_BD.ObterTipoMySQLCorreto(const pNomeTabela,
  pNomeCampo: String): Integer;
var
  CacheKey: string;
begin
  CacheKey := GetCacheKeyTipo(pNomeTabela, pNomeCampo);
  if FCacheTipos.TryGetValue(CacheKey, Result) then
    Exit;
  Result := 0;
  ibqueryestrutura.Close;
  ibqueryestrutura.SQL.Text := 'SELECT f.RDB$FIELD_TYPE ' +
    'FROM RDB$RELATION_FIELDS rf ' +
    'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
    'WHERE rf.RDB$RELATION_NAME = :TABELA AND rf.RDB$FIELD_NAME = :CAMPO';
  EnsureTransaction;
  ibqueryestrutura.ParamByName('TABELA').AsString := UpperCase(pNomeTabela);
  ibqueryestrutura.ParamByName('CAMPO').AsString := UpperCase(pNomeCampo);
  ibqueryestrutura.Open;
  if not ibqueryestrutura.IsEmpty then
  begin
    Result := ibqueryestrutura.FieldByName('RDB$FIELD_TYPE').AsInteger;
    FCacheTipos.Add(CacheKey, Result);
  end;
  ibqueryestrutura.Close;
  IBTransEstrutura.CommitRetaining;
end;
function TFRM_Atualizar_BD.ExisteCampoTabela(pNomeCampo,
  pNomeTabela: String): Boolean;
var
  CacheKey: string;
begin
  CacheKey := GetCacheKeyCampo(pNomeTabela, pNomeCampo) + '.FB';
  if FCacheValidacao.TryGetValue(CacheKey, Result) then
    Exit;
  Ibqueryestrutura.close;
  Ibqueryestrutura.Sql.clear;
  Ibqueryestrutura.Sql.Add('select RDB$FIELD_NAME from RDB$RELATION_FIELDS');
  Ibqueryestrutura.Sql.Add('WHERE RDB$RELATION_NAME = ' +
    QuotedStr(UpperCase(pNomeTabela)));
  Ibqueryestrutura.Sql.Add('AND RDB$FIELD_NAME = ' +
    QuotedStr(UpperCase(pNomeCampo)));
  Ibqueryestrutura.Open;
  Result := not Ibqueryestrutura.IsEmpty;
  FCacheValidacao.Add(CacheKey, Result);
end;
function TFRM_Atualizar_BD.ExisteTabela(pNomeTabela: String): Boolean;
begin
  Ibqueryestrutura.close;
  Ibqueryestrutura.Sql.clear;
  Ibqueryestrutura.Sql.Add('select 1 from RDB$RELATIONS');
  Ibqueryestrutura.Sql.Add('WHERE RDB$RELATION_NAME = ' +
    QuotedStr(UpperCase(pNomeTabela)));
  Ibqueryestrutura.Open;
  Result := not Ibqueryestrutura.IsEmpty;
end;
procedure TFRM_Atualizar_BD.VerificarEstruturaLogExclusoes;
begin
  EnsureTransaction;
  if not ExisteTabela('LOG_EXCLUSOES') then
  begin
    ibqueryestrutura.Close;
    ibqueryestrutura.SQL.Text := 'CREATE TABLE LOG_EXCLUSOES ( ' +
      '  CHAVE_EXCLUIDA VARCHAR(500) NOT NULL, ' +
      '  TABELA_EXCLUIDO VARCHAR(31) NOT NULL, ' +
      '  DATAHORA_EXCLUSAO TIMESTAMP NOT NULL )';
    ibqueryestrutura.ExecSQL;
    IBTransEstrutura.CommitRetaining;
    mensagem := 'Tabela LOG_EXCLUSOES criada com nova estrutura.';
    geralog;
  end
end;
function TFRM_Atualizar_BD.GetPrimaryKeys(const Tabela: string): TStringList;
begin
  Result := TStringList.Create;
  EnsureTransaction;

  ibqueryestrutura.Close;
  ibqueryestrutura.SQL.Text :=
    'SELECT TRIM(sg.RDB$FIELD_NAME) AS CAMPO ' +
    'FROM RDB$RELATION_CONSTRAINTS rc ' +
    'JOIN RDB$INDEX_SEGMENTS sg ON rc.RDB$INDEX_NAME = sg.RDB$INDEX_NAME ' +
    'WHERE rc.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
    'AND rc.RDB$RELATION_NAME = :TABELA ' + 'ORDER BY sg.RDB$FIELD_POSITION';
  ibqueryestrutura.ParamByName('TABELA').AsString := UpperCase(Tabela);
  ibqueryestrutura.Open;
  while not ibqueryestrutura.Eof do
  begin
    Result.Add(ibqueryestrutura.FieldByName('CAMPO').AsString);
    ibqueryestrutura.Next;
  end;
  ibqueryestrutura.Close;

  if Result.Count = 0 then
  begin
    mensagem := '[AVISO] Tabela "' + Tabela +
      '" não possui Chave Primária (PK) definida. Usando o primeiro campo como fallback.';
    geralog;
    ibqueryestrutura.SQL.Text := 'SELECT TRIM(RDB$FIELD_NAME) AS FIELD_NAME ' +
      'FROM RDB$RELATION_FIELDS ' + 'WHERE RDB$RELATION_NAME = :TABELA ' +
      'ORDER BY RDB$FIELD_POSITION';
    ibqueryestrutura.ParamByName('TABELA').AsString := UpperCase(Tabela);
    ibqueryestrutura.Open;
    if not ibqueryestrutura.IsEmpty then
      Result.Add(ibqueryestrutura.FieldByName('FIELD_NAME').AsString);
    ibqueryestrutura.Close;
  end;
end;
procedure TFRM_Atualizar_BD.CriarTabelaMySQL(const NomeTabela: string);
var
  qFirebird: TIBQuery;
  CreateSQL, campo, tipoMySQL, camposPK: string;
  FBType, FBSubType, FBLength, FBScale: Integer;
  isFirst: Boolean;
  ListaChavePrimaria: TStringList;
begin
  if not ValidarIdentificador(NomeTabela) then
    raise Exception.Create('Nome de tabela inválido: ' + NomeTabela);
  mensagem := 'Criando tabela MySQL: ' + NomeTabela;
  geralog;
  qFirebird := TIBQuery.Create(nil);
  ListaChavePrimaria := nil;
  try
    qFirebird.Database := IBDatabase1;
    qFirebird.Transaction := IBTransEstrutura;
    qFirebird.SQL.Text := 'SELECT ' +
      '  TRIM(rf.RDB$FIELD_NAME) AS FIELD_NAME, ' + '  f.RDB$FIELD_TYPE, ' +
      '  f.RDB$FIELD_SUB_TYPE, ' + '  f.RDB$FIELD_LENGTH, ' +
      '  f.RDB$FIELD_SCALE, ' + '  rf.RDB$NULL_FLAG ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'WHERE rf.RDB$RELATION_NAME = :TABELA ' + 'ORDER BY rf.RDB$FIELD_POSITION';
    qFirebird.ParamByName('TABELA').AsString := UpperCase(NomeTabela);
    EnsureTransaction;
    qFirebird.Open;
    if qFirebird.IsEmpty then
      raise Exception.Create('Tabela não encontrada no Firebird: ' + NomeTabela);
    ListaChavePrimaria := GetPrimaryKeys(NomeTabela);
    CreateSQL := Format('CREATE TABLE `%s` (', [NomeTabela]);
    isFirst := True;
    while not qFirebird.Eof do
    begin
      campo := qFirebird.FieldByName('FIELD_NAME').AsString;
      FBType := qFirebird.FieldByName('RDB$FIELD_TYPE').AsInteger;
      if qFirebird.FieldByName('RDB$FIELD_SUB_TYPE').IsNull then
        FBSubType := 0
      else
        FBSubType := qFirebird.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
      if qFirebird.FieldByName('RDB$FIELD_LENGTH').IsNull then
        FBLength := 0
      else
        FBLength := qFirebird.FieldByName('RDB$FIELD_LENGTH').AsInteger;
      if qFirebird.FieldByName('RDB$FIELD_SCALE').IsNull then
        FBScale := 0
      else
        FBScale := qFirebird.FieldByName('RDB$FIELD_SCALE').AsInteger;
      tipoMySQL := FirebirdTypeToMySQL(FBType, FBSubType, FBLength, FBScale);
      if not isFirst then
        CreateSQL := CreateSQL + ', ';
      CreateSQL := CreateSQL + Format('`%s` %s', [campo, tipoMySQL]);
      if not qFirebird.FieldByName('RDB$NULL_FLAG').IsNull then
        CreateSQL := CreateSQL + ' NOT NULL';
      isFirst := False;
      qFirebird.Next;
    end;
    if ListaChavePrimaria.Count > 0 then
    begin
      camposPK := '`' + StringReplace(ListaChavePrimaria.CommaText, ',', '`, `',
        [rfReplaceAll]) + '`';
      CreateSQL := CreateSQL + Format(', PRIMARY KEY (%s)', [camposPK]);
    end;
    CreateSQL := CreateSQL + ') ENGINE=InnoDB DEFAULT CHARSET=utf8mb4';
    mensagem := 'MySQL SQL: ' + CreateSQL;
    geralog;
    try
      EnsureConnections;
      FDConnection1.ExecSQL(CreateSQL);
      FDConnection1.Commit;
      mensagem := 'Tabela criada com sucesso no MySQL: ' + NomeTabela;
      geralog;
      FCacheValidacao.Remove('TABELA_MYSQL_' + UpperCase(NomeTabela));
    except
      on E: Exception do
      begin
        mensagem := 'Erro criando tabela MySQL ' + NomeTabela + ': ' + E.Message;
        geralog;
        raise;
      end;
    end;
  finally
    qFirebird.Free;
    if Assigned(ListaChavePrimaria) then
      ListaChavePrimaria.Free;
  end;
end;
procedure TFRM_Atualizar_BD.CriaTriggerExclusao(const Tabela: string);
var
  ListaChavePrimaria: TStringList;
  TriggerName, sql, camposPKConcatenados: string;
  i: Integer;
begin
  ListaChavePrimaria := GetPrimaryKeys(Tabela);
  try
    if ListaChavePrimaria.Count = 0 then
    begin
      mensagem := 'Não foi possível criar trigger de exclusão para ' + Tabela +
        ' pois não foi encontrada uma chave.';
      geralog;
      Exit;
    end;
    TriggerName := Format('TRG_%s_EXC', [Tabela]);
    camposPKConcatenados := '';
    for i := 0 to ListaChavePrimaria.Count - 1 do
    begin
      if i > 0 then
        camposPKConcatenados := camposPKConcatenados + ' || ''|'' || ';
      camposPKConcatenados := camposPKConcatenados + 'OLD.' +
        ListaChavePrimaria[i];
    end;
    sql := Format('CREATE OR ALTER TRIGGER %s FOR %s ACTIVE AFTER DELETE AS ' +
      'BEGIN ' +
      '  INSERT INTO LOG_EXCLUSOES (CHAVE_EXCLUIDA, TABELA_EXCLUIDO, DATAHORA_EXCLUSAO) '
      + '  VALUES (%s, ''%s'', CURRENT_TIMESTAMP);' + 'END',
      [TriggerName, Tabela, camposPKConcatenados, Tabela]);
    ibqueryestrutura.Close;
    ibqueryestrutura.SQL.Text := sql;
    EnsureTransaction;
    ibqueryestrutura.ExecSQL;
    ibqueryestrutura.Transaction.CommitRetaining;
    mensagem := 'Trigger de exclusão criada/atualizada: ' + TriggerName;
    geralog;
  finally
    ListaChavePrimaria.Free;
  end;
end;
procedure TFRM_Atualizar_BD.CriaTabelaFirebird(pNomeTabela: String);
begin
  ibqueryestrutura.Close;
  ibqueryestrutura.SQL.Clear;
  ibqueryestrutura.SQL.Add('CREATE TABLE ' + UpperCase(pNomeTabela) + ' (' +
    '  ID_EXCLUIDO INTEGER NOT NULL' +
    ')');
  ibqueryestrutura.ExecSQL;
  IBTransEstrutura.CommitRetaining;
end;
procedure TFRM_Atualizar_BD.FlushLog;
var
  sl: TStringList;
begin
  if FLogBuffer.Count = 0 then
    Exit;
  sl := TStringList.Create;
  try
    if FileExists('log_sincronizacao.txt') then
      sl.LoadFromFile('log_sincronizacao.txt');
    sl.AddStrings(FLogBuffer);
    sl.SaveToFile('log_sincronizacao.txt');
    FLogBuffer.Clear;
  finally
    sl.Free;
  end;
end;
procedure TFRM_Atualizar_BD.geralog;
begin
  FLogBuffer.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss', Now) + ' - ' + mensagem);
  if (FLogBuffer.Count >= 50) or (Pos('Erro', mensagem) > 0) or
    (Pos('[AVISO]', mensagem) > 0) then
    FlushLog;
end;
procedure TFRM_Atualizar_BD.VersaoExe;
var
  FI: TFileStream;
begin
  FI := TFileStream.Create(Application.ExeName, fmOpenRead or fmShareDenyWrite);
  try
    versaosistema := '1.0.0';
  finally
    FI.Free;
  end;
end;
procedure TFRM_Atualizar_BD.BtnAplicarClick(Sender: TObject);
begin
     case ComboBox1.ItemIndex of
    0: Timer1.Interval := 5 * 60 * 1000;
    1: Timer1.Interval := 10 * 60 * 1000;
    2: Timer1.Interval := 15 * 60 * 1000;
    3: Timer1.Interval := 20 * 60 * 1000;
  end;
end;

procedure TFRM_Atualizar_BD.CarregarParametrosMySQL(Conn: TFDConnection;
  const ArquivoIni: string);
var
  Ini: TIniFile;
begin
  if not FileExists(ArquivoIni) then
    raise Exception.Create('Arquivo de parâmetros não encontrado: ' +
      ArquivoIni);
  Ini := TIniFile.Create(ArquivoIni);
  try
    Conn.Params.Clear;
    Conn.Params.Add('DriverID=MySQL');
    Conn.Params.Add('Server=' + Ini.ReadString('MySQL', 'Server', 'localhost'));
    Conn.Params.Add('Port=' + Ini.ReadString('MySQL', 'Port', '3306'));
    Conn.Params.Add('Database=' + Ini.ReadString('MySQL', 'Database', ''));
    Conn.Params.Add('User_Name=' + Ini.ReadString('MySQL', 'User', 'root'));
    Conn.Params.Add('Password=' + Ini.ReadString('MySQL', 'Password', ''));
    Conn.LoginPrompt := False;
  finally
    Ini.Free;
  end;
end;
function TFRM_Atualizar_BD.ExisteCampoTabelaMySQL(const pNomeTabela,
  pNomeCampo: String): Boolean;
var
  q: TFDQuery;
  CacheKey: string;
begin
  CacheKey := GetCacheKeyCampo(pNomeTabela, pNomeCampo) + '.MYSQL';
  if FCacheValidacao.TryGetValue(CacheKey, Result) then
    Exit;
  Result := False;
  if not ValidarIdentificador(pNomeTabela) or not
    ValidarIdentificador(pNomeCampo) then
    Exit;
  q := TFDQuery.Create(nil);
  try
    q.Connection := FDConnection1;
    q.SQL.Text := 'SELECT 1 FROM INFORMATION_SCHEMA.COLUMNS ' +
      'WHERE TABLE_SCHEMA = DATABASE() ' + 'AND TABLE_NAME = :Tabela ' +
      'AND COLUMN_NAME = :Campo';
    q.ParamByName('Tabela').AsString := pNomeTabela;
    q.ParamByName('Campo').AsString := pNomeCampo;
    q.Open;
    Result := not q.IsEmpty;
    q.Close;
    FCacheValidacao.Add(CacheKey, Result);
  finally
    q.Free;
  end;
end;
procedure TFRM_Atualizar_BD.CriaCampoNaTabelaMySQL(pNomeCampo, pNomeTabela,
  pTipo: String);
var
  sql: string;
begin
  if not ValidarIdentificador(pNomeCampo) or not
    ValidarIdentificador(pNomeTabela) then
    raise Exception.Create('Identificador inválido');
  sql := Format('ALTER TABLE `%s` ADD COLUMN `%s` %s',
    [pNomeTabela, pNomeCampo, pTipo]);
  mensagem := 'MySQL SQL: ' + sql;
  geralog;
  try
    EnsureConnections;
    FDConnection1.ExecSQL(sql);
    FDConnection1.Commit;
    mensagem := Format('Campo criado MySQL: %s.%s', [pNomeTabela, pNomeCampo]);
    geralog;
    FCacheValidacao.Remove(GetCacheKeyCampo(pNomeTabela, pNomeCampo) +
      '.MYSQL');
  except
    on E: Exception do
    begin
      mensagem := 'Erro criando campo MySQL: ' + E.Message;
      geralog;
      raise;
    end;
  end;
end;
function TFRM_Atualizar_BD.ExisteTabelaMySQL(const pNomeTabela: string): Boolean;
var
  q: TFDQuery;
  CacheKey: string;
begin
  CacheKey := 'TABELA_MYSQL_' + UpperCase(pNomeTabela);
  if FCacheValidacao.TryGetValue(CacheKey, Result) then
    Exit;
  Result := False;
  if not ValidarIdentificador(pNomeTabela) then
    Exit;
  q := TFDQuery.Create(nil);
  try
    q.Connection := FDConnection1;
    q.SQL.Text := 'SELECT 1 FROM INFORMATION_SCHEMA.TABLES ' +
      'WHERE TABLE_SCHEMA = DATABASE() ' + 'AND TABLE_NAME = :Tabela';
    q.ParamByName('Tabela').AsString := pNomeTabela;
    q.Open;
    Result := not q.IsEmpty;
    q.Close;
    FCacheValidacao.Add(CacheKey, Result);
  finally
    q.Free;
  end;
end;
function TFRM_Atualizar_BD.FirebirdTypeToMySQL(FBType: Integer;
  FBSubType, FBLength, FBScale: Integer): string;
begin
  case FBType of
    7:
      Result := 'SMALLINT';
    8:
      Result := 'INT';
    10:
      Result := 'DOUBLE';
    12:
      Result := 'DATE';
    13:
      Result := 'TIME';
    14:
      begin
        if FBSubType = 1 then
          Result := Format('VARCHAR(%d)', [FBLength])
        else
          Result := Format('CHAR(%d)', [FBLength]);
      end;
    35:
      Result := 'DATETIME';
    37:
      Result := Format('VARCHAR(%d)', [FBLength]);
    16:
      Result := Format('DECIMAL(%d,%d)', [15, Abs(FBScale)]);
    261:
      begin
        if FBSubType = 1 then
          Result := 'LONGTEXT'
        else
          Result := 'LONGBLOB';
      end;
  else
    Result := 'VARCHAR(255)';
  end;
end;
procedure TFRM_Atualizar_BD.CriaCampoAtualizadoNaTabelaMySQL(pNomeCampo,
  pNomeTabela, pTipo, pValorDef: String);
var
  sql: string;
begin
  if not ValidarIdentificador(pNomeCampo) or not
    ValidarIdentificador(pNomeTabela) then
    raise Exception.Create('Identificador inválido');
  sql := Format('ALTER TABLE `%s` ADD COLUMN `%s` %s DEFAULT %s',
    [pNomeTabela, pNomeCampo, pTipo, QuotedStr(pValorDef)]);
  mensagem := 'MySQL SQL: ' + sql;
  geralog;
  try
    EnsureConnections;
    FDConnection1.ExecSQL(sql);
    FDConnection1.Commit;
    mensagem := Format('Campo criado MySQL: %s.%s', [pNomeTabela, pNomeCampo]);
    geralog;
    FCacheValidacao.Remove(GetCacheKeyCampo(pNomeTabela, pNomeCampo) +
      '.MYSQL');
  except
    on E: Exception do
    begin
      mensagem := 'Erro criando campo MySQL: ' + E.Message;
      geralog;
      raise;
    end;
  end;
end;
procedure TFRM_Atualizar_BD.CriaCampoNaTabelaFirebird(pNomeCampo,
  pNomeTabela, pTipo, pValorDef: String);
var
  sql: string;
begin
  if not ValidarIdentificador(pNomeCampo) or not
    ValidarIdentificador(pNomeTabela) then
    raise Exception.Create('Identificador inválido');
  if pValorDef <> '' then
    sql := Format('ALTER TABLE %s ADD %s %s DEFAULT %s', [pNomeTabela,
      pNomeCampo, pTipo, QuotedStr(pValorDef)])
  else
    sql := Format('ALTER TABLE %s ADD %s %s', [pNomeTabela, pNomeCampo, pTipo]);
  mensagem := 'Firebird SQL: ' + sql;
  geralog;
  ibqueryestrutura.Close;
  ibqueryestrutura.SQL.Clear;
  ibqueryestrutura.SQL.Add(sql);
  try
    EnsureTransaction;
    ibqueryestrutura.ExecSQL;
    ibqueryestrutura.Transaction.CommitRetaining;
    mensagem := Format('Campo criado Firebird: %s.%s',
      [pNomeTabela, pNomeCampo]);
    geralog;
    FCacheValidacao.Remove(GetCacheKeyCampo(pNomeTabela, pNomeCampo) + '.FB');
  except
    on E: Exception do
    begin
      if ibqueryestrutura.Transaction.InTransaction then
        ibqueryestrutura.Transaction.Rollback;
      mensagem := 'Erro criando campo Firebird: ' + E.Message;
      geralog;
      raise;
    end;
  end;
end;
procedure TFRM_Atualizar_BD.CriaCampoExcluidoNaTabelaFirebird(pNomeCampo,
  pNomeTabela, pTipo: String);
var
  sql: string;
begin
  if not ValidarIdentificador(pNomeCampo) or not
    ValidarIdentificador(pNomeTabela) then
    raise Exception.Create('Identificador inválido');
  sql := Format('ALTER TABLE %s ADD %s %s', [pNomeTabela, pNomeCampo, pTipo]);
  mensagem := 'Firebird SQL: ' + sql;
  geralog;
  ibqueryestrutura.Close;
  ibqueryestrutura.SQL.Clear;
  ibqueryestrutura.SQL.Add(sql);
  try
    EnsureTransaction;
    ibqueryestrutura.ExecSQL;
    ibqueryestrutura.Transaction.CommitRetaining;
    mensagem := Format('Campo criado Firebird: %s.%s',
      [pNomeTabela, pNomeCampo]);
    geralog;
    FCacheValidacao.Remove(GetCacheKeyCampo(pNomeTabela, pNomeCampo) + '.FB');
  except
    on E: Exception do
    begin
      if ibqueryestrutura.Transaction.InTransaction then
        ibqueryestrutura.Transaction.Rollback;
      mensagem := 'Erro criando campo Firebird: ' + E.Message;
      geralog;
      raise;
    end;
  end;
end;
procedure TFRM_Atualizar_BD.CriaTriggerChar(const Tabela: string);
var
  TriggerName: string;
  sql: string;
begin
  TriggerName := 'TRG_' + Tabela + '_UPD';
  sql := Format('CREATE OR ALTER TRIGGER %s FOR %s ' +
    'ACTIVE BEFORE INSERT OR UPDATE ' + 'AS ' + 'BEGIN ' +
    '  IF (INSERTING OR (NEW.ATUALIZADO = OLD.ATUALIZADO)) THEN ' +
    '    NEW.ATUALIZADO = ''N''; ' + 'END', [TriggerName, Tabela]);
  ibqueryestrutura.Close;
  ibqueryestrutura.SQL.Text := sql;
  EnsureTransaction;
  ibqueryestrutura.ExecSQL;
  IBTransEstrutura.CommitRetaining;
end;
procedure TFRM_Atualizar_BD.SincronizaTabela(const Tabela: string);
var
  QMySQL: TFDQuery;
  qFirebird: TIBQuery;
  ListaCampos: TStringList;
  i, TotalRegistros: Integer;
  Campo, PrimeiroCampoValor, FieldList, ParamList, UpdateList: string;
  CacheKey: string;
  elapsed, avgPerReg, remaining: Double;
  remainingSec: Integer;
begin
  if not ValidarIdentificador(Tabela) then
    Exit;
  CacheKey := 'CAMPOS_' + UpperCase(Tabela);
  if SameText(Tabela, 'LOG_EXCLUSOES') then
    Exit;
  if not FCacheCampos.TryGetValue(CacheKey, ListaCampos) then
  begin
    ListaCampos := TStringList.Create;
    FCacheCampos.Add(CacheKey, ListaCampos);
    EnsureConnections;
    EnsureTransaction;
    ibqueryestrutura.Close;
    ibqueryestrutura.SQL.Text :=
      'SELECT TRIM(RDB$FIELD_NAME) AS CAMPO FROM RDB$RELATION_FIELDS ' +
      'WHERE RDB$RELATION_NAME = ' + QuotedStr(UpperCase(Tabela)) +
      ' ORDER BY RDB$FIELD_POSITION';
    ibqueryestrutura.Open;
    if not ibqueryestrutura.Eof then
      ibqueryestrutura.First;
    while not ibqueryestrutura.Eof do
    begin
      Campo := Trim(ibqueryestrutura.FieldByName('CAMPO').AsString);
      if (Campo <> '') and (UpperCase(Campo) <> 'ATUALIZADO') then
        ListaCampos.Add(Campo);
      ibqueryestrutura.Next;
    end;
    ibqueryestrutura.Close;
    IBTransEstrutura.CommitRetaining;
  end;
  QMySQL := TFDQuery.Create(nil);
  qFirebird := TIBQuery.Create(nil);
  try
    QMySQL.Connection := FDConnection1;
    qFirebird.Database := IBDatabase1;
    qFirebird.Transaction := IBTransEstrutura;
    qFirebird.SQL.Text := Format('SELECT * FROM %s WHERE ATUALIZADO = ''N''',
      [Tabela]);
    EnsureTransaction;
    qFirebird.Open;
    TotalRegistros := qFirebird.RecordCount;
    if TotalRegistros > 0 then
    begin
      ProgressBar1.Max := FTotalRegistros;
      FContadorGlobal := FContadorGlobal - TotalRegistros;
    end;
    FieldList := '`' + StringReplace(ListaCampos.CommaText, ',', '`, `',
      [rfReplaceAll]) + '`';
    ParamList := ':' + StringReplace(ListaCampos.CommaText, ',', ', :',
      [rfReplaceAll]);
    UpdateList := '';
    for i := 0 to ListaCampos.Count - 1 do
    begin
      if i > 0 then
        UpdateList := UpdateList + ', ';
      UpdateList := UpdateList + '`' + ListaCampos[i] + '` = VALUES(`' +
        ListaCampos[i] + '`)';
    end;
    if not qFirebird.IsEmpty then
    begin
      qFirebird.First;
      while not qFirebird.Eof do
      begin
        if ListaCampos.Count > 0 then
          PrimeiroCampoValor := qFirebird.FieldByName(ListaCampos[0]).AsString
        else
          PrimeiroCampoValor := '';
        QMySQL.SQL.Text := Format('INSERT INTO `%s` (%s, `ATUALIZADO`) VALUES (%s, ''S'') '
          + 'ON DUPLICATE KEY UPDATE %s, `ATUALIZADO` = ''S''', [Tabela,
          FieldList, ParamList, UpdateList]);
        for i := 0 to ListaCampos.Count - 1 do
        begin
          Campo := ListaCampos[i];
          var fbType := ObterTipoMySQLCorreto(Tabela, Campo);
          QMySQL.ParamByName(Campo).DataType := FBTypeToDelphiType(fbType);
          if not qFirebird.FieldByName(Campo).IsNull then
            QMySQL.ParamByName(Campo).Value := qFirebird.FieldByName(Campo)
              .Value
          else
            QMySQL.ParamByName(Campo).Clear;
        end;
        try
          QMySQL.Prepare;
          QMySQL.ExecSQL;
          FDConnection1.Commit;
        except
          on E: Exception do
          begin
            mensagem := 'Erro ao inserir/atualizar registro na tabela "' +
              Tabela + '" - ' + E.Message;
            geralog;
          end;
        end;
        Inc(FContadorGlobal);
        ProgressBar1.Position := FContadorGlobal;
        if FTotalRegistros > 0 then
        begin
          elapsed := (Now - FStartTime) * 24 * 60 * 60;
          avgPerReg := elapsed / FContadorGlobal;
          remaining := (FTotalRegistros - FContadorGlobal) * avgPerReg;
          remainingSec := Round(remaining);
          LabelTempoRestante.Caption := Format('Tempo restante: %d min %d s',
            [remainingSec div 60, remainingSec mod 60]);
        end;
        LabelRegistro.Caption := Format('Registro: %d/%d', [FContadorGlobal,
          FTotalRegistros]);
        Application.ProcessMessages;
        qFirebird.Next;
      end;
    end;
  finally
    QMySQL.Free;
    qFirebird.Free;
    FlushLog;
  end;
end;
procedure TFRM_Atualizar_BD.Executa_Script_6;
var
  ListaTabelas: TStringList;
  i: Integer;
  TotalTabelas, TotalGeralRegistros: Integer;
  QFirebirdDel: TIBQuery;
  QMySQLDel: TFDQuery;
  ListaPKCampos: TStringList;
  ValoresPK: TArray<string>;
  NomeTabela, ChaveCompostaValores, whereClause: string;
begin
  EnsureConnections;
  EnsureTransaction;
  VerificarEstruturaLogExclusoes;
  ListaTabelas := TStringList.Create;
  QFirebirdDel := TIBQuery.Create(nil);
  QMySQLDel := TFDQuery.Create(nil);
  try
    QFirebirdDel.Database := IBDatabase1;
    QFirebirdDel.Transaction := IBTransEstrutura;
    QMySQLDel.Connection := FDConnection1;
    ibqueryestrutura.Close;
    ibqueryestrutura.SQL.Text :=
      'SELECT TRIM(RDB$RELATION_NAME) AS TABELA FROM RDB$RELATIONS WHERE RDB$SYSTEM_FLAG = 0 ORDER BY RDB$RELATION_NAME';
    ibqueryestrutura.Open;
    while not ibqueryestrutura.Eof do
    begin
      ListaTabelas.Add(UpperCase(Trim(ibqueryestrutura.FieldByName('TABELA')
        .AsString)));
      ibqueryestrutura.Next;
    end;
    ibqueryestrutura.Close;
    for i := 0 to ListaTabelas.Count - 1 do
    begin
      if SameText(ListaTabelas[i], 'LOG_EXCLUSOES') then
        Continue;
      if not ExisteTabelaMySQL(ListaTabelas[i]) then
        CriarTabelaMySQL(ListaTabelas[i]);
    end;
    QFirebirdDel.SQL.Text :=
      'SELECT CHAVE_EXCLUIDA, TABELA_EXCLUIDO FROM LOG_EXCLUSOES';
    QFirebirdDel.Open;
    while not QFirebirdDel.Eof do
    begin
      NomeTabela := QFirebirdDel.FieldByName('TABELA_EXCLUIDO').AsString;
      ChaveCompostaValores := QFirebirdDel.FieldByName('CHAVE_EXCLUIDA')
        .AsString;
      ListaPKCampos := nil;
      try
        ListaPKCampos := GetPrimaryKeys(NomeTabela);
        if ListaPKCampos.Count = 0 then
        begin
          mensagem :=
            'Erro ao processar exclusão: Não foi possível determinar a PK da tabela '
            + NomeTabela;
          geralog;
          QFirebirdDel.Next;
          Continue;
        end;
        ValoresPK := ChaveCompostaValores.Split(['|']);
        if ListaPKCampos.Count <> Length(ValoresPK) then
        begin
          mensagem := Format('Erro de integridade no log para tabela %s. ' +
            'Contagem de campos da PK (%d) difere da contagem de valores no log (%d). Valor: %s',
            [NomeTabela, ListaPKCampos.Count, Length(ValoresPK),
            ChaveCompostaValores]);
          geralog;
          QFirebirdDel.Next;
          Continue;
        end;
        whereClause := '';
        for i := 0 to ListaPKCampos.Count - 1 do
        begin
          if i > 0 then
            whereClause := whereClause + ' AND ';
          whereClause := whereClause + Format('`%s` = :%s', [ListaPKCampos[i],
            ListaPKCampos[i]]);
        end;
        QMySQLDel.SQL.Text := Format('DELETE FROM `%s` WHERE %s', [NomeTabela,
          whereClause]);
        for i := 0 to ListaPKCampos.Count - 1 do
        begin
          QMySQLDel.ParamByName(ListaPKCampos[i]).AsString := ValoresPK[i];
        end;
        QMySQLDel.ExecSQL;
        FDConnection1.Commit;
        ibqueryestrutura.Close;
        ibqueryestrutura.SQL.Text :=
          'DELETE FROM LOG_EXCLUSOES WHERE CHAVE_EXCLUIDA = :CHAVE AND TABELA_EXCLUIDO = :TABELA';
        ibqueryestrutura.ParamByName('CHAVE').AsString := ChaveCompostaValores;
        ibqueryestrutura.ParamByName('TABELA').AsString := NomeTabela;
        ibqueryestrutura.ExecSQL;
        IBTransEstrutura.CommitRetaining;
      except
        on E: Exception do
        begin
          mensagem := Format('Erro ao deletar registro com chave "%s" da tabela %s: %s',
            [ChaveCompostaValores, NomeTabela, E.Message]);
          geralog;
          IBTransEstrutura.RollbackRetaining;
        end;
      end;
      if Assigned(ListaPKCampos) then
        ListaPKCampos.Free;
      QFirebirdDel.Next;
    end;
    QFirebirdDel.Close;
    for i := 0 to ListaTabelas.Count - 1 do
    begin
      if SameText(ListaTabelas[i], 'LOG_EXCLUSOES') then
        Continue;
      if not ExisteCampoTabela('ATUALIZADO', ListaTabelas[i]) then
      begin
        CriaCampoNaTabelaFirebird('ATUALIZADO', ListaTabelas[i], 'CHAR(1)',
          '''N''');
      end;
      if not ExisteCampoTabelaMySQL(ListaTabelas[i], 'ATUALIZADO') then
      begin
        CriaCampoAtualizadoNaTabelaMySQL('ATUALIZADO', ListaTabelas[i],
          'CHAR(1)', 'S');
      end;
      EnsureTransaction;
      ibqueryestrutura.Close;
      ibqueryestrutura.SQL.Text := Format('UPDATE %s SET ATUALIZADO = ''N'' WHERE ATUALIZADO IS NULL',
        [ListaTabelas[i]]);
      ibqueryestrutura.ExecSQL;
      IBTransEstrutura.CommitRetaining;
    end;
    TotalTabelas := ListaTabelas.Count;
    TotalGeralRegistros := 0;
    for i := 0 to ListaTabelas.Count - 1 do
    begin
      if SameText(ListaTabelas[i], 'LOG_EXCLUSOES') then
        Continue;
      ibqueryestrutura.Close;
      ibqueryestrutura.SQL.Text :=
        Format('SELECT COUNT(*) FROM %s WHERE ATUALIZADO = ''N''',
        [ListaTabelas[i]]);
      try
        ibqueryestrutura.Open;
        TotalGeralRegistros := TotalGeralRegistros +
          ibqueryestrutura.Fields[0].AsInteger;
      finally
        ibqueryestrutura.Close;
      end;
    end;
    FTotalRegistros := TotalGeralRegistros;
    FContadorGlobal := 0;
    ProgressBar1.Min := 0;
    ProgressBar1.Max := FTotalRegistros;
    ProgressBar1.Position := 0;
    ProgressBar1.Visible := True;
    FStartTime := Now;
    mensagem := Format('Iniciando sincronização de %d tabelas, com um total de %d registros...',
      [TotalTabelas, FTotalRegistros]);
    geralog;
    for i := 0 to ListaTabelas.Count - 1 do
    begin
      if SameText(ListaTabelas[i], 'LOG_EXCLUSOES') then
        Continue;
      Self.Caption := Format('Sincronização - Tabela %d/%d: %s',
        [i + 1, TotalTabelas, ListaTabelas[i]]);
      SincronizaTabela(ListaTabelas[i]);
      CriaTriggerChar(ListaTabelas[i]);
      CriaTriggerExclusao(ListaTabelas[i]);
      Application.ProcessMessages;
      try
        ibqueryestrutura.Close;
        ibqueryestrutura.SQL.Text :=
          Format('UPDATE %s SET ATUALIZADO = ''S'' WHERE ATUALIZADO = ''N''',
          [ListaTabelas[i]]);
        EnsureTransaction;
        ibqueryestrutura.ExecSQL;
        IBTransEstrutura.CommitRetaining;
      finally
      end;
    end;
    ProgressBar1.Position := FTotalRegistros;
    Self.Caption := 'Sincronização Concluída';
    ProgressBar1.Visible := False;
    LabelTempoRestante.Caption := 'Concluído!';
    mensagem := Format('Sincronização de todas as %d tabelas e %d registros concluída com sucesso!',
      [TotalTabelas, FTotalRegistros]);
    geralog;
  finally
    ListaTabelas.Free;
    QFirebirdDel.Free;
    QMySQLDel.Free;
    FlushLog;
  end;
end;
procedure TFRM_Atualizar_BD.SpeedButton1Click(Sender: TObject);
var
  iniFile: string;
begin
  iniFile := ChangeFileExt(Application.ExeName, '.ini');
  if FDConnection1.Params.Count = 0 then
    CarregarParametrosMySQL(FDConnection1, iniFile);
  try
    EnsureConnections;
    Executa_Script_6;
    ShowMessage('Sincronização concluída!');
  except
    on E: Exception do
      ShowMessage('Erro: ' + E.Message);
  end;
end;

end.
