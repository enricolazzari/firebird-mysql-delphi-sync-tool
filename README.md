# sincronizador-delphi-firebird-mysql

Sincronizador de dados entre Firebird e MySQL desenvolvido em Delphi.

Este projeto é uma ferramenta de sincronização unidirecional desenvolvida em Delphi 10.4 (32-bits). Sua principal função é ler a estrutura e os dados de um banco de dados Firebird 3.5 e replicá-los em um banco de dados MySQL 8.0.43, mantendo o MySQL como um espelho atualizado do Firebird.

## ✨ Principais Funcionalidades

* **Replicação de Esquema:** Cria automaticamente tabelas no MySQL com base no esquema existente no Firebird, convertendo os tipos de dados de forma compatível.
* **Sincronização de Dados (Upsert):** Insere novos registros e atualiza registros existentes do Firebird para o MySQL de forma eficiente, utilizando o comando `INSERT ... ON DUPLICATE KEY UPDATE`.
* **Rastreamento de Alterações:** Utiliza um campo `ATUALIZADO` e triggers no Firebird para marcar registros que foram inseridos ou modificados, garantindo que apenas os dados necessários sejam sincronizados.
* **Tratamento de Exclusões:** Emprega uma tabela `LOG_EXCLUSOES` e triggers de exclusão no Firebird para replicar as deleções de registros no banco de dados MySQL.
* **Log de Operações:** Gera um arquivo `log_sincronizacao.txt` com o registro detalhado das operações e possíveis erros ocorridos durante o processo.

## 🛠️ Tecnologias e Pré-requisitos

* **Linguagem:** Delphi 10.4 (versão 32-bits)
* **Banco de Dados (Origem):** Firebird 3.5
* **Banco de Dados (Destino):** MySQL 8.0.43
* **Componentes de Acesso:** InterBase Express (IBX) para Firebird e FireDAC para MySQL.
* **Ferramentas SGBD:** IBExpert (Firebird) e MySQL Workbench (ou similar).

### Driver MySQL

É **obrigatório** o uso do driver `libmysql.dll` de **32 bits**. Como a IDE Delphi utilizada é um ambiente de 32 bits, o driver de conexão com o banco de dados deve seguir a mesma arquitetura para garantir a compatibilidade.

## ⚙️ Configuração e Uso

A configuração do ambiente é um processo manual e requer os seguintes passos:

1.  **Criação dos Bancos de Dados:** Crie manualmente os bancos de dados vazios no Firebird e no MySQL. A aplicação não cria os bancos, apenas gerencia as tabelas e os dados dentro deles.

2.  **Ambiente Delphi:**
    * Abra o projeto no Embarcadero Delphi.
    * No formulário, adicione manualmente todos os componentes listados na seção `type` do código (ex: `TFDConnection`, `TIBDatabase`, `TButton`, `TProgressBar`, etc.).
    * Conecte as propriedades e os eventos dos componentes conforme a implementação no código.
    * Configure as propriedades do componente `TIBDatabase` (IBX) para que ele se conecte ao seu banco de dados Firebird.

3.  **Arquivo de Configuração (`.ini`):**
    * As credenciais de conexão com o MySQL são carregadas de um arquivo de configuração. Crie um arquivo com o mesmo nome do seu executável e a extensão `.ini` (ex: `Sincronizador.ini`).
    * Este arquivo `.ini` e a `libmysql.dll` devem estar na mesma pasta do executável gerado (geralmente `Win32\Debug` ou `Win32\Release`).
    * Adicione a seguinte estrutura ao arquivo, preenchendo com seus dados:

    ```ini
    [MySQL]
    DriverID=MySQL
    Database=nome_do_seu_banco_mysql
    User=seu_usuario_mysql
    Password=sua_senha_mysql
    Server=localhost
    Port=3306
    VendorLib= C:\Caminho\Ate\Pasta\Executavel\libmysql.dll
    ```
   *Repita o processo apenas sem a parte do vendorlib para o Firebird.

   
4.  **Permissões no MySQL:**
    * Certifique-se de que o usuário do MySQL informado no `.ini` tem as permissões necessárias (`SELECT`, `INSERT`, `UPDATE`, `DELETE`, `CREATE`, `ALTER`) no banco de dados de destino.
    * **Nota:** Se você acabou de criar o usuário ou alterar suas permissões manualmente via linha de comando, pode ser necessário executar o comando `FLUSH PRIVILEGES;` no MySQL para que as alterações entrem em vigor imediatamente.

5.  **Execução:**
    * Compile e execute a aplicação. O processo de sincronização será iniciado ao acionar o evento correspondente (ex: clique de um botão).

## ⚠️ Notas Importantes

* **Atualizações em Lote (Batch):** Por padrão, a sincronização é feita registro a registro. Para cenários com grande quantidade de dados, recomenda-se adaptar o código para usar atualizações em lote (como o recurso `Array DML` do FireDAC) para uma performance superior.
* **Interface Gráfica (UI):** Os componentes visuais (botões, barras de progresso, labels) devem ser adicionados e posicionados manualmente no formulário. O código fornecido contém apenas a lógica de funcionamento.
* **Segurança:** As credenciais de conexão são armazenadas em texto plano no arquivo `.ini`. Para ambientes de produção, considere utilizar métodos mais seguros para gerenciar essas informações.

