      ******************************************************************
      * Author: RICARDO CABRAL DE MELO
      * Date: 18.06.2019
      * Purpose: PROVA DE COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROVACOB.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE ASSIGN TO DISK
             ORGANIZATION       IS INDEXED
             ACCESS MODE        IS DYNAMIC
             RECORD KEY         IS COD-CLIENTE
             LOCK MODE          IS MANUAL
             FILE STATUS        IS WS-RESULTADO-ACESSO.
           SELECT ARQ-VENDEDOR ASSIGN TO DISK
             ORGANIZATION       IS INDEXED
             ACCESS MODE        IS DYNAMIC
             RECORD KEY         IS COD-VENDEDOR
             LOCK MODE          IS MANUAL
             FILE STATUS        IS WS-RESULTADO-ACESSO.
           SELECT ARQ-IMPORTACAO ASSIGN TO DISK
             ORGANIZATION IS SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS        IS WS-RESULTADO-ACESSO.
       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CLIENTE VALUE OF FILE-ID IS "cliente".
       01 WID-ARQ-CLIENTE.
           05 COD-CLIENTE              PIC 9(007).
           05 CNPJ-CLIENTE             PIC 9(014).
           05 RAZAO-CLIENTE            PIC X(40).
           05 LAT-CLIENTE              PIC S9(003)V9(008).
           05 LON-CLIENTE              PIC S9(003)V9(008).
       FD ARQ-VENDEDOR VALUE OF FILE-ID IS "vendedor".
       01 WID-ARQ-VENDEDOR.
           05 COD-VENDEDOR             PIC 9(003).
           05 CPF-VENDEDOR             PIC 9(011).
           05 NOME-VENDEDOR            PIC X(40).
           05 LAT-VENDEDOR             PIC S9(003)V9(008).
           05 LON-VENDEDOR             PIC S9(003)V9(008).
       FD ARQ-IMPORTACAO VALUE OF FILE-ID IS NOMEARQ.
       01 WID-ARQ-IMPORTACAO.
           05 FILLER                   PIC X(003).
           05 COD-CLIENTE-IMP          PIC 9(007).
           05 CNPJ-CLIENTE-IMP         PIC 9(014).
           05 RAZAO-CLIENTE-IMP        PIC X(40).
           05 LAT-CLIENTE-IMP          PIC S9(003)V9(008).
           05 LON-CLIENTE-IMP          PIC S9(003)V9(008).
       WORKING-STORAGE SECTION.
       77 VALIDA                       PIC X(08) VALUE "VALIDA".
       77 NOMEARQ                      PIC X(70).
       01 WS-RESULTADO-ACESSO          PIC 9(02).
       77 WS-OPCAO1                    PIC X(01).
           88 CADASTRO     VALUE IS "C" "c".
           88 RELATORIO    VALUE IS "R" "r".
           88 EXECUCAO     VALUE IS "E" "e".
           88 ENCERRAR     VALUE IS "X" "x".
       77 WS-OPCAO2                    PIC X(01).
           88 CLIENTE      VALUE IS "c" "C".
           88 VENDEDOR     VALUE IS "V" "v".
           88 VOLTAR       VALUE IS "X" "x".
       77 WS-OPCAO3                    PIC X(01).
           88 INCLUSAO     VALUE IS "I" "i".
           88 ALTERACAO    VALUE IS "A" "a".
           88 EXCLUSAO     VALUE IS "E" "e".
           88 IMPORTACAO   VALUE IS "M" "m".
           88 VOLTARC      VALUE IS "X" "x".
       01 WS-CABECALHO.
           05 WS-CAB-PARTE1         PIC X(22).
           05 WS-CAB-PARTE2         PIC X(15).
       01 WS-ARQ                   PIC 9(01).
           88 ARQ-OK      VALUE IS 1.
       01 WS-REG                   PIC 9(01).
           88 REG-OK      VALUE IS 1.
       01 WS-ARQ-FIM               PIC 9(01).
           88 ARQ-FIM     VALUE IS 1.
       01 POSL                     PIC 9(03).
       01 POSC                     PIC 9(03).
       77 WS-MSGERRO               PIC X(80).
       77 WS-ERRO                  PIC X(01).
       77 WS-MSG                   PIC X(50).
       77 WS-LINHA                 PIC 9(03).
       COPY screenio.
       SCREEN SECTION.
       01 TELA.
           05 TELA-INICIAL.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL BACKGROUND-COLOR 1.
               10 LINE WS-LINHA COLUMN 01 ERASE EOL BACKGROUND-COLOR 1.
           05 TELA-CABECALHO.
               10 LINE 01 COLUMN 02 PIC X(30) FROM WS-CABECALHO
                  HIGHLIGHT FOREGROUND-COLOR 6
                  BACKGROUND-COLOR 1.
           05 TELA-MSG.
               10 LINE WS-LINHA COLUMN 2 ERASE EOL PIC X(50)
                  FROM WS-MSG HIGHLIGHT
                  FOREGROUND-COLOR 6
                  BACKGROUND-COLOR 1.
       01 TELA-ERRO.
           05 LINE WS-LINHA COLUMN 2 ERASE EOL PIC X(80)
              FROM WS-MSG HIGHLIGHT
              FOREGROUND-COLOR 6
              BACKGROUND-COLOR 1.
           05 COLUMN PLUS 2 TO WS-ERRO.
       01 TELA-MENU-PRINCIPAL FOREGROUND-COLOR 6.
           05 LINE 10 COLUMN 05 VALUE "C - CADASTRO".
           05 LINE 11 COLUMN 05 VALUE "R - RELATORIO".
           05 LINE 12 COLUMN 05 VALUE "E - EXECUCAO".
           05 LINE 13 COLUMN 05 VALUE "X - ENCERRAR".
           05 LINE 14 COLUMN 05 VALUE "OPCAO: ".
           05 LINE 14 COLUMN 12 USING WS-OPCAO1 AUTO.
       01 TELA-MENU-TIPO-CADASTRO FOREGROUND-COLOR 6.
           05 LINE 10 COLUMN 05 VALUE "C - CLIENTE".
           05 LINE 11 COLUMN 05 VALUE "V - VENDEDOR".
           05 LINE 12 COLUMN 05 VALUE "X - VOLTAR".
           05 LINE 13 COLUMN 05 VALUE "OPCAO: ".
           05 LINE 13 COLUMN 12 USING WS-OPCAO2 AUTO.
       01 TELA-MENU-CADASTRO FOREGROUND-COLOR 6.
           05 LINE 10 COLUMN 05 VALUE "I - INCLUSAO".
           05 LINE 11 COLUMN 05 VALUE "A - ALTERACAO".
           05 LINE 12 COLUMN 05 VALUE "E - EXCLUSAO".
           05 LINE 13 COLUMN 05 VALUE "M - IMPORTACAO".
           05 LINE 14 COLUMN 05 VALUE "X - VOLTAR".
           05 LINE 15 COLUMN 05 VALUE "OPCAO: ".
           05 LINE 15 COLUMN 12 USING WS-OPCAO3 AUTO.
       01 TELA-MENU-RELATORIO FOREGROUND-COLOR 6.
           05 LINE 10 COLUMN 05 VALUE "RELATORIO".
           05 LINE 12 COLUMN 05 VALUE "X - VOLTAR".
           05 LINE 13 COLUMN 05 VALUE "OPCAO: ".
           05 LINE 13 COLUMN 12 USING WS-OPCAO1 AUTO.
       01 TELA-MENU-EXECUCAO FOREGROUND-COLOR 6.
           05 LINE 10 COLUMN 05 VALUE "EXECUCAO".
           05 LINE 12 COLUMN 05 VALUE "X - VOLTAR".
           05 LINE 13 COLUMN 05 VALUE "OPCAO: ".
           05 LINE 13 COLUMN 12 USING WS-OPCAO1 AUTO.
       01 TELA-CADASTRO-CLIENTE FOREGROUND-COLOR 6.
           05 LINE 10 COLUMN 05 VALUE "CODIGO CLIENTE:".
           05 LINE 10 COLUMN 27 PIC 9(07) USING COD-CLIENTE
           BLANK WHEN ZEROS.
           05 LINE 11 COLUMN 05 VALUE "CNPJ..........:".
           05 LINE 11 COLUMN 27 PIC 9(14) USING CNPJ-CLIENTE
           BLANK WHEN ZEROS.
           05 LINE 12 COLUMN 05 VALUE "RAZAO CLIENTE.:".
           05 LINE 12 COLUMN 27 PIC X(40) USING RAZAO-CLIENTE.
           05 LINE 13 COLUMN 05 VALUE "LATITUDE......:".
           05 LINE 13 COLUMN 27 PIC S9(003)V9(008) USING LAT-CLIENTE.
           05 LINE 14 COLUMN 05 VALUE "LONGITUDE.....:".
           05 LINE 14 COLUMN 27 PIC S9(003)V9(008) USING LON-CLIENTE.
       01 TELA-CADASTRO-VENDEDOR FOREGROUND-COLOR 6.
           05 LINE 10 COLUMN 05 VALUE "CODIGO VENDEDOR:".
           05 LINE 10 COLUMN 27 PIC 9(03) USING COD-VENDEDOR
           BLANK WHEN ZEROS.
           05 LINE 11 COLUMN 05 VALUE "CPF...........:".
           05 LINE 11 COLUMN 27 PIC 9(11) USING CPF-VENDEDOR
           BLANK WHEN ZEROS.
           05 LINE 12 COLUMN 05 VALUE "NOME VENDEDOR.:".
           05 LINE 12 COLUMN 27 PIC X(40) USING NOME-VENDEDOR.
           05 LINE 13 COLUMN 05 VALUE "LATITUDE......:".
           05 LINE 13 COLUMN 27 PIC S9(003)V9(008) USING LAT-VENDEDOR.
           05 LINE 14 COLUMN 05 VALUE "LONGITUDE.....:".
           05 LINE 14 COLUMN 27 PIC S9(003)V9(008) USING LON-VENDEDOR.
       01 TELA-ARQ-IMPORTACAO FOREGROUND-COLOR 6.
           05 LINE 10 COLUMN 05 VALUE "ARQUIVO A IMPORTAR: ".
           05 LINE 11 COLUMN 05 PIC X(70) USING NOMEARQ.
       01 VALIDA-DADOS.
       COPY VALIDABOOK.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       PERFORM ABRE-ARQUIVO
       SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
       SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
       SET ENVIRONMENT 'ESCDELAY' TO '25'.
       PERFORM UNTIL ENCERRAR
               PERFORM MONTA-TELA-PRINCIPAL
               EVALUATE TRUE
                   WHEN CADASTRO
                       PERFORM EXECUTA-CADASTRO
                   WHEN RELATORIO
                       PERFORM MONTA-TELA-RELATORIO
                   WHEN EXECUCAO
                        PERFORM MONTA-TELA-EXECUCAO
                   WHEN ENCERRAR
                        CLOSE ARQ-CLIENTE ARQ-VENDEDOR ARQ-IMPORTACAO
                        STOP RUN
               END-EVALUATE
       END-PERFORM.
       ABRE-ARQUIVO SECTION.
       OPEN I-O ARQ-CLIENTE
           IF WS-RESULTADO-ACESSO = 35
               OPEN OUTPUT ARQ-CLIENTE
               CLOSE ARQ-CLIENTE
               OPEN I-O ARQ-CLIENTE
           END-IF.
       OPEN I-O ARQ-VENDEDOR
           IF WS-RESULTADO-ACESSO = 35
               OPEN OUTPUT ARQ-VENDEDOR
               CLOSE ARQ-VENDEDOR
               OPEN I-O ARQ-VENDEDOR
           END-IF.
           EXIT.
       EXECUTA-CADASTRO SECTION.
           PERFORM UNTIL VOLTAR
               PERFORM MONTA-TELA-TIPO-CADASTRO
               EVALUATE TRUE
                   WHEN CLIENTE
                       PERFORM EXECUTA-TELA-CLIENTE
                   WHEN VENDEDOR
                        PERFORM EXECUTA-TELA-VENDEDOR
               END-EVALUATE
           END-PERFORM.
           EXIT.
       EXECUTA-TELA-CLIENTE SECTION.
           PERFORM UNTIL VOLTARC
               PERFORM MONTA-TELA-CLIENTE
               EVALUATE TRUE
                   WHEN INCLUSAO
                        PERFORM MONTA-TELA-CADASTRO-CLIENTE
                        PERFORM EXECUTA-INCLUSAO-CLIENTE
                   WHEN ALTERACAO
                        PERFORM MONTA-TELA-CADASTRO-CLIENTE
                        PERFORM EXECUTA-ALTERACAO-CLIENTE
                   WHEN EXCLUSAO
                        PERFORM MONTA-TELA-CADASTRO-CLIENTE
                        PERFORM EXECUTA-EXCLUSAO-CLIENTE
                   WHEN IMPORTACAO
                        PERFORM MONTA-TELA-ARQUIVO-IMPORTACAO
                        PERFORM EXECUTA-IMPORTACAO
               END-EVALUATE
           END-PERFORM.
           EXIT.
       EXECUTA-TELA-VENDEDOR SECTION.
           PERFORM UNTIL VOLTARC
               PERFORM MONTA-TELA-VENDEDOR
               EVALUATE TRUE
                   WHEN INCLUSAO
                        PERFORM MONTA-TELA-CADASTRO-VENDEDOR
                   WHEN ALTERACAO
                        PERFORM MONTA-TELA-CADASTRO-VENDEDOR
                   WHEN EXCLUSAO
                        PERFORM MONTA-TELA-CADASTRO-VENDEDOR
                   WHEN IMPORTACAO
                        PERFORM MONTA-TELA-CADASTRO-VENDEDOR
               END-EVALUATE
           END-PERFORM.
           PERFORM EXECUTA-TELA-CLIENTE.
       EXIT.
       EXECUTA-INCLUSAO-CLIENTE SECTION.
           PERFORM UNTIL COB-CRT-STATUS = COB-SCR-ESC
               PERFORM CRITICA-DADOS-CLIENTE
               IF ARQ-OK
                   WRITE WID-ARQ-CLIENTE
                   PERFORM INICIALIZA-INCLUSAO-CLIENTE
               ELSE
                   DISPLAY TELA-ERRO
                   ACCEPT TELA-ERRO
                   DISPLAY TELA-MSG
               END-IF
               PERFORM MONTA-TELA-CADASTRO-CLIENTE
           END-PERFORM.
           EXIT.
       CRITICA-DADOS-CLIENTE SECTION.
           IF COD-CLIENTE = ZEROS
              MOVE "INFORME CODIGO VALIDO" TO WS-MSG
           ELSE
              IF CNPJ-CLIENTE = ZEROS
                 MOVE "INFORME CNPJ VALIDO" TO WS-MSG
              ELSE
                  IF RAZAO-CLIENTE = SPACES
                     MOVE "INFORME A RAZAO SOCIAL DO CLIENTE" TO WS-MSG
                  ELSE
                      IF LAT-CLIENTE = ZEROS
                         MOVE "INFORME LATITUDE VALIDA" TO WS-MSG
                      ELSE
                          IF LON-CLIENTE = ZEROS
                             MOVE "INFORME LONGITUDE VALIDA" TO WS-MSG
                          ELSE
                             MOVE 1 TO WS-ARQ
                          END-IF
                      END-IF
                  END-IF
              END-IF
           END-IF,
           IF ARQ-OK
               MOVE 2 TO FUNCAO
               MOVE CNPJ-CLIENTE TO DADO-VALIDAR
               PERFORM CHAMA-VALIDA
           END-IF.
           EXIT.
       CHAMA-VALIDA SECTION.
           CALL VALIDA USING VALIDA-DADOS
               IF COD-RETORNO <> ZEROS
              MOVE ZEROS TO WS-ARQ
              MOVE MSG-RETORNO TO WS-MSG
           EXIT.
       EXIT.
       EXECUTA-ALTERACAO-CLIENTE SECTION.
           PERFORM UNTIL COB-CRT-STATUS = COB-SCR-ESC
               MOVE 1 TO WS-REG
               PERFORM CONSULTA-DADOS-CLIENTE
               IF REG-OK
                   DISPLAY TELA-CADASTRO-CLIENTE
                   ACCEPT TELA-CADASTRO-CLIENTE
                   PERFORM CRITICA-DADOS-CLIENTE
                   IF ARQ-OK
                      REWRITE WID-ARQ-CLIENTE
                         INVALID KEY
                         MOVE "ERRO AO GRAVAR ARQUIVO CLIENTE" TO WS-MSG
                         MOVE ZEROS TO WS-REG
                      END-REWRITE
                   END-IF
                END-IF
               IF NOT ARQ-OK OR NOT REG-OK
                   DISPLAY TELA-ERRO
                   ACCEPT TELA-ERRO
                   DISPLAY TELA-MSG
               END-IF
               PERFORM MONTA-TELA-CADASTRO-CLIENTE
               MOVE SPACES TO WID-ARQ-CLIENTE
           END-PERFORM.
           EXIT.
       CONSULTA-DADOS-CLIENTE SECTION.
           IF NOT COB-CRT-STATUS = COB-SCR-ESC
               READ ARQ-CLIENTE
                   INVALID KEY
                   MOVE "CLIENTE NAO LOCALIZADO" TO WS-MSG
                   MOVE ZEROS TO WS-REG
               END-READ
           ELSE
               MOVE 99 to WS-RESULTADO-ACESSO
           END-IF
           EXIT.
       EXECUTA-EXCLUSAO-CLIENTE SECTION.
           PERFORM UNTIL COB-CRT-STATUS = COB-SCR-ESC
               MOVE 1 TO WS-REG
               PERFORM CONSULTA-DADOS-CLIENTE
               IF REG-OK
                   DISPLAY TELA-CADASTRO-CLIENTE
                   ACCEPT TELA-CADASTRO-CLIENTE
                   DELETE ARQ-CLIENTE
                      INVALID KEY
                      MOVE "ERRO AO DETETAR REGISTRO CLIENTE"
                           TO WS-MSG
                      MOVE ZEROS TO WS-REG
                   END-DELETE
               END-IF
               IF NOT REG-OK
                   DISPLAY TELA-ERRO
                   ACCEPT TELA-ERRO
                   DISPLAY TELA-MSG
               END-IF
               PERFORM MONTA-TELA-CADASTRO-CLIENTE
               MOVE SPACES TO WID-ARQ-CLIENTE
           END-PERFORM.
           EXIT.
       EXECUTA-IMPORTACAO SECTION.
           PERFORM UNTIL COB-CRT-STATUS = COB-SCR-ESC
           IF NOMEARQ = SPACES
              MOVE 'FAVOR INFORMAR O NOME DO ARQUIVO' TO WS-MSG
              DISPLAY TELA-ERRO
              ACCEPT TELA-ERRO
              DISPLAY TELA-MSG
           ELSE
              OPEN INPUT ARQ-IMPORTACAO
                 IF WS-RESULTADO-ACESSO = ZEROS
                    PERFORM UNTIL ARQ-FIM
                       PERFORM PROCESSA-ARQUIVO
                    END-PERFORM
                    MOVE SPACES TO WS-MSG
                 ELSE
                    IF WS-RESULTADO-ACESSO = 35
                       MOVE "ARQUIVO NAO LOCALIZADO" TO WS-MSG
                    ELSE
                       STRING 'ERRO NO ACESSO AO ARQUIVO '
                              WS-RESULTADO-ACESSO INTO WS-MSG
                       END-STRING
                    END-IF
                    DISPLAY TELA-ERRO
                    ACCEPT TELA-ERRO
                    DISPLAY TELA-MSG
                 END-IF
           END-IF
           PERFORM MONTA-TELA-ARQUIVO-IMPORTACAO
           END-PERFORM.
           EXIT.
       PROCESSA-ARQUIVO SECTION.
           MOVE ZEROS TO WS-ARQ-FIM
           READ ARQ-IMPORTACAO
               AT END
                  MOVE 1 TO WS-ARQ-FIM
               NOT AT END
                  PERFORM TRATA-REGISTRO
           END-READ
           EXIT.
       TRATA-REGISTRO SECTION.
5           MOVE ZEROS TO WS-ARQ
           MOVE COD-CLIENTE-IMP TO COD-CLIENTE
           MOVE CNPJ-CLIENTE-IMP TO CNPJ-CLIENTE
           MOVE RAZAO-CLIENTE-IMP TO RAZAO-CLIENTE
           MOVE LAT-CLIENTE-IMP TO LAT-CLIENTE
           MOVE LON-CLIENTE-IMP TO LON-CLIENTE
           PERFORM CRITICA-DADOS-CLIENTE
           IF ARQ-OK
              WRITE WID-ARQ-CLIENTE
           END-IF.
           EXIT.
       INICIALIZA-INCLUSAO-CLIENTE SECTION.
           MOVE "INCLUSAO" TO WS-CAB-PARTE1.
           MOVE "ESC PARA CANCELAR E ENTER PARA CONFIRMAR" TO WS-MSG.
           DISPLAY TELA.
           MOVE SPACES TO WID-ARQ-CLIENTE.
           EXIT.
       MONTA-TELA-PRINCIPAL SECTION.
           MOVE "MENU PRINCIPAL" TO WS-CAB-PARTE1
           PERFORM INICIALIZA-TELA.
           ACCEPT TELA-MENU-PRINCIPAL.
           EXIT.
       MONTA-TELA-TIPO-CADASTRO SECTION.
           MOVE "MENU TIPO DE CADASTRO" TO WS-CAB-PARTE1
           PERFORM INICIALIZA-TELA
           ACCEPT TELA-MENU-TIPO-CADASTRO.
           EXIT.
       MONTA-TELA-CLIENTE SECTION.
           MOVE "MENU CLIENTE" TO WS-CAB-PARTE1
           PERFORM INICIALIZA-TELA
           ACCEPT TELA-MENU-CADASTRO.
           EXIT.
       MONTA-TELA-VENDEDOR SECTION.
           MOVE "MENU VENDEDOR" TO WS-CAB-PARTE1
           PERFORM INICIALIZA-TELA
           ACCEPT TELA-MENU-CADASTRO.
           EXIT.
       MONTA-TELA-CADASTRO-CLIENTE SECTION.
           MOVE "MENU CADASTRO CLIENTE" TO WS-CAB-PARTE1
           MOVE "DIGITE ENTER PARA GRAVAR E ESC PARA SAJR" TO WS-MSG.
           MOVE 0 TO WS-ARQ
           DISPLAY TELA.
           ACCEPT TELA-CADASTRO-CLIENTE.
           EXIT.
       MONTA-TELA-CADASTRO-VENDEDOR SECTION.
           MOVE "MENU CADASTRO VENDEDOR" TO WS-CAB-PARTE1
           PERFORM INICIALIZA-TELA
           ACCEPT TELA-CADASTRO-VENDEDOR.
           EXIT.
       MONTA-TELA-RELATORIO SECTION.
           MOVE "MENU RELATORIO" TO WS-CAB-PARTE1
           PERFORM INICIALIZA-TELA
           ACCEPT TELA-MENU-RELATORIO.
           EXIT.
       MONTA-TELA-EXECUCAO SECTION.
           MOVE "MENU EXECUCAO" TO WS-CAB-PARTE1
           PERFORM INICIALIZA-TELA
           ACCEPT TELA-MENU-EXECUCAO.
           EXIT.
       MONTA-TELA-ARQUIVO-IMPORTACAO SECTION.
           MOVE SPACES TO NOMEARQ
           MOVE "MENU IMPORTACAO" TO WS-CAB-PARTE1
           MOVE "INFORME O NOME DO ARQUIVO" TO WS-MSG
           DISPLAY TELA.
           ACCEPT TELA-ARQ-IMPORTACAO.
           EXIT.
       INICIALIZA-TELA SECTION.
           MOVE "ESCOLHA A OPCAO" TO WS-MSG
           MOVE SPACES TO WS-OPCAO1 WS-OPCAO2 WS-OPCAO3
           ACCEPT WS-LINHA FROM LINES
           DISPLAY TELA.
           EXIT.
       END PROGRAM PROVACOB.
