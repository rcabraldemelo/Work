      ******************************************************************
      * Author:RICARDO CABRAL DE MELO
      * Date:20.06.2019
      * Purpose:VALIDACAO DE CPF E CNPJ
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  IND                     PIC 9(02).
       01  BASE                    PIC 9(02).
       01  MULT                    PIC 9(03).
       01  SOMA                    PIC 9(03).
       01  RESTO                   PIC 9(03).
       01  DV.
           05 DV1                  PIC 9(01).
           05 DV2                  PIC 9(01).
       01  BASE-VETOR.
           05  DADO-BASE-VETOR     PIC 9(01) OCCURS 12 TIMES.
       LINKAGE SECTION.
       01 PARAMETRES.
       COPY VALIDABOOK.
       PROCEDURE DIVISION USING PARAMETRES.
       MAIN-PROCEDURE.
           PERFORM VERIFICA-ENTRADA
           IF FUNCAO = 1
               PERFORM VALIDA-DV-CPF
           ELSE
               PERFORM VALIDA-DV-CNPJ
           END-IF.
           MOVE 0 TO COD-RETORNO
           GOBACK.
       VERIFICA-ENTRADA SECTION.
       IF FUNCAO <> 1 AND FUNCAO <> 2
           MOVE 1 TO COD-RETORNO
           MOVE "FUNCAO INVALIDA" TO MSG-RETORNO
           GOBACK
       END-IF.
       IF DADO-VALIDAR NOT NUMERIC
           MOVE 2 TO COD-RETORNO
           MOVE "CPF/CNPJ NAO NUMERICO" TO MSG-RETORNO
           GOBACK
       END-IF.
       IF DADO-BASE-INVALIDO
           MOVE 3 TO COD-RETORNO
           MOVE "CPF/CNPJ INVALIDO" TO MSG-RETORNO
           GOBACK
       EXIT.
       VALIDA-DV-CPF SECTION.
       MOVE DADO-BASE TO BASE-VETOR
       MOVE 12 TO IND
       MOVE ZEROS TO SOMA
       PERFORM UNTIL IND = 0
           COMPUTE BASE = 14 - IND
           COMPUTE SOMA = SOMA + DADO-BASE-VETOR(IND) * BASE
           COMPUTE IND = IND - 1
       END-PERFORM.
       COMPUTE RESTO = FUNCTION MOD (SOMA,11)
       IF RESTO < 2
           MOVE 0 TO DV1
       ELSE
           COMPUTE DV1 = 11 - RESTO
       END-IF.
       MOVE 12 TO IND
       MOVE ZEROS TO SOMA
       PERFORM UNTIL IND = 0
           COMPUTE BASE = 15 - IND
           COMPUTE SOMA = SOMA + DADO-BASE-VETOR(IND) * BASE
           COMPUTE IND = IND - 1
       END-PERFORM.
       COMPUTE SOMA = SOMA + DV1 * 2
       COMPUTE RESTO = FUNCTION MOD (SOMA,11)
       IF RESTO < 2
           MOVE 0 TO DV2
       ELSE
           COMPUTE DV2 = 11 - RESTO
       END-IF.
           IF DV <> DADO-DV
              MOVE 4 TO COD-RETORNO
              MOVE "DIGITO VERIFICADOR DO CPF INVALIDO"
                   TO MSG-RETORNO
              GOBACK
           END-IF.
       EXIT.
       VALIDA-DV-CNPJ SECTION.
       MOVE DADO-BASE TO BASE-VETOR
       MOVE 12 TO IND
       MOVE ZEROS TO SOMA
       PERFORM UNTIL IND = 0
           COMPUTE BASE = 14 - IND
           IF IND < 5
              COMPUTE BASE = BASE- 8
           END-IF
           COMPUTE SOMA = SOMA + DADO-BASE-VETOR(IND) * BASE
           COMPUTE IND = IND - 1
       END-PERFORM.
       COMPUTE RESTO = FUNCTION MOD (SOMA,11)
       IF RESTO < 2
           MOVE 0 TO DV1
       ELSE
           COMPUTE DV1 = 11 - RESTO
       END-IF.
       MOVE 12 TO IND
       MOVE ZEROS TO SOMA
       PERFORM UNTIL IND = 0
           COMPUTE BASE = 15 - IND
           IF IND < 6
              COMPUTE BASE = BASE- 8
           END-IF
           COMPUTE SOMA = SOMA + DADO-BASE-VETOR(IND) * BASE
           COMPUTE IND = IND - 1
       END-PERFORM.
       COMPUTE SOMA = SOMA + DV1 * 2
       COMPUTE RESTO = FUNCTION MOD (SOMA,11)
       IF RESTO < 2
           MOVE 0 TO DV2
       ELSE
           COMPUTE DV2 = 11 - RESTO
       END-IF.
           IF DV <> DADO-DV
              MOVE 4 TO COD-RETORNO
              MOVE "DIGITO VERIFICADOR DO CNPJ INVALIDO"
                   TO MSG-RETORNO
              GOBACK
           END-IF.
       EXIT.
       END PROGRAM VALIDA.
