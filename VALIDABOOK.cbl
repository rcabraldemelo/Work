     ***********************************************
     **BOOK PARA A ROTINA VALIDA
     ***********************************************
         05 ENTRADA.
           10 FUNCAO                   PIC 9(01).
           10 DADO-VALIDAR.
              15 DADO-BASE             PIC 9(12).
                 88 DADO-BASE-INVALIDO VALUE IS 000000000000
                                                111111111111
                                                222222222222                                            333333333333
                                                444444444444
                                                555555555555
                                                666666666666
                                                777777777777
                                                888888888888
                                                999999999999
                                                000111111111
                                                000222222222
                                                000333333333
                                                000444444444
                                                000555555555
                                                000666666666
                                                000777777777
                                                000888888888
                                                000999999999.
              15 DADO-DV               PIC 9(02).
         05 SAIDA.
           10 COD-RETORNO              PIC 9(01).
           10 MSG-RETORNO              PIC X(80).
