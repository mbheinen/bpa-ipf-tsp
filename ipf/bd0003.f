C    @(#)bd0003.f	20.4 7/18/96
        subroutine init_bd0003

        include 'ipfinc/com003.inc'

        lctl = 4
        comdic(1) = 'CUTT*'
        comdic(2) = 'END'
        comdic(3) = 'FICHE*'
        comdic(4) = 'DUMP*'

        lid = 3
        iddic(1) = 'CASE*'
        iddic(2) = 'PROJ*'
        iddic(3) = 'DESC*'
 
        return
        end

c        BLOCK DATA BD0003

c        include 'ipfinc:com003.inc'

c        DATA LCTL /4/, COMDIC /'CUTT*', 'END', 'FICHE*', 'DUMP*'/

c        DATA LID /2/, IDDIC /'CASE*','PROJ*'/
 
c        END
