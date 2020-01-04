C    @(#)bd0001.f	20.4 7/18/96
        subroutine init_bd0001

        include 'ipfinc/com001.inc'

        lctl = 6
        comdic(1) = 'POWERF*'
        comdic(2) = 'NEXTC*'
        comdic(3) = 'END'
        comdic(4) = 'STOP'
        comdic(5) = 'FICHE*'
        comdic(6) = 'DUMP*'

        lid = 3
        iddic(1) = 'CASE*'
        iddic(2) = 'PROJ*'
        iddic(3) = 'DESC*'

        return
        end

c        BLOCK DATA BD0001
 
c        include 'ipfinc:com001.inc'

c        DATA LCTL /6/, COMDIC
c     &       /'POWERF*','NEXTC*','END','STOP','FICHE*','DUMP*'/

c        DATA LID /2/,IDDIC/'CASE*','PROJ*'/
 
c        END
