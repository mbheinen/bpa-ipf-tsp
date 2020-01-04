C    @(#)bd0007.f	20.3 2/13/96
        subroutine init_bd0007
 
        include 'ipfinc/com007.inc'

        month(1)  = ' '
        month(2)  = '1'
        month(3)  = '2'
        month(4)  = '3'
        month(5)  = '4'
        month(6)  = '5'
        month(7)  = '6'
        month(8)  = '7'
        month(9)  = '8'
        month(10) = '9'
        month(11) = 'O'
        month(12) = 'N'
        month(13) = 'D'

        bustyp(1)  = ' '
        bustyp(2)  = 'E'
        bustyp(3)  = 'S'
        bustyp(4)  = 'C'
        bustyp(5)  = 'D'
        bustyp(6)  = 'V'
        bustyp(7)  = 'Q'
        bustyp(8)  = 'G'
        bustyp(9)  = ' ' ! type "O" deleted
        bustyp(10) = 'T'
        bustyp(11) = 'X'
        bustyp(12) = 'M'
        bustyp(13) = 'F'
        bustyp(14) = ' ' ! type "J" deleted
        bustyp(15) = ' ' ! type "K" deleted
        bustyp(16) = ' ' ! type "L" deleted
        bustyp(17) = ' ' ! type "W" deleted
 
        return
        end

c        BLOCK DATA BD0007
 
c        include 'ipfinc:com007.inc'

c        DATA MONTH/' ','1','2','3','4','5','6','7','8','9','O','N','D'/

c        DATA BUSTYP /' ','E','S','C','D','V','Q','G','O','T','X','M',
c     1               'F','J','K','L','W'/
 
c        END
