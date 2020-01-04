C    @(#)bd0004.f	20.3 2/13/96
        subroutine init_bd0004

        include 'ipfinc/com004.inc'

        lctl = 11
        dict(1)  = 'XXXXXXX'
        dict(2)  = 'WSCC'
        dict(3)  = 'DEBUG'
        dict(4)  = 'SAVEZONE*'
        dict(5)  = 'SAVEBASE*'
        dict(6)  = 'SAVEBUS*'
        dict(7)  = 'INCLUDEB*'
        dict(8)  = 'EXCLUDEB*'
        dict(9)  = 'PIBACK*'
        dict(10) = 'CUTBR*'
        dict(11) = 'INCLUDEC*'
 
        return
        end

c        BLOCK DATA BD0004
 
c        include 'ipfinc:com004.inc'

c        DATA LCTL /11/ ,DICT/ 'XXXXXXX', 'WSCC', 'DEBUG',
c     1       'SAVEZONE*', 'SAVEBASE*', 'SAVEBUS*', 'INCLUDEB*',
c     2       'EXCLUDEB*', 'PIBACK*', 'CUTBR*', 'INCLUDEC*'/
 
c        END
