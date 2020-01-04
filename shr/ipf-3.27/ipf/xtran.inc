C
        common /xtran/ xtrflg, xtran(2,MAXLTC)
        integer xtrflg, xtran
C
C       XTRFLG = 0 - LTRAN(3,*), LTRAN(10,*) refer to /ECS/
C                    XTRAN(1,*), XTRAN(10,*) is undefined.
C
C       XTRFLG = 1 - LTRAN(3,*), LTRAN(10,*) refer to /ALPHA2/
C                    XTRAN(1,*), XTRAN(10,*) refer to /ECS/
C
C                2 - LTRAN(3,*), LTRAN(10,*) refer to /ECS/
C                    XTRAN(1,*), XTRAN(10,*) refer to /ALPHA2/
C
