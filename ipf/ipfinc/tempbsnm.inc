
c      Tempory Remote control bus name data
c      buscb(i)  basecb(i)  itype(i)   ixndx(i)
c
c      busname   basekv        1         jx : kbsdta(9,jx) = -i or
c                                             kbsdta(13,jx) = -i
c                              2         jx : kbctbl(1,jx) = -i
c                              3         jx : kxdata(1,jx) = -i or
c                                             kxdata(2,jx) = -i
        integer MXBSTP
        parameter ( MXBSTP = MAXBUS/2 )
 
        common /tempbsnm/ ibuscb, basecb(MXBSTP), itype(MXBSTP),
     &                    ixndx(MXBSTP)
        common /tempbsnmc/ buscb(MXBSTP)
        character buscb*8
 
