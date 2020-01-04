C    @(#)srchtx.f	20.3 2/13/96
        integer function srchtx (dummy)
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/data.inc'
C
        character area*10,zone*2
        integer znsrch, arsrch, bsrch2
        save

        srchtx = 0
        return
C
C       SEARCH FOR AREAS
C
        entry arsrch (area)
        i1 = 1
        i2 = idat
  100   ix = (i1 + i2)/2
        if (kompr(area,savare(ix),junk)) 110,140,120
  110   i2 = ix - 1
        go to 130
  120   i1 = ix + 1
  130   if (i1.le.i2) go to 100
        ix=0
  140   arsrch=ix
        return
C
C       SEARCH FOR ZONES
C
        entry znsrch(zone)
        i1=1
        i2=idat
  150   ix=(i1+i2)/2
        if (kompr(zone,savzns(ix),junk)) 160,190,170
  160   i2=ix-1
        go to 180

  170   i1=ix+1
  180   if (i1.le.i2) go to 150
        ix=0
  190   znsrch=ix
        return
C
C       SEARCH FOR BASES
C
        entry bsrch2(base)
        i1=1
        i2=idat
  200   ix=(i1+i2)/2
        if (base-savbas(ix)) 210,240,220
  210   i2=ix-1
        go to 230
  220   i1=ix+1
  230   if(i1.le.i2) go to 200
        ix=0
  240   bsrch2=ix
        return
        end
