C    @(#)chgrat.f	20.3 2/13/96
      subroutine chgrat
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cgrate.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
C
      integer find_bus

      character bus1*8,bus2*8,id*1,idc*1

      external find_bus, kmprt2, swprt2
      save
C
C       Read in and sort summer/winter ratings
C
        write (outbuf,40) buf(1:80)
   40   format(' OUTAGE TEXT(',a,')')
        call prtout(1)
 
   20   read (inp,30,end=142) buf
   30   format(a)
 
        if (index('(HC/<',buf(1:1)).ne.0) go to 150
 
        read (buf,100) card,bus1,base1,bus2,base2,idc,rat1,rat2,rat3
  100   format (bz, a1, 5x, a8, f4.0, 1x, a8, f4.0, a1, 1x, 3f5.0)
        j1=find_bus(bus1,base1)
        j2=find_bus(bus2,base2)
 
        if  (j1 .le. 0 .or. j2 .le. 0) then
           write (errbuf(1),110)
  110      format(' Summer/winter rating branch is not in system')
           write (errbuf(2),120) buf(1:80)
  120      format(2x,'(',a80,')')
           call prterx ('W',2)
        else
           nrat=nrat+1
           if (nrat.gt.400) then
              write (errbuf(1),130)
  130         format('More than 400 line rating changes. Remaining',
     1               ' records ignored.')
              write (errbuf(2),140) buf(1:80)
  140         format(2x,'(',a80,')')
              call prterx ('W',2)
           else
              lrat(1,nrat)=j1
              lrat(2,nrat)=j2
              lrat(3,nrat)=ichar(idc)
           endif
        endif
        go to 20
 
  142   buf='( END ) CHGRAT'
 
C       Sort data items
 
  150   nrat=min0(nrat,400)
        card=buf(1:1)
        if (nrat.gt.0) call qiksrt(1,nrat,kmprt2 ,swprt2 )
        return
 
C       Substitute new branch rating if found.
 
        entry newrat(k1,k2,id,rate)
 
        k1x=min0(k1,k2)
        k2x=max0(k1,k2)
        n1=1
        n2=min0(nrat,400)
  160   if(n1.gt.n2) go to 230
        n=(n1+n2)/2
        if (k1x-lrat(1,n))190,170,200
  170   if(k2x-lrat(2,n)) 190,180,200
  180   if(ichar(id)-lrat(3,n)) 190,210,200
  190   n2=n-1
        go to 160
 
  200   n1=n+1
        go to 160
 
  210   if(kase1(35).eq.0) then
           ratnew=rat(2,n)
        else
           ratnew=rat(3,n)
        endif
 
        write (outbuf,220) bus(k1),base(k1),bus(k2),base(k2),id,
     1  rate,ratnew
  220   format(' CHANGED RATING OF BRANCH ',a8,f6.1,2x,a8,f6.1,1x,a1
     1         ,' FROM ',f5.0,' TO ',f5.0)
        call prtout(1)
        rate=ratnew
 
  230   return
        end
