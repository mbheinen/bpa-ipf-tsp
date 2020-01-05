C    @(#)chknet.f	20.3 2/13/96
      subroutine chknet (ktot, krem, amtrx)
      dimension amtrx(*) 
      double precision amtrx

C     Determines the number of subnetworks present in a system
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		km, kmlen, ikmu,
      include 'ipfinc/blank.inc'
c	Global variables used:
c		ksy
      include 'ipfinc/bus.inc'
c	Global variables used:
c		None
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbus, intbas
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf, errbus
      include 'ipfinc/red7.inc'
c	Global variables used:
c		None
c
      common /scratch/ kolum(MAXBUS),net(200)


      krem = 0       ! DETERMINE SUBNETWORK NUMBER OF EACH BUS  
      ksy = 0 
      do 150 k=1,ktot   
         kolum(k)=0
         ksy = max0 (ksy, km(k)+kmlen(k))  
  150 continue  
C       
C     Expand outwards in eliminated network from a kernel node  
C       
      knt=0 
 
  160 if (knt.ne.0) net(knt)=klast
      knt=knt+1
      do 210 k=1,ktot
         if (kolum(k).eq.0) go to 220
  210 continue
      go to 280
 
  220 if (knt .gt. 200) then
         write (errbuf(1)(1:120),230)
  230    format (' MORE THAN 200 SUBNETWORKS FOUND')
         call prterx ('W',1)
         call erexit
      endif
      klast=1
      amtrx(klast)=k
      kolum(k)=knt
      knext=0
      do while (knext .lt. klast)
         knext = knext + 1
         k = amtrx(knext)
         ls = kmlen(k)                                
         is = km(k) - 1                                
         do l=1,ls 
            m = ikmu(is+l)                                
            if (kolum(m) .eq. 0) then
               klast=klast+1 
               if (klast.gt.MAXBUS) call erexit  
               amtrx(klast)=m 
               kolum(m)=knt  
            endif
         enddo
      enddo
      go to 160

  280 knt = knt - 1 
      if (knt.eq.1) go to 900   
      write (outbuf,300) case1(31),knt  
  300 format ('0 MIN_EQUIV_Y (',e10.3,') has created ',i3,
     &        ' isolated sub systems with populations: ')
      call prtout (1)   
      do 320 kst=1,knt,10   
         kend=min0 (kst+9,knt) 
         write (outbuf,310) (i,net(i),i=kst,kend)  
  310    format (10(i8,i4))
         call prtout (1)   
  320 continue  
      max = 1   
      do 330 i = 1,knt  
         if (net(i).gt.net(max)) max = i   
  330 continue  
      do 430 i = 1,knt  
         if (i.eq.max) go to 430   
         write (outbuf,334) i  
  334    format ('0 Composition of subnetwork ',i3)
         call prtout (1)   
         do 420 kt = 1,ktot
            if (kolum(kt).ne.i) go to 420 
            write (outbuf,410) intbus(kt),intbas(kt)  
  410       format (16x,a8,f6.1)  
            call prtout (1)   
  420    continue  
  430 continue  
      do 440 kt = 1,ktot
         if (kolum(kt).ne.max) then
           krem = krem + 1
           amtrx(krem) = kt
         endif
  440 continue
  900 continue
      return
      end   
