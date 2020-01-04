C    @(#)sys100.f	20.3 2/13/96
      subroutine sys100
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/merge.inc'
      include 'ipfinc/mrgsys.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/red2.inc'
 

      dimension mtrx(MAXBUS)
      character ktrpos*1,own1*3,own2*3
      integer find_bus  
      external find_bus, kpface, spface
      logical found

      ksy = ksy +1

C     Establish connection matrix: add 2-terminal dc to y-matrix

      do i = 1,kdtot  
         k1 = dc2t(1,i)   
         k2 = dc2t(3,i)   
         kt = inp2opt(k1)
         mt = inp2opt(k2)

C        Add branch (KT,MT) to y-matrix   

         do isw = 1, 2
            ln = km(kt) - 1
            ls = kmlen(kt)
            do l = 1, ls
               if (ikmu(l+ln) .eq. mt) go to 102
            enddo
c
c           Appending an entity to km() requires relocating branch
c           list to end of km().
c
            ln = km(kt) - 1
            ls = kmlen(kt)
            do l = 1, ls
               ikmu(l+yptr) = ikmu(l+ln)
            enddo
            ikmu(ls+yptr+1) = mt
            yptr = yptr + ls + 1
            km(kt) = yptr 
            kmlen(kt) = ls + 1

  102       continue
            kt = inp2opt(k2)    ! swap kt and mt
            mt = inp2opt(k1)
         enddo
      enddo

C     Add N-terminal d-c to y-matrix

      do i=1,mtdcln   
         k1=dcmtln(1,i)
         k2=dcmtln(2,i)
         kt = inp2opt(k1)
         mt = inp2opt(k2)

         do isw = 1, 2
            ln = km(kt) - 1
            ls = kmlen(kt)
            do l = 1, ls
               if (ikmu(l+ln) .eq. mt) go to 104
            enddo
c
c           Appending an entity to km() requires relocating branch
c           list to end of km().
c
            ln = km(kt) - 1
            ls = kmlen(kt)
            do l = 1, ls
               ikmu(l+yptr) = ikmu(l+ln)
            enddo
            ikmu(ls+yptr+1) = mt
            km(kt) = yptr + 1
            kmlen(kt) = ls + 1
            yptr = yptr + ls + 1

  104       continue
            kt = inp2opt(k2)    ! swap kt and mt
            mt = inp2opt(k1)
         enddo
      enddo
C
c     IKK(1,*) = 0 - bus is in eliminated subsystem.  
c                1 - bus is in retained subsystem.  
c
c     IKK(2,*) = 0 - bus is not specifically classified. 
c                1 - bus is specifically classified. 
C
c     IKK(3,*) = isystm - subsystem number
C
c     IKKIND(1,*) = is - starting KOLUM() index   
c     IKKIND(2,*) = il - KOLUM length
C
      itot=1
      do kt=1,ntot  
         k=inp2opt(kt)   
         jtot=0
         do l = km(k), km(k)-1+kmlen(k)      
            mt = ikmu(l)                                 
            kolum(itot+jtot)=opt2inp(mt)
            jtot=jtot+1   
         enddo
         ikk(1,kt)=1
         ikk(2,kt)=0   
         ikk(3,kt)=0   
         ikk(4,kt)=0   
         ikkind(1,kt)=itot 
         ikkind(2,kt)=jtot 
         itot=itot+jtot
      enddo

      if (itot .gt. MAXYE) then
        write (errbuf(1),330) itot, MAXYE
  330   format ('Total branches in system (including transposes)',  
     1          'is ',i5,'.  Limit is ',i5,'.')
        call prterx ('F',1) 
        kerrsw = kerrsw + 1 
      endif

C     Identify merge subsystem

      ntotx=max0(nbsys1,nbsys2) 
      do 370 i=1,ntotx  
         kt = find_bus(mrgbus(i),mrgbas(i))
         if (kt .le. 0) then
            if (mrgbus(i) .ne. srtlst) then
               write (errbuf(1),350) mrgbus(i),mrgbas(i)
  350          format ('Merged subsystem bus (',a8,f6.1,
     &              ') is not in system.  Bus ignored. ')
               call prterx ('W',1)
            endif
         else
            ikk(1,kt)=0
         endif 
  370 continue
C
C     Identify all interface branches
C
      nsave=0
 
      do 380 i=1,ntot
         if (ikk(1,i).eq.1) nsave=nsave+1
  380 continue
 
      if (nsave .eq. 0) then
         write (errbuf(1),390)
  390    format ('0 Caution - Merge and Base system are identical. ',
     &        'Interface ignored. ')
         call prterx ('W',1)   
         go to 580 
      endif

      nsyst=0
      itface=0
      na=0  
      nl=0  
      do while (nl .le. nsave)
c
c        Search for kernel node KT to prime topological emanation
c
         kt = 1
         found = .false.
         do while (kt .le. ntot .and. .not. found)
            if (ikk(1,kt) .eq. 1 .and. ikk(3,kt) .eq. 0) then
               found = .true.
            else
               kt = kt + 1
            endif
         enddo
         if (.not. found) then
            write (errbuf(1), 400)
  400       format ('Base merge failed to establish interface ')
            call prterx ('F',1) 
            kerrsw = kerrsw + 1 
            go to 900
         endif
c
c        Add node KT as first node in subsystem NSYST
c
         na=na+1   
         mtrx(na)=kt   
         nl=na 
         nsyst=nsyst+1 
         ikk(3,kt)=nsyst   
         do while (nl .le. na)
            kt = mtrx(nl)
            i5 = ikkind(1,kt)   
            i6 = i5+ikkind(2,kt)-1  
            do 470 l=i5,i6
               mt=kolum(l)   
               if (ikk(1,mt) .eq. 0) then
                  ikk(2,kt)=1  
                  ikk(2,mt)=1   
                  do 430 i=1,itface 
                     if (face(1,i) .eq. kt .and. 
     &                   face(2,i) .eq. mt) go to 470
  430             continue  
                  itface=itface+1   
                  if (itface .ge. 399) then
                     write (errbuf(1),450) bus(kt), base(kt), bus(mt),
     &                  base(mt)   
  450                format (' More that 400 merge interface branches',
     &                  ' branch ignored :(',a8,f6.1, 2x,a8,f6.1,')')
                     call prterx ('W',1)   
                     itface = 399
                     kerrsw = kerrsw + 1
                  else
                     face(1,itface)=kt 
                     face(2,itface)=mt 
                     facec(itface)='0' // owner(kt) // owner(mt)   
                     face(4,itface)=nsyst  
                     itface=itface+1   
                     face(1,itface)=mt 
                     face(2,itface)=kt 
                     facec (itface)= '1' // owner(mt) // owner(kt) 
                     face(4,itface)=nsyst  
                  endif
               else if (ikk(3,mt) .eq. 0) then
c
c                 Add node MT as subsequent node in subsystem NSYST
c
                  na=na+1   
                  mtrx(na)=mt   
                  ikk(3,mt)=nsyst   
               endif
  470       continue  
            nl=nl+1   
         enddo
      enddo
      if (nl .lt. nsave) then
         write (errbuf(1), 480)
  480    format ('Base merge failed with an incomplete enclosure ')
         call prterx ('F',1) 
         kerrsw = kerrsw + 1 
         go to 900
      endif

C     END PROCEDURE

  490 continue  

C     SUMMARIZE "FACE" ARRAY BY SUBSYSTEMS 

      if (itface .eq. 0) go to 580

      if (itface .gt. 1) then
         key=1   
         call qiksrt(1,itface,kpface,spface) 
      endif
      nf=1  
      do 570 is=1,nsyst 
         write (outbuf,520) is 
  520    format ('0 Summary of Interface Branches for Subsystem ',i2)  
         call prtout(1)
         write (outbuf,530)
  530    format ('0 Base System     Merge System    Owner1 Owner2 ')   
         call prtout(1)  
         do 550 i=nf,itface
            if (face(4,i).ne.is) go to 560
            kt=face(1,i)  
            mt=face(2,i)  
            ktrpos=facec(i)(1:1)
            own1=facec(i)(2:4)  
            own2=facec(i)(5:7)  
            if (ktrpos .ne. '1') then
               write (outbuf,540) bus(kt), base(kt), bus(mt), base(mt),
     &            own1,own2
  540          format (2x,a8,f6.1,2x,a8,f6.1,3x,a3,5x,a3)
               call prtout(1)  
            endif
  550    continue  
         i=itface+1
  560    nf=i  
  570 continue  
  580 continue  
  900 continue
      return
      end   
