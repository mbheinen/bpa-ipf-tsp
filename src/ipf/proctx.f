C    @(#)proctx.f	20.4 2/13/96
	subroutine proctx
 
       	include 'ipfinc/parametr.inc'

       	include 'ipfinc/alpha.inc'
      	include 'ipfinc/blank.inc'
      	include 'ipfinc/bus.inc'
      	include 'ipfinc/ikk.inc'
      	include 'ipfinc/merge.inc'
      	include 'ipfinc/prt.inc'
      	include 'ipfinc/qksrt.inc'
      	include 'ipfinc/red2.inc'
 
        common /is_batch / is_batch

        external kpface,spface
        dimension mtrx(MAXBUS)
 
C       INITIALIZE "IKK"
 
        do 100 kt=1,ntot
           ikk(4,kt)=0
           ikk(5,kt)=0
  100   continue

C       BEGIN PROCEDURE: IDENTIFY INTERIOR NODES OF "INTERFACE BRANCHES

        if (itface.eq.0) go to 200  
        nx=0
        nl=0
        level=1 
        do i=1,itface   
           kt=face(1,i) 
           ikk(5,kt)=level  
           nx=nx+1  
           mtrx(nx)=kt  
        enddo
C                   PROPAGATE OUTWARDS FROM KERNEL SUBSYSTEM
  120   nf=nl+1 
        nl=nx   
        if (nf.gt.nl) go to 180 
        level=level+1   
        do 170 l=nf,nl  
           kt=mtrx(l)   
           if(ikk(1,kt) .le. 0) call erexit 
           i5=ikkind(1,kt)  
           i6=ikkind(2,kt)  
           do 160 i=1,i6
           mt=kolum(i+i5-1) 
           if (ikk(5,mt) .gt. 0) then   
           else if (ikk(1,mt) .eq. 1) then  
              nx=nx+1   
              mtrx(nx)=mt   
              ikk(1,mt)=1   
              ikk(2,mt)=1   
              ikk(5,mt)=level   

C          Note: The following test determines if the adjacent node 
C          MT is unprocessed.  If true, it is changed into a 
C          retained (interior) node.  If false, the node is 
C          tested to determine whether it is specifically a 
C          border node. If this subtest is false, an incomplete 
C          enclosure is detected.  
c
           else if (ikk(1,mt) .eq. 0 .and. ikk(2,mt) .eq. 0) then   
              nx=nx+1   
              mtrx(nx)=mt   
              ikk(1,mt)=1   
              ikk(2,mt)=1   
              ikk(5,mt)=level   
           else if (ikk(1,mt) .le. 0) then  
              if (level .gt. 2) then
                 jlevel=level-1 
                 write (errbuf(1),150) bus(kt),base(kt),jlevel,bus(mt), 
     1                  base(mt)
  150            format ('INCOMPLETE INTERFACE ENCLOSURE.  INTERIOR '   
     1           ,'BUS (',a8,f6.1,') IS ',i2,' NODES ADJACENT ',
     2           'TO EXTERIOR BUS (',a8,f6.1,')')   
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 kerrsw=kerrsw+1
              endif 
           endif
  160      continue 
        
  170   continue
        go to 120   
        
  180   do 190 i=1,ntot 
        ikk(5,i)=0  
  190   continue
        
C       END PROCEDURE   
        
C       BEGIN PROCEDURE: IDENTIFY ALL INTERFACE BRANCHES
        
  200 nsyst=0   
      nsave=0   
      do 210 i=1,ntot   
         ikk(2,i)=0
         ikk(4,i)=0
         ikk(5,i)=0
         if (ikk(1,i) .gt. 0) nsave=nsave+1
  210 continue
      if (nsave.gt.0) go to 230
      write (errbuf(1),220)
  220 format ('NO BUSES IN RETAINED SUBNETWORK.')
      if (is_batch .eq. 0) then
         call prterx ('E',1)
      else
         call prterx ('F',1)
      endif
      go to 410

  230 continue
      na=0
      nl=0
      do 310 jt=1,ntot
      kt=jt
      i5=ikkind(1,kt)
      i6=ikkind(2,kt)
      if (ikk(1,kt).le.0) go to 310
      if (ikk(4,kt).ne.0) go to 310
      na=na+1
      nl=na
      mtrx(na)=kt
      nsyst=nsyst+1
      ikk(4,kt)=nsyst
  240 do 300 l=1,i6
      mt=kolum(l+i5-1)
      if (ikk(1,mt).le.0) go to 250
      if (ikk(4,mt).ne.0) go to 300
      na=na+1
      mtrx(na)=mt
      ikk(4,mt)=nsyst
      go to 300
 
  250 ikk(2,kt)=1
      ikk(2,mt)=1
      do 260 i=1,itface
         if (face(1,i).ne.kt) go to 260
         if (face(2,i).ne.mt) go to 260
         face(4,i) = nsyst
         go to 300
  260 continue
      itface=itface+1
      if (itface .le. 200) then
         face(1,itface)=kt
         face(2,itface)=mt
         ifcsw = ikk(3,kt)
         face(3,itface)=ifcsw
         face(4,itface)=nsyst
      else
         write (errbuf(1),280) bus(kt),base(kt),bus(mt),base(mt)
  280    format ('EXCESS "INTERFACE BRANCHES" IGNORED (',a8,f6.1,2x,a8
     1           ,f6.1,')')
         call prterx ('W',1)
         itface=200 
      endif 
  300 continue  
      nl=nl+1   
      if (nl.gt.nsave) go to 320
      if (nl.gt.na) go to 310
      kt=mtrx(nl)
      i5=ikkind(1,kt)
      i6=ikkind(2,kt)
      go to 240
 
  310 continue
      call erexit
C
C     END PROCEDURE
C
  320 continue
      kdupsw=2
C
C     SUMMARIZE "FACE" ARRAY BY SUBSYSTEMS
C
      if (itface-1) 410,340,330
  330 key = 1
      call qiksrt (1,itface,kpface,spface)
  340 continue
      nf = 1
      do 400 is = 1,nsyst   
      write (outbuf,350) is 
  350 format ('0 SUMMARY OF INTERFACE BRANCHES FOR SUBSYSTEM ',i2)  
      call prtout(1)
      write (outbuf,360)
  360 format ('0 RETAINED BUSES  ELIMINATED BUSES  INTERFACE LEVEL ')   
      call prtout(1)
      outbuf = ' '  
      call prtout(1)
      if (nf.gt.itface) then
          write (errbuf(1),362) is  
  362     format('0 MERGE SUBSYSTEM ',i2,' CONSISTING OF THE FOLLOWING',
     1           ' BUSES HAS NO INTERFACE BRANCHES.')
          errbuf(2)=' '
          call prterx ('W',2)
          knt=1
          outbuf=' '
          do 366 kt=1,ntot  
          if(ikk(4,kt).ne.is) go to 366 
          write (outbuf(knt:knt+19),364) bus(kt),base(kt)   
  364     format(5x,a8,f7.1)
          knt=knt+20
          if (knt.lt.101) go to 366 
          call prtout (1)   
          knt=1 
          outbuf=' '
  366     continue  
          if(knt.gt.1) call prtout (1)  
       else 
          do 380 i=nf,itface
             if (face(4,i).ne.is) go to 390
             kt = face(1,i)
             mt = face(2,i)
             write (outbuf,370) bus(kt),base(kt),bus(mt),base(mt)  
  370        format (2x,a8,f6.1,2x,a8,f6.1,10x,i2) 
             if (ikk(1,mt) .lt. 0) then
                outbuf(50:) = '(INTERFACE IS IGNORED)' 
             endif 
             call prtout (1)   
  380     continue  
          i=itface + 1  
  390     nf=i  
      endif 
  400 continue  
  410 continue  
      return
      end   
