C    @(#)arezln.f	20.5 3/29/99
        subroutine arezln (iarea,kadata)
c      
c         Things to do 
c      
c         23-Feb: Remove unused caps/reactors from BUSDTA.  Use CAPCOR 
c                 in REDUCT
c      
c         28-Feb: Improve multi-terminal d-c extension from d-c bus to 
c                 commutator bus.  If the d-c bus is a passive node, it
c                 has no commutator bus.  This error is unchecked. 
C       
C         This subroutine append zero-loss networks to eliminated   
C         areas.
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		gkmu (r*8), bkmu(r*8), gkku(r*8), bkku(r*8), pnetu(r*8), 
c		qnetu(r*8), inetr(r*8), ineti(r*8),ploadu(r*8), qloadu(r*8), 
c		vlimn(r*4), vlimx(r*4),km, kmlen, ikmu, ntypu
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
c	Global variables used:
c		karea(i*4), tie(r*8), jarzn,  
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8), busdta, kbsdta, inp2opt, opt2inp, base, 
c		zone, owner
      include 'ipfinc/dc2t.inc'
c	Global variables used:
c		dc2t(r*8)
      include 'ipfinc/dcmt.inc'
c	Global variables used
c		dcmtbs (r*8)
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red7.inc'
c	Global variables used:
c		yred(r*8)
      include 'ipfinc/reic.inc'

        common /mtrx/ mtrx(2,MAXBUS)
 
        dimension mbr(50), kadata(1), ai1(2,3), ktemp(MAXBUS),
     1            intset(100)
c
c	Put any local variables changed from single to double precision here.
c
        complex v(100), vzln
        double precision ai1
        integer start, first, count
        character reinam*8
        character chtemp *1
        external kmpmtx, swpmtx
        logical found, quit_zsplit
C
        quit_zsplit = .true.
        numzln = 0
        ntotx = ntot
        ksol = MAXYE + MAXBUS 
C       
C       Initialize KTEMP array  
C       
        do i = 1,ntot   
          ktemp(i) = 0
        enddo
C       
C       Generate unique REI name   
C       
        i = ichar('A')
        found = .false.
        do while (i .le. ichar('Z') .and. .not. found)
        
           chtemp = char(i)
           reinam = 'EQ' // chtemp 
        
           i1 = 1  
           i2 = ntot   
           do while (i1 .le. i2 .and. .not. found) 
              ix = (i1 + i2) / 2  
              komp = kompr(bus(ix)(1:4),reinam(1:4),junk)
              if (ix .lt. 0) then
                 i1 = ix + 1 
              else if (ix .gt. 0) then
                 i2 = ix - 1 
              else
                 found = .true.
              endif
           enddo
           i = i + 1
        enddo
        if (.not. found) then
           write (errbuf(1), 80)
   80      format('0 Program error module AREZLN - could not generate un
     &ique area equivalent name')
           call prterx ('W',1)
           return
        endif        

        if (jtie .eq. 0 .or. ntotc .eq. 0) then 
           do i = 1, ntot   
              jarzn(i) = 0   
           enddo
        endif   
        
        do i = 1, ntot   
           ikk(3,i) = 0
        enddo
C       
C       Identify eliminated areas:  {all areas} - {retained areas}  
C       
        do 300 jt = 1,ntotc 
           do i = 1,iarea   
              if (kadata(i) .eq. jt) go to 300 
           enddo
C       
C          Area JT is not retained.  Therefore, it is eliminated.   
C          Idenfify all subsystems in this area.
C       
           write (outbuf,112) arcnam(jt)
  112      format ('0 Area ',a10,' equivalent') 
           call prtout (1)  
           numnod = 0   
           nsystm = 0   
           do 150 i = 1,ntot
           if (jarzn(i) .eq. jt) then   
               jarzn(i) = -jt   
               nsystm = nsystm + 1  
               numnod = numnod + 1  
               numpre = numnod  
               mtrx(1,numnod) = i   
               kt = inp2opt(i)
               if (ikk(1,kt) .eq. 0) then   
                   mtrx(2,numnod) = -100*nsystm 
               else 
                   mtrx(2,numnod) = 100*nsystm  
               endif
               istart = numnod  
C       
C              Identify all internal subsytems in area JT.  
C       
  130          if (istart .le. numnod) then 
                  k = mtrx(1,istart)
                  kt = inp2opt(k) 
                  if (kmlen(kt) .gt. 0) then       
                     do 140 l = km(kt), km(kt)-1+kmlen(kt)
                     mt = ikmu(l)                  
                     m = opt2inp(mt)  
                     if (jarzn(m) .eq. jt) then 
                        jarzn(m) = -jt  
                        numnod = numnod + 1 
                        mtrx(1,numnod) = m  
                        if (ikk(1,mt) .eq. 0) then  
                           mtrx(2,numnod) = -100*nsystm 
                        else
                           mtrx(2,numnod) = 100*nsystm  
                        endif   
                     endif  
  140                continue   
                  endif 
                  istart = istart + 1   
                  go to 130 
               endif
               write (outbuf,142) nsystm, numnod-numpre+1   
  142          format (t7,'Subsystem ',i3,' number of buses ',i4)   
               call prtout (1)  
           endif
  150      continue 
C       
C          Identify set of intertie nodes pertaining to area JT 
C       
           do 180 i = 1,jtie
           k1=tie(1,i) 
           k2=tie(7,i) 
C       
C          Extend d-c border nodes to commutator buses  
C       
           if (tie(9,i) .ne. 0) then   
               do 152 j = 1,mtdcbs  
               if (dcmtbs(1,j) .eq. k1) then
                  k1 = dcmtbs(3,j)  
                  kt = inp2opt(k1)
                  ikk(1,kt) = 1 
                  go to 156 
               endif
  152          continue 
               do 154 j = 1,kdtot   
               if (dc2t(1,j) .eq. k1) then 
                  k1 = dc2t(33,j)  
                  kt = inp2opt(k1)
                  ikk(1,kt) = 1 
                  go to 156 
               else if (dc2t(3,j) .eq. k1) then
                  k1 = dc2t(34,j)  
                  kt = inp2opt(k1)
                  ikk(1,kt) = 1 
                  go to 156 
               endif
  154          continue 
               call erexit  
           endif
  156      continue 
           if (tie(2,i) .eq. jt) then  
              do 160 j = 1,numnod   
              if (mtrx(1,j) .eq. k1) then   
                 mtrx(2,j) = mtrx(2,j) 
     &                     + isign(ifix(sngl(tie(8,i))),mtrx(2,j)) 
              endif 
  160         continue  
           else if (tie(8,i) .eq. jt) then 
              do 170 j = 1,numnod   
              if (mtrx(1,j) .eq. k2) then   
                 mtrx(2,j) = mtrx(2,j) 
     &                     + isign(ifix(sngl(tie(2,i))),mtrx(2,j)) 
              endif 
  170         continue  
           else 
              go to 180 
           endif
  180      continue 
C       
C          Include area slack bus in intertie set   
C       
           k = karea(1,jt)  
           do 190 i = 1,numnod  
           if (mtrx(1,i) .eq. k) then   
              mtrx(2,i) = iabs(mtrx(2,i))   
           endif
  190      continue 
C       
C          Identify common intertie nodes via sorting   
C       
           call qiksrt (1,numnod,kmpmtx,swpmtx) 
           start = 1
        
  200      lastjt = 0   
           first = 0
           last = 0 
           num = 0  
           do 210 i = start,numnod  
           if (mtrx(2,i) .gt. 0) then   
              if (mod(mtrx(2,i),100) .gt. 0) then   
                 if (first .eq. 0) then 
                    first = i   
                    last = i
                    lastjt = mtrx(2,i)  
                    mtrx(2,i) = 0   
                 else if (mtrx(2,i) .eq. lastjt) then   
                    last = i
                    mtrx(2,i) = 0   
                 else   
                    if (first .eq. 0) then  
                       first = i - 1
                       last = i - 2 
                    endif   
                    go to 211   
                 endif  
                 k = mtrx(1,i)  
                 kt = inp2opt(k)  
                 num = num + 1  
                 intset(num) = k
                 v(num) = cmplx(e(kt),f(kt))
              endif 
           endif
  210      continue 
           if (first .eq. 0) first = numnod + 1 
  211      do 212 i = 1,num 
C       
C             Change intertie buses bus to types BJ, BK, or BL. 
C       
              k1 = intset(i)
              kt = inp2opt(k1)
              ntyp = ntypu(kt)                     
              ntypo = ntyp  
              if (ntyp .eq. 1 .or. ntyp .eq. 4 .or. ntyp .eq. 10) then  
                 ntyp = 14  
              else if (ntyp .eq. 2 ) then   
                 ntyp = 15  
              else if (ntyp .eq. 7 .or. ntyp .eq. 8 .or. ntyp .eq. 9
     1            .or. ntyp .eq. 11) then   
                 ntyp = 16  
              endif 
              if (ntyp .ne. ntypo) then 
                 ntypu(kt) = ntyp  
                 vlimn(kt) = cabs(v(i))   
                 vlimx(kt) = vlimn(kt)  
                 kbsdta(1,k1) = ntyp
                 busdta(11,k1) = vlimn(kt)        
                 busdta(11,k1) = vlimn(kt)        
                 busdta(12,k1) = datan2 (f(kt),e(kt))
              endif 
  212      continue 
           if (num .gt. 1) then 
C       
C             Exclude from the set INTSET all nodes which contain no
C             interior, eliminated nodes.   
C       
              count = 0 
              do 216 i = 1,num  
              k1 = intset(i)
              kt = inp2opt(k1)
              do 214 l = km(kt), km(kt)-1+kmlen(kt)   
              mt = ikmu(l)                         
              m = opt2inp(mt) 
              if (iabs (jarzn(m)) .eq. jt .and. ikk(1,mt) .eq. 0) then  
                 go to 216  
              endif 
  214         continue  
              count = count + 1 
              intset(i) = 0 
  216         continue  
C       
C             Compress deleted entities 
C       
              if (count .ne. 0) then
                 ilast = 0  
                 do 218 i = 1,num   
                 if (intset(i) .ne. 0) then 
                    ilast = ilast + 1   
                    intset(ilast) = intset(i)   
                    v(ilast) = v(i) 
                 endif  
  218            continue   
                 num = ilast
              endif 
           endif
           if (num .gt. 1) then 
C       
C             Border nodes INTSET(1):INTSET(NUM) are to be split and an 
C             interconnect, lossless network is to be constructed betwee
C             them. 
C       
C             Find average voltage. 
C       
              vzln = cmplx (0.0,0.0)
              do 222 i = 1,num  
  222         vzln = vzln + v(i)
              vzln = vzln/num   
C       
C             Determine that this voltage is unique. Otherwise, perturb 
C             This particular test is vulnerable to zero voltages, but t
C             is not supposed to occur. 
C       
  223         do 224 i = 1,num  
              if (vzln .eq. v(i)) then  
                 vzln = vzln * cmplx (1.01,0.0) 
                 go to 223  
              endif 
  224         continue  
              numzln = numzln + 1   
              kzln = ntotx + 1  
              ntotx = ntotx + num + 1   
C       
C             Initialize KZNL   
C       
              write (bus(kzln),230) reinam,numzln   
  230         format (a4,'*',i3)
              base(kzln) =  100.0   
              intbus(kzln) = bus(kzln)  
              intbas(kzln) = base(kzln) 
              k = intset(1) 
              zone(kzln) = zone(k)  
              owner(kzln) = '***'   
              jarzn(kzln) = jarzn(k)
              inp2opt(kzln) = kzln
              opt2inp(kzln) = kzln
              ikk(1,kzln) = 1   
              ikk(3,kzln) = numzln  
              e(kzln) = real (vzln) 
              f(kzln) = aimag (vzln)
              ktemp(kzln) = 0   
        
              vmag = cabs(vzln) 
              angle = 57.2957795 * datan2 (f(kzln),e(kzln))  
              write (outbuf,232) numzln,intbus(kzln),intbas(kzln),kzln, 
     1                           vmag,angle 
  232         format (t7,'ZLN subsystem ',i2,' Bus ',a8,f6.1,' Node ',  
     1                i4,' voltage ',f6.3,'/',f6.1)
              call prtout (1)   
        
              km(kzln) = ksy
              kmlen(kzln) = 0 
C                    Initialize "Ykm" data with space for 2*NUM branches
              ksy = ksy + 2 * num   
              if (ksy .gt. ksol) then           
                 write (errbuf(1),238) ksy,ksol  
  238            format('0 AREA EQUIVALENT SYSTEM OVERFLOWED ALLOCATED',
     &                  ' SPACE IN /Ykm/:',i5,' LIMIT:',i5)  
                 call prterx ('W',1)
              endif 
              do 240 i = 1,16   
                 kbsdta(i,kzln) = 0
  240         continue
              pnetu(kzln) = 0.0
              qnetu(kzln) = 0.0
              inetr(kzln) = 0 
              ineti(kzln) = 0 
              vlimx(kzln) = cabs(vzln)
              vlimn(kzln) = vlimx(kzln) 
              ploadu(kzln) = 0.0   
              qloadu(kzln) = 0.0   
              ntypu(kzln) = 15 

C             Initialize BUSDTA with space for 2*NUM branches  

              kbsdta(1,kzln) = ntyp 
              kbsdta(2,kzln) = kzln 
              busdta(11,kzln) = vlimn(kzln)        
              busdta(12,kzln) = datan2 (f(kzln),e(kzln)) 
              kbsdta(16,kzln) = ltot + 1
              do 242 i = ltot+1,ltot + 2*num
                 kbrnch(2,i) = 19999
                 kbrnch(12,i) = 19999   
  242         continue
              ltot = ltot + 2*num   
C                       Form interconnected, lossless network for KZLN  
              do 280 i = 1,num  
                 k1 = intset(i)
                 m1 = kzln + i 
C                       Identify eliminated branches of bus K1  
                 numsub = 0
                 kt = inp2opt(k1)
                 do 270 l = km(kt), km(kt)-1+kmlen(kt)    
                    mt = ikmu(l)                          
                    m = opt2inp(mt) 
                    if (iabs(jarzn(m)).eq.jt .and. ikk(1,mt).eq.0) then   
C                          Form set {M2} of eliminated nodes.   
                       numsub = numsub + 1
                       mbr(numsub) = m
                    endif 
  270            continue  
C                      Add complete lossless network:  
C                      K2 --- > K1 ---- KZLN ----> M1 ----> M2 
C                      Nodes KZLN and M1 are new.  

c*** comment out and replace with STOP until ZSPLIT is fixed
c***                 CALL ZSPLIT (K1,M1,KZLN,VZLN,MBR,NUMSUB,REINAM)   
      if( quit_zsplit ) stop 'AREZLN attempted to call ZSPLIT'
c****************************************************************

                 ktemp(k1) = m1
  280         continue  
              start = last + 1  
           else if (first .eq. last) then   
              start = last + 1  
           endif
           if (start .le. numnod) go to 200 
  300   continue
        ntot = ntotx
C                  Post debug test of KZLN system.  
        if (kase1(33) .ne. 0) then  
           write (dbug,302) 
  302      format (' Post ZLN-subsystem injection error check') 
           do 320 kt = 1,ntot   
           if (kmlen(kt) .le. 0) go to 320         
           ls = kmlen(kt) + 1                       
           is = km(kt)-1  
           do i = 1, kmlen(kt)                     
              yred(1,i)= ikmu(is+i)                
              yred(2,i) = gkmu(is+i)               
              yred(3,i) = bkmu(is+i)               
           end do                                    
           yred(1,ls) = kt 
           yred(2,ls) = gkku(kt)                    
           yred(3,ls) = bkku(kt)                    
           do j = 1,3   
              ai1(1,j) = 0.0d0
              ai1(2,j) = 0.0d0
           end do
           call pkqk1 (kt, ls, yred, ai1)
  320      continue 
           write (dbug,322) 
  322      format (' End of post ZLN-subsystem injection error check')  
        endif   
C       
C       Renumber phase shifter terminal nodes of ZLN subsystems.
C       Determine which of the three conditions prevail:
C       
C       1. Neither terminal node is affected by ZLN systems 
C          (KTEMP(J1) and KTEMP(J1) are both zero); 
C       2. One of the terminal nodes is affected by ZLN systems,
C          but the phase shifter branch is intact; or   
C       3. One of the terminal nodes is affected by ZLN systems 
C          which effects also the phase shifter branch. 
C       
        do 370 jt = 1,jphno 
        k1 = jphid(1,jt)
        k2 = jphid(2,jt)
C       
C       Test for condition 1.   
C       
        if (ktemp(k1) .ne. 0 .or. ktemp(k2) .ne. 0) then
           kt = inp2opt(k1)   
           mt = inp2opt(k2)   
C       
C          Test for condition 2.
C       
           if (kmlen(kt) .gt. 0) then 
               ls = kmlen(kt)                      
               is = km(kt)-1  
               do 330 l = 1,ls  
                  if (ikmu(is+l) .eq. mt) go to 370    
  330          continue 
C       
C              Test for condition 3 prevailing for node K2. 
C       
               if (ktemp(k2) .ne. 0) then   
                  j2 = ktemp(k2)
                  nt = inp2opt(j2)
                  do 340 l = 1,ls   
                     if (ikmu(is+l) .eq. nt) then      
                        jphid(2,jt) = j2   
                        go to 370  
                     endif 
  340             continue  
               endif
           endif
C       
C          Test for condition 3 prevailing for node K1. 
C       
           if (ktemp(k1) .ne. 0) then   
              j1 = ktemp(k1)
              kt = inp2opt(j1)
              if (kmlen(kt) .gt. 0) then           
                 ls = kmlen(kt)                    
                 is = km(kt)-1
                 do 350 l = 1,ls
                    if (ikmu(is+l) .eq. mt) then       
                       jphid(1,jt) = j1
                       go to 370   
                    endif  
  350            continue   
C       
C                Test for condition 3 prevailing for nodes K1 and K2.   
C       
                 if (ktemp(k2) .ne. 0) then 
                    j2 = ktemp(k2)  
                    nt = inp2opt(j2)  
                    do 360 l = 1,ls 
                       if (ikmu(is+l) .eq. nt) then    
                          jphid(1,jt) = j1 
                          jphid(2,jt) = j2 
                          go to 370
                       endif   
  360               continue
                 endif  
              endif 
           endif
C       
C          Error - Phase shifter terminal has been modified, but cannot 
C          be located in Y-matrix.  
C       
           call erexit  
        endif   
  370   continue
        
        return  
        end 
