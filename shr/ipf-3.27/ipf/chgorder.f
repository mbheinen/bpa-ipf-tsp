C    @(#)chgorder.f	20.3 2/13/96
C****************************************************************
C
C   File: chgorder.for
C
C   Purpose: Routine to convert solution arrays from input order to
C            optimal order ('inp2opt') or from optimal order to input
C            order ('opt2inp').
C
C   Author: BPA staff        Date: 4 November 1992
C   Called by: p_solton and prodat
C
C****************************************************************
C
        subroutine chgorder ( order )
        character*(*) order
 
C       DUAL change orders:
 
c       order = 'inp2opt'
C                EXT2IN : Convert arrays from external bus numbers in
c                         "input" order to internal bus numbers in 
c                         "optimum" order.
c
c       order = 'opt2inp'
C                INT2EX : Restore arrays from internal bus numbers in 
c                         "optimum" order to external bus numbers in
c                         "input" order.
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		gkmu(r*8), bkmu(r*8), gkku(r*8), bkku(r*8), pnetu(r*8), 
c		qnetu(r*8), ineti(r*8), inetr(r*8), ploadu(r*8), 
c		qloadu(r*8), vlimn(r*4), vlimx(r*4), km, kmlen, ikmu, ntypu,
c		nspar
c
      include 'ipfinc/area.inc'
c	Global variables used:
c		area(r*4), karea(i*4), tie(r*8), kaloc
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8), capcor(r*8), inp2opt, opt2inp, base, bus
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/sort.inc'
      include 'ipfinc/tbx.inc'
c	Global variables used:
c		tbx(r*8)
      include 'ipfinc/tran.inc'
c	Global variables used:
c		ltran(i*4), tran (r*4), tap(r*8)
      include 'ipfinc/xdata.inc'   
c	Global variables used:
c		xdata(r*8)

      save  

      external kmpkal,swpkal  

      do kt = 1, ntot   
         nb = opt2inp(kt)
         intbus(kt) = bus(nb)  
         intbas(kt) = base(nb) 
      enddo

      if ( order .eq. 'inp2opt' .and. ordymx .eq. 1) then

C        Set flags for internal "opt" order  

c        reorder internal arrays   

         call mvnew1   (km,inp2opt,ntot)  
         call mvnew1   (kmlen,inp2opt,ntot)   
         call mvnew1d  (pnetu,inp2opt,ntot)
         call mvnew1d  (qnetu,inp2opt,ntot)
         call mvnew1d  (gkku,inp2opt,ntot) 
         call mvnew1d  (bkku,inp2opt,ntot) 
         call mvnew1   (ntypu,inp2opt,ntot)
         call mvnew1   (nspar,inp2opt,ntot)   
         call mvnew1   (vlimn,inp2opt,ntot)   
         call mvnew1   (vlimx,inp2opt,ntot)   
         call mvnew1d  (ploadu,inp2opt,ntot)   
         call mvnew1d  (qloadu,inp2opt,ntot)   
         call mvnew1d  (inetr,inp2opt,ntot)   
         call mvnew1d  (ineti,inp2opt,ntot)   
         call mvnew1d  (e,inp2opt,ntot) 
         call mvnew1d  (f,inp2opt,ntot) 
         call mvnew2d  (capcor,inp2opt,ntot)
 
c        Reorder y-matrix and convert external "inp" numbers to   
c        internal "opt" numbers.   

         if (kase1(38) .gt. 0) then  
            write (dbug,110) 
  110       format ('0   NO   KM LENGTH BUS               GKM        BKM  
     1      PNET     QNET   NTYP  EMIN EMAX      PLOAD     QLOAD     INE
     1TR   INETI '//)
         endif   

         do 200 kt=1,ntot   
            nb=opt2inp(kt)
            intbus(kt)=bus(nb)  
            intbas(kt)=base(nb) 
            js = km(kt)
            jf = km(kt)-1+kmlen(kt)  
            do l = js, jf
               mt = ikmu(l)
               mt = inp2opt(mt)   
               ikmu(l) = mt
               if (l .gt. js)   then
                  k = l-1
                  do while (k .ge. js .and. (ikmu(k+1) .lt. ikmu(k)))
                     itemp = ikmu(k+1)   
                     ikmu(k+1) = ikmu(k)
                     ikmu(k) = itemp 
                     temp = gkmu(k+1)
                     gkmu(k+1) = gkmu(k)
                     gkmu(k) = temp  
                     temp = bkmu(k+1)
                     bkmu(k+1) = bkmu(k)
                     bkmu(k) = temp  
                     k = k -1 
                  end do 
               endif
            end do  
            if (kase1(38) .gt. 0) then  
               write (dbug,170) kt, js, kmlen(kt), intbus(kt), 
     +                          intbas(kt), gkku(kt), bkku(kt),
     +                          pnetu(kt), qnetu(kt), ntypu(kt),
     +                          vlimn(kt), vlimx(kt), ploadu(kt),
     +                          qloadu(kt), inetr(kt), ineti(kt)  
  170          format(1x, i5, i6, i5, 2x, a8, f6.1, 2f11.4, 2f9.3, 
     +                i3, 2f7.4, 4f9.3)  
               do 190 l = js, jf   
                  mt = ikmu(l)
                  write (dbug,180) mt, l, intbus(mt), intbas(mt),
     +                             gkmu(l), bkmu(l)
  180             format (6x, i6, i5, 2x, a8, f6.1, 2f11.4)
  190          continue 
            endif   
  200    continue

      endif
c
c     Reorder each of the following arrays either in optimal order
c     (order .eq. 'opt2inp') or in input order (order .eq. 'opt2inp')
c       
c     convert numbers in "tbx" array  

      do 240 i=1,ntotb
         kt = tbx(2,i) 
         mt = dabs(tbx(8,i))  
         if (order .eq. 'opt2inp') then  
            tbx(2,i)=opt2inp(kt)   
            if (mt.gt.0) tbx(8,i)=opt2inp(mt)  
         else 
            tbx(2,i)=inp2opt(kt)   
            if (mt.gt.0) tbx(8,i)=inp2opt(mt)  
            if (kase1(38) .gt. 0) then
               write (dbug,230) i,(ifix(sngl(tbx(j,i))),j=1,2),
     1            (tbx(j,i),j=3,6),(ifix(sngl(tbx(j,i))),j=7,8)
  230          format(' TBX ',i4,2i5,4f10.3,2i5)  
            endif 
         endif
  240 continue
C       
C     Convert numbers in 'TRAN' array 
C       
      do 270 i=1,ntota
         kt=ltran(1,i)   
         mt=ltran(9,i)   
         kc=ltran(2,i)   
         if (order .eq. 'opt2inp') then
            ltran(1,i)=opt2inp(kt) 
            ltran(9,i)=opt2inp(mt) 
            if (kc.gt.0) ltran(2,i)=opt2inp(kc)
         else
            ltran(1,i) = inp2opt(kt)   
            ltran(9,i) = inp2opt(mt)   
            if (kc .gt. 0) ltran(2,i) = inp2opt(kc)
C       
C           Link up transformers with Y-matrix   
C       
            k1=ltran(1,i)
            k2=ltran(9,i)
  252       do 254 l = km(k1), km(k1)-1+kmlen(k1)  
               if (ikmu(l).eq.k2) go to 256            
  254       continue 
            call erexit  
  256       if (k1.eq.ltran(1,i)) then   
               ltran(3,i)= l                         
               k1 = ltran(9,i)   
               k2 = ltran(1,i)   
               go to 252 
            else 
               ltran(12,i)= l                        
            endif
            if (kase1(38) .gt. 0) then   
               write (dbug,260) i,(ltran(j,i),j=1,3),
     &                          (tran(j,i),j=4,8)   
     1             ,(ltran(j,i),j=9,10),tran(11,i),ltran(12,i),tap(i)
  260           format(' TRAN ',i4,3i5,5f10.4,2i5,f10.4,i5,f10.4) 
            endif
         endif   
  270 continue
C       
C     Convert numbers in "TIE" array  
c       
      do 300 i=1,jtie 
         kt=tie(1,i) 
         mt=tie(7,i) 
         if (order .eq. 'opt2inp') then 
            tie(1,i)=opt2inp(kt)   
            tie(7,i)=opt2inp(mt)   
         else 
C       
C           Build KALOC array and link up with AREA array 
C       
            kt = inp2opt(kt)
            mt = inp2opt(mt)
            tie(1,i) = kt
            tie(7,i) = mt
            ka1=tie(2,i) 
            ka2=tie(8,i) 
            kaloc(4*i-3)=100000*ka1 + kt  
            kaloc(4*i-2) = 100000*ka1 + mt
            kaloc(4*i-1)=100000*ka2 + mt  
            kaloc(4*i)=100000*ka2 + kt
            nsysno(4*i-3)=i   
            nsysno(4*i-2)=-i  
            nsysno(4*i-1)=-i  
            nsysno(4*i)=i 
            if (kase1(38) .gt. 0) then
               write (dbug,290) i,(ifix(sngl(tie(j,i))),j=1,2),
     &            (tie(j,i),j=3,6), (ifix(sngl(tie(j,i))),j=7,10)  
  290          format(' TIE ', i4,2i5,4f10.4,4i5) 
            endif 
         endif
  300 continue
C       
C     Convert "KAREA" array and sort "KALOC" array
C       
      if (order .eq. 'inp2opt' .and. jtie .gt. 0) then  
         call qiksrt(1, 4*jtie, kmpkal, swpkal)  
      endif   

      jt=1
      do 350 i = 1,ntotc  
         kt=karea(1,i)   
         if (order .eq. 'opt2inp') then
            karea(1,i)=opt2inp(kt)
         else
            karea(1,i)=inp2opt(kt)
            karea(3,i) = jt 
            karea(4,i) = 0  
C       
C           Link up "KALOC"  
C       
            do while (jt .le. 4*jtie .and. kaloc(jt)/100000 .eq. i) 
               kaloc(jt)=nsysno(jt)   
               karea(4,i)=karea(4,i)+1
               jt=jt+1
            enddo
            if (kase1(38) .gt. 0) then   
               write (dbug,340) i, karea(1,i), area(2,i), 
     1            (karea(j,i),j=3,5), (area(j,i),j=6,8)   
  340          format (' AREA ',i3,i5,f8.3,3i5,3f8.3)
            endif
         endif
  350 continue 
      if (order .eq. 'inp2opt' .and. jtie .gt. 0 .and. 
     &   kase1(38) .eq. 1) then   
         write (dbug,360) (j,kaloc(j),j=1,4*jtie) 
  360    format (' KALOC ',10(i5,'-',i5)/(7x,10(i5,'-',i5)))  
      endif   
C       
C     Convert numbers in "XDATA"  
C       
      do 390 i=1,kxtot
         kt = xdata(1,i)
         mt = xdata(2,i)
         if (order.eq.'inp2opt') then  
            xdata(1,i) = inp2opt(kt)
            if(mt.gt.0) xdata(2,i) = inp2opt(mt)
         else
            xdata(1,i) = opt2inp(kt)
            if (mt.gt.0) xdata(2,i) = opt2inp(mt)
         endif   
         if (kase1(38) .gt. 0) then  
            write (dbug,380) i, (xdata(j,i),j=1,2),
     1         (xdata(j,i),j=3,22)   
  380       format(' XDATA ', i4, 2f5.0, 4f8.1, 8(f3.0, f7.1))  
         endif   
  390 continue

      if (order .eq. 'inp2opt') then
         ordcap = 2  
         orddc  = 2   
         ordltc = 2  
         ordtbx = 2  
         ordtie = 2  
         ordvlt = 2  
         ordymx = 2  
      else
         ordcap = 2  
         orddc  = 1   
         ordltc = 1  
         ordtbx = 1  
         ordtie = 1  
         ordvlt = 2  
         ordymx = 2    ! Note: the y-matrix is never reordered.
      endif
      return  
      end   
