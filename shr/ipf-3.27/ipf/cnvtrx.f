C    @(#)cnvtrx.f	20.16 5/3/00
      subroutine cnvtrx
C
C     convert R/X ratios.
C
C     Entry CNVTRX inserts pseudo buses in all branches whose
C     reactance is compensated 95-105%.
C
C     Entry RESTRX restores the Y-matrix to it original form.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/com008.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
c
      integer max_gb
      parameter (MAX_GB = 400)
      common /kgb/ ngb, gb(12,MAX_GB), kgb(MAX_GB)
      real*8 gb
      integer kgb

      common /is_batch / is_batch

      integer build_gb, status
      complex ia, v(2), y

      save
        
      ngb = 0   
      ntotz1 = ntot 
      ntotz2 = ntotx
      ntotb_old = ntotb
      idswb = iopton(13)
        
      if (iopton(19) .eq. 0) go to 100 
        
      do kt = 1, ntot  
        kmluu = km(kt) - 1   
        ls = kmlen(kt)                               
        iflag = 0
c
c       First pass - process Y-matix off-diagonals in low-high order
c
        do l = 1, ls 
          lx = l + kmluu
          mt = ikmu(lx)                           
          if (dabs(gkmu(lx)) .gt. dabs(bkmu(lx))) then
            status = build_gb (kt, mt, lx, 1, ix)
            if (status .eq. 1) go to 100
            if (ix .gt. 0) iflag = 1
          endif        
        enddo
c
c       Second pass - process ill-conditioned 2-port Y-matrix
c       conditions
c
        if (ls .eq. 2 .and. iflag .eq. 0) then
          if (bkmu(kmluu+1) * bkmu(kmluu+2) .lt. 0.0) then
            if (dmax1(dabs(bkmu(kmluu+1)), dabs(bkmu(kmluu+2))) .gt. 
     &          100.0 * dabs(bkku(kt))) then
              lx = kmluu + 1
              mt = ikmu(lx)
              status = build_gb (kt, mt, lx, 2, ix)
              lx = kmluu + 2
              mt = ikmu(lx)
              status = build_gb (kt, mt, lx, 3, ix)
            endif
          endif        
        endif
      enddo
  100 continue        
      return
        
      entry restrx  
C       
C     Entry RESTRX restores the Y-matrix to its original form.  
C       
      if (ngb .eq. 0) go to 120
        
      ntot = ntotz1 
      ntotx = ntotz2
      do i = 1, ngb
        k1 = gb(1,i) 
        k2 = gb(2,i) 
        if (kgb(i) .eq. 2) then
c
c         Compute ia (i21) using modified admittances
c          
          v(1) = cmplx (e(k1), f(k1))
          v(2) = cmplx (e(k2), f(k2))
          kmluu = km(k1) - 1
          ls = kmlen(k1)                               
          do l = 1, ls 
            mt = ikmu(l+kmluu)                            
            if (mt .eq. k2) go to 102
          enddo
          call erexit ()
  102     y = -cmplx (gkmu(l+kmluu), bkmu(l+kmluu))
          ia = y * (v(1) - v(2))
c
c         Compute a new voltage v(1) which will result in the
c         same value of i12 using the restored line admittances.
c
          y = -cmplx (gb(3,i), gb(4,i))
          v(1) = (ia + y * v(2)) / y
          write (dbug, 10000) i, intbus(k1), intbas(k1), intbus(k2),
     &      intbas(k2), v(2)
10000     format(' R/X Conversion ', i3, 1x, a8, f7.1, 1x, a8, f7.1,
     &      ' New voltage ', 2f8.5)

          e(k2) = real (v(1))
          f(k2) = aimag (v(1))
        endif
        do ksw = 1, 2
          kmluu = km(k1) - 1
          ls = kmlen(k1)                               
          do l = 1, ls 
            mt = ikmu(l+kmluu)                            
            if (mt .eq. k2) go to 110
          enddo
          call erexit ()
  110     if (ksw .eq. 1) then  
            gkmu(l+kmluu) = gb(3,i)   
            bkmu(l+kmluu) = gb(4,i)   
            gkku(k1) = gkku(k1) - gb(3,i) + gb(5,i)  
            bkku(k1) = bkku(k1) - gb(4,i) + gb(6,i)  
          else  
            gkmu(l+kmluu) = gb(7,i)   
            bkmu(l+kmluu) = gb(8,i)   
            gkku(k1) = gkku(k1) - gb(7,i) + gb(9,i)  
            bkku(k1) = bkku(k1) - gb(8,i) + gb(10,i) 
          endif 
          k2 = gb(1,i) 
          k1 = gb(2,i) 
        enddo
      enddo

  120 continue  

      return
      end   
