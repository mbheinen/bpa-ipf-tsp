C    @(#)build_gb.f	20.7 5/3/00
      integer function build_gb (k1, k2, lx, pass, ix)
      integer k1, k2, lx, pass, ix
C
C     This function modifies the R/X ratios of selected Y-matrix 
c     elements.
C
c     Return codes: 0 - normal
c                   1 - array limit hit - abort further gb processing

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
c
      common /is_batch / is_batch

      integer MAX_GB
      parameter (MAX_GB = 400)
      common /kgb/ ngb, gb(12,MAX_GB), kgb(MAX_GB)
      real*8 gb
      integer kgb

      real*8 r1, r2, x1, x2, z1, gold, bold, ysq, gnew, bnew
      logical phase_shifter
 
      save

      build_gb = 0
      phase_shifter = .false.
      ix = 0
      k1x = k1
      k2x = k2 
C       
C     Exempt bus-tie logic (phase shifter)  
C       
      do jt = 1, ntxtie
        k1_temp = dmin1 (txtie(1,jt),txtie(2,jt))
        k2_temp = dmax1 (txtie(1,jt),txtie(2,jt))
        if (k1_temp .ne. min0 (k1x, k2x)) then
        else if (k2_temp .eq. max0 (k1x, k2x)) then
          go to 900
        endif
      enddo
C
C     Search for prior insertion of k2-k1
C       
      do igb = 1, ngb  
        if (gb(2,igb) .ne. k1x) then
        else if (gb(1,igb) .eq. k2x) then
          if (kgb(igb) .ne. pass) then
            write (errbuf(1), 10000) bus(k1x), base(k1x), bus(k2x),
     &        base(k2x), pass, igb
10000       format('Duplicate entity in gb() ignored ', a8, f7.1, 
     &        1x, a8, f7.1, ' Pass ', 2i4)
            call prterx ('W', 1)
            go to 900
          else
            ix = igb
            ly = lx
            go to 110 
          endif
        endif
      enddo

      if (k1x .gt. k2x .and. pass .eq. 1) then
C       
C       This asymmetric branch ordinarily would be a phase shifter 
C       which should have been detected on the first pass (k1x < k2x),
C       but due to idiosyncracies in the values of Gkm, Bkm, Gmk, 
C       and Bmk, the "ill-conditioned-ness" of Gkm and Bkm was 
C       caught in second pass (k1x > k2x). Since this is the last 
C       chance to process this branch, we must duplicate some of 
C       the second-pass logic here.
C       
C       Confirm as phase shifter. 
C       
        do j = 1, jphno  
          k1_temp = min0 (inp2opt(jphid(1,j)), inp2opt(jphid(2,j)))
          k2_temo = max0 (inp2opt(jphid(1,j)), inp2opt(jphid(2,j)))
          if (k1_temp .ne. min0 (k1x, k2x)) then
          else if (k2_temp .eq. max0 (k1x, k2x)) then 
            phase_shifter = .true.
            go to 100  
          endif  
        enddo
        call erexit ()
        go to 900
      endif
C       
C     Create k1-k2 entity in gb array
C       
  100 ngb = ngb + 1 
      if (ngb .gt. MAX_GB) then
        write (errbuf(1), 10010) MAX_GB 
10010   format('0 More than ', i4, ' entities in GB array (R << X modifi
     &cation)') 
        if (is_batch .eq. 0) then
          call prterx ('E',1)
        else
          call prterx ('F',1)
        endif
        ngb = ngb - 1  
        build_gb = 1
        go to 900
      endif 
      ix = ngb
      kgb(ngb) = pass
      gb(1,ngb) = k1x   
      gb(2,ngb) = k2x   
      gold = gkmu(lx)                         
      bold = bkmu(lx)                         
      gb(3,ngb) = gold  
      gb(4,ngb) = bold  
      ysq = gold ** 2 + bold ** 2   
      r1 = -gold / ysq  
      x1 = bold / ysq   
      z1 = 1.0 / dsqrt (ysq)
      if (pass .eq. 1) then
        if (dabs (x1) .gt. 1.0e-4) then
          r2 = 0.2d0 * dabs (x1)
          x2 = x1
        else  
          r2 = 0.2d0 * r1
          x2 = r1
        endif 
      else
        r2 = 0.0d0
        x2 = 0.00100d0
      endif
      zsq = r2 ** 2 + x2 ** 2   
      gnew = -r2 / zsq  
      bnew = x2 / zsq   
      gkmu(lx) = gnew
      bkmu(lx) = bnew
      gb(5,ngb) = gnew  
      gb(6,ngb) = bnew  
      gkku(k1x) = gkku(k1x) + gold - gnew   
      bkku(k1x) = bkku(k1x) + bold - bnew   

      if (pass .eq. 1 .and. .not. phase_shifter) go to 900
C       
C     The following logic emulates the second pass of a special 
C     class of phase shifters noted previously. Note that k1 and
c     k2 quantities are transposed
C       
      k1x = k2
      k2x = k1
      kmjuu = km(k1x) - 1
      js = kmlen(k1x)                               
      do j = 1, js 
        if (ikmu(j+kmjuu) .eq. k2x) then            
          igb = ngb   
          ly = j + kmjuu
          go to 110  
        endif  
      enddo
      call erexit ()
      go to 900
c
c     Create k2-k1 entity in array gb   
c        
  110 gold = gkmu(ly)                         
      bold = bkmu(ly)                         
      gb(7,igb) = gold  
      gb(8,igb) = bold  
      ysq = gold ** 2 + bold ** 2   
      r1 = -gold / ysq  
      x1 = bold / ysq   
      z1 = 1.0 / dsqrt (ysq)
      if (pass .eq. 1) then
        if (dabs (x1) .gt. 1.0e-4) then
          r2 = 0.2d0 * dabs (x1)
          x2 = x1
        else  
          r2 = 0.2d0 * r1
          x2 = r1
        endif 
      else
        r2 = 0.0d0
        x2 = 0.00100d0
      endif
      zsq = r2 ** 2 + x2 ** 2   
      gnew = -r2 / zsq  
      bnew = x2 / zsq   
      gb(9,igb) = gnew  
      gb(10,igb) = bnew 
      gkku(k1x) = gkku(k1x) + gold - gnew   
      bkku(k1x) = bkku(k1x) + bold - bnew   
      gkmu(ly) = gnew
      bkmu(ly) = bnew

  900 continue  
        
      return
      end
