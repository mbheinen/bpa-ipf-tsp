C    @(#)nrdclp.f	20.6 10/20/99
      subroutine nrdclp
C
C     This routine solves the d-c network using a general purpose LP,
C     permitting a realistic mixture of "hard" and "soft" limits.
C     The hard limits are physical: Current, LTC, and angle limits.
C     The soft limits are scheduled d-c values: controlled d-c voltage
C     or power. Thus, the controlled values may not be held if some
C     physical limit is hit.
C       
C     This routine is entered in three modes:   
C       
C     1. Initialization: Entry NRDCLP with IDCHG = 0.   
C        (Freeze converter angle; determine Vdo from d-c values.)   
C     2. Normal: Entry NRECLP with IDCHG > 0.   
C        (Check a-c and d-c values with limits.)
C     3. Final: Entry NRDCLU.   
C        (Compute new a-c injections using a-c commutator voltage,  
C        LTC tap, and final d-c values.)
C       
C     Input:
C       
C     Output:   
C       
C     STATUS -  1 : 'Optimum'   
C               2 : 'Infeasible'
C               3 : 'Unbounded' 
C               4 : 'The maximum size of the inverse has been exceeded  
C               5 : 'The maximum number of iterations has been reached  
C               6 : 'Either the AA vector is full or the number of  
C                    constraints is exceed the maximum' 
C               7 : 'Still inaccurate after reinverting'
C     Z - Value of objective function.  
C       
C     The LP used is copied from the book by A. Land and S. Powell: 
C     "Fortran Codes for Mathematical Programming", 1978, John Wiley
C     and Sons, N.Y.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/aref.inc'      
      include 'ipfinc/dc.inc'
      include 'ipfinc/dcinit.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/smallp.inc'
      include 'ipfinc/tran.inc'
 
      dimension iv(10), ic(10), jv(10), adi(10), vdi(10), vdo(10)
      character dc_code * 1

      integer row, col, status, debug, convgp, convgv
      logical noiter
        
C     C1 = 18/PI**2
C     C2 = 3/PI**2 
C     C3 = PI/(3*SQRT(2))  

      data c1, c2, c3 / 1.823781306, 0.9549296586, 0.740480489 /   

C     "KNTRY" Identifies entry  
C     0 -- Normal   
C     1 -- DC update after final AC solution

      kntry = 0 
      go to 100 

      entry nrdclu  
      kntry = 1 

  100 continue  

C     The following tolerances are defined for double-precision words  
C     (64-bits).  See page 186 for 32-bit equivalent tolerances.   

      big = 1.0e11  
      small = 1.0e - 9  

C     The following value of TOL(1) differs from the suggested value   
C     (1.0E-6). The revised value was also determined heuristically.   
C     The need for a smaller value is motivated by two aspects:
C     1. The range of coefficients in the A matrix is larger than   
C        0.1 to 10.0 as suggested in the text. The dependent variables  
C        could be scaled to eliminate this. For the moment, the 
C        simplier approach was opted: tighten the check for roundoff.   
C     2. The dependent variables are delta_X, which are naturally   
C        small near convergence.

      tol(1) = 1.0e - 8 
      tol(2) = 1.0e - 5 
      tol(3) = 1.0e - 6 
      tol(4) = 1.0e - 5 
      tol(5) = 1.0e - 5 
      tol(6) = 1.0e - 5 
      tol(7) = 1.0e - 5 
      tol(8) = tol(5) * 10.0
      isdone = 0
      debug = iopton(15)
      kerrsw = 0
      noiter = .true.

C     Begin loop of each DC circuit

      do 420 jckt = 1, idckt
         nec = nckt(jckt) - 1   
         ntdc = nckt(jckt+1) - nckt(jckt)   
         convgp = 0 
         convgv = 0 
         if (kntry .ne. 0) go to 390
         do 118 i = 1, ntdc 
            it = i + nec
            iv(i) = 0   
            ic(i) = 0   
            jv(i) = 0   
            ivc = dcbus(27,it) 
            ipc = dcbus(31,it) 
            iac = dcbus(32,it) 
            if (ivc .ne. 0) iv(i) = 1   
            if (ipc .ne. 0) ic(i) = 1   
            if (iac .ne. 0) ic(i) = 2   
            ivc = dcbus(28,it) 
            ipc = dcbus(33,it) 
            if (ivc .ne. 0) jv(i) = 1   
            if (ipc .ne. 0) jv(i) = 2   

C           Initialize DC values  

            vdi(i) = dcbus(20,it)   
            adi(i) = dcbus(19,it) / vdi(i)  
            arate = 0.001 * dcbus(9,it) 
c
c           iopton(23) = 0:  BRIDGE_CURRENT_RATING = ON
c                        1:                          OFF
c
            if (abs (adi(i)) .gt. arate .and. iopton(23) .eq. 0) then
               adi(i) = sign (arate,adi(i))
            endif
            kt = dcbus(1,it)   
            if (kt .gt. 0) then 
               cx = c1  
               if (dcbus(19,it) .lt. 0) cx = -c1
               rci = (cx * dcbus(17,it) 
     1             + c2 * dcbus(18,it)) * dcbus(7,it)   
               dcbus(23,it) = rci   
            endif   
C       
C           Examine commutating LTC'S   
C       
            jt = dcbus(15,it)  
            if (jt .ne. 0) go to 112
            if (idchg .ne. 0) go to 112 
            if (dcbus(3,it) .eq. 0) go to 112  
            i1 = iflag(kt)  
            i2 = iflag(kt+1) - 1
            do 110 j = i1, i2   
               if (jflag(1,j) .eq. 2) then  
                  jt = jflag(2,j)   
                  dcbus(15,it) = jt
                  go to 112 
               endif
  110       continue
C       
C           Update LTC taps 
C       
  112       jt = dcbus(15,it)  
            if (jt .gt. 0) dcbus(16,it) = tap(jt)   
C       
C           Initialize converter angles 
C       
            call getchr_8 (1, dc_code,dcbus(4,it))
            if (idchg .eq. 0) then  
               if (dc_code .eq. 'M') then  
                  dcbus(13,it) = dcbus(29,it)   
               else 
                  dcbus(13,it) = dcbus(12,it)   
               endif
            endif   
C       
C           Check a-c/d-c voltage/injection balance 
C       
            rci = dcbus(23,it)  
            kc = dcbus(3,it)   
            if (kc .eq. 0) then 
               vdo(i) = 0.0 
            else
               call nrpqv (kt,pk,dp,qk,dq,vk)   
               dv = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)  
               vc = dsqrt(e(kc) ** 2 + f(kc) ** 2)   
               ec = vc * dcbus(2,it) * dcbus(7,it) * dcbus(16,it)   
               vdo(i) = ec / c3 
C       
C              "Vdo" is the interface between the a-c and d-c   
C              Systems. It is first computed as the secondary   
C              voltage on the converter side of an ideal a-c
C              transformer (see notes). Below, it is compared with  
C              the d-c computed value DCBUS(24,*).  
C       
               dvdo = vdo(i) - dcbus(24,it) 
               rt = dcbus(17,it) * dcbus(7,it)  
               xt = dcbus(18,it) * dcbus(7,it)  
               c4 = 0.57735027 * bmva / (dcbus(2,it) * dcbus(7,it)) 
               ac = c4 * sqrt (pk ** 2 + qk ** 2) / vk  
               ac = sign (ac,adi(i))
               cs = (-pk + rt * ac ** 2) / (ec * ac)
               if (abs(cs) .gt. 1.0) cs = sign(1.0,cs)  
               phi = acos(cs)   
               if (adi(i) .gt. 0) phi = -phi
               if (debug .ne. 0) then   
                  write (dbug,114) jckt, i, kt, iv(i), ic(i),   
     1               adi(i), vdi(i), dcbus(13,it), vdo(i),  
     2               pk, qk, vk, phi,   
     3               dp, dq, dv, dvdo   
  114             format (' Old d-c state - circuit/node/status ',  
     1               i2,i3,i5,2i2,  
     2               t52, ' Id, Vd ',2e12.5, ' Alpha, Vdo ',2e12.5,/,   
     3               t52, ' Pk, Qk ',2e12.5, ' Vk,    Phi ',2e12.5,/,   
     4               t52, ' eP, eQ ',2e12.5, ' eVk,  eVdo ',2e12.5) 
               endif
C       
C              Determine whether d-c values should be recomputed on 
C              basis of mismatched Vdo value.  If terminal injection
C              has not converged, Vdo mismatch is meaningless, and  
C              updating is skipped this iteration (except for initial   
C              iteration (IDCHG = 0).   
C       
               if (dp ** 2 + dq ** 2 .le. option(4) ** 2) then  
               else 
C       
C                 Nonconvergence flag set so that a-c values will   
C                 not be redefined. (D-c values also remain unchanged.  
C       
                  convgp = 1
               endif
C       
C              Check convergence of converter a-c voltage.  
C       
               if (abs (dv) .gt. 0.001 .and. ittot .gt. 3) then
                  if (jt .gt. 0) then   
                     kstat = jacltc (jt,1)  
                     if (kstat .eq. 1) then 
C       
C                    LTC is active in auto control (implicit in 
C                    Jacobian), but voltage has not converged.  
C                    Voltage convergence flag remains unchanged
C                    set so that a-c values will not be redefined. 
C                    (D-c values also remain unchanged.) 
C       
                     else if (kstat.eq.0) then  
                        dx = abs (dpt(1,jt))
                        if (dx .ge. amin1 (0.25*abs(dv),0.001)) then
C       
C                    LTC is active in manual control (explicit, not 
C                    in Jacobian), but voltage has not converged.   
C                    Voltage convergence flag remains unchanged
c                    set so that a-c values will not be redefined. 
C                    (D-c values also remain unchanged.) 
C       
                        else
                           convgv = 1
                           if (debug .ne. 0) then   
                              write (dbug,116) i, jt, kt, kstat, dv 
  116                         format(' DC LTC control off ',4i5,e12.5)  
                           endif
                        endif   
                     else   
                        if (debug .ne. 0) then  
                            write (dbug,116) i, jt, kt, kstat, dv   
                        endif   
                     endif  
                  else
                     conjgv = 1
                  endif 
               endif
            endif   
  118    continue   
C       
C        Recompute d-c quantities under the following conditions:   
C       
C           1. Initial iteration; or
C           2. P, Q has converged and V has not.
C       
         if (idchg .ne. 0) then 
            if (convgp .ne. 0) go to 420
            if (convgv .eq. 0) go to 420
         endif  
        
         m = 5 * ntdc   
         n = 14 * ntdc  
         isbnd = n  
         mhold = m  
         nhold = n  
        
         if (m .gt. MAXM .or. n .gt. MAXN .or. isbnd .gt. MAXN) then
            write (dbug,120) MAXM, MAXN 
  120       format (' EXCEEDED CAPACITY OF LP -', i5,   
     1         ' CONSTRAINTS AND ', i5, ' VARIABLES')   
            isdone = 1  
            kerrsw = 1  
            go to 430   
         endif  
        
         isbnd = n  
         morepr = 1 
         itrmax = 0 
         irmax = 0  
C       
C        Initialize arrays  
C       
         bndj = -1.0
         if (isbnd .eq. -1) bndj = 1.0  
        
         if (itrmax .eq. 0) then
            ijk = isbnd 
            if (ijk .eq. -1) ijk = n
            itrmax = 3 * (m + n + ijk)  
         endif  
        
         if (irmax .eq. 0) then 
            irmax = itrmax  
         endif  
        
         do 130 j = 1, n
  130    bound(j) = bndj
C       
C        D-c terminal constraints are "soft" constraints, imposed by
C        optimization. The penalty factors are negative since this is   
C        a maximization algorithm.  
C       
         cmax = 0.0 
         vmax = 0.0 
         do 131 i = 1,ntdc  
            cmax = amax1 (cmax, abs (adi(i)))   
            vmax = amax1 (vmax, abs (vdi(i)))   
  131    continue   
         cv = -1.0  
         ci = -vmax / cmax  
         ct = 0.0   
        
         do 132 j = 1, 8 * ntdc 
  132    c(j) = 0.0 
C       
C        Add cost coefficients to delta_V_violation.
C       
         do 134 j = 8 * ntdc + 1, 10 * ntdc 
  134    c(j) = cv  
C       
C        Add cost coefficients to delta_I_violation.
C       
         do 136 j = 10 * ntdc + 1, 12 * ntdc
  136    c(j) = ci  
C       
C        Add cost coefficients to delta_theta_excursion.
C       
         do 138 j = 12 * ntdc + 1, 14 * ntdc
  138    c(j) = ct  
        
         do 140 i = 1, m
            b(i) = big  
  140    s(i) = 1.0 
C       
C        Begin iteration loop   
C       
         do 380 iter = 1, 10
        
            error = 0.0 
C       
C           Establish present order (values constrained on d-c nodes).  
C       
            m = mhold   
            n = nhold   
        
C       
C              Load A and B elements. SI(*) flags equality-inequality   
C              status. Non-zero A elements must be loaded in ascending  
C              order:  A(1,1),A(1,3),...,A(2,1),...  At least one   
C              element in each row must be specified, even if that  
C              element is zero! 
C       
C              1 means <=   
C              2 means =>   
C              0 means =
        
            look = 1
C       
C           Load I_net constraint   
C       
            do 182 i = 1, ntdc  
               it = i + nec 
               row = i  
               irow(row) = look 
               gerror = 0.0 
        
               do 170 j = 1, ntdc   
                  col = 2 * j - 1   
                  jcol(look) = col  
                  gdc = dcy(i,j,jckt)   
                  aa(look) = gdc
        
                  look = look + 1   
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -gdc   
        
                  look = look + 1   
                  gerror = gerror + gdc * vdi(j)
  170          continue 
        
               gerror = gerror - adi(i) 
        
               col = 2 * ntdc + 2 * i - 1   
               jcol(look) = col 
               aa(look) = -1.0  
               look = look + 1  
        
               col = col + 1
               jcol(look) = col
               aa(look) = 1.0
               look = look + 1
 
               b(row) = -gerror
               s(row) = 0.0
               error = error + abs (b(row))
 
               if (debug .ne. 0) then
                  write (dbug,180) row, i, b(row), s(row)
  180             format (' NRDCLP - I_net ROW: I, B, S ',
     1                    2i5, 2e10.3 )
               endif
        
  182       continue
C       
C           Load Vdo constraint 
C       
            do 192 i = 1,ntdc   
               it = i + nec 
               kc = dcbus(3,it)
               if (kc .eq. 0) then  
C       
C                 If passive d-c node, add dummy constraint 
C       
                  row = ntdc + i
                  irow(row) = look  
        
                  col = 4 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  aa(look) = 1.0
                  look = look + 1   
        
                  b(row) = 0.0  
                  s(row) = 0.0  
        
               else 
        
                  cx = c1   
                  if (dcbus(19,it) .lt. 0) cx = -c1 
                  rci = dcbus(23,it)
                  angle = dcbus(13,it)  
C       
C                 For IDCHG = 0, use d-c values to determine a-c
C                 quantities, starting with Vdo.
C       
                  if (idchg .eq. 0) then
        
                     rci = dcbus(23,it) 
                     vdocos = vdi(i) + dsign(dcbus(8,it),dble(adi(i)))
     1                               + rci * abs (adi(i))   
                     vdo(i) = vdocos / cos (angle)  
                     dcbus(24,it) = vdo(i)  
        
                  endif 
        
                  gerror = vdi(i) + rci * abs (adi(i))  
     1                            + dsign(dcbus(8,it),dble(adi(i)))  
     2                            - vdo(i) * cos (angle)
        
                  row = ntdc + i
                  irow(row) = look  
        
                  col = 2 * i - 1   
                  jcol(look) = col  
                  aa(look) = 1.0
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -1.0   
                  look = look + 1   
        
                  col = 2 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  aa(look) = sign (rci,adi(i))  
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -sign (rci,adi(i)) 
                  look = look + 1   
        
                  col = 4 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  vdosin = vdo(i) * sin (angle) 
                  aa(look) = vdosin 
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -vdosin
                  look = look + 1   
        
                  col = 6 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  cosang = cos (angle)  
                  aa(look) = -cosang
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = cosang 
                  look = look + 1   
        
                  b(row) = -gerror  
                  s(row) = 0.0  
                  error = error + abs (b(row))  
        
               endif
        
               if (debug .ne. 0) then   
                  write (dbug,190) row, i, b(row), s(row)   
  190             format (' NRDCLP - Vdo   ROW: I, B, S ', 2i5, 
     1               2e10.3)
               endif
  192       continue
C       
C           Load network pseudo-constraints.  These introduce   
C           variables which can be penalized, simulating adding "soft"  
C           constraints.
C       
            row = 2 * ntdc  
        
            do 252 i = 1, ntdc  
               it = i + nec 
C       
C              Impose constrained d-v by linear penalties   
C       
               if (iv(i) .eq. 1) then   
        
                  row = row + 1 
                  irow(row) = look  
                  dcbus(27,it) = row   
        
                  col = 2 * i - 1   
                  jcol(look) = col  
                  aa(look) = 1.0
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -1.0   
                  look = look + 1   
        
                  col = 8 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  aa(look) = -1.0   
                  look = look + 1   
        
                  b(row) = dcbus(6,it) - vdi(i) 
                  s(row) = 1.0  
        
                  if (debug .ne. 0) then
                     write (dbug,200) row, i, b(row), s(row)
  200                format (' NRDCLP - V_max ROW: I, B, S ', 2i5,  
     1                  2e10.3) 
                  endif 
        
                  row = row + 1 
                  irow(row) = look  
        
                  col = 2 * i - 1   
                  jcol(look) = col  
                  aa(look) = 1.0
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -1.0   
                  look = look + 1   
        
                  col = 8 * ntdc + 2 * i
                  jcol(look) = col  
                  aa(look) = 1.0
                  look = look + 1   
        
                  b(row) = dcbus(6,it) - vdi(i) 
                  s(row) = 2.0  
        
                  if (debug .ne. 0) then
                     write (dbug,210) row, i, b(row), s(row)
  210                format (' NRDCLP - V_min ROW: I, B, S ', 2i5,  
     1                  2e10.3) 
                  endif 
               endif
C       
C              Impose constrained d-c power by linear penalties 
C       
               if (ic(i) .eq. 1) then   
        
                  row = row + 1 
                  irow(row) = look  
                  dcbus(31,it) = row   
        
                  col = 2 * i - 1   
                  jcol(look) = col  
                  aa(look) = adi(i) 
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col + 1  
                  aa(look) = -adi(i)
                  look = look + 1   
        
                  col = 2 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  aa(look) = vdi(i) 
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -vdi(i)
                  look = look + 1   
        
                  if (adi(i) .ge. 0.0) then 
                     col = 8 * ntdc + 2 * i - 1 
                     jcol(look) = col   
                     aa(look) = -adi(i) 
                     look = look + 1
                  else  
                     col = 8 * ntdc + 2 * i 
                     jcol(look) = col   
                     aa(look) = adi(i)  
                     look = look + 1
                  endif 
        
                  col = 10 * ntdc + 2 * i - 1   
                  jcol(look) = col  
                  aa(look) = -vdi(i)
                  look = look + 1   
        
                  b(row) = dcbus(5,it) - vdi(i) * adi(i)
                  s(row) = 1.0  
        
                  if (debug .ne. 0) then
                     write (dbug,220) row, i, b(row), s(row)
  220                format (' NRDCLP - P_max ROW: I, B, S ', 2i5,  
     1                  2e10.3) 
                  endif 
        
                  row = row + 1 
                  irow(row) = look  
        
                  col = 2 * i - 1   
                  jcol(look) = col  
                  aa(look) = adi(i) 
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -adi(i)
                  look = look + 1   
        
                  col = 2 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  aa(look) = vdi(i) 
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -vdi(i)
                  look = look + 1   
        
                  if (adi(i) .ge. 0.0) then 
                     col = 8 * ntdc + 2 * i 
                     jcol(look) = col   
                     aa(look) = adi(i)  
                     look = look + 1
                  else  
                     col = 8 * ntdc + 2 * i + 1 
                     jcol(look) = col   
                     aa(look) = -adi(i) 
                     look = look + 1
                  endif 
        
                  col = 10 * ntdc + 2 * i   
                  jcol(look) = col  
                  aa(look) = vdi(i) 
                  look = look + 1   
        
                  b(row) = dcbus(5,it) - vdi(i) * adi(i)
                  s(row) = 2.0  
        
                  if (debug .ne. 0) then
                     write (dbug,230) row, i, b(row), s(row)
  230                format (' NRDCLP - P_min ROW: I, B, S ', 2i5,  
     1                  2e10.3) 
                  endif 
C       
C              Impose constrained d-c by linear penalties   
C       
               else if (ic(i) .eq. 1) then  
        
                  row = row + 1 
                  irow(row) = look  
                  dcbus(32,it) = row   
        
                  col = 2 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  aa(look) = 1.0
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -1.0   
                  look = look + 1   
        
                  col = 10 * ntdc + 2 * i - 1   
                  jcol(look) = col  
                  aa(look) = -1.0   
                  look = look + 1   
        
                  b(row) = dcbus(5,it) / dcbus(6,it) - adi(i)   
                  s(row) = 1.0  
        
                  if (debug .ne. 0) then
                     write (dbug,240) row, i, b(row), s(row)
  240                format (' NRDCLP - I_max ROW: I, B, S ', 2i5,  
     1                  2e10.3) 
                  endif 
        
                  row = row + 1 
                  irow(row) = look  
        
                  col = 2 * ntdc + 2 * i - 1
                  jcol(look) = col  
                  aa(look) = 1.0
                  look = look + 1   
        
                  col = col + 1 
                  jcol(look) = col  
                  aa(look) = -1.0   
                  look = look + 1   
        
                  col = 10 * ntdc + 2 * i   
                  jcol(look) = col  
                  aa(look) = 1.0
                  look = look + 1   
        
                  b(row) = dcbus(5,it) / dcbus(6,it) - adi(i)   
                  s(row) = 2.0  
        
                  if (debug .ne. 0) then
                     write (dbug,250) row, i, b(row), s(row)
  250                format (' NRDCLP - I_min ROW: I, B, S ', 2i5,  
     1                  2e10.3) 
                  endif 
        
               endif
        
  252       continue
C       
C           Load angle excursion pseudo-constraints.  These introduce   
C           variables which can be penalized, simulating "soft" 
C           constraints.
C       
            do 262 i = 1, ntdc  
               it = i + nec 
        
               row = 4 * ntdc + i   
               irow(row) = look 
        
               col = 4 * ntdc + 2 * i - 1   
               jcol(look) = col 
               aa(look) = 1.0   
               look = look + 1  
        
               col = col + 1
               jcol(look) = col 
               aa(look) = -1.0  
               look = look + 1  
        
               col = 12 * ntdc + 2 * i - 1  
               jcol(look) = col 
               aa(look) = -1.0  
               look = look + 1  
        
               col = col + 1
               jcol(look) = col 
               aa(look) = 1.0   
               look = look + 1  

               call getchr_8 (1, dc_code, dcbus(4,it))
               if (dc_code .eq. 'M') then  
                  b(row) = dcbus(29,it) - dcbus(13,it)  
               else 
                  b(row) = dcbus(12,it) - dcbus(13,it)  
               endif
               s(row) = 0.0 
        
               if (debug .ne. 0) then   
                  write (dbug,260) row, i, b(row), s(row)   
  260             format (' NRDCLP - T_exc ROW: I, B, S ', 2i5, 2e10.3  
     1               )  
               endif
        
  262       continue
        
            do 300 i = 1, ntdc  
               it = i + nec 
C       
C              Impose current limits (ratings) by bounds
C       
               angle = dcbus(13,it) 
               col = 2 * ntdc + 2 * i - 1   
               arate = 0.001 * dcbus(9,it)  
c
c              iopton(23) = 0:  BRIDGE_CURRENT_RATING = ON
c                           1:                          OFF
c
               if (iopton(23) .eq. 0) then
                  bound(col) = dim (arate,adi(i))
                  bound(col+1) = dim (adi(i),-arate)
               else
                  bound(col) = bndj
                  bound(col+1) = bndj
               endif
C       
C              Impose angle limits by bounds. During initialization,
C              freeze angle excursions. 
C       
               col = 4 * ntdc + 2 * i - 1   
               if (idchg .eq. 0) then   
                  bound(col) = 0.0  
                  bound(col+1) = 0.0
               else 
                  if (adi(i) .gt. 0) then   
                     amin = dcbus(10,it)
                     amax = dcbus(11,it)
                  else  
                     amin = dcbus(12,it)
                     amax = dcbus(11,it)
                  endif 
                  bound(col) = dim (amax,angle) 
                  bound(col+1) = dim (angle,amin)   
               endif
C       
C              Impose Vdo limits by bounds according to status of   
C              LTC's and a-c convergence. Recall that default is
C              unboundedness.   
C       
               col = 6 * ntdc + 2 * i - 1   
               if (ittot .lt. 3 .or. idchg .eq. 0 .or. convgv .ne. 0)   
     1            go to 280 
               jt = dcbus(15,it)   
               if (jt .gt. 0) then  
                  kstat = jacltc (jt,1) 
                  if (kstat .eq. 1) then
C       
C                    LTC's are on auto control. Impose Vdo limits by
C                    bounds according to LTC tap limits.
C       
                     vdomax = vdo(i) * tran(7,jt) / tap(jt) 
                     vdomin = vdo(i) * tran(8,jt) / tap(jt) 
                     bound(col) = ddim (dble(vdomax),dcbus(24,it)) 
                     bound(col+1) = ddim (dcbus(24,it),dble(vdomin))   
                  else if (kstat.eq.0) then 
                     dx = abs (dpt(1,jt))   
                     kt = dcbus(1,it)  
                     vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                     dv = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)  
                     if (dx .ge. amin1 (0.25*abs(dv),0.001)) then   
C       
C                    LTC's are on manual control. Impose Vdo limits by  
C                    bounds according to LTC tap limits.
C       
                        vdomax = vdo(i) * tran(7,jt) / tap(jt)  
                        vdomin = vdo(i) * tran(8,jt) / tap(jt)  
                        bound(col) = ddim(dble(vdomax),dcbus(24,it))  
                        bound(col+1) = ddim(dcbus(24,it),dble(vdomin))
                     else   
C       
C                    LTC's are off. Set Vdo limits to a-c voltage.  
C       
                        bound(col) = ddim(dble(vdo(i)),dcbus(24,it)) 
                        bound(col+1) = ddim(dcbus(24,it),dble(vdo(i)))
                     endif  
                  else  
C       
C                    LTC's are controlling Qkm instead of voltage.  
C                    Set Vdo limits to a-c voltage. 
C       
                     bound(col) = ddim(dble(vdo(i)),dcbus(24,it)) 
                     bound(col+1) = ddim(dcbus(24,it),dble(vdo(i)))   
                  endif 
               else 
C       
C                 No LTC's. Set Vdo limits to a-c voltage.  
C       
                  bound(col) = ddim(dble(vdo(i)),dcbus(24,it))
                  bound(col+1) = ddim(dcbus(24,it),dble(vdo(i)))  
               endif
  280          continue 
        
  300       continue
        
        
            irow(row+1) = look  
            irow(row+2) = look + 1  
        
            do 320 j = 1, m 
               si = s(j)
               if (si .ne. 1.0 .and. si .ne. 2.0 .and. si .ne. 0.0)
     1            then
                  write (dbug,310) j, s(j)
  310             format ('0 Illegal equality-inequality code for row '
     1                    , i3, ' (', f3.0,').  Legitimate values ',
     &                    'are "0" (=), "1" (<=), or "2" 1(=>).')
               endif
               if (si .eq. 2.0) si = -1.0   
               s(j) = si
  320       continue
        
            mnow = m
        
            isdone = 0  
            inrev = 0   
            ir = 0  
            itr = 0 
            neginv = 0  
            negrow = 0  
            newx = 0
            newy = 0
            r = 0.0 
            size = 0
            isbig = 1   
            yaminc = 0.0
C       
C           Execute LP  
C       
            istate = 0  
            call lp 
        
            z = obj 
            status = istate 
            if (istate .ne. 1) then 
               kerrsw = 1   
               go to 390
            else
               gerror = 0.0 
               do 360 i = 1, ntdc   
                  it = i + nec  
                  col = 2 * i - 1   
                  vdi(i) = vdi(i) + x(col) - x(col+1)   
                  dcbus(20,it) = vdi(i) 
                  col = 2 * ntdc + 2 * i - 1
                  adi(i) = adi(i) + x(col) - x(col+1)   
                  dcbus(19,it) = vdi(i) * adi(i)
                  col = 4 * ntdc + 2 * i - 1
                  delang = x(col) - x(col+1)
                  anew = dcbus(13,it) + delang

                  call getchr_8 (1, dc_code, dcbus(4,it))
                  if (adi(i) .lt. 0) then   
                     if (dc_code .eq. 'I' .or. dc_code .eq. 'M') then 
                         amin = dcbus(29,it)
                     else   
                         amin = dcbus(12,it)
                     endif  
                  else  
                     amin = dcbus(10,it)
                  endif 
                  amax = dcbus(11,it)   
                  da = dim (anew,amax) - dim(amin,anew) 
                  dcbus(13,it) = anew - da  
                  col = 6 * ntdc + 2 * i - 1
                  dvdo = x(col) - x(col+1)  
                  dcbus(24,it) = dcbus(24,it) + dvdo
C       
C                 Compute residual error after iteration correction 
C       
                  da = 0
                  if (dcbus(3,it) .gt. 0) then 
                     arate = 0.001 * dcbus(9,it)
                     if (arate .ne. 0.0) da = dim (abs (adi(i)),arate)
                     if (ic(i) .eq. 0) da = 0.0 
                  endif 
                  if (iv(i) .eq. 1) then
                     gerror = gerror + abs (dcbus(6,it) - vdi(i))   
                     if (debug .ne. 0) then 
                        write (dbug,330) i, iter, vdi(i), dcbus(6,it),  
     1                      vdi(i)-dcbus(6,it)  
  330                   format (' DC node ',i3,' iter ',i3,
     1                      ' V_act/sch ', 2e12.5,' error ',e12.5) 
                     endif  
                  endif 
                  if (ic(i) .eq. 1) then
                     gerror = gerror + abs (dcbus(19,it) - dcbus(5,it)) 
                     if (debug .ne. 0) then 
                        write (dbug,340) i, iter, vdi(i) * adi(i),  
     1                      dcbus(5,it), vdi(i)*adi(i)-dcbus(5,it)  
  340                   format (' DC node ',i3,' iter ',i3,
     1                      ' P_act/sch ', 2e12.5,' error ',e12.5) 
                     endif  
                  else if (ic(i) .eq. 2) then   
                     gerror = gerror
     1                      + abs (dcbus(5,it)/dcbus(6,it) - adi(i))
                     if (debug .ne. 0) then 
                        write (dbug,350) i, iter, adi(i),   
     1                      dcbus(5,it)/vdi(i), 
     2                      adi(i)-dcbus(5,it)/vdi(i)   
  350                   format (' DC node ',i3,' iter ',i3,
     1                      ' I_act/sch ', 2e12.5,' error ',e12.5) 
                     endif  
                  endif 
  360          continue 
               if (debug .ne. 0) then   
                  write (dbug,370 ) jckt, iter, error, gerror   
  370             format (' DC ckt ', i2, ' iteration ', i2, ' error ', 
     1                e12.5, ' d-c error ',e12.5)   
               endif
C       
C              Note: d-c quantities are softly constrained.  As such,   
C              any "errors" occurring indicate a physically impossible  
C              system. The following tests excursion convergence at 
C              the a-c termainal injections.
C       
C              Iterations re-enabled! 
C    
c              if( noiter ) go to 390
               if (error .le. 2.0*option(4) .and. iter .gt. 2) go to 390
C       
C              End of error iteration loop  
C       
            endif   
  380    continue   
C       
C        Compute AC terminal values from DC values  
C       
  390    continue   
        
         do 410 i = 1, ntdc 
            it = i + nec
            if (dcbus(3,it) .eq. 0) go to 410  
            if (kntry .eq. 0) then  
C       
C              Compute a-c terminal values entirely from new d-c
C              values.  
C       
               vti = vdi(i)+dsign(dcbus(8,it),dble(adi(i))) 
               rci = dcbus(23,it)   
               angle = dcbus(13,it) 
               vdocos = vti + rci * abs (adi(i))
               vdo(i) = vdocos / cos(angle) 
               ec = vdo(i) * c3 
        
            else
C       
C              Compute a-c terminal values from a-c commutator voltage  
C              and the remaining d-c quantities.
C       
               kc = dcbus(3,it)
               jt = dcbus(15,it)   
               if (jt .ne. 0) dcbus(16,it) = tap(jt)
               vdi(i) = dcbus(20,it)
               adi(i) = dcbus(19,it) / vdi(i)   
               vti = vdi(i) +dsign(dcbus(8,it),dble(adi(i))) 
               vc = dsqrt(e(kc) ** 2 + f(kc) ** 2)   
               ec = vc * dcbus(2,it) * dcbus(7,it) * dcbus(16,it)   
               vdo(i) = ec / c3 
        
            endif   
        
            p = vti * adi(i)
            ac = adi(i) / c3
            rt = dcbus(17,it) * dcbus(7,it) 
            xt = dcbus(18,it) * dcbus(7,it) 
            cs = (p + rt * ac ** 2) / (ec * ac) 
            if (abs(cs) .gt. 1.0) cs = sign(1.0,cs) 
            phi = acos(cs)  
            if (adi(i) .gt. 0) phi = -phi   
            sn = sin(phi)   
            q = -ec * ac * sn - ac ** 2 * xt
            vr = ec - ac * (cs * rt - sn * xt)  
            vi = -ac * (cs * xt + sn * rt)  
            vknew = sqrt (vr ** 2 + vi ** 2)
     1            / (dcbus(2,it) * dcbus(7,it)) 
            kt = dcbus(1,it)   
            pk = pnetu(kt)                         
            qk = qnetu(kt)                         
            pnetu(kt) = -p / bmva  
            qnetu(kt) = -q / bmva  
            dcbus(25,it) = p
            dcbus(26,it) = q
            dp = abs (pk-pnetu(kt))                
            dq = abs (qk-qnetu(kt))                
            vk = dsqrt (e(kt) ** 2 + f(kt) ** 2) 
            dv = vk - vknew 
            dvdo = vdo(i) - dcbus(24,it)
            if (dp .gt. option(4) .or. dq .gt. option(4))   
     1         kowntd = kowntd + 1  
            if (debug .ne. 0) then  
               write (dbug,400) jckt, i, kt, iv(i), ic(i),  
     1            adi(i), vdi(i), dcbus(13,it), vdo(i), 
     2            pnetu(kt), qnetu(kt), vknew, phi,   
     3            dp, dq, dv, dvdo  
  400          format (' New d-c state - circuit/node/status ', 
     1            i2,i3,i5,2i2, 
     2            t52, ' Id, Vd ',2e12.5, ' Alpha, Vdo ',2e12.5,/,  
     3            t52, ' Pk, Qk ',2e12.5, ' Vk,    Phi ',2e12.5,/,  
     4            t52, ' eP, eQ ',2e12.5, ' eVk,  eVdo ',2e12.5)
            endif   
            jt = dcbus(15,it)  
            if (jt .gt. 0) then 
               volt(kt) = vknew / vk - 1.0  
            endif   
            vlimn(kt) = vknew 
            vlimx(kt) = vknew 
  410    continue   
         if (debug .ne. 0) call iexit (istate)
C       
C        End of DC circuit loop. Print out summary of optimation.   
C       
  420 continue  
        
  430 idchg = 1 
      if (kerrsw .ne. 0) ntotcs = 2 
      return
      end   
