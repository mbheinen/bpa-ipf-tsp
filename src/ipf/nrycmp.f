C    @(#)nrycmp.f	20.4 1/4/99
       subroutine nrycmp
C
C       control Pij on special RZ lines by adjusting Xij.
C $$$
C $$$   Potential improvments could upgrade the sensitivity matrix
C $$$   YCPSEN by the transformation:
C $$$
C $$$   YCPSEN(j+1) = Ti * YCPSEN(j) * Ti,
C $$$
C $$$   where Ti is the elementary diagonal matrix with di = SQRT(ci).
C $$$
C $$$   and ci is the desired ratio dPij/dXij(*) and dPij/dXij(0).
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/trmdbg.inc'
      include 'ipfinc/ycomp.inc'
      include 'ipfinc/ycpsen.inc'
      include 'ipfinc/ycpsln.inc'
 
        character id * 1
C
C       Use caution in COMPLEX * 16 statement below.  Interface problems
C       may occur if complex arguments are passed as formal parameters  
C       to routines which expect COMPLEX * 8, e.g. PIEQIV.  
C       
        complex * 16 v(4), y(4,4), a(4), s(4), yij, det, yeq(2,2),  
     1               oldy11, oldy12, yxx, yxy   
        real inew,         imag(MXYCMP),   pij(MXYCMP),  pijadj(MXYCMP),
     1       dx(MXYCMP),   iovld,          vmag(MXYCMP),
     2       dp(MXYCMP),   db(MXYCMP),     serr(MXYCMP), pijvio(MXYCMP),
     3       vmax(MXYCMP), vmin(MXYCMP),   ivio(MXYCMP), iadj(MXYCMP)   
        integer status, stat(MXYCMP), statot, debug 
        
        ditot = 0.0 
        dytot = 0.0 
        kownty = 0  
        if (nycomp .eq. 0) go to 900
C            Store line terminal nodal quantities. (They are perturbed  
C            during Y-matrix modifications, and distort injection errors
C            for parallel variable compensated lines.)  
        statot = 0  
        debug = 1   
        do 140 jt = 1,nycomp
        stat(jt) = 0
        k1 = kycomp(1,jt)   
        k2 = kycomp(2,jt)   
        kt = inp2opt(k1)  
        mt = inp2opt(k2)  
        id = char (kycomp(3,jt))
        ksect = kycomp(4,jt)
        call nrpqv (kt,pk,dpk,qk,dqk,vmag(jt))  
        call nrpqv (mt,pk,dpm,qk,dqm,vm)
        serr(jt) = amax1 (abs(dpk),abs(dqk),abs(dpm),abs(dqm))  
        dp(jt) = dpk - dpm  
C                    Store Vmax, Vmin limits
        vmin(jt) = vlimn(kt)                      
        vmax(jt) = vlimx(kt)                      
C                Compute Pij in the following steps:
C                1. Define terminal injections. 
        k = 8   
        do 100 i = 1,2  
        do 100 j = 1,2  
           yeq(i,j) = dcmplx (ycomp(k,jt),ycomp(k+1,jt))   
           k = k + 2   
  100   continue
        v(1) = dcmplx (e(kt),f(kt)) 
        v(2) = dcmplx (e(mt),f(mt)) 
        a(1) = yeq(1,1) * v(1) + yeq(1,2) * v(2)
        a(2) = yeq(2,1) * v(1) + yeq(2,2) * v(2)
        xij = ycomp(32,jt)  
        bis = ycomp(36,jt)  
        dx(jt) = 0.0
        pijadj(jt) = 0.0
C                If this is a passive node sequence, the terminal voltages  
C                for section KSECT must be computed by Y-matrix identities. 
        if (ksect .eq. 0) then  
C                       Store new voltages  
           ycomp(44,jt) = dreal (v(1))   
           ycomp(45,jt) = dimag (v(1))  
           ycomp(46,jt) = dreal (v(2))   
           ycomp(47,jt) = dimag (v(2))  
        
           s(1) = v(1) * conjg (a(1))   
           pij(jt) = dreal (s(1))
        
           imag(jt) = dsqrt((dreal(a(1)))**2 + (dimag(a(1)))**2) 
        
           imag(jt) = sign (imag(jt),pij(jt))   
        
        else
C
C       1. Initialize the 4-node Y-matrix. The rows and columns are
C          ordered 1 (Node 1), 2 (Node 2), 3 (Node x), and 4 (Node y).
C
           do 110 j = 1,4
           do 110 i = 1,4
              y(i,j) = 0.0
  110      continue
C
C       2. Append subsystem (1-x), (x-y), and (y-2).
C
           k = 16
           do 120 i = 1,3,2
           do 120 j = 1,3,2
              y(i,j) = y(i,j) + dcmplx (ycomp(k,jt),ycomp(k+1,jt))
              k = k + 2
  120      continue
 
           yij = dcmplx (-1.0,0.0)/dcmplx(0.0,xij)
           y(3,4) = y(3,4) + yij
           y(4,3) = y(4,3) + yij
           y(3,3) = y(3,3) + dcmplx (0.0,bis) - yij 
           y(4,4) = -yij
        
           k = 24   
           do 130 i = 2,4,2 
           do 130 j = 2,4,2 
           y(i,j) = y(i,j) + dcmplx (ycomp(k,jt),ycomp(k+1,jt)) 
  130      k = k + 2
C       
C       3. Modify row 3 if nodes 1-x are coalesced. 
C       
           if (y(1,1) .eq. 0.0) then
              a(3) = a(1)   
           else 
              a(3) = -y(3,1) * v(1) 
           endif
C       
C       4. Modify row 4 if nodes 2-y are coalesced. 
C       
           if (y(2,2) .eq. 0.0) then
              a(4) = a(2)   
           else 
              a(4) = -y(4,2) * v(2) 
           endif
C       
C          Compute voltages 
C       
           det = y(3,3) * y(4,4) - y(3,4) * y(4,3)  
           v(3) = (y(4,4) * a(3) - y(3,4) * a(4)) / det 
           v(4) = (-y(4,3) * a(3) + y(3,3) * a(4)) / det
C       
C          Store new voltages   
C       
           ycomp(44,jt) = dreal (v(3))   
           ycomp(45,jt) = dimag (v(3))  
           ycomp(46,jt) = dreal (v(4))   
           ycomp(47,jt) = dimag (v(4))  
        
           yxy = dcmplx (-1.0,0.0)/dcmplx(0.0,xij)  
           yxx = dcmplx (0.0,bis) - yxy 
        
           a(3) = yxx * v(3) + yxy * v(4)   
           s(3) = v(3) * conjg (a(3))   
           pij(jt) = dreal (s(3))
        
           imag(jt) = dsqrt((dreal(a(3)))**2 + (dimag(a(3)))**2) 
           imag(jt) = sign (imag(jt),pij(jt))   
        
        endif   
C       
C       Compute residuals   
C       
        dpij = dim (pij(jt),ycomp(6,jt)) - dim (ycomp(5,jt),pij(jt))
        dytot = dytot + abs (dpij)  
        
        if (ycomp(40,jt) .gt. 0.0) then 
           ditot = ditot + dim (imag(jt),ycomp(40,jt)) +
     1                     dim (-ycomp(40,jt),imag(jt)) 
        endif   
C       
C       If injection error dominates, ignore X and dPij/dXij updating   
C       this iteration. 
C       
        if (abs (dpij) .gt. option(6)) then 
           da = dp(jt)/dpij 
           if ( abs (da) .gt. 2.0) stat(jt) = stat(jt) + 100
        endif   
        
        if (abs (serr(jt)) .gt. option(5)) stat(jt) = stat(jt) + 200
C       
C       YCPSCR(1,*) = Pij(o)
C       YCPSCR(2,*) = Pin(j-1)  
C       YCPSCR(3,*) = dXij(j-1) 
C       YCPSCR(4,*) = dPij(0)/dXij  
C       YCPSCR(5,*) = dPij(*)/dXij  
C       YCPSCR(6,*) = Pij (best) - Pmax 
C       
        dpdxol = ycomp(42,jt)   
        dpdxne = ycomp(42,jt)   
        
        if (ycpscr(4,jt) .eq. 0.0) then 
           ycpscr(1,jt) = pij(jt)   
           ycpscr(2,jt) = pij(jt)   
           ycpscr(3,jt) = 0.0   
           ycpscr(4,jt) = ycomp(42,jt)  
           ycpscr(5,jt) = ycomp(42,jt)  
           ycpscr(6,jt) = pij(jt) - ycomp(5,jt) 
        else if (ycpscr(3,jt) .eq. 0.0) then
           ycpscr(2,jt) = pij(jt)   
           ycpscr(6,jt) = pij(jt) - ycomp(5,jt) 
        else if (ycpscr(3,jt) .ne. 0.0) then
           dpdxne = (pij(jt) - ycpscr(2,jt)) / ycpscr(3,jt) 
C       
C          Conditionally update dPij/dXij(*) if two conditions are  
C          met: 
C       
C          1. Pij(j) is closer to scheduled Pmax.   
C          2. The ratio of the measured dPij/dXij is "close" to the 
C             anticipated value dPij/dXij(*).   
C       
           ratio = dpdxne / ycpscr(5,jt)
           if (ratio .lt. 0.25 .or .ratio .gt. 4.0) then
              ycpscr(2,jt) = pij(jt)
              ycpscr(3,jt) = 0.0
              ycpscr(6,jt) = pij(jt) - ycomp(5,jt)  
           else 
              if (abs (pij(jt) - ycomp(5,jt)) .le. abs (ycpscr(6,jt)))  
     1           then   
                 ycpscr(5,jt) = ycpscr(4,jt) +  
     1                         (ycomp(5,jt) - ycpscr(1,jt)) *   
     2                         (dpdxne - ycpscr(4,jt)) /
     3                         (pij(jt) - ycpscr(1,jt)) 
                 ycpscr(6,jt) = pij(jt) - ycomp(5,jt)   
              endif 
           endif
        endif   
C       
C       Exterpolate sensitivity dPij/dXij   
C       
        dpdxop = ycpscr(4,jt) + 
     1           (pij(jt) - ycpscr(1,jt)) * 
     2           (ycpscr(4,jt) - ycpscr(5,jt)) /
     3           (ycpscr(1,jt) - ycomp(5,jt))   
C       
C       Set flag to abandon DX, DB adjustment if DPDXOP is significantly
C       different from DPDXNE.  
C       
        ratio = dpdxne / dpdxop 
        if (ratio .lt. 0.25 .or .ratio .gt. 4.0) then   
           stat(jt) = stat(jt) + 500
        endif   
        if (ycpscr(3,jt) .ne. 0 .and. stat(jt) .lt. 100) then   
           ycomp(42,jt) = dpdxop
        endif   
        
  140   statot = statot + stat(jt)  
C       
C       Conditionally modify sensitivities to conform with DPDXOP.  
C       
        if (statot .eq. 0 .and. statot .ne. 0) then 
C       
C       Logic disabled 4 Oct 1985 - WLP 
C       
           if (debug .ne. 0) then   
              do 146 jt = 1,nycomp  
              if (jt .eq. 1) then   
                 write (dbug,142) jt,(j,ycpsen(jt,j),j=1,nycomp)
  142            format (' NRYCMP/YCPSEN (old) - row ',i3,  
     1                   5(1x,i4,1x,e12.4) /
     2              (31x,5(1x,i4,1x,e12.4)))
                 if (iterm .ne. 0) then 
                    write (*,142) jt,(j,ycpsen(jt,j),j=1,nycomp)
                 endif  
              else  
                 write (dbug,144) jt,(j,ycpsen(jt,j),j=1,nycomp)
  144            format ('                       row ',i3,  
     1                   5(1x,i4,1x,e12.4) /
     2              (31x,5(1x,i4,1x,e12.4)))
                 if (iterm .ne. 0) then 
                    write (*,144) jt,(j,ycpsen(jt,j),j=1,nycomp)
                 endif  
              endif 
  146         continue  
           endif
           do 160 jt = 1, nycomp
           ratio = sqrt (ycomp(42,jt) / ycpsen(jt,jt))  
           do 150 j = 1, nycomp 
           if (j .ne. jt) then  
              ycpsen(jt,j) = ycpsen(jt,j) * ratio   
              ycpsen(j,jt) = ycpsen(j,jt) * ratio   
           endif
  150      continue 
  160      continue 
           do 170 jt = 1, nycomp
           ycpsen(jt,jt) = ycomp(42,jt) 
  170      continue 
           if (debug .ne. 0) then   
              do 176 jt = 1,nycomp  
              if (jt .eq. 1) then   
                 write (dbug,172) jt,(j,ycpsen(jt,j),j=1,nycomp)
  172            format (' NRYCMP/YCPSEN (new) - row ',i3,  
     1                   5(1x,i4,1x,e12.4) /
     2              (31x,5(1x,i4,1x,e12.4)))
                 if (iterm .ne. 0) then 
                    write (*,172) jt,(j,ycpsen(jt,j),j=1,nycomp)
                 endif  
              else  
                 write (dbug,174) jt,(j,ycpsen(jt,j),j=1,nycomp)
  174            format ('                       row ',i3,  
     1                   5(1x,i4,1x,e12.4) /
     2              (31x,5(1x,i4,1x,e12.4)))
                 if (iterm .ne. 0) then 
                    write (*,174) jt,(j,ycpsen(jt,j),j=1,nycomp)
                 endif  
              endif 
  176         continue  
           endif
        endif   
C       Obtain DX,DB using LP: Minimize Z,  
C       
C       Z = CP * (P_VIOLATION) + CV * (V_VIOLATION) + CI * (I_VIOLATION)
C       
        call ycplp (pij,pijadj,imag,vmag,vmin,vmax,dx,db,pijvio,
     1              iadj,ivio,z,status) 
        
        do 300 jt = 1,nycomp
        
        k1 = kycomp(1,jt)   
        k2 = kycomp(2,jt)   
        
        kt = inp2opt(k1)  
        mt = inp2opt(k2)  
        
        xij = ycomp(32,jt)  
        bis = ycomp(36,jt)  
        
        xijnew = xij + dx(jt)   
        bisnew = bis + db(jt)   
        pijnew = pij(jt) + pijadj(jt)   
        
        k = 8   
        do 190 i = 1,2  
        do 190 j = 1,2  
        yeq(i,j) = dcmplx (ycomp(k,jt),ycomp(k+1,jt))   
  190   k = k + 2   
C       
C       Compute residuals   
C       
        inew = imag(jt) + iadj(jt)  
        if (ycomp(40,jt) .gt. 0.0) then 
           iovld = dim (abs(inew),ycomp(40,jt)) 
        else
           iovld = 0.0  
        endif   
        
        if (idswb .ne. 0) then  
           write (dbug,200) jt, status, stat(jt), kt, mt, z,
     1        pij(jt), pijadj(jt), pijnew, pijvio(jt),  
     2        xij, dx(jt), xijnew, ycomp(34,jt), ycomp(35,jt),  
     3        bis, db(jt), bisnew, ycomp(38,jt), ycomp(39,jt),  
     4        imag(jt), iadj(jt), inew, ivio(jt), iovld,
     5        dpdxol, dpdxne, dpdxop
  200      format (' NRYCMP JT,STATUS,STAT,KT,MT,Z       ',5i5,5x,e10.3 
     1           / '        PIJ,DPIJ,PIJNEW,PIJVIO       ',4f10.5   
     2           / '        XIJ,DX,XIJNEW,XIJMIN,XIJMAX  ',5f10.5   
     3           / '        BIS,DB,BISNEW,BISMIN,BISMAX, ',5f10.5   
     4           / '        IOLD,DI,INEW,IVIO,IOVLD      ',5f10.5   
     5           / '        dP/dX(0),(j-1),(j)           ',3e10.3 ) 
           if (iterm .ne. 0) then   
              write (*,200) jt, status, stat(jt), kt, mt, z,
     1           pij(jt), pijadj(jt), pijnew, pijvio(jt),   
     2           xij, dx(jt), xijnew, ycomp(34,jt), ycomp(35,jt),   
     3           bis, db(jt), bisnew, ycomp(38,jt), ycomp(39,jt),   
     4           imag(jt), iadj(jt), inew, ivio(jt), iovld, 
     5           dpdxol, dpdxne, dpdxop 
        
           endif
        endif   
        
        if (abs (dx(jt)) .le. 1.0e-5 .or. statot .ge. 100) then 
        
        else
        
           kownty = kownty + 1  
C       
C          Store new sensitivity values:
C       
C          YCPSCR(2,*) = Pij(j-1)   
C          YCPSCR(3,*) = dXij(j-1)  
C       
           ycpscr(2,jt) = pij(jt)   
           ycpscr(3,jt) = dx(jt)
C       
C          Store new admittances
C       
           ycomp(32,jt) = xijnew
           ycomp(36,jt) = bisnew
C       
C          Modify Y-matrix  
C       
C       1. Initialize the 4-node Y-matrix. The rows and columns are 
C          ordered 1 (Node 1), 2 (Node 2), 3 (Node x), and 4 (Node y).  
C       
           do 210 i = 1,4   
           do 210 j = 1,4   
  210      y(i,j) = 0.0 
C       
C       2. Append subsystem (1-x), (x-y), and (y-2).
C       
           k = 16   
           do 220 i = 1,3,2 
           do 220 j = 1,3,2 
           y(i,j) = y(i,j) + dcmplx (ycomp(k,jt),ycomp(k+1,jt)) 
  220      k = k + 2
        
           if (xijnew .eq. 0.0) xijnew = 1.0e-6 
           yij = dcmplx (-1.0,0.0)/dcmplx(0.0,xijnew)   
           y(3,4) = y(3,4) + yij
           y(4,3) = y(4,3) + yij
           y(3,3) = y(3,3) + dcmplx (0.0,bisnew) - yij  
           y(4,4) = -yij
        
           k = 24   
           do 230 i = 2,4,2 
           do 230 j = 2,4,2 
           y(i,j) = y(i,j) + dcmplx (ycomp(k,jt),ycomp(k+1,jt)) 
  230      k = k + 2
C       
C       3. If Y(1,1) = 0, then coalesce nodes x and 1;  
C          otherwise eliminate node x.  
C       
           if (y(1,1) .eq. 0.0) then
C       
C             r1' = r1 + r3;
C             c1' = c1 + c3 
C       
C             Y(1,1) = Y(1,1) + Y(1,3) + Y(3,1) + Y(3,3)
C             Y(1,4) = Y(1,4) + Y(3,4)  
C             Y(4,1) = Y(4,1) + Y(4,3)  
C       
              y(1,1) = y(3,3)   
              y(1,4) = y(3,4)   
              y(4,1) = y(4,3)   
           else 
              y(3,1) = y(3,1) / y(3,3)  
              y(3,4) = y(3,4) / y(3,3)  
              y(4,4) = y(4,4) - y(4,3) * y(3,4) 
              y(4,1) = -y(4,3) * y(3,1) 
              y(1,1) = y(1,1) - y(1,3) * y(3,1) 
              y(1,4) = -y(1,3) * y(3,4) 
           endif
C       
C       4. If Y(2,2) = 0, then coalesce nodes y and 2;  
C          otherwise eliminate node x.  
C       
           if (y(2,2) .eq. 0.0) then
C       
C             r2' = r2 + r4;
C             c2' = c2 + c4 
C       
C             Y(2,2) = Y(2,2) + Y(2,4) + Y(4,2) + Y(4,4)
C             Y(1,2) = Y(1,2) + Y(1,4)  
C             Y(2,1) = Y(2,1) + Y(4,1)  
C       
              y(2,2) = y(4,4)   
              y(1,2) = y(1,4)   
              y(2,1) = y(4,1)   
           else 
              y(4,1) = y(4,1) / y(4,4)  
              y(4,2) = y(4,2) / y(4,4)  
              y(1,1) = y(1,1) - y(1,4) * y(4,1) 
              y(1,2) = y(1,2) - y(1,4) * y(4,2) 
              y(2,1) = y(2,1) - y(2,4) * y(4,1) 
              y(2,2) = y(2,2) - y(2,4) * y(4,2) 
           endif
C       
C          Y(1,1):Y(2,2) now contains the new Y-matrix values.  
C       
           k = 8
           do 232 i = 1,2   
           do 232 j = 1,2   
           ycomp(k,jt) = dreal (y(i,j))  
           ycomp(k+1,jt) = dimag (y(i,j))   
  232      k = k + 2
           do 240 l = km(kt), km(kt)-1+kmlen(kt)  
           if (ikmu(l) .eq. mt) then               
              oldy12 = dcmplx (gkmu(l),bkmu(l))   
              l1 = l
              gkmu(l) = gkmu(l) - dreal (yeq(1,2)) + dreal (y(1,2)) 
              bkmu(l) = bkmu(l) - dimag (yeq(1,2)) + dimag (y(1,2))   
       endif                                        
  240  continue                                     
              oldy11 = dcmplx (gkku(kt) ,bkku(kt))
              l2 = l
              gkku(kt) = gkku(kt) - dreal (yeq(1,1)) + dreal (y(1,1))   
              bkku(kt) = bkku(kt) - dimag (yeq(1,1)) + dimag (y(1,1)) 
           if (idswb .ne. 0) then   
               write (dbug,250) kt,mt,oldy11,oldy12,gkku(kt),bkmu(kt),
     1           gkmu(l1),bkmu(l1)                
  250         format (' Y-matrix adjustment : Old Y11 ',2i6,2e12.5, 
     1              /,'                       Old Y12 ',12x,2e12.5, 
     2              /,'                       New Y11 ',12x,2e12.5, 
     3              /,'                       New Y12 ',12x,2e12.5) 
           endif
        
           do 260 l = km(mt), km(mt)-1+kmlen(mt)  
           if (ikmu(l) .eq. kt) then               
              oldy12 = dcmplx (gkmu(l), bkmu(l))  
              l1 = l
              gkmu(l) = gkmu(l) - dreal (yeq(2,1)) + dreal (y(2,1)) 
              bkmu(l) = bkmu(l) - dimag (yeq(2,1)) + dimag (y(2,1))   
           endif                                    
  260      continue                                 
              oldy11 = dcmplx (gkku(mt),bkku(mt))      ! diagonal 
              gkku(mt) = gkku(mt) - dreal (yeq(2,2)) + dreal (y(2,2))   
              bkku(mt) = bkku(mt) - dimag (yeq(2,2)) + dimag (y(2,2)) 
           if (idswb .ne. 0) then   
              write (dbug,250) mt,kt,oldy11,oldy12,gkku(mt),bkku(mt), 
     1           gkmu(l1), bkku(l1)               
           endif
C                    Modify TIE admittance if K1 - K2 is also a tie line
           if (kycomp(7,jt) .ne. 0) then
              iax = kycomp(7,jt)
              if (tie(1,iax) .eq. kt .and. tie(7,iax) .eq. mt) then   
        
                 oldy11 = dcmplx (tie(3,iax),tie(4,iax))
                 oldy12 = dcmplx (tie(5,iax),tie(6,iax))
        
                 tie(5,iax) = tie(5,iax) - dreal (yeq(1,2))  
     1                                   + dreal (y(1,2))
                 tie(6,iax) = tie(6,iax) - dimag (yeq(1,2)) 
     2                                   + dimag (y(1,2))   
                 tie(3,iax) = tie(3,iax) - dreal (yeq(1,1))  
     1                                   + dreal (y(1,1))
                 tie(4,iax) = tie(4,iax) - dimag (yeq(1,1)) 
     1                                   + dimag (y(1,1))   
              else if (tie(1,iax) .eq. mt .and. tie(7,iax) .eq. kt)   
     1           then   
        
                 oldy11 = dcmplx (tie(3,iax),tie(4,iax))
                 oldy12 = dcmplx (tie(5,iax),tie(6,iax))
        
                 tie(5,iax) = tie(5,iax) - dreal (yeq(2,1))  
     1                                   + dreal (y(2,1))
                 tie(6,iax) = tie(6,iax) - dimag (yeq(2,1)) 
     2                                   + dimag (y(2,1))   
                 tie(3,iax) = tie(3,iax) - dreal (yeq(2,2))  
     1                                   + dreal (y(2,2))
                 tie(4,iax) = tie(4,iax) - dimag (yeq(2,2)) 
     1                                   + dimag (y(2,2))   
              else  
                 call erexit
              endif 
              if (idswb .ne. 0) then
                 write (dbug,270) iax,ifix(sngl(tie(1,iax))),
     &              ifix(sngl(tie(7,iax))),  
     1              oldy11,oldy12,tie(3,iax),tie(4,iax),
     2              tie(5,iax),tie(6,iax)   
  270            format (' Tie adjustment',i6,': Old Y11 ',2i6,2e12.5,  
     1              /,'                          Old Y12 ',12x,2e12.5,  
     2              /,'                          New Y11 ',12x,2e12.5,  
     3              /,'                          New Y12 ',12x,2e12.5)  
              endif 
           endif
        endif   
  300   continue
        write (outbuf,310) ditot, dytot, kownty 
  310   format (t53,2f13.5,i7)  
        call prtout(1)  
  900   continue
        return  
        end 
