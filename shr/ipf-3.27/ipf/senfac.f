C    @(#)senfac.f	20.3 2/13/96
      subroutine senfac
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
 
      common /txstat/ txstat(MAXLTC)
      integer txstat
 
      integer senltc
 
      ikec = 1

C     Compute Jacobian elements for LTC

      do jt = 1,ntota
         jndex(1,jt) = ikec
         kstat = senltc (jt,0)
         l = 1

C        Normalize row

         rh = rowh(l)
         rhin = 1.0/rh
         an = rown(l)*rhin
         rj = rowj(l)
         rl = rowl(l)
         rlin = 1.0/(rl-rj*an)
         amtrx(ikec) = rhin
         amtrx(ikec+1) = rj
         amtrx(ikec+2) = rlin
         ikec = ikec+3
         jndex(2,jt) = ikec
         amtrx(ikec) = an
         ikec = ikec + 1
         do l = 2,lp
            mt = kolum(l)
            ah = rowh(l)*rhin
            an = rown(l)*rhin
            amtrx(ikec) = mt
            amtrx(ikec+1) = ah
            amtrx(ikec+2) = (rowj(l) - rj*ah)*rlin
            amtrx(ikec+3) = an
            amtrx(ikec+4) = (rowl(l) - rj*an)*rlin
            ikec = ikec+5
         enddo
      enddo

C     Add an identity row for slack bus elements

      do kta = ntota+1,ntota+nbslck
         jndex(1,kta) = ikec

C        Normalize row

         amtrx(ikec) = 1.0
         amtrx(ikec+1) = 0.0
         amtrx(ikec+2) = 1.0
         ikec = ikec+3
         jndex(2,kta) = ikec
         amtrx(ikec) = 0.0
         ikec = ikec + 1
      enddo

C     Define Jacobian elements for bus constraints

      do kt = nbslck+1,ntot
         kta = kt + ntota
         jndex(1,kta) = ikec
         call senjac (kt,0)
         mel = 0
         mel = korder(mel)
         do while (mel .gt. 0 .and. (kolum(mel) .lt. kta))
            mt = kolum(mel)

C           Eliminate column MT from working row

            ik = jndex(2,mt)
            ikstop = jndex(1,mt+1)
            xn = amtrx(ik)
            ik = ik + 1
            rh = rowh(mel)
            rn = rown(mel)-rh*xn
            rj = rowj(mel)
            rl = rowl(mel)-rj*xn
            amtrx(ikec) = mt
            amtrx(ikec+1) = rh
            amtrx(ikec+2) = rn
            amtrx(ikec+3) = rj
            amtrx(ikec+4) = rl
            ikec = ikec+5
            krw = mel

C           Perform Row MT elimination

            do while (ik .lt. ikstop)
               ml = amtrx(ik)
               if (ml .gt. max) then
                  max = ml
                  ko = lp
                  lp = mend
               else
                  do while (kolum(krw) .lt. ml)
                     ko = krw
                     krw = korder(krw)
                  enddo
               endif
               if (kolum(krw) .eq. ml) then
                  mlc = krw
               else
                  korder(mend) = korder(ko)
                  kolum(mend) = ml
                  korder(ko) = mend
                  mlc = mend
                  ko = mend
                  mend = mend+1
                  rowh(mlc) = 0
                  rown(mlc) = 0
                  rowj(mlc) = 0
                  rowl(mlc) = 0
               endif
               xh = amtrx(ik+1)
               xj = amtrx(ik+2)
               rowh(mlc) = rowh(mlc) - rh*xh - rn*xj
               rowj(mlc) = rowj(mlc) - rj*xh - rl*xj
               xn = amtrx(ik+3)
               xl = amtrx(ik+4)
               rown(mlc) = rown(mlc) - rh*xn - rn*xl
               rowl(mlc) = rowl(mlc) - rj*xn - rl*xl
               ik = ik+5
            enddo
            mel = korder(mel)
         enddo
         if (mel .eq. 0) then
           
C           Fatal error - no residual diagonal element
            
            call erexit

         else if (kolum(mel) .gt. kta) then
           
C           Fatal error - no residual diagonal element
            
            call erexit

         else

C           Normalize row

            rh = rowh(mel)
            rhin = 1.0/rh
            an = rown(mel)*rhin
            rj = rowj(mel)
            rl = rowl(mel)
            rlin = 1.0/(rl-rj*an)
            amtrx(ikec) = rhin
            amtrx(ikec+1) = rj
            amtrx(ikec+2) = rlin
            ikec = ikec+3
            jndex(2,kta) = ikec
            amtrx(ikec) = an
            ikec = ikec + 1
            mel = korder(mel)
            do while (mel .gt. 0)
               mt = kolum(mel)
               ah = rowh(mel)*rhin
               an = rown(mel)*rhin
               amtrx(ikec) = mt
               amtrx(ikec+1) = ah
               amtrx(ikec+2) = (rowj(mel) - rj*ah)*rlin
               amtrx(ikec+3) = an
               amtrx(ikec+4) = (rowl(mel) - rj*an)*rlin
               ikec = ikec+5
               mel = korder(mel)
            enddo
         endif 
      enddo
      jndex(1,ntotx) = ikec

C     End of bus loop

      return
      end
