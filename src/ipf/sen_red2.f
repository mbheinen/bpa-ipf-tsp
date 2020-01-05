C    @(#)sen_red2.f	20.1 10/10/96
      subroutine sen_red2 (kt, ikec)
 
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
 
         jndex(1,kt) = ikec
         mel = 0
         mel = korder(mel)
         do while (mel .gt. 0 .and. (kolum(mel) .lt. kt))
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
                  do while (kolum(krw) .lt. ml .and. krw .gt. 0)
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

         else if (kolum(mel) .gt. kt) then
           
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
            jndex(2,kt) = ikec
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
         jndex(1,kt+1) = ikec

      return
      end
