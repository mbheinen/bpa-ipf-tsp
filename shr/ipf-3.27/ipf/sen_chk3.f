C    @(#)sen_chk3.f	20.5 8/30/00
      subroutine sen_chk3 (lossless_flag)
      logical lossless_flag
 
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
      include 'ipfinc/optim1.inc'
 
      character error1*10, error2*10
      ikec = 1

      ntotx = ntot + 2*ntopt + 1

      do kt = 1, nbslck
         do jtx = 1, 2
            do i = 1, ntot+2*ntopt
               dpt(1,i) = 0.0
               dpt(2,i) = 0.0
            enddo
            if (jtx .eq. 1) then
               dpt(1,kt) = 1.0
               dpt(2,kt) = 0.0
            else
               dpt(1,kt) = 0.0
               dpt(2,kt) = 1.0
            endif
            call sen_bak2 (1, ntot+2*ntopt)
            i1_max = 0
            dpt1_max = 0.0
            i2_max = 0
            dpt2_max = 0.0
            do i = 1, ntot+2*ntopt         
               if (abs(dpt(1,i)) .gt. dpt1_max) then
                  i1_max = i
                  dpt1_max = abs(dpt(1,i))
               endif
               if (abs(dpt(2,i)) .gt. dpt2_max) then
                  i2_max = i
                  dpt2_max = abs(dpt(2,i))
               endif
            enddo
            error1 = ' '
            error2 = ' '
            if (jtx .eq. 1) then
              if (i1_max .eq. kt) then
                if (abs(dpt1_max-1.0) .ge. 1.0e-6) error1 = '* err 1 *'
              else
                error1 = '* err 1 *'
              endif
              if (abs(dpt2_max) .ge. 1.0e-6) error2 = '* err 2 *'
            else
              if (abs(dpt1_max) .ge. 1.0e-6) error1 = '* err 1 *'
              if (i2_max .eq. kt) then
                if (abs(dpt2_max-1) .ge. 1.0e-6) error2 = '* err 2 *'
              else
                error2 = '* err 2 *'
              endif
            endif
            write (*, 100) kt, jtx, i1_max, dpt1_max, i2_max, 
     &        dpt2_max, error1, error2
            write (11, 100) kt, jtx, i1_max, dpt1_max, i2_max, 
     &        dpt2_max, error1, error2
  100       format (' Row ', i4, i2, ' Column ', i4, e12.5, 1x, i4, 
     &        e12.5, 2x, a, 2x, a)
         enddo
      enddo

C     Define Jacobian elements for bus constraints

      do kt = nbslck+1,ntot+ntopt
         call sen_jac3 (kt, 0, lossless_flag)
         do jtx = 1, 2
            do i = 1, ntot+2*ntopt
               dpt(1,i) = 0.0
               dpt(2,i) = 0.0
            enddo
            if (jtx .eq. 1) then
               mel = korder(0)
               do while (mel .gt. 0)
                  mt = kolum(mel)
                  dpt(1,mt) = rowh(mel)
                  dpt(2,mt) = rown(mel)
                  mel = korder(mel)
               enddo
            else
               mel = korder(0)
               do while (mel .gt. 0)
                  mt = kolum(mel)
                  dpt(1,mt) = rowj(mel)
                  dpt(2,mt) = rowl(mel)
                  mel = korder(mel)
               enddo
            endif
            call sen_bak2 (1, ntot+2*ntopt)
            i1_max = 0
            dpt1_max = 0.0
            i2_max = 0
            dpt2_max = 0.0
            do i = 1, ntot+2*ntopt
               if (abs(dpt(1,i)) .gt. dpt1_max) then
                  i1_max = i
                  dpt1_max = abs(dpt(1,i))
               endif
               if (abs(dpt(2,i)) .gt. dpt2_max) then
                  i2_max = i
                  dpt2_max = abs(dpt(2,i))
               endif
            enddo
            error1 = ' '
            error2 = ' '
            if (jtx .eq. 1) then
              if (i1_max .eq. kt) then
                if (abs(dpt1_max-1.0) .ge. 1.0e-6) error1 = '* err 1 *'
              else
                error1 = '* err 1 *'
              endif
              if (abs(dpt2_max) .ge. 1.0e-6) error2 = '* err 2 *'
            else
              if (abs(dpt1_max) .ge. 1.0e-6) error1 = '* err 1 *'
              if (i2_max .eq. kt) then
                if (abs(dpt2_max-1) .ge. 1.0e-6) error2 = '* err 2 *'
              else
                error2 = '* err 2 *'
              endif
            endif
            write (*, 100) kt, jtx, i1_max, dpt1_max, i2_max, 
     &         dpt2_max, error1, error2
            write (11, 100) kt, jtx, i1_max, dpt1_max, i2_max, 
     &         dpt2_max, error1, error2
         enddo
      enddo

C     Define Jacobian elements for phase shifter constraints

      do jt = 1, ntxtie
         k1 = txtie(1,jt) 
         k2 = txtie(2,jt) 
         if (k1 .lt. k2) then
            ltc = txtie(7,jt)
            lt = txtie(8,jt)
            kt = ntot + ntopt + iabs (lt)
            if (ltc .eq. 0) then
               korder(0) = 1
               lp = 2
               ko = 1
               max = ntot + iabs (lt)
               kolum(1) = k1
               korder(1) = 2
               rowh(1) = 1.0
               rown(1) = 0.0  
               rowj(1) = 0.0  
               rowl(1) = 1.0
               kolum(2) = ntot + iabs (lt)
               korder(2) = 0
               rowh(2) = -1.0
               rown(2) = 0.0  
               rowj(2) = 0.0  
               rowl(2) = -1.0
               mend = 3
            else
               korder(0) = 1
               lp = 1
               ko = 0
               max = ntot + ntopt + iabs (lt)
               kolum(1) = max
               korder(1) = 0
               if (ltc .gt. 0) then
                  rowh(1) = 1.0
                  rown(1) = 0.0  
                  rowj(1) = 0.0  
                  rowl(1) = 1.0
               else
                  rowh(1) = -1.0
                  rown(1) = 0.0  
                  rowj(1) = 0.0  
                  rowl(1) = -1.0
               endif
               mend = 2
            endif
            do jtx = 1, 2
               do i = 1, ntot+2*ntopt
                  dpt(1,i) = 0.0
                  dpt(2,i) = 0.0
               enddo
               if (jtx .eq. 1) then
                  mel = korder(0)
                  do while (mel .gt. 0)
                     mt = kolum(mel)
                     dpt(1,mt) = rowh(mel)
                     dpt(2,mt) = rown(mel)
                     mel = korder(mel)
                  enddo
               else
                  mel = korder(0)
                  do while (mel .gt. 0)
                     mt = kolum(mel)
                     dpt(1,mt) = rowj(mel)
                     dpt(2,mt) = rowl(mel)
                     mel = korder(mel)
                  enddo
               endif
               call sen_bak2 (1, ntot+2*ntopt)
               i1_max = 0
               dpt1_max = 0.0
               i2_max = 0
               dpt2_max = 0.0
               do i = 1, ntot+2*ntopt         
                  if (abs(dpt(1,i)) .gt. dpt1_max) then
                     i1_max = i
                     dpt1_max = abs(dpt(1,i))
                  endif
                  if (abs(dpt(2,i)) .gt. dpt2_max) then
                     i2_max = i
                     dpt2_max = abs(dpt(2,i))
                  endif
               enddo
               error1 = ' '
               error2 = ' '
               if (jtx .eq. 1) then
                 if (i1_max .eq. kt) then
                   if (abs(dpt1_max-1.0) .ge. 1.0e-6) 
     &                error1 = '* err 1 *'
                 else
                   error1 = '* err 1 *'
                 endif
                 if (abs(dpt2_max) .ge. 1.0e-6) error2 = '* err 2 *'
               else
                 if (abs(dpt1_max) .ge. 1.0e-6) error1 = '* err 1 *'
                 if (i2_max .eq. kt) then
                   if (abs(dpt2_max-1) .ge. 1.0e-6) error2 = '* err 2 *'
                 else
                   error2 = '* err 2 *'
                 endif
               endif
               write (*, 100) kt, jtx, i1_max, dpt1_max, i2_max, 
     &           dpt2_max, error1, error2
               write (11, 100) kt, jtx, i1_max, dpt1_max, i2_max, 
     &           dpt2_max, error1, error2
            enddo
         endif
      enddo

C     End of bus loop

      return
      end
