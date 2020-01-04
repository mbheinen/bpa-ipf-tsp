C    @(#)ivaln.f	20.4 5/27/98
      integer function ivaln(i)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/elim2.inc'
      include 'ipfinc/red2.inc'

      logical finished, increment1, increment2
C
C     THIS FUNCTION COMPUTES THE VALENCY OF THE NETWORK FROM THE
C     SIMULATED REMOVAL OF NODE I.
C
      ivaln = 0
      l1 = loc(i)
      do while (l1 .gt. 0)
C
C       Process K1-I, the emanating node of any potential equivalent 
C       branch
C
        k1 = kolum(l1)
        if (k1 .ne. i) then
          ksw = 1
          l2 = loc(i)
          l3 = loc(k1)
          ivaln = ivaln - 2
          increment1 = .true.
          increment2 = .true.

          do while (increment1 .or. increment1)
            if (increment1) then
C
C             Increment I-K2, the terminating node of any potential 
C             equivalent branch
C
              finished = (l2 .eq. 0)
              do while (.not. finished)
                k2 = kolum(l2)
                if (k2 .ne. i .and. k2 .ne. k1) then
                  finished = .true.
                else
                  l2 = korder(l2)
                  finished = (l2 .eq. 0)
                endif
              enddo
              if (l2 .eq. 0 .and. 
     &           (ksw .eq. 1 .or. ksw .eq. 2)) ksw = ksw + 2
              increment1 = .false.
            endif

            if (increment2) then
C
C             Increment K1-K3, searching for an existing branch (K1-K2)
C
              finished = (l3 .eq. 0)
              do while (.not. finished)
                k3 = kolum(l3)
                if (k3 .ne. i) then
                  finished = .true.
                else
                  l3 = korder(l3)
                  finished = (l3 .eq. 0)
                endif
              enddo
              if (l3 .eq. 0 .and. 
     &           (ksw .eq. 1 .or. ksw .eq. 3)) ksw = ksw + 1
              increment2 = .false.
            endif
C
C           "KSW" IS ASSIGNED THE FOLLOWING ATTRIBUTES
C
C           1 -- NORMAL
C           2 -- END-OF-DATA for K1-K3
C           3 -- END-OF-DATA for K1-I-K2
C           4 -- END-OF-DATA (2 and 3)
C
            if (ksw .eq. 1) then
              if (k3 .lt. k2) then
C
C               INCREMENT K3

                l3 = korder(l3)
                increment2 = .true.
              else if (k3 .gt. k2) then
C
C               INCREMENT K2
 
                ivaln = ivaln + 1
                l2 = korder(l2)
                increment1 = .true.

              else
C
C               INCREMENT K2 AND K3
C
                l2 = korder(l2)
                l3 = korder(l3)
                increment1 = .true.
                increment2 = .true.
              endif
            else if (ksw .eq. 2) then
C
C             INCREMENT K2

              ivaln = ivaln + 1
              l2 = korder(l2)
              increment1 = .true.

            endif
          enddo
        endif
        l1 = korder(l1)
      enddo
      return
      end
