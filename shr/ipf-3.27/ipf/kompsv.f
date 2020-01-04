C    @(#)kompsv.f	20.3 2/13/96
        function kompsv (m,n)
C
C       This function performs an inverted compare (high-to-low)
C       upon the absolute value of array SENMAX.
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/sensit.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/ordsta.inc'
 
        i = maxsrt(m)
        j = maxsrt(n)
        if (abs(senmax(i)) .lt. abs(senmax(j))) then
           kompsv = +1
        else if (abs(senmax(i)) .gt. abs(senmax(j))) then
           kompsv = -1
        else
           if (i .lt. ntota) then
              if (ordltc .eq. 1) then
                 k1 = inp2alf(ltran(1,i))
                 m1 = inp2alf(ltran(9,i))
              else
                 k1 = inp2alf(opt2inp(ltran(1,i)))
                 m1 = inp2alf(opt2inp(ltran(9,i)))
              endif
           else
              k1 = inp2alf(opt2inp(i-ntota))
              m1 = 0
           endif
           if (j .lt. ntota) then
              if (ordltc .eq. 1) then
                 k2 = inp2alf(ltran(1,j))
                 m2 = inp2alf(ltran(9,j))
              else
                 k2 = inp2alf(opt2inp(ltran(1,j)))
                 m2 = inp2alf(opt2inp(ltran(9,j)))
              endif
           else
              k2 = inp2alf(opt2inp(j-ntota))
              m2 = 0
           endif
           kompsv = k1 - k2
           if (kompsv .eq. 2) kompsv = m1 - m2
        endif
        return
        end
