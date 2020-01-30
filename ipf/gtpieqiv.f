C    @(#)gtpieqiv.f	20.3 2/13/96
c
c This routine is part of IPFDAT
c This is a modified version of "pieqiv" and is used only by IPFDAT
c
      integer function gtpieqiv (numtext, text, newtext)
      character text(*)*(*), newtext*(*)
C                                                                      *
C     This program calculates pi-equivalent data from branch sections  *
C                                                                      *
      complex * 8 y(2,2), ex
      character code * 10
 
      logical debug
 
      gtpieqiv = 0               ! Set return status = "successful"
      debug = .false.
      ksect = 0
 
      do 220 i = 1, numtext
         ksect = ksect + 1

         read (text(i), 100, err=200 ) r, x
  100    format (t39, 2f6.5)

         z = r ** 2 + x ** 2
         if (z .eq. 0.0) then
            write (*, 120) text(i)(1:33)
  120       format (' Branch record ', a, 
     &              ' has zero transfer impedance.')
            z = 1.0
         endif
 
         if (text(i)(1:2) .eq. 'L ') then
C                                                                      *
C           "L" DATA                                                      *
C                                                                      *
            read (text(i), 130, err=200 ) g, b
  130       format (t51, 2f6.5)
 
            y(1,2) = -cmplx (r/z, -x/z)
            y(2,1) = y(1,2)
            y(1,1) = cmplx (g,b) - y(1,2)
            y(2,2) = y(1,1)
 
         else if (text(i)(1:2) .eq. 'T ') then
C                                                                      *
C           "T" DATA                                                   *
C                                                                      *
            read (text(i), 140, err=200 ) base1, base2, g, b, tap1, tap2
  140       format (t15, f4.0, t28, f4.0, t51, 2f6.5, 2f5.2)
 
            t1 = tap1 / base1
            t2 = tap2 / base2
            if (amin1 (t1,t2) .lt. 0.667 .or. amax1 (t1,t2) .gt. 1.333)
     1         then
               write (*, 150) text(i)(1:33)
  150          format ('0 Transformer ', a,
     1            ' has taps which are beyond 33% of base kv.')
            endif
            t12 = 1.0 / (t1 * t2)
            y(1,1) = cmplx (0.5 * t12 * g, -0.5 * t12 * abs(b))
            y(2,2) = y(1,1)
            y(1,2) = -cmplx (r / (z * t1 * t2), -x / (z * t1 * t2))
            y(2,1) = y(1,2)
            y(1,1) = y(1,1)
     &             + cmplx (r / (z * t1 ** 2), -x / (z * t1 ** 2))
            y(2,2) = y(2,2) 
     &             + cmplx (r / (z * t2 ** 2), -x / (z * t2 ** 2))
    
         else if (text(i)(1:2) .eq. 'TP') then
C                                                                      *
C           "TP" DATA                                                  *
C                                                                      *
            read (text(i),190,err=200 ) g, b, tap1, tap2
 
            t2 = tap2 / base2
            if (t2 .eq. 0.0) t2 = 1.0
            if (t2 .lt. 0.667 .or. t2 .gt. 1.333) then
               write (*, 160) text(i)(1:33)
  160          format ('0 Transformer ', a,
     1            ' has taps which are beyond 33% ofbase kv.')
               t2 = 1.0
            endif
            angle = 0.0174532925 * t1
            if (t1 .lt. -120.1 .or. t1 .gt. 120.1) then
               write (*, 170) text(i)(1:33)
  170          format ('0 Transformer ', a,
     1            ' has excessive phase shift (120.0) degrees).')
            endif
 
            y(1,2) = cmplx (0.5 * g, -0.5 * abs(b))
            y(1,1) = y(1,2) + cmplx (r/z, -x/z)
            y(2,2) = y(1,2) 
     &             + cmplx (r/z, -x/z) / cmplx (t2 ** 2,0.0)
            ex = cmplx (cos (angle), sin (angle))
            y(1,2) = -ex * cmplx (r / (z * t2), -x / (z * t2))
            ex = cmplx (cos (angle), -sin (angle))
            y(2,1) = -ex * cmplx (r / (z * t2), -x / (z * t2))
 
         else if (text(i)(1:1) .eq. 'E') then
C                                                                      *
C           "E" DATA                                                   *
C                                                                      *
            read (text(i), 180, err=200 ) g1, b1, g2, b2
  180       format (t51, 4f6.5)
 
            y(1,2)= -cmplx (r/z, -x/z)
            y(2,1) = y(1,2)
            y(1,1) = cmplx(g1,b1) - y(1,2)
            y(2,2) = cmplx(g2,b2) - y(2,1)
 
         else
 
            write (*, 190) text(i)(1:33)
  190       format (' Illegal branch type ', a)
            gtpieqiv = 1
            go to 220
         endif
 
         if (ksect .eq. 1) then
            call first_sect (y)
         else
            call next_sect (y)
         endif
         go to 220
 
  200    write (*, 210 ) text(i)(1:33)
  210    format (' Illegal data in field ', a)
         gtpieqiv = 1
 
  220 continue

      if (gtpieqiv .eq. 0) then
         call final_sect (y)

         if (debug) then
            write (*, 230) y(1,1), y(1,2), y(2,1), y(2,2)
  230       format ( /, ' Y(1,1) = (', e12.5, ',', e12.5, ') ',
     1               /, ' Y(1,2) = (', e12.5, ',', e12.5, ') ',
     2               /, ' Y(2,1) = (', e12.5, ',', e12.5, ') ',
     3               /, ' Y(2,2) = (', e12.5, ',', e12.5, ') ',
     4               /)
         endif 
         r = -real (cmplx (1.0,0.0) / y(1,2))
         x = -aimag (cmplx (1.0,0.0) / y(1,2))
         g1 = real (y(1,1) + y(1,2))
         b1 = aimag (y(1,1) + y(1,2))
         g2 = real (y(2,2) + y(2,1))
         b2 = aimag (y(2,2) + y(2,1))

         newtext = text(1)(1:33)
         newtext(1:2) = 'E '
         newtext(39:44) = code (r, 6, 5)
         newtext(45:50) = code (x, 6, 5)
         newtext(51:56) = code (g1, 6, 5)
         newtext(57:62) = code (b1, 6, 5)
         newtext(63:68) = code (g2, 6, 5)
         newtext(69:74) = code (b2, 6, 5)
 
         write (*, 240) newtext(1:80)
  240    format (' Consolidated branch (', a, ')')
 
      endif 
      return 
      end
