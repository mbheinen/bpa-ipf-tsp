C    @(#)lcsymm.f	20.3 2/13/96
      subroutine lcsymm ( p, z )
      implicit real * 8 (a-h, o-z), integer * 4 (i-n)
      parameter (MAXHAF=171) ! N / 2 * (N/2+1) / 2
      dimension p(MAXHAF), z(MAXHAF)
      dimension ar(3,3), ai(3,3), fr(3), fi(3)

      include 'ipfinc/deck25.inc'

      onetrd = 1. / 3.
      switxx = switch
  100 if ( kcirct .eq. 2 ) go to 270
      fr(1) = unity
      fi(1) = 0.0
      fr(2) = -onehaf
      fi(2) = valu7
      fr(3) = -onehaf
      fi(3) = -fi(2)
      kparam = 0
  110 ki = kparam * ( kparam + 1 ) / 2
      j = ki + kparam
      kold = kparam
      kparam = kparam + 3
      if ( kparam .gt. kcirct ) go to 260
  120 l = ki + 1
      k = 0
  130 k = k + 1
      if ( ki .eq. j ) go to 250
  140 l3 = l + 2
  150 l2 = l + 1
  160 do 180 i = 1, 3
      f1 = p(l) + fr(i) * ( p(l2) + p(l3) )
      f2 = fi(i) * ( p(l2) - p(l3) )
      if ( switxx .lt. 0.0 ) go to 170
      f1 = f1 + fi(i) * ( z(l3) - z(l2) )
      f2 = f2 + z(l) + fr(i) * ( z(l2) + z(l3) )
  170 ar(i,k) = f1
  180 ai(i,k) = f2
      if ( k .eq. 3 ) go to 190
      l = l + kold + k
      go to 130
  190 l = ki
      k = 0
  200 k = k + 1
      do 220 i = 1, 3
      m = l + i
      if ( ki .lt. j ) go to 210
      if ( i .gt. k ) go to 230
  210 d20 = fr(k) * ( ar(i,2) + ar(i,3) )
      d21 = fi(k) * ( ai(i,3) - ai(i,2) )
      p(m) = ( ar(i,1) + d20 + d21 ) * onetrd
      d20 = fr(k) * ( ai(i,2) + ai(i,3) )
      d21 = fi(k) * ( ar(i,2) - ar(i,3) )
  220 z(m) = ( ai(i,1) + d20 + d21 ) * onetrd
  230 if ( k .eq. 3 ) go to 240
      l = l + kold + k
      go to 200
  240 ki = ki + 3
      if ( ki .gt. j ) go to 110
      go to 120
  250 if ( k .eq. 3 ) go to 140
      l2 = j + kparam - 1
      l3 = l2 + kparam
      if ( k .eq. 2 ) go to 150
      l3 = l3 - 1
      go to 160
  260 kparam = kparam - 3
      go to 280
  270 f1 = ( p(1) + p(3) ) * onehaf
      f2 = p(2)
      p(2) = ( p(1) - p(3) ) * onehaf
      p(1) = f1 + f2
      p(3) = f1 - f2
      p(5) = p(3)
      kparam = 2
      if ( switxx .lt. 0.0 ) go to 280
      f1 = ( z(1) + z(3) ) * onehaf
      f2 = z(2)
      z(2) = ( z(1) - z(3) ) * onehaf
      z(1) = f1 + f2
      z(3) = f1 - f2
      z(5) = z(3)
  280 if ( iprsup .ge. 1 ) write (lunit6,290 )
  290 format ( ' Exit  "LCSYMM".'  )
      return
      entry lcsym1( p, z )
      switxx = unity
      go to 100
      end
 
