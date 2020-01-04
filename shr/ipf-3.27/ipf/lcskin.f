C    @(#)lcskin.f	20.3 2/13/96
      subroutine lcskin ( s, rr, rf, xf , freq )
      implicit real * 8 (a-h, o-z), integer * 4 (i-n)

      include 'ipfinc/deck25.inc'

      s2 = s * s
      s3 = ( unity - s2 ) * rr
      r2 = freq * valu8 / s3
      rf = rr
      xf = 0.0
      if ( r2 .eq. 0.0 ) go to 270
      qremb = 0.0
      if ( s .lt. tenm6 ) go to 130
      q2 = r2 * s2
      if ( s2 .lt. 0.8 .or. q2 .gt. 64.0 .or. r2 .le. 64.0 ) go to 110
  110 if ( q2 .gt. 64.0 ) qremb = dsqrt ( q2 ) * sqrt2
      xx = dsqrt ( q2 )
      x2 = xx * xx / 64.0
      iback = 2
      if ( x2 .le. unity ) go to 170
      go to 220
  120 a = -berd
      b = -beid
      aremb = gerd
      bremb = geid
  130 xx = dsqrt( r2 )
      x2 = xx * xx / 64.0
      iback = 1
      if ( x2 .le. unity ) go to 170
      go to 220
  140 g = ber
      h = bei
      e = berd
      f = beid
      if ( s .lt. tenm6 ) go to 150
      g = a * ger - b * gei + aremb * ber - bremb *bei
      h = a * gei + b * ger + aremb * bei + bremb * ber
      e = a * gerd - b * geid + aremb * berd - bremb * beid
      f = a * geid + b * gerd + aremb * beid + bremb * berd
  150 e2f2 = e * *2 + f * *2
      s2 = xx * s3 * onehaf / e2f2
      if ( iprsup .ge. 1 ) write (lunit6,160 ) e, f, e2f2, s2
  160 format ( ' "LCSKIN".   E, F, E2F2, S2 =',  4e25.16 )
      rf = ( -h * e + g * f ) * s2
      xf = ( g * e + h * f ) * s2
      go to 270
  170 z = x2
      ber = unity
      bei = 0.0
      berd = 0.0
      beid = onehaf
      gerd = 0.0
      geid = valu9
      ger = - valu10
      gei = 0.0
      ialt = 1
      do 200 k = 1, 14
      if ( ialt .eq. 1 ) go to 180
      ber = ber + fbe(k) * z
      beid = beid + fbed(k) * z
      if ( s .lt. tenm6 ) go to 190
      geid = geid + fked(k) * z
      if ( iback .eq. 2 ) go to 190
      ger = ger + fke(k) * z
      go to 190
  180 bei = bei + fbe(k) * z
      berd = berd + fbed(k) * z
      if ( s .lt. tenm6 ) go to 190
      gerd = gerd + fked(k) * z
      if ( iback .eq. 2 ) go to 190
      gei = gei + fke(k) * z
  190 z = z * x2
  200 ialt = -ialt
      beid = beid * xx
      berd = berd * xx
      if ( s .lt. tenm6 ) go to 210
      xl = dlog( xx * onehaf )
      gerd = -xl * berd - ber / xx + beid * aaa1 + xx * gerd
      geid = -xl * beid - bei / xx - berd * aaa1 + xx * geid
      if ( iback .eq. 2 ) go to 210
      ger = -xl * ber + bei * aaa1 + ger
      gei = -xl * bei - ber * aaa1 + gei
  210 go to (140 ,120 ), iback
  220 x2 = 8.0 / xx
      z = x2
      ber = 0.0
      bei = -valu11
      berd = ber
      beid = bei
      ger = unity / sqrt2
      gei = ger
      gerd = ger
      geid = gei
      ialt = 1
      do 250 k = 1, 6
      thetar = fbe(k+14) * z
      thetai = fbed(k+14) * z
      phir = fke(k+14) * z
      phii = fked(k+14) * z
      ber = ber + thetar
      bei = bei + thetai
      ger = ger + phir
      gei = gei + phii
      if ( ialt .eq. 1 ) go to 230
      berd = berd + thetar
      beid = beid + thetai
      gerd = gerd + phir
      geid = geid + phii
      go to 240
  230 berd = berd - thetar
      beid = beid - thetai
      gerd = gerd - phir
      geid = geid - phii
  240 ialt = -ialt
  250 z = z * x2
      xl = xx * sqrt2
      if ( qremb .lt. 1.0 ) go to 260
      xl = xl - qremb
  260 thetar = -xl + berd
      thetai = -xl + beid
      z = dsqrt ( xx )
      x2 = valu12 / z
      z = valu13 / z * dexp ( thetar )
      fr = z * dcos ( thetai )
      fi = z * dsin ( thetai )
      x2 = x2 * dexp ( ber )
      thetar = x2 * dcos ( bei )
      thetai = x2 * dsin ( bei )
      z = -fr * gerd + fi * geid
      geid = -fr * geid - fi * gerd
      gerd = z
      z = aaa2 * dexp ( -qremb )
      gr = z * dsin ( qremb )
      gi = z * dcos ( qremb )
      berd = thetar * ger - thetai * gei + gerd * gr - geid * gi
      beid = thetar * gei + thetai * ger + gerd * gi + geid * gr
      ger = fr
      gei = fi
      ber = thetar + ger * gr - gei * gi
      bei = thetai + ger * gi + gei * gr
      go to (140 ,120 ), iback
  270 if ( iprsup .ge. 1 ) write (lunit6,280 ) z, thetar, thetai
  280 format ( 36h exit "lcskin".  z, thetar, thetai =,  3e25.16 )
      return
      end
 
