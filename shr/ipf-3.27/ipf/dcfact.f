C    @(#)dcfact.f	20.7 4/5/00
        subroutine dcfact (ind)
	integer ind
        dimension ind(2,*)
c
C
C       THIS SUBROUTINE FACTORS THE "A" AND "C" MATRICES IN THE
C       DECOUPLED EQUATIONS:
C
C       A * T = P
C       C * V = Q
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/dcsln.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/ntotzt.inc'
      include 'ipfinc/prt.inc'

      common /is_batch / is_batch
C
c     Local variables:
c
      integer mel, mlc, klp, krw, mlp
      integer ikstop, ml, ms, mt, ik, is_batch
      double precision rhin, rh, xh
c
C     If NTOTZT < NTOTX, reduce to NTOTZT and store in ALP. Then
C     continue factorization.
C
C     The reduced ALP system will be used with an LP to solve optimally
C     values of phase shift angle which will yield the most satisfactory
C     solution.
C
C     Begin factorization/reduction
C
      do 100 kt=1,nbslck
         ind(1,kt+ntota)=ikec
  100 ind(2,kt+ntota)=ikec
      kt=nbslck
  110 kt=kt+1
      kta=kt+ntota
      ind(1,kta)=ikec
      if (kta.eq.ntotx) go to 250
      if (ikk(1,kta).gt.0) go to 130
  120 continue
      ind(2,kta)=ikec
      go to 110

  130 call jacbfc
      if (lp.eq.0) go to 120
      mel=0
  140 mel=korder(mel)
      ms=kolum(mel)
      if (ms-kta) 160,146,142
  142 write (errbuf(1),144) intbus(kt),intbas(kt)
  144 format ('0 ILL-CONDITIONED CONSTRAINT: BUS ',a8,f7.1)
      call prterx ('W',1)
      call erexit
 
  146 if (kta .ge. ntotzt .and. msw .eq. 0) then
C
C        Store reduced columns NTOTZT to NTOTX - 1 only if prior
C        elimination has not occurred.
C
         klp = kta - ntotzt + 1
         if (alp(klp,klp) .eq. 0.0) then
            krw = mel
  148       if (krw .gt. 0) then
               mlp = kolum(krw) - ntotzt + 1
               alp(klp,mlp) = sngl(rowh(krw))
               krw = korder(krw)
               go to 148
            endif
         endif
      endif
c
c     if rowh=0 we have problems!
c
      rh=rowh(mel)
      if (abs (rh) .le. 1.0e-6) then
        rh = 1.0d-4
      endif
      rhin=1.0d0/rh
      amtrx(ikec)=rhin
      ikec=ikec+1
      ind(2,kta)=ikec
      if (kta.ge.ntotx-1) go to 110
  150 mel=korder(mel)
      if (mel.le.0) go to 110
      mt=kolum(mel)
      amtrx(ikec)=mt
      amtrx(ikec+1)=rowh(mel)*rhin
      ikec=ikec+2
      go to 150
 
  160 if (ms .ge. ntotzt .and. msw .eq. 0) then
C
C        Store reduced columns NTOTZT to NTOTX - 1
C
         klp = kta - ntotzt + 1
         krw = mel
  162    if (krw .gt. 0) then
            mlp = kolum(krw) - ntotzt + 1
            alp(klp,mlp) = sngl(rowh(krw))
            krw = korder(krw)
            go to 162
         endif
      endif
 
      rh=rowh(mel)
      ik=ind(2,ms)
      ikstop=ind(1,ms+1)-1
      if (ik.gt.ikstop) go to 140
 
      amtrx(ikec)=ms
      amtrx(ikec+1)=rh
      ikec=ikec+2
 
      krw=mel
  170 ml=amtrx(ik)
      xh=amtrx(ik+1)
      if (ml.gt.max) go to 200
  180 if (kolum(krw)-ml) 190,230,210
  190 ko=krw
      krw=korder(krw)
      go to 180
  200 max=ml
      ko=lp
      lp=mend
  210 korder(mend)=korder(ko)
      kolum(mend)=ml
      korder(ko)=mend
      mlc=mend
      ko=mend
      mend=mend+1
      rowh(mlc)=0.0d0
      if (mend .gt. 500) then
         write (errbuf(1),220)
  220    format('0 FATAL WORKING ROW OVERFLOW (500)')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
            go to 250
         else
            call prterx ('F',1)
            call erexit()
         endif
      else
         go to 240
      endif
  230 mlc=krw
  240 rowh(mlc)=rowh(mlc)-rh*xh
      ik=ik+2
      if (ik.ge.ikstop) go to 140
      go to 170
  250 continue
      return
      end
