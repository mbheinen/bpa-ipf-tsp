C    @(#)ickikk.f	20.3 2/13/96
        function ickikk(kt,mt)
C
C       THIS FUNCTION DETERMINES THE STATUS OF THE VARIABLE KT
C
C
C       ICKIKK          CONTROL
C       ------          -------
C         0             KT NOT IN CONTROL SCHEME
C         1             V(KT)-->V(MT)
C         2             V(KT)<--V(MT)
C         3             T(KT)-->V(MT)
C         4             V(KT)<--T(MT)
C        -N             ABOVE CONTROL IS FLAGGED BUT INACTIVE
C
 
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/ikk.inc'
C
        ickikk=0
        mt=0
        ix=ikk(4,kt)
  100   if (ix.eq.0.or.ix.gt.nindxx) go to 120
        if (indx(1,ix).eq.kt) go to 110
        if (iabs(indx(1,ix)).ne.kt) go to 120
        ix=ix+1
        go to 100

  110   ickikk=indx(2,ix)
        mt=indx(3,ix)
        if(ikk(2,kt).eq.0) ickikk = -ickikk
  120   continue
        return
C
C     THIS ENTRY DETERMINES THE POSSIBILITY OF ESTABLISHING
C      CONTROLS KT-->MT.
C
      entry icktmt(kt,mt)
      icktmt=0
      ix=ikk(4,kt)
  122 if(ix.eq.0.or.ix.gt.nindxx) go to 126
      if (iabs(indx(1,ix)).ne.kt) go to 126
      if (indx(3,ix).eq.mt) go to 124
      ix=ix+1
      go to 122
  124 icktmt=1
  126 ix=ikk(4,mt)
  127 if (ix.eq.0.or.ix.gt.nindxx) go to 130
      if (iabs(indx(1,ix)).ne.mt) go to 130
      if (indx(3,ix).eq.kt) go to 128
      ix=ix+1
      go to 127

  128 icktmt=icktmt+2
  130 return
C
C       THIS ENTRY RETURNS CONSTRAINT INDEX ASSOCIATED WITH TYPE JT.
C
C
C
C       JCKIKK          CONTROL
C       -------         --------
C
C       0               ILLEGAL
C       1               (NOT USED)
C       2               S(K)=F(TAP(M))
C       3               S(K)=F(AREA(M))
C       4               S(K)=F(PCNTVAR(M))
C       5               S(K)=F(TIE(M))
C       6               TAP(K)=U(M)
C       7               TAP(K)=F(TIE(M))
C
C
        entry jckikk(kt,jt)
        jckikk=0
        ix = ikk(5,kt)
  140   if (ix.eq.0.or.ix.gt.njndxx) go to 180
        if (iabs(jndx(1,ix)).ne.kt) go to 180
        if (jndx(2,ix).ne.jt) go to 150
        jckikk = jndx(3,ix)
        go to 180

  150   ix=ix+1
        go to 140

  180   continue
        return
      end
