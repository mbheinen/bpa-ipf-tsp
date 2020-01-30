C    @(#)bdprim.f	20.3 2/13/96
      subroutine bdprim

C     THIS SUBROUTINE SETS UP THE B" MATRIX
C     FOR THE CASE WHERE B" HAS NBUS-1 ROWS
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/apcom.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/time1.inc'
 
c
C     TEMPORARY ARRAYS FOR B" MATRIX
C     ARRAYS BP SHOULD BE DIMENSIONED
C     (MAX NUMBER OF NODES + MAX NUMBER OF BRANCHES)
 
      common /cpwork/ ipbpp(MXCPBS), bpp(MXCPBS+2*MXYBR),
     1                jbpp(MXCPBS+2*MXYBR)
C
      biggy = 1.0e10
      ib = 1
      do 1850 k=2,nbus
         k1 = k-1
         ipbpp(k1) = ib
 
C        IPBP CONTAINS THE STARTING ADDRESS OF THE K1TH ROW
C        FOR THE B" MATRIX

         iptr = ipyl(1,k)
         kl = ipyl(2,k)
C
C        LOOP OVER ALL THE BRANCHES CONNECTED TO NODE K
C        IN THE LOWER TRIANGLE
C
         do 1500 l=1,kl
             m1 = mfarl(iptr) - 1
             bkm = -ykml(2,iptr)
C                    IGNORE THIS ENTRY IF THE FAR-END NODE IS NODE 1
             if (m1.eq.0) go to 1500
C                    STORE B" ELEMENT. COLUMN NUMBER AND VALUE.
             jbpp(ib) = m1
             bpp(ib) = -bkm
             ib = ib+1
 1500    iptr = iptr + 1
C
C        LOOP OVER ALL THE BRANCHES CONNECTED TO NODE K
C        IN THE UPPER TRIANGLE
C
         iptr = ipyu(1,k)
         kl = ipyu(2,k)
C
         do 1700 l=1,kl
             m1 = mfaru(iptr) - 1
             bkm = -ykmu(2,iptr)
             jbpp(ib) = m1
             bpp(ib) = -bkm
             ib = ib + 1
             iptr = iptr + 1
 1700    continue
C
C        DIAGONAL TERMS FOR B" MATRIX
C
         jbpp(ib) = k1
         bpp(ib) = cykk(2,k)
         if (ipqv(k).eq.0) bpp(ib) = sign (biggy,bpp(ib))
         ib = ib+1
 1850 continue

C     SET LAST ELEMENT OF POINTER ARRAYS

      ipbpp(nbus) = ib
C
      if (idebug .gt. 1) then
         write(dbug,9530) (i,ipbpp(i),i=1,nbus)
 9530    format(//,' IPBPP ARRAY AND B" MATRIX',/,(1x,2i10))
         k = ib-1
         write(dbug,9550) (i,jbpp(i),bpp(i),i=1,k)
 9550    format(1x,2i10,e15.5)
      endif
C
C     FACTOR B" MATRIX
C
      call sfctr (iq,ipbpp,bpp,jbpp, ipfbpp,fbpp,jfbpp, dgbpp,rpad1)
      k = ipfbpp(nbus)
C
      if (idebug .gt. 10) then
         write(dbug,9580) (i,ipfbpp(i),i=1,nbus)
 9580    format(//,' IPFBPP ARRAY AFTER FACTORING B"',/,(1x,2i10))
         write(dbug,9550) (i,jfbpp(i),fbpp(i),i=1,k)
      endif

      return
      end
