C    @(#)bprim.f	20.3 2/13/96
      subroutine bprim

C     THIS SUBROUTINE SETS UP THE B' MATRIX

      include 'ipfinc/parametr.inc'

      include 'ipfinc/apcom.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/time1.inc'

C     TEMPORARY ARRAYS FOR B-PRIME MATRIX
C     ARRAYS BP SHOULD BE DIMENSIONED
C     2*(MAX NUMBER OF NODES + MAX NUMBER OF BRANCHES)

      common /cpwork/ ipbp(MXCPBS), bp(MXCPBS+2*MXYBR),
     1                jbp(MXCPBS+2*MXYBR)
C
      ib = 1
      do 1850 k=2,nbus
         k1 = k-1
         ipbp(k1) = ib

C        IPBP CONTAINS THE STARTING ADDRESS OF THE K1TH ROW
C        FOR THE B-PRIME MATRIX

         bkk = 0.
         iptr = ipyl(1,k)
         kl = ipyl(2,k)
C
C        LOOP OVER ALL THE BRANCHES CONNECTED TO NODE K
C        IN THE LOWER TRIANGLE
C
         do 1500 l=1,kl
            m1 = mfarl(iptr) - 1
            bkm = -ykml(2,iptr)
            bkk = bkk + bkm

C           IGNORE THIS ENTRY IF THE FAR-END NODE IS NODE 0

            if (m1.eq.0) go to 1500

C           STORE B-PRIME ELEMENT. COLUMN NUMBER AND VALUE.

            jbp(ib) = m1
            bp(ib) = -bkm
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
            bkk = bkk + bkm
            jbp(ib) = m1
            bp(ib) = -bkm
            ib = ib + 1
            iptr = iptr + 1
 1700    continue
C
C        DIAGONAL TERMS FOR B-PRIME MATRIX
C
         jbp(ib) = k1
         bp(ib) = bkk
         ib = ib+1
 1850 continue

C     SET LAST ELEMENT OF POINTER ARRAYS

      ipbp(nbus) = ib
C
      if (idebug .gt. 1) then
         write(dbug,9530) (i,ipbp(i),i=1,nbus)
 9530    format(' IPBP ARRAY AND B-PRIME MATRIX',/,(1x,2i10))
         k = ib-1
         write(dbug,9550) (i,jbp(i),bp(i),i=1,k)
 9550    format(1x,2i10,e15.5)
      endif
C
C     FACTOR B-PRIME MATRIX
C
      call sfctr (iq, ipbp, bp, jbp, ipfbp, fbp, jfbp, dgbp, rpad1)
      k = ipfbp(nbus)
C
      if (idebug .gt. 1) then
         write(dbug,9580) (i,ipfbp(i),i=1,nbus)
 9580    format(' IPFBP ARRAY AFTER FACTORING B-PRIME',/,(1x,2i10))
         write(dbug,9550) (i,jfbp(i),fbp(i),i=1,k)
      endif
C
      return
      end
