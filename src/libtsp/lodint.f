C    %W% %G%
      subroutine lodint(ibus, pld, qld, nitem, iecsl, ifreq, nbusl, 
     &                  nbusr, incrm)
c    
c     THIS SUBROUTINE INITIALIZES THE LOAD REPRESENTATION TABLES
c     I IS THE BUS NUMBER BEING PROCESS, PLD IS THE REAL LOAD IN
c     PER UNIT AT THE BUS, AND QLD IS THE REACTIVE LOAD IN PER
c     UNIT AT THE BUS.  LODINT IS CALLED BY INITL3.
c    
C     Input parameters:
c        ibus     - a-c bus number.
c        pld      - Pload (p.u.)
c        qld      - Qload (p.u.)
c        niterm   - Counter of number of load representation items
c        iecsl    - index to BUSLDN(*,*) for first iterm
c        ifreq    - Frequency Factor ldp, ldq
c        nbusl    -
c        nbusr    -
c        incrm    -
c 
      include 'tspinc/params.inc'
      include 'tspinc/param.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/ldidxn.inc'
      include 'tspinc/ldndxp.inc'
      include 'tspinc/ldshdn.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'

C     Local variables
      equivalence (ivgt1, vgt1), (asat, iasat), (istgt1, cstgt1),
     &            (bsat, ibsat)

C     Begin     Begin     Begin     Begin     Begin     Begin
c    
c     ADD TRANSFORMER AND LOAD SATURATION LOGIC
c     WARNING!!! THIS LOGIC HAS NEVER BEEN DEBUGGED
c    
      if (ifreq .ge. 4) then
        if (nbusl .eq. nbusr) then
          ivgt1 = ldndxp(1, incrm)
          istgt1 = ldndxp(2, incrm)
          dmva = busldn(1, iecsl+1)
          vtxbse = busldn(2, iecsl+1)
          cstat1 = busldn(1, iecsl+2)
          vsfull = busldn(2, iecsl+2)
        else
          ifreq = 0
        endif
      endif
      do ind = iecsl, iecsl + nitem - 1
C       
C       IFREQ .GT. 3 MEANS SATURATION LOGIC
C       
        if (ifreq .gt. 3) then
          if (ind .eq. 2 .or. ind .eq. 3) goto 100
        endif
C       
C       MODIFY BUSLD TABLE SO EACH ENTRY IS PER UNIT LOAD
C       (ON 100 MVA BASE) FOR THAT LOAD TYPE
C       
        busldn(1, ind) =  - pld*busldn(1, ind)
        busldn(2, ind) =  - qld*busldn(2, ind)
        if (keybrd(31) .ne. 0) then
          write (outbuf, 10000) busldn(1, ind), busldn(2, ind), ind
          call prtout(1)
10000     format ('0', 5x, 'P,Q', 2f9.4, 2x, i5)
        endif
C       
C       TEST FOR TRANSFORMER AND LOAD SATURATION LOGIC
C       
  100   continue
      enddo
      if (ifreq .ge. 4) then
C       
C       SET ITYP2, ITYP3 = 0.0 FOR TX, LD SATURATION
C       
        ldidxn(2, ibus) = 0
        ldidxn(3, ibus) = 0
C       
C       FOR TX OR LD
C       
        if (ifreq .eq. 5) then
C         
C         THIS A LOAD REPRESENTATION!
C         
          vcnv = 1.0
          ckamp = dmva*pld
          if (pld .eq. 0.0) then
            ldndxp(1, incrm) = 0.0
            ldndxp(2, incrm) = 0.0
            ldndxp(3, incrm) = 6
            goto 120
          else
            cio = ckamp*cstat1/100.
          endif
        else
C         
C         PROCESS FOR TX DATA
C         
          ibase = ixnamn(ibus)
          sbaskv = basekv(ibase)
C         
C         CHECK FOR OPTION USED
C         
          ckamp = dmva*sbaskv/(bmva*vtxbse)
          cio = ckamp*cstat1/100.
          vcnv = sbaskv/vtxbse
          if (vgt1 .ne. 0.0 .and. cstgt1 .ne. 0.0) then
            cgt1 = ckamp*cstgt1/100.
            bsat = cio*vgt1/(cgt1*vgt1-cgt1*vsfull)
            asat =  - bsat*vsfull
            goto 110
          endif
        endif
        bsat = 1.0/(1.0-vsfull)
        asat = 1.0 - bsat
  110   ldndxp(1, incrm) = iasat
        ldndxp(2, incrm) = ibsat
  120   csatmx = 3.0*cio*vsfull/(asat+bsat*0.90*vsfull)
      endif
      return
      end
