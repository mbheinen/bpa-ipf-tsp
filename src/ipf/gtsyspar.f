C    @(#)gtsyspar.f	20.6 7/18/96
C****************************************************************
C
C       File: gtsyspar.f
C
C       Purpose: Subroutine to obtain system parameters
C
C       Author: Walt Powell  Date: 1 May 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine gtsyspar (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
C       This routine obtains and applies a filter for entire input 
C       network data.
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/arcntl.inc'
        include 'ipfinc/agc.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/basval.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/slnopt.inc'

        character  text * 120, null * 1, linefeed * 1
        integer o2, apdoutbuf

        save

        null = char(0)
        linefeed = char(10)
        last = index (in_buffer, null)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        lasto2 = o2
c
c       Write system parameters
c
        write (text, 90) basval(5)
   90   format ('CASE_DT = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 100) obasnm
  100   format ('OLD_BASE = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 110) nbasnm
  110   format ('NEW_BASE = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 120) bsbrnm
  120   format ('OLD_NETD = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 130) newnetfil
  130   format ('NEW_NETD = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 140) chgnam
  140   format ('OLD_CHGF = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 150) newchgfil
  150   format ('NEW_CHGF = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 160) prgvsn
  160   format ('PRG_VERS = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 170) bmva
  170   format ('BASE_MVA = ', f8.1)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 180) idckt
  180   format ('NUM_DC_SYS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 190) ntotc
  190   format ('NUM_AREA = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 200) ntotic
  200   format ('NUM_ITIE = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 210) nztot
  210   format ('NUM_ZONE = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 220) numown
  220   format ('NUM_OWN = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 230) ntot_alf
  230   format ('NUM_BUS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 240) ntotc
  240   format ('NUM_AREA_SBUS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 245) 2*kdtot + mtdcbs
  245   format ('NUM_DC_BUS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 250) numagc
  250   format ('NUM_AGC_BUS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 260) kxtot
  260   format ('NUM_BX_BUS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 270) ntotb
  270   format ('NUM_ADJ_BUS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 280) npctvr
  280   format ('NUM_PCT_VAR_BUS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 290) ltot
  290   format ('NUM_BRN = ', i5)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 300) kbrknt
  300   format ('NUM_CKT = ', i5)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 310) kdtot + mtdcln
  310   format ('NUM_DC_LINE = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 320) ntota
  320   format ('NUM_LTC = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 330) jphno
  330   format ('NUM_PHAS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 340) lskp
  340   format ('SOLN_STATUS = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 350) numbases
  350   format ('NUM_KV = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 360) 29
  360   format ('NUM_REC_TYP = ', i4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        return
        end
