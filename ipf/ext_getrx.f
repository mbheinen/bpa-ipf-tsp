C    @(#)ext_getrx.f	20.4 8/30/00
C****************************************************************
C
C     File: ext_getrx.f
C
C     Purpose: Routine to extract area interchange data in GE format
C
C     Input parameters:
C
C             savfil   - the logical unit opened
C             version  - "23" or "24"
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: save_ged.f
C
C****************************************************************
      integer function ext_getrx (scrfil, version, option, total)
      integer scrfil, version, total(4)
      character *(*) option(10)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/ordsta.inc'
 
      integer status, error, fnd_ptia, write_ge_file
      character xbuf*256, areaname*32, areanum1*3, areanum2*3 

      ext_getrx = 0        ! set default return status = successful
      error = 0           ! initialize error count

      write (*, 10000) ntotic / 2
10000 format (' * Writing ', i5, ' area transaction records to NETDAT fi
     &le')

      write (xbuf, 10010) ntotic / 2
10010 format ('transaction data  [', i5, ']    st     sch_mw sch_mvar fl
     &g stn pid transaction name')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do ib = 1, ntotic

        p_export = arcinp(ib)
        q_export = 0.0

        if (kompr (arcint(1,ib), arcint(2,ib), junk) .lt. 0) then
          if (option(3) .eq. 'Y') then
c
c           Remove d-c compenent from scheduled intertie
c
            call getintflo (ib, tie_pexport, tie_qexport, dc_pexport, 
     &                      dc_qexport, option)
            p_export = p_export - dc_pexport
            q_export = q_export - dc_qexport

            write ( errbuf(1), 10020) arcint(1,ib), arcint(2,ib),
     &        dc_pexport
10020       format (' Area Intertie (', a10, 1x, a10, 
     &        ') is compensated ', f8.1, 
     &        ' MW for d-c flows netted as loads')
            call prterx ('W', 1)
          endif

          iptia1 = fnd_ptia (arcint(1,ib))
          if (iptia1 .le. 0) then
            write ( errbuf(1), 10030) arcint(1,ib), arcint(2,ib)
10030       format (' Cannot obtain PTI number of Area1 in Intertie (',
     &        a10, 1x, a10, ')')
            call prterx ('W', 1)
          endif
          iptia2 = fnd_ptia (arcint(2,ib))
          if (iptia2 .le. 0) then
            write ( errbuf(1), 10040) arcint(1,ib), arcint(2,ib)
10040       format (' Cannot obtain PTI number of Area2 in Intertie (',
     &        a10, 1x, a10, ')')
            call prterx ('W', 1)
          endif
          if (iptia1 .gt. 0 .and. iptia2 .gt. 0) then
            write (areanum1, fmt='(i3.3)') pti_anum(iptia1)
            write (areanum2, fmt='(i3.3)') pti_anum(iptia2)
            areaname = arcint(1,ib) // ' ' // arcint(2,ib)

            write (xbuf, 10050) pti_anum(iptia1), areanum1(2:3),
     &        'a', pti_anum(iptia2), areanum2(2:3), 'a', 1, p_export, 
     &        q_export, ' ', 1, 1000, areaname
10050       format (3x, i2, 1x, '"', a, '"', 1x, '"', a, '"', 1x, i2, 
     &        1x, '"', a, '"', 1x, '"', a, '"', ' : ', i2, 1x, 
     &        2(1x, f9.1), 1x, '"', a, '"', i2, i5, 1x, '"', a, '"') 
            last = lastch (xbuf)
            status = write_ge_file (0, xbuf(1:last))
          endif
        endif
      enddo

      write ( errbuf(1), 10110) ntotic / 2
10110 format (' Total transaction records extracted:', i5)
      call prterx ('I', 1)
 
      ext_getrx = ntotic

      return
      end
