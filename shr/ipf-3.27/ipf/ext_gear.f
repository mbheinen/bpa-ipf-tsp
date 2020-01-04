C    @(#)ext_gear.f	20.4 2/28/00
C****************************************************************
C
C     File: ext_gear.f
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
      integer function ext_gear (scrfil, version, option, total)
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
 
      integer status, error, gtge_num, write_ge_file
      character xbuf*256, areaname*32

      ext_gear = 0        ! set default return status = successful
      error = 0           ! initialize error count

      write (*, 10000) ntotc
10000 format (' * Writing ', i5, ' area records to NETDAT file')

      write (xbuf, 10010) ntotc
10010 format ('area data  [', i2, ']                            swing  d
     &esired    tol      pnet     qnet')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do ib = 1, ntotc
        nb = karea(1,ib)
        if (ordtie .eq. 2) nb = opt2inp(nb)
c
c       Get GE area, zone, owner, and bus number
c
        status = gtge_num (nb, iptia, iptiz, iptib)
        if (status .ne. 0) then
          error = 5
          write ( errbuf(1), 10020) bus(nb), base(nb) 
10020     format (' Cannot obtain bus number for area slack bus (', 
     &      a8, f7.1, ')')
          call prterx ('E', 1)
          go to 900
        endif

        call gtareint (ib, p_export, q_export, dc_pexport, dc_qexport,
     &                 option)
        areaname = arcnam(ib)
        if (option(3) .eq. 'Y' .and. dc_pexport .ne. 0.0) then
          export = arcnet(ib) * bmva - dc_pexport
          p_export = p_export - dc_pexport
          q_export = q_export - dc_qexport
          write ( errbuf(1), 10022) areaname, dc_pexport
10022     format (' Area (', a10, ') is compensated ', f8.1, 
     &       ' MW for d-c flows netted as loads')
          call prterx ('W', 1)
        else
          export = arcnet(ib) * bmva 
        endif
        write (xbuf, 10030) pti_anum(iptia), areaname,
     &    pti_num(iptib), export, 10.0, p_export, q_export
10030   format (3x, i2, 1x, '"', a, '"', 1x, 3x, i5, 4(1x, f9.1))
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

  900   continue
      enddo

      write ( errbuf(1), 10110) ntotc
10110 format (' Total area records extracted:', i5)
      call prterx ('I', 1)
 
      ext_gear = ntotc

      return
      end
