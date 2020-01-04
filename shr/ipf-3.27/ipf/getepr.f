C    @(#)getepr.f	20.4 8/20/98
      subroutine getepr 
C
C     This subroutine preliminarily processes /EPRI_DC data.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/epridc.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
      character word(100) * 30, capital * 30, temfil * 60
 
      integer teminp
      external kmpepb, swpepb, kmpepc, swpepc
C
C     Cleanup any existing EPRI_1964 records.
C
      if (nepbus .gt. 0 .or. nepctl .gt. 0) then
         call epclup
      endif
C
C     Set logical flags to indicate EPRI_1964 data has been
C     introduced into this case.
C
C      MEPBUS = .TRUE.
C      MEPCTL = .TRUE.
      mepbus = 1
      mepctl = 1
 
      if (index(inrcd,'EPRI_DC') .ne. 0 .or.
     1    index(inrcd,'EPRIDC') .ne. 0) then
C
C        /EPRI_DC, FILE = *
C
         call scan(inrcd, word, nwrd)
         do 150 i = 1, nwrd
         word(i) = capital (word(i))
  150    continue
 
         do 160 iwrd = 2, nwrd-1, 2
            if (ickdic(word(iwrd), 'FILE*', 1) .eq. 1) then
                temfil=inpnm
                teminp=inp
                inpnm = word(iwrd+1)
                inp = 30
                ierror=0
                call opnfil (inp,inpnm,ierror)
                if (ierror.ne.0) then
                   write (errbuf(1),1410) inpnm
 1410              format ('0 File ', a, 
     1                     ' cannot be opened. File ignored.')
                   call prterx ('W',1)
                   inpnm=temfil
                   inp=teminp
                else
                   incsw = 1
                endif
 
            endif
 
  160    continue
C
         numrcd = 0
  232    read (inp, 260, end=450) inrcd
  260    format (a)
         card = inrcd(1:1)
         numrcd = numrcd + 1
C
C        Skip "." comment text.
C
         if (card .eq. '.') go to 232
 
         write (outbuf,233 ) inrcd(1:80)
  233    format (' EPRI d-c text (',a,')')
         call prtout (1)
 
         if (inrcd(1:2) .eq. 'BD' .or. inrcd(1:2) .eq. 'BZ' .or.
     1       inrcd(1:2) .eq. 'LD' .or. inrcd(1:2) .eq. 'LB' .or.
     2       inrcd(1:2) .eq. 'MD') then
            if (nepbus .gt. 100) then
               write (errbuf(1),10237) 100
10237          format ('More than ',i3, 'EPRI d-c network records.')
               call prterx ('E', 1)
               error = 1
            else
               nepbus = nepbus + 1
               epbus(nepbus) = inrcd
            endif
            go to 232
         else if (inrcd(1:2) .eq. 'CC' .or. inrcd(1:2) .eq. 'CD' .or.
     1            inrcd(1:2) .eq. 'CN' .or. inrcd(1:2) .eq. 'CR') then
            if (nepctl .gt. 100) then
               write (errbuf(1),10238) 100
10238          format ('More than ',i3, 'EPRI d-c control records.')
               call prterx ('E', 1)
               error = 1
            else
               nepctl = nepctl + 1
               epctl(nepctl) = inrcd
C
C              For sorting purposes, type CN records have identification
C              fields of previous CR appended to columns 81:109.
C
               if (inrcd(1:2) .eq. 'CN') then
                  epctl(nepctl)(81:109) = epctl(nepctl-1)(6:34)
               endif
            endif
            go to 232
         else if (index ('/(', inrcd(1:1)) .eq. 0) then
            write (errbuf(1),10239)
10239       format ('Unrecognized record')
            write (errbuf(2),10240) inrcd(1:80)
10240       format ('(', a80, ')')
            call prterx ('E', 2)
            error = 1
            go to 232
         endif
         go to 460
C
C        End-of-file. This is normal if alternate file is used.
C
  450    if (incsw .eq. 0) inrcd = '( END ) GETEPR'
C
C        Restore normal input file if alternate file invoked.
C
  460    if (incsw .eq. 1) then
            write (outbuf, 300) numrcd, inpnm
  300       format (1x, i4, ' records read from /EPRI_1964 file ',
     1         a)
            call prtout (1)
            close( unit = inp )
            inpnm=temfil
            inp=teminp
            incsw = 0
            read (inp, 260, end=450) inrcd
            card = inrcd(1:1)
         endif
 
      endif
      return
      end
