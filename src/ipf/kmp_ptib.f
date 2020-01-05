C    @(#)kmp_ptib.f	20.8 5/3/00
C****************************************************************
C
C     File: kmp_ptib.f
C
C     Purpose: Routine to compare PTI bus names for qiksrt.
C
c     Return code:  n = <bus1> - <bus2>
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: chk_ptib.f
C
C****************************************************************
      integer function kmp_ptib ( p, q)
      integer p, q

      include 'ipfinc/parametr.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/prt.inc'

      integer MAXPTIRECORDS
      parameter (MAXPTIRECORDS = 16000)
      common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),
     &                 nextptr_2(MAXBUS), count_newbus, 
     &                 newbusno(MAXBUS), count_newzone, 
     &                 newzoneno(MAXCZN), count_newown, 
     &                 newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                 sort_tempc(MAXPTIRECORDS)
      integer array, count, htable_2, count_newbus, newbusno, 
     &        count_newzone, newzoneno, count_newown, newownno, 
     &        sort_tempc
      character tempc*80

      common /bpa_num / user_rule, num_area_rule, num_zone_rule, 
     &                  num_owner_rule, num_default_rule, 
     &                  area_rule(3,MAXCAR), zone_rule(3,MAXZON), 
     &                  owner_rule(3,MAXOWN), default_rule(2,100), 
     &                  owner_code_ge(MAXOWN)

      integer user_rule, num_area_rule, num_zone_rule, num_owner_rule,
     &        num_default_rule, area_rule, zone_rule, owner_rule, 
     &        default_rule
      character owner_code_ge*4

      integer HASHSIZE
      parameter (HASHSIZE = 1999)
      common /masterlist/ num_master, bus_master(MAXBUS),
     &    base_master(MAXBUS), area_master(MAXBUS), 
     &    zone_master(MAXBUS), owner_master(MAXBUS),
     &    case_master(MAXBUS), htable_master(0:HASHSIZE), 
     &    nextptr_master(0:MAXBUS), busnum_master(MAXBUS)

      character bus_master*8, area_master*10, zone_master*2,
     &    owner_master*3, case_master*10
      integer num_master, htable_master, nextptr_master,
     &    busnum_master
      real base_master

      integer ftn_atoi
      character busname1*8, busname2*8, areaname1*8, areaname2*8,
     &          zonename1*2, zonename2*8, ownername1*3, ownername2*3,
     &          word1(10)*32, word2(10)*32

      if (key .eq. 1) then

        kmp_ptib = kompr (pti_name(sort(p)), pti_name(sort(q)), junk)
        if (kmp_ptib .eq. 0) then
           kmp_ptib = 100.0 * (pti_base(sort(p)) - pti_base(sort(q)))
        endif
        if (kmp_ptib .eq. 0) then
           kmp_ptib = pti_area(sort(p)) - pti_area(sort(q))
        endif

      else if (key .eq. 2) then

        ip = newbusno(p)
        iq = newbusno(q)
        kmp_ptib = pti_num(ip) - pti_num(iq)

      else if (key .eq. 3) then

        ip = newzoneno(p)
        iq = newzoneno(q)
        kmp_ptib = pti_znum(ip) - pti_znum(iq)

      else if (key .eq. 4) then

        ip = newownno(p)
        iq = newownno(q)
        kmp_ptib = pti_onum(ip) - pti_onum(iq)

      else if (key .eq. 5) then

        kmp_ptib = pti_anum(p) - pti_anum(q)

      else if (key .eq. 6) then

        ip = sort(p)
        iq = sort(q)
        kmp_ptib = pti_znum(ip) - pti_znum(iq)

      else if (key .eq. 7) then

        ip = sort(p)
        iq = sort(q)
        kmp_ptib = pti_onum(ip) - pti_onum(iq)
        if (kmp_ptib .eq. 0) 
     &    kmp_ptib = kompr (owner_code_ge(ip), owner_code_ge(iq), junk)

      else if (key .eq. 8) then

        ip = sort(p)
        iq = sort(q)
        kmp_ptib = pti_anum(ip) - pti_anum(iq)

      else if (key .eq. 101) then
c
c       Sort old PTI bus numbers
c
        ip = sort_tempc(p)
        iq = sort_tempc(q)

        read (tempc(ip), 10010, err=230) numbus1, busname1, basekv1, 
     &    numarea1, numzone1
        read (tempc(iq), 10010, err=232) numbus2, busname2, basekv2, 
     &    numarea2, numzone2
10010   format (1x, i5, 2x, a8, f4.0, i4, 20x, i4)

        kmp_ptib = numbus1 - numbus2
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (busname1, busname2, junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = 100.0 * (basekv1 - basekv2)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = numarea1 - numarea2
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = numzone1 - numzone2
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (tempc(ip)(1:1), tempc(iq)(1:1), junk)
        endif
        go to 240

  230   write (errbuf(1), 10020) tempc(ip)(1:60)
10020   format ('Error decoding bus record in *.TRN file ', a)
        call prterx ('W', 1)
        error = error + 1
        go to 240

  232   write (errbuf(1), 10020) tempc(iq)(1:60)
        call prterx ('W', 1)
        error = error + 1

  240   continue

      else if (key .eq. 102) then
c
c       Sort old PTI area numbers
c
        ip = sort_tempc(p)
        iq = sort_tempc(q)

        read (tempc(ip), 10050, err=290) numarea1, areaname1
        read (tempc(ip), 10050, err=292) numarea2, areaname2
10050   format (1x, i3, 2x, a8)

        kmp_ptib = numarea1 - numarea2
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (areaname1, areaname2, junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (tempc(ip)(1:1), tempc(iq)(1:1), junk)
        endif
        go to 300

  290   write (errbuf(1), 10060) tempc(ip)(1:60)
10060   format ('Error decoding area record in *.TRN file ', a)
        call prterx ('W', 1)
        error = error + 1
        go to 300

  292   write (errbuf(1), 10060) tempc(iq)(1:60)
        call prterx ('W', 1)
        error = error + 1

  300   continue

      else if (key .eq. 103) then
c
c       Sort old PTI zone numbers
c
        ip = sort_tempc(p)
        iq = sort_tempc(q)

        read (tempc(ip), 10090, err=360) numzone1, zonename1
        read (tempc(ip), 10050, err=362) numzone2, zonename2
10090   format (1x, i3, 2x, a2)

        kmp_ptib = numzone1 - numzone2
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (zonename1, zonename2, junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (tempc(ip)(1:1), tempc(iq)(1:1), junk)
        endif
        go to 370

  360   write (errbuf(1), 10100) tempc(ip)(1:60)
10100   format ('Error decoding zone record in *.TRN file ', a)
        call prterx ('W', 1)
        error = error + 1
        go to 370

  362   write (errbuf(1), 10100) tempc(iq)(1:60)
        call prterx ('W', 1)
        error = error + 1

  370   continue

      else if (key .eq. 104) then
c
c       Sort old PTI owner numbers
c
        ip = sort_tempc(p)
        iq = sort_tempc(q)

        read (tempc(ip), 10130, err=420) numowner1, ownername1
        read (tempc(ip), 10130, err=422) numowner2, ownername2
10130   format (1x, i3, 2x, a3)

        kmp_ptib = numowner1 - numowner2
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (ownername1, ownername2, junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (tempc(ip)(1:1), tempc(iq)(1:1), junk)
        endif
        go to 430

  420   write (errbuf(1), 10140) tempc(ip)(1:60)
10140   format ('Error decoding owner record in *.TRN file ', a)
        call prterx ('W', 1)
        error = error + 1
        go to 430

  422   write (errbuf(1), 10140) tempc(iq)(1:60)
        call prterx ('W', 1)
        error = error + 1

  430   continue

      else if (key .eq. 201) then
c
c       Sort old GE bus numbers
c
        ip = sort_tempc(p)
        iq = sort_tempc(q)

        last1 = lastch (tempc(ip))
        call uscan (tempc(ip)(1:last1), word1, nwrd1, '=',  ' ,')
        last2 = lastch (tempc(iq))
        call uscan (tempc(iq)(1:last2), word2, nwrd2, '=',  ' ,')

        kmp_ptib = ftn_atoi (word1(1)) - ftn_atoi (word2(1))
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (word1(2), word2(2), junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = 100.0 * (ftn_atof(word1(3)) - ftn_atof(word2(3)))
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = ftn_atoi (word1(4)) - ftn_atoi (word2(4))
        endif

      else if (key .eq. 202) then
c
c       Sort old GE area numbers
c
        ip = sort_tempc(p)
        iq = sort_tempc(q)

        last1 = lastch (tempc(ip))
        call uscan (tempc(ip)(1:last1), word1, nwrd1, '=',  ' ,')
        last2 = lastch (tempc(iq))
        call uscan (tempc(iq)(1:last2), word2, nwrd2, '=',  ' ,')

        kmp_ptib = ftn_atoi (word1(1)) - ftn_atoi (word2(1))
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (word1(2), word2(2), junk)
        endif

      else if (key .eq. 203) then
c
c       Sort old GE zone numbers
c
        ip = sort_tempc(p)
        iq = sort_tempc(q)

        last1 = lastch (tempc(ip))
        call uscan (tempc(ip)(1:last1), word1, nwrd1, '=',  ' ,')
        last2 = lastch (tempc(iq))
        call uscan (tempc(iq)(1:last2), word2, nwrd2, '=',  ' ,')

        kmp_ptib = ftn_atoi (word1(1)) - ftn_atoi (word2(1))
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (word1(2), word2(2), junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (word1(3), word2(3), junk)
        endif

      else if (key .eq. 204) then
c
c       Sort old GE owner numbers
c
        ip = sort_tempc(p)
        iq = sort_tempc(q)

        last1 = lastch (tempc(ip))
        call uscan (tempc(ip)(1:last1), word1, nwrd1, '=',  ' ,')
        last2 = lastch (tempc(iq))
        call uscan (tempc(iq)(1:last2), word2, nwrd2, '=',  ' ,')

        kmp_ptib = ftn_atoi (word1(1)) - ftn_atoi (word2(1))
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (word1(2), word2(2), junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (word1(3), word2(3), junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (word1(4), word2(4), junk)
        endif

      else if (key .eq. 205) then

        ip = sort(p)
        iq = sort(q)
        kmp_ptib = area_rule(1,ip) - area_rule(1,iq)

      else if (key .eq. 206) then

        ip = sort(p)
        iq = sort(q)
        kmp_ptib = zone_rule(1,ip) - zone_rule(1,iq)

      else if (key .eq. 207) then

        ip = sort(p)
        iq = sort(q)
        kmp_ptib = owner_rule(1,ip) - owner_rule(1,iq)

      else if (key .eq. 301) then
c
c       Sort new GE bus numbers
c
        ip = sort(p)
        iq = sort(q)

        nptib1 = busnum_master(ip)
        nptib2 = busnum_master(iq)
        kmp_ptib = pti_num(nptib1) - pti_num(nptib2)
        if (kmp_ptib .eq. 0) then
          kmp_ptib = kompr (pti_name(nptib1), pti_name(nptib2), junk)
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = 100.0 * (pti_base(nptib1) - pti_base(nptib2))
        endif
        if (kmp_ptib .eq. 0) then
          kmp_ptib = pti_area(nptib1) - pti_area(nptib2)
        endif

      endif
      return
      end
