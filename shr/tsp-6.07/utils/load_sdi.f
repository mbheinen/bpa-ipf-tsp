	integer function load_sdi (lunsdi, sdifile, max_sdi, num_sdi, 
     &                             sdi_data)
        integer lunsdi, num_sdi
        character sdifile *(*), sdi_data(*)*80

        integer MAXBUS
        parameter (MAXBUS = 8000)

        common /scratch/ lnetc,  lnetn, basekv
        character lnetc(MAXBUS)*8
        integer lnetn(MAXBUS)
        real basekv(120)
        
        character newexctyp*2, code*4, basekvc*4, text*80
        external newexctyp
        character*10 swdata, savout, swdate, pfdate, datx, dev1, 
     &               dev2
        integer lrd, lrr, lsc, ldar, ldz, lrep, ls, ln, mac, count

        load_sdi = 0         ! Default return "success"
        num_sdi = 0

        if (lunsdi .eq. 0) go to 920

        read (lunsdi, end=900) swdata, savout, swdate, pfdate    

      ! Fetch number of base kV's
        read (lunsdi, end=900) ibxyz, (basekv(ii),ii=1,ibxyz)

      ! Fetch LOCAL RELAY header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, lrd   
        write (*, 90000) 1, dev1, dev2, datx, lrd
90000   format (' Record ', i4, ' [', a10, 1x, a10, 1x, a10,
     &    i4, ']')
        if (num_sdi + lrd .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, lrd, num_sdi
10000     format (' Overflow reading *.SDI block [', a, 1x, a, 1x,
     &      a, 1x, i4, '] total = ', i5) 
          load_sdi = 1
          go to 900
        endif
        i = 0
        count = lrd
        do while (count .gt. 0)

      !    Relay data is stored in 100-sdi_data blocks

           num = min (100, count)
           read (lunsdi, end=900) (sdi_data(ii),ii=i+1+num_sdi,
     &                             i+num+num_sdi)
           count = count - num
           i = i + num
        enddo
        num_sdi = num_sdi + lrd

      ! Fetch REMOTE RELAY header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, lrr
        write (*, 90000) 2, dev1, dev2, datx, lrr
        if (num_sdi + lrr .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, lrr, num_sdi
          load_sdi = 1
          go to 900
        endif
        if (lrr .gt. 0) then
           read (lunsdi, end=900) (sdi_data(ii),ii=1+num_sdi,
     &                             lrr+num_sdi)
        endif
        num_sdi = num_sdi + lrr

      ! Fetch SERIES CAPACITOR header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, lsc
        write (*, 90000) 3, dev1, dev2, datx, lsc
        if (num_sdi + lsc .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, lsc, num_sdi
          load_sdi = 1
          go to 900
        endif
        if (lsc .gt. 0) then
           read (lunsdi, end=900) (sdi_data(ii),ii=1+num_sdi,
     &                             lsc+num_sdi)
        endif
        num_sdi = num_sdi + lsc

      ! Fetch LOAD REPRESENTATION BY AREA header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, ldar
        write (*, 90000) 4, dev1, dev2, datx, ldar
        if (num_sdi + ldar .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, ldar, num_sdi
          load_sdi = 1
          go to 900
        endif
        if (ldar .gt. 0) then
           read (lunsdi, end=900) (sdi_data(ii),ii=1+num_sdi,
     &                             ldar+num_sdi)
        endif
        num_sdi = num_sdi + ldar

      ! Fetch LOAD REPRESENTATION BY ZONE header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, ldz
        write (*, 90000) 5, dev1, dev2, datx, ldz
        if (num_sdi + ldz .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, ldz, num_sdi
          load_sdi = 1
          go to 900
        endif
        i = 0
        count = ldz
        do while (count .gt. 0)

      !    Load Representation data is stored in 100-sdi_data blocks

           num = min (100, count)
           read (lunsdi, end=900) (sdi_data(ii),ii=i+1+num_sdi,
     &                             i+num+num_sdi)
           count = count - num
           i = i + num
        enddo
        num_sdi = num_sdi + ldz

      ! Fetch LOAD REPRESENTATION BY BUS header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, lrep
        write (*, 90000) 6, dev1, dev2, datx, lrep
        if (num_sdi + lrep .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, lrep, num_sdi
          load_sdi = 1
          go to 900
        endif
        i = 0
        count = lrep
        do while (count .gt. 0)

      !    Load Representation data is stored in 100-sdi_data blocks

           num = min (100, count)
           read (lunsdi, end=900) (sdi_data(ii),ii=i+1+num_sdi,
     &                             i+num+num_sdi)
           count = count - num
           i = i + num
        enddo
        num_sdi = num_sdi + lrep

      ! Fetch LOAD SHEDDING header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, ls
        write (*, 90000) 7, dev1, dev2, datx, ls
        if (num_sdi + ls .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, ls, num_sdi
          load_sdi = 1
          go to 900
        endif
        i = 0
        count = ls
        do while (count .gt. 0)

      !    Load Representation data is stored in 100-sdi_data blocks

           num = min (100, count)
           read (lunsdi, end=900) (sdi_data(ii),ii=i+1+num_sdi,
     &                             i+num+num_sdi)
           i = i + num
           count = count - num
        enddo
        num_sdi = num_sdi + ls

      ! Fetch LOAD NETTING header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, ln, ibxyz
        write (*, 90000) 8, dev1, dev2, datx, ln
        if (num_sdi + ln .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, ln, num_sdi
          load_sdi = 1
          go to 900
        endif
        if (ln .gt. 0) then
           read (lunsdi, end=900) (lnetc(ii),ii=1,ln), 
     &                            (lnetn(ii),ii=1,ln),
     &                            (basekv(ii),ii=1,ibxyz)
        endif
        do i = 1, ln
          basekvc = code (basekv(lnetn(i)), 4, 0)
          write (text, 10010) lnetc(i), basekvc
10010     format ('LN ', a, a)
          sdi_data(i+num_sdi) = text
        enddo
        num_sdi = num_sdi + ln

      ! Fetch MACHINE (PLANT DATA) header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, mac
        write (*, 90000) 9, dev1, dev2, datx, mac
        if (num_sdi + mac .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, mac, num_sdi
          load_sdi = 1
          go to 900
        endif
        i = 0
        count = mac
        do while (count .gt. 0)

      !    Machine data is stored in 100-sdi_data blocks

           num = min (100, count)
           read (lunsdi, end=900) (sdi_data(ii),ii=i+1+num_sdi,
     &                             i+num+num_sdi)
           do ii = i+1+num_sdi, i+num+num_sdi
              if (sdi_data(ii)(1:1) .eq. 'E') then
                 sdi_data(ii)(1:2) = newexctyp(sdi_data(ii)(1:2))
              endif
           enddo
           count = count - num
           i = i + num
        enddo
        num_sdi = num_sdi + mac

      ! Fetch DIRECT CURRENT header and number of sdi_datas
        read (lunsdi, end=900) dev1, dev2, datx, jdc
        write (*, 90000) 10, dev1, dev2, datx, jdc
        if (num_sdi + jdc .gt. max_sdi) then
          write (*, 10000) dev1, dev2, datx, jdc, num_sdi
          load_sdi = 1
          go to 900
        endif
        if (jdc .gt. 0) then
           read (lunsdi, end=900) (sdi_data(ii),ii=1+num_sdi,
     &                             jdc+num_sdi)
        endif
        num_sdi = num_sdi + jdc

        go to 920
  900   last = lastch (sdifile)
        write (*, 910) sdifile(1:last)
  910   format (' E-O-F reading file ', a)
        load_sdi = 1         ! Set return "failure"
  
  920   return
        end
