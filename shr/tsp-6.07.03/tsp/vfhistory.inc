
      integer MAX_BSIZ
      parameter (MAX_BSIZ = 8000)
      integer MAX_POINTS
      parameter (MAX_POINTS = 8000)
      integer MAX_DIPS
      parameter (MAX_DIPS = 20)
      integer MAX_AREA
      parameter (MAX_AREA = 60)

      integer   mxxbus, ibuss(8,MAX_BSIZ), ptrbus(MAX_BSIZ),
     &          pointer(MAX_POINTS), idip(MAX_POINTS), 
     &          idip_points(3,MAX_DIPS), dip_pop, nextptr, lastptr, 
     &          nvdlor, nfqlor, nvllor, nvdhir, nvdlof, nfqlof, nvllof, 
     &          nvdhif, ibcode, iflagr, nintrvl, iarea(MAX_BSIZ),
     &          sort_index(MAX_BSIZ), sort_key
      real      vdlo, fqlo, vllo, vdhi, vdipld, vdipnl, frqdip,
     &          tstart, tarm, rbuss(9,MAX_BSIZ), 
     &          valmin(4), dip(3,MAX_POINTS), dip_points(3,3,MAX_DIPS) 
      character arcnam(MAX_AREA)*10, busowner(MAX_BSIZ)*3
      logical   load_bus(MAX_BSIZ)
      common /vfhistory/ mxxbus, ibuss, ptrbus, pointer, idip,
     &          idip_points, dip_pop, nextptr, lastptr, nvdlor, nfqlor, 
     &          nvllor, nvdhir, nvdlof, nfqlof, nvllof, nvdhif, vdlo, 
     &          fqlo, vllo, vdhi, vdipld, vdipnl, frqdip, tstart, 
     &          tarm, rbuss, valmin, dip, dip_points, ibcode, iflagr, 
     &          nintrvl, iarea, arcnam, load_bus, sort_index, sort_key,
     &          busowner

