
      common /outpt2/ v(2), y(2,2), a(2), s(2), 		!complex * 16
     &		      pin, qin, pout, qout, ploss, qloss,	!r * 8
     & 		      ain, bin, aout, bout, pintot, qintot,	!r * 8
     &		      poutot, qoutot, pltot, qltot, x, 		!r * 8
     &		      ztot(10), pvlv, qvlv,			!r * 8
     &                nbsec, nbpar, ktybr, k1, k2, isect,	!i * 4
     &		      nxtype, nxk1, nxk2, nxsect, nxtran, 	!i * 4
     &		      kerrsw, kanal, ksect, kparl, kauto,	!i * 4
     &		      kareas, iprint, iznsw, koutsw, nzone,	!i * 4
     &		      intor					!i * 4
c
      common/outpt3/  pgen, pload, ploadm, pnetmw, qgen, qload,	!r * 8
     &		      qloadm, qnetmw, qmin, qmax, skcond,	!r * 8 
     &		      skreak, skcap, qunsk, qreak, qcap,	!r * 8
     &		      vmin, vmax, pmax, voltpu, pload2, qload2,	!r * 8
     &		      skcon2, skcap2, skrek2, pnetec, qnetec,	!r * 8
     &		      pgen2, qgen2, gequiv, bequiv, pgenmw,	!r * 8
     &		      qgenmw,					!r * 8
     &		      bsvolt, voltkv, degree,		 	!r * 4
     &		      kcb, ktype, itype, nb, jtbx, kbus, 	!i * 4
     &		      ltype, idc, jdc				!i * 4
c
      common/outpt4/trx1, trx2,  	   			!r * 8
     &		    tmax, tmin, taps, skmin, skmax, tuse,	!r * 4
     &		    rating, amiles, pctol, amag, vamag, tang,  	!r * 4
     &		    vapct, arate,  base1, base2, pwrfk, 	!r * 4
     &		    list, iparl, jtx, jtran			!i * 4
c
      common /ratnom/ ratnom					!r * 4
c
      common/ot234c / id, kowner, nxid, bsname, zname, kode,	!character
     &		      type, atrx1, atrx2, ovld, ibsown		!character
			
      equivalence (miles,amiles)
c
      complex * 16 v, y, a, s                                   !complex * 16
c
      double precision pin, qin, pout, qout, ploss, qloss,      !r * 8
     &                ain, bin, aout, bout, pintot, qintot,     !r * 8
     &                poutot, qoutot, pltot, qltot, x,          !r * 8
     &                ztot, pvlv, qvlv,                     	!r * 8
     &                pgen, pload, ploadm, pnetmw, qgen, qload, !r * 8
     &                qloadm, qnetmw, qmin, qmax, skcond,       !r * 8
     &                skreak, skcap, qunsk, qreak, qcap,        !r * 8
     &		      vmin, vmax, pmax, voltpu, pload2, qload2,	!r * 8
     &		      skcon2, skcap2, skrek2, pnetec, qnetec,	!r * 8
     &		      pgen2, qgen2, gequiv, bequiv, pgenmw,	!r * 8
     &		      qgenmw, trx1, trx2			!r * 8

      real	    bsvolt, voltkv, degree,		 	!r * 4
     &		    tmax, tmin, taps, skmin, skmax, tuse,	!r * 4
     &		    rating, amiles, pctol, amag, vamag, tang,  	!r * 4
     &		    vapct, arate,  base1, base2, pwrfk, 	!r * 4
     &		    ratnom					!r * 4
c
      integer	      nbsec, nbpar, ktybr, k1, k2, isect,	!i * 4
     &		      nxtype, nxk1, nxk2, nxsect, nxtran, 	!i * 4
     &		      kerrsw, kanal, ksect, kparl, kauto,	!i * 4
     &		      kareas, iprint, iznsw, koutsw, nzone,	!i * 4
     &		      intor,					!i * 4
     &	              kcb, ktype, itype, nb, jtbx, kbus, 	!i * 4
     &		      ltype, idc, jdc,				!i * 4
     &	              list, iparl, jtx, jtran			!i * 4
c
       character ibsown*3, kowner*3, id*1, kode*1, nxid*1, 	!char
     &           bsname*8, zname*2, type*1, atrx1*4, atrx2*4, 	!char
     &		 ovld*8	                    			!char
c
