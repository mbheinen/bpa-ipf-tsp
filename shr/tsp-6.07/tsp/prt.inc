C    @(#)prt.inc	4.3 9/21/93

      common /prtcomt/ lprtsw, fichsw, crtsw, lprt, mfich, dbug
      integer lprtsw, fichsw, crtsw, lprt, mfich, dbug
      common /prtbuft/ rdate, outbuf, errbuf(10), header, coment(2),
     &                subhed(5), repnam
      character *133 outbuf, coment, subhed
      character *120 errbuf
      character rdate*9, header*50, repnam*60

