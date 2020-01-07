C
      common /prtsw/ lprtsw,fichsw,crtsw
      integer lprtsw,fichsw,crtsw
      common /prtbuf/ outbuf,errbuf(10)
      character outbuf*133,errbuf*120
C
