C    %W% %G%
      subroutine rcinp
C * * *
C * * * THIS SUBROUTINE DECODES THE RC RESISTANCE BRAKE CARD
C * * * AND FORMS INITIAL DATA TABLES.  IT IS CALLED BY INPUT1.
C * * *
      include 'tspinc/params.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/rbcom.inc'
      include 'tspinc/brakn.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/reread.inc'
      character*8 bus4c,bus5c,bus6c
      character*1 iparc,id6
      character*5 finsc
C     -     begin     begin     begin     begin     begin     begin
      if (irb .eq. 0) return
 1000 read (buffer,1200) bus4c,base4,bus5c,base5,iparc,bus6c,base6,
     1  id6,consa,consb,consc,ttrip2,fdisc,fins,tmax,pbias2
 1200 format (bz,6x,a8,f4.0,1x,a8,f4.0,a1,1x,a8,f4.0,a1,4f4.2,f5.4,f5.3,
     1  f4.1,f4.0)
C * * *
C * * *  DATA CHECKING ON BRAKE RC CARD
C * * *
      if (ttrip2.le.0.0) ttrip2=0.01
      if (tmax.le.0.0) tmax=0.01
      if (consc.eq.0.0) go to 1800
      if (consa.eq.0.0) go to 2000
      if(fins.ne.0.0) go to 1600
C * * *
C * * *   IF FINS IS BLANK SET IT TO A LARGE NUMBER
C * * *
      read (buffer,1400) finsc
 1400 format (bz,62x,a5)
      if (finsc.eq.' ') fins=1.0e10
 1600 if (fins.le.fdisc) go to 2000
      go to 2400
 1800 if (consb.eq.0.0) go to 2000
      go to 2400
 2000 iabort=1
      write (errbuf(1),2200) bus6c,base6,id6
      call prterr ('E',1)
 2200 format (1h0,'  CHECK RC CARD AT ',a8,2x,f5.1,2x,a1,'--M, T2 ',
     1        'MUST BE POSITIVE AND FDISC LESS THAN FINS')
 2400 kb4=nambas(base4)
      kb5=nambas(base5)
      kb6=nambas(base6)
      ibus2=inam(bus4c,kb4)
      jbus2=inam(bus5c,kb5)
      ibusb=inam(bus6c,kb6)
      if (ibus2.eq.0.or.jbus2.eq.0.or.ibusb.eq.0)then
         write (errbuf(1),2600) bus6c,base6,id6
         call prterr ('E',1)
 2600    format (1h0,'RC - BUS6 ',a8,f5.1,1x,a1)
         iabort = 1
         return
      endif
      if(ibkbno(irb) .ne. ibusb .or. ibkid(irb) .ne. id6)then
         write (errbuf(1),2800)
         call prterr ('E',1)
 2800    format (1h0,'THE BUS BRAKE IDENTIFICATION ON THE PREVIOUS ',
     1          'TWO CARDS DO NOT AGREE.')
         iabort = 1
         return
      endif
      irc=irc+1
      if (iparc.eq.'0') iparc = ' '                                      !dem
      ircidc(irb)=iparc
      ircidn(1,irb)=ibus2
      ircidn(2,irb)=jbus2
      ircidn(3,irb)=irb
      ibkib2(irb) = ibus2
      ibkjb2(irb) = jbus2
      bkcona(irb) = consa
      bkconb(irb) = consb
      bkconc(irb) = consc
      bkpbi2(irb) = pbias2
      bktrg2(irb) = fdisc
      bktrp2(irb) = ttrip2
      bkfins(irb) = fins
      bktblk(irb) = tmax
      ibrkc(irb) = 1
      return
      end
