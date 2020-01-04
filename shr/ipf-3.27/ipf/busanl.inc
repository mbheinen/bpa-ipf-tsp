
      parameter   ( MAXCMP = 200 )    !   lines with series compensation
      parameter   ( MAXLOD = 200 )    !   load breakdowns in analysis
      parameter   ( MAXOLB = 400 )    !   no. of overloaded branches
      parameter   ( MAXTRN = 100 )    !   transformers to analyze
      parameter   ( MAXOVL = 400 )    !   own-volt gen/load losses

      common /busanl/ jowner, jvolt, jyear, nba, ncomp, ncon, ndcln,
     1                nolbr, noltr, novls, ntran, abus(17,MAXBUS),
     2                adc(14,MAXDCL), aload(20,MAXLOD), comp(5,MAXCMP),
     3                kov(MAXVLT,MAXOWN), olbr(6,MAXOLB),
     4                oltr(4,MAXOLT), ovlos(8,MAXOVL), syst(10,MAXOWN),
     5                trsum(11,MAXTRN), zonsrt(MAXZON), zsum(26,MAXZON),
     6                eff(13,MAXBRN), noleff, txeff(9,2*MAXOLT),
     7                ntxeff, syst_used(2,MAXOWN)
      dimension kabus(17,MAXBUS), ladc(14,MAXDCL), komp(5,MAXCMP),
     1          load(20,MAXLOD), kolbr(6,MAXOLB), koltr(4,MAXOLT),
     2          ktrsum(11,MAXTRN), leff(13,MAXBRN),
     3          ltxeff(9,2*MAXOLT)
      equivalence (abus,kabus), (adc,ladc), (comp,komp), (aload,load),
     1            (olbr,kolbr), (oltr,koltr), (trsum,ktrsum),
     2            (eff,leff), (txeff,ltxeff)

