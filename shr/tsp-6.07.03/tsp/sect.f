C    %W% %G%
      subroutine sect (k)
C * * *
C * * * THIS SUBROUTINE FORMS A SINGLE EQUIVALENT PI FOR A LINE
C * * * CONTAINING MULTIPLE PI SECTIONS.  IT IS CALLED BY
C * * * INPUT2.
C * * *
      include 'tspinc/blkcom1.inc'
      include 'tspinc/legspi.inc'

      go to (100,120,140), k
  100 gs1=gij+gj1
      gs2=-gij
      gs4=gij+gi1
      bs1=bij+bj1
      bs2=-bij
      bs4=bij+bi1
      go to 160
  120 gs1=gs1+gij+gi1
      gs3=-gij
      gs5=gij+gj1
      bs1=bs1+bij+bi1
      bs3=-bij
      bs5=bij+bj1
      sq=gs1*gs1+bs1*bs1
      pg=gs1/sq
      pb=-bs1/sq
      a1=pg*gs2-pb*bs2
      a2=pb*gs2+pg*bs2
      gs4=gs4-(a1*gs2-a2*bs2)
      bs4=bs4-(a2*gs2+a1*bs2)
      gs2=-(a1*gs3-a2*bs3)
      bs2=-(a2*gs3+a1*bs3)
      a1=pg*gs3-pb*bs3
      a2=pb*gs3+pg*bs3
      gs1=gs5-(a1*gs3-a2*bs3)
      bs1=bs5-(a2*gs3+a1*bs3)
      go to 160
  140 gi1=gs4+gs2
      bi1=bs4+bs2
      gj1=gs1+gs2
      bj1=bs1+bs2
      gij=-gs2
      bij=-bs2
  160 return
      end
