****************
Typical Examples
****************

Introduction
============
Some typical power system design application examples are given in this section to demonstrate possible applications of the core power flow programs. Each example indicates possible uses of the program relative to some stated network planning objectives.

Setting Up a Network Data File
==============================
Use the following values for this four-bus network:

.. figure:: ../img/Four_Bus_Network.png

   Four Bus Network

**Line Data**

  * Use 100 MVA, 500 kV base.
  * X = .525 ohm/mile, X/R = 18.
  * B/2   = .01024 pu/mile, where B is the total line charging and B/2 is equal to :math:`B_{pi}`, if long-line effect is ignored.
  * Current Rating is 3000 amps.

**Transformer Data**

  * X   =  20% (0.20 pu) on 525 kV, 1770 MVA base.
  * Transformer Taps  =  22 kV, 525 kV.

**Generator Data for Power Flow**

  * P(max)  =  1770MW, P(g) = 1680 MW.
  * Q(lim) =  +/- 521MVAR.
  * Type G bus controlling voltage of GEN HI to 1.08 pu.

**Station Service Load**

  * 190 +  j95 (constant power for stability).

**Approach**
The preparation of the network data will be presented in the following steps:

1. Prepare the bus data group.
   
    Define the following buses:

    .. code::
  
      GEN 22 
      GEN HI 500 
      INF BUS 
      500 MID 500
 
    a. Data for GEN 22

    .. code::
  
      Record Type:     B 
      Sub Type:        G 
      Change Code:     blank 
      Owner:           blank (not specified) 
      Name:            GEN 
      kV:              22 
      Zone:            blank (not specified) 
      Load P MW:       190 
      Load Q MVAR:     95
      Shunt Load MW:   blank (or zero) 
      Shunt Load MVAR: blank (or zero) 
      P MAX:           1770 
      P GEN MW:        1680 
      Q MAX MVAR:      521 
      Q MIN MVAR:      -521 
      V MAX PU:        blank (not specified) 
      V MIN PU:        blank (not specified) 
      Remote Bus Name: GEN HI 
      Remote Bus kV:   500 
      % VARS Supplied: blank (defaults to 100)

New Facilities
==============
The purpose of this example is to provide new generating facilities to serve the growing loads near the town of Keller and Mount Tolman.

Proposed New Facility
---------------------
The proposed facility includes a new substation at NEW SUB, a 21-mile 230 kV line tapped from the A SUB - B SUB No. 2 line and another line to D SUB tapped from A SUB - NEW SUB line but which will be built and owned by another utility. The equipment at NEW SUB substation will include a 25 MVA transformer with protective equipment serving a 34.5 kV load. In the diagram on the following page, starred or dotted lines are the proposed facility while the dashed lines indicate existing facility. This proposed facility may be one among other possible alternatives. Each alternative, and modifications thereof, will be considered a case and submitted to the power flow program for analysis and report.

Possible Uses of IPF
--------------------
IPF will be used to study the power flows, voltage regulation and reliability of service utilizing the proposed facility.

.. figure:: ../img/A-SUB-New-SUB-230-kV-Line

   A-SUB - New SUB 230 kV Line

The processes ``(POWERFLOW)`` and ``(OUTAGE_SIM)`` will be used to check power flow as well as reliability. Refer to :ref:`power-flow-control-language`, for complete description of these processes.

Suppose the base network which the proposed new facility will amend has been described in an old base file named ``BASENET.BSE``. The following program control file can be built for this case:

.. code::

  (POWERFLOW, CASEID = EXAMPLE1, PROJECT=KELLER-SUB)
  /OLDBASE FILE = BASENET.BSE\ 
  /NEWBASE = EXAMPLE1.CAS\ 
  .... 
  ....  Optional Network Solution Qualifiers Listed Under 
  ....                  (POWERFLOW) 
  /CHANGES 
  B - record for NEW SUB 34.5 
  B - record for NEW SUB 230 
  B - record for NEW SUB Tap 230 
  B - record for D SUB 230 
  B - record for A SUB TAP 230 
  L - record to delete A SUB 230 B SUB 230  2 
  L - record for A SUB 230 A SUB Tap 230 
  L - record for A SUB Tap 230 B SUB 230 
  T - record for NEW SUB 230 NEW SUB 34.5 
  . The above record formats are described 
  . Under appropriate Heading starting with the key letter 
  (POWERFLOW, CASEID=EXAMPLE1, PROJECT=NEW-SUB) 
  /OLDBASE FILE=EXAMPLE1.CAS 
  /OUTAGE_SIM 
  .... 
  ....  Optional Qualifiers Listed Under /OUTAGE_SIM 
  .... 
  (STOP)

The output listings from this case will be inspected to make sure no overloads occur during normal operation as well as during the outages of key lines.

Special environmental concerns may suggest evaluation of alternatives to the river crossing indicated in the exhibit. Conductor sizing may also be influenced by energy conservation (loss-reduction) considerations. The effect of each alternative can be determined from the network solution output listing.

In this example, the major point to monitor is the impact of tapping lines since segments of a tapped line perform differently from the untapped line.

Reconductoring
==============