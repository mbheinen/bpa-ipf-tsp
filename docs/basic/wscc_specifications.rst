*******************
WSCC Specifications
*******************

Functional specifications for Wester Systems Coordinating Council Load Flow and Stability Computer Programs.

General, Load Flow, & Stability
===============================

  A. WSCC will consider an deviations and alternates of these specifications, provided the details of such deviations and alternates are explained by the supplier.

  B. Load Flow and Stability programs shall be compatible. Load flow shall accept card input or tape input from previous cases. Stability program shall accept load flow output and system data directly from load flow history tape of any case. Load flow and stability shall also run "back-to-back" if desired, that is, stability program run as a continuation of load flow without operator intervention to intiate stability run.

  C. Program shall be written ina standard Fortran language for ease of modification except for relocatable Fortran subroutines.

  D. Programs shall be completely documented including full explanation of theory and equations upon which calculations are based as well as the Fortran listings. Complete users manual and program documentation shall be supplied with delivery of programs. A glossary of terms shall be included in the documentation.

  E. Programs shall be written for computer acceptable to the Coucil.

  F. Reduced computer storage capacity shall result in reduced prgoram limits rather than elimination of program features.

  G. Program features shall not be at the expense of excessive input manipulation or machine time.

  H. Diagnostics shall be provided in program operation to reveal cause of program stoppage or failure to converge. Included in diagnostic shall be maximum real and quadrature components of power and voltage change and names of buses on which they occur. Bus mismatch shall be available as diagnostic at user's option. Diagnostics shall be available at each iteration at user's option.



Load Flow Program
=================

  A. System Limits
  
    1. 2000 buses
    2. 3000 branches, 1000 of which may be transformers with fixed or LTC taps.
    3. 50 phase shifters
    4. 60 interchange control areas.
    5. Voltage at 400 buses controlled remotely by generation or transformer LTC.
    6. 90 loss accumulation areas other than interchange areas.
    7. 10 two-terminal and 5 three-terminal DC lines.

  B. Buses

    1. Machines, loads, shunt capacitors, and shunt reactors shall all be representable separately at each bus.
    2. Loads shall be representable as scheduled mw and mvar.
    3. Shunt admittances shall be representable in per-unit as follows:

      (a) Fixed admittance
      (b) Switched capacitor in steps with on and off voltages
      (c) Switched reactor in steps with on and off voltages

    4. Machines shall be represetable as follows:

      (a) Scheduled voltage magnitude and angle (swing).
      (b) Scheduled mw and voltage magnitude, with or without mvar limits. In event mvar limit is reached, voltage schedule is no longer held.
      (c) Scheduled mw and mvar, with or without voltage limits. In event voltage limit is reached, mvar schedule is no longer held.
      (d) Scheduled mw and variable mvar to hold voltage on remote bus. Provision shall be made at 50 voltage controlled buses for the controlling action to be shared in scheduled proportion among the reactive outputs of up to 5 remote machines.

    3. Buses shall be identified by name rather than number. Bus name shall include bus voltage base.

    