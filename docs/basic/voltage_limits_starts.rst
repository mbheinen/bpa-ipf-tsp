*******************************************
Global Voltage Limits and Starting Voltages
*******************************************

The following table allows you to find global and starting voltages for a bus given the base kV and zone. These values are hard-coded in the program and cannot be altered by the user.

.. table:: Global Voltage Limits and Starting Voltages

  Base KV From  Range To Zone Restrictions      Global V Limits             Starting V 
                                           V_min (p.u.) V_max (p.u.)   Gen Buses (40%) Load Buses (20%)
  0.1 6.5 0.950 1.052 1.011 1.032
  6.6 6.6 M1 0.950 1.065 1.019 1.042
  6.6 49.9 0.950 1.052 1.011 1.032
  50.0 50.0 16 1.100 1.200 1.160 1.180
  50.0 59.9 0.950 1.052 1.011 1.032
  60.0 60.0 17 0.950 1.100 1.040 1.070
  60.0 62.9 0.950 1.052 1.011 1.032
  63.0 63.0 20 0.930 1.080 1.020 1.050
  63.0 99.9 0.950 1.052 1.011 1.032
  100.0 100.0 16 M5 1.100 1.200 1.160 1.180
  100.0 100.0 M4 0.950 1.070 1.022 1.046
  100.0 114.9 0.950 1.052 1.011 1.032
  115.0 115.0 M4 0.950 1.070 1.022 1.046
  115.0 131.9 0.950 1.052 1.011 1.032
  132.0 161.0 17 20 0.950 1.090 1.034 1.062
  132.0 199.9 0.950 1.052 1.011 1.032
  200.0 200.0 16 1.100 1.200 1.160 1.180
  200.0 229.9 0.950 1.052 1.011 1.032
  230.0 230.0 17 20 0.950 1.070 1.022 1.046
  230.0 499.9 0.950 1.052 1.011 1.032
  500.0 500.0 1.000 1.100 1.060 1.080
  500.1 1099.9 1 0.950 1.052 1.011 1.032
  1100.0 1100.0 1 1.000 1.100 1.060 1.080
  1100.1 9999.9 1 0.950 1.052 1.011 1.032

Notes
=====

  1. Twenty percent (20%) and forty percent (40%) starting voltages are the percentages from :math:`V_{max}` to :math:`V_{min}`:

  .. math::

    V_{start} = V_{max} * (1 - %pct)  + V_{min} * %pct
  
  "0%" starts at :math:`V_{max}`; "100%" starts at :math:`V_{min}`.

  Ideal starting percentages would be a value which lies closest to the final voltages. For peak load cases, a generator percentage of 30% is better; for light loads, 50% is better. The current 40% is a trade-off.

  2. Zone restrictions mean that base kV's are also subject to zones. If the zone restriction list has nonblank entities, only base kV's within the range and zone list zones apply. Conversely, if the zone restriction list is blank, then there are no zone restrictions: all base kV's within the range apply.

  3. To find global and starting voltages for a bus with Base XXX and Zone ZZ, proceed as follows:
     
    a. Find the encompassing base kV:
       
       From_base  <= XXX <= To_base
       
       There will be one or more such ranges. Zone restrictions may qualify the base range.

     b. If zone restrictions apply, check whether zone ZZ qualifies. If true, then the pertinent entity has been located.

     If false, proceed to the next encompassing base kV and repeat step b. All encompassing base ranges terminate with an inclusive blank zone list, which ultimately defaults to "no restrictions".

     
