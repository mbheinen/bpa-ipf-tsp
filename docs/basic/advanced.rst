*****************
Advanced Features
*****************

Network Reduction
=================
This section gives a detailed description of the network reduction method and solution options.

Method of Reduction
-------------------
This program reduces a large sparse network into a smaller equivalent network by Gaussian decomposition. The original network is linearized about the operating point and is expressed by the current equation:

.. _`eq-1`:
.. math::

  \begin{bmatrix} I_1 \\ I_2 \end{bmatrix} = \begin{bmatrix} Y_{11} & Y_{12} \\ Y_{21} & Y_{22} \end{bmatrix} \begin{bmatrix} V_1 \\ V_2 \end{bmatrix}

where: :math:`I1` is the complex current net injection matrix, :math:`Y` the complex nodal admittance matrix, and :math:`V` the complex nodal voltage matrix. The subscripts 1 and 2 pertain to the eliminated and retained portions of the network, respectively. For simplicity, the matrix is reordered as shown. We may separate the partitioned matrix equations in `eq_1`.

.. _`eq-2`:
.. math::

  \begin{bmatrix} I_1 \end{bmatrix} &= \begin{bmatrix} Y_{11} \end{bmatrix} \begin{bmatrix} V_1 \end{bmatrix} + \begin{bmatrix} Y_{12} \end{bmatrix} \begin{bmatrix} V_2 \end{bmatrix} \\
  \begin{bmatrix} I_2 \end{bmatrix} &= \begin{bmatrix} Y_{21} \end{bmatrix} \begin{bmatrix} V_1 \end{bmatrix} + \begin{bmatrix} Y_{22} \end{bmatrix} \begin{bmatrix} V_2 \end{bmatrix}


We may solve for :math:``from :ref:`eq-2`.

  \begin{bmatrix} V_2 \end{bmatrix} &= \begin{bmatrix} Y_{21} \end{bmatrix} \begin{bmatrix} V_1 \end{bmatrix} + \begin{bmatrix} Y_{22} \end{bmatrix} \begin{bmatrix} V_2 \end{bmatrix}