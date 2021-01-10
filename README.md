EGSnrcTools
===========

Here are various tools relating to the use of the EGSnrc Monte Carlo system for radiation transport simulations and dosimetry.

1. Linux script (bash) for running the code in parallel.

2. Linux script (bash) for doing sensitivity analysis: one or more parameters (such as the beam energy) are changed and 
new egsinp-files are created based on a template. The script runs the code, and the main output can be extracted from the 
egslst files. The key results of the entire sensitivity analysis (input, parameters, comuting times, and out) 
are put into a log-file. An example is included. This is the BROM use case where the shielding effect of a lead
or Wolfram container is computed for several gamma energies (including those of Br-82).

3. R-script for anayzing egsgph-files output from EGSnrc RZ user codes with IWATCH = graph (4). See pdf file for details.
The egsgph files includes detailed information about particle positions, particle type, and energy. It is shown how the
data can be used for computation of the projected range of electrons.
