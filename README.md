EGSnrcTools
===========

Here are various tools relating to the use of the EGSnrc Monte Carlo system for radiation transport simulations and dosimetry.

1. egs-parallel2. Linux script (bash) for running the EGSnrc user codes in parallel on a multi-core computer. 
This script is no longer needed as such functionality has been built into EGSnrc in the 2021 version.
The script was developed for the RZ user codes only. It uses the IRESTART parameter.

Now one can run EGSnrc in parallel simply using (here N = 12 cores):

egs-parallel -f -q short -n12 -v -c "dosrznrc -i inputfilename.egsinp -p 521icru.pegs4dat" 


3. egs-sensitivity2. Linux script (bash) for doing sensitivity analysis: one or more parameters (such as the beam energy) are changed and 
new egsinp-files are created based on a template. The script runs the code, and the main output can be extracted from the 
egslst files. The key results of the entire sensitivity analysis (input, parameters, computing times, and EGRnrc output) 
are put into a single log-file. An example is included. This is the BROM use case where the shielding effect of a lead
or Wolfram container is computed for several gamma energies (including those of the radioactive isotope Br-82).

4. IWATCH-reader. R-script for anayzing egsgph-files output from EGSnrc RZ user codes with IWATCH = graph (4). See pdf file for details.
The egsgph files includes detailed information about particle positions, particle type, and energy. It is shown show the
data can be used for computation of the projected range of electrons.
