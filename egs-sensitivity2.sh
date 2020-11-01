#!/bin/bash
# Created: July 30, 2014
# Revised: July 30, 2014
# Revised: October 31, 2020
# Revised: November 1, 2020
# Name   : Claus E. Andersen

# This script if for bash / linux.

# Objective: To run a sensitivity analysis using EGSnrc.
# The code is targeted for analysis for  a multi-core environment.

# Procedure: 
# 1. Create a template egsinp file where the parameters to be studied
# are marked as, for example, xxMeV and xxHistories.

# 2. Modify the script such as to loop over the parameter space
# of interest (for example, testing different energies as done
# below using xxMeV equal to 0.5, 0.6 etc.).

# 2. Produce new egsinp files using the naming scheme that includes
# a sequential run number (for example, 1100, 1101, 1101 etc.).
# The start number can be defined using the variable called "start").


# This is an improved sensitivity script that includes:
#  1. Creation of a junk-folder
#  2. Movement of all files with w1 or w 22 etc in their names (such
#     files are normally from multi core runs).
#  3. Extraction of output from the lst-file (i.e. the main EGSnrc output)
#     and output to the sensitivity log-file.
#  4. Improved naming, where the extension of egsinp and egslst files
#     are now hardwired into the code (and not included in the postfix as
#     done previously). 

echo "Hello and welcome (egs-sensitivity2.sh)"

# User parameters:
template="Brom1002-template-0001.egsinp"
prefix="BromNoPb-"
start=1100
postfix=""
logfile="BromNoPb-sensitivity-log-file.txt"
junkfolder="junkfolder"
cores=60

xxHistories=100000000

if [ ! -e "$junkfolder" ]; then
      echo "Create a junk folder for temp. files it does not alæready exist."
      mkdir "$junkfolder"
fi

if [ -e "$logfile" ]; then
      echo "Remove old log file, if it exists."
      rm "$logfile"
fi
touch  "$logfile"
# ###################################################
no=$start
echo "The template is: .... $template" 

# Make a time stamp
time1=$( date +%s )   
for xxMeV in 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0 2.5 3.0
do
  time10=$( date +%s )   
  filename="${prefix}${no}${postfix}.egsinp"
  outfilename="${prefix}${no}${postfix}.egslst"
  cp $template $filename
  echo "The energy is $xxMeV and the filename is $filename"
  echo "--------------------------------------------------------------------" >> $logfile   
  echo "File name = $filename" >> $logfile   
  echo "MeV = $xxMeV" >> $logfile   
  sed -i 's/xxMeV/'$xxMeV'/' "${filename}" 
  sed -i 's/xxHistories/'$xxHistories'/' "${filename}" 

  # This is where EGSnrc is called (modify as needed):
  ./egs-parallel2.sh "${cores}" dosrznrc -i "${filename}" -p 521icru.pegs4dat

  time20=$( date +%s )
  timedelta=$(( $time20 - $time10 ))
  echo "Time for this job= $timedelta s" >> $logfile

  
  # Extract main data regarding IRL19 (-Ax and -Bx means x line before and after the match).
  # (modify as needed)
  grep -hnr  -A2 -B1 "MAX # OF HISTORIES TO RUN" $outfilename >> $logfile
  grep -hnr  -A2 -B1 "INCIDENT KINETIC ENERGY" $outfilename >> $logfile
  grep -hnr  -A6 -B1 "SOURCE PARAMETERS" $outfilename >> $logfile
  grep -hnr  -A3 -B1 "IRL 19" $outfilename >> $logfile

 
  # Move stuff to junkfolder
  mv *w[0-9]* $junkfolder
  mv *w[0-9][0-9]* $junkfolder
  echo "Moved temp. files *w1* and *w10* etc. to the junkfolder: $junkfolder"

  ((no +=1))
  
done

time2=$( date +%s )
timedelta=$(( $time2 - $time1 ))
cat "$logfile"
echo "Time used for main script execution: $timedelta s"
echo "Number of histories: $xxHistories"


echo "ByeBye (egs-sensitivity2.sh)"

