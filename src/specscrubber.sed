# This is adapted from the sed one-liner document at http://sed.sourceforge.net/sed1line.txt
:a
1,/(\*CUT BEFORE\*)/d
# Fix up the comments that shield the spec parts from compilation
/(\*SPECSTART\|SPECEND\*)/d

# Let out the spec separator
s/(\*%%\*)/%%/

#Remove multiline comments
s/(\*.*\*)//g
/(\*/N
//ba
