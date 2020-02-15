# metal-properties
These two programs calculate the material properties for ductile metals using the Power Law Plasticity (MAT_018) for the LS-DYNA FEA code.
One is for Python and the other is for Fortran 95. The material.txt database is over 1000 raw material properties of metals.
There is more info here: http://www.varmintal.com/aengr.htm#Mats-for-LS-DYNA

The .BAS files were the original codes written in Microsoft Basic in 2010. Then they were ported over to python and fortran.

GASB.BAS  This code uses the Beattie-Bridgeman equaiton of state
to calculate the pressure, volume, and mass, given
two of these and the temperature.  The units are:

 Pressure...(psi) & (pascal)"
 Volume.............(liters)"
 Mass................(grams)": PRINT

Good Hunting from.... Varmint Al
