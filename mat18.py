##/usr/bin/env python3.6.8
# -*- coding: utf-8 -*-
"""
Created on Sun Jun  9 12:46:51 2019

@author: Al Harral
"""
from math import sqrt,log,exp

print('Power Law Plasticity')
print('English Units 6/7/2010')
print('http://www.VarmintAl.com/aengr.htm')
print('English Units (lbf-s2/in, in, s, lbf, psi, lbf-in)')
print('Output formated for LS-DYNA')
print('Written in Python 3.6.8')

# Temp input below so I don't have to keep typint it in

#a = raw_input('Material Name........................ ')
# Use for Python 2.7.15+

a = input('Material Name........................ ')     
rho = eval(input('Density (lb/cu-in)................... '))     
e = eval(input('Elastic Modulus (psi)................ '))      
nu = eval(input('Poisson Ratio........................ '))      
sigmay = eval(input('Engineering Yield Stress (psi)....... '))      
sigmau = eval(input('Engineering Ultimate Stress (psi).... '))     
elong = eval(input('Elongnation (%)...................... '))           
ra = eval(input('Reduction in Area  (%)............... '))     
yoff = eval(input('Yield Offset (%)..................... '))

"""       
a='mag 231'
rho=.065         # Density
e=6400000.      # Elastic Modulus
nu=.35           # Poisson Ratio
sigmay=29000.    # Yield Stress at Offset
sigmau=37000.    # Ultimate Stress necking starts
elong=8.0       # Elongnation at Failure
ra=0.           # Reduction is Crossection at Failure
yoff=.2          # Offset Parallel to Elastic Range
"""
#                s0=54943.     K value check
#                am=0.0506919  M value check
#     mb         m for brittle metal - elong < 9%
#     md          m for ductile metal
# tstrainy       True Yield Strain
# tstressy       True Yield Stress
# tsrtanu        Ture Ultimate Strain
# tstress        True Ultimate Stress
# mb             Strain Hardening Exponent Brittle Metal
# md             Strain Hardening Exponent Ductile Metal
z=0.0            # Zero if needed
edummy=30.
sfail = 0        # No Failure Strain Unknown

print(a)
print('Density (lb/cu-in)................... ','{0:.4f}'.format(rho))
print('Elastic Modulus (psi)................ ','{0:.1f}'.format(e))
print('Poisson Ratio........................ ','{0:.4f}'.format(nu))
print('Engineering Yield Stress (psi)....... ','{0:.4f}'.format(sigmay))
print('Engineering Ultimate Stress (psi).... ','{0:.4f}'.format(sigmau))
print('Elongnation (%)...................... ','{0:.4f}'.format(elong))
print('Reduction in Area  (%)............... ','{0:.4f}'.format(ra))
print('Yield Offset (%)..................... ','{0:.4f}'.format(yoff))

# .......... input #1, rho, e, nu, sigmay, sigmau, elong, ra, cte, yoff, sfail
# .......... calculate strain hardening equation       

if yoff <= 0: # Cannot have yoff a zero value
    yoff = .2 # Set to standard 0.2% standard offset
g = e / (2.0 * (1.0 + nu))          # Shear Modulus
k = e / (3.0 * (1.0 - 2.0 * nu))    # Bulk Modulus
tstrainy = log(1.0 + yoff / 100.0 + sigmay / e)
tstressy = sigmay * (1.0 + yoff / 100.0 + sigmay / e)      

# ..... assume 30% elongation if not given for first guess of m
edummy = elong
if elong < 0.01: 
    edummy = 30.

    # edummy is used for the first guess
tstrainu = log(1.0 + elong / 100.0)      
tstressu = sigmau * (1.0 + tstrainu)
mtop = log(tstressu / tstressy)
mbot = log(tstrainu / tstrainy)
mb =  mtop/mbot    # Exponent for Brittle Metal
m = 1.0
print('tstressu, tstressy, d ', m, mtop, mbot, elong, edummy)

# .......... use newtons method to solve for m if elong>9%

testm = 100.
while testm >= 0.000000015:
    param1 = m / (2.718282 * tstrainy)
    fofm = param1**m - sigmau / tstressy
    fpofm = param1**m * (1.0 + log(param1))
    m2 = m - fofm / fpofm
    testm = (m2 - m)**2
    m = m2
# found m 
md = m       # Exponent for Ductile Metal

sigma0 = tstressy / tstrainy**m
strainy = 1.0 / (e/ sigma0) ** (1.0 / (1.0 - m))
stressy = e * strainy
if (elong>9.0) or (ra>10.0):   # Ductile Metal find min strainf
    sra = log(100 / (100 - ra))
    srelong = log(1.0 + elong / 100.0)
    strainfd = sra
    if (srelong > sra):
        strainfd = srelong

#  ===================== good to here

if (ra < 0.5) and (elong < 9.): # a Brittle Metal
    m = mb
    sigma0 = tstressy / tstrainy**m
    strainf = log(1.0 + elong / 100.0)
    stressf = sigma0 * strainf**m
    strainut = m
    stressut = sigma0 * strainut**m
else:
    m = md                       # Ductile Metal
    strainf = strainfd
    strainf = log(100 / (100 - ra))
    stressf = sigma0 * strainf**m
    strainut = strainf
    stressut = sigma0 * strainf**m

print('The results are formated for the LS-DYNA key word input file.')
print(' ')
print ('$ ========== Formatted for LS-DYNA key word file ===================')
print ('*MAT_POWER_LAW_PLASTICITY')
print ('$')
print(('$  '),a)
print ('$  ')
print ('$             Power Law Plasticity (MAT_018)')
print ('$       English units (lbf-s^2/in, in, s, lbf, psi, lbf-in)')
print ('$  ')
print(('$  Material Density........... lb/in^3    {0:7.4f}'.format(rho)))
print(('$  Youngs Modulus................. psi     {0:.5e}'.format(e)))
print(('$  Shear Modulus...................psi     {0:.5e}'.format(g)))
print(('$  Bulk Modulus ...................psi     {0:.5e}'.format(k)))
print(('$  Poisson Ratio......................    {0:7.4f}'.format(nu)))
print(('$  Yield Stress at Offset..........psi {0:7.4f}'.format(sigmay)))
print(('$  Engineering Ultimate Stress.....psi {0:7.4f}'.format(sigmau)))
print(('$  Elongation at Failure.............%    {0:7.4f}'.format(elong)))
print(('$  Reduction in Area.................%    {0:7.4f}'.format(ra)))
print(('$  Yield Offset .....................%    {0:7.4f}'.format(yoff)))
print ('$  ------------------ calculated values -----------------------')
print ('$  Strain Hardening Equation       s = s0 * e^m')
print(('$  Equation Constants             s0 = {0:6.4e}' '     m = {1:6.6f}'.format(sigma0,m)))
#write (*,'(a,f10.2,a,f10.6)') '$  yield point                    sy = ',stressy,'  ey = ', strainy
print(('$  Yield Point                    sy = {0:6.4e}' '    ey = {1:6.6f}'.format(stressy,strainy)))
#write (*,'(a,f10.2,a,f10.6)') '$  ultimate (engineering)         su = ',sigmau, '  eu = ', exp(m) - 1!
print(('$  Ultimate (Engineering)         su = {0:6.4e}' '    eu = {1:6.6f}'.format(sigmau,(exp(m)-1))))
#write (*,'(a,f10.2,a,f10.6)') '$  ultimate stress/total strain  sut = ',stressut,' eut = ', strainut
if ra < 1.0:
    print(('$  Ultimate Stress/Total Strain  sut = {0:6.4e}' '   eut = {1:6.6f}'.format(stressut,strainut)))
#       write (*,'(a,f10.6)') '$  effective plastic failure strain              epfs = ', sfail

print('$      mid       rho         e        pr         k         n       src       srp')
print('$          lbf-s2/in       psi                 psi')
#write (*,'(a,1pe10.3,1pe10.3,0pf10.4,f10.1,f10.7)') '        xx', rho/386.09, e, nu, sigma0, m
print(('       xx {0:5.8f} {1:0.3e} {2:5.7f} {3:0.3e} {4:5.7f}'.format(rho/386.09,e,nu,sigma0,m)))
#            1 7.3240e-4 3.0000e+7  0.290000 1.2811e+5  0.086821
print('$#    sigy        vp      epfs')
#write (*,'(3f10.2)') z, z, sfail
print ('    0.0000    0.0000    0.0000')
print ('$ ========Replace xx above with the local material I.D. number======')

#print 'Error. Could not find (m) in s = s0 * e^m. Check the input.'     

print(' ')
print('Calculation Completed')
print(' ')

     






            
