REM PHMAT18.BAS PH Units 6/7/2010
REM PH Units (gram, cm, microsec, 1e+07 N, Mbar, 1e+07 N-cm)
REM
OPEN "DATA1044.txt" FOR INPUT AS #1
OPEN "PMAT18Z.txt" FOR OUTPUT AS #2
PRINT #2, "Power Law Plasticity"
PRINT #2, "PMAT18Z.BAS Physics Units 7/7/2010"
PRINT #2, "http://www.VarmintAl.com/aengr.htm"
PRINT #2, "Physics Units: gram, cm, microsec, 1e+07 N, Mbar, 1e+07 N-cm"
PRINT #2, ""
cc1 = 27.6799
cc2 = 6.894757E-08

FOR i = 1 TO 1044

INPUT #1, IMAT, A$

INPUT #1, RHO, E, NU, SIGMAY, SIGMAU, ELONG, RA, CTE, YOFF, SFAIL

REM Calculate strain hardening equation
4020 REM
4030 IF YOFF = 0 THEN YOFF = .2
4040 G = E / (2! * (1! + NU))
4050 K = E / (3! * (1! - 2! * NU))
4060 TSTRAINY = LOG(1 + YOFF / 100 + SIGMAY / E)
4070 TSTRESSY = SIGMAY * (1 + YOFF / 100 + SIGMAY / E)
4080 REM
4090 REM Assume 30% elongation if not given for first guess of m
4100 REM
4110 IF ELONG <= 0! THEN EDUMMY = 30 ELSE EDUMMY = ELONG
4120 TSTRAINU = LOG(1 + EDUMMY / 100)
4130 TSTRESSU = SIGMAU * (1 + TSTRAINU)
4140 M = LOG(TSTRESSU / TSTRESSY) / LOG(TSTRAINU / TSTRAINY)
4150 REM
4160 REM Use NewtonREM s method to solve for m if elong>9%
4170 REM
4180 IF RA = 0 AND (ELONG > 0 AND ELONG < 9!) THEN 4240
4190   PARAM1 = M / (EXP(1) * TSTRAINY)
4200 FOFM = PARAM1 ^ M - SIGMAU / TSTRESSY
4210 FPOFM = PARAM1 ^ M * (1 + LOG(PARAM1))
4220 M = M - FOFM / FPOFM
4230 IF ABS((FOFM / FPOFM) / M) > .000001 THEN 4190
4240   SIGMA0 = TSTRESSY / TSTRAINY ^ M
4250 STRAINY = 1 / (E / SIGMA0) ^ (1 / (1 - M))
4260 STRESSY = E * STRAINY


4270 IF RA = 0 THEN 4450
4280 '
4290 ' Check if failure strain < ultimate strain only for RA<>0
4300 '
4310 STRAINF = LOG(100 / (100 - RA))
4320 IF M < 1.01 * STRAINF THEN 4460

4450 IF ELONG > 0 AND ELONG < 9! THEN STRAINF = LOG(1 + ELONG / 100!) ELSE STRAINF = 0
4460 STRESSF = SIGMA0 * STRAINF ^ M
4470 IF RA <> 0 OR (ELONG = 0 OR ELONG > 9) THEN STRAINUT = M ELSE STRAINUT = STRAINF
4480 STRESSUT = SIGMA0 * STRAINUT ^ M

PRINT #2, "*MAT_POWER_LAW_PLASTICITY"
5050 PRINT #2, "$  "
5051 PRINT #2, "$  " + A$
5052 PRINT #2, "$  "
5053 PRINT #2, USING "$  ####       Power Law Plasticity (MAT__018)"; IMAT
5054 PRINT #2, "$        PH Units (gram, cm, microsec, 1e+07 N, Mbar, 1e+07 N-cm)"

5070 PRINT #2, USING "_$   Material density . . . . . .     ##.####^^^^ gm/cm^3"; RHO * cc1

5090 PRINT #2, USING "_$   Young's Modulus. . . . . . .     ##.####^^^^ Mbar"; E * cc2
5100 PRINT #2, USING "_$   Shear Modulus. . . . . . . .     ##.####^^^^ Mbar"; G * cc2
5110 PRINT #2, USING "_$   Bulk Modulus . . . . . . . .     ##.####^^^^ Mbar"; K * cc2
5120 PRINT #2, USING "_$   Poisson's ratio. . . . . . .      #.####    "; NU
5130 PRINT #2, USING "_$   Yield stress at offset . . .     ##.####^^^^ Mbar"; SIGMAY * cc2
5140 PRINT #2, USING "_$   Engineering ultimate stress.     ##.####^^^^ Mbar"; SIGMAU * cc2
5150 IF ELONG <> 0 THEN PRINT #2, USING "_$   Elongation at failure. . . .    ###.###      %"; ELONG
5160 IF RA <> 0 THEN PRINT #2, USING "_$   Reduction in area. . . . . .    ###.###      %"; RA
5170 IF CTE <> 0 THEN PRINT #2, USING "_$   CTE. . . . . . . . . . . . .     ##.####^^^^ 1/C"; CTE / (1000000! * (5 / 9))
5180 PRINT #2, USING "_$   Yield offset . . . . . . . .     ##.####     %"; YOFF
5190 PRINT #2, "$  ------------------ Calculated values -----------------------"
5200 PRINT #2, "$  Strain Hardening equation       s = s0 * e^m"
5210 PRINT #2, USING "_$   Equation constants             s0 = ##.#####^^^^   m  = #.######"; SIGMA0 * cc2; M
5220 PRINT #2, USING "_$   Yield point                    sy = ##.#####^^^^   ey = #.######"; STRESSY * cc2; STRAINY
5230 PRINT #2, USING "_$   Ultimate (Engineering)         SU = ##.#####^^^^   EU = #.######"; SIGMAU * cc2; EXP(M) - 1!
5240 PRINT #2, USING "_$   Ultimate Stress/Total Strain  sut = ##.#####^^^^  eut = #.######"; STRESSUT * cc2; STRAINUT
5245 PRINT #2, USING "_$   Effective Plastic Failure Strain                 epfs = #.######"; SFAIL

PRINT #2, "$#     MID       RHO         E        PR         K         N       src       srp"
PRINT #2, "$             gm/cm^3      Mbar                Mbar       "
PRINT #2, USING "        xx_,##.####^^^^_,##.####^^^^_,##.###_,##.####^^^^_,##.######_, 0.0, 0.0"; RHO * cc1; E * cc2; NU; SIGMA0 * cc2; M
REM         1 7.3240E-4 3.0000E+7  0.290000 1.2811E+5  0.086821
PRINT #2, "$#    sigy        vp      epfs"
PRINT #2, USING "       0.0       0.0    #.####"; ZFAIL
PRINT #2, "=================================================================="

NEXT i
PRINT "DONE"
RESET


