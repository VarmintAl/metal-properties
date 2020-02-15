4 KEY OFF
5 SCREEN 0
6 COLOR 12, 8, 1: CLS
7 DEF SEG = 64
8 POKE 23, (PEEK(23) OR 64)'TURN CAP LOCKS ON
9 DEF SEG = 0
10 DIM X$(40), Y$(40), T$(40), G$(300)
30 PRINT "                    >>> GAS EQUATION OF STATE CODE <<<"
35 PRINT "                               By Al Harral"
40 PRINT
45 COLOR 11, 8, 1
50 PRINT "          This code uses the Beattie-Bridgeman equaiton of state"
60 PRINT "          to calculate the pressure, volume, and mass, given"
70 PRINT "          two of these and the temperature.  The units are:"
80 PRINT
90 PRINT "                      Pressure...(psi) & (pascal)"
100 PRINT "                     Volume.............(liters)"
110 PRINT "                     Mass................(grams)": PRINT
200 GOSUB 7000'read in gas data
210 GOSUB 1000'select gas (n)
220 GOSUB 2500'get temperature
230 GOSUB 2000'select case (j5)
240 ON J5 GOSUB 3000, 4000, 5000
250 GOSUB 10000'print the results
260 INPUT "new (G)as, (C)ondition, (T)emperature, (R)un, or (E)nd"; A$
265 F3 = 0
270 IF LEFT$(A$, 1) = "R" THEN 240
280 IF LEFT$(A$, 1) = "G" THEN GOSUB 1000
290 IF LEFT$(A$, 1) = "C" THEN GOSUB 2000
300 IF LEFT$(A$, 1) = "T" THEN 500
305 IF LEFT$(A$, 1) = "E" THEN 400
310 GOTO 260
400 SCREEN 0
405 COLOR 7, 0, 0
410 CLS
420 KEY ON
430 END
500 'for a renge of temperature
510 PRINT "Calculations for a range of temperatures.........."
520 PRINT "Start, Stop, and Incremental temperature (deg f)"
530 INPUT F1, F2, F3
540 GOSUB 2000'get condition
550 PRINT
560 PRINT "   Temp           Pressure           Volume           Mass"
561 PRINT "  (deg. f)         (psig)           (liters)         (grams)"
570 FOR F = F1 TO F2 + .00001 STEP F3
580 ON J5 GOSUB 3010, 4010, 5010
651 PRINT USING "######.##      #######.###       #####.######    ######.######"; F; P0; V; M
690 NEXT
695 GOTO 260

1000 'select the gas
1010 PRINT "The gases available are:"
1020 FOR I = 1 TO 10
1030 PRINT USING " ### ........ \                  \"; I; G$(I)
1040 NEXT
1050 PRINT
1060 INPUT "Desired gas number"; N
1070 N = INT(N + .1)
1080 IF N < 1 OR N > 10 THEN 1060
1090 RETURN

2000 'select the case
2010 PRINT "The three cases are:"
2020 PRINT "(1)... Given: Volume & Pressure ----- Find the Mass"
2030 PRINT "(2)... Given: Volume & Mass --------- Find the Pressure"
2040 PRINT "(3)... Given: Pressure & Mass ------- Find the Volume"
2050 INPUT "Select the desired case"; J5
2060 J5 = INT(J5)
2070 IF J5 < 1 OR J5 > 3 THEN 2010
2080 RETURN

2500 'get temperature in deg f
2510 INPUT "Temperature (deg f)"; F
2520 RETURN

3000 'given v, p0 find m
3005 INPUT "Volume (liters), Pressure (psig)"; V, P0
3010 P = (P0 + A1) / A1
3011 'p0=pressure in psi
3012 'p=pressure in atm
3013 'a1=one atm
3020 T = 273.16 + (F - 32) * 5 / 9'temp in kelvin
3030 GOSUB 6000 'solve for v2 in liters/(gram mole)
3040 M = V * M1(N) / V2'mass in grams
3050 RETURN


4000 'given v, m find p0
4005 INPUT "Volume (liters), Mass (grams)"; V, M
4010 'v in liters
4020 'm in grams
4025 T = 273.16 + (F - 32) * 5 / 9'temp in kelvin
4030 V2 = V * M1(N) / M'liters/(gram mole)
4040 A = A0(N) * (1 - A1(N) / V2)
4050 B = B0(N) * (1 - B1(N) / V2)
4060 E = C(N) / (V2 * T ^ 3)
4070 P = (R * T * (1 - E) * (V2 + B) - A) / V2 ^ 2
4080 P0 = (P - 1) * A1
4090 RETURN

5000 'given p0, m find v
5005 INPUT "Pressure (psi), Mass (grams)"; P0, M
5010 P = (P0 + A1) / A1
5020 T = 273.16 + (F - 32) * 5 / 9'temp in kelvin
5030 GOSUB 6000 'solve for v2 in liters/(gram mole)
5040 V = V2 * M / M1(N)'in liters
5050 RETURN

6000 'start of newton method
6010 'set up the constants for newton method
6020 C0 = R * C(N) * B1(N) * B0(N) / T ^ 2
6030 C1 = -R * T * B1(N) * B0(N) - R * C(N) * B0(N) / T ^ 2 + A1(N) * A0(N)
6040 C2 = R * T * B0(N) - R * C(N) / T ^ 2 - A0(N)
6050 C3 = R * T
6060 C4 = -P
6070 'newtons method
6080 'given p0 (atm) and t (kelvin) find specific volume
6090 'specific volume v2 (liters/(gram-mole))
6095 V1 = R * T / P
6100 T6 = C4 * V1 ^ 4 + C3 * V1 ^ 3 + C2 * V1 ^ 2 + C1 * V1 + C0
6110 B6 = 4 * C4 * V1 ^ 3 + 3 * C3 * V1 ^ 2 + 2 * C2 * V1 + C1
6120 V2 = V1 - T6 / B6
6125 IF F3 > 0 THEN 6140
6130 PRINT "Difference by Newton method ="; ABS(V2 - V1)
6140 IF ABS(V2 - V1) <= .000001 THEN 6170
6150 V1 = V2
6160 GOTO 6100
6170 RETURN

7000 'Beattie-Bridgeman equaiton of state constants
7005 A1 = 14.6959: R = .08206
7010 N = 10' for 10 gases
7030 DATA "Helium",.0216,.05984,.014,0,.004,4.0026
7040 DATA "Neon",.2125,.2196,.02060,0,.101,20.183
7050 DATA "Argon",1.2907,.2328,.03931,0,5.99,39.948
7060 DATA "Hydrogen",.1975,-.00506,.02096,-.04359,.0504,2.01594
7070 DATA "Nitrogen",1.3445,.02617,.05046,-.00691,4.2,28.0134
7080 DATA "Oxygen",1.4911,.02562,.04624,.004208,4.8,31.9988
7090 DATA "Air",1.3012,.01931,.04611,-.001101,4.34,29
7100 DATA "Carbon Dioxide",5.0065,.07132,.10476,.07235,66,44.00995
7110 DATA "Ammonia",2.393,.17031,.03415,.19112,476.87,17.03061
7120 DATA "Methane",2.2769,.01855,.05587,-.01587,12.83,16.04303
7130 FOR I = 1 TO N
7140 READ G$(I), A0(I), A1(I), B0(I), B1(I), C(I), M1(I)
7170 C(I) = C(I) * 10000
7200 NEXT
7210 RETURN

10000 PRINT : PRINT USING "\                   \ at ###.## (deg. f)"; G$(N); F
10010 P0 = (P - 1) * A1
10020 P1 = P0 * 6894.757'pascal
10030 'v already in liters
10040 'm already in grams
10050 PRINT USING "Pressure (psig)....... ######.#######"; P0
10060 PRINT USING "Pressure (pascal).....     ##.###^^^^"; P1
10070 PRINT USING "Volume (liters)....... ######.#######"; V
10080 PRINT USING "Mass (grams).......... ######.#######"; M
10090 PRINT
10100 RETURN

