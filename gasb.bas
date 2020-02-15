4 key off
5 screen 0
6 color 12, 8, 1: cls
7 def seg = 64
8 poke 23, (peek(23) or 64)'turn cap locks on
9 def seg = 0
10 dim x$(40), y$(40), t$(40), g$(300)
30 print "                    >>> gas equation of state code <<<"
35 print "                               by al harral"
40 print
45 color 11, 8, 1
50 print "          this code uses the beattie-bridgeman equaiton of state"
60 print "          to calculate the pressure, volume, and mass, given"
70 print "          two of these and the temperature.  the units are:"
80 print
90 print "                      pressure...(psi) & (pascal)"
100 print "                     volume.............(liters)"
110 print "                     mass................(grams)": print
200 gosub 7000'read in gas data
210 gosub 1000'select gas (n)
220 gosub 2500'get temperature
230 gosub 2000'select case (j5)
240 on j5 gosub 3000, 4000, 5000
250 gosub 10000'print the results
260 input "new (g)as, (c)ondition, (t)emperature, (r)un, or (e)nd"; a$
265 f3 = 0
270 if left$(a$, 1) = "r" then 240
280 if left$(a$, 1) = "g" then gosub 1000
290 if left$(a$, 1) = "c" then gosub 2000
300 if left$(a$, 1) = "t" then 500
305 if left$(a$, 1) = "e" then 400
310 goto 260
400 screen 0
405 color 7, 0, 0
410 cls
420 key on
430 end
500 'for a renge of temperature
510 print "calculations for a range of temperatures.........."
520 print "start, stop, and incremental temperature (deg f)"
530 input f1, f2, f3
540 gosub 2000'get condition
550 print
560 print "   temp           pressure           volume           mass"
561 print "  (deg. f)         (psig)           (liters)         (grams)"
570 for f = f1 to f2 + .00001 step f3
580 on j5 gosub 3010, 4010, 5010
651 print using "######.##      #######.###       #####.######    ######.######"; f; p0; v; m
690 next
695 goto 260
1000 'select the gas
1010 print "the gases available are:"
1020 for i = 1 to 10
1030 print using " ### ........ \                  \"; i; g$(i)
1040 next
1050 print
1060 input "desired gas number"; n
1070 n = int(n + .1)
1080 if n < 1 or n > 10 then 1060
1090 return
2000 'select the case
2010 print "the three cases are:"
2020 print "(1)... given: volume & pressure ----- find the mass"
2030 print "(2)... given: volume & mass --------- find the pressure"
2040 print "(3)... given: pressure & mass ------- find the volume"
2050 input "select the desired case"; j5
2060 j5 = int(j5)
2070 if j5 < 1 or j5 > 3 then 2010
2080 return
2500 'get temperature in deg f
2510 input "temperature (deg f)"; f
2520 return
3000 'given v, p0 find m
3005 input "volume (liters), pressure (psig)"; v, p0
3010 p = (p0 + a1) / a1
3011 'p0=pressure in psi
3012 'p=pressure in atm
3013 'a1=one atm
3020 t = 273.16 + (f - 32) * 5 / 9'temp in kelvin
3030 gosub 6000 'solve for v2 in liters/(gram mole)
3040 m = v * m1(n) / v2'mass in grams
3050 return
4000 'given v, m find p0
4005 input "volume (liters), mass (grams)"; v, m
4010 'v in liters
4020 'm in grams
4025 t = 273.16 + (f - 32) * 5 / 9'temp in kelvin
4030 v2 = v * m1(n) / m'liters/(gram mole)
4040 a = a0(n) * (1 - a1(n) / v2)
4050 b = b0(n) * (1 - b1(n) / v2)
4060 e = c(n) / (v2 * t ^ 3)
4070 p = (r * t * (1 - e) * (v2 + b) - a) / v2 ^ 2
4080 p0 = (p - 1) * a1
4090 return
5000 'given p0, m find v
5005 input "pressure (psi), mass (grams)"; p0, m
5010 p = (p0 + a1) / a1
5020 t = 273.16 + (f - 32) * 5 / 9'temp in kelvin
5030 gosub 6000 'solve for v2 in liters/(gram mole)
5040 v = v2 * m / m1(n)'in liters
5050 return
6000 'start of newton method
6010 'set up the constants for newton method
6020 c0 = r * c(n) * b1(n) * b0(n) / t ^ 2
6030 c1 = -r * t * b1(n) * b0(n) - r * c(n) * b0(n) / t ^ 2 + a1(n) * a0(n)
6040 c2 = r * t * b0(n) - r * c(n) / t ^ 2 - a0(n)
6050 c3 = r * t
6060 c4 = -p
6070 'newtons method
6080 'given p0 (atm) and t (kelvin) find specific volume
6090 'specific volume v2 (liters/(gram-mole))
6095 v1 = r * t / p
6100 t6 = c4 * v1 ^ 4 + c3 * v1 ^ 3 + c2 * v1 ^ 2 + c1 * v1 + c0
6110 b6 = 4 * c4 * v1 ^ 3 + 3 * c3 * v1 ^ 2 + 2 * c2 * v1 + c1
6120 v2 = v1 - t6 / b6
6125 if f3 > 0 then 6140
6130 print "difference by newton method ="; abs(v2 - v1)
6140 if abs(v2 - v1) <= .000001 then 6170
6150 v1 = v2
6160 goto 6100
6170 return
7000 'beattie-bridgeman equaiton of state constants
7005 a1 = 14.6959: r = .08206
7010 n = 10' for 10 gases
7030 data "helium",.0216,.05984,.014,0,.004,4.0026
7040 data "neon",.2125,.2196,.02060,0,.101,20.183
7050 data "argon",1.2907,.2328,.03931,0,5.99,39.948
7060 data "hydrogen",.1975,-.00506,.02096,-.04359,.0504,2.01594
7070 data "nitrogen",1.3445,.02617,.05046,-.00691,4.2,28.0134
7080 data "oxygen",1.4911,.02562,.04624,.004208,4.8,31.9988
7090 data "air",1.3012,.01931,.04611,-.001101,4.34,29
7100 data "carbon dioxide",5.0065,.07132,.10476,.07235,66,44.00995
7110 data "ammonia",2.393,.17031,.03415,.19112,476.87,17.03061
7120 data "methane",2.2769,.01855,.05587,-.01587,12.83,16.04303
7130 for i = 1 to n
7140 read g$(i), a0(i), a1(i), b0(i), b1(i), c(i), m1(i)
7170 c(i) = c(i) * 10000
7200 next
7210 return
10000 print : print using "\                   \ at ###.## (deg. f)"; g$(n); f
10010 p0 = (p - 1) * a1
10020 p1 = p0 * 6894.757'pascal
10030 'v already in liters
10040 'm already in grams
10050 print using "pressure (psig)....... ######.#######"; p0
10060 print using "pressure (pascal).....     ##.###^^^^"; p1
10070 print using "volume (liters)....... ######.#######"; v
10080 print using "mass (grams).......... ######.#######"; m
10090 print
10100 return

