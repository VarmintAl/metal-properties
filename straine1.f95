  
! .......... mat18.bas english units 6/7/2010
! .......... english units (lbf-s2/in, in, s, lbf, psi, lbf-in)

       real(kind=4) rho, e, nu, sigmay, sigmau, elong, ra, yoff, g, k, strainf
       real(kind=4) tstrainy, tstressy, tstrainu, param1
       real(kind=4) sigma0, strainy, sfail, strainut, tstressu, s0, am, stressy
       real(kind=4) top, bot
       real(kind=4) stressf, stressut
       real(kind=4) m, fofm, fpofm
       character a*60, b*60
       integer i, j
       i=1
       j=1
       b=" "
       
      write(*,*) "Power Law Plasticity"
      write(*,*) "English Units 6/7/2010"
      write(*,*) "http://www.VarmintAl.com/aengr.htm"
      write(*,*) "English Units (lbf-s2/in, in, s, lbf, psi, lbf-in)"
      write(*,*) "Output formated for LS-DYNA"
      write(*,*)
      
      write (*,*)"Material Name........................ "
      read (*,10) a      
      write (*,*)"Density (lb/cu-in)................... "
      read (*,*) rho
      write (*,*)"Elastic Modulus (psi)................ "
      read (*,11) e      
      write (*,*)"Poisson Ratio........................ "
      read (*,11) nu      
      write (*,*)"Engineering Yield Stress (psi)....... "
      read (*,11) sigmay      
      write (*,*)"Engineering Ultimate Stress (psi).... "
      read (*,11) sigmau      
      write (*,*)"Elongnation (%)...................... "
      read (*,11) elong            
      write (*,*)"Reduction in Area  (%)............... "
      read (*,11) ra      
      write (*,*)"Yield Offset (%)..................... "
      read (*,11)yoff
      
      write (*,'(1a60)') a
      write (*,'(a,f12.4)')'Density (lb/cu-in)................... ', rho
      write (*,'(a,1pe12.4)')'Elastic Modulus (psi)................ ', e
      write (*,'(a,f12.4)')'Poisson Ratio........................ ', nu
      write (*,'(a,f12.4)')'Engineering Yield Stress (psi)....... ', sigmay
      write (*,'(a,f12.4)')'Engineering Ultimate Stress (psi).... ', sigmau
      write (*,'(a,f12.4)')'Elongnation (%)...................... ', elong
      write (*,'(a,f12.4)')'Reduction in Area  (%)............... ', ra
      write (*,'(a,f12.4)')'Yield Offset (%)..................... ', yoff
 
   10 format (1a60)
   11 format (2f20.6)
       
!        a="aluminum 6061-t6"
!        rho=.098
!        e=10000000.
!        nu=.33
!        sigmay=42200.
!        sigmau=44900.
!        elong=16.5
!        ra=50.
!        yoff=.2
!        s0=54943.
!        am=0.0506919
!        z=0.0


! .......... input #1, rho, e, nu, sigmay, sigmau, elong, ra, cte, yoff, sfail

       sfail = 0

! .......... calculate strain hardening equation       
      
       if (yoff .eq. 0)  yoff = .2
       g = e / (2 * (1 + nu))
       k = e / (3 * (1 - 2 * nu))
       tstrainy = alog(1 + yoff / 100 + sigmay / e)
       tstressy = sigmay * (1 + yoff / 100 + sigmay / e)
 
! .......... assume 30% elongation if not given for first guess of dm

       if (elong .lt. 0.01) then 
       edummy = 30 
       else 
       edummy = elong
       end if       
       tstrainu = alog(1 + edummy / 100)
       tstressu = sigmau * (1 + tstrainu)
       top = alog(tstressu / tstressy)
       bot = alog(tstrainu / tstrainy)
       m =  top/bot
       write (*,*) "tstressu, tstressy, d ", m, top, bot
                   
! .......... use newtons method to solve for m if elong>9%
 
       if (ra .eq. 0 .and. (elong .gt. 0 .and. elong .lt. 9)) go to  40
90     param1 = m / (2.718282 * tstrainy)
       fofm = param1**m - sigmau / tstressy
       fpofm = param1**m * (1 + alog(param1))
       m = m - fofm / fpofm
       
       write (*,*) "iteration and m = ", i, m
       i=i+1
       if (i .gt. 50) go to 91       
       
       if (abs((fofm / fpofm) / m) .gt. .0000015) go to  90
40     sigma0 = tstressy / tstrainy**m
       strainy = 1 / (e / sigma0)**(1 / (1 - m))
       stressy = e * strainy


       if (ra .eq. 0) go to  50
     
! ...     check if failure strain < ultimate strain only for ra<>0
 
       strainf = alog(100 / (100 - ra))
       if (m .lt. 1.01 * strainf) go to 60

50     if (elong .gt. 0 .and. elong .lt. 9) then 
       strainf = alog(1 + elong / 100)     
       else 
       strainf = 0
       end if
       
60     stressf = sigma0 * strainf**m

       if (ra .ne. 0 .or. (elong .eq. 0 .or. elong .gt. 9)) then 
       strainut = m
       else 
       strainut = strainf
       end if
       
       stressut = sigma0 * strainut**m
       
       write (*,*) "The results are formated for the LS-DYNA key word input file."
       write (*,*) " "

       write (*,'(a,a1)') '$ ==================================================================', b
       write (*,'(a,a1)') '*MAT_POWER_LAW_PLASTICITY',b
       write (*,'(a,a1)') '$',b
       write (*,'(a,a60)') '$  ', a
       write (*,'(a,a1)') '$  ',b
       write (*,'(a,a1)') '$             Power Law Plasticity (MAT_018)',b
       write (*,'(a,a1)') '$       English units (lbf-s2/in, in, s, lbf, psi, lbf-in)',b
       write (*,'(a,a1)') '$  ',b
       write (*,'(a,f12.4)') '$  material density........... lb/in^3 ', rho
       write (*,'(a,1pe12.4)') '$  youngs modulus................. psi ', e
       write (*,'(a,1pe12.4)') '$  shear modulus...................psi ', g
       write (*,'(a,1pe12.4)') '$  bulk modulus ...................psi ', k
       write (*,'(a,f12.4)') '$  poisson ratio...................... ', nu
       write (*,'(a,f12.4)') '$  yield stress at offset..........psi ', sigmay
       write (*,'(a,f12.4)') '$  engineering ultimate stress.....psi ', sigmau
       write (*,'(a,f12.4)') '$  elongation at failure.............% ', elong
       write (*,'(a,f12.4)') '$  reduction in area.................% ', ra
       write (*,'(a,f12.4)') '$  yield offset .....................% ', yoff
       write (*,'(a,a1)') '$  ------------------ calculated values -----------------------', b
       write (*,'(a,a1)') '$  strain hardening equation       s = s0 * e^m',b
       write (*,'(a,f10.2,a,f10.6)') '$  equation constants             s0 = ',sigma0, '   m = ', m
       write (*,'(a,f10.2,a,f10.6)') '$  yield point                    sy = ',stressy,'  ey = ', strainy
       write (*,'(a,f10.2,a,f10.6)') '$  ultimate (engineering)         su = ',sigmau, '  eu = ', exp(m) - 1!
       write (*,'(a,f10.2,a,f10.6)') '$  ultimate stress/total strain  sut = ',stressut,' eut = ', strainut
!       write (*,'(a,f10.6)') '$  effective plastic failure strain              epfs = ', sfail

       write (*,'(a,a1)') '$#     mid       rho         e        pr         k         n       src       srp', b
       write (*,'(a,a1)') '$          lbf-s2/in       psi                 psi', b
       write (*,'(a,1pe10.3,1pe10.3,0pf10.4,f10.1,f10.7)') '        xx', rho/386.09, e, nu, sigma0, m
!            1 7.3240e-4 3.0000e+7  0.290000 1.2811e+5  0.086821
       write (*,'(a,a1)') '$#    sigy        vp      epfs', b
       write (*,'(3f10.2)') z, z, sfail
       write (*,'(a,a1)') '$ ========Replace xx above with the local material I.D. number======', b

       go to 92  
       
  91   write (*,*) "Error. Could not find (m) in s = s0 * e^m. Check the input."     

  92   continue     
       write (*,*) " "
       write (*,*) "Calculation Completed"
       write (*,*) " "
       stop
       end
     
     

