cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
c
c  Begin of the file "s37.for"                      date: July 17, 1996
c
c                                   modified: December 09, 2008
c                                   changing filenames of MOD and OSC files,
c                                   changing all block names to *.s37 and so on. 
c
c ==========                                    ==========
c      <<<<<<    Stellar evolution package    >>>>>>
c ==========                                    ==========
c
c          prepared (in part) and distributed by:
c
c      Ryszard Sienkiewicz
c      Nicolaus Copernicus Astronomical Center
c      Bartycka 18, 00-716 Warsaw, POLAND
c
c      e-mail: camk@plearn.bitnet
c      FAX   : (intl) - 48 (Poland) - 22 (Warsaw) - 410828
c      telex : 81 3978  (zapan pl)
c
c ==========                                    ==========
c               ANY COMMENTS ARE MOST WELCOME
c ==========                                    ==========

c        This file contains "Schwarzschild-type" program "s37",
c        providing the evolutionary program "h37" with first, static
c        Zero-age Hydrogen Main Sequence model.

c        When compiling, the program is automatically supplemented with
c        seven extra disk files:
c         - "data.s37"                       - "opacity.s37"
c         - "envelope.s37"                   - "nuclear.s37"
c         - "center.s37"                     - "commonsb.s37"
c         - "eostate.s37"                (see following comments)
c
c ----------------------------------------------------------------------------
c Input data:
c    -) screen input 
c         starm = total stellar mass                 [M Sun]
c         telg  = log10 (surface temperature)        [K]
c         fllg  = log10 (surface luminosity)         [L Sun]
c         tclg  = log10 (central temperature)        [K]
c         rhclg = log10 (central density)            [g/cm**3]
c         x, z  = hydrogen and heavy element content [by mass fraction]
c   notice:
c         telg, fllg, tclg, rhclg are guessed values of the boundary
c         parameters to be improved by the subroutine "sch"
c
c    -) other input data files, as requested by included subroutines
c       (see "include" directives at the end of the code) i.e.:
c        -) opacity data files - e.g. '700020c.g93'
c        -) equation-of-state data file - "electron.gas"
c
c Output data:
c    -) to a disk file "mod00000"
c    -) screen output (see explanations inside the code).
c
c ----------------------------------------------------------------------------
c WARNING: rename, or remove existing "mod00000" file
c          before running the program again !
c          Real*8 (double precision) has extended exponents.
c          On other machines there may be fatal underflows or zero divides.
c ----------------------------------------------------------------------------
c
c "s37.for"  is the main part of the code and includes the main program
c            and the following nine subroutines, all others subroutines
c            are located and described on separate disk files and are
c            included through "include" directives.
c
c    subroutine:        subject:
c    -----------        --------
c
c   - "setsch" -    sets parameters, reads data for iterations
c   - "sch"    -    iterations, Scwarzschild method
c                     SUBROUTINES CALLED:
c                     - "upintg" -
c                     - "dnintg" -
c                     - "solve"  -
c   - "solve"  -    finds corrections, Newton method
c   - "dnintg" -    downward integration to the fitting point
c                     SUBROUTINES CALLED:
c                     - "env"    - (included - file "envelope.s37")
c                     - "integ"  -
c   - "upintg" -    upward integration to the fitting point
c                     SUBROUTINES CALLED:
c                     - "center" - (included - file "center.s37")
c                     - "integ"  -
c   - "integ"  -    conducts integrations
c                     SUBROUTINES CALLED:
c                     - "step"   -
c   - "step"   -    used by "integ"
c                     SUBROUTINES CALLED:
c                     - "rhs"    -
c   - "rhs"    -    used by "step" to calculate the right-hand sides
c                   of stellar structure equations
c                     SUBROUTINES CALLED:
c                     - "opact" -  (included - file "opacity.s37")
c                     - "state" -  (included - file "eostate.s37")
c                     - "nburn" -  (included - file "nuclear.s37")
c   - "storeh" -    stores results for the evolutionary program "h37"
c                     SUBROUTINES CALLED:
c                     - "upintg" -
c                     - "dnintg" -
c                     - "grid1"  - (included - file "commonsb.s37")
c                          redistributes mass points
c                     - "writeh" - (included - file "commonsb.s37")
c                          writes a model on the disk file "mod00000"
c ----------------------------------------------------------------------------
c ----------------------------------------------------------------------------
c
c There are some separated files which provide the code with "standard"
c       physics and boundary conditions. They are listed below.
c T h e s e   f i l e s   m a y   b e   e a s i l y   r e p l a c e d, if ne-
c cessary. However, to avoid changes in the main part, new SUBROUTINES should:
c   -) provide the program with actually requested data
c   -) have the same names as the old ones
c   -) be connected with the code in the same manner (i.e., through proper
c      parameters and/or common blocks - see appropriate "call" statements).
c
c   disk file:             subject:
c   ----------             --------
c
c "data.s37"     - model parameters and constants
c "envelope.s37" - calculates envelope model and provides us with values
c                  of all variables at the bottom of the envelope
c                  (used to prepare outer boundary conditions)
c "center.s37"   - provides us with values of all variables at the surface
c                - of a small inner core
c                  (used to prepare inner boundary conditions)
c "eostate.s37"  - equation of state includes contributions from:
c                  nondegenerate/degenerate electrons, nondegenerate ions
c                  and radiation
c                  (reads the disk file "electron.gas")
c "opal.s37"  -    reads and interpolates coefficient of opacity from
c                  the OPAL radiative opacity tables with conductive
c                  opacities and Alexander's molecular opacities added)
c "nuclear.s37"- calculates nuclear reaction rates for p-p chain and CNO cycle
c                  + neutrino energy loss rate (if hydrogen content = 0)
c "commonsb.s37" - includes subroutines common for the "s37" and "h37" codes.
c
c ----------------------------------------------------------------------------
c ----------------------------------------------------------------------------
c
c 
c HEREAFTER:  "time difference" (or "time change") stands for the change
c             of a variable (the array "hx") during the current time step
c             (the array "dh").
c
c             "Gradient" alone stands for the temperature gradient.
c
c..............................................................................

      implicit double precision(a-h,o-z)
      include 'data.s37'
      common /comkappa/ ideriv
      dimension xvec(nx)
c                                                        --- set the constants,
c                                                            read the parameters
      ideriv=0
      call setsch(starm,telg,fllg,tclg,rhclg,xvec)
c                                                        --- converge the model
      call sch   (starm,telg,fllg,tclg,rhclg,xvec,npts)
c                                                        --- store the model
      call storeh(starm,telg,fllg,tclg,rhclg,xvec,npts)
c
      stop '=> s37 - okay'
c                                                        --- end of main program
      end
c
c............................................................................
      subroutine setsch(starm,telg,fllg,tclg,rhclg,xvec)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      dimension xvec(nx)
      common/hen/ hx(m2,nt), dh(m2,nt), nh
      common /comset/ vrot              ! to transfer vrot from setsch to sch
c
c subject:
c     this subroutine sets the constants and reads the parameters of a
c     stellar main sequence model to be calculated by the program "s37"
c output:
c     starm   = total stellar mass                          [M Sun]
c     telg    = log10 (surface temperature)   (a guess)     [K]
c     fllg    = log10 (surface luminosity)    (a guess)     [L Sun]
c     tclg    = log10 (central temperature)   (a guess)     [K]
c     rhclg   = log10 (central density)       (a guess)     [g/cm**3]
c     xvec(1) =     hydrogen content                        [by mass fraction]
c     xvec(2) =     helium-3 content (= 0 in this version)  [by mass fraction]
c     xvec(3) =     helium-4 content                        [by mass fraction]
c     xvec(6) =     nitrogen-14 content                     [by mass fraction]
c     xvec(7) =     oxygen-16 content                       [by mass fraction]
c     zp        zp * density / amu = total # of CNO nuclei.
c..........................................................................

c                              --- read the model parameters from the keyboard:
      write(*,100)
      write(*,101)
      read(*,*) starm, telg, fllg, tclg, rhclg
      rsur = (fllg+log10(sunl/8.d0/pi/sigcst))/2.d0-2.d0*telg
      rsur = 10.0d0**rsur
      write(*,*) ' Type Vrot [km/sec] :'
      read (*,*) vrot
      omega=vrot*1.0d5/rsur
      write (*,*) ' Vrot, Omega =', vrot, omega
      write(*,102)
      read(*,*) x, z
      write(*,103) starm, telg, fllg, tclg, rhclg, x, z
  224 format(/,' Thanks ! - end of screen input - now, wait...')
      write(*,224)
c                              --- set the constants related to the actual
c                                   stellar mass and luminosity
      flunit=10.0**fllg*sunl
      fmtot=starm*sunm
c                              --- set data in common /heninc/
      model=0
      time=0
      dtime=0
c                              --- set vector of chemical abundances
      xvec(1) = x
      xvec(2) = 0.0
      xvec(3) = 1.-z-xvec(1)
c                                  pric12 * z  = primordial C12       content
c                                  prin14 * z  = primordial N14       content
c                                  prio16 * z  = primordial O16       content
c                                  xvec(6)     = primordial C12 + N14 content.
c                                  Indexes 4-5 are reserved to follow
c                                  (in future) C12 and C13 abundances
      xvec(4) = 0.
      xvec(5) = 0.
      xvec(6) = (pric12    + prin14               ) * z
      xvec(7) =                          prio16     * z
      zp      = (pric12/12 + prin14/14 + prio16/16) * z
c
c                              --- initiate common /heninc/
      zed =  z
      do 1 i = 1, 20
         com(i)  =  0.
         icom(i) =  0
         mp(i)   = -1
    1 continue
      com(13)=omega    

  100 format(/,' This program "s37"',
     *' calculates a Zero-age Hydrogen Main Sequence model'
     *,/,' - results will be stored on a disk file "mod00000"')
  101 format(' Type starm, telg, fllg, tclg, rhclg :')
  102 format(' Type X, Z :')
  103 format('                 Mass of the star / solar mass =',f7.4/
     *       '      log10 (surface temperature in degrees K) =',f7.4/
     *       '         log10 (luminosity / solar luminosity) =',f7.4/
     *       '      log10 (central temperature in degrees K) =',f7.4/
     *       '            log10 (central density in g/cm**3) =',f7.4/
     *       '         hydrogen abundance (by mass fraction) =',f7.4/
     *       '   heavy elements abundance (by mass fraction) =',f7.4/)

c                              --- end of "setsch"
      end
c
c...........................................................................
c
      subroutine sch(starm,telg,fllg,tclg,rhclg,xvec,npts)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      dimension xvec(nx)
      common /comset/ vrot              ! to transfer vrot from setsch to sch
c
c subject:
c     calculate chemically homogeneous stellar model in a thermal
c     equilibrium, i.e. a model on a zero age main sequence by means of
c     fitting results of integrations of envelope and core at the
c     fitting mass, fmf.
c input:
c     starm   = total stellar mass                            [M Sun]
c     telg    = log10 (surface temperature)   (a guess)       [K]
c     fllg    = log10 (surface luminosity)    (a guess)       [L Sun]
c     tclg    = log10 (central temperature)   (a guess)       [K]
c     rhclg   = log10 (central density)       (a guess)       [g/cm**3]
c     xvec(1) =     hydrogen content                        [by mass fraction]
c     xvec(2) =     helium-3 content (= 0 in this version)  [by mass fraction]
c     xvec(3) =     helium-4 content                        [by mass fraction]
c     xvec(6) =     nitrogen-14 content                     [by mass fraction]
c     xvec(7) =     oxygen-16 content                       [by mass fraction]
c output:
c     telg    = log10 (surface temperature) in fitted model   [K]
c     fllg    = log10 (surface luminosity)  in fitted model   [L Sun]
c     tclg    = log10 (central temperature) in fitted model   [K]
c     rhclg   = log10 (central density)     in fitted model   [g/cm**3]
c     npts    = number of mass points needed to store the fitted model
c called
c     upintg, dnintg, solve
c auxiliary variables:
c     difacc     = the maximum difference allowed at the fitting point
c     del        = perturbations of the logarithms of boundary parameters
c     itmax      = the maximum number of iterations
c     iter       = the iteration counter
c     difmax     = the maximum difference calculated at the fitting point
c     delfit(i)  = the calculated differences at the fitting point for
c                     the four variables = xe(i)/xc(i)-1 , i=1,2,3,4
c     xe(i)      = results of the downward integrations at the fitting point
c     xc(i)      = results of the upward integrations at the fitting point
c     deriv(i,j) = partial derivatives of the differences at the fitting
c                     point with respect to the boundary parameters
c     delce(i)   = corrections added to the boundary parameters
c............................................................................

      common /comsch/ xe(20),xe1(20),xe2(20),xc(m2),xc3(m2),xc4(m2),
     *                delfit(4),delce(4),deriv(4,5)
c               --- the common above is declared for needs of SVS/386 v.01
c                   fortran compiler. It is not used outside of this subroutine

c                               --- set the constants for fitting and iterations
      difacc=1.0d-8
      del=1.0d-9
      itmax=20
c                               --- iterations
      iter=0
    1 continue
      iter=iter+1
c                               --- emergency exit if iterations do not converge
      if (iter.gt.itmax) then
         write(*,*)' iterations have not converged - increase "itmax"'
         write(*,*)' ... or try again using another initial values
     $ of the stellar parameters'
         write(*,*)' ... or decrease values of the "delmax" array'
         stop ': file "s37.for" - subroutine "sch" - '
      end if
c                              --- basic downward and upward integrations

      istore=0
      call upintg (tclg,rhclg,xvec,istore,xc,nptup)
      call dnintg (starm,telg,fllg,xvec,istore,xe,nptdn)

c                              --- number of mass points needed to store results
c                                  (not including space for boundary values)
      npts=nptdn+nptup-1
c                              --- compute differences between downward and
c                                  upward integrations.
      difmax=0.0
      do 2 i=1,4
         delfit(i)=xe(i)/xc(i)-1.0
         dif=abs(delfit(i))
         if (difmax.lt.dif) difmax=dif
    2 continue
c                              --- check if downward and upward integrations
c                                  fit each other
      if(difmax.lt.difacc)go to 5

c                              --- calculate derivatives of differences with
c                                  respect to the boundary telg,fllg,tclg,rhclg

      istore=0
      call dnintg (starm,telg+del,fllg,xvec,istore,xe1,nptdn)
      call dnintg (starm,telg,fllg+del,xvec,istore,xe2,nptdn)
      call upintg (tclg+del,rhclg,xvec,istore,xc3,nptup)
      call upintg (tclg,rhclg+del,xvec,istore,xc4,nptup)

      do 3 i=1,4
         deriv(i,1)=((xe1(i)/xc(i)-1.0)-delfit(i))/del
         deriv(i,2)=((xe2(i)/xc(i)-1.0)-delfit(i))/del
         deriv(i,3)=((xe(i)/xc3(i)-1.0)-delfit(i))/del
         deriv(i,4)=((xe(i)/xc4(i)-1.0)-delfit(i))/del
         deriv(i,5)=delfit(i)
    3 continue
c                              --- calculate corrections to the boundary
c                                  parameters. Newton method
      call solve(deriv,delce,facdel)

c                              --- write results of iteration

      if(iter.eq.1)write(*,149)difacc
      if(iter.eq.1)write(*,159)
      write(*,101)iter-1,telg,fllg,tclg,rhclg,facdel,difmax

c                              --- add corrections to the boundary parameters
      telg=telg+delce(1)
      fllg=fllg+delce(2)
      tclg=tclg+delce(3)
      rhclg=rhclg+delce(4)
c                              --- end of an iteration
      go to 1
c                              --- iterations have converged, downward and
c                                  upward integrations fit each other
    5 continue

      write(*,101)iter-1,telg,fllg,tclg,rhclg,facdel,difmax

      write(*,103)
      x=xvec(1)
      z=1.-xvec(1)-xvec(3)
      write(*,104)x,z,starm,telg,fllg,tclg,rhclg
c
      open (77,file='s37.out',status='unknown',form='formatted')
      rewind (77)      
      write (77,771) starm, telg, fllg, tclg, rhclg
  771 format(0p5f9.4)
      write (77,772) vrot
  772 format(0p1f8.4,'       ******** Vrot [km/sec]')
      write (77,773) x, z
  773 format(0p2f9.5)
      close (77)
c
      return

  101 format(1x,i5,4f8.4,3x,2f15.9)
  103 format(1x,'   X        Z       M     log Ts   log L  log Tc'
     *,'  log rhc')
  104 format(1x,f6.3,f10.7,5f8.4,i4)
  149 format (1x,/,
     $' iter    = the iteration counter                       ',/,
     $' log Tsurf = log10 (surface temperature)      [K]      ',/,
     $' log L   = log10 (surface luminosity)         [L Sun]  ',/,
     $' log Tc  = log10 (central temperature)        [K]      ',/,
     $' log rhc = log10 (central density)            [g/cm**3]',/,
     $' facdel  = reduction factor for the corrections,',
     $' if no reduction',/,
     $'           is applied then "facdel" indicates',
     $' the largest correction',/,
     $'           (in units of "delmax")',/,
     $' difmax  = the maximum relative difference calculated',
     $' at the fitting point,',/,
     $'           in a fitted model, it is less than  ',1pd6.0)
  159 format(/,'  iter  log Ts  log L   log Tc  log rhc        facdel',
     $'         difmax')

c                              --- end of "sch"
      end
c
c............................................................................

      subroutine solve(deriv,delce,facdel)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      dimension deriv(4,5),delce(4)

c subject:
c     solve n=4 linear algebraic equations
c input:
c     deriv(n,n+1) = array of coefficients of "n" linear algebraic equations
c output:
c     delce(n)   = n corrections, without delmax(i) they should satisfy n
c                  equations: sum over j: deriv(i,j)*delce(j)+deriv(i,n+1)=0
c                  however, delce(i) are reduced so as to make them not larger
c                  than delmax(i)
c     facdel     = reduction factor for the corrections, if no reduction is
c                  applied then "facdel" indicates the largest correction
c                  (in units of "delmax")
c auxiliary variables:
c     delmax(n)  = maximum acceptable values of corrections
c     n = 4      = number of equations = number of unknowns (i.e. corrections)
c............................................................................

      dimension delmax(4)
c                                --- set maximum acceptable values for
c                                    corrections to the boundary parameters:
c                                    telg, fllg, tclg, rhclg respectively
      delmax(1)=0.02d0
      delmax(2)=0.05d0
      delmax(3)=0.02d0
      delmax(4)=0.06d0

      n=4
      nm=n-1
      np=n+1
c                                --- solving n * n set of linear equations
      do 1,k=1,nm
         kp=k+1
         fac1=deriv(k,k)
         do 2 i=kp,n
            fac2=deriv(i,k)
            do 3 j=kp,np
               deriv(i,j)=deriv(i,j)*fac1-deriv(k,j)*fac2
    3       continue
    2    continue
    1 continue

c                              --- the matrix of the set is now triangular

      delce(n)=-deriv(n,np)/deriv(n,n)

      do 4 i=2,n
         i1=n-i+1
         i2=i1+1
         delce(i1)=-deriv(i1,np)
         do 5 j=i2,n
            delce(i1)=delce(i1)-deriv(i1,j)*delce(j)
    5    continue
         delce(i1)=delce(i1)/deriv(i1,i1)
    4 continue
c                              --- the unknowns delce(i) have been found

c                              --- find the largest "delce" in units of "delmax"
      dm=0.0
      do 6 i=1,n
         d=abs(delce(i)/delmax(i))
         if(dm.lt.d)dm=d
    6 continue
c                              --- reduce corrections to the boundary parameters
      facdel=1.0
      if(dm.gt.1.0)facdel=dm
      do 7 i=1,n
         delce(i)=delce(i)/facdel
    7 continue

      facdel=dm
c                              --- end of "solve"
      end
c
c............................................................................

      subroutine dnintg(starm,telg,fllg,xvec,istore,xx,npts)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      dimension xvec(nx)
      dimension xx(m2)

c subject:
c     integrate stellar structure equations from the surface to the fitting
c     point for a chemically homogeneous star
c input:
c     starm   = total stellar mass                            [M Sun]
c     telg    = log10 (surface temperature)                   [K]
c     fllg    = log10 (surface luminosity)                    [L Sun]
c     xvec(1) =     hydrogen content                        [by mass fraction]
c     xvec(2) =     helium-3 content (= 0 in this version)  [by mass fraction]
c     xvec(3) =     helium-4 content                        [by mass fraction]
c     xvec(6) =     nitrogen-14 content                     [by mass fraction]
c     xvec(7) =     oxygen-16 content                       [by mass fraction]
c     istore  = control variable. See "integ" for description
c output:
c     xx at the fitting point:
c     xx(1)   = log10 (density)                               [g/cm**3]
c     xx(2)   = log10 (temperature)                           [K]
c     xx(3)   = log10 (radius)                                [cm]
c     xx(4)   = luminosity / luminosity unit
c     xx(5)   = mass/total mass
c     xx(6)...xx(5+nx) = vector of chemical abundances
c     npts    = number of mass points used through integration
c called
c     env        to integrate the outermost envelope
c     integ      to integrate to the fitting point
c predefined in 'data.s37'
c     mm, nx
c...........................................................................

      dimension resenv(20)

c                                               --- call envelope integrations
      iout=0
      iprint=0
      call env(telg,fllg,starm,xvec,resenv,iout,iprint)

c                                               --- prepare starting vector for
c                                                   downward integrations
      do 1 i=1,mm
      xx(i)=resenv(i)
    1 continue
      do 2 i=1,nx
      xx(mm+i)=xvec(i)
    2 continue
c                                               --- downward integrations
      call integ(xx,npts,istore)
c                                               --- end of "dnintg"
      end
c
c............................................................................
c
      subroutine upintg(tclg,rhclg,xvec,istore,xx,npts)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      dimension xvec(nx), xx(m2)

c subject:
c     integrate stellar structure equations from the center to the fitting
c     point for a chemically homogeneous star
c input:
c     rhclg   = log10 (central density)                       [g/cm**3]
c     tclg    = log10 (central temperature)                   [K]
c     xvec(1) =     hydrogen content                        [by mass fraction]
c     xvec(2) =     helium-3 content (= 0 in this version)  [by mass fraction]
c     xvec(3) =     helium-4 content                        [by mass fraction]
c     xvec(6) =     nitrogen-14 content                     [by mass fraction]
c     xvec(7) =     oxygen-16 content                       [by mass fraction]
c     istore  = control variable. See "integ" for description
c output:
c     xx at the fitting point:
c     xx(1)   = log10 (density)                               [g/cm**3]
c     xx(2)   = log10 (temperature)                           [K]
c     xx(3)   = log10 (radius)                                [cm]
c     xx(4)   = luminosity / luminosity unit
c     xx(5)   = mass / total stellar mass
c     xx(6)...xx(5+nx) = vector of chemical abundances
c     npts    = number of mass points used through integration
c called
c     center     to calculate all variables at the surface of the
c                innermost sphere
c     integ      to calculate all variables at the fitting point
c............................................................................
c
      call center(rhclg,tclg,xvec,0.d0,0.d0,0.d0,xx)
      call integ(xx,npts,istore)
c                                               --- end of "upintg"
      end
c
c............................................................................
c
      subroutine integ(xx,npts,istore)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
      dimension xx(m2)

c subject:
c     integrate stellar structure equations from the starting point
c     defined by the vector "xs" to the fitting point "fmf" for a chemically
c     homogeneous star
c input:
c     xx     = vector of starting values
c     xx(1)  = log10 (density)                                     (g/cm**3)
c     xx(2)  = log10 (temperature)                                 (K)
c     xx(3)  = log10 (radius)                                      (cm)
c     xx(4)  = luminosity / luminosity unit
c     xx(mm) = mass / total stellar mass
c     xx(mm+1)...xx(mm+nx) = vector of chem. abundances: hydrogen, helium etc
c     istore = control variable
c            = 0 - results of integration will not be stored
c            = 1 - results will be stored in hx(i,2), hx(i,3) and up
c            = 2 - results will be stored in hx(i,nt-1), hx(i,nt-2) and down
c output:
c     xx     = vector of results of integration in the fitting point.
c              all variables have the same meaning as on input
c     npts   = number of integration points (including starting one)
c called
c     step       to make integration steps
c common/hen/
c     hx     = array for (temporary) storing results.
c defined in 'data.s37'
c     nt, mm, m2
c............................................................................

      dimension acc(5)
c                                    --- set upper limits for step size in all
c                                        variables, using those defined in
c                                        'data.s37' as ss1, ss2, ss3, ss4, ss5
c                                        and multiplied by "facss"
      acc(1)=ss1*facss
      acc(2)=ss2*facss
      acc(3)=ss3*facss
      acc(4)=ss4*facss
      acc(5)=ss5*facss
c                                    --- set maximum number of mass points
      nptmax=1000
      if (istore.ne.0) nptmax=nt-2
c                                    --- store starting data in the array "hx"
      if (istore.ne.0) then
         khx=2
         if (istore.eq.2) khx=nt-1
         do 1 i=1,m2
            hx(i,khx)=xx(i)
    1    continue
      end if
c                                    --- switch on nuclear reactions
      inuc=1
c                                    --- carry integrations
      npts=1
    2 continue
         npts=npts+1
         call step(xx,acc,inuc)
c                                    --- store results in the array "hx"
         if (istore.ne.0) then
            if (istore.eq.1) khx=khx+1
            if (istore.eq.2) khx=khx-1
            do 3 i=1,m2
               hx(i,khx)=xx(i)
    3       continue
         end if
c                                    --- check end-of-integration conditions
         if (abs(xx(mm)/fmf-1.0).lt.1.0e-6) return
c                                    --- emergency exit
         if (npts.ge.nptmax) stop ': file "s37.for" - subroutine "sch"
     * - too many mass points'
      go to 2
c                                    --- end of "integ"
      end
c
c..............................................................................
c
      subroutine step(xx,acc,inuc)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      dimension xx(m2),acc(mm)
c subject:
c     make one integration step for a stellar core or envelope with a second
c     order Runge-Kutta method. The steps are carried up to the point xx(5)=fmf
c     Step is positive if on input fmf>xx(5) and negative in the other case
c input:
c     xx(1) = log10 (density)                      [g/cm**3]
c     xx(2) = log10 (temperature)                  [K]
c     xx(3) = log10 (radius)                       [cm]
c     xx(4) = luminosity / luminosity unit
c     xx(mm)= mass / total stellar mass
c     xx(mm+1)...xx(mm+nx) = vector of chem. abundances: hydrogen, helium etc.
c     acc(1)...acc(mm) = upper limits for step size in all variables (positive)
c     inuc  = control variable. If inuc=0 then nuclear reactions are neglected.
c output:
c     xx(i), i=1..m2   at the end of integration step
c called:
c     rhs     to calculate right hand sides of differential equations
c auxiliary variables:
c     h   = integration step size
c     yy(i) = d xx(i) / d xx(mm) as calculated by subroutine rhs
c     xi(i) = values of xx(i) variables at the middle of the integration step
c     extra = vector of extra results produced by "rhs". Not used here.
c predefined in 'data.s37'
c     mm = number of variables (incl. indep. one)
c     m2 = number of variables (incl. indep. one) + number of chemical elements
c.............................................................................

      dimension yy(m2),xs(m2),extra(20)

c                              --- preserve variables at the beginning of step
      do 1 i=1,mm
      xs(i)=xx(i)
    1 continue

      call rhs(xx,yy,extra,inuc)
c                              --- estimate the integration step
      hi=0.
      do 2 i=1,mm
         h=abs(yy(i)/acc(i))
         if (hi.lt.h) hi=h
    2 continue
      h=1./hi
c                              --- choose direction of integration
      if(xx(mm).gt.fmf) h=-h
c                              --- match the fitting point exactly
      if((xx(mm)-fmf)*(xx(mm)+h-fmf) .lt. 0.) h=fmf-xx(mm)

c                              --- make the first half of the integration step
      do 3 i=1,mm
      xx(i)=xs(i)+0.5*h*yy(i)
    3 continue
c                              --- compute right hand sides in the half of step
      call rhs(xx,yy,extra,inuc)
c                              --- make the whole integration step
      do 4 i=1,mm
      xx(i)=xs(i)+h*yy(i)
    4 continue
c                              --- end of "step"
      end
c
c.............................................................................
c
      subroutine rhs(xx,yy,extra,inuc)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      dimension xx(m2),yy(m2),extra(20)
c
c subject:
c     calculate right hand sides of the stellar structure equations
c     for the thermally static case (time-dependent terms not included)
c     If inuc=0 then nuclear reactions are switched off
c input:
c     xx(1) = log10 (density)                      [g/cm**3]
c     xx(2) = log10 (temperature)                  [K]
c     xx(3) = log10 (radius)                       [cm]
c     xx(4) = luminosity / unit luminosity
c     xx(5) = mass / total stellar mass
c     xx(6) = hydrogen content                     [by mass fraction]
c     xx(7) = helium content                       [by mass fraction]
c     xx(8..5+nx) = subsequent elements content    [by mass fraction]
c     inuc  = if inuc=0 then nuclear reactions are not taken into account
c output:
c     yy(1) = d log10 rh/ d xx(5)   attention: this is different than in "rhsh"
c     yy(2) = d log10 t / d xx(5)
c     yy(3) = d log10 r / d xx(5)
c     yy(4) = d l/lunit / d xx(5) = 0. if inuc=0.
c     yy(5) = d xx(5)   / d xx(5) = 1.0
c     yy(6)...yy(m2) = 0.
c     extra = number of additional functions. See "rhsh" for description
c called:
c     state    to calculate thermodynamic functions
c     opact    to calculate opacity (total: radiative and conductive)
c     nburn    to calculate energy generation rate in hydrogen burning
c              + neutrino energy loss rate (if hydrogen content = 0)
c              (if inuc=0 then no call of "nburn")
c common /heninc/
c     flunit  = luminosity unit, close to maximum or surface lumin.  [erg/s]
c     fmtot   = total stellar mass                                   [g]
c auxiliary variables:
c     p      = total pressure                                        [c.g.s.]
c     prad   = radiation pressure                                    [c.g.s.]
c     grad   = d ln t / d ln p     at constant entropy (adiabatic temperature
c                                  gradient)
c     grrad  = d ln t / d ln p     radiative temperature gradient
c     grt    = d ln t / d ln p     temperature gradient in the star
c     grrh   = d ln rh / d ln p    density gradient in the star
c     dpm    = d log10 p / d ( Mr / total mass ) pressure gradient in the star
c.............................................................................

      dimension xvec(nx),dxtvec(nx)

      fcon1   = (flunit/fmtot)/(16*pi*clight*gconst)
      fcon2   = gconst/4./pi*fmtot**2/fln10
      fcon3   = fmtot/4./pi/fln10
      fcon4   = fmtot/flunit

c --------------------------------------------------  ROTATION
      fcon5=2.0d0/3.0d0*com(13)**2/gconst/fmtot
c --------------------------------------------------
      rh=10.**xx(1)
      t=10.**xx(2)
      r=10.**xx(3)
      flr=xx(4)
      fmr=xx(5)

      epsx=0.

      do 1 i=1,nx
      xvec(i)=xx(mm+i)
      dxtvec(i)=0.
    1 continue

      call opact(rh,t,xvec,fkap,opr,opt)
      call state(rh,t,xvec,extra)
      if (inuc.ne.0) call nburn(rh,t,xvec,epsx,dxtvec)
      p    = extra(1)
      pt   = extra(2)
      pr   = extra(3)
c      pgas = extra(4)                 ! not used
c      prad = extra(5)
      grad = extra(6)
c      qt   = extra(7)                 ! not used
c      qr   = extra(8)                 ! not used

c      plog=log10(p)                   ! not used
      frot=1.0d0-fcon5*r**3/fmr     
      prad=arad/3.0*t**4
      grrad=fcon1*fkap*p/prad*flr/fmr/frot              ! ---- WITH ROTATION
      grt=grad
      if(grt.gt.grrad)grt=grrad
      grrh=(1-grt*pt)/pr
      dpm=-fcon2*fmr/r**4/p*frot                        ! ---- WITH ROTATION

      do 2 i=1,m2
      yy(i)=0.
   2  continue

      yy(1)=grrh*dpm
      yy(2)=grt*dpm
      yy(3)=fcon3/(rh*r**3)
      if (inuc.ne.0) yy(4)=fcon4*epsx
      yy(5)=1.
c                                           --- end of "rhs"
      end
c
c..............................................................................
c
      subroutine storeh(starm,telg,fllg,tclg,rhclg,xvec,npts)
      implicit double precision (a-h,o-z)
      include 'data.s37'
      dimension xvec(nx)
      common/hen/ hx(m2,nt), dh(m2,nt), nh

c subject:
c     store on disk the results of model integrations in format convenient
c     for the main evolutionary program
c input:
c     starm   = total stellar mass                           [M Sun]
c     telg    = log10 (surface temperature) in fitted model  [K]
c     fllg    = log10 (surface luminosity)  in fitted model  [L Sun]
c     tclg    = log10 (central temperature) in fitted model  [K]
c     rhclg   = log10 (central density)     in fitted model  [g/cm**3]
c     xvec(1) =     hydrogen content                        [by mass fraction]
c     xvec(2) =     helium-3 content (= 0 in this version)  [by mass fraction]
c     xvec(3) =     helium-4 content                        [by mass fraction]
c     xvec(6) =     nitrogen-14 content                     [by mass fraction]
c     xvec(7) =     oxygen-16 content                       [by mass fraction]
c     npts    = number of mass points needed to store the fitted model
c output:
c     first goes to the common /hen/ in temporary format. Then format is
c     changed to a standard form. The final results are written as a disk file.
c common/hen/ - all mass points
c     hx(1,k) = log10 (density)                              [g/cm**3]
c     hx(2,k) = log10 (temperature)                          [K]
c     hx(3,k) = log10 (radius)                               [cm]
c     hx(4,k) = luminosity / luminosity unit
c     hx(5=mm,k) = mass / total stellar mass
c     hx(6,k) = x = hydrogen content                         [by mass fraction]
c     hx(7,k) =     helium-3 content (= 0 in this version)   [by mass fraction]
c     hx(8,k) =     helium-4 content                         [by mass fraction]
c     hx(11,k)=     nitrogen-14 content                      [by mass fraction]
c     hx(12,k)=     oxygen-16 content                        [by mass fraction]
c common/hen/ - the boundary parameters:
c     hx(1,nh)= telg    = log10 ( surface temperature )      [K]
c     hx(2,nh)= fllg    = log10 ( surface luminosity / solar luminosity )
c     hx(5=mm,nh)= starm   = total stellar mass / solar mass
c     hx(1,1) = tclg    = log10 ( central temperature )      [K]
c     hx(2,1) = rhclg   = log10 ( central density )          [g/cm**3]
c common/hen/ - the other parameters
c     nh      = total number of mass points including two boundary points
c     dh      = not used here
c defined in 'data.s37'
c     nt,mm,m2
c     m2      = the number of variables in every mass zone
c............................................................................

      dimension xe(20),xc(m2)

c                            --- the total number of mass points to store
c                                fitted model is known. If there is no space
c                                enough in the array "hx" then emergency exit
      if (npts.gt.nt-2) then
         write(*,*) 'no space enough in array "hx" to store the model'
         stop ': file "s37.for" - subroutine "storeh" - '
      end if

c                            --- store results of integrations in the array "hx"
c                                on positions 2,3,... and nt-1,nt-2,... resp.
      istore=2
      call dnintg (starm,telg,fllg,xvec,istore,xe,nptdn)
      istore=1
      call upintg (tclg,rhclg,xvec,istore,xc,nptup)

c                            --- shift down results of downward integrations
      nh=nptup+1
      do 2 k=nt-nptdn+1,nt-1
         nh=nh+1
         do 1 i=1,m2
            hx(i,nh)=hx(i,k)
    1    continue
    2 continue
c                            --- store boundary values in array "hx"
      nh=nh+1
      do 3 i=1,m2
         hx(i,1)=0.
         hx(i,nh)=0.
    3 continue
      do 4 i=1,nx
         hx(mm+i,1)=xvec(i)
    4 continue
      hx(1,1)=rhclg
      hx(2,1)=tclg
      hx(1,nh)=telg
      hx(2,nh)=fllg
      hx(mm,nh)=starm
c                            --- renormalize luminosity in every mass zone:
      nh1=nh-1
      faclum=hx(4,nh1)
      flunit=flunit*faclum
      do 5 k=2,nh1
         hx(4,k)=hx(4,k)/faclum
    5 continue
c                            --- redistribute mass zones so, as to make them
c                                about the same width in units of ss(i).
      call grid1(2,nh-1)
         mp(1)  = 2
         mp(18) = nh
         mp(19) = nh
         mp(20) = nh-1
c                            --- write the model as disk file
      call writeh
c                            --- end of "storeh"
      end
c.............................................................................

c this is a good place to include some extra code

      include 'envelope.s37'
      include 'center.s37'
      include 'eostate.s37'
      include 'opac.s37'
      include 'nuclear.s37'
      include 'commonsb.s37'

      subroutine stupid
c This is stupid but NDP Fortran compiler needs here the "end" statement
      end
c..............................................................................
c        This file contains "Schwarzschild-type" program "s37" -
c         main program and nine subroutines called:
c               - "setsch" -
c               - "sch"    -
c               - "solve"  -
c               - "dnintg" -
c               - "upintg" -
c               - "integ"  -
c               - "step"   -
c               - "rhs"    -
c               - "storeh" - 
c                                                    end of the file "s37.for"
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
