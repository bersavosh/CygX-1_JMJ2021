C     Last change:  AAP  09 December 2008
c                   changing filenames of MOD and OSC files,
c                   changing all block names to *.s37 and so on. 
C     Last change:  AAP  25 November 2008
c                   new treatment of overshooting 
c                   prepare file OSC inside the program (without program ARP13)
c
cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
c
c  Begin of the file "h37.for"
c NEW TREATMENT OF OVERSHOOTING

c   name:         subject:
c   -----         --------
c
c   main
c  program - SUBROUTINES CALLED:
c
c             - "readh"  - (included - file "commonsb.s37")
c                          reads a model from the disk file "mod#####"
c             - "grid1"  - (included - file "commonsb.s37")
c                          redistributes mass points
c             - "extrap" -
c             - "profil" -
c             - "hsolve" -
c             - "addcor" -
c             - "writeh" - (included - file "commonsb.s37")
c                          writes a model on the disk file "mod#####"
c             - "modout" - writes data for stellar pulsation
c
c "extrap" - estimates new time step
c
c "profil" - estimates new hydrogen and helium profiles due to
c            nuclear burning ( + mixing, if convective core exists)
c            SUBROUTINES CALLED:
c             - "rhsh"
c             - "nburn"   (included - file "nuclear.s37")
c
c "hsolve" - calculates time changes of all variables using a "Henyey-scheme"
c            SUBROUTINES CALLED:
c             - "env"     (included - file "envelope.s37")
c             - "deriv"
c             - "center"  (included - file "center.s37")
c
c "deriv"  - calculates the right hand sides of stellar structure equations
c            and their derivatives
c            SUBROUTINES CALLED:
c             - "rhsh"
c
c "rhsh"   - calculates the right hand sides of stellar structure equations
c            SUBROUTINES CALLED:
c             - "state"   (included - file "eostate.s37")
c             - "opact"   (included - file "opacity.s37")
c             - "nburn"   (included - file "nuclear.s37")
c
c "addcor" - adds the time changes to all variables.
c
c "modout"   - writes data for stellar pulsation.
c            SUBROUTINES CALLED:
c             - "env"     (included - file "envelope.s37")
c             - "corout"
c             - "env"     (included - file "envelope.s37")
c
c "corout"   - writes data for stellar pulsation.
c            SUBROUTINES CALLED:
c             - "rhsh"
c             - "drorsu"
c             - "dgatsu"
c
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
c  disk file:             subject:
c  ----------             --------
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
c "opacity.s37"  - reads and interpolates coefficient of opacity from
c                  the Los Alamos radiative opacity tables with conductive
c                  opacities and Alexander's molecular opacities added
c                  (the disk files "opint1"..."opint7" are used in the
c                  internal part of a model and the disk file "envopa"
c                  is used for the envelope model calculations)
c "nuclear.s37"  - calculates nuclear reaction rates for p-p chain and CN cycle
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
      logical wind
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
      common /comkappa/ ideriv
C     NEWOVER
      COMMON/overshoot/INHO,ICON,xpri(5,1000),qov
c      dimension outbur(20)
      dimension outbur(20),xvec(nx)
c                                     --- opening file for intermediate results
      iout2=2
      ideriv=0
c
c      OPEN(33,FILE='inho',STATUS='unknown')
c      write (33,888) alfaov,wykla
c  888 format('alfaov, wykla = ',0p2f8.2)
c      write (33,889)
c  889 format('**********************************')
c
      open(iout2,file='evol.h37',status='unknown')
      rewind(iout2)
c                                     --- Reading model. Asking about file name
c                                         Unit No 1 opened inside, then closed.
c ------------------------------------------
      call readh(iout2)
      endfile (iout2)
c ------------------------------------------
c                                     --- Screen input
  222 format(' number of models to be calculated,         nofmod= ')
  223 format(' every nwrmod`th model data will be stored, nwrmod= ')
  224 format(' every nwrosc`th model data will be store
     # for oscillations, nwrosc= ')
  225 format(/,' Thanks ! - end of screen input - now, wait...')
      write(*,222)
      read(*,*)nofmod
      write(*,223)
      read(*,*)nwrmod
      write(*,224)
      read(*,*)nwrosc
      write(*,225) 

   98 format('wfac=')
      write(*,98)
      read(*,*)wfac

c                                      --- end of input
c
c ..................................   --- begin of  N E W   M O D E L
      do 3 istep=1,nofmod
      model=model+1
      nh1=nh-1
      ifact=0

c ---------------------------- grid1:    "hx" used
      n1=mp(1)
c
c      if (model.gt.1) then
c         n2=mp(2)
c         call grid1(n1,n2)
c         n1=n2
c      endif
c      n2=mp(20)
c      call grid1(n1,n2)
c
      do 93 i=1,20
      n2=mp(i)
      if(n2.le.n1.or.n2.gt.mp(20))go to 93
      call grid1(n1,n2)
      n1=n2
   93 continue
c ---------------------------- grid1:     NEW "nh", "hx" and "dh" redistributed


      call tefref
      tefflg=com(16)
      refflg=com(17)
      fllg=hx(2,nh)
      
      z=1.-hx(mm+1,nh-1)-hx(mm+3,nh-1)

      zet=z/0.02

      reff=10**(refflg-log10(sunr))
      dmdt=-14.0177+0.5*log10(zet)+0.81*(refflg-log10(sunr))+1.24*fllg+
     *              0.16*log10(fmtot/sunm)
      dmdt=10**dmdt

      dmdt=dmdt*wfac

cc      write(*,99)nh,dmdt,reff,tefflg,refflg,fllg,fmtot/sunm
   99 format(i5,e15.6,8f12.4)


c ---------------------------- extrap:  "hx" and "dh" used
      call extrap
c ---------------------------- extrap:   NEW "dtime", "dh" set to 0.

    4 continue
      dtimeo=dtime
      fmtoto=fmtot
      wind=.true.

      if(wind)then                       ! MASS LOSS ...............
      nho=nh
                   delm=dmdt/year*sunm
                   delm=delm*dtime
      fmtot=fmtoto-delm

cc      write(*,99)nh,dmdt,delm/sunm,fmtoto/sunm,fmtot/sunm
cccc      stop

      starm=fmtot
      hx(mm,nh)=fmtot/sunm
      fmbot=(1-fmenv)*fmtot/fmtoto

cc      write(*,99)nh,dmdt,(hx(mm,l),l=nh-5,nh),fmbot

      do kk=3,nh-1
      hk1=hx(mm,kk-1)
      hk =hx(mm,kk)
      if(fmbot.ge.hk1.and.fmbot.le.hk)then
      facm=(fmbot-hk1)/(hk-hk1)
      do 741 ii=1,m2
      hx(ii,kk)=hx(ii,kk-1)*(1-facm)+hx(ii,kk)*facm
  741 continue
      nh=kk+1

   90 format(i5,e15.6,i5,8f12.4)

cc      write(*,90)nh,dmdt,kk,hk,hk1,facm,hx(mm,kk-1),hx(mm,kk),fmbot
cc      write(*,99)nh,dmdt,(hx(mm,l),l=nh-5,nh)


      go to 742 
      end if
      enddo
  742 continue
cccc  write(*,99)nh,dmdt,hk,hk1,facm,hx(mm,nh-1),hx(mm,nh)
cc      write(*,99)nh,dmdt,(hx(mm,l),l=nh-5,nh)

cc      write(*,99)nh,sunm,reff,fmbot,refflg,fllg,fmtot/sunm

      do 743 kk=1,nh-1
  743 hx(mm,kk)= hx(mm,kk)*fmtoto/fmtot 
c Warning:  Used here are the actual meanings of "surf1" = hx(1,nh) = telg,
c                                            and "surf2" = hx(2,nh) = fllg
      hx(1,nh) = hx(1,nho)
      hx(2,nh) = hx(2,nho)
      hx(mm,nh)=fmtot/sunm

       do 64 iii=1,90
        if(mp(iii).ge.nh-1)then
         if(mp(iii).le.nho-1)then
          mp(iii)=nh-1
         else
          mp(iii)=nh
         end if
        end if
   64  continue

c ----------------------------    NEW: fmtot,nh,mp(), hx(mm,2...nh)
cccc        write(*,*)' new mass=',hx(mm,nh)
      end if                                    ! MASS LOSS ............... 


cccc      write(*,99)nh,dmdt,hx(mm,nh-1),fmbot,refflg,fllg,fmtot/sunm
cccc      write(*,99)nho,dmdt,hx(1,nh),hx(2,nh),tefflg,fllg,fmtoto/sunm
cccc      write(*,99)nh,dmdt,(hx(mm,l),l=nh-5,nh-1)

cccc      write(*,99)nh,dmdt,hx(mm,nh-1),fmbot,refflg,fllg,fmtot/sunm

cc      write(*,99)nh,fmtot,hx(mm,nh-1),hx(mm,nh),fmbot,refflg,fllg,
cc     * fmtot/sunm




c ---------------------------- profil:  "hx" used
      call profil(outbur)
c ---------------------------- profil:   NEW "dh" calculated (H and He-4, only)

      acc=1.0d-8
      iterat=0
      iterm=20
c                              * * * * * *        --- begin of iteration loop
    1 continue
      iterat=iterat+1
      if(iterat.gt.iterm)go to 2

c ---------------------------- hsolve:  "hx" + "dh" used
      call hsolve(dismax,delmax)
c ---------------------------- hsolve:         "dh" corrected

c      write(*    ,100)model,iterat,dismax,delmax
  100 format('istep,iter,dismax,delmax=',2i5,2f20.15)
      if(dismax.gt.acc.or.delmax.gt.acc) go to 1

c                              * * * * * *        --- end of iteration loop

c ---------------------------- addcor:  "hx" = "hx" + "dh"
      call addcor
c ---------------------------- addcor:  time = time + dtime
c                              addcor: luminosity re-normalized

c  . . . . . . . . . . . . . .           begin of screen & disk  output:
      tilog=0.
      dtlog=0.
      if (time.ne.0.) tilog=log10(time/year)
      if (dtime.ne.0.)dtlog=log10(dtime/year)
      rhclg=hx(1,1)
      tclg=hx(2,1)
      telg=hx(1,nh)
      fllg=hx(2,nh)
      xc =outbur(1)
      xn14c=hx(11,1)
      xo16c=hx(12,1)
c      fmc=outbur(2)*hx(mm,nh)
c      if(outbur(3).ne.0.d0)fmc=outbur(3)*hx(mm,nh)
      fmc=outbur(2)
      if(outbur(3).ne.0.d0)fmc=outbur(3)
      gogcen=outbur(5)
c
      do k=2,nh-1
         do i=1,nx
            xvec(i)=hx(mm+i,k)
         enddo
      enddo
c ---------------------------- tefref:  compute and save Teff, Reff, Rsurf
      call tefref
c ---------------------------- tefref
      tefflg=com(16)
      refflg=com(17)
      reflgs=refflg-dlog10(sunr)
      gravlg=gconst*fmtot/10.0d0**(2.0d0*refflg)
      gravlg=gravlg-2.0d0/3.0d0*com(13)**2*10.0d0**refflg
      gravlg=dlog10(gravlg)

cc      reff=10**reflgs
      dmdt=-14.0177+0.5*log10(zet)+0.81*reflgs+1.24*fllg+
     *              0.16*log10(fmtot/sunm)
      dmdt=10**dmdt                 
      dmdt=dmdt*wfac
                             

c
      vrot=com(13)*10.0d0**refflg/1.0d5
c ===
c model   = sequential model number
c iterat  = number of iterations
c nh      = number of mass points
c tilog   = lg(t)  = log10 (time)                      [yr]
c dtlog   = lgDt   = log10 (time step)                 [yr]
c tefflg  = lgTeff = log10 ( effective temperature )   [K]
c refflg  = lgReff = log10 ( effective radius )        [cm]
c gravlg  = lg g   = log10 ( gravity at Reff)          [cm/sec**2]
c telg    = lgTsurf= log10 ( surface temperature )     [K]
c fllg    = lg L   = log10 ( surface luminosity / solar luminosity )
c tclg    = lgTc   = log10 ( central temperature )     [K]
c rhclg   = lgRhoc = log10 ( central density )         [g/cm**3]
c xc      = Xc     = central hydrogen content
c fmc     = Mc/M   = fractional mass of convective or helium core
c gogcen  = central radiative/adiabatic gradient ratio
c xn14c   = N14c   = central N14 content (in mass)
c xo16c   = O16c   = central O16 content (in mass)

      if(model.eq.1)write(*,307)
      if(model.eq.1)write(*,*)
      if(model.eq.1)dtime=year
      write(*    ,101)model,tilog,telg,tefflg,fllg,reflgs,
     *                tclg,rhclg,xc,fmc,qov,vrot,xn14c,xo16c,
     *                log10(dtime/year), fmtot/sunm,dmdt

c      write(*,*)
  101 format(i5,f10.6,f7.4,f8.5,4f7.4,f10.7,2f7.4,f6.1,3f8.4,f9.4,e15.4)
  307 format(/,'  MOD   lg(t)   lgTsur  lgTeff  lg L  lgReff',
     *     '  lgTc  lgRhoc    Xc     Mc/M   Mov/M    Vrot  N14     O16'
     *         '   lg(dtime)  Mtot        dM/dt')
c ===
      backspace(iout2)
      write(iout2,101)model,tilog,telg,tefflg,fllg,reflgs,
     *                tclg,rhclg,xc,fmc,qov,vrot,xn14c,xo16c,
     *                log10(dtime/year), fmtot/sunm,dmdt
      endfile(iout2)
c ===
      if (istep/nwrosc*nwrosc.eq.istep) then
c ------ NEWOVER
c         write (33,333) model
c333      format(I5,1P5E13.3)
c         do i=icon-3,inho+25
c            write (33,333) i,(xpri(k,i),k=1,5)
c         enddo
c ------
         ideriv=1
c ---------------------------- modout: data for oscillation computation
         call modout
c ---------------------------- modout
         ideriv=0
      endif
c      
c ---------------------------- newomeg: compute Omega for next model
      call newomeg
c ---------------------------- newomeg:
c
c ------------------------------------------------- writeh
      if (istep/nwrmod*nwrmod.eq.istep) THEN
        call writeh
      endif
c ------------------------------------------------- writeh
c  . . . . . . . . . . . . . .             end of screen & disk  output

c ..................................   --- end of  N E W   M O D E L
      go to 3

    2 continue
c ---------------------------
  102 FORMAT(/1X,' Henyey iterations DO NOT CONVERGE:',
     *           ' DECREASE the TIME STEP,  IFACT =',I2)
      ifact=ifact+1
      write(*    ,102)ifact
      write(iout2,102)ifact
      if (ifact.gt.5)stop ': file "h37.for" - time step
     * decreased many times !...'
      dtime=dtime*0.5d0
c                           --- set all time changes = 0
      do 44 k=1,nh
      do 33 i=1,m2
         dh(i,k)=0.
   33 continue
   44 continue
      go to 4
c ---------------------------

    3 continue


c                                    --- end of main program
      stop '=> h37 - okay'
      end


c..............................................................................
c..............................................................................
c
      subroutine extrap
      implicit double precision (a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
c
c subject:
c     establish new time step. Put time changes to the model, "dh" = 0.
c output:
c common /heninc/
c     dtime = "old" time step on input and "new" on output     [s]
c     model = sequential model no
c     fmtot = total stellar mass                               [g]
c     flunit= luminosity unit                                  [erg/s]
c common /hen/
c     nh - 2   = number of mass points
c     time step extrapolation is basing on time changes "dh (1..m2,2..nh-1)"
c     (excluding "dh(mm,2..nh-1)) to the model.
c     On output all the time changes are set to 0.
c predefined in 'data.s37'
c     sunm
c     mm, m2
c     m2       is also dimension of the vector "st"
c     (st1..st8)*facst = limits for time differences
c............................................................................

      dimension st(m2)

      nh1=nh-1

      st(1)=st1 * facst
      st(2)=st2 * facst
      st(3)=st3 * facst
      st(4)=st4 * facst
      st(5)=st5 * facst
      st(6)=st6 * facst
      st(7)=st7 * facst
      st(8)=st8 * facst
      st(9)=st9 * facst
      st(10)=st10 * facst
      st(11)=st11 * facst
      st(12)=st12 * facst
c                           --- find maximum time difference in units of "st"
      ftinv=0.
      do 2 k=2,nh1
      do 1 i=1,m2
         if (i.ne.mm) ft=abs(dh(i,k)/st(i))
         if (ft.gt.ftinv) then
            ftinv=ft
         end if
    1 continue
    2 continue
c                           --- if all time changes are 0 then time step will
c                               not be changed
      if (abs(ftinv).lt.1.d-99) ftinv=1.
c                           --- if central hydrogen abundance > "endhra" then
c                               it should not fade faster than by half of value
c                               during single time step. For convective cores
c                               "endhra" may have to be replaced with "endhco".
      xcen=hx(mm+1,2)
      if(xcen.gt.endhra) then
         dxcen=abs(dh(mm+1,2))
         ftlim=2.*dxcen/xcen
         if (ftinv.lt.ftlim) ftinv=ftlim
      end if

      facdt=1./ftinv
c                           --- growth of time step will be limited to the
c                               factor 1.5 (but cutting down is off limits)
      if (facdt.gt.1.5) facdt=1.5
c                           --- estimate the new time step:
      dtime=dtime*facdt

c                           --- apply main sequence time scale of evolution
c                               if the time step not established yet
      if (abs(dtime).lt.1.d-99.and.model.gt.1) then
         dtime=1.0e7/(fmtot/sunm)**2*year
      end if
c                           --- set all time changes = 0
c                               (they have been added earlier)
      do 4 k=1,nh
      do 3 i=1,m2
         dh(i,k)=0.
    3 continue
    4 continue
c                           --- end of "extrap"
      end
c
c............................................................................
c
      subroutine profil(out)
      implicit double precision (a-h,o-z)
      include 'data.s37'
      dimension out(20)
      DIMENSION stir(1000)
      common/hen/ hx(m2,nt), dh(m2,nt), nh
cNEWOVER
      COMMON/overshoot/INHO,ICON,xpri(5,1000),qov
c
c subject:
c     hydrogen burning.
c     hydrogen mixing in the convective core
c
c     dxtpp     = hydrogen    depletion rate   (the pp chain)    [1/s]
c     dxtcn     = hydrogen    depletion rate  (the CNO cycle)    [1/s]
c     dxtvec(1) = hydrogen    depletion rate  ( = dxtpp + dxtcn) [1/s]
c     dxtvec(2) = helium-3    depletion rate  (= 0 in this version)  [1/s]
c     dxtvec(6) = dh6/dtime = N14 depletion rate  (the CNO cycle)    [1/s]
c     dxtvec(7) = dh7/dtime = O16 depletion rate  (the CNO cycle)    [1/s]
c output:
c     out(1)= xcen   = new central hydrogen content
c     out(2)= fmccor = convective core mass / total stellar mass
c     out(3)= fmheco =     helium core mass / total stellar mass
c     out(4)= dgogx  = derivative of gradients ratio over hydrogen content at
c                      the boundary of the convective core on the outer side.
c                      This is the test for semiconvection occurence just above
c                      the convective core
c     out(5)= gogcen = central radiative/adiabatic gradient ratio
c----------------------------------------------------------------------------
c IMPORTANT: future ("new") convective core and chemical profiles
c            (for H and He-4 only) are predicted on the basis of actual model.
c            Boundary of the convective core is found by applying
c            the Schwarzschild stability criterion.
c----------------------------------------------------------------------------
c predefined in 'data.s37'
c     nt,mm,nx,m2,ahe3,endhra,endhco,tenuc
c common /heninc/
c     dtime,zp
c............................................................................

      dimension xvec(nx), dxtvec(nx), xx(m2), yy(m2), extra(20)
      dimension xvecme(nx), dxtmea(nx)

      nh1=nh-1
c                                    --- set some default values
      dgogx=0.
      gogcen=0.
      fmccor=0.
      fmheco=0.
      icom(18)=2
      kccore=2
      do i=1,m2
         xx(i)=hx(i,2)
      enddo
      call rhsh(xx,yy,0.d0,0.d0,0.d0,extra)
      gogcen=extra(20)
      gogp=gogcen
c                                    --- dh(mm+1...nx,2...nh1) stay = 0 ,
c                                        if dtime = 0
c ************  MODIFICATION 8.12.96 (do not consider this 'if...'): BEGIN 
c      if (dtime.lt.1.d-99) then
c         write(*,*)' dtime=0, fmc is not estimated'
c         go to 130
c      end if
c ************  MODIFICATION 8.12.96 (do not consider this 'if...'): END
c    
      

c                          --- search for helium core boundary
      khecor=1
    1 khecor=khecor+1
      if (khecor.eq.nh1) stop ': file "h37.for" - subroutine "profil"
     * - pure helium star ...'
      if (hx(mm+1,khecor).lt.endhco) go to 1
      khecor=khecor-1
      khecor=max(khecor,mp(3))
c      if (khecor.gt.1) fmheco=0.5*(hx(mm,khecor)+hx(mm,khecor+1))
c      if (khecor.gt.1) go to 10
      if (khecor.gt.1) then
         fmheco=0.5*(hx(mm,khecor)+hx(mm,khecor+1))
c                                    --- there is only He4 and metals
c                                        in the hydrogen-exhausted core.
c                                        There are no longer chemical changes
c                                        due to hydrogen burning
         nd=max(mp(3),1)
         do nkk=nd,khecor
            hx(mm+3,nkk)=hx(mm+1,nkk)+hx(mm+2,nkk)+hx(mm+3,nkk)
            hx(mm+1,nkk)=0.
            hx(mm+2,nkk)=0.
            do inx=1,nx
               dh(mm+inx,nkk)=0.
            enddo
         enddo
c                                    --- set new values of MAIN POINTS
         mp(2)=-1
         mp(3)=khecor
c                                    --- check if there is convection
c                                        in the helium core. If so,
c                                        search for convective core boundary
c                                        by applying the Schwarzschild
c                                        stability criterion.
         do k=2,nh1
            do i=1,m2
               xx(i)=hx(i,k)
            enddo
            call rhsh(xx,yy,0.d0,0.d0,0.d0,extra)
            if (k.eq.2.and.extra(20).lt.1.) go to 10
            kccore=k
            if (extra(20).lt.1.) go to 55
         enddo
         stop 'fully convective helium star'
c
   55    fmccor=hx(mm,kccore)
         mp(2)=kccore
         if(mp(2).gt.mp(3)) stop 'convective core > helium core'
c
         go to 10
      end if
c                                   --- no helium core (khecor = 1). Now,
c                                       check for convection in central region.
c                                       kccore=2 means:"no central convection"
      kccore=2
      mp(2)=kccore
      if (gogcen.lt.1.) go to 10
c                                    --- there is convection in central region.
c                                        Search for convective core boundary
c                                        by applying the Schwarzschild
c                                        stability criterion.
      do 4 k=2,nh1
         do 3 i=1,m2
            xx(i)=hx(i,k)
    3    continue
         call rhsh(xx,yy,0.d0,0.d0,0.d0,extra)
         gog=extra(20)
         if (gog.lt.1.) go to 5
         kccore=k
         gogp=gog
    4 continue
      if (kccore.eq.nh1) stop ': file "h37.for" - subroutine "profil"
     * - fully convective star ...'

c                               --- convective core boundary is interpolated
c                                   to the point where radiative temp. gradient
c                                   is equal to the adiabatic one.

    5 fac=(gogp-1.0)/(gogp-gog)
      fmccor=(1.0-fac)*hx(mm,kccore)+fac*hx(mm,kccore+1)
c
      k=kccore
      if ((hx(mm,k+1)-fmccor).lt.(fmccor-hx(mm,k))) k=kccore+1
c
      do 51 i=1,m2
      hx(i,k)=(1.0-fac)*hx(i,kccore)+fac*hx(i,kccore+1)
      dh(i,k)=(1.0-fac)*dh(i,kccore)+fac*dh(i,kccore+1)
   51 continue
c
      kccore=k
      icom(18) = kccore
      mp(2)=kccore
      if(mp(2).gt.mp(3))  mp(3)=-1
c ------------------------ approximately, subroutine MIX of "04" version: BEGIN
c
c                                   --- calculation of mean content of elements
c                                       in the convective core
      do 71 i=1,nx
      xvecme(i)=0.d0
   71 continue

      do 81 k=2,kccore

      do 72 i=1,nx
      xvec(i)=hx(mm+i,k)
      if(xvec(i).lt.0.d0)stop ': file "h37.for" - subroutine "profil"
     * - negative chemical abundance(s) ...'
   72 continue
                         dm=0.5d0*(hx(mm,k+1)   -hx(mm,k-1))
         if(k.eq.2)      dm=0.5d0*(hx(mm,2)     +hx(mm,3))
         if(k.eq.kccore) dm=0.5d0*(hx(mm,kccore)-hx(mm,kccore-1))
      do 73 i=1,nx
      xvecme(i) = xvecme(i) + xvec(i)*dm
   73 continue
   81 continue

      do 74 i=1,nx
      xvecme(i) = xvecme(i)/fmccor
   74 continue
      if(xvecme(1).lt.endhco)xvecme(1)=0.d0
c
c                                   --- calculation of mean burning rates
c                                       in the convective core
      do 75 i=1,nx
      dxtmea(i)=0.d0
   75 continue
      ppmea=0.
      cnmea=0.

      do 8 k=2,kccore
         do 82 i=1,nx
            dxtvec(i)=0.
   82    continue
         dxtpp=0.
         dxtcn=0.
         rh=10.d0**hx(1,k)
         t =10.d0**hx(2,k)
c
         if(t.lt.tenuc)go to 83
         call nburn(rh,t,xvecme,epsx,dxtvec)
         call deltano(.true.,rh,t,xvecme(1),xvecme,dh6,dh7)
c ************  MODIFICATION 2.02.07 (treat dtime=0 case to avoid 0/0): BEGIN
         if (dtime.gt.0.0) then
            dxtvec(6) = dh6/dtime
            dxtvec(7) = dh7/dtime
         else
            dxtvec(6) = 0.0
            dxtvec(7) = 0.0
         endif
c ************  MODIFICATION 2.02.07 (treat dtime=0 case to avoid 0/0): END
   83    continue
                         dm=0.5d0*(hx(mm,k+1)   -hx(mm,k-1))
         if(k.eq.2)      dm=0.5d0*(hx(mm,2)     +hx(mm,3))
         if(k.eq.kccore) dm=0.5d0*(hx(mm,kccore)-hx(mm,kccore-1))
c
         do 76 i=1,nx
         dxtmea(i) = dxtmea(i) + dxtvec(i)*dm
   76    continue
         ppmea=ppmea+com(19)*dm
         cnmea=cnmea+com(20)*dm
c         ppmea=ppmea - dxtpp/xvecme(1)**2     *dm         ! from "04"
c         cnmea=cnmea - dxtcn/xvecme(1)        *dm         ! from "04"
    8 continue

      do 77 i=1,nx
      dxtmea(i) = dxtmea(i)/fmccor
   77 continue
      ppmea=ppmea/fmccor
      cnmea=cnmea/fmccor
c
c WARNING: "ppmea" and "cnmea" are > 0., "cnmea" HAS ALWAYS to be > 0.
c
c                                   --- calculation of time changes of elements
c                                       in the convective core
      if(xvecme(1).lt.endhco)then
        xnew = 0.d0
      else
      if(abs(xvecme(1)/2.).gt.abs(dxtmea(1)*dtime))then
        xnew = xvecme(1) + dxtmea(1)*dtime
      else
c------------------------------------------------
c  analytical integration of eq.: - dx/dt=ppmea*x**2+cnmea*x,(x=hydr. content),
c  ppmea, cnmea are assumed to be, approximately, independent on "x".
c  This is a good aproximation only if the PPI chain
c  or the CNO cycle dominate and screening corrections are negligible.
c  Because if it, we use this way only in these extreme cases when time
c  changes of x are too large to be calculated in a standard manner.

        xmea = xvecme(1)
        if(dtime*(ppmea*xmea+cnmea).lt.1.d-16)then
          xnew=xmea
        else
          if(cnmea*dtime.gt.1.d-8)then
            xnew=cnmea/((ppmea+cnmea/xmea)*exp(cnmea*dtime)-ppmea)
          else
            xnew=xmea/(1.d0+dtime*(ppmea*xmea+cnmea))
          end if
        end if
        if(xnew.gt..99*xmea.or.xnew.le.0.)stop 'strange xnew'
c------------------------------------------------
      end if
      end if
      if (xnew.lt.endhco) xnew=0.d0

c ************  MODIFICATION 8.12.96 (added to avoid stop for ZAMS): BEGIN 
      if (dtime.lt.1.d-99) go to 10
c ************  MODIFICATION 8.12.96 (added to avoid stop for ZAMS): END
      do 9 k=2,kccore
         dh(mm+1,k) = xnew - hx(mm+1,k)
         dh(mm+3,k) = - dh(mm+1,k)
         if(hx(mm+3,k)+dh(mm+3,k).lt.0.)stop 'He4<0'
         do 91 inx=4,nx
c            if(xvecme(inx)+dxtmea(inx)*dtime.lt.0.)stop 'inx el.<0'
            if(xvecme(inx)+dxtmea(inx)*dtime.lt.0.) then
              write (*,999) inx,xvecme(inx),dxtmea(inx)
  999         format(' inx,xvecme(inx),dxtmea(inx):',i3,1p2d15.4)
	      stop 'inx el.<0'
            endif
            dh(mm+inx,k)=xvecme(inx)+dxtmea(inx)*dtime-hx(mm+inx,k)
   91    continue
    9 continue
c                                   --- element mixing and burning in the
c                                       convective core has been calculated
c
c ------------------------ approximately, subroutine MIX of "04" version: END
c
   10 continue
c                                   --- calculate changes of chemical compo-
c                                       sition outside the convective or helium
c                                       core
      k1=2
      if (kccore.gt.2) k1=kccore+1
      if (khecor.ge.2) k1=khecor+1

      do 12 k=k1,nh1
         do 84 i=1,nx
            dxtvec(i) =0.
            dh(mm+i,k)=0.
   84    continue
         dxtpp=0.
         dxtcn=0.
         rh=10.**hx(1,k)
         t=10.**hx(2,k)
c                                   --- set dh(mm+1...nx,k) = 0
c                                       if temperature is too low
c                                       for nuclear burning
         if(t.lt.tenuc)go to 12
c
         do 11 i=1,nx
            xvec(i)=hx(mm+i,k)
   11    continue
         if(xvec(1).lt.endhco)xvec(1)=0.d0
         call nburn(rh,t,xvec,epsx,dxtvec)
         x=xvec(1)
         aq=com(19)
         bq=com(20)
c
c WARNING: "aq" and "bq" are > 0., "bq" HAS ALWAYS to be > 0.
c
         if(x.lt.endhco)then
           dx=0.
         else
           if(abs(dxtvec(1)*dtime).lt.abs(x/2.))then
             dx=dxtvec(1)*dtime
           else
c------------------------------------------------
c  analytical integration of eq.: - dx/dt=aq*x**2+bq*x, (x=hydr. content)
c  aq, bq are assumed to be, approximately, independent on "x".
c  This is a good aproximation only if the PPI chain
c  or the CNO cycle dominate and screening corrections are negligible.
c  Because if it, we use this way only in these extreme cases when time
c  changes of x are too large to be calculated in a standard manner.

c           aq=-dxtpp/x/x                                   ! from "04"
c           bq=-dxtcn/x                                     ! from "04"
c  Notice: aq and bq are always > 0.

             if(bq+aq.lt.1.d-29.or.dtime*(aq*x+bq).lt.1.d-12)then
               dx=0.
             else
               if(bq*dtime.gt.1.d-8)then
                 dx=bq/((aq+bq/x)*exp(bq*dtime)-aq)-x
               else
                 dx=x/(1.d0+dtime*(aq*x+bq))-x
               end if
             end if
c------------------------------------------------
           end if
           if(x+dx.lt.endhra)dx=-x
         end if
         if(dx.gt.0.d0)stop ': file "h37.for" - subroutine "profil"
     * - delta(X) > 0'

         dh(mm+1,k)= dx
         dh(mm+3,k)=-dx
c                                   --- now, calculate changes of N14 and O16
c                                       using partly implicite method, as the
c                                       H-content is averaged over the timestep
c
         call deltano(.false.,rh,t,xvec(1)+dx/2.,xvec,dh6,dh7)
         dh(mm+6,k) = dh6
         dh(mm+7,k) = dh7
   12 continue
      k=kccore
c
c      write (33,773) model,dh(mm+1,kccore)
c  773 format(' model,dh(mm+1,kccore):',
c     *         i4,1p1e10.2)
c
      IF(dh(mm+1,kccore).GT.-1.0e-4) GO to 444
c      IF(dh(mm+1,kccore).GT.0.0) GO to 444
c
c NEWOVER
c !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c Calculation of xc0,qov,xov,dxov=d(xov)/d(q),
c                kover = closest point to qov for which q(kover)<qov
c
      ICON=kccore
      kover=kccore    ! initialization
c
      do i=1,m2
         xx(i)=hx(i,kccore)
      enddo
c *****************************************
      call rhsh(xx,yy,0.d0,0.d0,0.d0,extra)
c *****************************************
      rhocor=10.**xx(1)
      rcor=10.**xx(3)
      pcor=extra(1)
      grav=gconst*fmccor*fmtot/rcor**2
      hpconv=pcor/grav/rhocor
      dov=alfaov*min(hpconv,rcor)
      dmov=4.0*pi*rcor**2*rhocor*dov
      fmov=fmccor+dmov/fmtot
      fmrp=fmccor
c
      do k=kccore+1,nh1
         fmr=hx(mm,k)
         if (fmrp.lt.fmov.and.fmr.gt.fmov) then
            kover=k-1
            qov=fmov
            xov=hx(mm+1,kover)+dh(mm+1,kover)
            xov1=hx(mm+1,k)+dh(mm+1,k)
            dxov=(xov1-xov)/(fmr-fmrp)
            xov=xov+dxov*(fmov-fmrp)
            go to 411
         endif
         fmrp=fmr
      enddo
c
  411 continue
      qpre=fmccor
      xcore=hx(mm+1,kccore)+dh(mm+1,kccore)
      xc0=xcore
      qxc=xc0*fmccor
      xlfint=0.0
      xlfp=xc0
      fqq1int=0.0
      fqq1p=1.0
c
      qq=qov-fmccor                ! = Q in equations
      kmixt=kover
      do kmix=kccore+1,kmixt
         qcur=hx(mm,kmix)
         dq=qcur-qpre
         yqq=(qcur-fmccor)/qq      ! = y in equations
         yw=yqq**wykla
         fqq1=1.0-yw*(wykla+1.0-wykla*yqq)
         fqq2=yw*(xov*(wykla+1.0-wykla*yqq)-qq*dxov*(1.0-yqq))
         xloc=hx(mm+1,kmix)+dh(mm+1,kmix)
         xlf=xloc-fqq2 
         xlfint=xlfint+0.5*(xlf+xlfp)*dq
         fqq1int=fqq1int+0.5*(fqq1+fqq1p)*dq
         xlfp=xlf
         fqq1p=fqq1
         qpre=qcur
      enddo
      xcore=(qxc+xlfint)/(fmccor+fqq1int)
      dxcore=xcore-xc0
c
      xlocs=(hx(mm+1,kccore)+dh(mm+1,kccore))*fmccor
      xsum=xcore*fmccor
c      dmixp=hx(mm+1,kccore)+dh(mm+1,kccore)
      qpre=0
      do k=2,kmixt
         qcur=hx(mm,k)
         dq=qcur-qpre
         xloc=hx(mm+1,k)+dh(mm+1,k)
         if(k.le.kccore) then
           stir(k)=1.0
           dh(mm+1,k)=dh(mm+1,k)+dxcore
           dh(mm+3,k)=dh(mm+3,k)-dxcore
           xmix=hx(mm+1,k)+dh(mm+1,k)
           dmix=dxcore
           fqq1=1.0
           fqq2=0.0
         else
           yqq=(qcur-fmccor)/qq      ! = y in equations
           yw=yqq**wykla
           fqq1=1.0-yw*(wykla+1.0-wykla*yqq)
           fqq2=yw*(xov*(wykla+1.0-wykla*yqq)-qq*dxov*(1.0-yqq))
           xmix=xcore*fqq1+fqq2
           dmix=xmix-(hx(mm+1,k)+dh(mm+1,k))
           dh(mm+1,k)=dh(mm+1,k)+dmix
           dh(mm+3,k)=dh(mm+3,k)-dmix
           stir(k)=1-fqq2/(xloc-xcore*fqq1)
           xsum=xsum+dq*0.5*(xmix+xmixp)
           xlocs=xlocs+dq*0.5*(xloc+xlocp)
          endif
          qpre=qcur
          xmixp=xmix
          xlocp=xloc
      end do
      inho=kmixt
c
c
C   NITROGEN AND OXYGEN
      x6core=hx(mm+6,kccore)+dh(mm+6,kccore)
      x7core=hx(mm+7,kccore)+dh(mm+7,kccore)
      x6c0=x6core
      x7c0=x7core
      sint=0
      s6int=0
      s7int=0
      qpre=hx(mm,kccore)
      do k=kccore+1,kmixt
         qcur=hx(mm,k)
         x6loc=hx(mm+6,k)+dh(mm+6,k)
         x7loc=hx(mm+7,k)+dh(mm+7,k)
         dq=qcur-qpre
         sint=sint+0.5*(stir(k)+stir(k-1))*dq
         s6int=s6int+0.5*(x6loc*stir(k)+x6locp*stir(k-1))*dq
         s7int=s7int+0.5*(x7loc*stir(k)+x7locp*stir(k-1))*dq
         qpre=qcur
         x6locp=x6loc
         x7locp=x7loc
      end do
      x6core=(x6c0*fmccor+s6int)/(fmccor+sint)
      x7core=(x7c0*fmccor+s7int)/(fmccor+sint)
      dx6co=x6core-x6c0
      dx7co=x7core-x7c0
      do k=2,kmixt
         qcur=hx(mm,k)
         x6loc=hx(mm+6,k)+dh(mm+6,k)
         x7loc=hx(mm+7,k)+dh(mm+7,k)
         if(k.le.kccore) then
           dh(mm+6,k)=dh(mm+6,k)+dx6co
           dh(mm+7,k)=dh(mm+7,k)+dx7co
           xmix=hx(mm+1,k)+dh(mm+1,k)
           x6mix=hx(mm+6,k)+dh(mm+6,k)
           x7mix=hx(mm+7,k)+dh(mm+7,k)
         else
           xmix=hx(mm+1,k)+dh(mm+1,k)             !!!!!!!!!!!!!!
           x6mix=x6loc-stir(k)*(x6loc-x6core)
           x7mix=x7loc-stir(k)*(x7loc-x7core)
           dh(mm+6,k)=x6mix-hx(mm+6,k)
           dh(mm+7,k)=x7mix-hx(mm+7,k)
         endif
         xpri(1,k)=qcur
         xpri(2,k)=stir(k)
         xpri(3,k)=xmix
         xpri(4,k)=x6mix
         xpri(5,k)=x7mix
      enddo
c
      do k=kmixt+1,kmixt+25
         qcur=hx(mm,k)
         xloc=hx(mm+1,k)+dh(mm+1,k)
         x6loc=hx(mm+6,k)+dh(mm+6,k)
         x7loc=hx(mm+7,k)+dh(mm+7,k)
         xpri(1,k)=qcur
         xpri(2,k)=0.0d0
         xpri(3,k)=xloc
         xpri(4,k)=x6loc
         xpri(5,k)=x7loc
      enddo
c
C     END of OVERSHOOTING
444   continue
      do 125 inx=1,nx
         dh(mm+inx,1)=dh(mm+inx,2)
         hx(mm+inx,1)=hx(mm+inx,2)
  125 continue
c                                   --- set outgoing data
      do 13 i=1,20
         out(i)=0.
   13 continue
      out(1) = hx(mm+1,1)+dh(mm+1,1)
      out(2) = fmccor
      out(3) = fmheco
      out(4) = dgogx
      out(5) = gogcen
c                                   --- end of "profil"
      end
c
c............................................................................
c
      subroutine deltano(conv,rh,t,xavg,xvec,dh6,dh7)
c
c  to be called, if (temperature.gt.tenuc)
c ------------------------------------------------------------
c input:
c     conv    = .true., if there is convection in this mass shell
c     rh      = density (g/cm**3)
c     t       = temperature (K)
c     xavg    = an average value of hydrogen content for the timestep
c     xvec    = vector of chemical content (mass fraction)
c via /heninc/ and parameters from data.s37:
c     ahyd, an14, ao16, dtime
c     zp      = Y(N14) + Y(O16) + Y(O17)
c              (Yi = Xi / Ai); total # of CNO nuclei = zp * rh / amu
c              (assuming that all primordial C has been cycled into N14)
c output:
c     dh6,dh7 = time changes of N14 and O16 during the timestep
c ------------------------------------------------------------
c
c       the CNO cycle reactions:
c
c       dY(N14)/dt = chi * { - alf * Y(N14) + bet * Y(O17) }
c       dY(O17)/dt = chi * { - bet * Y(O17) + gam * Y(O16) }
c       dY(O16)/dt = chi * { - gam * Y(O16) + alf * Y(N14) }
c       dY(H1)/dt  = chi * { - 4. * rn14 * Y(N14) }
c       dY(He4)/dt = chi * {        rn14 * Y(N14) }
c
c       where chi = rh*Y(H1), alf = rn14h1*(rn15h1o/rn15h1c),
c       bet = (ro17h1n+ro17h1f), gam = ro16h1 .
c
c       Notice here that # of NO nuclei are conserved in the first three
c       reactions; that is, those 3 eqns. sum to zero.
c       Notice further that the last two eqautions are not consistent with
c       the full CNO cycle in that they do not account for the conversion
c       of mass from H1-He4 into CNO as the latter come into equilibrium.
c       Thus, total mass is not being conserved; the associated energy
c       generation is also being ignored in this subroutine.  The entire
c       set could be made consistent by including more terms in the last
c       two equations above and by tabulating more nuclear reaction rates.
c
c-------------------------------------------------------------
      implicit double precision (a-h,o-z)
      include 'data.s37'
      logical conv
      dimension xvec(nx)
      real*8 mue,h12b10(5),bh12(5)
      parameter (eqlfac=0.002d0,cnocut= 0.00001d0)
      parameter(flogt0=6.20d0,fdelt=0.02d0)
c              rh1(i) <-> flogt0 + i*fdelt etc.
      common/rattab/rh1(100),rhe3(100),rhe4(100),pp3pp2(100),
     *              rn14(100),alpha(100),beta(100),gamma(100),inuc
c   Rates in common/rattab/ are the decimal logs of the
c   actual (unscreened) rates in the above equations.
      save /rattab/
c-------------------------------------------------------------------
                               if(inuc.ne.99)call tabrat
c
        tem = log10(t)
        fact = (tem-flogt0)/fdelt
        it = fact
        if (it .le. 1) it = 1
        if (it .gt. 99) it = 99
        fact = fact-it
        cfact = 1.0d0-fact
        itplu1 = it+1
c
        alphak = cfact*alpha(it) + fact*alpha(itplu1)
        betak  = cfact*beta(it) + fact*beta(itplu1)
        gammak = cfact*gamma(it) + fact*gamma(itplu1)
c
        x    = xvec(1)
        call screen(x,log(rh),log(t),h12b10,bh12)
        alphak = 10.0d0**(alphak   + h12b10(4))
        betak  = 10.0d0**(betak    + h12b10(5))
        gammak = 10.0d0**(gammak   + h12b10(5))
c-------------------------------------------------------------------
        yn14   = xvec(6)/an14
        yo16   = xvec(7)/ao16
c
c   calculate new values for yn14, yo16 from semi-analytic form
c
        sigma  = alphak+betak+gammak
        bigc   = alphak*betak + betak*gammak + gammak*alphak
        delta  = sigma**2 - 4.d0*bigc
c
        yn14eq = betak*gammak*zp/bigc
        yo16eq = alphak*betak*zp/bigc
        bigd14 = yn14 - yn14eq
        bigd16 = yo16 - yo16eq
c
        dh6    = 0.
        dh7    = 0.
        tau    = rh*xavg/ahyd*dtime
c
c   skip the CNO calculations if the changes would be small
c
        if (sigma*tau .le. cnocut) return
c
c   test to see if N14 and O16 are near eql. values; if so, cause their
c   updates to bring them to eql. value; else calculate updates based on
c   an average value of hydrogen content for the timestep
c
        if (abs(bigd14/yn14eq) .lt. eqlfac .and.
     *      abs(bigd16/yo16eq) .lt. eqlfac .and. .not.conv )then
                dh6 = -bigd14*an14
                dh7 = -bigd16*ao16
        else
                dy14dt = -alphak*yn14 + betak*(zp-yn14-yo16)
                dy16dt = -gammak*yo16 + alphak*yn14
c
c   if delta > 0 then soln. is purely exponential; else soln. is
c   exp. times sinusoid
c
                if (delta .gt. 0.0d0) then
                        delta  = sqrt(delta)
                        etaplu = (sigma+delta)/2.d0
                        etamin = etaplu-delta
                        expplu = exp(-etaplu*tau)
                        expmin = exp(-etamin*tau)
                     fac0   = etamin*(1.d0-expplu)-etaplu*(1.d0-expmin)
                        fac0   = fac0/delta
                        fac1   = (expmin-expplu)/delta
                else
                      if (delta .eq. 0.0d0) delta = -sigma**2/10000.0d0
                        delta  = sqrt(-delta)
                        sind2t = sin(delta*tau/2.d0)
                        expsig = exp(-sigma*tau/2.d0)
                        fac0   = expsig*(cos(delta*tau/2.d0)+
     *                                   sind2t*sigma/delta) - 1.0d0
                        fac1   = expsig*sind2t*2.d0/delta
                end if
c
               dh6 = (fac1*dy14dt + fac0*bigd14) * an14
               dh7 = (fac1*dy16dt + fac0*bigd16) * ao16
        end if
c
        return
c                                   --- end of "deltano"
        end
c.............................................................................
c
      subroutine hsolve(dismax,delmax)
      implicit double precision (a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
c
c subject:
c     calculate the inner and the outer boundary conditions,
c     calculate all difference equations
c     calculate and adds the corrections "dx" to all time changes "dh"
c input:
c     the array of variables "hx" and time changes "dh"
c     at all mass points, stored in common/hen/
c output:
c     the array of variables "hx" and time changes "dh"
c     at all mass points, stored in common/hen/
c           with the corrections "dx" that have been added to "dh"
c     dismax = the largest discrepancy in the difference equations
c     delmax = the largest correction "dx"
c                     both in units of mass zone sizes, "ss"
c predefined in 'data.s37'
c     nt, m2, nss, mm, nx
c............................................................................

c           --- below table declarations form a common block due to SVS/386 1.0
c               fortran compiler needs. This common is not used outside of this
c               subroutine and nothing must be saved from call to call.
c
      common /comhso/ ss(nss), s(4,9), cx(4,3,nt), dx(4,nt),
     *       fobc(4,3), fibc(4,3), sk(4,5), sk1(4,5), xvec(nx),
     *       resenv(20), resen1(20), xx(m2), xp(m2)


      dtinv=0.
      if (dtime.ne.0.)dtinv=1.0/dtime

c                                 === CONSTANTS ===
c                                 --- convert the constants:
      nh1=nh-1
c                     ---  ss(i), i=1,2,3,4 = the maximum step size
c                          in four variables. Defined in 'data.s37'
      ss(1)=ss1
      ss(2)=ss2
      ss(3)=ss3
      ss(4)=ss4

c                                 === END OF CONSTANTS ===

c                      === BEGINNING OF OUTER BOUNDARY CONDITIONS ===
c                      --- calculate the array "fobc" defined as:
c                          fobc(i,1) = xx(i)-hx(i,nh1)-dh(i,nh1)
c                          fobc(i,2) = d ( xx(i) ) / dtelg
c                          fobc(i,3) = d ( xx(i) ) / dfllg
c                          where  i = 1, 2, 3, 4,    and:
c                          xx(i) = value of variable "i" at k=nh-1 as
c                                  calculated with subroutine env
c                          dtelg = perturbation of "telg", i.e. of the
c                                  log10 ( surface temperature)
c                          dfllg = perturbation of "fllg", i.e. of the
c                                  log10 ( surface luminosity )
      telg=hx(1,nh)+dh(1,nh)
      fllg=hx(2,nh)+dh(2,nh)
      starm=hx(mm,nh)
c      fmenv=1.-hx(mm,nh1)
c                      --- prepare vector of chemical content at the outer edge
      do 40 i=1,nx
      xvec(i)=hx(mm+i,nh1) + dh(mm+i,nh1)
   40 continue

      iout=1
      iprint=-1
      dismax=0.
      kdisma=0
      idisma=0
      dlogt=0.
      dlogrh=0.

cccc      open(2,file='evol.h38',status='unknown')

cccc      write(*,99)telg,fllg,starm,fmtot
cccc      write(2,99)telg,fllg,starm,fmtot
   99 format(2f10.4,2e15.4)      


      call env(telg,fllg,starm,xvec,resenv,iout,iprint)
c
      do 1 i=1,4
      fobc(i,1)=resenv(i)-hx(i,nh1)-dh(i,nh1)
      dis=abs(fobc(i,1))
      if (dismax.lt.dis) then
         dismax=dis
         kdisma=nh1
         idisma=i
      end if
    1 continue

      del=1.0d-9

      call env(telg+del,fllg,starm,xvec,resen1,iout,iprint)

      do 2 i=1,4
      fobc(i,2)=(resen1(i)-resenv(i))/del
    2 continue

      call env(telg,fllg+del,starm,xvec,resen1,iout,iprint)

      do 3 i=1,4
      fobc(i,3)=(resen1(i)-resenv(i))/del
    3 continue

c                      --- the corrections to the surface parameters and the
c                          corrections to the variables at "nh-1" satify
c                          the following equations:
c  fobc(i,1)+fobc(i,2)*dtelg+fobc(i,3)*dfllg - dx(i,nh1) = 0  ,   i=1,2,3,4

c                     --- calculate the elements of the "cx" array defined with
c        dtelg=dx(1,nh)=cx(1,1,nh1)+cx(1,2,nh1)*dx(1,nh1)+cx(1,3,nh1)*dx(2,nh1)
c        dfllg=dx(2,nh)=cx(2,1,nh1)+cx(2,2,nh1)*dx(1,nh1)+cx(2,3,nh1)*dx(2,nh1)
c             dx(3,nh1)=cx(3,1,nh1)+cx(3,2,nh1)*dx(1,nh1)+cx(3,3,nh1)*dx(2,nh1)
c             dx(4,nh1)=cx(4,1,nh1)+cx(4,2,nh1)*dx(1,nh1)+cx(4,3,nh1)*dx(2,nh1)
c
c  the first two equations may be rewritten as:

c  [cx(1,1,nh1)+cx(1,2,nh1)*fobc(1,1)+cx(1,3,nh1)*fobc(2,1)]
c    + dtelg * [cx(1,2,nh1)*fobc(1,2)+cx(1,3,nh1)*fobc(2,2)-1]
c    + dfllg * [cx(1,2,nh1)*fobc(1,3)+cx(1,3,nh1)*fobc(2,3)]   = 0

c  [cx(2,1,nh1)+cx(2,2,nh1)*fobc(1,1)+cx(2,3,nh1)*fobc(2,1)]
c    + dtelg * [cx(2,2,nh1)*fobc(1,2)+cx(2,3,nh1)*fobc(2,2)]
c    + dfllg * [cx(2,2,nh1)*fobc(1,3)+cx(2,3,nh1)*fobc(2,3)-1] = 0

c  to fulfil this set of equations for each values of "dtelg" and "dfllg"
c  the expressions in square brackets have to be all equal zero.
c  These six equations allow us to estimate:

      det=fobc(1,2)*fobc(2,3)-fobc(2,2)*fobc(1,3)
      cx(1,1,nh1)=(fobc(2,1)*fobc(1,3)-fobc(1,1)*fobc(2,3))/det
      cx(1,2,nh1)=fobc(2,3)/det
      cx(1,3,nh1)=-fobc(1,3)/det
      cx(2,1,nh1)=(fobc(1,1)*fobc(2,2)-fobc(2,1)*fobc(1,2))/det
      cx(2,2,nh1)=-fobc(2,2)/det
      cx(2,3,nh1)=fobc(1,2)/det

      do 5 i=3,4
      do 4 j=1,3
      cx(i,j,nh1)=fobc(i,2)*cx(1,j,nh1)+fobc(i,3)*cx(2,j,nh1)
    4 continue
      cx(i,1,nh1)=cx(i,1,nh1)+fobc(i,1)
    5 continue
c                    --- the array "cx" at the point "nh-1" has been calculated
c                    === END OF OUTER BOUNDARY CONDITIONS ===

c                    --- calculate the right hand sides and their derivatives
c                        at k=nh-1:
      k=nh-1
      do 60 i=1,m2
      xx(i)=hx(i,k)+dh(i,k)
   60 continue
      dlogrh=dh(1,k)
      dlogt=dh(2,k)
      call deriv(xx,dtinv,dlogt,dlogrh,sk,plog,pt,pr)

c                     === BEGIN THE ZONES: k = nh-1, nh-2, ... 3, 2  ===
    6 continue

      do 62 i=1,4
      do 61 j=1,5
      sk1(i,j)=sk(i,j)
   61 continue
   62 continue
      plog1=plog
      pt1=pt
      pr1=pr

      k=k-1
c                     === CALCULATE THE "s" ARRAY FOR "k" , "k+1": ===
c
c                     --- the corrections "dx" have to satisfy the equations:
c                         (i=1,2,3,4)
c                         s(i,1) +
c                         + s(i,2)*dx(1,k) + s(i,3)*dx(2,k) + s(i,4)*dx(3,k) +
c                         + s(i,5)*dx(4,k) + s(i,6)*dx(1,k1) + s(i,7)*dx(2,k1)+
c                         + s(i,8)*dx(3,k1) + s(i,9)*dx(4,k1) = 0

      k1=k+1
      dm=hx(mm,k1)-hx(mm,k)
c                     --- the mass between "k+1" and "k" has been calculated

c                     --- calculate the right hand sides and their derivatives
      do 63 i=1,m2
      xx(i)=hx(i,k)+dh(i,k)
   63 continue
      dlogrh=dh(1,k)
      dlogt=dh(2,k)
      call deriv(xx,dtinv,dlogt,dlogrh,sk,plog,pt,pr)

c                     --- calculate the discrepancy between the two sides
c                         of the difference equations:

      s(1,1)=dm*(sk(1,1)+sk1(1,1))/2+plog-plog1
      dis=abs(s(1,1))
      if (dismax.lt.dis) then
         kdisma=k
         idisma=1
         dismax=dis
      end if

      do 11 i=2,4
      s(i,1)=dm*(sk(i,1)+sk1(i,1))/2+hx(i,k)-hx(i,k1)+dh(i,k)-dh(i,k1)
      dis=abs(s(i,1))
      if (dismax.lt.dis) then
         kdisma=k
         idisma=i
         dismax=dis
      end if
   11 continue

c                     --- "dismax" is the maximum discrepancy in units of "ss"

c                     --- calculate the derivatives of the discrepancies with
c                         respect to all variables in "k" and "k+1",
c                         and store them in the "s" array:
      do 13 j=1,4

      j1=j+1
      j5=j+5

      do 12 i=1,4
      s(i,j1)=dm*sk(i,j1)/2
      s(i,j5)=dm*sk1(i,j1)/2
   12 continue

      if(j.eq.1)go to 14
      s(j,j1)=s(j,j1)+1
      s(j,j5)=s(j,j5)-1

   14 continue
   13 continue

      s(1,2)=s(1,2)+pr
      s(1,3)=s(1,3)+pt
      s(1,6)=s(1,6)-pr1
      s(1,7)=s(1,7)-pt1

c                    === "s" ARRAY HAS BEEN CALCULATED ===

c                    === BEGIN CALCULATIONS OF THE "cx" ARRAY FOR "k","k+1" ===
c                    --- the arrray "s" has been calculated, it is  4 x 9 :
c                        equation #              <---- k --->  <--- k1 --->
c                                 1          y   +1  x  x  x   -1  x  x  x
c                                 2          y    x +1  x  x    x -1  x  x
c                                 3          y    0  x +1  0    0  x -1  0
c                                 4          y    x  x  0 +1    x  x  0 -1
c                        in this cartoon symbols have the following meaning:
c                        y = the "free" term, should be equal  0
c                        x = a non-zero element, proportional to zone thickness
c                       +1 = an element approximately equal +1
c                       -1 = an element approximately equal -1
c                        0 = an element that is identically equal  0

c                    --- the last array "cx" gives:
c                   dx(3,k1)=cx(3,1,k1)+cx(3,2,k1)*dx(1,k1)+cx(3,3,k1)*dx(2,k1)
c                   dx(4,k1)=cx(4,1,k1)+cx(4,2,k1)*dx(1,k1)+cx(4,3,k1)*dx(2,k1)

c                    --- eliminate "dx(3,k1)" and "dx(4,k1)" from the array "s"
      do 20 i=1,4
      s(i,1)=s(i,1)+s(i,8)*cx(3,1,k1)+s(i,9)*cx(4,1,k1)
      s(i,6)=s(i,6)+s(i,8)*cx(3,2,k1)+s(i,9)*cx(4,2,k1)
      s(i,7)=s(i,7)+s(i,8)*cx(3,3,k1)+s(i,9)*cx(4,3,k1)
   20 continue
c                    --- the array "s" is now  4 x 7 :
c                        y   +1  x  x  x   -1  x
c                        y    x +1  x  x    x -1
c                        y    0  x +1  0    x  x
c                        y    x  x  0 +1    x  x

      do 22 i=1,2
      fac3=s(i,4)/s(3,4)
      fac4=s(i,5)/s(4,5)
      do 21 j=1,7
      s(i,j)=s(i,j)-s(3,j)*fac3-s(4,j)*fac4
   21 continue
   22 continue

c                     --- the array "s" is now  4 x 7 :
c                         y   +1  x  0  0   -1  x
c                         y    x +1  0  0    x -1
c                         y    0  x +1  0    x  x
c                         y    x  x  0 +1    x  x

c                     --- solve the first two equations
c                         for "dx(1,k1)" and "dx(2,k1)":

      det=s(1,6)*s(2,7)-s(1,7)*s(2,6)

      do 25 m=1,2
      if(m.eq.1)fac1=-s(2,7)
      if(m.eq.1)fac2=s(1,7)
      if(m.eq.2)fac1=s(2,6)
      if(m.eq.2)fac2=-s(1,6)
      do 23 j=1,3
      cx(m,j,k)=(s(1,j)*fac1+s(2,j)*fac2)/det
   23 continue
   25 continue

c                     --- plug this solution into equations 3 and 4,
c                         and solve for "dx(3,k)" and "dx(4,k)"
      do 27 i=3,4
      do 26 j=1,3
      cx(i,j,k)=-(s(i,j)+cx(1,j,k)*s(i,6)+cx(2,j,k)*s(i,7))/s(i,i+1)
   26 continue
   27 continue

c                     === THE ARRAY "cx" FOR "k" , "k+1" HAS BEEN CALCULATED ==

      if(k.gt.2)go to 6

c                     === END OF THE ZONES: k = nh-1, nh-2, ... 3, 2 :

c                     --- we are at  k=2, the last array "cx" gives:
c                         dx(3,2)=cx(3,1,2)+cx(3,2,2)*dx(1,2)+cx(3,3,2)*dx(2,2)
c                         dx(4,2)=cx(4,1,2)+cx(4,2,2)*dx(1,2)+cx(4,3,2)*dx(2,2)
c                     --- the inner boundary will provide more equations

c                     === BEGINNING OF INNER BOUNDARY CONDITIONS: ===
c                     --- calculate the array "fibc" defined as:
c                         fibc(i,1) = xx(i)-hx(i,2)-dh(i,2)
c                         fibc(i,2) = d ( xx(i) ) / drhclg
c                         fibc(i,3) = d ( xx(i) ) / dtclg
c                         where  i = 1, 2, 3, 4,    and:
c                         xx(i) = value of variable "i" at   k=2
c                                 as calculated with subr. center
c                         dtclg = perturbation of "tclg", i.e. of the
c                                 log10 ( central temperature)
c                         drhclg= perturbation of ""rhclg, i.e. of the
c                                 log10 ( central density )
      rhclg=hx(1,1)+dh(1,1)
      tclg=hx(2,1)+dh(2,1)
c      fmc=hx(mm,2)
      dlogrh=dh(1,2)
      dlogt=dh(2,2)

      do 93 i=1,nx
      xvec(i)=hx(mm+i,2) + dh(mm+i,2)
   93 continue

      call center(rhclg,tclg,xvec,dtinv,dlogt,dlogrh,xx)

      do 28 i=1,4
      fibc(i,1)=xx(i)-hx(i,2)-dh(i,2)
      dis=abs(fibc(i,1))
      if (dismax.lt.dis) then
         kdisma=2
         idisma=i
         dismax=dis
      end if
   28 continue

      del=1.0d-9

      call center(rhclg+del,tclg,xvec,dtinv,dlogt,dlogrh,xp)

      do 29 i=1,4
      fibc(i,2)=(xp(i)-xx(i))/del
   29 continue

      call center(rhclg,tclg+del,xvec,dtinv,dlogt,dlogrh,xp)

      do 30 i=1,4
      fibc(i,3)=(xp(i)-xx(i))/del
   30 continue


c                    --- the corrections to the central parameters and the
c                        corrections to the variables at  k=2  satify
c                       the following equations:
c                       fibc(i,1)+fibc(i,2)*drhclg+fibc(i,3)*dtclg - dx(i,2) =0
c                                                i=1,2,3,4

c                    --- calculate the elements of the "cx" array defined with:
c                  drhclg=dx(1,1)=cx(1,1,1)+cx(1,2,1)*dx(1,2)+cx(1,3,1)*dx(2,2)
c                   dtclg=dx(2,1)=cx(2,1,1)+cx(2,2,1)*dx(1,2)+cx(2,3,1)*dx(2,2)
c                         dx(3,2)=cx(3,1,1)+cx(3,2,1)*dx(1,2)+cx(3,3,1)*dx(2,2)
c                         dx(4,2)=cx(4,1,1)+cx(4,2,1)*dx(1,2)+cx(4,3,1)*dx(2,2)

      det=fibc(1,2)*fibc(2,3)-fibc(2,2)*fibc(1,3)
      cx(1,1,1)=(fibc(2,1)*fibc(1,3)-fibc(1,1)*fibc(2,3))/det
      cx(1,2,1)=fibc(2,3)/det
      cx(1,3,1)=-fibc(1,3)/det
      cx(2,1,1)=(fibc(1,1)*fibc(2,2)-fibc(2,1)*fibc(1,2))/det
      cx(2,2,1)=-fibc(2,2)/det
      cx(2,3,1)=fibc(1,2)/det

      do 32 i=3,4
      do 31 j=1,3
      cx(i,j,1)=fibc(i,2)*cx(1,j,1)+fibc(i,3)*cx(2,j,1)
   31 continue
      cx(i,1,1)=cx(i,1,1)+fibc(i,1)
   32 continue
c                    --- the array cx at the inner boundary has been calculated

c                    --- combine the two sets of equations expressing "dx(3,2)"
c                        and "dx(4,2)" in terms of "dx(1,2)" and "dx(2,2)":
      do 34 i=3,4
      do 33 j=1,3
      cx(i,j,1)=cx(i,j,1)-cx(i,j,2)
   33 continue
   34 continue

      det=cx(3,2,1)*cx(4,3,1)-cx(4,2,1)*cx(3,3,1)
      dx(1,2)=(cx(4,1,1)*cx(3,3,1)-cx(3,1,1)*cx(4,3,1))/det
      dx(2,2)=(cx(3,1,1)*cx(4,2,1)-cx(4,1,1)*cx(3,2,1))/det

c                     --- corrections: "dx(1,2)" and "dx(2,2)" have been found
      do 35 i=1,2
      dx(i,1)=cx(i,1,1)+cx(i,2,1)*dx(1,2)+cx(i,3,1)*dx(2,2)
   35 continue

c                     --- corr. to the central parameters have been calculated
c                     === END OF THE INNER BOUNDARY CONDITIONS ===

c                     === BEGIN CORRECTIONS: ===
c                     --- calculate all the corrections,
c                         look for the largest correction:
      delmax=abs(dx(1,1)/ss(1))
      delm=abs(dx(2,1)/ss(2))
      if(delmax.lt.delm)delmax=delm

      do 37 k=2,nh1
      k1=k+1
      dx(1,k1)=cx(1,1,k)+cx(1,2,k)*dx(1,k)+cx(1,3,k)*dx(2,k)
      dx(2,k1)=cx(2,1,k)+cx(2,2,k)*dx(1,k)+cx(2,3,k)*dx(2,k)
      dx(3,k) =cx(3,1,k)+cx(3,2,k)*dx(1,k)+cx(3,3,k)*dx(2,k)
      dx(4,k) =cx(4,1,k)+cx(4,2,k)*dx(1,k)+cx(4,3,k)*dx(2,k)

      do 36 i=1,4
      delm=abs(dx(i,k)/ss(i))
      if(delmax.lt.delm)delmax=delm
   36 continue
   37 continue

      delm=abs(dx(1,nh)/ss(1))
      if(delmax.lt.delm)delmax=delm
      delm=abs(dx(2,nh)/ss(4))
      if(delmax.lt.delm)delmax=delm
c                     --- all the corrections have been calculated,
c                         the largest is "delmax"

c                     --- add corrections "dx" to all time changes "dh" :
      fac=1.
      if((delmax*.3).gt.fac)fac=1./(delmax*.3)

c                 ^                          ^
c            ---    Attention, please - relatively big corrections allowed.
c            ---    In the case of trubbles with convergence, you can try,
c            ---    as a first step, to increase the above indicated values.
      do 39 k=2,nh1

      do 38 i=1,4
      dh(i,k)=dh(i,k)+dx(i,k)*fac
   38 continue

   39 continue

c                     --- add corrections to the boundary parameters:
      dh(1,1)=dh(1,1)+dx(1,1)*fac
      dh(2,1)=dh(2,1)+dx(2,1)*fac
      dh(1,nh)=dh(1,nh)+dx(1,nh)*fac
      dh(2,nh)=dh(2,nh)+dx(2,nh)*fac

c                                           --- end of "hsolve"
      end
c
c..............................................................................
c
      subroutine deriv(xx,dtinv,dlogt,dlogrh,sk,plog,pt,pr)
      implicit double precision (a-h,o-z)
      include 'data.s37'
      dimension xx(m2),sk(4,5)
c
c subject:
c     calculate right hand sides and the derivatives
c input:
c     xx(1)  = log10 (density)                      [g/cm**3]
c     xx(2)  = log10 (temperature)                  [K]
c     xx(3)  = log10 (radius)                       [cm]
c     xx(4)  = luminosity / luminosity unit
c     xx(5)  = mass / total stellar mass
c     xx(6)  = x         hydrogen content                 [by mass fraction]
c     xx(7)  = y           helium content                 [by mass fraction]
c     xx(8)  = zn        nitrogen content for CNO burning [by mass fraction]
c     dtinv  = 1 / the current time step            [1/s]
c              ("dtime" of /heninc/ not used here)
c     dlogt  = change of log10 (temperature) during the current time step
c     dlogrh = change of log10 (density) during the current time step
c notice:
c     xx(i) already include the recent time change
c output:
c     sk(1,1)   = d plog / d xx(5)
c     sk(i,1)   = d xx(i) / d xx(5)    ,       i =    2, 3, 4
c     sk(i,j+1) = d sk(i,1) / d xx(j)  ,    i, j = 1, 2, 3, 4
c     plog      = log10 (pressure)                  [c.g.s.]
c     pt        = d ln p / d ln t     at constant density
c     pr        = d ln p / d ln rh    at constant temperature
c predefined in 'data.s37'
c     m2  = number of variables in every mass point
c called:
c     rhsh     to calculate all the derivatives
c.............................................................................

      dimension yy(m2),xp(m2),yp(m2)
      dimension extra(20)

c                         --- calculate the right hand sides:

      call rhsh(xx,yy,dtinv,dlogt,dlogrh,extra)
      plog=extra(17)
      pt=extra(2)
      pr=extra(3)

      do 1 i=1,4
      sk(i,1)=yy(i)
    1 continue

c                         --- calculate the derivatives of the right hand sides
      del=1.0d-9
c                         --- "del" seems to be rather small...,  but it works!
      do 4 j=1,4

      do 2 i=1,m2
      xp(i)=xx(i)
    2 continue
      dlogtp=dlogt
      dlogrp=dlogrh

      xp(j)=xp(j)+del
      if(j.eq.1)dlogrp=dlogrp+del
      if(j.eq.2)dlogtp=dlogtp+del

      call rhsh(xp,yp,dtinv,dlogtp,dlogrp,extra)

      j1=j+1

      do 3 i=1,4
      sk(i,j1)=(yp(i)-yy(i))/del
    3 continue

    4 continue
c                         --- the array "sk" has been calculated
c                         --- end of "deriv"
      end
c
c..............................................................................
c
      subroutine rhsh(xx,yy,dtinv,dlogt,dlogrh,extra)
      implicit double precision(a-h,o-z)
      include 'data.s37'
      dimension xx(m2),yy(m2),extra(20)
c
c subject:
c     calculate right hand sides of the stellar structure equations
c     including time-dependent terms.
c     However,  if dtinv=0. then time-dependent terms are neglected.
c input:
c     xx(1) = log10 (density)                      [g/cm**3]
c     xx(2) = log10 (temperature)                  [K]
c     xx(3) = log10 (radius)                       [cm]
c     xx(4) = luminosity / unit luminosity
c     xx(5) = mass / total stellar mass
c     xx(6) = hydrogen content                     [by mass fraction]
c     xx(7) = helium content                       [by mass fraction]
c     xx(8..5+nx) = subsequent elements contents   [by mass fraction]
c     dtinv = 1 / the current time step            [1/s]
c     dlogt  = change of log10 (temperature) during the current time step
c     dlogrh = change of log10 (density)     during the current time step
c output
c     yy(1) = d log10 p / d xx(5)   attention: this is different than in "rhs"
c     yy(2) = d log10 t / d xx(5)
c     yy(3) = d log10 r / d xx(5)
c     yy(4) = d l/lunit / d xx(5)
c     yy(5) = d xx(5)   / d xx(5) = 1.0
c     yy(6)...yy(m2) = 0.
c     extra = number of additional functions
c     extra(1) = p     = pressure                  [c.g.s.]
c     extra(2) = pt    = d ln(p) / d ln(t)    (at constant density)
c     extra(3) = pr    = d ln(p) / d ln(ro)   (at constant temperature)
c     extra(4) = pgas  = gas pressure              [c.g.s.]
c     extra(5) = prad  = radiation pressure        [c.g.s.]
c     extra(6) = grad  = adiabatic temperature gradient
c     extra(7) = qt
c     extra(8) = qr
c     extra(9) = log10(kappa)
c     extra(10)= opt = d ln(kappa) /d ln(t)
c     extra(11)= opr = d ln(kappa) /d ln(ro)
c     extra(12)= not used
c     extra(13)= not used
c     extra(14)= log10(epsx)
c     extra(15)= dxt   = d X / d t = hydrogen burning rate           [1/s]
c     extra(16)= drom  = d log10(rh) / d (mass fraction)
c     extra(17)= plog  = log10(p)
c     extra(18)= dpm   = d log10(p) / d (mass fraction)
c     extra(19)= grrad = radiative temperature gradient
c     extra(20)= grrad/grad = radiative/adiabatic gradient ratio
c called:
c     state    to calculate thermodynamic functions
c     opact    to calculate opacity (total: radiative and conductive)
c     nburn    to calculate energy generation rate in hydrogen burning
c              + neutrino energy loss rate (if hydrogen content = 0)
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
c     grrh   = d ln rh / d ln p     density gradient in the star
c     dpm    = d log10 p / d ( Mr / total mass ) pressure gradient in the star
c............................................................................

      dimension xvec(nx), dxtvec(nx)

      fcon1   = (flunit/fmtot)/(16*pi*clight*gconst)
      fcon2   = gconst/4./pi*fmtot**2/fln10
      fcon3   = fmtot/4./pi/fln10
      fcon4   = fmtot/flunit
c
c ------------------------------------------------------ ROTATION
      fcon5=2.0d0/3.0d0*com(13)**2/gconst/fmtot
c ------------------------------------------------------
      rh=10.**xx(1)
      t=10.**xx(2)
      r=10.**xx(3)
      flr=xx(4)
      fmr=xx(5)

      do 1 i=1,nx
      xvec(i)=xx(mm+i)
    1 continue

      call opact(rh,t,xvec,fkap,opr,opt)
      call state(rh,t,xvec,extra)
      call nburn(rh,t,xvec,epsx,dxtvec)

      p    = extra(1)
      pt   = extra(2)
      pr   = extra(3)
c      pgas = extra(4)                      ! not used
c      prad = extra(5)
      grad = extra(6)
      qt   = extra(7)
      qr   = extra(8)

      plog=log10(p)
c ------------------------------------------------------ ROTATION
      frot=1.0d0-fcon5*r**3/fmr     
      prad=arad/3.0*t**4
      grrad=fcon1*fkap*p/prad*flr/fmr/frot               ! --- WITH ROTATION
      grt=grad
      if(grt.gt.grrad)grt=grrad
      grrh=(1-grt*pt)/pr
      dpm=-fcon2*fmr/r**4/p*frot                         ! --- WITH ROTATION

      do 2 i=1,m2
      yy(i)=0.
    2 continue

      yy(1)=dpm
      yy(2)=grt*dpm
      yy(3)=fcon3/(rh*r**3)
      yy(4)=fcon4*epsx
      if (dtinv.ne.0.)
     *   yy(4)=yy(4)-fcon4*fln10*dtinv*(dlogt*qt-dlogrh*qr)
      yy(5)=1.

      drom=grrh*dpm
      extra(9)  = log10(fkap)
      extra(10) = opt
      extra(11) = opr
      if (epsx.le.0.0d0) epsx=1.0d-50
      extra(14) = log10(epsx)
      extra(15) = dxtvec(1)
      extra(16) = drom
      extra(17) = plog
      extra(18) = dpm
      extra(19) = grrad
      extra(20) = grrad/grad
c                                           --- end of "rhsh"
      end
c
c........................................................................
c
      subroutine addcor
      implicit double precision (a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
c
c subject:
c     add time changes "dh" to the table "hx"
c     re-normalize luminosity (this is the only place where it is done)
c     add time step
c common /hen/
c     this is both input and output to this subroutine
c common /heninc/
c     flunit is re-normalized
c predefined in 'data.s37'
c     m2,mm
c remark
c     on output time changes are not set to 0. It is done  within
c     the subroutine "extrap".
c..............................................................................

      nh1=nh-1
c                                               --- find maximum luminosity
      flmax=0.
      do 1 k=2,nh1
         fl=hx(4,k) + dh(4,k)
         if (fl.gt.flmax) flmax=fl
    1 continue
      faclum=flmax
c                                               --- re-normalize luminosity
      do 2 k=2,nh1
         hx(4,k)=hx(4,k)/faclum
         dh(4,k)=dh(4,k)/faclum
    2 continue
      flunit=flunit*faclum
c                                               --- add time changes
      do 4 k=1,nh
      do 3 i=1,m2
         if (i.ne.mm) hx(i,k) = hx(i,k) + dh(i,k)
    3 continue
    4 continue
c                                               --- add time step
      time=time+dtime
c                                               --- end of "addcor"
      end
c
c..............................................................................

c                                            NONSTANDARD SUBROUTINEs
      subroutine newomeg
      implicit double precision (a-h, o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
      fmr=hx(mm,2)
      cal = (10.**hx(3,2))**2/2.*fmr
      do 1 k=2,nh-2
         fmr=hx(mm,k)
         fmr1=hx(mm,k+1)
         dm=fmr1-fmr
         rr= 10.**hx(3,k)
         rr1=10.**hx(3,k+1)
         cal=cal+(rr**2+rr1**2)/2.*dm       ! cal = moment of inertia [1/fmtot]
    1 continue
         cal=cal*fmtot + com(14)     ! cal - in [cgs]; com(14) - from env
c
      if(model.eq.1)then
        com(11)=com(13)*cal
      else
        com(13)=com(11)/cal
      end if

      end
c
c..........................................................................
c
      subroutine tefref
      implicit double precision (a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
c
c subject:
c     compute Teff, Reff, Rsurf
c     for use in main program and optionally in MODOUT
c called:
c     env
c variables computed for /heninc/ of DATA.s37:
c     com(16) = lgTeff = log10 ( effective temperature )   [K]
c     com(17) = lgReff = log10 ( effective radius )        [cm]
c     rsurf   = starra = surface radius                    [cm]
c..............................................................................

      dimension xvec(nx), resenv(20)

c
      nh1=nh-1
      telg=hx(1,nh)
      fllg=hx(2,nh)
c                                             --- prepare data to call envelope
      starm=fmtot/sunm
c      fmenv=1.-hx(5,nh1)
      iout=1
      iprint=-1
c                                             --- prepare xvec for envelope
      do 1 i=1,nx
      xvec(i)=hx(mm+i,nh1)
    1 continue
    
    
c -------------------------------
      call env(telg,fllg,starm,xvec,resenv,iout,iprint)
c -------------------------------
c
c                 ---  save resenv(8)   ( = log Teff   (K) ),
c                           resenv(12)  ( = log Reff  (cm) ),
c                           resenv(10)  ( = log Rsurf (cm) ),
c                           resenv(13)  ( = moment of inertia (c.g.s.) ),
c                      they will be used in main and in MODOUT
c
      com(16)=resenv(8)
      com(17)=resenv(12)
      com(14)=resenv(13)
      rsurf=10.0**resenv(10)
c                                              --- end of tefref
      end
c...........................................................................
c
      subroutine modout
      implicit double precision (a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
c
c subject:
c     write data for stellar pulsation
c called:
c     env
c     corout
c output units, filenames
c     file 'osc#####' created through i/o unit 1 (open then close)
c..............................................................................
c
      dimension xvec(nx), resenv(20)
      parameter (ntm=3000)
      dimension puls(13), pulsenv(13,ntm), pulscor(13,ntm)
      dimension ar(13,ntm)
      character fname*8
      common /envarp/ pulsenv, nenv 
      common /corcom/ puls
c
      write(*,*) 'writing "osc" data...'
      iout= 1
c                                              --- preparing envelope results
      nh1=nh-1
c
      telg=hx(1,nh)
      fllg=hx(2,nh)
      fl  = 10.0**fllg
c                                             --- prepare data to call envelope
      starm=fmtot/sunm
c      fmenv=1.-hx(5,nh1)
      iprint=1
c                                             --- prepare xvec for envelope
      do 1 i=1,nx
      xvec(i)=hx(mm+i,nh1)
    1 continue
c                                  --- Teff, Reff, Rsurf et al.
c                                      (they saved in TEFREF)
      teff = 10.0**com(16)
      reff = 10.0**com(17)
c                                    --- call ENV to prepare envelope results
c                                        (made by GOBOUT called in ENV)
c -------------------------------
      call env(telg,fllg,starm,xvec,resenv,iout,iprint)
c -------------------------------
c                                              --- preparing inner results
      nh1=nh-1
c
c     ---  nh = 2 + the number of mass points in the converged model
c
      iprint=1
      com(18)=0.0d0
      do 2 k=nh1,2,-1
      kk=nh1-k+1
c                                    --- call COROUT to prepare inner results
c -------------------------------
      call corout(k,iout,iprint)
c -------------------------------
      do i=1,13
         pulscor(i,kk)=puls(i)
      enddo
    2 continue
c                                           --- writing kccore, ar(6,kccore)
      kccore=icom(18)
      ar6nc=com(18)
      omega=com(13)
c
c                                    --- block based on ARP13.for to produce OSC file
c
c
c                 ng = number of mass points in the envelope
c
      ng=nenv
c
c                         nc = number of layers in the convective core
      nc=kccore-1
c
c                         ntt = full number of layers in the model
      ntt=nenv+nh-3
      if (ntt.le.ntm) go to 30
c
      write(*,*) ' too many points'
      stop
c
   30 continue
c
      do k=nh-2, ntt
         kk=ntt-k+1
         do i=1,13
            ar(i,k)=pulsenv(i,kk) 
         enddo
      enddo
c
      do k=1,nh-3
         kk=nh-2-k+1
         do i=1,13
            ar(i,k)=pulscor(i,kk) 
         enddo
      enddo
c
      write(fname,123) model
      open(iout,file=fname,status='new',form='unformatted')
c      open(iout,file=fname,status='new')
c      write(*,*) ' writing data...'
c
      sm=fmtot/sunm
      rt=rsurf/sunr
      reff=reff/sunr
c
      write(iout) sm,rt,fl,teff,reff,nc,ntt,ng,ar6nc,omega
c      write(iout,105) sm,rt,fl,teff,reff,nc,ntt,ng,ar6nc,omega
c      write(iout,*)
c
      do k=1,ntt
         write(iout) (ar(i,k),i=1,13)
c         write(iout,101) (ar(i,k),i=1,13)
      enddo
c
      close(iout)
c
c  101 format(1p13e14.6)
c  105 format(0p2f10.5,1p2e14.6,0p1f10.5,3i5,1p3e14.6)
  123 format('osc',i5.5)
c
c -------------------------------
      write(*,*) '  ...done'
c                                              --- end of modout
      end
c..........................................................................
c17b
c                                            NONSTANDARD SUBROUTINE
      subroutine corout(k,iunit,iprint)
      implicit double precision (a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
      dimension xx(m2),yy(m2),extra(20), puls(13)
      common /corcom/ puls
c
c     xx(1) = log10 (density)                      [g/cm**3]
c     xx(2) = log10 (temperature)                  [K]
c     xx(3) = log10 (radius)                       [cm]
c     xx(4) = luminosity / unit luminosity
c     xx(5) = mass / total stellar mass
c     xx(6) = hydrogen content                     [by mass fraction]
c     xx(7) = helium content                       [by mass fraction]
c     xx(8..5+nx) = subsequent elements contents   [by mass fraction]
c
c     drm    = d log10 r / d ( Mr / total mass )
c     dpt    = d ln(p) / d ln(t)    (at constant density)
c     dpro   = d ln(p) / d ln(ro)   (at constant temperature)
c     gradad = d ln t / d ln p     adiabatic temperature gradient
c     extra(19)= grrad = radiative temperature gradient
c     grt    = d ln t / d ln p     temperature gradient in the star
c     drot   = d ln(ro) /d ln(t)   (at constant pressure)
c     dror   = d log10 ro / d log10 r  from the model (= d ln ro / d ln r)
c     dgat   = d gradad / d ln t   from the model
c     grrh   = d ln rh / d ln p     density gradient in the star
c     dpm    = d log10 p / d ( Mr / total mass ) pressure gradient in the star
c     gam1   = d ln(p) / dln(ro)   (at constant entropy)
c     opt    = d ln(kappa) /d ln(t)
c     opr    = d ln(kappa) /d ln(ro)
c     drom   = d log10(rh) / d (mass fraction)
c     yy(2)  = d log10 t / d ( Mr / total mass )
c............................................................................

      if(iprint.lt.0) return
c                                      --- prepare data for rhsh
      do 1 i=1,m2
      xx(i)=hx(i,k)
    1 continue
      call rhsh(xx,yy,0.0d0,0.0d0,0.0d0,extra)

      starl   = 10.**hx(2,nh)*sunl
      ro      = 10.**xx(1)
      t       = 10.**xx(2)
      r       = 10.**xx(3)
      flr     = xx(4)
      fm      = xx(5)
      drm     = yy(3)
      p       = extra(1)
      dpro    = extra(3)
      dpt     = extra(2)
      gradad  = extra(6)
      opt     = extra(10)
      opr     = extra(11)
      drom    = extra(16)
      dpm     = extra(18)
      drop    = 1./dpro
      drot    = -dpt/dpro
      gam1    = 1./(drop + drot*gradad)
c      gam1    = extra(13)                    ! LLNL EOS+
      call drorsu(k,dror)
      grrad   = extra(19)
      grt     = yy(2)/dpm
c                                       --- store results
      puls(1) = log(r/rsurf)
      puls(2) = 3.*(r/rsurf)**3/fm
      puls(3) = -dpm/drm
      puls(4) = 1./gam1
      puls(5) = 4.*pi*r**3*ro/(fm*fmtot)
      puls(7) = drot
      puls(8) = grt
      puls(10)= gradad/grt
      puls(11)= opt-4.0+drot*opr
c      puls(12)= grrad/grt
      puls(12)= starl/(flr*flunit)
      puls(13)= -sqrt(3.0*gconst*fmtot/rsurf**3)
     #          *4.0*pi*r**3*p*drot/starl/gradad
c
      kccore=icom(18)
      if (k.eq.kccore) com(18) = -dror + dpm/gam1/drm
      if (k.le.kccore.and.kccore.gt.2) then
         puls(6) = (-drom + dpm/gam1)/drm
         if (abs(puls(6)).lt.1.d-15) puls(6)=0.d0
      else
         puls(6) = -dror + dpm/gam1/drm
      endif
c
      call dgatsu(k,dgat)
      puls(9) = gradad*(opt-4.0)+puls(4)*opr+puls(10)-dgat
c
c                                       --- end of "corout"
      end
c
c...........................................................................
c
      subroutine drorsu(kpt,res)
      implicit double precision (a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
c
c subject:
c     to compute derivative d log rho / d log r from the model
c method:
c     3-point polynome, Newton scheme
c input:
c     kpt = mass point number
c output
c     res = result
c..............................................................................

      x=hx(3,kpt)
      k=kpt
      kccore=icom(18)
      if(k.eq.kccore) k=kccore+1
      if(k.gt.nh-2) k=nh-2
      if(k.lt.3) k=3
      x0=hx(3,k+1)
      x1=hx(3,k)
      x2=hx(3,k-1)
      f0=hx(1,k+1)
      f1=hx(1,k)
      f2=hx(1,k-1)
      f01=(f1-f0)/(x1-x0)
      f12=(f2-f1)/(x2-x1)
      f012=(f12-f01)/(x2-x0)
      res=f01+f012*(2.*x-x0-x1)
c                                               --- end of "drorsu"
      end
c..............................................................................
c
      subroutine dgatsu(kpt,res)
      implicit double precision (a-h,o-z)
      include 'data.s37'
      common/hen/ hx(m2,nt), dh(m2,nt), nh
      dimension xvec(nx),gradad(3),extra(20)
c
c subject:
c     to compute derivative d gradad / d log t from the model
c method:
c     3-point polynome, Newton scheme
c input:
c     kpt = mass point number
c output
c     res = result
c..............................................................................

      x=hx(2,kpt)*fln10
      k=kpt
      if(k.gt.nh-2) k=nh-2
      if(k.lt.3) k=3
c
      do 2 j=1,3
      do 1 i=1,nx
      xvec(i)=hx(mm+i,k+2-j)
    1 continue
c
      rh = 10.**hx(1,k+2-j)
      t  = 10.**hx(2,k+2-j)
      call state(rh,t,xvec,extra)
      gradad(j)=extra(6)
    2 continue
c
      x0=hx(2,k+1)*fln10
      x1=hx(2,k)*fln10
      x2=hx(2,k-1)*fln10
      f0=gradad(1)
      f1=gradad(2)
      f2=gradad(3)
      f01=(f1-f0)/(x1-x0)
      f12=(f2-f1)/(x2-x1)
      f012=(f12-f01)/(x2-x0)
      res=f01+f012*(2.*x-x0-x1)
c                                               --- end of "dgatsu"
      end
c..............................................................................

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
c        This file contains "STANDARD" STELLAR EVOLUTION PROGRAM "h37" -
c         main program and six subroutines called:
c             - "extrap" -
c             - "profil" -
c             - "hsolve" -
c             - "deriv"  -
c             - "rhsh"   -
c             - "addcor" -
c             - "modout" -
c             - "corout" -
c             - "drorsu" -
c                                                    end of the file "h37.for"
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

