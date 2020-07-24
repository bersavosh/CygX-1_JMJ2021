c *****************************************************************************
c      program OPAC.for
c **************************
c Comment of 07.05.2008: 
c
c Please choose and open one of the files:
c
c       open(2, FILE='GN93hz.dat')        ! OPAL GN93
c       open(2, FILE='OPAL_A04.dat')      ! OPAL A04
c       open(2, FILE='OPCD3_GN93.dat')    ! OP   GN93
c       open(2, FILE='OPCD3_A04.dat')     ! OP   A04  (our new standard)
c **********************************************************************
c                                                           date: 24.11.95
c Obtained from OPACITY.for (taken from R.S., 5.11.95).
c !!! Set value of parameter IGRID in subroutine OPACITY2 (igrid = 1 or 2).
c !!! In the case IGRID > 2 change also formats 100 and 103 in subr. OPACITY2.
c ------------------------------------------
c Assuming given metal mass fraction, Z,
c this code prepares opacity tables for future use in evolutionary codes.
c See the following input/output paremeter list.
c-----------------------------------------------------------------------------
c The OPAL opacity tables ( table "GN93hz",last issue; see also:
c            Rogers, F.J., & Iglesias, C.A., A.S.P. Conf. Ser. 78, p.78 (1995)
c            Iglesias, C.A. & Rogers, F.J., 1995, Ap.J., submitted.
c are prolongated towards lower temperatures using molecular & grain opacities
c calculated (special thanks !) for similar chemical mixes as the OPAL tables
c (Alexander,D.R., & Ferguson,J.W. 1994, ApJ, 437, 879;
c  Alexander, D.R. 1995, private communication to R. Sienkiewicz).
c We use the OPAL opacity tables for log10(T) > 3.95 , and
c the Alexander's opacity tables for log10(T) < 3.95, where T = temperature.
c A very good fit is possible at log10(T)=3.95, for all hydrogen mass fractions.
c
c The above radiative opacities are supplemented with conductive opacities,
c according to formulae by Yakovlev & Urpin (1980, Astr.Zh., 57, 526),
c tested & compiled by P.Haensel.
c------------------------------------------------------------------------------
c The tabulated value is log10 ( coefficient of opacity, [c.g.s.] )
c as function of temperature, density and chemical composition in the form
c as used in the OPAL opacity tables. These tables - named "x**" - are saved
c on disk to make easier visual examinations of intermediate results.
c
c The final opacity files, "OPAC_7.DAT", and "OPAC_ENV.DAT" are obtained by 
c applying cubic splines (in temperature and R=rho/T6**3) to the tables "x**",
c to calculate data for parameter grid denser than that used in "x**".
c This is done to allow further LINEAR (i.e., fast) INTERPOLATION in the tables
c "OPAC...". A further interpolation along hydrogen mass fraction will be
c always quadratic (see a file "OPAC.04" in an evolutionary package).
c
c The above "x**" parameter grid is a sum of the OPAL and Alexander's tables
c grids in temperature, while grid in R-parameter is the same in both tables.
c The resulting parameter grid of the tables "O**" is as follows:
c log10(temperature)  =  3.0 (0.05/igrid) 6.0 (0.10/igrid) 8.1 (0.20/igrid) 8.7
c log10(R)            = -7.0 (0.50/igrid) 1.0
c If igrid=1, the Alexander + OPAL temperature grids are reproduced.
c
c            An user should find the biggest value of the parameter "igrid"
c            - according to amount of a fast memory available -
c
c            and PUT THIS VALUE ON A PARAMETER LIST OF THE SUBR. "opacity2".
c
c Hereinafter, "kappa" stands for a coefficient of opacity ([c.g.s.]).
c..............................................................................
c INPUT:
c         "GN93hz.dat"  - the OPAL opacities                       (disk file)
c       "Alexande.dat"  - molecular & grain opacities              (disk file)
c                 Z     - metal mass fraction to be used              (screen)
c                 Xenv  - Hydrogen mass fraction for "OPAC_ENV.DAT"   (screen)
c
c       from a parameter list of sbr."opacity2":
c              igrid    - make an output-opacity-table grid
c                         igrid-times denser (in temperature
c                         and R-parameter) than the source one
c                         ( R = 1.e18*density/temperature**3)
c
c OUTPUT:
c
c   "X00, X10, X20, X35, X50, X70, X80"  - log10(kappa), intermediate results
c                                          for the standard grid
c   "OPAC_7.DAT" - log10(kappa), dlog(Kap)/dlog(rho), dlog(Kap)/dlog(T) for
c              7 values of hydrogen abundances (X=0.0,0.1,0.2,0.35,0.5,0.7,0.8)
c   "OPAC_ENV.DAT" - log10(kappa), dlog(Kap)/dlog(rho), dlog(Kap)/dlog(T) for
c                    a given (from input) X in the stellar envelope
c..............................................................................
c
      real*4 logtar,logt,logkap,logrp,logr,lkappa
      character*12 opacx
      common /e/opact,dopact,dopacr,dopactd
      save /e/
      parameter (logrp=-7.00,  dlogr=0.50,  nnr= 17, nnt=85, nnx=7)
      dimension logtar(nnt),opacx(nnx),ropal(nnr)
      dimension xopal(nnx),lkappa(nnt,nnr,nnx)
      common /comenv/ xenv
      data ropal /-7.0, -6.5, -6.0, -5.5, -5.0, -4.5, -4.0, -3.5,
     #            -3.0, -2.5, -2.0, -1.5, -1.0, -0.5,  0.0,  0.5,  1.0/
      data xopal/ 0.00,  0.10,  0.20,  0.35,  0.50,  0.70,  0.80/
      data opacx/'x00','x10','x20','x35','x50','x70','x80'/
c
c  logr=log10(R), where R=density[c.g.s.]/(temperature[K]/1.e6)**3,
c  hydrogen mass fraction, X = xopal(k).
c  The ranges of R and X partly agree with the OPAL ranges.
c
c **************************************************************************
       open(2, FILE='GN93hz.dat')        ! OPAL GN93
c       open(2, FILE='OPAL_A04.dat')      ! OPAL A04
c       open(2, FILE='OPCD3_GN93.dat')    ! OP   GN93
c       open(2, FILE='OPCD3_A04.dat')     ! OP   A04  (our new standard)
c **************************************************************************
      do 111 i=1,60
  111 logtar(i)=      3.00+(i-1) *0.05
      do 112 i=61,81
  112 logtar(i)=      6.00+(i-61)*0.10
      do 113 i=82,nnt
  113 logtar(i)=      8.10+(i-82)*0.20
c..........................................begin of screen input
      write(*,119)
  119 format(//,24x,'  heavy element mass fraction Z = ',$)
      read (*,*) z
      write(*,120)
  120 format(/,20x,'  hydrogen mass fraction in envelope Xenv = ',$)
      read (*,*) xenv
c..........................................  end of screen input
      if (z+xopal(nnx).gt.1.) stop 'X + Z > 1'
      if (xenv.gt.xopal(nnx))  stop 'Xenv > max X(tabl)'
c
      do 2 k=1,nnx
      do 2 j=1,nnr
      do 2 i=1,nnt
    2 lkappa(i,j,k)=1.0e35
      do 1 k=1,nnx
c----------------------------------------------------------- begin of x-loop
           x=xopal(k)
      do 1 j=1,nnr
c-------------------------------------------- begin of r-loop
        logr=logrp+(j-1)*dlogr
      do 1 i=1,nnt
c----------------------------- begin of t-loop
        logt=logtar(i)
        t   =10.**logt
        t6  =t/1.0e6
c
c If log10(t) < 3.95, the Alexander's tables are used .
c                ----------------------------------    
      if(i.lt.20)call alexa(x,z,logt,logr,logkap)
c                ----------------------------------    
c
      if(i.eq.20)then
c fit the Alexander's and the OPAL tables at log10(t)=3.95.
c For this temperature, the mean (in R-parameter) relative differences
c between the above tables are typically 2 % for X > 0 and 4 % for X = 0.
c     ----------------------------------    
      call alexa(x,z,logt,logr,logkap)
c     ----------------------------------    
      if(k.eq.1.and.j.le.2)t6=1.000001*t6
c     ----------------------------------    
      call opacgn93(z,x,t6,10.0**logr)
c     ----------------------------------    
      logkap=(logkap+opact)/2.0
      end if
c
      if(i.gt.20)then
c If log10(t) > 3.95, the OPAL tables are used.
c      -----------------------------------------------------------------------
c      Some of the original OPAL high-density/temperature grid points are
c      not filled. They will be filled later by linear extrapolation to make
c      tables rectangular as requested by splines subroutines. This is not
c      very crucial to have exact values of radiative opacities in these nodes
c      as the effective opacity is dominated by conductive opacity there.
       if(i.ge.73.and.j.ge.17 .or.
     $    i.ge.74.and.j.ge.16 .or.
     $    i.ge.76.and.j.ge.15 .or.
     $    i.ge.80.and.j.ge.14 .or.
     $    i.ge.85.and.j.ge.13)        go to 1
c      -----------------------------------------------------------------------
c      to avoid possible, numerically induced, small exceeding of table range:
       if(i.eq.72.and.j.eq.17 .or.
     $    i.eq.73.and.j.eq.16 .or.
     $    i.eq.75.and.j.eq.15 .or.
     $    i.eq.79.and.j.eq.14 .or.
     $    i.eq.84.and.j.eq.13 .or.
     $    i.eq.85.and.j.lt.13)        t6=0.999997*t6
c      -----------------------------------------------------------------------
c     ----------------------------------    
      call opacgn93(z,x,t6,10.0**logr)
c     ----------------------------------    
                               logkap=opact
      end if
c
      lkappa(i,j,k)=logkap
c
    1 continue
c----------------------------- end of t-loop
c------------------------------------------ end of r-loop
c------------------------------------------------------- end of x-loop
c
c.......................................fill empty space in the OPAL tables
c                                       by linear extrapolation
      do 115 k=1,nnx
      do 115 i=1,nnt
      do 115 j=1,nnr
      if(lkappa(i,j,k).gt.15.0)then
      if(j-2.lt.1.or.lkappa(i,j-1,k).gt.15.0
     $.or.lkappa(i,j-2,k).gt.15.0 ) stop 'empty space filling fails'
      lkappa(i,j,k)=2.0*lkappa(i,j-1,k)-lkappa(i,j-2,k)
      end if
  115 continue
c
c.......................................now, add conductive opacity
      do 11 k=1,nnx
      x=xopal(k)
        do 11 j=1,nnr
        logr=logrp+(j-1)*dlogr
          do 11 i=1,nnt
          t  =10.0**logtar(i)
          t6 =t/1.0e6
          rho=10.0**logr*t6**3
          y  =1.0-x-z
c         ----------------------------------    
          call condop(x,y,z,rho,t,ck)
c         ----------------------------------    
          lkappa(i,j,k)=-log10(1.0/10.0**lkappa(i,j,k)+1./ck)
   11 continue
c
c.......................................save intermediate results on disk
c                                       as the "x**" files
      do 22 k=1,nnx
      open(1,file=opacx(k),status='unknown')
      rewind(1)
      x=xopal(k)
      write(1,101)z,x
      write(1,100)
      write(1,100)
      write(1,102)
      write(1,100)
      write(1,103)(ropal(j),j=1,nnr)
      write(1,100)
      do 23 i=1,nnt
      write(1,100)logtar(i),(lkappa(i,j,k),j=1,nnr)
   23 continue
      close(1)
   22 continue
  100 format(1x,f11.6,17f7.3)
  101 format(1x,' Z =',1pd15.6,'          X =',1pd15.6)
  102 format(40x,'log R')
  103 format(1x,'   log T   ',17f7.3)
c correct (if necessary) format 100,103 according to an actual nnr-value
c..............................................................................
c
      do 222 k=1,nnx
      do 222 j=1,nnr
      do 222 i=1,nnt
      if(abs(lkappa(i,j,k)).gt.15.0)then
      write(*,*)'i,j,k=',i,j,k
      stop 'empty space not filled'
      end if
  222 continue
c .................now, calculate kappa for denser grid in log10(t) & log10(R),
c                                 by applying cubic splines to the tables "x**"
c     -----------------
      call opacity2(z)
c     -----------------
c
      stop '  O.K.'
      end
c *****************************************************************************
      subroutine alexa(x,z,logt,logr,logkap)
      implicit double precision (a-h,o-z)
      real*4 x,z,logt,logr,logkap
        xd=dble(x)
        zd=dble(z)
      tlog=dble(logt)
      rlog=dble(logr)
      call dra95(xd,zd,tlog,rlog,dlgkap)
      logkap=real(dlgkap)
      return
      end
c *****************************************************************************
      subroutine dra95(x,z,tlog,rlog,logkap)
      implicit double precision(a-h,o-z)
      real*8 logkap
      character*18 a18
      character*2  a2
c read-in and interpolate (in Z) from the Alexander's tables.
c Calculate opacity for given Z and grid-point (!) values of other parameters
c input:
c        x      = hydrogen mass fraction
c        z      = metal    mass fraction
c        tlog   = log10(temperature)
c        rlog   = log10(density/temperature**3) + 18
c output:
c        logkap = log10(Rosseland mean opacity coefficient)      [c.g.s.]
c-data--------------------------------------------------------------------
c                                                                      X=0..8
      parameter (nx=7, nz=15, nt=23, nr=17)
      parameter (tmax=4.1d0, det=0.05d0, rmin=-7.0d0, d=1.0d-7)
      parameter (zero=1.0d-30)
      dimension o(nx,nz,nt,nr),xtab(nx),ztab(nz),ttab(nt),rtab(nr)
      common /drac/oz(nx,nt,nr),imemo
      save  /drac/
c                                                                      X=0..8
      data xtab /0.0d0, 0.1d0, 0.2d0, 0.35d0, 0.5d0, 0.7d0, 0.8d0/
      data ztab /0.1d0, 0.08d0, 0.06d0, 0.04d0, 0.03d0, 0.02d0, 0.01d0,
     #           0.004d0, 0.002d0, 0.001d0, 0.0003d0, 0.0001d0,
     #           0.00003d0, 0.00001d0, 0.0d0/

      d1 = 1.0d0 + d
c                   for some numerical reasons, X and Z are assumed
c                   to be equal to a very small value "zero" rather than 0
      ztab(nz)=max(ztab(nz),zero)
      xtab(1) =max(xtab(1), zero)
      xh      =max(x,       zero)
      zet     =max(z,       zero)
      if(z.gt.d1*ztab(1))        stop 'Z > 0.1'
      if(z.lt.0.0d0)                stop 'Z < 0  '
      do 4 k=1,nt
    4 ttab(k)=tmax-(k-1)*det
c                     this R-range is the same as that in the OPAL tables
      do 13 k=1,nr
   13 rtab(k)=rmin+(k-1)*0.5d0
c-data--------------------------------------------------------------------
c                       if not done yet, read the Alexander's tables
      if(imemo.ne.99)go to 5

    6 continue
c             this subroutine is called only for x, tlog, rlog grid-point values
      do 14 i=1,nx
      if(abs(1.-xh/xtab(i)).lt.d)go to 15
   14 continue
      stop 'x fit no X-grid-point value'
   15 continue
      do 16 k=1,nt
      if(abs(1.-tlog/ttab(k)).lt.d)go to 17
   16 continue
      write(*,*)'tlog=',tlog
      stop 'tlog fit no logT-grid-point value'
   17 continue
      do 18 l=1,nr
      if(abs(rtab(l)).lt.d)then
      if(abs(rlog)   .lt.d)then
      go to 19
      else
      go to 18
      end if
      end if
      if(abs(1.0d0-rlog/rtab(l)).lt.d)go to 19
   18 continue
      stop 'rlog fit no logR-grid-point value'

   19 logkap=oz(i,k,l)
      return

    5 imemo=99
      open(1,file='Alexande.dat',status='old')
      rewind(1)
      do 91 i=1,nx
      do 91 k=1,nt
      do 91 l=1,nr
      oz(i,k,l)=1.0d35
   91 continue
      do 92 i=1,nx
      do 92 j=1,nz
      do 92 k=1,nt
      do 92 l=1,nr
      o(i,j,k,l)=1.0d35
   92 continue
c
c      Map of requested (x) and calculated (xc)
c      Alexander's molecular & grain opacity tables on the X,Z plane
c
c  X /Z=.10 .08 .06 .04 .03 .02 .01 4-3 2-3 1-3 3-4 1-4 3-5 1-5 .00
c .00   xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc
c .10   xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc
c .20   xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc
c .35   xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc
c .50   xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc
c .70   xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc
c .80   xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc  xc
c      Actually, the tables below are not taken into account
c .90   x   x   x   x   x   x   x   x   x   x   x   x   x   x   x
c
c .92       x
c                                         X + Z <= 1 for each x-node
c .94           x
c .95   o   o   o   x   x   x   x   x   x   x   x   x   x   x   x
c .96               x
c                                         X + Z >  1 for each o-node
c .97                   x                        (has to be omitted)
c
c .98                       x
c                                         X + Z =  1 along
c .99                           x  <----- this diagonal x-line
c                                         
c .996                              x     
c                                         
c .998                                  x

c .999                                      x

c .9997                                         x

c .9999                                             x

c .99997                                                x

c .99999                                                    x

c1.00000                                                        x
c
c
      do 3 i=1,nx
      do 3 j=1,nz
      read(1,1,end=20)a18,xx,a2,zz
      xx=max(xx,zero)
      zz=max(zz,zero)
      if(abs(1.-xx/xtab(i)).gt.d)stop 'xx.ne.xtab'
      if(abs(1.-zz/ztab(j)).gt.d)stop 'zz.ne.ztab'

      do 3 k=1,nt
      read(1,2)nk,nl,tt,tr,(o(i,j,k,l),l=1,nr)
      if(nk.ne.k)                stop 'nk.ne.k'
      if(nl.ne.nr)               stop 'nl.ne.nr'
      if(abs(1.0d0-tt/ttab(k)).gt.d)stop 'tt.ne.ttab'
      if(abs(1.0d0-tr/rmin)   .gt.d)stop 'tr.ne.rmin'
    3 continue
      close(1)
c             check if requested Z-value is equal to one of Z-grid-point values.
c             If so, skip the following parabolic Z-interpolation
      do 7 j=1,nz
      if(abs(1.0d0-zet/ztab(j)).lt.d)go to 8
    7 continue
      do 10 j=1,nz-1
      if(zet.lt.ztab(j).and.zet.gt.ztab(j+1))go to 11
   10 continue
                                 stop 'check Z'
   11                                  n=j
      if(ztab(j)-zet.gt.zet-ztab(j+1)) n=j+1
      if(n.eq.1)                       n=2
      if(n.eq.nz)                      n=nz-1
      zetl=log10(zet)
      x1=log10(ztab(n-1))
      x2=log10(ztab(n  ))
      x3=log10(ztab(n+1))
      do 12 i=1,nx
      do 12 k=1,nt
      do 12 l=1,nr
      y1=o(i,n-1,k,l)
      y2=o(i,n  ,k,l)
      y3=o(i,n+1,k,l)
      aa=((y1-y3)/(x1-x3)-(y2-y3)/(x2-x3))/(x1-x2)
      bb=(y2-y3)/(x2-x3)-aa*(x2+x3)
      cc=y3-aa*x3**2-bb*x3
      oz(i,k,l)=aa*zetl**2 + bb*zetl + cc
   12 continue
      go to 6

    8 continue
      do 9 i=1,nx
      do 9 k=1,nt
      do 9 l=1,nr
      oz(i,k,l)=o(i,j,k,l)
    9 continue
      go to 6

    1 format(a18,f6.4,a2,f7.5)
    2 format(i2,i3,f6.3,f5.1,8f8.3/9f8.3)
   20 stop 'e.o.f.'
      end
c *****************************************************************************
c-----------------------------------------------------------
c This is a part of the file  OPAC.FOR               1/30/90
c written by P. Haensel, with some changes by RS     2/13/95
c-----------------------------------------------------------
c Subroutine calculates plasma opacity, assuming heat
c transport to be dominated by electron conduction.
c Formulae of Yakovlev&Urpin, tested/compiled by P.Haensel.
c Input: composition x,y,z, density "rho", temperature "t",
c x - fraction of hydrogen
c y - fraction of helium4
c z - fraction of O16
c Output: coefficient of conductive opacity "ck"   [c.g.s.]
c----------------------------------------------------------
      subroutine condop(sx,sy,sz,srho,st,ck)
      implicit double precision(a-h,o-z)
      real*4 sx,sy,sz,srho,st,ck
      common/deg/Tf
      x  =dble(sx)
      y  =dble(sy)
      z  =dble(sz)
      rho=dble(srho)
      t  =dble(st)
      Aav=1./(x+y/4.0d0+z/16.0d0)
      Zav=Aav*(x+y/2.0d0+z/2.0d0)
      vcond=cond(Zav,Aav,rho,t)
      T5=t/1.0d5
      rho5=rho/1.0d5
      opac=3.024d6*T5**3/rho5/vcond
c      if(Tf.lt.5.0d0*t)opac=1.0d35
      ck=sngl(opac)
      return
      end
c *****************************************************************************
c--------------------------------------------------------
c Thermal conductivity of dense matter below neutron
c drip. Assumptions:
c   a) electrons are strongly degenerate
c   b) electrons form an ideal  Fermi gas. Valid for
c      density>>A*Z.
c   c) e-ion scattering is elastic: temperature much
c      larger than me/m(ion)*(me*c**2/kB)*x**2=Tc
c
c---------------------------------------file cond2.for--
      double precision function cond(Z,A,rho,t)
      implicit double precision(a-h,l,m,o-z)
      common/deg/Tf
c
c notation as in : Yakovlev,Urpin(1980),Astr.Zh.57,526
c
c------------ main parameters--
c T6 - temperature in 10**6 K.
c x=pf/(me*c) - electron relativistic parameter
c Tf - electron Fermi temperature
c alfaB=e**2/(hbar*vf) - electron Born parameter
c gamma - ion coupling parameter
c Tpi - ion plasma temperature
c Tc  -  critical temperature. For T>>Tc electron scattering
c        on nuclei is elastic.
c rho6 - density in 10**6 g/cm**3
c-------------
      pi=dacos(-1.0d0)
      rho6=rho/1.0d6
      T6=t/1.0d6
      x=1.0067d0*(rho6*Z/A)**(1.0d0/3.0d0)
      Tf=5.930d9*(dsqrt(1.0d0+x**2)-1)
      alfaB=1.0d0/(137.036d0*x)*dsqrt(1.0d0+x**2)
      gamma=22.74d0/T6*Z**(5.0d0/3.0d0)*(rho6*Z/A)
     1                               **(1.0d0/3.0d0)
      Tpi=7.832d6*dsqrt(rho6)*Z/A
      Tc=3.251d6*x**2/A
c
c   The melting point : gamma=178 has been calculated by
c   Slattery et al.(1982), Phys.Rev.A26, 2255.
c
c-------------- calculation of the electron relaxation time --
c
      if(gamma.gt.178.0d0) go to 100
c
c ---- liquid or gaseous ion phase --
c lambda - Coulomb logarithm
c lambdaB -  - - - - in the Born approximation
c
c We use the formulae of Yakovlev,Urpin(1980)
c
      lambdaB=dlog((2*pi*Z/3.0d0)**(1.0d0/3.0d0)*
     1        dsqrt(3.d0/2.d0+3.0d0/gamma))-x**2/(2*(1.0d0+x**2))
c
c dlambda - correction to lambdaB calculated by Yakovlev(1986)
c           Astr.Zh.64,661.May be important for large Z.
c
      beta2=x**2/(1.0d0+x**2)
      dlambda=0.5d0*pi*alfaB*beta2*(1.0d0+1.3d0*alfaB)/
     1     (1.0d0+alfaB**2*(0.71d0-0.54d0*beta2))
      lambda=lambdaB+dlambda
c
c taue16 - electron relaxation time in the units of 10**(-16) s.
c
               taue16=0.570d0/(dsqrt(1.0d0+x**2)*Z*lambda)
c
      go to 200
c
c------
100   continue
c
c-------- Solid regime for ions. Formulae derived in ------------
c         Raikh,Yakovlev(1982),As.Space Sci.87, 193.
c
      tt=t/Tpi
      F=(2.0d0+x**2)*13.0d0/(1.0d0+x**2)/dsqrt(1.0d0+0.017d0/tt**2)+
     1   (3*(dlog(Z)/3.d0+0.86d0)-x*2/(1.0d0+x**2)-1.0d0)
     2   *0.1d0/(tt**2*(1.0d0+0.0116d0/tt**2)**(1.5d0))
      xtau=0.096d0*T6*dsqrt(1.0d0+x**2)/x*F
c
              taue16=1.d0/xtau
c------------------------------------------------------------------
200   continue
c
c---- Thermal conductivity of dense matter in c.g.s. units
c
              cond=4.034d15*x**3/dsqrt(1.0d0+x**2)*T6*taue16
c
      return
      end
c *****************************************************************************
c
c ***********    begin of THE LIVERMORE LABORATORY OPAL PACKAGE     ***********
c
c   - "OPACGN93" - reads and interpolates interiors opacities from the disk
c                  file "GN93hz.dat", which contains the Livermore Laboratory
c                  radiative opacities (OPAL, table "GN93hz")
c..............................................................................
c October, 1995;
c + some programming language changes (to satisfy NDP compiler)  added by RS
c
c *****************************************************************************
      subroutine instruct
c-----VERSION of October 5, 1995-----------------------------------------
c-----------------------------------------------------------------------

c          This subroutine contains instructions for using the subroutine
c     OPACGN93( z, xh, t6, R ) and OPAC(izi,mzin,xh,t6,r).
c          The purpose of these subroutines is to perform 3 or 4 variable
c     interpolation on log10(kappa).  The opacity tables to be interpolated
c     are known to have somewhat random numerical errors of a few percent.
c     Consequently, adjusting the data prior to performing the interpolation is
c     justified at this level.  These codes are set-up to read the original
c     (unsmoothed) tabular data, this data is then passed through a smoothing
c     filter; using a set of routines developed by Mike Seaton (see M. J. Seaton
c     MNRAS 265,L25(1993)). It is the adjusted data that is actually used in
c     carrying out the interpolations in OPACGN93 and OPAC.  The interpolation
c     method, described below,  also produces smooth values for logK and its
c     first derivatives. The initial adjustment step helps improve the smoothness
c     of the OPACGN93 and OPAC  output,particularly at the smallest values of R.
c     The medium to large R output is only slightly effected by this step.  It
c     takes only a few seconds to carryout the initial data smoothing step. This
c     step can be skipped by setting the parameter ismdata in subroutine
c     readco =1.

c          The interpolation variables are :
c
c          z       The metallicity, Z
c          xh      The hydrogen mass fraction, X
c          t6      The temperature in millions of degrees Kelvin, T6
c          r       =rho(gm/cc)/T6**3, R
c
c          Additional input to OPAC is:

c          izi     Keeps or recalculates table indices. The value 0 causes
c                  the table indices to be recalculated.  A value other than 0
c                  causes the previous indices to be used.

c          mzin    The integer value of i of the Z value to use.  The
c                  choices are:
c                  1=0.0  2=0.0001 3=0.0003 4=0.001 5=0.002 6=0.004 7=0.01
c                  8=0.02 9=0.03  10=0.04  11=0.06 12=0.08 13=0.1
c
c
c          An interpolation between overlapping quadratics is used to obtain
c     smoothed results.  A 4x4 grid in logT6 and logR is used to interpolate
c     in four different 3x3 sub-grids. Linear interpolation between quadratic
c     fits in these different sub-grids gives smoothed results in both log T6
c     and Log R directions. Compared to ealier versions of this code, the
c     interpolation in Z is in Kappa vs. Z; not log Kappa vs. Z.
c     The overlapping quadratic procedure produces results that are  smooth,
c     similar to bicubic spline interpolation, but require storage of only local
c     information.
c
c          The code OPACGN93 performs interpolation in Z, X, T6, and R. It calls
c     the subroutine OPAC at each Z.  If you are working with a fixed Z, as
c     listed above, it is more efficient to call OPAC directly. In this case use
c     izi=0 and the appropiate value of mzin. The  opacity data will be read
c     from unit 2 in the subroutine readco.  you will need to have the file
c     GN93hz.dat available on disk.
c         Each of the individual tables in the GN93hz.dat file cover 70 temperatures
c     in the range logT=3.75[T6=0.0056341325]( referred to as temperature 1) to
c     logT=8.7[T6=500]. and 19 values of log R in the range -8 (referred to as 1)
c     to +1; at half-integer steps.  (NOTE: earlier tables were explicitly in
c     terms of T6. For convenience the present tables tabulate log Kappa vs logT. The
c     interpolation however still uses T6 for the temperature variable)
c     For specialized problems, if storage space is a problem, a reduced set of
c     data can be input .  This requires a recompilation with altered parameter
c     values.  In order to limit the range of R, set the parameter nrb= index of
c     lowest value of log R to use(count from log R=-8).  Then set the parameter
c     nre to the index of the largest value of log R to use.  (NOTE: nre-nrb must
c     be at least 5). To ignore the first ntb-1 values of T6 (starting from
c     temperature 1) set the parameter ntb to desired value.  No other parameters
c     should be modified.

c     ***CAUTION***
c         As a result of the mixing procedure used to calculate the data a few
c     X=0.0, low T-small R, table values fell outside the range of T and R accessible
c     from the X=0.35 data directly calculated for this purpose.  These T-R
c     locations are filled in with 9.99 (or for diagnostic purposes in some cases
c     larger values.  At the same locations the derivatives are set to 99.9.  When
c     T-R falls in this region a message is issued by the interpolation code to
c     inform the user of this situation.  Presumable very few users will have
c     applications that take them into this region.
c
c          Your routine that performs the call to OPAC should include the
c      statement:
c
c         common/e/ opact,dopact,dopacr,dopactd
c
c         These variables have the following meanings:
c
c         OPACT        Is the Log of the Rosseland mean opacity: Log(kappa)
c         DOPACT      Is Dlog(kappa)/Dlog(T6)   at constant R
c         DOPACR      Is Dlog(kappa)/Dlog(R)    at constant T
c         DOPACTD     Is Dlog(kappa)/Dlog(T6)   at constant Rho
c
      dum=0.0
      return
      end

c *****************************************************************************
      subroutine opacgn93 (z,xh,t6,r)
c.....The purpose of this subroutine is to interpolate the data along Z
      save
      parameter (mx=10,mz=13,nrm=19,nrb=1,nre=19,nr=nrm+1-nrb
     . ,ntm=70,ntb=1,nt=ntm+1-ntb)
      common/a/ mzz, xz(mx,mz,nt,nr),
     . t6list(nt),alr(nr),n(mx),alt(nt),opk(nt,nr),opk2(nt,nr),dfsx(mx)
     . ,dfs(nt),dfsr(nr),dfsz(mz),a(3,mx),b(3),m,mf,xa(mx)
     . ,alrf(nrm),xzf(nt,nr),t6listf(ntm),za(mz)
c..... OPACT- opacity obtained from a quadraric interpolation at
c      fixed log T6 at three values of log R; followed by quadratic
c      interpolation along log T6. Results smoothed bt mixing
c      overlapping quadratics.
c..... DOPACT- is Dlog(k)/Dlog(T6) smoothed by mixing quadratics
c              at fixed R
c..... DOPACR- is  Dlog(k)/Dlog(R) smoothed by mixing quadratics.
c..... DOPACTD- is Dlog(k)/Dlog(T6) smoothed by mixing quadratics
c               at fixed rho
      common/e/ opact,dopact,dopacr,dopactd
      common/ee/ opl(mx,nt,nr),xx(mx),zza(mz)
      common/eee/m1,zval
      real kapz,kapz1,kapz2
      dimension kapz(mz),dkapdtr(mz),dkapdrt(mz)
      zval=z
      zzl=z   ! use zzl=log10(.0001+z) for log interpolation
      if(itime .ne. 12345678) then
      do i=1,mz
      zza(i)=za(i)  ! use zza=log10(0.0001+za(i)) for log interpolation
      enddo
      endif
      if(itime .ne. 12345678) then
        itime=12345678
      endif

      do i=1,mz
        if(abs(z-za(i)) .lt. 1.e-7 ) then
          izz=i
          call opac (0,izz,xh,t6,r)
          if (opact .gt. 9.0) write(*,*)'logK>9.0, X,Z,T6,R=',xh,z,t6,r
          return
        endif
      enddo

      ilo=2
      ihi=mz
    8 if(ihi-ilo .gt. 1) then
      imd=(ihi+ilo)/2
      if(z .le. za(imd)+1.e-7) then
        ihi=imd
      else
        ilo=imd
        endif
        go to 8
      endif
      i=ihi
      m1=i-2
      m2=i-1
      m3=i
      m4=i+1
      mfm=m4
c.....check whether Z is near a table limit
      if((z .le. za(2)+1.e-7) .or. (z .ge. za(mz-1))) mfm=m3
c.....  Check if Z+X interpolation sums exceed unity at needed indices.
c       If so, backup to lower Z indices to perform interpolation.
c       This should work OK, due to density of Z-grid points and the
c       slow Z variation(except at very small Z)
      if(xh+za(mfm) .gt. 1.) mfm=m3
        if(xh+za(mfm) .gt. 1.) then
      if(m1.le.1)stop 'special case: X,Z location not covered by logic'
          m1=m1-1
          m2=m2-1
          m3=m3-1
        endif
c
      izi=0
      do iz=m1,mfm
        izz=iz
        call opac(izi,izz,xh,t6,r)
          if (opact .gt. 9.0) write(*,*)'logK>9.0, X,Z,T6,R=',xh,z,t6,r
        izi=1
        kapz(iz)=10.**opact ! converts logK to K
        dkapdtr(iz)=dopact
        dkapdrt(iz)=dopacr
      enddo
      is=0
      iw=1
      kapz1=quad(is,iw,zzl,kapz(m1),kapz(m2),kapz(m3)
     x ,zza(m1),zza(m2),zza(m3))
      is=1
      dkapz1=quad(is,iw,zzl,dkapdtr(m1),dkapdtr(m2),dkapdtr(m3)
     x ,zza(m1),zza(m2),zza(m3))
      dkapz3=quad(is,iw,zzl,dkapdrt(m1),dkapdrt(m2),dkapdrt(m3)
     x ,zza(m1),zza(m2),zza(m3))
      if (mfm .eq. m3) then
        opact=log10(kapz1)   ! converts K to logK
        dopact=dkapz1
        dopacr=dkapz3
        dopactd=-3.*dopacr+dopact
        is=0

        return

      endif
      is=0
      iw=2
      kapz2=quad(is,iw,zzl,kapz(m2),kapz(m3),kapz(m4)
     x ,zza(m2),zza(m3),zza(m4))
      is=1
      dkapz2=quad(is,iw,zzl,dkapdtr(m2),dkapdtr(m3),dkapdtr(m4)
     x ,zza(m2),zza(m3),zza(m4))
      dkapz4=quad(is,iw,zzl,dkapdrt(m2),dkapdrt(m3),dkapdrt(m4)
     x ,zza(m2),zza(m3),zza(m4))
      dix=(zza(m3)-zzl)*dfsz(m3)
      opact=log10(kapz1*dix+kapz2*(1.-dix))   ! converts K to logK
      dopact=dkapz1*dix+dkapz2*(1.-dix)
      dopacr=dkapz3*dix+dkapz4*(1.-dix)
      dopactd=-3.*dopacr+dopact
      is=0
      return
      end
c *****************************************************************************
      subroutine opac (izi,mzin,xh,t6,r)
c..... The purpose of this subroutine is to interpolate log kappa
c      in in X, T6, R
c        izi=0 recalulate table indices to use; =1 keep previous
c        mzin=index of za(i) in block data. za(i) are metallicities
c        t6=T6=temperature in millions of degrees kelvin
c        r=R=density(g/cm**3)/T6**3
c..... to use opac insert common/e/ in the calling routine.
c      This common contains interpolated values for kappa and its
c      first derivities.
c
      save
      integer w
      parameter (mx=10,mz=13,nrm=19,nrb=1,nre=19,nr=nrm+1-nrb
     . ,ntm=70,ntb=1,nt=ntm+1-ntb)
      common/aa/ q(4),h(4),xxh
      common/a/ mzz, xz(mx,mz,nt,nr),
     . t6list(nt),alr(nr),n(mx),alt(nt),opk(nt,nr),opk2(nt,nr),dfsx(mx)
     . ,dfs(nt),dfsr(nr),dfsz(mz),a(3,mx),b(3),m,mf,xa(mx)
     . ,alrf(nrm),xzf(nt,nr),t6listf(ntm),za(mz)
      common/b/ itab(mx,mz),nta(nr),x(mx,mz),y(mx,mz),
     . zz(mx,mz)
      common/d/dkap
      common/bb/l1,l2,l3,l4,k1,k2,k3,k4,ip,iq
      common/ee/ opl(mx,nt,nr),xx(mx),zza(mz)
      common/eee/m1,zval
c..... OPACT- opacity obtained from a quadraric interpolation at
c      fixed log T6 at three values of log R; followed by quadratic
c      interpolation along log T6. Results smoothed bt mixing
c      overlapping quadratics.
c..... DOPACT- is Dlog(k)/Dlog(T6) smoothed by mixing quadratics
c              at fixed R
c..... DOPACR- is  Dlog(k)/Dlog(R) smoothed by mixing quadratics.
c..... DOPACTD- is Dlog(k)/Dlog(T6) smoothed by mixing quadratics
c               at fixed rho
      common/e/ opact,dopact,dopacr,dopactd
c
      iop=1   ! provides smoothed interpolations; iop=0 gives no smoothing
      mzz=mzin
      z=za(mzz)

      if(nre .lt. 6) go to 65
c
      if((izi .eq. 0) .and. (z+xh-1.e-6 .gt. 1 )) go to 61
      if((izi .ne. 0) .and. (zval+xh-1.e-6 .gt. 1 )) go to 61
      xxh=xh
      xxi=xh
      t6i=t6
      ri=r
c
      xxx=log10(.005+xh)
      slt=log10(t6)
      slr=log10(r)
c
      if(itime .ne. 12345678) then
        itime=12345678
        do  i=1,mx
          xx(i)=log10(.005+xa(i))
        enddo
c..... this is the first time throught. Calculate the decadic
c      log of the perimeter points shifted by Z. m refers to
c      xa(m); the hydrogen table value.

c..... read the data files
        call readco
        xamx1=xa(mx-1)
        xxmx1=xx(mx-1)
        dfsxmx1=dfsx(mx-1)
      endif
      mxend=mx
      xa(mx)=1.-z
      xa(mx-1)=xamx1
      xx(mx-1)=xxmx1
      dfsx(mx-1)=dfsxmx1
        if (xa(mx) .lt. xa(mx-1)) then
          mxend=mx-1
          xa(mxend)=xa(mx)
        endif
        if (xh .ge. 0.8 ) then
          xx(mxend)=log10 (0.005+xa(mxend))
          dfsx(mxend)=1./(xx(mxend)-xx(mxend-1))
        endif
c
c
c..... Determine log R and log T6 grid points to use in the
c      interpolation.
      if((slt .lt. alt(1)).or.(slt .gt. alt(nt))) go to 62
      if((slr .lt. alr (1)).or.(slr .gt. alr(nre))) go to 62
c
c
c
      if (izi .eq. 0) then  ! freeze table indices
        ilo=2
        ihi=mx
    8   if(ihi-ilo .gt. 1) then
          imd=(ihi+ilo)/2
            if(xh .le. xa(imd)+1.e-7) then
              ihi=imd
            else
              ilo=imd
            endif
          go to 8
        endif
        i=ihi
        mf=i-2
        mg=i-1
        mh=i
        mi=i+1
        mf2=mi
        if (xh .lt. 1.e-6) then
        mh=1
        mg=1
        mi=2
        mf2=1
        endif
        if((xh .le. xa(2)+1.e-7) .or. (xh .ge. xa(mx-2)-1.e-7)) mf2=mh
c
        ilo=2
        ihi=nre
   12     if(ihi-ilo .gt. 1) then
          imd=(ihi+ilo)/2
            if(slr .le. alr(imd)+1.e-7) then
              ihi=imd
            else
              ilo=imd
            endif
          go to 12
          endif
        i=ihi
        l1=i-2
        l2=i-1
        l3=i
        l4=l3+1
c
        ilo=2
        ihi=nt
   11     if(ihi-ilo .gt. 1) then
          imd=(ihi+ilo)/2
            if(t6 .le. t6list(imd)+1.e-7) then
              ihi=imd
            else
              ilo=imd
            endif
          go to 11
          endif
        i=ihi
        k1=i-2
        k2=i-1
        k3=i
        k4=k3+1
        l3s=l3+nrb-1
        k3s=k3+ntb-1
      endif

      kmin=0
      k1in=k1
      iadvance=0
      mfin=mf
      if ((mfin .eq. 1) .and. (xz(1,mzz,k1,l1) .gt. 9.)) then   ! no data
      do i=1,6
        if (xz(1,mzz,i,l1) .gt. 9.)  then
          if (xh .lt. .1) then
           kmin=i+1
          else

            if (iadvance .eq. 0) then
            iadvance=iadvance+1
            mf=mf+1
            mg=mg+1
            mh=mh+1
            mi=mi+1
            mf2=mf2+1
            endif
          endif
        endif
      enddo
      if ((iadvance .eq. 0) .and. (k1 .le. kmin) .and.
     x    (slt .le. alt(kmin))) then
      k1=kmin
      if ((xz(1,mzz,kmin,l1+1) .lt. 9.) .and.
     x   ((slr+.01) .gt. alr(l1+1))) then
      l1=l1+1
      kmin=0
      k1=k1in
      do i=1,6
      if (xz(1,mzz,i,l1) .gt. 9.) kmin=i+1
      enddo
      if ((kmin .ne. 0) .and. (k1in .lt. kmin)) k1=kmin
      endif
      endif
      if ((slt+.001) .lt. alt(k1)) then
      opact=30.
      dopact=99.
      dopacr=99.
      return
      endif
      l2=l1+1
      l3=l2+1
      l4=l3+1
      l3s=l3+nrb-1
      k2=k1+1
      k3=k2+1
      k4=k3+1
      k3s=k3+ntb-1
      endif
        do i=14,18   ! allows jagged edge at high T,rho
          if((l3s .gt. i) .and. (k3s .gt. nta(i+1))) go to 62
        enddo
      do 123 m=mf,mf2
      ip=3
      iq=3
      ntlimit=nta(l3s)
      if((k3. eq. ntlimit) .or. (iop .eq. 0)) then 
        ip=2
        iq=2
      endif
      if(t6 .le. t6list(2)+1.e-7) ip=2

      if((l3 .eq. nre) .or. (iop .eq. 0)) then 
       iq=2
       ip=2
      endif
      if ((l4 .le.nr) .and. (xz(m,mzz,k3,l4) .eq. .0)) iq=2
      if(slr .le. alr(2)+1.e-7) iq=2
c
      is=0
c
c__________
      do ir=l1,l1+iq
        do it=k1,k1+ip
        opl(m,it,ir)=xz(m,mzz,it,ir)
        is=1
        enddo
      enddo
  123 continue
      if((zz(mg,mzin) .ne. zz(mf,mzin)) .or.
     x    (zz(mh,mzin) .ne. zz(mf,mzin)))
     x stop 'Z does not match Z in GN93hz.dat files you are using'
      if(z .ne. zz(mf,mzin)) go to 66
c                  with return
      is=0
      iw=1
      do 45 ir=l1,l1+iq
        do it=k1,k1+ip
        if (mf2 .eq. 1) then
        opk(it,ir)=opl(mf,it,ir)
        go to 46
        endif
        opk(it,ir)=quad(is,iw,xxx,opl(mf,it,ir),opl(mg,it,ir)
     x  ,opl(mh,it,ir),xx(mf),xx(mg),xx(mh))
        is=1
   46   continue
        enddo
   45 continue

      if (mi .eq. mf2) then  ! interpolate between quadratics
      is=0
      iw=1
       dixr=(xx(mh)-xxx)*dfsx(mh)
      do 47 ir=l1,l1+iq
        do it=k1,k1+ip
        opk2(it,ir)=quad(is,iw,xxx,opl(mg,it,ir),opl(mh,it,ir)
     x  ,opl(mi,it,ir),xx(mg),xx(mh),xx(mi))
        opk(it,ir)=opk(it,ir)*dixr+opk2(it,ir)*(1.-dixr)
        is=1
        enddo
   47 continue
c     interpolate X between two overlapping quadratics
      endif

      is=0
c
c..... completed H,Z interpolation. Now interpolate T6 and log R on a
c      4x4 grid. (log(T6(i)),i=i1,i1+3),log(R(j)),j=j1,j1+3)).Procedure
c      mixes overlapping quadratics to obtain smoothed derivatives.
c
c
      call t6rinterp(slr,slt)
      return

   61 stop ' Mass fractions exceed unity'
   62 write(*,*)' T6/LogR outside of table range'
c      write(*,*)' slt,alt(1),alt(nt),slr,alr(1),alr(nre),l3s,i,k3s,
c     x nta(i+1) ',slt,alt(1),alt(nt),slr,alr(1),
c     x alr(nre),l3s,i,k3s,nta(i+1)
      stop
   64 stop ' X not equal to zero: To run this case it
     xis necessary to recompile with parameter (mx=1)'
   65 stop 'Too few R values; NRE+1-NRB < 6'
   66 stop 'Z does not match Z in codata* files you are using'
      end

c *****************************************************************************
      subroutine t6rinterp(slr,slt)
c     The purpose of this subroutine is to interpolate in logT6 and logR
      save
      parameter (mx=10,mz=13,nrm=19,nrb=1,nre=19,nr=nrm+1-nrb
     . ,ntm=70,ntb=1,nt=ntm+1-ntb)
      common/ee/ opl(mx,nt,nr),xx(mx),zza(mz)
      common/aa/ q(4),h(4),xxh
      common/a/ mzz, xz(mx,mz,nt,nr),
     . t6list(nt),alr(nr),n(mx),alt(nt),opk(nt,nr),opk2(nt,nr),dfsx(mx)
     . ,dfs(nt),dfsr(nr),dfsz(mz),a(3,mx),b(3),m,mf,xa(mx)
     . ,alrf(nrm),xzf(nt,nr),t6listf(ntm),za(mz)
      common/d/dkap
      common/bb/l1,l2,l3,l4,k1,k2,k3,k4,ip,iq
      common/e/ opact,dopact,dopacr,dopactd
c
      iu=0
      is=0
      do kx=k1,k1+ip
          iw=1
        iu=iu+1
        h(iu)=quad(is,iw,slr,opk(kx,l1),opk(kx,l2),opk(kx,l3),
     x  alr(l1),alr(l2),alr(l3))
          if(iq. eq. 3) then
            iw=2
            q(iu)=quad(is,iw,slr,opk(kx,l2),opk(kx,l3),opk(kx,l4),
     x      alr(l2),alr(l3),alr(l4))
          endif
        is=1
      enddo
c
      is=0
      iw=1
c..... k and Dlog(k)/dlog(T6) in lower-right 3x3(i=i1,i1+2 j=j1,j1+2)
      opact=quad(is,iw,slt,h(1),h(2),h(3),alt(k1),alt(k2),alt(k3))
      dopact=dkap
      dkap1=dkap
        if (iq. eq. 3) then
c.....    k and Dlog(k)/Dlog(T6) upper-right 3x3(i=i1+1,i1+3 j=j1,j1+2)
          opactq=quad(is,iw,slt,q(1),q(2),q(3),alt(k1),alt(k2),alt(k3))
          dkapq1=dkap
        endif
        if(ip .eq. 3) then
c.....    k and Dlog(k)/Dlog(T6) in lower-left 3x3.
          opact2=quad(is,iw,slt,h(2),h(3),h(4),alt(k2),alt(k3),alt(k4))
          dkap2=dkap
c.....    k and Dlog(k)/Dlog(T6) smoothed in left 3x4
          dix=(alt(k3)-slt)*dfs(k3)
          dopact=dkap1*dix+dkap2*(1.-dix)
          opact=opact*dix+opact2*(1.-dix)
        endif
        if(iq .eq. 3) then

c.....    k and Dlog(k)/Dlog(T6) in upper-right 3x3.
          opactq2=quad(is,iw,slt,q(2),q(3),q(4),alt(k2),alt(k3),alt(k4))
          dkapq2=dkap
          dopactq=dkapq1*dix+dkapq2*(1.-dix)
          opactq=opactq*dix+opactq2*(1.-dix)
        endif
c
      iu=0
      do lx=l1,l1+iq
        iw=1
        iu=iu+1
        h(iu)=quad(is,iw,slt,opk(k1,lx),opk(k2,lx),opk(k3,lx),
     x  alt(k1),alt(k2),alt(k3))
          if(ip .eq. 3) then
            iw=2
            q(iu)=quad(is,iw,slt,opk(k2,lx),opk(k3,lx),opk(k4,lx),
     x      alt(k2),alt(k3),alt(k4))
          endif
        is=1
      enddo
c
      is=0
      iw=1
c..... k and Dlog(k)/Dlog(R) in lower-left 3x3
      opacr=quad(is,iw,slr,h(1),h(2),h(3),alr(l1),alr(l2),alr(l3))
      dopacr=dkap
        if(ip .eq. 3) then
          opacrq=quad(is,iw,slr,q(1),q(2),q(3),alr(l1),alr(l2),alr(l3))
c.....    k and Dlog(k)/Dlog(R) in upper-left 3x3.
          dkapq3=dkap
        endif
        if(iq .eq. 3) then
c.....    k and Dlog(k)/Dlog(R) in lower-right 3x3.
          opact2=quad(is,iw,slr,h(2),h(3),h(4),alr(l2),alr(l3),alr(l4))
          dix2=(alr(l3)-slr)*dfsr(l3)
          dopacr=dopacr*dix2+dkap*(1.-dix2)
            if(ip .eq. 3) then
c.....        k and Dlog(k)/Dlog(T6) smoothed in both log(T6) and log(R)
              dopact=dopact*dix2+dopactq*(1.-dix2)
              opact=opact*dix2+opactq*(1-dix2)
            endif
         endif
        if(ip .eq. 3) then
c.....    k and Dlog(k)/Dlog(R) in upper-right 3x3.
          opacrq=quad(is,iw,slr,q(2),q(3),q(4),alr(l2),alr(l3),alr(l4))
            if(iq .eq. 3) then
c.....        Dlog(k)/Dlog(R) smoothed in both log(T6) and Log(R).
              dopacrq=dkapq3*dix2+dkap*(1.-dix2)
              dopacr=dopacr*dix+dopacrq*(1.-dix)
            endif
        endif
      dopactd=dopact-3.*dopacr
      if(opact.gt.1.e+15)stop 'Interpolation indices out of range'
          if (opact .gt. 9.) then
            dopact=99.
            dopacr=99.
            dopactd=99.
          endif
      return
      end

c
c *****************************************************************************
      subroutine readco
c..... The purpose of this subroutine is to read the data tables
      save
      parameter (ismdata=0)   ! modified
      parameter (mx=10,mz=13,nrm=19,nrb=1,nre=19,nr=nrm+1-nrb
     . ,ntm=70,ntb=1,nt=ntm+1-ntb)
      character*1 dumarra(250)
      common/aa/ q(4),h(4),xxh
      common/a/ mzz, xz(mx,mz,nt,nr),
     . t6list(nt),alr(nr),n(mx),alt(nt),opk(nt,nr),opk2(nt,nr),dfsx(mx)
     . ,dfs(nt),dfsr(nr),dfsz(mz),a(3,mx),b(3),m,mf,xa(mx)
     . ,alrf(nrm),xzf(nt,nr),t6listf(ntm),za(mz)
      common/b/ itab(mx,mz),nta(nr),x(mx,mz),y(mx,mz),
     . zz(mx,mz)
      common/e/ opact,dopact,dopacr,dopactd
      common/ee/ opl(mx,nt,nr),xx(mx),zza(mz)
      common/alink/ NTEMP,NSM,nrlow,nrhigh,RLE,t6arr(100),xzff(100,nr)
      COMMON/CST/NRL,RLS,nset,tmax  ! modified
c
        if (itimeco .ne. 12345678) then
        do i=1,mx
          do j=1,mz
            do k=1,nt
              do l=1,nr
                xz(i,j,k,l)=1.e+35
              enddo
            enddo
          enddo
        enddo
        itimeco=12345678
        endif
c
c..... read  tables
c      read header
      do i=1,240
      read (2,'(a)') dumarra(i)
      enddo
c
      do 3 m=1,mx
      do 2 i=1,n(m)
c
      read(2,'(f10.5)') dum
      read (2,'(7x,i3,26x,f6.4,3x,f6.4,3x,f6.4)')
     .itab(m,i),x(m,i),y(m,i),zz(m,i)
      read(2,'(f10.5)') dum,dum,dum
      read(2,'(4x,f6.1,18f7.1)') (alrf(kk),kk=1,nrm)
      read(2,'(f10.5)') dum
        do k=1,ntm
        read(2,'(f4.2,19f7.3)') alt(k),(xzf(k,l), l=1,nrm)
        alt(k)=alt(k)-6.
        if (isett6 .ne. 1234567) then
        t6listf(k)=10.**alt(k)
        t6arr(k)=t6listf(k)
        endif
          do ll=1,nrm   ! modified
          xzff(k,ll)=xzf(k,ll)
          enddo
        enddo
        isett6=1234567
c
       if (ismdata .eq. 0) then
        tmax=10.   ! modified
        nset=65
        RLS=-8.
        nsm=1
          RLE=1.
          nrlow=1
          nrhigh=2*(RLE-RLS)+1
c
        call opaltab    !modified
       endif
c
 1010  continue
c
      ll=1
      do 110 kk=1,nre
      alr(ll)=alrf(kk)
        do k=1,nt
        t6list(k)=t6listf(k+ntb-1)
        if(ismdata .eq. 0) then
c           Following skip required because, due to missing data,
c           the X=0  low T data cannot be smoothed
          if ((m  .eq. 1) .and. (k .le. 9)) then
            xz(m,i,k,ll)=xzf(k+ntb-1,kk)
          else
            xz(m,i,k,ll)=xzff(k+ntb-1,kk)
          endif
        else
         xz(m,i,k,ll)=xzf(k+ntb-1,kk)
        endif
        enddo
  110 ll=ll+1
c
    2 continue
    3 continue
c added by RS
      close(2)
c
      do 12 i=2,nt
   12 dfs(i)=1./(alt(i)-alt(i-1))
      do 13 i=2,nr
   13 dfsr(i)=1./(alr(i)-alr(i-1))
      do i=2,mx-1
      dfsx(i)=1./(xx(i)-xx(i-1))
      enddo
      do i=2,mz
      dfsz(i)=1./(zza(i)-zza(i-1))
      enddo
      return
      end
c
c *****************************************************************************
      function quad(ic,i,x,y1,y2,y3,x1,x2,x3)
c..... this function performs a quadratic interpolation.
      save
      common/d/dkap
      dimension  xx(3),yy(3),xx12(30),xx13(30),xx23(30),xx1sq(30)
     . ,xx1pxx2(30)
      xx(1)=x1
      xx(2)=x2
      xx(3)=x3
      yy(1)=y1
      yy(2)=y2
      yy(3)=y3
        if(ic .eq. 0) then
          xx12(i)=1./(xx(1)-xx(2))
          xx13(i)=1./(xx(1)-xx(3))
          xx23(i)=1./(xx(2)-xx(3))
          xx1sq(i)=xx(1)*xx(1)
          xx1pxx2(i)=xx(1)+xx(2)
        endif
      c3=(yy(1)-yy(2))*xx12(i)
      c3=c3-(yy(2)-yy(3))*xx23(i)
      c3=c3*xx13(i)
      c2=(yy(1)-yy(2))*xx12(i)-(xx1pxx2(i))*c3
      c1=yy(1)-xx(1)*c2-xx1sq(i)*c3
      dkap=c2+(x+x)*c3
      quad=c1+x*(c2+x*c3)
      return
      end
c *****************************************************************************
      block data
      parameter (mx=10,mz=13,nrm=19,nrb=1,nre=19,nr=nrm+1-nrb
     . ,ntm=70,ntb=1,nt=ntm+1-ntb)
      common/aa/ q(4),h(4),xxh
      common/a/ mzz, xz(mx,mz,nt,nr),
     . t6list(nt),alr(nr),n(mx),alt(nt),opk(nt,nr),opk2(nt,nr),dfsx(mx)
     . ,dfs(nt),dfsr(nr),dfsz(mz),a(3,mx),b(3),m,mf,xa(mx)
     . ,alrf(nrm),xzf(nt,nr),t6listf(ntm),za(mz)
      common/b/ itab(mx,mz),nta(nr),x(mx,mz),y(mx,mz),
     . zz(mx,mz)
      data (xa(i),i=1,mx-1)/0.0,0.1,0.2,0.35,0.5,.7,.8,.9,.95/
      data (za(i),i=1,mz)/.0,0.0001,.0003,.001,.002,.004,.01,.02,.03,
     x .04,.06,.08,.1/
      data (nta(i),i=1,nrm)/14*70,69,64,60,58,57/
      data (n(i),i=1,mx)/13,13,13,13,13,13,13,13,10,12/
      end
c
c *****************************************************************************
      subroutine opaltab
C
C  CODE FOR FITTING AND SMOOTHING OPAL DATA. ADAPTED FROM A CODE
C     WRITTEN BY MIKE SEATON(obtained june 1993)
C
C     OPAL DATA.
C     ASSUMES FIRST T6=0.006, LAST T6=10.OR 0.04). Depending on position
C     in the table.
C     USES RECTANGULAR ARRAY FOR VARIABLES T6 AND LOG10(R)
C
C     (1) NSM=NUMBER OF PASSES THROUGH SMOOTHING FILTER.
C     USE OF NSM=1 OR 2 IS RECOMMENDED.
C     NO SMOOTHING WITH NSM=0
C     (2) RANGE FOR LOG10(R),
C     RLS=FIRST VALUE, RLE=LAST VALE
C     (RLS MUST BE FIRST VALUYE IN TABLE)
C
C  SUBROUTINE INTERP
C     AFTER PROCESSING, DATA ARE IN A FORM FOR USE OF
C               SUBROUTINE INTERP
C     WHICH GIVES LOG(ROSS) AND TWO FIRST DERIVATIVES FOR ANY
C     VALUES OF LOG(T) AND LOG(RHO). SEE BELOW FOR FURTHER
C     EXPLANATION.
C
C  OUTPUT FOR THE CASE OF NSM.GT.0.
C     INTERP IS USED TO OBTAIN SMOOTHED DATA INTERPOLATED
C     BACK TO THE ORIGINAL OPAL MESH. TWO FILES ARE WRITTEN.
C
C
C  THE SUBROUTINES SPLINE AND SPLINT ARE ADAPTED FROM THOSE GIVE BY
C  W.H. Press, S.A. Teulolsky, W.T. Vettering and B.P. Flannery,
C  "Numerical Recipes in FORTRAN", 2nd edn., 1992, C.U.P.
C  OTHER REFERENCES ARE MADE TO METHODS DESCRIBED IN THAT BOOK.
C
      PARAMETER(IP=100,IPR=20)
      parameter (mx=10,mz=13,nrm=19,nrb=1,nre=19,nr=nrm+1-nrb
     . ,ntm=70,ntb=1,nt=ntm+1-ntb)
      DIMENSION U(IP),ROSSL(IP,IPR),V(IP),V2(IP)
      COMMON/CF/F(85,IPR),FX(85,IPR),FY(85,IPR),FXY(85,IPR)
      CHARACTER*1 HEAD(100)
      COMMON/CST/NRL,RLS,nset,tmax  ! modified
      common/alink/ N,NSM,nrlow,nrhigh,RLE,t6arr(100),xzff(100,nr)
      LOGICAL IERR

C
      NRL=2*(RLE-RLS)+1
C
C     STORE LOG10(T) IN U AND LOG10(ROSS) IN ROSSL
C     CHECK FIRST VALUE OF T6
      T6=t6arr(1)
      do j=1,NRL
      ROSSL(1,j)=xzff(1,j)
      enddo

      if (abs(T6-.0056341325) .lt. 1.e-8) then
         U(1)=6.+LOG10(T6)
      ENDIF
C     SET ROSSL UP TO T6=t6arr(nset)
      I=1
    5 I=I+1
      T6=t6arr(I)
      do j=1,NRL
      ROSSL(I,j)=xzff(I,j)
      enddo
         U(I)=6+LOG10(T6)
         IF(T6.LT.tmax)GOTO 5
      N=I
      IF(N.GT.IP)THEN
         PRINT*,' REQUIRE PARAMETER IP OF AT LEAST ',N
         STOP
      ENDIF
C
C
C     DEFINE VARIABLES
C         X=20.0*(LOG10(T)-3.80)+1
C         Y=2.0*(LOG10(R)-RLS)+1
C     USE INDICES I=1 TO nset AND J=1 TO NRL
C     X AND Y ARE SUCH THAT, ON MESH-POINT (I,J), X=I AND Y=J
C     OBTAIN:-
C         F(I,J)=LOG10(ROSS)
C         FX(I,J)=dF/dX
C         FY(I,J)=dF/dY
C         FXY(I,J)=ddF/dXdY
C
C
C     FIRST GET F AND FX, INTERPOLATING FROM OPAL T6 TO
C     INTERVAL OF 0.05 IN LOG10(T).
      DO 40 J=1,NRL
C        FOR EACH LOG10(R), STORE LOG10(ROSS) IN V(I)
         DO 20 I=1,N
            V(I)=ROSSL(I,J)
   20    CONTINUE
C
C        GET FIRST DERIVATIVES AT END POINTS
C
C        GET SECOND DERIVATIVES FOR SPLINE FIT
         CALL SPLINE(U,V,N,V2)
C
C        INTERPOLATE TO LOG10(T)=FLT, FLT=3.8(0.05)8.0
         DO 30 I=1,nset ! modified
            FLT=3.75+0.05*I
            CALL SPLINT(U,V,N,V2,FLT,F(I,J),FX(I,J))
   30    CONTINUE
C
   40 CONTINUE
C
C
C  OPTION FOR SMOOTHING
      IF(NSM.GT.0)THEN
         DO 35 NS=1,NSM
            CALL SMOOTH
   35    CONTINUE
         CALL FITX
      ENDIF
C
C
C  GET FY AND FXY
      CALL FITY
C
C  THE ARRAYS F, FX, FY AND FXY ARE NOW STORED
C
C  CAN NOW DO INTERPOLATIONS USING
C       CALL INTERP(FLT,FLRHO,G,DGDT,DGDRHO,IERR)
C       INPUT IS FLT=LOG10(T), FLRHO=LOG10(RHO)
C       OUTPUT IS G=LOG10(ROSS)
C              DGDT=dG/d(LOG10(T))
C            DGDRHO=dG/d(LOG10(RHO))
C              IERR=.TRUE. IF INPUT FLT, FLRHO ARE OUT-OF-RANGE,
C                          ELSE IERR=.FALSE.
C
C INTERPOLATE BACK TO OPAL POINTS
      IF(NSM.GT.0)THEN
         do l=1,NRL
         xzff(1,l)=ROSSL(1,l)
         enddo

         DO 70 K=2,N
            FLT=U(K)
            DO 50 L=nrlow,nrhigh
               FLR=RLS+.5*(L-1)
               FLRHO=FLR-18.+3.*FLT
               CALL INTERP(FLT,FLRHO,G,DGDT,DGDRHO,IERR)
               IF(IERR)THEN
               ENDIF
               V(L)=G
   50       CONTINUE
            T6=t6arr(K)
            do l=nrlow,nrhigh
            xzff(K,l)=V(l)

            enddo

   70    CONTINUE
      ENDIF
C
C
 1000 FORMAT('  SMOOTHED OPAL DATA')
 1100 FORMAT('  OPAL DATA, (SMOOTHED-ORIGINAL)')
 2000 FORMAT(100A1)
 2222 FORMAT(F8.3,20F7.3)
 6000 FORMAT(/' FIRST T6=',1P,E10.3,', SHOULD BE 0.006')
 6003 FORMAT(/' !!! OUT-OF-RANGE !!!'/' FLT=',1P,E10.3,', FLRHO=',E10.3,
     + ', FLR=',E10.3)
C
      END
C
c *****************************************************************************
      SUBROUTINE SPLINE(X,Y,N,Y2)
      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
C
C     FIRST DERIVATIVES AT END POINTS USING CUBIC FIT
         YP1=((Y(3)-Y(1))*(X(2)-X(1))**2
     +   -(Y(2)-Y(1))*(X(3)-X(1))**2)/
     +   ((X(3)-X(1))*(X(2)-X(1))*(X(2)-X(3)))
         YPN=((Y(N-2)-Y(N))*(X(N-1)-X(N))**2
     +   -(Y(N-1)-Y(N))*(X(N-2)-X(N))**2)/
     +   ((X(N-2)-X(N))*(X(N-1)-X(N))*(X(N-1)-X(N-2)))
C
      Y2(1)=-0.5
      U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      QN=0.5
      UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END
c *****************************************************************************
      SUBROUTINE SPLINT(XA,YA,N,Y2A,X,Y,YP)
      DIMENSION XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) PAUSE 'Bad XA input.'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      YP=0.05*  (  (-YA(KLO)+YA(KHI))/H
     +   +      ( -(3*A**2-1)*Y2A(KLO)
     +            +(3*B**2-1)*Y2A(KHI) )*H/6. )
      RETURN
      END
c *****************************************************************************
      SUBROUTINE FITY
C
C  THIS ROUTINE MAKES SPLINE FITS FOR F AND FX, AND OBTAINS
C  FY AND FXY
C
C
      COMMON/CST/NRL,RLS,nset,tmax  ! modified
C
      PARAMETER(IPR=20)
      DIMENSION A(IPR),B(IPR),AD(IPR),BD(IPR)
      COMMON/CF/F(85,IPR),FX(85,IPR),FY(85,IPR),FXY(85,IPR)
C
      DO 30 I=1,nset   ! modified
         DO 10 J=1,NRL
            A(J)=F(I,J)
            B(J)=FX(I,J)
   10    CONTINUE
C
         CALL GETD(A,NRL,AD,AP1,APN)
         CALL GETD(B,NRL,BD,BP1,BPN)
C
         FY(I,1)=AP1
         FY(I,NRL)=APN
         FXY(I,1)=BP1
         FXY(I,NRL)=BPN
         DO 20 J=2,NRL-1
            FY(I,J)= -A(J)+A(J+1)-2.*AD(J)-AD(J+1)
            FXY(I,J)=-B(J)+B(J+1)-2.*BD(J)-BD(J+1)
   20    CONTINUE
   30 CONTINUE
C
      RETURN
      END
c *****************************************************************************
      SUBROUTINE FITX
C
C  THIS ROUTINE IS USED ONLY AFTER SMOOTHING.
C  ITS FUNCTION IS TO RECOMPUTE FX USING SMOOTHED F.
C
C
      PARAMETER(IPR=20)
      DIMENSION A(85),D(85)
C
      COMMON/CST/NRL,RLS,nset,tmax  ! modified
      COMMON/CF/F(85,IPR),FX(85,IPR),FY(85,IPR),FXY(85,IPR)
C
      DO 30 J=1,NRL
         DO 10 I=1,nset ! modified
            A(I)=F(I,J)
   10    CONTINUE
         CALL GETD(A,nset,D,AP1,APN)  ! modified
         FX(1,J)=AP1
         FX(nset,J)=APN   ! modified
         DO 20 I=2,nset-1  ! modified
            FX(I,J)=-A(I)+A(I+1)-2.*D(I)-D(I+1)
   20    CONTINUE
   30 CONTINUE
C
      RETURN
      END
C
c *****************************************************************************
      SUBROUTINE GETD(F,N,D,FP1,FPN)
C
C  SIMPLIFIED CODE FOR SPLINE COEFFICIENTS, FOR CASE OF INTERVALS
C  OF UNITY.
C
C
      DIMENSION F(N),D(N),T(85)
C
      FP1=(-11.*F(1)+18.*F(2)-9.*F(3)+2.*F(4))/6.
      FPN=(11.*F(N)-18.*F(N-1)+9.*F(N-2)-2.*F(N-3))/6.
C
      D(1)=-.5
      T(1)=.5*(-F(1)+F(2)-FP1)
C
      DO 10 J=2,N-1
         D(J)=-1./(4.+D(J-1))
         T(J)=-D(J)*(F(J-1)-2.*F(J)+F(J+1)-T(J-1))
   10 CONTINUE
C
      D(N)=(FPN+F(N-1)-F(N)-T(N-1))/(2.+D(N-1))
C
      DO 20 J=N-1,1,-1
         D(J)=D(J)*D(J+1)+T(J)
   20 CONTINUE
C
      RETURN
      END
C
c *****************************************************************************
      SUBROUTINE INTERP(FLT,FLRHO,G,DGDT,DGDRHO,IERR)
C
C  GIVEN F,FX,FY AND FXY ON THE GRID POINTS, THIS ROUTINE
C  DOES BI-CUBIC INTERPOLATIONS USING METHODS DESCRIBED IN
C  Numerical Recipes, PP. 118 TO 120
C
C
      PARAMETER(IPR=20)
      COMMON/CF/ F(85,IPR),FX(85,IPR),FY(85,IPR),FXY(85,IPR)
      DIMENSION B(16)
      LOGICAL IERR
C
      COMMON/CST/NRL,RLS,nset,tmax  ! modified
C
C  EXTREME LIMITS ALLOWED ARE:-
C     (3.800-0.0125) TO (8.000+0.0125) FOR LOG10(T)
C     (RLS-0.125) TO (RLE+0.1254) FOR LOG10(R)
C     (ALLOWING FOR SMALL EXTRAPOLATIONS BEYOND TABULAR VALUES)
C
C  FUNCTION DEFINITIONS FOR CUBIC EXPANSION
C
      FF(S,T)=    B( 1)+T*(B( 2)+T*(B( 3)+T*B( 4)))
     +   +S*(     B( 5)+T*(B( 6)+T*(B( 7)+T*B( 8)))
     +   +S*(     B( 9)+T*(B(10)+T*(B(11)+T*B(12)))
     +   +S*(     B(13)+T*(B(14)+T*(B(15)+T*B(16))) )))
C
      FFX(S,T)=   B( 5)+T*(B( 6)+T*(B( 7)+T*B( 8)))
     +   +S*(  2*(B( 9)+T*(B(10)+T*(B(11)+T*B(12))))
     +   +S*(  3*(B(13)+T*(B(14)+T*(B(15)+T*B(16)))) ))
C
      FFY(S,T)=   B( 2)+S*(B( 6)+S*(B(10)+S*B(14)))
     +   +T*(  2*(B( 3)+S*(B( 7)+S*(B(11)+S*B(15))))
     +   +T*(  3*(B( 4)+S*(B( 8)+S*(B(12)+S*B(16)))) ))
C
      FFXY(S,T)=  B( 6)+T*(2*B( 7)+3*T*B( 8))
     +   +S*(   2*B(10)+T*(4*B(11)+6*T*B(12))
     +   +S*(   3*B(14)+T*(6*B(15)+9*T*B(16)) ))
C
C
      IERR=.FALSE.
C
      X=20.*(FLT-3.800)+1
      FLR=FLRHO+18.-3.*FLT
      Y=2*( FLR - RLS )+1
C
      IF(X.LT.2.)THEN
         IF(X.LT.0.75)THEN
            IERR=.TRUE.
         ELSE
            I=1
         ENDIF
      ELSEIF(X.GT.84)THEN
         IF(X.GT.85.25)THEN
            IERR=.TRUE.
         ELSE
            I=84
         ENDIF
      ELSE
         I=X
      ENDIF
      U=X-I
C
      IF(Y.LT.2.)THEN
         IF(Y.LT.0.75)THEN
            IERR=.TRUE.
         ELSE
            J=1
         ENDIF
      ELSEIF(Y.GT.NRL-1)THEN
         IF(Y.GT.NRL+.25)THEN
            IERR=.TRUE.
         ELSE
            J=NRL-1
         ENDIF
      ELSE
         J=Y
      ENDIF
      V=Y-J
C
      IF(IERR)THEN
         G=9.999
         DGDT=9.999
         DGDRHO=9.999
         RETURN
      ENDIF
C
C
C  GIVEN FUNCTIONS AND DERIVATIVES AT GRID POINTS, COMPUTE COEFFICIENTS.
      B(1)=F(I,J)
      B(2)=FY(I,J)
      B(3)=3*(-F(I,J)+F(I,J+1))-2*FY(I,J)-FY(I,J+1)
      B(4)=2*(F(I,J)-F(I,J+1))+FY(I,J)+FY(I,J+1)
C
      B(5)=FX(I,J)
      B(6)=FXY(I,J)
      B(7)=3*(-FX(I,J)+FX(I,J+1))-2*FXY(I,J)-FXY(I,J+1)
      B(8)=2*(FX(I,J)-FX(I,J+1))+FXY(I,J)+FXY(I,J+1)
C
      B(9)=3*(-F(I,J)+F(I+1,J))-2*FX(I,J)-FX(I+1,J)
      B(10)=3*(-FY(I,J)+FY(I+1,J))-2*FXY(I,J)-FXY(I+1,J)
      B(11)=9*(F(I,J)-F(I+1,J)+F(I+1,J+1)-F(I,J+1))
     + +6*(FX(I,J)-FX(I,J+1)+FY(I,J)-FY(I+1,J))
     + +4*FXY(I,J)
     + +3*(FX(I+1,J)-FX(I+1,J+1)-FY(I+1,J+1)+FY(I,J+1))
     + +2*(FXY(I,J+1)+FXY(I+1,J))
     + +FXY(I+1,J+1)
      B(12)=6*(-F(I,J)+F(I+1,J)-F(I+1,J+1)+F(I,J+1))
     + +4*(-FX(I,J)+FX(I,J+1))
     + +3*(-FY(I,J)+FY(I+1,J)+FY(I+1,J+1)-FY(I,J+1))
     + +2*(-FX(I+1,J)+FX(I+1,J+1)-FXY(I,J)-FXY(I,J+1))
     + -FXY(I+1,J)-FXY(I+1,J+1)
C
      B(13)=2*(F(I,J)-F(I+1,J))+FX(I,J)+FX(I+1,J)
      B(14)=2*(FY(I,J)-FY(I+1,J))+FXY(I,J)+FXY(I+1,J)
      B(15)=6*(-F(I,J)+F(I+1,J)-F(I+1,J+1)+F(I,J+1))
     + +4*(-FY(I,J)+FY(I+1,J))
     + +3*(-FX(I,J)-FX(I+1,J)+FX(I+1,J+1)+FX(I,J+1))
     + +2*(FY(I+1,J+1)-FY(I,J+1)-FXY(I,J)-FXY(I+1,J))
     + -FXY(I+1,J+1)-FXY(I,J+1)
      B(16)=4*(F(I,J)-F(I+1,J)+F(I+1,J+1)-F(I,J+1))
     + +2*(FX(I,J)+FX(I+1,J)-FX(I+1,J+1)-FX(I,J+1)
     +    +FY(I,J)-FY(I+1,J)-FY(I+1,J+1)+FY(I,J+1))
     + +FXY(I,J)+FXY(I+1,J)+FXY(I+1,J+1)+FXY(I,J+1)
C
C  GET G=LOG10(ROSS), DGDT=d LOG10(ROSS)/d LOG10(T),
C      DGDRHO=d LOG10(ROSS)/d LOG10(RHO)
      G=FF(U,V)
      DGDT=20.*FFX(U,V)-6.*FFY(U,V)
      DGDRHO=2.*FFY(U,V)
C
C
      RETURN
      END

c *****************************************************************************
      SUBROUTINE SMOOTH
C
C  THIS SUBROUTINE USES A 2-DIMENSIONAL GENERALISATION OF THE SMOOTHING
C  TECHNIQUES DESCRIBED ON PP. 644 TO 649 OF Numerical Recipes.
C
C  CONSIDER THE 25 POINTS DEFINED BY
C       I+n, n=-2,-1,0,1,2 AND J+m, m=-2,-1,0,1,2.
C  THE FUNCTION TO BE SMOOTHED IS FITTED TO A BI-CUBIC, INVOLVING
C  16 COEFFICIENTS, USING TECHNIQUES OF LEAST-SQUARES. THE SMOOTHED
C  FUNCTION (TEMPORARILY STORED IN FXY) IS GIVEN BY THE FITTED VALUE
C  AT THE POINT I AND J.
C
C  THE FITTING IS SHIFTED FOR POINTS CLOSE TO BOUNDARIES.
C
C
      PARAMETER(IPR=20)
C
      COMMON/CST/NRL,RLS,nset,tmax  ! modified
      COMMON/CF/F(85,IPR),FX(85,IPR),FY(85,IPR),FXY(85,IPR)
C
      DIMENSION GAM(6),BET(11),ALP(11)
      DATA GAM/+0.0073469388,-0.0293877551,-0.0416326531,
     +         +0.1175510204,+0.1665306122,+0.2359183673/
      DATA BET/
     + -0.0048979592,-0.0661224490,-0.0293877551,+0.0195918367,
     +  0.2644897959,+0.1175510204,-0.0783673469,+0.0277551020,
     +  0.3746938776,+0.1665306122,-0.1110204082/
      DATA ALP/
     + -0.0844897959,-0.0048979592,+0.0073469388,+0.0012244898,
     +  0.3379591837,+0.0195918367,-0.0293877551,+0.4787755102,
     +  0.0277551020,-0.0416326531,-0.0069387755/
C
C
      DO 20 I=3,nset-2
C
         J=1
         FXY(I,J)=
     +    ALP(1)*( F(I-2,J  )+F(I+2,J  ) )
     +   +ALP(2)*( F(I-2,J+1)+F(I+2,J+1)+F(I-2,J+3)+F(I+2,J+3)
     +          +F(I-1,J+4)+F(I+1,J+4) )
     +   +ALP(3)*( F(I-2,J+2)+F(I+2,J+2) )
     +   +ALP(4)*( F(I-2,J+4)+F(I+2,J+4) )
     +   +ALP(5)*( F(I-1,J  )+F(I+1,J  ) )
     +   +ALP(6)*( F(I-1,J+1)+F(I+1,J+1)+F(I-1,J+3)+F(I+1,J+3) )
     +   +ALP(7)*( F(I-1,J+2)+F(I+1,J+2) )
     +   +ALP(8)*  F(I  ,J  )
     +   +ALP(9)*( F(I  ,J+1)+F(I  ,J+3) )
     +   +ALP(10)* F(I  ,J+2) +ALP(11)*F(I  ,J+4)
C
         J=2
         FXY(I,J)=
     +    BET(1)*( F(I-2,J-1)+F(I+2,J-1)+F(I-2,J+3)+F(I+2,J+3) )
     +   +BET(2)*( F(I-2,J  )+F(I+2,J  ) )
     +   +BET(3)*( F(I-2,J+1)+F(I+2,J+1) )
     +   +BET(4)*( F(I-2,J+2)+F(I+2,J+2)+F(I-1,J-1)+F(I+1,J-1)
     +            +F(I-1,J+3)+F(I+1,J+3) )
     +   +BET(5)*( F(I-1,J  )+F(I+1,J  ) )
     +   +BET(6)*( F(I-1,J+1)+F(I+1,J+1) )
     +   +BET(7)*( F(I-1,J+2)+F(I+1,J+2) )
     +   +BET(8)*( F(I  ,J-1)+F(I  ,J+3) )
     +   +BET(9)*F(I  ,J  ) +BET(10)*F(I  ,J+1) +BET(11)*F(I  ,J+2)
C
         DO 10 J=3,NRL-2
            FXY(I,J)=
     +         GAM(1)*( F(I-2,J-2)+F(I-2,J+2)+F(I+2,J-2)+F(I+2,J+2) )
     +        +GAM(2)*( F(I-2,J+1)+F(I-2,J-1)+F(I-1,J-2)+F(I-1,J+2)
     +                 +F(I+1,J-2)+F(I+1,J+2)+F(I+2,J-1)+F(I+2,J+1) )
     +        +GAM(3)*( F(I-2,J  )+F(I+2,J  )+F(I  ,J-2)+F(I  ,J+2) )
     +        +GAM(4)*( F(I-1,J-1)+F(I-1,J+1)+F(I+1,J-1)+F(I+1,J+1) )
     +        +GAM(5)*( F(I-1,J  )+F(I  ,J-1)+F(I  ,J+1)+F(I+1,J  ) )
     +        +GAM(6)*  F(I  ,J  )
   10    CONTINUE
C
         J=NRL-1
         FXY(I,J)=
     +     BET(1)*( F(I-2,J+1)+F(I+2,J+1)+F(I-2,J-3)+F(I+2,J-3) )
     +    +BET(2)*( F(I-2,J  )+F(I+2,J  ) )
     +    +BET(3)*( F(I-2,J-1)+F(I+2,J-1) )
     +    +BET(4)*( F(I-2,J-2)+F(I+2,J-2)+F(I-1,J+1)+F(I+1,J+1)
     +             +F(I-1,J-3)+F(I+1,J-3) )
     +    +BET(5)*( F(I-1,J  )+F(I+1,J  ) )
     +    +BET(6)*( F(I-1,J-1)+F(I+1,J-1) )
     +    +BET(7)*( F(I-1,J-2)+F(I+1,J-2) )
     +    +BET(8)*( F(I  ,J+1)+F(I  ,J-3) )
     +    +BET(9)*F(I  ,J  ) +BET(10)*F(I  ,J-1) +BET(11)*F(I  ,J-2)
C
         J=NRL
         FXY(I,J)=
     +     ALP(1)*( F(I-2,J  )+F(I+2,J  ) )
     +    +ALP(2)*( F(I-2,J-1)+F(I+2,J-1)+F(I-2,J-3)+F(I+2,J-3)
     +             +F(I-1,J-4)+F(I+1,J-4) )
     +    +ALP(3)*( F(I-2,J-2)+F(I+2,J-2) )
     +    +ALP(4)*( F(I-2,J-4)+F(I+2,J-4) )
     +    +ALP(5)*( F(I-1,J  )+F(I+1,J  ) )
     +    +ALP(6)*( F(I-1,J-1)+F(I+1,J-1)+F(I-1,J-3)+F(I+1,J-3) )
     +    +ALP(7)*( F(I-1,J-2)+F(I+1,J-2) )
     +    +ALP(8)*  F(I  ,J  )
     +    +ALP(9)*( F(I  ,J-1)+F(I  ,J-3) )
     +    +ALP(10)* F(I  ,J-2) +ALP(11)*F(I  ,J-4)
C
   20 CONTINUE
C
      DO 40 I=3,nset-2   ! modified
         DO 30 J=1,NRL
            F(I,J)=FXY(I,J)
   30    CONTINUE
   40 CONTINUE
C
      RETURN
      END
C
c..............................................................................
c
c ***********      end of THE LIVERMORE LABORATORY OPAL PACKAGE     ***********
c
c *****************************************************************************
c -------------------------------------------- begin of subroutine OPACITY2
      subroutine opacity2(zs)
      implicit double precision (a-h,o-z)
      real*8 logt0, logt1, logt2, logt3, logr0, logr2
      real*4 zs, xenv
      parameter (igrid = 2)
      parameter (logt0 = 3.00d0,    dlogt0 = 0.05d0/igrid)
      parameter (logt1 = 6.00d0,    dlogt1 = 0.10d0/igrid)
      parameter (logt2 = 8.10d0,    dlogt2 = 0.20d0/igrid)
      parameter (logt3 = 8.70d0)
      parameter (nt1 =       (logt1-logt0)/dlogt0 + 0.5d0 + 1)
      parameter (nt2 = nt1 + (logt2-logt1)/dlogt1 + 0.5d0)
      parameter (ntt = nt2 + (logt3-logt2)/dlogt2 + 0.5d0)
      parameter (logr0 = -7.0d0, dlogr = 0.50d0/igrid, logr2 = 1.0d0)
      parameter (nr = (logr2 - logr0)/dlogr  + 0.5d0 + 1, nxtab = 7)
c
      dimension xtab(nxtab), ttab(ntt), rtab(nr),
     #          o(nxtab,nr,ntt), or(nxtab,nr,ntt), ot(nxtab,nr,ntt), 
     #          oe(nr,ntt), ore(nr,ntt), ote(nr,ntt)
      common /comenv/ xenv
      common /comkappa/ideriv
      save /comkappa/
      data xtab /0.0d0, 0.1d0, 0.2d0, 0.35d0, 0.5d0, 0.7d0, 0.8d0/
c --------------------------------------------------------------------------
      z      = dble(zs)
      zchem  = z
      xenvdp = dble(xenv)
c
      write(*,*)' '
      write(*,*)' Final opacity tables OPAC_7.DAT  &  OPAC_ENV.DAT '
      write(*,*)' igrid-times denser than tables X**,   igrid =',igrid
      write(*,*)' '
      ideriv=1
      write(*,119)
  119 format(//,'  Remember to put the following values',
     #          ' on a parameter list')
      write(*,*)' of the subroutine "opacity" (file "opac.04"):'
      write(*,*)'  ntt =',ntt
      write(*,*)'   nr =',nr
      write(*,*)' '
      write(*,*)' Thanks, now wait.........'
      write(*,*)' '
c ----------------------------------------------------- initialization
      do i = 1, ntt
         if (i.le.nt1)              t = logt0 + (i -   1) * dlogt0
         if (i.gt.nt1.and.i.le.nt2) t = logt1 + (i - nt1) * dlogt1
         if (i.gt.nt2.and.i.le.ntt) t = logt2 + (i - nt2) * dlogt2
         ttab(i) = t
         do j = 1, nr
            oe(j,i)   = 1.0d35
            ore(j,i)  = 1.0d35
            ote(j,i)  = 1.0d35
            if (i.eq.1) rtab(j) = logr0 + (j - 1) * dlogr
            do k = 1, nxtab
               o(k,j,i)    = 1.0d35
               or(k,j,i)   = 1.0d35
               ot(k,j,i)   = 1.0d35
            enddo
         enddo
      enddo
c
c ------------------------------------------------- opening opacity files
      ifi = 11
      ife = 21
      open (ifi,file='opac_7.dat',status='unknown')
      rewind (ifi)
      open (ife,file='opac_env.dat',status='unknown')
      rewind (ife)
c --------------------------------------- computing opacities and derivatives
      do k = 1, nxtab
         xchem = xtab(k)
         do i = 1, ntt
            t = ttab(i)
            te  = 10.0d0 ** t
            t6 = te / 1.0d6
            do j = 1, nr
               r  = 10.0d0 ** rtab(j)
               rh = r * t6 ** 3
c              ------------------------------------------
               call opacity(rh,te,xchem,zchem,op,opr,opt)
c              ------------------------------------------
               o(k,j,i)  = log10(op)
               or(k,j,i) = opr
               ot(k,j,i) = opt
               if (k.eq.1) then
c                 -------------------------------------------
                  call opacity(rh,te,xenvdp,zchem,op,opr,opt)
c                 -------------------------------------------
                  oe(j,i)  = log10(op)
                  ore(j,i) = opr
                  ote(j,i) = opt
               endif
            enddo
            write (*,107) k, i, t
  107       format(' k(X), i(T), log(T) :',2i4,0p1f10.5)
         enddo
      enddo
c ----------------------------------------- writing opacities and derivatives
      do iop = 1, 3
         do k = 1, nxtab
            xchem = xtab(k)
            write (ifi,101) zchem, xchem
  101       format(/1x,' Z =',1pd15.6,'          X =',1pd15.6)
            write (ifi,100)
            if (iop.eq.1) write (ifi,104)
  104                     format(' log10(Kappa) array')
            if (iop.eq.2) write (ifi,105)
  105                     format(' dlog(Kappa)/dlog(rho) array')
            if (iop.eq.3) write (ifi,106)
  106                     format(' dlog(Kappa)/dlog(T) array')
            write (ifi,102)
  102       format(40x,'log R')
            write (ifi,100)
            write (ifi,103) (rtab(j), j = 1, nr)
  103       format(1x,'   log T   ',0p33f7.3)
            write (ifi,100)
c -----------------------------------------------------------------
            if (k.eq.1) then
               write (ife,101) zchem, xenvdp
               write (ife,100)
               if (iop.eq.1) write (ife,104)
               if (iop.eq.2) write (ife,105)
               if (iop.eq.3) write (ife,106)
               write (ife,102)
               write (ife,100)
               write (ife,103) (rtab(j), j = 1, nr)
               write (ife,100)
            endif
c -----------------------------------------------------------------------
            do i = 1, ntt
               if (iop.eq.1) write (ifi,100) ttab(i),(o(k,j,i),j=1,nr)
  100          format(1x,0pf11.6,33f7.3)
               if (iop.eq.2) write (ifi,110) ttab(i),(or(k,j,i),j=1,nr)
               if (iop.eq.3) write (ifi,110) ttab(i),(ot(k,j,i),j=1,nr)
  110          format(1x,0pf11.6,0p33f12.6)
               if (k.eq.1.and.iop.eq.1) 
     #            write (ife,100) ttab(i),(oe(j,i),j=1,nr)
               if (k.eq.1.and.iop.eq.2)
     #            write (ife,110) ttab(i),(ore(j,i),j=1,nr)
               if (k.eq.1.and.iop.eq.3)
     #            write (ife,110) ttab(i),(ote(j,i),j=1,nr)
            enddo
         enddo
      enddo
      close (ifi)
      close (ife)
c ---------------------------
      return
      end
c -------------------------------------------- end of subroutine OPACITY2
c *****************************************************************************
c
cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
c  This is file "opacity.st4" compiled and developed by Alosha Pamyatnykh
c  with some changes by R. Sienkiewicz (RS)
c
c --------------------------------------------------------------------------
c    Ranges in log10(R) [=log10(rh/T6**3)] and log10(t) [t=temperature]
c
c    log10(R) = -7.0 (0.5) 1.0 (17 points).
c    log10(t) =
c    20 points from Alexander: log10(t) = 3.00(0.05)3.95
c  + 65 points from OPAL:      log10(t) = 4.00(0.05)6.00(0.10)8.10(0.20)8.70
c --------------------------------------------------------------------------
c
c    The following subroutines read and interpolate from opacity files "x**":
c
c   - "opacity"  - reads nxtab=7 tables for a given Z and different X:
c                  X=0.0, 0.1, 0.2, 0.35, 0.5, 0.7, 0.8.
c                  Interpolates opacities by cubic splines.
c                  In X, Newton 3-point interpolation is used.
c   - "coeff1"
c   - "coeff2"
c   - "terp1"
c   - "terp2" (function)
c   - "intrp"
c   - "search"
c   - "trip"
c *****************************************************************************
      subroutine opacity(rh,t,xchem,zchem,op,opr,opt)
      implicit double precision (a-h,o-z)
c
c subject:
c     calculate opacity from tables.
c     Two-dimensional cubic spline interpolation is used.
c     Opacity derivatives are calculated from the same cubic spline
c     and are fully consistent with the opacity itself.
c
c internal working variables are:
c                                  x  =  dlog10(rh/t6**3)
c                                  y  =  dlog10(t6)
c                                  f  =  dlog10(kappa)
c     where t6 = t*1.d-6
c
c input:
c     rh  = density                                 [g/cm**3]
c     t   = temperature                             [K]
c   xchem = hydrogen mass fraction
c   zchem = metal    mass fraction
c output:
c     op  = kappa (coefficient of opacity)          [cm**2/g]
c
c  !!! IMPORTANT !!! :  Derivatives are computed only if ideriv=1
c                       (to be used in the oscillation coefficients)
c     opr = d ln(kappa) /d ln(rh)
c     opt = d ln(kappa) /d ln(t)
c..............................................................................
c...................................................  changed by RS
      parameter (nxtab=7,nr=17,nte=85,inter=3,nwk=max(nr,nte)*3+1)
      parameter (dok=1.d-6,  zero=1.d-30)
c...................................................  changed by RS
c
c               nxtab  = max number of tables for different X (Hydrogen abund.)
c               nr     = dimesion in log R
c               nte    = (Alexander + OPAL) dimension in temperature
c           inter(=3)  = 3-point Newton interpolation in X is used
c               nwk    = max(nr,nte)*3 + 1   =  dimension of working array wk
      character*1 achar
      character*12 fintkp(nxtab),ach12
      dimension xtab(nxtab),opn(inter),optn(inter),oprhn(inter)
      dimension dn(3,inter),adn(3,inter)
      dimension xx(nr),yy(nte),yyn(nxtab,nte),
     #          ff(nr,nte),ffn(nxtab,nr,nte),
     #          fxx(nr,nte),fyy(nr,nte),fxxyy(nr,nte),ibd(4),wk(nwk),
     #          fxxn(nxtab,nr,nte),fyyn(nxtab,nr,nte),
     #          fxxyyn(nxtab,nr,nte)
      common /comop/ xx,yyn,ffn,fxxn,fyyn,fxxyyn
      common /comkappa/ideriv
        save /comop/,/comkappa/
      save icontrol
      data icontrol/0/
c...................................................  changed by RS
      data fintkp /'x00', 'x10', 'x20', 'x35', 'x50',
     #             'x70', 'x80'/
      data xtab /0.0d0, 0.1d0, 0.2d0, 0.35d0, 0.5d0,
     #           0.7d0, 0.8d0/
c...................................................  changed by RS
c ===========================  out of X - range alarm : BEGIN
      if (xchem.gt.xtab(nxtab)+1.0d-7) then
         write(*,204) xchem, zchem
  204    format(' X value is too large: X, Z = ', 1p2e10.3)
         stop
      endif
c ===========================  out of X - range alarm : END
c
      if (icontrol.ne.0) go to 1111
      icontrol=1
c
c ===============  INITIALISATION: begin  ==============================
c
c ====  reading the tables in
c
      do 3 n=1,nxtab
      nfile=n+50
      open (unit=nfile,file=fintkp(n),status='old')
      rewind (nfile)
c
      read(nfile,1101)zar,xar
      if(zchem.gt.zero)then
      if(abs(1.-zar/zchem).gt.dok)stop 'Z.ne.Ztab'
      else
      if(zar.gt.zero)stop 'Z.ne.Ztab'
      end if
      if(xtab(n).gt.zero)then
      if(abs(1.-xar/xtab(n)).gt.dok)stop 'X.ne.Xtab'
      else
      if(xar.gt.zero)stop 'X.ne.Xtab'
      end if
 1101 format(1x,4x,1pd15.6,13x,1pd15.6)
      do 4 i=1,4
    4 read (nfile,100) achar
  100    format(a1)
c
      read (nfile,101) ach12,(xx(i), i=1,nr)
  101 format(a12,17f7.3)
      read (nfile,100) achar
c
      do 20 j=1,nte
         read (nfile,102) ttt,(ffn(n,i,j), i=1,nr)
  102    format(f12.6,17f7.3)
c .... changed by RS
         yyn(n,j) = ttt - 6.
         yy(j)    = yyn(n,j)
         do 21 i=1,nr
   21       ff(i,j) = ffn(n,i,j)
   20 continue
      close (nfile)
c
c ====  calculating the interpolant
c
      ibd(1)=4
      ibd(2)=4
      ibd(3)=4
      ibd(4)=4
c ----------------------------------------------------------
      call coeff2(nr,xx,nte,yy,ff,fxx,fyy,fxxyy,nr,ibd,wk)
c ----------------------------------------------------------
      do 22 j=1,nte
      do 22 i=1,nr
         fxxn(n,i,j)   = fxx(i,j)
         fyyn(n,i,j)   = fyy(i,j)
 22      fxxyyn(n,i,j) = fxxyy(i,j)
c
    3 continue
c ================================================ END of INITIALISATION
 1111 continue
c --------------------------------------- (x,y) = (logR,logT6) point:
      x=dlog10(rh)-3.d0*dlog10(t)+18.d0
      y=dlog10(t)-6.d0
c
c ===========================  out of logR - range alarms : BEGIN
      if (x.lt.xx(1)-1.0d-7) then
         write(*,199) x,y
  199    format(' R is too small: lg R, lg T6 = ', 1p2e10.2)
         stop
      endif
c
      if (x.gt.xx(nr)+1.0d-7) then
         write(*,200) x,y
  200    format(' R is too large: lg R, lg T6 = ', 1p2e10.2)
         stop
      endif
c ===========================  out of logR - range alarms : END
c
c ===========================  out of logT6 - range alarms : BEGIN
      ialarm=0
      do 771 n=1,nxtab
         if (y.lt.yyn(n,1)-1.0d-7) ialarm=ialarm+1
  771 continue
c...................................................  changed by RS
      if (ialarm.eq.nxtab) stop 'T is too small'
      if (ialarm.ne.0)     stop 'different table size ?'
c                   Hereinafter, we are working with tables of the same size.
c                   ialarm should equal to 0 or nxtab and
c                   nskip  should equal to 0 by definition
      nskip=ialarm
c...................................................  changed by RS
c
      do 772 n=1,nxtab
         if (y.gt.yyn(n,nte)+1.0d-7) then
            write(*,202) x,y
  202       format(' T is too large: lg R, lg T6 = ', 1p2e10.2)
            stop
         endif
  772 continue
c
      if (xchem.lt.xtab(nskip+1)-1.0d-7) then
         write(*,205) xchem,x,y
  205    format(' T is too small for given X: X,lgR,lgT6 = ',1p3e10.2)
         stop
      endif
c ===========================  out of logT6 - range alarms : END
c
c ===========================  Interpolation alarm : BEGIN
      if (nskip.gt.nxtab-3) then
         write(*,203) x,y
  203    format(' Quadratic interpolation in X is impossible:',
     #         /' Check data: lg R, lg T6 = ', 1p2e10.2)
         stop
      endif
c ===========================  Interpolation alarm : END
c
c ====  OPACITY INTERPOLATION for each of three X (Hydrogen abundance) tables
c       (true value XCHEM lies between these tabulated values)
c
      n=nskip
c
 2221 n=n+1
      if (n.lt.(nxtab-1).and.xchem.gt.xtab(n+1)) go to 2221
      ninit=n
      if (n.eq.nxtab-1) ninit=n-1
c
      do 2222 n=ninit,ninit+2
      do 23 j=1,nte
         yy(j)      = yyn(n,j)
      do 23 i=1,nr
         ff(i,j)    = ffn(n,i,j)
         fxx(i,j)   = fxxn(n,i,j)
         fyy(i,j)   = fyyn(n,i,j)
 23      fxxyy(i,j) = fxxyyn(n,i,j)
c
c ----------------------------------------------------------
        f = terp2(x,y,nr,xx,nte,yy,ff,fxx,fyy,fxxyy,nr,0,0)
c ----------------------------------------------------------
c
c ====  Computation of derivatives - only if ideriv=1
      if (ideriv.eq.1) then
c ----------------------------------------------------------
         fx=terp2(x,y,nr,xx,nte,yy,ff,fxx,fyy,fxxyy,nr,1,0)
         fy=terp2(x,y,nr,xx,nte,yy,ff,fxx,fyy,fxxyy,nr,0,1)
c ----------------------------------------------------------
      else
         fx=0.0
         fy=0.0
      endif
c
c ====  transformation to t, rh, kappa variables
c       (also, possible element's displacement for the convenience)
c
      ni=n-ninit+1
      opn(ni)   = 10.d0**f
      optn(ni)  = opn(ni)/t*(fy-3*fx)
      oprhn(ni) = opn(ni)/rh*fx
c
 2222 continue
c --------------------------- END of interpolation for each X
c ========================== BEGIN of 3-point Newton interpolation in X
c                            taking into account true number of X-tables.
      do 31 i = 1,inter
         ii=i+ninit-1
         dn(1,i) = opn(i)
         dn(2,i) = optn(i)
         dn(3,i) = oprhn(i)
         if (i.gt.1) then
            do 32 k = i-1,1,-1
               kk=k+ninit-1
               do 32 j = 1,3
   32             dn(j,k) = (dn(j,k+1)-dn(j,k))/(xtab(ii)-xtab(kk))
         endif
         do 33 j = 1,3
   33       adn(j,i) = dn(j,1)
   31 continue
c
      op   = adn(1,inter)
      opt  = adn(2,inter)
      oprh = adn(3,inter)
      do 34 i = inter-1,1,-1
         ii=i+ninit-1
         op   = op*(xchem-xtab(ii))+adn(1,i)
         opt  = opt*(xchem-xtab(ii))+adn(2,i)
         oprh = oprh*(xchem-xtab(ii))+adn(3,i)
   34 continue
c ========================== END of 3-point Newton interpolation in X
c
c ====  conversion to logarithmic derivatives
c ====  with respect to t and rh
c
        opr = rh/op*oprh
        opt = t/op*opt
c
        return
        end
c                                                  end of "opacity"
c *****************************************************************************
      SUBROUTINE COEFF1 (N,X,F,W,IOP,INT,WK)
C
      implicit double precision (A-H,O-Z)
C
      DIMENSION       X(1)       ,F(1)       ,W(1)       ,IOP(2)     ,
     1                WK(N,1)
C
C ARITHMETIC STATENENT FUNCTION USED TO LOCATE ENTRIES IN F AND W ARRAYS
C
      II(INDEX)=(INDEX-1)*INT+1
C
C
C
C START TO SET UP TRIDIAGONAL SYSTEM
C
      J0 = 1
      DO 101 I=2,N
         JM = J0
         J0 = J0+INT
         WK(I,1) = X(I)-X(I-1)
         WK(I,2) = (F(J0)-F(JM))/WK(I,1)
         WK(I,3) = WK(I,1)/6.D0
         WK(I,1) = WK(I,1)/3.D0
  101 CONTINUE
      NN = N
      MK = IOP(1)
      ML = IOP(2)
C
C APPLY BOUNDARY CONDITIONS AT BOUNDARY 1
C
      GO TO (102,103,104,105),MK
C
C SECOND DERIVATIVE GIVEN AT BOUNDARY 1
C
  102 CONTINUE
      WK(2,2) = WK(3,2)-WK(2,2)-WK(2,3)*W(1)
      WK(2,3) = 0.D0
      WK(2,1) = WK(2,1)+WK(3,1)
      I1 = 2
      NN = NN-1
      GO TO 106
C
C FIRST DERIVATIVE GIVEN AT BOUNDARY 1
C
  103 CONTINUE
      WK(1,2) = WK(2,2)-W(1)
      WK(2,2) = WK(3,2)-WK(2,2)
      WK(1,3) = 0.D0
      WK(1,1) = WK(2,1)
      WK(2,1) = WK(2,1)+WK(3,1)
      I1 = 1
      GO TO 106
C
C PERIODIC BOUNDARY CONDITION
C
  104 CONTINUE
      Y2 = WK(2,2)
      B2 = WK(2,1)
      WK(2,2) = WK(3,2)-WK(2,2)
      WK(2,1) = WK(3,1)+WK(2,1)
      I1 = 2
      NN = NN-1
      GO TO 106
C
C FIRST DERIVATIVE AT BOUNDARY 1 FROM 4 POINT INTERPOLATION.
C
  105 CONTINUE
      A12 = X(1)-X(2)
      A13 = X(1)-X(3)
      A14 = X(1)-X(4)
      A23 = X(2)-X(3)
      A24 = X(2)-X(4)
      A34 = X(3)-X(4)
      J1 = 1
      J2 = J1+INT
      J3 = J2+INT
      J4 = J3+INT
      W(1)    = (1.D0/A12+1.D0/A13+1.D0/A14)*F(J1)-
     1          A13*A14/(A12*A23*A24)*F(J2)+A12*A14/(A13*A23*A34)*F(J3)-
     2          A12*A13/(A14*A24*A34)*F(J4)
      GO TO 103
C COMPUTE TRIDIAGONAL ARRAYS
  106 CONTINUE
      I2 = N-2
      DO 107 I=3,I2
         WK(I,2) = WK(I+1,2)-WK(I,2)
         WK(I,1) = WK(I+1,1)+WK(I,1)
  107 CONTINUE
C
C APPLY BOUNDARY CONDITIONS AT BOUNDARY 2.
C
      IN = II(N)
      GO TO (108,109,110,111),ML
C
C SECOND DERIVATIVE GIVEN AT BOUNDARY 2.
C
  108 CONTINUE
      WK(N-1,2) = WK(N,2)-WK(N-1,2)-WK(N,3)*W(IN)
      WK(N,3) = 0.D0
      WK(N-1,1) = WK(N-1,1)+WK(N,1)
      NN = NN-1
      GO TO 112
C
C FIRST DERIVATIVE GIVEN AT BOUNDARY 2.
C
  109 CONTINUE
      WK(N-1,2) = WK(N,2)-WK(N-1,2)
      WK(N,2) = -WK(N,2)+W(IN)
      WK(N-1,1) = WK(N-1,1)+WK(N,1)
      WK(1,4) = 0.D0
      GO TO 112
C
C PERIODIC BOUNDARY CONDITION
C
  110 CONTINUE
      WK(N-1,2) = WK(N,2)-WK(N-1,2)
      WK(N,2) = Y2-WK(N,2)
      WK(N-1,1) = WK(N-1,1)+WK(N,1)
      WK(N,1) = WK(N,1)+B2
      WK(1,4) = WK(2,3)
      GO TO 112
C
C FIRST DERIVATIVE AT BOUNDARY 2 FROM 4 POINT INTERPOLATION.
C
  111 CONTINUE
      A12 = X(N)-X(N-1)
      A13 = X(N)-X(N-2)
      A14 = X(N)-X(N-3)
      A23 = X(N-1)-X(N-2)
      A24 = X(N-1)-X(N-3)
      A34 = X(N-2)-X(N-3)
      J1 = IN
      J2 = J1-INT
      J3 = J2-INT
      J4 = J3-INT
      W(IN)   = (1.D0/A12+1.D0/A13+1.D0/A14)*F(J1)-
     1          A13*A14/(A12*A23*A24)*F(J2)+A12*A14/(A13*A23*A34)*F(J3)-
     2          A12*A13/(A14*A24*A34)*F(J4)
      GO TO 109
  112 CONTINUE
      IW1 = II(I1)
      CALL TRIP (NN,WK(I1,3),WK(I1,1),WK(I1+1,3),WK(I1,2),W(IW1),INT)
      GO TO (114,114,113,114),MK
  113 CONTINUE
      W(1) = W(IN)
  114 CONTINUE
      RETURN
      END
c *****************************************************************************
      SUBROUTINE COEFF2 (NX,X,NY,Y,F,FXX,FYY,FXXYY,IDM,IBD,WK)
C
      implicit double precision (A-H,O-Z)
C
      DIMENSION       X(1)       ,Y(1)       ,F(IDM,1)   ,FXX(IDM,1) ,
     1                FYY(IDM,1) ,FXXYY(IDM,1)           ,IBD(4)     ,
     2                ILOC(2)    ,JLOC(2)
      DATA ILOC(1),ILOC(2),JLOC(1),JLOC(2)/1,1,4,4/
C
C COMPUTE FXX
C
      DO 101 J=1,NY
         CALL COEFF1 (NX,X,F(1,J),FXX(1,J),IBD(1),1,WK)
  101 CONTINUE
C
C COMPUTE FYY
C
      DO 102 I=1,NX
         CALL COEFF1 (NY,Y,F(I,1),FYY(I,1),IBD(3),IDM,WK)
  102 CONTINUE
C
C CHECK FOR PERIODIC BOUNDARY CONDITION IN BOTH DIRECTIONS
C
      IF (IBD(1) .EQ. 3) GO TO 103
      IF (IBD(3) .EQ. 3) GO TO 105
C
C CALCULATE FXXYY ALONG LEFT AND RIGHT BOUNDARIES
C
      CALL COEFF1 (NY,Y,FXX(1,1),FXXYY(1,1),JLOC,IDM,WK)
      CALL COEFF1 (NY,Y,FXX(NX,1),FXXYY(NX,1),JLOC,IDM,WK)
      GO TO 106
  103 CONTINUE
C
C PERIODIC IN X DIRECTION . CALCULATE FXXYY ALONG LOWER AND UPPER
C BOUNDARIES.
C
      CALL COEFF1 (NX,X,FYY(1,1),FXXYY(1,1),IBD(1),1,WK)
      CALL COEFF1 (NX,X,FYY(1,NY),FXXYY(1,NY),IBD(1),1,WK)
C
C CALCULATE REMAINING FXXYY
C
      DO 104 I=1,NX
         CALL COEFF1 (NY,Y,FXX(I,1),FXXYY(I,1),ILOC,IDM,WK)
  104 CONTINUE
      GO TO 108
  105 CONTINUE
C
C PERIODIC IN Y DIRECTION. CALCULATE FXXYY ALONG LEFT AND RIGHT
C BOUNDARIES.
C
      CALL COEFF1 (NY,Y,FXX(1,1),FXXYY(1,1),IBD(3),IDM,WK)
      CALL COEFF1 (NY,Y,FXX(NX,1),FXXYY(NX,1),IBD(3),IDM,WK)
  106 CONTINUE
C
C CALCULATE REMAINING FXXYY
C
      DO 107 J=1,NY
         CALL COEFF1 (NX,X,FYY(1,J),FXXYY(1,J),ILOC,1,WK)
  107 CONTINUE
  108 CONTINUE
      RETURN
      END
c *****************************************************************************
      SUBROUTINE TERP1 (N,X,F,W,Y,INT,TAB,ITAB)
C
      implicit double precision (A-H,O-Z)
C
      DIMENSION       X(1)       ,F(1)       ,W(1)       ,TAB(3)     ,
     1                ITAB(3)
C
C PERFORM SEARCH
C
      CALL SEARCH (Y,X,N,I)
C
C CARRY OUT INTERPOLATION (OR EXTRAPOLATION)
C
      call intrp (N,X,F,W,Y,I,INT,TAB,ITAB)
      RETURN
      END
c *****************************************************************************
      FUNCTION TERP2 (XB,YB,NX,X,NY,Y,F,FXX,FYY,FXXYY,IDM,IXD,IYD)
C
      implicit double precision (A-H,O-Z)
C
      DIMENSION       X(1)       ,Y(1)       ,F(IDM,1)   ,FXX(IDM,1) ,
     1                FYY(IDM,1) ,FXXYY(IDM,1)           ,FF(2)      ,
     2                WW(2)      ,TAB(3)     ,ITAB(3)
C
C SEARCH IN X AND Y ARRAYS.
C
      CALL SEARCH (XB,X,NX,I)
      CALL SEARCH (YB,Y,NY,J)
C
C INTERPOLATE IN X DIRECTION
C
      DO 101 I1=1,3
         ITAB(I1) = 0
  101 CONTINUE
      I1 = IXD+1
      ITAB(I1) = 1
      DO 102 J1=1,2
         JJ = J+J1-1
         call intrp (N,X,F(1,JJ),FXX(1,JJ),XB,I,1,TAB,ITAB)
         FF(J1) = TAB(I1)
         call intrp (N,X,FYY(1,JJ),FXXYY(1,JJ),XB,I,1,TAB,ITAB)
         WW(J1) = TAB(I1)
  102 CONTINUE
C
C INTERPOLATE IN Y DIRECTION
C
      DO 103 J1=1,3
         ITAB(J1) = 0
  103 CONTINUE
      J1 = IYD+1
      ITAB(J1) = 1
      call intrp (2,Y(J),FF,WW,YB,1,1,TAB,ITAB)
      TERP2 = TAB(J1)
      RETURN
      END
c *****************************************************************************
      SUBROUTINE intrp (N,X,F,W,Y,I,INT,TAB,ITAB)
      implicit double precision (A-H,O-Z)
C
      DIMENSION       X(1)       ,F(1)       ,W(1)       ,TAB(3)
     -                ,ITAB(3)
C
C ARITHMETIC STATEMENT FUNCTION USED TO LOCATE ENTRIES IN F AND W ARRAYS
C
      II(INDEX)=(INDEX-1)*INT+1
C
C PERFORM INTERPOLATION OR EXTRAPOLATION
C
      FLK = X(I+1)-X(I)
      FLP = X(I+1)-Y
      FL0 = Y-X(I)
      I0 = II(I)
      IP = I0+INT
      IF (ITAB(1) .NE. 0) GO TO 101
      GO TO 102
  101 CONTINUE
C
C CALCULATE F(Y)
C
      A = (W(I0)*FLP**3+W(IP)*FL0**3)/(6.D0*FLK)
      B = (F(IP)/FLK-W(IP)*FLK/6.D0)*FL0
      C = (F(I0)/FLK-W(I0)*FLK/6.D0)*FLP
      TAB(1) = A+B+C
  102 CONTINUE
      IF (ITAB(2) .NE. 0) GO TO 103
      GO TO 104
  103 CONTINUE
C
C CALCULATE FIRST DERIVATIVE AT Y
C
      A = (W(IP)*FL0**2-W(I0)*FLP**2)/(2.D0*FLK)
      B = (F(IP)-F(I0))/FLK
      C = (W(I0)-W(IP))*FLK/6.D0
      TAB(2) = A+B+C
  104 CONTINUE
      IF (ITAB(3) .NE. 0) GO TO 105
      GO TO 106
  105 CONTINUE
C
C CALCULATE SECOND DERIVATIVE AT Y
C
      TAB(3) = (W(I0)*FLP+W(IP)*FL0)/FLK
  106 CONTINUE
      RETURN
      END
c *****************************************************************************
      SUBROUTINE SEARCH (XBAR,X,N,I)
C
      implicit double precision (A-H,O-Z)
C
      DIMENSION       X(1)
      DATA B/.69314718D0/
C
C IF XBAR IS OUTSIDE RANGE OF X TABLE EXTRAPOLATE
C
      IF (XBAR .GT. X(2)) GO TO 101
      I = 1
      RETURN
  101 CONTINUE
      IF (XBAR .LT. X(N-1)) GO TO 102
      I = N-1
      RETURN
  102 CONTINUE
C
C FIND MAXIMUM POWER OF TWO LESS THAN N
C
c      M = INT((DLOG(DFLOAT(N)))/B)
      M = INT((DLOG(  dble(N)))/B)
      I = 2**M
      IF (I .GE. N) I = I/2
      K = I
      NM1 = N-1
C
C CONDUCT BINARY SEARCH.
C
  103 CONTINUE
      K = K/2
      IF (XBAR .GE. X(I)) GO TO 104
      I = I-K
      GO TO 103
  104 CONTINUE
      IF (XBAR .LE. X(I+1)) RETURN
      I = MIN0(I+K,NM1)
      GO TO 103
      END
c *****************************************************************************
      SUBROUTINE TRIP (N,A,B,C,Y,Z,INT)
C
      implicit double precision (A-H,O-Z)
C
      DIMENSION       A(1)       ,B(1)       ,C(1)       ,Y(1)       ,
     1                Z(1)
C
C ARITHMETIC STATEMENT FUNCTION USED TO LOCATE ENTRIES IN ARRAY Z.
C
      II(INDEX)=(INDEX-1)*INT+1
C
C GAUSSIAN ELIMINATION
C
      BN = B(N)
      YN = Y(N)
      V = C(N)
      Y(1) = Y(1)/B(1)
      A(1) = A(1)/B(1)
      B(1) = C(1)/B(1)
      NM2 = N-2
      DO 101 J=2,NM2
         DEN = B(J)-A(J)*B(J-1)
         B(J) = C(J)/DEN
         Y(J) = (Y(J)-A(J)*Y(J-1))/DEN
         A(J) = -A(J)*A(J-1)/DEN
         BN = BN-V*A(J-1)
         YN = YN-V*Y(J-1)
         V = -V*B(J-1)
  101 CONTINUE
      DEN = B(N-1)-A(N-1)*B(N-2)
      B(N-1) = (C(N-1)-A(N-1)*A(N-2))/DEN
      Y(N-1) = (Y(N-1)-A(N-1)*Y(N-2))/DEN
      BN = BN-V*A(N-2)
      YN = YN-V*Y(N-2)
      V = A(N)-V*B(N-2)
C BACK SUBSTITUTION
      IIN = II(N)
      Z(IIN) = (YN-V*Y(N-1))/(BN-V*B(N-1))
      IIN2 = II(N-1)
      Z(IIN2) = Y(N-1)-B(N-1)*Z(IIN)
      NM1 = N-1
      IN = II(N)
      DO 102 J=2,NM1
         K = N-J
         IK = II(K)
      IKT = IK+INT
  102 Z(IK) = Y(K)-B(K)*Z(IKT)-A(K)*Z(IN)
      RETURN
      END
c *****************************************************************************
c        This file contains subroutines :
c
c     -   "opacity"           -
c     -   "coeff1"            -
c     -   "coeff2"            -
c     -   "terp1"             -
c     -   "terp2" (function)  -
c     -   "intrp"             -
c     -   "search"            -
c     -   "trip"              -
c                                              end of the file "opacity.st4"
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
