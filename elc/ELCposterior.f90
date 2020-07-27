INCLUDE 'pgmods.f90'
PROGRAM elcposterior
!
!   Will plot the posterior distributions from
!   hammerELC or demcmcELC.  Need to run ELCcorplotter
!   first.
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=dp) :: binx(500),biny(500),rmin,rmax
   REAL(KIND=dp) :: ymax,answer,siglow,sighigh,rmedian,clip
   REAL(KIND=dp) :: medsiglow,medsighigh,low90,high90
!
   REAL(KIND=dp), ALLOCATABLE :: parm(:),parmt(:),parmr(:),parmtr(:)
!
   REAL(KIND=pg)  :: xlow,xhigh,xplot(500),thresh
   REAL(KIND=pg)  :: yplot(500),xline(2),yline(2)
!
   INTEGER :: nlength,ioptimizer,nvar,ios,nvmax,istart,nfiles,nskip
   INTEGER :: i,iplot,length,nextra,istop,isepar,pgerr,pgbegin
   INTEGER :: indexparm(199),nparm,maxparm,nbin
   INTEGER :: nrat,indexrat(199),maxrat,isw24,j
!
   PARAMETER  (nvmax=100)
!
   CHARACTER(LEN=40) :: svar(nvmax),parmsuffix(999)
   CHARACTER(LEN=50) :: extrastring(58),extralabel(58)
   CHARACTER(LEN=80) :: parmlabel(999),parmlabelnbsh(999)
   CHARACTER(LEN=80) :: suffixstring,suffixstringnbsh,labelstring
   CHARACTER(LEN=80) :: outstring,labelstringnbsh
!
   LOGICAL :: isteff,israd,istemprat
!
   ALLOCATE(parm(9999999))
!
   nbin=200
   istart=500
   nskip=30
   istop=999999
   clip=0.0_dp
   CALL getpost(istart,nskip,istop,nbin,clip)
!
!   Figure out which optimizer was run.
!
   ios=0
   OPEN(UNIT=20,FILE='ELC.optimizer',STATUS='old',IOSTAT=ios,ERR=260)
   READ(20,*)ioptimizer
   CLOSE(20)
!
!   Get the length of each file, and the number of variables
!   that were fitted for.
!
   CALL getlength(nlength,nvar,svar,nvmax,ioptimizer)
!
!   Find the number of files.
!
   CALL getnumberfiles(ioptimizer,nfiles)
!
   IF(istop > nfiles)istop=nfiles
   IF(ioptimizer == 1)WRITE(*,60)
   IF(ioptimizer == 2)WRITE(*,70)
   IF(ioptimizer == 3)WRITE(*,80)
   IF(ioptimizer == 4)WRITE(*,90)
   IF(ioptimizer == 5)WRITE(*,100)
   IF(ioptimizer == 6)WRITE(*,110)
   IF(ioptimizer == 7)WRITE(*,120)
   IF(ioptimizer == 9)WRITE(*,130)
   IF(ioptimizer == 10)WRITE(*,140)
   CALL istringnoequals('Number of parameters = ',nvar, outstring,length)
   WRITE(*,160)TRIM(outstring)
   CALL istringnoequals('Length of each generation = ',nlength, outstring,length)
   WRITE(*,160)TRIM(outstring)
   CALL istringnoequals('Number of files = ',nfiles,outstring, length)
   WRITE(*,160)TRIM(outstring)
   CALL istringnoequals('Starting at file ',istart,outstring, length)
   WRITE(*,160)TRIM(outstring)
   CALL istringnoequals('Ending at file ',istop,outstring, length)
   WRITE(*,160)TRIM(outstring)
   CALL istringnoequals('Nskip = ',nskip,outstring,length)
   WRITE(*,160)TRIM(outstring)
   CALL istringnoequals('Number of bins = ',nbin, outstring,length)
   WRITE(*,160)TRIM(outstring)
   CALL pnoequalstring('Clip =',2,clip,outstring,length)
   WRITE(*,160)TRIM(outstring)
!
!   Figure out how many "extra" parameters are printed out at the
!   end of the generation* files
!
   nextra=0
   CALL checkextra(nextra,extrastring,extralabel)
   IF(nextra > 0)THEN
      CALL istringnoequals('Number of extra parameters = ',  &
         nextra,outstring,length)
      WRITE(*,160)TRIM(outstring)
   END IF
!
   CALL readcordata(ioptimizer,nlength,nvar,nfiles,svar,nvmax,  &
      istart,nskip,nextra,extrastring,istop)
!
!   Check to see of the user wants to include derived parameters
!   from ELCparm.* files
!
   nparm=0
   CALL getparm(nparm,indexparm,maxparm,isepar)
   IF(nparm > 0)THEN
      CALL getelcparmstring(parmsuffix,parmlabel)
      CALL istringnoequals('Number of derived parameters = ',  &
         nparm,outstring,length)
      WRITE(*,160)TRIM(outstring)
      CALL readcorparm(nparm,indexparm,nfiles,nlength,  &
         maxparm,ioptimizer,istart,nskip,istop,isepar)
   END IF

!   Check to see of the user wants to include flux ratios
!   from ELCratio.* files
!
   nrat=0
   CALL checkratio(isw24)
   IF(isw24 > 0)THEN
      CALL getratio(nrat,indexrat,maxrat)
      IF(nrat > 0)THEN
         CALL istringnoequals('Number of flux ratios = ',nrat,outstring,length)
         WRITE(*,160)TRIM(outstring)
         CALL readcorratio(nrat,indexrat,nfiles,nlength,  &
            maxrat,ioptimizer,istart,nskip,istop)
      END IF
   END IF
!
   OPEN(UNIT=44,FILE='ELCposterior.out',STATUS='unknown')
   WRITE(44,145)
!
!   now attempt to plot
!
   pgerr=pgbegin(0,'/vcps',1,2)
   DO i=1,nvar
      WRITE(*,"('Computing variable #',I0,1x,'(',A2,')',A,15x,A)",ADVANCE= &
         'no')i,svar(i)(1:2),' ',CHAR(13)
!
      CALL pgsch(1.15_pg)
      CALL pgslw(3)
!
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      CALL getsuffixnbsh(svar(i)(1:2),suffixstring,labelstringnbsh)
      OPEN(UNIT=33,FILE='ELCjunk.'//TRIM(suffixstring),STATUS='old')
!
      DO iplot=1,90000000
         READ(33,*,END=12)parm(iplot)
      END DO
!
12    iplot=iplot-1
      CLOSE(33)
!
      CALL dpassign90(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,low90,high90)
      CALL dpassignbin(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,medsiglow,medsighigh)
!
      OPEN(UNIT=34,FILE='hist.'//TRIM(suffixstring),STATUS='unknown')
!
      DO j=1,nbin
         WRITE(34,*)binx(j),biny(j)
         xplot(j)=REAL(binx(j),KIND=pg)
         yplot(j)=REAL(biny(j),KIND=pg)
      END DO
      CLOSE(34)
!
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      CALL getsuffixnbsh(svar(i)(1:2),suffixstring,labelstringnbsh)
      CALL posteriorrange(nbin,binx,biny,answer,siglow,sighigh)
      WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
         medsiglow,medsiglow,medsighigh,low90,high90,TRIM(labelstringnbsh)
!
      xlow=REAL(rmin-0.01_dp*(rmax-rmin),KIND=pg)
      xhigh=REAL(rmax+0.01_dp*(rmax-rmin),KIND=pg)
      thresh=REAL(ymax*1.05_dp,KIND=pg)
!
!   plot the histograms
!
      CALL pgenv(xlow,xhigh,0.0_pg,thresh,0,0)
      CALL pglabel(TRIM(labelstring),'N',' ')
      CALL pgbin(nbin,xplot,yplot,.true.)
      xline(1)=REAL(medsiglow,KIND=pg)
      xline(2)=REAL(medsiglow,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(medsighigh,KIND=pg)
      xline(2)=REAL(medsighigh,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(rmedian,KIND=pg)
      xline(2)=REAL(rmedian,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(4)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      xline(1)=REAL(low90,KIND=pg)
      xline(2)=REAL(low90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(high90,KIND=pg)
      xline(2)=REAL(high90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
   END DO
!
   IF(nextra == 0)GO TO 25
!
   DO i=1,nextra
      WRITE(*,"('Computing extra variable #',I0,1x,'(',A,')',A,15x,A)",  &
          ADVANCE='no')i,TRIM(extrastring(i)),' ',CHAR(13)
!
      CALL pgsch(1.15_pg)
      CALL pgslw(3)
!
      OPEN(UNIT=33,FILE='ELCjunk.'//TRIM(extrastring(i)),STATUS='old')
!
      DO iplot=1,90000000
         READ(33,*,END=10)parm(iplot)
      END DO
!
10    iplot=iplot-1
      CLOSE(33)
!
      CALL dpassign90(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,low90,high90)
      CALL dpassignbin(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,medsiglow,medsighigh)
!
      OPEN(UNIT=34,FILE='hist.'//TRIM(extrastring(i)),STATUS='unknown')
!
      DO j=1,nbin
         WRITE(34,*)binx(j),biny(j)
         xplot(j)=REAL(binx(j),KIND=pg)
         yplot(j)=REAL(biny(j),KIND=pg)
      END DO
      CLOSE(34)
!
      CALL posteriorrange(nbin,binx,biny,answer,siglow,sighigh)
      WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
         medsiglow,medsiglow,medsighigh,low90,high90,TRIM(extralabel(i))
!
      xlow=REAL(rmin-0.01_dp*(rmax-rmin),KIND=pg)
      xhigh=REAL(rmax+0.01_dp*(rmax-rmin),KIND=pg)
      thresh=REAL(ymax*1.05_dp,KIND=pg)
!
!   plot the histograms
!
      CALL pgenv(xlow,xhigh,0.0_pg,thresh,0,0)
      CALL pglabel(TRIM(extralabel(i)),'N',' ')
      CALL pgbin(nbin,xplot,yplot,.true.)
      xline(1)=REAL(medsiglow,KIND=pg)
      xline(2)=REAL(medsiglow,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(medsighigh,KIND=pg)
      xline(2)=REAL(medsighigh,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(rmedian,KIND=pg)
      xline(2)=REAL(rmedian,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(4)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      xline(1)=REAL(low90,KIND=pg)
      xline(2)=REAL(low90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(high90,KIND=pg)
      xline(2)=REAL(high90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
   END DO
!
25 IF(nparm == 0)GO TO 30
!
   WRITE(*,*)
   CALL getelcparmstring(parmsuffix,parmlabel)
   CALL getelcparmstringnbsh(parmsuffix,parmlabelnbsh)
   DO i=1,nparm
      WRITE(*,"('Computing derived parameter #',I0,1x,'(',A,')',A,15x,A)", &
         ADVANCE='no')indexparm(i),TRIM(parmsuffix(indexparm(i))),' ',CHAR(13)
!
      CALL pgsch(1.15_pg)
      CALL pgslw(3)
!
!   extract the i-th variable from parm into plottable
!   x and y arrays
!
      OPEN(UNIT=33,FILE='ELCjunk.'//TRIM(parmsuffix(indexparm(i))),STATUS= &
          'old')
!
      DO iplot=1,90000000
         READ(33,*,END=20)parm(iplot)
      END DO
!
20    iplot=iplot-1
      CLOSE(33)
!
      CALL dpassign90(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,low90,high90)
      CALL dpassignbin(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,medsiglow,medsighigh)
!
      OPEN(UNIT=34,FILE='hist.'//TRIM(parmsuffix(indexparm(i))),STATUS= &
         'unknown')
!
      DO j=1,nbin
         WRITE(34,*)binx(j),biny(j)
         xplot(j)=REAL(binx(j),KIND=pg)
         yplot(j)=REAL(biny(j),KIND=pg)
      END DO
      CLOSE(34)
!
      CALL posteriorrange(nbin,binx,biny,answer,siglow,sighigh)
      WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
         medsiglow,medsiglow,medsighigh,low90,high90,TRIM(parmlabelnbsh(indexparm(i)))
!
      xlow=REAL(rmin-0.01_dp*(rmax-rmin),KIND=pg)
      xhigh=REAL(rmax+0.01_dp*(rmax-rmin),KIND=pg)
      thresh=REAL(ymax*1.05_dp,KIND=pg)
!
!   plot the histograms
!
      CALL pgenv(xlow,xhigh,0.0_pg,thresh,0,0)
      CALL pglabel(TRIM(parmlabel(indexparm(i))),'N',' ')
      CALL pgbin(nbin,xplot,yplot,.true.)
      xline(1)=REAL(medsiglow,KIND=pg)
      xline(2)=REAL(medsiglow,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(medsighigh,KIND=pg)
      xline(2)=REAL(medsighigh,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(rmedian,KIND=pg)
      xline(2)=REAL(rmedian,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(4)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      xline(1)=REAL(low90,KIND=pg)
      xline(2)=REAL(low90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(high90,KIND=pg)
      xline(2)=REAL(high90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
   END DO
!
!   combine all of the compressed pages into one file
!
30 IF(isepar /= 3)GO TO 33
!
   WRITE(*,*)
   DO i=1,2
      IF(i == 1)THEN
         WRITE(*,"('Computing derived parameter #',I0,1x,'(',A,')',A)",  &
            ADVANCE='no')nparm+1,'a1',CHAR(13)
      ELSE
         WRITE(*,"('Computing derived parameter #',I0,1x,'(',A,')',A)",  &
            ADVANCE='no')nparm+2,'a2',CHAR(13)
      END IF
!
      CALL pgsch(1.15_pg)
      CALL pgslw(3)
!
!   extract the i-th variable from parm into plottable
!   x and y arrays
!
      IF(i == 1)THEN
         OPEN(UNIT=33,FILE='ELCjunk.a1',STATUS='old')
      ELSE
         OPEN(UNIT=33,FILE='ELCjunk.a2',STATUS='old')
      END IF
!
      DO iplot=1,90000000
         READ(33,*,END=21)parm(iplot)
      END DO
!
21    iplot=iplot-1
      CLOSE(33)
!
      CALL dpassign90(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,low90,high90)
      CALL dpassignbin(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,medsiglow,medsighigh)
!
      IF(i == 1)THEN
         OPEN(UNIT=34,FILE='hist.a1',STATUS='unknown')
      ELSE
         OPEN(UNIT=34,FILE='hist.a2',STATUS='unknown')
      END IF
!
      DO j=1,nbin
         WRITE(34,*)binx(j),biny(j)
         xplot(j)=REAL(binx(j),KIND=pg)
         yplot(j)=REAL(biny(j),KIND=pg)
      END DO
      CLOSE(34)
!
      CALL posteriorrange(nbin,binx,biny,answer,siglow,sighigh)
      IF(i == 1)THEN
         WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
            medsiglow,medsiglow,medsighigh,low90,high90,'a1 (AU)'
      ELSE
         WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
            medsiglow,medsiglow,medsighigh,low90,high90,'a2 (AU)'
      END IF
!
      xlow=REAL(rmin-0.01_dp*(rmax-rmin),KIND=pg)
      xhigh=REAL(rmax+0.01_dp*(rmax-rmin),KIND=pg)
      thresh=REAL(ymax*1.05_dp,KIND=pg)
!
!   plot the histograms
!
      CALL pgenv(xlow,xhigh,0.0_pg,thresh,0,0)
      IF(i == 1)THEN
         CALL pglabel('a1 (AU)','N',' ')
      ELSE
         CALL pglabel('a2 (AU)','N',' ')
      END IF
      CALL pgbin(nbin,xplot,yplot,.true.)
      xline(1)=REAL(medsiglow,KIND=pg)
      xline(2)=REAL(medsiglow,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(medsighigh,KIND=pg)
      xline(2)=REAL(medsighigh,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(rmedian,KIND=pg)
      xline(2)=REAL(rmedian,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(4)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      xline(1)=REAL(low90,KIND=pg)
      xline(2)=REAL(low90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(high90,KIND=pg)
      xline(2)=REAL(high90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
   END DO
!
!  Check to see if we can compute luminosities
!
33 isteff=.false.
   israd=.false.
   INQUIRE(FILE='ELCjunk.Teff1',EXIST=isteff)
   INQUIRE(FILE='ELCjunk.R1solar',EXIST=israd)
   IF((isteff).AND.(israd))THEN
      WRITE(*,"('Computing luminosity 1',A,15x,A)",  &
            ADVANCE='no')' ',CHAR(13)
!
      CALL pgsch(1.15_pg)
      CALL pgslw(3)
!
!   extract the i-th variable from parm into plottable
!   x and y arrays
!
      ALLOCATE(parmt(9999999),parmr(9999999))

      OPEN(UNIT=33,FILE='ELCjunk.Teff1',STATUS='old')
      OPEN(UNIT=35,FILE='ELCjunk.R1solar',STATUS='old')
!
      DO iplot=1,90000000
         READ(33,*,END=22)parmt(iplot)
         READ(35,*,end=22)parmr(iplot)
         parm(iplot)=parmr(iplot)**2*(parmt(iplot)/5770.0_pg)**4
      END DO
!
22    iplot=iplot-1
      CLOSE(33)
      CLOSE(35)
!
      CALL dpassign90(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,low90,high90)
      CALL dpassignbin(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,medsiglow,medsighigh)
!
      OPEN(UNIT=34,FILE='hist.L1',STATUS='unknown')
!
      DO j=1,nbin
         WRITE(34,*)binx(j),biny(j)
         xplot(j)=REAL(binx(j),KIND=pg)
         yplot(j)=REAL(biny(j),KIND=pg)
      END DO
      CLOSE(34)
!
      CALL posteriorrange(nbin,binx,biny,answer,siglow,sighigh)
      WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
            medsiglow,medsiglow,medsighigh,low90,high90,'L1 (solar)'
!
      xlow=REAL(rmin-0.01_dp*(rmax-rmin),KIND=pg)
      xhigh=REAL(rmax+0.01_dp*(rmax-rmin),KIND=pg)
      thresh=REAL(ymax*1.05_dp,KIND=pg)
!
!   plot the histograms
!
      CALL pgenv(xlow,xhigh,0.0_pg,thresh,0,0)
      CALL pglabel('L\d1\u (L\d\(2281)\u)','N',' ')
      CALL pgbin(nbin,xplot,yplot,.true.)
      xline(1)=REAL(medsiglow,KIND=pg)
      xline(2)=REAL(medsiglow,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(medsighigh,KIND=pg)
      xline(2)=REAL(medsighigh,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(rmedian,KIND=pg)
      xline(2)=REAL(rmedian,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(4)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      xline(1)=REAL(low90,KIND=pg)
      xline(2)=REAL(low90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(high90,KIND=pg)
      xline(2)=REAL(high90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      DEALLOCATE(parmt,parmr)
!
   END IF
!
!   check to see if we can compute Luminosity of star 2,
!   with teff2 a fitting variable 
!
   isteff=.false.
   israd=.false.
   INQUIRE(FILE='ELCjunk.Teff2',EXIST=isteff)
   INQUIRE(FILE='ELCjunk.R2solar',EXIST=israd)
   IF((isteff).AND.(israd))THEN
      WRITE(*,"('Computing luminosity 2',A,15x,A)",  &
            ADVANCE='no')' ',CHAR(13)
!
      CALL pgsch(1.15_pg)
      CALL pgslw(3)
!
!   extract the i-th variable from parm into plottable
!   x and y arrays
!
      ALLOCATE(parmt(9999999),parmr(9999999))

      OPEN(UNIT=33,FILE='ELCjunk.Teff2',STATUS='old')
      OPEN(UNIT=35,FILE='ELCjunk.R2solar',STATUS='old')
!
      DO iplot=1,90000000
         READ(33,*,END=23)parmt(iplot)
         READ(35,*,end=23)parmr(iplot)
         parm(iplot)=parmr(iplot)**2*(parmt(iplot)/5770.0_pg)**4
      END DO
!
23    iplot=iplot-1
      CLOSE(33)
      CLOSE(35)
!
      CALL dpassign90(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,low90,high90)
      CALL dpassignbin(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,medsiglow,medsighigh)
!
      OPEN(UNIT=34,FILE='hist.L2',STATUS='unknown')
!
      DO j=1,nbin
         WRITE(34,*)binx(j),biny(j)
         xplot(j)=REAL(binx(j),KIND=pg)
         yplot(j)=REAL(biny(j),KIND=pg)
      END DO
      CLOSE(34)
!
      CALL posteriorrange(nbin,binx,biny,answer,siglow,sighigh)
      WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
            medsiglow,medsiglow,medsighigh,low90,high90,'L2 (solar)'
!
      xlow=REAL(rmin-0.01_dp*(rmax-rmin),KIND=pg)
      xhigh=REAL(rmax+0.01_dp*(rmax-rmin),KIND=pg)
      thresh=REAL(ymax*1.05_dp,KIND=pg)
!
!   plot the histograms
!
      CALL pgenv(xlow,xhigh,0.0_pg,thresh,0,0)
      CALL pglabel('L\d2\u (L\d\(2281)\u)','N',' ')
      CALL pgbin(nbin,xplot,yplot,.true.)
      xline(1)=REAL(medsiglow,KIND=pg)
      xline(2)=REAL(medsiglow,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(medsighigh,KIND=pg)
      xline(2)=REAL(medsighigh,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(rmedian,KIND=pg)
      xline(2)=REAL(rmedian,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(4)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      xline(1)=REAL(low90,KIND=pg)
      xline(2)=REAL(low90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(high90,KIND=pg)
      xline(2)=REAL(high90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      DEALLOCATE(parmt,parmr)
!
   END IF
!
!   check to see if we can compute Luminosity of star 2,
!   with teff1 and temprat fitting variables 
!
   istemprat=.false.
   isteff=.false.
   israd=.false.
   INQUIRE(FILE='ELCjunk.Teff1',EXIST=isteff)
   INQUIRE(FILE='ELCjunk.temprat',EXIST=istemprat)
   INQUIRE(FILE='ELCjunk.R2solar',EXIST=israd)
   IF((isteff).AND.(israd).AND.(istemprat))THEN
      WRITE(*,"('Computing luminosity 2',A,15x,A)",  &
            ADVANCE='no')' ',CHAR(13)
!
      CALL pgsch(1.15_pg)
      CALL pgslw(3)
!
!   extract the i-th variable from parm into plottable
!   x and y arrays
!
      ALLOCATE(parmt(9999999),parmr(9999999),parmtr(9999999))

      OPEN(UNIT=33,FILE='ELCjunk.Teff1',STATUS='old')
      OPEN(UNIT=35,FILE='ELCjunk.R2solar',STATUS='old')
      OPEN(UNIT=36,FILE='ELCjunk.temprat',STATUS='old')
!
      DO iplot=1,90000000
         READ(33,*,END=24)parmt(iplot)
         READ(35,*,end=24)parmr(iplot)
         READ(36,*,end=24)parmtr(iplot)
         parm(iplot)=parmr(iplot)**2*(parmt(iplot)*parmtr(iplot)/5770.0_pg)**4
      END DO
!
24    iplot=iplot-1
      CLOSE(33)
      CLOSE(35)
      CLOSE(36)
!
      CALL dpassign90(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,low90,high90)
      CALL dpassignbin(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,medsiglow,medsighigh)
!
      OPEN(UNIT=34,FILE='hist.L2',STATUS='unknown')
!
      DO j=1,nbin
         WRITE(34,*)binx(j),biny(j)
         xplot(j)=REAL(binx(j),KIND=pg)
         yplot(j)=REAL(biny(j),KIND=pg)
      END DO
      CLOSE(34)
!
      CALL posteriorrange(nbin,binx,biny,answer,siglow,sighigh)
      WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
            medsiglow,medsiglow,medsighigh,low90,high90,'L2 (solar)'
!
      xlow=REAL(rmin-0.01_dp*(rmax-rmin),KIND=pg)
      xhigh=REAL(rmax+0.01_dp*(rmax-rmin),KIND=pg)
      thresh=REAL(ymax*1.05_dp,KIND=pg)
!
!   plot the histograms
!
      CALL pgenv(xlow,xhigh,0.0_pg,thresh,0,0)
      CALL pglabel('L\d2\u (L\d\(2281)\u)','N',' ')
      CALL pgbin(nbin,xplot,yplot,.true.)
      xline(1)=REAL(medsiglow,KIND=pg)
      xline(2)=REAL(medsiglow,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(medsighigh,KIND=pg)
      xline(2)=REAL(medsighigh,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(rmedian,KIND=pg)
      xline(2)=REAL(rmedian,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(4)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      xline(1)=REAL(low90,KIND=pg)
      xline(2)=REAL(low90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(high90,KIND=pg)
      xline(2)=REAL(high90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      DEALLOCATE(parmt,parmr,parmtr)
!
   END IF
!
!   Any flux ratios?
!
   IF(nrat == 0)GO TO 50
   WRITE(*,*)
!
   CALL getelcratiostring(parmsuffix,parmlabel)
   DO i=1,nrat
      WRITE(*,"('Computing flux #',I0,1x,'(',A,')',A)",ADVANCE='no')  &
         indexrat(i),TRIM(parmsuffix(indexrat(i))),CHAR(13)
!
      CALL pgsch(1.15_pg)
      CALL pgslw(3)
!
!   extract the i-th variable from parm into plottable
!   x and y arrays
!
      OPEN(UNIT=33,FILE='ELCjunk.'//TRIM(parmsuffix(indexrat(i))),STATUS='old')
!
      DO iplot=1,90000000
         READ(33,*,END=40)parm(iplot)
      END DO
!
40    iplot=iplot-1
      CLOSE(33)
!
      CALL dpassign90(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,low90,high90)
      CALL dpassignbin(iplot,nbin,parm,binx,biny,rmin,rmax,ymax,  &
         rmedian,clip,medsiglow,medsighigh)
!
      OPEN(UNIT=34,FILE='hist.'//TRIM(parmsuffix(indexrat(i))),STATUS='unknown')
!
      DO j=1,nbin
         WRITE(34,*)binx(j),biny(j)
         xplot(j)=REAL(binx(j),KIND=pg)
         yplot(j)=REAL(biny(j),KIND=pg)
      END DO
      CLOSE(34)
!
      CALL posteriorrange(nbin,binx,biny,answer,siglow,sighigh)
      WRITE(44,150)answer,rmedian,medsighigh-rmedian,rmedian-  &
         medsiglow,medsiglow,medsighigh,low90,high90,TRIM(parmlabel(indexrat(i)))
!
      xlow=REAL(rmin-0.01_dp*(rmax-rmin),KIND=pg)
      xhigh=REAL(rmax+0.01_dp*(rmax-rmin),KIND=pg)
      thresh=REAL(ymax*1.05_dp,KIND=pg)
!
!   plot the histograms
!
      CALL pgenv(xlow,xhigh,0.0_pg,thresh,0,0)
      CALL pglabel(TRIM(parmlabel(indexrat(i))),'N',' ')
      CALL pgbin(nbin,xplot,yplot,.true.)
      xline(1)=REAL(medsiglow,KIND=pg)
      xline(2)=REAL(medsiglow,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(4)
      xline(1)=REAL(medsighigh,KIND=pg)
      xline(2)=REAL(medsighigh,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(2)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(rmedian,KIND=pg)
      xline(2)=REAL(rmedian,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(4)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
      xline(1)=REAL(low90,KIND=pg)
      xline(2)=REAL(low90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
      xline(1)=REAL(high90,KIND=pg)
      xline(2)=REAL(high90,KIND=pg)
      yline(1)=0.0_pg
      yline(2)=thresh*1.02_pg
      CALL pgsci(3)
      CALL pgsls(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL pgsls(1)
!
   END DO
!
50 CLOSE(44)
!
   CALL pgend
   WRITE(*,*)
!
   DEALLOCATE(parm)
!
60 FORMAT('amoebaELC was run.')
70 FORMAT('demcmcELC was run.')
80 FORMAT('geneticELC was run.')
90 FORMAT('amoebaELC was run.')
100 FORMAT('loopELC was run.')
110 FORMAT('markovELC was run.')
120 FORMAT('mqELC was run.')
130 FORMAT('randomELC was run.')
140 FORMAT('hammerELC was run.')
145 FORMAT(9X,'Mode',17X,'Median',14X,'+1 sigma',9X,'-1 sigma',12X,  &
      'Low_bound',12X,'Upper_bound',18X,'5%',17X,'95%',12X,'Parameter')
150 FORMAT((2F20.12,2X),1X,2(f16.8,2X),2(f20.12,2X),2X,2(f20.12,2X),a)
160 FORMAT(a)
260 IF(ios /= 0)THEN
      WRITE(*,*) 'ELC.optimizer not found'
      STOP
   END IF
!
END PROGRAM elcposterior
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
INCLUDE  'plotsubs.f90'
INCLUDE  'pgsubs.f90'
