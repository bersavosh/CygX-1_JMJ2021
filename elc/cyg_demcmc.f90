INCLUDE 'lcmods.f90'
PROGRAM demcmcelc
!
!   May 9, 2014
!
!   DE MCMC algorithm of Ter Braak (2006)
!
   USE accur
   USE ran_lec
   USE medblock
!
   IMPLICIT NONE
!
   INTEGER :: nvmax,montemax
!
   PARAMETER(nvmax=80)
   PARAMETER(montemax=10000)
!
!   real scalars
!
   REAL(KIND=dp) :: aa,alb1,alb2,angdiff1,angdiff2,angdiff3,angdiff4,angdiff5
   REAL(KIND=dp) :: angdiff6,angdiff7,angdiff8,angsum1,angsum2,angsum3,angsum4
   REAL(KIND=dp) :: angsum5,angsum6,angsum7,angsum8,argper,avejump,beam1,beam2
   REAL(KIND=dp) :: betarim,bigbeta,bigbeta2,bigbeta3,bigbeta4,bigi,bigi2,bigi3
   REAL(KIND=dp) :: bigi4,bin2m3,bin2m4,bin2massdiff,bin2masssum,bin2q,bin2r3
   REAL(KIND=dp) :: bin2r4,bin2raddiff,bin2radsum,bin2ratrad,binqtc,chi1,chiall
   REAL(KIND=dp) :: chilimb,chisqb,chisqh,chisqi,chisqj,chisqk,chisqr,chisqrv1
   REAL(KIND=dp) :: chisqrv2,chisqu,chisqv,col2,col3,col4,contam,contams0
   REAL(KIND=dp) :: contams1,contams2,contams3,ddiff,deltachi,density,dphase,ecc
   REAL(KIND=dp) :: ecctemp,ecosw,fill1,fill2,filldiff,fillsum,finc,fm,frac1
   REAL(KIND=dp) :: frac2,fracdiff,fracjump,fracsum,g10,g3,g6,g7,g8,g9,gamma
   REAL(KIND=dp) :: gamma1,george,ggamma1,ggamma2,ggamma3,ggamma4,gmax,gmin,gsig
   REAL(KIND=dp) :: hh,massdiff,masssum,ochi,ochilr,ocose,omega1,omega10,omega2
   REAL(KIND=dp) :: omega3,omega4,omega5,omega6,omega7,omega8,omega9,omegadot
   REAL(KIND=dp) :: osine,p1mtc,p1ptc,p1qtc,p2ecos,p2esin,p2incl,p2mtc,p2omega
   REAL(KIND=dp) :: p2period,p2ptc,p2q,p2qtc,p2ratrad,p2t0,p2tconj,p3ecos,p3esin
   REAL(KIND=dp) :: p3incl,p3mtc,p3omega,p3period,p3ptc,p3q,p3qtc,p3ratrad,p3t0
   REAL(KIND=dp) :: p3tconj,p4ecos,p4esin,p4incl,p4mtc,p4omega,p4period,p4ptc
   REAL(KIND=dp) :: p4q,p4qtc,p4ratrad,p4t0,p4tconj,p5ecos,p5esin,p5incl,p5mtc
   REAL(KIND=dp) :: p5omega,p5period,p5ptc,p5q,p5qtc,p5ratrad,p5t0,p5tconj
   REAL(KIND=dp) :: p6ecos,p6esin,p6incl,p6mtc,p6omega,p6period,p6ptc,p6q,p6qtc
   REAL(KIND=dp) :: p6ratrad,p6t0,p6tconj,p7ecos,p7esin,p7incl,p7mtc,p7omega
   REAL(KIND=dp) :: p7period,p7ptc,p7q,p7qtc,p7ratrad,p7t0,p7tconj,p8ecos,p8esin
   REAL(KIND=dp) :: p8incl,p8mtc,p8omega,p8period,p8ptc,p8q,p8qtc,p8ratrad,p8t0
   REAL(KIND=dp) :: p8tconj,pbmtc,pbptc,period,primk,primmass,primrad,prob
   REAL(KIND=dp) :: pshift,purgefactor,q,raddiff,radsum,ratrad,rdphase,rfracmod
   REAL(KIND=dp) :: rinner,rk10,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rlimbsum1,rlimbsum2
   REAL(KIND=dp) :: rlx,router,rsw23,rsw24,rsw26,rsw27,rsw28,rsw29,rsw30,rsw6
   REAL(KIND=dp) :: rsw7,rsw8,rsw9,sa3,saveasini,savesep,scalegam,secmass,secrad
   REAL(KIND=dp) :: separ,siggam,small,sqecos,sqesin,sqp2ecos,sqp2esin,sqp3ecos
   REAL(KIND=dp) :: sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin,sqp6ecos
   REAL(KIND=dp) :: sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin,sqtertecos
   REAL(KIND=dp) :: sqtertesin,ssw48,ssw84,ssw85,sw1,sw2,sw23,sw24,sw25,sw26
   REAL(KIND=dp) :: sw27,sw28,sw29,sw3,sw30,sw47,sw48,sw49,sw5,sw6,sw7,sw72,sw73
   REAL(KIND=dp) :: sw8,sw80,sw81,sw82,sw83,sw84,sw85,sw86,sw87,sw88,sw89,sw9,t0
   REAL(KIND=dp) :: t10,t1off,t2off,t3,t3off,t4off,t5off,t6,t6off,t7,t7off,t8
   REAL(KIND=dp) :: t8off,t9,target,tbinoff,tconj,tdisk,teff1,teff2,tempchi
   REAL(KIND=dp) :: temperature,temphigh,templow,temprat,tempstart,tertconj
   REAL(KIND=dp) :: tertecos,tertesin,tertincl,tertomega,tertperiod,tertq
   REAL(KIND=dp) :: tertratrad,tertt0,tessbin,tesscontam,tgrav1,tgrav2,thresh
   REAL(KIND=dp) :: tmax,tmin,ttemp,uran,urand,varx,vx1,vx2,vxadd,vxmult,worst
   REAL(KIND=dp) :: xi,zerob,zeroh,zeroi,zeroj,zerok,zeror,zerou,zerov,zzz      
!
!   real arrays
!
   REAL(KIND=dp), DIMENSION(2) ::  xsob
   REAL(KIND=dp), DIMENSION(5) ::  gamma1array,gamma2array,gamma3array,gamma4array
   REAL(KIND=dp), DIMENSION(5) ::  gamma5array
   REAL(KIND=dp), DIMENSION(8) ::  sdarkint6,sdarkint7,sdarkint8
   REAL(KIND=dp), DIMENSION(8) ::  sdarkint9,sdarkint10,ochidisk
   REAL(KIND=dp), DIMENSION(8) ::  sdarkint1,sdarkint2,sdarkint3
   REAL(KIND=dp), DIMENSION(8) ::  sdarkint4,sdarkint5,wave
   REAL(KIND=dp), DIMENSION(25) ::  eobv,obsparm,obv
   REAL(kind=dp), DIMENSION(1000) ::  chiarr,dynwinhigh,dynwinlow,oldchiarr
   REAL(KIND=dp), DIMENSION(9999) ::  gaphigh,gaplow,probarr,xsc,ysc,zzzarr
   REAL(KIND=dp), DIMENSION(nvmax) :: sss,stepsave,var,vstart,vstep
   REAL(KIND=dp), DIMENSION(montemax) :: arrsobx,arrsoby
   REAL(KIND=dp), DIMENSION(2,4) :: spotdparm,spot1parm,spot2parm
   REAL(KIND=dp), DIMENSION(8,2) :: dbolx,dboly
   REAL(KIND=dp), DIMENSION(8,3) :: compfracs
   REAL(KIND=dp), DIMENSION(8,9) :: powercoeff
   REAL(KIND=dp), DIMENSION(8,10) :: dwavex,dwavey
   REAL(KIND=dp), DIMENSION(nvmax,10000) :: oldparmarray,olducparmarray,parmarray,ucparmarray
!
!   integer scalars
!
   INTEGER(KIND=8) :: col5,col6,col7,col8,col9 
   INTEGER :: col1,i,i1,i16,i2,i7,iatm,ibest,ichilabel,icnb,icnh
   INTEGER :: icni,icnj,icnk,icnr,icnrv1,icnrv2,icnrv3,icnrv4,icnu,icnv,icnvrt
   INTEGER :: idark1,idark2,idraw,iecheck,ifastflag,ifixgamma,ifrac,ig
   INTEGER :: igen,igrid,iiatm,iicnb,iicnh,iicni,iicnj,iicnk,iicnr,iicnu,iicnv
   INTEGER :: iidark1,iidark2,iidint,iidraw,iiecheck,iiidint,iiii,iikeep,iilaw
   INTEGER :: iimag,iirvfilt,iism1,iisw1,iisw100,iisw12,iisw13,iisw2,iisw21
   INTEGER :: iisw22,iisw23,iisw24,iisw25,iisw26,iisw27,iisw28,iisw29,iisw3
   INTEGER :: iisw30,iisw31,iisw32,iisw33,iisw34,iisw4,iisw5,iisw6,iisw7,iisw8
   INTEGER :: iisw9,iisynch,iit1,iit2,iit3,iit4,iitconj,ikeep,ilaw,ilimbcheck
   INTEGER :: ilum,imag,inalph1,inalph2,inalph3,inbet1,inbet2,inbet3,inradius
   INTEGER :: inref,inseg,intheta,ioldseed,iopened,ios,ioutflag,ipurgeint
   INTEGER :: ipurgestart,irvfilt,isavnb,isavnh,isavni,isavnj,isavnk,isavnr
   INTEGER :: isavnu,isavnv,isavrv1,isavrv2,iskip,ism1,istart,istartcorr,istuck
   INTEGER :: isvel1,isvel2,isvel3,isvel4,isw1,isw100,isw12,isw13,isw2,isw21
   INTEGER :: isw22,isw23,isw24,isw25,isw26,isw27,isw28,isw29,isw3,isw30,isw31
   INTEGER :: isw32,isw33,isw34,isw4,isw5,isw6,isw7,isw8,isw80,isw81,isw82,isw83
   INTEGER :: isw84,isw85,isw86,isw87,isw88,isw89,isw9,isw9save,isynch,it1,it2
   INTEGER :: it3,it4,itconj,itemp,itime,iunit,iupdate,iversion,iworst,j,jj,jjj
   INTEGER :: jkl,jsw80,jsw81,jsw82,jsw83,jsw84,jsw85,jsw86,jsw87,jsw88,jsw89,kk
   INTEGER :: kkk,lll,lll1,lll2,lll3,maxlines,maxmu,mmm,nalph1,nalph2,nalph3
   INTEGER :: nback,nbet1,nbet2,nbet3,ndatab,ndatah,ndatai,ndataj,ndatak
   INTEGER :: ndatamax,ndatar,ndatau,ndatav,ndattot,ndynwin,ngen,njump,nlength
   INTEGER :: nlines,nmaxeclipse,nmaxphase,nnn,nobv,np,nphase,nradius,nref,nrv1
   INTEGER :: nrv2,nrv3,nrv4,nrvphase,nsc,nseg,nset,nskip,nterms,ntheta,numparm
   INTEGER :: nvar,tessfilt                                                     
!
!   integer arrays
!
   INTEGER, DIMENSION(5) ::  nrv1array,nrv2array,nrv3array,nrv4array,nrv5array
   INTEGER, DIMENSION(5) ::  icnrv1array,icnrv2array,icnrv3array,icnrv4array,icnrv5array
   INTEGER, DIMENSION(40) ::  icnarray,ncycle,nobscycle
   INTEGER, DIMENSION(9999) ::  ichange,idxarr1,idxarr2
   INTEGER, DIMENSION(nvmax) :: nstep
!
   LOGICAL :: exst
!
   CHARACTER (LEN=2) :: tempstring
   CHARACTER (LEN=7) :: extension
   CHARACTER (LEN=30) :: outstring,outstring1,outstring2,outstring3
   CHARACTER (LEN=40) :: sobv(25),rv1file,rv2file,command,jdatafile
   CHARACTER (LEN=40) :: udatafile,svar(nvmax),hdatafile,kdatafile
   CHARACTER (LEN=40) :: bdatafile,vdatafile,rdatafile,idatafile
   CHARACTER (LEN=40) :: rv3file,rv4file,rv5file
   CHARACTER (LEN=350) :: fracstring
   CHARACTER (LEN=1000) :: parmstring
   CHARACTER (LEN=1700) :: dynparm
   CHARACTER (LEN=3000) :: line
   CHARACTER (LEN=3000) :: planetparm
   CHARACTER (LEN=4000) :: bigstring(3000,5)
   CHARACTER (LEN=4000) :: newparmstring
!
!   Allocatable model arrays
!
   REAL(KIND=dp), ALLOCATABLE :: drv1(:),drv2(:),drv3(:),drv4(:),drv5(:)
   REAL(KIND=dp), ALLOCATABLE :: fracs1(:,:),fracs2(:,:),fracs3(:,:),fracs4(:,:)
   REAL(KIND=dp), ALLOCATABLE :: fracs5(:,:),fracs6(:,:),fracs7(:,:),fracs8(:,:)
   REAL(KIND=dp), ALLOCATABLE :: rv1(:),rv2(:),rv3(:),rv4(:),rv5(:),xmod(:)
   REAL(KIND=dp), ALLOCATABLE :: xrvmod(:),ymodb(:),ymodd(:),ymodh(:),ymodi(:)
   REAL(KIND=dp), ALLOCATABLE :: ymodj(:),ymodk(:),ymodr(:),ymods1(:),ymods2(:)
   REAL(KIND=dp), ALLOCATABLE :: ymods3(:),ymods4(:),ymods5(:),ymodu(:),ymodv(:)
!
!   Allocatable data arrays
!
   REAL(KIND=dp), ALLOCATABLE :: errb(:),errh(:),erri(:),errj(:),errk(:),errr(:)
   REAL(KIND=dp), ALLOCATABLE :: errrv1(:),errrv1array(:,:),errrv2(:)
   REAL(KIND=dp), ALLOCATABLE :: errrv2array(:,:),errrv3(:),errrv3array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: errrv4(:),errrv4array(:,:),errrv5array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: erru(:),errv(:),resb(:),resh(:),resi(:),resj(:)
   REAL(KIND=dp), ALLOCATABLE :: resk(:),resr(:),resrv1(:),resrv1array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: resrv2(:),resrv2array(:,:),resrv3(:)
   REAL(KIND=dp), ALLOCATABLE :: resrv3array(:,:),resrv4(:),resrv4array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: resrv5array(:,:),resu(:),resv(:),saverrb(:)
   REAL(KIND=dp), ALLOCATABLE :: saverrh(:),saverri(:),saverrj(:),saverrk(:)
   REAL(KIND=dp), ALLOCATABLE :: saverrr(:),saverrrv1(:),saverrrv1array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: saverrrv2(:),saverrrv2array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: saverrrv3array(:,:),saverrrv4array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: saverrrv5array(:,:),saverru(:),saverrv(:)
   REAL(KIND=dp), ALLOCATABLE :: savxdatab(:),savxdatah(:),savxdatai(:)
   REAL(KIND=dp), ALLOCATABLE :: savxdataj(:),savxdatak(:),savxdatar(:)
   REAL(KIND=dp), ALLOCATABLE :: savxdatau(:),savxdatav(:),savxrv1(:)
   REAL(KIND=dp), ALLOCATABLE :: savxrv1array(:,:),savxrv2(:),savxrv2array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: savxrv3array(:,:),savxrv4array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: savxrv5array(:,:),savydatab(:),savydatah(:)
   REAL(KIND=dp), ALLOCATABLE :: savydatai(:),savydataj(:),savydatak(:)
   REAL(KIND=dp), ALLOCATABLE :: savydatar(:),savydatau(:),savydatav(:)
   REAL(KIND=dp), ALLOCATABLE :: savyrv1(:),savyrv1array(:,:),savyrv2(:)
   REAL(KIND=dp), ALLOCATABLE :: savyrv2array(:,:),savyrv3array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: savyrv4array(:,:),savyrv5array(:,:),xdatab(:)
   REAL(KIND=dp), ALLOCATABLE :: xdatah(:),xdatai(:),xdataj(:),xdatak(:)
   REAL(KIND=dp), ALLOCATABLE :: xdatar(:),xdatau(:),xdatav(:),xrv1(:)
   REAL(KIND=dp), ALLOCATABLE :: xrv1array(:,:),xrv2(:),xrv2array(:,:),xrv3(:)
   REAL(KIND=dp), ALLOCATABLE :: xrv3array(:,:),xrv4(:),xrv4array(:,:)
   REAL(KIND=dp), ALLOCATABLE :: xrv5array(:,:),ydatab(:),ydatah(:),ydatai(:)
   REAL(KIND=dp), ALLOCATABLE :: ydataj(:),ydatak(:),ydatar(:),ydatau(:)
   REAL(KIND=dp), ALLOCATABLE :: ydatav(:),yrv1(:),yrv1array(:,:),yrv2(:)
   REAL(KIND=dp), ALLOCATABLE :: yrv2array(:,:),yrv3(:),yrv3array(:,:),yrv4(:)
   REAL(KIND=dp), ALLOCATABLE :: yrv4array(:,:),yrv5array(:,:)                  
!
!   variables associated with observed eclipses
!   when Nbody >= 3 and itime >= 2
!
   REAL(KIND=dp), ALLOCATABLE :: obsterr(:,:),obsttimes(:,:),resttimes(:,:)
   REAL(KIND=dp), ALLOCATABLE :: tdur1(:,:),tdur2(:,:),tseps(:,:),ttimes(:,:)   
!
!   Arrays for model atmosphere
!
   INTEGER, ALLOCATABLE        :: nmu(:)
   REAL(KIND=dp), ALLOCATABLE  :: atmt(:),atmg(:),atmmu(:,:),atmint1(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: atmint2(:,:),atmint3(:,:),atmint4(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: atmint5(:,:),atmint6(:,:),atmint7(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: atmint8(:,:)
!
!   arrays for covariance matrix computation
!
   REAL(KIND=dp), ALLOCATABLE  :: corr(:),uncorr(:),aveparm(:)
   REAL(KIND=dp), ALLOCATABLE  :: covchol(:,:),invcovchol(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: fracjumparr(:)
!
   INTEGER              :: ifracjump,irecover,ntemp
!
!   COMMON /ranblock/ idum
!   COMMON /medblock/ rmed
!
   irecover=1

   DO i=1,3000
      bigstring(i,1)=''
      bigstring(i,2)=''
      bigstring(i,3)=''
      bigstring(i,4)=''
      bigstring(i,5)=''
   END DO
   DO i=1,nvmax
      svar(i)=' '
   END DO
   DO i=1,1000
      parmstring(i:i)=' '
   END DO
   DO i=1,3000
      planetparm(i:i)=' '
   END DO
   DO i=1,1700
      dynparm(i:i)=' '
      line(i:i)=' '
   END DO
   DO i=1,4000
      newparmstring(i:i)=' '
   END DO
   nphase=0
   nrvphase=0
   zerou=0.0_dp
   zerob=0.0_dp
   zerov=0.0_dp
   zeror=0.0_dp
   zeroi=0.0_dp
   zeroj=0.0_dp
   zeroh=0.0_dp
   zerok=0.0_dp
   ioutflag=0
!
!   open up optional file demcmcELC.inp and read the values of
!
!    a
!    gsig
!    rfracmod
!    rfracparm
!    ipurgestart
!    ipurgeint
!    purgefactor
!    istuck
!    target
!    itemp
!    tempstart
!    istart
!    iskip
!    ioldseed
!    istartcorr
!    nback
!    nskip
!    irecover
!
   aa=0.34_dp
   gsig=0.0005_dp
   rfracmod=0.05_dp
   numparm=1
   ipurgestart=200
   ipurgeint=25
   purgefactor=500.0_dp
   istuck=100
   target=0.35_dp
   itemp=1
   tempstart=1.0_dp
   istart=0
   iskip=0
   ioldseed=-1
   istartcorr=999999
   nback=5
   nskip=100
   irecover=1
!
   OPEN(UNIT=44,FILE='demcmcELC.inp',STATUS='old',ERR=20)
!
   READ(44,*,END=10)aa
   READ(44,*,END=10)gsig
   READ(44,*,END=10)rfracmod
   READ(44,*,END=10)numparm
   READ(44,*,END=10)ipurgestart
   READ(44,*,END=10)ipurgeint
   READ(44,*,END=10)purgefactor
   READ(44,*,END=10)istuck
   READ(44,*,END=10)target
   READ(44,*,END=10)itemp
   READ(44,*,END=10)tempstart
   READ(44,*,END=10)istart
   READ(44,*,END=10)iskip
   READ(44,*,END=10)ioldseed
   READ(44,*,END=10)istartcorr
   READ(44,*,END=10)nback
   READ(44,*,END=10)nskip
   READ(44,*,END=10)irecover
10 CLOSE(44)
   GO TO 30
!
20 WRITE(*,*)'demcmcELC.inp not found.  Using default values'
   WRITE(*,*)'gamma=0.34'
   WRITE(*,*)'gsig=0.0005'
   WRITE(*,*)'rfracmod=0.05'
   WRITE(*,*)'numparm=1'
   WRITE(*,*)'ipurgestart=200'
   WRITE(*,*)'ipurgeint=25'
   WRITE(*,*)'purgefactor=500.0'
   WRITE(*,*)'istuck=100'
   WRITE(*,*)'target=0.35'
   WRITE(*,*)'itemp=1'
   WRITE(*,*)'tempstart=1.0'
   WRITE(*,*)'istart=0'
   WRITE(*,*)'iskip=0'
   WRITE(*,*)'ioldseed=-1'
   WRITE(*,*)'istartcorr=999999'
   WRITE(*,*)'Nback=5'
   WRITE(*,*)'Nskip=100'
   WRITE(*,*)'irecover=1'
!
30 IF(itemp < 1)THEN
      WRITE(*,*)'Error: itemp less than 1, setting itemp=1'
      itemp=1
   END IF
!
   IF(tempstart < 1.0_dp)THEN
      WRITE(*,*)'Error: tempstart less than 1, setting', ' tempstart=1'
      tempstart=1.0_dp
   END IF
!
   IF(istart < 0)THEN
      WRITE(*,*)'Error: istart less than 0, setting istart=0'
      istart=0
   END IF
!
   IF(iskip < 0)THEN
      WRITE(*,*)'Error: iskip less than 0, setting iskip=0'
      itemp=1
   END IF
!
   IF(ioldseed < -1)THEN
      WRITE(*,*)'Error: ioldseed less than -1, setting ', 'ioldseed=abs(ioldseed)'
      ioldseed=ABS(ioldseed)
   END IF
!
   IF(irecover < 1)THEN
      WRITE(*,*)'Error: irecover should be larger than 1, setting irecover=1'
      irecover=1
   END IF
!
   IF(irecover > 1)THEN
      IF((MOD(irecover,5).ne.0).OR.((irecover /= 1).AND.(irecover < 30)))THEN
         WRITE(*,*)'Error: irecover should be 1 or a multiple of 5 that is >= 30'
         STOP
      END IF
   END IF
!
   IF(istartcorr-nback*nskip-1 < 1)THEN
      WRITE(*,*)'Error: problem with covariance parameters, setting'
      WRITE(*,*)'istartcorr=999999'
      WRITE(*,*)'Nback=5'
      WRITE(*,*)'Nskip=100'
      istartcorr=999999
      nback=5
      nskip=100
   END IF
!
!     open ELC.optimizer and record that demcmcELC ran
!
   OPEN(UNIT=69,FILE='ELC.optimizer',STATUS='unknown')
   WRITE(69,190)
   CLOSE(69)
!
   CALL pstring('gamma',3,aa,outstring,lll)
   CALL pstring('gsig',8,gsig,outstring1,lll1)
   CALL pstring('rfracmod',4,rfracmod,outstring2,lll2)
   CALL istring('numparm',numparm,outstring3,lll3)
   WRITE(*,200)TRIM(outstring),TRIM(outstring1),  &
      TRIM(outstring2),TRIM(outstring3)
   CALL istring('ipurgestart',ipurgestart,outstring,lll)
   CALL istring('ipurgeint',ipurgeint,outstring1,lll1)
   CALL pstring('purgefactor',2,purgefactor,outstring2,lll2)
   CALL istring('istuck',istuck,outstring3,lll3)
   WRITE(*,200)TRIM(outstring),TRIM(outstring1),  &
      TRIM(outstring2),TRIM(outstring3)
   CALL pstring('target_accept_frac',4,target, outstring,lll)
   CALL istring('itemp',itemp,outstring1,lll1)
   CALL pstring('tempstart',4,tempstart, outstring2,lll2)
   WRITE(*,210)TRIM(outstring),TRIM(outstring1),TRIM(outstring2)
   CALL istring('istart',istart,outstring1,lll1)
   CALL istring('iskip',iskip,outstring2,lll2)
   CALL istring('ioldseed',ioldseed,outstring,lll)
   WRITE(*,210)TRIM(outstring1),TRIM(outstring2),TRIM(outstring)
   CALL istring('istartcorr',istartcorr,outstring1,lll1)
   CALL istring('Nback',nback,outstring2,lll2)
   CALL istring('Nskip',nskip,outstring,lll)
   CALL istring('irecover',irecover,outstring3,lll3)
   WRITE(*,200)TRIM(outstring1),TRIM(outstring2),TRIM(outstring),TRIM(outstring3)
!
!   Open the parameter file and read all of the parameters.
!   Pass the parameters to the light curve routines
!   via a common block.
!
   CALL newgetinput(0,nalph1,nalph2,nalph3,nbet1,nbet2,nbet3,  &
      nradius,nref,nseg,ntheta,irvfilt,iatm,icnb,icnh,icni,icnj,  &
      icnk,icnr,icnu,icnv,idark1,idark2,idraw,iecheck,iidint,ikeep,  &
      ilaw,ism1,isw1,isw12,isw13,isw2,isw21,isw22,isw23,isw24,  &
      isw25,isw26,isw27,isw28,isw29,isw3,isw30,isw31,isw32,isw33,  &
      isw34,isw4,isw5,isw6,isw7,isw8,isw80,isw81,isw82,isw83,isw84,  &
      isw85,isw86,isw87,isw88,isw89,isw9,isw100,isynch,it1,it2,it3,  &
      it4,itconj,p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,p2esin,p2incl,  &
      p2mtc,p2ptc,p2period,p2ratrad,p2tconj,p3omega,p3q,p3t0,  &
      p3ecos,p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,p3tconj,  &
      p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,p4ptc,p4period,  &
      p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,p5esin,p5incl,p5mtc,  &
      p5ptc,p5period,p5ratrad,p5tconj,p6omega,p6q,p6t0,p6ecos,  &
      p6esin,p6incl,p6mtc,p6ptc,p6period,p6ratrad,p6tconj,p7omega,  &
      p7q,p7t0,p7ecos,p7esin,p7incl,p7mtc,p7ptc,p7period,p7ratrad,  &
      p7tconj,p8omega,p8q,p8t0,p8ecos,p8esin,p8incl,p8mtc,p8ptc,  &
      p8period,p8ratrad,p8tconj,pbmtc,pbptc,period,q,sa3,t0,tconj,  &
      teff1,teff2,tgrav1,tgrav2,alb1,alb2,argper,beam1,beam2,  &
      betarim,bigi,bigi2,bigi3,bigi4,bigbeta,bigbeta2,bigbeta3,  &
      bigbeta4,bin2q,bin2massdiff,bin2masssum,bin2raddiff,  &
      bin2radsum,bin2ratrad,contam,contams0,contams1,contams2,  &
      contams3,dbolx,dboly,density,dphase,dwavex,dwavey,ecc,ecosw,  &
      fill1,fill2,finc,fm,frac1,frac2,g10,g3,g6,g7,g8,g9,gamma,hh,  &
      massdiff,masssum,ocose,omega1,omega2,omega3,omega4,omega5,  &
      omega6,omega7,omega8,omega9,omega10,omegadot,osine,  &
      powercoeff,primk,primmass,primrad,pshift,rlx,raddiff,radsum,  &
      ratrad,rinner,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,router,  &
      sdarkint1,sdarkint2,sdarkint3,sdarkint4,sdarkint5,sdarkint6,  &
      sdarkint7,sdarkint8,sdarkint9,sdarkint10,secmass,secrad,  &
      separ,spot1parm,spot2parm,spotdparm,sw1,sw2,sw23,sw24,sw25,  &
      sw26,sw27,sw28,sw29,sw3,sw30,sw47,sw48,sw49,sw5,sw6,sw7,sw72,  &
      sw73,sw8,sw80,sw81,sw82,sw83,sw84,sw85,sw86,sw87,sw88,sw89,  &
      sw9,t10,t3,t6,t7,t8,t9,tdisk,temprat,tertomega,tertq,  &
      tertconj,tertecos,tertesin,tertincl,tertperiod,tertratrad,  &
      tertt0,wave,xi,iunit,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,  &
      bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin,  &
      sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin,  &
      sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin,  &
      angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3,angsum4,  &
      angdiff4,angsum5,angdiff5,angsum6,angdiff6,angsum7,angdiff7,  &
      angsum8,angdiff8,imag,fillsum,filldiff,binqtc,p1qtc,p2qtc,  &
      p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,  &
      t3off,t4off,t5off,t6off,t7off,t8off,iversion,tesscontam, &
      tessfilt,tessbin)
!
!   Define a new variable called sepsave, which is equal to the
!   value of separ given in ELC.inp
!
   savesep=separ
!
   IF(ilaw == 4)THEN
      DO jjj=1,8
         rlimbsum1=dwavex(jjj,1)+dwavey(jjj,1)
         IF(rlimbsum1 > 1.0_dp)THEN
            WRITE(*,*)'the sum of x and y exceeds 1.0 for index ', jjj
            STOP
         END IF
         rlimbsum2=dwavex(jjj,2)+dwavey(jjj,2)
         IF(rlimbsum2 > 1.0_dp)THEN
            WRITE(*,*)'the sum of x and y exceeds 1.0 for index ', jjj
            STOP
         END IF
      END DO
   END IF
!
!   Load the atmosphere table here, rather than in the subroutine
!
!  If the flag iatm>0, then load the model atmosphere table.
!
   gmin=0.0_dp
   gmax=0.0_dp
   tmin=0.0_dp
   tmax=0.0_dp
   IF(iatm >= 1)THEN
      CALL counttable(maxlines,maxmu)
      maxlines=maxlines+1
      maxmu=maxmu+1
      ALLOCATE(nmu(maxlines),atmt(maxlines),atmg(maxlines))
      ALLOCATE(atmint1(maxlines,maxmu),atmint2(maxlines,maxmu))
      ALLOCATE(atmint3(maxlines,maxmu),atmint4(maxlines,maxmu))
      ALLOCATE(atmint5(maxlines,maxmu),atmint6(maxlines,maxmu))
      ALLOCATE(atmint7(maxlines,maxmu),atmint8(maxlines,maxmu))
      ALLOCATE(atmmu(maxlines,maxmu))
      CALL loadtable(maxlines,maxmu,nlines,atmt,atmg,atmmu,nmu,  &
         atmint1,atmint2,atmint3,atmint4,atmint5,atmint6,atmint7,  &
         atmint8,tmax,tmin,gmax,gmin)
   ELSE
      maxlines=2
      maxmu=2
      ALLOCATE(nmu(maxlines),atmt(maxlines),atmg(maxlines))
      ALLOCATE(atmint1(maxlines,maxmu),atmint2(maxlines,maxmu))
      ALLOCATE(atmint3(maxlines,maxmu),atmint4(maxlines,maxmu))
      ALLOCATE(atmint5(maxlines,maxmu),atmint6(maxlines,maxmu))
      ALLOCATE(atmint7(maxlines,maxmu),atmint8(maxlines,maxmu))
      ALLOCATE(atmmu(maxlines,maxmu))
   END IF
!
!   Set the threshold to record models in files
!
   thresh=sw48
!
!   Determine the needed size for the data arrays.  isw4 is ifixgamma
!
   CALL countdata(isw4,nlength)
   ndatamax=nlength
!
!  Allocate the arrays
!
   ALLOCATE(xrv1array(ndatamax,5),xrv2array(ndatamax,5))
   ALLOCATE(xrv3array(ndatamax,5),xrv4array(ndatamax,5))
   ALLOCATE(xrv5array(ndatamax,5),yrv5array(ndatamax,5))
   ALLOCATE(yrv1array(ndatamax,5),yrv2array(ndatamax,5))
   ALLOCATE(yrv3array(ndatamax,5),yrv4array(ndatamax,5))
   ALLOCATE(errrv1array(ndatamax,5),errrv2array(ndatamax,5))
   ALLOCATE(errrv3array(ndatamax,5),errrv4array(ndatamax,5))
   ALLOCATE(errrv5array(ndatamax,5))
   ALLOCATE(savxrv1array(ndatamax,5),savxrv2array(ndatamax,5))
   ALLOCATE(savxrv3array(ndatamax,5),savxrv4array(ndatamax,5))
   ALLOCATE(savxrv5array(ndatamax,5))
   ALLOCATE(savyrv1array(ndatamax,5),savyrv2array(ndatamax,5))
   ALLOCATE(savyrv3array(ndatamax,5),savyrv4array(ndatamax,5))
   ALLOCATE(savyrv5array(ndatamax,5))
   ALLOCATE(saverrrv1array(ndatamax,5),saverrrv2array(ndatamax,5))
   ALLOCATE(saverrrv3array(ndatamax,5),saverrrv4array(ndatamax,5))
   ALLOCATE(saverrrv5array(ndatamax,5))
   ALLOCATE(xdatau(ndatamax),xdatab(ndatamax),xdatav(ndatamax),xdatar(ndatamax))
   ALLOCATE(xdatai(ndatamax),xdataj(ndatamax),xdatah(ndatamax),xdatak(ndatamax))
   ALLOCATE(ydatau(ndatamax),ydatab(ndatamax),ydatav(ndatamax),ydatar(ndatamax))
   ALLOCATE(ydatai(ndatamax),ydataj(ndatamax),ydatah(ndatamax),ydatak(ndatamax))
   ALLOCATE(erru(ndatamax),errb(ndatamax),errv(ndatamax),errr(ndatamax))
   ALLOCATE(erri(ndatamax),errj(ndatamax),errh(ndatamax),errk(ndatamax))
   ALLOCATE(errrv1(ndatamax),errrv2(ndatamax),errrv3(ndatamax),errrv4(ndatamax))
   ALLOCATE(xrv1(ndatamax),xrv2(ndatamax),xrv3(ndatamax),xrv4(ndatamax))
   ALLOCATE(yrv1(ndatamax),yrv2(ndatamax),yrv3(ndatamax),yrv4(ndatamax))
   ALLOCATE(resrv1(ndatamax),resrv2(ndatamax),resrv3(ndatamax),resrv4(ndatamax))
   ALLOCATE(resu(ndatamax),resb(ndatamax),resv(ndatamax))
   ALLOCATE(resr(ndatamax),resi(ndatamax),resj(ndatamax))
   ALLOCATE(resh(ndatamax),resk(ndatamax))
   ALLOCATE(savxdatau(ndatamax),savydatau(ndatamax))
   ALLOCATE(saverru(ndatamax),savydatab(ndatamax))
   ALLOCATE(saverrb(ndatamax),savydatav(ndatamax))
   ALLOCATE(saverrv(ndatamax),savydatar(ndatamax))
   ALLOCATE(saverrr(ndatamax),savydatai(ndatamax))
   ALLOCATE(saverri(ndatamax),savydataj(ndatamax))
   ALLOCATE(saverrj(ndatamax),savydatah(ndatamax))
   ALLOCATE(saverrh(ndatamax),savydatak(ndatamax))
   ALLOCATE(saverrk(ndatamax),savyrv1(ndatamax))
   ALLOCATE(saverrrv1(ndatamax),savyrv2(ndatamax))
   ALLOCATE(saverrrv2(ndatamax),savxdatab(ndatamax))
   ALLOCATE(savxdatav(ndatamax),savxdatar(ndatamax))
   ALLOCATE(savxdatai(ndatamax),savxdataj(ndatamax))
   ALLOCATE(savxdatah(ndatamax),savxdatak(ndatamax))
   ALLOCATE(savxrv1(ndatamax),savxrv2(ndatamax))
   ALLOCATE(resrv1array(ndatamax,5),resrv2array(ndatamax,5))
   ALLOCATE(resrv3array(ndatamax,5),resrv4array(ndatamax,5))
   ALLOCATE(resrv5array(ndatamax,5))
!
!   Allocate arrays for the eclipses if using the
!   dynamical integrator
!
!   isw88=Ndynwin    sw23=tstart   sw24=tend
!
!   isw7=itime    isw30=Nbody
!
   CALL counteclipse(isw88,sw23,sw24,hh,period,nlength,isw7,isw30)
   nmaxeclipse=nlength
!
   ALLOCATE(ttimes(40,nmaxeclipse),obsttimes(40,nmaxeclipse))
   ALLOCATE(tseps(40,nmaxeclipse),tdur1(40,nmaxeclipse))
   ALLOCATE(obsterr(40,nmaxeclipse),tdur2(40,nmaxeclipse))
   ALLOCATE(resttimes(40,nmaxeclipse))
!
   CALL initeclipse(nmaxeclipse,ncycle,ttimes,tseps,tdur1,  &
      tdur2,obsttimes,obsterr)
!
!          ichecklim=1 for hammer, markov, genetic, demcmc
!
   CALL setupdata(1,it2,isw31,gaplow,gaphigh,nsc,xsc,ysc,itime,  &
      isw7,rmed,sw6,idraw,ifixgamma,isw4,gamma1,gamma,teff2,rinner,  &
      fill2,isw9save,isw9,montemax,xsob,arrsobx,arrsoby,nvmax,  &
      nvar,nstep,var,svar,sss,isw28,finc,period,ecc,argper,t0,  &
      tconj,nobv,nrv3,udatafile,bdatafile,vdatafile,rdatafile,  &
      idatafile,jdatafile,hdatafile,kdatafile,rv1file,rv2file,  &
      vstart,vstep,sobv,obv,eobv,ifrac,ilum,iidint,ilaw,ilimbcheck,  &
      stepsave,icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk,icnrv1,  &
      icnrv2,ndatamax,ndatau,xdatau,ydatau,erru,ndatab,xdatab,  &
      ydatab,errb,ndatav,xdatav,ydatav,errv,ndatar,xdatar,ydatar,  &
      errr,ndatai,xdatai,ydatai,erri,ndataj,xdataj,ydataj,errj,  &
      ndatah,xdatah,ydatah,errh,ndatak,xdatak,ydatak,errk,nrv1,  &
      xrv1,yrv1,errrv1,nrv2,xrv2,yrv2,errrv2,isavnu,savxdatau,  &
      savydatau,saverru,isavnb,savxdatab,savydatab,saverrb,isavnv,  &
      savxdatav,savydatav,saverrv,isavnr,savxdatar,savydatar,  &
      saverrr,isavni,savxdatai,savydatai,saverri,isavnj,savxdataj,  &
      savydataj,saverrj,isavnh,savxdatah,savydatah,saverrh,isavnk,  &
      savxdatak,savydatak,saverrk,isavrv1,savxrv1,savyrv1,  &
      saverrrv1,isavrv2,savxrv2,savyrv2,saverrrv2,icnrv3,icnrv4,  &
      icnarray,isw30,isw23,nobscycle,obsttimes,obsterr,xrv3,yrv3,  &
      errrv3,nmaxeclipse,nrv4,xrv4,yrv4,errrv4,isw88,isw89,  &
      ndynwin,dynwinlow,dynwinhigh,sw23,sw24,hh,sw9,rv3file,  &
      rv4file,rv5file,nset,nrv1array,icnrv1array,xrv1array,  &
      yrv1array,errrv1array,nrv2array,icnrv2array,xrv2array,  &
      yrv2array,errrv2array,nrv3array,icnrv3array,xrv3array,  &
      yrv3array,errrv3array,nrv4array,icnrv4array,xrv4array,  &
      yrv4array,errrv4array,nrv5array,icnrv5array,xrv5array,  &
      yrv5array,errrv5array,savxrv1array,savyrv1array,  &
      saverrrv1array,savxrv2array,savyrv2array,saverrrv2array,  &
      savxrv3array,savyrv3array,saverrrv3array,savxrv4array,  &
      savyrv4array,saverrrv4array,savxrv5array,savyrv5array, &
      saverrrv5array)
!
!  Check for inconsistent tags
!
   CALL checkgridloop(nvmax,svar,temprat,isw29,ilaw,isw86,  &
      iidint,isw27,isw34,isw28,pbmtc,pbptc,binqtc,p1qtc,p1mtc,  &
      p1ptc,p2qtc,p2mtc,p2ptc,p3qtc,p3mtc,p3ptc,p4qtc,p4mtc,p4ptc,  &
      p5qtc,p5mtc,p5ptc,p6qtc,p6mtc,p6ptc,p7qtc,p7mtc,p7ptc,p8qtc,  &
      p8mtc,p8ptc,isw80,isw30,angsum1,angdiff1,angsum2,angdiff2,  &
      angsum3,angdiff3,angsum4,angdiff4,angsum5,angdiff5,angsum6,  &
      angdiff6,angsum7,angdiff7,angsum8,angdiff8,sqecos,sqesin,  &
      primmass,secmass,q,masssum,primk,primrad,secrad,radsum,  &
      ratrad,fracsum,frac1,frac2,fillsum,filldiff,sqtertesin,  &
      sqtertecos,sqp2ecos,sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,  &
      sqp4esin,sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,  &
      sqp7esin,sqp8ecos,sqp8esin,bin2masssum,bin2q,bin2radsum,  &
      bin2ratrad,bin2m3,bin2m4,bin2r3,bin2r4,isw26,iversion,  &
      itconj)
!
   ALLOCATE(corr(nvar),uncorr(nvar),aveparm(nvar),  &
      covchol(nvar,nvar),invcovchol(nvar,nvar))
!
   aveparm=0.0_dp
   corr=0.0_dp
   uncorr=0.0_dp
   ucparmarray=0.0_dp
   olducparmarray=0.0_dp
!
   isvel1=0
   isvel2=0
   IF(icnrv1 /= 430)isvel1=299
   IF(icnrv2 /= 430)isvel2=299
!
   IF(isw32 < 0)THEN
      idum=isw32
   ELSE
      idum=-1234567
   END IF
   idum=0
!
!  seeds for the new random number generator
!
   IF(isw32.ne.0)THEN
      seed5=abs(isw32)
   ELSE
      seed5=153587801
   END IF
   seed6=-759022222
   seed7=1288503317
   seed8=-1718083407
   seed9=-123456789
   CALL init_seeds6(seed5,seed6,seed7,seed8,seed9)
!
!   UPDATE November 6, 2008
!
!   if sw25 > 0, use that as an error for asini.
!
   saveasini=sw5
!
!   Start the looping here.
!
   chi1=0.0_dp
   small=9.0E+33_dp
   ndattot=0
   ndattot=ndatau+ndatab+ndatav+ndatar+ndatai+ndataj+ndatah+  &
      ndatak+nrv1+nrv2+nrv3
!
!  create scripts to get parameter values
!
   CALL getelcparmvals(isw30,2)
   isvel1=0
   IF(icnrv1 /= 430)isvel1=1
   isvel2=0
   IF(icnrv2 /= 430)isvel2=1
   isvel3=0
   IF(icnrv3 /= 430)isvel3=1
   isvel4=0
   IF(icnrv4 /= 430)isvel4=1
   ecctemp=ecc
   IF(isw29 > 0)THEN
      ecctemp=SQRT(ocose*ocose+osine*osine)
      IF((sqecos /= 0.0_dp).AND.(sqesin /= 0.0_dp))THEN
         ecctemp=sqecos**2+sqesin**2
      END IF
   END IF
   CALL getgenerationvals(2,nvar,nvmax,svar,isw29,isvel1,isvel2,  &
      isvel3,isvel4,sw5,ecctemp,ifixgamma,icnrv1array,  &
      icnrv2array,icnrv3array,icnrv4array,icnrv5array)
!
!   Open an output file for the fitting statistics.
!
   INQUIRE(FILE='demcmcELC.out',EXIST=exst)
   IF(exst)THEN
      IF((irecover >= 30).and.(mod(irecover,5) == 0))THEN
         command='cp demcmcELC.out ELCjunk.demcmcELC.out'
         CALL EXECUTE_COMMAND_LINE(command)
      END IF
      command='rm demcmcELC.out'
      CALL EXECUTE_COMMAND_LINE(command)
   END IF
   OPEN(UNIT=38,FILE='demcmcELC.out',STATUS='unknown')
   CLOSE(38)
!
   OPEN(UNIT=37,FILE='demcmcELC.jumps',STATUS='unknown')
!
   WRITE(extension,320)1000000+istart
!
   nterms=0
   DO  i7=1,nvar
      var(i7)=vstart(i7)
      kkk=icnvrt(svar(i7)(1:2))
      IF(kkk == 430)THEN
         EXIT
      ELSE
         nterms=nterms+1
      END IF
   END DO
!
   IF(isw9save > nstep(1))THEN
      WRITE(*,*)'ielete is too large'
      STOP
   END IF
!
   CALL istring('Nterms',nterms,outstring,lll)
   CALL istring('Nvar',nvar,outstring1,lll1)
   CALL istring('Ndattot',ndattot,outstring2,lll2)
   WRITE(*,210)outstring(1:lll),outstring1(1:lll1),outstring2(1:lll2)
   WRITE(*,*)
!
!   Define the random parameter sets.  Nstep(1) is the number of
!   random sets to define.  Nstep(2) is the number of generations
!
   np=nstep(1)
   IF(np <= 2)np=3
   ngen=nstep(2)
   ifracjump=ngen+10
   ALLOCATE(fracjumparr(ifracjump))
!
   IF(np > 10000)THEN
      WRITE(*,*)'Error:  Maximum population allowed is 10000'
      STOP
   END IF
   DO  j=1,nterms
      vx1=vstart(j)
      vx2=vstep(j)
      vxmult=(vx2-vx1)
      vxadd=vx1
      DO  i=1,np
         urand=ran6(idum)
         varx=urand*vxmult+vxadd
         parmarray(j,i)=varx
         ichange(i)=0
      END DO
   END DO
!
!   Evaluate the fitness of the initial population.
!
   DO  i16=1,np
!
!   UPDATE JULY 21, 2006
!
!   Add a fast mode where the light curve is first computed using
!   a very coarse grid and a large dphase.  Then compare the chi^2
!   to the current lowest.  If the chi^2 is very large, then use
!   the chi^2 from the coarse grid.  If the chi^2 is close to the
!   smallest value, repeat the light curve computation with the
!   normal grid.
!
!   We need to always compute the first model, so set ifastflag=0
!   when i16=1
!
      IF(isw22 >= 1)ifastflag=1
      IF(i16 == 1)ifastflag=0
      DO  j=1,nterms
         var(j)=parmarray(j,i16)
      END DO
!
!   UPDATE August 16, 2001
!
!   If the isw9 flag is 1 or greater, then alter the first
!   population member and put the parameters inside ELC.inp into
!   the var array
!
      IF((isw9save >= 1).AND.(i16 == 1))THEN
!
         CALL varassign(nvmax,svar,var,fill1,fill2,omega1,omega2,  &
            q,finc,teff1,teff2,betarim,rinner,router,tdisk,xi,rlx,  &
            separ,gamma,t3,g3,sa3,ecc,argper,pshift,spot1parm,  &
            spot2parm,spotdparm,period,t0,alb1,alb2,dwavex,dwavey,  &
            primmass,primk,primrad,ratrad,frac1,frac2,ecosw,temprat,  &
            bigi,bigbeta,density,tconj,beam1,beam2,contam,  &
            ocose,osine,isw29,tertperiod,tertt0,tertecos,tertesin,  &
            tertincl,tertomega,tertq,tgrav1,tgrav2,tertconj,omegadot,  &
            contams0,contams1,contams2,contams3,p2tconj,p2period,  &
            p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,p2ratrad,p3tconj,  &
            p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,  &
            p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,  &
            p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl,  &
            p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,p6esin,  &
            p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,p7ecos,  &
            p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,  &
            p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,sw72,sw73,sw49,  &
            sw80,sw81,sdarkint1,sdarkint2,sdarkint3,sdarkint4,  &
            sdarkint5,tertratrad,p1mtc,p1ptc,p2mtc,p2ptc,p3mtc,p3ptc,  &
            p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,p7mtc,p7ptc,p8mtc,  &
            p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,bigbeta2,bigbeta3,  &
            bigbeta4,bin2q,bin2massdiff,bin2masssum,bin2raddiff,  &
            bin2radsum,bin2ratrad,g10,g6,g7,g8,g9,massdiff,masssum,  &
            omega3,omega4,omega5,omega6,omega7,omega8,omega9,omega10,  &
            raddiff,radsum,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,  &
            sdarkint6,sdarkint7,sdarkint8,sdarkint9,sdarkint10,  &
            secmass,secrad,t10,t6,t7,t8,t9,fracsum,fracdiff,bin2m3,  &
            bin2m4,bin2r3,bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,  &
            sqp2ecos,sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,  &
            sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,  &
            sqp8ecos,sqp8esin,angsum1,angdiff1,angsum2,angdiff2,  &
            angsum3,angdiff3,angsum4,angdiff4,angsum5,angdiff5,  &
            angsum6,angdiff6,angsum7,angdiff7,angsum8,angdiff8,  &
            fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,  &
            p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,t3off,t4off,t5off,  &
            t6off,t7off,t8off,tesscontam)
!
         DO  j=1,nterms
            vx1=vstart(j)
            vx2=vstep(j)
            vxmult=(vx2-vx1)
            vxadd=vx1
            varx=var(j)
            urand=(varx-vxadd)/vxmult
            IF(urand > 1.0_dp)urand=1.0_dp
            IF(urand < 0.0_dp)urand=0.0_dp
            parmarray(j,1)=varx
            IF(varx < vstart(j))THEN
               WRITE(*,220)svar(j),i16
               WRITE(*,230)varx,vstart(j),vstep(j)
               STOP
            END IF
            IF(varx > vstep(j))THEN
               WRITE(*,220)svar(j),i16
               WRITE(*,230)varx,vstart(j),vstep(j)
               STOP
            END IF
         END DO
!
      END IF
!
!   UPDATE December 20, 2012
!
!   if ielete (isw9) is more than 1, add additional preset
!   parameters to the mix
!
      IF((isw9save > 1).AND.(i16 > 1).AND.(i16 <= isw9).AND. (istart < 1))THEN
         kkk=i16
         CALL newgetinput(kkk-1,inalph1,inalph2,inalph3,inbet1,  &
            inbet2,inbet3,inradius,inref,inseg,intheta,iirvfilt,  &
            iiatm,iicnb,iicnh,iicni,iicnj,iicnk,iicnr,iicnu,iicnv,  &
            iidark1,iidark2,iidraw,iiecheck,iiidint,iikeep,iilaw,  &
            iism1,iisw1,iisw12,iisw13,iisw2,iisw21,iisw22,iisw23,  &
            iisw24,iisw25,iisw26,iisw27,iisw28,iisw29,iisw3,iisw30,  &
            iisw31,iisw32,iisw33,iisw34,iisw4,iisw5,iisw6,iisw7,  &
            iisw8,jsw80,jsw81,jsw82,jsw83,jsw84,jsw85,jsw86,jsw87,  &
            jsw88,jsw89,iisw9,iisw100,iisynch,iit1,iit2,iit3,iit4,  &
            iitconj,p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,p2esin,  &
            p2incl,p2mtc,p2ptc,p2period,p2ratrad,p2tconj,p3omega,p3q,  &
            p3t0,p3ecos,p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,  &
            p3tconj,p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,  &
            p4ptc,p4period,p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,  &
            p5esin,p5incl,p5mtc,p5ptc,p5period,p5ratrad,p5tconj,  &
            p6omega,p6q,p6t0,p6ecos,p6esin,p6incl,p6mtc,p6ptc,  &
            p6period,p6ratrad,p6tconj,p7omega,p7q,p7t0,p7ecos,p7esin,  &
            p7incl,p7mtc,p7ptc,p7period,p7ratrad,p7tconj,p8omega,p8q,  &
            p8t0,p8ecos,p8esin,p8incl,p8mtc,p8ptc,p8period,p8ratrad,  &
            p8tconj,pbmtc,pbptc,period,q,sa3,t0,tconj,teff1,teff2,  &
            tgrav1,tgrav2,alb1,alb2,argper,beam1,beam2,betarim,bigi,  &
            bigi2,bigi3,bigi4,bigbeta,bigbeta2,bigbeta3,bigbeta4,  &
            bin2q,bin2massdiff,bin2masssum,bin2raddiff,bin2radsum,  &
            bin2ratrad,contam,contams0,contams1,contams2,contams3,  &
            dbolx,dboly,density,rdphase,dwavex,dwavey,ecc,ecosw,  &
            fill1,fill2,finc,fm,frac1,frac2,g10,g3,g6,g7,g8,g9,gamma,  &
            hh,massdiff,masssum,ocose,omega1,omega2,omega3,omega4,  &
            omega5,omega6,omega7,omega8,omega9,omega10,omegadot,  &
            osine,powercoeff,primk,primmass,primrad,pshift,rlx,  &
            raddiff,radsum,ratrad,rinner,rk3,rk4,rk5,rk6,rk7,rk8,rk9,  &
            rk10,router,sdarkint1,sdarkint2,sdarkint3,sdarkint4,  &
            sdarkint5,sdarkint6,sdarkint7,sdarkint8,sdarkint9,  &
            sdarkint10,secmass,secrad,separ,spot1parm,spot2parm,  &
            spotdparm,sw1,sw2,rsw23,rsw24,sw25,rsw26,rsw27,rsw28,  &
            rsw29,sw3,rsw30,sw47,ssw48,sw49,sw5,rsw6,rsw7,sw72,sw73,  &
            rsw8,sw80,sw81,sw82,sw83,ssw84,ssw85,sw86,sw87,sw88,sw89,  &
            rsw9,t10,t3,t6,t7,t8,t9,tdisk,temprat,tertomega,tertq,  &
            tertconj,tertecos,tertesin,tertincl,tertperiod,  &
            tertratrad,tertt0,wave,xi,iunit,fracsum,fracdiff,bin2m3,  &
            bin2m4,bin2r3,bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,  &
            sqp2ecos,sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,  &
            sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,  &
            sqp8ecos,sqp8esin,angsum1,angdiff1,angsum2,angdiff2,  &
            angsum3,angdiff3,angsum4,angdiff4,angsum5,angdiff5,  &
            angsum6,angdiff6,angsum7,angdiff7,angsum8,angdiff8,iimag,  &
            fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,  &
            p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,t3off,t4off,t5off,  &
            t6off,t7off,t8off,iversion,tesscontam,tessfilt,tessbin)
!
         CALL varassign(nvmax,svar,var,fill1,fill2,omega1,omega2,  &
            q,finc,teff1,teff2,betarim,rinner,router,tdisk,xi,rlx,  &
            separ,gamma,t3,g3,sa3,ecc,argper,pshift,spot1parm,  &
            spot2parm,spotdparm,period,t0,alb1,alb2,dwavex,dwavey,  &
            primmass,primk,primrad,ratrad,frac1,frac2,ecosw,temprat,  &
            bigi,bigbeta,density,tconj,beam1,beam2,contam,  &
            ocose,osine,isw29,tertperiod,tertt0,tertecos,tertesin,  &
            tertincl,tertomega,tertq,tgrav1,tgrav2,tertconj,omegadot,  &
            contams0,contams1,contams2,contams3,p2tconj,p2period,  &
            p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,p2ratrad,p3tconj,  &
            p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,  &
            p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,  &
            p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl,  &
            p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,p6esin,  &
            p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,p7ecos,  &
            p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,  &
            p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,sw72,sw73,sw49,  &
            sw80,sw81,sdarkint1,sdarkint2,sdarkint3,sdarkint4,  &
            sdarkint5,tertratrad,p1mtc,p1ptc,p2mtc,p2ptc,p3mtc,p3ptc,  &
            p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,p7mtc,p7ptc,p8mtc,  &
            p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,bigbeta2,bigbeta3,  &
            bigbeta4,bin2q,bin2massdiff,bin2masssum,bin2raddiff,  &
            bin2radsum,bin2ratrad,g10,g6,g7,g8,g9,massdiff,masssum,  &
            omega3,omega4,omega5,omega6,omega7,omega8,omega9,omega10,  &
            raddiff,radsum,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,  &
            sdarkint6,sdarkint7,sdarkint8,sdarkint9,sdarkint10,  &
            secmass,secrad,t10,t6,t7,t8,t9,fracsum,fracdiff,bin2m3,  &
            bin2m4,bin2r3,bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,  &
            sqp2ecos,sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,  &
            sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,  &
            sqp8ecos,sqp8esin,angsum1,angdiff1,angsum2,angdiff2,  &
            angsum3,angdiff3,angsum4,angdiff4,angsum5,angdiff5,  &
            angsum6,angdiff6,angsum7,angdiff7,angsum8,angdiff8,  &
            fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,  &
            p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,t3off,t4off,t5off,  &
            t6off,t7off,t8off,tesscontam)
!
         DO j=1,nterms
            vx1=vstart(j)
            vx2=vstep(j)
            vxmult=(vx2-vx1)
            vxadd=vx1
            varx=var(j)
            urand=(varx-vxadd)/vxmult
            IF(urand > 1.0_dp)urand=1.0_dp
            IF(urand < 0.0_dp)urand=0.0_dp
            parmarray(j,kkk)=varx
            IF(varx < vstart(j))THEN
               WRITE(*,220)svar(j),i16
               WRITE(*,230)varx,vstart(j),vstep(j)
               STOP
            END IF
            IF(varx > vstep(j))THEN
               WRITE(*,220)svar(j),i16
               WRITE(*,230)varx,vstart(j),vstep(j)
               STOP
            END IF
         END DO
      END IF
!
!   if ielete (isw9) is equal to Nstep(1) and istart > 1, then
!   assume a restart.
!
      IF((isw9save == nstep(1)).AND.(istart > 1))THEN
         kkk=i16
         CALL newgetinput(kkk,inalph1,inalph2,inalph3,inbet1,  &
            inbet2,inbet3,inradius,inref,inseg,intheta,iirvfilt,  &
            iiatm,iicnb,iicnh,iicni,iicnj,iicnk,iicnr,iicnu,iicnv,  &
            iidark1,iidark2,iidraw,iiecheck,iiidint,iikeep,iilaw,  &
            iism1,iisw1,iisw12,iisw13,iisw2,iisw21,iisw22,iisw23,  &
            iisw24,iisw25,iisw26,iisw27,iisw28,iisw29,iisw3,iisw30,  &
            iisw31,iisw32,iisw33,iisw34,iisw4,iisw5,iisw6,iisw7,  &
            iisw8,jsw80,jsw81,jsw82,jsw83,jsw84,jsw85,jsw86,jsw87,  &
            jsw88,jsw89,iisw9,iisw100,iisynch,iit1,iit2,iit3,iit4,  &
            iitconj,p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,p2esin,  &
            p2incl,p2mtc,p2ptc,p2period,p2ratrad,p2tconj,p3omega,p3q,  &
            p3t0,p3ecos,p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,  &
            p3tconj,p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,  &
            p4ptc,p4period,p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,  &
            p5esin,p5incl,p5mtc,p5ptc,p5period,p5ratrad,p5tconj,  &
            p6omega,p6q,p6t0,p6ecos,p6esin,p6incl,p6mtc,p6ptc,  &
            p6period,p6ratrad,p6tconj,p7omega,p7q,p7t0,p7ecos,p7esin,  &
            p7incl,p7mtc,p7ptc,p7period,p7ratrad,p7tconj,p8omega,p8q,  &
            p8t0,p8ecos,p8esin,p8incl,p8mtc,p8ptc,p8period,p8ratrad,  &
            p8tconj,pbmtc,pbptc,period,q,sa3,t0,tconj,teff1,teff2,  &
            tgrav1,tgrav2,alb1,alb2,argper,beam1,beam2,betarim,bigi,  &
            bigi2,bigi3,bigi4,bigbeta,bigbeta2,bigbeta3,bigbeta4,  &
            bin2q,bin2massdiff,bin2masssum,bin2raddiff,bin2radsum,  &
            bin2ratrad,contam,contams0,contams1,contams2,contams3,  &
            dbolx,dboly,density,rdphase,dwavex,dwavey,ecc,ecosw,  &
            fill1,fill2,finc,fm,frac1,frac2,g10,g3,g6,g7,g8,g9,gamma,  &
            hh,massdiff,masssum,ocose,omega1,omega2,omega3,omega4,  &
            omega5,omega6,omega7,omega8,omega9,omega10,omegadot,  &
            osine,powercoeff,primk,primmass,primrad,pshift,rlx,  &
            raddiff,radsum,ratrad,rinner,rk3,rk4,rk5,rk6,rk7,rk8,rk9,  &
            rk10,router,sdarkint1,sdarkint2,sdarkint3,sdarkint4,  &
            sdarkint5,sdarkint6,sdarkint7,sdarkint8,sdarkint9,  &
            sdarkint10,secmass,secrad,separ,spot1parm,spot2parm,  &
            spotdparm,sw1,sw2,rsw23,rsw24,sw25,rsw26,rsw27,rsw28,  &
            rsw29,sw3,rsw30,sw47,ssw48,sw49,sw5,rsw6,rsw7,sw72,sw73,  &
            rsw8,sw80,sw81,sw82,sw83,ssw84,ssw85,sw86,sw87,sw88,sw89,  &
            rsw9,t10,t3,t6,t7,t8,t9,tdisk,temprat,tertomega,tertq,  &
            tertconj,tertecos,tertesin,tertincl,tertperiod,  &
            tertratrad,tertt0,wave,xi,iunit,fracsum,fracdiff,bin2m3,  &
            bin2m4,bin2r3,bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,  &
            sqp2ecos,sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,  &
            sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,  &
            sqp8ecos,sqp8esin,angsum1,angdiff1,angsum2,angdiff2,  &
            angsum3,angdiff3,angsum4,angdiff4,angsum5,angdiff5,  &
            angsum6,angdiff6,angsum7,angdiff7,angsum8,angdiff8,iimag,  &
            fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,  &
            p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,t3off,t4off,t5off,  &
            t6off,t7off,t8off,iversion,tesscontam,tessfilt,tessbin)
!
         CALL varassign(nvmax,svar,var,fill1,fill2,omega1,omega2,  &
            q,finc,teff1,teff2,betarim,rinner,router,tdisk,xi,rlx,  &
            separ,gamma,t3,g3,sa3,ecc,argper,pshift,spot1parm,  &
            spot2parm,spotdparm,period,t0,alb1,alb2,dwavex,dwavey,  &
            primmass,primk,primrad,ratrad,frac1,frac2,ecosw,temprat,  &
            bigi,bigbeta,density,tconj,beam1,beam2,contam,  &
            ocose,osine,isw29,tertperiod,tertt0,tertecos,tertesin,  &
            tertincl,tertomega,tertq,tgrav1,tgrav2,tertconj,omegadot,  &
            contams0,contams1,contams2,contams3,p2tconj,p2period,  &
            p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,p2ratrad,p3tconj,  &
            p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,  &
            p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,  &
            p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl,  &
            p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,p6esin,  &
            p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,p7ecos,  &
            p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,  &
            p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,sw72,sw73,sw49,  &
            sw80,sw81,sdarkint1,sdarkint2,sdarkint3,sdarkint4,  &
            sdarkint5,tertratrad,p1mtc,p1ptc,p2mtc,p2ptc,p3mtc,p3ptc,  &
            p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,p7mtc,p7ptc,p8mtc,  &
            p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,bigbeta2,bigbeta3,  &
            bigbeta4,bin2q,bin2massdiff,bin2masssum,bin2raddiff,  &
            bin2radsum,bin2ratrad,g10,g6,g7,g8,g9,massdiff,masssum,  &
            omega3,omega4,omega5,omega6,omega7,omega8,omega9,omega10,  &
            raddiff,radsum,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,  &
            sdarkint6,sdarkint7,sdarkint8,sdarkint9,sdarkint10,  &
            secmass,secrad,t10,t6,t7,t8,t9,fracsum,fracdiff,bin2m3,  &
            bin2m4,bin2r3,bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,  &
            sqp2ecos,sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,  &
            sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,  &
            sqp8ecos,sqp8esin,angsum1,angdiff1,angsum2,angdiff2,  &
            angsum3,angdiff3,angsum4,angdiff4,angsum5,angdiff5,  &
            angsum6,angdiff6,angsum7,angdiff7,angsum8,angdiff8,  &
            fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,  &
            p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,t3off,t4off,t5off,  &
            t6off,t7off,t8off,tesscontam)
!
         DO j=1,nterms
            vx1=vstart(j)
            vx2=vstep(j)
            vxmult=(vx2-vx1)
            vxadd=vx1
            varx=var(j)
            urand=(varx-vxadd)/vxmult
            IF(urand > 1.0_dp)urand=1.0_dp
            IF(urand < 0.0_dp)urand=0.0_dp
            parmarray(j,kkk)=varx
            IF(varx < vstart(j))THEN
               WRITE(*,220)svar(j),i16
               WRITE(*,230)varx,vstart(j),vstep(j)
               STOP
            END IF
            IF(varx > vstep(j))THEN
               WRITE(*,220)svar(j),i16
               WRITE(*,230)varx,vstart(j),vstep(j)
               STOP
            END IF
         END DO
      END IF

   END DO
!
!   alter subsequent sets according to input parameters
!
   IF(isw9save < np)THEN
      DO i=isw9save+1,np
         i1=1
         IF(isw9save > 1)THEN
            i1=INT(ran6(idum)*REAL(isw9save,KIND=dp))+1
            IF(i1 < 1)i1=1
            IF(i1 > isw9save)i1=isw9save
         END IF
!
!  by default, use a Gaussian to tweak all parameters
!
         george=ran6(idum)
         IF(george < rfracmod)THEN
            DO j=1,nterms
               urand=gsig*gasdev6(idum)
               urand=urand*(vstep(j)-vstart(j))
               parmarray(j,i)=parmarray(j,i1)+urand
               IF(parmarray(j,i) < vstart(j))parmarray(j,i)=vstart(j)
               IF(parmarray(j,i) > vstep(j))parmarray(j,i)=vstep(j)
            END DO
         ELSE
            DO j=1,nterms
               parmarray(j,i)=parmarray(j,i1)
            END DO
            DO j=1,numparm
               i2=INT(ran6(idum)*REAL(nterms,KIND=dp))+1
               IF(i2 < 1)i2=1
               IF(i2 > nterms)i2=nterms
               urand=ran6(idum)*(vstep(i2)-vstart(i2))
               parmarray(i2,i)=vstart(i2)+urand
               IF(parmarray(i2,i) < vstart(i2))parmarray(i2,i)=vstart(i2)
               IF(parmarray(i2,i) > vstep(i2))parmarray(i2,i)=vstep(i2)
            END DO
         END IF
      END DO
   END IF
!
!    scramble the order of the parameter array
!
   IF(istart == 0)THEN
      DO i=1,np
110      i1=INT(ran6(idum)*REAL(np,KIND=dp))+1
         IF(i1 < 1)i1=1
         IF(i1 > np)i1=np
         IF(i1 == i)GO TO 110
         DO j=1,nterms
            ttemp=parmarray(j,i1)
            parmarray(j,i1)=parmarray(j,i)
            parmarray(j,i)=ttemp
         END DO
      END DO
   END IF
!
!  Figure out how long the model arrays need to be, and
!  allocate them
!
!  tstart=sw23,  tend=sw24,  tstep=sw9
!  Ngap=isw31,   itime=isw7
!
   CALL modellength(isw31,gaplow,gaphigh,nlength,sw23,sw24,sw9,isw7,dphase)
   nmaxphase=nlength
!
   ALLOCATE(xmod(nmaxphase),ymodu(nmaxphase),ymodb(nmaxphase))
   ALLOCATE(ymodv(nmaxphase),ymodr(nmaxphase),ymodi(nmaxphase))
   ALLOCATE(ymodj(nmaxphase),ymodh(nmaxphase),ymodk(nmaxphase))
   ALLOCATE(ymods1(nmaxphase),ymods2(nmaxphase),ymods3(nmaxphase))
   ALLOCATE(ymods4(nmaxphase),ymods5(nmaxphase),ymodd(nmaxphase))
   ALLOCATE(xrvmod(nmaxphase),rv1(nmaxphase),rv2(nmaxphase))
   ALLOCATE(rv3(nmaxphase),rv4(nmaxphase),rv5(nmaxphase))
   ALLOCATE(drv1(nmaxphase),drv2(nmaxphase),drv3(nmaxphase))
   ALLOCATE(drv4(nmaxphase),drv5(nmaxphase))
   ALLOCATE(fracs1(nmaxphase,4),fracs2(nmaxphase,4))
   ALLOCATE(fracs3(nmaxphase,4),fracs4(nmaxphase,4))
   ALLOCATE(fracs5(nmaxphase,4),fracs6(nmaxphase,4))
   ALLOCATE(fracs7(nmaxphase,4),fracs8(nmaxphase,4))
!
   xmod=0.0_dp
   ymodu=0.0_dp
   ymodb=0.0_dp
   ymodv=0.0_dp
   ymodr=0.0_dp
   ymodi=0.0_dp
   ymodj=0.0_dp
   ymodh=0.0_dp
   ymodk=0.0_dp
   ymods1=0.0_dp
   ymods2=0.0_dp
   ymods3=0.0_dp
   ymods4=0.0_dp
   ymods5=0.0_dp
   ymodd=0.0_dp
   xrvmod=0.0_dp
   rv1=0.0_dp
   rv2=0.0_dp
   rv3=0.0_dp
   rv4=0.0_dp
   rv5=0.0_dp
   drv1=0.0_dp
   drv2=0.0_dp
   drv3=0.0_dp
   drv4=0.0_dp
   drv5=0.0_dp
   fracs1=0.0_dp
   fracs2=0.0_dp
   fracs3=0.0_dp
   fracs4=0.0_dp
   fracs5=0.0_dp
   fracs6=0.0_dp
   fracs7=0.0_dp
   fracs8=0.0_dp
!
!  If irecover is a multiple of 5 and is 30 or larger, attempt
!  to read the restart file.
!
   IF((irecover >= 30).and.(mod(irecover,5) == 0))THEN
      WRITE(extension,320)1000000+irecover
      ios=0
      OPEN(UNIT=70,file='demcmc_recover.'//extension,STATUS='old',IOSTAT=ios)
      IF(ios > 0)THEN
         WRITE(*,*)'Error:  restart file demcmc_recover.',extension,' not found'
         STOP
      END IF
      ios=0
      READ(70,*,ERR=50,IOSTAT=ios)ntemp
      IF(ntemp /= nvar)THEN
        WRITE(*,*)'Error:  Nvar in recover file not compatible with gridloop.opt'
        STOP
      END IF
      DO mmm=1,nvar
        READ(70,*,ERR=50,IOSTAT=ios)tempstring
        IF(icnvrt(svar(mmm)(1:2)) /= icnvrt(tempstring(1:2)))THEN
          WRITE(*,*)'Error:  strings in recover file not compatible with gridloop.opt'
          STOP
        END IF
      END DO
      DO mmm=1,nvar
        READ(70,*,ERR=50,IOSTAT=ios)templow,temphigh
        IF(abs(templow-vstart(mmm))>10.0_dp*EPSILON(templow))THEN
          WRITE(*,*)'Error:  lower bound in recover file not compatible with gridloop.opt'
          STOP
        END IF           
        IF(abs(temphigh-vstep(mmm))>10.0_dp*EPSILON(temphigh))THEN
          WRITE(*,*)'Error:  upper bound in recover file not compatible with gridloop.opt'
          STOP
        END IF           
      END DO
      DO mmm=1,np
         READ(70,*,ERR=50,IOSTAT=ios)bigstring(mmm,1)
         READ(70,*,ERR=50,IOSTAT=ios)bigstring(mmm,2)
         IF((isw30 >= 3).AND.(it4 == 0))READ(70,*,ERR=50,IOSTAT=ios)bigstring(mmm,4)
         IF(isw24 >= 1)READ(70,*,ERR=50,IOSTAT=ios)bigstring(mmm,5)
      END DO
      DO mmm=1,np
         READ(70,*,ERR=50,IOSTAT=ios)i1,(parmarray(j,mmm),j=1,nterms)
      END DO
      DO mmm=1,np
         READ(70,*,ERR=50,IOSTAT=ios)i1,(oldparmarray(j,mmm),j=1,nterms)
      END DO
      DO mmm=1,np
         READ(70,*,ERR=50,IOSTAT=ios)i1,(ucparmarray(j,mmm),j=1,nterms)
      END DO
      DO mmm=1,np
         READ(70,*,ERR=50,IOSTAT=ios)i1,(olducparmarray(j,mmm),j=1,nterms)
      END DO
      DO mmm=1,np
         READ(70,*,ERR=50,IOSTAT=ios)i1,chiarr(mmm),oldchiarr(mmm),ichange(mmm)
      END DO
      DO mmm=1,nterms
         READ(70,*,ERR=50,IOSTAT=ios)i1,sss(mmm)
      END DO
      DO mmm=irecover-29,irecover
         READ(70,*,ERR=50,IOSTAT=ios)i1,fracjumparr(mmm)
      END DO
      READ(70,*,ERR=50,IOSTAT=ios)seed5,seed6,seed7,seed8,seed9
      READ(70,*,ERR=50,IOSTAT=ios)small,scalegam,siggam,iworst,iskip,idum,iupdate
      READ(70,*,ERR=50,IOSTAT=ios)ggamma1,ggamma2,ggamma3
      CLOSE(70)
50    IF(ios > 0)THEN
         WRITE(*,*)'Error reading demcmc_recover.'//extension
         STOP
      END IF
!  
      ios=0
      OPEN(UNIT=38,FILE='demcmcELC.out',POSITION='append',STATUS ='old')
      OPEN(UNIT=70,FILE='ELCjunk.demcmcELC.out',STATUS='old')
      DO mmm=1,irecover
        READ(70,*,ERR=51)col1,col2,col3,col4,col5,col6,col7,col8,col9
        WRITE(38,345)col1,col2,col3,col4,col5,col6,col7,col8,col9
      END DO
      CLOSE(38)
      CLOSE(70)
51    IF(ios > 0)THEN
         WRITE(*,*)'Error reading demcmcELC.out'
         STOP
      END IF
      irecover=irecover+1
      GO TO 69
   END IF
!
!   open the normal output files
!
   OPEN(UNIT=45,FILE='generation.'//extension,STATUS='unknown')
   OPEN(UNIT=46,FILE='ELCparm.'//extension,STATUS='unknown')
   IF(isw24 >= 1)OPEN(UNIT=47,FILE='ELCratio.'//extension,STATUS='unknown')
   OPEN(UNIT=55,FILE='chi.'//extension,STATUS='unknown')
   IF(isw30 >= 3)OPEN(UNIT=49,FILE='ELCdynparm.'//extension,STATUS='unknown')
!
!  demcmc_loop01
!$omp  parallel default(none) firstprivate(NRVphase,Nphase,P1qTc, &
!$omp   P2Omega,P2Q,P2T0,P2ecos,P2esin,P2incl,P2period,P2qTc,P2ratrad, &
!$omp   P2tconj,P3Omega,P3Q,P3T0,P3ecos,P3esin,P3incl,P3period,P3qTc, &
!$omp   P3ratrad,P3tconj,P4Omega,P4Q,P4T0,P4ecos,P4esin,P4incl, &
!$omp   P4period,P4qTc,P4ratrad,P4tconj,P5Omega,P5Q,P5T0,P5ecos, &
!$omp   P5esin,P5incl,P5period,P5qTc,P5ratrad,P5tconj,P6Omega,P6Q, &
!$omp   P6T0,P6ecos,P6esin,P6incl,P6period,P6qTc,P6ratrad,P6tconj, &
!$omp   P7Omega,P7Q,P7T0,P7ecos,P7esin,P7incl,P7period,P7qTc,P7ratrad, &
!$omp   P7tconj,P8Omega,P8Q,P8Qtc,P8T0,P8ecos,P8esin,P8incl,P8period, &
!$omp   P8ratrad,P8tconj,Period,Q,SA3,T0,T1off,T2off,T3off,T4off, &
!$omp   T5off,T6off,T7off,T8off,Tbinoff,Tconj,Teff1,Teff2,Tgrav1, &
!$omp   Tgrav2,alb1,alb2,angdiff1,angdiff2,angdiff3,angdiff4,angdiff5, &
!$omp   angdiff6,angdiff7,angdiff8,angsum1,angsum2,angsum3,angsum4, &
!$omp   angsum5,angsum6,angsum7,angsum8,argper,beam1,beam2,betarim, &
!$omp   bigI,bigI2,bigI3,bigI4,bigbeta,bigbeta2,bigbeta3,bigbeta4, &
!$omp   bin2M3,bin2M4,bin2Q,bin2R3,bin2R4,bin2massdiff,bin2masssum, &
!$omp   bin2raddiff,bin2radsum,bin2ratrad,binqTc,contam,contamS0, &
!$omp   contamS1,contamS2,contamS3,dbolx,dboly,density,dphase,dwavex, &
!$omp   dwavey,ecc,ecosw,errB,errH,errI,errJ,errK,errR,errRV1, &
!$omp   errRV1array,errRV2,errRV2array,errRV3,errRV3array,errRV4array, &
!$omp   errRV5array,errU,errV,fill1,fill2,filldiff,fillsum,finc,fm, &
!$omp   frac1,frac2,fracdiff,fracsum,g10,g3,g6,g7,g8,g9,gamma,i16, &
!$omp   ibest,ichilabel,ifastflag,ioutflag,massdiff,masssum,ocose, &
!$omp   omega1,omega10,omega2,omega3,omega4,omega5,omega6,omega7, &
!$omp   omega8,omega9,omegadot,osine,p1mtc,p1ptc,p2mtc,p2ptc,p3mtc, &
!$omp   p3ptc,p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,p7mtc,p7ptc,p8mtc, &
!$omp   p8ptc,pbmtc,pbptc,powercoeff,primK,primmass,primrad,pshift, &
!$omp   rLx,raddiff,radsum,ratrad,rinner,rk10,rk3,rk4,rk5,rk6,rk7,rk8, &
!$omp   rk9,router,saveasini,savesep,sdarkint1,sdarkint10,sdarkint2, &
!$omp   sdarkint3,sdarkint4,sdarkint5,sdarkint6,sdarkint7,sdarkint8, &
!$omp   sdarkint9,secmass,secrad,separ,spot1parm,spot2parm,spotdparm, &
!$omp   sqP2ecos,sqP2esin,sqP3ecos,sqP3esin,sqP4ecos,sqP4esin, &
!$omp   sqP5ecos,sqP5esin,sqP6ecos,sqP6esin,sqP7ecos,sqP7esin, &
!$omp   sqP8ecos,sqP8esin,sqecos,sqesin,sqtertecos,sqtertesin,sw1,sw2, &
!$omp   sw23,sw24,sw25,sw26,sw27,sw28,sw29,sw3,sw30,sw47,sw48,sw49, &
!$omp   sw5,sw6,sw7,sw72,sw73,sw8,sw80,sw81,sw82,sw83,sw84,sw85,sw86, &
!$omp   sw87,sw88,sw89,sw9,t10,t3,t6,t7,t8,t9,tdisk,temprat,tertOmega, &
!$omp   tertQ,tertconj,tertecos,tertesin,tertincl,tertperiod, &
!$omp   tertratrad,tertt0,var,wave,xRV1,xRV1array,xRV2,xRV2array,xRV3, &
!$omp   xRV3array,xRV4,xRV4array,xRV5array,xdataB,xdataH,xdataI, &
!$omp   xdataJ,xdataK,xdataR,xdataU,xdataV,xi,yRV1,yRV1array,yRV2, &
!$omp   yRV2array,yRV3,yRV3array,yRV4,yRV4array,yRV5array,ydataB, &
!$omp   ydataH,ydataI,ydataJ,ydataK,ydataR,ydataU,ydataV,tesscontam) &
!$omp private(Ncycle,RV1,RV2,RV3,RV4,RV5,Tdur1,Tdur2,Tseps,Ttimes, &
!$omp   chi1,chilimb,chisqB,chisqH,chisqI,chisqJ,chisqK,chisqR, &
!$omp   chisqRV1,chisqRV2,chisqU,chisqV,compfracs,drv1,drv2,drv3,drv4, &
!$omp   drv5,dynparm,fracs1,fracs2,fracs3,fracs4,fracs5,fracs6,fracs7, &
!$omp   fracs8,fracstring,gamma1array,gamma2array,gamma3array, &
!$omp   gamma4array,gamma5array,ggamma1,ggamma2,ggamma3,ggamma4,line, &
!$omp   newparmstring,obsparm,ochi,ochidisk,ochilr,parmstring, &
!$omp   planetparm,resB,resH,resI,resJ,resK,resR,resRV1,resRV1array, &
!$omp   resRV2,resRV2array,resRV3,resRV3array,resRV4,resRV4array, &
!$omp   resRV5array,resTtimes,resU,resV,xRVmod,xmod,ymodB,ymodH,ymodI, &
!$omp   ymodJ,ymodK,ymodR,ymodU,ymodV,ymodd,ymods1,ymods2,ymods3, &
!$omp   ymods4,ymods5,zeroB,zeroH,zeroI,zeroJ,zeroK,zeroR,zeroU,zeroV) &
!$omp shared(NRV1,NRV1array,NRV2,NRV2array,NRV3,NRV3array, &
!$omp   NRV4,NRV4array,NRV5array,NSC,Nalph1,Nalph2,Nalph3,Nbet1,Nbet2, &
!$omp   Nbet3,NdataB,NdataH,NdataI,NdataJ,NdataK,NdataR,NdataU,NdataV, &
!$omp   Ndattot,Ndynwin,Nlines,Nmu,Nobscycle,Nobv,Nradius,Nref,Nseg, &
!$omp   Nset,Nterms,Ntheta,Tmax,Tmin,arrsobx,arrsoby,atmT,atmg, &
!$omp   atmint1,atmint2,atmint3,atmint4,atmint5,atmint6,atmint7, &
!$omp   atmint8,atmmu,bigstring,chiarr,dynwinhigh,dynwinlow,eobv, &
!$omp   errRV4,gaphigh,gaplow,gmax,gmin,hh,iRVfilt,iatm,ichange,icnB, &
!$omp   icnH,icnI,icnJ,icnK,icnR,icnRV1,icnRV1array,icnRV2,iimag, &
!$omp   icnRV2array,icnRV3,icnRV3array,icnRV4,icnRV4array,icnRV5array, &
!$omp   icnU,icnV,icnarray,idark1,idark2,idraw,iecheck,ifixgamma, &
!$omp   ifrac,iidint,ikeep,ilaw,ilum,imag,isavNB,isavNH,isavNI,isavNJ, &
!$omp   isavNK,isavNR,isavNU,isavNV,isavRV1,isavRV2,iskip,ism1,istart, &
!$omp   isvel1,isvel2,isw1,isw100,isw12,isw13,isw2,isw21,isw22,isw23, &
!$omp   isw24,isw25,isw26,isw27,isw28,isw29,isw3,isw30,isw31,isw32, &
!$omp   isw33,isw34,isw4,isw5,isw6,isw7,isw8,isw80,isw81,isw82,isw83, &
!$omp   isw84,isw85,isw86,isw87,isw88,isw89,isw9,isynch,it1,it2,it3, &
!$omp   it4,itconj,iunit,nnn,np,obsTerr,obsTtimes,obv,oldchiarr, &
!$omp   parmarray,saverrB,saverrH,saverrI,saverrJ,saverrK,saverrR, &
!$omp   saverrRV1,saverrRV1array,saverrRV2,saverrRV2array, &
!$omp   saverrRV3array,saverrRV4array,saverrRV5array,saverrU,saverrV, &
!$omp   savxRV1,savxRV1array,savxRV2,savxRV2array,savxRV3array, &
!$omp   savxRV4array,savxRV5array,savxdataB,savxdataH,savxdataI, &
!$omp   savxdataJ,savxdataK,savxdataR,savxdataU,savxdataV,savyRV1, &
!$omp   savyRV1array,savyRV2,savyRV2array,savyRV3array,savyRV4array, &
!$omp   savyRV5array,savydataB,savydataH,savydataI,savydataJ, &
!$omp   savydataK,savydataR,savydataU,savydataV,small,sobv,sss,svar, &
!$omp   thresh,xSC,ySC,iversion,tessfilt,tessbin,nmaxphase,ndatamax, &
!$omp   nmaxeclipse,maxlines,maxmu,rmed,idum)
!$omp do
!
   DO  i16=1,np
!
!$omp critical
!
      IF(isw22 >= 1)ifastflag=1
      IF(i16 == 1)ifastflag=0
      DO j=1,nterms
         var(j)=parmarray(j,i16)
      END DO
!
!$omp end critical
!
      chilimb=0.0_dp
      ifastflag=0
      ibest=0
      ichilabel=0
      ggamma1=0.0_dp
      ggamma2=0.0_dp
      ggamma3=0.0_dp
      ggamma4=0.0_dp
      CALL monster(nphase,nmaxphase,xmod,ymodu,ymodb,ymodv,ymodr,  &
         ymodi,ymodj,ymodh,ymodk,ymods1,ymods2,ymods3,ymodd,rv1,rv2,  &
         drv1,drv2,obsparm,ifastflag,nrvphase,xrvmod,fracs1,fracs2,  &
         fracs3,fracs4,fracs5,fracs6,fracs7,fracs8,chisqu,chisqb,  &
         chisqv,chisqr,chisqi,chisqj,chisqh,chisqk,chisqrv1,  &
         chisqrv2,chilimb,chi1,ndatau,xdatau,ydatau,erru,zerou,resu,  &
         ndatab,xdatab,ydatab,errb,zerob,resb,ndatav,xdatav,ydatav,  &
         errv,zerov,resv,ndatar,xdatar,ydatar,errr,zeror,resr,  &
         ndatai,xdatai,ydatai,erri,zeroi,resi,ndataj,xdataj,ydataj,  &
         errj,zeroj,resj,ndatah,xdatah,ydatah,errh,zeroh,resh,  &
         ndatak,xdatak,ydatak,errk,zerok,resk,nrv1,xrv1,yrv1,errrv1,  &
         nrv2,xrv2,yrv2,errrv2,ggamma1,ggamma2,nobv,sobv,obv,eobv,  &
         ochi,ochidisk,ochilr,nvmax,svar,var,saveasini,savxdatau,  &
         savydatau,saverru,savxdatab,savydatab,saverrb,savxdatav,  &
         savydatav,saverrv,savxdatar,savydatar,saverrr,savxdatai,  &
         savydatai,saverri,savxdataj,savydataj,saverrj,savxdatah,  &
         savydatah,saverrh,savxdatak,savydatak,saverrk,savxrv1,  &
         savyrv1,saverrrv1,savxrv2,savyrv2,saverrrv2,ifrac,ilum,i16,  &
         isavnu,isavnb,isavnv,isavnr,isavni,isavnj,isavnh,isavnk,  &
         isavrv1,isavrv2,isvel1,isvel2,ndatamax,ibest,ifixgamma,  &
         savesep,ichilabel,resrv1,resrv2,thresh,small,ncycle,ttimes,  &
         tseps,nobscycle,obsttimes,obsterr,icnarray,rv3,xrv3,yrv3,  &
         errrv3,icnrv3,resrv3,ggamma3,nrv3,parmstring,planetparm,  &
         dynparm,line,fill1,fill2,omega1,omega2,dphase,q,finc,teff1,  &
         teff2,tgrav1,tgrav2,betarim,rinner,router,tdisk,xi,alb1,  &
         alb2,rlx,period,fm,separ,gamma,wave,dbolx,dboly,dwavex,  &
         dwavey,t3,g3,sa3,density,sw1,sw2,sw3,t0,ecc,argper,pshift,  &
         sw5,sw6,sw7,sw8,sw9,primmass,primk,primrad,ratrad,frac1,  &
         frac2,ecosw,temprat,bigi,bigbeta,sw23,sw24,sw25,  &
         sw26,sw27,sw28,sw29,sw30,contam,tconj,beam1,beam2,ocose,  &
         osine,omegadot,contams0,contams1,contams2,contams3,sw47,  &
         sw48,sw49,gaplow,gaphigh,p2tconj,p2period,p2t0,p2ecos,  &
         p2esin,p2incl,p2omega,p2q,p2ratrad,p3tconj,p3period,p3t0,  &
         p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,p4tconj,p4period,  &
         p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,p4ratrad,p5tconj,  &
         p5period,p5t0,p5ecos,p5esin,p5incl,p5omega,p5q,p5ratrad,  &
         p6tconj,p6period,p6t0,p6ecos,p6esin,p6incl,p6omega,p6q,  &
         p6ratrad,p7tconj,p7period,p7t0,p7ecos,p7esin,p7incl,  &
         p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,p8ecos,p8esin,  &
         p8incl,p8omega,p8q,p8ratrad,xsc,ysc,spot1parm,spot2parm,  &
         spotdparm,nalph1,nbet1,nalph2,nbet2,ntheta,nradius,nref,  &
         idraw,iecheck,iidint,iatm,ism1,ilaw,icnu,icnb,icnv,icnr,  &
         icni,icnj,icnh,icnk,icnrv1,icnrv2,irvfilt,isw1,isw2,isw3,  &
         isw4,ikeep,isynch,isw5,isw6,isw7,isw8,isw9,idark1,idark2,  &
         isw12,isw13,isw21,isw22,isw23,isw24,isw25,isw26,isw27,  &
         isw28,isw29,isw30,isw31,isw32,isw33,isw34,nsc,compfracs,  &
         tertperiod,tertt0,tertecos,tertesin,tertincl,tertomega,  &
         tertq,tertconj,tertratrad,hh,sw72,sw73,nmaxeclipse,tdur1,  &
         tdur2,sw80,sw81,sw82,sw83,sw84,sw85,  &
         isw80,isw81,isw82,isw83,isw84,isw85,isw86,isw87,isw88,  &
         isw89,ymods4,ymods5,arrsobx,arrsoby,montemax,nrv4,xrv4,  &
         yrv4,rv4,icnrv4,resrv4,ggamma4,errrv4,sdarkint1,sdarkint2,  &
         sdarkint3,sdarkint4,sdarkint5,atmt,atmg,atmmu,atmint1,  &
         atmint2,atmint3,atmint4,atmint5,atmint6,atmint7,atmint8,  &
         tmin,nlines,nmu,nalph3,nbet3,itconj,it1,it2,  &
         it3,it4,ndynwin,dynwinlow,dynwinhigh,p1mtc,p1ptc,p2mtc,  &
         p2ptc,p3mtc,p3ptc,p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,  &
         p7mtc,p7ptc,p8mtc,p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,  &
         bigbeta2,bigbeta3,bigbeta4,bin2q,bin2massdiff,bin2masssum,  &
         bin2raddiff,bin2radsum,bin2ratrad,g10,g6,g7,g8,g9,massdiff,  &
         masssum,omega3,omega4,omega5,omega6,omega7,omega8,omega9,  &
         omega10,raddiff,radsum,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,  &
         sdarkint6,sdarkint7,sdarkint8,sdarkint9,sdarkint10,secmass,  &
         secrad,t10,t6,t7,t8,t9,nseg,isw100,iunit,fracsum,fracdiff,  &
         bin2m3,bin2m4,bin2r3,bin2r4,sqecos,sqesin,sqtertecos,  &
         sqtertesin,sqp2ecos,sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,  &
         sqp4esin,sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,  &
         sqp7esin,sqp8ecos,sqp8esin,angsum1,angdiff1,angsum2,  &
         angdiff2,angsum3,angdiff3,angsum4,angdiff4,angsum5,  &
         angdiff5,angsum6,angdiff6,angsum7,angdiff7,angsum8,  &
         angdiff8,imag,newparmstring,ioutflag,fillsum,filldiff,  &
         binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,  &
         tbinoff,t1off,t2off,t3off,t4off,t5off,t6off,t7off,t8off,  &
         fracstring,resttimes,drv3,drv4,drv5,rv5,nset,nrv1array,  &
         icnrv1array,xrv1array,yrv1array,errrv1array,nrv2array,  &
         icnrv2array,xrv2array,yrv2array,errrv2array,nrv3array,  &
         icnrv3array,xrv3array,yrv3array,errrv3array,nrv4array,  &
         icnrv4array,xrv4array,yrv4array,errrv4array,icnrv5array, &
         savxrv1array,savyrv1array,saverrrv1array,savxrv2array, &
         savyrv2array,saverrrv2array,resrv1array,resrv2array,  &
         resrv3array,resrv4array,resrv5array,gamma1array,  &
         gamma2array,gamma3array,gamma4array,gamma5array,0,  &
         iversion,tesscontam,tessfilt,tessbin,maxlines,maxmu)
!
!$omp critical
!
      bigstring(i16,1)=TRIM(line)
      bigstring(i16,2)=TRIM(newparmstring)
      bigstring(i16,3)=TRIM(planetparm)
      IF((isw30 >= 3).AND.(it4 == 0))bigstring(i16,4)=TRIM(dynparm)
      IF(isw24 >= 1)bigstring(i16,5)=TRIM(fracstring)
      IF(ifastflag >= 1)THEN
         IF(chi1 < 3.0_dp*small*ABS(REAL(ndattot-nterms,KIND=dp)))THEN
            ifastflag=0
         END IF
      END IF
!
      chiarr(i16)=chi1
      oldchiarr(i16)=chi1
      IF(chi1 < small)THEN
         small=chi1
         DO mmm=1,nvmax
            sss(mmm)=var(mmm)
         END DO
      END IF
!
!$omp end critical
!
      IF(isw22 == 0)THEN
         IF(rmed >= 1.0_dp)THEN
            CALL printsmallmed(small)
         ELSE
            CALL printsmall(small)
         END IF
      END IF
      nnn=0+istart
      IF(isw22 == 0)CALL printiter(i16,'iteration number = ',nnn,  &
         'generation number = ')
      WRITE(*,*)' '
!
!  continue loop over generation members
!
   END DO
!$omp enddo
!$omp end parallel
!
!  We want to record the parameters here.
!
   OPEN(UNIT=66,FILE='demcmc_fitparm.'//extension,STATUS='unknown')
   OPEN(UNIT=67,FILE='demcmc_starparm.'//extension,STATUS='unknown')
   OPEN(UNIT=70,FILE='demcmc_chi.'//extension,STATUS='unknown')
   IF(isw24 >= 1)OPEN(UNIT=71,FILE='demcmc_ratio.'//extension,STATUS='unknown')
   IF(isw30 >= 3)THEN
      IF(it4 == 0)THEN
         OPEN(UNIT=69,FILE='demcmc_dynparm.'//extension,STATUS='unknown')
      END IF
   END IF
   iworst=-10
   worst=-12345.6_dp
   DO jj=1,np
      lll=LEN_TRIM(bigstring(jj,1))
      WRITE(66,330)bigstring(jj,1)(26:lll)
      WRITE(67,330)TRIM(bigstring(jj,2))
      IF(isw24 > 0)WRITE(71,330)TRIM(bigstring(jj,5))
      tempchi=chiarr(jj)
      IF(tempchi > worst)THEN
         worst=tempchi
         iworst=jj
      END IF
      IF(tempchi > 1.0E+13_dp)tempchi=999999999999.9999_dp
      WRITE(70,340)tempchi
      IF(isw30 >= 3)THEN
         IF(it4 == 0)WRITE(69,330)TRIM(bigstring(jj,4))
      END IF
   END DO
!
   CLOSE(66)
   CLOSE(67)
   CLOSE(69)
   CLOSE(70)
   IF(isw24 >= 1)CLOSE(71)
!
!   reset the variables at their best values and print the chi^2
!
   DO mmm=1,nvmax
      var(mmm)=sss(mmm)
      DO nnn=1,np
         oldparmarray(mmm,nnn)=parmarray(mmm,nnn)
      END DO
   END DO
!
   chilimb=0.0_dp
   IF(ilimbcheck > 1)THEN
      IF(ilaw == 4)CALL getchilimb(nvmax,nterms,svar,dwavex,dwavey,chilimb)
      IF(ilaw == 14)CALL getchilimb(nvmax,nterms,svar,dwavex,dwavey,chilimb)
   END IF
!
   ifastflag=0
   ibest=99
   ichilabel=0
   ioutflag=9
   CALL monster(nphase,nmaxphase,xmod,ymodu,ymodb,ymodv,ymodr,  &
      ymodi,ymodj,ymodh,ymodk,ymods1,ymods2,ymods3,ymodd,rv1,rv2,  &
      drv1,drv2,obsparm,ifastflag,nrvphase,xrvmod,fracs1,fracs2,  &
      fracs3,fracs4,fracs5,fracs6,fracs7,fracs8,chisqu,chisqb,  &
      chisqv,chisqr,chisqi,chisqj,chisqh,chisqk,chisqrv1,chisqrv2,  &
      chilimb,chi1,ndatau,xdatau,ydatau,erru,zerou,resu,ndatab,  &
      xdatab,ydatab,errb,zerob,resb,ndatav,xdatav,ydatav,errv,  &
      zerov,resv,ndatar,xdatar,ydatar,errr,zeror,resr,ndatai,  &
      xdatai,ydatai,erri,zeroi,resi,ndataj,xdataj,ydataj,errj,  &
      zeroj,resj,ndatah,xdatah,ydatah,errh,zeroh,resh,ndatak,  &
      xdatak,ydatak,errk,zerok,resk,nrv1,xrv1,yrv1,errrv1,nrv2,  &
      xrv2,yrv2,errrv2,ggamma1,ggamma2,nobv,sobv,obv,eobv,ochi,  &
      ochidisk,ochilr,nvmax,svar,var,saveasini,savxdatau,savydatau,  &
      saverru,savxdatab,savydatab,saverrb,savxdatav,savydatav,  &
      saverrv,savxdatar,savydatar,saverrr,savxdatai,savydatai,  &
      saverri,savxdataj,savydataj,saverrj,savxdatah,savydatah,  &
      saverrh,savxdatak,savydatak,saverrk,savxrv1,savyrv1,  &
      saverrrv1,savxrv2,savyrv2,saverrrv2,ifrac,ilum,i16,isavnu,  &
      isavnb,isavnv,isavnr,isavni,isavnj,isavnh,isavnk,isavrv1,  &
      isavrv2,isvel1,isvel2,ndatamax,ibest,ifixgamma,savesep,  &
      ichilabel,resrv1,resrv2,thresh,small,ncycle,ttimes,tseps,  &
      nobscycle,obsttimes,obsterr,icnarray,rv3,xrv3,yrv3,errrv3,  &
      icnrv3,resrv3,ggamma3,nrv3,parmstring,planetparm,dynparm,  &
      line,fill1,fill2,omega1,omega2,dphase,q,finc,teff1,teff2,  &
      tgrav1,tgrav2,betarim,rinner,router,tdisk,xi,alb1,alb2,rlx,  &
      period,fm,separ,gamma,wave,dbolx,dboly,dwavex,dwavey,t3,g3,  &
      sa3,density,sw1,sw2,sw3,t0,ecc,argper,pshift,sw5,sw6,sw7,sw8,  &
      sw9,primmass,primk,primrad,ratrad,frac1,frac2,ecosw,temprat,  &
      bigi,bigbeta,sw23,sw24,sw25,sw26,sw27,sw28,sw29,  &
      sw30,contam,tconj,beam1,beam2,ocose,osine,omegadot,contams0,  &
      contams1,contams2,contams3,sw47,sw48,sw49,gaplow,gaphigh,  &
      p2tconj,p2period,p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,  &
      p2ratrad,p3tconj,p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,  &
      p3q,p3ratrad,p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,  &
      p4omega,p4q,p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,  &
      p5incl,p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,  &
      p6esin,p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,  &
      p7ecos,p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,  &
      p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,xsc,ysc,  &
      spot1parm,spot2parm,spotdparm,nalph1,nbet1,nalph2,nbet2,  &
      ntheta,nradius,nref,idraw,iecheck,iidint,iatm,ism1,ilaw,icnu,  &
      icnb,icnv,icnr,icni,icnj,icnh,icnk,icnrv1,icnrv2,irvfilt,  &
      isw1,isw2,isw3,isw4,ikeep,isynch,isw5,isw6,isw7,isw8,isw9,  &
      idark1,idark2,isw12,isw13,isw21,isw22,isw23,isw24,isw25,  &
      isw26,isw27,isw28,isw29,isw30,isw31,isw32,isw33,isw34,nsc,  &
      compfracs,tertperiod,tertt0,tertecos,tertesin,tertincl,  &
      tertomega,tertq,tertconj,tertratrad,hh,sw72,sw73,nmaxeclipse,  &
      tdur1,tdur2,sw80,sw81,sw82,sw83,sw84,sw85,  &
      isw80,isw81,isw82,isw83,isw84,isw85,isw86,isw87,isw88,  &
      isw89,ymods4,ymods5,arrsobx,arrsoby,montemax,nrv4,xrv4,yrv4,  &
      rv4,icnrv4,resrv4,ggamma4,errrv4,sdarkint1,sdarkint2,  &
      sdarkint3,sdarkint4,sdarkint5,atmt,atmg,atmmu,atmint1,  &
      atmint2,atmint3,atmint4,atmint5,atmint6,atmint7,atmint8,  &
      tmin,nlines,nmu,nalph3,nbet3,itconj,it1,it2,it3,  &
      it4,ndynwin,dynwinlow,dynwinhigh,p1mtc,p1ptc,p2mtc,p2ptc,  &
      p3mtc,p3ptc,p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,p7mtc,p7ptc,  &
      p8mtc,p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,bigbeta2,bigbeta3,  &
      bigbeta4,bin2q,bin2massdiff,bin2masssum,bin2raddiff,  &
      bin2radsum,bin2ratrad,g10,g6,g7,g8,g9,massdiff,masssum,  &
      omega3,omega4,omega5,omega6,omega7,omega8,omega9,omega10,  &
      raddiff,radsum,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,sdarkint6,  &
      sdarkint7,sdarkint8,sdarkint9,sdarkint10,secmass,secrad,t10,  &
      t6,t7,t8,t9,nseg,isw100,iunit,fracsum,fracdiff,bin2m3,bin2m4,  &
      bin2r3,bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,  &
      sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,  &
      sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,  &
      sqp8esin,angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3,  &
      angsum4,angdiff4,angsum5,angdiff5,angsum6,angdiff6,angsum7,  &
      angdiff7,angsum8,angdiff8,imag,newparmstring,ioutflag,  &
      fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,  &
      p7qtc,p8qtc,tbinoff,t1off,t2off,t3off,t4off,t5off,t6off,  &
      t7off,t8off,fracstring,resttimes,drv3,drv4,drv5,rv5,nset,  &
      nrv1array,icnrv1array,xrv1array,yrv1array,errrv1array,  &
      nrv2array,icnrv2array,xrv2array,yrv2array,errrv2array,  &
      nrv3array,icnrv3array,xrv3array,yrv3array,errrv3array,  &
      nrv4array,icnrv4array,xrv4array,yrv4array,errrv4array,  &
      icnrv5array,savxrv1array,savyrv1array,saverrrv1array, &
      savxrv2array,savyrv2array,saverrrv2array,resrv1array,  &
      resrv2array,resrv3array,resrv4array,resrv5array,gamma1array,  &
      gamma2array,gamma3array,gamma4array,gamma5array,1,  &
      iversion,tesscontam,tessfilt,tessbin,maxlines,maxmu)
!
   ioutflag=0
   chiall=(chisqu+chisqb+chisqv+chisqr+chisqi+chisqj+chisqh+  &
      chisqk+chisqrv1+chisqrv2+ochi+chilimb)
!
   chiall=chi1
   WRITE(*,*)' '
   CALL prints(chiall)
!
   IF((isw30 >= 3).AND.(isw23 >= 1))THEN
      CALL writeeclipse(ncycle,ttimes,tseps,isw30,nmaxeclipse,tdur1,tdur2,0)
   END IF
!
   CALL writemodels(nphase,isw7,xmod,ymodu,ymodb,ymodv,ymodr,  &
      ymodi,ymodj,ymodh,ymodk,ymods1,ymods2,ymods3,ymods4,ymods5,  &
      t3,g3,sa3,iidint,ymodd,nrvphase,xrvmod,rv1,rv2,rv3,rv4,isw30,  &
      ggamma1,ggamma2,ggamma3,itime,ndatamax,ndatau,ndatab,ndatav,  &
      ndatar,ndatai,ndataj,ndatah,ndatak,nrv1,nrv2,nrv3,nrv4,  &
      xdatau,ydatau,erru,xdatab,ydatab,errb,xdatav,ydatav,errv,  &
      xdatar,ydatar,errr,xdatai,ydatai,erri,xdataj,ydataj,errj,  &
      xdatah,ydatah,errh,xdatak,ydatak,errk,xrv1,yrv1,errrv1,xrv2,  &
      yrv2,errrv2,xrv3,yrv3,errrv3,xrv4,yrv4,errrv4,resu,resb,resv,  &
      resr,resi,resj,resh,resk,icnu,icnb,icnv,icnr,icni,icnj,icnh,  &
      icnk,icnrv1,icnrv2,icnrv3,icnrv4,zerou,zerob,zerov,zeror,  &
      zeroi,zeroj,zeroh,zerok,resrv1,resrv2,resrv3,resrv4,drv1,  &
      drv2,imag,resttimes,icnarray,nobscycle,obsterr,nmaxeclipse,  &
      obsttimes,drv3,drv4,drv5,rv5,resrv1array,resrv2array,  &
      resrv3array,resrv4array,resrv5array,gamma1array,gamma2array,  &
      gamma3array,gamma4array,gamma5array,ifixgamma,nset,  &
      nrv1array,icnrv1array,xrv1array,yrv1array,errrv1array,  &
      nrv2array,icnrv2array,xrv2array,yrv2array,errrv2array,  &
      nrv3array,icnrv3array,xrv3array,yrv3array,errrv3array,  &
      nrv4array,icnrv4array,xrv4array,yrv4array,errrv4array,  &
      nrv5array,icnrv5array,xrv5array,yrv5array,errrv5array)
!
   igrid=1000000
   CALL newwritegridout(nalph1,nalph2,nalph3,nbet1,nbet2,nbet3,  &
      nradius,nref,nseg,ntheta,irvfilt,iatm,icnb,icnh,icni,icnj,  &
      icnk,icnr,icnu,icnv,idark1,idark2,idraw,iecheck,iidint,ikeep,  &
      ilaw,ism1,isw1,isw13,isw2,isw21,isw22,isw23,isw24,  &
      isw25,isw26,isw27,isw28,isw29,isw3,isw30,isw31,isw32,isw33,  &
      isw34,isw4,isw5,isw6,isw7,isw8,isw80,isw81,  &
      isw85,isw86,isw87,isw88,isw89,isw9,isw100,isynch,it1,it2,it3,  &
      it4,itconj,p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,p2esin,p2incl,  &
      p2mtc,p2ptc,p2period,p2ratrad,p2tconj,p3omega,p3q,p3t0,  &
      p3ecos,p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,p3tconj,  &
      p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,p4ptc,p4period,  &
      p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,p5esin,p5incl,p5mtc,  &
      p5ptc,p5period,p5ratrad,p5tconj,p6omega,p6q,p6t0,p6ecos,  &
      p6esin,p6incl,p6mtc,p6ptc,p6period,p6ratrad,p6tconj,p7omega,  &
      p7q,p7t0,p7ecos,p7esin,p7incl,p7mtc,p7ptc,p7period,p7ratrad,  &
      p7tconj,p8omega,p8q,p8t0,p8ecos,p8esin,p8incl,p8mtc,p8ptc,  &
      p8period,p8ratrad,p8tconj,pbmtc,pbptc,period,q,sa3,t0,tconj,  &
      teff1,teff2,tgrav1,tgrav2,alb1,alb2,argper,beam1,beam2,  &
      betarim,bigi,bigi2,bigi3,bigi4,bigbeta,bigbeta2,bigbeta3,  &
      bigbeta4,bin2q,bin2massdiff,bin2masssum,bin2raddiff,  &
      bin2radsum,bin2ratrad,contam,contams0,contams1,contams2,  &
      contams3,dbolx,dboly,dphase,dwavex,dwavey,ecc,ecosw,  &
      fill1,fill2,finc,frac1,frac2,g10,g3,g6,g7,g8,g9,gamma,hh,  &
      massdiff,masssum,ocose,omega1,omega2,omega3,omega4,omega5,  &
      omega6,omega7,omega8,omega9,omega10,osine,  &
      primk,primmass,primrad,pshift,rlx,raddiff,radsum,  &
      ratrad,rinner,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,router,  &
      sdarkint1,sdarkint2,sdarkint3,sdarkint4,sdarkint5,sdarkint6,  &
      sdarkint7,sdarkint8,sdarkint9,sdarkint10,secmass,secrad,  &
      separ,spot1parm,spot2parm,spotdparm,sw1,sw2,sw23,sw24,sw25,  &
      sw26,sw27,sw28,sw29,sw3,sw30,sw47,sw48,sw49,sw5,sw6,sw7,sw72,  &
      sw73,sw8,sw80,sw81,sw82,sw83,sw84,  &
      sw9,t10,t3,t6,t7,t8,t9,tdisk,temprat,tertomega,tertq,  &
      tertconj,tertecos,tertesin,tertincl,tertperiod,tertratrad,  &
      tertt0,wave,xi,iunit,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,  &
      bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin,  &
      sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin,  &
      sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin,  &
      angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3,angsum4,  &
      angdiff4,angsum5,angdiff5,angsum6,angdiff6,angsum7,angdiff7,  &
      angsum8,angdiff8,imag,fillsum,filldiff,binqtc,p1qtc,p2qtc,  &
      p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,  &
      t3off,t4off,t5off,t6off,t7off,t8off,igrid,iversion,  &
      tesscontam,tessfilt,tessbin)
!
   igrid=-99
   CALL newwritegridout(nalph1,nalph2,nalph3,nbet1,nbet2,nbet3,  &
      nradius,nref,nseg,ntheta,irvfilt,iatm,icnb,icnh,icni,icnj,  &
      icnk,icnr,icnu,icnv,idark1,idark2,idraw,iecheck,iidint,ikeep,  &
      ilaw,ism1,isw1,isw13,isw2,isw21,isw22,isw23,isw24,  &
      isw25,isw26,isw27,isw28,isw29,isw3,isw30,isw31,isw32,isw33,  &
      isw34,isw4,isw5,isw6,isw7,isw8,isw80,isw81,  &
      isw85,isw86,isw87,isw88,isw89,isw9,isw100,isynch,it1,it2,it3,  &
      it4,itconj,p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,p2esin,p2incl,  &
      p2mtc,p2ptc,p2period,p2ratrad,p2tconj,p3omega,p3q,p3t0,  &
      p3ecos,p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,p3tconj,  &
      p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,p4ptc,p4period,  &
      p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,p5esin,p5incl,p5mtc,  &
      p5ptc,p5period,p5ratrad,p5tconj,p6omega,p6q,p6t0,p6ecos,  &
      p6esin,p6incl,p6mtc,p6ptc,p6period,p6ratrad,p6tconj,p7omega,  &
      p7q,p7t0,p7ecos,p7esin,p7incl,p7mtc,p7ptc,p7period,p7ratrad,  &
      p7tconj,p8omega,p8q,p8t0,p8ecos,p8esin,p8incl,p8mtc,p8ptc,  &
      p8period,p8ratrad,p8tconj,pbmtc,pbptc,period,q,sa3,t0,tconj,  &
      teff1,teff2,tgrav1,tgrav2,alb1,alb2,argper,beam1,beam2,  &
      betarim,bigi,bigi2,bigi3,bigi4,bigbeta,bigbeta2,bigbeta3,  &
      bigbeta4,bin2q,bin2massdiff,bin2masssum,bin2raddiff,  &
      bin2radsum,bin2ratrad,contam,contams0,contams1,contams2,  &
      contams3,dbolx,dboly,dphase,dwavex,dwavey,ecc,ecosw,  &
      fill1,fill2,finc,frac1,frac2,g10,g3,g6,g7,g8,g9,gamma,hh,  &
      massdiff,masssum,ocose,omega1,omega2,omega3,omega4,omega5,  &
      omega6,omega7,omega8,omega9,omega10,osine,  &
      primk,primmass,primrad,pshift,rlx,raddiff,radsum,  &
      ratrad,rinner,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,router,  &
      sdarkint1,sdarkint2,sdarkint3,sdarkint4,sdarkint5,sdarkint6,  &
      sdarkint7,sdarkint8,sdarkint9,sdarkint10,secmass,secrad,  &
      separ,spot1parm,spot2parm,spotdparm,sw1,sw2,sw23,sw24,sw25,  &
      sw26,sw27,sw28,sw29,sw3,sw30,sw47,sw48,sw49,sw5,sw6,sw7,sw72,  &
      sw73,sw8,sw80,sw81,sw82,sw83,sw84,  &
      sw9,t10,t3,t6,t7,t8,t9,tdisk,temprat,tertomega,tertq,  &
      tertconj,tertecos,tertesin,tertincl,tertperiod,tertratrad,  &
      tertt0,wave,xi,iunit,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,  &
      bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin,  &
      sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin,  &
      sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin,  &
      angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3,angsum4,  &
      angdiff4,angsum5,angdiff5,angsum6,angdiff6,angsum7,angdiff7,  &
      angsum8,angdiff8,imag,fillsum,filldiff,binqtc,p1qtc,p2qtc,  &
      p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,  &
      t3off,t4off,t5off,t6off,t7off,t8off,igrid,iversion,  &
      tesscontam,tessfilt,tessbin)
!
   igrid=1000000
   CALL recordloopopt(udatafile,bdatafile,vdatafile,rdatafile,  &
      idatafile,jdatafile,hdatafile,kdatafile,rv1file,rv2file,  &
      nvmax,nvar,svar,var,vstart,stepsave,nstep,nobv,sobv,obv,eobv,  &
      vstep,rv3file,rv4file,rv5file,igrid)
!
   CLOSE(45)
   CLOSE(46)
   IF(isw24 > 0)CLOSE(47)
   CLOSE(55)
!
!   set a flag to signal the need to update the model
!
   iupdate=0
!
!   Main generation  loop
!
   scalegam=aa
   siggam=0.001_dp
!
!   If ieliete = np, and istart > 1, assume a restart.  Set
!   the random number seed to jdum
!
   IF((isw9save == np).AND.(istart > 1))THEN
      IF(isw32 < 0)THEN
         idum=ABS(isw32)
      ELSE
         idum=1234567
      END IF
      idum=0
      i1=INT(ran6(idum))
      idum=ioldseed
      IF(isw32.ne.0)THEN
         seed5=abs(isw32)
      ELSE
         seed5=153587801
      END IF
      seed6=-759022222
      seed7=1288503317
      seed8=-1718083407
      seed9=-123456789
!
      OPEN(UNIT=44,FILE='ELCseeds.opt',STATUS='old',ERR=22)
      READ(44,*,err=22)seed1,seed2,seed3
      CLOSE(44)
!
22    CALL init_seeds6(seed5,seed6,seed7,seed8,seed9)

   END IF
!
69 CONTINUE    ! we jump to here if the recovery file was read correctly
!
   DO  ig=irecover,ngen
!
!   Main population loop
!
      igen=ig
      nlength=np
!
      IF(igen+istart == istartcorr+1)THEN
         scalegam=scalegam*1.20_dp
         siggam=2.0_dp*siggam
      END IF
      IF(igen+istart == istartcorr)THEN
         CALL getcov(igen+istart-1,nback,nskip,nlength,nvar,  &
            covchol,invcovchol,aveparm,istartcorr-nback*nskip-1,2)
         DO i16=1,np
            DO i1=1,nvar
               corr(i1)=parmarray(i1,i16)-aveparm(i1)
            END DO
            uncorr=matmul(corr,invcovchol)
            DO i1=1,nvar
               ucparmarray(i1,i16)=uncorr(i1)
               olducparmarray(i1,i16)=uncorr(i1)
            END DO
         END DO
      END IF
      IF((igen+istart > istartcorr+1).AND.  &
         (MOD(igen+istart,nskip) == 0))CALL getcov(igen+istart-1,  &
         nback,nskip,nlength,nvar,covchol,invcovchol,aveparm,  &
         istartcorr-nback*nskip-1,2)
      IF(ig >= itemp)THEN
         temperature=1.0_dp
      ELSE
         temperature=(tempstart-1.0_dp)/REAL(itemp-1,KIND=dp)*REAL(1-(ig+istart),KIND=dp) &
            +tempstart
         IF(temperature < 1.0_dp)temperature=1.0_dp
      END IF
!
      WRITE(extension,320)ig+1000000+istart
!
      IF(MOD(ig,iskip+1) == 0)THEN
         OPEN(UNIT=45,FILE='generation.'//extension,STATUS='unknown')
         IF(isw24 >= 1)OPEN(UNIT=47,FILE='ELCratio.'//extension,STATUS='unknown')
         OPEN(UNIT=55,FILE='chi.'//extension,STATUS='unknown')
         OPEN(UNIT=46,FILE='ELCparm.'//extension,STATUS='unknown')
         IF((isw30 >= 3).AND.(it4 == 0))OPEN(UNIT=49,FILE=  &
            'ELCdynparm.'//extension,STATUS='unknown')
      END IF
!
!   Save the random number seed
!
!      idumsave=idum
!
!   loop over the population
!
      njump=0
      DO  i16=1,np
         chiarr(i16)=oldchiarr(i16)
         ifastflag=0
!
!   pick two random integers between 1 and np
!
130      i1=INT(ran6(idum)*REAL(np,KIND=dp))+1
         IF(i1 < 1)i1=1
         IF(i1 > np)i1=np
         IF(i1 == i16)GO TO 130
140      i2=INT(ran6(idum)*REAL(np,KIND=dp))+1
         IF(i2 < 1)i2=1
         IF(i2 > np)i2=np
         IF(i2 == i16)GO TO 140
         IF(i2 == i1)GO TO 140
!
         idxarr1(i16)=i1
         idxarr2(i16)=i2
         zzzarr(i16)=gasdev6(idum)
         probarr(i16)=ran6(idum)
      END DO
!
!  demcmc_loop02
!$omp  parallel default(none) firstprivate(NRVphase,Nphase,P1qTc, &
!$omp   P2Omega,P2Q,P2T0,P2ecos,P2esin,P2incl,P2period,P2qTc,P2ratrad, &
!$omp   P2tconj,P3Omega,P3Q,P3T0,P3ecos,P3esin,P3incl,P3period,P3qTc, &
!$omp   P3ratrad,P3tconj,P4Omega,P4Q,P4T0,P4ecos,P4esin,P4incl, &
!$omp   P4period,P4qTc,P4ratrad,P4tconj,P5Omega,P5Q,P5T0,P5ecos, &
!$omp   P5esin,P5incl,P5period,P5qTc,P5ratrad,P5tconj,P6Omega,P6Q, &
!$omp   P6T0,P6ecos,P6esin,P6incl,P6period,P6qTc,P6ratrad,P6tconj, &
!$omp   P7Omega,P7Q,P7T0,P7ecos,P7esin,P7incl,P7period,P7qTc,P7ratrad, &
!$omp   P7tconj,P8Omega,P8Q,P8Qtc,P8T0,P8ecos,P8esin,P8incl,P8period, &
!$omp   P8ratrad,P8tconj,Period,Q,SA3,T0,T1off,T2off,T3off,T4off, &
!$omp   T5off,T6off,T7off,T8off,Tbinoff,Tconj,Teff1,Teff2,Tgrav1, &
!$omp   Tgrav2,alb1,alb2,angdiff1,angdiff2,angdiff3,angdiff4,angdiff5, &
!$omp   angdiff6,angdiff7,angdiff8,angsum1,angsum2,angsum3,angsum4, &
!$omp   angsum5,angsum6,angsum7,angsum8,argper,beam1,beam2,betarim, &
!$omp   bigI,bigI2,bigI3,bigI4,bigbeta,bigbeta2,bigbeta3,bigbeta4, &
!$omp   bin2M3,bin2M4,bin2Q,bin2R3,bin2R4,bin2massdiff,bin2masssum, &
!$omp   bin2raddiff,bin2radsum,bin2ratrad,binqTc,contam,contamS0, &
!$omp   contamS1,contamS2,contamS3,density,dphase,dwavex,dwavey,ecc, &
!$omp   ecosw,errB,errH,errI,errJ,errK,errR,errRV1,errRV1array,errRV2, &
!$omp   errRV2array,errRV3,errRV3array,errRV4array,errRV5array,errU, &
!$omp   errV,fill1,fill2,filldiff,fillsum,finc,fm,frac1,frac2, &
!$omp   fracdiff,fracsum,g10,g3,g6,g7,g8,g9,gamma,i16,ibest,ichilabel, &
!$omp   ifastflag,ioutflag,massdiff,masssum,ocose, &
!$omp   omega1,omega10,omega2,omega3,omega4,omega5,omega6,omega7, &
!$omp   omega8,omega9,omegadot,osine,p1mtc,p1ptc,p2mtc,p2ptc,p3mtc, &
!$omp   p3ptc,p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,p7mtc,p7ptc,p8mtc, &
!$omp   p8ptc,pbmtc,pbptc,primK,primmass,primrad,pshift,rLx,raddiff, &
!$omp   radsum,ratrad,rinner,rk10,rk3,rk4,rk5,rk6,rk7,rk8,rk9,router, &
!$omp   rsw23,rsw24,rsw26,rsw27,rsw28,rsw29,rsw30,rsw6,rsw7,rsw8,rsw9, &
!$omp   saveasini,savesep,scalegam,sdarkint1,sdarkint10,sdarkint2, &
!$omp   sdarkint3,sdarkint4,sdarkint5,sdarkint6,sdarkint7,sdarkint8, &
!$omp   sdarkint9,secmass,secrad,separ,siggam,spot1parm,spot2parm, &
!$omp   spotdparm,sqP2ecos,sqP2esin,sqP3ecos,sqP3esin,sqP4ecos, &
!$omp   sqP4esin,sqP5ecos,sqP5esin,sqP6ecos,sqP6esin,sqP7ecos, &
!$omp   sqP7esin,sqP8ecos,sqP8esin,sqecos,sqesin,sqtertecos, &
!$omp   sqtertesin,ssw48,ssw85,sw1,sw2,sw23,sw24,sw25,sw26,sw27,sw28, &
!$omp   sw29,sw3,sw30,sw47,sw48,sw49,sw5,sw6,sw7,sw72,sw73,sw8,sw80, &
!$omp   sw81,sw82,sw83,sw84,sw85,sw86,sw87,sw88,sw89,sw9,t10,t3,t6,t7, &
!$omp   t8,t9,tdisk,temprat,tertOmega,tertQ,tertconj,tertecos, &
!$omp   tertesin,tertincl,tertperiod,tertratrad,tertt0,thresh,var, &
!$omp   vstart,vstep,xRV1,xRV1array,xRV2,xRV2array,xRV3,xRV3array, &
!$omp   xRV4,xRV4array,xRV5array,xdataB,xdataH,xdataI,xdataJ,xdataK, &
!$omp   xdataR,xdataU,xdataV,xi,yRV1,yRV1array,yRV2,yRV2array,yRV3, &
!$omp   yRV3array,yRV4,yRV4array,yRV5array,ydataB,ydataH,ydataI, &
!$omp   ydataJ,ydataK,ydataR,ydataU,ydataV,ssw84,tesscontam) &
!$omp private(Deltachi,Ncycle,RV1,RV2,RV3,RV4,RV5,Tdur1,Tdur2,Tseps, &
!$omp   Ttimes,chi1,chilimb,chisqB,chisqH,chisqI,chisqJ,chisqK,chisqR, &
!$omp   chisqRV1,chisqRV2,chisqU,chisqV,compfracs,drv1,drv2,drv3,drv4, &
!$omp   drv5,dynparm,fracs1,fracs2,fracs3,fracs4,fracs5,fracs6,fracs7, &
!$omp   fracs8,fracstring,gamma1array,gamma2array,gamma3array,ddiff, &
!$omp   gamma4array,gamma5array,ggamma1,ggamma2,ggamma3,ggamma4,i1,i2, &
!$omp   line,lll,lll1,lll2,lll3,newparmstring,obsparm,ochi,ochidisk, &
!$omp   ochilr,outstring,outstring1,outstring2,parmstring,planetparm, &
!$omp   prob,resB,resH,resI,resJ,resK,resR,resRV1,resRV1array,resRV2, &
!$omp   resRV2array,resRV3,resRV3array,resRV4,resRV4array,resRV5array, &
!$omp   resTtimes,resU,resV,uran,urand,varx,vx1,vx2,vxadd,vxmult, &
!$omp   xRVmod,xmod,ymodB,ymodH,ymodI,ymodJ,ymodK,ymodR,ymodU,ymodV, &
!$omp   ymodd,ymods1,ymods2,ymods3,ymods4,ymods5,zeroB,zeroH,zeroI, &
!$omp   zeroJ,zeroK,zeroR,zeroU,zeroV,corr,uncorr) &
!$omp shared(NRV1,NRV1array,NRV2,NRV2array,NRV3,NRV3array, &
!$omp   NRV4,NRV4array,NRV5array,NSC,Nalph1,Nalph2,Nalph3,Nbet1,Nbet2, &
!$omp   Nbet3,NdataB,NdataH,NdataI,NdataJ,NdataK,NdataR,NdataU,NdataV, &
!$omp   Ndynwin,Nlines,Nmu,Nobscycle,Nobv,Nradius,Nref,Nseg,Nset,jsw80, &
!$omp   Nterms,Ntheta,Tmax,Tmin,arrsobx,arrsoby,atmT,atmg,atmint1, &
!$omp   atmint2,atmint3,atmint4,atmint5,atmint6,atmint7,atmint8,atmmu, &
!$omp   bigstring,chiarr,dbolx,dboly,dynwinhigh,dynwinlow,eobv,errRV4, &
!$omp   gaphigh,gaplow,gmax,gmin,hh,iRVfilt,iatm,ichange,icnB,icnH, &
!$omp   icnI,icnJ,icnK,icnR,icnRV1,icnRV1array,icnRV2,icnRV2array, &
!$omp   icnRV3,icnRV3array,icnRV4,icnRV4array,icnRV5array,icnU,icnV, &
!$omp   icnarray,idark1,idark2,idraw,idxarr1,idxarr2,iecheck,iNseg, &
!$omp   ifixgamma,ifrac,ig,iiatm,iicnb,iicnh,iicni,iicnj,iicnk,iicnr, &
!$omp   iicnu,iicnv,iidark1,iidark2,iidint,iidraw,iiecheck,iiidint, &
!$omp   iikeep,iilaw,iirvfilt,iism1,iisw1,iisw12,iisw13,iisw2,iisw21, &
!$omp   iisw22,iisw23,iisw24,iisw25,iisw26,iisw27,iisw28,iisw29,iisw3, &
!$omp   iisw30,iisw31,iisw32,iisw33,iisw34,iisw4,iisw5,iisw6,iisw7, &
!$omp   iisw8,iisw9,iisynch,iit1,iit2,iit3,iit4,ikeep,ilaw, &
!$omp   ilimbcheck,ilum,iimag,imag,inalph1,inalph2,inalph3,inbet1, &
!$omp   inbet2, &
!$omp   inbet3,inradius,inref,intheta,iopened,isavNB,isavNH,isavNI, &
!$omp   isavNJ,isavNK,isavNR,isavNU,isavNV,isavRV1,isavRV2,iskip,ism1, &
!$omp   istart,isvel1,isvel2,isw1,isw100,isw12,isw13,isw2,isw21,isw22, &
!$omp   isw23,isw24,isw25,isw26,isw27,isw28,isw29,isw3,isw30,isw31, &
!$omp   isw32,isw33,isw34,isw4,isw5,isw6,isw7,isw8,isw80,isw81,isw82, &
!$omp   isw83,isw84,isw85,isw86,isw87,isw88,isw89,isw9,isynch,it1,it2, &
!$omp   it3,it4,itconj,iunit,iupdate,iworst,njump,nnn,np,obsTerr, &
!$omp   obsTtimes,obv,oldchiarr,parmarray,powercoeff,probarr,rdphase, &
!$omp   saverrB,saverrH,saverrI,saverrJ,saverrK,saverrR,saverrRV1, &
!$omp   saverrRV1array,saverrRV2,saverrRV2array,saverrRV3array, &
!$omp   saverrRV4array,saverrRV5array,saverrU,saverrV,savxRV1, &
!$omp   savxRV1array,savxRV2,savxRV2array,savxRV3array,savxRV4array, &
!$omp   savxRV5array,savxdataB,savxdataH,savxdataI,savxdataJ, &
!$omp   savxdataK,savxdataR,savxdataU,savxdataV,savyRV1,savyRV1array, &
!$omp   savyRV2,savyRV2array,savyRV3array,savyRV4array,savyRV5array, &
!$omp   savydataB,savydataH,savydataI,savydataJ,savydataK,savydataR, &
!$omp   savydataU,savydataV,small,sobv,sss,svar,temperature,wave,xSC, &
!$omp   ySC,zzz,zzzarr,jsw81,jsw85,jsw86,jsw87,jsw88,jsw89,iisw100, &
!$omp   iitconj,istartcorr,covchol,invcovchol,aveparm,oldparmarray, &
!$omp   ucparmarray,olducparmarray,nmaxphase,tessfilt,tessbin,iversion, &
!$omp   ndatamax,nmaxeclipse,maxlines,maxmu,rmed,idum)
!$omp do
!
      DO  i16=1,np
!$omp critical
!
         i1=idxarr1(i16)
         i2=idxarr2(i16)
         CALL istring('index',i16,outstring,lll)
         CALL istring('i1',i1,outstring1,lll1)
         CALL istring('i2',i2,outstring2,lll2)
!
         IF(isw22 == 0)THEN
            WRITE(*,*)' '
            WRITE(*,300)outstring(1:lll),outstring1(1:lll1),outstring2(1:lll2)
            CALL chistring('chi_index',oldchiarr(i16),outstring,lll)
         END IF
         CALL chistring('chi_i1',oldchiarr(i1),outstring1,lll2)
         CALL chistring('chi_i2',oldchiarr(i2),outstring2,lll3)
         IF(isw22 == 0)WRITE(*,300)TRIM(outstring),  &
            TRIM(outstring1),TRIM(outstring2)
         zzz=siggam*zzzarr(i16)
         CALL pstring('z',8,zzz,outstring,lll)
         CALL pstring('gamma_0',8,scalegam,outstring1,lll1)
         CALL pstring('gamma',8,scalegam*(1.0_dp+zzz),outstring2,lll2)
         IF(isw22 == 0)THEN
            WRITE(*,310)TRIM(outstring),TRIM(outstring1),TRIM(outstring2)
         END IF
         DO  j=1,nterms
            IF(ig+istart > istartcorr)THEN
               uncorr(j)=olducparmarray(j,i16)+(olducparmarray(j,i1)-  &
                  olducparmarray(j,i2))*scalegam*(1.0_dp+zzz)
            ELSE
               var(j)=oldparmarray(j,i16)+(oldparmarray(j,i1)-  &
                  oldparmarray(j,i2))*scalegam*(1.0_dp+zzz)
               IF(var(j) < vstart(j))THEN
                  ddiff=ABS(vstart(j)-var(j))
                  var(j)=vstart(j)+ddiff
               END IF
               IF(var(j) > vstep(j))THEN
                  ddiff=ABS(var(j)-vstep(j))
                  var(j)=vstep(j)-ddiff
               END IF
               IF(var(j) < vstart(j))THEN
                  ddiff=ABS(vstart(j)-var(j))
                  var(j)=vstart(j)+ddiff
               END IF
               IF(var(j) > vstep(j))THEN
                  ddiff=ABS(var(j)-vstep(j))
                  var(j)=vstep(j)-ddiff
               END IF
            END IF
!
         END DO
!
         IF(ig+istart > istartcorr)THEN
            corr=matmul(uncorr,covchol)
            DO j=1,nterms
               var(j)=corr(j)+aveparm(j)
               IF(var(j) < vstart(j))THEN
                  ddiff=ABS(vstart(j)-var(j))
                  var(j)=vstart(j)+ddiff
               END IF
               IF(var(j) > vstep(j))THEN
                  ddiff=ABS(var(j)-vstep(j))
                  var(j)=vstep(j)-ddiff
               END IF
               IF(var(j) < vstart(j))THEN
                  ddiff=ABS(vstart(j)-var(j))
                  var(j)=vstart(j)+ddiff
               END IF
               IF(var(j) > vstep(j))THEN
                  ddiff=ABS(var(j)-vstep(j))
                  var(j)=vstep(j)-ddiff
               END IF
            END DO
         END IF
!
!   Add the ability to insert a population member here
!
         IF(i16 == iworst)THEN
!
            iopened=0
!
            CALL newinsgridinput(iopened,inalph1,inalph2,inalph3,  &
               inbet1,inbet2,inbet3,inradius,inref,inseg,intheta,  &
               iirvfilt,iiatm,iicnb,iicnh,iicni,iicnj,iicnk,iicnr,  &
               iicnu,iicnv,iidark1,iidark2,iidraw,iiecheck,iiidint,  &
               iikeep,iilaw,iism1,iisw1,iisw13,iisw2,iisw21,  &
               iisw22,iisw23,iisw24,iisw25,iisw26,iisw27,iisw28,  &
               iisw29,iisw3,iisw30,iisw31,iisw32,iisw33,iisw34,iisw4,  &
               iisw5,iisw6,iisw7,iisw8,jsw80,jsw81,  &
               jsw85,jsw86,jsw87,jsw88,jsw89,iisw9,iisw100,iisynch,  &
               iit1,iit2,iit3,iit4,iitconj,p1mtc,p1ptc,p2omega,p2q,  &
               p2t0,p2ecos,p2esin,p2incl,p2mtc,p2ptc,p2period,  &
               p2ratrad,p2tconj,p3omega,p3q,p3t0,p3ecos,p3esin,p3incl,  &
               p3mtc,p3ptc,p3period,p3ratrad,p3tconj,p4omega,p4q,p4t0,  &
               p4ecos,p4esin,p4incl,p4mtc,p4ptc,p4period,p4ratrad,  &
               p4tconj,p5omega,p5q,p5t0,p5ecos,p5esin,p5incl,p5mtc,  &
               p5ptc,p5period,p5ratrad,p5tconj,p6omega,p6q,p6t0,  &
               p6ecos,p6esin,p6incl,p6mtc,p6ptc,p6period,p6ratrad,  &
               p6tconj,p7omega,p7q,p7t0,p7ecos,p7esin,p7incl,p7mtc,  &
               p7ptc,p7period,p7ratrad,p7tconj,p8omega,p8q,p8t0,  &
               p8ecos,p8esin,p8incl,p8mtc,p8ptc,p8period,p8ratrad,  &
               p8tconj,pbmtc,pbptc,period,q,sa3,t0,tconj,teff1,teff2,  &
               tgrav1,tgrav2,alb1,alb2,argper,beam1,beam2,betarim,  &
               bigi,bigi2,bigi3,bigi4,bigbeta,bigbeta2,bigbeta3,  &
               bigbeta4,bin2q,bin2massdiff,bin2masssum,bin2raddiff,  &
               bin2radsum,bin2ratrad,contam,contams0,contams1,  &
               contams2,contams3,dbolx,dboly,rdphase,dwavex,  &
               dwavey,ecc,ecosw,fill1,fill2,finc,frac1,frac2,g10,  &
               g3,g6,g7,g8,g9,gamma,hh,massdiff,masssum,ocose,omega1,  &
               omega2,omega3,omega4,omega5,omega6,omega7,omega8,  &
               omega9,omega10,osine,primk,  &
               primmass,primrad,pshift,rlx,raddiff,radsum,ratrad,  &
               rinner,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,router,  &
               sdarkint1,sdarkint2,sdarkint3,sdarkint4,sdarkint5,  &
               sdarkint6,sdarkint7,sdarkint8,sdarkint9,sdarkint10,  &
               secmass,secrad,separ,spot1parm,spot2parm,spotdparm,sw1,  &
               sw2,rsw23,rsw24,sw25,rsw26,rsw27,rsw28,rsw29,sw3,rsw30,  &
               sw47,ssw48,sw49,sw5,rsw6,rsw7,sw72,sw73,rsw8,sw80,sw81,  &
               sw82,sw83,ssw84,rsw9,t10,t3,  &
               t6,t7,t8,t9,tdisk,temprat,tertomega,tertq,tertconj,  &
               tertecos,tertesin,tertincl,tertperiod,tertratrad,  &
               tertt0,wave,xi,iunit,fracsum,fracdiff,bin2m3,bin2m4,  &
               bin2r3,bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,  &
               sqp2ecos,sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,  &
               sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,  &
               sqp8ecos,sqp8esin,angsum1,angdiff1,angsum2,angdiff2,  &
               angsum3,angdiff3,angsum4,angdiff4,angsum5,angdiff5,  &
               angsum6,angdiff6,angsum7,angdiff7,angsum8,angdiff8,  &
               iimag,fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,p4qtc,  &
               p5qtc,p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,t3off,  &
               t4off,t5off,t6off,t7off,t8off)
!
            IF(iopened > 0)THEN
               CALL varassign(nvmax,svar,var,fill1,fill2,omega1,  &
                  omega2,q,finc,teff1,teff2,betarim,rinner,router,  &
                  tdisk,xi,rlx,separ,gamma,t3,g3,sa3,ecc,argper,pshift,  &
                  spot1parm,spot2parm,spotdparm,period,t0,alb1,alb2,  &
                  dwavex,dwavey,primmass,primk,primrad,ratrad,frac1,  &
                  frac2,ecosw,temprat,bigi,bigbeta,density,  &
                  tconj,beam1,beam2,contam,ocose,osine,isw29,  &
                  tertperiod,tertt0,tertecos,tertesin,tertincl,  &
                  tertomega,tertq,tgrav1,tgrav2,tertconj,omegadot,  &
                  contams0,contams1,contams2,contams3,p2tconj,p2period,  &
                  p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,p2ratrad,  &
                  p3tconj,p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,  &
                  p3q,p3ratrad,p4tconj,p4period,p4t0,p4ecos,p4esin,  &
                  p4incl,p4omega,p4q,p4ratrad,p5tconj,p5period,p5t0,  &
                  p5ecos,p5esin,p5incl,p5omega,p5q,p5ratrad,p6tconj,  &
                  p6period,p6t0,p6ecos,p6esin,p6incl,p6omega,p6q,  &
                  p6ratrad,p7tconj,p7period,p7t0,p7ecos,p7esin,p7incl,  &
                  p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,p8ecos,  &
                  p8esin,p8incl,p8omega,p8q,p8ratrad,sw72,sw73,sw49,  &
                  sw80,sw81,sdarkint1,sdarkint2,sdarkint3,sdarkint4,  &
                  sdarkint5,tertratrad,p1mtc,p1ptc,p2mtc,p2ptc,p3mtc,  &
                  p3ptc,p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,p7mtc,  &
                  p7ptc,p8mtc,p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,  &
                  bigbeta2,bigbeta3,bigbeta4,bin2q,bin2massdiff,  &
                  bin2masssum,bin2raddiff,bin2radsum,bin2ratrad,g10,g6,  &
                  g7,g8,g9,massdiff,masssum,omega3,omega4,omega5,  &
                  omega6,omega7,omega8,omega9,omega10,raddiff,radsum,  &
                  rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,sdarkint6,sdarkint7,  &
                  sdarkint8,sdarkint9,sdarkint10,secmass,secrad,t10,t6,  &
                  t7,t8,t9,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,  &
                  bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,  &
                  sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,  &
                  sqp5ecos,sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,  &
                  sqp7esin,sqp8ecos,sqp8esin,angsum1,angdiff1,angsum2,  &
                  angdiff2,angsum3,angdiff3,angsum4,angdiff4,angsum5,  &
                  angdiff5,angsum6,angdiff6,angsum7,angdiff7,angsum8,  &
                  angdiff8,fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,  &
                  p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,  &
                  t3off,t4off,t5off,t6off,t7off,t8off,tesscontam)
!
               DO j=1,nterms
                  vx1=vstart(j)
                  vx2=vstep(j)
                  vxmult=(vx2-vx1)
                  vxadd=vx1
                  varx=var(j)
                  urand=(varx-vxadd)/vxmult
                  IF(urand > 1.0_dp)urand=1.0_dp
                  IF(urand < 0.0_dp)urand=0.0_dp
                  parmarray(j,np)=varx
                  corr(j)=varx-aveparm(j)
               END DO
               IF(ig+istart >= istartcorr)uncorr=matmul(corr,invcovchol)
               IF(ig+istart >= istartcorr)THEN
                  DO j=1,nterms
                     ucparmarray(j,np)=uncorr(j)
                  END DO
               END IF
            END IF
         END IF
!
!$omp end critical
!
         chilimb=0.0_dp
         IF(ilimbcheck > 1)THEN
            IF(ilaw == 4)CALL getchilimb(nvmax,nterms,svar,dwavex,dwavey,chilimb)
            IF(ilaw == 14)CALL getchilimb(nvmax,nterms,svar,dwavex,dwavey,chilimb)
         END IF
!
         ibest=MOD(ig,iskip+1)
         ichilabel=1
         CALL monster(nphase,nmaxphase,xmod,ymodu,ymodb,ymodv,  &
            ymodr,ymodi,ymodj,ymodh,ymodk,ymods1,ymods2,ymods3,ymodd,  &
            rv1,rv2,drv1,drv2,obsparm,ifastflag,nrvphase,xrvmod,  &
            fracs1,fracs2,fracs3,fracs4,fracs5,fracs6,fracs7,fracs8,  &
            chisqu,chisqb,chisqv,chisqr,chisqi,chisqj,chisqh,chisqk,  &
            chisqrv1,chisqrv2,chilimb,chi1,ndatau,xdatau,ydatau,erru,  &
            zerou,resu,ndatab,xdatab,ydatab,errb,zerob,resb,ndatav,  &
            xdatav,ydatav,errv,zerov,resv,ndatar,xdatar,ydatar,errr,  &
            zeror,resr,ndatai,xdatai,ydatai,erri,zeroi,resi,ndataj,  &
            xdataj,ydataj,errj,zeroj,resj,ndatah,xdatah,ydatah,errh,  &
            zeroh,resh,ndatak,xdatak,ydatak,errk,zerok,resk,nrv1,  &
            xrv1,yrv1,errrv1,nrv2,xrv2,yrv2,errrv2,ggamma1,ggamma2,  &
            nobv,sobv,obv,eobv,ochi,ochidisk,ochilr,nvmax,svar,var,  &
            saveasini,savxdatau,savydatau,saverru,savxdatab,  &
            savydatab,saverrb,savxdatav,savydatav,saverrv,savxdatar,  &
            savydatar,saverrr,savxdatai,savydatai,saverri,savxdataj,  &
            savydataj,saverrj,savxdatah,savydatah,saverrh,savxdatak,  &
            savydatak,saverrk,savxrv1,savyrv1,saverrrv1,savxrv2,  &
            savyrv2,saverrrv2,ifrac,ilum,i16,isavnu,isavnb,isavnv,  &
            isavnr,isavni,isavnj,isavnh,isavnk,isavrv1,isavrv2,  &
            isvel1,isvel2,ndatamax,ibest,ifixgamma,savesep,ichilabel,  &
            resrv1,resrv2,thresh,small,ncycle,ttimes,tseps,nobscycle,  &
            obsttimes,obsterr,icnarray,rv3,xrv3,yrv3,errrv3,icnrv3,  &
            resrv3,ggamma3,nrv3,parmstring,planetparm,dynparm,line,  &
            fill1,fill2,omega1,omega2,dphase,q,finc,teff1,teff2,  &
            tgrav1,tgrav2,betarim,rinner,router,tdisk,xi,alb1,alb2,  &
            rlx,period,fm,separ,gamma,wave,dbolx,dboly,dwavex,dwavey,  &
            t3,g3,sa3,density,sw1,sw2,sw3,t0,ecc,argper,pshift,sw5,  &
            sw6,sw7,sw8,sw9,primmass,primk,primrad,ratrad,frac1,  &
            frac2,ecosw,temprat,bigi,bigbeta,sw23,sw24,  &
            sw25,sw26,sw27,sw28,sw29,sw30,contam,tconj,beam1,beam2,  &
            ocose,osine,omegadot,contams0,contams1,contams2,contams3,  &
            sw47,sw48,sw49,gaplow,gaphigh,p2tconj,p2period,p2t0,  &
            p2ecos,p2esin,p2incl,p2omega,p2q,p2ratrad,p3tconj,  &
            p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,  &
            p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,  &
            p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl,  &
            p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,p6esin,  &
            p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,p7ecos,  &
            p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,  &
            p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,xsc,ysc,  &
            spot1parm,spot2parm,spotdparm,nalph1,nbet1,nalph2,nbet2,  &
            ntheta,nradius,nref,idraw,iecheck,iidint,iatm,ism1,ilaw,  &
            icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk,icnrv1,icnrv2,  &
            irvfilt,isw1,isw2,isw3,isw4,ikeep,isynch,isw5,isw6,isw7,  &
            isw8,isw9,idark1,idark2,isw12,isw13,isw21,isw22,isw23,  &
            isw24,isw25,isw26,isw27,isw28,isw29,isw30,isw31,isw32,  &
            isw33,isw34,nsc,compfracs,tertperiod,tertt0,tertecos,  &
            tertesin,tertincl,tertomega,tertq,tertconj,tertratrad,hh,  &
            sw72,sw73,nmaxeclipse,tdur1,tdur2,sw80,sw81,sw82,sw83,  &
            sw84,sw85,isw80,isw81,isw82,isw83,  &
            isw84,isw85,isw86,isw87,isw88,isw89,ymods4,ymods5,  &
            arrsobx,arrsoby,montemax,nrv4,xrv4,yrv4,rv4,icnrv4,  &
            resrv4,ggamma4,errrv4,sdarkint1,sdarkint2,sdarkint3,  &
            sdarkint4,sdarkint5,atmt,atmg,atmmu,atmint1,atmint2,  &
            atmint3,atmint4,atmint5,atmint6,atmint7,atmint8,  &
            tmin,nlines,nmu,nalph3,nbet3,itconj,it1,it2,  &
            it3,it4,ndynwin,dynwinlow,dynwinhigh,p1mtc,p1ptc,p2mtc,  &
            p2ptc,p3mtc,p3ptc,p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,  &
            p7mtc,p7ptc,p8mtc,p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,  &
            bigbeta2,bigbeta3,bigbeta4,bin2q,bin2massdiff,  &
            bin2masssum,bin2raddiff,bin2radsum,bin2ratrad,g10,g6,g7,  &
            g8,g9,massdiff,masssum,omega3,omega4,omega5,omega6,  &
            omega7,omega8,omega9,omega10,raddiff,radsum,rk3,rk4,rk5,  &
            rk6,rk7,rk8,rk9,rk10,sdarkint6,sdarkint7,sdarkint8,  &
            sdarkint9,sdarkint10,secmass,secrad,t10,t6,t7,t8,t9,nseg,  &
            isw100,iunit,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,  &
            bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,  &
            sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,  &
            sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,  &
            sqp8esin,angsum1,angdiff1,angsum2,angdiff2,angsum3,  &
            angdiff3,angsum4,angdiff4,angsum5,angdiff5,angsum6,  &
            angdiff6,angsum7,angdiff7,angsum8,angdiff8,imag,  &
            newparmstring,ioutflag,fillsum,filldiff,binqtc,p1qtc,  &
            p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,tbinoff,t1off,  &
            t2off,t3off,t4off,t5off,t6off,t7off,t8off,fracstring,  &
            resttimes,drv3,drv4,drv5,rv5,nset,nrv1array,icnrv1array,  &
            xrv1array,yrv1array,errrv1array,nrv2array,icnrv2array,  &
            xrv2array,yrv2array,errrv2array,nrv3array,icnrv3array,  &
            xrv3array,yrv3array,errrv3array,nrv4array,icnrv4array,  &
            xrv4array,yrv4array,errrv4array,icnrv5array,savxrv1array,  &
            savyrv1array,saverrrv1array,savxrv2array,savyrv2array,  &
            saverrrv2array,resrv1array,resrv2array,  &
            resrv3array,resrv4array,resrv5array,gamma1array,  &
            gamma2array,gamma3array,gamma4array,gamma5array,0,  &
            iversion,tesscontam,tessfilt,tessbin,maxlines,maxmu)
!
!$omp critical
!
         deltachi=chi1-oldchiarr(i16)
         deltachi=deltachi/temperature
         IF(deltachi < 0.0_dp)THEN
!
!  take jump and swap
!
            DO j=1,nterms
               parmarray(j,i16)=var(j)
               IF(ig+istart >= istartcorr)corr(j)=var(j)-aveparm(j)
            END DO
            IF(ig+istart >= istartcorr)THEN
               uncorr=matmul(corr,invcovchol)
               DO j=1,nterms
                  ucparmarray(j,i16)=uncorr(j)
               END DO
            END IF
            chiarr(i16)=chi1
            CALL chistring('chi_start',oldchiarr(i16),outstring,lll1)
            CALL chistring(' chi_end',chi1,outstring1,lll2)
            IF(isw22 == 0)WRITE(*,280)TRIM(outstring),TRIM(outstring1)
            CALL pstring('Delta_chi',5,deltachi,outstring,lll)
            IF(isw22 == 0)WRITE(*,270)TRIM(outstring)
            njump=njump+1
            ichange(i16)=0
            bigstring(i16,1)=TRIM(line)
            bigstring(i16,2)=TRIM(newparmstring)
            bigstring(i16,3)=TRIM(planetparm)
            IF((isw30 >= 3).AND.(it4 == 0))bigstring(i16,4)=TRIM(dynparm)
            IF(isw24 >= 1)bigstring(i16,5)=TRIM(fracstring)
         END IF
!
         IF(deltachi >= 0.0_dp)THEN
            prob=EXP(-ABS(deltachi)*0.50_dp)
            uran=probarr(i16)
            IF(uran < prob)THEN
               CALL chistring('chi_start',oldchiarr(i16),outstring,lll1)
               CALL chistring(' chi_end',chi1,outstring1,lll2)
               IF(isw22 == 0)WRITE(*,280)TRIM(outstring),TRIM(outstring1)
               CALL pstring('Delta_chi',5,deltachi,outstring,lll)
               IF(isw22 == 0)WRITE(*,250)TRIM(outstring),uran,prob
               njump=njump+1
               ichange(i16)=0
!
               DO j=1,nterms
                  parmarray(j,i16)=var(j)
                  IF(ig+istart >= istartcorr)corr(j)=var(j)-aveparm(j)
               END DO
               IF(ig+istart >= istartcorr)THEN
                  uncorr=matmul(corr,invcovchol)
                  DO j=1,nterms
                     ucparmarray(j,i16)=uncorr(j)
                  END DO
               END IF
               chiarr(i16)=chi1
               bigstring(i16,1)=TRIM(line)
               bigstring(i16,2)=TRIM(newparmstring)
               bigstring(i16,3)=TRIM(planetparm)
               IF((isw30 >= 3).AND.(it4 == 0))bigstring(i16,4)=TRIM(dynparm)
               IF(isw24 >= 1)bigstring(i16,5)=TRIM(fracstring)
            ELSE
               ichange(i16)=ichange(i16)+1
               CALL chistring('chi_start',oldchiarr(i16),outstring,lll1)
               CALL chistring(' chi_end',chi1,outstring1,lll2)
               IF(isw22 == 0)WRITE(*,280)TRIM(outstring),TRIM(outstring1)
               CALL pstring('Delta_chi',5,deltachi,outstring,lll)
               IF(isw22 == 0)WRITE(*,260)TRIM(outstring),uran,prob
            END IF
         END IF
!
         IF(chi1 < small)THEN
            small=chi1
            iupdate=999
            DO mmm=1,nvmax
               sss(mmm)=var(mmm)
            END DO
         END IF
!
!$omp end critical
!
         IF(isw22 == 0)THEN
            IF(rmed >= 1.0_dp)THEN
               CALL printsmallmed(small)
            ELSE
               CALL printsmall(small)
            END IF
            CALL printiter(i16,'iteration number = ',ig+istart,  &
               'generation number = ')
         END IF
!
!  continue loop over chain
!
      END DO
!
!$omp enddo
!$omp end parallel
!
      WRITE(*,*)' '
      CALL istring('number of jumps',njump,outstring,lll)
      CALL istring('Npop',np,outstring1,lll1)
      WRITE(*,290)TRIM(outstring),TRIM(outstring1)
      fracjump=REAL(njump,KIND=dp)/REAL(np,KIND=dp)
      fracjumparr(ig)=fracjump
      IF((ig > 20+itemp).AND.(MOD(ig,20) == 1))THEN
         CALL gscaleadjust(ig,scalegam,fracjumparr,avejump,target,ifracjump)
      END IF
      IF((ig >= ipurgestart-1).AND.(MOD(ig,ipurgestart)  &
         .EQ.1))scalegam=1.4_dp*scalegam
      CALL pstring('jump fraction',8,fracjump,outstring,lll)
      WRITE(*,240)TRIM(outstring)
      CALL pstring('new gamma',8,scalegam,outstring,lll)
      WRITE(*,240)TRIM(outstring)
      CALL pstring('temperature',6,temperature,outstring,lll)
      WRITE(*,240)TRIM(outstring)
!
      OPEN(UNIT=38,FILE='demcmcELC.out',POSITION='append',STATUS ='old')
      WRITE(38,345)ig+istart,fracjump,scalegam,temperature,seed5,seed6,seed7, &
                 seed8,seed9
      CLOSE(38)
!
      IF(isw22 >= 1)THEN
         IF(rmed >= 1.0_dp)THEN
            CALL printsmallmed(small)
         ELSE
            CALL printsmall(small)
         END IF
      END IF
      CALL istring16('random count',idum,outstring1,lll1)
      WRITE(*,240)outstring1(1:lll1)
      CALL istring('generation number',ig+istart,outstring1,lll1)
      WRITE(*,240)TRIM(outstring1)
!
!  We want to record the parameters here.
!
      IF(MOD(ig,iskip+1) == 0)THEN
!
         IF(ig+istart >= istartcorr+1)THEN
            OPEN(UNIT=166,FILE='demcmc_uncorr.'//extension,STATUS='unknown')
         END IF
         OPEN(UNIT=66,FILE='demcmc_fitparm.'//extension,STATUS='unknown')
         OPEN(UNIT=67,FILE='demcmc_starparm.'//extension,STATUS='unknown')
         OPEN(UNIT=70,FILE='demcmc_chi.'//extension,STATUS='unknown')
         IF(isw24 >= 1)OPEN(UNIT=71,FILE='demcmc_ratio.'//  &
            extension,STATUS='unknown')
!
         IF(isw30 >= 3)THEN
            IF(it4 == 0)THEN
               OPEN(UNIT=69,FILE='demcmc_dynparm.'//extension,STATUS='unknown')
            END IF
         END IF
!
         worst=-12345.6_dp
         DO jj=1,np
            IF(ig+istart >= istartcorr+1)THEN
               WRITE(166,666)(ucparmarray(j,jj),j=1,nvar)
            END IF
            lll=LEN_TRIM(bigstring(jj,1))
            WRITE(66,330)bigstring(jj,1)(26:lll)
            WRITE(67,330)TRIM(bigstring(jj,2))
            IF(isw24 >= 1)WRITE(71,330)TRIM(bigstring(jj,5))
            tempchi=chiarr(jj)
            IF(tempchi > worst)THEN
               worst=tempchi
               iworst=jj
            END IF
            IF(tempchi > 1.0E+13_dp)tempchi=999999999999.9999_dp
            WRITE(70,340)tempchi
            IF(isw30 >= 3)THEN
               tempchi=chiarr(jj)
               IF(it4 == 0)WRITE(69,330)TRIM(bigstring(jj,4))
            END IF
         END DO
!
!  END IF mod(ig,iskip+1) = 0
!
      END IF
!
!   Reset the variables at their best values and print
!   the chi^2 if needed.
!
      CALL istring('iupdate',iupdate,outstring1,lll1)
      IF(isw22 == 0)WRITE(*,240)outstring1(1:lll1)
      DO mmm=1,nvar
         DO nnn=1,np
            oldparmarray(mmm,nnn)=parmarray(mmm,nnn)
            olducparmarray(mmm,nnn)=ucparmarray(mmm,nnn)
         END DO
      END DO
!
!   copy the current chi^2 array into old one
!
      DO jkl=1,np
         oldchiarr(jkl)=chiarr(jkl)
      END DO
!
!   get rid of the worst model
!
      IF((ig >= ipurgestart).AND.(MOD(ig,ipurgeint) == 0))THEN
         IF(chiarr(iworst) > purgefactor+small)THEN
            DO j=1,nterms
               oldparmarray(j,iworst)=sss(j)
               parmarray(j,iworst)=sss(j)
               IF(ig+istart >= istartcorr)corr(j)=sss(j)-aveparm(j)
            END DO
            IF(ig+istart >= istartcorr)THEN
               uncorr=matmul(corr,invcovchol)
               DO j=1,nterms
                  ucparmarray(j,iworst)=uncorr(j)
                  olducparmarray(j,iworst)=uncorr(j)
               END DO
            END IF
            oldchiarr(iworst)=small
            chiarr(iworst)=small
         END IF
      END IF
!
      iiii=ipurgeint/2
      IF((ig >= ipurgestart).AND.(MOD(ig,ipurgeint) == iiii))THEN
         DO kk=1,np
            IF(ichange(kk) > istuck)THEN
               DO j=1,nterms
                  oldparmarray(j,kk)=sss(j)
                  parmarray(j,kk)=sss(j)
                  IF(ig+istart >= istartcorr)corr(j)=sss(j)-aveparm(j)
               END DO
               IF(ig+istart >= istartcorr)THEN
                  uncorr=matmul(corr,invcovchol)
                  DO j=1,nterms
                     ucparmarray(j,kk)=uncorr(j)
                     olducparmarray(j,kk)=uncorr(j)
                  END DO
               END IF
               oldchiarr(kk)=small
               chiarr(kk)=small
               ichange(kk)=0
               GO TO 370
            END IF
         END DO
      END IF
!
370   DO kk=1,np
         WRITE(37,*)ig+istart,ichange(kk),kk
      END DO
!
      IF(iupdate /= 0)THEN
         DO mmm=1,nvmax
            var(mmm)=sss(mmm)
         END DO
!
         chilimb=0.0_dp
         IF(ilimbcheck > 1)THEN
            IF(ilaw == 4)CALL getchilimb(nvmax,nterms,svar,dwavex,dwavey,chilimb)
            IF(ilaw == 14)CALL getchilimb(nvmax,nterms,svar,dwavex,dwavey,chilimb)
         END IF
!
         ibest=99
         ichilabel=0
         ioutflag=9
         CALL monster(nphase,nmaxphase,xmod,ymodu,ymodb,ymodv,  &
            ymodr,ymodi,ymodj,ymodh,ymodk,ymods1,ymods2,ymods3,ymodd,  &
            rv1,rv2,drv1,drv2,obsparm,ifastflag,nrvphase,xrvmod,  &
            fracs1,fracs2,fracs3,fracs4,fracs5,fracs6,fracs7,fracs8,  &
            chisqu,chisqb,chisqv,chisqr,chisqi,chisqj,chisqh,chisqk,  &
            chisqrv1,chisqrv2,chilimb,chi1,ndatau,xdatau,ydatau,erru,  &
            zerou,resu,ndatab,xdatab,ydatab,errb,zerob,resb,ndatav,  &
            xdatav,ydatav,errv,zerov,resv,ndatar,xdatar,ydatar,errr,  &
            zeror,resr,ndatai,xdatai,ydatai,erri,zeroi,resi,ndataj,  &
            xdataj,ydataj,errj,zeroj,resj,ndatah,xdatah,ydatah,errh,  &
            zeroh,resh,ndatak,xdatak,ydatak,errk,zerok,resk,nrv1,  &
            xrv1,yrv1,errrv1,nrv2,xrv2,yrv2,errrv2,ggamma1,ggamma2,  &
            nobv,sobv,obv,eobv,ochi,ochidisk,ochilr,nvmax,svar,var,  &
            saveasini,savxdatau,savydatau,saverru,savxdatab,  &
            savydatab,saverrb,savxdatav,savydatav,saverrv,savxdatar,  &
            savydatar,saverrr,savxdatai,savydatai,saverri,savxdataj,  &
            savydataj,saverrj,savxdatah,savydatah,saverrh,savxdatak,  &
            savydatak,saverrk,savxrv1,savyrv1,saverrrv1,savxrv2,  &
            savyrv2,saverrrv2,ifrac,ilum,i16,isavnu,isavnb,isavnv,  &
            isavnr,isavni,isavnj,isavnh,isavnk,isavrv1,isavrv2,  &
            isvel1,isvel2,ndatamax,ibest,ifixgamma,savesep,ichilabel,  &
            resrv1,resrv2,thresh,small,ncycle,ttimes,tseps,nobscycle,  &
            obsttimes,obsterr,icnarray,rv3,xrv3,yrv3,errrv3,icnrv3,  &
            resrv3,ggamma3,nrv3,parmstring,planetparm,dynparm,line,  &
            fill1,fill2,omega1,omega2,dphase,q,finc,teff1,teff2,  &
            tgrav1,tgrav2,betarim,rinner,router,tdisk,xi,alb1,alb2,  &
            rlx,period,fm,separ,gamma,wave,dbolx,dboly,dwavex,dwavey,  &
            t3,g3,sa3,density,sw1,sw2,sw3,t0,ecc,argper,pshift,sw5,  &
            sw6,sw7,sw8,sw9,primmass,primk,primrad,ratrad,frac1,  &
            frac2,ecosw,temprat,bigi,bigbeta,sw23,sw24,  &
            sw25,sw26,sw27,sw28,sw29,sw30,contam,tconj,beam1,beam2,  &
            ocose,osine,omegadot,contams0,contams1,contams2,contams3,  &
            sw47,sw48,sw49,gaplow,gaphigh,p2tconj,p2period,p2t0,  &
            p2ecos,p2esin,p2incl,p2omega,p2q,p2ratrad,p3tconj,  &
            p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,  &
            p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,  &
            p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl,  &
            p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,p6esin,  &
            p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,p7ecos,  &
            p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,  &
            p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,xsc,ysc,  &
            spot1parm,spot2parm,spotdparm,nalph1,nbet1,nalph2,nbet2,  &
            ntheta,nradius,nref,idraw,iecheck,iidint,iatm,ism1,ilaw,  &
            icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk,icnrv1,icnrv2,  &
            irvfilt,isw1,isw2,isw3,isw4,ikeep,isynch,isw5,isw6,isw7,  &
            isw8,isw9,idark1,idark2,isw12,isw13,isw21,isw22,isw23,  &
            isw24,isw25,isw26,isw27,isw28,isw29,isw30,isw31,isw32,  &
            isw33,isw34,nsc,compfracs,tertperiod,tertt0,tertecos,  &
            tertesin,tertincl,tertomega,tertq,tertconj,tertratrad,hh,  &
            sw72,sw73,nmaxeclipse,tdur1,tdur2,sw80,sw81,sw82,sw83,  &
            sw84,sw85,isw80,isw81,isw82,isw83,  &
            isw84,isw85,isw86,isw87,isw88,isw89,ymods4,ymods5,  &
            arrsobx,arrsoby,montemax,nrv4,xrv4,yrv4,rv4,icnrv4,  &
            resrv4,ggamma4,errrv4,sdarkint1,sdarkint2,sdarkint3,  &
            sdarkint4,sdarkint5,atmt,atmg,atmmu,atmint1,atmint2,  &
            atmint3,atmint4,atmint5,atmint6,atmint7,atmint8,  &
            tmin,nlines,nmu,nalph3,nbet3,itconj,it1,it2,  &
            it3,it4,ndynwin,dynwinlow,dynwinhigh,p1mtc,p1ptc,p2mtc,  &
            p2ptc,p3mtc,p3ptc,p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,  &
            p7mtc,p7ptc,p8mtc,p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,  &
            bigbeta2,bigbeta3,bigbeta4,bin2q,bin2massdiff,  &
            bin2masssum,bin2raddiff,bin2radsum,bin2ratrad,g10,g6,g7,  &
            g8,g9,massdiff,masssum,omega3,omega4,omega5,omega6,  &
            omega7,omega8,omega9,omega10,raddiff,radsum,rk3,rk4,rk5,  &
            rk6,rk7,rk8,rk9,rk10,sdarkint6,sdarkint7,sdarkint8,  &
            sdarkint9,sdarkint10,secmass,secrad,t10,t6,t7,t8,t9,nseg,  &
            isw100,iunit,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,  &
            bin2r4,sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,  &
            sqp2esin,sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,  &
            sqp5esin,sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,  &
            sqp8esin,angsum1,angdiff1,angsum2,angdiff2,angsum3,  &
            angdiff3,angsum4,angdiff4,angsum5,angdiff5,angsum6,  &
            angdiff6,angsum7,angdiff7,angsum8,angdiff8,imag,  &
            newparmstring,ioutflag,fillsum,filldiff,binqtc,p1qtc,  &
            p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,tbinoff,t1off,  &
            t2off,t3off,t4off,t5off,t6off,t7off,t8off,fracstring,  &
            resttimes,drv3,drv4,drv5,rv5,nset,nrv1array,icnrv1array,  &
            xrv1array,yrv1array,errrv1array,nrv2array,icnrv2array,  &
            xrv2array,yrv2array,errrv2array,nrv3array,icnrv3array,  &
            xrv3array,yrv3array,errrv3array,nrv4array,icnrv4array,  &
            xrv4array,yrv4array,errrv4array,icnrv5array,savxrv1array,  &
            savyrv1array,saverrrv1array,savxrv2array,savyrv2array,  &
            saverrrv2array,resrv1array,resrv2array,  &
            resrv3array,resrv4array,resrv5array,gamma1array,  &
            gamma2array,gamma3array,gamma4array,gamma5array,1,  &
            iversion,tesscontam,tessfilt,tessbin,maxlines,maxmu)
!
         ioutflag=0
         chiall=(chisqu+chisqb+chisqv+chisqr+chisqi+chisqj+chisqh+  &
            chisqk+chisqrv1+chisqrv2+ochi+chilimb)
         WRITE(*,*)' '
         chiall=chi1
         CALL prints(chiall)
!
         IF((isw30 >= 3).AND.(isw23 >= 1))THEN
            CALL writeeclipse(ncycle,ttimes,tseps,isw30,nmaxeclipse,tdur1,tdur2,0)
         END IF
!
         CALL writemodels(nphase,isw7,xmod,ymodu,ymodb,ymodv,  &
            ymodr,ymodi,ymodj,ymodh,ymodk,ymods1,ymods2,ymods3,  &
            ymods4,ymods5,t3,g3,sa3,iidint,ymodd,nrvphase,xrvmod,rv1,  &
            rv2,rv3,rv4,isw30,ggamma1,ggamma2,ggamma3,itime,ndatamax,  &
            ndatau,ndatab,ndatav,ndatar,ndatai,ndataj,ndatah,ndatak,  &
            nrv1,nrv2,nrv3,nrv4,xdatau,ydatau,erru,xdatab,ydatab,  &
            errb,xdatav,ydatav,errv,xdatar,ydatar,errr,xdatai,ydatai,  &
            erri,xdataj,ydataj,errj,xdatah,ydatah,errh,xdatak,ydatak,  &
            errk,xrv1,yrv1,errrv1,xrv2,yrv2,errrv2,xrv3,yrv3,errrv3,  &
            xrv4,yrv4,errrv4,resu,resb,resv,resr,resi,resj,resh,resk,  &
            icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk,icnrv1,icnrv2,  &
            icnrv3,icnrv4,zerou,zerob,zerov,zeror,zeroi,zeroj,zeroh,  &
            zerok,resrv1,resrv2,resrv3,resrv4,drv1,drv2,imag,  &
            resttimes,icnarray,nobscycle,obsterr,nmaxeclipse,  &
            obsttimes,drv3,drv4,drv5,rv5,resrv1array,resrv2array,  &
            resrv3array,resrv4array,resrv5array,gamma1array,  &
            gamma2array,gamma3array,gamma4array,gamma5array,  &
            ifixgamma,nset,nrv1array,icnrv1array,xrv1array,  &
            yrv1array,errrv1array,nrv2array,icnrv2array,xrv2array,  &
            yrv2array,errrv2array,nrv3array,icnrv3array,xrv3array,  &
            yrv3array,errrv3array,nrv4array,icnrv4array,xrv4array,  &
            yrv4array,errrv4array,nrv5array,icnrv5array,xrv5array,  &
            yrv5array,errrv5array)
!
         igrid=ig+1000000+istart
         CALL newwritegridout(nalph1,nalph2,nalph3,nbet1,nbet2,  &
            nbet3,nradius,nref,nseg,ntheta,irvfilt,iatm,icnb,icnh,  &
            icni,icnj,icnk,icnr,icnu,icnv,idark1,idark2,idraw,  &
            iecheck,iidint,ikeep,ilaw,ism1,isw1,isw13,isw2,  &
            isw21,isw22,isw23,isw24,isw25,isw26,isw27,isw28,isw29,  &
            isw3,isw30,isw31,isw32,isw33,isw34,isw4,isw5,isw6,isw7,  &
            isw8,isw80,isw81,isw85,isw86,isw87,  &
            isw88,isw89,isw9,isw100,isynch,it1,it2,it3,it4,itconj,  &
            p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,p2esin,p2incl,p2mtc,  &
            p2ptc,p2period,p2ratrad,p2tconj,p3omega,p3q,p3t0,p3ecos,  &
            p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,p3tconj,  &
            p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,p4ptc,  &
            p4period,p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,p5esin,  &
            p5incl,p5mtc,p5ptc,p5period,p5ratrad,p5tconj,p6omega,p6q,  &
            p6t0,p6ecos,p6esin,p6incl,p6mtc,p6ptc,p6period,p6ratrad,  &
            p6tconj,p7omega,p7q,p7t0,p7ecos,p7esin,p7incl,p7mtc,  &
            p7ptc,p7period,p7ratrad,p7tconj,p8omega,p8q,p8t0,p8ecos,  &
            p8esin,p8incl,p8mtc,p8ptc,p8period,p8ratrad,p8tconj,  &
            pbmtc,pbptc,period,q,sa3,t0,tconj,teff1,teff2,tgrav1,  &
            tgrav2,alb1,alb2,argper,beam1,beam2,betarim,bigi,bigi2,  &
            bigi3,bigi4,bigbeta,bigbeta2,bigbeta3,bigbeta4,bin2q,  &
            bin2massdiff,bin2masssum,bin2raddiff,bin2radsum,  &
            bin2ratrad,contam,contams0,contams1,contams2,contams3,  &
            dbolx,dboly,dphase,dwavex,dwavey,ecc,ecosw,fill1,  &
            fill2,finc,frac1,frac2,g10,g3,g6,g7,g8,g9,gamma,hh,  &
            massdiff,masssum,ocose,omega1,omega2,omega3,omega4,  &
            omega5,omega6,omega7,omega8,omega9,omega10,  &
            osine,primk,primmass,primrad,pshift,rlx,  &
            raddiff,radsum,ratrad,rinner,rk3,rk4,rk5,rk6,rk7,rk8,rk9,  &
            rk10,router,sdarkint1,sdarkint2,sdarkint3,sdarkint4,  &
            sdarkint5,sdarkint6,sdarkint7,sdarkint8,sdarkint9,  &
            sdarkint10,secmass,secrad,separ,spot1parm,spot2parm,  &
            spotdparm,sw1,sw2,sw23,sw24,sw25,sw26,sw27,sw28,sw29,sw3,  &
            sw30,sw47,sw48,sw49,sw5,sw6,sw7,sw72,sw73,sw8,sw80,sw81,  &
            sw82,sw83,sw84,sw9,t10,t3,t6,t7,  &
            t8,t9,tdisk,temprat,tertomega,tertq,tertconj,tertecos,  &
            tertesin,tertincl,tertperiod,tertratrad,tertt0,wave,xi,  &
            iunit,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,bin2r4,  &
            sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin,  &
            sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin,  &
            sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin,  &
            angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3,  &
            angsum4,angdiff4,angsum5,angdiff5,angsum6,angdiff6,  &
            angsum7,angdiff7,angsum8,angdiff8,imag,fillsum,filldiff,  &
            binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,  &
            tbinoff,t1off,t2off,t3off,t4off,t5off,t6off,t7off,t8off,  &
            igrid,iversion,tesscontam,tessfilt,tessbin)
!
         igrid=-99
         CALL newwritegridout(nalph1,nalph2,nalph3,nbet1,nbet2,  &
            nbet3,nradius,nref,nseg,ntheta,irvfilt,iatm,icnb,icnh,  &
            icni,icnj,icnk,icnr,icnu,icnv,idark1,idark2,idraw,  &
            iecheck,iidint,ikeep,ilaw,ism1,isw1,isw13,isw2,  &
            isw21,isw22,isw23,isw24,isw25,isw26,isw27,isw28,isw29,  &
            isw3,isw30,isw31,isw32,isw33,isw34,isw4,isw5,isw6,isw7,  &
            isw8,isw80,isw81,isw85,isw86,isw87,  &
            isw88,isw89,isw9,isw100,isynch,it1,it2,it3,it4,itconj,  &
            p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,p2esin,p2incl,p2mtc,  &
            p2ptc,p2period,p2ratrad,p2tconj,p3omega,p3q,p3t0,p3ecos,  &
            p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,p3tconj,  &
            p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,p4ptc,  &
            p4period,p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,p5esin,  &
            p5incl,p5mtc,p5ptc,p5period,p5ratrad,p5tconj,p6omega,p6q,  &
            p6t0,p6ecos,p6esin,p6incl,p6mtc,p6ptc,p6period,p6ratrad,  &
            p6tconj,p7omega,p7q,p7t0,p7ecos,p7esin,p7incl,p7mtc,  &
            p7ptc,p7period,p7ratrad,p7tconj,p8omega,p8q,p8t0,p8ecos,  &
            p8esin,p8incl,p8mtc,p8ptc,p8period,p8ratrad,p8tconj,  &
            pbmtc,pbptc,period,q,sa3,t0,tconj,teff1,teff2,tgrav1,  &
            tgrav2,alb1,alb2,argper,beam1,beam2,betarim,bigi,bigi2,  &
            bigi3,bigi4,bigbeta,bigbeta2,bigbeta3,bigbeta4,bin2q,  &
            bin2massdiff,bin2masssum,bin2raddiff,bin2radsum,  &
            bin2ratrad,contam,contams0,contams1,contams2,contams3,  &
            dbolx,dboly,dphase,dwavex,dwavey,ecc,ecosw,fill1,  &
            fill2,finc,frac1,frac2,g10,g3,g6,g7,g8,g9,gamma,hh,  &
            massdiff,masssum,ocose,omega1,omega2,omega3,omega4,  &
            omega5,omega6,omega7,omega8,omega9,omega10,  &
            osine,primk,primmass,primrad,pshift,rlx,  &
            raddiff,radsum,ratrad,rinner,rk3,rk4,rk5,rk6,rk7,rk8,rk9,  &
            rk10,router,sdarkint1,sdarkint2,sdarkint3,sdarkint4,  &
            sdarkint5,sdarkint6,sdarkint7,sdarkint8,sdarkint9,  &
            sdarkint10,secmass,secrad,separ,spot1parm,spot2parm,  &
            spotdparm,sw1,sw2,sw23,sw24,sw25,sw26,sw27,sw28,sw29,sw3,  &
            sw30,sw47,sw48,sw49,sw5,sw6,sw7,sw72,sw73,sw8,sw80,sw81,  &
            sw82,sw83,sw84,sw9,t10,t3,t6,t7,  &
            t8,t9,tdisk,temprat,tertomega,tertq,tertconj,tertecos,  &
            tertesin,tertincl,tertperiod,tertratrad,tertt0,wave,xi,  &
            iunit,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,bin2r4,  &
            sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin,  &
            sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin,  &
            sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin,  &
            angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3,  &
            angsum4,angdiff4,angsum5,angdiff5,angsum6,angdiff6,  &
            angsum7,angdiff7,angsum8,angdiff8,imag,fillsum,filldiff,  &
            binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,  &
            tbinoff,t1off,t2off,t3off,t4off,t5off,t6off,t7off,t8off,  &
            igrid,iversion,tesscontam,tessfilt,tessbin)
!
         igrid=ig+1000000+istart
         CALL recordloopopt(udatafile,bdatafile,vdatafile,  &
            rdatafile,idatafile,jdatafile,hdatafile,kdatafile,  &
            rv1file,rv2file,nvmax,nvar,svar,var,vstart,stepsave,  &
            nstep,nobv,sobv,obv,eobv,vstep,rv3file,rv4file,rv5file, igrid)
!
         iupdate=0
      END IF
!
      CLOSE(45)
      CLOSE(46)
      CLOSE(66)
      IF(ig+istart >= istartcorr+1)CLOSE(166)
      CLOSE(67)
      CLOSE(69)
      CLOSE(70)
      IF(isw24 > 0)CLOSE(71)
      IF(isw24 > 0)CLOSE(47)
      IF((isw30 >= 3).AND.(it4 == 0))CLOSE(49)
      CLOSE(55)
!
!   Write recovery file here, every 5th generation
!
      IF((ig >= 30).and.(mod(ig+istart,5)==0))THEN
         OPEN(UNIT=70,file='demcmc_recover.'//extension,STATUS='unknown')
         WRITE(70,*)nvar
         DO mmm=1,nvar
           WRITE(70,*)"'",svar(mmm)(1:2),"'"
         END DO
         DO mmm=1,nvar
           WRITE(70,*)vstart(mmm),vstep(mmm)
         END DO
         DO mmm=1,np
            WRITE(70,*)"'",TRIM(bigstring(mmm,1)),"'"
            WRITE(70,*)"'",TRIM(bigstring(mmm,2)),"'"
            IF((it4 == 0).and.(isw30 >= 3))WRITE(70,*)"'",TRIM(bigstring(mmm,4)),"'"
            IF(isw24 >= 1)WRITE(70,*)bigstring(mmm,5)
         END DO
         DO mmm=1,np
            WRITE(70,*)mmm,(parmarray(j,mmm),j=1,nterms)
         END DO
         DO mmm=1,np
            WRITE(70,*)mmm,(oldparmarray(j,mmm),j=1,nterms)
         END DO
         DO mmm=1,np
            WRITE(70,*)mmm,(ucparmarray(j,mmm),j=1,nterms)
         END DO
         DO mmm=1,np
            WRITE(70,*)mmm,(olducparmarray(j,mmm),j=1,nterms)
         END DO
         DO mmm=1,np
            WRITE(70,*)mmm,chiarr(mmm),oldchiarr(mmm),ichange(mmm)
         END DO
         DO mmm=1,nterms
            WRITE(70,*)mmm,sss(mmm)
         END DO
         DO mmm=ig-29,ig
            WRITE(70,*)mmm,fracjumparr(mmm)
         END DO
         WRITE(70,*)seed5,seed6,seed7,seed8,seed9
         WRITE(70,*)small,scalegam,siggam,iworst,iskip,idum,iupdate
         WRITE(70,*)ggamma1,ggamma2,ggamma3
         CLOSE(70)
      END IF
!
!  END DO of main generation loop
!
   END DO
!
   CLOSE(37)
   CLOSE(38)
!
   DEALLOCATE(corr,uncorr,aveparm,covchol,invcovchol)
!
!  deallocate the memory for the model arrays
!
   DEALLOCATE(xmod,ymodu,ymodb)
   DEALLOCATE(ymodv,ymodr,ymodi)
   DEALLOCATE(ymodj,ymodh,ymodk)
   DEALLOCATE(ymods1,ymods2,ymods3)
   DEALLOCATE(ymods4,ymods5,ymodd)
   DEALLOCATE(xrvmod,rv1,rv2)
   DEALLOCATE(rv3,rv4,rv5)
   DEALLOCATE(drv1,drv2,drv3)
   DEALLOCATE(drv4,drv5)
   DEALLOCATE(fracs1,fracs2)
   DEALLOCATE(fracs3,fracs4)
   DEALLOCATE(fracs5,fracs6)
   DEALLOCATE(fracs7,fracs8)
!
!  Deallocate the data arrays
!
   DEALLOCATE(xrv1array,xrv2array)
   DEALLOCATE(xrv3array,xrv4array)
   DEALLOCATE(xrv5array,yrv5array)
   DEALLOCATE(yrv1array,yrv2array)
   DEALLOCATE(yrv3array,yrv4array)
   DEALLOCATE(errrv1array,errrv2array)
   DEALLOCATE(errrv3array,errrv4array)
   DEALLOCATE(errrv5array)
   DEALLOCATE(savxrv1array,savxrv2array)
   DEALLOCATE(savxrv3array,savxrv4array)
   DEALLOCATE(savxrv5array)
   DEALLOCATE(savyrv1array,savyrv2array)
   DEALLOCATE(savyrv3array,savyrv4array)
   DEALLOCATE(savyrv5array)
   DEALLOCATE(saverrrv1array,saverrrv2array)
   DEALLOCATE(saverrrv3array,saverrrv4array)
   DEALLOCATE(saverrrv5array)
   DEALLOCATE(xdatau,xdatab,xdatav,xdatar)
   DEALLOCATE(xdatai,xdataj,xdatah,xdatak)
   DEALLOCATE(ydatau,ydatab,ydatav,ydatar)
   DEALLOCATE(ydatai,ydataj,ydatah,ydatak)
   DEALLOCATE(erru,errb,errv,errr)
   DEALLOCATE(erri,errj,errh,errk)
   DEALLOCATE(errrv1,errrv2,errrv3,errrv4)
   DEALLOCATE(xrv1,xrv2,xrv3,xrv4)
   DEALLOCATE(yrv1,yrv2,yrv3,yrv4)
   DEALLOCATE(resrv1,resrv2,resrv3,resrv4)
   DEALLOCATE(resu,resb,resv)
   DEALLOCATE(resr,resi,resj)
   DEALLOCATE(resh,resk)
   DEALLOCATE(savxdatau,savydatau)
   DEALLOCATE(saverru,savydatab)
   DEALLOCATE(saverrb,savydatav)
   DEALLOCATE(saverrv,savydatar)
   DEALLOCATE(saverrr,savydatai)
   DEALLOCATE(saverri,savydataj)
   DEALLOCATE(saverrj,savydatah)
   DEALLOCATE(saverrh,savydatak)
   DEALLOCATE(saverrk,savyrv1)
   DEALLOCATE(saverrrv1,savyrv2)
   DEALLOCATE(saverrrv2,savxdatab)
   DEALLOCATE(savxdatav,savxdatar)
   DEALLOCATE(savxdatai,savxdataj)
   DEALLOCATE(savxdatah,savxdatak)
   DEALLOCATE(savxrv1,savxrv2)
   DEALLOCATE(resrv1array,resrv2array)
   DEALLOCATE(resrv3array,resrv4array)
   DEALLOCATE(resrv5array)
!
!  Deallocate the eclipse arrays
!
   DEALLOCATE(ttimes,obsttimes,tseps,tdur1,obsterr,tdur2,resttimes)
!
!  Deallocate the model atmosphere table arrays
!
   DEALLOCATE(nmu,atmt,atmg)
   DEALLOCATE(atmint1,atmint2)
   DEALLOCATE(atmint3,atmint4)
   DEALLOCATE(atmint5,atmint6)
   DEALLOCATE(atmint7,atmint8)
   DEALLOCATE(atmmu)
!
   DEALLOCATE(fracjumparr)
!
190 FORMAT('2          demcmcELC')
200 FORMAT(a,', ',a,', ',a,', ',a)
210 FORMAT(a,', ',a,', ',a)
220 FORMAT('error:  initial value of variable ',a2,' is out of ra'  &
      ,'nge for initial',' model number ',i4)
230 FORMAT(3(f16.8,3X))
240 FORMAT(a)
250 FORMAT(a,', ran=',f7.5,', probability=',f7.5,',',' jump taken' )
260 FORMAT(a,', ran=',f7.5,', probability=',f7.5,',',' jump not t'  &
      ,'aken')
270 FORMAT(a,', jump taken')
280 FORMAT(a,',',a)
290 FORMAT(a,', ',a)
300 FORMAT(a,', ',a,', ',a)
310 FORMAT(a,', ',a,', ',a)
320 FORMAT(i7)
330 FORMAT(a)
340 FORMAT(f30.14)
345 FORMAT(i6,3X,f9.7,3X,f9.6,2X,f12.5,3X,5(i20,2x))
666 FORMAT(80(f14.11,1X))
!
   STOP
!
END PROGRAM demcmcelc
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
INCLUDE  'lcsubs.f90'
INCLUDE  'cyg_opt.f90'
