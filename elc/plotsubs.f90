SUBROUTINE acfstart(istart,maxlag,iplot)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: istart
   INTEGER, INTENT(OUT)                     :: maxlag
   INTEGER, INTENT(OUT)                     :: iplot
!
   INTEGER :: ios
!
   CHARACTER (LEN=40) :: arg1,arg2,arg3
!
   ios=0
   CALL get_command_argument(1,arg1)
   READ(arg1,*,IOSTAT=ios)istart
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)maxlag
   CALL get_command_argument(3,arg3)
   READ(arg3,*,IOSTAT=ios)iplot
   IF(ios == 0)RETURN
!
END SUBROUTINE acfstart
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE assignbin(np,nbin,arrayx,xlow,xhigh,binx,biny,  &
   yhigh,ibig)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ibig
   INTEGER, INTENT(IN)                      :: np
   INTEGER, INTENT(IN)                      :: nbin
   REAL(KIND=pg), INTENT(IN)                :: arrayx(ibig)
   REAL(KIND=pg), INTENT(IN)                :: xlow
   REAL(KIND=pg), INTENT(IN)                :: xhigh
   REAL(KIND=pg), INTENT(OUT)               :: binx(200)
   REAL(KIND=pg), INTENT(OUT)               :: biny(200)
   REAL(KIND=pg), INTENT(OUT)               :: yhigh
!
   REAL(KIND=pg)  :: qq,t1
!
   INTEGER :: i, j
!
   yhigh=-123456.0_pg
   qq=(xhigh-xlow)/REAL(nbin,KIND=pg)
!
   DO  i=1,nbin
      binx(i)=REAL(i-1,KIND=pg)*qq+xlow+qq/2.0_pg
      biny(i)=0.0_pg
   END DO
!
   loop30:  DO  i=1,np
      DO  j=1,nbin
         t1=ABS(arrayx(i)-binx(j))
         IF(t1 <= qq/2.0_pg)THEN
            biny(j)=biny(j)+1.0_pg
            CYCLE loop30
         END IF
      END DO
   END DO loop30
!
   DO  i=1,nbin
      IF(biny(i) > yhigh)yhigh=biny(i)
   END DO
!
   RETURN
!
END SUBROUTINE assignbin
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE assignvar(nvarmax,svar,var,fill1,fill2,omega1,  &
   omega2,q,finc,teff1,teff2,betarim,rinner,router,tdisk,xi,rlx,  &
   separ,gamma,t3,g3,sa3,ecc,argper,pshift,spot1parm,spot2parm,  &
   spotdparm,period,t0,alb1,alb2,dwavex,dwavey,primmass,primk,  &
   primrad,ratrad,frac1,frac2,ecosw,temprat,bigi,bigbeta,  &
   density,tconj,beam1,beam2,contam,ocose,osine,  &
   isw29,tertperiod,tertt0,tertecos,tertesin,tertincl,tertomega,  &
   tertq,tgrav1,tgrav2,tertconj,omegadot,contams0,contams1,  &
   contams2,contams3,p2tconj,p2period,p2t0,p2ecos,p2esin,p2incl,  &
   p2omega,p2q,p2ratrad,p3tconj,p3period,p3t0,p3ecos,p3esin,  &
   p3incl,p3omega,p3q,p3ratrad,p4tconj,p4period,p4t0,p4ecos,  &
   p4esin,p4incl,p4omega,p4q,p4ratrad,p5tconj,p5period,p5t0,  &
   p5ecos,p5esin,p5incl,p5omega,p5q,p5ratrad,p6tconj,p6period,  &
   p6t0,p6ecos,p6esin,p6incl,p6omega,p6q,p6ratrad,p7tconj,  &
   p7period,p7t0,p7ecos,p7esin,p7incl,p7omega,p7q,p7ratrad,  &
   p8tconj,p8period,p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,  &
   p8ratrad,sw72,sw73,sw49,sw80,sw81,sdarkint1,sdarkint2,  &
   sdarkint3,sdarkint4,sdarkint5,tertratrad,p1mtc,p1ptc,p2mtc,  &
   p2ptc,p3mtc,p3ptc,p4mtc,p4ptc,p5mtc,p5ptc,p6mtc,p6ptc,p7mtc,  &
   p7ptc,p8mtc,p8ptc,pbmtc,pbptc,bigi2,bigi3,bigi4,bigbeta2,  &
   bigbeta3,bigbeta4,bin2q,b2massdiff,b2masssum,b2raddiff,  &
   b2radsum,bin2ratrad,g10,g6,g7,g8,g9,massdiff,masssum,  &
   omega3,omega4,omega5,omega6,omega7,omega8,omega9,omega10,  &
   raddiff,radsum,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,sdarkint6,  &
   sdarkint7,sdarkint8,sdarkint9,sdarkint10,secmass,secrad,t10,  &
   t6,t7,t8,t9,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,bin2r4,  &
   sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin,  &
   sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin,  &
   sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin,  &
   angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3,angsum4,  &
   angdiff4,angsum5,angdiff5,angsum6,angdiff6,angsum7,angdiff7,  &
   angsum8,angdiff8,fillsum,filldiff,binqtc,p1qtc,p2qtc,p3qtc,  &
   p4qtc,p5qtc,p6qtc,p7qtc,p8qtc,tbinoff,t1off,t2off,t3off,  &
   t4off,t5off,t6off,t7off,t8off,iGR,tesscontam)
!
!   November 12, 1999
!
!   This routine will determine which variables need to be changed
!   based on the string codes in svar(1:Nvarmax)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvarmax
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvarmax)
   REAL(KIND=dp), INTENT(IN)                :: var(nvarmax)
   REAL(KIND=dp), INTENT(OUT)               :: fill1
   REAL(KIND=dp), INTENT(IN OUT)            :: fill2
   REAL(KIND=dp), INTENT(OUT)               :: omega1
   REAL(KIND=dp), INTENT(OUT)               :: omega2
   REAL(KIND=dp), INTENT(OUT)               :: q
   REAL(KIND=dp), INTENT(OUT)               :: finc
   REAL(KIND=dp), INTENT(OUT)               :: teff1
   REAL(KIND=dp), INTENT(OUT)               :: teff2
   REAL(KIND=dp), INTENT(OUT)               :: betarim
   REAL(KIND=dp), INTENT(OUT)               :: rinner
   REAL(KIND=dp), INTENT(OUT)               :: router
   REAL(KIND=dp), INTENT(OUT)               :: tdisk
   REAL(KIND=dp), INTENT(OUT)               :: xi
   REAL(KIND=dp), INTENT(OUT)               :: rlx
   REAL(KIND=dp), INTENT(OUT)               :: separ
   REAL(KIND=dp), INTENT(OUT)               :: gamma
   REAL(KIND=dp), INTENT(OUT)               :: t3
   REAL(KIND=dp), INTENT(OUT)               :: g3
   REAL(KIND=dp), INTENT(OUT)               :: sa3
   REAL(KIND=dp), INTENT(OUT)               :: ecc
   REAL(KIND=dp), INTENT(OUT)               :: argper
   REAL(KIND=dp), INTENT(OUT)               :: pshift
   REAL(KIND=dp), INTENT(OUT)               :: spot1parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spot2parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spotdparm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: period
   REAL(KIND=dp), INTENT(OUT)               :: t0
   REAL(KIND=dp), INTENT(OUT)               :: alb1
   REAL(KIND=dp), INTENT(OUT)               :: alb2
   REAL(KIND=dp), INTENT(OUT)               :: dwavex(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: dwavey(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: primmass
   REAL(KIND=dp), INTENT(OUT)               :: primk
   REAL(KIND=dp), INTENT(OUT)               :: primrad
   REAL(KIND=dp), INTENT(OUT)               :: ratrad
   REAL(KIND=dp), INTENT(OUT)               :: frac1
   REAL(KIND=dp), INTENT(OUT)               :: frac2
   REAL(KIND=dp), INTENT(OUT)               :: ecosw
   REAL(KIND=dp), INTENT(OUT)               :: temprat
   REAL(KIND=dp), INTENT(OUT)               :: bigi
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta
   REAL(KIND=dp), INTENT(OUT)               :: density
   REAL(KIND=dp), INTENT(OUT)               :: tconj
   REAL(KIND=dp), INTENT(OUT)               :: beam1
   REAL(KIND=dp), INTENT(OUT)               :: beam2
   REAL(KIND=dp), INTENT(OUT)               :: contam
   REAL(KIND=dp), INTENT(OUT)               :: ocose
   REAL(KIND=dp), INTENT(OUT)               :: osine
   INTEGER, INTENT(IN)                      :: isw29
   REAL(KIND=dp), INTENT(OUT)               :: tertperiod
   REAL(KIND=dp), INTENT(OUT)               :: tertt0
   REAL(KIND=dp), INTENT(OUT)               :: tertecos
   REAL(KIND=dp), INTENT(OUT)               :: tertesin
   REAL(KIND=dp), INTENT(OUT)               :: tertincl
   REAL(KIND=dp), INTENT(OUT)               :: tertomega
   REAL(KIND=dp), INTENT(OUT)               :: tertq
   REAL(KIND=dp), INTENT(OUT)               :: tgrav1
   REAL(KIND=dp), INTENT(OUT)               :: tgrav2
   REAL(KIND=dp), INTENT(OUT)               :: tertconj
   REAL(KIND=dp), INTENT(OUT)               :: omegadot
   REAL(KIND=dp), INTENT(OUT)               :: contams0
   REAL(KIND=dp), INTENT(OUT)               :: contams1
   REAL(KIND=dp), INTENT(OUT)               :: contams2
   REAL(KIND=dp), INTENT(OUT)               :: contams3
   REAL(KIND=dp), INTENT(OUT)               :: p2tconj
   REAL(KIND=dp), INTENT(OUT)               :: p2period
   REAL(KIND=dp), INTENT(OUT)               :: p2t0
   REAL(KIND=dp), INTENT(OUT)               :: p2ecos
   REAL(KIND=dp), INTENT(OUT)               :: p2esin
   REAL(KIND=dp), INTENT(OUT)               :: p2incl
   REAL(KIND=dp), INTENT(OUT)               :: p2omega
   REAL(KIND=dp), INTENT(OUT)               :: p2q
   REAL(KIND=dp), INTENT(OUT)               :: p2ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p3tconj
   REAL(KIND=dp), INTENT(OUT)               :: p3period
   REAL(KIND=dp), INTENT(OUT)               :: p3t0
   REAL(KIND=dp), INTENT(OUT)               :: p3ecos
   REAL(KIND=dp), INTENT(OUT)               :: p3esin
   REAL(KIND=dp), INTENT(OUT)               :: p3incl
   REAL(KIND=dp), INTENT(OUT)               :: p3omega
   REAL(KIND=dp), INTENT(OUT)               :: p3q
   REAL(KIND=dp), INTENT(OUT)               :: p3ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p4tconj
   REAL(KIND=dp), INTENT(OUT)               :: p4period
   REAL(KIND=dp), INTENT(OUT)               :: p4t0
   REAL(KIND=dp), INTENT(OUT)               :: p4ecos
   REAL(KIND=dp), INTENT(OUT)               :: p4esin
   REAL(KIND=dp), INTENT(OUT)               :: p4incl
   REAL(KIND=dp), INTENT(OUT)               :: p4omega
   REAL(KIND=dp), INTENT(OUT)               :: p4q
   REAL(KIND=dp), INTENT(OUT)               :: p4ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p5tconj
   REAL(KIND=dp), INTENT(OUT)               :: p5period
   REAL(KIND=dp), INTENT(OUT)               :: p5t0
   REAL(KIND=dp), INTENT(OUT)               :: p5ecos
   REAL(KIND=dp), INTENT(OUT)               :: p5esin
   REAL(KIND=dp), INTENT(OUT)               :: p5incl
   REAL(KIND=dp), INTENT(OUT)               :: p5omega
   REAL(KIND=dp), INTENT(OUT)               :: p5q
   REAL(KIND=dp), INTENT(OUT)               :: p5ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p6tconj
   REAL(KIND=dp), INTENT(OUT)               :: p6period
   REAL(KIND=dp), INTENT(OUT)               :: p6t0
   REAL(KIND=dp), INTENT(OUT)               :: p6ecos
   REAL(KIND=dp), INTENT(OUT)               :: p6esin
   REAL(KIND=dp), INTENT(OUT)               :: p6incl
   REAL(KIND=dp), INTENT(OUT)               :: p6omega
   REAL(KIND=dp), INTENT(OUT)               :: p6q
   REAL(KIND=dp), INTENT(OUT)               :: p6ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p7tconj
   REAL(KIND=dp), INTENT(OUT)               :: p7period
   REAL(KIND=dp), INTENT(OUT)               :: p7t0
   REAL(KIND=dp), INTENT(OUT)               :: p7ecos
   REAL(KIND=dp), INTENT(OUT)               :: p7esin
   REAL(KIND=dp), INTENT(OUT)               :: p7incl
   REAL(KIND=dp), INTENT(OUT)               :: p7omega
   REAL(KIND=dp), INTENT(OUT)               :: p7q
   REAL(KIND=dp), INTENT(OUT)               :: p7ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p8tconj
   REAL(KIND=dp), INTENT(OUT)               :: p8period
   REAL(KIND=dp), INTENT(OUT)               :: p8t0
   REAL(KIND=dp), INTENT(OUT)               :: p8ecos
   REAL(KIND=dp), INTENT(OUT)               :: p8esin
   REAL(KIND=dp), INTENT(OUT)               :: p8incl
   REAL(KIND=dp), INTENT(OUT)               :: p8omega
   REAL(KIND=dp), INTENT(OUT)               :: p8q
   REAL(KIND=dp), INTENT(OUT)               :: p8ratrad
   REAL(KIND=dp), INTENT(OUT)               :: sw72
   REAL(KIND=dp), INTENT(OUT)               :: sw73
   REAL(KIND=dp), INTENT(OUT)               :: sw49
   REAL(KIND=dp), INTENT(OUT)               :: sw80
   REAL(KIND=dp), INTENT(OUT)               :: sw81
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint1(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint2(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint3(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint4(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint5(8)
   REAL(KIND=dp), INTENT(OUT)               :: tertratrad
   REAL(KIND=dp), INTENT(OUT)               :: p1mtc
   REAL(KIND=dp), INTENT(OUT)               :: p1ptc
   REAL(KIND=dp), INTENT(OUT)               :: p2mtc
   REAL(KIND=dp), INTENT(OUT)               :: p2ptc
   REAL(KIND=dp), INTENT(OUT)               :: p3mtc
   REAL(KIND=dp), INTENT(OUT)               :: p3ptc
   REAL(KIND=dp), INTENT(OUT)               :: p4mtc
   REAL(KIND=dp), INTENT(OUT)               :: p4ptc
   REAL(KIND=dp), INTENT(OUT)               :: p5mtc
   REAL(KIND=dp), INTENT(OUT)               :: p5ptc
   REAL(KIND=dp), INTENT(OUT)               :: p6mtc
   REAL(KIND=dp), INTENT(OUT)               :: p6ptc
   REAL(KIND=dp), INTENT(OUT)               :: p7mtc
   REAL(KIND=dp), INTENT(OUT)               :: p7ptc
   REAL(KIND=dp), INTENT(OUT)               :: p8mtc
   REAL(KIND=dp), INTENT(OUT)               :: p8ptc
   REAL(KIND=dp), INTENT(OUT)               :: pbmtc
   REAL(KIND=dp), INTENT(OUT)               :: pbptc
   REAL(KIND=dp), INTENT(OUT)               :: bigi2
   REAL(KIND=dp), INTENT(OUT)               :: bigi3
   REAL(KIND=dp), INTENT(OUT)               :: bigi4
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta2
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta3
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta4
   REAL(KIND=dp), INTENT(OUT)               :: bin2q
   REAL(KIND=dp), INTENT(OUT)               :: b2massdiff
   REAL(KIND=dp), INTENT(OUT)               :: b2masssum
   REAL(KIND=dp), INTENT(OUT)               :: b2raddiff
   REAL(KIND=dp), INTENT(OUT)               :: b2radsum
   REAL(KIND=dp), INTENT(OUT)               :: bin2ratrad
   REAL(KIND=dp), INTENT(OUT)               :: g10
   REAL(KIND=dp), INTENT(OUT)               :: g6
   REAL(KIND=dp), INTENT(OUT)               :: g7
   REAL(KIND=dp), INTENT(OUT)               :: g8
   REAL(KIND=dp), INTENT(OUT)               :: g9
   REAL(KIND=dp), INTENT(OUT)               :: massdiff
   REAL(KIND=dp), INTENT(OUT)               :: masssum
   REAL(KIND=dp), INTENT(OUT)               :: omega3
   REAL(KIND=dp), INTENT(OUT)               :: omega4
   REAL(KIND=dp), INTENT(OUT)               :: omega5
   REAL(KIND=dp), INTENT(OUT)               :: omega6
   REAL(KIND=dp), INTENT(OUT)               :: omega7
   REAL(KIND=dp), INTENT(OUT)               :: omega8
   REAL(KIND=dp), INTENT(OUT)               :: omega9
   REAL(KIND=dp), INTENT(OUT)               :: omega10
   REAL(KIND=dp), INTENT(OUT)               :: raddiff
   REAL(KIND=dp), INTENT(OUT)               :: radsum
   REAL(KIND=dp), INTENT(OUT)               :: rk3
   REAL(KIND=dp), INTENT(OUT)               :: rk4
   REAL(KIND=dp), INTENT(OUT)               :: rk5
   REAL(KIND=dp), INTENT(OUT)               :: rk6
   REAL(KIND=dp), INTENT(OUT)               :: rk7
   REAL(KIND=dp), INTENT(OUT)               :: rk8
   REAL(KIND=dp), INTENT(OUT)               :: rk9
   REAL(KIND=dp), INTENT(OUT)               :: rk10
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint6(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint7(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint8(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint9(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint10(8)
   REAL(KIND=dp), INTENT(OUT)               :: secmass
   REAL(KIND=dp), INTENT(OUT)               :: secrad
   REAL(KIND=dp), INTENT(OUT)               :: t10
   REAL(KIND=dp), INTENT(OUT)               :: t6
   REAL(KIND=dp), INTENT(OUT)               :: t7
   REAL(KIND=dp), INTENT(OUT)               :: t8
   REAL(KIND=dp), INTENT(OUT)               :: t9
   REAL(KIND=dp), INTENT(OUT)               :: fracsum
   REAL(KIND=dp), INTENT(OUT)               :: fracdiff
   REAL(KIND=dp), INTENT(OUT)               :: bin2m3
   REAL(KIND=dp), INTENT(OUT)               :: bin2m4
   REAL(KIND=dp), INTENT(OUT)               :: bin2r3
   REAL(KIND=dp), INTENT(OUT)               :: bin2r4
   REAL(KIND=dp), INTENT(OUT)               :: sqecos
   REAL(KIND=dp), INTENT(OUT)               :: sqesin
   REAL(KIND=dp), INTENT(OUT)               :: sqtertecos
   REAL(KIND=dp), INTENT(OUT)               :: sqtertesin
   REAL(KIND=dp), INTENT(OUT)               :: sqp2ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp2esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp3ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp3esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp4ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp4esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp5ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp5esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp6ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp6esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp7ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp7esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp8ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp8esin
   REAL(KIND=dp), INTENT(OUT)               :: angsum1
   REAL(KIND=dp), INTENT(OUT)               :: angdiff1
   REAL(KIND=dp), INTENT(OUT)               :: angsum2
   REAL(KIND=dp), INTENT(OUT)               :: angdiff2
   REAL(KIND=dp), INTENT(OUT)               :: angsum3
   REAL(KIND=dp), INTENT(OUT)               :: angdiff3
   REAL(KIND=dp), INTENT(OUT)               :: angsum4
   REAL(KIND=dp), INTENT(OUT)               :: angdiff4
   REAL(KIND=dp), INTENT(OUT)               :: angsum5
   REAL(KIND=dp), INTENT(OUT)               :: angdiff5
   REAL(KIND=dp), INTENT(OUT)               :: angsum6
   REAL(KIND=dp), INTENT(OUT)               :: angdiff6
   REAL(KIND=dp), INTENT(OUT)               :: angsum7
   REAL(KIND=dp), INTENT(OUT)               :: angdiff7
   REAL(KIND=dp), INTENT(OUT)               :: angsum8
   REAL(KIND=dp), INTENT(OUT)               :: angdiff8
   REAL(KIND=dp), INTENT(OUT)               :: fillsum
   REAL(KIND=dp), INTENT(OUT)               :: filldiff
   REAL(KIND=dp), INTENT(OUT)               :: binqtc
   REAL(KIND=dp), INTENT(OUT)               :: p1qtc
   REAL(KIND=dp), INTENT(OUT)               :: p2qtc
   REAL(KIND=dp), INTENT(OUT)               :: p3qtc
   REAL(KIND=dp), INTENT(OUT)               :: p4qtc
   REAL(KIND=dp), INTENT(OUT)               :: p5qtc
   REAL(KIND=dp), INTENT(OUT)               :: p6qtc
   REAL(KIND=dp), INTENT(OUT)               :: p7qtc
   REAL(KIND=dp), INTENT(OUT)               :: p8qtc
   REAL(KIND=dp), INTENT(OUT)               :: tbinoff
   REAL(KIND=dp), INTENT(OUT)               :: t1off
   REAL(KIND=dp), INTENT(OUT)               :: t2off
   REAL(KIND=dp), INTENT(OUT)               :: t3off
   REAL(KIND=dp), INTENT(OUT)               :: t4off
   REAL(KIND=dp), INTENT(OUT)               :: t5off
   REAL(KIND=dp), INTENT(OUT)               :: t6off
   REAL(KIND=dp), INTENT(OUT)               :: t7off
   REAL(KIND=dp), INTENT(OUT)               :: t8off
   INTEGER, INTENT(IN)                      :: iGR
   REAL(KIND=dp), INTENT(OUT)               :: tesscontam
!
   INTEGER :: i,kk,icnvrt
!
   REAL(KIND=dp)  :: g4,g5
!
!   loop over the gridloop.opt variables and match the tag codes
!   to the variables
!
   DO  i=1,nvarmax
      kk=icnvrt(svar(i)(1:2))
!
!  tag bq, binqTc, (slope in P-Tc plane for binary)
!
      IF(kk == 48)THEN
         binqtc=var(i)
      END IF
!
!  tag ct, tesscontam
!
      IF(kk == 83)THEN
         tesscontam=var(i)
      END IF
!
!  tag 1q, P1qTc, (slope in P-Tc plane for body 3)
!
      IF(kk == 3528)THEN
         p1qtc=var(i)
      END IF
!
!  tag 2q, P2qTc, (slope in P-Tc plane for body 4)
!
      IF(kk == 3560)THEN
         p2qtc=var(i)
      END IF
!
!  tag 3q, P3qTc, (slope in P-Tc plane for body 5)
!
      IF(kk == 3592)THEN
         p3qtc=var(i)
      END IF
!
!  tag 4q, P4qTc, (slope in P-Tc plane for body 6)
!
      IF(kk == 3624)THEN
         p4qtc=var(i)
      END IF
!
!  tag 5q, P5qTc, (slope in P-Tc plane for body 7)
!
      IF(kk == 3656)THEN
         p5qtc=var(i)
      END IF
!
!  tag 6q, P6qTc, (slope in P-Tc plane for body 8)
!
      IF(kk == 3688)THEN
         p6qtc=var(i)
      END IF
!
!  tag 7q, P7qTc, (slope in P-Tc plane for body 9)
!
      IF(kk == 3720)THEN
         p7qtc=var(i)
      END IF
!
!  tag 8q, P8qTc, (slope in P-Tc plane for body 10)
!
      IF(kk == 3752)THEN
         p8qtc=var(i)
      END IF
!
!  tag bt, Tbinoff, (offset in P-Tc plane for binary)
!
      IF(kk == 51)THEN
         tbinoff=var(i)
      END IF
!
!  tag 1t, T1off, (offset in P-Tc plane for body 3)
!
      IF(kk == 3531)THEN
         t1off=var(i)
      END IF
!
!  tag 2t, T2off, (offset in P-Tc plane for body 4)
!
      IF(kk == 3563)THEN
         t2off=var(i)
      END IF
!
!  tag 3t, T3off, (offset in P-Tc plane for body 5)
!
      IF(kk == 3595)THEN
         t3off=var(i)
      END IF
!
!  tag 4t, T4off, (offset in P-Tc plane for body 6)
!
      IF(kk == 3627)THEN
         t4off=var(i)
      END IF
!
!  tag 5t, T5off, (offset in P-Tc plane for body 7)
!
      IF(kk == 3659)THEN
         t5off=var(i)
      END IF
!
!  tag 6t, T6off, (offset in P-Tc plane for body 8)
!
      IF(kk == 3691)THEN
         t6off=var(i)
      END IF
!
!  tag 7t, T7off, (offset in P-Tc plane for body 9)
!
      IF(kk == 3723)THEN
         t7off=var(i)
      END IF
!
!  tag 8t, T8off, (offset in P-Tc plane for body 10)
!
      IF(kk == 3755)THEN
         t8off=var(i)
      END IF
!
!  tag sf, fillsum (sum of fill1 and fill2)
!
      IF(kk == 581)THEN
         fillsum=var(i)
      END IF
!
!  tag sd, filldiff (difference of fill1 and fill2)
!
      IF(kk == 579)THEN
         filldiff=var(i)
      END IF
!
!  tag 1a, angsum1 (sum of P1incl and P1Omega)
!
      IF(kk == 3512)THEN
         angsum1=var(i)
      END IF
!
!  tag 2a, angsum2 (sum of P2incl and P2Omega)
!
      IF(kk == 3544)THEN
         angsum2=var(i)
      END IF
!
!  tag 3a, angsum3 (sum of P3incl and P3Omega)
!
      IF(kk == 3576)THEN
         angsum3=var(i)
      END IF
!
!  tag 4a, angsum4 (sum of P4incl and P4Omega)
!
      IF(kk == 3608)THEN
         angsum4=var(i)
      END IF
!
!  tag 5a, angsum5 (sum of P5incl and P5Omega)
!
      IF(kk == 3640)THEN
         angsum5=var(i)
      END IF
!
!  tag 6a, angsum6 (sum of P6incl and P6Omega)
!
      IF(kk == 3672)THEN
         angsum6=var(i)
      END IF
!
!  tag 7a, angsum7 (sum of P7incl and P7Omega)
!
      IF(kk == 3704)THEN
         angsum7=var(i)
      END IF
!
!  tag 8a, angsum8 (sum of P8incl and P8Omega)
!
      IF(kk == 3736)THEN
         angsum8=var(i)
      END IF
!
!  tag 1d, angdiff1 (difference of P1incl and P1Omega)
!
      IF(kk == 3515)THEN
         angdiff1=var(i)
      END IF
!
!  tag 2d, angdiff2 (difference of P2incl and P2Omega)
!
      IF(kk == 3547)THEN
         angdiff2=var(i)
      END IF
!
!  tag 3d, angdiff3 (difference of P3incl and P3Omega)
!
      IF(kk == 3579)THEN
         angdiff3=var(i)
      END IF
!
!  tag 4d, angdiff4 (difference of P4incl and P4Omega)
!
      IF(kk == 3611)THEN
         angdiff4=var(i)
      END IF
!
!  tag 5d, angdiff5 (difference of P5incl and P5Omega)
!
      IF(kk == 3643)THEN
         angdiff5=var(i)
      END IF
!
!  tag 6d, angdiff6 (difference of P6incl and P6Omega)
!
      IF(kk == 3675)THEN
         angdiff6=var(i)
      END IF
!
!  tag 7d, angdiff7 (difference of P7incl and P7Omega)
!
      IF(kk == 3707)THEN
         angdiff7=var(i)
      END IF
!
!  tag 8d, angdiff8 (difference of P8incl and P8Omega)
!
      IF(kk == 3739)THEN
         angdiff8=var(i)
      END IF
!
!  tag fs, fracsum (sum of fractional radii)
!
      IF(kk == 178)THEN
         fracsum=var(i)
      END IF
!
!  tag fd, fracdiff (difference of fractional radii)
!
      IF(kk == 163)THEN
         fracdiff=var(i)
      END IF
!
!  tag 97, bin2M3 (M3 in solar masses in binary+binary)
!
      IF(kk == 4790)THEN
         bin2m3=var(i)
      END IF
!
!  tag 98, bin2M4 (M4 in solar masses in binary+binary)
!
      IF(kk == 4791)THEN
         bin2m4=var(i)
      END IF
!
!  tag 99, bin2R3 (R3 in solar radii in binary+binary)
!
      IF(kk == 4792)THEN
         bin2r3=var(i)
      END IF
!
!  tag 90, bin2R3 (R3 in solar radii in binary+binary)
!
      IF(kk == 4783)THEN
         bin2r4=var(i)
      END IF
!
!  tag bs, sqesin, (sqrt(e)*sin(omega), binary)
!
      IF(kk == 50)THEN
         sqesin=var(i)
      END IF
!
!  tag bc, sqecos, (sqrt(e)*sin(omega), binary)
!
      IF(kk == 34)THEN
         sqecos=var(i)
      END IF
!
!  tag 1s, sqtertesin, (sqrt(e)*sin(omega), body 3)
!
      IF(kk == 3530)THEN
         sqtertesin=var(i)
      END IF
!
!  tag 1c, sqtertecos, (sqrt(e)*sin(omega), body 3)
!
      IF(kk == 3514)THEN
         sqtertecos=var(i)
      END IF
!
!  tag 2s, sqP2esin, (sqrt(e)*sin(omega), body 4)
!
      IF(kk == 3562)THEN
         sqp2esin=var(i)
      END IF
!
!  tag 2c, sqP2ecos, (sqrt(e)*sin(omega), body 4)
!
      IF(kk == 3546)THEN
         sqp2ecos=var(i)
      END IF
!
!  tag 3s, sqP3tesin, (sqrt(e)*sin(omega), body 5)
!
      IF(kk == 3594)THEN
         sqp3esin=var(i)
      END IF
!
!  tag 3c, sqP3tecos, (sqrt(e)*sin(omega), body 5)
!
      IF(kk == 3578)THEN
         sqp3ecos=var(i)
      END IF
!
!  tag 4s, sqP4tesin, (sqrt(e)*sin(omega), body 6)
!
      IF(kk == 3626)THEN
         sqp4esin=var(i)
      END IF
!
!  tag 4c, sqP4tecos, (sqrt(e)*sin(omega), body 6)
!
      IF(kk == 3610)THEN
         sqp4ecos=var(i)
      END IF
!
!  tag 5s, sqP5tesin, (sqrt(e)*sin(omega), body 7)
!
      IF(kk == 3658)THEN
         sqp5esin=var(i)
      END IF
!
!  tag 5c, sqP5tecos, (sqrt(e)*sin(omega), body 7)
!
      IF(kk == 3642)THEN
         sqp5ecos=var(i)
      END IF
!
!  tag 6s, sqP6tesin, (sqrt(e)*sin(omega), body 8)
!
      IF(kk == 3690)THEN
         sqp6esin=var(i)
      END IF
!
!  tag 6c, sqP6tecos, (sqrt(e)*sin(omega), body 8)
!
      IF(kk == 3674)THEN
         sqp6ecos=var(i)
      END IF
!
!  tag 7s, sqP7tesin, (sqrt(e)*sin(omega), body 9)
!
      IF(kk == 3722)THEN
         sqp7esin=var(i)
      END IF
!
!  tag 7c, sqP7tecos, (sqrt(e)*sin(omega), body 9)
!
      IF(kk == 3706)THEN
         sqp7ecos=var(i)
      END IF
!
!  tag 8s, sqP8tesin, (sqrt(e)*sin(omega), body 10)
!
      IF(kk == 3754)THEN
         sqp8esin=var(i)
      END IF
!
!  tag 8c, sqP8tecos, (sqrt(e)*sin(omega), body 10)
!
      IF(kk == 3738)THEN
         sqp8ecos=var(i)
      END IF
!
!  tag 1m, P1mTc, period-Tconj, body 3
!
      IF(kk == 3524)THEN
         p1mtc=var(i)
      END IF
!
!  tag 1p, P1pTc, period+Tconj, body 3
!
      IF(kk == 3527)THEN
         p1ptc=var(i)
      END IF
!
!  tag 2m, P2mTc, period-Tconj, body 3
!
      IF(kk == 3556)THEN
         p2mtc=var(i)
      END IF
!
!  tag 2p, P2pTc, period+Tconj, body 4
!
      IF(kk == 3559)THEN
         p2ptc=var(i)
      END IF
!
!  tag 3m, P3mTc, period-Tconj, body 5
!
      IF(kk == 3588)THEN
         p3mtc=var(i)
      END IF
!
!  tag 3p, P2pTc, period+Tconj, body 5
!
      IF(kk == 3591)THEN
         p3ptc=var(i)
      END IF
!
!  tag 4m, P4mTc, period-Tconj, body 6
!
      IF(kk == 3620)THEN
         p4mtc=var(i)
      END IF
!
!  tag 4p, P4pTc, period+Tconj, body 6
!
      IF(kk == 3623)THEN
         p4ptc=var(i)
      END IF
!
!  tag 5m, P5mTc, period-Tconj, body 7
!
      IF(kk == 3652)THEN
         p5mtc=var(i)
      END IF
!
!  tag 5p, P5pTc, period+Tconj, body 7
!
      IF(kk == 3655)THEN
         p5ptc=var(i)
      END IF
!
!  tag 6m, P6mTc, period-Tconj, body 8
!
      IF(kk == 3684)THEN
         p6mtc=var(i)
      END IF
!
!  tag 6p, P6pTc, period+Tconj, body 8
!
      IF(kk == 3687)THEN
         p6ptc=var(i)
      END IF
!
!  tag 7m, P7mTc, period-Tconj, body 9
!
      IF(kk == 3716)THEN
         p7mtc=var(i)
      END IF
!
!  tag 7p, P7pTc, period+Tconj, body 9
!
      IF(kk == 3719)THEN
         p7ptc=var(i)
      END IF
!
!  tag 8m, P8mTc, period-Tconj, body 10
!
      IF(kk == 3748)THEN
         p8mtc=var(i)
      END IF
!
!  tag 8p, P8pTc, period+Tconj, body 10
!
      IF(kk == 3751)THEN
         p8ptc=var(i)
      END IF
!
!  tag bm, PbmTc, period-Tconj, binary
!
      IF(kk == 44)THEN
         pbmtc=var(i)
      END IF
!
!  tag bp, PbpTc, period+Tconj, binary
!
      IF(kk == 47)THEN
         pbptc=var(i)
      END IF
!
!  tag bi, axis_I2, inclination of rot. axis star 2
!
      IF(kk == 40)THEN
         bigi2=var(i)
      END IF
!
!  tag bb, axis_beta2, position angle of rot. axis star 2
!
      IF(kk == 33)THEN
         bigbeta2=var(i)
      END IF
!
!  tag ci, axis_I3, inclination of rot. axis star 3
!
      IF(kk == 72)THEN
         bigi3=var(i)
      END IF
!
!  tag cb, axis_beta3, position angle of rot. axis star 3
!
      IF(kk == 65)THEN
         bigbeta3=var(i)
      END IF
!
!  tag di, axis_I4, inclination of rot. axis star 4
!
      IF(kk == 104)THEN
         bigi4=var(i)
      END IF
!
!  tag db, axis_beta4, position angle of rot. axis star 4
!
      IF(kk == 97)THEN
         bigbeta4=var(i)
      END IF
!
!  tag 91, b2masssum (M3+M4 in binary+binary)
!
      IF(kk == 4784)THEN
         b2masssum=var(i)
      END IF
!
!  tag 92, b2massdiff (M3-M4 in binary+binary)
!
      IF(kk == 4785)THEN
         b2massdiff=var(i)
      END IF
!
!  tag 93, bin2Q (M4/M3 in binary+binary)
!
      IF(kk == 4786)THEN
         bin2q=var(i)
      END IF
!
!  tag 94, b2radsum (R3+R4 in binary+binary)
!
      IF(kk == 4787)THEN
         b2radsum=var(i)
      END IF
!
!  tag 95, b2raddiff (R3-R4 in binary+binary)
!
      IF(kk == 4788)THEN
         b2raddiff=var(i)
      END IF
!
!  tag 96, bin2ratrad (R3/R4 in binary+binary)
!
      IF(kk == 4789)THEN
         bin2ratrad=var(i)
      END IF
!
!   tag g4, g4
!
      IF(kk == 1211)THEN
         g4=var(i)
      END IF
!
!   tag g5, g5
!
      IF(kk == 1212)THEN
         g5=var(i)
      END IF
!
!   tag g6, g6
!
      IF(kk == 1213)THEN
         g6=var(i)
      END IF
!
!   tag g7, g7
!
      IF(kk == 1214)THEN
         g7=var(i)
      END IF
!
!   tag g8, g8
!
      IF(kk == 1215)THEN
         g8=var(i)
      END IF
!
!   tag g9, g9
!
      IF(kk == 1216)THEN
         g9=var(i)
      END IF
!
!   tag g0, g10
!
      IF(kk == 1207)THEN
         g10=var(i)
      END IF
!
!   tag md, massdiff (M1-M2 for binary)
!
      IF(kk == 387)THEN
         massdiff=var(i)
      END IF
!
!   tag ms, masssum (M1+M2 for binary)
!
      IF(kk == 402)THEN
         masssum=var(i)
      END IF
!
!   tag o3, omega3
!
      IF(kk == 1466)THEN
         omega3=var(i)
      END IF
!
!   tag o4, omega4
!
      IF(kk == 1467)THEN
         omega4=var(i)
      END IF
!
!   tag o5, omega5
!
      IF(kk == 1468)THEN
         omega5=var(i)
      END IF
!
!   tag o6, omega6
!
      IF(kk == 1469)THEN
         omega6=var(i)
      END IF
!
!   tag o7, omega7
!
      IF(kk == 1470)THEN
         omega7=var(i)
      END IF
!
!   tag o8, omega8
!
      IF(kk == 1471)THEN
         omega8=var(i)
      END IF
!
!   tag o9, omega9
!
      IF(kk == 1472)THEN
         omega9=var(i)
      END IF
!
!   tag o0, omega10
!
      IF(kk == 1463)THEN
         omega10=var(i)
      END IF
!
!   tag rs, radsum (R1+R2 for binary)
!
      IF(kk == 562)THEN
         radsum=var(i)
      END IF
!
!   tag rd, raddiff (R1-R2 for binary)
!
      IF(kk == 547)THEN
         raddiff=var(i)
      END IF
!
!   tag a3, rk3 (apsidal constant, body 3)
!
      IF(kk == 1018)THEN
         rk3=var(i)
      END IF
!
!   tag a4, rk4 (apsidal constant, body 4)
!
      IF(kk == 1019)THEN
         rk4=var(i)
      END IF
!
!   tag a5, rk5 (apsidal constant, body 5)
!
      IF(kk == 1020)THEN
         rk5=var(i)
      END IF
!
!   tag a6, rk6 (apsidal constant, body 6)
!
      IF(kk == 1021)THEN
         rk6=var(i)
      END IF
!
!   tag a7, rk7 (apsidal constant, body 7)
!
      IF(kk == 1022)THEN
         rk7=var(i)
      END IF
!
!   tag a8, rk8 (apsidal constant, body 8)
!
      IF(kk == 1023)THEN
         rk8=var(i)
      END IF
!
!   tag a9, rk9 (apsidal constant, body 9)
!
      IF(kk == 1024)THEN
         rk9=var(i)
      END IF
!
!   tag a0, rk10 (apsidal constant, body 10)
!
      IF(kk == 1015)THEN
         rk10=var(i)
      END IF
!
!   tag sm, secmass
!
      IF(kk == 588)THEN
         secmass=var(i)
      END IF
!
!   tag sr, secrad
!
      IF(kk == 593)THEN
         secrad=var(i)
      END IF
!
!   tag t6, t6 (Teff6)
!
      IF(kk == 1629)THEN
         t6=var(i)
      END IF
!
!   tag t7, t7 (Teff7)
!
      IF(kk == 1630)THEN
         t7=var(i)
      END IF
!
!   tag t8, t8 (Teff8)
!
      IF(kk == 1631)THEN
         t8=var(i)
      END IF
!
!   tag t9, t9 (Teff9)
!
      IF(kk == 1632)THEN
         t9=var(i)
      END IF
!
!   tag ta, t10 (Teff10)
!
      IF(kk == 608)THEN
         t10=var(i)
      END IF
!
!  tag 61, flux star 6, band 1
!
      IF(kk == 4688)THEN
         sdarkint6(1)=var(i)
         IF(sdarkint6(1) < 0.0_dp)sdarkint6(1)=0.0_dp
      END IF
!
!  tag 62, flux star 6, band 2
!
      IF(kk == 4689)THEN
         sdarkint6(2)=var(i)
         IF(sdarkint6(2) < 0.0_dp)sdarkint6(2)=0.0_dp
      END IF
!
!  tag 63, flux star 6, band 3
!
      IF(kk == 4690)THEN
         sdarkint6(3)=var(i)
         IF(sdarkint6(3) < 0.0_dp)sdarkint6(3)=0.0_dp
      END IF
!
!  tag 64, flux star 6, band 4
!
      IF(kk == 4691)THEN
         sdarkint6(4)=var(i)
         IF(sdarkint6(4) < 0.0_dp)sdarkint6(4)=0.0_dp
      END IF
!
!  tag 65, flux star 6, band 5
!
      IF(kk == 4692)THEN
         sdarkint6(5)=var(i)
         IF(sdarkint6(5) < 0.0_dp)sdarkint6(5)=0.0_dp
      END IF
!
!  tag 66, flux star 6, band 6
!
      IF(kk == 4693)THEN
         sdarkint6(6)=var(i)
         IF(sdarkint6(6) < 0.0_dp)sdarkint6(6)=0.0_dp
      END IF
!
!  tag 67, flux star 6, band 7
!
      IF(kk == 4694)THEN
         sdarkint6(7)=var(i)
         IF(sdarkint6(7) < 0.0_dp)sdarkint6(7)=0.0_dp
      END IF
!
!  tag 68, flux star 6, band 8
!
      IF(kk == 4695)THEN
         sdarkint6(8)=var(i)
         IF(sdarkint6(8) < 0.0_dp)sdarkint6(8)=0.0_dp
      END IF
!
!  tag 71, flux star 7, band 1
!
      IF(kk == 4720)THEN
         sdarkint7(1)=var(i)
         IF(sdarkint7(1) < 0.0_dp)sdarkint7(1)=0.0_dp
      END IF
!
!  tag 72, flux star 7, band 2
!
      IF(kk == 4721)THEN
         sdarkint7(2)=var(i)
         IF(sdarkint7(2) < 0.0_dp)sdarkint7(2)=0.0_dp
      END IF
!
!  tag 73, flux star 7, band 3
!
      IF(kk == 4722)THEN
         sdarkint7(3)=var(i)
         IF(sdarkint7(3) < 0.0_dp)sdarkint7(3)=0.0_dp
      END IF
!
!  tag 74, flux star 7, band 4
!
      IF(kk == 4723)THEN
         sdarkint7(4)=var(i)
         IF(sdarkint7(4) < 0.0_dp)sdarkint7(4)=0.0_dp
      END IF
!
!  tag 75, flux star 7, band 5
!
      IF(kk == 4724)THEN
         sdarkint7(5)=var(i)
         IF(sdarkint7(5) < 0.0_dp)sdarkint7(5)=0.0_dp
      END IF
!
!  tag 76, flux star 7, band 6
!
      IF(kk == 4725)THEN
         sdarkint7(6)=var(i)
         IF(sdarkint7(6) < 0.0_dp)sdarkint7(6)=0.0_dp
      END IF
!
!  tag 77, flux star 7, band 7
!
      IF(kk == 4726)THEN
         sdarkint7(7)=var(i)
         IF(sdarkint7(7) < 0.0_dp)sdarkint7(7)=0.0_dp
      END IF
!
!  tag 78, flux star 7, band 8
!
      IF(kk == 4727)THEN
         sdarkint7(8)=var(i)
         IF(sdarkint7(8) < 0.0_dp)sdarkint7(8)=0.0_dp
      END IF
!
!  tag 81, flux star 8, band 1
!
      IF(kk == 4752)THEN
         sdarkint8(1)=var(i)
         IF(sdarkint8(1) < 0.0_dp)sdarkint8(1)=0.0_dp
      END IF
!
!  tag 82, flux star 8, band 2
!
      IF(kk == 4753)THEN
         sdarkint8(2)=var(i)
         IF(sdarkint8(2) < 0.0_dp)sdarkint8(2)=0.0_dp
      END IF
!
!  tag 83, flux star 8, band 3
!
      IF(kk == 4754)THEN
         sdarkint8(3)=var(i)
         IF(sdarkint8(3) < 0.0_dp)sdarkint8(3)=0.0_dp
      END IF
!
!  tag 84, flux star 8, band 4
!
      IF(kk == 4755)THEN
         sdarkint8(4)=var(i)
         IF(sdarkint8(4) < 0.0_dp)sdarkint8(4)=0.0_dp
      END IF
!
!  tag 85, flux star 8, band 5
!
      IF(kk == 4756)THEN
         sdarkint8(5)=var(i)
         IF(sdarkint8(5) < 0.0_dp)sdarkint8(5)=0.0_dp
      END IF
!
!  tag 86, flux star 8, band 6
!
      IF(kk == 4757)THEN
         sdarkint8(6)=var(i)
         IF(sdarkint8(6) < 0.0_dp)sdarkint8(6)=0.0_dp
      END IF
!
!  tag 87, flux star 8, band 7
!
      IF(kk == 4758)THEN
         sdarkint8(7)=var(i)
         IF(sdarkint8(7) < 0.0_dp)sdarkint8(7)=0.0_dp
      END IF
!
!  tag 88, flux star 8, band 8
!
      IF(kk == 4759)THEN
         sdarkint8(8)=var(i)
         IF(sdarkint8(8) < 0.0_dp)sdarkint8(8)=0.0_dp
      END IF
!
!  tag 91, flux star 9, band 1
!
      IF(kk == 2784)THEN
         sdarkint9(1)=var(i)
         IF(sdarkint9(1) < 0.0_dp)sdarkint9(1)=0.0_dp
      END IF
!
!  tag 92, flux star 9, band 2
!
      IF(kk == 2785)THEN
         sdarkint9(2)=var(i)
         IF(sdarkint9(2) < 0.0_dp)sdarkint9(2)=0.0_dp
      END IF
!
!  tag 93, flux star 9, band 3
!
      IF(kk == 2786)THEN
         sdarkint9(3)=var(i)
         IF(sdarkint9(3) < 0.0_dp)sdarkint9(3)=0.0_dp
      END IF
!
!  tag 94, flux star 9, band 4
!
      IF(kk == 2787)THEN
         sdarkint9(4)=var(i)
         IF(sdarkint9(4) < 0.0_dp)sdarkint9(4)=0.0_dp
      END IF
!
!  tag 95, flux star 9, band 5
!
      IF(kk == 2788)THEN
         sdarkint9(5)=var(i)
         IF(sdarkint9(5) < 0.0_dp)sdarkint9(5)=0.0_dp
      END IF
!
!  tag 96, flux star 9, band 6
!
      IF(kk == 2789)THEN
         sdarkint9(6)=var(i)
         IF(sdarkint9(6) < 0.0_dp)sdarkint9(6)=0.0_dp
      END IF
!
!  tag 97, flux star 9, band 7
!
      IF(kk == 2790)THEN
         sdarkint9(7)=var(i)
         IF(sdarkint9(7) < 0.0_dp)sdarkint9(7)=0.0_dp
      END IF
!
!  tag 98, flux star 9, band 8
!
      IF(kk == 2791)THEN
         sdarkint9(8)=var(i)
         IF(sdarkint9(8) < 0.0_dp)sdarkint9(8)=0.0_dp
      END IF
!
!  tag 01, flux star 10, band 1
!
      IF(kk == 4496)THEN
         sdarkint10(1)=var(i)
         IF(sdarkint10(1) < 0.0_dp)sdarkint10(1)=0.0_dp
      END IF
!
!  tag 02, flux star 10, band 2
!
      IF(kk == 4497)THEN
         sdarkint10(2)=var(i)
         IF(sdarkint10(2) < 0.0_dp)sdarkint10(2)=0.0_dp
      END IF
!
!  tag 03, flux star 10, band 3
!
      IF(kk == 4498)THEN
         sdarkint10(3)=var(i)
         IF(sdarkint10(3) < 0.0_dp)sdarkint10(3)=0.0_dp
      END IF
!
!  tag 04, flux star 10, band 4
!
      IF(kk == 4499)THEN
         sdarkint10(4)=var(i)
         IF(sdarkint10(4) < 0.0_dp)sdarkint10(4)=0.0_dp
      END IF
!
!  tag 05, flux star 10, band 5
!
      IF(kk == 4500)THEN
         sdarkint10(5)=var(i)
         IF(sdarkint10(5) < 0.0_dp)sdarkint10(5)=0.0_dp
      END IF
!
!  tag 06, flux star 10, band 6
!
      IF(kk == 4501)THEN
         sdarkint10(6)=var(i)
         IF(sdarkint10(6) < 0.0_dp)sdarkint10(6)=0.0_dp
      END IF
!
!  tag 07, flux star 10, band 7
!
      IF(kk == 4502)THEN
         sdarkint10(7)=var(i)
         IF(sdarkint10(7) < 0.0_dp)sdarkint10(7)=0.0_dp
      END IF
!
!  tag 08, flux star 10, band 8
!
      IF(kk == 4503)THEN
         sdarkint10(8)=var(i)
         IF(sdarkint10(8) < 0.0_dp)sdarkint10(8)=0.0_dp
      END IF
!
!  tag 11, flux star 1, band 1
!
      IF(kk == 4528)THEN
         sdarkint1(1)=var(i)
         IF(sdarkint1(1) < 0.0_dp)sdarkint1(1)=0.0_dp
      END IF
!
!  tag 12, flux star 1, band 2
!
      IF(kk == 4529)THEN
         sdarkint1(2)=var(i)
         IF(sdarkint1(2) < 0.0_dp)sdarkint1(2)=0.0_dp
      END IF
!
!  tag 13, flux star 1, band 3
!
      IF(kk == 4530)THEN
         sdarkint1(3)=var(i)
         IF(sdarkint1(3) < 0.0_dp)sdarkint1(3)=0.0_dp
      END IF
!
!  tag 14, flux star 1, band 4
!
      IF(kk == 4531)THEN
         sdarkint1(4)=var(i)
         IF(sdarkint1(4) < 0.0_dp)sdarkint1(4)=0.0_dp
      END IF
!
!  tag 15, flux star 1, band 5
!
      IF(kk == 4532)THEN
         sdarkint1(5)=var(i)
         IF(sdarkint1(5) < 0.0_dp)sdarkint1(5)=0.0_dp
      END IF
!
!  tag 16, flux star 1, band 6
!
      IF(kk == 4533)THEN
         sdarkint1(6)=var(i)
         IF(sdarkint1(6) < 0.0_dp)sdarkint1(6)=0.0_dp
      END IF
!
!  tag 17, flux star 1, band 7
!
      IF(kk == 4534)THEN
         sdarkint1(7)=var(i)
         IF(sdarkint1(7) < 0.0_dp)sdarkint1(7)=0.0_dp
      END IF
!
!  tag 18, flux star 1, band 8
!
      IF(kk == 4535)THEN
         sdarkint1(8)=var(i)
         IF(sdarkint1(8) < 0.0_dp)sdarkint1(8)=0.0_dp
      END IF
!
!  tag 21, flux star 2, band 1
!
      IF(kk == 4560)THEN
         sdarkint2(1)=var(i)
         IF(sdarkint2(1) < 0.0_dp)sdarkint2(1)=0.0_dp
      END IF
!
!  tag 22, flux star 2, band 2
!
      IF(kk == 4561)THEN
         sdarkint2(2)=var(i)
         IF(sdarkint2(2) < 0.0_dp)sdarkint2(2)=0.0_dp
      END IF
!
!  tag 23, flux star 2, band 3
!
      IF(kk == 4562)THEN
         sdarkint2(3)=var(i)
         IF(sdarkint2(3) < 0.0_dp)sdarkint2(3)=0.0_dp
      END IF
!
!  tag 24, flux star 2, band 4
!
      IF(kk == 4563)THEN
         sdarkint2(4)=var(i)
         IF(sdarkint2(4) < 0.0_dp)sdarkint2(4)=0.0_dp
      END IF
!
!  tag 25, flux star 2, band 5
!
      IF(kk == 4564)THEN
         sdarkint2(5)=var(i)
         IF(sdarkint2(5) < 0.0_dp)sdarkint2(5)=0.0_dp
      END IF
!
!  tag 26, flux star 2, band 6
!
      IF(kk == 4565)THEN
         sdarkint2(6)=var(i)
         IF(sdarkint2(6) < 0.0_dp)sdarkint2(6)=0.0_dp
      END IF
!
!  tag 27, flux star 2, band 7
!
      IF(kk == 4566)THEN
         sdarkint2(7)=var(i)
         IF(sdarkint2(7) < 0.0_dp)sdarkint2(7)=0.0_dp
      END IF
!
!  tag 28, flux star 2, band 8
!
      IF(kk == 4567)THEN
         sdarkint2(8)=var(i)
         IF(sdarkint2(8) < 0.0_dp)sdarkint2(8)=0.0_dp
      END IF
!
!  tag 31, flux star 3, band 1
!
      IF(kk == 4592)THEN
         sdarkint3(1)=var(i)
         IF(sdarkint3(1) < 0.0_dp)sdarkint3(1)=0.0_dp
      END IF
!
!  tag 32, flux star 3, band 2
!
      IF(kk == 4593)THEN
         sdarkint3(2)=var(i)
         IF(sdarkint3(2) < 0.0_dp)sdarkint3(2)=0.0_dp
      END IF
!
!  tag 33, flux star 3, band 3
!
      IF(kk == 4594)THEN
         sdarkint3(3)=var(i)
         IF(sdarkint3(3) < 0.0_dp)sdarkint3(3)=0.0_dp
      END IF
!
!  tag 34, flux star 3, band 4
!
      IF(kk == 4595)THEN
         sdarkint3(4)=var(i)
         IF(sdarkint3(4) < 0.0_dp)sdarkint3(4)=0.0_dp
      END IF
!
!  tag 35, flux star 3, band 5
!
      IF(kk == 4596)THEN
         sdarkint3(5)=var(i)
         IF(sdarkint3(5) < 0.0_dp)sdarkint3(5)=0.0_dp
      END IF
!
!  tag 36, flux star 3, band 6
!
      IF(kk == 4597)THEN
         sdarkint3(6)=var(i)
         IF(sdarkint3(6) < 0.0_dp)sdarkint3(6)=0.0_dp
      END IF
!
!  tag 37, flux star 3, band 7
!
      IF(kk == 4598)THEN
         sdarkint3(7)=var(i)
         IF(sdarkint3(7) < 0.0_dp)sdarkint3(7)=0.0_dp
      END IF
!
!  tag 38, flux star 3, band 8
!
      IF(kk == 4599)THEN
         sdarkint3(8)=var(i)
         IF(sdarkint3(8) < 0.0_dp)sdarkint3(8)=0.0_dp
      END IF
!
!  tag 41, flux star 4, band 1
!
      IF(kk == 4624)THEN
         sdarkint4(1)=var(i)
         IF(sdarkint4(1) < 0.0_dp)sdarkint4(1)=0.0_dp
      END IF
!
!  tag 42, flux star 4, band 2
!
      IF(kk == 4625)THEN
         sdarkint4(2)=var(i)
         IF(sdarkint4(2) < 0.0_dp)sdarkint4(2)=0.0_dp
      END IF
!
!  tag 43, flux star 4, band 3
!
      IF(kk == 4626)THEN
         sdarkint4(3)=var(i)
         IF(sdarkint4(3) < 0.0_dp)sdarkint4(3)=0.0_dp
      END IF
!
!  tag 44, flux star 4, band 4
!
      IF(kk == 4627)THEN
         sdarkint4(4)=var(i)
         IF(sdarkint4(4) < 0.0_dp)sdarkint4(4)=0.0_dp
      END IF
!
!  tag 45, flux star 4, band 5
!
      IF(kk == 4628)THEN
         sdarkint4(5)=var(i)
         IF(sdarkint4(5) < 0.0_dp)sdarkint4(5)=0.0_dp
      END IF
!
!  tag 46, flux star 4, band 6
!
      IF(kk == 4629)THEN
         sdarkint4(6)=var(i)
         IF(sdarkint4(6) < 0.0_dp)sdarkint4(6)=0.0_dp
      END IF
!
!  tag 47, flux star 4, band 7
!
      IF(kk == 4630)THEN
         sdarkint4(7)=var(i)
         IF(sdarkint4(7) < 0.0_dp)sdarkint4(7)=0.0_dp
      END IF
!
!  tag 48, flux star 4, band 8
!
      IF(kk == 4631)THEN
         sdarkint4(8)=var(i)
         IF(sdarkint4(8) < 0.0_dp)sdarkint4(8)=0.0_dp
      END IF
!
!  tag 51, flux star 5, band 1
!
      IF(kk == 4656)THEN
         sdarkint5(1)=var(i)
         IF(sdarkint5(1) < 0.0_dp)sdarkint5(1)=0.0_dp
      END IF
!
!  tag 52, flux star 5, band 2
!
      IF(kk == 4657)THEN
         sdarkint5(2)=var(i)
         IF(sdarkint5(2) < 0.0_dp)sdarkint5(2)=0.0_dp
      END IF
!
!  tag 53, flux star 5, band 3
!
      IF(kk == 4658)THEN
         sdarkint5(3)=var(i)
         IF(sdarkint5(3) < 0.0_dp)sdarkint5(3)=0.0_dp
      END IF
!
!  tag 54, flux star 5, band 4
!
      IF(kk == 4659)THEN
         sdarkint5(4)=var(i)
         IF(sdarkint5(4) < 0.0_dp)sdarkint5(4)=0.0_dp
      END IF
!
!  tag 55, flux star 5, band 5
!
      IF(kk == 4660)THEN
         sdarkint5(5)=var(i)
         IF(sdarkint5(5) < 0.0_dp)sdarkint5(5)=0.0_dp
      END IF
!
!  tag 56, flux star 5, band 6
!
      IF(kk == 4661)THEN
         sdarkint5(6)=var(i)
         IF(sdarkint5(6) < 0.0_dp)sdarkint5(6)=0.0_dp
      END IF
!
!  tag 57, flux star 5, band 7
!
      IF(kk == 4662)THEN
         sdarkint5(7)=var(i)
         IF(sdarkint5(7) < 0.0_dp)sdarkint5(7)=0.0_dp
      END IF
!
!  tag 58, flux star 5, band 8
!
      IF(kk == 4663)THEN
         sdarkint5(8)=var(i)
         IF(sdarkint5(8) < 0.0_dp)sdarkint5(8)=0.0_dp
      END IF
!
!  tag a1, tidal apsidal constant, star 1
!
      IF(kk == 1016)THEN
         sw72=var(i)
         IF(sw72 < 0.0_dp)sw72=0.0_dp
         IF(iGR == 4)sw73=sw72
      END IF
!
!  tag a2, tidal apsidal constant, star 2
!
      IF(kk == 1017)THEN
         sw73=var(i)
         IF(sw73 < 0.0_dp)sw73=0.0_dp
      END IF
!
!  tag Ob, nodal angle of binary
!
      IF(kk == 449)THEN
         sw49=var(i)
      END IF
!
!  planet 2 parameters
!
!  tag uj, P2tconj
!
!
      IF(kk == 649)THEN
         p2tconj=var(i)
      END IF
!
!  tag ut, P2period
!
      IF(kk == 659)THEN
         p2period=var(i)
      END IF
!
!  tag uu, P2T0
!
      IF(kk == 660)THEN
         p2t0=var(i)
      END IF
!
!  tag uv, P2ecos
!
      IF(kk == 661)THEN
         p2ecos=var(i)
      END IF
!
!  tag uw, P2esin
!
      IF(kk == 662)THEN
         p2esin=var(i)
      END IF
!
!  tag ux, P2incl
!
      IF(kk == 663)THEN
         p2incl=var(i)
      END IF
!
!  tag uy, P2Omega
!
      IF(kk == 664)THEN
         p2omega=var(i)
      END IF
!
!  tag uz, P2Q
!
      IF(kk == 665)THEN
         p2q=var(i)
      END IF
!
!  tag ub, P2ratrad
!
      IF(kk == 641)THEN
         p2ratrad=var(i)
      END IF
!
!  planet 3 parameters
!
!  tag vj, P3tconj
!
      IF(kk == 681)THEN
         p3tconj=var(i)
      END IF
!
!  tag vt, P3period
!
      IF(kk == 691)THEN
         p3period=var(i)
      END IF
!
!  tag vu, P3T0
!
      IF(kk == 692)THEN
         p3t0=var(i)
      END IF
!
!  tag vv, P3ecos
!
      IF(kk == 693)THEN
         p3ecos=var(i)
      END IF
!
!  tag vw, P3esin
!
      IF(kk == 694)THEN
         p3esin=var(i)
      END IF
!
!  tag vx, P3incl
!
      IF(kk == 695)THEN
         p3incl=var(i)
      END IF
!
!  tag vy, P3Omega
!
      IF(kk == 696)THEN
         p3omega=var(i)
      END IF
!
!  tag vz, P3Q
!
      IF(kk == 697)THEN
         p3q=var(i)
      END IF
!
!  tag vb, P3ratrad
!
      IF(kk == 673)THEN
         p3ratrad=var(i)
      END IF
!
!  planet 4 parameters
!
!  tag wj, P4tconj
!
      IF(kk == 713)THEN
         p4tconj=var(i)
      END IF
!
!  tag wt, P4period
!
      IF(kk == 723)THEN
         p4period=var(i)
      END IF
!
!  tag wu, P4T0
!
      IF(kk == 724)THEN
         p4t0=var(i)
      END IF
!
!  tag wv, P4ecos
!
      IF(kk == 725)THEN
         p4ecos=var(i)
      END IF
!
!  tag ww, P4esin
!
      IF(kk == 726)THEN
         p4esin=var(i)
      END IF
!
!  tag wx, P4incl
!
      IF(kk == 727)THEN
         p4incl=var(i)
      END IF
!
!  tag wy, P4Omega
!
      IF(kk == 728)THEN
         p4omega=var(i)
      END IF
!
!  tag wz, P4Q
!
      IF(kk == 729)THEN
         p4q=var(i)
      END IF
!
!  tag wb, P4ratrad
!
      IF(kk == 705)THEN
         p4ratrad=var(i)
      END IF
!
!  planet 5 parameters
!
!  tag xj, P5tconj
!
      IF(kk == 745)THEN
         p5tconj=var(i)
      END IF
!
!  tag xt, P5period
!
      IF(kk == 755)THEN
         p5period=var(i)
      END IF
!
!  tag xu, P5T0
!
      IF(kk == 756)THEN
         p5t0=var(i)
      END IF
!
!  tag xv, P5ecos
!
      IF(kk == 757)THEN
         p5ecos=var(i)
      END IF
!
!  tag xw, P5esin
!
      IF(kk == 758)THEN
         p5esin=var(i)
      END IF
!
!  tag xx, P5incl
!
      IF(kk == 759)THEN
         p5incl=var(i)
      END IF
!
!  tag xy, P5Omega
!
      IF(kk == 760)THEN
         p5omega=var(i)
      END IF
!
!  tag xz, P5Q
!
      IF(kk == 761)THEN
         p5q=var(i)
      END IF
!
!  tag xb, P5ratrad
!
      IF(kk == 737)THEN
         p5ratrad=var(i)
      END IF
!
!  planet 6 parameters
!
!  tag sj, P6tconj
!
      IF(kk == 585)THEN
         p6tconj=var(i)
      END IF
!
!  tag st, P6period
!
      IF(kk == 595)THEN
         p6period=var(i)
      END IF
!
!  tag su, P6T0
!
      IF(kk == 596)THEN
         p6t0=var(i)
      END IF
!
!  tag sv, P6ecos
!
      IF(kk == 597)THEN
         p6ecos=var(i)
      END IF
!
!  tag sw, P6esin
!
      IF(kk == 598)THEN
         p6esin=var(i)
      END IF
!
!  tag sx, P6incl
!
      IF(kk == 599)THEN
         p6incl=var(i)
      END IF
!
!  tag sy, P6Omega
!
      IF(kk == 600)THEN
         p6omega=var(i)
      END IF
!
!  tag sz, P6Q
!
      IF(kk == 601)THEN
         p6q=var(i)
      END IF
!
!  tag sb, P6ratrad
!
      IF(kk == 577)THEN
         p6ratrad=var(i)
      END IF
!
!  planet 7 parameters
!
!  tag hj, P7tconj
!
      IF(kk == 233)THEN
         p7tconj=var(i)
      END IF
!
!  tag ht, P7period
!
      IF(kk == 243)THEN
         p7period=var(i)
      END IF
!
!  tag hu, P7T0
!
      IF(kk == 244)THEN
         p7t0=var(i)
      END IF
!
!  tag hv, P7ecos
!
      IF(kk == 245)THEN
         p7ecos=var(i)
      END IF
!
!  tag hw, P7esin
!
      IF(kk == 246)THEN
         p7esin=var(i)
      END IF
!
!  tag hx, P7incl
!
      IF(kk == 247)THEN
         p7incl=var(i)
      END IF
!
!  tag hy, P7Omega
!
      IF(kk == 248)THEN
         p7omega=var(i)
      END IF
!
!  tag hz, P7Q
!
      IF(kk == 249)THEN
         p7q=var(i)
      END IF
!
!  tag hb, P7ratrad
!
      IF(kk == 225)THEN
         p7ratrad=var(i)
      END IF
!
!  planet 8 parameters
!
!  tag kj, P8tconj
!
      IF(kk == 329)THEN
         p8tconj=var(i)
      END IF
!
!  tag kt, P8period
!
      IF(kk == 339)THEN
         p8period=var(i)
      END IF
!
!  tag ku, P8T0
!
      IF(kk == 340)THEN
         p8t0=var(i)
      END IF
!
!  tag kv, P8ecos
!
      IF(kk == 341)THEN
         p8ecos=var(i)
      END IF
!
!  tag kw, P8esin
!
      IF(kk == 342)THEN
         p8esin=var(i)
      END IF
!
!  tag kx, P8incl
!
      IF(kk == 343)THEN
         p8incl=var(i)
      END IF
!
!  tag ky, P8Omega
!
      IF(kk == 344)THEN
         p8omega=var(i)
      END IF
!
!  tag kz, P8Q
!
      IF(kk == 345)THEN
         p8q=var(i)
      END IF
!
!  tag kb, P8ratrad
!
      IF(kk == 42)THEN
         p8ratrad=var(i)
      END IF
!
!  tag s0, seasonal contamination season 0
!
      IF(kk == 1591)THEN
         contams0=var(i)
         IF(contams0 < 0.0_dp)contams0=0.0_dp
      END IF
!
!  tag s1, seasonal contamination season 1
!
      IF(kk == 1592)THEN
         contams1=var(i)
         IF(contams1 < 0.0_dp)contams1=0.0_dp
      END IF
!
!  tag s2, seasonal contamination season 2
!
      IF(kk == 1593)THEN
         contams2=var(i)
         IF(contams2 < 0.0_dp)contams2=0.0_dp
      END IF
!
!  tag s3, seasonal contamination s3
!
      IF(kk == 1594)THEN
         contams3=var(i)
         IF(contams3 < 0.0_dp)contams3=0.0_dp
      END IF
!
!  tag do, omegadot
!
      IF(kk == 110)THEN
         omegadot=var(i)
      END IF
!
!  tag g1, Tgrav1, gravity darkening exponent star 1
!
      IF(kk == 1208)THEN
         tgrav1=var(i)
      END IF
!
!  tag g2, Tgrav2, gravity darkening exponent star 2
!
      IF(kk == 1209)THEN
         tgrav2=var(i)
      END IF
!
!  tag tj, tertconj, third body conjunction
!
      IF(kk == 617)THEN
         tertconj=var(i)
      END IF
!
!  tag tt, tertperiod, third body perioe
!
      IF(kk == 627)THEN
         tertperiod=var(i)
      END IF
!
!  tag tu, tertT0, third body periastron
!
      IF(kk == 628)THEN
         tertt0=var(i)
      END IF
!
!  tag tv, tertecos, e*cos(omega) for body 3
!
      IF(kk == 629)THEN
         tertecos=var(i)
      END IF
!
!  tag tw, tertesin, e*sin(omega) for body 3
!
      IF(kk == 630)THEN
         tertesin=var(i)
      END IF
!
!  tag tx, tertincl, inclination for body 3
!
      IF(kk == 631)THEN
         tertincl=var(i)
      END IF
!
!  tag ty, tertOmega, Omega for body 3
!
      IF(kk == 632)THEN
         tertomega=var(i)
      END IF
!
!  tag tz, tertQ, third body mass ratio
!
      IF(kk == 633)THEN
         tertq=var(i)
      END IF
!
!  tag tb, tertratrad, third body radius ratio
!
      IF(kk == 609)THEN
         tertratrad=var(i)
      END IF
!
!  tag oc, ocose, e*cos(omega) for binary
!
      IF((isw29 > 0).AND.(kk == 450))THEN
         ocose=var(i)
      END IF
!
!  tag os, osine, e*sin(omega) for binary
!
      IF((isw29 > 0).AND.(kk == 466))THEN
         osine=var(i)
      END IF
!
!  tag co, contam, Kepler contamination
!
      IF(kk == 78)THEN
         contam=var(i)
      END IF
!
!  tag e1, beam1, Doppler beaming coefficient star 1
!
      IF(kk == 1144)THEN
         beam1=var(i)
      END IF
!
!  tag e2, beam2, Doppler beaming coefficient star 2
!
      IF(kk == 1145)THEN
         beam2=var(i)
      END IF
!
!  tag tc, Tconj, conjunction time for binary
!
      IF(kk == 610)THEN
         tconj=var(i)
      END IF
!
!  tag de, density for primary star
!
      IF(kk == 100)THEN
         density=var(i)
      END IF
!
!  tag ai, bigI, inclination of star 1's spin axis
!
      IF(kk == 8)THEN
         bigi=var(i)
      END IF
!
!  tag ab, bigbeta, position angle of star 1's spin axis
!
      IF(kk == 1)THEN
         bigbeta=var(i)
      END IF
!
!  tag dp, ecosw, phase difference of secondary eclipse
!
      IF(kk == 111)THEN
         ecosw=var(i)
      END IF
!
!  tag te, temprat, temperature ratio star 2 to star 1
!
      IF(kk == 612)THEN
         temprat=var(i)
      END IF
!
!  tag pm, primmass, primary mass in solar masses
!
      IF(kk == 492)THEN
         primmass=var(i)
         IF(primmass < 0.0_dp)primmass=0.0_dp
      END IF
!
!  tag pr, primrad, primary radius in solar masses
!
      IF(kk == 497)THEN
         primrad=var(i)
         IF(primrad < 0.0_dp)primrad=0.0_dp
      END IF
!
!  tag pk, primK, K-velocity of primary in km/sec
!
      IF(kk == 490)THEN
         primk=var(i)
         IF(primk < 0.0_dp)primk=0.0_dp
      END IF
!
!  tag ra, ratrad, ratio of star 1 radius to star 2 radius
!
      IF(kk == 544)THEN
         ratrad=var(i)
         IF(ratrad < 0.0_dp)ratrad=0.0_dp
      END IF
!
!  tag q1, frac1, fractional radius of star 1 (R_1/a)
!
      IF(kk == 1528)THEN
         frac1=var(i)
         IF(frac1 < 0.0_dp)frac1=0.0_dp
      END IF
!
!  tag q2, frac2, fractional radius of star 2 (R_2/a)
!
      IF(kk == 1529)THEN
         frac2=var(i)
!              if(frac2.lt.0.0d0)frac2=0.0d0
      END IF
!
!  tag l1, alb1, star 1 albedo
!
      IF(kk == 1368)THEN
         alb1=var(i)
         IF(alb1 < 0.0_dp)alb1=0.0_dp
      END IF
!
!  tag l2, alb2, star 2 albedo
!
      IF(kk == 1369)THEN
         alb2=var(i)
         IF(alb2 < 0.0_dp)alb2=0.0_dp
      END IF
!
!  tag pe, period, binary period in days
!
      IF(kk == 484)THEN
         period=var(i)
      END IF
!
!  tag t0, T0, time of periastron passage of binary
!
      IF(kk == 1623)THEN
         t0=var(i)
      END IF
!
!  tag d1, temperature factor spot 1 on disk
!
      IF(kk == 1112)THEN
         spotdparm(1,1)=var(i)
         IF(spotdparm(1,1) <= -10.0_dp)spotdparm(1,1)=-1.0_dp
      END IF
!
!  tag d5, temperature factor spot 2 on disk
!
      IF(kk == 1116)THEN
         spotdparm(2,1)=var(i)
         IF(spotdparm(2,1) <= -10.0_dp)spotdparm(2,1)=-1.0_dp
      END IF
!
!  tag d2, azimuth spot 1 on disk
!
      IF(kk == 1113)THEN
         spotdparm(1,2)=MOD(var(i),360.0_dp)
         spotdparm(1,2)=var(i)
      END IF
!
!  tag d6, azimuth spot 2 on disk
!
      IF(kk == 1117)THEN
         spotdparm(2,2)=MOD(var(i),360.0_dp)
         spotdparm(2,2)=var(i)
      END IF
!
!  tag d3, cutoff radius for spot 1 on disk
!
      IF(kk == 1114)THEN
         spotdparm(1,3)=var(i)
         IF(spotdparm(1,3) > 1.0_dp)spotdparm(1,3)=1.0_dp
         IF(spotdparm(1,3) < 0.0_dp)spotdparm(1,3)=0.0_dp
      END IF
!
!  tag d7, cutoff radius for spot 2 on disk
!
      IF(kk == 1118)THEN
         spotdparm(2,3)=var(i)
         IF(spotdparm(2,3) > 1.0_dp)spotdparm(2,3)=1.0_dp
         IF(spotdparm(2,3) < 0.0_dp)spotdparm(2,3)=0.0_dp
      END IF
!
!  tag d4, angular width of spot 1 on disk
!
      IF(kk == 1115)THEN
         spotdparm(1,4)=var(i)
         IF(spotdparm(1,4) < 0.0_dp)spotdparm(1,4)=0.0_dp
      END IF
!
!  tag d8, angular width of spot 2 on disk
!
      IF(kk == 1119)THEN
         spotdparm(2,4)=var(i)
         IF(spotdparm(2,4) < 0.0_dp)spotdparm(2,4)=0.0_dp
      END IF
!
!  tag c1, temperature factor spot 1, star 2
!
      IF(kk == 1080)THEN
         spot2parm(1,1)=var(i)
      END IF
!
!  tag c5,  temperature factor spot 2, star 2
!
      IF(kk == 1084)THEN
         spot2parm(2,1)=var(i)
      END IF
!
!  tag b1, temperature factor spot 1, star 1
!
      IF(kk == 1048)THEN
         spot1parm(1,1)=var(i)
      END IF
!
!  tag b5, temperature factor spot 2, star 1
!
      IF(kk == 1052)THEN
         spot1parm(2,1)=var(i)
      END IF
!
!  tag c2, latitude spot 1, star 2
!
      IF(kk == 1081)THEN
         spot2parm(1,2)=MOD(var(i),180.0_dp)
      END IF
!
!  tag c6, latitude spot 2, star 2
!
      IF(kk == 1085)THEN
         spot2parm(2,2)=MOD(var(i),180.0_dp)
      END IF
!
!  tag b2, latitude spot 1, star 1
!
      IF(kk == 1049)THEN
         spot1parm(1,2)=MOD(var(i),180.0_dp)
      END IF
!
!  tag b6,  latitude spot 2, star 1
!
      IF(kk == 1053)THEN
         spot1parm(2,2)=MOD(var(i),180.0_dp)
      END IF
!
!  tag c3, longitude spot 1, star 2
!
      IF(kk == 1082)THEN
         spot2parm(1,3)=MOD(var(i),360.0_dp)
      END IF
!
!  tag c7, longitude spot 2, star 2
!
      IF(kk == 1086)THEN
         spot2parm(2,3)=MOD(var(i),360.0_dp)
      END IF
!
!  tag b3, longitude spot 1, star 1
!
      IF(kk == 1050)THEN
         spot1parm(1,3)=MOD(var(i),360.0_dp)
      END IF
!
!  tag b7, longitude spot 2, star 1
!
      IF(kk == 1054)THEN
         spot1parm(2,3)=MOD(var(i),360.0_dp)
      END IF
!
!  tag c4, radius spot 1, star 2
!
      IF(kk == 1083)THEN
         spot2parm(1,4)=var(i)
      END IF
!
!  tag c8, radius spot 2, star 2
!
      IF(kk == 1087)THEN
         spot2parm(2,4)=var(i)
      END IF
!
!  tag b4, radius spot 1, star 1
!
      IF(kk == 1051)THEN
         spot1parm(1,4)=var(i)
      END IF
!
!  tag b8,  radius spot 2, star 1
!
      IF(kk == 1055)THEN
         spot1parm(2,4)=var(i)
      END IF
!
!  tag ps, pshift
!
      IF(kk == 498)THEN
         pshift=var(i)
         IF(pshift > 1.0_dp)pshift=1.0_dp
         IF(pshift < -1.0_dp)pshift=-1.0_dp
      END IF
!
!  tag in, finc, inclination of binary
!
      IF(kk == 269)THEN
         finc=var(i)
!              if(finc.gt.90.0)finc=90.0
         IF(finc < 0.0_dp)finc=0.0_dp
      END IF
!
!  tag ma, Q, mass ratio of binary
!
      IF(kk == 384)THEN
         q=var(i)
         IF(q <= 0.0_dp)q=0.001_dp
      END IF
!
!  tag ec, ecc, eccentricity of binary
!
      IF(kk == 130)THEN
         ecc=var(i)
         IF(ecc <= 0.0_dp)ecc=0.000_dp
         IF(ecc >= 1.0_dp)ecc=0.9999_dp
      END IF
!
!  tag ar, argper, argument of periastron of binary
!
      IF(kk == 17)THEN
         argper=var(i)
!              if(argper.le.0.0d0)argper=0.000d0
!              if(argper.gt.360.0d0)argper=360.0d0
      END IF
!
!  tag f1, fill1, Roche lobe filling factor star 1
!
      IF(kk == 1176)THEN
         fill1=var(i)
         IF(fill1 > 1.0_dp)fill1=1.0_dp
      END IF
!
!  tag ri, rinner, inner radius of accretion disk
!
      IF(kk == 552)THEN
         rinner=var(i)
         IF((rinner < fill2).AND.(teff2 > 0.0_dp))rinner=fill2
      END IF
!
!  tag f2, fill 2, Roche lobe filling factor star 2
!
      IF(kk == 1177)THEN
         fill2=var(i)
         IF(fill2 > 1.0_dp)fill2=1.0_dp
         IF(teff2 > 0.0_dp)rinner=fill2
         IF(fill2 < 0.0_dp)THEN
            fill2=0.000001_dp
            rinner=fill2
         END IF
      END IF
!
!  tag o1, omega1, spin rate of star 1
!
      IF(kk == 1464)THEN
         omega1=var(i)
      END IF
!
!  tag o2, omega2, spin rate of star 2
!
      IF(kk == 1465)THEN
         omega2=var(i)
      END IF
!
!  tag ro, router, outer radius of accretion disk
!
      IF(kk == 558)THEN
         router=var(i)
         IF(router > 1.0_dp)router=1.0_dp
      END IF
!
!  tag td, Tdisk, temperature of inner disk
!
      IF(kk == 611)THEN
         tdisk=var(i)
         IF(tdisk < 100.0_dp)tdisk=100.0_dp
      END IF
!
!  tag be, betarim, opening angle of disk
!
      IF(kk == 36)THEN
         betarim=var(i)
         IF(betarim < 0.0_dp)betarim=0.0_dp
      END IF
!
!  tag t1, Teff1, temperature of star 1
!
      IF(kk == 1624)THEN
         teff1=var(i)
         IF(teff1 < 100.0_dp)teff1=100.0_dp
      END IF
!
!  tag t2, Teff2, temperature of star 2
!
      IF(kk == 1625)THEN
         teff2=var(i)
         IF(teff2 < 100.0_dp)teff2=100.0_dp
      END IF
!
!  tag xi, xi, power-law exponent on disk temperature profile
!
      IF(kk == 744)THEN
         xi=var(i)
      END IF
!
!  tag lx, rLx, log of the X-ray luminosity
!
      IF(kk == 375)THEN
         rlx=var(i)
         IF(rlx < 0.0_dp)rlx=0.0_dp
      END IF
!
!  tag se, separ, binary semimajor axis
!
      IF(kk == 580)THEN
         separ=var(i)
         IF(separ < 0.01_dp)separ=0.01_dp
      END IF
!
!  tag ga, gamma, gamma-velocity of binary
!
      IF(kk == 192)THEN
         gamma=var(i)
      END IF
!
!  tag t3, Teff3, temperature of star 3
!
      IF(kk == 1626)THEN
         t3=var(i)
         IF(t3 < 0.01_dp)t3=0.01_dp
      END IF
!
!  tag t4, Teff4, temperature of star 4
!
      IF(kk == 1627)THEN
         sw80=var(i)
         IF(sw80 < 0.01_dp)sw80=0.01_dp
      END IF
!
!  tag t5, Teff5, temperature of star 5
!
      IF(kk == 1628)THEN
         sw81=var(i)
         IF(sw81 < 0.01_dp)sw81=0.01_dp
      END IF
!
!  tag g3, g3, log(g) of star 3
!
      IF(kk == 1210)THEN
         g3=var(i)
         IF(g3 < 0.01_dp)g3=0.01_dp
      END IF
!
!  tag sa, SA3, ratio of area of star 3 to area of star 1
!
      IF(kk == 576)THEN
         sa3=var(i)
      END IF
!
!   Here are the assignments for the limb darkening parameters.
!   use tags x1, x2, to x8 for the x-coefficient for star 1
!   and y1, y2, to y8 for the y-coefficient for star 1.
!
!   Use z1, z2, to z8 for the x-coefficient for star 2 and
!   use w1, w2, to w8 for the y-coefficient for star 2.
!
      IF(kk == 1752)dwavex(1,1)=var(i)
      IF(kk == 1753)dwavex(2,1)=var(i)
      IF(kk == 1754)dwavex(3,1)=var(i)
      IF(kk == 1755)dwavex(4,1)=var(i)
      IF(kk == 1756)dwavex(5,1)=var(i)
      IF(kk == 1757)dwavex(6,1)=var(i)
      IF(kk == 1758)dwavex(7,1)=var(i)
      IF(kk == 1759)dwavex(8,1)=var(i)
!
      IF(kk == 1784)dwavey(1,1)=var(i)
      IF(kk == 1785)dwavey(2,1)=var(i)
      IF(kk == 1786)dwavey(3,1)=var(i)
      IF(kk == 1787)dwavey(4,1)=var(i)
      IF(kk == 1788)dwavey(5,1)=var(i)
      IF(kk == 1789)dwavey(6,1)=var(i)
      IF(kk == 1790)dwavey(7,1)=var(i)
      IF(kk == 1791)dwavey(8,1)=var(i)
!
      IF(kk == 1816)dwavex(1,2)=var(i)
      IF(kk == 1817)dwavex(2,2)=var(i)
      IF(kk == 1818)dwavex(3,2)=var(i)
      IF(kk == 1819)dwavex(4,2)=var(i)
      IF(kk == 1820)dwavex(5,2)=var(i)
      IF(kk == 1821)dwavex(6,2)=var(i)
      IF(kk == 1822)dwavex(7,2)=var(i)
      IF(kk == 1823)dwavex(8,2)=var(i)
!
      IF(kk == 1720)dwavey(1,2)=var(i)
      IF(kk == 1721)dwavey(2,2)=var(i)
      IF(kk == 1722)dwavey(3,2)=var(i)
      IF(kk == 1723)dwavey(4,2)=var(i)
      IF(kk == 1724)dwavey(5,2)=var(i)
      IF(kk == 1725)dwavey(6,2)=var(i)
      IF(kk == 1726)dwavey(7,2)=var(i)
      IF(kk == 1727)dwavey(8,2)=var(i)
!
!   body 3 limb darkening, use tags m1 through m8
!   for the first coefficient, and tags n1 through
!   n8 for the second coefficient
!
      IF(kk == 1400)dwavex(1,3)=var(i)
      IF(kk == 1401)dwavex(2,3)=var(i)
      IF(kk == 1402)dwavex(3,3)=var(i)
      IF(kk == 1403)dwavex(4,3)=var(i)
      IF(kk == 1404)dwavex(5,3)=var(i)
      IF(kk == 1405)dwavex(6,3)=var(i)
      IF(kk == 1406)dwavex(7,3)=var(i)
      IF(kk == 1407)dwavex(8,3)=var(i)
!
      IF(kk == 1432)dwavey(1,3)=var(i)
      IF(kk == 1433)dwavey(2,3)=var(i)
      IF(kk == 1434)dwavey(3,3)=var(i)
      IF(kk == 1435)dwavey(4,3)=var(i)
      IF(kk == 1436)dwavey(5,3)=var(i)
      IF(kk == 1437)dwavey(6,3)=var(i)
      IF(kk == 1438)dwavey(7,3)=var(i)
      IF(kk == 1439)dwavey(8,3)=var(i)
!
!   body 4 limb darkening, use tags i1 through i8
!   for the first coefficient, and tags j1 through
!   j8 for the second coefficient
!
      IF(kk == 1272)dwavex(1,4)=var(i)
      IF(kk == 1273)dwavex(2,4)=var(i)
      IF(kk == 1274)dwavex(3,4)=var(i)
      IF(kk == 1275)dwavex(4,4)=var(i)
      IF(kk == 1276)dwavex(5,4)=var(i)
      IF(kk == 1277)dwavex(6,4)=var(i)
      IF(kk == 1278)dwavex(7,4)=var(i)
      IF(kk == 1279)dwavex(8,4)=var(i)
!
      IF(kk == 1304)dwavey(1,4)=var(i)
      IF(kk == 1305)dwavey(2,4)=var(i)
      IF(kk == 1306)dwavey(3,4)=var(i)
      IF(kk == 1307)dwavey(4,4)=var(i)
      IF(kk == 1308)dwavey(5,4)=var(i)
      IF(kk == 1309)dwavey(6,4)=var(i)
      IF(kk == 1310)dwavey(7,4)=var(i)
      IF(kk == 1311)dwavey(8,4)=var(i)
!
!   body 5 limb darkening, use tags k1 through k8
!   for the first coefficient, and tags p1 through
!   p8 for the second coefficient
!
      IF(kk == 1336)dwavex(1,5)=var(i)
      IF(kk == 1337)dwavex(2,5)=var(i)
      IF(kk == 1338)dwavex(3,5)=var(i)
      IF(kk == 1339)dwavex(4,5)=var(i)
      IF(kk == 1340)dwavex(5,5)=var(i)
      IF(kk == 1341)dwavex(6,5)=var(i)
      IF(kk == 1342)dwavex(7,5)=var(i)
      IF(kk == 1343)dwavex(8,5)=var(i)
!
      IF(kk == 1496)dwavey(1,5)=var(i)
      IF(kk == 1497)dwavey(2,5)=var(i)
      IF(kk == 1498)dwavey(3,5)=var(i)
      IF(kk == 1499)dwavey(4,5)=var(i)
      IF(kk == 1500)dwavey(5,5)=var(i)
      IF(kk == 1501)dwavey(6,5)=var(i)
      IF(kk == 1502)dwavey(7,5)=var(i)
      IF(kk == 1503)dwavey(8,5)=var(i)
!
   END DO
!
   RETURN
!
END SUBROUTINE assignvar
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE checkextra(nextra,extrastr,extralabel)
!
!   This routine will figure out what extra parameters are written
!   to the generation* files (e.g. ecc, argper, gamma1, gamma2, etc)
!   and will return the number of them, and strs for the files
!   and the graph labels.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nextra
   CHARACTER(LEN=50), INTENT(OUT)           :: extrastr(58)
   CHARACTER(LEN=50), INTENT(OUT)           :: extralabel(58)
!
   INTEGER, PARAMETER :: nmaxeclipse=2500
   INTEGER, PARAMETER :: nvmax=80
   INTEGER, PARAMETER :: ndatamax=405000
!
   REAL(KIND=dp)  :: fill1,fill2,omega1,omega2,dphase,q,finc,teff1,teff2
   REAL(KIND=dp)  :: tgrav1,tgrav2,betarim,rinner,router,tdisk,xi,alb1,alb2
   REAL(KIND=dp)  :: rlx,period,fm,separ,gamma,t3,g3,sa3,density,sw1,sw2
   REAL(KIND=dp)  :: sw3,t0,wave,dbolx,dboly,dwavex,dwavey,ecc,argper
   REAL(KIND=dp)  :: pshift,sw5,sw6,sw7,sw8,sw9,spot1parm,spot2parm,omega9
   REAL(KIND=dp)  :: spotdparm,primmass,primk,primrad,ratrad,frac1,frac2
   REAL(KIND=dp)  :: ecosw,temprat,bigi,bigbeta,sw23,sw24,powercoeff,sw25
   REAL(KIND=dp)  :: sw26,sw27,sw28,sw29,sw30,contam,tconj,beam1,beam2
   REAL(KIND=dp)  :: ocose,osine,omegadot,contams0,contams1,contams2
   REAL(KIND=dp)  :: contams3,sw47,sw48,sw49,sw80,sw81,sw82,sw83,sw84,sw85
   REAL(KIND=dp)  :: sw86,sw87,sw88,sw89,sdarkint1,sdarkint2,sdarkint3
   REAL(KIND=dp)  :: sdarkint4,sdarkint5,tertperiod,tertt0,tertecos,omega8
   REAL(KIND=dp)  :: tertesin,tertincl,tertomega,tertq,tertconj,tertratrad
   REAL(KIND=dp)  :: hh,sw72,sw73,p2tconj,p2period,p2t0,p2ecos,p2esin
   REAL(KIND=dp)  :: p2incl,p2omega,p2q,p2ratrad,p3tconj,p3period,p3t0
   REAL(KIND=dp)  :: p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,p4tconj
   REAL(KIND=dp)  :: p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,omega7
   REAL(KIND=dp)  :: p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl
   REAL(KIND=dp)  :: p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos
   REAL(KIND=dp)  :: p6esin,p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period
   REAL(KIND=dp)  :: p7t0,p7ecos,p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj
   REAL(KIND=dp)  :: p8period,p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad
   REAL(KIND=dp)  :: secmass,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,t6,g6,omega5
   REAL(KIND=dp)  :: sdarkint6,g7,t7,sdarkint7,t8,g8,sdarkint8,omega3
   REAL(KIND=dp)  :: sdarkint9,sdarkint10,t9,g9,t10,g10,secrad,omega4
   REAL(KIND=dp)  :: radsum,raddiff,masssum,massdiff,p1ptc,p1mtc,p2ptc
   REAL(KIND=dp)  :: p2mtc,p5ptc,p5mtc,p6ptc,p6mtc,p7ptc,p7mtc,p8ptc,p8mtc
   REAL(KIND=dp)  :: p3ptc,p3mtc,pbptc,pbmtc,p4ptc,p4mtc,bigi4,bigbeta4
   REAL(KIND=dp)  :: bin2masssum,bin2massdiff,bin2q,bin2radsum,bin2raddiff
   REAL(KIND=dp)  :: bin2ratrad,bigi2,bigbeta2,bigi3,bigbeta3,omega6
   REAL(KIND=dp)  :: omega10,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,bin2r4
   REAL(KIND=dp)  :: sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin
   REAL(KIND=dp)  :: sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin
   REAL(KIND=dp)  :: sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin
   REAL(KIND=dp)  :: angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3
   REAL(KIND=dp)  :: angsum4,angdiff4,angsum5,angdiff5,angsum6,angdiff6
   REAL(KIND=dp)  :: angsum7,angdiff7,angsum8,angdiff8,fillsum,filldiff
   REAL(KIND=dp)  :: binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc
   REAL(KIND=dp)  :: tbinoff,t1off,t2off,t3off,t4off,t5off,t6off,t7off,t8off
   REAL(KIND=dp)  :: var,vstart,vstep,obv,eobv,tesscontam,tessbin
   REAL(KIND=dp)  :: obsttimes,obsterr
!
   INTEGER :: nalph1,nbet1,nalph2,nbet2,ntheta,nradius,nref,idraw
   INTEGER :: iecheck,iidint,iatm,ism1,icnu,icnb,icnv,icnr,icni
   INTEGER :: icnj,icnh,icnk,irvfilt,isw1,isw2,isw3,isw4,ilaw
   INTEGER :: ikeep,isynch,isw5,isw6,isw7,isw8,isw9,idark1,idark2
   INTEGER :: isw12,isw13,isw21,isw22,isw23,isw24,isw25,isw26,isw27
   INTEGER :: isw28,isw29,isw30,isw31,isw32,isw33,isw34,isw80,isw81
   INTEGER :: isw82,isw83,isw84,isw85,isw86,isw87,isw88,isw89,i
   INTEGER :: nalph3,nbet3,itconj,it1,it2,it3,it4,isw100,nseg,isvel4
   INTEGER :: iunit,kkkk,imag,iargper, isvel1,isvel2,isvel3
   INTEGER :: icnarray,nobscycle,kk,ifixgamma,tessfilt
   INTEGER :: icnvrt,nvar,nstep,nobv,icnrv1array,iversion
   INTEGER :: icnrv2array,icnrv3array,icnrv4array,icnrv5array
!
   CHARACTER(LEN=40) :: bdatafile,vdatafile,rdatafile,idatafile
   CHARACTER(LEN=40) :: udatafile,hdatafile,kdatafile,rv1file
   CHARACTER(LEN=40) :: rv2file,sobv(19),svar(nvmax),jdatafile
   CHARACTER(LEN=40) :: rv3file,rv4file,rv5file
!
   DIMENSION wave(8),dbolx(8,2),dboly(8,2),dwavex(8,10)
   DIMENSION nobscycle(40),obsttimes(40,nmaxeclipse),obv(19)
   DIMENSION obsterr(40,nmaxeclipse),icnarray(40),eobv(19)
   DIMENSION var(nvmax),vstart(nvmax),vstep(nvmax),nstep(nvmax)
   DIMENSION spot1parm(2,4),spot2parm(2,4),spotdparm(2,4)
   DIMENSION powercoeff(8,9),dwavey(8,10),sdarkint7(8)
   DIMENSION sdarkint1(8),sdarkint2(8),sdarkint3(8)
   DIMENSION sdarkint4(8),sdarkint5(8),sdarkint6(8)
   DIMENSION sdarkint8(8),sdarkint9(8),sdarkint10(8)
   DIMENSION icnrv1array(5),icnrv2array(5),icnrv3array(5)
   DIMENSION icnrv4array(5),icnrv5array(5)
!
   kkkk=0
   isw24=0
!
   CALL newgetinput(kkkk,nalph1,nalph2,nalph3,nbet1,nbet2,nbet3,  &
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
   CALL getloopopt(udatafile,bdatafile,vdatafile,rdatafile,  &
      idatafile,jdatafile,hdatafile,kdatafile,rv1file,rv2file,  &
      nvmax,nvar,svar,var,vstart,vstep,nstep,nobv,sobv,obv,eobv,  &
      rv3file,rv4file,rv5file)
!
   ifixgamma=isw4
!
   isvel1=0
   isvel2=0
   isvel3=0
   isvel4=0
   IF(icnvrt(rv1file(1:2)) /= 430)isvel1=99
   IF(icnvrt(rv2file(1:2)) /= 430)isvel2=99
   IF(icnvrt(rv3file(1:2)) /= 430)isvel3=99
   IF(icnvrt(rv4file(1:2)) /= 430)isvel4=99
!
   kk=0
   iargper=0
   DO i=1,nvar
      kk=icnvrt(svar(i)(1:2))
      IF(kk == 111)iargper=99
   END DO
!
!  If we are fitting eclipse times ((isw30.ge.3).and.(isw23.eq.1))
!  then load the data
!
!   Initialize the icnarray to 430 values (no eclipse times to be fit).
!
   DO i=1,40
      icnarray(i)=430
   END DO
!
   IF((isw30 >= 3).AND.(isw23 >= 1))THEN
      CALL loadtimes(icnarray,nobscycle,obsttimes,obsterr,nmaxeclipse)
   END IF
!
   nextra=0
!
!  ecosw specified?
!
   IF(iargper > 0)THEN
      nextra=nextra+1
      extrastr(nextra)='argper2deg'
      extralabel(nextra)='binary argument of periastron (degrees)'
   END IF
!
!   fitting for e*cos(omega) and e*sin(omega) ?
!
   IF(isw29 > 0)THEN
      nextra=nextra+1
      extrastr(nextra)='ecc2'
      extralabel(nextra)='binary eccentricity'
      nextra=nextra+1
      extrastr(nextra)='argper2deg'
      extralabel(nextra)='binary argument of periastron (degrees)'
   END IF
!
!  Check for fitting RV curves 1 through 4.
!  If ifixgamma is 0, 1, or 2, then we have
!  at most 1 gamma velocity per star.  If ifixgamma
!  is 10, 11, or 12 then we can have up to 5 gamma
!  velocities per star.
!
   IF(ifixgamma < 10)THEN
      IF(isvel1 > 0)THEN
         nextra=nextra+1
         extrastr(nextra)='gam1'
         extralabel(nextra)='gamma velocity, star 1 (km/sec)'
      END IF
!
      IF(isvel2 > 0)THEN
         nextra=nextra+1
         extrastr(nextra)='gam2'
         extralabel(nextra)='gamma velocity, star 2 (km/sec)'
      END IF
!
      IF(isvel3 > 0)THEN
         nextra=nextra+1
         extrastr(nextra)='gam3'
         extralabel(nextra)='gamma velocity, star 3 (km/sec)'
      END IF
!
      IF(isvel4 > 0)THEN
         nextra=nextra+1
         extrastr(nextra)='gam4'
         extralabel(nextra)='gamma velocity, star 4 (km/sec)'
      END IF
   END IF
!
!  Case when ifixgamma is 10 or more
!
   IF(ifixgamma >= 10)THEN
      CALL readrvdata(icnrv1array,icnrv2array,icnrv3array,  &
         icnrv4array,icnrv5array,ndatamax)
!
!  star 1 gamma velocities
!
      IF(icnrv1array(1) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam1A'
         extralabel(nextra)='gamma velocity, star 1, set A (km/sec)'
      END IF
      IF(icnrv1array(2) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam1B'
         extralabel(nextra)='gamma velocity, star 1, set B (km/sec)'
      END IF
      IF(icnrv1array(3) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam1C'
         extralabel(nextra)='gamma velocity, star 1, set C (km/sec)'
      END IF
      IF(icnrv1array(4) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam1D'
         extralabel(nextra)='gamma velocity, star 1, set D (km/sec)'
      END IF
      IF(icnrv1array(5) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam1E'
         extralabel(nextra)='gamma velocity, star 1, set E (km/sec)'
      END IF
!
!  star 2 gamma velocities
!
      IF(icnrv2array(1) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam2A'
         extralabel(nextra)='gamma velocity, star 2, set A (km/sec)'
      END IF
      IF(icnrv2array(2) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam2B'
         extralabel(nextra)='gamma velocity, star 2, set B (km/sec)'
      END IF
      IF(icnrv2array(3) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam2C'
         extralabel(nextra)='gamma velocity, star 2, set C (km/sec)'
      END IF
      IF(icnrv2array(4) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam2D'
         extralabel(nextra)='gamma velocity, star 2, set D (km/sec)'
      END IF
      IF(icnrv2array(5) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam2E'
         extralabel(nextra)='gamma velocity, star 2, set E (km/sec)'
      END IF
!
!  star 3 gamma velocities
!
      IF(icnrv3array(1) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam3A'
         extralabel(nextra)='gamma velocity, star 3, set A (km/sec)'
      END IF
      IF(icnrv3array(2) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam3B'
         extralabel(nextra)='gamma velocity, star 3, set B (km/sec)'
      END IF
      IF(icnrv3array(3) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam3C'
         extralabel(nextra)='gamma velocity, star 3, set C (km/sec)'
      END IF
      IF(icnrv3array(4) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam3D'
         extralabel(nextra)='gamma velocity, star 3, set D (km/sec)'
      END IF
      IF(icnrv3array(5) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam3E'
         extralabel(nextra)='gamma velocity, star 3, set E (km/sec)'
      END IF
!
!  star 4 gamma velocities
!
      IF(icnrv4array(1) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam4A'
         extralabel(nextra)='gamma velocity, star 4, set A (km/sec)'
      END IF
      IF(icnrv4array(2) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam4B'
         extralabel(nextra)='gamma velocity, star 4, set B (km/sec)'
      END IF
      IF(icnrv4array(3) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam4C'
         extralabel(nextra)='gamma velocity, star 4, set C (km/sec)'
      END IF
      IF(icnrv4array(4) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam4D'
         extralabel(nextra)='gamma velocity, star 4, set D (km/sec)'
      END IF
      IF(icnrv4array(5) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam4E'
         extralabel(nextra)='gamma velocity, star 4, set E (km/sec)'
      END IF
!
!  star 5 gamma velocities
!
      IF(icnrv5array(1) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam5A'
         extralabel(nextra)='gamma velocity, star 5, set A (km/sec)'
      END IF
      IF(icnrv5array(2) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam5B'
         extralabel(nextra)='gamma velocity, star 5, set B (km/sec)'
      END IF
      IF(icnrv5array(3) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam5C'
         extralabel(nextra)='gamma velocity, star 5, set C (km/sec)'
      END IF
      IF(icnrv5array(4) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam5D'
         extralabel(nextra)='gamma velocity, star 5, set D (km/sec)'
      END IF
      IF(icnrv5array(5) /= 430)THEN
         nextra=nextra+1
         extrastr(nextra)='gam5E'
         extralabel(nextra)='gamma velocity, star 5, set E (km/sec)'
      END IF
!
   END IF
!
!  pulsar a*sin(i)
!
   IF(sw5 > 0.0_dp)THEN
      nextra=nextra+1
      extrastr(nextra)='asini'
      extralabel(nextra)='a_x*sin(i) (sec)'
   END IF
!
!  if ecc > 0, get the times of periastron passage
!  and conjunction
!
   IF((iargper > 0).OR.(isw29 > 0))THEN
      nextra=nextra+1
      extrastr(nextra)='Tperi2'
      extralabel(nextra)='Time of binary periastron passage'
      nextra=nextra+1
      extrastr(nextra)='Tsupconj2'
      extralabel(nextra)='Time of superior conjunction of star 1'
      nextra=nextra+1
      extrastr(nextra)='Tinfconj2'
      extralabel(nextra)='Time of inferior conjunction of star 1'
   END IF
!
!  If Nbody is 3 or more and if inform is 2 or more, then
!  the true anomaly, the mean anamoly, and the mean longitude
!  or each orbit is recorded
!
   IF((isw30 >= 3).AND.(it2 >= 2))THEN
      nextra=nextra+1
      extrastr(nextra)='TrueAn1'
      extralabel(nextra)='True anomaly, orbit 1 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn1'
      extralabel(nextra)='Mean anomaly, orbit 1 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong1'
      extralabel(nextra)='Mean longitude, orbit 1 (deg)'
!
      nextra=nextra+1
      extrastr(nextra)='TrueAn2'
      extralabel(nextra)='True anomaly, orbit 2 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn2'
      extralabel(nextra)='Mean anomaly, orbit 2 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong2'
      extralabel(nextra)='Mean longitude, orbit 2 (deg)'
!
   END IF
!
   IF((isw30 >= 4).AND.(it2 >= 2))THEN
      nextra=nextra+1
      extrastr(nextra)='TrueAn3'
      extralabel(nextra)='True anomaly, orbit 3 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn1'
      extralabel(nextra)='Mean anomaly, orbit 3 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong1'
      extralabel(nextra)='Mean longitude, orbit 3 (deg)'
!
   END IF
!
   IF((isw30 >= 5).AND.(it2 >= 2))THEN
      nextra=nextra+1
      extrastr(nextra)='TrueAn4'
      extralabel(nextra)='True anomaly, orbit 4 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn4'
      extralabel(nextra)='Mean anomaly, orbit 4 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong4'
      extralabel(nextra)='Mean longitude, orbit 4 (deg)'
!
   END IF
!
   IF((isw30 >= 6).AND.(it2 >= 2))THEN
      nextra=nextra+1
      extrastr(nextra)='TrueAn5'
      extralabel(nextra)='True anomaly, orbit 5 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn5'
      extralabel(nextra)='Mean anomaly, orbit 5 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong5'
      extralabel(nextra)='Mean longitude, orbit 5 (deg)'
!
   END IF
!
   IF((isw30 >= 7).AND.(it2 >= 2))THEN
      nextra=nextra+1
      extrastr(nextra)='TrueAn6'
      extralabel(nextra)='True anomaly, orbit 6 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn6'
      extralabel(nextra)='Mean anomaly, orbit 6 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong6'
      extralabel(nextra)='Mean longitude, orbit 6 (deg)'
!
   END IF
!
   IF((isw30 >= 8).AND.(it2 >= 2))THEN
      nextra=nextra+1
      extrastr(nextra)='TrueAn7'
      extralabel(nextra)='True anomaly, orbit 7 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn7'
      extralabel(nextra)='Mean anomaly, orbit 7 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong7'
      extralabel(nextra)='Mean longitude, orbit 7 (deg)'
!
   END IF
!
   IF((isw30 >= 9).AND.(it2 >= 2))THEN
      nextra=nextra+1
      extrastr(nextra)='TrueAn8'
      extralabel(nextra)='True anomaly, orbit 8 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn8'
      extralabel(nextra)='Mean anomaly, orbit 8 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong8'
      extralabel(nextra)='Mean longitude, orbit 8 (deg)'
!
   END IF
!
   IF((isw30 >= 10).AND.(it2 >= 2))THEN
      nextra=nextra+1
      extrastr(nextra)='TrueAn9'
      extralabel(nextra)='True anomaly, orbit 9 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanAn9'
      extralabel(nextra)='Mean anomaly, orbit 9 (deg)'
      nextra=nextra+1
      extrastr(nextra)='MeanLong9'
      extralabel(nextra)='Mean longitude, orbit 9 (deg)'
!
   END IF

   RETURN
!
END SUBROUTINE checkextra
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE checklog(it1)
!
!   Will read the ELC.inp file and will return
!   the value of it1, which determines how the
!   planet mass ratios are used.
!
   USE accur

   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                  :: it1
!
   REAL(KIND=dp)   :: fill1,fill2,omega1,omega2,dphase,q,finc,teff1,teff2
   REAL(KIND=dp)   :: tgrav1,tgrav2,betarim,rinner,router,tdisk,xi,alb1,alb2
   REAL(KIND=dp)   :: rlx,period,fm,separ,gamma,t3,g3,sa3,density,sw1,sw2
   REAL(KIND=dp)   :: sw3,t0,wave,dbolx,dboly,dwavex,dwavey,ecc,argper
   REAL(KIND=dp)   :: pshift,sw5,sw6,sw7,sw8,sw9,spot1parm,spot2parm,omega9
   REAL(KIND=dp)   :: spotdparm,primmass,primk,primrad,ratrad,frac1,frac2
   REAL(KIND=dp)   :: ecosw,temprat,bigi,bigbeta,sw23,sw24,powercoeff,sw25
   REAL(KIND=dp)   :: sw26,sw27,sw28,sw29,sw30,contam,tconj,beam1,beam2
   REAL(KIND=dp)   :: ocose,osine,omegadot,contams0,contams1,contams2
   REAL(KIND=dp)   :: contams3,sw47,sw48,sw49,sw80,sw81,sw82,sw83,sw84,sw85
   REAL(KIND=dp)   :: sw86,sw87,sw88,sw89,sdarkint1,sdarkint2,sdarkint3
   REAL(KIND=dp)   :: sdarkint4,sdarkint5,tertperiod,tertt0,tertecos,omega8
   REAL(KIND=dp)   :: tertesin,tertincl,tertomega,tertq,tertconj,tertratrad
   REAL(KIND=dp)   :: hh,sw72,sw73,p2tconj,p2period,p2t0,p2ecos,p2esin
   REAL(KIND=dp)   :: p2incl,p2omega,p2q,p2ratrad,p3tconj,p3period,p3t0
   REAL(KIND=dp)   :: p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,p4tconj
   REAL(KIND=dp)   :: p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,omega7
   REAL(KIND=dp)   :: p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl
   REAL(KIND=dp)   :: p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos
   REAL(KIND=dp)   :: p6esin,p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period
   REAL(KIND=dp)   :: p7t0,p7ecos,p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj
   REAL(KIND=dp)   :: p8period,p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad
   REAL(KIND=dp)   :: secmass,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,t6,g6,omega5
   REAL(KIND=dp)   :: sdarkint6,g7,t7,sdarkint7,t8,g8,sdarkint8,omega3
   REAL(KIND=dp)   :: sdarkint9,sdarkint10,t9,g9,t10,g10,secrad,omega4
   REAL(KIND=dp)   :: radsum,raddiff,masssum,massdiff,p1ptc,p1mtc,p2ptc
   REAL(KIND=dp)   :: p2mtc,p5ptc,p5mtc,p6ptc,p6mtc,p7ptc,p7mtc,p8ptc,p8mtc
   REAL(KIND=dp)   :: p3ptc,p3mtc,pbptc,pbmtc,p4ptc,p4mtc,bigi4,bigbeta4
   REAL(KIND=dp)   :: bin2masssum,bin2massdiff,bin2q,bin2radsum,bin2raddiff
   REAL(KIND=dp)   :: bin2ratrad,bigi2,bigbeta2,bigi3,bigbeta3,omega6
   REAL(KIND=dp)   :: omega10,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,bin2r4
   REAL(KIND=dp)   :: sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin
   REAL(KIND=dp)   :: sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin
   REAL(KIND=dp)   :: sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin
   REAL(KIND=dp)   :: angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3
   REAL(KIND=dp)   :: angsum4,angdiff4,angsum5,angdiff5,angsum6,angdiff6
   REAL(KIND=dp)   :: angsum7,angdiff7,angsum8,angdiff8,fillsum,filldiff
   REAL(KIND=dp)   :: binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc
   REAL(KIND=dp)   :: tbinoff,t1off,t2off,t3off,t4off,t5off,t6off,t7off,t8off
   REAL(KIND=dp)   :: tesscontam,tessbin
!
   INTEGER :: nalph1,nbet1,nalph2,nbet2,ntheta,nradius,nref,idraw
   INTEGER :: iecheck,iidint,iatm,ism1,icnu,icnb,icnv,icnr,icni
   INTEGER :: icnj,icnh,icnk,irvfilt,isw1,isw2,isw3,isw4,ilaw
   INTEGER :: ikeep,isynch,isw5,isw6,isw7,isw8,isw9,idark1,idark2
   INTEGER :: isw12,isw13,isw21,isw22,isw23,isw24,isw25,isw26,isw27
   INTEGER :: isw28,isw29,isw30,isw31,isw32,isw33,isw34,isw80,isw81
   INTEGER :: isw82,isw83,isw84,isw85,isw86,isw87,isw88,isw89
   INTEGER :: nalph3,nbet3,itconj, it2,it3,it4,isw100,nseg
   INTEGER :: iunit,kkkk,imag,iversion,tessfilt
!
   DIMENSION wave(8),dbolx(8,2),dboly(8,2),dwavex(8,10)
   DIMENSION spot1parm(2,4),spot2parm(2,4),spotdparm(2,4)
   DIMENSION powercoeff(8,9),dwavey(8,10),sdarkint7(8)
   DIMENSION sdarkint1(8),sdarkint2(8),sdarkint3(8)
   DIMENSION sdarkint4(8),sdarkint5(8),sdarkint6(8)
   DIMENSION sdarkint8(8),sdarkint9(8),sdarkint10(8)
!
   kkkk=0
   isw24=0
!
   CALL newgetinput(kkkk,nalph1,nalph2,nalph3,nbet1,nbet2,nbet3,  &
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
      t3off,t4off,t5off,t6off,t7off,t8off,iversion,tesscontam,  &
      tessfilt,tessbin)
!
   RETURN
!
END SUBROUTINE checklog
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE checkratio(isw24)
!
!   Will read the ELC.inp file and will check to see
!   if the user requested to write the ELCratio.??????
!   files.  If so, set isw24 > 0
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: isw24
!
   REAL(KIND=dp)  :: fill1,fill2,omega1,omega2,dphase,q,finc,teff1,teff2
   REAL(KIND=dp)  :: tgrav1,tgrav2,betarim,rinner,router,tdisk,xi,alb1,alb2
   REAL(KIND=dp)  :: rlx,period,fm,separ,gamma,t3,g3,sa3,density,sw1,sw2
   REAL(KIND=dp)  :: sw3,t0,wave,dbolx,dboly,dwavex,dwavey,ecc,argper
   REAL(KIND=dp)  :: pshift,sw5,sw6,sw7,sw8,sw9,spot1parm,spot2parm,omega9
   REAL(KIND=dp)  :: spotdparm,primmass,primk,primrad,ratrad,frac1,frac2
   REAL(KIND=dp)  :: ecosw,temprat,bigi,bigbeta,sw23,sw24,powercoeff,sw25
   REAL(KIND=dp)  :: sw26,sw27,sw28,sw29,sw30,contam,tconj,beam1,beam2
   REAL(KIND=dp)  :: ocose,osine,omegadot,contams0,contams1,contams2
   REAL(KIND=dp)  :: contams3,sw47,sw48,sw49,sw80,sw81,sw82,sw83,sw84,sw85
   REAL(KIND=dp)  :: sw86,sw87,sw88,sw89,sdarkint1,sdarkint2,sdarkint3
   REAL(KIND=dp)  :: sdarkint4,sdarkint5,tertperiod,tertt0,tertecos,omega8
   REAL(KIND=dp)  :: tertesin,tertincl,tertomega,tertq,tertconj,tertratrad
   REAL(KIND=dp)  :: hh,sw72,sw73,p2tconj,p2period,p2t0,p2ecos,p2esin
   REAL(KIND=dp)  :: p2incl,p2omega,p2q,p2ratrad,p3tconj,p3period,p3t0
   REAL(KIND=dp)  :: p3ecos,p3esin,p3incl,p3omega,p3q,p3ratrad,p4tconj
   REAL(KIND=dp)  :: p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,omega7
   REAL(KIND=dp)  :: p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl
   REAL(KIND=dp)  :: p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos
   REAL(KIND=dp)  :: p6esin,p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period
   REAL(KIND=dp)  :: p7t0,p7ecos,p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj
   REAL(KIND=dp)  :: p8period,p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad
   REAL(KIND=dp)  :: secmass,rk3,rk4,rk5,rk6,rk7,rk8,rk9,rk10,t6,g6,omega5
   REAL(KIND=dp)  :: sdarkint6,g7,t7,sdarkint7,t8,g8,sdarkint8,omega3
   REAL(KIND=dp)  :: sdarkint9,sdarkint10,t9,g9,t10,g10,secrad,omega4
   REAL(KIND=dp)  :: radsum,raddiff,masssum,massdiff,p1ptc,p1mtc,p2ptc
   REAL(KIND=dp)  :: p2mtc,p5ptc,p5mtc,p6ptc,p6mtc,p7ptc,p7mtc,p8ptc,p8mtc
   REAL(KIND=dp)  :: p3ptc,p3mtc,pbptc,pbmtc,p4ptc,p4mtc,bigi4,bigbeta4
   REAL(KIND=dp)  :: bin2masssum,bin2massdiff,bin2q,bin2radsum,bin2raddiff
   REAL(KIND=dp)  :: bin2ratrad,bigi2,bigbeta2,bigi3,bigbeta3,omega6
   REAL(KIND=dp)  :: omega10,fracsum,fracdiff,bin2m3,bin2m4,bin2r3,bin2r4
   REAL(KIND=dp)  :: sqecos,sqesin,sqtertecos,sqtertesin,sqp2ecos,sqp2esin
   REAL(KIND=dp)  :: sqp3ecos,sqp3esin,sqp4ecos,sqp4esin,sqp5ecos,sqp5esin
   REAL(KIND=dp)  :: sqp6ecos,sqp6esin,sqp7ecos,sqp7esin,sqp8ecos,sqp8esin
   REAL(KIND=dp)  :: angsum1,angdiff1,angsum2,angdiff2,angsum3,angdiff3
   REAL(KIND=dp)  :: angsum4,angdiff4,angsum5,angdiff5,angsum6,angdiff6
   REAL(KIND=dp)  :: angsum7,angdiff7,angsum8,angdiff8,fillsum,filldiff
   REAL(KIND=dp)  :: binqtc,p1qtc,p2qtc,p3qtc,p4qtc,p5qtc,p6qtc,p7qtc,p8qtc
   REAL(KIND=dp)  :: tbinoff,t1off,t2off,t3off,t4off,t5off,t6off,t7off,t8off
   REAL(KIND=dp)  :: tesscontam,tessbin
!
   INTEGER :: nalph1,nbet1,nalph2,nbet2,ntheta,nradius,nref,idraw
   INTEGER :: iecheck,iidint,iatm,ism1,icnu,icnb,icnv,icnr,icni
   INTEGER :: icnj,icnh,icnk,irvfilt,isw1,isw2,isw3,isw4,ilaw
   INTEGER :: ikeep,isynch,isw5,isw6,isw7,isw8,isw9,idark1,idark2
   INTEGER :: isw12,isw13,isw21,isw22,isw23, isw25,isw26,isw27
   INTEGER :: isw28,isw29,isw30,isw31,isw32,isw33,isw34,isw80,isw81
   INTEGER :: isw82,isw83,isw84,isw85,isw86,isw87,isw88,isw89
   INTEGER :: nalph3,nbet3,itconj,it1,it2,it3,it4,isw100,nseg
   INTEGER :: iunit,kkkk,imag,iversion,tessfilt
!
   DIMENSION wave(8),dbolx(8,2),dboly(8,2),dwavex(8,10)
   DIMENSION spot1parm(2,4),spot2parm(2,4),spotdparm(2,4)
   DIMENSION powercoeff(8,9),dwavey(8,10),sdarkint7(8)
   DIMENSION sdarkint1(8),sdarkint2(8),sdarkint3(8)
   DIMENSION sdarkint4(8),sdarkint5(8),sdarkint6(8)
   DIMENSION sdarkint8(8),sdarkint9(8),sdarkint10(8)
!
   kkkk=0
   isw24=0
!
   CALL newgetinput(kkkk,nalph1,nalph2,nalph3,nbet1,nbet2,nbet3,  &
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
   RETURN
!
END SUBROUTINE checkratio
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE cholfac(nvar,cholc,cholb)
!
!   Computes the Cholesky factorization of the
!   input matrix CholC (Nvar by Nvar).
!
   USE accur

   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvar
   REAL(KIND=dp), INTENT(IN)                :: cholc(nvar,nvar)
   REAL(KIND=dp), INTENT(OUT)               :: cholb(nvar,nvar)
!
   REAL(KIND=dp)  :: ll(100,100),sum1,sum2
!
   INTEGER :: i,j, k
!
!   initialize the LL matrix
!
   DO i=1,100
      DO j=1,100
         ll(i,j)=0.0_dp
      END DO
   END DO
   ll(1,1)=SQRT(cholc(1,1))
   DO i=2,nvar
      ll(i,1)=cholc(i,1)/ll(1,1)
   END DO
!
   DO i=1,nvar
      DO k=1,i
         sum1=0.0_dp
         sum2=0.0_dp
         DO j=1,k-1
            IF(i == k)THEN
               sum1=sum1+(ll(k,j)*ll(k,j))
               ll(k,k)=(SQRT(cholc(k,k)-sum1))
            ELSE IF(i > k)THEN
               sum2=sum2+(ll(i,j)*ll(k,j))
               ll(i,k)=(1/ll(k,k))*(cholc(i,k)-sum2)
            ELSE
               ll(i,k)=0
            END IF
         END DO
      END DO
   END DO
!
   DO i=1,nvar
      DO j=1,nvar
         cholb(i,j)=ll(j,i)
      END DO
   END DO
!
   RETURN
!
END SUBROUTINE cholfac
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE clipstring(instring,outstring,lengthout)
!
!   Will remove leading spaces from the input string
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: instring
   CHARACTER (LEN=*), INTENT(OUT)           :: outstring
   INTEGER, INTENT(OUT)                     :: lengthout
!
   INTEGER :: i,icount,k1
!
   outstring=instring
   icount=0
   k1=LEN_TRIM(instring)
   DO i=1,k1
      IF(instring(i:i) /= ' ')THEN
         icount=icount+1
         outstring(icount:icount)=instring(i:i)
      END IF
   END DO
!
   DO i=icount+1,k1
      outstring(i:i)=' '
   END DO
!
   lengthout=LEN_TRIM(outstring)
!
   RETURN
!
END SUBROUTINE clipstring
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE colortable(itable)
!
!   Loads color table for color plots of stars
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: itable
!
   REAL(KIND=pg)  ::  cr,cg,cb
!
   INTEGER :: i
   INTEGER :: icr_01(256),icg_01(256),icb_01(256)
   INTEGER :: icr_02(256),icg_02(256),icb_02(256)
   INTEGER :: icr_03(256),icg_03(256),icb_03(256)
   INTEGER :: icr_04(256),icg_04(256),icb_04(256)
   INTEGER :: icr_05(256),icg_05(256),icb_05(256)
   INTEGER :: icr_06(256),icg_06(256),icb_06(256)
   INTEGER :: icr_07(256),icg_07(256),icb_07(256)
   INTEGER :: icr_08(256),icg_08(256),icb_08(256)
!
   DATA icr_01/0,1,2,4,5,7,8,10,11,13,14,15,17,18,20,21,23,24,26,  &
      27,28,30,31,33,34,36,37,39,40,42,43,44,46,47,49,50,52,53,55,  &
      56,57,59,60,62,63,65,66,68,69,70,72,73,75,76,78,79,81,82,84,  &
      85,86,88,89,91,92,94,95,97,98,99,101,102,104,105,107,108,110,  &
      111,113,114,115,117,118,120,121,123,124,126,127,128,130,131,  &
      133,134,136,137,139,140,141,143,144,146,147,149,150,152,153,  &
      155,156,157,159,160,162,163,165,166,168,169,170,172,173,175,  &
      176,178,179,181,182,184,185,186,188,189,191,192,194,195,197,  &
      198,199,201,202,204,205,207,208,210,211,212,214,215,217,218,  &
      220,221,223,224,226,227,228,230,231,233,234,236,237,239,240,  &
      241,243,244,246,247,249,250,252,253,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255/
!
   DATA icg_01/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,1,3,5,7,9,11,13,15,17,18,20,22,24,26,28,30,32,34,  &
      35,37,39,41,43,45,47,49,51,52,54,56,58,60,62,64,66,68,69,71,  &
      73,75,77,79,81,83,85,86,88,90,92,94,96,98,100,102,103,105,  &
      107,109,111,113,115,117,119,120,122,124,126,128,130,132,134,  &
      136,137,139,141,143,145,147,149,151,153,154,156,158,160,162,  &
      164,166,168,170,171,173,175,177,179,181,183,185,187,188,190,  &
      192,194,196,198,200,202,204,205,207,209,211,213,215,217,219,  &
      221,222,224,226,228,230,232,234,236,238,239,241,243,245,247,  &
      249,251,253,255/
!
   DATA icb_01/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,7,11,15,19,23,27,31,35,39,  &
      43,47,51,54,58,62,66,70,74,78,82,86,90,94,98,102,105,109,113,  &
      117,121,125,129,133,137,141,145,149,153,156,160,164,168,172,  &
      176,180,184,188,192,196,200,204,207,211,215,219,223,227,231,  &
      235,239,243,247,251,255/
!
   DATA icr_02/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,9,14,19,23,  &
      28,33,38,42,47,52,57,61,66,71,76,81,81,81,81,81,81,81,81,80,  &
      80,80,80,80,80,80,79,84,89,94,99,104,109,114,119,124,129,134,  &
      139,144,149,154,159,164,169,174,180,185,190,196,201,206,212,  &
      217,222,228,233,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,248,240,232,  &
      225,217,209,202,194,186,179,171,163,168,173,178,183,188,193,  &
      198,203,209,214,219,224,229,234,239,244,249,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255/
!
   DATA icg_02/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,  &
      10,16,21,27,32,37,43,48,54,59,64,70,75,81,85,90,95,100,105,  &
      109,114,119,124,129,134,138,143,148,153,158,163,163,163,163,  &
      163,163,163,163,163,163,163,163,163,163,163,163,163,163,163,  &
      163,163,163,163,163,163,163,163,163,163,163,163,163,169,175,  &
      181,187,193,199,205,212,218,224,230,236,242,248,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  &
      255,255/
!
   DATA icr_03/124,124,121,117,114,112,109,105,103,100,96,93,91,  &
      87,84,82,79,75,73,70,68,65,61,59,56,54,52,49,47,45,42,40,38,  &
      35,33,31,28,26,24,24,22,19,19,17,15,14,12,10,12,8,8,7,5,5,3,  &
      3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,5,5,7,8,8,12,10,12,  &
      14,15,17,19,19,22,24,24,26,28,31,33,35,38,40,42,45,47,49,52,  &
      54,56,59,61,65,68,70,73,75,79,82,84,87,91,93,96,100,103,105,  &
      109,112,114,117,121,124,128,130,133,137,140,142,145,149,151,  &
      154,158,161,163,167,170,172,175,179,181,184,186,189,193,195,  &
      198,200,202,205,207,209,212,214,216,219,221,223,226,228,230,  &
      230,232,235,235,237,239,240,242,244,244,246,246,248,249,249,  &
      251,251,251,253,253,253,253,253,253,253,255,253,253,253,253,  &
      253,253,253,251,251,251,249,249,248,246,246,244,244,242,240,  &
      239,237,235,235,232,230,230,228,226,223,221,219,216,214,212,  &
      209,207,205,202,200,198,195,193,189,186,184,181,179,175,172,  &
      170,167,163,161,158,154,151,149,145,142,140,137,133,133/
!
   DATA icg_03/121,121,140,154,160,154,140,121,96,72,52,38,33,38,  &
      52,72,96,121,140,154,160,154,140,121,96,72,52,38,33,38,52,72,  &
      96,121,140,154,160,154,140,121,96,72,52,38,33,38,52,72,96,  &
      121,140,154,160,154,140,121,96,72,52,38,33,38,52,72,96,121,  &
      140,154,160,154,140,121,96,72,52,38,33,38,52,72,96,121,140,  &
      154,160,154,140,121,96,72,52,38,33,38,52,72,96,121,140,154,  &
      160,154,140,121,96,72,52,38,33,38,52,72,96,121,140,154,160,  &
      154,140,121,96,72,52,38,33,38,52,72,96,121,140,154,160,154,  &
      140,121,96,72,52,38,33,38,52,72,96,121,140,154,160,154,140,  &
      121,96,72,52,38,33,38,52,72,96,121,140,154,160,154,140,121,  &
      96,72,52,38,33,38,52,72,96,121,140,154,160,154,140,121,96,72,  &
      52,38,33,38,52,72,96,121,140,154,160,154,140,121,96,72,52,38,  &
      33,38,52,72,96,121,140,154,160,154,140,121,96,72,52,38,33,38,  &
      52,72,96,121,140,154,160,154,140,121,96,72,52,38,33,38,52,72,  &
      96,121,140,154,160,154,140,121,96,72,52,38,33,38,52,52/
!
   DATA icb_03/130,130,133,137,140,142,145,149,151,154,158,161,  &
      163,167,170,172,175,179,181,184,186,189,193,195,198,200,202,  &
      205,207,209,212,214,216,219,221,223,226,228,230,230,232,235,  &
      235,237,239,240,242,244,244,246,246,248,249,249,251,251,251,  &
      253,253,253,253,253,253,253,255,253,253,253,253,253,253,253,  &
      251,251,251,249,249,248,246,246,244,244,242,240,239,237,235,  &
      235,232,230,230,228,226,223,221,219,216,214,212,209,207,205,  &
      202,200,198,195,193,189,186,184,181,179,175,172,170,167,163,  &
      161,158,154,151,149,145,142,140,137,133,130,126,124,121,117,  &
      114,112,109,105,103,100,96,93,91,87,84,82,79,75,73,70,68,65,  &
      61,59,56,54,52,49,47,45,42,40,38,35,33,31,28,26,24,24,22,19,  &
      19,17,15,14,12,10,12,8,8,7,5,5,3,3,3,1,1,1,1,1,1,1,1,1,1,1,1,  &
      1,1,1,3,3,3,5,5,7,8,8,12,10,12,14,15,17,19,19,22,24,24,26,28,  &
      31,33,35,38,40,42,45,47,49,52,54,56,59,61,65,68,70,73,75,79,  &
      82,84,87,91,93,96,100,103,105,109,112,114,117,121,121/
!
   DATA icr_04/3,3,5,7,8,10,12,14,15,15,17,17,17,17,17,17,15,15,  &
      14,12,10,12,8,5,3,1,0,3,5,7,12,12,12,15,17,17,19,19,19,19,19,  &
      17,15,14,12,8,5,1,1,5,10,17,22,28,35,42,49,56,63,70,77,84,93,  &
      100,105,112,119,124,130,135,138,142,145,149,149,151,151,151,  &
      149,147,144,142,137,133,126,121,114,107,98,89,82,73,63,54,43,  &
      35,24,15,5,1,12,17,24,31,38,43,47,52,54,56,56,56,56,52,49,45,  &
      38,33,24,15,5,3,15,26,40,52,66,80,96,110,126,140,156,170,184,  &
      198,212,225,237,248,251,242,233,226,221,216,214,212,212,212,  &
      216,219,225,232,239,249,251,239,226,212,198,182,167,149,133,  &
      116,98,80,63,47,29,14,1,15,28,42,54,63,73,80,86,91,93,94,94,  &
      91,87,82,75,66,54,42,28,12,3,21,40,59,80,103,124,147,170,193,  &
      216,239,249,228,207,186,168,151,133,119,105,94,84,77,70,66,  &
      65,66,68,72,79,86,98,110,124,140,158,177,196,219,240,244,221,  &
      195,170,144,119,94,70,45,22,1,19,38,56,73,89,102,112,121,126,126/
!
   DATA icg_04/51,51,58,65,72,79,84,91,98,105,110,117,124,128,  &
      135,140,145,151,156,161,167,170,175,179,184,188,191,195,198,  &
      200,202,205,207,209,212,212,214,214,216,216,216,216,216,214,  &
      214,212,212,209,209,205,204,200,198,195,191,188,184,181,177,  &
      172,168,163,158,153,147,142,137,130,124,119,112,105,100,93,  &
      86,79,73,66,59,52,45,38,31,24,17,10,3,1,8,15,22,28,35,42,49,  &
      54,61,68,73,79,84,91,96,100,105,110,116,119,124,128,133,135,  &
      140,142,145,149,151,153,154,156,158,160,161,161,161,161,161,  &
      161,161,161,160,158,156,154,153,151,147,145,142,138,135,131,  &
      128,124,119,114,110,105,100,94,89,84,79,73,66,61,54,47,42,35,  &
      28,22,15,8,1,5,12,19,26,33,40,47,54,61,66,73,80,87,93,100,  &
      107,112,119,126,130,137,142,147,154,158,163,168,172,177,181,  &
      184,188,193,195,198,202,204,207,209,211,212,214,214,216,216,  &
      216,216,216,216,214,214,212,212,209,207,205,202,200,198,193,  &
      191,186,184,179,175,170,165,161,156,151,145,140,135,128,123,  &
      117,110,103,98,91,84,77,70,63,56,49,43,36,29,22,22/
!
   DATA icb_04/116,116,105,93,84,73,63,54,45,38,29,24,17,12,8,5,  &
      1,0,0,0,1,3,5,12,14,19,26,33,40,49,59,68,77,87,98,109,119,  &
      130,142,153,163,174,184,193,202,211,218,226,232,237,242,246,  &
      249,251,253,253,253,251,249,246,240,235,230,223,216,207,200,  &
      189,181,170,160,149,138,128,117,105,96,84,75,65,56,47,38,31,  &
      24,17,12,8,5,1,0,0,0,1,3,5,8,14,19,24,31,40,47,56,66,75,86,  &
      96,107,119,130,140,151,161,172,181,191,200,209,216,225,230,  &
      237,242,246,249,251,253,253,253,251,249,246,242,237,230,225,  &
      216,209,200,191,181,172,161,151,140,130,119,107,96,86,75,66,  &
      56,47,40,31,24,19,14,8,5,3,1,0,0,0,1,5,8,12,17,24,31,38,47,  &
      56,65,75,84,96,105,117,128,138,149,160,170,181,189,200,207,  &
      216,223,230,235,240,246,249,251,253,253,253,251,249,246,242,  &
      237,232,226,218,211,202,193,184,174,163,153,142,130,119,109,  &
      98,87,77,68,59,49,40,33,26,19,14,12,5,3,1,0,0,0,1,5,8,12,17,  &
      24,29,38,45,54,63,73,84,93,105,105/
!
   DATA icr_05/59,60,61,62,64,65,66,67,68,69,70,71,72,74,75,76,  &
      77,78,79,81,82,83,84,86,87,88,90,91,93,94,95,97,98,99,101,  &
      102,103,104,105,107,108,109,111,112,113,115,116,118,119,120,  &
      122,123,125,126,127,129,130,131,133,134,136,137,138,140,141,  &
      142,144,145,147,148,149,151,152,153,155,156,158,159,160,162,  &
      163,164,166,167,169,170,171,173,174,175,177,178,179,180,182,  &
      183,184,185,187,188,189,190,192,193,194,195,197,198,199,200,  &
      202,203,204,205,206,207,209,210,211,212,213,214,215,216,217,  &
      218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,  &
      233,234,234,235,236,237,237,238,239,239,240,240,241,242,242,  &
      243,243,244,244,245,245,245,246,246,246,246,247,247,247,247,  &
      247,247,247,247,247,247,247,247,247,247,247,247,247,247,247,  &
      247,246,246,246,245,245,244,244,244,243,243,243,242,242,241,  &
      241,240,240,239,239,238,237,237,236,235,234,234,233,232,231,  &
      230,229,228,227,226,226,225,224,223,222,221,220,219,218,217,  &
      215,214,213,212,211,209,208,207,206,204,203,202,200,199,198,  &
      196,195,193,192,191,189,188,186,185,183,182/
!
   DATA icg_05/76,78,80,81,83,85,87,88,90,92,94,95,97,99,101,102,  &
      104,106,107,109,111,112,114,115,117,119,120,122,124,125,127,  &
      128,130,132,133,135,136,138,139,141,142,144,145,147,148,150,  &
      151,153,154,155,157,158,160,161,162,164,165,166,168,169,171,  &
      172,173,175,176,177,178,180,181,182,183,184,185,186,187,188,  &
      190,191,192,193,194,195,196,197,198,198,199,200,201,202,203,  &
      204,205,206,206,207,208,209,209,210,211,211,212,212,213,214,  &
      214,215,215,216,216,217,217,217,218,218,218,218,219,219,219,  &
      219,220,220,220,221,221,221,221,221,220,220,219,218,217,217,  &
      216,215,215,214,214,213,212,212,211,210,209,209,208,207,206,  &
      205,204,203,202,201,200,199,198,197,196,195,194,193,192,190,  &
      189,188,187,186,185,183,182,181,180,178,177,176,174,173,172,  &
      170,169,167,166,165,163,162,160,159,157,156,154,152,151,149,  &
      148,146,144,143,141,139,138,136,134,132,131,129,127,125,123,  &
      121,120,118,116,114,112,110,108,106,104,102,100,98,96,94,92,  &
      90,88,86,84,82,80,78,76,74,71,69,67,64,62,60,58,55,53,50,47,  &
      44,40,36,32,28,24,19,14,9/
!
   DATA icb_05/192,194,195,197,198,200,201,203,204,205,207,208,  &
      210,211,212,214,215,216,218,219,220,221,223,224,225,226,227,  &
      229,230,231,232,233,234,235,236,237,238,239,239,240,241,242,  &
      243,243,244,245,246,246,247,248,248,249,249,250,250,251,251,  &
      251,252,252,253,253,253,254,254,254,254,255,255,255,255,255,  &
      255,255,255,255,255,255,255,255,255,255,255,254,254,254,254,  &
      253,253,253,252,252,251,251,250,250,249,248,248,247,247,246,  &
      245,245,244,243,243,242,241,240,240,239,238,237,236,235,234,  &
      233,232,231,230,229,228,227,226,225,223,222,221,220,218,217,  &
      215,214,212,211,209,208,206,205,203,202,200,199,197,196,194,  &
      193,191,190,188,187,185,184,182,181,179,178,176,175,173,171,  &
      170,168,167,165,163,162,160,158,157,155,154,153,151,150,148,  &
      146,145,143,142,140,138,137,135,133,132,130,129,127,126,124,  &
      123,122,120,119,117,116,114,113,111,109,108,106,105,103,102,  &
      100,99,98,96,95,93,92,91,89,88,87,85,84,83,81,80,78,77,76,74,  &
      73,71,70,69,67,66,65,63,62,61,60,58,57,56,55,54,53,52,50,49,  &
      48,47,46,45,44,43,41,40,39/
!
   DATA icr_06/82,82,77,73,68,63,59,54,49,45,42,38,33,29,26,22,  &
      19,15,12,12,8,5,3,1,0,1,3,5,5,5,7,7,8,7,7,5,5,5,3,1,0,1,3,5,  &
      8,12,12,15,19,22,26,29,33,38,42,45,49,54,59,63,68,73,77,82,  &
      86,91,96,100,105,110,114,119,123,126,130,135,138,142,147,149,  &
      154,156,160,163,165,168,170,172,174,175,177,179,179,179,181,  &
      181,181,181,181,179,179,179,177,175,174,172,170,168,165,163,  &
      160,156,154,149,147,142,138,135,130,126,123,119,114,110,105,  &
      100,96,91,86,82,77,73,68,63,59,54,49,45,42,38,33,29,26,22,19,  &
      15,12,12,8,5,3,1,0,1,3,5,5,5,7,7,8,7,7,5,5,5,3,1,0,1,3,5,8,  &
      12,12,15,19,22,26,29,33,38,42,45,49,54,59,63,68,73,77,82,86,  &
      91,96,100,105,110,114,119,123,126,130,135,138,142,147,149,  &
      154,156,160,163,165,168,170,172,174,175,177,179,179,179,181,  &
      181,181,181,181,179,179,179,177,175,174,172,170,168,165,163,  &
      160,156,154,149,147,142,138,135,130,126,123,119,114,110,105,  &
      100,96,96/
!
   DATA icg_06/0,0,1,1,1,1,3,3,3,5,5,5,7,8,8,12,10,12,12,15,15,  &
      17,19,19,22,22,24,26,28,29,31,33,35,36,38,40,42,45,45,47,49,  &
      51,52,54,56,58,59,59,61,61,63,63,65,66,66,66,66,66,66,66,66,  &
      66,66,66,66,65,63,63,63,61,61,59,59,56,56,54,54,52,52,51,49,  &
      47,47,47,45,45,43,43,43,42,42,42,42,43,43,45,45,47,47,49,51,  &
      52,54,56,59,61,65,68,70,73,77,80,84,87,91,96,100,103,107,112,  &
      116,119,124,128,131,135,140,144,147,151,154,158,161,163,165,  &
      168,170,172,175,175,177,179,179,179,179,179,179,179,179,177,  &
      175,175,172,170,168,165,163,161,158,154,151,147,144,140,137,  &
      133,130,126,124,119,117,112,110,107,103,100,98,96,94,93,91,  &
      89,89,87,87,87,87,87,89,91,91,93,96,98,102,105,109,112,117,  &
      121,126,131,137,142,147,154,160,165,172,179,186,191,198,205,  &
      212,218,225,230,237,242,248,253,251,246,240,237,232,228,226,  &
      223,219,218,216,214,214,212,212,214,214,216,216,219,221,223,  &
      228,230,235,239,244,248,253,251,246,239,233,233/
!
   DATA icb_06/214,214,202,188,177,165,153,140,128,117,107,96,84,  &
      75,65,56,47,38,29,22,15,8,1,1,7,12,15,19,22,24,24,26,26,26,  &
      24,24,22,19,15,12,7,1,1,8,15,22,29,38,47,56,65,75,84,96,107,  &
      117,128,140,153,165,177,188,202,214,226,239,251,246,233,221,  &
      209,198,186,175,163,153,142,131,121,112,103,94,86,79,72,66,  &
      59,54,49,43,40,36,33,31,31,29,28,29,31,31,33,36,40,43,49,54,  &
      59,66,72,79,86,94,103,112,121,131,142,153,163,175,186,198,  &
      209,221,233,246,251,239,226,214,202,188,177,165,153,140,128,  &
      117,107,96,84,75,65,56,47,38,29,22,15,8,1,1,7,12,15,19,22,24,  &
      24,26,26,26,24,24,22,19,15,12,7,1,1,8,15,22,29,38,47,56,65,  &
      75,84,96,107,117,128,140,153,165,177,188,202,214,226,239,251,  &
      246,233,221,209,198,186,175,163,153,142,131,121,112,103,94,  &
      86,79,72,66,59,54,49,43,40,36,33,31,31,29,28,29,31,31,33,36,  &
      40,43,49,54,59,66,72,79,86,94,103,112,121,131,142,153,163,  &
      175,186,198,209,221,233,246,251,251/
!
   DATA icr_07/0,0,0,0,79,79,79,79,160,160,160,160,239,239,239,  &
      239,0,0,0,0,79,79,79,79,160,160,160,160,239,239,239,239,0,0,  &
      0,0,79,79,79,79,160,160,160,160,239,239,239,239,0,0,0,0,79,  &
      79,79,79,160,160,160,160,239,239,239,239,0,0,0,0,79,79,79,79,  &
      160,160,160,160,239,239,239,239,0,0,0,0,79,79,79,79,160,160,  &
      160,160,239,239,239,239,0,0,0,0,79,79,79,79,160,160,160,160,  &
      239,239,239,239,0,0,0,0,79,79,79,79,160,160,160,160,239,239,  &
      239,239,0,0,0,0,79,79,79,79,160,160,160,160,239,239,239,239,  &
      0,0,0,0,79,79,79,79,160,160,160,160,239,239,239,239,0,0,0,0,  &
      79,79,79,79,160,160,160,160,239,239,239,239,0,0,0,0,79,79,79,  &
      79,160,160,160,160,239,239,239,239,0,0,0,0,79,79,80,79,160,  &
      160,160,160,239,239,239,239,0,0,0,0,79,79,79,79,160,160,160,  &
      160,239,239,239,239,0,0,0,0,79,79,79,79,160,160,160,160,239,  &
      239,239,239,0,0,0,0,79,79,79,79,160,160,160,160,239,239,239,239/
!
   DATA icg_07/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15,15,15,15,15,  &
      15,15,15,15,15,15,15,15,15,15,31,31,31,31,31,31,31,31,31,31,  &
      31,31,31,31,31,31,47,47,47,47,47,47,47,47,47,47,47,47,47,47,  &
      47,47,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,79,79,  &
      79,79,79,79,79,79,79,79,79,79,79,79,79,79,96,96,96,96,96,96,  &
      96,96,96,96,96,96,96,96,96,96,112,112,112,112,112,112,112,  &
      112,112,112,112,112,112,112,112,112,128,128,128,128,128,128,  &
      128,128,128,128,128,128,128,128,128,128,144,144,144,144,144,  &
      144,144,144,144,144,144,144,144,144,144,144,160,160,160,160,  &
      160,160,160,160,160,160,160,160,160,160,160,160,175,175,175,  &
      175,175,175,175,175,175,175,175,175,175,175,175,175,191,191,  &
      191,191,191,191,193,191,191,191,191,191,191,191,191,191,207,  &
      207,207,207,207,207,207,207,207,207,207,207,207,207,207,207,  &
      223,223,223,223,223,223,223,223,223,223,223,223,223,223,223,  &
      223,239,239,239,239,239,239,239,239,239,239,239,239,239,239,  &
      239,239/
!
   DATA icb_07/79,79,160,239,0,79,160,239,0,79,160,239,0,79,160,  &
      239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,  &
      160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,  &
      0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,  &
      239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,  &
      160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,  &
      0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,  &
      239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,  &
      160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,  &
      0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,  &
      239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,  &
      160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,  &
      0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,  &
      239,0,79,160,239,0,79,160,239,0,79,160,239,0,79,160,160/
!
   DATA icr_08/1,1,3,3,5,5,7,8,8,12,10,12,14,15,15,17,17,19,21,  &
      22,22,24,24,26,26,28,29,31,31,33,33,35,35,36,38,38,38,40,40,  &
      42,42,43,45,45,45,47,47,49,49,51,52,52,54,54,56,56,58,59,59,  &
      61,61,63,65,66,66,68,70,70,72,73,75,77,77,79,80,82,84,84,86,  &
      87,89,89,91,93,93,94,96,98,98,100,100,102,103,103,103,105,  &
      105,107,107,107,107,109,110,110,110,110,112,112,112,114,114,  &
      114,116,117,117,117,119,119,121,121,123,124,124,126,128,128,  &
      130,131,133,135,137,137,140,142,142,144,147,149,149,151,154,  &
      154,156,158,160,161,163,163,165,167,168,168,170,170,172,172,  &
      174,174,175,175,175,175,177,177,177,177,177,177,179,179,179,  &
      179,179,179,181,181,181,181,182,184,184,184,186,186,188,189,  &
      191,191,193,195,196,198,200,202,204,205,207,209,212,214,216,  &
      218,219,221,223,225,226,228,230,232,233,235,237,237,239,240,  &
      242,242,242,244,244,244,246,246,246,246,246,246,246,246,246,  &
      246,246,246,246,246,246,246,246,246,248,248,248,249,249,251,  &
      251,251,253,255,253,251,251,249,246,246/
!
   DATA icg_08/19,19,19,22,24,28,33,38,43,51,58,65,73,80,89,98,  &
      105,114,124,131,140,147,154,161,168,175,179,184,188,191,193,  &
      193,195,193,193,191,188,184,179,175,168,161,154,147,140,131,  &
      124,114,107,98,89,80,73,65,58,51,43,38,33,28,24,22,19,19,19,  &
      19,19,22,24,28,33,38,43,51,58,65,73,80,89,98,107,114,124,131,  &
      140,147,154,161,168,175,179,184,188,191,193,193,195,193,193,  &
      191,188,184,179,175,168,161,154,147,140,131,124,114,105,98,  &
      89,80,73,65,58,51,43,38,33,28,24,22,19,19,19,19,19,22,24,28,  &
      33,38,43,51,58,65,73,80,89,98,107,114,124,131,140,147,154,  &
      161,168,175,179,184,188,191,193,193,195,193,193,191,188,184,  &
      179,175,168,161,154,147,140,131,124,114,105,98,89,80,73,65,  &
      58,51,43,38,33,28,24,22,19,19,19,19,19,22,24,28,33,38,43,51,  &
      58,65,73,80,89,98,107,114,124,131,140,147,154,161,168,175,  &
      179,184,188,191,193,193,195,193,193,191,188,184,179,175,168,  &
      161,154,147,140,131,124,114,105,98,89,80,73,65,58,51,43,38,  &
      33,28,24,22,19,19/
!
   DATA icb_08/7,7,10,15,19,22,26,29,33,38,42,47,51,54,59,63,66,  &
      70,75,77,82,86,89,93,96,100,103,107,110,112,116,119,121,124,  &
      128,130,133,135,138,140,144,147,149,153,156,158,161,165,168,  &
      172,175,179,184,188,191,195,200,205,209,214,219,225,230,235,  &
      240,246,251,253,248,242,237,232,226,221,216,212,207,202,198,  &
      193,189,186,181,179,175,172,170,167,165,161,160,158,156,154,  &
      154,151,151,149,147,147,145,144,142,140,140,137,135,133,131,  &
      128,126,123,119,116,112,107,103,98,93,89,82,77,72,65,59,52,  &
      45,38,31,24,17,10,5,253,246,239,232,226,221,214,209,205,200,  &
      195,191,188,184,181,177,175,174,172,170,170,168,168,168,168,  &
      168,168,168,168,168,168,170,170,170,168,168,168,167,165,163,  &
      161,160,156,154,149,145,140,135,130,124,119,112,105,98,91,82,  &
      75,66,58,49,40,33,24,15,8,255,246,239,232,225,219,212,207,  &
      202,196,193,188,186,182,181,179,177,177,177,177,177,179,179,  &
      181,182,184,186,188,189,191,193,195,195,196,198,198,198,198,  &
      196,195,193,191,188,184,179,175,168,163,156,149,142,133,124,  &
      116,107,96,86,86/
!
!  black and white table
!
   DO i=0,255
      cr=REAL(i,KIND=pg)/255.0_pg
      cg=REAL(i,KIND=pg)/255.0_pg
      cb=REAL(i,KIND=pg)/255.0_pg
      CALL pgscr(i,cr,cg,cb)
   END DO
!
!   red temperature table
!
   IF(itable == 2)THEN
      DO i=0,255
         cr=REAL(icr_01(i+1),KIND=pg)/255.0_pg
         cg=REAL(icg_01(i+1),KIND=pg)/255.0_pg
         cb=REAL(icb_01(i+1),KIND=pg)/255.0_pg
         CALL pgscr(i,cr,cg,cb)
      END DO
      RETURN
   END IF
!
!   std-gamma table
!
   IF(itable == 3)THEN
      DO i=0,255
         cr=REAL(icr_02(i+1),KIND=pg)/255.0_pg
         cg=REAL(icg_02(i+1),KIND=pg)/255.0_pg
         cb=REAL(icb_02(i+1),KIND=pg)/255.0_pg
         CALL pgscr(i,cr,cg,cb)
      END DO
      RETURN
   END IF
!
!   waves table
!
   IF(itable == 4)THEN
      DO i=0,255
         cr=REAL(icr_03(i+1),KIND=pg)/255.0_pg
         cg=REAL(icg_03(i+1),KIND=pg)/255.0_pg
         cb=REAL(icb_03(i+1),KIND=pg)/255.0_pg
         CALL pgscr(i,cr,cg,cb)
      END DO
      RETURN
   END IF
!
!   hardcandy table
!
   IF(itable == 5)THEN
      DO i=0,255
         cr=REAL(icr_04(i+1),KIND=pg)/255.0_pg
         cg=REAL(icg_04(i+1),KIND=pg)/255.0_pg
         cb=REAL(icb_04(i+1),KIND=pg)/255.0_pg
         CALL pgscr(i,cr,cg,cb)
      END DO
      RETURN
   END IF
!
!   diverge table
!
   IF(itable == 6)THEN
      DO i=0,255
         cr=REAL(icr_05(i+1),KIND=pg)/255.0_pg
         cg=REAL(icg_05(i+1),KIND=pg)/255.0_pg
         cb=REAL(icb_05(i+1),KIND=pg)/255.0_pg
         CALL pgscr(i,cr,cg,cb)
      END DO
      RETURN
   END IF
!
!   blue_waves table
!
   IF(itable == 7)THEN
      DO i=0,255
         cr=REAL(icr_06(i+1),KIND=pg)/255.0_pg
         cg=REAL(icg_06(i+1),KIND=pg)/255.0_pg
         cb=REAL(icb_06(i+1),KIND=pg)/255.0_pg
         CALL pgscr(i,cr,cg,cb)
      END DO
      RETURN
   END IF
!
!   peppermint table
!
   IF(itable == 8)THEN
      DO i=0,255
         cr=REAL(icr_07(i+1),KIND=pg)/255.0_pg
         cg=REAL(icg_07(i+1),KIND=pg)/255.0_pg
         cb=REAL(icb_07(i+1),KIND=pg)/255.0_pg
         CALL pgscr(i,cr,cg,cb)
      END DO
      RETURN
   END IF
!
!   plasma table
!
   IF(itable == 9)THEN
      DO i=0,255
         cr=REAL(icr_08(i+1),KIND=pg)/255.0_pg
         cg=REAL(icg_08(i+1),KIND=pg)/255.0_pg
         cb=REAL(icb_08(i+1),KIND=pg)/255.0_pg
         CALL pgscr(i,cr,cg,cb)
      END DO
      RETURN
   END IF
!
   RETURN
!
END SUBROUTINE colortable
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE covpar(istart,nskip,istop)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                  :: istart
   INTEGER, INTENT(OUT)                  :: nskip
   INTEGER, INTENT(OUT)                  :: istop
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg1,arg2,arg3
!
   ios=0
   CALL get_command_argument(1,arg1)
   READ(arg1,*,IOSTAT=ios)istart
!
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)nskip
!
   CALL get_command_argument(3,arg3)
   READ(arg3,*,IOSTAT=ios)istop
   IF(ios == 0)RETURN
!
END SUBROUTINE covpar
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE dpassign90(np,nbin,arrayx,binx,biny,rmin,rmax,  &
   ymax,rmedian,clip,siglow,sighigh)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: np
   INTEGER, INTENT(IN)                      :: nbin
   REAL(KIND=dp), INTENT(IN OUT)            :: arrayx(np)
   REAL(KIND=dp), INTENT(OUT)               :: binx(500)
   REAL(KIND=dp), INTENT(OUT)               :: biny(500)
   REAL(KIND=dp), INTENT(OUT)               :: rmin
   REAL(KIND=dp), INTENT(OUT)               :: rmax
   REAL(KIND=dp), INTENT(OUT)               :: ymax
   REAL(KIND=dp), INTENT(OUT)               :: rmedian
   REAL(KIND=dp), INTENT(IN)                :: clip
   REAL(KIND=dp), INTENT(OUT)               :: siglow
   REAL(KIND=dp), INTENT(OUT)               :: sighigh
!
   REAL(KIND=dp)  :: std,summ,ave,qq,t1
   REAL(KIND=dp), ALLOCATABLE  :: scratch(:)
!
   INTEGER :: i,ithresh,icount,j
!
   ALLOCATE(scratch(9000000))
!
   ithresh=nint(0.00005_dp*REAL(np,KIND=dp))
   CALL sort(np,arrayx)
   rmedian=arrayx(np/2)
   siglow=arrayx(INT(REAL(np,KIND=dp)*0.05_dp))
   sighigh=arrayx(INT(REAL(np,KIND=dp)*0.95_dp))
!
   summ=0.0_dp
   IF(clip > 3.0_dp)THEN
      DO i=1,np
         summ=summ+arrayx(i)
      END DO
      ave=summ/REAL(np,KIND=dp)
      summ=0.0_dp
      DO i=1,np
         summ=summ+(arrayx(i)-ave)**2
      END DO
      std=summ/REAL(np,KIND=dp)
      std=SQRT(std)
!
      icount=0
      DO i=1,np
         IF(ABS(arrayx(i)-ave) < clip*std)THEN
            icount=icount+1
            scratch(icount)=arrayx(i)
         END IF
      END DO
   ELSE
      icount=0
      DO i=1+ithresh,np-ithresh
         icount=icount+1
         scratch(icount)=arrayx(i)
      END DO
   END IF
!
   rmax=scratch(icount)
   rmin=scratch(1)
   qq=(rmax-rmin)/REAL(nbin,KIND=dp)
!
   DO i=1,nbin
      binx(i)=REAL(i-1,KIND=dp)*qq+rmin+qq/2.0_dp
      biny(i)=0.0_dp
   END DO
!
   loop30:  DO  i=1,icount
      DO  j=1,nbin
         t1=ABS(scratch(i)-binx(j))
         IF(t1 <= qq/2.0_dp)THEN
            biny(j)=biny(j)+1.0_dp
            CYCLE loop30
         END IF
      END DO
   END DO loop30
   ymax=-1000.0_dp
   DO  i=1,nbin
      IF(biny(i) > ymax)ymax=biny(i)
   END DO
!
   DEALLOCATE(scratch)
!
   RETURN
!
END SUBROUTINE dpassign90
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE dpassignbin(np,nbin,arrayx,binx,biny,rmin,rmax,  &
   ymax,rmedian,clip,siglow,sighigh)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: np
   INTEGER, INTENT(IN)                      :: nbin
   REAL(KIND=dp), INTENT(IN OUT)            :: arrayx(np)
   REAL(KIND=dp), INTENT(OUT)               :: binx(500)
   REAL(KIND=dp), INTENT(OUT)               :: biny(500)
   REAL(KIND=dp), INTENT(OUT)               :: rmin
   REAL(KIND=dp), INTENT(OUT)               :: rmax
   REAL(KIND=dp), INTENT(OUT)               :: ymax
   REAL(KIND=dp), INTENT(OUT)               :: rmedian
   REAL(KIND=dp), INTENT(IN)                :: clip
   REAL(KIND=dp), INTENT(OUT)               :: siglow
   REAL(KIND=dp), INTENT(OUT)               :: sighigh
!
   REAL(KIND=dp)  :: std,summ,ave,qq,t1
   REAL(KIND=dp), ALLOCATABLE  :: scratch(:)
!
   INTEGER :: i,ithresh,icount,j
!
   ALLOCATE(scratch(9000000))
!
   ithresh=nint(0.00005_dp*REAL(np,KIND=dp))
   CALL sort(np,arrayx)
   rmedian=arrayx(np/2)
   siglow=arrayx(INT(REAL(np,KIND=dp)*0.1585_dp))
   sighigh=arrayx(INT(REAL(np,KIND=dp)*0.8415_dp))
!
   summ=0.0_dp
   IF(clip > 3.0_dp)THEN
      DO i=1,np
         summ=summ+arrayx(i)
      END DO
      ave=summ/REAL(np,KIND=dp)
      summ=0.0_dp
      DO i=1,np
         summ=summ+(arrayx(i)-ave)**2
      END DO
      std=summ/REAL(np,KIND=dp)
      std=SQRT(std)
!
      icount=0
      DO i=1,np
         IF(ABS(arrayx(i)-ave) < clip*std)THEN
            icount=icount+1
            scratch(icount)=arrayx(i)
         END IF
      END DO
   ELSE
      icount=0
      DO i=1+ithresh,np-ithresh
         icount=icount+1
         scratch(icount)=arrayx(i)
      END DO
   END IF
!
   rmax=scratch(icount)
   rmin=scratch(1)
   qq=(rmax-rmin)/REAL(nbin,KIND=dp)
!
   DO i=1,nbin
      binx(i)=REAL(i-1,KIND=dp)*qq+rmin+qq/2.0_dp
      biny(i)=0.0_dp
   END DO
!
   loop30:  DO  i=1,icount
      DO  j=1,nbin
         t1=ABS(scratch(i)-binx(j))
         IF(t1 <= qq/2.0_dp)THEN
            biny(j)=biny(j)+1.0_dp
            CYCLE loop30
         END IF
      END DO
   END DO loop30
   ymax=-1000.0_dp
   DO  i=1,nbin
      IF(biny(i) > ymax)ymax=biny(i)
   END DO
!
   DEALLOCATE(scratch)
!
   RETURN
!
END SUBROUTINE dpassignbin
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE gelstart(istart,nskip,istop)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: istart
   INTEGER, INTENT(OUT)                     :: nskip
   INTEGER, INTENT(OUT)                     :: istop
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg1,arg2,arg3
!
   ios=0
   CALL get_command_argument(1,arg1)
   READ(arg1,*,IOSTAT=ios)istart
!
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)nskip
!
   CALL get_command_argument(3,arg3)
   READ(arg3,*,IOSTAT=ios)istop
   IF(ios == 0)RETURN
!
END SUBROUTINE gelstart
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getacf(icount,maxlag,ydata,xlag,ylag)
!
!   Will compute the ACF of the input data ydata
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: icount
   INTEGER, INTENT(IN)                      :: maxlag
   REAL(KIND=dp), INTENT(IN OUT)            :: ydata(icount)
   REAL(KIND=dp), INTENT(OUT)               :: xlag(maxlag)
   REAL(KIND=dp), INTENT(OUT)               :: ylag(maxlag)
!
   INTEGER :: i,k
!
   REAL(KIND=dp)  :: summ,ave,rms
!
!   Remove the average
!
   summ=0.0_dp
   DO i=1,icount
      summ=summ+ydata(i)
   END DO
   ave=summ/REAL(icount,KIND=dp)
   summ=0.0_dp
   DO i=1,icount
      ydata(i)=ydata(i)-ave
      summ=summ+ydata(i)**2
   END DO
   rms=summ
!
!   find the ACF
!
   DO k=0,maxlag-1
      summ=0.0_dp
      DO i=1,icount-k
         summ=summ+ydata(i)*ydata(i+k)
      END DO
      xlag(k+1)=REAL(k,KIND=dp)
      ylag(k+1)=summ/rms
   END DO
!
   RETURN
!
END SUBROUTINE getacf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getbody3(nalph3,nbet3,tertperiod,tertt0,tertecos,  &
   tertesin,tertincl,tertomega,tertq,dwavex,dwavey,itconj,it1,  &
   it2,it3,it4,tertconj,tertratrad,hh,sw72,sw73,p2tconj,  &
   p2period,p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,p2ratrad,  &
   p3tconj,p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,p3q,  &
   p3ratrad,p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,p4omega,  &
   p4q,p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,p5incl,  &
   p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,p6esin,  &
   p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,p7ecos,  &
   p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,  &
   p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,nbody)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nalph3
   INTEGER, INTENT(OUT)                     :: nbet3
   REAL(KIND=dp), INTENT(OUT)               :: tertperiod
   REAL(KIND=dp), INTENT(OUT)               :: tertt0
   REAL(KIND=dp), INTENT(OUT)               :: tertecos
   REAL(KIND=dp), INTENT(OUT)               :: tertesin
   REAL(KIND=dp), INTENT(OUT)               :: tertincl
   REAL(KIND=dp), INTENT(OUT)               :: tertomega
   REAL(KIND=dp), INTENT(OUT)               :: tertq
   REAL(KIND=dp), INTENT(IN OUT)            :: dwavex(8,10)
   REAL(KIND=dp), INTENT(IN OUT)            :: dwavey(8,10)
   INTEGER, INTENT(OUT)                     :: itconj
   INTEGER, INTENT(OUT)                     :: it1
   INTEGER, INTENT(OUT)                     :: it2
   INTEGER, INTENT(OUT)                     :: it3
   INTEGER, INTENT(OUT)                     :: it4
   REAL(KIND=dp), INTENT(OUT)               :: tertconj
   REAL(KIND=dp), INTENT(OUT)               :: tertratrad
   REAL(KIND=dp), INTENT(OUT)               :: hh
   REAL(KIND=dp), INTENT(OUT)               :: sw72
   REAL(KIND=dp), INTENT(OUT)               :: sw73
   REAL(KIND=dp), INTENT(OUT)               :: p2tconj
   REAL(KIND=dp), INTENT(OUT)               :: p2period
   REAL(KIND=dp), INTENT(OUT)               :: p2t0
   REAL(KIND=dp), INTENT(OUT)               :: p2ecos
   REAL(KIND=dp), INTENT(OUT)               :: p2esin
   REAL(KIND=dp), INTENT(OUT)               :: p2incl
   REAL(KIND=dp), INTENT(OUT)               :: p2omega
   REAL(KIND=dp), INTENT(OUT)               :: p2q
   REAL(KIND=dp), INTENT(OUT)               :: p2ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p3tconj
   REAL(KIND=dp), INTENT(OUT)               :: p3period
   REAL(KIND=dp), INTENT(OUT)               :: p3t0
   REAL(KIND=dp), INTENT(OUT)               :: p3ecos
   REAL(KIND=dp), INTENT(OUT)               :: p3esin
   REAL(KIND=dp), INTENT(OUT)               :: p3incl
   REAL(KIND=dp), INTENT(OUT)               :: p3omega
   REAL(KIND=dp), INTENT(OUT)               :: p3q
   REAL(KIND=dp), INTENT(OUT)               :: p3ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p4tconj
   REAL(KIND=dp), INTENT(OUT)               :: p4period
   REAL(KIND=dp), INTENT(OUT)               :: p4t0
   REAL(KIND=dp), INTENT(OUT)               :: p4ecos
   REAL(KIND=dp), INTENT(OUT)               :: p4esin
   REAL(KIND=dp), INTENT(OUT)               :: p4incl
   REAL(KIND=dp), INTENT(OUT)               :: p4omega
   REAL(KIND=dp), INTENT(OUT)               :: p4q
   REAL(KIND=dp), INTENT(OUT)               :: p4ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p5tconj
   REAL(KIND=dp), INTENT(OUT)               :: p5period
   REAL(KIND=dp), INTENT(OUT)               :: p5t0
   REAL(KIND=dp), INTENT(OUT)               :: p5ecos
   REAL(KIND=dp), INTENT(OUT)               :: p5esin
   REAL(KIND=dp), INTENT(OUT)               :: p5incl
   REAL(KIND=dp), INTENT(OUT)               :: p5omega
   REAL(KIND=dp), INTENT(OUT)               :: p5q
   REAL(KIND=dp), INTENT(OUT)               :: p5ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p6tconj
   REAL(KIND=dp), INTENT(OUT)               :: p6period
   REAL(KIND=dp), INTENT(OUT)               :: p6t0
   REAL(KIND=dp), INTENT(OUT)               :: p6ecos
   REAL(KIND=dp), INTENT(OUT)               :: p6esin
   REAL(KIND=dp), INTENT(OUT)               :: p6incl
   REAL(KIND=dp), INTENT(OUT)               :: p6omega
   REAL(KIND=dp), INTENT(OUT)               :: p6q
   REAL(KIND=dp), INTENT(OUT)               :: p6ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p7tconj
   REAL(KIND=dp), INTENT(OUT)               :: p7period
   REAL(KIND=dp), INTENT(OUT)               :: p7t0
   REAL(KIND=dp), INTENT(OUT)               :: p7ecos
   REAL(KIND=dp), INTENT(OUT)               :: p7esin
   REAL(KIND=dp), INTENT(OUT)               :: p7incl
   REAL(KIND=dp), INTENT(OUT)               :: p7omega
   REAL(KIND=dp), INTENT(OUT)               :: p7q
   REAL(KIND=dp), INTENT(OUT)               :: p7ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p8tconj
   REAL(KIND=dp), INTENT(OUT)               :: p8period
   REAL(KIND=dp), INTENT(OUT)               :: p8t0
   REAL(KIND=dp), INTENT(OUT)               :: p8ecos
   REAL(KIND=dp), INTENT(OUT)               :: p8esin
   REAL(KIND=dp), INTENT(OUT)               :: p8incl
   REAL(KIND=dp), INTENT(OUT)               :: p8omega
   REAL(KIND=dp), INTENT(OUT)               :: p8q
   REAL(KIND=dp), INTENT(OUT)               :: p8ratrad
   INTEGER, INTENT(IN)                      :: nbody
!
   INTEGER :: ios,i
!
   ios=0
   OPEN(UNIT=1,FILE='ELCbody3.inp',STATUS='old',ERR=20,IOSTAT= ios)
!
   READ(1,*)nalph3
   READ(1,*)nbet3
   READ(1,*)itconj
   READ(1,*)it1
   READ(1,*)it2
   READ(1,*)it3
   READ(1,*)it4
   READ(1,*)tertconj
   READ(1,*)tertperiod
   READ(1,*)tertt0
   READ(1,*)tertecos
   READ(1,*)tertesin
   READ(1,*)tertincl
   READ(1,*)tertomega
   READ(1,*)tertq
!
!  Load the limb darkening parameters.
!
   DO  i=1,8
      READ(1,*)dwavex(i,3),dwavey(i,3)
   END DO
!
   READ(1,*)tertratrad
   READ(1,*)hh
   READ(1,*)sw72
   READ(1,*)sw73
!
!  body 4 parameters.  If Nbody is 3, then zero these out
!
   READ(1,*)p2tconj
   READ(1,*)p2period
   READ(1,*)p2t0
   READ(1,*)p2ecos
   READ(1,*)p2esin
   READ(1,*)p2incl
   READ(1,*)p2omega
   READ(1,*)p2q
   READ(1,*)p2ratrad
   IF(nbody < 4)THEN
      p2tconj=0.0_dp
      p2period=0.0_dp
      p2t0=0.0_dp
      p2ecos=0.0_dp
      p2esin=0.0_dp
      p2incl=0.0_dp
      p2omega=0.0_dp
      p2q=0.0_dp
      p2ratrad=0.0_dp
   END IF
!
   READ(1,*)p3tconj
   READ(1,*)p3period
   READ(1,*)p3t0
   READ(1,*)p3ecos
   READ(1,*)p3esin
   READ(1,*)p3incl
   READ(1,*)p3omega
   READ(1,*)p3q
   READ(1,*)p3ratrad
   IF(nbody < 5)THEN
      p3tconj=0.0_dp
      p3period=0.0_dp
      p3t0=0.0_dp
      p3ecos=0.0_dp
      p3esin=0.0_dp
      p3incl=0.0_dp
      p3omega=0.0_dp
      p3q=0.0_dp
      p3ratrad=0.0_dp
   END IF
!
   READ(1,*)p4tconj
   READ(1,*)p4period
   READ(1,*)p4t0
   READ(1,*)p4ecos
   READ(1,*)p4esin
   READ(1,*)p4incl
   READ(1,*)p4omega
   READ(1,*)p4q
   READ(1,*)p4ratrad
   IF(nbody < 6)THEN
      p4tconj=0.0_dp
      p4period=0.0_dp
      p4t0=0.0_dp
      p4ecos=0.0_dp
      p4esin=0.0_dp
      p4incl=0.0_dp
      p4omega=0.0_dp
      p4q=0.0_dp
      p4ratrad=0.0_dp
   END IF
!
   READ(1,*)p5tconj
   READ(1,*)p5period
   READ(1,*)p5t0
   READ(1,*)p5ecos
   READ(1,*)p5esin
   READ(1,*)p5incl
   READ(1,*)p5omega
   READ(1,*)p5q
   READ(1,*)p5ratrad
   IF(nbody < 7)THEN
      p5tconj=0.0_dp
      p5period=0.0_dp
      p5t0=0.0_dp
      p5ecos=0.0_dp
      p5esin=0.0_dp
      p5incl=0.0_dp
      p5omega=0.0_dp
      p5q=0.0_dp
      p5ratrad=0.0_dp
   END IF
!
   READ(1,*)p6tconj
   READ(1,*)p6period
   READ(1,*)p6t0
   READ(1,*)p6ecos
   READ(1,*)p6esin
   READ(1,*)p6incl
   READ(1,*)p6omega
   READ(1,*)p6q
   READ(1,*)p6ratrad
   IF(nbody < 8)THEN
      p6tconj=0.0_dp
      p6period=0.0_dp
      p6t0=0.0_dp
      p6ecos=0.0_dp
      p6esin=0.0_dp
      p6incl=0.0_dp
      p6omega=0.0_dp
      p6q=0.0_dp
      p6ratrad=0.0_dp
   END IF
!
   READ(1,*)p7tconj
   READ(1,*)p7period
   READ(1,*)p7t0
   READ(1,*)p7ecos
   READ(1,*)p7esin
   READ(1,*)p7incl
   READ(1,*)p7omega
   READ(1,*)p7q
   READ(1,*)p7ratrad
   IF(nbody < 9)THEN
      p7tconj=0.0_dp
      p7period=0.0_dp
      p7t0=0.0_dp
      p7ecos=0.0_dp
      p7esin=0.0_dp
      p7incl=0.0_dp
      p7omega=0.0_dp
      p7q=0.0_dp
      p7ratrad=0.0_dp
   END IF
!
   READ(1,*)p8tconj
   READ(1,*)p8period
   READ(1,*)p8t0
   READ(1,*)p8ecos
   READ(1,*)p8esin
   READ(1,*)p8incl
   READ(1,*)p8omega
   READ(1,*)p8q
   READ(1,*)p8ratrad
   IF(nbody < 10)THEN
      p8tconj=0.0_dp
      p8period=0.0_dp
      p8t0=0.0_dp
      p8ecos=0.0_dp
      p8esin=0.0_dp
      p8incl=0.0_dp
      p8omega=0.0_dp
      p8q=0.0_dp
      p8ratrad=0.0_dp
   END IF
!
!   limb darkening of body 4 and 5
!
   DO i=1,8
      READ(1,*)dwavex(i,4),dwavey(i,4)
   END DO
!
   DO i=1,8
      READ(1,*)dwavex(i,5),dwavey(i,5)
   END DO
!
!   Come here if the input file ELCbody3.inp does not exist. The
!   subroutine writeinput will make the correct file and set default
!   values.
!
20 IF(ios > 0)CALL writebody3input(nalph3,nbet3,tertperiod,  &
      tertt0,tertecos,tertesin,tertincl,tertomega,tertq,dwavex,  &
      dwavey,itconj,it1,it2,it3,it4,tertconj,tertratrad,hh,sw72,  &
      sw73,p2tconj,p2period,p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,  &
      p2ratrad,p3tconj,p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,  &
      p3q,p3ratrad,p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,  &
      p4omega,p4q,p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,  &
      p5incl,p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,  &
      p6esin,p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,  &
      p7ecos,p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,  &
      p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad)
!
!   Put this if-then block for successful completion.
!
   IF(ios == 0)THEN
      CLOSE(1)
      RETURN
   END IF
!
   RETURN
!
END SUBROUTINE getbody3
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getchifiles(nfiles)
!
!   Will count the number of generation files
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nfiles
!
   INTEGER :: i
!
   CHARACTER(LEN=90) :: command,filein
!
!   command='ls generation.* > ELCjunk'
   command='find . -maxdepth 1 -name "generation.*" -print0 | xargs -0 ls '// &
     '| sort > ELCjunk'
   CALL execute_command_line(command)
   OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
   DO i=1,99999
      READ(33,40,END=10)filein
      IF(filein(1:2) == 'qt')filein='ELCjunk'
   END DO
10 CLOSE(33)
   nfiles=i-1
   RETURN
!
40 FORMAT(a90)
!
   RETURN
!
END SUBROUTINE getchifiles
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getcov(igen,nback,nskip,nlength,nvar,covchol,  &
   invcovchol,aveparm,iburn,ioptimizer)
!
!   Will read the demcmc_fitparm* files on the fly to determine
!   the covariance matrix.  Nback is the number of files
!   to average over, and Nskip is the number to skip in between.
!   igen is the current generation number.  Thus the generation
!   numbers to be used would be igen-Nskip, igen-2*Nskip,
!   igen-3*Nskip, ... igen*Nback*Nskip
!
!   covchol(Nvar,Nvar) is the square root of the covariance
!   matrix, aveparm(Nvar) contains the averages,
!   and invcovchol(Nvar,Nvar) is the inverse of covchol
!
!   To get a vector of uncorrelated parameters from a vector
!   corr, use
!
!   uncorr = matmul(corr,invcovchol)
!
!   To reverse that, use
!
!   corr = matmul(uncorr,covchol
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: igen
   INTEGER, INTENT(IN)                      :: nback
   INTEGER, INTENT(IN)                      :: nskip
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   REAL(KIND=dp), INTENT(OUT)               :: covchol(nvar,nvar)
   REAL(KIND=dp), INTENT(OUT)               :: invcovchol(nvar,nvar)
   REAL(KIND=dp), INTENT(OUT)               :: aveparm(nvar)
   INTEGER, INTENT(IN)                      :: iburn
   INTEGER, INTENT(IN)                      :: ioptimizer
!
   REAL(KIND=dp), ALLOCATABLE  :: parm(:,:),sum(:),cov(:,:)
!
   INTEGER :: i,j,k,ios,ifile,icount
!
   CHARACTER (LEN=7) :: extension
!
   ios=0
   ifile=0
   icount=0
!
   ALLOCATE(parm(99999,99),sum(99999),cov(nvar,nvar))
!
   DO k=0,nback
      IF(igen-k*nskip < iburn)EXIT
      WRITE(extension,30)igen-k*nskip+1000000
      IF(ioptimizer == 2)THEN
         OPEN(UNIT=2,FILE='demcmc_fitparm.'//extension,STATUS= 'old')
      END IF
      IF(ioptimizer == 10)THEN
         OPEN(UNIT=2,FILE='hammer_fitparm.'//extension,STATUS= 'old')
      END IF
      DO j=1,nlength
         icount=icount+1
         READ(2,*,END=10)(parm(icount,i),i=1,nvar)
      END DO
      10 CLOSE(2)
   END DO
!
20 DO i=1,nvar
      sum(i)=0.0_dp
      aveparm(i)=0.0_dp
      DO j=1,nvar
         cov(i,j)=0.0_dp
      END DO
   END DO
!
   DO i=1,nvar
      DO j=1,icount
         sum(i)=sum(i)+parm(j,i)
      END DO
   END DO
!
   DO i=1,nvar
      aveparm(i)=sum(i)/REAL(icount,KIND=dp)
   END DO
!
   DO i=1,nvar
      DO j=1,nvar
         DO k=1,icount
            cov(i,j)=cov(i,j)+(parm(k,i)-aveparm(i))*(parm(k,j)-aveparm(j))
         END DO
         cov(i,j)=cov(i,j)/REAL(icount-1,KIND=dp)
      END DO
   END DO
!
   CALL cholfac(nvar,cov,covchol)
!
!   copy the matrix covchol into cov as a temporary
!   copy for use in the inversemat routine
!
   DO i=1,nvar
      DO j=1,nvar
         cov(i,j)=covchol(i,j)
      END DO
   END DO
!
!   get the inverse of
!
   CALL inversemat(cov,invcovchol,nvar)
!
   DEALLOCATE(parm,sum,cov)
!
   RETURN
!
30 FORMAT(i7)
!
END SUBROUTINE getcov
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getelite(filein,ithresh)
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=40), INTENT(OUT)          :: filein
   INTEGER, INTENT(OUT)                     :: ithresh
!
   INTEGER :: ios, ios1
!
   CHARACTER (LEN=40) :: arg
!
!   See if the file name was put on the command line.
!
   ios=0
   ios1=0
   CALL get_command_argument(1,filein)
   IF(filein == '')THEN
      WRITE(*,*)'enter the input file'
      READ(*,100)filein
   END IF
!
!   Attempt to open the file.
!
   OPEN(UNIT=20,FILE=filein,STATUS='old',IOSTAT=ios)
   IF(ios == 0)THEN
      CALL get_command_argument(2,arg)
      READ(arg,*,IOSTAT=ios1)ithresh
      RETURN
   END IF
!
!   Problem opening file
!
   WRITE(*,200)filein
   STOP
!
100 FORMAT(a40)
200 FORMAT('Error opening file',1X,a)

END SUBROUTINE getelite
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getelcparmstring(suffix,label)
!
   USE accur

   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(OUT)           :: suffix(999)
   CHARACTER (LEN=*), INTENT(OUT)           :: label(999)
!
   suffix(3)='M1solar'
   label(3)='M\d1\u (solar masses)'
!
   suffix(4)='M2solar'
   label(4)='M\d2\u (solar masses)'
!
   suffix(5)='R1solar'
   label(5)='R\d1\u (solar radii)'
!
   suffix(6)='R2solar'
   label(6)='R\d2\u (solar radii)'
!
   suffix(7)='log_g1'
   label(7)='log_g1 (cgs)'
!
   suffix(8)='log_g2'
   label(8)='log_g2 (cgs)'
!
   suffix(9)='den1'
   label(9)='density of star 1 (g/cc)'
!
   suffix(10)='den2'
   label(10)='density of star 2 (g/cc)'
!
   suffix(11)='fill1'
   label(11)='filling factor of star 1'
!
   suffix(12)='fill2'
   label(12)='filling factor of star 2'
!
   suffix(13)='K1'
   label(13)='K\d1\u (km/sec)'
!
   suffix(14)='K2'
   label(14)='K\d2\u (km/sec)'
!
   suffix(15)='Vrot1'
   label(15)='Vrot1 (km/sec)'
!
   suffix(16)='Vrot2'
   label(16)='Vrot2 (km/sec)'
!
   suffix(17)='Vrot1_omega1'
   label(17)='Vrot1_omega1 (km/sec)'
!
   suffix(18)='Vrot2_omega2'
   label(18)='Vrot2_omega2 (km/sec)'
!
   suffix(19)='tspot_1_1'
   label(19)='tspot_1_1 (K)'
!
   suffix(20)='tspot_2_1'
   label(20)='tspot_2_1 (K)'
!
   suffix(21)='tspot_1_2'
   label(21)='tspot_1_2 (K)'
!
   suffix(22)='tspot_2_2'
   label(22)='tspot_2_2 (K)'
!
   suffix(23)='tspot_1_disk'
   label(23)='tspot_1_disk (K)'
!
   suffix(24)='tspot_2_disk'
   label(24)='tspot_2_disk (K)'
!
   suffix(25)='diskrad'
   label(25)='disk radius (solar radii)'
!
   suffix(26)='diskthick'
   label(26)='disk thickness (solar radii)'
!
   suffix(27)='X_duras'
   label(27)='X-ray eclipse duration (degrees)'
!
   suffix(28)='impact1'
   label(28)='impact parameter 1'
!
   suffix(29)='tran_duras'
   label(29)='transit duration (days)'
!
   suffix(30)='tran_depth'
   label(30)='transit depth'
!
   suffix(31)='tran_duras_alt'
   label(31)='transit duration (days)'
!
   suffix(32)='M2earth'
   label(32)='M\d2\u (Earth masses)'
!
   suffix(33)='R2earth'
   label(33)='R\d2\u (Earth radii)'
!
   suffix(34)='period2days'
   label(34)='binary orbital period (days)'
!
   suffix(35)='a2AU'
   label(35)='binary semimajor axis (AU)'
!
   suffix(36)='ecc2'
   label(36)='binary eccentricity'
!
   suffix(37)='argper2deg'
   label(37)='binary argument of periastron (degrees)'
!
   suffix(38)='incl2deg'
   label(38)='binary inclination (degrees)'
!
   suffix(39)='Omega2deg'
   label(39)='binary nodal angle (degrees)'
!
   suffix(40)='Tconj2'
   label(40)='binary T_conj'
!
   suffix(41)='Tperi2'
   label(41)='binary periastron passage'
!
   suffix(42)='mutual2deg'
   label(42)='binary mutual inclination (degrees)'
!
   suffix(45)='M3solar'
   label(45)='M\d3\u (solar masses)'
!
   suffix(46)='R3solar'
   label(46)='R\d3\u (solar radii)'
!
   suffix(47)='M3earth'
   label(47)='M\d3\u (Earth masses)'
!
   suffix(48)='R3earth'
   label(48)='R\d3\u (Earth radii)'
!
   suffix(49)='log_g3'
   label(49)='log_g3 (cgs)'
!
   suffix(50)='den3'
   label(50)='density of body 3 (g/cc)'
!
   suffix(51)='period3days'
   label(51)='period of body 3 (days)'
!
   suffix(52)='a3AU'
   label(52)='semimajor axis of body 3 orbit (AU)'
!
   suffix(53)='ecc3'
   label(53)='eccentricity of body 3 orbit'
!
   suffix(54)='argper3deg'
   label(54)='argument of periastron of body 3 orbit (degrees)'
!
   suffix(55)='incl3deg'
   label(55)='inclination of body 3 orbit (degrees)'
!
   suffix(56)='Omega3deg'
   label(56)='nodal angle of body 3 orbit (degrees)'
!
   suffix(57)='Tconj3'
   label(57)='body 3 T_conj'
!
   suffix(58)='Tperi3'
   label(58)='body 3 T_peri'
!
   suffix(59)='mutual3deg'
   label(59)='mutual inclination of body 3 orbit (degrees)'
!
   suffix(60)='M4solar'
   label(60)='M\d4\u (solar masses)'
!
   suffix(61)='R4solar'
   label(61)='R\d4\u (solar radii)'
!
   suffix(62)='M4earth'
   label(62)='M\d4\u (Earth masses)'
!
   suffix(63)='R4earth'
   label(63)='R\d4\u (Earth radii)'
!
   suffix(64)='log_g4'
   label(64)='log_g4 (cgs)'
!
   suffix(65)='den4'
   label(65)='density of body 4 (g/cc)'
!
   suffix(66)='period4days'
   label(66)='period of body 4 orbit (days)'
!
   suffix(67)='a4AU'
   label(67)='semimajor axis of body 4 orbit (AU)'
!
   suffix(68)='ecc4'
   label(68)='eccentricity of body 4 orbit'
!
   suffix(69)='argper4deg'
   label(69)='argument of periastron of body 4 orbit (degrees)'
!
   suffix(70)='incl4deg'
   label(70)='inclination of body 4 orbit (degrees)'
!
   suffix(71)='Omega4deg'
   label(71)='nodal angle of body 4 orbit (degrees)'
!
   suffix(72)='Tconj4'
   label(72)='body 4 orbit T_conj'
!
   suffix(73)='Tperi4'
   label(73)='body 4 orbit T_peri'
!
   suffix(74)='mutual4deg'
   label(74)='mutual inclination of body 4 orbit (degrees)'
!
   suffix(75)='M5solar'
   label(75)='M\d5\u (solar masses)'
!
   suffix(76)='R5solar'
   label(76)='R\d5\u (solar radii)'
!
   suffix(77)='M5earth'
   label(77)='M\d5\u (Earth masses)'
!
   suffix(78)='R5earth'
   label(78)='R\d5\u (Earth radii)'
!
   suffix(79)='log_g5'
   label(79)='log_g5 (cgs)'
!
   suffix(80)='den5'
   label(80)='density of body 5 (g/cc)'
!
   suffix(81)='period5days'
   label(81)='period of body 5 orbit (days)'
!
   suffix(82)='a5AU'
   label(82)='semimajor axis of body 5 orbit (AU)'
!
   suffix(83)='ecc5'
   label(83)='eccentricity of body 5 orbit'
!
   suffix(84)='argper5deg'
   label(84)='argument of periastron of body 5 orbit (degrees)'
!
   suffix(85)='incl5deg'
   label(85)='inclination of body 5 orbit (degrees)'
!
   suffix(86)='Omega5deg'
   label(86)='nodal angle of body 5 orbit (degrees)'
!
   suffix(87)='Tconj5'
   label(87)='body 5 orbit T_conj'
!
   suffix(88)='Tperi5'
   label(88)='body 5 orbit T_peri'
!
   suffix(89)='mutual5deg'
   label(89)='mutual inclination of body 5 orbit (degrees)'
!
   suffix(90)='M6solar'
   label(90)='M\d6\u (solar masses)'
!
   suffix(91)='R6solar'
   label(91)='R\d6\u (solar radii)'
!
   suffix(92)='M6earth'
   label(92)='M\d6\u (Earth masses)'
!
   suffix(93)='R6earth'
   label(93)='R\d6\u (Earth radii)'
!
   suffix(94)='log_g6'
   label(94)='log_g6 (cgs)'
!
   suffix(95)='den6'
   label(95)='density of body 6 (g/cc)'
!
   suffix(96)='period6days'
   label(96)='period of body 6 orbit (days)'
!
   suffix(97)='a6AU'
   label(97)='semimajor axis of body 6 orbit (AU)'
!
   suffix(98)='ecc6'
   label(98)='eccentricity of body 6 orbit'
!
   suffix(99)='argper6deg'
   label(99)='argument of periastron of body 6 orbit (degrees)'
!
   suffix(100)='incl6deg'
   label(100)='inclination of body 6 orbit (degrees)'
!
   suffix(101)='Omega6deg'
   label(101)='nodal angle of body 6 orbit (degrees)'
!
   suffix(102)='Tconj6'
   label(102)='body 6 orbit T_conj'
!
   suffix(103)='Tperi6'
   label(103)='body 6 orbit T_peri'
!
   suffix(104)='mutual6deg'
   label(104)='mutual inclination of body 6 orbit (degrees)'
!
   suffix(105)='M7solar'
   label(105)='M\d7\u (solar masses)'
!
   suffix(106)='R7solar'
   label(106)='R\d7\u (solar radii)'
!
   suffix(107)='M7earth'
   label(107)='M\d7\u (Earth masses)'
!
   suffix(108)='R7earth'
   label(108)='R\d7\u (Earth radii)'
!
   suffix(109)='log_g7'
   label(109)='log_g7 (cgs)'
!
   suffix(110)='den7'
   label(110)='density of body 7 (g/cc)'
!
   suffix(111)='period7days'
   label(111)='period of body 7 orbit (days)'
!
   suffix(112)='a7AU'
   label(112)='semimajor axis of body 7 orbit (AU)'
!
   suffix(113)='ecc7'
   label(113)='eccentricity of body 7 orbit'
!
   suffix(114)='argper7deg'
   label(114)='argument of periastron of body 7 orbit (degrees)'
!
   suffix(115)='incl7deg'
   label(115)='inclination of body 7 orbit (degrees)'
!
   suffix(116)='Omega7deg'
   label(116)='nodal angle of body 7 orbit (degrees)'
!
   suffix(117)='Tconj7'
   label(117)='body 7 orbit T_conj'
!
   suffix(118)='Tperi7'
   label(118)='body 7 orbit T_peri'
!
   suffix(119)='mutual7deg'
   label(119)='mutual inclination of body 7 orbit (degrees)'
!
   suffix(120)='M8solar'
   label(120)='M\d8\u (solar masses)'
!
   suffix(121)='R8solar'
   label(121)='R\d8\u (solar radii)'
!
   suffix(122)='M8earth'
   label(122)='M\d8\u (Earth masses)'
!
   suffix(123)='R8earth'
   label(123)='R\d8\u (Earth radii)'
!
   suffix(124)='log_g8'
   label(124)='log_g8 (cgs)'
!
   suffix(125)='den8'
   label(125)='density of body 8 (g/cc)'
!
   suffix(126)='period8days'
   label(126)='period of body 8 orbit (days)'
!
   suffix(127)='a8AU'
   label(127)='semimajor axis of body 8 orbit (AU)'
!
   suffix(128)='ecc8'
   label(128)='eccentricity of body 8 orbit'
!
   suffix(129)='argper8deg'
   label(129)='argument of periastron of body 8 orbit (degrees)'
!
   suffix(130)='incl8deg'
   label(130)='inclination of body 8 orbit (degrees)'
!
   suffix(131)='Omega8deg'
   label(131)='nodal angle of body 8 orbit (degrees)'
!
   suffix(132)='Tconj8'
   label(132)='body 8 orbit T_conj'
!
   suffix(133)='Tperi8'
   label(133)='body 8 orbit T_peri'
!
   suffix(134)='mutual8deg'
   label(134)='mutual inclination of body 8 orbit (degrees)'
!
   suffix(135)='M9solar'
   label(135)='M\d9\u (solar masses)'
!
   suffix(136)='R9solar'
   label(136)='R\d9\u (solar radii)'
!
   suffix(137)='M9earth'
   label(137)='M\d9\u (Earth masses)'
!
   suffix(138)='R9earth'
   label(138)='R\d9\u (Earth radii)'
!
   suffix(139)='log_g9'
   label(139)='log_g9 (cgs)'
!
   suffix(140)='den9'
   label(140)='density of body 9 (g/cc)'
!
   suffix(141)='period9days'
   label(141)='period of body 9 orbit (days)'
!
   suffix(142)='a9AU'
   label(142)='semimajor axis of body 9 orbit (AU)'
!
   suffix(143)='ecc9'
   label(143)='eccentricity of body 9 orbit'
!
   suffix(144)='argper9deg'
   label(144)='argument of periastron of body 9 orbit (degrees)'
!
   suffix(145)='incl9deg'
   label(145)='inclination of body 9 orbit (degrees)'
!
   suffix(146)='Omega9deg'
   label(146)='nodal angle of body 9 orbit (degrees)'
!
   suffix(147)='Tconj9'
   label(147)='body 9 orbit T_conj'
!
   suffix(148)='Tperi9'
   label(148)='body 9 orbit T_peri'
!
   suffix(149)='mutual9deg'
   label(149)='mutual inclination of body 9 orbit (degrees)'
!
   suffix(150)='M10solar'
   label(150)='M\d10\u (solar masses)'
!
   suffix(151)='R10solar'
   label(151)='R\d10\u (solar radii)'
!
   suffix(152)='M10earth'
   label(152)='M\d10\u (Earth masses)'
!
   suffix(153)='R10earth'
   label(153)='R\d10\u (Earth radii)'
!
   suffix(154)='log_g10'
   label(154)='log_g10 (cgs)'
!
   suffix(155)='den10'
   label(155)='density of body 10 (g/cc)'
!
   suffix(156)='period10days'
   label(156)='period of body 10 orbit (days)'
!
   suffix(157)='a10AU'
   label(157)='semimajor axis of body 10 orbit (AU)'
!
   suffix(158)='ecc10'
   label(158)='eccentricity of body 10 orbit'
!
   suffix(159)='argper10deg'
   label(159)='argument of periastron of body 10 orbit (degrees)'
!
   suffix(160)='incl10deg'
   label(160)='inclination of body 10 orbit (degrees)'
!
   suffix(161)='Omega10deg'
   label(161)='nodal angle of body 10 orbit (degrees)'
!
   suffix(162)='Tconj10'
   label(162)='body 10 orbit T_conj'
!
   suffix(163)='Tperi10'
   label(163)='body 10 orbit T_peri'
!
   suffix(164)='mutual10deg'
   label(164)='mutual inclination of body 10 orbit (degrees)'
!
!
   RETURN
!
END SUBROUTINE getelcparmstring
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getelcparmstringnbsh(suffix,label)
!
   USE accur

   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(OUT)           :: suffix(999)
   CHARACTER (LEN=*), INTENT(OUT)           :: label(999)
!
   suffix(3)='M1solar'
   label(3)='M1 (solar masses)'
!
   suffix(4)='M2solar'
   label(4)='M2 (solar masses)'
!
   suffix(5)='R1solar'
   label(5)='R1 (solar radii)'
!
   suffix(6)='R2solar'
   label(6)='R2 (solar radii)'
!
   suffix(7)='log_g1'
   label(7)='log_g1 (cgs)'
!
   suffix(8)='log_g2'
   label(8)='log_g2 (cgs)'
!
   suffix(9)='den1'
   label(9)='density of star 1 (g/cc)'
!
   suffix(10)='den2'
   label(10)='density of star 2 (g/cc)'
!
   suffix(11)='fill1'
   label(11)='filling factor of star 1'
!
   suffix(12)='fill2'
   label(12)='filling factor of star 2'
!
   suffix(13)='K1'
   label(13)='K1 (km/sec)'
!
   suffix(14)='K2'
   label(14)='K2 (km/sec)'
!
   suffix(15)='Vrot1'
   label(15)='Vrot1 (km/sec)'
!
   suffix(16)='Vrot2'
   label(16)='Vrot2 (km/sec)'
!
   suffix(17)='Vrot1_omega1'
   label(17)='Vrot1_omega1 (km/sec)'
!
   suffix(18)='Vrot2_omega2'
   label(18)='Vrot2_omega2 (km/sec)'
!
   suffix(19)='tspot_1_1'
   label(19)='tspot_1_1 (K)'
!
   suffix(20)='tspot_2_1'
   label(20)='tspot_2_1 (K)'
!
   suffix(21)='tspot_1_2'
   label(21)='tspot_1_2 (K)'
!
   suffix(22)='tspot_2_2'
   label(22)='tspot_2_2 (K)'
!
   suffix(23)='tspot_1_disk'
   label(23)='tspot_1_disk (K)'
!
   suffix(24)='tspot_2_disk'
   label(24)='tspot_2_disk (K)'
!
   suffix(25)='diskrad'
   label(25)='disk radius (solar radii)'
!
   suffix(26)='diskthick'
   label(26)='disk thickness (solar radii)'
!
   suffix(27)='X_duras'
   label(27)='X-ray eclipse duration (degrees)'
!
   suffix(28)='impact1'
   label(28)='impact parameter 1'
!
   suffix(29)='tran_duras'
   label(29)='transit duration (days)'
!
   suffix(30)='tran_depth'
   label(30)='transit depth'
!
   suffix(31)='tran_duras_alt'
   label(31)='transit duration (days)'
!
   suffix(32)='M2earth'
   label(32)='M2 (Earth masses)'
!
   suffix(33)='R2earth'
   label(33)='R2 (Earth radii)'
!
   suffix(34)='period2days'
   label(34)='binary orbital period (days)'
!
   suffix(35)='a2AU'
   label(35)='binary semimajor axis (AU)'
!
   suffix(36)='ecc2'
   label(36)='binary eccentricity'
!
   suffix(37)='argper2deg'
   label(37)='binary argument of periastron (degrees)'
!
   suffix(38)='incl2deg'
   label(38)='binary inclination (degrees)'
!
   suffix(39)='Omega2deg'
   label(39)='binary nodal angle (degrees)'
!
   suffix(40)='Tconj2'
   label(40)='binary T_conj'
!
   suffix(41)='Tperi2'
   label(41)='binary periastron passage'
!
   suffix(42)='mutual2deg'
   label(42)='binary mutual inclination (degrees)'
!
   suffix(45)='M3solar'
   label(45)='M3 (solar masses)'
!
   suffix(46)='R3solar'
   label(46)='R3 (solar radii)'
!
   suffix(47)='M3earth'
   label(47)='M3 (Earth masses)'
!
   suffix(48)='R3earth'
   label(48)='R3 (Earth radii)'
!
   suffix(49)='log_g3'
   label(49)='log_g3 (cgs)'
!
   suffix(50)='den3'
   label(50)='density of body 3 (g/cc)'
!
   suffix(51)='period3days'
   label(51)='period of body 3 (days)'
!
   suffix(52)='a3AU'
   label(52)='semimajor axis of body 3 orbit (AU)'
!
   suffix(53)='ecc3'
   label(53)='eccentricity of body 3 orbit'
!
   suffix(54)='argper3deg'
   label(54)='argument of periastron of body 3 orbit (degrees)'
!
   suffix(55)='incl3deg'
   label(55)='inclination of body 3 orbit (degrees)'
!
   suffix(56)='Omega3deg'
   label(56)='nodal angle of body 3 orbit (degrees)'
!
   suffix(57)='Tconj3'
   label(57)='body 3 T_conj'
!
   suffix(58)='Tperi3'
   label(58)='body 3 T_peri'
!
   suffix(59)='mutual3deg'
   label(59)='mutual inclination of body 3 orbit (degrees)'
!
   suffix(60)='M4solar'
   label(60)='M4 (solar masses)'
!
   suffix(61)='R4solar'
   label(61)='R4 (solar radii)'
!
   suffix(62)='M4earth'
   label(62)='M4 (Earth masses)'
!
   suffix(63)='R4earth'
   label(63)='R4 (Earth radii)'
!
   suffix(64)='log_g4'
   label(64)='log_g4 (cgs)'
!
   suffix(65)='den4'
   label(65)='density of body 4 (g/cc)'
!
   suffix(66)='period4days'
   label(66)='period of body 4 orbit (days)'
!
   suffix(67)='a4AU'
   label(67)='semimajor axis of body 4 orbit (AU)'
!
   suffix(68)='ecc4'
   label(68)='eccentricity of body 4 orbit'
!
   suffix(69)='argper4deg'
   label(69)='argument of periastron of body 4 orbit (degrees)'
!
   suffix(70)='incl4deg'
   label(70)='inclination of body 4 orbit (degrees)'
!
   suffix(71)='Omega4deg'
   label(71)='nodal angle of body 4 orbit (degrees)'
!
   suffix(72)='Tconj4'
   label(72)='body 4 orbit T_conj'
!
   suffix(73)='Tperi4'
   label(73)='body 4 orbit T_peri'
!
   suffix(74)='mutual4deg'
   label(74)='mutual inclination of body 4 orbit (degrees)'
!
   suffix(75)='M5solar'
   label(75)='M5 (solar masses)'
!
   suffix(76)='R5solar'
   label(76)='R5 (solar radii)'
!
   suffix(77)='M5earth'
   label(77)='M5 (Earth masses)'
!
   suffix(78)='R5earth'
   label(78)='R5 (Earth radii)'
!
   suffix(79)='log_g5'
   label(79)='log_g5 (cgs)'
!
   suffix(80)='den5'
   label(80)='density of body 5 (g/cc)'
!
   suffix(81)='period5days'
   label(81)='period of body 5 orbit (days)'
!
   suffix(82)='a5AU'
   label(82)='semimajor axis of body 5 orbit (AU)'
!
   suffix(83)='ecc5'
   label(83)='eccentricity of body 5 orbit'
!
   suffix(84)='argper5deg'
   label(84)='argument of periastron of body 5 orbit (degrees)'
!
   suffix(85)='incl5deg'
   label(85)='inclination of body 5 orbit (degrees)'
!
   suffix(86)='Omega5deg'
   label(86)='nodal angle of body 5 orbit (degrees)'
!
   suffix(87)='Tconj5'
   label(87)='body 5 orbit T_conj'
!
   suffix(88)='Tperi5'
   label(88)='body 5 orbit T_peri'
!
   suffix(89)='mutual5deg'
   label(89)='mutual inclination of body 5 orbit (degrees)'
!
   suffix(90)='M6solar'
   label(90)='M6 (solar masses)'
!
   suffix(91)='R6solar'
   label(91)='R6 (solar radii)'
!
   suffix(92)='M6earth'
   label(92)='M6 (Earth masses)'
!
   suffix(93)='R6earth'
   label(93)='R6 (Earth radii)'
!
   suffix(94)='log_g6'
   label(94)='log_g6 (cgs)'
!
   suffix(95)='den6'
   label(95)='density of body 6 (g/cc)'
!
   suffix(96)='period6days'
   label(96)='period of body 6 orbit (days)'
!
   suffix(97)='a6AU'
   label(97)='semimajor axis of body 6 orbit (AU)'
!
   suffix(98)='ecc6'
   label(98)='eccentricity of body 6 orbit'
!
   suffix(99)='argper6deg'
   label(99)='argument of periastron of body 6 orbit (degrees)'
!
   suffix(100)='incl6deg'
   label(100)='inclination of body 6 orbit (degrees)'
!
   suffix(101)='Omega6deg'
   label(101)='nodal angle of body 6 orbit (degrees)'
!
   suffix(102)='Tconj6'
   label(102)='body 6 orbit T_conj'
!
   suffix(103)='Tperi6'
   label(103)='body 6 orbit T_peri'
!
   suffix(104)='mutual6deg'
   label(104)='mutual inclination of body 6 orbit (degrees)'
!
   suffix(105)='M7solar'
   label(105)='M7 (solar masses)'
!
   suffix(106)='R7solar'
   label(106)='R7 (solar radii)'
!
   suffix(107)='M7earth'
   label(107)='M7 (Earth masses)'
!
   suffix(108)='R7earth'
   label(108)='R7 (Earth radii)'
!
   suffix(109)='log_g7'
   label(109)='log_g7 (cgs)'
!
   suffix(110)='den7'
   label(110)='density of body 7 (g/cc)'
!
   suffix(111)='period7days'
   label(111)='period of body 7 orbit (days)'
!
   suffix(112)='a7AU'
   label(112)='semimajor axis of body 7 orbit (AU)'
!
   suffix(113)='ecc7'
   label(113)='eccentricity of body 7 orbit'
!
   suffix(114)='argper7deg'
   label(114)='argument of periastron of body 7 orbit (degrees)'
!
   suffix(115)='incl7deg'
   label(115)='inclination of body 7 orbit (degrees)'
!
   suffix(116)='Omega7deg'
   label(116)='nodal angle of body 7 orbit (degrees)'
!
   suffix(117)='Tconj7'
   label(117)='body 7 orbit T_conj'
!
   suffix(118)='Tperi7'
   label(118)='body 7 orbit T_peri'
!
   suffix(119)='mutual7deg'
   label(119)='mutual inclination of body 7 orbit (degrees)'
!
   suffix(120)='M8solar'
   label(120)='M8 (solar masses)'
!
   suffix(121)='R8solar'
   label(121)='R8 (solar radii)'
!
   suffix(122)='M8earth'
   label(122)='M8 (Earth masses)'
!
   suffix(123)='R8earth'
   label(123)='R8 (Earth radii)'
!
   suffix(124)='log_g8'
   label(124)='log_g8 (cgs)'
!
   suffix(125)='den8'
   label(125)='density of body 8 (g/cc)'
!
   suffix(126)='period8days'
   label(126)='period of body 8 orbit (days)'
!
   suffix(127)='a8AU'
   label(127)='semimajor axis of body 8 orbit (AU)'
!
   suffix(128)='ecc8'
   label(128)='eccentricity of body 8 orbit'
!
   suffix(129)='argper8deg'
   label(129)='argument of periastron of body 8 orbit (degrees)'
!
   suffix(130)='incl8deg'
   label(130)='inclination of body 8 orbit (degrees)'
!
   suffix(131)='Omega8deg'
   label(131)='nodal angle of body 8 orbit (degrees)'
!
   suffix(132)='Tconj8'
   label(132)='body 8 orbit T_conj'
!
   suffix(133)='Tperi8'
   label(133)='body 8 orbit T_peri'
!
   suffix(134)='mutual8deg'
   label(134)='mutual inclination of body 8 orbit (degrees)'
!
   suffix(135)='M9solar'
   label(135)='M9 (solar masses)'
!
   suffix(136)='R9solar'
   label(136)='R9 (solar radii)'
!
   suffix(137)='M9earth'
   label(137)='M9 (Earth masses)'
!
   suffix(138)='R9earth'
   label(138)='R9 (Earth radii)'
!
   suffix(139)='log_g9'
   label(139)='log_g9 (cgs)'
!
   suffix(140)='den9'
   label(140)='density of body 9 (g/cc)'
!
   suffix(141)='period9days'
   label(141)='period of body 9 orbit (days)'
!
   suffix(142)='a9AU'
   label(142)='semimajor axis of body 9 orbit (AU)'
!
   suffix(143)='ecc9'
   label(143)='eccentricity of body 9 orbit'
!
   suffix(144)='argper9deg'
   label(144)='argument of periastron of body 9 orbit (degrees)'
!
   suffix(145)='incl9deg'
   label(145)='inclination of body 9 orbit (degrees)'
!
   suffix(146)='Omega9deg'
   label(146)='nodal angle of body 9 orbit (degrees)'
!
   suffix(147)='Tconj9'
   label(147)='body 9 orbit T_conj'
!
   suffix(148)='Tperi9'
   label(148)='body 9 orbit T_peri'
!
   suffix(149)='mutual9deg'
   label(149)='mutual inclination of body 9 orbit (degrees)'
!
   suffix(150)='M10solar'
   label(150)='M10 (solar masses)'
!
   suffix(151)='R10solar'
   label(151)='R10 (solar radii)'
!
   suffix(152)='M10earth'
   label(152)='M10 (Earth masses)'
!
   suffix(153)='R10earth'
   label(153)='R10 (Earth radii)'
!
   suffix(154)='log_g10'
   label(154)='log_g10 (cgs)'
!
   suffix(155)='den10'
   label(155)='density of body 10 (g/cc)'
!
   suffix(156)='period10days'
   label(156)='period of body 10 orbit (days)'
!
   suffix(157)='a10AU'
   label(157)='semimajor axis of body 10 orbit (AU)'
!
   suffix(158)='ecc10'
   label(158)='eccentricity of body 10 orbit'
!
   suffix(159)='argper10deg'
   label(159)='argument of periastron of body 10 orbit (degrees)'
!
   suffix(160)='incl10deg'
   label(160)='inclination of body 10 orbit (degrees)'
!
   suffix(161)='Omega10deg'
   label(161)='nodal angle of body 10 orbit (degrees)'
!
   suffix(162)='Tconj10'
   label(162)='body 10 orbit T_conj'
!
   suffix(163)='Tperi10'
   label(163)='body 10 orbit T_peri'
!
   suffix(164)='mutual10deg'
   label(164)='mutual inclination of body 10 orbit (degrees)'
!
!
   RETURN
!
END SUBROUTINE getelcparmstringnbsh
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getelcratiostring(suffix,label)
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(OUT)           :: suffix(999)
   CHARACTER (LEN=*), INTENT(OUT)           :: label(999)
!
   suffix(1)='F2divF1_U'
   label(1)='flux fraction (star 2/star 1), U'
!
   suffix(2)='F2divF1_B'
   label(2)='flux fraction (star 2/star 1), B'
!
   suffix(3)='F2divF1_V'
   label(3)='flux fraction (star 2/star 1), V'
!
   suffix(4)='F2divF1_R'
   label(4)='flux fraction (star 2/star 1), R'
!
   suffix(5)='F2divF1_I'
   label(5)='flux fraction (star 2/star 1), I'
!
   suffix(6)='F2divF1_J'
   label(6)='flux fraction (star 2/star 1), J'
!
   suffix(7)='F2divF1_H'
   label(7)='flux fraction (star 2/star 1), H'
!
   suffix(8)='F2divF1_K'
   label(8)='flux fraction (star 2/star 1), K'
!
   suffix(9)='diskfrac_U'
   label(9)='disk fraction, U'
!
   suffix(10)='diskfrac_B'
   label(10)='disk fraction, B'
!
   suffix(11)='diskfrac_V'
   label(11)='disk fraction, V'
!
   suffix(12)='diskfrac_R'
   label(12)='disk fraction, R'
!
   suffix(13)='diskfrac_I'
   label(13)='disk fraction, I'
!
   suffix(14)='diskfrac_J'
   label(14)='disk fraction, J'
!
   suffix(15)='diskfrac_H'
   label(15)='disk fraction, H'
!
   suffix(16)='diskfrac_K'
   label(16)='disk fraction, K'
!
   suffix(17)='F3divF1_U'
   label(17)='flux fraction (star 3/star 1), U'
!
   suffix(18)='F3divF1_B'
   label(18)='flux fraction (star 3/star 1), B'
!
   suffix(19)='F3divF1_V'
   label(19)='flux fraction (star 3/star 1), V'
!
   suffix(20)='F3divF1_R'
   label(20)='flux fraction (star 3/star 1), R'
!
   suffix(21)='F3divF1_I'
   label(21)='flux fraction (star 3/star 1), I'
!
   suffix(22)='F3divF1_J'
   label(22)='flux fraction (star 3/star 1), J'
!
   suffix(23)='F3divF1_H'
   label(23)='flux fraction (star 3/star 1), H'
!
   suffix(24)='F3divF1_K'
   label(24)='flux fraction (star 3/star 1), K'
!
   RETURN
!
END SUBROUTINE getelcratiostring
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getfile(filein)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER(LEN=40), INTENT(OUT)          :: filein
!
   INTEGER :: ios
!
!   See if the file name was put on the command line.
!
   ios=0
   CALL get_command_argument(1,filein)
   IF(filein == '')THEN
      WRITE(*,*)'enter the input file'
      READ(*,100)filein
   END IF
!
!   Attempt to open the file.
!
   OPEN(UNIT=20,FILE=filein,STATUS='old',IOSTAT=ios)
   IF(ios == 0)RETURN
!
!   Problem opening file
!
   WRITE(*,200)filein
   STOP
!
100 FORMAT(a40)
200 FORMAT('Error opening file',1X,a)
!
END SUBROUTINE getfile
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getgridbody3(kkk,nalph3,nbet3,tertperiod,tertt0,  &
   tertecos,tertesin,tertincl,tertomega,tertq,dwavex,dwavey,  &
   itconj,it1,it2,it3,it4,tertconj,tertratrad,hh,sw72,sw73,  &
   p2tconj,p2period,p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,  &
   p2ratrad,p3tconj,p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,  &
   p3q,p3ratrad,p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,  &
   p4omega,p4q,p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,  &
   p5incl,p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,  &
   p6esin,p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,  &
   p7ecos,p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,  &
   p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,nbody)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: kkk
   INTEGER, INTENT(OUT)                     :: nalph3
   INTEGER, INTENT(OUT)                     :: nbet3
   REAL(KIND=dp), INTENT(OUT)               :: tertperiod
   REAL(KIND=dp), INTENT(OUT)               :: tertt0
   REAL(KIND=dp), INTENT(OUT)               :: tertecos
   REAL(KIND=dp), INTENT(OUT)               :: tertesin
   REAL(KIND=dp), INTENT(OUT)               :: tertincl
   REAL(KIND=dp), INTENT(OUT)               :: tertomega
   REAL(KIND=dp), INTENT(OUT)               :: tertq
   REAL(KIND=dp), INTENT(IN OUT)            :: dwavex(8,10)
   REAL(KIND=dp), INTENT(IN OUT)            :: dwavey(8,10)
   INTEGER, INTENT(OUT)                     :: itconj
   INTEGER, INTENT(OUT)                     :: it1
   INTEGER, INTENT(OUT)                     :: it2
   INTEGER, INTENT(OUT)                     :: it3
   INTEGER, INTENT(OUT)                     :: it4
   REAL(KIND=dp), INTENT(OUT)               :: tertconj
   REAL(KIND=dp), INTENT(OUT)               :: tertratrad
   REAL(KIND=dp), INTENT(OUT)               :: hh
   REAL(KIND=dp), INTENT(OUT)               :: sw72
   REAL(KIND=dp), INTENT(OUT)               :: sw73
   REAL(KIND=dp), INTENT(OUT)               :: p2tconj
   REAL(KIND=dp), INTENT(OUT)               :: p2period
   REAL(KIND=dp), INTENT(OUT)               :: p2t0
   REAL(KIND=dp), INTENT(OUT)               :: p2ecos
   REAL(KIND=dp), INTENT(OUT)               :: p2esin
   REAL(KIND=dp), INTENT(OUT)               :: p2incl
   REAL(KIND=dp), INTENT(OUT)               :: p2omega
   REAL(KIND=dp), INTENT(OUT)               :: p2q
   REAL(KIND=dp), INTENT(OUT)               :: p2ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p3tconj
   REAL(KIND=dp), INTENT(OUT)               :: p3period
   REAL(KIND=dp), INTENT(OUT)               :: p3t0
   REAL(KIND=dp), INTENT(OUT)               :: p3ecos
   REAL(KIND=dp), INTENT(OUT)               :: p3esin
   REAL(KIND=dp), INTENT(OUT)               :: p3incl
   REAL(KIND=dp), INTENT(OUT)               :: p3omega
   REAL(KIND=dp), INTENT(OUT)               :: p3q
   REAL(KIND=dp), INTENT(OUT)               :: p3ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p4tconj
   REAL(KIND=dp), INTENT(OUT)               :: p4period
   REAL(KIND=dp), INTENT(OUT)               :: p4t0
   REAL(KIND=dp), INTENT(OUT)               :: p4ecos
   REAL(KIND=dp), INTENT(OUT)               :: p4esin
   REAL(KIND=dp), INTENT(OUT)               :: p4incl
   REAL(KIND=dp), INTENT(OUT)               :: p4omega
   REAL(KIND=dp), INTENT(OUT)               :: p4q
   REAL(KIND=dp), INTENT(OUT)               :: p4ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p5tconj
   REAL(KIND=dp), INTENT(OUT)               :: p5period
   REAL(KIND=dp), INTENT(OUT)               :: p5t0
   REAL(KIND=dp), INTENT(OUT)               :: p5ecos
   REAL(KIND=dp), INTENT(OUT)               :: p5esin
   REAL(KIND=dp), INTENT(OUT)               :: p5incl
   REAL(KIND=dp), INTENT(OUT)               :: p5omega
   REAL(KIND=dp), INTENT(OUT)               :: p5q
   REAL(KIND=dp), INTENT(OUT)               :: p5ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p6tconj
   REAL(KIND=dp), INTENT(OUT)               :: p6period
   REAL(KIND=dp), INTENT(OUT)               :: p6t0
   REAL(KIND=dp), INTENT(OUT)               :: p6ecos
   REAL(KIND=dp), INTENT(OUT)               :: p6esin
   REAL(KIND=dp), INTENT(OUT)               :: p6incl
   REAL(KIND=dp), INTENT(OUT)               :: p6omega
   REAL(KIND=dp), INTENT(OUT)               :: p6q
   REAL(KIND=dp), INTENT(OUT)               :: p6ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p7tconj
   REAL(KIND=dp), INTENT(OUT)               :: p7period
   REAL(KIND=dp), INTENT(OUT)               :: p7t0
   REAL(KIND=dp), INTENT(OUT)               :: p7ecos
   REAL(KIND=dp), INTENT(OUT)               :: p7esin
   REAL(KIND=dp), INTENT(OUT)               :: p7incl
   REAL(KIND=dp), INTENT(OUT)               :: p7omega
   REAL(KIND=dp), INTENT(OUT)               :: p7q
   REAL(KIND=dp), INTENT(OUT)               :: p7ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p8tconj
   REAL(KIND=dp), INTENT(OUT)               :: p8period
   REAL(KIND=dp), INTENT(OUT)               :: p8t0
   REAL(KIND=dp), INTENT(OUT)               :: p8ecos
   REAL(KIND=dp), INTENT(OUT)               :: p8esin
   REAL(KIND=dp), INTENT(OUT)               :: p8incl
   REAL(KIND=dp), INTENT(OUT)               :: p8omega
   REAL(KIND=dp), INTENT(OUT)               :: p8q
   REAL(KIND=dp), INTENT(OUT)               :: p8ratrad
   INTEGER, INTENT(IN OUT)                  :: nbody
!
   INTEGER :: i,ios
!
!   Declare the variable bell to be character*1
!
   CHARACTER (LEN=1) :: bell
   CHARACTER (LEN=4) :: extension
!
   IF (kkk > 9999) THEN
      bell=CHAR(7)
      WRITE(*,70)bell
      STOP
   END IF
!
   IF (kkk < 10) WRITE(extension,30)kkk
   IF ((kkk >= 10).AND.(kkk < 100)) WRITE(extension,40)kkk
   IF ((kkk >= 100).AND.(kkk < 1000)) WRITE(extension,50)kkk
   IF (kkk >= 1000) WRITE (extension,60)kkk+1000
!
   ios=0
   OPEN(UNIT=1,FILE='ELCbody3.'//extension,STATUS='old',ERR=10,IOSTAT=ios)
!
   READ(1,*,END=20,ERR=20)nalph3
   READ(1,*,END=20,ERR=20)nbet3
   READ(1,*,END=20,ERR=20)itconj
   READ(1,*,END=20,ERR=20)it1
   READ(1,*,END=20,ERR=20)it2
   READ(1,*,END=20,ERR=20)it3
   READ(1,*,END=20,ERR=20)it4
   READ(1,*,END=20,ERR=20)tertconj
   READ(1,*,END=20,ERR=20)tertperiod
   READ(1,*,END=20,ERR=20)tertt0
   READ(1,*,END=20,ERR=20)tertecos
   READ(1,*,END=20,ERR=20)tertesin
   READ(1,*,END=20,ERR=20)tertincl
   READ(1,*,END=20,ERR=20)tertomega
   READ(1,*,END=20,ERR=20)tertq
!
!  Load the limb darkening parameters for body 3
!
   DO i=1,8
      READ(1,*,END=20,ERR=20)dwavex(i,3),dwavey(i,3)
   END DO
!
   READ(1,*,END=20,ERR=20)tertratrad
   READ(1,*,END=20,ERR=20)hh
   READ(1,*,END=20,ERR=20)sw72
   READ(1,*,END=20,ERR=20)sw73
!
!   body 4 parameters.  If Nbody < 4, then zero out
!
   READ(1,*)p2tconj
   READ(1,*)p2period
   READ(1,*)p2t0
   READ(1,*)p2ecos
   READ(1,*)p2esin
   READ(1,*)p2incl
   READ(1,*)p2omega
   READ(1,*)p2q
   READ(1,*)p2ratrad
   IF(nbody < 4)THEN
      p2tconj=0.0_dp
      p2period=0.0_dp
      p2t0=0.0_dp
      p2ecos=0.0_dp
      p2esin=0.0_dp
      p2incl=0.0_dp
      p2omega=0.0_dp
      p2q=0.0_dp
      p2ratrad=0.0_dp
   END IF
!
   READ(1,*)p3tconj
   READ(1,*)p3period
   READ(1,*)p3t0
   READ(1,*)p3ecos
   READ(1,*)p3esin
   READ(1,*)p3incl
   READ(1,*)p3omega
   READ(1,*)p3q
   READ(1,*)p3ratrad
   IF(nbody < 5)THEN
      p3tconj=0.0_dp
      p3period=0.0_dp
      p3t0=0.0_dp
      p3ecos=0.0_dp
      p3esin=0.0_dp
      p3incl=0.0_dp
      p3omega=0.0_dp
      p3q=0.0_dp
      p3ratrad=0.0_dp
   END IF
!
   READ(1,*)p4tconj
   READ(1,*)p4period
   READ(1,*)p4t0
   READ(1,*)p4ecos
   READ(1,*)p4esin
   READ(1,*)p4incl
   READ(1,*)p4omega
   READ(1,*)p4q
   READ(1,*)p4ratrad
   IF(nbody < 6)THEN
      p4tconj=0.0_dp
      p4period=0.0_dp
      p4t0=0.0_dp
      p4ecos=0.0_dp
      p4esin=0.0_dp
      p4incl=0.0_dp
      p4omega=0.0_dp
      p4q=0.0_dp
      p4ratrad=0.0_dp
   END IF
!
   READ(1,*)p5tconj
   READ(1,*)p5period
   READ(1,*)p5t0
   READ(1,*)p5ecos
   READ(1,*)p5esin
   READ(1,*)p5incl
   READ(1,*)p5omega
   READ(1,*)p5q
   READ(1,*)p5ratrad
   IF(nbody < 7)THEN
      p5tconj=0.0_dp
      p5period=0.0_dp
      p5t0=0.0_dp
      p5ecos=0.0_dp
      p5esin=0.0_dp
      p5incl=0.0_dp
      p5omega=0.0_dp
      p5q=0.0_dp
      p5ratrad=0.0_dp
   END IF
!
   READ(1,*)p6tconj
   READ(1,*)p6period
   READ(1,*)p6t0
   READ(1,*)p6ecos
   READ(1,*)p6esin
   READ(1,*)p6incl
   READ(1,*)p6omega
   READ(1,*)p6q
   READ(1,*)p6ratrad
   IF(nbody < 8)THEN
      p6tconj=0.0_dp
      p6period=0.0_dp
      p6t0=0.0_dp
      p6ecos=0.0_dp
      p6esin=0.0_dp
      p6incl=0.0_dp
      p6omega=0.0_dp
      p6q=0.0_dp
      p6ratrad=0.0_dp
   END IF
!
   READ(1,*)p7tconj
   READ(1,*)p7period
   READ(1,*)p7t0
   READ(1,*)p7ecos
   READ(1,*)p7esin
   READ(1,*)p7incl
   READ(1,*)p7omega
   READ(1,*)p7q
   READ(1,*)p7ratrad
   IF(nbody < 9)THEN
      p7tconj=0.0_dp
      p7period=0.0_dp
      p7t0=0.0_dp
      p7ecos=0.0_dp
      p7esin=0.0_dp
      p7incl=0.0_dp
      p7omega=0.0_dp
      p7q=0.0_dp
      p7ratrad=0.0_dp
   END IF
!
   READ(1,*)p8tconj
   READ(1,*)p8period
   READ(1,*)p8t0
   READ(1,*)p8ecos
   READ(1,*)p8esin
   READ(1,*)p8incl
   READ(1,*)p8omega
   READ(1,*)p8q
   READ(1,*)p8ratrad
   IF(nbody < 10)THEN
      p8tconj=0.0_dp
      p8period=0.0_dp
      p8t0=0.0_dp
      p8ecos=0.0_dp
      p8esin=0.0_dp
      p8incl=0.0_dp
      p8omega=0.0_dp
      p8q=0.0_dp
      p8ratrad=0.0_dp
   END IF
!
!  Load the limb darkening parameters for body 4
!
   DO i=1,8
      READ(1,*,END=20,ERR=20)dwavex(i,4),dwavey(i,4)
   END DO
!
!  Load the limb darkening parameters for body 5
!
   DO i=1,8
      READ(1,*,END=20,ERR=20)dwavex(i,5),dwavey(i,5)
   END DO
!
   IF(ios == 0)THEN
      CLOSE(1)
      RETURN
   END IF
!
!   Come here if the input file ELCbody3.???? does not exist.
!
10 IF(ios > 0)THEN
      bell=CHAR(7)
      WRITE(*,80)bell,kkk
      STOP
   END IF
!
20 bell=CHAR(7)
   WRITE(*,90)bell,kkk
   CLOSE(1)
!
   RETURN
!
30 FORMAT('100',i1)
40 FORMAT('10',i2)
50 FORMAT('1',i3)
60 FORMAT(i4)
70 FORMAT(a1,'ielete is too large')
80 FORMAT(a1,'Error:  File ELCbody3.???? does not exist for ',  &
      'index ',i4)
90 FORMAT(a1,'Error:  Bad entry in ELCbody3.???? for index ',i4)
!
END SUBROUTINE getgridbody3
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getgridinput(kkk,nalph1,nbet1,nalph2,nbet2,fill1,  &
   fill2,omega1,omega2,dphase,q,finc,teff1,teff2,tgrav1,tgrav2,  &
   betarim,rinner,router,tdisk,xi,ntheta,nradius,alb1,alb2,nref,  &
   rlx,period,fm,separ,gamma,t3,g3,sa3,density,sw1,sw2,sw3,t0,  &
   idraw,iecheck,iidint,iatm,ism1,icnu,icnb,icnv,icnr,icni,icnj,  &
   icnh,icnk,irvfilt,isw1,isw2,isw3,isw4,ilaw,wave,dbolx,dboly,  &
   dwavex,dwavey,ecc,argper,pshift,sw5,sw6,sw7,sw8,sw9,ikeep,  &
   isynch,isw5,isw6,isw7,isw8,isw9,spot1parm,spot2parm,  &
   spotdparm,primmass,primk,primrad,ratrad,frac1,frac2,ecosw,  &
   temprat,idark1,idark2,isw12,isw13,isw21,isw22,isw23,isw24,  &
   bigi,bigbeta,sw23,sw24,powercoeff,sw25,sw26,sw27,sw28,sw29,  &
   sw30,contam,tconj,beam1,beam2,isw25,isw26,isw27,isw28,isw29,  &
   isw30,isw31,isw32,isw33,isw34,ocose,osine,omegadot,contams0,  &
   contams1,contams2,contams3,sw47,sw48,sw49,sw80,sw81,sw82,  &
   sw83,sw84,sw85,sw86,sw87,sw88,sw89,isw80,isw81,isw82,isw83,  &
   isw84,isw85,isw86,isw87,isw88,isw89,sdarkint1,sdarkint2,  &
   sdarkint3,sdarkint4,sdarkint5)
!
!   UPDATE December 20, 2012
!
!   This routine used by geneticELC.  If ieliete > 1, then open ELC.1001
!   ELC.1002, ... ELC.100(ielete-1)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: kkk
   INTEGER, INTENT(OUT)                     :: nalph1
   INTEGER, INTENT(OUT)                     :: nbet1
   INTEGER, INTENT(OUT)                     :: nalph2
   INTEGER, INTENT(OUT)                     :: nbet2
   REAL(KIND=dp), INTENT(OUT)               :: fill1
   REAL(KIND=dp), INTENT(OUT)               :: fill2
   REAL(KIND=dp), INTENT(OUT)               :: omega1
   REAL(KIND=dp), INTENT(OUT)               :: omega2
   REAL(KIND=dp), INTENT(OUT)               :: dphase
   REAL(KIND=dp), INTENT(OUT)               :: q
   REAL(KIND=dp), INTENT(OUT)               :: finc
   REAL(KIND=dp), INTENT(OUT)               :: teff1
   REAL(KIND=dp), INTENT(OUT)               :: teff2
   REAL(KIND=dp), INTENT(OUT)               :: tgrav1
   REAL(KIND=dp), INTENT(OUT)               :: tgrav2
   REAL(KIND=dp), INTENT(OUT)               :: betarim
   REAL(KIND=dp), INTENT(OUT)               :: rinner
   REAL(KIND=dp), INTENT(OUT)               :: router
   REAL(KIND=dp), INTENT(OUT)               :: tdisk
   REAL(KIND=dp), INTENT(OUT)               :: xi
   INTEGER, INTENT(OUT)                     :: ntheta
   INTEGER, INTENT(OUT)                     :: nradius
   REAL(KIND=dp), INTENT(OUT)               :: alb1
   REAL(KIND=dp), INTENT(OUT)               :: alb2
   INTEGER, INTENT(OUT)                     :: nref
   REAL(KIND=dp), INTENT(OUT)               :: rlx
   REAL(KIND=dp), INTENT(OUT)               :: period
   REAL(KIND=dp), INTENT(OUT)               :: fm
   REAL(KIND=dp), INTENT(OUT)               :: separ
   REAL(KIND=dp), INTENT(OUT)               :: gamma
   REAL(KIND=dp), INTENT(OUT)               :: t3
   REAL(KIND=dp), INTENT(OUT)               :: g3
   REAL(KIND=dp), INTENT(OUT)               :: sa3
   REAL(KIND=dp), INTENT(OUT)               :: density
   REAL(KIND=dp), INTENT(OUT)               :: sw1
   REAL(KIND=dp), INTENT(OUT)               :: sw2
   REAL(KIND=dp), INTENT(OUT)               :: sw3
   REAL(KIND=dp), INTENT(OUT)               :: t0
   INTEGER, INTENT(OUT)                     :: idraw
   INTEGER, INTENT(OUT)                     :: iecheck
   INTEGER, INTENT(OUT)                     :: iidint
   INTEGER, INTENT(OUT)                     :: iatm
   INTEGER, INTENT(OUT)                     :: ism1
   INTEGER, INTENT(OUT)                     :: icnu
   INTEGER, INTENT(OUT)                     :: icnb
   INTEGER, INTENT(OUT)                     :: icnv
   INTEGER, INTENT(OUT)                     :: icnr
   INTEGER, INTENT(OUT)                     :: icni
   INTEGER, INTENT(OUT)                     :: icnj
   INTEGER, INTENT(OUT)                     :: icnh
   INTEGER, INTENT(OUT)                     :: icnk
   INTEGER, INTENT(OUT)                     :: irvfilt
   INTEGER, INTENT(OUT)                     :: isw1
   INTEGER, INTENT(OUT)                     :: isw2
   INTEGER, INTENT(OUT)                     :: isw3
   INTEGER, INTENT(OUT)                     :: isw4
   INTEGER, INTENT(OUT)                     :: ilaw
   REAL(KIND=dp), INTENT(OUT)               :: wave(8)
   REAL(KIND=dp), INTENT(OUT)               :: dbolx(8,2)
   REAL(KIND=dp), INTENT(OUT)               :: dboly(8,2)
   REAL(KIND=dp), INTENT(IN OUT)            :: dwavex(8,10)
   REAL(KIND=dp), INTENT(IN OUT)            :: dwavey(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: ecc
   REAL(KIND=dp), INTENT(OUT)               :: argper
   REAL(KIND=dp), INTENT(OUT)               :: pshift
   REAL(KIND=dp), INTENT(OUT)               :: sw5
   REAL(KIND=dp), INTENT(OUT)               :: sw6
   REAL(KIND=dp), INTENT(OUT)               :: sw7
   REAL(KIND=dp), INTENT(OUT)               :: sw8
   REAL(KIND=dp), INTENT(OUT)               :: sw9
   INTEGER, INTENT(OUT)                     :: ikeep
   INTEGER, INTENT(OUT)                     :: isynch
   INTEGER, INTENT(OUT)                     :: isw5
   INTEGER, INTENT(OUT)                     :: isw6
   INTEGER, INTENT(OUT)                     :: isw7
   INTEGER, INTENT(OUT)                     :: isw8
   INTEGER, INTENT(OUT)                     :: isw9
   REAL(KIND=dp), INTENT(OUT)               :: spot1parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spot2parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spotdparm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: primmass
   REAL(KIND=dp), INTENT(OUT)               :: primk
   REAL(KIND=dp), INTENT(OUT)               :: primrad
   REAL(KIND=dp), INTENT(OUT)               :: ratrad
   REAL(KIND=dp), INTENT(OUT)               :: frac1
   REAL(KIND=dp), INTENT(OUT)               :: frac2
   REAL(KIND=dp), INTENT(OUT)               :: ecosw
   REAL(KIND=dp), INTENT(OUT)               :: temprat
   INTEGER, INTENT(OUT)                     :: idark1
   INTEGER, INTENT(OUT)                     :: idark2
   INTEGER, INTENT(OUT)                     :: isw12
   INTEGER, INTENT(OUT)                     :: isw13
   INTEGER, INTENT(OUT)                     :: isw21
   INTEGER, INTENT(OUT)                     :: isw22
   INTEGER, INTENT(OUT)                     :: isw23
   INTEGER, INTENT(OUT)                     :: isw24
   REAL(KIND=dp), INTENT(OUT)               :: bigi
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta
   REAL(KIND=dp), INTENT(OUT)               :: sw23
   REAL(KIND=dp), INTENT(OUT)               :: sw24
   REAL(KIND=dp), INTENT(OUT)               :: powercoeff(8,9)
   REAL(KIND=dp), INTENT(OUT)               :: sw25
   REAL(KIND=dp), INTENT(OUT)               :: sw26
   REAL(KIND=dp), INTENT(OUT)               :: sw27
   REAL(KIND=dp), INTENT(OUT)               :: sw28
   REAL(KIND=dp), INTENT(OUT)               :: sw29
   REAL(KIND=dp), INTENT(OUT)               :: sw30
   REAL(KIND=dp), INTENT(OUT)               :: contam
   REAL(KIND=dp), INTENT(OUT)               :: tconj
   REAL(KIND=dp), INTENT(OUT)               :: beam1
   REAL(KIND=dp), INTENT(OUT)               :: beam2
   INTEGER, INTENT(OUT)                     :: isw25
   INTEGER, INTENT(OUT)                     :: isw26
   INTEGER, INTENT(OUT)                     :: isw27
   INTEGER, INTENT(OUT)                     :: isw28
   INTEGER, INTENT(OUT)                     :: isw29
   INTEGER, INTENT(OUT)                     :: isw30
   INTEGER, INTENT(OUT)                     :: isw31
   INTEGER, INTENT(OUT)                     :: isw32
   INTEGER, INTENT(OUT)                     :: isw33
   INTEGER, INTENT(OUT)                     :: isw34
   REAL(KIND=dp), INTENT(OUT)               :: ocose
   REAL(KIND=dp), INTENT(OUT)               :: osine
   REAL(KIND=dp), INTENT(OUT)               :: omegadot
   REAL(KIND=dp), INTENT(OUT)               :: contams0
   REAL(KIND=dp), INTENT(OUT)               :: contams1
   REAL(KIND=dp), INTENT(OUT)               :: contams2
   REAL(KIND=dp), INTENT(OUT)               :: contams3
   REAL(KIND=dp), INTENT(OUT)               :: sw47
   REAL(KIND=dp), INTENT(OUT)               :: sw48
   REAL(KIND=dp), INTENT(OUT)               :: sw49
   REAL(KIND=dp), INTENT(OUT)               :: sw80
   REAL(KIND=dp), INTENT(OUT)               :: sw81
   REAL(KIND=dp), INTENT(OUT)               :: sw82
   REAL(KIND=dp), INTENT(OUT)               :: sw83
   REAL(KIND=dp), INTENT(OUT)               :: sw84
   REAL(KIND=dp), INTENT(OUT)               :: sw85
   REAL(KIND=dp), INTENT(OUT)               :: sw86
   REAL(KIND=dp), INTENT(OUT)               :: sw87
   REAL(KIND=dp), INTENT(OUT)               :: sw88
   REAL(KIND=dp), INTENT(OUT)               :: sw89
   INTEGER, INTENT(OUT)                     :: isw80
   INTEGER, INTENT(OUT)                     :: isw81
   INTEGER, INTENT(OUT)                     :: isw82
   INTEGER, INTENT(OUT)                     :: isw83
   INTEGER, INTENT(OUT)                     :: isw84
   INTEGER, INTENT(OUT)                     :: isw85
   INTEGER, INTENT(OUT)                     :: isw86
   INTEGER, INTENT(OUT)                     :: isw87
   INTEGER, INTENT(OUT)                     :: isw88
   INTEGER, INTENT(OUT)                     :: isw89
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint1(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint2(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint3(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint4(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint5(8)
!
   INTEGER :: ios,i,k,jj
!
   CHARACTER (LEN=4) :: extension
   CHARACTER (LEN=1) :: bell
!
   IF(kkk > 9999)THEN
      bell=CHAR(7)
      WRITE(*,80)bell
      STOP
   END IF
!
   IF(kkk < 10)WRITE(extension,40)kkk
   IF((kkk >= 10).AND.(kkk < 100))WRITE(extension,50)kkk
   IF((kkk >= 100).AND.(kkk < 1000))WRITE(extension,60)kkk
   IF(kkk >= 1000)WRITE(extension,70)kkk
!
   IF(kkk < 8999)WRITE(extension,70)kkk+1000
!
   ios=0
   OPEN(UNIT=1,FILE='ELC.'//extension,STATUS='old',ERR=20,IOSTAT= ios)
!
   READ(1,*,END=30,ERR=30)nalph1
   READ(1,*,END=30,ERR=30)nbet1
   READ(1,*,END=30,ERR=30)nalph2
   READ(1,*,END=30,ERR=30)nbet2
   READ(1,*,END=30,ERR=30)fill1
   READ(1,*,END=30,ERR=30)fill2
   READ(1,*,END=30,ERR=30)omega1
   READ(1,*,END=30,ERR=30)omega2
   READ(1,*,END=30,ERR=30)dphase
   READ(1,*,END=30,ERR=30)q
   READ(1,*,END=30,ERR=30)finc
   READ(1,*,END=30,ERR=30)teff1
   READ(1,*,END=30,ERR=30)teff2
   READ(1,*,END=30,ERR=30)tgrav1
   READ(1,*,END=30,ERR=30)tgrav2
   READ(1,*,END=30,ERR=30)betarim
   READ(1,*,END=30,ERR=30)rinner
   READ(1,*,END=30,ERR=30)router
   READ(1,*,END=30,ERR=30)tdisk
   READ(1,*,END=30,ERR=30)xi
   READ(1,*,END=30,ERR=30)ntheta
   READ(1,*,END=30,ERR=30)nradius
   READ(1,*,END=30,ERR=30)alb1
   READ(1,*,END=30,ERR=30)alb2
   READ(1,*,END=30,ERR=30)nref
   READ(1,*,END=30,ERR=30)rlx
   READ(1,*,END=30,ERR=30)period
   READ(1,*,END=30,ERR=30)fm
   READ(1,*,END=30,ERR=30)separ
   READ(1,*,END=30,ERR=30)gamma
   READ(1,*,END=30,ERR=30)t3
   READ(1,*,END=30,ERR=30)g3
   READ(1,*,END=30,ERR=30)sa3
   READ(1,*,END=30,ERR=30)density
   READ(1,*,END=30,ERR=30)sw1
   READ(1,*,END=30,ERR=30)sw2
   READ(1,*,END=30,ERR=30)sw3
   READ(1,*,END=30,ERR=30)t0
   READ(1,*,END=30,ERR=30)idraw
   READ(1,*,END=30,ERR=30)iecheck
   READ(1,*,END=30,ERR=30)iidint
   READ(1,*,END=30,ERR=30)iatm
   READ(1,*,END=30,ERR=30)ism1
   READ(1,*,END=30,ERR=30)icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk
   READ(1,*,END=30,ERR=30)irvfilt
   READ(1,*,END=30,ERR=30)isw1
   READ(1,*,END=30,ERR=30)isw2
   READ(1,*,END=30,ERR=30)isw3
   READ(1,*,END=30,ERR=30)isw4
   READ(1,*,END=30,ERR=30)ilaw
   DO  i=1,8
      READ(1,*,END=30,ERR=30)wave(i),dbolx(i,1),dboly(i,1),  &
         dbolx(i,2),dboly(i,2),dwavex(i,1),dwavey(i,1),dwavex(i,2),dwavey(i,2)
   END DO
   READ(1,*,END=30,ERR=30)ecc
   READ(1,*,END=30,ERR=30)argper
   READ(1,*,END=30,ERR=30)pshift
   READ(1,*,END=30,ERR=30)sw5
   READ(1,*,END=30,ERR=30)sw6
   READ(1,*,END=30,ERR=30)sw7
   READ(1,*,END=30,ERR=30)sw8
   READ(1,*,END=30,ERR=30)sw9
   READ(1,*,END=30,ERR=30)ikeep
   READ(1,*,END=30,ERR=30)isynch
   READ(1,*,END=30,ERR=30)isw5
   READ(1,*,END=30,ERR=30)isw6
   READ(1,*,END=30,ERR=30)isw7
   READ(1,*,END=30,ERR=30)isw8
   READ(1,*,END=30,ERR=30)isw9
   ios=0
   READ(1,*,END=30,ERR=30)spot1parm(1,1)
   READ(1,*,END=30,ERR=30)spot1parm(1,2)
   READ(1,*,END=30,ERR=30)spot1parm(1,3)
   READ(1,*,END=30,ERR=30)spot1parm(1,4)
   READ(1,*,END=30,ERR=30)spot1parm(2,1)
   READ(1,*,END=30,ERR=30)spot1parm(2,2)
   READ(1,*,END=30,ERR=30)spot1parm(2,3)
   READ(1,*,END=30,ERR=30)spot1parm(2,4)
   READ(1,*,END=30,ERR=30)spot2parm(1,1)
   READ(1,*,END=30,ERR=30)spot2parm(1,2)
   READ(1,*,END=30,ERR=30)spot2parm(1,3)
   READ(1,*,END=30,ERR=30)spot2parm(1,4)
   READ(1,*,END=30,ERR=30)spot2parm(2,1)
   READ(1,*,END=30,ERR=30)spot2parm(2,2)
   READ(1,*,END=30,ERR=30)spot2parm(2,3)
   READ(1,*,END=30,ERR=30)spot2parm(2,4)
   READ(1,*,END=30,ERR=30)spotdparm(1,1)
   READ(1,*,END=30,ERR=30)spotdparm(1,2)
   READ(1,*,END=30,ERR=30)spotdparm(1,3)
   READ(1,*,END=30,ERR=30)spotdparm(1,4)
   READ(1,*,END=30,ERR=30)spotdparm(2,1)
   READ(1,*,END=30,ERR=30)spotdparm(2,2)
   READ(1,*,END=30,ERR=30)spotdparm(2,3)
   READ(1,*,END=30,ERR=30)spotdparm(2,4)
   READ(1,*,END=30,ERR=30)primmass
   READ(1,*,END=30,ERR=30)primk
   READ(1,*,END=30,ERR=30)primrad
   READ(1,*,END=30,ERR=30)ratrad
   READ(1,*,END=30,ERR=30)frac1
   READ(1,*,END=30,ERR=30)frac2
   READ(1,*,END=30,ERR=30)ecosw
   READ(1,*,END=30,ERR=30)temprat
   READ(1,*,END=30,ERR=30)idark1
   READ(1,*,END=30,ERR=30)idark2
   READ(1,*,END=30,ERR=30)isw12
   READ(1,*,END=30,ERR=30)isw13
   READ(1,*,END=30,ERR=30)isw21
   READ(1,*,END=30,ERR=30)isw22
   READ(1,*,END=30,ERR=30)isw23
   READ(1,*,END=30,ERR=30)isw24
   READ(1,*,END=30,ERR=30)(powercoeff(1,k),k=1,9)
   READ(1,*,END=30,ERR=30)(powercoeff(2,k),k=1,9)
   READ(1,*,END=30,ERR=30)(powercoeff(3,k),k=1,9)
   READ(1,*,END=30,ERR=30)(powercoeff(4,k),k=1,9)
   READ(1,*,END=30,ERR=30)(powercoeff(5,k),k=1,9)
   READ(1,*,END=30,ERR=30)(powercoeff(6,k),k=1,9)
   READ(1,*,END=30,ERR=30)(powercoeff(7,k),k=1,9)
   READ(1,*,END=30,ERR=30)(powercoeff(8,k),k=1,9)
   READ(1,*,END=30,ERR=30)bigi
   READ(1,*,END=30,ERR=30)bigbeta
   READ(1,*,END=30,ERR=30)sw23
   READ(1,*,END=30,ERR=30)sw24
   READ(1,*,END=30,ERR=30)sw25
   READ(1,*,END=30,ERR=30)sw26
   READ(1,*,END=30,ERR=30)sw27
   READ(1,*,END=30,ERR=30)sw28
   READ(1,*,END=30,ERR=30)sw29
   READ(1,*,END=30,ERR=30)sw30
   READ(1,*,END=30,ERR=30)contam
   READ(1,*,END=30,ERR=30)tconj
   READ(1,*,END=30,ERR=30)beam1
   READ(1,*,END=30,ERR=30)beam2
   READ(1,*,END=30,ERR=30)isw25
   READ(1,*,END=30,ERR=30)isw26
   READ(1,*,END=30,ERR=30)isw27
   READ(1,*,END=30,ERR=30)isw28
   READ(1,*,END=30,ERR=30)isw29
   READ(1,*,END=30,ERR=30)isw30
   READ(1,*,END=30,ERR=30)isw31
   READ(1,*,END=30,ERR=30)isw32
   READ(1,*,END=30,ERR=30)isw33
   READ(1,*,END=30,ERR=30)isw34
   READ(1,*,END=30,ERR=30)ocose
   READ(1,*,END=30,ERR=30)osine
   READ(1,*,END=30,ERR=30)omegadot
   READ(1,*,END=30,ERR=30)contams0
   READ(1,*,END=30,ERR=30)contams1
   READ(1,*,END=30,ERR=30)contams2
   READ(1,*,END=30,ERR=30)contams3
   READ(1,*,END=30,ERR=30)sw47
   READ(1,*,END=30,ERR=30)sw48
   READ(1,*,END=30,ERR=30)sw49
   READ(1,*,END=30,ERR=30)sw80
   READ(1,*,END=30,ERR=30)sw81
   READ(1,*,END=30,ERR=30)sw82
   READ(1,*,END=30,ERR=30)sw83
   READ(1,*,END=30,ERR=30)sw84
   READ(1,*,END=30,ERR=30)sw85
   READ(1,*,END=30,ERR=30)sw86
   READ(1,*,END=30,ERR=30)sw87
   READ(1,*,END=30,ERR=30)sw88
   READ(1,*,END=30,ERR=30)sw89
   READ(1,*,END=30,ERR=30)isw80
   READ(1,*,END=30,ERR=30)isw81
   READ(1,*,END=30,ERR=30)isw82
   READ(1,*,END=30,ERR=30)isw83
   READ(1,*,END=30,ERR=30)isw84
   READ(1,*,END=30,ERR=30)isw85
   READ(1,*,END=30,ERR=30)isw86
   READ(1,*,END=30,ERR=30)isw87
   READ(1,*,END=30,ERR=30)isw88
   READ(1,*,END=30,ERR=30)isw89
   READ(1,*,END=30,ERR=30)(sdarkint1(jj),jj=1,8)
   READ(1,*,END=30,ERR=30)(sdarkint2(jj),jj=1,8)
   READ(1,*,END=30,ERR=30)(sdarkint3(jj),jj=1,8)
   READ(1,*,END=30,ERR=30)(sdarkint4(jj),jj=1,8)
   READ(1,*,END=30,ERR=30)(sdarkint5(jj),jj=1,8)
!
   IF(ios == 0)THEN
      CLOSE(1)
      RETURN
   END IF
!
!   Come here if the input file ELC.???? does not exist.
!
20 IF(ios > 0)THEN
      bell=CHAR(7)
      WRITE(*,90) bell,kkk
      STOP
   END IF
30 bell=CHAR(7)
!
!   file ended too soon
!
   WRITE(*,100) bell,kkk
   CLOSE(1)
!
   RETURN
!
40 FORMAT('100',i1)
50 FORMAT('10',i2)
60 FORMAT('1',i3)
70 FORMAT(i4)
80 FORMAT(a1,'ielete is too large')
90 FORMAT(a1,'Error:  File ELC.???? does not exist for index ', i4)
100 FORMAT(a1,'Error:  Bad entry in ELC.???? for index ',i4)
!
END SUBROUTINE getgridinput
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getinput(nalph1,nbet1,nalph2,nbet2,fill1,fill2,  &
   omega1,omega2,dphase,q,finc,teff1,teff2,tgrav1,tgrav2,  &
   betarim,rinner,router,tdisk,xi,ntheta,nradius,alb1,alb2,nref,  &
   rlx,period,fm,separ,gamma,t3,g3,sa3,density,sw1,sw2,sw3,t0,  &
   idraw,iecheck,iidint,iatm,ism1,icnu,icnb,icnv,icnr,icni,icnj,  &
   icnh,icnk,irvfilt,isw1,isw2,isw3,isw4,ilaw,wave,dbolx,dboly,  &
   dwavex,dwavey,ecc,argper,pshift,sw5,sw6,sw7,sw8,sw9,ikeep,  &
   isynch,isw5,isw6,isw7,isw8,isw9,spot1parm,spot2parm,  &
   spotdparm,primmass,primk,primrad,ratrad,frac1,frac2,ecosw,  &
   temprat,idark1,idark2,isw12,isw13,isw21,isw22,isw23,isw24,  &
   bigi,bigbeta,sw23,sw24,powercoeff,sw25,sw26,sw27,sw28,sw29,  &
   sw30,contam,tconj,beam1,beam2,isw25,isw26,isw27,isw28,isw29,  &
   isw30,isw31,isw32,isw33,isw34,ocose,osine,omegadot,contams0,  &
   contams1,contams2,contams3,sw47,sw48,sw49,sw80,sw81,sw82,  &
   sw83,sw84,sw85,sw86,sw87,sw88,sw89,isw80,isw81,isw82,isw83,  &
   isw84,isw85,isw86,isw87,isw88,isw89,sdarkint1,sdarkint2,  &
   sdarkint3,sdarkint4,sdarkint5)
!
   USE accur
!
   IMPLICIT NONE

   INTEGER, INTENT(OUT)                     :: nalph1
   INTEGER, INTENT(OUT)                     :: nbet1
   INTEGER, INTENT(OUT)                     :: nalph2
   INTEGER, INTENT(OUT)                     :: nbet2
   REAL(KIND=dp), INTENT(OUT)               :: fill1
   REAL(KIND=dp), INTENT(OUT)               :: fill2
   REAL(KIND=dp), INTENT(OUT)               :: omega1
   REAL(KIND=dp), INTENT(OUT)               :: omega2
   REAL(KIND=dp), INTENT(OUT)               :: dphase
   REAL(KIND=dp), INTENT(OUT)               :: q
   REAL(KIND=dp), INTENT(OUT)               :: finc
   REAL(KIND=dp), INTENT(OUT)               :: teff1
   REAL(KIND=dp), INTENT(OUT)               :: teff2
   REAL(KIND=dp), INTENT(OUT)               :: tgrav1
   REAL(KIND=dp), INTENT(OUT)               :: tgrav2
   REAL(KIND=dp), INTENT(OUT)               :: betarim
   REAL(KIND=dp), INTENT(OUT)               :: rinner
   REAL(KIND=dp), INTENT(OUT)               :: router
   REAL(KIND=dp), INTENT(OUT)               :: tdisk
   REAL(KIND=dp), INTENT(OUT)               :: xi
   INTEGER, INTENT(OUT)                     :: ntheta
   INTEGER, INTENT(OUT)                     :: nradius
   REAL(KIND=dp), INTENT(OUT)               :: alb1
   REAL(KIND=dp), INTENT(OUT)               :: alb2
   INTEGER, INTENT(OUT)                     :: nref
   REAL(KIND=dp), INTENT(OUT)               :: rlx
   REAL(KIND=dp), INTENT(OUT)               :: period
   REAL(KIND=dp), INTENT(OUT)               :: fm
   REAL(KIND=dp), INTENT(OUT)               :: separ
   REAL(KIND=dp), INTENT(OUT)               :: gamma
   REAL(KIND=dp), INTENT(OUT)               :: t3
   REAL(KIND=dp), INTENT(OUT)               :: g3
   REAL(KIND=dp), INTENT(OUT)               :: sa3
   REAL(KIND=dp), INTENT(OUT)               :: density
   REAL(KIND=dp), INTENT(OUT)               :: sw1
   REAL(KIND=dp), INTENT(OUT)               :: sw2
   REAL(KIND=dp), INTENT(OUT)               :: sw3
   REAL(KIND=dp), INTENT(OUT)               :: t0
   INTEGER, INTENT(OUT)                     :: idraw
   INTEGER, INTENT(OUT)                     :: iecheck
   INTEGER, INTENT(OUT)                     :: iidint
   INTEGER, INTENT(OUT)                     :: iatm
   INTEGER, INTENT(OUT)                     :: ism1
   INTEGER, INTENT(OUT)                     :: icnu
   INTEGER, INTENT(OUT)                     :: icnb
   INTEGER, INTENT(OUT)                     :: icnv
   INTEGER, INTENT(OUT)                     :: icnr
   INTEGER, INTENT(OUT)                     :: icni
   INTEGER, INTENT(OUT)                     :: icnj
   INTEGER, INTENT(OUT)                     :: icnh
   INTEGER, INTENT(OUT)                     :: icnk
   INTEGER, INTENT(OUT)                     :: irvfilt
   INTEGER, INTENT(OUT)                     :: isw1
   INTEGER, INTENT(OUT)                     :: isw2
   INTEGER, INTENT(OUT)                     :: isw3
   INTEGER, INTENT(OUT)                     :: isw4
   INTEGER, INTENT(OUT)                     :: ilaw
   REAL(KIND=dp), INTENT(OUT)               :: wave(8)
   REAL(KIND=dp), INTENT(OUT)               :: dbolx(8,2)
   REAL(KIND=dp), INTENT(OUT)               :: dboly(8,2)
   REAL(KIND=dp), INTENT(OUT)               :: dwavex(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: dwavey(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: ecc
   REAL(KIND=dp), INTENT(OUT)               :: argper
   REAL(KIND=dp), INTENT(OUT)               :: pshift
   REAL(KIND=dp), INTENT(OUT)               :: sw5
   REAL(KIND=dp), INTENT(OUT)               :: sw6
   REAL(KIND=dp), INTENT(OUT)               :: sw7
   REAL(KIND=dp), INTENT(OUT)               :: sw8
   REAL(KIND=dp), INTENT(OUT)               :: sw9
   INTEGER, INTENT(OUT)                     :: ikeep
   INTEGER, INTENT(OUT)                     :: isynch
   INTEGER, INTENT(OUT)                     :: isw5
   INTEGER, INTENT(OUT)                     :: isw6
   INTEGER, INTENT(OUT)                     :: isw7
   INTEGER, INTENT(OUT)                     :: isw8
   INTEGER, INTENT(OUT)                     :: isw9
   REAL(KIND=dp), INTENT(OUT)               :: spot1parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spot2parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spotdparm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: primmass
   REAL(KIND=dp), INTENT(OUT)               :: primk
   REAL(KIND=dp), INTENT(OUT)               :: primrad
   REAL(KIND=dp), INTENT(OUT)               :: ratrad
   REAL(KIND=dp), INTENT(OUT)               :: frac1
   REAL(KIND=dp), INTENT(OUT)               :: frac2
   REAL(KIND=dp), INTENT(OUT)               :: ecosw
   REAL(KIND=dp), INTENT(OUT)               :: temprat
   INTEGER, INTENT(OUT)                     :: idark1
   INTEGER, INTENT(OUT)                     :: idark2
   INTEGER, INTENT(OUT)                     :: isw12
   INTEGER, INTENT(OUT)                     :: isw13
   INTEGER, INTENT(OUT)                     :: isw21
   INTEGER, INTENT(OUT)                     :: isw22
   INTEGER, INTENT(OUT)                     :: isw23
   INTEGER, INTENT(OUT)                     :: isw24
   REAL(KIND=dp), INTENT(OUT)               :: bigi
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta
   REAL(KIND=dp), INTENT(OUT)               :: sw23
   REAL(KIND=dp), INTENT(OUT)               :: sw24
   REAL(KIND=dp), INTENT(OUT)               :: powercoeff(8,9)
   REAL(KIND=dp), INTENT(OUT)               :: sw25
   REAL(KIND=dp), INTENT(OUT)               :: sw26
   REAL(KIND=dp), INTENT(OUT)               :: sw27
   REAL(KIND=dp), INTENT(OUT)               :: sw28
   REAL(KIND=dp), INTENT(OUT)               :: sw29
   REAL(KIND=dp), INTENT(OUT)               :: sw30
   REAL(KIND=dp), INTENT(OUT)               :: contam
   REAL(KIND=dp), INTENT(OUT)               :: tconj
   REAL(KIND=dp), INTENT(OUT)               :: beam1
   REAL(KIND=dp), INTENT(OUT)               :: beam2
   INTEGER, INTENT(OUT)                     :: isw25
   INTEGER, INTENT(OUT)                     :: isw26
   INTEGER, INTENT(OUT)                     :: isw27
   INTEGER, INTENT(OUT)                     :: isw28
   INTEGER, INTENT(OUT)                     :: isw29
   INTEGER, INTENT(OUT)                     :: isw30
   INTEGER, INTENT(OUT)                     :: isw31
   INTEGER, INTENT(OUT)                     :: isw32
   INTEGER, INTENT(OUT)                     :: isw33
   INTEGER, INTENT(OUT)                     :: isw34
   REAL(KIND=dp), INTENT(OUT)               :: ocose
   REAL(KIND=dp), INTENT(OUT)               :: osine
   REAL(KIND=dp), INTENT(OUT)               :: omegadot
   REAL(KIND=dp), INTENT(OUT)               :: contams0
   REAL(KIND=dp), INTENT(OUT)               :: contams1
   REAL(KIND=dp), INTENT(OUT)               :: contams2
   REAL(KIND=dp), INTENT(OUT)               :: contams3
   REAL(KIND=dp), INTENT(OUT)               :: sw47
   REAL(KIND=dp), INTENT(OUT)               :: sw48
   REAL(KIND=dp), INTENT(OUT)               :: sw49
   REAL(KIND=dp), INTENT(OUT)               :: sw80
   REAL(KIND=dp), INTENT(OUT)               :: sw81
   REAL(KIND=dp), INTENT(OUT)               :: sw82
   REAL(KIND=dp), INTENT(OUT)               :: sw83
   REAL(KIND=dp), INTENT(OUT)               :: sw84
   REAL(KIND=dp), INTENT(OUT)               :: sw85
   REAL(KIND=dp), INTENT(OUT)               :: sw86
   REAL(KIND=dp), INTENT(OUT)               :: sw87
   REAL(KIND=dp), INTENT(OUT)               :: sw88
   REAL(KIND=dp), INTENT(OUT)               :: sw89
   INTEGER, INTENT(OUT)                     :: isw80
   INTEGER, INTENT(OUT)                     :: isw81
   INTEGER, INTENT(OUT)                     :: isw82
   INTEGER, INTENT(OUT)                     :: isw83
   INTEGER, INTENT(OUT)                     :: isw84
   INTEGER, INTENT(OUT)                     :: isw85
   INTEGER, INTENT(OUT)                     :: isw86
   INTEGER, INTENT(OUT)                     :: isw87
   INTEGER, INTENT(OUT)                     :: isw88
   INTEGER, INTENT(OUT)                     :: isw89
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint1(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint2(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint3(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint4(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint5(8)
!
   INTEGER :: ios,i,k,jj
!
!   UPDATE June 22, 2002
!
!   Declare the variable bell to be character*1
!
   CHARACTER (LEN=1) :: bell
!
!   UPDATE August 12, 2016
!
!   initalize some new integer flags
!
   isw80=0
   isw81=0
   isw82=0
   isw83=0
   isw84=0
   isw85=0
   isw86=0
   isw87=0
   isw88=0
   isw89=0
!
   ios=0
   OPEN(UNIT=1,FILE='ELC.inp',STATUS='old',ERR=30,IOSTAT=ios)
!
   READ(1,*)nalph1
   READ(1,*)nbet1
   READ(1,*)nalph2
   READ(1,*)nbet2
   READ(1,*)fill1
   READ(1,*)fill2
   READ(1,*)omega1
   READ(1,*)omega2
   READ(1,*)dphase
   READ(1,*)q
   READ(1,*)finc
   READ(1,*)teff1
   READ(1,*)teff2
   READ(1,*)tgrav1
   READ(1,*)tgrav2
   READ(1,*)betarim
   READ(1,*)rinner
   READ(1,*)router
   READ(1,*)tdisk
   READ(1,*)xi
   READ(1,*)ntheta
   READ(1,*)nradius
   READ(1,*)alb1
   READ(1,*)alb2
   READ(1,*)nref
   READ(1,*)rlx
   READ(1,*)period
   READ(1,*)fm
   READ(1,*)separ
   READ(1,*)gamma
   READ(1,*)t3
   READ(1,*)g3
   READ(1,*)sa3
   READ(1,*)density
   READ(1,*)sw1
   READ(1,*)sw2
   READ(1,*)sw3
   READ(1,*)t0
   READ(1,*)idraw
   READ(1,*)iecheck
   READ(1,*)iidint
   READ(1,*)iatm
   READ(1,*)ism1
   READ(1,*)icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk
   READ(1,*)irvfilt
   READ(1,*)isw1
   READ(1,*)isw2
   READ(1,*)isw3
   READ(1,*)isw4
   READ(1,*)ilaw
!
!  Load the limb darkening parameters.
!
   DO  i=1,8
      READ(1,*)wave(i),dbolx(i,1),dboly(i,1),dbolx(i,2),dboly(i,  &
         2),dwavex(i,1),dwavey(i,1),dwavex(i,2),dwavey(i,2)
      dwavex(i,3)=dwavex(i,1)
      dwavey(i,3)=dwavey(i,1)
      dwavex(i,4)=dwavex(i,1)
      dwavey(i,4)=dwavey(i,1)
      dwavex(i,5)=dwavex(i,1)
      dwavey(i,5)=dwavey(i,1)
   END DO
   READ(1,*,END=20)ecc
   READ(1,*)argper
   READ(1,*)pshift
   READ(1,*)sw5
   READ(1,*)sw6
   READ(1,*)sw7
   READ(1,*)sw8
   READ(1,*)sw9
   READ(1,*)ikeep
   READ(1,*)isynch
   READ(1,*)isw5
   READ(1,*)isw6
   READ(1,*)isw7
   READ(1,*)isw8
   READ(1,*)isw9
   ios=0
   READ(1,*,END=40,ERR=40)spot1parm(1,1)
   READ(1,*,END=40,ERR=40)spot1parm(1,2)
   READ(1,*,END=40,ERR=40)spot1parm(1,3)
   READ(1,*,END=40,ERR=40)spot1parm(1,4)
   READ(1,*,END=40,ERR=40)spot1parm(2,1)
   READ(1,*,END=40,ERR=40)spot1parm(2,2)
   READ(1,*,END=40,ERR=40)spot1parm(2,3)
   READ(1,*,END=40,ERR=40)spot1parm(2,4)
   READ(1,*,END=40,ERR=40)spot2parm(1,1)
   READ(1,*,END=40,ERR=40)spot2parm(1,2)
   READ(1,*,END=40,ERR=40)spot2parm(1,3)
   READ(1,*,END=40,ERR=40)spot2parm(1,4)
   READ(1,*,END=40,ERR=40)spot2parm(2,1)
   READ(1,*,END=40,ERR=40)spot2parm(2,2)
   READ(1,*,END=40,ERR=40)spot2parm(2,3)
   READ(1,*,END=40,ERR=40)spot2parm(2,4)
   READ(1,*,END=40,ERR=40)spotdparm(1,1)
   READ(1,*,END=40,ERR=40)spotdparm(1,2)
   READ(1,*,END=40,ERR=40)spotdparm(1,3)
   READ(1,*,END=40,ERR=40)spotdparm(1,4)
   READ(1,*,END=40,ERR=40)spotdparm(2,1)
   READ(1,*,END=40,ERR=40)spotdparm(2,2)
   READ(1,*,END=40,ERR=40)spotdparm(2,3)
   READ(1,*,END=40,ERR=40)spotdparm(2,4)
   READ(1,*,END=40,ERR=40)primmass
   READ(1,*,END=40,ERR=40)primk
   READ(1,*,END=40,ERR=40)primrad
   READ(1,*,END=40,ERR=40)ratrad
   READ(1,*,END=40,ERR=40)frac1
   READ(1,*,END=40,ERR=40)frac2
   READ(1,*,END=40,ERR=40)ecosw
   READ(1,*,END=40,ERR=40)temprat
   READ(1,*,END=40,ERR=40)idark1
   READ(1,*,END=40,ERR=40)idark2
   READ(1,*,END=40,ERR=40)isw12
   READ(1,*,END=40,ERR=40)isw13
   READ(1,*,END=40,ERR=40)isw21
   READ(1,*,END=40,ERR=40)isw22
   READ(1,*,END=40,ERR=40)isw23
   READ(1,*,END=40,ERR=40)isw24
   READ(1,*,END=40,ERR=40)(powercoeff(1,k),k=1,9)
   READ(1,*,END=40,ERR=40)(powercoeff(2,k),k=1,9)
   READ(1,*,END=40,ERR=40)(powercoeff(3,k),k=1,9)
   READ(1,*,END=40,ERR=40)(powercoeff(4,k),k=1,9)
   READ(1,*,END=40,ERR=40)(powercoeff(5,k),k=1,9)
   READ(1,*,END=40,ERR=40)(powercoeff(6,k),k=1,9)
   READ(1,*,END=40,ERR=40)(powercoeff(7,k),k=1,9)
   READ(1,*,END=40,ERR=40)(powercoeff(8,k),k=1,9)
   READ(1,*,END=40,ERR=40)bigi
   READ(1,*,END=40,ERR=40)bigbeta
   READ(1,*,END=40,ERR=40)sw23
   READ(1,*,END=40,ERR=40)sw24
   READ(1,*,END=40,ERR=40)sw25
   READ(1,*,END=40,ERR=40)sw26
   READ(1,*,END=40,ERR=40)sw27
   READ(1,*,END=40,ERR=40)sw28
   READ(1,*,END=40,ERR=40)sw29
   READ(1,*,END=40,ERR=40)sw30
   READ(1,*,END=40,ERR=40)contam
   READ(1,*,END=40,ERR=40)tconj
   READ(1,*,END=40,ERR=40)beam1
   READ(1,*,END=40,ERR=40)beam2
   READ(1,*,END=40,ERR=40)isw25
   READ(1,*,END=40,ERR=40)isw26
   READ(1,*,END=40,ERR=40)isw27
   READ(1,*,END=40,ERR=40)isw28
   READ(1,*,END=40,ERR=40)isw29
   READ(1,*,END=40,ERR=40)isw30
   READ(1,*,END=40,ERR=40)isw31
   READ(1,*,END=40,ERR=40)isw32
   READ(1,*,END=40,ERR=40)isw33
   READ(1,*,END=40,ERR=40)isw34
   READ(1,*,END=40,ERR=40)ocose
   READ(1,*,END=40,ERR=40)osine
   READ(1,*,END=40,ERR=40)omegadot
   READ(1,*,END=40,ERR=40)contams0
   READ(1,*,END=40,ERR=40)contams1
   READ(1,*,END=40,ERR=40)contams2
   READ(1,*,END=40,ERR=40)contams3
   READ(1,*,END=40,ERR=40)sw47
   READ(1,*,END=40,ERR=40)sw48
   READ(1,*,END=40,ERR=40)sw49
   READ(1,*,END=40,ERR=40)sw80
   READ(1,*,END=40,ERR=40)sw81
   READ(1,*,END=40,ERR=40)sw82
   READ(1,*,END=40,ERR=40)sw83
   READ(1,*,END=40,ERR=40)sw84
   READ(1,*,END=40,ERR=40)sw85
   READ(1,*,END=40,ERR=40)sw86
   READ(1,*,END=40,ERR=40)sw87
   READ(1,*,END=40,ERR=40)sw88
   READ(1,*,END=40,ERR=40)sw89
   READ(1,*,END=40,ERR=40)isw80
   READ(1,*,END=40,ERR=40)isw81
   READ(1,*,END=40,ERR=40)isw82
   READ(1,*,END=40,ERR=40)isw83
   READ(1,*,END=40,ERR=40)isw84
   READ(1,*,END=40,ERR=40)isw85
   READ(1,*,END=40,ERR=40)isw86
   READ(1,*,END=40,ERR=40)isw87
   READ(1,*,END=40,ERR=40)isw88
   READ(1,*,END=40,ERR=40)isw89
   READ(1,*,END=40,ERR=40)(sdarkint1(jj),jj=1,8)
   READ(1,*,END=40,ERR=40)(sdarkint2(jj),jj=1,8)
   READ(1,*,END=40,ERR=40)(sdarkint3(jj),jj=1,8)
   READ(1,*,END=40,ERR=40)(sdarkint4(jj),jj=1,8)
   READ(1,*,END=40,ERR=40)(sdarkint5(jj),jj=1,8)
!
20 CLOSE(1)
!
!   Come here if the input file ELC.inp does not exist.  The subroutine
!   writeinput will make the correct file and set default values.
!
30 IF(ios > 0)CALL writeinput(nalph1,nbet1,nalph2,nbet2,fill1,  &
      fill2,omega1,omega2,dphase,q,finc,teff1,teff2,tgrav1,tgrav2,  &
      betarim,rinner,router,tdisk,xi,ntheta,nradius,alb1,alb2,nref,  &
      rlx,period,fm,separ,gamma,t3,g3,sa3,density,sw1,sw2,sw3,t0,  &
      idraw,iecheck,iidint,iatm,ism1,icnu,icnb,icnv,icnr,icni,icnj,  &
      icnh,icnk,irvfilt,isw1,isw2,isw3,isw4,ilaw,wave,dbolx,dboly,  &
      dwavex,dwavey,ecc,argper,pshift,sw5,sw6,sw7,sw8,sw9,ikeep,  &
      isynch,isw5,isw6,isw7,isw8,isw9,spot1parm,spot2parm,  &
      spotdparm,primmass,primk,primrad,ratrad,frac1,frac2,ecosw,  &
      temprat,idark1,idark2,isw12,isw13,isw21,isw22,isw23,isw24,  &
      bigi,bigbeta,sw23,sw24,powercoeff,sw25,sw26,sw27,sw28,sw29,  &
      sw30,contam,tconj,beam1,beam2,isw25,isw26,isw27,isw28,isw29,  &
      isw30,isw31,isw32,isw33,isw34,ocose,osine,omegadot,contams0,  &
      contams1,contams2,contams3,sw47,sw48,sw49,sw80,sw81,sw82,  &
      sw83,sw84,sw85,sw86,sw87,sw88,sw89,isw80,isw81,isw82,isw83,  &
      isw84,isw85,isw86,isw87,isw88,isw89,sdarkint1,sdarkint2,  &
      sdarkint3,sdarkint4,sdarkint5)
!
!   Put this if-then block for successful completion.
!
   IF(ios == 0)THEN
      CLOSE(1)
      RETURN
   END IF
!
!  file ended too soon
!
40 bell=CHAR(7)
   WRITE(*,50)bell
   CLOSE(1)
!
   RETURN
!
50 FORMAT(a1,'Error:  Bad entry in ELC.inp')
!
END SUBROUTINE getinput
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getlength(nlength,nvar,svar,nvmax,ioptimizer)
!
!   Will read the gridloop.opt file to get the length
!   of each generation and the number of parameters
!   fitted.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(OUT)                     :: nlength
   INTEGER, INTENT(OUT)                     :: nvar
   CHARACTER (LEN=40), INTENT(OUT)          :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: ioptimizer
!
   REAL(KIND=dp)  :: var,vstart,vstep,obv,eobv
!
   INTEGER :: nstep,nobv, icnvrt,i,j
!
   CHARACTER(LEN=40) :: bdatafile,vdatafile,rdatafile,idatafile
   CHARACTER(LEN=40) :: udatafile,hdatafile,kdatafile,rv1file
   CHARACTER(LEN=40) :: rv2file,sobv(19),jdatafile
   CHARACTER(LEN=40) :: rv3file,rv4file,rv5file
!
   DIMENSION var(nvmax),vstart(nvmax),vstep(nvmax),nstep(nvmax)
   DIMENSION obv(19),eobv(19)
!
   CALL getloopopt(udatafile,bdatafile,vdatafile,rdatafile,  &
      idatafile,jdatafile,hdatafile,kdatafile,rv1file,rv2file,  &
      nvmax,nvar,svar,var,vstart,vstep,nstep,nobv,sobv,obv,eobv,  &
      rv3file,rv4file,rv5file)
!
   nlength=nstep(1)
   IF(ioptimizer == 10)nlength=2*nlength
!
   j=0
   DO i=1,nvar
      IF(icnvrt(svar(i)(1:2)) /= 430)j=j+1
   END DO
!
   nvar=j
!
   RETURN
!
END SUBROUTINE getlength
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getloopopt(udatafile,bdatafile,vdatafile,  &
   rdatafile,idatafile,jdatafile,hdatafile,kdatafile,rv1file,  &
   rv2file,nvmax,nvar,svar,var,vstart,vstep,nstep,nobv,sobv,obv,  &
   eobv,rv3file,rv4file,rv5file)
!
!    November 12, 1999
!
!    This routine will read the input file to set up the optimizer (either
!    the 'grid search' or the Levenburg-Marquardt routine).
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=40), INTENT(OUT)          :: udatafile
   CHARACTER (LEN=40), INTENT(OUT)          :: bdatafile
   CHARACTER (LEN=40), INTENT(OUT)          :: vdatafile
   CHARACTER (LEN=40), INTENT(OUT)          :: rdatafile
   CHARACTER (LEN=40), INTENT(OUT)          :: idatafile
   CHARACTER (LEN=40), INTENT(OUT)          :: jdatafile
   CHARACTER (LEN=40), INTENT(OUT)          :: hdatafile
   CHARACTER (LEN=40), INTENT(OUT)          :: kdatafile
   CHARACTER (LEN=40), INTENT(OUT)          :: rv1file
   CHARACTER (LEN=40), INTENT(OUT)          :: rv2file
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(OUT)                     :: nvar
   CHARACTER (LEN=40), INTENT(OUT)          :: svar(nvmax)
   REAL(KIND=dp), INTENT(OUT)               :: var(nvmax)
   REAL(KIND=dp), INTENT(OUT)               :: vstart(nvmax)
   REAL(KIND=dp), INTENT(OUT)               :: vstep(nvmax)
   INTEGER, INTENT(OUT)                     :: nstep(nvmax)
   INTEGER, INTENT(OUT)                     :: nobv
   CHARACTER (LEN=40), INTENT(OUT)          :: sobv(19)
   REAL(KIND=dp), INTENT(OUT)               :: obv(19)
   REAL(KIND=dp), INTENT(OUT)               :: eobv(19)
   CHARACTER (LEN=40), INTENT(OUT)          :: rv3file
   CHARACTER (LEN=40), INTENT(IN OUT)       :: rv4file
   CHARACTER (LEN=40), INTENT(IN OUT)       :: rv5file
!
   INTEGER :: i,ios,isnumber,k1,iosopen,j
!
   CHARACTER(LEN=1) :: bell
   CHARACTER(LEN=40) :: line
!
   DO  i=1,19
      sobv(i)='none'
      obv(i)=-99.0_dp
      eobv(i)=-99.0_dp
   END DO
   nobv=0
!
   DO  i=1,nvmax
      svar(i)='none'
   END DO
!
   WRITE(rv3file,120)
   WRITE(rv4file,121)
   WRITE(rv5file,122)
!
   bell=CHAR(7)
   ios=0
   iosopen=0
   OPEN(UNIT=11,FILE='gridloop.opt',STATUS='old',ERR=90,IOSTAT= iosopen)
!
   READ(11,100)udatafile
   READ(11,100)bdatafile
   READ(11,100)vdatafile
   READ(11,100)rdatafile
   READ(11,100)idatafile
   READ(11,100)jdatafile
   READ(11,100)hdatafile
   READ(11,100)kdatafile
   READ(11,100)rv1file
   READ(11,100)rv2file
!
!  In the old format, the next line should be an integer.  In
!  the new format, we have three more lines of strings.
!  look at the next line and determine if it is an integer.
!
   isnumber=0
   READ(11,100)line
   k1=LEN_TRIM(line)
   ios=0
   DO  i=1,k1
      j=ICHAR(line(i:i))
!
!  Check for leading blank (ICHAR = 32)
!
      IF(j == 32)CYCLE
!
!  Check for digits 0 through 9 (ICHAR between 48 and
!  57)
!
      IF((j < 48).OR.(j > 57))ios=99
   END DO
!
   IF(ios /= 0)THEN
!
!   Looks like a string, so read two more lines, since
!   the string line is already RV3file.
!
      rv3file=TRIM(line)
      READ(11,100)rv4file
      READ(11,100)rv5file
   ELSE
!
!   Looks like a number, set the value of Nvar to that value
!
      BACKSPACE(11)
   END IF
!
   READ(11,*)nvar
!
   IF(nvar == 0)GO TO 50
   IF(nvar > nvmax)THEN
      WRITE(*,110)bell
      STOP
   END IF
!
   DO  i=1,nvar
      READ(11,100)svar(i)
   END DO
!
   DO  i=1,nvar
      READ(11,*)vstart(i),vstep(i),nstep(i)
      var(i)=vstart(i)
   END DO
!
!  Look here for possible observed parameters
!
50 READ(11,*,END=80,ERR=80)nobv
!
   DO  i=1,nobv
      READ(11,100)sobv(i)
   END DO
   DO  i=1,nobv
      READ(11,*)obv(i),eobv(i)
   END DO
!
80 CLOSE(11)
!
!  error in opening the file
!
90 IF(iosopen /= 0)THEN
      CALL makeloopopt(nvmax)
   END IF
!
100 FORMAT(a40)
110 FORMAT(a1,'Error:  too many variables in ''gridloop.opt''')
120 FORMAT('none RV3file',28X)
121 FORMAT('none RV4file',28X)
122 FORMAT('none RV5file',28X)
!
   RETURN
!
END SUBROUTINE getloopopt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getmaxparm(maxparm)
!
!   Will determine how many columns in the ELCparm.* files
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                      :: maxparm
!
   INTEGER :: i
!
!   Find the maximum number of allowed parameters by reading
!   the last index of ELC.parm
!
   OPEN(UNIT=1,FILE='ELC.parm',STATUS='old',ERR=50)
!
   DO  i=1,199
      READ(1,*,END=20)maxparm
   END DO
20 CLOSE(1)
!
   RETURN
!
!  Can't find the file ELC.parm
!
50 WRITE(*,*)'ELC.parm not found. Setting Nparm=0'
!
   RETURN
!
END SUBROUTINE getmaxparm
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getnbin(nbin)
!
!   Get the number of bins from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nbin
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg
!
   ios=0
   CALL get_command_argument(1,arg)
   READ(arg,*,IOSTAT=ios)nbin
   IF(ios == 0)RETURN
!
END SUBROUTINE getnbin
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getnestedfiles(ioptimizer,nfiles)
!
!   Will count the number of ELClivegeneration files.)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(OUT)                     :: nfiles
!
   INTEGER :: i
!
   CHARACTER(LEN=60) :: command,filein
!
!   case for demcmcELC
!
   IF(ioptimizer == 2)THEN
      command='ls demcmc_fitparm.1* > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=10)filein
      END DO
10    CLOSE(33)
      nfiles=i-1
      RETURN
   END IF
!
!   case for hammerELC
!
   IF(ioptimizer == 10)THEN
      command='ls hammer_fitparm.1* > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=20)filein
         IF(filein(1:2) == 'qt')filein='junk'
      END DO
20    CLOSE(33)
      nfiles=i-1
      RETURN
   END IF
!
!   case for geneticELC
!
   IF(ioptimizer == 3)THEN
      command='ls generation.1* > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=30)filein
      END DO
30    CLOSE(33)
      nfiles=i-1
      RETURN
   END IF
!
!   case for nestedELC
!
   IF(ioptimizer == 12)THEN
      command='ls ELClivegeneration.* > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=32)filein
      END DO
32    CLOSE(33)
      nfiles=i-1
      RETURN
   END IF
!
40 FORMAT(a60)
!
   RETURN
!
END SUBROUTINE getnestedfiles
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getnestedlength(nlength,nvar,svar,nvmax,ioptimizer)
!
!   Will read the gridloop.opt file to get the length
!   of each generation and the number of parameters
!   fitted.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(OUT)                     :: nlength
   INTEGER, INTENT(OUT)                     :: nvar
   CHARACTER (LEN=40), INTENT(OUT)          :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: ioptimizer
!
   REAL(KIND=dp)  :: var,vstart,vstep,obv,eobv
!
   INTEGER :: nstep,nobv
!
   CHARACTER(LEN=40) :: bdatafile,vdatafile,rdatafile,idatafile
   CHARACTER(LEN=40) :: udatafile,hdatafile,kdatafile,rv1file
   CHARACTER(LEN=40) :: rv2file,sobv(19),jdatafile
   CHARACTER(LEN=40) :: rv3file,rv4file,rv5file
!
   DIMENSION var(nvmax),vstart(nvmax),vstep(nvmax),nstep(nvmax)
   DIMENSION obv(19),eobv(19)
!
   CALL getloopopt(udatafile,bdatafile,vdatafile,rdatafile,  &
      idatafile,jdatafile,hdatafile,kdatafile,rv1file,rv2file,  &
      nvmax,nvar,svar,var,vstart,vstep,nstep,nobv,sobv,obv,eobv,  &
      rv3file,rv4file,rv5file)
!
   nlength=nstep(2)
   IF(ioptimizer == 10)nlength=2*nlength
!
   RETURN
!
END SUBROUTINE getnestedlength
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getnestedlive(nlive,nvar,svar,nvmax)
!
!   Will read the gridloop.opt file to get the number of live
!   points and the number of parameters
!   fitted.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(OUT)                     :: nlive
   INTEGER, INTENT(OUT)                     :: nvar
   CHARACTER (LEN=40), INTENT(OUT)          :: svar(nvmax)
!
   REAL(KIND=dp)  :: var,vstart,vstep,obv,eobv
!
   INTEGER :: nstep,nobv,icnvrt,i,j
!
   CHARACTER(LEN=40) :: bdatafile,vdatafile,rdatafile,idatafile
   CHARACTER(LEN=40) :: udatafile,hdatafile,kdatafile,rv1file
   CHARACTER(LEN=40) :: rv2file,sobv(19), jdatafile
   CHARACTER(LEN=40) :: rv3file,rv4file,rv5file
!
   DIMENSION var(nvmax),vstart(nvmax),vstep(nvmax),nstep(nvmax)
   DIMENSION obv(19),eobv(19)
!
   CALL getloopopt(udatafile,bdatafile,vdatafile,rdatafile,  &
      idatafile,jdatafile,hdatafile,kdatafile,rv1file,rv2file,  &
      nvmax,nvar,svar,var,vstart,vstep,nstep,nobv,sobv,obv,eobv,  &
      rv3file,rv4file,rv5file)
!
   nlive=nstep(2)
!
   j=0
   DO i=1,nvar
      IF(icnvrt(svar(i)(1:2)) /= 430)j=j+1
   END DO
!
   nvar=j
!
   RETURN
!
END SUBROUTINE getnestedlive
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getnestpost(nbin,nsamp)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nbin
   INTEGER, INTENT(OUT)                     :: nsamp
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg1,arg2
!
   ios=0
   CALL get_command_argument(1,arg1)
   READ(arg1,*,IOSTAT=ios)nbin
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)nsamp
   IF(ios == 0)RETURN
!
END SUBROUTINE getnestpost
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getnumberchifiles(ioptimizer,nfiles)
!
!   Will count the number of chi files (demcmc_chi*
!   etc.)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(OUT)                     :: nfiles
!
   INTEGER :: i,nchi
!
   CHARACTER(LEN=100) :: command,filein
!
!   case for demcmcELC
!
   nchi=0
   nfiles=0
   IF(ioptimizer == 2)THEN
!      command='ls demcmc_chi.1* > ELCjunk'
      command='find . -maxdepth 1 -name "demcmc_chi.1*" -print0 | xargs ' // &
          ' -0 ls | sort > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=10)filein
      END DO
10    CLOSE(33)
      nfiles=i-1
!
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=15)filein
         nchi=nchi+1
      END DO
15    CLOSE(33)
      IF(nchi <= 1)THEN
         command='rm ELCjunk'
         CALL execute_command_line(command)
         WRITE(*,*)'No chi^2 files'
         STOP
      END IF
!
      RETURN
   END IF
!
!   case for hammerELC
!
   IF(ioptimizer == 10)THEN
!      command='ls hammer_chi.1* > ELCjunk'
      command='find . -maxdepth 1 -name "hammer_chi.1*" -print0 | xargs ' // &
            ' -0 ls | sort > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=20)filein
         IF(filein(1:2) == 'qt')filein='junk'
      END DO
20    CLOSE(33)
      nfiles=i-1
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=25)filein
         nchi=nchi+1
      END DO
25    CLOSE(33)
      IF(nchi <= 1)THEN
         command='rm ELCjunk'
         CALL execute_command_line(command)
         WRITE(*,*)'No chi^2 files'
         STOP
      END IF
!
      RETURN
   END IF
!
!   case for geneticELC
!
   IF(ioptimizer == 3)THEN
!      command='ls chi.1* > ELCjunk'
      command='find . -maxdepth 1 -name "chi.1*" -print0 | xargs -0 ls ' // &
         ' | sort > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=30)filein
      END DO
30    CLOSE(33)
      nfiles=i-1
      RETURN
   END IF
!
!   case for nestedELC
!
   IF(ioptimizer == 12)THEN
!      command='ls chi.1* > ELCjunk'
      command='find . -maxdepth 1 -name "chi.1*" -print0 | xargs -0 ' // &
          ' ls | sort > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,99999
         READ(33,40,END=32)filein
      END DO
32    CLOSE(33)
      nfiles=i-1
      RETURN
   END IF
!
40 FORMAT(a100)
!
   RETURN
!
END SUBROUTINE getnumberchifiles
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getnumberfiles(ioptimizer,nfiles)
!
!   Will count the number of files (generation.*, demcmc_fitparm*
!   etc.)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(OUT)                     :: nfiles
!
   INTEGER :: i,nchi
!
   CHARACTER(LEN=100) :: command,filein
!
   nchi=0
   nfiles=0
!
!   case for demcmcELC
!
   IF(ioptimizer == 2)THEN
!      command='ls demcmc_fitparm.1* > ELCjunk'
      command='find . -maxdepth 1 -name "demcmc_fitparm.1*" -print0 | ' // &
        'xargs -0 ls | sort > ELCjunk'
      CALL execute_command_line(command)
!      command='ls demcmc_chi.1* > ELCjunkchi'
      command='find . -maxdepth 1 -name "demcmc_chi.1*" -print0 | xargs ' // &
          '-0 ls | sort > ELCjunkchi'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,999999
         READ(33,40,END=10)filein
      END DO
10    CLOSE(33)
      nfiles=i-1
!
      OPEN(UNIT=33,FILE='ELCjunkchi',STATUS='old')
      DO i=1,999999
         READ(33,40,END=15)filein
         nchi=nchi+1
      END DO
15    CLOSE(33)
      IF(nchi <= 1)THEN
         command='rm ELCjunkchi'
         CALL execute_command_line(command)
      END IF
!
      RETURN
   END IF
!
!   case for hammerELC
!
   IF(ioptimizer == 10)THEN
!      command='ls hammer_fitparm.1* > ELCjunk'
      command='find . -maxdepth 1 -name "hammer_fitparm.1*" -print0 | ' // &
          'xargs -0 ls | sort > ELCjunk'
      CALL execute_command_line(command)
!      command='ls hammer_chi.1* > ELCjunkchi'
      command='find . -maxdepth 1 -name "hammer_chi.1*" -print0 | xargs ' // &
          ' -0 ls | sort > ELCjunkchi'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,999999
         READ(33,40,END=20)filein
         IF(filein(1:2) == 'qt')filein='junk'
      END DO
20    CLOSE(33)
      nfiles=i-1
!
      OPEN(UNIT=33,FILE='ELCjunkchi',STATUS='old')
      DO i=1,999999
         READ(33,40,END=25)filein
         nchi=nchi+1
      END DO
25    CLOSE(33)
      IF(nchi <= 1)THEN
         command='rm ELCjunkchi'
         CALL execute_command_line(command)
      END IF
!
      RETURN
   END IF
!
!   case for geneticELC
!
   IF(ioptimizer == 3)THEN
!      command='ls generation.1* > ELCjunk'
      command='find . -maxdepth 1 -name "generation.1*" -print0 | xargs ' // &
           ' -0 ls | sort > ELCjunk'
      CALL execute_command_line(command)
!      command='ls chi.1* > ELCjunkchi'
      command='find . -maxdepth 1 -name "chi.1*" -print0 | xargs -0 ls ' // &
         ' | sort > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,999999
         READ(33,40,END=30)filein
      END DO
30    CLOSE(33)
      nfiles=i-1
      RETURN
   END IF
!
!   case for nestedELC
!
   IF(ioptimizer == 12)THEN
!      command='ls generation.1* > ELCjunk'
      command='find . -maxdepth 1 -name "generation.1*" -print0 | ' // &
        'xargs -0 ls | sort > ELCjunk'
      CALL execute_command_line(command)
!      command='ls chi.1* > ELCjunkchi'
      command='find . -maxdepth 1 -name "chi.1*" -print0 | xargs -0 ls ' // &
          ' | sort > ELCjunk'
      CALL execute_command_line(command)
      OPEN(UNIT=33,FILE='ELCjunk',STATUS='old')
      DO i=1,999999
         READ(33,40,END=32)filein
      END DO
32    CLOSE(33)
      nfiles=i-1
      RETURN
   END IF
!
40 FORMAT(a100)
!
   RETURN
!
END SUBROUTINE getnumberfiles
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getparm(nparm,indexparm,maxparm,isepar)
!
!  Will look for an open an optional file called
!  ELCcolumns.inp.  This file should contain integers
!  giving the indices of the derived parameters in ELCparm.*
!  files
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nparm
   INTEGER, INTENT(OUT)                     :: indexparm(199)
   INTEGER, INTENT(OUT)                     :: maxparm
   INTEGER, INTENT(OUT)                     :: isepar
!
   INTEGER :: i,j,icount
!
!   Find the maximum number of allowed parameters by reading
!   the last index of ELC.parm
!
   OPEN(UNIT=1,FILE='ELC.parm',STATUS='old',ERR=50)
!
   DO  i=1,199
      READ(1,*,END=20)maxparm
   END DO
20 CLOSE(1)
!
!   Attemp to open ELCcolumns.inp and read the data therein.
!
   OPEN(UNIT=1,FILE='ELCcolumns.inp',STATUS='old',ERR=60)
!
   isepar=0
   icount=0
   DO  i=1,maxparm
      READ(1,*,END=40,ERR=70)j
      IF((j > 1).AND.(j <= maxparm))THEN
         icount=icount+1
         indexparm(icount)=j
         IF(j == 3)isepar=isepar+1
         IF(j == 4)isepar=isepar+1
         IF(j == 35)isepar=isepar+1
      END IF
   END DO
40 CLOSE(1)
!
   nparm=icount
!
   RETURN
!
!  Can't find the file ELC.parm
!
50 WRITE(*,*)'ELC.parm not found. Setting Nparm=0'
   RETURN
!
!  File ELCcolumns.inp does not exist, set Nparm=0 and
!  return
!
60 nparm=0
   RETURN
!
!  Error in ELCcolumns.inp, set Nparm=0 and return
!
70 WRITE(*,*)'Error reading ELCcolumns.inp.'
   WRITE(*,*)'Setting Nparm=0.'
   nparm=0
   RETURN
!
END SUBROUTINE getparm
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getparmspread(choice,tag,istart,istop,narray,  &
   iarray)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=40), INTENT(OUT)          :: choice
   CHARACTER (LEN=40), INTENT(OUT)          :: tag
   INTEGER, INTENT(OUT)                     :: istart
   INTEGER, INTENT(OUT)                     :: istop
   INTEGER, INTENT(OUT)                     :: narray
   INTEGER, INTENT(OUT)                     :: iarray(99)
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg3,arg4,blank40
   CHARACTER(LEN=100) :: arg5,blank100
!
   ios=0
   WRITE(blank40,100)
   WRITE(blank100,110)
!
   CALL get_command_argument(1,choice)
   IF(choice == blank40)THEN
      WRITE(*,120)
      WRITE(*,130)
      WRITE(*,140)
      WRITE(*,150)
      WRITE(*,160)
      STOP
   END IF
!
   CALL get_command_argument(2,tag)
   IF(tag == blank40)THEN
      WRITE(*,120)
      WRITE(*,130)
      WRITE(*,140)
      WRITE(*,150)
      WRITE(*,160)
      STOP
   END IF
!
   CALL get_command_argument(3,arg3)
   IF(arg3 == blank40)RETURN
   READ(arg3,*,IOSTAT=ios)istart
   IF(ios /= 0)THEN
      WRITE(*,170)
      RETURN
   END IF
!
   CALL get_command_argument(4,arg4)
   IF(arg4 == blank40)RETURN
   READ(arg4,*,IOSTAT=ios)istop
   IF(ios /= 0)THEN
      WRITE(*,180)
      RETURN
   END IF
!
   CALL get_command_argument(5,arg5)
   IF(arg5 == blank100)RETURN
   CALL parsecomma(arg5,narray,iarray)
   IF(narray > 1)CALL isort(narray,iarray)
!
   RETURN
!
100 FORMAT(40X)
110 FORMAT(100X)
120 FORMAT('error: Use ELCspreadparm CHOICE TAG')
130 FORMAT('CHOICE is FITTED or PARM or RATIO')
140 FORMAT('TAG is from gridloop.opt when CHOICE=FITTED')
150 FORMAT('TAG is number from ELC.parm when CHOICE=PARM')
160 FORMAT('TAG is 1 through 24 when CHOICE=RATIO')
170 FORMAT('error reading entry, setting istart = 1 ', 'istop = 999999')
180 FORMAT('error reading entry, setting istop = 999999')
!
END SUBROUTINE getparmspread
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getplotpost(nbin,clip)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nbin
   REAL(KIND=dp), INTENT(OUT)               :: clip
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg1,arg2
!
   ios=0
   CALL get_command_argument(1,arg1)
   READ(arg1,*,IOSTAT=ios)nbin
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)clip
   IF(ios == 0)RETURN
!
END SUBROUTINE getplotpost
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getpost(istart,nskip,istop,nbin,clip)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: istart
   INTEGER, INTENT(OUT)                     :: nskip
   INTEGER, INTENT(OUT)                     :: istop
   INTEGER, INTENT(OUT)                     :: nbin
   REAL(KIND=dp), INTENT(OUT)               :: clip
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg1,arg2,arg3,arg4,arg5
!
   ios=0
   CALL get_command_argument(1,arg1)
   READ(arg1,*,IOSTAT=ios)istart
!
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)nskip
!
   CALL get_command_argument(3,arg3)
   READ(arg3,*,IOSTAT=ios)istop
!
   CALL get_command_argument(4,arg4)
   READ(arg4,*,IOSTAT=ios)nbin
!
   CALL get_command_argument(5,arg5)
   READ(arg5,*,IOSTAT=ios)clip
   IF(ios == 0)RETURN
!
END SUBROUTINE getpost
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getratio(nparm,indexparm,maxparm)
!
!  Will look for an open an optional file called
!  ELCratio.inp.  This file should contain integers
!  giving the indices of the flux ratios in ELCratio.*
!  files
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nparm
   INTEGER, INTENT(OUT)                     :: indexparm(199)
   INTEGER, INTENT(OUT)                     :: maxparm
!
   INTEGER :: i, j, icount
!
   maxparm=24
!
!   Attemp to open ELCratio.inp and read the data therein.
!
   OPEN(UNIT=1,FILE='ELCratio.inp',STATUS='old',ERR=30)
!
   icount=0
   DO  i=1,maxparm
      READ(1,*,END=20,ERR=40)j
      IF((j >= 1).AND.(j <= maxparm))THEN
         icount=icount+1
         indexparm(icount)=j
      END IF
   END DO
20 CLOSE(1)
!
   nparm=icount
!
   RETURN
!
!  File ELCratio.inp does not exist, set Nparm=0 and
!  return
!
30 nparm=0
   RETURN
!
!  Error in ELCratio.inp, set Nparm=0 and return
!
40 WRITE(*,*)'Error reading ELCratio.inp.'
   WRITE(*,*)'Setting Nratio=0.'
   nparm=0
   RETURN
!
END SUBROUTINE getratio
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getskip(istart,nskip)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: istart
   INTEGER, INTENT(OUT)                     :: nskip
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg1,arg2
!
   ios=0
   CALL get_command_argument(1,arg1)
   READ(arg1,*,IOSTAT=ios)istart
!
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)nskip
   IF(ios == 0)RETURN
!
END SUBROUTINE getskip
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getspread(istart,istop,narray,iarray)
!
!   Get the starting file number from the command line
!
   USe accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: istart
   INTEGER, INTENT(OUT)                     :: istop
   INTEGER, INTENT(IN OUT)                  :: narray
   INTEGER, INTENT(IN OUT)                  :: iarray(99)
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg1,arg2,BLANK
   CHARACTER(LEN=100) :: arg3,blank3
!
   ios=0
   WRITE(BLANK,100)
   WRITE(blank3,110)
!
   CALL get_command_argument(1,arg1)
   IF(arg1 == BLANK)RETURN
   READ(arg1,*,IOSTAT=ios)istart
   IF(ios /= 0)THEN
      WRITE(*,120)
      RETURN
   END IF
!
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)istop
   IF(arg2 == BLANK)RETURN
   IF(ios /= 0)THEN
      WRITE(*,130)
      RETURN
   END IF
!
   CALL get_command_argument(3,arg3)
   IF(arg3 == blank3)RETURN
   CALL parsecomma(arg3,narray,iarray)
   IF(narray > 1)CALL isort(narray,iarray)
!
   RETURN
!
100 FORMAT(40X)
110 FORMAT(100X)
120 FORMAT('error reading entry, setting istart = 1 ', 'istop = 999999')
130 FORMAT('error reading entry, setting istop = 999999')
!
END SUBROUTINE getspread
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getstart(istart)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: istart
!
   INTEGER :: ios
!
   CHARACTER (LEN=40) :: arg
!
   ios=0
   CALL get_command_argument(1,arg)
   READ(arg,*,IOSTAT=ios)istart
   IF(ios == 0)RETURN
!
END SUBROUTINE getstart
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getsuffix(instr1,suffixstr,labelstr)
!
!  This subroutine will take an input tag, and return strings
!  to be used as file extensions and as labels.  For example,
!  "pe" gives "period" as a file extension and "Period (days)"
!  as a label.
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: instr1
   CHARACTER (LEN=80 ), INTENT(OUT)         :: suffixstr
   CHARACTER (LEN=80 ), INTENT(OUT)         :: labelstr
!
   INTEGER :: icnvrt,kk,it1
!
!  Figure out the variable that goes with the input tag.
!
   kk=icnvrt(instr1(1:2))
!
!  tag ct, tesscontam
!
   IF(kk == 83)THEN
      suffixstr='tesscontam'
      labelstr='TESS contaminatione'
   END IF
!
!  tag bq, binqTc, (slope in P-Tc plane for binary)
!
   IF(kk == 48)THEN
      suffixstr='binqTc'
      labelstr='binary P-Tc plane slope'
   END IF
!
!  tag 1q, P1qTc, (slope in P-Tc plane for body 3)
!
   IF(kk == 3528)THEN
      suffixstr='P1qTc'
      labelstr='body 3 P-Tc plane slope'
   END IF
!
!  tag 2q, P2qTc, (slope in P-Tc plane for body 4)
!
   IF(kk == 3560)THEN
      suffixstr='P2qTc'
      labelstr='body 4 P-Tc plane slope'
   END IF
!
!  tag 3q, P3qTc, (slope in P-Tc plane for body 5)
!
   IF(kk == 3592)THEN
      suffixstr='P3qTc'
      labelstr='body 5 P-Tc plane slope'
   END IF
!
!  tag 4q, P4qTc, (slope in P-Tc plane for body 6)
!
   IF(kk == 3624)THEN
      suffixstr='P4qTc'
      labelstr='body 6 P-Tc plane slope'
   END IF
!
!  tag 5q, P5qTc, (slope in P-Tc plane for body 7)
!
   IF(kk == 3656)THEN
      suffixstr='P5qTc'
      labelstr='body 7 P-Tc plane slope'
   END IF
!
!  tag 6q, P6qTc, (slope in P-Tc plane for body 8)
!
   IF(kk == 3688)THEN
      suffixstr='P6qTc'
      labelstr='body 8 P-Tc plane slope'
   END IF
!
!  tag 7q, P7qTc, (slope in P-Tc plane for body 9)
!
   IF(kk == 3720)THEN
      suffixstr='P7qTc'
      labelstr='body 9 P-Tc plane slope'
   END IF
!
!  tag 8q, P8qTc, (slope in P-Tc plane for body 10)
!
   IF(kk == 3752)THEN
      suffixstr='P8qTc'
      labelstr='body 10 P-Tc plane slope'
   END IF
!
!  tag bt, Tbinoff, (offset in P-Tc plane for binary)
!
   IF(kk == 51)THEN
      suffixstr='Tbinoff'
      labelstr='T offset binary'
   END IF
!
!  tag 1t, T1off, (offset in P-Tc plane for body 3)
!
   IF(kk == 3531)THEN
      suffixstr='T1off'
      labelstr='T offset body 3'
   END IF
!
!  tag 2t, T2off, (offset in P-Tc plane for body 4)
!
   IF(kk == 3563)THEN
      suffixstr='T2off'
      labelstr='T offset body 4'
   END IF
!
!  tag 3t, T3off, (offset in P-Tc plane for body 5)
!
   IF(kk == 3595)THEN
      suffixstr='T3off'
      labelstr='T offset body 5'
   END IF
!
!  tag 4t, T4off, (offset in P-Tc plane for body 6)
!
   IF(kk == 3627)THEN
      suffixstr='T4off'
      labelstr='T offset body 6'
   END IF
!
!  tag 5t, T5off, (offset in P-Tc plane for body 7)
!
   IF(kk == 3659)THEN
      suffixstr='T5off'
      labelstr='T offset body 7'
   END IF
!
!  tag 6t, T6off, (offset in P-Tc plane for body 8)
!
   IF(kk == 3691)THEN
      suffixstr='T6off'
      labelstr='T offset body 8'
   END IF
!
!  tag 7t, T7off, (offset in P-Tc plane for body 9)
!
   IF(kk == 3723)THEN
      suffixstr='T7off'
      labelstr='T offset body 9'
   END IF
!
!  tag 8t, T8off, (offset in P-Tc plane for body 10)
!
   IF(kk == 3755)THEN
      suffixstr='T8off'
      labelstr='T offset body 10'
   END IF
!
!  tag sf, fillsum (sum of fill1 and fill2)
!
   IF(kk == 581)THEN
      suffixstr='fillsum'
      labelstr='fill1+fill2'
   END IF
!
!  tag sd, filldiff (difference of fill1 and fill2)
!
   IF(kk == 579)THEN
      suffixstr='filldiff'
      labelstr='fill1-fill2'
   END IF
!
!  tag 1a, angsum1 (sum of P1incl and P1Omega)
!
   IF(kk == 3512)THEN
      suffixstr='angsum1'
      labelstr='P1incl+P1Omega (deg)'
   END IF
!
!  tag 2a, angsum2 (sum of P2incl and P2Omega)
!
   IF(kk == 3544)THEN
      suffixstr='angsum2'
      labelstr='P2incl+P2Omega (deg)'
   END IF
!
!  tag 3a, angsum3 (sum of P3incl and P3Omega)
!
   IF(kk == 3576)THEN
      suffixstr='angsum3'
      labelstr='P3incl+P3Omega (deg)'
   END IF
!
!  tag 4a, angsum4 (sum of P4incl and P4Omega)
!
   IF(kk == 3608)THEN
      suffixstr='angsum4'
      labelstr='P4incl+P4Omega (deg)'
   END IF
!
!  tag 5a, angsum5 (sum of P5incl and P5Omega)
!
   IF(kk == 3640)THEN
      suffixstr='angsum5'
      labelstr='P5incl+P5Omega (deg)'
   END IF
!
!  tag 6a, angsum6 (sum of P6incl and P6Omega)
!
   IF(kk == 3672)THEN
      suffixstr='angsum6'
      labelstr='P6incl+P6Omega (deg)'
   END IF
!
!  tag 7a, angsum7 (sum of P7incl and P7Omega)
!
   IF(kk == 3704)THEN
      suffixstr='angsum7'
      labelstr='P7incl+P7Omega (deg)'
   END IF
!
!  tag 8a, angsum8 (sum of P8incl and P8Omega)
!
   IF(kk == 3736)THEN
      suffixstr='angsum8'
      labelstr='P8incl+P8Omega (deg)'
   END IF
!
!  tag 1d, angdiff1 (difference of P1incl and P1Omega)
!
   IF(kk == 3515)THEN
      suffixstr='angdiff1'
      labelstr='P1incl-P1Omega (deg)'
   END IF
!
!  tag 2d, angdiff2 (difference of P2incl and P2Omega)
!
   IF(kk == 3547)THEN
      suffixstr='angdiff2'
      labelstr='P2incl-P2Omega (deg)'
   END IF
!
!  tag 3d, angdiff3 (difference of P3incl and P3Omega)
!
   IF(kk == 3579)THEN
      suffixstr='angdiff3'
      labelstr='P3incl-P3Omega (deg)'
   END IF
!
!  tag 4d, angdiff4 (difference of P4incl and P4Omega)
!
   IF(kk == 3611)THEN
      suffixstr='angdiff4'
      labelstr='P4incl-P4Omega (deg)'
   END IF
!
!  tag 5d, angdiff5 (difference of P5incl and P5Omega)
!
   IF(kk == 3643)THEN
      suffixstr='angdiff5'
      labelstr='P5incl-P5Omega (deg)'
   END IF
!
!  tag 6d, angdiff6 (difference of P6incl and P6Omega)
!
   IF(kk == 3675)THEN
      suffixstr='angdiff6'
      labelstr='P6incl-P6Omega (deg)'
   END IF
!
!  tag 7d, angdiff7 (difference of P7incl and P7Omega)
!
   IF(kk == 3707)THEN
      suffixstr='angdiff7'
      labelstr='P7incl-P7Omega (deg)'
   END IF
!
!  tag 8d, angdiff8 (difference of P8incl and P8Omega)
!
   IF(kk == 3739)THEN
      suffixstr='angdiff8'
      labelstr='P8incl-P8Omega (deg)'
   END IF
!
!  tag fs, fracsum (sum of fractional radii)
!
   IF(kk == 178)THEN
      suffixstr='fracsum'
      labelstr='frac1+frac2'
   END IF
!
!  tag fd, fracdiff (difference of fractional radii)
!
   IF(kk == 163)THEN
      suffixstr='fracdiff'
      labelstr='frac1-frac2'
   END IF
!
!  tag 97, bin2M3 (M3 in solar masses in binary+binary)
!
   IF(kk == 4790)THEN
      suffixstr='bin2M3'
      labelstr='bin2M3 (solar)'
   END IF
!
!  tag 98, bin2M4 (M4 in solar masses in binary+binary)
!
   IF(kk == 4791)THEN
      suffixstr='bin2M4'
      labelstr='bin2M4 (solar)'
   END IF
!
!  tag 99, bin2R3 (R3 in solar radii in binary+binary)
!
   IF(kk == 4792)THEN
      suffixstr='bin2R3'
      labelstr='bin2R3 (solar)'
   END IF
!
!  tag 90, bin2R4 (R4 in solar radii in binary+binary)
!
   IF(kk == 4783)THEN
      suffixstr='bin2R4'
      labelstr='bin2R4 (solar)'
   END IF
!
!  tag bs, sqesin, (sqrt(e)*sin(omega), binary)
!
   IF(kk == 50)THEN
      suffixstr='sqrt_e_sin_omega'
      labelstr='sqrt(e)*sin(omega)'
   END IF
!
!  tag bc, sqecos, (sqrt(e)*sin(omega), binary)
!
   IF(kk == 34)THEN
      suffixstr='sqrt_e_cos_omega'
      labelstr='sqrt(e)*cos(omega)'
   END IF
!
!  tag 1s, sqtertesin, (sqrt(e)*sin(omega), body 3)
!
   IF(kk == 3530)THEN
      suffixstr='sqP1esin'
      labelstr='sqrt(e1)*sin(omega1)'
   END IF
!
!  tag 1c, sqtertecos, (sqrt(e)*sin(omega), body 3)
!
   IF(kk == 3514)THEN
      suffixstr='sqP1ecos'
      labelstr='sqrt(e1)*cos(omega1)'
   END IF
!
!  tag 2s, sqP2esin, (sqrt(e)*sin(omega), body 4)
!
   IF(kk == 3562)THEN
      suffixstr='sqP2esin'
      labelstr='sqrt(e2)*sin(omega2)'
   END IF
!
!  tag 2c, sqP2ecos, (sqrt(e)*sin(omega), body 4)
!
   IF(kk == 3546)THEN
      suffixstr='sqP2ecos'
      labelstr='sqrt(e2)*cos(omega2)'
   END IF
!
!  tag 3s, sqP3tesin, (sqrt(e)*sin(omega), body 5)
!
   IF(kk == 3594)THEN
      suffixstr='sqP3esin'
      labelstr='sqrt(e3)*sin(omega3)'
   END IF
!
!  tag 3c, sqP3tecos, (sqrt(e)*sin(omega), body 5)
!
   IF(kk == 3578)THEN
      suffixstr='sqP3ecos'
      labelstr='sqrt(e3)*cos(omega3)'
   END IF
!
!  tag 4s, sqP4tesin, (sqrt(e)*sin(omega), body 6)
!
   IF(kk == 3626)THEN
      suffixstr='sqP4esin'
      labelstr='sqrt(e4)*sin(omega4)'
   END IF
!
!  tag 4c, sqP4tecos, (sqrt(e)*sin(omega), body 6)
!
   IF(kk == 3610)THEN
      suffixstr='sqP4ecos'
      labelstr='sqrt(e4)*cos(omega4)'
   END IF
!
!  tag 5s, sqP5tesin, (sqrt(e)*sin(omega), body 7)
!
   IF(kk == 3658)THEN
      suffixstr='sqP5esin'
      labelstr='sqrt(e5)*sin(omega5)'
   END IF
!
!  tag 5c, sqP5tecos, (sqrt(e)*sin(omega), body 7)
!
   IF(kk == 3642)THEN
      suffixstr='sqP5ecos'
      labelstr='sqrt(e5)*cos(omega5)'
   END IF
!
!  tag 6s, sqP6tesin, (sqrt(e)*sin(omega), body 8)
!
   IF(kk == 3690)THEN
      suffixstr='sqP6esin'
      labelstr='sqr(e6)*sin(omega6)'
   END IF
!
!  tag 6c, sqP6tecos, (sqrt(e)*sin(omega), body 8)
!
   IF(kk == 3674)THEN
      suffixstr='sqP6ecos'
      labelstr='sqrt(e6)*cos(omega6)'
   END IF
!
!  tag 7s, sqP7tesin, (sqrt(e)*sin(omega), body 9)
!
   IF(kk == 3722)THEN
      suffixstr='sqP7esin'
      labelstr='sqrt(e7)*sin(omega7)'
   END IF
!
!  tag 7c, sqP7tecos, (sqrt(e)*sin(omega), body 9)
!
   IF(kk == 3706)THEN
      suffixstr='sqP7ecos'
      labelstr='sqrt(e7)*cos(omega7)'
   END IF
!
!  tag 8s, sqP8tesin, (sqrt(e)*sin(omega), body 10)
!
   IF(kk == 3754)THEN
      suffixstr='sqP8esin'
      labelstr='sqrt(e8)*sin(omega8)'
   END IF
!
!  tag 8c, sqP8tecos, (sqrt(e)*sin(omega), body 10)
!
   IF(kk == 3738)THEN
      suffixstr='sqP8ecos'
      labelstr='sqrt(e8)*cos(omega8)'
   END IF
!
!  tag 1m, P1mTc, period-Tconj, body 3
!
   IF(kk == 3524)THEN
      suffixstr='P1mTc'
      labelstr='P1-Tc1 (days)'
   END IF
!
!  tag 1p, P1pTc, period+Tconj, body 3
!
   IF(kk == 3527)THEN
      suffixstr='P1pTc'
      labelstr='P1+Tc1 (days)'
   END IF
!
!  tag 2m, P2mTc, period-Tconj, body 3
!
   IF(kk == 3556)THEN
      suffixstr='P2mTc'
      labelstr='P2-Tc2 (days)'
   END IF
!
!  tag 2p, P2pTc, period+Tconj, body 4
!
   IF(kk == 3559)THEN
      suffixstr='P2pTc'
      labelstr='P2+Tc2 (days)'
   END IF
!
!  tag 3m, P3mTc, period-Tconj, body 5
!
   IF(kk == 3588)THEN
      suffixstr='P3mTc'
      labelstr='P3-Tc3 (days)'
   END IF
!
!  tag 3p, P2pTc, period+Tconj, body 5
!
   IF(kk == 3591)THEN
      suffixstr='P3pTc'
      labelstr='P3+Tc3 (days)'
   END IF
!
!  tag 4m, P4mTc, period-Tconj, body 6
!
   IF(kk == 3620)THEN
      suffixstr='P4mTc'
      labelstr='P4-Tc4 (days)'
   END IF
!
!  tag 4p, P4pTc, period+Tconj, body 6
!
   IF(kk == 3623)THEN
      suffixstr='P4pTc'
      labelstr='P4+Tc4 (days)'
   END IF
!
!  tag 5m, P5mTc, period-Tconj, body 7
!
   IF(kk == 3652)THEN
      suffixstr='P5mTc'
      labelstr='P5-Tc5 (days)'
   END IF
!
!  tag 5p, P5pTc, period+Tconj, body 7
!
   IF(kk == 3655)THEN
      suffixstr='P5pTc'
      labelstr='P5+Tc5 (days)'
   END IF
!
!  tag 6m, P6mTc, period-Tconj, body 8
!
   IF(kk == 3684)THEN
      suffixstr='P6mTc'
      labelstr='P6-Tc6 (days)'
   END IF
!
!  tag 6p, P6pTc, period+Tconj, body 8
!
   IF(kk == 3687)THEN
      suffixstr='P6pTc'
      labelstr='P6+Tc6 (days)'
   END IF
!
!  tag 7m, P7mTc, period-Tconj, body 9
!
   IF(kk == 3716)THEN
      suffixstr='P7mTc'
      labelstr='P7-Tc7 (days)'
   END IF
!
!  tag 7p, P7pTc, period+Tconj, body 9
!
   IF(kk == 3719)THEN
      suffixstr='P7pTc'
      labelstr='P7+Tc7 (days)'
   END IF
!
!  tag 8m, P8mTc, period-Tconj, body 10
!
   IF(kk == 3748)THEN
      suffixstr='P8mTc'
      labelstr='P8-Tc8 (days)'
   END IF
!
!  tag 8p, P8pTc, period+Tconj, body 10
!
   IF(kk == 3751)THEN
      suffixstr='P8pTc'
      labelstr='P8+Tc8 (days)'
   END IF
!
!  tag bm, PbmTc, period-Tconj, binary
!
   IF(kk == 44)THEN
      suffixstr='PbinmTc'
      labelstr='Pbin-Tc (days)'
   END IF
!
!  tag bp, PbpTc, period+Tconj, binary
!
   IF(kk == 47)THEN
      suffixstr='PbinpTc'
      labelstr='Pbin+Tc (days)'
   END IF
!
!  tag bi, axis_I2, inclination of rot. axis star 2
!
   IF(kk == 40)THEN
      suffixstr='axis_I2'
      labelstr='axis_I2 (deg)'
   END IF
!
!  tag bb, axis_beta2, position angle of rot. axis star 2
!
   IF(kk == 33)THEN
      suffixstr='axis_beta2'
      labelstr='axis_beta2 (deg)'
   END IF
!
!  tag ci, axis_I3, inclination of rot. axis star 3
!
   IF(kk == 72)THEN
      suffixstr='axis_I3'
      labelstr='axis_I3 (deg)'
   END IF
!
!  tag cb, axis_beta3, position angle of rot. axis star 3
!
   IF(kk == 65)THEN
      suffixstr='axis_beta3'
      labelstr='axis_beta3 (deg)'
   END IF
!
!  tag di, axis_I4, inclination of rot. axis star 4
!
   IF(kk == 104)THEN
      suffixstr='axis_I4'
      labelstr='axis_I4 (deg)'
   END IF
!
!  tag db, axis_beta4, position angle of rot. axis star 4
!
   IF(kk == 97)THEN
      suffixstr='axis_beta4'
      labelstr='axis_beta4 (deg)'
   END IF
!
!  tag 91, bin2masssum (M3+M4 in binary+binary)
!
   IF(kk == 4784)THEN
      suffixstr='bin2masssum'
      labelstr='bin2 M\d3\u+M\d4\u (solar)'
   END IF
!
!  tag 92, bin2massdiff (M3-M4 in binary+binary)
!
   IF(kk == 4785)THEN
      suffixstr='bin2massdiff'
      labelstr='bin2 M\d3\u-M\d4\u (solar)'
   END IF
!
!  tag 93, bin2Q (M4/M3 in binary+binary)
!
   IF(kk == 4786)THEN
      suffixstr='bin2Q'
      labelstr='bin2Q (M\d4\u/M\d3\u)'
   END IF
!
!  tag 94, bin2radsum (R3+R4 in binary+binary)
!
   IF(kk == 4787)THEN
      suffixstr='bin2radsum'
      labelstr='bin2 R\d3\u+R\d4\u (solar)'
   END IF
!
!  tag 95, bin2raddiff (R3-R4 in binary+binary)
!
   IF(kk == 4788)THEN
      suffixstr='bin2raddiff'
      labelstr='bin2 R\d3\u-R\d4\u (solar)'
   END IF
!
!  tag 96, bin2ratrad (R3/R4 in binary+binary)
!
   IF(kk == 4789)THEN
      suffixstr='bin2ratrad'
      labelstr='bin2ratrad (R\d3\u/R\d4\u)'
   END IF
!
!   tag g6, g6
!
   IF(kk == 1213)THEN
      suffixstr='g6'
      labelstr='log(g6)'
   END IF
!
!   tag g7, g7
!
   IF(kk == 1214)THEN
      suffixstr='g7'
      labelstr='log(g7)'
   END IF
!
!   tag g8, g8
!
   IF(kk == 1215)THEN
      suffixstr='g8'
      labelstr='log(g8)'
   END IF
!
!   tag g9, g9
!
   IF(kk == 1216)THEN
      suffixstr='g9'
      labelstr='log(g9)'
   END IF
!
!   tag g0, g10
!
   IF(kk == 1207)THEN
      suffixstr='g10'
      labelstr='log(g10)'
   END IF
!
!   tag md, massdiff (M1-M2 for binary)
!
   IF(kk == 387)THEN
      suffixstr='massdiff'
      labelstr='M\d1\u-M\d2\u (solar)'
   END IF
!
!   tag ms, masssum (M1+M2 for binary)
!
   IF(kk == 402)THEN
      suffixstr='masssum'
      labelstr='M\d1\u+M\d2\u (solar)'
   END IF
!
!   tag o3, omega3
!
   IF(kk == 1466)THEN
      suffixstr='omega3'
      labelstr='omega3'
   END IF
!
!   tag o4, omega4
!
   IF(kk == 1467)THEN
      suffixstr='omega4'
      labelstr='omega4'
   END IF
!
!   tag o5, omega5
!
   IF(kk == 1468)THEN
      suffixstr='omega5'
      labelstr='omega5'
   END IF
!
!   tag o6, omega6
!
   IF(kk == 1469)THEN
      suffixstr='omega6'
      labelstr='omega6'
   END IF
!
!   tag o7, omega7
!
   IF(kk == 1470)THEN
      suffixstr='omega7'
      labelstr='omega7'
   END IF
!
!   tag o8, omega8
!
   IF(kk == 1471)THEN
      suffixstr='omega8'
      labelstr='omega8'
   END IF
!
!   tag o9, omega9
!
   IF(kk == 1472)THEN
      suffixstr='omega9'
      labelstr='omega9'
   END IF
!
!   tag o0, omega10
!
   IF(kk == 1463)THEN
      suffixstr='omega10'
      labelstr='omega10'
   END IF
!
!   tag rs, radsum (R1+R2 for binary)
!
   IF(kk == 562)THEN
      suffixstr='radsum'
      labelstr='R\d1\u+R\d2\u (solar)'
   END IF
!
!   tag rd, raddiff (R1-R2 for binary)
!
   IF(kk == 547)THEN
      suffixstr='raddiff'
      labelstr='R\d1\u-R\d2\u (solar)'
   END IF
!
!   tag a3, rk3 (apsidal constant, body 3)
!
   IF(kk == 1018)THEN
      suffixstr='rk3'
      labelstr='apsidal_k3'
   END IF
!
!   tag a4, rk4 (apsidal constant, body 4)
!
   IF(kk == 1019)THEN
      suffixstr='rk4'
      labelstr='aspsidal_k4'
   END IF
!
!   tag a5, rk5 (apsidal constant, body 5)
!
   IF(kk == 1020)THEN
      suffixstr='rk5'
      labelstr='apsidal_k5'
   END IF
!
!   tag a6, rk6 (apsidal constant, body 6)
!
   IF(kk == 1021)THEN
      suffixstr='rk6'
      labelstr='apsidal_k6'
   END IF
!
!   tag a7, rk7 (apsidal constant, body 7)
!
   IF(kk == 1022)THEN
      suffixstr='rk7'
      labelstr='apsidal_k7'
   END IF
!
!   tag a8, rk8 (apsidal constant, body 8)
!
   IF(kk == 1023)THEN
      suffixstr='rk8'
      labelstr='apsidal_k8'
   END IF
!
!   tag a9, rk9 (apsidal constant, body 9)
!
   IF(kk == 1024)THEN
      suffixstr='rk9'
      labelstr='apsidal_k9'
   END IF
!
!   tag a0, rk10 (apsidal constant, body 10)
!
   IF(kk == 1015)THEN
      suffixstr='rk10'
      labelstr='apsidal_k10'
   END IF
!
!   tag sm, secmass
!
   IF(kk == 588)THEN
      suffixstr='secmass'
      labelstr='M\d2\u (solar)'
   END IF
!
!   tag sr, secrad
!
   IF(kk == 593)THEN
      suffixstr='secrad'
      labelstr='R\d2\u (solar)'
   END IF
!
!   tag t6, t6 (Teff6)
!
   IF(kk == 1629)THEN
      suffixstr='Teff6'
      labelstr='Teff6 (K)'
   END IF
!
!   tag t7, t7 (Teff7)
!
   IF(kk == 1630)THEN
      suffixstr='Teff7'
      labelstr='Teff7 (K)'
   END IF
!
!   tag t8, t8 (Teff8)
!
   IF(kk == 1631)THEN
      suffixstr='Teff8'
      labelstr='Teff8 (K)'
   END IF
!
!   tag t9, t9 (Teff9)
!
   IF(kk == 1632)THEN
      suffixstr='Teff9'
      labelstr='Teff9 (K)'
   END IF
!
!   tag ta, t10 (Teff10)
!
   IF(kk == 608)THEN
      suffixstr='Teff10'
      labelstr='Teff10 (K)'
   END IF
!
!  tag 61, flux star 6, band 1
!
   IF(kk == 4688)THEN
      suffixstr='flux6_U'
      labelstr='flux6_U'
   END IF
!
!  tag 62, flux star 6, band 2
!
   IF(kk == 4689)THEN
      suffixstr='flux6_B'
      labelstr='flux6_B'
   END IF
!
!  tag 63, flux star 6, band 3
!
   IF(kk == 4690)THEN
      suffixstr='flux6_V'
      labelstr='flux6_V'
   END IF
!
!  tag 64, flux star 6, band 4
!
   IF(kk == 4691)THEN
      suffixstr='flux6_R'
      labelstr='flux6_R'
   END IF
!
!  tag 65, flux star 6, band 5
!
   IF(kk == 4692)THEN
      suffixstr='flux6_I'
      labelstr='flux6_I'
   END IF
!
!  tag 66, flux star 6, band 6
!
   IF(kk == 4693)THEN
      suffixstr='flux6_J'
      labelstr='flux6_J'
   END IF
!
!  tag 67, flux star 6, band 7
!
   IF(kk == 4694)THEN
      suffixstr='flux6_H'
      labelstr='flux6_H'
   END IF
!
!  tag 68, flux star 6, band 8
!
   IF(kk == 4695)THEN
      suffixstr='flux6_K'
      labelstr='flux6_K'
   END IF
!
!  tag 71, flux star 7, band 1
!
   IF(kk == 4720)THEN
      suffixstr='flux7_U'
      labelstr='flux7_U'
   END IF
!
!  tag 72, flux star 7, band 2
!
   IF(kk == 4721)THEN
      suffixstr='flux7_B'
      labelstr='flux7_B'
   END IF
!
!  tag 73, flux star 7, band 3
!
   IF(kk == 4722)THEN
      suffixstr='flux7_V'
      labelstr='flux7_V'
   END IF
!
!  tag 74, flux star 7, band 4
!
   IF(kk == 4723)THEN
      suffixstr='flux7_R'
      labelstr='flux7_R'
   END IF
!
!  tag 75, flux star 7, band 5
!
   IF(kk == 4724)THEN
      suffixstr='flux7_I'
      labelstr='flux7_I'
   END IF
!
!  tag 76, flux star 7, band 6
!
   IF(kk == 4725)THEN
      suffixstr='flux7_J'
      labelstr='flux7_J'
   END IF
!
!  tag 77, flux star 7, band 7
!
   IF(kk == 4726)THEN
      suffixstr='flux7_H'
      labelstr='flux7_H'
   END IF
!
!  tag 78, flux star 7, band 8
!
   IF(kk == 4727)THEN
      suffixstr='flux7_K'
      labelstr='flux7_K'
   END IF
!
!  tag 81, flux star 8, band 1
!
   IF(kk == 4752)THEN
      suffixstr='flux8_U'
      labelstr='flux8_U'
   END IF
!
!  tag 82, flux star 8, band 2
!
   IF(kk == 4753)THEN
      suffixstr='flux8_B'
      labelstr='flux8_B'
   END IF
!
!  tag 83, flux star 8, band 3
!
   IF(kk == 4754)THEN
      suffixstr='flux8_V'
      labelstr='flux8_V'
   END IF
!
!  tag 84, flux star 8, band 4
!
   IF(kk == 4755)THEN
      suffixstr='flux8_R'
      labelstr='flux8_R'
   END IF
!
!  tag 85, flux star 8, band 5
!
   IF(kk == 4756)THEN
      suffixstr='flux8_I'
      labelstr='flux8_I'
   END IF
!
!  tag 86, flux star 8, band 6
!
   IF(kk == 4757)THEN
      suffixstr='flux8_J'
      labelstr='flux8_J'
   END IF
!
!  tag 87, flux star 8, band 7
!
   IF(kk == 4758)THEN
      suffixstr='flux8_H'
      labelstr='flux8_H'
   END IF
!
!  tag 88, flux star 8, band 8
!
   IF(kk == 4759)THEN
      suffixstr='flux8_K'
      labelstr='flux8_K'
   END IF
!
!  tag 91, flux star 9, band 1
!
   IF(kk == 2784)THEN
      suffixstr='flux9_U'
      labelstr='flux9_U'
   END IF
!
!  tag 92, flux star 9, band 2
!
   IF(kk == 2785)THEN
      suffixstr='flux9_B'
      labelstr='flux9_B'
   END IF
!
!  tag 93, flux star 9, band 3
!
   IF(kk == 2786)THEN
      suffixstr='flux9_V'
      labelstr='flux9_V'
   END IF
!
!  tag 94, flux star 9, band 4
!
   IF(kk == 2787)THEN
      suffixstr='flux9_R'
      labelstr='flux9_R'
   END IF
!
!  tag 95, flux star 9, band 5
!
   IF(kk == 2788)THEN
      suffixstr='flux9_I'
      labelstr='flux9_I'
   END IF
!
!  tag 96, flux star 9, band 6
!
   IF(kk == 2789)THEN
      suffixstr='flux9_J'
      labelstr='flux9_J'
   END IF
!
!  tag 97, flux star 9, band 7
!
   IF(kk == 2790)THEN
      suffixstr='flux9_H'
      labelstr='flux9_H'
   END IF
!
!  tag 98, flux star 9, band 8
!
   IF(kk == 2791)THEN
      suffixstr='flux9_K'
      labelstr='flux9_K'
   END IF
!
!  tag 01, flux star 10, band 1
!
   IF(kk == 4496)THEN
      suffixstr='flux10_U'
      labelstr='flux10_U'
   END IF
!
!  tag 02, flux star 10, band 2
!
   IF(kk == 4497)THEN
      suffixstr='flux10_B'
      labelstr='flux10_B'
   END IF
!
!  tag 03, flux star 10, band 3
!
   IF(kk == 4498)THEN
      suffixstr='flux10_V'
      labelstr='flux10_V'
   END IF
!
!  tag 04, flux star 10, band 4
!
   IF(kk == 4499)THEN
      suffixstr='flux10_R'
      labelstr='flux10_R'
   END IF
!
!  tag 05, flux star 10, band 5
!
   IF(kk == 4500)THEN
      suffixstr='flux10_I'
      labelstr='flux10_I'
   END IF
!
!  tag 06, flux star 10, band 6
!
   IF(kk == 4501)THEN
      suffixstr='flux10_J'
      labelstr='flux10_J'
   END IF
!
!  tag 07, flux star 10, band 7
!
   IF(kk == 4502)THEN
      suffixstr='flux10_H'
      labelstr='flux10_H'
   END IF
!
!  tag 08, flux star 10, band 8
!
   IF(kk == 4503)THEN
      suffixstr='flux10_K'
      labelstr='flux10_K'
   END IF
!
!  tag 11, flux star 1, band 1
!
   IF(kk == 4528)THEN
      suffixstr='flux1_U'
      labelstr='flux1_U'
   END IF
!
!  tag 12, flux star 1, band 2
!
   IF(kk == 4529)THEN
      suffixstr='flux1_B'
      labelstr='flux1_B'
   END IF
!
!  tag 13, flux star 1, band 3
!
   IF(kk == 4530)THEN
      suffixstr='flux1_V'
      labelstr='flux1_V'
   END IF
!
!  tag 14, flux star 1, band 4
!
   IF(kk == 4531)THEN
      suffixstr='flux1_R'
      labelstr='flux1_R'
   END IF
!
!  tag 15, flux star 1, band 5
!
   IF(kk == 4532)THEN
      suffixstr='flux1_I'
      labelstr='flux1_I'
   END IF
!
!  tag 16, flux star 1, band 6
!
   IF(kk == 4533)THEN
      suffixstr='flux1_J'
      labelstr='flux1_J'
   END IF
!
!  tag 17, flux star 1, band 7
!
   IF(kk == 4534)THEN
      suffixstr='flux1_H'
      labelstr='flux1_H'
   END IF
!
!  tag 18, flux star 1, band 8
!
   IF(kk == 4535)THEN
      suffixstr='flux1_K'
      labelstr='flux1_K'
   END IF
!
!  tag 21, flux star 2, band 1
!
   IF(kk == 4560)THEN
      suffixstr='flux2_U'
      labelstr='flux2_U'
   END IF
!
!  tag 22, flux star 2, band 2
!
   IF(kk == 4561)THEN
      suffixstr='flux2_B'
      labelstr='flux2_B'
   END IF
!
!  tag 23, flux star 2, band 3
!
   IF(kk == 4562)THEN
      suffixstr='flux2_V'
      labelstr='flux2_V'
   END IF
!
!  tag 24, flux star 2, band 4
!
   IF(kk == 4563)THEN
      suffixstr='flux2_R'
      labelstr='flux2_R'
   END IF
!
!  tag 25, flux star 2, band 5
!
   IF(kk == 4564)THEN
      suffixstr='flux2_I'
      labelstr='flux2_I'
   END IF
!
!  tag 26, flux star 2, band 6
!
   IF(kk == 4565)THEN
      suffixstr='flux2_J'
      labelstr='flux2_J'
   END IF
!
!  tag 27, flux star 2, band 7
!
   IF(kk == 4566)THEN
      suffixstr='flux2_H'
      labelstr='flux2_H'
   END IF
!
!  tag 28, flux star 2, band 8
!
   IF(kk == 4567)THEN
      suffixstr='flux2_K'
      labelstr='flux2_K'
   END IF
!
!  tag 31, flux star 3, band 1
!
   IF(kk == 4592)THEN
      suffixstr='flux3_U'
      labelstr='flux3_U'
   END IF
!
!  tag 32, flux star 3, band 2
!
   IF(kk == 4593)THEN
      suffixstr='flux3_B'
      labelstr='flux3_B'
   END IF
!
!  tag 33, flux star 3, band 3
!
   IF(kk == 4594)THEN
      suffixstr='flux3_V'
      labelstr='flux3_V'
   END IF
!
!  tag 34, flux star 3, band 4
!
   IF(kk == 4595)THEN
      suffixstr='flux3_R'
      labelstr='flux3_R'
   END IF
!
!  tag 35, flux star 3, band 5
!
   IF(kk == 4596)THEN
      suffixstr='flux3_I'
      labelstr='flux3_I'
   END IF
!
!  tag 36, flux star 3, band 6
!
   IF(kk == 4597)THEN
      suffixstr='flux3_J'
      labelstr='flux3_J'
   END IF
!
!  tag 37, flux star 3, band 7
!
   IF(kk == 4598)THEN
      suffixstr='flux3_H'
      labelstr='flux3_H'
   END IF
!
!  tag 38, flux star 3, band 8
!
   IF(kk == 4599)THEN
      suffixstr='flux3_K'
      labelstr='flux3_K'
   END IF
!
!  tag 41, flux star 4, band 1
!
   IF(kk == 4624)THEN
      suffixstr='flux4_U'
      labelstr='flux4_U'
   END IF
!
!  tag 42, flux star 4, band 2
!
   IF(kk == 4625)THEN
      suffixstr='flux4_B'
      labelstr='flux4_B'
   END IF
!
!  tag 43, flux star 4, band 3
!
   IF(kk == 4626)THEN
      suffixstr='flux4_V'
      labelstr='flux4_V'
   END IF
!
!  tag 44, flux star 4, band 4
!
   IF(kk == 4627)THEN
      suffixstr='flux4_R'
      labelstr='flux4_R'
   END IF
!
!  tag 45, flux star 4, band 5
!
   IF(kk == 4628)THEN
      suffixstr='flux4_I'
      labelstr='flux4_I'
   END IF
!
!  tag 46, flux star 4, band 6
!
   IF(kk == 4629)THEN
      suffixstr='flux4_J'
      labelstr='flux4_J'
   END IF
!
!  tag 47, flux star 4, band 7
!
   IF(kk == 4630)THEN
      suffixstr='flux4_H'
      labelstr='flux4_H'
   END IF
!
!  tag 48, flux star 4, band 8
!
   IF(kk == 4631)THEN
      suffixstr='flux4_K'
      labelstr='flux4_K'
   END IF
!
!  tag 51, flux star 5, band 1
!
   IF(kk == 4656)THEN
      suffixstr='flux5_U'
      labelstr='flux5_U'
   END IF
!
!  tag 52, flux star 5, band 2
!
   IF(kk == 4657)THEN
      suffixstr='flux5_B'
      labelstr='flux5_B'
   END IF
!
!  tag 53, flux star 5, band 3
!
   IF(kk == 4658)THEN
      suffixstr='flux5_V'
      labelstr='flux5_V'
   END IF
!
!  tag 54, flux star 5, band 4
!
   IF(kk == 4659)THEN
      suffixstr='flux5_R'
      labelstr='flux5_R'
   END IF
!
!  tag 55, flux star 5, band 5
!
   IF(kk == 4660)THEN
      suffixstr='flux5_I'
      labelstr='flux5_I'
   END IF
!
!  tag 56, flux star 6, band 6
!
   IF(kk == 4661)THEN
      suffixstr='flux5_J'
      labelstr='flux5_J'
   END IF
!
!  tag 57, flux star 5, band 7
!
   IF(kk == 4662)THEN
      suffixstr='flux5_H'
      labelstr='flux5_H'
   END IF
!
!  tag 58, flux star 5, band 8
!
   IF(kk == 4663)THEN
      suffixstr='flux5_K'
      labelstr='flux5_K'
   END IF
!
!  tag ob, Omega_bin, nodal angle of binary
!
   IF(kk == 449)THEN
      suffixstr='Omega_bin'
      labelstr='Omega_bin (deg)'
   END IF
!
!  tag oc, ocose, e*cos(omega) for binary
!
   IF(kk == 450)THEN
      suffixstr='ecos'
      labelstr='e*cos(omega)'
   END IF
!
!  tag os, osine, e*sin(omega) for binary
!
   IF(kk == 466)THEN
      suffixstr='esin'
      labelstr='e*sin(omega)'
   END IF
!
!  tag a1, rk1, tidal apsidal constant for star 1
!
   IF(kk == 1016)THEN
      suffixstr='rk1'
      labelstr='apsidal_k1'
   END IF
!
!  tag a2, rk2, tidal apsidal constant for star 2
!
   IF(kk == 1017)THEN
      suffixstr='rk2'
      labelstr='apsidal_k2'
   END IF
!
!  planet 2 parameters
!
!  tag uj, P2tconj
!
   IF(kk == 649)THEN
      suffixstr='P2tconj'
      labelstr='P2tconj'
   END IF
!
!  tag ut, P2period
!
   IF(kk == 659)THEN
      suffixstr='P2period'
      labelstr='P2period (days)'
   END IF
!
!  tag uu, P2T0
!
   IF(kk == 660)THEN
      suffixstr='P2T0'
      labelstr='P2T0'
   END IF
!
!  tag uv, P2ecos
!
   IF(kk == 661)THEN
      suffixstr='P2ecos'
      labelstr='e2*cos(omega2)'
   END IF
!
!  tag uw, P2esin
!
   IF(kk == 662)THEN
      suffixstr='P2esin'
      labelstr='e2*sin(omega2)'
   END IF
!
!  tag ux, P2incl
!
   IF(kk == 663)THEN
      suffixstr='P2incl'
      labelstr='P2incl (deg)'
   END IF
!
!  tag uy, P2Omega
!
   IF(kk == 664)THEN
      suffixstr='P2Omega'
      labelstr='P2Omega (deg)'
   END IF
!
!  tag uz, P2Q
!
   IF(kk == 665)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P2Q'
      IF(it1 == 0)labelstr='P2Q [(M\d1\u+M\d2\u)/M\d4\u]'
      IF(it1 == 1)labelstr='log(P2Q) [(M\d1\u+M\d2\u)/M\d4\u]'
      IF(it1 == 2)labelstr= 'P2Q (Mass of body 4 in Earth masses)'
   END IF
!
!  tag ub, P2ratrad
!
   IF(kk == 641)THEN
      suffixstr='P2ratrad'
      labelstr='P2ratrad (R\d1\u/R\d4\u)'
   END IF
!
!  planet 3 parameters
!
!  tag vj, P3tconj
!
   IF(kk == 681)THEN
      suffixstr='P3tconj'
      labelstr='P3tconj'
   END IF
!
!  tag vt, P3period
!
   IF(kk == 691)THEN
      suffixstr='P3period'
      labelstr='P3period (days)'
   END IF
!
!  tag vu, P3T0
!
   IF(kk == 692)THEN
      suffixstr='P3T0'
      labelstr='P3T0'
   END IF
!
!  tag vv, P3ecos
!
   IF(kk == 693)THEN
      suffixstr='P3ecos'
      labelstr='e3*cos(omega3)'
   END IF
!
!  tag vw, P3esin
!
   IF(kk == 694)THEN
      suffixstr='P3esin'
      labelstr='e3*sin(omega3)'
   END IF
!
!  tag vx, P3incl
!
   IF(kk == 695)THEN
      suffixstr='P3incl'
      labelstr='P3incl (deg)'
   END IF
!
!  tag vy, P3Omega
!
   IF(kk == 696)THEN
      suffixstr='P3Omega'
      labelstr='P3Omega (deg)'
   END IF
!
!  tag vz, P3Q
!
   IF(kk == 697)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P3Q'
      IF(it1 == 0)labelstr='P3Q [(M\d1\u+M\d2\u)/M\d5\u]'
      IF(it1 == 1)labelstr='log(P3Q) [(M\d1\u+M\d2\u)/M\d5\u]'
      IF(it1 == 2)labelstr= 'P3Q (Mass of body 5 in Earth masses)'
   END IF
!
!  tag vb, P3ratrad
!
   IF(kk == 673)THEN
      suffixstr='P3ratrad'
      labelstr='P3ratrad (R\d1\u/R\d5\u)'
   END IF
!
!  planet 4 parameters
!
!  tag wj, P4tconj
!
   IF(kk == 713)THEN
      suffixstr='P4tconj'
      labelstr='P4tconj'
   END IF
!
!  tag wt, P4period
!
   IF(kk == 723)THEN
      suffixstr='P4period'
      labelstr='P4period (days)'
   END IF
!
!  tag wu, P4T0
!
   IF(kk == 724)THEN
      suffixstr='P4T0'
      labelstr='P4T0'
   END IF
!
!  tag wv, P4ecos
!
   IF(kk == 725)THEN
      suffixstr='P4ecos'
      labelstr='e4*cos(omega4)'
   END IF
!
!  tag ww, P4esin
!
   IF(kk == 726)THEN
      suffixstr='P4esin'
      labelstr='e4*sin(omega4)'
   END IF
!
!  tag wx, P4incl
!
   IF(kk == 727)THEN
      suffixstr='P4incl'
      labelstr='P4incl (deg)'
   END IF
!
!  tag wy, P4Omega
!
   IF(kk == 728)THEN
      suffixstr='P4Omega'
      labelstr='P4Omega (deg)'
   END IF
!
!  tag wz, P4Q
!
   IF(kk == 729)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P4Q'
      IF(it1 == 0)labelstr='P4Q [(M\d1\u+M\d2\u)/M\d6\u]'
      IF(it1 == 1)labelstr='log(P4Q) [(M\d1\u+M\d2\u)/M\d6\u]'
      IF(it1 == 2)labelstr= 'P4Q (Mass of body 6 in Earth masses)'
   END IF
!
!  tag wb, P4ratrad
!
   IF(kk == 705)THEN
      suffixstr='P4ratrad'
      labelstr='P4ratrad (R\d1\u/R\d6\u)'
   END IF
!
!  planet 5 parameters
!
!  tag xj, P5tconj
!
   IF(kk == 745)THEN
      suffixstr='P5tconj'
      labelstr='P5tconj'
   END IF
!
!  tag xt, P5period
!
   IF(kk == 755)THEN
      suffixstr='P5period'
      labelstr='P5period (days)'
   END IF
!
!  tag xu, P5T0
!
   IF(kk == 756)THEN
      suffixstr='P5T0'
      labelstr='P5T0'
   END IF
!
!  tag xv, P5ecos
!
   IF(kk == 757)THEN
      suffixstr='P5ecos'
      labelstr='e5*cos(omega5)'
   END IF
!
!  tag xw, P5esin
!
   IF(kk == 758)THEN
      suffixstr='P5esin'
      labelstr='e5*sin(omega5)'
   END IF
!
!  tag xx, P5incl
!
   IF(kk == 759)THEN
      suffixstr='P5incl'
      labelstr='P5incl (deg)'
   END IF
!
!  tag xy, P5Omega
!
   IF(kk == 760)THEN
      suffixstr='P5Omega'
      labelstr='P5Omega (deg)'
   END IF
!
!  tag xz, P5Q
!
   IF(kk == 761)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P5Q'
      IF(it1 == 0)labelstr='P5Q [(M\d1\u+M\d2\u)/M\d7\u]'
      IF(it1 == 1)labelstr='log(P5Q) [(M\d1\u+M\d2\u)/M\d7\u]'
      IF(it1 == 2)labelstr= 'P5Q (Mass of body 7 in Earth masses)'
   END IF
!
!  tag xb, P5ratrad
!
   IF(kk == 737)THEN
      suffixstr='P5ratrad'
      labelstr='P5ratrad (R\d1\u/R\d7\u)'
   END IF
!
!  planet 6 parameters
!
!  tag sj, P6tconj
!
   IF(kk == 585)THEN
      suffixstr='P6tconj'
      labelstr='P6tconj'
   END IF
!
!  tag st, P6period
!
   IF(kk == 595)THEN
      suffixstr='P6period'
      labelstr='P6period (days)'
   END IF
!
!  tag su, P6T0
!
   IF(kk == 596)THEN
      suffixstr='P6T0'
      labelstr='P6T0'
   END IF
!
!  tag sv, P6ecos
!
   IF(kk == 597)THEN
      suffixstr='P6ecos'
      labelstr='e6*cos(omega6)'
   END IF
!
!  tag sw, P6esin
!
   IF(kk == 598)THEN
      suffixstr='P6esin'
      labelstr='e6*sin(omega6)'
   END IF
!
!  tag sx, P6incl
!
   IF(kk == 599)THEN
      suffixstr='P6incl'
      labelstr='P6incl (deg)'
   END IF
!
!  tag sy, P6Omega
!
   IF(kk == 600)THEN
      suffixstr='P6Omega'
      labelstr='P6Omega (deg)'
   END IF
!
!  tag sz, P6Q
!
   IF(kk == 601)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P6Q'
      IF(it1 == 0)labelstr='P6Q [(M\d1\u+M\d2\u)/M\d8\u]'
      IF(it1 == 1)labelstr='log(P6Q) [(M\d1\u+M\d2\u)/M\d8\u]'
      IF(it1 == 2)labelstr= 'P6Q (Mass of body 8 in Earth masses)'
   END IF
!
!  tag sb, P6ratrad
!
   IF(kk == 577)THEN
      suffixstr='P6ratrad'
      labelstr='P6ratrad (R\d1\u/R\d8\u)'
   END IF
!
!  planet 7 parameters
!
!  tag hj, P7tconj
!
   IF(kk == 233)THEN
      suffixstr='P7tconj'
      labelstr='P7tconj'
   END IF
!
!  tag ht, P7period
!
   IF(kk == 243)THEN
      suffixstr='P7period'
      labelstr='P7period (days)'
   END IF
!
!  tag hu, P7T0
!
   IF(kk == 244)THEN
      suffixstr='P7T0'
      labelstr='P7T0'
   END IF
!
!  tag hv, P7ecos
!
   IF(kk == 245)THEN
      suffixstr='P7ecos'
      labelstr='e7*cos(omega7)'
   END IF
!
!  tag hw, P7esin
!
   IF(kk == 246)THEN
      suffixstr='P7esin'
      labelstr='e7*sin(omega7)'
   END IF
!
!  tag hx, P7incl
!
   IF(kk == 247)THEN
      suffixstr='P7incl'
      labelstr='P7incl (deg)'
   END IF
!
!  tag hy, P7Omega
!
   IF(kk == 248)THEN
      suffixstr='P7Omega'
      labelstr='P7Omega (deg)'
   END IF
!
!  tag hz, P7Q
!
   IF(kk == 249)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P7Q'
      IF(it1 == 0)labelstr='P7Q [(M\d1\u+M\d2\u)/M\d9\u]'
      IF(it1 == 1)labelstr='log(P7Q) [(M\d1\u+M\d2\u)/M\d9\u]'
      IF(it1 == 2)labelstr= 'P7Q (Mass of body 9 in Earth masses)'
   END IF
!
!  tag hb, P7ratrad
!
   IF(kk == 225)THEN
      suffixstr='P7ratrad'
      labelstr='P7ratrad (R\d1\u/R\d9\u)'
   END IF
!
!  planet 8 parameters
!
!  tag kj, P8tconj
!
   IF(kk == 329)THEN
      suffixstr='P8tconj'
      labelstr='P8tconj'
   END IF
!
!  tag kt, P8period
!
   IF(kk == 339)THEN
      suffixstr='P8period'
      labelstr='P8period (days)'
   END IF
!
!  tag ku, P8T0
!
   IF(kk == 340)THEN
      suffixstr='P8T0'
      labelstr='P8T0'
   END IF
!
!  tag kv, P8ecos
!
   IF(kk == 341)THEN
      suffixstr='P8ecos'
      labelstr='e8*cos(omega8)'
   END IF
!
!  tag kw, P8esin
!
   IF(kk == 342)THEN
      suffixstr='P8esin'
      labelstr='e8*sin(omega8)'
   END IF
!
!  tag kx, P8incl
!
   IF(kk == 343)THEN
      suffixstr='P8incl'
      labelstr='P8incl (deg)'
   END IF
!
!  tag ky, P8Omega
!
   IF(kk == 344)THEN
      suffixstr='P8Omega'
      labelstr='P8Omega (deg)'
   END IF
!
!  tag kz, P8Q
!
   IF(kk == 345)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P8Q'
      IF(it1 == 0)labelstr='P8Q [(M\d1\u+M\d2\u)/M\d10\u]'
      IF(it1 == 1)labelstr='log(P8Q) [(M\d1\u+M\d2\u)/M\d10\u]'
      IF(it1 == 2)labelstr= 'P8Q (Mass of body 10 in Earth masses)'
   END IF
!
!  tag kb, P8ratrad
!
   IF(kk == 321)THEN
      suffixstr='P8ratrad'
      labelstr='P8ratrad (R\d1\u/R\d10\u)'
   END IF
!
!  tag g1, Tgrav1, gravity darkening exponent star 1
!
   IF(kk == 1208)THEN
      suffixstr='Tgrav1'
      labelstr='Tgrav1'
   END IF
!
!  tag g2, Tgrav2, gravity darkening exponent star 2
!
   IF(kk == 1209)THEN
      suffixstr='Tgrav2'
      labelstr='Tgrav2'
   END IF
!
!  tag s0, contamS0, contamination season 0
!
   IF(kk == 1591)THEN
      suffixstr='contamS0'
      labelstr='contamS0'
   END IF
!
!  tag s1, contamS1, contamination season 1
!
   IF(kk == 1592)THEN
      suffixstr='contamS1'
      labelstr='contamS1'
   END IF
!
!  tag s2, contamS2, contamination season 2
!
   IF(kk == 1593)THEN
      suffixstr='contamS2'
      labelstr='contamS2'
   END IF
!
!  tag s3, contamS3, contamination season 3
!
   IF(kk == 1594)THEN
      suffixstr='contamS3'
      labelstr='contamS3'
   END IF
!
!  tag do, omegadot
!
   IF(kk == 110)THEN
      suffixstr='omega_dot'
      labelstr='omega_dot (deg/sec)'
   END IF
!
!  tag tt, tertperiod, orbital period of body 3
!
   IF(kk == 627)THEN
      suffixstr='P1period'
      labelstr='P1period (days)'
   END IF
!
!  tag tu, tertT0, periastron passage of body 3 orbit
!
   IF(kk == 628)THEN
      suffixstr='P1T0'
      labelstr='P1T0'
   END IF
!
!  tag tj, tertconj, time of primary conjunction of body 3 orbit
!
   IF(kk == 617)THEN
      suffixstr='P1tconj'
      labelstr='P1tconj'
   END IF
!
!  tag tv, tertecos, e*cos(omega) for body 3 orbit
!
   IF(kk == 629)THEN
      suffixstr='P1ecos'
      labelstr='e1*cos(omega1)'
   END IF
!
!  tag tw, tertesin, e*sin(omega) for body 3 orbit
!
   IF(kk == 630)THEN
      suffixstr='P1esin'
      labelstr='e1*sin(omega1)'
   END IF
!
!  tag tx, tertincl, inclination of body 3 orbit
!
   IF(kk == 631)THEN
      suffixstr='P1incl'
      labelstr='P1incl (deg)'
   END IF
!
!  tag ty, tertOmega, nodal angle of body 3 orbit
!
   IF(kk == 632)THEN
      suffixstr='P1Omega'
      labelstr='P1Omega (deg)'
   END IF
!
!  tag tz, tertQ, mass ratio of body 3
!
   IF(kk == 633)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P1Q'
      IF(it1 == 0)labelstr='P1Q [(M\d1\u+M\d2\u)/M\d3\u]'
      IF(it1 == 1)labelstr='log(P1Q) [(M\d1\u+M\d2\u)/M\d3\u]'
      IF(it1 == 2)labelstr= 'P1Q (Mass of body 3 in Earth masses)'
   END IF
!
!  tag tb, tertratrad, radius ratio of body 3
!
   IF(kk == 609)THEN
      suffixstr='P1ratrad'
      labelstr='P1ratrad (R\d1\u/R\d3\u)'
   END IF
!
!  tag co, contam, Kepler contamination
!
   IF(kk == 78)THEN
      suffixstr='contam'
      labelstr='Kepler contamination'
   END IF
!
!  tag e1, beam1, Doppler boosting coefficient for star 1
!
   IF(kk == 1144)THEN
      suffixstr='beam1'
      labelstr='beam1'
   END IF
!
!  tag e2, beam2, Doppler boosting coefficient for star 2
!
   IF(kk == 1145)THEN
      suffixstr='beam2'
      labelstr='beam2'
   END IF
!
!  tag pm, primmass, mass of star 1 in solar masses
!
   IF(kk == 492)THEN
      suffixstr='primmass'
      labelstr='M\d1\u (solar)'
   END IF
!
!  tag pr, primrad, radius of star in solar radii
!
   IF(kk == 497)THEN
      suffixstr='primrad'
      labelstr='R\d1\u (solar)'
   END IF
!
!  tag te, temprat, ratio of star 2 temperature to star 1 temperature
!
   IF(kk == 612)THEN
      suffixstr='temprat'
      labelstr='T\d2\u/T\d1\u'
   END IF
!
!  tag pk, primK, K-velocity of star 1 in km/sec
!
   IF(kk == 490)THEN
      suffixstr='primK'
      labelstr='K\d1\u (km/sec)'
   END IF
!
!  tag ra, ratrad, ratio of star 1 radius to star 2 radius
!
   IF(kk == 544)THEN
      suffixstr='ratrad'
      labelstr='R\d1\u/R\d2\u'
   END IF
!
!  tag q1, frac1, fractional radius of star 1 (R_1/a)
!
   IF(kk == 1528)THEN
      suffixstr='frac1'
      labelstr='frac1 (R\d1\u/a)'
   END IF
!
!  tag q2, frac2, fractional radius of star 2 (R_2/a)
!
   IF(kk == 1529)THEN
      suffixstr='frac2'
      labelstr='frac2 (R\d2\u/a)'
   END IF
!
!  tag de, density, density of star 1 in cgs
!
   IF(kk == 100)THEN
      suffixstr='density'
      labelstr='density1 (g/cc)'
   END IF
!
!  tag l1, alb1, albedo of star 1
!
   IF(kk == 1368)THEN
      suffixstr='al1'
      labelstr='albedo1'
   END IF
!
!  tag l2, alb2, albedo of star 2
!
   IF(kk == 1369)THEN
      suffixstr='al2'
      labelstr='albedo2'
   END IF
!
!  tag dp, dphi, phase difference between secondary and primary eclipse
!
   IF(kk == 111)THEN
      suffixstr='dphi'
      labelstr='dphi'
   END IF
!
!  tag tc, Tconj, time of primary conjunction of the binary orbit
!
   IF(kk == 610)THEN
      suffixstr='Tconj'
      labelstr='Tconj'
   END IF
!
!  tag t0, T0, time of periastron passage of binary orbit
!
   IF(kk == 1623)THEN
      suffixstr='T0'
      labelstr='T0'
   END IF
!
!  tag p2, period, binary orbital period in days
!
   IF(kk == 484)THEN
      suffixstr='P'
      labelstr='binary period (days)'
   END IF
!
!  tag in, finc, inclination of binary in degrees
!
   IF(kk == 269)THEN
      suffixstr='i'
      labelstr='binary inclination (deg)'
   END IF
!
!  tag ai, bigI, inclination of star 1's spin axis
!
   IF(kk == 8)THEN
      suffixstr='bigI'
      labelstr='bigI (deg)'
   END IF
!
!  tag ab, bigbeta, position angle of star 1's spin axis
!
   IF(kk == 1)THEN
      suffixstr='bigbeta'
      labelstr='bigbeta (deg)'
   END IF
!
!  tag ma, Q, mass ratio of binary
!
   IF(kk == 384)THEN
      suffixstr='Q'
      labelstr='Q (M\d2\u/M\d1\u)'
   END IF
!
!  tag f1, fill1, Roche lobe filling factor of star 1
!
   IF(kk == 1176)THEN
      suffixstr='fill1'
      labelstr='fill1'
   END IF
!
!  tag f2, fill2, Roche lobe filling factor of star 2
!
   IF(kk == 1177)THEN
      suffixstr='fill2'
      labelstr='fill2'
   END IF
!
!  tag ri, rinner, inner radius of accretion disk
!
   IF(kk == 552)THEN
      suffixstr='rin'
      labelstr='rinner'
   END IF
!
!  tag o1, omega1, spin frequency of star 1
!
   IF(kk == 1464)THEN
      suffixstr='omega1'
      labelstr='omega1'
   END IF
!
!  tag o2, spin frequency of star 2
!
   IF(kk == 1465)THEN
      suffixstr='omega2'
      labelstr='omega2'
   END IF
!
!  tag ro, router, radius of outer edge of accretion disk
!
   IF(kk == 558)THEN
      suffixstr='rout'
      labelstr='r_outer'
   END IF
!
!  tag t3, Teff3, effective temperature of star 3
!
   IF(kk == 1626)THEN
      suffixstr='Teff3'
      labelstr='Teff3 (K)'
   END IF
!
!  tag t4, Teff4, effective temperature of star 4
!
   IF(kk == 1627)THEN
      suffixstr='Teff4'
      labelstr='Teff4 (K)'
   END IF
!
!  tag t5, Teff5, effective temperature of star 5
!
   IF(kk == 1628)THEN
      suffixstr='Teff5'
      labelstr='Teff5 (K)'
   END IF
!
!  tag g3, g3, gravity of star 3 (log(g))
!
   IF(kk == 1210)THEN
      suffixstr='g3'
      labelstr='log(g3)'
   END IF
!
!  tag g4, g4, gravity of star 4 (log(g))
!
   IF(kk == 1210)THEN
      suffixstr='g4'
      labelstr='log(g4)'
   END IF
!
!  tag g5, g5, gravity of star 5 (log(g))
!
   IF(kk == 1210)THEN
      suffixstr='g5'
      labelstr='log(g5)'
   END IF
!
!  tag be, betarim, opening angle of accretion disk rim
!
   IF(kk == 36)THEN
      suffixstr='beta_rim'
      labelstr='beta_rim (deg)'
   END IF
!
!  tag t1, Teff1, effective temperature of star 1
!
   IF(kk == 1624)THEN
      suffixstr='Teff1'
      labelstr='Teff1 (K)'
   END IF
!
!  tag t2, Teff2, effective temperature of star 2
!
   IF(kk == 1625)THEN
      suffixstr='Teff2'
      labelstr='Teff2 (K)'
   END IF
!
!  tag xi, xi, power-law exponent of the disk temperature profile
!
   IF(kk == 744)THEN
      suffixstr='xi'
      labelstr='xi'
   END IF
!
!  tag td, Tdisk, temperature of the inner edge of the accretion disk
!
   IF(kk == 611)THEN
      suffixstr='Tdisk'
      labelstr='Tdisk'
   END IF
!
!  tag lx, rLx, log of the X-ray luminosity in cgs units
!
   IF(kk == 375)THEN
      suffixstr='Lx'
      labelstr='log(Lx) (erg/sec)'
   END IF
!
!  tag se, separ, semimajor axis of binary orbit
!
   IF(kk == 580)THEN
      suffixstr='separ'
      labelstr='separation (solar)'
   END IF
!
!  tag sa, SA3, ratio of area of star 3 to area of star 1
!
   IF(kk == 576)THEN
      suffixstr='SA3'
      labelstr='SA3 (R\d3\u/R\d1\u)\u2\d'
   END IF
!
!  tag ps, pshift
!
   IF(kk == 498)THEN
      suffixstr='pshift'
      labelstr='pshift'
   END IF
!
!  tag ec, ecc, eccentricity of binary orbit
!
   IF(kk == 130)THEN
      suffixstr='ecc'
      labelstr='binary eccentricity'
   END IF
!
!  tag ar, argper, argument of periastron of binary orbit
!
   IF(kk == 17)THEN
      suffixstr='argper'
      labelstr='binary arg_per (deg)'
   END IF
!
!  tag b1, temperature factor spot 1, star 1
!
   IF(kk == 1048)THEN
      suffixstr='TF_spot1_star1'
      labelstr='TF_spot1_star1'
   END IF
!
!  tag b2, latitude spot 1, star 1
!
   IF(kk == 1049)THEN
      suffixstr='lat_spot1_star1'
      labelstr='lat_spot1_star1 (deg)'
   END IF
!
!  tag b3, longitude spot 1, star 1
!
   IF(kk == 1050)THEN
      suffixstr='lon_spot1_star1'
      labelstr='lon_spot1_star1 (deg)'
   END IF
!
!  tag  b4, radius spot 1, star 1
!
   IF(kk == 1051)THEN
      suffixstr='rad_spot1_star1'
      labelstr='rad_spot1_star1 (deg)'
   END IF
!
!  tag b5, temperature factor spot 2, star 1
!
   IF(kk == 1052)THEN
      suffixstr='TF_spot2_star1'
      labelstr='TF_spot2_star1'
   END IF
!
!  tag b6,  latitude spot 2, star 1
!
   IF(kk == 1053)THEN
      suffixstr='lat_spot2_star1'
      labelstr='lat_spot2_star1 (deg)'
   END IF
!
!  tag b7, longitude spot 2, star 1
!
   IF(kk == 1054)THEN
      suffixstr='lon_spot2_star1'
      labelstr='lon_spot2_star1 (deg)'
   END IF
!
!  tag b8, radius spot 2, star 1
!
   IF(kk == 1055)THEN
      suffixstr='rad_spot2_star1'
      labelstr='rad_spot2_star1 (deg)'
   END IF
!
!  tag c1, temperature factor spot 1, star 2
!
   IF(kk == 1080)THEN
      suffixstr='TF_spot1_star2'
      labelstr='TF_spot1_star2'
   END IF
!
!  tag c2, latitude spot 1, star 2
!
   IF(kk == 1081)THEN
      suffixstr='lat_spot1_star2'
      labelstr='lat_spot1_star2 (deg)'
   END IF
!
!  tag c3, longitude spot 1, star 2
!
   IF(kk == 1082)THEN
      suffixstr='lon_spot1_star2'
      labelstr='lon_spot1_star2 (deg)'
   END IF
!
!  tag c4, radius spot 1, star 2
!
   IF(kk == 1083)THEN
      suffixstr='rad_spot1_star2'
      labelstr='rad_spot1_star2 (deg)'
   END IF
!
!  tag c5,  temperature factor spot 2, star 2
!
   IF(kk == 1084)THEN
      suffixstr='TF_spot2_star2'
      labelstr='TF_spot2_star2'
   END IF
!
!  tag c6, latitude spot 2, star 2
!
   IF(kk == 1085)THEN
      suffixstr='lat_spot2_star2'
      labelstr='lat_spot2_star2 (deg)'
   END IF
!
!  tag c7, longitude spot 2, star 2
!
   IF(kk == 1086)THEN
      suffixstr='lon_spot2_star2'
      labelstr='lon_spot2_star2 (deg)'
   END IF
!
!  tag c8, radius spot 2, star 2
!
   IF(kk == 1087)THEN
      suffixstr='rad_spot2_star2'
      labelstr='rad_spot2_star2 (deg)'
   END IF
!
!  tag d1, temperature factor spot 1 on disk
!
   IF(kk == 1112)THEN
      suffixstr='TF_spot1_disk'
      labelstr='TF_spot1_disk'
   END IF
!
!  tag d2, azimuth spot 1 on disk
!
   IF(kk == 1113)THEN
      suffixstr='azi_spot1_disk'
      labelstr='azi_spot1_disk (deg)'
   END IF
!
!  tag d3, cutoff radius for spot 1 on disk
!
   IF(kk == 1114)THEN
      suffixstr='cut_spot1_disk'
      labelstr='cut_spot1_disk'
   END IF
!
!  tag d4, angular width of spot 1 on disk
!
   IF(kk == 1115)THEN
      suffixstr='wid_spot1_disk'
      labelstr='wid_spot1_disk (deg)'
   END IF
!
!  tag d5, temperature factor spot 2 on disk
!
   IF(kk == 1116)THEN
      suffixstr='TF_spot2_disk'
      labelstr='TF_spot2_disk'
   END IF
!
!  tag d6, azimuth spot 2 on disk
!
   IF(kk == 1117)THEN
      suffixstr='azi_spot2_disk'
      labelstr='azi_spot2_disk (deg)'
   END IF
!
!  tag d7, cutoff radius for spot 2 on disk
!
   IF(kk == 1118)THEN
      suffixstr='cut_spot2_disk'
      labelstr='cut_spot2_disk'
   END IF
!
!  tag d8, angular width of spot 2 on disk
!
   IF(kk == 1119)THEN
      suffixstr='wid_spot2_disk'
      labelstr='wid_spot2_disk (deg)'
   END IF
!
!  tag x1, limb darkening x-coefficient, band 1, star 1
!
   IF(kk == 1752)THEN
      suffixstr='x1_U'
      labelstr='x1_U'
   END IF
!
!  tag x2, limb darkening x-coefficient, band 2, star 1
!
   IF(kk == 1753)THEN
      suffixstr='x1_B'
      labelstr='x1_B'
   END IF
!
!  tag x3, limb darkening x-coefficient, band 3, star 1
!
   IF(kk == 1754)THEN
      suffixstr='x1_V'
      labelstr='x1_V'
   END IF
!
!  tag x4, limb darkening x-coefficient, band 4, star 1
!
   IF(kk == 1755)THEN
      suffixstr='x1_R'
      labelstr='x1_R'
   END IF
!
!  tag x5, limb darkening x-coefficient, band 5, star 1
!
   IF(kk == 1756)THEN
      suffixstr='x1_I'
      labelstr='x1_I'
   END IF
!
!  tag x6, limb darkening x-coefficient, band 6, star 1
!
   IF(kk == 1757)THEN
      suffixstr='x1_J'
      labelstr='x1_J'
   END IF
!
!  tag x7, limb darkening x-coefficient, band 7, star 1
!
   IF(kk == 1758)THEN
      suffixstr='x1_H'
      labelstr='x1_H'
   END IF
!
!  tag x8, limb darkening x-coefficient, band 8, star 1
!
   IF(kk == 1759)THEN
      suffixstr='x1_K'
      labelstr='x1_K'
   END IF
!
!  tag z1, limb darkening x-coefficient, band 1, star 2
!
   IF(kk == 1816)THEN
      suffixstr='x2_U'
      labelstr='x2_U'
   END IF
!
!  tag z2, limb darkening x-coefficient, band 2, star 2
!
   IF(kk == 1817)THEN
      suffixstr='x2_B'
      labelstr='x2_B'
   END IF
!
!  tag z3, limb darkening x-coefficient, band 3, star 2
!
   IF(kk == 1818)THEN
      suffixstr='x2_V'
      labelstr='x2_V'
   END IF
!
!  tag z4, limb darkening x-coefficient, band 4, star 2
!
   IF(kk == 1819)THEN
      suffixstr='x2_R'
      labelstr='x2_R'
   END IF
!
!  tag z5, limb darkening x-coefficient, band 5, star 2
!
   IF(kk == 1820)THEN
      suffixstr='x2_I'
      labelstr='x2_I'
   END IF
!
!  tag z6, limb darkening x-coefficient, band 6, star 2
!
   IF(kk == 1821)THEN
      suffixstr='x2_J'
      labelstr='x2_J'
   END IF
!
!  tag z7, limb darkening x-coefficient, band 7, star 2
!
   IF(kk == 1822)THEN
      suffixstr='x2_H'
      labelstr='x2_H'
   END IF
!
!  tag z8, limb darkening x-coefficient, band 8, star 2
!
   IF(kk == 1823)THEN
      suffixstr='x2_K'
      labelstr='x2_K'
   END IF
!
!  tag y1, limb darkening y-coefficient, band 1, star 1
!
   IF(kk == 1784)THEN
      suffixstr='y1_U'
      labelstr='y1_U'
   END IF
!
!  tag y2, limb darkening y-coefficient, band 2, star 1
!
   IF(kk == 1785)THEN
      suffixstr='y1_B'
      labelstr='y1_B'
   END IF
!
!  tag y3, limb darkening y-coefficient, band 3, star 1
!
   IF(kk == 1786)THEN
      suffixstr='y1_V'
      labelstr='y1_V'
   END IF
!
!  tag y4, limb darkening y-coefficient, band 4, star 1
!
   IF(kk == 1787)THEN
      suffixstr='y1_R'
      labelstr='y1_R'
   END IF
!
!  tag y5, limb darkening y-coefficient, band 5, star 1
!
   IF(kk == 1788)THEN
      suffixstr='y1_I'
      labelstr='y1_I'
   END IF
!
!  tag y6, limb darkening y-coefficient, band 6, star 1
!
   IF(kk == 1789)THEN
      suffixstr='y1_J'
      labelstr='y1_J'
   END IF
!
!  tag y7, limb darkening y-coefficient, band 7, star 1
!
   IF(kk == 1790)THEN
      suffixstr='y1_H'
      labelstr='y1_H'
   END IF
!
!  tag y8, limb darkening y-coefficient, band 8, star 1
!
   IF(kk == 1791)THEN
      suffixstr='y1_K'
      labelstr='y1_K'
   END IF
!
!  tag w1, limb darkening y-coefficient, band 1, star 2
!
   IF(kk == 1720)THEN
      suffixstr='y2_U'
      labelstr='y2_U'
   END IF
!
!  tag w2, limb darkening y-coefficient, band 2, star 2
!
   IF(kk == 1721)THEN
      suffixstr='y2_B'
      labelstr='y2_B'
   END IF
!
!  tag w3, limb darkening y-coefficient, band 3, star 2
!
   IF(kk == 1722)THEN
      suffixstr='y2_V'
      labelstr='y2_V'
   END IF
!
!  tag w4, limb darkening y-coefficient, band 4, star 2
!
   IF(kk == 1723)THEN
      suffixstr='y2_R'
      labelstr='y2_R'
   END IF
!
!  tag w5, limb darkening y-coefficient, band 5, star 2
!
   IF(kk == 1724)THEN
      suffixstr='y2_I'
      labelstr='y2_I'
   END IF
!
!  tag w6, limb darkening y-coefficient, band 6, star 2
!
   IF(kk == 1725)THEN
      suffixstr='y2_J'
      labelstr='y2_J'
   END IF
!
!  tag w7, limb darkening y-coefficient, band 7, star 2
!
   IF(kk == 1726)THEN
      suffixstr='y2_H'
      labelstr='y2_H'
   END IF
!
!  tag w8, limb darkening y-coefficient, band 8, star 2
!
   IF(kk == 1727)THEN
      suffixstr='y2_K'
      labelstr='y2_K'
   END IF
!
!  tag m1, limb darkening x-coefficient, band 1, star 3
!
   IF(kk == 1400)THEN
      suffixstr='x3_U'
      labelstr='x3_U'
   END IF
!
!  tag m2, limb darkening x-coefficient, band 2, star 3
!
   IF(kk == 1401)THEN
      suffixstr='x3_B'
      labelstr='x3_B'
   END IF
!
!  tag m3, limb darkening x-coefficient, band 3, star 3
!
   IF(kk == 1402)THEN
      suffixstr='x3_V'
      labelstr='x3_V'
   END IF
!
!  tag m4, limb darkening x-coefficient, band 4, star 3
!
   IF(kk == 1403)THEN
      suffixstr='x3_R'
      labelstr='x3_R'
   END IF
!
!  tag m5, limb darkening x-coefficient, band 5, star 3
!
   IF(kk == 1404)THEN
      suffixstr='x3_I'
      labelstr='x3_I'
   END IF
!
!  tag m6, limb darkening x-coefficient, band 6, star 3
!
   IF(kk == 1405)THEN
      suffixstr='x3_J'
      labelstr='x3_J'
   END IF
!
!  tag m7, limb darkening x-coefficient, band 7, star 3
!
   IF(kk == 1406)THEN
      suffixstr='x3_H'
      labelstr='x3_H'
   END IF
!
!  tag m8, limb darkening x-coefficient, band 8, star 3
!
   IF(kk == 1407)THEN
      suffixstr='x3_K'
      labelstr='x3_K'
   END IF
!
!  tag n1, limb darkening y-coefficient, band 1, star 3
!
   IF(kk == 1432)THEN
      suffixstr='y3_U'
      labelstr='y3_U'
   END IF
!
!  tag n2, limb darkening y-coefficient, band 2, star 3
!
   IF(kk == 1433)THEN
      suffixstr='y3_B'
      labelstr='y3_B'
   END IF
!
!  tag n3, limb darkening y-coefficient, band 3, star 3
!
   IF(kk == 1434)THEN
      suffixstr='y3_V'
      labelstr='y3_V'
   END IF
!
!  tag n4, limb darkening y-coefficient, band 4, star 3
!
   IF(kk == 1435)THEN
      suffixstr='y3_R'
      labelstr='y3_R'
   END IF
!
!  tag n5, limb darkening y-coefficient, band 5, star 3
!
   IF(kk == 1436)THEN
      suffixstr='y3_I'
      labelstr='y3_I'
   END IF
!
!  tag n6, limb darkening y-coefficient, band 6, star 3
!
   IF(kk == 1437)THEN
      suffixstr='y3_J'
      labelstr='y3_J'
   END IF
!
!  tag n7, limb darkening y-coefficient, band 7, star 3
!
   IF(kk == 1438)THEN
      suffixstr='y3_H'
      labelstr='y3_H'
   END IF
!
!  tag n8, limb darkening y-coefficient, band 8, star 3
!
   IF(kk == 1439)THEN
      suffixstr='y3_K'
      labelstr='y3_K'
   END IF
!
!  tag i1, limb darkening x-coefficient, band 1, star 4
!
   IF(kk == 1272)THEN
      suffixstr='x4_U'
      labelstr='x4_U'
   END IF
!
!  tag i2, limb darkening x-coefficient, band 2, star 4
!
   IF(kk == 1273)THEN
      suffixstr='x4_B'
      labelstr='x4_B'
   END IF
!
!  tag i3, limb darkening x-coefficient, band 3, star 4
!
   IF(kk == 1274)THEN
      suffixstr='x4_V'
      labelstr='x4_V'
   END IF
!
!  tag i4, limb darkening x-coefficient, band 4, star 4
!
   IF(kk == 1275)THEN
      suffixstr='x4_R'
      labelstr='x4_R'
   END IF
!
!  tag i5, limb darkening x-coefficient, band 5, star 4
!
   IF(kk == 1276)THEN
      suffixstr='x4_I'
      labelstr='x4_I'
   END IF
!
!  tag i6, limb darkening x-coefficient, band 6, star 4
!
   IF(kk == 1277)THEN
      suffixstr='x4_J'
      labelstr='x4_J'
   END IF
!
!  tag i7, limb darkening x-coefficient, band 7, star 4
!
   IF(kk == 1278)THEN
      suffixstr='x4_H'
      labelstr='x4_H'
   END IF
!
!  tag i8, limb darkening x-coefficient, band 8, star 4
!
   IF(kk == 1279)THEN
      suffixstr='x4_K'
      labelstr='x4_K'
   END IF
!
!  tag j1, limb darkening y-coefficient, band 1, star 4
!
   IF(kk == 1304)THEN
      suffixstr='y4_U'
      labelstr='y4_U'
   END IF
!
!  tag j2, limb darkening y-coefficient, band 2, star 4
!
   IF(kk == 1305)THEN
      suffixstr='y4_B'
      labelstr='y4_B'
   END IF
!
!  tag j3, limb darkening y-coefficient, band 3, star 4
!
   IF(kk == 1306)THEN
      suffixstr='y4_V'
      labelstr='y4_V'
   END IF
!
!  tag j4, limb darkening y-coefficient, band 4, star 4
!
   IF(kk == 1307)THEN
      suffixstr='y4_R'
      labelstr='y4_R'
   END IF
!
!  tag j5, limb darkening y-coefficient, band 5, star 4
!
   IF(kk == 1308)THEN
      suffixstr='y4_I'
      labelstr='y4_I'
   END IF
!
!  tag j6, limb darkening y-coefficient, band 6, star 4
!
   IF(kk == 1309)THEN
      suffixstr='y4_J'
      labelstr='y4_J'
   END IF
!
!  tag j7, limb darkening y-coefficient, band 7, star 4
!
   IF(kk == 1310)THEN
      suffixstr='y4_H'
      labelstr='y4_H'
   END IF
!
!  tag j8, limb darkening y-coefficient, band 8, star 4
!
   IF(kk == 1311)THEN
      suffixstr='y4_K'
      labelstr='y4_K'
   END IF
!
!  tag k1, limb darkening x-coefficient, band 1, star 5
!
   IF(kk == 1336)THEN
      suffixstr='x5_U'
      labelstr='x5_U'
   END IF
!
!  tag k2, limb darkening x-coefficient, band 2, star 5
!
   IF(kk == 1337)THEN
      suffixstr='x5_B'
      labelstr='x5_B'
   END IF
!
!  tag k3, limb darkening x-coefficient, band 3, star 5
!
   IF(kk == 1338)THEN
      suffixstr='x5_V'
      labelstr='x5_V'
   END IF
!
!  tag k4, limb darkening x-coefficient, band 4, star 5
!
   IF(kk == 1339)THEN
      suffixstr='x5_R'
      labelstr='x5_R'
   END IF
!
!  tag k5, limb darkening x-coefficient, band 5, star 5
!
   IF(kk == 1340)THEN
      suffixstr='x5_I'
      labelstr='x5_I'
   END IF
!
!  tag k6, limb darkening x-coefficient, band 6, star 5
!
   IF(kk == 1341)THEN
      suffixstr='x5_J'
      labelstr='x5_J'
   END IF
!
!  tag k7, limb darkening x-coefficient, band 7, star 5
!
   IF(kk == 1342)THEN
      suffixstr='x5_H'
      labelstr='x5_H'
   END IF
!
!  tag k8, limb darkening x-coefficient, band 8, star 5
!
   IF(kk == 1343)THEN
      suffixstr='x5_K'
      labelstr='x5_K'
   END IF
!
!  tag p1, limb darkening y-coefficient, band 1, star 5
!
   IF(kk == 1496)THEN
      suffixstr='y5_U'
      labelstr='y5_U'
   END IF
!
!  tag p2, limb darkening y-coefficient, band 2, star 5
!
   IF(kk == 1497)THEN
      suffixstr='y5_B'
      labelstr='y5_B'
   END IF
!
!  tag p3, limb darkening y-coefficient, band 3, star 5
!
   IF(kk == 1498)THEN
      suffixstr='y5_V'
      labelstr='y5_V'
   END IF
!
!  tag p4, limb darkening y-coefficient, band 4, star 5
!
   IF(kk == 1499)THEN
      suffixstr='y5_R'
      labelstr='y5_R'
   END IF
!
!  tag p5, limb darkening y-coefficient, band 5, star 5
!
   IF(kk == 1500)THEN
      suffixstr='y5_I'
      labelstr='y5_I'
   END IF
!
!  tag p6, limb darkening y-coefficient, band 6, star 5
!
   IF(kk == 1501)THEN
      suffixstr='y5_J'
      labelstr='y5_J'
   END IF
!
!  tag p7, limb darkening y-coefficient, band 7, star 5
!
   IF(kk == 1502)THEN
      suffixstr='y5_H'
      labelstr='y5_H'
   END IF
!
!  tag p8, limb darkening y-coefficient, band 8, star 5
!
   IF(kk == 1503)THEN
      suffixstr='y5_K'
      labelstr='y5_K'
   END IF
!
   RETURN
!
END SUBROUTINE getsuffix
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getsuffixnbsh(instr1,suffixstr,labelstr)
!
!  This subroutine will take an input tag, and return strings
!  to be used as file extensions and as labels.  For example,
!  "pe" gives "period" as a file extension and "Period (days)"
!  as a label.
!
!  This version will not have the escape characters that
!  getsuffix has (\d1, etc.)
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: instr1
   CHARACTER (LEN=80 ), INTENT(OUT)         :: suffixstr
   CHARACTER (LEN=80 ), INTENT(OUT)         :: labelstr
!
   INTEGER :: icnvrt,kk,it1
!
!  Figure out the variable that goes with the input tag.
!
   kk=icnvrt(instr1(1:2))
!
!  tag ct, tesscontam
!
   IF(kk == 83)THEN
      suffixstr='tesscontam'
      labelstr='TESS contaminatione'
   END IF
!
!  tag bq, binqTc, (slope in P-Tc plane for binary)
!
   IF(kk == 48)THEN
      suffixstr='binqTc'
      labelstr='binary P-Tc plane slope'
   END IF
!
!  tag 1q, P1qTc, (slope in P-Tc plane for body 3)
!
   IF(kk == 3528)THEN
      suffixstr='P1qTc'
      labelstr='body 3 P-Tc plane slope'
   END IF
!
!  tag 2q, P2qTc, (slope in P-Tc plane for body 4)
!
   IF(kk == 3560)THEN
      suffixstr='P2qTc'
      labelstr='body 4 P-Tc plane slope'
   END IF
!
!  tag 3q, P3qTc, (slope in P-Tc plane for body 5)
!
   IF(kk == 3592)THEN
      suffixstr='P3qTc'
      labelstr='body 5 P-Tc plane slope'
   END IF
!
!  tag 4q, P4qTc, (slope in P-Tc plane for body 6)
!
   IF(kk == 3624)THEN
      suffixstr='P4qTc'
      labelstr='body 6 P-Tc plane slope'
   END IF
!
!  tag 5q, P5qTc, (slope in P-Tc plane for body 7)
!
   IF(kk == 3656)THEN
      suffixstr='P5qTc'
      labelstr='body 7 P-Tc plane slope'
   END IF
!
!  tag 6q, P6qTc, (slope in P-Tc plane for body 8)
!
   IF(kk == 3688)THEN
      suffixstr='P6qTc'
      labelstr='body 8 P-Tc plane slope'
   END IF
!
!  tag 7q, P7qTc, (slope in P-Tc plane for body 9)
!
   IF(kk == 3720)THEN
      suffixstr='P7qTc'
      labelstr='body 9 P-Tc plane slope'
   END IF
!
!  tag 8q, P8qTc, (slope in P-Tc plane for body 10)
!
   IF(kk == 3752)THEN
      suffixstr='P8qTc'
      labelstr='body 10 P-Tc plane slope'
   END IF
!
!  tag bt, Tbinoff, (offset in P-Tc plane for binary)
!
   IF(kk == 51)THEN
      suffixstr='Tbinoff'
      labelstr='T offset binary'
   END IF
!
!  tag 1t, T1off, (offset in P-Tc plane for body 3)
!
   IF(kk == 3531)THEN
      suffixstr='T1off'
      labelstr='T offset body 3'
   END IF
!
!  tag 2t, T2off, (offset in P-Tc plane for body 4)
!
   IF(kk == 3563)THEN
      suffixstr='T2off'
      labelstr='T offset body 4'
   END IF
!
!  tag 3t, T3off, (offset in P-Tc plane for body 5)
!
   IF(kk == 3595)THEN
      suffixstr='T3off'
      labelstr='T offset body 5'
   END IF
!
!  tag 4t, T4off, (offset in P-Tc plane for body 6)
!
   IF(kk == 3627)THEN
      suffixstr='T4off'
      labelstr='T offset body 6'
   END IF
!
!  tag 5t, T5off, (offset in P-Tc plane for body 7)
!
   IF(kk == 3659)THEN
      suffixstr='T5off'
      labelstr='T offset body 7'
   END IF
!
!  tag 6t, T6off, (offset in P-Tc plane for body 8)
!
   IF(kk == 3691)THEN
      suffixstr='T6off'
      labelstr='T offset body 8'
   END IF
!
!  tag 7t, T7off, (offset in P-Tc plane for body 9)
!
   IF(kk == 3723)THEN
      suffixstr='T7off'
      labelstr='T offset body 9'
   END IF
!
!  tag 8t, T8off, (offset in P-Tc plane for body 10)
!
   IF(kk == 3755)THEN
      suffixstr='T8off'
      labelstr='T offset body 10'
   END IF
!
!  tag sf, fillsum (sum of fill1 and fill2)
!
   IF(kk == 581)THEN
      suffixstr='fillsum'
      labelstr='fill1+fill2'
   END IF
!
!  tag sd, filldiff (difference of fill1 and fill2)
!
   IF(kk == 579)THEN
      suffixstr='filldiff'
      labelstr='fill1-fill2'
   END IF
!
!  tag 1a, angsum1 (sum of P1incl and P1Omega)
!
   IF(kk == 3512)THEN
      suffixstr='angsum1'
      labelstr='P1incl+P1Omega (deg)'
   END IF
!
!  tag 2a, angsum2 (sum of P2incl and P2Omega)
!
   IF(kk == 3544)THEN
      suffixstr='angsum2'
      labelstr='P2incl+P2Omega (deg)'
   END IF
!
!  tag 3a, angsum3 (sum of P3incl and P3Omega)
!
   IF(kk == 3576)THEN
      suffixstr='angsum3'
      labelstr='P3incl+P3Omega (deg)'
   END IF
!
!  tag 4a, angsum4 (sum of P4incl and P4Omega)
!
   IF(kk == 3608)THEN
      suffixstr='angsum4'
      labelstr='P4incl+P4Omega (deg)'
   END IF
!
!  tag 5a, angsum5 (sum of P5incl and P5Omega)
!
   IF(kk == 3640)THEN
      suffixstr='angsum5'
      labelstr='P5incl+P5Omega (deg)'
   END IF
!
!  tag 6a, angsum6 (sum of P6incl and P6Omega)
!
   IF(kk == 3672)THEN
      suffixstr='angsum6'
      labelstr='P6incl+P6Omega (deg)'
   END IF
!
!  tag 7a, angsum7 (sum of P7incl and P7Omega)
!
   IF(kk == 3704)THEN
      suffixstr='angsum7'
      labelstr='P7incl+P7Omega (deg)'
   END IF
!
!  tag 8a, angsum8 (sum of P8incl and P8Omega)
!
   IF(kk == 3736)THEN
      suffixstr='angsum8'
      labelstr='P8incl+P8Omega (deg)'
   END IF
!
!  tag 1d, angdiff1 (difference of P1incl and P1Omega)
!
   IF(kk == 3515)THEN
      suffixstr='angdiff1'
      labelstr='P1incl-P1Omega (deg)'
   END IF
!
!  tag 2d, angdiff2 (difference of P2incl and P2Omega)
!
   IF(kk == 3547)THEN
      suffixstr='angdiff2'
      labelstr='P2incl-P2Omega (deg)'
   END IF
!
!  tag 3d, angdiff3 (difference of P3incl and P3Omega)
!
   IF(kk == 3579)THEN
      suffixstr='angdiff3'
      labelstr='P3incl-P3Omega (deg)'
   END IF
!
!  tag 4d, angdiff4 (difference of P4incl and P4Omega)
!
   IF(kk == 3611)THEN
      suffixstr='angdiff4'
      labelstr='P4incl-P4Omega (deg)'
   END IF
!
!  tag 5d, angdiff5 (difference of P5incl and P5Omega)
!
   IF(kk == 3643)THEN
      suffixstr='angdiff5'
      labelstr='P5incl-P5Omega (deg)'
   END IF
!
!  tag 6d, angdiff6 (difference of P6incl and P6Omega)
!
   IF(kk == 3675)THEN
      suffixstr='angdiff6'
      labelstr='P6incl-P6Omega (deg)'
   END IF
!
!  tag 7d, angdiff7 (difference of P7incl and P7Omega)
!
   IF(kk == 3707)THEN
      suffixstr='angdiff7'
      labelstr='P7incl-P7Omega (deg)'
   END IF
!
!  tag 8d, angdiff8 (difference of P8incl and P8Omega)
!
   IF(kk == 3739)THEN
      suffixstr='angdiff8'
      labelstr='P8incl-P8Omega (deg)'
   END IF
!
!  tag fs, fracsum (sum of fractional radii)
!
   IF(kk == 178)THEN
      suffixstr='fracsum'
      labelstr='frac1+frac2'
   END IF
!
!  tag fd, fracdiff (difference of fractional radii)
!
   IF(kk == 163)THEN
      suffixstr='fracdiff'
      labelstr='frac1-frac2'
   END IF
!
!  tag 97, bin2M3 (M3 in solar masses in binary+binary)
!
   IF(kk == 4790)THEN
      suffixstr='bin2M3'
      labelstr='bin2M3 (solar)'
   END IF
!
!  tag 98, bin2M4 (M4 in solar masses in binary+binary)
!
   IF(kk == 4791)THEN
      suffixstr='bin2M4'
      labelstr='bin2M4 (solar)'
   END IF
!
!  tag 99, bin2R3 (R3 in solar radii in binary+binary)
!
   IF(kk == 4792)THEN
      suffixstr='bin2R3'
      labelstr='bin2R3 (solar)'
   END IF
!
!  tag 90, bin2R4 (R4 in solar radii in binary+binary)
!
   IF(kk == 4783)THEN
      suffixstr='bin2R4'
      labelstr='bin2R4 (solar)'
   END IF
!
!  tag bs, sqesin, (sqrt(e)*sin(omega), binary)
!
   IF(kk == 50)THEN
      suffixstr='sqrt_e_sin_omega'
      labelstr='sqrt(e)*sin(omega)'
   END IF
!
!  tag bc, sqecos, (sqrt(e)*sin(omega), binary)
!
   IF(kk == 34)THEN
      suffixstr='sqrt_e_cos_omega'
      labelstr='sqrt(e)*cos(omega)'
   END IF
!
!  tag 1s, sqtertesin, (sqrt(e)*sin(omega), body 3)
!
   IF(kk == 3530)THEN
      suffixstr='sqP1esin'
      labelstr='sqrt(e1)*sin(omega1)'
   END IF
!
!  tag 1c, sqtertecos, (sqrt(e)*sin(omega), body 3)
!
   IF(kk == 3514)THEN
      suffixstr='sqP1ecos'
      labelstr='sqrt(e1)*cos(omega1)'
   END IF
!
!  tag 2s, sqP2esin, (sqrt(e)*sin(omega), body 4)
!
   IF(kk == 3562)THEN
      suffixstr='sqP2esin'
      labelstr='sqrt(e2)*sin(omega2)'
   END IF
!
!  tag 2c, sqP2ecos, (sqrt(e)*sin(omega), body 4)
!
   IF(kk == 3546)THEN
      suffixstr='sqP2ecos'
      labelstr='sqrt(e2)*cos(omega2)'
   END IF
!
!  tag 3s, sqP3tesin, (sqrt(e)*sin(omega), body 5)
!
   IF(kk == 3594)THEN
      suffixstr='sqP3esin'
      labelstr='sqrt(e3)*sin(omega3)'
   END IF
!
!  tag 3c, sqP3tecos, (sqrt(e)*sin(omega), body 5)
!
   IF(kk == 3578)THEN
      suffixstr='sqP3ecos'
      labelstr='sqrt(e3)*cos(omega3)'
   END IF
!
!  tag 4s, sqP4tesin, (sqrt(e)*sin(omega), body 6)
!
   IF(kk == 3626)THEN
      suffixstr='sqP4esin'
      labelstr='sqrt(e4)*sin(omega4)'
   END IF
!
!  tag 4c, sqP4tecos, (sqrt(e)*sin(omega), body 6)
!
   IF(kk == 3610)THEN
      suffixstr='sqP4ecos'
      labelstr='sqrt(e4)*cos(omega4)'
   END IF
!
!  tag 5s, sqP5tesin, (sqrt(e)*sin(omega), body 7)
!
   IF(kk == 3658)THEN
      suffixstr='sqP5esin'
      labelstr='sqrt(e5)*sin(omega5)'
   END IF
!
!  tag 5c, sqP5tecos, (sqrt(e)*sin(omega), body 7)
!
   IF(kk == 3642)THEN
      suffixstr='sqP5ecos'
      labelstr='sqrt(e5)*cos(omega5)'
   END IF
!
!  tag 6s, sqP6tesin, (sqrt(e)*sin(omega), body 8)
!
   IF(kk == 3690)THEN
      suffixstr='sqP6esin'
      labelstr='sqr(e6)*sin(omega6)'
   END IF
!
!  tag 6c, sqP6tecos, (sqrt(e)*sin(omega), body 8)
!
   IF(kk == 3674)THEN
      suffixstr='sqP6ecos'
      labelstr='sqrt(e6)*cos(omega6)'
   END IF
!
!  tag 7s, sqP7tesin, (sqrt(e)*sin(omega), body 9)
!
   IF(kk == 3722)THEN
      suffixstr='sqP7esin'
      labelstr='sqrt(e7)*sin(omega7)'
   END IF
!
!  tag 7c, sqP7tecos, (sqrt(e)*sin(omega), body 9)
!
   IF(kk == 3706)THEN
      suffixstr='sqP7ecos'
      labelstr='sqrt(e7)*cos(omega7)'
   END IF
!
!  tag 8s, sqP8tesin, (sqrt(e)*sin(omega), body 10)
!
   IF(kk == 3754)THEN
      suffixstr='sqP8esin'
      labelstr='sqrt(e8)*sin(omega8)'
   END IF
!
!  tag 8c, sqP8tecos, (sqrt(e)*sin(omega), body 10)
!
   IF(kk == 3738)THEN
      suffixstr='sqP8ecos'
      labelstr='sqrt(e8)*cos(omega8)'
   END IF
!
!  tag 1m, P1mTc, period-Tconj, body 3
!
   IF(kk == 3524)THEN
      suffixstr='P1mTc'
      labelstr='P1-Tc1 (days)'
   END IF
!
!  tag 1p, P1pTc, period+Tconj, body 3
!
   IF(kk == 3527)THEN
      suffixstr='P1pTc'
      labelstr='P1+Tc1 (days)'
   END IF
!
!  tag 2m, P2mTc, period-Tconj, body 3
!
   IF(kk == 3556)THEN
      suffixstr='P2mTc'
      labelstr='P2-Tc2 (days)'
   END IF
!
!  tag 2p, P2pTc, period+Tconj, body 4
!
   IF(kk == 3559)THEN
      suffixstr='P2pTc'
      labelstr='P2+Tc2 (days)'
   END IF
!
!  tag 3m, P3mTc, period-Tconj, body 5
!
   IF(kk == 3588)THEN
      suffixstr='P3mTc'
      labelstr='P3-Tc3 (days)'
   END IF
!
!  tag 3p, P2pTc, period+Tconj, body 5
!
   IF(kk == 3591)THEN
      suffixstr='P3pTc'
      labelstr='P3+Tc3 (days)'
   END IF
!
!  tag 4m, P4mTc, period-Tconj, body 6
!
   IF(kk == 3620)THEN
      suffixstr='P4mTc'
      labelstr='P4-Tc4 (days)'
   END IF
!
!  tag 4p, P4pTc, period+Tconj, body 6
!
   IF(kk == 3623)THEN
      suffixstr='P4pTc'
      labelstr='P4+Tc4 (days)'
   END IF
!
!  tag 5m, P5mTc, period-Tconj, body 7
!
   IF(kk == 3652)THEN
      suffixstr='P5mTc'
      labelstr='P5-Tc5 (days)'
   END IF
!
!  tag 5p, P5pTc, period+Tconj, body 7
!
   IF(kk == 3655)THEN
      suffixstr='P5pTc'
      labelstr='P5+Tc5 (days)'
   END IF
!
!  tag 6m, P6mTc, period-Tconj, body 8
!
   IF(kk == 3684)THEN
      suffixstr='P6mTc'
      labelstr='P6-Tc6 (days)'
   END IF
!
!  tag 6p, P6pTc, period+Tconj, body 8
!
   IF(kk == 3687)THEN
      suffixstr='P6pTc'
      labelstr='P6+Tc6 (days)'
   END IF
!
!  tag 7m, P7mTc, period-Tconj, body 9
!
   IF(kk == 3716)THEN
      suffixstr='P7mTc'
      labelstr='P7-Tc7 (days)'
   END IF
!
!  tag 7p, P7pTc, period+Tconj, body 9
!
   IF(kk == 3719)THEN
      suffixstr='P7pTc'
      labelstr='P7+Tc7 (days)'
   END IF
!
!  tag 8m, P8mTc, period-Tconj, body 10
!
   IF(kk == 3748)THEN
      suffixstr='P8mTc'
      labelstr='P8-Tc8 (days)'
   END IF
!
!  tag 8p, P8pTc, period+Tconj, body 10
!
   IF(kk == 3751)THEN
      suffixstr='P8pTc'
      labelstr='P8+Tc8 (days)'
   END IF
!
!  tag bm, PbmTc, period-Tconj, binary
!
   IF(kk == 44)THEN
      suffixstr='PbinmTc'
      labelstr='Pbin-Tc (days)'
   END IF
!
!  tag bp, PbpTc, period+Tconj, binary
!
   IF(kk == 47)THEN
      suffixstr='PbinpTc'
      labelstr='Pbin+Tc (days)'
   END IF
!
!  tag bi, axis_I2, inclination of rot. axis star 2
!
   IF(kk == 40)THEN
      suffixstr='axis_I2'
      labelstr='axis_I2 (deg)'
   END IF
!
!  tag bb, axis_beta2, position angle of rot. axis star 2
!
   IF(kk == 33)THEN
      suffixstr='axis_beta2'
      labelstr='axis_beta2 (deg)'
   END IF
!
!  tag ci, axis_I3, inclination of rot. axis star 3
!
   IF(kk == 72)THEN
      suffixstr='axis_I3'
      labelstr='axis_I3 (deg)'
   END IF
!
!  tag cb, axis_beta3, position angle of rot. axis star 3
!
   IF(kk == 65)THEN
      suffixstr='axis_beta3'
      labelstr='axis_beta3 (deg)'
   END IF
!
!  tag di, axis_I4, inclination of rot. axis star 4
!
   IF(kk == 104)THEN
      suffixstr='axis_I4'
      labelstr='axis_I4 (deg)'
   END IF
!
!  tag db, axis_beta4, position angle of rot. axis star 4
!
   IF(kk == 97)THEN
      suffixstr='axis_beta4'
      labelstr='axis_beta4 (deg)'
   END IF
!
!  tag 91, bin2masssum (M3+M4 in binary+binary)
!
   IF(kk == 4784)THEN
      suffixstr='bin2masssum'
      labelstr='bin2 M3+M4 (solar)'
   END IF
!
!  tag 92, bin2massdiff (M3-M4 in binary+binary)
!
   IF(kk == 4785)THEN
      suffixstr='bin2massdiff'
      labelstr='bin2 M3-M4 (solar)'
   END IF
!
!  tag 93, bin2Q (M4/M3 in binary+binary)
!
   IF(kk == 4786)THEN
      suffixstr='bin2Q'
      labelstr='bin2Q (M4/M3)'
   END IF
!
!  tag 94, bin2radsum (R3+R4 in binary+binary)
!
   IF(kk == 4787)THEN
      suffixstr='bin2radsum'
      labelstr='bin2 R3+R4 (solar)'
   END IF
!
!  tag 95, bin2raddiff (R3-R4 in binary+binary)
!
   IF(kk == 4788)THEN
      suffixstr='bin2raddiff'
      labelstr='bin2 R3-R4 (solar)'
   END IF
!
!  tag 96, bin2ratrad (R3/R4 in binary+binary)
!
   IF(kk == 4789)THEN
      suffixstr='bin2ratrad'
      labelstr='bin2ratrad (R3/R4)'
   END IF
!
!   tag g6, g6
!
   IF(kk == 1213)THEN
      suffixstr='g6'
      labelstr='log(g6)'
   END IF
!
!   tag g7, g7
!
   IF(kk == 1214)THEN
      suffixstr='g7'
      labelstr='log(g7)'
   END IF
!
!   tag g8, g8
!
   IF(kk == 1215)THEN
      suffixstr='g8'
      labelstr='log(g8)'
   END IF
!
!   tag g9, g9
!
   IF(kk == 1216)THEN
      suffixstr='g9'
      labelstr='log(g9)'
   END IF
!
!   tag g0, g10
!
   IF(kk == 1207)THEN
      suffixstr='g10'
      labelstr='log(g10)'
   END IF
!
!   tag md, massdiff (M1-M2 for binary)
!
   IF(kk == 387)THEN
      suffixstr='massdiff'
      labelstr='M1-M2 (solar)'
   END IF
!
!   tag ms, masssum (M1+M2 for binary)
!
   IF(kk == 402)THEN
      suffixstr='masssum'
      labelstr='M1+M2 (solar)'
   END IF
!
!   tag o3, omega3
!
   IF(kk == 1466)THEN
      suffixstr='omega3'
      labelstr='omega3'
   END IF
!
!   tag o4, omega4
!
   IF(kk == 1467)THEN
      suffixstr='omega4'
      labelstr='omega4'
   END IF
!
!   tag o5, omega5
!
   IF(kk == 1468)THEN
      suffixstr='omega5'
      labelstr='omega5'
   END IF
!
!   tag o6, omega6
!
   IF(kk == 1469)THEN
      suffixstr='omega6'
      labelstr='omega6'
   END IF
!
!   tag o7, omega7
!
   IF(kk == 1470)THEN
      suffixstr='omega7'
      labelstr='omega7'
   END IF
!
!   tag o8, omega8
!
   IF(kk == 1471)THEN
      suffixstr='omega8'
      labelstr='omega8'
   END IF
!
!   tag o9, omega9
!
   IF(kk == 1472)THEN
      suffixstr='omega9'
      labelstr='omega9'
   END IF
!
!   tag o0, omega10
!
   IF(kk == 1463)THEN
      suffixstr='omega10'
      labelstr='omega10'
   END IF
!
!   tag rs, radsum (R1+R2 for binary)
!
   IF(kk == 562)THEN
      suffixstr='radsum'
      labelstr='R1+R2 (solar)'
   END IF
!
!   tag rd, raddiff (R1-R2 for binary)
!
   IF(kk == 547)THEN
      suffixstr='raddiff'
      labelstr='R1-R2 (solar)'
   END IF
!
!   tag a3, rk3 (apsidal constant, body 3)
!
   IF(kk == 1018)THEN
      suffixstr='rk3'
      labelstr='apsidal_k3'
   END IF
!
!   tag a4, rk4 (apsidal constant, body 4)
!
   IF(kk == 1019)THEN
      suffixstr='rk4'
      labelstr='aspsidal_k4'
   END IF
!
!   tag a5, rk5 (apsidal constant, body 5)
!
   IF(kk == 1020)THEN
      suffixstr='rk5'
      labelstr='apsidal_k5'
   END IF
!
!   tag a6, rk6 (apsidal constant, body 6)
!
   IF(kk == 1021)THEN
      suffixstr='rk6'
      labelstr='apsidal_k6'
   END IF
!
!   tag a7, rk7 (apsidal constant, body 7)
!
   IF(kk == 1022)THEN
      suffixstr='rk7'
      labelstr='apsidal_k7'
   END IF
!
!   tag a8, rk8 (apsidal constant, body 8)
!
   IF(kk == 1023)THEN
      suffixstr='rk8'
      labelstr='apsidal_k8'
   END IF
!
!   tag a9, rk9 (apsidal constant, body 9)
!
   IF(kk == 1024)THEN
      suffixstr='rk9'
      labelstr='apsidal_k9'
   END IF
!
!   tag a0, rk10 (apsidal constant, body 10)
!
   IF(kk == 1015)THEN
      suffixstr='rk10'
      labelstr='apsidal_k10'
   END IF
!
!   tag sm, secmass
!
   IF(kk == 588)THEN
      suffixstr='secmass'
      labelstr='M2 (solar)'
   END IF
!
!   tag sr, secrad
!
   IF(kk == 593)THEN
      suffixstr='secrad'
      labelstr='R2 (solar)'
   END IF
!
!   tag t6, t6 (Teff6)
!
   IF(kk == 1629)THEN
      suffixstr='Teff6'
      labelstr='Teff6 (K)'
   END IF
!
!   tag t7, t7 (Teff7)
!
   IF(kk == 1630)THEN
      suffixstr='Teff7'
      labelstr='Teff7 (K)'
   END IF
!
!   tag t8, t8 (Teff8)
!
   IF(kk == 1631)THEN
      suffixstr='Teff8'
      labelstr='Teff8 (K)'
   END IF
!
!   tag t9, t9 (Teff9)
!
   IF(kk == 1632)THEN
      suffixstr='Teff9'
      labelstr='Teff9 (K)'
   END IF
!
!   tag ta, t10 (Teff10)
!
   IF(kk == 608)THEN
      suffixstr='Teff10'
      labelstr='Teff10 (K)'
   END IF
!
!  tag 61, flux star 6, band 1
!
   IF(kk == 4688)THEN
      suffixstr='flux6_U'
      labelstr='flux6_U'
   END IF
!
!  tag 62, flux star 6, band 2
!
   IF(kk == 4689)THEN
      suffixstr='flux6_B'
      labelstr='flux6_B'
   END IF
!
!  tag 63, flux star 6, band 3
!
   IF(kk == 4690)THEN
      suffixstr='flux6_V'
      labelstr='flux6_V'
   END IF
!
!  tag 64, flux star 6, band 4
!
   IF(kk == 4691)THEN
      suffixstr='flux6_R'
      labelstr='flux6_R'
   END IF
!
!  tag 65, flux star 6, band 5
!
   IF(kk == 4692)THEN
      suffixstr='flux6_I'
      labelstr='flux6_I'
   END IF
!
!  tag 66, flux star 6, band 6
!
   IF(kk == 4693)THEN
      suffixstr='flux6_J'
      labelstr='flux6_J'
   END IF
!
!  tag 67, flux star 6, band 7
!
   IF(kk == 4694)THEN
      suffixstr='flux6_H'
      labelstr='flux6_H'
   END IF
!
!  tag 68, flux star 6, band 8
!
   IF(kk == 4695)THEN
      suffixstr='flux6_K'
      labelstr='flux6_K'
   END IF
!
!  tag 71, flux star 7, band 1
!
   IF(kk == 4720)THEN
      suffixstr='flux7_U'
      labelstr='flux7_U'
   END IF
!
!  tag 72, flux star 7, band 2
!
   IF(kk == 4721)THEN
      suffixstr='flux7_B'
      labelstr='flux7_B'
   END IF
!
!  tag 73, flux star 7, band 3
!
   IF(kk == 4722)THEN
      suffixstr='flux7_V'
      labelstr='flux7_V'
   END IF
!
!  tag 74, flux star 7, band 4
!
   IF(kk == 4723)THEN
      suffixstr='flux7_R'
      labelstr='flux7_R'
   END IF
!
!  tag 75, flux star 7, band 5
!
   IF(kk == 4724)THEN
      suffixstr='flux7_I'
      labelstr='flux7_I'
   END IF
!
!  tag 76, flux star 7, band 6
!
   IF(kk == 4725)THEN
      suffixstr='flux7_J'
      labelstr='flux7_J'
   END IF
!
!  tag 77, flux star 7, band 7
!
   IF(kk == 4726)THEN
      suffixstr='flux7_H'
      labelstr='flux7_H'
   END IF
!
!  tag 78, flux star 7, band 8
!
   IF(kk == 4727)THEN
      suffixstr='flux7_K'
      labelstr='flux7_K'
   END IF
!
!  tag 81, flux star 8, band 1
!
   IF(kk == 4752)THEN
      suffixstr='flux8_U'
      labelstr='flux8_U'
   END IF
!
!  tag 82, flux star 8, band 2
!
   IF(kk == 4753)THEN
      suffixstr='flux8_B'
      labelstr='flux8_B'
   END IF
!
!  tag 83, flux star 8, band 3
!
   IF(kk == 4754)THEN
      suffixstr='flux8_V'
      labelstr='flux8_V'
   END IF
!
!  tag 84, flux star 8, band 4
!
   IF(kk == 4755)THEN
      suffixstr='flux8_R'
      labelstr='flux8_R'
   END IF
!
!  tag 85, flux star 8, band 5
!
   IF(kk == 4756)THEN
      suffixstr='flux8_I'
      labelstr='flux8_I'
   END IF
!
!  tag 86, flux star 8, band 6
!
   IF(kk == 4757)THEN
      suffixstr='flux8_J'
      labelstr='flux8_J'
   END IF
!
!  tag 87, flux star 8, band 7
!
   IF(kk == 4758)THEN
      suffixstr='flux8_H'
      labelstr='flux8_H'
   END IF
!
!  tag 88, flux star 8, band 8
!
   IF(kk == 4759)THEN
      suffixstr='flux8_K'
      labelstr='flux8_K'
   END IF
!
!  tag 91, flux star 9, band 1
!
   IF(kk == 2784)THEN
      suffixstr='flux9_U'
      labelstr='flux9_U'
   END IF
!
!  tag 92, flux star 9, band 2
!
   IF(kk == 2785)THEN
      suffixstr='flux9_B'
      labelstr='flux9_B'
   END IF
!
!  tag 93, flux star 9, band 3
!
   IF(kk == 2786)THEN
      suffixstr='flux9_V'
      labelstr='flux9_V'
   END IF
!
!  tag 94, flux star 9, band 4
!
   IF(kk == 2787)THEN
      suffixstr='flux9_R'
      labelstr='flux9_R'
   END IF
!
!  tag 95, flux star 9, band 5
!
   IF(kk == 2788)THEN
      suffixstr='flux9_I'
      labelstr='flux9_I'
   END IF
!
!  tag 96, flux star 9, band 6
!
   IF(kk == 2789)THEN
      suffixstr='flux9_J'
      labelstr='flux9_J'
   END IF
!
!  tag 97, flux star 9, band 7
!
   IF(kk == 2790)THEN
      suffixstr='flux9_H'
      labelstr='flux9_H'
   END IF
!
!  tag 98, flux star 9, band 8
!
   IF(kk == 2791)THEN
      suffixstr='flux9_K'
      labelstr='flux9_K'
   END IF
!
!  tag 01, flux star 10, band 1
!
   IF(kk == 4496)THEN
      suffixstr='flux10_U'
      labelstr='flux10_U'
   END IF
!
!  tag 02, flux star 10, band 2
!
   IF(kk == 4497)THEN
      suffixstr='flux10_B'
      labelstr='flux10_B'
   END IF
!
!  tag 03, flux star 10, band 3
!
   IF(kk == 4498)THEN
      suffixstr='flux10_V'
      labelstr='flux10_V'
   END IF
!
!  tag 04, flux star 10, band 4
!
   IF(kk == 4499)THEN
      suffixstr='flux10_R'
      labelstr='flux10_R'
   END IF
!
!  tag 05, flux star 10, band 5
!
   IF(kk == 4500)THEN
      suffixstr='flux10_I'
      labelstr='flux10_I'
   END IF
!
!  tag 06, flux star 10, band 6
!
   IF(kk == 4501)THEN
      suffixstr='flux10_J'
      labelstr='flux10_J'
   END IF
!
!  tag 07, flux star 10, band 7
!
   IF(kk == 4502)THEN
      suffixstr='flux10_H'
      labelstr='flux10_H'
   END IF
!
!  tag 08, flux star 10, band 8
!
   IF(kk == 4503)THEN
      suffixstr='flux10_K'
      labelstr='flux10_K'
   END IF
!
!  tag 11, flux star 1, band 1
!
   IF(kk == 4528)THEN
      suffixstr='flux1_U'
      labelstr='flux1_U'
   END IF
!
!  tag 12, flux star 1, band 2
!
   IF(kk == 4529)THEN
      suffixstr='flux1_B'
      labelstr='flux1_B'
   END IF
!
!  tag 13, flux star 1, band 3
!
   IF(kk == 4530)THEN
      suffixstr='flux1_V'
      labelstr='flux1_V'
   END IF
!
!  tag 14, flux star 1, band 4
!
   IF(kk == 4531)THEN
      suffixstr='flux1_R'
      labelstr='flux1_R'
   END IF
!
!  tag 15, flux star 1, band 5
!
   IF(kk == 4532)THEN
      suffixstr='flux1_I'
      labelstr='flux1_I'
   END IF
!
!  tag 16, flux star 1, band 6
!
   IF(kk == 4533)THEN
      suffixstr='flux1_J'
      labelstr='flux1_J'
   END IF
!
!  tag 17, flux star 1, band 7
!
   IF(kk == 4534)THEN
      suffixstr='flux1_H'
      labelstr='flux1_H'
   END IF
!
!  tag 18, flux star 1, band 8
!
   IF(kk == 4535)THEN
      suffixstr='flux1_K'
      labelstr='flux1_K'
   END IF
!
!  tag 21, flux star 2, band 1
!
   IF(kk == 4560)THEN
      suffixstr='flux2_U'
      labelstr='flux2_U'
   END IF
!
!  tag 22, flux star 2, band 2
!
   IF(kk == 4561)THEN
      suffixstr='flux2_B'
      labelstr='flux2_B'
   END IF
!
!  tag 23, flux star 2, band 3
!
   IF(kk == 4562)THEN
      suffixstr='flux2_V'
      labelstr='flux2_V'
   END IF
!
!  tag 24, flux star 2, band 4
!
   IF(kk == 4563)THEN
      suffixstr='flux2_R'
      labelstr='flux2_R'
   END IF
!
!  tag 25, flux star 2, band 5
!
   IF(kk == 4564)THEN
      suffixstr='flux2_I'
      labelstr='flux2_I'
   END IF
!
!  tag 26, flux star 2, band 6
!
   IF(kk == 4565)THEN
      suffixstr='flux2_J'
      labelstr='flux2_J'
   END IF
!
!  tag 27, flux star 2, band 7
!
   IF(kk == 4566)THEN
      suffixstr='flux2_H'
      labelstr='flux2_H'
   END IF
!
!  tag 28, flux star 2, band 8
!
   IF(kk == 4567)THEN
      suffixstr='flux2_K'
      labelstr='flux2_K'
   END IF
!
!  tag 31, flux star 3, band 1
!
   IF(kk == 4592)THEN
      suffixstr='flux3_U'
      labelstr='flux3_U'
   END IF
!
!  tag 32, flux star 3, band 2
!
   IF(kk == 4593)THEN
      suffixstr='flux3_B'
      labelstr='flux3_B'
   END IF
!
!  tag 33, flux star 3, band 3
!
   IF(kk == 4594)THEN
      suffixstr='flux3_V'
      labelstr='flux3_V'
   END IF
!
!  tag 34, flux star 3, band 4
!
   IF(kk == 4595)THEN
      suffixstr='flux3_R'
      labelstr='flux3_R'
   END IF
!
!  tag 35, flux star 3, band 5
!
   IF(kk == 4596)THEN
      suffixstr='flux3_I'
      labelstr='flux3_I'
   END IF
!
!  tag 36, flux star 3, band 6
!
   IF(kk == 4597)THEN
      suffixstr='flux3_J'
      labelstr='flux3_J'
   END IF
!
!  tag 37, flux star 3, band 7
!
   IF(kk == 4598)THEN
      suffixstr='flux3_H'
      labelstr='flux3_H'
   END IF
!
!  tag 38, flux star 3, band 8
!
   IF(kk == 4599)THEN
      suffixstr='flux3_K'
      labelstr='flux3_K'
   END IF
!
!  tag 41, flux star 4, band 1
!
   IF(kk == 4624)THEN
      suffixstr='flux4_U'
      labelstr='flux4_U'
   END IF
!
!  tag 42, flux star 4, band 2
!
   IF(kk == 4625)THEN
      suffixstr='flux4_B'
      labelstr='flux4_B'
   END IF
!
!  tag 43, flux star 4, band 3
!
   IF(kk == 4626)THEN
      suffixstr='flux4_V'
      labelstr='flux4_V'
   END IF
!
!  tag 44, flux star 4, band 4
!
   IF(kk == 4627)THEN
      suffixstr='flux4_R'
      labelstr='flux4_R'
   END IF
!
!  tag 45, flux star 4, band 5
!
   IF(kk == 4628)THEN
      suffixstr='flux4_I'
      labelstr='flux4_I'
   END IF
!
!  tag 46, flux star 4, band 6
!
   IF(kk == 4629)THEN
      suffixstr='flux4_J'
      labelstr='flux4_J'
   END IF
!
!  tag 47, flux star 4, band 7
!
   IF(kk == 4630)THEN
      suffixstr='flux4_H'
      labelstr='flux4_H'
   END IF
!
!  tag 48, flux star 4, band 8
!
   IF(kk == 4631)THEN
      suffixstr='flux4_K'
      labelstr='flux4_K'
   END IF
!
!  tag 51, flux star 5, band 1
!
   IF(kk == 4656)THEN
      suffixstr='flux5_U'
      labelstr='flux5_U'
   END IF
!
!  tag 52, flux star 5, band 2
!
   IF(kk == 4657)THEN
      suffixstr='flux5_B'
      labelstr='flux5_B'
   END IF
!
!  tag 53, flux star 5, band 3
!
   IF(kk == 4658)THEN
      suffixstr='flux5_V'
      labelstr='flux5_V'
   END IF
!
!  tag 54, flux star 5, band 4
!
   IF(kk == 4659)THEN
      suffixstr='flux5_R'
      labelstr='flux5_R'
   END IF
!
!  tag 55, flux star 5, band 5
!
   IF(kk == 4660)THEN
      suffixstr='flux5_I'
      labelstr='flux5_I'
   END IF
!
!  tag 56, flux star 6, band 6
!
   IF(kk == 4661)THEN
      suffixstr='flux5_J'
      labelstr='flux5_J'
   END IF
!
!  tag 57, flux star 5, band 7
!
   IF(kk == 4662)THEN
      suffixstr='flux5_H'
      labelstr='flux5_H'
   END IF
!
!  tag 58, flux star 5, band 8
!
   IF(kk == 4663)THEN
      suffixstr='flux5_K'
      labelstr='flux5_K'
   END IF
!
!  tag ob, Omega_bin, nodal angle of binary
!
   IF(kk == 449)THEN
      suffixstr='Omega_bin'
      labelstr='Omega_bin (deg)'
   END IF
!
!  tag oc, ocose, e*cos(omega) for binary
!
   IF(kk == 450)THEN
      suffixstr='ecos'
      labelstr='e*cos(omega)'
   END IF
!
!  tag os, osine, e*sin(omega) for binary
!
   IF(kk == 466)THEN
      suffixstr='esin'
      labelstr='e*sin(omega)'
   END IF
!
!  tag a1, rk1, tidal apsidal constant for star 1
!
   IF(kk == 1016)THEN
      suffixstr='rk1'
      labelstr='apsidal_k1'
   END IF
!
!  tag a2, rk2, tidal apsidal constant for star 2
!
   IF(kk == 1017)THEN
      suffixstr='rk2'
      labelstr='apsidal_k2'
   END IF
!
!  planet 2 parameters
!
!  tag uj, P2tconj
!
   IF(kk == 649)THEN
      suffixstr='P2tconj'
      labelstr='P2tconj'
   END IF
!
!  tag ut, P2period
!
   IF(kk == 659)THEN
      suffixstr='P2period'
      labelstr='P2period (days)'
   END IF
!
!  tag uu, P2T0
!
   IF(kk == 660)THEN
      suffixstr='P2T0'
      labelstr='P2T0'
   END IF
!
!  tag uv, P2ecos
!
   IF(kk == 661)THEN
      suffixstr='P2ecos'
      labelstr='e2*cos(omega2)'
   END IF
!
!  tag uw, P2esin
!
   IF(kk == 662)THEN
      suffixstr='P2esin'
      labelstr='e2*sin(omega2)'
   END IF
!
!  tag ux, P2incl
!
   IF(kk == 663)THEN
      suffixstr='P2incl'
      labelstr='P2incl (deg)'
   END IF
!
!  tag uy, P2Omega
!
   IF(kk == 664)THEN
      suffixstr='P2Omega'
      labelstr='P2Omega (deg)'
   END IF
!
!  tag uz, P2Q
!
   IF(kk == 665)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P2Q'
      IF(it1 == 0)labelstr='P2Q [(M1+M2)/M4]'
      IF(it1 == 1)labelstr='log(P2Q) [(M1+M2)/M4]'
      IF(it1 == 2)labelstr= 'P2Q (Mass of body 4 in Earth masses)'
   END IF
!
!  tag ub, P2ratrad
!
   IF(kk == 641)THEN
      suffixstr='P2ratrad'
      labelstr='P2ratrad (R1/R4)'
   END IF
!
!  planet 3 parameters
!
!  tag vj, P3tconj
!
   IF(kk == 681)THEN
      suffixstr='P3tconj'
      labelstr='P3tconj'
   END IF
!
!  tag vt, P3period
!
   IF(kk == 691)THEN
      suffixstr='P3period'
      labelstr='P3period (days)'
   END IF
!
!  tag vu, P3T0
!
   IF(kk == 692)THEN
      suffixstr='P3T0'
      labelstr='P3T0'
   END IF
!
!  tag vv, P3ecos
!
   IF(kk == 693)THEN
      suffixstr='P3ecos'
      labelstr='e3*cos(omega3)'
   END IF
!
!  tag vw, P3esin
!
   IF(kk == 694)THEN
      suffixstr='P3esin'
      labelstr='e3*sin(omega3)'
   END IF
!
!  tag vx, P3incl
!
   IF(kk == 695)THEN
      suffixstr='P3incl'
      labelstr='P3incl (deg)'
   END IF
!
!  tag vy, P3Omega
!
   IF(kk == 696)THEN
      suffixstr='P3Omega'
      labelstr='P3Omega (deg)'
   END IF
!
!  tag vz, P3Q
!
   IF(kk == 697)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P3Q'
      IF(it1 == 0)labelstr='P3Q [(M1+M2)/M5]'
      IF(it1 == 1)labelstr='log(P3Q) [(M1+M2)/M5]'
      IF(it1 == 2)labelstr= 'P3Q (Mass of body 5 in Earth masses)'
   END IF
!
!  tag vb, P3ratrad
!
   IF(kk == 673)THEN
      suffixstr='P3ratrad'
      labelstr='P3ratrad (R1/R5)'
   END IF
!
!  planet 4 parameters
!
!  tag wj, P4tconj
!
   IF(kk == 713)THEN
      suffixstr='P4tconj'
      labelstr='P4tconj'
   END IF
!
!  tag wt, P4period
!
   IF(kk == 723)THEN
      suffixstr='P4period'
      labelstr='P4period (days)'
   END IF
!
!  tag wu, P4T0
!
   IF(kk == 724)THEN
      suffixstr='P4T0'
      labelstr='P4T0'
   END IF
!
!  tag wv, P4ecos
!
   IF(kk == 725)THEN
      suffixstr='P4ecos'
      labelstr='e4*cos(omega4)'
   END IF
!
!  tag ww, P4esin
!
   IF(kk == 726)THEN
      suffixstr='P4esin'
      labelstr='e4*sin(omega4)'
   END IF
!
!  tag wx, P4incl
!
   IF(kk == 727)THEN
      suffixstr='P4incl'
      labelstr='P4incl (deg)'
   END IF
!
!  tag wy, P4Omega
!
   IF(kk == 728)THEN
      suffixstr='P4Omega'
      labelstr='P4Omega (deg)'
   END IF
!
!  tag wz, P4Q
!
   IF(kk == 729)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P4Q'
      IF(it1 == 0)labelstr='P4Q [(M1+M2)/M6]'
      IF(it1 == 1)labelstr='log(P4Q) [(M1+M2)/M6]'
      IF(it1 == 2)labelstr= 'P4Q (Mass of body 6 in Earth masses)'
   END IF
!
!  tag wb, P4ratrad
!
   IF(kk == 705)THEN
      suffixstr='P4ratrad'
      labelstr='P4ratrad (R1/R6)'
   END IF
!
!  planet 5 parameters
!
!  tag xj, P5tconj
!
   IF(kk == 745)THEN
      suffixstr='P5tconj'
      labelstr='P5tconj'
   END IF
!
!  tag xt, P5period
!
   IF(kk == 755)THEN
      suffixstr='P5period'
      labelstr='P5period (days)'
   END IF
!
!  tag xu, P5T0
!
   IF(kk == 756)THEN
      suffixstr='P5T0'
      labelstr='P5T0'
   END IF
!
!  tag xv, P5ecos
!
   IF(kk == 757)THEN
      suffixstr='P5ecos'
      labelstr='e5*cos(omega5)'
   END IF
!
!  tag xw, P5esin
!
   IF(kk == 758)THEN
      suffixstr='P5esin'
      labelstr='e5*sin(omega5)'
   END IF
!
!  tag xx, P5incl
!
   IF(kk == 759)THEN
      suffixstr='P5incl'
      labelstr='P5incl (deg)'
   END IF
!
!  tag xy, P5Omega
!
   IF(kk == 760)THEN
      suffixstr='P5Omega'
      labelstr='P5Omega (deg)'
   END IF
!
!  tag xz, P5Q
!
   IF(kk == 761)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P5Q'
      IF(it1 == 0)labelstr='P5Q [(M1+M2)/M7]'
      IF(it1 == 1)labelstr='log(P5Q) [(M1+M2)/M7]'
      IF(it1 == 2)labelstr= 'P5Q (Mass of body 7 in Earth masses)'
   END IF
!
!  tag xb, P5ratrad
!
   IF(kk == 737)THEN
      suffixstr='P5ratrad'
      labelstr='P5ratrad (R1/R7)'
   END IF
!
!  planet 6 parameters
!
!  tag sj, P6tconj
!
   IF(kk == 585)THEN
      suffixstr='P6tconj'
      labelstr='P6tconj'
   END IF
!
!  tag st, P6period
!
   IF(kk == 595)THEN
      suffixstr='P6period'
      labelstr='P6period (days)'
   END IF
!
!  tag su, P6T0
!
   IF(kk == 596)THEN
      suffixstr='P6T0'
      labelstr='P6T0'
   END IF
!
!  tag sv, P6ecos
!
   IF(kk == 597)THEN
      suffixstr='P6ecos'
      labelstr='e6*cos(omega6)'
   END IF
!
!  tag sw, P6esin
!
   IF(kk == 598)THEN
      suffixstr='P6esin'
      labelstr='e6*sin(omega6)'
   END IF
!
!  tag sx, P6incl
!
   IF(kk == 599)THEN
      suffixstr='P6incl'
      labelstr='P6incl (deg)'
   END IF
!
!  tag sy, P6Omega
!
   IF(kk == 600)THEN
      suffixstr='P6Omega'
      labelstr='P6Omega (deg)'
   END IF
!
!  tag sz, P6Q
!
   IF(kk == 601)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P6Q'
      IF(it1 == 0)labelstr='P6Q [(M1+M2)/M8]'
      IF(it1 == 1)labelstr='log(P6Q) [(M1+M2)/M8]'
      IF(it1 == 2)labelstr= 'P6Q (Mass of body 8 in Earth masses)'
   END IF
!
!  tag sb, P6ratrad
!
   IF(kk == 577)THEN
      suffixstr='P6ratrad'
      labelstr='P6ratrad (R1/R8)'
   END IF
!
!  planet 7 parameters
!
!  tag hj, P7tconj
!
   IF(kk == 233)THEN
      suffixstr='P7tconj'
      labelstr='P7tconj'
   END IF
!
!  tag ht, P7period
!
   IF(kk == 243)THEN
      suffixstr='P7period'
      labelstr='P7period (days)'
   END IF
!
!  tag hu, P7T0
!
   IF(kk == 244)THEN
      suffixstr='P7T0'
      labelstr='P7T0'
   END IF
!
!  tag hv, P7ecos
!
   IF(kk == 245)THEN
      suffixstr='P7ecos'
      labelstr='e7*cos(omega7)'
   END IF
!
!  tag hw, P7esin
!
   IF(kk == 246)THEN
      suffixstr='P7esin'
      labelstr='e7*sin(omega7)'
   END IF
!
!  tag hx, P7incl
!
   IF(kk == 247)THEN
      suffixstr='P7incl'
      labelstr='P7incl (deg)'
   END IF
!
!  tag hy, P7Omega
!
   IF(kk == 248)THEN
      suffixstr='P7Omega'
      labelstr='P7Omega (deg)'
   END IF
!
!  tag hz, P7Q
!
   IF(kk == 249)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P7Q'
      IF(it1 == 0)labelstr='P7Q [(M1+M2)/M9]'
      IF(it1 == 1)labelstr='log(P7Q) [(M1+M2)/M9]'
      IF(it1 == 2)labelstr= 'P7Q (Mass of body 9 in Earth masses)'
   END IF
!
!  tag hb, P7ratrad
!
   IF(kk == 225)THEN
      suffixstr='P7ratrad'
      labelstr='P7ratrad (R1/R9)'
   END IF
!
!  planet 8 parameters
!
!  tag kj, P8tconj
!
   IF(kk == 329)THEN
      suffixstr='P8tconj'
      labelstr='P8tconj'
   END IF
!
!  tag kt, P8period
!
   IF(kk == 339)THEN
      suffixstr='P8period'
      labelstr='P8period (days)'
   END IF
!
!  tag ku, P8T0
!
   IF(kk == 340)THEN
      suffixstr='P8T0'
      labelstr='P8T0'
   END IF
!
!  tag kv, P8ecos
!
   IF(kk == 341)THEN
      suffixstr='P8ecos'
      labelstr='e8*cos(omega8)'
   END IF
!
!  tag kw, P8esin
!
   IF(kk == 342)THEN
      suffixstr='P8esin'
      labelstr='e8*sin(omega8)'
   END IF
!
!  tag kx, P8incl
!
   IF(kk == 343)THEN
      suffixstr='P8incl'
      labelstr='P8incl (deg)'
   END IF
!
!  tag ky, P8Omega
!
   IF(kk == 344)THEN
      suffixstr='P8Omega'
      labelstr='P8Omega (deg)'
   END IF
!
!  tag kz, P8Q
!
   IF(kk == 345)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P8Q'
      IF(it1 == 0)labelstr='P8Q [(M1+M2)/M10]'
      IF(it1 == 1)labelstr='log(P8Q) [(M1+M2)/M10]'
      IF(it1 == 2)labelstr= 'P8Q (Mass of body 10 in Earth masses)'
   END IF
!
!  tag kb, P8ratrad
!
   IF(kk == 321)THEN
      suffixstr='P8ratrad'
      labelstr='P8ratrad (R1/R10)'
   END IF
!
!  tag g1, Tgrav1, gravity darkening exponent star 1
!
   IF(kk == 1208)THEN
      suffixstr='Tgrav1'
      labelstr='Tgrav1'
   END IF
!
!  tag g2, Tgrav2, gravity darkening exponent star 2
!
   IF(kk == 1209)THEN
      suffixstr='Tgrav2'
      labelstr='Tgrav2'
   END IF
!
!  tag s0, contamS0, contamination season 0
!
   IF(kk == 1591)THEN
      suffixstr='contamS0'
      labelstr='contamS0'
   END IF
!
!  tag s1, contamS1, contamination season 1
!
   IF(kk == 1592)THEN
      suffixstr='contamS1'
      labelstr='contamS1'
   END IF
!
!  tag s2, contamS2, contamination season 2
!
   IF(kk == 1593)THEN
      suffixstr='contamS2'
      labelstr='contamS2'
   END IF
!
!  tag s3, contamS3, contamination season 3
!
   IF(kk == 1594)THEN
      suffixstr='contamS3'
      labelstr='contamS3'
   END IF
!
!  tag do, omegadot
!
   IF(kk == 110)THEN
      suffixstr='omega_dot'
      labelstr='omega_dot (deg/sec)'
   END IF
!
!  tag tt, tertperiod, orbital period of body 3
!
   IF(kk == 627)THEN
      suffixstr='P1period'
      labelstr='P1period (days)'
   END IF
!
!  tag tu, tertT0, periastron passage of body 3 orbit
!
   IF(kk == 628)THEN
      suffixstr='P1T0'
      labelstr='P1T0'
   END IF
!
!  tag tj, tertconj, time of primary conjunction of body 3 orbit
!
   IF(kk == 617)THEN
      suffixstr='P1tconj'
      labelstr='P1tconj'
   END IF
!
!  tag tv, tertecos, e*cos(omega) for body 3 orbit
!
   IF(kk == 629)THEN
      suffixstr='P1ecos'
      labelstr='e1*cos(omega1)'
   END IF
!
!  tag tw, tertesin, e*sin(omega) for body 3 orbit
!
   IF(kk == 630)THEN
      suffixstr='P1esin'
      labelstr='e1*sin(omega1)'
   END IF
!
!  tag tx, tertincl, inclination of body 3 orbit
!
   IF(kk == 631)THEN
      suffixstr='P1incl'
      labelstr='P1incl (deg)'
   END IF
!
!  tag ty, tertOmega, nodal angle of body 3 orbit
!
   IF(kk == 632)THEN
      suffixstr='P1Omega'
      labelstr='P1Omega (deg)'
   END IF
!
!  tag tz, tertQ, mass ratio of body 3
!
   IF(kk == 633)THEN
      it1=0
      CALL checklog(it1)
      IF(it1 > 2)it1=0
      suffixstr='P1Q'
      IF(it1 == 0)labelstr='P1Q [(M1+M2)/M3]'
      IF(it1 == 1)labelstr='log(P1Q) [(M1+M2)/M3]'
      IF(it1 == 2)labelstr= 'P1Q (Mass of body 3 in Earth masses)'
   END IF
!
!  tag tb, tertratrad, radius ratio of body 3
!
   IF(kk == 609)THEN
      suffixstr='P1ratrad'
      labelstr='P1ratrad (R1/R3)'
   END IF
!
!  tag co, contam, Kepler contamination
!
   IF(kk == 78)THEN
      suffixstr='contam'
      labelstr='Kepler contamination'
   END IF
!
!  tag e1, beam1, Doppler boosting coefficient for star 1
!
   IF(kk == 1144)THEN
      suffixstr='beam1'
      labelstr='beam1'
   END IF
!
!  tag e2, beam2, Doppler boosting coefficient for star 2
!
   IF(kk == 1145)THEN
      suffixstr='beam2'
      labelstr='beam2'
   END IF
!
!  tag pm, primmass, mass of star 1 in solar masses
!
   IF(kk == 492)THEN
      suffixstr='primmass'
      labelstr='M1 (solar)'
   END IF
!
!  tag pr, primrad, radius of star in solar radii
!
   IF(kk == 497)THEN
      suffixstr='primrad'
      labelstr='R1 (solar)'
   END IF
!
!  tag te, temprat, ratio of star 2 temperature to star 1 temperature
!
   IF(kk == 612)THEN
      suffixstr='temprat'
      labelstr='T2/T1'
   END IF
!
!  tag pk, primK, K-velocity of star 1 in km/sec
!
   IF(kk == 490)THEN
      suffixstr='primK'
      labelstr='K1 (km/sec)'
   END IF
!
!  tag ra, ratrad, ratio of star 1 radius to star 2 radius
!
   IF(kk == 544)THEN
      suffixstr='ratrad'
      labelstr='R1/R2'
   END IF
!
!  tag q1, frac1, fractional radius of star 1 (R_1/a)
!
   IF(kk == 1528)THEN
      suffixstr='frac1'
      labelstr='frac1 (R1/a)'
   END IF
!
!  tag q2, frac2, fractional radius of star 2 (R_2/a)
!
   IF(kk == 1529)THEN
      suffixstr='frac2'
      labelstr='frac2 (R2/a)'
   END IF
!
!  tag de, density, density of star 1 in cgs
!
   IF(kk == 100)THEN
      suffixstr='density'
      labelstr='density1 (g/cc)'
   END IF
!
!  tag l1, alb1, albedo of star 1
!
   IF(kk == 1368)THEN
      suffixstr='al1'
      labelstr='albedo1'
   END IF
!
!  tag l2, alb2, albedo of star 2
!
   IF(kk == 1369)THEN
      suffixstr='al2'
      labelstr='albedo2'
   END IF
!
!  tag dp, dphi, phase difference between secondary and primary eclipse
!
   IF(kk == 111)THEN
      suffixstr='dphi'
      labelstr='dphi'
   END IF
!
!  tag tc, Tconj, time of primary conjunction of the binary orbit
!
   IF(kk == 610)THEN
      suffixstr='Tconj'
      labelstr='Tconj'
   END IF
!
!  tag t0, T0, time of periastron passage of binary orbit
!
   IF(kk == 1623)THEN
      suffixstr='T0'
      labelstr='T0'
   END IF
!
!  tag p2, period, binary orbital period in days
!
   IF(kk == 484)THEN
      suffixstr='P'
      labelstr='binary period (days)'
   END IF
!
!  tag in, finc, inclination of binary in degrees
!
   IF(kk == 269)THEN
      suffixstr='i'
      labelstr='binary inclination (deg)'
   END IF
!
!  tag ai, bigI, inclination of star 1's spin axis
!
   IF(kk == 8)THEN
      suffixstr='bigI'
      labelstr='bigI (deg)'
   END IF
!
!  tag ab, bigbeta, position angle of star 1's spin axis
!
   IF(kk == 1)THEN
      suffixstr='bigbeta'
      labelstr='bigbeta (deg)'
   END IF
!
!  tag ma, Q, mass ratio of binary
!
   IF(kk == 384)THEN
      suffixstr='Q'
      labelstr='Q (M2/M1)'
   END IF
!
!  tag f1, fill1, Roche lobe filling factor of star 1
!
   IF(kk == 1176)THEN
      suffixstr='fill1'
      labelstr='fill1'
   END IF
!
!  tag f2, fill2, Roche lobe filling factor of star 2
!
   IF(kk == 1177)THEN
      suffixstr='fill2'
      labelstr='fill2'
   END IF
!
!  tag ri, rinner, inner radius of accretion disk
!
   IF(kk == 552)THEN
      suffixstr='rin'
      labelstr='rinner'
   END IF
!
!  tag o1, omega1, spin frequency of star 1
!
   IF(kk == 1464)THEN
      suffixstr='omega1'
      labelstr='omega1'
   END IF
!
!  tag o2, spin frequency of star 2
!
   IF(kk == 1465)THEN
      suffixstr='omega2'
      labelstr='omega2'
   END IF
!
!  tag ro, router, radius of outer edge of accretion disk
!
   IF(kk == 558)THEN
      suffixstr='rout'
      labelstr='r_outer'
   END IF
!
!  tag t3, Teff3, effective temperature of star 3
!
   IF(kk == 1626)THEN
      suffixstr='Teff3'
      labelstr='Teff3 (K)'
   END IF
!
!  tag t4, Teff4, effective temperature of star 4
!
   IF(kk == 1627)THEN
      suffixstr='Teff4'
      labelstr='Teff4 (K)'
   END IF
!
!  tag t5, Teff5, effective temperature of star 5
!
   IF(kk == 1628)THEN
      suffixstr='Teff5'
      labelstr='Teff5 (K)'
   END IF
!
!  tag g3, g3, gravity of star 3 (log(g))
!
   IF(kk == 1210)THEN
      suffixstr='g3'
      labelstr='log(g3)'
   END IF
!
!  tag g4, g4, gravity of star 4 (log(g))
!
   IF(kk == 1210)THEN
      suffixstr='g4'
      labelstr='log(g4)'
   END IF
!
!  tag g5, g5, gravity of star 5 (log(g))
!
   IF(kk == 1210)THEN
      suffixstr='g5'
      labelstr='log(g5)'
   END IF
!
!  tag be, betarim, opening angle of accretion disk rim
!
   IF(kk == 36)THEN
      suffixstr='beta_rim'
      labelstr='beta_rim (deg)'
   END IF
!
!  tag t1, Teff1, effective temperature of star 1
!
   IF(kk == 1624)THEN
      suffixstr='Teff1'
      labelstr='Teff1 (K)'
   END IF
!
!  tag t2, Teff2, effective temperature of star 2
!
   IF(kk == 1625)THEN
      suffixstr='Teff2'
      labelstr='Teff2 (K)'
   END IF
!
!  tag xi, xi, power-law exponent of the disk temperature profile
!
   IF(kk == 744)THEN
      suffixstr='xi'
      labelstr='xi'
   END IF
!
!  tag td, Tdisk, temperature of the inner edge of the accretion disk
!
   IF(kk == 611)THEN
      suffixstr='Tdisk'
      labelstr='Tdisk'
   END IF
!
!  tag lx, rLx, log of the X-ray luminosity in cgs units
!
   IF(kk == 375)THEN
      suffixstr='Lx'
      labelstr='log(Lx) (erg/sec)'
   END IF
!
!  tag se, separ, semimajor axis of binary orbit
!
   IF(kk == 580)THEN
      suffixstr='separ'
      labelstr='separation (solar)'
   END IF
!
!  tag sa, SA3, ratio of area of star 3 to area of star 1
!
   IF(kk == 576)THEN
      suffixstr='SA3'
      labelstr='SA3 (R3/R1)2'
   END IF
!
!  tag ps, pshift
!
   IF(kk == 498)THEN
      suffixstr='pshift'
      labelstr='pshift'
   END IF
!
!  tag ec, ecc, eccentricity of binary orbit
!
   IF(kk == 130)THEN
      suffixstr='ecc'
      labelstr='binary eccentricity'
   END IF
!
!  tag ar, argper, argument of periastron of binary orbit
!
   IF(kk == 17)THEN
      suffixstr='argper'
      labelstr='binary arg_per (deg)'
   END IF
!
!  tag b1, temperature factor spot 1, star 1
!
   IF(kk == 1048)THEN
      suffixstr='TF_spot1_star1'
      labelstr='TF_spot1_star1'
   END IF
!
!  tag b2, latitude spot 1, star 1
!
   IF(kk == 1049)THEN
      suffixstr='lat_spot1_star1'
      labelstr='lat_spot1_star1 (deg)'
   END IF
!
!  tag b3, longitude spot 1, star 1
!
   IF(kk == 1050)THEN
      suffixstr='lon_spot1_star1'
      labelstr='lon_spot1_star1 (deg)'
   END IF
!
!  tag  b4, radius spot 1, star 1
!
   IF(kk == 1051)THEN
      suffixstr='rad_spot1_star1'
      labelstr='rad_spot1_star1 (deg)'
   END IF
!
!  tag b5, temperature factor spot 2, star 1
!
   IF(kk == 1052)THEN
      suffixstr='TF_spot2_star1'
      labelstr='TF_spot2_star1'
   END IF
!
!  tag b6,  latitude spot 2, star 1
!
   IF(kk == 1053)THEN
      suffixstr='lat_spot2_star1'
      labelstr='lat_spot2_star1 (deg)'
   END IF
!
!  tag b7, longitude spot 2, star 1
!
   IF(kk == 1054)THEN
      suffixstr='lon_spot2_star1'
      labelstr='lon_spot2_star1 (deg)'
   END IF
!
!  tag b8, radius spot 2, star 1
!
   IF(kk == 1055)THEN
      suffixstr='rad_spot2_star1'
      labelstr='rad_spot2_star1 (deg)'
   END IF
!
!  tag c1, temperature factor spot 1, star 2
!
   IF(kk == 1080)THEN
      suffixstr='TF_spot1_star2'
      labelstr='TF_spot1_star2'
   END IF
!
!  tag c2, latitude spot 1, star 2
!
   IF(kk == 1081)THEN
      suffixstr='lat_spot1_star2'
      labelstr='lat_spot1_star2 (deg)'
   END IF
!
!  tag c3, longitude spot 1, star 2
!
   IF(kk == 1082)THEN
      suffixstr='lon_spot1_star2'
      labelstr='lon_spot1_star2 (deg)'
   END IF
!
!  tag c4, radius spot 1, star 2
!
   IF(kk == 1083)THEN
      suffixstr='rad_spot1_star2'
      labelstr='rad_spot1_star2 (deg)'
   END IF
!
!  tag c5,  temperature factor spot 2, star 2
!
   IF(kk == 1084)THEN
      suffixstr='TF_spot2_star2'
      labelstr='TF_spot2_star2'
   END IF
!
!  tag c6, latitude spot 2, star 2
!
   IF(kk == 1085)THEN
      suffixstr='lat_spot2_star2'
      labelstr='lat_spot2_star2 (deg)'
   END IF
!
!  tag c7, longitude spot 2, star 2
!
   IF(kk == 1086)THEN
      suffixstr='lon_spot2_star2'
      labelstr='lon_spot2_star2 (deg)'
   END IF
!
!  tag c8, radius spot 2, star 2
!
   IF(kk == 1087)THEN
      suffixstr='rad_spot2_star2'
      labelstr='rad_spot2_star2 (deg)'
   END IF
!
!  tag d1, temperature factor spot 1 on disk
!
   IF(kk == 1112)THEN
      suffixstr='TF_spot1_disk'
      labelstr='TF_spot1_disk'
   END IF
!
!  tag d2, azimuth spot 1 on disk
!
   IF(kk == 1113)THEN
      suffixstr='azi_spot1_disk'
      labelstr='azi_spot1_disk (deg)'
   END IF
!
!  tag d3, cutoff radius for spot 1 on disk
!
   IF(kk == 1114)THEN
      suffixstr='cut_spot1_disk'
      labelstr='cut_spot1_disk'
   END IF
!
!  tag d4, angular width of spot 1 on disk
!
   IF(kk == 1115)THEN
      suffixstr='wid_spot1_disk'
      labelstr='wid_spot1_disk (deg)'
   END IF
!
!  tag d5, temperature factor spot 2 on disk
!
   IF(kk == 1116)THEN
      suffixstr='TF_spot2_disk'
      labelstr='TF_spot2_disk'
   END IF
!
!  tag d6, azimuth spot 2 on disk
!
   IF(kk == 1117)THEN
      suffixstr='azi_spot2_disk'
      labelstr='azi_spot2_disk (deg)'
   END IF
!
!  tag d7, cutoff radius for spot 2 on disk
!
   IF(kk == 1118)THEN
      suffixstr='cut_spot2_disk'
      labelstr='cut_spot2_disk'
   END IF
!
!  tag d8, angular width of spot 2 on disk
!
   IF(kk == 1119)THEN
      suffixstr='wid_spot2_disk'
      labelstr='wid_spot2_disk (deg)'
   END IF
!
!  tag x1, limb darkening x-coefficient, band 1, star 1
!
   IF(kk == 1752)THEN
      suffixstr='x1_U'
      labelstr='x1_U'
   END IF
!
!  tag x2, limb darkening x-coefficient, band 2, star 1
!
   IF(kk == 1753)THEN
      suffixstr='x1_B'
      labelstr='x1_B'
   END IF
!
!  tag x3, limb darkening x-coefficient, band 3, star 1
!
   IF(kk == 1754)THEN
      suffixstr='x1_V'
      labelstr='x1_V'
   END IF
!
!  tag x4, limb darkening x-coefficient, band 4, star 1
!
   IF(kk == 1755)THEN
      suffixstr='x1_R'
      labelstr='x1_R'
   END IF
!
!  tag x5, limb darkening x-coefficient, band 5, star 1
!
   IF(kk == 1756)THEN
      suffixstr='x1_I'
      labelstr='x1_I'
   END IF
!
!  tag x6, limb darkening x-coefficient, band 6, star 1
!
   IF(kk == 1757)THEN
      suffixstr='x1_J'
      labelstr='x1_J'
   END IF
!
!  tag x7, limb darkening x-coefficient, band 7, star 1
!
   IF(kk == 1758)THEN
      suffixstr='x1_H'
      labelstr='x1_H'
   END IF
!
!  tag x8, limb darkening x-coefficient, band 8, star 1
!
   IF(kk == 1759)THEN
      suffixstr='x1_K'
      labelstr='x1_K'
   END IF
!
!  tag z1, limb darkening x-coefficient, band 1, star 2
!
   IF(kk == 1816)THEN
      suffixstr='x2_U'
      labelstr='x2_U'
   END IF
!
!  tag z2, limb darkening x-coefficient, band 2, star 2
!
   IF(kk == 1817)THEN
      suffixstr='x2_B'
      labelstr='x2_B'
   END IF
!
!  tag z3, limb darkening x-coefficient, band 3, star 2
!
   IF(kk == 1818)THEN
      suffixstr='x2_V'
      labelstr='x2_V'
   END IF
!
!  tag z4, limb darkening x-coefficient, band 4, star 2
!
   IF(kk == 1819)THEN
      suffixstr='x2_R'
      labelstr='x2_R'
   END IF
!
!  tag z5, limb darkening x-coefficient, band 5, star 2
!
   IF(kk == 1820)THEN
      suffixstr='x2_I'
      labelstr='x2_I'
   END IF
!
!  tag z6, limb darkening x-coefficient, band 6, star 2
!
   IF(kk == 1821)THEN
      suffixstr='x2_J'
      labelstr='x2_J'
   END IF
!
!  tag z7, limb darkening x-coefficient, band 7, star 2
!
   IF(kk == 1822)THEN
      suffixstr='x2_H'
      labelstr='x2_H'
   END IF
!
!  tag z8, limb darkening x-coefficient, band 8, star 2
!
   IF(kk == 1823)THEN
      suffixstr='x2_K'
      labelstr='x2_K'
   END IF
!
!  tag y1, limb darkening y-coefficient, band 1, star 1
!
   IF(kk == 1784)THEN
      suffixstr='y1_U'
      labelstr='y1_U'
   END IF
!
!  tag y2, limb darkening y-coefficient, band 2, star 1
!
   IF(kk == 1785)THEN
      suffixstr='y1_B'
      labelstr='y1_B'
   END IF
!
!  tag y3, limb darkening y-coefficient, band 3, star 1
!
   IF(kk == 1786)THEN
      suffixstr='y1_V'
      labelstr='y1_V'
   END IF
!
!  tag y4, limb darkening y-coefficient, band 4, star 1
!
   IF(kk == 1787)THEN
      suffixstr='y1_R'
      labelstr='y1_R'
   END IF
!
!  tag y5, limb darkening y-coefficient, band 5, star 1
!
   IF(kk == 1788)THEN
      suffixstr='y1_I'
      labelstr='y1_I'
   END IF
!
!  tag y6, limb darkening y-coefficient, band 6, star 1
!
   IF(kk == 1789)THEN
      suffixstr='y1_J'
      labelstr='y1_J'
   END IF
!
!  tag y7, limb darkening y-coefficient, band 7, star 1
!
   IF(kk == 1790)THEN
      suffixstr='y1_H'
      labelstr='y1_H'
   END IF
!
!  tag y8, limb darkening y-coefficient, band 8, star 1
!
   IF(kk == 1791)THEN
      suffixstr='y1_K'
      labelstr='y1_K'
   END IF
!
!  tag w1, limb darkening y-coefficient, band 1, star 2
!
   IF(kk == 1720)THEN
      suffixstr='y2_U'
      labelstr='y2_U'
   END IF
!
!  tag w2, limb darkening y-coefficient, band 2, star 2
!
   IF(kk == 1721)THEN
      suffixstr='y2_B'
      labelstr='y2_B'
   END IF
!
!  tag w3, limb darkening y-coefficient, band 3, star 2
!
   IF(kk == 1722)THEN
      suffixstr='y2_V'
      labelstr='y2_V'
   END IF
!
!  tag w4, limb darkening y-coefficient, band 4, star 2
!
   IF(kk == 1723)THEN
      suffixstr='y2_R'
      labelstr='y2_R'
   END IF
!
!  tag w5, limb darkening y-coefficient, band 5, star 2
!
   IF(kk == 1724)THEN
      suffixstr='y2_I'
      labelstr='y2_I'
   END IF
!
!  tag w6, limb darkening y-coefficient, band 6, star 2
!
   IF(kk == 1725)THEN
      suffixstr='y2_J'
      labelstr='y2_J'
   END IF
!
!  tag w7, limb darkening y-coefficient, band 7, star 2
!
   IF(kk == 1726)THEN
      suffixstr='y2_H'
      labelstr='y2_H'
   END IF
!
!  tag w8, limb darkening y-coefficient, band 8, star 2
!
   IF(kk == 1727)THEN
      suffixstr='y2_K'
      labelstr='y2_K'
   END IF
!
!  tag m1, limb darkening x-coefficient, band 1, star 3
!
   IF(kk == 1400)THEN
      suffixstr='x3_U'
      labelstr='x3_U'
   END IF
!
!  tag m2, limb darkening x-coefficient, band 2, star 3
!
   IF(kk == 1401)THEN
      suffixstr='x3_B'
      labelstr='x3_B'
   END IF
!
!  tag m3, limb darkening x-coefficient, band 3, star 3
!
   IF(kk == 1402)THEN
      suffixstr='x3_V'
      labelstr='x3_V'
   END IF
!
!  tag m4, limb darkening x-coefficient, band 4, star 3
!
   IF(kk == 1403)THEN
      suffixstr='x3_R'
      labelstr='x3_R'
   END IF
!
!  tag m5, limb darkening x-coefficient, band 5, star 3
!
   IF(kk == 1404)THEN
      suffixstr='x3_I'
      labelstr='x3_I'
   END IF
!
!  tag m6, limb darkening x-coefficient, band 6, star 3
!
   IF(kk == 1405)THEN
      suffixstr='x3_J'
      labelstr='x3_J'
   END IF
!
!  tag m7, limb darkening x-coefficient, band 7, star 3
!
   IF(kk == 1406)THEN
      suffixstr='x3_H'
      labelstr='x3_H'
   END IF
!
!  tag m8, limb darkening x-coefficient, band 8, star 3
!
   IF(kk == 1407)THEN
      suffixstr='x3_K'
      labelstr='x3_K'
   END IF
!
!  tag n1, limb darkening y-coefficient, band 1, star 3
!
   IF(kk == 1432)THEN
      suffixstr='y3_U'
      labelstr='y3_U'
   END IF
!
!  tag n2, limb darkening y-coefficient, band 2, star 3
!
   IF(kk == 1433)THEN
      suffixstr='y3_B'
      labelstr='y3_B'
   END IF
!
!  tag n3, limb darkening y-coefficient, band 3, star 3
!
   IF(kk == 1434)THEN
      suffixstr='y3_V'
      labelstr='y3_V'
   END IF
!
!  tag n4, limb darkening y-coefficient, band 4, star 3
!
   IF(kk == 1435)THEN
      suffixstr='y3_R'
      labelstr='y3_R'
   END IF
!
!  tag n5, limb darkening y-coefficient, band 5, star 3
!
   IF(kk == 1436)THEN
      suffixstr='y3_I'
      labelstr='y3_I'
   END IF
!
!  tag n6, limb darkening y-coefficient, band 6, star 3
!
   IF(kk == 1437)THEN
      suffixstr='y3_J'
      labelstr='y3_J'
   END IF
!
!  tag n7, limb darkening y-coefficient, band 7, star 3
!
   IF(kk == 1438)THEN
      suffixstr='y3_H'
      labelstr='y3_H'
   END IF
!
!  tag n8, limb darkening y-coefficient, band 8, star 3
!
   IF(kk == 1439)THEN
      suffixstr='y3_K'
      labelstr='y3_K'
   END IF
!
!  tag i1, limb darkening x-coefficient, band 1, star 4
!
   IF(kk == 1272)THEN
      suffixstr='x4_U'
      labelstr='x4_U'
   END IF
!
!  tag i2, limb darkening x-coefficient, band 2, star 4
!
   IF(kk == 1273)THEN
      suffixstr='x4_B'
      labelstr='x4_B'
   END IF
!
!  tag i3, limb darkening x-coefficient, band 3, star 4
!
   IF(kk == 1274)THEN
      suffixstr='x4_V'
      labelstr='x4_V'
   END IF
!
!  tag i4, limb darkening x-coefficient, band 4, star 4
!
   IF(kk == 1275)THEN
      suffixstr='x4_R'
      labelstr='x4_R'
   END IF
!
!  tag i5, limb darkening x-coefficient, band 5, star 4
!
   IF(kk == 1276)THEN
      suffixstr='x4_I'
      labelstr='x4_I'
   END IF
!
!  tag i6, limb darkening x-coefficient, band 6, star 4
!
   IF(kk == 1277)THEN
      suffixstr='x4_J'
      labelstr='x4_J'
   END IF
!
!  tag i7, limb darkening x-coefficient, band 7, star 4
!
   IF(kk == 1278)THEN
      suffixstr='x4_H'
      labelstr='x4_H'
   END IF
!
!  tag i8, limb darkening x-coefficient, band 8, star 4
!
   IF(kk == 1279)THEN
      suffixstr='x4_K'
      labelstr='x4_K'
   END IF
!
!  tag j1, limb darkening y-coefficient, band 1, star 4
!
   IF(kk == 1304)THEN
      suffixstr='y4_U'
      labelstr='y4_U'
   END IF
!
!  tag j2, limb darkening y-coefficient, band 2, star 4
!
   IF(kk == 1305)THEN
      suffixstr='y4_B'
      labelstr='y4_B'
   END IF
!
!  tag j3, limb darkening y-coefficient, band 3, star 4
!
   IF(kk == 1306)THEN
      suffixstr='y4_V'
      labelstr='y4_V'
   END IF
!
!  tag j4, limb darkening y-coefficient, band 4, star 4
!
   IF(kk == 1307)THEN
      suffixstr='y4_R'
      labelstr='y4_R'
   END IF
!
!  tag j5, limb darkening y-coefficient, band 5, star 4
!
   IF(kk == 1308)THEN
      suffixstr='y4_I'
      labelstr='y4_I'
   END IF
!
!  tag j6, limb darkening y-coefficient, band 6, star 4
!
   IF(kk == 1309)THEN
      suffixstr='y4_J'
      labelstr='y4_J'
   END IF
!
!  tag j7, limb darkening y-coefficient, band 7, star 4
!
   IF(kk == 1310)THEN
      suffixstr='y4_H'
      labelstr='y4_H'
   END IF
!
!  tag j8, limb darkening y-coefficient, band 8, star 4
!
   IF(kk == 1311)THEN
      suffixstr='y4_K'
      labelstr='y4_K'
   END IF
!
!  tag k1, limb darkening x-coefficient, band 1, star 5
!
   IF(kk == 1336)THEN
      suffixstr='x5_U'
      labelstr='x5_U'
   END IF
!
!  tag k2, limb darkening x-coefficient, band 2, star 5
!
   IF(kk == 1337)THEN
      suffixstr='x5_B'
      labelstr='x5_B'
   END IF
!
!  tag k3, limb darkening x-coefficient, band 3, star 5
!
   IF(kk == 1338)THEN
      suffixstr='x5_V'
      labelstr='x5_V'
   END IF
!
!  tag k4, limb darkening x-coefficient, band 4, star 5
!
   IF(kk == 1339)THEN
      suffixstr='x5_R'
      labelstr='x5_R'
   END IF
!
!  tag k5, limb darkening x-coefficient, band 5, star 5
!
   IF(kk == 1340)THEN
      suffixstr='x5_I'
      labelstr='x5_I'
   END IF
!
!  tag k6, limb darkening x-coefficient, band 6, star 5
!
   IF(kk == 1341)THEN
      suffixstr='x5_J'
      labelstr='x5_J'
   END IF
!
!  tag k7, limb darkening x-coefficient, band 7, star 5
!
   IF(kk == 1342)THEN
      suffixstr='x5_H'
      labelstr='x5_H'
   END IF
!
!  tag k8, limb darkening x-coefficient, band 8, star 5
!
   IF(kk == 1343)THEN
      suffixstr='x5_K'
      labelstr='x5_K'
   END IF
!
!  tag p1, limb darkening y-coefficient, band 1, star 5
!
   IF(kk == 1496)THEN
      suffixstr='y5_U'
      labelstr='y5_U'
   END IF
!
!  tag p2, limb darkening y-coefficient, band 2, star 5
!
   IF(kk == 1497)THEN
      suffixstr='y5_B'
      labelstr='y5_B'
   END IF
!
!  tag p3, limb darkening y-coefficient, band 3, star 5
!
   IF(kk == 1498)THEN
      suffixstr='y5_V'
      labelstr='y5_V'
   END IF
!
!  tag p4, limb darkening y-coefficient, band 4, star 5
!
   IF(kk == 1499)THEN
      suffixstr='y5_R'
      labelstr='y5_R'
   END IF
!
!  tag p5, limb darkening y-coefficient, band 5, star 5
!
   IF(kk == 1500)THEN
      suffixstr='y5_I'
      labelstr='y5_I'
   END IF
!
!  tag p6, limb darkening y-coefficient, band 6, star 5
!
   IF(kk == 1501)THEN
      suffixstr='y5_J'
      labelstr='y5_J'
   END IF
!
!  tag p7, limb darkening y-coefficient, band 7, star 5
!
   IF(kk == 1502)THEN
      suffixstr='y5_H'
      labelstr='y5_H'
   END IF
!
!  tag p8, limb darkening y-coefficient, band 8, star 5
!
   IF(kk == 1503)THEN
      suffixstr='y5_K'
      labelstr='y5_K'
   END IF
!
   RETURN
!
END SUBROUTINE getsuffixnbsh
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE getthresh(thresh)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(OUT)            :: thresh
!
   INTEGER :: ios
!
   CHARACTER(LEN=40) :: arg
!
   ios=0
   CALL get_command_argument(1,arg)
   READ(arg,*,IOSTAT=ios)thresh
   IF(ios == 0)RETURN
!
END SUBROUTINE getthresh
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
FUNCTION icnvrt(astring)
!
!    November 12, 1999
!
!    This function was stolen directly out of Peter Stetson's DAOPHOT IIe
!    source code.  The following are his comments:
!
!=======================================================================
!
! This little function is supposed to take two ASCII characters and
! express them as an integer in the range 0-(32**2-1) without
! distinguishing upper and lower case:
!
! AA = Aa = aA = aa = 0, AB = Ab = aB = ab = 1, BA = Ba = bA = ba = 32,
! etc.
!
! Argument
!
! ASTRING is a character string containing two ASCII characters.
!
!=======================================================================
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER ::   icnvrt
!
   CHARACTER(LEN=2), INTENT(IN)            :: astring
!
!-----------------------------------------------------------------------
!
   icnvrt=32*MOD(ICHAR(astring(1:1))-1,32)+MOD(ICHAR(astring(2:2))-1,32)
!
!   modifications to allow for numbers in the two
!   input characters
!
   IF(astring(2:2) == '0')icnvrt=icnvrt+1000
   IF(astring(2:2) == '1')icnvrt=icnvrt+1000
   IF(astring(2:2) == '2')icnvrt=icnvrt+1000
   IF(astring(2:2) == '3')icnvrt=icnvrt+1000
   IF(astring(2:2) == '4')icnvrt=icnvrt+1000
   IF(astring(2:2) == '5')icnvrt=icnvrt+1000
   IF(astring(2:2) == '6')icnvrt=icnvrt+1000
   IF(astring(2:2) == '7')icnvrt=icnvrt+1000
   IF(astring(2:2) == '8')icnvrt=icnvrt+1000
   IF(astring(2:2) == '9')icnvrt=icnvrt+1000
!
   IF(astring(1:1) == '0')icnvrt=icnvrt+3000
   IF(astring(1:1) == '1')icnvrt=icnvrt+3000
   IF(astring(1:1) == '2')icnvrt=icnvrt+3000
   IF(astring(1:1) == '3')icnvrt=icnvrt+3000
   IF(astring(1:1) == '4')icnvrt=icnvrt+3000
   IF(astring(1:1) == '5')icnvrt=icnvrt+3000
   IF(astring(1:1) == '6')icnvrt=icnvrt+3000
   IF(astring(1:1) == '7')icnvrt=icnvrt+3000
   IF(astring(1:1) == '8')icnvrt=icnvrt+3000
   IF(astring(1:1) == '9')icnvrt=icnvrt+3000
!
   RETURN
!
END FUNCTION icnvrt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE indexx(n,arr,indx)
!
!  Used by geneticELC to rank arrays by fitness.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=dp), INTENT(IN)                :: arr(n)
   INTEGER, INTENT(OUT)                     :: indx(n)
!
   INTEGER, PARAMETER :: m=7
   INTEGER, PARAMETER :: nstack=80
!
   INTEGER :: i,indxt,ir,itemp,j,jstack,k,l,istack(nstack)
!
   REAL(KIND=dp) ::  a
!
   DO  j=1,n
      indx(j)=j
   END DO
!
   jstack=0
   l=1
   ir=n
20 IF(ir-l < m)THEN
      DO  j=l+1,ir
         indxt=indx(j)
         a=arr(indxt)
         DO  i=j-1,1,-1
            IF(arr(indx(i)) <= a)GO TO 40
            indx(i+1)=indx(i)
         END DO
         i=0
40       indx(i+1)=indxt
      END DO
      IF(jstack == 0)RETURN
      ir=istack(jstack)
      l=istack(jstack-1)
      jstack=jstack-2
   ELSE
      k=(l+ir)/2
      itemp=indx(k)
      indx(k)=indx(l+1)
      indx(l+1)=itemp
      IF(arr(indx(l+1)) > arr(indx(ir)))THEN
         itemp=indx(l+1)
         indx(l+1)=indx(ir)
         indx(ir)=itemp
      END IF
      IF(arr(indx(l)) > arr(indx(ir)))THEN
         itemp=indx(l)
         indx(l)=indx(ir)
         indx(ir)=itemp
      END IF
      IF(arr(indx(l+1)) > arr(indx(l)))THEN
         itemp=indx(l+1)
         indx(l+1)=indx(l)
         indx(l)=itemp
      END IF
      i=l+1
      j=ir
      indxt=indx(l)
      a=arr(indxt)
60    i=i+1
      IF(arr(indx(i)) < a)GO TO 60
70    j=j-1
      IF(arr(indx(j)) > a)GO TO 70
      IF(j < i)GO TO 80
      itemp=indx(i)
      indx(i)=indx(j)
      indx(j)=itemp
      GO TO 60
80    indx(l)=indx(j)
      indx(j)=indxt
      jstack=jstack+2
      IF(jstack > nstack)THEN
         WRITE(*,*)'nstack too small in indexx'
         STOP
      END IF
      IF(ir-i+1 >= j-l)THEN
         istack(jstack)=ir
         istack(jstack-1)=i
         ir=j-1
      ELSE
         istack(jstack)=j-1
         istack(jstack-1)=l
         l=i
      END IF
   END IF
   GO TO 20
!
END SUBROUTINE indexx
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE inversemat(a,c,n)
!
!============================================================
! Inverse matrix
! Method: Based on Doolittle LU factorization for Ax=b
! Alex G. December 2009
!-----------------------------------------------------------
! input ...
! a(n,n) - array of coefficients for matrix A
! n      - dimension
! output ...
! c(n,n) - inverse matrix of A
! comments ...
! the original matrix a(n,n) will be destroyed
! during the calculation
!===========================================================
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=dp), INTENT(IN OUT)            :: a(n,n)
   REAL(KIND=dp), INTENT(OUT)               :: c(n,n)
!
   REAL(KIND=dp)  :: l(100,100),u(100,100),b(100),d(100),x(100)
   REAL(KIND=dp)  :: coeff
!
   INTEGER :: i,j,k
!
! step 0: initialization for matrices L and U and b
! Fortran 90/95 aloows such operations on matrices
!
   DO i=1,100
      b(i)=0.0_dp
      d(i)=0.0_dp
      x(i)=0.0_dp
      DO j=1,100
         l(i,j)=0.0_dp
         u(i,j)=0.0_dp
      END DO
   END DO
!
! step 1: forward elimination
!
   DO k=1,n-1
      DO i=k+1,n
         coeff=a(i,k)/a(k,k)
         l(i,k)=coeff
         DO j=k+1,n
            a(i,j)=a(i,j)-coeff*a(k,j)
         END DO
      END DO
   END DO
!
! Step 2: prepare L and U matrices
! L matrix is a matrix of the elimination coefficient
! + the diagonal elements are 1.0
!
   DO i=1,n
      l(i,i)=1.0_dp
   END DO
!
! U matrix is the upper triangular part of A
!
   DO j=1,n
      DO i=1,j
         u(i,j)=a(i,j)
      END DO
   END DO
!
! Step 3: compute columns of the inverse matrix C
!
   DO k=1,n
      b(k)=1.0_dp
      d(1)=b(1)
!
! Step 3a: Solve Ld=b using the forward substitution
!
      DO i=2,n
         d(i)=b(i)
         DO j=1,i-1
            d(i)=d(i)-l(i,j)*d(j)
         END DO
      END DO
!
! Step 3b: Solve Ux=d using the back substitution
!
      x(n)=d(n)/u(n,n)
      DO i=n-1,1,-1
         x(i)=d(i)
         DO j=n,i+1,-1
            x(i)=x(i)-u(i,j)*x(j)
         END DO
         x(i)=x(i)/u(i,i)
      END DO
!
! Step 3c: fill the solutions x(n) into column k of C
!
      DO i=1,n
         c(i,k)=x(i)
      END DO
      b(k)=0.0_dp
   END DO
!
END SUBROUTINE inversemat
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE isort(n,ra)
!
!   UPDATE September 10, 2001
!
!   This is a new subroutine, similar to sort3.
!
!   Taken from Numerical Recipes.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   INTEGER, INTENT(IN OUT)                  :: ra(n)
!
   INTEGER :: rra,l,ir,i,j
!
   l=n/2+1
   ir=n
10 IF(l > 1)THEN
      l=l-1
      rra=ra(l)
   ELSE
      rra=ra(ir)
      ra(ir)=ra(1)
      ir=ir-1
      IF(ir == 1)THEN
         ra(1)=rra
         RETURN
      END IF
   END IF
   i=l
   j=l+l
20 IF(j <= ir)THEN
      IF(j < ir)THEN
         IF(ra(j) < ra(j+1))j=j+1
      END IF
      IF(rra < ra(j))THEN
         ra(i)=ra(j)
         i=j
         j=j+j
      ELSE
         j=ir+1
      END IF
      GO TO 20
   END IF
   ra(i)=rra
   GO TO 10
!
END SUBROUTINE isort
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE istring(instring,ichi,outstring,ilength)
!
!  Will combine an input string and an input integer in a compact
!  way for printing on the screen.
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: instring
   INTEGER, INTENT(IN)                      :: ichi
   CHARACTER (LEN=*), INTENT(OUT)           :: outstring
   INTEGER, INTENT(OUT)                     :: ilength
!
   INTEGER :: itemp
!
   ilength=0
   outstring=' '
!
!   1 digit positive
!
   IF((ichi >= 0).AND.(ichi < 10))THEN
      WRITE(outstring,10)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   2 digits positive
!
   IF((ichi >= 10).AND.(ichi < 100))THEN
      WRITE(outstring,20)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   3 digits positive
!
   IF((ichi >= 100).AND.(ichi < 1000))THEN
      WRITE(outstring,30)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   4 digits positive
!
   IF((ichi >= 1000).AND.(ichi < 10000))THEN
      WRITE(outstring,40)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   5 digits positive
!
   IF((ichi >= 10000).AND.(ichi < 100000))THEN
      WRITE(outstring,50)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   6 digits positive
!
   IF((ichi >= 100000).AND.(ichi < 1000000))THEN
      WRITE(outstring,60)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   7 digits positive
!
   IF((ichi >= 1000000).AND.(ichi < 10000000))THEN
      WRITE(outstring,70)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   8 digits positive
!
   IF((ichi >= 10000000).AND.(ichi < 100000000))THEN
      WRITE(outstring,80)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   9 digits positive
!
   IF((ichi >= 100000000).AND.(ichi < 1000000000))THEN
      WRITE(outstring,90)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   10 digits positive
!
   IF((ichi >= 1000000000))THEN
      itemp=999999999
      WRITE(outstring,91)instring,itemp
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!!   11 digits positive
!!
!          IF((ichi.ge.100000000000))THEN
!            itemp=99999999999
!            WRITE(outstring,92)instring,itemp
!            ilength=LEN_TRIM(outstring)
!            RETURN
!          END IF
!
!   1 digit negative
!
   IF((ichi > -10).AND.(ichi <= 0))THEN
      WRITE(outstring,100)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   2 digits negative
!
   IF((ichi > -100).AND.(ichi <= -10))THEN
      WRITE(outstring,110)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   3 digits negative
!
   IF((ichi > -1000).AND.(ichi <= -100))THEN
      WRITE(outstring,120)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   4 digits negative
!
   IF((ichi > -10000).AND.(ichi <= -1000))THEN
      WRITE(outstring,130)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   5 digits negative
!
   IF((ichi > -100000).AND.(ichi <= -10000))THEN
      WRITE(outstring,140)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   6 digits negative
!
   IF((ichi > -1000000).AND.(ichi <= -100000))THEN
      WRITE(outstring,150)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   7 digits negative
!
   IF((ichi > -10000000).AND.(ichi <= -1000000))THEN
      WRITE(outstring,160)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   8 digits negative
!
   IF((ichi > -100000000).AND.(ichi <= -10000000))THEN
      WRITE(outstring,170)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   9 digits negative
!
   IF((ichi <= -100000000))THEN
      itemp=-999999999
      WRITE(outstring,180)instring,itemp
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
10 FORMAT(a,'=',i1)
20 FORMAT(a,'=',i2)
30 FORMAT(a,'=',i3)
40 FORMAT(a,'=',i4)
50 FORMAT(a,'=',i5)
60 FORMAT(a,'=',i6)
70 FORMAT(a,'=',i7)
80 FORMAT(a,'=',i8)
90 FORMAT(a,'=',i9)
91 FORMAT(a,'=',i10)
100 FORMAT(a,'=',i2)
110 FORMAT(a,'=',i3)
120 FORMAT(a,'=',i4)
130 FORMAT(a,'=',i5)
140 FORMAT(a,'=',i6)
150 FORMAT(a,'=',i7)
160 FORMAT(a,'=',i8)
170 FORMAT(a,'=',i9)
180 FORMAT(a,'=',i10)
!
   RETURN
!
END SUBROUTINE istring
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE istringnoequals(instring,ichi,outstring,ilength)
!
!  Will combine an input string and an input integer in a compact
!  way for printing on the screen.
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: instring
   INTEGER, INTENT(IN)                      :: ichi
   CHARACTER (LEN=*), INTENT(OUT)           :: outstring
   INTEGER, INTENT(OUT)                     :: ilength
!
   INTEGER :: itemp
!
   ilength=0
   outstring=' '
!
!   1 digit positive
!
   IF((ichi >= 0).AND.(ichi < 10))THEN
      WRITE(outstring,10)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   2 digits positive
!
   IF((ichi >= 10).AND.(ichi < 100))THEN
      WRITE(outstring,20)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   3 digits positive
!
   IF((ichi >= 100).AND.(ichi < 1000))THEN
      WRITE(outstring,30)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   4 digits positive
!
   IF((ichi >= 1000).AND.(ichi < 10000))THEN
      WRITE(outstring,40)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   5 digits positive
!
   IF((ichi >= 10000).AND.(ichi < 100000))THEN
      WRITE(outstring,50)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   6 digits positive
!
   IF((ichi >= 100000).AND.(ichi < 1000000))THEN
      WRITE(outstring,60)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   7 digits positive
!
   IF((ichi >= 1000000).AND.(ichi < 10000000))THEN
      WRITE(outstring,70)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   8 digits positive
!
   IF((ichi >= 10000000).AND.(ichi < 100000000))THEN
      WRITE(outstring,80)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   9 digits positive
!
   IF((ichi >= 1000000000))THEN
      itemp=999999999
      WRITE(outstring,90)instring,itemp
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   1 digit negative
!
   IF((ichi > -10).AND.(ichi <= 0))THEN
      WRITE(outstring,100)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   2 digits negative
!
   IF((ichi > -100).AND.(ichi <= -10))THEN
      WRITE(outstring,110)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   3 digits negative
!
   IF((ichi > -1000).AND.(ichi <= -100))THEN
      WRITE(outstring,120)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   4 digits negative
!
   IF((ichi > -10000).AND.(ichi <= -1000))THEN
      WRITE(outstring,130)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   5 digits negative
!
   IF((ichi > -100000).AND.(ichi <= -10000))THEN
      WRITE(outstring,140)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   6 digits negative
!
   IF((ichi > -1000000).AND.(ichi <= -100000))THEN
      WRITE(outstring,150)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   7 digits negative
!
   IF((ichi > -10000000).AND.(ichi <= -1000000))THEN
      WRITE(outstring,160)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   8 digits negative
!
   IF((ichi > -100000000).AND.(ichi <= -10000000))THEN
      WRITE(outstring,170)instring,ichi
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
!   9 digits negative
!
   IF((ichi <= -100000000))THEN
      itemp=-999999999
      WRITE(outstring,180)instring,itemp
      ilength=LEN_TRIM(outstring)
      RETURN
   END IF
!
10 FORMAT(a,i1)
20 FORMAT(a,i2)
30 FORMAT(a,i3)
40 FORMAT(a,i4)
50 FORMAT(a,i5)
60 FORMAT(a,i6)
70 FORMAT(a,i7)
80 FORMAT(a,i8)
90 FORMAT(a,i9)
100 FORMAT(a,i2)
110 FORMAT(a,i3)
120 FORMAT(a,i4)
130 FORMAT(a,i5)
140 FORMAT(a,i6)
150 FORMAT(a,i7)
160 FORMAT(a,i8)
170 FORMAT(a,i9)
180 FORMAT(a,i10)
!
   RETURN
!
END SUBROUTINE istringnoequals
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE loadtimes(icnarray,nobscycle,obsttimes,obsterr,  &
   nmeclpse)
!
!   This routine will load the observed eclipse times into the
!   arrays.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nmeclpse
   INTEGER, INTENT(OUT)                     :: icnarray(40)
   INTEGER, INTENT(OUT)                     :: nobscycle(40)
   REAL(KIND=dp), INTENT(OUT)               :: obsttimes(40,nmeclpse)
   REAL(KIND=dp), INTENT(OUT)               :: obsterr(40,nmeclpse)
!
   REAL(KIND=dp) :: col1
!
   INTEGER :: ios,ios1,ios2,i,ios3,j,icnvrt,ios4
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=1) :: bell
!
   bell=CHAR(7)
   ios=0
   ios1=0
   ios2=0
!
   OPEN(UNIT=20,FILE='ELCeclipsetimes.opt',STATUS='old',ERR=50,IOSTAT=ios)
   DO i=1,34
      READ(20,80,ERR=70,IOSTAT=ios2)filein
      icnarray(i)=icnvrt(filein(1:2))
      IF(icnarray(i) /= 430)THEN
         OPEN(UNIT=21,FILE=filein,STATUS='old',ERR=60,IOSTAT=ios1)
         DO j=1,10000
            READ(21,*,END=10)col1,obsttimes(i,j),obsterr(i,j)
         END DO
10       nobscycle(i)=j-1
         CLOSE(21)
      END IF
   END DO
   ios3=0
!
   DO i=35,40
      READ(20,80,ERR=70,IOSTAT=ios2)filein
      icnarray(i)=icnvrt(filein(1:2))
      IF(icnarray(i) /= 430)THEN
         OPEN(UNIT=21,FILE=filein,STATUS='old',ERR=60,IOSTAT=ios1)
         DO j=1,10000
            READ(21,*,END=30)col1,obsttimes(i,j),obsterr(i,j)
         END DO
30       nobscycle(i)=j-1
         CLOSE(21)
      END IF
   END DO
!
   ios4=0
!
!  supress compilier warning about unusued variable
!
   col1=1.0_dp
   col1=col1*col1
!
50 IF(ios /= 0)THEN
      WRITE(*,90)bell
      STOP
   END IF
!
60 IF(ios1 /= 0)THEN
      WRITE(*,100)bell,TRIM(filein)
      STOP
   END IF
!
70 IF((ios2 /= 0).OR.(ios3 /= 0).OR.(ios4 /= 0))THEN
      WRITE(*,110)bell
      STOP
   END IF
!
80 FORMAT(a40)
90 FORMAT(a1,'Error:  ELCeclipsetimes.opt not found')
100 FORMAT(a1,'Error:  data file not found (',a,')')
110 FORMAT(a1,'Error:  error reading ELCeclipsetimes.opt')
!
   CLOSE(20)
   RETURN
!
END SUBROUTINE loadtimes
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE lowerenv(narray,arrayx,arrayy,dpsmall,pmin,pmax)
!
!   Will take the input arrays (parameter, chi^2) and find the
!   lower and upper 1-sigma limits
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: narray
   REAL(KIND=dp), INTENT(IN OUT)            :: arrayx(narray)
   REAL(KIND=dp), INTENT(IN OUT)            :: arrayy(narray)
   REAL(KIND=dp), INTENT(IN)                :: dpsmall
   REAL(KIND=dp), INTENT(OUT)               :: pmin
   REAL(KIND=dp), INTENT(OUT)               :: pmax
!
   INTEGER :: i
!
   CALL sort2(narray,arrayy,arrayx)
!
   pmin=1.23E+66_dp
   pmax=-1.23E+66_dp
   DO i=1,narray
      IF(i > 10)THEN
         IF(arrayy(i) > dpsmall+1.01_dp)GO TO 10
      END IF
      IF(arrayx(i) > pmax)pmax=arrayx(i)
      IF(arrayx(i) < pmin)pmin=arrayx(i)
   END DO
!
10 RETURN
!
END SUBROUTINE lowerenv
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE makeloopopt(nvmax)
!
!   This routine will write a correctly formatted 'gridloop.opt' file
!   in the event one does not exist.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
!
   INTEGER :: nvar,idummy
!
   CHARACTER(LEN=1) :: bell
!
   bell=CHAR(7)
   WRITE(*,10)bell
!
   OPEN(UNIT=11,FILE='gridloop.opt',STATUS='new')
!
   WRITE(11,20)
   WRITE(11,30)
   WRITE(11,40)
   WRITE(11,50)
   WRITE(11,60)
   WRITE(11,70)
   WRITE(11,80)
   WRITE(11,90)
   WRITE(11,100)
   WRITE(11,110)
   WRITE(11,111)
   WRITE(11,112)
   WRITE(11,113)
   nvar=nvmax
   WRITE(11,120)nvar
   WRITE(11,130)
   WRITE(11,140)
   WRITE(11,150)
   WRITE(11,160)
   WRITE(11,170)
   WRITE(11,180)
   WRITE(11,190)
   WRITE(11,200)
   WRITE(11,210)
   WRITE(11,220)
   WRITE(11,230)
   WRITE(11,240)
   WRITE(11,250)
   WRITE(11,260)
   WRITE(11,270)
   WRITE(11,280)
   WRITE(11,290)
   WRITE(11,300)
   WRITE(11,310)
   WRITE(11,320)
   WRITE(11,330)
   WRITE(11,340)
   WRITE(11,350)
   WRITE(11,360)
   WRITE(11,370)
   WRITE(11,380)
   WRITE(11,390)
   WRITE(11,400)
   WRITE(11,410)
   WRITE(11,420)
   WRITE(11,430)
   WRITE(11,440)
   WRITE(11,450)
   WRITE(11,460)
   WRITE(11,470)
   WRITE(11,480)
   WRITE(11,490)
   WRITE(11,500)
   WRITE(11,510)
   WRITE(11,520)
   WRITE(11,530)
   WRITE(11,540)
   WRITE(11,550)
   WRITE(11,560)
   WRITE(11,570)
   WRITE(11,580)
!
   idummy=10
   IF(idummy == 10)THEN
      WRITE(*,590)
      STOP
   END IF
!
10 FORMAT(a1,'Error:  file ''gridloop.opt'' not found!  ','I''m '  &
      ,'making one up!')
20 FORMAT('put_your_file_for_U_data_here')
30 FORMAT('put_your_file_for_B_data_here')
40 FORMAT('put_your_file_for_V_data_here')
50 FORMAT('put_your_file_for_R_data_here')
60 FORMAT('put_your_file_for_I_data_here')
70 FORMAT('put_your_file_for_J_data_here')
80 FORMAT('put_your_file_for_H_data_here')
90 FORMAT('put_your_file_for_K_data_here')
100 FORMAT('put_your_file_for_RV1_data_here')
110 FORMAT('put_your_file_for_RV2_data_here')
111 FORMAT('put_your_file_for_RV3_data_here')
112 FORMAT('put_your_file_for_RV4_data_here')
113 FORMAT('put_your_file_for_RV5_data_here')
120 FORMAT(i2,18X,'Number of variables to adjust')
130 FORMAT('inclination')
140 FORMAT('mass ratio')
150 FORMAT('f1 (fill 1)')
160 FORMAT('f2 (fill 2)')
170 FORMAT('o1 (omega1)')
180 FORMAT('o2 (omega2)')
190 FORMAT('rinner [inner disk radius (same units as fill2)]')
200 FORMAT('router [outer disk radius','                         '  &
      ,'               (in units of Rl volume of star 2)]')
210 FORMAT('Tdisk (inner disk temperature)')
220 FORMAT('betarim (half-opening angle of disk rim in degrees)')
230 FORMAT('T1 (T_eff of star 1)')
240 FORMAT('T2 (T_eff of star 2)')
250 FORMAT('xi')
260 FORMAT('Lx (L_x/L_opt)')
270 FORMAT('separation (solar radii)')
280 FORMAT('gamma (gamma velocity in km/sec)')
290 FORMAT('70.0     1.0     3')
300 FORMAT('2.0      0.25    3')
310 FORMAT('0.70     0.1     3')
320 FORMAT('0.70     0.1     3')
330 FORMAT('1.0      0.1     3')
340 FORMAT('1.0      0.1     3')
350 FORMAT('0.01     0.001   3')
360 FORMAT('0.75     0.01    3')
370 FORMAT('20000.0  100.0   3')
380 FORMAT('4.0      0.5     3')
390 FORMAT('6500.0   100.0   3')
400 FORMAT('7500.0   100.0   3')
410 FORMAT('-0.75    0.05    3')
420 FORMAT('10.0     1.0     3')
430 FORMAT('5.0      0.5     3')
440 FORMAT('-69.0    10.0    3')
450 FORMAT('##############################################')
460 FORMAT('#    The first 8 entry MUST be a string indicating ',  &
      'the name of the FOLDED')
470 FORMAT('#    data files.  Put ''NONE'' if there is no file.')
480 FORMAT('#')
490 FORMAT('#    The next entry MUST be a positive integer (Nvar)' )
500 FORMAT('#    The next Nvar entries MUST be strings.  Put the',  &
      ' name ','of the variable you want ')
510 FORMAT('#    in the innermost loop first, the name of the ',''  &
      ,'variable in the second')
520 FORMAT('#    loop second, etc.',' If you do not want to loop '  &
      ,'over all variables, ')
530 FORMAT('#    put the string ''NOTHING'' in the unwanted',' po'  &
      ,'sitions.')
540 FORMAT('#    The names of the allowed variables that can be ',  &
      'put into loops are listed')
550 FORMAT('#    The next Nvar lines contain the starting values',  &
      ' of the variables')
560 FORMAT('#    the step size, and the number of loops.')
570 FORMAT('#')
580 FORMAT('##############################################')
590 FORMAT('Edit the file ''gridloop.opt'' and restart the',' pro'  &
      ,'gram.')
!
   RETURN
!
END SUBROUTINE makeloopopt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE newgetinput(kkkk,nalph1,nalph2,nalph3,nbet1,nbet2,  &
   nbet3,nradius,nref,nseg,ntheta,irvfilt,iatm,icnb,icnh,icni,  &
   icnj,icnk,icnr,icnu,icnv,idark1,idark2,idraw,iecheck,iidint,  &
   ikeep,ilaw,ism1,isw1,isw12,isw13,isw2,isw21,isw22,isw23,  &
   isw24,isw25,isw26,isw27,isw28,isw29,isw3,isw30,isw31,isw32,  &
   isw33,isw34,isw4,isw5,isw6,isw7,isw8,isw80,isw81,isw82,isw83,  &
   isw84,isw85,isw86,isw87,isw88,isw89,isw9,isw100,isynch,it1,  &
   it2,it3,it4,itconj,p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,  &
   p2esin,p2incl,p2mtc,p2ptc,p2period,p2ratrad,p2tconj,p3omega,  &
   p3q,p3t0,p3ecos,p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,  &
   p3tconj,p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,p4ptc,  &
   p4period,p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,p5esin,  &
   p5incl,p5mtc,p5ptc,p5period,p5ratrad,p5tconj,p6omega,p6q,  &
   p6t0,p6ecos,p6esin,p6incl,p6mtc,p6ptc,p6period,p6ratrad,  &
   p6tconj,p7omega,p7q,p7t0,p7ecos,p7esin,p7incl,p7mtc,p7ptc,  &
   p7period,p7ratrad,p7tconj,p8omega,p8q,p8t0,p8ecos,p8esin,  &
   p8incl,p8mtc,p8ptc,p8period,p8ratrad,p8tconj,pbmtc,pbptc,  &
   period,q,sa3,t0,tconj,teff1,teff2,tgrav1,tgrav2,alb1,alb2,  &
   argper,beam1,beam2,betarim,bigi,bigi2,bigi3,bigi4,bigbeta,  &
   bigbeta2,bigbeta3,bigbeta4,bin2q,b2massdiff,b2masssum,  &
   b2raddiff,b2radsum,bin2ratrad,contam,contams0,contams1,  &
   contams2,contams3,dbolx,dboly,density,dphase,dwavex,dwavey,  &
   ecc,ecosw,fill1,fill2,finc,fm,frac1,frac2,g10,g3,g6,g7,g8,g9,  &
   gamma,hh,massdiff,masssum,ocose,omega1,omega2,omega3,omega4,  &
   omega5,omega6,omega7,omega8,omega9,omega10,omegadot,osine,  &
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
   t3off,t4off,t5off,t6off,t7off,t8off,iversion,tesscontam,  &
   tessfilt,tessbin)
!
!   Reads version 6 input files.  If the first line is not
!   #1, then the program will attempt to read the old-style
!   input.
!
!   If KKKK is zero, then ELC.inp is opened.  Otherwise,
!   ELC.???? will be opened, where ???? = KKKK + 1000
!
!   November 1, 2019
!
!   Add the option for some TESS features.  If the header is
!   #2, then set version = 1, and read three lines before Nbody
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: kkkk
   INTEGER, INTENT(OUT)                     :: nalph1
   INTEGER, INTENT(OUT)                     :: nalph2
   INTEGER, INTENT(OUT)                     :: nalph3
   INTEGER, INTENT(OUT)                     :: nbet1
   INTEGER, INTENT(OUT)                     :: nbet2
   INTEGER, INTENT(OUT)                     :: nbet3
   INTEGER, INTENT(OUT)                     :: nradius
   INTEGER, INTENT(OUT)                     :: nref
   INTEGER, INTENT(OUT)                     :: nseg
   INTEGER, INTENT(OUT)                     :: ntheta
   INTEGER, INTENT(OUT)                     :: irvfilt
   INTEGER, INTENT(OUT)                     :: iatm
   INTEGER, INTENT(OUT)                     :: icnb
   INTEGER, INTENT(OUT)                     :: icnh
   INTEGER, INTENT(OUT)                     :: icni
   INTEGER, INTENT(OUT)                     :: icnj
   INTEGER, INTENT(OUT)                     :: icnk
   INTEGER, INTENT(OUT)                     :: icnr
   INTEGER, INTENT(OUT)                     :: icnu
   INTEGER, INTENT(OUT)                     :: icnv
   INTEGER, INTENT(OUT)                     :: idark1
   INTEGER, INTENT(OUT)                     :: idark2
   INTEGER, INTENT(OUT)                     :: idraw
   INTEGER, INTENT(OUT)                     :: iecheck
   INTEGER, INTENT(OUT)                     :: iidint
   INTEGER, INTENT(OUT)                     :: ikeep
   INTEGER, INTENT(OUT)                     :: ilaw
   INTEGER, INTENT(OUT)                     :: ism1
   INTEGER, INTENT(OUT)                     :: isw1
   INTEGER, INTENT(OUT)                     :: isw12
   INTEGER, INTENT(OUT)                     :: isw13
   INTEGER, INTENT(OUT)                     :: isw2
   INTEGER, INTENT(OUT)                     :: isw21
   INTEGER, INTENT(OUT)                     :: isw22
   INTEGER, INTENT(OUT)                     :: isw23
   INTEGER, INTENT(OUT)                     :: isw24
   INTEGER, INTENT(OUT)                     :: isw25
   INTEGER, INTENT(OUT)                     :: isw26
   INTEGER, INTENT(OUT)                     :: isw27
   INTEGER, INTENT(OUT)                     :: isw28
   INTEGER, INTENT(OUT)                     :: isw29
   INTEGER, INTENT(OUT)                     :: isw3
   INTEGER, INTENT(OUT)                     :: isw30
   INTEGER, INTENT(OUT)                     :: isw31
   INTEGER, INTENT(OUT)                     :: isw32
   INTEGER, INTENT(OUT)                     :: isw33
   INTEGER, INTENT(OUT)                     :: isw34
   INTEGER, INTENT(OUT)                     :: isw4
   INTEGER, INTENT(OUT)                     :: isw5
   INTEGER, INTENT(OUT)                     :: isw6
   INTEGER, INTENT(OUT)                     :: isw7
   INTEGER, INTENT(OUT)                     :: isw8
   INTEGER, INTENT(OUT)                     :: isw80
   INTEGER, INTENT(OUT)                     :: isw81
   INTEGER, INTENT(OUT)                     :: isw82
   INTEGER, INTENT(OUT)                     :: isw83
   INTEGER, INTENT(OUT)                     :: isw84
   INTEGER, INTENT(OUT)                     :: isw85
   INTEGER, INTENT(OUT)                     :: isw86
   INTEGER, INTENT(OUT)                     :: isw87
   INTEGER, INTENT(OUT)                     :: isw88
   INTEGER, INTENT(OUT)                     :: isw89
   INTEGER, INTENT(OUT)                     :: isw9
   INTEGER, INTENT(OUT)                     :: isw100
   INTEGER, INTENT(OUT)                     :: isynch
   INTEGER, INTENT(OUT)                     :: it1
   INTEGER, INTENT(OUT)                     :: it2
   INTEGER, INTENT(OUT)                     :: it3
   INTEGER, INTENT(OUT)                     :: it4
   INTEGER, INTENT(OUT)                     :: itconj
   REAL(KIND=dp), INTENT(OUT)               :: p1mtc
   REAL(KIND=dp), INTENT(OUT)               :: p1ptc
   REAL(KIND=dp), INTENT(OUT)               :: p2omega
   REAL(KIND=dp), INTENT(OUT)               :: p2q
   REAL(KIND=dp), INTENT(OUT)               :: p2t0
   REAL(KIND=dp), INTENT(OUT)               :: p2ecos
   REAL(KIND=dp), INTENT(OUT)               :: p2esin
   REAL(KIND=dp), INTENT(OUT)               :: p2incl
   REAL(KIND=dp), INTENT(OUT)               :: p2mtc
   REAL(KIND=dp), INTENT(OUT)               :: p2ptc
   REAL(KIND=dp), INTENT(OUT)               :: p2period
   REAL(KIND=dp), INTENT(OUT)               :: p2ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p2tconj
   REAL(KIND=dp), INTENT(OUT)               :: p3omega
   REAL(KIND=dp), INTENT(OUT)               :: p3q
   REAL(KIND=dp), INTENT(OUT)               :: p3t0
   REAL(KIND=dp), INTENT(OUT)               :: p3ecos
   REAL(KIND=dp), INTENT(OUT)               :: p3esin
   REAL(KIND=dp), INTENT(OUT)               :: p3incl
   REAL(KIND=dp), INTENT(OUT)               :: p3mtc
   REAL(KIND=dp), INTENT(OUT)               :: p3ptc
   REAL(KIND=dp), INTENT(OUT)               :: p3period
   REAL(KIND=dp), INTENT(OUT)               :: p3ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p3tconj
   REAL(KIND=dp), INTENT(OUT)               :: p4omega
   REAL(KIND=dp), INTENT(OUT)               :: p4q
   REAL(KIND=dp), INTENT(OUT)               :: p4t0
   REAL(KIND=dp), INTENT(OUT)               :: p4ecos
   REAL(KIND=dp), INTENT(OUT)               :: p4esin
   REAL(KIND=dp), INTENT(OUT)               :: p4incl
   REAL(KIND=dp), INTENT(OUT)               :: p4mtc
   REAL(KIND=dp), INTENT(OUT)               :: p4ptc
   REAL(KIND=dp), INTENT(OUT)               :: p4period
   REAL(KIND=dp), INTENT(OUT)               :: p4ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p4tconj
   REAL(KIND=dp), INTENT(OUT)               :: p5omega
   REAL(KIND=dp), INTENT(OUT)               :: p5q
   REAL(KIND=dp), INTENT(OUT)               :: p5t0
   REAL(KIND=dp), INTENT(OUT)               :: p5ecos
   REAL(KIND=dp), INTENT(OUT)               :: p5esin
   REAL(KIND=dp), INTENT(OUT)               :: p5incl
   REAL(KIND=dp), INTENT(OUT)               :: p5mtc
   REAL(KIND=dp), INTENT(OUT)               :: p5ptc
   REAL(KIND=dp), INTENT(OUT)               :: p5period
   REAL(KIND=dp), INTENT(OUT)               :: p5ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p5tconj
   REAL(KIND=dp), INTENT(OUT)               :: p6omega
   REAL(KIND=dp), INTENT(OUT)               :: p6q
   REAL(KIND=dp), INTENT(OUT)               :: p6t0
   REAL(KIND=dp), INTENT(OUT)               :: p6ecos
   REAL(KIND=dp), INTENT(OUT)               :: p6esin
   REAL(KIND=dp), INTENT(OUT)               :: p6incl
   REAL(KIND=dp), INTENT(OUT)               :: p6mtc
   REAL(KIND=dp), INTENT(OUT)               :: p6ptc
   REAL(KIND=dp), INTENT(OUT)               :: p6period
   REAL(KIND=dp), INTENT(OUT)               :: p6ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p6tconj
   REAL(KIND=dp), INTENT(OUT)               :: p7omega
   REAL(KIND=dp), INTENT(OUT)               :: p7q
   REAL(KIND=dp), INTENT(OUT)               :: p7t0
   REAL(KIND=dp), INTENT(OUT)               :: p7ecos
   REAL(KIND=dp), INTENT(OUT)               :: p7esin
   REAL(KIND=dp), INTENT(OUT)               :: p7incl
   REAL(KIND=dp), INTENT(OUT)               :: p7mtc
   REAL(KIND=dp), INTENT(OUT)               :: p7ptc
   REAL(KIND=dp), INTENT(OUT)               :: p7period
   REAL(KIND=dp), INTENT(OUT)               :: p7ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p7tconj
   REAL(KIND=dp), INTENT(OUT)               :: p8omega
   REAL(KIND=dp), INTENT(OUT)               :: p8q
   REAL(KIND=dp), INTENT(OUT)               :: p8t0
   REAL(KIND=dp), INTENT(OUT)               :: p8ecos
   REAL(KIND=dp), INTENT(OUT)               :: p8esin
   REAL(KIND=dp), INTENT(OUT)               :: p8incl
   REAL(KIND=dp), INTENT(OUT)               :: p8mtc
   REAL(KIND=dp), INTENT(OUT)               :: p8ptc
   REAL(KIND=dp), INTENT(OUT)               :: p8period
   REAL(KIND=dp), INTENT(OUT)               :: p8ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p8tconj
   REAL(KIND=dp), INTENT(OUT)               :: pbmtc
   REAL(KIND=dp), INTENT(OUT)               :: pbptc
   REAL(KIND=dp), INTENT(OUT)               :: period
   REAL(KIND=dp), INTENT(OUT)               :: q
   REAL(KIND=dp), INTENT(OUT)               :: sa3
   REAL(KIND=dp), INTENT(OUT)               :: t0
   REAL(KIND=dp), INTENT(OUT)               :: tconj
   REAL(KIND=dp), INTENT(OUT)               :: teff1
   REAL(KIND=dp), INTENT(OUT)               :: teff2
   REAL(KIND=dp), INTENT(OUT)               :: tgrav1
   REAL(KIND=dp), INTENT(OUT)               :: tgrav2
   REAL(KIND=dp), INTENT(OUT)               :: alb1
   REAL(KIND=dp), INTENT(OUT)               :: alb2
   REAL(KIND=dp), INTENT(OUT)               :: argper
   REAL(KIND=dp), INTENT(OUT)               :: beam1
   REAL(KIND=dp), INTENT(OUT)               :: beam2
   REAL(KIND=dp), INTENT(OUT)               :: betarim
   REAL(KIND=dp), INTENT(OUT)               :: bigi
   REAL(KIND=dp), INTENT(OUT)               :: bigi2
   REAL(KIND=dp), INTENT(OUT)               :: bigi3
   REAL(KIND=dp), INTENT(OUT)               :: bigi4
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta2
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta3
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta4
   REAL(KIND=dp), INTENT(OUT)               :: bin2q
   REAL(KIND=dp), INTENT(OUT)               :: b2massdiff
   REAL(KIND=dp), INTENT(OUT)               :: b2masssum
   REAL(KIND=dp), INTENT(OUT)               :: b2raddiff
   REAL(KIND=dp), INTENT(OUT)               :: b2radsum
   REAL(KIND=dp), INTENT(OUT)               :: bin2ratrad
   REAL(KIND=dp), INTENT(OUT)               :: contam
   REAL(KIND=dp), INTENT(OUT)               :: contams0
   REAL(KIND=dp), INTENT(OUT)               :: contams1
   REAL(KIND=dp), INTENT(OUT)               :: contams2
   REAL(KIND=dp), INTENT(OUT)               :: contams3
   REAL(KIND=dp), INTENT(OUT)               :: dbolx(8,2)
   REAL(KIND=dp), INTENT(OUT)               :: dboly(8,2)
   REAL(KIND=dp), INTENT(OUT)               :: density
   REAL(KIND=dp), INTENT(OUT)               :: dphase
   REAL(KIND=dp), INTENT(OUT)               :: dwavex(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: dwavey(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: ecc
   REAL(KIND=dp), INTENT(OUT)               :: ecosw
   REAL(KIND=dp), INTENT(OUT)               :: fill1
   REAL(KIND=dp), INTENT(OUT)               :: fill2
   REAL(KIND=dp), INTENT(OUT)               :: finc
   REAL(KIND=dp), INTENT(OUT)               :: fm
   REAL(KIND=dp), INTENT(OUT)               :: frac1
   REAL(KIND=dp), INTENT(OUT)               :: frac2
   REAL(KIND=dp), INTENT(OUT)               :: g10
   REAL(KIND=dp), INTENT(OUT)               :: g3
   REAL(KIND=dp), INTENT(OUT)               :: g6
   REAL(KIND=dp), INTENT(OUT)               :: g7
   REAL(KIND=dp), INTENT(OUT)               :: g8
   REAL(KIND=dp), INTENT(OUT)               :: g9
   REAL(KIND=dp), INTENT(OUT)               :: gamma
   REAL(KIND=dp), INTENT(OUT)               :: hh
   REAL(KIND=dp), INTENT(OUT)               :: massdiff
   REAL(KIND=dp), INTENT(OUT)               :: masssum
   REAL(KIND=dp), INTENT(OUT)               :: ocose
   REAL(KIND=dp), INTENT(OUT)               :: omega1
   REAL(KIND=dp), INTENT(OUT)               :: omega2
   REAL(KIND=dp), INTENT(OUT)               :: omega3
   REAL(KIND=dp), INTENT(OUT)               :: omega4
   REAL(KIND=dp), INTENT(OUT)               :: omega5
   REAL(KIND=dp), INTENT(OUT)               :: omega6
   REAL(KIND=dp), INTENT(OUT)               :: omega7
   REAL(KIND=dp), INTENT(OUT)               :: omega8
   REAL(KIND=dp), INTENT(OUT)               :: omega9
   REAL(KIND=dp), INTENT(OUT)               :: omega10
   REAL(KIND=dp), INTENT(OUT)               :: omegadot
   REAL(KIND=dp), INTENT(OUT)               :: osine
   REAL(KIND=dp), INTENT(OUT)               :: powercoeff(8,9)
   REAL(KIND=dp), INTENT(OUT)               :: primk
   REAL(KIND=dp), INTENT(OUT)               :: primmass
   REAL(KIND=dp), INTENT(OUT)               :: primrad
   REAL(KIND=dp), INTENT(OUT)               :: pshift
   REAL(KIND=dp), INTENT(OUT)               :: rlx
   REAL(KIND=dp), INTENT(OUT)               :: raddiff
   REAL(KIND=dp), INTENT(OUT)               :: radsum
   REAL(KIND=dp), INTENT(OUT)               :: ratrad
   REAL(KIND=dp), INTENT(OUT)               :: rinner
   REAL(KIND=dp), INTENT(OUT)               :: rk3
   REAL(KIND=dp), INTENT(OUT)               :: rk4
   REAL(KIND=dp), INTENT(OUT)               :: rk5
   REAL(KIND=dp), INTENT(OUT)               :: rk6
   REAL(KIND=dp), INTENT(OUT)               :: rk7
   REAL(KIND=dp), INTENT(OUT)               :: rk8
   REAL(KIND=dp), INTENT(OUT)               :: rk9
   REAL(KIND=dp), INTENT(OUT)               :: rk10
   REAL(KIND=dp), INTENT(OUT)               :: router
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint1(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint2(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint3(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint4(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint5(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint6(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint7(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint8(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint9(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint10(8)
   REAL(KIND=dp), INTENT(OUT)               :: secmass
   REAL(KIND=dp), INTENT(OUT)               :: secrad
   REAL(KIND=dp), INTENT(OUT)               :: separ
   REAL(KIND=dp), INTENT(OUT)               :: spot1parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spot2parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spotdparm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: sw1
   REAL(KIND=dp), INTENT(OUT)               :: sw2
   REAL(KIND=dp), INTENT(OUT)               :: sw23
   REAL(KIND=dp), INTENT(OUT)               :: sw24
   REAL(KIND=dp), INTENT(OUT)               :: sw25
   REAL(KIND=dp), INTENT(OUT)               :: sw26
   REAL(KIND=dp), INTENT(OUT)               :: sw27
   REAL(KIND=dp), INTENT(OUT)               :: sw28
   REAL(KIND=dp), INTENT(OUT)               :: sw29
   REAL(KIND=dp), INTENT(OUT)               :: sw3
   REAL(KIND=dp), INTENT(OUT)               :: sw30
   REAL(KIND=dp), INTENT(OUT)               :: sw47
   REAL(KIND=dp), INTENT(OUT)               :: sw48
   REAL(KIND=dp), INTENT(OUT)               :: sw49
   REAL(KIND=dp), INTENT(OUT)               :: sw5
   REAL(KIND=dp), INTENT(OUT)               :: sw6
   REAL(KIND=dp), INTENT(OUT)               :: sw7
   REAL(KIND=dp), INTENT(OUT)               :: sw72
   REAL(KIND=dp), INTENT(OUT)               :: sw73
   REAL(KIND=dp), INTENT(OUT)               :: sw8
   REAL(KIND=dp), INTENT(OUT)               :: sw80
   REAL(KIND=dp), INTENT(OUT)               :: sw81
   REAL(KIND=dp), INTENT(OUT)               :: sw82
   REAL(KIND=dp), INTENT(OUT)               :: sw83
   REAL(KIND=dp), INTENT(OUT)               :: sw84
   REAL(KIND=dp), INTENT(OUT)               :: sw85
   REAL(KIND=dp), INTENT(OUT)               :: sw86
   REAL(KIND=dp), INTENT(OUT)               :: sw87
   REAL(KIND=dp), INTENT(OUT)               :: sw88
   REAL(KIND=dp), INTENT(OUT)               :: sw89
   REAL(KIND=dp), INTENT(OUT)               :: sw9
   REAL(KIND=dp), INTENT(OUT)               :: t10
   REAL(KIND=dp), INTENT(OUT)               :: t3
   REAL(KIND=dp), INTENT(OUT)               :: t6
   REAL(KIND=dp), INTENT(OUT)               :: t7
   REAL(KIND=dp), INTENT(OUT)               :: t8
   REAL(KIND=dp), INTENT(OUT)               :: t9
   REAL(KIND=dp), INTENT(OUT)               :: tdisk
   REAL(KIND=dp), INTENT(OUT)               :: temprat
   REAL(KIND=dp), INTENT(OUT)               :: tertomega
   REAL(KIND=dp), INTENT(OUT)               :: tertq
   REAL(KIND=dp), INTENT(OUT)               :: tertconj
   REAL(KIND=dp), INTENT(OUT)               :: tertecos
   REAL(KIND=dp), INTENT(OUT)               :: tertesin
   REAL(KIND=dp), INTENT(OUT)               :: tertincl
   REAL(KIND=dp), INTENT(OUT)               :: tertperiod
   REAL(KIND=dp), INTENT(OUT)               :: tertratrad
   REAL(KIND=dp), INTENT(OUT)               :: tertt0
   REAL(KIND=dp), INTENT(OUT)               :: wave(8)
   REAL(KIND=dp), INTENT(OUT)               :: xi
   INTEGER, INTENT(OUT)                     :: iunit
   REAL(KIND=dp), INTENT(OUT)               :: fracsum
   REAL(KIND=dp), INTENT(OUT)               :: fracdiff
   REAL(KIND=dp), INTENT(OUT)               :: bin2m3
   REAL(KIND=dp), INTENT(OUT)               :: bin2m4
   REAL(KIND=dp), INTENT(OUT)               :: bin2r3
   REAL(KIND=dp), INTENT(OUT)               :: bin2r4
   REAL(KIND=dp), INTENT(OUT)               :: sqecos
   REAL(KIND=dp), INTENT(OUT)               :: sqesin
   REAL(KIND=dp), INTENT(OUT)               :: sqtertecos
   REAL(KIND=dp), INTENT(OUT)               :: sqtertesin
   REAL(KIND=dp), INTENT(OUT)               :: sqp2ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp2esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp3ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp3esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp4ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp4esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp5ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp5esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp6ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp6esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp7ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp7esin
   REAL(KIND=dp), INTENT(OUT)               :: sqp8ecos
   REAL(KIND=dp), INTENT(OUT)               :: sqp8esin
   REAL(KIND=dp), INTENT(OUT)               :: angsum1
   REAL(KIND=dp), INTENT(OUT)               :: angdiff1
   REAL(KIND=dp), INTENT(OUT)               :: angsum2
   REAL(KIND=dp), INTENT(OUT)               :: angdiff2
   REAL(KIND=dp), INTENT(OUT)               :: angsum3
   REAL(KIND=dp), INTENT(OUT)               :: angdiff3
   REAL(KIND=dp), INTENT(OUT)               :: angsum4
   REAL(KIND=dp), INTENT(OUT)               :: angdiff4
   REAL(KIND=dp), INTENT(OUT)               :: angsum5
   REAL(KIND=dp), INTENT(OUT)               :: angdiff5
   REAL(KIND=dp), INTENT(OUT)               :: angsum6
   REAL(KIND=dp), INTENT(OUT)               :: angdiff6
   REAL(KIND=dp), INTENT(OUT)               :: angsum7
   REAL(KIND=dp), INTENT(OUT)               :: angdiff7
   REAL(KIND=dp), INTENT(OUT)               :: angsum8
   REAL(KIND=dp), INTENT(OUT)               :: angdiff8
   INTEGER, INTENT(OUT)                     :: imag
   REAL(KIND=dp), INTENT(OUT)               :: fillsum
   REAL(KIND=dp), INTENT(OUT)               :: filldiff
   REAL(KIND=dp), INTENT(OUT)               :: binqtc
   REAL(KIND=dp), INTENT(OUT)               :: p1qtc
   REAL(KIND=dp), INTENT(OUT)               :: p2qtc
   REAL(KIND=dp), INTENT(OUT)               :: p3qtc
   REAL(KIND=dp), INTENT(OUT)               :: p4qtc
   REAL(KIND=dp), INTENT(OUT)               :: p5qtc
   REAL(KIND=dp), INTENT(OUT)               :: p6qtc
   REAL(KIND=dp), INTENT(OUT)               :: p7qtc
   REAL(KIND=dp), INTENT(OUT)               :: p8qtc
   REAL(KIND=dp), INTENT(OUT)               :: tbinoff
   REAL(KIND=dp), INTENT(OUT)               :: t1off
   REAL(KIND=dp), INTENT(OUT)               :: t2off
   REAL(KIND=dp), INTENT(OUT)               :: t3off
   REAL(KIND=dp), INTENT(OUT)               :: t4off
   REAL(KIND=dp), INTENT(OUT)               :: t5off
   REAL(KIND=dp), INTENT(OUT)               :: t6off
   REAL(KIND=dp), INTENT(OUT)               :: t7off
   REAL(KIND=dp), INTENT(OUT)               :: t8off
   INTEGER, INTENT(OUT)                     :: iversion
   REAL(KIND=dp), INTENT(OUT)               :: tesscontam
   INTEGER, INTENT(OUT)                     :: tessfilt
   REAL(KIND=dp), INTENT(OUT)               :: tessbin
!
   INTEGER           :: i,ios
!
   REAL(KIND=dp)     :: fake
!
   CHARACTER (LEN=1) :: bell
   CHARACTER (LEN=4) :: extension
   CHARACTER (LEN=8) :: ff
   CHARACTER (LEN=40) :: blank
!
   bell=CHAR(7)
!
   IF(kkkk > 8999)THEN
      WRITE(*,20)bell
      STOP
   END IF
!
   IF(kkkk < 8999)WRITE(extension,30)kkkk+1000
!
   IF(kkkk == 0)extension='inp'
   ff='ELC.'//extension
!
!   Set all of the integers to zero.
!
   iversion=0
   tessfilt=8
   nalph1=0
   nbet1=0
   nalph2=0
   nbet2=0
   ntheta=0
   nradius=0
   nref=0
   idraw=0
   iecheck=0
   iidint=0
   iatm=0
   ism1=0
   icnu=0
   icnb=0
   icnv=0
   icnr=0
   icni=0
   icnj=0
   icnh=0
   icnk=0
   irvfilt=0
   isw1=0
   isw2=0
   isw3=0
   isw4=0
   ilaw=0
   ikeep=0
   isynch=0
   isw5=0
   isw6=0
   isw7=0
   isw8=0
   isw9=0
   imag=0
   idark1=0
   idark2=0
   isw12=0
   isw13=0
   isw21=0
   isw22=0
   isw23=0
   isw24=0
   isw25=0
   isw26=0
   isw27=0
   isw28=0
   isw29=0
   isw30=0
   isw31=0
   isw32=0
   isw33=0
   isw34=0
   isw80=0
   isw81=0
   isw82=0
   isw83=0
   isw84=0
   isw85=0
   isw86=0
   isw87=0
   isw88=0
   isw89=0
   nalph3=0
   nbet3=0
   itconj=0
   it1=0
   it2=0
   it3=0
   it4=0
   isw100=0
   nseg=0
   i=0
   iunit=0
!
!  Set the reals to zero
!
   tessbin=0.0_dp
   tesscontam=0.0_dp
   fill1=0.0_dp
   fill2=0.0_dp
   omega1=0.0_dp
   omega2=0.0_dp
   dphase=0.0_dp
   q=1.0_dp
   finc=0.0_dp
   teff1=0.0_dp
   teff2=0.0_dp
   tgrav1=0.0_dp
   tgrav2=0.0_dp
   betarim=0.0_dp
   rinner=0.0_dp
   router=0.0_dp
   tdisk=0.0_dp
   xi=0.0_dp
   alb1=0.0_dp
   alb2=0.0_dp
   rlx=0.0_dp
   period=0.0_dp
   fm=0.0_dp
   separ=0.0_dp
   gamma=0.0_dp
   t3=0.0_dp
   g3=0.0_dp
   sa3=0.0_dp
   density=0.0_dp
   sw1=0.0_dp
   sw2=0.0_dp
   sw3=0.0_dp
   t0=0.0_dp
   ecc=0.0_dp
   argper=0.0_dp
   pshift=0.0_dp
   sw5=0.0_dp
   sw6=0.0_dp
   sw7=0.0_dp
   sw8=0.0_dp
   sw9=0.0_dp
   omega9=0.0_dp
   primmass=0.0_dp
   primk=0.0_dp
   primrad=0.0_dp
   ratrad=0.0_dp
   frac1=0.0_dp
   frac2=0.0_dp
   fillsum=0.0_dp
   filldiff=0.0_dp
   ecosw=0.0_dp
   temprat=0.0_dp
   bigi=0.0_dp
   bigbeta=0.0_dp
   sw23=0.0_dp
   sw24=0.0_dp
   sw25=0.0_dp
   sw26=0.0_dp
   sw27=0.0_dp
   sw28=0.0_dp
   sw29=0.0_dp
   sw30=0.0_dp
   contam=0.0_dp
   tconj=0.0_dp
   beam1=0.0_dp
   beam2=0.0_dp
   ocose=0.0_dp
   osine=0.0_dp
   omegadot=0.0_dp
   contams0=0.0_dp
   contams1=0.0_dp
   contams2=0.0_dp
   contams3=0.0_dp
   sw47=0.0_dp
   sw48=0.0_dp
   sw49=0.0_dp
   sw80=0.0_dp
   sw81=0.0_dp
   sw82=0.0_dp
   sw83=0.0_dp
   sw84=0.0_dp
   sw85=0.0_dp
   sw86=0.0_dp
   sw87=0.0_dp
   sw88=0.0_dp
   sw89=0.0_dp
   tertperiod=0.0_dp
   tertt0=0.0_dp
   tertecos=0.0_dp
   omega8=0.0_dp
   tertesin=0.0_dp
   tertincl=0.0_dp
   tertomega=0.0_dp
   tertq=0.0_dp
   tertconj=0.0_dp
   tertratrad=0.0_dp
   hh=0.0_dp
   sw72=0.0_dp
   sw73=0.0_dp
   p2tconj=0.0_dp
   p2period=0.0_dp
   p2t0=0.0_dp
   p2ecos=0.0_dp
   p2esin=0.0_dp
   p2incl=0.0_dp
   p2omega=0.0_dp
   p2q=0.0_dp
   p2ratrad=0.0_dp
   p3tconj=0.0_dp
   p3period=0.0_dp
   p3t0=0.0_dp
   p3ecos=0.0_dp
   p3esin=0.0_dp
   p3incl=0.0_dp
   p3omega=0.0_dp
   p3q=0.0_dp
   p3ratrad=0.0_dp
   p4tconj=0.0_dp
   p4period=0.0_dp
   p4t0=0.0_dp
   p4ecos=0.0_dp
   p4esin=0.0_dp
   p4incl=0.0_dp
   p4omega=0.0_dp
   p4q=0.0_dp
   omega7=0.0_dp
   p4ratrad=0.0_dp
   p5tconj=0.0_dp
   p5period=0.0_dp
   p5t0=0.0_dp
   p5ecos=0.0_dp
   p5esin=0.0_dp
   p5incl=0.0_dp
   p5omega=0.0_dp
   p5q=0.0_dp
   p5ratrad=0.0_dp
   p6tconj=0.0_dp
   p6period=0.0_dp
   p6t0=0.0_dp
   p6ecos=0.0_dp
   p6esin=0.0_dp
   p6incl=0.0_dp
   p6omega=0.0_dp
   p6q=0.0_dp
   p6ratrad=0.0_dp
   p7tconj=0.0_dp
   p7period=0.0_dp
   p7t0=0.0_dp
   p7ecos=0.0_dp
   p7esin=0.0_dp
   p7incl=0.0_dp
   p7omega=0.0_dp
   p7q=0.0_dp
   p7ratrad=0.0_dp
   p8tconj=0.0_dp
   p8period=0.0_dp
   p8t0=0.0_dp
   p8ecos=0.0_dp
   p8esin=0.0_dp
   p8incl=0.0_dp
   p8omega=0.0_dp
   p8q=0.0_dp
   p8ratrad=0.0_dp
   secmass=0.0_dp
   rk3=0.0_dp
   rk4=0.0_dp
   rk5=0.0_dp
   rk6=0.0_dp
   rk7=0.0_dp
   rk8=0.0_dp
   rk9=0.0_dp
   rk10=0.0_dp
   t6=0.0_dp
   g6=0.0_dp
   omega5=0.0_dp
   g7=0.0_dp
   t7=0.0_dp
   t8=0.0_dp
   g8=0.0_dp
   omega3=0.0_dp
   t9=0.0_dp
   g9=0.0_dp
   t10=0.0_dp
   g10=0.0_dp
   secrad=0.0_dp
   omega4=0.0_dp
   radsum=0.0_dp
   raddiff=0.0_dp
   masssum=0.0_dp
   massdiff=0.0_dp
   p1ptc=0.0_dp
   p1mtc=0.0_dp
   p2ptc=0.0_dp
   p2mtc=0.0_dp
   p5ptc=0.0_dp
   p5mtc=0.0_dp
   p6ptc=0.0_dp
   p6mtc=0.0_dp
   p7ptc=0.0_dp
   p7mtc=0.0_dp
   p8ptc=0.0_dp
   p8mtc=0.0_dp
   p3ptc=0.0_dp
   p3mtc=0.0_dp
   pbptc=0.0_dp
   pbmtc=0.0_dp
   p4ptc=0.0_dp
   p4mtc=0.0_dp
   bigi4=0.0_dp
   bigbeta4=0.0_dp
   b2masssum=0.0_dp
   b2massdiff=0.0_dp
   bin2q=0.0_dp
   b2radsum=0.0_dp
   b2raddiff=0.0_dp
   bin2ratrad=0.0_dp
   bigi2=0.0_dp
   bigbeta2=0.0_dp
   bigi3=0.0_dp
   bigbeta3=0.0_dp
   fake=0.0_dp
   omega6=0.0_dp
   omega10=0.0_dp
   fracsum=0.0_dp
   fracdiff=0.0_dp
   bin2m3=0.0_dp
   bin2m4=0.0_dp
   bin2r3=0.0_dp
   bin2r4=0.0_dp
   sqecos=0.0_dp
   sqesin=0.0_dp
   sqtertecos=0.0_dp
   sqtertesin=0.0_dp
   sqp2ecos=0.0_dp
   sqp2esin=0.0_dp
   sqp3ecos=0.0_dp
   sqp3esin=0.0_dp
   sqp4ecos=0.0_dp
   sqp4esin=0.0_dp
   sqp5ecos=0.0_dp
   sqp5esin=0.0_dp
   sqp6ecos=0.0_dp
   sqp6esin=0.0_dp
   sqp7ecos=0.0_dp
   sqp7esin=0.0_dp
   sqp8ecos=0.0_dp
   sqp8esin=0.0_dp
   angsum1=0.0_dp
   angdiff1=0.0_dp
   angsum2=0.0_dp
   angdiff2=0.0_dp
   angsum3=0.0_dp
   angdiff3=0.0_dp
   angsum4=0.0_dp
   angdiff4=0.0_dp
   angsum5=0.0_dp
   angdiff5=0.0_dp
   angsum6=0.0_dp
   angdiff6=0.0_dp
   angsum7=0.0_dp
   angdiff7=0.0_dp
   angsum8=0.0_dp
   angdiff8=0.0_dp
   binqtc=-99.0_dp
   p1qtc=-99.0_dp
   p2qtc=-99.0_dp
   p3qtc=-99.0_dp
   p4qtc=-99.0_dp
   p5qtc=-99.0_dp
   p6qtc=-99.0_dp
   p7qtc=-99.0_dp
   p8qtc=-99.0_dp
   tbinoff=0.0_dp
   t1off=0.0_dp
   t2off=0.0_dp
   t3off=0.0_dp
   t4off=0.0_dp
   t5off=0.0_dp
   t6off=0.0_dp
   t7off=0.0_dp
   t8off=0.0_dp
!
   DO i=1,8
      dbolx(i,1)=0.6_dp
      dbolx(i,2)=0.4_dp
      wave(i)=0.0_dp
      sdarkint1(i)=0.0_dp
      sdarkint2(i)=0.0_dp
      sdarkint3(i)=0.0_dp
      sdarkint4(i)=0.0_dp
      sdarkint5(i)=0.0_dp
      sdarkint6(i)=0.0_dp
      sdarkint7(i)=0.0_dp
      sdarkint8(i)=0.0_dp
      sdarkint9(i)=0.0_dp
      sdarkint10(i)=0.0_dp
      dwavex(i,1)=0.64_dp
      dwavey(i,1)=0.46_dp
      dwavex(i,2)=0.64_dp
      dwavey(i,2)=0.46_dp
      dwavex(i,3)=0.64_dp
      dwavey(i,3)=0.46_dp
      dwavex(i,4)=0.64_dp
      dwavey(i,4)=0.46_dp
      dwavex(i,5)=0.64_dp
      dwavey(i,5)=0.46_dp
      dwavex(i,6)=0.64_dp
      dwavey(i,6)=0.46_dp
      dwavex(i,7)=0.342_dp
      dwavey(i,7)=0.721_dp
      dwavex(i,8)=0.123_dp
      dwavey(i,8)=0.456_dp
      dwavex(i,9)=0.456_dp
      dwavey(i,9)=0.789_dp
      dwavex(i,10)=0.987_dp
      dwavey(i,10)=0.654_dp
      powercoeff(i,1)=0.0_dp
      powercoeff(i,2)=0.0_dp
      powercoeff(i,3)=0.0_dp
      powercoeff(i,4)=0.0_dp
      powercoeff(i,5)=0.0_dp
      powercoeff(i,6)=0.0_dp
      powercoeff(i,7)=0.0_dp
      powercoeff(i,8)=0.0_dp
      powercoeff(i,9)=0.0_dp
   END DO
!
   DO i=1,4
      spot1parm(1,i)=0.0_dp
      spot1parm(2,i)=0.0_dp
      spot2parm(1,i)=0.0_dp
      spot2parm(2,i)=0.0_dp
      spotdparm(1,i)=0.0_dp
      spotdparm(2,i)=0.0_dp
   END DO

!
   ios=0
   OPEN(UNIT=1,FILE=ff,STATUS='old',ERR=10,IOSTAT=ios)
!
   READ(1,40)blank
   blank=TRIM(blank)
!
!  Determine if we read a version 6 style input, or if we
!  attempt to read the old style files.
!
   IF(blank(1:3) == '#1')iversion=0
   IF(blank(1:3) == '#2')iversion=1
   IF((blank(1:3) /= '#1').AND.(blank(1:3) /= '#2'))THEN
      CLOSE(1)
!
!   if KKKK = 0, then read ELC.inp and ELCbody3.inp
!
      IF(kkkk == 0)THEN
         CALL getinput(nalph1,nbet1,nalph2,nbet2,fill1,fill2,  &
            omega1,omega2,dphase,q,finc,teff1,teff2,tgrav1,tgrav2,  &
            betarim,rinner,router,tdisk,xi,ntheta,nradius,alb1,alb2,  &
            nref,rlx,period,fm,separ,gamma,t3,g3,sa3,density,sw1,sw2,  &
            sw3,t0,idraw,iecheck,iidint,iatm,ism1,icnu,icnb,icnv,  &
            icnr,icni,icnj,icnh,icnk,irvfilt,isw1,isw2,isw3,isw4,  &
            ilaw,wave,dbolx,dboly,dwavex,dwavey,ecc,argper,pshift,  &
            sw5,sw6,sw7,sw8,sw9,ikeep,isynch,isw5,isw6,isw7,isw8,  &
            isw9,spot1parm,spot2parm,spotdparm,primmass,primk,  &
            primrad,ratrad,frac1,frac2,ecosw,temprat,idark1,idark2,  &
            isw12,isw13,isw21,isw22,isw23,isw24,bigi,bigbeta,sw23,  &
            sw24,powercoeff,sw25,sw26,sw27,sw28,sw29,sw30,contam,  &
            tconj,beam1,beam2,isw25,isw26,isw27,isw28,isw29,isw30,  &
            isw31,isw32,isw33,isw34,ocose,osine,omegadot,contams0,  &
            contams1,contams2,contams3,sw47,sw48,sw49,sw80,sw81,sw82,  &
            sw83,sw84,sw85,sw86,sw87,sw88,sw89,isw80,isw81,isw82,  &
            isw83,isw84,isw85,isw86,isw87,isw88,isw89,sdarkint1,  &
            sdarkint2,sdarkint3,sdarkint4,sdarkint5)
!
!   If isw30 >= 1, then load the third body parameters
!
         IF((isw30 >= 1).AND.(isw7 < 2))THEN
            WRITE(*,*)'Error:  set itime=2 if body 3 switch', ' is used'
            STOP
         END IF
         IF((isw30 >= 1).AND.(isw7 >= 2))THEN
            CALL getbody3(nalph3,nbet3,tertperiod,tertt0,tertecos,  &
               tertesin,tertincl,tertomega,tertq,dwavex,dwavey,itconj,  &
               it1,it2,it3,it4,tertconj,tertratrad,hh,sw72,sw73,  &
               p2tconj,p2period,p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,  &
               p2ratrad,p3tconj,p3period,p3t0,p3ecos,p3esin,p3incl,  &
               p3omega,p3q,p3ratrad,p4tconj,p4period,p4t0,p4ecos,  &
               p4esin,p4incl,p4omega,p4q,p4ratrad,p5tconj,p5period,  &
               p5t0,p5ecos,p5esin,p5incl,p5omega,p5q,p5ratrad,p6tconj,  &
               p6period,p6t0,p6ecos,p6esin,p6incl,p6omega,p6q,  &
               p6ratrad,p7tconj,p7period,p7t0,p7ecos,p7esin,p7incl,  &
               p7omega,p7q,p7ratrad,p8tconj,p8period,p8t0,p8ecos,  &
               p8esin,p8incl,p8omega,p8q,p8ratrad,isw30)
!
         END IF
      END IF
!
!   if KKKK > 0, then read ELC.1??? and ELCbody3.1???
!
      IF(kkkk > 0)THEN
         CALL getgridinput(kkkk,nalph1,nbet1,nalph2,nbet2,fill1,  &
            fill2,omega1,omega2,dphase,q,finc,teff1,teff2,tgrav1,  &
            tgrav2,betarim,rinner,router,tdisk,xi,ntheta,nradius,  &
            alb1,alb2,nref,rlx,period,fm,separ,gamma,t3,g3,sa3,  &
            density,sw1,sw2,sw3,t0,idraw,iecheck,iidint,iatm,ism1,  &
            icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk,irvfilt,isw1,  &
            isw2,isw3,isw4,ilaw,wave,dbolx,dboly,dwavex,dwavey,ecc,  &
            argper,pshift,sw5,sw6,sw7,sw8,sw9,ikeep,isynch,isw5,isw6,  &
            isw7,isw8,isw9,spot1parm,spot2parm,spotdparm,primmass,  &
            primk,primrad,ratrad,frac1,frac2,ecosw,temprat,idark1,  &
            idark2,isw12,isw13,isw21,isw22,isw23,isw24,bigi,bigbeta,  &
            sw23,sw24,powercoeff,sw25,sw26,sw27,sw28,sw29,sw30,  &
            contam,tconj,beam1,beam2,isw25,isw26,isw27,isw28,isw29,  &
            isw30,isw31,isw32,isw33,isw34,ocose,osine,omegadot,  &
            contams0,contams1,contams2,contams3,sw47,sw48,sw49,sw80,  &
            sw81,sw82,sw83,sw84,sw85,sw86,sw87,sw88,sw89,isw80,isw81,  &
            isw82,isw83,isw84,isw85,isw86,isw87,isw88,isw89,  &
            sdarkint1,sdarkint2,sdarkint3,sdarkint4,sdarkint5)
!
!   If isw30 >= 1, then load the third body parameters
!
         IF((isw30 >= 1).AND.(isw7 < 2))THEN
            WRITE(*,*) 'Error:  set itime=2 if body 3 switch', ' is used'
            STOP
         END IF
         IF((isw30 >= 1).AND.(isw7 >= 2))THEN
            CALL getgridbody3(kkkk,nalph3,nbet3,tertperiod,tertt0,  &
               tertecos,tertesin,tertincl,tertomega,tertq,dwavex,  &
               dwavey,itconj,it1,it2,it3,it4,tertconj,tertratrad,hh,  &
               sw72,sw73,p2tconj,p2period,p2t0,p2ecos,p2esin,p2incl,  &
               p2omega,p2q,p2ratrad,p3tconj,p3period,p3t0,p3ecos,  &
               p3esin,p3incl,p3omega,p3q,p3ratrad,p4tconj,p4period,  &
               p4t0,p4ecos,p4esin,p4incl,p4omega,p4q,p4ratrad,p5tconj,  &
               p5period,p5t0,p5ecos,p5esin,p5incl,p5omega,p5q,  &
               p5ratrad,p6tconj,p6period,p6t0,p6ecos,p6esin,p6incl,  &
               p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,p7ecos,  &
               p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,  &
               p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad,isw30)
!
         END IF
      END IF
      RETURN
   END IF
!
!  We have a version 6 style input file, so start reading.
!  The various subroutines will scan for comment lines and
!  report errors for the specific variable if needed.
!
   CALL rdint(1,iunit,'iunit',ff)
   CALL rdint(1,isw7,'itime',ff)
   CALL rdreal(1,sw23,'t_start',ff)
   CALL rdreal(1,sw24,'t_end',ff)
   CALL rdreal(1,sw9,'time step',ff)
   IF((sw9 <= 0.0_dp).and.(isw7 >= 2))THEN
      WRITE(*,25)bell
   END IF
   CALL rdreal(1,dphase,'dphase',ff)
   CALL rdint(1,iatm,'iatm',ff)
   CALL rdint8(1,icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk,'icn',ff)
   CALL rdint(1,irvfilt,'iRVfilt',ff)
   CALL rdint(1,ilaw,'ilaw',ff)
   CALL rdreal(1,wave(1),'wavelength filter 1',ff)
   CALL rdreal(1,wave(2),'wavelength filter 2',ff)
   CALL rdreal(1,wave(3),'wavelength filter 3',ff)
   CALL rdreal(1,wave(4),'wavelength filter 4',ff)
   CALL rdreal(1,wave(5),'wavelength filter 5',ff)
   CALL rdreal(1,wave(6),'wavelength filter 6',ff)
   CALL rdreal(1,wave(7),'wavelength filter 7',ff)
   CALL rdreal(1,wave(8),'wavelength filter 8',ff)
   CALL rdint(1,nref,'Nref',ff)
   CALL rdreal(1,rlx,'log10(Lx)',ff)
   CALL rdint(1,isw25,'X-ray foreshortening switch',ff)
   CALL rdint(1,idark1,'idark1',ff)
   CALL rdint(1,idark2,'idark2',ff)
   CALL rdint(1,isw5,'ispotprof',ff)
   CALL rdint(1,isw6,'igrav',ff)
   CALL rdint(1,isw86,'flux switch',ff)
   CALL rdreal(1,t3,'T3',ff)
   CALL rdreal(1,g3,'g3',ff)
   CALL rdreal(1,sa3,'SA3',ff)
   CALL rdint(1,isw27,'Nterms',ff)
   CALL rdint(1,isw33,'Mandel',ff)
   CALL rdint(1,isw31,'Ngap',ff)
   CALL rdint(1,iecheck,'iecheck',ff)
   CALL rdint(1,ism1,'ism1',ff)
   CALL rdint(1,isw13,'ifasttrans',ff)
   CALL rdint(1,isw8,'MonteCarlo',ff)
   CALL rdreal(1,sw7,'phaselow',ff)
   CALL rdreal(1,sw8,'phasehigh',ff)
   CALL rdint(1,isw2,'isquare',ff)
   CALL rdint(1,isw3,'iusepot',ff)
   CALL rdreal(1,sw2,'usepot1',ff)
   CALL rdreal(1,sw3,'iusepot2',ff)
   CALL rdint(1,idraw,'idraw',ff)
   CALL rdint(1,isw1,'ionephase',ff)
   CALL rdreal(1,sw1,'onephase',ff)
   CALL rdreal(1,pshift,'pshift',ff)
   CALL rdint(1,ikeep,'ikeep',ff)
   CALL rdint(1,isynch,'isynch',ff)
   CALL rdint(1,isw21,'ialign',ff)
   CALL rdint(1,isw28,'Tconj switch',ff)
   CALL rdint(1,isw29,'e*cos(omega), e*sin(omega) switch',ff)
   CALL rdint(1,imag,'imag',ff)
   CALL rdint(1,isw9,'ieliete',ff)
   CALL rdint(1,isw4,'ifixgamma',ff)
   CALL rdint(1,isw23,'iwriteeclipse',ff)
   CALL rdint(1,isw24,'frac switch',ff)
   CALL rdint(1,isw22,'screen output supress switch',ff)
   CALL rdint(1,it4,'output files supress switch',ff)
   CALL rdreal(1,sw48,'chi^2 threshold',ff)
   CALL rdreal(1,sw6,'median fit',ff)
   CALL rdint(1,isw32,'jdum',ff)
   CALL rdint(1,isw88,'Ndynwin',ff)
   CALL rdreal(1,sw47,'Tref',ff)
   CALL rdreal(1,hh,'hh',ff)
   CALL rdint(1,isw26,'iGR',ff)
   CALL rdint(1,isw80,'binary+binary switch',ff)
   CALL rdint(1,itconj,'itconj',ff)
   CALL rdint(1,it1,'logarithmic mass ratios switch',ff)
   CALL rdint(1,isw100,'planet radii switch',ff)
   CALL rdint(1,it3,'transit treatment switch',ff)
   CALL rdint(1,isw81,'transit penalty switch',ff)
   CALL rdint(1,isw85,'secondary eclipse penalty switch',ff)
   CALL rdreal(1,sw84,'chi^2 penalty',ff)
   CALL rdint(1,it2,'informational output switch',ff)
   CALL rdreal(1,contam,'contam',ff)
   CALL rdint(1,isw34,'Iseason',ff)
   CALL rdreal(1,contams0,'contamS0',ff)
   CALL rdreal(1,contams1,'contamS1',ff)
   CALL rdreal(1,contams2,'contamS2',ff)
   CALL rdreal(1,contams3,'contamS3',ff)
   CALL rdint(1,nseg,'Nseg',ff)
   CALL rdint(1,isw87,'fast Kepler binning switch',ff)
   CALL rdreal(1,sw29,'LC bin size',ff)
   CALL rdreal(1,sw30,'RV bin size',ff)
   CALL rdint(1,isw89,'NSC',ff)
!
!  Add the part about TESS options here, if iversion=1
!
   IF(iversion == 1)THEN
      CALL rdreal(1,tesscontam,'tesscontam',ff)
      CALL rdint(1,tessfilt,'tessfilt',ff)
      CALL rdreal(1,tessbin,'tessbin',ff)
   END IF
!
   CALL rdint(1,isw30,'Nbody',ff)
   CALL rdreal(1,period,'Period',ff)
   CALL rdreal(1,t0,'T0',ff)
   CALL rdreal(1,tconj,'Tconj',ff)
   CALL rdreal(1,pbptc,'PbpTc',ff)
   CALL rdreal(1,pbmtc,'PbmTc',ff)
   CALL rdreal(1,binqtc,'binqTc',ff)
   CALL rdreal(1,tbinoff,'Tbinoff',ff)
   CALL rdreal(1,ecc,'eccentricity',ff)
   CALL rdreal(1,argper,'argper',ff)
   CALL rdreal(1,ocose,'ocose',ff)
   CALL rdreal(1,osine,'osine',ff)
   CALL rdreal(1,sqecos,'sqrt(e)*cos(omega)',ff)
   CALL rdreal(1,sqesin,'sqrt(e)*sin(omega)',ff)
   CALL rdreal(1,finc,'finc',ff)
   CALL rdreal(1,sw49,'Omega_bin',ff)
   CALL rdreal(1,primk,'primK',ff)
   CALL rdreal(1,separ,'separ',ff)
   CALL rdreal(1,gamma,'gamma',ff)
   CALL rdreal(1,ecosw,'ecosw',ff)
   CALL rdint2(1,nalph1,nbet1,'Nalph1,Nbet1',ff)
   CALL rdreal(1,teff1,'Teff1',ff)
   CALL rdreal(1,tgrav1,'Tgrav1',ff)
   CALL rdreal(1,alb1,'alb1',ff)
   CALL rdreal(1,omega1,'omega1',ff)
   CALL rdreal(1,fill1,'fill1',ff)
   CALL rdreal(1,primrad,'primrad',ff)
   CALL rdreal(1,frac1,'frac1',ff)
   CALL rdreal(1,sw27,'radfill1',ff)
   CALL rdreal(1,primmass,'primmass',ff)
   CALL rdreal(1,bigi,'bigI',ff)
   CALL rdreal(1,bigbeta,'bigbeta',ff)
   CALL rdreal2(1,dwavex(1,1),dwavey(1,1),'l.d. 1, band 1',ff)
   CALL rdreal2(1,dwavex(2,1),dwavey(2,1),'l.d. 1, band 2',ff)
   CALL rdreal2(1,dwavex(3,1),dwavey(3,1),'l.d. 1, band 3',ff)
   CALL rdreal2(1,dwavex(4,1),dwavey(4,1),'l.d. 1, band 4',ff)
   CALL rdreal2(1,dwavex(5,1),dwavey(5,1),'l.d. 1, band 5',ff)
   CALL rdreal2(1,dwavex(6,1),dwavey(6,1),'l.d. 1, band 6',ff)
   CALL rdreal2(1,dwavex(7,1),dwavey(7,1),'l.d. 1, band 7',ff)
   CALL rdreal2(1,dwavex(8,1),dwavey(8,1),'l.d. 1, band 8',ff)
   CALL rdreal2(1,dbolx(1,1),dboly(1,1),'bolo. l.d. 1',ff)
   CALL rdreal(1,spot1parm(1,1),'spot1parm 1,1',ff)
   CALL rdreal(1,spot1parm(1,2),'spot1parm 1,2',ff)
   CALL rdreal(1,spot1parm(1,3),'spot1parm 1,3',ff)
   CALL rdreal(1,spot1parm(1,4),'spot1parm 1,4',ff)
   CALL rdreal(1,spot1parm(2,1),'spot1parm 2,1',ff)
   CALL rdreal(1,spot1parm(2,2),'spot1parm 2,2',ff)
   CALL rdreal(1,spot1parm(2,3),'spot1parm 2,3',ff)
   CALL rdreal(1,spot1parm(2,4),'spot1parm 2,4',ff)
   CALL rdreal(1,beam1,'beam1',ff)
   CALL rdreal(1,sw72,'rk1',ff)
   CALL rdreal(1,sdarkint1(1),'flux 1, band 1',ff)
   CALL rdreal(1,sdarkint1(2),'flux 1, band 2',ff)
   CALL rdreal(1,sdarkint1(3),'flux 1, band 3',ff)
   CALL rdreal(1,sdarkint1(4),'flux 1, band 4',ff)
   CALL rdreal(1,sdarkint1(5),'flux 1, band 5',ff)
   CALL rdreal(1,sdarkint1(6),'flux 1, band 6',ff)
   CALL rdreal(1,sdarkint1(7),'flux 1, band 7',ff)
   CALL rdreal(1,sdarkint1(8),'flux 1, band 8',ff)
   CALL rdint2(1,nalph2,nbet2,'Nalph2',ff)
   CALL rdreal(1,teff2,'Teff2',ff)
   CALL rdreal(1,temprat,'temprat',ff)
   CALL rdreal(1,tgrav2,'Tgrav2',ff)
   CALL rdreal(1,alb2,'alb2',ff)
   CALL rdreal(1,omega2,'omega2',ff)
   CALL rdreal(1,fill2,'fill2',ff)
   CALL rdreal(1,fillsum,'fillsum',ff)
   CALL rdreal(1,filldiff,'filldiff',ff)
   CALL rdreal(1,secrad,'secrad',ff)
   CALL rdreal(1,frac2,'frac2',ff)
   CALL rdreal(1,ratrad,'ratrad',ff)
   CALL rdreal(1,radsum,'radsum',ff)
   CALL rdreal(1,raddiff,'raddiff',ff)
   CALL rdreal(1,fracsum,'fracsum',ff)
   CALL rdreal(1,fracdiff,'fracdiff',ff)
   CALL rdreal(1,sw28,'radfill2',ff)
   CALL rdreal(1,q,'Q',ff)
   CALL rdreal(1,secmass,'secmass',ff)
   CALL rdreal(1,masssum,'masssum',ff)
   CALL rdreal(1,massdiff,'massdiff',ff)
   CALL rdreal(1,bigi2,'axis_I2',ff)
   CALL rdreal(1,bigbeta2,'axis_beta2',ff)
   CALL rdreal2(1,dwavex(1,2),dwavey(1,2),'l.d. 2 band 1',ff)
   CALL rdreal2(1,dwavex(2,2),dwavey(2,2),'l.d. 2 band 2',ff)
   CALL rdreal2(1,dwavex(3,2),dwavey(3,2),'l.d. 2 band 3',ff)
   CALL rdreal2(1,dwavex(4,2),dwavey(4,2),'l.d. 2 band 4',ff)
   CALL rdreal2(1,dwavex(5,2),dwavey(5,2),'l.d. 2 band 5',ff)
   CALL rdreal2(1,dwavex(6,2),dwavey(6,2),'l.d. 2 band 6',ff)
   CALL rdreal2(1,dwavex(7,2),dwavey(7,2),'l.d. 2 band 7',ff)
   CALL rdreal2(1,dwavex(8,2),dwavey(8,2),'l.d. 2 band 8',ff)
   CALL rdreal2(1,dbolx(1,2),dboly(1,2),'bolo. l.d. 2',ff)
   CALL rdreal(1,spot2parm(1,1),'spot2 parm 1,1 ',ff)
   CALL rdreal(1,spot2parm(1,2),'spot2 parm 1,2 ',ff)
   CALL rdreal(1,spot2parm(1,3),'spot2 parm 1,3 ',ff)
   CALL rdreal(1,spot2parm(1,4),'spot2 parm 1,4 ',ff)
   CALL rdreal(1,spot2parm(2,1),'spot2 parm 2,1 ',ff)
   CALL rdreal(1,spot2parm(2,2),'spot2 parm 2,2 ',ff)
   CALL rdreal(1,spot2parm(2,3),'spot2 parm 2,3 ',ff)
   CALL rdreal(1,spot2parm(2,4),'spot2 parm 2,4 ',ff)
   CALL rdreal(1,beam2,'beam2',ff)
   CALL rdreal(1,sw73,'rk2',ff)
   CALL rdreal(1,sdarkint2(1),'flux 2, band 1',ff)
   CALL rdreal(1,sdarkint2(2),'flux 2, band 2',ff)
   CALL rdreal(1,sdarkint2(3),'flux 2, band 3',ff)
   CALL rdreal(1,sdarkint2(4),'flux 2, band 4',ff)
   CALL rdreal(1,sdarkint2(5),'flux 2, band 5',ff)
   CALL rdreal(1,sdarkint2(6),'flux 2, band 6',ff)
   CALL rdreal(1,sdarkint2(7),'flux 2, band 7',ff)
   CALL rdreal(1,sdarkint2(8),'flux 2, band 8',ff)
   CALL rdint(1,iidint,'idint',ff)
   CALL rdint(1,ntheta,'Ntheta',ff)
   CALL rdint(1,nradius,'Nradius',ff)
   CALL rdreal(1,betarim,'betarim',ff)
   CALL rdreal(1,rinner,'rinner',ff)
   CALL rdreal(1,router,'router',ff)
   CALL rdreal(1,tdisk,'tdisk',ff)
   CALL rdreal(1,xi,'xi',ff)
   CALL rdreal(1,spotdparm(1,1),'disk spot 1,1',ff)
   CALL rdreal(1,spotdparm(1,2),'disk spot 1,2',ff)
   CALL rdreal(1,spotdparm(1,3),'disk spot 1,3',ff)
   CALL rdreal(1,spotdparm(1,4),'disk spot 1,4',ff)
   CALL rdreal(1,spotdparm(2,1),'disk spot 2,1',ff)
   CALL rdreal(1,spotdparm(2,2),'disk spot 2,2',ff)
   CALL rdreal(1,spotdparm(2,3),'disk spot 2,3',ff)
   CALL rdreal(1,spotdparm(2,4),'disk spot 2,4',ff)
   CALL rdreal(1,sw26,'reference phase disk fraction',ff)
   CALL rdreal(1,sw5,'asin(i)',ff)
   CALL rdreal(1,sw25,'asin(i) error',ff)
!
   DO i=2,8
      dbolx(i,1)=dbolx(1,1)
      dboly(i,1)=dboly(1,1)
      dbolx(i,2)=dbolx(1,2)
      dboly(i,2)=dboly(1,2)
   END DO
!
!  If Nbody < 3, then we are done.
!
   IF(isw30 < 3)THEN
      CLOSE(1)
      RETURN
   END IF
!
   CALL rdreal(1,tertconj,'P1Tconj',ff)
   CALL rdreal(1,tertperiod,'P1period',ff)
   CALL rdreal(1,p1ptc,'P1pTc',ff)
   CALL rdreal(1,p1mtc,'P1mTc',ff)
   CALL rdreal(1,p1qtc,'P1qTc',ff)
   CALL rdreal(1,t1off,'T1off',ff)
   CALL rdreal(1,tertt0,'P1T0',ff)
   CALL rdreal(1,tertecos,'P1e*cos',ff)
   CALL rdreal(1,tertesin,'P1e*sin',ff)
   CALL rdreal(1,sqtertecos,'P1sqrt(e)*cos',ff)
   CALL rdreal(1,sqtertesin,'P1sqrt(e)*sin',ff)
   CALL rdreal(1,tertincl,'P1incl',ff)
   CALL rdreal(1,tertomega,'P1Omega',ff)
   CALL rdreal(1,angsum1,'angsum1',ff)
   CALL rdreal(1,angdiff1,'angdiff1',ff)
   CALL rdint2(1,nalph3,nbet3,'Nalph3',ff)
   CALL rdreal(1,t3,'T3',ff)
   CALL rdreal(1,g3,'g3',ff)
   CALL rdreal(1,tertratrad,'P1ratrad',ff)
   CALL rdreal(1,tertq,'P1Q',ff)
   CALL rdreal(1,omega3,'omega3',ff)
   CALL rdreal(1,bigi3,'axis_I3',ff)
   CALL rdreal(1,bigbeta3,'axis_beta3',ff)
   CALL rdreal2(1,dwavex(1,3),dwavey(1,3),' l.d. 3, band 1',ff)
   CALL rdreal2(1,dwavex(2,3),dwavey(2,3),' l.d. 3, band 2',ff)
   CALL rdreal2(1,dwavex(3,3),dwavey(3,3),' l.d. 3, band 3',ff)
   CALL rdreal2(1,dwavex(4,3),dwavey(4,3),' l.d. 3, band 4',ff)
   CALL rdreal2(1,dwavex(5,3),dwavey(5,3),' l.d. 3, band 5',ff)
   CALL rdreal2(1,dwavex(6,3),dwavey(6,3),' l.d. 3, band 6',ff)
   CALL rdreal2(1,dwavex(7,3),dwavey(7,3),' l.d. 3, band 7',ff)
   CALL rdreal2(1,dwavex(8,3),dwavey(8,3),' l.d. 3, band 8',ff)
   CALL rdreal(1,rk3,'rk3',ff)
   CALL rdreal(1,sdarkint3(1),'flux 3, band 1',ff)
   CALL rdreal(1,sdarkint3(2),'flux 3, band 2',ff)
   CALL rdreal(1,sdarkint3(3),'flux 3, band 3',ff)
   CALL rdreal(1,sdarkint3(4),'flux 3, band 4',ff)
   CALL rdreal(1,sdarkint3(5),'flux 3, band 5',ff)
   CALL rdreal(1,sdarkint3(6),'flux 3, band 6',ff)
   CALL rdreal(1,sdarkint3(7),'flux 3, band 7',ff)
   CALL rdreal(1,sdarkint3(8),'flux 3, band 8',ff)
!
!  If Nbody < 4, then we are done.
!
   IF(isw30 < 4)THEN
      CLOSE(1)
      RETURN
   END IF
!
   CALL rdreal(1,p2tconj,'P2tconj',ff)
   CALL rdreal(1,p2period,'P2period',ff)
   CALL rdreal(1,p2ptc,'P2pTc',ff)
   CALL rdreal(1,p2mtc,'P2mTc',ff)
   CALL rdreal(1,p2qtc,'P2qTc',ff)
   CALL rdreal(1,t2off,'T2off',ff)
   CALL rdreal(1,p2t0,'P2T0',ff)
   CALL rdreal(1,p2ecos,'P2ecos',ff)
   CALL rdreal(1,p2esin,'P2esin',ff)
   CALL rdreal(1,sqp2ecos,'P2sqrt(e)cos',ff)
   CALL rdreal(1,sqp2esin,'P2sqrt(e)sin',ff)
   CALL rdreal(1,p2incl,'P2incl',ff)
   CALL rdreal(1,p2omega,'P2Omega',ff)
   CALL rdreal(1,angsum2,'angsum2',ff)
   CALL rdreal(1,angdiff2,'angdiff2',ff)
   CALL rdreal(1,sw80,'Teff4',ff)
   CALL rdreal(1,sw82,'g4',ff)
   CALL rdreal(1,p2ratrad,'P2ratrad',ff)
   CALL rdreal(1,p2q,'P2Q',ff)
   CALL rdreal(1,omega4,'omega4',ff)
   CALL rdreal(1,bigi4,'axis_I4',ff)
   CALL rdreal(1,bigbeta4,'axis_beta4',ff)
   CALL rdreal2(1,dwavex(1,4),dwavey(1,4),'l.d. 4, band 1',ff)
   CALL rdreal2(1,dwavex(2,4),dwavey(2,4),'l.d. 4, band 2',ff)
   CALL rdreal2(1,dwavex(3,4),dwavey(3,4),'l.d. 4, band 3',ff)
   CALL rdreal2(1,dwavex(4,4),dwavey(4,4),'l.d. 4, band 4',ff)
   CALL rdreal2(1,dwavex(5,4),dwavey(5,4),'l.d. 4, band 5',ff)
   CALL rdreal2(1,dwavex(6,4),dwavey(6,4),'l.d. 4, band 6',ff)
   CALL rdreal2(1,dwavex(7,4),dwavey(7,4),'l.d. 4, band 7',ff)
   CALL rdreal2(1,dwavex(8,4),dwavey(8,4),'l.d. 4, band 8',ff)
   CALL rdreal(1,rk4,'rk4',ff)
   CALL rdreal(1,sdarkint4(1),'flux 4, band 1',ff)
   CALL rdreal(1,sdarkint4(2),'flux 4, band 2',ff)
   CALL rdreal(1,sdarkint4(3),'flux 4, band 3',ff)
   CALL rdreal(1,sdarkint4(4),'flux 4, band 4',ff)
   CALL rdreal(1,sdarkint4(5),'flux 4, band 5',ff)
   CALL rdreal(1,sdarkint4(6),'flux 4, band 6',ff)
   CALL rdreal(1,sdarkint4(7),'flux 4, band 7',ff)
   CALL rdreal(1,sdarkint4(8),'flux 4, band 8',ff)
   CALL rdreal(1,b2masssum,'bin2masssum',ff)
   CALL rdreal(1,b2massdiff,'bin2massdiff',ff)
   CALL rdreal(1,bin2q,'bin2Q',ff)
   CALL rdreal(1,b2radsum,'bin2radsum',ff)
   CALL rdreal(1,b2raddiff,'bin2raddiff',ff)
   CALL rdreal(1,bin2ratrad,'bin2ratrad',ff)
   CALL rdreal(1,bin2m3,'bin2M3',ff)
   CALL rdreal(1,bin2m4,'bin2M4',ff)
   CALL rdreal(1,bin2r3,'bin2R3',ff)
   CALL rdreal(1,bin2r4,'bin2R4',ff)
!
!  If Nbody < 5, then we are done.
!
   IF(isw30 < 5)THEN
      CLOSE(1)
      RETURN
   END IF
!
   CALL rdreal(1,p3tconj,'P3tconj',ff)
   CALL rdreal(1,p3period,'P3period',ff)
   CALL rdreal(1,p3ptc,'P3pTc',ff)
   CALL rdreal(1,p3mtc,'P3mTc',ff)
   CALL rdreal(1,p3qtc,'P3qTc',ff)
   CALL rdreal(1,t3off,'T3off',ff)
   CALL rdreal(1,p3t0,'P3T0',ff)
   CALL rdreal(1,p3ecos,'P3ecos',ff)
   CALL rdreal(1,p3esin,'P3esin',ff)
   CALL rdreal(1,sqp3ecos,'P3sqrt(e)cos',ff)
   CALL rdreal(1,sqp3esin,'P3sqrt(e)sin',ff)
   CALL rdreal(1,p3incl,'P3incl',ff)
   CALL rdreal(1,p3omega,'P3Omega',ff)
   CALL rdreal(1,angsum3,'angsum3',ff)
   CALL rdreal(1,angdiff3,'angdiff3',ff)
   CALL rdreal(1,sw81,'Teff5',ff)
   CALL rdreal(1,sw83,'g5',ff)
   CALL rdreal(1,p3ratrad,'P3ratrad',ff)
   CALL rdreal(1,p3q,'P3Q',ff)
   CALL rdreal(1,omega5,'omega5',ff)
   CALL rdreal(1,fake,'axis_I5',ff)
   CALL rdreal(1,fake,'axis_beta5',ff)
   CALL rdreal2(1,dwavex(1,5),dwavey(1,5),'l.d. 5,band 1',ff)
   CALL rdreal2(1,dwavex(2,5),dwavey(2,5),'l.d. 5,band 2',ff)
   CALL rdreal2(1,dwavex(3,5),dwavey(3,5),'l.d. 5,band 3',ff)
   CALL rdreal2(1,dwavex(4,5),dwavey(4,5),'l.d. 5,band 4',ff)
   CALL rdreal2(1,dwavex(5,5),dwavey(5,5),'l.d. 5,band 5',ff)
   CALL rdreal2(1,dwavex(6,5),dwavey(6,5),'l.d. 5,band 6',ff)
   CALL rdreal2(1,dwavex(7,5),dwavey(7,5),'l.d. 5,band 7',ff)
   CALL rdreal2(1,dwavex(8,5),dwavey(8,5),'l.d. 5,band 8',ff)
   CALL rdreal(1,rk5,'rk5',ff)
   CALL rdreal(1,sdarkint5(1),'flux 5, band 1',ff)
   CALL rdreal(1,sdarkint5(2),'flux 5, band 2',ff)
   CALL rdreal(1,sdarkint5(3),'flux 5, band 3',ff)
   CALL rdreal(1,sdarkint5(4),'flux 5, band 4',ff)
   CALL rdreal(1,sdarkint5(5),'flux 5, band 5',ff)
   CALL rdreal(1,sdarkint5(6),'flux 5, band 6',ff)
   CALL rdreal(1,sdarkint5(7),'flux 5, band 7',ff)
   CALL rdreal(1,sdarkint5(8),'flux 5, band 8',ff)
!
!  If Nbody < 6, then we are done.
!
   IF(isw30 < 6)THEN
      CLOSE(1)
      RETURN
   END IF
!
   CALL rdreal(1,p4tconj,'P4tconj',ff)
   CALL rdreal(1,p4period,'P4period',ff)
   CALL rdreal(1,p4ptc,'P4pTc',ff)
   CALL rdreal(1,p4mtc,'P4mTc',ff)
   CALL rdreal(1,p4qtc,'P4qTc',ff)
   CALL rdreal(1,t4off,'T4off',ff)
   CALL rdreal(1,p4t0,'P4T0',ff)
   CALL rdreal(1,p4ecos,'P4ecos',ff)
   CALL rdreal(1,p4esin,'P4esin',ff)
   CALL rdreal(1,sqp4ecos,'P4sqrt(e)cos',ff)
   CALL rdreal(1,sqp4esin,'P4sqrt(e)sin',ff)
   CALL rdreal(1,p4incl,'P4incl',ff)
   CALL rdreal(1,p4omega,'P4Omega',ff)
   CALL rdreal(1,angsum4,'angsum4',ff)
   CALL rdreal(1,angdiff4,'angdiff4',ff)
   CALL rdreal(1,t6,'t6',ff)
   CALL rdreal(1,g6,'g6',ff)
   CALL rdreal(1,p4ratrad,'P4ratrad',ff)
   CALL rdreal(1,p4q,'P4Q',ff)
   CALL rdreal(1,omega6,'omega6',ff)
   CALL rdreal(1,fake,'axis_I6',ff)
   CALL rdreal(1,fake,'axis_beta6',ff)
   CALL rdreal2(1,dwavex(1,6),dwavey(1,6),'l.d. 6, band 1',ff)
   CALL rdreal2(1,dwavex(2,6),dwavey(2,6),'l.d. 6, band 2',ff)
   CALL rdreal2(1,dwavex(3,6),dwavey(3,6),'l.d. 6, band 3',ff)
   CALL rdreal2(1,dwavex(4,6),dwavey(4,6),'l.d. 6, band 4',ff)
   CALL rdreal2(1,dwavex(5,6),dwavey(5,6),'l.d. 6, band 5',ff)
   CALL rdreal2(1,dwavex(6,6),dwavey(6,6),'l.d. 6, band 6',ff)
   CALL rdreal2(1,dwavex(7,6),dwavey(7,6),'l.d. 6, band 7',ff)
   CALL rdreal2(1,dwavex(8,6),dwavey(8,6),'l.d. 6, band 8',ff)
   CALL rdreal(1,rk6,'rk6',ff)
   CALL rdreal(1,sdarkint6(1),'flux 6, band 1',ff)
   CALL rdreal(1,sdarkint6(2),'flux 6, band 2',ff)
   CALL rdreal(1,sdarkint6(3),'flux 6, band 3',ff)
   CALL rdreal(1,sdarkint6(4),'flux 6, band 4',ff)
   CALL rdreal(1,sdarkint6(5),'flux 6, band 5',ff)
   CALL rdreal(1,sdarkint6(6),'flux 6, band 6',ff)
   CALL rdreal(1,sdarkint6(7),'flux 6, band 7',ff)
   CALL rdreal(1,sdarkint6(8),'flux 6, band 8',ff)
!
!  If Nbody < 7, then we are done.
!
   IF(isw30 < 7)THEN
      CLOSE(1)
      RETURN
   END IF
!
   CALL rdreal(1,p5tconj,'P5tconj',ff)
   CALL rdreal(1,p5period,'P5period',ff)
   CALL rdreal(1,p5ptc,'P5pTc',ff)
   CALL rdreal(1,p5mtc,'P5mTc',ff)
   CALL rdreal(1,p5qtc,'P5qTc',ff)
   CALL rdreal(1,t5off,'T5off',ff)
   CALL rdreal(1,p5t0,'P5T0',ff)
   CALL rdreal(1,p5ecos,'P5ecos',ff)
   CALL rdreal(1,p5esin,'P5esin',ff)
   CALL rdreal(1,sqp5ecos,'P5sqrt(e)cos',ff)
   CALL rdreal(1,sqp5esin,'P5sqrt(e)sin',ff)
   CALL rdreal(1,p5incl,'P5incl',ff)
   CALL rdreal(1,p5omega,'P5Omega',ff)
   CALL rdreal(1,angsum5,'angsum5',ff)
   CALL rdreal(1,angdiff5,'angdiff5',ff)
   CALL rdreal(1,t7,'t7',ff)
   CALL rdreal(1,g7,'g7',ff)
   CALL rdreal(1,p5ratrad,'P5ratrad',ff)
   CALL rdreal(1,p5q,'P5Q',ff)
   CALL rdreal(1,omega7,'omega7',ff)
   CALL rdreal(1,fake,'axis_I7',ff)
   CALL rdreal(1,fake,'axis_beta7',ff)
   CALL rdreal2(1,dwavex(1,7),dwavey(1,7),'l.d. 7, band 1',ff)
   CALL rdreal2(1,dwavex(2,7),dwavey(2,7),'l.d. 7, band 2',ff)
   CALL rdreal2(1,dwavex(3,7),dwavey(3,7),'l.d. 7, band 3',ff)
   CALL rdreal2(1,dwavex(4,7),dwavey(4,7),'l.d. 7, band 4',ff)
   CALL rdreal2(1,dwavex(5,7),dwavey(5,7),'l.d. 7, band 5',ff)
   CALL rdreal2(1,dwavex(6,7),dwavey(6,7),'l.d. 7, band 6',ff)
   CALL rdreal2(1,dwavex(7,7),dwavey(7,7),'l.d. 7, band 7',ff)
   CALL rdreal2(1,dwavex(8,7),dwavey(8,7),'l.d. 7, band 8',ff)
   CALL rdreal(1,rk7,'rk7',ff)
   CALL rdreal(1,sdarkint7(1),'flux 7, band 1',ff)
   CALL rdreal(1,sdarkint7(2),'flux 7, band 2',ff)
   CALL rdreal(1,sdarkint7(3),'flux 7, band 3',ff)
   CALL rdreal(1,sdarkint7(4),'flux 7, band 4',ff)
   CALL rdreal(1,sdarkint7(5),'flux 7, band 5',ff)
   CALL rdreal(1,sdarkint7(6),'flux 7, band 6',ff)
   CALL rdreal(1,sdarkint7(7),'flux 7, band 7',ff)
   CALL rdreal(1,sdarkint7(8),'flux 7, band 8',ff)
!
!  If Nbody < 8, then we are done.
!
   IF(isw30 < 8)THEN
      CLOSE(1)
      RETURN
   END IF
!
   CALL rdreal(1,p6tconj,'P6tconj',ff)
   CALL rdreal(1,p6period,'P6period',ff)
   CALL rdreal(1,p6ptc,'P6pTc',ff)
   CALL rdreal(1,p6mtc,'P6mTc',ff)
   CALL rdreal(1,p6qtc,'P6qTc',ff)
   CALL rdreal(1,t6off,'T6off',ff)
   CALL rdreal(1,p6t0,'P6T0',ff)
   CALL rdreal(1,p6ecos,'P6ecos',ff)
   CALL rdreal(1,p6esin,'P6esin',ff)
   CALL rdreal(1,sqp6ecos,'P6sqrt(e)cos',ff)
   CALL rdreal(1,sqp6esin,'P6sqrt(e)sin',ff)
   CALL rdreal(1,p6incl,'P6incl',ff)
   CALL rdreal(1,p6omega,'P6Omega',ff)
   CALL rdreal(1,angsum6,'angsum6',ff)
   CALL rdreal(1,angdiff6,'angdiff6',ff)
   CALL rdreal(1,t8,'t8',ff)
   CALL rdreal(1,g8,'g8',ff)
   CALL rdreal(1,p6ratrad,'P6ratrad',ff)
   CALL rdreal(1,p6q,'P6Q',ff)
   CALL rdreal(1,omega8,'omega8',ff)
   CALL rdreal(1,fake,'axis_I8',ff)
   CALL rdreal(1,fake,'axis_beta8',ff)
   CALL rdreal2(1,dwavex(1,8),dwavey(1,8),'l.d. 8, band 1',ff)
   CALL rdreal2(1,dwavex(2,8),dwavey(2,8),'l.d. 8, band 2',ff)
   CALL rdreal2(1,dwavex(3,8),dwavey(3,8),'l.d. 8, band 3',ff)
   CALL rdreal2(1,dwavex(4,8),dwavey(4,8),'l.d. 8, band 4',ff)
   CALL rdreal2(1,dwavex(5,8),dwavey(5,8),'l.d. 8, band 5',ff)
   CALL rdreal2(1,dwavex(6,8),dwavey(6,8),'l.d. 8, band 6',ff)
   CALL rdreal2(1,dwavex(7,8),dwavey(7,8),'l.d. 8, band 7',ff)
   CALL rdreal2(1,dwavex(8,8),dwavey(8,8),'l.d. 8, band 8',ff)
   CALL rdreal(1,rk8,'rk8',ff)
   CALL rdreal(1,sdarkint8(1),'flux 8, band 1',ff)
   CALL rdreal(1,sdarkint8(2),'flux 8, band 2',ff)
   CALL rdreal(1,sdarkint8(3),'flux 8, band 3',ff)
   CALL rdreal(1,sdarkint8(4),'flux 8, band 4',ff)
   CALL rdreal(1,sdarkint8(5),'flux 8, band 5',ff)
   CALL rdreal(1,sdarkint8(6),'flux 8, band 6',ff)
   CALL rdreal(1,sdarkint8(7),'flux 8, band 7',ff)
   CALL rdreal(1,sdarkint8(8),'flux 8, band 8',ff)
!
!  If Nbody < 9, then we are done.
!
   IF(isw30 < 9)THEN
      CLOSE(1)
      RETURN
   END IF
!
   CALL rdreal(1,p7tconj,'P7tconj',ff)
   CALL rdreal(1,p7period,'P7period',ff)
   CALL rdreal(1,p7ptc,'P7pTc',ff)
   CALL rdreal(1,p7mtc,'P7mTc',ff)
   CALL rdreal(1,p7qtc,'P7qTc',ff)
   CALL rdreal(1,t7off,'T7off',ff)
   CALL rdreal(1,p7t0,'P7T0',ff)
   CALL rdreal(1,p7ecos,'P7ecos',ff)
   CALL rdreal(1,p7esin,'P7esin',ff)
   CALL rdreal(1,sqp7ecos,'P7sqrt(e)cos',ff)
   CALL rdreal(1,sqp7esin,'P7sqrt(e)esin',ff)
   CALL rdreal(1,p7incl,'P7incl',ff)
   CALL rdreal(1,p7omega,'P7Omega',ff)
   CALL rdreal(1,angsum7,'angsum7',ff)
   CALL rdreal(1,angdiff7,'angdiff7',ff)
   CALL rdreal(1,t9,'t9',ff)
   CALL rdreal(1,g9,'g9',ff)
   CALL rdreal(1,p7ratrad,'P7ratrad',ff)
   CALL rdreal(1,p7q,'P7Q',ff)
   CALL rdreal(1,omega9,'omega9',ff)
   CALL rdreal(1,fake,'axis_I9',ff)
   CALL rdreal(1,fake,'axis_I9',ff)
   CALL rdreal2(1,dwavex(1,9),dwavey(1,9),'l.d. 9, band 1',ff)
   CALL rdreal2(1,dwavex(2,9),dwavey(2,9),'l.d. 9, band 2',ff)
   CALL rdreal2(1,dwavex(3,9),dwavey(3,9),'l.d. 9, band 3',ff)
   CALL rdreal2(1,dwavex(4,9),dwavey(4,9),'l.d. 9, band 4',ff)
   CALL rdreal2(1,dwavex(5,9),dwavey(5,9),'l.d. 9, band 5',ff)
   CALL rdreal2(1,dwavex(6,9),dwavey(6,9),'l.d. 9, band 6',ff)
   CALL rdreal2(1,dwavex(7,9),dwavey(7,9),'l.d. 9, band 7',ff)
   CALL rdreal2(1,dwavex(8,9),dwavey(8,9),'l.d. 9, band 8',ff)
   CALL rdreal(1,rk9,'rk9',ff)
   CALL rdreal(1,sdarkint9(1),'flux 9, band 1',ff)
   CALL rdreal(1,sdarkint9(2),'flux 9, band 2',ff)
   CALL rdreal(1,sdarkint9(3),'flux 9, band 3',ff)
   CALL rdreal(1,sdarkint9(4),'flux 9, band 4',ff)
   CALL rdreal(1,sdarkint9(5),'flux 9, band 5',ff)
   CALL rdreal(1,sdarkint9(6),'flux 9, band 6',ff)
   CALL rdreal(1,sdarkint9(7),'flux 9, band 7',ff)
   CALL rdreal(1,sdarkint9(8),'flux 9, band 8',ff)
!
!  If Nbody < 10, then we are done.
!
   IF(isw30 < 10)THEN
      CLOSE(1)
      RETURN
   END IF
!
   CALL rdreal(1,p8tconj,'P8tconj',ff)
   CALL rdreal(1,p8period,'P8period',ff)
   CALL rdreal(1,p8ptc,'P8pTc',ff)
   CALL rdreal(1,p8mtc,'P8mTc',ff)
   CALL rdreal(1,p8qtc,'P8qTc',ff)
   CALL rdreal(1,t8off,'T8off',ff)
   CALL rdreal(1,p8t0,'P8T0',ff)
   CALL rdreal(1,p8ecos,'P8ecos',ff)
   CALL rdreal(1,p8esin,'P8esin',ff)
   CALL rdreal(1,sqp8ecos,'P8sqrt(e)cos',ff)
   CALL rdreal(1,sqp8esin,'P8sqrt(e)sin',ff)
   CALL rdreal(1,p8incl,'P8incl',ff)
   CALL rdreal(1,p8omega,'P8Omega',ff)
   CALL rdreal(1,angsum8,'angsum8',ff)
   CALL rdreal(1,angdiff8,'angdiff8',ff)
   CALL rdreal(1,t10,'t10',ff)
   CALL rdreal(1,g10,'g10',ff)
   CALL rdreal(1,p8ratrad,'P8ratrad',ff)
   CALL rdreal(1,p8q,'P8Q',ff)
   CALL rdreal(1,omega10,'omega10',ff)
   CALL rdreal(1,fake,'axis_I10',ff)
   CALL rdreal(1,fake,'axis_I10',ff)
   CALL rdreal2(1,dwavex(1,10),dwavey(1,10),'l.d. 10, band 1', ff)
   CALL rdreal2(1,dwavex(2,10),dwavey(2,10),'l.d. 10, band 1', ff)
   CALL rdreal2(1,dwavex(3,10),dwavey(3,10),'l.d. 10, band 1', ff)
   CALL rdreal2(1,dwavex(4,10),dwavey(4,10),'l.d. 10, band 1', ff)
   CALL rdreal2(1,dwavex(5,10),dwavey(5,10),'l.d. 10, band 1', ff)
   CALL rdreal2(1,dwavex(6,10),dwavey(6,10),'l.d. 10, band 1', ff)
   CALL rdreal2(1,dwavex(7,10),dwavey(7,10),'l.d. 10, band 1', ff)
   CALL rdreal2(1,dwavex(8,10),dwavey(8,10),'l.d. 10, band 1', ff)
   CALL rdreal(1,rk10,'rk10',ff)
   CALL rdreal(1,sdarkint10(1),'flux 10, band 1',ff)
   CALL rdreal(1,sdarkint10(2),'flux 10, band 2',ff)
   CALL rdreal(1,sdarkint10(3),'flux 10, band 3',ff)
   CALL rdreal(1,sdarkint10(4),'flux 10, band 4',ff)
   CALL rdreal(1,sdarkint10(5),'flux 10, band 5',ff)
   CALL rdreal(1,sdarkint10(6),'flux 10, band 6',ff)
   CALL rdreal(1,sdarkint10(7),'flux 10, band 7',ff)
   CALL rdreal(1,sdarkint10(8),'flux 10, band 8',ff)
!
   IF(ios == 0)THEN
      CLOSE(1)
      RETURN
   END IF
!
!   Come here if there is a problem with opening the file
!
10 IF(ios > 0)THEN
      IF(kkkk == 0)WRITE(*,50)bell
      IF(kkkk > 0)WRITE(*,60)bell,kkkk
      STOP
   END IF
!
20 FORMAT(a1,'index is too large')
25 FORMAT(a1,'Error:  Time step is zero when itime=2')
30 FORMAT(i4)
40 FORMAT(a40)
50 FORMAT(a1,'Error:  File ELC.inp does not exist')
60 FORMAT(a1,'Error:  File ELC.???? does not exist for index ', i4)
!
   RETURN
!
END SUBROUTINE newgetinput
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE newwritegridout(nalph1,nalph2,nalph3,nbet1,nbet2,  &
   nbet3,nradius,nref,nseg,ntheta,irvfilt,iatm,icnb,icnh,icni,  &
   icnj,icnk,icnr,icnu,icnv,idark1,idark2,idraw,iecheck,iidint,  &
   ikeep,ilaw,ism1,isw1,isw13,isw2,isw21,isw22,isw23,  &
   isw24,isw25,isw26,isw27,isw28,isw29,isw3,isw30,isw31,isw32,  &
   isw33,isw34,isw4,isw5,isw6,isw7,isw8,isw80,isw81,  &
   isw85,isw86,isw87,isw88,isw89,isw9,isw100,isynch,it1,  &
   it2,it3,it4,itconj,p1mtc,p1ptc,p2omega,p2q,p2t0,p2ecos,  &
   p2esin,p2incl,p2mtc,p2ptc,p2period,p2ratrad,p2tconj,p3omega,  &
   p3q,p3t0,p3ecos,p3esin,p3incl,p3mtc,p3ptc,p3period,p3ratrad,  &
   p3tconj,p4omega,p4q,p4t0,p4ecos,p4esin,p4incl,p4mtc,p4ptc,  &
   p4period,p4ratrad,p4tconj,p5omega,p5q,p5t0,p5ecos,p5esin,  &
   p5incl,p5mtc,p5ptc,p5period,p5ratrad,p5tconj,p6omega,p6q,  &
   p6t0,p6ecos,p6esin,p6incl,p6mtc,p6ptc,p6period,p6ratrad,  &
   p6tconj,p7omega,p7q,p7t0,p7ecos,p7esin,p7incl,p7mtc,p7ptc,  &
   p7period,p7ratrad,p7tconj,p8omega,p8q,p8t0,p8ecos,p8esin,  &
   p8incl,p8mtc,p8ptc,p8period,p8ratrad,p8tconj,pbmtc,pbptc,  &
   period,q,sa3,t0,tconj,teff1,teff2,tgrav1,tgrav2,alb1,alb2,  &
   argper,beam1,beam2,betarim,bigi,bigi2,bigi3,bigi4,bigbeta,  &
   bigbeta2,bigbeta3,bigbeta4,bin2q,b2massdiff,b2masssum,  &
   b2raddiff,b2radsum,bin2ratrad,contam,contams0,contams1,  &
   contams2,contams3,dbolx,dboly,dphase,dwavex,dwavey,  &
   ecc,ecosw,fill1,fill2,finc,frac1,frac2,g10,g3,g6,g7,g8,g9,  &
   gamma,hh,massdiff,masssum,ocose,omega1,omega2,omega3,omega4,  &
   omega5,omega6,omega7,omega8,omega9,omega10,osine,  &
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
   t3off,t4off,t5off,t6off,t7off,t8off,iextension,iversion,  &
   tesscontam,tessfilt,tessbin)
!
!   November 1, 2019
!
!   Modified to handle version numbers.  If iversion=0, then
!   there is no change.  If iversion=1, then add some
!   blocks about TESS features after the Kepler block.  There
!   will be 3 items:
!
!   tesscontam
!   tessfilt
!   tessbin
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nalph1
   INTEGER, INTENT(IN)                      :: nalph2
   INTEGER, INTENT(IN)                      :: nalph3
   INTEGER, INTENT(IN)                      :: nbet1
   INTEGER, INTENT(IN)                      :: nbet2
   INTEGER, INTENT(IN)                      :: nbet3
   INTEGER, INTENT(IN)                      :: nradius
   INTEGER, INTENT(IN)                      :: nref
   INTEGER, INTENT(IN)                      :: nseg
   INTEGER, INTENT(IN)                      :: ntheta
   INTEGER, INTENT(IN)                      :: irvfilt
   INTEGER, INTENT(IN)                      :: iatm
   INTEGER, INTENT(IN)                      :: icnb
   INTEGER, INTENT(IN)                      :: icnh
   INTEGER, INTENT(IN)                      :: icni
   INTEGER, INTENT(IN)                      :: icnj
   INTEGER, INTENT(IN)                      :: icnk
   INTEGER, INTENT(IN)                      :: icnr
   INTEGER, INTENT(IN)                      :: icnu
   INTEGER, INTENT(IN)                      :: icnv
   INTEGER, INTENT(IN)                      :: idark1
   INTEGER, INTENT(IN)                      :: idark2
   INTEGER, INTENT(IN)                      :: idraw
   INTEGER, INTENT(IN)                      :: iecheck
   INTEGER, INTENT(IN)                      :: iidint
   INTEGER, INTENT(IN)                      :: ikeep
   INTEGER, INTENT(IN)                      :: ilaw
   INTEGER, INTENT(IN)                      :: ism1
   INTEGER, INTENT(IN)                      :: isw1
   INTEGER, INTENT(IN)                      :: isw13
   INTEGER, INTENT(IN)                      :: isw2
   INTEGER, INTENT(IN)                      :: isw21
   INTEGER, INTENT(IN)                      :: isw22
   INTEGER, INTENT(IN)                      :: isw23
   INTEGER, INTENT(IN)                      :: isw24
   INTEGER, INTENT(IN)                      :: isw25
   INTEGER, INTENT(IN)                      :: isw26
   INTEGER, INTENT(IN)                      :: isw27
   INTEGER, INTENT(IN)                      :: isw28
   INTEGER, INTENT(IN)                      :: isw29
   INTEGER, INTENT(IN)                      :: isw3
   INTEGER, INTENT(IN)                      :: isw30
   INTEGER, INTENT(IN)                      :: isw31
   INTEGER, INTENT(IN)                      :: isw32
   INTEGER, INTENT(IN)                      :: isw33
   INTEGER, INTENT(IN)                      :: isw34
   INTEGER, INTENT(IN)                      :: isw4
   INTEGER, INTENT(IN)                      :: isw5
   INTEGER, INTENT(IN)                      :: isw6
   INTEGER, INTENT(IN)                      :: isw7
   INTEGER, INTENT(IN)                      :: isw8
   INTEGER, INTENT(IN)                      :: isw80
   INTEGER, INTENT(IN)                      :: isw81
   INTEGER, INTENT(IN)                      :: isw85
   INTEGER, INTENT(IN)                      :: isw86
   INTEGER, INTENT(IN)                      :: isw87
   INTEGER, INTENT(IN)                      :: isw88
   INTEGER, INTENT(IN)                      :: isw89
   INTEGER, INTENT(IN)                      :: isw9
   INTEGER, INTENT(IN)                      :: isw100
   INTEGER, INTENT(IN)                      :: isynch
   INTEGER, INTENT(IN)                      :: it1
   INTEGER, INTENT(IN)                      :: it2
   INTEGER, INTENT(IN)                      :: it3
   INTEGER, INTENT(IN)                      :: it4
   INTEGER, INTENT(IN)                      :: itconj
   REAL(KIND=dp), INTENT(IN)                :: p1mtc
   REAL(KIND=dp), INTENT(IN)                :: p1ptc
   REAL(KIND=dp), INTENT(IN)                :: p2omega
   REAL(KIND=dp), INTENT(IN)                :: p2q
   REAL(KIND=dp), INTENT(IN)                :: p2t0
   REAL(KIND=dp), INTENT(IN)                :: p2ecos
   REAL(KIND=dp), INTENT(IN)                :: p2esin
   REAL(KIND=dp), INTENT(IN)                :: p2incl
   REAL(KIND=dp), INTENT(IN)                :: p2mtc
   REAL(KIND=dp), INTENT(IN)                :: p2ptc
   REAL(KIND=dp), INTENT(IN)                :: p2period
   REAL(KIND=dp), INTENT(IN)                :: p2ratrad
   REAL(KIND=dp), INTENT(IN)                :: p2tconj
   REAL(KIND=dp), INTENT(IN)                :: p3omega
   REAL(KIND=dp), INTENT(IN)                :: p3q
   REAL(KIND=dp), INTENT(IN)                :: p3t0
   REAL(KIND=dp), INTENT(IN)                :: p3ecos
   REAL(KIND=dp), INTENT(IN)                :: p3esin
   REAL(KIND=dp), INTENT(IN)                :: p3incl
   REAL(KIND=dp), INTENT(IN)                :: p3mtc
   REAL(KIND=dp), INTENT(IN)                :: p3ptc
   REAL(KIND=dp), INTENT(IN)                :: p3period
   REAL(KIND=dp), INTENT(IN)                :: p3ratrad
   REAL(KIND=dp), INTENT(IN)                :: p3tconj
   REAL(KIND=dp), INTENT(IN)                :: p4omega
   REAL(KIND=dp), INTENT(IN)                :: p4q
   REAL(KIND=dp), INTENT(IN)                :: p4t0
   REAL(KIND=dp), INTENT(IN)                :: p4ecos
   REAL(KIND=dp), INTENT(IN)                :: p4esin
   REAL(KIND=dp), INTENT(IN)                :: p4incl
   REAL(KIND=dp), INTENT(IN)                :: p4mtc
   REAL(KIND=dp), INTENT(IN)                :: p4ptc
   REAL(KIND=dp), INTENT(IN)                :: p4period
   REAL(KIND=dp), INTENT(IN)                :: p4ratrad
   REAL(KIND=dp), INTENT(IN)                :: p4tconj
   REAL(KIND=dp), INTENT(IN)                :: p5omega
   REAL(KIND=dp), INTENT(IN)                :: p5q
   REAL(KIND=dp), INTENT(IN)                :: p5t0
   REAL(KIND=dp), INTENT(IN)                :: p5ecos
   REAL(KIND=dp), INTENT(IN)                :: p5esin
   REAL(KIND=dp), INTENT(IN)                :: p5incl
   REAL(KIND=dp), INTENT(IN)                :: p5mtc
   REAL(KIND=dp), INTENT(IN)                :: p5ptc
   REAL(KIND=dp), INTENT(IN)                :: p5period
   REAL(KIND=dp), INTENT(IN)                :: p5ratrad
   REAL(KIND=dp), INTENT(IN)                :: p5tconj
   REAL(KIND=dp), INTENT(IN)                :: p6omega
   REAL(KIND=dp), INTENT(IN)                :: p6q
   REAL(KIND=dp), INTENT(IN)                :: p6t0
   REAL(KIND=dp), INTENT(IN)                :: p6ecos
   REAL(KIND=dp), INTENT(IN)                :: p6esin
   REAL(KIND=dp), INTENT(IN)                :: p6incl
   REAL(KIND=dp), INTENT(IN)                :: p6mtc
   REAL(KIND=dp), INTENT(IN)                :: p6ptc
   REAL(KIND=dp), INTENT(IN)                :: p6period
   REAL(KIND=dp), INTENT(IN)                :: p6ratrad
   REAL(KIND=dp), INTENT(IN)                :: p6tconj
   REAL(KIND=dp), INTENT(IN)                :: p7omega
   REAL(KIND=dp), INTENT(IN)                :: p7q
   REAL(KIND=dp), INTENT(IN)                :: p7t0
   REAL(KIND=dp), INTENT(IN)                :: p7ecos
   REAL(KIND=dp), INTENT(IN)                :: p7esin
   REAL(KIND=dp), INTENT(IN)                :: p7incl
   REAL(KIND=dp), INTENT(IN)                :: p7mtc
   REAL(KIND=dp), INTENT(IN)                :: p7ptc
   REAL(KIND=dp), INTENT(IN)                :: p7period
   REAL(KIND=dp), INTENT(IN)                :: p7ratrad
   REAL(KIND=dp), INTENT(IN)                :: p7tconj
   REAL(KIND=dp), INTENT(IN)                :: p8omega
   REAL(KIND=dp), INTENT(IN)                :: p8q
   REAL(KIND=dp), INTENT(IN)                :: p8t0
   REAL(KIND=dp), INTENT(IN)                :: p8ecos
   REAL(KIND=dp), INTENT(IN)                :: p8esin
   REAL(KIND=dp), INTENT(IN)                :: p8incl
   REAL(KIND=dp), INTENT(IN)                :: p8mtc
   REAL(KIND=dp), INTENT(IN)                :: p8ptc
   REAL(KIND=dp), INTENT(IN)                :: p8period
   REAL(KIND=dp), INTENT(IN)                :: p8ratrad
   REAL(KIND=dp), INTENT(IN)                :: p8tconj
   REAL(KIND=dp), INTENT(IN)                :: pbmtc
   REAL(KIND=dp), INTENT(IN)                :: pbptc
   REAL(KIND=dp), INTENT(IN)                :: period
   REAL(KIND=dp), INTENT(IN)                :: q
   REAL(KIND=dp), INTENT(IN)                :: sa3
   REAL(KIND=dp), INTENT(IN)                :: t0
   REAL(KIND=dp), INTENT(IN)                :: tconj
   REAL(KIND=dp), INTENT(IN)                :: teff1
   REAL(KIND=dp), INTENT(IN)                :: teff2
   REAL(KIND=dp), INTENT(IN)                :: tgrav1
   REAL(KIND=dp), INTENT(IN)                :: tgrav2
   REAL(KIND=dp), INTENT(IN)                :: alb1
   REAL(KIND=dp), INTENT(IN)                :: alb2
   REAL(KIND=dp), INTENT(IN)                :: argper
   REAL(KIND=dp), INTENT(IN)                :: beam1
   REAL(KIND=dp), INTENT(IN)                :: beam2
   REAL(KIND=dp), INTENT(IN)                :: betarim
   REAL(KIND=dp), INTENT(IN)                :: bigi
   REAL(KIND=dp), INTENT(IN)                :: bigi2
   REAL(KIND=dp), INTENT(IN)                :: bigi3
   REAL(KIND=dp), INTENT(IN)                :: bigi4
   REAL(KIND=dp), INTENT(IN)                :: bigbeta
   REAL(KIND=dp), INTENT(IN)                :: bigbeta2
   REAL(KIND=dp), INTENT(IN)                :: bigbeta3
   REAL(KIND=dp), INTENT(IN)                :: bigbeta4
   REAL(KIND=dp), INTENT(IN)                :: bin2q
   REAL(KIND=dp), INTENT(IN)                :: b2massdiff
   REAL(KIND=dp), INTENT(IN)                :: b2masssum
   REAL(KIND=dp), INTENT(IN)                :: b2raddiff
   REAL(KIND=dp), INTENT(IN)                :: b2radsum
   REAL(KIND=dp), INTENT(IN)                :: bin2ratrad
   REAL(KIND=dp), INTENT(IN)                :: contam
   REAL(KIND=dp), INTENT(IN)                :: contams0
   REAL(KIND=dp), INTENT(IN)                :: contams1
   REAL(KIND=dp), INTENT(IN)                :: contams2
   REAL(KIND=dp), INTENT(IN)                :: contams3
   REAL(KIND=dp), INTENT(IN)                :: dbolx(8,2)
   REAL(KIND=dp), INTENT(IN)                :: dboly(8,2)
   REAL(KIND=dp), INTENT(IN)                :: dphase
   REAL(KIND=dp), INTENT(IN)                :: dwavex(8,10)
   REAL(KIND=dp), INTENT(IN)                :: dwavey(8,10)
   REAL(KIND=dp), INTENT(IN)                :: ecc
   REAL(KIND=dp), INTENT(IN)                :: ecosw
   REAL(KIND=dp), INTENT(IN)                :: fill1
   REAL(KIND=dp), INTENT(IN)                :: fill2
   REAL(KIND=dp), INTENT(IN)                :: finc
   REAL(KIND=dp), INTENT(IN)                :: frac1
   REAL(KIND=dp), INTENT(IN)                :: frac2
   REAL(KIND=dp), INTENT(IN)                :: g10
   REAL(KIND=dp), INTENT(IN)                :: g3
   REAL(KIND=dp), INTENT(IN)                :: g6
   REAL(KIND=dp), INTENT(IN)                :: g7
   REAL(KIND=dp), INTENT(IN)                :: g8
   REAL(KIND=dp), INTENT(IN)                :: g9
   REAL(KIND=dp), INTENT(IN)                :: gamma
   REAL(KIND=dp), INTENT(IN)                :: hh
   REAL(KIND=dp), INTENT(IN)                :: massdiff
   REAL(KIND=dp), INTENT(IN)                :: masssum
   REAL(KIND=dp), INTENT(IN)                :: ocose
   REAL(KIND=dp), INTENT(IN)                :: omega1
   REAL(KIND=dp), INTENT(IN)                :: omega2
   REAL(KIND=dp), INTENT(IN)                :: omega3
   REAL(KIND=dp), INTENT(IN)                :: omega4
   REAL(KIND=dp), INTENT(IN)                :: omega5
   REAL(KIND=dp), INTENT(IN)                :: omega6
   REAL(KIND=dp), INTENT(IN)                :: omega7
   REAL(KIND=dp), INTENT(IN)                :: omega8
   REAL(KIND=dp), INTENT(IN)                :: omega9
   REAL(KIND=dp), INTENT(IN)                :: omega10
   REAL(KIND=dp), INTENT(IN)                :: osine
   REAL(KIND=dp), INTENT(IN)                :: primk
   REAL(KIND=dp), INTENT(IN)                :: primmass
   REAL(KIND=dp), INTENT(IN)                :: primrad
   REAL(KIND=dp), INTENT(IN)                :: pshift
   REAL(KIND=dp), INTENT(IN)                :: rlx
   REAL(KIND=dp), INTENT(IN)                :: raddiff
   REAL(KIND=dp), INTENT(IN)                :: radsum
   REAL(KIND=dp), INTENT(IN)                :: ratrad
   REAL(KIND=dp), INTENT(IN)                :: rinner
   REAL(KIND=dp), INTENT(IN)                :: rk3
   REAL(KIND=dp), INTENT(IN)                :: rk4
   REAL(KIND=dp), INTENT(IN)                :: rk5
   REAL(KIND=dp), INTENT(IN)                :: rk6
   REAL(KIND=dp), INTENT(IN)                :: rk7
   REAL(KIND=dp), INTENT(IN)                :: rk8
   REAL(KIND=dp), INTENT(IN)                :: rk9
   REAL(KIND=dp), INTENT(IN)                :: rk10
   REAL(KIND=dp), INTENT(IN)                :: router
   REAL(KIND=dp), INTENT(IN)                :: sdarkint1(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint2(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint3(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint4(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint5(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint6(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint7(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint8(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint9(8)
   REAL(KIND=dp), INTENT(IN)                :: sdarkint10(8)
   REAL(KIND=dp), INTENT(IN)                :: secmass
   REAL(KIND=dp), INTENT(IN)                :: secrad
   REAL(KIND=dp), INTENT(IN)                :: separ
   REAL(KIND=dp), INTENT(IN)                :: spot1parm(2,4)
   REAL(KIND=dp), INTENT(IN)                :: spot2parm(2,4)
   REAL(KIND=dp), INTENT(IN)                :: spotdparm(2,4)
   REAL(KIND=dp), INTENT(IN)                :: sw1
   REAL(KIND=dp), INTENT(IN)                :: sw2
   REAL(KIND=dp), INTENT(IN)                :: sw23
   REAL(KIND=dp), INTENT(IN)                :: sw24
   REAL(KIND=dp), INTENT(IN)                :: sw25
   REAL(KIND=dp), INTENT(IN)                :: sw26
   REAL(KIND=dp), INTENT(IN)                :: sw27
   REAL(KIND=dp), INTENT(IN)                :: sw28
   REAL(KIND=dp), INTENT(IN)                :: sw29
   REAL(KIND=dp), INTENT(IN)                :: sw3
   REAL(KIND=dp), INTENT(IN)                :: sw30
   REAL(KIND=dp), INTENT(IN)                :: sw47
   REAL(KIND=dp), INTENT(IN)                :: sw48
   REAL(KIND=dp), INTENT(IN)                :: sw49
   REAL(KIND=dp), INTENT(IN)                :: sw5
   REAL(KIND=dp), INTENT(IN)                :: sw6
   REAL(KIND=dp), INTENT(IN)                :: sw7
   REAL(KIND=dp), INTENT(IN)                :: sw72
   REAL(KIND=dp), INTENT(IN)                :: sw73
   REAL(KIND=dp), INTENT(IN)                :: sw8
   REAL(KIND=dp), INTENT(IN)                :: sw80
   REAL(KIND=dp), INTENT(IN)                :: sw81
   REAL(KIND=dp), INTENT(IN)                :: sw82
   REAL(KIND=dp), INTENT(IN)                :: sw83
   REAL(KIND=dp), INTENT(IN)                :: sw84
   REAL(KIND=dp), INTENT(IN)                :: sw9
   REAL(KIND=dp), INTENT(IN)                :: t10
   REAL(KIND=dp), INTENT(IN)                :: t3
   REAL(KIND=dp), INTENT(IN)                :: t6
   REAL(KIND=dp), INTENT(IN)                :: t7
   REAL(KIND=dp), INTENT(IN)                :: t8
   REAL(KIND=dp), INTENT(IN)                :: t9
   REAL(KIND=dp), INTENT(IN)                :: tdisk
   REAL(KIND=dp), INTENT(IN)                :: temprat
   REAL(KIND=dp), INTENT(IN)                :: tertomega
   REAL(KIND=dp), INTENT(IN)                :: tertq
   REAL(KIND=dp), INTENT(IN)                :: tertconj
   REAL(KIND=dp), INTENT(IN)                :: tertecos
   REAL(KIND=dp), INTENT(IN)                :: tertesin
   REAL(KIND=dp), INTENT(IN)                :: tertincl
   REAL(KIND=dp), INTENT(IN)                :: tertperiod
   REAL(KIND=dp), INTENT(IN)                :: tertratrad
   REAL(KIND=dp), INTENT(IN)                :: tertt0
   REAL(KIND=dp), INTENT(IN)                :: wave(8)
   REAL(KIND=dp), INTENT(IN)                :: xi
   INTEGER, INTENT(IN)                      :: iunit
   REAL(KIND=dp), INTENT(IN)                :: fracsum
   REAL(KIND=dp), INTENT(IN)                :: fracdiff
   REAL(KIND=dp), INTENT(IN)                :: bin2m3
   REAL(KIND=dp), INTENT(IN)                :: bin2m4
   REAL(KIND=dp), INTENT(IN)                :: bin2r3
   REAL(KIND=dp), INTENT(IN)                :: bin2r4
   REAL(KIND=dp), INTENT(IN)                :: sqecos
   REAL(KIND=dp), INTENT(IN)                :: sqesin
   REAL(KIND=dp), INTENT(IN)                :: sqtertecos
   REAL(KIND=dp), INTENT(IN)                :: sqtertesin
   REAL(KIND=dp), INTENT(IN)                :: sqp2ecos
   REAL(KIND=dp), INTENT(IN)                :: sqp2esin
   REAL(KIND=dp), INTENT(IN)                :: sqp3ecos
   REAL(KIND=dp), INTENT(IN)                :: sqp3esin
   REAL(KIND=dp), INTENT(IN)                :: sqp4ecos
   REAL(KIND=dp), INTENT(IN)                :: sqp4esin
   REAL(KIND=dp), INTENT(IN)                :: sqp5ecos
   REAL(KIND=dp), INTENT(IN)                :: sqp5esin
   REAL(KIND=dp), INTENT(IN)                :: sqp6ecos
   REAL(KIND=dp), INTENT(IN)                :: sqp6esin
   REAL(KIND=dp), INTENT(IN)                :: sqp7ecos
   REAL(KIND=dp), INTENT(IN)                :: sqp7esin
   REAL(KIND=dp), INTENT(IN)                :: sqp8ecos
   REAL(KIND=dp), INTENT(IN)                :: sqp8esin
   REAL(KIND=dp), INTENT(IN)                :: angsum1
   REAL(KIND=dp), INTENT(IN)                :: angdiff1
   REAL(KIND=dp), INTENT(IN)                :: angsum2
   REAL(KIND=dp), INTENT(IN)                :: angdiff2
   REAL(KIND=dp), INTENT(IN)                :: angsum3
   REAL(KIND=dp), INTENT(IN)                :: angdiff3
   REAL(KIND=dp), INTENT(IN)                :: angsum4
   REAL(KIND=dp), INTENT(IN)                :: angdiff4
   REAL(KIND=dp), INTENT(IN)                :: angsum5
   REAL(KIND=dp), INTENT(IN)                :: angdiff5
   REAL(KIND=dp), INTENT(IN)                :: angsum6
   REAL(KIND=dp), INTENT(IN)                :: angdiff6
   REAL(KIND=dp), INTENT(IN)                :: angsum7
   REAL(KIND=dp), INTENT(IN)                :: angdiff7
   REAL(KIND=dp), INTENT(IN)                :: angsum8
   REAL(KIND=dp), INTENT(IN)                :: angdiff8
   INTEGER, INTENT(IN)                      :: imag
   REAL(KIND=dp), INTENT(IN)                :: fillsum
   REAL(KIND=dp), INTENT(IN)                :: filldiff
   REAL(KIND=dp), INTENT(IN)                :: binqtc
   REAL(KIND=dp), INTENT(IN)                :: p1qtc
   REAL(KIND=dp), INTENT(IN)                :: p2qtc
   REAL(KIND=dp), INTENT(IN)                :: p3qtc
   REAL(KIND=dp), INTENT(IN)                :: p4qtc
   REAL(KIND=dp), INTENT(IN)                :: p5qtc
   REAL(KIND=dp), INTENT(IN)                :: p6qtc
   REAL(KIND=dp), INTENT(IN)                :: p7qtc
   REAL(KIND=dp), INTENT(IN)                :: p8qtc
   REAL(KIND=dp), INTENT(IN)                :: tbinoff
   REAL(KIND=dp), INTENT(IN)                :: t1off
   REAL(KIND=dp), INTENT(IN)                :: t2off
   REAL(KIND=dp), INTENT(IN)                :: t3off
   REAL(KIND=dp), INTENT(IN)                :: t4off
   REAL(KIND=dp), INTENT(IN)                :: t5off
   REAL(KIND=dp), INTENT(IN)                :: t6off
   REAL(KIND=dp), INTENT(IN)                :: t7off
   REAL(KIND=dp), INTENT(IN)                :: t8off
   INTEGER, INTENT(IN OUT)                  :: iextension
   INTEGER, INTENT(IN)                      :: iversion
   REAL(KIND=dp), INTENT(IN)                :: tesscontam
   INTEGER, INTENT(IN)                      :: tessfilt
   REAL(KIND=dp), INTENT(IN)                :: tessbin
!
   INTEGER :: iu,ib,iv,ir,ii,ij,ih,ik
!
   REAL(KIND=dp)  :: fake
!
   CHARACTER(LEN=7) :: extension
!
   IF(iextension == -99)THEN
      OPEN(UNIT=1,FILE='bestELC.inp',STATUS='unknown')
   ELSE
      IF(iextension < 0)THEN
         OPEN(UNIT=1,FILE='gridELC.inp',STATUS='unknown')
      ELSE
         IF(iextension < 1000000)iextension=1000000
         WRITE(extension,5)iextension
         OPEN(UNIT=1,FILE='ELC.'//extension,STATUS='unknown')
      END IF
   END IF
!
   fake=0.0_dp
!
!  Modify the icn? flags so that they are either 0 or 1, which conforms
!  to the output format statement.
!
   IF(icnu == 430)THEN
      iu=0
   ELSE
      iu=1
   END IF
   IF(icnb == 430)THEN
      ib=0
   ELSE
      ib=1
   END IF
   IF(icnv == 430)THEN
      iv=0
   ELSE
      iv=1
   END IF
   IF(icnr == 430)THEN
      ir=0
   ELSE
      ir=1
   END IF
   IF(icni == 430)THEN
      ii=0
   ELSE
      ii=1
   END IF
   IF(icnj == 430)THEN
      ij=0
   ELSE
      ij=1
   END IF
   IF(icnh == 430)THEN
      ih=0
   ELSE
      ih=1
   END IF
   IF(icnk == 430)THEN
      ik=0
   ELSE
      ik=1
   END IF
!
!  update the header if version 1
!
   IF(iversion == 0)WRITE(1,10)
   IF(iversion == 1)WRITE(1,15)
   WRITE(1,20)
   WRITE(1,30)
   WRITE(1,20)
   WRITE(1,40)
   WRITE(1,20)
   WRITE(1,50)iunit
   WRITE(1,20)
   WRITE(1,60)
   WRITE(1,20)
   WRITE(1,70)isw7
   WRITE(1,80)sw23
   WRITE(1,90)sw24
   WRITE(1,100)sw9
   WRITE(1,110)dphase
   WRITE(1,20)
   WRITE(1,120)
   WRITE(1,20)
   WRITE(1,130)iatm
   WRITE(1,140)iu,ib,iv,ir,ii,ij,ih,ik
   WRITE(1,150)irvfilt
   WRITE(1,160)ilaw
   WRITE(1,170)wave(1),1
   WRITE(1,170)wave(2),2
   WRITE(1,170)wave(3),3
   WRITE(1,170)wave(4),4
   WRITE(1,170)wave(5),5
   WRITE(1,170)wave(6),6
   WRITE(1,170)wave(7),7
   WRITE(1,170)wave(8),8
   WRITE(1,180)nref
   WRITE(1,190)rlx
   WRITE(1,200)isw25
   WRITE(1,210)idark1
   WRITE(1,220)idark2
   WRITE(1,230)isw5
   WRITE(1,240)isw6
   WRITE(1,250)isw86
   WRITE(1,20)
   WRITE(1,260)
   WRITE(1,20)
   WRITE(1,270)t3
   WRITE(1,280)g3
   WRITE(1,290)sa3
   WRITE(1,20)
   WRITE(1,300)
   WRITE(1,20)
   WRITE(1,310)isw27
   WRITE(1,320)isw33
   WRITE(1,330)isw31
   WRITE(1,340)iecheck
   WRITE(1,350)ism1
   WRITE(1,360)isw13
   WRITE(1,370)isw8
   WRITE(1,380)sw7
   WRITE(1,390)sw8
   WRITE(1,20)
   WRITE(1,400)
   WRITE(1,20)
   WRITE(1,410)isw2
   WRITE(1,420)isw3
   WRITE(1,430)sw2
   WRITE(1,440)sw3
   WRITE(1,20)
   WRITE(1,450)
   WRITE(1,20)
   WRITE(1,460)idraw
   WRITE(1,470)isw1
   WRITE(1,480)sw1
   WRITE(1,20)
   WRITE(1,490)
   WRITE(1,20)
   WRITE(1,500)pshift
   WRITE(1,510)ikeep
   WRITE(1,520)isynch
   WRITE(1,530)isw21
   WRITE(1,540)isw28
   WRITE(1,550)isw29
   WRITE(1,20)
   WRITE(1,560)
   WRITE(1,20)
   WRITE(1,570)imag
   WRITE(1,580)isw9
   WRITE(1,590)isw4
   WRITE(1,600)isw23
   WRITE(1,610)isw24
   WRITE(1,620)isw22
   WRITE(1,630)it4
   WRITE(1,640)sw48
   WRITE(1,650)sw6
   WRITE(1,660)isw32
   WRITE(1,20)
   WRITE(1,670)
   WRITE(1,20)
   WRITE(1,680)isw88
   WRITE(1,690)sw47
   WRITE(1,700)hh
   WRITE(1,710)isw26
   WRITE(1,720)isw80
   WRITE(1,730)itconj
   WRITE(1,740)it1
   WRITE(1,750)isw100
   WRITE(1,760)it3
   WRITE(1,770)isw81
   WRITE(1,780)isw85
   WRITE(1,790)sw84
   WRITE(1,800)it2
   WRITE(1,20)
   WRITE(1,810)
   WRITE(1,20)
   WRITE(1,820)contam
   WRITE(1,830)isw34
   WRITE(1,840)contams0
   WRITE(1,850)contams1
   WRITE(1,860)contams2
   WRITE(1,870)contams3
   WRITE(1,880)nseg
   WRITE(1,890)isw87
   WRITE(1,900)sw29
   WRITE(1,910)sw30
   WRITE(1,920)isw89
!
!  If iversion=1, then add the TESS block
!
   IF(iversion == 1)THEN
      WRITE(1,921)
      WRITE(1,922)
      WRITE(1,923)
      WRITE(1,924)tesscontam
      WRITE(1,925)tessfilt
      WRITE(1,926)tessbin
   END IF
   WRITE(1,20)
   WRITE(1,930)
   WRITE(1,20)
   WRITE(1,940)isw30
   WRITE(1,20)
   WRITE(1,950)
   WRITE(1,20)
   WRITE(1,960)period
   WRITE(1,970)t0
   WRITE(1,980)tconj
   WRITE(1,990)pbptc
   WRITE(1,1000)pbmtc
   WRITE(1,1010)binqtc
   WRITE(1,1020)tbinoff
   WRITE(1,1030)ecc
   WRITE(1,1040)argper
   WRITE(1,1050)ocose
   WRITE(1,1060)osine
   WRITE(1,1070)sqecos
   WRITE(1,1080)sqesin
   WRITE(1,1090)finc
   WRITE(1,1100)sw49
   WRITE(1,1110)primk
   WRITE(1,1120)separ
   WRITE(1,1130)gamma
   WRITE(1,1140)ecosw
   WRITE(1,20)
   WRITE(1,1150)
   WRITE(1,20)
   WRITE(1,1160)nalph1,nbet1
   WRITE(1,1170)teff1
   WRITE(1,1180)tgrav1
   WRITE(1,1190)alb1
   WRITE(1,1200)omega1
   WRITE(1,1210)fill1
   WRITE(1,1220)primrad
   WRITE(1,1230)frac1
   WRITE(1,1240)sw27
   WRITE(1,1250)primmass
   WRITE(1,1260)bigi
   WRITE(1,1270)bigbeta
   WRITE(1,1280)dwavex(1,1),dwavey(1,1)
   WRITE(1,1290)dwavex(2,1),dwavey(2,1)
   WRITE(1,1300)dwavex(3,1),dwavey(3,1)
   WRITE(1,1310)dwavex(4,1),dwavey(4,1)
   WRITE(1,1320)dwavex(5,1),dwavey(5,1)
   WRITE(1,1330)dwavex(6,1),dwavey(6,1)
   WRITE(1,1340)dwavex(7,1),dwavey(7,1)
   WRITE(1,1350)dwavex(8,1),dwavey(8,1)
   WRITE(1,1360)dbolx(1,1),dboly(1,1)
   WRITE(1,1370)spot1parm(1,1)
   WRITE(1,1380)spot1parm(1,2)
   WRITE(1,1390)spot1parm(1,3)
   WRITE(1,1400)spot1parm(1,4)
   WRITE(1,1410)spot1parm(2,1)
   WRITE(1,1420)spot1parm(2,2)
   WRITE(1,1430)spot1parm(2,3)
   WRITE(1,1440)spot1parm(2,4)
   WRITE(1,1450)beam1
   WRITE(1,1460)sw72
   WRITE(1,1470)sdarkint1(1)
   WRITE(1,1480)sdarkint1(2)
   WRITE(1,1490)sdarkint1(3)
   WRITE(1,1500)sdarkint1(4)
   WRITE(1,1510)sdarkint1(5)
   WRITE(1,1520)sdarkint1(6)
   WRITE(1,1530)sdarkint1(7)
   WRITE(1,1540)sdarkint1(8)
   WRITE(1,20)
   WRITE(1,1550)
   WRITE(1,20)
   WRITE(1,1560)nalph2,nbet2
   WRITE(1,1570)teff2
   WRITE(1,1580)temprat
   WRITE(1,1590)tgrav2
   WRITE(1,1600)alb2
   WRITE(1,1610)omega2
   WRITE(1,1620)fill2
   WRITE(1,1630)fillsum
   WRITE(1,1640)filldiff
   WRITE(1,1650)secrad
   WRITE(1,1660)frac2
   WRITE(1,1670)ratrad
   WRITE(1,1680)radsum
   WRITE(1,1690)raddiff
   WRITE(1,1700)fracsum
   WRITE(1,1710)fracdiff
   WRITE(1,1720)sw28
   WRITE(1,1730)q
   WRITE(1,1740)secmass
   WRITE(1,1750)masssum
   WRITE(1,1760)massdiff
   WRITE(1,1770)bigi2
   WRITE(1,1780)bigbeta2
   WRITE(1,1790)dwavex(1,2),dwavey(1,2)
   WRITE(1,1800)dwavex(2,2),dwavey(2,2)
   WRITE(1,1810)dwavex(3,2),dwavey(3,2)
   WRITE(1,1820)dwavex(4,2),dwavey(4,2)
   WRITE(1,1830)dwavex(5,2),dwavey(5,2)
   WRITE(1,1840)dwavex(6,2),dwavey(6,2)
   WRITE(1,1850)dwavex(7,2),dwavey(7,2)
   WRITE(1,1860)dwavex(8,2),dwavey(8,2)
   WRITE(1,1870)dbolx(1,2),dboly(1,2)
   WRITE(1,1880)spot2parm(1,1)
   WRITE(1,1890)spot2parm(1,2)
   WRITE(1,1900)spot2parm(1,3)
   WRITE(1,1910)spot2parm(1,4)
   WRITE(1,1920)spot2parm(2,1)
   WRITE(1,1930)spot2parm(2,2)
   WRITE(1,1940)spot2parm(2,3)
   WRITE(1,1950)spot2parm(2,4)
   WRITE(1,1960)beam2
   WRITE(1,1970)sw73
   WRITE(1,1980)sdarkint2(1)
   WRITE(1,1990)sdarkint2(2)
   WRITE(1,2000)sdarkint2(3)
   WRITE(1,2010)sdarkint2(4)
   WRITE(1,2020)sdarkint2(5)
   WRITE(1,2030)sdarkint2(6)
   WRITE(1,2040)sdarkint2(7)
   WRITE(1,2050)sdarkint2(8)
   WRITE(1,20)
   WRITE(1,2060)
   WRITE(1,20)
   WRITE(1,2070)iidint
   WRITE(1,2080)ntheta
   WRITE(1,2090)nradius
   WRITE(1,2100)betarim
   WRITE(1,2110)rinner
   WRITE(1,2120)router
   WRITE(1,2130)tdisk
   WRITE(1,2140)xi
   WRITE(1,2150)spotdparm(1,1)
   WRITE(1,2160)spotdparm(1,2)
   WRITE(1,2170)spotdparm(1,3)
   WRITE(1,2180)spotdparm(1,4)
   WRITE(1,2190)spotdparm(2,1)
   WRITE(1,2200)spotdparm(2,2)
   WRITE(1,2210)spotdparm(2,3)
   WRITE(1,2220)spotdparm(2,4)
   WRITE(1,2230)sw26
   WRITE(1,20)
   WRITE(1,2240)
   WRITE(1,20)
   WRITE(1,2250)sw5
   WRITE(1,2260)sw25
!
!  If Nbody < 3, then we are done.
!
   IF(isw30 < 3)THEN
      CLOSE(1)
      RETURN
   END IF
!
   WRITE(1,20)
   WRITE(1,2270)
   WRITE(1,20)
   WRITE(1,2280)tertconj
   WRITE(1,2290)tertperiod
   WRITE(1,2300)p1ptc
   WRITE(1,2310)p1mtc
   WRITE(1,2320)p1qtc
   WRITE(1,2330)t1off
   WRITE(1,2340)tertt0
   WRITE(1,2350)tertecos
   WRITE(1,2360)tertesin
   WRITE(1,2370)sqtertecos
   WRITE(1,2380)sqtertesin
   WRITE(1,2390)tertincl
   WRITE(1,2400)tertomega
   WRITE(1,2410)angsum1
   WRITE(1,2420)angdiff1
   WRITE(1,20)
   WRITE(1,2430)
   WRITE(1,20)
   WRITE(1,2440)nalph3,nbet3
   WRITE(1,2450)t3
   WRITE(1,2460)g3
   WRITE(1,2470)tertratrad
   WRITE(1,2480)tertq
   WRITE(1,2490)omega3
   WRITE(1,2500)bigi3
   WRITE(1,2510)bigbeta3
   WRITE(1,2520)dwavex(1,3),dwavey(1,3)
   WRITE(1,2530)dwavex(2,3),dwavey(2,3)
   WRITE(1,2540)dwavex(3,3),dwavey(3,3)
   WRITE(1,2550)dwavex(4,3),dwavey(4,3)
   WRITE(1,2560)dwavex(5,3),dwavey(5,3)
   WRITE(1,2570)dwavex(6,3),dwavey(6,3)
   WRITE(1,2580)dwavex(7,3),dwavey(7,3)
   WRITE(1,2590)dwavex(8,3),dwavey(8,3)
   WRITE(1,2600)rk3
   WRITE(1,2610)sdarkint3(1)
   WRITE(1,2620)sdarkint3(2)
   WRITE(1,2630)sdarkint3(3)
   WRITE(1,2640)sdarkint3(4)
   WRITE(1,2650)sdarkint3(5)
   WRITE(1,2660)sdarkint3(6)
   WRITE(1,2670)sdarkint3(7)
   WRITE(1,2680)sdarkint3(8)
!
!  If Nbody < 4, then we are done.
!
   IF(isw30 < 4)THEN
      CLOSE(1)
      RETURN
   END IF
!
   WRITE(1,20)
   WRITE(1,2690)
   WRITE(1,20)
   WRITE(1,2700)p2tconj
   WRITE(1,2710)p2period
   WRITE(1,2720)p2ptc
   WRITE(1,2730)p2mtc
   WRITE(1,2740)p2qtc
   WRITE(1,2750)t2off
   WRITE(1,2760)p2t0
   WRITE(1,2770)p2ecos
   WRITE(1,2780)p2esin
   WRITE(1,2790)sqp2ecos
   WRITE(1,2800)sqp2esin
   WRITE(1,2810)p2incl
   WRITE(1,2820)p2omega
   WRITE(1,2830)angsum2
   WRITE(1,2840)angdiff2
   WRITE(1,20)
   WRITE(1,2850)
   WRITE(1,20)
   WRITE(1,2860)sw80
   WRITE(1,2870)sw82
   WRITE(1,2880)p2ratrad
   WRITE(1,2890)p2q
   WRITE(1,2900)omega4
   WRITE(1,2910)bigi4
   WRITE(1,2920)bigbeta4
   WRITE(1,2930)dwavex(1,4),dwavey(1,4)
   WRITE(1,2940)dwavex(2,4),dwavey(2,4)
   WRITE(1,2950)dwavex(3,4),dwavey(3,4)
   WRITE(1,2960)dwavex(4,4),dwavey(4,4)
   WRITE(1,2970)dwavex(5,4),dwavey(5,4)
   WRITE(1,2980)dwavex(6,4),dwavey(6,4)
   WRITE(1,2990)dwavex(7,4),dwavey(7,4)
   WRITE(1,3000)dwavex(8,4),dwavey(8,4)
   WRITE(1,3010)rk4
   WRITE(1,3020)sdarkint4(1)
   WRITE(1,3030)sdarkint4(2)
   WRITE(1,3040)sdarkint4(3)
   WRITE(1,3050)sdarkint4(4)
   WRITE(1,3060)sdarkint4(5)
   WRITE(1,3070)sdarkint4(6)
   WRITE(1,3080)sdarkint4(7)
   WRITE(1,3090)sdarkint4(8)
   WRITE(1,20)
   WRITE(1,3100)
   WRITE(1,20)
   WRITE(1,3110)b2masssum
   WRITE(1,3120)b2massdiff
   WRITE(1,3130)bin2q
   WRITE(1,3140)b2radsum
   WRITE(1,3150)b2raddiff
   WRITE(1,3160)bin2ratrad
   WRITE(1,3170)bin2m3
   WRITE(1,3180)bin2m4
   WRITE(1,3190)bin2r3
   WRITE(1,3200)bin2r4
!
!  If Nbody < 5, then we are done.
!
   IF(isw30 < 5)THEN
      CLOSE(1)
      RETURN
   END IF
!
   WRITE(1,20)
   WRITE(1,3210)
   WRITE(1,20)
   WRITE(1,3220)p3tconj
   WRITE(1,3230)p3period
   WRITE(1,3240)p3ptc
   WRITE(1,3250)p3mtc
   WRITE(1,3260)p3qtc
   WRITE(1,3270)t3off
   WRITE(1,3280)p3t0
   WRITE(1,3290)p3ecos
   WRITE(1,3300)p3esin
   WRITE(1,3310)sqp3ecos
   WRITE(1,3320)sqp3esin
   WRITE(1,3330)p3incl
   WRITE(1,3340)p3omega
   WRITE(1,3350)angsum3
   WRITE(1,3360)angdiff3
   WRITE(1,20)
   WRITE(1,3370)
   WRITE(1,20)
   WRITE(1,3380)sw81
   WRITE(1,3390)sw83
   WRITE(1,3400)p3ratrad
   WRITE(1,3410)p3q
   WRITE(1,3420)omega5
   WRITE(1,3430)fake
   WRITE(1,3440)fake
   WRITE(1,3450)dwavex(1,5),dwavey(1,5)
   WRITE(1,3460)dwavex(2,5),dwavey(2,5)
   WRITE(1,3470)dwavex(3,5),dwavey(3,5)
   WRITE(1,3480)dwavex(4,5),dwavey(4,5)
   WRITE(1,3490)dwavex(5,5),dwavey(5,5)
   WRITE(1,3500)dwavex(6,5),dwavey(6,5)
   WRITE(1,3510)dwavex(7,5),dwavey(7,5)
   WRITE(1,3520)dwavex(8,5),dwavey(8,5)
   WRITE(1,3530)rk5
   WRITE(1,3540)sdarkint5(1)
   WRITE(1,3550)sdarkint5(2)
   WRITE(1,3560)sdarkint5(3)
   WRITE(1,3570)sdarkint5(4)
   WRITE(1,3580)sdarkint5(5)
   WRITE(1,3590)sdarkint5(6)
   WRITE(1,3600)sdarkint5(7)
   WRITE(1,3610)sdarkint5(8)
!
!  If Nbody < 6, then we are done.
!
   IF(isw30 < 6)THEN
      CLOSE(1)
      RETURN
   END IF
!
   WRITE(1,20)
   WRITE(1,3620)
   WRITE(1,20)
   WRITE(1,3630)p4tconj
   WRITE(1,3640)p4period
   WRITE(1,3650)p4ptc
   WRITE(1,3660)p4mtc
   WRITE(1,3670)p4qtc
   WRITE(1,3680)t4off
   WRITE(1,3690)p4t0
   WRITE(1,3700)p4ecos
   WRITE(1,3710)p4esin
   WRITE(1,3720)sqp4ecos
   WRITE(1,3730)sqp4esin
   WRITE(1,3740)p4incl
   WRITE(1,3750)p4omega
   WRITE(1,3760)angsum4
   WRITE(1,3770)angdiff4
   WRITE(1,20)
   WRITE(1,3780)
   WRITE(1,20)
   WRITE(1,3790)t6
   WRITE(1,3800)g6
   WRITE(1,3810)p4ratrad
   WRITE(1,3820)p4q
   WRITE(1,3830)omega6
   WRITE(1,3840)fake
   WRITE(1,3850)fake
   WRITE(1,3860)dwavex(1,6),dwavey(1,6)
   WRITE(1,3870)dwavex(2,6),dwavey(2,6)
   WRITE(1,3880)dwavex(3,6),dwavey(3,6)
   WRITE(1,3890)dwavex(4,6),dwavey(4,6)
   WRITE(1,3900)dwavex(5,6),dwavey(5,6)
   WRITE(1,3910)dwavex(6,6),dwavey(6,6)
   WRITE(1,3920)dwavex(7,6),dwavey(7,6)
   WRITE(1,3930)dwavex(8,6),dwavey(8,6)
   WRITE(1,3940)rk6
   WRITE(1,3950)sdarkint6(1)
   WRITE(1,3960)sdarkint6(2)
   WRITE(1,3970)sdarkint6(3)
   WRITE(1,3980)sdarkint6(4)
   WRITE(1,3990)sdarkint6(5)
   WRITE(1,4000)sdarkint6(6)
   WRITE(1,4010)sdarkint6(7)
   WRITE(1,4020)sdarkint6(8)
!
!  If Nbody < 7, then we are done.
!
   IF(isw30 < 7)THEN
      CLOSE(1)
      RETURN
   END IF
!
   WRITE(1,20)
   WRITE(1,4030)
   WRITE(1,20)
   WRITE(1,4040)p5tconj
   WRITE(1,4050)p5period
   WRITE(1,4060)p5ptc
   WRITE(1,4070)p5mtc
   WRITE(1,4080)p5qtc
   WRITE(1,4090)t5off
   WRITE(1,4100)p5t0
   WRITE(1,4110)p5ecos
   WRITE(1,4120)p5esin
   WRITE(1,4130)sqp5ecos
   WRITE(1,4140)sqp5esin
   WRITE(1,4150)p5incl
   WRITE(1,4160)p5omega
   WRITE(1,4170)angsum5
   WRITE(1,4180)angdiff5
   WRITE(1,20)
   WRITE(1,4190)
   WRITE(1,20)
   WRITE(1,4200)t7
   WRITE(1,4210)g7
   WRITE(1,4220)p5ratrad
   WRITE(1,4230)p5q
   WRITE(1,4240)omega7
   WRITE(1,4250)fake
   WRITE(1,4260)fake
   WRITE(1,4270)dwavex(1,7),dwavey(1,7)
   WRITE(1,4280)dwavex(2,7),dwavey(2,7)
   WRITE(1,4290)dwavex(3,7),dwavey(3,7)
   WRITE(1,4300)dwavex(4,7),dwavey(4,7)
   WRITE(1,4310)dwavex(5,7),dwavey(5,7)
   WRITE(1,4320)dwavex(6,7),dwavey(6,7)
   WRITE(1,4330)dwavex(7,7),dwavey(7,7)
   WRITE(1,4340)dwavex(8,7),dwavey(8,7)
   WRITE(1,4350)rk7
   WRITE(1,4360)sdarkint7(1)
   WRITE(1,4370)sdarkint7(2)
   WRITE(1,4380)sdarkint7(3)
   WRITE(1,4390)sdarkint7(4)
   WRITE(1,4400)sdarkint7(5)
   WRITE(1,4410)sdarkint7(6)
   WRITE(1,4420)sdarkint7(7)
   WRITE(1,4430)sdarkint7(8)
!
!  If Nbody < 8, then we re done.
!
   IF(isw30 < 8)THEN
      CLOSE(1)
      RETURN
   END IF
!
   WRITE(1,20)
   WRITE(1,4440)
   WRITE(1,20)
   WRITE(1,4450)p6tconj
   WRITE(1,4460)p6period
   WRITE(1,4470)p6ptc
   WRITE(1,4480)p6mtc
   WRITE(1,4490)p6qtc
   WRITE(1,4500)t6off
   WRITE(1,4510)p6t0
   WRITE(1,4520)p6ecos
   WRITE(1,4530)p6esin
   WRITE(1,4540)sqp6ecos
   WRITE(1,4550)sqp6esin
   WRITE(1,4560)p6incl
   WRITE(1,4570)p6omega
   WRITE(1,4580)angsum6
   WRITE(1,4590)angdiff6
   WRITE(1,20)
   WRITE(1,4600)
   WRITE(1,20)
   WRITE(1,4610)t8
   WRITE(1,4620)g8
   WRITE(1,4630)p6ratrad
   WRITE(1,4640)p6q
   WRITE(1,4650)omega8
   WRITE(1,4660)fake
   WRITE(1,4670)fake
   WRITE(1,4680)dwavex(1,8),dwavey(1,8)
   WRITE(1,4690)dwavex(2,8),dwavey(2,8)
   WRITE(1,4700)dwavex(3,8),dwavey(3,8)
   WRITE(1,4710)dwavex(4,8),dwavey(4,8)
   WRITE(1,4720)dwavex(5,8),dwavey(5,8)
   WRITE(1,4730)dwavex(6,8),dwavey(6,8)
   WRITE(1,4740)dwavex(7,8),dwavey(7,8)
   WRITE(1,4750)dwavex(8,8),dwavey(8,8)
   WRITE(1,4760)rk8
   WRITE(1,4770)sdarkint8(1)
   WRITE(1,4780)sdarkint8(2)
   WRITE(1,4790)sdarkint8(3)
   WRITE(1,4800)sdarkint8(4)
   WRITE(1,4810)sdarkint8(5)
   WRITE(1,4820)sdarkint8(6)
   WRITE(1,4830)sdarkint8(7)
   WRITE(1,4840)sdarkint8(8)
!
!  If Nbody < 9, then we are done.
!
   IF(isw30 < 9)THEN
      CLOSE(1)
      RETURN
   END IF
!
   WRITE(1,20)
   WRITE(1,4850)
   WRITE(1,20)
   WRITE(1,4860)p7tconj
   WRITE(1,4870)p7period
   WRITE(1,4880)p7ptc
   WRITE(1,4890)p7mtc
   WRITE(1,4900)p7qtc
   WRITE(1,4910)t7off
   WRITE(1,4920)p7t0
   WRITE(1,4930)p7ecos
   WRITE(1,4940)p7esin
   WRITE(1,4950)sqp7ecos
   WRITE(1,4960)sqp7esin
   WRITE(1,4970)p7incl
   WRITE(1,4980)p7omega
   WRITE(1,4990)angsum7
   WRITE(1,5000)angdiff7
   WRITE(1,20)
   WRITE(1,5010)
   WRITE(1,20)
   WRITE(1,5020)t9
   WRITE(1,5030)g9
   WRITE(1,5040)p7ratrad
   WRITE(1,5050)p7q
   WRITE(1,5060)omega9
   WRITE(1,5070)fake
   WRITE(1,5080)fake
   WRITE(1,5090)dwavex(1,9),dwavey(1,9)
   WRITE(1,5100)dwavex(2,9),dwavey(2,9)
   WRITE(1,5110)dwavex(3,9),dwavey(3,9)
   WRITE(1,5120)dwavex(4,9),dwavey(4,9)
   WRITE(1,5130)dwavex(5,9),dwavey(5,9)
   WRITE(1,5140)dwavex(6,9),dwavey(6,9)
   WRITE(1,5150)dwavex(7,9),dwavey(7,9)
   WRITE(1,5160)dwavex(8,9),dwavey(8,9)
   WRITE(1,5170)rk9
   WRITE(1,5180)sdarkint9(1)
   WRITE(1,5190)sdarkint9(2)
   WRITE(1,5200)sdarkint9(3)
   WRITE(1,5210)sdarkint9(4)
   WRITE(1,5220)sdarkint9(5)
   WRITE(1,5230)sdarkint9(6)
   WRITE(1,5240)sdarkint9(7)
   WRITE(1,5250)sdarkint9(8)
!
!  If Nbody < 10, then we are done.
!
   IF(isw30 < 10)THEN
      CLOSE(1)
      RETURN
   END IF
!
   WRITE(1,20)
   WRITE(1,5260)
   WRITE(1,20)
   WRITE(1,5270)p8tconj
   WRITE(1,5280)p8period
   WRITE(1,5290)p8ptc
   WRITE(1,5300)p8mtc
   WRITE(1,5310)p8qtc
   WRITE(1,5320)t8off
   WRITE(1,5330)p8t0
   WRITE(1,5340)p8ecos
   WRITE(1,5350)p8esin
   WRITE(1,5360)sqp8ecos
   WRITE(1,5370)sqp8esin
   WRITE(1,5380)p8incl
   WRITE(1,5390)p8omega
   WRITE(1,5400)angsum8
   WRITE(1,5410)angdiff8
   WRITE(1,20)
   WRITE(1,5420)
   WRITE(1,20)
   WRITE(1,5430)t10
   WRITE(1,5440)g10
   WRITE(1,5450)p8ratrad
   WRITE(1,5460)p8q
   WRITE(1,5470)omega10
   WRITE(1,5480)fake
   WRITE(1,5490)fake
   WRITE(1,5500)dwavex(1,10),dwavey(1,10)
   WRITE(1,5510)dwavex(2,10),dwavey(2,10)
   WRITE(1,5520)dwavex(3,10),dwavey(3,10)
   WRITE(1,5530)dwavex(4,10),dwavey(4,10)
   WRITE(1,5540)dwavex(5,10),dwavey(5,10)
   WRITE(1,5550)dwavex(6,10),dwavey(6,10)
   WRITE(1,5560)dwavex(7,10),dwavey(7,10)
   WRITE(1,5570)dwavex(8,10),dwavey(8,10)
   WRITE(1,5580)rk10
   WRITE(1,5590)sdarkint10(1)
   WRITE(1,5600)sdarkint10(2)
   WRITE(1,5610)sdarkint10(3)
   WRITE(1,5620)sdarkint10(4)
   WRITE(1,5630)sdarkint10(5)
   WRITE(1,5640)sdarkint10(6)
   WRITE(1,5650)sdarkint10(7)
   WRITE(1,5660)sdarkint10(8)
!
   CLOSE(1)
!
5  FORMAT(i7)
10 FORMAT('#1 New version 7 input, this line required')
15 FORMAT('#2 New version 7.1 input, this line required')
20 FORMAT('!')
30 FORMAT('!  Lines starting with # or ! can be inserted',' anyw'  &
      ,'here')
40 FORMAT('!  Units')
50 FORMAT(i1,24X,'iunit')
60 FORMAT('!  Parameters for light curve sampling')
70 FORMAT(i1,24X,'itime')
80 FORMAT(f15.8,10X,'t_start (if itime=2)')
90 FORMAT(f15.8,10X,'t_end   (if itime=2)')
100 FORMAT(f11.8,14X,'time step in days (if itime=2)')
110 FORMAT(f10.6,15X,'dphase in degrees (if itime=0 or 1)')
120 FORMAT('!  Control flags for filters and limb darkening')
130 FORMAT(i1,24X,'iatm (0 for black body)')
140 FORMAT(8(i1,1X),9X,'icnU,icnB,icnV,icnR,icnI,icnJ,icnH,icnK')
150 FORMAT(i1,24X,'iRVfilt')
160 FORMAT(i3,22X,'ilaw (1=lin., 2=log, 3=sqrt, 4=quad,',' 5=Kipp'  &
      ,'ing quad)')
170 FORMAT(f11.4,14X,'central wavelength for filter ',i1,' when i'  &
      ,'atm=0')
180 FORMAT(i2,23X,'Nref')
190 FORMAT(f10.5,15X,'log10(Lx) in erg/sec, tag Lx')
200 FORMAT(i1,24X,'X-ray foreshortening switch (1 for',' point so'  &
      ,'urce)')
210 FORMAT(i1,24X,'idark1')
220 FORMAT(i1,24X,'idark2')
230 FORMAT(i1,24X,'ispotprof (0=constant, 1=linear, ','2=Gaussian'  &
      ,' change in temperature profile')
240 FORMAT(i1,24X,'igrav')
250 FORMAT(i1,24X,'set to 1 to use fluxes')
260 FORMAT('!  Third light parameters')
270 FORMAT(f18.10,7X,'T3 (Kelvin), tag t3')
280 FORMAT(f15.11,10X,'g3 (log(g) in c.g.s), tag g3')
290 FORMAT(f22.16,3X,'SA3 (ratio of body 1 to body 3 areas),',' t'  &
      ,'ag SA')
300 FORMAT('!  Control flags related to speed and accuracy')
310 FORMAT(i6,19X,'Nterms for fast analytic')
320 FORMAT(i1,24X,'mandel (0 for Gimenez, 1 for Mandel &',' Agol)' )
330 FORMAT(i4,21X,'Ngap')
340 FORMAT(i2,23X,'iecheck')
350 FORMAT(i1,24X,'ism1')
360 FORMAT(i1,24X,'ifasttrans (>0 for fast transit mode)')
370 FORMAT(i6,19X,'MonteCarlo (0 for interpolation, >10 ','for Mo'  &
      ,'nte Carlo integration)')
380 FORMAT(f12.6,13X,'phaselow  (phase range if phaselow>0 and',''  &
      ,' phasehigh>0')
390 FORMAT(f12.6,13X,'phasehigh  and phaselow < phasehigh)')
400 FORMAT('!  Control flags for Roche geometry')
410 FORMAT(i1,24X,'isquare')
420 FORMAT(i1,24X,'iusepot')
430 FORMAT(f12.6,13X,'usepot1')
440 FORMAT(f12.6,13X,'usepot2')
450 FORMAT('!  Drawing flags')
460 FORMAT(i1,24X,'idraw (1 to write output drawing files)')
470 FORMAT(i1,24X,'ionephase')
480 FORMAT(f12.6,13X,'onephase')
490 FORMAT('!  Control flags for orbital elements')
500 FORMAT(f12.9,13X,'pshift, tag ps')
510 FORMAT(i1,24X,'ikeep (1 to put primary eclipse at phase 0.0)')
520 FORMAT(i1,24X,'isynch (1 to keep rotation synchronous',' at p'  &
      ,'eriastron)')
530 FORMAT(i1,24X,'ialign (0 for rotation aligned with orbit)')
540 FORMAT(i1,24X,'set to 1 to fit for Tconj of binary')
550 FORMAT(i1,24X,'set to 1 to fit for e*cos(omega), ','e*sin(ome'  &
      ,'ga) of binary')
560 FORMAT('!  Control flags for use with optimizers')
570 FORMAT(i2,23X,'imag ','(0 for input data in mag, 1 for input '  &
      ,'data in flux)')
580 FORMAT(i4,21X,'ielite')
590 FORMAT(i2,23X,'ifixgamma (0, 1, 2) or (10, 11, 12)')
600 FORMAT(i1,24X,'iwriteeclipse (1 to fit for eclipse times ','w'  &
      ,'hen Nbody >=3 and itime=2)')
610 FORMAT(i1,24X,'frac switch (>1 to enable ELCratio.* files)')
620 FORMAT(i1,24X,'set to 1 to supress optimizer screen output')
630 FORMAT(i1,24X,'set to 1 to supress demcmcELC output files')
640 FORMAT(f15.5,10X,'chi^2 threshold to write to output files')
650 FORMAT(f3.1,22X,'median fit (0 for chi^2, >0 for median)')
660 FORMAT(i14,11X,'jdum (seed for markovELC, geneticELC, ','rand'  &
      ,'omELC, demcmcELC, hammerELC)')
670 FORMAT('!  Control flags and parameters for use with ','the d'  &
      ,'ynamical integrator (Nbody >=3)')
680 FORMAT(i2,23X,'Ndynwin (number of segments to integrate)')
690 FORMAT(f15.6,10X,'Tref for dynamical integrator (Nbody >=3)')
700 FORMAT(f8.5,17X,'hh (step size in days)')
710 FORMAT(i1,24X,'iGR (1 for GR, 2 for tidal, 3 for both)')
720 FORMAT(i1,24X,'set to 1 for binary+binary model (use ','Nbody'  &
      ,'=4)')
730 FORMAT(i1,24X,'itconj (0=T_peri, 1=T_tran, 2=T_occul)')
740 FORMAT(i1,24X,'set to 1 for logarithmic mass ratios')
750 FORMAT(i1,24X,'set to 1 for planet radii in Earth radii')
760 FORMAT(i1,24X,'set to 1 to treat transits and occultations ',  &
      'together')
770 FORMAT(i1,24X,'set to 1 for transit penalty')
780 FORMAT(i1,24X,'set to 1 for secondary eclipse penalty')
790 FORMAT(f15.4,10X,'chi^2 penalty for transit or secondary ','e'  &
      ,'clipse')
800 FORMAT(i1,24X,'set to 1 for informational output (ELC only)')
810 FORMAT('! Features for Kepler data')
820 FORMAT(f16.13,9X,'Kepler contamination, tag co')
830 FORMAT(i1,24X,'Iseason (1 for seasonal Kepler contamination)')
840 FORMAT(f16.13,9X,'contamS0 (season 0 contamination, tag s0)')
850 FORMAT(f16.13,9X,'contamS1 (season 1 contamination, tag s1)')
860 FORMAT(f16.13,9X,'contamS2 (season 2 contamination, tag s2)')
870 FORMAT(f16.13,9X,'contamS3 (season 3 contamination, tag s3)')
880 FORMAT(i4,21X,'Nseg (number of segments to fit for','contamin'  &
      ,'ation)')
890 FORMAT(i1,24X,'set to 1 for fast Kepler binning')
900 FORMAT(f10.5,15X,'bin size for light curves (minutes)')
910 FORMAT(f10.5,15X,'bin size for RV curves (minutes)')
920 FORMAT(i2,23X,'NSC (number of short cadence intervals)')
!
!  If iversion=1, then add the TESS block
!
921 FORMAT('!')
922 FORMAT('! Features for TESS data')
923 FORMAT('!')
924 FORMAT(f16.13,9X,'TESS contamination, tag ct')
925 FORMAT(i1,24X,'TESS filter in ELC.atm')
926 FORMAT(f10.5,15X,'bin size for TESS light curve (minutes)')
!
930 FORMAT('!  Number of bodies (2 to 10)')
940 FORMAT(i2,23X,'Nbody')
950 FORMAT('!  Binary orbit parameters')
960 FORMAT(f22.15,3X,'Period (days), tag pe')
970 FORMAT(f22.15,3X,'T0 (time of periastron passage), tag T0')
980 FORMAT(f22.15,3X,'Tconj (time of primary eclipse), tag Tc')
990 FORMAT(f20.13,5X,'PbpTc (period + Tconj for binary), tag bp')
1000 FORMAT(f20.13,5X,'PbmTc (period - Tconj for binary), tag bm')
1010 FORMAT(f17.12,8X,'binqTc (slope in P-Tc plane for binary),',  &
      1X,'tag bq')
1020 FORMAT(f21.12,4X,'Tbinoff (offset in P-Tc plane for binary),',  &
      1X,'tag bt')
1030 FORMAT(f15.13,10X,'eccentricity, tag ec')
1040 FORMAT(f19.13,6X,'argument of periaston in degrees, tag ar')
1050 FORMAT(f20.17,5X,'e*cos(omega), tag oc')
1060 FORMAT(f20.17,5X,'e*sin(omega), tag os')
1070 FORMAT(f20.17,5X,'sqrt(e)*cos(omega), tag bc')
1080 FORMAT(f20.17,5X,'sqrt(e)*sin(omega), tag bs')
1090 FORMAT(f22.16,3X,'finc (inclination in degrees), tag in')
1100 FORMAT(f15.10,10X,'Omega_bin (nodal angle of binary ','in deg'  &
      ,'rees), tag Ob')
1110 FORMAT(f19.13,6X,'primK (K-velocity of star 1 in km/sec),',' '  &
      ,'tag pk')
1120 FORMAT(f12.6,13X,'separ (semimajor axis in solar radii), ','t'  &
      ,'ag se')
1130 FORMAT(f12.6,13X,'gamma')
1140 FORMAT(f10.7,15X,'ecosw (phase difference between eclipses)',  &
      ' tag dp')
1150 FORMAT('!  Body 1 parameters')
1160 FORMAT(2(i4,1X),15X,'Nalph1, Nbet1')
1170 FORMAT(f15.9,10X,'Teff1 (K), tag T1')
1180 FORMAT(f10.7,15X,'Tgrav1, tag g1')
1190 FORMAT(f10.7,15X,'alb1 (albedo of star 1), tag l1')
1200 FORMAT(f18.13,7X,'omega1, tag o1')
1210 FORMAT(f15.13,10X,'fill1, tag f1')
1220 FORMAT(f20.13,5X,'primrad (star 1 radius in solar radii),',' '  &
      ,'tag pr')
1230 FORMAT(f18.16,7X,'frac1 (fractional radius star 1:','  R_1/a',  &
      '), tag q1')
1240 FORMAT(f15.13,10X,'radfill1 (set to use fill1',' in terms of '  &
      ,'R_eff)')
1250 FORMAT(f22.17,3X,'primmass (star 1 mass in solar masses),',' '  &
      ,'tag pm')
1260 FORMAT(f12.7,13X,'axis_I1 (inclination of rotation axis if ',  &
      'ialign=1), tag ai')
1270 FORMAT(f12.7,13X,'axis_beta1 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag ab')
1280 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','1, tag x1, y1')
1290 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','2, tag x2, y2')
1300 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','3, tag x3, y3')
1310 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','4, tag x4, y4')
1320 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','5, tag x5, y5')
1330 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','6, tag x6, y6')
1340 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','7, tag x7, y7')
1350 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','8, tag x8, y8')
1360 FORMAT(2(f8.5,1X),7X,'bolometric L.D. coefficients',' (used w'  &
      ,'hen Nref >= 0)')
1370 FORMAT(f10.7,15X,'Temperature factor spot 1, star 1, tag b1')
1380 FORMAT(f11.7,14X,'Latitude of spot 1, star 1 (degrees),',' ta'  &
      ,'g b2')
1390 FORMAT(f11.7,14X,'Longitude of spot 1, star 1 (degrees),',' t'  &
      ,'ag b3')
1400 FORMAT(f11.7,14X,'Angular radius of spot 1, star 1 ','(degree'  &
      ,'s), tag b4')
1410 FORMAT(f10.7,15X,'Temperature factor spot 2, star 1, tag b5')
1420 FORMAT(f11.7,14X,'Latitude of spot 2, star 1 (degrees),',' ta'  &
      ,'g b6')
1430 FORMAT(f11.7,14X,'Longitude of spot 2, star 1 (degrees),',' t'  &
      ,'ag b7')
1440 FORMAT(f11.7,14X,'Angular radius of spot 2, star 1 ','(degree'  &
      ,'s), tag b8')
1450 FORMAT(f13.10,12X,'beam1 (Doppler boosting factor,',' star 1)'  &
      ,', tag e1')
1460 FORMAT(f10.8,15X,'rk1 (apsidal constant, star 1), tag a1')
1470 FORMAT(f20.11,5X,'flux star 1, filter 1 (need Nterms > 0),',''  &
      ,' tag 11')
1480 FORMAT(f20.11,5X,'flux star 1, filter 2 (need Nterms > 0),',''  &
      ,' tag 12')
1490 FORMAT(f20.11,5X,'flux star 1, filter 3 (need Nterms > 0),',''  &
      ,' tag 13')
1500 FORMAT(f20.11,5X,'flux star 1, filter 4 (need Nterms > 0),',''  &
      ,' tag 14')
1510 FORMAT(f20.11,5X,'flux star 1, filter 5 (need Nterms > 0),',''  &
      ,' tag 15')
1520 FORMAT(f20.11,5X,'flux star 1, filter 6 (need Nterms > 0),',''  &
      ,' tag 16')
1530 FORMAT(f20.11,5X,'flux star 1, filter 7 (need Nterms > 0),',''  &
      ,' tag 17')
1540 FORMAT(f20.11,5X,'flux star 1, filter 8 (need Nterms > 0),',''  &
      ,' tag 18')
1550 FORMAT('!  Body 2 parameters')
1560 FORMAT(2(i4,1X),15X,'Nalph2, Nbet2')
1570 FORMAT(f15.9,10X,'Teff2 (K), tag T2')
1580 FORMAT(f20.16,5X,'temprat (T_2/T_1), tag te')
1590 FORMAT(f10.7,15X,'Tgrav2, tag g2')
1600 FORMAT(f10.7,15X,'alb2 (albedo of star 2), tag l2')
1610 FORMAT(f18.13,7X,'omega2, tag o2')
1620 FORMAT(f15.13,10X,'fill2, tag f2')
1630 FORMAT(f15.13,10X,'fillsum, tag sf')
1640 FORMAT(f16.13,9X,'filldiff, tag sd')
1650 FORMAT(f17.13,8X,'secrad (secondary star radius in solar ','r'  &
      ,'adii), tag sr')
1660 FORMAT(f18.16,7X,'frac2 (fractional radius star 2:','  R_2/a',  &
      '), tag q2')
1670 FORMAT(f20.13,5X,'ratrad (ratio of star 1 radius to ','star 2'  &
      ,' radius),  tag ra')
1680 FORMAT(f20.13,5X,'radsum (sum of radii in solar), ','tag rs')
1690 FORMAT(f20.13,5X,'raddiff (R_1 - R_2  in solar), ','tag rd')
1700 FORMAT(f17.15,8X,'fracsum ((R_1 + R_2)/a), tag fs')
1710 FORMAT(f17.15,8X,'fracdiff ((R_1 - R_2)/a), tag fd')
1720 FORMAT(f15.13,10X,'radfill2 (set to use fill2',' in terms of '  &
      ,'R_eff)')
1730 FORMAT(f24.17,1X,'Q (M_2/M_1), tag ma')
1740 FORMAT(f20.15,5X,'secmass (star 2 mass in solar masses),',' t'  &
      ,'ag sm')
1750 FORMAT(f20.15,5X,'masssum (sum of masses in solar), ',' tag m' ,'s')
1760 FORMAT(f20.15,5X,'massdiff (M_1 - M_2 in solar), tag md')
1770 FORMAT(f12.7,13X,'axis_I2 (inclination of rotation axis if ',  &
      'ialign=1), tag bi')
1780 FORMAT(f12.7,13X,'axis_beta2 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag bb')
1790 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','1, tag z1, w1')
1800 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','2, tag z2, w2')
1810 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','3, tag z3, w3')
1820 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','4, tag z4, w4')
1830 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','5, tag z5, w5')
1840 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','6, tag z6, w6')
1850 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','7, tag z7, w7')
1860 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','8, tag z8, w8')
1870 FORMAT(2(f8.5,1X),7X,'bolometric L.D. coefficients',' (used w'  &
      ,'hen Nref >= 0)')
1880 FORMAT(f10.7,15X,'Temperature factor spot 1, star 2, tag c1')
1890 FORMAT(f11.7,14X,'Latitude of spot 1, star 2 (degrees),',' ta'  &
      ,'g c2')
1900 FORMAT(f11.7,14X,'Longitude of spot 1, star 2 (degrees),',' t'  &
      ,'ag c3')
1910 FORMAT(f11.7,14X,'Angular radius of spot 1, star 2 ','(degree'  &
      ,'s), tag c4')
1920 FORMAT(f10.7,15X,'Temperature factor spot 2, star 2, tag c5')
1930 FORMAT(f11.7,14X,'Latitude of spot 2, star 2 (degrees),',' ta'  &
      ,'g c6')
1940 FORMAT(f11.7,14X,'Longitude of spot 2, star 2 (degrees),',' t'  &
      ,'ag c7')
1950 FORMAT(f11.7,14X,'Angular radius of spot 2, star 2 ','(degree'  &
      ,'s), tag c8')
1960 FORMAT(f13.10,12X,'beam2 (Doppler boosting factor,',' star 2)'  &
      ,', tag e2')
1970 FORMAT(f10.8,15X,'rk2 (apsidal constant, star 2), tag a2')
1980 FORMAT(f20.11,5X,'flux star 2, filter 1 (need Nterms > 0),',''  &
      ,' tag 21')
1990 FORMAT(f20.11,5X,'flux star 2, filter 2 (need Nterms > 0),',''  &
      ,' tag 22')
2000 FORMAT(f20.11,5X,'flux star 2, filter 3 (need Nterms > 0),',''  &
      ,' tag 23')
2010 FORMAT(f20.11,5X,'flux star 2, filter 4 (need Nterms > 0),',''  &
      ,' tag 24')
2020 FORMAT(f20.11,5X,'flux star 2, filter 5 (need Nterms > 0),',''  &
      ,' tag 25')
2030 FORMAT(f20.11,5X,'flux star 2, filter 6 (need Nterms > 0),',''  &
      ,' tag 26')
2040 FORMAT(f20.11,5X,'flux star 2, filter 7 (need Nterms > 0),',''  &
      ,' tag 27')
2050 FORMAT(f20.11,5X,'flux star 2, filter 8 (need Nterms > 0),',''  &
      ,' tag 28')
2060 FORMAT('!  Accretion disk parameters')
2070 FORMAT(i1,24X,'idint (1 for accretion disk)')
2080 FORMAT(i3,22X,'Ntheta')
2090 FORMAT(i3,22X,'Nradius')
2100 FORMAT(f11.8,14X,'beta_rim (opening angle in degrees),',' tag'  &
      ,' be')
2110 FORMAT(f11.9,14X,'rinner (radius of inner hole), tag ri')
2120 FORMAT(f11.9,14X,'router (radius of outer disk), tag ro')
2130 FORMAT(f13.6,12X,'Tdisk (temperature of inner edge in K)',' t'  &
      ,'ag td')
2140 FORMAT(f12.9,13X,'xi (power-law exponent on temperature',' pr'  &
      ,'ofile), tag xi')
2150 FORMAT(f13.10,12X,'Temperature factor spot 1, disk, tag d1')
2160 FORMAT(f13.9,12X,'Azimuth of spot 1, disk (degrees), tag d2')
2170 FORMAT(f13.9,12X,'Radial cutoff of spot 1, disk (0 <= ',' r_c'  &
      ,'ut <=1), tag d3')
2180 FORMAT(f13.9,12X,'Angular size of spot 1, disk (degrees),',' '  &
      ,'tag d4')
2190 FORMAT(f13.10,12X,'Temperature factor spot 2, disk, tag d5')
2200 FORMAT(f13.9,12X,'Azimuth of spot 2, disk (degrees), tag d6')
2210 FORMAT(f13.9,12X,'Radial cutoff of spot 2, disk (0 <= r_cut',  &
      ' <=1), tag d7')
2220 FORMAT(f13.9,12X,'Angular size of spot 2, disk (degrees),',' '  &
      ,'tag d8')
2230 FORMAT(f15.9,10X,'reference phase for disk fraction')
2240 FORMAT('!  Pulsar parameters')
2250 FORMAT(f17.11,8X,'asini (projected semimajor axis in ','secon'  &
      ,'ds)')
2260 FORMAT(f13.10,12X,'asini error')
2270 FORMAT('!  Body 3 orbital parameters')
2280 FORMAT(f21.13,4X,'P1Tconj, tag tj')
2290 FORMAT(f22.14,3X,'P1period (days), tag tt')
2300 FORMAT(f21.13,4X,'P1pTc (period + Tconj for body 3), tag 1p')
2310 FORMAT(f21.13,4X,'P1mTc (period - Tconj for body 3), tag 1m')
2320 FORMAT(f17.12,8X,'P1qTc (slope in P-Tc plane for body 3),',1X,  &
      'tag 1q')
2330 FORMAT(f21.12,4X,'T1off (offset in P-Tc plane for body 3),',  &
      1X,'tag 1t')
2340 FORMAT(f21.13,4X,'P1T0, tag tu')
2350 FORMAT(f20.17,5X,'P1e*cos(omega), tag tv')
2360 FORMAT(f20.17,5X,'P1e*sin(omega), tag tw')
2370 FORMAT(f20.17,5X,'P1sqrt(e)*cos(omega), tag 1c')
2380 FORMAT(f20.17,5X,'P1sqrt(e)*sin(omega), tag 1s')
2390 FORMAT(f20.15,5X,'P1incl (degrees), tag tx')
2400 FORMAT(f20.15,5X,'P1Omega (degrees), tag ty')
2410 FORMAT(f20.14,5X,'angsum1 (P1incl + P1Omega), tag 1a')
2420 FORMAT(f20.14,5X,'angdiff1 (P1incl - P1Omega), tag 1d')
2430 FORMAT('!  Body 3 parameters')
2440 FORMAT(2(i4,1X),15X,'Nalph3, Nbet3')
2450 FORMAT(f15.9,10X,'Teff3 (K), tag T3')
2460 FORMAT(f8.5,17X,'g3, tag g3')
2470 FORMAT(f16.11,9X,'P1ratrad (radius of star 1 to ','body 3), t'  &
      ,'ag tb')
2480 FORMAT(f20.11,5X,'P1Q (mass of EB to body 3 mass), ','tag tz')
2490 FORMAT(f18.13,7X,'omega3, tag o3')
2500 FORMAT(f12.7,13X,'axis_I3 (inclination of rotation axis if ',  &
      'ialign=1), tag ci')
2510 FORMAT(f12.7,13X,'axis_beta3 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag cb')
2520 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','1, tag m1, n1')
2530 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','2, tag m2, n2')
2540 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','3, tag m3, n3')
2550 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','4, tag m4, n4')
2560 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','5, tag m5, n5')
2570 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','6, tag m6, n6')
2580 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','7, tag m7, n7')
2590 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','8, tag m8, n8')
2600 FORMAT(f10.8,15X,'rk3 (apsidal constant, body 3), tag a3')
2610 FORMAT(f20.11,5X,'flux body 3, filter 1 (need Nterms > 0),',''  &
      ,' tag 31')
2620 FORMAT(f20.11,5X,'flux body 3, filter 2 (need Nterms > 0),',''  &
      ,' tag 32')
2630 FORMAT(f20.11,5X,'flux body 3, filter 3 (need Nterms > 0),',''  &
      ,' tag 33')
2640 FORMAT(f20.11,5X,'flux body 3, filter 4 (need Nterms > 0),',''  &
      ,' tag 34')
2650 FORMAT(f20.11,5X,'flux body 3, filter 5 (need Nterms > 0),',''  &
      ,' tag 35')
2660 FORMAT(f20.11,5X,'flux body 3, filter 6 (need Nterms > 0),',''  &
      ,' tag 36')
2670 FORMAT(f20.11,5X,'flux body 3, filter 7 (need Nterms > 0),',''  &
      ,' tag 37')
2680 FORMAT(f20.11,5X,'flux body 3, filter 8 (need Nterms > 0),',''  &
      ,' tag 38')
2690 FORMAT('!  Body 4 orbital parameters')
2700 FORMAT(f21.13,4X,'P2Tconj, tag uj')
2710 FORMAT(f22.14,3X,'P2period (days), tag ut')
2720 FORMAT(f21.13,4X,'P2pTc (period + Tconj for body 4), tag 2p')
2730 FORMAT(f21.13,4X,'P2mTc (period - Tconj for body 4), tag 2m')
2740 FORMAT(f17.12,8X,'P2qTc (slope in P-Tc plane for body 4),',1X,  &
      'tag 2q')
2750 FORMAT(f21.12,4X,'T2off (offset in P-Tc plane for body 4),',  &
      1X,'tag 2t')
2760 FORMAT(f21.13,4X,'P2T0, tag uu')
2770 FORMAT(f20.17,5X,'P2e*cos(omega), tag uv')
2780 FORMAT(f20.17,5X,'P2e*sin(omega), tag uw')
2790 FORMAT(f20.17,5X,'P2sqrt(e)*cos(omega), tag 2c')
2800 FORMAT(f20.17,5X,'P2sqrt(e)*sin(omega), tag 2s')
2810 FORMAT(f20.15,5X,'P2incl (degrees), tag ux')
2820 FORMAT(f20.15,5X,'P2Omega (degrees), tag uy')
2830 FORMAT(f20.14,5X,'angsum2 (P2incl + P2Omega), tag 2a')
2840 FORMAT(f20.14,5X,'angdiff2 (P2incl - P2Omega), tag 2d')
2850 FORMAT('!  Body 4 parameters')
2860 FORMAT(f15.9,10X,'Teff4 (K), tag T4')
2870 FORMAT(f8.5,17X,'g4, tag g4')
2880 FORMAT(f16.11,9X,'P2ratrad (radius of star 1 to ','body 4), t'  &
      ,'ag ub')
2890 FORMAT(f20.11,5X,'P2Q (mass of EB to body 4 mass), ','tag uz')
2900 FORMAT(f18.13,7X,'omega4, tag o4')
2910 FORMAT(f12.7,13X,'axis_I4 (inclination of rotation axis if ',  &
      'ialign=1), tag di')
2920 FORMAT(f12.7,13X,'axis_beta4 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag db')
2930 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','1, tag i1, j1')
2940 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','2, tag i2, j2')
2950 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','3, tag i3, j3')
2960 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','4, tag i4, j4')
2970 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','5, tag i5, j5')
2980 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','6, tag i6, j6')
2990 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','7, tag i7, j7')
3000 FORMAT(2(f11.8,1X),1X,'limb darkening coefficients,',' filter'  &
      ,' ','8, tag i8, j8')
3010 FORMAT(f10.8,15X,'rk4 (apsidal constant, body 4), tag a4')
3020 FORMAT(f20.11,5X,'flux body 4, filter 1 (need Nterms > 0),',''  &
      ,' tag 41')
3030 FORMAT(f20.11,5X,'flux body 4, filter 2 (need Nterms > 0),',''  &
      ,' tag 42')
3040 FORMAT(f20.11,5X,'flux body 4, filter 3 (need Nterms > 0),',''  &
      ,' tag 43')
3050 FORMAT(f20.11,5X,'flux body 4, filter 4 (need Nterms > 0),',''  &
      ,' tag 44')
3060 FORMAT(f20.11,5X,'flux body 4, filter 5 (need Nterms > 0),',''  &
      ,' tag 45')
3070 FORMAT(f20.11,5X,'flux body 4, filter 6 (need Nterms > 0),',''  &
      ,' tag 46')
3080 FORMAT(f20.11,5X,'flux body 4, filter 7 (need Nterms > 0),',''  &
      ,' tag 47')
3090 FORMAT(f20.11,5X,'flux body 4, filter 8 (need Nterms > 0),',''  &
      ,' tag 48')
3100 FORMAT('!  binary+binary stellar parameters')
3110 FORMAT(f21.14,4X,'bin2masssum (M_3 + M_4 in binary+binary ',''  &
      ,'mode), tag 91')
3120 FORMAT(f21.14,4X,'bin2massdiff (M_3 - M_4 in binary+binary ',  &
      'mode), tag 92')
3130 FORMAT(f21.14,4X,'bin2Q (M_4/M_3 in binary+binary ','mode), t'  &
      ,'ag 93')
3140 FORMAT(f21.14,4X,'bin2radsum (R_3 + R_4 in binary+binary ','m'  &
      ,'ode), tag 94')
3150 FORMAT(f21.14,4X,'bin2raddiff (R_3 - R_4 in binary+binary ',''  &
      ,'mode), tag 95')
3160 FORMAT(f21.14,4X,'bin2ratrad (R_3/R_4 in binary+binary ','mod'  &
      ,'e), tag 96')
3170 FORMAT(f21.15,4X,'bin2M3 (M3 in solar masses), tag 97')
3180 FORMAT(f21.15,4X,'bin2M4 (M4 in solar masses), tag 98')
3190 FORMAT(f21.15,4X,'bin2R3 (R3 in solar radii), tag 99')
3200 FORMAT(f21.15,4X,'bin2R4 (R4 in solar radii), tag 90')
3210 FORMAT('!  Body 5 orbital parameters')
3220 FORMAT(f21.13,4X,'P3Tconj, tag vj')
3230 FORMAT(f22.14,3X,'P3period (days), tag vt')
3240 FORMAT(f21.13,4X,'P3pTc (period + Tconj for body 5), tag 3p')
3250 FORMAT(f21.13,4X,'P3mTc (period - Tconj for body 5), tag 3m')
3260 FORMAT(f17.12,8X,'P3qTc (slope in P-Tc plane for body 5),',1X,  &
      'tag 3q')
3270 FORMAT(f21.12,4X,'T3off (offset in P-Tc plane for body 5),',  &
      1X,'tag 3t')
3280 FORMAT(f21.13,4X,'P3T0, tag vu')
3290 FORMAT(f20.17,5X,'P3e*cos(omega), tag vv')
3300 FORMAT(f20.17,5X,'P3e*sin(omega), tag vw')
3310 FORMAT(f20.17,5X,'P3sqrt(e)*cos(omega), tag 3c')
3320 FORMAT(f20.17,5X,'P3sqrt(e)*sin(omega), tag 3s')
3330 FORMAT(f20.15,5X,'P3incl (degrees), tag vx')
3340 FORMAT(f20.15,5X,'P3Omega (degrees), tag vy')
3350 FORMAT(f20.14,5X,'angsum3 (P3incl + P3Omega), tag 3a')
3360 FORMAT(f20.14,5X,'angdiff3 (P3incl - P3Omega), tag 3d')
3370 FORMAT('!  Body 5 parameters')
3380 FORMAT(f15.9,10X,'Teff5 (K), tag T5')
3390 FORMAT(f8.5,17X,'g5, tag g5')
3400 FORMAT(f16.11,9X,'P3ratrad (radius of star 1 to ','body 5), t'  &
      ,'ag vb')
3410 FORMAT(f20.11,5X,'P3Q (mass of EB to body 5 mass), ','tag vz')
3420 FORMAT(f12.8,13X,'omega5, tag o5')
3430 FORMAT(f12.7,13X,'axis_I5 (inclination of rotation axis if ',  &
      'ialign=1), tag ...')
3440 FORMAT(f12.7,13X,'axis_beta5 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag ...')
3450 FORMAT(2(f11.8,1X),7X,'limb darkening coefficients,',' filter'  &
      ,' ','1, tag k1, p1')
3460 FORMAT(2(f11.8,1X),7X,'limb darkening coefficients,',' filter'  &
      ,' ','2, tag k2, p2')
3470 FORMAT(2(f11.8,1X),7X,'limb darkening coefficients,',' filter'  &
      ,' ','3, tag k3, p3')
3480 FORMAT(2(f11.8,1X),7X,'limb darkening coefficients,',' filter'  &
      ,' ','4, tag k4, p4')
3490 FORMAT(2(f11.8,1X),7X,'limb darkening coefficients,',' filter'  &
      ,' ','5, tag k5, p5')
3500 FORMAT(2(f11.8,1X),7X,'limb darkening coefficients,',' filter'  &
      ,' ','6, tag k6, p6')
3510 FORMAT(2(f11.8,1X),7X,'limb darkening coefficients,',' filter'  &
      ,' ','7, tag k7, p7')
3520 FORMAT(2(f11.8,1X),7X,'limb darkening coefficients,',' filter'  &
      ,' ','8, tag k8, p8')
3530 FORMAT(f10.8,15X,'rk5 (apsidal constant, body 5), tag a5')
3540 FORMAT(f20.11,5X,'flux body 5, filter 1 (need Nterms > 0),',''  &
      ,' tag 51')
3550 FORMAT(f20.11,5X,'flux body 5, filter 2 (need Nterms > 0),',''  &
      ,' tag 52')
3560 FORMAT(f20.11,5X,'flux body 5, filter 3 (need Nterms > 0),',''  &
      ,' tag 53')
3570 FORMAT(f20.11,5X,'flux body 5, filter 4 (need Nterms > 0),',''  &
      ,' tag 54')
3580 FORMAT(f20.11,5X,'flux body 5, filter 5 (need Nterms > 0),',''  &
      ,' tag 55')
3590 FORMAT(f20.11,5X,'flux body 5, filter 6 (need Nterms > 0),',''  &
      ,' tag 56')
3600 FORMAT(f20.11,5X,'flux body 5, filter 7 (need Nterms > 0),',''  &
      ,' tag 57')
3610 FORMAT(f20.11,5X,'flux body 5, filter 8 (need Nterms > 0),',''  &
      ,' tag 58')
3620 FORMAT('!  Body 6 orbital parameters')
3630 FORMAT(f21.13,4X,'P4Tconj, tag wj')
3640 FORMAT(f22.14,3X,'P4period (days), tag wt')
3650 FORMAT(f21.13,4X,'P4pTc (period + Tconj for body 6), tag 4p')
3660 FORMAT(f21.13,4X,'P4mTc (period - Tconj for body 6), tag 4m')
3670 FORMAT(f17.12,8X,'P4qTc (slope in P-Tc plane for body 6),',1X,  &
      'tag 4q')
3680 FORMAT(f21.12,4X,'T4off (offset in P-Tc plane for body 6),',  &
      1X,'tag 4t')
3690 FORMAT(f21.13,4X,'P4T0, tag wu')
3700 FORMAT(f20.17,5X,'P4e*cos(omega), tag wv')
3710 FORMAT(f20.17,5X,'P4e*sin(omega), tag ww')
3720 FORMAT(f20.17,5X,'P4sqrt(e)*cos(omega), tag 4c')
3730 FORMAT(f20.17,5X,'P4sqrt(e)*sin(omega), tag 4s')
3740 FORMAT(f20.15,5X,'P4incl (degrees), tag wx')
3750 FORMAT(f20.15,5X,'P4Omega (degrees), tag wy')
3760 FORMAT(f20.14,5X,'angsum4 (P4incl + P4Omega), tag 4a')
3770 FORMAT(f20.14,5X,'angdiff4 (P4incl - P4Omega), tag 4d')
3780 FORMAT('!  Body 6 parameters')
3790 FORMAT(f15.9,10X,'Teff6 (K), tag T6')
3800 FORMAT(f8.5,17X,'g6, tag g6')
3810 FORMAT(f16.11,9X,'P4ratrad (radius of star 1 to ','body 6), t'  &
      ,'ag wb')
3820 FORMAT(f20.11,5X,'P4Q (mass of EB to body 6 mass), ','tag wz')
3830 FORMAT(f12.8,13X,'omega6, tag o6')
3840 FORMAT(f12.7,13X,'axis_I6 (inclination of rotation axis if ',  &
      'ialign=1), tag ...')
3850 FORMAT(f12.7,13X,'axis_beta6 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag ...')
3860 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'1, tag ..., ...')
3870 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'2, tag ..., ...')
3880 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'3, tag ..., ...')
3890 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'4, tag ..., ...')
3900 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'5, tag ..., ...')
3910 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'6, tag ..., ...')
3920 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'7, tag ..., ...')
3930 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'8, tag ..., ...')
3940 FORMAT(f10.8,15X,'rk6 (apsidal constant, body 6), tag a6')
3950 FORMAT(f20.11,5X,'flux body 6, filter 1 (need Nterms > 0),',''  &
      ,' tag 61')
3960 FORMAT(f20.11,5X,'flux body 6, filter 2 (need Nterms > 0),',''  &
      ,' tag 62')
3970 FORMAT(f20.11,5X,'flux body 6, filter 3 (need Nterms > 0),',''  &
      ,' tag 63')
3980 FORMAT(f20.11,5X,'flux body 6, filter 4 (need Nterms > 0),',''  &
      ,' tag 64')
3990 FORMAT(f20.11,5X,'flux body 6, filter 5 (need Nterms > 0),',''  &
      ,' tag 65')
4000 FORMAT(f20.11,5X,'flux body 6, filter 6 (need Nterms > 0),',''  &
      ,' tag 66')
4010 FORMAT(f20.11,5X,'flux body 6, filter 7 (need Nterms > 0),',''  &
      ,' tag 67')
4020 FORMAT(f20.11,5X,'flux body 6, filter 8 (need Nterms > 0),',''  &
      ,' tag 68')
4030 FORMAT('!  Body 7 orbital parameters')
4040 FORMAT(f21.13,4X,'P5Tconj, tag xj')
4050 FORMAT(f22.14,3X,'P5period (days), tag xt')
4060 FORMAT(f21.13,4X,'P5pTc (period + Tconj for body 7), tag 5p')
4070 FORMAT(f21.13,4X,'P5mTc (period - Tconj for body 7), tag 5m')
4080 FORMAT(f17.12,8X,'P5qTc (slope in P-Tc plane for body 7),',1X,  &
      'tag 5q')
4090 FORMAT(f21.12,4X,'T5off (offset in P-Tc plane for body 7),',  &
      1X,'tag 5t')
4100 FORMAT(f21.13,4X,'P5T0, tag xu')
4110 FORMAT(f20.17,5X,'P5e*cos(omega), tag xv')
4120 FORMAT(f20.17,5X,'P5e*sin(omega), tag xw')
4130 FORMAT(f20.17,5X,'P5sqrt(e)*cos(omega), tag 5c')
4140 FORMAT(f20.17,5X,'P5sqrt(e)*sin(omega), tag 5s')
4150 FORMAT(f20.15,5X,'P5incl (degrees), tag xx')
4160 FORMAT(f20.15,5X,'P5Omega (degrees), tag xy')
4170 FORMAT(f20.14,5X,'angsum5 (P5incl + P5Omega), tag 5a')
4180 FORMAT(f20.14,5X,'angdiff5 (P5incl - P5Omega), tag 5d')
4190 FORMAT('!  Body 7 parameters')
4200 FORMAT(f15.9,10X,'Teff7 (K), tag T7')
4210 FORMAT(f8.5,17X,'g7, tag g7')
4220 FORMAT(f16.11,9X,'P5ratrad (radius of star 1 to ','body 7), t'  &
      ,'ag xb')
4230 FORMAT(f20.11,5X,'P5Q (mass of EB to body 7 mass), ','tag xz')
4240 FORMAT(f12.8,13X,'omega7, tag o7')
4250 FORMAT(f12.7,13X,'axis_I7 (inclination of rotation axis if ',  &
      'ialign=1), tag ...')
4260 FORMAT(f12.7,13X,'axis_beta7 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag ...')
4270 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'1, tag ..., ...')
4280 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'2, tag ..., ...')
4290 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'3, tag ..., ...')
4300 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'4, tag ..., ...')
4310 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'5, tag ..., ...')
4320 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'6, tag ..., ...')
4330 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'7, tag ..., ...')
4340 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'8, tag ..., ...')
4350 FORMAT(f10.8,15X,'rk7 (apsidal constant, body 7), tag a7')
4360 FORMAT(f20.11,5X,'flux body 7, filter 1 (need Nterms > 0),',''  &
      ,' tag 71')
4370 FORMAT(f20.11,5X,'flux body 7, filter 2 (need Nterms > 0),',''  &
      ,' tag 72')
4380 FORMAT(f20.11,5X,'flux body 7, filter 3 (need Nterms > 0),',''  &
      ,' tag 73')
4390 FORMAT(f20.11,5X,'flux body 7, filter 4 (need Nterms > 0),',''  &
      ,' tag 74')
4400 FORMAT(f20.11,5X,'flux body 7, filter 5 (need Nterms > 0),',''  &
      ,' tag 75')
4410 FORMAT(f20.11,5X,'flux body 7, filter 6 (need Nterms > 0),',''  &
      ,' tag 76')
4420 FORMAT(f20.11,5X,'flux body 7, filter 7 (need Nterms > 0),',''  &
      ,' tag 77')
4430 FORMAT(f20.11,5X,'flux body 7, filter 8 (need Nterms > 0),',''  &
      ,' tag 78')
4440 FORMAT('!  Body 8 orbital parameters')
4450 FORMAT(f21.13,4X,'P6Tconj, tag sj')
4460 FORMAT(f22.14,3X,'P6period (days), tag st')
4470 FORMAT(f21.13,4X,'P6pTc (period + Tconj for body 8), tag 6p')
4480 FORMAT(f21.13,4X,'P6mTc (period - Tconj for body 8), tag 6m')
4490 FORMAT(f17.12,8X,'P6qTc (slope in P-Tc plane for body 8),',1X,  &
      'tag 6q')
4500 FORMAT(f21.12,4X,'T6off (offset in P-Tc plane for body 8),',  &
      1X,'tag 6t')
4510 FORMAT(f21.13,4X,'P6T0, tag su')
4520 FORMAT(f20.17,5X,'P6e*cos(omega), tag sv')
4530 FORMAT(f20.17,5X,'P6e*sin(omega), tag sw')
4540 FORMAT(f20.17,5X,'P6sqrt(e)*cos(omega), tag 6c')
4550 FORMAT(f20.17,5X,'P6sqrt(e)*sin(omega), tag 6s')
4560 FORMAT(f20.15,5X,'P6incl (degrees), tag sx')
4570 FORMAT(f20.15,5X,'P6Omega (degrees), tag sy')
4580 FORMAT(f20.14,5X,'angsum6 (P6incl + P6Omega), tag 6a')
4590 FORMAT(f20.14,5X,'angdiff6 (P6incl - P6Omega), tag 6d')
4600 FORMAT('!  Body 8 parameters')
4610 FORMAT(f15.9,10X,'Teff8 (K), tag T8')
4620 FORMAT(f8.5,17X,'g8, tag g8')
4630 FORMAT(f16.11,9X,'P6ratrad (radius of star 1 to ','body 8), t'  &
      ,'ag sb')
4640 FORMAT(f20.11,5X,'P6Q (mass of EB to body 8 mass), ','tag sz')
4650 FORMAT(f12.8,13X,'omega8, tag o8')
4660 FORMAT(f12.7,13X,'axis_I8 (inclination of rotation axis if ',  &
      'ialign=1), tag ...')
4670 FORMAT(f12.7,13X,'axis_beta8 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag ...')
4680 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'1, tag ..., ...')
4690 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'2, tag ..., ...')
4700 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'3, tag ..., ...')
4710 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'4, tag ..., ...')
4720 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'5, tag ..., ...')
4730 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'6, tag ..., ...')
4740 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'7, tag ..., ...')
4750 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'8, tag ..., ...')
4760 FORMAT(f10.8,15X,'rk8 (apsidal constant, body 8), tag a8')
4770 FORMAT(f20.11,5X,'flux body 8, filter 1 (need Nterms > 0),',''  &
      ,' tag 81')
4780 FORMAT(f20.11,5X,'flux body 8, filter 2 (need Nterms > 0),',''  &
      ,' tag 82')
4790 FORMAT(f20.11,5X,'flux body 8, filter 3 (need Nterms > 0),',''  &
      ,' tag 83')
4800 FORMAT(f20.11,5X,'flux body 8, filter 4 (need Nterms > 0),',''  &
      ,' tag 84')
4810 FORMAT(f20.11,5X,'flux body 8, filter 5 (need Nterms > 0),',''  &
      ,' tag 85')
4820 FORMAT(f20.11,5X,'flux body 8, filter 6 (need Nterms > 0),',''  &
      ,' tag 86')
4830 FORMAT(f20.11,5X,'flux body 8, filter 7 (need Nterms > 0),',''  &
      ,' tag 87')
4840 FORMAT(f20.11,5X,'flux body 8, filter 8 (need Nterms > 0),',''  &
      ,' tag 88')
4850 FORMAT('!  Body 9 orbital parameters')
4860 FORMAT(f21.13,4X,'P7Tconj, tag hj')
4870 FORMAT(f22.14,3X,'P7period (days), tag ht')
4880 FORMAT(f21.13,4X,'P7pTc (period + Tconj for body 9), tag 7p')
4890 FORMAT(f21.13,4X,'P7mTc (period - Tconj for body 9), tag 7m')
4900 FORMAT(f17.12,8X,'P7qTc (slope in P-Tc plane for body 9),',1X,  &
      'tag 7q')
4910 FORMAT(f21.12,4X,'T7off (offset in P-Tc plane for body 9),',  &
      1X,'tag 7t')
4920 FORMAT(f21.13,4X,'P7T0, tag hu')
4930 FORMAT(f20.17,5X,'P7e*cos(omega), tag hv')
4940 FORMAT(f20.17,5X,'P7e*sin(omega), tag hw')
4950 FORMAT(f20.17,5X,'P7sqrt(e)*cos(omega), tag 7c')
4960 FORMAT(f20.17,5X,'P7sqrt(e)*sin(omega), tag 7s')
4970 FORMAT(f20.15,5X,'P7incl (degrees), tag hx')
4980 FORMAT(f20.15,5X,'P7Omega (degrees), tag hy')
4990 FORMAT(f20.14,5X,'angsum7 (P7incl + P7Omega), tag 7a')
5000 FORMAT(f20.14,5X,'angdiff7 (P7incl - P7Omega), tag 7d')
5010 FORMAT('!  Body 9 parameters')
5020 FORMAT(f15.9,10X,'Teff9 (K), tag T9')
5030 FORMAT(f8.5,17X,'g9, tag g9')
5040 FORMAT(f16.11,9X,'P7ratrad (radius of star 1 to ','body 9), t'  &
      ,'ag hb')
5050 FORMAT(f20.11,5X,'P7Q (mass of EB to body 9 mass), ','tag hz')
5060 FORMAT(f12.8,13X,'omega9, tag o9')
5070 FORMAT(f12.7,13X,'axis_I9 (inclination of rotation axis if ',  &
      'ialign=1), tag ...')
5080 FORMAT(f12.7,13X,'axis_beta9 (angle of rotation axis wrt ','o'  &
      ,'rbit if ialign=1), tag ...')
5090 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'1, tag ..., ...')
5100 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'2, tag ..., ...')
5110 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'3, tag ..., ...')
5120 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'4, tag ..., ...')
5130 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'5, tag ..., ...')
5140 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'6, tag ..., ...')
5150 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'7, tag ..., ...')
5160 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'8, tag ..., ...')
5170 FORMAT(f10.8,15X,'rk9 (apsidal constant, body 9), tag a9')
5180 FORMAT(f20.11,5X,'flux body 9, filter 1 (need Nterms > 0),',''  &
      ,' tag 91')
5190 FORMAT(f20.11,5X,'flux body 9, filter 2 (need Nterms > 0),',''  &
      ,' tag 92')
5200 FORMAT(f20.11,5X,'flux body 9, filter 3 (need Nterms > 0),',''  &
      ,' tag 93')
5210 FORMAT(f20.11,5X,'flux body 9, filter 4 (need Nterms > 0),',''  &
      ,' tag 94')
5220 FORMAT(f20.11,5X,'flux body 9, filter 5 (need Nterms > 0),',''  &
      ,' tag 95')
5230 FORMAT(f20.11,5X,'flux body 9, filter 6 (need Nterms > 0),',''  &
      ,' tag 96')
5240 FORMAT(f20.11,5X,'flux body 9, filter 7 (need Nterms > 0),',''  &
      ,' tag 97')
5250 FORMAT(f20.11,5X,'flux body 9, filter 8 (need Nterms > 0),',''  &
      ,' tag 98')
5260 FORMAT('!  Body 10 orbital parameters')
5270 FORMAT(f21.13,4X,'P8Tconj, tag kj')
5280 FORMAT(f22.14,3X,'P8period (days), tag kt')
5290 FORMAT(f21.13,4X,'P8pTc (period + Tconj for body 10), tag 8p')
5300 FORMAT(f21.13,4X,'P8mTc (period - Tconj for body 10), tag 8m')
5310 FORMAT(f17.12,8X,'P8qTc (slope in P-Tc plane for body 10),',  &
      1X,'tag 8q')
5320 FORMAT(f21.12,4X,'T8off (offset in P-Tc plane for body 10),',  &
      1X,'tag 8t')
5330 FORMAT(f21.13,4X,'P8T0, tag ku')
5340 FORMAT(f20.17,5X,'P8e*cos(omega), tag kv')
5350 FORMAT(f20.17,5X,'P8e*sin(omega), tag kw')
5360 FORMAT(f20.17,5X,'P8sqrt(e)*cos(omega), tag 8c')
5370 FORMAT(f20.17,5X,'P8sqrt(e)*sin(omega), tag 8s')
5380 FORMAT(f20.15,5X,'P8incl (degrees), tag kx')
5390 FORMAT(f20.15,5X,'P8Omega (degrees), tag ky')
5400 FORMAT(f20.14,5X,'angsum8 (P8incl + P8Omega), tag 8a')
5410 FORMAT(f20.14,5X,'angdiff8 (P8incl - P8Omega), tag 8d')
5420 FORMAT('!  Body 10 parameters')
5430 FORMAT(f15.9,10X,'Teff10 (K), tag ...')
5440 FORMAT(f8.5,17X,'g10, tag g0')
5450 FORMAT(f16.11,9X,'P8ratrad (radius of star 1 to ','body 10),'  &
      ,' tag kb')
5460 FORMAT(f20.11,5X,'P8Q (mass of EB to body 10 mass), ','tag k' ,'z')
5470 FORMAT(f12.8,13X,'omega10, tag o0')
5480 FORMAT(f12.7,13X,'axis_I10 (inclination of rotation axis if ',  &
      'ialign=1), tag ...')
5490 FORMAT(f12.7,13X,'axis_beta10 (angle of rotation axis wrt ',''  &
      ,'orbit if ialign=1), tag ...')
5500 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'1, tag ..., ...')
5510 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'2, tag ..., ...')
5520 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'3, tag ..., ...')
5530 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'4, tag ..., ...')
5540 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'5, tag ..., ...')
5550 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'6, tag ..., ...')
5560 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'7, tag ..., ...')
5570 FORMAT(2(f8.5,1X),7X,'limb darkening coefficients,',' filter '  &
      ,'8, tag ..., ...')
5580 FORMAT(f10.8,15X,'rk10 (apsidal constant, body 10), tag ...')
5590 FORMAT(f20.11,5X,'flux body 10, filter 1 (need Nterms > 0),',  &
      ' tag 01')
5600 FORMAT(f20.11,5X,'flux body 10, filter 2 (need Nterms > 0),',  &
      ' tag 02')
5610 FORMAT(f20.11,5X,'flux body 10, filter 3 (need Nterms > 0),',  &
      ' tag 03')
5620 FORMAT(f20.11,5X,'flux body 10, filter 4 (need Nterms > 0),',  &
      ' tag 04')
5630 FORMAT(f20.11,5X,'flux body 10, filter 5 (need Nterms > 0),',  &
      ' tag 05')
5640 FORMAT(f20.11,5X,'flux body 10, filter 6 (need Nterms > 0),',  &
      ' tag 06')
5650 FORMAT(f20.11,5X,'flux body 10, filter 7 (need Nterms > 0),',  &
      ' tag 07')
5660 FORMAT(f20.11,5X,'flux body 10, filter 8 (need Nterms > 0),',  &
      ' tag 08')
!
   RETURN
!
END SUBROUTINE newwritegridout
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE parsecomma(instring,narray,iarray)
!
!   This routine will read in a string with
!   comma separated values (assumed to be integers)
!   and will return an array with those integers.
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=100), INTENT(IN)          :: instring
   INTEGER, INTENT(OUT)                     :: narray
   INTEGER, INTENT(OUT)                     :: iarray(99)
!
   INTEGER :: i,k1,ncomma,ios,ipos
!
   DIMENSION  ipos(99)
!
   ios=0
!
   k1=LEN_TRIM(instring)
   IF(k1 == 0)THEN
      WRITE(*,*)'error parsing string, zero length'
      narray=1
      iarray=0
      iarray(1)=1
      RETURN
   END IF
!
!   If there is a comma in the first position, assume
!   an error.
!
   IF(instring(1:1) == ',')THEN
      WRITE(*,*)'error parsing string, leading comma'
      narray=1
      iarray=0
      iarray(1)=1
      RETURN
   END IF
!
!   Count the number of commas.  If there are two commas
!   in a row then assume an error.
!
   ncomma=0
   DO i=1,k1
      IF(i > 1)THEN
         IF(instring(i-1:i) == ',,')THEN
            WRITE(*,*)'error parsing string, double commas'
            narray=1
            iarray=0
            iarray(1)=1
            RETURN
         END IF
      END IF
      IF(instring(i:i) == ',')THEN
         ncomma=ncomma+1
         ipos(ncomma)=i
      END IF
   END DO
!
!   If there are no commas, then assume only one number
!
   IF(ncomma == 0)THEN
      narray=1
      READ(instring(1:k1),*,IOSTAT=ios)iarray(1)
      IF(ios /= 0)THEN
         WRITE(*,*)'error parsing string, not an integer'
         narray=1
         iarray=0
         iarray(1)=1
         RETURN
      END IF
      RETURN
   END IF
!
   IF(ncomma > 0)THEN
      narray=1
      ios=0
      READ(instring(1:ipos(1)-1),*,IOSTAT=ios)iarray(1)
      IF(ios /= 0)THEN
         WRITE(*,*)'error parsing string, not an integer'
         narray=1
         iarray=0
         iarray(1)=1
         RETURN
      END IF
      DO i=2,ncomma
         narray=narray+1
         READ(instring(ipos(i-1)+1:ipos(i)-1),*,IOSTAT=ios)iarray(narray)
         IF(ios /= 0)THEN
            WRITE(*,*)'error parsing string, not an integer'
            narray=1
            iarray=0
            iarray(1)=1
            RETURN
         END IF
      END DO
      IF(ipos(ncomma) < k1)THEN
         narray=narray+1
         READ(instring(ipos(ncomma)+1:k1),*,IOSTAT=ios)iarray(narray)
         IF(ios /= 0)THEN
            WRITE(*,*)'error parsing string, not an integer'
            narray=1
            iarray=0
            iarray(1)=1
            RETURN
         END IF
      ELSE
         RETURN
      END IF
   END IF
!
   RETURN
!
END SUBROUTINE parsecomma
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE plotconf(arrayx,nx,first1,rlast1)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nx
   REAL(KIND=dp), INTENT(IN)                :: arrayx(nx)
   REAL(KIND=dp), INTENT(IN OUT)            :: first1
   REAL(KIND=dp), INTENT(IN OUT)            :: rlast1
!
   INTEGER :: i,imax,nbin,j,index3,mark1,mark2,k
!
   REAL(KIND=dp), DIMENSION(300)  :: binx,biny,y2
!
   REAL(KIND=dp)  :: xhigh,xlow,t1,con1,con2,con3,diff,xlowkeep,xhighkeep
   REAL(KIND=dp)  :: runtot,frac,rlev3,qq,step,diffmin,xxx,ymax,area,yyy
!
   area=0.0_dp
!
   nbin=200
!
   xhigh=-1.2345E+40_dp
   xlow=1.2345E+40_dp
   imax=1
!
   DO i=1,nx
      IF(arrayx(i) > xhigh)xhigh=arrayx(i)
      IF(arrayx(i) < xlow)xlow=arrayx(i)
   END DO
!
   DO k=1,3
      IF(k > 1)THEN
         xlow=first1
         xhigh=rlast1
      END IF
      xlowkeep=xlow
      xhighkeep=xhigh
      qq=(xhigh-xlow)/REAL(nbin,KIND=dp)
!
      DO i=1,nbin
         binx(i)=REAL(i-1,KIND=dp)*qq+xlow+qq/2.0_dp
         biny(i)=0.0_dp
      END DO
!
      loop20:  DO  i=1,nx
         DO  j=1,nbin
            t1=ABS(arrayx(i)-binx(j))
            IF(t1 <= qq/2.0_dp)THEN
               biny(j)=biny(j)+1.0_dp
               CYCLE loop20
            END IF
         END DO
      END DO loop20
!
      area=0.0_dp
      ymax=-33333.0_dp
      DO  i=1,nbin
         area=area+biny(i)*qq
         IF(biny(i) > ymax)THEN
            ymax=biny(i)
            imax=i
         END IF
      END DO
!
      CALL sort2(nbin,biny,binx)!sort by y-values
!
!   now start from ymax and add area until get 68.3% and 90% of area
!
      con1=0.683_dp
      con2=0.900_dp
      con3=0.985_dp
!
      index3=1
      runtot=0.0_dp
      DO  i=nbin,1,-1
         runtot=biny(i)*qq+runtot
         frac=runtot/area
         IF(frac > con1)THEN
            EXIT
         END IF
      END DO
!
      runtot=0.0_dp
      DO  i=nbin,1,-1
         runtot=biny(i)*qq+runtot
         frac=runtot/area
         IF(frac > con2)THEN
            EXIT
         END IF
      END DO
!
      runtot=0.0_dp
      DO  i=nbin,1,-1
         runtot=biny(i)*qq+runtot
         frac=runtot/area
         IF(frac > con3)THEN
            index3=i
            EXIT
         END IF
      END DO
!
      ymax=biny(nbin)
      rlev3=biny(index3)
!
      CALL sort2(nbin,binx,biny)
!
      CALL spline(binx,biny,nbin,0.0_dp,0.0_dp,y2)
!
      mark1=1
      mark2=1
      DO  i=1,imax
         IF(biny(i) > rlev3)THEN
            mark1=i
            EXIT
         END IF
      END DO
!
      DO  i=imax+1,nbin
         IF(biny(i) < rlev3)THEN
            mark2=i
            EXIT
         END IF
      END DO
      IF(mark1 <= 1)mark1=2
      IF(mark2 <= 1)mark2=2
      step=ABS(binx(mark1)-binx(mark1-1))/10.0_dp
      diffmin=100000.0_dp
!
      xxx=binx(mark1-1)
      DO  i=1,10
         CALL splint(binx,biny,y2,nbin,xxx,yyy)
         diff=ABS(yyy-rlev3)
         IF(diff < diffmin)THEN
            diffmin=diff
            xlow=xxx
         END IF
         xxx=xxx+step
      END DO
!
      first1=xlow
!
      step=ABS(binx(mark2)-binx(mark2-1))/10.0_dp
      diffmin=100000.0_dp
      xxx=binx(mark2-1)
      DO  i=1,10
         CALL splint(binx,biny,y2,nbin,xxx,yyy)
         diff=ABS(yyy-rlev3)
         IF(diff < diffmin)THEN
            diffmin=diff
            xlow=xxx
         END IF
         xxx=xxx+step
      END DO
!
      rlast1=xlow
!
      step=ABS(binx(mark1)-binx(mark1-1))/10.0_dp
      diffmin=100000.0_dp
!
      xxx=binx(mark1-1)
      DO  i=1,10
         CALL splint(binx,biny,y2,nbin,xxx,yyy)
         diff=ABS(yyy-rlev3)
         IF(diff < diffmin)THEN
            diffmin=diff
            xlow=xxx
         END IF
         xxx=xxx+step
      END DO
!
!        first1=xlow
!
      step=ABS(binx(mark2)-binx(mark2-1))/10.0_dp
      diffmin=100000.0_dp
      xxx=binx(mark2-1)
      DO  i=1,10
         CALL splint(binx,biny,y2,nbin,xxx,yyy)
         diff=ABS(yyy-rlev3)
         IF(diff < diffmin)THEN
            diffmin=diff
            xlow=xxx
         END IF
         xxx=xxx+step
      END DO
!
!        rlast1=xlow
!
      IF(first1 >= rlast1)THEN
         first1=xlowkeep
         rlast1=xhighkeep
      END IF
!
      IF(imax == 1)THEN
         first1=xlowkeep
         rlast1=xhighkeep
      END IF

      IF(imax == nbin)THEN
         first1=xlowkeep
         rlast1=xhighkeep
      END IF
!
   END DO
!
   RETURN
!
END SUBROUTINE plotconf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE plotgray(ndatax,ndatay,xplot,yplot)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ndatax
   INTEGER, INTENT(IN)                      :: ndatay
   REAL(KIND=pg), INTENT(IN)                :: xplot(ndatax)
   REAL(KIND=pg), INTENT(IN)                :: yplot(ndatay)
!
   INTEGER :: i,j,indexx,indexy
!
   REAL(KIND=pg)  ::  xxx,yyy,trans,gray,yhigh

   DIMENSION gray(100,100),trans(6)
!
   DATA trans/0.0_pg,1.0_pg,0.0_pg,0.0_pg,0.0_pg,1.0_pg/
!
   DO i=1,100
      DO j=1,100
         gray(i,j)=1.0_pg
      END DO
   END DO
!
   CALL pgwindow(0.0_pg,100.0_pg,0.0_pg,100.0_pg)
   DO  i=1,ndatax
      xxx=xplot(i)
      yyy=yplot(i)
      indexx=INT(100.0_pg*xxx)+1
      indexy=INT(100.0_pg*yyy)+1
      IF(indexx < 1)CYCLE
      IF(indexx > 100)CYCLE
      IF(indexy < 1)CYCLE
      IF(indexy > 100)CYCLE
      gray(indexx,indexy)=gray(indexx,indexy)+1.0_pg
   END DO
!
   yhigh=-123456.0_pg
   DO i=1,100
      DO j=1,100
         gray(i,j)=LOG10(gray(i,j))
         IF(gray(i,j) > yhigh)THEN
            yhigh=gray(i,j)
         END IF
      END DO
   END DO
!
   CALL pggray(gray,100,100,1,100,1,100,yhigh,0.0_pg,trans)
!
   RETURN
!
END SUBROUTINE plotgray
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pnoequalstring(instring,iplace,chisq,outstring,  &
   ilength)
!
!  this routine will return the instring appended to a string
!  giving its value formatted in a compact way.  iplace gives the
!  number of decimal places. For example, chisq=12345.6789, iplace=1,
!  and instring='chi^2' yields 'chi^2 12345.8'
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: instring
   INTEGER, INTENT(IN)                      :: iplace
   REAL(KIND=dp), INTENT(IN)                :: chisq
   CHARACTER (LEN=*), INTENT(OUT)           :: outstring
   INTEGER, INTENT(OUT)                     :: ilength
!
   REAL(KIND=dp)  :: tempchi
!
   CHARACTER (LEN=120) :: clip
!
   outstring=' '
   ilength=0
!
!   iplace=1
!
   IF(iplace == 1)THEN
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'9'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999999.9_dp)THEN
         tempchi=999999999999999.0_dp
!              WRITE(outstring,10)empchi
!              CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//'999999999999999.9'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,10)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=1
!
   END IF
!
!   iplace=2
!
   IF(iplace == 2)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.99_dp
         WRITE(outstring,20)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999999.99_dp)THEN
         tempchi=999999999999999.00_dp
!              WRITE(outstring,20)empchi
!              CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//'999999999999999.99'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF

      WRITE(outstring,20)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=2
!
   END IF
!
!   iplace=3
!
   IF(iplace == 3)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'99'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999999.999_dp)THEN
         tempchi=999999999999999.000_dp
!              WRITE(outstring,30)empchi
!              CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//'999999999999999.999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,30)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=3
!
   END IF
!
!   iplace=4
!
   IF(iplace == 4)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999999.9999_dp)THEN
         tempchi=9999999999999.9999_dp
!              WRITE(outstring,40)empchi
!              CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//'999999999999999.9999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,40)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=4
!
   END IF
!
!   iplace=5
!
   IF(iplace == 5)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'9999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999999.99999_dp)THEN
         tempchi=99999999999999.99_dp
!              WRITE(outstring,20)empchi
!              CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//'999999999999999.99999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,50)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=5
!
   END IF
!
!   iplace=6
!
   IF(iplace == 6)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'99999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999.999999_dp)THEN
         tempchi=999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'99999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,60)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=6
!
   END IF
!
!   iplace=7
!
   IF(iplace == 7)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999.9999999_dp)THEN
         tempchi=999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,70)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=7
!
   END IF
!
!   iplace=8
!
   IF(iplace == 8)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'9999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999.99999999_dp)THEN
         tempchi=999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'9999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,80)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=8
!
   END IF
!
!   iplace=9
!
   IF(iplace == 9)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'99999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999.999999999_dp)THEN
         tempchi=999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'99999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,90)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=9
!
   END IF
!
!   iplace=10
!
   IF(iplace == 10)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'999999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999.9999999999_dp)THEN
         tempchi=999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'999999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      WRITE(outstring,100)chisq
      CALL clipstring(outstring,clip,ilength)
      outstring=instring//' '//clip(1:ilength)
      ilength=LEN_TRIM(outstring)
      RETURN
!
!  end if iplace=10
!
   END IF
!
!   iplace=11
!
   IF(iplace == 11)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'9999999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999.99999999999_dp)THEN
         tempchi=999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'9999999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF((chisq > 0.0_dp).AND.(chisq < 1.0E-08_dp))THEN
         WRITE(outstring,110)chisq
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)
         ilength=LEN_TRIM(outstring)
         RETURN
      ELSE
         WRITE(outstring,120)chisq
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
!  end if iplace=11
!
   END IF
!
!   iplace=12
!
   IF(iplace == 12)THEN
!
      IF(chisq < -1000000000000.0_dp)THEN
         tempchi=-9999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'99999999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF(chisq > 999999999999.999999999999_dp)THEN
         tempchi=999999999999.9_dp
         WRITE(outstring,10)tempchi
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)//'99999999999'
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
      IF((chisq > 0.0_dp).AND.(chisq < 1.0E-09_dp))THEN
         WRITE(outstring,130)chisq
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)
         ilength=LEN_TRIM(outstring)
         RETURN
      ELSE
         WRITE(outstring,140)chisq
         CALL clipstring(outstring,clip,ilength)
         outstring=instring//' '//clip(1:ilength)
         ilength=LEN_TRIM(outstring)
         RETURN
      END IF
!
!  end if iplace=12
!
   END IF
!
10 FORMAT(f19.1)
20 FORMAT(f20.2)
30 FORMAT(f20.3)
40 FORMAT(f21.4)
50 FORMAT(f22.5)
60 FORMAT(f23.6)
70 FORMAT(f24.7)
80 FORMAT(f25.8)
90 FORMAT(f26.9)
100 FORMAT(f27.10)
110 FORMAT(1PE29.11)
120 FORMAT(f28.11)
130 FORMAT(1PE29.12)
140 FORMAT(f29.12)
!
   RETURN
!
END SUBROUTINE pnoequalstring
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE posteriorrange(nbin,binx,biny,answer,siglow,  &
   sighigh)
!
!   returns the lower and upper 1 sigma ranges from the input
!   histogram
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nbin
   REAL(KIND=dp), INTENT(IN OUT)            :: binx(nbin)
   REAL(KIND=dp), INTENT(IN OUT)            :: biny(nbin)
   REAL(KIND=dp), INTENT(OUT)               :: answer
   REAL(KIND=dp), INTENT(OUT)               :: siglow
   REAL(KIND=dp), INTENT(OUT)               :: sighigh
!
   REAL(KIND=dp)  :: area,ymax,step,con1,frac,rlev1,runtot,y2
   REAL(KIND=dp)  :: diffmin,xxx,yyy,xlow,diff
!
   INTEGER :: i,imax,index1,mark1,mark2
!
   DIMENSION  y2(nbin)
!
   area=0.0_dp
!
   mark1=2
   mark2=nbin-2
   ymax=-33333.0_dp
   DO i=1,nbin
      area=area+biny(i)
      IF(biny(i) > ymax)THEN
         ymax=biny(i)
         imax=i
      END IF
   END DO
!
   answer=binx(imax)
!
   CALL sort2(nbin,biny,binx) !sort by y-values
!
!   now start from ymax and add area until get 68.3% and
!   90% of area
!
   con1=0.683_dp
!
   runtot=0.0_dp
   DO i=nbin,1,-1
      runtot=biny(i)+runtot
      frac=runtot/area
      IF(frac > con1)THEN
         index1=i
         GO TO 10
      END IF
   END DO
!
10 ymax=biny(nbin)
   rlev1=biny(index1)
!
!  sort by x
!
   CALL sort2(nbin,binx,biny)
!
   CALL spline(binx,biny,nbin,0.0_dp,0.0_dp,y2)
!
   DO i=1,imax
      IF(biny(i) > rlev1)THEN
         mark1=i
         GO TO 20
      END IF
   END DO
!
20 diffmin=199900000.0_dp
!
   IF(mark1 == 1)mark1=2
   DO i=imax+1,nbin
      IF(biny(i) < rlev1)THEN
         mark2=i
         GO TO 30
      END IF
   END DO
!
30 step=ABS(binx(mark1)-binx(mark1-1))/10.0_dp
!
   xxx=binx(mark1-1)
   DO i=1,10
      CALL splint(binx,biny,y2,nbin,xxx,yyy)
      diff=ABS(yyy-rlev1)
      IF(diff < diffmin)THEN
         diffmin=diff
         xlow=xxx
      END IF
      xxx=xxx+step
   END DO
!
   siglow=xlow
   step=ABS(binx(mark2)-binx(mark2-1))/10.0_dp
   diffmin=10000000.0_dp
   xxx=binx(mark2-1)
   DO i=1,10
      CALL splint(binx,biny,y2,nbin,xxx,yyy)
      diff=ABS(yyy-rlev1)
      IF(diff < diffmin)THEN
         diffmin=diff
         xlow=xxx
      END IF
      xxx=xxx+step
   END DO
!
   sighigh=xlow
!
   RETURN
!
END SUBROUTINE posteriorrange
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
FUNCTION ran2(idum)
!
!  From Numerical Recipies
!
   USE accur

   IMPLICIT NONE

   INTEGER, INTENT(IN OUT)                  :: idum
!
   REAL(KIND=dp)            :: ran2
   INTEGER, PARAMETER       :: im1=2147483563
   INTEGER, PARAMETER       :: im2=2147483399
   REAL(KIND=dp), PARAMETER :: am=1.0_dp/REAL(im1,KIND=dp)
   INTEGER, PARAMETER       :: imm1=im1-1
   INTEGER, PARAMETER       :: ia1=40014
   INTEGER, PARAMETER       :: ia2=40692
   INTEGER, PARAMETER       :: iq1=53668
   INTEGER, PARAMETER       :: iq2=52774
   INTEGER, PARAMETER       :: ir1=12211
   INTEGER, PARAMETER       :: ir2=3791
   INTEGER, PARAMETER       :: ntab=32
   INTEGER, PARAMETER       :: ndiv=67108861  !1+imm1/ntab
   REAL(KIND=dp), PARAMETER :: eps=1.2E-15_dp
   REAL(KIND=dp), PARAMETER :: rnmx=1.0_dp-eps
   INTEGER                  :: idum2,j,k,iv(ntab),iy
!
   SAVE  iv,iy,idum2
!
   DATA idum2/123456789/,iv/ntab*0/,iy/0/
!
   IF(idum <= 0)THEN
      idum=MAX(-idum,1)
      idum2=idum
      DO  j=ntab+8,1,-1
         k=idum/iq1
         idum=ia1*(idum-k*iq1)-k*ir1
         IF(idum < 0)idum=idum+im1
         IF(j <= ntab)iv(j)=idum
      END DO
      iy=iv(1)
   END IF
   k=idum/iq1
   idum=ia1*(idum-k*iq1)-k*ir1
   IF(idum < 0)idum=idum+im1
   k=idum2/iq2
   idum2=ia2*(idum2-k*iq2)-k*ir2
   IF(idum2 < 0)idum2=idum2+im2
   j=1+iy/ndiv
   iy=iv(j)-idum2
   iv(j)=idum
   IF(iy < 1)iy=iy+imm1
   ran2=MIN(am*REAL(iy,KIND=dp),rnmx)
   RETURN
!
END FUNCTION ran2
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE rdint(iunit,i1,stringerr,filein)
!
!   A utility code to read an integer value from unit=iunit.
!   The value is returned in I1, and the same of the
!   variable is returned in the string stringerr.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: iunit
   INTEGER, INTENT(OUT)                     :: i1
   CHARACTER (LEN=*), INTENT(IN)            :: stringerr
   CHARACTER (LEN=8), INTENT(IN)            :: filein
!
   CHARACTER (LEN=60) :: blank
!
!   Read in the line from iunit and determine if it is
!   a comment line.  If so, then read the next line and
!   keep going until a non comment line is found.
!   After that, backspace and read in an integer value.
!
10 READ(iunit,30)blank
!
   IF((blank(1:1) == '!').OR.(blank(1:1) == '#').OR.(blank(1:1)  &
      == '%'))GO TO 10
!
   BACKSPACE(iunit)
!
   READ(iunit,*,ERR=20)i1
!
   RETURN
!
!   If we make it this far there is an error.
!
20 WRITE(*,40)stringerr,filein
   STOP
!
30 FORMAT(a60)
40 FORMAT('Error reading input file for variable ',a,1X,'in',1X,a)
!
   RETURN
!
END SUBROUTINE rdint
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE rdint2(iunit,i1,i2,stringerr,filein)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: iunit
   INTEGER, INTENT(OUT)                     :: i1
   INTEGER, INTENT(OUT)                     :: i2
   CHARACTER (LEN=*), INTENT(IN)            :: stringerr
   CHARACTER (LEN=8), INTENT(IN)            :: filein
!
   CHARACTER (LEN=60) :: blank
!
!   Read in the line from iunit and determine if it is
!   a comment line.  If so, then read the next line and
!   keep going until a non comment line is found.
!   After that, backspace and read in an integer value.
!
10 READ(iunit,30)blank
!
   IF((blank(1:1) == '!').OR.(blank(1:1) == '#').OR.(blank(1:1)  &
      == '%'))GO TO 10
!
   BACKSPACE(iunit)
!
   READ(iunit,*,ERR=20)i1,i2
!
   RETURN
!
!  If we made it this far, there was an error.
!
20 WRITE(*,40)stringerr,filein
   STOP
!
30 FORMAT(a60)
40 FORMAT('Error reading input file for variable ',a,1X,'in',1X, a)
!
   RETURN
!
END SUBROUTINE rdint2
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE rdint8(iunit,i1,i2,i3,i4,i5,i6,i7,i8,stringerr,  &
   filein)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: iunit
   INTEGER, INTENT(OUT)                     :: i1
   INTEGER, INTENT(OUT)                     :: i2
   INTEGER, INTENT(OUT)                     :: i3
   INTEGER, INTENT(OUT)                     :: i4
   INTEGER, INTENT(OUT)                     :: i5
   INTEGER, INTENT(OUT)                     :: i6
   INTEGER, INTENT(OUT)                     :: i7
   INTEGER, INTENT(OUT)                     :: i8
   CHARACTER (LEN=*), INTENT(IN)            :: stringerr
   CHARACTER (LEN=8), INTENT(IN)            :: filein
!
   CHARACTER (LEN=60) :: blank
!
!   Read in the line from iunit and determine if it is
!   a comment line.  If so, then read the next line and
!   keep going until a non comment line is found.
!   After that, backspace and read in an integer value.
!
10 READ(iunit,30)blank
!
   IF((blank(1:1) == '!').OR.(blank(1:1) == '#').OR.(blank(1:1)  &
      == '%'))GO TO 10
!
   BACKSPACE(iunit)
!
   READ(iunit,*,ERR=20)i1,i2,i3,i4,i5,i6,i7,i8
!
   RETURN
!
!  If we made it this far, there was an error.
!
20 WRITE(*,40)stringerr,filein
   STOP
!
30 FORMAT(a60)
40 FORMAT('Error reading input file for variable ',a,1X,'in',1X,a)
!
   RETURN
!
END SUBROUTINE rdint8
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE rdreal(iunit,r1,stringerr,filein)
!
!   Utility code to read in a real number from unit=iunit.
!   Value is returned in r1.  The name of the quantity
!   is returned as an error in the string stringerr.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: iunit
   REAL(KIND=dp), INTENT(OUT)               :: r1
   CHARACTER (LEN=*), INTENT(IN)            :: stringerr
   CHARACTER (LEN=8), INTENT(IN)            :: filein
!
   CHARACTER (LEN=60) :: blank
!
!   Read in the line from iunit and determine if it is
!   a comment line.  If so, then read the next line and
!   keep going until a non comment line is found.
!   After that, backspace and read in an integer value.
!
10 READ(iunit,30)blank
!
   IF((blank(1:1) == '!').OR.(blank(1:1) == '#').OR.(blank(1:1)  &
      == '%'))GO TO 10
!
   BACKSPACE(iunit)
!
   READ(iunit,*,ERR=20)r1
!
   RETURN
!
!  If we made here then there was an error.
!
20 WRITE(*,40)stringerr,filein
   STOP
!
30 FORMAT(a60)
40 FORMAT('Error reading input file for variable ',a,1X,'in',1X,a)
!
   RETURN
!
END SUBROUTINE rdreal
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE rdreal2(iunit,r1,r2,stringerr,filein)
!
!   Utility code to read in two real numbers from unit=iunit.
!   Values are returned in r1, r2.  The name of the quantities
!   is returned as an error in the string stringerr.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: iunit
   REAL(KIND=dp), INTENT(OUT)               :: r1
   REAL(KIND=dp), INTENT(OUT)               :: r2
   CHARACTER (LEN=*), INTENT(IN)            :: stringerr
   CHARACTER (LEN=8), INTENT(IN)            :: filein
!
   CHARACTER (LEN=60) :: blank
!
!   Read in the line from iunit and determine if it is
!   a comment line.  If so, then read the next line and
!   keep going until a non comment line is found.
!   After that, backspace and read in an integer value.
!
10 READ(iunit,30)blank
!
   IF((blank(1:1) == '!').OR.(blank(1:1) == '#').OR.(blank(1:1)  &
      == '%'))GO TO 10
!
   BACKSPACE(iunit)
!
   READ(iunit,*,ERR=20)r1,r2
!
   RETURN
!
!  If we made it this far, there was an error.
!
20 WRITE(*,40)stringerr,filein
   STOP
!
30 FORMAT(a60)
40 FORMAT('Error reading input file for variable ',a,1X,'in',1X,a)
!
   RETURN
!
END SUBROUTINE rdreal2
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readacfdata(ioptimizer,nlength,nvar,  &
   nfiles,svar,nvmax,istart,maxlag,iplot)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nfiles
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: maxlag
   INTEGER, INTENT(IN)                      :: iplot
!
   REAL(KIND=dp)  :: col1,col2,ave,summ,bigsumm,parmmean(nvmax),amin
!
   REAL(KIND=dp), ALLOCATABLE :: parm(:,:,:),chainvar(:,:)
   REAL(KIND=dp), ALLOCATABLE :: acf(:,:),chainmean(:,:)

   REAL(KIND=pg)  :: xplot(nfiles),yplot(nfiles),summ_sngl(nfiles)
   REAL(KIND=pg)  :: xline(2),yline(2),tausum
!
   INTEGER :: mlength,kcount,i,j,k,k1,kmin,lll,ii,pgbegin,pgerr
!
   CHARACTER(LEN=6) :: extension
   CHARACTER(LEN=40) :: filein,outstring
   CHARACTER(LEN=80) :: suffixstring,labelstring
!
   ALLOCATE(parm(nfiles,nlength,nvmax),chainvar(nlength,nvmax))
   ALLOCATE(acf(nfiles,nlength),chainmean(nlength,nvmax))
!
   amin=9.99E+66_dp
   pgerr=pgbegin(0,'/cps',1,2)
   CALL pgsch(1.05_pg)

   DO i=1,nvmax
      parmmean(i)=0.0_dp
   END DO
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   kcount=0
   DO  k=1,nfiles
      READ(1,30)filein
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      WRITE(*,"('Reading fit file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF(k < istart)CYCLE
      kcount=kcount+1
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      DO j=1,nlength
         IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
            READ(2,*,END=10)col1,col2,(parm(kcount,j,i),i=1,nvar)
            col1=col1*2.0_dp
            col2=col1*col2
         ELSE
            READ(2,*,END=10)(parm(kcount,j,i),i=1,nvar)
         END IF
      END DO
10    CLOSE(2)
      mlength=j-1
   END DO
!
30 FORMAT(a40)
!
   CLOSE(1)
!
   WRITE(*,*)
   bigsumm=0.0_dp
   DO j=1,nlength
      DO i=1,nvar
         summ=0.0_dp
         ave=0.0_dp
         DO k=1,kcount
            summ=summ+parm(k,j,i)
         END DO
         ave=summ/REAL(kcount,KIND=dp)
         chainmean(j,i)=ave
         summ=0.0_dp
         DO k=1,kcount
            parm(k,j,i)=parm(k,j,i)-ave
            summ=summ+(parm(k,j,i))**2
         END DO
         chainvar(j,i)=summ
      END DO
   END DO
!
!   loop over parameters
!
   DO i=1,nvar
      DO j=1,nlength
         DO k=0,maxlag   
            summ=0.00_dp
            DO k1=1,kcount-k
               summ=summ+parm(k1,j,i)*parm(k1+k,j,i)
            END DO
            acf(k+1,j)=summ/chainvar(j,i)
            IF(acf(k+1,j) < amin)amin=acf(k+1,j)
         END DO
      END DO
!
!   We have the ACF for each chain.  Plot them
!
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      CALL pgenv(0.0_pg,REAL(maxlag,KIND=pg),REAL(amin,KIND=pg),1.05_pg,0,0)
      CALL pglabel('Lag','ACF',TRIM(suffixstring))
!
      DO k=1,maxlag   
         summ_sngl(k)=0.0_pg
      END DO
      DO j=1,nlength
         IF(iplot > 0)THEN
            WRITE(extension,999)j+10000
            OPEN(UNIT=33,FILE='ACF_'//TRIM(suffixstring)//  &
               extension,STATUS='unknown')
         END IF
         DO k=1,maxlag   
            xplot(k)=REAL(k,KIND=pg)
            yplot(k)=REAL(acf(k,j),KIND=pg)
            summ_sngl(k)=summ_sngl(k)+yplot(k)
            IF(iplot > 0)WRITE(33,*)xplot(k),yplot(k)
         END DO
         IF(iplot > 0)CLOSE(33)
         CALL pgline(maxlag,xplot,yplot)
      END DO
      tausum=0.0_pg
      kmin=123456
      DO k=1,maxlag   
         yplot(k)=summ_sngl(k)/REAL(nlength,KIND=pg)
         IF(k > INT(5.0_pg*yplot(k)))THEN
            IF(k < kmin)kmin=k
         END IF
         tausum=tausum+yplot(k)
      END DO
      CALL pgsci(2)
      IF(iplot > 0)THEN
         OPEN(UNIT=33,FILE='ACF_'//TRIM(suffixstring)//  &
            '.average',STATUS='unknown')
         DO ii=1,maxlag 
            WRITE(33,*)xplot(ii),yplot(ii)
         END DO
         CLOSE(33)
      END IF
      CALL pgline(maxlag,xplot,yplot)
      CALL pgsci(1)
!
      tausum=1.0_pg+2.0_pg*tausum
      xline(1)=tausum
      xline(2)=xline(1)
      yline(1)=-2.0_pg
      yline(2)=2.0_pg
      CALL pgsci(4)
      CALL pgline(2,xline,yline)
      CALL pgsci(1)
      CALL istringnoequals('auto-correlation length = ',  &
         INT(tausum),outstring,lll)
      WRITE(*,100)TRIM(suffixstring),TRIM(outstring)
!
   END DO
!
   CALL pgend
!
100 FORMAT('parameter',1X,a,', ',a)
999 FORMAT('.',i5)
!
   DEALLOCATE(parm,chainvar)
   DEALLOCATE(acf,chainmean)
!
   RETURN
!
END SUBROUTINE readacfdata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readchidata(nlength,nvar,nfiles,svar,nvmax,thresh,  &
   nextra,extrastr,small)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nfiles
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvmax)
   REAL(KIND=dp), INTENT(IN)                :: thresh
   INTEGER, INTENT(IN)                      :: nextra
   CHARACTER (LEN=50), INTENT(IN)           :: extrastr(58)
   REAL(KIND=dp), INTENT(OUT)               :: small
!
   REAL(KIND=dp), ALLOCATABLE  :: parm(:,:),extra(:,:),chi(:)
!
   REAL(KIND=dp) :: col1
   INTEGER :: mlength,i,j,k,m
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring,labelstring
!
   ALLOCATE(chi(199999),parm(99,199999),extra(58,199999))
!
   OPEN(UNIT=211,FILE='ELCjunk',STATUS='old')
!
   small=1.23E+66_dp
   DO i=1,nvar
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      OPEN(UNIT=10+i,FILE='ELCjunk.'//TRIM(suffixstring),STATUS='unknown')
   END DO
!
   DO i=1,nextra
      OPEN(UNIT=10+i+nvar,FILE='ELCjunk.'//TRIM(extrastr(i)), STATUS='unknown')
   END DO
!
   DO  k=1,nfiles
      READ(211,20)filein
      WRITE(*,"('Reading generation file ',I0,A)",ADVANCE='no')k,CHAR(13)
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      DO j=1,999999+nlength
         IF(nextra == 0)THEN
            READ(2,*,END=15)col1,chi(j),(parm(i,j),i=1,nvar)
         ELSE
            READ(2,*,END=15)col1,chi(j),(parm(i,j),i=1,nvar),  &
               (extra(m,j),m=1,nextra)
         END IF
         col1=col1*2
         IF(chi(j) < small)small=chi(j)
      END DO
15    CLOSE(2)
      mlength=j-1
      DO i=1,nvar
         DO j=1,mlength 
            IF(chi(j) < small+thresh)WRITE(10+i,*)parm(i,j),chi(j)
         END DO
      END DO
      DO i=1,nextra
         DO j=1,mlength 
            IF(chi(j) < small+thresh)WRITE(10+i+nvar,*)extra(i,j),chi(j)
         END DO
      END DO
   END DO
!
   CLOSE(211)
   DO i=1,nvar
      CLOSE(i+10)
   END DO
   DO i=1,nextra
      CLOSE(i+10+nvar)
   END DO
!
   WRITE(*,*)
!
20 FORMAT(a40)
!
   DEALLOCATE(chi,parm,extra)
!
   RETURN
!
END SUBROUTINE readchidata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readchiparm(nparm,indexparm,nfiles,nlength,maxparm,  &
   thresh)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN OUT)                  :: indexparm(199)
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: maxparm
   REAL(KIND=dp), INTENT(IN OUT)            :: thresh
!
   REAL(KIND=dp) :: small,col1
   REAL(KIND=dp), ALLOCATABLE  :: parm(:,:),chi(:)
!
   INTEGER :: i,j,k,k1,mlength
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring(999),labelstring(999)
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   ALLOCATE(chi(199999),parm(199,199999))
!
   small=1.23E+66_dp
   CALL getelcparmstring(suffixstring,labelstring)
   DO i=1,nparm
      OPEN(UNIT=100+i,FILE='ELCjunk.'  &
         //TRIM(suffixstring(indexparm(i))),STATUS='unknown')
   END DO
!
   DO  k=1,nfiles
      READ(1,20)filein
      k1=LEN_TRIM(filein)
      WRITE(*,"('Reading ELCparm file ',I0,A)",ADVANCE='no')k,CHAR(13)
      OPEN(UNIT=2,FILE='ELCparm'//filein(13:k1),STATUS='old')
      DO j=1,999999+nlength
         READ(2,*,END=9)col1,chi(j),(parm(i,j),i=1,maxparm-2)
         col1=col1*2
         IF(chi(j) < small)small=chi(j)
      END DO
9     CLOSE(2)
      mlength=j-1
      DO i=1,nparm
         DO j=1,mlength
            IF(chi(j) < small+thresh)THEN
               WRITE(100+i,*)parm(indexparm(i)-2,j),chi(j)
            END IF
         END DO
      END DO
   END DO
!
   CLOSE(1)
   DO i=1,nparm
      CLOSE(i+100)
   END DO
   WRITE(*,*)
!
20 FORMAT(a40)
!
   DEALLOCATE(parm,chi)
!
   RETURN
!
END SUBROUTINE readchiparm
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readchiratio(nparm,indexparm,nfiles,nlength,  &
   maxparm,thresh)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN)                      :: indexparm(199)
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: maxparm
   REAL(KIND=dp), INTENT(IN)                :: thresh
!
   REAL(KIND=dp), ALLOCATABLE :: parm(:,:),chi(:)

   REAL(KIND=dp)  :: col1,small
!
   INTEGER :: i,j,k,k1,mlength
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring(999),labelstring(999)
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   ALLOCATE(parm(199,199999),chi(199999))
!
   small=1.23E+66_dp
   CALL getelcratiostring(suffixstring,labelstring)
   DO i=1,nparm
      OPEN(UNIT=100+i,FILE='ELCjunk.'  &
         //TRIM(suffixstring(indexparm(i))),STATUS='unknown')
   END DO
!
   DO  k=1,nfiles
      READ(1,20)filein
      k1=LEN_TRIM(filein)
      WRITE(*,"('Reading ELCratio file ',I0,A)",ADVANCE='no')k,CHAR(13)
      OPEN(UNIT=2,FILE='ELCratio'//filein(13:k1),STATUS='old')
      DO j=1,999999+nlength
         READ(2,*,END=9)col1,chi(j),(parm(i,j),i=1,maxparm)
         col1=col1*2
         IF(chi(j) < small)small=chi(j)
      END DO
9     CLOSE(2)
      mlength=j-1
      DO i=1,nparm
         DO j=1,mlength
            IF(chi(j) < small+thresh)THEN
               WRITE(100+i,*)parm(indexparm(i),j),chi(j)
            END IF
         END DO
      END DO
   END DO
!
   CLOSE(1)
   DO i=1,nparm
      CLOSE(i+100)
   END DO
   WRITE(*,*)
!
20 FORMAT(a40)
!
   DEALLOCATE(parm,chi)
!
   RETURN
!
END SUBROUTINE readchiratio
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readcordata(ioptimizer,nlength,nvar,nfiles,svar,  &
   nvmax,istart,nskip,nextra,extrastr,istop)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nfiles
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: nskip
   INTEGER, INTENT(IN)                      :: nextra
   CHARACTER (LEN=50), INTENT(IN)           :: extrastr(58)
   INTEGER, INTENT(IN)                      :: istop
!
   REAL(KIND=dp)   :: col1,col2
!
   REAL(KIND=dp), ALLOCATABLE  ::  chi(:),extra(:,:),parm(:,:)
!
   INTEGER :: i,j,k,mlength,m,ios,ifile
!
   CHARACTER(LEN=40) :: filein,filechi
   CHARACTER(LEN=80) :: suffixstring,labelstring
!
   ALLOCATE(chi(9999),extra(58,9999),parm(99,9999))
!
   ios=0
   ifile=0
   OPEN(UNIT=211,FILE='ELCjunk',STATUS='old')
   OPEN(UNIT=3,FILE='ELCjunkchi',STATUS='old',IOSTAT=ios,ERR=5)
!
   OPEN(UNIT=311,FILE='ELCposterior.files',STATUS='unknown')
!
5  IF(ios /= 0)WRITE(*,*)'No chi^2 files'
!
   DO i=1,nvar
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      OPEN(UNIT=10+i,FILE='ELCjunk.'//TRIM(suffixstring),STATUS='unknown')
   END DO
   DO i=1,nextra
      OPEN(UNIT=10+i+nvar,FILE='ELCjunk.'//TRIM(extrastr(i)),STATUS='unknown')
   END DO
   IF(ios == 0)OPEN(UNIT=10+nvar+nextra+1,FILE='ELCjunk.chi',STATUS='unknown')
!
   DO  k=1,istop
      READ(211,20)filein
      IF(ios == 0)READ(3,20)filechi
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      ifile=ifile+1
      IF(MOD(ifile,nskip) /= 1)CYCLE
      WRITE(*,"('Reading fit file ',I0,A)",ADVANCE='no')k,CHAR(13)
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      WRITE(311,20)filein
      IF(ios == 0)OPEN(UNIT=4,FILE=filechi,STATUS='old')
      DO j=1,9999+nlength
         IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
            IF(nextra == 0)THEN
               READ(2,*,END=15)col1,col2,(parm(i,j),i=1,nvar)
            ELSE
               READ(2,*,END=15)col1,col2,(parm(i,j),i=1,nvar),  &
                  (extra(m,j),m=1,nextra)
            END IF
            col1=col1*2.0_dp
            col2=col1*col2
            IF(ios == 0)READ(4,*,END=15)col1,chi(j)
         ELSE
            IF(nextra == 0)THEN
               READ(2,*,END=15)(parm(i,j),i=1,nvar)
            ELSE
               READ(2,*,END=15)(parm(i,j),i=1,nvar),(extra(m,j),m=1,nextra)
            END IF
            IF(ios == 0)READ(4,*,END=15)chi(j)
         END IF
      END DO
15    CLOSE(2)
      IF(ios == 0)CLOSE(4)
      mlength=j-1
      DO i=1,nvar
         DO j=1,mlength
            WRITE(10+i,*)parm(i,j)
         END DO
      END DO
      DO i=1,nextra
         DO j=1,mlength
            WRITE(10+i+nvar,*)extra(i,j)
         END DO
      END DO
      IF(ios == 0)THEN
         DO j=1,mlength
            WRITE(10+nvar+nextra+1,*)chi(j)
         END DO
      END IF
   END DO
!
   DO i=1,nvar
      CLOSE(i+10)
   END DO
   DO i=1,nextra
      CLOSE(i+10+nvar)
   END DO
   IF(ios == 0)CLOSE(10+nvar+1+nextra)
   WRITE(*,*)
!
20 FORMAT(a40)
!
   CLOSE(211)
   CLOSE(311)
   CLOSE(3)
!
   DEALLOCATE(chi,extra,parm)
!
   RETURN
!
END SUBROUTINE readcordata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readcorparm(nparm,indexparm,nfiles,nlength,maxparm,  &
   ioptimizer,istart,nskip,istop,isepar)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN)                      :: indexparm(199)
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: maxparm
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: nskip
   INTEGER, INTENT(IN)                      :: istop
   INTEGER, INTENT(IN)                      :: isepar
!
   REAL(KIND=dp)   ::  col1,fac1,fac2
!
   REAL(KIND=dp), ALLOCATABLE :: parm(:,:),chi(:)
!
   INTEGER :: lengthtemp,i,j,k,k1,ifile
!
   CHARACTER (LEN=40) :: filein
   CHARACTER (LEN=80) :: suffixstring(999),labelstring(999)
!
   ALLOCATE(chi(9999),parm(199,9999))
!
   ifile=0
   lengthtemp=nlength
!
   OPEN(UNIT=211,FILE='ELCjunk',STATUS='old')
!
   CALL getelcparmstring(suffixstring,labelstring)
   DO i=1,nparm
      OPEN(UNIT=100+i,FILE='ELCjunk.'  &
         //TRIM(suffixstring(indexparm(i))),STATUS='unknown')
   END DO
   IF(isepar == 3)OPEN(UNIT=100+nparm+1,FILE='ELCjunk.a1',STATUS='unknown')
   IF(isepar == 3)OPEN(UNIT=100+nparm+2,FILE='ELCjunk.a2',STATUS='unknown')
!
   DO  k=1,istop   
      READ(211,20)filein
      k1=LEN_TRIM(filein)
      IF(k == nfiles)CYCLE
      IF(k < istart)CYCLE
      ifile=ifile+1
      IF(MOD(ifile,nskip) /= 1)CYCLE
      WRITE(*,"('Reading parm file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF((ioptimizer /= 3).AND.(ioptimizer /= 12))THEN
         IF(ioptimizer == 2)THEN
            OPEN(UNIT=2,FILE='demcmc_starparm'//filein(17:k1),STATUS='old')
            DO j=1,lengthtemp
               READ(2,*)(parm(i,j),i=1,maxparm-2)
            END DO
            CLOSE(2)
         END IF
         IF(ioptimizer == 10)THEN
            OPEN(UNIT=2,FILE='hammer_starparm'//filein(17:k1),STATUS='old')
            DO j=1,lengthtemp
               READ(2,*)(parm(i,j),i=1,maxparm-2)
            END DO
            CLOSE(2)
         END IF
      ELSE
         OPEN(UNIT=2,FILE='ELCparm.'//filein(14:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*)col1,chi(j),(parm(i,j),i=1,maxparm-2)
            col1=col1*2
            chi(j)=chi(j)**2
         END DO
         CLOSE(2)
      END IF
      DO i=1,nparm
         DO j=1,lengthtemp
            WRITE(100+i,*)parm(indexparm(i)-2,j)
         END DO
      END DO
      IF(isepar == 3)THEN
         DO j=1,lengthtemp
            fac1=parm(33,j)
            fac2=parm(1,j)+parm(2,j)
            WRITE(100+nparm+1,*)parm(2,j)*fac1/fac2
            WRITE(100+nparm+2,*)parm(1,j)*fac1/fac2
         END DO
      END IF
   END DO
!
   CLOSE(211)
   DO i=1,nparm
      CLOSE(i+100)
   END DO
   IF(isepar == 3)CLOSE(nparm+100+1)
   IF(isepar == 3)CLOSE(nparm+100+2)
   WRITE(*,*)
!
20 FORMAT(a40)
!
   DEALLOCATE(parm,chi)
!
   RETURN
!
END SUBROUTINE readcorparm
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readcorratio(nparm,indexparm,nfiles,nlength,  &
   maxparm,ioptimizer,istart,nskip,istop)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN)                      :: indexparm(199)
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: maxparm
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: nskip
   INTEGER, INTENT(IN)                      :: istop
!
   REAL(KIND=dp), ALLOCATABLE  :: parm(:,:),chi(:)
   REAL(KIND=dp)  :: col1
!
   INTEGER :: lengthtemp,i,j,k,k1,ifile
!
   CHARACTER (LEN=40) :: filein
   CHARACTER (LEN=80) :: suffixstring(999),labelstring(999)
!
   ALLOCATE(parm(199,9999),chi(9999))
!
   lengthtemp=nlength
   ifile=0
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   CALL getelcratiostring(suffixstring,labelstring)
   DO i=1,nparm
!      k1=lnblnk(suffixstring(indexparm(i)))
      OPEN(UNIT=100+i,FILE='ELCjunk.'  &
         //TRIM(suffixstring(indexparm(i))),STATUS='unknown')
   END DO
!
   DO  k=1,istop    !Nfiles
      READ(1,20)filein
      k1=LEN_TRIM(filein)
      IF(k == nfiles)CYCLE
      IF(k < istart)CYCLE
      ifile=ifile+1
      IF(MOD(ifile,nskip) /= 1)CYCLE
      WRITE(*,"('Reading ratio file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF(ioptimizer /= 3)THEN
         IF(ioptimizer == 2)THEN
            OPEN(UNIT=2,FILE='demcmc_ratio'//filein(17:k1),STATUS='old')
            DO j=1,lengthtemp
               READ(2,*)(parm(i,j),i=1,maxparm)
            END DO
            CLOSE(2)
         END IF
         IF(ioptimizer == 10)THEN
            OPEN(UNIT=2,FILE='hammer_ratio'//filein(17:k1),STATUS='old')
            DO j=1,lengthtemp
               READ(2,*)(parm(i,j),i=1,maxparm)
            END DO
            CLOSE(2)
         END IF
      ELSE
         OPEN(UNIT=2,FILE='ELCratio.'//filein(13:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*)col1,chi(j),(parm(i,j),i=1,maxparm)
            col1=col1*2
            chi(j)=chi(j)**2
         END DO
         CLOSE(2)
      END IF
      DO i=1,nparm
         DO j=1,lengthtemp
            WRITE(100+i,*)parm(indexparm(i),j)
         END DO
      END DO
   END DO
!
   CLOSE(1)
   DO i=1,nparm
      CLOSE(i+100)
   END DO
   WRITE(*,*)
!
20 FORMAT(a40)
!
   DEALLOCATE(parm,chi)

   RETURN
!
END SUBROUTINE readcorratio
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readdata(ioptimizer,nlength,nvar,nfiles,svar,  &
   nvmax,istart,nextra,extrastr)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nfiles
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: nextra
   CHARACTER (LEN=50), INTENT(IN)           :: extrastr(58)
!
   REAL(KIND=dp)  :: col1,col2
!
   REAL(KIND=dp), ALLOCATABLE    :: parm(:,:),extra(:,:)

   INTEGER :: i,j,k,loopup,ilength,m
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring,labelstring
!
   ALLOCATE(extra(50,99999),parm(99,99999))
!
   loopup=nlength
   IF(ioptimizer == 12)loopup=99999
   OPEN(UNIT=211,FILE='ELCjunk',STATUS='old')
!
   DO i=1,nvar
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      OPEN(UNIT=10+i,FILE='ELCjunk.'//TRIM(suffixstring),STATUS='unknown')
   END DO
   IF(nextra > 0)THEN
      DO i=1,nextra
         OPEN(UNIT=10+i+nvar,FILE='ELCjunk.'//TRIM(extrastr(i)),STATUS=  &
            'unknown')
      END DO
   END IF
!
   DO  k=1,nfiles
      READ(211,20)filein
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      WRITE(*,"('Reading fit file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF((ioptimizer == 12).AND.(k == 1))CYCLE
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      DO j=1,loopup
         IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
            IF(nextra == 0)THEN
               READ(2,*,END=9)col1,col2,(parm(i,j),i=1,nvar)
               col1=col1*2.0_dp
               col2=col1*col2
            ELSE
               READ(2,*,END=9)col1,col2,(parm(i,j),i=1,nvar),(extra(m,j),m=1, &
                   nextra)
               col1=col1*2.0_dp
               col2=col1*col2
            END IF
         ELSE
            IF(nextra == 0)THEN
               READ(2,*,END=9)(parm(i,j),i=1,nvar)
            ELSE
               READ(2,*,END=9)(parm(i,j),i=1,nvar),(extra(m,j),m=1,nextra)
            END IF
         END IF
      END DO
9     CLOSE(2)
      ilength=j-1
      DO i=1,nvar
         DO j=1,ilength
            WRITE(10+i,*)parm(i,j),REAL(k,KIND=dp)
         END DO
      END DO
      IF(nextra > 0)THEN
         DO i=1,nextra
            DO j=1,ilength
               WRITE(10+i+nvar,*)extra(i,j),REAL(k,KIND=dp)
            END DO
         END DO
      END IF
!
   END DO
!
   CLOSE(211)
   DO i=1,nvar
      CLOSE(i+10)
   END DO
   IF(nextra > 0)THEN
      DO i=1,nextra
         CLOSE(i+10+nvar)
      END DO
   END IF
!
   WRITE(*,*)
!
   DEALLOCATE(parm,extra)
!
20 FORMAT(a40)
!
   RETURN
!
END SUBROUTINE readdata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readextra(ioptimizer,nlength,nvar,nfiles,  &
   istart,nextra,extrastr)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: nextra
   CHARACTER (LEN=50), INTENT(IN)           :: extrastr(58)
!
   REAL(KIND=dp)  :: col1,col2
!
   REAL(KIND=dp), ALLOCATABLE   ::  parm(:,:),extra(:,:)
!
   INTEGER :: i,j,k,loopup,ilength,m
!
   CHARACTER(LEN=40) :: filein
!
   ALLOCATE(extra(58,99999),parm(99,99999))
!
   loopup=nlength
   IF(ioptimizer == 12)loopup=99999
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   DO i=1,nextra
      OPEN(UNIT=10+i,FILE='ELCjunk.'//TRIM(extrastr(i)),STATUS='unknown')
   END DO
!
   DO  k=1,nfiles
      READ(1,20)filein
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      WRITE(*,"('Reading fit file ',I0,A)",ADVANCE='no')k,CHAR(13)
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      IF((ioptimizer == 12).AND.(k == 1))CYCLE
      DO j=1,loopup
         IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
            READ(2,*,END=9)col1,col2,(parm(i,j),i=1,nvar),(extra(m,j),m=1, &
                nextra)
            col1=col1*2.0_dp
            col2=col1*col2
         ELSE
            READ(2,*,END=9)(parm(i,j),i=1,nvar),(extra(m,j),m=1,nextra)
         END IF
      END DO
9     CLOSE(2)
      ilength=j-1
      DO i=1,nextra
         DO j=1,ilength
            WRITE(10+i,*)extra(i,j),REAL(k,KIND=dp)
         END DO
      END DO
   END DO
!
   DO i=1,nextra
      CLOSE(i+10)
   END DO
   WRITE(*,*)
!
   DEALLOCATE(extra,parm)
!
20 FORMAT(a40)
!
   RETURN
!
END SUBROUTINE readextra
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readfitspread(ioptimizer,nlength,nfiles,istart,  &
   istop,xlow,xhigh,nvar,needindex)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: istop
   REAL(KIND=pg), INTENT(OUT)               :: xlow
   REAL(KIND=pg), INTENT(OUT)               :: xhigh
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: needindex
!
   REAL(KIND=dp), ALLOCATABLE  :: parm(:),col(:,:)
   REAL(KIND=dp)  :: z1,z2
!
   INTEGER :: i,j,k,loopup
!
   CHARACTER (LEN=7) :: suffixstring
   CHARACTER (LEN=80) :: filein
!
   ALLOCATE(parm(999999),col(nlength,nvar))
!
   loopup=nlength
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   xlow=1.0E+33_pg
   xhigh=-1.0E+33_pg
   DO i=1,nlength
      WRITE(suffixstring,5)i+1000000
      OPEN(UNIT=10+i,FILE='ELCjunk.'//suffixstring,STATUS='unknown')
   END DO
!
   DO  k=1,istop
      READ(1,20)filein
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      WRITE(*,"('Reading generation file ',I0,A)",ADVANCE='no')k,CHAR(13)
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      DO j=1,nlength
         IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
            READ(2,*,END=9)z1,z2,(col(j,i),i=1,nvar)
            z1=2.0_dp*z1+z2
            z2=2.0_dp*z2+z1
            parm(j)=col(j,needindex)
            IF(REAL(parm(j),KIND=pg) < xlow)xlow=REAL(parm(j),KIND=pg)
            IF(REAL(parm(j),KIND=pg) > xhigh)xhigh=REAL(parm(j),KIND=pg)
         ELSE
            READ(2,*,END=9)(col(j,i),i=1,nvar)
            parm(j)=col(j,needindex)
            IF(REAL(parm(j),KIND=pg) < xlow)xlow=REAL(parm(j),KIND=pg)
            IF(REAL(parm(j),KIND=pg) > xhigh)xhigh=REAL(parm(j),KIND=pg)
         END IF
      END DO
9     CLOSE(2)
      DO j=1,nlength
         WRITE(10+j,*)k,parm(j)
      END DO
   END DO
!
   DO i=1,nlength
      CLOSE(i+10)
   END DO
   WRITE(*,*)
!
5  FORMAT(i7)
20 FORMAT(a40)
!
   DEALLOCATE(parm,col)

   RETURN
!
END SUBROUTINE readfitspread
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readfluxspread(ioptimizer,nlength,nfiles,istart,  &
   istop,xlow,xhigh,needindex,maxparm)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: istop
   REAL(KIND=pg), INTENT(OUT)               :: xlow
   REAL(KIND=pg), INTENT(OUT)               :: xhigh
   INTEGER, INTENT(IN)                      :: needindex
   INTEGER, INTENT(IN)                      :: maxparm
!
   REAL(KIND=pg)               :: fred
   REAL(KIND=dp)               :: z1,z2
   REAL(KIND=dp), ALLOCATABLE  :: col(:,:)
!
   INTEGER :: k1,i,j,k,loopup
!
   CHARACTER (LEN=7) :: suffixstring
   CHARACTER (LEN=80) :: filein
!
   ALLOCATE(col(nlength,maxparm))
!
   loopup=nlength
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   xlow=1.0E+33_pg
   xhigh=-1.0E+33_pg
   DO i=1,nlength
      WRITE(suffixstring,20)i+1000000
      OPEN(UNIT=10+i,FILE='ELCjunk.'//suffixstring,STATUS= 'unknown')
   END DO
!
   DO  k=1,istop
      READ(1,30)filein
      k1=LEN_TRIM(filein)
      WRITE(*,"('Reading ratio file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      IF(k == nfiles)CYCLE
      IF(ioptimizer /= 3)THEN
         IF(ioptimizer == 2)THEN
            OPEN(UNIT=2,FILE='demcmc_ratio'//filein(17:k1),STATUS='old')
            DO j=1,nlength
               READ(2,*)(col(j,i),i=1,maxparm)
            END DO
            CLOSE(2)
         END IF
         IF(ioptimizer == 10)THEN
            OPEN(UNIT=2,FILE='hammer_ratio'//filein(17:k1),STATUS='old')
            DO j=1,nlength
               READ(2,*)(col(j,i),i=1,maxparm)
            END DO
            CLOSE(2)
         END IF
      ELSE
         OPEN(UNIT=2,FILE='ELCratio.'//filein(18:k1),STATUS='old')
         DO j=1,nlength
            READ(2,*)z1,z2,(col(j,i),i=1,maxparm)
            z1=2.8_dp*z1
            z2=z2-z1
         END DO
         CLOSE(2)
      END IF
      DO j=1,nlength
         WRITE(10+j,*)k,col(j,needindex)
         fred=REAL(col(j,needindex),KIND=pg)
         IF(fred > xhigh)xhigh=fred
         IF(fred < xlow)xlow=fred
      END DO
   END DO
!
   DO i=1,nlength
      CLOSE(i+10)
   END DO
   WRITE(*,*)
!
20 FORMAT(i7)
30 FORMAT(a40)
!
   DEALLOCATE(col)

   RETURN
!
END SUBROUTINE readfluxspread
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readgelmanrubindata(ioptimizer,nlength,nvar,  &
   nfiles,svar,nvmax,istart,istop,nskip)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nfiles
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: istop
   INTEGER, INTENT(IN)                      :: nskip
!
   REAL(KIND=dp)  :: col1,col2,ave,summ,bigsumm
   REAL(KIND=dp)  :: parmmean(nvmax),b(nvmax),w(nvmax),r(nvmax)
   REAL(KIND=dp)  :: vbar(nvmax),sig
!
   REAL(KIND=dp), ALLOCATABLE  :: parm(:,:,:)
   REAL(KIND=dp), ALLOCATABLE  :: chainmean(:,:),chainvar(:,:)
!
   INTEGER :: mlength,kcount,i,j,k
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring,labelstringnbsh
!
   DO i=1,nvmax
      parmmean(i)=0.0_dp
   END DO
!
   ALLOCATE(parm(nfiles,nlength,nvmax))
   ALLOCATE(chainmean(nlength,nvmax),chainvar(nlength,nvmax))
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
   OPEN(UNIT=44,FILE='ELCgelmanrubin.out',STATUS='unknown')
!
   kcount=0
   DO  k=1,istop
      READ(1,30) filein
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      WRITE(*,"('Reading fit file ',I0,A)",ADVANCE='no') k,CHAR(13)
      IF(k < istart)CYCLE
      IF(MOD(k,nskip) /= 1)CYCLE
      kcount=kcount+1
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      DO j=1,nlength
         IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
            READ(2,*,END=10)col1,col2,(parm(kcount,j,i),i=1,nvar)
            col1=col1*2.0_dp
            col2=col1*col2
         ELSE
            READ(2,*,END=10)(parm(kcount,j,i),i=1,nvar)
         END IF
      END DO
10    CLOSE(2)
      mlength=j-1
   END DO
!
30 FORMAT(a40)
!
   CLOSE(1)
!
   WRITE(*,*)
   bigsumm=0.0_dp
   DO j=1,nlength
      DO i=1,nvar
         summ=0.0_dp
         ave=0.0_dp
         DO k=1,kcount
            summ=summ+parm(k,j,i)
         END DO
         ave=summ/REAL(kcount,KIND=dp)
         chainmean(j,i)=ave
         summ=0.0_dp
         DO k=1,kcount
            summ=summ+(parm(k,j,i)-ave)**2
         END DO
         chainvar(j,i)=summ/REAL(kcount-1,KIND=dp)
      END DO
   END DO
!
   DO i=1,nvar
      bigsumm=0.0_dp
      DO j=1,nlength
         bigsumm=bigsumm+chainmean(j,i)
      END DO
      parmmean(i)=bigsumm/REAL(nlength,KIND=dp)
   END DO
!
   DO i=1,nvar
      bigsumm=0.0_dp
      DO j=1,nlength
         bigsumm=bigsumm+(chainmean(j,i)-parmmean(i))**2
      END DO
      b(i)=1.0_dp/REAL(nlength-1,KIND=dp)*bigsumm
!
      bigsumm=0.0_dp
      DO j=1,nlength
         bigsumm=bigsumm+chainvar(j,i)
      END DO
      w(i)=1.0_dp/REAL(nlength,KIND=dp)*bigsumm
!
      sig=REAL(kcount-1,KIND=dp)/REAL(kcount,KIND=dp)*w(i)+b(i)
      vbar(i)=sig+b(i)/REAL(nlength,KIND=dp)
!
      r(i)=SQRT(vbar(i)/w(i))
!
      CALL getsuffixnbsh(svar(i)(1:2),suffixstring,labelstringnbsh)
!      k1=lnblnk(labelstring)
!
      WRITE(44,40)r(i),parmmean(i),TRIM(labelstringnbsh)
!
   END DO
!
40 FORMAT(f10.6,2X,f20.13,3X,a)
!
   DEALLOCATE(parm,chainmean,chainvar)
!
   RETURN
!
END SUBROUTINE readgelmanrubindata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readnesteddata(nvar,svar,nvmax,nextra,extrastr,  &
   nlive,nsamp,nparm,indexparm,maxparm,isepar)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nvmax
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: nextra
   CHARACTER (LEN=50), INTENT(IN)           :: extrastr(58)
   INTEGER, INTENT(IN)                      :: nlive
   INTEGER, INTENT(IN)                      :: nsamp
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN)                      :: indexparm(199)
   INTEGER, INTENT(IN)                      :: maxparm
   INTEGER, INTENT(IN)                      :: isepar
!
   REAL(KIND=dp)  :: logwtmax,logran,col1,col2,fac1,fac2,ran2
!
   REAL(KIND=dp), ALLOCATABLE  :: liveparm(:,:),logwt(:)
   REAL(KIND=dp), ALLOCATABLE  :: liveextra(:,:),deadparm(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: deadextra(:,:),logllive(:)
   REAL(KIND=dp), ALLOCATABLE  :: logldead(:),logl(:)
   REAL(KIND=dp), ALLOCATABLE  :: parm(:,:),extra(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: livederive(:,:),deadderive(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: derive(:,:)
!
   INTEGER                   :: i,j,k,loopup,m,idum,ndead,n
   INTEGER, DIMENSION(49999) :: indxlive,indxdead
!
   CHARACTER (LEN=40) :: filein,fileinparm
   CHARACTER (LEN=80) :: suffixstring,labelstring
   CHARACTER (LEN=80) :: psuffixstring(999),plabelstring(999)
!
   loopup=49999
   idum=-123456
!
   ALLOCATE(liveparm(99,49999),logwt(49999))
   ALLOCATE(liveextra(58,49999),deadparm(99,49999))
   ALLOCATE(deadextra(58,49999),logllive(49999))
   ALLOCATE(logldead(49999),logl(49999))
   ALLOCATE(parm(99,49999),extra(11,49999))
   ALLOCATE(livederive(99,49999),deadderive(99,49999))
   ALLOCATE(derive(99,49999))
!
   DO i=1,nvar
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      OPEN(UNIT=10+i,FILE='ELCjunk.'//TRIM(suffixstring),STATUS='unknown')
   END DO
   IF(nextra > 0)THEN
      DO i=1,nextra
         OPEN(UNIT=10+i+nvar,FILE='ELCjunk.'//TRIM(extrastr(i)),STATUS= &
            'unknown')
      END DO
   END IF
!
   IF(nparm > 0)THEN
      CALL getelcparmstring(psuffixstring,plabelstring)
      DO i=1,nparm
         OPEN(UNIT=100+i,FILE='ELCjunk.'  &
            //TRIM(psuffixstring(indexparm(i))),STATUS='unknown')
      END DO
   END IF
   IF(isepar == 3)OPEN(UNIT=100+nparm+1,FILE='ELCjunk.a1',STATUS='unknown')
   IF(isepar == 3)OPEN(UNIT=100+nparm+2,FILE='ELCjunk.a2',STATUS='unknown')
!
   CALL execute_command_line('ls ELClivegeneration.1* > ELCjunk')
   OPEN(UNIT=311,FILE='ELCjunk',STATUS='old')
   DO k=1,4999999
      READ(311,40,END=10)filein
   END DO
10 CLOSE(311)
!
!   filein has the last ELClivegeneration.1* file.
!
   IF(nparm > 0)THEN
      CALL execute_command_line('ls ELCliveparm.1* > ELCjunk')
      OPEN(UNIT=311,FILE='ELCjunk',STATUS='old')
      DO k=1,4999999
         READ(311,40,END=11)fileinparm
      END DO
11    CLOSE(311)
   END IF
!
!   fileinparm has the last ELClivegeneration.1* file
!
!   Attempt to read the files
!
   OPEN(UNIT=2,FILE=filein,STATUS='old')
   IF(nparm > 0)OPEN(UNIT=3,FILE=fileinparm,STATUS='old')
   DO j=1,nlive
      IF(nextra == 0)THEN
         READ(2,*,END=20)col1,logllive(j),(liveparm(i,j),i=1,nvar)
         col1=col1*2.0_dp
      ELSE
         READ(2,*,END=20)col1,logllive(j),(liveparm(i,j),i=1,nvar),  &
            (liveextra(m,j),m=1,nextra)
         col1=col1*2.0_dp
      END IF
      IF(nparm > 0)THEN
         READ(3,*,END=20)col1,col2,(livederive(i,j),i=1,maxparm-2)
         col2=col2**2
      END IF
      logllive(j)=-logllive(j)
   END DO
20 CLOSE(2)
   IF(nparm > 0)CLOSE(3)
!
!   Now do a similar thing for the deadpoints file.
!
   OPEN(UNIT=2,FILE='ELCdeadpoints.generation',STATUS='old')
   IF(nparm > 0)OPEN(UNIT=3,FILE='ELCdeadpoints.parm',STATUS='old')
   DO j=1,loopup
      IF(nextra == 0)THEN
         READ(2,*,END=30)col1,logldead(j),(deadparm(i,j),i=1,nvar)
         col1=col1*2.0_dp
      ELSE
         READ(2,*,END=30)col1,logldead(j),(deadparm(i,j),i=1,nvar),  &
            (deadextra(m,j),m=1,nextra)
         col1=col1*2.0_dp
      END IF
      IF(nparm > 0)THEN
         READ(3,*,END=20)col1,col2,(deadderive(i,j),i=1,maxparm-2)
         col2=col2**2
      END IF
      logldead(j)=-logldead(j)
   END DO
30 CLOSE(2)
   IF(nparm > 0)CLOSE(3)
!
   ndead=j-1
!
   n=nlive+ndead
!
!   sort the live points by logL, and sort the
!   dead points by logL, and combine the two.
!
   CALL indexx(nlive,logllive,indxlive)
   CALL indexx(ndead,logldead,indxdead)
!
   DO i=1,ndead
      logl(i)=logldead(indxdead(i))
      DO j=1,nvar
         parm(j,i)=deadparm(j,indxdead(i))
      END DO
      DO j=1,nextra
         extra(j,i)=deadextra(j,indxdead(i))
      END DO
      IF(nparm > 0)THEN
         DO j=1,maxparm-2   !Nparm
            derive(j,i)=deadderive(j,indxdead(i))
         END DO
      END IF
   END DO
!
   DO i=1,nlive
      logl(i+ndead)=logllive(indxlive(i))
      DO j=1,nvar
         parm(j,i+ndead)=liveparm(j,indxlive(i))
      END DO
      DO j=1,nextra
         extra(j,i+ndead)=liveextra(j,indxlive(i))
      END DO
      IF(nparm > 0)THEN
         DO j=1,maxparm-2   !Nparm
            derive(j,i+ndead)=livederive(j,indxlive(i))
         END DO
      END IF
   END DO
!
!   get the weights
!
   DO i=1,ndead
      logwt(i)=logl(i)-REAL(i,KIND=dp)/REAL(nlive,KIND=dp)
   END DO
!
   DO i=1,nlive
      logwt(i+ndead)=logl(i+ndead)-REAL(ndead,KIND=dp)/REAL(nlive,KIND=dp)
   END DO
!
   logwtmax=-9.99E+99_dp
!
   DO i=1,n
      IF(logwt(i) > logwtmax)logwtmax=logwt(i)
   END DO
!
!   normalize the weights
!
   DO i=1,n
      logwt(i)=logwt(i)-logwtmax
   END DO
!
!   Now do the posterior.
!
   OPEN(UNIT=311,FILE='ELCjunk.logL',STATUS='unknown')
!
   DO k=1,nsamp
      DO j=1,n
         logran=LOG(ran2(idum))
         IF(logwt(j) > logran)THEN
            WRITE(311,*)logl(j)
            DO i=1,nvar
               WRITE(10+i,*)parm(i,j)
            END DO
            IF(nextra > 0)THEN
               DO i=1,nextra
                  WRITE(10+i+nvar,*)extra(i,j)
               END DO
            END IF
            IF(nparm > 0)THEN
               DO i=1,nparm
                  WRITE(100+i,*)derive(indexparm(i)-2,j)
               END DO
            END IF
            IF(isepar == 3)THEN
               fac1=derive(33,j)
               fac2=derive(1,j)+derive(2,j)
               WRITE(100+nparm+1,*)derive(2,j)*fac1/fac2
               WRITE(100+nparm+2,*)derive(1,j)*fac1/fac2
            END IF
!
         END IF
      END DO
   END DO
!
   CLOSE(311)
   DO i=1,nvar
      CLOSE(i+10)
   END DO
   IF(nextra > 0)THEN
      DO i=1,nextra
         CLOSE(i+10+nvar)
      END DO
   END IF
   IF(nparm > 0)THEN
      DO i=1,nparm
         CLOSE(i+100)
      END DO
   END IF
   IF(isepar == 3)CLOSE(100+nparm+1)
   IF(isepar == 3)CLOSE(100+nparm+2)
   WRITE(*,*)
!
40 FORMAT(a40)
!
   DEALLOCATE(liveparm,logwt)
   DEALLOCATE(liveextra,deadparm)
   DEALLOCATE(deadextra,logllive)
   DEALLOCATE(logldead,logl)
   DEALLOCATE(parm,extra)
   DEALLOCATE(livederive,deadderive)
   DEALLOCATE(derive)
!
   RETURN
!
END SUBROUTINE readnesteddata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readnestedparm(nparm,indexparm,nfiles,nlength,  &
   maxparm,ioptimizer,istart)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN)                      :: indexparm(199)
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: maxparm
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: istart
!
   REAL(KIND=dp), ALLOCATABLE  :: parm(:,:),chi(:)
   REAL(KIND=dp)  :: col1
!
   INTEGER :: lengthtemp,i,j,k,k1
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring(999),labelstring(999)
!
   ALLOCATE(parm(199,9999),chi(9999))
!
   lengthtemp=nlength
!    
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   CALL getelcparmstring(suffixstring,labelstring)
   DO i=1,nparm
      OPEN(UNIT=100+i,FILE='ELCjunk.'//TRIM(suffixstring(indexparm(i))),  &
         STATUS='unknown')
   END DO
!
   DO  k=istart,nfiles
      READ(1,20)filein
      k1=LEN_TRIM(filein)
      IF(k < istart)CYCLE
      WRITE(*,"('Reading parm file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF(k == nfiles)CYCLE
      IF(ioptimizer == 2)THEN
         OPEN(UNIT=2,FILE='demcmc_starparm'//filein(17:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*)(parm(i,j),i=1,maxparm-2)
         END DO
         CLOSE(2)
      END IF
      IF(ioptimizer == 10)THEN
         OPEN(UNIT=2,FILE='hammer_starparm'//filein(17:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*)(parm(i,j),i=1,maxparm-2)
         END DO
         CLOSE(2)
      END IF
      IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
         OPEN(UNIT=2,FILE='ELCliveparm'//filein(20:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*)col1,chi(j),(parm(i,j),i=1,maxparm-2)
            col1=col1*2
            chi(j)=chi(j)**2
         END DO
         CLOSE(2)
      END IF
      DO i=1,nparm
         DO j=1,lengthtemp
            WRITE(100+i,*)parm(indexparm(i)-2,j),k
         END DO
      END DO
   END DO
!
   CLOSE(1)
   DO i=1,nparm
      CLOSE(i+100)
   END DO
   WRITE(*,*)
!
20 FORMAT(a40)
!
   DEALLOCATE(parm,chi)
!
   RETURN
!
END SUBROUTINE readnestedparm
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readparm(nparm,indexparm,nfiles,nlength,maxparm,  &
   ioptimizer,istart)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN)                      :: indexparm(199)
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: maxparm
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: istart
!
   REAL(KIND=dp),ALLOCATABLE  :: parm(:,:),chi(:)
   REAL(KIND=dp)              :: col1
!
   INTEGER :: lengthtemp,i,j,k,k1,ilength
!
   CHARACTER (LEN=40) :: filein
   CHARACTER (LEN=80) :: suffixstring(999),labelstring(999)
!
   ALLOCATE(parm(199,9999),chi(9999))
!
   lengthtemp=nlength
   ilength=nlength
   IF(ioptimizer == 12)lengthtemp=9999
!
   OPEN(UNIT=211,FILE='ELCjunk',STATUS='old')
!
   CALL getelcparmstring(suffixstring,labelstring)
   DO i=1,nparm
!      k1=lnblnk(suffixstring(indexparm(i)))
      OPEN(UNIT=100+i,FILE='ELCjunk.'//TRIM(suffixstring(indexparm(i))),  &
         STATUS='unknown')
   END DO
!
   DO  k=1,nfiles
      READ(211,20)filein
      k1=LEN_TRIM(filein)
      IF(k < istart)CYCLE
      IF(k == nfiles)CYCLE
      WRITE(*,"('Reading parm file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF(ioptimizer == 2)THEN
         OPEN(UNIT=2,FILE='demcmc_starparm'//filein(17:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*)(parm(i,j),i=1,maxparm-2)
         END DO
         CLOSE(2)
         ilength=j-1
      END IF
      IF(ioptimizer == 10)THEN
         OPEN(UNIT=2,FILE='hammer_starparm'//filein(17:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*,END=9)(parm(i,j),i=1,maxparm-2)
         END DO
9        CLOSE(2)
         ilength=j-1
      END IF
      IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
         OPEN(UNIT=2,FILE='ELCparm'//filein(13:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*,END=19)col1,chi(j),(parm(i,j),i=1,maxparm-2)
            col1=col1*2
            chi(j)=chi(j)**2
         END DO
19       CLOSE(2)
         ilength=j-1
      END IF
      DO i=1,nparm
         DO j=1,ilength
            WRITE(100+i,*)parm(indexparm(i)-2,j),k
         END DO
      END DO
   END DO
!
   CLOSE(211)
   DO i=1,nparm
      CLOSE(i+100)
   END DO
   WRITE(*,*)
!
20 FORMAT(a40)
!
   DEALLOCATE(parm,chi)
!
   RETURN
!
END SUBROUTINE readparm
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readparmspread(ioptimizer,nlength,nfiles,istart,  &
   istop,xlow,xhigh,needindex,maxparm)
!
   USE accur
!
   IMPLICIT NONE

   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: istop
   REAL(KIND=pg), INTENT(OUT)               :: xlow
   REAL(KIND=pg), INTENT(OUT)               :: xhigh
   INTEGER, INTENT(IN)                      :: needindex
   INTEGER, INTENT(IN)                      :: maxparm
!
   REAL(KIND=pg)  :: fred
   REAL(KIND=dp), ALLOCATABLE  :: col(:,:),z1,z2
!
   INTEGER :: k1,i,j,k,loopup
!
   CHARACTER(LEN=7) :: suffixstring
   CHARACTER(LEN=80) :: filein
!
   loopup=nlength
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   ALLOCATE(col(nlength,maxparm))
!
   xlow=1.0E+33_pg
   xhigh=-1.0E+33_pg
   DO i=1,nlength
      WRITE(suffixstring,20)i+1000000
      OPEN(UNIT=10+i,FILE='ELCjunk.'//suffixstring,STATUS='unknown')
   END DO
!
   DO  k=1,istop
      READ(1,30)filein
      k1=LEN_TRIM(filein)
      WRITE(*,"('Reading parm file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      IF(k == nfiles)CYCLE
      IF(ioptimizer /= 3)THEN
         IF(ioptimizer == 2)THEN
            OPEN(UNIT=2,FILE='demcmc_starparm'//filein(17:k1),STATUS='old')
            DO j=1,nlength
               READ(2,*)(col(j,i),i=1,maxparm-2)
            END DO
            CLOSE(2)
         END IF
         IF(ioptimizer == 10)THEN
            OPEN(UNIT=2,FILE='hammer_starparm'//filein(17:k1),STATUS='old')
            DO j=1,nlength
               READ(2,*)(col(j,i),i=1,maxparm-2)
            END DO
            CLOSE(2)
         END IF
      ELSE
         OPEN(UNIT=2,FILE='ELCparm.'//filein(18:k1),STATUS='old')
         DO j=1,nlength
            READ(2,*)z1,z2,(col(j,i),i=1,maxparm-2)
            z1=2.8_dp*z1
            z2=z2-z1
         END DO
         CLOSE(2)
      END IF
      DO j=1,nlength
         WRITE(10+j,*)k,col(j,needindex-2)
         fred=REAL(col(j,needindex-2),KIND=pg)
         IF(fred > xhigh)xhigh=fred
         IF(fred < xlow)xlow=fred
      END DO
   END DO
!
   DO i=1,nlength
      CLOSE(i+10)
   END DO
   WRITE(*,*)
!
20 FORMAT(i7)
30 FORMAT(a40)
!
   DEALLOCATE(col)
!
   RETURN
!
END SUBROUTINE readparmspread
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readplottrenddata(ioptimizer,nlength,nvar,  &
   nfiles,svar,nvmax,istart,istop,nextra,extrastr)
!
!   This routine will compute the mean, median, and
!   standard deviation of each fitting parameter (and
!   optionally derived parameters) as a function of the
!   generation number.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nfiles
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: istop
   INTEGER, INTENT(IN)                      :: nextra
   CHARACTER (LEN=50), INTENT(IN)           :: extrastr(58)
!
   REAL(KIND=dp) :: parm(nfiles,nlength,nvmax),col1,col2
   REAL(KIND=dp) :: array(nlength),ave,summ,rmedian,sig
   REAL(KIND=dp), ALLOCATABLE :: extra(:,:,:)
!
   INTEGER :: mlength,kcount,i,j,k
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring,labelstring
!
   DO i=1,nlength
      array(i)=0.0_dp
   END DO
!
   OPEN(UNIT=211,FILE='ELCjunk',STATUS='old')
!
   DO i=1,nvar
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      OPEN(UNIT=10+i,FILE='ELCjunk.'//TRIM(suffixstring),STATUS='unknown')
   END DO
!
   DO i=1,nextra
      OPEN(UNIT=10+i+nvar,FILE='ELCjunk.'//TRIM(extrastr(i)),STATUS='unknown')
   END DO
!
   ALLOCATE(extra(nfiles,nlength,nvmax))
!
   kcount=0
   DO  k=1,istop     !Nfiles
      READ(211,30)filein
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      WRITE(*,"('Reading fit file ',I0,A)",ADVANCE='no')k,CHAR(13)
      kcount=kcount+1
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      DO j=1,nlength
         IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
            IF(nextra == 0)THEN
               READ(2,*,END=10)col1,col2,(parm(kcount,j,i),i=1,nvar)
            ELSE
               READ(2,*,END=10)col1,col2,(parm(kcount,j,i),i=1,nvar),  &
                  (extra(kcount,j,i),i=1,nextra)
            END IF
            col1=col1*2.0_dp
            col2=col1*col2
         ELSE
            IF(nextra == 0)THEN
               READ(2,*,END=10)(parm(kcount,j,i),i=1,nvar)
            ELSE
               READ(2,*,END=10)(parm(kcount,j,i),i=1,nvar),  &
                  (extra(kcount,j,i),i=1,nextra)
            END IF
         END IF
      END DO
10    CLOSE(2)
      mlength=j-1
   END DO
!
30 FORMAT(a40)
!
   CLOSE(211)
!
   WRITE(*,*)
   DO i=1,kcount
      WRITE(*,"('Computing generation ',I0,A)",ADVANCE='no')i+istart-1,CHAR(13)
      DO j=1,nvar
         summ=0.0_dp
         DO k=1,mlength
            array(k)=parm(i,k,j)
            summ=summ+array(k)
         END DO
         ave=summ/REAL(mlength,KIND=dp)
!
         summ=0.0_dp
         DO k=1,mlength
            summ=summ+(ave-array(k))**2
         END DO
         sig=summ/REAL(mlength-1,KIND=dp)
         sig=SQRT(sig)
         CALL sort(mlength,array)
         rmedian=array(mlength/2)
         WRITE(10+j,40)i+istart-1,ave,rmedian,sig
      END DO
!
      IF(nextra > 0)THEN
         DO j=1,nextra
            summ=0.0_dp
            DO k=1,mlength
               array(k)=extra(i,k,j)
               summ=summ+array(k)
            END DO
            ave=summ/REAL(mlength,KIND=dp)
!
            summ=0.0_dp
            DO k=1,mlength
               summ=summ+(ave-array(k))**2
            END DO
            sig=summ/REAL(mlength-1,KIND=dp)
            sig=SQRT(sig)
            CALL sort(mlength,array)
            rmedian=array(mlength/2)
            WRITE(10+j+nvar,40)i+istart-1,ave,rmedian,sig
         END DO
      END IF
!
   END DO
!
   DO i=1,nvar
      CLOSE(i+10)
   END DO
   DO i=1,nextra
      CLOSE(i+10+nvar)
   END DO
!
   DEALLOCATE(extra)
!
   RETURN
!
40 FORMAT(i6,1X,2(f20.12,2X),2X,f20.10)
!
END SUBROUTINE readplottrenddata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readplottrendparm(ioptimizer,nlength,  &
   nfiles,istart,istop,maxparm,nparm,indexparm)
!
!   This routine will compute the mean, median, and
!   standard deviation of each fitting parameter (and
!   optionally derived parameters) as a function of the
!   generation number.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: istop
   INTEGER, INTENT(IN)                      :: maxparm
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN)                      :: indexparm(199)
!
   REAL(KIND=dp),ALLOCATABLE :: parm(:,:,:)
   REAL(KIND=dp) :: array(nlength),ave,summ,rmedian,sig,col1,col2
!
   INTEGER :: ilength,kcount,i,j,k,k1
!
   CHARACTER (LEN=40) :: filein
   CHARACTER (LEN=80) :: suffixstring(999),labelstring(999)
!
   DO i=1,nlength
      array(i)=0.0_dp
   END DO
!
   OPEN(UNIT=211,FILE='ELCjunk',STATUS='old')
!
   CALL getelcparmstring(suffixstring,labelstring)
   DO i=1,nparm
!      k1=lnblnk(suffixstring(indexparm(i)))
      OPEN(UNIT=10+i,FILE='ELCjunk.'//TRIM(suffixstring(indexparm(i))),  &
         STATUS='unknown')
   END DO
!
   ALLOCATE(parm(nfiles,nlength,maxparm))
!
   kcount=0
   DO  k=1,istop     
      READ(211,30)filein
      k1=LEN_TRIM(filein)
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      WRITE(*,"('Reading parm file ',I0,A)",ADVANCE='no')k,CHAR(13)
      kcount=kcount+1
      IF(ioptimizer == 2)THEN
         OPEN(UNIT=2,FILE='demcmc_starparm'//filein(17:k1),STATUS='old')
         DO j=1,nlength
            READ(2,*,END=10)(parm(kcount,j,i),i=1,maxparm-2)
         END DO
10       CLOSE(2)
         ilength=j-1
      END IF
!
      IF(ioptimizer == 10)THEN
         OPEN(UNIT=2,FILE='hammer_starparm'//filein(17:k1),STATUS='old')
         DO j=1,nlength
            READ(2,*,END=11)(parm(kcount,j,i),i=1,maxparm-2)
         END DO
11       CLOSE(2)
         ilength=j-1
      END IF
!
      IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
         OPEN(UNIT=2,FILE='ELCparm'//filein(13:k1),STATUS='old')
         DO j=1,9999
            READ(2,*,END=12)col1,col2,(parm(kcount,j,i),i=1,maxparm-2)
            col1=col1*2.0_dp
            col2=col2*2.0_dp
         END DO
12       CLOSE(2)
         ilength=j-1
      END IF
!
   END DO
!
30 FORMAT(a40)
!
   CLOSE(211)
!
   WRITE(*,*)
   DO i=1,kcount
      WRITE(*,"('Computing generation ',I0,A)",ADVANCE='no')i+istart-1,CHAR(13)
      DO j=1,nparm
         summ=0.0_dp
         DO k=1,ilength
            array(k)=parm(i,k,indexparm(j)-2)
            summ=summ+array(k)
         END DO
         ave=summ/REAL(ilength,KIND=dp)
!
         summ=0.0_dp
         DO k=1,ilength
            summ=summ+(ave-array(k))**2
         END DO
         sig=summ/REAL(ilength-1,KIND=dp)
         sig=SQRT(sig)
         CALL sort(ilength,array)
         rmedian=array(ilength/2)
         WRITE(10+j,40)i+istart-1,ave,rmedian,sig
      END DO
!
   END DO
!
   DO i=1,nparm
      CLOSE(i+10)
   END DO
!
   DEALLOCATE(parm)
!
   RETURN
!
40 FORMAT(i6,1X,2(f20.12,2X),2X,f20.10)
!
END SUBROUTINE readplottrendparm
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readratio(nparm,indexparm,nfiles,nlength,maxparm,  &
   ioptimizer,istart)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nparm
   INTEGER, INTENT(IN)                      :: indexparm(199)
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: maxparm
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: istart
!
   REAL(KIND=dp) :: parm(199,9999),chi(9999),col1
!
   INTEGER :: lengthtemp,i,j,k,k1,ilength
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring(999),labelstring(999)
!
   lengthtemp=nlength
   IF(ioptimizer == 12)lengthtemp=9999
   OPEN(UNIT=211,FILE='ELCjunk',STATUS='old')
!
   CALL getelcratiostring(suffixstring,labelstring)
   DO i=1,nparm
      OPEN(UNIT=100+i,FILE='ELCjunk.'//TRIM(suffixstring(indexparm(i))),  &
         STATUS='unknown')
   END DO
!
   DO  k=1,nfiles
      READ(211,20)filein
      k1=LEN_TRIM(filein)
      IF(k < istart)CYCLE
      WRITE(*,"('Reading ratio file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF(k == nfiles)CYCLE
      IF(k < istart)CYCLE
      IF(ioptimizer == 2)THEN
         OPEN(UNIT=2,FILE='demcmc_ratio'//filein(17:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*)(parm(i,j),i=1,maxparm)
         END DO
         CLOSE(2)
      END IF
      IF(ioptimizer == 10)THEN
         OPEN(UNIT=2,FILE='hammer_ratio'//filein(17:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*,END=9)(parm(i,j),i=1,maxparm)
         END DO
9        CLOSE(2)
         ilength=j-1
      END IF
      IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
         OPEN(UNIT=2,FILE='ELCratio'//filein(13:k1),STATUS='old')
         DO j=1,lengthtemp
            READ(2,*,END=19)col1,chi(j),(parm(i,j),i=1,maxparm)
            col1=col1*2
            chi(j)=chi(j)**2
         END DO
19       CLOSE(2)
         ilength=j-1
      END IF
      DO i=1,nparm
         DO j=1,ilength 
            WRITE(100+i,*)parm(indexparm(i),j),k
         END DO
      END DO
   END DO
!
   CLOSE(211)
   DO i=1,nparm
      CLOSE(i+100)
   END DO
   WRITE(*,*)
!
20 FORMAT(a40)
!
   RETURN
!
END SUBROUTINE readratio
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readrvdata(icnrv1arry,icnrv2arry,icnrv3arry,  &
   icnrv4arry,icnrv5arry,ndatamax)
!
!   If there are multiple observatories for the RV data,
!   they can be given in the optional ELCRVdata.opt.  This
!   subroutine will read that file and
!   return flags in the icnRV?arry variables
! 
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: icnrv1arry(5)
   INTEGER, INTENT(OUT)                     :: icnrv2arry(5)
   INTEGER, INTENT(OUT)                     :: icnrv3arry(5)
   INTEGER, INTENT(OUT)                     :: icnrv4arry(5)
   INTEGER, INTENT(OUT)                     :: icnrv5arry(5)
   INTEGER, INTENT(IN)                      :: ndatamax
!
   REAL(KIND=dp), ALLOCATABLE  :: xrv1arry(:,:),yrv1arry(:,:),errrv1arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: xrv2arry(:,:),yrv2arry(:,:),errrv2arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: xrv3arry(:,:),yrv3arry(:,:),errrv3arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: xrv4arry(:,:),yrv4arry(:,:),errrv4arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: xrv5arry(:,:),yrv5arry(:,:),errrv5arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: savxrv1arry(:,:),savyrv1arry(:,:),saverrrv1arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: savxrv2arry(:,:),savyrv2arry(:,:),saverrrv2arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: savxrv3arry(:,:),savyrv3arry(:,:),saverrrv3arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: savxrv4arry(:,:),savyrv4arry(:,:),saverrrv4arry(:,:)
   REAL(KIND=dp), ALLOCATABLE  :: savxrv5arry(:,:),savyrv5arry(:,:),saverrrv5arry(:,:)
!
   INTEGER :: nrv1arry,nrv2arry,icnvrt,nrv3arry,nrv4arry
   INTEGER :: nrv5arry,nset,i,ios,j
!
   DIMENSION nrv1arry(5),nrv2arry(5),nrv3arry(5),nrv4arry(5)
   DIMENSION nrv5arry(5)
!
   CHARACTER (LEN=1) :: bell
   CHARACTER (LEN=50) :: file1,file2,file3,file4,file5
!
   DO i=1,5
      nrv1arry(i)=0
      nrv2arry(i)=0
      nrv3arry(i)=0
      nrv4arry(i)=0
      nrv5arry(i)=0
      icnrv1arry(i)=430
      icnrv2arry(i)=430
      icnrv3arry(i)=430
      icnrv4arry(i)=430
      icnrv5arry(i)=430
   END DO
!
   bell=CHAR(7)
!
!   Attempt to open ELCRVdata.opt
!
   ios=0
   OPEN(UNIT=20,FILE='ELCRVdata.opt',STATUS='old',IOSTAT=ios)
   IF(ios /= 0)THEN
      WRITE(*,70)bell
      STOP
   END IF
!
!   Attempt to read the first line, which should be an integer
!   between 1 and 5
!
   ios=0
   READ(20,*,IOSTAT=ios,ERR=120)nset
!
   IF((nset <= 0).OR.(nset > 5))THEN
      WRITE(*,90)bell
      STOP
   END IF
!
   ALLOCATE(xrv1arry(ndatamax,5),xrv2arry(ndatamax,5))
   ALLOCATE(xrv3arry(ndatamax,5),xrv4arry(ndatamax,5))
   ALLOCATE(xrv5arry(ndatamax,5),yrv5arry(ndatamax,5))
   ALLOCATE(yrv1arry(ndatamax,5),yrv2arry(ndatamax,5))
   ALLOCATE(yrv3arry(ndatamax,5),yrv4arry(ndatamax,5))
   ALLOCATE(errrv1arry(ndatamax,5),errrv2arry(ndatamax,5))
   ALLOCATE(errrv3arry(ndatamax,5),errrv4arry(ndatamax,5))
   ALLOCATE(errrv5arry(ndatamax,5),saverrrv2arry(ndatamax,5))
   ALLOCATE(savxrv1arry(ndatamax,5),savxrv2arry(ndatamax,5))
   ALLOCATE(savxrv3arry(ndatamax,5),savxrv4arry(ndatamax,5))
   ALLOCATE(savxrv5arry(ndatamax,5),savyrv5arry(ndatamax,5))
   ALLOCATE(savyrv1arry(ndatamax,5),savyrv2arry(ndatamax,5))
   ALLOCATE(savyrv3arry(ndatamax,5),savyrv4arry(ndatamax,5))
   ALLOCATE(saverrrv1arry(ndatamax,5))
   ALLOCATE(saverrrv3arry(ndatamax,5))
   ALLOCATE(saverrrv5arry(ndatamax,5))
   ALLOCATE(saverrrv4arry(ndatamax,5))
!
!   Loop over Nset, read in files, load data
!
   DO i=1,nset
      ios=0
      READ(20,60,ERR=130,IOSTAT=ios)file1
      READ(20,60,ERR=130,IOSTAT=ios)file2
      READ(20,60,ERR=130,IOSTAT=ios)file3
      READ(20,60,ERR=130,IOSTAT=ios)file4
      READ(20,60,ERR=130,IOSTAT=ios)file5
!
      icnrv1arry(i)=icnvrt(file1)
      IF(icnrv1arry(i) /= 430)THEN
         OPEN(UNIT=72,FILE=file1,IOSTAT=ios,ERR=140)
         DO j=1,ndatamax
            READ(72,*,END=10)xrv1arry(j,i),yrv1arry(j,i),errrv1arry(j,i)
            savxrv1arry(j,i)=xrv1arry(j,i)
            savyrv1arry(j,i)=yrv1arry(j,i)
            saverrrv1arry(j,i)=errrv1arry(j,i)
         END DO
10       CLOSE(72)
         nrv1arry(i)=j-1
      END IF
!
      icnrv2arry(i)=icnvrt(file2)
      IF(icnrv2arry(i) /= 430)THEN
         OPEN(UNIT=72,FILE=file2,IOSTAT=ios,ERR=140)
         DO j=1,ndatamax
            READ(72,*,END=20)xrv2arry(j,i),yrv2arry(j,i),errrv2arry(j,i)
            savxrv2arry(j,i)=xrv2arry(j,i)
            savyrv2arry(j,i)=yrv2arry(j,i)
            saverrrv2arry(j,i)=errrv2arry(j,i)
         END DO
20       CLOSE(72)
         nrv2arry(i)=j-1
      END IF
!
      icnrv3arry(i)=icnvrt(file3)
      IF(icnrv3arry(i) /= 430)THEN
         OPEN(UNIT=72,FILE=file3,IOSTAT=ios,ERR=140)
         DO j=1,ndatamax
            READ(72,*,END=30)xrv3arry(j,i),yrv3arry(j,i),errrv3arry(j,i)
            savxrv3arry(j,i)=xrv3arry(j,i)
            savyrv3arry(j,i)=yrv3arry(j,i)
            saverrrv3arry(j,i)=errrv3arry(j,i)
         END DO
30       CLOSE(72)
         nrv3arry(i)=j-1
      END IF
!
      icnrv4arry(i)=icnvrt(file4)
      IF(icnrv4arry(i) /= 430)THEN
         OPEN(UNIT=72,FILE=file4,IOSTAT=ios,ERR=140)
         DO j=1,ndatamax
            READ(72,*,END=40)xrv4arry(j,i),yrv4arry(j,i),errrv4arry(j,i)
            savxrv4arry(j,i)=xrv4arry(j,i)
            savyrv4arry(j,i)=yrv4arry(j,i)
            saverrrv4arry(j,i)=errrv4arry(j,i)
         END DO
40       CLOSE(72)
         nrv4arry(i)=j-1
      END IF
!
      icnrv5arry(i)=icnvrt(file5)
      IF(icnrv5arry(i) /= 430)THEN
         OPEN(UNIT=72,FILE=file4,IOSTAT=ios,ERR=140)
         DO j=1,ndatamax
            READ(72,*,END=50)xrv5arry(j,i),yrv5arry(j,i),errrv5arry(j,i)
            savxrv5arry(j,i)=xrv5arry(j,i)
            savyrv5arry(j,i)=yrv5arry(j,i)
            saverrrv5arry(j,i)=errrv5arry(j,i)
         END DO
50       CLOSE(72)
         nrv5arry(i)=j-1
      END IF
!
   END DO
!
   CLOSE(20)
!
60 FORMAT(a50)
70 FORMAT(a1,'Error:  ELCRVdata.opt not found')
80 FORMAT(a1,'Error:  First line of ELCRVdata.opt is ','not an i'  &
      ,'nteger')
90 FORMAT(a1,'Error:  Nset should be between 1 and 5')
100 FORMAT(a1,'Error:  Error reading ELCRVdata')
110 FORMAT(a1,'Error reading file ',a50)
!
120 IF(ios /= 0)THEN
      WRITE(*,80)bell
      STOP
   END IF
!
130 IF(ios /= 0)THEN
      WRITE(*,100)bell
      STOP
   END IF
!
140 IF(ios /= 0)THEN
      WRITE(*,110)bell
      STOP
   END IF
!
   DEALLOCATE(xrv1arry,xrv2arry)
   DEALLOCATE(xrv3arry,xrv4arry)
   DEALLOCATE(xrv5arry,yrv5arry)
   DEALLOCATE(yrv1arry,yrv2arry)
   DEALLOCATE(yrv3arry,yrv4arry)
   DEALLOCATE(errrv1arry,errrv2arry)
   DEALLOCATE(errrv3arry,errrv4arry)
   DEALLOCATE(errrv5arry,saverrrv2arry)
   DEALLOCATE(savxrv1arry,savxrv2arry)
   DEALLOCATE(savxrv3arry,savxrv4arry)
   DEALLOCATE(savxrv5arry,savyrv5arry)
   DEALLOCATE(savyrv1arry,savyrv2arry)
   DEALLOCATE(savyrv3arry,savyrv4arry)
   DEALLOCATE(saverrrv1arry)
   DEALLOCATE(saverrrv3arry)
   DEALLOCATE(saverrrv5arry)
   DEALLOCATE(saverrrv4arry)
!
   RETURN
!
END SUBROUTINE readrvdata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readskipdata(ioptimizer,nlength,nvar,nfiles,svar,  &
   nvmax,istart,maxlag)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nvmax
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nvar
   INTEGER, INTENT(IN)                      :: nfiles
   CHARACTER (LEN=40), INTENT(IN)           :: svar(nvmax)
   INTEGER, INTENT(IN)                      :: istart
   INTEGER, INTENT(IN)                      :: maxlag
!
   INTEGER :: iskip,mlength,kcount,i,j,k,k1,icount,jcount
   INTEGER :: pgerr,pgbegin
!
   REAL(KIND=dp) :: parm(nfiles,nlength,nvmax),col1,col2
   REAL(KIND=dp) :: summ,ydata(nfiles*nlength)
   REAL(KIND=dp) :: xacf(maxlag),yacf(maxlag)
!
   REAL(KIND=pg) :: xplot1(maxlag),yplot1(maxlag)
   REAL(KIND=pg) :: xplot2(maxlag),yplot2(maxlag)
!
   CHARACTER(LEN=40) :: filein
   CHARACTER(LEN=80) :: suffixstring,labelstring
!
   pgerr=pgbegin(0,'/cps',1,2)
   CALL pgsch(1.05_pg)
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   kcount=0
   DO  k=1,nfiles
      READ(1,30)filein
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      WRITE(*,"('Reading fit file ',I0,A)",ADVANCE='no')k,CHAR(13)
      IF(k < istart)CYCLE
      kcount=kcount+1
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      DO j=1,nlength
         IF((ioptimizer == 3).OR.(ioptimizer == 12))THEN
            READ(2,*,END=10)col1,col2,(parm(kcount,j,i),i=1,nvar)
            col1=col1*2.0_dp
            col2=col1*col2
         ELSE
            READ(2,*,END=10)(parm(kcount,j,i),i=1,nvar)
         END IF
      END DO
10    CLOSE(2)
      mlength=j-1
   END DO
!
30 FORMAT(a40)
!
   CLOSE(1)
!
   WRITE(*,*)
   pgerr=pgbegin(0,'/cps',1,1)
   CALL pgslw(2)
   CALL pgsch(1.05_pg)
!
   DO i=1,nvar
      CALL getsuffix(svar(i)(1:2),suffixstring,labelstring)
      k1=LEN_TRIM(suffixstring)
!
      OPEN(UNIT=20,FILE='ELCjunk.'//suffixstring(1:k1),STATUS= 'unknown')
!
      WRITE(*,"('Computing parameter ',I0,1x,'(',A,')',A,15x,A)",ADVANCE= &
         'no')i,suffixstring(1:k1),' ',CHAR(13)
!
      DO iskip=2,maxlag
         icount=0
         DO k=1,kcount!,iskip
            IF(MOD(k,iskip) == 1)THEN
               DO j=1,nlength
                  icount=icount+1
                  ydata(icount)=parm(k,j,i)
               END DO
            END IF
         END DO
         CALL getacf(icount,maxlag,ydata,xacf,yacf)
         summ=0.0_dp
         jcount=0
         DO j=2,maxlag
            IF(MOD(INT(xacf(j)),nlength) /= 0)THEN
               jcount=jcount+1
               summ=summ+yacf(j)**2
            END IF
         END DO
!
         WRITE(20,*)REAL(iskip,KIND=dp),SQRT(summ/REAL(jcount,KIND=dp)),yacf(nlength+1)
!
         xplot1(iskip-1)=REAL(iskip,KIND=pg)
         xplot2(iskip-1)=REAL(iskip,KIND=pg)
         yplot1(iskip-1)=REAL(SQRT(summ/REAL(jcount,KIND=dp)),KIND=pg)
         IF(yplot1(iskip-1) < 0.00001_pg)yplot1(iskip-1)=0.00001_pg
         yplot2(iskip-1)=REAL(yacf(nlength+1),KIND=pg)
         IF(yplot2(iskip-1) < 0.00001_pg)yplot2(iskip-1)=0.00001_pg
         yplot1(iskip-1)=LOG10(yplot1(iskip-1))
         yplot2(iskip-1)=LOG10(yplot2(iskip-1))
      END DO
!
      CLOSE(20)
!
      CALL pgenv(0.0_pg,REAL(maxlag,KIND=pg),-3.0_pg,0.0_pg,0,20)
      CALL pglabel('N\dskip\u','ACF',suffixstring(1:k1))
      CALL pgline(maxlag-1,xplot1,yplot1)
      CALL pgsci(2)
      CALL pgline(maxlag-1,xplot2,yplot2)
      CALL pgsci(1)
   END DO
!
   WRITE(*,*)
   CALL pgend
!
   RETURN
!
END SUBROUTINE readskipdata
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE readspread(ioptimizer,nlength,nfiles,istart,xlow,  &
   xhigh,istop)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ioptimizer
   INTEGER, INTENT(IN)                      :: nlength
   INTEGER, INTENT(IN)                      :: nfiles
   INTEGER, INTENT(IN)                      :: istart
   REAL(KIND=pg), INTENT(OUT)               :: xlow
   REAL(KIND=pg), INTENT(OUT)               :: xhigh
   INTEGER, INTENT(IN)                      :: istop
!
   REAL(KIND=dp) :: parm(99999)
!
   INTEGER :: i,j,k,loopup
!
   CHARACTER (LEN=7) :: suffixstring
   CHARACTER (LEN=80) :: filein
!
   loopup=nlength
!          if(ioptimizer.eq.12)loopup=99999
!
   OPEN(UNIT=1,FILE='ELCjunk',STATUS='old')
!
   xlow=1.0E+33_pg
   xhigh=-1.0E+33_pg
   DO i=1,nlength
      WRITE(suffixstring,5)i+1000000
      OPEN(UNIT=10+i,FILE='ELCjunk.'//suffixstring,STATUS= 'unknown')
   END DO
!
5  FORMAT(i7)
   DO  k=1,istop
      READ(1,20)filein
      IF((k == nfiles).AND.(ioptimizer == 3))CYCLE
      IF(k < istart)CYCLE
      WRITE(*,"('Reading chi file ',I0,A)",ADVANCE='no')k,CHAR(13)
      OPEN(UNIT=2,FILE=filein,STATUS='old')
      DO j=1,nlength
         READ(2,*,END=9)parm(j)
         IF(REAL(parm(j),KIND=pg) < xlow)xlow=REAL(parm(j),KIND=pg)
         IF(REAL(parm(j),KIND=pg) > xhigh)xhigh=REAL(parm(j),KIND=pg)
      END DO
9     CLOSE(2)
      DO j=1,nlength
         WRITE(10+j,*)k,parm(j)
      END DO
   END DO
!
   DO i=1,nlength
      CLOSE(i+10)
   END DO
   WRITE(*,*)
!
20 FORMAT(a40)
!
   RETURN
!
END SUBROUTINE readspread
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE sort(n,ra)
!
!   UPDATE September 10, 2001
!
!   This is a new subroutine, similar to sort3.
!
!   Taken from Numerical Recipes.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=dp), INTENT(IN OUT)            :: ra(n)
!
   REAL(KIND=dp)  :: rra
!
   INTEGER :: l,ir,i,j
!
   l=n/2+1
   ir=n
10 IF(l > 1)THEN
      l=l-1
      rra=ra(l)
   ELSE
      rra=ra(ir)
      ra(ir)=ra(1)
      ir=ir-1
      IF(ir == 1)THEN
         ra(1)=rra
         RETURN
      END IF
   END IF
   i=l
   j=l+l
20 IF(j <= ir)THEN
      IF(j < ir)THEN
         IF(ra(j) < ra(j+1))j=j+1
      END IF
      IF(rra < ra(j))THEN
         ra(i)=ra(j)
         i=j
         j=j+j
      ELSE
         j=ir+1
      END IF
      GO TO 20
   END IF
   ra(i)=rra
   GO TO 10
!
END SUBROUTINE sort
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE sort2(n,ra,rb)
!
!   UPDATE September 10, 2001
!
!   This is a new subroutine, similar to sort3.
!
!   Taken from Numerical Recipes.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=dp), INTENT(IN OUT)            :: ra(n)
   REAL(KIND=dp), INTENT(IN OUT)            :: rb(n)
!
   REAL(KIND=dp) ::  rra,rrb
!
   INTEGER :: l,ir,i,j
!
   l=n/2+1
   ir=n
10 IF(l > 1)THEN
      l=l-1
      rra=ra(l)
      rrb=rb(l)
   ELSE
      rra=ra(ir)
      rrb=rb(ir)
      ra(ir)=ra(1)
      rb(ir)=rb(1)
      ir=ir-1
      IF(ir == 1)THEN
         ra(1)=rra
         rb(1)=rrb
         RETURN
      END IF
   END IF
   i=l
   j=l+l
20 IF(j <= ir)THEN
      IF(j < ir)THEN
         IF(ra(j) < ra(j+1))j=j+1
      END IF
      IF(rra < ra(j))THEN
         ra(i)=ra(j)
         rb(i)=rb(j)
         i=j
         j=j+j
      ELSE
         j=ir+1
      END IF
      GO TO 20
   END IF
   ra(i)=rra
   rb(i)=rrb
   GO TO 10
!
END SUBROUTINE sort2
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE spline(x,y,n,yp1,ypn,y2)
!
!   November 12, 1999
!
!   This is a spline interpolation routine taken from NUMERICAL RECIPES.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=dp), INTENT(IN)                :: x(n)
   REAL(KIND=dp), INTENT(IN)                :: y(n)
   REAL(KIND=dp), INTENT(IN)                :: yp1
   REAL(KIND=dp), INTENT(IN)                :: ypn
   REAL(KIND=dp), INTENT(OUT)               :: y2(n)
!
   REAL(KIND=dp), ALLOCATABLE  :: u(:)
   REAL(KIND=dp)               ::  p,qn,sig,un   
!
   INTEGER :: i,k
!
   ALLOCATE(u(n+2))
!
   IF(yp1 > 0.99E+30_dp)THEN
      y2(1)=0.0_dp
      u(1)=0.0_dp
   ELSE
      y2(1)=-0.5_dp
      u(1)=(3.0_dp/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
   END IF
   DO  i=2,n-1
      sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
      p=sig*y2(i-1)+2.0_dp
      y2(i)=(sig-1.0_dp)/p
      u(i)=(6.0_dp*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-  &
         x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
      IF(ABS(u(i)) < 1.0E-250_dp)u(i)=0.0_dp
   END DO
   IF(ypn > 0.99E+30_dp)THEN
      qn=0.0_dp
      un=0.0_dp
   ELSE
      qn=0.5_dp
      un=(3.0_dp/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
   END IF
   y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.0_dp)
   DO  k=n-1,1,-1
      y2(k)=y2(k)*y2(k+1)+u(k)
      IF(ABS(y2(k)) < 1.0E-250_dp)y2(k)=0.0_dp
   END DO
   DEALLOCATE(u)
   RETURN
!
END SUBROUTINE spline
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE splint(xa,ya,y2a,n,x,y)
!
!   November 12, 1999
!
!   This is a spline interpolation routine taken from Numerical Recipes.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=dp), INTENT(IN)                :: xa(n)
   REAL(KIND=dp), INTENT(IN)                :: ya(n)
   REAL(KIND=dp), INTENT(IN)                :: y2a(n)
   REAL(KIND=dp), INTENT(IN)                :: x
   REAL(KIND=dp), INTENT(OUT)               :: y
!
   INTEGER :: k,khi,klo
!
   REAL(KIND=dp)  :: a,b,h
!
   klo=1
   khi=n
10 IF(khi-klo > 1)THEN
      k=(khi+klo)/2
      IF(xa(k) > x)THEN
         khi=k
      ELSE
         klo=k
      END IF
      GO TO 10
   END IF
   h=xa(khi)-xa(klo)
   IF(ABS(h)  <= EPSILON(h))THEN
      WRITE(*,*) 'bad xa input in splint'
      STOP
   END IF
   a=(xa(khi)-x)/h
   b=(x-xa(klo))/h
   y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.0_dp
   RETURN
!
END SUBROUTINE splint
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE trendstart(istart,istop,nskip)
!
!   Get the starting file number from the command line
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                  :: istart
   INTEGER, INTENT(OUT)                  :: istop
   INTEGER, INTENT(OUT)                  :: nskip
!
   INTEGER :: ios
!
   CHARACTER (LEN=40) :: arg1,arg2,arg3
!
   ios=0
   CALL get_command_argument(1,arg1)
   READ(arg1,*,IOSTAT=ios)istart
!
   CALL get_command_argument(2,arg2)
   READ(arg2,*,IOSTAT=ios)istop
!
   CALL get_command_argument(3,arg3)
   READ(arg3,*,IOSTAT=ios)nskip
   IF(ios == 0)RETURN
!
END SUBROUTINE trendstart
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE writebody3input(nalph3,nbet3,tertperiod,tertt0,  &
   tertecos,tertesin,tertincl,tertomega,tertq,dwavex,dwavey,  &
   itconj,it1,it2,it3,it4,tertconj,tertratrad,hh,sw72,sw73,  &
   p2tconj,p2period,p2t0,p2ecos,p2esin,p2incl,p2omega,p2q,  &
   p2ratrad,p3tconj,p3period,p3t0,p3ecos,p3esin,p3incl,p3omega,  &
   p3q,p3ratrad,p4tconj,p4period,p4t0,p4ecos,p4esin,p4incl,  &
   p4omega,p4q,p4ratrad,p5tconj,p5period,p5t0,p5ecos,p5esin,  &
   p5incl,p5omega,p5q,p5ratrad,p6tconj,p6period,p6t0,p6ecos,  &
   p6esin,p6incl,p6omega,p6q,p6ratrad,p7tconj,p7period,p7t0,  &
   p7ecos,p7esin,p7incl,p7omega,p7q,p7ratrad,p8tconj,p8period,  &
   p8t0,p8ecos,p8esin,p8incl,p8omega,p8q,p8ratrad)
!
!    will write the correctly formatted file ELCbody3.inp and return
!    default parameters
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nalph3
   INTEGER, INTENT(OUT)                     :: nbet3
   REAL(KIND=dp), INTENT(OUT)               :: tertperiod
   REAL(KIND=dp), INTENT(OUT)               :: tertt0
   REAL(KIND=dp), INTENT(OUT)               :: tertecos
   REAL(KIND=dp), INTENT(OUT)               :: tertesin
   REAL(KIND=dp), INTENT(OUT)               :: tertincl
   REAL(KIND=dp), INTENT(OUT)               :: tertomega
   REAL(KIND=dp), INTENT(OUT)               :: tertq
   REAL(KIND=dp), INTENT(OUT)               :: dwavex(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: dwavey(8,10)
   INTEGER, INTENT(OUT)                     :: itconj
   INTEGER, INTENT(OUT)                     :: it1
   INTEGER, INTENT(OUT)                     :: it2
   INTEGER, INTENT(OUT)                     :: it3
   INTEGER, INTENT(OUT)                     :: it4
   REAL(KIND=dp), INTENT(OUT)               :: tertconj
   REAL(KIND=dp), INTENT(OUT)               :: tertratrad
   REAL(KIND=dp), INTENT(OUT)               :: hh
   REAL(KIND=dp), INTENT(OUT)               :: sw72
   REAL(KIND=dp), INTENT(OUT)               :: sw73
   REAL(KIND=dp), INTENT(OUT)               :: p2tconj
   REAL(KIND=dp), INTENT(OUT)               :: p2period
   REAL(KIND=dp), INTENT(OUT)               :: p2t0
   REAL(KIND=dp), INTENT(OUT)               :: p2ecos
   REAL(KIND=dp), INTENT(OUT)               :: p2esin
   REAL(KIND=dp), INTENT(OUT)               :: p2incl
   REAL(KIND=dp), INTENT(OUT)               :: p2omega
   REAL(KIND=dp), INTENT(OUT)               :: p2q
   REAL(KIND=dp), INTENT(OUT)               :: p2ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p3tconj
   REAL(KIND=dp), INTENT(OUT)               :: p3period
   REAL(KIND=dp), INTENT(OUT)               :: p3t0
   REAL(KIND=dp), INTENT(OUT)               :: p3ecos
   REAL(KIND=dp), INTENT(OUT)               :: p3esin
   REAL(KIND=dp), INTENT(OUT)               :: p3incl
   REAL(KIND=dp), INTENT(OUT)               :: p3omega
   REAL(KIND=dp), INTENT(OUT)               :: p3q
   REAL(KIND=dp), INTENT(OUT)               :: p3ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p4tconj
   REAL(KIND=dp), INTENT(OUT)               :: p4period
   REAL(KIND=dp), INTENT(OUT)               :: p4t0
   REAL(KIND=dp), INTENT(OUT)               :: p4ecos
   REAL(KIND=dp), INTENT(OUT)               :: p4esin
   REAL(KIND=dp), INTENT(OUT)               :: p4incl
   REAL(KIND=dp), INTENT(OUT)               :: p4omega
   REAL(KIND=dp), INTENT(OUT)               :: p4q
   REAL(KIND=dp), INTENT(OUT)               :: p4ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p5tconj
   REAL(KIND=dp), INTENT(OUT)               :: p5period
   REAL(KIND=dp), INTENT(OUT)               :: p5t0
   REAL(KIND=dp), INTENT(OUT)               :: p5ecos
   REAL(KIND=dp), INTENT(OUT)               :: p5esin
   REAL(KIND=dp), INTENT(OUT)               :: p5incl
   REAL(KIND=dp), INTENT(OUT)               :: p5omega
   REAL(KIND=dp), INTENT(OUT)               :: p5q
   REAL(KIND=dp), INTENT(OUT)               :: p5ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p6tconj
   REAL(KIND=dp), INTENT(OUT)               :: p6period
   REAL(KIND=dp), INTENT(OUT)               :: p6t0
   REAL(KIND=dp), INTENT(OUT)               :: p6ecos
   REAL(KIND=dp), INTENT(OUT)               :: p6esin
   REAL(KIND=dp), INTENT(OUT)               :: p6incl
   REAL(KIND=dp), INTENT(OUT)               :: p6omega
   REAL(KIND=dp), INTENT(OUT)               :: p6q
   REAL(KIND=dp), INTENT(OUT)               :: p6ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p7tconj
   REAL(KIND=dp), INTENT(OUT)               :: p7period
   REAL(KIND=dp), INTENT(OUT)               :: p7t0
   REAL(KIND=dp), INTENT(OUT)               :: p7ecos
   REAL(KIND=dp), INTENT(OUT)               :: p7esin
   REAL(KIND=dp), INTENT(OUT)               :: p7incl
   REAL(KIND=dp), INTENT(OUT)               :: p7omega
   REAL(KIND=dp), INTENT(OUT)               :: p7q
   REAL(KIND=dp), INTENT(OUT)               :: p7ratrad
   REAL(KIND=dp), INTENT(OUT)               :: p8tconj
   REAL(KIND=dp), INTENT(OUT)               :: p8period
   REAL(KIND=dp), INTENT(OUT)               :: p8t0
   REAL(KIND=dp), INTENT(OUT)               :: p8ecos
   REAL(KIND=dp), INTENT(OUT)               :: p8esin
   REAL(KIND=dp), INTENT(OUT)               :: p8incl
   REAL(KIND=dp), INTENT(OUT)               :: p8omega
   REAL(KIND=dp), INTENT(OUT)               :: p8q
   REAL(KIND=dp), INTENT(OUT)               :: p8ratrad
!
   INTEGER :: i
!
   CHARACTER (LEN=1) :: bell
!
   bell=CHAR(7)
   WRITE(*,110)bell
!
   nalph3=60
   nbet3=20
   tertperiod=20.0_dp
   tertt0=1020.0_dp
   tertecos=0.10_dp
   tertesin=-0.20_dp
   tertincl=90.0_dp
   tertomega=0.0_dp
   tertq=0.5_dp
   tertconj=50.0_dp
   itconj=0
   it1=0
   it2=0
   it3=0
   it4=0
   tertratrad=0.0_dp
   hh=0.0_dp
   sw72=0.0_dp
   sw73=0.0_dp
   p2tconj=0.0_dp
   p2period=0.0_dp
   p2t0=0.0_dp
   p2ecos=0.0_dp
   p2esin=0.0_dp
   p2incl=0.0_dp
   p2omega=0.0_dp
   p2q=0.0_dp
   p2ratrad=0.0_dp
   p3tconj=0.0_dp
   p3period=0.0_dp
   p3t0=0.0_dp
   p3ecos=0.0_dp
   p3esin=0.0_dp
   p3incl=0.0_dp
   p3omega=0.0_dp
   p3q=0.0_dp
   p3ratrad=0.0_dp
   p4tconj=0.0_dp
   p4period=0.0_dp
   p4t0=0.0_dp
   p4ecos=0.0_dp
   p4esin=0.0_dp
   p4incl=0.0_dp
   p4omega=0.0_dp
   p4q=0.0_dp
   p4ratrad=0.0_dp
   p5tconj=0.0_dp
   p5period=0.0_dp
   p5t0=0.0_dp
   p5ecos=0.0_dp
   p5esin=0.0_dp
   p5incl=0.0_dp
   p5omega=0.0_dp
   p5q=0.0_dp
   p5ratrad=0.0_dp
   p6tconj=0.0_dp
   p6period=0.0_dp
   p6t0=0.0_dp
   p6ecos=0.0_dp
   p6esin=0.0_dp
   p6incl=0.0_dp
   p6omega=0.0_dp
   p6q=0.0_dp
   p6ratrad=0.0_dp
   p7tconj=0.0_dp
   p7period=0.0_dp
   p7t0=0.0_dp
   p7ecos=0.0_dp
   p7esin=0.0_dp
   p7incl=0.0_dp
   p7omega=0.0_dp
   p7q=0.0_dp
   p7ratrad=0.0_dp
   p8tconj=0.0_dp
   p8period=0.0_dp
   p8t0=0.0_dp
   p8ecos=0.0_dp
   p8esin=0.0_dp
   p8incl=0.0_dp
   p8omega=0.0_dp
   p8q=0.0_dp
   p8ratrad=0.0_dp
!
   DO i=1,8
      dwavex(i,3)=0.635_dp
      dwavey(i,3)=0.130_dp
   END DO
!
   DO i=1,8
      dwavex(i,4)=0.635_dp
      dwavey(i,4)=0.130_dp
   END DO
!
   DO i=1,8
      dwavex(i,5)=0.635_dp
      dwavey(i,5)=0.130_dp
   END DO
!
   OPEN(UNIT=1,FILE='ELCbody3.inp',STATUS='unknown')
!
   WRITE(1,120)nalph3
   WRITE(1,130)nbet3
   WRITE(1,10)itconj
   WRITE(1,20)it1
   WRITE(1,30)it2
   WRITE(1,40)it3
   WRITE(1,50)it4
   WRITE(1,60)tertconj
   WRITE(1,140)tertperiod
   WRITE(1,150)tertt0
   WRITE(1,160)tertecos
   WRITE(1,170)tertesin
   WRITE(1,180)tertincl
   WRITE(1,190)tertomega
   WRITE(1,200)tertq
   DO i=1,8
      WRITE(1,210)dwavex(i,3),dwavey(i,3)
   END DO
   WRITE(1,70)tertratrad
   WRITE(1,80)hh
   WRITE(1,90)sw72
   WRITE(1,100)sw73
   WRITE(1,220)p2tconj
   WRITE(1,230)p2period
   WRITE(1,240)p2t0
   WRITE(1,250)p2ecos
   WRITE(1,260)p2esin
   WRITE(1,270)p2incl
   WRITE(1,280)p2omega
   WRITE(1,290)p2q
   WRITE(1,300)p2ratrad
   WRITE(1,310)p3tconj
   WRITE(1,320)p3period
   WRITE(1,330)p3t0
   WRITE(1,340)p3ecos
   WRITE(1,350)p3esin
   WRITE(1,360)p3incl
   WRITE(1,370)p3omega
   WRITE(1,380)p3q
   WRITE(1,390)p3ratrad
   WRITE(1,400)p4tconj
   WRITE(1,410)p4period
   WRITE(1,420)p4t0
   WRITE(1,430)p4ecos
   WRITE(1,440)p4esin
   WRITE(1,450)p4incl
   WRITE(1,460)p4omega
   WRITE(1,470)p4q
   WRITE(1,480)p4ratrad
   WRITE(1,490)p5tconj
   WRITE(1,500)p5period
   WRITE(1,510)p5t0
   WRITE(1,520)p5ecos
   WRITE(1,530)p5esin
   WRITE(1,540)p5incl
   WRITE(1,550)p5omega
   WRITE(1,560)p5q
   WRITE(1,570)p5ratrad
   WRITE(1,580)p6tconj
   WRITE(1,590)p6period
   WRITE(1,600)p6t0
   WRITE(1,610)p6ecos
   WRITE(1,620)p6esin
   WRITE(1,630)p6incl
   WRITE(1,640)p6omega
   WRITE(1,650)p6q
   WRITE(1,660)p6ratrad
   WRITE(1,670)p7tconj
   WRITE(1,680)p7period
   WRITE(1,690)p7t0
   WRITE(1,700)p7ecos
   WRITE(1,710)p7esin
   WRITE(1,720)p7incl
   WRITE(1,730)p7omega
   WRITE(1,740)p7q
   WRITE(1,750)p7ratrad
   WRITE(1,760)p8tconj
   WRITE(1,770)p8period
   WRITE(1,780)p8t0
   WRITE(1,790)p8ecos
   WRITE(1,800)p8esin
   WRITE(1,810)p8incl
   WRITE(1,820)p8omega
   WRITE(1,830)p8q
   WRITE(1,840)p8ratrad
!
   DO i=1,8
      WRITE(1,210)dwavex(i,4),dwavey(i,4)
   END DO
!
   DO i=1,8
      WRITE(1,210)dwavex(i,5),dwavey(i,5)
   END DO
!
   CLOSE(1)
!
10 FORMAT(i1,19X,'itconj (0=T_peri, 1=T_tran, 2=T_occul)')
20 FORMAT(i1,19X,'set to 1 for logarithmic mass ratios')
30 FORMAT(i1,19X,'set to 1 for informational output file')
40 FORMAT(i1,19X,'set to 1 to treat transits and ','occultations'  &
      ,' together')
50 FORMAT(i1,19X,'set to 1 to suppress demcmcELC output files')
60 FORMAT(f16.8,4X,'tertconj             tag tj')
70 FORMAT(f4.2,16X,'tertratrad (P1 radius to star 1 radius,''TAG'  &
      ,' tb')
80 FORMAT(f16.8,4X,'h (step size for dynamical integration)')
90 FORMAT(f10.8,10X,'rk1 (apsidal constant star 1  tag a1)')
100 FORMAT(f10.8,10X,'rk2 (apsidal constant star 2  tag a2)')
110 FORMAT(a1,'I can''T FIND THE FILE ''ELCBODY3.INP''!',/'I''M M'  &
      ,'aking one up and setting default values')
120 FORMAT(i2,18X,'Nalph3')
130 FORMAT(i2,18X,'Nbet3')
140 FORMAT(f14.6,6X,'tertperiod (days)    tag tt')
150 FORMAT(f16.8,4X,'tertT0               tag tu')
160 FORMAT(f12.9,8X,'terte*cos(omega)     tag tv')
170 FORMAT(f12.9,8X,'terte*sin(omega)     tag tw')
180 FORMAT(f13.9,7X,'tertincl (degrees)   tag tx')
190 FORMAT(f13.8,7X,'tertOmega (degrees)  tag ty')
200 FORMAT(f19.7,1X,'tertQ (EB/body3)     tag tz')
210 FORMAT(2(f9.6,1X))
220 FORMAT(f13.6,7X,'P2tconj              tag uj')
230 FORMAT(f14.6,6X,'P2period (days)      tag ut')
240 FORMAT(f16.8,4X,'P2T0                 tag uu')
250 FORMAT(f12.9,8X,'P2ecos               tag uv')
260 FORMAT(f12.9,8X,'P2esin               tag uw')
270 FORMAT(f13.8,7X,'P2incl (degrees)     tag ux')
280 FORMAT(f13.8,7X,'P2Omega (degrees)    tag uy')
290 FORMAT(f19.7,1X,'P2Q (EB/body3)       tag uz')
300 FORMAT(f13.6,7X,'P2ratrad             tag ub')
310 FORMAT(f13.6,7X,'P3tconj              tag vj')
320 FORMAT(f14.6,6X,'P3period (days)      tag vt')
330 FORMAT(f16.8,4X,'P3T0                 tag vu')
340 FORMAT(f12.9,8X,'P3ecos               tag vv')
350 FORMAT(f12.9,8X,'P3esin               tag vw')
360 FORMAT(f13.8,7X,'P3incl (degrees)     tag vx')
370 FORMAT(f13.8,7X,'P3Omega (degrees)    tag vy')
380 FORMAT(f19.7,1X,'P3Q (EB/body3)       tag vz')
390 FORMAT(f13.6,7X,'P3ratrad             tag vb')
400 FORMAT(f13.6,7X,'P4tconj              tag wj')
410 FORMAT(f14.6,6X,'P4period (days)      tag wt')
420 FORMAT(f16.8,4X,'P4T0                 tag wu')
430 FORMAT(f12.9,8X,'P4ecos               tag wv')
440 FORMAT(f12.9,8X,'P4esin               tag ww')
450 FORMAT(f13.8,7X,'P4incl (degrees)     tag wx')
460 FORMAT(f13.8,7X,'P4Omega (degrees)    tag wy')
470 FORMAT(f19.7,1X,'P4Q (EB/body3)       tag wz')
480 FORMAT(f13.6,7X,'P4ratrad             tag wb')
490 FORMAT(f13.6,7X,'P5tconj              tag xj')
500 FORMAT(f14.6,6X,'P5period (days)      tag xt')
510 FORMAT(f16.8,4X,'P5T0                 tag xu')
520 FORMAT(f12.9,8X,'P5ecos               tag xv')
530 FORMAT(f12.9,8X,'P5esin               tag xw')
540 FORMAT(f13.8,7X,'P5incl (degrees)     tag xx')
550 FORMAT(f13.8,7X,'P5Omega (degrees)    tag xy')
560 FORMAT(f19.7,1X,'P5Q (EB/body3)       tag xz')
570 FORMAT(f13.6,7X,'P5ratrad             tag xb')
580 FORMAT(f13.6,7X,'P6tconj              tag sj')
590 FORMAT(f14.6,6X,'P6period (days)      tag st')
600 FORMAT(f16.8,4X,'P6T0                 tag su')
610 FORMAT(f12.9,8X,'P6ecos               tag sv')
620 FORMAT(f12.9,8X,'P6esin               tag sw')
630 FORMAT(f13.8,7X,'P6incl (degrees)     tag sx')
640 FORMAT(f13.8,7X,'P6Omega (degrees)    tag sy')
650 FORMAT(f19.7,1X,'P6Q (EB/body3)       tag sz')
660 FORMAT(f13.6,7X,'P6ratrad             tag sb')
670 FORMAT(f13.6,7X,'P7tconj              tag hj')
680 FORMAT(f14.6,6X,'P7period (days)      tag ht')
690 FORMAT(f16.8,4X,'P7T0                 tag hu')
700 FORMAT(f12.9,8X,'P7ecos               tag hv')
710 FORMAT(f12.9,8X,'P7esin               tag hw')
720 FORMAT(f13.8,7X,'P7incl (degrees)     tag hx')
730 FORMAT(f13.8,7X,'P7Omega (degrees)    tag hy')
740 FORMAT(f19.7,1X,'P7Q (EB/body3)       tag hz')
750 FORMAT(f13.6,7X,'P7ratrad             tag hb')
760 FORMAT(f13.6,7X,'P8tconj              tag kj')
770 FORMAT(f14.6,6X,'P8period (days)      tag kt')
780 FORMAT(f16.8,4X,'P8T0                 tag ku')
790 FORMAT(f12.9,8X,'P8ecos               tag kv')
800 FORMAT(f12.9,8X,'P8esin               tag kw')
810 FORMAT(f13.8,7X,'P8incl (degrees)     tag kx')
820 FORMAT(f13.8,7X,'P8Omega (degrees)    tag ky')
830 FORMAT(f19.7,1X,'P8Q (EB/body3)       tag kz')
840 FORMAT(f13.6,7X,'P8ratrad             tag kb')
!
   RETURN
!
END SUBROUTINE writebody3input
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE writeinput(nalph1,nbet1,nalph2,nbet2,fill1,fill2,  &
   omega1,omega2,dphase,q,finc,teff1,teff2,tgrav1,tgrav2,  &
   betarim,rinner,router,tdisk,xi,ntheta,nradius,alb1,alb2,nref,  &
   rlx,period,fm,separ,gamma,t3,g3,sa3,density,sw1,sw2,sw3,t0,  &
   idraw,iecheck,iidint,iatm,ism1,icnu,icnb,icnv,icnr,icni,icnj,  &
   icnh,icnk,irvfilt,isw1,isw2,isw3,isw4,ilaw,wave,dbolx,dboly,  &
   dwavex,dwavey,ecc,argper,pshift,sw5,sw6,sw7,sw8,sw9,ikeep,  &
   isynch,isw5,isw6,isw7,isw8,isw9,spot1parm,spot2parm,  &
   spotdparm,primmass,primk,primrad,ratrad,frac1,frac2,ecosw,  &
   temprat,idark1,idark2,isw12,isw13,isw21,isw22,isw23,isw24,  &
   bigi,bigbeta,sw23,sw24,powercoeff,sw25,sw26,sw27,sw28,sw29,  &
   sw30,contam,tconj,beam1,beam2,isw25,isw26,isw27,isw28,isw29,  &
   isw30,isw31,isw32,isw33,isw34,ocose,osine,omegadot,contams0,  &
   contams1,contams2,contams3,sw47,sw48,sw49,sw80,sw81,sw82,  &
   sw83,sw84,sw85,sw86,sw87,sw88,sw89,isw80,isw81,isw82,isw83,  &
   isw84,isw85,isw86,isw87,isw88,isw89,sdarkint1,sdarkint2,  &
   sdarkint3,sdarkint4,sdarkint5)
!
!    will write the correctly formatted file ELC.inp and return
!    default parameters
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: nalph1
   INTEGER, INTENT(OUT)                     :: nbet1
   INTEGER, INTENT(OUT)                     :: nalph2
   INTEGER, INTENT(OUT)                     :: nbet2
   REAL(KIND=dp), INTENT(OUT)               :: fill1
   REAL(KIND=dp), INTENT(OUT)               :: fill2
   REAL(KIND=dp), INTENT(OUT)               :: omega1
   REAL(KIND=dp), INTENT(OUT)               :: omega2
   REAL(KIND=dp), INTENT(OUT)               :: dphase
   REAL(KIND=dp), INTENT(OUT)               :: q
   REAL(KIND=dp), INTENT(OUT)               :: finc
   REAL(KIND=dp), INTENT(OUT)               :: teff1
   REAL(KIND=dp), INTENT(OUT)               :: teff2
   REAL(KIND=dp), INTENT(OUT)               :: tgrav1
   REAL(KIND=dp), INTENT(OUT)               :: tgrav2
   REAL(KIND=dp), INTENT(OUT)               :: betarim
   REAL(KIND=dp), INTENT(OUT)               :: rinner
   REAL(KIND=dp), INTENT(OUT)               :: router
   REAL(KIND=dp), INTENT(OUT)               :: tdisk
   REAL(KIND=dp), INTENT(OUT)               :: xi
   INTEGER, INTENT(OUT)                     :: ntheta
   INTEGER, INTENT(OUT)                     :: nradius
   REAL(KIND=dp), INTENT(OUT)               :: alb1
   REAL(KIND=dp), INTENT(OUT)               :: alb2
   INTEGER, INTENT(OUT)                     :: nref
   REAL(KIND=dp), INTENT(OUT)               :: rlx
   REAL(KIND=dp), INTENT(OUT)               :: period
   REAL(KIND=dp), INTENT(OUT)               :: fm
   REAL(KIND=dp), INTENT(OUT)               :: separ
   REAL(KIND=dp), INTENT(OUT)               :: gamma
   REAL(KIND=dp), INTENT(OUT)               :: t3
   REAL(KIND=dp), INTENT(OUT)               :: g3
   REAL(KIND=dp), INTENT(OUT)               :: sa3
   REAL(KIND=dp), INTENT(OUT)               :: density
   REAL(KIND=dp), INTENT(OUT)               :: sw1
   REAL(KIND=dp), INTENT(OUT)               :: sw2
   REAL(KIND=dp), INTENT(OUT)               :: sw3
   REAL(KIND=dp), INTENT(OUT)               :: t0
   INTEGER, INTENT(OUT)                     :: idraw
   INTEGER, INTENT(OUT)                     :: iecheck
   INTEGER, INTENT(OUT)                     :: iidint
   INTEGER, INTENT(OUT)                     :: iatm
   INTEGER, INTENT(OUT)                     :: ism1
   INTEGER, INTENT(OUT)                     :: icnu
   INTEGER, INTENT(OUT)                     :: icnb
   INTEGER, INTENT(OUT)                     :: icnv
   INTEGER, INTENT(OUT)                     :: icnr
   INTEGER, INTENT(OUT)                     :: icni
   INTEGER, INTENT(OUT)                     :: icnj
   INTEGER, INTENT(OUT)                     :: icnh
   INTEGER, INTENT(OUT)                     :: icnk
   INTEGER, INTENT(OUT)                     :: irvfilt
   INTEGER, INTENT(OUT)                     :: isw1
   INTEGER, INTENT(OUT)                     :: isw2
   INTEGER, INTENT(OUT)                     :: isw3
   INTEGER, INTENT(OUT)                     :: isw4
   INTEGER, INTENT(OUT)                     :: ilaw
   REAL(KIND=dp), INTENT(OUT)               :: wave(8)
   REAL(KIND=dp), INTENT(OUT)               :: dbolx(8,2)
   REAL(KIND=dp), INTENT(OUT)               :: dboly(8,2)
   REAL(KIND=dp), INTENT(OUT)               :: dwavex(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: dwavey(8,10)
   REAL(KIND=dp), INTENT(OUT)               :: ecc
   REAL(KIND=dp), INTENT(OUT)               :: argper
   REAL(KIND=dp), INTENT(OUT)               :: pshift
   REAL(KIND=dp), INTENT(OUT)               :: sw5
   REAL(KIND=dp), INTENT(OUT)               :: sw6
   REAL(KIND=dp), INTENT(OUT)               :: sw7
   REAL(KIND=dp), INTENT(OUT)               :: sw8
   REAL(KIND=dp), INTENT(OUT)               :: sw9
   INTEGER, INTENT(OUT)                     :: ikeep
   INTEGER, INTENT(OUT)                     :: isynch
   INTEGER, INTENT(OUT)                     :: isw5
   INTEGER, INTENT(OUT)                     :: isw6
   INTEGER, INTENT(OUT)                     :: isw7
   INTEGER, INTENT(OUT)                     :: isw8
   INTEGER, INTENT(OUT)                     :: isw9
   REAL(KIND=dp), INTENT(OUT)               :: spot1parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spot2parm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: spotdparm(2,4)
   REAL(KIND=dp), INTENT(OUT)               :: primmass
   REAL(KIND=dp), INTENT(OUT)               :: primk
   REAL(KIND=dp), INTENT(OUT)               :: primrad
   REAL(KIND=dp), INTENT(OUT)               :: ratrad
   REAL(KIND=dp), INTENT(OUT)               :: frac1
   REAL(KIND=dp), INTENT(OUT)               :: frac2
   REAL(KIND=dp), INTENT(OUT)               :: ecosw
   REAL(KIND=dp), INTENT(OUT)               :: temprat
   INTEGER, INTENT(OUT)                     :: idark1
   INTEGER, INTENT(OUT)                     :: idark2
   INTEGER, INTENT(OUT)                     :: isw12
   INTEGER, INTENT(OUT)                     :: isw13
   INTEGER, INTENT(OUT)                     :: isw21
   INTEGER, INTENT(OUT)                     :: isw22
   INTEGER, INTENT(OUT)                     :: isw23
   INTEGER, INTENT(OUT)                     :: isw24
   REAL(KIND=dp), INTENT(OUT)               :: bigi
   REAL(KIND=dp), INTENT(OUT)               :: bigbeta
   REAL(KIND=dp), INTENT(OUT)               :: sw23
   REAL(KIND=dp), INTENT(OUT)               :: sw24
   REAL(KIND=dp), INTENT(OUT)               :: powercoeff(8,9)
   REAL(KIND=dp), INTENT(OUT)               :: sw25
   REAL(KIND=dp), INTENT(OUT)               :: sw26
   REAL(KIND=dp), INTENT(OUT)               :: sw27
   REAL(KIND=dp), INTENT(OUT)               :: sw28
   REAL(KIND=dp), INTENT(OUT)               :: sw29
   REAL(KIND=dp), INTENT(OUT)               :: sw30
   REAL(KIND=dp), INTENT(OUT)               :: contam
   REAL(KIND=dp), INTENT(OUT)               :: tconj
   REAL(KIND=dp), INTENT(OUT)               :: beam1
   REAL(KIND=dp), INTENT(OUT)               :: beam2
   INTEGER, INTENT(OUT)                     :: isw25
   INTEGER, INTENT(OUT)                     :: isw26
   INTEGER, INTENT(OUT)                     :: isw27
   INTEGER, INTENT(OUT)                     :: isw28
   INTEGER, INTENT(OUT)                     :: isw29
   INTEGER, INTENT(OUT)                     :: isw30
   INTEGER, INTENT(OUT)                     :: isw31
   INTEGER, INTENT(OUT)                     :: isw32
   INTEGER, INTENT(OUT)                     :: isw33
   INTEGER, INTENT(OUT)                     :: isw34
   REAL(KIND=dp), INTENT(OUT)               :: ocose
   REAL(KIND=dp), INTENT(OUT)               :: osine
   REAL(KIND=dp), INTENT(OUT)               :: omegadot
   REAL(KIND=dp), INTENT(OUT)               :: contams0
   REAL(KIND=dp), INTENT(OUT)               :: contams1
   REAL(KIND=dp), INTENT(OUT)               :: contams2
   REAL(KIND=dp), INTENT(OUT)               :: contams3
   REAL(KIND=dp), INTENT(OUT)               :: sw47
   REAL(KIND=dp), INTENT(OUT)               :: sw48
   REAL(KIND=dp), INTENT(OUT)               :: sw49
   REAL(KIND=dp), INTENT(OUT)               :: sw80
   REAL(KIND=dp), INTENT(OUT)               :: sw81
   REAL(KIND=dp), INTENT(OUT)               :: sw82
   REAL(KIND=dp), INTENT(OUT)               :: sw83
   REAL(KIND=dp), INTENT(OUT)               :: sw84
   REAL(KIND=dp), INTENT(OUT)               :: sw85
   REAL(KIND=dp), INTENT(OUT)               :: sw86
   REAL(KIND=dp), INTENT(OUT)               :: sw87
   REAL(KIND=dp), INTENT(OUT)               :: sw88
   REAL(KIND=dp), INTENT(OUT)               :: sw89
   INTEGER, INTENT(OUT)                     :: isw80
   INTEGER, INTENT(OUT)                     :: isw81
   INTEGER, INTENT(OUT)                     :: isw82
   INTEGER, INTENT(OUT)                     :: isw83
   INTEGER, INTENT(OUT)                     :: isw84
   INTEGER, INTENT(OUT)                     :: isw85
   INTEGER, INTENT(OUT)                     :: isw86
   INTEGER, INTENT(OUT)                     :: isw87
   INTEGER, INTENT(OUT)                     :: isw88
   INTEGER, INTENT(OUT)                     :: isw89
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint1(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint2(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint3(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint4(8)
   REAL(KIND=dp), INTENT(OUT)               :: sdarkint5(8)
!
   REAL(KIND=dp)  ::  www(8)
!
   INTEGER :: j,kk,jj,kkk,i
!
   CHARACTER(LEN=1) :: bell
!
   DATA www/3600.0_dp,4500.0_dp,5550.0_dp,6700.0_dp,8700.0_dp, &
      12000.0_dp,16200.0_dp,22000.0_dp/
!
   bell=CHAR(7)
   WRITE(*,1720)bell
!
   nalph1=40
   nbet1=14
   nalph2=40
   nbet2=14
   fill1=1.00_dp
   fill2=0.005_dp
   omega1=1.0_dp
   omega2=1.0_dp
   dphase=3.0_dp
   q=2.0_dp
   finc=80.0_dp
   teff1=6500.0_dp
   teff2=6500.0_dp
   tgrav1=0.25_dp
   tgrav2=0.25_dp
   betarim=2.0_dp
   rinner=0.005_dp
   router=0.75_dp
   tdisk=30000.0_dp
   xi=-0.75_dp
   ntheta=90
   nradius=60
   alb1=1.0_dp
   alb2=1.0_dp
   nref=1
   rlx=0.001_dp
   period=2.62_dp
   fm=3.0_dp
   separ=5.0_dp
   gamma=50.0_dp
   idraw=0
   iecheck=1
   iidint=1
   iatm=0
   ism1=1
   isw2=0
   ilaw=1
   icnu=1
   icnb=1
   icnv=1
   icnr=1
   icni=1
   icnj=1
   icnh=1
   icnk=1
   t3=-5000.0_dp
   g3=-5.0_dp
   sa3=-0.1_dp
   density=0.0_dp
   sw1=0.0_dp
   sw2=0.0_dp
   sw3=0.0_dp
   t0=0.0_dp
   irvfilt=3
   isw1=0
   isw2=0
   isw3=0
   isw4=0
   isw5=0
   isw6=0
   isw7=0
   isw8=0
   isw9=0
   ikeep=0
   isynch=0
   ecc=0.0_dp
   argper=90.0_dp
   pshift=0.0_dp
   sw5=0.0_dp
   sw6=0.0_dp
   sw7=0.0_dp
   sw8=0.0_dp
   sw9=0.0_dp
   dwavex=0.6_dp
   dwavey=0.0_dp
   powercoeff=0.0_dp
!
   DO  i=1,8
      wave(i)=www(i)
      dbolx(i,1)=0.635_dp
      dbolx(i,2)=0.635_dp
      dboly(i,1)=0.242_dp
      dboly(i,2)=0.242_dp
   END DO
!
   DO  i=1,4
      DO  j=1,2
         spot1parm(j,i)=-1.0_dp
         spot2parm(j,i)=-1.0_dp
         spotdparm(j,i)=-1.0_dp
      END DO
   END DO
!
   primmass=0.0_dp
   primk=0.0_dp
   primrad=0.0_dp
   ratrad=0.0_dp
   frac1=0.0_dp
   frac2=0.0_dp
   ecosw=0.0_dp
   temprat=0.0_dp
   idark1=0
   idark2=0
   isw12=0
   isw13=0
   isw21=0
   isw22=0
   isw23=0
   isw24=0
   bigi=0.0_dp
   bigbeta=0.0_dp
   sw23=0.0_dp
   sw24=0.0_dp
   sw25=0.0_dp
   sw26=0.0_dp
   sw27=0.0_dp
   sw28=0.0_dp
   sw29=0.0_dp
   sw30=0.0_dp
   contam=0.0_dp
   tconj=0.0_dp
   beam1=0.0_dp
   beam2=0.0_dp
   isw25=0
   isw26=0
   isw27=0
   isw28=0
   isw29=0
   isw30=0
   isw31=0
   isw32=0
   isw33=0
   isw34=0
   ocose=0.0_dp
   osine=0.0_dp
   omegadot=0.0_dp
   contams0=0.0_dp
   contams1=0.0_dp
   contams2=0.0_dp
   contams3=0.0_dp
   sw47=0.0_dp
   sw48=0.0_dp
   sw49=0.0_dp
   isw80=0
   isw81=0
   isw82=0
   isw83=0
   isw84=0
   isw85=0
   isw86=0
   isw87=0
   isw88=0
   isw89=0
   sw80=0.0_dp
   sw81=0.0_dp
   sw82=4.5_dp
   sw83=4.5_dp
   sw84=0.0_dp
   sw85=0.0_dp
   sw86=0.0_dp
   sw87=0.0_dp
   sw88=0.0_dp
   sw89=0.0_dp
!
   DO kk=1,8
      sdarkint1(kk)=0.0_dp
      sdarkint2(kk)=0.0_dp
      sdarkint3(kk)=0.0_dp
      sdarkint4(kk)=0.0_dp
      sdarkint5(kk)=0.0_dp
   END DO
!
   OPEN(UNIT=1,FILE='ELC.inp',STATUS='unknown')
!
   WRITE(1,60)nalph1
   WRITE(1,70)nbet1
   WRITE(1,80)nalph2
   WRITE(1,90)nbet2
   WRITE(1,100)fill1
   WRITE(1,110)fill2
   WRITE(1,120)omega1
   WRITE(1,130)omega2
   WRITE(1,140)dphase
   WRITE(1,150)q
   WRITE(1,160)finc
   WRITE(1,170)teff1
   WRITE(1,180)teff2
   WRITE(1,190)tgrav1
   WRITE(1,200)tgrav2
   WRITE(1,210)betarim
   WRITE(1,220)rinner
   WRITE(1,230)router
   WRITE(1,240)tdisk
   WRITE(1,250)xi
   WRITE(1,260)ntheta
   WRITE(1,270)nradius
   WRITE(1,280)alb1
   WRITE(1,290)alb2
   WRITE(1,300)nref
   WRITE(1,310)rlx
   WRITE(1,320)period
   WRITE(1,330)fm
   WRITE(1,340)separ
   WRITE(1,350)gamma
   WRITE(1,360)t3
   WRITE(1,370)g3
   WRITE(1,380)sa3
   WRITE(1,390)density
   WRITE(1,400)sw1
   WRITE(1,410)sw2
   WRITE(1,420)sw3
   WRITE(1,430)t0
   WRITE(1,440)idraw
   WRITE(1,450)iecheck
   WRITE(1,460)iidint
   WRITE(1,470)iatm
   WRITE(1,480)ism1
   WRITE(1,490)icnu,icnb,icnv,icnr,icni,icnj,icnh,icnk
   WRITE(1,500)irvfilt
   WRITE(1,510)isw1
   WRITE(1,520)isw2
   WRITE(1,530)isw3
   WRITE(1,540)isw4
   WRITE(1,550)ilaw
!
   DO  i=1,8
      WRITE(1,560)wave(i),dbolx(i,1),dboly(i,1),dbolx(i,2),  &
         dboly(i,2),dwavex(i,1),dwavey(i,1),dwavex(i,2),dwavey(i,2)
   END DO
!
   WRITE(1,570)ecc
   WRITE(1,580)argper
   WRITE(1,590)pshift
   WRITE(1,600)sw5
   WRITE(1,610)sw6
   WRITE(1,620)sw7
   WRITE(1,630)sw8
   WRITE(1,640)sw9
   WRITE(1,650)ikeep
   WRITE(1,660)isynch
   WRITE(1,670)isw5
   WRITE(1,680)isw6
   WRITE(1,690)isw7
   WRITE(1,700)isw8
   WRITE(1,710)isw9
   WRITE(1,720)spot1parm(1,1)
   WRITE(1,730)spot1parm(1,2)
   WRITE(1,740)spot1parm(1,3)
   WRITE(1,750)spot1parm(1,4)
   WRITE(1,760)spot1parm(2,1)
   WRITE(1,770)spot1parm(2,2)
   WRITE(1,780)spot1parm(2,3)
   WRITE(1,790)spot1parm(2,4)
   WRITE(1,800)spot2parm(1,1)
   WRITE(1,810)spot2parm(1,2)
   WRITE(1,820)spot2parm(1,3)
   WRITE(1,830)spot2parm(1,4)
   WRITE(1,840)spot2parm(2,1)
   WRITE(1,850)spot2parm(2,2)
   WRITE(1,860)spot2parm(2,3)
   WRITE(1,870)spot2parm(2,4)
   WRITE(1,880)spotdparm(1,1)
   WRITE(1,890)spotdparm(1,2)
   WRITE(1,900)spotdparm(1,3)
   WRITE(1,910)spotdparm(1,4)
   WRITE(1,920)spotdparm(2,1)
   WRITE(1,930)spotdparm(2,2)
   WRITE(1,940)spotdparm(2,3)
   WRITE(1,950)spotdparm(2,4)
   WRITE(1,960)primmass
   WRITE(1,970)primk
   WRITE(1,980)primrad
   WRITE(1,990)ratrad
   WRITE(1,1000)frac1
   WRITE(1,1010)frac2
   WRITE(1,1020)ecosw
   WRITE(1,1030)temprat
   WRITE(1,1040)idark1
   WRITE(1,1050)idark2
   WRITE(1,1060)isw12
   WRITE(1,1070)isw13
   WRITE(1,1080)isw21
   WRITE(1,1090)isw22
   WRITE(1,1100)isw23
   WRITE(1,1110)isw24
!
   DO  kk=1,8
      WRITE(1,1120)(powercoeff(kk,jj),jj=1,9)
   END DO
!
   WRITE(1,1130)bigi
   WRITE(1,1140)bigbeta
   WRITE(1,1150)sw23
   WRITE(1,1160)sw24
   WRITE(1,1170)sw25
   WRITE(1,1180)sw26
   WRITE(1,1190)sw27
   WRITE(1,1200)sw28
   WRITE(1,1210)sw29
   WRITE(1,1220)sw30
   WRITE(1,1230)contam
   WRITE(1,1240)tconj
   WRITE(1,1250)beam1
   WRITE(1,1260)beam2
   WRITE(1,1270)isw25
   WRITE(1,1280)isw26
   WRITE(1,1290)isw27
   WRITE(1,1300)isw28
   WRITE(1,1310)isw29
   WRITE(1,1320)isw30
   WRITE(1,1330)isw31
   WRITE(1,1340)isw32
   WRITE(1,1350)isw33
   WRITE(1,1360)isw34
   WRITE(1,1370)ocose
   WRITE(1,1380)osine
   WRITE(1,1390)omegadot
   WRITE(1,1400)contams0
   WRITE(1,1410)contams1
   WRITE(1,1420)contams2
   WRITE(1,1430)contams3
   WRITE(1,1440)sw47
   WRITE(1,1450)sw48
   WRITE(1,1460)sw49
   WRITE(1,1520)sw80
   WRITE(1,1530)sw81
   WRITE(1,1540)sw82
   WRITE(1,1550)sw83
   WRITE(1,1560)sw84
   WRITE(1,1570)sw85
   WRITE(1,1580)sw86
   WRITE(1,1590)sw87
   WRITE(1,1600)sw88
   WRITE(1,1610)sw89
   WRITE(1,1470)(sdarkint1(kkk),kkk=1,8)
   WRITE(1,1480)(sdarkint2(kkk),kkk=1,8)
   WRITE(1,1490)(sdarkint3(kkk),kkk=1,8)
   WRITE(1,1500)(sdarkint4(kkk),kkk=1,8)
   WRITE(1,1510)(sdarkint5(kkk),kkk=1,8)
   WRITE(1,1620)isw80
   WRITE(1,1630)isw81
   WRITE(1,1640)isw82
   WRITE(1,1650)isw83
   WRITE(1,1660)isw84
   WRITE(1,1670)isw85
   WRITE(1,1680)isw86
   WRITE(1,1690)isw87
   WRITE(1,1700)isw88
   WRITE(1,1710)isw89
!
   CLOSE(1)
!
60 FORMAT(i2,18X,'Nalph1')
70 FORMAT(i2,18X,'Nbet1')
80 FORMAT(i2,18X,'Nalph2')
90 FORMAT(i2,18X,'Nbet2')
100 FORMAT(f7.4,13X,'fill1')
110 FORMAT(f7.4,13X,'fill2')
120 FORMAT(f7.2,13X,'omega1')
130 FORMAT(f7.2,13X,'omega2')
140 FORMAT(f5.2,15X,'dphase')
150 FORMAT(f5.2,15X,'Q')
160 FORMAT(f5.2,15X,'finc')
170 FORMAT(f6.1,14X,'Teff1')
180 FORMAT(f6.1,14X,'Teff2')
190 FORMAT(f5.2,15X,'Tgrav1')
200 FORMAT(f5.2,15X,'Tgrav2')
210 FORMAT(f5.2,15X,'betarim')
220 FORMAT(f6.3,14X,'rinner')
230 FORMAT(f5.2,15X,'router')
240 FORMAT(f7.1,13X,'tdisk')
250 FORMAT(f7.4,13X,'xi')
260 FORMAT(i3,17X,'Ntheta')
270 FORMAT(i3,17X,'Nradius')
280 FORMAT(f7.4,13X,'alb1')
290 FORMAT(f7.4,13X,'alb2')
300 FORMAT(i1,19X,'Nref')
310 FORMAT(f10.5,10X,'log10(Lx)')
320 FORMAT(f9.6,11X,'Period')
330 FORMAT(f6.3,14X,'fm')
340 FORMAT(f6.3,14X,'separ')
350 FORMAT(f7.2,13X,'gamma velocity (km/sec)')
360 FORMAT(f10.2,10X,'t3')
370 FORMAT(f5.2,15X,'g3')
380 FORMAT(f12.6,8X,'SA3')
390 FORMAT(f12.6,8X,'density in g/cc')
400 FORMAT(f12.6,8X,'onephase')
410 FORMAT(f12.6,8X,'usepot1')
420 FORMAT(f12.6,8X,'usepot2')
430 FORMAT(f12.6,8X,'T0')
440 FORMAT(i1,19X,'idraw')
450 FORMAT(i1,19X,'iecheck')
460 FORMAT(i1,19X,'iidint')
470 FORMAT(i1,19X,'iatm  (0 for BB, 1 for model atmospheres)')
480 FORMAT(i1,19X,'ism1  (0 for all phases, 1 for 0-180)')
490 FORMAT(8(i1,1X),4X,'icnU,icnB,icnV,icnR,icnI,icnJ,icnH,icnK')
500 FORMAT(i1,19X,'iRVfilt')
510 FORMAT(i1,19X,'ionephase')
520 FORMAT(i1,19X,'isquare')
530 FORMAT(i1,19X,'iusepot')
540 FORMAT(i1,19X,'ifixgamma')
550 FORMAT(i2,18X,'ilaw  (1=linear law, 2=logarithmic law,',' 3=s'  &
      ,'quare root law, 4=quad law, >10 for power',' series)')
560 FORMAT(f7.1,3X,8(f7.4,1X))
570 FORMAT(f11.8,9X,'eccentricity')
580 FORMAT(f13.8,7X,'argument of peristron in degrees')
590 FORMAT(f11.8,9X,'pshift')
600 FORMAT(f12.6,8X,'asini (projected semimajor axis in seconds)')
610 FORMAT(f12.6,8X,'median fit (geneticELC only)')
620 FORMAT(f12.6,8X,'sw7 (phase range when sw8>sw7>0)')
630 FORMAT(f12.6,8X,'sw8 (phase range when sw8>sw7>0)')
640 FORMAT(f12.6,8X,'time step when itime=2')
650 FORMAT(i1,19X,'ikeep (1 to put eclipse at phase 0.0)')
660 FORMAT(i1,19X,'isynch (1 to keep rotation synchronous',' at p'  &
      ,'eriastron)')
670 FORMAT(i1,19X,'ispotprof')
680 FORMAT(i1,19X,'igrav')
690 FORMAT(i1,19X,'itime')
700 FORMAT(i6,14X,'MonteCarlo (0 for interpolation, >10 ','for Mo'  &
      ,'nte Carlo)')
710 FORMAT(i6,14X,'ielite')
720 FORMAT(f10.7,10X,'Temperature factor spot 1, star 1')
730 FORMAT(f11.7,9X,'Latitude of spot 1, star 1 (degrees)')
740 FORMAT(f11.7,9X,'Longitude of spot 1, star 1 (degrees)')
750 FORMAT(f11.7,9X,'Angular radius of spot 1, star 1 (degrees)')
760 FORMAT(f10.7,10X,'Temperature factor spot 2, star 1')
770 FORMAT(f11.7,9X,'Latitude of spot 2, star 1 (degrees)')
780 FORMAT(f11.7,9X,'Longitude of spot 2, star 1 (degrees)')
790 FORMAT(f11.7,9X,'Angular radius of spot 2, star 1 (degrees)')
800 FORMAT(f10.7,10X,'Temperature factor spot 1, star 2')
810 FORMAT(f11.7,9X,'Latitude of spot 1, star 2 (degrees)')
820 FORMAT(f11.7,9X,'Longitude of spot 1, star 2 (degrees)')
830 FORMAT(f11.7,9X,'Angular radius of spot 1, star 2 (degrees)')
840 FORMAT(f10.7,10X,'Temperature factor spot 2, star 2')
850 FORMAT(f11.7,9X,'Latitude of spot 2, star 2 (degrees)')
860 FORMAT(f11.7,9X,'Longitude of spot 2, star 2 (degrees)')
870 FORMAT(f11.7,9X,'Angular radius of spot 2, star 2 (degrees)')
880 FORMAT(f10.7,10X,'Temperature factor spot 1, disk')
890 FORMAT(f11.7,9X,'Azimuth of spot 1, disk (degrees)')
900 FORMAT(f11.7,9X,'Radial cutoff of spot 1, disk (0 <= r_cut <='  &
      ,'1)')
910 FORMAT(f11.7,9X,'Angular size of spot 1, disk (degrees)')
920 FORMAT(f10.7,10X,'Temperature factor spot 2, disk')
930 FORMAT(f11.7,9X,'Azimuth of spot 2, disk (degrees)')
940 FORMAT(f11.7,9X,'Radial cutoff of spot 2, disk (0 <= r_cut <='  &
      ,'1)')
950 FORMAT(f11.7,9X,'Angular size of spot 2, disk (degrees)')
960 FORMAT(f13.9,7X,'primmass (star 1 mass in solar masses)')
970 FORMAT(f14.9,6X,'primK (K-velocity of star 1 in km/sec)')
980 FORMAT(f14.9,6X,'primrad (star 1 radius in solar radii)')
990 FORMAT(f16.9,4X,'ratrad (ratio of star 1 radius and star 2 ra'  &
      ,'dius)')
1000 FORMAT(f5.2,15X,'frac1 (fractional radius star 1: R_1/a)')
1010 FORMAT(f5.2,15X,'frac2 (fractional radius star 2: R_2/a)')
1020 FORMAT(f12.9,8X,'ecosw (phase difference between eclipses)')
1030 FORMAT(f10.7,10X,'temprat (T_1/T_2)')
1040 FORMAT(i1,19X,'idark1')
1050 FORMAT(i1,19X,'idark2')
1060 FORMAT(i6,14X,'Npoly (0 for numerical)')
1070 FORMAT(i1,19X,'ifasttrans (>0 for fast transit mode)')
1080 FORMAT(i1,19X,'ialign (1 for rotation aligned with orbit)')
1090 FORMAT(i1,19X,'set to 1 to supress optimizer screen output')
1100 FORMAT(i1,19X,'iwriteeclipse (1 to fit for eclipse times)')
1110 FORMAT(i1,19X,'frac switch (>1 to enable ELCratio.???? files)' )
1120 FORMAT(9(f8.5,1X))
1130 FORMAT(f11.7,9X,'axis_I (inclination of rotation axis if ','i'  &
      ,'align=0)')
1140 FORMAT(f11.7,9X,'axis_beta (angle of rotation axis wrt to ',''  &
      ,'orbit if ialign=0)')
1150 FORMAT(f15.7,5X,'t_start')
1160 FORMAT(f15.7,5X,'t_end')
1170 FORMAT(f10.7,9X,'asini error')
1180 FORMAT(f11.8,9X,'reference phase for disk fraction')
1190 FORMAT(f11.8,9X,'radfill1 (set to use fill1 in terms of R_ef' ,'f')
1200 FORMAT(f11.8,9X,'radfill2 (set to use fill2 in terms of R_ef' ,'f')
1210 FORMAT(f10.4,10X,'bin size for light curves (minutes)')
1220 FORMAT(f10.4,10X,'bin size for RV curves (minutes)')
1230 FORMAT(f10.7,10X,'Kepler contamination')
1240 FORMAT(f15.8,5X,'Tconj')
1250 FORMAT(f5.2,15X,'beam1 (Doppler boost factor, star 1')
1260 FORMAT(f5.2,15X,'beam2 (Doppler boost factor, star 2)')
1270 FORMAT(i1,19X,'X-ray foreshortening switch',' (1 for point so'  &
      ,'urce)')
1280 FORMAT(i1,19X,'iGR (1 for GR, 2 for tidal, 3 for both)')
1290 FORMAT(i6,14X,'Nterms for fast analytic')
1300 FORMAT(i1,19X,'set to 1 to fit for Tconj')
1310 FORMAT(i1,19X,'set to 1 to fit for e*sin(omega), ','e*cos(ome'  &
      ,'ga)')
1320 FORMAT(i1,19X,'body 3 switch')
1330 FORMAT(i1,19X,'Ngap')
1340 FORMAT(i16,4X,'jdum (seed for markovELC, geneticELC, ','rando'  &
      ,'mELC)')
1350 FORMAT(i1,19X,'mandel  (0 for Gimenez, 1 for Mandel & Agol)')
1360 FORMAT(i1,19X,'Iseason (1 for seasonal Kepler contamination)')
1370 FORMAT(f12.8,8X,'e*cos(omega)')
1380 FORMAT(f12.8,8X,'e*sin(omega)')
1390 FORMAT(f12.8,8X,'omega_dot (degrees per year)')
1400 FORMAT(f12.8,8X,'contamS0 (season 0 contamination, tag s0)')
1410 FORMAT(f12.8,8X,'contamS1 (season 1 contamination, tag s1)')
1420 FORMAT(f12.8,8X,'contamS2 (season 2 contamination, tag s2)')
1430 FORMAT(f12.8,8X,'contamS3 (season 3 contamination, tag s3)')
1440 FORMAT(f16.8,4X,'Tref for dynamical integrator')
1450 FORMAT(f12.8,8X,'threshold to write chi^2')
1460 FORMAT(f13.9,7X,'Omega_bin ','(nodal angle of binary in degre'  &
      ,'es)')
1520 FORMAT(f10.4,10X,'Teff4       tag t4')
1530 FORMAT(f10.4,10X,'Teff5       tag t5')
1540 FORMAT(f7.4,13X,'g4')
1550 FORMAT(f7.4,13X,'g5')
1560 FORMAT(f12.2,8X,'chi^2 penalty for transit')
1570 FORMAT(f5.2,15X,'impact parameter threshold for ','eclipse ch'  &
      ,'ecking')
1580 FORMAT(f5.2,15X,'sw86 (currently inactive)')
1590 FORMAT(f5.2,15X,'sw87 (currently inactive)')
1600 FORMAT(f5.2,15X,'sw88 (currently inactive)')
1610 FORMAT(f5.2,15X,'sw89 (currently inactive)')
1620 FORMAT(i1,19X,'set to 1 for binary+binary mode (use',' body3 '  &
      ,'switch=4)')
1630 FORMAT(i1,19X,'set to 1 for transit penalty')
1640 FORMAT(i1,19X,'set to 1 to exclude body 3 eclipses')
1650 FORMAT(i1,19X,'set to 1 to exclude body 4 eclipses')
1660 FORMAT(i1,19X,'set to 1 to exclude body 5 eclipses')
1670 FORMAT(i1,19X,'set to 1 to exclude secondary eclipses')
1680 FORMAT(i1,19X,'set to 1 to use fluxes (needs Nterms>0)')
1690 FORMAT(i1,19X,'set to 1 for fast binning of Kepler models')
1700 FORMAT(i3,17X,'Ndynwin')
1710 FORMAT(i1,19X,'NSC')
1470 FORMAT(8(f11.7,1X),' fluxes for star 1')
1480 FORMAT(8(f11.7,1X),' fluxes for star 2')
1490 FORMAT(8(f11.7,1X),' fluxes for star 3')
1500 FORMAT(8(f11.7,1X),' fluxes for star 4')
1510 FORMAT(8(f11.7,1X),' fluxes for star 5')
1720 FORMAT(a1,'I can''T FIND THE FILE ''ELC.INP''!  I''M MAKING',  &
      ' one up and setting default values')
!
   RETURN
!
END SUBROUTINE writeinput
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
