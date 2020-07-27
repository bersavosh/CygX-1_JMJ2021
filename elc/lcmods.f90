MODULE accur
!
  IMPLICIT NONE
!
  SAVE
!
!  gives REAL*16
!
! INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(33,4931)
!
!  gives REAL*10
!
!  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(18,4931)
!
!  gives double precision, REAL*8
!
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15,307)
!
!   gives single precision, REAL*4
!
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,37)
!
!   gives double precision, REAL*8, use with PGPLOT
!
  INTEGER, PARAMETER :: pg = SELECTED_REAL_KIND(15,307)
!
END MODULE accur
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
MODULE getpie
!
!  will return values of pi, 2*pi, pi/2, etc.
!
   USE accur
!
   IMPLICIT NONE
!
   SAVE
!
   REAL(KIND=dp), PARAMETER                      :: pie=4.0_dp*ATAN(1.0_dp)
   REAL(KIND=dp), PARAMETER                      :: twopie=pie+pie
   REAL(KIND=dp), PARAMETER                      :: halfpie=pie/2.0_dp
   REAL(KIND=dp), PARAMETER                      :: threehalfpie=1.5_dp*pie
   REAL(KIND=dp), PARAMETER                      :: degtorad=pie/180.0_dp
   REAL(KIND=dp), PARAMETER                      :: radtodeg=180.0_dp/pie
   REAL(KIND=dp), PARAMETER                      :: fourthirdspie=4.0_dp/3.0_dp*pie
   REAL(KIND=dp), PARAMETER                      :: fourpiesquared=4.0_dp*pie*pie
!
END MODULE getpie
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
MODULE constants
!
!   Returns various conversion factors, physical constants, etc.
!
   USE accur
   USE getpie, ONLY : pie
!
   IMPLICIT NONE
!
   SAVE
!
!   Speed of light, in km/sec and in meters/sec. This
!   is exact.
!
   REAL(KIND=dp), PARAMETER  :: speedlightkm=2.99792458E+05_dp
   REAL(KIND=dp), PARAMETER  :: speedlightm=2.99792458E+08_dp
!
!  AU in meters, from IAU 2010.  This is exact.
!
   REAL(KIND=dp), PARAMETER  :: au=149597870700.0_dp
!
!   Number of seconds in a day, exact.
!
   REAL(KIND=dp), PARAMETER   :: secinday=86400.0_dp
!
!   convert AU per day to meters/sec
!
   REAL(KIND=dp), PARAMETER  :: auperdaytompersec=au/secinday
   REAL(KIND=dp), PARAMETER  :: speedauperday=speedlightm/auperdaytompersec
!
!   Newton's gravitational constant, in mks and cgs,
!   from http://physics.nist.gov/cuu/index.html
!
   REAL(KIND=dp), PARAMETER   :: Gnewton=6.67408E-11_dp
   REAL(KIND=dp), PARAMETER   :: gnewtoncgs=6.67408E-08_dp
!
!   Here is the Gravitational Constant in AU**3/solar_mass/day**2
!   units, used in the dynamical integrator.  From
!   Clemence, G. M. (1965). "The System of Astronomical
!   Constants". Annual Review of Astronomy and
!   Astrophysics. 3: 93.
!
   REAL(KIND=dp), PARAMETER   :: g=(0.01720209895_dp)**2
!
!   Stefan-Boltzmann constant in cgs,
!   from http://physics.nist.gov/cuu/index.html
!
   REAL(KIND=dp), PARAMETER   :: stefan=5.670367E-05_dp
!
!   Nominal solar radius in meters, from IAU 2015
!   Resolution B3, Mamajek, E.E, Prsa, A, Torres, G.,
!   et al., arXiv:1510.07674
!
   REAL(KIND=dp), PARAMETER  :: solarrad=6.9570E+08_dp
!
!   The equatorial radius of Earth in meters, from Mamajek,
!   et al.
!
   REAL(KIND=dp), PARAMETER   :: earthmeter=6.3781E+06_dp
   REAL(KIND=dp), PARAMETER   :: earthcm=100.0_dp*earthmeter
!
!   The ratio of the solar radius to Earth's radius
!
   REAL(KIND=dp), PARAMETER   :: earthradinsolar=solarrad/earthmeter
!
!   (GM)_sun in meters^3/second^2, where
!   (GM)_sun = (AU)^3G/secinday^2, which is
!   roughly 1.32712440018d20 meter^3/second^2
!
   REAL(KIND=dp), PARAMETER   :: gmsun=au**3*g/secinday**2
!
!   The solar mass in kg is gmsun/Gnewton
!
   REAL(KIND=dp), PARAMETER   :: solarmass=gmsun/gnewton
!
!   The geocentric gravitational constant, from
!   Lumin, B., Capitaine, N., Fienga, A., et al. 2011,
!   Celest. Mech. Dyn. Ast., 110:293-304
!
   REAL(KIND=dp), PARAMETER :: gmearth=3.986004418E+14_dp
!
!   The mass of the Earth is gmearth/Gnewton
!
   REAL(KIND=dp), PARAMETER  :: earthkg=gmearth/gnewton
   REAL(KIND=dp), PARAMETER  :: earthgram=1000.0_dp*earthkg
!
!   The Sun mass to Earth mass ratio
!
   REAL(KIND=dp), PARAMETER   :: earthmasstosolar=gmsun/gmearth
!
!  (GM)_jupiter in meters^3/seconds^2, from IAU 2015
!   Resolution B3, Mamajek, E.E, Prsa, A, Torres, G.,
!   et al., arXiv:1510.07674
!
   REAL(KIND=dp), PARAMETER   :: gmjupiter=1.2668653E+17_dp
   REAL(KIND=dp), PARAMETER   :: jupiterkg=gmjupiter/gnewton
   REAL(KIND=dp), PARAMETER   :: jupitermassinsolar=gmsun/gmjupiter
!
!  nominal Jupiter equatoral radius in meters, from IAU 2015
!   Resolution B3, Mamajek, E.E, Prsa, A, Torres, G.,
!   et al., arXiv:1510.07674
!
   REAL(KIND=dp), PARAMETER   :: jupitermeter=7.1492E+07_dp
!
!   The number of AU in a nominal solar radius
!
   REAL(KIND=dp), PARAMETER   :: auinsolarrad=solarrad/au
!
!   Random constant used.
!
   REAL(KIND=dp), PARAMETER   :: coeff=(4.0_dp*pie*pie*solarrad**3)/(gnewton*(3600.0_dp)**2*solarmass)
!
!   The nominal solar gravity in cgs.
!
   REAL(KIND=dp), PARAMETER   :: gsun=100.0_dp*gnewton*solarmass/solarrad**2
!
!   The number of days in a year (Julian year)
!
   REAL(KIND=dp), PARAMETER   :: daysinyear=365.25_dp
!
!   Boltzmann constant
!
   REAL(KIND=dp), PARAMETER   :: boltzmann=1.380649E-23_dp
!
!   Planck's constant
!
   REAL(KIND=dp), PARAMETER   :: planck=6.62607015E-34_dp
!
END MODULE constants
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
MODULE medblock
!
!   replaces the common blocks medblock and ranblock
!
  USE accur
! 
  IMPLICIT NONE
!
  INTEGER(KIND=8), SAVE     ::  idum
!
  REAL(KIND=dp), SAVE       :: rmed
!
END MODULE medblock

MODULE ran_lec
!
! L'Ecuyer's 1996 random number generator.
! Fortran version by Alan.Miller @ vic.cmis.csiro.au
! N.B. This version is compatible with Lahey's ELF90
! http://www.ozemail.com.au/~milleraj
! Latest revision - 30 March 1999
!
   USE accur
!
   IMPLICIT NONE
!
!INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(14, 60)
!
! These are unsigned integers in the C version
!
   INTEGER, SAVE         :: seed1 = 1234, seed2 = -4567, seed3 = 7890
   INTEGER(KIND=8), SAVE :: seed5 = 153587801, seed6 = -759022222
   INTEGER(KIND=8), SAVE :: seed7 = 1288503317, seed8 = -1718083407, seed9 = -123456789
!
CONTAINS
!
   SUBROUTINE init_seeds(i1,i2,i3)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: i1,i2,i3
!
      seed1 = i1
      seed2 = i2
      seed3 = i3
      IF(IAND(seed1,-2) == 0)seed1 = i1 - 1023
      IF(IAND(seed2,-8) == 0)seed2 = i2 - 1023
      IF(IAND(seed3,-16) == 0)seed3 = i3 - 1023
!
      RETURN
!
   END SUBROUTINE init_seeds
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
   SUBROUTINE init_seeds6(i1, i2, i3, i4, i5)
!
      IMPLICIT NONE
!
      INTEGER (KIND=8), INTENT(IN) :: i1, i2, i3, i4, i5
!
      seed5 = i1
      seed6 = i2
      seed7 = i3
      seed8 = i4
      seed9 = i5
      IF (IAND(seed5,      -2) == 0) seed5 = i1 - 8388607
      IF (IAND(seed6,    -512) == 0) seed6 = i2 - 8388607
      IF (IAND(seed7,   -4096) == 0) seed7 = i3 - 8388607
      IF (IAND(seed8, -131072) == 0) seed8 = i4 - 8388607
      IF (IAND(seed9,-8388608) == 0) seed9 = i5 - 8388607
!
      RETURN
!
   END SUBROUTINE init_seeds6
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
   FUNCTION ran5(idum) RESULT(random_numb)
!
! Generates a random number between 0 and 1.  Translated from C function in:
! Reference:
! L'Ecuyer, P. (1996) `Maximally equidistributed combined Tausworthe
! generators', Math. of Comput., 65, 203-213.
!
! The cycle length is claimed to be about 2^(88) or about 3.09E+26.
! Actually - (2^31 - 1).(2^29 - 1).(2^28 - 1).
!
      IMPLICIT NONE
!
!   JAO added the dummy input variable idum, and scale_factor
!
      INTEGER, INTENT(IN)      :: idum
!
      REAL(KIND=dp)            :: random_numb
!
      INTEGER                  :: b,jdum
!
      REAL(KIND=dp), PARAMETER :: scale_factor=1.0_dp/(2.0_dp**32 - 1.0_dp)
!
! N.B. ISHFT(i,j) is a bitwise (non-circular) shift operation;
!      to the left if j > 0, otherwise to the right.
!
      jdum=idum+1
      b  = ISHFT(IEOR(ISHFT(seed1,13),seed1),-19)
      seed1 = IEOR(ISHFT(IAND(seed1,-2),12),b)
      b  = ISHFT(IEOR(ISHFT(seed2,2),seed2),-25)
      seed2 = IEOR(ISHFT(IAND(seed2,-8),4),b)
      b  = ISHFT(IEOR(ISHFT(seed3,3),seed3),-11)
      seed3 = IEOR(ISHFT(IAND(seed3,-16),17),b)
!      random_numb = IEOR(IEOR(seed1,seed2),seed3)*2.3283064365E-10_dp+0.5_dp
      random_numb = IEOR(IEOR(seed1,seed2),seed3)*scale_factor+0.5_dp
!
      RETURN
!
   END FUNCTION ran5
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
   FUNCTION ran6(idum) RESULT(random_numb)
!
! Generates a random number between 0 and 1.  Translated from C function in:
! Reference:
! L'Ecuyer, P. (1999) `Tables of maximally equidistributed combined LFSR
! generators', Math. of Comput., 68, 261-269.
!
! The cycle length is claimed to be about 2^(258) or about 4.63 x 10^77.
! Actually - (2^63 - 1).(2^55 - 1).(2^52 - 1).(2^47 - 1).(2^41 - 1)
!
      IMPLICIT NONE
!
      REAL(KIND=dp) :: random_numb
!
      INTEGER(KIND=8), INTENT(IN OUT)      :: idum
!
      INTEGER (KIND=8)   :: b
!      INTEGER            :: jdum
!
      REAL(KIND=dp), PARAMETER :: scale_factor=1.0_dp/(2.0_dp**64 - 1.0_dp)
!
      idum=idum+1
!
! N.B. ISHFT(i,j) is a bitwise (non-circular) shift operation;
!      to the left if j > 0, otherwise to the right.
!
      b  = ISHFT(IEOR(ISHFT(seed5,1),seed5),-53)
      seed5 = IEOR(ISHFT(IAND(seed5,-2),10),b)
      b  = ISHFT(IEOR(ISHFT(seed6,24),seed6),-50)
      seed6 = IEOR(ISHFT(IAND(seed6,-512),5),b)
      b  = ISHFT(IEOR(ISHFT(seed7,3),seed7),-23)
      seed7 = IEOR(ISHFT(IAND(seed7,-4096),29),b)
      b  = ISHFT(IEOR(ISHFT(seed8,5),seed8),-24)
      seed8 = IEOR(ISHFT(IAND(seed8,-131072),23),b)
      b  = ISHFT(IEOR(ISHFT(seed9,3),seed9),-33)
      seed9 = IEOR(ISHFT(IAND(seed9,-8388608),8),b)
!
! The constant below is the reciprocal of (2^64 - 1)
!random_numb = IEOR( IEOR( IEOR( IEOR(seed5,seed6), seed7), seed8), s5)  &
!              * 5.4210108624275221E-20_dp + 0.5_dp
!
      random_numb = IEOR(IEOR(IEOR(IEOR(seed5,seed6),seed7),seed8),seed9)  &
         * scale_factor + 0.5_dp
!
      RETURN
!
   END FUNCTION ran6
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
   FUNCTION gasdev5(idum) RESULT(gauss_deviate)
!
!  Routine for computing random numbers drawn from normal
!  distribution with zero mean and unit variance.
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN OUT)   :: idum
!
      INTEGER        :: jdum
!
      REAL(KIND=dp)  :: v1,v2,rsq,fac,gauss_deviate
!
      jdum=idum+1
10    v1=2.0_dp*ran5(idum)-1.0_dp
      v2=2.0_dp*ran5(idum)-1.0_dp
      rsq=v1**2+v2**2
      IF((rsq >= 1.0_dp).OR.(ABS(rsq) <= EPSILON(rsq)))GO TO 10
      fac=SQRT(-2.0_dp*LOG(rsq)/rsq)
      gauss_deviate=v2*fac
!
!  Both v1 and v2 are Gaussian deviates, just keep
!  the second one for purposes of restarting the sequence
!
      RETURN
!
   END FUNCTION gasdev5
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
   FUNCTION gasdev6(idum) RESULT(gauss_deviate)
!
!  Routine for computing random numbers drawn from normal
!  distribution with zero mean and unit variance.
!
      IMPLICIT NONE
!
      INTEGER(KIND=8), INTENT(IN OUT)   :: idum
!
!      INTEGER(KIND=8)                   :: jdum
!
      REAL(KIND=dp)  :: v1,v2,rsq,fac,gauss_deviate
!
!      jdum=idum+1
10    v1=2.0_dp*ran6(idum)-1.0_dp
      v2=2.0_dp*ran6(idum)-1.0_dp
      rsq=v1**2+v2**2
      IF((rsq >= 1.0_dp).OR.(ABS(rsq) <= EPSILON(rsq)))GO TO 10
      fac=SQRT(-2.0_dp*LOG(rsq)/rsq)
      gauss_deviate=v2*fac
!
!  Both v1 and v2 are Gaussian deviates, just keep
!  the second one for purposes of restarting the sequence
!
      RETURN
!
   END FUNCTION gasdev6
!
END MODULE ran_lec
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
