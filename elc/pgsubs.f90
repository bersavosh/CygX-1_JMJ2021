!*GRAREA -- define a clipping window
!+
SUBROUTINE grarea(ident,x0,y0,xsize,ysize)
!
! GRPCKG: Define a rectangular window in the current plotting area. All
! graphics (except characters written with GRCHAR) will be blanked
! outside this window.  The default window is the full plotting area
! defined by default or by GRSETS.
!
! Arguments:
!
! IDENT (input, integer): the plot identifier, returned by GROPEN.
! X0, Y0 (input, real): the lower left corner of the window, in absolute
!       device coordinates.
! XSIZE, YSIZE (input, real): width and height of the window in absolute
!       coordinates; if either is negative, the window will be reset to
!       the full plotting area.
!--
!  1-Feb-1983 - [TJP].
! 25-Nov-1994 - use floating-point [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                    :: ident
   REAL(KIND=pg), INTENT(IN)              :: x0
   REAL(KIND=pg), INTENT(IN)              :: y0
   REAL(KIND=pg), INTENT(IN)              :: xsize
   REAL(KIND=pg), INTENT(IN)              :: ysize
!
   CALL grslct(ident)
!
   IF((xsize <= 0.0_pg).OR.(ysize <= 0.0_pg))THEN
      grxmin(ident)=0
      grxmax(ident)=grxmxa(ident)
      grymin(ident)=0
      grymax(ident)=grymxa(ident)
   ELSE
      grxmin(ident)=MAX(x0,0.0_pg)
      grymin(ident)=MAX(y0,0.0_pg)
      grxmax(ident)=MIN(xsize+x0,REAL(grxmxa(ident),KIND=pg))
      grymax(ident)=MIN(ysize+y0,REAL(grymxa(ident),KIND=pg))
   END IF
!
END SUBROUTINE grarea
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRBPIC -- begin picture
!+
SUBROUTINE grbpic
!
! GRPCKG (internal routine). Send a "begin picture" command to the
! device driver, and send commands to set deferred attributes (color,
! line width, etc.)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg) :: rbuf(2)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=20) :: chr
!
   grpltd(grcide)=.true.
   IF(grgtyp > 0)THEN
!
!         -- begin picture
!
      rbuf(1)=grxmxa(grcide)
      rbuf(2)=grymxa(grcide)
      nbuf=2
      CALL grexec(grgtyp,11,rbuf,nbuf,chr,lchr)
!
!         -- set color index
!
      rbuf(1)=grccol(grcide)
      nbuf=1
      CALL grexec(grgtyp,15,rbuf,nbuf,chr,lchr)
!
!         -- set line width
!
      IF(grgcap(grcide)(5:5) == 'T')THEN
         rbuf(1)=ABS(grwidt(grcide))
         nbuf=1
         CALL grexec(grgtyp,22,rbuf,nbuf,chr,lchr)
      END IF
!
!         -- set hardware dashing
!
      IF(grgcap(grcide)(3:3) == 'D')THEN
         rbuf(1)=grstyl(grcide)
         nbuf=1
         CALL grexec(grgtyp,19,rbuf,nbuf,chr,lchr)
      END IF
   END IF
!
END SUBROUTINE grbpic
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!+
!                                                                     *
!  PGPLOT Fortran Graphics Subroutine Library                         *
!                                                                     *
!  T. J. Pearson, California Institute of Technology,                 *
!  Pasadena, California 91125.                                        *
!                                                                     *
!  ------------------------------------------------                   *
!  These routines are not called by PGPLOT but are called by some     *
!  old user-written programs.                                         *
! GRCHAR -- draw a string of characters
! GRCHR0 -- support routine for GRCHAR and GRMARK
! GRDAT2 -- character set definition (block data)
! GRGTC0 -- obtain character digitization
! GRMARK -- mark points with specified symbol
!--
!*GRCHAR -- draw a string of characters
!+
SUBROUTINE grchar(ident,center,orient,absxy,x0,y0,string)
!
! GRPCKG: Draw a string of characters. The plot is not windowed
! in the current subarea, but in the full plotting area.
!
! Arguments:
!
! IDENT (input, integer): plot identifier, as returned by GROPEN.
! CENTER (input, logical): if .TRUE., the first character of the string
!      is centered at (X0,Y0); otherwise the bottom left corner of the
!      first character is placed at (X0,Y0).
! ORIENT (input, real): the angle in degrees that the string is to make
!      with the horizontal, increasing anticlockwise.
! ABSXY (input, logical): if .TRUE., (X0,Y0) are absolute device
!      coordinates; otherwise they are world coordinates (the scaling
!      transformation is applied).
! X0, Y0 (input, real): position of first character (see CENTER).
! STRING (input, character): the string of ASCII characters; control
!      characters 0-20 have special representations; all other
!      non-graphic characters are plotted as blank spaces.
!
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: ident
   LOGICAL, INTENT(IN OUT)                  :: center
   REAL(KIND=pg), INTENT(IN OUT)            :: orient
   LOGICAL, INTENT(IN OUT)                  :: absxy
   REAL(KIND=pg), INTENT(IN OUT)            :: x0
   REAL(KIND=pg), INTENT(IN OUT)            :: y0
   CHARACTER (LEN=256), INTENT(IN OUT)      :: string
!
   CALL grslct(ident)
   CALL grchr0(.false.,center,orient,absxy,x0,y0,string)
   RETURN
!
END SUBROUTINE grchar
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRCHR0 -- support routine for GRCHAR and GRMARK
!+
SUBROUTINE grchr0(window,center,orient,absxy,x0,y0,string)
!
! GRPCKG (internal routine): Support routine for GRCHAR and GRMARK.
! Draw a string of characters.
!
! Arguments:
!
! WINDOW (input, logical): if .TRUE., the plot is windowed in the
!      current window.
! CENTER (input, logical): if .TRUE., the first character of the string
!      is centered at (X0,Y0); otherwise the bottom left corner of the
!      first character is placed at (X0,Y0).
! ORIENT (input, real): the angle in degrees that the string is to make
!      with the horizontal, increasing anticlockwise.
! ABSXY (input, logical): if .TRUE., (X0,Y0) are absolute device
!      coordinates; otherwise they are world coordinates (the scaling
!      transformation is applied).
! X0, Y0 (input, real): position of first character (see CENTER).
! STRING (input, character): the string of ASCII characters; control
!      characters 0-20 have special representations; all other
!      non-graphic characters are plotted as blank spaces.
!
! (1-Mar-1983)
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   LOGICAL, INTENT(IN)                  :: window
   LOGICAL, INTENT(IN)                  :: center
   REAL(KIND=pg), INTENT(IN)            :: orient
   LOGICAL, INTENT(IN)                  :: absxy
   REAL(KIND=pg), INTENT(IN)            :: x0
   REAL(KIND=pg), INTENT(IN)            :: y0
   CHARACTER (LEN=*), INTENT(IN)        :: string
!

   INTEGER, PARAMETER :: dot=3
   INTEGER, PARAMETER :: move=2
   INTEGER, PARAMETER :: vecsiz=30
   REAL(KIND=pg), PARAMETER :: pi=4.0_pg*atan(1.0_pg) !3.14159265359
   CHARACTER (LEN=1) :: next
   REAL(KIND=pg) :: xmin,xmax,ymin,ymax
   INTEGER :: mode,lstyle,level
   INTEGER :: i,j,l,ch,points
   LOGICAL :: more
   REAL(KIND=pg) :: angle,factor,base,fac
   REAL(KIND=pg) :: cosa,sina
   REAL(KIND=pg) :: dx,dy,xorg,yorg
   REAL(KIND=pg) :: xc(vecsiz),yc(vecsiz),xt,yt
!
   IF(LEN(string) <= 0)RETURN
!
! Compute scaling and orientation.
!
   CALL grqls(lstyle)
   CALL grsls(1)
   angle=(MOD(orient,360.0_pg)/180.0_pg)*pi
   factor=grcfac(grcide)
   cosa=factor*COS(angle)
   sina=factor*SIN(angle)
   dx=10.0_pg*cosa
   dy=10.0_pg*sina
   CALL grtxy0(absxy,x0,y0,xorg,yorg)
   IF(.NOT.window)THEN
      xmin=grxmin(grcide)
      xmax=grxmax(grcide)
      ymin=grymin(grcide)
      ymax=grymax(grcide)
      CALL grarea(grcide,0.0_pg,0.0_pg,0.0_pg,0.0_pg)
   END IF
!
! Plot the string of characters.
!
   mode=move
   base=0.0_pg
   fac=1.0_pg
   i=1
   level=0
   l=LEN(string)
!     -- DO WHILE (I.LE.L)
10 IF(i <= l)THEN
      IF(i < l.AND.string(i:i) == CHAR(92))THEN
         CALL grtoup(next,string(i+1:i+1))
         IF(next == 'U')THEN
            level=level+1
            base=base+4.0_pg*fac
            fac=0.6_pg**IABS(level)
            i=i+2
         ELSE IF(next == 'D')THEN
            level=level-1
            fac=0.6_pg**ABS(level)
            base=base-4.0_pg*fac
            i=i+2
         ELSE
            i=i+1
         END IF
      ELSE
         ch=ICHAR(string(i:i))
         IF(ch > 127.OR.ch < 0)ch=ICHAR(' ')
         more=.true.
!         -- DO WHILE (MORE)
20       IF(more)THEN
            CALL grgtc0(ch,center,points,xc,yc,more)
            DO  j=1,points
               xt=xc(j)*fac
               yt=yc(j)*fac+base
               xc(j)=xorg+cosa*xt-sina*yt
               yc(j)=yorg+sina*xt+cosa*yt
            END DO
            IF(points == 1)mode=dot
            IF(points > 0)CALL grvct0(mode,.true.,points,xc,yc)
            IF(points == 1)mode=move
            GO TO 20
         END IF
!         -- end DO WHILE
         xorg=xorg+dx*fac
         yorg=yorg+dy*fac
         i=i+1
      END IF
      GO TO 10
   END IF
!     -- end DO WHILE
!
! Clean up and return.
!
   IF(.NOT.window)THEN
      grxmin(grcide)=xmin
      grxmax(grcide)=xmax
      grymin(grcide)=ymin
      grymax(grcide)=ymax
   END IF
   CALL grsls(lstyle)
   RETURN
!
END SUBROUTINE grchr0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRCHSZ -- inquire default character attributes
!+
SUBROUTINE grchsz(ident,xsize,ysize,xspace,yspace)
!
! GRPCKG: Obtain the default character attributes.
!
! Arguments:
!
! IDENT (input, integer): the plot identifier, returned by GROPEN.
! XSIZE, YSIZE (output, real): the default character size
!      (absolute device units).
! XSPACE, YSPACE (output, real): the default character spacing
!      (absolute units); XSPACE is the distance between the lower left
!      corners of adjacent characters in a plotted string; YSPACE
!      is the corresponding vertical spacing.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ident
   REAL(KIND=pg), INTENT(OUT)               :: xsize
   REAL(KIND=pg), INTENT(OUT)               :: ysize
   REAL(KIND=pg), INTENT(OUT)               :: xspace
   REAL(KIND=pg), INTENT(OUT)               :: yspace
!
   REAL(KIND=pg) :: factor
!
   CALL grslct(ident)
   factor=grcscl(ident)
   xsize=grcxsz*factor
   ysize=grcysz*factor
   xspace=10.0_pg*factor
   yspace=13.0_pg*factor
!
END SUBROUTINE grchsz
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRCLIP -- clip a point against clipping rectangle
!+
SUBROUTINE grclip(x,y,xmin,xmax,ymin,ymax,c)
!
! GRPCKG (internal routine): support routine for the clipping algorithm;
! called from GRLIN0 only. C is a 4 bit code indicating the relationship
! between point (X,Y) and the window boundaries; 0 implies the point is
! within the window.
!
! Arguments:
!--
! (11-Feb-1983)
! Revised 20-Jun-1985 (TJP); use floating arithmetic
! Revised 12-Jun-1992 (TJP); clip exactly on the boundary
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)            :: x
   REAL(KIND=pg), INTENT(IN OUT)            :: y
   REAL(KIND=pg), INTENT(IN OUT)            :: xmin
   REAL(KIND=pg), INTENT(IN OUT)            :: xmax
   REAL(KIND=pg), INTENT(IN OUT)            :: ymin
   REAL(KIND=pg), INTENT(IN OUT)            :: ymax
   INTEGER, INTENT(OUT)                     :: c
!
   c=0
   IF(x < xmin)THEN
      c=1
   ELSE IF(x > xmax)THEN
      c=2
   END IF
   IF(y < ymin)THEN
      c=c+4
   ELSE IF(y > ymax)THEN
      c=c+8
   END IF
!
END SUBROUTINE grclip
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRCLOS -- close graphics device
!+
SUBROUTINE grclos
!
! GRPCKG: Close the open plot on the current device. Any pending output
! is sent to the device, the device is released for other users or the
! disk file is closed, and no further plotting is allowed on the device
! without a new call to GROPEN.
!
! Arguments: none.
!--
!  1-Jun-1984 - [TJP].
! 17-Jul-1984 - ignore call if plot is not open [TJP].
!  1-Oct-1984 - reset color to default (1) and position text cursor
!               at bottom of VT screen [TJP].
! 19-Oct-1984 - add VV device [TJP].
! 22-Dec-1984 - use GRBUFL and GRIOTA parameters [TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 21-Feb-1987 - modify END_PICTURE sequence [AFT].
! 11-Jun-1987 - remove built-ins [TJP].
! 31-Aug-1987 - do not eject blank page [TJP].
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=1) :: chr
!
! Check a plot is open.
!
   IF(grcide < 1)RETURN
!
! Reset color to default (1). This is useful
! for VT240 terminals, which use the color tables for text.
!
   CALL grsci(1)
!
! Flush buffer.
!
   CALL grterm
!
! End picture.
!
   CALL grepic
!
! This plot identifier is no longer in use.
! Set state to "workstation closed".
!
   grstat(grcide)=0
   grcide=0
!
! Close workstation.
!
   CALL grexec(grgtyp,10,rbuf,nbuf,chr,lchr)
!
END SUBROUTINE grclos
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRCLPL -- clip line against clipping rectangle
!+
SUBROUTINE grclpl(x0,y0,x1,y1,vis)
!
! GRPCKG (internal routine): Change the end-points of the line (X0,Y0)
! (X1,Y1) to clip the line at the window boundary.  The algorithm is
! that of Cohen and Sutherland (ref: Newman & Sproull).
!
! Arguments:
!
! X0, Y0 (input/output, real): device coordinates of starting point
!       of line.
! X1, Y1 (input/output, real): device coordinates of end point of line.
! VIS (output, logical): .TRUE. if line lies wholly or partially
!       within the clipping rectangle; .FALSE. if it lies entirely
!       outside the rectangle.
!--
! 13-Jul-1984 - [TJP].
! 20-Jun-1985 - [TJP] - revise clipping algorithm.
! 28-Jun-1991 - [TJP] - use IAND().
! 12-Jun-1992 - [TJP] - clip exactly on the boundary.
!
! Caution: IAND is a non-standard intrinsic function to do bitwise AND
! of two integers. If it is not supported by your Fortran compiler, you
! will need to modify this routine or supply an IAND function.
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)            :: x0
   REAL(KIND=pg), INTENT(IN OUT)            :: y0
   REAL(KIND=pg), INTENT(IN OUT)            :: x1
   REAL(KIND=pg), INTENT(IN OUT)            :: y1
   LOGICAL, INTENT(OUT)                     :: vis
!
   INTEGER :: c0,c1,c
   REAL(KIND=pg) :: xmin,xmax,ymin,ymax
   REAL(KIND=pg) :: x,y
   INTEGER :: IAND
!
   xmin=grxmin(grcide)
   ymin=grymin(grcide)
   xmax=grxmax(grcide)
   ymax=grymax(grcide)
   CALL grclip(x0,y0,xmin,xmax,ymin,ymax,c0)
   CALL grclip(x1,y1,xmin,xmax,ymin,ymax,c1)
10 IF(c0 /= 0.OR.c1 /= 0)THEN
      IF(IAND(c0,c1) /= 0)THEN
!
!             ! line is invisible
!
         vis=.false.
         RETURN
      END IF
      c=c0
      IF(c == 0)c=c1
      IF(IAND(c,1) /= 0)THEN
!
!             ! crosses XMIN
!
         y=y0+(y1-y0)*(xmin-x0)/(x1-x0)
         x=xmin
      ELSE IF(IAND(c,2) /= 0)THEN
!
!             ! crosses XMAX
!
         y=y0+(y1-y0)*(xmax-x0)/(x1-x0)
         x=xmax
      ELSE IF(IAND(c,4) /= 0)THEN
!
!             ! crosses YMIN
!
         x=x0+(x1-x0)*(ymin-y0)/(y1-y0)
         y=ymin
      ELSE IF(IAND(c,8) /= 0)THEN
!
!             ! crosses YMAX
!
         x=x0+(x1-x0)*(ymax-y0)/(y1-y0)
         y=ymax
      END IF
      IF(c == c0)THEN
         x0=x
         y0=y
         CALL grclip(x,y,xmin,xmax,ymin,ymax,c0)
      ELSE
         x1=x
         y1=y
         CALL grclip(x,y,xmin,xmax,ymin,ymax,c1)
      END IF
      GO TO 10
   END IF
   vis=.true.
!
   RETURN
!
END SUBROUTINE grclpl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRCTOI -- convert character string to integer
!+
FUNCTION grctoi(s,i)
!
! GRCTOI: attempt to read an integer from a character string, and return
! the result. No attempt is made to avoid integer overflow. A valid
! integer is any sequence of decimal digits.
!
! Returns:
!  GRCTOI           : the value of the integer; if the first character
!                    read is not a decimal digit, the value returned
!                    is zero.
! Arguments:
!  S      (input)  : character string to be parsed.
!  I      (in/out) : on input, I is the index of the first character
!                    in S to be examined; on output, either it points
!                    to the next character after a valid integer, or
!                    it is equal to LEN(S)+1.
!
!--
!  1985 Oct  8 - New routine, based on CTOI (T. J. Pearson).
!  1997 Jun  3 - allow leading + or - sign (TJP).
!-----------------------------------------------------------------------
!
!
   CHARACTER (LEN=*), INTENT(IN)            :: s
   INTEGER, INTENT(IN OUT)                  :: i
!
   INTEGER :: k,SIGN,x,grctoi
   CHARACTER (LEN=1) :: digits(0:9)
!
   DATA digits/'0','1','2','3','4','5','6','7','8','9'/
!
   x=0
   SIGN=+1
   IF(i > LEN(s))GO TO 30
   IF(s(i:i) == '+')THEN
      i=i+1
   ELSE IF(s(i:i) == '-')THEN
      i=i+1
      SIGN=-1
   END IF
10 IF(i > LEN(s))GO TO 30
   DO  k=0,9
      IF(s(i:i) == digits(k))THEN
         x=x*10+k
         i=i+1
         GO TO 10
      END IF
   END DO
30 grctoi=x*SIGN
   RETURN
!
END FUNCTION grctoi
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRCURS -- read cursor position
!+
FUNCTION grcurs(ident,ix,iy,ixref,iyref,mode,posn,ch)
!
! GRPCKG: Read the cursor position and a character typed by the user.
! The position is returned in absolute device coordinates (pixels).
! GRCURS positions the cursor at the position specified, and
! allows the user to move the cursor using the joystick or
! arrow keys or whatever is available on the device. When he has
! positioned the cursor, the user types a single character on his
! keyboard; GRCURS then returns this character and the new cursor
! position.
!
! "Rubber band" feedback of cursor movement can be requested (although
! it may not be supported on some devices). If MODE=1, a line from
! the anchor point to the current cursor position is displayed as
! the cursor is moved. If MODE=2, a rectangle with vertical and
! horizontal sides and one vertex at the anchor point and the opposite
! vertex at the current cursor position is displayed as the cursor is
! moved.
!
! Returns:
!
! GRCURS (integer): 1 if the call was successful; 0 if the device
!      has no cursor or some other error occurs.
!
! Arguments:
!
! IDENT (integer, input):  GRPCKG plot identifier (from GROPEN).
! IX    (integer, in/out): the device x-coordinate of the cursor.
! IY    (integer, in/out): the device y-coordinate of the cursor.
! IXREF (integer, input):  x-coordinate of anchor point.
! IYREF (integer, input):  y-coordinate of anchor point.
! MODE  (integer, input):  type of rubber-band feedback.
! CH    (char,    output): the character typed by the user; if the device
!      has no cursor or if some other error occurs, the value CHAR(0)
!      [ASCII NUL character] is returned.
!--
!  1-Aug-1984 - extensively revised [TJP].
! 29-Jan-1985 - add ARGS and HP2648 devices (?) [KS/TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 11-Jun-1987 - remove built-ins [TJP].
! 15-Feb-1988 - remove test for batch jobs; leave this to the device
!               handler [TJP].
! 13-Dec-1990 - remove code to abort after 10 cursor errors [TJP].
!  7-Sep-1994 - add support for rubber-band modes [TJP].
! 17-Jan-1995 - start picture if necessary [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE  grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ident
   INTEGER, INTENT(IN OUT)                  :: ix
   INTEGER, INTENT(IN OUT)                  :: iy
   INTEGER, INTENT(IN)                      :: ixref
   INTEGER, INTENT(IN)                      :: iyref
   INTEGER, INTENT(IN)                      :: mode
   INTEGER, INTENT(IN)                      :: posn
   CHARACTER (LEN=*), INTENT(OUT)           :: ch
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr,icurs,errcnt,grcurs
   CHARACTER(LEN=16) :: chr
   CHARACTER(LEN=1) :: c
!
   SAVE  errcnt
!
   DATA errcnt/0/
!
! Validate identifier, and select device.
!
   CALL grslct(ident)
   CALL grterm
!
! Begin picture if necessary.
!
   IF(.NOT.grpltd(grcide))CALL grbpic
!
! Make sure cursor is on view surface. (It does not
! have to be in the viewport.)
!
   ix=MAX(0,MIN(grxmxa(grcide),ix))
   iy=MAX(0,MIN(grymxa(grcide),iy))
!
! Does the device have a cursor?
!
   c=grgcap(grcide)(2:2)
   icurs=0
   IF(c == 'C'.OR.c == 'X')icurs=1
!
! Device does have a cursor.
!
   IF(icurs > 0)THEN
!
!         -- initial position of cursor
!
      rbuf(1)=REAL(ix,KIND=pg)
      rbuf(2)=REAL(iy,KIND=pg)
!
!         -- reference point for rubber band
!
      rbuf(3)=REAL(ixref,KIND=pg)
      rbuf(4)=REAL(iyref,KIND=pg)
!
!         -- rubber band mode
!
      rbuf(5)=REAL(mode,KIND=pg)
!
!         -- position cursor?
!
      rbuf(6)=REAL(posn,KIND=pg)
      nbuf=6
      lchr=0
      CALL grexec(grgtyp,17,rbuf,nbuf,chr,lchr)
      ix=INT(rbuf(1))
      iy=INT(rbuf(2))
      ch=chr(1:1)
      grcurs=1
!
!         -- error if driver returns NUL
!
      IF(ICHAR(chr(1:1)) == 0)grcurs=0
!
! Other devices are illegal.
!
   ELSE
      CALL grexec(grgtyp,1,rbuf,nbuf,chr,lchr)
      lchr=INDEX(chr,' ')
      IF(errcnt <= 10)CALL grwarn('output device has no cursor: '//chr(:lchr))
      ch=CHAR(0)
      grcurs=0
      errcnt=errcnt+1
   END IF
!
END FUNCTION grcurs
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRDATE -- get date and time as character string (MS-DOS)
!+
SUBROUTINE grdate(cdate,ldate)
!
! Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
! To receive the whole string, the CDATE should be declared
! CHARACTER*17.
!
! Arguments:
!  CDATE : receives date and time, truncated or extended with
!           blanks as necessary.
!  L      : receives the number of characters in STRING, excluding
!           trailing blanks. This will always be 17, unless the length
!           of the string supplied is shorter.
!--
! 1989-Mar-17 - [AFT]
!-----------------------------------------------------------------------
!
!  JAO:  modified to use the standard FORTRAN date_and_time intrinsic
!
   CHARACTER (LEN=17), INTENT(IN OUT)       :: cdate
   INTEGER, INTENT(OUT)                     :: ldate
!
   CHARACTER (LEN=8) :: date
   CHARACTER (LEN=10) :: time
!
!   CHARACTER (LEN=3) :: cmon(12)
!   INTEGER :: ihr,imin,isec,i100th
!   INTEGER :: iyr,imon,iday
!
!   DATA cmon/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',  &
!      'Oct','Nov','Dec'/
!---
!      CALL GETTIM(IHR, IMIN, ISEC, I100TH)
!      CALL GETDAT(IYR, IMON, IDAY)
!
!   WRITE(cdate,10)iday,cmon(imon),iyr,ihr,imin
!
!
!10 FORMAT(i2,'-',a3,'-',i4,' ',i2,':',i2)
!
   CALL date_and_time(DATE=date,TIME=time)
!
   cdate(1:2)=date(7:8)
   cdate(3:3)='-'
   IF(date(5:6) == '01')cdate(4:6)='Jan'
   IF(date(5:6) == '02')cdate(4:6)='Feb'
   IF(date(5:6) == '03')cdate(4:6)='Mar'
   IF(date(5:6) == '04')cdate(4:6)='Apr'
   IF(date(5:6) == '05')cdate(4:6)='May'
   IF(date(5:6) == '06')cdate(4:6)='Jun'
   IF(date(5:6) == '07')cdate(4:6)='Jul'
   IF(date(5:6) == '08')cdate(4:6)='Aug'
   IF(date(5:6) == '09')cdate(4:6)='Sep'
   IF(date(5:6) == '10')cdate(4:6)='Oct'
   IF(date(5:6) == '11')cdate(4:6)='Nov'
   IF(date(5:6) == '12')cdate(4:6)='Dec'
   cdate(7:7)='-'
   cdate(8:11)=date(1:4)
   cdate(12:12)=' '
   cdate(13:14)=time(1:2)
   cdate(15:15)=':'
   cdate(16:17)=time(3:4)
!
   ldate=17

   RETURN
!
END SUBROUTINE grdate
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRDOT0 -- draw a dot
!+
SUBROUTINE grdot0(x,y)
!
! GRPCKG (internal routine): Draw a single dot (pixel) at a specified
! location.
!
! Arguments:
!
! X, Y (real, input): absolute device coordinates of the dot (these
!       are rounded to the nearest integer by GRDOT0).
!--
! (1-Jun-1984)
! 22-Oct-1984 - rewrite [TJP].
! 29-Jan-1985 - add HP2648 device [KS/TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 21-Feb-1987 - If needed, calls begin picture [AFT].
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: x
   REAL(KIND=pg), INTENT(IN)                         :: y
!
   INTEGER :: nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=1) :: chr
!
! (X,Y) is the new current position.
!
   grxpre(grcide)=x
   grypre(grcide)=y
!
! Check window.
!
   IF(x < grxmin(grcide))RETURN
   IF(x > grxmax(grcide))RETURN
   IF(y < grymin(grcide))RETURN
   IF(y > grymax(grcide))RETURN
!
! Begin picture if necessary.
!
   IF(.NOT.grpltd(grcide))CALL grbpic
!
! If a "thick pen" is to be simulated, use the line-drawing routines
! instead.
!
   IF(grwidt(grcide) > 1)THEN
      CALL grlin3 (x,y,x,y)
   ELSE
      rbuf(1)=x
      rbuf(2)=y
      nbuf=2
      CALL grexec(grgtyp,13,rbuf,nbuf,chr,lchr)
   END IF
!
END SUBROUTINE grdot0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRDOT1 -- draw dots
!+
SUBROUTINE grdot1(points,x,y)
!
! GRPCKG (internal routine): Draw a set of dots.
!
! Arguments:
!
! POINTS (input, integer): the number of coordinate pairs.
! X, Y (input, real arrays, dimensioned POINTS or greater): the
!       X and Y world coordinates of the points.
!--
! 14-Mar-1997 - new routine to optimize drawing many dots [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: points
   REAL(KIND=pg), INTENT(IN)                :: x(points)
   REAL(KIND=pg), INTENT(IN)                :: y(points)
!
   INTEGER :: i,nbuf,lchr
   REAL(KIND=pg) :: rbuf(6),xp,yp
   CHARACTER (LEN=1) :: chr
   EQUIVALENCE (xp,rbuf(1)),(yp,rbuf(2))
!
! Begin picture if necessary.
!
   IF(.NOT.grpltd(grcide))CALL grbpic
!
! Loop for points: driver support.
!
   IF(grwidt(grcide) <= 1)THEN
      nbuf=2
      lchr=0
      DO  i=1,points
!
!        -- Convert to device coordinates
!
         xp=x(i)*grxscl(grcide)+grxorg(grcide)
         yp=y(i)*gryscl(grcide)+gryorg(grcide)
!
!           -- Clip against viewport
!
         IF(xp >= grxmin(grcide).AND.xp <= grxmax(grcide).AND.yp  &
            .GE.grymin(grcide).AND.yp <= grymax(grcide))THEN
            CALL grexec(grgtyp,13,rbuf,nbuf,chr,lchr)
         END IF
      END DO
!
! Thick line emulation required.
!
   ELSE
      DO  i=1,points
!
!        -- Convert to device coordinates
!
         xp=x(i)*grxscl(grcide)+grxorg(grcide)
         yp=y(i)*gryscl(grcide)+gryorg(grcide)
!
!           -- Clip against viewport
!
         IF(xp >= grxmin(grcide).AND.xp <= grxmax(grcide).AND.yp  &
            .GE.grymin(grcide).AND.yp <= grymax(grcide))THEN
            CALL grlin3(xp,yp,xp,yp)
         END IF
      END DO
   END IF
!
! New pen position.
!
   grxpre(grcide)=xp
   grypre(grcide)=yp
!
END SUBROUTINE grdot1
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRDTYP -- decode graphics device type string
!+
FUNCTION grdtyp(text)
!
! GRPCKG (internal routine): determine graphics device type code from
! type name. It compares the argument with the table of known device
! types in common.
!
! Argument:
!
! TEXT (input, character): device type name, eg 'PRINTRONIX'; the name
!       may be abbreviated to uniqueness.
!
! Returns:
!
! GRDTYP (integer): the device type code, in the range 1 to
!       GRTMAX, zero if the type name is not recognised, or -1
!       if the type name is ambiguous.
!--
! 27-Dec-1984 - rewrite so that is doesn't have to be modified for
!               new devices [TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 10-Nov-1995 - ignore drivers that report no device type [TJP].
! 30-Aug-1996 - check for an exact match; indicate if type is
!               ambiguous [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN OUT)        :: text
!
   INTEGER :: code,i,l,match,grdtyp
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: ndev,nbuf,lchr
   INTEGER :: grtrim
   CHARACTER (LEN=32) :: chr
!
   grdtyp=0
   l=grtrim(text)
   IF(l < 1)RETURN
   match=0
   code=0
   CALL grexec(0,0,rbuf,nbuf,chr,lchr)
   ndev=nint(rbuf(1))
   DO  i=1,ndev
      CALL grexec(i,1,rbuf,nbuf,chr,lchr)
      IF(lchr > 0)THEN
         IF(text(1:l) == chr(1:l))THEN
            IF(chr(l+1:l+1) == ' ')THEN
!
!                 -- exact match
!
               grdtyp=i
               grgtyp=grdtyp
               RETURN
            ELSE
               match=match+1
               code=i
            END IF
         END IF
      END IF
   END DO
   IF(match == 0)THEN
!
!        -- no match
!
      grdtyp=0
   ELSE IF(match == 1)THEN
      grdtyp=code
      grgtyp=grdtyp
   ELSE
      grdtyp=-1
   END IF
!
END FUNCTION grdtyp
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GREPIC -- end picture
!+
SUBROUTINE grepic
!
! GRPCKG: End the current picture.
!
! Arguments: none.
!--
! 17-Nov-1994 - [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=1) :: chr
!
! Check a plot is open.
!
   IF(grcide < 1)RETURN
!
! End picture.
!
   IF(grpltd(grcide))THEN
      rbuf(1)=1.
      nbuf=1
      CALL grexec(grgtyp,14,rbuf,nbuf,chr,lchr)
   END IF
   grpltd(grcide)=.false.
!
END SUBROUTINE grepic
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRESC -- escape routine
!+
SUBROUTINE gresc(text)
!
! GRPCKG: "Escape" routine. The specified text is sent directly to the
! selected graphics device, with no interpretation by GRPCKG. This
! routine must be used with care; e.g., the programmer needs to know
! the device type of the currently selected device, and the instructions
! that that device can accept.
!
! Arguments: none.
!  TEXT (input, character*(*)):  text to be sent to the device.
!
! 15-May-1985 - new routine [TJP].
! 26-May-1987 - add GREXEC support [TJP].
! 19-Dec-1988 - start new page if necessary [TJP].
!  4-Feb-1997 - RBUF should be an array, not a scalar [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN OUT)        :: text
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf
!
!   JAO:  add the variable templength
!
   INTEGER :: templength
!
! If no device is currently selected, do nothing.
!
   IF(grcide > 0)THEN
      IF(.NOT.grpltd(grcide))CALL grbpic
      nbuf=0
      rbuf=0.0_pg
!
!   JAO:  use the variable templength instead of LEN(text). The
!         former is a variable and the latter is nonvariable.
!         The grexec subroutine specifies the final argument
!         to be INTENT(IN OUT), and a nonvariable quantity
!         cannot be updated
!
!  CALL grexec(grgtyp,23,rbuf,nbuf,text,LEN(text))
!
      templength=LEN(text)
      CALL grexec(grgtyp,23,rbuf,nbuf,text,templength)
   END IF
!
END SUBROUTINE gresc
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRETXT -- erase text from graphics screen
!+
SUBROUTINE gretxt
!
! GRPCKG: Erase the text screen.  Some graphics devices have
! two superimposed view surfaces, of which one is used for graphics and
! the other for alphanumeric text.  This routine erases the text
! view surface without affecting the graphics view surface. It does
! nothing if there is no text view surface associated with the device.
!
! Arguments: none.
!--
! (1-Feb-1983)
! 16-Oct-1984 - add ID100 device [RSS/TJP].
! 29-Jan-1985 - add HP2648 device [KS/TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 11-Jun-1987 - remove built-in devices [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   CHARACTER (LEN=1) :: chr
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr
!
   IF(grcide >= 1)THEN
      rbuf=0.0_pg
      CALL grexec(grgtyp,18,rbuf,nbuf,chr,lchr)
   END IF
!
END SUBROUTINE gretxt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GREXEC -- PGPLOT device handler dispatch routine
!+
SUBROUTINE grexec(idev,ifunc,rbuf,nbuf,chr,lchr)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idev
   INTEGER, INTENT(IN)                      :: ifunc
   REAL(KIND=pg), INTENT(IN OUT)            :: rbuf(*)
   INTEGER, INTENT(IN OUT)                  :: nbuf
   CHARACTER (LEN=*), INTENT(IN OUT)        :: chr
   INTEGER, INTENT(IN OUT)                  :: lchr
!
! DO NOT MODIFY THIS ROUTINE.
! You should always create a new version by re-executing
! the command file NEWEXEC.COM.
!---
!
   INTEGER, PARAMETER :: ndev=6
   CHARACTER (LEN=10) :: msg
!---
   SELECT CASE(idev)
    CASE (1)
      GO TO 10
    CASE (2)
      GO TO 20
    CASE (3)
      GO TO 30
    CASE (4)
      GO TO 40
    CASE (5)
      GO TO 50
    CASE (6)
      GO TO 60
   END SELECT
!
   IF(idev == 0)THEN
      rbuf(1)=REAL(ndev,KIND=pg)
      nbuf=1
   ELSE
      WRITE(msg,'(I10)') idev
      CALL grquit('Unknown device code in GREXEC: '//msg)
   END IF
   RETURN
!---
10 CALL nudriv(ifunc,rbuf,nbuf,chr,lchr)
   RETURN
20 CALL psdriv(ifunc,rbuf,nbuf,chr,lchr,1)
   RETURN
30 CALL psdriv(ifunc,rbuf,nbuf,chr,lchr,1)
   RETURN
40 CALL psdriv(ifunc,rbuf,nbuf,chr,lchr,2)
   RETURN
50 CALL psdriv(ifunc,rbuf,nbuf,chr,lchr,3)
   RETURN
60 CALL psdriv(ifunc,rbuf,nbuf,chr,lchr,4)
   RETURN
!
END SUBROUTINE grexec
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRFA -- fill area (polygon)
!+
SUBROUTINE grfa(n,px,py)
!
! GRPCKG: FILL AREA: fill a polygon with solid color.  The polygon
! is defined by the (x,y) world coordinates of its N vertices.  If
! this is not a function supported by the device, shading is
! accomplished by drawing horizontal lines spaced by 1 pixel.  By
! selecting color index 0, the interior of the polygon can be erased
! on devices which permit it.  The polygon need not be convex, but if
! it is re-entrant (i.e., edges intersect other than at the vertices),
! it may not be obvious which regions are "inside" the polygon.  The
! following rule is applied: for a given point, create a straight line
! starting at the point and going to infinity. If the number of
! intersections between the straight line and the polygon is odd, the
! point is within the polygon; otherwise it is outside. If the
! straight line passes a polygon vertex tangentially, the
! intersection  count is not affected. The only attribute which applies
! to FILL AREA is color index: line-width and line-style are ignored.
! There is a limitation on the complexity of the polygon: GFA will
! fail if any horizontal line intersects more than 32 edges of the
! polygon.
!
! Arguments:
!
! N (input, integer): the number of vertices of the polygon (at least
!       3).
! PX, PY (input, real arrays, dimension at least N): world coordinates
!       of the N vertices of the polygon.
!--
! 16-Jul-1984 - [TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 21-Feb-1987 - If needed, calls begin picture [AFT].
!  7-Sep-1994 - avoid driver call for capabilities [TJP].
!  1-May-1995 - fixed bug for re-entrant polygons, and optimized code
!               [A.F.Carman].
! 18-Oct-1995 - fixed bug: emulated fill failed for reversed y-axis
!               [S.C.Allendorf/TJP].
!  4-Dec-1995 - remove use of real variable as do-loop variable [TJP].
! 20-Mar-1996 - use another do loop 40 to avoid gaps between adjacent
!               polygons [RS]
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: px(*)
   REAL(KIND=pg), INTENT(IN)                :: py(*)
!
   INTEGER, PARAMETER :: maxsec=32
   INTEGER :: i,j,nsect,lw,ls,nbuf,lchr,line
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=32) :: chr
   REAL(KIND=pg) :: x(maxsec),y,ymin,ymax,dy,yd,temp,s1,s2,t1,t2
   LOGICAL :: forwd
!
   IF(grcide < 1)RETURN
   IF(n < 3)THEN
      CALL grwarn('GRFA - polygon has < 3 vertices.')
      RETURN
   END IF
!
! Devices with polygon fill capability.
!
   IF(grgcap(grcide)(4:4) == 'A')THEN
      IF(.NOT.grpltd(grcide))CALL grbpic
      rbuf(1)=n
      CALL grexec(grgtyp,20,rbuf,nbuf,chr,lchr)
      DO  i=1,n
         rbuf(1)=px(i)*grxscl(grcide)+grxorg(grcide)
         rbuf(2)=py(i)*gryscl(grcide)+gryorg(grcide)
         CALL grexec(grgtyp,20,rbuf,nbuf,chr,lchr)
      END DO
      RETURN
   END IF
!
! For other devices fill area is simulated.
!
! Save attributes.
!
   CALL grqls(ls)
   CALL grqlw(lw)
   CALL grsls(1)
   CALL grslw(1)
!
! Find range of raster-lines to be shaded.
!
   ymin=py(1)*gryscl(grcide)+gryorg(grcide)
   ymax=ymin
   DO  i=2,n
      yd=py(i)*gryscl(grcide)+gryorg(grcide)
      ymin=MIN(ymin,yd)
      ymax=MAX(ymax,yd)
   END DO
   CALL grexec(grgtyp,3,rbuf,nbuf,chr,lchr)
   dy=ABS(rbuf(3))
!
! Find intersections of edges with current raster line.
!
   forwd=.true.
   s1=px(n)*grxscl(grcide)+grxorg(grcide)
   t1=py(n)*gryscl(grcide)+gryorg(grcide)
!
   DO  line=nint(ymin/dy),nint(ymax/dy)
      y=line*dy
      nsect=0
      DO  i=1,n
         s2=px(i)*grxscl(grcide)+grxorg(grcide)
         t2=py(i)*gryscl(grcide)+gryorg(grcide)
         IF((t1 < y.AND.y <= t2).OR.(t1 >= y.AND.y > t2))THEN
            nsect=nsect+1
            IF(nsect > maxsec)THEN
               CALL grwarn('GRFA - polygon is too complex.')
               RETURN
            END IF
            x(nsect)=(s1+(s2-s1)*((y-t1)/(t2-t1)))
         END IF
         s1=s2
         t1=t2
      END DO
!
! Sort the intersections into increasing x order.
!
      DO  i=2,nsect
         DO  j=1,i
            IF(x(j) > x(i))THEN
               temp=x(j)
               x(j)=x(i)
               x(i)=temp
            END IF
         END DO
      END DO
!
! Draw the horizontal line-segments.
!
      grypre(grcide)=y
      IF(forwd)THEN
         DO  i=1,nsect-1,2
            grxpre(grcide)=x(i)
            CALL grlin0(x(i+1),y)
         END DO
         forwd=.false.
      ELSE
         DO  i=nsect,2,-2
            grxpre(grcide)=x(i)
            CALL grlin0(x(i-1),y)
         END DO
         forwd=.true.
      END IF
   END DO
!
! Restore attributes.
!
   CALL grsls(ls)
   CALL grslw(lw)
!
END SUBROUTINE grfa
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRFAO - format character string containing integers
!+
SUBROUTINE grfao(frmat,l,str,v1,v2,v3,v4)
!
! The input string FRMAT is copied to the output string STR with
! the first occurrence of '#' replaced by the value of V1, the second
! by the value of V2, etc.  The length of the resulting string is
! returned in L.
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: frmat
   INTEGER, INTENT(OUT)                     :: l
   CHARACTER (LEN=*), INTENT(OUT)           :: str
   INTEGER, INTENT(IN)                      :: v1
   INTEGER, INTENT(IN)                      :: v2
   INTEGER, INTENT(IN)                      :: v3
   INTEGER, INTENT(IN)                      :: v4
!
   INTEGER :: i,q,val,gritoc
!
   l=0
   q=0
   DO  i=1,LEN(frmat)
      IF(l >= LEN(str))RETURN
      IF(frmat(i:i) /= '#')THEN
         l=l+1
         str(l:l)=frmat(i:i)
      ELSE
         q=q+1
         val=0
         IF(q == 1)val=v1
         IF(q == 2)val=v2
         IF(q == 3)val=v3
         IF(q == 4)val=v4
         l=l+gritoc(val,str(l+1:))
      END IF
   END DO
!
END SUBROUTINE grfao
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRFLUN -- free a Fortran logical unit number (Sun/Convex-UNIX)
!+
SUBROUTINE grflun(lun)
!
! Free a Fortran logical unit number allocated by GRGLUN. [This version
! does nothing.]
!
! Arguments:
!  LUN    : the logical unit number to free.
!--
! 25-Nov-1988
!-----------------------------------------------------------------------
!
   INTEGER, INTENT(IN OUT)                  :: lun
!
   lun=lun+1
!
   RETURN
!
END SUBROUTINE grflun
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRGCOM -- read with prompt from user's terminal (Sun/Convex-UNIX)
!+
FUNCTION grgcom(string,prompt,l)
!
! Issue prompt and read a line from the user's terminal; in VMS,
! this is equivalent to LIB$GET_COMMAND.
!
! Arguments:
!  STRING : (output) receives the string read from the terminal.
!  PROMPT : (input) prompt string.
!  L      : (output) length of STRING.
!
! Returns:
!  GRGCOM : 1 if successful, 0 if an error occurs (e.g., end of file).
!--
! 9-Feb-1988
! 10-Feb-1990 revised to always read from stdin (unit 5), but issue a
!             prompt only when device is a terminal.
!-----------------------------------------------------------------------
!
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN OUT)        :: string
   CHARACTER (LEN=*), INTENT(IN)            :: prompt
   INTEGER, INTENT(OUT)                     :: l
!
   INTEGER :: ier,grgcom
!
   grgcom=0
   l=0
   ier=0
   WRITE(*,'(1X,A)',ADVANCE='NO',IOSTAT=ier)prompt
   IF(ier == 0)READ(*,'(A)',IOSTAT=ier)string
   IF(ier == 0)grgcom=1
   l=LEN(string)
10 IF(string(l:l) /= ' ')GO TO 20
   l=l-1
   GO TO 10
20 CONTINUE
!
END FUNCTION grgcom
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRGENV -- get value of PGPLOT environment parameter (Sun/Convex-UNIX)
!+
SUBROUTINE grgenv(NAME,value,l)
!
! Return the value of a PGPLOT environment parameter. In Sun/Convex-UNIX,
! environment parameters are UNIX environment variables; e.g. parameter
! ENVOPT is environment variable PGPLOT_ENVOPT. Translation is not
! recursive and is case-sensitive.
!
! Arguments:
!  NAME   : (input) the name of the parameter to evaluate.
!  VALUE  : receives the value of the parameter, truncated or extended
!           with blanks as necessary. If the parameter is undefined,
!           a blank string is returned.
!  L      : receives the number of characters in VALUE, excluding
!           trailing blanks. If the parameter is undefined, zero is
!           returned.
!--
! 19-Jan-1988
!-----------------------------------------------------------------------
!
!
   CHARACTER (LEN=*), INTENT(IN)            :: NAME
   CHARACTER (LEN=*), INTENT(IN OUT)        :: value
   INTEGER, INTENT(OUT)                     :: l
!
   INTEGER :: i,lin
   CHARACTER (LEN=32) :: test
!
   test='PGPLOT_'//NAME
   lin=INDEX(test,' ')-1
   CALL get_environment_variable(test(:lin),value)
   IF(value == ' ')THEN
      l=0
   ELSE
      DO  i=LEN(value),1,-1
         l=i
         IF(value(i:i) /= ' ')GO TO 20
      END DO
      l=0
20    CONTINUE
   END IF
!
END SUBROUTINE grgenv
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRGFIL -- find data file
!+
SUBROUTINE grgfil(TYPE,NAME)
!
! This routine encsapsulates the algorithm for finding the PGPLOT
! run-time data files.
!
! 1. The binary font file: try the following in order:
!     file specified by PGPLOT_FONT
!     file "grfont.dat" in directory specified by PGPLOT_DIR
!                       (with or without '/' appended)
!     file "grfont.dat" in directory /usr/local/pgplot/
!
! 2. The color-name database: try the following in order:
!     file specified by PGPLOT_RGB
!     file "rgb.txt" in directory specified by PGPLOT_DIR
!                       (with or without '/' appended)
!     file "rgb.txt" in directory /usr/local/pgplot/
!
! Arguments:
!  TYPE (input)  : either 'FONT' or 'RGB' to request the corresponding
!                  file.
!  NAME (output) : receives the file name.
!--
!  2-Dec-1994 - new routine [TJP].
!-----------------------------------------------------------------------
!
   CHARACTER (LEN=*), INTENT(IN)            :: TYPE
   CHARACTER (LEN=*), INTENT(OUT)           :: NAME
!
   CHARACTER (LEN=*), PARAMETER :: defdir='/usr/local/pgplot/'
   CHARACTER (LEN=*), PARAMETER :: deffnt='grfont.dat'
   CHARACTER (LEN=*), PARAMETER :: defrgb='rgb.txt'
   CHARACTER (LEN=255) :: ff
   CHARACTER (LEN=16) :: deflt
   INTEGER :: i,l,ld
   LOGICAL :: test,debug
!
! Is debug output requested?
!
   CALL grgenv('DEBUG',ff,l)
   debug=l > 0
!
! Which file?
!
   IF(TYPE == 'FONT')THEN
      deflt=deffnt
      ld=LEN(deffnt)
   ELSE IF(TYPE == 'RGB')THEN
      deflt=defrgb
      ld=LEN(defrgb)
   ELSE
      CALL grwarn('Internal error in routine GRGFIL')
   END IF
!
! Try each possibility in turn.
!
   DO  i=1,4
      IF(i == 1)THEN
         CALL grgenv(TYPE,ff,l)
      ELSE IF(i == 2)THEN
         CALL grgenv('DIR',ff,l)
         IF(l > 0)THEN
            ff(l+1:)=deflt
            l=l+ld
         END IF
      ELSE IF(i == 3)THEN
         CALL grgenv('DIR',ff,l)
         IF(l > 0)THEN
            ff(l+1:l+1)='/'
            ff(l+2:)=deflt
            l=l+1+ld
         END IF
      ELSE IF(i == 4)THEN
         ff=defdir//deflt
         l=LEN(defdir)+ld
      END IF
      IF(l > 0)THEN
         IF(debug)THEN
            CALL grwarn('Looking for '//ff(:l))
         END IF
         INQUIRE (FILE=ff(:l),EXIST=test)
         IF(test)THEN
            NAME=ff(:l)
            RETURN
         ELSE IF(debug)THEN
            CALL grwarn('WARNING: file not found')
         END IF
      END IF
   END DO
!
! Failed to find the file.
!
   NAME=deflt
!
END SUBROUTINE grgfil
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRGLUN -- get a Fortran logical unit number (Sun/Convex-UNIX)
!+
SUBROUTINE grglun (lun)
!
! Get an unused Fortran logical unit number.
! Returns a Logical Unit Number that is not currently opened.
! After GRGLUN is called, the unit should be opened to reserve
! the unit number for future calls.  Once a unit is closed, it
! becomes free and another call to GRGLUN could return the same
! number.  Also, GRGLUN will not return a number in the range 1-9
! as older software will often use these units without warning.
!
! Arguments:
!  LUN    : receives the logical unit number, or -1 on error.
!--
! 12-Feb-1989 [AFT/TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: lun
!
   INTEGER :: i
   LOGICAL :: qopen
!---
   DO  i=99,10,-1
      INQUIRE(UNIT=i,OPENED=qopen)
      IF(.NOT.qopen)THEN
         lun=i
         RETURN
      END IF
   END DO
   CALL grwarn('GRGLUN: out of units.')
   lun=-1
   RETURN
!
END SUBROUTINE grglun
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRGMSG -- print system message (Sun/Convex-UNIX)
!+
SUBROUTINE grgmsg(STATUS)
!
! This routine obtains the text of the VMS system message corresponding
! to code STATUS, and displays it using routine GRWARN. On non-VMS
! systems, it just displays the error number.
!
! Argument:
!  STATUS (input): 32-bit system message code.
!--
! 18-Feb-1988
!-----------------------------------------------------------------------
!
   INTEGER, INTENT(IN OUT)                  :: STATUS
!
   CHARACTER (LEN=10) :: buffer
!
   WRITE(buffer,'(I10)') STATUS
!
   CALL grwarn('system message number: '//buffer)
!
END SUBROUTINE grgmsg
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRGRAY -- gray-scale map of a 2D data array
!+
SUBROUTINE grgray(a,idim,jdim,i1,i2,j1,j2,fg,bg,pa,minind,maxind,mode)
!
! This is a device-dependent support routine for PGGRAY.
!
! Draw gray-scale map of an array in current window. Array
! values between FG and BG are shaded in gray levels determined
! by linear interpolation. FG may be either less than or greater
! than BG.  Array values outside the range FG to BG are
! shaded black or white as appropriate.
!
! GRGRAY uses GRIMG0 on devices with enough color indices available.
! Note that it changes the color table to gray-scale.
! Otherwise in does a random dither with GRIMG3.
!
! Arguments:
!  A      (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  FG     (input)  : the array value which is to appear in
!                    foreground color.
!  BG     (input)  : the array value which is to appear in
!                    background color.
!  PA     (input)  : transformation matrix between array grid and
!                    device coordinates (see GRCONT).
!  MODE   (input)  : transfer function.
!--
! 12-Dec-1986 - Speed up plotting [J. Biretta].
!  3-Apr-1987 - Add special code for /PS, /VPS, /GR.
!  2-Sep-1987 - Adapted from PGGRAY [TJP].
!  1-Dec-1988 - Put random-number generator inline [TJP].
!  3-Apr-1989 - Use "line of pixels" primitive where available [TJP].
!  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
! 19-Jan-1990 - Add special code for /CPS, /VCPS [DLM]
!  3-Sep-1992 - Add special code for NULL device [TJP].
! 25-Nov-1992 - Add special code for /NEXT [AFT].
! 17-Mar-1994 - Scale in device coordinates [TJP].
! 31-Aug-1994 - use GRIMG0 when appropriate [TJP].
!  7-Sep-1994 - speed up random dither [TJP].
!  8-Feb-1995 - use color ramp based on color indices 0 and 1 [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   REAL(KIND=pg), INTENT(IN)                :: a(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: fg
   REAL(KIND=pg), INTENT(IN)                :: bg
   REAL(KIND=pg), INTENT(IN)                :: pa(6)
   INTEGER, INTENT(IN)                      :: minind
   INTEGER, INTENT(IN)                      :: maxind
   INTEGER, INTENT(IN)                      :: mode
!
   INTEGER :: i
   REAL(KIND=pg) :: a0,a1,cr0,cg0,cb0,cr1,cg1,cb1
!
!          INTRINSIC  REAL
!
!-----------------------------------------------------------------------
!
! N.B. Arguments are assumed to be valid (checked by PGGRAY).
!
! Use GRIMG0 if this an appropriate device; first initialize the
! color table to a linear ramp between the colors assigned to color
! indices 0 and 1.
!
   IF(grgcap(grcide)(7:7) /= 'N'.AND.maxind-minind > 15)THEN
      CALL grqcr(0,cr0,cg0,cb0)
      CALL grqcr(1,cr1,cg1,cb1)
      DO  i=minind,maxind
         a0=REAL(i-minind,KIND=pg)/REAL(maxind-minind,KIND=pg)
         a1=1.0-a0
         CALL grscr(i,a0*cr0+a1*cr1,a0*cg0+a1*cg1,a0*cb0+a1*cb1)
      END DO
      CALL grimg0(a,idim,jdim,i1,i2,j1,j2,fg,bg,pa,minind,maxind,mode)
      RETURN
!
! Otherwise use random dither in current color index.
!
   ELSE
      CALL grimg3(a,idim,jdim,i1,i2,j1,j2,fg,bg,pa,mode)
   END IF
!
END SUBROUTINE grgray
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRGTC0 -- obtain character digitization
!+
SUBROUTINE grgtc0(CHAR,center,points,x,y,more)
!
! GRPCKG (internal routine): obtain character digitization.
!
! (10-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE grdat2
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: CHAR
   LOGICAL, INTENT(IN)                      :: center
   INTEGER, INTENT(OUT)                     :: points
   REAL(KIND=pg), INTENT(OUT)               :: x(1)
   REAL(KIND=pg), INTENT(OUT)               :: y(1)
   LOGICAL, INTENT(OUT)                     :: more
!
!COMMON /grcs02/ cindx1,cindx2,chtbl
!
   INTEGER :: i
   INTEGER :: coords
   LOGICAL :: tailed
!
!-----------------------------------------------------------------------
!
   IF(cindx2 <= 0)cindx2=CHAR+1
!
! Get the next segment of the character.
!
   points=chtbl(cindx1,cindx2)
   IF(points == 0)GO TO 20
   DO  i=1,points
      cindx1=cindx1+1
      coords=chtbl(cindx1,cindx2)
      tailed=coords < 0
      IF(tailed)coords=IABS(coords)
      x(i)=FLOAT(coords/10)
      y(i)=FLOAT(MOD(coords,10))
      IF(tailed)y(i)=-y(i)
      IF(.NOT.center)CYCLE
      x(i)=x(i)-3.0
      y(i)=y(i)-4.0
   END DO
20 CONTINUE
!
! Set status and return.
!
   IF(cindx1 == ctd1)GO TO 30
   cindx1=cindx1+1
   IF(chtbl(cindx1,cindx2) == 0)GO TO 30
   more=.true.
   RETURN
30 more=.false.
   cindx1=1
   cindx2=0
   RETURN
!
END SUBROUTINE grgtc0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRIMG0 -- color image of a 2D data array
!+
SUBROUTINE grimg0(a,idim,jdim,i1,i2,j1,j2,a1,a2,pa,minind,maxind,mode)
!
! This is a support routine for PGIMAG.
!
! Arguments:
!  A      (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  A1     (input)  : the array value which is to appear in color
!                    index MININD.
!  A2     (input)  : the array value which is to appear in color
!                    index MAXIND.
!  PA     (input)  : transformation matrix between array grid and
!                    device coordinates.
!  MININD (input)  : minimum color index to use.
!  MAXIND (input)  : maximum color index to use.
!  MODE   (input)  : =0 for linear, =1 for logarithmic, =2 for
!                    square-root mapping of array values to color
!                    indices.
!--
!  7-Sep-1994 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: idim
   INTEGER, INTENT(IN)                  :: jdim
   REAL(KIND=pg), INTENT(IN)            :: a(idim,jdim)
   INTEGER, INTENT(IN)                  :: i1
   INTEGER, INTENT(IN)                  :: i2
   INTEGER, INTENT(IN)                  :: j1
   INTEGER, INTENT(IN)                  :: j2
   REAL(KIND=pg), INTENT(IN)            :: a1
   REAL(KIND=pg), INTENT(IN)            :: a2
   REAL(KIND=pg), INTENT(IN)            :: pa(6)
   INTEGER, INTENT(IN)                  :: minind
   INTEGER, INTENT(IN)                  :: maxind
   INTEGER, INTENT(IN)                  :: mode
!
   CHARACTER (LEN=1) :: c
!
!-----------------------------------------------------------------------
!
! Switch on type of device support.
!
   c=grgcap(grcide)(7:7)
   IF(c == 'Q')THEN
!
!         -- Image-primitive devices
!
      CALL grimg1(a,idim,jdim,i1,i2,j1,j2,a1,a2,pa,minind,maxind,mode)
   ELSE IF(c == 'P')THEN
!
!         -- Pixel-primitive devices
!
      CALL grimg2(a,idim,jdim,i1,i2,j1,j2,a1,a2,pa,minind,maxind,mode)
   ELSE IF(c == 'N')THEN
!
!         -- Other devices
!
      CALL grwarn('images cannot be displayed on the selected device')
   ELSE
!
!         -- Unknown device code
!
      CALL grwarn('unexpected error in routine GRIMG0')
   END IF
!
END SUBROUTINE grimg0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRIMG1 -- image of a 2D data array (image-primitive devices)
!+
SUBROUTINE grimg1(a,idim,jdim,i1,i2,j1,j2,a1,a2,pa,minind,maxind,mode)
!
! (This routine is called by GRIMG0.)
!--
! 7-Sep-1994  New routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: idim
   INTEGER, INTENT(IN)                  :: jdim
   REAL(KIND=pg), INTENT(IN)            :: a(idim,jdim)
   INTEGER, INTENT(IN)                  :: i1
   INTEGER, INTENT(IN)                  :: i2
   INTEGER, INTENT(IN)                  :: j1
   INTEGER, INTENT(IN)                  :: j2
   REAL(KIND=pg), INTENT(IN)            :: a1
   REAL(KIND=pg), INTENT(IN)            :: a2
   REAL(KIND=pg), INTENT(IN)            :: pa(6)
   INTEGER, INTENT(IN)                  :: minind
   INTEGER, INTENT(IN)                  :: maxind
   INTEGER, INTENT(IN)                  :: mode
!
   INTEGER :: nbuf,lchr
   REAL(KIND=pg) :: rbuf(21),fac,av,sfacl
   CHARACTER (LEN=1) :: chr
   INTEGER :: i,j,ii,nxp,nyp,iv
   REAL(KIND=pg), PARAMETER :: sfac=65000.0
!
!INTRINSIC  nint,LOG
!
!
! Size of image.
!
   nxp=i2-i1+1
   nyp=j2-j1+1
   rbuf=0.0_pg
   rbuf(1)=0.0_pg
   rbuf(2)=REAL(nxp,KIND=pg)
   rbuf(3)=REAL(nyp,KIND=pg)
!
! Clipping rectangle.
!
   rbuf(4)=grxmin(grcide)
   rbuf(5)=grxmax(grcide)
   rbuf(6)=grymin(grcide)
   rbuf(7)=grymax(grcide)
!
! Image transformation matrix.
!
   fac=pa(2)*pa(6)-pa(3)*pa(5)
   rbuf(8)=pa(6)/fac
   rbuf(9)=(-pa(5))/fac
   rbuf(10)=(-pa(3))/fac
   rbuf(11)=pa(2)/fac
   rbuf(12)=(pa(3)*pa(4)-pa(1)*pa(6))/fac-(i1-0.5_pg)
   rbuf(13)=(pa(5)*pa(1)-pa(4)*pa(2))/fac-(j1-0.5_pg)
!
! Send setup info to driver.
!
   IF(.NOT.grpltd(grcide))CALL grbpic
   CALL grterm
   nbuf=13
   lchr=0
   CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
!
! Convert image array to color indices and send to driver.
!
   sfacl=LOG(1.0+sfac)
   ii=0
   DO  j=j1,j2
      DO  i=i1,i2
         av=a(i,j)
         IF(a2 > a1)THEN
            av=MIN(a2,MAX(a1,av))
         ELSE
            av=MIN(a1,MAX(a2,av))
         END IF
         IF(mode == 0)THEN
            iv=nint((minind*(a2-av)+maxind*(av-a1))/(a2-a1))
         ELSE IF(mode == 1)THEN
            iv=minind+nint((maxind-minind)*LOG(1.0_pg+sfac*ABS((av-a1)/  &
               (a2-a1)))/sfacl)
         ELSE IF(mode == 2)THEN
            iv=minind+nint((maxind-minind)*SQRT(ABS((av-a1)/(a2-a1))))
         ELSE
            iv=minind
         END IF
         ii=ii+1
         rbuf(ii+1)=iv
         IF(ii == 20)THEN
            nbuf=ii+1
            rbuf(1)=ii
            CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
            ii=0
         END IF
      END DO
   END DO
   IF(ii > 0)THEN
      nbuf=ii+1
      rbuf(1)=ii
      CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
      ii=0
   END IF
!
! Send termination code to driver.
!
   nbuf=1
   rbuf(1)=-1.0_pg
   CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
!
END SUBROUTINE grimg1
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRIMG2 -- image of a 2D data array (pixel-primitive devices)
!+
SUBROUTINE grimg2(a,idim,jdim,i1,i2,j1,j2,a1,a2,pa,minind,maxind,mode)
!
! (This routine is called by GRIMG0.)
!--
! 7-Sep-1994  New routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: idim
   INTEGER, INTENT(IN)                  :: jdim
   REAL(KIND=pg), INTENT(IN)            :: a(idim,jdim)
   INTEGER, INTENT(IN)                  :: i1
   INTEGER, INTENT(IN)                  :: i2
   INTEGER, INTENT(IN)                  :: j1
   INTEGER, INTENT(IN)                  :: j2
   REAL(KIND=pg), INTENT(IN)            :: a1
   REAL(KIND=pg), INTENT(IN)            :: a2
   REAL(KIND=pg), INTENT(IN)            :: pa(6)
   INTEGER, INTENT(IN)                  :: minind
   INTEGER, INTENT(IN)                  :: maxind
   INTEGER, INTENT(IN)                  :: mode
!
   INTEGER :: i,iv,ix,ix1,ix2,iy,iy1,iy2,j,npix,lchr
   REAL(KIND=pg) :: den,av,sfacl
   REAL(KIND=pg) :: xxaa,xxbb,yyaa,yybb,xyaa,xybb,yxaa,yxbb,xyaaiy,yxaaiy
   REAL(KIND=pg) :: buffer(1026)
   CHARACTER (LEN=1) :: chr
   REAL(KIND=pg), PARAMETER :: sfac=65000.0_pg
!
!   JAO:  add the integer tempint
!
   INTEGER  ::  tempint
!
!INTRINSIC  nint,LOG
!
! Location of current window in device coordinates.
!
   ix1=nint(grxmin(grcide))+1
   ix2=nint(grxmax(grcide))-1
   iy1=nint(grymin(grcide))+1
   iy2=nint(grymax(grcide))-1
!
! Transformation from array coordinates to device coordinates.
!
   den=pa(2)*pa(6)-pa(3)*pa(5)
   xxaa=(-pa(6))*pa(1)/den
   xxbb=pa(6)/den
   xyaa=(-pa(3))*pa(4)/den
   xybb=pa(3)/den
   yyaa=(-pa(2))*pa(4)/den
   yybb=pa(2)/den
   yxaa=(-pa(5))*pa(1)/den
   yxbb=pa(5)/den
!
! Start a new page if necessary.
!
   IF(.NOT.grpltd(grcide))CALL grbpic
!
! Run through every device pixel (IX, IY) in the current window and
! determine which array pixel (I,J) it falls in.
!
   sfacl=LOG(1.0_pg+sfac)
   DO  iy=iy1,iy2
      xyaaiy=xxaa-xyaa-xybb*iy
      yxaaiy=yyaa+yybb*iy-yxaa
      npix=0
      buffer(2)=REAL(iy,KIND=pg)
      DO  ix=ix1,ix2
         i=nint(xyaaiy+xxbb*ix)
         IF(i < i1.OR.i > i2)CYCLE
         j=nint(yxaaiy-yxbb*ix)
         IF(j < j1.OR.j > j2)CYCLE
!
!           -- determine color index IV of this pixel
!
         av=a(i,j)
         IF(a2 > a1)THEN
            av=MIN(a2,MAX(a1,av))
         ELSE
            av=MIN(a1,MAX(a2,av))
         END IF
         IF(mode == 0)THEN
            iv=nint((minind*(a2-av)+maxind*(av-a1))/(a2-a1))
         ELSE IF(mode == 1)THEN
            iv=minind+nint((maxind-minind)*LOG(1.0_pg+sfac*ABS((av-a1)/  &
               (a2-a1)))/sfacl)
         ELSE IF(mode == 2)THEN
            iv=minind+nint((maxind-minind)*SQRT(ABS((av-a1)/(a2-a1))))
         ELSE
            iv=minind
         END IF
!
         IF(npix <= 1024)THEN
!
!               -- drop pixels if buffer too small (to be fixed!)
!
            npix=npix+1
            IF(npix == 1)buffer(1)=REAL(ix,KIND=pg)
            buffer(npix+2)=REAL(iv,KIND=pg)
         END IF
      END DO
!
!   JAO:  Use the variable tempint instead of the fix expression
!         npix+2.  That dummy argument is defined INTENT(IN OUT)
!         in the subroutine and the expression npix+2 cannot
!         be updated
!
!  IF(npix > 0)CALL grexec(grgtyp,26,buffer,npix+2,chr,lchr)
!
      tempint=npix+2
      IF(npix > 0)CALL grexec(grgtyp,26,buffer,tempint,chr,lchr)
   END DO
!
END SUBROUTINE grimg2
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRIMG3 -- gray-scale map of a 2D data array, using dither
!+
SUBROUTINE grimg3(a,idim,jdim,i1,i2,j1,j2,black,white,pa,mode)
!--
! 2-Sep-1994 - moved from GRGRAY [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: idim
   INTEGER, INTENT(IN)                  :: jdim
   REAL(KIND=pg), INTENT(IN)            :: a(idim,jdim)
   INTEGER, INTENT(IN)                  :: i1
   INTEGER, INTENT(IN)                  :: i2
   INTEGER, INTENT(IN)                  :: j1
   INTEGER, INTENT(IN)                  :: j2
   REAL(KIND=pg), INTENT(IN)            :: black
   REAL(KIND=pg), INTENT(IN)            :: white
   REAL(KIND=pg), INTENT(IN)            :: pa(6)
   INTEGER, INTENT(IN)                  :: mode
!
   INTEGER :: i,ix,ix1,ix2,iy,iy1,iy2,j
   REAL(KIND=pg) :: den,value,bw
   REAL(KIND=pg) :: xxaa,xxbb,yyaa,yybb,xyaa,xybb,yxaa,yxbb,xyaaiy,yxaaiy
   INTEGER :: jran,ilast,jlast,ixstep,iystep
   REAL(KIND=pg) :: rand,facl
   INTEGER, PARAMETER :: m=714025
   INTEGER, PARAMETER :: iaa=1366
   INTEGER, PARAMETER :: icc=150889
   REAL(KIND=pg), PARAMETER :: rm=1.0_pg/REAL(m,KIND=pg)
   REAL(KIND=pg), PARAMETER :: fac=65000.0_pg
!
!INTRINSIC  MOD,nint,REAL,LOG
!
   IF(mode < 0.OR.mode > 2)RETURN
!
! Initialize random-number generator (based on RAN2 of Press et al.,
! Numerical Recipes)
!
   jran=76773
!
   ix1=nint(grxmin(grcide))+1
   ix2=nint(grxmax(grcide))-1
   iy1=nint(grymin(grcide))+1
   iy2=nint(grymax(grcide))-1
   den=pa(2)*pa(6)-pa(3)*pa(5)
!
! Calculate constants.
!
   bw=ABS(black-white)
   facl=LOG(1.0_pg+fac)
   xxaa=(-pa(6))*pa(1)/den
   xxbb=pa(6)/den
   xyaa=(-pa(3))*pa(4)/den
   xybb=pa(3)/den
   yyaa=(-pa(2))*pa(4)/den
   yybb=pa(2)/den
   yxaa=(-pa(5))*pa(1)/den
   yxbb=pa(5)/den
!
! Choose step size: at least 1/200 inch, assuming the line-width
! unit is 1/200 inch.
!
   ixstep=MAX(1,nint(grwidt(grcide)*grpxpi(grcide)/200.0_pg))
   iystep=MAX(1,nint(grwidt(grcide)*grpypi(grcide)/200.0_pg))
!
! Draw dots.
!
   ilast=0
   jlast=0
   DO  iy=iy1,iy2,iystep
      xyaaiy=xxaa-xyaa-xybb*iy
      yxaaiy=yyaa+yybb*iy-yxaa
      DO  ix=ix1,ix2,ixstep
         i=nint(xyaaiy+xxbb*ix)
         IF(i < i1.OR.i > i2)CYCLE
         j=nint(yxaaiy-yxbb*ix)
         IF(j < j1.OR.j > j2)CYCLE
         IF(i /= ilast.OR.j /= jlast)THEN
            ilast=i
            jlast=j
            value=ABS(a(i,j)-white)/bw
            IF(mode == 0)THEN
!
!                     -- "linear"
!
            ELSE IF(mode == 1)THEN
!
!                     -- "logarithmic"
!
               value=LOG(1.0_pg+fac*value)/facl
            ELSE IF(mode == 2)THEN
!
!                     -- "square root"
!
               value=SQRT(value)
            END IF
         END IF
         jran=MOD(jran*iaa+icc,m)
         rand=jran*rm
         IF(value > rand)CALL grdot0(REAL(ix,KIND=pg),REAL(iy,KIND=pg))
      END DO
   END DO
!
END SUBROUTINE grimg3
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRINIT -- initialize GRPCKG
!+
SUBROUTINE grinit
!
! Initialize GRPCKG and read font file. Called by GROPEN, but may be
! called explicitly if needed.
!--
! 29-Apr-1996 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
   USE grsy00
!
   IMPLICIT NONE
!
   INTEGER :: i
   LOGICAL :: init
   SAVE  init
   DATA init/.true./
!
   IF(init)THEN
      DO  i=1,grimax
         grstat(i)=0
      END DO
!  CALL grsy00
      init=.false.
   END IF
   RETURN
!
END SUBROUTINE grinit
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRINQFONT -- inquire current font [obsolete]
!
SUBROUTINE grinqfont(IF)
!
   INTEGER, INTENT(IN OUT)                  :: IF
!
   CALL grqfnt(IF)
!
END SUBROUTINE grinqfont
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRINQLI -- *obsolete routine*
!+
SUBROUTINE grinqli(inten)
!
! GRPCKG: obtain the line intensity of the current graphics device.
! Obsolete routine.
! Argument:
!
! INTEN (integer, output): always returns 1.
!--
! (1-Feb-1983; revised 16-Aug-1987).
!-----------------------------------------------------------------------
!
   INTEGER, INTENT(OUT)                     :: inten
!
   inten=1
!
END SUBROUTINE grinqli
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRINQPEN -- *obsolete routine*
!+
SUBROUTINE grinqpen(ip)
!
! GRPCKG: obtain the pen number of the current graphics device.
! Obsolete routine.
! Argument:
!
! IP (integer, output): always receives 1.
!--
! 16-Aug-1987 - [TJP].
!-----------------------------------------------------------------------
!
   INTEGER, INTENT(OUT)                     :: ip
!
   ip=1
!
END SUBROUTINE grinqpen
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRITOC - convert integer to character string
!+
FUNCTION gritoc(INT,str)
!
! Convert integer INT into (decimal) character string in STR.
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: INT
   CHARACTER (LEN=*), INTENT(OUT)           :: str
!
   CHARACTER (LEN=10) :: digits
   INTEGER :: d,i,intval,j,l,gritoc
   CHARACTER (LEN=1) :: k
   DATA digits/'0123456789'/
!
   intval=ABS(INT)
   i=0
!
! Generate digits in reverse order.
!
10 i=i+1
   d=1+MOD(intval,10)
   str(i:i)=digits(d:d)
   intval=intval/10
   IF(i < LEN(str).AND.intval /= 0)GO TO 10
!
! Add minus sign if necessary.
!
   IF(INT < 0.AND.i < LEN(str))THEN
      i=i+1
      str(i:i)='-'
   END IF
   gritoc=i
!
! Reverse string in place.
!
   l=i/2
   DO  j=1,l
      k=str(i:i)
      str(i:i)=str(j:j)
      str(j:j)=k
      i=i-1
   END DO
!
END FUNCTION gritoc
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLDEV -- list supported device types
!+
SUBROUTINE grldev
!
! Support routine for PGLDEV.
!
! Arguments: none
!--
!  5-Aug-1986 [AFT]
! 13-Dec-1990 Change warnings to messages [TJP].
! 18-Jan-1993 Display one per line [TJP].
! 13-Jan-1995 Change message [TJP].
! 10-Nov-1995 Ignore device types of zero length [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   INTEGER :: i,ndev,nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=72) :: chr
   CHARACTER (LEN=72) :: text
!---
   CALL grmsg('Device types available:')
!
!--- First obtain number of devices.
!
   rbuf=0.0_pg
   CALL grexec(0,0,rbuf,nbuf,chr,lchr)
   ndev=nint(rbuf(1))
!
   DO  i=1,ndev
      CALL grexec(i,1,rbuf,nbuf,chr,lchr)
      IF(lchr > 0)THEN
         text(1:1)='/'
         text(2:)=chr(:lchr)
         CALL grmsg(text)
      END IF
   END DO
!
END SUBROUTINE grldev
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLEN -- inquire plotted length of character string
!+
SUBROUTINE grlen(string,d)
!
! GRPCKG: length of text string (absolute units)
!--
! (3-Mar-1983)
! 19-Jan-1988 - remove unused label [TJP].
!  9-Sep-1989 - standardize [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: string
   REAL(KIND=pg), INTENT(OUT)               :: d
!
   LOGICAL :: unused
   INTEGER :: xygrid(300)
   INTEGER :: list(256)
!
   REAL(KIND=pg) :: factor,cosa,sina,dx, ratio,fntbas,fntfac
   INTEGER :: i,ifntlv,lx,nlist
!
!INTRINSIC  ABS,LEN
!
   d=0.0_pg
   IF(LEN(string) <= 0)RETURN
!
!-----------------------------------------------------------------------
!               Compute scaling and orientation
!-----------------------------------------------------------------------
!
   factor=grcfac(grcide)/2.5_pg
   ratio=grpxpi(grcide)/grpypi(grcide)
   cosa=factor
   sina=0.0_pg
   fntbas=0.0_pg
   fntfac=1.0_pg
   ifntlv=0
!
!               Convert string to symbol numbers:
!               \u and \d escape sequences are converted to -1,-2
!
   CALL grsyds(list,nlist,string,grcfnt(grcide))
!
!               Plot the string of characters
!
   DO  i=1,nlist
      IF(list(i) < 0)THEN
         IF(list(i) == -1)THEN
            ifntlv=ifntlv+1
            fntbas=fntbas+16.0_pg*fntfac
            fntfac=0.6_pg**ABS(ifntlv)
         ELSE IF(list(i) == -2)THEN
            ifntlv=ifntlv-1
            fntfac=0.6_pg**ABS(ifntlv)
            fntbas=fntbas-16.0_pg*fntfac
         END IF
         CYCLE
      END IF
      CALL grsyxd(list(i),xygrid,unused)
      lx=xygrid(5)-xygrid(4)
      dx=cosa*lx*ratio
      d=d+dx*fntfac
   END DO
!
END SUBROUTINE grlen
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLGTR -- translate logical name (dummy version)
!+
SUBROUTINE grlgtr(NAME)
!
! Recursive translation of a logical name.
! This is used in the parsing of device specifications in the
! VMS implementation of PGPLOT. In other implementations, it may
! be replaced by a null routine.
!
! Argument:
!  NAME (input/output): initially contains the name to be
!       inspected.  If an equivalence is found it will be replaced
!       with the new name. If not, the old name will be left there.
!--
! 19-Dec-1994
!-----------------------------------------------------------------------
!
   CHARACTER (LEN=*), INTENT(IN OUT)        :: name
   name=TRIM(name)
!
END SUBROUTINE grlgtr
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLIN0 -- draw a line
!+
SUBROUTINE grlin0(xp,yp)
!
! GRPCKG (internal routine): draw a line from the current position to a
! specified position, which becomes the new current position. This
! routine takes care of clipping at the viewport boundary, dashed and
! thick lines.
!
! Arguments:
!
! XP, YP (input, real): absolute device coordinates of the end-point of
!       the line.
!--
! 13-Jul-1984
!  7-May-1985 - add MIN/MAX kluge to prevent integer overflow [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: xp
   REAL(KIND=pg), INTENT(IN)                     :: yp
!
   LOGICAL :: vis
   REAL(KIND=pg) :: x0,y0,x1,y1
!
! End-points of line are (X0,Y0), (X1,Y1).
!
   x0=grxpre(grcide)
   y0=grypre(grcide)
   x1=MIN(2.0E+09_pg,MAX(-2.0E+09_pg,xp))
   y1=MIN(2.0E+09_pg,MAX(-2.0E+09_pg,yp))
   grxpre(grcide)=x1
   grypre(grcide)=y1
!
! Change the end-points of the line (X0,Y0) - (X1,Y1)
! to clip the line at the window boundary.
!
   CALL grclpl(x0,y0,x1,y1,vis)
   IF(.NOT.vis)RETURN
!
! Draw the line in the appropriate style.
!
   IF(grdash(grcide))THEN
!
!         ! dashed line
!
      CALL grlin1(x0,y0,x1,y1,.false.)
   ELSE IF(grwidt(grcide) > 1)THEN
!
!         ! heavy line
!
      CALL grlin3(x0,y0,x1,y1)
   ELSE
!
!         ! full line
!
      CALL grlin2(x0,y0,x1,y1)
   END IF
!
END SUBROUTINE grlin0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLIN1 -- draw a dashed line
!+
SUBROUTINE grlin1(x0,y0,x1,y1,reset)
!
! GRPCKG : dashed line. Generate a visible dashed line between points
! (X0,Y0) and (X1,Y1) according to the dash pattern stored in common.
! If RESET = .TRUE., the pattern will start from the beginning.
! Otherwise, it will continue from its last position.
!     DASHED LINE PATTERN ARRAY CONTAINING LENGTHS OF
!          MARKS AND SPACES IN UNIT CUBE: GRPATN(*)
!     OFFSET IN CURRENT PATTERN SEGMENT: GRPOFF
!     CURRENT PATTERN SEGMENT NUMBER: GRIPAT
!     NUMBER OF PATTERN SEGMENTS: 8
!--
! (1-Feb-1983)
!  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)            :: x0
   REAL(KIND=pg), INTENT(IN)            :: y0
   REAL(KIND=pg), INTENT(IN)            :: x1
   REAL(KIND=pg), INTENT(IN)            :: y1
   LOGICAL, INTENT(IN)                  :: reset
!
!REAL(KIND=pg) :: arg1,arg2,alfarg,adjust
!
   REAL(KIND=pg) :: scale,seglen,ds,dsold
   REAL(KIND=pg) :: alpha1,alpha2,xp,yp,xq,yq

   INTEGER :: thick
!
!INTRINSIC  ABS,MIN,MOD,REAL,SQRT
!
!  replace statement fuction with internal at bottom
!
!adjust(arg1,arg2,alfarg)=alfarg*(arg2-arg1)+arg1
!
   thick=grwidt(grcide)
   scale=SQRT(REAL(ABS(thick),KIND=pg))
   IF(reset)THEN
      grpoff(grcide)=0.0_pg
      gripat(grcide)=1
   END IF
   seglen=SQRT((x1-x0)**2+(y1-y0)**2)
   IF(ABS(seglen)  <= EPSILON(seglen))RETURN
   ds=0.0_pg
!
!       Repeat until (ALPHA2 .GE. 1.0)
!
!       Line segments matching the pattern segments are determined
!       by finding values (ALPHA1,ALPHA2) defining the start and end
!       of the segment in the parametric equation (1-ALPHA)*P1 + ALPHA*P2
!       defining the line.  DS measures the progress along the line
!       segment and defines the starting ALPHA1.  The ending ALPHA2
!       is computed from the end of the current pattern mark or space
!       or the segment end, whichever comes first.
!
10 dsold=ds
   alpha1=ds/seglen
   alpha2=MIN(1.0_pg,(ds+scale*grpatn(grcide,gripat(grcide))-  &
      grpoff(grcide))/seglen)
   IF(MOD(gripat(grcide),2) /= 0)THEN
      xp=adjust(x0,x1,alpha1)
      yp=adjust(y0,y1,alpha1)
      xq=adjust(x0,x1,alpha2)
      yq=adjust(y0,y1,alpha2)
      IF(thick > 1)THEN
         CALL grlin3(xp,yp,xq,yq)
      ELSE
         CALL grlin2(xp,yp,xq,yq)
      END IF
   END IF
   ds=alpha2*seglen
   IF(alpha2 >= 1.0)THEN
      grpoff(grcide)=grpoff(grcide)+ds-dsold
      RETURN
   END IF
   gripat(grcide)=MOD(gripat(grcide),8)+1
   grpoff(grcide)=0.0_pg
   GO TO 10
!
CONTAINS
!
   FUNCTION adjust(arg1,arg2,alfarg)
!
      USE accur
      IMPLICIT NONE
!
      REAL(KIND=pg), INTENT(IN)  :: arg1
      REAL(KIND=pg), INTENT(IN)  :: arg2
      REAL(KIND=pg), INTENT(IN)  :: alfarg
!
      REAL(KIND=pg) :: adjust
!
      adjust=alfarg*(arg2-arg1)+arg1
!
   END FUNCTION adjust
!
END SUBROUTINE grlin1
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLIN2 -- draw a normal line
!+
SUBROUTINE grlin2(x0,y0,x1,y1)
!
! GRPCKG : plot a visible line segment in absolute coords from
! (X0,Y0) to (X1,Y1).  The endpoints of the line segment are rounded
! to the nearest integer and passed to the appropriate device-specific
! routine. It is assumed that the entire line-segment lies within the
! view surface, and that the physical device coordinates are
! non-negative.
!--
! (1-Jun-1984)
! 19-Oct-1984 - rewritten for speed [TJP].
! 29-Jan-1985 - add HP2648 device [KS/TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 21-Feb-1987 - If needed, calls begin picture [AFT].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: x0
   REAL(KIND=pg), INTENT(IN)                         :: y0
   REAL(KIND=pg), INTENT(IN)                         :: x1
   REAL(KIND=pg), INTENT(IN)                         :: y1
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=1) :: chr
!
!- If this is first thing plotted then set something plotted flag
!- and for a GREXEC device call BEGIN_PICTURE.
!
   IF(.NOT.grpltd(grcide))CALL grbpic
!---
   rbuf(1)=x0
   rbuf(2)=y0
   rbuf(3)=x1
   rbuf(4)=y1
   nbuf=4
!
!     WRITE(*,'(A,4F10.5)') 'GRLIN2',RBUF(1), RBUF(2), RBUF(3), RBUF(4)
!
   CALL grexec(grgtyp,12,rbuf,nbuf,chr,lchr)
!
END SUBROUTINE grlin2
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLIN3 -- draw a thick line (multiple strokes)
!+
SUBROUTINE grlin3(x0,y0,x1,y1)
!
! GRPCKG: draw a heavy line from (X0,Y0) to (X1,Y1) by making multiple
! strokes.  In order to simulate a thick pen, the line drawn has
! circular, rather than square, end points.  If this is not done,
! thick letters and other figures have an abnormal and unpleasant
! appearance.
!
! Vocabulary:
!
! LINEWT: the number of strokes required to draw the line; if
!       this is odd, one stroke will lie along the requested vector.
!       The nominal line thickness is (LINEWT-1)*0.005 in.
! RSQURD: the square of the semi-line thickness.
! (DX,DY): the vector length of the line.
! (VX,VY): a vector of length 1 pixel in the direction of the line.
! (VY,-VX): a vector of length 1 pixel perpendicular to (VX,VY).
! OFF: the offset parallel to (VY,-VX) of the K'th stroke.
! (VXK,VYK): the vector increment of the K'th stroke to allow for the
!       semi-circular terminal on the line.
! (PXK,PYK): the vector offset of the K'th stroke perpendicular to the
!       line vector.
!--
! (1-Feb-1983)
! 23-Nov-1994 - change algorithm so that the unit of line-width is
!               0.005 inch instead of 1 pixel [TJP].
! March 1995 - added ABS to prevent domain error in SQRT (CTD)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                   :: x0
   REAL(KIND=pg), INTENT(IN)                   :: y0
   REAL(KIND=pg), INTENT(IN)                   :: x1
   REAL(KIND=pg), INTENT(IN)                   :: y1

   INTEGER :: k,linewt
   REAL(KIND=pg) :: dx,dy,hk,off,pxk,pyk,rsqurd,vlen,vx,vy,vxk,vyk
   REAL(KIND=pg) :: xs0,xs1,ys0,ys1,spix,spiy
!
   LOGICAL :: vis
!
! Determine number of strokes and line thickness.
!
   linewt=grwidt(grcide)
   rsqurd=((linewt-1)**2)*0.25_pg
!
! Determine the vectors (VX,VY), (VY,-VX). If the line-length is zero,
! pretend it is a very short horizontal line.
!
   dx=x1-x0
   dy=y1-y0
   vlen=SQRT(dx**2+dy**2)
   spix=grpxpi(grcide)*0.005_pg
   spiy=grpypi(grcide)*0.005_pg
!
   IF(ABS(vlen) <= EPSILON(vlen))THEN
      vx=spix
      vy=0.0_pg
   ELSE
      vx=dx/vlen*spix
      vy=dy/vlen*spiy
   END IF
!
! Draw LINEWT strokes. We have to clip again in case thickening the
! line has taken us outside the window.
!
   off=(linewt-1)*0.5_pg
   DO  k=1,linewt
      pxk=vy*off
      pyk=-(vx*off)
      hk=SQRT(ABS(rsqurd-off**2))
      vxk=vx*hk
      vyk=vy*hk
      xs1=x1+pxk+vxk
      ys1=y1+pyk+vyk
      xs0=x0+pxk-vxk
      ys0=y0+pyk-vyk
      CALL grclpl(xs1,ys1,xs0,ys0,vis)
      IF(vis)CALL grlin2(xs1,ys1,xs0,ys0)
      off=off-1.0_pg
   END DO
!
END SUBROUTINE grlin3
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLINA -- draw a line (absolute, world coordinates)
!+
SUBROUTINE grlina(x,y)
!
! GRPCKG: draw line from current position to a specified position.
!
! Arguments:
!
! X, Y (real, input): world coordinates of the end-point of the line.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: x
   REAL(KIND=pg), INTENT(IN)                     :: y
!
   IF(grcide >= 1)THEN
!
!         WRITE (*,'(A,2F10.5)') 'GRLINA', X, Y
!
      CALL grlin0(x*grxscl(grcide)+grxorg(grcide),y*  &
         gryscl(grcide)+gryorg(grcide))
   END IF
!
END SUBROUTINE grlina
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRLINR -- draw a line (relative, world coordinates)
!+
SUBROUTINE grlinr(dx,dy)
!
! GRPCKG: draw a line from the current position by a specified
! relative displacement.
!
! Arguments:
!
! DX, DY (real, input): the displacement in world coordinates: the pen
!       position is incremented by DX in x and DY in y.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)              :: dx
   REAL(KIND=pg), INTENT(IN OUT)              :: dy
!
   IF(grcide >= 1)THEN
      CALL grlin0(dx*grxscl(grcide)+grxpre(grcide),dy*  &
         gryscl(grcide)+grypre(grcide))
   END IF
!
END SUBROUTINE grlinr
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRMARK -- mark points with specified symbol
!+
SUBROUTINE grmark(ident,center,symbol,absxy,points,x,y)
!
! GRPCKG: mark a sequence of points with a specified symbol. The
! plot is windowed in the current subarea.
!
! Arguments:
!
! IDENT (integer, input): plot identifier from GROPEN.
! CENTER (input, logical): if .TRUE. the symbol is centered on the point,
!      otherwise the bottom left corner is placed at the point.
! SYMBOL (byte or integer, input): code number of symbol in range 0-127
!      (ASCII character or special symbol); if SYMBOL is outside this
!      range, nothing is plotted.
! ABSXY (logical, input): if .TRUE. (X,Y) are absolute (device)
!      coordinates; otherwise they are world coordinates and the
!      scaling transformation is applied.
! POINTS (integer, input): the number of points; if POINTS is less than
!      or equal to 0, nothing is plotted.
! X,Y (real arrays, dimension at least POINTS, input): the coordinate
!      pairs; if POINTS=1, these may be scalars instead of arrays.
!
! (9-Mar-1983)
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: ident
   LOGICAL, INTENT(IN)                  :: center
   INTEGER, INTENT(IN)                  :: symbol
   LOGICAL, INTENT(IN)                  :: absxy
   INTEGER, INTENT(IN)                  :: points
   REAL(KIND=pg), INTENT(IN)            :: x(*)
   REAL(KIND=pg), INTENT(IN)            :: y(*)
!
   CHARACTER(LEN=1) :: mark
   INTEGER :: i
!
   IF(points <= 0.OR.symbol < 0.OR.symbol > 127)RETURN
   CALL grslct(ident)
   mark=CHAR(symbol)
   DO  i=1,points
      CALL grchr0(.true.,center,0.0_pg,absxy,x(i),y(i),mark)
   END DO
!
   RETURN
!
END SUBROUTINE grmark
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE grmcur(ich,icx,icy)
!
! Cursor movement:
! Input: ICH character code
! In/Out: ICX, ICY cursor position
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ich
   INTEGER, INTENT(OUT)                     :: icx
   INTEGER, INTENT(OUT)                     :: icy
!
   INTEGER :: step
   SAVE  step
   DATA step/4/
!
!     Up arrow or keypad 8:
!
   IF(ich == -1.OR.ich == -28)THEN
      icy=icy+step
!
!     Down arrow or keypad 2:
!
   ELSE IF(ich == -2.OR.ich == -22)THEN
      icy=icy-step
!
!     Right arrow or keypad 6:
!
   ELSE IF(ich == -3.OR.ich == -26)THEN
      icx=icx+step
!
!     Left arrow or keypad 4:
!
   ELSE IF(ich == -4.OR.ich == -24)THEN
      icx=icx-step
!
!     Keypad 7 (left and up):
!
   ELSE IF(ich == -27)THEN
      icx=icx-step
      icy=icy+step
!
!     Keypad 9 (right and up):
!
   ELSE IF(ich == -29)THEN
      icx=icx+step
      icy=icy+step
!
!     Keypad 3 (right and down):
!
   ELSE IF(ich == -23)THEN
      icx=icx+step
      icy=icy-step
!
!     Keypad 1 (left and down):
!
   ELSE IF(ich == -21)THEN
      icx=icx-step
      icy=icy-step
!
!     PF1:
!
   ELSE IF(ich == -11)THEN
      step=1
!
!     PF2:
!
   ELSE IF(ich == -12)THEN
      step=4
!
!     PF3:
!
   ELSE IF(ich == -13)THEN
      step=16
!
!     PF4:
!
   ELSE IF(ich == -14)THEN
      step=64
   END IF
!
END SUBROUTINE grmcur
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRMKER -- draw graph markers
!+
SUBROUTINE grmker(symbol,absxy,n,x,y)
!
! GRPCKG: Draw a graph marker at a set of points in the current
! window. Line attributes (color, intensity, and  thickness)
! apply to markers, but line-style is ignored. After the call to
! GRMKER, the current pen position will be the center of the last
! marker plotted.
!
! Arguments:
!
! SYMBOL (input, integer): the marker number to be drawn. Numbers
!       0-31 are special marker symbols; numbers 32-127 are the
!       corresponding ASCII characters (in the current font). If the
!       number is >127, it is taken to be a Hershey symbol number.
!       If -ve, a regular polygon is drawn.
! ABSXY (input, logical): if .TRUE., the input corrdinates (X,Y) are
!       taken to be absolute device coordinates; if .FALSE., they are
!       taken to be world coordinates.
! N (input, integer): the number of points to be plotted.
! X, Y (input, real arrays, dimensioned at least N): the (X,Y)
!       coordinates of the points to be plotted.
!--
! (19-Mar-1983)
! 20-Jun-1985 - revise to window markers whole [TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
!  1-Aug-1988 - add direct use of Hershey number [TJP].
! 15-Dec-1988 - standardize [TJP].
! 17-Dec-1990 - add polygons [PAH/TJP].
! 12-Jun-1992 - [TJP]
! 22-Sep-1992 - add support for hardware markers [TJP].
!  1-Sep-1994 - suppress driver call [TJP].
! 15-Feb-1994 - fix bug (expanding viewport!) [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: symbol
   LOGICAL, INTENT(IN)                      :: absxy
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: x(*)
   REAL(KIND=pg), INTENT(IN)                :: y(*)
!
   INTEGER :: c
   LOGICAL :: unused,visble
   INTEGER :: i,k,lstyle,lx,ly,lxlast,lylast,symnum,nv
   INTEGER :: xygrid(300)
   REAL(KIND=pg) :: angle,cosa,sina,factor,ratio
   REAL(KIND=pg) :: xcur,ycur,xorg,yorg
   REAL(KIND=pg) :: theta,xoff(40),yoff(40),xp(40),yp(40)
   REAL(KIND=pg) :: xmin,xmax,ymin,ymax
   REAL(KIND=pg) :: xminx,xmaxx,yminx,ymaxx
   REAL(KIND=pg) :: rbuf(4)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=32) :: chr
!
! Check that there is something to be plotted.
!
   IF(n <= 0)RETURN
!
! Check that a device is selected.
!
   IF(grcide < 1)THEN
      CALL grwarn('GRMKER - no graphics device is active.')
      RETURN
   END IF
!
   xmin=grxmin(grcide)
   xmax=grxmax(grcide)
   ymin=grymin(grcide)
   ymax=grymax(grcide)
   xminx=xmin-0.01_pg
   xmaxx=xmax+0.01_pg
   yminx=ymin-0.01_pg
   ymaxx=ymax+0.01_pg
!
! Does the device driver do markers (only markers 0-31 at present)?
!
   IF(grgcap(grcide)(10:10) == 'M'.AND.symbol >= 0.AND.symbol <= 31)THEN
      IF(.NOT.grpltd(grcide))CALL grbpic
!
!         -- symbol number
!
      rbuf(1)=REAL(symbol,KIND=pg)
!
!          -- scale factor
!
      rbuf(4)=grcfac(grcide)/2.5_pg
      nbuf=4
      lchr=0
      DO  k=1,n
!
!             -- convert to device coordinates
!
         CALL grtxy0(absxy,x(k),y(k),xorg,yorg)
!
!             -- is the marker visible?
!
         CALL grclip(xorg,yorg,xminx,xmaxx,yminx,ymaxx,c)
         IF(c == 0)THEN
            rbuf(2)=xorg
            rbuf(3)=yorg
            CALL grexec(grgtyp,28,rbuf,nbuf,chr,lchr)
         END IF
      END DO
      RETURN
   END IF
!
! Otherwise, draw the markers here.
!
! Save current line-style, and set style "normal".
!
   CALL grqls(lstyle)
   CALL grsls(1)
!
! Save current viewport, and open the viewport to include the full
! view surface.
!
   CALL grarea(grcide,0.0_pg,0.0_pg,0.0_pg,0.0_pg)
!
! Compute scaling and orientation.
!
   angle=0.0_pg
   factor=grcfac(grcide)/2.5_pg
   ratio=grpxpi(grcide)/grpypi(grcide)
   cosa=factor*COS(angle)
   sina=factor*SIN(angle)
!
! Convert the supplied marker number SYMBOL to a symbol number and
! obtain the digitization.
!
   IF(symbol >= 0)THEN
      IF(symbol > 127)THEN
         symnum=symbol
      ELSE
         CALL grsymk(symbol,grcfnt(grcide),symnum)
      END IF
      CALL grsyxd(symnum,xygrid,unused)
!
! Positive symbols.
!
      DO  i=1,n
         CALL grtxy0(absxy,x(i),y(i),xorg,yorg)
         CALL grclip(xorg,yorg,xminx,xmaxx,yminx,ymaxx,c)
         IF(c /= 0)CYCLE
         visble=.false.
         k=4
         lxlast=-64
         lylast=-64
20       k=k+2
         lx=xygrid(k)
         ly=xygrid(k+1)
         IF(ly == -64)CYCLE
         IF(lx == -64)THEN
            visble=.false.
         ELSE
            IF((lx /= lxlast).OR.(ly /= lylast))THEN
               xcur=xorg+(cosa*lx-sina*ly)*ratio
               ycur=yorg+(sina*lx+cosa*ly)
               IF(visble)THEN
                  CALL grlin0(xcur,ycur)
               ELSE
                  grxpre(grcide)=xcur
                  grypre(grcide)=ycur
               END IF
            END IF
            visble=.true.
            lxlast=lx
            lylast=ly
         END IF
         GO TO 20
      END DO
!
! Negative symbols.
!
   ELSE
!
!         ! negative symbol: filled polygon of radius 8
!
      nv=MIN(31,MAX(3,ABS(symbol)))
      DO  i=1,nv
         theta=3.141592653589793_pg*(REAL(2*(i-1),KIND=pg)/REAL(nv,KIND=pg) &
            +0.5_pg)-angle
         xoff(i)=COS(theta)*factor*ratio/grxscl(grcide)*8.0_pg
         yoff(i)=SIN(theta)*factor/gryscl(grcide)*8.0_pg
      END DO
      DO  k=1,n
         CALL grtxy0(absxy,x(k),y(k),xorg,yorg)
         CALL grclip(xorg,yorg,xminx,xmaxx,yminx,ymaxx,c)
         IF(c == 0)THEN
            DO  i=1,nv
               xp(i)=x(k)+xoff(i)
               yp(i)=y(k)+yoff(i)
            END DO
            CALL grfa(nv,xp,yp)
         END IF
      END DO
   END IF
!
! Set current pen position.
!
   grxpre(grcide)=xorg
   grypre(grcide)=yorg
!
! Restore the viewport and line-style, and return.
!
   grxmin(grcide)=xmin
   grxmax(grcide)=xmax
   grymin(grcide)=ymin
   grymax(grcide)=ymax
   CALL grsls(lstyle)
!
   RETURN
!
END SUBROUTINE grmker
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRMOVA -- move pen (absolute, world coordinates)
!+
SUBROUTINE grmova(x,y)
!
! GRPCKG: move the pen to a specified location.
!
! Arguments:
!
! X, Y (real, input): world coordinates of the new pen position.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   REAL(KIND=pg), INTENT(IN)                  :: x
   REAL(KIND=pg), INTENT(IN)                  :: y
!
   IF(grcide >= 1)THEN
!
!         WRITE (*,'(A,2F10.5)') 'GRMOVA', X, Y
!
      grxpre(grcide)=x*grxscl(grcide)+grxorg(grcide)
      grypre(grcide)=y*gryscl(grcide)+gryorg(grcide)
   END IF

!
END SUBROUTINE grmova
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRMOVR -- move pen (relative, world coordinates)
!+
SUBROUTINE grmovr(dx,dy)
!
! GRPCKG: move the pen through a specified displacement.
!
! Arguments:
!
! DX, DY (real, input): the displacement in world coordinates: the pen
!       position is incremented by DX in x and DY in y.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                  :: dx
   REAL(KIND=pg), INTENT(IN)                  :: dy
!
   IF(grcide >= 1)THEN
      grxpre(grcide)=grxpre(grcide)+dx*grxscl(grcide)
      grypre(grcide)=grypre(grcide)+dy*gryscl(grcide)
   END IF
!
END SUBROUTINE grmovr
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRMSG -- issue message to user
!+
SUBROUTINE grmsg(text)
!
! Display a message on standard output.
!
! Argument:
!  TEXT (input): text of message to be printed (the string
!      may not be blank).
!--
!  8-Nov-1994 [TJP].
!-----------------------------------------------------------------------
!
   CHARACTER (LEN=*), INTENT(IN)        :: text
!
   INTEGER :: grtrim
!
   IF(text /= ' ')THEN
      WRITE(*,'(1X,A)')text(1:grtrim(text))
   END IF
!
END SUBROUTINE grmsg
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE grnu00(ifunc,state)
!
! PGPLOT NULL device driver: report error
!-----------------------------------------------------------------------
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: ifunc
   INTEGER, INTENT(IN)                  :: state
!
   INTEGER :: l
   CHARACTER (LEN=80) :: msg
!
   CALL grfao('++ internal error: driver in state # for opcode # ', &
      l,msg,state,ifunc,0,0)
   CALL grwarn(msg(1:l))
   RETURN
!
END SUBROUTINE grnu00
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GROPEN -- open device for graphics
!+
FUNCTION gropen(TYPE,dummy,FILE,ident)
!
! GRPCKG: assign a device and prepare for plotting.  GROPEN must be
! called before all other calls to GRPCKG routines.
!
! Returns:
!
! GROPEN (output, integer): 1 => success, any other value
!       indicates a failure (usually the value returned will
!       be a VMS error code). In the event of an error, a
!       message will be sent to the standard error unit.
!
! Arguments:
!
! TYPE (input, integer): default device type (integer code).
! DUMMY (input, integer): not used at present.
! FILE (input, character): plot specifier, of form 'device/type'.
! IDENT (output, integer): plot identifier to be used in later
!       calls to GRPCKG.
!
!  1-Jun-1984 - [TJP].
!  2-Jul-1984 - change to call GRSLCT [TJP].
! 13-Jul-1984 - add device initialization [TJP].
! 23-Jul-1984 - add /APPEND qualifier.
! 19-Oct-1984 - add VV device [TJP].
! 26-Dec-1984 - obtain default file name from common [TJP].
! 29-Jan-1985 - add HP2648 device [KS/TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 12-Oct-1986 - fix bug causing GREXEC to erase screen [AFT].
!  3-Jun-1987 - remove declaration of exit handler [TJP].
! 15-Dec-1988 - standardize [TJP].
! 25-Jun-1989 - remove code that removes spaces from the device name
!               [TJP].
! 26-Nov-1990 - [TJP].
!  5-Jan-1993 - [TJP].
!  1-Sep-1994 - store device capabilities in common for later use [TJP].
! 17-Apr-1995 - zero-length string fix [TJP].
!  6-Jun-1995 - explicitly initialize GRSTAT [TJP].
! 29-Apr-1996 - moved initialization into GRINIT [TJP].
! 12-Jul-1999 - fix bug [TJP].
!-----------------------------------------------------------------------
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: TYPE
   INTEGER, INTENT(IN)                      :: dummy
   CHARACTER (LEN=*), INTENT(IN)            :: FILE
   INTEGER, INTENT(OUT)                     :: ident
!
   INTEGER :: ier,ftype,nbuf,lchr,gropen,dummy1
   INTEGER :: grpars,grtrim
   REAL(KIND=pg) :: rbuf(6)
   LOGICAL :: APPEND
   CHARACTER (LEN=90) :: ffile
   CHARACTER (LEN=128) :: chr
!
! Initialize GRPCKG; read font file (if necessary).
!
   dummy1=dummy+1
   rbuf=0.0_pg
   CALL grinit
!
! Allocate an identifier.
!
   ident=1
10 IF(grstat(ident) /= 0)THEN
      ident=ident+1
      IF(ident > grimax)THEN
         CALL grwarn('Too many active plots.')
         gropen=-1
         ident=0
         RETURN
      END IF
      GO TO 10
   END IF
!
! Validate the device specification.
!
   ier=grpars(FILE,ffile,ftype,APPEND)
   IF(ier /= 1)THEN
      chr='Invalid device specification: '
      chr(31:)=FILE
      CALL grwarn(chr)
      gropen=-1
      RETURN
   END IF
   IF(ftype == 0)ftype=TYPE
   IF(1 <= ftype)THEN
      grtype(ident)=ftype
   ELSE
      chr='Device type omitted or invalid: '
      chr(33:)=FILE
      CALL grwarn(chr)
      gropen=-1
      RETURN
   END IF
!
! Install the file name, or assign default.
!
   IF(ffile == ' ')THEN
      CALL grexec(grtype(ident),5,rbuf,nbuf,ffile,lchr)
   END IF
   grfile(ident)=ffile
   grfnln(ident)=MAX(1,grtrim(grfile(ident)))
!
! Open workstation.
!
   rbuf(3)=0.0_pg
   IF(APPEND)rbuf(3)=1.0_pg
   nbuf=3
   CALL grexec(grgtyp,9,rbuf,nbuf,grfile(ident),grfnln(ident))
   gropen=INT(rbuf(2))
   IF(gropen /= 1)THEN
      ident=0
      RETURN
   END IF
   grgtyp=grtype(ident)
   grunit(ident)=INT(rbuf(1))
   grpltd(ident)=.false.
   grstat(ident)=1
   CALL grslct(ident)
!
! Install the default plot parameters
!
!--- Inquire color-index range.
!
   CALL grexec(grgtyp,2,rbuf,nbuf,chr,lchr)
   grmnci(ident)=INT(rbuf(5))
   grmxci(ident)=INT(rbuf(6))
!
!--- Inquire resolution.
!
   CALL grexec(grgtyp,3,rbuf,nbuf,chr,lchr)
   grpxpi(ident)=INT(rbuf(1))
   grpypi(ident)=INT(rbuf(2))
!
!--- Inquire default character size.
!
   CALL grexec(grgtyp,7,rbuf,nbuf,chr,lchr)
   grcscl(ident)=INT(rbuf(1))
   grcfac(ident)=INT(rbuf(1))
!
!--- Inquire default plot size.
!
   CALL grexec(grgtyp,6,rbuf,nbuf,chr,lchr)
   grxmxa(ident)=INT(rbuf(2))
   grymxa(ident)=INT(rbuf(4))
   grxmin(ident)=INT(rbuf(1))
   grxmax(ident)=INT(rbuf(2))
   grymin(ident)=INT(rbuf(3))
   grymax(ident)=INT(rbuf(4))
!
!--- Inquire device capabilities.
!
   grgcap(ident)='NNNNNNNNNNN'
   CALL grexec(grgtyp,4,rbuf,nbuf,chr,lchr)
   IF(lchr > LEN(grgcap(ident)))lchr=LEN(grgcap(ident))
   grgcap(ident)(1:lchr)=chr(:lchr)
!
!--- Current pen position.
!
   grxpre(ident)=0.0_pg
   grypre(ident)=0.0_pg
!
!--- GRSETS has not been called.
!
   gradju(ident)=.false.
!
!---Default scaling.
!
   CALL grtrn0(0.0_pg,0.0_pg,1.0_pg,1.0_pg)
!
! Default attributes.
!  text font (normal)
!  color (white)
!  line-style (full)
!  line-width (minimum)
!  marker number (dot)
!
   grcfnt(ident)=1
   grccol(ident)=1
   grstyl(ident)=1
   grwidt(ident)=1
   grcmrk(ident)=1
   grdash(ident)=.false.
!
   gropen=1
!
   RETURN
!
END FUNCTION gropen
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GROPTX -- open input/output text file [Unix]
!+
FUNCTION groptx(UNIT,NAME,defnam,mode)
!
! Input:
!  UNIT : Fortran unit number to use
!  NAME : name of file to create
!  DEFNAM : default file name (used to fill in missing fields for VMS)
!  MODE : 0 to open for reading, 1 to open for writing.
!
! Returns:
!  0 => success; any other value => error.
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: unit
   CHARACTER (LEN=*), INTENT(IN)            :: NAME
   CHARACTER (LEN=*), INTENT(IN)            :: defnam
   INTEGER, INTENT(IN)                      :: mode
!
   INTEGER :: ier,groptx
!
   IF(defnam == ' ')THEN
      ier=0
   ELSE
      ier=0
   END IF
   IF(mode == 1)THEN
      OPEN(UNIT=unit,FILE=NAME,STATUS='UNKNOWN',IOSTAT=ier)
   ELSE
      OPEN(UNIT=unit,FILE=NAME,STATUS='OLD',IOSTAT=ier)
   END IF
   groptx=ier
!
END FUNCTION groptx
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPAGE -- end picture
!+
SUBROUTINE grpage
!
! GRPCKG: Advance the plotting area to a new page. For video devices,
! this amounts to erasing the screen; for hardcopy devices, the plot
! buffer is written to the output file followed by a form-feed to
! advance the paper to the start of the next page.
!
! Arguments: none.
!--
!  3-Jun-1983 - [TJP].
! 18-Feb-1984 - remove unnecessary 'T' initialization of VT125, and add
!               S(G1) for Rainbow REGIS [TJP].
!  1-Jun-1984 - add type GMFILE [TJP].
!  2-Jul-1984 - change initialization of VT125 for color [TJP].
! 13-Jul-1984 - move initialization of VT125 and Grinnell to GROPEN
!               [TJP].
! 19-Oct-1984 - add VV device [TJP].
! 29-Jan-1985 - add HP2648 terminal [KS/TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 21-Feb-1987 - fix GREXEC end picture sequence [AFT].
! 11-Jun-1987 - remove built-in devices [TJP].
! 11-Feb-1992 - update veiew surface size: it may have changed! [TJP].
!  5-Jan-1993 - but only if GRSETS has not been called! [TJP]
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER :: nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
!
   CHARACTER (LEN=1) :: chr
!
! Flush the buffer.
!
   CALL grterm
!
! Erase the text screen (if there is one).
!
   CALL gretxt
!
! End picture.
!
   CALL grepic
!
! Update the view surface size: it may have changed (on windowing
! devices)
!
   rbuf=0.0_pg
   IF(.NOT.gradju(grcide))THEN
      CALL grexec(grgtyp,6,rbuf,nbuf,chr,lchr)
      grxmxa(grcide)=INT(rbuf(2))
      grymxa(grcide)=INT(rbuf(4))
   END IF
!
END SUBROUTINE grpage
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPARS -- parse device specification string
!+
FUNCTION grpars(spec,dev,TYPE,APPEND)
!
! GRPCKG: decode a device-specification; called by GROPEN.
!
! Returns:
!  GRPARS (output): 1 if the device-specification is
!       acceptable; any other value indicates an error.
!
! Arguments:
!  SPEC (input): the device specification.
!  DEV  (output):  device name or file spec.
!  TYPE (output): device type (integer code); 0 if no device
!       type is specified.
!  APPEND (output): .TRUE. if /APPEND specified, .FALSE. otherwise.
!--
! 23-Jul-1984 - [TJP].
! 19-Feb-1988 - allow device part to be quoted [TJP].
! 30-Mar-1989 - remove logical translation of device and type [TJP].
! 17-Jun-1991 - ignore comments after ' (' [TJP].
! 19-Dec-1994 - rewritten to scan backwards [TJP].
!  6-Jun-1995 - correct a zero-length string problem [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: spec
   CHARACTER (LEN=*), INTENT(OUT)           :: dev
   INTEGER, INTENT(OUT)                     :: TYPE
   LOGICAL, INTENT(OUT)                     :: APPEND
!
   CHARACTER (LEN=32) :: ctype,upper
   CHARACTER (LEN=6) :: appstr
   CHARACTER (LEN=256) :: descr
   INTEGER :: grdtyp,grtrim,grpars
   INTEGER :: l,lc,ls
   DATA appstr/'APPEND'/
!
! Default results.
!
   dev=' '
   TYPE=0
   APPEND=.false.
   grpars=1
   ctype=' '
!
! Null string is acceptable.
!
   IF(LEN(spec) < 1)RETURN
   IF(spec == ' ')RETURN
!
! On systems where it is possible, perform a "logical name" translation.
!
   descr=spec
   CALL grlgtr (descr)
!
! Discard trailing blanks: L is length of remainder.
!
   l=grtrim(descr)
!
! Find last slash in string (position LS or 0).
!
   ls=l
10 IF(descr(ls:ls) /= '/')THEN
      ls=ls-1
      IF(ls > 0)GO TO 10
   END IF
!
! Check for /APPEND qualifier; if present, look again for type.
!
   IF(ls > 0)THEN
      ctype=descr(ls+1:l)
      CALL grtoup(upper,ctype)
      ctype=upper
      IF(ctype == appstr)THEN
         APPEND=.true.
         l=ls-1
         ls=l
20       IF(descr(ls:ls) /= '/')THEN
            ls=ls-1
            IF(ls > 0)GO TO 20
         END IF
      ELSE
         APPEND=.false.
      END IF
   END IF
!
! If LS=0 there is no type field: use PGPLOT_TYPE.
!
   IF(ls == 0)THEN
      CALL grgenv('TYPE',ctype,lc)
   ELSE
      ctype=descr(ls+1:l)
      lc=l-ls
      l=ls-1
   END IF
!
! Check for allowed type.
!
   IF(lc > 0)THEN
      CALL grtoup(upper,ctype)
      ctype=upper
      TYPE=grdtyp(ctype)
      IF(TYPE == 0)CALL grwarn('Unrecognized device type')
      IF(TYPE == -1)CALL grwarn('Device type is ambiguous')
   ELSE
      TYPE=0
      CALL grwarn('Device type omitted')
   END IF
   IF(TYPE == 0)grpars=grpars+2
!
! Remove quotes from device if necessary.
!
   IF(l >= 1)THEN
      IF(descr(1:1) == '"'.AND.descr(l:l) == '"')THEN
         dev=descr(2:l-1)
         l=l-2
      ELSE
         dev=descr(1:l)
      END IF
   END IF
!
!      write (*,*) 'Device = [', DEV(1:L), ']'
!      write (*,*) 'Type   = [', CTYPE, ']', TYPE
!      write (*,*) 'APPEND = ', APPEND
!
   RETURN
!
END FUNCTION grpars
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPIXL -- solid-fill multiple rectangular areas
!+
SUBROUTINE grpixl(ia,idim,jdim,i1,i2,j1,j2,x1,x2,y1,y2)
!
! Determine the size of each rectangular element. If it is equal
! to the device pen width and the device supports pixel primitives,
! use pixel primitives. Otherwise, if the size is smaller than the
! device pen width emulate pixel output by plotting points. If the
! size is larger than the device pen width, emulate by outputting
! solid-filled rectangles.
!
! Arguments:
!  IA     (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  X1, Y1 (input)  : world coordinates of one corner of the output
!                    region
!  X2, Y2 (input)  : world coordinates of the opposite corner of the
!                    output region
!--
! 18-Jan-1991 - [Ge van Geldorp]
! 31-Mar-1993 - Include color PostScript GRPXPS [Remko Scharroo]
!  4-Apr-1993 - New version of GRPXPS incorporated
!  4-Aug-1993 - Debugging
!  7-Sep-1994 - Revised for v5.0 [TJP].
! 24-Jan-1996 - GRXMIN etc changed to REAL as required in grpckg1.inc [RS]
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   INTEGER, INTENT(IN)                      :: ia(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: x1
   REAL(KIND=pg), INTENT(IN)                :: x2
   REAL(KIND=pg), INTENT(IN)                :: y1
   REAL(KIND=pg), INTENT(IN)                :: y2
!
   REAL(KIND=pg) :: rbuf(3)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=32) :: chr
   REAL(KIND=pg) :: xll,yll,xur,yur
   REAL(KIND=pg) :: xmin,ymin,xmax,ymax,xpix,ypix
   REAL(KIND=pg) :: width,xsize,ysize
   INTEGER :: il,ir,jb,jt
!
   IF(grcide < 1)RETURN
!
! Convert to device coordinates
!
   CALL grtxy0(.false.,x1,y1,xll,yll)
   CALL grtxy0(.false.,x2,y2,xur,yur)
   xmin=MIN(xll,xur)
   xmax=MAX(xll,xur)
   ymin=MIN(yll,yur)
   ymax=MAX(yll,yur)
!
! Check if completely outside clipping region
!
   IF(xmax < grxmin(grcide).OR.grxmax(grcide) < xmin.OR.ymax  &
      .LT.grymin(grcide).OR.grymax(grcide) < ymin)RETURN
!
! Don't paint "pixels" completely before left clipping boundary
!
   xpix=xmax-xmin
   ypix=ymax-ymin
   IF(xmin < grxmin(grcide))THEN
      il=i1+INT((grxmin(grcide)-xmin)*REAL((i2-i1+1),KIND=pg)/xpix)
      xmin=xmin+(xpix*(il-i1))/(i2-i1+1)
   ELSE
      il=i1
   END IF
!
! Don't paint "pixels" completely after right clipping boundary
!
   IF(grxmax(grcide) < xmax)THEN
      ir=i2-INT((xmax-grxmax(grcide))*REAL((i2-i1+1),KIND=pg)/xpix)+1
      xmax=xmin+(xpix*(ir-i1+1))/(i2-i1+1)
   ELSE
      ir=i2
   END IF
!
! Don't paint "pixels" completely under bottom clipping boundary
!
   IF(ymin < grymin(grcide))THEN
      jb=j1+INT((grymin(grcide)-ymin)*REAL((j2-j1+1),KIND=pg)/ypix)
      ymin=ymin+(ypix*(jb-j1))/(j2-j1+1)
   ELSE
      jb=j1
   END IF
!
! Don't paint "pixels" completely above top clipping boundary
!
   IF(grymax(grcide) < ymax)THEN
      jt=j2-INT((ymax-grymax(grcide))*REAL((j2-j1+1),KIND=pg)/ypix)+1
      ymax=ymin+(ypix*(jt-j1+1))/(j2-j1+1)
   ELSE
      jt=j2
   END IF
!
! If device accepts image primitives, use GRPXPS
!
   IF(grgcap(grcide)(7:7) == 'Q')THEN
      CALL grpxps(ia,idim,jdim,il,ir,jb,jt,xmin,xmax,ymin,ymax)
      RETURN
   END IF
!
! Check against pen width
!
   CALL grexec(grgtyp,3,rbuf,nbuf,chr,lchr)
   width=rbuf(3)
   xsize=(i2-i1+1)*width
   ysize=(j2-j1+1)*width
   xpix=xmax-xmin+1
   ypix=ymax-ymin+1
!
! Use rectangles if "pixel" is too large
!
   IF(xpix > xsize+0.5_pg*width.OR.ypix > ysize+0.5_pg*width)THEN
!
!     write (6,*) 'GRPXRE'
!
      CALL grpxre(ia,idim,jdim,il,ir,jb,jt,xmin,xmax,ymin,ymax)
!
! Use either pixel primitives or points
!
   ELSE
!
! Clip pixels lying more than 50% outside clipping boundaries
!
      IF(xmin < grxmin(grcide)-0.5_pg*width)THEN
         xmin=xmin+xpix/(ir-il+1)
         il=il+1
      END IF
      IF(grxmax(grcide)+0.5_pg*width < xmax)THEN
         xmax=xmax-xpix/(ir-il+1)
         ir=ir-1
      END IF
      IF(ymin < grymin(grcide)-0.5_pg*width)THEN
         ymin=ymin+ypix/(jt-jb+1)
         jb=jb+1
      END IF
      IF(grymax(grcide)+0.5_pg*width < ymax)THEN
         ymax=ymax-ypix/(jt-jb+1)
         jt=jt-1
      END IF
!
! Recalculate size
!
      xsize=(ir-il+1)*width
      ysize=(jt-jb+1)*width
      xpix=xmax-xmin+1
      ypix=ymax-ymin+1
!
! Use pixel primitives if available and possible
!
      IF(grgcap(grcide)(7:7) == 'P'.AND.xsize-0.5_pg*width <= xpix  &
         .AND.ysize-0.5_pg*width <= ypix)THEN
!
!     write (6,*) 'GRPXPX'
!
         CALL grpxpx(ia,idim,jdim,il,ir,jb,jt,xmin,ymin)
!
! Otherwise, use points
!
      ELSE
!
!     write (6,*) 'GRPXPO'
!
         CALL grpxpo(ia,idim,jdim,il,ir,jb,jt,xmin,xmax,ymin,ymax)
      END IF
   END IF
!
END SUBROUTINE grpixl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPOCL -- polygon clip
!+
SUBROUTINE grpocl(n,px,py,edge,val,maxout,nout,qx,qy)
!
! Clip a polygon against a rectangle: Sutherland-Hodgman algorithm.
! this routine must be called four times to clip against each of the
! edges of the rectangle in turn.
!
! Arguments:
!
! N (input, integer): the number of vertices of the polygon (at least
!       3).
! PX, PY (input, real arrays, dimension at least N): world coordinates
!       of the N vertices of the input polygon.
! EDGE (input, integer):
!     1: clip against left edge,   X > XMIN=VAL
!     2: clip against right edge,  X < XMAX=VAL
!     3: clip against bottom edge, Y > YMIN=VAL
!     4: clip against top edge,    Y < YMIN=VAL
! VAL  (input, real): coordinate value of current edge.
! MAXOUT (input, integer): maximum number of vertices allowed in
!     output polygon (dimension of QX, QY).
! NOUT (output, integer): the number of vertices in the clipped polygon.
! QX, QY (output, real arrays, dimension at least MAXOUT): world
!       coordinates of the NOUT vertices of the output polygon.
!--
! 19-Sep-1994 - [TJP].
! 27-Feb-1996 - fix bug: overflow if coordinates are large [TJP].
! 11-Jul-1996 - fix bug: left and bottom edges disappeared when precisely
!               on edge [Remko Scharroo]
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: px(*)
   REAL(KIND=pg), INTENT(IN)                :: py(*)
   INTEGER, INTENT(IN)                      :: edge
   REAL(KIND=pg), INTENT(IN)                :: val
   INTEGER, INTENT(IN)                      :: maxout
   INTEGER, INTENT(OUT)                     :: nout
   REAL(KIND=pg), INTENT(OUT)               :: qx(*)
   REAL(KIND=pg), INTENT(OUT)               :: qy(*)
!
   INTEGER :: i
   REAL(KIND=pg) :: fx,fy,sx,sy
!
   nout=0
   DO  i=1,n
      IF(i == 1)THEN
!
!           -- save first point
!
         fx=px(i)
         fy=py(i)
      ELSE IF((edge == 1.OR.edge == 2).AND.(SIGN(1.0_pg,px(i)-val)  &
         .NE.SIGN(1.0_pg,sx-val)))THEN
!
!           -- SP intersects this edge: output vertex at intersection
!
         nout=nout+1
         IF(nout <= maxout)THEN
            qx(nout)=val
            qy(nout)=sy+(py(i)-sy)*((val-sx)/(px(i)-sx))
         END IF
      ELSE IF((edge == 3.OR.edge == 4).AND.(SIGN(1.0_pg,py(i)-val)  &
         /= SIGN(1.0_pg,sy-val)))THEN
!
!           -- SP intersects this edge: output vertex at intersection
!
         nout=nout+1
         IF(nout <= maxout)THEN
            qx(nout)=sx+(px(i)-sx)*((val-sy)/(py(i)-sy))
            qy(nout)=val
         END IF
      END IF
      sx=px(i)
      sy=py(i)
      IF((edge == 1.AND.sx >= val).OR.(edge == 2.AND.sx <= val)  &
         .OR.(edge == 3.AND.sy >= val).OR.(edge == 4.AND.sy <= val)) THEN
!
!           -- output visible vertex S
!
         nout=nout+1
         IF(nout <= maxout)THEN
            qx(nout)=sx
            qy(nout)=sy
         END IF
      END IF
   END DO
!
!      -- Does SF intersect edge?
!
   IF((edge == 1.OR.edge == 2).AND.(SIGN(1.0_pg,sx-val) /= SIGN(1.0_pg,fx-val)))THEN
      nout=nout+1
      IF(nout <= maxout)THEN
         qx(nout)=val
         qy(nout)=sy+(fy-sy)*((val-sx)/(fx-sx))
      END IF
   ELSE IF((edge == 3.OR.edge == 4).AND.(SIGN(1.0_pg,sy-val) /=  &
      SIGN(1.0_pg,fy-val)))THEN
      nout=nout+1
      IF(nout <= maxout)THEN
         qy(nout)=val
         qx(nout)=sx+(fx-sx)*((val-sy)/(fy-sy))
      END IF
   END IF
!
END SUBROUTINE grpocl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPROM -- prompt user before clearing screen
!+
SUBROUTINE grprom
!
! If the program is running under control of a terminal, display
! message and wait for the user to type <CR> before proceeding.
!
! Arguments:
!  none
!--
! 18-Aug-1994
!-----------------------------------------------------------------------
!
   INTEGER :: ier,l,grgcom
   CHARACTER (LEN=16) :: junk
!
   ier=grgcom(junk,'Type <RETURN> for next page: ',l)
!
END SUBROUTINE grprom
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPXPO -- Emulate pixel operations using points
!+
SUBROUTINE grpxpo(ia,idim,jdim,i1,i2,j1,j2,x1,x2,y1,y2)
!
! Arguments:
!  IA     (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  X1, X2 (input)  : the horizontal range of the output region
!  Y1, Y2 (input)  : the vertical range of the output region
!--
! 16-Jan-1991 - [GvG]
! 28-Jun-1991
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   INTEGER, INTENT(IN)                      :: ia(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: x1
   REAL(KIND=pg), INTENT(IN)                :: x2
   REAL(KIND=pg), INTENT(IN)                :: y1
   REAL(KIND=pg), INTENT(IN)                :: y2
!
   INTEGER :: lw
   INTEGER :: i,j
   INTEGER :: icol,lstcol
!
! Save attributes
!
   CALL grqlw(lw)
   CALL grqci(icol)
   CALL grslw(1)
   lstcol=icol
   DO  j=j1,j2
      DO  i=i1,i2
!
! Color changed?
!
         IF(ia(i,j) /= lstcol)THEN
            CALL grsci(ia(i,j))
            lstcol=ia(i,j)
         END IF
!
! Output dot
!
         CALL grdot0(x1+(x2-x1)*(i-i1+0.5)/(i2-i1+1),y1+(y2-y1)*  &
            (j-j1+0.5)/(j2-j1+1))
      END DO
   END DO
!
! Restore attributes
!
   CALL grsci(icol)
   CALL grslw(lw)
!
END SUBROUTINE grpxpo
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPXPS -- pixel dump for color or grey PostScript.
!+
SUBROUTINE grpxps(ia,idim,jdim,i1,i2,j1,j2,xmin,xmax,ymin,ymax)
!
! This routine is called by GRPIXL.
!--
!  4-Apr-93 - Created from GRGRAY by Remko Scharroo - DUT/SSRT
!  8-Apr-93 - Bugs fixed.
!  6-Jul-94 - Aligned with PGPLOT V4.9H
!  7-Sep-94 - updated for V5.0 [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   INTEGER, INTENT(IN)                      :: ia(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: xmin
   REAL(KIND=pg), INTENT(IN)                :: xmax
   REAL(KIND=pg), INTENT(IN)                :: ymin
   REAL(KIND=pg), INTENT(IN)                :: ymax
!
   INTEGER :: i,j,nxp,nyp,nbuf,lchr,ii
   REAL(KIND=pg) :: dx,dy,rbuf(32)
   CHARACTER (LEN=32) :: chr
!
   nxp=i2-i1+1
   nyp=j2-j1+1
!
! Build an image transformation matrix.
!
   dx=(xmax-xmin)/nxp
   dy=(ymax-ymin)/nyp
   rbuf(1)=0_pg
   rbuf(2)=REAL(nxp,KIND=pg)
   rbuf(3)=REAL(nyp,KIND=pg)
   rbuf(4)=grxmin(grcide)
   rbuf(5)=grxmax(grcide)
   rbuf(6)=grymin(grcide)
   rbuf(7)=grymax(grcide)
   rbuf(8)=1.0_pg/dx
   rbuf(9)=0.0_pg
   rbuf(10)=0.0_pg
   rbuf(11)=1.0_pg/dy
   rbuf(12)=(-xmin)/dx
   rbuf(13)=(-ymin)/dy
!
! Send setup info to driver.
!
   IF(.NOT.grpltd(grcide))CALL grbpic
   CALL grterm
   nbuf=13
   lchr=0
   CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
!
! Send the array of color indices to the driver.
!
   ii=0
   DO  j=j1,j2
      DO  i=i1,i2
         ii=ii+1
         rbuf(ii+1)=REAL(ia(i,j),KIND=pg)
         IF(ii == 20)THEN
            nbuf=ii+1
            rbuf(1)=REAL(ii,KIND=pg)
            CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
            ii=0
         END IF
      END DO
   END DO
   IF(ii > 0)THEN
      nbuf=ii+1
      rbuf(1)=REAL(ii,KIND=pg)
      CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
      ii=0
   END IF
!
! Send termination code to driver.
!
   nbuf=1
   rbuf(1)=-1.0_pg
   CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
!
   RETURN
!
END SUBROUTINE grpxps
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPXPX -- Perform pixel operations using pixel primitive
!+
SUBROUTINE grpxpx(ia,idim,jdim,i1,i2,j1,j2,x,y)
!
! Arguments:
!  IA     (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  X, Y   (input)  : the lower left corner of the output region
!                    (device coordinates)
!--
! 16-Jan-1991 - [GvG]
!  4-Aug-1993 - Debugged by Remko Scharroo
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   INTEGER, INTENT(IN)                      :: ia(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: x
   REAL(KIND=pg), INTENT(IN)                :: y
!
   INTEGER, PARAMETER :: nsize=1280
   REAL(KIND=pg) :: rbuf(nsize+2)
   REAL(KIND=pg) :: width
   INTEGER :: ic1,ic2
   INTEGER :: i,j,l
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=1) :: chr
!
   IF(.NOT.grpltd(grcide))CALL grbpic
!
! Get allowable color range and pixel width
!
   CALL grqcol(ic1,ic2)
   CALL grexec(grgtyp,3,rbuf,nbuf,chr,lchr)
   width=rbuf(3)
   DO  j=j1,j2
!
! Compute Y coordinate for this line
!
      rbuf(2)=y+REAL(j-j1,KIND=pg)*width
      i=i1
10    l=1
!
! Compute left X coordinate for this line segment
!
      rbuf(1)=x+REAL(i-i1,KIND=pg)*width
!
! Check color index
!
20    IF(ia(i,j) < ic1.OR.ic2 < ia(i,j))THEN
         rbuf(l+2)=1.0
      ELSE
         rbuf(l+2)=REAL(ia(i,j),KIND=pg)
      END IF
      l=l+1
      i=i+1
!
! Still room in segment and something left?
!
      IF(l <= nsize.AND.i <= i2)GO TO 20
!
! Output segment
!
!           NBUF = L + 2 ! wrong ! should be: (RS)
!
      nbuf=l+1
      CALL grexec(grgtyp,26,rbuf,nbuf,chr,lchr)
!
! Something left?
!
      IF(i <= i2)GO TO 10
   END DO
!
END SUBROUTINE grpxpx
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRPXRE -- Emulate pixel operations using rectangles
!+
SUBROUTINE grpxre(ia,idim,jdim,i1,i2,j1,j2,x1,x2,y1,y2)
!
! Arguments:
!  IA     (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  X1, X2 (input)  : the horizontal range of the output region
!  Y1, Y2 (input)  : the vertical range of the output region
!--
! 18-Jan-1991 - [GvG]
!-----------------------------------------------------------------------
!
   USE accur
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   INTEGER, INTENT(IN)                      :: ia(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: x1
   REAL(KIND=pg), INTENT(IN)                :: x2
   REAL(KIND=pg), INTENT(IN)                :: y1
   REAL(KIND=pg), INTENT(IN)                :: y2
!
   REAL(KIND=pg) :: yb,yt
   INTEGER :: i,j,icol,lstcol
!
! Save color attribute
!
   CALL grqci(icol)
   lstcol=icol
   DO  j=j1,j2
!
! Compute Y range for this index
!
      yb=y1+((y2-y1)*(j-j1))/(j2-j1+1)
      yt=y1+((y2-y1)*(j-j1+1))/(j2-j1+1)
      DO  i=i1,i2
!
! Need to change color?
!
         IF(ia(i,j) /= lstcol)THEN
            CALL grsci (ia(i,j))
            lstcol=ia(i,j)
         END IF
!
! Output rectangle
!
         CALL grrec0(x1+((x2-x1)*(i-i1))/(i2-i1+1),yb,x1+((x2-x1)*  &
            (i-i1+1))/(i2-i1+1),yt)
      END DO
   END DO
!
! Restore color attribute
!
   CALL grsci(icol)
!
END SUBROUTINE grpxre
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQCAP -- inquire device capabilities
!+
SUBROUTINE grqcap(string)
!
! GRPCKG: obtain the "device capabilities" string from the device
! driver for the current device.
!
! Arguments:
!
! STRING (output, CHARACTER*(*)): receives the device capabilities
!       string.
!--
! 26-Nov-92: new routine [TJP].
!  1-Sep-94: get from common instead of driver [TJP].
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   CHARACTER (LEN=*), INTENT(OUT)           :: string
!
   IF(grcide < 1)THEN
      CALL grwarn('GRQCAP - no graphics device is active.')
      string='NNNNNNNNNN'
   ELSE
      string=grgcap(grcide)
   END IF
!
END SUBROUTINE grqcap
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQCI -- inquire current color index
!+
SUBROUTINE grqci(c)
!
! GRPCKG: obtain the color index of the current graphics device.
!
! Argument:
!
! C (integer, output): receives the current color index (0-255).
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: c
!
   IF(grcide < 1)THEN
      CALL grwarn('GRQCI - no graphics device is active.')
      c=1
   ELSE
      c=grccol(grcide)
   END IF
!
END SUBROUTINE grqci
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQCOL -- inquire color capability
!+
SUBROUTINE grqcol(ci1,ci2)
!
! Query the range of color indices available on the current device.
!
! Argument:
!  CI1    (output) : the minimum available color index. This will be
!                    either 0 if the device can write in the
!                    background color, or 1 if not.
!  CI2    (output) : the maximum available color index. This will be
!                    1 if the device has no color capability, or a
!                    larger number (e.g., 3, 7, 15, 255).
!--
! 31-May-1989 - new routine [TJP].
!  1-Sep-1994 - avoid driver call [TJP].
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: ci1
   INTEGER, INTENT(OUT)                     :: ci2
!
! Error if no workstation is open.
!
   IF(grcide < 1)THEN
      ci1=0
      ci2=0
   ELSE
      ci1=grmnci(grcide)
      ci2=grmxci(grcide)
   END IF
!
END SUBROUTINE grqcol
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQCR -- inquire color representation
!+
SUBROUTINE grqcr(ci,cr,cg,cb)
!
! Return the color representation (red, green, blue intensities)
! currently associated with the specified color index. This may be
! different from that requested on some devices.
!
! Arguments:
!
! CI (integer, input): color index.
! CR, CG, CB (real, output): red, green, and blue intensities,
!       in range 0.0 to 1.0.
!--
!  7-Sep-1994 - rewrite [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ci
   REAL(KIND=pg), INTENT(OUT)               :: cr
   REAL(KIND=pg), INTENT(OUT)               :: cg
   REAL(KIND=pg), INTENT(OUT)               :: cb
!
   INTEGER :: nbuf,lchr,k
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=1) :: chr
!
   cr=1.0_pg
   cg=1.0_pg
   cb=1.0_pg
   k=ci
   IF(grcide < 1)THEN
!
!         -- no device open: return white
!
      CALL grwarn('GRQCR: no plot device is open.')
   ELSE IF(grgcap(grcide)(9:9) /= 'Y')THEN
!
!         -- devices that don't allow query color representation:
!            return black for ci 0, white for all others
!
      IF(k == 0)THEN
         cr=0.0_pg
         cg=0.0_pg
         cb=0.0_pg
      END IF
   ELSE
!
!         -- query device driver; treat invalid ci as 1
!
      IF(k < grmnci(grcide).OR.ci > grmxci(grcide))THEN
         CALL grwarn('GRQCR: invalid color index.')
         k=1
      END IF
      rbuf(1)=REAL(k,KIND=pg)
      nbuf=1
      lchr=0
      CALL grexec(grgtyp,29,rbuf,nbuf,chr,lchr)
      IF(nbuf < 4)THEN
         CALL grwarn('GRSCR: device driver error')
      ELSE
         cr=rbuf(2)
         cg=rbuf(3)
         cb=rbuf(4)
      END IF
   END IF
!
END SUBROUTINE grqcr
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQDEV -- inquire current device
!+
SUBROUTINE grqdev(device,l)
!
! Obtain the name of the current graphics device or file.
!
! Argument:
!  DEVICE (output): receives the device name of the
!       currently active device.
!  L (output): number of characters in DEVICE, excluding trailing
!       blanks.
!--
! 19-Feb-1988
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(OUT)           :: device
   INTEGER, INTENT(OUT)                     :: l
!
   IF(grcide < 1)THEN
      device='?'
      l=1
   ELSE
      device=grfile(grcide)
      l=grfnln(grcide)
      IF(l > LEN(device))l=LEN(device)
   END IF
!
END SUBROUTINE grqdev
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQDT -- inquire current device and type
!+
SUBROUTINE grqdt(device)
!
! GRPCKG: obtain the name and type of the current graphics device.
!
! Argument:
!
! DEVICE (output, character): receives the device name and type of the
!       currently active device in the form 'device/type'; this is a
!       valid string for input to GROPEN.
!--
!  1-Feb-1983
! 19-Feb-1988 - add quotes if necessary.
!-----------------------------------------------------------------------\
!
   USE grpckg1
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(OUT)           :: device
!
   CHARACTER (LEN=14) :: TYPE
   LOGICAL :: junk
   INTEGER :: l
!
   IF(grcide < 1)THEN
      CALL grwarn('GRQDT - no graphics device is active.')
      device='/NULL'
   ELSE
      CALL grqtyp(TYPE,junk)
      l=grfnln(grcide)
      IF(l <= 0)THEN
         device='/'//TYPE
      ELSE IF(INDEX(grfile(grcide)(1:l),'/') == 0)THEN
         device=grfile(grcide)(1:l)//'/'//TYPE
      ELSE
         device='"'//grfile(grcide)(1:l)//'"/'//TYPE
      END IF
   END IF
!
END SUBROUTINE grqdt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQFNT -- inquire current font
!+
SUBROUTINE grqfnt(IF)
!
! GRPCKG: obtain the font number of the current graphics device.
!
! Argument:
!
! IF (integer, output): receives the current font number (1-3).
!--
! (19-Mar-1983)
! 15-Dec-1988 - change name [TJP].
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                  :: IF
!
   IF(grcide < 1)THEN
      CALL grwarn('GRQFNT - no graphics device is active.')
      IF=1
   ELSE
      IF=grcfnt(grcide)
   END IF
!
END SUBROUTINE grqfnt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQLS -- inquire current line-style
!+
SUBROUTINE grqls(istyle)
!
! GRPCKG: obtain the line-style of the current graphics device.
!
! Argument:
!  ISTYLE (output): receives the current line-style code.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: istyle
!
   IF(grcide < 1)THEN
      CALL grwarn('GRQLS - no graphics device is active.')
      istyle=1
   ELSE
      istyle=grstyl(grcide)
   END IF
!
END SUBROUTINE grqls
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQLW -- inquire current line width
!+
SUBROUTINE grqlw(iwidth)
!
! GRPCKG: obtain the line-width of the current graphics device.
!
! Argument:
!  IWIDTH (output): receives the current line-width.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: iwidth
!
   IF(grcide < 1)THEN
      CALL grwarn('GRQLW - no graphics device is active.')
      iwidth=1
   ELSE
      iwidth=ABS(grwidt(grcide))
   END IF
!
END SUBROUTINE grqlw
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQPOS -- return current pen position (absolute, world coordinates)
!+
SUBROUTINE grqpos(x,y)
!
! GRQPOS: returns the current pen position in absolute, world
! coordinates.
!
! Arguments:
!
! X, Y (real, output): world coordinates of the pen position.
!--
!  1-Mar-1991 - new routine  [JM].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(OUT)                 :: x
   REAL(KIND=pg), INTENT(OUT)                 :: y
!
   IF(grcide >= 1)THEN
      x=(grxpre(grcide)-grxorg(grcide))/grxscl(grcide)
      y=(grypre(grcide)-gryorg(grcide))/gryscl(grcide)
   END IF
!
END SUBROUTINE grqpos
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQTXT -- get text bounding box
!+
SUBROUTINE grqtxt(orient,x0,y0,string,xbox,ybox)
!
! GRPCKG: get the bounding box of a string drawn by GRTEXT.
!--
! 12-Sep-1993 - [TJP].
!  8-Nov-1994 - return something even if string is blank [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: orient
   REAL(KIND=pg), INTENT(IN)                :: x0
   REAL(KIND=pg), INTENT(IN)                :: y0
   CHARACTER (LEN=*), INTENT(IN)            :: string
   REAL(KIND=pg), INTENT(OUT)               :: xbox(4)
   REAL(KIND=pg), INTENT(OUT)               :: ybox(4)
   LOGICAL :: unused,visble,plot
   INTEGER :: xygrid(300)
   INTEGER :: list(256)
!
   REAL(KIND=pg) :: angle,factor,fntbas,fntfac,cosa,sina,dx,dy,xorg,yorg
   REAL(KIND=pg) :: ratio, rlx,rly
   REAL(KIND=pg) :: xg,yg,xgmin,xgmax,ygmin,ygmax
   INTEGER :: i,ifntlv,nlist,lx,ly,k,lxlast,lylast
!
!INTRINSIC  ABS,COS,LEN,MAX,MIN,SIN
!
! Default return values.
!
   DO  i=1,4
      xbox(i)=x0
      ybox(i)=y0
   END DO
!
! Check that there is something to be plotted.
!
   IF(LEN(string) <= 0)RETURN
!
! Check that a device is selected.
!
   IF(grcide < 1)THEN
      CALL grwarn('GRQTXT - no graphics device is active.')
      RETURN
   END IF
!
   xorg=grxpre(grcide)
   yorg=grypre(grcide)
!
! Compute scaling and orientation.
!
   angle=orient*(3.141592653589793_pg/180.0_pg)
   factor=grcfac(grcide)/2.5_pg
   ratio=grpxpi(grcide)/grpypi(grcide)
   cosa=factor*COS(angle)
   sina=factor*SIN(angle)
   xorg=x0
   yorg=y0
!
! Convert the string to a list of symbol numbers; to prevent overflow
! of array LIST, the length of STRING is limited to 256 characters.
!
   CALL grsyds(list,nlist,string(1:MIN(256,LEN(string))),grcfnt(grcide))
!
! Run through the string of characters, getting bounding box
! in character coordinates. (XG, YG) is the starting point
! of the current character. The x/y limits of the bbox are
! XGMIN...XGMAX, YGMIN...YGMAX.
!
   fntbas=0.0_pg
   fntfac=1.0_pg
   ifntlv=0
   dx=0.0_pg
   dy=0.0_pg
   xg=0.0_pg
   yg=0.0_pg
   xgmin=10E+30_pg
   xgmax=-1.0E+30_pg
   ygmin=1.0E+30_pg
   ygmax=-1.0E+30_pg
   plot=.false.
   DO  i=1,nlist
      IF(list(i) < 0)THEN
         IF(list(i) == -1)THEN
!
!                 ! up
!
            ifntlv=ifntlv+1
            fntbas=fntbas+16.0_pg*fntfac
            fntfac=0.75_pg**ABS(ifntlv)
         ELSE IF(list(i) == -2)THEN
!
!                 ! down
!
            ifntlv=ifntlv-1
            fntfac=0.75_pg**ABS(ifntlv)
            fntbas=fntbas-16.0_pg*fntfac
         ELSE IF(list(i) == -3)THEN
!
!                 ! backspace
!
            xg=xg-dx*fntfac
         END IF
         CYCLE
      END IF
      CALL grsyxd(list(i),xygrid,unused)
      visble=.false.
      dx=xygrid(5)-xygrid(4)
      k=4
      lxlast=-64
      lylast=-64
20    k=k+2
      lx=xygrid(k)
      ly=xygrid(k+1)
      IF(ly == -64)GO TO 30
      IF(lx == -64)THEN
         visble=.false.
      ELSE
         rlx=(lx-xygrid(4))*fntfac
         rly=(ly-xygrid(2))*fntfac+fntbas
         IF((lx /= lxlast).OR.(ly /= lylast))THEN
            xgmin=MIN(xgmin,xg+rlx)
            xgmax=MAX(xgmax,xg+rlx)
            ygmin=MIN(ygmin,rly)
            ygmax=MAX(ygmax,rly)
            plot=.true.
         END IF
         visble=.true.
         lxlast=lx
         lylast=ly
      END IF
      GO TO 20
30    xg=xg+dx*fntfac
   END DO
!
! Check whether anything was plotted.
!
   IF(.NOT.plot)RETURN
!
! Expand the box a bit to allow for line-width.
!
   xgmin=xgmin-5.0_pg
   xgmax=xgmax+5.0_pg
   ygmin=ygmin-4.0_pg
   ygmax=ygmax+4.0_pg
!
! Convert bounding box to device coordinates.
!
!     WRITE (*,*) XGMIN, XGMAX, YGMIN, YGMAX
!
   xbox(1)=xorg+(cosa*xgmin-sina*ygmin)*ratio
   ybox(1)=yorg+(sina*xgmin+cosa*ygmin)
   xbox(2)=xorg+(cosa*xgmin-sina*ygmax)*ratio
   ybox(2)=yorg+(sina*xgmin+cosa*ygmax)
   xbox(3)=xorg+(cosa*xgmax-sina*ygmax)*ratio
   ybox(3)=yorg+(sina*xgmax+cosa*ygmax)
   xbox(4)=xorg+(cosa*xgmax-sina*ygmin)*ratio
   ybox(4)=yorg+(sina*xgmax+cosa*ygmin)
!
END SUBROUTINE grqtxt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQTYP -- inquire current device type
!+
SUBROUTINE grqtyp(TYPE,inter)
!
! GRPCKG: obtain the device type of the currently selected graphics
! device, and determine whether or not it is an interactive device.
!
! Arguments:
!
! TYPE (output, CHARACTER*(*)): receives the device type, as a
!       character string, eg 'PRINTRONIX', 'TRILOG', 'VERSATEC',
!       'TEK4010', 'TEK4014', 'GRINNELL', or 'VT125'.  The character
!       string should have a length of at least 8 to ensure that the
!       type is unique.
! INTER (output, LOGICAL): receives the value .TRUE. if the device is
!       interactive, .FALSE. otherwise.
!--
! (23-May-1983)
!  5-Aug-1986 - add GREXEC support [AFT].
! 18-Jan-1993 - return type only, not description [TJP].
!  1-Sep-1994 - get capabilities from common [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(OUT)           :: TYPE
   LOGICAL, INTENT(OUT)                     :: inter
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=32) :: chr
!
   IF(grcide < 1)THEN
      CALL grwarn('GRQTYP - no graphics device is active.')
      TYPE='NULL'
      inter=.false.
   ELSE
      CALL grexec(grgtyp,1,rbuf,nbuf,chr,lchr)
      lchr=INDEX(chr,' ')
      TYPE=chr(:lchr)
      inter=(grgcap(grcide)(1:1) == 'I')
   END IF
!
END SUBROUTINE grqtyp
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRQUIT -- report a fatal error and abort execution
!+
SUBROUTINE grquit(text)
!
! Report a fatal error (via GRWARN) and exit program.
! This routine should be called in the event of an unrecoverable
! PGPLOT error.
!
! Argument:
!  TEXT (input): text of message to be sent to GRWARN.
!--
! 12-Nov-1994
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)        :: text
!
   CALL grwarn(text)
   CALL grwarn('Fatal error in PGPLOT library: program terminating.')
   STOP
!
END SUBROUTINE grquit
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRREC0 -- fill a rectangle (device coordinates)
!+
SUBROUTINE grrec0(x0,y0,x1,y1)
!
! GRPCKG: Fill a rectangle with solid color.  The rectangle
! is defined by the (x,y) device coordinates of its lower left and
! upper right corners; the edges are parallel to the coordinate axes.
! X0 is guaranteed to be <= X1 and Y0 <= Y1. The rectangle possible
! extends beyond the clipping boundaries
!
! Arguments:
!
! X0, Y0 (input, real): device coordinates of one corner of the
!       rectangle.
! X1, Y1 (input, real): device coordinates of the opposite corner of
!       the rectangle.
!--
! 23-Mar-1988 - [TJP].
! 18-Jan-1991 - Code moved from GRRECT to GRREC0 so that it can also be
!               used by GRPXRE.
!  1-Sep-1994 - suppress driver call [TJP].
!  4-Dec-1995 - avoid use of real variable as do-loop index [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: x0
   REAL(KIND=pg), INTENT(IN)                         :: y0
   REAL(KIND=pg), INTENT(IN)                         :: x1
   REAL(KIND=pg), INTENT(IN)                         :: y1
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=32) :: chr
   REAL(KIND=pg) :: xmin,ymin,xmax,ymax,y,dy
   INTEGER :: ls,lw,i,nlines
!
! Clip
!
   xmin=x0
   xmax=x1
   ymin=y0
   ymax=y1
   IF(xmin < grxmin(grcide))xmin=grxmin(grcide)
   IF(xmax > grxmax(grcide))xmax=grxmax(grcide)
   IF(ymin < grymin(grcide))ymin=grymin(grcide)
   IF(ymax > grymax(grcide))ymax=grymax(grcide)
   IF(xmin > xmax)RETURN
   IF(ymin > ymax)RETURN
!
! Use hardware rectangle fill if available.
!
   IF(grgcap(grcide)(6:6) == 'R')THEN
      IF(.NOT.grpltd(grcide))CALL grbpic
      rbuf(1)=xmin
      rbuf(2)=ymin
      rbuf(3)=xmax
      rbuf(4)=ymax
      CALL grexec(grgtyp,24,rbuf,nbuf,chr,lchr)
      RETURN
!
! Else use hardware polygon fill if available.
!
   ELSE IF(grgcap(grcide)(4:4) == 'A')THEN
      IF(.NOT.grpltd(grcide))CALL grbpic
      rbuf(1)=4.0_pg
      CALL grexec(grgtyp,20,rbuf,nbuf,chr,lchr)
      rbuf(1)=xmin
      rbuf(2)=ymin
      CALL grexec(grgtyp,20,rbuf,nbuf,chr,lchr)
      rbuf(1)=xmax
      rbuf(2)=ymin
      CALL grexec(grgtyp,20,rbuf,nbuf,chr,lchr)
      rbuf(1)=xmax
      rbuf(2)=ymax
      CALL grexec(grgtyp,20,rbuf,nbuf,chr,lchr)
      rbuf(1)=xmin
      rbuf(2)=ymax
      CALL grexec(grgtyp,20,rbuf,nbuf,chr,lchr)
      RETURN
   END IF
!
! For other devices fill area is simulated.
!
! Save attributes.
!
   CALL grqls(ls)
   CALL grqlw(lw)
   CALL grsls(1)
   CALL grslw(1)
   CALL grexec(grgtyp,3,rbuf,nbuf,chr,lchr)
   dy=rbuf(3)
!
! Draw horizontal raster lines.
!
   nlines=INT(ABS((ymax-ymin)/dy))
   y=ymin-dy/2.0
   DO  i=1,nlines
      y=y+dy
      grxpre(grcide)=xmin
      grypre(grcide)=y
      CALL grlin0 (xmax,y)
   END DO
!
! Restore attributes.
!
   CALL grsls(ls)
   CALL grslw(lw)
!
END SUBROUTINE grrec0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRRECT -- fill a rectangle
!+
SUBROUTINE grrect(x0,y0,x1,y1)
!
! GRPCKG: Fill a rectangle with solid color.  The rectangle
! is defined by the (x,y) world coordinates of its lower left and upper
! right corners; the edges are parallel to the coordinate axes.
!
! Arguments:
!
! X0, Y0 (input, real): world coordinates of one corner of the
!       rectangle.
! X1, Y1 (input, real): world coordinates of the opposite corner of the
!       rectangle.
!--
! 23-Mar-1988 - [TJP].
! 18-Jan-1991 - Code moved from GRRECT to GRREC0 so that it can also be
!               used by GRPXRE
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: x0
   REAL(KIND=pg), INTENT(IN)                     :: y0
   REAL(KIND=pg), INTENT(IN)                     :: x1
   REAL(KIND=pg), INTENT(IN)                     :: y1
!
   REAL(KIND=pg) :: xll,yll,xur,yur
   REAL(KIND=pg) :: xmin,ymin,xmax,ymax
!
   IF(grcide < 1)RETURN
!
! Convert to device coordinates and clip.
!
   CALL grtxy0(.false.,x0,y0,xll,yll)
   CALL grtxy0(.false.,x1,y1,xur,yur)
   xmin=MIN(xll,xur)
   xmax=MAX(xll,xur)
   ymin=MIN(yll,yur)
   ymax=MAX(yll,yur)
!
! Do the real work
!
   CALL grrec0(xmin,ymin,xmax,ymax)
!
END SUBROUTINE grrect
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSCI -- set color index
!+
SUBROUTINE grsci(ic)
!
! GRPCKG: Set the color index for subsequent plotting. Calls to GRSCI
! are ignored for monochrome devices. The default color index is 1,
! usually white on a black background for video displays or black on a
! white background for printer plots. The color index is an integer in
! the range 0 to a device-dependent maximum. Color index 0 corresponds
! to the background color; lines may be "erased" by overwriting them
! with color index 0.
!
! Color indices 0-7 are predefined as follows: 0 = black (background
! color), 1 = white (default), 2 = red, 3 = green, 4 = blue, 5 = cyan
! (blue + green), 6 = magenta (red + blue), 7 = yellow (red + green).
! The assignment of colors to color indices can be changed with
! subroutine GRSCR (set color representation).
!
! Argument:
!
! IC (integer, input): the color index to be used for subsequent
!       plotting on the current device (in range 0-255). If the
!       index exceeds the device-dependent maximum, the result is
!       device-dependent.
!--
! 11-Apr-1983 - [TJP].
!  3-Jun-1984 - add GMFILE device [TJP].
! 13-Jun-1984 - add code for TK4100 devices [TJP].
!  2-Jul-1984 - add code for RETRO and VT125 (REGIS) devices [TJP].
!  2-Oct-1984 - change REGIS to improve VT240 behavior [TJP].
! 22-Dec-1984 - add PRTX, TRILOG, VERS and VV devices [TJP].
! 29-Jan-1985 - add HP2648 device [KS/TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 21-Feb-1987 - delays setting color if picture not open [AFT].
! 11-Jun-1987 - remove built-in devices [TJP].
! 31-May-1989 - add check for valid color index [TJP].
!  1-Sep-1994 - use common data [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ic
   INTEGER :: color,ic1,ic2,nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=1) :: chr
!
! Error if no workstation is open.
!
   IF(grcide < 1)THEN
      CALL grwarn('GRSCI - no graphics device is active.')
      RETURN
   END IF
!
! Use color index 1 if out of range.
!
   ic1=grmnci(grcide)
   ic2=grmxci(grcide)
   color=ic
   IF(color < ic1.OR.color > ic2)color=1
!
! If no change to color index is requested, take no action.
!
   IF(color == grccol(grcide))RETURN
!
! If the workstation is in "picture open" state, send command to
! driver.
!
   IF(grpltd(grcide))THEN
      rbuf(1)=color
      CALL grexec(grgtyp,15,rbuf,nbuf,chr,lchr)
   END IF
!
! Set the current color index.
!
   grccol(grcide)=color
!
END SUBROUTINE grsci
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSCR -- set color representation
!+
SUBROUTINE grscr(ci,cr,cg,cb)
!
! GRPCKG: SET COLOUR REPRESENTATION -- define the colour to be
! associated with a colour index.  Ignored for devices which do not
! support variable colour or intensity.  On monochrome output
! devices (e.g. VT125 terminals with monochrome monitors), the
! monochrome intensity is computed from the specified Red, Green, Blue
! intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television
! systems, NTSC encoding.  Note that most devices do not have an
! infinite range of colours or monochrome intensities available;
! the nearest available colour is used.
!
! Arguments:
!
! CI (integer, input): colour index. If the colour index is outside the
!       range available on the device, the call is ignored. Colour
!       index 0 applies to the background colour.
! CR, CG, CB (real, input): red, green, and blue intensities,
!       in range 0.0 to 1.0.
!--
! 20-Feb-1984 - [TJP].
!  5-Jun-1984 - add GMFILE device [TJP].
!  2-Jul-1984 - add REGIS device [TJP].
!  2-Oct-1984 - change use of map tables in Regis [TJP].
! 11-Nov-1984 - add code for /TK [TJP].
!  1-Sep-1986 - add GREXEC support [AFT].
! 21-Feb-1987 - If needed, calls begin picture [AFT].
! 31-Aug-1994 - suppress call of begin picture [TJP].
!  1-Sep-1994 - use common data [TJP].
! 26-Jul-1995 - fix bug: some drivers would ignore a change to the
!               current color [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ci
   REAL(KIND=pg), INTENT(IN)                :: cr
   REAL(KIND=pg), INTENT(IN)                :: cg
   REAL(KIND=pg), INTENT(IN)                :: cb
!
   INTEGER :: nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=1) :: chr
!
   IF(grcide < 1)THEN
      CALL grwarn('GRSCR - Specified workstation is not open.')
   ELSE IF(cr < 0.0_pg.OR.cg < 0.0_pg.OR.cb < 0.0_pg.OR.cr > 1.0_pg.OR.  &
      cg > 1.0_pg.OR.cb > 1.0_pg)THEN
      CALL grwarn('GRSCR - Colour is outside range [0,1].')
   ELSE IF(ci >= grmnci(grcide).AND.ci <= grmxci(grcide))THEN
!         IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
      rbuf(1)=ci
      rbuf(2)=cr
      rbuf(3)=cg
      rbuf(4)=cb
      nbuf=4
      CALL grexec(grgtyp,21,rbuf,nbuf,chr,lchr)
!
!         -- If this is the current color, reselect it in the driver.
!
      IF(ci == grccol(grcide))THEN
         rbuf(1)=ci
         CALL grexec(grgtyp,15,rbuf,nbuf,chr,lchr)
      END IF
   END IF
!
END SUBROUTINE grscr
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
! GRSCRL -- scroll pixels in viewport
!+
SUBROUTINE grscrl(dx,dy)
!
! Shift the pixels in the viewport by DX and DY in device coordinates.
!--
! 24-Feb-97: new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   INTEGER, INTENT(IN)                      :: dx
   INTEGER, INTENT(IN)                      :: dy
!
   INTEGER :: nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=8) :: chr
!
! Do nothing if device is not open or not in appropriate state.
!
   IF(grcide < 1)RETURN
   IF(.NOT.grpltd(grcide))RETURN
!
! If device has scroll capability, use it. The arguments in
! RBUF are: (1..4) current viewport in device coordinates;
! (5..6) scroll displacement in world coordinates.
!
   IF(grgcap(grcide)(11:11) == 'S')THEN
      rbuf(1)=REAL(nint(grxmin(grcide)),KIND=pg)
      rbuf(2)=REAL(nint(grymin(grcide)),KIND=pg)
      rbuf(3)=REAL(nint(grxmax(grcide)),KIND=pg)
      rbuf(4)=REAL(nint(grymax(grcide)),KIND=pg)
      rbuf(5)=dx
      rbuf(6)=dy
      nbuf=6.0_pg
      lchr=0
      CALL grexec(grgtyp,30,rbuf,nbuf,chr,lchr)
!
! Otherwise, report an error.
!
   ELSE
      CALL grwarn('Device does not support scrolling')
   END IF
!
END SUBROUTINE grscrl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSETC -- set character size
!+
SUBROUTINE grsetc(ident,xsize)
!
! GRPCKG : change the character size (user-callable routine).
!
! Input:   IDENT : plot identifier
!          XSIZE : the new character width. The character height
!                  and spacing will be scaled by the same factor.
!                  If XSIZE is negative or zero, the character size
!                  will be set to the default size.
!--
! (1-Feb-1983)
! 16-Sep-1985 - add code for metafile output (TJP).
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ident
   REAL(KIND=pg), INTENT(IN)                         :: xsize
!
! Record the new size (GRCFAC).
!
   CALL grslct(ident)
   IF(xsize <= 0.0_pg)THEN
      grcfac(ident)=1.0_pg
   ELSE
      grcfac(ident)=xsize/grcxsz
   END IF
!
END SUBROUTINE grsetc
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSETFONT -- set text font [obsolete]
!
SUBROUTINE grsetfont(IF)
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: IF
!
   CALL grsfnt(IF)
!
END SUBROUTINE grsetfont
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSETLI -- *obsolete routine*
!+
SUBROUTINE grsetli(in)
!
! GRPCKG: Set the line intensity for subsequent plotting on the current
! device. *** OBSOLETE ROUTINE *** Intensity is now set with GRSCI
! and GRSCR. For compatibility, GRSETLI now sets color zero if its
! argument is 0, and resets the previous color if its argument is
! non-zero.
!
! Argument:
!
! IN (integer, input): the intensity to be used for subsequent
!       plotting on the current device (in range 0-3).
!--
! 11-Apr-1983 - [TJP].
! 12-Jul-1984 - modify to call GRSCI [TJP].
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: in
   INTEGER :: oldcol(grimax)
   DATA oldcol/grimax*1/
!
   IF(grcide < 1)THEN
      CALL grwarn('GRSETLI - no graphics device is active.')
   ELSE IF(in == 0)THEN
      oldcol(grcide)=grccol(grcide)
      CALL grsci(0)
   ELSE
      CALL grsci(oldcol(grcide))
   END IF
!
END SUBROUTINE grsetli
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSETPEN -- *obsolete routine*
!+
SUBROUTINE grsetpen
!
! GRPCKG: Set the pen number for subsequent plotting.  Obsolete
! routine: ignored.
!-----------------------------------------------------------------------
!
   CALL grwarn('GRSETPEN is an obsolete routine.')
!
END SUBROUTINE grsetpen
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSETS -- change size of view surface
!+
SUBROUTINE grsets(ident,xsize,ysize)
!
! GRPCKG : change size of plotting area. The requested dimensions
! will be reduced to the absolute maximum of the plot device if
! necessary.
!
! Arguments:
!
! IDENT (input, integer): plot identifier from GROPEN.
! XSIZE (input, real): new x dimension of plot area (absolute
!               units); if less than zero, the default dimension
!               will be used.
! YSIZE (input, real): new y dimension of plot area (absolute
!               units); if less than zero, the default dimension
!               will be used.
!--
! (1-Feb-1983)
!  5-Aug-1986 - add GREXEC support [AFT].
!  5-Jan-1993 - set GRADJU [TJP].
!------------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: ident
   REAL(KIND=pg), INTENT(IN)            :: xsize
   REAL(KIND=pg), INTENT(IN)            :: ysize
   INTEGER :: i, j,ix,iy,nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=1) :: chr
!
   CALL grslct(ident)
!
!     write (*,*) 'GRSETS: old size', GRXMXA(IDENT), GRYMXA(IDENT)
!
   CALL grpage
   IF((xsize < 0.0_pg).OR.(ysize < 0.0_pg))THEN
      CALL grexec(grgtyp,6,rbuf,nbuf,chr,lchr)
      grxmxa(ident)=INT(rbuf(2))
      grymxa(ident)=INT(rbuf(4))
   ELSE
      i=nint(xsize)
      j=nint(ysize)
      CALL grexec(grgtyp,2,rbuf,nbuf,chr,lchr)
      ix=INT(rbuf(2))
      iy=INT(rbuf(4))
      IF(ix > 0)i=MIN(i,ix)
      IF(iy > 0)j=MIN(j,iy)
      grxmxa(ident)=i
      grymxa(ident)=j
   END IF
!     write (*,*) 'GRSETS: new size', GRXMXA(IDENT), GRYMXA(IDENT)
   grxmin(ident)=0
   grxmax(ident)=grxmxa(ident)
   grymin(ident)=0
   grymax(ident)=grymxa(ident)
   gradju(ident)=.true.
!
END SUBROUTINE grsets
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSFNT -- set text font
!+
SUBROUTINE grsfnt(IF)
!
! GRPCKG: Set the font for subsequent text plotting.
! The default font is 1 ("Normal" font); others available are 2
! ("Roman"), 3 ("Italic"), and 4 ("Script").
!
! Argument:
!  IF (input): the font number to be used for subsequent
!       text plotting on the current device (in range 1-4).
!--
! 19-Mar-1983 - [TJP].
!  4-Jun-1984 - add code for GMFILE device [TJP].
! 15-Dec-1988 - change name [TJP].
!-----------------------------------------------------------------------
!
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: if
!
   INTEGER :: i
!
   IF(grcide < 1)THEN
      CALL grwarn('GRSFNT - no graphics device is active.')
      RETURN
   END IF
!
! Set software font index.
!
   IF(if < 1.OR.if > 4)THEN
      CALL grwarn('Illegal font selected: font 1 used.')
      i=1
   ELSE
      i=if
   END IF
!
! Ignore request if no change is to be made.
!
   IF(if == grcfnt(grcide))RETURN
!
! Save font setting.
!
   grcfnt(grcide)=i
!
END SUBROUTINE grsfnt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSIZE -- inquire device size and resolution
!+
SUBROUTINE grsize(ident,xszdef,yszdef,xszmax,yszmax,xperin,yperin)
!
! GRPCKG : obtain device parameters (user-callable routine).
!--
! (1-Feb-1983)
!  5-Aug-1986 - add GREXEC support [AFT].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: ident
   REAL(KIND=pg), INTENT(OUT)               :: xszdef
   REAL(KIND=pg), INTENT(OUT)               :: yszdef
   REAL(KIND=pg), INTENT(OUT)               :: xszmax
   REAL(KIND=pg), INTENT(OUT)               :: yszmax
   REAL(KIND=pg), INTENT(OUT)               :: xperin
   REAL(KIND=pg), INTENT(OUT)               :: yperin
!
   INTEGER :: nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=1) :: chr
!
   CALL grslct(ident)
   CALL grexec(grgtyp,6,rbuf,nbuf,chr,lchr)
   xszdef=rbuf(2)
   yszdef=rbuf(4)
   CALL grexec(grgtyp,2,rbuf,nbuf,chr,lchr)
   xszmax=rbuf(2)
   yszmax=rbuf(4)
   xperin=grpxpi(grcide)
   yperin=grpypi(grcide)
!
END SUBROUTINE grsize
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSKPB -- skip blanks in character string
!+
SUBROUTINE grskpb(s,i)
!
! GRSKPB: increment I so that it points to the next non-blank
! character in string S.  'Blank' characters are space and tab (ASCII
! character value 9).
!
! Arguments:
!  S      (input)  : character string to be parsed.
!  I      (in/out) : on input, I is the index of the first character
!                    in S to be examined; on output, either it points
!                    to the next non-blank character, or it is equal
!                    to LEN(S)+1 (if all the rest of the string is
!                    blank).
!--
!  1985 Oct 8 - New routine, based on SKIPBL (T. J. Pearson).
!-----------------------------------------------------------------------
!
   CHARACTER (LEN=*), INTENT(IN OUT)        :: s
   INTEGER, INTENT(OUT)                     :: i
!
10 IF(i > LEN(s))RETURN
   IF(s(i:i) /= ' '.AND.s(i:i) /= CHAR(9))RETURN
   i=i+1
   GO TO 10
!
END SUBROUTINE grskpb
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSLCT -- select active output device
!+
SUBROUTINE grslct(ident)
!
! GRPCKG: Check that IDENT is a valid plot identifier, and select the
! corresponding plot as the current plot. All subsequent plotting will
! be directed to this device until the assignment is changed by another
! call to GRSLCT.
!
! Argument:
!
! IDENT (input, integer): the identifier of the plot to be selected, as
!       returned by GROPEN.
!--
! (1-Feb-1983)
!  5-Aug-1986 - add GREXEC support [AFT].
!  4-Jun-1987 - skip action if no change in ID [TJP].
! 26-Nov-1990 - [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ident
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=1) :: chr
!
   IF((ident <= 0).OR.(ident > grimax).OR.(grstat(ident) == 0)) THEN
      CALL grwarn('GRSLCT - invalid plot identifier.')
   ELSE IF(ident == grcide)THEN
      grgtyp=grtype(ident)
      RETURN
   ELSE
      grcide=ident
      grgtyp=grtype(ident)
      rbuf(1)=grcide
      rbuf(2)=grunit(grcide)
      nbuf=2
      CALL grexec(grgtyp,8,rbuf,nbuf,chr,lchr)
   END IF
!
END SUBROUTINE grslct
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSLS -- set line style
!+
SUBROUTINE grsls(is)
!
! GRPCKG: Set the line style for subsequent plotting on the current
! device. The different line styles are generated in hardware on
! some devices and by GRPCKG software for the other devices. Five
! different line styles are available, with the following codes:
! 1 (full line), 2 (dashed), 3 (dot-dash-dot-dash), 4 (dotted),
! 5 (dash-dot-dot-dot). The default is 1 (normal full line). Line
! style is ignored when drawing characters, which are always drawn with
! a full line.
!
! Argument:
!
! IS (input, integer): the line-style code for subsequent plotting on
!       the current device (in range 1-5).
!--
!  9-Feb-1983 - [TJP].
!  3-Jun-1984 - add GMFILE device [TJP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 21-Feb-1987 - If needed, calls begin picture [AFT].
! 19-Jan-1987 - fix bug in GREXEC call [TJP].
! 16-May-1989 - fix bug for hardware line dash [TJP].
!  1-Sep-1994 - do not call driver to get size and capabilities [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: is
!
   INTEGER :: i,l,idash,nbuf,lchr
   REAL(KIND=pg) :: rbuf(6),tmp
   CHARACTER (LEN=10) :: chr
   REAL(KIND=pg) :: patern(8,5)
!
   DATA patern/8*10.0_pg,8*10.0_pg,8.0_pg,6.0_pg,1.0_pg,6.0_pg, &
      8.0_pg,6.0_pg,1.0_pg,6.0_pg,1.0_pg,6.0_pg,1.0_pg,6.0_pg, &
      1.0_pg,6.0_pg,1.0_pg,6.0_pg,8.0_pg,6.0_pg,1.0_pg,6.0_pg, &
      1.0_pg,6.0_pg,1.0_pg,6.0_pg/
!
   IF(grcide < 1)THEN
      CALL grwarn('GRSLS - no graphics device is active.')
      RETURN
   END IF
!
   i=is
   IF(i < 1.OR.i > 5)THEN
      CALL grwarn('GRSLS - invalid line-style requested.')
      i=1
   END IF
!
! Inquire if hardware dash is available.
!
   idash=0
   IF(grgcap(grcide)(3:3) == 'D')idash=1
!
! Set up for hardware dash.
!
   IF(idash /= 0)THEN
      grdash(grcide)=.false.
      IF(grpltd(grcide))THEN
         rbuf(1)=i
         nbuf=1
         CALL grexec(grgtyp,19,rbuf,nbuf,chr,lchr)
      END IF
!
! Set up for software dash.
!
   ELSE
      IF(i == 1)THEN
         grdash(grcide)=.false.
      ELSE
         grdash(grcide)=.true.
         gripat(grcide)=1
         grpoff(grcide)=0.0_pg
         tmp=grymxa(grcide)/1000.0_pg
         DO  l=1,8
            grpatn(grcide,l)=patern(l,i)*tmp
         END DO
      END IF
   END IF
   grstyl(grcide)=i
!
END SUBROUTINE grsls
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSLW -- set line width
!+
SUBROUTINE grslw(iw)
!
! GRPCKG: Set the line width for subsequent plotting on the current
! device. If the hardware does not support thick lines, they are
! simulated by tracing each line with multiple strokes offset in the
! direction perpendicular to the line. The line width is specified by
! the number of strokes to be used, which must be in the range 1-201.
! The actual line width obtained depends on the device resolution.
! If the hardware does support thick lines, the width of the line
! is approximately 0.005 inches times the value of argument IW.
!
! Argument:
!
! IW (integer, input): the number of strokes to be used for subsequent
!       plotting on the current device (in range 1-201).
!--
!  1-Feb-1983 [TJP].
!  3-Jun-1984 [TJP] - add GMFILE device.
! 28-Aug-1984 [TJP] - correct bug in GMFILE: redundant SET_LINEWIDTH
!                     commands were not being filtered out.
! 26-May-1987 [TJP] - add GREXEC support.
! 11-Jun-1987 [TJP] - remove built-in devices.
! 31-May-1989 [TJP] - increase maximum width from 21 to 201.
!  1-Sep-1994 [TJP]
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: iw
!
   INTEGER :: i,ithick
   REAL(KIND=pg) :: rbuf(1)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=32) :: chr
!
! Check that graphics is active.
!
   IF(grcide < 1)THEN
      CALL grwarn('GRSLW - no graphics device is active.')
      RETURN
   END IF
!
! Check that requested line-width is valid.
!
   i=iw
   IF(i < 1.OR.i > 201)THEN
      CALL grwarn('GRSLW - invalid line-width requested.')
      i=1
   END IF
!
! Ignore the request if the linewidth is unchanged.
!
   IF(i == ABS(grwidt(grcide)))RETURN
!
! Inquire if hardware supports thick lines.
!
   ithick=0
   IF(grgcap(grcide)(5:5) == 'T')ithick=1
!
! For devices with hardware support of thick lines, send the
! appropriate commands to the device driver, and give the "current
! linewidth" parameter a negative value to suppress software linewidth
! emulation.
!
   IF(ithick == 1.AND.grpltd(grcide))THEN
      rbuf(1)=i
      CALL grexec(grgtyp,22,rbuf,nbuf,chr,lchr)
   END IF
!
! Save the current linewidth.
!
   grwidt(grcide)=i
   IF(ithick == 1)grwidt(grcide)=-i
!
END SUBROUTINE grslw
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSYDS -- decode character string into list of symbol numbers
!+
SUBROUTINE grsyds(symbol,nsymbs,text,font)
!
! Given a character string, this routine returns a list of symbol
! numbers to be used to plot it. It is responsible for interpreting
! all escape sequences.  Negative `symbol numbers' are inserted in the
! list to represent pen movement. The following escape sequences are
! defined (the letter following the \ may be either upper or lower
! case):
!
! \u       :      up one level (returns -1)
! \d       :      down one level (returns -2)
! \b       :      backspace (returns -3)
! \A       :      (upper case only) Angstrom symbol, roman font
! \x       :      multiplication sign
! \.       :      centered dot
! \\       :      \, returns the code for backslash
! \gx      :      greek letter corresponding to roman letter x
! \fn      :      switch to Normal font
! \fr      :      switch to Roman font
! \fi      :      switch to Italic font
! \fs      :      switch to Script font
! \mn or \mnn :   graph marker number n or nn (1 or 2 digits)
! \(nnn)   :      Hershey symbol number nnn (any number of digits)
!
! Arguments:
!  SYMBOL (output) : receives the list of symbol numers.
!  NSYMBS (output) : receives the actual number of symbols specified
!                    by the string; it is assumed that the dimension of
!                    SYMBOL is big enough (not less than LEN(TEXT)).
!  TEXT   (input)  : the text string to be decoded.
!  FONT   (input)  : the font number (1..4) to be used for decoding the
!                    string (this can be overridden by an escape
!                    sequence within the string).
!--
!  3-May-1983 - [TJP].
! 13-Jun-1984 - add \A [TJP].
! 15-Dec-1988 - standardize [TJP].
! 29-Nov-1990 - add \m escapes [TJP].
! 27-Nov-1991 - add \x escape [TJP].
! 27-Jul-1995 - extend for 256-character set [TJP]
!  7-Nov-1995 - add \. escape [TJP].
!-----------------------------------------------------------------------
!
   INTEGER, INTENT(OUT)                     :: symbol(*)
   INTEGER, INTENT(OUT)                     :: nsymbs
   CHARACTER (LEN=*), INTENT(IN)            :: text
   INTEGER, INTENT(IN)                      :: font
!
   CHARACTER (LEN=8), PARAMETER :: fonts='nrisNRIS'
   CHARACTER (LEN=48), PARAMETER :: greek='ABGDEZYHIKLMNCOPRSTUFXQWabgdezyhiklmncoprstufxqw'
   INTEGER :: ch,ig,j,lentxt,ifont,mark
!
! Initialize parameters.
!
   ifont=font
   lentxt=LEN(text)
   nsymbs=0
   j=0
!
! Get next character; treat non-printing characters as spaces.
!
10 j=j+1
   IF(j > lentxt)RETURN
   ch=ICHAR(text(j:j))
   IF(ch < 0)ch=32
   IF(ch > 303)ch=32
!
! Test for escape sequence (\)
!
   IF(ch == 92)THEN
      IF((lentxt-j) >= 1)THEN
         IF(text(j+1:j+1) == CHAR(92))THEN
            j=j+1
         ELSE IF(text(j+1:j+1) == 'u'.OR.text(j+1:j+1) == 'U')THEN
            nsymbs=nsymbs+1
            symbol(nsymbs)=-1
            j=j+1
            GO TO 10
         ELSE IF(text(j+1:j+1) == 'd'.OR.text(j+1:j+1) == 'D')THEN
            nsymbs=nsymbs+1
            symbol(nsymbs)=-2
            j=j+1
            GO TO 10
         ELSE IF(text(j+1:j+1) == 'b'.OR.text(j+1:j+1) == 'B')THEN
            nsymbs=nsymbs+1
            symbol(nsymbs)=-3
            j=j+1
            GO TO 10
         ELSE IF(text(j+1:j+1) == 'A')THEN
            nsymbs=nsymbs+1
            symbol(nsymbs)=2078
            j=j+1
            GO TO 10
         ELSE IF(text(j+1:j+1) == 'x')THEN
            nsymbs=nsymbs+1
            symbol(nsymbs)=2235
            IF(ifont == 1)symbol(nsymbs)=727
            j=j+1
            GO TO 10
         ELSE IF(text(j+1:j+1) == '.')THEN
            nsymbs=nsymbs+1
            symbol(nsymbs)=2236
            IF(ifont == 1)symbol(nsymbs)=729
            j=j+1
            GO TO 10
         ELSE IF(text(j+1:j+1) == '(')THEN
            nsymbs=nsymbs+1
            symbol(nsymbs)=0
            j=j+2
!
!               -- DO WHILE ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9')
!
20          IF('0' <= text(j:j).AND.text(j:j) <= '9')THEN
               symbol(nsymbs)=symbol(nsymbs)*10+ICHAR(text(j:j))-ICHAR('0')
               j=j+1
               GO TO 20
            END IF
!
!               -- end DO WHILE
!
            IF(text(j:j) /= ')')j=j-1
            GO TO 10
         ELSE IF(text(j+1:j+1) == 'm'.OR.text(j+1:j+1) == 'M')THEN
            mark=0
            j=j+2
            IF('0' <= text(j:j).AND.text(j:j) <= '9')THEN
               mark=mark*10+ICHAR(text(j:j))-ICHAR('0')
               j=j+1
            END IF
            IF('0' <= text(j:j).AND.text(j:j) <= '9')THEN
               mark=mark*10+ICHAR(text(j:j))-ICHAR('0')
               j=j+1
            END IF
            j=j-1
            nsymbs=nsymbs+1
            CALL grsymk (mark,ifont,symbol(nsymbs))
            GO TO 10
         ELSE IF(text(j+1:j+1) == 'f'.OR.text(j+1:j+1) == 'F')THEN
            ifont=INDEX(fonts,text(j+2:j+2))
            IF(ifont > 4)ifont=ifont-4
            IF(ifont == 0)ifont=1
            j=j+2
            GO TO 10
         ELSE IF(text(j+1:j+1) == 'g'.OR.text(j+1:j+1) == 'G')THEN
            ig=INDEX(greek,text(j+2:j+2))
            nsymbs=nsymbs+1
            CALL grsymk(255+ig,ifont,symbol(nsymbs))
            j=j+2
            GO TO 10
         END IF
      END IF
   END IF
!
! Decode character.
!
   nsymbs=nsymbs+1
   CALL grsymk(ch,ifont,symbol(nsymbs))
   GO TO 10
!
   RETURN
!
END SUBROUTINE grsyds
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSYMK -- convert character number into symbol number
!+
SUBROUTINE grsymk(code,font,symbol)
!
! This routine returns the Hershey symbol number (SYMBOL) corresponding
! to ASCII code CODE in font FONT.
!
! Characters 0-31 are the same in all fonts, and are the standard
! graph markers. Characters 32-127 are standard representations of
! the ASCII codes. Characters 128-255 are reserved for the upper
! half of the ISO Latin-1 character set. Characters 256-303 are
! used for the greek alphabet.
!
! Arguments:
!  CODE   (input)  : the extended ASCII code number.
!  FONT   (input)  : the font to be used 31 (range 1-4).
!  SYMBOL (output) : the number of the symbol to be plotted.
!--
! 24-Apr-1986.
! 15-Dec-1988 - standardize [TJP].
! 29-Nov-1990 - eliminate common block [TJP].
! 27-Nov-1991 - correct code for backslash [TJP].
! 27-Jul-1995 - extend for 256-character set; add some defaults for
!               ISO Latin-1 (full glyph set not available) [TJP].
!-----------------------------------------------------------------------
!
   INTEGER, INTENT(IN)                      :: code
   INTEGER, INTENT(IN)                      :: font
   INTEGER, INTENT(OUT)                     :: symbol
!
   INTEGER :: i,k,hersh(0:303,4)
   SAVE  hersh
!
! Special characters (graph markers).
!
   DATA (hersh(0,k),k=1,4)/841,841,841,841/
   DATA (hersh(1,k),k=1,4)/899,899,899,899/
   DATA (hersh(2,k),k=1,4)/845,845,845,845/
   DATA (hersh(3,k),k=1,4)/847,847,847,847/
   DATA (hersh(4,k),k=1,4)/840,840,840,840/
   DATA (hersh(5,k),k=1,4)/846,846,846,846/
   DATA (hersh(6,k),k=1,4)/841,841,841,841/
   DATA (hersh(7,k),k=1,4)/842,842,842,842/
   DATA (hersh(8,k),k=1,4)/2284,2284,2284,2284/
   DATA (hersh(9,k),k=1,4)/2281,2281,2281,2281/
   DATA (hersh(10,k),k=1,4)/735,735,735,735/
   DATA (hersh(11,k),k=1,4)/843,843,843,843/
   DATA (hersh(12,k),k=1,4)/844,844,844,844/
   DATA (hersh(13,k),k=1,4)/852,852,852,852/
   DATA (hersh(14,k),k=1,4)/866,866,866,866/
   DATA (hersh(15,k),k=1,4)/868,868,868,868/
   DATA (hersh(16,k),k=1,4)/851,851,851,851/
   DATA (hersh(17,k),k=1,4)/850,850,850,850/
   DATA (hersh(18,k),k=1,4)/856,856,856,856/
   DATA (hersh(19,k),k=1,4)/254,254,254,254/
   DATA (hersh(20,k),k=1,4)/900,900,900,900/
   DATA (hersh(21,k),k=1,4)/901,901,901,901/
   DATA (hersh(22,k),k=1,4)/902,902,902,902/
   DATA (hersh(23,k),k=1,4)/903,903,903,903/
   DATA (hersh(24,k),k=1,4)/904,904,904,904/
   DATA (hersh(25,k),k=1,4)/905,905,905,905/
   DATA (hersh(26,k),k=1,4)/906,906,906,906/
   DATA (hersh(27,k),k=1,4)/907,907,907,907/
   DATA (hersh(28,k),k=1,4)/2263,2263,2263,2263/
   DATA (hersh(29,k),k=1,4)/2261,2261,2261,2261/
   DATA (hersh(30,k),k=1,4)/2262,2262,2262,2262/
   DATA (hersh(31,k),k=1,4)/2264,2264,2264,2264/
!
! US-ASCII (ISO Latin-1 lower half).
!
!   32:39 space exclam quotdbl numbersign
!         dollar percent ampersand quoteright
!
   DATA (hersh(32,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(33,k),k=1,4)/714,2214,2764,2764/
   DATA (hersh(34,k),k=1,4)/717,2217,2778,2778/
   DATA (hersh(35,k),k=1,4)/733,2275,2275,2275/
   DATA (hersh(36,k),k=1,4)/719,2274,2769,2769/
   DATA (hersh(37,k),k=1,4)/2271,2271,2271,2271/
   DATA (hersh(38,k),k=1,4)/734,2272,2768,2768/
   DATA (hersh(39,k),k=1,4)/716,2216,2777,2777/
!
!   40:47 parenleft parenright asterisk plus
!         comma minus period slash
!
   DATA (hersh(40,k),k=1,4)/721,2221,2771,2771/
   DATA (hersh(41,k),k=1,4)/722,2222,2772,2772/
   DATA (hersh(42,k),k=1,4)/728,2219,2773,2773/
   DATA (hersh(43,k),k=1,4)/725,2232,2775,2775/
   DATA (hersh(44,k),k=1,4)/711,2211,2761,2761/
   DATA (hersh(45,k),k=1,4)/724,2231,2774,2774/
   DATA (hersh(46,k),k=1,4)/710,2210,2760,2760/
   DATA (hersh(47,k),k=1,4)/720,2220,2770,2770/
!
!   48:55 zero one two three four five six seven
!
   DATA (hersh(48,k),k=1,4)/700,2200,2750,2750/
   DATA (hersh(49,k),k=1,4)/701,2201,2751,2751/
   DATA (hersh(50,k),k=1,4)/702,2202,2752,2752/
   DATA (hersh(51,k),k=1,4)/703,2203,2753,2753/
   DATA (hersh(52,k),k=1,4)/704,2204,2754,2754/
   DATA (hersh(53,k),k=1,4)/705,2205,2755,2755/
   DATA (hersh(54,k),k=1,4)/706,2206,2756,2756/
   DATA (hersh(55,k),k=1,4)/707,2207,2757,2757/
!
!   56:63 eight nine colon semicolon less equal greater question
!
   DATA (hersh(56,k),k=1,4)/708,2208,2758,2758/
   DATA (hersh(57,k),k=1,4)/709,2209,2759,2759/
   DATA (hersh(58,k),k=1,4)/712,2212,2762,2762/
   DATA (hersh(59,k),k=1,4)/713,2213,2763,2763/
   DATA (hersh(60,k),k=1,4)/2241,2241,2241,2241/
   DATA (hersh(61,k),k=1,4)/726,2238,2776,2776/
   DATA (hersh(62,k),k=1,4)/2242,2242,2242,2242/
   DATA (hersh(63,k),k=1,4)/715,2215,2765,2765/
!
!   64:71 at A B C D E F G
!
   DATA (hersh(64,k),k=1,4)/2273,2273,2273,2273/
   DATA (hersh(65,k),k=1,4)/501,2001,2051,2551/
   DATA (hersh(66,k),k=1,4)/502,2002,2052,2552/
   DATA (hersh(67,k),k=1,4)/503,2003,2053,2553/
   DATA (hersh(68,k),k=1,4)/504,2004,2054,2554/
   DATA (hersh(69,k),k=1,4)/505,2005,2055,2555/
   DATA (hersh(70,k),k=1,4)/506,2006,2056,2556/
   DATA (hersh(71,k),k=1,4)/507,2007,2057,2557/
!
!   72:79 H I J K L M N O
!
   DATA (hersh(72,k),k=1,4)/508,2008,2058,2558/
   DATA (hersh(73,k),k=1,4)/509,2009,2059,2559/
   DATA (hersh(74,k),k=1,4)/510,2010,2060,2560/
   DATA (hersh(75,k),k=1,4)/511,2011,2061,2561/
   DATA (hersh(76,k),k=1,4)/512,2012,2062,2562/
   DATA (hersh(77,k),k=1,4)/513,2013,2063,2563/
   DATA (hersh(78,k),k=1,4)/514,2014,2064,2564/
   DATA (hersh(79,k),k=1,4)/515,2015,2065,2565/
!
!   80:87 P Q R S T U V W
!
   DATA (hersh(80,k),k=1,4)/516,2016,2066,2566/
   DATA (hersh(81,k),k=1,4)/517,2017,2067,2567/
   DATA (hersh(82,k),k=1,4)/518,2018,2068,2568/
   DATA (hersh(83,k),k=1,4)/519,2019,2069,2569/
   DATA (hersh(84,k),k=1,4)/520,2020,2070,2570/
   DATA (hersh(85,k),k=1,4)/521,2021,2071,2571/
   DATA (hersh(86,k),k=1,4)/522,2022,2072,2572/
   DATA (hersh(87,k),k=1,4)/523,2023,2073,2573/
!
!   88:95 X Y Z bracketleft
!         backslash bracketright asciicircum underscore
!
   DATA (hersh(88,k),k=1,4)/524,2024,2074,2574/
   DATA (hersh(89,k),k=1,4)/525,2025,2075,2575/
   DATA (hersh(90,k),k=1,4)/526,2026,2076,2576/
   DATA (hersh(91,k),k=1,4)/2223,2223,2223,2223/
   DATA (hersh(92,k),k=1,4)/804,804,804,804/
   DATA (hersh(93,k),k=1,4)/2224,2224,2224,2224/
   DATA (hersh(94,k),k=1,4)/718,2218,2779,2779/
   DATA (hersh(95,k),k=1,4)/590,590,590,590/
!
!   96:103 quoteleft a b c d e f g
!
   DATA (hersh(96,k),k=1,4)/2249,2249,2249,2249/
   DATA (hersh(97,k),k=1,4)/601,2101,2151,2651/
   DATA (hersh(98,k),k=1,4)/602,2102,2152,2652/
   DATA (hersh(99,k),k=1,4)/603,2103,2153,2653/
   DATA (hersh(100,k),k=1,4)/604,2104,2154,2654/
   DATA (hersh(101,k),k=1,4)/605,2105,2155,2655/
   DATA (hersh(102,k),k=1,4)/606,2106,2156,2656/
   DATA (hersh(103,k),k=1,4)/607,2107,2157,2657/
!
!  104:111 h i j k l m n o
!
   DATA (hersh(104,k),k=1,4)/608,2108,2158,2658/
   DATA (hersh(105,k),k=1,4)/609,2109,2159,2659/
   DATA (hersh(106,k),k=1,4)/610,2110,2160,2660/
   DATA (hersh(107,k),k=1,4)/611,2111,2161,2661/
   DATA (hersh(108,k),k=1,4)/612,2112,2162,2662/
   DATA (hersh(109,k),k=1,4)/613,2113,2163,2663/
   DATA (hersh(110,k),k=1,4)/614,2114,2164,2664/
   DATA (hersh(111,k),k=1,4)/615,2115,2165,2665/
!
!  112:119 p q r s t u v w
!
   DATA (hersh(112,k),k=1,4)/616,2116,2166,2666/
   DATA (hersh(113,k),k=1,4)/617,2117,2167,2667/
   DATA (hersh(114,k),k=1,4)/618,2118,2168,2668/
   DATA (hersh(115,k),k=1,4)/619,2119,2169,2669/
   DATA (hersh(116,k),k=1,4)/620,2120,2170,2670/
   DATA (hersh(117,k),k=1,4)/621,2121,2171,2671/
   DATA (hersh(118,k),k=1,4)/622,2122,2172,2672/
   DATA (hersh(119,k),k=1,4)/623,2123,2173,2673/
!
!  120:127 x y z braceleft bar braceright asciitilde -
!
   DATA (hersh(120,k),k=1,4)/624,2124,2174,2674/
   DATA (hersh(121,k),k=1,4)/625,2125,2175,2675/
   DATA (hersh(122,k),k=1,4)/626,2126,2176,2676/
   DATA (hersh(123,k),k=1,4)/2225,2225,2225,2225/
   DATA (hersh(124,k),k=1,4)/723,2229,2229,2229/
   DATA (hersh(125,k),k=1,4)/2226,2226,2226,2226/
   DATA (hersh(126,k),k=1,4)/2246,2246,2246,2246/
   DATA (hersh(127,k),k=1,4)/699,2199,2199,2199/
!
! ISO Latin-1 upper half.
!
!  128:135 - - - - - - - -
!
   DATA (hersh(128,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(129,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(130,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(131,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(132,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(133,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(134,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(135,k),k=1,4)/699,2199,2199,2199/
!
!  136:143 - - - - - - - -
!
   DATA (hersh(136,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(137,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(138,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(139,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(140,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(141,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(142,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(143,k),k=1,4)/699,2199,2199,2199/
!
!   144:151 dotlessi grave acute circumflex tilde - breve dotaccent
!
   DATA (hersh(144,k),k=1,4)/699,2182,2196,2199/
   DATA (hersh(145,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(146,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(147,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(148,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(149,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(150,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(151,k),k=1,4)/699,2199,2199,2199/
!
!   152:159 dieresis - ring - - - - -
!
   DATA (hersh(152,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(153,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(154,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(155,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(156,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(157,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(158,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(159,k),k=1,4)/699,2199,2199,2199/
!
!   160:167 space exclamdown cent sterling currency yen brokenbar section
!
   DATA (hersh(160,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(161,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(162,k),k=1,4)/910,910,910,910/
   DATA (hersh(163,k),k=1,4)/272,272,272,272/
   DATA (hersh(164,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(165,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(166,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(167,k),k=1,4)/2276,2276,2276,2276/
!
!   168:175 - copyright - - - - registered -
!
   DATA (hersh(168,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(169,k),k=1,4)/274,274,274,274/
   DATA (hersh(170,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(171,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(172,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(173,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(174,k),k=1,4)/273,273,273,273/
   DATA (hersh(175,k),k=1,4)/699,2199,2199,2199/
!
!   176:183 degree plusminus twosuperior threesuperior
!           acute mu paragraph periodcentered
!
   DATA (hersh(176,k),k=1,4)/718,2218,2779,2779/
   DATA (hersh(177,k),k=1,4)/2233,2233,2233,2233/
   DATA (hersh(178,k),k=1,4)/702,2202,2752,2752/
   DATA (hersh(179,k),k=1,4)/703,2203,2753,2753/
   DATA (hersh(180,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(181,k),k=1,4)/638,2138,2138,2138/
   DATA (hersh(182,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(183,k),k=1,4)/729,729,729,729/
!
!   184:191 cedilla onesuperior ordmasculine guillemotright
!           onequarter onehalf threequarters questiondown
!
   DATA (hersh(184,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(185,k),k=1,4)/701,2201,2751,2751/
   DATA (hersh(186,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(187,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(188,k),k=1,4)/270,270,270,270/
   DATA (hersh(189,k),k=1,4)/261,261,261,261/
   DATA (hersh(190,k),k=1,4)/271,271,271,271/
   DATA (hersh(191,k),k=1,4)/699,2199,2199,2199/
!
!   192:199 Agrave Aacute Acircumflex Atilde Aring AE Ccedilla
!
   DATA (hersh(192,k),k=1,4)/501,2001,2051,2551/
   DATA (hersh(193,k),k=1,4)/501,2001,2051,2551/
   DATA (hersh(194,k),k=1,4)/501,2001,2051,2551/
   DATA (hersh(195,k),k=1,4)/501,2001,2051,2551/
   DATA (hersh(196,k),k=1,4)/501,2001,2051,2551/
   DATA (hersh(197,k),k=1,4)/501,2078,2051,2551/
   DATA (hersh(198,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(199,k),k=1,4)/503,2003,2053,2553/
!
!   200:207 Egrave Eacute Ecircumflex Edieresis
!           Igrave Iacute Icircumflex Idieresis
!
   DATA (hersh(200,k),k=1,4)/505,2005,2055,2555/
   DATA (hersh(201,k),k=1,4)/505,2005,2055,2555/
   DATA (hersh(202,k),k=1,4)/505,2005,2055,2555/
   DATA (hersh(203,k),k=1,4)/505,2005,2055,2555/
   DATA (hersh(204,k),k=1,4)/509,2009,2059,2559/
   DATA (hersh(205,k),k=1,4)/509,2009,2059,2559/
   DATA (hersh(206,k),k=1,4)/509,2009,2059,2559/
   DATA (hersh(207,k),k=1,4)/509,2009,2059,2559/
!
!   208:215 Eth Ntilde Ograve Oacute
!           Ocircumflex Otilde Odieresis multiply
!
   DATA (hersh(208,k),k=1,4)/504,2004,2054,2554/
   DATA (hersh(209,k),k=1,4)/514,2014,2064,2564/
   DATA (hersh(210,k),k=1,4)/515,2015,2065,2565/
   DATA (hersh(211,k),k=1,4)/515,2015,2065,2565/
   DATA (hersh(212,k),k=1,4)/515,2015,2065,2565/
   DATA (hersh(213,k),k=1,4)/515,2015,2065,2565/
   DATA (hersh(214,k),k=1,4)/515,2015,2065,2565/
   DATA (hersh(215,k),k=1,4)/2235,2235,2235,2235/
!
!   216:223 Oslash Ugrave Uacute Ucircumflex
!           Udieresis Yacute Thorn germandbls
!
   DATA (hersh(216,k),k=1,4)/515,2015,2065,2565/
   DATA (hersh(217,k),k=1,4)/521,2021,2071,2571/
   DATA (hersh(218,k),k=1,4)/521,2021,2071,2571/
   DATA (hersh(219,k),k=1,4)/521,2021,2071,2571/
   DATA (hersh(220,k),k=1,4)/521,2021,2071,2571/
   DATA (hersh(221,k),k=1,4)/525,2025,2075,2575/
   DATA (hersh(222,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(223,k),k=1,4)/699,2199,2199,2199/
!
!   224:231 agrave aacute acircumflex atilde aring ae ccedilla
!
   DATA (hersh(224,k),k=1,4)/601,2101,2151,2651/
   DATA (hersh(225,k),k=1,4)/601,2101,2151,2651/
   DATA (hersh(226,k),k=1,4)/601,2101,2151,2651/
   DATA (hersh(227,k),k=1,4)/601,2101,2151,2651/
   DATA (hersh(228,k),k=1,4)/601,2101,2151,2651/
   DATA (hersh(229,k),k=1,4)/601,2101,2151,2651/
   DATA (hersh(230,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(231,k),k=1,4)/603,2103,2153,2653/
!
!   232:239 egrave eacute ecircumflex edieresis
!           igrave iacute icircumflex idieresis
!
   DATA (hersh(232,k),k=1,4)/605,2105,2155,2655/
   DATA (hersh(233,k),k=1,4)/605,2105,2155,2655/
   DATA (hersh(234,k),k=1,4)/605,2105,2155,2655/
   DATA (hersh(235,k),k=1,4)/605,2105,2155,2655/
   DATA (hersh(236,k),k=1,4)/609,2109,2159,2659/
   DATA (hersh(237,k),k=1,4)/609,2109,2159,2659/
   DATA (hersh(238,k),k=1,4)/609,2109,2159,2659/
   DATA (hersh(239,k),k=1,4)/609,2109,2159,2659/
!
!   240:247 eth ntilde ograve oacute
!           ocircumflex otilde odieresis divide
!
   DATA (hersh(240,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(241,k),k=1,4)/614,2114,2164,2664/
   DATA (hersh(242,k),k=1,4)/615,2115,2165,2665/
   DATA (hersh(243,k),k=1,4)/615,2115,2165,2665/
   DATA (hersh(244,k),k=1,4)/615,2115,2165,2665/
   DATA (hersh(245,k),k=1,4)/615,2115,2165,2665/
   DATA (hersh(246,k),k=1,4)/615,2115,2165,2665/
   DATA (hersh(247,k),k=1,4)/2237,2237,2237,2237/
!
!   248:255 oslash ugrave uacute ucircumflex
!           udieresis yacute thorn ydieresis
!
   DATA (hersh(248,k),k=1,4)/615,2115,2165,2665/
   DATA (hersh(249,k),k=1,4)/621,2121,2171,2671/
   DATA (hersh(250,k),k=1,4)/621,2121,2171,2671/
   DATA (hersh(251,k),k=1,4)/621,2121,2171,2671/
   DATA (hersh(252,k),k=1,4)/621,2121,2171,2671/
   DATA (hersh(253,k),k=1,4)/625,2125,2175,2675/
   DATA (hersh(254,k),k=1,4)/699,2199,2199,2199/
   DATA (hersh(255,k),k=1,4)/625,2125,2175,2675/
!
! Greek alphabet.
!
   DATA (hersh(256,k),k=1,4)/527,2027,2027,2027/
   DATA (hersh(257,k),k=1,4)/528,2028,2028,2028/
   DATA (hersh(258,k),k=1,4)/529,2029,2029,2029/
   DATA (hersh(259,k),k=1,4)/530,2030,2030,2030/
   DATA (hersh(260,k),k=1,4)/531,2031,2031,2031/
   DATA (hersh(261,k),k=1,4)/532,2032,2032,2032/
   DATA (hersh(262,k),k=1,4)/533,2033,2033,2033/
   DATA (hersh(263,k),k=1,4)/534,2034,2034,2034/
   DATA (hersh(264,k),k=1,4)/535,2035,2035,2035/
   DATA (hersh(265,k),k=1,4)/536,2036,2036,2036/
   DATA (hersh(266,k),k=1,4)/537,2037,2037,2037/
   DATA (hersh(267,k),k=1,4)/538,2038,2038,2038/
   DATA (hersh(268,k),k=1,4)/539,2039,2039,2039/
   DATA (hersh(269,k),k=1,4)/540,2040,2040,2040/
   DATA (hersh(270,k),k=1,4)/541,2041,2041,2041/
   DATA (hersh(271,k),k=1,4)/542,2042,2042,2042/
   DATA (hersh(272,k),k=1,4)/543,2043,2043,2043/
   DATA (hersh(273,k),k=1,4)/544,2044,2044,2044/
   DATA (hersh(274,k),k=1,4)/545,2045,2045,2045/
   DATA (hersh(275,k),k=1,4)/546,2046,2046,2046/
   DATA (hersh(276,k),k=1,4)/547,2047,2047,2047/
   DATA (hersh(277,k),k=1,4)/548,2048,2048,2048/
   DATA (hersh(278,k),k=1,4)/549,2049,2049,2049/
   DATA (hersh(279,k),k=1,4)/550,2050,2050,2050/
   DATA (hersh(280,k),k=1,4)/627,2127,2127,2127/
   DATA (hersh(281,k),k=1,4)/628,2128,2128,2128/
   DATA (hersh(282,k),k=1,4)/629,2129,2129,2129/
   DATA (hersh(283,k),k=1,4)/630,2130,2130,2130/
   DATA (hersh(284,k),k=1,4)/684,2184,2184,2184/
   DATA (hersh(285,k),k=1,4)/632,2132,2132,2132/
   DATA (hersh(286,k),k=1,4)/633,2133,2133,2133/
   DATA (hersh(287,k),k=1,4)/685,2185,2185,2185/
   DATA (hersh(288,k),k=1,4)/635,2135,2135,2135/
   DATA (hersh(289,k),k=1,4)/636,2136,2136,2136/
   DATA (hersh(290,k),k=1,4)/637,2137,2137,2137/
   DATA (hersh(291,k),k=1,4)/638,2138,2138,2138/
   DATA (hersh(292,k),k=1,4)/639,2139,2139,2139/
   DATA (hersh(293,k),k=1,4)/640,2140,2140,2140/
   DATA (hersh(294,k),k=1,4)/641,2141,2141,2141/
   DATA (hersh(295,k),k=1,4)/642,2142,2142,2142/
   DATA (hersh(296,k),k=1,4)/643,2143,2143,2143/
   DATA (hersh(297,k),k=1,4)/644,2144,2144,2144/
   DATA (hersh(298,k),k=1,4)/645,2145,2145,2145/
   DATA (hersh(299,k),k=1,4)/646,2146,2146,2146/
   DATA (hersh(300,k),k=1,4)/686,2186,2186,2186/
   DATA (hersh(301,k),k=1,4)/648,2148,2148,2148/
   DATA (hersh(302,k),k=1,4)/649,2149,2149,2149/
   DATA (hersh(303,k),k=1,4)/650,2150,2150,2150/
!
   IF((code < 0).OR.(code > 303))THEN
      i=1
   ELSE
      i=code
   END IF
   symbol=hersh(i,font)
!
END SUBROUTINE grsymk
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRSYXD -- obtain the polyline representation of a given symbol
!+
SUBROUTINE grsyxd(symbol,xygrid,unused)
!
! Return the digitization coordinates of a character. Each character is
! defined on a grid with X and Y coordinates in the range (-49,49),
! with the origin (0,0) at the center of the character.  The coordinate
! system is right-handed, with X positive to the right, and Y positive
! upward.
!
! Arguments:
!  SYMBOL (input)  : symbol number in range (1..3000).
!  XYGRID (output) : height range, width range, and pairs of (x,y)
!                    coordinates returned.  Height range = (XYGRID(1),
!                    XYGRID(3)).  Width range = (XYGRID(4),XYGRID(5)).
!                    (X,Y) = (XYGRID(K),XYGRID(K+1)) (K=6,8,...).
!  UNUSED (output) : receives .TRUE. if SYMBOL is an unused symbol
!                    number. A character of normal height and zero width
!                    is returned. Receives .FALSE. if SYMBOL is a
!                    valid symbol number.
!
! The height range consists of 3 values: (minimum Y, baseline Y,
! maximum Y).  The first is reached by descenders on lower-case g, p,
! q, and y.  The second is the bottom of upper-case letters.  The third
! is the top of upper-case letters.  A coordinate pair (-64,0) requests
! a pen raise, and a pair (-64,-64) terminates the coordinate list. It
! is assumed that movement to the first coordinate position will be
! done with the pen raised - no raise command is explicitly included to
! do this.
!--
!  7-Mar-1983.
! 15-Dec-1988 - standardize.
!-----------------------------------------------------------------------
!
   USE grsy00
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: symbol
   INTEGER, INTENT(OUT)                     :: xygrid(300)
   LOGICAL, INTENT(OUT)                     :: unused
!
!INTEGER*2 buffer(27000)
!INTEGER :: INDEX(3000)
!INTEGER :: nc1,nc2
   INTEGER  :: ix,iy,k,l,locbuf
!
!COMMON /grsymb/ nc1,nc2,INDEX,buffer
!
! Extract digitization.
!
   IF(symbol < nc1.OR.symbol > nc2)GO TO 20
   l=symbol-nc1+1
   locbuf=INDEX(l)
   IF(locbuf == 0)GO TO 20
   xygrid(1)=buffer(locbuf)
   locbuf=locbuf+1
   k=2
   iy=-1
!
!     -- DO WHILE (IY.NE.-64)
!
10 IF(iy /= -64)THEN
      ix=buffer(locbuf)/128
      iy=buffer(locbuf)-128*ix-64
      xygrid(k)=ix-64
      xygrid(k+1)=iy
      k=k+2
      locbuf=locbuf+1
      GO TO 10
   END IF
!
!     -- end DO WHILE
!
   unused=.false.
   RETURN
!
! Unimplemented character.
!
20 xygrid(1)=-16
   xygrid(2)=-9
   xygrid(3)=+12
   xygrid(4)=0
   xygrid(5)=0
   xygrid(6)=-64
   xygrid(7)=-64
   unused=.true.
   RETURN
!
END SUBROUTINE grsyxd
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRTERM -- flush buffer to output device
!+
SUBROUTINE grterm
!
! GRPCKG: flush the buffer associated with the current plot. GRTERM
! should be called only when it is necessary to make sure that all the
! graphics created up to this point in the program are visible on the
! device, e.g., before beginning a dialog with the user. GRTERM has no
! effect on hardcopy devices.
!
! Arguments: none.
!--
!  6-Oct-1983
! 29-Jan-1985 - add HP2648 device [KS/TJP].
! 31-Dec-1985 - do not send CAN code to true Tek [TJP/PCP].
!  5-Aug-1986 - add GREXEC support [AFT].
! 11-Jun-1987 - remove built-in devices [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER :: nbuf,lchr
   REAL(KIND=pg) :: rbuf(6)
   CHARACTER (LEN=1) :: chr
!
   IF(grcide >= 1)THEN
      CALL grexec(grgtyp,16,rbuf,nbuf,chr,lchr)
   END IF
!
END SUBROUTINE grterm
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRTEXT -- draw text
!+
SUBROUTINE grtext(center,orient,absxy,x0,y0,string)
!
! GRPCKG: Write a text string using the high-quality character set.
! The text is NOT windowed in the current viewport, but may extend over
! the whole view surface.  Line attributes (color, intensity thickness)
! apply to text, but line-style is ignored.  The current pen position
! after a call to GRTEXT is undefined.
!
! Arguments:
!
! STRING (input, character): the character string to be plotted. This
!       may include standard escape-sequences to represent non-ASCII
!       characters and special commands. The number of characters in
!       STRING (i.e., LEN(STRING)) should not exceed 256.
!--
! (3-May-1983)
!  5-Aug-1986 - add GREXEC support [AFT].
!  6-Sep-1989 - standardize [TJP].
! 20-Apr-1995 - Verbose PS file support.  If PGPLOT_PS_VERBOSE_TEXT is
!               defined, text strings in PS files are preceded by a
!               comment with the text of the string plotted as vectors
!               [TJP after D.S.Briggs].
!  4-Feb-1997 - grexec requires an RBUF array, not a scalar [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   LOGICAL, INTENT(IN)                      :: center
   REAL(KIND=pg), INTENT(IN)                :: orient
   LOGICAL, INTENT(IN)                      :: absxy
   REAL(KIND=pg), INTENT(IN)                :: x0
   REAL(KIND=pg), INTENT(IN)                :: y0
   CHARACTER (LEN=*), INTENT(IN)            :: string
   LOGICAL :: unused,visble
   INTEGER :: xygrid(300)
   INTEGER :: list(256)
!
   REAL(KIND=pg) :: angle,factor,fntbas,fntfac,cosa,sina,dx,dy,xorg,yorg
   REAL(KIND=pg) :: xcur,ycur,ratio,rlx,rly
   REAL(KIND=pg) :: xmin,xmax,ymin,ymax
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: i,ifntlv,nlist,lx,ly,k,lxlast,lylast,lstyle
   INTEGER :: slen,grtrim
!
!   JAO:  add integer variable zero whose value will be zero,
!         and add integer variable templength
!
   INTEGER ::  zero,templength
!
!INTRINSIC  ABS,COS,LEN,MIN,SIN
!
   CHARACTER(LEN=14) :: devtyp
   CHARACTER(LEN=258)  :: stemp
!
   LOGICAL :: devint,vtext
!
   zero=0
!
   IF(center)angle=0.0_pg
!
! Check that there is something to be plotted.
!
   IF(LEN(string) <= 0)RETURN
!
! Check that a device is selected.
!
   IF(grcide < 1)THEN
      CALL grwarn('GRTEXT - no graphics device is active.')
      RETURN
   END IF
!
! Save current line-style, and set style "normal".
!
   CALL grqls(lstyle)
   CALL grsls(1)
!
! Put device dependent code here or at end
!
   vtext=.false.
   CALL grqtyp(devtyp,devint)
   IF((devtyp == 'PS').OR.(devtyp == 'VPS').OR.(devtyp == 'CPS')  &
      .OR.(devtyp == 'VCPS'))THEN
      CALL grgenv('PS_VERBOSE_TEXT',stemp,i)
      vtext=(i > 0)
      IF(vtext)THEN
         slen=grtrim(string)
         stemp='% Start "'//string(1:slen)//'"'
!
!   JAO:  use the varible zero instead of the literal 0,
!         and also templength instead of slen+10.  These
!         changes remove having nonvariable expressions
!         as arguments that are INTENT(IN OUT) in the
!         subroutine
!
!    CALL grexec(grgtyp,23,rbuf,0,stemp,slen+10)
!
         templength=slen+10
         CALL grexec(grgtyp,23,rbuf,zero,stemp,templength)
      END IF
   END IF
!
! Save current viewport, and open the viewport to include the full
! view surface.
!
   xorg=grxpre(grcide)
   yorg=grypre(grcide)
   xmin=grxmin(grcide)
   xmax=grxmax(grcide)
   ymin=grymin(grcide)
   ymax=grymax(grcide)
   CALL grarea(grcide,0.0_pg,0.0_pg,0.0_pg,0.0_pg)
!
! Compute scaling and orientation.
!
   angle=orient*(3.141592653589793_pg/180.0_pg)
   factor=grcfac(grcide)/2.5_pg
   ratio=grpxpi(grcide)/grpypi(grcide)
   cosa=factor*COS(angle)
   sina=factor*SIN(angle)
   CALL grtxy0(absxy,x0,y0,xorg,yorg)
   fntbas=0.0_pg
   fntfac=1.0_pg
   ifntlv=0
   dx=0.0_pg
   dy=0.0_pg
!
! Convert the string to a list of symbol numbers; to prevent overflow
! of array LIST, the length of STRING is limited to 256 characters.
!
   CALL grsyds(list,nlist,string(1:MIN(256,LEN(string))),grcfnt(grcide))
!
! Plot the string of characters
!
   DO  i=1,nlist
      IF(list(i) < 0)THEN
         IF(list(i) == -1)THEN
!
!                 ! up
!
            ifntlv=ifntlv+1
            fntbas=fntbas+16.0_pg*fntfac
            fntfac=0.75_pg**ABS(ifntlv)
         ELSE IF(list(i) == -2)THEN
!
!                 ! down
!
            ifntlv=ifntlv-1
            fntfac=0.75_pg**ABS(ifntlv)
            fntbas=fntbas-16.0_pg*fntfac
         ELSE IF(list(i) == -3)THEN
!
!                 ! backspace
!
            xorg=xorg-dx*fntfac
            yorg=yorg-dy*fntfac
         END IF
         CYCLE
      END IF
      CALL grsyxd(list(i),xygrid,unused)
      visble=.false.
      lx=xygrid(5)-xygrid(4)
      dx=cosa*lx*ratio
      dy=sina*lx
      k=4
      lxlast=-64
      lylast=-64
10    k=k+2
      lx=xygrid(k)
      ly=xygrid(k+1)
      IF(ly == -64)GO TO 20
      IF(lx == -64)THEN
         visble=.false.
      ELSE
         rlx=(lx-xygrid(4))*fntfac
         rly=(ly-xygrid(2))*fntfac+fntbas
         IF((lx /= lxlast).OR.(ly /= lylast))THEN
            xcur=xorg+(cosa*rlx-sina*rly)*ratio
            ycur=yorg+(sina*rlx+cosa*rly)
            IF(visble)THEN
               CALL grlin0(xcur,ycur)
            ELSE
               grxpre(grcide)=xcur
               grypre(grcide)=ycur
            END IF
         END IF
         visble=.true.
         lxlast=lx
         lylast=ly
      END IF
      GO TO 10
20    xorg=xorg+dx*fntfac
      yorg=yorg+dy*fntfac
   END DO
!
! Set pen position ready for next character.
!
   grxpre(grcide)=xorg
   grypre(grcide)=yorg
!
! Another possible device dependent section
!
   IF(vtext)THEN
      stemp='% End "'//string(1:slen)//'"'
!
!   JAO:  use the varible zero instead of the literal 0, and
!         templength instead of slen+8
!
!  CALL grexec(grgtyp,23,rbuf,0,stemp,slen+8)
!
      templength=slen+8
      CALL grexec(grgtyp,23,rbuf,zero,stemp,templength)
   END IF
!
! Restore the viewport and line-style, and return.
!
   grxmin(grcide)=xmin
   grxmax(grcide)=xmax
   grymin(grcide)=ymin
   grymax(grcide)=ymax
   CALL grsls(lstyle)
!
END SUBROUTINE grtext
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!

!*GRTOUP -- convert character string to upper case
!+
SUBROUTINE grtoup(dst,src)
!
! GRPCKG (internal routine): convert character string to upper case.
!
! Arguments:
!  DST    (output) : output string (upper case).
!  SRC    (input)  : input string to be converted.
!--
! 1988-Jan-18 (TJP)
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(OUT)        :: dst
   CHARACTER (LEN=*), INTENT(IN)        :: src
!
   INTEGER :: i,n,nchi,ncho,nch
!
   nchi=LEN(src)
   ncho=LEN(dst)
   nch=MIN(nchi,ncho)
   DO  i=1,nch
      n=ICHAR(src(i:i))
      IF((n >= 97).AND.(n <= 122))THEN
         dst(i:i)=CHAR(n-32)
      ELSE
         dst(i:i)=CHAR(n)
      END IF
   END DO
   IF(ncho > nchi)dst(nchi+1:ncho)=' '
!
END SUBROUTINE grtoup
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRTRAN -- define scaling transformation
!+
SUBROUTINE grtran(ident,xorg,yorg,xscale,yscale)
!
! GRPCKG (internal routine): Define scaling transformation.
!
! Arguments:
!
! IDENT (input, integer): plot identifier, as returned by GROPEN.
! XORG, YORG, XSCALE, YSCALE (input, real): parameters of the scaling
!       transformation. This is defined by:
!               XABS = XORG + XWORLD * XSCALE,
!               YABS = YORG + YWORLD * YSCALE,
!       where (XABS, YABS) are the absolute device coordinates
!       corresponding to world coordinates (XWORLD, YWORLD).
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: ident
   REAL(KIND=pg), INTENT(IN OUT)            :: xorg
   REAL(KIND=pg), INTENT(IN OUT)            :: yorg
   REAL(KIND=pg), INTENT(IN OUT)            :: xscale
   REAL(KIND=pg), INTENT(IN OUT)            :: yscale
!
   CALL grslct(ident)
   CALL grtrn0(xorg,yorg,xscale,yscale)
!
END SUBROUTINE grtran
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRTRIM -- length of string excluding trailing blanks
!+
FUNCTION grtrim(s)
!
! Find the length of a character string excluding trailing blanks.
! A blank string returns a value of 0.
!
! Argument:
!  S      (input)  : character string.
!
! Returns:
!  GRTRIM          : number of characters in S, excluding trailing
!                    blanks, in range 0...LEN(S). A blank string
!                    returns a value of 0.
!
! Subroutines required:
!  None
!
! Fortran 77 extensions:
!  None
!
! History:
!  1987 Nov 12 - TJP.
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: s
!
   INTEGER :: i,grtrim
!
   IF(s == ' ')THEN
      grtrim=0
   ELSE
      DO  i=LEN(s),1,-1
         grtrim=i
         IF(s(i:i) /= ' ')GO TO 20
      END DO
      grtrim=0
20    CONTINUE
   END IF
!
END FUNCTION grtrim
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRTRML -- get name of user's terminal (UNIX)
!+
SUBROUTINE grtrml(string,l)
!
! Return the device name of the user's terminal, if any. In Sun/Convex-UNIX,
! the name of the terminal is always /dev/tty.
!
! Arguments:
!  STRING : receives the terminal name, truncated or extended with
!           blanks as necessary.
!  L      : receives the number of characters in STRING, excluding
!           trailing blanks. If there is not attached terminal,
!           zero is returned.
!--
! 19-Jan-1988
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(OUT)           :: string
   INTEGER, INTENT(OUT)                     :: l
!
   string='/dev/tty'
   l=MIN(LEN(string),8)
!
END SUBROUTINE grtrml
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRTRN0 -- define scaling transformation
!+
SUBROUTINE grtrn0(xorg,yorg,xscale,yscale)
!
! GRPCKG (internal routine): Define scaling transformation for current
! device (equivalent to GRTRAN without device selection).
!
! Arguments:
!
! XORG, YORG, XSCALE, YSCALE (input, real): parameters of the scaling
!       transformation. This is defined by:
!               XABS = XORG + XWORLD * XSCALE,
!               YABS = YORG + YWORLD * YSCALE,
!       where (XABS, YABS) are the absolute device coordinates
!       corresponding to world coordinates (XWORLD, YWORLD).
!--
!  1-Feb-83:
! 11-Feb-92: Add driver support (TJP).
!  1-Sep-94: Suppress driver call (TJP).
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: xorg
   REAL(KIND=pg), INTENT(IN)                         :: yorg
   REAL(KIND=pg), INTENT(IN)                         :: xscale
   REAL(KIND=pg), INTENT(IN)                         :: yscale
!
   REAL(KIND=pg) :: rbuf(6)
   INTEGER :: nbuf,lchr
   CHARACTER (LEN=16) :: chr
!
   grxorg(grcide)=xorg
   grxscl(grcide)=xscale
   gryorg(grcide)=yorg
   gryscl(grcide)=yscale
!
! Pass info to device driver?
!
   IF(grgcap(grcide)(2:2) == 'X')THEN
      rbuf(1)=xorg
      rbuf(2)=xscale
      rbuf(3)=yorg
      rbuf(4)=yscale
      nbuf=4
      lchr=0
      CALL grexec(grgtyp,27,rbuf,nbuf,chr,lchr)
   END IF
!
   RETURN
!
END SUBROUTINE grtrn0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRTTER -- test whether device is user's terminal (Sun/Convex-UNIX)
!+
SUBROUTINE grtter(string,same)
!
! Return a logical flag indicating whether the supplied device
! name is a name for the user's controlling terminal or not.
! (Some PGPLOT programs wish to take special action if they are
! plotting on the user's terminal.)
!
! Arguments:
!  STRING : (input) the device name to be tested.
!  SAME   : (output) .TRUE. is STRING contains a valid name for the
!           user's terminal; .FALSE. otherwise.
!--
! 18-Feb-1988
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN OUT)        :: string
   LOGICAL, INTENT(IN OUT)                  :: same
!
   CHARACTER (LEN=64) :: t
   INTEGER :: l
!
   CALL grtrml(t,l)
   same=(string == t(:l))
!
END SUBROUTINE grtter
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRTXY0 -- convert world coordinates to device coordinates
!+
SUBROUTINE grtxy0(absxy,x,y,xt,yt)
!
! GRPCKG (internal routine): Convert scaled position to absolute
! position.
!
! Arguments:
!
! ABSXY (input, logical): if FALSE, convert world coordinates to
!       absolute device coordinates; if TRUE, return the input
!       coordinates unchanged.
! X, Y (input, real): input coordinates (absolute or world, depending
!       on setting of ABSXY).
! XT, YT (output, real): output absolute device coordinates.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   LOGICAL, INTENT(IN)                      :: absxy
   REAL(KIND=pg), INTENT(IN)                :: x
   REAL(KIND=pg), INTENT(IN)                :: y
   REAL(KIND=pg), INTENT(OUT)               :: xt
   REAL(KIND=pg), INTENT(OUT)               :: yt
!
   IF(absxy)THEN
      xt=x
      yt=y
   ELSE
      xt=x*grxscl(grcide)+grxorg(grcide)
      yt=y*gryscl(grcide)+gryorg(grcide)
   END IF
!
   RETURN
!
END SUBROUTINE grtxy0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRUSER -- get user name (MS-DOS)
!+
SUBROUTINE gruser(cuser,luser)
!
! Return the name of the user running the program.
!
! Arguments:
!  CUSER  : receives user name, truncated or extended with
!           blanks as necessary.
!  LUSER  : receives the number of characters in VALUE, excluding
!           trailing blanks.
!--
! 1989-Mar-19 - [AFT]
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN OUT)        :: cuser
   INTEGER, INTENT(IN OUT)                  :: luser
!
   CALL grgenv('USER',cuser,luser)
   RETURN
!
END SUBROUTINE gruser
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRVCT0 -- draw line segments or dots
!+
SUBROUTINE grvct0(mode,absxy,points,x,y)
!
! GRPCKG (internal routine): Draw a line or a set of dots. This
! is the same as GRVECT, but without device selection. It can be used to
! draw a single line-segment, a continuous series of line segments, or
! one or more single dots (pixels).
!
! Arguments:
!
! MODE (input, integer): if MODE=1, a series of line segments is drawn,
!       starting at the current position, moving to X(1),Y(1), ... and
!       ending at X(POINTS),Y(POINTS).
!       If MODE=2, the first vector is blanked, so the line starts at
!       X(1),Y(1).
!       If MODE=3, a single dot is placed at each coordinate pair, with
!       no connecting lines.
! ABSXY (input, logical): if TRUE, the coordinates are absolute device
!       coordinates; if FALSE, they are world coordinates and the
!       scaling transformation is applied.
! POINTS (input, integer): the number of coordinate pairs.
! X, Y (input, real arrays, dimensioned POINTS or greater): the
!       X and Y coordinates of the points.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE grpckg1
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: mode
   LOGICAL, INTENT(IN)                      :: absxy
   INTEGER, INTENT(IN)                      :: points
   REAL(KIND=pg), INTENT(IN)                :: x(points)
   REAL(KIND=pg), INTENT(IN)                :: y(points)
!
   INTEGER :: i
!
   REAL(KIND=pg) :: xcur,ycur
!
   IF(mode == 1)THEN
      CALL grtxy0(absxy,x(1),y(1),xcur,ycur)
      CALL grlin0(xcur,ycur)
   ELSE IF(mode == 2)THEN
      CALL grtxy0(absxy,x(1),y(1),grxpre(grcide),grypre(grcide))
   END IF
   IF(mode == 1.OR.mode == 2)THEN
      DO  i=2,points
         CALL grtxy0(absxy,x(i),y(i),xcur,ycur)
         CALL grlin0(xcur,ycur)
      END DO
   ELSE IF(mode == 3)THEN
      DO  i=1,points
         CALL grtxy0(absxy,x(i),y(i),xcur,ycur)
         CALL grdot0(xcur,ycur)
      END DO
   END IF
!
   RETURN
!
END SUBROUTINE grvct0
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRVECT -- draw line segments or dots
!+
SUBROUTINE grvect(ident,mode,absxy,points,x,y)
!
! GRPCKG: Draw a line or a set of dots. This routine can be used to
! draw a single line-segment, a continuous series of line segments, or
! one or more single dots (pixels).
!
! Arguments:
!
! IDENT (input, integer): the plot identifier, as returned by GROPEN.
! MODE (input, integer): if MODE=1, a series of line segments is drawn,
!       starting at the current position, moving to X(1),Y(1), ... and
!       ending at X(POINTS),Y(POINTS).
!       If MODE=2, the first vector is blanked, so the line starts at
!       X(1),Y(1).
!       If MODE=3, a single dot is placed at each coordinate pair, with
!       no connecting lines.
! ABSXY (input, logical): if TRUE, the coordinates are absolute device
!       coordinates; if FALSE, they are world coordinates and the
!       scaling transformation is applied.
! POINTS (input, integer): the number of coordinate pairs.
! X, Y (input, real arrays, dimensioned POINTS or greater): the
!       X and Y coordinates of the points.
!--
! (1-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: ident
   INTEGER, INTENT(IN OUT)                  :: mode
   LOGICAL, INTENT(IN OUT)                  :: absxy
   INTEGER, INTENT(IN OUT)                  :: points
   REAL(KIND=pg), INTENT(IN OUT)            :: x(points)
   REAL(KIND=pg), INTENT(IN OUT)            :: y(points)
!
   CALL grslct(ident)
   IF(mode <= 0.OR.mode > 3)THEN
      CALL grwarn('GRVECT - invalid MODE parameter.')
   ELSE IF(points > 0)THEN
      CALL grvct0(mode,absxy,points,x,y)
   END IF
!
   RETURN
!
END SUBROUTINE grvect
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRWARN -- issue warning message to user
!+
SUBROUTINE grwarn(text)
!
! Report a warning message on standard output, with prefix "%PGPLOT, ".
!
! Argument:
!  TEXT (input): text of message to be printed (the string
!      may not be blank).
!--
!  8-Nov-1994 [TJP]
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)        :: text

   INTEGER :: grtrim
!
   IF(text /= ' ')THEN
      WRITE(*,'(1X,2A)') '%PGPLOT, ',text(1:grtrim(text))
   END IF
!
END SUBROUTINE grwarn
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRXHLS -- convert RGB color to HLS color
!+
SUBROUTINE grxhls(r,g,b,h,l,s)
!
! GRPCKG: Convert a color specified in the RGB color model to one in
! the HLS model.  This is a support routine: no graphics I/O occurs.
! The inverse transformation is accomplished with routine GRXRGB.
! Reference: SIGGRAPH Status Report of the Graphic Standards Planning
! Committee, Computer Graphics, Vol.13, No.3, Association for
! Computing Machinery, New York, NY, 1979.
!
! Arguments:
!
! R,G,B (real, input): red, green, blue color coordinates, each in the
!       range 0.0 to 1.0. Input outside this range causes HLS = (0,1,0)
!       [white] to be returned.
! H,L,S (real, output): hue (0 to 360), lightness (0 to 1.0), and
!       saturation (0 to 1.0).
!--
!  2-Jul-1984 - new routine [TJP].
! 29-Sep-1994 - force H to be in rnage 0-360 [Remko Scharroo; TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)                     :: r
   REAL(KIND=pg), INTENT(IN)                         :: g
   REAL(KIND=pg), INTENT(IN OUT)                     :: b
   REAL(KIND=pg), INTENT(OUT)                        :: h
   REAL(KIND=pg), INTENT(OUT)                        :: l
   REAL(KIND=pg), INTENT(OUT)                        :: s
   REAL(KIND=pg) :: ma,mi,rr,gg,bb,d
!
   h=0.0_pg
   l=1.0_pg
   s=0.0_pg
   ma=MAX(r,g,b)
   mi=MIN(r,g,b)
   IF(ma > 1.0_pg.OR.mi < 0.0_pg)RETURN
   rr=(ma-r)
   gg=(ma-g)
   bb=(ma-b)
!
! Lightness
!
   l=0.5_pg*(ma+mi)
!
! Achromatic case (R=G=B)
!
   IF(ABS(ma - mi) <= EPSILON(ma))THEN
      s=0.0_pg
      h=0.0_pg
!
! Chromatic case
!
   ELSE
!
!         -- Saturation
!
      d=ma-mi
      IF(l <= 0.5_pg)THEN
         s=d/(ma+mi)
      ELSE
         s=d/(2.0_pg-ma-mi)
      END IF
!
!         -- Hue
!
      IF(ABS(r - ma) <= EPSILON(r))THEN
!
!             -- yellow to magenta
!
         h=(2.0_pg*d+bb-gg)
      ELSE IF(ABS(g - ma) <= EPSILON(g))THEN
         h=(4.0_pg*d+rr-bb)
      ELSE
!
!             ! (B.EQ.MA)
!
         h=(6.0_pg*d+gg-rr)
      END IF
      h=MOD(h*60.0_pg/d,360.0_pg)
      IF(h < 0.0_pg)h=h+360.0_pg
   END IF
!
   RETURN
!
END SUBROUTINE grxhls
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*GRXRGB -- convert HLS color to RGB color
!+
SUBROUTINE grxrgb(h,l,s,r,g,b)
!
! GRPCKG: Convert a color specified in the HLS color model to one in
! the RGB model.  This is a support routine: no graphics I/O occurs.
! The inverse transformation is accomplished with routine GRXHLS.
! Reference: SIGGRAPH Status Report of the Graphic Standards Planning
! Committee, Computer Graphics, Vol.13, No.3, Association for
! Computing Machinery, New York, NY, 1979.
!
! Arguments:
!
! H,L,S (real, input): hue (0 to 360), lightness (0 to 1.0), and
!       saturation (0 to 1.0).
! R,G,B (real, output): red, green, blue color coordinates, each in the
!       range 0.0 to 1.0.
!--
!  2-Jul-1984 - new routine [TJP].
! 29-Sep-1994 - take H module 360 [TJP].
! 26-Nov-1996 - force results to be in range (avoid rounding error
!               problems on some machines) [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: h
   REAL(KIND=pg), INTENT(IN)                         :: l
   REAL(KIND=pg), INTENT(IN)                         :: s
   REAL(KIND=pg), INTENT(OUT)                        :: r
   REAL(KIND=pg), INTENT(OUT)                        :: g
   REAL(KIND=pg), INTENT(OUT)                        :: b
!
   REAL(KIND=pg) :: ma,mi,hm
!
   hm=MOD(h,360.0_pg)
   IF(hm < 0.0_pg)hm=hm+360.0_pg
   IF(l <= 0.5_pg)THEN
      ma=l*(1.0_pg+s)
   ELSE
      ma=l+s-l*s
   END IF
   mi=2.0_pg*l-ma
!
! R component
!
   IF(hm < 60.0_pg)THEN
      r=mi+(ma-mi)*hm/60.0_pg
   ELSE IF(hm < 180.0_pg)THEN
      r=ma
   ELSE IF(hm < 240.0_pg)THEN
      r=mi+(ma-mi)*(240.0_pg-hm)/60.0_pg
   ELSE
      r=mi
   END IF
!
! G component
!
   IF(hm < 120.0_pg)THEN
      g=mi
   ELSE IF(hm < 180.0_pg)THEN
      g=mi+(ma-mi)*(hm-120.0_pg)/60.0_pg
   ELSE IF(hm < 300.0_pg)THEN
      g=ma
   ELSE
      g=mi+(ma-mi)*(360.0_pg-hm)/60.0_pg
   END IF
!
! B component
!
   IF(hm < 60.0_pg.OR.hm >= 300.0_pg)THEN
      b=ma
   ELSE IF(hm < 120.0_pg)THEN
      b=mi+(ma-mi)*(120.0_pg-hm)/60.0_pg
   ELSE IF(hm < 240.0_pg)THEN
      b=mi
   ELSE
      b=mi+(ma-mi)*(hm-240.0_pg)/60.0_pg
   END IF
!
   r=MIN(1.0_pg,MAX(0.0_pg,r))
   g=MIN(1.0_pg,MAX(0.0_pg,g))
   b=MIN(1.0_pg,MAX(0.0_pg,b))
!
   RETURN
!
END SUBROUTINE grxrgb
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*NUDRIV -- PGPLOT Null device driver
!+
SUBROUTINE nudriv(ifunc,rbuf,nbuf,chr,lchr)
!
! PGPLOT driver for Null device (no graphical output)
!
! Version 1.0  - 1987 May 26 - T. J. Pearson.
! Version 1.1  - 1988 Mar 23 - add rectangle fill.
! Version 1.2  - 1992 Sep  3 - add line-of-pixels.
! Version 1.3  - 1992 Sep 21 - add markers.
! Version 1.4  - 1993 Apr 22 - add optional debugging.
! Version 1.5  - 1994 Aug 31 - use image primitives.
! Version 2.0  - 1996 Jan 22 - allow multiple active devices;
!                              add QCR primitive.
! Version 2.1  - 1997 Jun 13 - correctly initialize STATE.
!
! Supported device: The ``null'' device can be used to suppress
! all graphic output from a program.  If environment variable
! PGPLOT_DEBUG is defined, some debugging information is
! reported on standard output.
!
! Device type code: /NULL.
!
! Default device name: None (the device name, if specified, is
! ignored).
!
! Default view surface dimensions: Undefined (The device pretends to
! be a hardcopy device with 1000 pixels/inch and a view surface 8in
! high by 10.5in wide.)
!
! Resolution: Undefined.
!
! Color capability: Color indices 0--255 are accepted.
!
! Input capability: None.
!
! File format: None.
!
! Obtaining hardcopy: Not possible.
!-----------------------------------------------------------------------
! Notes:
!  Up to MAXDEV "devices" may be open at once. ACTIVE is the number
!  of the currently selected device, or 0 if no devices are open.
!  STATE(i) is 0 if device i is not open, 1 if it is open but with
!  no current picture, or 2 if it is open with a current picture.
!
!  When debugging is enabled, open/close device and begin/end picture
!  calls are reported on stdout, and a cumulative count of all
!  driver calls is kept.
!-----------------------------------------------------------------------
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ifunc
   REAL(KIND=pg), INTENT(OUT)               :: rbuf(*)
   INTEGER, INTENT(OUT)                     :: nbuf
   CHARACTER (LEN=*), INTENT(OUT)           :: chr
   INTEGER, INTENT(OUT)                     :: lchr
!
!
   CHARACTER (LEN=*), PARAMETER :: device='NULL  (Null device, no output)'
!
   INTEGER, PARAMETER :: maxdev=8
   INTEGER, PARAMETER :: maxd1=maxdev+1
!
   INTEGER, PARAMETER :: nopcod=29
   CHARACTER (LEN=10) :: msg
   CHARACTER (LEN=32) :: text
   CHARACTER (LEN=8) :: lab(nopcod)
   INTEGER :: count(nopcod),i,state(0:maxdev),l,npic(maxdev)
   INTEGER :: active
   LOGICAL :: debug
   INTEGER :: ctable(3,0:255),cdeflt(3,0:15)
   SAVE  count,state,npic,debug,ctable,cdeflt,active
!
   DATA active/-1/
   DATA state/maxd1*0/
   DATA count/nopcod*0/
   DATA debug/.false./
   DATA lab/'qdev    ','qmaxsize','qscale  ','qcapab  ','qdefnam ', &
      'qdefsize','qmisc   ','select  ','open    ','close   ','beginpic',  &
      'line    ','dot     ','endpic  ','set CI  ','flush   ','cursor  ',  &
      'eralpha ','set LS  ','polygon ','set CR  ','set LW  ','escape  ',  &
      'rectangl','set patt','pix/imag','scaling ','marker  ','query CR'/
   DATA cdeflt/000,000,000,255,255,255,255,000,000,000,255,000,  &
      000,000,255,000,255,255,255,000,255,255,255,000,255,128,000,  &
      128,255,000,000,255,128,000,128,255,128,000,255,255,000,128,  &
      085,085,085,170,170,170/
!-----------------------------------------------------------------------
!
   IF(active == -1)THEN
      CALL grgenv ('DEBUG',text,l)
      debug=l > 0
      active=0
   END IF
!
   IF(ifunc < 1.OR.ifunc > nopcod)GO TO 10
   count(ifunc)=count(ifunc)+1
!
!   replace computed GO TO
!
   IF(ifunc == 1)GO TO 20
   IF(ifunc == 2)GO TO 30
   IF(ifunc == 3)GO TO 40
   IF(ifunc == 4)GO TO 50
   IF(ifunc == 5)GO TO 60
   IF(ifunc == 6)GO TO 70
   IF(ifunc == 7)GO TO 80
   IF(ifunc == 8)GO TO 90
   IF(ifunc == 9)GO TO 100
   IF(ifunc == 10)GO TO 150
   IF(ifunc == 11)GO TO 170
   IF(ifunc == 12)GO TO 180
   IF(ifunc == 13)GO TO 190
   IF(ifunc == 14)GO TO 200
   IF(ifunc == 15)GO TO 210
   IF(ifunc == 16)GO TO 220
   IF(ifunc == 17)GO TO 10
   IF(ifunc == 18)GO TO 230
   IF(ifunc == 19)GO TO 240
   IF(ifunc == 20)GO TO 250
   IF(ifunc == 21)GO TO 260
   IF(ifunc == 22)GO TO 270
   IF(ifunc == 23)GO TO 280
   IF(ifunc == 24)GO TO 290
   IF(ifunc == 25)GO TO 300
   IF(ifunc == 26)GO TO 310
   IF(ifunc == 27)GO TO 320
   IF(ifunc == 28)GO TO 330
   IF(ifunc == 29)GO TO 340
!
10 WRITE(msg,'(I10)') ifunc
   CALL grwarn('Unimplemented function in NULL device driver: '//msg)
   nbuf=-1
   RETURN
!
!--- IFUNC = 1, Return device name.-------------------------------------
!
20 chr=device
   lchr=LEN(device)
   RETURN
!
!--- IFUNC = 2, Return physical min and max for plot device, and range
!               of color indices.---------------------------------------
!
30 rbuf(1)=0
   rbuf(2)=65535
   rbuf(3)=0
   rbuf(4)=65535
   rbuf(5)=0
   rbuf(6)=255
   nbuf=6
   RETURN
!
!--- IFUNC = 3, Return device resolution. ------------------------------
!
40 rbuf(1)=1000.0_pg
   rbuf(2)=1000.0_pg
   rbuf(3)=1
   nbuf=3
   RETURN
!
!--- IFUNC = 4, Return misc device info. -------------------------------
!    (This device is Hardcopy, No cursor, Dashed lines, Area fill, Thick
!    lines, Rectangle fill, Images, , , Markers, query color rep)
!
50 chr='HNDATRQNYM'
   lchr=10
   RETURN
!
!--- IFUNC = 5, Return default file name. ------------------------------
!
60 chr='NL:'
   lchr=3
   RETURN
!
!--- IFUNC = 6, Return default physical size of plot. ------------------
!
70 rbuf(1)=0
   rbuf(2)=10499
   rbuf(3)=0
   rbuf(4)=7999
   nbuf=4
   RETURN
!
!--- IFUNC = 7, Return misc defaults. ----------------------------------
!
80 rbuf(1)=1
   nbuf=1
   RETURN
!
!--- IFUNC = 8, Select plot. -------------------------------------------
!
90 CONTINUE
   i=INT(rbuf(2))-67890
   IF(i < 1.OR.i > maxdev)THEN
      CALL grwarn('internal error: NULL opcode 8')
   ELSE IF(state(i) > 0)THEN
      active=i
   ELSE
      CALL grnu00(ifunc,0)
   END IF
   RETURN
!
!--- IFUNC = 9, Open workstation. --------------------------------------
!
100 CONTINUE
!     -- Find an inactive device, and select it
   DO  i=1,maxdev
      IF(state(i) == 0)THEN
         active=i
         state(active)=1
         GO TO 120
      END IF
   END DO
   IF(debug)CALL grwarn('09 Open workstation')
   CALL grwarn('maximum number of devices of type NULL exceeded')
   rbuf(1)=0
   rbuf(2)=0
   nbuf=2
   RETURN
!
!     -- Initialize the new device
!
120 CONTINUE
   rbuf(1)=active+67890
   rbuf(2)=1
   nbuf=2
   npic(active)=0
!
!     -- Initialize color table
!
   DO  i=0,15
      ctable(1,i)=cdeflt(1,i)
      ctable(2,i)=cdeflt(2,i)
      ctable(3,i)=cdeflt(3,i)
   END DO
   DO  i=16,255
      ctable(1,i)=128
      ctable(2,i)=128
      ctable(3,i)=128
   END DO
   IF(debug)THEN
      CALL grfao('09 Open workstation: device #',l,text,active,0, 0,0)
      CALL grwarn(text(1:l))
   END IF
   RETURN
!
!--- IFUNC=10, Close workstation. --------------------------------------
!
150 CONTINUE
   IF(state(active) /= 1)CALL grnu00(ifunc,state(active))
   state(active)=0
   IF(debug)THEN
      CALL grfao('10 Close workstation: device #',l,text,active, 0,0,0)
      CALL grwarn(text(1:l))
      CALL grwarn('Device driver calls:')
      DO  i=1,nopcod
         IF(count(i) > 0)THEN
            WRITE(text,'(3X,I2,1X,A8,I10)')i,lab(i),count(i)
            CALL grwarn(text)
         END IF
      END DO
   END IF
   RETURN
!
!--- IFUNC=11, Begin picture. ------------------------------------------
!
170 CONTINUE
   IF(state(active) /= 1)CALL grnu00(ifunc,state(active))
   state(active)=2
   npic(active)=npic(active)+1
   IF(debug)THEN
      CALL grfao('11   Begin picture # on device #',l,text,  &
         npic(active),active,0,0)
      CALL grwarn(text(:l))
   END IF
   RETURN
!
!--- IFUNC=12, Draw line. ----------------------------------------------
!
180 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=13, Draw dot. -----------------------------------------------
!
190 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=14, End picture. --------------------------------------------
!
200 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
   state(active)=1
   IF(debug)THEN
      CALL grfao ('14   End picture   # on device #',l,text,  &
         npic(active),active,0,0)
      CALL grwarn(text(:l))
   END IF
   RETURN
!
!--- IFUNC=15, Select color index. -------------------------------------
!
210 CONTINUE
   IF(state(active) < 1)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=16, Flush buffer. -------------------------------------------
!
220 CONTINUE
   IF(state(active) < 1)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=17, Read cursor. --------------------------------------------
!    (Not implemented: should not be called.)
!
!
!--- IFUNC=18, Erase alpha screen. -------------------------------------
!
230 CONTINUE
   IF(state(active) < 1)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=19, Set line style. -----------------------------------------
!
240 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=20, Polygon fill. -------------------------------------------
!
250 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=21, Set color representation. -------------------------------
!
260 CONTINUE
   IF(state(active) < 1)CALL grnu00(ifunc,state(active))
   i=INT(rbuf(1))
   ctable(1,i)=nint(rbuf(2)*255)
   ctable(2,i)=nint(rbuf(3)*255)
   ctable(3,i)=nint(rbuf(4)*255)
   RETURN
!
!--- IFUNC=22, Set line width. -----------------------------------------
!
270 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=23, Escape. -------------------------------------------------
!
280 CONTINUE
   RETURN
!
!--- IFUNC=24, Rectangle fill. -----------------------------------------
!
290 CONTINUE
   IF(debug.AND.state(active) /= 2)CALL grnu00(ifunc, state(active))
   RETURN
!
!--- IFUNC=25, Not implemented -----------------------------------------
!
300 CONTINUE
   RETURN
!
!--- IFUNC=26, Line of pixels ------------------------------------------
!
310 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=27, Scaling info -- -----------------------------------------
!
320 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
   RETURN
!
!--- IFUNC=28, Draw marker ---------------------------------------------
!
330 CONTINUE
   IF(state(active) /= 2)CALL grnu00(ifunc,state(active))
!     WRITE (*,'(1X,A,I4,1X,3F10.1)') 'MARKER', NINT(RBUF(1)), RBUF(2),
!    1      RBUF(3), RBUF(4)
   RETURN
!
!--- IFUNC=29, Query color representation. -----------------------------
!
340 CONTINUE
   IF(state(active) < 1)CALL grnu00(ifunc,state(active))
   i=INT(rbuf(1))
   rbuf(2)=ctable(1,i)/255.0_pg
   rbuf(3)=ctable(2,i)/255.0_pg
   rbuf(4)=ctable(3,i)/255.0_pg
   nbuf=4
   RETURN
!
END SUBROUTINE nudriv
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGADVANCE -- non-standard alias for PGPAGE
!+
SUBROUTINE pgadvance
!
! See description of PGPAGE.
!--
   CALL pgpage
!
END SUBROUTINE pgadvance
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGARRO -- draw an arrow
!%void cpgarro(float x1, float y1, float x2, float y2);
!+
SUBROUTINE pgarro(x1,y1,x2,y2)
!
! Draw an arrow from the point with world-coordinates (X1,Y1) to
! (X2,Y2). The size of the arrowhead at (X2,Y2) is determined by
! the current character size set by routine PGSCH. The default size
! is 1/40th of the smaller of the width or height of the view surface.
! The appearance of the arrowhead (shape and solid or open) is
! controlled by routine PGSAH.
!
! Arguments:
!  X1, Y1 (input)  : world coordinates of the tail of the arrow.
!  X2, Y2 (input)  : world coordinates of the head of the arrow.
!--
!  7-Feb-92 Keith Horne @ STScI / TJP.
! 13-Oct-92 - use arrowhead attributes; scale (TJP).
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: x1
   REAL(KIND=pg), INTENT(IN)                         :: y1
   REAL(KIND=pg), INTENT(IN)                         :: x2
   REAL(KIND=pg), INTENT(IN)                         :: y2
!
   INTEGER :: ahfs,fs
   REAL(KIND=pg) :: dx,dy,xv1,xv2,yv1,yv2,xl,xr,yb,yt,dindx,dindy
   REAL(KIND=pg) :: xinch,yinch,rinch,ca,sa,so,co,yp,xp,ym,xm,dhx,dhy
   REAL(KIND=pg) :: px(4),py(4)
   REAL(KIND=pg) :: ahangl,ahvent,semang,ch,dh,xs1,xs2,ys1,ys2
!
   CALL pgbbuf
   CALL pgqah(ahfs,ahangl,ahvent)
   CALL pgqfs(fs)
   CALL pgsfs(ahfs)
   dx=x2-x1
   dy=y2-y1
   CALL pgqch(ch)
   CALL pgqvsz(1,xs1,xs2,ys1,ys2)
!
!     -- length of arrowhead: 1 40th of the smaller of the height or
!        width of the view surface, scaled by character height.
!
   dh=ch*MIN(ABS(xs2-xs1),ABS(ys2-ys1))/40.0_pg
   CALL pgmove(x2,y2)
!
!     -- Is there to be an arrowhead ?
!
   IF(dh > 0.0_pg)THEN
      IF((ABS(dx) > EPSILON(dx)).OR.(ABS(dy) > EPSILON(dy)))THEN
!
!             -- Get x and y scales
!
         CALL pgqvp(1,xv1,xv2,yv1,yv2)
         CALL pgqwin(xl,xr,yb,yt)
         IF(ABS(xr - xl) > EPSILON(xr).AND.ABS(yt - yb) > EPSILON(yt))THEN
            dindx=(xv2-xv1)/(xr-xl)
            dindy=(yv2-yv1)/(yt-yb)
            dhx=dh/dindx
            dhy=dh/dindy
!
!                 -- Unit vector in direction of the arrow
!
            xinch=dx*dindx
            yinch=dy*dindy
            rinch=SQRT(xinch*xinch+yinch*yinch)
            ca=xinch/rinch
            sa=yinch/rinch
!
!                 -- Semiangle in radians
!
            semang=ahangl/2.0_pg/57.29577951308232_pg
            so=SIN(semang)
            co=-COS(semang)
!
!                 -- Vector back along one edge of the arrow
!
            xp=dhx*(ca*co-sa*so)
            yp=dhy*(sa*co+ca*so)
!
!                 -- Vector back along other edge of the arrow
!
            xm=dhx*(ca*co+sa*so)
            ym=dhy*(sa*co-ca*so)
!
!                 -- Draw the arrowhead
!
            px(1)=x2
            py(1)=y2
            px(2)=x2+xp
            py(2)=y2+yp
            px(3)=x2+0.5_pg*(xp+xm)*(1.0_pg-ahvent)
            py(3)=y2+0.5_pg*(yp+ym)*(1.0_pg-ahvent)
            px(4)=x2+xm
            py(4)=y2+ym
            CALL pgpoly(4,px,py)
            CALL pgmove(px(3),py(3))
         END IF
      END IF
   END IF
   CALL pgdraw(x1,y1)
   CALL pgmove(x2,y2)
   CALL pgsfs(fs)
   CALL pgebuf
   RETURN
!
END SUBROUTINE pgarro
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGASK -- control new page prompting
!%void cpgask(Logical flag);
!+
SUBROUTINE pgask(flag)
!
! Change the ``prompt state'' of PGPLOT. If the prompt state is
! ON, PGPAGE will type ``Type RETURN for next page:'' and will wait
! for the user to type a carriage-return before starting a new page.
! The initial prompt state (after the device has been opened) is ON
! for interactive devices. Prompt state is always OFF for
! non-interactive devices.
!
! Arguments:
!  FLAG   (input)  : if .TRUE., and if the device is an interactive
!                    device, the prompt state will be set to ON. If
!                    .FALSE., the prompt state will be set to OFF.
!--
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   LOGICAL, INTENT(IN)                  :: flag
!
   LOGICAL :: pgnoto
   CHARACTER (LEN=1) :: TYPE
!
   IF(pgnoto('PGASK'))RETURN
!
   IF(flag)THEN
      CALL grqtyp(TYPE,pgprmp(pgid))
   ELSE
      pgprmp(pgid)=.false.
   END IF
!
END SUBROUTINE pgask
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGAXIS -- draw an axis
!%void cpgaxis(const char *opt, float x1, float y1, float x2, float y2, \
!%             float v1, float v2, float step, int nsub, float dmajl, \
!%             float dmajr, float fmin, float disp, float orient);
!+
SUBROUTINE pgaxis(opt,x1,y1,x2,y2,v1,v2,step,nsub,dmajl,  &
   dmajr,fmin,disp,orient)
!
! Draw a labelled graph axis from world-coordinate position (X1,Y1) to
! (X2,Y2).
!
! Normally, this routine draws a standard LINEAR axis with equal
! subdivisions.   The quantity described by the axis runs from V1 to V2;
! this may be, but need not be, the same as X or Y.
!
! If the 'L' option is specified, the routine draws a LOGARITHMIC axis.
! In this case, the quantity described by the axis runs from 10**V1 to
! 10**V2. A logarithmic axis always has major, labeled, tick marks
! spaced by one or more decades. If the major tick marks are spaced
! by one decade (as specified by the STEP argument), then minor
! tick marks are placed at 2, 3, .., 9 times each power of 10;
! otherwise minor tick marks are spaced by one decade. If the axis
! spans less than two decades, numeric labels are placed at 1, 2, and
! 5 times each power of ten.
!
! If the axis spans less than one decade, or if it spans many decades,
! it is preferable to use a linear axis labeled with the logarithm of
! the quantity of interest.
!
! Arguments:
!  OPT    (input)  : a string containing single-letter codes for
!                    various options. The options currently
!                    recognized are:
!                    L : draw a logarithmic axis
!                    N : write numeric labels
!                    1 : force decimal labelling, instead of automatic
!                        choice (see PGNUMB).
!                    2 : force exponential labelling, instead of
!                        automatic.
!  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
!  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
!  V1     (input)  : axis value at first endpoint.
!  V2     (input)  : axis value at second endpoint.
!  STEP   (input)  : major tick marks are drawn at axis value 0.0 plus
!                    or minus integer multiples of STEP. If STEP=0.0,
!                    a value is chosen automatically.
!  NSUB   (input)  : minor tick marks are drawn to divide the major
!                    divisions into NSUB equal subdivisions (ignored if
!                    STEP=0.0). If NSUB <= 1, no minor tick marks are
!                    drawn. NSUB is ignored for a logarithmic axis.
!  DMAJL  (input)  : length of major tick marks drawn to left of axis
!                    (as seen looking from first endpoint to second), in
!                    units of the character height.
!  DMAJR  (input)  : length of major tick marks drawn to right of axis,
!                    in units of the character height.
!  FMIN   (input)  : length of minor tick marks, as fraction of major.
!  DISP   (input)  : displacement of baseline of tick labels to
!                    right of axis, in units of the character height.
!  ORIENT (input)  : orientation of label text, in degrees; angle between
!                    baseline of text and direction of axis (0-360°).
!--
! 25-Mar-1997 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: opt
   REAL(KIND=pg), INTENT(IN OUT)            :: x1
   REAL(KIND=pg), INTENT(IN OUT)            :: y1
   REAL(KIND=pg), INTENT(IN OUT)            :: x2
   REAL(KIND=pg), INTENT(IN OUT)            :: y2
   REAL(KIND=pg), INTENT(IN OUT)            :: v1
   REAL(KIND=pg), INTENT(IN OUT)            :: v2
   REAL(KIND=pg), INTENT(IN OUT)            :: step
   INTEGER, INTENT(IN OUT)                  :: nsub
   REAL(KIND=pg), INTENT(IN OUT)            :: dmajl
   REAL(KIND=pg), INTENT(IN OUT)            :: dmajr
   REAL(KIND=pg), INTENT(IN OUT)            :: fmin
   REAL(KIND=pg), INTENT(IN OUT)            :: disp
   REAL(KIND=pg), INTENT(IN OUT)            :: orient
!
   REAL(KIND=pg) :: v,vmin,vmax,dvmaj,dvmin
   REAL(KIND=pg) :: pgrnd
   INTEGER :: i,k,k1,k2,nsubt,np,llab,clip,FORM
   LOGICAL :: optn,pgnoto
   CHARACTER (LEN=1) :: tempch
   CHARACTER (LEN=32) :: ch,label
!
!   JAO:  make nv a 64 bit integer
!
   INTEGER(KIND=8) :: nv
!
! Check arguments.
!
   IF(pgnoto('PGAXIS'))RETURN
   IF((ABS(x1 - x2) <= EPSILON(x1)).AND.(ABS(y1 - y2) <= EPSILON(y1)))RETURN
   IF(ABS(v1 - v2) <= EPSILON(v1))RETURN
!
! Decode options.
!
   FORM=0
   optn=.false.
   DO  i=1,LEN(opt)
!
!  JAO:  use a temporary string tempch in the argument list of grtoup
!
      ch=opt(i:i)
      tempch=opt(i:i)
      CALL grtoup(ch,tempch)
      IF(ch == 'N')THEN
!
!           -- numeric labels requested
!
         optn=.true.
      ELSE IF(ch == 'L')THEN
!
!           -- logarithmic axis requested
!
         CALL pgaxlg(opt,x1,y1,x2,y2,v1,v2,step,dmajl,dmajr,fmin, disp,orient)
         RETURN
      ELSE IF(ch == '1')THEN
!
!           -- decimal labels requested
!
         FORM=1
      ELSE IF(ch == '2')THEN
!
!           -- exponential labels requested
!
         FORM=2
      END IF
   END DO
!
! Choose major interval if defaulted. Requested interval = STEP,
! with NSUB subdivisions. We will use interval = DVMAJ with NSUBT
! subdivisions of size DVMIN. Note that DVMAJ is always positive.
!
   IF(ABS(step) <= EPSILON(step))THEN
      dvmaj=pgrnd(0.20_pg*ABS(v1-v2),nsubt)
   ELSE
      dvmaj=ABS(step)
      nsubt=MAX(nsub,1)
   END IF
   dvmin=dvmaj/nsubt
!
! For labelling, we need to express DVMIN as an integer times a
! power of 10, NV*(10**NP).
!
   np=INT(LOG10(ABS(dvmin)))-4
   nv=nint(dvmin/10.0_pg**np)
   dvmin=REAL(nv,KIND=pg)*(10.0_pg**np)
!
   CALL pgbbuf
   CALL pgqclp(clip)
   CALL pgsclp(0)
!
! Draw the axis.
!
   CALL pgmove(x1,y1)
   CALL pgdraw(x2,y2)
!
! Draw the tick marks. Minor ticks are drawn at V = K*DVMIN,
! major (labelled) ticks where K is a multiple of NSUBT.
!
   vmin=MIN(v1,v2)
   vmax=MAX(v1,v2)
   k1=INT(vmin/dvmin)
   IF(dvmin*k1 < vmin)k1=k1+1
   k2=INT(vmax/dvmin)
   IF(dvmin*k2 > vmax)k2=k2-1
   DO  k=k1,k2
      v=(k*dvmin-v1)/(v2-v1)
      IF(MOD(k,nsubt) == 0)THEN
!
!             -- major tick mark
!
         IF(optn)THEN
            CALL pgnumb(k*nv,np,FORM,label,llab)
         ELSE
            label=' '
            llab=1
         END IF
         CALL pgtick(x1,y1,x2,y2,v,dmajl,dmajr,disp,orient,label(:llab))
      ELSE
!
!             -- minor tick mark
!
         CALL pgtick(x1,y1,x2,y2,v,dmajl*fmin,dmajr*fmin,0.0_pg,orient,' ')
      END IF
   END DO
!
   CALL pgsclp(clip)
   CALL pgebuf
!
END SUBROUTINE pgaxis
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
! PGAXLG -- draw a logarithmic axis [internal routine]
!
SUBROUTINE pgaxlg(opt,x1,y1,x2,y2,v1,v2,step,dmajl,dmajr,fmin,disp,orient)
!
! Draw a labelled graph axis from world-coordinate position (X1,Y1)
!  to (X2,Y2). The quantity described by the axis runs from 10**V1 to
! 10**V2. A logarithmic axis always has major, labeled, tick marks
! spaced by one or more decades. If the major tick marks are spaced
! by one decade (as specified by the STEP argument), then minor
! tick marks are placed at 2, 3, .., 9 times each power of 10;
! otherwise minor tick marks are spaced by one decade. If the axis
! spans less than two decades, numeric labels are placed at 1, 2, and
! 5 times each power of ten.
!
! It is not advisable to use this routine if the axis spans less than
! one decade, or if it spans many decades. In these cases it is
! preferable to use a linear axis labeled with the logarithm of the
! quantity of interest.
!
! Arguments:
!  OPT    (input)  : a string containing single-letter codes for
!                    various options. The options currently
!                    recognized are:
!                    N : write numeric labels
!                    1 : force decimal labelling, instead of automatic
!                        choice (see PGNUMB).
!                    2 : force exponential labelling, instead of
!                        automatic.
!  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
!  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
!  V1     (input)  : logarithm of axis value at first endpoint.
!  V2     (input)  : logarithm of axis value at second endpoint.
!  STEP   (input)  : the number of decades between major (labeled) tick
!                    marks.
!  DMAJL  (input)  : length of major tick marks drawn to left of axis
!                    (as seen looking from first endpoint to second), in
!                    units of the character height.
!  DMAJR  (input)  : length of major tick marks drawn to right of axis,
!                    in units of the character height.
!  FMIN   (input)  : length of minor tick marks, as fraction of major.
!  DISP   (input)  : displacement of baseline of tick labels to
!                    right of axis, in units of the character height.
!  ORIENT (input)  : orientation of text label relative to axis (see
!                    PGTICK).
!--
! 25-Mar-1997 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)        :: opt
   REAL(KIND=pg), INTENT(IN)            :: x1
   REAL(KIND=pg), INTENT(IN)            :: y1
   REAL(KIND=pg), INTENT(IN)            :: x2
   REAL(KIND=pg), INTENT(IN)            :: y2
   REAL(KIND=pg), INTENT(IN)            :: v1
   REAL(KIND=pg), INTENT(IN)            :: v2
   REAL(KIND=pg), INTENT(IN)            :: step
   REAL(KIND=pg), INTENT(IN)            :: dmajl
   REAL(KIND=pg), INTENT(IN)            :: dmajr
   REAL(KIND=pg), INTENT(IN)            :: fmin
   REAL(KIND=pg), INTENT(IN)            :: disp
   REAL(KIND=pg), INTENT(IN)            :: orient
!
   REAL(KIND=pg) :: v,vmin,vmax,dvmaj,dvmin,pgrnd
   INTEGER :: i,k,k1,k2,llab,nsubt,clip,FORM
   LOGICAL :: xlab,optn
   CHARACTER (LEN=32) :: label
   REAL(KIND=pg) :: tab(9)
!
! Table of logarithms 1..9
!
   DATA tab/0.0_pg,0.301029995663981_pg,0.477121254719662_pg,  &
      0.602059991327962_pg,0.698970004336019_pg,0.77815125038644_pg,  &
      0.845098040014257_pg,0.903089986991944_pg,0.954242509439325_pg/
!
! Check arguments.
!
   IF((ABS(x1 - x2) <= EPSILON(x1)).AND.(ABS(y1 - y2) <= EPSILON(y1)))RETURN
   IF(ABS(v1 - v2) <= EPSILON(v1))RETURN
!
! Decode options.
!
   optn=INDEX(opt,'N') /= 0.OR.INDEX(opt,'n') /= 0
   FORM=0
   IF(INDEX(opt,'1') /= 0)FORM=1
   IF(INDEX(opt,'2') /= 0)FORM=2
!
! Choose major interval (DVMAJ in the logarithm, with minimum value
! 1.0 = one decade). The minor interval is always 1.0.
!
   IF(step > 0.5_pg)THEN
      dvmaj=nint(step)
   ELSE
      dvmaj=pgrnd(0.20_pg*ABS(v1-v2),nsubt)
      IF(dvmaj < 1.0_pg)dvmaj=1.0
   END IF
   dvmin=1.0_pg
   nsubt=INT(dvmaj/dvmin)
!
   CALL pgbbuf
   CALL pgqclp(clip)
   CALL pgsclp(0)
!
! Draw the axis.
!
   CALL pgmove(x1,y1)
   CALL pgdraw(x2,y2)
!
! Draw the tick marks. Major ticks are drawn at V = K*DVMAJ.
!
   vmin=MIN(v1,v2)
   vmax=MAX(v1,v2)
   k1=INT(vmin/dvmin)
   IF(dvmin*k1 < vmin)k1=k1+1
   k2=INT(vmax/dvmin)
   IF(dvmin*k2 > vmax)k2=k2-1
   xlab=(k2-k1) <= 2
   DO  k=k1,k2
      v=(k*dvmin-v1)/(v2-v1)
      IF(MOD(k,nsubt) == 0)THEN
!
!             -- major tick mark
!
         IF(optn)THEN
            CALL pgnumb(INT(1,KIND=8),nint(k*dvmin),FORM,label,llab)
         ELSE
            label=' '
            llab=1
         END IF
         CALL pgtick(x1,y1,x2,y2,v,dmajl,dmajr,disp,orient,label(:llab))
      ELSE
!
!             -- minor tick mark
!
         CALL pgtick(x1,y1,x2,y2,v,dmajl*fmin,dmajr*fmin,0.0_pg,orient,' ')
      END IF
   END DO
!
! Draw intermediate tick marks if required.
! Label them if axis spans less than 2 decades.
!
   IF(nsubt == 1)THEN
      DO  k=k1-1,k2+1
         DO  i=2,9
            v=(k*dvmin+tab(i)-v1)/(v2-v1)
            IF(v >= 0.0_pg.AND.v <= 1.0_pg)THEN
               IF(optn.AND.(xlab.AND.(i == 2.OR.i == 5)))THEN
!
!                    -- labeled minor tick mark
!
                  CALL pgnumb(INT(i,KIND=8),nint(k*dvmin),FORM,label,llab)
               ELSE
!
!                    -- unlabeled minor tick mark
!
                  label=' '
                  llab=1
               END IF
               CALL pgtick(x1,y1,x2,y2,v,dmajl*fmin,dmajr*fmin,disp,  &
                  orient,label(:llab))
            END IF
         END DO
      END DO
   END IF
!
   CALL pgsclp(clip)
   CALL pgebuf
!
END SUBROUTINE pgaxlg
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGBAND -- read cursor position, with anchor
!%int cpgband(int mode, int posn, float xref, float yref, float *x,\
!%            float *y, char *ch_scalar);
!+
FUNCTION pgband(mode,posn,xref,yref,x,y,ch)
!
! Read the cursor position and a character typed by the user.
! The position is returned in world coordinates.  PGBAND positions
! the cursor at the position specified (if POSN=1), allows the user to
! move the cursor using the mouse or arrow keys or whatever is available
! on the device. When he has positioned the cursor, the user types a
! single character on the keyboard; PGBAND then returns this
! character and the new cursor position (in world coordinates).
!
! Some interactive devices offer a selection of cursor types,
! implemented as thin lines that move with the cursor, but without
! erasing underlying graphics. Of these types, some extend between
! a stationary anchor-point at XREF,YREF, and the position of the
! cursor, while others simply follow the cursor without changing shape
! or size. The cursor type is specified with one of the following MODE
! values. Cursor types that are not supported by a given device, are
! treated as MODE=0.
!
! -- If MODE=0, the anchor point is ignored and the routine behaves
! like PGCURS.
! -- If MODE=1, a straight line is drawn joining the anchor point
! and the cursor position.
! -- If MODE=2, a hollow rectangle is extended as the cursor is moved,
! with one vertex at the anchor point and the opposite vertex at the
! current cursor position; the edges of the rectangle are horizontal
! and vertical.
! -- If MODE=3, two horizontal lines are extended across the width of
! the display, one drawn through the anchor point and the other
! through the moving cursor position. This could be used to select
! a Y-axis range when one end of the range is known.
! -- If MODE=4, two vertical lines are extended over the height of
! the display, one drawn through the anchor point and the other
! through the moving cursor position. This could be used to select an
! X-axis range when one end of the range is known.
! -- If MODE=5, a horizontal line is extended through the cursor
! position over the width of the display. This could be used to select
! an X-axis value such as the start of an X-axis range. The anchor point
! is ignored.
! -- If MODE=6, a vertical line is extended through the cursor
! position over the height of the display. This could be used to select
! a Y-axis value such as the start of a Y-axis range. The anchor point
! is ignored.
! -- If MODE=7, a cross-hair, centered on the cursor, is extended over
! the width and height of the display. The anchor point is ignored.
!
! Returns:
!  PGBAND          : 1 if the call was successful; 0 if the device
!                    has no cursor or some other error occurs.
! Arguments:
!  MODE   (input)  : display mode (0, 1, ..7: see above).
!  POSN   (input)  : if POSN=1, PGBAND attempts to place the cursor
!                    at point (X,Y); if POSN=0, it leaves the cursor
!                    at its current position. (On some devices this
!                    request may be ignored.)
!  XREF   (input)  : the world x-coordinate of the anchor point.
!  YREF   (input)  : the world y-coordinate of the anchor point.
!  X      (in/out) : the world x-coordinate of the cursor.
!  Y      (in/out) : the world y-coordinate of the cursor.
!  CH     (output) : the character typed by the user; if the device has
!                    no cursor or if some other error occurs, the value
!                    CHAR(0) [ASCII NUL character] is returned.
!
! Note: The cursor coordinates (X,Y) may be changed by PGBAND even if
! the device has no cursor or if the user does not move the cursor.
! Under these circumstances, the position returned in (X,Y) is that of
! the pixel nearest to the requested position.
!--
! 7-Sep-1994 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: mode
   INTEGER, INTENT(IN)                      :: posn
   REAL(KIND=pg), INTENT(IN)                :: xref
   REAL(KIND=pg), INTENT(IN)                :: yref
   REAL(KIND=pg), INTENT(IN OUT)            :: x
   REAL(KIND=pg), INTENT(IN OUT)            :: y
   CHARACTER (LEN=*), INTENT(OUT)           :: ch
!
   INTEGER :: grcurs,i,j,iref,jref,pgband
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGBAND'))THEN
      ch=CHAR(0)
      pgband=0
      RETURN
   END IF
   IF(mode < 0.OR.mode > 7)CALL grwarn('invalid MODE argument in PGBAND')
   IF(posn < 0.OR.posn > 1)CALL grwarn('invalid POSN argument in PGBAND')
!
   i=nint(pgxorg(pgid)+x*pgxscl(pgid))
   j=nint(pgyorg(pgid)+y*pgyscl(pgid))
   iref=nint(pgxorg(pgid)+xref*pgxscl(pgid))
   jref=nint(pgyorg(pgid)+yref*pgyscl(pgid))
   pgband=grcurs(pgid,i,j,iref,jref,mode,posn,ch)
   x=(i-pgxorg(pgid))/pgxscl(pgid)
   y=(j-pgyorg(pgid))/pgyscl(pgid)
   CALL grterm
!
END FUNCTION pgband
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGBBUF -- begin batch of output (buffer)
!%void cpgbbuf(void);
!+
SUBROUTINE pgbbuf
!
! Begin saving graphical output commands in an internal buffer; the
! commands are held until a matching PGEBUF call (or until the buffer
! is emptied by PGUPDT). This can greatly improve the efficiency of
! PGPLOT.  PGBBUF increments an internal counter, while PGEBUF
! decrements this counter and flushes the buffer to the output
! device when the counter drops to zero.  PGBBUF and PGEBUF calls
! should always be paired.
!
! Arguments: none
!--
! 21-Nov-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   LOGICAL :: pgnoto
!
   IF(.NOT.pgnoto('PGBBUF'))THEN
      pgblev(pgid)=pgblev(pgid)+1
   END IF
!
END SUBROUTINE pgbbuf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGBEG -- open a graphics device
!%int cpgbeg(int unit, const char *file, int nxsub, int nysub);
!+
FUNCTION pgbeg(unit,FILE,nxsub,nysub)
!
! Note: new programs should use PGOPEN rather than PGBEG. PGOPEN
! is retained for compatibility with existing programs. Unlike PGOPEN,
! PGBEG closes any graphics devices that are already open, so it
! cannot be used to open devices to be used in parallel.
!
! PGBEG opens a graphical device or file and prepares it for
! subsequent plotting. A device must be opened with PGBEG or PGOPEN
! before any other calls to PGPLOT subroutines for the device.
!
! If any device  is already open for PGPLOT output, it is closed before
! the new device is opened.
!
! Returns:
!  PGBEG         : a status return value. A value of 1 indicates
!                    successful completion, any other value indicates
!                    an error. In the event of error a message is
!                    written on the standard error unit.
!                    To test the return value, call
!                    PGBEG as a function, eg IER=PGBEG(...); note
!                    that PGBEG must be declared INTEGER in the
!                    calling program. Some Fortran compilers allow
!                    you to use CALL PGBEG(...) and discard the
!                    return value, but this is not standard Fortran.
! Arguments:
!  UNIT  (input)   : this argument is ignored by PGBEG (use zero).
!  FILE  (input)   : the "device specification" for the plot device.
!                    (For explanation, see description of PGOPEN.)
!  NXSUB  (input)  : the number of subdivisions of the view surface in
!                    X (>0 or <0).
!  NYSUB  (input)  : the number of subdivisions of the view surface in
!                    Y (>0).
!                    PGPLOT puts NXSUB x NYSUB graphs on each plot
!                    page or screen; when the view surface is sub-
!                    divided in this way, PGPAGE moves to the next
!                    panel, not the  next physical page. If
!                    NXSUB > 0, PGPLOT uses the panels in row
!                    order; if <0, PGPLOT uses them in column order.
!--
! 21-Dec-1995 [TJP] - changed for multiple devices; call PGOPEN.
! 27-Feb-1997 [TJP] - updated description.
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: unit
   CHARACTER (LEN=*), INTENT(IN)            :: FILE
   INTEGER, INTENT(IN)                      :: nxsub
   INTEGER, INTENT(IN)                      :: nysub
!
   INTEGER :: ier,pgbeg,i
   INTEGER :: pgopen
!
   i=unit+1
!
! Initialize PGPLOT if necessary.
!
   CALL pginit
!
! Close the plot-file if it is already open.
!
   CALL pgend
!
! Call PGOPEN to open the device.
!
   ier=pgopen(FILE)
   IF(ier > 0)THEN
      CALL pgsubp(nxsub,nysub)
      pgbeg=1
   ELSE
      pgbeg=ier
   END IF
!
   RETURN
!
END FUNCTION pgbeg
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGBEGIN -- non-standard alias for PGBEG
!+
FUNCTION pgbegin(UNIT,FILE,nxsub,nysub)
!
   INTEGER, INTENT(IN)                      :: UNIT
   CHARACTER (LEN=*), INTENT(IN)            :: FILE
   INTEGER, INTENT(IN)                      :: nxsub
   INTEGER, INTENT(IN)                      :: nysub
!
! See description of PGBEG.
!--
   INTEGER :: pgbeg,pgbegin
!
   pgbegin=pgbeg(UNIT,FILE,nxsub,nysub)
!
END FUNCTION pgbegin
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGBIN -- histogram of binned data
!%void cpgbin(int nbin, const float *x, const float *data, \
!% Logical center);
!+
SUBROUTINE pgbin(nbin,x,dat,center)
!
! Plot a histogram of NBIN values with X(1..NBIN) values along
! the ordinate, and DAT(1...NBIN) along the abscissa. Bin width is
! spacing between X values.
!
! Arguments:
!  NBIN   (input)  : number of values.
!  X      (input)  : abscissae of bins.
!  DAT   (input)  : data values of bins.
!  CENTER (input)  : if .TRUE., the X values denote the center of the
!                    bin; if .FALSE., the X values denote the lower
!                    edge (in X) of the bin.
!--
! 19-Aug-92: change argument check (TJP).
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nbin
   REAL(KIND=pg), INTENT(IN)                :: x(*)
   REAL(KIND=pg), INTENT(IN)                :: dat(*)
   LOGICAL, INTENT(IN)                      :: center
!
   LOGICAL :: pgnoto
   INTEGER :: ibin
   REAL(KIND=pg) :: tx(4),ty(4)
!
! Check arguments.
!
   IF(nbin < 2)RETURN
   IF(pgnoto('PGBIN'))RETURN
   CALL pgbbuf
!
! Draw Histogram. Centered an uncentered bins are treated separately.
!
   IF(center)THEN
!
!         !set up initial point.
!
      tx(2)=(3.0_pg*x(1)-x(2))/2.0_pg
      ty(2)=dat(1)
      tx(3)=(x(1)+x(2))/2.0_pg
      ty(3)=ty(2)
      CALL grvct0(2,.false.,2,tx(2),ty(2))
!
!         !draw initial horizontal line
!         !now loop over bins
!
      DO  ibin=2,nbin-1
         tx(1)=tx(3)
         tx(2)=tx(1)
         tx(3)=(x(ibin)+x(ibin+1))/2.0_pg
         ty(1)=ty(3)
         ty(2)=dat(ibin)
         ty(3)=ty(2)
         CALL grvct0(2,.false.,3,tx,ty)
      END DO
!
!         !now draw last segment.
!
      tx(1)=tx(3)
      tx(2)=tx(1)
      tx(3)=(3.*x(nbin)-x(nbin-1))/2.0_pg
      ty(1)=ty(3)
      ty(2)=dat(nbin)
      ty(3)=ty(2)
      CALL grvct0(2,.false.,3,tx,ty)
!
!               Uncentered bins
!
   ELSE
!
!         !set up first line.
!
      tx(2)=x(1)
      ty(2)=dat(1)
      tx(3)=x(2)
      ty(3)=ty(2)
      CALL grvct0(2,.false.,2,tx(2),ty(2))
      DO  ibin=2,nbin
         tx(1)=tx(3)
         tx(2)=tx(1)
         IF(ibin == nbin)THEN
            tx(3)=2.*x(nbin)-x(nbin-1)
         ELSE
            tx(3)=x(ibin+1)
         END IF
         ty(1)=ty(3)
!
!             !get height for last segment.
!
         ty(2)=dat(ibin)
         ty(3)=ty(2)
         CALL grvct0(2,.false.,3,tx,ty)
      END DO
   END IF
!
   CALL pgebuf
!
END SUBROUTINE pgbin
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGBOX -- draw labeled frame around viewport
!%void cpgbox(const char *xopt, float xtick, int nxsub, \
!% const char *yopt, float ytick, int nysub);
!+
SUBROUTINE pgbox(xopt,xtick,nxsub,yopt,ytick,nysub)
!
! Annotate the viewport with frame, axes, numeric labels, etc.
! PGBOX is called by on the user's behalf by PGENV, but may also be
! called explicitly.
!
! Arguments:
!  XOPT   (input)  : string of options for X (horizontal) axis of
!                    plot. Options are single letters, and may be in
!                    any order (see below).
!  XTICK  (input)  : world coordinate interval between major tick marks
!                    on X axis. If XTICK=0.0, the interval is chosen by
!                    PGBOX, so that there will be at least 3 major tick
!                    marks along the axis.
!  NXSUB  (input)  : the number of subintervals to divide the major
!                    coordinate interval into. If XTICK=0.0 or NXSUB=0,
!                    the number is chosen by PGBOX.
!  YOPT   (input)  : string of options for Y (vertical) axis of plot.
!                    Coding is the same as for XOPT.
!  YTICK  (input)  : like XTICK for the Y axis.
!  NYSUB  (input)  : like NXSUB for the Y axis.
!
! Options (for parameters XOPT and YOPT):
!  A : draw Axis (X axis is horizontal line Y=0, Y axis is vertical
!      line X=0).
!  B : draw bottom (X) or left (Y) edge of frame.
!  C : draw top (X) or right (Y) edge of frame.
!  G : draw Grid of vertical (X) or horizontal (Y) lines.
!  I : Invert the tick marks; ie draw them outside the viewport
!      instead of inside.
!  L : label axis Logarithmically (see below).
!  N : write Numeric labels in the conventional location below the
!      viewport (X) or to the left of the viewport (Y).
!  P : extend ("Project") major tick marks outside the box (ignored if
!      option I is specified).
!  M : write numeric labels in the unconventional location above the
!      viewport (X) or to the right of the viewport (Y).
!  T : draw major Tick marks at the major coordinate interval.
!  S : draw minor tick marks (Subticks).
!  V : orient numeric labels Vertically. This is only applicable to Y.
!      The default is to write Y-labels parallel to the axis.
!  1 : force decimal labelling, instead of automatic choice (see PGNUMB).
!  2 : force exponential labelling, instead of automatic.
!
! To get a complete frame, specify BC in both XOPT and YOPT.
! Tick marks, if requested, are drawn on the axes or frame
! or both, depending which are requested. If none of ABC is specified,
! tick marks will not be drawn. When PGENV calls PGBOX, it sets both
! XOPT and YOPT according to the value of its parameter AXIS:
! -1: 'BC', 0: 'BCNST', 1: 'ABCNST', 2: 'ABCGNST'.
!
! For a logarithmic axis, the major tick interval is always 1.0. The
! numeric label is 10**(x) where x is the world coordinate at the
! tick mark. If subticks are requested, 8 subticks are drawn between
! each major tick at equal logarithmic intervals.
!
! To label an axis with time (days, hours, minutes, seconds) or
! angle (degrees, arcmin, arcsec), use routine PGTBOX.
!--
! 19-Oct-1983
! 23-Sep-1984 - fix bug in labelling reversed logarithmic axes.
!  6-May-1985 - improve behavior for pen plotters [TJP].
! 23-Nov-1985 - add 'P' option [TJP].
! 14-Jan-1986 - use new routine PGBOX1 to fix problem of missing
!               labels at end of axis [TJP].
!  8-Apr-1987 - improve automatic choice of tick interval; improve
!               erroneous rounding of tick interval to 1 digit [TJP].
! 23-Apr-1987 - fix bug: limit max number of ticks to ~10 [TJP].
!  7-Nov-1987 - yet another change to algorithm for choosing tick
!               interval; maximum tick interval is now 0.2*range of
!               axis, which may round up to 0.5 [TJP].
! 15-Dec-1988 - correct declaration of MAJOR [TJP].
!  6-Sep-1989 - use Fortran generic intrinsic functions [TJP].
! 18-Oct-1990 - correctly initialize UTAB(1) [AFT].
! 19-Oct-1990 - do all plotting in world coordinates [TJP].
!  6-Nov-1991 - label logarithmic subticks when necessary [TJP].
!  4-Jul-1994 - add '1' and '2' options [TJP].
! 20-Apr-1995 - adjust position of labels slightly, and move out
!               when ticks are inverted [TJP].
! 26-Feb-1997 - use new routine pgclp [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)        :: xopt
   REAL(KIND=pg), INTENT(IN)            :: xtick
   INTEGER, INTENT(IN)                  :: nxsub
   CHARACTER (LEN=*), INTENT(IN)        :: yopt
   REAL(KIND=pg), INTENT(IN)            :: ytick
   INTEGER, INTENT(IN)                  :: nysub
!
   CHARACTER (LEN=20) :: clbl
   CHARACTER (LEN=64) :: opt
   LOGICAL :: xopta,xoptb,xoptc,xoptg,xoptn,xoptm,xoptt,xopts
   LOGICAL :: yopta,yoptb,yoptc,yoptg,yoptn,yoptm,yoptt,yopts
   LOGICAL :: xopti,yopti,yoptv,xoptl,yoptl,xoptp,yoptp
   LOGICAL :: major,xoptls,yoptls,pgnoto
   REAL(KIND=pg) :: tab(9),utab(9)
   INTEGER :: i,i1,i2,j,nc,np,ki,clip
   INTEGER :: nsubx,nsuby,jmax,xnform,ynform
   REAL(KIND=pg) :: tikl,tikl1,tikl2,xc,yc
   REAL(KIND=pg) :: xint,xint2,xval,yint,yint2,yval
   REAL(KIND=pg) :: pgrnd
!REAL(KIND=pg) :: a,b,c
   REAL(KIND=pg) :: xndsp,xmdsp,yndsp,ymdsp,ynvdsp,ymvdsp
   REAL(KIND=pg) :: xblc,xtrc,yblc,ytrc
   INTEGER(KIND=8) :: nv
!
!  JAO made nv a 64 bit integer, also commented out
!  INTRINSIC statement
!
!INTRINSIC  ABS,INDEX,INT,LOG10,MAX,MIN,MOD,nint,SIGN,REAL
!
! Table of logarithms 1..9
!
   DATA tab/0.0_pg,0.301029995663981_pg,0.477121254719662_pg,  &
      0.602059991327962_pg,0.698970004336019_pg,0.77815125038644_pg, &
      0.845098040014257_pg,0.903089986991944_pg,0.954242509439325_pg/
!
!   moved statement function to the end, changed names to avoid
!   conflict with intrinsic function of the same name
!
!rnge(a,b,c)=(a < b.AND.b < c).OR.(c < b.AND.b < a)
!irnge(a,b,c)=(a <= b.AND.b <= c).OR.(c <= b.AND.b <= a)
!
   IF(pgnoto('PGBOX'))RETURN
   CALL pgbbuf
   CALL pgqwin(xblc,xtrc,yblc,ytrc)
!
! Decode options.
!
   CALL grtoup(opt,xopt)
   xopta=INDEX(opt,'A') /= 0.AND.rnge(yblc,0.0_pg,ytrc)
   xoptb=INDEX(opt,'B') /= 0
   xoptc=INDEX(opt,'C') /= 0
   xoptg=INDEX(opt,'G') /= 0
   xopti=INDEX(opt,'I') /= 0
   xoptl=INDEX(opt,'L') /= 0
   xoptm=INDEX(opt,'M') /= 0
   xoptn=INDEX(opt,'N') /= 0
   xopts=INDEX(opt,'S') /= 0
   xoptt=INDEX(opt,'T') /= 0
   xoptp=INDEX(opt,'P') /= 0.AND.(.NOT.xopti)
   xnform=0
   IF(INDEX(opt,'1') /= 0)xnform=1
   IF(INDEX(opt,'2') /= 0)xnform=2
   CALL grtoup(opt,yopt)
   yopta=INDEX(opt,'A') /= 0.AND.rnge(xblc,0.0_pg,xtrc)
   yoptb=INDEX(opt,'B') /= 0
   yoptc=INDEX(opt,'C') /= 0
   yoptg=INDEX(opt,'G') /= 0
   yopti=INDEX(opt,'I') /= 0
   yoptl=INDEX(opt,'L') /= 0
   yoptn=INDEX(opt,'N') /= 0
   yoptm=INDEX(opt,'M') /= 0
   yopts=INDEX(opt,'S') /= 0
   yoptt=INDEX(opt,'T') /= 0
   yoptv=INDEX(opt,'V') /= 0
   yoptp=INDEX(opt,'P') /= 0.AND.(.NOT.yopti)
   ynform=0
   IF(INDEX(opt,'1') /= 0)ynform=1
   IF(INDEX(opt,'2') /= 0)ynform=2
!
! Displacement of labels from edge of box
! (for X bottom/top, Y left/right, and Y left/right with V option).
!
   xndsp=1.2_pg
   xmdsp=0.7_pg
   yndsp=0.7_pg
   ymdsp=1.2_pg
   ynvdsp=0.7_pg
   ymvdsp=0.7_pg
   IF(xopti)THEN
      xndsp=xndsp+0.3_pg
      xmdsp=xmdsp+0.3_pg
   END IF
   IF(yopti)THEN
      yndsp=yndsp+0.3_pg
      ymdsp=ymdsp+0.3_pg
      ynvdsp=ynvdsp+0.3_pg
      ymvdsp=ymvdsp+0.3_pg
   END IF
!
! Disable clipping.
!
   CALL pgqclp(clip)
   CALL pgsclp(0)
!
! Draw box.
!
   IF(xoptb)THEN
      CALL grmova(xblc,yblc)
      CALL grlina(xtrc,yblc)
   END IF
   IF(yoptc)THEN
      CALL grmova(xtrc,yblc)
      CALL grlina(xtrc,ytrc)
   END IF
   IF(xoptc)THEN
      CALL grmova(xtrc,ytrc)
      CALL grlina(xblc,ytrc)
   END IF
   IF(yoptb)THEN
      CALL grmova(xblc,ytrc)
      CALL grlina(xblc,yblc)
   END IF
!
! Draw axes if required.
!
   IF(xopta.AND..NOT.xoptg)THEN
      CALL grmova(xblc,0.0_pg)
      CALL grlina(xtrc,0.0_pg)
   END IF
   IF(yopta.AND..NOT.yoptg)THEN
      CALL grmova(0.0_pg,yblc)
      CALL grlina(0.0_pg,ytrc)
   END IF
!
! Length of X tick marks.
!
   tikl1=pgxsp(pgid)*0.6_pg*(ytrc-yblc)/pgylen(pgid)
   IF(xopti)tikl1=-tikl1
   tikl2=tikl1*0.5_pg
!
! Choose X tick intervals. Major interval = XINT,
! minor interval = XINT2 = XINT/NSUBX.
!
   utab(1)=0.0_pg
   IF(xoptl)THEN
      xint=SIGN(1.0_pg,xtrc-xblc)
      nsubx=1
      DO  j=2,9
         utab(j)=tab(j)
         IF(xint < 0.0_pg)utab(j)=1.0_pg-tab(j)
      END DO
   ELSE IF(ABS(xtick) <= EPSILON(xtick))THEN
      xint=MAX(0.05_pg,MIN(7.0_pg*pgxsp(pgid)/pgxlen(pgid),0.20_pg))*(xtrc- xblc)
      xint=pgrnd(xint,nsubx)
   ELSE
      xint=SIGN(xtick,xtrc-xblc)
      nsubx=MAX(nxsub,1)
   END IF
   IF(.NOT.xopts)nsubx=1
   np=INT(LOG10(ABS(xint)))-4
   nv=nint(xint/10.0_pg**np)
   xint2=xint/nsubx
   xoptls=xoptl.AND.xopts.AND.(ABS(xtrc-xblc) < 2.0_pg)
!
! Draw X grid.
!
   IF(xoptg)THEN
      CALL pgbox1(xblc,xtrc,xint,i1,i2)
      DO  i=i1,i2
         CALL grmova(REAL(i)*xint,yblc)
         CALL grlina(REAL(i)*xint,ytrc)
      END DO
   END IF
!
! Draw X ticks.
!
   IF(xoptt.OR.xopts)THEN
      CALL pgbox1(xblc,xtrc,xint2,i1,i2)
      jmax=1
      IF(xoptl.AND.xopts)jmax=9
!
!         Bottom ticks.
!
      IF(xoptb)THEN
         DO  i=i1-1,i2
            DO  j=1,jmax
               major=(MOD(i,nsubx) == 0).AND.xoptt.AND.j == 1
               tikl=tikl2
               IF(major)tikl=tikl1
               xval=(i+utab(j))*xint2
               IF(irnge(xblc,xval,xtrc))THEN
                  IF(xoptp.AND.major)THEN
                     CALL grmova(xval,yblc-tikl2)
                  ELSE
                     CALL grmova(xval,yblc)
                  END IF
                  CALL grlina(xval,yblc+tikl)
               END IF
            END DO
         END DO
      END IF
!
!         Axis ticks.
!
      IF(xopta)THEN
         DO  i=i1-1,i2
            DO  j=1,jmax
               major=(MOD(i,nsubx) == 0).AND.xoptt.AND.j == 1
               tikl=tikl2
               IF(major)tikl=tikl1
               xval=(i+utab(j))*xint2
               IF(irnge(xblc,xval,xtrc))THEN
                  CALL grmova(xval,-tikl)
                  CALL grlina(xval,tikl)
               END IF
            END DO
         END DO
      END IF
!
!         Top ticks.
!
      IF(xoptc)THEN
         DO  i=i1-1,i2
            DO  j=1,jmax
               major=(MOD(i,nsubx) == 0).AND.xoptt.AND.j == 1
               tikl=tikl2
               IF(major)tikl=tikl1
               xval=(i+utab(j))*xint2
               IF(irnge(xblc,xval,xtrc))THEN
                  CALL grmova(xval,ytrc-tikl)
                  CALL grlina(xval,ytrc)
               END IF
            END DO
         END DO
      END IF
   END IF
!
! Write X labels.
!
   IF(xoptn.OR.xoptm)THEN
      CALL pgbox1(xblc,xtrc,xint,i1,i2)
      DO  i=i1,i2
         xc=(i*xint-xblc)/(xtrc-xblc)
         IF(xoptl)THEN
            CALL pgnumb(INT(1,KIND=8),nint(i*xint),xnform,clbl,nc)
         ELSE
            CALL pgnumb(INT(i,KIND=8)*nv,np,xnform,clbl,nc)
         END IF
         IF(xoptn)CALL pgmtxt('B',xndsp,xc,0.5_pg,clbl(1:nc))
         IF(xoptm)CALL pgmtxt('T',xmdsp,xc,0.5_pg,clbl(1:nc))
      END DO
   END IF
!
! Extra X labels for log axes.
!
   IF(xoptls)THEN
      CALL pgbox1(xblc,xtrc,xint2,i1,i2)
      DO  i=i1-1,i2
         DO  j=2,5,3
            xval=(i+utab(j))*xint2
            xc=(xval-xblc)/(xtrc-xblc)
            ki=i
            IF(xtrc < xblc)ki=ki+1
            IF(irnge(xblc,xval,xtrc))THEN
               CALL pgnumb(INT(j,KIND=8),nint(ki*xint2),xnform,clbl,nc)
               IF(xoptn)CALL pgmtxt('B',xndsp,xc,0.5_pg,clbl(1:nc))
               IF(xoptm)CALL pgmtxt('T',xmdsp,xc,0.5_pg,clbl(1:nc))
            END IF
         END DO
      END DO
   END IF
!
! Length of Y tick marks.
!
   tikl1=pgxsp(pgid)*0.6_pg*(xtrc-xblc)/pgxlen(pgid)
   IF(yopti)tikl1=-tikl1
   tikl2=tikl1*0.5_pg
!
! Choose Y tick intervals. Major interval = YINT,
! minor interval = YINT2 = YINT/NSUBY.
!
   utab(1)=0.0_pg
   IF(yoptl)THEN
      yint=SIGN(1.0_pg,ytrc-yblc)
      nsuby=1
      DO  j=2,9
         utab(j)=tab(j)
         IF(yint < 0.0_pg)utab(j)=1.0_pg-tab(j)
      END DO
   ELSE IF(ABS(ytick) <= EPSILON(ytick))THEN
      yint=MAX(0.05_pg,MIN(7.0_pg*pgxsp(pgid)/pgylen(pgid),0.20_pg))*(ytrc-yblc)
      yint=pgrnd(yint,nsuby)
   ELSE
      yint=SIGN(ytick,ytrc-yblc)
      nsuby=MAX(nysub,1)
   END IF
   IF(.NOT.yopts)nsuby=1
   np=INT(LOG10(ABS(yint)))-4
   nv=nint(yint/10.0_pg**np)
   yint2=yint/nsuby
   yoptls=yoptl.AND.yopts.AND.(ABS(ytrc-yblc) < 2.0_pg)
!
! Draw Y grid.
!
   IF(yoptg)THEN
      CALL pgbox1(yblc,ytrc,yint,i1,i2)
      DO  i=i1,i2
         CALL grmova(xblc,REAL(i)*yint)
         CALL grlina(xtrc,REAL(i)*yint)
      END DO
   END IF
!
! Draw Y ticks.
!
   IF(yoptt.OR.yopts)THEN
      CALL pgbox1(yblc,ytrc,yint2,i1,i2)
      jmax=1
      IF(yoptl.AND.yopts)jmax=9
!
!               Left ticks.
!
      IF(yoptb)THEN
         DO  i=i1-1,i2
            DO  j=1,jmax
               major=(MOD(i,nsuby) == 0).AND.yoptt.AND.j == 1
               tikl=tikl2
               IF(major)tikl=tikl1
               yval=(i+utab(j))*yint2
               IF(irnge(yblc,yval,ytrc))THEN
                  IF(yoptp.AND.major)THEN
                     CALL grmova(xblc-tikl2,yval)
                  ELSE
                     CALL grmova(xblc,yval)
                  END IF
                  CALL grlina(xblc+tikl,yval)
               END IF
            END DO
         END DO
      END IF
!
!               Axis ticks.
!
      IF(yopta)THEN
         DO  i=i1-1,i2
            DO  j=1,jmax
               major=(MOD(i,nsuby) == 0).AND.yoptt.AND.j == 1
               tikl=tikl2
               IF(major)tikl=tikl1
               yval=(i+utab(j))*yint2
               IF(irnge(yblc,yval,ytrc))THEN
                  CALL grmova(-tikl,yval)
                  CALL grlina(tikl,yval)
               END IF
            END DO
         END DO
      END IF
!
!               Right ticks.
!
      IF(yoptc)THEN
         DO  i=i1-1,i2
            DO  j=1,jmax
               major=(MOD(i,nsuby) == 0).AND.yoptt.AND.j == 1
               tikl=tikl2
               IF(major)tikl=tikl1
               yval=(i+utab(j))*yint2
               IF(irnge(yblc,yval,ytrc))THEN
                  CALL grmova(xtrc-tikl,yval)
                  CALL grlina(xtrc,yval)
               END IF
            END DO
         END DO
      END IF
   END IF
!
! Write Y labels.
!
   IF(yoptn.OR.yoptm)THEN
      CALL pgbox1(yblc,ytrc,yint,i1,i2)
      DO  i=i1,i2
         yc=(i*yint-yblc)/(ytrc-yblc)
         IF(yoptl)THEN
            CALL pgnumb(INT(1,KIND=8),nint(i*yint),ynform,clbl,nc)
         ELSE
            CALL pgnumb(INT(i,KIND=8)*nv,np,ynform,clbl,nc)
         END IF
         IF(yoptv)THEN
            IF(yoptn)CALL pgmtxt('LV',ynvdsp,yc,1.0_pg,clbl(1:nc))
            IF(yoptm)CALL pgmtxt('RV',ymvdsp,yc,0.0_pg,clbl(1:nc))
         ELSE
            IF(yoptn)CALL pgmtxt('L',yndsp,yc,0.5_pg,clbl(1:nc))
            IF(yoptm)CALL pgmtxt('R',ymdsp,yc,0.5_pg,clbl(1:nc))
         END IF
      END DO
   END IF
!
! Extra Y labels for log axes.
!
   IF(yoptls)THEN
      CALL pgbox1(yblc,ytrc,yint2,i1,i2)
      DO  i=i1-1,i2
         DO  j=2,5,3
            yval=(i+utab(j))*yint2
            yc=(yval-yblc)/(ytrc-yblc)
            ki=i
            IF(yblc > ytrc)ki=ki+1
            IF(irnge(yblc,yval,ytrc))THEN
               CALL pgnumb(INT(j,KIND=8),nint(ki*yint2),ynform,clbl,nc)
               IF(yoptv)THEN
                  IF(yoptn)CALL pgmtxt('LV',ynvdsp,yc,1.0_pg,clbl(1:nc))
                  IF(yoptm)CALL pgmtxt('RV',ymvdsp,yc,0.0_pg,clbl(1:nc))
               ELSE
                  IF(yoptn)CALL pgmtxt('L',yndsp,yc,0.5_pg,clbl(1:nc))
                  IF(yoptm)CALL pgmtxt('R',ymdsp,yc,0.5_pg,clbl(1:nc))
               END IF
            END IF
         END DO
      END DO
   END IF
!
! Enable clipping.
!
   CALL pgsclp(clip)
!
   CALL pgebuf
!
   RETURN
!
CONTAINS
!
   FUNCTION rnge(a,b,c)
!
      USE accur
!
      IMPLICIT NONE
!
      REAL(KIND=pg), INTENT(IN)  :: a
      REAL(KIND=pg), INTENT(IN)  :: b
      REAL(KIND=pg), INTENT(IN)  :: c
!
      LOGICAL  :: rnge
!
      rnge = (a < b.AND.b < c).OR.(c < b.AND.b < a)
!
      RETURN
!
   END FUNCTION rnge
!
   FUNCTION irnge(a,b,c)
!
      USE accur
!
      IMPLICIT NONE
!
      REAL(KIND=pg), INTENT(IN)  :: a
      REAL(KIND=pg), INTENT(IN)  :: b
      REAL(KIND=pg), INTENT(IN)  :: c
!
      LOGICAL  :: irnge
!
      irnge = (a <= b.AND.b <= c).OR.(c <= b.AND.b <= a)
!
      RETURN
!
   END FUNCTION irnge
!
END SUBROUTINE pgbox
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
! PGBOX1 -- support routine for PGBOX
!
SUBROUTINE pgbox1(xa,xb,xd,i1,i2)
!
! This routine is used to determine where to draw the tick marks on
! an axis. The input arguments XA and XB are the world-coordinate
! end points of the axis; XD is the tick interval. PGBOX1 returns
! two integers, I1 and I2, such that the required tick marks are
! to be placed at world-coordinates (I*XD), for I=I1,...,I2.
! Normally I2 is greater than or equal to I1, but if there are no
! values of I such that I*XD lies in the inclusive range (XA, XB),
! then I2 will be 1 less than I1.
!
! Arguments:
!  XA, XB (input)  : world-coordinate end points of the axis. XA must
!                    not be equal to XB; otherwise, there are no
!                    restrictions.
!  XD     (input)  : world-coordinate tick interval. XD may be positive
!                    or negative, but may not be zero.
!  I1, I2 (output) : tick marks should be drawn at world
!                    coordinates I*XD for I in the inclusive range
!                    I1...I2 (see above).
!
! 14-Jan-1986 - new routine [TJP].
! 13-Dec-1990 - remove rror check [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: xa
   REAL(KIND=pg), INTENT(IN)                :: xb
   REAL(KIND=pg), INTENT(IN)                :: xd
   INTEGER, INTENT(OUT)                     :: i1
   INTEGER, INTENT(OUT)                     :: i2
!
   REAL(KIND=pg) :: xlo,xhi
!
   xlo=MIN(xa/xd,xb/xd)
   xhi=MAX(xa/xd,xb/xd)
   i1=nint(xlo)
   IF(i1 < xlo)i1=i1+1
   i2=nint(xhi)
   IF(i2 > xhi)i2=i2-1
!
END SUBROUTINE pgbox1
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCIRC -- draw a circle, using fill-area attributes
!%void cpgcirc(float xcent, float ycent, float radius);
!+
SUBROUTINE pgcirc(xcent,ycent,radius)
!
! Draw a circle. The action of this routine depends
! on the setting of the Fill-Area Style attribute. If Fill-Area Style
! is SOLID (the default), the interior of the circle is solid-filled
! using the current Color Index. If Fill-Area Style is HOLLOW, the
! outline of the circle is drawn using the current line attributes
! (color index, line-style, and line-width).
!
! Arguments:
!  XCENT  (input)  : world x-coordinate of the center of the circle.
!  YCENT  (input)  : world y-coordinate of the center of the circle.
!  RADIUS (input)  : radius of circle (world coordinates).
!--
! 26-Nov-1992 - [TJP].
! 20-Sep-1994 - adjust number of points according to size [TJP].
!-----------------------------------------------------------------------\
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: xcent
   REAL(KIND=pg), INTENT(IN)                         :: ycent
   REAL(KIND=pg), INTENT(IN)                         :: radius
!
   INTEGER, PARAMETER :: maxpts=72
!
   INTEGER :: npts,i,radpix
   REAL(KIND=pg) :: angle
   REAL(KIND=pg) :: x(maxpts),y(maxpts)
!
   radpix=nint(radius*MAX(pgxscl(pgid),pgyscl(pgid)))
   npts=MAX(8,MIN(maxpts,radpix))
   DO  i=1,npts
      angle=i*360.0_pg/REAL(npts,KIND=pg)/57.295779513082321_pg
      x(i)=xcent+radius*COS(angle)
      y(i)=ycent+radius*SIN(angle)
   END DO
   CALL pgpoly(npts,x,y)
!
!     write (*,*) 'PGCIRC', NPTS
!
   RETURN
!
END SUBROUTINE pgcirc
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pgcl(k,x,y,z)
!
! PGPLOT (internal routine): Label one contour segment (for use by
! PGCONX).
!
! Arguments:
!
! K (input, integer): if K=0, move the pen to (X,Y); if K=1, draw
!       a line from the current position to (X,Y); otherwise
!       do nothing.
! X (input, real): X world-coordinate of end point.
! Y (input, real): Y world-coordinate of end point.
! Z (input, real): the value of the contour level, not used by PGCL.
!--
!  5-May-1994 - new routine [TJP]
!  7-Mar-1995 - correct error in angle; do not draw labels outside
!               window [TJP].
! 28-Aug-1995 - check arguments of atan2 [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: k
   REAL(KIND=pg), INTENT(IN)                :: x
   REAL(KIND=pg), INTENT(IN)                :: y
   REAL(KIND=pg), INTENT(IN)                :: z
!
   REAL(KIND=pg) :: xx,yy,xc,yc,xv1,xv2,yv1,yv2,xl,xr,yb,yt
   REAL(KIND=pg) :: xn,yn
   REAL(KIND=pg) :: angle,xo,yo,xp,yp,dindx,dindy,xbox(4),ybox(4)
   INTEGER :: i,tb
   SAVE  i
   DATA i/0/
!
!     -- transform to world coordinates
!
   xx=1.1_pg*z
!
   xx=trans(1)+trans(2)*x+trans(3)*y
   yy=trans(4)+trans(5)*x+trans(6)*y
!
   IF(k == 0)THEN
!
!        -- start of contour: reset segment counter
!
      i=0
   ELSE
!
!        -- increment segment counter and check whether this
!           segment should be labelled
!
      i=MOD(i+1,pgcint)
      IF(i == pgcmin)THEN
!
!           -- find center of this segment (XC, YC)
!
         CALL pgqpos(xp,yp)
         xc=(xx+xp)*0.5_pg
         yc=(yy+yp)*0.5_pg
!
!            -- find slope of this segment (ANGLE)
!
         CALL pgqvp(1,xv1,xv2,yv1,yv2)
         CALL pgqwin(xl,xr,yb,yt)
         angle=0.0_pg
         IF((ABS(xr - xl) > EPSILON(xr)).AND.(ABS(yt - yb) > EPSILON(yt)))THEN
            dindx=(xv2-xv1)/(xr-xl)
            dindy=(yv2-yv1)/(yt-yb)
            IF((ABS(yy-yp) > EPSILON(yy)).OR.(ABS(xx-xp) > EPSILON(xx)))angle= &
               57.295779513082321_pg*ATAN2((yy-yp)*dindy,(xx-xp)*dindx)
         END IF
!
!           -- check whether point is in window
!
         xn=(xc-xl)/(xr-xl)
         yn=(yc-yb)/(yt-yb)
         IF(xn >= 0.0_pg.AND.xn <= 1.0_pg.AND.yn >= 0.0_pg.AND.yn <= 1.0_pg)THEN
!
!              -- save current text background and set to erase
!
            CALL pgqtbg(tb)
            CALL pgstbg(0)
!
!              -- find bounding box of label
!
            CALL pgqtxt(xc,yc,angle,0.5_pg,pgclab,xbox,ybox)
            xo=0.5_pg*(xbox(1)+xbox(3))
            yo=0.5_pg*(ybox(1)+ybox(3))
!
!              -- plot label with bounding box centered at (XC, YC)
!
            CALL pgptxt(2.0_pg*xc-xo,2.0_pg*yc-yo,angle,0.5_pg,pgclab)
!
!              -- restore text background
!
            CALL pgstbg(tb)
         END IF
      END IF
   END IF
   CALL pgmove(xx,yy)
!
   RETURN
!
END SUBROUTINE pgcl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCLOS -- close the selected graphics device
!%void cpgclos(void);
!+
SUBROUTINE pgclos
!
! Close the currently selected graphics device. After the device has
! been closed, either another open device must be selected with PGSLCT
! or another device must be opened with PGOPEN before any further
! plotting can be done. If the call to PGCLOS is omitted, some or all
! of the plot may be lost.
!
! [This routine was added to PGPLOT in Version 5.1.0. Older programs
! use PGEND instead.]
!
! Arguments: none
!--
! 22-Dec-1995 - new routine, derived from the old PGEND.
!-----------------------------------------------------------------------
!
   USE pgplot
!
   CHARACTER (LEN=16) :: defstr
   LOGICAL :: pgnoto
!
   IF(.NOT.pgnoto('PGCLOS'))THEN
      CALL grterm
      IF(pgprmp(pgid))THEN
         CALL grqcap(defstr)
         IF(defstr(8:8) == 'V')CALL grprom
      END IF
      CALL grclos
      pgdevs(pgid)=0
      pgid=0
   END IF
!
!     WRITE (*,*) 'PGCLOS', PGID, ':', PGDEVS
!
   RETURN
!
END SUBROUTINE pgclos
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pgcn01 (z,mx,my,ia,ib,ja,jb,z0,plot,flags,is,js,sdir)
!
! Support routine for PGCNSC. This routine draws a continuous contour,
! starting at the specified point, until it either crosses the edge of
! the array or closes on itself.
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: mx
   INTEGER, INTENT(IN)                      :: my
   REAL(KIND=pg), INTENT(IN)                :: z(mx,*)
   INTEGER, INTENT(IN)                      :: ia
   INTEGER, INTENT(IN)                      :: ib
   INTEGER, INTENT(IN)                      :: ja
   INTEGER, INTENT(IN)                      :: jb
   REAL(KIND=pg), INTENT(IN)                :: z0
   INTEGER, PARAMETER                       :: maxemx=100
   INTEGER, PARAMETER                       :: maxemy=100
   LOGICAL, INTENT(OUT)                     :: flags(maxemx,maxemy,2)
   INTEGER, INTENT(IN)                      :: is
   INTEGER, INTENT(IN)                      :: js
   INTEGER, INTENT(IN)                      :: sdir
!
   INTEGER, PARAMETER :: up=1
   INTEGER, PARAMETER :: down=2
   INTEGER, PARAMETER :: left=3
   INTEGER, PARAMETER :: right=4
!
   INTEGER :: i,j,ii,jj,dir
!
   REAL(KIND=pg) :: x,y,startx,starty
!
!   EXTERNAL plot
!
   INTERFACE
      SUBROUTINE plot(k,x,y,z)
        USE accur
        IMPLICIT NONE
        INTEGER,INTENT(IN)  ::  k
        REAL(KIND=pg), INTENT(IN) :: x,y,z
      END SUBROUTINE
   END INTERFACE
!
   i=my+1
   i=is
   j=js
   dir=sdir
   ii=1+i-ia
   jj=1+j-ja
   IF(dir == up.OR.dir == down)THEN
      x=REAL(i,KIND=pg)+(z0-z(i,j))/(z(i+1,j)-z(i,j))
      y=REAL(j,KIND=pg)
   ELSE
      x=REAL(i,KIND=pg)
      y=REAL(j,KIND=pg)+(z0-z(i,j))/(z(i,j+1)-z(i,j))
   END IF
!
!D    WRITE (*,*) 'SEGMENT'
!
! Move to start of contour and record starting point.
!
   CALL plot(0,x,y,z0)
   startx=x
   starty=y
!
! We have reached grid-point (I,J) going in direction DIR (UP, DOWN,
! LEFT, or RIGHT). Look at the other three sides of the cell we are
! entering to decide where to go next. It is important to look to the
! two sides before looking straight ahead, in order to avoid self-
! intersecting contours. If all 3 sides have unused crossing-points,
! the cell is "degenerate" and we have to decide which of two possible
! pairs of contour segments to draw; at present we make an arbitrary
! choice. If we have reached the edge of the array, we have
! finished drawing an unclosed contour. If none of the other three
! sides of the cell have an unused crossing-point, we must have
! completed a closed contour, which requires a final segment back to
! the starting point.
!
!D    WRITE (*,*) I,J,DIR
!
10 ii=1+i-ia
   jj=1+j-ja
   SELECT CASE (dir)
    CASE (1)
      GO TO 20
    CASE (2)
      GO TO 30
    CASE (3)
      GO TO 40
    CASE (4)
      GO TO 50
   END SELECT
!
! DIR = UP
!
20 CONTINUE
   flags(ii,jj,1)=.false.
   IF(j == jb)THEN
      RETURN
   ELSE IF(flags(ii,jj,2))THEN
      dir=left
      GO TO 60
   ELSE IF(flags(ii+1,jj,2))THEN
      dir=right
      i=i+1
      GO TO 60
   ELSE IF(flags(ii,jj+1,1))THEN
!
!!        DIR = UP
!
      j=j+1
      GO TO 70
   ELSE
      GO TO 80
   END IF
!
! DIR = DOWN
!
30 CONTINUE
   flags(ii,jj,1)=.false.
   IF(j == ja)THEN
      RETURN
   ELSE IF(flags(ii+1,jj-1,2))THEN
      dir=right
      i=i+1
      j=j-1
      GO TO 60
   ELSE IF(flags(ii,jj-1,2))THEN
      dir=left
      j=j-1
      GO TO 60
   ELSE IF(flags(ii,jj-1,1))THEN
!
!!        DIR = DOWN
!
      j=j-1
      GO TO 70
   ELSE
      GO TO 80
   END IF
!
! DIR = LEFT
!
40 CONTINUE
   flags(ii,jj,2)=.false.
   IF(i == ia)THEN
      RETURN
   ELSE IF(flags(ii-1,jj,1))THEN
      dir=down
      i=i-1
      GO TO 70
   ELSE IF(flags(ii-1,jj+1,1))THEN
      dir=up
      i=i-1
      j=j+1
      GO TO 70
   ELSE IF(flags(ii-1,jj,2))THEN
!!        DIR = LEFT
      i=i-1
      GO TO 60
   ELSE
      GO TO 80
   END IF
!
! DIR = RIGHT
!
50 CONTINUE
   flags(ii,jj,2)=.false.
   IF(i == ib)THEN
      RETURN
   ELSE IF(flags(ii,jj+1,1))THEN
      dir=up
      j=j+1
      GO TO 70
   ELSE IF(flags(ii,jj,1))THEN
      dir=down
      GO TO 70
   ELSE IF(flags(ii+1,jj,2))THEN
!
!!        DIR = RIGHT
!
      i=i+1
      GO TO 60
   ELSE
      GO TO 80
   END IF
!
! Draw a segment of the contour.
!
60 x=REAL(i,KIND=pg)
   y=REAL(j,KIND=pg)+(z0-z(i,j))/(z(i,j+1)-z(i,j))
   CALL plot(1,x,y,z0)
   GO TO 10
70 x=REAL(i,KIND=pg)+(z0-z(i,j))/(z(i+1,j)-z(i,j))
   y=REAL(j,KIND=pg)
   CALL plot(1,x,y,z0)
   GO TO 10
!
! Close the contour and go look for another one.
!
80  CALL plot(1,startx,starty,z0)
!
   RETURN
!
END SUBROUTINE pgcn01
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pgcnsc(z,mx,my,ia,ib,ja,jb,z0,plot)
!
! PGPLOT (internal routine): Draw a single contour.  This routine is
! called by PGCONT, but may be called directly by the user.
!
! Arguments:
!
! Z (real array dimension MX,MY, input): the array of function values.
! MX,MY (integer, input): actual declared dimension of Z(*,*).
! IA,IB (integer, input): inclusive range of the first index of Z to be
!       contoured.
! JA,JB (integer, input): inclusive range of the second index of Z to
!       be contoured.
! Z0 (real, input): the contour level sought.
! PLOT (the name of a subroutine declared EXTERNAL in the calling
!       routine): this routine is called by PGCNSC to do all graphical
!       output. The calling sequence is CALL PLOT(K,X,Y,Z) where Z is
!       the contour level, (X,Y) are the coordinates of a point (in the
!       inclusive range I1<X<I2, J1<Y<J2, and if K is 0, the routine is
!       to move then pen to (X,Y); if K is 1, it is to draw a line from
!       the current position to (X,Y).
!
! NOTE:  the intervals (IA,IB) and (JA,JB) must not exceed the
! dimensions of an internal array. These are currently set at 100.
!--
! 17-Sep-1989 - Completely rewritten [TJP]. The algorithm is my own,
!               but it is probably not original. It could probably be
!               coded more briefly, if not as clearly.
!  1-May-1994 - Modified to draw contours anticlockwise about maxima,
!               to prevent contours at different levels from
!               crossing in degenerate cells [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: mx
   INTEGER, INTENT(IN)                      :: my
   REAL(KIND=pg), INTENT(IN)                :: z(mx,*)
   INTEGER, INTENT(IN)                      :: ia
   INTEGER, INTENT(IN)                      :: ib
   INTEGER, INTENT(IN)                      :: ja
   INTEGER, INTENT(IN)                      :: jb
   REAL(KIND=pg), INTENT(IN)                :: z0
!
!  EXTERNAL plot
!
   INTERFACE
      SUBROUTINE plot(k,x,y,z)
        USE accur
        IMPLICIT NONE
        INTEGER,INTENT(IN)  ::  k
        REAL(KIND=pg), INTENT(IN) :: x,y,z
      END SUBROUTINE
   END INTERFACE
!
   INTEGER, PARAMETER :: up=1
   INTEGER, PARAMETER :: down=2
   INTEGER, PARAMETER :: left=3
   INTEGER, PARAMETER :: right=4
   INTEGER, PARAMETER :: maxemx=100
   INTEGER, PARAMETER :: maxemy=100
!
   LOGICAL :: flags(maxemx,maxemy,2)
   INTEGER :: i,j,ii,jj,dir
   REAL(KIND=pg) :: z1,z2,z3
!
! The statement function RANGE decides whether a contour at level P
! crosses the line between two gridpoints with values P1 and P2. It is
! important that a contour cannot cross a line with equal endpoints.
!
!  moved to the bottom, changed name to rnge
!
! Check for errors.
!
   IF((ib-ia+1) > maxemx.OR.(jb-ja+1) > maxemy)THEN
      CALL grwarn('PGCNSC - array index range exceeds'//  &
         ' built-i n limit of 100')
      RETURN
   END IF
!
! Initialize the flags. The first flag for a gridpoint is set if
! the contour crosses the line segment to the right of the gridpoint
! (joining [I,J] to [I+1,J]); the second flag is set if if it crosses
! the line segment above the gridpoint (joining [I,J] to [I,J+1]).
! The top and right edges require special treatment. (For purposes
! of description only, we assume I increases horizontally to the right
! and J increases vertically upwards.)
!
   DO  i=ia,ib
      ii=i-ia+1
      DO  j=ja,jb
         jj=j-ja+1
         z1=z(i,j)
         flags(ii,jj,1)=.false.
         flags(ii,jj,2)=.false.
         IF(i < ib)THEN
            z2=z(i+1,j)
            IF(rnge(z0,z1,z2))flags(ii,jj,1)=.true.
         END IF
         IF(j < jb)THEN
            z3=z(i,j+1)
            IF(rnge(z0,z1,z3))flags(ii,jj,2)=.true.
         END IF
      END DO
   END DO
!
! Search the edges of the array for the start of an unclosed contour.
! Note that (if the algorithm is implemented correctly) all unclosed
! contours must begin and end at the edge of the array. When one is
! found, call PGCN01 to draw the contour, telling it the correct
! starting direction so that it follows the contour into the array
! instead of out of it. A contour is only started if the higher
! ground lies to the left: this is to enforce the direction convention
! that contours are drawn anticlockwise around maxima. If the high
! ground lies to the right, we will find the other end of the contour
! and start there.
!
! Bottom edge.
!
   j=ja
   jj=j-ja+1
   DO  i=ia,ib-1
      ii=i-ia+1
      IF(flags(ii,jj,1).AND.(z(i,j) > z(i+1,j)))CALL pgcn01(z,  &
         mx,my,ia,ib,ja,jb,z0,plot,flags,i,j,up)
   END DO
!
! Right edge.
!
   i=ib
   ii=i-ia+1
   DO  j=ja,jb-1
      jj=j-ja+1
      IF(flags(ii,jj,2).AND.(z(i,j) > z(i,j+1)))CALL pgcn01(z,  &
         mx,my,ia,ib,ja,jb,z0,plot,flags,i,j,left)
   END DO
!
! Top edge.
!
   j=jb
   jj=j-ja+1
   DO  i=ib-1,ia,-1
      ii=i-ia+1
      IF(flags(ii,jj,1).AND.(z(i+1,j) > z(i,j)))CALL pgcn01(z,  &
         mx,my,ia,ib,ja,jb,z0,plot,flags,i,j,down)
   END DO
!
! Left edge.
!
   i=ia
   ii=i-ia+1
   DO  j=jb-1,ja,-1
      jj=j-ja+1
      IF(flags(ii,jj,2).AND.(z(i,j+1) > z(i,j)))CALL pgcn01(z,  &
         mx,my,ia,ib,ja,jb,z0,plot,flags,i,j,right)
   END DO
!
! Now search the interior of the array for a crossing point, which will
! lie on a closed contour (because all unclosed contours have been
! eliminated). It is sufficient to search just the horizontal crossings
! (or the vertical ones); any closed contour must cross a horizontal
! and a vertical gridline. PGCN01 assumes that when it cannot proceed
! any further, it has reached the end of a closed contour. Thus all
! unclosed contours must be eliminated first.
!
   DO  i=ia+1,ib-1
      ii=i-ia+1
      DO  j=ja+1,jb-1
         jj=j-ja+1
         IF(flags(ii,jj,1))THEN
            dir=up
            IF(z(i+1,j) > z(i,j))dir=down
            CALL pgcn01(z,mx,my,ia,ib,ja,jb,z0,plot,flags,i,j,dir)
         END IF
      END DO
   END DO
!
! We didn't find any more crossing points: we're finished.
!
   RETURN
!
CONTAINS
!
   FUNCTION rnge(p,p1,p2)
!
      USE accur
!
      IMPLICIT NONE
!
      REAL(KIND=pg), INTENT(IN)  :: p
      REAL(KIND=pg), INTENT(IN)  :: p1
      REAL(KIND=pg), INTENT(IN)  :: p2
!
      LOGICAL     :: rnge
      rnge=(p > MIN(p1,p2)).AND.(p <= MAX(p1,p2)).AND.(ABS(p1 - p2) > &
          EPSILON(p1))
!
      RETURN
!
   END FUNCTION rnge
!
END SUBROUTINE pgcnsc
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCONB -- contour map of a 2D data array, with blanking
!%void cpgconb(const float *a, int idim, int jdim, int i1, int i2, \
!% int j1, int j2, const float *c, int nc, const float *tr, \
!% float blank);
!+
SUBROUTINE pgconb(a,idim,jdim,i1,i2,j1,j2,c,nc,tr,blnk)
!
! Draw a contour map of an array. This routine is the same as PGCONS,
! except that array elements that have the "magic value" defined by
! argument blnk are ignored, making gaps in the contour map. The
! routine may be useful for data measured on most but not all of the
! points of a grid.
!
! Arguments:
!  A      (input)  : data array.
!  IDIM   (input)  : first dimension of A.
!  JDIM   (input)  : second dimension of A.
!  I1,I2  (input)  : range of first index to be contoured (inclusive).
!  J1,J2  (input)  : range of second index to be contoured (inclusive).
!  C      (input)  : array of contour levels (in the same units as the
!                    data in array A); dimension at least NC.
!  NC     (input)  : number of contour levels (less than or equal to
!                    dimension of C). The absolute value of this
!                    argument is used (for compatibility with PGCONT,
!                    where the sign of NC is significant).
!  TR     (input)  : array defining a transformation between the I,J
!                    grid of the array and the world coordinates. The
!                    world coordinates of the array point A(I,J) are
!                    given by:
!                      X = TR(1) + TR(2)*I + TR(3)*J
!                      Y = TR(4) + TR(5)*I + TR(6)*J
!                    Usually TR(3) and TR(5) are zero - unless the
!                    coordinate transformation involves a rotation
!                    or shear.
!  blnk   (input) : elements of array A that are exactly equal to
!                    this value are ignored (blanked).
!--
! 21-Sep-1989 - Derived from PGCONS [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   REAL(KIND=pg), INTENT(IN)                :: a(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: c(*)
   INTEGER, INTENT(IN)                      :: nc
   REAL(KIND=pg), INTENT(IN)                :: tr(6)
   REAL(KIND=pg), INTENT(IN)                :: blnk
!
   INTEGER :: i,ic,icorn,idelt(6),j,k,npt
   INTEGER :: ioff(8),joff(8),ienc,itmp,jtmp,ilo,itot
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: ctr,delta,dval(5),xx,yy,x(4),y(4)
!
!INTRINSIC  ABS
!
   DATA idelt/0,-1,-1,0,0,-1/
   DATA ioff/-2,-2,-1,-1,0,0,1,1/
   DATA joff/0,-1,-2,1,-2,1,-1,0/
!
! Check arguments.
!
   IF(pgnoto('PGCONB'))RETURN
   IF(i1 < 1.OR.i2 > idim.OR.i1 >= i2.OR.j1 < 1.OR.j2 > jdim.OR.j1 >= j2)RETURN
   IF(nc == 0)RETURN
   CALL pgbbuf
!
   DO  j=j1+1,j2
      DO  i=i1+1,i2
         dval(1)=a(i-1,j)
         dval(2)=a(i-1,j-1)
         dval(3)=a(i,j-1)
         dval(4)=a(i,j)
         dval(5)=dval(1)
         IF((ABS(dval(1) - blnk) <= EPSILON(blnk)).OR.(ABS(dval(2) - blnk) &
                 <= EPSILON(blnk))  &
            .OR.(ABS(dval(3) - blnk) <= EPSILON(blnk)).OR.(ABS(dval(4) -  &
                 blnk) <= EPSILON(blnk)))CYCLE
         DO  ic=1,ABS(nc)
            ctr=c(ic)
            npt=0
            DO  icorn=1,4
               IF((dval(icorn) < ctr.AND.dval(icorn+1) < ctr).OR.  &
                  (dval(icorn) >= ctr.AND.dval(icorn+1) >= ctr))CYCLE
               npt=npt+1
               delta=(ctr-dval(icorn))/(dval(icorn+1)-dval(icorn))
               SELECT CASE (icorn)
                CASE (1)
                  GO TO 10
                CASE (2)
                  GO TO 20
                CASE (3)
                  GO TO 10
                CASE (4)
                  GO TO 20
               END SELECT
!
10             xx=i+idelt(icorn+1)
               yy=REAL(j+idelt(icorn),KIND=pg)+delta*REAL(idelt(icorn+1)- &
                  idelt(icorn),KIND=pg)
               GO TO 30
!
20             xx=REAL(i+idelt(icorn+1),KIND=pg)+delta*REAL(idelt(icorn+2)-  &
                  idelt(icorn+1),KIND=pg)
               yy=j+idelt(icorn)
!
30             x(npt)=tr(1)+tr(2)*xx+tr(3)*yy
               y(npt)=tr(4)+tr(5)*xx+tr(6)*yy
!
            END DO
            IF(npt == 2)THEN
!
!             -- Contour crosses two sides of cell. Draw line-segment.
!
               CALL pgmove(x(1),y(1))
               CALL pgdraw(x(2),y(2))
            ELSE IF(npt == 4)THEN
!
!             -- The 'ambiguous' case.  The routine must draw two line
!             segments here and there are two ways to do so.  The
!             following 4 lines would implement the original PGPLOT
!             method:
!            CALL PGCP(0,X(1),Y(1),CTR)
!            CALL PGCP(1,X(2),Y(2),CTR)
!            CALL PGCP(0,X(3),Y(3),CTR)
!            CALL PGCP(1,X(4),Y(4),CTR)
!            -- Choose between \\ and // based on the 8 points just
!            outside the current box.  If half or more of these points
!            lie below the contour level, then draw the lines such that
!            the high corners lie between the lines, otherwise, draw
!            the lines such that the low corners are enclosed.  Care is
!            taken to avoid going off the edge.
!
               itot=0
               ilo=0
               DO  k=1,8
                  itmp=i+ioff(k)
                  jtmp=j+joff(k)
                  IF(itmp < i1.OR.itmp > i2)CYCLE
                  IF(jtmp < j1.OR.jtmp > j2)CYCLE
                  IF(ABS(a(itmp,jtmp) - blnk) <= EPSILON(blnk))CYCLE
                  itot=itot+1
                  IF(a(itmp,jtmp) < ctr)ilo=ilo+1
               END DO
               ienc=+1
               IF(ilo < itot/2)ienc=-1
               IF(ienc < 0.AND.dval(1) < ctr.OR.ienc > 0.AND.dval(1)  >= &
                      ctr)THEN
                  CALL pgmove(x(1),y(1))
                  CALL pgdraw(x(2),y(2))
                  CALL pgmove(x(3),y(3))
                  CALL pgdraw(x(4),y(4))
               ELSE
                  CALL pgmove(x(1),y(1))
                  CALL pgdraw(x(4),y(4))
                  CALL pgmove(x(3),y(3))
                  CALL pgdraw(x(2),y(2))
               END IF
            END IF
         END DO
      END DO
   END DO
!
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgconb
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCONF -- fill between two contours
!%void cpgconf(const float *a, int idim, int jdim, int i1, int i2, \
!% int j1, int j2, float c1, float c2, const float *tr);
!+
SUBROUTINE pgconf(a,idim,jdim,i1,i2,j1,j2,c1,c2,tr)
!
! Shade the region between two contour levels of a function defined on
! the nodes of a rectangular grid. The routine uses the current fill
! attributes, hatching style (if appropriate), and color index.
!
! If you want to both shade between contours and draw the contour
! lines, call this routine first (once for each pair of levels) and
! then CALL PGCONT (or PGCONS) to draw the contour lines on top of the
! shading.
!
! Note 1: This routine is not very efficient: it generates a polygon
! fill command for each cell of the mesh that intersects the desired
! area, rather than consolidating adjacent cells into a single polygon.
!
! Note 2: If both contours intersect all four edges of a particular
! mesh cell, the program behaves badly and may consider some parts
! of the cell to lie in more than one contour range.
!
! Note 3: If a contour crosses all four edges of a cell, this
! routine may not generate the same contours as PGCONT or PGCONS
! (these two routines may not agree either). Such cases are always
! ambiguous and the routines use different approaches to resolving
! the ambiguity.
!
! Arguments:
!  A      (input)  : data array.
!  IDIM   (input)  : first dimension of A.
!  JDIM   (input)  : second dimension of A.
!  I1,I2  (input)  : range of first index to be contoured (inclusive).
!  J1,J2  (input)  : range of second index to be contoured (inclusive).
!  C1, C2 (input)  : contour levels; note that C1 must be less than C2.
!  TR     (input)  : array defining a transformation between the I,J
!                    grid of the array and the world coordinates. The
!                    world coordinates of the array point A(I,J) are
!                    given by:
!                      X = TR(1) + TR(2)*I + TR(3)*J
!                      Y = TR(4) + TR(5)*I + TR(6)*J
!                    Usually TR(3) and TR(5) are zero - unless the
!                    coordinate transformation involves a rotation
!                    or shear.
!--
! 03-Oct-1996 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   REAL(KIND=pg), INTENT(IN)                :: a(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: c1
   REAL(KIND=pg), INTENT(IN)                :: c2
   REAL(KIND=pg), INTENT(IN)                :: tr(6)
!
   INTEGER :: i,j,ic,npt,lev
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: dval(5),x(8),y(8),delta,xx,yy,c,r
   INTEGER :: idelt(6)
   DATA idelt/0,-1,-1,0,0,-1/
!
! Check arguments.
!
   IF(pgnoto('PGCONF'))RETURN
   IF(i1 < 1.OR.i2 > idim.OR.i1 >= i2.OR.j1 < 1.OR.j2 > jdim.OR.j1 >= j2)RETURN
   IF(c1 >= c2)RETURN
   CALL pgbbuf
!
   DO  j=j1+1,j2
      DO  i=i1+1,i2
         dval(1)=a(i-1,j)
         dval(2)=a(i-1,j-1)
         dval(3)=a(i,j-1)
         dval(4)=a(i,j)
         dval(5)=dval(1)
!
         npt=0
         loop120: DO  ic=1,4
            IF(dval(ic) >= c1.AND.dval(ic) < c2)THEN
               npt=npt+1
               xx=i+idelt(ic+1)
               yy=j+idelt(ic)
               x(npt)=tr(1)+tr(2)*xx+tr(3)*yy
               y(npt)=tr(4)+tr(5)*xx+tr(6)*yy
            END IF
            r=dval(ic+1)-dval(ic)
            IF(ABS(r) <= EPSILON(r))CYCLE loop120
            DO  lev=1,2
               IF(r > 0.0_pg)THEN
                  c=c1
                  IF(lev == 2)c=c2
               ELSE
                  c=c2
                  IF(lev == 2)c=c1
               END IF
               delta=(c-dval(ic))/r
               IF(delta > 0.0_pg.AND.delta < 1.0_pg)THEN
                  IF(ic == 1.OR.ic == 3)THEN
                     xx=i+idelt(ic+1)
                     yy=REAL(j+idelt(ic),KIND=pg)+delta*REAL(idelt(ic+1)- &
                        idelt(ic),KIND=pg)
                  ELSE
                     xx=REAL(i+idelt(ic+1),KIND=pg)+delta*REAL(idelt(ic+2)- &
                       idelt(ic+1),KIND=pg)
                     yy=j+idelt(ic)
                  END IF
                  npt=npt+1
                  x(npt)=tr(1)+tr(2)*xx+tr(3)*yy
                  y(npt)=tr(4)+tr(5)*xx+tr(6)*yy
               END IF
            END DO
         END DO loop120
         IF(npt >= 3)CALL pgpoly(npt,x,y)
      END DO
   END DO
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgconf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCONL -- label contour map of a 2D data array
!%void cpgconl(const float *a, int idim, int jdim, int i1, int i2, \
!% int j1, int j2, float c, const float *tr, const char *label, \
!% int intval, int minint);
!+
SUBROUTINE pgconl(a,idim,jdim,i1,i2,j1,j2,c,tr,label,intval,minint)
!
! Label a contour map drawn with routine PGCONT. Routine PGCONT should
! be called first to draw the contour lines, then this routine should be
! called to add the labels. Labels are written at intervals along the
! contour lines, centered on the contour lines with lettering aligned
! in the up-hill direction. Labels are opaque, so a part of the under-
! lying contour line is obscured by the label. Labels use the current
! attributes (character height, line width, color index, character
! font).
!
! The first 9 arguments are the same as those supplied to PGCONT, and
! should normally be identical to those used with PGCONT. Note that
! only one contour level can be specified; tolabel more contours, call
! PGCONL for each level.
!
! The Label is supplied as a character string in argument LABEL.
!
! The spacing of labels along the contour is specified by parameters
! INTVAL and MININT. The routine follows the contour through the
! array, counting the number of cells that the contour crosses. The
! first label will be written in the MININT'th cell, and additional
! labels will be written every INTVAL cells thereafter. A contour
! that crosses less than MININT cells will not be labelled. Some
! experimentation may be needed to get satisfactory results; a good
! place to start is INTVAL=20, MININT=10.
!
! Arguments:
!  A      (input) : data array.
!  IDIM   (input) : first dimension of A.
!  JDIM   (input) : second dimension of A.
!  I1, I2 (input) : range of first index to be contoured (inclusive).
!  J1, J2 (input) : range of second index to be contoured (inclusive).
!  C      (input) : the level of the contour to be labelled (one of the
!                   values given to PGCONT).
!  TR     (input) : array defining a transformation between the I,J
!                   grid of the array and the world coordinates.
!                   The world coordinates of the array point A(I,J)
!                   are given by:
!                     X = TR(1) + TR(2)*I + TR(3)*J
!                     Y = TR(4) + TR(5)*I + TR(6)*J
!                   Usually TR(3) and TR(5) are zero - unless the
!                   coordinate transformation involves a rotation or
!                   shear.
!  LABEL  (input) : character strings to be used to label the specified
!                   contour. Leading and trailing blank spaces are
!                   ignored.
!  INTVAL (input) : spacing along the contour between labels, in
!                   grid cells.
!  MININT (input) : contours that cross less than MININT cells
!                   will not be labelled.
!--
!  5-May-1994 - New routine; this routine is virtually identical to
!               PGCONT, but calls PGCONX with a different external
!               routine [TJP].
!  4-Feb-1997 - PGCONX requires an array argument, not scalar [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   REAL(KIND=pg), INTENT(IN)                :: a(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: c
   REAL(KIND=pg), INTENT(IN)                :: tr(6)
   CHARACTER (LEN=*), INTENT(IN)            :: label
   INTEGER, INTENT(IN)                      :: intval
   INTEGER, INTENT(IN)                      :: minint
!
   INTEGER :: i
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: cl(1)
!
!EXTERNAL pgcl
!
   INTERFACE
      SUBROUTINE pgcl(k,x,y,z)
        USE accur
        IMPLICIT NONE
        INTEGER,INTENT(IN)  ::  k
        REAL(KIND=pg), INTENT(IN) :: x,y,z
      END SUBROUTINE
   END INTERFACE
!
   IF(pgnoto('PGCONL'))RETURN
!
! Save TRANS matrix and other parameters.
!
   DO  i=1,6
      trans(i)=tr(i)
   END DO
   pgcint=intval
   pgcmin=minint
   pgclab=label
!
! Use PGCONX with external function PGCL.
!
   cl(1)=c
!
   CALL pgconx(a,idim,jdim,i1,i2,j1,j2,cl,1,pgcl)
!
   RETURN
!
END SUBROUTINE pgconl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCONS -- contour map of a 2D data array (fast algorithm)
!%void cpgcons(const float *a, int idim, int jdim, int i1, int i2, \
!% int j1, int j2, const float *c, int nc, const float *tr);
!+
SUBROUTINE pgcons(a,idim,jdim,i1,i2,j1,j2,c,nc,tr)
!
! Draw a contour map of an array. The map is truncated if
! necessary at the boundaries of the viewport.  Each contour line is
! drawn with the current line attributes (color index, style, and
! width).  This routine, unlike PGCONT, does not draw each contour as a
! continuous line, but draws the straight line segments composing each
! contour in a random order.  It is thus not suitable for use on pen
! plotters, and it usually gives unsatisfactory results with dashed or
! dotted lines.  It is, however, faster than PGCONT, especially if
! several contour levels are drawn with one call of PGCONS.
!
! Arguments:
!  A      (input)  : data array.
!  IDIM   (input)  : first dimension of A.
!  JDIM   (input)  : second dimension of A.
!  I1,I2  (input)  : range of first index to be contoured (inclusive).
!  J1,J2  (input)  : range of second index to be contoured (inclusive).
!  C      (input)  : array of contour levels (in the same units as the
!                    data in array A); dimension at least NC.
!  NC     (input)  : number of contour levels (less than or equal to
!                    dimension of C). The absolute value of this
!                    argument is used (for compatibility with PGCONT,
!                    where the sign of NC is significant).
!  TR     (input)  : array defining a transformation between the I,J
!                    grid of the array and the world coordinates. The
!                    world coordinates of the array point A(I,J) are
!                    given by:
!                      X = TR(1) + TR(2)*I + TR(3)*J
!                      Y = TR(4) + TR(5)*I + TR(6)*J
!                    Usually TR(3) and TR(5) are zero - unless the
!                    coordinate transformation involves a rotation
!                    or shear.
!--
! 27-Aug-1984 - [TJP].
! 21-Sep-1989 - Better treatment of the 'ambiguous' case [A. Tennant];
!               compute world coordinates internally and eliminate
!               dependence on common block [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   REAL(KIND=pg), INTENT(IN)                :: a(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: c(*)
   INTEGER, INTENT(IN)                      :: nc
   REAL(KIND=pg), INTENT(IN)                :: tr(6)
!
   INTEGER :: i,ic,icorn,idelt(6),j,k,npt
   INTEGER :: ioff(8),joff(8),ienc,itmp,jtmp,ilo,itot
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: ctr,delta,dval(5),xx,yy,x(4),y(4)
!
!          INTRINSIC  ABS
!
   DATA idelt/0,-1,-1,0,0,-1/
   DATA ioff/-2,-2,-1,-1,0,0,1,1/
   DATA joff/0,-1,-2,1,-2,1,-1,0/
!
! Check arguments.
!
   IF(pgnoto('PGCONS'))RETURN
   IF(i1 < 1.OR.i2 > idim.OR.i1 >= i2.OR.j1 < 1.OR.j2 > jdim.OR.j1 >= j2)RETURN
   IF(nc == 0)RETURN
   CALL pgbbuf
!
   DO  j=j1+1,j2
      DO  i=i1+1,i2
         dval(1)=a(i-1,j)
         dval(2)=a(i-1,j-1)
         dval(3)=a(i,j-1)
         dval(4)=a(i,j)
         dval(5)=dval(1)
         DO  ic=1,ABS(nc)
            ctr=c(ic)
            npt=0
            DO  icorn=1,4
               IF((dval(icorn) < ctr.AND.dval(icorn+1) < ctr).OR.  &
                  (dval(icorn) >= ctr.AND.dval(icorn+1) >= ctr))CYCLE
               npt=npt+1
               delta=(ctr-dval(icorn))/(dval(icorn+1)-dval(icorn))
               SELECT CASE (icorn)
                CASE (1)
                  GO TO 10
                CASE (2)
                  GO TO 20
                CASE (3)
                  GO TO 10
                CASE (4)
                  GO TO 20
               END SELECT
!
10             xx=i+idelt(icorn+1)
               yy=REAL(j+idelt(icorn),KIND=pg)+delta*REAL(idelt(icorn+1)- &
                  idelt(icorn),KIND=pg)
               GO TO 30
!
20             xx=REAL(i+idelt(icorn+1),KIND=pg)+delta*REAL(idelt(icorn+2)-  &
                  idelt(icorn+1),KIND=pg)
               yy=j+idelt(icorn)
!
30             x(npt)=tr(1)+tr(2)*xx+tr(3)*yy
               y(npt)=tr(4)+tr(5)*xx+tr(6)*yy
!
            END DO
            IF(npt == 2)THEN
!
!             -- Contour crosses two sides of cell. Draw line-segment.
!
               CALL pgmove(x(1),y(1))
               CALL pgdraw(x(2),y(2))
            ELSE IF(npt == 4)THEN
!
!             -- The 'ambiguous' case.  The routine must draw two line
!             segments here and there are two ways to do so.  The
!             following 4 lines would implement the original PGPLOT
!             method:
!            CALL PGCP(0,X(1),Y(1),CTR)
!            CALL PGCP(1,X(2),Y(2),CTR)
!            CALL PGCP(0,X(3),Y(3),CTR)
!            CALL PGCP(1,X(4),Y(4),CTR)
!            -- Choose between \\ and // based on the 8 points just
!            outside the current box.  If half or more of these points
!            lie below the contour level, then draw the lines such that
!            the high corners lie between the lines, otherwise, draw
!            the lines such that the low corners are enclosed.  Care is
!            taken to avoid going off the edge.
!
               itot=0
               ilo=0
               DO  k=1,8
                  itmp=i+ioff(k)
                  jtmp=j+joff(k)
                  IF(itmp < i1.OR.itmp > i2)CYCLE
                  IF(jtmp < j1.OR.jtmp > j2)CYCLE
                  itot=itot+1
                  IF(a(itmp,jtmp) < ctr)ilo=ilo+1
               END DO
               ienc=+1
               IF(ilo < itot/2)ienc=-1
               IF(ienc < 0.AND.dval(1) < ctr.OR.ienc > 0.AND.dval(1)  &
                       >= ctr)THEN
                  CALL pgmove(x(1),y(1))
                  CALL pgdraw(x(2),y(2))
                  CALL pgmove(x(3),y(3))
                  CALL pgdraw(x(4),y(4))
               ELSE
                  CALL pgmove(x(1),y(1))
                  CALL pgdraw(x(4),y(4))
                  CALL pgmove(x(3),y(3))
                  CALL pgdraw(x(2),y(2))
               END IF
            END IF
         END DO
      END DO
   END DO
!
   CALL pgebuf
!
END SUBROUTINE pgcons
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCONT -- contour map of a 2D data array (contour-following)
!%void cpgcont(const float *a, int idim, int jdim, int i1, int i2, \
!% int j1, int j2, const float *c, int nc, const float *tr);
!+
SUBROUTINE pgcont(a,idim,jdim,i1,i2,j1,j2,c,nc,tr)
!
! Draw a contour map of an array.  The map is truncated if
! necessary at the boundaries of the viewport.  Each contour line
! is drawn with the current line attributes (color index, style, and
! width); except that if argument NC is positive (see below), the line
! style is set by PGCONT to 1 (solid) for positive contours or 2
! (dashed) for negative contours.
!
! Arguments:
!  A      (input) : data array.
!  IDIM   (input) : first dimension of A.
!  JDIM   (input) : second dimension of A.
!  I1, I2 (input) : range of first index to be contoured (inclusive).
!  J1, J2 (input) : range of second index to be contoured (inclusive).
!  C      (input) : array of NC contour levels; dimension at least NC.
!  NC     (input) : +/- number of contour levels (less than or equal
!                   to dimension of C). If NC is positive, it is the
!                   number of contour levels, and the line-style is
!                   chosen automatically as described above. If NC is
!                   negative, it is minus the number of contour
!                   levels, and the current setting of line-style is
!                   used for all the contours.
!  TR     (input) : array defining a transformation between the I,J
!                   grid of the array and the world coordinates.
!                   The world coordinates of the array point A(I,J)
!                   are given by:
!                     X = TR(1) + TR(2)*I + TR(3)*J
!                     Y = TR(4) + TR(5)*I + TR(6)*J
!                   Usually TR(3) and TR(5) are zero - unless the
!                   coordinate transformation involves a rotation or
!                   shear.
!--
! (7-Feb-1983)
! (24-Aug-1984) Revised to add the option of not automatically
!       setting the line-style. Sorry about the ugly way this is
!       done (negative NC); this is the least incompatible way of doing
!       it (TJP).
! (21-Sep-1989) Changed to call PGCONX instead of duplicating the code
!       [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: idim
   INTEGER, INTENT(IN)                  :: jdim
   REAL(KIND=pg), INTENT(IN)            :: a(idim,jdim)
   INTEGER, INTENT(IN)                  :: i1
   INTEGER, INTENT(IN)                  :: i2
   INTEGER, INTENT(IN)                  :: j1
   INTEGER, INTENT(IN)                  :: j2
   REAL(KIND=pg), INTENT(IN)            :: c(*)
   INTEGER, INTENT(IN)                  :: nc
   REAL(KIND=pg), INTENT(IN)            :: tr(6)
!
   INTEGER :: i
   LOGICAL :: pgnoto
!
!EXTERNAL pgcp
!
   INTERFACE
      SUBROUTINE pgcp(k,x,y,z)
        USE accur
        IMPLICIT NONE
        INTEGER,INTENT(IN)  ::  k
        REAL(KIND=pg), INTENT(IN) :: x,y,z
      END SUBROUTINE
   END INTERFACE
!
   IF(pgnoto('PGCONT'))RETURN
!
! Save TRANS matrix.
!
   DO  i=1,6
      trans(i)=tr(i)
   END DO
!
! Use PGCONX with external function PGCP, which applies the TRANS
! scaling.
!
   CALL pgconx(a,idim,jdim,i1,i2,j1,j2,c,nc,pgcp)
!
   RETURN
!
END SUBROUTINE pgcont
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCONX -- contour map of a 2D data array (non rectangular)
!+
SUBROUTINE pgconx(a,idim,jdim,i1,i2,j1,j2,c,nc,plot)
!
! Draw a contour map of an array using a user-supplied plotting
! routine.  This routine should be used instead of PGCONT when the
! data are defined on a non-rectangular grid.  PGCONT permits only
! a linear transformation between the (I,J) grid of the array
! and the world coordinate system (x,y), but PGCONX permits any
! transformation to be used, the transformation being defined by a
! user-supplied subroutine. The nature of the contouring algorithm,
! however, dictates that the transformation should maintain the
! rectangular topology of the grid, although grid-points may be
! allowed to coalesce.  As an example of a deformed rectangular
! grid, consider data given on the polar grid theta=0.1n(pi/2),
! for n=0,1,...,10, and r=0.25m, for m=0,1,..,4. This grid
! contains 55 points, of which 11 are coincident at the origin.
! The input array for PGCONX should be dimensioned (11,5), and
! data values should be provided for all 55 elements.  PGCONX can
! also be used for special applications in which the height of the
! contour affects its appearance, e.g., stereoscopic views.
!
! The map is truncated if necessary at the boundaries of the viewport.
! Each contour line is drawn with the current line attributes (color
! index, style, and width); except that if argument NC is positive
! (see below), the line style is set by PGCONX to 1 (solid) for
! positive contours or 2 (dashed) for negative contours. Attributes
! for the contour lines can also be set in the user-supplied
! subroutine, if desired.
!
! Arguments:
!  A      (input) : data array.
!  IDIM   (input) : first dimension of A.
!  JDIM   (input) : second dimension of A.
!  I1, I2 (input) : range of first index to be contoured (inclusive).
!  J1, J2 (input) : range of second index to be contoured (inclusive).
!  C      (input) : array of NC contour levels; dimension at least NC.
!  NC     (input) : +/- number of contour levels (less than or equal
!                   to dimension of C). If NC is positive, it is the
!                   number of contour levels, and the line-style is
!                   chosen automatically as described above. If NC is
!                   negative, it is minus the number of contour
!                   levels, and the current setting of line-style is
!                   used for all the contours.
!  PLOT   (input) : the address (name) of a subroutine supplied by
!                   the user, which will be called by PGCONX to do
!                   the actual plotting. This must be declared
!                   EXTERNAL in the program unit calling PGCONX.
!
! The subroutine PLOT will be called with four arguments:
!      CALL PLOT(VISBLE,X,Y,Z)
! where X,Y (input) are real variables corresponding to
! I,J indices of the array A. If  VISBLE (input, integer) is 1,
! PLOT should draw a visible line from the current pen
! position to the world coordinate point corresponding to (X,Y);
! if it is 0, it should move the pen to (X,Y). Z is the value
! of the current contour level, and may be used by PLOT if desired.
! Example:
!       SUBROUTINE PLOT (VISBLE,X,Y,Z)
!       REAL X, Y, Z, XWORLD, YWORLD
!       INTEGER VISBLE
!       XWORLD = X*COS(Y) ! this is the user-defined
!       YWORLD = X*SIN(Y) ! transformation
!       IF (VISBLE.EQ.0) THEN
!           CALL PGMOVE (XWORLD, YWORLD)
!       ELSE
!           CALL PGDRAW (XWORLD, YWORLD)
!       END IF
!       END
!--
! 14-Nov-1985 - new routine [TJP].
! 12-Sep-1989 - correct documentation error [TJP].
! 22-Apr-1990 - corrected bug in panelling algorithm [TJP].
! 13-Dec-1990 - make errors non-fatal [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   REAL(KIND=pg), INTENT(IN)                :: a(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: c(*)
   INTEGER, INTENT(IN)                      :: nc
!
!   EXTERNAL plot
!
   INTERFACE
      SUBROUTINE plot(K,X,Y,Z)
        USE accur
        IMPLICIT NONE
        INTEGER,INTENT(IN)  ::  k
        REAL(KIND=pg), INTENT(IN) :: x,y,z
      END SUBROUTINE
   END INTERFACE
!
   INTEGER, PARAMETER :: maxemx=100
   INTEGER, PARAMETER :: maxemy=100
   INTEGER :: i
   INTEGER :: nnx,nny,kx,ky,ki,kj,ia,ib,ja,jb,ls,px,py
   LOGICAL :: style,pgnoto
!
! Check arguments.
!
   IF(pgnoto('PGCONX'))RETURN
   IF(i1 < 1.OR.i2 > idim.OR.i1 >= i2.OR.j1 < 1.OR.j2 > jdim.OR.j1 >= j2)THEN
      CALL grwarn('PGCONX: invalid range I1:I2, J1:J2')
      RETURN
   END IF
   IF(nc == 0)RETURN
   style=nc > 0
   CALL pgqls(ls)
   CALL pgbbuf
!
! Divide arrays into panels not exceeding MAXEMX by MAXEMY for
! contouring by PGCNSC.
!
!D    write (*,*) 'PGCONX window:',i1,i2,j1,j2
!
   nnx=i2-i1+1
   nny=j2-j1+1
   kx=MAX(1,(nnx+maxemx-2)/(maxemx-1))
   ky=MAX(1,(nny+maxemy-2)/(maxemy-1))
   px=(nnx+kx-1)/kx
   py=(nny+ky-1)/ky
   DO  ki=1,kx
      ia=i1+(ki-1)*px
      ib=MIN(i2,ia+px)
      DO  kj=1,ky
         ja=j1+(kj-1)*py
         jb=MIN(j2,ja+py)
!
!             Draw the contours in one panel.
!
!D            write (*,*) 'PGCONX panel:',ia,ib,ja,jb
!
         IF(style)CALL pgsls(1)
         DO  i=1,ABS(nc)
            IF(style.AND.(c(i) < 0.0))CALL pgsls(2)
            CALL pgcnsc(a,idim,jdim,ia,ib,ja,jb,c(i),plot)
            IF(style)CALL pgsls(1)
         END DO
      END DO
   END DO
!
   CALL pgsls(ls)
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgconx
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pgcp(k,x,y,z)
!
! PGPLOT (internal routine): Draw one contour segment (for use by
! PGCNSC).
!
! Arguments:
!
! K (input, integer): if K=0, move the pen to (X,Y); if K=1, draw
!       a line from the current position to (X,Y); otherwise
!       do nothing.
! X (input, real): X world-coordinate of end point.
! Y (input, real): Y world-coordinate of end point.
! Z (input, real): the value of the contour level, not used by PGCP at
!       the moment.
!
! (7-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: k
   REAL(KIND=pg), INTENT(IN)                :: x
   REAL(KIND=pg), INTENT(IN)                :: y
   REAL(KIND=pg), INTENT(IN)                :: z
!
   REAL(KIND=pg) :: xx, yy
!
   xx=z
   xx=trans(1)+trans(2)*x+trans(3)*y
   yy=trans(4)+trans(5)*x+trans(6)*y
   IF(k == 1)THEN
      CALL grlina(xx,yy)
   ELSE IF(k == 0)THEN
      CALL grmova(xx,yy)
   END IF
!
   RETURN
!
END SUBROUTINE pgcp
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCTAB -- install the color table to be used by PGIMAG
!%void cpgctab(const float *l, const float *r, const float *g, \
!% const float *b, int nc, float contra, float bright);
!+
SUBROUTINE pgctab(l,r,g,b,nc,contra,bright)
!
! Use the given color table to change the color representations of
! all color indexes marked for use by PGIMAG. To change which
! color indexes are thus marked, call PGSCIR before calling PGCTAB
! or PGIMAG. On devices that can change the color representations
! of previously plotted graphics, PGCTAB will also change the colors
! of existing graphics that were plotted with the marked color
! indexes. This feature can then be combined with PGBAND to
! interactively manipulate the displayed colors of data previously
! plotted with PGIMAG.
!
! Limitations:
!  1. Some devices do not propagate color representation changes
!     to previously drawn graphics.
!  2. Some devices ignore requests to change color representations.
!  3. The appearance of specific color representations on grey-scale
!     devices is device-dependent.
!
! Notes:
!  To reverse the sense of a color table, change the chosen contrast
!  and brightness to -CONTRA and 1-BRIGHT.
!
!  In the following, the term 'color table' refers to the input
!  L,R,G,B arrays, whereas 'color ramp' refers to the resulting
!  ramp of colors that would be seen with PGWEDG.
!
! Arguments:
!  L      (input)  : An array of NC normalized ramp-intensity levels
!                    corresponding to the RGB primary color intensities
!                    in R(),G(),B(). Colors on the ramp are linearly
!                    interpolated from neighbouring levels.
!                    Levels must be sorted in increasing order.
!                     0.0 places a color at the beginning of the ramp.
!                     1.0 places a color at the end of the ramp.
!                    Colors outside these limits are legal, but will
!                    not be visible if CONTRA=1.0 and BRIGHT=0.5.
!  R      (input)  : An array of NC normalized red intensities.
!  G      (input)  : An array of NC normalized green intensities.
!  B      (input)  : An array of NC normalized blue intensities.
!  NC     (input)  : The number of color table entries.
!  CONTRA (input)  : The contrast of the color ramp (normally 1.0).
!                    Negative values reverse the direction of the ramp.
!  BRIGHT (input)  : The brightness of the color ramp. This is normally
!                    0.5, but can sensibly hold any value between 0.0
!                    and 1.0. Values at or beyond the latter two
!                    extremes, saturate the color ramp with the colors
!                    of the respective end of the color table.
!--
!  17-Sep-1994 - New routine [MCS].
!  14-Apr-1997 - Modified to implement a more conventional
!                interpretation of contrast and brightness [MCS].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nc
   REAL(KIND=pg), INTENT(IN)                :: l(nc)
   REAL(KIND=pg), INTENT(IN)                :: r(nc)
   REAL(KIND=pg), INTENT(IN)                :: g(nc)
   REAL(KIND=pg), INTENT(IN)                :: b(nc)
   REAL(KIND=pg), INTENT(IN OUT)            :: contra
   REAL(KIND=pg), INTENT(IN OUT)            :: bright
!
   INTEGER :: minind,maxind,ci
   INTEGER :: ntotal,nspan
   INTEGER :: below,above
   LOGICAL :: forwrd
   REAL(KIND=pg) :: ca,cb,cifrac,span
   REAL(KIND=pg) :: level
   REAL(KIND=pg) :: ldiff,lfrac
   REAL(KIND=pg) :: red,green,blue
!
! Set the minimum absolute contrast - this prevents a divide by zero.
!
   REAL(KIND=pg), PARAMETER :: minctr=1.0_pg/256.0_pg
!
! No colormap entries?
!
   IF(nc == 0)RETURN
!
! Determine the range of color indexes to be used.
!
   CALL pgqcir(minind,maxind)
!
! Count the total number of color indexes to be processed.
!
   ntotal=maxind-minind+1
!
! No definable colors?
!
   IF(ntotal < 1.OR.minind < 0)RETURN
!
! Prevent a divide by zero later by ensuring that CONTRA >= ABS(MINCTR).
!
   IF(ABS(contra) < minctr)THEN
      contra=SIGN(minctr,contra)
   END IF
!
! Convert contrast to the normalized stretch of the
! color table across the available color index range.
!
   span=1.0_pg/ABS(contra)
!
! Translate from brightness and contrast to the normalized color index
! coordinates, CA and CB, at which to place the start and end of the
! color table.
!
   IF(contra >= 0.0_pg)THEN
      ca=1.0_pg-bright*(1.0_pg+span)
      cb=ca+span
   ELSE
      ca=bright*(1.0_pg+span)
      cb=ca-span
   END IF
!
! Determine the number of color indexes spanned by the color table.
!
   nspan=INT(span*ntotal)
!
! Determine the direction in which the color table should be traversed.
!
   forwrd=ca <= cb
!
! Initialize the indexes at which to start searching the color table.
!
! Set the start index for traversing the table from NC to 1.
!
   below=nc
!
! Set the start index for traversing the table from 1 to NC.
!
   above=1
!
! Buffer PGPLOT commands until the color map has been completely
! installed.
!
   CALL pgbbuf
!
! Linearly interpolate the color table RGB values onto each color index.
!
   DO  ci=minind,maxind
!
! Turn the color index into a fraction of the range MININD..MAXIND.
!
      cifrac=REAL(ci-minind)/REAL(maxind-minind)
!
! Determine the color table position that corresponds to color index,
! CI.
!
      IF(nspan > 0)THEN
         level=(cifrac-ca)/(cb-ca)
      ELSE
         IF(cifrac <= ca)THEN
            level=0.0_pg
         ELSE
            level=1.0_pg
         END IF
      END IF
!
! Search for the indexes of the two color table entries that straddle
! LEVEL. The search algorithm assumes that values in L() are
! arranged in increasing order. This allows us to search the color table
! from the point at which the last search left off, rather than having
! to search the whole color table each time.
!
      IF(forwrd)THEN
10       IF(above <= nc.AND.l(above) < level)THEN
            above=above+1
            GO TO 10
         END IF
         below=above-1
      ELSE
20       IF(below >= 1.AND.l(below) > level)THEN
            below=below-1
            GO TO 20
         END IF
         above=below+1
      END IF
!
! If the indexes lie outside the table, substitute the index of the
! nearest edge of the table.
!
      IF(below < 1)THEN
         level=0.0_pg
         below=1
         above=1
      ELSE IF(above > nc)THEN
         level=1.0_pg
         below=nc
         above=nc
      END IF
!
! Linearly interpolate the primary color intensities from color table
! entries, BELOW and ABOVE.
!
      ldiff=l(above)-l(below)
      IF(ldiff > minctr)THEN
         lfrac=(level-l(below))/ldiff
      ELSE
         lfrac=0.0_pg
      END IF
      red=r(below)+(r(above)-r(below))*lfrac
      green=g(below)+(g(above)-g(below))*lfrac
      blue=b(below)+(b(above)-b(below))*lfrac
!
! Intensities are only defined between 0 and 1.
!
      IF(red < 0.0_pg)red=0.0_pg
      IF(red > 1.0_pg)red=1.0_pg
      IF(green < 0.0_pg)green=0.0_pg
      IF(green > 1.0_pg)green=1.0_pg
      IF(blue < 0.0_pg)blue=0.0_pg
      IF(blue > 1.0_pg)blue=1.0_pg
!
! Install the new color representation.
!
      CALL pgscr(ci,red,green,blue)
   END DO
!
! Reveal the changed color map.
!
   CALL pgebuf
   RETURN
!
END SUBROUTINE pgctab
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCURS -- read cursor position
!%int cpgcurs(float *x, float *y, char *ch_scalar);
!+
FUNCTION pgcurs(x,y,ch)
!
! Read the cursor position and a character typed by the user.
! The position is returned in world coordinates.  PGCURS positions
! the cursor at the position specified, allows the user to move the
! cursor using the joystick or arrow keys or whatever is available on
! the device. When he has positioned the cursor, the user types a
! single character on the keyboard; PGCURS then returns this
! character and the new cursor position (in world coordinates).
!
! Returns:
!  PGCURS         : 1 if the call was successful; 0 if the device
!                    has no cursor or some other error occurs.
! Arguments:
!  X      (in/out) : the world x-coordinate of the cursor.
!  Y      (in/out) : the world y-coordinate of the cursor.
!  CH     (output) : the character typed by the user; if the device has
!                    no cursor or if some other error occurs, the value
!                    CHAR(0) [ASCII NUL character] is returned.
!
! Note: The cursor coordinates (X,Y) may be changed by PGCURS even if
! the device has no cursor or if the user does not move the cursor.
! Under these circumstances, the position returned in (X,Y) is that of
! the pixel nearest to the requested position.
!--
!  7-Sep-1994 - changed to use PGBAND [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)            :: x
   REAL(KIND=pg), INTENT(IN OUT)            :: y
   CHARACTER (LEN=*), INTENT(OUT)           :: ch
!
   INTEGER :: pgband,pgcurs
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGCURS'))THEN
      ch=CHAR(0)
      pgcurs=0
   ELSE
      pgcurs=pgband(0,1,0.0_pg,0.0_pg,x,y,ch)
   END IF
!
   RETURN
!
END FUNCTION pgcurs
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGCURSE -- non-standard alias for PGCURS
!+
FUNCTION pgcurse(x,y,ch)
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)            :: x
   REAL(KIND=pg), INTENT(IN OUT)            :: y
   CHARACTER (LEN=1), INTENT(IN OUT)        :: ch
!
! See description of PGCURS.
!--
   INTEGER :: pgcurs,pgcurse
!
   pgcurse=pgcurs(x,y,ch)
!
END FUNCTION pgcurse
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGDRAW -- draw a line from the current pen position to a point
!%void cpgdraw(float x, float y);
!+
SUBROUTINE pgdraw(x,y)
!
! Draw a line from the current pen position to the point
! with world-coordinates (X,Y). The line is clipped at the edge of the
! current window. The new pen position is (X,Y) in world coordinates.
!
! Arguments:
!  X      (input)  : world x-coordinate of the end point of the line.
!  Y      (input)  : world y-coordinate of the end point of the line.
!--
! 27-Nov-1986
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: x
   REAL(KIND=pg), INTENT(IN)                     :: y
!
   CALL pgbbuf
   CALL grlina(x,y)
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgdraw
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGEBUF -- end batch of output (buffer)
!%void cpgebuf(void);
!+
SUBROUTINE pgebuf
!
! A call to PGEBUF marks the end of a batch of graphical output begun
! with the last call of PGBBUF.  PGBBUF and PGEBUF calls should always
! be paired. Each call to PGBBUF increments a counter, while each call
! to PGEBUF decrements the counter. When the counter reaches 0, the
! batch of output is written on the output device.
!
! Arguments: none
!--
! 21-Nov-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   LOGICAL :: pgnoto
!
   IF(.NOT.pgnoto('PGEBUF'))THEN
      pgblev(pgid)=MAX(0,pgblev(pgid)-1)
      IF(pgblev(pgid) == 0)CALL grterm
   END IF
!
   RETURN
!
END SUBROUTINE pgebuf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGEND -- close all open graphics devices
!%void cpgend(void);
!+
SUBROUTINE pgend
!
! Close and release any open graphics devices. All devices must be
! closed by calling either PGCLOS (for each device) or PGEND before
! the program terminates. If a device is not closed properly, some
! or all of the graphical output may be lost.
!
! Arguments: none
!--
! 22-Dec-1995 [TJP] - revised to call PGCLOS for each open device.
! 25-Feb-1997 [TJP] - revised description.
!-----------------------------------------------------------------------
!
   USE pgplot
!
   INTEGER :: i
!
   DO  i=1,pgmaxd
      IF(pgdevs(i) == 1)THEN
         CALL pgslct(i)
         CALL pgclos
      END IF
   END DO
!
   RETURN
!
END SUBROUTINE pgend
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGENV -- set window and viewport and draw labeled frame
!%void cpgenv(float xmin, float xmax, float ymin, float ymax, \
!% int just, int axis);
!+
SUBROUTINE pgenv(xmin,xmax,ymin,ymax,just,axis)
!
! Set PGPLOT "Plotter Environment".  PGENV establishes the scaling
! for subsequent calls to PGPT, PGLINE, etc.  The plotter is
! advanced to a new page or panel, clearing the screen if necessary.
! If the "prompt state" is ON (see PGASK), confirmation
! is requested from the user before clearing the screen.
! If requested, a box, axes, labels, etc. are drawn according to
! the setting of argument AXIS.
!
! Arguments:
!  XMIN   (input)  : the world x-coordinate at the bottom left corner
!                    of the viewport.
!  XMAX   (input)  : the world x-coordinate at the top right corner
!                    of the viewport (note XMAX may be less than XMIN).
!  YMIN   (input)  : the world y-coordinate at the bottom left corner
!                    of the viewport.
!  YMAX   (input)  : the world y-coordinate at the top right corner
!                    of the viewport (note YMAX may be less than YMIN).
!  JUST   (input)  : if JUST=1, the scales of the x and y axes (in
!                    world coordinates per inch) will be equal,
!                    otherwise they will be scaled independently.
!  AXIS   (input)  : controls the plotting of axes, tick marks, etc:
!      AXIS = -2 : draw no box, axes or labels;
!      AXIS = -1 : draw box only;
!      AXIS =  0 : draw box and label it with coordinates;
!      AXIS =  1 : same as AXIS=0, but also draw the
!                  coordinate axes (X=0, Y=0);
!      AXIS =  2 : same as AXIS=1, but also draw grid lines
!                  at major increments of the coordinates;
!      AXIS = 10 : draw box and label X-axis logarithmically;
!      AXIS = 20 : draw box and label Y-axis logarithmically;
!      AXIS = 30 : draw box and label both axes logarithmically.
!
! For other axis options, use routine PGBOX. PGENV can be persuaded to
! call PGBOX with additional axis options by defining an environment
! parameter PGPLOT_ENVOPT containing the required option codes.
! Examples:
!   PGPLOT_ENVOPT=P      ! draw Projecting tick marks
!   PGPLOT_ENVOPT=I      ! Invert the tick marks
!   PGPLOT_ENVOPT=IV     ! Invert tick marks and label y Vertically
!--
!  1-May-1983
! 25-Sep-1985 [TJP] - change to use PGWNAD.
! 23-Nov-1985 [TJP] - add PGPLOT_ENVOPT option.
! 31-Dec-1985 [TJP] - remove automatic PGBEG call.
! 29-Aug-1989 [TJP] - remove common block; no longer needed.
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: xmin
   REAL(KIND=pg), INTENT(IN)                     :: xmax
   REAL(KIND=pg), INTENT(IN)                     :: ymin
   REAL(KIND=pg), INTENT(IN)                     :: ymax
   INTEGER, INTENT(IN)                           :: just
   INTEGER, INTENT(IN)                           :: axis
!
   INTEGER :: l
   LOGICAL :: pgnoto
   CHARACTER (LEN=10) :: xopts,yopts,envopt,temp
!
   IF(pgnoto('PGENV'))RETURN
!
! Start a new picture: move to a new panel or page as necessary.
!
   CALL pgpage
!
! Redefine the standard viewport.
!
   CALL pgvstd
!
! If invalid arguments are specified, issue warning and leave window
! unchanged.
!
   IF(ABS(xmin - xmax) <= EPSILON(xmin))THEN
      CALL grwarn('invalid x limits in PGENV: XMIN = XMAX.')
      RETURN
   ELSE IF(ABS(ymin - ymax) <= EPSILON(ymin))THEN
      CALL grwarn('invalid y limits in PGENV: YMIN = YMAX.')
      RETURN
   END IF
!
! Call PGSWIN to define the window.
! If equal-scales requested, adjust viewport.
!
   IF(just == 1)THEN
      CALL pgwnad(xmin,xmax,ymin,ymax)
   ELSE
      CALL pgswin(xmin,xmax,ymin,ymax)
   END IF
!
! Call PGBOX to draw and label frame around viewport.
!
   yopts='*'
   IF(axis == -2)THEN
      xopts=' '
   ELSE IF(axis == -1)THEN
      xopts='BC'
   ELSE IF(axis == 0)THEN
      xopts='BCNST'
   ELSE IF(axis == 1)THEN
      xopts='ABCNST'
   ELSE IF(axis == 2)THEN
      xopts='ABCGNST'
   ELSE IF(axis == 10)THEN
      xopts='BCNSTL'
      yopts='BCNST'
   ELSE IF(axis == 20)THEN
      xopts='BCNST'
      yopts='BCNSTL'
   ELSE IF(axis == 30)THEN
      xopts='BCNSTL'
      yopts='BCNSTL'
   ELSE
      CALL grwarn('PGENV: illegal AXIS argument.')
      xopts='BCNST'
   END IF
   IF(yopts == '*')yopts=xopts
!
! Additional PGBOX options from PGPLOT_ENVOPT.
!
   CALL grgenv('ENVOPT',envopt,l)
   IF(l > 0.AND.axis >= 0)THEN
      temp=xopts
      xopts=envopt(1:l)//temp
      temp=yopts
      yopts=envopt(1:l)//temp
   END IF
   CALL pgbox(xopts,0.0_pg,0,yopts,0.0_pg,0)
!
   RETURN
!
END SUBROUTINE pgenv
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGERAS -- erase all graphics from current page
!%void cpgeras(void);
!+
SUBROUTINE pgeras
!
! Erase all graphics from the current page (or current panel, if
! the view surface has been divided into panels with PGSUBP).
!
! Arguments: none
!--
! 24-Jun-1994
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER :: ci,fs
   REAL(KIND=pg) :: xv1,xv2,yv1,yv2,xw1,xw2,yw1,yw2
!
   CALL pgbbuf
   CALL pgqci(ci)
   CALL pgqfs(fs)
   CALL pgsci(0)
   CALL pgsfs(1)
   CALL pgqwin(xw1,xw2,yw1,yw2)
   CALL pgqvp(0,xv1,xv2,yv1,yv2)
   CALL pgsvp(0.0_pg,1.0_pg,0.0_pg,1.0_pg)
   CALL pgrect(xw1,xw2,yw1,yw2)
   CALL pgsvp(xv1,xv2,yv1,yv2)
   CALL pgsci(ci)
   CALL pgsfs(fs)
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgeras
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGERR1 -- horizontal or vertical error bar
!%void cpgerr1(int dir, float x, float y, float e, float t);
!+
SUBROUTINE pgerr1(dir,x,y,e,t)
!
! Plot a single error bar in the direction specified by DIR.
! This routine draws an error bar only; to mark the data point at
! the start of the error bar, an additional call to PGPT is required.
! To plot many error bars, use PGERRB.
!
! Arguments:
!  DIR    (input)  : direction to plot the error bar relative to
!                    the data point.
!                    One-sided error bar:
!                      DIR is 1 for +X (X to X+E);
!                             2 for +Y (Y to Y+E);
!                             3 for -X (X to X-E);
!                             4 for -Y (Y to Y-E).
!                    Two-sided error bar:
!                      DIR is 5 for +/-X (X-E to X+E);
!                             6 for +/-Y (Y-E to Y+E).
!  X      (input)  : world x-coordinate of the data.
!  Y      (input)  : world y-coordinate of the data.
!  E      (input)  : value of error bar distance to be added to the
!                    data position in world coordinates.
!  T      (input)  : length of terminals to be drawn at the ends
!                    of the error bar, as a multiple of the default
!                    length; if T = 0.0, no terminals will be drawn.
!--
! 31-Mar-1997 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: dir
   REAL(KIND=pg), INTENT(IN)                :: x
   REAL(KIND=pg), INTENT(IN)                :: y
   REAL(KIND=pg), INTENT(IN)                :: e
   REAL(KIND=pg), INTENT(IN)                :: t
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xtik,ytik,xx,yy
!
   IF(pgnoto('PGERR1'))RETURN
   IF(dir < 1.OR.dir > 6)RETURN
   CALL pgbbuf
!
! Determine terminal length.
!
   CALL pgtikl(t,xtik,ytik)
!
! Draw terminal at starting point if required.
!
   IF(dir == 5)THEN
      xx=x-e
      yy=y
   ELSE IF(dir == 6)THEN
      xx=x
      yy=y-e
   ELSE
      xx=x
      yy=y
   END IF
   IF(ABS(t) > EPSILON(t))THEN
      IF(dir == 5)THEN
         CALL grmova(xx,yy-ytik)
         CALL grlina(xx,yy+ytik)
      ELSE IF(dir == 6)THEN
         CALL grmova(xx-xtik,yy)
         CALL grlina(xx+xtik,yy)
      END IF
   END IF
!
! Draw the error bar itself.
!
   CALL grmova(xx,yy)
   IF(dir == 1.OR.dir == 5)THEN
      xx=x+e
      yy=y
   ELSE IF(dir == 2.OR.dir == 6)THEN
      xx=x
      yy=y+e
   ELSE IF(dir == 3)THEN
      xx=x-e
      yy=y
   ELSE IF(dir == 4)THEN
      xx=x
      yy=y-e
   END IF
   CALL grlina(xx,yy)
!
! Draw terminal at end point.
!
   IF(ABS(t) > EPSILON(t))THEN
      IF(MOD(dir,2) == 1)THEN
         CALL grmova(xx,yy-ytik)
         CALL grlina(xx,yy+ytik)
      ELSE
         CALL grmova(xx-xtik,yy)
         CALL grlina(xx+xtik,yy)
      END IF
   END IF
!
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgerr1
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGERRB -- horizontal or vertical error bar
!%void cpgerrb(int dir, int n, const float *x, const float *y, \
!% const float *e, float t);
!+
SUBROUTINE pgerrb(dir,n,x,y,e,t)
!
! Plot error bars in the direction specified by DIR.
! This routine draws an error bar only; to mark the data point at
! the start of the error bar, an additional call to PGPT is required.
!
! Arguments:
!  DIR    (input)  : direction to plot the error bar relative to
!                    the data point.
!                    One-sided error bar:
!                      DIR is 1 for +X (X to X+E);
!                             2 for +Y (Y to Y+E);
!                             3 for -X (X to X-E);
!                             4 for -Y (Y to Y-E).
!                    Two-sided error bar:
!                      DIR is 5 for +/-X (X-E to X+E);
!                             6 for +/-Y (Y-E to Y+E).
!  N      (input)  : number of error bars to plot.
!  X      (input)  : world x-coordinates of the data.
!  Y      (input)  : world y-coordinates of the data.
!  E      (input)  : value of error bar distance to be added to the
!                    data position in world coordinates.
!  T      (input)  : length of terminals to be drawn at the ends
!                    of the error bar, as a multiple of the default
!                    length; if T = 0.0, no terminals will be drawn.
!
! Note: the dimension of arrays X, Y, and E must be greater
! than or equal to N. If N is 1, X, Y, and E may be scalar
! variables, or expressions.
!--
!  1-Mar-1991 - new routine [JM].
! 20-Apr-1992 - correct bug [ALF, TJP].
! 28-Mar-1995 - add options DIR = 5 or 6 [TJP].
! 31-Mar-1997 - use pgtikl [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   INTEGER, INTENT(IN)                      :: dir
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: x(*)
   REAL(KIND=pg), INTENT(IN)                :: y(*)
   REAL(KIND=pg), INTENT(IN)                :: e(*)
   REAL(KIND=pg), INTENT(IN)                :: t
!
   INTEGER :: i
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xtik,ytik,xx,yy
!
   IF(pgnoto('PGERRB'))RETURN
   IF(n < 1)RETURN
   IF(dir < 1.OR.dir > 6)RETURN
   CALL pgbbuf
!
! Determine terminal length.
!
   CALL pgtikl(t,xtik,ytik)
!
! Loop through points.
!
   DO  i=1,n
!
! Draw terminal at starting point if required.
!
      IF(dir == 5)THEN
         xx=x(i)-e(i)
         yy=y(i)
      ELSE IF(dir == 6)THEN
         xx=x(i)
         yy=y(i)-e(i)
      ELSE
         xx=x(i)
         yy=y(i)
      END IF
      IF(ABS(t) > EPSILON(t))THEN
         IF(dir == 5)THEN
            CALL grmova(xx,yy-ytik)
            CALL grlina(xx,yy+ytik)
         ELSE IF(dir == 6)THEN
            CALL grmova(xx-xtik,yy)
            CALL grlina(xx+xtik,yy)
         END IF
      END IF
!
! Draw the error bar itself.
!
      CALL grmova(xx,yy)
      IF(dir == 1.OR.dir == 5)THEN
         xx=x(i)+e(i)
         yy=y(i)
      ELSE IF(dir == 2.OR.dir == 6)THEN
         xx=x(i)
         yy=y(i)+e(i)
      ELSE IF(dir == 3)THEN
         xx=x(i)-e(i)
         yy=y(i)
      ELSE IF(dir == 4)THEN
         xx=x(i)
         yy=y(i)-e(i)
      END IF
      CALL grlina (xx,yy)
!
! Draw terminal at end point.
!
      IF(ABS(t) > EPSILON(t))THEN
         IF(MOD(dir,2) == 1)THEN
            CALL grmova(xx,yy-ytik)
            CALL grlina(xx,yy+ytik)
         ELSE
            CALL grmova(xx-xtik,yy)
            CALL grlina(xx+xtik,yy)
         END IF
      END IF
!
   END DO
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgerrb
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGERRX -- horizontal error bar
!%void cpgerrx(int n, const float *x1, const float *x2, \
!% const float *y, float t);
!+
SUBROUTINE pgerrx(n,x1,x2,y,t)
!
! Plot horizontal error bars.
! This routine draws an error bar only; to mark the data point in
! the middle of the error bar, an additional call to PGPT or
! PGERRY is required.
!
! Arguments:
!  N      (input)  : number of error bars to plot.
!  X1     (input)  : world x-coordinates of lower end of the
!                    error bars.
!  X2     (input)  : world x-coordinates of upper end of the
!                    error bars.
!  Y      (input)  : world y-coordinates of the data.
!  T      (input)  : length of terminals to be drawn at the ends
!                    of the error bar, as a multiple of the default
!                    length; if T = 0.0, no terminals will be drawn.
!
! Note: the dimension of arrays X1, X2, and Y must be greater
! than or equal to N. If N is 1, X1, X2, and Y may be scalar
! variables, or expressions, eg:
!       CALL PGERRX(1,X-SIGMA,X+SIGMA,Y)
!--
! (6-Oct-1983)
! 31-Mar-1997 - use pgtikl [TJP[.
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                     :: x1(*)
   REAL(KIND=pg), INTENT(IN)                     :: x2(*)
   REAL(KIND=pg), INTENT(IN)                     :: y(*)
   REAL(KIND=pg), INTENT(IN)                     :: t
!
   INTEGER :: i
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xtik,ytik
!
   IF(pgnoto('PGERRX'))RETURN
   IF(n < 1)RETURN
   CALL pgbbuf
!
   CALL pgtikl(t,xtik,ytik)
   DO  i=1,n
      IF(ABS(t) > EPSILON(t))THEN
         CALL grmova(x1(i),y(i)-ytik)
         CALL grlina(x1(i),y(i)+ytik)
      END IF
      CALL grmova(x1(i),y(i))
      CALL grlina(x2(i),y(i))
      IF(ABS(t) > EPSILON(t))THEN
         CALL grmova(x2(i),y(i)-ytik)
         CALL grlina(x2(i),y(i)+ytik)
      END IF
   END DO
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgerrx
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGERRY -- vertical error bar
!%void cpgerry(int n, const float *x, const float *y1, \
!% const float *y2, float t);
!+
SUBROUTINE pgerry(n,x,y1,y2,t)
!
! Plot vertical error bars.
! This routine draws an error bar only; to mark the data point in
! the middle of the error bar, an additional call to PGPT or
! PGERRX is required.
!
! Arguments:
!  N      (input)  : number of error bars to plot.
!  X      (input)  : world x-coordinates of the data.
!  Y1     (input)  : world y-coordinates of top end of the
!                    error bars.
!  Y2     (input)  : world y-coordinates of bottom end of the
!                    error bars.
!  T      (input)  : length of terminals to be drawn at the ends
!                    of the error bar, as a multiple of the default
!                    length; if T = 0.0, no terminals will be drawn.
!
! Note: the dimension of arrays X, Y1, and Y2 must be greater
! than or equal to N. If N is 1, X, Y1, and Y2 may be scalar
! variables or expressions, eg:
!       CALL PGERRY(1,X,Y+SIGMA,Y-SIGMA)
!--
! (6-Oct-1983)
! 31-Mar-1997 - use pgtikl [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: x(*)
   REAL(KIND=pg), INTENT(IN)                :: y1(*)
   REAL(KIND=pg), INTENT(IN)                :: y2(*)
   REAL(KIND=pg), INTENT(IN)                :: t
!
   INTEGER :: i
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xtik,ytik
!
   IF(pgnoto('PGERRY'))RETURN
   IF(n < 1)RETURN
   CALL pgbbuf
!
   CALL pgtikl(t,xtik,ytik)
   DO  i=1,n
      IF(ABS(t) > EPSILON(t))THEN
         CALL grmova(x(i)-xtik,y1(i))
         CALL grlina(x(i)+xtik,y1(i))
      END IF
      CALL grmova(x(i),y1(i))
      CALL grlina(x(i),y2(i))
      IF(ABS(t) > EPSILON(t))THEN
         CALL grmova(x(i)-xtik,y2(i))
         CALL grlina(x(i)+xtik,y2(i))
      END IF
   END DO
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgerry
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGETXT -- erase text from graphics display
!%void cpgetxt(void);
!+
SUBROUTINE pgetxt
!
! Some graphics terminals display text (the normal interactive dialog)
! on the same screen as graphics. This routine erases the text from the
! view surface without affecting the graphics. It does nothing on
! devices which do not display text on the graphics screen, and on
! devices which do not have this capability.
!
! Arguments:
!  None
!--
! 18-Feb-1988
!-----------------------------------------------------------------------
!
   CALL gretxt
!
END SUBROUTINE pgetxt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGFUNT -- function defined by X = F(T), Y = G(T)
!+
SUBROUTINE pgfunt(fx,fy,n,tmin,tmax,pgflag)
!
! Draw a curve defined by parametric equations X = FX(T), Y = FY(T).
!
! Arguments:
!  FX     (external real function): supplied by the user, evaluates
!                    X-coordinate.
!  FY     (external real function): supplied by the user, evaluates
!                    Y-coordinate.
!  N      (input)  : the number of points required to define the
!                    curve. The functions FX and FY will each be
!                    called N+1 times.
!  TMIN   (input)  : the minimum value for the parameter T.
!  TMAX   (input)  : the maximum value for the parameter T.
!  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
!                    current window and viewport; if PGFLAG = 0,
!                    PGENV is called automatically by PGFUNT to
!                    start a new plot with automatic scaling.
!
! Note: The functions FX and FY must be declared EXTERNAL in the
! Fortran program unit that calls PGFUNT.
!--
!  5-Oct-1983
! 11-May-1990 - remove unnecessary include [TJP].
! 13-Dec-1990 - make errors non-fatal [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
!REAL(KIND=pg), INTENT(IN OUT)            :: fx
!REAL(KIND=pg), INTENT(IN OUT)            :: fy
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: tmin
   REAL(KIND=pg), INTENT(IN)                :: tmax
   INTEGER, INTENT(IN)                     :: pgflag
!
   REAL(KIND=pg) :: fx,fy
   EXTERNAL fx,fy
!
   INTEGER, PARAMETER :: maxp=1000
   INTEGER :: i
   REAL(KIND=pg) :: x(0:maxp),y(0:maxp),dt
   REAL(KIND=pg) :: xmin,xmax,ymin,ymax
!
   IF(n < 1.OR.n > maxp)THEN
      CALL grwarn('PGFUNT: invalid arguments')
      RETURN
   END IF
   CALL pgbbuf
!
! Evaluate function.
!
   dt=(tmax-tmin)/n
   x(0)=fx(tmin)
   y(0)=fy(tmin)
   xmin=x(0)
   xmax=x(0)
   ymin=y(0)
   ymax=y(0)
   DO  i=1,n
      x(i)=fx(tmin+dt*i)
      y(i)=fy(tmin+dt*i)
      xmin=MIN(xmin,x(i))
      xmax=MAX(xmax,x(i))
      ymin=MIN(ymin,y(i))
      ymax=MAX(ymax,y(i))
   END DO
   dt=0.05_pg*(xmax-xmin)
   IF(ABS(dt) <= EPSILON(dt))THEN
      xmin=xmin-1.0_pg
      xmax=xmax+1.0_pg
   ELSE
      xmin=xmin-dt
      xmax=xmax+dt
   END IF
   dt=0.05_pg*(ymax-ymin)
   IF(ABS(dt) <= EPSILON(dt))THEN
      ymin=ymin-1.0_pg
      ymax=ymax+1.0_pg
   ELSE
      ymin=ymin-dt
      ymax=ymax+dt
   END IF
!
! Define environment if necessary.
!
   IF(pgflag == 0)CALL pgenv(xmin,xmax,ymin,ymax,0,0)
!
! Draw curve.
!
   CALL pgmove(x(0),y(0))
   DO  i=1,n
      CALL pgdraw(x(i),y(i))
   END DO
!
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgfunt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGFUNX -- function defined by Y = F(X)
!+
SUBROUTINE pgfunx(fy,n,xmin,xmax,pgflag)
!
! Draw a curve defined by the equation Y = FY(X), where FY is a
! user-supplied subroutine.
!
! Arguments:
!  FY     (external real function): supplied by the user, evaluates
!                    Y value at a given X-coordinate.
!  N      (input)  : the number of points required to define the
!                    curve. The function FY will be called N+1 times.
!                    If PGFLAG=0 and N is greater than 1000, 1000
!                    will be used instead.  If N is less than 1,
!                    nothing will be drawn.
!  XMIN   (input)  : the minimum value of X.
!  XMAX   (input)  : the maximum value of X.
!  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
!                    current window and viewport; if PGFLAG = 0,
!                    PGENV is called automatically by PGFUNX to
!                    start a new plot with X limits (XMIN, XMAX)
!                    and automatic scaling in Y.
!
! Note: The function FY must be declared EXTERNAL in the Fortran
! program unit that calls PGFUNX.  It has one argument, the
! x-coordinate at which the y value is required, e.g.
!   REAL FUNCTION FY(X)
!   REAL X
!   FY = .....
!   END
!--
!  6-Oct-1983 - TJP.
!  6-May-1985 - fix Y(0) bug - TJP.
! 11-May-1990 - remove unnecessary include - TJP.
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg)                            :: fy
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: xmin
   REAL(KIND=pg), INTENT(IN)                :: xmax
   INTEGER, INTENT(IN)                      :: pgflag
!
   EXTERNAL fy
!
   INTEGER, PARAMETER :: maxp=1000
   INTEGER :: i,nn
   REAL(KIND=pg) :: y(0:maxp),dt,dy
   REAL(KIND=pg) :: ymin,ymax
!
! Check N > 1, and find parameter increment.
!
   IF(n < 1)RETURN
   dt=(xmax-xmin)/REAL(n,KIND=pg)
   CALL pgbbuf
!
! Case 1: we do not have to find limits.
!
   IF(pgflag /= 0)THEN
      CALL pgmove(xmin,fy(xmin))
      DO  i=1,n
         CALL pgdraw(xmin+i*dt,fy(xmin+REAL(i,KIND=pg)*dt))
      END DO
!
! Case 2: find limits and scale plot; function values must be stored
! in an array.
!
   ELSE
      nn=MIN(n,maxp)
      y(0)=fy(xmin)
      ymin=y(0)
      ymax=y(0)
      DO  i=1,nn
         y(i)=fy(xmin+dt*REAL(i,KIND=pg))
         ymin=MIN(ymin,y(i))
         ymax=MAX(ymax,y(i))
      END DO
      dy=0.05_pg*(ymax-ymin)
      IF(ABS(dy) <= EPSILON(dy))THEN
         ymin=ymin-1.0_pg
         ymax=ymax+1.0_pg
      ELSE
         ymin=ymin-dy
         ymax=ymax+dy
      END IF
      CALL pgenv(xmin,xmax,ymin,ymax,0,0)
      CALL pgmove(xmin,y(0))
      DO  i=1,nn
         CALL pgdraw(xmin+dt*REAL(i,KIND=pg),y(i))
      END DO
   END IF
!
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgfunx
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGFUNY -- function defined by X = F(Y)
!+
SUBROUTINE pgfuny(fx,n,ymin,ymax,pgflag)
!
! Draw a curve defined by the equation X = FX(Y), where FY is a
! user-supplied subroutine.
!
! Arguments:
!  FX     (external real function): supplied by the user, evaluates
!                    X value at a given Y-coordinate.
!  N      (input)  : the number of points required to define the
!                    curve. The function FX will be called N+1 times.
!                    If PGFLAG=0 and N is greater than 1000, 1000
!                    will be used instead.  If N is less than 1,
!                    nothing will be drawn.
!  YMIN   (input)  : the minimum value of Y.
!  YMAX   (input)  : the maximum value of Y.
!  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
!                    current window and viewport; if PGFLAG = 0,
!                    PGENV is called automatically by PGFUNY to
!                    start a new plot with Y limits (YMIN, YMAX)
!                    and automatic scaling in X.
!
! Note: The function FX must be declared EXTERNAL in the Fortran
! program unit that calls PGFUNY.  It has one argument, the
! y-coordinate at which the x value is required, e.g.
!   REAL FUNCTION FX(Y)
!   REAL Y
!   FX = .....
!   END
!--
!  5-Oct-1983
! 11-May-1990 - remove unnecessary include [TJP].
! 13-DEc-1990 - make errors non-fatal [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg)                            :: fx
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: ymin
   REAL(KIND=pg), INTENT(IN OUT)            :: ymax
   INTEGER, INTENT(IN)                      :: pgflag
!
   EXTERNAL fx
!
   INTEGER, PARAMETER :: maxp=1000
   INTEGER :: i
   REAL(KIND=pg) :: x(0:maxp),y(0:maxp),dt
   REAL(KIND=pg) :: xmin,xmax
!
   IF(n < 1.OR.n > maxp)THEN
      CALL grwarn('PGFUNY: invalid arguments')
      RETURN
   END IF
   CALL pgbbuf
!
! Evaluate function.
!
   dt=(ymax-ymin)/REAL(n,KIND=pg)
   x(0)=fx(ymin)
   y(0)=ymin
   xmin=x(0)
   xmax=x(0)
   DO  i=1,n
      x(i)=fx(ymin+dt*REAL(i,KIND=pg))
      y(i)=ymin+dt*REAL(i,KIND=pg)
      xmin=MIN(xmin,x(i))
      xmax=MAX(xmax,x(i))
   END DO
   dt=0.05_pg*(xmax-xmin)
   IF(ABS(dt) <= EPSILON(dt))THEN
      xmin=xmin-1.0_pg
      xmax=xmax+1.0_pg
   ELSE
      xmin=xmin-dt
      xmax=xmax+dt
   END IF
!
! Define environment if necessary.
!
   IF(pgflag == 0)CALL pgenv(xmin,xmax,ymin,ymax,0,0)
!
! Draw curve.
!
   CALL pgmove(x(0),y(0))
   DO  i=1,n
      CALL pgdraw(x(i),y(i))
   END DO
!
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgfuny
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGGRAY -- gray-scale map of a 2D data array
!%void cpggray(const float *a, int idim, int jdim, int i1, int i2, \
!% int j1, int j2, float fg, float bg, const float *tr);
!+
SUBROUTINE pggray(a,idim,jdim,i1,i2,j1,j2,fg,bg,tr)
!
! Draw gray-scale map of an array in current window. The subsection
! of the array A defined by indices (I1:I2, J1:J2) is mapped onto
! the view surface world-coordinate system by the transformation
! matrix TR. The resulting quadrilateral region is clipped at the edge
! of the window and shaded with the shade at each point determined
! by the corresponding array value.  The shade is a number in the
! range 0 to 1 obtained by linear interpolation between the background
! level (BG) and the foreground level (FG), i.e.,
!
!   shade = [A(i,j) - BG] / [FG - BG]
!
! The background level BG can be either less than or greater than the
! foreground level FG.  Points in the array that are outside the range
! BG to FG are assigned shade 0 or 1 as appropriate.
!
! PGGRAY uses two different algorithms, depending how many color
! indices are available in the color index range specified for images.
! (This range is set with routine PGSCIR, and the current or default
! range can be queried by calling routine PGQCIR).
!
! If 16 or more color indices are available, PGGRAY first assigns
! color representations to these color indices to give a linear ramp
! between the background color (color index 0) and the foreground color
! (color index 1), and then calls PGIMAG to draw the image using these
! color indices. In this mode, the shaded region is "opaque": every
! pixel is assigned a color.
!
! If less than 16 color indices are available, PGGRAY uses only
! color index 1, and uses  a "dithering" algorithm to fill in pixels,
! with the shade (computed as above) determining the faction of pixels
! that are filled. In this mode the shaded region is "transparent" and
! allows previously-drawn graphics to show through.
!
! The transformation matrix TR is used to calculate the world
! coordinates of the center of the "cell" that represents each
! array element. The world coordinates of the center of the cell
! corresponding to array element A(I,J) are given by:
!
!          X = TR(1) + TR(2)*I + TR(3)*J
!          Y = TR(4) + TR(5)*I + TR(6)*J
!
! Usually TR(3) and TR(5) are zero -- unless the coordinate
! transformation involves a rotation or shear.  The corners of the
! quadrilateral region that is shaded by PGGRAY are given by
! applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
!
! Arguments:
!  A      (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  FG     (input)  : the array value which is to appear with the
!                    foreground color (corresponding to color index 1).
!  BG     (input)  : the array value which is to appear with the
!                    background color (corresponding to color index 0).
!  TR     (input)  : transformation matrix between array grid and
!                    world coordinates.
!--
!  2-Sep-1987: remove device-dependent code to routine GRGRAY (TJP).
!  7-Jun-1988: change documentation and argument names (TJP).
! 31-May-1989: allow 1-pixel wide arrays to be plotted (TJP).
! 17-Mar-1994: pass PG scaling info to lower routines (TJP).
! 15-Sep-1994: use PGITF attribute (TJP).
!  8-Feb-1995: use color ramp based on current foreground and background
!              colors (TJP).
!  6-May-1996: allow multiple devives (TJP).
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: idim
   INTEGER, INTENT(IN)                  :: jdim
   REAL(KIND=pg), INTENT(IN)            :: a(idim,jdim)
   INTEGER, INTENT(IN)                  :: i1
   INTEGER, INTENT(IN)                  :: i2
   INTEGER, INTENT(IN)                  :: j1
   INTEGER, INTENT(IN)                  :: j2
   REAL(KIND=pg), INTENT(IN)            :: fg
   REAL(KIND=pg), INTENT(IN)            :: bg
   REAL(KIND=pg), INTENT(IN)            :: tr(6)
!
   REAL(KIND=pg) :: pa(6)
   LOGICAL :: pgnoto
!
! Check inputs.
!
   IF(pgnoto('PGGRAY'))RETURN
   IF(i1 < 1.OR.i2 > idim.OR.i1 > i2.OR.j1 < 1.OR.j2 > jdim.OR.j1 > j2)THEN
      CALL grwarn('PGGRAY: invalid range I1:I2, J1:J2')
   ELSE IF(ABS(fg - bg) <= EPSILON(fg))THEN
      CALL grwarn('PGGRAY: foreground level = background level')
   ELSE
!
! Call lower-level routine to do the work.
!
      CALL pgbbuf
      CALL pgsave
      CALL pgsci(1)
      pa(1)=tr(1)*pgxscl(pgid)+pgxorg(pgid)
      pa(2)=tr(2)*pgxscl(pgid)
      pa(3)=tr(3)*pgxscl(pgid)
      pa(4)=tr(4)*pgyscl(pgid)+pgyorg(pgid)
      pa(5)=tr(5)*pgyscl(pgid)
      pa(6)=tr(6)*pgyscl(pgid)
      CALL grgray(a,idim,jdim,i1,i2,j1,j2,fg,bg,pa,pgmnci(pgid),  &
         pgmxci(pgid),pgitf(pgid))
      CALL pgebuf
      CALL pgunsa
   END IF
!
   RETURN
!
END SUBROUTINE pggray
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGHI2D -- cross-sections through a 2D data array
!%void cpghi2d(const float *data, int nxv, int nyv, int ix1, \
!% int ix2, int iy1, int iy2, const float *x, int ioff, float bias, \
!% Logical center, float *ylims);
!+
SUBROUTINE pghi2d(dat,nxv,nyv,ix1,ix2,iy1,iy2,x,ioff,bias,center,ylims)
!
! Plot a series of cross-sections through a 2D data array.
! Each cross-section is plotted as a hidden line histogram.  The plot
! can be slanted to give a pseudo-3D effect - if this is done, the
! call to PGENV may have to be changed to allow for the increased X
! range that will be needed.
!
! Arguments:
!  DAT   (input)  : the data array to be plotted.
!  NXV    (input)  : the first dimension of DAT.
!  NYV    (input)  : the second dimension of DAT.
!  IX1    (input)
!  IX2    (input)
!  IY1    (input)
!  IY2    (input)  : PGHI2D plots a subset of the input array DAT.
!                    This subset is delimited in the first (x)
!                    dimension by IX1 and IX2 and the 2nd (y) by IY1
!                    and IY2, inclusively. Note: IY2 < IY1 is
!                    permitted, resulting in a plot with the
!                    cross-sections plotted in reverse Y order.
!                    However, IX2 must be => IX1.
!  X      (input)  : the abscissae of the bins to be plotted. That is,
!                    X(1) should be the X value for DAT(IX1,IY1), and
!                    X should have (IX2-IX1+1) elements.  The program
!                    has to assume that the X value for DAT(x,y) is
!                    the same for all y.
!  IOFF   (input)  : an offset in array elements applied to successive
!                    cross-sections to produce a slanted effect.  A
!                    plot with IOFF > 0 slants to the right, one with
!                    IOFF < 0 slants left.
!  BIAS   (input)  : a bias value applied to each successive cross-
!                    section in order to raise it above the previous
!                    cross-section.  This is in the same units as the
!                    data.
!  CENTER (input)  : if .true., the X values denote the center of the
!                    bins; if .false. the X values denote the lower
!                    edges (in X) of the bins.
!  YLIMS  (input)  : workspace.  Should be an array of at least
!                    (IX2-IX1+1) elements.
!--
! 21-Feb-1984 - Keith Shortridge.
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nxv
   INTEGER, INTENT(IN)                      :: nyv
   REAL(KIND=pg), INTENT(IN)                :: dat(nxv,nyv)
   INTEGER, INTENT(IN)                      :: ix1
   INTEGER, INTENT(IN)                      :: ix2
   INTEGER, INTENT(IN)                      :: iy1
   INTEGER, INTENT(IN)                      :: iy2
   REAL(KIND=pg), INTENT(IN)                :: x(ix2-ix1+1)
   INTEGER, INTENT(IN)                      :: ioff
   REAL(KIND=pg), INTENT(IN)                :: bias
   LOGICAL, INTENT(IN)                      :: center
   REAL(KIND=pg), INTENT(IN OUT)            :: ylims(ix2-ix1+1)
!
   LOGICAL :: first,pendow,hplot,vplot
   INTEGER :: iy,inc,ix,nelmx,ixpt,noff
   REAL(KIND=pg) :: cbias,ynwas,xnwas,yn,xn,vto,vfrom,ylimws,ylim
   REAL(KIND=pg) :: pghis1
   LOGICAL :: pgnoto
!
! Check arguments.
!
   IF(ix1 > ix2)RETURN
   IF(pgnoto('PGHI2D'))RETURN
   CALL pgbbuf
!
! Check Y order.
!
   IF(iy1 > iy2)THEN
      inc=-1
   ELSE
      inc=1
   END IF
!
! Clear limits array.
!
   nelmx=ix2-ix1+1
   DO  ix=1,nelmx
      ylims(ix)=pgyblc(pgid)
   END DO
!
! Loop through Y values.
!
   noff=0
   cbias=0.0_pg
   DO  iy=iy1,iy2,inc
      ynwas=cbias
      ylimws=ynwas
      xnwas=pghis1(x,nelmx,center,1+noff)
      pendow=.false.
      first=.true.
      ixpt=1
!
! Draw histogram for this Y value.
!
      DO  ix=ix1,ix2
         yn=dat(ix,iy)+cbias
         xn=pghis1(x,nelmx,center,ixpt+noff+1)
         ylim=ylims(ixpt)
!
! Given X and Y old and new values, and limits, see which parts of the
! lines are to be drawn.
!
         IF(yn > ylim)THEN
            ylims(ixpt)=yn
            hplot=.true.
            vplot=.true.
            vto=yn
            vfrom=ylim
            IF(ynwas > ylimws)vfrom=ynwas
         ELSE
            hplot=.false.
            IF(ynwas > ylimws)THEN
               vplot=.true.
               vfrom=ynwas
               vto=ylim
            ELSE
               vplot=.false.
            END IF
         END IF
!
! Plot the bin.
!
         IF(vplot)THEN
            IF(.NOT.pendow)THEN
               IF(first)THEN
                  CALL grmova(xnwas,MAX(vto,cbias))
                  first=.false.
               ELSE
                  CALL grmova(xnwas,vfrom)
               END IF
            END IF
            CALL grlina(xnwas,vto)
            IF(hplot)THEN
               CALL grlina(xn,yn)
            END IF
         END IF
         pendow=hplot
         ylimws=ylim
         ynwas=yn
         xnwas=xn
         ixpt=ixpt+1
      END DO
      IF(pendow)CALL grlina(xn,MAX(ylim,cbias))
!
! If any offset in operation, shift limits array to compensate for it.
!
      IF(ioff > 0)THEN
         DO  ix=1,nelmx-ioff
            ylims(ix)=ylims(ix+ioff)
         END DO
         DO  ix=nelmx-ioff+1,nelmx
            ylims(ix)=pgyblc(pgid)
         END DO
      ELSE IF(ioff < 0)THEN
         DO  ix=nelmx,1-ioff,-1
            ylims(ix)=ylims(ix+ioff)
         END DO
         DO  ix=1,-ioff
            ylims(ix)=pgyblc(pgid)
         END DO
      END IF
      cbias=cbias+bias
      noff=noff+ioff
   END DO
!
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pghi2d
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
FUNCTION pghis1(x,nelmx,center,ixv)
!
! PGPLOT Internal routine used by PGHI2D.  Calculates the X-value for
! the left hand edge of a given element of the array being plotted.
!
! Arguments -
!
! X (input, real array): abscissae of bins
! NELMX (input, integer): number of bins
! CENTER (Input, logical): if .true., X values denote the center of
!       the bin; if .false., the X values denote the lower edge (in X)
!       of the bin.
! IXV (input, integer): the bin number in question.  Note IXV may be
!       outside the range 1..NELMX, in which case an interpolated
!       value is returned.
!
! 21-Feb-1984 - Keith Shortridge.
!  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nelmx
   REAL(KIND=pg), INTENT(IN)                :: x(nelmx)
   LOGICAL, INTENT(IN)                      :: center
   INTEGER, INTENT(IN)                      :: ixv
!
!
   REAL(KIND=pg) :: xn,pghis1
!
!INTRINSIC  REAL
!
   IF(center)THEN
      IF((ixv > 1).AND.(ixv <= nelmx))THEN
         xn=(x(ixv-1)+x(ixv))*0.5_pg
      ELSE IF(ixv <= 1)THEN
         xn=x(1)-0.5_pg*(x(2)-x(1))*REAL(3-2*ixv,KIND=pg)
      ELSE IF(ixv > nelmx)THEN
         xn=x(nelmx)+0.5_pg*(x(nelmx)-x(nelmx-1))*REAL((ixv-nelmx)*2-1,KIND=pg)
      END IF
   ELSE
      IF((ixv >= 1).AND.(ixv <= nelmx))THEN
         xn=x(ixv)
      ELSE IF(ixv < 1)THEN
         xn=x(1)-(x(2)-x(1))*REAL(1-ixv,KIND=pg)
      ELSE IF(ixv > nelmx)THEN
         xn=x(nelmx)+(x(nelmx)-x(nelmx-1))*REAL(ixv-nelmx,KIND=pg)
      END IF
   END IF
!
   pghis1=xn
!
   RETURN
!
END FUNCTION pghis1
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGHIST -- histogram of unbinned data
!%void cpghist(int n, const float *data, float datmin, float datmax, \
!% int nbin, int pgflag);
!+
SUBROUTINE pghist(n,dat,datmin,datmax,nbin,pgflag)
!
! Draw a histogram of N values of a variable in array
! DAT(1...N) in the range DATMIN to DATMAX using NBIN bins.  Note
! that array elements which fall exactly on the boundary between
! two bins will be counted in the higher bin rather than the
! lower one; and array elements whose value is less than DATMIN or
! greater than or equal to DATMAX will not be counted at all.
!
! Arguments:
!  N      (input)  : the number of data values.
!  DAT   (input)  : the data values. Note: the dimension of array
!                    DAT must be greater than or equal to N. The
!                    first N elements of the array are used.
!  DATMIN (input)  : the minimum data value for the histogram.
!  DATMAX (input)  : the maximum data value for the histogram.
!  NBIN   (input)  : the number of bins to use: the range DATMIN to
!                    DATMAX is divided into NBIN equal bins and
!                    the number of DAT values in each bin is
!                    determined by PGHIST.  NBIN may not exceed 200.
!  PGFLAG (input)  : if PGFLAG = 1, the histogram is plotted in the
!                    current window and viewport; if PGFLAG = 0,
!                    PGENV is called automatically by PGHIST to start
!                    a new plot (the x-limits of the window will be
!                    DATMIN and DATMAX; the y-limits will be chosen
!                    automatically.
!                    IF PGFLAG = 2,3 the histogram will be in the same
!                    window and viewport but with a filled area style.
!                    If pgflag=4,5 as for pgflag = 0,1, but simple
!                    line drawn as for PGBIN
!
!--
! Side effects:
!
! The pen position is changed to (DATMAX,0.0) in world coordinates.
!--
!  6-Sep-83:
! 11-Feb-92: fill options added.
!-----------------------------------------------------------------------
   USE accur
!
   INTEGER, INTENT(IN)                  :: n
   REAL(KIND=pg), INTENT(IN)            :: dat(*)
   REAL(KIND=pg), INTENT(IN)            :: datmin
   REAL(KIND=pg), INTENT(IN)            :: datmax
   INTEGER, INTENT(IN)                  :: nbin
   INTEGER, INTENT(IN)                  :: pgflag
!
   INTEGER, PARAMETER :: maxbin=200
   INTEGER :: i,ibin,num(maxbin),nummax,junk
   REAL(KIND=pg) :: binsiz,pgrnd
   REAL(KIND=pg) :: cur,prev,xlo,xhi,ylo,yhi
   LOGICAL :: pgnoto
!
   IF(n < 1.OR.datmax <= datmin.OR.nbin < 1.OR.nbin > maxbin) THEN
      CALL grwarn('PGHIST: invalid arguments')
      RETURN
   END IF
   IF(pgnoto('PGHIST'))RETURN
   CALL pgbbuf
!
! How many values in each bin?
!
   DO  ibin=1,nbin
      num(ibin)=0
   END DO
   DO  i=1,n
      ibin=INT((dat(i)-datmin)/(datmax-datmin)*REAL(nbin,KIND=pg))+1
      IF(ibin >= 1.AND.ibin <= nbin)num(ibin)=num(ibin)+1
   END DO
   nummax=0
   DO  ibin=1,nbin
      nummax=MAX(nummax,num(ibin))
   END DO
   binsiz=(datmax-datmin)/REAL(nbin,KIND=pg)
!
! Boundaries of plot.
!
   xlo=datmin
   xhi=datmax
   ylo=0.0_pg
   yhi=pgrnd(1.01_pg*nummax,junk)
!
! Define environment if necessary.
!
   IF(MOD(pgflag,2) == 0)THEN
      CALL pgenv(xlo,xhi,ylo,yhi,0,0)
   END IF
!
! Draw Histogram.
!
   IF(pgflag/2 == 0)THEN
      prev=0.0_pg
      xhi=datmin
      CALL grmova(datmin,0.0_pg)
      DO  ibin=1,nbin
         cur=num(ibin)
         xlo=xhi
         xhi=datmin+REAL(ibin,KIND=pg)*binsiz
         IF(ABS(cur) <= EPSILON(cur))THEN
            CONTINUE
         ELSE IF(cur <= prev)THEN
            CALL grmova(xlo,cur)
            CALL grlina(xhi,cur)
         ELSE
            CALL grmova(xlo,prev)
            CALL grlina(xlo,cur)
            CALL grlina(xhi,cur)
         END IF
         CALL grlina(xhi,0.0_pg)
         prev=cur
      END DO
   ELSE IF(pgflag/2 == 1)THEN
      prev=0.0_pg
      xhi=datmin
      DO  ibin=1,nbin
         cur=num(ibin)
         xlo=xhi
         xhi=datmin+REAL(ibin,KIND=pg)*binsiz
         IF(ABS(cur) <= EPSILON(cur))THEN
           CYCLE
         ELSE
            CALL pgrect(xlo,xhi,0.0_pg,cur)
         END IF
      END DO
   ELSE IF(pgflag/2 == 2)THEN
      prev=0.0_pg
      CALL grmova(datmin,0.0_pg)
      xhi=datmin
      DO  ibin=1,nbin
         cur=num(ibin)
         xlo=xhi
         xhi=datmin+REAL(ibin,KIND=pg)*binsiz
         IF((ABS(cur) <= EPSILON(cur)).AND.(ABS(prev) <= EPSILON(prev)))THEN
            CALL grmova(xhi,0.0_pg)
         ELSE
            CALL grlina(xlo,cur)
            IF(ABS(cur) > EPSILON(cur))THEN
               CALL grlina(xhi,cur)
            ELSE
               CALL grmova(xhi,cur)
            END IF
         END IF
         prev=cur
      END DO
   END IF
!
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pghist
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!.PGHTCH -- hatch a polygonal area (internal routine)
!.
SUBROUTINE pghtch(n,x,y,da)
!
! Hatch a polygonal area using equi-spaced parallel lines. The lines
! are drawn using the current line attributes: line style, line width,
! and color index. Cross-hatching can be achieved by calling this
! routine twice.
!
! Limitations: the hatching will not be done correctly if the
! polygon is so complex that a hatch line intersects more than
! 32 of its sides.
!
! Arguments:
!  N      (input)  : the number of vertices of the polygonal.
!  X,Y    (input)  : the (x,y) world-coordinates of the vertices
!                    (in order).
!  DA      (input) : 0.0 for normal hatching, 90.0 for perpendicular
!                    hatching.
!--
! Reference: I.O. Angel and G. Griffith "High-resolution computer
! graphics using Fortran 77", Halsted Press, 1987.
!
! 18-Feb-1995 [TJP].
!-----------------------------------------------------------------------
!
! MAXP is the maximum number of intersections any hatch line may make
! with the sides of the polygon.
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN)                :: x(*)
   REAL(KIND=pg), INTENT(IN)                :: y(*)
   REAL(KIND=pg), INTENT(IN)                :: da
!
   INTEGER, PARAMETER :: maxp=32
   INTEGER :: np(maxp),i,j,ii,jj,nmin,nmax,nx,ni,nnp
   REAL(KIND=pg) :: angle,sepn,phase
   REAL(KIND=pg) :: rmu(maxp),dx,dy,c,cmid,cmin,cmax,sx,sy,ex,ey,delta
   REAL(KIND=pg) :: qx,qy,r,rmu1,rmu2,xi,yi,bx,by
   REAL(KIND=pg) :: dh,xs1,xs2,ys1,ys2,xl,xr,yt,yb,dindx,dindy
!
! Check arguments.
!
   IF(n < 3)RETURN
   CALL pgqhs(angle,sepn,phase)
   angle=angle+da
   IF(ABS(sepn) <= EPSILON(sepn))RETURN
!
! The unit spacing is 1 percent of the smaller of the height or
! width of the view surface. The line-spacing (DH), in inches, is
! obtained by multiplying this by argument SEPN.
!
   CALL pgqvsz(1,xs1,xs2,ys1,ys2)
   dh=sepn*MIN(ABS(xs2-xs1),ABS(ys2-ys1))/100.0_pg
!
! DINDX and DINDY are the scales in inches per world-coordinate unit.
!
   CALL pgqvp(1,xs1,xs2,ys1,ys2)
   CALL pgqwin(xl,xr,yb,yt)
   IF((ABS(xr - xl) > EPSILON(xr)).AND.(ABS(yt - yb) > EPSILON(yt)))THEN
      dindx=(xs2-xs1)/(xr-xl)
      dindy=(ys2-ys1)/(yt-yb)
   ELSE
      RETURN
   END IF
!
! Initialize.
!
   CALL pgbbuf
!
! The vector (SX,SY) is a vector length DH perpendicular to
! the hatching lines, which have vector (DX,DY).
!
   dx=COS(angle/57.295779513082321_pg)
   dy=SIN(angle/57.295779513082321_pg)
   sx=(-dh)*dy
   sy=dh*dx
!
! The hatch lines are labelled by a parameter C, the distance from
! the coordinate origin. Calculate CMID, the C-value of the line
! that passes through the hatching reference point (BX,BY), and
! CMIN and CMAX, the range of C-values spanned by lines that intersect
! the polygon.
!
   bx=phase*sx
   by=phase*sy
   cmid=dx*by-dy*bx
   cmin=dx*y(1)*dindy-dy*x(1)*dindx
   cmax=cmin
   DO  i=2,n
      c=dx*y(i)*dindy-dy*x(i)*dindx
      cmin=MIN(c,cmin)
      cmax=MAX(c,cmax)
   END DO
!
! Compute integer labels for the hatch lines; N=0 is the line
! which passes through the reference point; NMIN and NMAX define
! the range of labels for lines that intersect the polygon.
! [Note that INT truncates towards zero; we need FLOOR and CEIL
! functions.]
!
   cmin=(cmin-cmid)/dh
   cmax=(cmax-cmid)/dh
   nmin=INT(cmin)
   IF(REAL(nmin,KIND=pg) < cmin)nmin=nmin+1
   nmax=INT(cmax)
   IF(REAL(nmax,KIND=pg) > cmax)nmax=nmax-1
!
! Each iteration of the following loop draws one hatch line.
!
   DO  j=nmin,nmax
!
! The parametric representation of this hatch line is
! (X,Y) = (QX,QY) + RMU*(DX,DY).
!
      qx=bx+REAL(j,KIND=pg)*sx
      qy=by+REAL(j,KIND=pg)*sy
!
! Find the NX intersections of this line with the edges of the polygon.
!
      nx=0
      ni=n
      DO  i=1,n
         ex=(x(i)-x(ni))*dindx
         ey=(y(i)-y(ni))*dindy
         delta=ex*dy-ey*dx
         IF(ABS(delta) < 1.0E-05_pg)THEN
!
!                 -- lines are parallel
!
         ELSE
!
!                 -- lines intersect in (XI,YI)
!
            r=((qx-x(ni)*dindx)*dy-(qy-y(ni)*dindy)*dx)/delta
            IF((r > 0.0_pg).AND.(r <= 1.0_pg))THEN
               IF(nx < maxp)nx=nx+1
               np(nx)=nx
               IF(ABS(dx) > 0.5_pg)THEN
                  xi=x(ni)*dindx+r*ex
                  rmu(nx)=(xi-qx)/dx
               ELSE
                  yi=y(ni)*dindy+r*ey
                  rmu(nx)=(yi-qy)/dy
               END IF
            END IF
         END IF
         ni=i
      END DO
!
! The RMU array now contains the intersections. Sort them into order.
!
      DO  ii=1,nx-1
         DO  jj=ii+1,nx
            IF(rmu(np(ii)) < rmu(np(jj)))THEN
               nnp=np(ii)
               np(ii)=np(jj)
               np(jj)=nnp
            END IF
         END DO
      END DO
!
! Join the intersections in pairs.
!
      ni=1
!
!         -- do while NI < NX
!
50    IF(ni < nx)THEN
         rmu1=rmu(np(ni))
         rmu2=rmu(np(ni+1))
         CALL pgmove((qx+rmu1*dx)/dindx,(qy+rmu1*dy)/dindy)
         CALL pgdraw((qx+rmu2*dx)/dindx,(qy+rmu2*dy)/dindy)
         ni=ni+2
         GO TO 50
      END IF
   END DO
!
! Tidy up.
!
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pghtch
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGIDEN -- write username, date, and time at bottom of plot
!%void cpgiden(void);
!+
SUBROUTINE pgiden
!
! Write username, date, and time at bottom of plot.
!
! Arguments: none.
!--
!  9-Feb-1988
! 10-Sep-1990 : adjust position of text [TJP]
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER :: l,m,cf,ci,lw
   CHARACTER (LEN=64) :: text
   REAL(KIND=pg) :: d,ch
!
   CALL pgbbuf
!
! Get information for annotation.
!
   CALL gruser(text,l)
   text(l+1:)=' '
   CALL grdate(text(l+2:),m)
   l=l+1+m
!
! Save current attributes.
!
   CALL pgqcf(cf)
   CALL pgqci(ci)
   CALL pgqlw(lw)
   CALL pgqch(ch)
!
! Change attributes and write text.
!
   CALL pgscf(1)
   CALL pgsci(1)
   CALL pgslw(1)
   CALL pgsch(0.6_pg)
   CALL grlen(text(1:l),d)
   CALL grtext(.false.,0.0_pg,.true.,pgxsz(pgid)-d-2.0_pg,2.0_pg +  &
      pgysz(pgid)/130.0_pg,text(1:l))
!
! Restore attributes.
!
   CALL pgscf(cf)
   CALL pgsci(ci)
   CALL pgslw(lw)
   CALL pgsch(ch)
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgiden
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGIMAG -- color image from a 2D data array
!%void cpgimag(const float *a, int idim, int jdim, int i1, int i2, \
!% int j1, int j2, float a1, float a2, const float *tr);
!+
SUBROUTINE pgimag(a,idim,jdim,i1,i2,j1,j2,a1,a2,tr)
!
! Draw a color image of an array in current window. The subsection
! of the array A defined by indices (I1:I2, J1:J2) is mapped onto
! the view surface world-coordinate system by the transformation
! matrix TR. The resulting quadrilateral region is clipped at the edge
! of the window. Each element of the array is represented in the image
! by a small quadrilateral, which is filled with a color specified by
! the corresponding array value.
!
! The subroutine uses color indices in the range C1 to C2, which can
! be specified by calling PGSCIR before PGIMAG. The default values
! for C1 and C2 are device-dependent; these values can be determined by
! calling PGQCIR. Note that color representations should be assigned to
! color indices C1 to C2 by calling PGSCR before calling PGIMAG. On some
! devices (but not all), the color representation can be changed after
! the call to PGIMAG by calling PGSCR again.
!
! Array values in the range A1 to A2 are mapped on to the range of
! color indices C1 to C2, with array values <= A1 being given color
! index C1 and values >= A2 being given color index C2. The mapping
! function for intermediate array values can be specified by
! calling routine PGSITF before PGIMAG; the default is linear.
!
! On devices which have no available color indices (C1 > C2),
! PGIMAG will return without doing anything. On devices with only
! one color index (C1=C2), all array values map to the same color
! which is rather uninteresting. An image is always "opaque",
! i.e., it obscures all graphical elements previously drawn in
! the region.
!
! The transformation matrix TR is used to calculate the world
! coordinates of the center of the "cell" that represents each
! array element. The world coordinates of the center of the cell
! corresponding to array element A(I,J) are given by:
!
!          X = TR(1) + TR(2)*I + TR(3)*J
!          Y = TR(4) + TR(5)*I + TR(6)*J
!
! Usually TR(3) and TR(5) are zero -- unless the coordinate
! transformation involves a rotation or shear.  The corners of the
! quadrilateral region that is shaded by PGIMAG are given by
! applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
!
! Arguments:
!  A      (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  A1     (input)  : the array value which is to appear with shade C1.
!  A2     (input)  : the array value which is to appear with shade C2.
!  TR     (input)  : transformation matrix between array grid and
!                    world coordinates.
!--
! 15-Sep-1994: new routine [TJP].
! 21-Jun-1995: minor change to header comments [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: idim
   INTEGER, INTENT(IN)                  :: jdim
   REAL(KIND=pg), INTENT(IN)            :: a(idim,jdim)
   INTEGER, INTENT(IN)                  :: i1
   INTEGER, INTENT(IN)                  :: i2
   INTEGER, INTENT(IN)                  :: j1
   INTEGER, INTENT(IN)                  :: j2
   REAL(KIND=pg), INTENT(IN)            :: a1
   REAL(KIND=pg), INTENT(IN)            :: a2
   REAL(KIND=pg), INTENT(IN)            :: tr(6)
!
   REAL(KIND=pg) :: pa(6)
   LOGICAL :: pgnoto
!
! Check inputs.
!
   IF(pgnoto('PGIMAG'))RETURN
   IF(i1 < 1.OR.i2 > idim.OR.i1 > i2.OR.j1 < 1.OR.j2 > jdim.OR.j1 > j2)THEN
      CALL grwarn('PGIMAG: invalid range I1:I2, J1:J2')
   ELSE IF(ABS(a1 - a2) <= EPSILON(a1))THEN
      CALL grwarn('PGIMAG: foreground level = background level')
   ELSE IF(pgmnci(pgid) > pgmxci(pgid))THEN
      CALL grwarn('PGIMAG: not enough colors available')
   ELSE
!
! Call lower-level routine to do the work.
!
      CALL pgbbuf
      pa(1)=tr(1)*pgxscl(pgid)+pgxorg(pgid)
      pa(2)=tr(2)*pgxscl(pgid)
      pa(3)=tr(3)*pgxscl(pgid)
      pa(4)=tr(4)*pgyscl(pgid)+pgyorg(pgid)
      pa(5)=tr(5)*pgyscl(pgid)
      pa(6)=tr(6)*pgyscl(pgid)
      CALL grimg0(a,idim,jdim,i1,i2,j1,j2,a1,a2,pa,pgmnci(pgid),  &
         pgmxci(pgid),pgitf(pgid))
      CALL pgebuf
   END IF
!
   RETURN
!
END SUBROUTINE pgimag
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
! PGINIT -- initialize PGPLOT (internal routine)
!
SUBROUTINE pginit
!
! Initialize PGPLOT. This routine should be called once during program
! execution, before any other PGPLOT routines.
!--
! Last modified: 1996 Apr 30 [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   INTEGER :: called,i
   SAVE  called
   DATA called/0/
!
   IF(called == 0)THEN
      pgid=0
      DO  i=1,pgmaxd
         pgdevs(i)=0
      END DO
      CALL grinit
      called=1
   END IF
!
   RETURN
!
END SUBROUTINE pginit
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGLAB -- write labels for x-axis, y-axis, and top of plot
!%void cpglab(const char *xlbl, const char *ylbl, const char *toplbl);
!+
SUBROUTINE pglab(xlbl,ylbl,toplbl)
!
! Write labels outside the viewport. This routine is a simple
! interface to PGMTXT, which should be used if PGLAB is inadequate.
!
! Arguments:
!  XLBL   (input) : a label for the x-axis (centered below the
!                   viewport).
!  YLBL   (input) : a label for the y-axis (centered to the left
!                   of the viewport, drawn vertically).
!  TOPLBL (input) : a label for the entire plot (centered above the
!                   viewport).
!--
! 11-May-1990 - remove unnecessary include - TJP.
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)        :: xlbl
   CHARACTER (LEN=*), INTENT(IN)        :: ylbl
   CHARACTER (LEN=*), INTENT(IN)        :: toplbl
!
   CALL pgbbuf
   CALL pgmtxt('T',2.0_pg,0.5_pg,0.5_pg,toplbl)
   CALL pgmtxt('B',3.2_pg,0.5_pg,0.5_pg,xlbl)
   CALL pgmtxt('L',2.2_pg,0.5_pg,0.5_pg,ylbl)
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pglab
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGLABEL -- non-standard alias for PGLAB
!+
SUBROUTINE pglabel(xlbl,ylbl,toplbl)
!
! See description of PGLAB.
!--
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)        :: xlbl
   CHARACTER (LEN=*), INTENT(IN)        :: ylbl
   CHARACTER (LEN=*), INTENT(IN)        :: toplbl
!
   CALL pglab(xlbl,ylbl,toplbl)
!
END SUBROUTINE pglabel
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGLCUR -- draw a line using the cursor
!%void cpglcur(int maxpt, int *npt, float *x, float *y);
!+
SUBROUTINE pglcur(maxpt,npt,x,y)
!
! Interactive routine for user to enter a polyline by use of
! the cursor.  Routine allows user to Add and Delete vertices;
! vertices are joined by straight-line segments.
!
! Arguments:
!  MAXPT  (input)  : maximum number of points that may be accepted.
!  NPT    (in/out) : number of points entered; should be zero on
!                    first call.
!  X      (in/out) : array of x-coordinates (dimension at least MAXPT).
!  Y      (in/out) : array of y-coordinates (dimension at least MAXPT).
!
! Notes:
!
! (1) On return from the program, cursor points are returned in
! the order they were entered. Routine may be (re-)called with points
! already defined in X,Y (# in NPT), and they will be plotted
! first, before editing.
!
! (2) User commands: the user types single-character commands
! after positioning the cursor: the following are accepted:
!   A (Add)    - add point at current cursor location.
!   D (Delete) - delete last-entered point.
!   X (eXit)   - leave subroutine.
!--
!  5-Aug-1984 - new routine [TJP].
! 16-Jul-1988 - correct error in delete operation [TJP].
! 13-Dec-1990 - change warnings to messages [TJP].
!  3-Sep-1992 - fixed erase first point bug under Add option [JM/TJP].
!  7-Sep-1994 - use PGBAND [TJP].
!  2-Aug-1995 - remove dependence on common block [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: maxpt
   INTEGER, INTENT(IN OUT)                  :: npt
   REAL(KIND=pg), INTENT(IN OUT)            :: x(*)
   REAL(KIND=pg), INTENT(IN OUT)            :: y(*)
!
   LOGICAL :: pgnoto
!
!   JAO:  add the string templetter
!
   CHARACTER (LEN=1) :: letter,templetter
   INTEGER :: pgband,i,savcol,mode
   REAL(KIND=pg) :: xp,yp,xref,yref
   REAL(KIND=pg) :: xblc,xtrc,yblc,ytrc
!
! Check that PGPLOT is in the correct state.
!
   IF(pgnoto('PGLCUR'))RETURN
!
! Save current color.
!
   CALL grqci(savcol)
!
! Put current line-segments on screen.
!
   IF(npt == 1)THEN
      CALL pgpt(1,x(1),y(1),1)
   END IF
   IF(npt > 0)THEN
      CALL grmova(x(1),y(1))
      DO  i=2,npt
         CALL grlina(x(i),y(i))
      END DO
   END IF
!
! Start with the cursor in the middle of the box,
! unless lines have already been drawn.
!
   CALL pgqwin(xblc,xtrc,yblc,ytrc)
   IF(npt > 0)THEN
      xp=x(npt)
      yp=y(npt)
   ELSE
      xp=0.5_pg*(xblc+xtrc)
      yp=0.5_pg*(yblc+ytrc)
   END IF
!
! Loop over cursor inputs.
!
   mode=0
20 xref=xp
   yref=yp
   IF(pgband(mode,1,xref,yref,xp,yp,letter) /= 1)RETURN
!
!   JAO:  Use the temporary string templetter in the
!         call to grtopu
!
   templetter=letter
   CALL grtoup(letter,templetter)
   mode=1
!
! A (ADD) command:
!
   IF(letter == 'A')THEN
      IF(npt >= maxpt)THEN
         CALL grmsg('ADD ignored (too many points).')
         GO TO 20
      END IF
      npt=npt+1
      x(npt)=xp
      y(npt)=yp
      IF(npt == 1)THEN
!
!           -- first point: draw a dot
!
         CALL grmova(x(npt),y(npt))
         CALL pgpt(1,x(npt),y(npt),1)
      ELSE
!
!           -- nth point: draw from (n-1) to (n)
!
         CALL grlina(x(npt),y(npt))
      END IF
      CALL grterm
!
! D (DELETE) command:
!
   ELSE IF(letter == 'D')THEN
      IF(npt <= 0)THEN
         CALL grmsg('DELETE ignored (there are no points left).')
         GO TO 20
      END IF
      IF(npt > 1)THEN
!
!           -- delete nth point: erase from (n-1) to (n)
!
         CALL grmova(x(npt-1),y(npt-1))
         CALL grsci(0)
         CALL grlina(x(npt),y(npt))
         CALL grsci(savcol)
         CALL grmova(x(npt-1),y(npt-1))
         CALL grterm
      ELSE IF(npt == 1)THEN
!
!           -- delete first point: erase dot
!
         CALL grsci(0)
         CALL pgpt(1,x(npt),y(npt),1)
         CALL grsci(savcol)
      END IF
      npt=npt-1
      IF(npt == 0)THEN
         xp=0.5_pg*(xblc+xtrc)
         yp=0.5_pg*(yblc+ytrc)
      ELSE
         xp=x(npt)
         yp=y(npt)
      END IF
      IF(npt == 1)THEN
!
!           -- delete 2nd point: redraw dot at first point
!
         CALL pgpt(1,x(1),y(1),1)
      END IF
!
! X (EXIT) command:
!
   ELSE IF(letter == 'X')THEN
      CALL gretxt
      RETURN
!
! Illegal command:
!
   ELSE
      CALL grmsg('Commands are A (add), D (delete), X (exit).')
   END IF
!
   GO TO 20
!
   RETURN
!
END SUBROUTINE pglcur
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGLDEV -- list available device types on standard output
!%void cpgldev(void);
!+
SUBROUTINE pgldev
!
! Writes (to standard output) a list of all device types available in
! the current PGPLOT installation.
!
! Arguments: none.
!--
! 5-Aug-1986 - [AFT].
! 1-Aug-1988 - add version number [TJP].
! 24-Apr-1989 - add copyright notice [TJP].
! 13-Dec-1990 - changed warnings to messages [TJP].
! 26-Feb-1997 - revised description [TJP].
! 18-Mar-1997 - revised [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   CHARACTER (LEN=16) :: gver
   INTEGER :: l
   CHARACTER (LEN=10) :: t
   CHARACTER (LEN=64) :: d
   INTEGER :: i,n,tlen,dlen,inter
!
! Initialize PGPLOT if necessary.
!
   CALL pginit
!
! Report version and copyright.
!
   CALL pgqinf('VERSION',gver,l)
   CALL grmsg('PGPLOT '//gver(:l)//' Copyright 1997 California Institute of Technology')
!
! Find number of device types.
!
   CALL pgqndt(n)
!
! Loop through device-type list (twice).

   CALL grmsg('Interactive devices:')
   DO  i=1,n
      CALL pgqdt(i,t,tlen,d,dlen,inter)
      IF(tlen > 0.AND.inter == 1)CALL grmsg('   '//t//' '//d(1:dlen))
   END DO
   CALL grmsg('Non-interactive file formats:')
   DO  i=1,n
      CALL pgqdt(i,t,tlen,d,dlen,inter)
      IF(tlen > 0.AND.inter == 0)CALL grmsg('   '//t//' '//d(1:dlen))
   END DO
!
   RETURN
!
END SUBROUTINE pgldev
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGLEN -- find length of a string in a variety of units
!%void cpglen(int units, const char *string, float *xl, float *yl);
!+
SUBROUTINE pglen(units,string,xl,yl)
!
! Work out length of a string in x and y directions
!
! Input
!  UNITS    :  0 => answer in normalized device coordinates
!              1 => answer in inches
!              2 => answer in mm
!              3 => answer in absolute device coordinates (dots)
!              4 => answer in world coordinates
!              5 => answer as a fraction of the current viewport size
!
!  STRING   :  String of interest
! Output
!  XL       :  Length of string in x direction
!  YL       :  Length of string in y direction
!
!--
! 15-Sep-1989 - new routine (Neil Killeen)
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: units
   CHARACTER (LEN=*), INTENT(IN)            :: string
   REAL(KIND=pg), INTENT(OUT)               :: xl
   REAL(KIND=pg), INTENT(OUT)               :: yl
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: d
!
   IF(pgnoto('PGLEN'))RETURN
!
!   Work out length of a string in absolute device coordinates (dots)
!   and then convert
!
   CALL grlen(string,d)
!
   IF(units == 0)THEN
      xl=d/pgxsz(pgid)
      yl=d/pgysz(pgid)
   ELSE IF(units == 1)THEN
      xl=d/pgxpin(pgid)
      yl=d/pgypin(pgid)
   ELSE IF(units == 2)THEN
      xl=25.4_pg*d/pgxpin(pgid)
      yl=25.4_pg*d/pgypin(pgid)
   ELSE IF(units == 3)THEN
      xl=d
      yl=d
   ELSE IF(units == 4)THEN
      xl=d/ABS(pgxscl(pgid))
      yl=d/ABS(pgyscl(pgid))
   ELSE IF(units == 5)THEN
      xl=d/pgxlen(pgid)
      yl=d/pgylen(pgid)
   ELSE
      CALL grwarn('Illegal value for UNITS in routine PGLEN')
   END IF
!
   RETURN
!
END SUBROUTINE pglen
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGLINE -- draw a polyline (curve defined by line-segments)
!%void cpgline(int n, const float *xpts, const float *ypts);
!+
SUBROUTINE pgline(n,xpts,ypts)
!
! Primitive routine to draw a Polyline. A polyline is one or more
! connected straight-line segments.  The polyline is drawn using
! the current setting of attributes color-index, line-style, and
! line-width. The polyline is clipped at the edge of the window.
!
! Arguments:
!  N      (input)  : number of points defining the line; the line
!                    consists of (N-1) straight-line segments.
!                    N should be greater than 1 (if it is 1 or less,
!                    nothing will be drawn).
!  XPTS   (input)  : world x-coordinates of the points.
!  YPTS   (input)  : world y-coordinates of the points.
!
! The dimension of arrays X and Y must be greater than or equal to N.
! The "pen position" is changed to (X(N),Y(N)) in world coordinates
! (if N > 1).
!--
! 27-Nov-1986
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN OUT)            :: xpts(*)
   REAL(KIND=pg), INTENT(IN OUT)            :: ypts(*)
!
   INTEGER :: i
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGLINE'))RETURN
   IF(n < 2)RETURN
!
   CALL pgbbuf
   CALL grmova(xpts(1),ypts(1))
   DO  i=2,n
      CALL grlina(xpts(i),ypts(i))
   END DO
   CALL pgebuf
!
END SUBROUTINE pgline
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!

!*PGMOVE -- move pen (change current pen position)
!%void cpgmove(float x, float y);
!+
SUBROUTINE pgmove(x,y)
!
! Primitive routine to move the "pen" to the point with world
! coordinates (X,Y). No line is drawn.
!
! Arguments:
!  X      (input)  : world x-coordinate of the new pen position.
!  Y      (input)  : world y-coordinate of the new pen position.
!--
! (29-Dec-1983)
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)              :: x
   REAL(KIND=pg), INTENT(IN)              :: y
!
   CALL grmova(x,y)
!
END SUBROUTINE pgmove
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGMTEXT -- non-standard alias for PGMTXT
!+
SUBROUTINE pgmtext(side,disp,coord,fjust,text)
!
! See description of PGMTXT.
!--
   USE accur
!
   CHARACTER (LEN=*), INTENT(IN)        :: side
   REAL(KIND=pg), INTENT(IN)            :: disp
   REAL(KIND=pg), INTENT(IN)            :: coord
   REAL(KIND=pg), INTENT(IN)            :: fjust
   CHARACTER (LEN=*), INTENT(IN)        :: text
!
   CALL pgmtxt(side,disp,coord,fjust,text)
!
END SUBROUTINE pgmtext
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGMTXT -- write text at position relative to viewport
!%void cpgmtxt(const char *side, float disp, float coord, \
!% float fjust, const char *text);
!+
SUBROUTINE pgmtxt(side,disp,coord,fjust,text)
!
! Write text at a position specified relative to the viewport (outside
! or inside).  This routine is useful for annotating graphs. It is used
! by routine PGLAB.  The text is written using the current values of
! attributes color-index, line-width, character-height, and
! character-font.
!
! Arguments:
!  SIDE   (input)  : must include one of the characters 'B', 'L', 'T',
!                    or 'R' signifying the Bottom, Left, Top, or Right
!                    margin of the viewport. If it includes 'LV' or
!                    'RV', the string is written perpendicular to the
!                    frame rather than parallel to it.
!  DISP   (input)  : the displacement of the character string from the
!                    specified edge of the viewport, measured outwards
!                    from the viewport in units of the character
!                    height. Use a negative value to write inside the
!                    viewport, a positive value to write outside.
!  COORD  (input)  : the location of the character string along the
!                    specified edge of the viewport, as a fraction of
!                    the length of the edge.
!  FJUST  (input)  : controls justification of the string parallel to
!                    the specified edge of the viewport. If
!                    FJUST = 0.0, the left-hand end of the string will
!                    be placed at COORD; if JUST = 0.5, the center of
!                    the string will be placed at COORD; if JUST = 1.0,
!                    the right-hand end of the string will be placed at
!                    at COORD. Other values between 0 and 1 give inter-
!                    mediate placing, but they are not very useful.
!  TEXT   (input) :  the text string to be plotted. Trailing spaces are
!                    ignored when justifying the string, but leading
!                    spaces are significant.
!
!--
! 18-Apr-1983
! 15-Aug-1987 - fix BBUF/EBUF error.
! 27-Aug-1987 - fix justification error if XPERIN.ne.YPERIN.
! 05-Sep-1989 - change so that DISP has some effect for 'RV' and
!               'LV' options [nebk]
! 16-Oct-1993 - erase background of opaque text.
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: side
   REAL(KIND=pg), INTENT(IN)                :: disp
   REAL(KIND=pg), INTENT(IN)                :: coord
   REAL(KIND=pg), INTENT(IN)                :: fjust
   CHARACTER (LEN=*), INTENT(IN)            :: text
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: angle,d,x,y,ratio,xbox(4),ybox(4)
   INTEGER :: ci,i,l,grtrim
   CHARACTER (LEN=20) :: test
!
   IF(pgnoto('PGMTXT'))RETURN
!
   l=grtrim(text)
   IF(l < 1)RETURN
   d=0.0_pg
   IF(ABS(fjust) > EPSILON(fjust))CALL grlen(text(1:l),d)
   d=d*fjust
   ratio=pgypin(pgid)/pgxpin(pgid)
   CALL grtoup(test,side)
   IF(INDEX(test,'B') /= 0)THEN
      angle=0.0_pg
      x=pgxoff(pgid)+coord*pgxlen(pgid)-d
      y=pgyoff(pgid)-pgysp(pgid)*disp
   ELSE IF(INDEX(test,'LV') /= 0)THEN
      angle=0.0_pg
      x=pgxoff(pgid)-pgysp(pgid)*disp-d
      y=pgyoff(pgid)+coord*pgylen(pgid)-0.3_pg*pgysp(pgid)
   ELSE IF(INDEX(test,'L') /= 0)THEN
      angle=90.0_pg
      x=pgxoff(pgid)-pgysp(pgid)*disp
      y=pgyoff(pgid)+coord*pgylen(pgid)-d*ratio
   ELSE IF(INDEX(test,'T') /= 0)THEN
      angle=0.0_pg
      x=pgxoff(pgid)+coord*pgxlen(pgid)-d
      y=pgyoff(pgid)+pgylen(pgid)+pgysp(pgid)*disp
   ELSE IF(INDEX(test,'RV') /= 0)THEN
      angle=0.0_pg
      x=pgxoff(pgid)+pgxlen(pgid)+pgysp(pgid)*disp-d
      y=pgyoff(pgid)+coord*pgylen(pgid)-0.3_pg*pgysp(pgid)
   ELSE IF(INDEX(test,'R') /= 0)THEN
      angle=90.0_pg
      x=pgxoff(pgid)+pgxlen(pgid)+pgysp(pgid)*disp
      y=pgyoff(pgid)+coord*pgylen(pgid)-d*ratio
   ELSE
      CALL grwarn('Invalid "SIDE" argument in PGMTXT.')
      RETURN
   END IF
   CALL pgbbuf
   IF(pgtbci(pgid) >= 0)THEN
      CALL grqtxt(angle,x,y,text(1:l),xbox,ybox)
      DO  i=1,4
         xbox(i)=(xbox(i)-pgxorg(pgid))/pgxscl(pgid)
         ybox(i)=(ybox(i)-pgyorg(pgid))/pgyscl(pgid)
      END DO
      CALL pgqci(ci)
      CALL pgsci(pgtbci(pgid))
      CALL grfa(4,xbox,ybox)
      CALL pgsci(ci)
   END IF
   CALL grtext(.false.,angle,.true.,x,y,text(1:l))
   CALL pgebuf
!
END SUBROUTINE pgmtxt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGNCUR -- mark a set of points using the cursor
!%void cpgncur(int maxpt, int *npt, float *x, float *y, int symbol);
!+
SUBROUTINE pgncur(maxpt,npt,x,y,symbol)
!
! Interactive routine for user to enter data points by use of
! the cursor.  Routine allows user to Add and Delete points.  The
! points are returned in order of increasing x-coordinate, not in the
! order they were entered.
!
! Arguments:
!  MAXPT  (input)  : maximum number of points that may be accepted.
!  NPT    (in/out) : number of points entered; should be zero on
!                    first call.
!  X      (in/out) : array of x-coordinates.
!  Y      (in/out) : array of y-coordinates.
!  SYMBOL (input)  : code number of symbol to use for marking
!                    entered points (see PGPT).
!
! Note (1): The dimension of arrays X and Y must be greater than or
! equal to MAXPT.
!
! Note (2): On return from the program, cursor points are returned in
! increasing order of X. Routine may be (re-)called with points
! already defined in X,Y (number in NPT), and they will be plotted
! first, before editing.
!
! Note (3): User commands: the user types single-character commands
! after positioning the cursor: the following are accepted:
! A (Add)    - add point at current cursor location.
! D (Delete) - delete nearest point to cursor.
! X (eXit)   - leave subroutine.
!--
! 27-Nov-1983
!  9-Jul-1983 - modified to use GRSCI instead of GRSETLI [TJP].
! 13-Dec-1990 - changed warnings to messages [TJP].
!  2-Aug-1995 - [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE

   INTEGER, INTENT(IN)                  :: maxpt
   INTEGER, INTENT(IN OUT)              :: npt
   REAL(KIND=pg), INTENT(IN OUT)        :: x(*)
   REAL(KIND=pg), INTENT(IN OUT)        :: y(*)
   INTEGER, INTENT(IN)                  :: symbol
!
!   JAO:  add the temporary string templetter
!
   CHARACTER (LEN=1) :: letter,templetter
   LOGICAL :: pgnoto
   INTEGER :: pgcurs,i,j,savcol
   REAL(KIND=pg) :: delta,xp,yp,xphys,yphys
   REAL(KIND=pg) :: xmin,xip,yip
   REAL(KIND=pg) :: xblc,xtrc,yblc,ytrc
!
! Check that PGPLOT is in the correct state.
!
   IF(pgnoto('PGNCUR'))RETURN
!
! Save current color.
!
   CALL grqci(savcol)
!
! Put current points on screen.
!
   IF(npt /= 0)CALL pgpt(npt,x,y,symbol)
!
! Start with the cursor in the middle of the viewport.
!
   CALL pgqwin(xblc,xtrc,yblc,ytrc)
   xp=0.5_pg*(xblc+xtrc)
   yp=0.5_pg*(yblc+ytrc)
!
! Loop over cursor inputs.
!
10 IF(pgcurs(xp,yp,letter) /= 1)RETURN
   IF(letter == CHAR(0))RETURN
!
!  JAO:  use the temporary string templetter in the
!        argument list
!
   templetter=letter
   CALL grtoup(letter,templetter)
!
! A (ADD) command:
!
   IF(letter == 'A')THEN
      IF(npt >= maxpt)THEN
         CALL grmsg('ADD ignored (too many points).')
         GO TO 10
      END IF
!
!         ! Find what current points new point is between.
!
      DO  j=1,npt
         IF(xp < x(j))GO TO 30
      END DO
      j=npt+1
!
!         ! New point is beyond last current
!
30    CONTINUE
!
!         ! J is vector location where new point should be included.
!
      DO  i=npt,j,-1
         x(i+1)=x(i)
         y(i+1)=y(i)
      END DO
      npt=npt+1
!
!         ! Add new point to point array.
!
      x(j)=xp
      y(j)=yp
      CALL pgpt(1,x(j),y(j),symbol)
      CALL grterm
!
! D (DELETE) command:
!
   ELSE IF(letter == 'D')THEN
      IF(npt <= 0)THEN
         CALL grmsg('DELETE ignored (there are no points left).')
         GO TO 10
      END IF
      xmin=1.0E+08_pg
!
!         ! Look for point closest in radius.
!         ! Convert cursor points to physical.
!
      xphys=pgxorg(pgid)+xp*pgxscl(pgid)
      yphys=pgyorg(pgid)+yp*pgyscl(pgid)
      DO  i=1,npt

!
!             ! Convert array points to physical.
!
         xip=pgxorg(pgid)+x(i)*pgxscl(pgid)
         yip=pgyorg(pgid)+y(i)*pgyscl(pgid)
         delta=SQRT((xip-xphys)**2+(yip-yphys)**2)
         IF(delta < xmin)THEN
            xmin=delta
            j=i
         END IF
      END DO
!
!         ! Remove point from screen by writing in background color.
!
      CALL grsci(0)
      CALL pgpt(1,x(j),y(j),symbol)
      CALL grsci(savcol)
      CALL grterm
!
!         ! Remove point from cursor array.
!
      npt=npt-1
      DO  i=j,npt
         x(i)=x(i+1)
         y(i)=y(i+1)
      END DO
!
! X (EXIT) command:
!
   ELSE IF(letter == 'X')THEN
      CALL gretxt
      RETURN
!
! Illegal command:
!
   ELSE
      CALL grmsg('Commands are A (add), D (delete), X (exit).')
   END IF
!
   GO TO 10
!
END SUBROUTINE pgncur
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGNCURSE -- non-standard alias for PGNCUR
!+
SUBROUTINE pgncurse(maxpt,npt,x,y,symbol)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: maxpt
   INTEGER, INTENT(IN OUT)                  :: npt
   REAL(KIND=pg), INTENT(IN OUT)            :: x(*)
   REAL(KIND=pg), INTENT(IN OUT)            :: y(*)
   INTEGER, INTENT(IN)                      :: symbol
!
! See description of PGNCUR.
!--
   CALL pgncur(maxpt,npt,x,y,symbol)
!
END SUBROUTINE pgncurse
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
LOGICAL FUNCTION pgnoto(rtn)
!
! PGPLOT (internal routine): Test whether a PGPLOT device is open and
! print a message if not. Usage:
!     LOGICAL PGNOTO
!     IF (PGNOTO('routine')) RETURN
!
! Arguments:
!
! RTN (input, character): routine name to be include in message.
!
! Returns:
!     .TRUE. if PGPLOT is not open.
!--
! 11-Nov-1994
! 21-Dec-1995 - revised for multiple devices.
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: rtn
!
   CHARACTER (LEN=80) :: text
!
   CALL pginit
   pgnoto=.false.
   IF(pgid < 1.OR.pgid > pgmaxd)THEN
      pgnoto=.true.
      text=rtn//': no graphics device has been selected'
      CALL grwarn(text)
   ELSE IF(pgdevs(pgid) /= 1)THEN
      pgnoto=.true.
      text=rtn//': selected graphics device is not open'
      CALL grwarn(text)
   END IF
!
   RETURN
!
END FUNCTION pgnoto
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!
!.PGNPL -- Work out how many numerals there are in an integer
!.
SUBROUTINE pgnpl(nmax,n,npl)
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: nmax
   INTEGER, INTENT(IN)                      :: n
   INTEGER, INTENT(OUT)                     :: npl
!
!     Work out how many numerals there are in an integer for use with
!     format statements.
!     e.g.  N=280 => NPL=3,   N=-3 => NPL=2
!
!     Input:
!       NMAX   :   If > 0, issue a warning that N is going to
!                  exceed the format statement field size if NPL
!                  exceeds NMAX
!       N      :   Integer of interest
!     Output:
!       NPL    :   Number of numerals
!
!-
!  20-Apr-1991 -- new routine (Neil Killeen)
!-------------------------------------------------------------------------
!
   IF(n == 0)THEN
      npl=1
   ELSE
      npl=INT(LOG10(REAL(ABS(n),KIND=pg)))+1
   END IF
   IF(n < 0)npl=npl+1
!
   IF(nmax > 0.AND.npl > nmax)CALL grwarn ('pgnpl: output conversion error likely; ' &
      //'number too big for format')
!
   RETURN
!
END SUBROUTINE pgnpl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGNUMB -- convert a number into a plottable character string
!%void cpgnumb(int mm, int pp, int form, char *string, \
!% int *string_length);
!+
SUBROUTINE pgnumb(mm,pp,FORM,string,nc)
!
! This routine converts a number into a decimal character
! representation. To avoid problems of floating-point roundoff, the
! number must be provided as an integer (MM) multiplied by a power of 10
! (10**PP).  The output string retains only significant digits of MM,
! and will be in either integer format (123), decimal format (0.0123),
! or exponential format (1.23x10**5). Standard escape sequences \u, \d
! raise the exponent and \x is used for the multiplication sign.
! This routine is used by PGBOX to create numeric labels for a plot.
!
! Formatting rules:
!   (a) Decimal notation (FORM=1):
!       - Trailing zeros to the right of the decimal sign are
!         omitted
!       - The decimal sign is omitted if there are no digits
!         to the right of it
!       - When the decimal sign is placed before the first digit
!         of the number, a zero is placed before the decimal sign
!       - The decimal sign is a period (.)
!       - No spaces are placed between digits (ie digits are not
!         grouped in threes as they should be)
!       - A leading minus (-) is added if the number is negative
!   (b) Exponential notation (FORM=2):
!       - The exponent is adjusted to put just one (non-zero)
!         digit before the decimal sign
!       - The mantissa is formatted as in (a), unless its value is
!         1 in which case it and the multiplication sign are omitted
!       - If the power of 10 is not zero and the mantissa is not
!         zero, an exponent of the form \x10\u[-]nnn is appended,
!         where \x is a multiplication sign (cross), \u is an escape
!         sequence to raise the exponent, and as many digits nnn
!         are used as needed
!   (c) Automatic choice (FORM=0):
!         Decimal notation is used if the absolute value of the
!         number is less than 10000 or greater than or equal to
!         0.01. Otherwise exponential notation is used.
!
! Arguments:
!  MM     (input)
!  PP     (input)  : the value to be formatted is MM*10**PP.
!  FORM   (input)  : controls how the number is formatted:
!                    FORM = 0 -- use either decimal or exponential
!                    FORM = 1 -- use decimal notation
!                    FORM = 2 -- use exponential notation
!  STRING (output) : the formatted character string, left justified.
!                    If the length of STRING is insufficient, a single
!                    asterisk is returned, and NC=1.
!  NC     (output) : the number of characters used in STRING:
!                    the string to be printed is STRING(1:NC).
!--
! 23-Nov-1983
!  9-Feb-1988 [TJP] - Use temporary variable to avoid illegal character
!                     assignments; remove non-standard DO loops.
! 15-Dec-1988 [TJP] - More corrections of the same sort.
! 27-Nov-1991 [TJP] - Change code for multiplication sign.
! 23-Jun-1994 [TJP] - Partial implementation of FORM=1 and 2.
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER(KIND=8), INTENT(IN)              :: mm
   INTEGER, INTENT(IN)                      :: pp
   INTEGER, INTENT(IN)                      :: FORM
   CHARACTER (LEN=*), INTENT(OUT)           :: string
   INTEGER, INTENT(OUT)                     :: nc
!
   CHARACTER (LEN=1) :: bslash
   CHARACTER (LEN=2) :: times,up,down
   CHARACTER (LEN=20) :: wexp,temp
   CHARACTER (LEN=22) :: work
   INTEGER :: p,nd,i,j,k,nbp
   LOGICAL :: minus
   INTEGER(KIND=8) :: m
!
!   JAO:  define mm and m as 64-bit integers
!   JAO:  initialize work string
!
   work=''
!
! Define backslash (escape) character and escape sequences.
!
   bslash=CHAR(92)
   times=bslash//'x'
   up=bslash//'u'
   down=bslash//'d'
!
! Zero is always printed as "0".
!
   IF(mm == 0)THEN
      string='0'
      nc=1
      RETURN
   END IF
!
! If negative, make a note of that fact.
!
   minus=mm < 0
   m=ABS(mm)
   p=pp
!
! Convert M to a left-justified digit string in WORK. As M is a
! positive integer, it cannot use more than 10 digits (2147483647).
!
!  JAO:  use a 64 bit integer and 16 places
!
   j=16
10 IF(m /= 0)THEN
      k=INT(MOD(m,10_8),KIND=4)
      m=m/10
      work(j:j)=CHAR(ICHAR('0')+k)
      j=j-1
      GO TO 10
   END IF
   temp=work(j+1:)
   work=temp
   nd=16-j
!
! Remove right-hand zeros, and increment P for each one removed.
! ND is the final number of significant digits in WORK, and P the
! power of 10 to be applied. Number of digits before decimal point
! is NBP.
!
20 IF(work(nd:nd) == '0')THEN
      nd=nd-1
      p=p+1
      GO TO 20
   END IF
   nbp=nd+MIN(p,0)
!
! Integral numbers of 4 or less digits are formatted as such.
!
! JAO:  made the limit 5 digits
!
   IF((p >= 0).AND.((FORM == 0.AND.p+nd <= 5).OR.(FORM == 1.AND.  &
      p+nd <= 10)))THEN
      DO  i=1,p
         nd=nd+1
         work(nd:nd)='0'
      END DO
      p=0
!
! If NBP is 4 or less, simply insert a decimal point in the right place.
!
   ELSE IF(FORM /= 2.AND.nbp >= 1.AND.nbp <= 5.AND.nbp < nd)  &
      THEN
      temp=work(nbp+1:nd)
      work(nbp+2:nd+1)=temp
      work(nbp+1:nbp+1)='.'
      nd=nd+1
      p=0
!
! Otherwise insert a decimal point after the first digit, and adjust P.
!
   ELSE
      p=p+nd-1
      IF(FORM /= 2.AND.p == -1)THEN
         temp=work(1:20)
         work='0'//temp
         nd=nd+1
         p=0
      ELSE IF(FORM /= 2.AND.p == -2)THEN
         temp=work(1:20)
         work='00'//temp
         nd=nd+2
         p=0
      END IF
      IF(nd > 1)THEN
         temp=work(2:nd)
         work(3:nd+1)=temp
         work(2:2)='.'
         nd=nd+1
      END IF
   END IF
!
! Add exponent if necessary.
!
   IF(p /= 0)THEN
      work(nd+1:nd+6)=times//'10'//up
      nd=nd+6
      IF(p < 0)THEN
         p=-p
         nd=nd+1
         work(nd:nd)='-'
      END IF
      j=10
40    IF(p /= 0)THEN
         k=MOD(p,10)
         p=p/10
         wexp(j:j)=CHAR(ICHAR('0')+k)
         j=j-1
         GO TO 40
      END IF
      work(nd+1:)=wexp(j+1:10)
      nd=nd+10-j
      IF(work(1:3) == '1'//times)THEN
         temp=work(4:)
         work=temp
         nd=nd-3
      END IF
      work(nd+1:nd+2)=down
      nd=nd+2
   END IF
!
! Add minus sign if necessary and move result to output.
!
   IF(minus)THEN
      temp=work(1:nd)
      string='-'//temp
      nc=nd+1
   ELSE
      string=work(1:nd)
      nc=nd
   END IF
!
! Check result fits.
!
   IF(nc > LEN(string))THEN
      string='*'
      nc=1
   END IF
!
END SUBROUTINE pgnumb
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGOLIN -- mark a set of points using the cursor
!%void cpgolin(int maxpt, int *npt, float *x, float *y, int symbol);
!
SUBROUTINE pgolin(maxpt,npt,x,y,symbol)
!
! Interactive routine for user to enter data points by use of
! the cursor.  Routine allows user to Add and Delete points.  The
! points are returned in the order that they were entered (unlike
! PGNCUR).
!
! Arguments:
!  MAXPT  (input)  : maximum number of points that may be accepted.
!  NPT    (in/out) : number of points entered; should be zero on
!                    first call.
!  X      (in/out) : array of x-coordinates.
!  Y      (in/out) : array of y-coordinates.
!  SYMBOL (input)  : code number of symbol to use for marking
!                    entered points (see PGPT).
!
! Note (1): The dimension of arrays X and Y must be greater than or
! equal to MAXPT.
!
! Note (2): On return from the program, cursor points are returned in
! the order they were entered. Routine may be (re-)called with points
! already defined in X,Y (number in NPT), and they will be plotted
! first, before editing.
!
! Note (3): User commands: the user types single-character commands
! after positioning the cursor: the following are accepted:
! A (Add)    - add point at current cursor location.
! D (Delete) - delete the last point entered.
! X (eXit)   - leave subroutine.
!--
!  4-Nov-1985 - new routine (adapted from PGNCUR) - TJP.
! 13-Dec-1990 - change warnings to messages [TJP].
!  7-Sep-1994 - use PGBAND [TJP].
!  2-Aug-1995 - remove dependence on common block [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: maxpt
   INTEGER, INTENT(OUT)                     :: npt
   REAL(KIND=pg), INTENT(IN OUT)            :: x(*)
   REAL(KIND=pg), INTENT(IN OUT)            :: y(*)
   INTEGER, INTENT(IN)                      :: symbol
!
   LOGICAL :: pgnoto
!
!   JAO:   added the temporary string templetter
!
   CHARACTER (LEN=1) :: letter,templetter
   INTEGER :: pgband,savcol
   REAL(KIND=pg) :: xp,yp,xref,yref
   REAL(KIND=pg) :: xblc,xtrc,yblc,ytrc
!
! Check that PGPLOT is in the correct state.
!
   IF(pgnoto('PGOLIN'))RETURN
!
! Save current color.
!
   CALL grqci(savcol)
!
! Put current points on screen.  Position cursor on last point,
! or in middle viewport if there are no current points.
!
   CALL pgqwin(xblc,xtrc,yblc,ytrc)
   IF(npt /= 0)THEN
      CALL pgpt(npt,x,y,symbol)
      xp=x(npt)
      yp=y(npt)
   ELSE
      xp=0.5_pg*(xblc+xtrc)
      yp=0.5_pg*(yblc+ytrc)
   END IF
!
! Loop over cursor inputs.
!
10 xref=xp
   yref=yp
   IF(pgband(0,1,xref,yref,xp,yp,letter) /= 1)RETURN
   IF(letter == CHAR(0))RETURN
!
!   JAO:  use the temporary string templetter in the
!         argument list of grtopu
!
   templetter=letter
   CALL grtoup(letter,templetter)
!
! A (ADD) command:
!
   IF(letter == 'A')THEN
      IF(npt >= maxpt)THEN
         CALL grmsg('ADD ignored (too many points).')
      ELSE
         npt=npt+1
         x(npt)=xp
         y(npt)=yp
         CALL pgpt(1,x(npt),y(npt),symbol)
         CALL grterm
      END IF
!
! D (DELETE) command:
!
   ELSE IF(letter == 'D')THEN
      IF(npt <= 0)THEN
         CALL grmsg('DELETE ignored (there are no points left).')
      ELSE
         CALL grsci(0)
         CALL pgpt(1,x(npt),y(npt),symbol)
         xp=x(npt)
         yp=y(npt)
         CALL grsci(savcol)
         CALL grterm
         npt=npt-1
      END IF
!
! X (EXIT) command:
!
   ELSE IF(letter == 'X')THEN
      CALL gretxt
      RETURN
!
! Illegal command:
!
   ELSE
      CALL grmsg('Commands are A (add), D (delete), X (exit).')
   END IF
!
   GO TO 10
!
   RETURN
!
END SUBROUTINE pgolin
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGOPEN -- open a graphics device
!%int cpgopen(const char *device);
!+
FUNCTION pgopen(device)
!
! Open a graphics device for PGPLOT output. If the device is
! opened successfully, it becomes the selected device to which
! graphics output is directed until another device is selected
! with PGSLCT or the device is closed with PGCLOS.
!
! The value returned by PGOPEN should be tested to ensure that
! the device was opened successfully, e.g.,
!
!       ISTAT = PGOPEN('plot.ps/PS')
!       IF (ISTAT .LE. 0 ) STOP
!
! Note that PGOPEN must be declared INTEGER in the calling program.
!
! The DEVICE argument is a character constant or variable; its value
! should be one of the following:
!
! (1) A complete device specification of the form 'device/type' or
!     'file/type', where 'type' is one of the allowed PGPLOT device
!     types (installation-dependent) and 'device' or 'file' is the
!     name of a graphics device or disk file appropriate for this type.
!     The 'device' or 'file' may contain '/' characters; the final
!     '/' delimits the 'type'. If necessary to avoid ambiguity,
!     the 'device' part of the string may be enclosed in double
!     quotation marks.
! (2) A device specification of the form '/type', where 'type' is one
!     of the allowed PGPLOT device types. PGPLOT supplies a default
!     file or device name appropriate for this device type.
! (3) A device specification with '/type' omitted; in this case
!     the type is taken from the environment variable PGPLOT_TYPE,
!     if defined (e.g., setenv PGPLOT_TYPE PS). Because of possible
!     confusion with '/' in file-names, omitting the device type
!     in this way is not recommended.
! (4) A blank string (' '); in this case, PGOPEN will use the value
!     of environment variable PGPLOT_DEV as the device specification,
!     or '/NULL' if the environment variable is undefined.
! (5) A single question mark, with optional trailing spaces ('?'); in
!     this case, PGPLOT will prompt the user to supply the device
!     specification, with a prompt string of the form
!         'Graphics device/type (? to see list, default XXX):'
!     where 'XXX' is the default (value of environment variable
!     PGPLOT_DEV).
! (6) A non-blank string in which the first character is a question
!     mark (e.g., '?Device: '); in this case, PGPLOT will prompt the
!     user to supply the device specification, using the supplied
!     string as the prompt (without the leading question mark but
!     including any trailing spaces).
!
! In cases (5) and (6), the device specification is read from the
! standard input. The user should respond to the prompt with a device
! specification of the form (1), (2), or (3). If the user types a
! question-mark in response to the prompt, a list of available device
! types is displayed and the prompt is re-issued. If the user supplies
! an invalid device specification, the prompt is re-issued. If the user
! responds with an end-of-file character, e.g., ctrl-D in UNIX, program
! execution is aborted; this  avoids the possibility of an infinite
! prompting loop.  A programmer should avoid use of PGPLOT-prompting
! if this behavior is not desirable.
!
! The device type is case-insensitive (e.g., '/ps' and '/PS' are
! equivalent). The device or file name may be case-sensitive in some
! operating systems.
!
! Examples of valid DEVICE arguments:
!
! (1)  'plot.ps/ps', 'dir/plot.ps/ps', '"dir/plot.ps"/ps',
!      'user:[tjp.plots]plot.ps/PS'
! (2)  '/ps'      (PGPLOT interprets this as 'pgplot.ps/ps')
! (3)  'plot.ps'  (if PGPLOT_TYPE is defined as 'ps', PGPLOT
!                  interprets this as 'plot.ps/ps')
! (4)  '   '      (if PGPLOT_DEV is defined)
! (5)  '?  '
! (6)  '?Device specification for PGPLOT: '
!
! [This routine was added to PGPLOT in Version 5.1.0. Older programs
! use PGBEG instead.]
!
! Returns:
!  PGOPEN          : returns either a positive value, the
!                    identifier of the graphics device for use with
!                    PGSLCT, or a 0 or negative value indicating an
!                    error. In the event of error a message is
!                    written on the standard error unit.
! Arguments:
!  DEVICE  (input) : the 'device specification' for the plot device
!                    (see above).
!--
! 22-Dec-1995 - new routine [TJP].
! 14-May-1996 - device '? ' should not give a blank prompt [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: device
!
   INTEGER :: deftyp,grdtyp,gropen,l,lr,ic1,lpromp,pgopen
   INTEGER :: grgcom,ier,ldefde,UNIT,istat
   REAL(KIND=pg) :: dummy,dummy2,xcsz,xsz,ysz
   CHARACTER (LEN=128) :: defdev,prompt
   CHARACTER (LEN=20) :: defstr,tempdef
   CHARACTER (LEN=256) :: req
   LOGICAL :: junk
!
! Initialize PGPLOT if necessary.
!
   CALL pginit
!
! Get the default device/type (environment variable PGPLOT_DEV).
!
   CALL grgenv('DEV',defdev,ldefde)
   IF(ldefde == 0)THEN
      defdev='/NULL'
      ldefde=5
   END IF
!
! Open the plot file; default type is given by environment variable
! PGPLOT_TYPE.
!
   CALL grgenv('TYPE',defstr,l)
!
!   JAO:  copy defstr to a temporary string in order
!         to call grtopu.
!
   tempdef=defstr
   IF(l == 0)THEN
      deftyp=0
   ELSE
      CALL grtoup(defstr,tempdef)
      deftyp=grdtyp(defstr(1:l))
   END IF
   IF(device == ' ')THEN
!
!        -- Blank device string: use default device and type.
!
      istat=gropen(deftyp,UNIT,defdev(1:ldefde),pgid)
   ELSE IF(device(1:1) == '?')THEN
      IF(device == '?')THEN
!
!           -- Device string is a ingle question mark: prompt user
!           -- for device/type
!
         prompt='Graphics device/type (? to see list, default '//  &
            defdev(1:ldefde)//'): '
         lpromp=ldefde+48
      ELSE
!
!           -- Device string starts with a question mark: use it
!           -- as a prompt
!
         prompt=device(2:)
         lpromp=LEN(device)-1
      END IF
10    ier=grgcom(req,prompt(1:lpromp),lr)
      IF(ier /= 1)THEN
         CALL grwarn('Error reading device specification')
         pgopen=-1
         RETURN
      END IF
      IF(lr < 1.OR.req == ' ')THEN
         req=defdev(1:ldefde)
      ELSE IF(req(1:1) == '?')THEN
         CALL pgldev
         GO TO 10
      END IF
      istat=gropen(deftyp,UNIT,req,pgid)
      IF(istat /= 1)GO TO 10
   ELSE
      istat=gropen(deftyp,UNIT,device,pgid)
   END IF
!
! Failed to open plot file?
!
   IF(istat /= 1)THEN
      pgopen=-1
      RETURN
   END IF
!
! Success: determine device characteristics.
!
   IF(pgid < 0.OR.pgid > pgmaxd)CALL grwarn('something terribly wrong in PGOPEN')
   pgdevs(pgid)=1
   pgadvs(pgid)=0
   pgpfix(pgid)=.false.
   CALL grsize(pgid,xsz,ysz,dummy,dummy2,pgxpin(pgid),pgypin(pgid))
   CALL grchsz(pgid,xcsz,dummy,pgxsp(pgid),pgysp(pgid))
   pgrows(pgid)=.true.
   pgnx(pgid)=1
   pgny(pgid)=1
   pgxsz(pgid)=xsz
   pgysz(pgid)=ysz
   pgnxc(pgid)=1
   pgnyc(pgid)=1
   CALL grqtyp(defstr,junk)
!
! Set the prompt state to ON, so that terminal devices pause between
! pages; this can be changed with PGASK.
!
   CALL pgask(.true.)
!
! If environment variable PGPLOT_BUFFER is defined (any value),
! start buffering output.
!
   pgblev(pgid)=0
   CALL grgenv('BUFFER',defstr,l)
   IF(l > 0)CALL pgbbuf
!
! Set background and foreground colors if requested.
!
   CALL grgenv('BACKGROUND',defstr,l)
   IF(l > 0)CALL pgscrn(0,defstr(1:l),ier)
   CALL grgenv('FOREGROUND',defstr,l)
   IF(l > 0)CALL pgscrn(1,defstr(1:l),ier)
!
! Set default attributes.
!
   CALL pgsci(1)
   CALL pgsls(1)
   CALL pgslw(1)
   CALL pgsch(1.0_pg)
   CALL pgscf(1)
   CALL pgsfs(1)
   CALL pgsah(1,45.0_pg,0.3_pg)
   CALL pgstbg(-1)
   CALL pgshs(45.0_pg,1.0_pg,0.0_pg)
   CALL pgsclp(1)
!
! Set the default range of color indices available for images (16 to
! device maximum, if device maximum >= 16; otherwise not possible).
! Select linear transfer function.
!
   CALL grqcol(ic1,pgmxci(pgid))
   pgmnci(pgid)=16
   IF(pgmxci(pgid) < 16)pgmxci(pgid)=0
   pgitf(pgid)=0
!
! Set the default window (unit square).
!
   pgxblc(pgid)=0.0_pg
   pgxtrc(pgid)=1.0_pg
   pgyblc(pgid)=0.0_pg
   pgytrc(pgid)=1.0_pg
!
! Set the default viewport.
!
   CALL pgvstd
!
   pgopen=pgid
   RETURN
!
END FUNCTION pgopen
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPAGE -- advance to new page
!%void cpgpage(void);
!+
SUBROUTINE pgpage
!
! Advance plotter to a new page or panel, clearing the screen if
! necessary. If the "prompt state" is ON (see PGASK), confirmation is
! requested from the user before clearing the screen. If the view
! surface has been subdivided into panels with PGBEG or PGSUBP, then
! PGPAGE advances to the next panel, and if the current panel is the
! last on the page, PGPAGE clears the screen or starts a new sheet of
! paper.  PGPAGE does not change the PGPLOT window or the viewport
! (in normalized device coordinates); but note that if the size of the
! view-surface is changed externally (e.g., by a workstation window
! manager) the size of the viewport is changed in proportion.
!
! Arguments: none
!--
!  7-Feb-1983
! 23-Sep-1984 - correct bug: call GRTERM at end (if flush mode set).
! 31-Jan-1985 - make closer to Fortran-77.
! 19-Nov-1987 - explicitly clear the screen if device is interactive;
!               this restores the behavior obtained with older versions
!               of GRPCKG.
!  9-Feb-1988 - move prompting into routine GRPROM.
! 11-Apr-1989 - change name to PGPAGE.
! 10-Sep-1990 - add identification labelling.
! 11-Feb-1992 - check if device size has changed.
!  3-Sep-1992 - allow column ordering of panels.
! 17-Nov-1994 - move identification to drivers.
! 23-Nov-1994 - fix bug: character size not getting reset.
! 23-Jan-1995 - rescale viewport if size of view surface  has changed.
!  4-Feb-1997 - bug fix; character size was not correctly indexed by
!               device ID.
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   CHARACTER (LEN=16) :: str
   LOGICAL :: inter,pgnoto
   REAL(KIND=pg) :: dum1,dum2,xs,ys,xvp1,xvp2,yvp1,yvp2
!
   IF(pgnoto('PGPAGE'))RETURN
!
   IF(pgrows(pgid))THEN
      pgnxc(pgid)=pgnxc(pgid)+1
      IF(pgnxc(pgid) > pgnx(pgid))THEN
         pgnxc(pgid)=1
         pgnyc(pgid)=pgnyc(pgid)+1
         IF(pgnyc(pgid) > pgny(pgid))pgnyc(pgid)=1
      END IF
   ELSE
      pgnyc(pgid)=pgnyc(pgid)+1
      IF(pgnyc(pgid) > pgny(pgid))THEN
         pgnyc(pgid)=1
         pgnxc(pgid)=pgnxc(pgid)+1
         IF(pgnxc(pgid) > pgnx(pgid))pgnxc(pgid)=1
      END IF
   END IF
   IF(pgnxc(pgid) == 1.AND.pgnyc(pgid) == 1)THEN
      IF(pgadvs(pgid) == 1.AND.pgprmp(pgid))THEN
         CALL grterm
         CALL grprom
      END IF
      CALL grpage
      IF(.NOT.pgpfix(pgid))THEN
!
!             -- Get current viewport in NDC.
!
         CALL pgqvp(0,xvp1,xvp2,yvp1,yvp2)
!
!             -- Reset view surface size if it has changed
!
         CALL grsize(pgid,xs,ys,dum1,dum2,pgxpin(pgid), pgypin(pgid))
         pgxsz(pgid)=xs/pgnx(pgid)
         pgysz(pgid)=ys/pgny(pgid)
!
!             -- and character size
!
         CALL pgsch(pgchsz(pgid))
!
!             -- and viewport
!
         CALL pgsvp(xvp1,xvp2,yvp1,yvp2)
      END IF
!
! If the device is interactive, call GRBPIC to clear the page.
! (If the device is not interactive, GRBPIC will be called
! automatically before the first output; omitting the call here
! ensures that a blank page is not output.)
!
      CALL grqtyp(str,inter)
      IF(inter)CALL grbpic
   END IF
   pgxoff(pgid)=pgxvp(pgid)+(pgnxc(pgid)-1)*pgxsz(pgid)
   pgyoff(pgid)=pgyvp(pgid)+(pgny(pgid)-pgnyc(pgid))*pgysz(pgid)
!
! Window the plot in the new viewport.
!
   CALL pgvw
   pgadvs(pgid)=1
   CALL grterm
!
   RETURN
!
END SUBROUTINE pgpage
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPANL -- switch to a different panel on the view surface
!%void cpgpanl(int nxc, int nyc);
!+
SUBROUTINE pgpanl(ix,iy)
!
! Start plotting in a different panel. If the view surface has been
! divided into panels by PGBEG or PGSUBP, this routine can be used to
! move to a different panel. Note that PGPLOT does not remember what
! viewport and window were in use in each panel; these should be reset
! if necessary after calling PGPANL. Nor does PGPLOT clear the panel:
! call PGERAS after calling PGPANL to do this.
!
! Arguments:
!  IX     (input)  : the horizontal index of the panel (in the range
!                    1 <= IX <= number of panels in horizontal
!                    direction).
!  IY     (input)  : the vertical index of the panel (in the range
!                    1 <= IY <= number of panels in horizontal
!                    direction).
!--
!  1-Dec-1994 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ix
   INTEGER, INTENT(IN)                      :: iy
!
   LOGICAL :: pgnoto
!
! Check that a device is open.
!
   IF(pgnoto('PGPANL'))RETURN
!
! Check arguments.
!
   IF(ix < 1.OR.ix > pgnx(pgid).OR.iy < 1.OR.iy > pgny(pgid)) THEN
      CALL grwarn('PGPANL: the requested panel does not exist')
!
! Adjust the viewport to the new panel and window the plot
! in the new viewport.
!
   ELSE
      pgnxc(pgid)=ix
      pgnyc(pgid)=iy
      pgxoff(pgid)=pgxvp(pgid)+(ix-1)*pgxsz(pgid)
      pgyoff(pgid)=pgyvp(pgid)+(pgny(pgid)-iy)*pgysz(pgid)
      CALL pgvw
   END IF
!
   RETURN
!
END SUBROUTINE pgpanl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPAP -- change the size of the view surface
!%void cpgpap(float width, float aspect);
!+
SUBROUTINE pgpap(width,aspect)
!
! This routine changes the size of the view surface ("paper size") to a
! specified width and aspect ratio (height/width), in so far as this is
! possible on the specific device. It is always possible to obtain a
! view surface smaller than the default size; on some devices (e.g.,
! printers that print on roll or fan-feed paper) it is possible to
! obtain a view surface larger than the default.
!
! This routine should be called either immediately after PGBEG or
! immediately before PGPAGE. The new size applies to all subsequent
! images until the next call to PGPAP.
!
! Arguments:
!  WIDTH  (input)  : the requested width of the view surface in inches;
!                    if WIDTH=0.0, PGPAP will obtain the largest view
!                    surface available consistent with argument ASPECT.
!                    (1 inch = 25.4 mm.)
!  ASPECT (input)  : the aspect ratio (height/width) of the view
!                    surface; e.g., ASPECT=1.0 gives a square view
!                    surface, ASPECT=0.618 gives a horizontal
!                    rectangle, ASPECT=1.618 gives a vertical rectangle.
!--
! (22-Apr-1983; bug fixed 7-Jun-1988)
!  6-Oct-1990 Modified to work correctly on interactive devices.
! 13-Dec-1990 Make errors non-fatal [TJP].
! 14-Sep-1994 Fix bug to do with drivers changing view surface size.
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: width
   REAL(KIND=pg), INTENT(IN)                         :: aspect
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: hdef,hmax,hreq,wdef,wmax,wreq
   REAL(KIND=pg) :: xsmax,ysmax,xsz,ysz
!
   IF(pgnoto('PGPAP'))RETURN
   IF((width < 0.0_pg).OR.(aspect <= 0.0_pg))THEN
      CALL grwarn('PGPAP ignored: invalid arguments')
      RETURN
   END IF
!
   pgpfix(pgid)=.true.
!
!     -- Find default size WDEF, HDEF and maximum size WMAX, HMAX
!        of view surface (inches)
!
   CALL grsize(pgid,xsz,ysz,xsmax,ysmax,pgxpin(pgid),pgypin(pgid))
   wdef=xsz/pgxpin(pgid)
   hdef=ysz/pgypin(pgid)
   wmax=xsmax/pgxpin(pgid)
   hmax=ysmax/pgypin(pgid)
!
!     -- Find desired size WREQ, HREQ of view surface (inches)
!
   IF(ABS(width) > EPSILON(width))THEN
      wreq=width
      hreq=width*aspect
   ELSE
      wreq=wdef
      hreq=wdef*aspect
      IF(hreq > hdef)THEN
         wreq=hdef/aspect
         hreq=hdef
      END IF
   END IF
!
!     -- Scale the requested view surface to fit the maximum
!        dimensions
!
   IF((wmax > 0.0_pg).AND.(wreq > wmax))THEN
      wreq=wmax
      hreq=wmax*aspect
   END IF
   IF((hmax > 0.0_pg).AND.(hreq > hmax))THEN
      wreq=hmax/aspect
      hreq=hmax
   END IF
!
!     -- Establish the new view surface dimensions
!
   xsz=wreq*pgxpin(pgid)
   ysz=hreq*pgypin(pgid)
   CALL grsets(pgid,xsz,ysz)
   pgxsz(pgid)=xsz/pgnx(pgid)
   pgysz(pgid)=ysz/pgny(pgid)
   pgnxc(pgid)=pgnx(pgid)
   pgnyc(pgid)=pgny(pgid)
   CALL pgsch(1.0_pg)
   CALL pgvstd
!
   RETURN
!
END SUBROUTINE pgpap
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPAPER -- non-standard alias for PGPAP
!+
SUBROUTINE pgpaper(width,aspect)
!
   USE accur
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)              :: width
   REAL(KIND=pg), INTENT(IN OUT)              :: aspect
!
! See description of PGPAP.
!--
   CALL pgpap(width,aspect)
!
END SUBROUTINE pgpaper
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPIXL -- draw pixels
!%void cpgpixl(const int *ia, int idim, int jdim, int i1, int i2, \
!% int j1, int j2, float x1, float x2, float y1, float y2);
!+
SUBROUTINE pgpixl(ia,idim,jdim,i1,i2,j1,j2,x1,x2,y1,y2)
!
! Draw lots of solid-filled (tiny) rectangles aligned with the
! coordinate axes. Best performance is achieved when output is
! directed to a pixel-oriented device and the rectangles coincide
! with the pixels on the device. In other cases, pixel output is
! emulated.
!
! The subsection of the array IA defined by indices (I1:I2, J1:J2)
! is mapped onto world-coordinate rectangle defined by X1, X2, Y1
! and Y2. This rectangle is divided into (I2 - I1 + 1) * (J2 - J1 + 1)
! small rectangles. Each of these small rectangles is solid-filled
! with the color index specified by the corresponding element of
! IA.
!
! On most devices, the output region is "opaque", i.e., it obscures
! all graphical elements previously drawn in the region. But on
! devices that do not have erase capability, the background shade
! is "transparent" and allows previously-drawn graphics to show
! through.
!
! Arguments:
!  IA     (input)  : the array to be plotted.
!  IDIM   (input)  : the first dimension of array A.
!  JDIM   (input)  : the second dimension of array A.
!  I1, I2 (input)  : the inclusive range of the first index
!                    (I) to be plotted.
!  J1, J2 (input)  : the inclusive range of the second
!                    index (J) to be plotted.
!  X1, Y1 (input)  : world coordinates of one corner of the output
!                    region
!  X2, Y2 (input)  : world coordinates of the opposite corner of the
!                    output region
!--
! 16-Jan-1991 - [GvG]
!-----------------------------------------------------------------------
!
   USE accur
!
   INTEGER, INTENT(IN)                  :: idim
   INTEGER, INTENT(IN)                  :: jdim
   INTEGER, INTENT(IN)                  :: ia(idim,jdim)
   INTEGER, INTENT(IN)                  :: i1
   INTEGER, INTENT(IN)                  :: i2
   INTEGER, INTENT(IN)                  :: j1
   INTEGER, INTENT(IN)                  :: j2
   REAL(KIND=pg), INTENT(IN)            :: x1
   REAL(KIND=pg), INTENT(IN)            :: x2
   REAL(KIND=pg), INTENT(IN)            :: y1
   REAL(KIND=pg), INTENT(IN)            :: y2
!
   LOGICAL :: pgnoto
!
! Check inputs.
!
   IF(pgnoto('PGPIXL'))RETURN
   IF(i1 < 1.OR.i2 > idim.OR.i1 > i2.OR.j1 < 1.OR.j2 > jdim.OR.j1 > j2)THEN
      CALL grwarn('PGPIXL: invalid range I1:I2, J1:J2')
   ELSE
!
! Call lower-level routine to do the work.
!
      CALL pgbbuf
      CALL grpixl(ia,idim,jdim,i1,i2,j1,j2,x1,x2,y1,y2)
      CALL pgebuf
   END IF
!
   RETURN
!
END SUBROUTINE pgpixl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPNTS -- draw several graph markers, not all the same
!%void cpgpnts(int n, const float *x, const float *y, \
!% const int *symbol, int ns);
!+
SUBROUTINE pgpnts(n,x,y,symbol,ns)
!
! Draw Graph Markers. Unlike PGPT, this routine can draw a different
! symbol at each point. The markers are drawn using the current values
! of attributes color-index, line-width, and character-height
! (character-font applies if the symbol number is >31).  If the point
! to be marked lies outside the window, no marker is drawn.  The "pen
! position" is changed to (XPTS(N),YPTS(N)) in world coordinates
! (if N > 0).
!
! Arguments:
!  N      (input)  : number of points to mark.
!  X      (input)  : world x-coordinate of the points.
!  Y      (input)  : world y-coordinate of the points.
!  SYMBOL (input)  : code number of the symbol to be plotted at each
!                    point (see PGPT).
!  NS     (input)  : number of values in the SYMBOL array.  If NS <= N,
!                    then the first NS points are drawn using the value
!                    of SYMBOL(I) at (X(I), Y(I)) and SYMBOL(1) for all
!                    the values of (X(I), Y(I)) where I > NS.
!
! Note: the dimension of arrays X and Y must be greater than or equal
! to N and the dimension of the array SYMBOL must be greater than or
! equal to NS.  If N is 1, X and Y may be scalars (constants or
! variables).  If NS is 1, then SYMBOL may be a scalar.  If N is
! less than 1, nothing is drawn.
!--
! 11-Mar-1991 - new routine [JM].
! 26-Feb-1997 - revised to use PGPT1 [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   INTEGER, INTENT(IN)                  :: n
   REAL(KIND=pg), INTENT(IN)            :: x(*)
   REAL(KIND=pg), INTENT(IN)            :: y(*)
   INTEGER, INTENT(IN)                  :: symbol(*)
   INTEGER, INTENT(IN)                  :: ns
!
   INTEGER :: i,symb
!
   IF(n < 1)RETURN
   CALL pgbbuf
   DO  i=1,n
      IF(i <= ns)THEN
         symb=symbol(i)
      ELSE
         symb=symbol(1)
      END IF
      CALL pgpt1(x(i),y(i),symb)
   END DO
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgpnts
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPOINT -- non-standard alias for PGPT
!+
SUBROUTINE pgpoint(n,xpts,ypts,symbol)
!
! See description of PGPT.
!--
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: n
   REAL(KIND=pg), INTENT(IN)            :: xpts(*)
   REAL(KIND=pg), INTENT(IN)            :: ypts(*)
   INTEGER, INTENT(IN)                  :: symbol
!
   CALL pgpt(n,xpts,ypts,symbol)
!
   RETURN
!
END SUBROUTINE pgpoint
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPOLY -- draw a polygon, using fill-area attributes
!%void cpgpoly(int n, const float *xpts, const float *ypts);
!+
SUBROUTINE pgpoly(n,xpts,ypts)
!
! Fill-area primitive routine: shade the interior of a closed
! polygon in the current window.  The action of this routine depends
! on the setting of the Fill-Area Style attribute (see PGSFS).
! The polygon is clipped at the edge of the
! window. The pen position is changed to (XPTS(1),YPTS(1)) in world
! coordinates (if N > 1).  If the polygon is not convex, a point is
! assumed to lie inside the polygon if a straight line drawn to
! infinity intersects and odd number of the polygon's edges.
!
! Arguments:
!  N      (input)  : number of points defining the polygon; the
!                    line consists of N straight-line segments,
!                    joining points 1 to 2, 2 to 3,... N-1 to N, N to 1.
!                    N should be greater than 2 (if it is 2 or less,
!                    nothing will be drawn).
!  XPTS   (input)  : world x-coordinates of the vertices.
!  YPTS   (input)  : world y-coordinates of the vertices.
!                    Note: the dimension of arrays XPTS and YPTS must be
!                    greater than or equal to N.
!--
! 21-Nov-1983 - [TJP].
! 16-Jul-1984 - revised to shade polygon with GRFA [TJP].
! 21-Oct-1985 - test PGFAS [TJP].
! 25-Nov-1994 - implement clipping [TJP].
! 13-Jan-1994 - fix bug in clipping [TJP].
!  6-Mar-1995 - add support for fill styles 3 and 4 [TJP].
! 12-Sep-1995 - fix another bug in clipping [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   REAL(KIND=pg), INTENT(IN OUT)            :: xpts(*)
   REAL(KIND=pg), INTENT(IN OUT)            :: ypts(*)
!
   INTEGER, PARAMETER :: maxout=1000
   LOGICAL :: clip
   INTEGER :: i,n1,n2,n3,n4
   REAL(KIND=pg) :: qx(maxout),qy(maxout),rx(maxout),ry(maxout)
   REAL(KIND=pg) :: xl,xh,yl,yh
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGPOLY'))RETURN
   IF(n < 1)RETURN
!
! Outline style, or polygon of less than 3 vertices.
!
   IF(pgfas(pgid) == 2.OR.n < 3)THEN
      CALL pgbbuf
      CALL grmova(xpts(n),ypts(n))
      DO  i=1,n
         CALL grlina(xpts(i),ypts(i))
      END DO
!
! Hatched style.
!
   ELSE IF(pgfas(pgid) == 3)THEN
      CALL pgbbuf
      CALL pghtch(n,xpts,ypts,0.0_pg)
   ELSE IF(pgfas(pgid) == 4)THEN
      CALL pgbbuf
      CALL pghtch(n,xpts,ypts,0.0_pg)
      CALL pghtch(n,xpts,ypts,90.0_pg)
   ELSE
!
! Test whether polygon lies completely in the window.
!
      clip=.false.
      xl=MIN(pgxblc(pgid),pgxtrc(pgid))
      xh=MAX(pgxblc(pgid),pgxtrc(pgid))
      yl=MIN(pgyblc(pgid),pgytrc(pgid))
      yh=MAX(pgyblc(pgid),pgytrc(pgid))
      DO  i=1,n
         IF(xpts(i) < xl.OR.xpts(i) > xh.OR.ypts(i) < yl.OR. ypts(i) > yh)THEN
            clip=.true.
            GO TO 30
         END IF
      END DO
30    CONTINUE
!
! Filled style, no clipping required.
!
      CALL pgbbuf
      IF(.NOT.clip)THEN
         CALL grfa(n,xpts,ypts)
!
! Filled style, clipping required: the vertices of the clipped
! polygon are put in temporary arrays QX,QY, RX, RY.
!
      ELSE
         CALL grpocl(n,xpts,ypts,1,xl,maxout,n1,qx,qy)
         IF(n1 > maxout)GO TO 40
         IF(n1 < 3)GO TO 50
         CALL grpocl(n1,qx,qy,2,xh,maxout,n2,rx,ry)
         IF(n2 > maxout)GO TO 40
         IF(n2 < 3)GO TO 50
         CALL grpocl(n2,rx,ry,3,yl,maxout,n3,qx,qy)
         IF(n3 > maxout)GO TO 40
         IF(n3 < 3)GO TO 50
         CALL grpocl(n3,qx,qy,4,yh,maxout,n4,rx,ry)
         IF(n4 > maxout)GO TO 40
         IF(n4 > 0)CALL grfa(n4,rx,ry)
         GO TO 50
40       CALL grwarn('PGPOLY: polygon is too complex')
50       CONTINUE
      END IF
   END IF
!
! Set the current pen position.
!
   CALL grmova(xpts(1),ypts(1))
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgpoly
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPT -- draw several graph markers
!%void cpgpt(int n, const float *xpts, const float *ypts, int symbol);
!+
SUBROUTINE pgpt(n,xpts,ypts,symbol)
!
! Primitive routine to draw Graph Markers (polymarker). The markers
! are drawn using the current values of attributes color-index,
! line-width, and character-height (character-font applies if the symbol
! number is >31).  If the point to be marked lies outside the window,
! no marker is drawn.  The "pen position" is changed to
! (XPTS(N),YPTS(N)) in world coordinates (if N > 0).
!
! Arguments:
!  N      (input)  : number of points to mark.
!  XPTS   (input)  : world x-coordinates of the points.
!  YPTS   (input)  : world y-coordinates of the points.
!  SYMBOL (input)  : code number of the symbol to be drawn at each
!                    point:
!                    -1, -2  : a single dot (diameter = current
!                              line width).
!                    -3..-31 : a regular polygon with ABS(SYMBOL)
!                              edges (style set by current fill style).
!                    0..31   : standard marker symbols.
!                    32..127 : ASCII characters (in current font).
!                              e.g. to use letter F as a marker, let
!                              SYMBOL = ICHAR('F').
!                    > 127  :  a Hershey symbol number.
!
! Note: the dimension of arrays X and Y must be greater than or equal
! to N. If N is 1, X and Y may be scalars (constants or variables). If
! N is less than 1, nothing is drawn.
!--
! 27-Nov-1986
! 17-Dec-1990 - add polygons [PAH].
! 14-Mar-1997 - optimization: use GRDOT1 [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   INTEGER, INTENT(IN)                  :: n
   REAL(KIND=pg), INTENT(IN)            :: xpts(*)
   REAL(KIND=pg), INTENT(IN)            :: ypts(*)
   INTEGER, INTENT(IN)                  :: symbol
!
   LOGICAL :: pgnoto
!
   IF(n < 1)RETURN
   IF(pgnoto('PGPT'))RETURN
!
   CALL pgbbuf
   IF(symbol >= 0.OR.symbol <= -3)THEN
      CALL grmker(symbol,.false.,n,xpts,ypts)
   ELSE
      CALL grdot1(n,xpts,ypts)
   END IF
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgpt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPT1 -- draw one graph marker
!%void cpgpt1(float xpt, float ypt, int symbol);
!+
SUBROUTINE pgpt1(xpt,ypt,symbol)
!
! Primitive routine to draw a single Graph Marker at a specified point.
! The marker is drawn using the current values of attributes
! color-index, line-width, and character-height (character-font applies
! if the symbol number is >31).  If the point to be marked lies outside
! the window, no marker is drawn.  The "pen position" is changed to
! (XPT,YPT) in world coordinates.
!
! To draw several markers with coordinates specified by X and Y
! arrays, use routine PGPT.
!
! Arguments:
!  XPT    (input)  : world x-coordinate of the point.
!  YPT    (input)  : world y-coordinate of the point.
!  SYMBOL (input)  : code number of the symbol to be drawn:
!                    -1, -2  : a single dot (diameter = current
!                              line width).
!                    -3..-31 : a regular polygon with ABS(SYMBOL)
!                              edges (style set by current fill style).
!                    0..31   : standard marker symbols.
!                    32..127 : ASCII characters (in current font).
!                              e.g. to use letter F as a marker, let
!                              SYMBOL = ICHAR('F').
!                    > 127  :  a Hershey symbol number.
!--
!  4-Feb-1997 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: xpt
   REAL(KIND=pg), INTENT(IN)                :: ypt
   INTEGER, INTENT(IN)                      :: symbol
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xpts(1),ypts(1)
!
   IF(pgnoto('PGPT1'))RETURN
   xpts(1)=xpt
   ypts(1)=ypt
   CALL pgpt(1,xpts,ypts,symbol)
!
   RETURN
!
END SUBROUTINE pgpt1
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPTEXT -- non-standard alias for PGPTXT
!+
SUBROUTINE pgptext(x,y,angle,fjust,text)
!
! See description of PGPTXT.
!--
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)            :: x
   REAL(KIND=pg), INTENT(IN)            :: y
   REAL(KIND=pg), INTENT(IN)            :: angle
   REAL(KIND=pg), INTENT(IN)            :: fjust
   CHARACTER (LEN=*), INTENT(IN)        :: text
!
   CALL pgptxt(x,y,angle,fjust,text)
!
   RETURN
!
END SUBROUTINE pgptext
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGPTXT -- write text at arbitrary position and angle
!%void cpgptxt(float x, float y, float angle, float fjust, \
!% const char *text);
!+
SUBROUTINE pgptxt(x,y,angle,fjust,text)
!
! Primitive routine for drawing text. The text may be drawn at any
! angle with the horizontal, and may be centered or left- or right-
! justified at a specified position.  Routine PGTEXT provides a
! simple interface to PGPTXT for horizontal strings. Text is drawn
! using the current values of attributes color-index, line-width,
! character-height, and character-font.  Text is NOT subject to
! clipping at the edge of the window.
!
! Arguments:
!  X      (input)  : world x-coordinate.
!  Y      (input)  : world y-coordinate. The string is drawn with the
!                    baseline of all the characters passing through
!                    point (X,Y); the positioning of the string along
!                    this line is controlled by argument FJUST.
!  ANGLE  (input)  : angle, in degrees, that the baseline is to make
!                    with the horizontal, increasing counter-clockwise
!                    (0.0 is horizontal).
!  FJUST  (input)  : controls horizontal justification of the string.
!                    If FJUST = 0.0, the string will be left-justified
!                    at the point (X,Y); if FJUST = 0.5, it will be
!                    centered, and if FJUST = 1.0, it will be right
!                    justified. [Other values of FJUST give other
!                    justifications.]
!  TEXT   (input)  : the character string to be plotted.
!--
! (2-May-1983)
! 31-Jan-1985 - convert to Fortran-77 standard...
! 13-Feb-1988 - correct a PGBBUF/PGEBUF mismatch if string is blank.
! 16-Oct-1993 - erase background of opaque text.
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: x
   REAL(KIND=pg), INTENT(IN)                :: y
   REAL(KIND=pg), INTENT(IN)                :: angle
   REAL(KIND=pg), INTENT(IN)                :: fjust
   CHARACTER (LEN=*), INTENT(IN)            :: text
!
   INTEGER :: ci,i,l,grtrim
   REAL(KIND=pg) :: d,xp,yp
   REAL(KIND=pg) :: xbox(4),ybox(4)
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGPTXT'))RETURN
   CALL pgbbuf
!
   l=grtrim(text)
   d=0.0_pg
   IF(ABS(fjust) > EPSILON(fjust))CALL grlen(text(1:l),d)
   xp=pgxorg(pgid)+x*pgxscl(pgid)-d*fjust*COS(angle/57.295779513082321_pg)
   yp=pgyorg(pgid)+y*pgyscl(pgid)-d*fjust*SIN(angle/57.295779513082321_pg)
   IF(pgtbci(pgid) >= 0)THEN
      CALL grqtxt(angle,xp,yp,text(1:l),xbox,ybox)
      DO  i=1,4
         xbox(i)=(xbox(i)-pgxorg(pgid))/pgxscl(pgid)
         ybox(i)=(ybox(i)-pgyorg(pgid))/pgyscl(pgid)
      END DO
      CALL pgqci(ci)
      CALL pgsci(pgtbci(pgid))
      CALL grfa(4,xbox,ybox)
      CALL pgsci(ci)
   END IF
   CALL grtext(.true.,angle,.true.,xp,yp,text(1:l))
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgptxt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQAH -- inquire arrow-head style
!%void cpgqah(int *fs, float *angle, float *barb);
!+
SUBROUTINE pgqah(fs,angle,barb)
!
! Query the style to be used for arrowheads drawn with routine PGARRO.
!
! Argument:
!  FS     (output) : FS = 1 => filled; FS = 2 => outline.
!  ANGLE  (output) : the acute angle of the arrow point, in degrees.
!  BARB   (output) : the fraction of the triangular arrow-head that
!                    is cut away from the back.
!--
! 13-Oct-1992 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: fs
   REAL(KIND=pg), INTENT(OUT)               :: angle
   REAL(KIND=pg), INTENT(OUT)               :: barb
!
   fs=pgahs(pgid)
   angle=pgaha(pgid)
   barb=pgahv(pgid)
!
   RETURN
!
END SUBROUTINE pgqah
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQCF -- inquire character font
!%void cpgqcf(int *font);
!+
SUBROUTINE pgqcf (font)
!
! Query the current Character Font (set by routine PGSCF).
!
! Argument:
!  FONT   (output)   : the current font number (in range 1-4).
!--
!  5-Nov-1985 - new routine [TJP].
! 25-OCT-1993 - changed name of argument [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: font
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQCF'))THEN
      font=1
   ELSE
      CALL grqfnt(font)
   END IF
!
END SUBROUTINE pgqcf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQCH -- inquire character height
!%void cpgqch(float *size);
!+
SUBROUTINE pgqch(size)
!
! Query the Character Size attribute (set by routine PGSCH).
!
! Argument:
!  SIZE   (output) : current character size (dimensionless multiple of
!                    the default size).
!--
!  5-Nov-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(OUT)                        :: size
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQCH'))THEN
      size=1.0
   ELSE
      size=pgchsz(pgid)
   END IF
!
   RETURN
!
END SUBROUTINE pgqch
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQCI -- inquire color index
!%void cpgqci(int *ci);
!+
SUBROUTINE pgqci(ci)
!
! Query the Color Index attribute (set by routine PGSCI).
!
! Argument:
!  CI     (output) : the current color index (in range 0-max). This is
!                    the color index actually in use, and may differ
!                    from the color index last requested by PGSCI if
!                    that index is not available on the output device.
!--
!  5-Nov-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: ci
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQCI'))THEN
      ci=1
   ELSE
      CALL grqci(ci)
   END IF
!
   RETURN
!
END SUBROUTINE pgqci
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQCIR -- inquire color index range
!%void cpgqcir(int *icilo, int *icihi);
!+
SUBROUTINE pgqcir(icilo,icihi)
!
! Query the color index range to be used for producing images with
! PGGRAY or PGIMAG, as set by routine PGSCIR or by device default.
!
! Arguments:
!  ICILO  (output) : the lowest color index to use for images
!  ICIHI  (output) : the highest color index to use for images
!--
! 1994-Mar-17 : new routine [AFT/TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   INTEGER, INTENT(OUT)                     :: icilo
   INTEGER, INTENT(OUT)                     :: icihi
!
   icilo=pgmnci(pgid)
   icihi=pgmxci(pgid)
!
   RETURN
!
END SUBROUTINE pgqcir
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQCLP -- inquire clipping status
!%void cpgqclp(int *state);
!+
SUBROUTINE pgqclp(state)
!
! Query the current clipping status (set by routine PGSCLP).
!
! Argument:
!  STATE  (output) : receives the clipping status (0 => disabled,
!                    1 => enabled).
!--
! 25-Feb-1997 [TJP] - new routine.
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: state
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQCLP'))THEN
      state=1
   ELSE
      state=pgclp(pgid)
   END IF
!
   RETURN
!
END SUBROUTINE pgqclp
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQCOL -- inquire color capability
!%void cpgqcol(int *ci1, int *ci2);
!+
SUBROUTINE pgqcol(ci1,ci2)
!
! Query the range of color indices available on the current device.
!
! Argument:
!  CI1    (output) : the minimum available color index. This will be
!                    either 0 if the device can write in the
!                    background color, or 1 if not.
!  CI2    (output) : the maximum available color index. This will be
!                    1 if the device has no color capability, or a
!                    larger number (e.g., 3, 7, 15, 255).
!--
! 31-May-1989 - new routine [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: ci1
   INTEGER, INTENT(IN OUT)                  :: ci2
!
   CALL grqcol(ci1,ci2)
!
   RETURN
!
END SUBROUTINE pgqcol
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQCR  -- inquire color representation
!%void cpgqcr(int ci, float *cr, float *cg, float *cb);
!+
SUBROUTINE pgqcr(ci,cr,cg,cb)
!
! Query the RGB colors associated with a color index.
!
! Arguments:
!  CI  (input)  : color index
!  CR  (output) : red, green and blue intensities
!  CG  (output)   in the range 0.0 to 1.0
!  CB  (output)
!--
! 7-Apr-1992 - new routine [DLT]
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: ci
   REAL(KIND=pg), INTENT(OUT)           :: cr
   REAL(KIND=pg), INTENT(OUT)           :: cg
   REAL(KIND=pg), INTENT(OUT)           :: cb
!
   CALL grqcr(ci,cr,cg,cb)
!
END SUBROUTINE pgqcr
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQCS  -- inquire character height in a variety of units
!%void cpgqcs(int units, float *xch, float *ych);
!+
SUBROUTINE pgqcs(units,xch,ych)
!
! Return the current PGPLOT character height in a variety of units.
! This routine provides facilities that are not available via PGQCH.
! Use PGQCS if the character height is required in units other than
! those used in PGSCH.
!
! The PGPLOT "character height" is a dimension that scales with the
! size of the view surface and with the scale-factor specified with
! routine PGSCH. The default value is 1/40th of the height or width
! of the view surface (whichever is less); this value is then
! multiplied by the scale-factor supplied with PGSCH. Note that it
! is a nominal height only; the actual character size depends on the
! font and is usually somewhat smaller.
!
! Arguments:
!  UNITS  (input)  : Used to specify the units of the output value:
!                    UNITS = 0 : normalized device coordinates
!                    UNITS = 1 : inches
!                    UNITS = 2 : millimeters
!                    UNITS = 3 : pixels
!                    UNITS = 4 : world coordinates
!                    Other values give an error message, and are
!                    treated as 0.
!  XCH    (output) : The character height for text written with a
!                    vertical baseline.
!  YCH    (output) : The character height for text written with
!                    a horizontal baseline (the usual case).
!
! The character height is returned in both XCH and YCH.
!
! If UNITS=1 or UNITS=2, XCH and YCH both receive the same value.
!
! If UNITS=3, XCH receives the height in horizontal pixel units, and YCH
! receives the height in vertical pixel units; on devices for which the
! pixels are not square, XCH and YCH will be different.
!
! If UNITS=4, XCH receives the height in horizontal world coordinates
! (as used for the x-axis), and YCH receives the height in vertical
! world coordinates (as used for the y-axis). Unless special care has
! been taken to achive equal world-coordinate scales on both axes, the
! values of XCH and YCH will be different.
!
! If UNITS=0, XCH receives the character height as a fraction of the
! horizontal dimension of the view surface, and YCH receives the
! character height as a fraction of the vertical dimension of the view
! surface.
!--
! 15-Oct-1992 - new routine [MCS].
!  4-Dec-1992 - added more explanation [TJP].
!  5-Sep-1995 - add UNITS=4; correct error for non-square pixels [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: units
   REAL(KIND=pg), INTENT(OUT)               :: xch
   REAL(KIND=pg), INTENT(OUT)               :: ych
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: ratio
!                       Conversion factor inches -> mm
   REAL(KIND=pg), PARAMETER :: intomm=25.4_pg
!
   IF(pgnoto('PGQCS'))RETURN
   ratio=pgypin(pgid)/pgxpin(pgid)
!
! Return the character height in the required units.
!
!                                        Inches.
   IF(units == 1)THEN
      xch=pgysp(pgid)/pgxpin(pgid)
      ych=xch
!                                        Millimeters.
   ELSE IF(units == 2)THEN
      xch=pgysp(pgid)/pgxpin(pgid)*intomm
      ych=xch
!
!                                        Pixels.
!
   ELSE IF(units == 3)THEN
      xch=pgysp(pgid)
      ych=pgysp(pgid)*ratio
!
!                                        World coordinates.
!
   ELSE IF(units == 4)THEN
      xch=pgysp(pgid)/pgxscl(pgid)
      ych=pgysp(pgid)*ratio/pgyscl(pgid)
!
!                                        Normalized device coords, or
!                                        unknown.
!
   ELSE
      xch=pgysp(pgid)/pgxsz(pgid)
      ych=pgysp(pgid)*ratio/pgysz(pgid)
      IF(units /= 0)CALL grwarn('Invalid "UNITS" argument in PGQCS.')
   END IF
!
   RETURN
!
END SUBROUTINE pgqcs
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQDT -- inquire name of nth available device type
!%void cpgqdt(int n, char *type, int *type_length, char *descr, \
!% int *descr_length, int *inter);
!+
SUBROUTINE pgqdt(n,TYPE,tlen,descr,dlen,inter)
!
! Return the name of the Nth available device type as a character
! string. The number of available types can be determined by calling
! PGQNDT. If the value of N supplied is outside the range from 1 to
! the number of available types, the routine returns DLEN=TLEN=0.
!
! Arguments:
!  N      (input)  : the number of the device type (1..maximum).
!  TYPE   (output) : receives the character device-type code of the
!                    Nth device type. The argument supplied should be
!                    large enough for at least 8 characters. The first
!                    character in the string is a '/' character.
!  TLEN   (output) : receives the number of characters in TYPE,
!                    excluding trailing blanks.
!  DESCR  (output) : receives a description of the device type. The
!                    argument supplied should be large enough for at
!                    least 64 characters.
!  DLEN   (output) : receives the number of characters in DESCR,
!                    excluding trailing blanks.
!  INTER  (output) : receives 1 if the device type is an interactive
!                    one, 0 otherwise.
!--
! 17-Mar-1997 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: n
   CHARACTER (LEN=*), INTENT(OUT)           :: TYPE
   INTEGER, INTENT(OUT)                     :: tlen
   CHARACTER (LEN=*), INTENT(OUT)           :: descr
   INTEGER, INTENT(OUT)                     :: dlen
   INTEGER, INTENT(OUT)                     :: inter
!
   INTEGER :: ndev,nbuf,lchr,l1,l2
   REAL(KIND=pg) :: rbuf(1)
   CHARACTER (LEN=80) :: chr
!
! Initialize PGPLOT if necessary.
!
   CALL pginit
!
   TYPE='error'
   tlen=0
   descr=' '
   dlen=0
   inter=1
   CALL pgqndt(ndev)
   IF(n >= 1.AND.n <= ndev)THEN
      nbuf=0
      CALL grexec(n,1,rbuf,nbuf,chr,lchr)
      IF(lchr > 0)THEN
         l1=INDEX(chr(1:lchr),' ')
         IF(l1 > 1)THEN
            TYPE(1:1)='/'
            IF(LEN(TYPE) > 1)TYPE(2:)=chr(1:l1-1)
            tlen=MIN(l1,LEN(TYPE))
         END IF
         l2=INDEX(chr(1:lchr),'(')
         IF(l2 > 0)descr=chr(l2:lchr)
         dlen=MIN(lchr-l2+1,LEN(descr))
         CALL grexec(n,4,rbuf,nbuf,chr,lchr)
         IF(chr(1:1) == 'H')inter=0
      END IF
   END IF
!
   RETURN
!
END SUBROUTINE pgqdt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQFS -- inquire fill-area style
!%void cpgqfs(int *fs);
!+
SUBROUTINE pgqfs(fs)
!
! Query the current Fill-Area Style attribute (set by routine
! PGSFS).
!
! Argument:
!  FS     (output) : the current fill-area style:
!                      FS = 1 => solid (default)
!                      FS = 2 => outline
!                      FS = 3 => hatched
!                      FS = 4 => cross-hatched
!--
!  5-Nov-1985 - new routine [TJP].
!  6-Mar-1995 - add styles 3 and 4 [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: fs
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQFS'))THEN
      fs=1
   ELSE
      fs=pgfas(pgid)
   END IF
!
   RETURN
!
END SUBROUTINE pgqfs
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQHS -- inquire hatching style
!%void cpgqhs(float *angle, float *sepn, float* phase);
!+
SUBROUTINE pgqhs(angle,sepn,phase)
!
! Query the style to be used hatching (fill area with fill-style 3).
!
! Arguments:
!  ANGLE  (output) : the angle the hatch lines make with the
!                    horizontal, in degrees, increasing
!                    counterclockwise (this is an angle on the
!                    view surface, not in world-coordinate space).
!  SEPN   (output) : the spacing of the hatch lines. The unit spacing
!                    is 1 percent of the smaller of the height or
!                    width of the view surface.
!  PHASE  (output) : a real number between 0 and 1; the hatch lines
!                    are displaced by this fraction of SEPN from a
!                    fixed reference.  Adjacent regions hatched with the
!                    same PHASE have contiguous hatch lines.
!--
! 26-Feb-1995 - new routine [TJP].
! 19-Jun-1995 - correct synopsis [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(OUT)               :: angle
   REAL(KIND=pg), INTENT(OUT)               :: sepn
   REAL(KIND=pg), INTENT(OUT)               :: phase
!
   angle=pghsa(pgid)
   sepn=pghss(pgid)
   phase=pghsp(pgid)
!
   RETURN
!
END SUBROUTINE pgqhs
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQID -- inquire current device identifier
!%void cpgqid(int *id);
!+
SUBROUTINE pgqid(id)
!
! This subroutine returns the identifier of the currently
! selected device, or 0 if no device is selected.  The identifier is
! assigned when PGOPEN is called to open the device, and may be used
! as an argument to PGSLCT.  Each open device has a different
! identifier.
!
! [This routine was added to PGPLOT in Version 5.1.0.]
!
! Argument:
!  ID     (output) : the identifier of the current device, or 0 if
!                    no device is currently selected.
!--
! 22-Dec-1995 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: id
!
   id=pgid
!
   RETURN
!
END SUBROUTINE pgqid
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQINF -- inquire PGPLOT general information
!%void cpgqinf(const char *item, char *value, int *value_length);
!+
SUBROUTINE pgqinf(item,value,length)
!
! This routine can be used to obtain miscellaneous information about
! the PGPLOT environment. Input is a character string defining the
! information required, and output is a character string containing the
! requested information.
!
! The following item codes are accepted (note that the strings must
! match exactly, except for case, but only the first 8 characters are
! significant). For items marked *, PGPLOT must be in the OPEN state
! for the inquiry to succeed. If the inquiry is unsuccessful, either
! because the item code is not recognized or because the information
! is not available, a question mark ('?') is returned.
!
!   'VERSION'     - version of PGPLOT software in use.
!   'STATE'       - status of PGPLOT ('OPEN' if a graphics device
!                   is open for output, 'CLOSED' otherwise).
!   'USER'        - the username associated with the calling program.
!   'NOW'         - current date and time (e.g., '17-FEB-1986 10:04').
!   'DEVICE'    * - current PGPLOT device or file.
!   'FILE'      * - current PGPLOT device or file.
!   'TYPE'      * - device-type of the current PGPLOT device.
!   'DEV/TYPE'  * - current PGPLOT device and type, in a form which
!                   is acceptable as an argument for PGBEG.
!   'HARDCOPY'  * - is the current device a hardcopy device? ('YES' or
!                   'NO').
!   'TERMINAL'  * - is the current device the user's interactive
!                   terminal? ('YES' or 'NO').
!   'CURSOR'    * - does the current device have a graphics cursor?
!                   ('YES' or 'NO').
!   'SCROLL'    * - does current device have rectangle-scroll
!                   capability ('YES' or 'NO'); see PGSCRL.
!
! Arguments:
!  ITEM  (input)  : character string defining the information to
!                   be returned; see above for a list of possible
!                   values.
!  VALUE (output) : returns a character-string containing the
!                   requested information, truncated to the length
!                   of the supplied string or padded on the right with
!                   spaces if necessary.
!  LENGTH (output): the number of characters returned in VALUE
!                   (excluding trailing blanks).
!--
! 18-Feb-1988 - [TJP].
! 30-Aug-1988 - remove pseudo logical use of IER.
! 12-Mar-1992 - change comments for clarity.
! 17-Apr-1995 - clean up some zero-length string problems [TJP].
!  7-Jul-1995 - get cursor information directly from driver [TJP].
! 24-Feb-1997 - add SCROLL request.
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: item
   CHARACTER (LEN=*), INTENT(OUT)           :: value
   INTEGER, INTENT(OUT)                     :: length
!
   INTEGER :: ier,l1,grtrim
   LOGICAL :: inter,same
   CHARACTER (LEN=8) :: test
   CHARACTER (LEN=64) :: dev1
!
! Initialize PGPLOT if necessary.
!
   CALL pginit
!
   CALL grtoup(test,item)
   IF(test == 'USER')THEN
      CALL gruser(value,length)
      ier=1
   ELSE IF(test == 'NOW')THEN
      CALL grdate(value,length)
      ier=1
   ELSE IF(test == 'VERSION')THEN
      value='v5.2.2'
      length=6
      ier=1
   ELSE IF(test == 'STATE')THEN
      IF(pgid < 1.OR.pgid > pgmaxd)THEN
         value='CLOSED'
         length=6
      ELSE IF(pgdevs(pgid) == 0)THEN
         value='CLOSED'
         length=6
      ELSE
         value='OPEN'
         length=4
      END IF
      ier=1
   ELSE IF(pgid < 1.OR.pgid > pgmaxd)THEN
      ier=0
   ELSE IF(pgdevs(pgid) == 0)THEN
      ier=0
   ELSE IF(test == 'DEV/TYPE')THEN
      CALL grqdt(value)
      length=grtrim(value)
      ier=0
      IF(length > 0)ier=1
   ELSE IF(test == 'DEVICE'.OR.test == 'FILE')THEN
      CALL grqdev(value,length)
      ier=1
   ELSE IF(test == 'TERMINAL')THEN
      CALL grqdev(dev1,l1)
      IF(l1 >= 1)THEN
         CALL grtter(dev1(1:l1),same)
      ELSE
         same=.false.
      END IF
      IF(same)THEN
         value='YES'
         length=3
      ELSE
         value='NO'
         length=2
      END IF
      ier=1
   ELSE IF(test == 'TYPE')THEN
      CALL grqtyp(value,inter)
      length=grtrim(value)
      ier=0
      IF(length > 0)ier=1
   ELSE IF(test == 'HARDCOPY')THEN
      CALL grqtyp(value,inter)
      IF(inter)THEN
         value='NO'
         length=2
      ELSE
         value='YES'
         length=3
      END IF
      ier=1
   ELSE IF(test == 'CURSOR')THEN
      CALL grqcap(dev1)
      IF(dev1(2:2) == 'N')THEN
         value='NO'
         length=2
      ELSE
         value='YES'
         length=3
      END IF
      ier=1
   ELSE IF(test == 'SCROLL')THEN
      CALL grqcap(dev1)
      IF(dev1(11:11) /= 'S')THEN
         value='NO'
         length=2
      ELSE
         value='YES'
         length=3
      END IF
      ier=1
   ELSE
      ier=0
   END IF
   IF(ier /= 1)THEN
      value='?'
      length=1
   ELSE IF(length < 1)THEN
      length=1
      value=' '
   END IF
!
   RETURN
!
END SUBROUTINE pgqinf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQITF -- inquire image transfer function
!%void cpgqitf(int *itf);
!+
SUBROUTINE pgqitf(itf)
!
! Return the Image Transfer Function as set by default or by a previous
! call to PGSITF. The Image Transfer Function is used by routines
! PGIMAG, PGGRAY, and PGWEDG.
!
! Argument:
!  ITF    (output) : type of transfer function (see PGSITF)
!--
! 15-Sep-1994 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: itf
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQITF'))THEN
      itf=0
   ELSE
      itf=pgitf(pgid)
   END IF
!
   RETURN
!
END SUBROUTINE pgqitf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQLS -- inquire line style
!%void cpgqls(int *ls);
!+
SUBROUTINE pgqls(ls)
!
! Query the current Line Style attribute (set by routine PGSLS).
!
! Argument:
!  LS     (output) : the current line-style attribute (in range 1-5).
!--
!  5-Nov-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: ls
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQLS'))THEN
      ls=1
   ELSE
      CALL grqls(ls)
   END IF
!
   RETURN
!
END SUBROUTINE pgqls
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQLW -- inquire line width
!%void cpgqlw(int *lw);
!+
SUBROUTINE pgqlw(lw)
!
! Query the current Line-Width attribute (set by routine PGSLW).
!
! Argument:
!  LW     (output)  : the line-width (in range 1-201).
!--
!  5-Nov-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: lw
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQLW'))THEN
      lw=1
   ELSE
      CALL grqlw(lw)
   END IF
!
   RETURN
!
END SUBROUTINE pgqlw
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQNDT -- inquire number of available device types
!%void cpgqndt(int *n);
!+
SUBROUTINE pgqndt(n)
!
! Return the number of available device types. This routine is
! usually used in conjunction with PGQDT to get a list of the
! available device types.
!
! Arguments:
!  N      (output) : the number of available device types.
!--
! 17-Mar-1997 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: n
!
   INTEGER :: nbuf,lchr
   REAL(KIND=pg) :: rbuf(2)
   CHARACTER (LEN=1) :: chr
!
! Initialize PGPLOT if necessary.
!
   CALL pginit
!
! Find number of device types.
!
   rbuf=0.0_pg
   CALL grexec(0,0,rbuf,nbuf,chr,lchr)
   n=nint(rbuf(1))
!
   RETURN
!
END SUBROUTINE pgqndt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQPOS -- inquire current pen position
!%void cpgqpos(float *x, float *y);
!+
SUBROUTINE pgqpos(x,y)
!
! Query the current "pen" position in world C coordinates (X,Y).
!
! Arguments:
!  X      (output)  : world x-coordinate of the pen position.
!  Y      (output)  : world y-coordinate of the pen position.
!--
!  1-Mar-1991 - new routine [JM].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)                     :: x
   REAL(KIND=pg), INTENT(IN OUT)                     :: y
!
   CALL grqpos(x,y)
!
   RETURN
!
END SUBROUTINE pgqpos
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQTBG -- inquire text background color index
!%void cpgqtbg(int *tbci);
!+
SUBROUTINE pgqtbg(tbci)
!
! Query the current Text Background Color Index (set by routine
! PGSTBG).
!
! Argument:
!  TBCI   (output) : receives the current text background color index.
!--
! 16-Oct-1993 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(OUT)                     :: tbci
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGQTBG'))THEN
      tbci=0
   ELSE
      tbci=pgtbci(pgid)
   END IF
!
   RETURN
!
END SUBROUTINE pgqtbg
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQTXT -- find bounding box of text string
!%void cpgqtxt(float x, float y, float angle, float fjust, \
!% const char *text, float *xbox, float *ybox);
!+
SUBROUTINE pgqtxt(x,y,angle,fjust,text,xbox,ybox)
!
! This routine returns a bounding box for a text string. Instead
! of drawing the string as routine PGPTXT does, it returns in XBOX
! and YBOX the coordinates of the corners of a rectangle parallel
! to the string baseline that just encloses the string. The four
! corners are in the order: lower left, upper left, upper right,
! lower right (where left and right refer to the first and last
! characters in the string).
!
! If the string is blank or contains no drawable characters, all
! four elements of XBOX and YBOX are assigned the starting point
! of the string, (X,Y).
!
! Arguments:
!  X, Y, ANGLE, FJUST, TEXT (input) : these arguments are the same as
!                    the corrresponding arguments in PGPTXT.
!  XBOX, YBOX (output) : arrays of dimension 4; on output, they
!                    contain the world coordinates of the bounding
!                    box in (XBOX(1), YBOX(1)), ..., (XBOX(4), YBOX(4)).
!--
! 12-Sep-1993 - new routine [TJP].
!  8-Nov-1994 - return something for blank string [TJP].
! 14-Jan-1997 - additional explanation [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: x
   REAL(KIND=pg), INTENT(IN)                :: y
   REAL(KIND=pg), INTENT(IN)                :: angle
   REAL(KIND=pg), INTENT(IN)                :: fjust
   CHARACTER (LEN=*), INTENT(IN)            :: text
   REAL(KIND=pg), INTENT(OUT)               :: xbox(4)
   REAL(KIND=pg), INTENT(OUT)               :: ybox(4)
!
   LOGICAL :: pgnoto
   INTEGER :: i,l,grtrim
   REAL(KIND=pg) :: d,xp,yp,xpbox(4),ypbox(4),xoffs,yoffs
!
   IF(pgnoto('PGQTXT'))RETURN
!
   l=grtrim(text)
   IF(l <= 0)THEN
      DO  i=1,4
         xbox(i)=x
         ybox(i)=y
      END DO
   ELSE
      d=0.0_pg
      IF(ABS(fjust) > EPSILON(fjust))CALL grlen(text(1:l),d)
      xoffs=pgxorg(pgid)-d*fjust*COS(angle/57.295779513082321_pg)
      yoffs=pgyorg(pgid)-d*fjust*SIN(angle/57.295779513082321_pg)
      xp=x*pgxscl(pgid)+xoffs
      yp=y*pgyscl(pgid)+yoffs
      CALL grqtxt(angle,xp,yp,text(1:l),xpbox,ypbox)
      DO  i=1,4
         xbox(i)=(xpbox(i)-pgxorg(pgid))/pgxscl(pgid)
         ybox(i)=(ypbox(i)-pgyorg(pgid))/pgyscl(pgid)
      END DO
   END IF
!
   RETURN
!
END SUBROUTINE pgqtxt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQVP -- inquire viewport size and position
!%void cpgqvp(int units, float *x1, float *x2, float *y1, float *y2);
!+
SUBROUTINE pgqvp(units,x1,x2,y1,y2)
!
! Inquiry routine to determine the current viewport setting.
! The values returned may be normalized device coordinates, inches, mm,
! or pixels, depending on the value of the input parameter CFLAG.
!
! Arguments:
!  UNITS  (input)  : used to specify the units of the output parameters:
!                    UNITS = 0 : normalized device coordinates
!                    UNITS = 1 : inches
!                    UNITS = 2 : millimeters
!                    UNITS = 3 : pixels
!                    Other values give an error message, and are
!                    treated as 0.
!  X1     (output) : the x-coordinate of the bottom left corner of the
!                    viewport.
!  X2     (output) : the x-coordinate of the top right corner of the
!                    viewport.
!  Y1     (output) : the y-coordinate of the bottom left corner of the
!                    viewport.
!  Y2     (output) : the y-coordinate of the top right corner of the
!                    viewport.
!--
! 26-Sep-1985 - new routine (TJP).
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: units
   REAL(KIND=pg), INTENT(OUT)               :: x1
   REAL(KIND=pg), INTENT(OUT)               :: x2
   REAL(KIND=pg), INTENT(OUT)               :: y1
   REAL(KIND=pg), INTENT(OUT)               :: y2
!
   REAL(KIND=pg) :: sx,sy
!
   IF(units == 0)THEN
      sx=pgxsz(pgid)
      sy=pgysz(pgid)
   ELSE IF(units == 1)THEN
      sx=pgxpin(pgid)
      sy=pgypin(pgid)
   ELSE IF(units == 2)THEN
      sx=(pgxpin(pgid)/25.4_pg)
      sy=(pgypin(pgid)/25.4_pg)
   ELSE IF(units == 3)THEN
      sx=1.0_pg
      sy=1.0_pg
   ELSE
      CALL grwarn('Illegal value for parameter UNITS in routine PGQVP')
      sx=pgxsz(pgid)
      sy=pgysz(pgid)
   END IF
   x1=pgxvp(pgid)/sx
   x2=(pgxvp(pgid)+pgxlen(pgid))/sx
   y1=pgyvp(pgid)/sy
   y2=(pgyvp(pgid)+pgylen(pgid))/sy
!
   RETURN
!
END SUBROUTINE pgqvp
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQVSZ -- inquire size of view surface
!%void cpgqvsz(int units, float *x1, float *x2, float *y1, float *y2);
!+
SUBROUTINE pgqvsz(units,x1,x2,y1,y2)
!
! This routine returns the dimensions of the view surface (the maximum
! plottable area) of the currently selected graphics device, in
! a variety of units. The size of the view surface is device-dependent
! and is established when the graphics device is opened. On some
! devices, it can be changed by calling PGPAP before starting a new
! page with PGPAGE. On some devices, the size can be changed (e.g.,
! by a workstation window manager) outside PGPLOT, and PGPLOT detects
! the change when PGPAGE is used. Call this routine after PGPAGE to
! find the current size.
!
! Note 1: the width and the height of the view surface in normalized
! device coordinates are both always equal to 1.0.
!
! Note 2: when the device is divided into panels (see PGSUBP), the
! view surface is a single panel.
!
! Arguments:
!  UNITS  (input)  : 0,1,2,3 for output in normalized device coords,
!                    inches, mm, or device units (pixels)
!  X1     (output) : always returns 0.0
!  X2     (output) : width of view surface
!  Y1     (output) : always returns 0.0
!  Y2     (output) : height of view surface
!--
! 28-Aug-1992 - new routine [Neil Killeen].
!  2-Dec-1992 - changed to avoid resetting the viewport [TJP].
! 26-Feb-1997 - revised description [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: units
   REAL(KIND=pg), INTENT(OUT)               :: x1
   REAL(KIND=pg), INTENT(OUT)               :: x2
   REAL(KIND=pg), INTENT(OUT)               :: y1
   REAL(KIND=pg), INTENT(OUT)               :: y2
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: sx,sy
!
   IF(pgnoto('PGQVSZ'))THEN
      x1=0.0_pg
      x2=0.0_pg
      y1=0.0_pg
      y2=0.0_pg
      RETURN
   END IF
!
   IF(units == 0)THEN
      sx=pgxsz(pgid)
      sy=pgysz(pgid)
   ELSE IF(units == 1)THEN
      sx=pgxpin(pgid)
      sy=pgypin(pgid)
   ELSE IF(units == 2)THEN
      sx=(pgxpin(pgid)/25.4_pg)
      sy=(pgypin(pgid)/25.4_pg)
   ELSE IF(units == 3)THEN
      sx=1.0_pg
      sy=1.0_pg
   ELSE
      CALL grwarn ('Illegal value for parameter UNITS in routine PGQVSZ')
      sx=pgxsz(pgid)
      sy=pgysz(pgid)
   END IF
   x1=0.0_pg
   x2=pgxsz(pgid)/sx
   y1=0.0_pg
   y2=pgysz(pgid)/sy
!
   RETURN
!
END SUBROUTINE pgqvsz
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGQWIN -- inquire window boundary coordinates
!%void cpgqwin(float *x1, float *x2, float *y1, float *y2);
!+
SUBROUTINE pgqwin(x1,x2,y1,y2)
!
! Inquiry routine to determine the current window setting.
! The values returned are world coordinates.
!
! Arguments:
!  X1     (output) : the x-coordinate of the bottom left corner
!                    of the window.
!  X2     (output) : the x-coordinate of the top right corner
!                    of the window.
!  Y1     (output) : the y-coordinate of the bottom left corner
!                    of the window.
!  Y2     (output) : the y-coordinate of the top right corner
!                    of the window.
!--
! 26-Sep-1985 - new routine (TJP).
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(OUT)                        :: x1
   REAL(KIND=pg), INTENT(OUT)                        :: x2
   REAL(KIND=pg), INTENT(OUT)                        :: y1
   REAL(KIND=pg), INTENT(OUT)                        :: y2
!
   x1=pgxblc(pgid)
   x2=pgxtrc(pgid)
   y1=pgyblc(pgid)
   y2=pgytrc(pgid)
!
   RETURN
!
END SUBROUTINE pgqwin
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGRECT -- draw a rectangle, using fill-area attributes
!%void cpgrect(float x1, float x2, float y1, float y2);
!+
SUBROUTINE pgrect(x1,x2,y1,y2)
!
! This routine can be used instead of PGPOLY for the special case of
! drawing a rectangle aligned with the coordinate axes; only two
! vertices need be specified instead of four.  On most devices, it is
! faster to use PGRECT than PGPOLY for drawing rectangles.  The
! rectangle has vertices at (X1,Y1), (X1,Y2), (X2,Y2), and (X2,Y1).
!
! Arguments:
!  X1, X2 (input) : the horizontal range of the rectangle.
!  Y1, Y2 (input) : the vertical range of the rectangle.
!--
! 21-Nov-1986 - [TJP].
! 22-Mar-1988 - use GRRECT for fill [TJP].
!  6-Mar-1995 - add hatching (by calling PGHTCH) [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: x1
   REAL(KIND=pg), INTENT(IN)                         :: x2
   REAL(KIND=pg), INTENT(IN)                         :: y1
   REAL(KIND=pg), INTENT(IN)                         :: y2
!
   REAL(KIND=pg) :: xp(4),yp(4)
!
   CALL pgbbuf
!
! Outline only.
!
   IF(pgfas(pgid) == 2)THEN
      CALL grmova(x1,y1)
      CALL grlina(x1,y2)
      CALL grlina(x2,y2)
      CALL grlina(x2,y1)
      CALL grlina(x1,y1)
!
! Hatching.
!
   ELSE IF(pgfas(pgid) == 3.OR.pgfas(pgid) == 4)THEN
      xp(1)=x1
      xp(2)=x1
      xp(3)=x2
      xp(4)=x2
      yp(1)=y1
      yp(2)=y2
      yp(3)=y2
      yp(4)=y1
      CALL pghtch(4,xp,yp,0.0_pg)
      IF(pgfas(pgid) == 4)CALL pghtch(4,xp,yp,90.0_pg)
!
! Solid fill.
!
   ELSE
      CALL grrect(x1,y1,x2,y2)
      CALL grmova(x1,y1)
   END IF
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgrect
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGRND -- find the smallest `round' number greater than x
!%float cpgrnd(float x, int *nsub);
!+
FUNCTION pgrnd(x,nsub)
!
! Routine to find the smallest "round" number larger than x, a
! "round" number being 1, 2 or 5 times a power of 10. If X is negative,
! PGRND(X) = -PGRND(ABS(X)). eg PGRND(8.7) = 10.0,
! PGRND(-0.4) = -0.5.  If X is zero, the value returned is zero.
! This routine is used by PGBOX for choosing  tick intervals.
!
! Returns:
!  PGRND         : the "round" number.
! Arguments:
!  X      (input)  : the number to be rounded.
!  NSUB   (output) : a suitable number of subdivisions for
!                    subdividing the "nice" number: 2 or 5.
!--
!  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
!  2-Dec-1991 - Fix for bug found on Fujitsu [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: x
   INTEGER, INTENT(OUT)                     :: nsub
!
   INTEGER :: i,ilog
   REAL(KIND=pg) :: frac,nice(3),pwr,xlog,xx,pgrnd
!
!INTRINSIC  ABS,LOG10,SIGN
!
   DATA nice/2.0_pg,5.0_pg,10.0_pg/
!
   IF(ABS(x) <= EPSILON(x))THEN
      pgrnd=0.0_pg
      nsub=2
      RETURN
   END IF
   xx=ABS(x)
   xlog=LOG10(xx)
   ilog=INT(xlog)
   IF(xlog < 0)ilog=ilog-1
   pwr=10.0_pg**ilog
   frac=xx/pwr
   i=3
   IF(frac <= nice(2))i=2
   IF(frac <= nice(1))i=1
   pgrnd=SIGN(pwr*nice(i),x)
   nsub=5
   IF(i == 1)nsub=2
!
   RETURN
!
END FUNCTION pgrnd
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGRNGE -- choose axis limits
!%void cpgrnge(float x1, float x2, float *xlo, float *xhi);
!+
SUBROUTINE pgrnge(x1,x2,xlo,xhi)
!
! Choose plotting limits XLO and XHI which encompass the data
! range X1 to X2.
!
! Arguments:
!  X1, X2 (input)  : the data range (X1<X2), ie, the min and max values
!                    to be plotted.
!  XLO, XHI (output) : suitable values to use as the extremes of a graph
!                    axis (XLO <= X1, XHI >= X2).
!--
! 10-Nov-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                  :: x1
   REAL(KIND=pg), INTENT(IN)                  :: x2
   REAL(KIND=pg), INTENT(OUT)                 :: xlo
   REAL(KIND=pg), INTENT(OUT)                 :: xhi
!
   xlo=x1-0.1_pg*(x2-x1)
   xhi=x2+0.1_pg*(x2-x1)
   IF(xlo < 0.0_pg.AND.x1 >= 0.0_pg)xlo=0.0_pg
   IF(xhi > 0.0_pg.AND.x2 <= 0.0_pg)xhi=0.0_pg
!
   RETURN
!
END SUBROUTINE pgrnge
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSAH -- set arrow-head style
!%void cpgsah(int fs, float angle, float barb);
!+
SUBROUTINE pgsah(fs,angle,barb)
!
! Set the style to be used for arrowheads drawn with routine PGARRO.
!
! Argument:
!  FS     (input)  : FS = 1 => filled; FS = 2 => outline.
!                    Other values are treated as 2. Default 1.
!  ANGLE  (input)  : the acute angle of the arrow point, in degrees;
!                    angles in the range 20.0 to 90.0 give reasonable
!                    results. Default 45.0.
!  BARB   (input)  : the fraction of the triangular arrow-head that
!                    is cut away from the back. 0.0 gives a triangular
!                    wedge arrow-head; 1.0 gives an open >. Values 0.3
!                    to 0.7 give reasonable results. Default 0.3.
!--
! 13-Oct-1992 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: fs
   REAL(KIND=pg), INTENT(IN)                :: angle
   REAL(KIND=pg), INTENT(IN)                :: barb
!
   pgahs(pgid)=fs
   IF(pgahs(pgid) /= 1)pgahs(pgid)=2
   pgaha(pgid)=angle
   pgahv(pgid)=barb
!
   RETURN
!
END SUBROUTINE pgsah
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSAVE -- save PGPLOT attributes
!%void cpgsave(void);
!+
SUBROUTINE pgsave
!
! This routine saves the current PGPLOT attributes in a private storage
! area. They can be restored by calling PGUNSA (unsave). Attributes
! saved are: character font, character height, color index, fill-area
! style, line style, line width, pen position, arrow-head style,
! hatching style, and clipping state. Color representation is not saved.
!
! Calls to PGSAVE and PGUNSA should always be paired. Up to 20 copies
! of the attributes may be saved. PGUNSA always retrieves the last-saved
! values (last-in first-out stack).
!
! Note that when multiple devices are in use, PGUNSA retrieves the
! values saved by the last PGSAVE call, even if they were for a
! different device.
!
! Arguments: none
!--
! 20-Apr-1992 - new routine [TJP].
! 27-Nov-1992 - add arrowhead style [TJP].
!  6-Oct-1993 - add text opacity [TJP].
! 28-Feb-1994 - correct bug (variable not saved) [TJP].
! 26-Feb-1995 - add hatching attributes.
! 19-Jun-1996 - correction in header comments [TJP].
! 26-Feb-1997 - add clipping state [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER :: maxs
   PARAMETER  (maxs=20)
!
   INTEGER :: lev
   INTEGER :: cf(maxs),ci(maxs),fs(maxs),ls(maxs),lw(maxs)
   INTEGER :: ahfs(maxs),tbg(maxs),clp(maxs)
   REAL(KIND=pg) :: ch(maxs),pos(2,maxs)
   REAL(KIND=pg) :: ahang(maxs),ahbarb(maxs),hsa(maxs),hss(maxs),hsp(maxs)
   SAVE  lev,cf,ci,fs,ls,lw,ahfs,tbg,ch,pos
   SAVE  ahang,ahbarb,hsa,hss,hsp,clp
   DATA lev/0/
!
   IF(lev >= maxs)THEN
      CALL grwarn('Too many unmatched calls to PGSAVE')
   ELSE
      lev=lev+1
      CALL pgqcf(cf(lev))
      CALL pgqch(ch(lev))
      CALL pgqci(ci(lev))
      CALL pgqfs(fs(lev))
      CALL pgqls(ls(lev))
      CALL pgqlw(lw(lev))
!
!          CALL PGQVP(0, VP(1,LEV), VP(2,LEV), VP(3,LEV), VP(4,LEV))
!          CALL PGQWIN(WIN(1,LEV), WIN(2,LEV), WIN(3,LEV), WIN(4,LEV))
!
      CALL pgqpos(pos(1,lev),pos(2,lev))
      CALL pgqah(ahfs(lev),ahang(lev),ahbarb(lev))
      CALL pgqtbg(tbg(lev))
      CALL pgqhs(hsa(lev),hss(lev),hsp(lev))
      CALL pgqclp(clp(lev))
   END IF
   RETURN
!
!*PGUNSA -- restore PGPLOT attributes
!%void cpgunsa(void);
!+
 ENTRY  pgunsa
!
! This routine restores the PGPLOT attributes saved in the last call to
! PGSAVE. Usage: CALL PGUNSA (no arguments). See PGSAVE.
!
! Arguments: none
!-----------------------------------------------------------------------
!
   IF(lev <= 0)THEN
      CALL grwarn ('PGUNSA: nothing has been saved')
   ELSE
      CALL pgscf(cf(lev))
      CALL pgsch(ch(lev))
      CALL pgsci(ci(lev))
      CALL pgsfs(fs(lev))
      CALL pgsls(ls(lev))
      CALL pgslw(lw(lev))
!
!          CALL PGSVP(VP(1,LEV), VP(2,LEV), VP(3,LEV), VP(4,LEV))
!          CALL PGSWIN(WIN(1,LEV), WIN(2,LEV), WIN(3,LEV), WIN(4,LEV))
!
      CALL pgmove(pos(1,lev),pos(2,lev))
      CALL pgsah(ahfs(lev),ahang(lev),ahbarb(lev))
      CALL pgstbg(tbg(lev))
      CALL pgshs(hsa(lev),hss(lev),hsp(lev))
      CALL pgsclp(clp(lev))
      lev=lev-1
   END IF
   RETURN
!
END SUBROUTINE pgsave
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSCF -- set character font
!%void cpgscf(int font);
!+
SUBROUTINE pgscf(font)
!
! Set the Character Font for subsequent text plotting. Four different
! fonts are available:
!   1: (default) a simple single-stroke font ("normal" font)
!   2: roman font
!   3: italic font
!   4: script font
! This call determines which font is in effect at the beginning of
! each text string. The font can be changed (temporarily) within a text
! string by using the escape sequences \fn, \fr, \fi, and \fs for fonts
! 1, 2, 3, and 4, respectively.
!
! Argument:
!  FONT   (input)  : the font number to be used for subsequent text
!                    plotting (in range 1-4).
!--
! 26-Sep-1985 - new routine [TJP].
! 25-OCT-1993 - changed name of argument [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: font
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSCF'))RETURN
   CALL grsfnt(font)
!
   RETURN
!
END SUBROUTINE pgscf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSCH -- set character height
!%void cpgsch(float size);
!+
SUBROUTINE pgsch(size)
!
! Set the character size attribute. The size affects all text and graph
! markers drawn later in the program. The default character size is
! 1.0, corresponding to a character height about 1/40 the height of
! the view surface.  Changing the character size also scales the length
! of tick marks drawn by PGBOX and terminals drawn by PGERRX and PGERRY.
!
! Argument:
!  SIZE   (input)  : new character size (dimensionless multiple of
!                    the default size).
!--
! (1-Mar-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                         :: size
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xc,xcnew,yc,xs,ys
!
   IF(pgnoto('PGSCH'))RETURN
!
   CALL grchsz (pgid,xc,yc,xs,ys)
   IF(pgxsz(pgid)/pgxpin(pgid) > pgysz(pgid)/pgypin(pgid))THEN
      xcnew=size*xc*pgysz(pgid)/ys/40.0_pg
   ELSE
      xcnew=size*xc*(pgxsz(pgid)*pgypin(pgid)/pgxpin(pgid))/ys/40.0_pg
   END IF
   CALL grsetc(pgid,xcnew)
   pgxsp(pgid)=xs*xcnew/xc
   pgysp(pgid)=ys*xcnew/xc
   pgchsz(pgid)=size
!
   RETURN
!
END SUBROUTINE pgsch
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!

!*PGSCI -- set color index
!%void cpgsci(int ci);
!+
SUBROUTINE pgsci(ci)
!
! Set the Color Index for subsequent plotting, if the output device
! permits this. The default color index is 1, usually white on a black
! background for video displays or black on a white background for
! printer plots. The color index is an integer in the range 0 to a
! device-dependent maximum. Color index 0 corresponds to the background
! color; lines may be "erased" by overwriting them with color index 0
! (if the device permits this).
!
! If the requested color index is not available on the selected device,
! color index 1 will be substituted.
!
! The assignment of colors to color indices can be changed with
! subroutine PGSCR (set color representation).  Color indices 0-15
! have predefined color representations (see the PGPLOT manual), but
! these may be changed with PGSCR.  Color indices above 15  have no
! predefined representations: if these indices are used, PGSCR must
! be called to define the representation.
!
! Argument:
!  CI     (input)  : the color index to be used for subsequent plotting
!                    on the current device (in range 0-max). If the
!                    index exceeds the device-dependent maximum, the
!                    default color index (1) is used.
!--
! 26-Sep-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: ci
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSCI'))RETURN
   CALL grsci(ci)
!
   RETURN
!
END SUBROUTINE pgsci
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSCIR -- set color index range
!%void cpgscir(int icilo, int icihi);
!+
SUBROUTINE pgscir(icilo,icihi)
!
! Set the color index range to be used for producing images with
! PGGRAY or PGIMAG. If the range is not all within the range supported
! by the device, a smaller range will be used. The number of
! different colors available for images is ICIHI-ICILO+1.
!
! Arguments:
!  ICILO  (input)  : the lowest color index to use for images
!  ICIHI  (input)  : the highest color index to use for images
!--
! 1994-Mar-17 : new routine [AFT/TJP].
!---
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: icilo
   INTEGER, INTENT(IN OUT)                  :: icihi
!
   INTEGER :: ic1,ic2
!
   CALL grqcol(ic1,ic2)
   pgmnci(pgid)=MIN(ic2,MAX(ic1,icilo))
   pgmxci(pgid)=MIN(ic2,MAX(ic1,icihi))
!
   RETURN
!
END SUBROUTINE pgscir
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSCLP -- enable or disable clipping at edge of viewport
!%void cpgsclp(int state);
!+
SUBROUTINE pgsclp(state)
!
! Normally all PGPLOT primitives except text are ``clipped'' at the
! edge of the viewport: parts of the primitives that lie outside
! the viewport are not drawn. If clipping is disabled by calling this
! routine, primitives are visible wherever they lie on the view
! surface. The default (clipping enabled) is appropriate for almost
! all applications.
!
! Argument:
!  STATE  (input)  : 0 to disable clipping, or 1 to enable clipping.
!
! 25-Feb-1997 [TJP] - new routine.
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: state
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSCLP'))RETURN
!
! Disable clipping.
!
   IF(state == 0)THEN
      CALL grarea(pgid,0.0_pg,0.0_pg,-1.0_pg,-1.0_pg)
      pgclp(pgid)=0
!
! Enable clipping.
!
   ELSE
      CALL grarea(pgid,pgxoff(pgid),pgyoff(pgid),pgxlen(pgid),pgylen(pgid))
      pgclp(pgid)=1
   END IF
!
   RETURN
!
END SUBROUTINE pgsclp
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSCR -- set color representation
!%void cpgscr(int ci, float cr, float cg, float cb);
!+
SUBROUTINE pgscr(ci,cr,cg,cb)
!
! Set color representation: i.e., define the color to be
! associated with a color index.  Ignored for devices which do not
! support variable color or intensity.  Color indices 0-15
! have predefined color representations (see the PGPLOT manual), but
! these may be changed with PGSCR.  Color indices 16-maximum have no
! predefined representations: if these indices are used, PGSCR must
! be called to define the representation. On monochrome output
! devices (e.g. VT125 terminals with monochrome monitors), the
! monochrome intensity is computed from the specified Red, Green, Blue
! intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television
! systems, NTSC encoding.  Note that most devices do not have an
! infinite range of colors or monochrome intensities available;
! the nearest available color is used.  Examples: for black,
! set CR=CG=CB=0.0; for white, set CR=CG=CB=1.0; for medium gray,
! set CR=CG=CB=0.5; for medium yellow, set CR=CG=0.5, CB=0.0.
!
! Argument:
!  CI     (input)  : the color index to be defined, in the range 0-max.
!                    If the color index greater than the device
!                    maximum is specified, the call is ignored. Color
!                    index 0 applies to the background color.
!  CR     (input)  : red, green, and blue intensities,
!  CG     (input)    in range 0.0 to 1.0.
!  CB     (input)
!--
! 5-Nov-1985 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: ci
   REAL(KIND=pg), INTENT(IN)            :: cr
   REAL(KIND=pg), INTENT(IN)            :: cg
   REAL(KIND=pg), INTENT(IN)            :: cb
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSCR'))RETURN
   CALL grscr(ci,cr,cg,cb)
!
   RETURN
!
END SUBROUTINE pgscr
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSCRL -- scroll window
!%void cpgscrl(float dx, float dy);
!+
SUBROUTINE pgscrl(dx,dy)
!
! This routine moves the window in world-coordinate space while
! leaving the viewport unchanged. On devices that have the
! capability, the pixels within the viewport are scrolled
! horizontally, vertically or both in such a way that graphics
! previously drawn in the window are shifted so that their world
! coordinates are unchanged.
!
! If the old window coordinate range was (X1, X2, Y1, Y2), the new
! coordinate range will be approximately (X1+DX, X2+DX, Y1+DY, Y2+DY).
! The size and scale of the window are unchanged.
!
! Thee window can only be shifted by a whole number of pixels
! (device coordinates). If DX and DY do not correspond to integral
! numbers of pixels, the shift will be slightly different from that
! requested. The new window-coordinate range, and hence the exact
! amount of the shift, can be determined by calling PGQWIN after this
! routine.
!
! Pixels that are moved out of the viewport by this operation are
! lost completely; they cannot be recovered by scrolling back.
! Pixels that are ``scrolled into'' the viewport are filled with
! the background color (color index 0).
!
! If the absolute value of DX is bigger than the width of the window,
! or the aboslute value of DY is bigger than the height of the window,
! the effect will be the same as zeroing all the pixels in the
! viewport.
!
! Not all devices have the capability to support this routine.
! It is only available on some interactive devices that have discrete
! pixels. To determine whether the current device has scroll capability,
! call PGQINF.
!
! Arguments:
!  DX     (input)  : distance (in world coordinates) to shift the
!                    window horizontally (positive shifts window to the
!                    right and scrolls to the left).
!  DY     (input)  : distance (in world coordinates) to shift the
!                    window vertically (positive shifts window up and
!                    scrolls down).
!--
! 25-Feb-97: new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)           :: dx
   REAL(KIND=pg), INTENT(IN OUT)           :: dy
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: x1,x2,y1,y2,ddx,ddy
   INTEGER :: ndx,ndy
!
   IF(pgnoto('PGSCRL'))RETURN
!
! Shift must be a whole number of pixels.
!
   ndx=nint(dx*pgxscl(pgid))
   ndy=nint(dy*pgyscl(pgid))
!
   IF(ndx /= 0.OR.ndy /= 0)THEN
      CALL pgbbuf
      ddx=ndx/pgxscl(pgid)
      ddy=ndy/pgyscl(pgid)
!
!        -- Set new world-ccordinate window.
!
      x1=pgxblc(pgid)
      x2=pgxtrc(pgid)
      y1=pgyblc(pgid)
      y2=pgytrc(pgid)
      pgxblc(pgid)=x1+ddx
      pgxtrc(pgid)=x2+ddx
      pgyblc(pgid)=y1+ddy
      pgytrc(pgid)=y2+ddy
      CALL pgvw
!
!        -- Do hardware scroll.
!
      CALL grscrl(ndx,ndy)
      CALL pgebuf
   END IF
!
   RETURN
!
END SUBROUTINE pgscrl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSCRN -- set color representation by name
!%void cpgscrn(int ci, const char *name, int *ier);
!+
SUBROUTINE pgscrn(ci,NAME,ier)
!
! Set color representation: i.e., define the color to be
! associated with a color index.  Ignored for devices which do not
! support variable color or intensity.  This is an alternative to
! routine PGSCR. The color representation is defined by name instead
! of (R,G,B) components.
!
! Color names are defined in an external file which is read the first
! time that PGSCRN is called. The name of the external file is
! found as follows:
! 1. if environment variable (logical name) PGPLOT_RGB is defined,
!    its value is used as the file name;
! 2. otherwise, if environment variable PGPLOT_DIR is defined, a
!    file "rgb.txt" in the directory named by this environment
!    variable is used;
! 3. otherwise, file "rgb.txt" in the current directory is used.
! If all of these fail to find a file, an error is reported and
! the routine does nothing.
!
! Each line of the file
! defines one color, with four blank- or tab-separated fields per
! line. The first three fields are the R, G, B components, which
! are integers in the range 0 (zero intensity) to 255 (maximum
! intensity). The fourth field is the color name. The color name
! may include embedded blanks. Example:
!
! 255   0   0 red
! 255 105 180 hot pink
! 255 255 255 white
!   0   0   0 black
!
! Arguments:
!  CI     (input)  : the color index to be defined, in the range 0-max.
!                    If the color index greater than the device
!                    maximum is specified, the call is ignored. Color
!                    index 0 applies to the background color.
!  NAME   (input)  : the name of the color to be associated with
!                    this color index. This name must be in the
!                    external file. The names are not case-sensitive.
!                    If the color is not listed in the file, the
!                    color representation is not changed.
!  IER    (output) : returns 0 if the routine was successful, 1
!                    if an error occurred (either the external file
!                    could not be read, or the requested color was
!                    not defined in the file).
!--
! 12-Oct-1992 [TJP]
! 31-May-1993 [TJP] use GROPTX to open file.
!  7-Nov-1994 [TJP] better error messages.
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ci
   CHARACTER (LEN=*), INTENT(IN)            :: NAME
   INTEGER, INTENT(OUT)                     :: ier
!
   INTEGER, PARAMETER :: maxcol=1000
   INTEGER :: i,ir,ig,ib,j,l,ncol,UNIT,ios
   INTEGER :: grctoi,groptx,grtrim
   REAL(KIND=pg) :: rr(maxcol),rg(maxcol),rb(maxcol)
   CHARACTER (LEN=20) :: creq,cname(maxcol)
   CHARACTER (LEN=255) :: text
   SAVE  ncol,cname,rr,rg,rb
   DATA ncol/0/
!
! On first call, read the database.
!
   IF(ncol == 0)THEN
      CALL grgfil('RGB',text)
      l=grtrim(text)
      IF(l < 1)l=1
      CALL grglun(UNIT)
      ios=groptx(UNIT,text(1:l),'rgb.txt',0)
      IF(ios /= 0)GO TO 40
      DO  i=1,maxcol
         READ(UNIT,'(A)',ERR=20,END=20) text
         j=1
         CALL grskpb(text,j)
         ir=grctoi(text,j)
         CALL grskpb(text,j)
         ig=grctoi(text,j)
         CALL grskpb(text,j)
         ib=grctoi(text,j)
         CALL grskpb(text,j)
         ncol=ncol+1
         CALL grtoup(cname(ncol),text(j:))
         rr(ncol)=ir/255.0_pg
         rg(ncol)=ig/255.0_pg
         rb(ncol)=ib/255.0_pg
      END DO
20    CLOSE(UNIT)
      CALL grflun (UNIT)
   END IF
!
! Look up requested color and set color representation if found.
!
   CALL grtoup(creq,NAME)
   DO  i=1,ncol
      IF(creq == cname(i))THEN
         CALL pgscr(ci,rr(i),rg(i),rb(i))
         ier=0
         RETURN
      END IF
   END DO
!
! Color not found.
!
   ier=1
   text='Color not found: '//NAME
   CALL grwarn(text)
   RETURN
!
! Database not found.
!
40 ier=1
   ncol=-1
   CALL grflun(UNIT)
   CALL grwarn('Unable to read color file: '//text(1:l))
   CALL grwarn('Use environment variable PGPLOT_RGB to specify '  &
      //'the location of the PGPLOT rgb.txt file.')
   RETURN
!
END SUBROUTINE pgscrn
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pgsetc(size)
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)                     :: size
!
   CALL pgsch(size)
!
   RETURN
!
END SUBROUTINE pgsetc
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSFS -- set fill-area style
!%void cpgsfs(int fs);
!+
SUBROUTINE pgsfs(fs)
!
! Set the Fill-Area Style attribute for subsequent area-fill by
! PGPOLY, PGRECT, or PGCIRC.  Four different styles are available:
! solid (fill polygon with solid color of the current color-index),
! outline (draw outline of polygon only, using current line attributes),
! hatched (shade interior of polygon with parallel lines, using
! current line attributes), or cross-hatched. The orientation and
! spacing of hatch lines can be specified with routine PGSHS (set
! hatch style).
!
! Argument:
!  FS     (input)  : the fill-area style to be used for subsequent
!                    plotting:
!                      FS = 1 => solid (default)
!                      FS = 2 => outline
!                      FS = 3 => hatched
!                      FS = 4 => cross-hatched
!                    Other values give an error message and are
!                    treated as 2.
!--
! 21-Oct-1985 - new routine [TJP].
! 17-Dec-1990 - pass to GR level [TJP].
!  6-Mar-1995 - add styles 3 and 4 [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: fs
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSFS'))RETURN
   IF(fs < 1.OR.fs > 4)THEN
      CALL grwarn('illegal fill-area style requested')
      pgfas(pgid)=2
   ELSE
      pgfas(pgid)=fs
   END IF
!
   RETURN
!
END SUBROUTINE pgsfs
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSHLS -- set color representation using HLS system
!%void cpgshls(int ci, float ch, float cl, float cs);
!+
SUBROUTINE pgshls(ci,ch,cl,cs)
!
! Set color representation: i.e., define the color to be
! associated with a color index.  This routine is equivalent to
! PGSCR, but the color is defined in the Hue-Lightness-Saturation
! model instead of the Red-Green-Blue model. Hue is represented
! by an angle in degrees, with red at 120, green at 240,
! and blue at 0 (or 360). Lightness ranges from 0.0 to 1.0, with black
! at lightness 0.0 and white at lightness 1.0. Saturation ranges from
! 0.0 (gray) to 1.0 (pure color). Hue is irrelevant when saturation
! is 0.0.
!
! Examples:           H     L     S        R     G     B
!     black          any   0.0   0.0      0.0   0.0   0.0
!     white          any   1.0   0.0      1.0   1.0   1.0
!     medium gray    any   0.5   0.0      0.5   0.5   0.5
!     red            120   0.5   1.0      1.0   0.0   0.0
!     yellow         180   0.5   1.0      1.0   1.0   0.0
!     pink           120   0.7   0.8      0.94  0.46  0.46
!
! Reference: SIGGRAPH Status Report of the Graphic Standards Planning
! Committee, Computer Graphics, Vol.13, No.3, Association for
! Computing Machinery, New York, NY, 1979. See also: J. D. Foley et al,
! ``Computer Graphics: Principles and Practice'', second edition,
! Addison-Wesley, 1990, section 13.3.5.
!
! Argument:
!  CI     (input)  : the color index to be defined, in the range 0-max.
!                    If the color index greater than the device
!                    maximum is specified, the call is ignored. Color
!                    index 0 applies to the background color.
!  CH     (input)  : hue, in range 0.0 to 360.0.
!  CL     (input)  : lightness, in range 0.0 to 1.0.
!  CS     (input)  : saturation, in range 0.0 to 1.0.
!--
! 9-May-1988 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: ci
   REAL(KIND=pg), INTENT(IN OUT)            :: ch
   REAL(KIND=pg), INTENT(IN OUT)            :: cl
   REAL(KIND=pg), INTENT(IN OUT)            :: cs
!
   REAL(KIND=pg) :: cr,cg,cb
!
   CALL grxrgb(ch,cl,cs,cr,cg,cb)
   CALL grscr(ci,cr,cg,cb)
!
   RETURN
!
END SUBROUTINE pgshls
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSHS -- set hatching style
!%void cpgshs(float angle, float sepn, float phase);
!+
SUBROUTINE pgshs(angle,sepn,phase)
!
! Set the style to be used for hatching (fill area with fill-style 3).
! The default style is ANGLE=45.0, SEPN=1.0, PHASE=0.0.
!
! Arguments:
!  ANGLE  (input)  : the angle the hatch lines make with the
!                    horizontal, in degrees, increasing
!                    counterclockwise (this is an angle on the
!                    view surface, not in world-coordinate space).
!  SEPN   (input)  : the spacing of the hatch lines. The unit spacing
!                    is 1 percent of the smaller of the height or
!                    width of the view surface. This should not be
!                    zero.
!  PHASE  (input)  : a real number between 0 and 1; the hatch lines
!                    are displaced by this fraction of SEPN from a
!                    fixed reference.  Adjacent regions hatched with the
!                    same PHASE have contiguous hatch lines. To hatch
!                    a region with alternating lines of two colors,
!                    fill the area twice, with PHASE=0.0 for one color
!                    and PHASE=0.5 for the other color.
!--
! 26-Feb-1995 - new routine [TJP].
! 12-Feb-1996 - check for zero spacing [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: angle
   REAL(KIND=pg), INTENT(IN)                :: sepn
   REAL(KIND=pg), INTENT(IN)                :: phase
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSHS'))RETURN
   pghsa(pgid)=angle
   IF(ABS(sepn) <= EPSILON(sepn))THEN
      CALL grwarn('PGSHS: zero hatch line spacing requested')
      pghss(pgid)=1.0_pg
   ELSE
      pghss(pgid)=sepn
   END IF
   IF(phase < 0.0_pg.OR.phase > 1.0_pg)THEN
      CALL grwarn('PGSHS: hatching phase must be in (0.0,1.0)')
   END IF
   pghsp(pgid)=phase
!
   RETURN
!
END SUBROUTINE pgshs
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSITF -- set image transfer function
!%void cpgsitf(int itf);
!+
SUBROUTINE pgsitf(itf)
!
! Set the Image Transfer Function for subsequent images drawn by
! PGIMAG, PGGRAY, or PGWEDG. The Image Transfer Function is used
! to map array values into the available range of color indices
! specified with routine PGSCIR or (for PGGRAY on some devices)
! into dot density.
!
! Argument:
!  ITF    (input)  : type of transfer function:
!                      ITF = 0 : linear
!                      ITF = 1 : logarithmic
!                      ITF = 2 : square-root
!--
! 15-Sep-1994 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: itf
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSITF'))RETURN
   IF(itf < 0.OR.itf > 2)THEN
      pgitf(pgid)=0
      CALL grwarn('PGSITF: argument must be 0, 1, or 2')
   ELSE
      pgitf(pgid)=itf
   END IF
!
   RETURN
!
END SUBROUTINE pgsitf
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pgsize(width,height,shiftx,shifty,dummy)
!
! PGPLOT (obsolete routine; use PGVSIZ in preference): Change the
! size and position of the viewport.
!
! Arguments:
!
! WIDTH (input, real) : width of viewport in inches.
! HEIGHT (input, real) : height of viewport in inches.
! SHIFTX (input, real) : horizontal offset of bottom left corner
!       from blc of page or panel, in inches.
! SHIFTY (input, real) : vertical offset of bottom left corner
!       from blc of page or panel, in inches.
! DUMMY (input, real) : reserved for future use (must be 0.0).
!--
! 13-Dec-1990  Make errors non-fatal [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)            :: width
   REAL(KIND=pg), INTENT(IN OUT)            :: height
   REAL(KIND=pg), INTENT(IN OUT)            :: shiftx
   REAL(KIND=pg), INTENT(IN OUT)            :: shifty
   REAL(KIND=pg), INTENT(IN OUT)            :: dummy
!
   IF(width <= 0.0_pg.OR.height <= 0.0_pg.OR.ABS(dummy) > EPSILON(dummy))THEN
      CALL grwarn('PGSIZE ignored: invalid arguments')
      RETURN
   END IF
!
   CALL pgvsiz(shiftx,shiftx+width,shifty,shifty+height)
!
   RETURN
!
END SUBROUTINE pgsize
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSLCT -- select an open graphics device
!%void cpgslct(int id);
!+
SUBROUTINE pgslct(id)
!
! Select one of the open graphics devices and direct subsequent
! plotting to it. The argument is the device identifier returned by
! PGOPEN when the device was opened. If the supplied argument is not a
! valid identifier of an open graphics device, a warning message is
! issued and the current selection is unchanged.
!
! [This routine was added to PGPLOT in Version 5.1.0.]
!
! Arguments:
!
! ID (input, integer): identifier of the device to be selected.
!--
! 22-Dec-1995 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: id
!
   IF(id < 1.OR.id > pgmaxd)THEN
      CALL grwarn('PGSLCT: invalid argument')
   ELSE IF(pgdevs(id) /= 1)THEN
      CALL grwarn('PGSLCT: requested device is not open')
   ELSE
!
!        -- Select the new device
!
      pgid=id
      CALL grslct(pgid)
   END IF
!
   RETURN
!
END SUBROUTINE pgslct
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSLS -- set line style
!%void cpgsls(int ls);
!+
SUBROUTINE pgsls(ls)
!
! Set the line style attribute for subsequent plotting. This
! attribute affects line primitives only; it does not affect graph
! markers, text, or area fill.
! Five different line styles are available, with the following codes:
! 1 (full line), 2 (dashed), 3 (dot-dash-dot-dash), 4 (dotted),
! 5 (dash-dot-dot-dot). The default is 1 (normal full line).
!
! Argument:
!  LS     (input)  : the line-style code for subsequent plotting
!                    (in range 1-5).
!--
!  8-Aug-1985 - new routine, equivalent to GRSLS [TJP].
!  3-Jun-1984 - add GMFILE device [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: ls
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSLS'))RETURN
   CALL grsls(ls)
!
   RETURN
!
END SUBROUTINE pgsls
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSLW -- set line width
!%void cpgslw(int lw);
!+
SUBROUTINE pgslw(lw)
!
! Set the line-width attribute. This attribute affects lines, graph
! markers, and text. The line width is specified in units of 1/200
! (0.005) inch (about 0.13 mm) and must be an integer in the range
! 1-201. On some devices, thick lines are generated by tracing each
! line with multiple strokes offset in the direction perpendicular to
! the line.
!
! Argument:
!  LW     (input)  : width of line, in units of 0.005 inch (0.13 mm)
!                    in range 1-201.
!--
!  8-Aug-1985 - new routine, equivalent to GRSLW [TJP].
!  1-Feb-1995 - change comment [TJP].
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: lw
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSLW'))RETURN
   CALL grslw(lw)
!
   RETURN
!
END SUBROUTINE pgslw
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSTBG -- set text background color index
!%void cpgstbg(int tbci);
!+
SUBROUTINE pgstbg(tbci)
!
! Set the Text Background Color Index for subsequent text. By default
! text does not obscure underlying graphics. If the text background
! color index is positive, however, text is opaque: the bounding box
! of the text is filled with the color specified by PGSTBG before
! drawing the text characters in the current color index set by PGSCI.
! Use color index 0 to erase underlying graphics before drawing text.
!
! Argument:
!  TBCI   (input)  : the color index to be used for the background
!                    for subsequent text plotting:
!                      TBCI < 0  => transparent (default)
!                      TBCI >= 0 => text will be drawn on an opaque
!                    background with color index TBCI.
!--
! 16-Oct-1993 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE pgplot
!
   INTEGER, INTENT(IN)                      :: tbci
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSTBG'))RETURN
   IF(tbci < 0)THEN
      pgtbci(pgid)=-1
   ELSE
      pgtbci(pgid)=tbci
   END IF
!
   RETURN
!
END SUBROUTINE pgstbg
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSUBP -- subdivide view surface into panels
!%void cpgsubp(int nxsub, int nysub);
!+
SUBROUTINE pgsubp(nxsub,nysub)
!
! PGPLOT divides the physical surface of the plotting device (screen,
! window, or sheet of paper) into NXSUB x NYSUB `panels'. When the
! view surface is sub-divided in this way, PGPAGE moves to the next
! panel, not the next physical page. The initial subdivision of the
! view surface is set in the call to PGBEG. When PGSUBP is called,
! it forces the next call to PGPAGE to start a new physical page,
! subdivided in the manner indicated. No plotting should be done
! between a call of PGSUBP and a call of PGPAGE (or PGENV, which calls
! PGPAGE).
!
! If NXSUB > 0, PGPLOT uses the panels in row order; if <0,
! PGPLOT uses them in column order, e.g.,
!
!  NXSUB=3, NYSUB=2            NXSUB=-3, NYSUB=2
!
! +-----+-----+-----+         +-----+-----+-----+
! |  1  |  2  |  3  |         |  1  |  3  |  5  |
! +-----+-----+-----+         +-----+-----+-----+
! |  4  |  5  |  6  |         |  2  |  4  |  6  |
! +-----+-----+-----+         +-----+-----+-----+
!
! PGPLOT advances from one panels to the next when PGPAGE is called,
! clearing the screen or starting a new page when the last panel has
! been used. It is also possible to jump from one panel to another
! in random order by calling PGPANL.
!
! Arguments:
!  NXSUB  (input)  : the number of subdivisions of the view surface in
!                    X (>0 or <0).
!  NYSUB  (input)  : the number of subdivisions of the view surface in
!                    Y (>0).
!--
! 15-Nov-1993 [TJP] - new routine.
! 19-Feb-1994 [TJP] - rescale viewport when panel size changes.
! 23-Sep-1996 [TJP] - correct bug in assignment of PGROWS.
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                  :: nxsub
   INTEGER, INTENT(IN)                  :: nysub
!
   REAL(KIND=pg) :: ch,xfsz,yfsz
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xvp1,xvp2,yvp1,yvp2
!
   IF(pgnoto('PGSUBP'))RETURN
!
! Find current character size and viewport (NDC).
!
   CALL pgqch(ch)
   CALL pgqvp(0,xvp1,xvp2,yvp1,yvp2)
!
! Set the subdivisions.
!
   xfsz=pgnx(pgid)*pgxsz(pgid)
   yfsz=pgny(pgid)*pgysz(pgid)
   pgrows(pgid)=(nxsub >= 0)
   pgnx(pgid)=MAX(ABS(nxsub),1)
   pgny(pgid)=MAX(ABS(nysub),1)
   pgxsz(pgid)=xfsz/pgnx(pgid)
   pgysz(pgid)=yfsz/pgny(pgid)
!
! The current panel is the last on the physical page, to force
! a new physical page at next PGPAGE.
!
   pgnxc(pgid)=pgnx(pgid)
   pgnyc(pgid)=pgny(pgid)
!
! Rescale the character size and viewport to the new panel size.
!
   CALL pgsch(ch)
   CALL pgsvp(xvp1,xvp2,yvp1,yvp2)
!
   RETURN
!
END SUBROUTINE pgsubp
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSVP -- set viewport (normalized device coordinates)
!%void cpgsvp(float xleft, float xright, float ybot, float ytop);
!+
SUBROUTINE pgsvp(xleft,xright,ybot,ytop)
!
! Change the size and position of the viewport, specifying
! the viewport in normalized device coordinates.  Normalized
! device coordinates run from 0 to 1 in each dimension. The
! viewport is the rectangle on the view surface "through"
! which one views the graph.  All the PG routines which plot lines
! etc. plot them within the viewport, and lines are truncated at
! the edge of the viewport (except for axes, labels etc drawn with
! PGBOX or PGLAB).  The region of world space (the coordinate
! space of the graph) which is visible through the viewport is
! specified by a call to PGSWIN.  It is legal to request a
! viewport larger than the view surface; only the part which
! appears on the view surface will be plotted.
!
! Arguments:
!  XLEFT  (input)  : x-coordinate of left hand edge of viewport, in NDC.
!  XRIGHT (input)  : x-coordinate of right hand edge of viewport,
!                    in NDC.
!  YBOT   (input)  : y-coordinate of bottom edge of viewport, in NDC.
!  YTOP   (input)  : y-coordinate of top  edge of viewport, in NDC.
!--
! 13-Dec-1990  Make errors non-fatal [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)            :: xleft
   REAL(KIND=pg), INTENT(IN)            :: xright
   REAL(KIND=pg), INTENT(IN)            :: ybot
   REAL(KIND=pg), INTENT(IN)            :: ytop
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xs,ys
!
   IF(pgnoto('PGSVP'))RETURN
   IF(xleft >= xright.OR.ybot >= ytop)THEN
      CALL grwarn('PGSVP ignored: invalid arguments')
      RETURN
   END IF
!
   xs=pgxsz(pgid)/pgxpin(pgid)
   ys=pgysz(pgid)/pgypin(pgid)
   CALL pgvsiz(xleft*xs,xright*xs,ybot*ys,ytop*ys)
!
   RETURN
!
END SUBROUTINE pgsvp
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGSWIN -- set window
!%void cpgswin(float x1, float x2, float y1, float y2);
!+
SUBROUTINE pgswin(x1,x2,y1,y2)
!
! Change the window in world coordinate space that is to be mapped on
! to the viewport.  Usually PGSWIN is called automatically by PGENV,
! but it may be called directly by the user.
!
! Arguments:
!  X1     (input)  : the x-coordinate of the bottom left corner
!                    of the viewport.
!  X2     (input)  : the x-coordinate of the top right corner
!                    of the viewport (note X2 may be less than X1).
!  Y1     (input)  : the y-coordinate of the bottom left corner
!                    of the viewport.
!  Y2     (input)  : the y-coordinate of the top right corner
!                    of the viewport (note Y2 may be less than Y1).
!--
! 15-Nov-95: check arguments to prevent divide-by-zero [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: x1
   REAL(KIND=pg), INTENT(IN)                :: x2
   REAL(KIND=pg), INTENT(IN)                :: y1
   REAL(KIND=pg), INTENT(IN)                :: y2
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGSWIN'))RETURN
!
! If invalid arguments are specified, issue warning and leave window
! unchanged.
!
   IF(ABS(x1 - x2) <= EPSILON(x1))THEN
      CALL grwarn('invalid x limits in PGSWIN: X1 = X2.')
   ELSE IF(ABS(y1 - y2) <= EPSILON(y1))THEN
      CALL grwarn('invalid y limits in PGSWIN: Y1 = Y2.')
   ELSE
      pgxblc(pgid)=x1
      pgxtrc(pgid)=x2
      pgyblc(pgid)=y1
      pgytrc(pgid)=y2
      CALL pgvw
   END IF
!
   RETURN
!
END SUBROUTINE pgswin
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGTBOX -- draw frame and write (DD) HH MM SS.S labelling
!%void cpgtbox(const char *xopt, float xtick, int nxsub, \
!% const char *yopt, float ytick, int nysub);
!+
SUBROUTINE pgtbox(xopt,xtick,nxsub,yopt,ytick,nysub)
!
!
! Draw a box and optionally label one or both axes with (DD) HH MM SS
! style numeric labels (useful for time or RA - DEC plots).   If this
! style of labelling is desired, then PGSWIN should have been called
! previously with the extrema in SECONDS of time.
!
! In the seconds field, you can have at most 3 places after the decimal
! point, so that 1 ms is the smallest time interval you can time label.
!
! Large numbers are coped with by fields of 6 characters long.  Thus
! you could have times with days or hours as big as 999999.  However,
! in practice, you might have trouble with labels overwriting  themselves
! with such large numbers unless you a) use a small time INTERVAL,
! b) use a small character size or c) choose your own sparse ticks in
! the call to PGTBOX.
!
! PGTBOX will attempt, when choosing its own ticks, not to overwrite
! the labels, but this algorithm is not very bright and may fail.
!
! Note that small intervals but large absolute times such as
! TMIN = 200000.0 s and TMAX=200000.1 s will cause the algorithm
! to fail.  This is inherent in PGPLOT's use of single precision
! and cannot be avoided.  In such cases, you should use relative
! times if possible.
!
! PGTBOX's labelling philosophy is that the left-most or bottom tick of
! the axis contains a full label.  Thereafter, only changing fields are
! labelled.  Negative fields are given a '-' label, positive fields
! have none.   Axes that have the DD (or HH if the day field is not
! used) field on each major tick carry the sign on each field.  If the
! axis crosses zero, the zero tick will carry a full label and sign.
!
! This labelling style can cause a little confusion with some special
! cases, but as long as you know its philosophy, the truth can be divined.
! Consider an axis with TMIN=20s, TMAX=-20s.   The labels will look like
!
!        +----------+----------+----------+----------+
!     0h0m20s      10s      -0h0m0s      10s        20s
!
! Knowing that the left field always has a full label and that
! positive fields are unsigned, informs that time is decreasing
! from left to right, not vice versa.   This can become very
! unclear if you have used the 'F' option, but that is your problem !
!
! Exceptions to this labelling philosophy are when the finest time
! increment being displayed is hours (with option 'Y') or days.
! Then all fields carry a label.  For example,
!
!        +----------+----------+----------+----------+
!      -10h        -8h        -6h        -4h        -2h
!
!
! PGTBOX can be used in place of PGBOX; it calls PGBOX and only invokes
! time labelling if requested. Other options are passed intact to PGBOX.
!
! Inputs:
!  XOPT   :  X-options for PGTBOX.  Same as for PGBOX plus
!
!             'Z' for (DD) HH MM SS.S time labelling
!             'Y' means don't include the day field so that labels
!                 are HH MM SS.S rather than DD HH MM SS.S   The hours
!                 will accumulate beyond 24 if necessary in this case.
!             'X' label the HH field as modulo 24.  Thus, a label
!                 such as 25h 10m would come out as 1h 10m
!             'H' means superscript numbers with d, h, m, & s  symbols
!             'D' means superscript numbers with    o, ', & '' symbols
!             'F' causes the first label (left- or bottom-most) to
!                 be omitted. Useful for sub-panels that abut each other.
!                 Care is needed because first label carries sign as well.
!             'O' means omit leading zeros in numbers < 10
!                 E.g.  3h 3m 1.2s rather than 03h 03m 01.2s  Useful
!                 to help save space on X-axes. The day field does not
!                 use this facility.
!
!  YOPT   :  Y-options for PGTBOX.  See above.
!  XTICK  :  X-axis major tick increment.  0.0 for default.
!  YTICK  :  Y-axis major tick increment.  0.0 for default.
!            If the 'Z' option is used then XTICK and/or YTICK must
!            be in seconds.
!  NXSUB  :  Number of intervals for minor ticks on X-axis. 0 for default
!  NYSUB  :  Number of intervals for minor ticks on Y-axis. 0 for default
!
!  The regular XOPT and YOPT axis options for PGBOX are
!
!  A : draw Axis (X axis is horizontal line Y=0, Y axis is vertical
!      line X=0).
!  B : draw bottom (X) or left (Y) edge of frame.
!  C : draw top (X) or right (Y) edge of frame.
!  G : draw Grid of vertical (X) or horizontal (Y) lines.
!  I : Invert the tick marks; ie draw them outside the viewport
!      instead of inside.
!  L : label axis Logarithmically (see below).
!  N : write Numeric labels in the conventional location below the
!      viewport (X) or to the left of the viewport (Y).
!  P : extend ("Project") major tick marks outside the box (ignored if
!      option I is specified).
!  M : write numeric labels in the unconventional location above the
!      viewport (X) or to the right of the viewport (Y).
!  T : draw major Tick marks at the major coordinate interval.
!  S : draw minor tick marks (Subticks).
!  V : orient numeric labels Vertically. This is only applicable to Y.
!      The default is to write Y-labels parallel to the axis.
!  1 : force decimal labelling, instead of automatic choice (see PGNUMB).
!  2 : force exponential labelling, instead of automatic.
!
!      The default is to write Y-labels parallel to the axis
!
!
!        ******************        EXCEPTIONS       *******************
!
!        Note that
!          1) PGBOX option 'L' (log labels) is ignored with option 'Z'
!          2) The 'O' option will be ignored for the 'V' option as it
!             makes it impossible to align the labels nicely
!          3) Option 'Y' is forced with option 'D'
!
!        ***************************************************************
!
!
!--
! 05-Sep-1988 - new routine (Neil Killeen)
! 20-Apr-1991 - add support for new DD (day) field and implement
!               labelling on any axis (bottom,top,left,right) [nebk]
! 10-Jun-1993 - add option 'O' for leading zeros, correctly deal with
!               user ticks, fully support 'V' and 'NM' options, modify
!               slightly meaning of 'F' option [nebk]
! 16-Jan-1995 - add option 'X' [nebk]
! 16-Aug-1996 - Bring axis labelling displacements more in line with
!               those of pgbox.f [nebk]
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)            :: xopt
   REAL(KIND=pg), INTENT(IN)                :: xtick
   INTEGER, INTENT(IN)                      :: nxsub
   CHARACTER (LEN=*), INTENT(IN)            :: yopt
   REAL(KIND=pg), INTENT(IN)                :: ytick
   INTEGER, INTENT(IN)                      :: nysub
!
   REAL(KIND=pg) :: xtickd,ytickd,xmin,xmax,ymin,ymax
   INTEGER :: ipt,tscalx,tscaly,nxsubd,nysubd
   CHARACTER (LEN=15) :: xxopt,yyopt
   CHARACTER (LEN=4) :: suptyp
   LOGICAL :: xtime,ytime,first,dodayx,dodayy,do2,dopara,mod24
!
!  Copy inputs
!
   xtickd=xtick
   ytickd=ytick
   nxsubd=nxsub
   nysubd=nysub
!
!  Get window in world coordinates
!
   CALL pgqwin(xmin,xmax,ymin,ymax)
!
!  X-axis first
!
   CALL grtoup(xxopt,xopt)
   xtime=.false.
   IF(INDEX(xxopt,'Z') /= 0)THEN
!
!  Work out units for labelling and find the tick increments.
!
      IF(ABS(xmax-xmin) < 0.001)THEN
         CALL grwarn('PGTBOX: X-axis time interval too small '// &
            '(< 1 ms) for time labels')
      ELSE
         xtime=.true.
         dodayx=.true.
         IF(INDEX(xxopt,'Y') /= 0.OR.INDEX(xxopt,'D') /= 0)dodayx= .false.
!
         dopara=.true.
         CALL pgtbx1('X',dodayx,dopara,xmin,xmax,xtickd,nxsubd,tscalx)
      END IF
   END IF
!
!  Same again for Y-axis
!
   CALL grtoup(yyopt,yopt)
   ytime=.false.
   IF(INDEX(yyopt,'Z') /= 0)THEN
      IF(ABS(ymax-ymin) < 0.001)THEN
         CALL grwarn('PGTBOX: Y-axis time interval too small '// &
            '(< 1ms) for time labels')
      ELSE
         ytime=.true.
         dodayy=.true.
         IF(INDEX(yyopt,'Y') /= 0.OR.INDEX(yyopt,'D') /= 0)dodayy= .false.
!
         dopara=.true.
         IF(INDEX(yyopt,'V') /= 0)dopara=.false.
!
         CALL pgtbx1('Y',dodayy,dopara,ymin,ymax,ytickd,nysubd,tscaly)
      END IF
   END IF
!
!  Parse options list.  For call to PGBOX when doing time labelling, we
!  don't want L (log), N or M (write numeric labels).
!
   IF(xtime)THEN
      ipt=INDEX(xxopt,'L')
      IF(ipt /= 0)xxopt(ipt:ipt)=' '
      ipt=INDEX(xxopt,'N')
      IF(ipt /= 0)xxopt(ipt:ipt)=' '
      ipt=INDEX(xxopt,'M')
      IF(ipt /= 0)xxopt(ipt:ipt)=' '
   END IF
!
   IF(ytime)THEN
      ipt=INDEX(yyopt,'L')
      IF(ipt /= 0)yyopt(ipt:ipt)=' '
      ipt=INDEX(yyopt,'N')
      IF(ipt /= 0)yyopt(ipt:ipt)=' '
      ipt=INDEX(yyopt,'M')
      IF(ipt /= 0)yyopt(ipt:ipt)=' '
   END IF
!
!  Draw box and ticks
!
   CALL pgbox(xxopt,xtickd,nxsubd,yyopt,ytickd,nysubd)
!
!  Add (DD) HH MM SS labels if desired.  Go back to the original user
!  specified options list.
!
   xxopt=' '
   CALL grtoup(xxopt,xopt)
   IF(xtime.AND.(INDEX(xxopt,'N') /= 0.OR.INDEX(xxopt,'M') /= 0)) THEN
      first=.true.
      IF(INDEX(xxopt,'F') /= 0)first=.false.
!
      suptyp='NONE'
      IF(INDEX(xxopt,'D') /= 0)suptyp=' DMS'
      IF(INDEX(xxopt,'H') /= 0)suptyp='DHMS'
!
      do2=.true.
      IF(INDEX(xxopt,'O') /= 0)do2=.false.
!
      dopara=.true.
!
      mod24=.false.
      IF(INDEX(xxopt,'X') /= 0)mod24=.true.
!
      IF(INDEX(xxopt,'N') /= 0)CALL pgtbx4(dodayx,suptyp,'X',  &
         .true.,first,xmin,xmax,tscalx,xtickd,do2,dopara,mod24)
!
      IF(INDEX(xxopt,'M') /= 0)CALL pgtbx4(dodayx,suptyp,'X',  &
         .false.,first,xmin,xmax,tscalx,xtickd,do2,dopara,mod24)
   END IF
!
   yyopt=' '
   CALL grtoup(yyopt,yopt)
   IF(ytime.AND.(INDEX(yyopt,'N') /= 0.OR.INDEX(yyopt,'M') /= 0))THEN
      first=.true.
      IF(INDEX(yyopt,'F') /= 0)first=.false.
!
      suptyp='NONE'
      IF(INDEX(yyopt,'D') /= 0)suptyp=' DMS'
      IF(INDEX(yyopt,'H') /= 0)suptyp='DHMS'
!
      dopara=.true.
      IF(INDEX(yyopt,'V') /= 0)dopara=.false.
!
      do2=.true.
      IF(dopara.AND.INDEX(yyopt,'O') /= 0)do2=.false.
!
      mod24=.false.
      IF(INDEX(yyopt,'X') /= 0)mod24=.true.
!
      IF(INDEX(yyopt,'N') /= 0)CALL pgtbx4(dodayy,suptyp,'Y',  &
         .true.,first,ymin,ymax,tscaly,ytickd,do2,dopara,mod24)
!
      IF(INDEX(yyopt,'M') /= 0)CALL pgtbx4(dodayy,suptyp,'Y',  &
         .false.,first,ymin,ymax,tscaly,ytickd,do2,dopara,mod24)
!
   END IF
!
   RETURN
!
END SUBROUTINE pgtbox
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!
! PGTBX1 -- support routine for PGTBOX
!
SUBROUTINE pgtbx1(axis,doday,dopara,tmin,tmax,tick,nsub,tscale)
!
! Work out what the finest units the time labels will be in and
! return the tick increments if the user does not set them.
!
! This is a support routine for PGTBOX and should not
! be called by the user.
!
! Input:
!  AXIS   :  'X' or 'Y' for use in determining if labels overwrite
!  TMIN   :  Start time in seconds
!  TMAX   :  End   time in seconds
!  DOPARA :  True if label to be parallel to axis, else perpendicular
! Input/output:
!  DODAY  :  Write labels as DD HH MM SS.S else HH MM SS.S with
!            hours ranging above 24.  Useful for declination labels
!  TICK   :  Major tick interval in seconds.  If 0.0 on input, will
!            be set here.
!  NSUB   :  Number of minor ticks between major ticks. If 0 on input
!            will be set here.
! Outputs:
!  TSCALE :  Determines finest unit of labelling
!            (1 => ss, 60 => mm, 3600 => hh, 3600*24 => dd)
!
! 05-Sep-1988 - new routine (Neil Killeen)
! 08-Apr-1991 - correctly work out HH MM SS when the time > 60 h [nebk]
! 20-Apr-1991 - revise to add support for new DD (day) field and
!               do lots of work on tick algorithm [nebk]
! 10-Jun-1993 - deal with user given ticks & rename from PGTIME [nebk/jm]
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=1), INTENT(IN)            :: axis
   LOGICAL, INTENT(OUT)                     :: doday
   LOGICAL, INTENT(IN OUT)                  :: dopara
   REAL(KIND=pg), INTENT(IN OUT)            :: tmin
   REAL(KIND=pg), INTENT(IN OUT)            :: tmax
   REAL(KIND=pg), INTENT(OUT)               :: tick
   INTEGER, INTENT(OUT)                     :: nsub
   INTEGER, INTENT(OUT)                     :: tscale
!
   INTEGER, PARAMETER :: nlist1=19
   INTEGER, PARAMETER :: nlist2=10
   INTEGER, PARAMETER :: nlist3=6
   INTEGER, PARAMETER :: nlist4=8
   INTEGER, PARAMETER :: nticmx=8
!
   REAL(KIND=pg) :: ticks1(nlist1),ticks2(nlist2),ticks3(nlist3),  &
      ticks4(nlist4),tock,tock2,tint,tints,tmins,tmaxs
   INTEGER :: nsubs1(nlist1),nsubs2(nlist2),nsubs3(nlist3),  &
      nsubs4(nlist4),npl,ntick,itick,strlen
   CHARACTER (LEN=15) :: str
!
   SAVE  ticks1,ticks2,ticks3,ticks4
   SAVE  nsubs1,nsubs2,nsubs3,nsubs4
!
   DATA ticks1/0.001_pg,0.002_pg,0.005_pg,0.01_pg,0.02_pg,0.05_pg,0.1_pg,  &
      0.2_pg,0.5_pg,1.0_pg,2.0_pg,3.0_pg,4.0_pg,5.0_pg,6.0_pg,10.0_pg, &
      15.0_pg,20.0_pg,30.0_pg/
   DATA nsubs1/4,4,2,4,4,2,4,4,2,4,4,3,4,5,3,2,3,2,3/
!
   DATA ticks2/1.0_pg,2.0_pg,3.0_pg,4.0_pg,5.0_pg,6.0_pg,10.0_pg,15.0_pg, &
      20.0_pg,30.0_pg/
   DATA nsubs2/4,4,3,4,5,3,2,3,2,3/
!
   DATA ticks3/1.0_pg,2.0_pg,3.0_pg,4.0_pg,6.0_pg,12.0_pg/
   DATA nsubs3/4,4,3,4,3,2/
!
   DATA ticks4/1.0_pg,2.0_pg,3.0_pg,4.0_pg,5.0_pg,6.0_pg,8.0_pg,9.0_pg/
   DATA nsubs4/4,4,3,4,5,3,4,3/
!
!----------------------------------------------------------------------
!
!  Turn off DD (day) field if it has been unnecessarily asked for
!
   IF((ABS(tmin) < 24.0_pg*3600.0_pg).AND.(ABS(tmax) < 24.0_pg*3600.0_pg))doday=.false.
!
!  If a tick size is provided, use it to determine TSCALE
!
   tint=ABS(tmax-tmin)
   tick=ABS(tick)
   IF(ABS(tick) > EPSILON(tick))THEN
      IF(tick >= tint)THEN
         CALL grwarn('PGTBX1: user given tick bigger than time '//  &
            'interval; will auto-tick')
         tick=0.0_pg
      ELSE IF(tick < 0.001_pg)THEN
         CALL grwarn('PGTBX1: user given tick too small (< 1 ms); will auto-tick')
         tick=0.0_pg
      ELSE
         IF(ABS(MOD(tick,60.0_pg)) > EPSILON(tick))THEN
            tscale=1
         ELSE IF(ABS(MOD(tick,3600.0_pg)) > EPSILON(tick))THEN
            tscale=60
         ELSE IF(.NOT.doday)THEN
            tscale=3600
         ELSE IF(ABS(MOD(tick,(24.0_pg*3600.0_pg))) > EPSILON(tick))THEN
            tscale=3600
         ELSE
            tscale=24*3600
         END IF
!
!  Make a simple default for the number of minor ticks and bug out
!
         IF(nsub == 0)nsub=2
         RETURN
      END IF
   END IF
!
!  Work out label units depending on time interval if user
!  wants auto-ticking
!
   IF(tint <= 5*60)THEN
      tscale=1
   ELSE IF(tint <= 5*3600)THEN
      tscale=60
   ELSE
      IF(.NOT.doday)THEN
         tscale=3600
      ELSE
         IF(tint <= 5*24*3600)THEN
            tscale=3600
         ELSE
            tscale=3600*24
         END IF
      END IF
   END IF
!
!CCCC
!  Divide interval into NTICK major ticks and NSUB minor intervals
!  The tick choosing algorithm is not very robust, so watch out
!  if you fiddle anything.
!CCCC
!
   tints=tint/tscale
   IF(tscale == 1)THEN
!
!  Time in seconds.  If the time interval is very small, may need to
!  label with up to 3 decimal places.  Have less ticks to help prevent
!  label overwrite. STR is a dummy tick label to assess label
!  overwrite potential
!
      IF(dopara)THEN
         IF(tints <= 0.01_pg)THEN
            ntick=4
            str='60.423'
            strlen=6
         ELSE IF(tints <= 0.1_pg)THEN
            ntick=5
            str='60.42'
            strlen=5
         ELSE IF(tints <= 1.0_pg)THEN
            ntick=6
            str='60.4'
            strlen=4
         ELSE
            ntick=6
            str='60s'
            strlen=3
         END IF
      ELSE
         ntick=6
         str=' '
         strlen=1
      END IF
      tock=tints/ntick
!
!  Select nearest tick to TOCK from list.
!
      CALL pgtbx2(tock,nlist1,ticks1,nsubs1,tick,nsub,itick)
!
!  Check label overwrite and/or too many ticks.
!
      CALL pgtbx3(doday,0,tscale,tints,nticmx,nlist1,ticks1,  &
         nsubs1,itick,axis,dopara,str(1:strlen),tick,nsub)
   ELSE IF(tscale == 60)THEN
!
!  Time in minutes
!
      ntick=6
      tock=tints/ntick
!
!  Select nearest tick from list
!
      CALL pgtbx2(tock,nlist2,ticks2,nsubs2,tick,nsub,itick)
!
!  Check label overwrite and/or too many ticks.
!
      IF(dopara)THEN
         str='42m'
         strlen=3
      ELSE
         str=' '
         strlen=1
      END IF
      CALL pgtbx3(doday,0,tscale,tints,nticmx,nlist2,ticks2,  &
         nsubs2,itick,axis,dopara,str(1:strlen),tick,nsub)
   ELSE
      IF(tscale == 3600.AND.doday)THEN
!
!  Time in hours with the day field
!
         ntick=6
         tock=tints/ntick
!
!  Select nearest tick from list
!
         CALL pgtbx2(tock,nlist3,ticks3,nsubs3,tick,nsub,itick)
!
!   Check label overwrite and/or too many ticks.
!
         IF(dopara)THEN
            str='42h'
            strlen=3
         ELSE
            str=' '
            strlen=1
         END IF
         CALL pgtbx3(doday,0,tscale,tints,nticmx,nlist3,ticks3,  &
            nsubs3,itick,axis,dopara,str(1:strlen),tick,nsub)
      ELSE
!
!  Time in hours with no day field or time in days. Have less
!  ticks for big numbers or the parallel labels will overwrite.

         IF(dopara)THEN
            tmins=ABS(tmin)/tscale
            tmaxs=ABS(tmax)/tscale
            CALL pgnpl(-1,nint(MAX(tints,tmins,tmaxs)),npl)
            IF(npl <= 3)THEN
               ntick=6
            ELSE IF(npl == 4)THEN
               ntick=5
            ELSE
               ntick=4
            END IF
            str='345678912'
            str(npl+1:)='d'
            strlen=npl+1
         ELSE
            str=' '
            strlen=1
            ntick=6
         END IF
         tock=tints/ntick
!
!   Select nearest tick from list; 1 choose nearest nice integer
!   scaled by the appropriate power of 10
!
         CALL pgnpl(-1,nint(tock),npl)
         tock2=tock/10**(npl-1)
!
         CALL pgtbx2(tock2,nlist4,ticks4,nsubs4,tick,nsub,itick)
         tick=tick*10**(npl-1)
!
!  Check label overwrite and/or too many ticks.
!
         CALL pgtbx3(doday,npl,tscale,tints,nticmx,nlist4,ticks4,  &
            nsubs4,itick,axis,dopara,str(1:strlen),tick,nsub)
      END IF
   END IF
!
!  Convert tick to seconds
!
   tick=tick*tscale
!
   RETURN
!
END SUBROUTINE pgtbx1
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!
! PGTBX2 -- support routine for PGTBOX
!
SUBROUTINE pgtbx2(tock,nticks,ticks,nsubs,tick,nsub,itick)
!
! Find the nearest tick in a list to a given value.
!
! This is a support routine for PGTBOX and should not be called
! by the user.
!
! Input:
!  TOCK   :  Try to find the nearest tick in the list to TOCK
!  NTICKS :  Number of ticks in list
!  TICKS  :  List of ticks
!  NSUBS  :  List of number of minor ticks between ticks to go with TICKS
! Output:
!  TICK   :  The selected tick
!  ITICK  :  The index of the selected tick from the list TICKS
! Input/output
!  NSUB   :  Number of minor ticks between major ticks. If 0 on input
!            will be set here.
!
! 10-Jun-1993 - new routine [nebk]
!-----------------------------------------------------------------------
!
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN OUT)            :: tock
   INTEGER, INTENT(IN)                      :: nticks
   REAL(KIND=pg), INTENT(IN)                :: ticks(nticks)
   INTEGER, INTENT(IN)                      :: nsubs(nticks)
   REAL(KIND=pg), INTENT(OUT)               :: tick
   INTEGER, INTENT(IN OUT)                  :: nsub
   INTEGER, INTENT(OUT)                     :: itick
!
   INTEGER :: i,nsubd
   REAL(KIND=pg) :: dmin,diff
!
   nsubd=nsub
   dmin=1.0E+30_pg
   DO  i=1,nticks
      diff=ABS(tock-ticks(i))
      IF(diff < dmin)THEN
         tick=ticks(i)
         IF(nsubd == 0)nsub=nsubs(i)
         itick=i
!
         dmin=diff
      END IF
   END DO
!
   RETURN
!
END SUBROUTINE pgtbx2
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!
! PGTBX3 -- support routine for PGTBOX
!
SUBROUTINE pgtbx3(doday,npl,tscale,tints,nticmx,nticks,ticks,  &
   nsubs,itick,axis,dopara,str,tick,nsub)
!
! Try to see if label overwrite is going to occur with this tick
! selection, or if there are going to be more than a reasonable
! number of ticks in the displayed time range.  If so, choose,
! if available, the next tick (bigger separation) up in the list.
! If the overwrite requires that we would need to go up to the bext
! TSCALE, give up.  They will need to choose a smaller character size
!
! This is a support routine for PGTBOX and should not
! be called by the user.
!
! Input:
!  DODAY  :  True if day field being used
!  NPL    :  Number of characters needed to format TICK on input
!  TSCALE :  Dictates what the finest units of the labelling are.
!            1 = sec, 60 = min, 3600 = hr, 24*3600 = days
!  TINTS  :  Absolute time interval in units of TSCALE
!  NTICMX :  Max. reasonable number of ticks to allow in the time range
!  NTICKS :  Number of ticks in list of ticks to choose from
!  TICKS  :  List of ticks from which the current tick was chosen
!  NSUBS  :  List of number of minor ticks/major tick to choose NSUB from
!  ITICK  :  Index of chosen tick in list TICKS
!  AXIS   :  'X' or 'Y' axis
!  DOPARA :  Labels parallel or perpendicualr to axis
!  STR    :  A typical formatted string used for checking overwrite
! Input/output:
!  TICK   :  Current major tick interval in units of TSCALE. May be
!            made larger if possible if overwrite likely.
!  NSUB   :  Number of minor ticks between major ticks.
!
! 10-Jun-1993 - new routine [nebk]
!-----------------------------------------------------------------------
!
!
   USE accur
!
   IMPLICIT NONE
!
   LOGICAL, INTENT(IN OUT)                  :: doday
   INTEGER, INTENT(IN)                      :: npl
   INTEGER, INTENT(IN OUT)                  :: tscale
   REAL(KIND=pg), INTENT(IN OUT)            :: tints
   INTEGER, INTENT(IN)                      :: nticmx
   INTEGER, INTENT(IN)                      :: nticks
   REAL(KIND=pg), INTENT(IN)                :: ticks(nticks)
   INTEGER, INTENT(IN)                      :: nsubs(nticks)
   INTEGER, INTENT(IN OUT)                  :: itick
   CHARACTER (LEN=1), INTENT(IN)            :: axis
   LOGICAL, INTENT(IN OUT)                  :: dopara
   CHARACTER (LEN=*), INTENT(IN OUT)        :: str
   REAL(KIND=pg), INTENT(OUT)               :: tick
   INTEGER, INTENT(OUT)                     :: nsub
!
   INTEGER :: ntick
   REAL(KIND=pg) :: lens,lenx,leny
!
   CALL pglen(4,str,lenx,leny)
   lens=lenx
   IF((dopara.AND.axis == 'Y').OR.(.NOT.dopara.AND.axis == 'X'))lens=leny
!
   IF(tscale == 1.OR.tscale == 60.OR.(tscale == 3600.AND.doday))THEN
!
!  Time in seconds or minutes, or in hours with a day field
!
      ntick=INT(tints/tick)
      IF((itick < nticks).AND.((dopara.AND.(lens/tscale) > 0.9_pg*  &
         tick).OR.(ntick > nticmx)))THEN
         IF(ticks(itick+1) < tints)THEN
            nsub=nsubs(itick+1)
            tick=ticks(itick+1)
         END IF
      END IF
   ELSE
!
!  Time in hours and no day field or time in days
!
      ntick=INT(tints/tick)
      IF((dopara.AND.(lens/tscale) > 0.9_pg*tick).OR.(ntick > nticmx))THEN
         IF(itick < nticks)THEN
            IF(ticks(itick+1)*10**(npl-1) < tints)THEN
               nsub=nsubs(itick+1)
               tick=ticks(itick+1)*10**(npl-1)
            END IF
         ELSE
            IF(ticks(1)*10**npl < tints)THEN
               nsub=nsubs(1)
               tick=ticks(1)*10**npl
            END IF
         END IF
      END IF
   END IF
!
   RETURN
!
END SUBROUTINE pgtbx3
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!
! PGTBX4 -- support routine for PGTBOX
!
SUBROUTINE pgtbx4(doday,suptyp,axis,convtl,first,tmin,tmax,  &
   tscale,tick,do2,dopara,mod24)
!
! Label an axis in (DD) HH MM SS.S style.    This is the main
! workhorse of the PGTBOX routines.
!
! This is a support subroutine for PGTBOX and should not be
! called by the user.
!
! Inputs:
!  DODAY  :  Write labels as DD HH MM SS.S else HH MM SS.S with
!            hours ranging above 24.  Useful for declination labels
!  SUPTYP :  If 'DHMS' then superscript the fields with d, h, m, & s
!            If ' DMS' then superscript the fields with    o, '  & ''
!              Good for declination plots.  You should obviously not
!              ask for the day field for this to do anything sensible.
!            If '    ' then no superscripting is done.
!  AXIS   :  'X' for x-axis, 'Y' for y-axis
!  CONVTL :  If .true., write the labels in the conventional axis
!            locations (bottom and left for 'X' and 'Y').  Otherwise
!            write them on the top and right axes ('X' and 'Y')
!  FIRST  :  If .false. then omit the first label.
!  TMIN   :  Start time (seconds)
!  TMAX   :  End time (seconds)
!  TSCALE :  Determines finest units of axis
!              1 => ss, 60 => mm, 3600 => hh, 3600*24 => dd
!  TICK   :  Major tick interval in seconds
!  DO2    :  If .true., write labels less than 10 with a leading zero.
!  DOPARA :  Y axis label parallel to axis, else perpendicular
!  MOD24  :  HH field labelled as modulo 24
!
! 05-Sep-1988 - new routine (Neil Killeen)
! 20-Apr-1991 - add support for new DD (day) field [nebk]
! 10-Jun-1993 - complete rewrite & rename from PGTLAB. Fixes user given
!               ticks bug too [nebk]
! 15-Jan-1995 - Add argument MOD24
!-----------------------------------------------------------------------
!
!
   USE accur
!
   IMPLICIT NONE
!
   LOGICAL, INTENT(IN OUT)                  :: doday
   CHARACTER (LEN=*), INTENT(IN OUT)        :: suptyp
   CHARACTER (LEN=*), INTENT(IN)            :: axis
   LOGICAL, INTENT(IN)                      :: convtl
   LOGICAL, INTENT(IN OUT)                  :: first
   REAL(KIND=pg), INTENT(IN)                :: tmin
   REAL(KIND=pg), INTENT(IN)                :: tmax
   INTEGER, INTENT(IN OUT)                  :: tscale
   REAL(KIND=pg), INTENT(IN)                :: tick
   LOGICAL, INTENT(IN OUT)                  :: do2
   LOGICAL, INTENT(IN OUT)                  :: dopara
   LOGICAL, INTENT(IN OUT)                  :: mod24
!
   INTEGER, PARAMETER :: maxtik=1000
!LOGICAL, PARAMETER :: t=.true.
   LOGICAL, PARAMETER :: f=.false.
!
   REAL(KIND=pg) :: ss(maxtik),tfrac(maxtik)
   INTEGER :: dd(maxtik),hh(maxtik),mm(maxtik)
   CHARACTER (LEN=1) :: asign(maxtik),asignl
!
   REAL(KIND=pg) :: time,xlen,ylen,coord,fjust,rval,ssl,disp,xlen2,ylen2
   INTEGER :: is,sd,nt,izero,ipos,ineg,it,i,j,k,sprec,jst(2),jend(2),  &
      tlen,last,ival(3),ivalo(3),ivalz(3),ivalf(3),ivall(3),npass,inc,ddl,hhl,mml
   CHARACTER (LEN=1) :: signf
   CHARACTER (LEN=80) :: text
   CHARACTER (LEN=2) :: axloc
   LOGICAL :: writ(4)
!
   CALL pgbbuf
!
!  Direction signs
!
   sd=1
   IF(tmax < tmin)sd=-1
   is=1
   IF(tmin < 0.0)is=-1
!
!  Find first tick.  Return if none.
!
   nt=INT(tmin/tick)
   IF(is*sd == 1.AND.ABS(tmin) > ABS(nt)*tick)nt=nt+sd
   time=nt*tick
   IF((sd == 1.AND.(time < tmin.OR.time > tmax)).OR.(sd == -1  &
      .AND.(time > tmin.OR.time < tmax)))RETURN
!
!  Now step through time range in TICK increments and convert
!  times in seconds at each tick to  +/- (DD) HH MM SS.S
!
   izero=0
   it=1
10 IF((sd == 1.AND.time > (tmax+1.0E-05_pg)).OR.(sd == -1.AND.time  &
      .LT.(tmax-1.0E-5)))GO TO 20
   IF(it > maxtik)THEN
      CALL grwarn ('PGTBX4: storage exhausted -- you have'// &
         'asked for far too many ticks')
      GO TO 20
   END IF
!
!  Convert to (DD) HH MM SS.S and find fraction of window that this
!  tick falls at
!
   CALL pgtbx5(doday,time,asign(it),dd(it),hh(it),mm(it),ss(it))
   tfrac(it)=(time-tmin)/(tmax-tmin)
!
!  Note zero tick
!
   IF(nt == 0)izero=it
!
!  Increment time
!
   nt=nt+sd
   time=nt*tick
   it=it+1
!
   GO TO 10
20 CONTINUE
   it=it-1
!
!   Work out the precision with which to write fractional seconds
!   labels into the SS.S field.   All other fields have integer labels.
!
   sprec=0
   IF(tscale == 1)THEN
      IF(tick < 0.01_pg)THEN
         sprec=3
      ELSE IF(tick < 0.1_pg)THEN
         sprec=2
      ELSE IF(tick < 1.0_pg)THEN
         sprec=1
      END IF
   END IF
!
!  Label special case of first tick.  Prepare fields and label
!
   CALL pgtbx6(doday,mod24,tscale,dd(1),hh(1),mm(1),ss(1),ivalf,rval,writ)
   signf='H'
   IF(doday)signf='D'
   CALL pgtbx7(suptyp,signf,asign(1),ivalf,rval,writ,sprec,do2,text,tlen,last)
!
!   Set label displacements from axes.  This is messy for labels oriented
!   perpendicularly on the right hand axis as we need to know how long
!   the longest string we are going to write is before we write any
!   labels as they are right justified.
!
   IF(axis == 'X')THEN
      IF(convtl)THEN
         axloc='B'
         IF(suptyp /= 'NONE')THEN
            disp=1.4_pg
         ELSE
            disp=1.2_pg
         END IF
      ELSE
         axloc='T'
         disp=0.7_pg
      END IF
   ELSE IF(axis == 'Y')THEN
      IF(convtl)THEN
         axloc='LV'
         IF(dopara)axloc='L'
         disp=0.7_pg
      ELSE
         IF(dopara)THEN
            axloc='R'
            IF(suptyp /= 'NONE')THEN
               disp=1.7_pg
            ELSE
               disp=1.9_pg
            END IF
         ELSE
!
!  Work out number of characters in first label
!
            axloc='RV'
            IF(asign(1) /= '-'.AND.tmin*tmax < 0.0_pg)THEN
               CALL pglen (2,' -'//text(1:tlen),xlen,ylen)
            ELSE
               CALL pglen (2,' '//text(1:tlen),xlen,ylen)
            END IF
            CALL pgqcs (2,xlen2,ylen2)
            disp=(xlen/xlen2)
         END IF
      END IF
   END IF
!
!  Now write the label to the plot.  The X-axis label for the first tick is
!  centred such that the last field of the label is centred on the tick
!
   IF(first)THEN
      CALL pglen(5,text(last:tlen),xlen,ylen)
!
      IF(axis == 'X')THEN
         coord=tfrac(1)+xlen/2.0_pg
         fjust=1.0_pg
      ELSE IF(axis == 'Y')THEN
         IF(dopara)THEN
            coord=tfrac(1)+ylen/2.0_pg
            fjust=1.0_pg
         ELSE
            fjust=1.0_pg
            coord=tfrac(1)
         END IF
      END IF
      CALL pgmtxt(axloc,disp,coord,fjust,text(1:tlen))
   END IF
   IF(it == 1)RETURN
!
!   Designate which field out of DD or HH will carry the sign, depending
!   on whether you want the day field or not for the rest of the ticks
!
   signf='H'
   IF(doday)signf='D'
!
!  Set up labelling justifications for the rest of the labels
!
   IF(axis == 'X')THEN
      fjust=0.5_pg
   ELSE IF(axis == 'Y')THEN
      IF(dopara)THEN
         fjust=0.5_pg
      ELSE
         fjust=1.0_pg
      END IF
   END IF
!
!  Note zero crossings; IPOS is the first positive tick and
!  INEG is the first negative tick on either side of 0
!
   ipos=0
   ineg=0
!
   IF(izero /= 0)THEN
      j=izero-1
      IF(j >= 1)THEN
         IF(asign(j) == '-')THEN
            ineg=j
         ELSE IF(asign(j) == ' ')THEN
            ipos=j
         END IF
      END IF
      j=izero+1
      IF(j <= it)THEN
         IF(asign(j) == '-')THEN
            ineg=j
         ELSE IF(asign(j) == ' ')THEN
            ipos=j
         END IF
      END IF
   END IF
!
!  Now label special case of zero tick. It carries the sign change
!  when going from positive to negative time, left to right.
!
   IF(izero /= 0.AND.izero /= 1)THEN
      CALL pgtbx6(doday,mod24,tscale,dd(izero),hh(izero),  &
         mm(izero),ss(izero),ivalz,rval,writ)
!
      IF(asign(izero-1) == ' ')asign(izero)='-'
      CALL pgtbx7(suptyp,signf,asign(izero),ivalz,rval,writ,  &
         sprec,do2,text,tlen,last)
!
      coord=tfrac(izero)
      CALL pgmtxt(axloc,disp,coord,fjust,text(1:tlen))
   END IF
!
!   We may need an extra "virtual" tick if there is no zero crossing
!   and SD=-1 & IS=1 or SD=1 & IS=-1.  It is used to work out which
!   fields to label on the right most tick which is labelled first.
!
   IF(izero == 0)THEN
      IF(sd*is == -1)THEN
         IF((sd == -1.AND.time <= 0.0_pg).OR.(sd == 1.AND.time >= 0.0_pg))time=0.0_pg
         CALL pgtbx5(doday,time,asignl,ddl,hhl,mml,ssl)
         CALL pgtbx6(doday,mod24,tscale,ddl,hhl,mml,ssl,ivall,rval,writ)
      END IF
   END IF
!
!  We want to label in the direction(s) away from zero, so we may  need
!  two passes. Determine the start and end ticks for each required pass.
!
   jst(2)=0
   jend(2)=0
   npass=1
   IF(izero == 0)THEN
      IF(is*sd == 1)THEN
         jst(1)=1
         jend(1)=it
      ELSE
         jst(1)=it
         jend(1)=1
      END IF
   ELSE
      IF(ineg == 0.OR.ipos == 0)THEN
         jst(1)=izero
         jend(1)=it
         IF(izero == it)jend(1)=1
      ELSE
         npass=2
         jst(1)=izero
         jend(1)=1
         jst(2)=izero
         jend(2)=it
      END IF
   END IF
!
!  Now label the rest of the ticks.  Always label away from 0
!
   DO  i=1,npass
!
!  Initialize previous tick values.  Use virtual tick if labelling
!  left to right without a zero (one pass)
!
      DO  k=1,3
         ivalo(k)=ivalz(k)
         IF(izero == 0)THEN
            ivalo(k)=ivall(k)
            IF(jst(i) == 1)ivalo(k)=ivalf(k)
         END IF
      END DO
!
      inc=1
      IF(jend(i) < jst(i))inc=-1
      DO  j=jst(i),jend(i),inc
!
!  First and zero tick already labelled
!
         IF(j /= 1.AND.j /= izero)THEN
!
!  Prepare fields
!
            CALL pgtbx6(doday,mod24,tscale,dd(j),hh(j),mm(j),ss(j),ival,rval,writ)
!
!  Don't write unchanging fields
!
            DO  k=1,3
               IF(ival(k) == ivalo(k))writ(k)=f
            END DO
!
!  Prepare label
!
            CALL pgtbx7(suptyp,signf,asign(j),ival,rval,writ,sprec,  &
               do2,text,tlen,last)
!
!  Write label
!
            coord=tfrac(j)
            CALL pgmtxt(axloc,disp,coord,fjust,text(1:tlen))
!
!  Update old values
!
            DO  k=1,3
               ivalo(k)=ival(k)
            END DO
         END IF
      END DO
   END DO
   CALL pgebuf
!
   RETURN
!
END SUBROUTINE pgtbx4
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!
! PGTBX5 -- support routine for PGTBOX
!
SUBROUTINE pgtbx5(doday,tsec,asign,d,h,m,s)
!
!  Convert time in seconds to (DD) HH MM SS.S
!
! Input
!  DODAY  :  Use day field if true, else hours accumulates beyond 24
!  TSEC   :  Time in seconds (signed)
! Output
!  ASIGN  :  Sign, ' ' or '-'
!  D,H,M  :  DD, HH, MM (unsigned)
!  S      :  SS.S       (unsigned)
!
! 10-Jun-1993 - new routine [nebk]
!-----------------------------------------------------------------------
!
!
   USE accur
!
   IMPLICIT NONE
!
   LOGICAL, INTENT(IN OUT)                  :: doday
   REAL(KIND=pg), INTENT(IN OUT)            :: tsec
   CHARACTER (LEN=1), INTENT(OUT)           :: asign
   INTEGER, INTENT(OUT)                     :: d
   INTEGER, INTENT(OUT)                     :: h
   INTEGER, INTENT(OUT)                     :: m
   REAL(KIND=pg), INTENT(OUT)               :: s
!
   INTEGER :: it
!
   asign=' '
   IF(tsec < 0.0)asign='-'
!
   s=MOD(ABS(tsec),60.0_pg)
!
   it=nint(ABS(tsec)-s)/60
   m=MOD(it,60)
!
   it=(it-m)/60
   IF(doday)THEN
      h=MOD(it,24)
      d=(it-h)/24
   ELSE
      h=it
      d=0
   END IF
!
   RETURN
!
END SUBROUTINE pgtbx5
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!
! PGTBX6 -- support routine for PGTBOX
!
SUBROUTINE pgtbx6(doday,mod24,tscale,dd,hh,mm,ss,ival,rval,writ)
!
!   Find out which of the DD HH MM SS.S fields we want to write
!   into the label according to TSCALE and make a round off
!   error check.
!
!  Input:
!    DODAY  :  Use day field if true else hours accrue beyond 24
!    MOD24  :  HH field labelled as modulo 24
!    TSCALE :  Dictates which fields appear in labels
!    DD     :  Day of time  (will be 0 if DODAY=F and HH will compensate)
!    HH     :  Hour of time
!    MM     :  Minute of time
!    SS     :  Second of time
!  Output:
!    IVAL(3):  DD HH MM to write into label
!    RVAL   :  SS.S to write into label
!    WRIT(4):  T or F if DD,HH,MM,SS are to be written into the label
!              or not.  IVAL and RVAL fields are set explicitly to
!              zero if the corresponding WRIT field is false.
!              This really is overkill.
!
!  10-Jun-1993 - New routine [nebk]
!  16-Jan-1995 - Add argument MOD24
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   LOGICAL, INTENT(IN)                      :: doday
   LOGICAL, INTENT(IN OUT)                  :: mod24
   INTEGER, INTENT(IN OUT)                  :: tscale
   INTEGER, INTENT(IN)                      :: dd
   INTEGER, INTENT(IN)                      :: hh
   INTEGER, INTENT(IN)                      :: mm
   REAL(KIND=pg), INTENT(IN)                :: ss
   INTEGER, INTENT(OUT)                     :: ival(3)
   REAL(KIND=pg), INTENT(OUT)               :: rval
   LOGICAL, INTENT(OUT)                     :: writ(4)
!
   LOGICAL, PARAMETER :: t=.true.
   LOGICAL, PARAMETER :: f=.false.
   INTEGER :: wm
!
   ival(1)=dd
   ival(2)=hh
   ival(3)=mm
   rval=ss
!
!  SS should be 0.0; round off may get us 59.999 or the like but
!  not 60.001 (see PGTBX5)
!
   IF(tscale > 1)THEN
      wm=nint(ss/60.0_pg)
      ival(3)=ival(3)+wm
      IF(ival(3) == 60)THEN
         ival(3)=0
         ival(2)=ival(2)+1
         IF(doday.AND.ival(2) == 24)THEN
            ival(2)=0
            ival(1)=ival(1)+1
         END IF
      END IF
   END IF
!
! Make HH field modulo 24 if desired
!
   IF(mod24)ival(2)=MOD(ival(2),24)
!
   IF(tscale == 1)THEN
!
!  Label contains (DD) HH MM SS.S
!
      writ(1)=doday
      writ(2)=t
      writ(3)=t
      writ(4)=t
   ELSE IF(tscale == 60)THEN
!
!  Label contains (DD) HH MM
!
      writ(1)=doday
      writ(2)=t
      writ(3)=t
!
      rval=0.0_pg
      writ(4)=f
   ELSE IF(tscale == 3600)THEN
!
!  Label contains (DD) HH
!
      writ(1)=doday
      writ(2)=t
!
      ival(3)=0
      writ(3)=f
!
      rval=0.0_pg
      writ(4)=f
   ELSE IF(tscale == 3600*24)THEN
!
!  Label contains DD
!
      writ(1)=t
!
      ival(2)=0
      writ(2)=f
!
      ival(3)=0
      writ(3)=f
!
      rval=0.0_pg
      writ(4)=f
   END IF
!
   RETURN
!
END SUBROUTINE pgtbx6
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pgtbx7(suptyp,signf,asign,ival,rval,writ,sprec,do2,text,tlen,last)
!
! Write (DD) HH MM SS.S time labels into a string
!
! This is a support routine for PGTBOX and should not be
! called by the user
!
! Inputs
!  SUPTYP :  '    ', 'DHMS', or ' DMS' for no superscript labelling,
!            d,h,m,s   or   o,','' superscripting
!  SIGNF  :  Tells which field the sign is associated with.
!            One of 'D', 'H', 'M', or 'S'
!  ASIGN  :  ' ' or '-' for positive or negative times
!  IVAL(3):  Day, hour, minutes of time
!  RVAL   :  Seconds of time
!  WRIT(4):  If .true. then write DD, HH, MM, SS  into label
!  SPREC  :  Number of places after the decimal to write seconds
!            string to.  Must be in the range 0-3
!  DO2    :  If true, add a leading zero to numbers < 10
! Outputs
!  TEXT   :  Label
!  TLEN   :  Length of label
!  LAST   :  Is the location of the start character of the last
!            field written into TEXT
!
!  05-Sep-1989 -- New routine (Neil Killeen)
!  20-Apr-1991 -- Complete rewrite; support for new DD (day) field and
!                 superscripted labels [nebk]
!  14-May-1991 -- Removed BSL as a parameter (Char(92)) and made it
!                 a variable to appease Cray compiler [mjs/nebk]
!  10-Jun-1993 -- Rename from PGTLB1, add code to label superscript
!                 seconds above the '.' and add DO2 option [nebk/jm]
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=4), INTENT(IN OUT)        :: suptyp
   CHARACTER (LEN=1), INTENT(IN OUT)        :: signf
   CHARACTER (LEN=1), INTENT(IN)            :: asign
   INTEGER, INTENT(IN OUT)                  :: ival(3)
   REAL(KIND=pg), INTENT(IN OUT)            :: rval
   LOGICAL, INTENT(IN OUT)                  :: writ(4)
   INTEGER, INTENT(IN)                      :: sprec
   LOGICAL, INTENT(IN OUT)                  :: do2
   CHARACTER (LEN=*), INTENT(OUT)           :: text
   INTEGER, INTENT(OUT)                     :: tlen
   INTEGER, INTENT(OUT)                     :: last
!
   INTEGER :: flen,fst,fmax,trlen(3),suppnt,tmpnt,tlen2,ir1,ir2,ip
   CHARACTER (LEN=30) :: field,frmat
   CHARACTER (LEN=2) :: frmat2(3)
   CHARACTER (LEN=11) :: super(4,3)
   CHARACTER (LEN=100) :: tmp
   CHARACTER (LEN=1) :: bsl
!
   SAVE  frmat2
   SAVE  trlen
!
   DATA frmat2/'I1','I2','I3'/
   DATA trlen/5,11,5/
!
!   Initialize
!
   bsl=CHAR(92)
   tlen=0
   text=' '
!
!   Assign superscripting strings.  Use CHAR(92) for backslash as the
!   latter must be escaped on SUNs thus requiring preprocessing.  The
!   concatenator operator precludes the use of a data statement
!
   super(1,1)=bsl//'ud'//bsl//'d'
   super(2,1)=bsl//'uh'//bsl//'d'
   super(3,1)=bsl//'um'//bsl//'d'
   super(4,1)=bsl//'us'//bsl//'d'
!
   super(1,2)=bsl//'u'//bsl//'(2199)'//bsl//'d'
   super(2,2)=bsl//'u'//bsl//'(2729)'//bsl//'d'
   super(3,2)=bsl//'u'//bsl//'(2727)'//bsl//'d'
   super(4,2)=bsl//'u'//bsl//'(2728)'//bsl//'d'
!
   super(1,3)=bsl//'u'//' '//bsl//'d'
   super(2,3)=bsl//'u'//' '//bsl//'d'
   super(3,3)=bsl//'u'//' '//bsl//'d'
   super(4,3)=bsl//'u'//' '//bsl//'d'
!
!   Point at correct superscript strings
!
   IF(suptyp == 'DHMS')THEN
      suppnt=1
   ELSE IF(suptyp == ' DMS')THEN
      suppnt=2
   ELSE
      suppnt=3
   END IF
!
!CCC
!   Days field
!CCC
!
   IF(writ(1))THEN
      last=tlen+1
!
!   Write into temporary field
!
      field=' '
      CALL pgnpl (0,ival(1),flen)
      WRITE(field,'(I6)') ival(1)
      fmax=6
      fst=fmax-flen+1
!
!   Write output text string with desired superscripting
!
      tmpnt=2
      IF(signf == 'D'.AND.asign /= ' ')tmpnt=1
!
      tmp=asign//field(fst:fmax)//super(1,suppnt)
      tlen2=(2-tmpnt)+flen+trlen(suppnt)
!
      text(tlen+1:)=tmp(tmpnt:tmpnt+tlen2-1)
      tlen=tlen+tlen2
   END IF
!
!CCC
!   Hours field
!CCC
!
   IF(writ(2))THEN
      last=tlen+1
!
!   Write into temporary field
!
      field=' '
      CALL pgnpl(0,ival(2),flen)
      WRITE(field,'(I6)') ival(2)
      fmax=6
      fst=fmax-flen+1
!
      IF(do2.AND.flen == 1)THEN
         flen=flen+1
         fst=fst-1
         field(fst:fst)='0'
      END IF
!
!   Write output text string with desired superscripting
!
      tmpnt=2
      IF(signf == 'H'.AND.asign /= ' ')tmpnt=1
!
      tmp=asign//field(fst:fmax)//super(2,suppnt)
      tlen2=(2-tmpnt)+flen+trlen(suppnt)
!
      text(tlen+1:)=tmp(tmpnt:tmpnt+tlen2-1)
      tlen=tlen+tlen2
   END IF
!
!CCC
!   Minutes field
!CCC
!
   IF(writ(3))THEN
      last=tlen+1
!
!   Write into temporary field with desired superscripting
!
      field=' '
      WRITE(field,'(I2, A)')ival(3),super(3,suppnt) (1:trlen(suppnt))
      fmax=2+trlen(suppnt)
!
      fst=1
      IF(field(fst:fst) == ' ')THEN
         IF(do2)THEN
            field(fst:fst)='0'
         ELSE
            fst=fst+1
         END IF
      END IF
      flen=fmax-fst+1
!
!   Write output text string
!
      tmpnt=2
      IF(signf == 'M'.AND.asign /= ' ')tmpnt=1
!
      tmp=asign//field(fst:fmax)
      tlen2=(2-tmpnt)+flen
!
      text(tlen+1:)=tmp(tmpnt:tmpnt+tlen2-1)
      tlen=tlen+tlen2
   END IF
!
!CCC
!   Seconds field
!CCC
!
   IF(writ(4))THEN
      last=tlen+1
!
!   Write into temporary field
!
      field=' '
      fst=1
      IF(sprec >= 1)THEN
!
!   Fractional label.  Upto 3 places after the decimal point allowed
!   Muck around to get the superscript on top of the decimal point
!
         ir1=INT(rval)
         ir2=nint((rval-ir1)*10**sprec)
         frmat='(I2, A1, A, '//frmat2(sprec)//')'
         WRITE(field,frmat(1:15))ir1,'.',bsl//'b'//super(4,suppnt)  &
            (1:trlen(suppnt)),ir2
         ip=5+trlen(suppnt)+1
         IF(field(ip:ip) == ' ')field(ip:ip)='0'
         IF(field(ip+1:ip+1) == ' ')field(ip+1:ip+1)='0'
         fmax=1+2+sprec
      ELSE
!
!   Integer label.
!
         WRITE(field,'(I2,A)')nint(rval),super(4,suppnt) (1:trlen(suppnt))
         fmax=0
      END IF
      fmax=fmax+2+trlen(suppnt)
!
      IF(field(fst:fst) == ' ')THEN
         IF(do2)THEN
            field(fst:fst)='0'
         ELSE
            fst=fst+1
         END IF
      END IF
      flen=fmax-fst+1
!
!   Write output text string
!
      tmpnt=2
      IF(signf == 'S'.AND.asign /= ' ')tmpnt=1
      tmp=asign//field(fst:fmax)
      tlen2=(3-tmpnt)+flen
!
      text(tlen+1:)=tmp(tmpnt:tmpnt+tlen2-1)
      tlen=tlen+tlen2
   END IF
!
!   A trailing blank will occur if no superscripting wanted
!
   IF(tlen >= 5.AND.text(tlen-4:tlen) == bsl//'u'//' '//bsl//'d') tlen=tlen-5
!
   RETURN
!
END SUBROUTINE pgtbx7
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGTEXT -- write text (horizontal, left-justified)
!%void cpgtext(float x, float y, const char *text);
!+
SUBROUTINE pgtext(x,y,text)
!
! Write text. The bottom left corner of the first character is placed
! at the specified position, and the text is written horizontally.
! This is a simplified interface to the primitive routine PGPTXT.
! For non-horizontal text, use PGPTXT.
!
! Arguments:
!  X      (input)  : world x-coordinate of start of string.
!  Y      (input)  : world y-coordinate of start of string.
!  TEXT   (input)  : the character string to be plotted.
!--
! (2-May-1983)
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)            :: x
   REAL(KIND=pg), INTENT(IN)            :: y
   CHARACTER (LEN=*), INTENT(IN)        :: text
!
   CALL pgptxt(x,y,0.0_pg,0.0_pg,text)
!
   RETURN
!
END SUBROUTINE pgtext
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!

!*PGTICK -- draw a single tick mark on an axis
!%void cpgtick(float x1, float y1, float x2, float y2, float v, \
!% float tikl, float tikr, float disp, float orient, const char *str);
!+
SUBROUTINE pgtick(x1,y1,x2,y2,v,tikl,tikr,disp,orient,str)
!
! Draw and label single tick mark on a graph axis. The tick mark is
! a short line perpendicular to the direction of the axis (which is not
! drawn by this routine). The optional text label is drawn with its
! baseline parallel to the axis and reading in the same direction as
! the axis (from point 1 to point 2). Current line and text attributes
! are used.
!
! Arguments:
!  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
!  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
!  V      (input)  : draw the tick mark at fraction V (0<=V<=1) along
!                    the line from (X1,Y1) to (X2,Y2).
!  TIKL   (input)  : length of tick mark drawn to left of axis
!                    (as seen looking from first endpoint to second), in
!                    units of the character height.
!  TIKR   (input)  : length of major tick marks drawn to right of axis,
!                    in units of the character height.
!  DISP   (input)  : displacement of label text to
!                    right of axis, in units of the character height.
!  ORIENT (input)  : orientation of label text, in degrees; angle between
!                    baseline of text and direction of axis (0-360°).
!  STR    (input)  : text of label (may be blank).
!--
! 25-Mar-1997 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: x1
   REAL(KIND=pg), INTENT(IN)                :: y1
   REAL(KIND=pg), INTENT(IN)                :: x2
   REAL(KIND=pg), INTENT(IN)                :: y2
   REAL(KIND=pg), INTENT(IN)                :: v
   REAL(KIND=pg), INTENT(IN)                :: tikl
   REAL(KIND=pg), INTENT(IN)                :: tikr
   REAL(KIND=pg), INTENT(IN)                :: disp
   REAL(KIND=pg), INTENT(IN)                :: orient
   CHARACTER (LEN=*), INTENT(IN)            :: str
!
   REAL(KIND=pg) :: x,y,xv1,xv2,yv1,yv2,xw1,xw2,yw1,yw2
   REAL(KIND=pg) :: xpmm,ypmm,lenmm,angle,xch,ych
   REAL(KIND=pg) :: tikx,tiky,fjust,d,orien
!
! Check arguments.
!
   IF(ABS(x1 - x2) <= EPSILON(x1).AND.ABS(y1 - y2) <= EPSILON(y1))RETURN
!
! Get current character height (mm) [note: XCH = YCH].
!
   CALL pgqcs(2,xch,ych)
!
! Get x and y scales (units per mm).
!
   CALL pgqvp(2,xv1,xv2,yv1,yv2)
   CALL pgqwin(xw1,xw2,yw1,yw2)
   xpmm=(xw2-xw1)/(xv2-xv1)
   ypmm=(yw2-yw1)/(yv2-yv1)
!
! Length of axis in mm.
!
   lenmm=SQRT(((x2-x1)/xpmm)**2+((y2-y1)/ypmm)**2)
!
! Angle of axis to horizontal (device coordinates).
!
   angle=ATAN2((y2-y1)/ypmm,(x2-x1)/xpmm)*57.295779513082321_pg
!
! (x,y) displacement for 1 character height perpendicular to axis.
!
   tikx=(y1-y2)*xch*xpmm/(lenmm*ypmm)
   tiky=(x2-x1)*xch*ypmm/(lenmm*xpmm)
!
! Draw the tick mark at point (X,Y) on the axis.
!
   x=x1+v*(x2-x1)
   y=y1+v*(y2-y1)
   CALL pgmove (x-tikr*tikx,y-tikr*tiky)
   CALL pgdraw (x+tikl*tikx,y+tikl*tiky)
!
! Label the tick mark.
!
   d=disp
   IF(str == ' ')RETURN
   orien=MOD(orient,360.0_pg)
   IF(orien < 0.0_pg)orien=orien+360.0_pg
   IF(orien > 45.0_pg.AND.orien <= 135.0_pg)THEN
      fjust=0.0_pg
      IF(d < 0.0_pg)fjust=1.0_pg
   ELSE IF(orien > 135.0_pg.AND.orien <= 225.0_pg)THEN
      fjust=0.5_pg
      IF(d < 0.0_pg)d=d-1.0_pg
   ELSE IF(orien > 225.0_pg.AND.orien <= 315.0_pg)THEN
      angle=angle+90.0_pg
      fjust=1.0_pg
      IF(d < 0.0_pg)fjust=0.0_pg
   ELSE
      fjust=0.5_pg
      IF(d > 0.0_pg)d=d+1.0_pg
   END IF
   CALL pgptxt(x-d*tikx,y-d*tiky,angle-orien,fjust,str)
!
   RETURN
!
END SUBROUTINE pgtick
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!.PGTIKL -- length of error bar terminal
!
SUBROUTINE pgtikl (t,xl,yl)
!
! Return the length of the terminal of an error bar, in world
! coordinates.
!
! Arguments:
!  T      (input)  : terminal multiplier
!  XL     (output) : terminal lnegth in world x-coordinates
!  YL     (output) : terminal lnegth in world y-coordinates
!--
! 31-Mar-1997 - new routine [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                :: t
   REAL(KIND=pg), INTENT(OUT)               :: xl
   REAL(KIND=pg), INTENT(OUT)               :: yl
!
   xl=t*pgxsp(pgid)*0.15_pg/pgxscl(pgid)
   yl=t*pgxsp(pgid)*0.15_pg/pgyscl(pgid)
!
   RETURN
!
END SUBROUTINE pgtikl
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGUPDT -- update display
!%void cpgupdt(void);
!+
SUBROUTINE pgupdt
!
! Update the graphics display: flush any pending commands to the
! output device. This routine empties the buffer created by PGBBUF,
! but it does not alter the PGBBUF/PGEBUF counter. The routine should
! be called when it is essential that the display be completely up to
! date (before interaction with the user, for example) but it is not
! known if output is being buffered.
!
! Arguments: none
!--
! 27-Nov-1986
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGUPDT'))RETURN
   CALL grterm
!
   RETURN
!
END SUBROUTINE pgupdt
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGVECT -- vector map of a 2D data array, with blanking
!%void cpgvect(const float *a, const float *b, int idim, int jdim, \
!% int i1, int i2, int j1, int j2, float c, int nc, \
!% const float *tr, float blank);
!+
SUBROUTINE pgvect(a,b,idim,jdim,i1,i2,j1,j2,c,nc,tr,blnk)
!
! Draw a vector map of two arrays.  This routine is similar to
! PGCONB in that array elements that have the "magic value" defined by
! the argument blnk are ignored, making gaps in the vector map.  The
! routine may be useful for data measured on most but not all of the
! points of a grid. Vectors are displayed as arrows; the style of the
! arrowhead can be set with routine PGSAH, and the the size of the
! arrowhead is determined by the current character size, set by PGSCH.
!
! Arguments:
!  A      (input)  : horizontal component data array.
!  B      (input)  : vertical component data array.
!  IDIM   (input)  : first dimension of A and B.
!  JDIM   (input)  : second dimension of A and B.
!  I1,I2  (input)  : range of first index to be mapped (inclusive).
!  J1,J2  (input)  : range of second index to be mapped (inclusive).
!  C      (input)  : scale factor for vector lengths, if 0.0, C will be
!                    set so that the longest vector is equal to the
!                    smaller of TR(2)+TR(3) and TR(5)+TR(6).
!  NC     (input)  : vector positioning code.
!                    <0 vector head positioned on coordinates
!                    >0 vector base positioned on coordinates
!                    =0 vector centered on the coordinates
!  TR     (input)  : array defining a transformation between the I,J
!                    grid of the array and the world coordinates. The
!                    world coordinates of the array point A(I,J) are
!                    given by:
!                      X = TR(1) + TR(2)*I + TR(3)*J
!                      Y = TR(4) + TR(5)*I + TR(6)*J
!                    Usually TR(3) and TR(5) are zero - unless the
!                    coordinate transformation involves a rotation
!                    or shear.
!  blnk   (input) : elements of arrays A or B that are exactly equal to
!                    this value are ignored (blanked).
!--
!  4-Sep-1992: derived from PGCONB [J. Crane].
! 26-Nov-1992: revised to use PGARRO [TJP].
! 25-Mar-1994: correct error for NC not =0 [G. Gonczi].
!  5-Oct-1996: correct error in computing max vector length [TJP;
!              thanks to David Singleton].
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: idim
   INTEGER, INTENT(IN)                      :: jdim
   REAL(KIND=pg), INTENT(IN)                :: a(idim,jdim)
   REAL(KIND=pg), INTENT(IN)                :: b(idim,jdim)
   INTEGER, INTENT(IN)                      :: i1
   INTEGER, INTENT(IN)                      :: i2
   INTEGER, INTENT(IN)                      :: j1
   INTEGER, INTENT(IN)                      :: j2
   REAL(KIND=pg), INTENT(IN)                :: c
   INTEGER, INTENT(IN)                      :: nc
   REAL(KIND=pg), INTENT(IN)                :: tr(6)
   REAL(KIND=pg), INTENT(IN OUT)            :: blnk
!
   INTEGER :: i,j
   REAL(KIND=pg) :: x1,y1,x2,y2
   REAL(KIND=pg) :: cc
!
!INTRINSIC  SQRT,MAX,MIN
!
! Define grid to world transformation
!
! JAO:  moved to the end using CONTAIN
!
!x(i,j)=tr(1)+tr(2)*i+tr(3)*j
!y(i,j)=tr(4)+tr(5)*i+tr(6)*j
!
! Check arguments.
!
   IF(i1 < 1.OR.i2 > idim.OR.i1 >= i2.OR.j1 < 1.OR.j2 > jdim .OR.j1 >= j2)THEN
!        CALL GRWARN('PGVECT: invalid range I1:I2, J1:J2')
      RETURN
   END IF
!
! Check for scale factor C.
!
   cc=c
   IF(ABS(cc) <= EPSILON(cc))THEN
      DO  j=j1,j2
         DO  i=i1,i2
            IF((ABS(a(i,j) - blnk) > EPSILON(blnk)).AND.(ABS(b(i,j) - blnk) > &
               EPSILON(blnk)))cc=MAX(cc,SQRT(a(i,j)**2+b(i,j)**2))
         END DO
      END DO
      IF(ABS(cc) <= EPSILON(cc))RETURN
      cc=SQRT(MIN(tr(2)**2+tr(3)**2,tr(5)**2+tr(6)**2))/cc
   END IF
!
   CALL pgbbuf
!
   DO  j=j1,j2
      DO  i=i1,i2
!
! Ignore vector if element of A and B are both equal to blnk
!
         IF(.NOT.((ABS(a(i,j) - blnk) <= EPSILON(blnk)).AND.((ABS(b(i,j) - blnk)) <= &
            EPSILON(blnk))))THEN
!
! Define the vector starting and end points according to NC.
!
            IF(nc < 0)THEN
               x2=x(i,j,tr)
               y2=y(i,j,tr)
               x1=x2-a(i,j)*cc
               y1=y2-b(i,j)*cc
            ELSE IF(nc == 0)THEN
               x2=x(i,j,tr)+0.5_pg*a(i,j)*cc
               y2=y(i,j,tr)+0.5_pg*b(i,j)*cc
               x1=x2-a(i,j)*cc
               y1=y2-b(i,j)*cc
            ELSE
               x1=x(i,j,tr)
               y1=y(i,j,tr)
               x2=x1+a(i,j)*cc
               y2=y1+b(i,j)*cc
            END IF
!
! Draw vector.
!
            CALL pgarro(x1,y1,x2,y2)
         END IF
      END DO
   END DO
!
   CALL pgebuf
!
   RETURN
!
CONTAINS
!
   FUNCTION x(i,j,tr)
!
      USE accur
!
      INTEGER, INTENT(IN)       :: i
      INTEGER, INTENT(IN)       :: J
      REAL(KIND=pg), INTENT(IN) :: tr(6)
      REAL(KIND=pg) x
!
      x=tr(1)+tr(2)*i+tr(3)*REAL(j,KIND=pg)
!
      RETURN
!
   END FUNCTION x
!
   FUNCTION y(i,j,tr)
!
      USE accur
!
      INTEGER, INTENT(IN)       :: i
      INTEGER, INTENT(IN)       :: J
      REAL(KIND=pg), INTENT(IN) :: tr(6)
      REAL(KIND=pg) y
!
      y=tr(4)+tr(5)*i+tr(6)*REAL(j,KIND=pg)
!
      RETURN
!
   END FUNCTION y
!
END SUBROUTINE pgvect
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGVPORT -- non-standard alias for PGSVP
!+
SUBROUTINE pgvport(xleft,xright,ybot,ytop)
!
! See description of PGSVP.
!--
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)               :: xleft
   REAL(KIND=pg), INTENT(IN)               :: xright
   REAL(KIND=pg), INTENT(IN)               :: ybot
   REAL(KIND=pg), INTENT(IN)               :: ytop
!
   CALL pgsvp(xleft,xright,ybot,ytop)
!
   RETURN
!
END SUBROUTINE pgvport
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGVSIZ -- set viewport (inches)
!%void cpgvsiz(float xleft, float xright, float ybot, float ytop);
!+
SUBROUTINE pgvsiz(xleft,xright,ybot,ytop)
!
! Change the size and position of the viewport, specifying
! the viewport in physical device coordinates (inches).  The
! viewport is the rectangle on the view surface "through"
! which one views the graph.  All the PG routines which plot lines
! etc. plot them within the viewport, and lines are truncated at
! the edge of the viewport (except for axes, labels etc drawn with
! PGBOX or PGLAB).  The region of world space (the coordinate
! space of the graph) which is visible through the viewport is
! specified by a call to PGSWIN.  It is legal to request a
! viewport larger than the view surface; only the part which
! appears on the view surface will be plotted.
!
! Arguments:
!  XLEFT  (input)  : x-coordinate of left hand edge of viewport, in
!                    inches from left edge of view surface.
!  XRIGHT (input)  : x-coordinate of right hand edge of viewport, in
!                    inches from left edge of view surface.
!  YBOT   (input)  : y-coordinate of bottom edge of viewport, in
!                    inches from bottom of view surface.
!  YTOP   (input)  : y-coordinate of top  edge of viewport, in inches
!                    from bottom of view surface.
!--
! 13-Dec-1990  Make errors non-fatal [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: xleft
   REAL(KIND=pg), INTENT(IN)                     :: xright
   REAL(KIND=pg), INTENT(IN)                     :: ybot
   REAL(KIND=pg), INTENT(IN)                     :: ytop
!
   LOGICAL :: pgnoto
!
   IF(pgnoto('PGVSIZ'))RETURN
   IF(xleft >= xright.OR.ybot >= ytop)THEN
      CALL grwarn('PGVSIZ ignored: invalid arguments')
      RETURN
   END IF
!
   pgxlen(pgid)=(xright-xleft)*pgxpin(pgid)
   pgylen(pgid)=(ytop-ybot)*pgypin(pgid)
   pgxvp(pgid)=xleft*pgxpin(pgid)
   pgyvp(pgid)=ybot*pgypin(pgid)
   pgxoff(pgid)=pgxvp(pgid)+(pgnxc(pgid)-1)*pgxsz(pgid)
   pgyoff(pgid)=pgyvp(pgid)+(pgny(pgid)-pgnyc(pgid))*pgysz(pgid)
   CALL pgvw
!
   RETURN
!
END SUBROUTINE pgvsiz
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGVSIZE -- non-standard alias for PGVSIZ
!+
SUBROUTINE pgvsize(xleft,xright,ybot,ytop)
!
! See description of PGVSIZ.
!--
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: xleft
   REAL(KIND=pg), INTENT(IN)                     :: xright
   REAL(KIND=pg), INTENT(IN)                     :: ybot
   REAL(KIND=pg), INTENT(IN)                     :: ytop
!
   CALL pgvsiz(xleft,xright,ybot,ytop)
!
END SUBROUTINE pgvsize
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGVSTAND -- non-standard alias for PGVSTD
!+
SUBROUTINE pgvstand
!
! See description of PGVSTD.
!--
   CALL pgvstd
!
END SUBROUTINE pgvstand
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGVSTD -- set standard (default) viewport
!%void cpgvstd(void);
!+
SUBROUTINE pgvstd
!
! Define the viewport to be the standard viewport.  The standard
! viewport is the full area of the view surface (or panel),
! less a margin of 4 character heights all round for labelling.
! It thus depends on the current character size, set by PGSCH.
!
! Arguments: none.
!--
! 22-Apr-1983: [TJP].
!  2-Aug-1995: [TJP].
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: xleft,xright,ybot,ytop,r
!
   IF(pgnoto('PGVSIZ'))RETURN
!
   r=4.0_pg*pgysp(pgid)
   xleft=r/pgxpin(pgid)
   xright=xleft+(pgxsz(pgid)-2.0_pg*r)/pgxpin(pgid)
   ybot=r/pgypin(pgid)
   ytop=ybot+(pgysz(pgid)-2.0_pg*r)/pgypin(pgid)
   CALL pgvsiz(xleft,xright,ybot,ytop)
!
   RETURN
!
END SUBROUTINE pgvstd
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE pgvw
!
! PGPLOT (internal routine): set the GRPCKG scaling transformation
! and window appropriate for the current window and viewport. This
! routine is called whenever the viewport or window is changed.
!
! Arguments: none
!
! (11-Feb-1983)
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
! Scale plotter in world coordinates.
!
   pgxscl(pgid)=pgxlen(pgid)/ABS(pgxtrc(pgid)-pgxblc(pgid))
   pgyscl(pgid)=pgylen(pgid)/ABS(pgytrc(pgid)-pgyblc(pgid))
   IF(pgxblc(pgid) > pgxtrc(pgid))THEN
      pgxscl(pgid)=-pgxscl(pgid)
   END IF
   IF(pgyblc(pgid) > pgytrc(pgid))THEN
      pgyscl(pgid)=-pgyscl(pgid)
   END IF
   pgxorg(pgid)=pgxoff(pgid)-pgxblc(pgid)*pgxscl(pgid)
   pgyorg(pgid)=pgyoff(pgid)-pgyblc(pgid)*pgyscl(pgid)
   CALL grtrn0(pgxorg(pgid),pgyorg(pgid),pgxscl(pgid),pgyscl(pgid))
!
! Window plotter in viewport.
!
   CALL grarea(pgid,pgxoff(pgid),pgyoff(pgid),pgxlen(pgid),pgylen(pgid))
!
   RETURN
!
END SUBROUTINE pgvw
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGWEDG -- annotate an image plot with a wedge
!%void cpgwedg(const char *side, float disp, float width, \
!% float fg, float bg, const char *label);
!+
SUBROUTINE pgwedg(side,disp,width,fg,bg,label)
!
! Plot an annotated grey-scale or color wedge parallel to a given axis
! of the the current viewport. This routine is designed to provide a
! brightness/color scale for an image drawn with PGIMAG or PGGRAY.
! The wedge will be drawn with the transfer function set by PGSITF
! and using the color index range set by PGSCIR.
!
! Arguments:
!  SIDE   (input)  : The first character must be one of the characters
!                    'B', 'L', 'T', or 'R' signifying the Bottom, Left,
!                    Top, or Right edge of the viewport.
!                    The second character should be 'I' to use PGIMAG
!                    to draw the wedge, or 'G' to use PGGRAY.
!  DISP   (input)  : the displacement of the wedge from the specified
!                    edge of the viewport, measured outwards from the
!                    viewport in units of the character height. Use a
!                    negative value to write inside the viewport, a
!                    positive value to write outside.
!  WIDTH  (input)  : The total width of the wedge including annotation,
!                    in units of the character height.
!  FG     (input)  : The value which is to appear with shade
!                    1 ("foreground"). Use the values of FG and BG
!                    that were supplied to PGGRAY or PGIMAG.
!  BG     (input)  : the value which is to appear with shade
!                    0 ("background").
!  LABEL  (input)  : Optional units label. If no label is required
!                    use ' '.
!--
!  15-Oct-1992: New routine (MCS)
!   2-Aug-1995: no longer needs common (TJP).
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   CHARACTER (LEN=*), INTENT(IN)        :: side
   REAL(KIND=pg), INTENT(IN)            :: disp
   REAL(KIND=pg), INTENT(IN)            :: width
   REAL(KIND=pg), INTENT(IN)            :: fg
   REAL(KIND=pg), INTENT(IN)            :: bg
   CHARACTER (LEN=*), INTENT(IN)        :: label
!
   LOGICAL :: pgnoto
!
!                                        Temporary window coord storage.
!
   REAL(KIND=pg) :: wxa,wxb,wya,wyb,xa,xb,ya,yb
!
!                                        Viewport coords of wedge.
!
   REAL(KIND=pg) :: vxa,vxb,vya,vyb
!
!                          Original and anotation character heights.
!
   REAL(KIND=pg) :: oldch,newch
!
!                          Size of unit character height (NDC units).
!
   REAL(KIND=pg) :: ndcsiz
!
!                          True if wedge plotted horizontally.
!
   LOGICAL :: horiz
!
!                          Use PGIMAG (T) or PGGRAY (F).
!
   LOGICAL :: image
!
!                          Symbolic version of SIDE.
!
   INTEGER :: nside
   INTEGER, PARAMETER :: bot=1
   INTEGER, PARAMETER :: top=2
   INTEGER, PARAMETER :: lft=3
   INTEGER, PARAMETER :: rgt=4
   INTEGER :: i
   REAL(KIND=pg) :: wedwid,wdginc,vwidth,vdisp,xch,ych,labwid,fg1,bg1
!
!                          Set the fraction of WIDTH used for anotation.
!
   REAL(KIND=pg), PARAMETER :: txtfrc=0.6_pg
!
!                          Char separation between numbers and LABEL.
!
   REAL(KIND=pg), PARAMETER :: txtsep=2.2_pg
!
!                          Array to draw wedge in.
!
   INTEGER, PARAMETER :: wdgpix=100
   REAL(KIND=pg) :: wdgarr(wdgpix)
!
!                          Define the coordinate-mapping function.
!
   REAL(KIND=pg) :: tr(6)
   SAVE  tr
   DATA tr/0.0_pg,1.0_pg,0.0_pg,0.0_pg,0.0_pg,1.0_pg/
!
   IF(pgnoto('PGWEDG'))RETURN
!
! Get a numeric version of SIDE.
!
   IF(side(1:1) == 'B'.OR.side(1:1) == 'b')THEN
      nside=bot
      horiz=.true.
   ELSE IF(side(1:1) == 'T'.OR.side(1:1) == 't')THEN
      nside=top
      horiz=.true.
   ELSE IF(side(1:1) == 'L'.OR.side(1:1) == 'l')THEN
      nside=lft
      horiz=.false.
   ELSE IF(side(1:1) == 'R'.OR.side(1:1) == 'r')THEN
      nside=rgt
      horiz=.false.
   ELSE
      CALL grwarn('Invalid "SIDE" argument in PGWEDG.')
      RETURN
   END IF
!
! Determine which routine to use.
!
   IF(LEN(side) < 2)THEN
      image=.false.
   ELSE IF(side(2:2) == 'I'.OR.side(2:2) == 'i')THEN
      image=.true.
   ELSE IF(side(2:2) == 'G'.OR.side(2:2) == 'g')THEN
      image=.false.
   ELSE
      CALL grwarn('Invalid "SIDE" argument in PGWEDG.')
   END IF
!
   CALL pgbbuf
!
! Store the current world and viewport coords and the character height.
!
   CALL pgqwin(wxa,wxb,wya,wyb)
   CALL pgqvp(0,xa,xb,ya,yb)
   CALL pgqch(oldch)
!
! Determine the unit character height in NDC coords.
!
   CALL pgsch(1.0_pg)
   CALL pgqcs(0,xch,ych)
   IF(horiz)THEN
      ndcsiz=ych
   ELSE
      ndcsiz=xch
   END IF
!
! Convert 'WIDTH' and 'DISP' into viewport units.
!
   vwidth=width*ndcsiz*oldch
   vdisp=disp*ndcsiz*oldch
!
! Determine the number of character heights required under the wedge.
!
   labwid=txtsep
   IF(label /= ' ')labwid=labwid+1.0_pg
!
! Determine and set the character height required to fit the wedge
! anotation text within the area allowed for it.
!
   newch=txtfrc*vwidth/(labwid*ndcsiz)
   CALL pgsch(newch)
!
! Determine the width of the wedge part of the plot minus the anotation.
! (NDC units).
!
   wedwid=vwidth*(1.0_pg-txtfrc)
!
! Use these to determine viewport coordinates for the wedge + annotation.
!
   vxa=xa
   vxb=xb
   vya=ya
   vyb=yb
   IF(nside == bot)THEN
      vyb=ya-vdisp
      vya=vyb-wedwid
   ELSE IF(nside == top)THEN
      vya=yb+vdisp
      vyb=vya+wedwid
   ELSE IF(nside == lft)THEN
      vxb=xa-vdisp
      vxa=vxb-wedwid
   ELSE IF(nside == rgt)THEN
      vxa=xb+vdisp
      vxb=vxa+wedwid
   END IF
!
! Set the viewport for the wedge.
!
   CALL pgsvp(vxa,vxb,vya,vyb)
!
! Swap FG/BG if necessary to get axis direction right.
!
   fg1=MAX(fg,bg)
   bg1=MIN(fg,bg)
!
! Create a dummy wedge array to be plotted.
!
   wdginc=(fg1-bg1)/(wdgpix-1)
   DO  i=1,wdgpix
      wdgarr(i)=bg1+(i-1)*wdginc
   END DO
!
! Draw the wedge then change the world coordinates for labelling.
!
   IF(horiz)THEN
      CALL pgswin(1.0_pg,REAL(wdgpix,KIND=pg),0.9_pg,1.1_pg)
      IF(image)THEN
         CALL pgimag(wdgarr,wdgpix,1,1,wdgpix,1,1,fg,bg,tr)
      ELSE
         CALL pggray(wdgarr,wdgpix,1,1,wdgpix,1,1,fg,bg,tr)
      END IF
      CALL pgswin(bg1,fg1,0.0_pg,1.0_pg)
   ELSE
      CALL pgswin(0.9_pg,1.1_pg,1.0_pg,REAL(wdgpix,KIND=pg))
      IF(image)THEN
         CALL pgimag(wdgarr,1,wdgpix,1,1,1,wdgpix,fg,bg,tr)
      ELSE
         CALL pggray(wdgarr,1,wdgpix,1,1,1,wdgpix,fg,bg,tr)
      END IF
      CALL pgswin(0.0_pg,1.0_pg,bg1,fg1)
   END IF
!
! Draw a labelled frame around the wedge.
!
   IF(nside == bot)THEN
      CALL pgbox('BCNST',0.0_pg,0,'BC',0.0_pg,0)
   ELSE IF(nside == top)THEN
      CALL pgbox('BCMST',0.0_pg,0,'BC',0.0_pg,0)
   ELSE IF(nside == lft)THEN
      CALL pgbox('BC',0.0_pg,0,'BCNST',0.0_pg,0)
   ELSE IF(nside == rgt)THEN
      CALL pgbox('BC',0.0_pg,0,'BCMST',0.0_pg,0)
   END IF
!
! Write the units label.
!
   IF(label /= ' ')THEN
      CALL pgmtxt(side,txtsep,1.0_pg,1.0_pg,label)
   END IF
!
! Reset the original viewport and world coordinates.
!
   CALL pgsvp(xa,xb,ya,yb)
   CALL pgswin(wxa,wxb,wya,wyb)
   CALL pgsch(oldch)
   CALL pgebuf
   RETURN
!
END SUBROUTINE pgwedg
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGWINDOW -- non-standard alias for PGSWIN
!+
SUBROUTINE pgwindow(x1,x2,y1,y2)
!
! See description of PGSWIN.
!--
   USE accur
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: x1
   REAL(KIND=pg), INTENT(IN)                     :: x2
   REAL(KIND=pg), INTENT(IN)                     :: y1
   REAL(KIND=pg), INTENT(IN)                     :: y2
!
   CALL pgswin(x1,x2,y1,y2)
!
   RETURN
!
END SUBROUTINE pgwindow
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PGWNAD -- set window and adjust viewport to same aspect ratio
!%void cpgwnad(float x1, float x2, float y1, float y2);
!+
SUBROUTINE pgwnad(x1,x2,y1,y2)
!
! Change the window in world coordinate space that is to be mapped on
! to the viewport, and simultaneously adjust the viewport so that the
! world-coordinate scales are equal in x and y. The new viewport is
! the largest one that can fit within the previously set viewport
! while retaining the required aspect ratio.
!
! Arguments:
!  X1     (input)  : the x-coordinate of the bottom left corner
!                    of the viewport.
!  X2     (input)  : the x-coordinate of the top right corner
!                    of the viewport (note X2 may be less than X1).
!  Y1     (input)  : the y-coordinate of the bottom left corner
!                    of the viewport.
!  Y2     (input)  : the y-coordinate of the top right corner of the
!                    viewport (note Y2 may be less than Y1).
!--
! 25-Sep-1985 - new routine (TJP).
! 31-May-1989 - correct error: XVP and YVP not set (TJP).
!-----------------------------------------------------------------------
!
   USE accur
   USE pgplot
!
   IMPLICIT NONE
!
   REAL(KIND=pg), INTENT(IN)                     :: x1
   REAL(KIND=pg), INTENT(IN)                     :: x2
   REAL(KIND=pg), INTENT(IN)                     :: y1
   REAL(KIND=pg), INTENT(IN)                     :: y2
!
   LOGICAL :: pgnoto
   REAL(KIND=pg) :: scale,oxlen,oylen
!
   IF(pgnoto('PGWNAD'))RETURN
!
! If invalid arguments are specified, issue warning and leave window
! unchanged.
!
   IF(ABS(x1 - x2) <= EPSILON(x1))THEN
      CALL grwarn('invalid x limits in PGWNAD: X1 = X2.')
   ELSE IF(ABS(y1 - y2) <= EPSILON(y1))THEN
      CALL grwarn('invalid y limits in PGWNAD: Y1 = Y2.')
   ELSE
      scale=MIN(pgxlen(pgid)/ABS(x2-x1)/pgxpin(pgid),pgylen(pgid)/  &
         ABS(y2-y1)/pgypin(pgid))
      pgxscl(pgid)=scale*pgxpin(pgid)
      pgyscl(pgid)=scale*pgypin(pgid)
      oxlen=pgxlen(pgid)
      oylen=pgylen(pgid)
      pgxlen(pgid)=pgxscl(pgid)*ABS(x2-x1)
      pgylen(pgid)=pgyscl(pgid)*ABS(y2-y1)
      pgxvp(pgid)=pgxvp(pgid)+0.5_pg*(oxlen-pgxlen(pgid))
      pgyvp(pgid)=pgyvp(pgid)+0.5_pg*(oylen-pgylen(pgid))
      pgxoff(pgid)=pgxvp(pgid)+(pgnxc(pgid)-1)*pgxsz(pgid)
      pgyoff(pgid)=pgyvp(pgid)+(pgny(pgid)-pgnyc(pgid))*pgysz(pgid)
      CALL pgswin(x1,x2,y1,y2)
   END IF
!
   RETURN
!
END SUBROUTINE pgwnad
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
SUBROUTINE plot(visble,x,y,z)
!
!   dummy routine for pgcontx
!
   USE accur
!
   INTEGER, INTENT(IN)                  :: visble
   REAL(KIND=pg), INTENT(IN)            :: x
   REAL(KIND=pg), INTENT(IN)            :: y
   REAL(KIND=pg), INTENT(IN)            :: z
   REAL(KIND=pg) :: xworld,yworld
!
   xworld=z
   xworld=x!X*COS(Y) !this is the user-defined
   yworld=y!X*SIN(Y) !transformation
   IF(visble == 0)THEN
      CALL pgmove(xworld,yworld)
   ELSE
      CALL pgdraw(xworld,yworld)
   END IF
   RETURN
!
END SUBROUTINE plot
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
!*PSDRIV -- PGPLOT PostScript drivers
!+
SUBROUTINE psdriv(ifunc,rbuf,nbuf,chr,lchr,mode)
!
! PGPLOT driver for PostScript devices.
!
! Version 1.2  - 1987 Aug  5 - T. J. Pearson.
! Version 1.3  - 1987 Nov 16 - add "bind" commands to prolog - TJP.
! Version 1.4  - 1988 Jan 28 - change dimensions so whole field can be
!                              plotted - TJP.
! Version 1.5  - 1988 Oct 27 - make EOF characters optional - TJP.
! Version 1.6  - 1988 Dec 15 - standard Fortran - TJP.
! Version 1.7  - 1989 Jul  5 - change color indices so most colors
!                              are black - TJP.
! Version 2.0  - 1990 Sep 10 - parameterize dimensions; correct
!                              bounding box; add color support (from
!                              D. Meier's CPdriver) - TJP.
! Version 2.1  - 1991 Nov 29 - update Document Structuring Conventions
!                              to version 3.0.
! Version 3.0  - 1992 Sep 22 - add marker support; add CPS and VCPS
!                              modes - TJP.
! Version 3.1  - 1992 Nov 12 - up to 256 colors.
! Version 3.2  - 1993 May 26 - correct error in marker support.
! Version 4.0  - 1993 Sep 20 - trap Fortran I/O errors.
! Version 4.1  - 1994 Aug  4 - make marker support optional.
! Version 5.0  - 1994 Aug 30 - support for images.
! Version 5.1  - 1994 Sep  7 - support for PGQCR.
! Version 5.2  - 1994 Oct 12 - add IDENT option.
! Version 5.3  - 1995 May  8 - recognise '-' as standard output; keep
!                              track of bounding box; use upper case
!                              for all defined commands; move
!                              showpage outside save/restore.
! Version 5.4  - 1995 Aug 19 - correct usage of PS_BBOX.
! Version 6.0  - 1995 Dec 28 - reject concurrent access.
! Version 6.1  - 1996 Apr 29 - decode environment variables using GRCTOI.
! Version 6.2  - 1996 Oct  7 - correct bounding-box error (K-G Adams);
!                              correct error in use of GCTOI (G Gonczi);
!                              suppress <0 0 C> commands (R Scharroo);
!                              allow arbitrary page size.
! Version 6.3  - 1997 Nov 14 - shorter commands for setrgbcolor and setgray.
! Version 6.4  - 1997 Nov 19 - workaround a Ghostscript bug: split long
!                              polylines into shorter segments.
! Version 6.5  - 1998 Feb 23 - support for real linewidth.
! Version 6.6  - 1998 Nov 10 - provide easy way to convert color to grey.
! Version 6.7  - 1998 Dec 12 - added #copies to header.
!
! Supported device:
!   Any printer that accepts the PostScript page description language,
!   eg, the LaserWriter (Apple Computer, Inc.).
!   PostScript is a trademark of Adobe Systems Incorporated.
!
! Device type code:
!   /PS (monochrome landscape mode, long edge of paper horizontal).
!   /CPS (color landscape mode, long edge of paper horizontal).
!   /VPS (monochrome portrait mode, short edge of paper horizontal).
!   /VCPS (color portrait mode, short edge of paper horizontal).
!
! Default file name:
!   pgplot.ps
!
! Default view surface dimensions:
!   10.5 inches horizontal x  7.8 inches vertical (landscape mode),
!    7.8 inches horizontal x 10.5 inches vertical (portrait mode).
!   These dimensions can be changed with environment variables.
!
! Resolution:
!   The driver uses coordinate increments of 0.001 inch, giving an
!   ``apparent'' resolution of 1000 pixels/inch. The true resolution is
!   device-dependent; eg, on an Apple LaserWriter it is 300 pixels/inch
!   (in both dimensions).
!
! Color capability (monochrome mode):
!   Color indices 0-255 are supported. Color index 0 is white (erase
!   or background color), indices 1-13 are black, 14 is light grey,
!   and 15 is dark grey.
!
! Color capability (color mode):
!   Color indices 0-255 are supported. Color index 0 is white (erase
!   or background color), index 1 is black, and indices 2-15 have the
!   standard PGPLOT color assignments.
!
! Input capability: none.
!
! File format: the file contains variable length records (maximum 132
! characters) containing PostScript commands. The commands use only
! printable ASCII characters, and the file can be examined or modified
! with a text editor.
!
! Obtaining hardcopy: use the operating system print or copy command to
! send the file to a suitable device.
!
! Environment variables:
!
!  PGPLOT_PS_WIDTH      default  7800
!  PGPLOT_PS_HEIGHT     default 10500
!  PGPLOT_PS_HOFFSET    default   350
!  PGPLOT_PS_VOFFSET    default   250
! These variables tell PGPLOT how big an image to produce. The defaults
! are appropriate for 8.5 x 11-inch paper. The maximum dimensions of
! a PGPLOT image are WIDTH by HEIGHT, with the lower left corner offset
! by HOFFSET horizontally and VOFFSET vertically from the lower left
! corner of the paper. The units are milli-inches. The "top" of the
! paper is the edge that comes out of the printer first.
!
!  PGPLOT_IDENT
! If this variable is set, the user name, date and time are written
! in the bottom right corner of each page.
!
!  PGPLOT_PS_BBOX
! If this variable has value MAX, PGPLOT puts standard (full-page)
! bounding-box information in the header of the PostScript file. If
! the variable is unset or has some other value, PGPLOT puts the
! correct (smallest) bounding box information in the trailer of the
! PostScript file.
!
!  PGPLOT_PS_EOF
! Normally the output file does not contain special end-of-file
! characters. But if environment variable PGPLOT_PS_EOF is defined
! (with any value) PGPLOT writes a control-D job-separator character at
! the beginning and at the end of the file. This is appropriate for
! Apple LaserWriters using the serial interface, but it may not be
! appropriate for other PostScript devices.
!
!  PGPLOT_PS_MARKERS
! Specify "NO" to suppress use of a PostScript font for the graph
! markers; markers are then emulated by line-drawing.
!
! Document Structuring Conventions:
!
!  The PostScript files conform to Version 3.0 of the Adobe Document
!  Structuring Conventions (see ref.3) and to version 3.0 of the
!  encapsulated PostScript file (EPSF) format. This should allow
!  the files to be read by other programs that accept the EPSF format.
!  Note, though, that multi-page plots are not valid EPSF files. The
!  files do not contain a screen preview section.
!
! References:
!
! (1) Adobe Systems, Inc.: PostScript Language Reference Manual.
! Addison-Wesley, Reading, Massachusetts, 1985.
! (2) Adobe Systems, Inc.: PostScript Language Tutorial and Cookbook.
! Addison-Wesley, Reading, Massachusetts, 1985.
! (3) Adobe Systems, Inc.: PostScript Language Reference Manual, Second
! Edition. Addison-Wesley, Reading, Massachusetts, 1990.
!-----------------------------------------------------------------------
!
   USE accur
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN)                      :: ifunc
   REAL(KIND=pg), INTENT(OUT)               :: rbuf(*)
   INTEGER, INTENT(OUT)                     :: nbuf
   CHARACTER (LEN=*), INTENT(OUT)           :: chr
   INTEGER, INTENT(OUT)                     :: lchr
   INTEGER, INTENT(IN)                      :: mode
!
   CHARACTER (LEN=*), PARAMETER :: ptype='VPS   (PostScript file, portrait orientation)'
   CHARACTER (LEN=*), PARAMETER :: ltype='PS    (PostScript file, landscape orientation)'
   CHARACTER (LEN=*), PARAMETER :: cptype='VCPS  (Colour PostScript file, portrait orientation)'
   CHARACTER (LEN=*), PARAMETER :: cltype='CPS   (Colour PostScript file, landscape orientation) '
!
!     PARAMETER (PTYPE='VPS', LTYPE='PS', CPTYPE='VCPS', CLTYPE='CPS')
!
   CHARACTER (LEN=*), PARAMETER :: defnam='pgplot.ps'
!
! -- printable paper area: in milli-inches; (WIDTH, HEIGHT) are
!    the dimensions of the printable area; OFFW, OFFH the offset from
!    the lower left corner of the paper
!
   INTEGER, PARAMETER :: dwd=7800
   INTEGER, PARAMETER :: dht=10500
   INTEGER, PARAMETER :: doffw=350
   INTEGER, PARAMETER :: doffh=250
!
   INTEGER :: width,height,offw,offh
   SAVE  width,height,offw,offh
   INTEGER :: ier,i0,j0,i1,j1,l,ll,lasti,lastj,UNIT,lobuf
   SAVE  lasti,lastj,UNIT,lobuf
   INTEGER :: ci,npts,npage,ioerr,lfname
   SAVE  npts,npage,ioerr,lfname
   INTEGER :: state,nseg
   SAVE  state,nseg
   INTEGER :: nxp,nyp,xorg,yorg,xlen,ylen,n,rgb(3)
   INTEGER :: high,low,i,k,kmax,posn,ld,lu
   INTEGER :: bbox(4),bb1,bb2,bb3,bb4
   SAVE  bbox
   INTEGER :: groptx,grctoi
   LOGICAL :: start,landsc,color,stdout
   SAVE  start,color,stdout
   REAL(KIND=pg) :: lw
   SAVE  lw
   REAL(KIND=pg) :: bbxmin,bbxmax,bbymin,bbymax
   SAVE  bbxmin,bbxmax,bbymin,bbymax
   REAL(KIND=pg) :: rvalue(0:255),gvalue(0:255),bvalue(0:255)
   SAVE  rvalue,gvalue,bvalue
   CHARACTER (LEN=20) :: suser,sdate
   CHARACTER (LEN=120) :: instr,msg
   CHARACTER (LEN=132) :: obuf
   SAVE  obuf
   CHARACTER (LEN=255) :: fname
   SAVE  fname
   INTEGER :: marker(0:31),nsym,rad(0:31)
   SAVE  marker,rad
   REAL(KIND=pg) :: mfac
   SAVE  mfac
   REAL(KIND=pg) :: shade(0:15),rinit(0:15),ginit(0:15),binit(0:15)
   SAVE  shade,rinit,ginit,binit
   CHARACTER (LEN=1) :: hexdig(0:15)
   DATA hexdig/'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'/
   DATA shade/1.00_pg,13*0.00_pg,0.33_pg,0.67_pg/
   DATA rinit/1.00_pg,0.00_pg,1.00_pg,0.00_pg,0.00_pg,0.00_pg,1.00_pg,1.00_pg,  &
      1.00_pg,0.50_pg,0.00_pg,0.00_pg,0.50_pg,1.00_pg,0.33_pg,0.67_pg/
   DATA ginit/1.00_pg,0.00_pg,0.00_pg,1.00_pg,0.00_pg,1.00_pg,0.00_pg,1.00_pg, &
      0.50_pg,1.00_pg,1.00_pg,0.50_pg,0.00_pg,0.00_pg,0.33_pg,0.67_pg/
   DATA binit/1.00_pg,0.00_pg,0.00_pg,0.00_pg,1.00_pg,1.00_pg,1.00_pg,0.00_pg,  &
      0.00_pg,0.00_pg,0.50_pg,1.00_pg,1.00_pg,0.50_pg,0.33_pg,0.67_pg/
   DATA rad/6,1,7,6,7,5,6,8,7,7,9,10,9,8,6,8,4,5,9,12,2,4,5,7,11,  &
      17,22,41,9,9,9,9/
   DATA state/0/
!
   SELECT CASE (ifunc)
    CASE (    1)
      GO TO 10
    CASE (    2)
      GO TO 20
    CASE (    3)
      GO TO 30
    CASE (    4)
      GO TO 40
    CASE (    5)
      GO TO 50
    CASE (    6)
      GO TO 60
    CASE (    7)
      GO TO 70
    CASE (    8)
      GO TO 80
    CASE (    9)
      GO TO 90
    CASE (   10)
      GO TO 140
    CASE (   11)
      GO TO 150
    CASE (   12)
      GO TO 170
    CASE (   13)
      GO TO 180
    CASE (   14)
      GO TO 190
    CASE (   15)
      GO TO 200
    CASE (   16)
      GO TO 210
    CASE (   17)
      GO TO 330
    CASE (   18)
      GO TO 220
    CASE (   19)
      GO TO 330
    CASE (   20)
      GO TO 230
    CASE (   21)
      GO TO 240
    CASE (   22)
      GO TO 250
    CASE (   23)
      GO TO 260
    CASE (   24)
      GO TO 330
    CASE (   25)
      GO TO 330
    CASE (   26)
      GO TO 270
    CASE (   27)
      GO TO 330
    CASE (   28)
      GO TO 300
    CASE (   29)
      GO TO 310
   END SELECT
   GO TO 330
!
!--- IFUNC = 1, Return device name.-------------------------------------
!
10 IF(mode == 1)THEN
!         -- landscape, monochrome
      chr=ltype
      lchr=LEN(ltype)
   ELSE IF(mode == 2)THEN
!         -- portrait, monochrome
      chr=ptype
      lchr=LEN(ptype)
   ELSE IF(mode == 3)THEN
!         -- landscape, color
      chr=cltype
      lchr=LEN(cltype)
   ELSE
!         -- portrait, color
      chr=cptype
      lchr=LEN(cptype)
   END IF
   RETURN
!
!--- IFUNC = 2, Return physical min and max for plot device, and range
!               of color indices.---------------------------------------
!
20 rbuf(1)=0
   rbuf(2)=-1.0_pg
   rbuf(3)=0.0_pg
   rbuf(4)=-1.0_pg
   rbuf(5)=0.0_pg
   rbuf(6)=255.0_pg
   nbuf=6
   RETURN
!
!--- IFUNC = 3, Return device resolution. ------------------------------
!
30 rbuf(1)=1000.0_pg
   rbuf(2)=1000.0_pg
   rbuf(3)=5.0_pg
   nbuf=3
   RETURN
!
!--- IFUNC = 4, Return misc device info. -------------------------------
!    (This device is Hardcopy, No cursor, No dashed lines, Area fill,
!    Thick lines, QCR, Markers [optional])
!
40 CONTINUE
   chr='HNNATNQNYM'
!     -- Marker support suppressed?
   CALL grgenv('PS_MARKERS',instr,l)
   IF(l >= 2)THEN
      IF(instr(1:l) == 'NO'.OR.instr(1:l) == 'no')THEN
         chr(10:10)='N'
      END IF
   END IF
   lchr=10
   RETURN
!
!--- IFUNC = 5, Return default file name. ------------------------------
!
50 chr=defnam
   lchr=LEN(defnam)
   RETURN
!
!--- IFUNC = 6, Return default physical size of plot. ------------------
!
60 rbuf(1)=0
   rbuf(3)=0
   landsc=mode == 1.OR.mode == 3
   IF(landsc)THEN
      rbuf(2)=height-1
      rbuf(4)=width-1
   ELSE
      rbuf(2)=width-1
      rbuf(4)=height-1
   END IF
   nbuf=4
   RETURN
!
!--- IFUNC = 7, Return misc defaults. ----------------------------------
!
70 rbuf(1)=8
   nbuf=1
   RETURN
!
!--- IFUNC = 8, Select plot. -------------------------------------------
!
80 CONTINUE
   RETURN
!
!--- IFUNC = 9, Open workstation. --------------------------------------
!
90 CONTINUE
!     -- check for concurrent access
   IF(state == 1)THEN
      CALL grwarn('a PGPLOT PostScript file is already open')
      rbuf(1)=0
      rbuf(2)=0
      RETURN
   END IF
!     -- Color mode?
   CALL grgenv('PS_COLOR',instr,l)
   color=l > 0.OR.mode == 3.OR.mode == 4
   IF(color)THEN
      DO  ci=0,15
         rvalue(ci)=rinit(ci)
         gvalue(ci)=ginit(ci)
         bvalue(ci)=binit(ci)
      END DO
   ELSE
      DO  ci=0,15
         rvalue(ci)=shade(ci)
         gvalue(ci)=shade(ci)
         bvalue(ci)=shade(ci)
      END DO
   END IF
   DO  ci=16,255
      rvalue(ci)=0.0_pg
      gvalue(ci)=0.0_pg
      bvalue(ci)=0.0_pg
   END DO
!     -- Device dimensions
   width=dwd
   height=dht
   offw=doffw
   offh=doffh
   CALL grgenv('PS_WIDTH',instr,l)
   ll=1
   IF(l > 0)width=grctoi(instr(:l),ll)
   CALL grgenv('PS_HEIGHT',instr,l)
   ll=1
   IF(l > 0)height=grctoi(instr(:l),ll)
   CALL grgenv('PS_HOFFSET',instr,l)
   ll=1
   IF(l > 0)offw=grctoi(instr(:l),ll)
   CALL grgenv('PS_VOFFSET',instr,l)
   ll=1
   IF(l > 0)offh=grctoi(instr(:l),ll)
   stdout=chr(1:lchr) == '-'
   IF(stdout)THEN
      UNIT=6
!        -- machine-dependent!
   ELSE
      CALL grglun(UNIT)
   END IF
   nbuf=2
   rbuf(1)=UNIT
   IF(.NOT.stdout)THEN
      ier=groptx(UNIT,chr(1:lchr),defnam,1)
      IF(ier /= 0)THEN
         msg='Cannot open output file for PostScript plot: '// chr(:lchr)
         CALL grwarn(msg)
         rbuf(2)=0
         CALL grflun(UNIT)
         RETURN
      ELSE
         INQUIRE (UNIT=UNIT,NAME=chr)
         lchr=LEN(chr)
130      IF(chr(lchr:lchr) == ' ')THEN
            lchr=lchr-1
            GO TO 130
         END IF
         rbuf(2)=1
         fname=chr(:lchr)
         lfname=lchr
      END IF
   ELSE
      rbuf(2)=1
      fname='-'
      lfname=1
   END IF
   state=1
   ioerr=0
   lobuf=0
   lasti=-1
   lastj=-1
   lw=1
   npts=0
   CALL grgenv('PS_EOF',instr,l)
   IF(l > 0)CALL grps02(ioerr,UNIT,CHAR(4))
   CALL grps02(ioerr,UNIT,'%!PS-Adobe-3.0 EPSF-3.0')
   CALL gruser(instr,l)
   IF(l > 0)CALL grps02(ioerr,UNIT,'%%For: '//instr(1:l))
   CALL grps02(ioerr,UNIT,'%%Title: PGPLOT PostScript plot')
   CALL grps02(ioerr,UNIT,'%%Creator: PGPLOT [PSDRIV 6.6]')
   CALL grdate(instr,l)
   IF(l > 0)CALL grps02(ioerr,UNIT,'%%CreationDate: '// instr(1:l))
   CALL grgenv('PS_BBOX',instr,l)
   CALL grtoup(instr(1:3),instr(1:3))
   IF(instr(1:3) == 'MAX')THEN
!        -- bounding box is based on maximum plot dimensions, not
!           actual dimensions
      CALL grfao('%%BoundingBox: # # # #',l,instr,nint(offw*0.072_pg),nint(offh*0.072_pg), &
         nint((width+offw)*0.072_pg),nint((height+offh)*0.072_pg))
      CALL grps02(ioerr,UNIT,instr(:l))
   ELSE
      CALL grps02(ioerr,UNIT,'%%BoundingBox: (atend)')
   END IF
   CALL grps02(ioerr,UNIT,'%%DocumentFonts: (atend)')
   CALL grps02(ioerr,UNIT,'%%LanguageLevel: 1')
   landsc=mode == 1.OR.mode == 3
   IF(landsc)THEN
      CALL grps02(ioerr,UNIT,'%%Orientation: Landscape')
   ELSE
      CALL grps02(ioerr,UNIT,'%%Orientation: Portrait')
   END IF
   CALL grps02(ioerr,UNIT,'%%Pages: (atend)')
   CALL grps02(ioerr,UNIT,'%%EndComments')
   CALL grps02(ioerr,UNIT,'%%BeginProlog')
   CALL grps02(ioerr,UNIT,'/L {moveto rlineto currentpoint stroke moveto} bind def')
   CALL grps02(ioerr,UNIT,'/C {rlineto currentpoint stroke moveto} bind def')
   CALL grps02(ioerr,UNIT,'/D {moveto 0 0 rlineto currentpoint stroke moveto} bind def')
   CALL grps02(ioerr,UNIT,'/LW {5 mul setlinewidth} bind def')
   CALL grps02(ioerr,UNIT,'/BP {newpath moveto} bind def')
   CALL grps02(ioerr,UNIT,'/LP /rlineto load def')
   CALL grps02(ioerr,UNIT,'/EP {rlineto closepath eofill} bind def')
   CALL grps02(ioerr,UNIT,'/MB {gsave translate MFAC dup scale '  &
      //'1 setlinewidth 2 setlinecap 0 setlinejoin newpath} bind def')
   CALL grps02(ioerr,UNIT,'/ME /grestore load def')
   CALL grps02(ioerr,UNIT,'/CC {0 360 arc stroke} bind def')
   CALL grps02(ioerr,UNIT,'/FC {0 360 arc fill} bind def')
   CALL grps02(ioerr,UNIT,'/G {1024 div setgray} bind def')
   CALL grps02(ioerr,UNIT,'/K {3 -1 roll 1024 div 3 -1 roll 1024'// &
      ' div 3 -1 roll 1024 div setrgbcolor} bind def')
   CALL grps02(ioerr,UNIT,'% Uncomment next line to convert color to grey shades')
   CALL grps02(ioerr,UNIT,'%/K {3 -1 roll 3413 div 3 -1 roll 1739'//  &
      ' div 3 -1 roll 9309 div add add setgray} bind def')
   CALL grgenv('IDENT',instr,l)
   IF(l > 0)THEN
      CALL grps02(ioerr,UNIT,'/RS{findfont exch scalefont setfont moveto dup'// &
         ' stringwidth neg exch neg exch rmoveto show} bind def')
   END IF
   CALL grps02(ioerr,UNIT,'%%EndProlog')
   CALL grps02(ioerr,UNIT,'%%BeginSetup')
   CALL grps02(ioerr,UNIT,'/#copies 1 def')
   CALL grps02(ioerr,UNIT,'%%EndSetup')
   npage=0
   RETURN
!
!--- IFUNC=10, Close workstation. --------------------------------------
!
140 CONTINUE
   CALL grps02(ioerr,UNIT,' ')
   CALL grps02(ioerr,UNIT,'%%Trailer')
   CALL grgenv('PS_BBOX',instr,l)
   CALL grtoup(instr(1:3),instr(1:3))
   IF(instr(1:3) /= 'MAX')THEN
      CALL grfao('%%BoundingBox: # # # #',l,instr,bbox(1),bbox(2),bbox(3),bbox(4))
      CALL grps02(ioerr,UNIT,instr(:l))
   END IF
   CALL grps02(ioerr,UNIT,'%%DocumentFonts: ')
   CALL grfao('%%Pages: #',l,instr,npage,0,0,0)
   CALL grps02(ioerr,UNIT,instr(:l))
   CALL grps02(ioerr,UNIT,'%%EOF')
   CALL grgenv('PS_EOF',instr,l)
   IF(l > 0)CALL grps02 (ioerr,UNIT,CHAR(4))
   IF(ioerr /= 0)THEN
      CALL grwarn('++WARNING++ Error '//'writing PostScript file: file is incomplete')
      CALL grwarn('Check for device full or quota exceeded')
      CALL grwarn('Filename: '//fname(:lfname))
   END IF
   IF(.NOT.stdout)THEN
      CLOSE(UNIT,IOSTAT=ioerr)
      IF(ioerr /= 0)THEN
         CALL grwarn('Error closing PostScript file '// fname(:lfname))
      END IF
      CALL grflun(UNIT)
   END IF
   state=0
   RETURN
!
!--- IFUNC=11, Begin picture. ------------------------------------------
!
150 CONTINUE
   landsc=mode == 1.OR.mode == 3
   IF(landsc)THEN
      height=INT(rbuf(1))
      width=INT(rbuf(2))
   ELSE
      width=INT(rbuf(1))
      height=INT(rbuf(2))
   END IF
   npage=npage+1
   CALL grps02(ioerr,UNIT,' ')
   CALL grfao('%%Page: # #',l,instr,npage,npage,0,0)
   CALL grps02(ioerr,UNIT,instr(:l))
   CALL grps02(ioerr,UNIT,'%%BeginPageSetup')
   CALL grps02(ioerr,UNIT,'/PGPLOT save def')
   CALL grps02(ioerr,UNIT,'0.072 0.072 scale')
   landsc=mode == 1.OR.mode == 3
   IF(landsc)THEN
      CALL grfao('# # translate 90 rotate',l,instr,width+offw,offh,0,0)
   ELSE
      CALL grfao('# # translate',l,instr,offw,offh,0,0)
   END IF
   CALL grps02(ioerr,UNIT,instr(:l))
   CALL grps02(ioerr,UNIT,'1 setlinejoin 1 setlinecap 1 LW 1')
   CALL grps02(ioerr,UNIT,'%%EndPageSetup')
   CALL grps02(ioerr,UNIT,'%%PageBoundingBox: (atend)')
   DO  nsym=0,31
      marker(nsym)=0
   END DO
   mfac=0.0_pg
   bbxmin=width
   bbymin=height
   bbxmax=0.0_pg
   bbymax=0.0_pg
   RETURN
!
!--- IFUNC=12, Draw line. ----------------------------------------------
!
170 CONTINUE
   i0=nint(rbuf(1))
   j0=nint(rbuf(2))
   i1=nint(rbuf(3))
   j1=nint(rbuf(4))
   IF(i0 == lasti.AND.j0 == lastj)THEN
!        -- suppress zero-length continuation segment
      IF(i0 == i1.AND.j0 == j1)RETURN
      CALL grfao('# # C',l,instr,(i1-i0),(j1-j0),0,0)
      nseg=nseg+1
   ELSE
      nseg=1
      CALL grfao('# # # # L',l,instr,(i1-i0),(j1-j0),i0,j0)
   END IF
   lasti=i1
   IF(nseg > 200)lasti=-1
   lastj=j1
   bbxmin=MIN(bbxmin,i0-lw*5.0_pg,i1-lw*5.0_pg)
   bbxmax=MAX(bbxmax,i0+lw*5.0_pg,i1+lw*5.0_pg)
   bbymin=MIN(bbymin,j0-lw*5.0_pg,j1-lw*5.0_pg)
   bbymax=MAX(bbymax,j0+lw*5.0_pg,j1+lw*5.0_pg)
   GO TO 320
!
!--- IFUNC=13, Draw dot. -----------------------------------------------
!
180 CONTINUE
   i1=nint(rbuf(1))
   j1=nint(rbuf(2))
   CALL grfao('# # D',l,instr,i1,j1,0,0)
   lasti=i1
   lastj=j1
   bbxmin=MIN(bbxmin,i1-lw*5.0_pg)
   bbxmax=MAX(bbxmax,i1+lw*5.0_pg)
   bbymin=MIN(bbymin,j1-lw*5.0_pg)
   bbymax=MAX(bbymax,j1+lw*5.0_pg)
   GO TO 320
!
!--- IFUNC=14, End picture. --------------------------------------------
!
190 CONTINUE
   IF(lobuf /= 0)THEN
      CALL grps02(ioerr,UNIT,obuf(1:lobuf))
      lobuf=0
   END IF
   landsc=mode == 1.OR.mode == 3
!     -- optionally write identification
   CALL grgenv('IDENT',instr,l)
   IF(l > 0)THEN
      CALL gruser(suser,lu)
      CALL grdate(sdate,ld)
      posn=width-1
      IF(landsc)posn=height-1
      CALL grfao('('//suser(:lu)//' '//sdate(:ld)//' [#]) # # 100 /Helvetica RS',l, &
         instr,npage,posn,50,0)
      CALL grps02(ioerr,UNIT,'0 G')
      CALL grps02(ioerr,UNIT,instr(1:l))
   END IF
!     -- optionally draw bounding box
   CALL grgenv('PS_DRAW_BBOX',instr,l)
   IF(l > 0)THEN
      CALL grfao('0 G 0 LW newpath # # moveto',l,instr,  &
         nint(bbxmin),nint(bbymin),0,0)
      CALL grps02(ioerr,UNIT,instr(1:l))
      CALL grfao('# # lineto # # lineto',l,instr,nint(bbxmin),  &
         nint(bbymax),nint(bbxmax),nint(bbymax))
      CALL grps02(ioerr,UNIT,instr(1:l))
      CALL grfao('# # lineto closepath stroke',l,instr,  &
         nint(bbxmax),nint(bbymin),0,0)
      CALL grps02(ioerr,UNIT,instr(1:l))
   END IF
   CALL grps02(ioerr,UNIT,'PGPLOT restore showpage')
   CALL grps02(ioerr,UNIT,'%%PageTrailer')
   IF(landsc)THEN
      bb1=INT((width-bbymax+offw)*0.072_pg)
      bb2=INT((bbxmin+offh)*0.072_pg)
      bb3=1+INT((width-bbymin+offw)*0.072_pg)
      bb4=1+INT((bbxmax+offh)*0.072_pg)
   ELSE
      bb1=INT((bbxmin+offw)*0.072_pg)
      bb2=INT((bbymin+offh)*0.072_pg)
      bb3=1+INT((bbxmax+offw)*0.072_pg)
      bb4=1+INT((bbymax+offh)*0.072_pg)
   END IF
   CALL grfao('%%PageBoundingBox: # # # #',l,instr,bb1,bb2,bb3,bb4)
   CALL grps02(ioerr,UNIT,instr(1:l))
   IF(npage == 1)THEN
      bbox(1)=bb1
      bbox(2)=bb2
      bbox(3)=bb3
      bbox(4)=bb4
   ELSE
      bbox(1)=MIN(bbox(1),bb1)
      bbox(2)=MIN(bbox(2),bb2)
      bbox(3)=MAX(bbox(3),bb3)
      bbox(4)=MAX(bbox(4),bb4)
   END IF
   RETURN
!
!--- IFUNC=15, Select color index. -------------------------------------
!
200 CONTINUE
   ci=nint(rbuf(1))
   IF(color)THEN
      CALL grfao('# # # K',l,instr,nint(1024.0_pg*rvalue(ci)),  &
         nint(1024.0_pg*gvalue(ci)),nint(1024.0_pg*bvalue(ci)),0)
   ELSE
      CALL grfao('# G',l,instr,nint(1024.0_pg*rvalue(ci)),0,0,0)
   END IF
   lasti=-1
   GO TO 320
!
!--- IFUNC=16, Flush buffer. -------------------------------------------
!
210 CONTINUE
   IF(lobuf /= 0)THEN
      CALL grps02(ioerr,UNIT,obuf(1:lobuf))
      lobuf=0
   END IF
   RETURN
!
!--- IFUNC=17, Read cursor. --------------------------------------------
!    (Not implemented: should not be called.)
!
!
!--- IFUNC=18, Erase alpha screen. -------------------------------------
!    (Null operation: there is no alpha screen.)
!
220 CONTINUE
   RETURN
!
!--- IFUNC=19, Set line style. -----------------------------------------
!    (Not implemented: should not be called.)
!
!
!--- IFUNC=20, Polygon fill. -------------------------------------------
!
230 CONTINUE
   IF(npts == 0)THEN
      npts=INT(rbuf(1))
      start=.true.
      RETURN
   ELSE
      npts=npts-1
      i0=nint(rbuf(1))
      j0=nint(rbuf(2))
      IF(start)THEN
         CALL grfao('# # BP',l,instr,i0,j0,0,0)
         start=.false.
         lasti=i0
         lastj=j0
      ELSE IF(npts == 0)THEN
         CALL grfao('# # EP',l,instr,(i0-lasti),(j0-lastj),0,0)
         lasti=-1
         lastj=-1
      ELSE
         CALL grfao('# # LP',l,instr,(i0-lasti),(j0-lastj),0,0)
         lasti=i0
         lastj=j0
      END IF
      bbxmin=MIN(bbxmin,i0-lw*5.0_pg)
      bbxmax=MAX(bbxmax,i0+lw*5.0_pg)
      bbymin=MIN(bbymin,j0-lw*5.0_pg)
      bbymax=MAX(bbymax,j0+lw*5.0_pg)
      GO TO 320
   END IF
!
!--- IFUNC=21, Set color representation. -------------------------------
!
240 CONTINUE
   IF(color)THEN
      ci=INT(rbuf(1))
      rvalue(ci)=rbuf(2)
      gvalue(ci)=rbuf(3)
      bvalue(ci)=rbuf(4)
   ELSE
      ci=INT(rbuf(1))
      rvalue(ci)=0.30_pg*rbuf(2)+0.59_pg*rbuf(3)+0.11_pg*rbuf(4)
      gvalue(ci)=rvalue(ci)
      bvalue(ci)=rvalue(ci)
   END IF
   RETURN
!
!--- IFUNC=22, Set line width. -----------------------------------------
!
250 CONTINUE
   lw=rbuf(1)
   IF(ABS(REAL(INT(lw),KIND=pg) - lw) <= EPSILON(lw))THEN
      CALL grfao('# LW',l,instr,INT(lw),0,0,0)
   ELSE
      WRITE(instr,'(F6.2,'' LW'')')lw
      l=9
   END IF
   lasti=-1
   GO TO 320
!
!--- IFUNC=23, Escape. -------------------------------------------------
!
260 CONTINUE
   IF(lobuf /= 0)THEN
!         -- flush buffer first
      CALL grps02(ioerr,UNIT,obuf(1:lobuf))
      lobuf=0
   END IF
   CALL grps02(ioerr,UNIT,chr(:lchr))
   lasti=-1
   RETURN
!
!--- IFUNC=26, Image.---------------------------------------------------
!
270 CONTINUE
   n=INT(rbuf(1))
   IF(n == 0)THEN
!         -- First: setup for image
!         -- Set clipping region (RBUF(2...5))
      nxp=INT(rbuf(2))
      nyp=INT(rbuf(3))
      xorg=INT(rbuf(4))
      xlen=INT(rbuf(5)-rbuf(4))
      yorg=INT(rbuf(6))
      ylen=INT(rbuf(7)-rbuf(6))
      bbxmin=MIN(bbxmin,rbuf(4),rbuf(5))
      bbxmax=MAX(bbxmax,rbuf(4),rbuf(5))
      bbymin=MIN(bbymin,rbuf(6),rbuf(7))
      bbymax=MAX(bbymax,rbuf(6),rbuf(7))
!
      CALL grps02(ioerr,UNIT,'gsave newpath')
      CALL grfao('# # moveto # 0 rlineto 0 # rlineto',l,instr,  &
         xorg,yorg,xlen,ylen)
      CALL grps02(ioerr,UNIT,instr(:l))
      CALL grfao('# 0 rlineto closepath clip',l,instr,-xlen,0,0,0)
      CALL grps02(ioerr,UNIT,instr(:l))
!         --
      CALL grfao('/picstr # string def',l,instr,nxp,0,0,0)
      CALL grps02(ioerr,UNIT,instr(:l))
      CALL grfao('# # 8 [',l,instr,nxp,nyp,0,0)
      CALL grps02(ioerr,UNIT,instr(:l))
      WRITE(instr,'(6(1PE10.3, 1X), '']'')')(rbuf(i),i=8,13)
      CALL grps02 (ioerr,UNIT,instr(:67))
      IF(color)THEN
         CALL grps02(ioerr,UNIT, &
            '{currentfile picstr readhexstring pop} false 3 colorimage')
      ELSE
         CALL grps02(ioerr,UNIT,'{currentfile picstr readhexstring pop} image')
      END IF
   ELSE IF(n == -1)THEN
!         -- Last: terminate image
      CALL grps02 (ioerr,UNIT,'grestore')
   ELSE
!         -- Middle: write N image pixels; each pixel uses 6 chars
!            in INSTR, so N must be <= 20.
      l=0
      kmax=1
      IF(color)kmax=3
      DO  i=1,n
         ci=INT(rbuf(i+1))
         rgb(1)=nint(255.0_pg*rvalue(ci))
         rgb(2)=nint(255.0_pg*gvalue(ci))
         rgb(3)=nint(255.0_pg*bvalue(ci))
         DO  k=1,kmax
            high=rgb(k)/16
            low=rgb(k)-16*high
            l=l+1
            instr(l:l)=hexdig(high)
            l=l+1
            instr(l:l)=hexdig(low)
         END DO
      END DO
      CALL grps02(ioerr,UNIT,instr(1:l))
   END IF
   RETURN
!
!--- IFUNC=28, Marker.--------------------------------------------------
!
300 CONTINUE
   nsym=nint(rbuf(1))
!     -- Output code for this marker if necessary
   IF(marker(nsym) == 0)THEN
      IF(lobuf > 0)CALL grps02(ioerr,UNIT,obuf(1:lobuf))
      lobuf=0
      CALL grps03(ioerr,nsym,UNIT)
      marker(nsym)=1
   END IF
!     -- Output scale factor
   IF(ABS(rbuf(4) - mfac) > EPSILON(mfac))THEN
      IF(lobuf > 0)CALL grps02(ioerr,UNIT,obuf(1:lobuf))
      lobuf=0
      mfac=rbuf(4)
      WRITE(instr,'(''/MFAC '',F10.3,'' def'')') mfac
      CALL grps02(ioerr,UNIT,instr(1:24))
   END IF
!     -- Output an instruction to draw one marker
   i1=nint(rbuf(2))
   j1=nint(rbuf(3))
   CALL grfao ('# # M#',l,instr,i1,j1,nsym,0)
   lasti=-1
   bbxmin=MIN(bbxmin,i1-mfac*rad(nsym))
   bbxmax=MAX(bbxmax,i1+mfac*rad(nsym))
   bbymin=MIN(bbymin,j1-mfac*rad(nsym))
   bbymax=MAX(bbymax,j1+mfac*rad(nsym))
   GO TO 320
!
!--- IFUNC=29, Query color representation.------------------------------
!
310 CONTINUE
   ci=nint(rbuf(1))
   nbuf=4
   rbuf(2)=rvalue(ci)
   rbuf(3)=gvalue(ci)
   rbuf(4)=bvalue(ci)
   RETURN
!
!-----------------------------------------------------------------------
! Buffer output if possible.
!
320 IF((lobuf+l+1) > 132)THEN
      CALL grps02(ioerr,UNIT,obuf(1:lobuf))
      obuf(1:l)=instr(1:l)
      lobuf=l
   ELSE
      IF(lobuf > 1)THEN
         lobuf=lobuf+1
         obuf(lobuf:lobuf)=' '
      END IF
      obuf(lobuf+1:lobuf+l)=instr(1:l)
      lobuf=lobuf+l
   END IF
   RETURN
!-----------------------------------------------------------------------
! Error: unimplemented function.
!
330 WRITE(msg,'(''UNIMPLEMENTED FUNCTION IN PS DEVICE DRIVER: '',I10)')ifunc
   CALL grwarn(msg)
   nbuf=-1
   RETURN
!
END SUBROUTINE psdriv
!
!-----------------------------------------------------------------------
!
!*GRPS03 -- PGPLOT PostScript driver, marker support
!+
SUBROUTINE grps03(ioerr,nsym,UNIT)
!
! Write PostScript instructions for drawing graph marker number NSYM
! on Fortran unit UNIT.
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: ioerr
   INTEGER, INTENT(IN OUT)                  :: nsym
   INTEGER, INTENT(IN OUT)                  :: UNIT
!
   CHARACTER (LEN=80) :: t(6)
   INTEGER :: i,n
!
   IF(nsym < 0.OR.nsym > 31)RETURN
!
!GO TO (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,  &
!    170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320),nsym+1
!
   IF(nsym+1 == 1)GO TO 10
   IF(nsym+1 == 2)GO TO 20
   IF(nsym+1 == 3)GO TO 30
   IF(nsym+1 == 4)GO TO 40
   IF(nsym+1 == 5)GO TO 50
   IF(nsym+1 == 6)GO TO 60
   IF(nsym+1 == 7)GO TO 70
   IF(nsym+1 == 8)GO TO 80
   IF(nsym+1 == 9)GO TO 90
   IF(nsym+1 == 10)GO TO 100
   IF(nsym+1 == 11)GO TO 110
   IF(nsym+1 == 12)GO TO 120
   IF(nsym+1 == 13)GO TO 130
   IF(nsym+1 == 14)GO TO 140
   IF(nsym+1 == 15)GO TO 150
   IF(nsym+1 == 16)GO TO 160
   IF(nsym+1 == 17)GO TO 170
   IF(nsym+1 == 18)GO TO 180
   IF(nsym+1 == 19)GO TO 190
   IF(nsym+1 == 20)GO TO 200
   IF(nsym+1 == 21)GO TO 210
   IF(nsym+1 == 22)GO TO 220
   IF(nsym+1 == 23)GO TO 230
   IF(nsym+1 == 24)GO TO 240
   IF(nsym+1 == 25)GO TO 250
   IF(nsym+1 == 26)GO TO 260
   IF(nsym+1 == 27)GO TO 270
   IF(nsym+1 == 28)GO TO 280
   IF(nsym+1 == 29)GO TO 290
   IF(nsym+1 == 30)GO TO 300
   IF(nsym+1 == 31)GO TO 310
   IF(nsym+1 == 32)GO TO 320
!
   GO TO 330
!
10 t(1)='/M0 {MB -6 -6 moveto 0 12 rlineto 12 0 rlineto'
   t(2)='0 -12 rlineto closepath stroke ME} bind def'
   n=2
   GO TO 330
20 t(1)='/M1 {MB 0 0 1 FC ME} bind def'
   n=1
   GO TO 330
30 t(1)='/M2 {MB 0 7 moveto 0 -14 rlineto -7 0 moveto'
   t(2)='14 0 rlineto stroke ME} bind def'
   n=2
   GO TO 330
40 t(1)='/M3 {MB 0 6 moveto 0 -6 lineto -5 3 moveto 5 -3 lineto'
   t(2)='5 3 moveto -5 -3 lineto stroke ME} bind def'
   n=2
   GO TO 330
50 t(1)='/M4 {MB 0 0 7 CC ME} bind def'
   n=1
   GO TO 330
60 t(1)='/M5 {MB -5 -5 moveto 10 10 rlineto -5 5 moveto'
   t(2)='10 -10 rlineto stroke ME} bind def'
   n=2
   GO TO 330
70 t(1)='/M6 {MB -6 -6 moveto 0 12 rlineto 12 0 rlineto'
   t(2)='0 -12 rlineto closepath stroke ME} bind def'
   n=2
   GO TO 330
80 t(1)='/M7 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
   t(2)='stroke ME} bind def'
   n=2
   GO TO 330
90 t(1)='/M8 {MB 0 7 moveto 0 -14 rlineto -7 0 moveto 14 0 rlineto'
   t(2)='stroke 0 0 7 CC ME} bind def'
   n=2
   GO TO 330
100 t(1)='/M9 {MB 0 0 1 FC 0 0 7 CC ME} bind def'
   n=1
   GO TO 330
110 t(1)='/M10 {MB -9 9 moveto -8 7 lineto -7 3 lineto -7 -3 lineto'
   t(2)='-8 -7 lineto -9 -9 lineto -7 -8 lineto -3 -7 lineto'
   t(3)='3 -7 lineto 7 -8 lineto 9 -9 lineto 8 -7 lineto'
   t(4)='7 -3 lineto 7 3 lineto 8 7 lineto 9 9 lineto 7 8 lineto'
   t(5)='3 7 lineto -3 7 lineto  -7 8 lineto closepath stroke'
   t(6)='ME} bind def'
   n=6
   GO TO 330
120 t(1)='/M11 {MB 0 10 moveto -6 0 lineto 0 -10 lineto 6 0 lineto'
   t(2)='closepath stroke ME} bind def'
   n=2
   GO TO 330
130 t(1)='/M12 {MB 0 9 moveto -2 3 lineto -8 3 lineto -3 -1 lineto'
   t(2)='-5 -7 lineto 0 -3 lineto 5 -7 lineto 3 -1 lineto 8 3'
   t(3)='lineto 2 3 lineto closepath stroke ME} bind def'
   n=3
   GO TO 330
140 t(1)='/M13 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
   t(2)='fill ME} bind def'
   n=2
   GO TO 330
150 t(1)='/M14 {MB -2 6 moveto -2 2 lineto -6 2 lineto -6 -2 lineto'
   t(2)='-2 -2 lineto -2 -6 lineto 2 -6 lineto 2 -2 lineto'
   t(3)='6 -2 lineto 6 2 lineto 2 2 lineto 2 6 lineto closepath'
   t(4)='stroke ME} bind def'
   n=4
   GO TO 330
160 t(1)='/M15 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
   t(2)='0 -8 moveto 7 4 lineto -7 4 lineto closepath stroke ME}'
   t(3)='bind def'
   n=3
   GO TO 330
170 t(1)='/M16 {MB -4 -4 moveto 0 8 rlineto 8 0 rlineto 0 -8'
   t(2)='rlineto closepath fill ME} bind def'
   n=2
   GO TO 330
180 t(1)='/M17 {MB 0 0 4.5 FC ME} bind def'
   n=1
   GO TO 330
190 t(1)='/M18 {MB 0 9 moveto -2 3 lineto -8 3 lineto -3 -1 lineto'
   t(2)=' -5 -7 lineto 0 -3 lineto 5 -7 lineto 3 -1 lineto 8 3'
   t(3)='lineto 2 3 lineto closepath fill ME} bind def'
   n=3
   GO TO 330
200 t(1)='/M19 {MB -12 -12 moveto 0 24 rlineto 24 0 rlineto 0 -24'
   t(2)='rlineto closepath stroke ME} bind def'
   n=2
   GO TO 330
210 t(1)='/M20 {MB 0 0 2 CC ME} bind def'
   n=1
   GO TO 330
220 t(1)='/M21 {MB 0 0 4 CC ME} bind def'
   n=1
   GO TO 330
230 t(1)='/M22 {MB 0 0 5 CC ME} bind def'
   n=1
   GO TO 330
240 t(1)='/M23 {MB 0 0 7 CC ME} bind def'
   n=1
   GO TO 330
250 t(1)='/M24 {MB 0 0 11 CC ME} bind def'
   n=1
   GO TO 330
260 t(1)='/M25 {MB 0 0 17 CC ME} bind def'
   n=1
   GO TO 330
270 t(1)='/M26 {MB 0 0 22 CC ME} bind def'
   n=1
   GO TO 330
280 t(1)='/M27 {MB 0 0 41 CC ME} bind def'
   n=1
   GO TO 330
290 t(1)='/M28 {MB -6 2 moveto -9 0 lineto -6 -2 lineto -3 5'
   t(2)='moveto -8 0 lineto -3 -5 lineto -8 0 moveto 9 0 lineto'
   t(3)='stroke ME} bind def'
   n=3
   GO TO 330
300 t(1)='/M29 {MB 6 2 moveto 9 0 lineto 6 -2 lineto 3 5 moveto'
   t(2)='8 0 lineto 3 -5 lineto 8 0 moveto -9 0 lineto stroke ME}'
   t(3)='bind def'
   n=3
   GO TO 330
310 t(1)='/M30 {MB 2 6 moveto 0 9 lineto -2 6 lineto 5 3 moveto'
   t(2)='0 8 lineto -5 3 lineto 0 8 moveto 0 -9 lineto stroke ME}'
   t(3)='bind def'
   n=3
   GO TO 330
320 t(1)='/M31 {MB 2 -6 moveto 0 -9 lineto -2 -6 lineto 5 -3'
   t(2)='moveto 0 -8 lineto -5 -3 lineto 0 -8 moveto 0 9 lineto'
   t(3)='stroke ME} bind def'
   n=3
   GO TO 330
!
330 DO  i=1,n
      CALL grps02(ioerr,UNIT,t(i))
   END DO
!
   RETURN
!
END SUBROUTINE grps03
!
!*GRPS02 -- PGPLOT PostScript driver, copy buffer to file
!+
SUBROUTINE grps02 (ier,UNIT,s)
!
! Support routine for PSdriver: write character string S on
! specified Fortran unit.
!
! Error handling: if IER is not 0 on input, the routine returns
! immediately. Otherwise IER receives the I/O status from the Fortran
! write (0 => success).
!-----------------------------------------------------------------------
!
   IMPLICIT NONE
!
   INTEGER, INTENT(IN OUT)                  :: ier
   INTEGER, INTENT(IN OUT)                  :: UNIT
   CHARACTER (LEN=*), INTENT(IN)            :: s
!
   IF(ier == 0)THEN
      WRITE(UNIT,'(A)',IOSTAT=ier) s
      IF(ier /= 0)CALL grwarn ('++WARNING++ Error writing PostScript file')
   END IF
!
   RETURN
!
END SUBROUTINE grps02
!
!   !@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()
!
