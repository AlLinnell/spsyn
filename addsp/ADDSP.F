      PROGRAM ADDSP
C     Program to add one spectrum to another, with the second multiplied
C     by an arbitrary coefficient.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /CA/ OFL(500000),OWL(500000),FL(500000),WL(500000),
     $CFL(500000),
     $WLB(500000),FLB(500000)
      DIMENSION FL1(6)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 60 FL1,FMT1,FMT2
      CHARACTER * 7 LABEL
      DATA IN1,IN2,IN3,NOUT1,NOUT2/1,2,3,4,5/
	DATA KEY/0/
  100 FORMAT (A60)
  110 FORMAT (20X,'Program ADDSP',/)
C*********************************************************************
      OPEN ( UNIT = 6,     FILE ='ADDSP.ILS',  STATUS = 'OLD')
      READ ( 6, 100)       (FL1(J), J=1,5)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(4),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT2, FILE = FL1(5),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '**ADDSP'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Read divisor for first spectrum
      CALL DREAD ( RF1, 'E12.4')
	CALL DUMMY (1)
C     Read divisor for second spectrum
      CALL DREAD ( RF2, 'E12.4')
	CALL DUMMY (1)
C     Read KEY. If KEY=1, then the output will be a combined spectrum,
C     with the output being 1/2 the sum of the first spectrum plus the
C     product of the second spectrum and RF, in the
C     wavelength overlap region. The output spectrum extends from the
C     shortest wavelength of the two spectra to the longest wavelength
C     of the two spectra.
C     If KEY.NE.1, then the output will be a sum of the first spectrum
C     and the second spectrum, the latter multiplied by RF, with the
C     output wavelength range the same as that of the first spectrum.
C     KEY NOT IMPLEMENTED!
	CALL IREAD (KEY)
C
      CALL DUMMY (1)
C     Read format of first spectrum
	CALL CREAD(FMT1)
	CALL DUMMY (1)
C     Read format of second spectrum
	CALL CREAD(FMT2)
C     Read first spectrum
C     This is the spectrum to which the second will be added
C     OWL(J)=array of wavelengths
C     OFL(J)=array of flux values
C
C     Read initial line of ASCII information
C      READ (IN2, 100) BUF
      DO J=1,500000
          READ ( IN2, FMT1, END=1, ERR=90) OWL(J),OFL(J)
		OFL(J)=OFL(J)/RF1
      END DO
      CALL PMDSTOP
C     NOWL=Number of wavelengths
    1 NOWL=J-1
C     Read second spectrum
C     WL(J)=Array of second spectrum wavelengths
C     FL(J)=Array of second spectrum flux values
C     
      DO J=1,500000
          READ ( IN3, FMT2, END=2, ERR=91) WL(J),FL(J)
		FL(J)=FL(J)/RF2
      END DO
      CALL PMDSTOP
C     NWL= Number of second spectrum wavelengths
    2 NWL=J-1
C     Calculate array which is the sum of the first and second 
C     synthetic spectra, with the second multiplied by an arbitrary
C     coefficient.
C     A new array, CFL, is the second synthetic spectrum 
C     interpolated to the wavelengths of the first spectrum.
C     The array FLB is the sum array.
C     
C     
C
   21 CALL SPPRC(NOWL,NWL,KK,KEY)
      DO J=1,KK
		IF ((WLB(J).EQ.0.D0).AND.(CFL(J).EQ.0.D0)) GO TO 50
		WRITE (NOUT1,FMT1) WLB(J),CFL(J)
		WRITE (NOUT2,FMT1) WLB(J),FLB(J)
	END DO
C*********************************************************************
   50 CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN3,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT2,    STATUS = 'KEEP')
C*********************************************************************
   92 STOP
   90 CALL PMDSTOP
   91 CALL PMDSTOP
      END PROGRAM ADDSP
