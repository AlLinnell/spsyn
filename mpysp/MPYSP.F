      PROGRAM MPYSP
	USE MSFLIB
C     Program to multiply one spectrum by another, with the second multiplied
C     by an arbitrary coefficient.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /CA/ OFL(300000),OWL(300000),FL(300000),WL(300000),
     $CFL(300000),
     $WLB(300000),FLB(300000)
      DIMENSION FL1(6)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 45 FL1
      CHARACTER * 7 LABEL
      INTEGER(4) RESULT
      DATA IN1,IN2,IN3,NOUT1,NOUT2/1,2,3,4,5/
	DATA KEY/0/
  100 FORMAT (A45)
c  101 FORMAT (F12.3,E15.5)
  101 FORMAT (F15.6,E15.5)
  102 FORMAT (F15.6,E16.5)
  110 FORMAT (20X,'Program MPYSP',/)
C*********************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN ( UNIT = 10,     FILE ='C:\SPSYN\MPYSP\MPYSP.ILS',  
     $STATUS = 'OLD')
      READ ( 10, 100)       (FL1(J), J=1,5)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(4),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT2, FILE = FL1(5),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '**MPYSP'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Read multiplier for second spectrum
      CALL DREAD ( RF, 'E12.4')
C     Read first spectrum
C     This is the spectrum by which the second will be multiplied
C     OWL(J)=array of wavelengths
C     OFL(J)=array of flux values
      DO J=1,300000
         READ ( IN2, 101, END=1, ERR=90) OWL(J),OFL(J)
      END DO
      CALL PMDSTOP
C     NOWL=Number of wavelengths
    1 NOWL=J-1
C     Read second spectrum
C     WL(J)=Array of second spectrum wavelengths
C     FL(J)=Array of second spectrum flux values
C     
      DO J=1,300000
         READ ( IN3, 102, END=2, ERR=91) WL(J),FL(J)
      END DO
      CALL PMDSTOP
C     NWL= Number of second spectrum wavelengths
    2 NWL=J-1
C     Calculate array which is the ratio of the first and second 
C     synthetic spectra, with the second multiplied by an arbitrary
C     coefficient.
C     A new array, CFL, is the second synthetic spectrum 
C     interpolated to the wavelengths of the first spectrum.
C     The array FLB is the product array.
C     
C     
c      WRITE (NOUT1,110)
C
   21 CALL SPRAT(NOWL,NWL,RF,KK,KEY)
      DO J=1,KK
		IF ((WLB(J).EQ.0.D0).AND.(CFL(J).EQ.0.D0)) GO TO 50
		WRITE (NOUT1,101) WLB(J),CFL(J)
		WRITE (NOUT2,101) WLB(J),FLB(J)
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
      END
