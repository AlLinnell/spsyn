      Program DERIV
C	USE MSFLIB
C     Program to calculate first derivative of a tabular function
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION OFL(200000),OWL(200000),FL(200000),WL(200000),
     $CFL(200000),DFL(200000)
      DIMENSION FL1(5)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 45 FL1,FMT1,FMT2
      CHARACTER * 7 LABEL
C      INTEGER(4) RESULT
      DATA IN1,IN2,IN3,IN4,NOUT1/1,2,3,4,6/
  100 FORMAT (A45)
C*********************************************************************
C      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN ( UNIT = 10, FILE ='DERIV.ILS',STATUS = 
     $'OLD')
      READ ( 10, 100)       (FL1(J), J=1,5)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = IN4,   FILE = FL1(4),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(5),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '**DERIV'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL DREAD (H, 'F12.5')
	CALL DUMMY (1)
      CALL DREAD (U, 'F12.5')
	CALL DUMMY (1)
C     Read format of first spectrum
	CALL CREAD(FMT1)
	CALL DUMMY (1)
C     Read format of second spectrum
	CALL CREAD(FMT2)
      DO J=1,200000
          READ ( IN2, FMT1, END=1, ERR=90) OWL(J),OFL(J)
      END DO      
	CALL PMDSTOP
C     NOWL=Number of wavelengths
    1 NOWL=J-1
      DO J=1,200000
          READ ( IN3, FMT1, END=2, ERR=91) WL(J),FL(J)
      END DO
      CALL PMDSTOP
C     NWL= Number of second spectrum wavelengths
    2 NWL=J-1
      DO J=1,200000
          READ ( IN4, FMT2, END=3, ERR=92) CFL(J),DFL(J)
      END DO
      CALL PMDSTOP
C     NWL= Number of second spectrum wavelengths
    3 NCWL=J-1
      DO J=1,NOWL
		IF ((OWL(J).NE.WL(J)).OR.((OWL(J).NE.CFL(J)))) CALL PMDSTOP
		A=(OFL(J)+FL(J))/2.D0
		B=U*DFL(J)
		C=(A+B)/H
		WRITE (NOUT1,FMT1) OWL(J),C
	END DO
C*********************************************************************
      CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN3,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN4,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
C*********************************************************************
      GO TO 93
   90 STOP
   91 STOP
   92 STOP
   93	END PROGRAM DERIV
