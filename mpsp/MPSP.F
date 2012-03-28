      PROGRAM MPSP
C     Program to multiply a spectrum by a coefficient and store the product
C     spectrum
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /CA/ OFL(200000),OWL(200000),WT(200000)
      DIMENSION FL1(3)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 48 FL1
      CHARACTER * 7 LABEL
      DATA IN1,IN2,NOUT1/1,2,3/
  100 FORMAT (A48)
C  101 FORMAT (F15.6,E15.6)
  101 FORMAT (F10.3,1PE10.3)
C*********************************************************************
      OPEN (UNIT = 10,     FILE ='MPSP.ILS',
     $STATUS = 'OLD')
      READ ( 10, 100)       (FL1(J), J=1,3)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(3),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = ' **MPSP'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Read multiplier
      CALL DREAD ( RF, 'E14.7')
      DO J=1,200000
          READ ( IN2, 101, END=1, ERR=90) OWL(J),OFL(J)
		OFL(J)=OFL(J)*RF
      END DO
      CALL PMDSTOP
C     NOWL=Number of wavelengths
    1 NOWL=J-1
C     
      DO J=1,NOWL
		WRITE (NOUT1,101) OWL(J),OFL(J)
	END DO
C*********************************************************************
      CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
C*********************************************************************
      STOP 'MPSP completed successfully'
   90 CALL PMDSTOP
      END
