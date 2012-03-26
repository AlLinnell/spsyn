      Program Mask
	USE MSFLIB
C     Program to mask a specified number of spectral intervals of specified widths
C     within a specified wavelength range.
C
      DIMENSION FL1(3)
	DIMENSION AMSKST(100),AMSKND(100)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
	CHARACTER * 48 FL1
	CHARACTER * 7 LABEL
	INTEGER(4) RESULT
	DATA IN1,IN2,NOUT1/1,2,3/
  100 FORMAT (A48)
  110 FORMAT (F13.2,E14.5)
C*********************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN ( UNIT = 4,     FILE ='C:\SPSYN\MASK\MASK.ILS',
     $STATUS = 'OLD')
      READ ( 4, 100)       (FL1(J), J=1,3)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(3),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '***MASK'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL EREAD (STARTWL,'F10.5')
      CALL EREAD (ENDWL,'F10.5')
      CALL IREAD (NMASK)
	IF (NMASK.GT.100) CALL PMDSTOP
	DO I=1,NMASK
		CALL EREAD (STMSKI,'F10.5')
		CALL EREAD (ENDMSKI,'F10.5')
		AMSKST(I)=STMSKI
		AMSKND(I)=ENDMSKI
	END DO
C
      DO J=1,60000
		READ (2,110,END=1) WL,FX
		DO K=1,NMASK
			IF ((WL.GE.AMSKST(K)).AND.(WL.LE.AMSKND(K))) THEN
				FX=0.0
			END IF
		END DO
		WRITE (3,110) WL,FX
	END DO
C*********************************************************************
    1 CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
C*********************************************************************
      STOP
	END
