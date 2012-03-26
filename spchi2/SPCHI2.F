      PROGRAM SPCHI2
C     Program to fit synthetic spectrum to observed spectrum by use
C     of chi squared
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION OFL(200000),OWL(200000),FL(200000),WL(200000),
     $CFL(200000),DFL(200000),ER(200000)
      DIMENSION FL1(6)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 48 FL1
      CHARACTER * 7 LABEL
	CHARACTER * 4 KEY
C      DOUBLE PRECISION DIV,SL,TLT,SDVD,DLM,WLCORR,RWL,SYSVL,WL2,WL1,
C     $WLST1,WLND1,WLST2,WLND2
      DATA IN1,IN2,IN3,NOUT1,NOUT2,NOUT3/1,2,3,5,6,7/
  100 FORMAT (A48)
  101 FORMAT (F20.11,E23.13,E23.13,A4)
  102 FORMAT (F15.6, E15.6)
  103 FORMAT (
     $1X,'Value of chi squared=',8X,1PE12.4,/,
     $1X,'Reduced chi squared=',9X,1PE12.4,/,
     $1X,'Number of bins=',12X,I5)
  107 FORMAT (1X,'Number of entries in array DFL  =',I7,/,
     $1X,'Mean value of array DFL         =',1PE12.4,/,
     $1X,'Standard deviation of array DFL =',1PE12.4)
  108 FORMAT (1X,'Starting wavelength          =',F10.3,/,
     $1X,'Ending wavelength            =',F10.3,/,
     $1X,'Wavelength range             =',F10.3)
  119 FORMAT (1X,'Observed spectrum mean residual =',1PE12.4)   
  110 FORMAT (20X,'Program SPCHI2',/)
  111 FORMAT (1X,'Probability that Chi^2 is as poor as the value of 
     $Chi^2 is Q=',E12.4)
  115 FORMAT (E15.5)   
C*********************************************************************
      OPEN ( UNIT = 10, FILE ='SPCHI2.ILS',STATUS = 'OLD')
      READ ( 10, 100)       (FL1(J), J=1,6)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(4),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT2, FILE = FL1(5),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT3, FILE = FL1(6),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '*SPCHI2'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NBIN)
      CALL DREAD (WLST1, 'F12.5')
      CALL DREAD (WLND1, 'F12.5')
	CALL DREAD (RFLX,  'E12.4')
      WLST1A=WLST1
      WLND1A=WLND1
C     Read observed spectrum
C     OWL(J)=array of observed wavelengths
C     OFL(J)=array of observed flux values
      ERSM=0.0
	M=0
      DO J=1,200000
          READ ( IN2, 101, END=1, ERR=90) OV,FLVL,ERR, KEY
C     Restrict wavelength range to the specified value
		IF ((OV.GE.WLST1).AND.(OV.LE.WLND1)) THEN
C     KEY is a parameter that permits masking individual observations
C     To mask a particular obseration, put something in the "KEY' field
		IF (KEY.EQ.'    ') THEN
			M=M+1
			OWL(M)=OV
			OFL(M)=FLVL
			ER(M)=DABS(ERR)
			IF (ERR.EQ.0.D0) THEN
				ER(M)=ER(M-1)
			END IF
			ERSM=ERSM+ERR
		END IF
		END IF
      END DO
      CALL PMDSTOP
C     NOWL=Number of observed wavelengths
C     STWL=First observed wavelength
C     ENDWL=Last observed wavelength
C     DLWL=Range of observed wavelengths
    1 NOWL=M
      STWL=OWL(1)
      ENDWL=OWL(NOWL)
      DLWL=ENDWL-STWL
C     Tabular wavelengths must be in ascending sequence
	IF (DLWL.LT.0.0) CALL PMDSTOP
C     ERSM is the mean statistical error of the OFL() values
      ERSM=ERSM/NOWL
C     Read synthetic spectrum
	DO K=1,200000
		READ (IN3, 102, END=2, ERR=91) WL(K),FL(K)
	END DO
	CALL PMDSTOP
    2 CONTINUE
C     NCWL=Number of wavelengths in synthetic spectrum
C     INWL=First synthetic spectrum wavelength
C     FWL=Final synthetic spectrum wavelength
      NCWL=K-1
	BGWL=WL(1)
	FWL=WL(NCWL)
C     Wavelength entries must be in ascending sequence
      IF (FWL.LT.BGWL) CALL PMDSTOP
	IF (WL(1).GT.OWL(1)) CALL PMDSTOP
	IF (WL(NCWL).LT.OWL(NOWL)) CALL PMDSTOP
C     Calculate an interpolated synthetic spectrum flux value for each
C     entry in the observed flux array.
      LWL=1
      DO L=1,NOWL
		CALL TPWL(FL,WL,OWL(L),LWL,NCWL,CFL(L))
		WRITE (NOUT2,102) OWL(L),CFL(L)
C     Calculate residuals, normalized by RFLX
		DFL(L)=OFL(L)-CFL(L)/RFLX
		WRITE (NOUT3,102) OWL(L),DFL(L)
	END DO
C     Process residuals for mean error, std. deviation of error and chi~^2
      VMNERR=0.D0
	ERSM=0.D0
      DO L=1, NOWL
		VMNERR=VMNERR+DFL(L)
		ERSM=ERSM+ER(L)
	END DO
	VMNERR=VMNERR/NOWL
	VAR=0.D0
      DO L=1,NOWL
		VAR=VAR+(DFL(L)-VMNERR)**2	
	END DO
	VAR=VAR/(NOWL-1)
	STDV=DSQRT(VAR)
	CHISQ=0.D0
	DO L=1,NOWL
		CHISQ=CHISQ+(DFL(L)/ER(L))**2
C          CHISQ=CHISQ+(DFL(L)/STDV)**2
	END DO
	CHISQR=CHISQ/NOWL
	WRITE (NOUT1, 110)
	WRITE (NOUT1, 119) ERSM
	WRITE (NOUT1, 108) WLST1,WLND1,DLWL
	WRITE (NOUT1, 107) NOWL,VMNERR,STDV
	WRITE (NOUT1, 103) CHISQ,CHISQR,NOWL
   90	STOP
   91 STOP
C*********************************************************************
      CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN3,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT2,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT3,    STATUS = 'KEEP')
C*********************************************************************

	END PROGRAM SPCHI2
