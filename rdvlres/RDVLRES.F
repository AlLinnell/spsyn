      PROGRAM FTRDVL
	USE MSFLIB
C     Program to fit theoretical radial velocity curve to observed rad. vels.by
C     use of chi squared
      DIMENSION PH(6000),RV1(6000),RV2(6000),RVPD1(6000),RVPD2(6000),
     $RES1(6000),RES2(6000),PHTH(6000),CURR1(6000),CURR2(6000)
      DIMENSION FL1(7)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 34 FL1,FMT1,FMT2
      CHARACTER * 7 LABEL
	CHARACTER * 80 BUF,BUFA
      INTEGER(4) RESULT
      DATA IN1,IN2,IN3,IN4,NOUT1,NOUT2,NOUT3/1,2,3,4,5,6,7/
	DATA TWPI/6.28318530718/
	BUF='THEORETICAL RADIAL VELOCITY CURVE'
  100 FORMAT (A34)
  101 FORMAT (F12.7,2F12.5)
  103 FORMAT (20X,'Program RDVLRES',/
     $1X,'Number of phases=',1X,I5)
  104 FORMAT (F10.6,2F8.1)
  107 FORMAT (1X,'Iteration number',2X,'param.',1X,'mean resid.',1X,
     $'mean abs.res.',1X,'std.dev.',4X,'Chi^2',9X,'red. Chi^2')
  109 FORMAT (2X,I5,9X,F8.3,3(1X,E11.4),1X,E13.6,1X,E15.7)
  110 FORMAT (A80)
C*********************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN ( UNIT = 8,     FILE ='C:\SPSYN\RDVLRES\RDVLRES.ILS',
     $STATUS = 'OLD')
      READ ( 8, 100)       (FL1(J), J=1,7)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = IN4,   FILE = FL1(4),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(5),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT2, FILE = FL1(6),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT3, FILE = FL1(7),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = 'RDVLRES'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NPARAM)
	CALL IREAD (KEY)
	CALL EREAD (VK1,'E10.3')
	CALL EREAD (Q,'E10.3')
	CALL EREAD (GAM1,'E10.3')
	CALL EREAD (GAM2,'E10.3')
	CALL EREAD (DELK,'E10.3')
	CALL EREAD (DELQ,'E10.3')
	CALL EREAD (DGAM1,'E10.3')
	CALL EREAD (DGAM2,'E10.3')
	CALL IREAD (NITER)
	CALL CREAD (FMT1)
	CALL CREAD (FMT2)
C
C
      READ (IN2,110) BUFA
      READ (IN3,110) BUFA
     	K=0
      DO J=1,60000
          READ ( IN2, FMT1, END=10, ERR=90) PH(J), RV1(J),RV2(J)
	END DO
	CALL PMDSTOP
   10	DO K=J,60000
          READ ( IN3, FMT1, END=11, ERR=90) PH(K),RV1(K),RV2(K)
      END DO
	CALL PMDSTOP
   11 NPH=K-1
C     The number of "bins" equals the number of observed rad.vels.
      NBIN=NPH
	WRITE (NOUT1,103) NPH
	WRITE (NOUT1,107)
C     Input the theoretical radial velocity curves
      READ (IN4,110) BUFA
C
      DO J=1,6000
		READ (IN4,FMT2,END=21) PHTH(J),CURR1(J),CURR2(J)
		PHTH(J)=PHTH(J)/TWPI
	END DO
	CALL PMDSTOP
   21 NTH=J-1
C     Determine theoretical radial vel. for each observed value
      DO J=1,NPH
		DO K=1,NTH-1
			IF ((PH(J)-PHTH(K).GE.0.0).AND.(PH(J)-PHTH(K+1).LT.0.0)) 
     $		THEN
				A=PH(J)-PHTH(K)
				B=PHTH(K+1)-PH(J)
				C=CURR1(K+1)-CURR1(K)
				D=CURR2(K+1)-CURR2(K)
				E=PHTH(K+1)-PHTH(K)
				RVPD1(J)=CURR1(K)+A/E*C
				RVPD2(J)=CURR2(K)+A/E*D
			END IF	
		END DO	
	END DO
C
C
C     Do comparison NITER times
C
      DO ITER=1,NITER
C     Calculate residuals
		DO J=1,NPH
			IF (ABS(RV1(J)).LT.900.0) THEN
				RES1(J)=RV1(J)-RVPD1(J)
			ELSE
				RES1(J)=0.0
			END IF
			IF (ABS(RV2(J)).LT.900.0) THEN
				RES2(J)=RV2(J)-RVPD2(J)
			ELSE
				RES2(J)=0.0
			END IF
		END DO
C     Calculate mean residuals
		VMN1=0.0
		VMN2=0.0
		DO J=1,NPH
			VMN1=VMN1+RES1(J)
			VMN2=VMN2+RES2(J)
		END DO
		VMN1=VMN1/NPH
		VMN2=VMN2/NPH
C     Calculate std. dev. of residuals
		SDV1=0.0
		SDV2=0.0
		DO J=1,NPH
			SDV1=SDV1+(RES1(J)-VMN1)**2
			SDV2=SDV2+(RES2(J)-VMN2)**2
		END DO
		SDV1=SQRT(SDV1/(NPH-1))
		SDV2=SQRT(SDV2/(NPH-1))
C     Calculate chi^2 and reduced chi^2
		CHI21=0.0
		CHI22=0.0
		DO J=1,NPH
			CHI21=CHI21+(RES1(J)/SDV1)**2
			CHI22=CHI22+(RES2(J)/SDV2)**2
		END DO
		DGFREED=NBIN-NPARAM
		RCHI21=CHI21/DGFREED
		RCHI22=CHI22/DGFREED
C     Calculate mean absolute differences
		VABSDF1=0.0
		VABSDF2=0.0
		DO J=1,NPH
			VABSDF1=VABSDF1+ABS(RES1(J))
			VABSDF2=VABSDF2+ABS(RES2(J))
		END DO
		VABSDF1=VABSDF1/NPH
		VABSDF2=VABSDF2/NPH
C     Write output for this iteration
	IF (KEY.EQ.0) THEN											
		WRITE (NOUT1, 109) ITER,VK1,VMN1,VABSDF1,SDV1,CHI21,RCHI21
	END IF
	IF (KEY.EQ.1) THEN
		WRITE (NOUT1, 109) ITER,Q,VMN2,VABSDF2,SDV2,CHI22,RCHI22
	END IF
	IF (KEY.EQ.2) THEN											
		WRITE (NOUT1, 109) ITER,GAM1,VMN1,VABSDF1,SDV1,CHI21,RCHI21
	END IF
	IF (KEY.EQ.3) THEN											
		WRITE (NOUT1, 109) ITER,GAM2,VMN2,VABSDF2,SDV2,CHI22,RCHI22
	END IF
C     Increment proper parameter
		IF (KEY.EQ.0) THEN 
			VK1=VK1+DELK
		END IF
		IF (KEY.EQ.1) THEN
			Q=Q+DELQ
		END IF
		IF (KEY.EQ.2) THEN
			GAM1=GAM1+DGAM1
		END IF
		IF (KEY.EQ.3) THEN
			GAM2=GAM2+DGAM2
		END IF
	END DO
C     Sort theoretical radial vel. curve by phase
      DO J=1,NPH
		DO K=1,NPH-1
			IF (PH(K+1).LT.PH(K)) THEN
				SV=PH(K+1)
				PH(K+1)=PH(K)
				PH(K)=SV
C
				SV=RVPD1(K+1)
				RVPD1(K+1)=RVPD1(K)
				RVPD1(K)=SV
C
				SV=RVPD2(K+1)
				RVPD2(K+1)=RVPD2(K)
				RVPD2(K)=SV
C
				SV=RES1(K+1)
				RES1(K+1)=RES1(K)
				RES1(K)=SV
C
				SV=RES2(K+1)
				RES2(K+1)=RES2(K)
				RES2(K)=SV
			END IF
		END DO
	END DO
      DO J=1,NPH
C     Write theoretical radial velocity curve
         WRITE ( NOUT2, 101) PH(J)*TWPI,RVPD1(J),RVPD2(J)
C     Wreite residuals
	   WRITE ( NOUT3, 101) PH(J)*TWPI,RES1(J),RES2(J)
      END DO
C*********************************************************************
      CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN3,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT2,    STATUS = 'KEEP')
	CLOSE ( UNIT = NOUT3,    STATUS = 'KEEP')
C*********************************************************************
   92 STOP
   90 CALL PMDSTOP
   91 CALL PMDSTOP
      END
