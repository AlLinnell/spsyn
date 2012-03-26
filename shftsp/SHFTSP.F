      PROGRAM SHFTSP
	USE MSFLIB
C     Program to fit synthetic system spectrum to observed spectrum by
C     use of chi squared
C     The synthetic spectrum has been processed by program SPPRC1 to 
C     apply corrections for slope of the continuum, reference level for 
C     the continuum, third light, system Doppler shift, zero point
C     wavelength correction, and wavelength scale factor correction.
C     The uncorrected synthetic spectrum input to SPPRC1 is in, e.g.,
C     D:\OA2\BTLYRSM.DAT. The corrected synthetic spectrum output by
C     SPPRC1 is in, e.g., D:\OA2\BTLYRSMA.DAT. The corrected and uncorrected
C     spectra are in the same format.
C     
C     This program parallels program FTSP. This program determines a relative
C     wavelength shift between two spectra by successively shifting the synthetic
C     spectrum in wavelength and calculating the chi^2 fit to the observed
C     spectrum.
      DIMENSION OFL(60000),OWL(60000),FL(200000),WL(200000),CFL(60000),
     $DFL(60000),DFL2(60000),WLC(60000),FLP(200000)
	DIMENSION VCOUNT(5000)
      DIMENSION FL1(7)
	DOUBLE PRECISION  SDV2
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 34 FL1,FMT1,FMT2
      CHARACTER * 7 LABEL
      INTEGER(4) RESULT
      DATA IN1,IN2,IN3,NOUT1,NOUT2,NOUT3,NOUT4/1,2,3,4,5,6,7/
  100 FORMAT (A34)
  101 FORMAT (2F15.6)
  102 FORMAT (F15.6, E15.6)
  103 FORMAT (20X,'Program SHFTSP',/
     $1X,'Number of fitting parameters=',1X,I2,/,
     $1X,'Number of observed wavelengths=',1X,I5)
  106 FORMAT (I5,F12.3,3F10.5,E13.4)
  107 FORMAT (1X,'Iteration number',7X,'DWL',7X,'mean resid.',1X,'mean
     $abs. res.',1X,'std.dev.',3X,'Chi^2',9X,'red. Chi^2')
  108 FORMAT (1X,'Starting wavelength(data)   =',F10.3,/,
     $1X,'Ending wavelength(data)     =',F10.3,/,
     $1X,'Wavelength range            =',F10.3)
  109 FORMAT (2X,I5,9X,E15.5,3(1X,E11.4),1X,E13.6,1X,E15.7)
  110 FORMAT (A80)
  111 FORMAT (F10.3,F10.3)
C*********************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN ( UNIT = 8,     FILE ='C:\SPSYN\SHFTSP.ILS',  STATUS = 'OLD')
      READ ( 8, 100)       (FL1(J), J=1,7)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(4),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT2, FILE = FL1(5),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT3, FILE = FL1(6),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT4, FILE = FL1(7),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '*SHFTSP'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NPARAM)
	CALL EREAD (DIV1,'E10.3')
	CALL EREAD (DIV2,'E10.3')
	CALL EREAD (ADD2,'E10.3')
	CALL EREAD (WLL, 'F10.5')
	CALL EREAD (WLU, 'F10.5')
	CALL EREAD (ADDWL, 'F10.5')
	CALL EREAD (DWL, 'E10.3')
	CALL IREAD (NITER)
	CALL CREAD (FMT1)
	CALL CREAD (FMT2)
C
C
     	K=0
      DO J=1,60000
          READ ( IN2, FMT1, END=10, ERR=90) W, FLX
		FLX=FLX/DIV1
		IF ((W.GE.WLL).AND.(W.LE.WLU)) THEN
			K=K+1
			OWL(K)=W
			OFL(K)=FLX
		END IF
	END DO
	CALL PMDSTOP
   10 NOWL=K
C     The number of "bins" equals the number of observed wavelengths
      NBIN=NOWL
C
      STWL=OWL(1)
      ENDWL=OWL(NOWL)
      DLWL=ENDWL-STWL
C     Read synthetic spectrum
      K=0
      DO J=1,200000
          READ ( IN3, FMT2, END=20, ERR=91) W, FLX
		FLX=FLX/DIV2
		IF ((W.GE.WLL).AND.(W.LE.WLU)) THEN
    			K=K+1
			WL(K)=W
			FL(K)=FLX+ADD2
		END IF
      END DO
      CALL PMDSTOP
   20 NWL=K
      WRITE (NOUT1, 103) NPARAM,NOWL
      WRITE (NOUT1, 108) STWL,ENDWL,DLWL
	WRITE (NOUT1,107)
C
C     Do comparison NITER times
C
      DO ITER=1,NITER
	DO J=1,NOWL
		VCOUNT(J)=0.0
		CFL(J)=0.0
		DFL(J)=0.0
		DFL2(J)=0.0
	END DO
	DO J=1,NWL
		WLC(J)=WL(J)+DWL*(ITER-1)+ADDWL
		FLP(J)=FL(J)
	END DO
C     The following comments pertain to the corresponding parameters
C     OFL=array of observed flux values, of length NOWL
C     OWL=corresponding array of observed wavelengths
C     NOWL=number of entries in arrays OFL and OWL
C     FL=array of calculated flux values, of length NWL
C     FLC=array of flux values, relative to current zero point
C     WL=corresponding array of assigned wavelengths
C     NWL=number of entries in arrays FL and WL
C     CFL=array of calculated flux values, interpolated at wavelengths
C        OWL
C     DFL=array of differences OFL-CFL, of length NOWL
C     NBIN=number of bins for distribution count of DFL
C     SDV=std. deviation of array DFL
C     VMN=mean value of array DFL
C     DLT=width of each bin, in units of std. deviation
C     DFL2=similar to DFL, but sorted ito increasing order. Used in DFSP
	SDV2=0.D0
      SDV=0.0
      VMN=0.0
      DO 1 J=1,NOWL
C     Interpolate a value of calculated flux at the wavelength of the observed
C     flux
         CALL TPWL(FLP,WLC,OWL(J),LWL,NWL,CFL(J))
         IF (CFL(J).EQ.0.0) GO TO 1
C     Calculate difference between observed and calculated flux
         DFL(J)=OFL(J)-CFL(J)
         DFL2(J)=OFL(J)-CFL(J)
C     Accumulate mean value of diffences
         VMN=VMN+DFL(J)
    1 CONTINUE
C     Calculate mean value of differences
      VMN=VMN/NOWL
C     Calculate std. deviation of differences
C     SDV2 is double precision
	DO J=1,NOWL
		SDV2=SDV2+(DFL(J)-VMN)**2
	END DO
      SDV2=DSQRT(SDV2/(NOWL-1))
C     Set single precision SDV equal to double precision SDV2
	SDV=SDV2
C     Sort array of differences
      CALL SORT(NOWL,DFL2)
C     Calculate chi squared
C     Initialize chi^2 to 0.0.
      CH2=0.0
C     Assume the standard deviation at each wavelength is constant,
C     and is equal to SDV
C     See Numerical Recipes, p.502, eq.14.1.5 for following expression
C     for chi^2.
	DO J=1,NOWL
		CH2=CH2+(DFL2(J)/SDV)**2
	END DO
C     Calculate degrees of freedom
      DGFREED=NBIN-NPARAM
C     Calculate reduced chi^2
C     See "An Introduction to Error Analysis", by John R. Taylor, 1982,
C     p.229, eq.12.16
      RCHI2=CH2/DGFREED
C
      CHSQ=CH2
	RDCHSQ=RCHI2
C     The entries in VPRED now are sorted into increasing order
      
C     Calculate mean absolute differences
      VABSDF=0.0
	DO J=1,NOWL
		VABSDF=VABSDF+ABS(DFL(J))
	END DO
	VABSDF=VABSDF/NBIN
C
	SM=(ITER-1)*DWL
	WRITE (NOUT1, 109) ITER,SM,VMN,VABSDF,SDV,CHSQ,RDCHSQ
C     Note that the quality of fit is calculated only for a number of
C     interpolated values equal to the number of observed values, and
C     that the lengths of arrays CFL, Dfl and Dfl2 equal NOWL, the number
C     of observed wavelengths
      DO J=1,NOWL
		IF ((OWL(J).GE.WLL).AND.(OWL(J).LE.WLU)) THEN
			WRITE (NOUT4, 106) J,OWL(J),FL(J),CFL(J),DFL(J)
		END IF
      END DO
	END DO
      DLT=0.0
      DO J=1,NOWL
C     Write synthetic spectrum, interpolated to observed spectrum wavelengths
         WRITE ( NOUT2, 101) OWL(J),CFL(J)
C     Write difference spectrum, at the observed spectrum wavelengths
         WRITE ( NOUT3, 101) OWL(J),DFL(J)
      END DO
C*********************************************************************
      CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN3,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT2,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT3,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT4,    STATUS = 'KEEP')
C*********************************************************************
   92 STOP
   90 CALL PMDSTOP
   91 CALL PMDSTOP
      END
