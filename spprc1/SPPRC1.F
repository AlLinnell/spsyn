      PROGRAM SPPRC1
	USE MSFLIB
C     Program to do initial processing of spectrum for comparison
C     with observed spectrum
C     File IN1 contains processing parameters.
C     File IN2 is the observed spectrum.
C     IN2 is input to establish the array of wavelengths for which the
C     processed input spectra will be output.
C     Files IN3, IN4, IN5 are, respectively, the system spectrum, the
C     first component synthetic spectrum, and the second component
C     synthetic spectrum.
C     The reference level RF multiplies each of the input synthetic spectra
C     at input. Then other processing takes place, and finally the processed
C     synthetic spectra are divided by RF.
C     The system radial velocity SYSVL, in km/sec., is applied to each of the
C     three input spectra.
C     The input flux values for the input spectra are processed to the
C     wavelengths of the observed spectrum. A straight line correction is
C     applied to change the slope of the input synthetic spectra. The
C     calculated correction is multilpied by the trim coefficient., CF,
C     before being applied.
C     The multiplier FMPL is not used.
C
C     Note that if the comparison will be with a rectified obseerved spectrum,
C     the input synthetic spectra should also be rectified. Do this by
C     dividing the calculated synthetic spectrum by the corresponding
C     continuum spectrum.
C
C     The output files NOUT1, NOUT2, NOUT3 are the processed synthetic spectra,
C     corresponding to IN3, IN4, IN5 respectively.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WLA(100000),FLA(100000)
	DIMENSION WL(12),F(12)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 36 FL1
	CHARACTER * 80 FMT1
      INTEGER(4) RESULT
      DIMENSION FL1(8)
      DATA IN1,IN2,NOUT1/1,2,6/
      DATA CVEL/299792.50/    !Vel. of light, km/sec
  100 FORMAT (A36)
C*********************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (UNIT=9,     FILE ='C:\SPSYN\SPPRC1.ILS',  STATUS = 'OLD')
      READ ( 9, 100)    (FL1(J),J=1,3)
      OPEN (UNIT =  IN1, FILE = FL1(1),  STATUS = 'OLD')
      OPEN (UNIT =  IN2, FILE = FL1(2),  STATUS = 'OLD')
      OPEN (UNIT =NOUT1, FILE = FL1(3),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '*SPPRC1'
      LBLERR=0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) GO TO 20
	CALL DREAD (RF,'E12.4')     !Reference light level
      CALL DREAD (SYSVL, 'F12.3')	!System radial vel.
      CALL DREAD (TLT, 'F12.4')	!Third light in system
      CALL DREAD (DLM, 'F12.5')	!Zero pt. wavelength corr.
      CALL DREAD (WLCORR, 'F12.5')!Wavelength scale factor corr.,A
      CALL DREAD (WL1,'F12.4')	!Short wavelength boundary
	CALL DREAD (FLX1,'E12.4')  !Current flux at short wvlgth. bound.
      CALL DREAD (WL2,'F12.4')	!Second wavelength
	CALL DREAD (FLX2,'E12.4')  !Current flux at second wavelength
      CALL DREAD (WL3,'F12.4')	!Third wavelength
	CALL DREAD (FLX3,'E12.4')  !Current flux at third wavelength
      CALL DREAD (WL4,'F12.4')	!Fourth avelength
	CALL DREAD (FLX4,'E12.4')  !Current flux at fourth wavelength
      CALL DREAD (WL5,'F12.4')	!Fifth wavelength
	CALL DREAD (FLX5,'E12.4')  !Current flux at fifth wavelength
      CALL DREAD (WL6,'F12.4')	!Sixth wavelength
	CALL DREAD (FLX6,'E12.4')  !Current flux at sixth wavelength
      CALL DREAD (WL7,'F12.4')	!Seventh wavelength
	CALL DREAD (FLX7,'E12.4')  !Current flux at seventh wavelength
      CALL DREAD (WL8,'F12.4')	!Eighth wavelength
	CALL DREAD (FLX8,'E12.4')  !Current flux at eighth wavelength
      CALL DREAD (WL9,'F12.4')	!Ninth wavelength
	CALL DREAD (FLX9,'E12.4')  !Current flux at ninth wavelength
      CALL DREAD (WL10,'F12.4')	!Tenth wavelength
	CALL DREAD (FLX10,'E12.4')  !Current flux at tenth wavelength
      CALL DREAD (WL11,'F12.4')	!Eleventh wavelength
	CALL DREAD (FLX11,'E12.4')  !Current flux at eleventh wavelength
      CALL DREAD (WL12,'F12.4')	!Long wavelength boundary
	CALL DREAD (FLX12,'E12.4')  !Current flux at long wavlgth. bound
      CALL IREAD (NDEG)
C     The parameters WLSLM and WLLLM set absolute boundaries for the 
C     theoretical spectra, chosen so that the theor. spectra will fit
C     the wavelength range of the observed spectra.
C
C
C     Read format of observed data
	CALL CREAD (FMT1)
      DO J=1,100000
C     Wavelength and flux, system, theor. spectrum
         READ (IN2,FMT1,END=2,ERR=20) WLA(J),FLA(J)
		FLA(J)=FLA(J)/RF
	END DO
C     Set number of wavelengths in spectrum between limits
    2 N=J-1
C     
	FXLLM=FLX12
	FXSLM=FLX1
	WLLLM=WL12
	WLSLM=WL1
	F(1)=FLX1
	F(2)=FLX2
	F(3)=FLX3
	F(4)=FLX4
	F(5)=FLX5
	F(6)=FLX6
	F(7)=FLX7
	F(8)=FLX8
	F(9)=FLX9
	F(10)=FLX10
	F(11)=FLX11
	F(12)=FLX12
	WL(1)=WL1
	WL(2)=WL2
	WL(3)=WL3
	WL(4)=WL4
	WL(5)=WL5
	WL(6)=WL6
	WL(7)=WL7
	WL(8)=WL8
	WL(9)=WL9
	WL(10)=WL10
	WL(11)=WL11
	WL(12)=WL12
	A1=1.D0
	A2=1.D0
	A3=1.D0
	A4=1.D0
C     Apply correction to calculated spectrum
C     Also correct for Doppler motion of system
      DLWL=1.D0+SYSVL/CVEL
      DO J=1,N
C     Initialize arrays for rectified flux data
         IF (WLA(J).NE.0) THEN
		IF ((WLA(J).GE.WL1).AND.(WLA(J).LE.WL12)) THEN
C     NOTE!!! The following SRT has been modified to allow arrays of
C     length 12 items.
			CALL LAGIN(WLA(J),FX,WL,F,NDEG,12,M,1.D0)
			IF (M.LT.3) THEN
				CALL PMDSTOP
			END IF
			IF (FX.GT.0.D0) THEN
				FLA(J)=FLA(J)/FX
			END IF
		END IF
C     Correct for third light
            FLA(J)=(FLA(J)+TLT*FLA(J))/(1.D0+TLT*FLA(J))
C     Apply wavelength correction
            WLA(J)=WLA(J)+WLCORR/(WLLLM-WLSLM)*(WLA(J)-WLSLM)+DLM
         END IF
C     Apply Doppler correction for system radial velocity
         WLA(J)=WLA(J)*DLWL
      END DO
      DO J=1,N
C     Restore flux values to input units, for use in following 
C     processing
         WRITE (NOUT1,FMT1) WLA(J),FLA(J)*RF
      END DO      
C*********************************************************************
   20 CLOSE (UNIT = IN1,    STATUS = 'KEEP')
      CLOSE (UNIT = IN2,    STATUS = 'KEEP')
      CLOSE (UNIT = NOUT1,  STATUS = 'KEEP')
C*********************************************************************
      STOP
      END
