      SUBROUTINE SPPRC(N1,N2,KKK,KEY)
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C   
      COMMON /CA/OFL(500000),OWL(500000),FL(500000),WL(500000),
     $CFL(500000),WLB(500000),FLB(500000)
C     N1 = number of data values in array OWL
C     Find second spectrum wavelength ordinal position closest to
C     given entry in first spectrum.
C     Both spectra must have wavelengths in ascending order.
      DO 2 K=1,N1
         WLB(K)=OWL(K)
    2 END DO
C     WLB() now contains desired wavelengths. Start with first wavelength
C     and locate bracketing or coincident wavelength points in the
C     second spectrum.
C     KK is the counter for the second spectrum; KKK is the counter for the
C     first spectrum (= counter for output spectrum).
      KK=1
      DO KKK=1,N1-1
      WLV=WLB(KKK)
C     KSV is a pointer to the current starting point for a scan of the
C     second spectrum wavelengths.
      KSV=KK
C     Scan from current starting point to end of second spectrum.
      DO KCT=KSV,N2-1
C     WLS is current scan wavelength in second spectrum.
      WLS=WL(KCT)
C     KCT is reference index within the second spectrum
C     Isolate case when wavelengths coincide. Wavelengths in Angstroms.
	IF (DABS(WLS-WLV).LT.1.D-4) THEN
C     Set flux in interpolated spectrum.
                CFL(KKK)=FL(KCT)
C     Set flux sum.
		FLB(KKK)=OFL(KKK)+FL(KCT)
                GO TO 10
	END IF
C     Isolate case when wavelength in second spectrum is
C     less than the designated first spectrum wavelength
      IF ((WLS.LT.WLV).AND.(WL(KCT+1).GE.WLV)) THEN
C     Have located bracketing wavelengths and second specrum wavelength
C     WLS is less than WLV.
                        DLWL=WL(KCT+1)-WLS
                        DLFX=FL(KCT+1)-FL(KCT)
                        DLWL1=WLV-WLS
                        CFL(KKK)=FL(KCT)+DLWL1/DLWL*DLFX
			FLB(KKK)=OFL(KKK)+CFL(KKK)
                        AA=0.D0
                        GO TO 10
	END IF

C     Case when wavelength in second spectrum is greater
C     than designated first spectrum wavelength
	IF ((WLS.GT.WLV).AND.(WL(KCT-1).LE.WLV)) THEN
                        DLWL=WLS-WL(KCT-1)
                        DLFX=FL(KCT)-FL(KCT-1)
                        DLWL1=WLS-WLV
                        CFL(KKK)=FL(KCT-1)+DLWL1/DLWL*DLFX
			FLB(KKK)=OFL(KKK)+CFL(KKK)
                        AA=0.D0
                        GO TO 10
	END IF
       END DO
C     Exit when point has been determined
   10 CONTINUE
C     Reset starting point for scan of second spectrum.
      KSV=KCT
C     Increment point on first spectrum.
      END DO 
       RETURN
      END
