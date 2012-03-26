      SUBROUTINE DFSP(OFL,OWL,NOWL,FL,WL,NWL,CFL,DFL,NBIN,LBIN,SDV,
     $VMN,DLT,DFL2)
C     SRT to produce array of differences between observed spectrum
C     and synthetic spectrum. Also calculates mean and std. deviation
C     of differences. Determines distribution of differences among
C     NBIN bins.
C     OFL=array of observed flux values, of length NOWL
C     OWL=corresponding array of observed wavelengths
C     NOWL=number of entries in arrays OFL and OWL
C     FL=array of calculated flux values, of length NWL
C     WL=corresponding array of assigned wavelengths
C     NWL=number of entries in arrays FL and WL
C     CFL=array of calculated flux values, interpolated at wavelengths
C        OWL
C     DFL=array of differences OFL-CFL, of length NOWL
C     NBIN=number of bins for distribution count of DFL
C     LBIN=array of length NBIN containing distribution counts of DFL
C     SDV=std. deviation of array DFL
C     VMN=mean value of array DFL
C     DLT=width of each bin, in units of std. deviation
	DOUBLE PRECISION SDV2
      DIMENSION OFL(NOWL),OWL(NOWL),FL(NWL),WL(NWL),CFL(NOWL),DFL(NOWL),
     $LBIN(NBIN),DFL2(NOWL)
      DATA LWL/1/
	SDV2=0.D0
      SDV=0.0
      VMN=0.0
C     Make sure NBIN is an even number
      ITP=MOD(NBIN,2)
      IF (ITP.NE.0) NBIN=NBIN-1
      DO J=1,NBIN
         LBIN(J)=0
      END DO
      DO 1 J=1,NOWL
         CALL TPWL(FL,WL,OWL(J),LWL,NWL,CFL(J))
         IF (CFL(J).EQ.0.0) GO TO 1
         DFL(J)=OFL(J)-CFL(J)
         DFL2(J)=OFL(J)-CFL(J)
         VMN=VMN+DFL(J)
    1 CONTINUE
      VMN=VMN/NOWL
	DO J=1,NOWL
		SDV2=SDV2+(DFL(J)-VMN)**2
	END DO
      SDV2=DSQRT(SDV2/(NOWL-1))
	SDV=SDV2
      CALL SORT(NOWL,DFL2)
C     The entries in DFL2 now are sorted into increasing order
      RANGE1=ABS(DFL2(NOWL)-VMN)
      RANGE2=ABS(VMN-DFL2(1))
C     Make sure bins are centered on VMN
      IF (RANGE2.GE.RANGE1) RANGE1=RANGE2
      IF (RANGE2.LT.RANGE1) RANGE2=RANGE1
      RANGE=RANGE1+RANGE2
      DLT=RANGE/NBIN
      K=1
      START=VMN-RANGE/2.0
      DO J=1,NOWL
         C1=(K-1)*DLT+START
         C2=K*DLT+START
         IF ((DFL2(J).GE.C1).AND.(DFL2(J).LT.C2)) LBIN(K)=LBIN(K)+1
         IF (DFL2(J).GE.C2) THEN
            K=K+1
            LBIN(K)=LBIN(K)+1
         END IF
      END DO
	ISM=0
	DO J=1,K
		ISM=ISM+LBIN(J)
	END DO
      RETURN
      END
