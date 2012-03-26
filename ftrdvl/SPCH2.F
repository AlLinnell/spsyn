      SUBROUTINE SPCH2(OFL,OWL,NOWL,FL,WL,NWL,CFL,DFL,NBIN,LBIN,SDV,
     $VMN,DLT,DFL2,CH2)
C     SRT to calculate chi squared for fit of synthetic spectrum to
C     observed spectrum.
C     CH2 is value of chi squared
C     Other call parameters defined in heading of SRT DFSP.
      DIMENSION OFL(NOWL),OWL(NOWL),FL(NWL),WL(NWL),CFL(NOWL),DFL(NOWL)
     $,LBIN(NBIN),DFL2(NOWL)
      CALL DFSP(OFL,OWL,NOWL,FL,WL,NWL,CFL,DFL,NBIN,LBIN,SDV,VMN,DLT,
     $DFL2)
      CH2=0.0
      START=VMN-(NBIN/2)*SDV*DLT
      DO 10 J=1,NBIN
         C1=(J-1)*SDV*DLT+START
         C2=J*SDV*DLT+START
         GC1= GAMMP(0.5,C1*C1)
         GC2= GAMMP(0.5,C2*C2)
         EJ=NOWL*ABS(GC1-GC2)
         VNUM=LBIN(J)
         IF ((EJ.EQ.0.0).OR.(VNUM.EQ.0.0)) GO TO 10
         CH2=CH2+((VNUM-EJ)**2/EJ)
   10 CONTINUE
      RETURN
      END
