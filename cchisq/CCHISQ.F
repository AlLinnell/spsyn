      Program CCHISQ
C     Program to calculate values of chi^2 for a range of values
C     of mass transfer rates and white dwarf Teff values.
      DATA NMT/9/
	DATA NTEMP/17/
	INTEGER*4 RESULT
	CHARACTER*72 adracp,adrpgc,progname,adrp6,adrmod
	DATA IN1,IN2,IN3,IN4/5,6,7,8/
	DATA IST/6/
	DATA IND/9/
	DATA JST/1/
	DATA JND/17/
  100 FORMAT (A72)
	OPEN (UNIT=IN1, FILE='d:\cvsystems\rwsex\adrpgc.dat', 
     $STATUS='OLD')
	OPEN (UNIT=IN2, FILE='d:\cvsystems\rwsex\adracpgc.dat', 
     $STATUS='OLD')
	OPEN (UNIT=IN3, FILE='d:\cvsystems\rwsex\acp6adr.dat',
     $STATUS='OLD')
	OPEN (UNIT=IN4, FILE='d:\cvsystems\rwsex\modstr.dat',
     $STATUS = 'OLD')
	IF (IND.GT.NMT) GO TO 99
	IF (JND.GT.NTEMP) GO TO 99
      DO i=1, nmt
		READ(IN2,100) adracp
			progname=adracp
			result=systemqq(progname)
			IF (I.GT.IND) GO TO 99
			IF (I.LT.IST) GO TO 10
			progname='c:\lgtsyn\acpgc\debug\acpgc.exe'
			result=systemqq(progname)
   10		DO j=1,ntemp
			READ(IN1,100) adrpgc
			progname=adrpgc
			result=systemqq(progname)
			IF (I.LT.IST) GO TO 1
			IF (J.LT.JST) GO TO 1
			IF (J.GT.JND) GO TO 1
			progname='c:\lgtsyn\pgc\debug\pgc.exe'
			result=systemqq(progname)
			progname='c:\lgtsyn\pgd\debug\pgd.exe'
			result=systemqq(progname)
			progname='c:\lgtsyn\acpgd\debug\acpgd.exe'
			result=systemqq(progname)
			progname='c:\lgtsyn\rdvel2\debug\rdvel2.exe'
			result=systemqq(progname)
    1			CONTINUE
			READ(IN3,100) adrp6
			progname=adrp6
			result=systemqq(progname)
			IF (I.LT.IST) GO TO 2
			IF (J.LT.JST) GO TO 2
			IF (J.GT.JND) GO TO 2
			progname='c:\hubeny\acpgf6\debug\acpgf6.exe'
			result=systemqq(progname)
			progname='c:\lgtsyn\datconv\debug\datconv.exe'
			result=systemqq(progname)
    2         CONTINUE
			READ(IN4,100) adrmod
			IF (I.LT.IST) GO TO 3
			IF (J.LT.JST) GO TO 3
			IF (J.GT.JND) GO TO 3
			progname=adrmod
			result=systemqq(progname)
    3			CONTINUE
			READ(IN4,100) adrmod
			IF (I.LT.IST) GO TO 4
			IF (J.LT.JST) GO TO 4
			IF (J.GT.JND) GO TO 4
			progname=adrmod
			result=systemqq(progname)
    4			CONTINUE
			READ(IN4,100) adrmod
			IF (I.LT.IST) GO TO 5
			IF (J.LT.JST) GO TO 5
			IF (J.GT.JND) GO TO 5
			progname=adrmod
			result=systemqq(progname)
    5			CONTINUE
			READ(IN4,100) adrmod
			IF (I.LT.IST) GO TO 19
			IF (J.LT.JST) GO TO 19
			IF (J.GT.JND) GO TO 19
			progname=adrmod
			result=systemqq(progname)
   19 	END DO
	REWIND IN1
   20 END DO
   99 STOP
	END Program CCHISQ
