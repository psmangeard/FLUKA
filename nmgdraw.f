
*$ CREATE MGDRAW.FOR
*COPY MGDRAW
*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2005      by        Alfredo Ferrari           *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     MaGnetic field trajectory DRAWing: actually this entry manages   *
*                                        all trajectory dumping for    *
*                                        drawing                       *
*                                                                      *
*     Created by                     Alfredo Ferrari                   *
*                                    INFN - Milan                      *
*     last change  09-jul-05  by     Alfredo Ferrari                   *
*                                    INFN - Milan                      *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(CASLIM)'
      INCLUDE '(COMPUT)'
      INCLUDE '(SOURCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(GENSTK)'
      INCLUDE '(MGDDCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(SUMCOU)'
      INCLUDE '(TRACKR)'
ccccccc Begin Pierre-Simon 10/23/2016
      INCLUDE '(SOUEVT)'
ccccccc End Pierre-Simon 10/23/2016
      COMMON /LOCALDEPOSIT/xsumde1

*
      CHARACTER*20 FILNAM
      LOGICAL LFCOPE
      SAVE LFCOPE
      DATA LFCOPE / .FALSE. /
      DATA RdErth/637814000./
*
*----------------------------------------------------------------------*
*                                                                      *
*     Icode = 1: call from Kaskad                                      *
*     Icode = 2: call from Emfsco                                      *
*     Icode = 3: call from Kasneu                                      *
*     Icode = 4: call from Kashea                                      *
*     Icode = 5: call from Kasoph                                      *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
c      IF ( .NOT. LFCOPE ) THEN
c         LFCOPE = .TRUE.
c         IF ( KOMPUT .EQ. 2 ) THEN
c            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
c         ELSE
c            FILNAM = CFDRAW
c         END IF
c         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
c     &          'UNFORMATTED' )
c      END IF
c      WRITE (IODRAW) NTRACK, MTRACK, JTRACK, SNGL (ETRACK),
c     &               SNGL (WTRACK)
c      WRITE (IODRAW) ( SNGL (XTRACK (I)), SNGL (YTRACK (I)),
c     &                 SNGL (ZTRACK (I)), I = 0, NTRACK ),
c     &               ( SNGL (DTRACK (I)), I = 1,MTRACK ),
c     &                 SNGL (CTRACK)
d
d      do i=0,NTRACK-1
d        RADXY0 = SQRT(XTRACK(i)*XTRACK(i)+ YTRACK(i)*YTRACK(i))
d        RADXY1 = SQRT(XTRACK(i+1)*XTRACK(i+1)
d     &              + YTRACK(i+1)*YTRACK(i+1))
d        IF(RADXY0.LE.40.0 
d     & .AND. ZTRACK(I).GE.30.0.AND. ZTRACK(I).LE.31.0
d     & .AND. RADXY1.LE.40.0 
d     & .AND. ZTRACK(I+1).GE.30.0 .AND. ZTRACK(I+1).LE.31.0)  THEN
c         WRITE(99,*) JTRACK, ichrge(jtrack), 
c     & XTRACK(i), YTRACK(i),ZTRACK(I),DTRACK(I+1)
d       if(jtrack.gt.0) then 
d       if(ichrge(jtrack).ne.0)xsumde1=xsumde1+DTRACK(I+1)
                  
d        ENDIF
d        ENDIF
d      enddo

      RETURN
*
*======================================================================*
*                                                                      *
*     Boundary-(X)crossing DRAWing:                                    *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             19: boundary crossing                                    *
*     Icode = 2x: call from Emfsco                                     *
*             29: boundary crossing                                    *
*     Icode = 3x: call from Kasneu                                     *
*             39: boundary crossing                                    *
*     Icode = 4x: call from Kashea                                     *
*             49: boundary crossing                                    *
*     Icode = 5x: call from Kasoph                                     *
*             59: boundary crossing                                    *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY BXDRAW ( ICODE, MREG, NEWREG, XSCO, YSCO, ZSCO )

d     &      (MREG.eq.12.and.NEWREG.eq.9).or.
d     &      (MREG.eq.9.and.NEWREG.eq.12).or.
d     &      (MREG.eq.9.and.NEWREG.eq.2).or.
d     &      (MREG.eq.2.and.NEWREG.eq.9).or. 
d     &      (MREG.eq.9.and.NEWREG.eq.3).or.
d     &      (MREG.eq.3.and.NEWREG.eq.9).or.
d     &      (MREG.eq.9.and.NEWREG.eq.4).or.
d     &      (MREG.eq.4.and.NEWREG.eq.9).or.
d     &      (MREG.eq.9.and.NEWREG.eq.6).or.
d     &      (MREG.eq.6.and.NEWREG.eq.9).or.
d     &      (MREG.eq.9.and.NEWREG.eq.7).or.
d     &      (MREG.eq.7.and.NEWREG.eq.9).or.
d     &      (MREG.eq.9.and.NEWREG.eq.8).or.
d     &      (MREG.eq.8.and.NEWREG.eq.9)) then
d           WRITE(99,100) mreg,NEWREG,jtrack,etrack-am(jtrack),
d     &     XSCO,YSCO,ZSCO,atrack

ccccccc Begin Pierre-Simon 10/23/2016
c           WRITE(99,100) ncase,mreg,NEWREG,jtrack,
c     &     etrack-am(jtrack),
c     &     XSCO,YSCO,ZSCO,atrack
           WRITE(99,100) ncase,mreg,NEWREG,jtrack,
     &     etrack-am(jtrack),
     &     XSCO,YSCO,ZSCO,atrack,
     &     CXTRCK,CYTRCK,CZTRCK,
     &     IJSOEV(1),TKSOEV(1),
     &     TXSOEV(1),TYSOEV(1),TZSOEV(1),
     &     XSOEVT(1),YSOEVT(1),ZSOEVT(1)
ccccccc End Pierre-Simon 10/23/2016


          IF(
     &      (MREG.eq.19.and.NEWREG.eq.1).or.
     &      (MREG.eq.1.and.NEWREG.eq.19).or.
     &      (MREG.eq.19.and.NEWREG.eq.6).or.
     &      (MREG.eq.6.and.NEWREG.eq.19).or.
     &      (MREG.eq.19.and.NEWREG.eq.7).or.
     &      (MREG.eq.7.and.NEWREG.eq.19).or.
     &      (MREG.eq.19.and.NEWREG.eq.11).or.
     &      (MREG.eq.11.and.NEWREG.eq.19).or.
     &      (MREG.eq.19.and.NEWREG.eq.12).or.
     &      (MREG.eq.12.and.NEWREG.eq.19).or.
     &      (MREG.eq.10.and.NEWREG.eq.13).or.
     &      (MREG.eq.13.and.NEWREG.eq.10).or.
     &      (MREG.eq.19.and.NEWREG.eq.14).or.
     &      (MREG.eq.14.and.NEWREG.eq.19).or.
     &      (MREG.eq.19.and.NEWREG.eq.15).or.
     &      (MREG.eq.15.and.NEWREG.eq.19).or.
     &      (MREG.eq.19.and.NEWREG.eq.16).or.
     &      (MREG.eq.16.and.NEWREG.eq.19).or.
     &      (MREG.eq.19.and.NEWREG.eq.17).or.
     &      (MREG.eq.17.and.NEWREG.eq.19).or.
     &      (MREG.eq.19.and.NEWREG.eq.18).or.
     &      (MREG.eq.18.and.NEWREG.eq.19)
     &        ) THEN

c     &      (MREG.eq.11.and.NEWREG.eq.12)
c          IF(
c     &      (MREG.eq.12.and.NEWREG.le.11).or.
c     &      (MREG.le.11.and.NEWREG.eq.12)
c     &        ) THEN
         if(jtrack.ne.7) then
d           WRITE(99,100) ncase,mreg,NEWREG,jtrack,
d     &     etrack-am(jtrack),
d     &     XSCO,YSCO,ZSCO,atrack
c           WRITE(99,*) mreg,NEWREG,jtrack,etrack-am(jtrack),
         endif
DD        IGLPRTCNTXX=IGLPRTCNTXX+1
DD        ireg1xx(IGLPRTCNTXX)=mreg
DD        ireg2xx(IGLPRTCNTXX)=NEWREG
DD        JTRACKSTRXX(IGLPRTCNTXX)=jtrack
DD        ENGYXX(IGLPRTCNTXX)=etrack-am(jtrack)
DD        AGETXX(IGLPRTCNTXX)=atrack
DD        XTRCKXX(IGLPRTCNTXX)=XSCO
DD        YTRCKXX(IGLPRTCNTXX)=YSCO
DD        ZTRCKXX(IGLPRTCNTXX)=ZSCO-RdErth
DD        DXTRCKXX(IGLPRTCNTXX)=CXTRCK
DD        DYTRCKXX(IGLPRTCNTXX)=CYTRCK
DD        DZTRCKXX(IGLPRTCNTXX)=CZTRCK

dd            else
DD           iiitrack=ichrge(jtrack)*100+ int((am(jtrack)/AMUGEV)+0.5)
c           WRITE(99,100) mreg,NEWREG,iiitrack,etrack-am(jtrack),CXTRCK,
c     &     CYTRCK,CZTRCK,XSCO,YSCO,ZSCO-RdErth,Wscrng,atrack

DD        IGLPRTCNTXX=IGLPRTCNTXX+1
DD        JTRACKSTRXX(IGLPRTCNTXX)=iiitrack
DD        ENGYXX(IGLPRTCNTXX)=etrack-am(jtrack)
DD        AGETXX(IGLPRTCNTXX)=atrack
DD        XTRCKXX(IGLPRTCNTXX)=XSCO
DD        YTRCKXX(IGLPRTCNTXX)=YSCO
DD        ZTRCKXX(IGLPRTCNTXX)=ZSCO-RdErth
DD        DXTRCKXX(IGLPRTCNTXX)=CXTRCK
DD        DYTRCKXX(IGLPRTCNTXX)=CYTRCK
DD        DZTRCKXX(IGLPRTCNTXX)=CZTRCK
           
            endif
c           endif
ddd           endif
      RETURN
ccccccc Begin Pierre-Simon 10/23/2016
c 100   FORMAT (1X,I7,2(1X,I3),1X,I4,1X,E15.8,3(1X,E13.6),
c     &1X,E13.6)
 100   FORMAT (1X,I7,2(1X,I3),1X,I4,1X,E15.8,3(1X,E13.6),
     & 1X,E13.6,     
     & 3(1x,e12.6),
     & 1x,i4,1x,e12.6,
     & 3(1x,e12.6),
     & 3(1x,e12.6))    
ccccccc End Pierre-Simon 10/23/2016

d100   FORMAT ('S',2(1X,I3),1X,I4,4(1X,E12.5),3(1X,E13.6),
d     &1X,E11.6,1X,E13.6)
*
*======================================================================*
*                                                                      *
*     Event End DRAWing:                                               *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY EEDRAW ( ICODE )
c        write (99,*) ICODE, MREG, RULL, XSCO, YSCO, ZSCO
      RETURN
*
*======================================================================*
*                                                                      *
*     ENergy deposition DRAWing:                                       *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             10: elastic interaction recoil                           *
*             11: inelastic interaction recoil                         *
*             12: stopping particle                                    *
*             13: pseudo-neutron deposition                            *
*             14: escape                                               *
*             15: time kill                                            *
*     Icode = 2x: call from Emfsco                                     *
*             20: local energy deposition (i.e. photoelectric)         *
*             21: below threshold, iarg=1                              *
*             22: below threshold, iarg=2                              *
*             23: escape                                               *
*             24: time kill                                            *
*     Icode = 3x: call from Kasneu                                     *
*             30: target recoil                                        *
*             31: below threshold                                      *
*             32: escape                                               *
*             33: time kill                                            *
*     Icode = 4x: call from Kashea                                     *
*             40: escape                                               *
*             41: time kill                                            *
*             42: delta ray stack overflow                             *
*     Icode = 5x: call from Kasoph                                     *
*             50: optical photon absorption                            *
*             51: escape                                               *
*             52: time kill                                            *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY ENDRAW ( ICODE, MREG, RULL, XSCO, YSCO, ZSCO )
C        write (99,*) ICODE, MREG, RULL, XSCO, YSCO, ZSCO
c      IF ( .NOT. LFCOPE ) THEN
c         LFCOPE = .TRUE.
c         IF ( KOMPUT .EQ. 2 ) THEN
c            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
c         ELSE
c            FILNAM = CFDRAW
c         END IF
c         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
c     &          'UNFORMATTED' )
c      END IF
c      WRITE (IODRAW)  0, ICODE, JTRACK, SNGL (ETRACK), SNGL (WTRACK)
c      WRITE (IODRAW)  SNGL (XSCO), SNGL (YSCO), SNGL (ZSCO), SNGL (RULL)
      RETURN
*
*======================================================================*
*                                                                      *
*     SOurce particle DRAWing:                                         *
*                                                                      *
*======================================================================*
*
      ENTRY SODRAW
c        write (99,*) ICODE, MREG, RULL, XSCO, YSCO, ZSCO
c      IF ( .NOT. LFCOPE ) THEN
c         LFCOPE = .TRUE.
c         IF ( KOMPUT .EQ. 2 ) THEN
c            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
c         ELSE
c            FILNAM = CFDRAW
c         END IF
c         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
c     &          'UNFORMATTED' )
c      END IF
c      WRITE (IODRAW) -NCASE, NPFLKA, NSTMAX, SNGL (TKESUM),
c     &                SNGL (WEIPRI)
*  +-------------------------------------------------------------------*
*  |  Patch for heavy ions: it works only for 1 source particle on
*  |  the stack for the time being
c      IF ( ABS (ILOFLK (NPFLKA)) .GE. 10000 ) THEN
c         IONID = ILOFLK (NPFLKA)
c         CALL DCDION ( IONID )
c         WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-IONID)),
c     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
c     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
c     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
c     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |
c      ELSE
c         WRITE (IODRAW) ( ILOFLK(I), SNGL (TKEFLK(I)+AM(ILOFLK(I))),
c     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
c     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
c     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
c     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
c      END IF
*  |
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     USer dependent DRAWing:                                          *
*                                                                      *
*     Icode = 10x: call from Kaskad                                    *
*             100: elastic   interaction secondaries                   *
*             101: inelastic interaction secondaries                   *
*             102: particle decay  secondaries                         *
*             103: delta ray  generation secondaries                   *
*             104: pair production secondaries                         *
*             105: bremsstrahlung  secondaries                         *
*     Icode = 20x: call from Emfsco                                    *
*             208: bremsstrahlung secondaries                          *
*             210: Moller secondaries                                  *
*             212: Bhabha secondaries                                  *
*             214: in-flight annihilation secondaries                  *
*             215: annihilation at rest   secondaries                  *
*             217: pair production        secondaries                  *
*             219: Compton scattering     secondaries                  *
*             221: photoelectric          secondaries                  *
*             225: Rayleigh scattering    secondaries                  *
*     Icode = 30x: call from Kasneu                                    *
*             300: interaction secondaries                             *
*     Icode = 40x: call from Kashea                                    *
*             400: delta ray  generation secondaries                   *
*  For all interactions secondaries are put on GENSTK common (kp=1,np) *
*  but for KASHEA delta ray generation where only the secondary elec-  *
*  tron is present and stacked on FLKSTK common for kp=lstack          *
*                                                                      *
*======================================================================*
*
      ENTRY USDRAW ( ICODE, MREG, XSCO, YSCO, ZSCO )
c      IF ( .NOT. LFCOPE ) THEN
c         LFCOPE = .TRUE.
c         IF ( KOMPUT .EQ. 2 ) THEN
c            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
c         ELSE
c            FILNAM = CFDRAW
c         END IF
c         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
c     &          'UNFORMATTED' )
c      END IF
* No output by default:
      RETURN
*=== End of subrutine Mgdraw ==========================================*
      END

