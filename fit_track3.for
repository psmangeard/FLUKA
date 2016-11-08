            implicit real*8 (A-H,O-Z)
            external func
            dimension x(5000),y(5000),z(5000)
c            dimension xxd(50),zxd(50),yyd(50),zyd(50)
            common /trackdat/ xxd(50),zxd(50),yyd(50),zyd(50)
            dimension xb(2000),yb(2000),zb(2000)
            common /trackdatind/ixx,iyy
            common /dstep/hhh
            dimension par(5),grad(5) 
            character*132 line

            open(57,file='for57n.top',status='new')
c            open(55,file='/home/clem/myFlukaAreadec2015/nicenice001_fort.99',
            open(55,file='/home/sarah/AESOPLITE/FLUKA/aesoplite001_fort.99',
     &status='old')
            i1old=1
            icn=0
            itrig1=0
            itrig3=0
            itrig4=0


c* loop over particle records
            do k=1,10000000
             read(55,'(A132)') line
             read(line,*) i1,i2,i3,i4,f1,f2,f3,f4,f5
c new primary started, process prior event
c otherwise skip over this part
 200         continue       
c* IF - C
             if(i1.eq.i1old) then
c* IF - B
              if(i4.ne.7) then
c              if(i4.eq.11) then
          mreg=i2
          newreg=i3
c* IF - A
          IF(
     &      (MREG.eq.19.and.NEWREG.eq.1).or.
     &      (MREG.eq.19.and.NEWREG.eq.6).or.
     &      (MREG.eq.19.and.NEWREG.eq.7).or.
     &      (MREG.eq.19.and.NEWREG.eq.11).or.
     &      (MREG.eq.19.and.NEWREG.eq.12).or.
     &      (MREG.eq.10.and.NEWREG.eq.13).or.
     &      (MREG.eq.19.and.NEWREG.eq.14).or.
     &      (MREG.eq.19.and.NEWREG.eq.15).or.
     &      (MREG.eq.19.and.NEWREG.eq.16).or.
     &      (MREG.eq.19.and.NEWREG.eq.17).or.
     &      (MREG.eq.19.and.NEWREG.eq.18)
     &        ) THEN

           if(MREG.eq.19.and.NEWREG.eq.1.and.i4.ne.7) Itrig1=1
           if(MREG.eq.19.and.NEWREG.eq.6.and.i4.ne.7) Itrig3=1
           if(MREG.eq.19.and.NEWREG.eq.18.and.i4.ne.7) Itrig4=1

               icn=icn+1  
               x(icn)=f2  
               y(icn)=f3  
               z(icn)=f4  

              endif   
c* ENDIF - A
              endif   
c* ENDIF - B
              endif   
c* ENDIF - C


c *process prior event
             if(i1.ne.i1old) then
               i1old=i1

c* X-Z plane
               ixx=0
               do kk=1,icn 
                 if((z(kk).gt.-2.0.and. z(kk).lt.-1.0).or. 
     &             (z(kk).gt.-18.0.and.z(kk).lt.-17.0).or.
     &             (z(kk).lt.-21.0.and.z(kk).gt.-22.0)) then

                     ixx=ixx+1
                     xstrip=0.0228d0*float(int(x(kk)/0.0228d0))
                     xxd(ixx)=xstrip
c                     xxd(ixx)=x(kk)

                     zxd(ixx)=z(kk)

                 endif              
               enddo 

c  RPP   12      -9.0     +9.0      -9.0      +9.0      -1.5    -1.4996
c  RPP   13      -9.0     +9.0      -9.0      +9.0      -3.5    -3.4996 B
c  RPP   14      -9.0     +9.0      -9.0      +9.0      -7.5    -7.4996 B
c  RPP   15      -9.0     +9.0      -9.0      +9.0     -11.5   -11.4996 B
c  RPP   16      -9.0     +9.0      -9.0      +9.0     -13.5   -13.4996
c  RPP   17      -9.0     +9.0      -9.0      +9.0     -15.5   -15.4996 B
c  RPP   18      -9.0     +9.0      -9.0      +9.0     -17.5   -17.4996



c  RPP   12      -9.0     +9.0      -9.0      +9.0      -1.5    -1.4996
c  RPP   13      -9.0     +9.0      -9.0      +9.0      -3.5    -3.4996 B
c  RPP   14      -9.0     +9.0      -9.0      +9.0      -9.5    -9.4996 B
c  RPP   15      -9.0     +9.0      -9.0      +9.0     -15.5   -15.4996 B
c  RPP   16      -9.0     +9.0      -9.0      +9.0     -17.5   -17.4996
c  RPP   17      -9.0     +9.0      -9.0      +9.0     -19.5   -19.4996 B
c  RPP   18      -9.0     +9.0      -9.0      +9.0     -21.5   -21.4996

c* Y-Z plane
               iyy=0
               do kk=1,icn

                 if(
     &             (z(kk).gt.-4.0.and.z(kk).lt.-3.0).or.
     &             (z(kk).gt.-10.0.and.z(kk).lt.-9.0).or.
     &             (z(kk).gt.-16.0.and.z(kk).lt.-15.0).or.
     &             (z(kk).gt.-20.0.and.z(kk).lt.-19.0)) then
C     &             (z(kk).lt.-18.0)) then
                     iyy=iyy+1
                       ystrip=0.0228d0*float(int(y(kk)/0.0228d0))
c                       print *,ystrip,y(kk)
                     yyd(iyy)=ystrip
c                     yyd(iyy)=y(kk)
                     zyd(iyy)=z(kk)
                 endif              
               enddo 
               icn=0

cc* we have data


           print *,ixx,iyy

           if(iyy.le.2.or.ixx.le.1) then

               write(57,*) 'new frame'
               write(57,*) 'SET WINDOW X 1.25 6'
               write(57,*) 'SET WINDOW Y 1 9.3125'
               write(57,*) 'SET LIMIT X -10  10'
               write(57,*) 'SET LIMIT Y -17.5  17.5'
               write(57,*) 'SET SYMBOL 9O'
               write(57,*) 'SET ORDER X Y'
               do mm=1,ixx
                 write(57,*) xxd(mm),zxd(mm)+9.5
               enddo 
               write(57,*) 'plot'

               write(57,*) 'SET WINDOW Y 1 9.3125'
               write(57,*) 'SET WINDOW X 8 12.75'
               write(57,*) 'SET LIMIT X -10  10'
               write(57,*) 'SET LIMIT Y -17.5  17.5'
               write(57,*) 'SET SYMBOL 9O'
               write(57,*) 'SET ORDER X Y'
               do mm=1,iyy
                 write(57,*) yyd(mm),zyd(mm)+9.5
               enddo 
               write(57,*) 'plot'


           endif

           if(iyy.gt.2.and.ixx.gt.1.and.itrig3.eq.1.and.itrig4.eq.1) then
            itrig1=0
            itrig3=0
            itrig4=0

c           if(iyy.eq.3.and.ixx.eq.2) then
            n1=1
            n2=2
            n3=3
            if(zyd(n1).eq.zyd(n3)) go to 94
            if(zyd(n1).eq.zyd(n2)) go to 94

            aa = (yyd(n1)-yyd(n3))/(zyd(n1)-zyd(n3))
            bb = -1.0d0
            cc = yyd(n3)-aa*zyd(n3)
         ddis = abs(aa*zyd(2)+bb*yyd(2)+cc)/sqrt(aa*aa+bb*bb)
         dddy = 
     & yyd(n2)-(aa* (aa*yyd(n2)-bb*zyd(n2))-bb*cc)/(aa*aa+bb*bb)
c           print *,ddis,yydis,yyd(2)



c           if(iyy.gt.2.and.ixx.gt.1) then
           print *,'pass'
             chisqmin = 1.d30
                  xim = 1.d30
                  yim = 1.d30
              thetaim = 1.d30
                phiim = 1.d30
                 gamm = 1.d30


c& starting angle (thetai = pi = downward) 
             gam=10000.d0
             if(ddis.gt.0) then
               gam=-40./ddis
               if(dddy.gt.0) gam=40./ddis
             endif

c            gam=1.d0/gam

c           do yi=-20.0,20.0,1.
c           do yi=-10.0,10.0,1.0
c           do xi=-10.0,10.0,1.0
           xi=xxd(1)
           yi=yyd(1)
           xangi=(xxd(1)-xxd(2))/(zxd(1)-zxd(2)) 
     & * (10.0d0 + zxd(1) - zxd(2)) + xxd(2)
           yangi=(yyd(1)-yyd(2))/(zyd(1)-zyd(2)) 
     & * (10.0d0 + zyd(1) - zyd(2)) + yyd(2)
 
c           do yangi=yi-3.,yi+3.,1.
c          do xangi=xi-3.,xi+3.,1.
c           do gam=-50.,50.,5.
c             gam=-40.
             par(1)=gam
             par(2)=xi
             par(3)=yi
             par(4)=xangi
             par(5)=yangi

          if(abs(gam).gt.10.) then

           chisq=func(par)
           if(chisq.lt.chisqmin) then 
             chisqmin = chisq  
             chisqymin = chisqy  
                  xim = xi
                  yim = yi
               xangim = xangi
               yangim = yangi
                 gamm = gam
c                print *,'chisqmin=',chisqmin,gamm,xim,yim,phiim,thetaim
           endif

C ABS(GAM)>10
           endif
c GAM loop
c           enddo
c Xi loop 
c           enddo
c Yi loop
c           enddo
c Xangi loop
c           enddo
c Yangi loop
c           enddo

dd           xii=xim+0.1d0
dd           yii=yim+0.1d0
dd           thetaii=thetaim+0.01d0
dd           phiii=phiim+0.1d0
dd           gamii=gamm+1.0d0
dd           call findtrack(xxd,zxd,ixx,yyd,zyd,iyy,gamm,xim,yim,thetaim,phiim,chisqg)  

           print *,'final 1 chisqmin , gam, xi, yi xang, yang=',chisqmin,gamm,xim,yim,xangim,yangim

           npar=5
           par(1)=gamm
           par(2)=xim
           par(3)=yim
           par(4)=xangim
           par(5)=yangim

d          gam=1.d-5
d          iter=0
d  45      continue
d          iter=iter+1
d          call dfunc(par,grad)
d  44      continue       
d          chisqo=func(par)
d          do mn=1,5          
d
d            par(mn)=par(mn)-grad(mn)*gam            
d          enddo
d          chisqn=func(par)
c          print *,grad
c          print *,chisqn,chisqo
d          if(chisqn.lt.chisqo) goto 44
d          do mn=1,5          
d            par(mn)=par(mn)+grad(mn)*gam            
d          enddo
d          gam=gam*1.4 
d          if(iter.lt.1000) go to 45
d
c          print *,chisqo,par         
d          fret=func(par)          

d           hhh=1000.0
d           gtol=10.
d           call dfpmin(par,npar,gtol,iter,fret)
d           hhh=100.0
d           gtol=1.
d           call dfpmin(par,npar,gtol,iter,fret)
           hhh=10.0
           gtol=1.d-2
           call dfpmin(par,npar,gtol,iter,fret)
           hhh=0.1
           gtol=1.d-6
           call dfpmin(par,npar,gtol,iter,fret)
           hhh=0.001
           gtol=1.d-10
           call dfpmin(par,npar,gtol,iter,fret)

           hhh=0.00001
           gtol=1.d-12
           call dfpmin(par,npar,gtol,iter,fret)

           hhh=0.0000001
           gtol=1.d-15
           call dfpmin(par,npar,gtol,iter,fret)


           gamm=par(1)
           xim=par(2)
           yim=par(3)
           xangim=par(4)
           yangim=par(5)
           write(67,*) gamm*.511,ddis,fret 

           print *,'final 2 chisqmin , gam, xi, yi xang, yang=',i1-1,iter,fret,par
         
c           print *,'final chisqmin , gam, xi, yi phi, thetai=',i1-1,iter,chisqmin,gamm,xim,yim,phiim,thetaim
c           print *,'chisqy=',chisqymin
           call GETTRACK(xangim,yangim,Xim,Yim,GAMm,Xb,Yb,Zb,npts)

               write(57,*) 'new frame'
               write(57,25) i1-1,fret, gamm*0.511,ddis,iter
 25            format('Title 3 9.5 size 2.5 ',1H',I4,' ',F12.4,
     & ' ',F12.4,' ',F12.4,' ',I5,1H') 
               write(57,*) 'SET WINDOW X 1.25 6'
               write(57,*) 'SET WINDOW Y 1 9.3125'
               write(57,*) 'SET LIMIT X -10  10'
               write(57,*) 'SET LIMIT Y -17.5  17.5'
               write(57,*) 'SET SYMBOL 9O'
               write(57,*) 'SET ORDER X Y'
               do mm=1,ixx
                 write(57,*) xxd(mm),zxd(mm)+9.5
               enddo 
               write(57,*) 'plot'
           do mm=1,npts
c              write(57,*) Xb(mm),Yb(mm),Zb(mm)
              write(57,*) Xb(mm),Zb(mm)+9.5
           enddo
               write(57,*) 'join'

               write(57,*) 'SET WINDOW Y 1 9.3125'
               write(57,*) 'SET WINDOW X 8 12.75'
               write(57,*) 'SET LIMIT X -10  10'
               write(57,*) 'SET LIMIT Y -17.5  17.5'
               write(57,*) 'SET SYMBOL 9O'
               write(57,*) 'SET ORDER X Y'
               do mm=1,iyy
                 write(57,*) yyd(mm),zyd(mm)+9.5
               enddo 
               write(57,*) 'plot'
           do mm=1,npts
c              write(57,*) Xb(mm),Yb(mm),Zb(mm)
              write(57,*) yb(mm),Zb(mm)+9.5
           enddo
               write(57,*) 'join'



           endif
 94        continue 
            goto 200
            endif


 
           enddo
c* loop over particle records

            end


           function func(par)
           IMPLICIT REAL*8 (A-H,O-Z)
c           dimension xxd(50),zxd(50),yyd(50),zyd(50)
c           common trackdat/ xxd,zxd,yyd,zyd/
           common /trackdat/ xxd(50),zxd(50),yyd(50),zyd(50)
           common /trackdatind/ ixx,iyy
           dimension xb(2000),yb(2000),zb(2000),y2(2000)
           dimension xbi(2000),ybi(2000),zbi(2000)
           dimension par(5)
           gam=par(1)
           xi=par(2)
           yi=par(3)
           xangi=par(4)
           yangi=par(5)
c           write (27,*) par            

           chisq=0.0
           if(abs(gam).lt.10) chisq=chisq+(abs(gam)-10.d0)**2
c           if(thetai.gt.3.1415926535897932384626433832795) then
c            chisq=chisq+1d3
c            func=chisq
c            return
c          endif
c           gami=1.d0/gam
           call GETTRACK(XANGi,YANGi,XI,YI,gam,Xb,Yb,Zb,npts)
c           print *,'Func1',THETAi,PHIi,XI,YI,gam,Xbi,Ybi,Zbi,npts
c           do i=1,npts
c             irev=npts-i+1
c             xb(i)=xbi(irev)
c             yb(i)=ybi(irev)
c             zb(i)=zbi(irev)
c           enddo

d           do i=1,npts-1
d            dz=zb(i+1)-zb(i)
d             if(dz.le.0.d0) then
d              print *,'dz',dz
d              chisq=chisq+1.d3
d              func=chisq
d              return
d             endif
d            enddo

           icntx=0 
           do mm=1,ixx
           do kk=1,npts-1
            if(zxd(mm).le.zb(kk).and.zxd(mm).gt.zb(kk+1))then
               icntx=icntx+1
               xout=((xb(kk)-xb(kk+1))/(zb(kk)-zb(kk+1)))*(zxd(mm)-zb(kk+1))+xb(kk+1)
               chisq=chisq+(xout-xxd(mm))**2 
               goto 100
            endif
           enddo
 100       continue
           enddo
           if(icntx.ne.ixx)  chisq=chisq+1.0d3

           icnty=0 
           do mm=1,iyy
           do kk=1,npts-1
            if(zyd(mm).le.zb(kk).and.zyd(mm).gt.zb(kk+1))then
               icnty=icnty+1
               yout=((yb(kk)-yb(kk+1))/(zb(kk)-zb(kk+1)))*(zyd(mm)-zb(kk+1))+yb(kk+1)
               chisq=chisq+(yout-yyd(mm))**2 
               goto 200
            endif 
           enddo
 200       continue
           enddo
           if(icnty.ne.iyy)  chisq=chisq+1.0d3

           func=chisq
           return
           end

      FUNCTION dfridr(par,h,m,err)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NTAB
      REAL*8 dfridr,err,h,x,func,CON,CON2,BIG,SAFE
      PARAMETER (CON=1.4d0,CON2=CON*CON,BIG=1.d30,NTAB=10,SAFE=2.d0)
      EXTERNAL func
CU    USES func
      INTEGER i,j
      REAL*8 errt,fac,hh,a(NTAB,NTAB)
      dimension par(5),parp(5),parn(5)
      dfridr=0.d0
      if(h.eq.0.) pause 'h must be nonzero in dfridr'
      hh=h
      do mmm=1,5
        parp(mmm)=par(mmm)
        parn(mmm)=par(mmm)
      enddo

       parp(m)=par(m)+hh
       parn(m)=par(m)-hh

      a(1,1)=(func(parp)-func(parn))/(2.0d0*hh)
c      a(1,1)=(func(x+hh)-func(x-hh))/(2.0d0*hh)
      err=BIG
      do 12 i=2,NTAB
        hh=hh/CON
c        a(1,i)=(func(x+hh)-func(x-hh))/(2.0*hh)

        parp(m)=par(m)+hh
        parn(m)=par(m)-hh
        a(1,i)=(func(parp)-func(parn))/(2.0d0*hh)
        fac=CON2
        do 11 j=2,i
          a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.d0)
          fac=CON2*fac
          errt=max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
          if (errt.le.err) then
            err=errt
            dfridr=a(j,i)
          endif
11      continue
        if(abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err)return
12    continue
      return
      END



            subroutine dfunc(par,grad)
            IMPLICIT REAL*8 (A-H,O-Z)
            external func, dfridr
            common /dstep/hhh
            dimension par(5),grad(5)
            chisq1=func(par)
            do m=1,5
            h=hhh
            grad(m)= dfridr(par,h,m,err)
c            print *,grad(m),err
            enddo
           return
           end


            subroutine dfuncold(par,grad)
            IMPLICIT REAL*8 (A-H,O-Z)
            external func
            dimension par(5),dpar(5),parp(5),parm(5),grad(5)
            data dpar/0.01,0.01,0.01,0.01,0.01/
            chisq1=func(par)
             
            do m=1,5
            grad(m)=0.0
            gradold=grad(m)
            gam=chisq1
            if(gam.lt.1.0) gam=1.d0
 10         continue
            do l=1,5
              parp(l)=par(l)
              parm(l)=par(l)
              if(l.eq.m) then
               parp(l)=par(l)+dpar(l)*gam
               parm(l)=par(l)-dpar(l)*gam
              endif              
            enddo        
            grad(m)=(func(parp)-func(parm))/(2.d0*dpar(m)*gam)
c            print *,'gam',gam

            if(abs(grad(m)).le.1.0d-5.or.grad(m).gt.1.0e5) then
              gam = gam * 1.4
              go to 10
            endif 

            if(abs(gradold-grad(m)).gt.abs(1.d-2*gradold)) then
               gradold = grad(m)            
               gam = gam / 1.4
              go to 10
            endif

            enddo        
       

            print *,'Inside dfunc, chisq',chisq1
            print *,'par',par
            print *,'grad',grad
           return
           end



           subroutine GETTRACK(Xang,Yang,XII,YII,GAM,Xb,Yb,Zb,npts)
           IMPLICIT REAL*8 (A-H,O-Z)
           data alpha/1.8225D3/
           dimension xb(2000),yb(2000),zb(2000)
           PI=3.1415926535897932384626433832795d0
           Xi=Xii
           Yi=Yii
           dZang=10.d0
           dx=Xii-Xang
           dy=Yii-Yang
           rad=dsqrt(dx*dx+dy*dy+dzang*dzang)
           thetai=pi-ACOS(dzang/rad)
           PHIi = dATAN2(dY,dX)
         
c           print *,'get track'
c           print *,Xang,Yang,XII,YII,GAM
c           print *,thetai,phii
c           print *,' '
c           Thetai=Thetaii
c           Phii=Phiii
c* time step in ns   (c*dt = dL) 
           DTo=20.D-3
c* starting point
c           ZI = 30.0D0
           ZI = 0.0D0
           npts=0
c           print *,' '
c           print *,'1'
c           print *,THETAii,PHIii,XII,YII,gam
c           write(88,*) 'S'
           DO K=1,2000
c            WRITE(88,'(5(1X,F12.7))')Xi,Yi,Zi,thetai,phii
           npts=npts+1
           xb(k)=xi 
           yb(k)=yi 
           zb(k)=zi 
            CALL TRACK_BFIELD(Xi,Yi,Zi,THETAi,PHIi,ALPHA,GAM,DTo,
     &      Xf,Yf,Zf,THETAf,PHIf)
               Xi=Xf
               Yi=Yf
               Zi=Zf
               THETAi=THETAf
               PHIi=PHIf
           IF(ZF.LT.-30.d0)then
c              print *,'2'
              return
           endif 

           ENDDO
         
c           print *,'2a',npts
c           print *,xb(npts),yb(npts),zb(npts)


           return
           END

cc*************************************************************
cc** using the information of the initial trajectory        * *
cc** Routine to determine the resultant trajectory of       * *
cc** a charge particle in an external magnet field          * *
cc** within short distances in which the magnetic field     * *
cc** may be considered constant                             * *
cc*************************************************************

c** Inputs
c** (Xi,Yi,Zi) the initial position of the particle in cm
c** THETAi the initial Azimuthal angle in radians
c** PHIi the initial Polar angle in radians
c** ALPHA (CHARGE/MASS) electron charge/AMU (Electron ALPHA  = 1.8225E3)
c** GAM = 1./SQRT(1-BETA(v/c)**2)
c** dt(ns) elasped time to propagate the particle to new a point and direction
c** Bfield is taken to be constant within this distance of propagation

c** Outputs
c** (Xf,Yf,Zf) the final position of the particle in cm
c** THETAo the resulting Azimuthal angle in radians at (Xf,Yf,Zf) 
c** PHIo the resulting Polar angle in radians at (Xf,Yf,Zf) 

c           OPTIONS/EXTEND_SOURCE
           SUBROUTINE TRACK_BFIELD(Xi,Yi,Zi,THETAi,PHIi,ALPHA,GAM,DTo,
     &     Xf,Yf,Zf,THETAf,PHIf)
           IMPLICIT NONE 

           REAL*8 X,Y,Z,Bx,By,Bz,VXo,VYo,VZo,VX,VY,VZ,SCALE1,SCALE2
           REAL*8 THETA,PHI,GAM,XO,YO,ZO,VT,VXY,W,SN,CS,C,TPI,DT
           REAL*8 XM,YM,ZM,B1,B2,B3,Bxy
           REAL*8 Xi,Yi,Zi,THETAi,PHIi,ALPHA,BETA,DTo,
     &     Xf,Yf,Zf,THETAf,PHIf

           DATA C/29.979246d0/
           DATA TPI/6.283185307179586476925286766559d0/
c** To obtain units of Lorentz acceleration in Z(e)/M (amu) cm/ns**2
c** when velocity is expressed in units of cm/ns    
C** SCALE2
C** =[(1.E-9(ns))**2/(1.E-2 (cm))]*1.602E-19(e)*[(1.E9)/(1E2)]/1.660566E-27(amu)
            DATA SCALE2/9.6473D-2/
c            DATA SCALE2/9.6473E-4/
C** CONVERT FROM TESLA TO GAUSS
c            DATA SCALE1/1.D4/
            DATA SCALE1/1.D-4/

c                 X = Xi
c                 Y = Yi
c                 Z = Zi
             
                 THETA = THETAi
                 PHI = PHIi
                 
                 DT=DTO

C                 GAM = 1./SQRT(1.-BETA**2)

                 IF(abs(GAM).LT.5.0) THEN
                  XF=XI 
                  YF=YI 
                  ZF=ZI 
                  Thetaf=Thetai
                  Phif=Phii
                  return
                 ENDIF

                 BETA = SQRT(1.-1./GAM**2)

                 VT = C*BETA

                 VX = VT*SIN(THETA)*COS(PHI)
                 VY = VT*SIN(THETA)*SIN(PHI)
                 VZ = VT*COS(THETA)

C** INPUTS IN cm AND OUTPUT IN Gauss 
                 CALL MAGF(Xi,Yi,Zi,Bx,By,Bz)
c                 PRINT *,Xi,Yi,Zi,BX,BY,BZ               
                 IF(ABS(Bx).le.1.D-4 .AND. 
     & ABS(By).LE.1.D-4 .AND. ABS(Bz).le.1.D-4) then
c                   print *,'field too weak:: linear track'                
                   Xf = Xi + VX * DT
                   Yf = Yi + VY * DT
                   Zf = Zi + VZ * DT
                   Thetaf = Thetai
                   Phif = Phii

                   RETURN
                 END IF

                  W = SCALE1 * SCALE2 * ALPHA * SQRT(Bx**2+By**2+Bz**2)/GAM

                  IF(ABS(Bx).Ge.1.D-8 .OR.ABS(By).GE.1.D-8) THEN 

                  CALL ROT_CONV(Bx,By,Bz,VX,VY,VZ,VXo,VYo,VZo)
                  CALL ROT_CONV(Bx,By,Bz,Xi,Yi,Zi,Xo,Yo,Zo)
 

                  SN = SIN(W * DT) * VXo - COS(W * DT) * VYo
                  CS = COS(W * DT) * VXo + SIN(W * DT) * VYo
                  Xo = Xo + (SN + VYo)/W
                  Yo = Yo + (CS - VXo)/W
                  Zo = Zo + VZo*DT
 
                  VXo = CS
                  VYo = -SN
C                 VZo = VZo                   

                 CALL ROT_INCONV(Bx,By,Bz,VXo,VYo,VZo,VX,VY,VZ)
                 CALL ROT_INCONV(Bx,By,Bz,Xo,Yo,Zo,X,Y,Z)

                 ELSE

                
                  SN = SIN(W * DT) * VX - COS(W * DT) * VY
                  CS = COS(W * DT) * VX + SIN(W * DT) * VY
                  X = X + (SN + VY)/W
                  Y = Y + (CS - VX)/W
                  Z = Z + VZo*DT

                  VX = CS
                  VY = -SN
                  


                 ENDIF

                 PHIf = ATAN2(VY,VX)
                 IF(PHIf.lt.0.d0) PHIf=PHIf+TPI
                 THETAf = ACOS(VZ/SQRT(VX**2 + VY**2 + VZ**2))
                 Xf = X          
                 Yf = Y    
                 Zf = Z          

                 RETURN
               END


c               OPTIONS/EXTEND_SOURCE
               SUBROUTINE ROT_CONV(Bx,By,Bz,Xin,Yin,Zin,Xout,Yout,Zout)
C**        Rotates coordinate system to a 
C**        Bx'=0, By'=0, Bz'=SQRT(Bx**2+By**2+Bz**2) system
C**        This rotation reduces the equations of motion to a 2-D case
C**        --->  dVz'/dt=0 

               IMPLICIT NONE 
               REAL*8 Bx,By,Bz,Xin,Yin,Zin,Xout,Yout,Zout
               REAL*8 CSTH,SNTH,CSPH,SNPH


                 CSTH=By/DSQRT(By**2+Bx**2)
                 SNTH=Bx/DSQRT(By**2+Bx**2)

                 CSPH=Bz/DSQRT(Bz**2 + By**2 + Bx**2)
                 SNPH=DSQRT(1.-CSPH**2)
 
                 Xout=Xin*CSTH-Yin*SNTH
                 Yout=Xin*CSPH*SNTH+Yin*CSPH*CSTH-Zin*SNPH
                 Zout=Xin*SNPH*SNTH+Yin*SNPH*CSTH+Zin*CSPH

               RETURN 
               END



c               OPTIONS/EXTEND_SOURCE
               SUBROUTINE ROT_INCONV(Bx,By,Bz,Xin,Yin,Zin,Xout,Yout,Zout)
               IMPLICIT NONE 
               REAL*8 Bx,By,Bz,Xin,Yin,Zin,Xout,Yout,Zout
               REAL*8 CSTH,SNTH,CSPH,SNPH

c** Inverse rotation of SUBROUTINE ROT_CONV


                 CSTH=By/DSQRT(By**2+Bx**2)
                 SNTH=Bx/DSQRT(By**2+Bx**2)

                 CSPH=Bz/DSQRT(Bz**2 + By**2 + Bx**2)
                 SNPH=DSQRT(1.-CSPH**2)
 
                 Xout=Xin*CSTH+Yin*CSPH*SNTH+Zin*SNPH*SNTH
                 Yout=-Xin*SNTH+Yin*CSPH*CSTH+Zin*SNPH*CSTH
                 Zout=-Yin*SNPH+Zin*CSPH

               RETURN 
               END

               subroutine magf(x,y,z,Bx,By,Bz)
               implicit real*8 (A-H,O-Z)
c* uniform field of 10000 gauss in x direction z=+/-5cm
c* can be modified to open a file and read field data into an array  

               bx=0.d0
               by=0.0d0
               bz=0.0d0
               rr=dsqrt(x*x+y*y)
               if(rr.gt.6.4)return 
               if(z.gt.-4.6.or.z.lt.-14.6)return 
c               if(abs(z).lt.5) bx=10000.d0
               CALL MAGFLD ( X, Y, Z, BTX, BTY, BTZ, B)
               B = B * 1.0d4
               bx = btx * b
               by = bty * b
               bz = btz * b
               return
               end


      SUBROUTINE MAGFLD ( X, Y, Z, BTX, BTY, BTZ, B)
      implicit real*8 (A-H,O-Z)

      LOGICAL FIRSTCALL
      DATA FIRSTCALL/.TRUE./ 
      DIMENSION XX(81,81,81),YY(81,81,81),ZZ(81,81,81)
      DIMENSION BXX(81,81,81),BYY(81,81,81),BZZ(81,81,81)
      DIMENSION BTT(81,81,81)
        

*
*  +-------------------------------------------------------------------*
      IF ( firstcall ) THEN
      firstcall=.FALSE.
      OPEN(56,
     & FILE='/home/sarah/AESOPLITE/FLUKA/fieldmap.txt',
     & STATUS='OLD')
      DO II=1,81
       DO JJ=1,81
        DO KK=1,81
           READ(56,*) XX(II,JJ,KK),YY(II,JJ,KK),ZZ(II,JJ,KK),
     &   BXX(II,JJ,KK),BYY(II,JJ,KK),BZZ(II,JJ,KK)
         XX(II,JJ,KK)=XX(II,JJ,KK)/10.d0
         YY(II,JJ,KK)=YY(II,JJ,KK)/10.d0
         ZZ(II,JJ,KK)=ZZ(II,JJ,KK)/10.d0

        BTT(II,JJ,KK)
     &  =DSQRT(BXX(II,JJ,KK)**2 + BYY(II,JJ,KK)**2 + BZZ(II,JJ,KK)**2)
        BXX(II,JJ,KK) = BXX(II,JJ,KK)/BTT(II,JJ,KK)
        BYY(II,JJ,KK) = BYY(II,JJ,KK)/BTT(II,JJ,KK)
        BZZ(II,JJ,KK) = BZZ(II,JJ,KK)/BTT(II,JJ,KK)
        BTT(II,JJ,KK) = BTT(II,JJ,KK)/1.0D4

        ENDDO
       ENDDO
      ENDDO  
      close(56)
      ENDIF


*  |
*  +-------------------------------------------------------------------*
      IXX = DINT(2.D0 * (X + 20.0D0)) + 1
      IYY = DINT(2.D0 * (Y + 20.0D0)) + 1
      IZZ = DINT(2.D0 * (Z + 29.5D0)) + 1
      IF(IXX.LT.1.OR.IYY.LT.1.OR.IZZ.LT.1.OR.
     &   IXX.GT.81.OR.IYY.GT.81.OR.IZZ.GT.81) THEN
       BTX0   = 0.0D0
       BTY0   = 0.0D0
       BTZ0   = 0.0D0
       B0     = 0.0D0
      ELSE
       BTX0   = BXX(IXX,IYY,IZZ)
       BTY0   = BYY(IXX,IYY,IZZ)
       BTZ0   = BZZ(IXX,IYY,IZZ)
       B0     = BTT(IXX,IYY,IZZ)
      ENDIF

c      IXX1 = DINT(2.D0 * (X + 20.5D0)) + 1
c      IYY1 = DINT(2.D0 * (Y + 20.5D0)) + 1
c      IZZ1 = DINT(2.D0 * (Z + 30.0D0)) + 1
      IXX1=IXX+1 
      IYY1=IYY+1 
      IZZ1=IZZ+1 



      IF(IXX1.LT.1.OR.IYY1.LT.1.OR.IZZ1.LT.1.OR.
     &   IXX1.GT.81.OR.IYY1.GT.81.OR.IZZ1.GT.81) THEN
       BTX1   = 0.0D0
       BTY1   = 0.0D0
       BTZ1   = 0.0D0
       B1     = 0.0D0
      ELSE
       BTX1   = BXX(IXX1,IYY1,IZZ1)
       BTY1   = BYY(IXX1,IYY1,IZZ1)
       BTZ1   = BZZ(IXX1,IYY1,IZZ1)
       B1     = BTT(IXX1,IYY1,IZZ1)
      ENDIF
d
      dx = 2.D0 *(X + 20.0D0)-Dint(2.D0 *(X + 20.0D0))
      dy = 2.D0 *(Y + 20.0D0)-Dint(2.D0 *(Y + 20.0D0))
      dz = 2.D0 *(Z + 20.0D0)-Dint(2.D0 *(Z + 20.0D0))

      BTX11 = B1*BTX1*DX + B0*BTX0*(1.d0-DX)
      BTY11 = B1*BTY1*DY + B0*BTY0*(1.d0-DY)
      BTZ11 = B1*BTZ1*DZ + B0*BTZ0*(1.d0-DZ)
      B = DSQRT(BTX11*BTX11 + BTY11*BTY11 + BTZ11*BTZ11) 
      IF(B.GT.0) THEN 
       BTX = BTX11/B
       BTY = BTY11/B
       BTZ = BTZ11/B
      ENDIF

      RETURN
      END


C* spline interpolation 
      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      implicit real*8 (A-H,O-Z)
      INTEGER n
      REAL*8 x,y,xa(n),y2a(n),ya(n)
      INTEGER k,khi,klo
      REAL*8 a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *2)/6.
      return
      END

c calculate 2nd directives for spline function
      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      implicit real*8 (A-H,O-Z)
      INTEGER n,NMAX
      REAL*8 yp1,ypn,x(n),y(n),y2(n)
      PARAMETER (NMAX=3000)
      INTEGER i,k
      REAL*8 p,qn,sig,un,u(NMAX)
      if (yp1.gt..99d30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *u(i-1))/p
11    continue
      if (ypn.gt..99d30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END

c* gradient method minimization 


      SUBROUTINE dfpmin(p,n,gtol,iter,fret)
      implicit real*8 (A-H,O-Z)
      INTEGER iter,n,NMAX,ITMAX
      REAL*8 fret,gtol,p(5),gg(5),func,EPS,STPMX,TOLX
      PARAMETER (NMAX=50,ITMAX=800,STPMX=100.,EPS=3.d-8,TOLX=4.d0*EPS)
      EXTERNAL func
CU    USES dfunc,func,lnsrch
      INTEGER i,its,j
      LOGICAL check
      REAL*8 den,fac,fad,fae,fp,stpmax,sum,sumdg,sumxi,temp,test,dg(NMAX),
     *g(NMAX),hdg(NMAX),hessin(NMAX,NMAX),pnew(NMAX),xi(NMAX)
      fp=func(p)
      call dfunc(p,gg)
      g(1)=gg(1)
      g(2)=gg(2)
      g(3)=gg(3)
      g(4)=gg(4)
      g(5)=gg(5)
      sum=0.d0
      do 12 i=1,n
        do 11 j=1,n
          hessin(i,j)=0.d0
11      continue
        hessin(i,i)=1.
        xi(i)=-g(i)
        sum=sum+p(i)**2
12    continue
      stpmax=STPMX*max(sqrt(sum),float(n))
      do 27 its=1,ITMAX
        iter=its
        call lnsrch(n,p,fp,g,xi,pnew,fret,stpmax,check)
        fp=fret
        do 13 i=1,n
          xi(i)=pnew(i)-p(i)
          p(i)=pnew(i)
13      continue
        test=0.
        do 14 i=1,n
          temp=abs(xi(i))/max(abs(p(i)),1.)
          if(temp.gt.test)test=temp
14      continue
        if(test.lt.TOLX)return
        do 15 i=1,n
          dg(i)=g(i)
15      continue
c        call dfunc(p,g)
        call dfunc(p,gg)
        g(1)=gg(1)
        g(2)=gg(2)
        g(3)=gg(3)
        g(4)=gg(4)
        g(5)=gg(5)


        test=0.d0
        den=max(fret,1.)
        do 16 i=1,n
          temp=abs(g(i))*max(abs(p(i)),1.)/den
          if(temp.gt.test)test=temp
16      continue
        if(test.lt.gtol)return
        do 17 i=1,n
          dg(i)=g(i)-dg(i)
17      continue
        do 19 i=1,n
          hdg(i)=0.d0
          do 18 j=1,n
            hdg(i)=hdg(i)+hessin(i,j)*dg(j)
18        continue
19      continue
        fac=0.d0
        fae=0.d0
        sumdg=0.d0
        sumxi=0.d0
        do 21 i=1,n
          fac=fac+dg(i)*xi(i)
          fae=fae+dg(i)*hdg(i)
          sumdg=sumdg+dg(i)**2
          sumxi=sumxi+xi(i)**2
21      continue
        if(fac**2.gt.EPS*sumdg*sumxi)then
          fac=1./fac
          fad=1./fae
          do 22 i=1,n
            dg(i)=fac*xi(i)-fad*hdg(i)
22        continue
          do 24 i=1,n
            do 23 j=1,n
              hessin(i,j)=hessin(i,j)+fac*xi(i)*xi(j)-fad*hdg(i)*hdg(j)+
     *fae*dg(i)*dg(j)
23          continue
24        continue
        endif
        do 26 i=1,n
          xi(i)=0.d0
          do 25 j=1,n
            xi(i)=xi(i)-hessin(i,j)*g(j)
25        continue
26      continue
27    continue
c      pause 'too many iterations in dfpmin'
      return
      END


      SUBROUTINE lnsrch(n,xold,fold,g,p,x,f,stpmax,check)
      implicit real*8 (A-H,O-Z)
      INTEGER n
      LOGICAL check
      REAL*8 f,fold,stpmax,g(5),p(5),x(5),xold(5),func,ALF,TOLX
      PARAMETER (ALF=1.d-4,TOLX=1.d-7)
      EXTERNAL func
CU    USES func
      INTEGER i
      REAL*8 a,alam,alam2,alamin,b,disc,f2,fold2,rhs1,rhs2,slope,sum,temp,
     *test,tmplam
      check=.false.
      sum=0.d0
      do 11 i=1,n
        sum=sum+p(i)*p(i)
11    continue
      sum=sqrt(sum)
      if(sum.gt.stpmax)then
        do 12 i=1,n
          p(i)=p(i)*stpmax/sum
12      continue
      endif
      slope=0.d0
      do 13 i=1,n
        slope=slope+g(i)*p(i)
13    continue
      test=0.
      do 14 i=1,n
        temp=abs(p(i))/max(abs(xold(i)),1.d0)
        if(temp.gt.test)test=temp
14    continue
      if(test.eq.0.d0) print *,'test is zero'  
      if(test.eq.0.d0) return
      alamin=TOLX/test
      alam=1.d0
1     continue
        do 15 i=1,n
          x(i)=xold(i)+alam*p(i)
15      continue
        f=func(x)
        if(alam.lt.alamin)then
          do 16 i=1,n
            x(i)=xold(i)
16        continue
          check=.true.
          return
        else if(f.le.fold+ALF*alam*slope)then
          return
        else
          if(alam.eq.1.d0)then
            tmplam=-slope/(2.d0*(f-fold-slope))
          else
            rhs1=f-fold-alam*slope
            rhs2=f2-fold2-alam2*slope
            a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
            b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
            if(a.eq.0.d0)then
              tmplam=-slope/(2.d0*b)
            else
              disc=b*b-3.d0*a*slope
              tmplam=(-b+sqrt(disc))/(3.d0*a)
            endif
            if(tmplam.gt..5d0*alam)tmplam=.5d0*alam
          endif
        endif
        alam2=alam
        f2=f
        fold2=fold
        alam=max(tmplam,.1d0*alam)
      goto 1
      END

