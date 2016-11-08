            implicit real*8 (A-H,O-Z)
            dimension x(500),y(500),z(500)
            character*132 line
            open(56,file='plottrack.top',status='new')
            open(55,file='aesoplite20mev_fort.99',status='old')
c            open(55,file='aesoplite001_fort.99',status='old')
c            open(55,file='nicenice001_fort.99',status='old')
c            open(55,file='niceair001_fort.99',status='old')
            i1old=1
            icn=0
            do k=1,5000000
             iprint=0
             read(55,'(A132)') line
             read(line,*) i1,i2,i3,i4,f1,f2,f3,f4,f5
             if(f4.gt.33.499) energy=f1
c             print *,i1,i2,i3,i4,f1,f2,f3,f4,f5
             if(i1.eq.i1old) then
c* photons are not plotted
              if(i4.ne.7) then
c              if(i4.eq.11) then
          mreg=i2
          newreg=i3
          print *,mreg,newreg
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
               

               icn=icn+1  
               print *,' '
               print *,icn
               print *,' '

               x(icn)=f2  
               y(icn)=f3  
               z(icn)=f4  
              endif   
              endif   
              endif   

             if(i1.ne.i1old) then
             i1old=i1
             it1=0
             it3=0
             it4=0

c     &             (z(kk).gt.-2.0.and.z(kk).lt.-1.0
               do kk=1,icn 
                 if(
     &             (z(kk).gt.33.1.and.z(kk).lt.33.6).and.
     &             (sqrt(x(kk)**2+y(kk)**2).lt.13.5))it1=1
                 if(
     &             (z(kk).gt.0.1.and.z(kk).lt.0.6).and.
     &             (sqrt(x(kk)**2+y(kk)**2).lt.3.5))it3=1
                 if(
     &             (z(kk).gt.-23.5.and.z(kk).lt.-22.9).and.
     &             (sqrt(x(kk)**2+y(kk)**2).lt.18.0))it4=1

               enddo 



               if(it1.eq.1.and.it3.eq.1.and.it4.eq.1.and.icn.gt.1) then
c               if(iprint1.eq.1.and.iprint2.eq.1) then
               write(56,*) 'new frame'
               f1=0.3
               write(56,50) i1old,I4,energy
 50            Format('title 3 9.5 size 3 ',1H','Evt#=',I5,'   PrtID=',I3,'   E=',F7.3,1H')
               write(56,*) 'SET WINDOW X 1.25 6' 
               write(56,*) 'SET WINDOW Y 1 9.3125' 
               write(56,*) 'SET LIMIT X -10  10'
               write(56,*) 'SET LIMIT Y -17.5  17.5' 
c               print *,i1,icn
c               i1old=i1
               write(56,*) 'SET SYMBOL 9O'  
               write(56,*) 'SET ORDER X Y'
               do kk=1,icn 
c                 print *,'1',z(kk)
                 if((z(kk).gt.-2.0.and. z(kk).lt.-1.0).or. 
     &             (z(kk).gt.-18.0.and.z(kk).lt.-17.0).or.
     &             (z(kk).lt.-21.0.and.z(kk).gt.-22.0)) then
c                    print *,'2',z(kk)
                     write(56,*) x(kk),z(kk)+9.5
                 endif              
               enddo 
               write(56,*) 'plot'

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

d               if(icn.gt.0) then
               write(56,*) 'SET WINDOW Y 1 9.3125' 
               write(56,*) 'SET WINDOW X 8 12.75'
               write(56,*) 'SET LIMIT X -10  10'
               write(56,*) 'SET LIMIT Y -17.5  17.5' 
               write(56,*) 'SET SYMBOL 9O'  
               write(56,*) 'SET ORDER X Y'
               do kk=1,icn

C                 if(z(kk).gt.0.0.or. 
                 if(
     &             (z(kk).gt.-4.0.and.z(kk).lt.-3.0).or.
     &             (z(kk).gt.-10.0.and.z(kk).lt.-9.0).or.
     &             (z(kk).gt.-16.0.and.z(kk).lt.-15.0).or.
     &             (z(kk).gt.-20.0.and.z(kk).lt.-19.0)) then
C     &             (z(kk).lt.-18.0)) then
                     write(56,*) y(kk),z(kk)+9.5
                 endif              
               enddo 
               write(56,*) 'plot'

               endif

               icn=1
               x(icn)=f2  
               y(icn)=f3  
               z(icn)=f4  


            endif
            enddo

            end
