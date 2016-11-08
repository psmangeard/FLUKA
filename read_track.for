c variables that begin A-H and O-Z are double precision 
            implicit real*8 (A-H,O-Z)
c declaring of array to points along the track
            dimension x(500),y(500),z(500)
c declaring a character string with length 132  
            character*132 line
c open a new file for output   
            open(56,file='plottrack.top',status='new')
c open input file 
            open(55,file='aesoplite20mev_fort.99',status='old')
c            open(55,file='aesoplite001_fort.99',status='old')
c            open(55,file='nicenice001_fort.99',status='old')
c            open(55,file='niceair001_fort.99',status='old')
c stores old event number
            i1old=1
C  icn counts the number of particle crossings of the detectors in the system 
            icn=0
c  k loop about the records / lines in the file
            do k=1,5000000
c             iprint=0
c   read a single line in the fluka file into a character string  
             read(55,'(A132)') line
c   read the character string into integers and floating point values
             read(line,*) i1,i2,i3,i4,f1,f2,f3,f4,f5
c store the initial energy 
             if(f4.gt.33.499) energy=f1
c             print *,i1,i2,i3,i4,f1,f2,f3,f4,f5
c still reading the same event
             if(i1.eq.i1old) then
c* photons are not plotted
              if(i4.ne.7) then
c              if(i4.eq.11) then
c* boundary crossing information
c* assigning the particle's track old region ID to variable mreg
          mreg=i2
c* assigning the particle's track new region ID to variable newreg
          newreg=i3
c* print the old and new region IDs
          print *,mreg,newreg
          IF(
c* air to T1   from 19 to 1
     &      (MREG.eq.19.and.NEWREG.eq.1).or.
c* air to T3    from 19 to 6
     &      (MREG.eq.19.and.NEWREG.eq.6).or.
c* air to Guard from 19 to 7 (why is this here??)
     &      (MREG.eq.19.and.NEWREG.eq.7).or.
c* air to Tracker1 from 19 to 11 
     &      (MREG.eq.19.and.NEWREG.eq.11).or.
c* air to Tracker2 from 19 to 12 
     &      (MREG.eq.19.and.NEWREG.eq.12).or.
c* air to Tracker3 from 19 to 13 
     &      (MREG.eq.10.and.NEWREG.eq.13).or.										//mistake in code, should be MREG.eq.19
c* air to Tracker4 from 19 to 14
     &      (MREG.eq.19.and.NEWREG.eq.14).or.
c* air to Tracker5 from 19 to 15
     &      (MREG.eq.19.and.NEWREG.eq.15).or.
c* air to Tracker6 from 19 to 16
     &      (MREG.eq.19.and.NEWREG.eq.16).or.
c* air to Tracker7 from 19 to 17
     &      (MREG.eq.19.and.NEWREG.eq.17).or.
c* air to T4 from 19 to 18
     &      (MREG.eq.19.and.NEWREG.eq.18)
     &        ) THEN
               

c* icn = number of boundary crossings 
               icn=icn+1  
               print *,' '
               print *,icn
               print *,' '
c* storing the x,y,z point at the boundary crossing
               x(icn)=f2  
               y(icn)=f3  
               z(icn)=f4  
              endif   
              endif   
              endif   
c* I1 current event number and I1old was the event number during the previous read of the file
C* If I1 not equal to I1old, program read the 1st row of new event  
             if(i1.ne.i1old) then
c* I1old is updatd, but after the IF condition check
             i1old=i1
c* trigger requirement T1+T2+T4    
             it1=0
             it3=0
             it4=0
             itg=0

c     &             (z(kk).gt.-2.0.and.z(kk).lt.-1.0
c* loop through the boundary crossing points 
               do kk=1,icn 
c*  T1 location 
    			 if(
     &             (z(kk).gt.33.1.and.z(kk).lt.33.6).and.
     &             (sqrt(x(kk)**2+y(kk)**2).lt.13.5))it1=1
c*  T3 location 
                 if(
     &             (z(kk).gt.0.1.and.z(kk).lt.0.6).and.
     &             (sqrt(x(kk)**2+y(kk)**2).lt.3.5))it3=1
c*  T4 location 
                 if(
     &             (z(kk).gt.-23.5.and.z(kk).lt.-22.9).and.
     &             (sqrt(x(kk)**2+y(kk)**2).lt.18.0))it4=1

c*  Guard location 
                 if(
     &             (z(kk).gt.0.1.and.z(kk).lt.0.6).and.
     &             (sqrt(x(kk)**2+y(kk)**2).gt.3.5))itg=1

               enddo 


c did a trigger condition occur? If yes, output topdrawer plot commands and define bending and non-bending plane crossings   
               if(it1.eq.1.and.it3.eq.1.and.it4.eq.1.and.icn.gt.1) then
c               if(iprint1.eq.1.and.iprint2.eq.1) then
               write(56,*) 'new frame'
               f1=0.3
               write(56,50) i1old,I4,energy,itg
 50            Format('title 1 9.5 size 3 ',1H','Evt#=',I5,'   PrtID=',I3,'   E=',F7.3,'  Guard Hit=',i3,1H')
               write(56,*) 'SET WINDOW X 1.25 6' 
               write(56,*) 'SET WINDOW Y 1 9.3125' 
               write(56,*) 'SET LIMIT X -10  10'
               write(56,*) 'SET LIMIT Y -17.5  17.5' 
c               print *,i1,icn
c               i1old=i1
               write(56,*) 'SET SYMBOL 9O'  
               write(56,*) 'SET ORDER X Y'
               do kk=1,icn-1 
c                 print *,'1',z(kk)
c* Non bending plane tracker hits 
                 if((z(kk).gt.-2.0.and. z(kk).lt.-1.0).or. 
     &             (z(kk).gt.-18.0.and.z(kk).lt.-17.0).or.
     &             (z(kk).lt.-21.0.and.z(kk).gt.-22.0)) then
c                    print *,'2',z(kk)
c* move mag bore center to 0,0,0 for plotting purposes
                     write(56,*) x(kk),z(kk)+9.5
                 endif              
               enddo 
               write(56,*) 'plot'


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
               do kk=1,icn-1

C                 if(z(kk).gt.0.0.or. 
c* bending plane tracker hits 
                 if(
     &             (z(kk).gt.-4.0.and.z(kk).lt.-3.0).or.
     &             (z(kk).gt.-10.0.and.z(kk).lt.-9.0).or.
     &             (z(kk).gt.-16.0.and.z(kk).lt.-15.0).or.
     &             (z(kk).gt.-20.0.and.z(kk).lt.-19.0)) then
C     &             (z(kk).lt.-18.0)) then
c* move mag bore center to 0,0,0
                     write(56,*) y(kk),z(kk)+9.5
                 endif              
               enddo 
               write(56,*) 'plot'

               endif

c* since we already read the first row of the next event icn=1, not icn=0
               icn=1
               x(icn)=f2  
               y(icn)=f3  
               z(icn)=f4  


            endif
            enddo

            end
