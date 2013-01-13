      subroutine apcorrxtra(data,noise,onestar)
C     
C     Changed the output format to include the fit magnitude Dec 6, 1990
C     fit mag changed to zero pt of 25 , does not effect apcorr derived
c     feb 22.
c     Saturated stars excluded.
c     
c     changde output format so can still be read when scans longer than
c     9999 

c     apr95,I am happy with the trimmed mean sky so will continue to use it
c     and comment out the other apcor calcs, also a quicker sort was included
c     and the fit

c     Subroutine to determine aperture corrections and curves of growth.
c     
      include 'TUNEABLE'
c
      parameter (napple=5)     
      common /search/ nstot, thresh
      common /undergnd/ nfast,nslow
      common /starlist/ starpar(npmax,nsmax),imtype(nsmax),
     *     shadow(npmax,nsmax), errshad(npmax, nsmax)
      common /aperlist/ apple(napple,nsmax)
c     
      integer*2 data(nfast,nslow)
      integer noise(nfast,nslow)
c     
      real apcorr(napmax),apsum(napmax),aparea(napmax),trimx(10000)
c     
      logical badpix
c  
	integer lcnt
	real*4 abflatlim, flatg, stepsky, flatgold
c     
      external onestar
      data magic / 2147483547 /
      data iadd,isub / 1,-1 /
	abflatlim = 0.05
c
      write(6,*) napertures2, apmax2, apskymin2, apskymax2
c... get area of maximum aperture in pixels
	stepsky = 3.142*apmax2*apmax2
c... calculate 0.2-sig noise perpixel and divide by sqrt(max no. of pixels)
c....    then convert to DN ..
      stepsky = sqrt( skyguess*eperdn + rnoise*rnoise )/sqrt(stepsky)
        stepsky = stepsky*0.2
	stepsky = stepsky/eperdn
	write(*,*) 'stepsky = ', stepsky
c     
c     Now, loop through the stars.  Deal only with 1's with formal errors
c     better than some predetermined limit.
c     
      iunit = 30
      call opena(iunit,files(9),2,ierr)
c     
      do 15 i=1,nstot
c     
         if(imtype(i).ne.1 .and. imtype(i) .ne. 11) go to 15
         if(starpar(2,i).gt.itop) go to 15
         if(apple(4,i).gt.aperrmax) go to 15
c     
c     Ignore if the star is too near the edge.
c     
         xcenter = starpar(3,i)
         ycenter = starpar(4,i)
         ixcenter = int(xcenter)
         iycenter = int(ycenter)
c     
c        modify so can have stars closer to edge if all star + a bit of sky
c        iapsize = max(int(apskymax2),int(apmax2))
         iapsize = max(int(apskymin2),int(apmax2))
c     
         ixlow = ixcenter - iapsize
         if(ixlow.le.nbadleft) go to 15
         ixhigh = ixcenter + iapsize + 1
         if(ixhigh.gt.nfast-nbadright) go to 15      
c     
         iylow = iycenter - iapsize
         if(iylow.le.nbadbot) go to 15
         iyhigh = iycenter + iapsize + 1
         if(iyhigh.gt.nslow-nbadtop) go to 15      
c     
c     Add the star to the picture using the starpar parameters.
c     
         call addstar(onestar,data,noise,nfast,nslow,
     *        starpar(1,i),iadd)
c     
c     Now begin a loop of the pixels within the ranges determined above.
c     
         skywt = 0.0
         skysum = 0.0
         skyct = 0.0
         skysum_nwt = 0.0
         it=0
c     
         do 25 ii=1,napertures2
            aparea(ii) = 0.0
            apsum(ii) = 0.0
 25      continue
c     
         do 16 jy=iylow,iyhigh
            do 17 jx=ixlow,ixhigh
c     
               badpix = noise(jx,jy).ge.magic
c     
c     Distance from star center.
c     
               x = float(jx)
               y = float(jy)
               dist = sqrt((xcenter-x)**2 + (ycenter-y)**2)
               rdist = dist - 0.5
c     
               do 18 k=1,napertures2
                  app = (float(k)/float(napertures2))*apmax2
                  fractn = amax1(0.0,amin1(1.0,app-rdist))
                  if(fractn.gt.0.0.and.badpix) go to 215
                  aparea(k) = aparea(k) + fractn
                  apsum(k) = apsum(k) + fractn*data(jx,jy)
 18            continue
c     
c     Now the sky aperture.
c     
               fractnout = amax1(0.0,amin1(1.0,apskymax2-rdist))
               rdist = dist + 0.5
               fractnin = amax1(0.0,amin1(1.0,rdist-apskymin2))
               fractn = amin1(fractnin,fractnout)
               if(fractn.gt.0.0.and.badpix) go to 17
c     
c               skysum = skysum + data(jx,jy)*fractn/noise(jx,jy)
c               skywt = skywt + fractn/noise(jx,jy)
c               skyct = skyct + 1*fractn
c               skysum_nwt = skysum_nwt + data(jx,jy)*fractn
               if(fractn.ne.0.0)then
                  it = it + 1
                  trimx(it) = data(jx,jy)
               end if
c     
 17         continue
 16      continue      
c     
c     calc fit mag

c         if(skywt.eq.0.0) go to 215
         starlum = elarea(starpar(5,i),starpar(6,i),starpar(7,i))
         starlum = 6.283185*starlum*starpar(2,i)
C     
         if(starlum.gt.0) then
            fitmag = 30.0 - 2.5*alog10(starlum)
         else
            fitmag = 99.0
         end if
C     
c     
c     Convert counts to aperture corrections in mags and write out results to 
c     a file.
c     
c            avgsky = skysum/skywt
c         do 19 ii=1,napertures2
c            totcnts = apsum(ii) - avgsky*aparea(ii)
c     
c           if(totcnts.le.0.0) then
c               apcorr(ii) = 99.999
c            else
c               apcorr(ii) = 2.5*alog10(starlum/totcnts)
c            end if
c     
 19      continue
c     
c     Current output until I can think of something better.
c     
c         write(iunit,100) i,starpar(3,i),starpar(4,i),
c     *        avgsky, fitmag, (apcorr(k),k=1,napertures2)
c 100     format(i6,2f8.2,f7.1,1x,f7.3,<napertures2>f7.3)

c     do apertures again for non weighted sky usuing trimmed mean
         avgsknw = skysum_nwt/skyct
         call trimsig(it,trimx,trimsky)
         lcnt = 0
	 delstep = 0.0
         flatgold = 10.
876      if(lcnt .ne. 0) flatgold = flatg
         do 20 ii=1,napertures2
            totcnts_nwt = apsum(ii) - trimsky*aparea(ii)
c     
            if(totcnts_nwt.le.0.0) then
               apcorr(ii) = 99.999
            else
               apcorr(ii) = 2.5*alog10(starlum/totcnts_nwt)
            end if
c     
 20      continue
c
c**** OK here is the sky loop.....
c... note that it works only when napertures2 .gt. 7
c
	if(napertures2 .gt. 7 .and. lcnt .le. 250)  then 
          flatg = apcorr(napertures2) - apcorr(napertures2-3)
          delstep = abs(flatgold) - abs(flatg)
          if(lcnt .eq. 0) write(6,*) flatg
	  if(flatg .lt. -1.*abflatlim .or. (delstep .gt. 0.0
     $          .and. flatg .lt. 0.0) ) then 
            trimsky  = trimsky + stepsky
	    lcnt  = lcnt + 1
	    go to 876
	  end if 
          if(flatg .gt. abflatlim .or. (delstep .gt. 0.0 .and. 
     $          flatg .gt. 0.0) )  then
	    trimsky = trimsky - stepsky
	    lcnt = lcnt + 1 
	    go to 876
	  end if        
	  write(6,*) lcnt, flatg
	end if
c
c
C********    END sky loop ********
c     
       if(apple(1,i) .le. 0.0) then 
          dopapmag = 99.999
       else 
          dopapmag = -2.5*alog10(apple(1,i)) + 30.
       end if
c     write out non wt sky mags
         write(iunit,101) i,starpar(3,i),starpar(4,i),
     *        trimsky, fitmag, (apcorr(k),k=1,napertures2), 
     *        apple(4,i), dopapmag, apple(5,i), starpar(1,i)
 101     format(i6,2f9.2,f7.1,1x,f7.3, 8f7.3,3f7.2,f9.2)
 215     continue
c     
c     Subtract the star out again.
c     
         call addstar(onestar,data,noise,nfast,nslow,
     *        starpar(1,i),isub)
c     
 15   continue
c     
	write(*,*) 'stepsky = ', stepsky
      close(unit=iunit)
      return
      end




