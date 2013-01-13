c
c  This tests the parameter file reader and will ultimately become the
c  version 1.0 tuneup.f program.
c
      subroutine tuneup
c
      parameter (nhead=400)
      include 'TUNEABLE'
      common/HST/beta8
      common/perfs/pkprflo,pkprfhi
      character*80 prompt,header(nhead),comment,holditem
      character*20 keyword
      character*1 item(80)
      logical autoscale,autothresh
      integer*4 i4
      dimension hold(7)
c
      do 500 i=1,nff
      nfi = lenc(files(i))
      call charinit(files(i),nfi)
      nfl = lenc(flags(i))
      call charinit(flags(i),nfl)
 500  continue
c
c  Hardwire some fossilized parameters for now.
c
      cmin = 1.0
      factor = 1.5
      asprat = 1.0
      lperchip = 0
      n0left = 0
      n0right = 0
c
c  Ask for the modification parameter file name.
c
      prompt = 'Enter modification parameters file name'
      call opens(41,prompt,0)
      call paramfile(header,nlines)
c
c  First, read the file names and file usage flags.  These variables
c  will be transmitted to the outside world in the array FILES.
c
c  Read the input image name.  THE NAME MUST EXIST.  If not, it is
c  requested.
c
      call charinit(keyword,20)
      keyword = 'IMAGE_IN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
 211  continue
      if(nr.gt.0) then
      call readchar(item,nr,files(1))
      else
      prompt = 'Enter input image name: '
      call query(prompt)
      read(5,999) files(1)
 999  format(a)
c
      nr = lenc(files(1))
      call writechar(item,nr,files(1))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      end if
c
c  Make sure this exists.  If not, ask explicitly.
c
      call opena(50,files(1),0,ierr)
      close(unit=50)
      if(ierr.eq.1) then
      nr = 0
      print *,'Input image does not exist!'
      go to 211
      end if
c
c  Determine if an output picture file is to be saved.
c
      flags(5)(1:3) = 'NO '
      call charinit(keyword,20)
      keyword = 'IMAGE_OUT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      if(nr.gt.0) then
      call readchar(item,nr,files(2))
      flags(5)(1:3) = 'YES'
      end if
c
c  Read input objects list.  Leave null if none is given.
c
      call charinit(keyword,20)
      keyword = 'OBJECTS_IN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      flags(6)(1:3) = 'NO '
      if(nr.gt.0) then
      call readchar(item,nr,files(3))
      flags(6)(1:3) = 'YES'
      end if
c
c  The output objects file name MUST EXIST.  If it is not in the 
c  parameter file, it is requested.
c
      call charinit(keyword,20)
      keyword = 'OBJECTS_OUT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      if(nr.gt.0) then
      call readchar(item,nr,files(4))
      else
      prompt = 'Enter output objects file name: '
      call query(prompt)
      read(5,999) files(4)
c
      nr = lenc(files(4))
      call writechar(item,nr,files(4))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
      end if
c
c  Ask for the shadow file name if it is to be saved.  If no name 
c  is provided, the output file will get a default designation.
c
      flags(4)(1:3) = 'NO '
      call charinit(keyword,20)
      keyword = 'SHADOWFILE_OUT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      if(nr.gt.0) then
      call readchar(item,nr,files(5))
      flags(4)(1:3) = 'YES'
      end if
c
c  Figure out about an input shadow file if desired.
c
      if(flags(6)(1:1).eq.'Y'.or.flags(6)(1:1).eq.'y') then
c
      flags(7)(1:3) = 'NO '
      call charinit(keyword,20)
      keyword = 'SHADOWFILE_IN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      if(nr.gt.0) then
      call readchar(item,nr,files(7))
      flags(7)(1:3) = 'YES'
      end if
c
      end if
c
c  Ask for the log file name if it is to be saved.  If no name 
c  is provided, the output file will get a default designation.
c
      call charinit(keyword,20)
      keyword = 'LOGVERBOSITY'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,lverb)
c
      if(lverb.gt.0) then
      call charinit(keyword,20)
      keyword = 'LOGFILE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      if(nr.gt.0) then
      call readchar(item,nr,files(6))
      else
      files(6) = 'logfile.dat'
      nr = lenc(files(6))
      call writechar(item,nr,files(6))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      end if
      end if
c
c  Now read the psf and sky flags.  If no parameters are present, the
c  defaults shown below are assumed.
c
      call charinit(keyword,20)
      keyword = 'PSFTYPE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
c
      do 822 i=1,nr
 822  holditem(i:i) = item(i)
      call upper(holditem)
      do 823 i=1,nr
 823  item(i) = holditem(i:i)
c
      if(nr.eq.0) then
      flags(1) = 'PGAUSS'
      nr = lenc(flags(1))
      call writechar(item,nr,flags(1))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
      else
      call readchar(item,nr,flags(1))
      call upper(flags(1)(1:nr))
        if(flags(1)(1:5).ne.'PGAUS') then
 654      prompt = 'Enter PSF type: '
          call query(prompt)
          read(5,999) flags(1)
c
      nr = lenc(flags(1))
      call writechar(item,nr,flags(1))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
        if(flags(1)(1:5).ne.'PGAUS') then
        print *,'Valid type is PGAUSS.  Try again.'
            go to 654
          end if
        end if
      end if
c
      call charinit(keyword,20)
      keyword = 'SKYTYPE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
c
      do 832 i=1,nr
 832  holditem(i:i) = item(i)
      call upper(holditem)
      do 833 i=1,nr
 833  item(i) = holditem(i:i)
c
      if(nr.eq.0) then
      flags(2) = 'PLANE'
      nr = lenc(flags(2))
      call writechar(item,nr,flags(2))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
      else
      call readchar(item,nr,flags(2))
      call upper(flags(2)(1:nr))
        if(flags(2)(1:5).ne.'PLANE'.and.
     *     flags(2)(1:5).ne.'MEDIA'.and.
     *     flags(2)(1:5).ne.'HUBBL') then
 644      prompt = 'Enter sky type: '
          call query(prompt)
          read(5,999) flags(2)
c
      nr = lenc(flags(2))
      call writechar(item,nr,flags(2))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
        if(flags(2)(1:5).ne.'PLANE'.and.
     *     flags(2)(1:5).ne.'HUBBL') then
        print *,'Valid types are PLANE, MEDIAN and HUBBLE.  Try again.'
            go to 644
          end if
        end if
      end if
c
      call charinit(keyword,20)
      keyword = 'OBJTYPE_IN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
c
      do 842 i=1,nr
 842  holditem(i:i) = item(i)
      call upper(holditem)
      do 843 i=1,nr
 843  item(i) = holditem(i:i)
c
      if(nr.eq.0) then
        flags(8) = 'INTERNAL'
c
      nr = lenc(flags(8))
      call writechar(item,nr,flags(8))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      else
        call readchar(item,nr,flags(8))
        call upper(flags(8)(1:nr))
        if(flags(8)(1:5).ne.'COMPL'.and.flags(8)(1:5).ne.'INTER') then
 554      prompt = 'Enter input object file format type: '
          call query(prompt)
          read(5,999) flags(8)
c
      nr = lenc(flags(8))
      call writechar(item,nr,flags(8))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
        if(flags(8)(1:5).ne.'COMPL'.and.flags(8)(1:5).ne.'INTER') then
        print *,'Valid types are COMPLETE and INTERNAL.  Try again.'
            go to 554
          end if
        end if
      end if
c
      call charinit(keyword,20)
      keyword = 'OBJTYPE_OUT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
c
      do 852 i=1,nr
 852  holditem(i:i) = item(i)
      call upper(holditem)
      do 853 i=1,nr
 853  item(i) = holditem(i:i)
c
      if(nr.eq.0) then
        flags(3) = 'INTERNAL'
c
      nr = lenc(flags(3))
      call writechar(item,nr,flags(3))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      else
       call readchar(item,nr,flags(3))
        call upper(flags(3)(1:nr))
        if(flags(3)(1:5).ne.'COMPL'.and.
     *    flags(3)(1:5).ne.'INTER'.and.flags(3)(1:5).ne.'INCOM') then
 544      prompt = 'Enter output object file format type: '
          call query(prompt)
          read(5,999) flags(3)
c
      nr = lenc(flags(3))
      call writechar(item,nr,flags(3))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
          if(flags(3)(1:5).ne.'COMPL'.and.
     *      flags(3)(1:5).ne.'INTER'.and.flags(3)(1:5).ne.'INCOM') then
            print *,'Valid types are COMPLETE, INTERNAL and INCOMPLETE.
     *  Try again.'
            go to 544
          end if
        end if
      end if
c
c
      call charinit(keyword,20)
      keyword = 'IMAGE_FORMAT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
c
      do 862 i=1,nr
 862  holditem(i:i) = item(i)
      call upper(holditem)
      do 863 i=1,nr
 863  item(i) = holditem(i:i)
c
      if(nr.eq.0) then
        flags(9) = 'FITS'
c
      nr = lenc(flags(9))
      call writechar(item,nr,flags(9))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      else
       call readchar(item,nr,flags(9))
        call upper(flags(9)(1:nr))
        if(flags(9)(1:3).ne.'FIT'.and.flags(9)(1:3).ne.'LOC'.and.
     *    flags(9)(1:3).ne.'PDM'.and.flags(9)(1:3).ne.'DTA') then
 564      prompt = 'Enter input image data format type: '
          call query(prompt)
          read(5,999) flags(9)
c
      nr = lenc(flags(9))
      call writechar(item,nr,flags(9))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
            if(flags(9)(1:3).ne.'FIT'.and.flags(9)(1:3).ne.'LOC'.and.
     *      flags(9)(1:3).ne.'DTA'.and.flags(9)(1:3).ne.'PDM') then
      print *,'Valid types are FITS, DTA, PDM and LOCAL.  Try again.'
            go to 564
          end if
        end if
      end if
c
c  Now, unit 42 is the output parameter file.  The long job now is to 
c  extract the necessary keywords one by one. 
c
      call charinit(keyword,20)
      keyword = 'TILT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,tilt)
c
      call charinit(keyword,20)
      keyword = 'AXIS_RATIO'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,ar)
c
      call charinit(keyword,20)
      keyword = 'FWHM'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,fwhm)
      fwhm = fwhm*1.2
c
      tilt = tilt/57.29578
      gmajwid = (fwhm/2.3548)**2
      gxwid = gmajwid*(cos(tilt)**2 + (ar*sin(tilt))**2)
      gywid = gmajwid*((ar*cos(tilt))**2 + sin(tilt)**2)
c      gxwid = float(nint(10.0*gxwid))/10.0
c      gywid = float(nint(10.0*gywid))/10.0
      fwhmx = 2.3548*sqrt(gxwid)
      fwhmy = 2.3548*sqrt(gywid)
      if(lverb.gt.10) write(6,*) 'fwhmx,fwhmy = ',fwhmx,fwhmy
c
      call charinit(keyword,20)
      keyword = 'SKY'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,skyguess)
c
c  Update some fossils.
c
      ava(1) = skyguess
      ava(2) = 0.0
      ava(3) = 0.0
      ava(4) = 0.0
      ava(5) = gxwid
      ava(7) = gywid
      ava(6) = 0.01/sqrt(gxwid*gywid)
c
      call charinit(keyword,20)
      keyword = 'EPERDN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,eperdn)
c
      call charinit(keyword,20)
      keyword = 'RDNOISE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,rnoise)
c
c  Now determine if autoscaling is desired.
c
      call charinit(keyword,20)
      keyword = 'AUTOSCALE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      autoscale = .false.
      if(item(1).eq.'y'.or.item(1).eq.'Y') autoscale = .true.
c
      if(autoscale) then
c
      call charinit(keyword,20)
      keyword = 'SCALEFITBOX'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,scalefb)
c
      call charinit(keyword,20)
      keyword = 'FITBOXMIN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,fbmin)
c
      call charinit(keyword,20)
      keyword = 'SCALEAPBOX'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,scaleab)
c
      call charinit(keyword,20)
      keyword = 'APBOXMIN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,abmin)
c
      call charinit(keyword,20)
      keyword = 'SCALEMASKBOX'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,scalemb)
c
      call charinit(keyword,20)
      keyword = 'AMASKBOXMIN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,ambmin)
c
c  Apply these factors to the relevant quantities.
c
      i4 = nint(amax1(fwhmx*scalefb,fbmin))
      if(mod(i4,2).eq.0) i4 = i4 + 1
      irect(1) = i4
c
      call charinit(keyword,20)
      keyword = 'NFITBOX_X'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call writeint(item,nr,i4)
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      i4 = nint(amax1(fwhmy*scalefb,fbmin))
      if(mod(i4,2).eq.0) i4 = i4 + 1
      irect(2) = i4
c
      call charinit(keyword,20)
      keyword = 'NFITBOX_Y'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call writeint(item,nr,i4)
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      arect(1) = real(nint(amax1(fwhmx*scaleab,abmin)))
      if(mod(int(arect(1)),2).eq.0) arect(1) = arect(1)+1.0
      arect(2) = nint(amax1(fwhmy*scaleab,abmin))
      if(mod(int(arect(2)),2).eq.0) arect(2) = arect(2)+1.0
c
      call charinit(keyword,20)
      keyword = 'APBOX_X'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call writereal(item,nr,arect(1))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      call charinit(keyword,20)
      keyword = 'APBOX_Y'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call writereal(item,nr,arect(2))
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      ixby2 = nint(amax1(fwhmx*scalemb,ambmin))
      if(mod(ixby2,2).eq.0) ixby2 = ixby2 + 1
      ixby2 = (ixby2 - 1)/2
      iyby2 = nint(amax1(fwhmy*scalemb,ambmin))
      if(mod(iyby2,2).eq.0) iyby2 = iyby2 + 1
      iyby2 = (iyby2 - 1)/2
c
      call charinit(keyword,20)
      keyword = 'MASKBOX_X'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call writeint(item,nr,ixby2)
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      call charinit(keyword,20)
      keyword = 'MASKBOX_Y'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call writeint(item,nr,iyby2)
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      else
c
c  Else, read the parameters directly from the file.
c
      call charinit(keyword,20)
      keyword = 'NFITBOX_X'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,i4)
      irect(1) = i4
c
      call charinit(keyword,20)
      keyword = 'NFITBOX_Y'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,i4)
      irect(2) = i4
c
      call charinit(keyword,20)
      keyword = 'APBOX_X'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,arect(1))
c
      call charinit(keyword,20)
      keyword = 'APBOX_Y'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,arect(2))
c
      call charinit(keyword,20)
      keyword = 'MASKBOX_X'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,ixby2)
c
      call charinit(keyword,20)
      keyword = 'MASKBOX_Y'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,iyby2)
c
      if(mod(ixby2,2).eq.0) ixby2 = ixby2 + 1
      ixby2 = (ixby2 - 1)/2
      if(mod(iyby2,2).eq.0) iyby2 = iyby2 + 1
      iyby2 = (iyby2 - 1)/2
c
      end if
c
c  Ask if positions will be fixed.
c
      call charinit(keyword,20)
      keyword = 'FIXPOS'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      fixpos = .false.
      if(item(1).eq.'y'.or.item(1).eq.'Y') fixpos = .true.
c
c  Now, ask if auto thresholding is to be done.
c
      call charinit(keyword,20)
      keyword = 'AUTOTHRESH'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      autothresh = .false.
      if(item(1).eq.'y'.or.item(1).eq.'Y') autothresh = .true.
c
      if(autothresh) then
c
c  Read the needed scaling quantities.
c
      call charinit(keyword,20)
      keyword = 'SIGMAIBOTTOM'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,sigbot)
c
      call charinit(keyword,20)
      keyword = 'SIGMATHRESHMIN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,sigthresh)
c
c  Now, calculate the sigma of the sky and apply these parameters.
c
      sigskyguess = sqrt(eperdn*skyguess + rnoise**2)/eperdn
      ibot = skyguess - sigbot*sigskyguess
      tmin = sigthresh*sigskyguess
c
      call charinit(keyword,20)
      keyword = 'IBOTTOM'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call writeint(item,nr,ibot)
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      call charinit(keyword,20)
      keyword = 'THRESHMIN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call writereal(item,nr,tmin)
      call getcomment(header(line),comment,ncomm)
      call writeitem(header(line),keyword,nr,itype,item,comment,ncomm)
c
      else
c
c  Else, read the parameters directly from the file.
c
      call charinit(keyword,20)
      keyword = 'IBOTTOM'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,ibot)
c
      call charinit(keyword,20)
      keyword = 'THRESHMIN'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,tmin)
c
      end if
c
      nthpix = 0 
c
c      if nthpix is carried as 0 DoPHOT will change it to sqrt(nfast)
c      nfast is not known till data file is read, so it cannot be set 
c      here.
c
      if(flags(2)(1:5) .eq. 'MEDIA') then
c
        call charinit(keyword,20)
        keyword = 'JHXWID'
        call readitem(nlines,header,keyword,nr,itype,item,line)
        call readint(item,nr,jhxwid)
c
        call charinit(keyword,20)
        keyword = 'JHYWID'
        call readitem(nlines,header,keyword,nr,itype,item,line)
        call readint(item,nr,jhywid)
c
        call charinit(keyword,20)
        keyword = 'MPREC'
        call readitem(nlines,header,keyword,nr,itype,item,line)
        call readint(item,nr,mprec)
c
        call charinit(keyword,20)
        keyword = 'NTHPIX'
        call readitem(nlines,header,keyword,nr,itype,item,line)
        call readint(item,nr,nthpix)
c
        call charinit(keyword,20)
        keyword = 'NPSFFIT'
        call readitem(nlines,header,keyword,nr,itype,item,line)
        call readint(item,nr,npsffit)
c
        if(jhxwid .le. 0) jhxwid = max(2,nint(4.0*sqrt(gxwid)))
        if(jhywid .le. 0) jhywid = max(2,nint(4.0*sqrt(gywid)))
        if(mprec .le. 0) mprec = tmin/4.
        mprec = max(1,mprec)
c
      end if

c
c  Now, read all remaining numeric parameters.  There's still a bunch.
c
      call charinit(keyword,20)
      keyword = 'ITOP'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,itop)
c
      call charinit(keyword,20)
      keyword = 'THRESHMAX'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,tmax)
c
      call charinit(keyword,20)
      keyword = 'THRESHDEC'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,tfac)
c
      call charinit(keyword,20)
      keyword = 'RESIDNOISE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,fac)
c
      call charinit(keyword,20)
      keyword = 'FOOTPRINT_NOISE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,xpnd)
c
      call charinit(keyword,20)
      keyword = 'NPHSUB'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nphsub)
c
      call charinit(keyword,20)
      keyword = 'NPHOB'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nphob)
c
      call charinit(keyword,20)
      keyword = 'ICRIT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,icrit)
c
      call charinit(keyword,20)
      keyword = 'CENTINTMAX'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,cmax)
c
      call charinit(keyword,20)
      keyword = 'CTPERSAT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,ctpersat)
c
      call charinit(keyword,20)
      keyword = 'STARGALKNOB'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,stograt)
c
      call charinit(keyword,20)
      keyword = 'STARCOSKNOB'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,discrim)
c
      call charinit(keyword,20)
      keyword = 'SNLIM7'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,crit7)
      crit7 = crit7**2
c
      call charinit(keyword,20)
      keyword = 'PKPRFHI'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,pkprfhi)
c
      call charinit(keyword,20)
      keyword = 'PKPRFLO'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,pkprflo)
c
      call charinit(keyword,20)
      keyword = 'SNLIM'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,snlim)
c
      call charinit(keyword,20)
      keyword = 'SNLIMMASK'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,bumpcrit)
c
      call charinit(keyword,20)
      keyword = 'SNLIMCOS'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,sn2cos)
      sn2cos = sn2cos**2
c
      call charinit(keyword,20)
      keyword = 'NBADLEFT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nbadleft)
c
      call charinit(keyword,20)
      keyword = 'NBADRIGHT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nbadright)
c
      call charinit(keyword,20)
      keyword = 'NBADTOP'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nbadtop)
c
      call charinit(keyword,20)
      keyword = 'NBADBOT'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nbadbot)
c
      call charinit(keyword,20)
      keyword = 'NFITITER'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nit)
c
      call charinit(keyword,20)
      keyword = 'NPARAM'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,npar)
c
      call charinit(keyword,20)
      keyword = 'NFITMAG'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nfit1)
c
      call charinit(keyword,20)
      keyword = 'NFITSHAPE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,nfit2)
c
      call charinit(keyword,20)
      keyword = 'NFITBOXFIRST_X'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,i4)
      krect(1) = i4
c
      call charinit(keyword,20)
      keyword = 'NFITBOXFIRST_Y'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readint(item,nr,i4)
      krect(2) = i4
c
      call charinit(keyword,20)
      keyword = 'CHI2MINBIG'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,chicrit)
c
      call charinit(keyword,20)
      keyword = 'XTRA'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,xtra)
c
      call charinit(keyword,20)
      keyword = 'SIGMA1'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,sig(1))
c
      call charinit(keyword,20)
      keyword = 'SIGMA2'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,sig(2))
c
      call charinit(keyword,20)
      keyword = 'SIGMA3'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,sig(3))
c
      call charinit(keyword,20)
      keyword = 'ENUFF4'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,enuff4)
c
      call charinit(keyword,20)
      keyword = 'ENUFF7'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,enuff7)
c
      call charinit(keyword,20)
      keyword = 'COSOBLSIZE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,widobl)
c
      call charinit(keyword,20)
      keyword(1:6) = 'RELACC'
      do 600 i=1,7
      write(keyword(7:7),'(i1)') i
      call readitem(nlines,header,keyword(1:7),nr,itype,item,line)
      call readreal(item,nr,hold(i))
 600  continue
c
      acc(1) = hold(1)
      acc(2) = hold(4)
      acc(3) = hold(2)
      acc(4) = hold(3)
      acc(5) = hold(5)
      acc(6) = hold(6)
      acc(7) = hold(7)
c
      call charinit(keyword,20)
      keyword(1:6) = 'ABSLIM'
      do 601 i=1,7
      write(keyword(7:7),'(i1)') i
      call readitem(nlines,header,keyword(1:7),nr,itype,item,line)
      call readreal(item,nr,hold(i))
 601  continue
c
      alim(1) = hold(1)
      alim(2) = hold(4)
      alim(3) = hold(2)
      alim(4) = hold(3)
      alim(5) = hold(5)
      alim(6) = hold(6)
      alim(7) = hold(7)
c
      call charinit(keyword,20)
      keyword = 'BETA4'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,beta4)
c
      call charinit(keyword,20)
      keyword = 'BETA6'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,beta6)
c
      call charinit(keyword,20)
      keyword = 'BETA8'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,beta8)
c
      call charinit(keyword,20)
      keyword = 'PIXTHRESH'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,pixthresh)
c
      call charinit(keyword,20)
      keyword = 'APMAG_MAXERR'
      call readitem(nlines,header,keyword,nr,itype,item,line)
      call readreal(item,nr,apmagmaxerr)
c
c
c     Ask for the aperture correction parameters.  If no aperture correction
c     output file is given, then there is no need to read the parameters.
c     
      flags(10)(1:3) = 'NO '
      call charinit(keyword,20)
      keyword = 'APCORRFILE'
      call readitem(nlines,header,keyword,nr,itype,item,line)
c     
      if(nr.gt.0) then
         call readchar(item,nr,files(8))
         flags(10)(1:3) = 'YES'
c     
         call charinit(keyword,20)
         keyword = 'NAPERTURES'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readint(item,nr,napertures)
         napertures = min0(napertures,napmax)
c     
         call charinit(keyword,20)
         keyword = 'APMAX'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readreal(item,nr,apmax)
c     
         call charinit(keyword,20)
         keyword = 'APSKYMIN'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readreal(item,nr,apskymin)
c     
         call charinit(keyword,20)
         keyword = 'APSKYMAX'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readreal(item,nr,apskymax)
c     
         call charinit(keyword,20)
         keyword = 'APERRMAX'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readreal(item,nr,aperrmax)
c
c ....  now do for 2nd APCORR measurement as well:
c     
         call charinit(keyword,20)
         keyword = 'APCORRFILE2'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readchar(item,nr,files(9))
c
         call charinit(keyword,20)
         keyword = 'NAPERTURES2'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readint(item,nr,napertures2)
         napertures = min0(napertures,napmax)
c     
         call charinit(keyword,20)
         keyword = 'APMAX2'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readreal(item,nr,apmax2)
c     
         call charinit(keyword,20)
         keyword = 'APSKYMIN2'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readreal(item,nr,apskymin2)
c     
         call charinit(keyword,20)
         keyword = 'APSKYMAX2'
         call readitem(nlines,header,keyword,nr,itype,item,line)
         call readreal(item,nr,apskymax2)
c     
c .. done getting params for 2nd apcorr pass -- uses same APERMAX

      end if
c     
c
c
      call paramwrite(header,nlines)
      return
      end
