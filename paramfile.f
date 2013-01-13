c     
c  This subroutine asks the user for the parameter files.  If the modified file
c  does not exist, the default file is transferred directly to the output file.
c  A program (MKDEFAULT) is supplied in the DoPHOT directory to generate a 
c  valid default parameter file should one not exist already.
c     
      subroutine paramfile(defheader,nldef)
c
      include 'TUNEABLE'
c
      parameter (nhead=400)
      character*80 defheader(1),modheader(nhead)
      character*80 prompt,comment,hold
      character*20 keyword
      character*1 item(80)
c
c  First of all, find out if the names of the default and output parameter
c  files can be found in unit 41.  If not, ask for these.
c     
      i = 1
 30   continue
      read(41,999,err=230,end=230) modheader(i)
 999  format(a)
      i = i + 1
      go to 30
 230  continue
      nlmod = i - 1
c
c  Ask for default parameters file.  UNIT 40.
c
      call charinit(keyword,20)
      keyword = 'PARAMS_DEFAULT'
      call readitem(nlmod,modheader,keyword,nr,itype,item,ll)
c
      if(nr.gt.0) then
c
      do 31 i=1,nr
      hold(i:i) = item(i)
 31   continue
      do 311 i = nr+1,80
      hold(i:i) = ''
 311  continue
c
      write(*,999) 'PARAMS_DEFAULT file is: ',  hold
c 
      ierr = 0
      call opena(40,hold,0,ierr)
      if(ierr.eq.1) then
      print *,'Default parameters file not found!'
      nr = 0
      end if
c
      end if
c
      if(nr.eq.0) then
      prompt = 'Enter default parameters file name'
      call opens(40,prompt,0)
      end if
c
c  Loop over the default parameter file and look for those keywords in the
c  modified parameter file.
c
      i = 1
 11   continue
      read(40,999,err=211,end=211) defheader(i)
c
      call charinit(keyword,20)
      call findkey(defheader(i),keyword,lenkey)
c
c  If there is no key, this is a full-line comment and is not written to the 
c  output file.
c
      if(lenkey.eq.0) go to 11
c
c  See if you can find the keyword in the modified parameter file.
c 
      call readitem(nlmod,modheader,keyword,nr,itype,item,ll)
c
c  Did you find it?  If not, write the default header name without change.
c
      if(nr.gt.0.or.itype.eq.2) then
c
c  Else, read the full line, and write it out to the appropriate output file.
c
      call getcomment(modheader(ll),comment,ncomm)
      if(ncomm.eq.0) call getcomment(defheader(i),comment,ncomm)
      call writeitem(modheader(ll),keyword,nr,itype,item,comment,
     *     ncomm)
c
      defheader(i) = modheader(ll)
      end if
c
      i = i + 1
      go to 11
c
 211  nldef = i - 1
      close(unit=40)
      close(unit=41)
c
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Write current parameters to unit 42.
c
      subroutine paramwrite(header,nlines)
c
      parameter(nhead=400)
      character*(*) header(nhead)
      character*80 hold,prompt
      character*20 keyword
      character*1 item(80)
c
c  Ask for output parameters file.  UNIT 42.
c
      call charinit(keyword,20)
      keyword = 'PARAMS_OUT'
      call readitem(nlines,header,keyword,nr,itype,item,ll)
c
      if(nr.gt.0) then
c
      do 32 i=1,nr
      hold(i:i) = item(i)
 32   continue
      do 321 i = nr+1, 80
      hold(i:i) = ''
 321  continue 
c
      ierr = 0
      call opena(42,hold,2,ierr)
      if(ierr.eq.1) then
      print *,'Error opening output parameters file.'
      nr = 0
      end if
c
      end if
c
      if(nr.eq.0) then
      prompt = 'Enter output parameters file name'
      call opens(42,prompt,2)
      end if
c
      do 10 i=1,nlines
 10   write(42,999) header(i)
c
 999  format(a)
c
      close(unit=42)
      return
      end
