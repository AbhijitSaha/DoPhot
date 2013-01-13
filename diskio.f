c
c  If you need to produce a customized reader, search for a string of about
c  ten asterisks in this file.
c
c  Subroutine to read a disk fits file of the type created by IRAF.  Can
c  handle fixed and variable length records (the latter up to 4096 bytes 
c  long).  IMPORTANT:  swab = 0 does not swap bytes;  swab = 1 does swap bytes.
c  Assumes short (16 bit) integers. Some relatively minor changes are
c  needed for 32 bit integers.
c
c  Routines in this file are !!!!!!!!!!!!!!!VMS SPECIFIC!!!!!!!!!!!!!!
c
      subroutine rffile(nheader,header,n1,n2,data,filename)
c
      include 'tuneable'
c
      logical*1 record(nfits),huge(ncmax*nrmax*2),temp1(2)
      character*1 string(nfits)
      character*4096 bigline
      character*80 hold,header(1),prompt,filetype
      character*(*) filename
      integer*2 data(1),temp2
      integer naxis(2)
      equivalence (temp1,temp2)
      equivalence (record,string)
c
      iatend = 0
c
      nfilename = lenc(filename)
   46 call opensgen(10,filename(1:nfilename),0,irec,filetype)
      if(irec.eq.0) then
      prompt = 'Bad input image file name; try again:'
      call query(prompt)
      read(5,998) nfilename,filename
  998 format(q,a) 
      go to 46
      end if
c
c  Read all the bytes into huge until you run out of input file.
c
      i = 1
      ist = 1
   15 continue
c
      if(filetype(1:3).eq.'VAR') then
      read(10,998,end=215,err=215) irec,bigline
      iend = ist + irec - 1
      read(bigline(1:irec),997) (huge(k),k=ist,iend)
      ist = ist + irec
      else
      ist = (i-1)*irec + 1
      iend = i*irec
      read(10,997,end=215,err=215) (huge(k),k=ist,iend)
  997 format(<irec>a1)
      i = i + 1
      end if
c
      go to 15
c
  215 continue
      close(unit=10)
c
c  Now, stuff the data into FITS-sized records.  First, get the header
c  information and find the end of the header.
c
      i = 1
      idata = 1
      iheadrec = 0
      iquit = 0
      nheader = 0
c
   16 continue
c
      ist = (i-1)*nfits + 1
      iend = i*nfits
c
      if(iatend.eq.1) then
      ntot = naxis(1)*naxis(2)*2
      if(iend-iheadrec*nfits.ge.ntot) then
      iend = ntot + iheadrec*nfits
      iquit = 1
      end if
      end if
c
      jj = 1
      do 25 j=ist,iend
      record(jj) = huge(j)
      jj = jj + 1
   25 continue
c
      if(iatend.eq.0) then
c
c  Find NAXIS1 and NAXIS2, and see if this record contains the END of
c  the header.
c
      do 26 j=1,36
      iaxis = 0
      ist = (j-1)*80 + 1
      iend = ist + 79
      nheader = nheader + 1
c
      kk = 1
      do 28 k=ist,iend
      header(nheader)(kk:kk) = string(k)
      kk = kk + 1
   28 continue
c
      if(header(nheader)(1:6).eq.'NAXIS1') iaxis = 1
      if(header(nheader)(1:6).eq.'NAXIS2') iaxis = 2
      if(header(nheader)(1:6).eq.'END   ') iatend = 1
c
      if(iaxis.ne.0) read(header(nheader)(10:80),*) naxis(iaxis)
c
   26 continue
c
      i = i + 1
      iheadrec = iheadrec + 1
      go to 16
      end if
c
c Now, if the header is already passed, this is the data.
c
      if(iatend.eq.1) then
c
      do 35 j=ist,iend,2
c
      if(swab.eq.0) then
      temp1(1) = huge(j)
      temp1(2) = huge(j+1)
      else if(swab.eq.1) then
      temp1(1) = huge(j+1)
      temp1(2) = huge(j)
      end if
c
      data(idata) = temp2
      idata = idata + 1
c
   35 continue
c
      if(iquit.eq.1) go to 216
      i = i + 1
      go to 16
      end if
c
  216 continue
c
      n1 = naxis(1)
      n2 = naxis(2)
c
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine wffile(nheader,header,n1,n2,data,filename)
c
c  Write fits disk file;  The VMS record lengths are 2880 bytes.
c
      include 'tuneable'
c
      logical*1 record(nfits),temp1(2)
      character*1 string(nfits)
      character*80 header(1),dummy
      character*(*) filename
      integer*2 data(1),temp2
      equivalence (temp1,temp2)
      equivalence (record,string)
c
c  The output file records will be 2880 bytes long.
c
      irec = nfits
      nfilename = lenc(filename)
      call opensgen(10,filename(1:nfilename),2,irec,dummy)
c
c  Now stuff the header values into the first ihead records.
c
      ihead = ((nheader-1)/36) + 1
c
      do 15 i=1,ihead
      kk = 1
      do 16 j=1,36
      nheader = j + (i-1)*36
      do 16 k=1,80
      string(kk) = header(nheader)(k:k)
      kk = kk + 1
   16 continue
c
c  Now, write string out to the file.
c
      write(10,997) (string(k),k=1,nfits)
  997 format(<nfits>a1)
c
   15 continue
c
c  Now that the header is done, it is time to write the data out.  Swap
c  bytes if required.  First, how many bytes and how many fits records.
c
      nbytetot = n1*n2*2
      nfitrec = ((nbytetot-1)/nfits) + 1
      nextra = nbytetot - (nfitrec-1)*nfits
c
      do 25 i=1,nfitrec
c
      if(i.lt.nfitrec) iend = nfits
      if(i.eq.nfitrec) iend = nextra
c
      do 26 j=1,nfits,2
      temp2 = 0
      if(j.le.iend) then
      idata = 1+(((i-1)*nfits+j)/2)
      temp2 = data(idata)
      end if
c
      if(swab.eq.0) then
      record(j) = temp1(1)
      record(j+1) = temp1(2)
      else if(swab.eq.1) then      
      record(j+1) = temp1(1)
      record(j) = temp1(2)
      end if
c
   26 continue
c
      write(10,997) (string(k),k=1,nfits)
   25 continue
c
      close(unit=10)
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine opensgen(unit,filename,irorw,reclength,rectype)
      character*(*) filename,rectype
      character*80 ccontrol
      integer reclength,unit
c
c open a file for input;  This uses a carriage control carriage control.
c
      if(irorw.eq.0) then
      inquire(file=filename,recordtype=rectype,
     *     carriagecontrol=ccontrol,recl=reclength)
      open(unit=unit,name=filename,type='old',
     *     recordtype=rectype,recl=reclength,
     *     err=50,carriagecontrol=ccontrol)
      end if
c
      if(irorw.eq.1) open(unit=unit,name=filename,
     *     type='new',recordtype='fixed',recl=2880,
     *     err=50,carriagecontrol='none')
c
      if(irorw.eq.2) open(unit=unit,name=filename,
     *     type='unknown',recordtype='fixed',recl=2880,
     *     err=50,carriagecontrol='none')
c
      return
   50 continue
      reclength = 0
c
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  PDM format readers and writers.
c
      subroutine readpdmf(ndummy,dummy,nfast,nslow,ibig,filename)
c
c  Read a PDM file and stuff it into ibig.
c
      integer*2 ibig(1),head(4096)
      character*(*) filename,dummy(1)
c
      iunit = 50
c
      open(unit=iunit,file=filename,readonly,
     *     status='old',recordsize=256,recordtype='fixed',
     *     form='unformatted')
      call rdpdmf(iunit,8,512,head)
      nfast = head(5)
      nslow = head(6)
      call rdpdmf(iunit,nslow,nfast,ibig)
c
      close(unit=iunit)
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine writepdmf(ndummy,dummy,nfast,nslow,ibig,filename)
c
c  Write a PDM file from the data in ibig.
c
      integer*2 ibig(1),head(4096)
      character*(*) filename,dummy(1)
c
      iunit = 50
c
      open(unit=iunit,file=filename,
     *     status='unknown',recordsize=256,recordtype='fixed',
     *     form='unformatted')
      head(5) = nfast 
      head(6) = nslow 
      call wrpdmf(iunit,8,512,head)
      call wrpdmf(iunit,nslow,nfast,ibig)
c
      close(unit=iunit)
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	subroutine rwpdmf (iunit,nslow,nfast,long)
	integer*2 long(1)
	logical rd
*
10	continue
	ncolper = 4096/nfast
	recs = float(nslow)/ncolper
	irecs = recs
	if (recs - irecs .gt. 0) irecs = irecs + 1
*
*
	do k = 1, irecs
	  kk = (k-1)*ncolper*nfast
	  do j = 1, 8
	    ii1 = kk + 512*j - 511
	    ii2 = kk + 512*j
c	    if (rd) read(iunit) (long(i),i=ii1,ii2)
c	    if (.not.rd) write(iunit) (long(i),i=ii1,ii2)
	    if (rd)      call rd512(iunit,long(ii1))
	    if (.not.rd) call wr512(iunit,long(ii1))
	  end do
	end do
*
	return
*
	entry rdpdmf (iunit,nslow,nfast,long)
	rd = .true.
	go to 10
*
	entry wrpdmf (iunit,nslow,nfast,long)
	rd = .false.
	go to 10
	end
*
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	subroutine rd512(iunit,data)
	integer*2 data(512)
	read(iunit)data
	return
	entry wr512(iunit,data)
	write(iunit)data
	return
	end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	SUBROUTINE READDTAF(ndum,dum,NX,NY,JMAGE,FNAME)
c	include 'tuneable'
	character*(*) dum(1), FNAME
	integer*2 JMAGE(1)
	INTEGER*4 NCOL,NROW,NX,NY,IERROR
	CHARACTER*64 MSG
c
79	format(a)
C
C 	     use the DTA_ routines to get the image data
C
	CALL DTA_ASFNAM( 'FILE',FNAME,'OLD',0,'DUMMY',IERROR)
	call DTA_ERROR(IERROR,MSG)
	if(IERROR .ne. 0) then
	    write(*,79)  MSG
	    write(*,*) ' Could not open file successfully.. RETURNING '
	    return
	end if
c
	CALL DTA_RDVARI( 'FILE.Z.NAXIS1',1,NCOL,IERROR)
	CALL DTA_RDVARI( 'FILE.Z.NAXIS2',1,NROW,IERROR)
	NX = NCOL
	NY = NROW
C
C
	CALL RDARRAY (NX,NY,JMAGE)
C
c
	CALL DTA_FCLOSE( 'FILE', IERROR)
	RETURN
	END
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	SUBROUTINE RDARRAY( NX,NY,IMAGE)
C
C
	integer*2 IMAGE( NX, NY )
	INTEGER*4 NITEM,IERROR,IDIMS(2)
	CHARACTER*70 MESSAGE,NAME
C
	NITEM = NX*NY

	CALL DTA_RDVARS('FILE.Z.DATA',NITEM,IMAGE,IERROR)
c
	RETURN
	END
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	SUBROUTINE WRITEDTAF(ndum,dum,NCOLS,NROWS,LPIC,fname)
c	include 'tuneable'
	integer*2 LPIC(1)
	character*(*) fname, dum(1)
	character*80 ERROR
	character*22 dname
	integer*4 status, NCOLS, NROWS, DIMS(2)
c
100	format(a)
	NBLOCKS = (2*NCOLS*NROWS + 511)/512 + 10
	call DTA_ASFNAM('file',fname,'unknown',NBLOCKS,'image',status)
	call DTA_ERROR(STATUS,ERROR)
	call DTA_CRVAR('file.Z','STRUCT',status)
	call DTA_ERROR(STATUS,ERROR)
	call DTA_CRVAR('file.Z.NAXIS1','INT',status)
	call DTA_ERROR(STATUS,ERROR)
	call DTA_CRVAR('file.Z.NAXIS2','INT',status)
	call DTA_ERROR(STATUS,ERROR)
	DIMS(1) = NCOLS
	DIMS(2) = NROWS
	call DTA_CRNAM('file.Z','DATA',2,DIMS,dname,status)
c	
101	format(1x,a)
	call DTA_ERROR(STATUS,ERROR)
	call DTA_CRVAR(dname,'SHORT',status)
	call DTA_ERROR(STATUS,ERROR)
c
	call DTA_WRVARI('file.Z.NAXIS1',1,NCOLS,status)
	call DTA_ERROR(STATUS,ERROR)
	call DTA_WRVARI('file.Z.NAXIS2',1,NROWS,status)
	call DTA_ERROR(STATUS,ERROR)
	NITEMS = NCOLS*NROWS
	call DTA_WRVARS('file.Z.data',NITEMS,LPIC,status)
	call DTA_ERROR(STATUS,ERROR)
c
	call DTA_FCLOSE('file',status)
	call DTA_ERROR(STATUS,ERROR)
c
	return 
	end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ************** If you want to customize the disk reader for your particular
c  disk data format, put that routine here using the suggested calls.  The
c  necessary declarations are included, but note that header and nhead are
c  optional, and only needed if you wish to fully preserve the headers of your
c  file (and assuming they can be declared as character*80).  ****************
c
      subroutine readlocalf(nhead,header,nfast,nslow,ibig,filename)
      integer*2 ibig(1)
      character*(*) filename
      character*80 header(1)
c
      return
      end 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine writelocalf(nhead,header,nfast,nslow,ibig,filename)
      integer*2 ibig(1)
      character*(*) filename
      character*80 header(1)
c
      return
      end 
