c
c  This subroutine opens or closes a sequential file.  
c  It also handles basic errors.
c
      subroutine opens(iunit,prompt,iopen)
      character*80 name
      character*(*) prompt
c
      nprompt = lenc(prompt)
      prompt(nprompt+1:nprompt+2) = ': '
 10   write(6,997) prompt(1:nprompt+2)
 997  format(1x,a,$)
c
c  Now read the file name and open the file using a very vanilla open
c  statement.
c
      call charinit(name,80)
c
      read(5,999,err=210) name
      nname = lenc(name)
 999  format(a)
c
      if(iopen.eq.0) open(unit=iunit,status='old',
     *     file=name(1:nname),err=211)
      if(iopen.eq.1) open(unit=iunit,status='new',
     *     file=name(1:nname),err=211)
      if(iopen.eq.2) open(unit=iunit,status='unknown',
     *     file=name(1:nname),err=211)
c
      return
c
 210  write(6,*) 'Error in file name. Try again.'
      go to 10
c
 211  write(6,*) 'Error opening file. Try again.'
      go to 10
c
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  This subroutine opens or closes a sequential file given a file name.
c  It returns an error code, 0 if ok, 1 if there is a problem.
c
      subroutine opena(iunit,name,iopen,ierr)
      character*(*) name
c
      ierr = 0
c
c  Now read the file name and open the file using a very vanilla open
c  statement.
c
      nname = lenc(name)
c
      if(iopen.eq.0) open(unit=iunit,status='old',
     *     file=name(1:nname),err=210)
      if(iopen.eq.1) open(unit=iunit,status='new',
     *     file=name(1:nname),err=210)
      if(iopen.eq.2) open(unit=iunit,status='unknown',
     *     file=name(1:nname),err=210)
c
 212  return
c
 210  ierr = 1
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  This subroutine asks for input.
c
      subroutine query(prompt)
      character*(*) prompt
c
      nprompt = lenc(prompt) + 1
      prompt(nprompt:nprompt) = ' '
      write(6,997) prompt(1:nprompt)
 997  format(1x,a,$)
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Closes a parameter file.
c
      subroutine endparam(iunit)
      character*3 end
c
      end = 'END'
c
      write(iunit,999) end
 999  format(a)
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Header reading/writing subroutines.
c
      subroutine findkey(header,keyword,lenkey)
c
c  Reads a line and extracts the keyword from a single header line.  
c  LENKEY is the length of the keyword.
c
      character*(*) header,keyword
c
      lenmax = lenc(header)
      do 900 j=1,lenmax
      if(header(j:j).eq.'=') then
      lenkey = j - 1
      go to 899
      end if
 900  continue
 899  continue
c
      if(lenkey.eq.0) return
c
      do 901 j=lenkey-1,1,-1
      if(header(j:j).ne.' ') then
      lenkey = j
      go to 10
      end if
 901  continue
c
 10   keyword = header(1:lenkey)
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c 
      subroutine getcomment(header,comment,ncomm)
c
c  Extracts the comment, if any exists, from a header line.
c
      character*(*) header,comment
c
      call charinit(comment,80)
c
      lenmax = lenc(header)
c
c  If lenmax .le. 2 then there is no comment here.
c
      if(lenmax.le.2) then
      ncomm = 0
      return
      end if
c
      do 890 i=1,lenmax
      if(header(i:i).eq.'=') then
      len1 = i
      go to 891
      end if
 890  continue
 891  continue
c
c  Now, find the end of the parameter.  If len1 = 1, then the comment
c  starts after the first blank; otherwise it starts after the second one.
c
      do 901 i=len1+1,lenmax
      if(header(i:i).ne.' ') then
      nz1 = i
      go to 902
      end if
 901  continue
 902  continue
c
      if(len1.eq.1) then
      ncomm = lenmax
      comment(1:ncomm) = header(1:lenmax)
      return
      end if
c
      if(nz1.eq.lenmax) then
      ncomm = 0
      return
      end if
c
      do 911 i=nz1+2,lenmax
      if(header(i:i).eq.' ') then
      nz2 = i
      go to 912
      end if
 911  continue
      ncomm = 0
      return
 912  continue
c
      nc1 = nz2
      ncomm = lenmax - nc1 + 1
      comment(1:ncomm) = header(nc1:lenmax)
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine writeitem(header,keyword,nr,itype,item,comment,ncomm)
c
c  Writes values out to a header line.  Also writes the comment for that
c  line if ncomm .gt. 0.  nr is the length of the item string.  ITYPE is
c  coded as described in RFITEM.
c
      parameter(isigdig=6)
      character*(*) header,keyword,comment
      character*80 hold,format
      character*1 item(1)
      character*10 iform, gform
c
c  Clean the header variable.
c
      call charinit(header,80)
c
      lkey = lenc(keyword)
      header(1:lkey) = keyword(1:lkey)
      header(lkey+1:lkey+3) = ' = '
c
      lstart = lkey + 4
c
      do 901 i=1,max0(nr,1)
      hold(i:i) = item(i)
 901  continue
c
c  Write a real variable.  Should you require a parameter with more than 6 
c  significant digits, change ISIGDIG accordingly.  Sorry for the kludgy
c  formatting, but Unix lobotomized Fortran wins again.  Down with F77!
c
      write(gform,200) nr
200   format('(g',i2,'.0)')
      write(iform,201) nr
201   format('(i',i2,')')
c100   format(g<nr>.0)
c101   format(i<nr>)
      if(itype.eq.0) then
      read(hold(1:nr),gform) value
      if(value.ge.0.0) iwidth = 4
      if(value.lt.0.0) iwidth = 5
      lend = lstart + isigdig + iwidth
      format = '(1pexx.5)'
      write(format(5:6),500) isigdig+iwidth+1
      write(header(lstart:lend),format) value
      header(lend+1:lend+1) = ' '
c
c  Write an integer variable.
c
      else if(itype.eq.1) then
      read(hold(1:nr),iform) ivalue
      if(ivalue.eq.0) lvalue = 1
      if(ivalue.gt.0) lvalue = int(alog10(real(ivalue)))+1
      if(ivalue.lt.0) lvalue = int(alog10(real(abs(ivalue))))+2
      lend = lstart + lvalue - 1
      format = '(ixx)'
      write(format(3:4),500) lvalue
 500  format(i2)
      write(header(lstart:lend),format) ivalue
      header(lend+1:lend+1) = ' '
c
c  Write a character variable.  Keep track of the quotes.
c
      else if(itype.eq.2) then
      header(lstart:lstart) = ''''
      lstart = lstart + 1
      lend = lstart - 1 + max0(nr,1)
      header(lstart:lend) = hold(1:nr)
c      call upper(header(lstart:lend))
      header(lend+1:lend+2) = ''' '
      end if
c
c  Write the comment line at the end of header.
c
      if(ncomm.ne.0) then
      if(itype.le.1) lstart = lend + 2
      if(itype.eq.2) lstart = lend + 3
      lend = min0(lstart+ncomm-1,80)
      ncomm = min0(ncomm,80-lstart+1)
      header(lstart:lend) = comment(1:ncomm)
      end if
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine readitem(n,header,keyword,nr,itype,item,mline)
c
c  Reads the item with key keyword from the FITS header. 
c  Returns the number of bytes returned, nr.  ITYPE is 0 for a real variable,
c  1 for an integer (assumed from leading letter of keyword), and 2 if
c  a character variable.
c
      character*(*) header(n),keyword
      character*1 item(1),first
c
c Locate the item.
c

      itype = 0
      mline = 0
      nr = 0
c
      do 900 i = 1,n
      lenmx = lenc(header(i))
      do 899 j=1,lenmx
      if(header(i)(j:j).eq.'=') then
      lenhead = j - 1
      go to 199
      end if
 899  continue
 199  continue
c
c  Find a match to the keyword.
c
      call upper(header(i)(1:lenhead))
      call upper(keyword)
      if(header(i)(1:lenhead).eq.keyword) then
      mline = i
      goto 10
      end if
900   continue
c
c  No match?  Then return.
c
      nr = 0
      return
c
 10   continue
c
c Locate the beginnings and ends of the item
c
      i1 = lenhead + 2
c
c  Strip leading blanks.
c
      is = i1
      do 898 i=is,80
      if(header(mline)(i:i).ne.' ') then
c
c  Determine if this is a character variable.
c
      if(header(mline)(i:i).eq.'''') then
      itype = 2
      i1 = i + 1
      go to 12
      end if
c
      i1 = i
      go to 12
      end if
 898  continue
 12   continue
c
c  Now, find the end of the string.
c
      lenmax = lenc(header(mline))
      i2 = i1
      do 901 i=i1+1,lenmax
c
c  Take care if this is a character.
c
      if((itype.eq.2.and.header(mline)(i:i).eq.'''').or.
     *     (header(mline)(i:i).eq.' ')) then
      i2 = i - 1
      go to 11
      else if(i.eq.lenmax) then
      i2 = i
      go to 11
      end if
c
 901  continue
 11   continue
c
c Strip trailing blanks
c
      nr = lenc(header(mline)(i1:i2))
c      if(nr.le.1.and.itype.eq.2) nr = 1
c
      do 903 i=0,nr-1
      item(i+1) = header(mline)(i1+i:i1+i)
 903  continue
c
c  Determine if real or integer if not character.
c
      if(itype.ne.2) then
      itype = 0
      first = keyword(1:1)
      if(first.eq.'I'.or.first.eq.'i') itype = 1
      if(first.eq.'J'.or.first.eq.'j') itype = 1
      if(first.eq.'K'.or.first.eq.'k') itype = 1
      if(first.eq.'L'.or.first.eq.'l') itype = 1
      if(first.eq.'M'.or.first.eq.'m') itype = 1
      if(first.eq.'N'.or.first.eq.'n') itype = 1
      end if
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      function lenc(s)
c
c  Get the length of a character variable.  Only necessary because of the
c  Unix lobotomy of Fortran.
c
      character*(*) s
      lenc = len(s)
      do 900 i = len(s),1,-1
          if(s(i:i).ne.' ') then
              lenc = i
              return
          end if
900   continue
      lenc = 0
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine charinit(charvar,ncharvar)
      character*(*) charvar
c
      do 10 i=1,ncharvar
      charvar(i:i) = ' '
 10   continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  
      subroutine readreal(item,nr,value)
c
c  Reads a real variable from ITEM.
c
      character*80 hold
      character*1 item(80)
      character*10 gform
c
      do 10 i=1,nr
      hold(i:i) = item(i)
 10   continue
c
      write(gform,200) nr
200   format('(g',i2,'.0)')
      read(hold(1:nr),gform) value
c100   format(g<nr>.0)
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  
      subroutine writereal(item,nr,value)
c
c  Reads a real variable from ITEM.
c
      parameter(isigdig=6)
      character*80 hold,format
      character*1 item(80)
c
      do 11 i=1,80
 11   item(i) = ' '
c
      lstart = 1
      if(value.ge.0.0) iwidth = 4
      if(value.lt.0.0) iwidth = 5
      lend = lstart + isigdig + iwidth
      format = '(1pexx.5)'
      nr = isigdig + iwidth + 1
      write(format(5:6),500) nr
 500  format(i2)
      write(hold(lstart:lend),format) value
c
      do 10 i=1,nr
      item(i) = hold(i:i)
 10   continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  
      subroutine readint(item,nr,ivalue)
c
c  Reads a integer variable from ITEM.
c
      character*80 hold
      character*1 item(80)
      character*10 iform
c
      do 10 i=1,nr
      hold(i:i) = item(i)
 10   continue
c
      write(iform,200) nr
200   format('(i',i2,')')
      read(hold(1:nr),iform) ivalue
c100  format(i<nr>)
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  
      subroutine writeint(item,nr,ivalue)
c
c  Reads a integer variable from ITEM.
c
      character*80 hold,format
      character*1 item(80)
c
      do 11 i=1,80
 11   item(i) = ' '
c
      lstart = 1
      if(ivalue.eq.0) nr = 1
      if(ivalue.gt.0) nr = int(alog10(real(ivalue)))+1
      if(ivalue.lt.0) nr = int(alog10(real(abs(ivalue))))+2
      lend = lstart + nr - 1
      format = '(ixx)'
      write(format(3:4),500) nr
 500  format(i2)
      write(hold(lstart:lend),format) ivalue
c
      do 10 i=1,nr
      item(i) = hold(i:i)
 10   continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  
      subroutine readchar(item,nr,value)
c
c  Reads a character variable from ITEM.
c
      character*(*) value
      character*1 item(80)
c
      do 10 i=1,nr
      value(i:i) = item(i)
 10   continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  
      subroutine writechar(item,nr,value)
c
c  Reads a character variable from ITEM.
c
      character*(*) value
      character*1 item(80)
c
      do 11 i=1,80
 11   item(i) = ' '
c
      do 10 i=1,nr
      item(i) = value(i:i)
 10   continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine upper(string)
      character*(*) string
c
      nstring = lenc(string)
c
      do 10 i=1,nstring
      nhold = ichar(string(i:i))
      if(nhold.ge.97.and.nhold.le.122) nhold = nhold - 32
      string(i:i) = char(nhold)
 10   continue

      return
      end
c
c  This subroutine handles yes or no questions, ensuring that
c  y or n are the only allowable answers returned to the calling
c  program.
c
      subroutine yesorno(prompt,answer)
      character*80 prompt
      character*1 answer
c
      nprompt = lenc(prompt)
c
   30 call query(prompt(1:nprompt))
      read (5,999) answer
  999 format(a)
      if(answer.eq.'y'.or.answer.eq.'Y') answer = 'y'      
      if(answer.eq.'n'.or.answer.eq.'N') answer = 'n'      
      if(answer.ne.'y'.and.answer.ne.'n') go to 30
      return
      end
