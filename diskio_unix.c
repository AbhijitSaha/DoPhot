/*
* Routines to read and write disk FITS format data.
* These files have a header which consists of 36*n 80 byte header records
* which are a duplicate of the FITS tape format, with a mandatory END record.
* This is followed by 1440*n short integers of data.
* BEWARE: if SWAB, the data returned by wffile is byte swapped!
*/

#include <stdio.h>

#define NFITS (2880)

#define SWAB 1		/* Swap bytes? */


static char buf[NFITS];

/* Read a short integer file's header and data */

rffile_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     short *data;
     char *head, *file;
{
  rfitsns(16,n,head,npix,nline,data,file,headlen,filelen);
  if(SWAB) swab(data,data,2*(*npix)*(*nline));
}

/* Read a 32 bit file's header and data */

rffint_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     unsigned short *data;
     char *head, *file;
{
  register int i;
  unsigned short temp;
  rfitsns(32,n,head,npix,nline,data,file,headlen,filelen);
  if(SWAB) {
    swab(data,data,4*(*npix)*(*nline));
    for(i=0;i<2*(*npix)*(*nline);i+=2) {
      temp = data[i];
      data[i] = data[i+1];
      data[i+1] = temp;
    }
  }
}

/* Read a bitmap file's header and data */

rfbitfile_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     short *data;
     char *head, *file;
{
  rfitsns(1,n,head,npix,nline,data,file,headlen,filelen);
  if(SWAB) swab(data,data,2*(*npix)*(*nline));
}

/* Read a file's header and data ... no byte swap version */

rffilens_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     short *data;
     char *head, *file;
{
  rfitsns(16,n,head,npix,nline,data,file,headlen,filelen);
}


/* Read a file of int's don't swap bytes */

rffintns_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     int *data;
     char *head, *file;
{
  rfitsns(32,n,head,npix,nline,data,file,headlen,filelen);
}

/* Just read the bits, with bitsper bits per data element */
rfitsns(bitsper,n,head,npix,nline,data,file,headlen,filelen)
     int bitsper;
     int *n, *npix, *nline, headlen, filelen;
     unsigned char *data;
     char *head, *file;
{
  int fd, err, need, i, nread;

  while(--filelen > 0 && (file[filelen]==' ' || file[filelen]==0));
  for(i=0;i<=filelen;i++) buf[i] = file[i] ;
  buf[filelen+1] = '\0';

  if((fd=open(buf,0)) == -1) {
    fprintf(stderr,"rffile: cannot open file %s\n",buf);
    (*npix)=(*nline)=0;
    return;
  }

/* Read the header, looking for NAXIS1, NAXIS2 and END */
  if((err = getheader(fd,n,head,npix,nline)) != 0) {
    close(fd);
    (*npix)=(*nline)=0;
    return;
  }

/* Read the data according to the values for NPIX, NLINE */
  need = (bitsper*(*npix)*(*nline)+7) / 8;
  if((nread=read(fd,data,need)) != need) {
    fprintf(stderr,"rffile: end of file before data read\n");
    (*npix)=(*nline)=0;
  }
  close(fd);
}


/* Read just the file's header */

rfhead_(n,head,file,headlen,filelen)
     int *n, headlen, filelen;
     char *head, *file;
{
  int fd, err, nread, npix, nline, i;

  while(--filelen > 0 && (file[filelen]==' ' || file[filelen]==0));
  for(i=0;i<=filelen;i++) buf[i] = file[i] ;
  buf[filelen+1] = '\0';

  if((fd=open(buf,0)) == -1) {
    fprintf(stderr,"rffile: cannot open file %s\n",buf);
    return;
  }

/* Read the header, looking for NAXIS1, NAXIS2 and END */
  if((err = getheader(fd,n,head,&npix,&nline)) != 0) {
    close(fd);
    return;
  }
  close(fd);
}

/* Write a short integer fits format file */

wffile_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     short *data;
     char *head, *file;
{
  if(SWAB) swab(data,data,2*(*npix)*(*nline));
  wfitsns(16,n,head,npix,nline,data,file,headlen,filelen);
  if(SWAB) swab(data,data,2*(*npix)*(*nline));
}

/* Write a 32 bit file's header and data */

wffint_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     unsigned short *data;
     char *head, *file;
{
  register int i;
  unsigned short temp;
  if(SWAB) {
    swab(data,data,4*(*npix)*(*nline));
    for(i=0;i<2*(*npix)*(*nline);i+=2) {
      temp = data[i];
      data[i] = data[i+1];
      data[i+1] = temp;
    }
  }
  wfitsns(32,n,head,npix,nline,data,file,headlen,filelen);
  if(SWAB) {
    for(i=0;i<2*(*npix)*(*nline);i+=2) {
      temp = data[i];
      data[i] = data[i+1];
      data[i+1] = temp;
    }
    swab(data,data,4*(*npix)*(*nline));
  }
}

/* Write a bitmap fits format file */

wfbitfile_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     short *data;
     char *head, *file;
{
  if(SWAB) swab(data,data,2*((*npix)*(*nline)+15)/16);
  wfitsns(1,n,head,npix,nline,data,file,headlen,filelen);
  if(SWAB) swab(data,data,2*((*npix)*(*nline)+15)/16);
}


/* Write a fits format file ... no byte swap version */

wffilens_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     short *data;
     char *head, *file;
{
  wfitsns(16,n,head,npix,nline,data,file,headlen,filelen);
}

/* Write a fits format file ... no byte swap version */

wffintns_(n,head,npix,nline,data,file,headlen,filelen)
     int *n, *npix, *nline, headlen, filelen;
     short *data;
     char *head, *file;
{
  wfitsns(32,n,head,npix,nline,data,file,headlen,filelen);
}

wfitsns(bitsper,n,head,npix,nline,data,file,headlen,filelen)
     int bitsper;
     int *n, *npix, *nline, headlen, filelen;
     short *data;
     char *head, *file;
{
  int fd, end, nwrite, need;
  int naxis1, naxis2;
  int i, extra;

/* Consistency check, npix = NAXIS1 and nline = NAXIS2 or else! */
  naxis1 = 0;
  naxis2 = 0;
  for(i=0;i<(*n) && (naxis1==0 || naxis2==0);i++) {
    if(strncmp(head+80*i,"NAXIS1",6) == 0) naxis1 = atoi(head+80*i+10);
    if(strncmp(head+80*i,"NAXIS2",6) == 0) naxis2 = atoi(head+80*i+10);
  }
  if(naxis1==0 || naxis2==0) {
    fprintf(stderr,"wffile: cannot find NAXIS1, NAXIS2\n");
    return;
  }   
  if(naxis1 != *npix || naxis2 != *nline) {
    fprintf(stderr,"wffile: npix, NAXIS1, nline, NAXIS2 = %d %d %d %d\n",
	    *npix, naxis1, *nline, naxis2);
    return;
  }

/* Open the file */
  while(--filelen > 0 && (file[filelen]==' ' || file[filelen]==0));
  for(i=0;i<=filelen;i++) buf[i] = file[i] ;
  buf[filelen+1] = '\0';
/*
  fprintf(stderr,"file %s filelen %d\n",buf,filelen);
*/
  if((fd=creat(buf,0644)) == -1) {
    fprintf(stderr,"wffile: cannot open file %s\n",buf);
    return;
  }

/* Write the header, quitting as soon as END is found */
  end = 0;
  for(i=0;i<(*n)*80;i+=80) {
    if((nwrite=write(fd,head+i,80)) != 80) {
      fprintf(stderr,"wffile: error writing header\n");
      close(fd);
      return;
    }
    if(head[i]=='E' && head[i+1]=='N' && head[i+2]=='D') {
      end = 1;
      break;
    }
  }
  if(end == 0) {
    fprintf(stderr,"wffile: END missing from header\n");
    close(fd);
    return;
  }

  i += 80;
  extra = NFITS - (i%NFITS);
  if(extra < NFITS) {
    for(i=0;i<extra;i++) buf[i] = ' ';
    if((nwrite = write(fd,buf,extra)) != extra) {
      fprintf(stderr,"wffile: error writing header\n");
      close(fd);
      return;
    }
  }

/* Write the data to the file */

  need = (bitsper*(*npix)*(*nline)+7) / 8;
  if((nwrite = write(fd,data,need)) != need) {
    fprintf(stderr,"wffile: error writing data\n");
    close(fd);
    return;
  }

  extra = NFITS - (need%NFITS);
  if( extra < NFITS ) {
    for(i=0;i<extra;i++) buf[i] = 0;
    if((nwrite = write(fd,buf,extra)) != extra) {
      fprintf(stderr,"wffile: error writing data tail\n");
    }
  }
  close(fd);
  return(0);
}

getheader(fd,n,head,npix,nline)
     int fd, *n, *npix, *nline;
     char *head;
{
  int i, nread, extra, end;

  end = 0;
  for(i=0;i<(*n)*80;i+=80) {
    if((nread=read(fd,head+i,80)) != 80) {
      fprintf(stderr,"rffile: error reading header; END not found\n");
      return(1);
    }
    if(head[i]=='E' && head[i+1]=='N' && head[i+2]=='D') {
      end = 1;
      break;
    }
  }
  if(end == 0) {
    fprintf(stderr,"rffile: insufficient buffer for header\n");
    return(2);
  }

  i+= 80;
  extra = NFITS - (i%NFITS);
  if((i%NFITS) !=0) {
    if((nread = read(fd,buf,extra)) != extra) {
      fprintf(stderr,"rffile: error reading header, file too short\n");
      return(1);
    }
  }
  *n = i / 80;
  *npix = 0;
  *nline = 0;
  for(i=0;i<(*n) && (*npix==0 || *nline==0);i++) {
    if(strncmp(head+80*i,"NAXIS1",6) == 0) *npix = atoi(head+80*i+10);
    if(strncmp(head+80*i,"NAXIS2",6) == 0) *nline = atoi(head+80*i+10);
  }
  if(*npix==0 || *nline==0) {
    fprintf(stderr,"rffile: cannot find NPIX, NLINE\n");
    return(3);
  }   
  return(0);
}

/* Open a file and read a number of bytes from it */

rawreadfile_(nbyte,data,swapbyte,file,filelen)
int *nbyte, filelen, *swapbyte;
char *data, *file;
{
  int fd, nread, i;

  while(--filelen > 0 && (file[filelen]==' ' || file[filelen]==0));
  for(i=0;i<=filelen;i++) buf[i] = file[i] ;
  buf[filelen+1] = '\0';

  if((fd=open(buf,0)) == -1) {
    fprintf(stderr,"rffile: cannot open file %s\n",buf);
    *nbyte = 0;
    return;
  }

/* Read the data according to the requested number of bytes */
  if((nread=read(fd,data,*nbyte)) != *nbyte) {
    fprintf(stderr,"rawreadfile: end of file before data read\n");
    *nbyte = nread;
  }
  close(fd);
  if(*swapbyte == 1) swab(data,data,*nbyte);
}

swapbytes_(n,s)
     int *n;
     short *s;
{
  swab(s,s,*n);
}
