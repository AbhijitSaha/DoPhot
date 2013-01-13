        SUBROUTINE MEDFIL(NCOLS,NROWS,LPICT,NEWPICT,Z1,Z2,XHW,YHW,
     *   nrmax, ncmax, prec)
C
C.... 32767 has often been written as 32770-3 to prevent I*2 overflow 
C           in computation
c
        integer*2 LPICT(NCOLS,NROWS), NEWPICT(nrmax,ncmax)
	integer*2 BOX(65535)
	integer*4 i, j, count, k, kc, klim, XHW, YHW, Z1, Z2, min4
        integer*4 prec
c
c
	Z1 = MAX(Z1,-32767)
	Z2 = MIN(Z2,32767)
c	write(6,*) Z1, Z2, xhw, yhw
c
c
199	do 700 i = YHW+1,NROWS-YHW
c
        if(i .eq. YHW+1) go to 688
          if( MOD(i,200) .eq. 1) then 
           write(6,105) prec*min4, i-1, NEWPICT(NCOLS/3,i-1),
     *         NEWPICT((NCOLS/2), i-1), 
     *         NEWPICT((2*NCOLS/3),i-1), min4
          end if
 105	format('+', I7, ' ->   ',5I7 )
 107 	format('+', I7)
c........ Load Fresh box : ....
c
 688	min4 = Z2/prec
	do 689 m = 1,65535
 689	BOX(m) = 0
	count = 0
	do 690 krow = i-YHW,i+YHW
	  do 691 kcol = 1, 2*XHW+1
	  if(LPICT(kcol,krow) .ge. Z1 .and. 
     *       LPICT(kcol,krow) .le. Z2) then
           BOX(LPICT(kcol,krow)/prec + 32770-3) = 
     *          BOX(LPICT(kcol,krow)/prec +32770-3)+1
	   if(LPICT(kcol,krow)/prec .le. min4) then
              min4 = LPICT(kcol,krow)/prec
c	      type 107, min4
           end if
           count = count + 1
	  end if
691       continue
690       continue
c
		 kc = 0
c
c... if BOX is empty...  short-circuit:
c
	         if (count .eq. 0) then 
	            NEWPICT(XHW+1,i) = 0
c.. Set LOW so that objects are triggered on 1st pass but rejected on 
c.. on AVERAGE sky test.... setting high may have other problems
                    go to 200
	         end if
c..... Short circuit completed
c
	         klim = count/2 + 1
                 do 901 k = min4+32770-3, (Z2/prec + 32770-3)
		  kc = kc + BOX(k)
		  if(kc .ge. klim) then
		   NEWPICT(XHW+1,i) =  (k - 32767)*prec
		   go to 200
		  end if	
901		 continue
c	type *, i, count, NEWPICT(XHW+1,i)

c
200		do 701 j = XHW+2, NCOLS-XHW
c	if(j .eq. 51) type *, i,count,NEWPICT(50,i)
c
c....... Strip Old Left Column and Add New Right Column :- 
c
                 do 802 k = i-YHW,i+YHW
                  if(LPICT(j-(XHW+1),k) .ge. Z1 .and. 
     *             LPICT(j-(XHW+1),k) .le. Z2) then
                   BOX(LPICT(j-(XHW+1),k)/prec + 32770-3) = 
     *                BOX(LPICT(j-(XHW+1),k)/prec + 32770-3) - 1
		   count = count - 1
		  end if
		  if(LPICT(j+XHW,k) .ge. Z1 .and. LPICT(j+XHW,k)
     * 		     .le. Z2) then
		   BOX(LPICT(j+XHW,k)/prec + 32770-3) = 
     *                BOX(LPICT(j+XHW,k)/prec + 32770-3) + 1
                   if(LPICT(j+XHW,k)/prec .le. min4) then
                     min4 = LPICT(j+XHW,k)/prec
c	             type 107, min4
                   end if 
		   count = count + 1
                  end if
c
802              continue
c
		 kc = 0
c... if BOX is empty...  short-circuit:
	         if (count .eq. 0) then 
	            NEWPICT(j,i) = 0
                    go to 701
	         end if
c..... Short circuit completed
c
	         klim = count/2 + 1
                 do 902 k = min4+32770-3,(Z2/prec + 32770-3)
		  kc = kc + BOX(k)
		  if(kc .ge. klim) then
		   NEWPICT(j,i) = (k - 32767)*prec
		   go to 701
		  end if	
902		 continue
c
701		continue
700	continue
c
c.....Fill in the margins :-
c
	do 720 i = 1, NROWS
	  do 721 j =  1,NCOLS
	   if(i .ge. (YHW+1) .and. i .le. (NROWS-YHW) .and. 
     *      j .ge. (XHW+1) .and. j .le. (NCOLS-XHW) ) go to 721
c
           if( i .le. YHW) then
	    if(j .le. XHW) then
             NEWPICT(j,i) = NEWPICT(XHW+1,YHW+1)
             go to 721
            end if
            if(j .ge. NCOLS-XHW) then
             NEWPICT(j,i) = NEWPICT((NCOLS-XHW),YHW+1)
             go to 721
            end if 
            NEWPICT(j,i) = NEWPICT(j,YHW+1)
            go to 721
           end if
c
           if( i .ge. NROWS-YHW ) then
            if(j .le. XHW) then
             NEWPICT(j,i) = NEWPICT(XHW+1,NROWS-YHW)
             go to 721
            end if
            if(j .ge. NCOLS-XHW) then
             NEWPICT(j,i) = NEWPICT((NCOLS-XHW),(NROWS-YHW))
             go to 721
            end if
            NEWPICT(j,i) = NEWPICT(j,NROWS-YHW)
            go to 721
           end if
c
           if( j .le. XHW ) then
            NEWPICT(j,i) = NEWPICT(XHW+1,i)
            go to 721
           end if
           if( j .ge. NCOLS-XHW ) then
            NEWPICT(j,i) = NEWPICT(NCOLS-XHW,i)
            go to 721
           end if 
c
721	  continue   
720     continue
c     
c     
	return 
	end
