 FC= gfortran
# get rid of qualifier g (allows use of dbx) when you're happy and
# want speed
FFLAGS=  -g -m64 -ffixed-line-length-80

COBJECTS = diskio_unix.o

FOBJECTS = test_dophot.o isearch.o fillerup.o guess.o thst_shape.o \
        parupd.o errupd.o addstar.o varipar_psf.o \
        varipar_hub.o \
        skyfun_hub.o skyfun_quad.o parinterp_psf.o \
        makenoise.o \
        test_tuneup.o offpic.o test_chisq.o simul.o \
        addlims.o hst_pseud2d_test.o \
        test_paravg.o lubksb.o ludcmp.o hst_oblims.o \
        oblit.o improve.o \
        impaper.o warmstart.o makemask.o \
        transmask.o hst_galaxy.o toobright.o \
        cosprob.o \
        hst_toofaint.o cosmic.o twofit.o probgal.o \
        elarea.o \
        io_dophot.o paramfile.o \
        stdotpt.o stdinpt.o \
        ellipse.o ellipint.o badotpt.o medfil.o apcorr3.o quick.o apcorrxtra.o


dophot: $(FOBJECTS) $(COBJECTS)
	$(FC) $(FFLAGS) $(FOBJECTS) $(COBJECTS) \
	-o xtest_dophot


clean:
	rm -f *.o

