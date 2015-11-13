# Makefile for linear algebra tests 

#===============================================================================
# User Options
#===============================================================================

COMPILER = gfortran
OPTIMIZE = yes

#===============================================================================
# Vendor BLAS implementations
#===============================================================================

# For linking MKL, see:
#   http://software.intel.com/en-us/articles/intel-mkl-link-line-advisor/
LIBMKLSERIAL   = -I$(MKLROOT)/include -Wl,--start-group       \
                 $(MKLROOT)/lib/intel64/libmkl_intel_lp64.a   \
                 $(MKLROOT)/lib/intel64/libmkl_sequential.a   \
                 $(MKLROOT)/lib/intel64/libmkl_core.a         \
                 -Wl,--end-group -lpthread -lm
LIBMKLTHREAD   = -I$(MKLROOT)/include  -Wl,--start-group      \
                 $(MKLROOT)/lib/intel64/libmkl_intel_lp64.a   \
                 $(MKLROOT)/lib/intel64/libmkl_intel_thread.a \
                 $(MKLROOT)/lib/intel64/libmkl_core.a -Wl,    \
                 --end-group -openmp -lpthread -lm
LIBACMLSERIAL  = -L /home/robertsj/opt/acml/acml4.4.0/gfortran64/lib/  \
                 -lacml -lgfortran
LIBACMLTHREAD  = -L /home/robertsj/opt/acml/acml4.4.0/gfortran64_mp/lib/ \
                 -lacml_mp -lgfortran
LIBBLAS        =  -L -lblas3

#===============================================================================
# Compiler Definitions
#===============================================================================

ifeq ($(COMPILER),gfortran)
  F90 = gfortran
  OMPFLAG = -fopenmp
  ifeq ($(OPTIMIZE),yes)
    OPT = -O3
  endif
endif

ifeq ($(COMPILER),ifort)
  F90 = ifort
  OMPFLAG = -openmp
  ifeq ($(OPTIMIZE),yes)
    OPT = -O3
  endif
endif

#===============================================================================
# Targets
#===============================================================================

all: driver-mv-row-x driver-mv-col-x driver-mv-omp-x \
     driver-mv-blas-x \
     driver-mv-mkl-s-x driver-mv-mkl-t-x driver-mv-acml-s-x driver-mv-acml-t-x \
     driver-mm-x \
     driver-mm-blas-x \
     driver-mm-mkl-s-x driver-mm-mkl-t-x driver-mm-acml-s-x driver-mm-acml-t-x 

#===============================================================================
# Matrix-Vector Tests
#===============================================================================

# Row-major MV test
driver-mv-row-x:
	$(F90) $(OPT) $(OMPFLAG) matvec-row.f90 driver-mv.f90 -o $@ 

# Col-major MV test
driver-mv-col-x:
	$(F90) $(OPT) $(OMPFLAG) matvec-col.f90 driver-mv.f90 -o $@ 

# Row-major MV test with OpenMP.  Row-major works better.
driver-mv-omp-x:
	$(F90) $(OPT) $(OMPFLAG) matvec-omp.f90 driver-mv.f90 -o $@ 

# Serial default blas
driver-mv-blas-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBBLAS)

# Serial MKL
driver-mv-mkl-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBMKLSERIAL)

# Threaded MKL
driver-mv-mkl-t-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBMKLTHREAD)

# Serial ACML
driver-mv-acml-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBACMLSERIAL)

# Threaded ACML
driver-mv-acml-t-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBACMLTHREAD)

#===============================================================================
# Matrix-Matrix Tests
#===============================================================================

# Simple Implementation
driver-mm-x:
	$(F90) $(OPT) $(OMPFLAG) matmat.f90 driver-mm.f90 -o $@ 

# Serial default BLAS
driver-mm-blas-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBBLAS)

# Serial MKL
driver-mm-mkl-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBMKLSERIAL)

# Threaded MKL
driver-mm-mkl-t-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBMKLTHREAD)

# Serial ACML
driver-mm-acml-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBACMLSERIAL)

# Threaded ACML
driver-mm-acml-t-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBACMLTHREAD)


clean:
	@rm -f *.o *.mod *-x

neat:
	@rm -f *.o *.mod

#===============================================================================
# Rules
#===============================================================================

.SUFFIXES: .F90 .o
.PHONY: clean neat

%.o: %.f90
	$(F90) $(F90FLAGS) $(INCLUDE) -c $<


