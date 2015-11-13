# Makefile for linear algebra tests 

#===============================================================================
# User Options
#===============================================================================

COMPILER = ifort
OPTIMIZE = yes

#===============================================================================
# Vendor BLAS implementations
#===============================================================================

# For linking MKL, see:
#   http://software.intel.com/en-us/articles/intel-mkl-link-line-advisor/
LIBMKLSERIAL   =  -i8 -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include \
                   ${MKLROOT}/lib/intel64/libmkl_blas95_ilp64.a -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a ${MKLROOT}/lib/intel64/libmkl_core.a ${MKLROOT}/lib/intel64/libmkl_sequential.a -Wl,--end-group -lpthread -lm
LIBMKLTHREAD   =  -i8 -qopenmp -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include \
                  ${MKLROOT}/lib/intel64/libmkl_blas95_ilp64.a -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a ${MKLROOT}/lib/intel64/libmkl_core.a ${MKLROOT}/lib/intel64/libmkl_intel_thread.a -Wl,--end-group -lpthread -lm
LIBACMLSERIAL  = -L /home/robertsj/opt/acml/gfortran64/lib  \
                 -lacml -lgfortran  -lrt -ldl
LIBACMLTHREAD  = -L /home/robertsj/opt/acml/gfortran64_mp/lib \
                 -lacml_mp -lgfortran  -lrt -ldl
LIBBLAS        =  -L /usr/lib -lblas

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

all: simple blas mkl acml

simple: mv-row-x mv-col-x mv-omp-x mm-x 

blas : mv-blas-x mm-blas-x 

mkl : mv-mkl-s-x mv-mkl-t-x mm-mkl-s-x mm-mkl-t-x

acml : mv-acml-s-x mv-acml-t-x mm-acml-s-x mm-acml-t-x

#===============================================================================
# Matrix-Vector Tests
#===============================================================================

# Row-major MV test
mv-row-x:
	$(F90) $(OPT) $(OMPFLAG) matvec-row.f90 driver-mv.f90 -o $@ 

# Col-major MV test
mv-col-x:
	$(F90) $(OPT) $(OMPFLAG) matvec-col.f90 driver-mv.f90 -o $@ 

# Row-major MV test with OpenMP.  Row-major works better.
mv-omp-x:
	$(F90) $(OPT) $(OMPFLAG) matvec-omp.f90 driver-mv.f90 -o $@ 

# Serial default blas
mv-blas-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBBLAS)

# Serial MKL
mv-mkl-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBMKLSERIAL)

# Threaded MKL
mv-mkl-t-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBMKLTHREAD)

# Serial ACML
mv-acml-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBACMLSERIAL)

# Threaded ACML
mv-acml-t-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mv.f90 -o $@ $(LIBACMLTHREAD)

#===============================================================================
# Matrix-Matrix Tests
#===============================================================================

# Simple Implementation
mm-x:
	$(F90) $(OPT) $(OMPFLAG) matmat.f90 driver-mm.f90 -o $@ 

# Serial default BLAS
mm-blas-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBBLAS)

# Serial MKL
mm-mkl-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBMKLSERIAL)

# Threaded MKL
mm-mkl-t-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBMKLTHREAD)

# Serial ACML
mm-acml-s-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBACMLSERIAL)

# Threaded ACML
mm-acml-t-x:
	$(F90) $(OPT) $(OMPFLAG) driver-mm.f90 -o $@ $(LIBACMLTHREAD)

clean:
	@rm -f *.o *.mod *-x

neat:
	@rm -f *.o *.mod

#===============================================================================
# Rules
#===============================================================================

.SUFFIXES: .F90 .o
.PHONY: clean neat mkl acml simple

%.o: %.f90
	$(F90) $(F90FLAGS) $(INCLUDE) -c $<

