# Fortran compiler
#FC = ifort
FC = gfortran-8
# Fortran compiler flags
FF = -O3 -std=f2008

# C compiler
CC = gcc
# C compiler flags
CF = 

dep_list = regex_cmod.o regex_mod.o scan_verify_smod.o

.PHONY: clean all test

all: clean regex_mod.o test

regex_mod.o: regex_mod.f90 regex_cmod.o scan_verify_smod.f90
	$(FC) $(FF) -c $< scan_verify_smod.f90
	 ar -cvq libregex.a $(dep_list)
	
regex_cmod.o: regex_cmod.c
	$(CC) $(CF) -c $<

test: regex_test.f90 $(dep_list)
	$(FC) $(FF) $< -o regex_test -lregex -L. -I.
	./regex_test

clean:
	rm -f *.mod *.o *.smod *.a
