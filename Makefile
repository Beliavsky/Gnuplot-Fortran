executables = xgnuplot_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = kind.o gnuplot.o xgnuplot.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xgnuplot_gfort.exe: kind.o gnuplot.o xgnuplot.o
	$(FC) -o xgnuplot_gfort.exe kind.o gnuplot.o xgnuplot.o $(FFLAGS)

run: $(executables)
	./xgnuplot_gfort.exe

clean:
	rm -f $(executables) $(obj)

