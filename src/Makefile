include ../opt/option.txt

OBJDIR = objs

.SUFFIXES : .f90

TARGET = ../modeFold

SRC  =  variMode.f90\
	readParam.f90\
	readFreq.f90\
	readVTK.f90\
	writeVTK.f90\
	writeVTK2.f90\
	writeVTK3.f90\
	heapsort.f90\
	surfExtract.f90\
	initia.f90\
	surfArea.f90\
	f2mode.f90\
	mode2uf.f90\
	runge.f90\
	calcArea.f90\
	calcForce.f90\
	contactForce.f90\
	calcDis.f90\
	uf2u.f90\
	output.f90\
	step.f90\
	calcSDF.f90\
	main.f90

OBJ  =  $(patsubst %.f90, $(OBJDIR)/%.o, $(SRC))


all: $(OBJ)  $(TARGET) 

$(TARGET): $(OBJ)
	$(FC) $(OPT) -o $@  $(OBJ)

$(OBJ): $(OBJDIR)/%.o: %.f90
	$(FC) $(OPT) -c $<  -o $@ 

clean:
	rm -f $(OBJDIR)/*.o *.mod 

