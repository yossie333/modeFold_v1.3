
 Fortran program for vocal fold oscillation modeFold ver1.1
 2023/Nov/13    by  Tsukasa Yoshinaga
 
 This program calculate the vocal fold oscillation from the
 eigenmodes obtained from the COMSOL eigenanalysis.
 The external forces can be chosen as forced oscillation or
 1D Bernoulli's equation.

 In this version, the vocal fold shape was detected in a
 structured grids. The calcSDF calculates the inside and 
 outside of the vocal fold region and calculates the distance
 from the wall. The structured grids were outputed by 
 writeVTK2. Param file was used to determin the grid sizes.

 Input files: Parameter file (param.txt)
              COMSOL output (VTK file)
                            (frequency text)
              Surface point list (surface.txt)
 Output files: Structured grid files (result/grid***.vtu)
               flowrate (result/flowrate.txt)
   
 Module file: variMode
