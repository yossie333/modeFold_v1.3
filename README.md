# modeFold ver.1.3

**Fortran program for vocal fold oscillation simulation**  
**Date:** 2025/Oct/23  
**Author:** Tsukasa Yoshinaga  

---

## Overview

This program calculates **vocal fold oscillations** using the eigenmodes obtained from COMSOL eigenanalysis.  
The external aerodynamic force can be selected from either a **1D incompressible flow** or a **1D compressible flow** model, both based on Bernoulli’s principle.

The airflow and vocal fold vibration are computed using the **equivalent circuit model** proposed by *Ishizaka and Flanagan (1972)*.  
The results have been validated against experimental measurements and 3D compressible flow simulations (*Yoshinaga and Zhang, 2025, Physics of Fluids*).

---

## Version History

### From ver.1.0 to ver.1.2
- Added calculation of **SDF (Signed Distance Function)**.  
- When `noutfmt = 2`, structured grids and SDF values are generated via the subroutine `writeVTK2`.  
- `SDF(:,:)`: binary field (0 = flow, 1 = solid)  
- `SDF2(:,:)`: distance from the surface  

### From ver.1.2 to ver.1.3
- Added **1D compressible flow model** (`calcFlow`), enabled by setting `iflow = 1`.  
- Airflow is calculated from the inlet chamber to the subglottal tract.  
  The glottal flow rate is computed and the supraglottal pressure is updated accordingly.

---

## Requirements

- **Fortran 90** or later  
- No external libraries required  

---

## Compilation and Execution

```bash
make clean
make
./modeFold
```

---

## Output Files

- result/grid***.vtu     : Structured grid data (viewable in ParaView)
- result/deform***.vtu   : Unstructured grid data showing deformation (viewable in ParaView)
- result/history***      : Displacement history data
- result/flowrate.txt    : Time series of glottal flow rate
- result/pressure.txt    : Mouth (supraglottal) pressure history

The `.vtu` files can be opened and visualized using ParaView, a free and open-source scientific visualization tool. Download from: https://www.paraview.org/

---

## References

- Ishizaka, K. & Flanagan, J. L. (1972). Synthesis of voiced sounds from a two-mass model of the vocal cords. Bell System Technical Journal.

- Yoshinaga, T. & Zhang, Z. (2025). Evaluating the accuracy of one-dimensional glottal flow model in predicting voice production: comparison to experiments and three-dimensional flow simulations.[in press]

