# modeFold ver.1.3

**Fortran program for vocal fold oscillation simulation**  
**Date:** 2025/Oct/23  
**Author:** Tsukasa Yoshinaga  

---

## Overview

This program calculates **vocal fold oscillations** using the eigenmodes obtained from **COMSOL eigenanalysis**.  
The external aerodynamic force can be selected from either a **1D incompressible flow** or a **1D compressible flow** model, both based on **Bernoulli’s principle**.

The airflow and vocal fold vibration are computed using the **equivalent circuit model** proposed by *Ishizaka and Flanagan (1972)*.  
The results have been validated against **experimental measurements** and **3D compressible flow simulations** (*Yoshinaga and Zhang, 2025*).

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
