program main
!*******************************************************************
! Fortran program for vocal fold oscillation: modeFold ver1.3
! 2025/Oct/23    by  Tsukasa Yoshinaga
! 
! This program calculates vocal fold oscillations using the
! eigenmodes obtained from the COMSOL eigenanalysis.
! The external forces can be selected from eigher a 1D imcompressible 
! or a 1D compressible flow model, both based on Bernoulli's principle.
!
! Updates from version 1.0 to 1.2:
! -Calculation of SDF(Signed Distance FUnction) was added.
! -By putting noutfmt = 2, structured grids and SDF values
!  are generated with the subroutine writeVTK2 
! -SDF(:,:): binaryfield (0 for flow, 1 for solid)
! -SDF2(:,:): distance from the surface
!
! Updates from version 1.2 to 1.3:
! -A 1D compressible flow model was added (subroutine calcFlow), 
!  whichs can be enabled by setting iflow = 1.
!
! In this version, the airflow is calculated from the inlet
! chamber to the subglottal tract. The glottal flow rate is then
! computed, and the pressure in the supraglottal tract
! is updated accordingly. All flow components are computed using 
! the equivalent circuit model proposed by Ishizaka and Flanagan(1972).
!
! The vocal fold vibration and airflow results in this model have been
! validated against experimental measurements and 3D compressible flow
! simulations (Yoshinaga and Zhang, 2025).
!
! Input files: 
!        -Parameter file: param.txt
!        -COMSOL output: eigenmodes (.vtu) and frequencies (.txt)
!        -Surface point list: surface.txt
!
! Output files: 
!        -Unstructured grid files (result/deform***.vtu)
!        -Displacement history data (result/history***)   
!        -Flowrate (result/flowrate.txt)
!        -Mouth pressure (result/pressure.txt)
!   
! Module file: variMode
!*******************************************************************
      use variMode
      implicit none
      integer i,j,k,istep
      integer d(8),ti,tf,tr,tmax
      
      call date_and_time(values=d)
      call system_clock(ti)

      write(*,'("Solver: modeFold start")')
      write(*,'(a,i0,a,i0.2,a,i0.2,a,i0.2,a,i0.2)')&
              'Date: ',d(1),'/',d(2),'/',d(3),', Time:',d(5),':',d(6)

      !initial reading files
      call readParam
      call readFreq
      call readVTK

      !initial preparation
      call surfExtract
      call surfArea
      call initia
     
      !------------------------------------------------------
      !start time loop
      write(*,*)"!! Start time loop !!"
      do istep=2,nstep

          call step(istep)

          if (mod(istep,nwrite).eq.0)then             
             write(*,*)"MinArea: ",minHarea,",  Ug: ",Ug(istep)

             if(noutfmt .eq. 2)then
                  call calcSDF
                  call writeVTK2(istep/nwrite)
             elseif(noutfmt .eq. 1)then
                  call writeVTK(istep/nwrite)
             endif

          endif

      enddo
      write(*,*)"!! End time loop !!"
      write(*,'()')
      !end time loop
      !------------------------------------------------------

      !output results
      call output

      !output date and time
      call date_and_time(values=d)
      call system_clock(tf,tr,tmax)

      write(*,*) 'Successfully modeFold DONE !!'
      write(*,'(a11,i0,a,i0.2,a,i0.2,a,i0.2,a,i0.2)')&
            &' End time: ',d(1),'/',d(2),'/',d(3),'  ',d(5),':',d(6)
      if (tf < ti) then
         write(*,'(a,f10.1)')' Duration (s):',((tmax-ti)+tf+1)/dble(tr)
      else
         write(*,'(a,f10.1)')' Duration (s):',(tf-ti)/dble(tr)
      endif

  end program main
