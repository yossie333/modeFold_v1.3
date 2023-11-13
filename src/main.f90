program main
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.1
! 2023/Nov/13    by  Tsukasa Yoshinaga
! 
! This program calculate the vocal fold oscillation from the
! eigenmodes obtained from the COMSOL eigenanalysis.
! The external forces can be chosen as forced oscillation or
! 1D Bernoulli's equation.
!
! In this version, the vocal fold shape was detected in a
! structured grids. The calcSDF calculates the inside and 
! outside of the vocal fold region and calculates the distance
! from the wall. The structured grids were outputed by 
! writeVTK2. Param file was used to determin the grid sizes.
!
! Input files: Parameter file (param.txt)
!              COMSOL output (VTK file)
!                            (frequency text)
!              Surface point list (surface.txt)
! Output files: Structured grid files (result/grid***.vtu)
!               flowrate (result/flowrate.txt)
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

          if(noutfmt .eq.2)then
             call calcSDF
          endif

          if (mod(istep,nwrite).eq.0)then             
             write(*,*)"MinArea: ",minHarea,",  Ug: ",Ug(istep)

             if(noutfmt .eq. 2)then
                  call writeVTK2(istep/nwrite)
                  !call writeVTK3(istep/nwrite)
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
