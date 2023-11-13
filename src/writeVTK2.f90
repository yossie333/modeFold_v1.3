subroutine writeVTK2(n)
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.1
! 2023/Nov/12    by  Tsukasa Yoshinaga
! 
! This program output the VTK mesh file from the current 
! displacement.
! The output format is structured grids
! The output file: result/deform****.vtu
! This version only allows to write triangular poles.
!*******************************************************************
        use variMode
        implicit none
        integer i,j,k,iunit,n,jtype(13)
        character(80)filename,extention
        character(4) num


        extention = ".vtk"
        write(num,'(I4.4)')n

        write(*,*)"step: ",n*nwrite
        filename= trim(rdir) // "/grid" // num //extention
        write(*,*)"output: ",filename

        iunit=10
        open(iunit,file=filename,status="replace")
           write(iunit,'("# vtk DataFile Version 3.0")')
           write(iunit,'("Structured grid")')
           write(iunit,'("ASCII")')
           write(iunit,'("DATASET STRUCTURED_GRID")')
           write(iunit,'("DIMENSIONS ",I4,I4,I4)')nxg,nyg,nzg
           write(iunit,'("POINTS ",I8," float")')nxg*nyg*nzg
           do k=1,nzg
               do j=1,nyg
                  do i=1,nxg
                     write(iunit,'(3E17.5)')xgrid(i),ygrid(j),zgrid(k)
                  enddo
               enddo
           enddo
           write(iunit,'("POINT_DATA ",I8)')nxg*nyg*nzg
           write(iunit,'("SCALARS SDF float")')
           write(iunit,'("LOOKUP_TABLE default")')
           do k=1,nzg
               do j=1,nyg
                  do i=1,nxg
                     write(iunit,'(E15.5)')SDF(i,j,k)
                  enddo
               enddo
           enddo
           write(iunit,'("SCALARS SDF2 float")')
           write(iunit,'("LOOKUP_TABLE default")')
           do k=1,nzg
               do j=1,nyg
                  do i=1,nxg
                     write(iunit,'(E15.5)')SDF2(i,j,k)
                  enddo
               enddo
           enddo

        close(iunit)


end subroutine writeVTK2
