subroutine writeVTK3(n)
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
        integer i,j,k,l,m,iunit,n,jtype(13)
        character(80)filename,extention
        character(4) num


        extention = ".vtk"
        write(num,'(I4.4)')n

        write(*,*)"step: ",n*nwrite
        filename= trim(rdir) // "/surf" // num //extention
        write(*,*)"output: ",filename

        iunit=10
        open(iunit,file=filename,status="replace")
           write(iunit,'("# vtk DataFile Version 3.0")')
           write(iunit,'("Unstructured grid")')
           write(iunit,'("ASCII")')
           write(iunit,'("DATASET UNSTRUCTURED_GRID")')
           write(iunit,'("POINTS ",I8," float")')nsurfz*nsurfl
               do l=1,nsurfz
                  do m=1,nsurfl
                     write(iunit,'(3E17.5)')u(surfp(m,l)+1),v(surfp(m,l)+1),w(surfp(m,l)+1)
                  enddo
               enddo
           write(iunit,'("CELLS ",I8,I8)')(nsurfz-1)*(nsurfl-1),(nsurfz-1)*(nsurfl-1)*5
               do l=1,nsurfz-1
                  do m=1,nsurfl-1
                     write(iunit,'(5I4)')4,m-1+nsurfl*(l-1),m+nsurfl*(l-1),m+nsurfl*l,m-1+nsurfl*l
                  enddo
               enddo
           write(iunit,'("CELL_TYPES ",I8)')(nsurfz-1)*(nsurfl-1)
               do i=1,(nsurfz-1)*(nsurfl-1)
                  write(iunit,'(I4)')9
               enddo


        close(iunit)


end subroutine writeVTK3
