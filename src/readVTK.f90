subroutine readVTK
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program read COMSOL output vtk files for shapes and modes.
! The coodinates were changed to x (flow direction)
!                                y (medial-lateral direction)
!                                z (spanwise direction)
! The eigenmdoe vectors were normalized by the maximum value.
! Parameters:
! nop   : number of grid points
! noc   : number of cells
! connect(noc) : connectivity between grids
! offsets      : number of grids for each ellement
! types        : ellement types
! x,y,z : initial grid location
! mode  : eigenmode vectors at each grids up to nmode
! mmax  : maximum value in the mode
!*******************************************************************
        use variMode
        implicit none
        integer i,j,iunit,itmp(4),imode
        double precision mtmp(3)
        character(80)tmp

        iunit = 10
        write(*,'()')
        write(*,*)"Start reading VTK file ",fmode
        open(iunit, file=fmode,status="old")
        read(iunit,'(A)')tmp
        write(*,*)tmp
        read(iunit,'(A)')tmp
        write(*,*)tmp
        read(iunit,'(A)')tmp
        j=1
        do i=1,4
          itmp(i)=index(tmp(j:),'"')+j-1
          j=itmp(i)+1
        enddo
        read(tmp(itmp(1)+1:itmp(2)-1),'(I6)') nop
        read(tmp(itmp(3)+1:itmp(4)-1),'(I6)') noc
        write(*,*)"Number of Point: ",nop
        write(*,*)"Number of Cells: ",noc
        allocate(x(nop),y(nop),z(nop))
        allocate(connect(8,noc))

        ! read grid points
        ! x -> y, y -> z, z -> x
        read(iunit,'(A)')tmp
        write(*,*)tmp
        read(iunit,'(A)')tmp
        write(*,*)tmp
        do i=1,nop
             read(iunit,*)y(i),z(i),x(i)
        enddo
        read(iunit,'(A)')tmp
        write(*,*)tmp
        read(iunit,'(A)')tmp
        write(*,*)tmp

        !read Data Arrays
        read(iunit,'(A)')tmp
        write(*,*)tmp
        read(iunit,'(A)')tmp
        write(*,*)tmp

        i=1
        do
           read(iunit,'(A)')tmp
           if(tmp(1:6) .eq. "</Data")then
                   write(*,*)tmp
                   exit
           endif
           read(tmp,*,end=1)(connect(j,i),j=1,8)
           !1 write(*,*)(connect(j,i),j=1,8)
           1 i=i+1
        enddo
        allocate(offsets(noc),types(noc))

        !read offsets
        read(iunit,'(A)')tmp
        write(*,*)tmp
        do i=1,noc
           read(iunit,*)offsets(i)
        enddo
        read(iunit,'(A)')tmp
        write(*,*)tmp
        !read types
        read(iunit,'(A)')tmp
        write(*,*)tmp
        do i=1,noc
           read(iunit,*)types(i)
        enddo
        read(iunit,'(A)')tmp
        write(*,*)tmp
        read(iunit,'(A)')tmp
        write(*,*)tmp
        read(iunit,'(A)')tmp
        write(*,*)tmp

        !read Modes
        ! x -> y, y -> z, z -> x
        allocate(mode(3,nop,nmode))
        do imode=1,nmode
           read(iunit,'(A)')tmp
           write(*,*)tmp
           do i=1,nop
              read(iunit,*)(mtmp(j),j=1,3)
              mode(2,i,imode)=mtmp(1)
              mode(3,i,imode)=mtmp(2)
              mode(1,i,imode)=mtmp(3)
           enddo
           read(iunit,'(A)')tmp
        enddo
        
        mmax=maxval(mode)
        write(*,*)"max mode value",mmax
        !normalizing
        do imode=1,nmode
           do i=1,nop
              mode(1,i,imode)=mode(1,i,imode)/mmax
              mode(2,i,imode)=mode(2,i,imode)/mmax
              mode(3,i,imode)=mode(3,i,imode)/mmax
           enddo
        enddo

        close(iunit)
        write(*,*)"End reading VTK file"

end subroutine readVTK
