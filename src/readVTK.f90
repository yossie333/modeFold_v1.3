subroutine readVTK
!*******************************************************************
! Fortran program for vocal fold oscillation: modeFold ver.1.3
! 2025/Oct/23    by Tsukasa Yoshinaga
!
! This program reads COMSOL output VTK files containing
! the geometry and eigenmodes of the vocal folds.
!
! The coordinate system is defined as:
!   x : flow direction
!   y : medial–lateral direction
!   z : spanwise direction
!
! The eigenmode vectors are normalized by their maximum amplitude.
!
! Parameters:
!   nop          : number of grid points
!   noc          : number of cells
!   connect(noc) : grid connectivity
!   offsets      : number of grid points per element
!   types        : element types
!   x, y, z      : initial grid coordinates
!   mode         : eigenmode vectors at each grid up to nmode
!   mmax         : maximum amplitude in the mode
!*******************************************************************
        use variMode
        implicit none
        integer i,j,iunit,itmp(4),imode
        double precision mtmp(3),cj
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

        !lumped mass for each point
        mass = mass / nop
        
        !normalizing
        do imode=1,nmode
           cj = 0.d0
           do i=1,nop
              cj = cj + mode(1,i,imode)**2+mode(2,i,imode)**2+mode(3,i,imode)**2
           enddo
           cj = 1.d0/(sqrt(mass*cj))
           do i=1,nop
              mode(1,i,imode)=mode(1,i,imode)*cj
              mode(2,i,imode)=mode(2,i,imode)*cj
              mode(3,i,imode)=mode(3,i,imode)*cj
           enddo
        enddo

        close(iunit)
        write(*,*)"End reading VTK file"

end subroutine readVTK
