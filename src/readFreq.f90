subroutine readFreq
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program read eigenmode frquencies from COMSOL file.
! File name is defined by param.txt.
! ff  : eigenmode frequency
! omg : angular eigenmode frequency
!
!*******************************************************************
        use variMode
        implicit none
        integer i,iunit
        double precision dummy1,dummy2
        character(80) tmp

        iunit=10
        write(*,'()')
        write(*,*)"Start reading ",ffreq
        open(iunit,file=ffreq,status='old')
        
        do i=1,5
             read(iunit,'(A)')tmp
             write(*,'(A)')tmp
        enddo

        do i=1,nmode
             read(iunit,*)ff(i),omg(i),dummy1,dummy2
             write(*,*)ff(i),omg(i),dummy1,dummy2
        enddo

        close(iunit)
        write(*,*)"End reading ",ffreq

end subroutine readFreq
