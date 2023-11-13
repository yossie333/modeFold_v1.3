subroutine f2mode
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculate the external mode forces from the
! force on the general coordinates.
!
!*******************************************************************
        use variMode
        implicit none
        integer i,j,imode

        do imode=1,nmode
           fi(imode)=0.d0
           do i=1,nsurfl
              do j=1,nsurfz
                 fi(imode)=fi(imode)+fx(i,j)*mode(1,surfp(i,j)+1,imode)&
                                    +fy(i,j)*mode(2,surfp(i,j)+1,imode)&
                                    +fz(i,j)*mode(3,surfp(i,j)+1,imode)
              enddo
           enddo
        enddo

        !open(10,file="result/fi.txt",position="append")
        !    write(10,'(5E15.7)')(fi(imode),imode=1,5)
        !close(10)

end subroutine f2mode
