subroutine uf2u
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program replace the displacement predicted by the
! equation of motion. This will be done after the iteration
! when there was contact.
! In addition, the qi and qidot are also stored to qo and qodot.
!*******************************************************************
        use variMode
        implicit none
        integer i

        do i=1,nmode
            qo(i) = qi(i)
            qodot(i) = qidot(i)
        enddo
        do i=1,nop
           u(i) = uf(i)
           v(i) = vf(i)
           w(i) = wf(i)
        enddo

end subroutine uf2u
