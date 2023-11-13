subroutine mode2uf
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculate the future displacements from the 
! mode displacement qi. The future displacement will be
! replaced to u,v,w in the subroutine uf2u
!
!*******************************************************************
        use variMode
        implicit none
        integer i,j,imode

        do i=1,nop
           uf(i)=0.d0
           vf(i)=0.d0
           wf(i)=0.d0
           do imode=1,nmode
              uf(i) = uf(i) + qi(imode)*mode(1,i,imode)
              vf(i) = vf(i) + qi(imode)*mode(2,i,imode)
              wf(i) = wf(i) + qi(imode)*mode(3,i,imode)
           enddo
           uf(i)=uf(i)+x(i)
           vf(i)=vf(i)+y(i)
           wf(i)=wf(i)+z(i)
        enddo


end subroutine mode2uf
