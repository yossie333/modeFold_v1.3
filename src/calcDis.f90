subroutine calcDis
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculates the disspation forces.
! The equation is based on Avanzini and Walstjin (2004)
! The dissipation force is added when the future displacement 
! is in the contact position. The dissipation is calculated 
! as the accerelation and unit mass.
! if the contactflg become 1, the contact calculation continues
! until the contact stops.
!*******************************************************************
        use variMode
        implicit none
        integer i,j
        double precision vc

        do i=2,nxsup
           do j=2,nsurfz-1
               fdis(i,j)=0.d0
               if (v(surfp(i,j)+1) .le. ymid .and. vf(surfp(i,j)+1) .gt. ymid) then
                  !velocity
                  vc = (vf(surfp(i,j)+1) - v(surfp(i,j)+1))/dt
                  ! mm/s -> m/s
                  vc = vc/1000.d0

                  ! force = mass * accerelation
                  fdis(i,j) = -mass*vc/dt
                  fy(i,j) = fy(i,j) + fdis(i,j)

                  ! flag for contact iteration calculation
                  contactflg=1
               endif
           enddo
        enddo

end subroutine calcDis
