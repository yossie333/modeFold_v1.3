subroutine contactForce
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculates the elastic contact force
! The magnitude of the force is based on Zhang (2019)
! The elastic and dissipative forces are based on Avanzini and Walstjin 
! (2004).
! 
!  kc : contact coefficient (Pa/m*rad)
!  omg(1) : first angular eigenfrequency
!
!*******************************************************************
        use variMode
        implicit none
        integer i,j
        double precision ytmp,omg2

        do i=1,nxsup
           do j=2,nsurfz-1
               if (v(surfp(i,j)+1) .gt. ymid) then
                  !mm -> m
                  ytmp = (ymid - v(surfp(i,j)+1))*1.d-3
                  omg2 = omg(1)*omg(1)
                  !contact pressure * area (mm) -> (m)
                  fy(i,j) = fy(i,j) + kc1*omg2*ytmp*(1 + kc2*omg2*ytmp*ytmp)*sarea(i,j)*1.d-6

               endif
           enddo
        enddo


end subroutine contactForce
