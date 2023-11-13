subroutine calcArea
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculate the area for flow channel from the current 
! displacement. In addition, the angle of each face is calculated.
!
!  harea   : area of each position along the flow direction
!   (Note that the area is calculated from the distance from the 
!     mid-plane, and no consideration for asymmetry.)
!
!  degree(1,:,:) : angles along the x-y plane
!  degree(2,:,:) : angles along the y-z plane
!
!*******************************************************************
        use variMode
        implicit none
        integer i,j
        double precision ytmp,ztmp
        double precision dx,dy1,dz,dy2

        do i=1,nxsup
           harea(i)=0.d0
           do j=1,nsurfz-1
              ytmp = ymid - 0.5d0*(v(surfp(i,j)+1) + v(surfp(i,j+1)+1))
              ztmp = abs(w(surfp(i,j+1)+1) - w(surfp(i,j)+1))

              !assumption for symmetry
              harea(i) = harea(i) + 2.d0*ytmp*ztmp
           enddo
        enddo

        do i=2,nxsup-1
           do j=2,nsurfz-1
              dx = 0.5d0*(u(surfp(i+1,j)+1)-u(surfp(i-1,j)+1))
              dy1 = 0.5d0*(v(surfp(i+1,j)+1)-v(surfp(i-1,j)+1))
              dy2 = 0.5d0*(v(surfp(i,j+1)+1)-v(surfp(i,j-1)+1))
              dz = 0.5d0*(w(surfp(i,j+1)+1)-w(surfp(i,j-1)+1))

              degree(1,i,j)=atan(dy1/dx)
              degree(2,i,j)=atan(dy2/dz)
           enddo
        enddo

end subroutine calcArea
