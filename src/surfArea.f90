subroutine surfArea
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculate the surface area for each surface point
! to calculate the force from the pressure.
! This assume that the area change of surface is small during the
! oscillation.
!
! xsup  : position of the upper face
! nxsup : number of point that reaches the upper face
! (limit for surface and angle calculation for 1D flow)
!
! sarea : element area at surface point
!
!*******************************************************************
        use variMode
        implicit none
        integer i,j
        double precision dx,dy,ds,dz

         !extract medial plane
         ymid = maxval(y)
         write(*,*)"medial plane, y= ",ymid
         !extract superior plane
         xsup = maxval(x)
         write(*,*)"superior plane, x= ",xsup
         nxsup=0
         do i=1,nsurfl
             nxsup=i
             if(x(surfp(i,1)+1) .eq. xsup) then
                     exit
             endif
         enddo
         write(*,*)"superior point, nxsup= ",nxsup

         allocate(harea(nxsup),fdis(nxsup,nsurfz-1))
         allocate(sarea(nxsup,nsurfz-1))
         allocate(degree(2,nxsup,nsurfz-1))
         allocate(psurf(nxsup))

         do i=2,nxsup-1
            do j=2,nsurfz-1
               dx = 0.5d0*(x(surfp(i+1,j)+1)-x(surfp(i-1,j)+1))
               dy = 0.5d0*(y(surfp(i+1,j)+1)-y(surfp(i-1,j)+1))
               ds = sqrt(dx**2 + dy**2)
               dz = 0.5d0*(z(surfp(i,j+1)+1)-z(surfp(i,j-1)+1))
               sarea(i,j) = ds*dz
            enddo
         enddo
         !open(10,file='area.txt',status="replace")
         !   do i=2,nxsup-1
         !    write(10,'(18E15.6)')(sarea(i,j),j=2,nsurfz-1)
         !   enddo
         !close(10)

         write(*,*)"Calculation of surface area end"
         write(*,'()')

end subroutine surfArea
