subroutine initia
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.1
! 2023/Nov/12    by  Tsukasa Yoshinaga
! 
! This program defines the initial values for the simulation.
! Variables:
! qi, qdot  : mode displacements and velocity
! qo, qodot : previous time of mode displacements and velocity
! fi        : external force on each mode
! u, v, w   : coordinates for grid points(initial value + displacement)
! Ug        : glottal flow rate 
! nxg,nyg,nzg : number of grids for structured grids
! xgrid, ygrid, zgrid: grid positions
!*******************************************************************
        use variMode
        implicit none
        integer i
        double precision dx,dy,dz

        allocate(fi(nmode),qi(nmode),qidot(nmode))
        allocate(qo(nmode),qodot(nmode))
        allocate(u(nop),v(nop),w(nop))
        allocate(uf(nop),vf(nop),wf(nop))
        allocate(fx(nsurfl,nsurfz),fy(nsurfl,nsurfz),fz(nsurfl,nsurfz))

        do i=1,nmode
            fi(i)=0.d0
            qi(i)=0.d0
            qo(i)=0.d0
            qidot(i)=0.d0
            qodot(i)=0.d0
        enddo
        do i=1,nop
            u(i)=0.d0 + x(i)
            v(i)=0.d0 + y(i)
            w(i)=0.d0 + z(i)
        enddo

        Ug(1) = 0.d0

        if (noutfmt .eq. 2)then
                dx = (xmax-xmin)/dble(nxg-1)
                dy = (ymax-ymin)/dble(nyg-1)
                dz = (zmax-zmin)/dble(nzg-1)
                allocate(xgrid(nxg),ygrid(nyg),zgrid(nzg))
                allocate(sdf(nxg,nyg,nzg))
                allocate(sdf2(nxg,nyg,nzg))

                do i=1,nxg
                   xgrid(i)=xmin+dx*dble(i-1)
                enddo
                do i=1,nyg
                   ygrid(i)=ymin+dy*dble(i-1)
                enddo
                do i=1,nzg
                   zgrid(i)=zmin+dz*dble(i-1)
                enddo

                !dc = sqrt(dx**2+dy**2)
                dc = min(dx,dy,dz)
                write(*,*)"grid size, ",dc
                write(*,'()')
        endif

end subroutine initia
