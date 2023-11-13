program main
        implicit none
        integer i,j,k,iunit,ix,iy,iz
        integer nsurfl,nsurfz,nx,ny,nz
        integer tmp(8)
        integer, allocatable::iflg(:,:,:)
        integer, allocatable::iflg2(:,:,:)
        double precision a(3),b(3),c(3),d(3),u(8),dc,distmp,tn
        double precision lg,dx,dy,dz,xmin,xmax,ymin,ymax,zmin,zmax
        double precision, allocatable :: surflx(:),surfly(:),surflz(:)
        double precision, allocatable :: x(:),y(:),z(:)
        double precision, allocatable :: dis(:,:,:)
        character(80) fsurf


        fsurf="surface.txt"
        write(*,*)"Reading ",fsurf

        iunit = 100
        open(iunit,file=fsurf,status="old")

        read(iunit,*)nsurfl
        write(*,*)"Point number along the surface(xy)",nsurfl

        allocate(surflx(nsurfl),surfly(nsurfl))
        do i=1,nsurfl
           read(iunit,*)surfly(i),surflx(i)
        enddo

        close(iunit)

        nsurfz=20
        lg = 17.d0
        allocate(surflz(nsurfz))
        do i=1,nsurfz
           surflz(i)=dble(i-1)*lg/dble(nsurfz-1)
        enddo
        surflz(nsurfz)=lg+1.d-10

        dx = 0.2d0
        dy = 0.2d0
        dz = 0.2d0
        !dc=sqrt(dx**2+dy**2+dz**2)
        dc=sqrt(dx**2+dy**2)
        !dc=min(dx,dy,dz)
        write(*,*)dc

        xmin = -1.d0
        xmax = 11.d0

        ymin = 0.d0
        ymax = 10.d0 

        zmin = 0.d0
        zmax = 17.d0

        nx = nint((xmax-xmin)/dx)+1
        ny = nint((ymax-ymin)/dy)+1
        nz = nint((zmax-zmin)/dz)+1

        allocate(x(nx),y(ny),z(nz))
        allocate(iflg(nx,ny,nz))
        allocate(iflg2(nx,ny,nz),dis(nx,ny,nz))
        do i=1,nx
            x(i)=xmin+dx*dble(i-1)
        enddo
        do i=1,ny
            y(i)=ymin+dy*dble(i-1)
        enddo
        do i=1,nz
            z(i)=zmin+dz*dble(i-1)
        enddo
        
        dis(1:nx,1:ny,1:nz)=10E10
        do k = 1,nz
           do iz = 1,nsurfz-1
              if(z(k) .ge. surflz(iz) .and. z(k).lt.surflz(iz+1))then
                 do j = 1,ny
                    do i = 1,nx
                       do ix = 1,nsurfl-1
                       a(1)=surflx(ix+1)-surflx(ix)
                       a(2)=surfly(ix+1)-surfly(ix)
                       a(3)=surflz(iz)  -surflz(iz)
                       b(1)=surflx(ix+1)-surflx(ix)
                       b(2)=surfly(ix+1)-surfly(ix)
                       b(3)=surflz(iz+1)-surflz(iz)

                       call calcDis(surflx(ix),surfly(ix),surflz(iz),a,b,x(i),y(j),z(k),distmp)

                       dis(i,j,k)=min(dis(i,j,k),distmp)
                       enddo
                    enddo
                 enddo
              endif
           enddo
        enddo
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 dis(i,j,k)=max(min(dis(i,j,k)/dc,1.d0),0.d0)
              enddo
           enddo
        enddo

        open(iunit,file="output.vtk",status="replace")
           write(iunit,'("# vtk DataFile Version 3.0")')
           write(iunit,'("Structured grid")')
           write(iunit,'("ASCII")')
           write(iunit,'("DATASET STRUCTURED_GRID")')
           write(iunit,'("DIMENSIONS ",I4,I4,I4)')nx,ny,nz
           write(iunit,'("POINTS ",I8," float")')nx*ny*nz
           do k=1,nz
               do j=1,ny
                  do i=1,nx
                     write(iunit,'(3E17.5)')x(i),y(j),z(k)
                  enddo
               enddo
           enddo
           write(iunit,'("POINT_DATA ",I8)')nx*ny*nz
           write(iunit,'("SCALARS data float")')
           write(iunit,'("LOOKUP_TABLE default")')
           do k=1,nz
               do j=1,ny
                  do i=1,nx
                     write(iunit,'(I4)')iflg(i,j,k)
                  enddo
               enddo
           enddo
           write(iunit,'("SCALARS data2 float")')
           write(iunit,'("LOOKUP_TABLE default")')
           do k=1,nz
               do j=1,ny
                  do i=1,nx
                     write(iunit,'(E17.5)')dis(i,j,k)
                  enddo
               enddo
           enddo

        close(iunit)

end
subroutine det(a1,b1,c1,a2,b2,c2,u)
        implicit none
        double precision a1(3),b1(3),c1(3),a2(3),b2(3),c2(3)
        double precision u,det1,det2

        det1=a1(1)*b1(2)*c1(3)+a1(2)*b1(3)*c1(1)+a1(3)*b1(1)*c1(2) &
                -a1(3)*b1(2)*c1(1)-a1(2)*b1(1)*c1(3)-a1(1)*b1(3)*c1(2)

        det2=a2(1)*b2(2)*c2(3)+a2(2)*b2(3)*c2(1)+a2(3)*b2(1)*c2(2) &
                -a2(3)*b2(2)*c2(1)-a2(2)*b2(1)*c2(3)-a2(1)*b2(3)*c2(2)

        u = det1/det2

end subroutine det
subroutine  calcDis(x0,y0,z0,a,b,x,y,z,dis)
        implicit none
        double precision x0,y0,z0,p,q,r
        double precision a(3),b(3),x,y,z,dis

        p=a(2)*b(3)-a(3)*b(2)
        q=a(3)*b(1)-b(3)*a(1)
        r=a(1)*b(2)-a(2)*b(1)

        dis=(p*x+q*y+r*z-(p*x0+q*y0+r*z0))/sqrt(p**2+q**2+r**2)

end subroutine calcDis
