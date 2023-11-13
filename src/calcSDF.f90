subroutine calcSDF
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.1
! 2023/Nov/12    by  Tsukasa Yoshinaga
! 
! This module detect the inside of vocal folds and calculates 
! the SDF values.
! SDF: 1 when the grids are inside, 0 when outside
! SDF2: distance values from the wall. if this exceeds 1, 
!       the value is replaced by 1.
! The inside is detected by Crossing Number Algorithm.
! If the line crosses the wall even times, it is outside.
!*******************************************************************
        use variMode
        implicit none
        integer i,j,k,l,m,iflg,iflg2
        double precision a(3),b(3),c(3),d(3),e(8)
        double precision one

        !initializing
        !SDF(1:nxg,1:nyg,1:nzg)=1.0E10
        SDF(1:nxg,1:nyg,1:nzg)=0.d0
        SDF2(1:nxg,1:nyg,1:nzg)=1.0d10
        one=1.d0+1.0d-12

        do k=1,nzg
           do l=1,nsurfz-1
              if(zgrid(k) .ge. minval(w(surfp(1:nsurfl,l)+1)) .and. zgrid(k) .lt. maxval(w(surfp(1:nsurfl,l+1)+1)))then
                 do j=1,nyg
                    do i=1,nxg
                       do m=1,nsurfl-1
                       
                          a(1)=u(surfp(m+1,l)+1)-u(surfp(m,l)+1)
                          a(2)=v(surfp(m+1,l)+1)-v(surfp(m,l)+1)
                          a(3)=w(surfp(m+1,l)+1)-w(surfp(m,l)+1)
                          b(1)=u(surfp(m+1,l+1)+1)-u(surfp(m,l)+1)
                          b(2)=v(surfp(m+1,l+1)+1)-v(surfp(m,l)+1)
                          b(3)=w(surfp(m+1,l+1)+1)-w(surfp(m,l)+1)
                          c(1)=-1.d0/(dc*6)
                          c(2)=0.d0
                          c(3)=0.d0
                          d(1)=xgrid(i)-u(surfp(m,l)+1)
                          d(2)=ygrid(j)-v(surfp(m,l)+1)
                          d(3)=zgrid(k)-w(surfp(m,l)+1)
                       
                          call det(d,b,c,a,b,c,e(1))
                          call det(a,d,c,a,b,c,e(2))
                          call det(a,b,d,a,b,c,e(4))
                          e(3)=e(1)+e(2)
                       
                          a(1)=u(surfp(m+1,l+1)+1)-u(surfp(m,l)+1)
                          a(2)=v(surfp(m+1,l+1)+1)-v(surfp(m,l)+1)
                          a(3)=w(surfp(m+1,l+1)+1)-w(surfp(m,l)+1)
                          b(1)=u(surfp(m,l+1)+1)-u(surfp(m,l)+1)
                          b(2)=v(surfp(m,l+1)+1)-v(surfp(m,l)+1)
                          b(3)=w(surfp(m,l+1)+1)-w(surfp(m,l)+1)
                       
                          call det(d,b,c,a,b,c,e(5))
                          call det(a,d,c,a,b,c,e(6))
                          call det(a,b,d,a,b,c,e(8))
                          e(7)=e(5)+e(6)
                       
                         ! iflg=int((sum(pack(tmp(1:4),e(1:4)>=0.d0))+sum(pack(tmp(1:3),e(1:3)<=1.d0)))/7)+ &
                         !          int((sum(pack(tmp(5:8),e(5:8)>=0.d0))+sum(pack(tmp(5:7),e(5:7)<=1.d0)))/7)
                         ! iflg2=int((sum(pack(tmp(1:3),e(1:3)>=0.d0))+sum(pack(tmp(1:3),e(1:3)<=1.d0)))/6)+ &
                         !          int((sum(pack(tmp(5:7),e(5:7)>=0.d0))+sum(pack(tmp(5:7),e(5:7)<=1.d0)))/6)
                          iflg=int((0.5d0*(sign(1.d0,e(1))+1.d0)      +0.5d0*(sign(1.d0,e(2))+1.d0)+     &
                                    0.5d0*(sign(1.d0,e(3))+1.d0)      +0.5d0*(sign(1.d0,e(4))+1.d0)+     &
                                    0.5d0*(sign(1.d0,one-e(1))+1.d0) +0.5d0*(sign(1.d0,one-e(2))+1.d0)+  &
                                    0.5d0*(sign(1.d0,one-e(3))+1.d0)                            )/7.d0)+ &
                               int((0.5d0*(sign(1.d0,e(5))+1.d0)      +0.5d0*(sign(1.d0,e(6))+1.d0)+     &
                                    0.5d0*(sign(1.d0,e(7))+1.d0)      +0.5d0*(sign(1.d0,e(8))+1.d0)+     &
                                    0.5d0*(sign(1.d0,one-e(5))+1.d0) +0.5d0*(sign(1.d0,one-e(6))+1.d0)+  &
                                    0.5d0*(sign(1.d0,one-e(7))+1.d0)                            )/7.d0)
                          iflg2=max(int((0.5d0*(sign(1.d0,e(1))+1.d0)+0.5d0*(sign(1.d0,e(2))+1.d0)+        &
                                         0.5d0*(sign(1.d0,e(3))+1.d0)+                                     &
                                         0.5d0*(sign(1.d0,one-e(1))+1.d0)+0.5d0*(sign(1.d0,one-e(2))+1.d0)+&
                                         0.5d0*(sign(1.d0,one-e(3))+1.d0))/6.d0),                          &
                                    int((0.5d0*(sign(1.d0,e(5))+1.d0)+0.5d0*(sign(1.d0,e(6))+1.d0)+        &
                                         0.5d0*(sign(1.d0,e(7))+1.d0)+                                     &  
                                         0.5d0*(sign(1.d0,one-e(5))+1.d0)+0.5d0*(sign(1.d0,one-e(6))+1.d0)+&
                                         0.5d0*(sign(1.d0,one-e(7))+1.d0))/6.d0))
                          
                          sdf(i,j,k)=sdf(i,j,k)+dble(iflg)
                          sdf2(i,j,k)=min(1.0d10*dble(1-iflg2)+abs(e(4))*dble(iflg2),sdf2(i,j,k))

                       enddo

                       !sdf(i,j,k)=merge(0.d0, sdf(i,j,k),sdf(i,j,k)>=2.d0)
                       sdf(i,j,k)=sdf(i,j,k)*0.5d0*(sign(1.d0,1.9d0-sdf(i,j,k))+1.d0)
                    enddo
                 enddo
              endif
           enddo
        enddo

        do k=1,nzg
           do j=1,nyg
              do i=1,nxg
                  sdf2(i,j,k)=min(sdf2(i,j,k)*sdf(i,j,k),1.d0)
              enddo
           enddo
        enddo

end subroutine calcSDF
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
