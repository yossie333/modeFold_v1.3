subroutine calcForce(t,n)
!*******************************************************************
! Fortran program for vocal fold oscillation: modeFold ver.1.3
! 2025/Oct/23    by Tsukasa Yoshinaga
! 
! This program calculate the force on the surface of the vocal
! fold. 
!
! fx, fy, fz: force in the general coordinates
! 
! iforce = 1: force is calculated as the sinusoidal waves. The
! parameters of sinusoidal waves were defined by param.txt
! famp   :  amplitude of sin wave
! forcef :  frequency of the sin wave
! t      :  current time (input)
!
! iforce = 0: force is calculated from the 1D flow equations.
! When iflow = 0:
!    The equations are mainly based on Bernoulli's principle.
!    The glottal flowrate was simply calculated from the minimum area
!       minHarea : the minimum area for flow channel.
!       Ps       : subglottal pressure
!       Ug(n)    : glottal flow at n step 
!       rho      : density of air
!       psurf    : pressure distribution along the surface
! 
! iflow = 1 : using 1D compressible flow model of equivalent circuit 
!             proposed by Ishizaka and Flanagan (1972). 
!             it leads to subroutine calcFlow.
!*******************************************************************
        use variMode
        implicit none
        integer i,j,n
        double precision t,dx,dy,dz,ds,h,Ugm

        do i=1,nsurfl
            do j=1,nsurfz
               fx(i,j)=0.d0
               fy(i,j)=0.d0
               fz(i,j)=0.d0
            enddo
        enddo
        do i=1,nxsup
           do j=1,nsurfz-1
               fdis(i,j)=0.d0
           enddo
        enddo

        if (iforce .eq. 1)then

                minHarea = minval(harea)

                !sinusoical force
                do i=1,nsurfl
                    do j=1,nsurfz
                       !fx(i,j)=famp*sin(2*pi*forcef*t)
                       fy(i,j)=famp*sin(2.d0*pi*forcef*t)
                       !fz(i,j)=famp*sin(2*pi*forcef*t)
                    enddo
                enddo

        elseif(iforce .eq. 0 )then
                !minimum area 
                minHarea = minval(harea)
                minHareac(n) = minHarea

                !calculation of flow rate
                if(iflow .eq. 1)then

                        !minimum area (mm2 -> m2)
                        minHarea = minHarea * 1.d-6
                        !glottal length (mm -> m)
                        lg = lg*1.d-3

                        call calcFlow(n)
                        
                        !(m -> mm)
                        minHarea = minHarea * 1.d+6
                        lg = lg*1.d+3

                else
                        if(minHarea .gt. 0.d0)then
                            Ug(n) = sqrt(2.d0*Ps/rho)*minHarea
                        else
                            Ug(n) = 0.d0
                        endif
                endif

                !initializing pressure
                psurf(1:nsurfl)=0.d0
                !initial subglottal pressure
                if (iflow .eq. 1)then
                   psurf(1) = Pg(n)
                else
                   psurf(1) = Ps 
                endif

                if (minHarea .gt. 0.d0)then
                    !when the glottis is open
                    do i=2,nxsup
                        dx = x(surfp(i,j)+1)-x(surfp(i-1,j)+1)
                        h = (harea(i)+harea(i-1))/(2.d0*lg)

                        if (iflow .eq. 1) then
                           Ugm = Ug(n)*1.d+6
                           psurf(i) = psurf(i-1) + 0.5d0*rho*Ugm**2*(1.d0/harea(i-1)**2-1.d0/harea(i)**2)&
                                                 - 12.d0*mu*dx/(lg*h**3)*Ugm*1000.d0
                        else
                           psurf(i) = psurf(i-1) + 0.5d0*rho*Ug(n)**2*(1.d0/harea(i-1)**2-1.d0/harea(i)**2)&
                                                 - 12.d0*mu*dx/(lg*h**3)*Ug(n)*1000.d0
                        endif

                        !exit with separation point
                        if(psurf(i) .gt. psurf(i-1))then
                                exit
                        endif
                    enddo
                else
                    !when the glottis is closed
                    do i=2,nxsup
                        if(harea(i).le.0.d0)then
                            nsep = i
                            exit
                        endif
                    enddo
                    do i=2,nsep-1
                        psurf(i) = Ps
                    enddo
                endif

                !surface pressure to force mapping
                do i=2,nxsup
                    do j=2,nsurfz-1
                       dx = 0.5d0*(u(surfp(i+1,j)+1)-u(surfp(i-1,j)+1))
                       dy = 0.5d0*(v(surfp(i+1,j)+1)-v(surfp(i-1,j)+1))
                       ds = sqrt(dx**2 + dy**2)
                       dz = 0.5d0*(w(surfp(i,j+1)+1)-w(surfp(i,j-1)+1))

                       !Pa to N (mm2 -> m2)
                       fx(i,j)=psurf(i)*ds*dz*1.d-6*cos(degree(2,i,j))*sin(degree(1,i,j))
                       fy(i,j)=-psurf(i)*ds*dz*1.d-6*cos(degree(2,i,j))*cos(degree(1,i,j))
                       fz(i,j)=psurf(i)*ds*dz*1.d-6*sin(degree(2,i,j))
                    enddo
                enddo

        endif

end subroutine
