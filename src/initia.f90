subroutine initia
!*******************************************************************
! Fortran program for vocal fold oscillation: modeFold ver.1.3
! 2025/Oct/23    by Tsukasa Yoshinaga
! 
! This program defines the initial values for the simulation.
! Variables:
! qi, qdot  : mode displacements and velocity
! qo, qodot : previous time of mode displacements and velocity
! fi        : external force on each mode
! u, v, w   : coordinates for grid points(initial value + displacement)
! Ug        : glottal flow rate
! minHareac : minimum glottal area along the flow direction
! nxg,nyg,nzg : number of grids for structured grids
! xgrid, ygrid, zgrid: grid positions
! When iflow = 1:
! L: inductance (Lu inductance for uppstream, Ld for downstream)
! C: capacitance(Cu capacitance for upstream, Cd for downstream)
! R: resistance(Rr radiation resistance, R2: resistance for supraglottal tract)
!*******************************************************************
        use variMode
        implicit none
        integer i,j
        double precision dx,dy,dz,ell,S,alpha(2)

        allocate(fi(nmode),qi(nmode),qidot(nmode))
        allocate(qo(nmode),qodot(nmode))
        allocate(u(nop),v(nop),w(nop))
        allocate(uf(nop),vf(nop),wf(nop))
        allocate(fx(nsurfl,nsurfz),fy(nsurfl,nsurfz),fz(nsurfl,nsurfz))

        !initial values
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

        !flow rate and area at the first step
        Ug(1) = 0.d0
        minHareac(1) = 0.d0

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

                dc = min(dx,dy,dz)
                write(*,*)"grid size, ",dc
                write(*,'()')
        endif

        !for 1D flow model
        if (iflow .eq. 1)then
                !inductance, capacitance, resistance of each tract
                Lui = rho * Lsg1 / (2.d0 * Asg1) 
                Cui = Lsg1 *  Asg1 / (rho * c0**2)
                ell = Lsg / dble(Nsecg)
                Lu = rho * ell / (2.d0 * Asg) 
                Cu = ell *  Asg / (rho * c0**2)

                !coefficient
                alpha(1) = -2.5d-5*Ps+0.185d0
                alpha(2) = 1.6d-3*Ps+0.6d0
                beta = 1.125d-4*Ps+0.1375d0 

                R2 = alpha(1)/Asp**2 * sqrt(rho*mu*c0)
                ell = Lsp / dble(Nsecp)
                La = rho * ell / Asp
                Ca = (Asp * ell)/(rho * c0**2)

                !radiation loads
                Lr = rho * 1.1d0 * sqrt(Asp/pi) /Asp
                Rr = alpha(2)*rho*c0/(9.d0 * pi**2 * Asp)
                
                !initial pressure and flowrate for supra-and subglottal tract
                allocate(Pd(Nsecp),Ud(Nsecp))
                allocate(Uu(Nsecg+1),Pu(Nsecg+2))
                Pd(1:Nsecp) = 0.d0
                Ud(1:Nsecp) = 0.d0
                Pu(1:Nsecg+2)=0.d0
                Uu(1:Nsecg+1)=0.d0

                allocate(Pout(nstep),Pg(nstep))
                   Pout(1:nstep) = 0.d0
                   Pg(1:nstep) = 0.d0

        endif

        !initialization for sampling
        if (isample .eq. 1)then
           allocate(his_uvw(3,3,nstep))

           do i=1,nsample
              his_uvw(1,i,1)=u(isp(i))+offset(1)
              his_uvw(2,i,1)=-v(isp(i))+offset(2)
              his_uvw(3,i,1)=w(isp(i))+offset(3)
           enddo
        endif

end subroutine initia
