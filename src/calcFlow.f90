subroutine calcFlow(n)
!*******************************************************************
! Fortran program for vocal fold oscillation: modeFold ver.1.3
! 2025/Oct/23    by Tsukasa Yoshinaga
! 
! This program calculate the flow in the 1D flow channel described
! in Ishizaka and Flanagan (1972). The calculation is based on 
! equivalent circuit model.
! The downstream pressure computation was updated.
!
!  flow rate upstream: Uu, downstream: Ud
!  pressure  upstream: Pu, downstream: Pd
!  Ug is calculated with newton-raphson method.
!  Pg: subglottal pressure
!
!  Pout: radiation pressure at mouth
!*******************************************************************
        use variMode
        implicit none
        integer i,j,n
        double precision Lg1,Rk1,Rv1
        double precision F,Fd

        !calculate subglottal flow
        !flowrate difference
        do j=1,Nsecg
           Pu(j)=Pu(j) + (Uu(j)-Uu(j+1))
        enddo
           Pu(Nsecg+1)=Pu(Nsecg+1) + (Uu(Nsecg+1)-Ug(n-1))
           Pu(Nsecg+2)=Pu(Nsecg+2) + (Ug(n-1)-Ud(1))

        !flowrate upstream tract
        Uu(1)=Uu(1)-dt/Lui*(dt/Cui*Pu(1)-Ps)
        Uu(2)=Uu(2)-dt/(Lui+Lu)*(dt/Cu*Pu(2)-dt/Cui*Pu(1)+R2*Uu(2))
        do j=3,Nsecg+1
           Uu(j)=Uu(j)-dt/(2.d0*Lu)*(dt/Cu*Pu(j) &
                   -dt/Cu*Pu(j-1)+R2*Uu(j))
        enddo

        !calculate glottal flow rate
        if (minHarea .gt. 0.d0 )then
                Lg1 = rho*0.5d0*lis/minHarea 
                Rk1 = beta*rho/minHarea**2
                Rv1 = 12.d0*mu*lg**2*lis/minHarea**3
                !newton raphson method
                do i=1,100
                   F=Rk1*abs(Ug(n))*Ug(n)+Rv1*Ug(n)+(Lg1+La+Lu)*(Ug(n)-Ug(n-1))/dt &
                           +dt/Ca*Pu(Nsecg+2)-dt/Cu*Pu(Nsecg+1)
                   if(abs(F) < 1.0d-9) exit
                   Fd = 2.d0*Rk1*Ug(n)+Rv1+(Lg1+La+Lu)/dt
                   Ug(n) = Ug(n) -F/Fd
                enddo

        else
                Ug(n) = 0.d0
        endif

        !subglottal pressure
        Pg(n)=dt/Cu*Pu(Nsecg+1)

        !update vocal tract pressure
        do i=1,Nsecp
           if (i .eq. 1)then
                   Pd(i) = Pd(i) + (dt/Ca)*(Ug(n) - Ud(i))
           else
                   Pd(i) = Pd(i) + (dt/Ca)*(Ud(i-1) - Ud(i))
           endif
        enddo
        !saving output pressure
        Pout(n) = Pd(Nsecp)
        
        !update vocal tract flowrate
        do i=1,Nsecp-1
            Ud(i) = Ud(i) + dt/La * (Pd(i) - Pd(i+1))
        enddo
        Ud(Nsecp) = (Ud(Nsecp)*(La+Lr)/dt + (Pd(Nsecp-1) - 0.d0))/((La + Lr)/dt + Rr);
        

 end subroutine calcFlow
