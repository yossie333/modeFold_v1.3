subroutine runge(f,q,qdot,dt,omg,zeta,qf,qfdot)
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculate the equation of motion by 4th order
! Runge-Kutta Scheme. 
!
! input: q, qdot
! output: qf, qfdot
!
! other parameters: omg (eigenmode frequencies), zeta (damping factor),
!                  f (external force), dt (time step)
!
!*******************************************************************
        implicit none
        double precision f,q,qdot,dt,omg,zeta
        double precision qf,qfdot
        double precision L1,L2,L3,L4
        double precision K1,K2,K3,K4

        L1 = dt * qdot
        K1 = (f - 2.d0*zeta*omg*qdot - omg*omg*q)*dt

        L2 = dt * (qdot + K1/2.d0)
        K2 = (f - 2.d0*zeta*omg*(qdot+K1/2.d0) - omg*omg*(q+L1/2.d0))*dt
        
        L3 = dt * (qdot + K2/2.d0)
        K3 = (f - 2.d0*zeta*omg*(qdot+K2/2.d0) - omg*omg*(q+L2/2.d0))*dt
        
        L4 = dt * (qdot + K3)
        K4 = (f - 2.d0*zeta*omg*(qdot+K3) - omg*omg*(q+L3))*dt

        qf = q + (L1 + 2.d0*L2 + 2.d0*L3 + L4)/6.d0
        qfdot = qdot + (K1 + 2.d0*K2 + 2.d0*K3 + K4)/6.d0

end subroutine runge
