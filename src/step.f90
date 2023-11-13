subroutine step(istep)
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program proceeds the step by calculating the runge-kutta scheme
! Subroutines:
!       calcArea:  calculate the area from the current displacement
!       calcForce: calculate force of flow or sinusoidal wave
!       contactForce : calculate elastic contact force
!       f2mode  : convert surface force to mode force fi
!       runge   : calculate equation of motion for each mode
!                 input: qo, qodot
!                output: qi, qidot
!       mode2uf : convert qi to general future coordinates uf
!       calcDis : calculate dissipation force from uf
!       uf2u    : convert uf to current coordinates u 
!*******************************************************************
        use variMode
        implicit none
        integer i,imode,istep,icont

        ! calculate areas along surface
        call calcArea

        ! calculate force from flow 
        call calcForce(dble(istep)*dt,istep)
        
        ! add contact force
        call contactForce

        ! loop for dissipation calculation(calcDis)
        do icont=1,ncont

           !convert force to mode force fi
           call f2mode

           !4th-order runge-kutta
           do imode=1,nmode        
              call runge(fi(imode),qo(imode),qodot(imode),dt,omg(imode),zeta,&
                          qi(imode),qidot(imode))
           enddo

           !calculate displacements
           call mode2uf

           !calculate disspation force for contact
           contactflg=0
           call calcDis
           if (contactflg .eq. 0) exit
        enddo

        !store old and replace displacement
        ! uf -> u
        ! qo <- qi, qodot <- qidot
        call uf2u


        !open(10,file="result/qi.txt",position="append")
        !    write(10,'(5E15.7)')(qi(imode),imode=1,5)
        !close(10)

end subroutine step
