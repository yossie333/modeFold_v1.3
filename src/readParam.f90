subroutine readParam
!*******************************************************************
! Fortran program for vocal fold oscillation: modeFold ver.1.3
! 2025/Oct/23    by Tsukasa Yoshinaga
!
! This program reads simulation parameters from "param.txt".
! The following parameters control the dynamics, output format,
! and external forcing conditions of the vocal fold simulation.
!
! List of parameters:
!   nmode   : number of eigenmodes used
!   nsurfz  : number of surface points along the z-axis
!   nstep   : total number of time steps
!   nwrite  : output interval (number of steps between writes)
!   noutfmt : output format (1 = unstructured, 2 = structured)
!   dt      : time step for numerical integration
!   zeta    : damping coefficient for mode oscillation
!   kc1, kc2: stiffness coefficients for contact forces
!   ncont   : maximum number of iterations for contact calculation
!   ffreq, fmode, fsurf : file names for input data
!   idir, rdir : directories for input and output files
!   isample : flag for sampling surface displacement (1 = enabled)
!   iforce  : type of external force (0 = flow-induced, 1 = sinusoidal)
!   iflow   : flow model type (0 = incompressible, 1 = compressible)
!*******************************************************************
        use variMode
        implicit none
        integer i,iunit
        character(80)tmp

        pi = 4.d0*datan(1.d0)

        iunit = 10
        write(*,'()')
        write(*,*)"Start reading param.txt"

        open(iunit,file="input/param.txt",status="old")
        read(iunit,'(A)')tmp
        write(*,'(A)')tmp

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,'(I4)')nmode
        write(*,'(I4)')nmode
        
        allocate(ff(nmode),omg(nmode))
        
        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,'(I4)')nsurfz
        write(*,'(I4)')nsurfz

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)nstep
        write(*,'(I8)')nstep
        allocate(Ug(nstep),minHareac(nstep))
        
        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)nwrite
        write(*,'(I4)')nwrite

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)noutfmt
        write(*,'(I4)')noutfmt
        if (noutfmt .eq. 2)then
           read(iunit,'(A)')tmp
           write(*,'(A)')tmp
           read(iunit,*)nxg,nyg,nzg
           write(*,'(3I4)')nxg,nyg,nzg

           read(iunit,'(A)')tmp
           write(*,'(A)')tmp
           read(iunit,*)xmin,xmax,ymin,ymax,zmin,zmax
           write(*,'(6E15.5)')xmin,xmax,ymin,ymax,zmin,zmax
        endif

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)dt
        write(*,'(E15.7)')dt
        
        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)zeta
        write(*,'(E15.7)')zeta

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)kc1,kc2
        write(*,'(2E15.7)')kc1,kc2

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)ncont
        write(*,'(I4)')ncont

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,'(A)')ffreq
        ffreq=trim(adjustl(ffreq))
        write(*,'(A)')ffreq

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,'(A)')fmode
        fmode=trim(adjustl(fmode))
        write(*,'(A)')fmode

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,'(A)')fsurf
        fsurf=trim(adjustl(fsurf))
        write(*,'(A)')fsurf

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,'(A)')idir
        idir=trim(adjustl(idir))
        write(*,'(A)')idir
        ffreq=trim(idir) //"/"// ffreq
        fmode=trim(idir) //"/"// fmode
        fsurf=trim(idir) //"/" // fsurf

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,'(A)')rdir
        rdir=trim(adjustl(rdir))
        write(*,'(A)')rdir

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)isample
        write(*,*)isample

        if (isample .eq. 1)then
           read(iunit,'(A)')tmp
           write(*,'(A)')tmp
           read(iunit,*)nsample
           write(*,*)nsample
           allocate(isp(nsample))
           read(iunit,*)(isp(i),i=1,nsample)
         
           read(iunit,'(A)')tmp
           write(*,'(A)')tmp
           read(iunit,*)(offset(i),i=1,3)
           write(*,*)(offset(i),i=1,3)
        endif

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,'(I4)')iforce
        write(*,'(I4)')iforce

        if (iforce .eq. 1) then
            !parameters for sin force
            read(iunit,'(A)')tmp
            write(*,'(A)')tmp
            read(iunit,*)forcef
            write(*,*)forcef

            read(iunit,'(A)')tmp
            write(*,'(A)')tmp
            read(iunit,*)famp
            write(*,*)famp

        elseif(iforce .eq. 0)then
            !parameters for flow force
            read(iunit,'(A)')tmp
            write(*,'(A)')tmp
            read(iunit,*)ps
            write(*,*)ps

            read(iunit,'(A)')tmp
            write(*,'(A)')tmp
            read(iunit,*)rho
            write(*,*)rho

            read(iunit,'(A)')tmp
            write(*,'(A)')tmp
            read(iunit,*)mu
            write(*,*)mu

            read(iunit,'(A)')tmp
            write(*,'(A)')tmp
            read(iunit,*)mass
            write(*,*)mass

            ! switch for 1D flow model
            read(iunit,'(A)')tmp
            write(*,'(A)')tmp
            read(iunit,*)iflow
            write(*,*)iflow

            if (iflow .eq. 1 ) then
               read(iunit,'(A)')tmp
               write(*,'(A)')tmp
               read(iunit,*)Lsp,Asp,Nsecp
               write(*,*)Lsp,Asp,Nsecp

               read(iunit,'(A)')tmp
               write(*,'(A)')tmp
               read(iunit,*)lsg1,asg1
               write(*,*)lsg1,asg1

               read(iunit,'(A)')tmp
               write(*,'(A)')tmp
               read(iunit,*)Lsg,Asg,Nsecg
               write(*,*)Lsg,Asg,Nsecg

               ! cm -> m
               lsp = lsp /100.d0
               lsg = lsg /100.d0
               lsg1 = lsg1 /100.d0
               asg = asg /100.d0 / 100.d0
               asg1 = asg1 /100.d0 / 100.d0
               asp = asp /100.d0 / 100.d0

               read(iunit,'(A)')tmp
               write(*,'(A)')tmp
               read(iunit,*)c0
               write(*,*)c0

            elseif ( iflow .ne. 0 ) then
                    write(*,*)"Error: iflow is needed"
                    stop
            endif
        endif

        close(iunit)

        write(*,*)"End reading param.txt"

end subroutine readParam
