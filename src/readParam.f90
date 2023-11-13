subroutine readParam
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.1
! 2023/Nov/12    by  Tsukasa Yoshinaga
! 
! This program read parameters from param.txt
! List for parameters
! nmode  : number of modes
! nsurfz : number of points along z-axis
! nstep  : number of iterations
! nwrite : number of iterations for writing intervals
! noutfmt: output format i=1:unstructured, i=2:structured
! dt     : time step for time integration
! zeta   : dampling coefficient for mode oscillation
! kc1 kc2 : coefficients for contact forces
! ncont  : max number for iteration of contact calculation
! ffreq,fmode,fsurf : file names for reading files
! idir, rdir   : input and result directory
! iforce : flow force(0) or sin force (1)
!*******************************************************************
        use variMode
        implicit none
        integer iunit
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
        allocate(Ug(nstep))
        
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
            read(iunit,*)ha
            write(*,*)ha
        endif

        close(iunit)

        write(*,*)"End reading param.txt"

end subroutine readParam
