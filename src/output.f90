subroutine output
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program output the file for flowrate and etc.
!
!*******************************************************************
      use variMode
      implicit none
      integer i,iunit
      character(80) filename

      iunit=10

      filename= trim(rdir) // "/flowrate.txt"
      write(*,*)"Saving ",filename

      open(iunit,file=filename,status="replace")

         do i=1,nstep
           write(10,'(2E17.7)')dt*dble(i),Ug(i)
         enddo

      close(iunit)
      write(*,'()')

end subroutine output
