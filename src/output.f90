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
      integer i,j,k,iunit
      character(80) filename
      character(3) ch3

      iunit=10

      !saving flow rate and glottal area
      filename= trim(rdir) // "/flowrate.txt"
      write(*,*)"Saving ",filename

      open(iunit,file=filename,status="replace")

         do i=1,nstep
           write(10,'(5E17.7)')dt*dble(i),minHareac(i),Ug(i)
         enddo

      close(iunit)
      write(*,'()')

      !saving mouth pressure 
      filename= trim(rdir) // "/pressure.txt"
      write(*,*)"Saving ",filename

      open(iunit,file=filename,status="replace")

         do i=1,nstep
           write(10,'(5E17.7)')dt*dble(i),Pout(i)
         enddo

      close(iunit)
      write(*,'()')

      if (isample .eq. 1)then
         do i=1,nsample
            write(ch3,'(i3.3)')isp(i)
            filename= trim(rdir) // "/history" // ch3
            write(*,*)"Saving ",filename
            write(*,'(A,F10.5,A,F10.5,A,F10.5)')"x= ",x(isp(i)),", y= ",y(isp(i)),", z= ",z(isp(i))
       
            open(iunit,file=filename,status="replace")
              do k=1,nstep
                 write(iunit,'(4F12.7)')dt*dble(k),(his_uvw(j,i,k),j=1,3)
              enddo
            close(iunit)
         enddo
      endif
         
      write(*,'()')

end subroutine output
