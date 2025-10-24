subroutine surfExtract
!*******************************************************************
! Fortran program for vocal fold oscillation: modeFold ver.1.3
! 2025/Oct/23    by Tsukasa Yoshinaga
! 
! This program extract surface point data from surface coordinate
! read from surface.txt.
! Input: surface.txt
! nsurfl : number of points along the surface
! nsurfz : number of points in spanwise direction
! surflx, surfly, surflz: coordinates along the surface
!
! nos  : number of surface element
! lg   : glottal length (Lg)
!
! surfp(nsurfl,nsurfz) : grid number for surface points
!
! subroutine heapsort: sorting the element number from small to large
!********************************************************************
        use variMode
        implicit none
        integer i,j,k,iunit,flg
        double precision dz,e

        ! read surface.txt for surface extraction
        iunit=10
        write(*,'()')
        write(*,*)"Reading ",fsurf
        open(iunit,file=fsurf,status="old")
        read(iunit,*)nsurfl
        write(*,*)"Point number along the surface(xy)",nsurfl

        allocate(surflx(nsurfl),surfly(nsurfl))
        do i=1,nsurfl
           read(iunit,*)surfly(i),surflx(i)
        enddo


        !counting for surface number from connectivity
        nos=0
        do i=1,noc
           if(types(i) .eq. 9)then
                   nos=nos+1
           endif
        enddo
           
        allocate(surfl(nos*4))
        k=1
        do i=1,noc
            if (types(i) .eq. 9) then
                 do j=1,4
                    surfl((k-1)*4+j)=connect(j,i)
                 enddo
                 k=k+1
            endif
        enddo

        !sorting grid number in the surface
        call heapsort(nos*4,surfl)
        
        !deleting same number
        j=1
        do i=2,nos*4
           if(surfl(i).ne.surfl(j))then
                   j=j+1
                   surfl(j)=surfl(i)
           endif
        enddo
        ! nos <- replacing number of surface point
        ! surfl <- surface point list (all)
        nos=j

        !define the z-coordinates
        allocate(surflz(nsurfz))
        lg = maxval(z)
        write(*,*)"Maximum spanwise length Lg ",lg
        dz = dble(nint(lg / dble(nsurfz-1)*1E6))/1E6
        write(*,*)"interval dz ",dz

        do i=1,nsurfz
           surflz(i)= dble(nint(dble(i-1)*dz*1E6))/1E6
        enddo

        
        !search point number along surface ->surfp
        e = 1.d-3
        allocate(surfp(nsurfl,nsurfz))
        do j=1,nsurfz
             do i=1,nsurfl
                flg=0
                do k=1,nos
                    if(abs(x(surfl(k)+1)-surflx(i)) .lt. e .and. &
                       abs(y(surfl(k)+1)-surfly(i)) .lt. e .and. &
                       abs(z(surfl(k)+1)-surflz(j)) .lt. e) then
                       surfp(i,j)=surfl(k)
                       flg=flg+1
                       if(flg.ge.2)then
                          write(*,*)"Error: detecting two points!"
                       endif
                       
                     endif
                enddo
                if(flg .eq. 0)then
                        write(*,*)"Error: couldn't find the point!"
                        write(*,*)surflx(i),surfly(i),surflz(j)
                        stop
                endif
             enddo
         enddo

        do i=1,nsurfl
           z(surfp(i,1)+1)=z(surfp(i,1)+1)-1.0E-10
           z(surfp(i,nsurfz)+1)=z(surfp(i,nsurfz)+1)+1.0E-10
           write(*,*)"slightly adjusting z: ",z(surfp(i,nsurfz)+1)
        enddo
        do i=1,nsurfz
           y(surfp(1,i)+1)=-1.0E-10
           y(surfp(nsurfl,i)+1)=-1.0E-10
        enddo
        ! list for surface points       
        ! do j=1,nsurfz
        !    do i=1,nsurfl
        !       write(*,*)x(surfp(i,j)+1),y(surfp(i,j)+1),z(surfp(i,j)+1)
        !    enddo
        ! enddo
        
        write(*,*)"End surface extract"
        write(*,'()')

end subroutine surfExtract 
