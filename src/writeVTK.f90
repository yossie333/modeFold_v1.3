subroutine writeVTK(n)
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program output the VTK mesh file from the current 
! displacement.
! The output file: result/deform****.vtu
! This version only allows to write triangular poles.
!*******************************************************************
        use variMode
        implicit none
        integer i,j,iunit,n,jtype(13)
        character(80)filename,extention
        character(4) num

        !connectivity
        jtype(13)=6
        jtype(5)=3
        jtype(9)=4

        extention = ".vtu"
        write(num,'(I4.4)')n

        write(*,*)"step: ",n*nwrite
        filename= trim(rdir) // "/deform" // num //extention
        write(*,*)"output: ",filename

        iunit=10
        open(iunit,file=filename,status="replace")
        write(iunit,"('<',A,'>')")'VTKFile type="UnstructuredGrid" version="1.0" byte_order="LittleEndian" header_type="UInt64"'
        write(iunit,"('<',A,'>')")'UnstructuredGrid'
        write(iunit,"('<',A,I0,A,I0,A,'>')")'Piece NumberOfPoints= "',nop,'" NumberOfCells= "',noc,'"'
        write(iunit,"('<',A,'>')")'Points'
        write(iunit,"('<',A,'>')")'DataArray type="Float64" Name="Points" NumberOfComponents="3" format="ascii"'

        do i=1,nop
            write(iunit,"(3F10.6)")u(i),v(i),w(i)
        enddo
        write(iunit,"('<',A,'>')")'/DataArray'
        write(iunit,"('<',A,'>')")'/Points'

        write(iunit,"('<',A,'>')")'Cells'
        write(iunit,"('<',A,'>')")'DataArray type="Int64" Name="connectivity" format="ascii"'
        do i=1,noc
           write(iunit,*)(connect(j,i),j=1,jtype(types(i)))
        enddo 
        write(iunit,"('<',A,'>')")'/DataArray'

        write(iunit,"('<',A,'>')")'DataArray type="Int64" Name="offsets" format="ascii"'
        do i=1,noc
            write(iunit,*)offsets(i)
        enddo
        write(iunit,"('<',A,'>')")'/DataArray'

        write(iunit,"('<',A,'>')")'DataArray type="Int64" Name="types" format="ascii"'
        do i=1,noc
            write(iunit,*)types(i)
        enddo
        write(iunit,"('<',A,'>')")'/DataArray'
        write(iunit,"('<',A,'>')")'/Cells'
        write(iunit,"('<',A,'>')")'/Piece'
        write(iunit,"('<',A,'>')")'/UnstructuredGrid'
        write(iunit,"('<',A,'>')")'/VTKFile'

        close(iunit)


end subroutine writeVTK
