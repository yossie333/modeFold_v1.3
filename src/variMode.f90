module variMode
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.1
! 2023/Nov/12    by  Tsukasa Yoshinaga
! 
! This module defines the variables used in this program
!*******************************************************************
        implicit none

        !readParam
        integer nmode,iforce,nstep,nwrite,noutfmt
        double precision pi,forcef,famp,dt,zeta
        double precision rho,Ps,ha,mu
        double precision, allocatable :: Ug(:)
        character(80) ffreq,fmode,fsurf,idir,rdir

        !readFreq
        double precision,allocatable:: ff(:),omg(:)
        
        !readVTK
        integer nop,noc
        integer,allocatable:: connect(:,:),offsets(:),types(:)
        double precision mmax
        double precision,allocatable:: x(:),y(:),z(:)
        double precision,allocatable:: mode(:,:,:)

        !surfExtract
        integer nos,nsurfl,nsurfz
        integer,allocatable:: surfl(:),surfp(:,:)
        double precision lg
        double precision, allocatable:: surflx(:),surfly(:),surflz(:)

        !surfArea
        integer nsep,nxsup
        double precision minHarea,xsup,ymid
        double precision, allocatable:: psurf(:),sarea(:,:),harea(:)
        double precision, allocatable:: degree(:,:,:)

        !initia
        double precision, allocatable:: fi(:),qi(:),qidot(:)
        double precision, allocatable:: qo(:),qodot(:)
        double precision, allocatable:: u(:),v(:),w(:)
        double precision, allocatable:: uf(:),vf(:),wf(:)
        double precision, allocatable:: fx(:,:),fy(:,:),fz(:,:)

        !contact
        integer ncont,contactflg
        double precision kc1,kc2
        double precision, allocatable:: fdis(:,:)

        !writeVTK2
        integer nxg,nyg,nzg
        double precision xmin,xmax,ymin,ymax,zmin,zmax,dc
        double precision, allocatable:: xgrid(:),ygrid(:),zgrid(:),sdf(:,:,:),sdf2(:,:,:)


end module variMode
