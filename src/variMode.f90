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
        integer iflow,Nsecp,Nsecg
        integer isample,nsample
        integer, allocatable :: isp(:)
        double precision pi,forcef,famp,dt,zeta
        double precision rho,Ps,mu,mass
        double precision, allocatable :: Ug(:)
        double precision Asp,Lsp,Asg,Lsg,Asg1,Lsg1
        double precision c0,offset(3)
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
        double precision minHarea,xsup,ymid,lis
        double precision, allocatable:: psurf(:),sarea(:,:),harea(:)
        double precision, allocatable:: degree(:,:,:),minHareac(:)

        !initia
        double precision, allocatable:: fi(:),qi(:),qidot(:)
        double precision, allocatable:: qo(:),qodot(:)
        double precision, allocatable:: u(:),v(:),w(:)
        double precision, allocatable:: uf(:),vf(:),wf(:)
        double precision, allocatable:: fx(:,:),fy(:,:),fz(:,:)
        double precision Lu,Cu,Lui,Cui,R2,La,Ca,Lr,Rr,beta
        double precision, allocatable:: Pd(:),Ud(:)
        double precision, allocatable:: Uu(:),Pu(:)
        double precision, allocatable:: Pout(:),Pg(:)
        double precision, allocatable:: his_uvw(:,:,:)

        !contact
        integer ncont,contactflg
        double precision kc1,kc2
        double precision, allocatable:: fdis(:,:)

        !writeVTK2
        integer nxg,nyg,nzg
        double precision xmin,xmax,ymin,ymax,zmin,zmax,dc
        double precision, allocatable:: xgrid(:),ygrid(:),zgrid(:),sdf(:,:,:),sdf2(:,:,:)


end module variMode
