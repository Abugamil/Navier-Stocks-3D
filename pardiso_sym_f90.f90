!*******************************************************************************
!   Copyright(C) 2006-2012 Intel Corporation. All Rights Reserved.
!   
!   The source code, information  and  material ("Material") contained herein is
!   owned  by Intel Corporation or its suppliers or licensors, and title to such
!   Material remains  with Intel Corporation  or its suppliers or licensors. The
!   Material  contains proprietary information  of  Intel or  its  suppliers and
!   licensors. The  Material is protected by worldwide copyright laws and treaty
!   provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way  without Intel's  prior  express written  permission. No  license
!   under  any patent, copyright  or  other intellectual property rights  in the
!   Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
!   implication, inducement,  estoppel or  otherwise.  Any  license  under  such
!   intellectual  property  rights must  be express  and  approved  by  Intel in
!   writing.
!   
!   *Third Party trademarks are the property of their respective owners.
!   
!   Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
!   this  notice or  any other notice embedded  in Materials by Intel or Intel's
!   suppliers or licensors in any way.
!
!*******************************************************************************
!  Content:
!  Fortran-90 double precision example of solving 3D Poisson problem in a
!  parallelepiped domain using MKL Poisson Library
!
!*******************************************************************************
subroutine Poisson_3D
! Include modules defined by mkl_poisson.f90 and mkl_dfti.f90 header files
use mkl_poisson
use variables
use constants

implicit none

integer nx, ny, nz
parameter(nx=99, ny=99, nz=99)
double precision pi
parameter(pi=3.14159265358979324D0)

integer ix, iy, iz, stat,i,j,k
integer ipar(128)
double precision ax, bx, ay, by, az, bz, lx, ly, lz, hx, hy, hz, xi, yi, zi
double precision cx, cy, cz, c1
double precision dpar(13*(nx+ny)/2+9)
! Note that proper packing of data in right-hand side array f and boundary
! arrays bd_* is automatically provided by the following declarations of arrays
double precision fp(nx+1,ny+1,nz+1)
double precision bd_ax(ny+1,nz+1), bd_bx(ny+1,nz+1)
double precision bd_ay(nx+1,nz+1), bd_by(nx+1,nz+1)
double precision bd_az(nx+1,ny+1), bd_bz(nx+1,ny+1)
double precision q
type(DFTI_DESCRIPTOR), pointer :: xhandle, yhandle
character(6) BCtype

! Defining the parallelepiped domain 0<x<1, 0<y<1, 0<z<1 for 3D Poisson Solver
ax=0.0D0
bx=1.0D0
ay=0.0D0
by=1.0D0
az=0.0D0
bz=1.0D0

!*******************************************************************************
! Setting the coefficient q to 0.
! Note that this is the way to use Helmholtz Solver to solve Poisson problem!
!*******************************************************************************
q=0.0D0

! Computing the mesh size hx in x-direction
lx=bx-ax
hx=lx/nx
! Computing the mesh size hy in y-direction
ly=by-ay
hy=ly/ny
! Computing the mesh size hx in z-direction
lz=bz-az
hz=lz/nz

! Filling in the values of the TRUE solution
! u(x,y,z)=sin(2*pi*x)*sin(2*pi*y)*sin(2*pi*z)+1
! in the mesh points into the array u
! Filling in the right-hand side
! f(x,y,z)=(12*pi*pi)*sin(2*pi*x)*sin(2*pi*y)*sin(2*pi*z)
! in the mesh points into the array f.
! We choose the right-hand side to correspond to the TRUE solution of
! Poisson equation.
! Here we are using the mesh sizes hx, hy, and hz computed before to compute
! the coordinates (xi,yi,zi) of the mesh points
do i=1,nz+1
   do j=1,ny+1
      do k=1,nx+1
         fp(i,j,k)=-((Uon(i+1,j,k)-Uon(i,j,k))/dx+(Von(i,j+1,k)-Von(i,j,k))/dy+(Won(i,j,k+1)-Won(i,j,k))/dz)/dt
      enddo
   enddo
enddo

! Setting the type of the boundary conditions on each surface
! of the parallelepiped domain:
! On the boundary laying on the plane x=0(=ax) Dirichlet boundary condition
! will be used
! On the boundary laying on the plane x=1(=bx) Dirichlet boundary condition
! will be used
! On the boundary laying on the plane y=0(=ay) Neumann boundary condition will be used
! On the boundary laying on the plane y=1(=by) Neumann boundary condition will be used
! On the boundary laying on the plane z=0(=az) Neumann boundary condition will be used
! On the boundary laying on the plane z=1(=bz) Neumann boundary condition will be used
BCtype = 'NDNNNN'

! Setting the values of the boundary function G(x,y,z) that is equal to
! the TRUE solution in the mesh points laying on Dirichlet boundaries
do iy = 1,ny+1
   do iz = 1,nz+1
     bd_ax(iy,iz) = 0.0D0
     bd_bx(iy,iz) = 0.0D0
   enddo
enddo
! Setting the values of the boundary function g(x,y,z) that is equal to
! the normal derivative of the TRUE solution in the mesh points laying on
! Neumann boundaries
do ix = 1,nx+1
   do iz = 1,nz+1
      bd_ay(ix,iz) = 0.0D0
      bd_by(ix,iz) = 0.0D0
   enddo
enddo
do ix = 1,nx+1
   do iy = 1,ny+1
      bd_az(ix,iy) = 0.0D0
      bd_bz(ix,iy) = 0.0D0
   enddo
enddo

! Initializing ipar array to make it free from garbage
do i=1,128
   ipar(i)=0
enddo

! Initializing simple data structures of Poisson Library for 3D Poisson Solver
call d_init_Helmholtz_3D(ax, bx, ay, by, az, bz, nx, ny, nz, BCtype, q, ipar, dpar,&
                                                                                 stat)

! Initializing complex data structures of Poisson Library for 3D Poisson Solver
! NOTE: Right-hand side f may be altered after the Commit step. If you want
! to keep it, you should save it in another memory location!
call d_commit_Helmholtz_3D(fp, bd_ax, bd_bx, bd_ay, bd_by, bd_az, bd_bz, xhandle,&
                                                            yhandle, ipar, dpar, stat)

! Computing the approximate solution of 3D Poisson problem
! NOTE: Boundary data stored in the arrays bd_ax, bd_bx, bd_ay, bd_by, bd_az, bd_bz
! should not be changed
! between the Commit step and the subsequent call to the Solver routine!
! Otherwise the results may be wrong.
call d_Helmholtz_3D(fp, bd_ax, bd_bx, bd_ay, bd_by, bd_az, bd_bz, xhandle,&
                                                            yhandle, ipar, dpar, stat)

! Cleaning the memory used by xhandle and yhandle
call free_Helmholtz_3D(xhandle, yhandle, ipar, stat)
! Now we can use xhandle and yhandle to solve another 3D Poisson problem

! Printing the results
! Watching the error in the plane x=hx
do i=1,nz+1
   do j=1,ny+1
      do k=1,nx+1
         p(i,j,k)=fp(i,j,k)
      enddo
   enddo
enddo
if (c1.ge.0.5D+0) then
   print *, 'The computed solution seems to be inaccurate.'
endif

! Free MKL memory if any was allocated
! Success message to print if everything is OK
! End of the example code
end

