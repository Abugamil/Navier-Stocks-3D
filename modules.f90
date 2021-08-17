module constants
   !--------------------------------------------------------------!
   !     dx=dy=dz                                                   !
   !--------------------------------------------------------------!
   integer, parameter :: TypeFloat = selected_real_kind (p=14,r=200)
   !integer, parameter :: single = selected_real_kind(p=6,r=37)
   !integer, parameter :: double = selected_real_kind(p=14,r=200)
   !integer, parameter :: more   = selected_real_kind(p=18)
   !integer, parameter :: extra  = selected_real_kind(p=24,r=1000)
    
   integer, parameter :: MaxM = 100  ! Size By X
   integer, parameter :: MaxN = 100  ! Size By Y
   integer, parameter :: MaxO = 100  ! Size By Y
   integer, parameter :: Mx   = 100  ! Bigger dimmension
   
   real (kind=TypeFloat), parameter :: Re = 1000.0   
   
   integer, parameter :: pm = MaxM-1        !Dimmension for poisson equation
   integer, parameter :: pn = MaxN-1
   integer, parameter :: po = MaxO-1
   integer, parameter :: n = pn*pm*po
   integer, parameter :: nnz = n*4-pn*pm-pm*po-po*pn 
   
   integer, parameter :: MaxTime=500
   
   integer, parameter :: PrintEvery=1
   
   integer, parameter :: CentralDifference = 1       ! Occuracy
   integer, parameter :: ForwardDifference = 2
   integer, parameter :: BackwardDifference = 3
   integer, parameter :: CrossWind = 2
   
   integer, parameter :: iplus12=1
   integer, parameter :: iminus12=2 
   integer, parameter :: jplus12=3
   integer, parameter :: jminus12=4 
   integer, parameter :: kplus12=5
   integer, parameter :: kminus12=6 
   

   real (kind=TypeFloat), parameter :: Cs= 0.0D0 !Cs = 0.17
   
   integer, parameter :: VarU = 1
   integer, parameter :: VarV = 2
   integer, parameter :: VarW = 3
   integer, parameter :: VarU13 = 4
   integer, parameter :: VarV13 = 5
   integer, parameter :: VarW13 = 6
   integer, parameter :: VarU23 = 7
   integer, parameter :: VarV23 = 8
   integer, parameter :: VarW23 = 9
   integer, parameter :: VarP = 10

   real (kind=TypeFloat), parameter :: dx = 1.0/MaxN
   real (kind=TypeFloat), parameter :: dy = 1.0/MaxN
   real (kind=TypeFloat), parameter :: dz = 1.0/MaxN
   real (kind=TypeFloat), parameter :: dt = dx-dx*dx*dx*dx
   
   integer, parameter :: NoObs = 0
   integer, parameter :: ObsSquare = 1
   integer, parameter :: ObsSphere = 2
   integer, parameter :: ObsTriangle = 3   
   
   integer, parameter :: ByX = 1
   integer, parameter :: ByY = 2
   integer, parameter :: ByZ = 2
   
   integer, parameter :: ByXoY = 1
   integer, parameter :: ByXoZ = 2
   integer, parameter :: ByYoZ = 3
      
end module constants
module variables
   use constants
   integer :: Iter
   integer :: Cscheme = ForwardDifference
   integer :: ObsType = 1
   integer :: ObsSize = 10
   
   real (kind=TypeFloat), allocatable :: aUx1(:,:),bUx1(:,:),aUx2(:,:),bUx2(:,:)
   real (kind=TypeFloat), allocatable :: aUy1(:,:),bUy1(:,:),aUy2(:,:),bUy2(:,:)
   real (kind=TypeFloat), allocatable :: aUz1(:,:),bUz1(:,:),aUz2(:,:),bUz2(:,:)   
   real (kind=TypeFloat), allocatable :: aVx1(:,:),bVx1(:,:),aVx2(:,:),bVx2(:,:)
   real (kind=TypeFloat), allocatable :: aVy1(:,:),bVy1(:,:),aVy2(:,:),bVy2(:,:)
   real (kind=TypeFloat), allocatable :: aVz1(:,:),bVz1(:,:),aVz2(:,:),bVz2(:,:)   
   real (kind=TypeFloat), allocatable :: aWx1(:,:),bWx1(:,:),aWx2(:,:),bWx2(:,:)
   real (kind=TypeFloat), allocatable :: aWy1(:,:),bWy1(:,:),aWy2(:,:),bWy2(:,:)
   real (kind=TypeFloat), allocatable :: aWz1(:,:),bWz1(:,:),aWz2(:,:),bWz2(:,:)   
   
   
   real (kind=TypeFloat), allocatable :: U(:,:,:),V(:,:,:),W(:,:,:),P(:,:,:)
   real (kind=TypeFloat), allocatable :: Uon(:,:,:),Von(:,:,:),Won(:,:,:)
   real (kind=TypeFloat), allocatable :: Uo13(:,:,:),Vo13(:,:,:),Wo13(:,:,:)
   real (kind=TypeFloat), allocatable :: Uo23(:,:,:),Vo23(:,:,:),Wo23(:,:,:)
  
   integer, allocatable :: Obstacle(:,:,:)   
   INTEGER, ALLOCATABLE :: iparm( : )
   INTEGER, ALLOCATABLE :: ia( : )
   INTEGER, ALLOCATABLE :: ja( : )
   real (kind=TypeFloat), ALLOCATABLE :: ap( : )
   real (kind=TypeFloat), ALLOCATABLE :: bp( : )
   real (kind=TypeFloat), ALLOCATABLE :: xp( : )
   
end module variables
