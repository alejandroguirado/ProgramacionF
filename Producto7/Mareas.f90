Program Mareas
Implicit None


real, dimension (7674):: Altura
real :: Max1, Max2, Max3, k
real :: Tiempo1M, Tiempo2M, Tiempo3M

   integer :: i 


open (1,file="Mareas.csv")
do i=1,7674
read(*,*) Altura(i)        
end do
close (1)

Max1=0
do i=1,1344
k= Max1-Altura(i)
if (k<0) then
Max1= Altura(i)
Tiempo1M=i/48.00
end if
end do
 
Max2=0
do i=1345,2690
k= Max2-Altura(i)
if (k<0) then
Max2= Altura(i)
Tiempo2M=i/48.00
end if
end do
 
Max3=0
do i=2691,4035
k= Max3-Altura(i)
if (k<0) then
Max3= Altura(i)
Tiempo3M=i/48.00
end if
end do



Print *, Max1, Max2, Max3
Print *, Tiempo1M, Tiempo2M, Tiempo3M


end program Mareas
