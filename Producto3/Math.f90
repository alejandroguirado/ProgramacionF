
! Math . f90 : demo de algunas funciones matematicas en Fortran
! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
Program Mathtest
Real *8 :: x = 1.0 , y, z
y = sin (x)
z = exp (x) + 1.0
print * , x, y, z
End Program Mathtest 
