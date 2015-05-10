Program Mareas
Implicit None


real, dimension (7674):: Altura
real :: k
real :: Max1, Max2, Max3, Max4, Max5
real :: Tiempo1M, Tiempo2M, Tiempo3M, Tiempo4M, Tiempo5M
real :: Min1, Min2, Min3, Min4, Min5
real :: Tiempo1min, Tiempo2min, Tiempo3min, Tiempo4min, Tiempo5min
real :: Max1d, Max2d, Max3d, Max4d, Max5d, Max6d, Max7d
real :: Tiempo1Md, Tiempo2Md, Tiempo3Md, Tiempo4Md, Tiempo5Md, Tiempo6Md, Tiempo7Md
real :: Max1n, Max2n, Max3n, Max4n, Max5n, Max6n, Max7n
real :: Tiempo1Mn, Tiempo2Mn, Tiempo3Mn, Tiempo4Mn, Tiempo5Mn, Tiempo6Mn, Tiempo7Mn 
real :: Periodo1min, Periodo2min, Periodo3min, Periodo4min, PeriodoMin
real :: Periodo1M, Periodo2M, Periodo3M, Periodo4M, PeriodoP
real :: Periodo1d, Periodo2d, Periodo3d, Periodo4d,Periodo5d, Periodo6d, PeriodoD
real :: Periodon1, Periodon2, Periodon3, Periodon4,Periodon5, Periodon6, PeriodoN

   integer :: i 


open (1,file="Mareas.csv")
do i=1,7674
read(1,*) Altura(i)        
end do
close (1)


!Maximos mensuales, evaluado en 5 meses. Desde el 13 de Octubre del 2013 hasta el 12 de Marzo del 2014



Max1=0
do i=1,1440
k= Max1-Altura(i)
if (k<0) then
Max1= Altura(i)
Tiempo1M=i/48.00
end if
end do
 
Max2=0
do i=1441,2880
k= Max2-Altura(i)
if (k<0) then
Max2= Altura(i)
Tiempo2M=i/48.00
end if
end do
 
Max3=0
do i=2881,4320
k= Max3-Altura(i)
if (k<0) then
Max3= Altura(i)
Tiempo3M=i/48.00
end if
end do

Max4=0
do i=4321,5760
k= Max4-Altura(i)
if (k<0) then
Max3= Altura(i)
Tiempo4M=i/48.00
end if
end do

Max5=0
do i=5761,7200
k= Max5-Altura(i)
if (k<0) then
Max5= Altura(i)
Tiempo5M=i/48.00
end if
end do



!Minimos mensuales, evaluados en 5 meses.

Min1=0
do i=1,1440
k= Min1-Altura(i)
if (k>0) then
Min1= Altura(i)
Tiempo1min=i/48.00
end if
end do
 
Min2=0
do i=1441,2880
k= Min2-Altura(i)
if (k>0) then
Min2= Altura(i)
Tiempo2min=i/48.00
end if
end do
 
Min3=0
do i=2881,4320
k= Min3-Altura(i)
if (k>0) then
Min3= Altura(i)
Tiempo3min=i/48.00
end if
end do

Min4=0
do i=4321,5760
k= Min4-Altura(i)
if (k>0) then
Min4= Altura(i)
Tiempo4min=i/48.00
end if
end do

Min5=0
do i=5761,7200
k= Min5-Altura(i)
if (k>0) then
Min5= Altura(i)
Tiempo5min=i/48.00
end if
end do


!Maximos y minimos diarios (intercalados), evaluado en 7 dias.


Max1d=0
do i=18,42
k= Max1d-Altura(i)
if (k<0) then
Max1d= Altura(i)
Tiempo1Md=i/2.0000
end if
end do
 
Max1n=0
do i=43,66
k= Max1n-Altura(i)
if (k<0) then
Max1n= Altura(i)
Tiempo1Mn=i/2.0000
end if
end do

Max2d=0
do i=67,90
k= Max2d-Altura(i)
if (k<0) then
Max2d= Altura(i)
Tiempo2Md=i/2.0000
end if
end do
 
Max2n=0
do i=91,114
k= Max2n-Altura(i)
if (k<0) then
Max2n= Altura(i)
Tiempo2Mn=i/2.0000
end if
end do


Max3d=0
do i=115,138
k= Max3d-Altura(i)
if (k<0) then
Max3d= Altura(i)
Tiempo3Md=i/2.0000
end if
end do

Max3n=0
do i=139,162
k= Max3n-Altura(i)
if (k<0) then
Max3d= Altura(i)
Tiempo3Mn=i/2.00000
end if
end do

Max4d=0
do i=163,186
k= Max4d-Altura(i)
if (k<0) then
Max4d= Altura(i)
Tiempo4Md=i/2.00000
end if
end do

Max4n=0
do i=187,210
k= Max1d-Altura(i)
if (k<0) then
Max4n= Altura(i)
Tiempo4Mn=i/2.00000
end if
end do

Max5d=0
do i=211,235
k= Max5d-Altura(i)
if (k<0) then
Max5d= Altura(i)
Tiempo5Md=i/2.0000
end if
end do

Max5n=0
do i=236,259
k= Max5n-Altura(i)
if (k<0) then
Max5n= Altura(i)
Tiempo5Mn=i/2.0000
end if
end do

Max6d=0
do i=260,283
k= Max6d-Altura(i)
if (k<0) then
Max6d= Altura(i)
Tiempo6Md=i/2.00000
end if
end do

Max6n=0
do i=284,307
k= Max6n-Altura(i)
if (k<0) then
Max6n= Altura(i)
Tiempo6Mn=i/2.00000
end if
end do

Max7d=0
do i=308,331
k= Max7d-Altura(i)
if (k<0) then
Max7d= Altura(i)
Tiempo7Md=i/2.00000
end if
end do

Max7n=0
do i=332,355
k= Max7n-Altura(i)
if (k<0) then
Max7n= Altura(i)
Tiempo7Mn=i/2.00000
end if
end do

!Periodos (diferencias entre maximos o minimos)

Periodo1M= (Tiempo2M-Tiempo1M)
Periodo2M= (Tiempo3M-Tiempo2M)
Periodo3M= (Tiempo4M-Tiempo3M)
Periodo4M= (Tiempo5M-Tiempo4M)
PeriodoP= (Periodo1M+Periodo2M+Periodo3M+Periodo4M)/4

Periodo1min= (Tiempo2min-Tiempo1min)
Periodo2min= (Tiempo3min-Tiempo2min)
Periodo3min= (Tiempo4min-Tiempo3min)
Periodo4min= (Tiempo5min-Tiempo4min)
PeriodoMin= (Periodo1min+Periodo2min+Periodo3min+Periodo4min)/4

Periodo1d= (Tiempo2Md-Tiempo1Md)
Periodo2d= (Tiempo3Md-Tiempo2Md)
Periodo3d= (Tiempo4Md-Tiempo3Md)
Periodo4d= (Tiempo5Md-Tiempo4Md)
Periodo5d= (Tiempo6Md-Tiempo5Md)
Periodo6d= (Tiempo7Md-Tiempo6Md)

PeriodoD=(Periodo1d+Periodo2d+Periodo3d+Periodo4d+Periodo5d+Periodo6d)/7


Periodon1= (Tiempo2Mn-Tiempo1Mn)
Periodon2= (Tiempo3Mn-Tiempo2Mn)
Periodon3= (Tiempo4Mn-Tiempo3Mn)
Periodon4= (Tiempo5Mn-Tiempo4Mn)
Periodon5= (Tiempo6Mn-Tiempo5Mn)
Periodon6= (Tiempo7Mn-Tiempo6Mn)

PeriodoN=(Periodon1+Periodon2+Periodon3+Periodon4+Periodon5+Periodon6)/7


Print *,"Marea maxima de primer mes en:",  Max1, "en tiempo=", Tiempo1M
Print *,"Marea minima de primer mes en:",  Min1, "en tiempo=", Tiempo1min
Print *,"Marea maxima de segundo mes en:",  Max2, "en tiempo=", Tiempo2M
Print *,"Marea minima de segundo mes en:",  Min2, "en tiempo=", Tiempo2min
Print *,"Marea maxima de tercer mes en:",  Max3, "en tiempo=", Tiempo3M
Print *,"Marea minima de tercer mes en:",  Min3, "en tiempo=", Tiempo3min
Print *,"Marea maxima de cuarto mes en:",  Max4, "en tiempo=", Tiempo4M
Print *,"Marea minima de cuarto mes en:",  Min4, "en tiempo=", Tiempo4min
Print *,"Marea maxima de quinto mes en:",  Max5, "en tiempo=", Tiempo5M
Print *,"Marea minima de quinto mes en:",  Min5, "en tiempo=", Tiempo5min

Print *,"Periodo de mareas maximas de 13 de Octubre 2013  al 12 de Marzo del 2014", Periodo1M, Periodo2M, Periodo3M, Periodo4M
Print *,"Periodo promedio de mareas maximas en estos 5 meses", PeriodoP

Print *,"Periodo de mareas minimas de 13 de Octubre 2013  al 12 de Marzo del 2014", Periodo1min, Periodo2min, Periodo3min, Periodo4min
Print *,"Periodo promedio de mareas minimas  en estos 5 meses", PeriodoMin

Print *,"Marea maxima diurna del primer dia en:",  Max1d, "en tiempo=", Tiempo1Md
Print *,"Marea maxima nocturna del primer dia en:",  Max1n, "en tiempo=", Tiempo1Mn
Print *,"Marea maxima diurna del segundo dia en:",  Max2d, "en tiempo=", Tiempo2Md
Print *,"Marea maxima nocturna del segundo dia en:",  Max2n, "en tiempo=", Tiempo2Mn
Print *,"Marea maxima diurna del tercer dia en:",  Max3d, "en tiempo=", Tiempo3Md
Print *,"Marea maxima nocturna del tercer dia en:",  Max3n, "en tiempo=", Tiempo3Mn
Print *,"Marea maxima diurna del cuarto dia en:",  Max4d, "en tiempo=", Tiempo4Md
Print *,"Marea maxima nocturna del cuarto dia en:",  Max4n, "en tiempo=", Tiempo4Mn
Print *,"Marea maxima diurna del quinto dia en:",  Max5d, "en tiempo=", Tiempo5Md
Print *,"Marea maxima nocturna del quinto dia en:",  Max5n, "en tiempo=", Tiempo5Mn
Print *,"Marea maxima diurna del sexto dia en:",  Max6d, "en tiempo=", Tiempo6Md
Print *,"Marea maxima nocturna del sexto dia en:",  Max6n, "en tiempo=", Tiempo6Mn


Print *, "Periodo diruno  de mareas  en 7 dias", Periodo1d, Periodo2d, Periodo3d, Periodo4d,Periodo5d, Periodo6d 
Print *, "Periodo diurno promedio en 7 dias", PeriodoD

Print *, "Periodo nocturno  de mareas  en 7 dias", Periodon1, Periodon2, Periodon3, Periodon4,Periodon5, Periodon6 
Print *, "Periodo nocturno promedio en 7 dias", PeriodoN


end program Mareas



