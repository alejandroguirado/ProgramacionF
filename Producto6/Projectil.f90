!Programa para calcular la posicion de un proyectil en movimiento parabolico, consinderando la friccion y sin friccion.  obtiene el 
module Constantes !Declaracion de constantes
implicit NONE
real, parameter :: rad=(4.0*atan(1.0))/180 ! Conversion de angulos
real, parameter :: pi=4.0*atan(1.0)
integer, parameter :: pts= 7000 !Numero de puntos a calcular
real, parameter :: rf = 1.29
real, parameter :: cfe = 0.47 
end module Constantes


program proyectilf
use Constantes
implicit none
real :: x0,y0,v0,gr0
real :: xmax,ymax,tiempo,xmaxf,ymaxf,tiempof
real :: error
write (*,*), 'Insterte las coordenadas iniciales de x & y en metros'
read *, x0, y0
write (*,*), 'Inserte la velocidad inicial del proyectil en m/s'
read *, v0
write (*,*), 'Insterte el angulo inicial de tiro en grados'
read *, gr0

call sinfriccion (x0,y0,v0,gr0,xmax,ymax,tiempo)
call friccion (x0,y0,v0,gr0,xmaxf,ymaxf,tiempof)

error = ((xmax-xmaxf)/xmaxf * 100)

print *, "Coordenadas en", x0,y0
print *, "Velocidad inicial de", v0,"m/s"
print *, "Angulo de", gr0
print *, "---"
print *, "Sin friccion"
print *, "Tiempo en el aire=", tiempo,"s"
print *, "Altura maxima=", ymax, "m"
print *, "Alcance horizontal=", xmax,"m"
print *, "---"
print *, "Con friccion"
print *, "Tiempo en el aire=", tiempof,"s"
print *, "Altura maxima=", ymaxf,"m"
print *, "Alcance horizontal=", xmaxf,"m"
print *, "Y el error al no considerar ninguna el arrastre  del aire es=", error

end program proyectilf


subroutine sinfriccion  (x0,y0,v0,gr0,xmax,ymax,tiempo)
use Constantes
implicit none 
integer :: I
real, dimension (1:pts) :: x, y, t
real :: x0, y0, v0, gr0
real :: xmax, ymax, tiempo

gr0=gr0*rad
xmax = x0+((v0*v0*sin(2*gr0))/(9.8))
ymax = y0+((v0*v0*sin(gr0)*sin(gr0)))/(19.6)
tiempo= (2*v0*sin(gr0))/(9.8)

open (1, file= "sinfriccion.dat")
do I=1, pts, 1
t(I)= float(I)*0.01
x(I)= x0+(v0*cos(gr0)*t(I))
y(I)= y0+(v0*sin(gr0)*t(I))-(4.9*t(I)*(t(I)))

write (1,1001) x(I), y(I)
1001 format (f11.5,f11.5)

if (y(I)<0) exit
end do

close (1)
end subroutine sinfriccion




subroutine friccion (x0,y0,v0,gr0,xmaxf,ymaxf,tiempof)
use Constantes
implicit none
integer :: I
real, dimension (0:pts) :: p,q,c,velp,velq,lp,lq
real :: x0,y0,v0,gr0 !Entrada 
real :: xmaxf,ymaxf,tiempof !Salida
real :: d, area, radio, masa
print *, "Ingrese la masa en kg"
read *, masa
print *, "Ingrese el radio de la esfera en metros"
read *, radio

area=pi*radio*radio


p(0) = x0
q(0) = y0
velp(0) = v0*cos(gr0)
velq(0) = v0*sin(gr0)
d = (0.5*rf*area*cfe)/masa
lp(0) = -d*velp(0)*velp(0)
lq(0) = 9.8-(d*velq(0)*velq(0))
c(0) = 0

!Registrando valores
open (2, file="friccion.dat")
write (2,1001) p(0), q(0)
1001 format (f11.5,f11.5)

!Calculando posicion para t(I)
do I=0, pts, 1
c(I+1)= c(I)+ 0.01
velp(I+1) = velp(I)+lp(I)*c(I+1) 
velq(i+1) = velq(I)+lq(I)*c(I+1)

lp(I+1) = -d*velp(I)*velp(I)
lq(I+1) = -9.8-(d*velp(I)*velp(I))

p(I+1) = p(I)+velp(I)*c(I+1)+(0.5*lp(I)*c(I+1)*c(I+1))
q(I+1) = q(I)+velq(I)*c(I+1)+(0.5*lq(I)*c(I+1)*c(I+1))
write (2,*) p(I+1), q(I+1)
if (q(I)<0) exit
end do
close (2)

xmaxf = p(I)
ymaxf = MAXVAL(q)
tiempof = c(I)*10
end subroutine friccion



