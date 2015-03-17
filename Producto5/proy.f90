 !************************************************  
  !This program plots projectile motion of an object.  
  !The program requires user input for initial velocity   
  !and angle of the object.The algorithm uses a time   
  !step of 0.01 second i.e. it calculates object's  
  !location in the x and y plane every 0.01 second.  
  !**********By: Waleed Ishaque, 2013**************  
  program projectile_plot  
       implicit none  
       !Defining constants:  
       real, parameter :: pi = 4.0*atan(1.0) 
       real :: u, a, d, h, t, a_grados  
       real, parameter :: g = 9.81  
       real:: x(1500),y(1500)  

          integer :: i 


       !where g is gravity, pi is "pi"   

       !u is object's initial velocity   

       !a is object's initial angle   

       !t is time during the simulation   

       !x and y are arrays with 150 rows   

       !Seek user input   

       write(*,*) 'Enter angle of projectile (Real)'   

       read *, a_grados   

       write(*,*) 'Enter velocity of projectile (Real)'   
       read *, u   
  
     !Convert angle to radians   

       a = a_grados*pi/180.0
   
          t = 2*u*sin(a)*(1/g)
          h = u*u*sin(a)*sin(a)*(1/(2*g))
          d = u*u*sin(2*a)*(1/g)

          print * , 'Tiempo de vuelo =' , t
          print * , 'Alcance máximo =' , d
          print * , 'Altura máxima =' , h
 

       !open .dat file and start writing on it using the algorithm   

       open(1, file='proj.dat')   

         

       do i=1,100   

            !displacement of object in x and y direction   

            t = (float(i)*0.01)   

          x(i) = u*cos(a)*t   

            y(i) = u*sin(a)*t - 0.5*g*t*t   

            !write output in file "proj.dat" for plotting   

            write(1,*) x(i), y(i)   

            !kill the loop when the object hits the ground   

            if (y(i)<0) exit   

       end do   

       close(1)   

       !close file   

  end program projectile_plot 

