Program circle_area
Implicit None
Real *8 :: radius, circum, area
Real *8 :: PI = 4.0 * atan(1.0)
Integer :: model_n = 1
print *, 'enter a radius:'
read (*,*) radius
circum = 2.0 * PI * radius
area = radius * radius * PI
print *, 'Program number =' , model_n
print *, 'radius =' , radius
print *, 'circumference =' , circum
print *, 'area =' , area
end Program circle_area 
