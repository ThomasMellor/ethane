program DefineVariables
use accuracy 

        real(ark), dimension(2,2) :: i,c,c2,sxy,s2,s3

	real(ark), dimension(6,2,2) :: E_rep_1
	real :: e, o, a, b
	
	e = 1.0
	o = 0.0
	a = 0.5
	b = Sqrt(3.0)/2.0      

  i = transpose(reshape( (/ e, o, &
                            o, e /), (/ 2, 2/)))
  
    c = transpose(reshape( (/ -a, -b, &
                             b, -a/), (/ 2, 2/)))
  
    c2 = matmul(c,c)
  
    sxy = transpose(reshape( (/ o, e, &
                                e, o /), (/ 2, 2/)))
  
    
    s2 = matmul(c,matmul(c, sxy))
  
    s3 = matmul(c, sxy)
  
   
    E_rep_1(1,:,:) = i
    do j=1,2
      write(*,*) E_rep_1(1,j,1), " ", E_rep_1(1,j,2)
    end do	
    E_rep_1(2,:,:) = c
    E_rep_1(3,:,:) = c2
    E_rep_1(4,:,:) = sxy
    E_rep_1(5,:,:) = s2
    E_rep_1(6,:,:) = s3

end program 
        


