(define main (\
	(define first x1)
	(define second x2)
	(define sum x3)
	(define i x4)
	(define stop x5)
	
	(seti first 1)
	(seti second 1)
	(seti stop 100)
	
	(for-i-in-range i stop (\ 
		(add sum first second)
		(set first second)
		(set second sum)))))
	
	
(define set (\ register value
	(add register zero value)))
	
(define seti (\ register immediate
	(addi register zero immediate)))
	
(define for-i-in-range (\ i stop instruction
	(seti i 0)
	(label loop)
	(bge i stop end)
	(instruction)
	(j loop)
	(label end)))