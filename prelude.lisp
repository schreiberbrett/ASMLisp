(define b (\ label
	(jal zero label)))

(define ifeq (\ r1 r2 do
	(bne r1 r2 end)
	(do)
	(label end)))
	
(define iflt (\ r1 r2 do
	(bge r1 r2 end)
	(do)
	(label end)))
	
(define ifge (\ r1 r2 do
	(blt r1 r2 end)
	(do)
	(label end)))
	
(define ifeq-else (\ r1 r2 do else-do
	(bne r1 r2 else)
	(do)
	(b end)
	(label else)
	(else-do)
	(label end)))
	
(define ifne-else (\ r1 r2 do else-do
	(beq r1 r2 else)
	(do)
	(b end)
	(label else)
	(else-do)
	(label end)))
	
(define iflt-else (\ r1 r2 do else-do
	(bge r1 r2 else)
	(do)
	(b end)
	(label else)
	(else-do)
	(label end)))
	
(define ifge-else (\ r1 r2 do else-do
	(blt r1 r2 else)
	(do)
	(b end)
	(label else)
	(else-do)
	(label end)))
	

(define whileeq (\ r1 r2 do
	(label loop)
	(bne r1 r2 end)
	(do)
	(b loop)
	(end)))
	
(define whilene (\ r1 r2 do
	(label loop)
	(beq r1 r2 end)
	(do)
	(b loop)
	(end)))
	
(define whilelt (\ r1 r2 do
	(label loop)
	(bge r1 r2 end)
	(do)
	(b loop)
	(end)))

(define whilege (\ r1 r2 do
	(label loop)
	(blt r1 r2 end)
	(do)
	(b loop)
	(end)))
	
(define set (\ r1 r2
	(add r1 zero r2)))

(define seti (\ r immediate
	(addi r1 zero immediate)))
	
(define for-i-in-range-internal (\ i start stop do diff
	(set i start)
	(label loop)
	(bge i stop end)
	(do)
	(diff)
	(b start)
	(label end)))
	
(define for-i-in-range-start-stop-skip (\ i start stop skip do
	(for-i-in-range-internal i start stop (\
		(add i i skip)))))
		
(define for-i-in-range-start-stop (\ i start stop do
	(for-i-in-range-internal i start stop (\
		(increment i)))))
		
(define increment (\ r
	(addi r r 1)))
	
(define modify-word (\ address junk f
	(lw junk address)
	(f junk junk)
	(sw junk address)))
	
(define map-words (\ start size j1 j2 j3 f
	(define stop j1)
	(add stop start size)
	
	(define i j2)
	(for-i-in-range-start-stop i start stop (\
		(modify-word i j3 f)))))