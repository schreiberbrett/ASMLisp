(define b (λ label
	(jal zero label)))


(define if (λ predicate j do
	(ifne r1 zero do)))

(define ifeq (λ r1 r2 do
	(bne r1 r2 end)
	(do)
	(label end)))

(define ifne (λ r1 r2 do
	(beq r1 r2 end)
	(do)
	(label end)))

(define iflt (λ r1 r2 do
	(bge r1 r2 end)
	(do)
	(label end)))

(define ifge (λ r1 r2 do
	(blt r1 r2 end)
	(do)
	(label end)))


(define if-else (λ r1 

(define ifeq-else (λ r1 r2 do else-do
	(bne r1 r2 else)
	(do)
	(b end)
	(else)
	(else-do)
	(label end)))

(define ifne-else (λ r1 r2 do else-do
	(beq r1 r2 else)
	(do)
	(b end)
	(else)
	(else-do)
	(label end)))

(define iflt-else (λ r1 r2 do else-do
	(bge r1 r2 else)
	(do)
	(b end)
	(else)
	(else-do)
	(label end)))

(define ifge-else (λ r1 r2 do else-do
	(blt r1 r2 else)
	(do)
	(b end)
	(else)
	(else-do)
	(label end)))




(define whileeq (λ r1 r2 do
	(label loop)
	(bne r1 r2 end)
	(do)
	(b loop)
	(end)))

(define whilene (λ r1 r2 do
	(label loop)
	(beq r1 r2 end)
	(do)
	(b loop)
	(end)))

(define whilegt (λ r1 r2 do
	(label loop)
	(blt r1 r2 end)
	(do)
	(b loop)
	(end)))

(define whilelt (λ r1 r2 do
	(label loop)
	(bne r1 r2 end)
	(do)
	(b loop)
	(end)))


(define set (λ r1 r2
	(add r1 zero r2)))

(define seti (λ r immediate
	(add r zero immediate)))

(define for-in-range-internal (λ i start stop do
	(set i start)
	(label loop)
	(bge i stop end)
	(do)
	(diff)
	(b start)
	(label end)))

(define for-i-in-range-start-stop-skip (λ i start stop skip do
	(for-i-in-range-internal i start stop (λ
		(add i i skip)))))

(define for-i-in-range-start-stop (λ i start stop do
	(for-i-in-range-internal i start stop (λ
		(increment i)))))

(define increment (λ r
	(addi r r 1)))


(define modify-word (λ address junk f
	(lw junk address)
	(f junk)
	(sw junk address)))



(define map-words (λ start size j1 j2 f
	(define stop j1)
	(add stop start size)

	(define i j2)
	(for-i-in-range-start-stop (λ i start stop (λ
		(modify-word i j3 f)))))