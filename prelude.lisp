(define b (λ label
	(jal zero label)))


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

(define for-i-in-range (λ i start stop skip do
	(set i start)
	(label loop)
	(bge i stop end)
	(do)
	(add i i skip)
	(b start)
	(label end)))
