## PabloCompiler
Compiles the Pablo programming language into PabloIR - an intermediate representation based on LLVM IR

####
```
ghci
:l pabloc.hs
compilePablo <inputPath> <outputPath>
```

Example Input (in Pablo)
```
x = 111...
while a | b:
  z = Advance x 1
  if z & b:
    x = 000...
  else:
    y = x ^ ~a
  .
.
z = MatchStar x y
```

Example Output (in Pablo IR)
```
entry:
	%0x = 1
	br wCond0
wBody0:
	%0z = shl %0x, 1
	%0 = and %0z, %b
	cb %0, ifThen1, ifElse1
ifThen1:
	%1x = 0
	br ifEnd1
ifElse1:
	%1 = xor %a, 1
	%0y = xor %1x, %1
	br ifEnd1
ifEnd1:
	%2x = phi [%1x, ifThen1], [%0x, ifElse1]
	%1y = phi [0, ifThen1], [%0y, ifElse1]
	br wCond0
wCond0:
	%3x = phi [%0x, entry], [%1x, ifEnd1]
	%3 = or %a, %b
	cb %3, wBody0, wEnd0
wEnd0:
	%4 = %3x
	%5 = %1y
	%6 = and %4, %5
	%7 = add %6, %5
	%8 = xor %7, %5
	%1z = or %8, %4
	ret

```
