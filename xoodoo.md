```cryptol
module labs::moje::xoodoo where

mess M = split`{3}(groupBy`{32}(split (M):[384][1]))


//teta:
A0 M = head(take`{1} (mess M))
A1 M = head(drop`{1}( take`{2}(mess M)))
A2 M = head(drop`{2}( take`{3}(mess M)))

P M = (A0 M)^(A1 M)^(A2 M)

shift8 FUN Z N= T
    where 
        Q = head (FUN) >>> N
        W = head (drop`{1}( take`{2}(FUN))) >>> N
        E = head (drop`{2}( take`{3}(FUN))) >>> N
        Y = head (drop`{3}( take`{4}(FUN))) >>> N
        R = groupBy`{32}(Q#W#E#Y)
        T = (R)>>>Z

tetaE M = (shift8 (P M) 1 5)^(shift8 (P M) 1 14)

tetaA0 M = (A0 M)^(tetaE M)
tetaA1 M = (A1 M)^(tetaE M)
tetaA2 M = (A2 M)^(tetaE M)


//fiW:
fiA1 M = shift8 (tetaA1 M) 1 0
fiA2 M = shift8 (tetaA2 M) 0 11

//pi:
c0 = [0x00000012, 0x000001A0,  0x000000F0,  0x00000380,  0x0000002C,  0x00000060,  0x00000014, 0x00000120,  0x000000D0,  0x000003C0,  0x00000038,  0x00000058]


funtan nr2 = head( c0 @@ [nr2-1])

zm1 nr = groupBy`{32}(split (zero#(funtan nr)):[128][1])

piA0 M nr = (tetaA0 M)^(zm1 nr)

//xi:
B0 M = (~(fiA1 M))&&(fiA2 M)
B1 M nr = (~(fiA2 M))&&(piA0 M nr)
B2 M nr = (~(piA0 M nr))&&(fiA1 M)
xiA0 M nr = (piA0 M nr)^(B0 M)
xiA1 M nr = (fiA1 M)^(B1 M nr)
xiA2 M nr = (fiA2 M)^(B2 M nr)

//fiE:

fieA1 M nr= shift8 (xiA1 M nr) 0 1
fieA2 M nr= shift8 (xiA2 M nr) 2 8

end1 M nr = join(join((xiA0 M nr)#(fieA1 M nr)#(fieA2 M nr)))

decrement (x : [8]) = x - 1

xoodoo M nr = x
    where x = 
            if nr > 1 then xoodoo (join(join((xiA0 M nr)#(fieA1 M nr)#(fieA2 M nr)))) (decrement nr)
            else (join(join((xiA0 M 1)#(fieA1 M 1)#(fieA2 M 1))))

tab = [1,2,3]
fun i = tab@@[i]
```
