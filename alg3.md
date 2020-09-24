```cryptol
module labs::moje::alg3 where


//Algorythm 3:
/*
s = 0b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000//message
 
Rsq = 0b111010010011111100111010111011011100000101000001011110100010001110010110010101110100000000001000010110001110101110011101001100010000011011101100110001010101000101010111001101000101111001000100
 */

//down xi cd = s^(xi # 0b00000001 # zero # cd)
 
whileDownUp l cu y s Rsq = return
    where
    s1 = down zero 0b00000000 s
    s2 = ups (min (l -length y) Rsq) 0b00000000 s1
    ret = y # s2
    return = if (length s) < l
        then whileDownUp l cu ret s2 Rsq
        else y
       
sqAny l cu Rsq s= x
        where
            y = ups (min l Rsq) cu s
            x = whileDownUp l cu y s Rsq
 
down xi cd s = sa
    where
        sa = s^(xi # 0b00000001 # zero # cd)        
       
//ups yi cu s = xoodoo(s^(zero#cu))//with xoodoo
ups yi cu s = (s^(zero#cu))
 
/*
AbsorbAny X r cd s =  s4
    where
        X1 = head(split`{r}(split X))
        s1 = up zero 0b00000000 s //jak sprawdzić czy up??!!
        s2 = down X1 cd s1
        s4 = AbsorbAny2 X r cd s2 1

AbsorbAny2 X r cd s2 i = s5
    where
        s5 = 
        if i<r then AbsorbAny2 X r cd s3 (i+1)
        where 
            Xi i = split`{r}(split X) @@[i]
            s3 = down (Xi i) cd s2
        else
//K-sekretny klucz         
AbsorbKey K id counter Rkin  Rkout = [mode, Rabs, Rsq, zm1, zm2]
    where 
        mode = "keyed"
        Rabs = Rkin 
        Rsq = Rkout 
        zm1 = 
        if (length K) != 0 then AbsorbAny (K#id#(length id), Rabs, 0b00000010)
        zm2 =
        if counter != 0 then AbsorbAny(counter, 1, 0b00000000)
*/

// inc na poczatku jest 0
// Ii otrzymanie tablicy podzielonej na kawalki I np [[2,5][3,5][1,3]]
// decrypt = 1 lub 0
// save = [] pusta tablica

Crypt I s inc decrypt save = return
    where
        Ii = split`{3} I
        return = OiIi Ii s inc decrypt save

OiIi Ii s inc decrypt save= returnO
    where
    first = iffirstelement inc
    s1 = ups (Ii @ inc) first s
    Oi = (Ii @ inc) ^ s1
    Pi = ifdecrypt decrypt Oi (Ii @ inc)
    z0 = 0b00000000
    s2 = down Pi z0 s1
    ret = [Oi,s2, increment inc]
    out = save # (ret @ 0)
    returnO = if length Ii > inc
        then OiIi Ii s2 (ret @ 2) decrypt out 
        else out


fa f = n
    where
    n = if f ==1 then 1 else 9

iffirstelement inc = element
    where 
    element = if inc == 0
        then 0b10000000
        else 0b00000000

ifdecrypt decrypt Oi Ii = decryptout
    where 
    decryptout = if decrypt == 1
        then Oi
        else Ii


//funsplit X zm = split`{zm}(split (X))


increment x = x+1

```