module RandC

%link C "rand_c.o"
%include C "rand_c.h"

getRandom : Int -> Int -> IO Int
getRandom min max = mkForeign (FFun "random_number" [FInt, FInt] FInt) min max
