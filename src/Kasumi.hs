module Kasumi where

import Data.Bits
import Data.List
import Data.Word

-- Helpers
rol16 x 0 = x
rol16 x n = 0xFFFF .&. ((shiftL x n) .|. (shiftR x (16 - n)))

mask_bits :: Word -> Int -> Word
mask_bits x n = let msk = (foldl (\x _ -> 2*x) 1 [1..n]) - 1 in
                    x .&. msk

lower_bits :: Word -> Int -> Int -> Word
lower_bits x l n = mask_bits x n

upper_bits :: Word -> Int -> Int -> Word
upper_bits x l n = mask_bits (shiftR x (l - n)) n

combine_bits :: Word -> Word -> Int -> Int -> Word
combine_bits a b l n = mask_bits ((shiftL a n) .|. (mask_bits b n)) l

-- Constants

s7_data = [ 54, 50, 62, 56, 22, 34, 94, 96, 38,  6, 63, 93,  2, 18,123, 33,
        55,113, 39,114, 21, 67, 65, 12, 47, 73, 46, 27, 25,111,124, 81,
        53,  9,121, 79, 52, 60, 58, 48,101,127, 40,120,104, 70, 71, 43,
        20,122, 72, 61, 23,109, 13,100, 77,  1, 16,  7, 82, 10,105, 98,
        117,116, 76, 11, 89,106,  0,125,118, 99, 86, 69, 30, 57,126, 87,
        112, 51, 17,  5, 95, 14, 90, 84, 91,  8, 35,103, 32, 97, 28, 66,
        102, 31, 26, 45, 75,  4, 85, 92, 37, 74, 80, 49, 68, 29,115, 44,
        64,107,108, 24,110, 83, 36, 78, 42, 19, 15, 41, 88,119, 59,  3 ]

s7 :: Word -> Word
s7 i = s7_data !! fromIntegral i 

s9_data = [ 167,239,161,379,391,334,  9,338, 38,226, 48,358,452,385, 90,397,
        183,253,147,331,415,340, 51,362,306,500,262, 82,216,159,356,177,
        175,241,489, 37,206, 17,  0,333, 44,254,378, 58,143,220, 81,400,
        95 ,  3,315,245, 54,235,218,405,472,264,172,494,371,290,399, 76,
        165,197,395,121,257,480,423,212,240, 28,462,176,406,507,288,223,
        501,407,249,265, 89,186,221,428,164, 74,440,196,458,421,350,163,
        232,158,134,354, 13,250,491,142,191, 69,193,425,152,227,366,135,
        344,300,276,242,437,320,113,278, 11,243, 87,317, 36, 93,496, 27,
        487,446,482, 41, 68,156,457,131,326,403,339, 20, 39,115,442,124,
        475,384,508, 53,112,170,479,151,126,169, 73,268,279,321,168,364,
        363,292, 46,499,393,327,324, 24,456,267,157,460,488,426,309,229,
        439,506,208,271,349,401,434,236, 16,209,359, 52, 56,120,199,277,
        465,416,252,287,246,  6, 83,305,420,345,153,502, 65, 61,244,282,
        173,222,418, 67,386,368,261,101,476,291,195,430, 49, 79,166,330,
        280,383,373,128,382,408,155,495,367,388,274,107,459,417, 62,454,
        132,225,203,316,234, 14,301, 91,503,286,424,211,347,307,140,374,
        35 ,103,125,427, 19,214,453,146,498,314,444,230,256,329,198,285,
        50 ,116, 78,410, 10,205,510,171,231, 45,139,467, 29, 86,505, 32,
        72 , 26,342,150,313,490,431,238,411,325,149,473, 40,119,174,355,
        185,233,389, 71,448,273,372, 55,110,178,322, 12,469,392,369,190,
        1,109,375,137,181, 88, 75,308,260,484, 98,272,370,275,412,111,
        336,318,  4,504,492,259,304, 77,337,435, 21,357,303,332,483, 18,
        47, 85, 25,497,474,289,100,269,296,478,270,106, 31,104,433, 84,
        414,486,394, 96, 99,154,511,148,413,361,409,255,162,215,302,201,
        266,351,343,144,441,365,108,298,251, 34,182,509,138,210,335,133,
        311,352,328,141,396,346,123,319,450,281,429,228,443,481, 92,404,
        485,422,248,297, 23,213,130,466, 22,217,283, 70,294,360,419,127,
        312,377,  7,468,194,  2,117,295,463,258,224,447,247,187, 80,398,
        284,353,105,390,299,471,470,184, 57,200,348, 63,204,188, 33,451,
        97, 30,310,219, 94,160,129,493, 64,179,263,102,189,207,114,402,
        438,477,387,122,192, 42,381,  5,145,118,180,449,293,323,136,380,
        43, 66, 60,455,341,445,202,432,  8,237, 15,376,436,464, 59,461 ]

s9 :: Word -> Word
s9 i = s9_data !! fromIntegral i

alt_key_data = [ 0x0123, 0x4567, 0x89AB, 0xCDEF, 0xFEDC, 0xBA98, 0x7654, 0x3210 ]
alt_key_for :: [Word] -> [Word]
alt_key_for key = map (\[x,y] -> x `xor` y) $ transpose [key, alt_key_data]

-- Round Keys

k_L k k' i j = case j of
                    0 -> rol16 (k i) 1
                    1 -> k' (i + 2)

k_O k k' i j = case j of
                    0 -> rol16 (k (i + 1)) 5
                    1 -> rol16 (k (i + 5)) 8
                    2 -> rol16 (k (i + 6)) 13

k_I k k' i j = case j of
                    0 -> k' (i + 4)
                    1 -> k' (i + 3)
                    2 -> k' (i + 7)

-- F/FL/FO

f_F f_FL f_FO i x = if (odd i)
                       then f_FL i $ f_FO i x
                       else f_FO i $ f_FL i x

f_FL k_L i x = let l  = upper_bits x 32 16
                   r  = lower_bits x 32 16
                   r' = (rol16 (l .&. (k_L i 0)) 1) `xor` r
                   l' = (rol16 (r' .|. (k_L i 1)) 1) `xor` l in
                   combine_bits l' r' 32 16

f_FO k_O f_FI i x = let l0 = upper_bits x 32 16
                        r0 = lower_bits x 32 16
                        (l3, r3) = foldl (\(l,r) j -> let f_FI_result = f_FI i j (l `xor` (k_O i j)) in
                                                          (r, f_FI_result `xor` r)) (l0,r0) [0..2] in
                        combine_bits l3 r3 32 16

-- FI
f_FI k_I i j x0 = let l0 = upper_bits x0 16 9
                      r0 = lower_bits x0 16 7
                      r1 = (s9 l0) `xor` r0
                      l1 = (s7 r0) `xor` (lower_bits r1 16 7)
                      x1 = combine_bits l1 r1 16 9
                      x2 = x1 `xor` (k_I i j)
                      l2 = upper_bits x2 16 7
                      r2 = lower_bits x2 16 9
                      r3 = (s9 r2) `xor` l2
                      l3 = (s7 l2) `xor` (lower_bits r3 16 7) in
                      combine_bits l3 r3 16 9

-- Encode/Decode
kasumi_encode f_F (l0,r0) = foldl (\(l,r) i -> let f_F_result = f_F i l in
                                                   (f_F_result `xor` r, l)) (l0,r0) [0..7]

kasumi_decode f_F (l0,r0) = foldl (\(l,r) i -> let f_F_result = f_F i r in
                                                   (r, f_F_result `xor` l)) (l0,r0) $ reverse [0..7]

-- Now, put everything together
wrap_key k = \index -> k !! (index `mod` 8)

kasumi_wrap f key = let k     = wrap_key key
                        k'    = wrap_key $ alt_key_for key
                        k_I'  = k_I k k'
                        k_O'  = k_O k k'
                        k_L'  = k_L k k'
                        f_FI' = f_FI k_I'
                        f_FO' = f_FO k_O' f_FI'
                        f_FL' = f_FL k_L'
                        f_F'  = f_F f_FL' f_FO' in
                        f f_F'

kasumi_encoder = kasumi_wrap kasumi_encode
kasumi_decoder = kasumi_wrap kasumi_decode

-- Testing 
--   call with tupel t=(x,y) where t represents 64 bit of data
--   x is upper 32 bit, y is lower 32 bit
test_key = [0..7]
encode = kasumi_encoder test_key
decode = kasumi_decoder test_key

-- encode (30,32)
-- = (3255589620,1286811847)
-- 
-- decode (3255589620,1286811847)
-- = (30,32)
