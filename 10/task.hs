module Caesar where

import Data.Char

-- Kodieren/Dekodieren
base = ord ' '

chiffre = undefined -- das ist der Schlüssel

range = ord 'z' - base + 1

enc x = chr $ ((ord x - base + chiffre) `mod` range) + base
dec x = chr $ ((ord x - base - chiffre) `mod` range) + base

encode = map enc
decode = map dec

-- Kodierte Nachricht
crypt = "Sxt/Z p)')&/sp)t&(/.+tx/b()\"st\"=/Sxt/w#trw'(t/!#tv xrwt/_)\"z(.pw /x'(/prw(.xv=/Sxt/Qt'(twt\"'v&t\".t/x'(/*xt&.xv=/T'/vxq(/prw(/P)uvpqt\"=/Sxt/Z p)')&/ux\"st(/'(p((/p!/u)t\"u(t\"/Utq&)p&/sxt't'/Ypw&t'="