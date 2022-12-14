#Worksheet 6
#del Carmen

library(ggplot2)
library(dplyr)
##
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
##
## filter, lag
## The following objects are masked from 'package:base':
##
## intersect, setdiff, setequal, union

data(mpg)
dataf <- as.data.frame(mpg)
dataf

## manufacturer model displ year cyl trans drv cty hwy
## 1 audi a4 1.8 1999 4 auto(l5) f 18 29
## 2 audi a4 1.8 1999 4 manual(m5) f 21 29
## 3 audi a4 2.0 2008 4 manual(m6) f 20 31
## 4 audi a4 2.0 2008 4 auto(av) f 21 30
## 5 audi a4 2.8 1999 6 auto(l5) f 16 26
## 6 audi a4 2.8 1999 6 manual(m5) f 18 26
## 7 audi a4 3.1 2008 6 auto(av) f 18 27
## 8 audi a4 quattro 1.8 1999 4 manual(m5) 4 18 26
## 9 audi a4 quattro 1.8 1999 4 auto(l5) 4 16 25
## 10 audi a4 quattro 2.0 2008 4 manual(m6) 4 20 28
## 11 audi a4 quattro 2.0 2008 4 auto(s6) 4 19 27
## 12 audi a4 quattro 2.8 1999 6 auto(l5) 4 15 25
## 13 audi a4 quattro 2.8 1999 6 manual(m5) 4 17 25
## 14 audi a4 quattro 3.1 2008 6 auto(s6) 4 17 25
## 15 audi a4 quattro 3.1 2008 6 manual(m6) 4 15 25
## 16 audi a6 quattro 2.8 1999 6 auto(l5) 4 15 24
## 17 audi a6 quattro 3.1 2008 6 auto(s6) 4 17 25
## 18 audi a6 quattro 4.2 2008 8 auto(s6) 4 16 23
## 19 chevrolet c1500 suburban 2wd 5.3 2008 8 auto(l4) r 14 20
## 20 chevrolet c1500 suburban 2wd 5.3 2008 8 auto(l4) r 11 15
## 21 chevrolet c1500 suburban 2wd 5.3 2008 8 auto(l4) r 14 20
## 22 chevrolet c1500 suburban 2wd 5.7 1999 8 auto(l4) r 13 17
## 23 chevrolet c1500 suburban 2wd 6.0 2008 8 auto(l4) r 12 17
## 24 chevrolet corvette 5.7 1999 8 manual(m6) r 16 26
## 25 chevrolet corvette 5.7 1999 8 auto(l4) r 15 23
## 26 chevrolet corvette 6.2 2008 8 manual(m6) r 16 26
## 27 chevrolet corvette 6.2 2008 8 auto(s6) r 15 25
## 28 chevrolet corvette 7.0 2008 8 manual(m6) r 15 24
## 29 chevrolet k1500 tahoe 4wd 5.3 2008 8 auto(l4) 4 14 19
## 30 chevrolet k1500 tahoe 4wd 5.3 2008 8 auto(l4) 4 11 14
## 31 chevrolet k1500 tahoe 4wd 5.7 1999 8 auto(l4) 4 11 15
## 32 chevrolet k1500 tahoe 4wd 6.5 1999 8 auto(l4) 4 14 17
## 33 chevrolet malibu 2.4 1999 4 auto(l4) f 19 27
## 34 chevrolet malibu 2.4 2008 4 auto(l4) f 22 30
## 35 chevrolet malibu 3.1 1999 6 auto(l4) f 18 26
## 36 chevrolet malibu 3.5 2008 6 auto(l4) f 18 29
## 37 chevrolet malibu 3.6 2008 6 auto(s6) f 17 26
## 38 dodge caravan 2wd 2.4 1999 4 auto(l3) f 18 24
## 39 dodge caravan 2wd 3.0 1999 6 auto(l4) f 17 24
## 40 dodge caravan 2wd 3.3 1999 6 auto(l4) f 16 22
## 41 dodge caravan 2wd 3.3 1999 6 auto(l4) f 16 22
## 42 dodge caravan 2wd 3.3 2008 6 auto(l4) f 17 24
## 43 dodge caravan 2wd 3.3 2008 6 auto(l4) f 17 24
## 44 dodge caravan 2wd 3.3 2008 6 auto(l4) f 11 17
## 45 dodge caravan 2wd 3.8 1999 6 auto(l4) f 15 22
## 46 dodge caravan 2wd 3.8 1999 6 auto(l4) f 15 21
## 47 dodge caravan 2wd 3.8 2008 6 auto(l6) f 16 23
## 48 dodge caravan 2wd 4.0 2008 6 auto(l6) f 16 23
## 49 dodge dakota pickup 4wd 3.7 2008 6 manual(m6) 4 15 19
## 50 dodge dakota pickup 4wd 3.7 2008 6 auto(l4) 4 14 18
## 51 dodge dakota pickup 4wd 3.9 1999 6 auto(l4) 4 13 17
## 52 dodge dakota pickup 4wd 3.9 1999 6 manual(m5) 4 14 17
## 53 dodge dakota pickup 4wd 4.7 2008 8 auto(l5) 4 14 19
## 54 dodge dakota pickup 4wd 4.7 2008 8 auto(l5) 4 14 19
## 55 dodge dakota pickup 4wd 4.7 2008 8 auto(l5) 4 9 12
## 56 dodge dakota pickup 4wd 5.2 1999 8 manual(m5) 4 11 17
## 57 dodge dakota pickup 4wd 5.2 1999 8 auto(l4) 4 11 15
## 58 dodge durango 4wd 3.9 1999 6 auto(l4) 4 13 17
## 59 dodge durango 4wd 4.7 2008 8 auto(l5) 4 13 17
## 60 dodge durango 4wd 4.7 2008 8 auto(l5) 4 9 12
## 61 dodge durango 4wd 4.7 2008 8 auto(l5) 4 13 17
## 62 dodge durango 4wd 5.2 1999 8 auto(l4) 4 11 16
## 63 dodge durango 4wd 5.7 2008 8 auto(l5) 4 13 18
## 64 dodge durango 4wd 5.9 1999 8 auto(l4) 4 11 15
## 65 dodge ram 1500 pickup 4wd 4.7 2008 8 manual(m6) 4 12 16
## 66 dodge ram 1500 pickup 4wd 4.7 2008 8 auto(l5) 4 9 12
## 67 dodge ram 1500 pickup 4wd 4.7 2008 8 auto(l5) 4 13 17
## 68 dodge ram 1500 pickup 4wd 4.7 2008 8 auto(l5) 4 13 17
## 69 dodge ram 1500 pickup 4wd 4.7 2008 8 manual(m6) 4 12 16
## 70 dodge ram 1500 pickup 4wd 4.7 2008 8 manual(m6) 4 9 12
## 71 dodge ram 1500 pickup 4wd 5.2 1999 8 auto(l4) 4 11 15
## 72 dodge ram 1500 pickup 4wd 5.2 1999 8 manual(m5) 4 11 16
## 73 dodge ram 1500 pickup 4wd 5.7 2008 8 auto(l5) 4 13 17
## 74 dodge ram 1500 pickup 4wd 5.9 1999 8 auto(l4) 4 11 15
## 75 ford expedition 2wd 4.6 1999 8 auto(l4) r 11 17
## 76 ford expedition 2wd 5.4 1999 8 auto(l4) r 11 17
## 77 ford expedition 2wd 5.4 2008 8 auto(l6) r 12 18
## 78 ford explorer 4wd 4.0 1999 6 auto(l5) 4 14 17
## 79 ford explorer 4wd 4.0 1999 6 manual(m5) 4 15 19
## 80 ford explorer 4wd 4.0 1999 6 auto(l5) 4 14 17
## 81 ford explorer 4wd 4.0 2008 6 auto(l5) 4 13 19
## 82 ford explorer 4wd 4.6 2008 8 auto(l6) 4 13 19
## 83 ford explorer 4wd 5.0 1999 8 auto(l4) 4 13 17
## 84 ford f150 pickup 4wd 4.2 1999 6 auto(l4) 4 14 17
## 85 ford f150 pickup 4wd 4.2 1999 6 manual(m5) 4 14 17
## 86 ford f150 pickup 4wd 4.6 1999 8 manual(m5) 4 13 16
## 87 ford f150 pickup 4wd 4.6 1999 8 auto(l4) 4 13 16
## 88 ford f150 pickup 4wd 4.6 2008 8 auto(l4) 4 13 17
## 89 ford f150 pickup 4wd 5.4 1999 8 auto(l4) 4 11 15
## 90 ford f150 pickup 4wd 5.4 2008 8 auto(l4) 4 13 17
## 91 ford mustang 3.8 1999 6 manual(m5) r 18 26
## 92 ford mustang 3.8 1999 6 auto(l4) r 18 25
## 93 ford mustang 4.0 2008 6 manual(m5) r 17 26
## 94 ford mustang 4.0 2008 6 auto(l5) r 16 24
## 95 ford mustang 4.6 1999 8 auto(l4) r 15 21
## 96 ford mustang 4.6 1999 8 manual(m5) r 15 22
## 97 ford mustang 4.6 2008 8 manual(m5) r 15 23
## 98 ford mustang 4.6 2008 8 auto(l5) r 15 22
## 99 ford mustang 5.4 2008 8 manual(m6) r 14 20
## 100 honda civic 1.6 1999 4 manual(m5) f 28 33
## 101 honda civic 1.6 1999 4 auto(l4) f 24 32
## 102 honda civic 1.6 1999 4 manual(m5) f 25 32
## 103 honda civic 1.6 1999 4 manual(m5) f 23 29
## 104 honda civic 1.6 1999 4 auto(l4) f 24 32
## 105 honda civic 1.8 2008 4 manual(m5) f 26 34
## 106 honda civic 1.8 2008 4 auto(l5) f 25 36
## 107 honda civic 1.8 2008 4 auto(l5) f 24 36
## 108 honda civic 2.0 2008 4 manual(m6) f 21 29
## 109 hyundai sonata 2.4 1999 4 auto(l4) f 18 26
## 110 hyundai sonata 2.4 1999 4 manual(m5) f 18 27
## 111 hyundai sonata 2.4 2008 4 auto(l4) f 21 30
## 112 hyundai sonata 2.4 2008 4 manual(m5) f 21 31
## 113 hyundai sonata 2.5 1999 6 auto(l4) f 18 26
## 114 hyundai sonata 2.5 1999 6 manual(m5) f 18 26
## 115 hyundai sonata 3.3 2008 6 auto(l5) f 19 28
## 116 hyundai tiburon 2.0 1999 4 auto(l4) f 19 26
## 117 hyundai tiburon 2.0 1999 4 manual(m5) f 19 29
## 118 hyundai tiburon 2.0 2008 4 manual(m5) f 20 28
## 119 hyundai tiburon 2.0 2008 4 auto(l4) f 20 27
## 120 hyundai tiburon 2.7 2008 6 auto(l4) f 17 24
## 121 hyundai tiburon 2.7 2008 6 manual(m6) f 16 24
## 122 hyundai tiburon 2.7 2008 6 manual(m5) f 17 24
## 123 jeep grand cherokee 4wd 3.0 2008 6 auto(l5) 4 17 22
## 124 jeep grand cherokee 4wd 3.7 2008 6 auto(l5) 4 15 19
## 125 jeep grand cherokee 4wd 4.0 1999 6 auto(l4) 4 15 20
## 126 jeep grand cherokee 4wd 4.7 1999 8 auto(l4) 4 14 17
## 127 jeep grand cherokee 4wd 4.7 2008 8 auto(l5) 4 9 12
## 128 jeep grand cherokee 4wd 4.7 2008 8 auto(l5) 4 14 19
## 129 jeep grand cherokee 4wd 5.7 2008 8 auto(l5) 4 13 18
## 130 jeep grand cherokee 4wd 6.1 2008 8 auto(l5) 4 11 14
## 131 land rover range rover 4.0 1999 8 auto(l4) 4 11 15
## 132 land rover range rover 4.2 2008 8 auto(s6) 4 12 18
## 133 land rover range rover 4.4 2008 8 auto(s6) 4 12 18
## 134 land rover range rover 4.6 1999 8 auto(l4) 4 11 15
## 135 lincoln navigator 2wd 5.4 1999 8 auto(l4) r 11 17
## 136 lincoln navigator 2wd 5.4 1999 8 auto(l4) r 11 16
## 137 lincoln navigator 2wd 5.4 2008 8 auto(l6) r 12 18
## 138 mercury mountaineer 4wd 4.0 1999 6 auto(l5) 4 14 17
## 139 mercury mountaineer 4wd 4.0 2008 6 auto(l5) 4 13 19
## 140 mercury mountaineer 4wd 4.6 2008 8 auto(l6) 4 13 19
## 141 mercury mountaineer 4wd 5.0 1999 8 auto(l4) 4 13 17
## 142 nissan altima 2.4 1999 4 manual(m5) f 21 29
## 143 nissan altima 2.4 1999 4 auto(l4) f 19 27
## 144 nissan altima 2.5 2008 4 auto(av) f 23 31
## 145 nissan altima 2.5 2008 4 manual(m6) f 23 32
## 146 nissan altima 3.5 2008 6 manual(m6) f 19 27
## 147 nissan altima 3.5 2008 6 auto(av) f 19 26
## 148 nissan maxima 3.0 1999 6 auto(l4) f 18 26
## 149 nissan maxima 3.0 1999 6 manual(m5) f 19 25
## 150 nissan maxima 3.5 2008 6 auto(av) f 19 25
## 151 nissan pathfinder 4wd 3.3 1999 6 auto(l4) 4 14 17
## 152 nissan pathfinder 4wd 3.3 1999 6 manual(m5) 4 15 17
## 153 nissan pathfinder 4wd 4.0 2008 6 auto(l5) 4 14 20
## 154 nissan pathfinder 4wd 5.6 2008 8 auto(s5) 4 12 18
## 155 pontiac grand prix 3.1 1999 6 auto(l4) f 18 26
## 156 pontiac grand prix 3.8 1999 6 auto(l4) f 16 26
## 157 pontiac grand prix 3.8 1999 6 auto(l4) f 17 27
## 158 pontiac grand prix 3.8 2008 6 auto(l4) f 18 28
## 159 pontiac grand prix 5.3 2008 8 auto(s4) f 16 25
## 160 subaru forester awd 2.5 1999 4 manual(m5) 4 18 25
## 161 subaru forester awd 2.5 1999 4 auto(l4) 4 18 24
## 162 subaru forester awd 2.5 2008 4 manual(m5) 4 20 27
## 163 subaru forester awd 2.5 2008 4 manual(m5) 4 19 25
## 164 subaru forester awd 2.5 2008 4 auto(l4) 4 20 26
## 165 subaru forester awd 2.5 2008 4 auto(l4) 4 18 23
## 166 subaru impreza awd 2.2 1999 4 auto(l4) 4 21 26
## 167 subaru impreza awd 2.2 1999 4 manual(m5) 4 19 26
## 168 subaru impreza awd 2.5 1999 4 manual(m5) 4 19 26
## 169 subaru impreza awd 2.5 1999 4 auto(l4) 4 19 26
## 170 subaru impreza awd 2.5 2008 4 auto(s4) 4 20 25
## 171 subaru impreza awd 2.5 2008 4 auto(s4) 4 20 27
## 172 subaru impreza awd 2.5 2008 4 manual(m5) 4 19 25
## 173 subaru impreza awd 2.5 2008 4 manual(m5) 4 20 27
## 174 toyota 4runner 4wd 2.7 1999 4 manual(m5) 4 15 20
## 175 toyota 4runner 4wd 2.7 1999 4 auto(l4) 4 16 20
## 176 toyota 4runner 4wd 3.4 1999 6 auto(l4) 4 15 19
## 177 toyota 4runner 4wd 3.4 1999 6 manual(m5) 4 15 17
## 178 toyota 4runner 4wd 4.0 2008 6 auto(l5) 4 16 20
## 179 toyota 4runner 4wd 4.7 2008 8 auto(l5) 4 14 17
## 180 toyota camry 2.2 1999 4 manual(m5) f 21 29
## 181 toyota camry 2.2 1999 4 auto(l4) f 21 27
## 182 toyota camry 2.4 2008 4 manual(m5) f 21 31
## 183 toyota camry 2.4 2008 4 auto(l5) f 21 31
## 184 toyota camry 3.0 1999 6 auto(l4) f 18 26
## 185 toyota camry 3.0 1999 6 manual(m5) f 18 26
## 186 toyota camry 3.5 2008 6 auto(s6) f 19 28
## 187 toyota camry solara 2.2 1999 4 auto(l4) f 21 27
## 188 toyota camry solara 2.2 1999 4 manual(m5) f 21 29
## 189 toyota camry solara 2.4 2008 4 manual(m5) f 21 31
## 190 toyota camry solara 2.4 2008 4 auto(s5) f 22 31
## 191 toyota camry solara 3.0 1999 6 auto(l4) f 18 26
## 192 toyota camry solara 3.0 1999 6 manual(m5) f 18 26
## 193 toyota camry solara 3.3 2008 6 auto(s5) f 18 27
## 194 toyota corolla 1.8 1999 4 auto(l3) f 24 30
## 195 toyota corolla 1.8 1999 4 auto(l4) f 24 33
## 196 toyota corolla 1.8 1999 4 manual(m5) f 26 35
## 197 toyota corolla 1.8 2008 4 manual(m5) f 28 37
## 198 toyota corolla 1.8 2008 4 auto(l4) f 26 35
## 199 toyota land cruiser wagon 4wd 4.7 1999 8 auto(l4) 4 11 15
## 200 toyota land cruiser wagon 4wd 5.7 2008 8 auto(s6) 4 13 18
## 201 toyota toyota tacoma 4wd 2.7 1999 4 manual(m5) 4 15 20
## 202 toyota toyota tacoma 4wd 2.7 1999 4 auto(l4) 4 16 20
## 203 toyota toyota tacoma 4wd 2.7 2008 4 manual(m5) 4 17 22
## 204 toyota toyota tacoma 4wd 3.4 1999 6 manual(m5) 4 15 17
## 205 toyota toyota tacoma 4wd 3.4 1999 6 auto(l4) 4 15 19
## 206 toyota toyota tacoma 4wd 4.0 2008 6 manual(m6) 4 15 18
## 207 toyota toyota tacoma 4wd 4.0 2008 6 auto(l5) 4 16 20
## 208 volkswagen gti 2.0 1999 4 manual(m5) f 21 29
## 209 volkswagen gti 2.0 1999 4 auto(l4) f 19 26
## 210 volkswagen gti 2.0 2008 4 manual(m6) f 21 29
## 211 volkswagen gti 2.0 2008 4 auto(s6) f 22 29
## 212 volkswagen gti 2.8 1999 6 manual(m5) f 17 24
## 213 volkswagen jetta 1.9 1999 4 manual(m5) f 33 44
## 214 volkswagen jetta 2.0 1999 4 manual(m5) f 21 29
## 215 volkswagen jetta 2.0 1999 4 auto(l4) f 19 26
## 216 volkswagen jetta 2.0 2008 4 auto(s6) f 22 29
## 217 volkswagen jetta 2.0 2008 4 manual(m6) f 21 29
## 218 volkswagen jetta 2.5 2008 5 auto(s6) f 21 29
## 219 volkswagen jetta 2.5 2008 5 manual(m5) f 21 29
## 220 volkswagen jetta 2.8 1999 6 auto(l4) f 16 23
## 221 volkswagen jetta 2.8 1999 6 manual(m5) f 17 24
## 222 volkswagen new beetle 1.9 1999 4 manual(m5) f 35 44
## 223 volkswagen new beetle 1.9 1999 4 auto(l4) f 29 41
## 224 volkswagen new beetle 2.0 1999 4 manual(m5) f 21 29
## 225 volkswagen new beetle 2.0 1999 4 auto(l4) f 19 26
## 226 volkswagen new beetle 2.5 2008 5 manual(m5) f 20 28
## 227 volkswagen new beetle 2.5 2008 5 auto(s6) f 20 29
## 228 volkswagen passat 1.8 1999 4 manual(m5) f 21 29
## 229 volkswagen passat 1.8 1999 4 auto(l5) f 18 29
## 230 volkswagen passat 2.0 2008 4 auto(s6) f 19 28
## 231 volkswagen passat 2.0 2008 4 manual(m6) f 21 29
## 232 volkswagen passat 2.8 1999 6 auto(l5) f 16 26
## 233 volkswagen passat 2.8 1999 6 manual(m5) f 18 26
## 234 volkswagen passat 3.6 2008 6 auto(s6) f 17 26
## fl class
## 1 p compact
## 2 p compact
## 3 p compact
## 4 p compact
## 5 p compact
## 6 p compact
## 7 p compact
## 8 p compact
## 9 p compact
## 10 p compact
## 11 p compact
## 12 p compact
## 13 p compact
## 14 p compact
## 15 p compact
## 16 p midsize
## 17 p midsize
## 18 p midsize
## 19 r suv
## 20 e suv
## 21 r suv
## 22 r suv
## 23 r suv
## 24 p 2seater
## 25 p 2seater
## 26 p 2seater
## 27 p 2seater
## 28 p 2seater
## 29 r suv
## 30 e suv
## 31 r suv
## 32 d suv
## 33 r midsize
## 34 r midsize
## 35 r midsize
## 36 r midsize
## 37 r midsize
## 38 r minivan
## 39 r minivan
## 40 r minivan
## 41 r minivan
## 42 r minivan
## 43 r minivan
## 44 e minivan
## 45 r minivan
## 46 r minivan
## 47 r minivan
## 48 r minivan
## 49 r pickup
## 50 r pickup
## 51 r pickup
## 52 r pickup
## 53 r pickup
## 54 r pickup
## 55 e pickup
## 56 r pickup
## 57 r pickup
## 58 r suv
## 59 r suv
## 60 e suv
## 61 r suv
## 62 r suv
## 63 r suv
## 64 r suv
## 65 r pickup
## 66 e pickup
## 67 r pickup
## 68 r pickup
## 69 r pickup
## 70 e pickup
## 71 r pickup
## 72 r pickup
## 73 r pickup
## 74 r pickup
## 75 r suv
## 76 r suv
## 77 r suv
## 78 r suv
## 79 r suv
## 80 r suv
## 81 r suv
## 82 r suv
## 83 r suv
## 84 r pickup
## 85 r pickup
## 86 r pickup
## 87 r pickup
## 88 r pickup
## 89 r pickup
## 90 r pickup
## 91 r subcompact
## 92 r subcompact
## 93 r subcompact
## 94 r subcompact
## 95 r subcompact
## 96 r subcompact
## 97 r subcompact
## 98 r subcompact
## 99 p subcompact
## 100 r subcompact
## 101 r subcompact
## 102 r subcompact
## 103 p subcompact
## 104 r subcompact
## 105 r subcompact
## 106 r subcompact
## 107 c subcompact
## 108 p subcompact
## 109 r midsize
## 110 r midsize
## 111 r midsize
## 112 r midsize
## 113 r midsize
## 114 r midsize
## 115 r midsize
## 116 r subcompact
## 117 r subcompact
## 118 r subcompact
## 119 r subcompact
## 120 r subcompact
## 121 r subcompact
## 122 r subcompact
## 123 d suv
## 124 r suv
## 125 r suv
## 126 r suv
## 127 e suv
## 128 r suv
## 129 r suv
## 130 p suv
## 131 p suv
## 132 r suv
## 133 r suv
## 134 p suv
## 135 r suv
## 136 p suv
## 137 r suv
## 138 r suv
## 139 r suv
## 140 r suv
## 141 r suv
## 142 r compact
## 143 r compact
## 144 r midsize
## 145 r midsize
## 146 p midsize
## 147 p midsize
## 148 r midsize
## 149 r midsize
## 150 p midsize
## 151 r suv
## 152 r suv
## 153 p suv
## 154 p suv
## 155 r midsize
## 156 p midsize
## 157 r midsize
## 158 r midsize
## 159 p midsize
## 160 r suv
## 161 r suv
## 162 r suv
## 163 p suv
## 164 r suv
## 165 p suv
## 166 r subcompact
## 167 r subcompact
## 168 r subcompact
## 169 r subcompact
## 170 p compact
## 171 r compact
## 172 p compact
## 173 r compact
## 174 r suv
## 175 r suv
## 176 r suv
## 177 r suv
## 178 r suv
## 179 r suv
## 180 r midsize
## 181 r midsize
## 182 r midsize
## 183 r midsize
## 184 r midsize
## 185 r midsize
## 186 r midsize
## 187 r compact
## 188 r compact
## 189 r compact
## 190 r compact
## 191 r compact
## 192 r compact
## 193 r compact
## 194 r compact
## 195 r compact
## 196 r compact
## 197 r compact
## 198 r compact
## 199 r suv
## 200 r suv
## 201 r pickup
## 202 r pickup
## 203 r pickup
## 204 r pickup
## 205 r pickup
## 206 r pickup
## 207 r pickup
## 208 r compact
## 209 r compact
## 210 p compact
## 211 p compact
## 212 r compact
## 213 d compact
## 214 r compact
## 215 r compact
## 216 p compact
## 217 p compact
## 218 r compact
## 219 r compact
## 220 r compact
## 221 r compact
## 222 d subcompact
## 223 d subcompact
## 224 r subcompact
## 225 r subcompact
## 226 r subcompact
## 227 r subcompact
## 228 p midsize
## 229 p midsize
## 230 p midsize
## 231 p midsize
## 232 p midsize
## 233 p midsize
## 234 p midsize

#1. How many columns are in mpg dataset? How about the number of rows? Show thecodes and its result.

#ans - it has 11 columns and a rows of 234

mpg_data <- glimpse(dataf)

## Rows: 234
## Columns: 11
## $ manufacturer <chr> "audi", "audi", "audi", "audi", "audi", "audi", "audi", "~
## $ model <chr> "a4", "a4", "a4", "a4", "a4", "a4", "a4", "a4 quattro", "~
## $ displ <dbl> 1.8, 1.8, 2.0, 2.0, 2.8, 2.8, 3.1, 1.8, 1.8, 2.0, 2.0, 2.~
## $ year <int> 1999, 1999, 2008, 2008, 1999, 1999, 2008, 1999, 1999, 200~
## $ cyl <int> 4, 4, 4, 4, 6, 6, 6, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 8, 8, ~
## $ trans <chr> "auto(l5)", "manual(m5)", "manual(m6)", "auto(av)", "auto~
## $ drv <chr> "f", "f", "f", "f", "f", "f", "f", "4", "4", "4", "4", "4~
## $ cty <int> 18, 21, 20, 21, 16, 18, 18, 18, 16, 20, 19, 15, 17, 17, 1~
## $ hwy <int> 29, 29, 31, 30, 26, 26, 27, 26, 25, 28, 27, 25, 25, 25, 2~
## $ fl <chr> "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p~
## $ class <chr> "compact", "compact", "compact", "compact", "compact", "c~

#2. Which manufacturer has the most models in this data set? Which model has the most variations? ans - the
#manufacturer that has the most model are the Dodge while the most variations for model are the caravan2wd

mnftr <- mpg_data %>% group_by(manufacturer,model) %>% count()
mnftr

## # A tibble: 38 x 3
## # Groups: manufacturer, model [38]
## manufacturer model n
## <chr> <chr> <int>
## 1 audi a4 7
## 2 audi a4 quattro 8
## 3 audi a6 quattro 3
## 4 chevrolet c1500 suburban 2wd 5
## 5 chevrolet corvette 5
## 6 chevrolet k1500 tahoe 4wd 4
## 7 chevrolet malibu 5
## 8 dodge caravan 2wd 11
## 9 dodge dakota pickup 4wd 9
## 10 dodge durango 4wd 7
## # ... with 28 more rows

colnames(mnftr) <- c("Manufacturer", "Model", "Counts")

#a.Group the manufacturers and find the unique models. 
#Copy the codes and result.

data_unique <- mpg_data %>% group_by(manufacturer) %>%
               distinct(model) %>% count()

data_unique

## # A tibble: 15 x 2
## # Groups: manufacturer [15]
## manufacturer n
## <chr> <int>
## 1 audi 3
## 2 chevrolet 4
## 3 dodge 4
## 4 ford 4
## 5 honda 1
## 6 hyundai 2
## 7 jeep 1
## 8 land rover 1
## 9 lincoln 1
## 10 mercury 1
## 11 nissan 3
## 12 pontiac 1
## 13 subaru 2
## 14 toyota 6
## 15 volkswagen 4

colnames(data_unique) <- c("Manufacturer", "Unique Counts")

b. Graph the result by using plot() and ggplot(). Write the codes and its result.

plot(mnftr)

ggplot(mnftr, aes(Manufacturer, Model)) + geom_point()

#3. Same dataset will be used. You are going to show the relationship of the 
#model and the manufacturer. 
#a. What does ggplot(mpg, aes(model, manufacturer)) + geom_point() show? 
#ans - A point graph of model and manufacturer from the mpg data-set

ggplot(datab, aes(model, manufacturer)) + geom_point()

#b. For you, is it useful? If not, how could you modify the data to make it more
#informative? 
#ans - The data is already organised, but it can be improved to look more 
#information by using a legend to differentiate the data from scatter plot.

#4. Using the pipe (%>%), group the model and get the number of cars per model. 
#Show codes and its result.

car_mod <- mpg_data %>% group_by(model) %>% count()
car_mod

# A tibble: 38 ?? 2
# Groups:   model [38]
#model                  n
#<chr>              <int>
  #1 4runner 4wd            6
#2 a4                     7
#3 a4 quattro             8
#4 a6 quattro             3
#5 altima                 6
#6 c1500 suburban 2wd     5
#7 camry                  7
#8 camry solara           7
#9 caravan 2wd           11
#10 civic                  9
# ??? with 28 more rows

colnames(car_mod) <- c("Model", "Counts")

#a. Plot using the geom_bar() + coord_flip() just like what is shown below. Show codes and its result.

qplot(model,data = datab,
      main = "Number of Cars per Model",
      xlab = "Model",
      ylab = "Number of Cars", geom = "bar", fill = manufacturer) + coord_flip()

#b. Use only the top 20 observations. Show code and results.

top_data <- car_mod[1:20,] %>% top_n(2)

## Selecting by Counts

top_data

# A tibble: 20 ?? 2
# Groups:   Model [20]
#Model              Counts
#<chr>               <int>
#  1 4runner 4wd             6
#2 a4                      7
#3 a4 quattro              8
#4 a6 quattro              3
#5 altima                  6
#6 c1500 suburban 2wd      5
#7 camry                   7
#8 camry solara            7
#9 caravan 2wd            11
#10 civic                   9
#11 corolla                 5
#12 corvette                5
#13 dakota pickup 4wd       9
#14 durango 4wd             7
#15 expedition 2wd          3
#16 explorer 4wd            6
#17 f150 pickup 4wd         7
#18 forester awd            6
#19 grand cherokee 4wd      8
#20 grand prix              5

ggplot(top_data,aes(x = Model, y = Counts)) +
geom_bar(stat = "Identity") +coord_flip()

#5. Plot the relationship between cyl - number of cylinders and displ - engine 
#displacement using geom_point
#with aesthetic colour = engine displacement. Title should be ???Relationship 
#between No. of Cylinders and Engine Displacement???. 
#a. Show the codes and its result.

ggplot(data = mpg_data , mapping = aes(x = displ, y = cyl, main = "Relationship
between No of Cylinders and Engine Displacement")) + 
  geom_point(mapping=aes(colour = "engine displacement"))

#b. How would you describe its relationship? ans - the scatter plot shows the cyl 
#is y axis and displ in the x axis to easily distinguish the clustered data, that \
#indicate the engine displacement.

#6. Get the total number of observations for drv - type of drive train 
#(f = front-wheel drive, r = rear wheel drive, 4 = 4wd) and class - type of 
#class (Example: suv, 2seater, etc.). Plot using the geom_tile() where the 
#number of observations for class be used as a fill for aesthetics. 

#a. Show the codes and its result for the narrative in # 6

ggplot(data = mpg_data, mapping = aes(x = drv, y = class)) + 
geom_tile(aes(fill=class))

#b. Interpret the result. 

#using mapping on geometric tile will graph the data in the graph so that it will
#shown the different colors. also shown on x axis is the drv and the Y axis is 
#the class

#7. Discuss the difference between these codes. Its outputs for each are shown below. ans - the graph are
#almost the same but the first code are much easier to analyze since it has a legend that can distinguish the
#jittered data while the second code are only in a graph that has a color blue.

#first code
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = "blue"))

#secord code
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")
#8. Try to run the command ?mpg. What is the result of this command? 
#  ans - it shows the description for the data set of mpg.
ds1 <-?mpg
ds1
#a. Which variables from mpg dataset are categorical? 
#ans - the manufacturer, model, trans, drv, fl, class
#are the categorical variables from the data-set of mpg.
#b. Which are continuous variables? 
#ans - the consinuous variable of the mpg data-set are the dsipl, year,
#cyl, cty, and hwy.

#c. Plot the relationship between displ (engine displacement) and hwy(highway 
#miles per gallon). Mapped it with a continuous variable you have identified 
#in #5-b. What is its result? Why it produced such
#output? 
  
#the data that have been graph shows that they are in positive rate by using 
#displ for hyw and cty scattered plot.
  
ggplot(mpg, aes(x = cty, y = hwy, colour = displ)) + geom_point()

#9. Plot the relationship between displ (engine displacement) and 
#hwy(highway miles per gallon) using geom_point(). Add a trend line over the 
#existing plot using geom_smooth() with se = FALSE. Default method is ???loess???.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping=aes(color=class)) +
  geom_smooth(se = FALSE)

## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

#10. Using the relationship of displ and hwy, add a trend line over existing 
#plot. Set these = FALSE to remove the confidence interval and method = lm to 
#check for linear modeling.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm)

## `geom_smooth()` using formula = 'y ~ x'
