# scatter_plot works as expected

    Code
      print(tibble::as_tibble(fig[["data"]]), n = Inf)
    Output
      # A tibble: 254 x 7
          USUBJID     TRTVAR                 ALT   BILI  XVAR   YVAR .group
          <chr>       <ord>                <dbl>  <dbl> <dbl>  <dbl>  <int>
        1 01-701-1015 Placebo                 41  10.3     41  10.3       1
        2 01-701-1023 Placebo                 38  12.0     38  12.0       2
        3 01-701-1028 Xanomeline High Dose    33  18.8     33  18.8       3
        4 01-701-1033 Xanomeline Low Dose     35  15.4     35  15.4       4
        5 01-701-1034 Xanomeline High Dose    24  10.3     24  10.3       5
        6 01-701-1047 Placebo                 22   8.55    22   8.55      6
        7 01-701-1097 Xanomeline Low Dose     24  10.3     24  10.3       7
        8 01-701-1111 Xanomeline Low Dose     23   8.55    23   8.55      8
        9 01-701-1115 Xanomeline Low Dose     18   8.55    18   8.55      9
       10 01-701-1118 Placebo                 16  12.0     16  12.0      10
       11 01-701-1130 Placebo                 18  12.0     18  12.0      11
       12 01-701-1133 Xanomeline High Dose    23  15.4     23  15.4      12
       13 01-701-1146 Xanomeline High Dose    16  10.3     16  10.3      13
       14 01-701-1148 Xanomeline High Dose    41  12.0     41  12.0      14
       15 01-701-1153 Placebo                 17   8.55    17   8.55     15
       16 01-701-1180 Xanomeline High Dose    29  12.0     29  12.0      16
       17 01-701-1181 Xanomeline High Dose    18   8.55    18   8.55     17
       18 01-701-1188 Xanomeline Low Dose     29   8.55    29   8.55     18
       19 01-701-1192 Xanomeline Low Dose     19  12.0     19  12.0      19
       20 01-701-1203 Placebo                 16   8.55    16   8.55     20
       21 01-701-1211 Xanomeline Low Dose     21  25.6     21  25.6      21
       22 01-701-1234 Placebo                 40  20.5     40  20.5      22
       23 01-701-1239 Xanomeline High Dose    71  39.3     71  39.3      23
       24 01-701-1275 Xanomeline High Dose    22  10.3     22  10.3      24
       25 01-701-1287 Xanomeline High Dose    27  18.8     27  18.8      25
       26 01-701-1294 Xanomeline Low Dose     29  10.3     29  10.3      26
       27 01-701-1302 Xanomeline High Dose    62  10.3     62  10.3      27
       28 01-701-1317 Xanomeline Low Dose     14  32.5     14  32.5      28
       29 01-701-1324 Xanomeline Low Dose     15   8.55    15   8.55     29
       30 01-701-1341 Xanomeline Low Dose     58   8.55    58   8.55     30
       31 01-701-1345 Placebo                 25   8.55    25   8.55     31
       32 01-701-1360 Xanomeline High Dose    19  17.1     19  17.1      32
       33 01-701-1363 Placebo                 14  NA       14  NA        33
       34 01-701-1383 Xanomeline High Dose    19  10.3     19  10.3      34
       35 01-701-1387 Placebo                 12   6.84    12   6.84     35
       36 01-701-1392 Placebo                 23  10.3     23  10.3      36
       37 01-701-1415 Placebo                 16  13.7     16  13.7      37
       38 01-701-1429 Xanomeline Low Dose     28  13.7     28  13.7      38
       39 01-701-1440 Placebo                 26   8.55    26   8.55     39
       40 01-701-1442 Xanomeline Low Dose     33   8.55    33   8.55     40
       41 01-701-1444 Xanomeline High Dose    23  17.1     23  17.1      41
       42 01-702-1082 Xanomeline Low Dose     37  10.3     37  10.3      42
       43 01-703-1042 Placebo                 31  17.1     31  17.1      43
       44 01-703-1076 Xanomeline High Dose    27  10.3     27  10.3      44
       45 01-703-1086 Xanomeline Low Dose     29  15.4     29  15.4      45
       46 01-703-1096 Placebo                 12   8.55    12   8.55     46
       47 01-703-1100 Placebo                 18  13.7     18  13.7      47
       48 01-703-1119 Xanomeline Low Dose     44   8.55    44   8.55     48
       49 01-703-1175 Placebo                 19  13.7     19  13.7      49
       50 01-703-1182 Xanomeline Low Dose     19  17.1     19  17.1      50
       51 01-703-1197 Xanomeline Low Dose     18  10.3     18  10.3      51
       52 01-703-1210 Placebo                 18  12.0     18  12.0      52
       53 01-703-1258 Xanomeline High Dose    43  12.0     43  12.0      53
       54 01-703-1279 Xanomeline Low Dose     11   6.84    11   6.84     54
       55 01-703-1295 Xanomeline High Dose    21  10.3     21  10.3      55
       56 01-703-1299 Placebo                 13   8.55    13   8.55     56
       57 01-703-1335 Xanomeline High Dose    25   8.55    25   8.55     57
       58 01-703-1379 Xanomeline Low Dose     24   8.55    24   8.55     58
       59 01-703-1403 Xanomeline High Dose    40  12.0     40  12.0      59
       60 01-703-1439 Xanomeline High Dose    27  15.4     27  15.4      60
       61 01-704-1008 Xanomeline High Dose    25  22.2     25  22.2      61
       62 01-704-1009 Xanomeline Low Dose     39  12.0     39  12.0      62
       63 01-704-1010 Placebo                 14  22.2     14  22.2      63
       64 01-704-1017 Xanomeline High Dose    11  13.7     11  13.7      64
       65 01-704-1025 Xanomeline Low Dose     23  10.3     23  10.3      65
       66 01-704-1065 Xanomeline High Dose    20  10.3     20  10.3      66
       67 01-704-1074 Xanomeline High Dose    14  10.3     14  10.3      67
       68 01-704-1093 Xanomeline High Dose    26  17.1     26  17.1      68
       69 01-704-1114 Xanomeline Low Dose     21  13.7     21  13.7      69
       70 01-704-1120 Xanomeline Low Dose     33  18.8     33  18.8      70
       71 01-704-1127 Placebo                 26  12.0     26  12.0      71
       72 01-704-1135 Xanomeline Low Dose     19  17.1     19  17.1      72
       73 01-704-1164 Placebo                 13  13.7     13  13.7      73
       74 01-704-1218 Xanomeline Low Dose     23  20.5     23  20.5      74
       75 01-704-1233 Placebo                 16  10.3     16  10.3      75
       76 01-704-1241 Xanomeline High Dose    23  12.0     23  12.0      76
       77 01-704-1260 Placebo                 22  12.0     22  12.0      77
       78 01-704-1266 Xanomeline High Dose    21  13.7     21  13.7      78
       79 01-704-1323 Xanomeline Low Dose     16  NA       16  NA        79
       80 01-704-1325 Xanomeline Low Dose     17  20.5     17  20.5      80
       81 01-704-1332 Xanomeline High Dose    16  29.1     16  29.1      81
       82 01-704-1351 Placebo                 21  15.4     21  15.4      82
       83 01-704-1388 Placebo                 36  15.4     36  15.4      83
       84 01-704-1435 Placebo                 14  10.3     14  10.3      84
       85 01-704-1445 Placebo                 48  12.0     48  12.0      85
       86 01-705-1018 Placebo                 12   6.84    12   6.84     86
       87 01-705-1031 Xanomeline Low Dose     28  NA       28  NA        87
       88 01-705-1059 Placebo                 32  12.0     32  12.0      88
       89 01-705-1186 Placebo                107 125.     107 125.       89
       90 01-705-1199 Xanomeline Low Dose     15  10.3     15  10.3      90
       91 01-705-1280 Xanomeline High Dose    21  10.3     21  10.3      91
       92 01-705-1281 Xanomeline High Dose    18  12.0     18  12.0      92
       93 01-705-1282 Placebo                 32   6.84    32   6.84     93
       94 01-705-1292 Xanomeline Low Dose     88  12.0     88  12.0      94
       95 01-705-1303 Xanomeline High Dose    22  20.5     22  20.5      95
       96 01-705-1310 Xanomeline High Dose   129  17.1    129  17.1      96
       97 01-705-1349 Placebo                 39  27.4     39  27.4      97
       98 01-705-1377 Xanomeline High Dose    20   6.84    20   6.84     98
       99 01-705-1382 Xanomeline High Dose    13  12.0     13  12.0      99
      100 01-705-1393 Xanomeline Low Dose     12  NA       12  NA       100
      101 01-705-1431 Xanomeline Low Dose     21   8.55    21   8.55    101
      102 01-706-1041 Placebo                 24   8.55    24   8.55    102
      103 01-706-1049 Xanomeline High Dose    24   8.55    24   8.55    103
      104 01-706-1384 Xanomeline Low Dose     18   8.55    18   8.55    104
      105 01-707-1037 Xanomeline Low Dose      7   8.55     7   8.55    105
      106 01-707-1206 Placebo                 28  10.3     28  10.3     106
      107 01-708-1019 Xanomeline Low Dose     15   6.84    15   6.84    107
      108 01-708-1032 Xanomeline Low Dose     28  22.2     28  22.2     108
      109 01-708-1084 Xanomeline Low Dose     13   8.55    13   8.55    109
      110 01-708-1087 Placebo                 14  12.0     14  12.0     110
      111 01-708-1158 Placebo                 19   6.84    19   6.84    111
      112 01-708-1171 Placebo                 18   8.55    18   8.55    112
      113 01-708-1178 Xanomeline High Dose    17  10.3     17  10.3     113
      114 01-708-1213 Xanomeline High Dose    16  12.0     16  12.0     114
      115 01-708-1216 Xanomeline High Dose    28  20.5     28  20.5     115
      116 01-708-1236 Xanomeline High Dose    16  12.0     16  12.0     116
      117 01-708-1253 Placebo                 25  12.0     25  12.0     117
      118 01-708-1272 Xanomeline Low Dose     29  17.1     29  17.1     118
      119 01-708-1286 Placebo                124   8.55   124   8.55    119
      120 01-708-1296 Placebo                 22  10.3     22  10.3     120
      121 01-708-1297 Xanomeline Low Dose     24  15.4     24  15.4     121
      122 01-708-1316 Placebo                 18  12.0     18  12.0     122
      123 01-708-1336 Xanomeline High Dose    18  13.7     18  13.7     123
      124 01-708-1342 Placebo                 28  13.7     28  13.7     124
      125 01-708-1347 Xanomeline High Dose    23   8.55    23   8.55    125
      126 01-708-1348 Xanomeline Low Dose     15   8.55    15   8.55    126
      127 01-708-1353 Xanomeline Low Dose     15   8.55    15   8.55    127
      128 01-708-1372 Xanomeline High Dose    14   8.55    14   8.55    128
      129 01-708-1378 Placebo                 18  25.6     18  25.6     129
      130 01-708-1406 Xanomeline High Dose    20  27.4     20  27.4     130
      131 01-708-1428 Xanomeline Low Dose     14   8.55    14   8.55    131
      132 01-709-1001 Placebo                 32  10.3     32  10.3     132
      133 01-709-1007 Xanomeline Low Dose     21   8.55    21   8.55    133
      134 01-709-1020 Xanomeline Low Dose     13  18.8     13  18.8     134
      135 01-709-1029 Xanomeline High Dose    18  53.0     18  53.0     135
      136 01-709-1081 Xanomeline Low Dose     19   8.55    19   8.55    136
      137 01-709-1088 Placebo                 20  13.7     20  13.7     137
      138 01-709-1099 Xanomeline High Dose    29  12.0     29  12.0     138
      139 01-709-1102 Xanomeline Low Dose     88  12.0     88  12.0     139
      140 01-709-1168 Xanomeline High Dose    18  10.3     18  10.3     140
      141 01-709-1217 Xanomeline Low Dose     23   8.55    23   8.55    141
      142 01-709-1238 Xanomeline High Dose    32  12.0     32  12.0     142
      143 01-709-1259 Placebo                 21  15.4     21  15.4     143
      144 01-709-1285 Xanomeline Low Dose     17  15.4     17  15.4     144
      145 01-709-1301 Placebo                 69  10.3     69  10.3     145
      146 01-709-1306 Placebo                 13  10.3     13  10.3     146
      147 01-709-1309 Xanomeline High Dose    42  35.9     42  35.9     147
      148 01-709-1312 Placebo                 20  27.4     20  27.4     148
      149 01-709-1326 Xanomeline Low Dose     26   6.84    26   6.84    149
      150 01-709-1329 Xanomeline High Dose    41   6.84    41   6.84    150
      151 01-709-1339 Placebo                 31  12.0     31  12.0     151
      152 01-709-1424 Xanomeline High Dose    18   6.84    18   6.84    152
      153 01-710-1002 Xanomeline Low Dose     17  12.0     17  12.0     153
      154 01-710-1006 Xanomeline High Dose    18  12.0     18  12.0     154
      155 01-710-1021 Xanomeline High Dose    28  10.3     28  10.3     155
      156 01-710-1027 Placebo                 16  17.1     16  17.1     156
      157 01-710-1045 Xanomeline Low Dose     15  10.3     15  10.3     157
      158 01-710-1053 Xanomeline Low Dose     22  12.0     22  12.0     158
      159 01-710-1060 Placebo                 32  12.0     32  12.0     159
      160 01-710-1070 Xanomeline High Dose    14  13.7     14  13.7     160
      161 01-710-1077 Placebo                 19  12.0     19  12.0     161
      162 01-710-1078 Placebo                 22  10.3     22  10.3     162
      163 01-710-1083 Placebo                 12   8.55    12   8.55    163
      164 01-710-1137 Xanomeline High Dose    17  10.3     17  10.3     164
      165 01-710-1142 Xanomeline High Dose    26   6.84    26   6.84    165
      166 01-710-1154 Xanomeline Low Dose     22  27.4     22  27.4     166
      167 01-710-1166 Xanomeline Low Dose     23  12.0     23  12.0     167
      168 01-710-1183 Placebo                  8  15.4      8  15.4     168
      169 01-710-1187 Xanomeline High Dose    16  13.7     16  13.7     169
      170 01-710-1235 Xanomeline Low Dose     27  10.3     27  10.3     170
      171 01-710-1249 Xanomeline High Dose    14   8.55    14   8.55    171
      172 01-710-1264 Placebo                 16  13.7     16  13.7     172
      173 01-710-1270 Xanomeline Low Dose     24  12.0     24  12.0     173
      174 01-710-1271 Placebo                 13  10.3     13  10.3     174
      175 01-710-1278 Xanomeline High Dose    20  15.4     20  15.4     175
      176 01-710-1300 Xanomeline Low Dose     21  12.0     21  12.0     176
      177 01-710-1314 Placebo                 14  10.3     14  10.3     177
      178 01-710-1315 Placebo                 19  10.3     19  10.3     178
      179 01-710-1354 Xanomeline High Dose    38  12.0     38  12.0     179
      180 01-710-1358 Xanomeline Low Dose     20   8.55    20   8.55    180
      181 01-710-1368 Placebo                 23  15.4     23  15.4     181
      182 01-710-1385 Xanomeline Low Dose     43   8.55    43   8.55    182
      183 01-710-1408 Xanomeline High Dose    44  10.3     44  10.3     183
      184 01-711-1012 Xanomeline High Dose    17   8.55    17   8.55    184
      185 01-711-1036 Placebo                 25  NA       25  NA       185
      186 01-711-1143 Xanomeline Low Dose     21  15.4     21  15.4     186
      187 01-711-1433 Xanomeline High Dose    13   8.55    13   8.55    187
      188 01-713-1043 Xanomeline Low Dose     19  13.7     19  13.7     188
      189 01-713-1073 Xanomeline Low Dose     20  10.3     20  10.3     189
      190 01-713-1106 Xanomeline High Dose    37  12.0     37  12.0     190
      191 01-713-1141 Xanomeline High Dose    28  12.0     28  12.0     191
      192 01-713-1179 Placebo                 29   8.55    29   8.55    192
      193 01-713-1209 Xanomeline High Dose    21   6.84    21   6.84    193
      194 01-713-1256 Placebo                 15   8.55    15   8.55    194
      195 01-713-1269 Placebo                 19  12.0     19  12.0     195
      196 01-713-1448 Xanomeline Low Dose     22  10.3     22  10.3     196
      197 01-714-1035 Placebo                 37  18.8     37  18.8     197
      198 01-714-1068 Xanomeline Low Dose     21   6.84    21   6.84    198
      199 01-714-1195 Xanomeline Low Dose     39  18.8     39  18.8     199
      200 01-714-1288 Xanomeline High Dose    24  12.0     24  12.0     200
      201 01-714-1375 Placebo                 19  10.3     19  10.3     201
      202 01-714-1425 Xanomeline High Dose    22  12.0     22  12.0     202
      203 01-715-1085 Xanomeline Low Dose     19  13.7     19  13.7     203
      204 01-715-1107 Xanomeline Low Dose     39   8.55    39   8.55    204
      205 01-715-1155 Placebo                 13   6.84    13   6.84    205
      206 01-715-1207 Placebo                 14  10.3     14  10.3     206
      207 01-715-1319 Xanomeline High Dose    15  10.3     15  10.3     207
      208 01-715-1321 Xanomeline High Dose    29   8.55    29   8.55    208
      209 01-715-1397 Placebo                 16   8.55    16   8.55    209
      210 01-715-1405 Xanomeline Low Dose     20   6.84    20   6.84    210
      211 01-716-1024 Placebo                 17  15.4     17  15.4     211
      212 01-716-1026 Placebo                 21   6.84    21   6.84    212
      213 01-716-1030 Xanomeline High Dose    17  12.0     17  12.0     213
      214 01-716-1044 Placebo                 18  30.8     18  30.8     214
      215 01-716-1063 Xanomeline Low Dose     22  12.0     22  12.0     215
      216 01-716-1071 Xanomeline High Dose    21  10.3     21  10.3     216
      217 01-716-1094 Xanomeline Low Dose     22  15.4     22  15.4     217
      218 01-716-1103 Xanomeline Low Dose     15   6.84    15   6.84    218
      219 01-716-1108 Placebo                 16  13.7     16  13.7     219
      220 01-716-1151 Xanomeline Low Dose     40   6.84    40   6.84    220
      221 01-716-1157 Xanomeline Low Dose     21   8.55    21   8.55    221
      222 01-716-1160 Placebo                 26  10.3     26  10.3     222
      223 01-716-1167 Xanomeline Low Dose     39  10.3     39  10.3     223
      224 01-716-1177 Placebo                 17  15.4     17  15.4     224
      225 01-716-1189 Xanomeline High Dose    33   8.55    33   8.55    225
      226 01-716-1229 Xanomeline High Dose    38   5.13    38   5.13    226
      227 01-716-1298 Xanomeline Low Dose     15   8.55    15   8.55    227
      228 01-716-1308 Placebo                 14   6.84    14   6.84    228
      229 01-716-1311 Xanomeline Low Dose     26  18.8     26  18.8     229
      230 01-716-1364 Xanomeline High Dose    36  15.4     36  15.4     230
      231 01-716-1373 Xanomeline High Dose    50  12.0     50  12.0     231
      232 01-716-1418 Xanomeline High Dose    13  13.7     13  13.7     232
      233 01-716-1441 Placebo                 20  18.8     20  18.8     233
      234 01-716-1447 Xanomeline High Dose    27   8.55    27   8.55    234
      235 01-717-1004 Xanomeline Low Dose     26   6.84    26   6.84    235
      236 01-717-1109 Xanomeline High Dose    30  20.5     30  20.5     236
      237 01-717-1174 Xanomeline High Dose    37  13.7     37  13.7     237
      238 01-717-1201 Placebo                 23   8.55    23   8.55    238
      239 01-717-1344 Placebo                 12  10.3     12  10.3     239
      240 01-717-1357 Xanomeline High Dose    23  20.5     23  20.5     240
      241 01-717-1446 Xanomeline Low Dose     41  15.4     41  15.4     241
      242 01-718-1066 Xanomeline Low Dose     19   6.84    19   6.84    242
      243 01-718-1079 Xanomeline Low Dose     23   8.55    23   8.55    243
      244 01-718-1101 Xanomeline High Dose    17  15.4     17  15.4     244
      245 01-718-1139 Placebo                 21  12.0     21  12.0     245
      246 01-718-1150 Placebo                 76  12.0     76  12.0     246
      247 01-718-1170 Xanomeline Low Dose     17  10.3     17  10.3     247
      248 01-718-1172 Placebo                 18  17.1     18  17.1     248
      249 01-718-1250 Xanomeline Low Dose     19   6.84    19   6.84    249
      250 01-718-1254 Xanomeline Low Dose     22  12.0     22  12.0     250
      251 01-718-1328 Xanomeline High Dose    28  13.7     28  13.7     251
      252 01-718-1355 Placebo                 13  15.4     13  15.4     252
      253 01-718-1371 Xanomeline High Dose    30   8.55    30   8.55    253
      254 01-718-1427 Xanomeline High Dose    16  10.3     16  10.3     254

---

    Code
      fig[[y]]
    Output
      Aesthetic mapping: 
      * `x`      -> `XVAR`
      * `y`      -> `YVAR`
      * `shape`  -> `.data[["TRTVAR"]]`
      * `colour` -> `.data[["TRTVAR"]]`
      * `size`   -> `.data[["TRTVAR"]]`

---

    Code
      fig[[y]]
    Output
      $axis.title.x
      List of 11
       $ family       : NULL
       $ face         : chr "plain"
       $ colour       : NULL
       $ size         : num 12
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $axis.title.y
      List of 11
       $ family       : NULL
       $ face         : chr "plain"
       $ colour       : NULL
       $ size         : num 12
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $axis.text.x
      List of 11
       $ family       : NULL
       $ face         : chr "plain"
       $ colour       : NULL
       $ size         : num 8
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : num 0
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $axis.text.y
      List of 11
       $ family       : NULL
       $ face         : chr "plain"
       $ colour       : NULL
       $ size         : num 8
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : num 0
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $legend.background
      List of 5
       $ fill         : NULL
       $ colour       : chr "black"
       $ linewidth    : NULL
       $ linetype     : chr "solid"
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_rect" "element"
      
      $legend.position
      [1] "bottom"
      
      $legend.direction
      [1] "horizontal"
      
      $panel.background
      List of 5
       $ fill         : chr "white"
       $ colour       : chr "black"
       $ linewidth    : NULL
       $ linetype     : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_rect" "element"
      
      $panel.border
      List of 5
       $ fill         : logi NA
       $ colour       : chr "black"
       $ linewidth    : num 1
       $ linetype     : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_rect" "element"
      
      $plot.title
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : NULL
       $ hjust        : num 0.5
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      attr(,"complete")
      [1] FALSE
      attr(,"validate")
      [1] TRUE

---

    Code
      fig[[y]]
    Output
      $x
      [1] ""
      
      $y
      [1] ""
      
      $title
      [1] "Scatter Plot of maximum ALT vs BILI"
      
      $shape
      [1] "TRTVAR"
      
      $colour
      [1] "TRTVAR"
      
      $size
      [1] "TRTVAR"
      

