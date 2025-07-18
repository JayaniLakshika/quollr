# predict_emb() works

    Code
      predict_emb(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
      model_2d = scurve_model_obj$model_2d)
    Output
      # A tibble: 5,000 x 4
         pred_emb_1 pred_emb_2    ID pred_h
              <dbl>      <dbl> <int>  <int>
       1      0.691      0.848     1    205
       2      0.191      0.416     2    109
       3      0.316      0.199     3     66
       4      0.774      0.560     4    146
       5      0.774      0.560     5    146
       6      0.441      0.704     6    172
       7      0.941      0.127     7     58
       8      0.275      0.416     8    110
       9      0.358      0.560     9    141
      10      0.316      0.343    10     96
      # i 4,990 more rows

# glance() works

    Code
      glance(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
      model_2d = scurve_model_obj$model_2d)
    Output
      # A tibble: 1 x 2
        Error  RMSE
        <dbl> <dbl>
      1 1554. 0.190

# augment() works

    Code
      augment(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
      model_2d = scurve_model_obj$model_2d)
    Output
      # A tibble: 5,000 x 32
            ID      x1     x2       x3       x4       x5       x6        x7 pred_h
         <int>   <dbl>  <dbl>    <dbl>    <dbl>    <dbl>    <dbl>     <dbl>  <int>
       1     1 -0.120  1.64   -1.99     0.0104   0.0125   0.0923  -0.00128     205
       2     2 -0.0492 1.51    0.00121 -0.0177   0.00726 -0.0362  -0.00535     109
       3     3 -0.774  1.30    0.367   -0.00173  0.0156  -0.0962   0.00335      66
       4     4 -0.606  0.246  -1.80    -0.00897 -0.0187  -0.0716   0.00126     146
       5     5 -0.478  0.0177 -1.88     0.00848  0.00533  0.0998   0.000677    146
       6     6  0.818  0.927  -1.58    -0.00318 -0.00980  0.0989   0.00696     172
       7     7  0.910  1.40    1.42     0.00699 -0.0182  -0.0710   0.00966      58
       8     8 -0.0691 1.59    0.00239  0.0127  -0.0130   0.0396  -0.000185    110
       9     9  0.859  1.59   -0.488   -0.0119   0.00421 -0.00440 -0.00595     141
      10    10 -0.727  1.62    0.314    0.00251  0.0177  -0.0755  -0.00369      96
      # i 4,990 more rows
      # i 23 more variables: model_high_d_x1 <dbl>, model_high_d_x2 <dbl>,
      #   model_high_d_x3 <dbl>, model_high_d_x4 <dbl>, model_high_d_x5 <dbl>,
      #   model_high_d_x6 <dbl>, model_high_d_x7 <dbl>, error_square_x1 <dbl>,
      #   error_square_x2 <dbl>, error_square_x3 <dbl>, error_square_x4 <dbl>,
      #   error_square_x5 <dbl>, error_square_x6 <dbl>, error_square_x7 <dbl>,
      #   row_wise_total_error <dbl>, abs_error_x1 <dbl>, abs_error_x2 <dbl>, ...

# gen_diffbin1_errors() works

    Code
      gen_diffbin1_errors(highd_data = scurve, nldr_data = scurve_umap)
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      shull: too many triangles to swap, will retry with some jitter
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      triangle collapsed!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Condition
      Warning in `shull.deltri()`:
      three points coincide or are collinear!
    Message
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
      v Model generated successfully! 🎉
    Output
             Error       RMSE b1 b2    b    m   a1   a2       d_bar
      1  3352.8204 0.44466064  5  6   30   23 0.25 0.22 0.041859004
      2  2971.2747 0.39403876  6  7   42   29 0.21 0.18 0.037915000
      3  2583.8410 0.33536554  7  8   56   36 0.18 0.15 0.033488496
      4  2410.1173 0.30981124  8  9   72   45 0.16 0.14 0.027993649
      5  2112.1766 0.26785217  9 10   90   57 0.14 0.12 0.022082074
      6  1953.7700 0.24476756 10 11  110   66 0.12 0.11 0.020325579
      7  1813.9410 0.22840888 11 12  132   83 0.11 0.10 0.015554171
      8  1734.6662 0.21595324 12 13  156   92 0.10 0.09 0.015060200
      9  1639.1125 0.20316827 13 14  182  108 0.10 0.08 0.012830893
      10 1574.0344 0.19475024 14 15  210  118 0.09 0.08 0.012463008
      11 1492.4751 0.18150697 15 16  240  137 0.08 0.07 0.010607461
      12 1410.4536 0.17127450 16 17  272  151 0.08 0.07 0.009936720
      13 1373.2410 0.16610785 17 18  306  163 0.07 0.06 0.009622895
      14 1321.9993 0.15868859 18 19  342  185 0.07 0.06 0.008378351
      15 1291.7696 0.15829612 19 20  380  200 0.07 0.06 0.007984180
      16 1235.7678 0.14871194 20 21  420  220 0.06 0.05 0.007317209
      17 1184.6976 0.14179306 21 22  462  244 0.06 0.05 0.006556965
      18 1149.1706 0.13691691 22 23  506  263 0.06 0.05 0.006190375
      19 1127.7520 0.13526760 23 24  552  285 0.05 0.05 0.005763986
      20 1086.4532 0.12822549 24 25  600  306 0.05 0.05 0.005438783
      21 1056.1440 0.12593313 25 26  650  335 0.05 0.04 0.004929857
      22 1016.2161 0.12033895 26 28  728  380 0.05 0.04 0.004461782
      23  968.0032 0.11400300 27 29  783  407 0.04 0.04 0.004184556
      24  973.3554 0.11568762 28 30  840  433 0.04 0.04 0.003963521
      25  931.9450 0.10851221 29 31  899  457 0.04 0.04 0.003810827
      26  929.6126 0.11004297 30 32  960  482 0.04 0.03 0.003652826
      27  899.0002 0.10518031 31 33 1023  519 0.04 0.03 0.003361819
      28  888.7247 0.10573197 32 34 1088  548 0.04 0.03 0.003203618
      29  861.6003 0.10093642 33 35 1155  583 0.04 0.03 0.003004655
      30  852.9075 0.10071411 34 36 1224  618 0.04 0.03 0.002836973
      31  834.0949 0.09783394 35 37 1295  645 0.03 0.03 0.002749311
      32  821.8817 0.09653055 36 38 1368  668 0.03 0.03 0.002704366
      33  806.9369 0.09446922 37 39 1443  696 0.03 0.03 0.002623402
      34  791.3879 0.09270057 38 40 1520  739 0.03 0.03 0.002455021
      35  788.1956 0.09284611 39 41 1599  766 0.03 0.03 0.002402235
      36  758.3317 0.08725485 40 42 1680  801 0.03 0.03 0.002302072
      37  756.3256 0.08869973 41 43 1763  827 0.03 0.03 0.002266229
      38  751.6400 0.08988466 42 44 1848  873 0.03 0.03 0.002130833
      39  727.1551 0.08425821 43 45 1935  902 0.03 0.02 0.002087830
      40  721.0089 0.08513472 44 46 2024  931 0.03 0.02 0.002044086
      41  714.8983 0.08412141 45 47 2115  976 0.03 0.02 0.001945490
      42  700.6963 0.08118693 46 48 2208  986 0.03 0.02 0.001980751
      43  691.0882 0.08017290 47 49 2303 1019 0.03 0.02 0.001928789
      44  691.4167 0.08214708 48 50 2400 1056 0.03 0.02 0.001873515
      45  673.5282 0.07823582 49 51 2499 1082 0.02 0.02 0.001845278
      46  676.0091 0.07998069 50 52 2600 1109 0.02 0.02 0.001827859
      47  653.9877 0.07660344 51 54 2754 1166 0.02 0.02 0.001770375
      48  652.7405 0.07623185 52 55 2860 1189 0.02 0.02 0.001766302
      49  642.6947 0.07430140 53 56 2968 1205 0.02 0.02 0.001771812
      50  642.8361 0.07504518 54 57 3078 1222 0.02 0.02 0.001779007
      51  632.0356 0.07368387 55 58 3190 1256 0.02 0.02 0.001742121
      52  623.0893 0.07264231 56 59 3304 1281 0.02 0.02 0.001724624
      53  624.1113 0.07281587 57 60 3420 1286 0.02 0.02 0.001754350
      54  621.0158 0.07273580 58 61 3538 1319 0.02 0.02 0.001719176
      55  619.7735 0.07248593 59 62 3658 1340 0.02 0.02 0.001719864
      56  611.7607 0.07170907 60 63 3780 1350 0.02 0.02 0.001737788
      57  609.1778 0.07206868 61 64 3904 1376 0.02 0.02 0.001718961
      58  604.3029 0.07057197 62 65 4030 1394 0.02 0.02 0.001723593
      59  604.2344 0.07114452 63 66 4158 1402 0.02 0.02 0.001737464
      60  594.2218 0.06934383 64 67 4288 1422 0.02 0.02 0.001735958
      61  599.3320 0.07015396 65 68 4420 1408 0.02 0.02 0.001797036
      62  594.3891 0.06975544 66 69 4554 1421 0.02 0.02 0.001812497
      63  592.2602 0.06940718 67 70 4690 1424 0.02 0.02 0.001842954
      64  594.9171 0.06987987 68 71 4828 1444 0.02 0.02 0.001830845
      65  597.8851 0.07016582 69 72 4968 1435 0.02 0.02 0.001888147
      66  592.2904 0.06952636 70 73 5110 1451 0.02 0.02 0.001891272
      67  582.6692 0.06802297 71 74 5254 1469 0.02 0.01 0.001884570
      68  590.6869 0.06921399 72 75 5400 1463 0.02 0.01 0.001932985
      69  588.1003 0.06900879 73 76 5548 1472 0.02 0.01 0.001944819
      70  587.9622 0.06868911 74 77 5698 1476 0.02 0.01 0.001966491

