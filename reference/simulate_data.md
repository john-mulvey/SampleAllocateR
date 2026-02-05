# Simulate Data with Optional Blocking

This function generates a simulated dataset containing subject-level
data. Users can optionally specify block sizes to generate multiple
samples per subject.

## Usage

``` r
simulate_data(n_samples, block_size = NULL, seed = 123)
```

## Arguments

- n_samples:

  Integer; the total number of samples to generate. When `block_size` \>
  1, this must be a multiple of `block_size`.

- block_size:

  Integer; the number of samples per subject. If 1 or NULL (default),
  generates one sample per subject. If \> 1, generates multiple samples
  per subject.

- seed:

  Integer; the seed for random number generation to ensure
  reproducibility.

## Value

A `data.frame` with the following structure:

- `sample_id`: Unique identifier for each sample

- `age_at_baseline`: Subject's age at baseline (normally distributed,
  mean=55, sd=10)

- `bmi_at_baseline`: Subject's BMI at baseline (normally distributed,
  mean=30, sd=5)

- `sex`: Subject's sex ("M" or "F")

- `sample_timepoint`: Timepoint identifier (only present when
  `block_size` \> 1)

- `treatment`: Treatment assignment ("treatment" or "placebo")

- `subject_id`: Subject identifier

## Details

When `block_size` is greater than 1, the function generates multiple
samples per subject, creating a `sample_timepoint` column with labels
"timepoint_1", "timepoint_2", etc. Subject-level characteristics (age,
BMI, sex, treatment) remain constant across all samples for the same
subject.

## Examples

``` r
# Generate data with one sample per subject
simulate_data(n_samples = 100)
#>      sample_id age_at_baseline bmi_at_baseline sex treatment  subject_id
#> 1     sample_1        57.53319        33.93869   F treatment   subject_1
#> 2     sample_2        54.71453        33.84521   F treatment   subject_2
#> 3     sample_3        54.57130        31.66101   F treatment   subject_3
#> 4     sample_4        68.68602        24.95812   F   placebo   subject_4
#> 5     sample_5        52.74229        29.40274   M treatment   subject_5
#> 6     sample_6        70.16471        28.59802   M   placebo   subject_6
#> 7     sample_7        39.51247        32.81495   M   placebo   subject_7
#> 8     sample_8        60.84614        28.13781   M   placebo   subject_8
#> 9     sample_9        56.23854        34.88487   F treatment   subject_9
#> 10   sample_10        57.15942        28.12710   M treatment  subject_10
#> 11   sample_11        58.79639        35.26356   F   placebo  subject_11
#> 12   sample_12        49.97677        24.75411   M   placebo  subject_12
#> 13   sample_13        51.66793        23.69922   F   placebo  subject_13
#> 14   sample_14        44.81425        46.20520   M treatment  subject_14
#> 15   sample_15        44.28209        27.91571   F   placebo  subject_15
#> 16   sample_16        58.03529        31.49114   F treatment  subject_16
#> 17   sample_17        59.48210        33.18285   F   placebo  subject_17
#> 18   sample_18        55.53004        27.58110   M treatment  subject_18
#> 19   sample_19        64.22267        32.58431   F treatment  subject_19
#> 20   sample_20        75.50085        31.84482   F treatment  subject_20
#> 21   sample_21        50.08969        28.92310   F treatment  subject_21
#> 22   sample_22        31.90831        30.32647   F   placebo  subject_22
#> 23   sample_23        65.05739        29.82966   M treatment  subject_23
#> 24   sample_24        47.90799        40.64226   F treatment  subject_24
#> 25   sample_25        48.11991        26.29332   M treatment  subject_25
#> 26   sample_26        65.25571        24.52002   F treatment  subject_26
#> 27   sample_27        52.15227        30.18894   M   placebo  subject_27
#> 28   sample_28        42.79282        31.55240   M   placebo  subject_28
#> 29   sample_29        56.81303        32.18262   M treatment  subject_29
#> 30   sample_30        53.61109        27.70817   M   placebo  subject_30
#> 31   sample_31        55.05764        24.68337   F treatment  subject_31
#> 32   sample_32        58.85280        36.31593   F   placebo  subject_32
#> 33   sample_33        51.29340        28.25175   F treatment  subject_33
#> 34   sample_34        61.44377        25.67244   M   placebo  subject_34
#> 35   sample_35        52.79513        28.81860   M   placebo  subject_35
#> 36   sample_36        58.31782        29.01412   F treatment  subject_36
#> 37   sample_37        65.96839        35.54960   F treatment  subject_37
#> 38   sample_38        59.35181        30.42369   F treatment  subject_38
#> 39   sample_39        51.74068        33.77027   F treatment  subject_39
#> 40   sample_40        66.48808        27.50354   M   placebo  subject_40
#> 41   sample_41        64.93504        31.07223   M treatment  subject_41
#> 42   sample_42        60.48397        28.37657   M   placebo  subject_42
#> 43   sample_43        57.38732        30.47292   F   placebo  subject_43
#> 44   sample_44        48.72094        25.52318   F treatment  subject_44
#> 45   sample_45        68.60652        23.44599   F treatment  subject_45
#> 46   sample_46        48.99740        39.98607   M treatment  subject_46
#> 47   sample_47        76.87333        33.00354   M treatment  subject_47
#> 48   sample_48        70.32611        23.74364   F   placebo  subject_48
#> 49   sample_49        52.64300        26.94417   F treatment  subject_49
#> 50   sample_50        44.73579        24.07260   M treatment  subject_50
#> 51   sample_51        47.89593        40.99405   F   placebo  subject_51
#> 52   sample_52        57.56884        36.56206   M treatment  subject_52
#> 53   sample_53        52.53308        28.67427   F treatment  subject_53
#> 54   sample_54        51.52457        32.71597   F treatment  subject_54
#> 55   sample_55        45.48381        27.92830   F treatment  subject_55
#> 56   sample_56        54.54972        27.61877   M   placebo  subject_56
#> 57   sample_57        47.15096        26.05699   F   placebo  subject_57
#> 58   sample_58        38.32058        27.02691   F treatment  subject_58
#> 59   sample_59        51.19773        38.25454   F   placebo  subject_59
#> 60   sample_60        64.18997        29.72986   F treatment  subject_60
#> 61   sample_61        49.24653        30.59623   M treatment  subject_61
#> 62   sample_62        61.07964        31.21844   F   placebo  subject_62
#> 63   sample_63        38.82117        36.16238   F   placebo  subject_63
#> 64   sample_64        54.44438        27.41968   M treatment  subject_64
#> 65   sample_65        60.19407        25.03746   F treatment  subject_65
#> 66   sample_66        58.01153        38.37848   F   placebo  subject_66
#> 67   sample_67        56.05676        27.79418   F treatment  subject_67
#> 68   sample_68        48.59294        26.38467   F treatment  subject_68
#> 69   sample_69        46.50296        23.81863   F treatment  subject_69
#> 70   sample_70        44.75871        23.57642   M treatment  subject_70
#> 71   sample_71        56.17647        27.13013   F   placebo  subject_71
#> 72   sample_72        45.52525        33.08993   M treatment  subject_72
#> 73   sample_73        50.09443        35.54924   F treatment  subject_73
#> 74   sample_74        52.43908        33.53794   M treatment  subject_74
#> 75   sample_75        73.43862        28.18171   M treatment  subject_75
#> 76   sample_76        48.48050        30.29875   F   placebo  subject_76
#> 77   sample_77        57.35387        26.47702   M   placebo  subject_77
#> 78   sample_78        55.77961        26.41391   F treatment  subject_78
#> 79   sample_79        45.38143        34.42325   F   placebo  subject_79
#> 80   sample_80        54.28692        24.92204   M   placebo  subject_80
#> 81   sample_81        69.44551        39.77647   M   placebo  subject_81
#> 82   sample_82        59.51504        29.54840   F   placebo  subject_82
#> 83   sample_83        55.41233        31.07269   M treatment  subject_83
#> 84   sample_84        50.77503        26.30736   M   placebo  subject_84
#> 85   sample_85        34.46753        27.12806   F   placebo  subject_85
#> 86   sample_86        66.31337        23.41492   M   placebo  subject_86
#> 87   sample_87        40.39360        29.08537   M treatment  subject_87
#> 88   sample_88        62.39948        32.09491   F treatment  subject_88
#> 89   sample_89        74.09104        31.62152   F   placebo  subject_89
#> 90   sample_90        40.56107        26.09232   M treatment  subject_90
#> 91   sample_91        62.01784        26.05689   M   placebo  subject_91
#> 92   sample_92        52.37803        27.48901   F   placebo  subject_92
#> 93   sample_93        39.27856        37.48030   M treatment  subject_93
#> 94   sample_94        39.85332        24.31348   F   placebo  subject_94
#> 95   sample_95        38.98464        29.10474   M   placebo  subject_95
#> 96   sample_96        49.69093        39.51181   F treatment  subject_96
#> 97   sample_97        40.38244        29.49513   M treatment  subject_97
#> 98   sample_98        61.87917        23.20080   M   placebo  subject_98
#> 99   sample_99        76.00109        26.67615   F treatment  subject_99
#> 100 sample_100        42.12970        32.42730   F treatment subject_100
simulate_data(n_samples = 100, block_size = 1)
#>      sample_id age_at_baseline bmi_at_baseline sex treatment  subject_id
#> 1     sample_1        57.53319        33.93869   F treatment   subject_1
#> 2     sample_2        54.71453        33.84521   F treatment   subject_2
#> 3     sample_3        54.57130        31.66101   F treatment   subject_3
#> 4     sample_4        68.68602        24.95812   F   placebo   subject_4
#> 5     sample_5        52.74229        29.40274   M treatment   subject_5
#> 6     sample_6        70.16471        28.59802   M   placebo   subject_6
#> 7     sample_7        39.51247        32.81495   M   placebo   subject_7
#> 8     sample_8        60.84614        28.13781   M   placebo   subject_8
#> 9     sample_9        56.23854        34.88487   F treatment   subject_9
#> 10   sample_10        57.15942        28.12710   M treatment  subject_10
#> 11   sample_11        58.79639        35.26356   F   placebo  subject_11
#> 12   sample_12        49.97677        24.75411   M   placebo  subject_12
#> 13   sample_13        51.66793        23.69922   F   placebo  subject_13
#> 14   sample_14        44.81425        46.20520   M treatment  subject_14
#> 15   sample_15        44.28209        27.91571   F   placebo  subject_15
#> 16   sample_16        58.03529        31.49114   F treatment  subject_16
#> 17   sample_17        59.48210        33.18285   F   placebo  subject_17
#> 18   sample_18        55.53004        27.58110   M treatment  subject_18
#> 19   sample_19        64.22267        32.58431   F treatment  subject_19
#> 20   sample_20        75.50085        31.84482   F treatment  subject_20
#> 21   sample_21        50.08969        28.92310   F treatment  subject_21
#> 22   sample_22        31.90831        30.32647   F   placebo  subject_22
#> 23   sample_23        65.05739        29.82966   M treatment  subject_23
#> 24   sample_24        47.90799        40.64226   F treatment  subject_24
#> 25   sample_25        48.11991        26.29332   M treatment  subject_25
#> 26   sample_26        65.25571        24.52002   F treatment  subject_26
#> 27   sample_27        52.15227        30.18894   M   placebo  subject_27
#> 28   sample_28        42.79282        31.55240   M   placebo  subject_28
#> 29   sample_29        56.81303        32.18262   M treatment  subject_29
#> 30   sample_30        53.61109        27.70817   M   placebo  subject_30
#> 31   sample_31        55.05764        24.68337   F treatment  subject_31
#> 32   sample_32        58.85280        36.31593   F   placebo  subject_32
#> 33   sample_33        51.29340        28.25175   F treatment  subject_33
#> 34   sample_34        61.44377        25.67244   M   placebo  subject_34
#> 35   sample_35        52.79513        28.81860   M   placebo  subject_35
#> 36   sample_36        58.31782        29.01412   F treatment  subject_36
#> 37   sample_37        65.96839        35.54960   F treatment  subject_37
#> 38   sample_38        59.35181        30.42369   F treatment  subject_38
#> 39   sample_39        51.74068        33.77027   F treatment  subject_39
#> 40   sample_40        66.48808        27.50354   M   placebo  subject_40
#> 41   sample_41        64.93504        31.07223   M treatment  subject_41
#> 42   sample_42        60.48397        28.37657   M   placebo  subject_42
#> 43   sample_43        57.38732        30.47292   F   placebo  subject_43
#> 44   sample_44        48.72094        25.52318   F treatment  subject_44
#> 45   sample_45        68.60652        23.44599   F treatment  subject_45
#> 46   sample_46        48.99740        39.98607   M treatment  subject_46
#> 47   sample_47        76.87333        33.00354   M treatment  subject_47
#> 48   sample_48        70.32611        23.74364   F   placebo  subject_48
#> 49   sample_49        52.64300        26.94417   F treatment  subject_49
#> 50   sample_50        44.73579        24.07260   M treatment  subject_50
#> 51   sample_51        47.89593        40.99405   F   placebo  subject_51
#> 52   sample_52        57.56884        36.56206   M treatment  subject_52
#> 53   sample_53        52.53308        28.67427   F treatment  subject_53
#> 54   sample_54        51.52457        32.71597   F treatment  subject_54
#> 55   sample_55        45.48381        27.92830   F treatment  subject_55
#> 56   sample_56        54.54972        27.61877   M   placebo  subject_56
#> 57   sample_57        47.15096        26.05699   F   placebo  subject_57
#> 58   sample_58        38.32058        27.02691   F treatment  subject_58
#> 59   sample_59        51.19773        38.25454   F   placebo  subject_59
#> 60   sample_60        64.18997        29.72986   F treatment  subject_60
#> 61   sample_61        49.24653        30.59623   M treatment  subject_61
#> 62   sample_62        61.07964        31.21844   F   placebo  subject_62
#> 63   sample_63        38.82117        36.16238   F   placebo  subject_63
#> 64   sample_64        54.44438        27.41968   M treatment  subject_64
#> 65   sample_65        60.19407        25.03746   F treatment  subject_65
#> 66   sample_66        58.01153        38.37848   F   placebo  subject_66
#> 67   sample_67        56.05676        27.79418   F treatment  subject_67
#> 68   sample_68        48.59294        26.38467   F treatment  subject_68
#> 69   sample_69        46.50296        23.81863   F treatment  subject_69
#> 70   sample_70        44.75871        23.57642   M treatment  subject_70
#> 71   sample_71        56.17647        27.13013   F   placebo  subject_71
#> 72   sample_72        45.52525        33.08993   M treatment  subject_72
#> 73   sample_73        50.09443        35.54924   F treatment  subject_73
#> 74   sample_74        52.43908        33.53794   M treatment  subject_74
#> 75   sample_75        73.43862        28.18171   M treatment  subject_75
#> 76   sample_76        48.48050        30.29875   F   placebo  subject_76
#> 77   sample_77        57.35387        26.47702   M   placebo  subject_77
#> 78   sample_78        55.77961        26.41391   F treatment  subject_78
#> 79   sample_79        45.38143        34.42325   F   placebo  subject_79
#> 80   sample_80        54.28692        24.92204   M   placebo  subject_80
#> 81   sample_81        69.44551        39.77647   M   placebo  subject_81
#> 82   sample_82        59.51504        29.54840   F   placebo  subject_82
#> 83   sample_83        55.41233        31.07269   M treatment  subject_83
#> 84   sample_84        50.77503        26.30736   M   placebo  subject_84
#> 85   sample_85        34.46753        27.12806   F   placebo  subject_85
#> 86   sample_86        66.31337        23.41492   M   placebo  subject_86
#> 87   sample_87        40.39360        29.08537   M treatment  subject_87
#> 88   sample_88        62.39948        32.09491   F treatment  subject_88
#> 89   sample_89        74.09104        31.62152   F   placebo  subject_89
#> 90   sample_90        40.56107        26.09232   M treatment  subject_90
#> 91   sample_91        62.01784        26.05689   M   placebo  subject_91
#> 92   sample_92        52.37803        27.48901   F   placebo  subject_92
#> 93   sample_93        39.27856        37.48030   M treatment  subject_93
#> 94   sample_94        39.85332        24.31348   F   placebo  subject_94
#> 95   sample_95        38.98464        29.10474   M   placebo  subject_95
#> 96   sample_96        49.69093        39.51181   F treatment  subject_96
#> 97   sample_97        40.38244        29.49513   M treatment  subject_97
#> 98   sample_98        61.87917        23.20080   M   placebo  subject_98
#> 99   sample_99        76.00109        26.67615   F treatment  subject_99
#> 100 sample_100        42.12970        32.42730   F treatment subject_100

# Generate data with multiple samples per subject
simulate_data(n_samples = 102, block_size = 3)
#>      sample_id age_at_baseline bmi_at_baseline sex sample_timepoint treatment
#> 1     sample_1        35.33383        29.85727   F      timepoint_1 treatment
#> 2     sample_2        35.33383        29.85727   F      timepoint_2 treatment
#> 3     sample_3        35.33383        29.85727   F      timepoint_3 treatment
#> 4     sample_4        62.01356        29.78565   F      timepoint_1 treatment
#> 5     sample_5        62.01356        29.78565   F      timepoint_2 treatment
#> 6     sample_6        62.01356        29.78565   F      timepoint_3 treatment
#> 7     sample_7        50.27209        36.84301   F      timepoint_1 treatment
#> 8     sample_8        50.27209        36.84301   F      timepoint_2 treatment
#> 9     sample_9        50.27209        36.84301   F      timepoint_3 treatment
#> 10   sample_10        44.32176        28.87115   F      timepoint_1   placebo
#> 11   sample_11        44.32176        28.87115   F      timepoint_2   placebo
#> 12   sample_12        44.32176        28.87115   F      timepoint_3   placebo
#> 13   sample_13        52.82025        37.58235   M      timepoint_1 treatment
#> 14   sample_14        52.82025        37.58235   M      timepoint_2 treatment
#> 15   sample_15        52.82025        37.58235   M      timepoint_3 treatment
#> 16   sample_16        44.73996        22.25624   M      timepoint_1   placebo
#> 17   sample_17        44.73996        22.25624   M      timepoint_2   placebo
#> 18   sample_18        44.73996        22.25624   M      timepoint_3   placebo
#> 19   sample_19        47.71109        32.92307   M      timepoint_1   placebo
#> 20   sample_20        47.71109        32.92307   M      timepoint_2   placebo
#> 21   sample_21        47.71109        32.92307   M      timepoint_3   placebo
#> 22   sample_22        48.74961        30.61927   F      timepoint_1   placebo
#> 23   sample_23        48.74961        30.61927   F      timepoint_2   placebo
#> 24   sample_24        48.74961        30.61927   F      timepoint_3   placebo
#> 25   sample_25        38.13307        31.07971   F      timepoint_1 treatment
#> 26   sample_26        38.13307        31.07971   F      timepoint_2 treatment
#> 27   sample_27        38.13307        31.07971   F      timepoint_3 treatment
#> 28   sample_28        63.37787        31.89820   F      timepoint_1 treatment
#> 29   sample_29        63.37787        31.89820   F      timepoint_2 treatment
#> 30   sample_30        63.37787        31.89820   F      timepoint_3 treatment
#> 31   sample_31        56.53373        27.48838   F      timepoint_1   placebo
#> 32   sample_32        56.53373        27.48838   F      timepoint_2   placebo
#> 33   sample_33        56.53373        27.48838   F      timepoint_3   placebo
#> 34   sample_34        43.61863        28.33396   M      timepoint_1   placebo
#> 35   sample_35        43.61863        28.33396   M      timepoint_2   placebo
#> 36   sample_36        43.61863        28.33396   M      timepoint_3   placebo
#> 37   sample_37        67.53815        24.90712   M      timepoint_1   placebo
#> 38   sample_38        67.53815        24.90712   M      timepoint_2   placebo
#> 39   sample_39        67.53815        24.90712   M      timepoint_3   placebo
#> 40   sample_40        59.26464        24.64104   M      timepoint_1 treatment
#> 41   sample_41        59.26464        24.64104   M      timepoint_2 treatment
#> 42   sample_42        59.26464        24.64104   M      timepoint_3 treatment
#> 43   sample_43        52.04929        31.51764   M      timepoint_1   placebo
#> 44   sample_44        52.04929        31.51764   M      timepoint_2   placebo
#> 45   sample_45        52.04929        31.51764   M      timepoint_3   placebo
#> 46   sample_46        63.95126        32.24105   F      timepoint_1 treatment
#> 47   sample_47        63.95126        32.24105   F      timepoint_2 treatment
#> 48   sample_48        63.95126        32.24105   F      timepoint_3 treatment
#> 49   sample_49        63.78133        30.26502   F      timepoint_1   placebo
#> 50   sample_50        63.78133        30.26502   F      timepoint_2   placebo
#> 51   sample_51        63.78133        30.26502   F      timepoint_3   placebo
#> 52   sample_52        63.21581        34.61134   M      timepoint_1 treatment
#> 53   sample_53        63.21581        34.61134   M      timepoint_2 treatment
#> 54   sample_54        63.21581        34.61134   M      timepoint_3 treatment
#> 55   sample_55        61.88640        40.25042   M      timepoint_1 treatment
#> 56   sample_56        61.88640        40.25042   M      timepoint_2 treatment
#> 57   sample_57        61.88640        40.25042   M      timepoint_3 treatment
#> 58   sample_58        60.53918        27.54484   F      timepoint_1 treatment
#> 59   sample_59        60.53918        27.54484   F      timepoint_2 treatment
#> 60   sample_60        60.53918        27.54484   F      timepoint_3 treatment
#> 61   sample_61        54.38088        18.45416   F      timepoint_1 treatment
#> 62   sample_62        54.38088        18.45416   F      timepoint_2 treatment
#> 63   sample_63        54.38088        18.45416   F      timepoint_3 treatment
#> 64   sample_64        51.94037        35.02869   M      timepoint_1   placebo
#> 65   sample_65        51.94037        35.02869   M      timepoint_2   placebo
#> 66   sample_66        51.94037        35.02869   M      timepoint_3   placebo
#> 67   sample_67        51.19529        26.45400   M      timepoint_1 treatment
#> 68   sample_68        51.19529        26.45400   M      timepoint_2 treatment
#> 69   sample_69        51.19529        26.45400   M      timepoint_3 treatment
#> 70   sample_70        48.05293        26.55996   F      timepoint_1 treatment
#> 71   sample_71        48.05293        26.55996   F      timepoint_2 treatment
#> 72   sample_72        48.05293        26.55996   F      timepoint_3 treatment
#> 73   sample_73        52.92083        35.12786   F      timepoint_1 treatment
#> 74   sample_74        52.92083        35.12786   F      timepoint_2 treatment
#> 75   sample_75        52.92083        35.12786   F      timepoint_3 treatment
#> 76   sample_76        42.34604        28.57613   M      timepoint_1 treatment
#> 77   sample_77        42.34604        28.57613   M      timepoint_2 treatment
#> 78   sample_78        42.34604        28.57613   M      timepoint_3 treatment
#> 79   sample_79        76.68956        23.89641   M      timepoint_1   placebo
#> 80   sample_80        76.68956        23.89641   M      timepoint_2   placebo
#> 81   sample_81        76.68956        23.89641   M      timepoint_3   placebo
#> 82   sample_82        67.07962        30.90652   F      timepoint_1   placebo
#> 83   sample_83        67.07962        30.90652   F      timepoint_2   placebo
#> 84   sample_84        67.07962        30.90652   F      timepoint_3   placebo
#> 85   sample_85        43.76891        29.30554   M      timepoint_1 treatment
#> 86   sample_86        43.76891        29.30554   M      timepoint_2 treatment
#> 87   sample_87        43.76891        29.30554   M      timepoint_3 treatment
#> 88   sample_88        50.97115        30.02882   F      timepoint_1   placebo
#> 89   sample_89        50.97115        30.02882   F      timepoint_2   placebo
#> 90   sample_90        50.97115        30.02882   F      timepoint_3   placebo
#> 91   sample_91        50.33345        31.92640   F      timepoint_1 treatment
#> 92   sample_92        50.33345        31.92640   F      timepoint_2 treatment
#> 93   sample_93        50.33345        31.92640   F      timepoint_3 treatment
#> 94   sample_94        62.79965        28.14670   F      timepoint_1   placebo
#> 95   sample_95        62.79965        28.14670   F      timepoint_2   placebo
#> 96   sample_96        62.79965        28.14670   F      timepoint_3   placebo
#> 97   sample_97        54.16631        33.22188   F      timepoint_1 treatment
#> 98   sample_98        54.16631        33.22188   F      timepoint_2 treatment
#> 99   sample_99        54.16631        33.22188   F      timepoint_3 treatment
#> 100 sample_100        57.53319        28.89757   M      timepoint_1   placebo
#> 101 sample_101        57.53319        28.89757   M      timepoint_2   placebo
#> 102 sample_102        57.53319        28.89757   M      timepoint_3   placebo
#>     subject_id
#> 1    subject_1
#> 2    subject_1
#> 3    subject_1
#> 4    subject_2
#> 5    subject_2
#> 6    subject_2
#> 7    subject_3
#> 8    subject_3
#> 9    subject_3
#> 10   subject_4
#> 11   subject_4
#> 12   subject_4
#> 13   subject_5
#> 14   subject_5
#> 15   subject_5
#> 16   subject_6
#> 17   subject_6
#> 18   subject_6
#> 19   subject_7
#> 20   subject_7
#> 21   subject_7
#> 22   subject_8
#> 23   subject_8
#> 24   subject_8
#> 25   subject_9
#> 26   subject_9
#> 27   subject_9
#> 28  subject_10
#> 29  subject_10
#> 30  subject_10
#> 31  subject_11
#> 32  subject_11
#> 33  subject_11
#> 34  subject_12
#> 35  subject_12
#> 36  subject_12
#> 37  subject_13
#> 38  subject_13
#> 39  subject_13
#> 40  subject_14
#> 41  subject_14
#> 42  subject_14
#> 43  subject_15
#> 44  subject_15
#> 45  subject_15
#> 46  subject_16
#> 47  subject_16
#> 48  subject_16
#> 49  subject_17
#> 50  subject_17
#> 51  subject_17
#> 52  subject_18
#> 53  subject_18
#> 54  subject_18
#> 55  subject_19
#> 56  subject_19
#> 57  subject_19
#> 58  subject_20
#> 59  subject_20
#> 60  subject_20
#> 61  subject_21
#> 62  subject_21
#> 63  subject_21
#> 64  subject_22
#> 65  subject_22
#> 66  subject_22
#> 67  subject_23
#> 68  subject_23
#> 69  subject_23
#> 70  subject_24
#> 71  subject_24
#> 72  subject_24
#> 73  subject_25
#> 74  subject_25
#> 75  subject_25
#> 76  subject_26
#> 77  subject_26
#> 78  subject_26
#> 79  subject_27
#> 80  subject_27
#> 81  subject_27
#> 82  subject_28
#> 83  subject_28
#> 84  subject_28
#> 85  subject_29
#> 86  subject_29
#> 87  subject_29
#> 88  subject_30
#> 89  subject_30
#> 90  subject_30
#> 91  subject_31
#> 92  subject_31
#> 93  subject_31
#> 94  subject_32
#> 95  subject_32
#> 96  subject_32
#> 97  subject_33
#> 98  subject_33
#> 99  subject_33
#> 100 subject_34
#> 101 subject_34
#> 102 subject_34
```
