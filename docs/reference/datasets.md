# Information on Data Sets in Packages

The [`data`](https://rdrr.io/r/utils/data.html) function is used both to
load data sets from packages, and give a display of the names and titles
of data sets in one or more packages, however it does not return a
result that can be easily used to get additional information about the
nature of data sets in packages.

## Usage

``` r
datasets(
  package,
  allClass = FALSE,
  incPackage = length(package) > 1,
  maxTitle = NULL
)
```

## Arguments

- package:

  a character vector giving the package(s) to look in

- allClass:

  a logical variable. Include all classes of the item (`TRUE`) or just
  the last class (`FALSE`)?

- incPackage:

  include the package name in result?

- maxTitle:

  maximum length of data set Title

## Value

A `data.frame` whose rows correspond to data sets found in `package`.

The columns (for a single package) are:

- Item:

  data set name, a character variable

- class:

  class, the object class of the data set, typically one of
  `"data.frame"`, `"table"`, `"array"` ...

- dim:

  an abbreviation of the dimensions of the data set, in a form like
  `"36x3"` for a data.frame or matrix with 36 rows and 3 columns.

- Title:

  data set title

## Details

The `datasets()` function is designed to produce a more useful summary
display of data sets in one or more packages. It extracts the `class`
and dimension information (`dim` or codelength) of each item, and
formats these to provide additional descriptors.

The requested packages must be installed, and are silently loaded in
order to extract `class` and size information.

## Note

In Rmd documents, `datasets("package") |> knitr::kable()` can be used to
create a more pleasing display.

## See also

[`data`](https://rdrr.io/r/utils/data.html),
[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)

## Author

Michael Friendly, with R-help from Curt Seeliger

## Examples

``` r
datasets("vcdExtra")
#>              Item      class       dim
#> 1        Abortion      table     2x2x2
#> 2        Accident data.frame      80x5
#> 3        AirCrash data.frame     439x5
#> 4       Alligator data.frame      80x5
#> 5        Asbestos      array       5x4
#> 6        Bartlett      table     2x2x2
#> 7            Burt data.frame      27x3
#> 8          Caesar      table   3x2x2x2
#> 9          Cancer      table     2x2x2
#> 10     Cormorants data.frame     343x8
#> 11 CrabSatellites data.frame     173x5
#> 12  CyclingDeaths data.frame     208x2
#> 13   DaytonSurvey data.frame      32x6
#> 14        Depends      table        15
#> 15      Detergent      table   2x2x2x3
#> 16         Donner data.frame      90x5
#> 17      Draft1970 data.frame     366x3
#> 18 Draft1970table      table      12x3
#> 19           Dyke      table 2x2x2x2x2
#> 20      Fungicide      array   2x2x2x2
#> 21            GSS data.frame       6x3
#> 22       Geissler data.frame      90x4
#> 23          Gilby      table       6x4
#> 24          Glass data.frame      25x3
#> 25   HairEyePlace      array     4x5x2
#> 26       Hauser79 data.frame      25x3
#> 27          Heart      table     2x2x3
#> 28        Heckman      table 2x2x2x2x2
#> 29     HospVisits      table       3x3
#> 30     HouseTasks      table      13x4
#> 31           Hoyt      table   4x3x7x2
#> 32            ICU data.frame    200x22
#> 33         JobSat      table       4x4
#> 34     Mammograms      array       4x4
#> 35         Mental data.frame      24x3
#> 36           Mice data.frame      30x4
#> 37       Mobility      table       5x5
#> 38        PhdPubs data.frame     915x6
#> 39     ShakeWords data.frame     100x2
#> 40             TV      array    5x11x3
#> 41       Titanicp data.frame    1309x6
#> 42       Toxaemia data.frame      60x5
#> 43        Vietnam data.frame      40x4
#> 44       Vote1980 data.frame      28x4
#> 45      WorkerSat data.frame       8x4
#> 46    Yamaguchi87 data.frame      75x4
#>                                                    Title
#> 1                                  Abortion Opinion Data
#> 2             Traffic Accident Victims in France in 1958
#> 3                                         Air Crash Data
#> 4                                  Alligator Food Choice
#> 5                         Effect of Exposure to Asbestos
#> 6                    Bartlett Data on Plum Root Cuttings
#> 7       Burt (1950) Data on Hair, Eyes, Head and Stature
#> 8         Risk Factors for Infection in Caesarian Births
#> 9                     Survival of Breast Cancer Patients
#> 10              Advertising Behavior by Males Cormorants
#> 11                                 Horseshoe Crab Mating
#> 12                                 London Cycling Deaths
#> 13                Dayton Student Survey on Substance Use
#> 14                            Dependencies of R Packages
#> 15                             Detergent Preference Data
#> 16                          Survival in the Donner Party
#> 17                           USA 1970 Draft Lottery Data
#> 18                          USA 1970 Draft Lottery Table
#> 19                        Sources of Knowledge of Cancer
#> 20                   Carcinogenic Effects of a Fungicide
#> 21      General Social Survey- Sex and Party affiliation
#> 22                Geissler's Data on the Human Sex Ratio
#> 23          Clothing and Intelligence Rating of Children
#> 24              British Social Mobility from Glass(1954)
#> 25    Hair Color and Eye Color in Caithness and Aberdeen
#> 26                 Hauser (1979) Data on Social Mobility
#> 27                     Sex, Occupation and Heart Disease
#> 28 Labour Force Participation of Married Women 1967-1971
#> 29                                  Hospital Visits Data
#> 30       Household Tasks Performed by Husbands and Wives
#> 31                       Minnesota High School Graduates
#> 32                                          ICU data set
#> 33    Cross-classification of job satisfaction by income
#> 34                                     Mammogram Ratings
#> 35                     Mental Impairment and Parents SES
#> 36                                   Mice Depletion Data
#> 37                                  Social Mobility data
#> 38                        Publications of PhD Candidates
#> 39                   Shakespeare's Word Type Frequencies
#> 40                                       TV Viewing Data
#> 41                             Passengers on the Titanic
#> 42                        Toxaemia Symptoms in Pregnancy
#> 43                 Student Opinion about the Vietnam War
#> 44       Race and Politics in the 1980 Presidential Vote
#> 45                              Worker Satisfaction Data
#> 46              Occupational Mobility in Three Countries
# datasets(c("vcd", "vcdExtra"))
datasets("datasets", maxTitle=50)
#>                              Item      class     dim
#> 1                   AirPassengers         ts     144
#> 2                         BJsales         ts     150
#> 3          BJsales.lead (BJsales)         ts     150
#> 4                             BOD data.frame     6x2
#> 5                             CO2 data.frame    84x5
#> 6                     ChickWeight data.frame   578x4
#> 7                           DNase data.frame   176x3
#> 8                  EuStockMarkets      array  1860x4
#> 9                    Formaldehyde data.frame     6x2
#> 10                   HairEyeColor      table   4x4x2
#> 11                   Harman23.cor       list       3
#> 12                   Harman74.cor       list       3
#> 13                       Indometh data.frame    66x3
#> 14                   InsectSprays data.frame    72x2
#> 15                 JohnsonJohnson         ts      84
#> 16                      LakeHuron         ts      98
#> 17               LifeCycleSavings data.frame    50x5
#> 18                       Loblolly data.frame    84x3
#> 19                           Nile         ts     100
#> 20                         Orange data.frame    35x3
#> 21                  OrchardSprays data.frame    64x4
#> 22                    PlantGrowth data.frame    30x2
#> 23                      Puromycin data.frame    23x3
#> 24                      Seatbelts      array   192x8
#> 25                         Theoph data.frame   132x5
#> 26                        Titanic      table 4x2x2x2
#> 27                    ToothGrowth data.frame    60x3
#> 28                  UCBAdmissions      table   2x2x6
#> 29                 UKDriverDeaths         ts     192
#> 30                          UKgas         ts     108
#> 31                    USAccDeaths         ts      72
#> 32                      USArrests data.frame    50x4
#> 33                 USJudgeRatings data.frame   43x12
#> 34          USPersonalExpenditure      array     5x5
#> 35                      UScitiesD       dist      45
#> 36                       VADeaths      array     5x4
#> 37                       WWWusage         ts     100
#> 38                    WorldPhones      array     7x7
#> 39                    ability.cov       list       3
#> 40                       airmiles         ts      24
#> 41                     airquality data.frame   153x6
#> 42                       anscombe data.frame    11x8
#> 43                         attenu data.frame   182x5
#> 44                       attitude data.frame    30x7
#> 45                        austres         ts      89
#> 46              beaver1 (beavers) data.frame   114x4
#> 47              beaver2 (beavers) data.frame   100x4
#> 48                           cars data.frame    50x2
#> 49                       chickwts data.frame    71x2
#> 50                            co2         ts     468
#> 51                        crimtab      table   42x22
#> 52                    discoveries         ts     100
#> 53                          esoph data.frame    88x5
#> 54                           euro    numeric      11
#> 55              euro.cross (euro)      array   11x11
#> 56                       eurodist       dist     210
#> 57                       faithful data.frame   272x2
#> 58         fdeaths (UKLungDeaths)         ts      72
#> 59                         freeny data.frame    39x5
#> 60              freeny.x (freeny)      array    39x4
#> 61              freeny.y (freeny)         ts      39
#> 62                           gait      array 20x39x2
#> 63                         infert data.frame   248x8
#> 64                           iris data.frame   150x5
#> 65                          iris3      array  50x4x3
#> 66                        islands    numeric      48
#> 67         ldeaths (UKLungDeaths)         ts      72
#> 68                             lh         ts      48
#> 69                        longley data.frame    16x7
#> 70                           lynx         ts     114
#> 71         mdeaths (UKLungDeaths)         ts      72
#> 72                         morley data.frame   100x3
#> 73                         mtcars data.frame   32x11
#> 74                         nhtemp         ts      60
#> 75                         nottem         ts     240
#> 76                            npk data.frame    24x5
#> 77             occupationalStatus      table     8x8
#> 78                       penguins data.frame   344x8
#> 79        penguins_raw (penguins) data.frame  344x17
#> 80                         precip    numeric      70
#> 81                     presidents         ts     120
#> 82                       pressure data.frame    19x2
#> 83                         quakes data.frame  1000x5
#> 84                          randu data.frame   400x3
#> 85                         rivers    numeric     141
#> 86                           rock data.frame    48x4
#> 87                          sleep data.frame    20x3
#> 88         stack.loss (stackloss)    numeric      21
#> 89            stack.x (stackloss)      array    21x3
#> 90                      stackloss data.frame    21x4
#> 91              state.abb (state)  character      50
#> 92             state.area (state)    numeric      50
#> 93           state.center (state)       list       2
#> 94         state.division (state)     factor      50
#> 95             state.name (state)  character      50
#> 96           state.region (state)     factor      50
#> 97              state.x77 (state)      array    50x8
#> 98  sunspot.m2014 (sunspot.month)         ts    3177
#> 99                  sunspot.month         ts    3310
#> 100                  sunspot.year         ts     289
#> 101                      sunspots         ts    2820
#> 102                         swiss data.frame    47x6
#> 103                      treering         ts    7980
#> 104                         trees data.frame    31x3
#> 105                         uspop         ts      19
#> 106                       volcano      array   87x61
#> 107                    warpbreaks data.frame    54x3
#> 108                         women data.frame    15x2
#>                                                  Title
#> 1          Monthly Airline Passenger Numbers 1949-1960
#> 2                    Sales Data with Leading Indicator
#> 3                    Sales Data with Leading Indicator
#> 4                            Biochemical Oxygen Demand
#> 5                Carbon Dioxide Uptake in Grass Plants
#> 6       Weight versus age of chicks on different diets
#> 7                                 Elisa assay of DNase
#> 8   Daily Closing Prices of Major European Stock Indic
#> 9                        Determination of Formaldehyde
#> 10           Hair and Eye Color of Statistics Students
#> 11                                  Harman Example 2.3
#> 12                                  Harman Example 7.4
#> 13                    Pharmacokinetics of Indomethacin
#> 14                      Effectiveness of Insect Sprays
#> 15      Quarterly Earnings per Johnson & Johnson Share
#> 16                       Level of Lake Huron 1875-1972
#> 17                Intercountry Life-Cycle Savings Data
#> 18                       Growth of Loblolly Pine Trees
#> 19                              Flow of the River Nile
#> 20                              Growth of Orange Trees
#> 21                           Potency of Orchard Sprays
#> 22          Results from an Experiment on Plant Growth
#> 23          Reaction Velocity of an Enzymatic Reaction
#> 24            Road Casualties in Great Britain 1969-84
#> 25                    Pharmacokinetics of Theophylline
#> 26               Survival of passengers on the Titanic
#> 27  The Effect of Vitamin C on Tooth Growth in Guinea 
#> 28                   Student Admissions at UC Berkeley
#> 29            Road Casualties in Great Britain 1969-84
#> 30                        UK Quarterly Gas Consumption
#> 31               Accidental Deaths in the US 1973-1978
#> 32                     Violent Crime Rates by US State
#> 33  Lawyers' Ratings of State Judges in the US Superio
#> 34                           Personal Expenditure Data
#> 35  Distances Between European Cities and Between US C
#> 36                      Death Rates in Virginia (1940)
#> 37                           Internet Usage per Minute
#> 38                              The World's Telephones
#> 39                      Ability and Intelligence Tests
#> 40  Passenger Miles on Commercial US Airlines, 1937-19
#> 41                   New York Air Quality Measurements
#> 42  Anscombe's Quartet of 'Identical' Simple Linear Re
#> 43                   The Joyner-Boore Attenuation Data
#> 44                  The Chatterjee-Price Attitude Data
#> 45  Quarterly Time Series of the Number of Australian 
#> 46              Body Temperature Series of Two Beavers
#> 47              Body Temperature Series of Two Beavers
#> 48                Speed and Stopping Distances of Cars
#> 49                        Chicken Weights by Feed Type
#> 50             Mauna Loa Atmospheric CO2 Concentration
#> 51                       Student's 3000 Criminals Data
#> 52             Yearly Numbers of Important Discoveries
#> 53           Smoking, Alcohol and (O)esophageal Cancer
#> 54                 Conversion Rates of Euro Currencies
#> 55                 Conversion Rates of Euro Currencies
#> 56  Distances Between European Cities and Between US C
#> 57                            Old Faithful Geyser Data
#> 58         Monthly Deaths from Lung Diseases in the UK
#> 59                               Freeny's Revenue Data
#> 60                               Freeny's Revenue Data
#> 61                               Freeny's Revenue Data
#> 62                    Hip and Knee Angle while Walking
#> 63  Infertility after Spontaneous and Induced Abortion
#> 64                          Edgar Anderson's Iris Data
#> 65                          Edgar Anderson's Iris Data
#> 66               Areas of the World's Major Landmasses
#> 67         Monthly Deaths from Lung Diseases in the UK
#> 68                Luteinizing Hormone in Blood Samples
#> 69                  Longley's Economic Regression Data
#> 70            Annual Canadian Lynx trappings 1821-1934
#> 71         Monthly Deaths from Lung Diseases in the UK
#> 72                       Michelson Speed of Light Data
#> 73                          Motor Trend Car Road Tests
#> 74            Average Yearly Temperatures in New Haven
#> 75  Average Monthly Temperatures at Nottingham, 1920-1
#> 76              Classical N, P, K Factorial Experiment
#> 77       Occupational Status of Fathers and their Sons
#> 78  Measurements of Penguins near Palmer Station, Anta
#> 79  Measurements of Penguins near Palmer Station, Anta
#> 80          Annual Precipitation in Selected US Cities
#> 81         Quarterly Approval Ratings of US Presidents
#> 82  Vapor Pressure of Mercury as a Function of Tempera
#> 83                   Locations of Earthquakes off Fiji
#> 84    Random Numbers from Congruential Generator RANDU
#> 85              Lengths of Major North American Rivers
#> 86              Measurements on Petroleum Rock Samples
#> 87                                Student's Sleep Data
#> 88                    Brownlee's Stack Loss Plant Data
#> 89                    Brownlee's Stack Loss Plant Data
#> 90                    Brownlee's Stack Loss Plant Data
#> 91                          US State Facts and Figures
#> 92                          US State Facts and Figures
#> 93                          US State Facts and Figures
#> 94                          US State Facts and Figures
#> 95                          US State Facts and Figures
#> 96                          US State Facts and Figures
#> 97                          US State Facts and Figures
#> 98        Monthly Sunspot Data, from 1749 to "Present"
#> 99        Monthly Sunspot Data, from 1749 to "Present"
#> 100                     Yearly Sunspot Data, 1700-1988
#> 101                 Monthly Sunspot Numbers, 1749-1983
#> 102 Swiss Fertility and Socioeconomic Indicators (1888
#> 103                  Yearly Tree-Ring Data, -6000-1979
#> 104 Diameter, Height and Volume for Black Cherry Trees
#> 105              Populations Recorded by the US Census
#> 106 Topographic Information on Auckland's Maunga Whau 
#> 107        The Number of Breaks in Yarn during Weaving
#> 108     Average Heights and Weights for American Women

# just list dataset names in a package
datasets("vcdExtra")[,"Item"]
#>  [1] "Abortion"       "Accident"       "AirCrash"       "Alligator"     
#>  [5] "Asbestos"       "Bartlett"       "Burt"           "Caesar"        
#>  [9] "Cancer"         "Cormorants"     "CrabSatellites" "CyclingDeaths" 
#> [13] "DaytonSurvey"   "Depends"        "Detergent"      "Donner"        
#> [17] "Draft1970"      "Draft1970table" "Dyke"           "Fungicide"     
#> [21] "GSS"            "Geissler"       "Gilby"          "Glass"         
#> [25] "HairEyePlace"   "Hauser79"       "Heart"          "Heckman"       
#> [29] "HospVisits"     "HouseTasks"     "Hoyt"           "ICU"           
#> [33] "JobSat"         "Mammograms"     "Mental"         "Mice"          
#> [37] "Mobility"       "PhdPubs"        "ShakeWords"     "TV"            
#> [41] "Titanicp"       "Toxaemia"       "Vietnam"        "Vote1980"      
#> [45] "WorkerSat"      "Yamaguchi87"   
datasets("vcd")[,"Item"]
#>  [1] "Arthritis"       "Baseball"        "BrokenMarriage"  "Bundesliga"     
#>  [5] "Bundestag2005"   "Butterfly"       "CoalMiners"      "DanishWelfare"  
#>  [9] "Employment"      "Federalist"      "Hitters"         "HorseKicks"     
#> [13] "Hospital"        "JobSatisfaction" "JointSports"     "Lifeboats"      
#> [17] "MSPatients"      "NonResponse"     "OvaryCancer"     "PreSex"         
#> [21] "Punishment"      "RepVict"         "Rochdale"        "Saxony"         
#> [25] "SexualFun"       "SpaceShuttle"    "Suicide"         "Trucks"         
#> [29] "UKSoccer"        "VisualAcuity"    "VonBort"         "WeldonDice"     
#> [33] "WomenQueue"     

```
