breed [ firms firm ]
breed [ applicants applicant ]

firms-own [ value aspiration tried left_potential potential fully_employed employee ]
applicants-own [ value aspiration tried left_potential potential employed employer ]

undirected-link-breed [ pairs pair ]
pairs-own [ employed_date ]

globals [ firm_ycor applicant_ycor firm_width applicant_width
  max_trial adjustment employed_applicants value_sum value_mean difference difference_mean more_match ]

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  ask patches [ set pcolor white ]

  let margin 1.01
  let ycor_offset 0.8

  set-default-shape firms "box"
  set firm_width (max-pxcor - min-pxcor) / ((number_of_firms - 1) * margin)
  set firm_ycor max-pycor * ycor_offset

  set-default-shape applicants "person"
  set applicant_width (max-pxcor - min-pxcor) / ((number_of_applicants - 1) * margin)
  set applicant_ycor min-pycor * ycor_offset

  set more_match false

  set employed_applicants 0
  set value_sum 0
  set difference 0
  set value_mean 0
  set difference_mean 0

  set max_trial number_of_firms * trial_period / 100
  set adjustment 50 / (1 + trial_period)

  let xpos min-pxcor / margin

  create-firms number_of_firms [
    set size 0.8

    set value random 100

    set xcor xpos
    set xpos xpos + firm_width
    set ycor firm_ycor

    set color blue

;  For firms mean variation, put a switch "firm_mixed" and a slider "mixed_range" on interface
;    ifelse firm_mixed = true
;   [ set aspiration value + (random mixed_range) * 2 - mixed_range
;    ]
;    [
      set aspiration value - firms_mean_x
;    ]

    set tried 0
    set potential number_of_applicants
    set fully_employed false
    set employee []
  ]

  set xpos min-pxcor / margin

  create-applicants number_of_applicants [
    set size 0.3

    set value random 100

    set xcor xpos
    set xpos xpos + applicant_width
    set ycor applicant_ycor

    set color pink

    ifelse decision_rule = "TNB" or decision_rule = "ADJUSTREL2INIT0"
    [ set aspiration 0 ]
    [ ifelse decision_rule = "MEAN"
      [ set aspiration value - 5 ]
      [ ifelse modest_applicant = true
          [ set aspiration value - modest_x
          ]
          [ set aspiration applicant_initial_aspiration
          ]
      ]
    ]

    set tried 0
    set potential number_of_firms
    set employed false
    set employer -1
  ]

  ask firms [
    ;;set left_potential n-values number_of_applicants [j -> j]
      set left_potential n-values number_of_applicants [ ?1 -> turtle (?1 + number_of_firms) ]
  ]

  ask applicants [
    set left_potential n-values number_of_firms [ ?1 -> turtle ?1 ]
  ]
end

to go
  ifelse ticks < max_trial [
    ask applicants [go_trial]
  ]
  [
    set more_match false

    let job_wanted applicants with [ (not employed) and potential > 0 ]
    if any? job_wanted
      [ ask job_wanted [go_match] ]

    if not more_match
    [
      ifelse (employed_applicants != 0)
      [ set value_mean value_sum / employed_applicants
        set difference_mean difference / employed_applicants
      ]
      [ set value_mean "N/A"
        set difference_mean "N/A"
      ]
      stop
    ]
  ]
  tick
end

to go_trial
  let recruiter nobody
  set recruiter one-of (turtle-set left_potential)
  if recruiter != nobody
 [
   ask recruiter [interview myself]
    set left_potential remove recruiter left_potential
   set potential potential - 1
    set tried tried + 1
  ]
end

;;to go_trial
 ;; let recruiter nobody
  ;;let valid_potential filter [p -> p != 0] left_potential
  ;;if any? valid_potential [
    ;;set recruiter one-of (turtle-set valid_potential)
  ;;]
  ;;if recruiter != nobody
  ;;[
   ;; ask recruiter [
    ;;  interview myself
     ;; set left_potential remove recruiter left_potential
    ;;]
    ;;set potential potential - 1
    ;;set tried tried + 1
  ;;]
;;end


to interview [ applicant1 ]
  let new_value 0

;  if show_applied = true
;  [  create-pair-with applicant ]

  set left_potential remove applicant1 left_potential
  set potential potential - 1
  set tried tried + 1

  ifelse ([decision_rule] of applicant1 = "TNB")
    [
      if ([aspiration] of applicant1 < value)
      [ ask applicant1 [learn value]]
    ]
  [
    ifelse ([decision_rule] of applicant1 = "MEAN")
    []
    [
      ifelse ([decision_rule] of applicant1 = "ADJUSTUD")
      [
        ifelse (aspiration < [value] of applicant1)
        [ set new_value [aspiration] of applicant1 + adjustment
          ask applicant1 [ learn new_value ]
        ]
        [ set new_value [aspiration] of applicant1 - adjustment
          ask applicant1 [ learn new_value ]
        ]
      ]
      [
        ifelse ([decision_rule] of applicant1 = "ADJUSTREL")
        [
          ifelse (aspiration < [value] of applicant1)
          [ if (value > [aspiration] of applicant1)
            [ set new_value [aspiration] of applicant1 + adjustment
              ask applicant1 [ learn new_value ]
            ]
          ]
          [ if (value < [aspiration] of applicant1)
            [ set new_value [aspiration] of applicant1 - adjustment
              ask applicant1 [ learn new_value ]
            ]
          ]
        ]
        [
          if ([decision_rule] of applicant1 = "ADJUSTREL2" or
            [decision_rule] of applicant1 = "ADJUSTREL2INIT0")
          [
            ifelse (aspiration < [value] of applicant1)
            [ if (value > [aspiration] of applicant1)
              [ set new_value [aspiration] of applicant1 + abs(value - [aspiration] of applicant1) / 2
                ask applicant1 [ learn new_value ]
              ]
            ]
            [ if (value < [aspiration] of applicant1)
              [ set new_value [aspiration] of applicant1 - abs(value - [aspiration] of applicant1) / 2
                ask applicant1 [ learn new_value ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
end

to learn [ new_value ]
  set aspiration new_value
end

to go_match
  let recruiter nobody
  set recruiter one-of ((turtle-set left_potential) with [ not fully_employed ])
  if recruiter != nobody
  [
    set more_match true
    ask recruiter [propose myself]
    set left_potential remove recruiter left_potential
    set potential potential - 1
    set tried tried + 1
  ]
end

to propose [ applicant1 ]
  set left_potential remove applicant1 left_potential
  set potential potential - 1
  set tried tried + 1

;  if show_applied = true
;  [  create-pair-with applicant ]

  if (aspiration < [value] of applicant1 and [aspiration] of applicant1 < value )
  [ get_employed applicant1
    ask applicant1 [get_employed myself]

    set employed_applicants employed_applicants + 1
    set value_sum value_sum + [value] of applicant1
    set difference difference + abs(value - [value] of applicant1)
  ]
end

to get_employed [ partner ]
  ifelse breed = firms
  [ set employee sentence employee partner
    if length employee >= number_of_employees
    [ set fully_employed true]
  ]
  [ set employed true
    set employer partner
  ]

  if  show_employed = true
  [
    ; ifelse show_applied = true
    ;[ ask pair ([who] of self) ([who] of partner) [set color black]]
    ;[
      create-pair-with partner [
      set color black
      set employed_date ticks
     ]
    ;]

  set color black
  ask partner [set color black]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
1843
448
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-62
62
-16
16
0
0
1
ticks
30.0

SLIDER
11
10
200
43
number_of_firms
number_of_firms
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
12
121
200
154
trial_period
trial_period
0
100
15.0
1
1
%
HORIZONTAL

BUTTON
19
156
85
189
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
300
457
443
502
NIL
employed_applicants
17
1
11

MONITOR
302
511
442
556
NIL
value_mean
1
1
11

MONITOR
303
566
441
611
NIL
difference_mean
1
1
11

SWITCH
15
199
171
232
show_employed
show_employed
0
1
-1000

SLIDER
11
47
198
80
number_of_employees
number_of_employees
0
10
10.0
1
1
NIL
HORIZONTAL

SLIDER
11
84
199
117
number_of_applicants
number_of_applicants
0
1000
1000.0
1
1
NIL
HORIZONTAL

BUTTON
104
157
167
190
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
19
402
176
447
decision_rule
decision_rule
"ADJUSTREL2" "ADJUSTREL2INIT0"
1

SLIDER
16
239
188
272
firms_mean_x
firms_mean_x
-10
10
10.0
1
1
NIL
HORIZONTAL

SLIDER
16
280
197
313
applicant_initial_aspiration
applicant_initial_aspiration
0
100
5.0
1
1
NIL
HORIZONTAL

SWITCH
17
319
183
352
modest_applicant
modest_applicant
0
1
-1000

SLIDER
18
360
190
393
modest_x
modest_x
-10
10
4.0
1
1
NIL
HORIZONTAL

PLOT
457
456
1100
742
Moukaila Plot - Simulation 2
Sampling Ratio (%)
Succ Applicants Employed 
0.0
100.0
0.0
1000.0
true
true
"" ""
PENS
"Applicants employed" 1.0 0 -2674135 true "plot employed_applicants" "plot employed_applicants"
"Value mean" 1.0 0 -14730904 true "" "plot value_mean"
"Difference" 1.0 0 -12087248 true "" "plot difference_mean"

@#$#@#$#@
Asymmetric two-sided matching

This model is an extended version of the matching problem including the mate search problem, which is the generalization of a traditional optimization problem. The matching
problem is extended to a form of asymmetric two-sided matching problem.

Peter Todd tried a simulation for two-sided matching problem in symmetric setting in 1999(*).  In his model there are the same number of agents in two parties, each of whom has his/her own mate value. Each agent in both parties tries to find his/her mate in the other party based on his/her candidate’s mate value and his/her own aspiration level for the partner’s mate values. Each agent learns his/her own mate value and adjusts his/her aspiration level through the trial period (adolescence). Todd tried a several search rules and the learning mechanisms.  The rules and the mechanisms are symmetric for both parties in this setting.

In this model his model is extended to the asymmetric setting where two parties have the different number of agents. Therefore, the search rule and the learning mechanism for two parties differ. Through the simulation, the search rules and the learning mechanisms which were identified to be appropriate in symmetric setting are revealed to be inappropriate in asymmetric setting. The reason why they are inappropriate is discussed.
 
(*) Peter M. Todd and Geoffery F. Miller, "From pride and prejudice to persuasion", in Gred Gigerenzer, Peter M. Todd and the ABC Research Group, Simple heuristics that makes us smart, Oxford University Press, New York, 1999
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment (var trial_period)" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>employed_applicants</metric>
    <metric>value_mean</metric>
    <metric>difference_mean</metric>
    <steppedValueSet variable="trial_period" first="0" step="1" last="90"/>
    <steppedValueSet variable="random-seed" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="experiment (var app_init_asp)" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>employed_applicants</metric>
    <metric>value_mean</metric>
    <metric>difference_mean</metric>
    <steppedValueSet variable="applicant_initial_aspiration" first="0" step="1" last="50"/>
    <steppedValueSet variable="random-seed" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="experiment (var modest_x)" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>employed_applicants</metric>
    <metric>value_mean</metric>
    <metric>difference_mean</metric>
    <steppedValueSet variable="modest_x" first="-3" step="1" last="10"/>
    <steppedValueSet variable="random-seed" first="0" step="1" last="9"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
