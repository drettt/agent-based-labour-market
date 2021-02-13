directed-link-breed [ job-links job-link ]


breed [ job-seekers job-seeker ]
breed [ firms firm ]
breed [ vacancies vacancy]
breed [ jsps jsp]

globals
[

  maximum-distance
  months-counter
  profit-margin
  subscription-fee
  ad-price
  platform?

]

job-seekers-own
[
  seeker-probability
  productivity
  visited-firms
  preferences
  skills
  unemployment-duration
  employment-duration
  long-term-unemployed?
  employed?
  searching?
  bargaining?
  seeker-wage
  real-wage
  leisure
  search-units
  unemployment-value
  employment-value
  on-the-job-search?
  on-the-job-bargaining?
  theta-js

  use-jsp?
]

firms-own
[
  production-level
  recruiting?
  firing?
  cost
  total-cost                       ;;;cost variable on firms

  profit
  total-profit
]

vacancies-own
[
  kind
  skill-level
  recruitment-cost
  wage-offer
  vacant-value
  filled-value
  is-free?
  recruiting-duration
  producing?
  productivity-component
  production
  firm-probability
  theta-v

  on-jsp?
]

jsps-own [
  income
  users
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SETUP


to setup

  clear-all

  if platform = "on" [
    set platform? TRUE]
  if platform = "off" [
    set platform? FALSE]

  setup-job-seekers
  setup-firms
  setup-vacancies

  if platform? = TRUE [
    setup-jsps
  ]
  general-setup

  reset-ticks
end

to setup-job-seekers
  set-default-shape job-seekers "person"
  create-job-seekers number_of_job_seekers
  ask job-seekers
  [
      set color green
      set size 1
      set unemployment-duration 0
      set employment-duration 0

      set employed? FALSE
      set searching? TRUE
      set bargaining? FALSE
      set long-term-unemployed? FALSE


      set on-the-job-search? FALSE
      set on-the-job-bargaining? FALSE
      set visited-firms [ ]

      set use-jsp? FALSE

      set skills one-of [ 1 2 3 4 5 ]
         let random-number random-float 1
            if random-number < 0.5 [
            set preferences ["services"] ]
            if random-number < 0.8 and random-number > 0.5 [
            set preferences ["production"] ]
            if random-number < 1 and random-number > 0.8 [
            set preferences ["agri"] ]

          if skills <= 2 [
                set productivity random-normal 2 0.2
                set leisure random-float 0.5 ]

          if skills = 3 or skills = 4 [
                set productivity random-normal 2.5 0.2
                set leisure random-float 0.5 ]

           if skills = 5 [
                set productivity random-normal 3 0.2
                set leisure random-float 0.5 ]
      ]

end

to setup-firms

  set-default-shape firms "house"
  create-firms  number_of_firms
  ask firms
  [
    set color [77 77 77]
    set size 1.5
    set recruiting? FALSE
    set firing? FALSE
    set profit 0
    set total-profit 0
    set cost 0
    set total-cost 0


  ]

end

to setup-vacancies


  let maximum 600
  let minimum 500
  ;create-vacancies minimum + random(maximum - minimum + 1)
  create-vacancies number_of_vacancies

  ask vacancies [
    set is-free? TRUE
    set recruiting-duration 0
    set producing? FALSE

    set on-jsp? FALSE

    let random-number random-float 1
    if random-number < 0.5
    [ set shape "circle 2"
      set color orange
      set size 0.7
      set kind ["services"]
      set skill-level one-of [ 1 2 3 4 5 ] ]
    if random-number < 0.8 and random-number > 0.5
    [ set shape "square 2"
      set color lime
      set size 0.6
      set kind ["production"]
      set skill-level one-of [ 1 2 3 4 5 ] ]
    if random-number < 1 and random-number > 0.8
    [ set shape "triangle 2"
      set color yellow
      set size 0.6
      set kind ["agri"]
      set skill-level one-of [ 1 2 3 4 5 ] ]



    ;;; separate each with ifs for on-jsp vacancies


   ;if on-jsp? = FALSE [

      if skill-level <= 2 [
        set recruitment-cost random-normal rc-cost 0.2 ]
      set wage-offer minimum-wage + random-float 1
      set productivity-component random-normal prd-comp 0.2

      if skill-level = 3 or skill-level = 4 [
        set recruitment-cost random-normal (rc-cost + 0.2) 0.2 ]
      set wage-offer 1.23 * minimum-wage + random-float 1
      set productivity-component random-normal (prd-comp + 0.5) 0.2

      if skill-level = 5 [
        set recruitment-cost random-normal (rc-cost + 0.4) 0.2 ]
      set wage-offer 1.68 * minimum-wage + random-float 1
      set productivity-component random-normal (prd-comp + 1) 0.2                      ;include 2 more detail here, add separate wage for each skill level
    ]
  ;]

end


to setup-jsps

  if platform? = TRUE [
    create-jsps 1
    ask jsps[
      setxy 0 0
      set shape "factory"
      set color white

      set subscription-fee 0.2
      set ad-price 0.4
    ]

  ]


end

to general-setup

  ask job-seekers
    [
      let empty-patches patches with [ not any? turtles-here] ;and not any? agencies-on neighbors]
      if any? turtles-here [ move-to one-of empty-patches ]
    ]

  ask firms
    [ let empty-patches patches with [ not any? turtles-here and not any? firms-on neighbors] ;and not any? agencies-on neighbors]
      move-to one-of empty-patches
    ]

  ask vacancies
  [
      let firms-patches patches with [ any? firms-here ]

         if not any? firms-here [ move-to one-of firms-patches ]

            while [count vacancies-here > 3] [ move-to one-of firms-patches ] ]
  ask patches [
    set pcolor black]


  set months-counter 0
  reset-ticks
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GO PROCEDURE

to go
  if ticks >= 208 [ stop ]  ;; 1 year of warp-up period and 12 years of simulation
  initialize-search-process
  create-jobs
  search-for-job
  if platform? = TRUE and ticks mod 10 = 0 [
    join-jsp2
  ]


  update-value-functions
  get-employed
  otjs
  set-wage-production-profit  ;; added profit
  destroy-vacancies
  update-cv
  tick
  join-leave
end


to initialize-search-process

  ask firms [
    let num-of-workers count job-seekers-here with [ employed? = TRUE ]
    let my-vacancies count vacancies-here with [is-free? = TRUE ]
    if my-vacancies > 0 and num-of-workers < my-vacancies
        [ set recruiting? TRUE ]

    if my-vacancies = 0 or num-of-workers >= my-vacancies
        [ set recruiting? FALSE ]

  ]


  ask job-seekers with [ searching? = TRUE ] [

    if long-term-unemployed? = TRUE and use-jsp? = FALSE
        [ set search-units random-normal base-search-units 2 - ltu-search-minus]

    if long-term-unemployed? = TRUE and use-jsp? = TRUE
        [ set search-units random-normal base-search-units 2 - ltu-search-minus + search-bonus]               ;ltu-search-minus

    if long-term-unemployed? = FALSE and use-jsp? = TRUE
      [ set search-units random-normal base-search-units 2 + search-bonus]

    if long-term-unemployed? = FALSE and use-jsp? = FALSE
        [ set search-units random-normal base-search-units 2]

    set maximum-distance max [search-units] of job-seekers
  ]


  ask job-seekers with [ searching? = TRUE ] [

    if search-units < 0 [
      set search-units 0]

    let mean-of-others-search-units mean [search-units] of job-seekers in-cone maximum-distance 360
    if mean-of-others-search-units > 0 [
      let num-rival-job-seekers count job-seekers in-cone mean-of-others-search-units 360 with [ searching? = TRUE and preferences = [preferences] of myself ]
      let num-on-the-job count job-seekers in-cone mean-of-others-search-units 360 with [ on-the-job-search? = TRUE and preferences = [preferences] of myself ]
      let my-free-jobs count vacancies in-cone search-units 360 with [ is-free? = TRUE and kind = [preferences] of myself ]

      if  num-rival-job-seekers > 0 [

        set theta-js ( (my-free-jobs) / (num-rival-job-seekers) )

        if long-term-unemployed? = TRUE [
          set seeker-probability ( 0.8 * efficiency * theta-js ^ (1 - 0.5) )]
        if long-term-unemployed? = FALSE [
          set seeker-probability ( efficiency * theta-js ^ (1 - 0.5) )]
      ]
    ]
  ]

  ask vacancies with [ is-free? = TRUE ] [

    if any? job-seekers in-cone maximum-distance 360 [

      let mean-of-others-search-units mean [search-units] of job-seekers in-cone maximum-distance 360

      let num-job-seekers count job-seekers in-cone mean-of-others-search-units 360 with [ searching? = TRUE and preferences = [kind] of myself and search-units > 0]
      let num-on-the-job count job-seekers in-cone mean-of-others-search-units 360 with [ on-the-job-search? = TRUE and preferences = [kind] of myself ]
      let rival-free-jobs count vacancies in-cone mean-of-others-search-units 360 with [ is-free? = TRUE and kind = [kind] of myself ]

      if  num-job-seekers > 0 and rival-free-jobs > 0 [

        set theta-v ( (rival-free-jobs) / (num-job-seekers) )

        set firm-probability ( efficiency * 1 / ( theta-v ^ 0.5) ) ]
    ]
  ]
end


to create-jobs

  ask firms [

    let searchers job-seekers with [ searching? = TRUE or on-the-job-search? = TRUE]
    let my-vacancies vacancies-here

    if any? searchers and count my-vacancies < max_vacancies and any? vacancies-here and any? job-seekers in-cone maximum-distance 360 [


      let mean-of-others-search-units mean [search-units] of job-seekers in-cone maximum-distance 360

      let num-job-seekers count job-seekers in-cone mean-of-others-search-units 360 with [ searching? = TRUE]
      let num-on-the-job count job-seekers in-cone mean-of-others-search-units 360 with [ on-the-job-search? = TRUE ]
      let other-firms count firms in-cone mean-of-others-search-units 360

      if  num-job-seekers > 0 and other-firms > 0 [

        let t ( (other-firms) / (num-job-seekers) )
        let matching-prob ( efficiency * 1 / ( t ^ 0.5) )

        let my-searchers job-seekers in-cone maximum-distance 360 with [ searching? = TRUE ]
        let my-vacs vacancies-here
        let p max [productivity] of my-searchers
        let c  mean [recruitment-cost] of my-vacs
        let w  mean [wage-offer] of my-vacs
        let x max [productivity-component] of my-vacs

        let expected-profit (p * x - w)
        let expected-cost c * 1 / matching-prob


        if expected-profit >= 0.9 * expected-cost [

          let my-patch patch-here
          ask my-patch [ sprout-vacancies 1 [ set is-free? TRUE set recruiting-duration 0 set producing? FALSE
            let random-number random-float 1
            if random-number < 0.5
              [ set shape "circle 2"
                set color orange
                set size 0.7
                set kind ["services"]
                set skill-level one-of [ 1 2 3 4 5 ] ]
            if random-number < 0.8 and random-number > 0.5
              [ set shape "square 2"
                set color lime
                set size 0.6
                set kind ["production"]
                set skill-level one-of [ 1 2 3 4 5 ] ]
            if random-number < 1 and random-number > 0.8
              [ set shape "triangle 2"
                set color yellow
                set size 0.6
                set kind ["agri"]
                set skill-level one-of [ 1 2 3 4 5 ] ]
            if skill-level <= 2 [
              set recruitment-cost random-normal (rc-cost) 0.2 ]
            set wage-offer minimum-wage + random-float 0.5
            set productivity-component random-normal prd-comp 0.2
            if skill-level = 3 or skill-level = 4 [
              set recruitment-cost random-normal (rc-cost + 0.2) 0.2 ]
            set wage-offer minimum-wage + random-float 1
            set productivity-component random-normal (prd-comp + 0.5) 0.2
            if skill-level = 5 [
              set recruitment-cost random-normal (rc-cost + 0.4) 0.2 ]
            set wage-offer minimum-wage + random-float 1.5
            set productivity-component random-normal  (prd-comp + 1) 0.2 ]  ]
        ]
      ]
    ]

  ]

 ask firms [

    let searchers job-seekers with [ searching? = TRUE or on-the-job-search? = TRUE ]
    if not any? vacancies-here and any? vacancies in-cone maximum-distance 360 and any? searchers and any? job-seekers in-cone maximum-distance 360 [

      let mean-of-others-search-units mean [search-units] of job-seekers in-cone maximum-distance 360

      let num-job-seekers count job-seekers in-cone mean-of-others-search-units 360 with [ searching? = TRUE]
      let num-on-the-job count job-seekers in-cone mean-of-others-search-units 360 with [ on-the-job-search? = TRUE ]
      let other-firms count firms in-cone mean-of-others-search-units 360

      if  num-job-seekers > 0 and other-firms > 0 [

        let t ( (other-firms) / (num-job-seekers) )
        let matching-prob ( efficiency * 1 / ( t ^ 0.5) )

        let my-searchers job-seekers in-cone mean-of-others-search-units 360 with [ searching? = TRUE]

        let p max [productivity] of my-searchers
        let c  mean [recruitment-cost] of vacancies in-cone maximum-distance 360
        let w  mean [wage-offer] of vacancies in-cone maximum-distance  360
        let x max [productivity-component] of vacancies in-cone maximum-distance 360

        let expected-profit (p * x - w)
        let expected-cost c * 1 / matching-prob


        if expected-profit >= 0.9 * expected-cost [

          let my-patch patch-here
          ask my-patch [ sprout-vacancies 1 [ set is-free? TRUE set recruiting-duration 0 set producing? FALSE
            let random-number random-float 1
            if random-number < 0.5
              [ set shape "circle 2"
                set color orange
                set size 0.7
                set kind ["services"]
                set skill-level one-of [ 1 2 3 4 5 ] ]
            if random-number < 0.8 and random-number > 0.5
              [ set shape "square 2"
                set color lime
                set size 0.6
                set kind ["production"]
                set skill-level one-of [ 1 2 3 4 5 ] ]
            if random-number < 1 and random-number > 0.8
              [ set shape "triangle 2"
                set color yellow
                set size 0.6
                set kind ["agri"]
                set skill-level one-of [ 1 2 3 4 5 ] ]
            if skill-level <= 2 [
              set recruitment-cost random-normal rc-cost 0.2 ]
            set wage-offer minimum-wage + random-float 1
            set productivity-component random-normal prd-comp 0.2
            if skill-level = 3 or skill-level = 4 [
              set recruitment-cost random-normal (rc-cost + 0.2) 0.2 ]
            set wage-offer 1.23 * minimum-wage + random-float 1
            set productivity-component random-normal (prd-comp + 0.5) 0.2
            if skill-level = 5 [
              set recruitment-cost random-normal (rc-cost + 0.4) 0.2 ]
            set wage-offer 1.68 * minimum-wage + random-float 1
            set productivity-component random-normal (prd-comp + 1) 0.2 ]  ]
        ]
      ]
    ]

  ]

end


to search-for-job

  ask job-seekers with [ searching? = TRUE  ] [

    while [ search-units > 0 ] [

      let remaining-firms other vacancies-on patches in-cone maximum-distance 360 with [ not member? self [visited-firms] of myself ]
      let free-remaining-firms remaining-firms with [is-free? = TRUE]

      if not any? free-remaining-firms [set visited-firms [ ] set search-units 0 ]

      if any? free-remaining-firms[
        face min-one-of free-remaining-firms [distance myself]

        forward 1
        set search-units (search-units - 1)]

      set search-units (search-units - 1)]

    if any? vacancies-here

    [ set visited-firms (patch-set patch-here visited-firms)

      let my-jobs vacancies-here with [ is-free? = TRUE and kind = [preferences] of myself and skill-level <= [skills] of myself ]


      if any? my-jobs

      [

        create-job-link-to max-one-of my-jobs [ skill-level ]

        set bargaining? TRUE
        set searching? FALSE
        set search-units 0

        ;;;this is an issue here, total costs cannot be calculated because use-jsp is already false when that function gets called so need to
        ;;;position this somewhere else
        ;set use-jsp? FALSE

        ask my-jobs [ if any? my-in-job-links [ set is-free? FALSE ] ]

      ]


        ]
      ]



ask job-seekers [ if search-units < 0 [set search-units 0]]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;WAGE BARGAINING


;;calculate employment and unemployment values
;;for each agent which will be used later

to update-value-functions

  ask job-seekers [
    if bargaining? = TRUE [

      let my-future-jobs other vacancies in-cone maximum-distance 360 with [ is-free? = TRUE and kind = [preferences] of myself and skill-level <= [skills] of myself ]
      let my-curent-job out-job-link-neighbors
      let my-job-skills first [skill-level] of my-curent-job
      let current-payoff first [wage-offer] of my-curent-job
      if any? my-future-jobs
        [ let future-payoff mean [wage-offer] of my-future-jobs                                  ;use mean wage offer
          set unemployment-value (benefits + leisure + (seeker-probability * future-payoff) )

           ifelse my-job-skills = skills
               [ set employment-value (current-payoff - shock * (  current-payoff - unemployment-value ) ) ]
               [ set employment-value (current-payoff - shock * (  current-payoff - unemployment-value ) + (seeker-probability * future-payoff)) ] ]

      if not any? my-future-jobs  [ set unemployment-value (benefits + leisure)
                                    set employment-value current-payoff - shock * (  current-payoff - unemployment-value ) ]

    ]
  ]

;calculate value in vacancy and filled positions
  ask vacancies [
    if any? my-in-job-links
    [
      let my-future-seekers job-seekers in-cone maximum-distance 360 with [searching? = TRUE and preferences = [kind] of myself and skills >= [skill-level] of myself ]
      let my-current-seeker in-job-link-neighbors
      let my-seeker-skills [skills] of my-current-seeker
      let my-current-payoff first [productivity] of my-current-seeker * productivity-component

      if any? my-future-seekers
         [ let future-payoff mean [productivity] of my-future-seekers * productivity-component        ;use mean instead of max, as it better represent expectations
           set vacant-value ( - recruitment-cost + firm-probability * future-payoff)

           ifelse my-seeker-skills = skill-level
            [ set filled-value ( my-current-payoff - wage-offer - shock * ( my-current-payoff - vacant-value )) ]
            [ set filled-value ( my-current-payoff - wage-offer - shock * ( my-current-payoff - vacant-value ) + firm-probability * future-payoff ) ] ]

      if not any? my-future-seekers [ set vacant-value (- recruitment-cost)
                                      set filled-value ( my-current-payoff - wage-offer - shock * ( my-current-payoff - vacant-value )) ]


      ]



    if not any? my-in-job-links [set is-free? TRUE]

  ]

end




to get-employed
;;;first firms look at how many applicants used jsps, and they add the ad-price to the costs

  ask firms [
    let vacs-here vacancies-here
    if any? job-seekers-here with [ bargaining? = TRUE and use-jsp? = TRUE] [

      let jsp-js-here count job-seekers-here with [ bargaining? = TRUE and use-jsp? = TRUE]



      let costs-from-ads ad-price * (jsp-js-here)

      if costs-from-ads > profit [ ]    ; decided on this

    ]
  ]

  ask job-seekers [

    if bargaining? = TRUE and unemployment-value > employment-value                               ;job-seekers decision;
    [ ask my-out-job-links [die] set bargaining? FALSE set searching? TRUE]

    if bargaining? = TRUE and unemployment-value <= employment-value [
      let my-vacancy out-job-link-neighbors
      let V first [vacant-value] of my-vacancy
      let F first [filled-value] of my-vacancy
      ifelse V > F
      [ ask my-out-job-links [die] set bargaining? FALSE set searching? TRUE ]                ;firms decision
      [ set employed? TRUE set bargaining? FALSE set use-jsp? FALSE]


     ]
  ]

  ask vacancies [ if not any? my-in-job-links [ set is-free? TRUE ] ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;; On-the-job search procedure
to otjs
 ;set otjs true
 ask job-seekers [
   if employed? = TRUE and any? my-out-job-links
    [
       let me self
         ask me [ let my-job out-job-link-neighbors

                    let z [skill-level] of my-job
                    if first z < skills - 1  [ set on-the-job-search? TRUE ] ]
        ]
 ]

 ask job-seekers [
   if on-the-job-search? = TRUE and any? my-out-job-links
   [
      let search-on-the-job? random-float 1
       if search-on-the-job? < 0.1 [

           let my-job out-job-link-neighbors
           let my-job-skill first [skill-level] of my-job
           let targets vacancies in-cone maximum-distance 360 with [ is-free? = TRUE and skill-level > my-job-skill
                                                       and skill-level <= [skills] of myself ]

            if any? targets [

              let future-job one-of targets
              let F [ filled-value ] of future-job
              let V [ vacant-value ] of future-job

               if F > V and employment-value > unemployment-value [ ask my-out-job-links [ die ]
                                                                    move-to future-job
                                                                    create-job-link-to future-job
                                                                    set color white
                                                                    ask future-job [set is-free? FALSE ] ]
            ]
      ]

   ]

 ]

 ask vacancies [ if not any? my-in-job-links [ set is-free? TRUE ] ]

end

;;;this mechanism allows job seekers to randomly join jsp -> whihc controls for issues like selection bias
;;; we can compare the overall steady states of the two situations
to join-jsp2
  ask job-seekers [
    let thresh random-float 1

    if thresh < 0.4 [
      set use-jsp? TRUE
    ]
    if thresh >= 0.4 [
      set use-jsp? FALSE
    ]
  ]


end



to join-jsp

  ask job-seekers [

    if real-wage > 2 [

      if searching? = TRUE and bargaining? = FALSE and long-term-unemployed? = FALSE [

        let my-future-jobs vacancies in-cone maximum-distance 360 with [ is-free? = TRUE and kind = [preferences] of myself and skill-level <= [skills] of myself ]
        let my-job-skills skills

        if any? my-future-jobs [

          let future-payoff mean [wage-offer] of my-future-jobs

          set unemployment-value (benefits + leisure)                         ;set benefits to make these equations nice results
          set employment-value (seeker-probability * future-payoff)           ;maybe include how many future jobs there are, the more the higher the likelyhood of getting hired
                                                                              ;;                      investigate how seeker probability works for
                                                                              ;;                       unem and ltu-unem js, and how many separations we need
          if employment-value - subscription-fee > 0.7 * unemployment-value [
            set use-jsp? TRUE
          ]
        ]
      ]


      if searching? = TRUE and bargaining? = FALSE and long-term-unemployed? = TRUE [

        let my-future-jobs vacancies in-cone maximum-distance 360 with [ is-free? = TRUE and kind = [preferences] of myself and skill-level <= [skills] of myself ]
        let my-job-skills skills

        if any? my-future-jobs [

          let future-payoff mean [wage-offer] of my-future-jobs

          set unemployment-value (benefits + leisure)



          set employment-value (0.8 * seeker-probability * future-payoff)


          ;;;; why 0.7
          if employment-value - subscription-fee > 0.7 * unemployment-value [

            set use-jsp? TRUE
      ]]]

      if on-the-job-search? = TRUE [

        let my-future-jobs other vacancies in-cone maximum-distance 360 with [ is-free? = TRUE and kind = [preferences] of myself and skill-level <= [skills] of myself ]
        let my-curent-job out-job-link-neighbors
        let my-job-skills first [skill-level] of my-curent-job
        let current-payoff first [wage-offer] of my-curent-job
        if any? my-future-jobs
        [ let future-payoff mean [wage-offer] of my-future-jobs                                  ;use mean wage offer
          set unemployment-value (benefits + leisure)

          ifelse my-job-skills < skills
              [ set employment-value (current-payoff - shock * (  current-payoff - unemployment-value ) ) ]
          [ set employment-value (current-payoff - shock * (  current-payoff - unemployment-value ) + (seeker-probability * future-payoff)) ] ]

        if not any? my-future-jobs  [ set unemployment-value (benefits + leisure)
          set employment-value current-payoff - shock * (  current-payoff - unemployment-value ) ]
        if employment-value - subscription-fee > 0.7 * unemployment-value [

          set use-jsp? TRUE]

      ]

      set real-wage real-wage - 1
      ask jsps [
        set income income + 1 ]
    ]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;update production and wage and profit and costs
to set-wage-production-profit

  ask vacancies [
    if any? my-in-job-links [                               ;; someone is in the job



      let my-worker in-job-link-neighbors
      let p0 first [productivity] of my-worker
      let low-limit -0.10 - 0.8 * growth-rate
      let hi-limit 0.10 + growth-rate
      let stochastic low-limit + random-float ( hi-limit - low-limit)



      set production ((p0 + p0 * stochastic) * productivity-component)                        ;set production for each vacancy ; how's this related to the firms
      ask my-worker [set productivity p0 + p0 * stochastic]
    ]
  ]

  ask job-seekers [
    if employed? = TRUE [

        let beta 0.5
        let my-job out-job-link-neighbors
        let px first [production] of my-job
        let V first [vacant-value] of my-job
        set real-wage ( (1 - beta ) * unemployment-value + beta * px ) ]
        ]



  ask firms [
    if any? vacancies-here with [ is-free? = FALSE ] [

      if any? job-seekers-here with [ employed? = TRUE] [

        let prod sum [production] of vacancies-here with [ is-free? = FALSE ]
        let workers job-seekers-here with [ employed? = TRUE ]

        set production-level ( ( prod / count workers ) )

        set profit-margin 0.1

        set profit prod * profit-margin                     ;profit mechanism for firms

        set total-profit total-profit + profit
      ]

      if any? job-seekers-here with [use-jsp? = TRUE and employed? = TRUE][

        let num-rec-with-jsp-js count job-seekers-here with [use-jsp? = TRUE and employed? = TRUE ]
        let recruit-cost num-rec-with-jsp-js * ad-price

        set cost recruit-cost
        set total-cost total-cost + cost
      ]
    ]





      if not any? job-seekers-here with [ employed? = TRUE ] [ set production-level 0 ]

  ]

end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   DESTROY JOBS



;job destruction rate based on the shock
to destroy-vacancies
  ask job-seekers with [ employed? = TRUE and employment-duration > 6 ]

  [

    let random-number random-float 1

    if random-number < shock [

      let me self
      ask me [

        let destroyed-vacancy out-job-link-neighbors

        set employed? FALSE
        set searching? TRUE
        set long-term-unemployed? FALSE
        set unemployment-duration 0
        set employment-duration 0
        set on-the-job-search? FALSE
        set color red
        if productivity < 0.8 [ set productivity 0.8 ]
        ask my-out-links [die]
        ask destroyed-vacancy [die]

      ]
    ]
  ]

  ask job-seekers with [ employed? = TRUE and employment-duration < 6 ]

  [

    let random-number random-float 1

    if random-number < shock [

      let me self
      ask me [

        let destroyed-vacancy out-job-link-neighbors

        set employed? FALSE
        set searching? TRUE
        set unemployment-duration 0
        set employment-duration 0
        set on-the-job-search? FALSE
        set color yellow
        if productivity < 0.8 [ set productivity 0.8 ]

        ask my-out-links [die]
        ask destroyed-vacancy [die]
      ]

    ]

  ]

  ;skill requirement depreciation to creat more dynamic adjustments
  ask vacancies with [is-free? = TRUE] [
    set recruitment-cost (recruitment-cost * recruiting-duration)
    set recruiting-duration recruiting-duration + 1
    let random-variable random-float 1
    if random-variable < 0.05 and skill-level > 1

      [ set skill-level (skill-level - 1)
        if skill-level <= 2 [
          set wage-offer minimum-wage + random-float 0.5
          set productivity-component random-normal prd-comp 0.2 ]

        if skill-level = 3 or skill-level = 4 [
          set wage-offer minimum-wage + random-float 1
          set productivity-component random-normal (prd-comp + 0.5) 0.2 ]
    ]

    if recruiting-duration > 6                                    ;destroy vacancy if not recruited after 6 periods
      [
        let me self
        ask me [ let destroy? random-float 1
          if destroy? < 0.1 [ ask me [die] ]
        ]
    ]
  ]

  ask vacancies with [is-free? = FALSE] [
    set recruiting-duration 0 ]


end

;test if I change the time from months to weeks
to update-cv

  set months-counter (months-counter + 1)

  ask job-seekers [
    if searching? = True
        [ set unemployment-duration (unemployment-duration + 1)

          if unemployment-duration > 26 [ set color green
            set long-term-unemployed? TRUE ]               ;ltu mechanism



          if long-term-unemployed? = TRUE and productivity > 0.7
              [ set productivity (productivity - (0.5 * growth-rate * productivity) )
          ]
    ]

    if employed? = TRUE [
      set employment-duration (employment-duration + 1)
      set unemployment-duration 0
      if skills < 5 [
        let random-number random-float 1
        let skill-upgrade-probability 0.01

        if random-number < skill-upgrade-probability [ set skills skills + 1 ] ]       ;emplyed job-seekers upgrade skills

    ]


    if long-term-unemployed? = TRUE and skills > 1 [
      let random-number random-float 1
      let depreciation-probability 0.01

      if random-number < depreciation-probability [ set skills skills - 1]               ;ltu unemployed job-seeker degrade skills

    ]

    if long-term-unemployed? = FALSE and skills > 1 [
      let random-number random-float 1
      let depreciacion-probability 0.01

      if random-number < depreciacion-probability [ set skills skills - 1]
    ]

  ]
  ask jsps [
    set users count job-seekers with [use-jsp? = TRUE]
  ]

ask job-seekers with [ employed? = FALSE ] [

    if months-counter mod 6 = 0 [

      let serv count vacancies in-cone maximum-distance 360 with [ kind = ["services"] and is-free? = TRUE  ]
      let prod count vacancies in-cone maximum-distance 360 with [ kind = ["production"] and is-free? = TRUE ]
      let agri count vacancies in-cone maximum-distance 360 with [ kind = ["agri"] and is-free? = TRUE ]


      if serv > prod and serv > agri [ set preferences ["services"]]
      if prod > serv and prod > agri [ set preferences ["production"]]
      if agri > serv and agri > prod [ set preferences ["agri"]]
    ]
  ]

end



;;;add new function to model labour force leaving and joing --> should look at UK rates


to join-leave

  ask job-seekers with [ bargaining? = FALSE and employed? = FALSE ][
    ;;;leaving mechanism, with x probability they leave the work force

    if random-float 1 < 0.01 [

      die

    ]
  ]

  if random-float 1 < 0.1 [
    create-job-seekers 10
  ]

   ask job-seekers with [xcor = 0 and ycor = 0] [


    set color green
    set size 1
    set unemployment-duration 0
    set employment-duration 0

    set employed? FALSE
    set searching? TRUE
    set bargaining? FALSE
    set long-term-unemployed? FALSE


    set on-the-job-search? FALSE
    set on-the-job-bargaining? FALSE
    set visited-firms [ ]

    set use-jsp? FALSE

    set skills one-of [ 1 2 3 4 5 ]
    let random-number random-float 1
    if random-number < 0.5 [
      set preferences ["services"] ]
    if random-number < 0.8 and random-number > 0.5 [
      set preferences ["production"] ]
    if random-number < 1 and random-number > 0.8 [
      set preferences ["agri"] ]

    if skills <= 2 [
      set productivity random-normal 2 0.2
      set leisure random-float 0.5 ]

    if skills = 3 or skills = 4 [
      set productivity random-normal 2.5 0.2
      set leisure random-float 0.5 ]

    if skills = 5 [
      set productivity random-normal 3 0.2
      set leisure random-float 0.5 ]

    setxy random-xcor random-ycor
  ]


end

;;;list of reporters
;unem-rate
;otj
;real-stu-rate
;stu  => non-ltu
;theta
;ltu-prob
;js-prob
;dur
;ltu-dur
;stu-dur
;mismatch



to-report unem-rate
  let unemployed count job-seekers with [ searching? = TRUE ]
  let population count job-seekers
  report ( unemployed / population )
end

to-report unem
   report count job-seekers with [ searching? = TRUE ]
end

to-report otj
  report count job-seekers with [on-the-job-search? = TRUE]
end

to-report emp-num
  report count job-seekers with [employed? = TRUE]
end

to-report ltu-rate
  let ltu count job-seekers with [ long-term-unemployed? = TRUE and searching? = TRUE ]
  let unemployed count job-seekers with [ searching? = TRUE and employed? = FALSE ]
  ifelse unemployed > 0
    [ report ( ltu / count job-seekers ) ]
    [ report 0 ]
end
to-report stu-rate
  let stu count job-seekers with [ long-term-unemployed? = FALSE and searching? = TRUE ]
  let unemployed count job-seekers with [ searching? = TRUE and employed? = FALSE ]
  ifelse unemployed > 0
    [ report ( stu / unemployed ) ]
    [ report 0 ]
end
to-report real-stu-rate
  let stu count job-seekers with [ unemployment-duration <= 6 and searching? = TRUE ]
  let unemployed count job-seekers with [ searching? = TRUE and employed? = FALSE ]
  ifelse unemployed > 0
    [ report ( stu / unemployed ) ]
    [ report 0 ]
end


;;;I took out otj searchers from the theta
to-report theta
  let js count job-seekers with [searching? = TRUE]
  let ojs count job-seekers with [on-the-job-search? = TRUE]
  let v count vacancies with [is-free? = TRUE]

  report (v / (js + otj) )
end

to-report ltu-prob
  let ltus job-seekers with [long-term-unemployed? = TRUE and searching? = TRUE]
  ifelse any? ltus
  [ report mean [seeker-probability] of ltus ]
  [ report 0 ]
end



 to-report js-prob
  let js job-seekers with [long-term-unemployed? = FALSE and searching? = TRUE]
  ifelse any? js
  [ report mean [seeker-probability] of js]
  [ report 0 ]
end
;productivity of job-seekers non-ltu
to-report prd
  let js job-seekers with [long-term-unemployed? = FALSE and employed? = TRUE]
  ifelse any? js
  [ report mean [productivity] of js]
  [ report 0 ]
end
;productivity of ltu job-seekers
to-report ltu-prd
  let js job-seekers with [long-term-unemployed? = TRUE and employed? = TRUE]
  ifelse any? js
  [ report mean [productivity] of js]
  [ report 0 ]
end
to-report js-prd
  let js job-seekers with [searching? = TRUE]
  ifelse any? js
  [ report mean [productivity] of js]
  [ report 0 ]
end

; report mean wage
to-report wage
  let js job-seekers with [employed? = TRUE]
  ifelse any? js
  [ report mean [real-wage] of js]
  [ report 0 ]
end

to-report count_vacancies
  ifelse any? vacancies
  [ report count vacancies]
  [ report 0]
end


to-report agr-jobs
  let vacs vacancies with [kind = ["agri"]]
  ifelse any? vacs
  [ report count vacs ]
  [ report 0 ]
end

to-report serv-jobs
  let vacs vacancies with [kind = ["services"]]
  ifelse any? vacs
  [ report count vacs ]
  [ report 0 ]
end

to-report prod-jobs
  let vacs vacancies with [kind = ["production"]]
  ifelse any? vacs
  [ report count vacs ]
  [ report 0 ]
end
;low skill vacancies
to-report ls-vacs
  let vacs vacancies with [skill-level = 1 ]
  ifelse any? vacs
  [ report count vacs ]
  [ report 0 ]
end
;medium skill vacancies
to-report ms-vacs
  let vacs vacancies with [skill-level < 4 and skill-level >= 2 ]
  ifelse any? vacs
  [ report count vacs ]
  [ report 0 ]
end
;high skill vacancies
to-report hs-vacs
  let hvacs vacancies with [skill-level >= 4]
  ifelse any? hvacs
  [ report count hvacs ]
  [ report 0 ]
end
;mean duration of non-ltu job-seekers searching ==>capped at 12
to-report dur_non_ltu
  let js job-seekers with [searching? = TRUE and long-term-unemployed? = FALSE ]
  ifelse any? js
  [report mean [unemployment-duration] of js]
  [report 0]
end

to-report dur
  let js job-seekers with [searching? = TRUE]
  ifelse any? js
  [report mean [unemployment-duration] of js]
  [report 0]
end


to-report users.dur
  let js job-seekers with [searching? = TRUE and use-jsp? = TRUE]
  ifelse any? js
  [report mean [unemployment-duration] of js]
  [report 0]
end


to-report non.users.dur
  let js job-seekers with [searching? = TRUE and use-jsp? = FALSE]
  ifelse any? js
  [report mean [unemployment-duration] of js]
  [report 0]
end


to-report stu-dur
  let js job-seekers with [searching? = TRUE and unemployment-duration <= 13]
  ifelse any? js
  [report mean [unemployment-duration] of js]
  [report 0]
end

;mean duration of ltu job-seekers searching
to-report ltu-dur
  let js job-seekers with [searching? = TRUE and long-term-unemployed? = TRUE ]
  ifelse any? js
  [report mean [unemployment-duration] of js]
  [report 0]
end

to-report mismatch
  let jobs count vacancies with [is-free? = FALSE]
  let ot count job-seekers with [on-the-job-search? = TRUE]
  ifelse jobs > 0
  [report ot / jobs]
  [report ot ]
end

to-report jsp-rate
  report count job-seekers with [use-jsp? = TRUE]/ count job-seekers
end

to-report time
  report months-counter
end
@#$#@#$#@
GRAPHICS-WINDOW
271
10
902
642
-1
-1
15.2
1
10
1
1
1
0
0
0
1
-20
20
-20
20
1
1
1
ticks
30.0

BUTTON
111
127
188
160
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

BUTTON
14
127
100
160
go
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

MONITOR
144
12
195
57
theta
theta
17
1
11

SLIDER
917
277
1009
310
shock
shock
0
0.04
0.003
0.001
1
NIL
HORIZONTAL

MONITOR
208
12
265
57
u-rate
unem-rate
17
1
11

SLIDER
1040
101
1134
134
benefits
benefits
0.3
1.2
0.8
0.01
1
NIL
HORIZONTAL

PLOT
0
426
247
546
rates
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"u-rate" 1.0 0 -13345367 true "" "plot unem-rate"
"ltu-rate" 1.0 0 -14439633 true "" "plot ltu-rate"

MONITOR
209
62
267
107
NIL
ltu-rate
17
1
11

MONITOR
12
61
62
106
jobs
count vacancies
17
1
11

SLIDER
906
63
1030
96
search-bonus
search-bonus
0
8
5.0
1
1
NIL
HORIZONTAL

SLIDER
906
139
1036
172
ltu-search-minus
ltu-search-minus
0
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
910
23
1048
56
minimum-wage
minimum-wage
0.5
1.5
0.8
0.1
1
NIL
HORIZONTAL

SLIDER
1011
311
1149
344
efficiency
efficiency
0.1
0.5
0.23
0.01
1
NIL
HORIZONTAL

PLOT
446
687
688
807
job seekers and vacancies
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"employed" 1.0 0 -16777216 true "" "plot count job-seekers with [employed? = TRUE]"
"otj" 1.0 0 -15040220 true "" "plot count job-seekers with [on-the-job-search? = TRUE]"

SLIDER
1017
348
1143
381
growth-rate
growth-rate
0
0.05
0.02
0.01
1
NIL
HORIZONTAL

PLOT
701
690
925
810
duration
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"dur" 1.0 0 -16777216 true "" "plot dur"
"ltu-dur" 1.0 0 -15040220 true "" "plot ltu-dur"

PLOT
0
247
265
386
plot 1
f
ticks
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"vacancies" 1.0 0 -16777216 true "" "plot count vacancies with [is-free? = TRUE]"
"unemployed" 1.0 0 -5298144 true "" "plot count job-seekers with [employed? = false]"

PLOT
15
653
436
803
plot 2
NIL
NIL
0.0
156.0
0.0
1.0
true
true
"" ""
PENS
"long-term-unem rate" 1.0 0 -14439633 true "" "plot (count job-seekers with [ searching? = true and long-term-unemployed? = true]) / (count job-seekers with [searching? = true])"
"short-term-unem rate" 1.0 0 -14070903 true "" "plot (count job-seekers with [ searching? = true and long-term-unemployed? = false]) / (count job-seekers with [searching? = true])"

PLOT
933
665
1133
815
mean wage
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot wage"

MONITOR
158
568
246
613
jsps rate
count job-seekers with [use-jsp? = TRUE]/ count job-seekers
17
1
11

CHOOSER
55
182
193
227
platform
platform
"on" "off"
0

MONITOR
9
12
128
57
NIL
count job-seekers
17
1
11

SLIDER
1000
171
1172
204
base-search-units
base-search-units
0
10
6.0
1
1
NIL
HORIZONTAL

SLIDER
905
206
1077
239
rc-cost
rc-cost
0
2
0.8
0.1
1
NIL
HORIZONTAL

SLIDER
911
243
1083
276
prd-comp
prd-comp
0
2
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
912
444
1114
477
number_of_job_seekers
number_of_job_seekers
500
700
600.0
25
1
NIL
HORIZONTAL

SLIDER
912
478
1084
511
number_of_firms
number_of_firms
190
310
230.0
20
1
NIL
HORIZONTAL

SLIDER
913
511
1101
544
number_of_vacancies
number_of_vacancies
500
700
550.0
25
1
NIL
HORIZONTAL

SLIDER
905
390
1077
423
max_vacancies
max_vacancies
0
10
3.0
1
1
NIL
HORIZONTAL

MONITOR
114
69
171
114
NIL
dur
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ltu-rate</metric>
    <metric>unemployment-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>wage</metric>
    <enumeratedValueSet variable="shock">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ltu-search-bonus">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.84"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.03"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp1 for pres w/jsp" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>unem</metric>
    <metric>otj</metric>
    <metric>emp-num</metric>
    <metric>theta</metric>
    <metric>ltu-rate</metric>
    <metric>unemployment-rate</metric>
    <metric>unemployment-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>count_vacancies</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="profit-margin">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>unem</metric>
    <metric>otj</metric>
    <metric>emp-num</metric>
    <metric>theta</metric>
    <metric>ltu-rate</metric>
    <metric>stu-rate</metric>
    <metric>unemployment-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>count_vacancies</metric>
    <metric>prd</metric>
    <metric>ltu-prd</metric>
    <metric>jsp-rate</metric>
    <metric>mismatch</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="profit-margin">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;on&quot;"/>
      <value value="&quot;off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment with n-(4,1) search units" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>unem-rate</metric>
    <metric>otj</metric>
    <metric>real-stu-rate</metric>
    <metric>stu-rate</metric>
    <metric>ltu-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>stu-dur</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.6"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="1"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.3"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp to test shock and b" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>unem-rate</metric>
    <metric>otj</metric>
    <metric>real-stu-rate</metric>
    <metric>stu-rate</metric>
    <metric>ltu-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>stu-dur</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="5.0E-4"/>
      <value value="9.0E-4"/>
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;off&quot;"/>
      <value value="&quot;on&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp for stability" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>unem-rate</metric>
    <metric>otj</metric>
    <metric>real-stu-rate</metric>
    <metric>stu-rate</metric>
    <metric>ltu-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>stu-dur</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;off&quot;"/>
      <value value="&quot;on&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp for growth rate" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>prd</metric>
    <metric>ltu-prd</metric>
    <metric>js-prd</metric>
    <metric>unem-rate</metric>
    <metric>real-stu-rate</metric>
    <metric>stu-rate</metric>
    <metric>ltu-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>stu-dur</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;off&quot;"/>
      <value value="&quot;on&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment for base search units" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>unem-rate</metric>
    <metric>otj</metric>
    <metric>real-stu-rate</metric>
    <metric>stu-rate</metric>
    <metric>ltu-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>stu-dur</metric>
    <metric>prd</metric>
    <metric>ltu-prd</metric>
    <metric>js-prd</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-search-units">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;off&quot;"/>
      <value value="&quot;on&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp for rc-cost" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>unem-rate</metric>
    <metric>otj</metric>
    <metric>real-stu-rate</metric>
    <metric>stu-rate</metric>
    <metric>ltu-rate</metric>
    <metric>ltu-prob</metric>
    <metric>js-prob</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>stu-dur</metric>
    <metric>prd</metric>
    <metric>ltu-prd</metric>
    <metric>js-prd</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rc-cost">
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-search-units">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;off&quot;"/>
      <value value="&quot;on&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="big exp 2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>unem-rate</metric>
    <metric>ltu-rate</metric>
    <metric>real-stu-rate</metric>
    <metric>stu-rate</metric>
    <metric>theta</metric>
    <metric>count_vacancies</metric>
    <metric>unem</metric>
    <metric>otj</metric>
    <metric>mismatch</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>jsp-rate</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.6"/>
      <value value="1"/>
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.001"/>
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prd-comp">
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rc-cost">
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-search-units">
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.4"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.01"/>
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;off&quot;"/>
      <value value="&quot;on&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp with 100 reps for min unem" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>unem-rate</metric>
    <metric>ltu-rate</metric>
    <metric>real-stu-rate</metric>
    <metric>stu-rate</metric>
    <metric>theta</metric>
    <metric>count_vacancies</metric>
    <metric>unem</metric>
    <metric>otj</metric>
    <metric>mismatch</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <metric>jsp-rate</metric>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-wage">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prd-comp">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rc-cost">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-search-units">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;on&quot;"/>
      <value value="&quot;off&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>unem-rate</metric>
    <metric>ltu-rate</metric>
    <metric>real-stu-rate</metric>
    <metric>theta</metric>
    <metric>mismatch</metric>
    <metric>dur</metric>
    <metric>ltu-dur</metric>
    <enumeratedValueSet variable="minimum-wage">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shock">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prd-comp">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_job_seekers">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rc-cost">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_firms">
      <value value="210"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-bonus">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_vacancies">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ltu-search-minus">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-search-units">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_vacancies">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefits">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="platform">
      <value value="&quot;on&quot;"/>
    </enumeratedValueSet>
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
