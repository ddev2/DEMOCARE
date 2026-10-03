;TODOS 23/10/2017
; - Check if initial activity states are correctly assigned
; - Set AGE iteration at top of assignation procedures + checks for empty subsets
; - Fix dependency & activity assignation for lower & higher ages
; - Fix Care file M -> P
; - Fix Activity file (remove blank spaces)
; - Run activity transitions through R script
; - Fix Links file (add header) to
; - Modularize Table Importer
; - Activity below 15 and above 75 no need to iterate (filter cases)
; - Daniel: %D3 > %D2 > %D1

__includes [
  ;POPULATION
  "PopulationImporter.nls"
  "PopulationCreator.nls"
  "NetworkImporter.nls"
  "NetworkCreator.nls"

  ;CARE
  "CareImporter.nls"
  "CareUpdate.nls"

  ;DEPENDENCY
  "DependencyInitializer.nls"
  "DependencyTransitionsImporter.nls"
  "DependencyTransitionsUpdate.nls"

  ;ACTIVITY
  "ActivityInitializer.nls"
  "ActivityTransitionsImporter.nls"
  "ActivityTransitionsUpdate.nls"

  ;UTILES
  "Utiles.nls"
]

breed [ inactives inactive ]
breed [ actives active ]

undirected-link-breed [ partners partner ]
directed-link-breed [ descendants descendant ]
directed-link-breed [ ascendants ascendant ]

globals [
  gActivityTransitionsArray
  gDependencyTransitionsArray
  gCareArray

; daniel nov-dec 2017
  gTurtleUpdateSet              ; set of agents whose dependency status changes in the current tick.
                               ; We then need to change their activity status

  gCount_debug
  gDebug
  gDebugProc
  gTimer
  gDirSep ; Mac is '/' and windows is '\'

  SEP
  gViewFamIter
  gWarnings

  gMainFileHeader

  gPathIn
  gPathOut

;constant values
  kNoDebug
  kDebug

  kInLaws
  kNoInLaws
  kOnlyInLaws
  kCheckInLaws

  kActive
  kInactive
  kActiveInTheFuture

  kSingle
  kFutureMarried
  kMarried
  kWidowed

  kAgeNoDep                     ; age from which the person is not dependent if healthy (normally 16)
  kMaxCare                      ; the theorical number of hours available for caring (normally 60)

  kPartner_nupt                 ; 60/0: theoretical number of hours of care due to a partnership during ego's lifetime
  kNoPartner_mort               ; 60/0: number of hours of care not available due to partner's death
  kNoPartner_div                ; 60/0: number of hours of care not available due to divorce (not implemented at this time)
  kChildren_fert                ; 60 * k: theoretical number of hours of care available from children during ego's lifetime
  kNoChildren_mort              ; 60 * k: number of hours of care not available due to children's death (k is the number of dead children)
  kWork_partner                 ; max (w, 0): numbers of hours of care removed by partner's work
  kWork_children                ; max (w, 0) per child: numbers of hours of care removed by children's work
  kWork2_partner                ; max (w, 0): numbers of hours of care removed by partner's work
  kWork2_children               ; max (w, 0) per child: numbers of hours of care removed by children's work
  kDependency_partner           ; max (d, 0): number of hours of care removed by partner's dependency
  kDependency_children          ; max (d, 0): number of hours of care removed by children's dependency (total)
  kDependency2_partner          ; max (d, 0): number of hours of care removed by partner's dependency
  kDependency2_age_children     ; max (d, 0): number of hours of care removed by children's dependency as a youngster
  kDependency2_health_children  ; max (d, 0): number of hours of care removed by children's bad health

  kCompetition_partner          ; number of hours of care given by the family to ego's partner
  kCompetition_children         ; total number of hours of care given by ego's partner and their siblings to ego's children
  kCompetition_children_family  ; total number of hours given by ego's children to their own family (their partner as well as and their own children)
  kDeficit                      ; number of hours of the deficit, in case the theoretical total number of hours available is inferior to ego's needs
  kTotalHours                   ; total number of hours of care theoretically available to ego during her/his lifetime (number of partners and children * 60)
  kTotalHoursLivings            ; total number of hours of care theoretically available to ego by LIVING partner and children
  kTotalAvailable               ; total number of hours of care available (after removing hours taken by dependency and work)
  kTotalDependency              ; total number of hours of care taken by dependency
  kTotalWork                    ; total number of hours of care taken by work
  kTotalPartner                 ; number of hours of care given to ego's partner
  kTotalOthers                  ; total number of hours of care given to other relatives by ego's partner and children
  kPartnerHours                 ; number of hours of care by partner theoretically available to ego (60 if ego ever entered a partnership)
  kPartnerHoursLiving           ; number of hours of care by a living partner theoretically available to ego (60 if ego's partner is living)
  kPartnerAvailable             ; number of hours of care by egos' partner really available (after taking into consideration dependency and work)
  kPartnerDependency            ; number of hours of care taken by dependency
  kPartnerWork                  ; number of hours of care taken by work
  kPartnerOthers                ; number of hours of care taken by caring for others
  kLengthEgoAccount             ; number of variables in the list

  gFilteredActivityTable
  act_dependencyLevels
  act_statusLevels
  act_sexLevels
  act_activityLevels
  act_ageLevels

  gAgents_sum                    ; counting variable when exploring a network tree
  gAgents_set                    ; set of agents in the network tree to prevent for exploring the same agent two times
  gNumFamSel
  g_nEgos

  gFamCareReceivedByEgo
  gFamCareGivenToEgo

  ; for plot proportion by dependency level
  plot_d0
  plot_d1
  plot_d2
  plot_d3

  ;t_kinTypeRelEgo values:
  kEgo
  kEgoPartner
  kEgoChild
  kEgoChildInLaw
  kEgoGrandChild
  kEgoGrandChildInLaw

  ; family relationship between agentFrom and agentTo
  kEgoToPartner
  kEgoPartnerToEgo
  kEgoToChild
  kEgoPartnerToChild
  kChildToEgo
  kChildToEgoPartner
  kChildToSibling
  kChildToPartner
  kChildInLawToChild
  kChildToGrandChild
  kChildInLawToGrandChild
  kGrandChildToChild
  kGrandChildToChildInLaw
  kGrandChildToSibling

  kCare2EgoPToChild
  kCare2ChildToEgoP
  kCare2ChildToPartner
  kCare2ChildToSibling
  kCare2ChildToGrandChild
  kCare2EgoPToEgo
  kCare2ChildToEgo

]

turtles-own [
  t_gender
  t_genderRecoded ;{F=0, M=1}
  t_age
  t_ageDef                      ; BUG FIX (2026-08-16): the "ageDef" column from the population
                                 ; file (age this individual reaches at tickOut) -- previously never
                                 ; read at all. Needed so UpdateSurvival can compute t_age directly
                                 ; from (t_ageDef, tickOut, ticks) every active tick instead of
                                 ; incrementing a running total -- see UpdateSurvival for why.
  t_status
  t_statusRecoded ;{A=0, B=1, M=2}
  t_activity
  t_activityRecoded ;{A=0, C=1, P=2}
  t_dependency
  tickIn
  tickOut

  t_isEgo                       ; whether current agent is an ego
  t_isPartner                   ; whether current agent is a partner of ego
  t_isChildren                  ; whether current agent is a children of ego
  t_isChildrenInLaw             ; whether current agent is an in-law children of ego
  t_isGrandChildren             ; whether current agent is a grand children of ego
  t_kinTypeRelEgo				        ; kin type relative to ego
  								; ego is 0
  								; ego's partner is 1
  								; ego's children are 2
  								; ego's children-in-law are 3
  								; ego's grand children are 4
  								; ego's grand children-in-law are 5

  t_maritalStatus                 ; marital status (single, married, widowed) at each tick
  t_nPartner                      ; number of partners during ego's lifetime
  t_nActivePartner
  t_nFutureActivePartner
  t_nDeadPartner
  t_hasPartnerEgo                 ; whether ego has a partner during her/his lifetime
  t_hasChildrenEgo                   ; whether ego has children during her/his lifetime
  t_hasChildrenInLawEgo              ; whether ego has in-law children during her/his lifetime
  t_childrenNeedCare              ; whether children in the current family need care
  t_nChildren                     ; total number of children during agent lifetime
  t_nActiveChildren               ; number of ACTIVES children at each tick
  t_nFutureActiveChildren         ; number of currently INACTIVES children at each tick
  t_nDeadChildren                 ; number of currently DEAD children at each tick
  t_nInLaws                       ; total number of in-law children during agent lifetime
  t_nActiveInLaws                 ; number of ACTIVES in-law children at each tick
  t_nFutureActiveInLaws           ; number of currently INACTIVES in-law children at each tick
                                ; that will be ACTIVES in the future, because they will marry one of
                                ; agent's children
  t_nDeadInLaws                   ; number of currently DEAD in-law children at each tick

  t_childrenSet                 ; set of children of agent at each tick (we need that for males, since the network is female centric)
                              ; The current simulation set agent state to 'died' when they reach their tickOut value
                              ; so they disappear from the agent sets, and if a female partner dies, then her/hist list of children is not available to the surviving male partner

  t_idpartner
  t_idfamily

  t_care                          ; care available or needed at the personal level by the agent, as determined by personal characteristics like age, activity, educational level, dependency
  t_careSupply                    ; care available for relatives (when the variable care is positive) at the START of the computeCare process
  t_careDemand                    ; care needed (when the variable care is negative) at the START of the computeCare process
  t_netCareSupply                 ; care still available after taking into account relatives' demand, at the END of the computeCare process
  t_netCareDemand                 ; care still needed by agent, after looking at total available from all other family members, at the END of the computeCare process.
                                ; If equal 0 and careDemand > 0, then needs are fully covered by informal care. If positive, formal care is needed

  t_cr_young                      ; number of hours of care lost for being young
  t_cr_dep                        ; number of hours of care lost for being dependent
  t_cr_work                       ; number of hours of care lost for working

  t_careGivenToEgo                ; care given to ego by relatives
  t_careGivenToPartnerEgo         ; care given to ego's partner by relatives
  t_careGivenToOtherRelatives     ; care given to relatives by ego's relatives

  t_carebyPartner                 ; care received from partner: 0 or positive
  t_carebyChildren                ; care received from children: 0 or positive
  t_carebyParents                 ; care received from both parents: 0 or positive
  t_carebyBrothers                ; care received from siblings: 0 or positive
  t_carebyRelatives               ; care received from all relatives (sum of the preceding four)

  t_ego-accounts                ; list of accounting variables for ego
                              ; NOPARTNER_NUPT, NOPARTNER_MORT, NOPARTNER_DIV, NOCHILDREN_FERTILITY, NOCHILDREN_MORTALITY
                              ; WORK_PARTNER, WORK_CHILDREN
                              ; DEPENDENCY_PARTNER, DEPENDENCY_CHILDREN
                              ; COMPETITION_PARTNER, COMPETITION_CHILDREN, COMPETITION_CHILDREN_FAMILY
                              ; DEFICIT
                              ; TOTAL_HOURS, TOTAL_HOURS_LIVINGS, TOTAL_AVAILABLE, TOTAL_DEPENDENCY, TOTAL_WORK, TOTAL_PARTNER, TOTAL_OTHERS
                              ; PARTNER_HOURS, PARTNER_HOURS_LIVING, PARTNER_AVAILABLE, PARTNER_DEPENDENCY, PARTNER_WORK, PARTNER_CARE_RECEIVED

  ;;obsolete
  ;;mixedformal
  ;;mixedinformal

  ; the following "care2" variables are used in an alternate way of computing and accounting for care
  t_care2                       ; amount of care needed or available (do not vary)
  t_care2Supply                 ; amount of care available to others (vary until fully depleted)
  t_care2Demand                 ; amount of care needed (vary until fully covered)

  t_care2From_list                   ; list of agents who gave care: [kinType id amount]
  t_care2To_list                     ; list of agents who received care: [kinType id amount]
  t_care2CompCount                   ; list of hours of care given to agents competing with ego

  ; kinTypes who give/receive care are:
  ; 1 ego
  ; 2 ego's partner
  ; 3 children
  ; 4 in-law partner
  ; 5 siblings
  ; 6 grand children
]

to initGlobals
  clear-all

  random-seed 1234

  set SEP "\t"
  set gViewFamIter 0
  set gWarnings TRUE

  set gPathIn (word "/Users/" User "/Google Drive/DemoCare_BS/results/")
  set gPathOut (word "/Users/" User "/Dropbox/DemoCare_BS/BootStraps/")

  ; 2026-08-18 (round 1): homogenized with the two R ports' output at
  ; Daniel's request, for direct Excel/pivot-table comparison across all
  ; three engines. Two changes from the previous 65-column header:
  ;
  ; (1) Dropped 8 columns that are either genuinely constant/run-level
  ;     (not per-ego, not useful to compare) or provably redundant with
  ;     another column already in this same header -- see the matching
  ;     dated comment in printInfoEgo below for the full evidence trail
  ;     on each one (this header and printInfoEgo's file-print list must
  ;     always stay in exact 1:1 correspondence, same count, same order):
  ;       inputKin, educKind, varFert, CareFromChildrenInLaw? (run-level
  ;         constants, same value every row -- R's output already never
  ;         had these)
  ;       kTotalPartner, kTotalOthers (kPartnerOthers is a separate,
  ;         similarly-always-0 column that is KEPT -- see below):
  ;         confirmed always 0 under care2Only?=TRUE (Daniel's real
  ;         run config) by this model's own warning message (~line 902,
  ;         "kTotalPartner/kPartnerOthers/kTotalOthers will read as 0 in
  ;         this mode") and CareUpdate.nls's own comment at
  ;         egoAccountsActualize2 ("NOT fixed here: kTotalPartner /
  ;         kPartnerOthers / kTotalOthers still come out as 0"). Neither R
  ;         script computes these at all, so dropping them creates no
  ;         parity gap.
  ;       kNoPartner_div: hardcoded to exactly 0 unconditionally in BOTH
  ;         egoAccountsActualize [CareUpdate.nls ~614] and
  ;         egoAccountsActualize2 [~664] -- always 0 regardless of run
  ;         mode, not config-dependent like the others above. R never
  ;         computed this either.
  ;       kPartner_nupt: CareUpdate.nls copies this straight into
  ;         kPartnerHours right before this file is written
  ;         (egoAccountsActualize2 ~708: "set ... kPartnerHours ...
  ;         item kPartner_nupt t_ego-accounts") -- so kPartner_nupt and
  ;         kPartnerHours are ALWAYS numerically identical in this output,
  ;         a pure duplicate. kPartnerHours is kept (matches R's own
  ;         column of the same name); kPartner_nupt is redundant with it.
  ;     kPartnerOthers itself is KEPT even though also always 0 under
  ;     care2Only?=TRUE, because Modules/R PAPER/R code/
  ;     democare_compute.R's careSupply() reads it by exact name from
  ;     BOTH R outputs -- dropping it from R would break that existing
  ;     script, and dropping it from only NetLogo (while R keeps it)
  ;     would break the column-position parity this whole homogenization
  ;     is for. Likewise carebyPartner/carebyChildren are KEPT even
  ;     though always 0 here under care2Only?=TRUE (only the disabled
  ;     "care1" cascade ever populates them -- see this file's own
  ;     gWarnings note and Modules/R PAPER/R code/compare_netlogo_vs_r.R's
  ;     documented care2Only? workaround) -- both R scripts keep them too
  ;     (as real, non-zero duplicates of care2byPartner/care2byChildren,
  ;     since democare_graphs.R's default reads the "care2" names), so
  ;     dropping only NetLogo's copy would again break parity. Flagging
  ;     this asymmetry explicitly: carebyPartner/carebyChildren will
  ;     always read 0 in NetLogo's output specifically, but not in
  ;     either R output -- use care2byPartner/care2byChildren for any
  ;     actual cross-engine comparison, in all three files.
  ;
  ; (2) Added 2 columns neither R script's port had a NetLogo source for
  ;     until now (t_dependency was never written to this file at all;
  ;     grandchildren were only ever computed per-child, batch by batch,
  ;     inside care2Update [CareUpdate.nls], never rolled up to a single
  ;     per-ego total) -- same two columns added to both R scripts earlier
  ;     today: egoDependencyLevel (t_dependency, 0-3) and nGrandChildren.
  ;
  ; 2026-08-18 (round 2, "exact parity"): Daniel asked for the UNION of
  ; every column either engine (NetLogo or R) ever produces, identical
  ; set/order in all three output files, for direct Excel pivot-table
  ; comparison. Three more changes on top of round 1:
  ;
  ; (a) Dropped 3 more confirmed-exact-duplicate columns: kWork2_partner
  ;     (≡kWork_partner AND ≡kPartnerWork under care2Only?=TRUE --
  ;     egoAccountsActualize2 lines 678/682, CareUpdate.nls), kWork2_children
  ;     (≡kWork_children -- line 683), kDependency2_partner (≡kDependency_partner
  ;     AND ≡kPartnerDependency under care2Only?=TRUE -- lines 677/685).
  ;     kWork_partner/kPartnerWork and kDependency_partner/kPartnerDependency
  ;     are themselves ALSO exact duplicates of each other under
  ;     care2Only?=TRUE, but both survive since both R scripts now carry
  ;     both names too (see computeCare_baseR.R's Section 7c comment for
  ;     the full trace) -- Daniel wants every column mirrored, not
  ;     deduplicated across engines.
  ; (b) Fixed a typo from round 1: "nInLaws " had a trailing space baked
  ;     into the header string itself.
  ; (c) Added 9 columns R already had that NetLogo's file never printed:
  ;     familyId (t_idfamily), status (t_status), age (t_age), activity
  ;     (t_activity), hasPartner (t_hasPartnerEgo -- a LIFETIME flag, not
  ;     current marital status; distinct from kPartnerHoursLiving's own
  ;     t_maritalStatus=kMarried condition), formalCareNeeded
  ;     (max(0, t_care2Demand) -- the REAL, active care2 value under
  ;     care2Only?=TRUE, NOT t_netCareDemand, which is dead care1-only
  ;     code in this mode -- see printInfoEgo's dated comment below),
  ;     nDependentChildren/nChildrenActivityC/nChildrenActivityP (each
  ;     ego's own active bio children's dependency/activity breakdown).
  ;
  ; The full 65-column master order below is now IDENTICAL, column-for-
  ; column, to computeCare_baseR.R's and computeCare_tidyverse.R's own
  ; `outCols` / final select() lists -- if you add a column to one engine,
  ; add it to all three (and keep this header + printInfoEgo's file-print
  ; list below in the same 1:1 correspondence).
  set gMainFileHeader (word
    "ticks" SEP
    "who" SEP
    "familyId" SEP
    "sex" SEP
    "status" SEP
    "age" SEP
    "egoDependencyLevel" SEP
    "activity" SEP
    "hasPartner" SEP
    "nGrandChildren" SEP
    "maritalStatus" SEP
    "nPartner" SEP
    "nActivePartner" SEP
    "nFutureActivePartner" SEP
    "nDeadPartner" SEP
    "nChildren" SEP
    "nActiveChildren" SEP
    "nFutureActiveChildren" SEP
    "nDeadChildren" SEP
    "nDependentChildren" SEP
    "nChildrenActivityC" SEP
    "nChildrenActivityP" SEP
    "nInLaws" SEP
    "nActiveInLaws" SEP
    "nFutureActiveInLaws" SEP
    "nDeadInLaws" SEP
    "care" SEP
    "netCareDemand" SEP
    "netCareSupply" SEP
    "formalCareNeeded" SEP
    "care2Demand" SEP
    "care2Supply" SEP
    "carebyPartner" SEP
    "carebyChildren" SEP
    "care2byPartner" SEP
    "care2byChildren" SEP
    "egoPtoChild" SEP
    "childToEgoP" SEP
    "childToPartner" SEP
    "childToSiblings" SEP
    "childTograndCh" SEP
    "kPartnerHours" SEP
    "kTotalHours" SEP
    "kTotalHoursLivings" SEP
    "kTotalAvailable" SEP
    "kTotalDependency" SEP
    "kTotalWork" SEP
    "kPartnerHoursLiving" SEP
    "kPartnerAvailable" SEP
    "kPartnerDependency" SEP
    "kPartnerWork" SEP
    "kPartnerOthers" SEP
    "kNoPartner_mort" SEP
    "kChildren_fert" SEP
    "kNoChildren_mort" SEP
    "kWork_partner" SEP
    "kWork_children" SEP
    "kDependency_partner" SEP
    "kDependency_children" SEP
    "kDependency_health_children" SEP
    "kDependency_age_children" SEP
    "kCompetition_partner" SEP
    "kCompetition_children" SEP
    "kCompetition_children_family" SEP
    "kDeficit"
  )


  set gDebug false
  set gDebugProc []
  set debug? false
  set kNoDebug false
  set kDebug true

  set kAgeNoDep 16

  set kInLaws 0
  set kNoInLaws 1
  set kOnlyInLaws 2
  set kCheckInLaws kInLaws
  ifelse CareFromChildrenInLaw? [set kCheckInLaws kInLaws][set kCheckInLaws kNoInLaws]

  set kSingle 0
  set kMarried 1
  set kWidowed 2
  set kFutureMarried 3


  set kEgo 0
  set kEgoPartner 1
  set kEgoChild 2
  set kEgoChildInLaw 3
  set kEgoGrandChild 4
  set kEgoGrandChildInLaw 5

  set kEgoToPartner 0
  set kEgoPartnerToEgo (kEgoToPartner + 1)
  set kEgoToChild (kEgoPartnerToEgo + 1)
  set kEgoPartnerToChild (kEgoToChild + 1)
  set kChildToEgo (kEgoPartnerToChild + 1)
  set kChildToEgoPartner (kChildToEgo + 1)
  set kChildToSibling (kChildToEgoPartner + 1)
  set kChildToPartner (kChildToSibling + 1)
  set kChildInLawToChild (kChildToPartner + 1)
  set kChildToGrandChild (kChildInLawToChild + 1)
  set kChildInLawToGrandChild (kChildToGrandChild + 1)
  set kGrandChildToChild (kChildInLawToGrandChild + 1)
  set kGrandChildToChildInLaw (kGrandChildToChild + 1)
  set kGrandChildToSibling (kGrandChildToChildInLaw + 1)

  set kPartner_nupt 0
  set kNoPartner_mort kPartner_nupt + 1
  set kNoPartner_div kNoPartner_mort + 1
  set kChildren_fert kNoPartner_div + 1
  set kNoChildren_mort kChildren_fert + 1
  set kWork_partner kNoChildren_mort + 1
  set kWork_children kWork_partner + 1
  set kWork2_partner kWork_children + 1
  set kWork2_children kWork2_partner + 1
  set kDependency_partner kWork2_children + 1
  set kDependency2_partner kDependency_partner + 1
  set kDependency_children kDependency2_partner + 1
  set kDependency2_health_children kDependency_children + 1
  set kDependency2_age_children kDependency2_health_children + 1
  set kCompetition_partner kDependency2_age_children + 1
  set kCompetition_children kCompetition_partner + 1
  set kCompetition_children_family kCompetition_children + 1
  set kDeficit kCompetition_children_family + 1
  set kTotalHours kDeficit + 1
  set kTotalHoursLivings kTotalHours + 1
  set kTotalAvailable kTotalHoursLivings + 1
  set kTotalDependency kTotalAvailable + 1
  set kTotalWork kTotalDependency + 1
  set kTotalPartner kTotalWork + 1
  set kTotalOthers kTotalPartner + 1
  set kPartnerHours kTotalOthers + 1
  set kPartnerHoursLiving kPartnerHours + 1
  set kPartnerAvailable kPartnerHoursLiving + 1
  set kPartnerDependency kPartnerAvailable + 1
  set kPartnerWork kPartnerDependency + 1
  set kPartnerOthers kPartnerWork + 1
  set kLengthEgoAccount kPartnerOthers + 1

  set kCare2EgoPToChild 0
  set kCare2ChildToEgoP (kCare2EgoPToChild + 1)
  set kCare2ChildToPartner (kCare2ChildToEgoP + 1)
  set kCare2ChildToSibling (kCare2ChildToPartner + 1)
  set kCare2ChildToGrandChild (kCare2ChildToSibling + 1)
  set kCare2EgoPToEgo (kCare2ChildToGrandChild + 1)
  set kCare2ChildToEgo (kCare2EgoPToEgo + 1)

  set kActive 1
  set kInactive 0
  set kActiveInTheFuture 2

  ImportPermanentData

end

to writeHeader
    if outputKind = "dependency" [file-print (word "ticks" SEP "sex" SEP "niv" SEP "d0" SEP "d1" SEP "d2" SEP "d3")]
    if outputKind = "activity" or outputKind = "activityUpdate" [file-print (word "ticks" SEP "sex" SEP "niv" SEP "dep" SEP "A" SEP "C" SEP "P")]
    if outputKind = "setup" [outputSetup]
    if outputKind = "ego" and not fileAppend? [  file-print gMainFileHeader ]
    if outputKind = "partnerEgo" and not fileAppend? [  file-print (word "inputKin" SEP "educKind" SEP "ticks" SEP "who" SEP "care" SEP "careSupply" SEP "careDemand" SEP "netCareSupply " SEP "netCareDemand" SEP "careGivenToOtherRelatives" SEP "careGivenToEgo" SEP "careGivenToPartnerEgo") ]
    if outputKind = "childrenEgo" and not fileAppend? [  file-print (word "inputKin" SEP "educKind" SEP "ticks" SEP "who" SEP "care" SEP "careSupply" SEP "careDemand" SEP "netCareSupply " SEP "netCareDemand" SEP "careGivenToOtherRelatives" SEP "careGivenToEgo") ]
end

to chooseFile
  initGlobals

  user-message ("Select a democare file with extension _indKin.txt")

  let fileInWithPath user-file
  if fileInWithPath = false [
    user-message ("No file selected")
    stop
  ]

  if (gDirSep = 0) [
    ifelse member? "/" fileInWithPath [
      set gDirSep "/" ; Mac
    ][
      set gDirSep "\\" ; Win
    ]
  ]

  let state TRUE
  let pos 0
  let posStartFileName 0
  let lgFileIn length fileInWithPath
  while [pos < lgFileIn]
  [
    if (gDirSep = item pos fileInWithPath)
    [set posStartFileName pos]
    set pos pos + 1
  ]
  set posStartFileName posStartFileName + 1

  set gPathIn substring fileInWithPath 0 posStartFileName
  let fileIn substring fileInWithPath posStartFileName lgFileIn
  let fileRoot 0
  let fileRootWithPath 0
  ifelse not (member? "_indKin.txt" fileIn) [
    user-message ("Incorrect file name (not ending with _indKin.txt)")
    stop
  ][
    set fileRoot substring fileIn 0 (length fileIn - 11)
    set fileRootWithPath substring fileInWithPath 0 (length fileInWithPath - 11)
  ]

  set current_input fileRoot

  ifelse OutputDirSameAsInput = true [
    set gPathOut gPathIn
  ][
    user-message ("Select the output directory")
    set gPathOut user-directory
    if gPathOut = false [
      user-message ("No directory selected. Stopping...")
      stop
    ]
  ]


  InitializePopulation fileRootWithPath

  let newFile (word gPathOut fileRoot ".dat")

  if file-exists? newFile and not fileAppend? [file-delete newFile]
  file-open newFile
  if not fileAppend? [writeHeader]

  reset-ticks
  resetTimer
  print "Setup finished without errors!"

end

to setup
  initGlobals

  let nameInputFile_root nameInputFile_sub false

  ifelse inputKin != "Other" [
    let dataKind "/smallData/"
    if bigData? [
      set dataKind "/bigData/"
    ]

    let pathKin word "../data_original/" inputKin
    set pathKin (word pathKin "/" educKind)
    set pathKin word pathKin dataKind


    set nameInputFile_root word pathKin nameInputFile_root
  ][
    set nameInputFile_root word gPathIn item 0 item 0 nameInputFile_root
    user-message nameInputFile_root
  ]

  InitializePopulation nameInputFile_root

  ;set kMaxCare item 0 item 0 item 25 gCareArray

  ifelse inputKin != "Other" [
   if outputs? [
     let newFile ""
     ifelse askName? [
       set newFile user-new-file
     ][
       let nameFile (word inputKin "_" educkind)
       if not bigData? []
       set newFile (word "../results/results_" nameFile ".txt")
     ]

     if file-exists? newFile and not fileAppend? [file-delete newFile]
     file-open (word newFile ".csv")
     if not fileAppend? [writeHeader]
   ]
  ][
    let newFile (word gPathOut current_input ".csv")

    if file-exists? newFile and not fileAppend? [file-delete newFile]
    file-open newFile
    if not fileAppend? [writeHeader]
  ]

  reset-ticks
  resetTimer
  print "Setup finished without errors!"

 ; if not letsGo [ stop ]
end

to-report nameInputFile_sub [cList]
  let nameInputFile_root ""

  if inputKin = "cohort_98" [
    set nameInputFile_root "KIN1898"
  ] if inputKin = "cohort_08" [
    set nameInputFile_root "KIN1908"
  ] if inputKin = "cohort_18" [
    set nameInputFile_root "KIN1918"
  ] if inputKin = "cohort_28" [
    set nameInputFile_root "KIN1928"
  ] if inputKin = "cohort_38" [
    set nameInputFile_root "KIN1938"
  ] if inputKin = "cohort_48" [
    set nameInputFile_root "KIN1948"
  ] if inputKin = "cohort_58" [
    set nameInputFile_root "KIN1958"
  ] if inputKin = "cohort_68" [
    set nameInputFile_root "KIN1968"
  ] if inputKin = "cohort_69" [
    set nameInputFile_root "KIN1969"
  ] if inputKin = "cohort_70" [
    set nameInputFile_root "KIN1970"

  ] if (inputKin = "Other")  [

    ifelse (current_input = "") [
      set nameInputFile_root user-input "Input root file name: "
      set current_input nameInputFile_root
    ][
      set nameInputFile_root current_input
    ]
    user-message "Now locate the input directory..."
    set gPathIn user-directory
    user-message "... now locate the output directory..."
    set gPathOut user-directory
    let aList (list inputKin educKind varFert?)
    set aList (list (list nameInputFile_root aList))
    report aList
  ]

  if inputKin != "Other" [
      if educKind = "stochastic" [
      set nameInputFile_root word nameInputFile_root "_EDU0"
    ] if educKind = "individual" [
      set nameInputFile_root word nameInputFile_root "_EDU1"] if educKind = "intrafamily" [
      set nameInputFile_root word nameInputFile_root "_EDU2"
    ]

    ifelse varFert? [
      set nameInputFile_root word nameInputFile_root "_VAR"
    ][
      set nameInputFile_root word nameInputFile_root "_CONSTANT"
    ]

  ]

  if cList [
    let aList (list inputKin educKind varFert?)
    set nameInputFile_root (list (list nameInputFile_root aList))
  ]

  report nameInputFile_root
end

to batch_files
  initGlobals

  ImportPermanentData

  let newFile (word "../results_.txt")

  if file-exists? newFile [file-delete newFile]
  file-open newFile
  file-print gMainFileHeader
  file-close-all

  let nameInputFile_rootList [
    ["../data_original/cohort_08/individual/bigData/KIN1908_EDU1_VAR" ["cohort_08" "individual" true]]
    ["../data_original/cohort_18/individual/bigData/KIN1918_EDU1_VAR" ["cohort_18" "individual" true]]
    ["../data_original/cohort_28/individual/bigData/KIN1928_EDU1_VAR" ["cohort_28" "individual" true]]
    ["../data_original/cohort_38/individual/bigData/KIN1938_EDU1_VAR" ["cohort_38" "individual" true]]
    ["../data_original/cohort_48/individual/bigData/KIN1948_EDU1_VAR" ["cohort_48" "individual" true]]
    ["../data_original/cohort_48/individual/bigData/KIN1948_EDU1_CONSTANT" ["cohort_48" "individual" false]]
    ["../data_original/cohort_58/individual/bigData/KIN1958_EDU1_VAR" ["cohort_58" "individual" true]]
    ["../data_original/cohort_68/stochastic/bigData/KIN1968_EDU0_VAR" ["cohort_68" "stochastic" true]]
    ["../data_original/cohort_68/individual/bigData/KIN1968_EDU1_VAR" ["cohort_68" "individual" true]]
    ["../data_original/cohort_68/intrafamily/bigData/KIN1968_EDU2_VAR" ["cohort_68" "intrafamily" true]]
    ["../data_original/cohort_68/individual/bigData/KIN1968_EDU1_CONSTANT" ["cohort_68" "individual" false]]
  ]
  let nameInputFile_root ""
  let infoList []

  no-display
  foreach nameInputFile_rootList [
    aList ->
    set nameInputFile_root item 0 aList
    set infoList item 1 aList

    InitializePopulation nameInputFile_root

    ;set kMaxCare item 0 item 0 item 25 gCareArray

    reset-ticks

    file-open newFile

    while [letsGo true infoList]
    [ tick ]

    clear-turtles
  ]
  display
  file-close-all

end

to bootstrapList
  initGlobals

  let nameInputFile_rootList [
    ["KIN1908_EDU1_VAR" ["cohort_08" "individual" true]]
    ["KIN1918_EDU1_VAR" ["cohort_18" "individual" true]]
    ["KIN1928_EDU1_VAR" ["cohort_28" "individual" true]]
    ["KIN1938_EDU1_VAR" ["cohort_38" "individual" true]]
    ["KIN1948_EDU1_VAR" ["cohort_48" "individual" true]]
    ["KIN1958_EDU1_VAR" ["cohort_58" "individual" true]]
    ["KIN1968_EDU0_VAR" ["cohort_68" "stochastic" true]]
    ["KIN1968_EDU1_VAR" ["cohort_68" "individual" true]]
    ["KIN1968_EDU2_VAR" ["cohort_68" "intrafamily" true]]
    ["KIN1968_EDU1_CONSTANT" ["cohort_68" "individual" false]]
  ]

  bootstrap_sub nameInputFile_rootList
end

to bootstrap_inputKin
  initGlobals

  let nameInputFile_rootList nameInputFile_sub true

  bootstrap_sub nameInputFile_rootList

  print ("Done bootstrapping!")
end

to bootstrap_sub [nameInputFile_rootList]
  print "================ Bootstrapping!! ================="
  print nameInputFile_rootList

  ; reset the random number generator.
  ;In this context this is not necessary, because we bootstrap in a continuos way, so at the start of a simulation based on the same KinFert file, the first random number will be different
  random-seed new-seed

  ImportPermanentData

  let nameInputFile_root ""
  let nameInputFile ""
  let nameOutputFile ""
  let infoList []
  let bs_X_mod bs_X - (depSim - 1)
  let loop_simul n-values bs_X_mod [ i -> i + depSim ]
  let loop_bootstrap n-values bs_N [ i -> i + 1 ]

  no-display
  foreach nameInputFile_rootList [
    aList ->
    set nameInputFile_root item 0 aList
    set infoList item 1 aList
    foreach loop_simul [
      nSimul ->
      set nameInputFile (word gPathIn nameInputFile_root "_" nSimul)
      print (word "input => " nameInputFile)

      foreach loop_bootstrap [
        nBootStrap ->

        set nameOutputFile (word gPathOut nameInputFile_root "_" nSimul "_" nBootStrap ".txt")
        print (word "output => " nameOutputFile)

        if (not file-exists? nameOutputFile) or reWrite? [
          InitializePopulation nameInputFile

          ;set kMaxCare item 0 item 0 item 25 gCareArray
          reset-ticks
          if file-exists? nameOutputFile [file-delete nameOutputFile]
          file-open nameOutputFile
          file-print gMainFileHeader

          resetTimer

          while [letsGo true infoList]
          [ tick ]
          clear-turtles

          file-close-all
          showTimer
          repeat 3 [ beep wait 0.1 ]
        ]

        set gWarnings FALSE

      ]

    ]
  ]
  display

end

to countEgos
  let tot 0
  ask turtles with [t_isEgo] [
    set tot tot + 1
  ]
  print (word "egos: " tot)
end

to ImportPermanentData
  ; paths repointed 2026-08-07 from "../data_original/" and "../data_transformed/"
  ; (folders that don't exist in this project) to "../inputs/", which is where these
  ; files actually live (Modules/NETLOGO/inputs/) -- same folder computeCare_baseR.R
  ; and computeCare_tidyverse.R already read from. act_trans_trans.csv was also
  ; renamed to match the real file, act_transitions.csv; care 0.1_trans.csv to the
  ; real file, care 0.1.csv.
  set gActivityTransitionsArray ImportActivityTransitions "../inputs/act_transitions.csv"
  if depTrans = "main" [
    set gDependencyTransitionsArray ImportDependencyTransitions "../inputs/dependency_transitions 0.1.csv"
  ]
  if depTrans = "1->2" [
    set gDependencyTransitionsArray ImportDependencyTransitions "../inputs/dependency_transitions p1_2.csv"
  ]
  if depTrans = "1->3" [
    set gDependencyTransitionsArray ImportDependencyTransitions "../inputs/dependency_transitions p1_3.csv"
  ]
  if depTrans = "0->2" [
    user-message "dependency transitions 0->2 not created yet. Using MAIN!"
    set gDependencyTransitionsArray ImportDependencyTransitions "../inputs/dependency_transitions 0.1.csv"
  ]
  set gCareArray ImportCare "../inputs/care 0.1.csv"
end

to InitializePopulation [nameInputFile_root]
  print (word nameInputFile_root "_indKin.txt")
  ;let agentList ImportPopulation word "Population 0.1.txt"
  ;; 2026-08-11: ImportPopulation now reports (list agentList hasIdFamily
  ;; egoIdx), auto-detecting (a) whether this population file has the
  ;; newer leading "idFamily" column (added for bootstrap resampling) or
  ;; the older format without it, and (b) where the "ego" column sits,
  ;; since newer files drop the trailing "linked"/"useful" columns that
  ;; used to follow it -- see PopulationImporter.nls / PopulationCreator.nls.
  let popResult ImportPopulation (word nameInputFile_root "_indKin.txt")
  let agentList item 0 popResult
  let hasIdFamily item 1 popResult
  let egoIdx item 2 popResult
  if empty? agentList [ stop ]
  print "DONE: Population file imported"
  CreatePopulation agentList hasIdFamily egoIdx print "DONE: Population created"

  print (word nameInputFile_root "_indKin_link.txt")
  ;let network ImportNetwork word "links 0.1.txt" print "DONE: Links file imported"
  let network ImportNetwork (word nameInputFile_root "_indKin_link.txt") print "DONE: Links file imported"
  CreateNetwork network print "DONE: Links created"
  AssignDependency "../inputs/dependency 0.1.csv" print "DONE: Dependency assigned"
  AssignActivity "../inputs/activity 0.3.csv" print "DONE: Activity assigned"
  ask turtles [
    ;UpdateCare
  ]
end

to multGo [num]
  loop [
    ifelse num > 0 [
      set num num - 1
      if not go [stop]
    ][
      stop
    ]
  ]
end

to-report go
  no-display
  if not (letsGo false (list inputKin educKind varFert?)) [ report false ]
  tick
  report true
end

to-report letsGo [multiple infoList]

  set gDebug debug?

  set g_nEgos count actives with [t_isEgo]

  if g_nEgos = 0 [
    if not multiple [
      file-close-all
      print "DEMOCARE Ended!!"
    ]
    report false
  ]

  if gWarnings [
    if ticks = 0 and not depTrans? [user-message "dependency transitions not activated!"]
    if ticks = 0 and not actTrans? [user-message "activity transitions not activated!"]
    if ticks = 0 and depTrans != "main" [user-message "dependency transitions for sensivity analysis only!"]
    if ticks = 0 and care2Only? [user-message "care2Only? is ON: using the care2 algorithm (matches the R port). kTotalPartner/kPartnerOthers/kTotalOthers will read as 0 in this mode -- see CareUpdate.nls comments (2026-08-07 fix)."]
  ]

; daniel nov-dec 2017
  if not multiple and outputs? [
   if outputKind = "dependency" [outputDependency]
   if outputKind = "activity" [outputActivity actives]
  ]
; end daniel

  ask inactives [
    if tickIn = ticks [
      set breed actives
      set hidden? false
      initAgent self
    ]
  ]

; daniel nov-dec 2017
  set gTurtleUpdateSet (turtle-set nobody)

  ask actives [
    if t_dependency < 3 and depTrans? [
      UpdateDependency
    ]
  ]

  if count gTurtleUpdateSet > 0 [
    ;print (word "updated ego " count gTurtleUpdateSet with [t_isEgo] " of total updated " count gTurtleUpdateSet)
    setActivity gTurtleUpdateSet
    if not multiple and outputs? [
      if outputKind = "activityUpdate" [outputActivity gTurtleUpdateSet]
    ]
  ]
 ; end daniel

  ask actives [

    if t_age > 14 and t_age < 75 and actTrans? [
      UpdateActivity
    ]
    UpdateSurvival
; daniel nov-dec 2017
    UpdateCare
; end daniel
  ]

  ; TEMPORARY DIAGNOSTIC (2026-08-16): checks whether the 2026-08-16 t_age
  ; fix is actually running -- prints a few "married-in" individuals'
  ; t_age next to what the corrected formula says it should be, once,
  ; at tick 10. Safe to delete once you've confirmed the answer; doesn't
  ; touch any simulation state or output files, just prints to the Output
  ; area like any other `print` call already in this file.
  checkAgeFix

  set gCount_debug 1
  ask actives with [t_isEgo] [
   computeCare self kNoDebug

   if multiple or outputs? [
    if outputKind = "ego" [
        printInfoEgo self infoList
    ]
   ]
  ]

  if not multiple [ plotsUpdate ]

  report true
end

; TEMPORARY DIAGNOSTIC (2026-08-16): see the call site above (inside
; letsGo, right after the UpdateSurvival/UpdateCare ask-actives block)
; for what this checks and why. Fires exactly once, at tick 10, so it
; doesn't spam the Output area on a long/multi-tick run. Look for lines
; starting "AGE-FIX-CHECK" in the Output/Command Center panel after
; running -- no need to type anything, just run setup+go as usual and
; read what appears there. Delete this procedure and its call site
; once you've confirmed the answer.
to checkAgeFix
  if ticks = 10 [
    let candidates actives with [tickIn > 0 and t_age > 0]
    ifelse any? candidates [
      ask n-of (min list 5 count candidates) candidates [
        print (word "AGE-FIX-CHECK who=" who
          " t_age=" t_age
          " t_ageDef=" t_ageDef
          " tickIn=" tickIn
          " tickOut=" tickOut
          " ticks=" ticks
          " expected=" (t_ageDef - (tickOut - ticks))
          " MATCH=" (t_age = (t_ageDef - (tickOut - ticks))))
      ]
    ][
      print "AGE-FIX-CHECK: no married-in individuals (tickIn>0, age>0) active yet at tick 10 -- nothing to check this tick."
    ]
  ]
end

to printInfoEgo [agentEgo infoList]
  ; 2026-08-18 (round 1): inputKinVal/educKindVal/varFertVal (from
  ; infoList) and CareFromChildrenInLaw? dropped from the output below --
  ; run-level constants, same value every row, matching both R ports
  ; (which never had these columns). infoList itself is left as-is
  ; (harmless unused parameter) rather than touching this procedure's
  ; call sites.
  ;
  ; 2026-08-18 (round 2, "exact parity"): dropped item kWork2_partner /
  ; item kWork2_children / item kDependency2_partner (confirmed exact
  ; duplicates -- see gMainFileHeader's dated comment above), and added
  ; familyId/status/age/activity/hasPartner/formalCareNeeded/
  ; nDependentChildren/nChildrenActivityC/nChildrenActivityP below, all
  ; reordered to match the new 65-column master order exactly (same
  ; order as computeCare_baseR.R's/computeCare_tidyverse.R's own output).
  ; Also: care2byPartner/care2byChildren (new columns this round) are NOT
  ; t_carebyPartner/t_carebyChildren under another name -- those are dead
  ; care1-only values here (always 0 under care2Only?=TRUE, same as
  ; carebyPartner/carebyChildren just above them). The REAL care2-sourced
  ; "care ego received from partner" / "from children" values already
  ; existed in t_care2CompCount all along, just never printed to this
  ; file under those names: kCare2EgoPToEgo and kCare2ChildToEgo. Using
  ; those here is the correct like-for-like pairing against both R
  ; scripts' own care2byPartner/care2byChildren (= their real
  ; carebyPartner/carebyChildren, duplicated under the "care2" name).

  ask agentEgo [

  ; 2026-08-18: nGrandChildren, added at Daniel's request for parity with
  ; both R ports (see gMainFileHeader's dated comment above for the full
  ; rationale/evidence on every column changed this round) -- ego's total
  ; grandchildren count, summed across children the same way
  ; care2Update's [CareUpdate.nls] own per-child myGrandChildren is
  ; computed, just rolled all the way up to the ego here (nobody
  ; previously did that roll-up in NetLogo either).
  let myNGrandChildren 0
  ask getChildrenSet self kCheckInLaws kActive [
    set myNGrandChildren myNGrandChildren + count (getChildrenSet self kCheckInLaws kActive)
  ]

  ; 2026-08-18: nDependentChildren / nChildrenActivityC / nChildrenActivityP,
  ; added for full NetLogo/R output parity -- each ego's currently-active
  ; BIO children's own dependency/activity breakdown (in-laws excluded,
  ; kNoInLaws, matching this script's use of kNoInLaws elsewhere for
  ; children-only counts, e.g. nActiveChildren/t_nActiveChildren itself).
  ; Mirrors computeCare_baseR.R's/computeCare_tidyverse.R's identical
  ; nDependentChildren/nChildrenActivityC/nChildrenActivityP columns.
  let myActiveChildren getChildrenSet self kNoInLaws kActive
  let myNDependentChildren 0
  let myNChildrenActivityC 0
  let myNChildrenActivityP 0
  ask myActiveChildren [
    if t_dependency > 0 [ set myNDependentChildren myNDependentChildren + 1 ]
    if t_activity = "C" [ set myNChildrenActivityC myNChildrenActivityC + 1 ]
    if t_activity = "P" [ set myNChildrenActivityP myNChildrenActivityP + 1 ]
  ]

  ; 2026-08-18: formalCareNeeded, added for full NetLogo/R output parity
  ; -- max(0, t_care2Demand), NOT max(0, t_netCareDemand): under Daniel's
  ; actual care2Only?=TRUE run config, t_netCareDemand is dead care1-only
  ; code (only ever touched by checkCare/explore_netCare_Parents, which
  ; never run in this mode), while t_care2Demand is the REAL, active,
  ; post-cascade leftover demand (decremented by care2firstCareSecond as
  ; care is actually transferred -- see gMainFileHeader's dated comment
  ; and computeCare_baseR.R's care2Demand/care2Supply comment for the
  ; full trace). Mirrors both R ports' own formalCareNeeded =
  ; pmax(netCareDemand, 0), which is ALREADY computed from their real,
  ; active netCareDemand (this script only ever implements the care2
  ; algorithm) -- so this is the correct like-for-like pairing, not
  ; NetLogo's own plain t_netCareDemand.
  let myFormalCareNeeded (max (list 0 t_care2Demand))

  file-print (word
ticks SEP
who SEP
t_idfamily SEP
t_gender SEP
t_status SEP
t_age SEP
t_dependency SEP
t_activity SEP
t_hasPartnerEgo SEP
myNGrandChildren SEP
t_maritalStatus SEP
t_nPartner SEP
t_nActivePartner SEP
t_nFutureActivePartner SEP
t_nDeadPartner SEP
t_nChildren SEP
t_nActiveChildren SEP
t_nFutureActiveChildren SEP
t_nDeadChildren SEP
myNDependentChildren SEP
myNChildrenActivityC SEP
myNChildrenActivityP SEP
t_nInLaws SEP
t_nActiveInLaws SEP
t_nFutureActiveInLaws SEP
t_nDeadInLaws SEP
t_care SEP
t_netCareDemand SEP
t_netCareSupply SEP
myFormalCareNeeded SEP
t_care2Demand SEP
t_care2Supply SEP
t_carebyPartner SEP
t_carebyChildren SEP
item kCare2EgoPToEgo t_care2CompCount SEP
item kCare2ChildToEgo t_care2CompCount SEP
item kCare2EgoPToChild t_care2CompCount SEP
item kCare2ChildToEgoP t_care2CompCount SEP
item kCare2ChildToPartner t_care2CompCount SEP
item kCare2ChildToSibling t_care2CompCount SEP
item kCare2ChildToGrandChild t_care2CompCount SEP
item kPartnerHours t_ego-accounts SEP
item kTotalHours t_ego-accounts SEP
item kTotalHoursLivings t_ego-accounts SEP
item kTotalAvailable t_ego-accounts SEP
item kTotalDependency t_ego-accounts SEP
item kTotalWork t_ego-accounts SEP
item kPartnerHoursLiving t_ego-accounts SEP
item kPartnerAvailable t_ego-accounts SEP
item kPartnerDependency t_ego-accounts SEP
item kPartnerWork t_ego-accounts SEP
item kPartnerOthers t_ego-accounts SEP
item kNoPartner_mort t_ego-accounts SEP
item kChildren_fert t_ego-accounts SEP
item kNoChildren_mort t_ego-accounts SEP
item kWork_partner t_ego-accounts SEP
item kWork_children t_ego-accounts SEP
item kDependency_partner t_ego-accounts SEP
item kDependency_children t_ego-accounts SEP
item kDependency2_health_children t_ego-accounts SEP
item kDependency2_age_children t_ego-accounts SEP
item kCompetition_partner t_ego-accounts SEP
item kCompetition_children t_ego-accounts SEP
item kCompetition_children_family t_ego-accounts SEP
item kDeficit t_ego-accounts
)
  ]
end

to UpdateSurvival
  ; BUG FIX (2026-08-16): this used to be `set t_age t_age + 1`, incrementing
  ; a running total that was only ever initialized once, at population
  ; creation, from the raw "age" column (PopulationCreator.nls). Two real
  ; bugs followed from that: (1) t_age was never advanced while a turtle
  ; sat on the `inactives` breed waiting for its tickIn (nothing touches
  ; t_age except this procedure, and this procedure only runs on `actives`)
  ; -- so anyone who joins a family after tick 0 (tickIn>0, e.g. a partner
  ; marrying in) had their age silently frozen for their entire dormant
  ; period, then resumed counting from that stale baseline once activated:
  ; verified against the real population file this made such individuals
  ; up to (tickIn-1) years too YOUNG (median ~12.5 years, max 60) for their
  ; whole active lifetime, ~18% of the population; (2) even for tickIn==0
  ; agents, incrementing t_age here means it's already been bumped for this
  ; tick BEFORE UpdateCare runs below, i.e. everyone was treated as 1 year
  ; OLDER than their `ageDef`/`tickOut`-implied age at every tick.
  ; Fix: compute t_age directly and independently every active tick from
  ; the always-reliable ageDef/tickOut/ticks trio (no running state, so no
  ; freeze-while-dormant possible), the same formula the R port already
  ; uses (`ageDef - (tickOut - t)`), and stop 1 tick short of ageDef on
  ; this, the last tick this individual computes anything (they reach
  ; ageDef only at tickOut itself, which they never live to compute, since
  ; the `die` below fires first on that tick).
  set t_age (t_ageDef - (tickOut - ticks))
  if t_age = 75 [
    set t_activity "A"
  ]

  let agentMarStat t_maritalStatus
  ask partner-neighbors [
    if is-active? self [
      set agentMarStat kMarried
    ]
  ]
  set t_maritalStatus agentMarStat

  if ticks >= tickOut [
    ;print (word "Agent " who " died")
    ask partner-neighbors [
      set t_maritalStatus kWidowed
    ]
    die
  ]
end

; daniel nov-dec 2017
to outputDependency
 (foreach ["M" "F"] [sex ->
  (foreach ["B" "M" "A"] [niv ->
   let d0 count actives with [t_isEgo and t_gender = sex and t_dependency = 0 and t_status = niv]
   let d1 count actives with [t_isEgo and t_gender = sex and t_dependency = 1 and t_status = niv]
   let d2 count actives with [t_isEgo and t_gender = sex and t_dependency = 2 and t_status = niv]
   let d3 count actives with [t_isEgo and t_gender = sex and t_dependency = 3 and t_status = niv]
   file-print (word ticks SEP sex SEP niv SEP d0 SEP d1 SEP d2 SEP d3)
  ])
 ])
end

to outputActivity [turtleSet]
 (foreach ["M" "F"] [sex ->
  (foreach ["B" "M" "A"] [niv ->
   (foreach [0 1 2 3] [dep ->
    let acA count turtleSet with [t_isEgo and t_gender = sex and t_activityRecoded = 0 and t_status = niv and t_dependency = dep]
    let acC count turtleSet with [t_isEgo and t_gender = sex and t_activityRecoded = 1 and t_status = niv and t_dependency = dep]
    let acP count turtleSet with [t_isEgo and t_gender = sex and t_activityRecoded = 2 and t_status = niv and t_dependency = dep]
    file-print (word ticks SEP sex SEP niv SEP dep SEP acA SEP acC SEP acP )
   ])
  ])
 ])
end

to outputSetup
  file-print (word "age" SEP "sex" SEP "niv" SEP "dep" SEP "A" SEP "C" SEP "P")
  let indAge 0
  while[indAge < 105]
  [
    (foreach ["M" "F"] [sex ->
     (foreach ["B" "M" "A"] [niv ->
      (foreach [0 1 2 3] [dep ->
       let acA count turtles with [t_age = indAge and t_gender = sex and t_activityRecoded = 0 and t_status = niv and t_dependency = dep]
       let acC count turtles with [t_age = indAge and t_gender = sex and t_activityRecoded = 1 and t_status = niv and t_dependency = dep]
       let acP count turtles with [t_age = indAge and t_gender = sex and t_activityRecoded = 2 and t_status = niv and t_dependency = dep]
       file-print (word indAge SEP sex SEP niv SEP dep SEP acA SEP acC SEP acP )
      ])
     ])
    ])
    set indAge indAge + 1
  ]
  file-close
  print "Setup data written!"
end
; end daniel
@#$#@#$#@
GRAPHICS-WINDOW
230
10
583
364
-1
-1
10.455
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

PLOT
1215
10
1595
227
Ego Dependency %
NIL
NIL
1.0
50.0
0.0
100.0
false
true
"" ""
PENS
"Level 0" 1.0 0 -13840069 true "" "plot plot_d0"
"Level 1" 1.0 0 -13345367 true "" "plot plot_d1"
":evel 2" 1.0 0 -2674135 true "" "plot plot_d2"
"Level 3" 1.0 0 -7500403 true "" "plot plot_d3"

BUTTON
100
200
155
233
NIL
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
170
205
230
238
goTicks
resetTimer\nifelse goTicks > 0 [\n  multGo goTicks\n][\n  if not go [stop]\n]\nset goTicks 0\nshowTimer
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
110
15
165
48
goAll
if (gTimer = 0) [resetTimer]\nif not go [\n  showTimer\n  stop\n]\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
110
580
185
613
View family
viewFamily \"list\"\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
0
660
105
693
outputs?
outputs?
0
1
-1000

SWITCH
0
885
117
918
showInfo?
showInfo?
1
1
-1000

CHOOSER
0
200
100
245
inputKin
inputKin
"Other" "cohort_08" "cohort_18" "cohort_28" "cohort_38" "cohort_48" "cohort_58" "cohort_68" "cohort_69" "cohort_70"
0

SWITCH
640
900
745
933
bigData?
bigData?
0
1
-1000

CHOOSER
0
750
105
795
outputKind
outputKind
"dependency" "activity" "activityUpdate" "ego" "partnerEgo" "childrenEgo" "setup" "all"
3

SWITCH
0
815
117
848
depTrans?
depTrans?
0
1
-1000

SWITCH
0
850
117
883
actTrans?
actTrans?
0
1
-1000

TEXTBOX
3
795
145
822
Debugging switches
12
15.0
1

SWITCH
0
365
125
398
idInd?
idInd?
0
1
-1000

INPUTBOX
0
425
65
485
famNum
1476.0
1
0
Number

OUTPUT
230
370
1855
820
13

CHOOSER
125
365
220
410
egoDependency
egoDependency
0 1 2 3 "all"
4

SWITCH
0
395
125
428
showInactives?
showInactives?
1
1
-1000

CHOOSER
125
410
220
455
partnerEgoDep
partnerEgoDep
0 1 2 3 "all"
4

SWITCH
125
500
220
533
childDep
childDep
1
1
-1000

MONITOR
125
530
220
575
gNumFamSel
gNumFamSel
17
1
11

MONITOR
170
295
230
340
ticks
ticks
17
1
11

CHOOSER
125
455
220
500
inLawDep
inLawDep
0 1 2 3 "all"
0

INPUTBOX
170
235
230
295
goTicks
0.0
1
0
Number

PLOT
830
10
1210
225
Care
NIL
NIL
0.0
50.0
0.0
50.0
false
true
"" ""
PENS
"formal" 1.0 0 -13840069 true "" "plot mean [t_netCareDemand] of actives with [t_isEgo]"
"informal" 1.0 0 -13345367 true "" "plot mean [t_carebyRelatives] of actives with [t_isEgo]"
"total" 1.0 0 -2674135 true "" "plot mean [t_careDemand] of actives with [t_isEgo]"

PLOT
625
170
825
320
informal care needed
hours of care
egos
0.0
100.0
0.0
500.0
false
false
"set-plot-x-range 0 100\nset-plot-y-range 0 1000\nset-plot-pen-mode 1\nset-histogram-num-bars 10\n\n" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [t_netCareDemand] of turtles with [t_isEgo]"

PLOT
620
10
820
160
nEgo
NIL
NIL
0.0
50.0
0.0
11000.0
false
false
"" ""
PENS
"egos" 1.0 0 -16777216 true "" "plot count actives with [t_isEgo]"

CHOOSER
640
830
745
875
educKind
educKind
"stochastic" "individual" "intrafamily"
1

SWITCH
0
690
105
723
askName?
askName?
1
1
-1000

SWITCH
0
720
105
753
fileAppend?
fileAppend?
1
1
-1000

INPUTBOX
60
425
125
485
Individual
0.0
1
0
Number

SWITCH
640
870
745
903
varFert?
varFert?
0
1
-1000

BUTTON
245
845
465
881
All birth cohorts (1908-1968)
batch_files
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
5
620
212
653
CareFromChildrenInLaw?
CareFromChildrenInLaw?
1
1
-1000

BUTTON
470
845
587
880
bootstrap list
bootstrapList
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
0
275
115
308
bs_X
bs_X
1
20
20.0
1
1
NIL
HORIZONTAL

SLIDER
0
305
115
338
bs_N
bs_N
1
20
20.0
1
1
NIL
HORIZONTAL

BUTTON
0
245
115
278
Bootstrap inputKin
bootstrap_inputKin
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
115
660
207
705
depTrans
depTrans
"main" "1->2" "1->3" "0->2"
0

BUTTON
5
580
105
613
View tree care2
ViewFamily \"tree\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
5
485
105
518
delFam?
delFam?
1
1
-1000

SWITCH
5
515
105
548
debug?
debug?
1
1
-1000

SWITCH
5
545
105
578
Comp?
Comp?
1
1
-1000

SWITCH
115
720
210
753
reWrite?
reWrite?
1
1
-1000

INPUTBOX
115
275
170
340
depSim
1.0
1
0
Number

INPUTBOX
750
830
820
890
User
Daniel
1
0
String

INPUTBOX
5
80
205
140
current_input
KINFERT_
1
0
String

TEXTBOX
70
345
220
363
Debug panel
12
15.0
0

SWITCH
0
920
115
953
care2Only?
care2Only?
0
1
-1000

BUTTON
5
15
102
48
Choose file
chooseFile
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
5
50
205
83
OutputDirSameAsInput
OutputDirSameAsInput
0
1
-1000

TEXTBOX
45
180
195
198
Old Democare interface
11
15.0
1

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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
1
@#$#@#$#@
