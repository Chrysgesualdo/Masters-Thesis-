* Encoding: UTF-8.
*Descriptive Stats: FREQUENCIES of demographic variables and measures.

*for entire sample N=90.
FREQUENCIES VARIABLES=Gender Age Born Condition Program Education Had_Sex Language Religion SESTI_Total Post_SESTI_Total SSBQ_Total Post_SSBQ_Total PTSRC_Total 
    Post_PTSRC_Total PMI_Total Post_PMI_Total PRS_Total Post_PRS_Total
  /NTILES=4
  /STATISTICS=STDDEV VARIANCE MEAN MEDIAN MODE SKEWNESS SESKEW KURTOSIS SEKURT
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

*for A Ganar entire group.
USE ALL.
COMPUTE filter_$=(Condition <= 2).
VARIABLE LABELS filter_$ 'Condition <= 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
FREQUENCIES VARIABLES=Gender Age Born Condition Program Education Had_Sex Language Religion SESTI_Total Post_SESTI_Total SSBQ_Total Post_SSBQ_Total PTSRC_Total 
    Post_PTSRC_Total PMI_Total Post_PMI_Total PRS_Total Post_PRS_Total 
  /NTILES=4
  /NTILES=4
  /STATISTICS=STDDEV VARIANCE MEAN MEDIAN MODE SKEWNESS SESKEW KURTOSIS SEKURT
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

*for UNICA entire group.
USE ALL.
COMPUTE filter_$=(Condition  >= 3).
VARIABLE LABELS filter_$ 'Condition  >= 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
FREQUENCIES VARIABLES=Gender Age Born Condition Program Education Had_Sex Language Religion SESTI_Total Post_SESTI_Total SSBQ_Total Post_SSBQ_Total PTSRC_Total 
    Post_PTSRC_Total PMI_Total Post_PMI_Total PRS_Total Post_PRS_Total 
  /NTILES=4
  /NTILES=4
  /STATISTICS=STDDEV VARIANCE MEAN MEDIAN MODE SKEWNESS SESKEW KURTOSIS SEKURT
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

*for Experimental A Ganar.
USE ALL. 
COMPUTE filter_$=(Condition   = 1). 
VARIABLE LABELS filter_$ 'Condition   = 1 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMATS filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE. 
FREQUENCIES VARIABLES=Gender Age Born Condition Program Education Had_Sex Language Religion SESTI_Total Post_SESTI_Total SSBQ_Total Post_SSBQ_Total PTSRC_Total 
    Post_PTSRC_Total PMI_Total Post_PMI_Total PRS_Total Post_PRS_Total 
  /NTILES=4
  /NTILES=4 
  /STATISTICS=STDDEV VARIANCE MEAN MEDIAN MODE SKEWNESS SESKEW KURTOSIS SEKURT 
  /HISTOGRAM NORMAL 
  /ORDER=ANALYSIS.

*for Control A Ganar.
USE ALL.
COMPUTE filter_$=(Condition   = 2).
VARIABLE LABELS filter_$ 'Condition   = 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
FREQUENCIES VARIABLES=Gender Age Born Condition Program Education Had_Sex Language Religion SESTI_Total Post_SESTI_Total SSBQ_Total Post_SSBQ_Total PTSRC_Total 
    Post_PTSRC_Total PMI_Total Post_PMI_Total PRS_Total Post_PRS_Total 
  /NTILES=4
  /NTILES=4
  /STATISTICS=STDDEV VARIANCE MEAN MEDIAN MODE SKEWNESS SESKEW KURTOSIS SEKURT
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

*for Experimental UNICA.
USE ALL.
COMPUTE filter_$=(Condition   = 3).
VARIABLE LABELS filter_$ 'Condition   = 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
FREQUENCIES VARIABLES=Gender Age Born Condition Program Education Had_Sex Language Religion SESTI_Total Post_SESTI_Total SSBQ_Total Post_SSBQ_Total PTSRC_Total 
    Post_PTSRC_Total PMI_Total Post_PMI_Total PRS_Total Post_PRS_Total 
  /NTILES=4
  /NTILES=4
  /STATISTICS=STDDEV VARIANCE MEAN MEDIAN MODE SKEWNESS SESKEW KURTOSIS SEKURT
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

*for Control UNICA.
USE ALL.
COMPUTE filter_$=(Condition   = 4).
VARIABLE LABELS filter_$ 'Condition   = 4 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
FREQUENCIES VARIABLES=Gender Age Born Condition Program Education Had_Sex Language Religion SESTI_Total Post_SESTI_Total SSBQ_Total Post_SSBQ_Total PTSRC_Total 
    Post_PTSRC_Total PMI_Total Post_PMI_Total PRS_Total Post_PRS_Total 
  /NTILES=4
  /NTILES=4
  /STATISTICS=STDDEV VARIANCE MEAN MEDIAN MODE SKEWNESS SESKEW KURTOSIS SEKURT
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.

*K-S Normality Tests for Dependent Variables (general and by program).

*for SE STI/HIV.
EXAMINE VARIABLES=SESTI_Total Post_SESTI_Total
  /PLOT BOXPLOT HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

*for SSBQ.
EXAMINE VARIABLES=SSBQ_Total Post_SSBQ_Total
  /PLOT BOXPLOT HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

*T-tests.

*t-test UNICA.
USE ALL.
COMPUTE filter_$=(Program = 2).
VARIABLE LABELS filter_$ 'Program = 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
T-TEST GROUPS=Condition(3 4)
  /MISSING=ANALYSIS
  /VARIABLES=Age Born Education Religion Language
  /CRITERIA=CI(.95).

*t-test A Ganar.
USE ALL.
COMPUTE filter_$=(Program = 1).
VARIABLE LABELS filter_$ 'Program = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
T-TEST GROUPS=Condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=Age Born Education Religion Language
  /CRITERIA=CI(.95).


FILTER OFF.
USE ALL.
EXECUTE.

*ANCOVA RQ1 (SE STI/HIV).

*for A GANAR RQ1.

USE ALL.
COMPUTE filter_$=(Program = 1).
VARIABLE LABELS filter_$ 'Program = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
UNIANOVA Post_SESTI_Total BY Condition WITH SESTI_Total Language Religion Education Born Age Gender
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=ZRESID
  /EMMEANS=TABLES(Condition) WITH(SESTI_Total=MEAN Language=MEAN Religion=MEAN Education=MEAN
    Born=MEAN Age=MEAN Gender=MEAN) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(OVERALL) WITH(SESTI_Total=MEAN Language=MEAN Religion=MEAN Education=MEAN
    Born=MEAN Age=MEAN Gender=MEAN)
  /PRINT ETASQ DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=SESTI_Total Language Religion Education Born Age Gender Condition.

*Normality.

EXAMINE VARIABLES=ZRE_2
  /PLOT BOXPLOT STEMLEAF NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

*Correlations of covariates.

DATASET ACTIVATE DataSet1.

SAVE OUTFILE='/Users/chrysgesualdo/Desktop/DATA/All Students .sav'
  /COMPRESSED.
CORRELATIONS
  /VARIABLES=Gender Age Born Education Religion Language SESTI_Total
  /PRINT=TWOTAIL NOSIG
  /STATISTICS XPROD
  /MISSING=PAIRWISE.

*for UNICA RQ1.

USE ALL.
COMPUTE filter_$=(Program = 2).
VARIABLE LABELS filter_$ 'Program = 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
UNIANOVA Post_SESTI_Total BY Condition WITH SESTI_Total Language Religion Education Born Age Gender
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=ZRESID
  /EMMEANS=TABLES(Condition) WITH(SESTI_Total=MEAN Language=MEAN Religion=MEAN Education=MEAN
    Born=MEAN Age=MEAN Gender=MEAN) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(OVERALL) WITH(SESTI_Total=MEAN Language=MEAN Religion=MEAN Education=MEAN
    Born=MEAN Age=MEAN Gender=MEAN)
  /PRINT ETASQ DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=SESTI_Total Language Religion Education Born Age Gender Condition.
 

*Normality.

EXAMINE VARIABLES=ZRE_3
  /PLOT BOXPLOT STEMLEAF NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.


*Correlations of covariates.

CORRELATIONS
  /VARIABLES=Gender Age Born Education Religion Language SESTI_Total
  /PRINT=TWOTAIL NOSIG
  /STATISTICS XPROD
  /MISSING=PAIRWISE.

FILTER OFF.
USE ALL.
EXECUTE.

*Chi Square (CS) for gender.
*CS gender per condition.

CROSSTABS
  /TABLES=Condition BY Gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CORR
  /CELLS=COUNT EXPECTED COLUMN TOTAL
  /COUNT ROUND CELL.

*CS gender per condition filtered by program (A Ganar, because only they have different genders).
USE ALL.
COMPUTE filter_$=(Program = 1).
VARIABLE LABELS filter_$ 'Program = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
CROSSTABS
  /TABLES=Condition BY Gender
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CORR
  /CELLS=COUNT EXPECTED COLUMN TOTAL
  /COUNT ROUND CELL.

*Paired Samples T-test for all measures.
SORT CASES  BY Program.
SPLIT FILE SEPARATE BY Program.
T-TEST PAIRS=SESTI_Total SSBQ_Total PTSRC_Total PMI_Total PRS_Total WITH Post_SESTI_Total
    Post_SSBQ_Total Post_PTSRC_Total Post_PMI_Total Post_PRS_Total (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS. 

SPLIT FILE OFF.

*Repeated Measures ANCOVA for H1.
*Split file RM ANCOVA H1.
GET
  FILE='/Users/chrysgesualdo/Desktop/DATA/All Students .sav'.
DATASET NAME DataSet1 WINDOW=FRONT.
SPLIT FILE LAYERED BY Program.
GLM SESTI_Total Post_SESTI_Total BY Condition WITH Program Gender Age Born Education Religion
    Language
  /WSFACTOR=time 2 Polynomial
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(OVERALL) WITH(Program=MEAN Gender=MEAN Age=MEAN Born=MEAN Education=MEAN
    Religion=MEAN Language=MEAN)
  /EMMEANS=TABLES(Condition) WITH(Program=MEAN Gender=MEAN Age=MEAN Born=MEAN Education=MEAN
    Religion=MEAN Language=MEAN)COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(time) WITH(Program=MEAN Gender=MEAN Age=MEAN Born=MEAN Education=MEAN
    Religion=MEAN Language=MEAN)COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(Condition*time) WITH(Program=MEAN Gender=MEAN Age=MEAN Born=MEAN Education=MEAN
    Religion=MEAN Language=MEAN)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=time
  /DESIGN=Program Gender Age Born Education Religion Language Condition.

*ANCOVA for RQ2 & H2.

*Filter per program RQ2 ANCOVA without ‘Has had sex’ as fixed factor.

*A Ganar ANCOVA RQ2.
USE ALL.
COMPUTE filter_$=(Program = 1).
VARIABLE LABELS filter_$ 'Program = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
UNIANOVA Post_SSBQ_Total BY  Condition WITH Gender Age Born Education Religion Language SSBQ_Total
  /RANDOM=Condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=ZRESID
  /EMMEANS=TABLES(OVERALL) WITH(Gender=MEAN Age=MEAN Born=MEAN Education=MEAN Religion=MEAN
    Language=MEAN SSBQ_Total=MEAN)
  /EMMEANS=TABLES(Condition) WITH(Gender=MEAN Age=MEAN Born=MEAN Education=MEAN Religion=MEAN
    Language=MEAN SSBQ_Total=MEAN) COMPARE ADJ(BONFERRONI)
  /PRINT ETASQ DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=Gender Age Born Education Religion Language SSBQ_Total Condition.


*Normality ANCOVA for A GANAR RQ2.
EXAMINE VARIABLES=ZRE_11
  /PLOT BOXPLOT STEMLEAF NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

CORRELATIONS
  /VARIABLES=Gender Age Born Education Religion Language SSBQ_Total
  /PRINT=TWOTAIL NOSIG
  /STATISTICS XPROD
  /MISSING=PAIRWISE.

*UNICA ANCOVA RQ2.
USE ALL.
COMPUTE filter_$=(Program = 2).
VARIABLE LABELS filter_$ 'Program = 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
UNIANOVA Post_SSBQ_Total BY  Condition WITH Gender Age Born Education Religion Language SSBQ_Total
  /RANDOM=Condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=ZRESID
  /EMMEANS=TABLES(OVERALL) WITH(Gender=MEAN Age=MEAN Born=MEAN Education=MEAN Religion=MEAN
    Language=MEAN SSBQ_Total=MEAN)
  /EMMEANS=TABLES(Condition) WITH(Gender=MEAN Age=MEAN Born=MEAN Education=MEAN Religion=MEAN
    Language=MEAN SSBQ_Total=MEAN) COMPARE ADJ(BONFERRONI)
  /PRINT ETASQ DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=Gender Age Born Education Religion Language SSBQ_Total Condition.

*Normality ANCOVA for UNICA RQ2.
EXAMINE VARIABLES=ZRE_12
  /PLOT BOXPLOT STEMLEAF NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

*Correlations of covariates ANCOVA for UNICA RQ2.
CORRELATIONS
  /VARIABLES=Gender Age Born Education Religion Language SSBQ_Total
  /PRINT=TWOTAIL NOSIG
  /STATISTICS XPROD
  /MISSING=PAIRWISE.
FILTER OFF.
USE ALL.
EXECUTE.

*Repeated measures ANCOVA for RQ3 & H3.

*split file by program (compare groups) without ‘program’ in b/w variable (reported this one).
SPLIT FILE LAYERED BY Program.
GLM SESTI_Total Post_SESTI_Total BY Condition WITH Age Born Education Religion Language
  /WSFACTOR=time 2 Polynomial
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Condition) WITH(Age=MEAN Born=MEAN Education=MEAN Religion=MEAN
    Language=MEAN)COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(time) WITH(Age=MEAN Born=MEAN Education=MEAN Religion=MEAN Language=MEAN)COMPARE
    ADJ(BONFERRONI)
  /EMMEANS=TABLES(Condition*time) WITH(Age=MEAN Born=MEAN Education=MEAN Religion=MEAN
    Language=MEAN)
  /PRINT=DESCRIPTIVE ETASQ TEST(MMATRIX)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=time
  /DESIGN=Age Born Education Religion Language Condition.

SPLIT FILE OFF.

*RM ANCOVA RQ-H-3 with ‘program’ in b/w variable.
GLM SESTI_Total Post_SESTI_Total BY Condition Program WITH Age
  /WSFACTOR=time 2 Polynomial
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(OVERALL) WITH(Age=MEAN)
  /EMMEANS=TABLES(Condition) WITH(Age=MEAN)COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(Program) WITH(Age=MEAN)COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(time) WITH(Age=MEAN)COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(Condition*Program) WITH(Age=MEAN)
  /EMMEANS=TABLES(Condition*time) WITH(Age=MEAN)
  /EMMEANS=TABLES(Program*time) WITH(Age=MEAN)
  /EMMEANS=TABLES(Condition*Program*time) WITH(Age=MEAN)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=time
  /DESIGN=Age Condition Program Condition*Program.


*Reliability of Scales.

*SE STI/HIV.
*N=90.
FILTER OFF.
USE ALL.
EXECUTE.
RELIABILITY
  /VARIABLES=SE1_Abstinence SE2_LoyaltyOne SE3_ProperCondomUse SE4_TeachCondom
    SE5_PartnerCommSafety SE6_AvoidDrugs SE7_AvoidAlcohol SE8_TalkSTIHIV SE9_FamilyTalkSTIHIV
    SE10_CommunityHIVSTI
  /SCALE('SE STI‎/HIV') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*PTSRC.
*N=90.
FILTER OFF.
USE ALL.
EXECUTE.
RELIABILITY
  /VARIABLES=PTSRC1_BC PTSRC2_STI PTSRC3_HIV PTSRC4_ProtectionSTIHIV PTSRC5_Condoms PTSRC6_Postpone
    PTSRC7_SexualPressure PTSRC8_PeerPressure
  /SCALE('PTSRC') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*PMI.
*1-4 N=90.
FILTER OFF.
USE ALL.
EXECUTE.
RELIABILITY
  /VARIABLES=PMI1_IndirectMoni PMI2_DirectMoni PMI3_SchoolMoni PMI4_HealthMoni
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
*5-7 N=90.
FILTER OFF.
USE ALL.
EXECUTE.
RELIABILITY
  /VARIABLES=PMI5_ComputerMoni PMI6_PhoneMoni PMI7_RestrictiveMoni
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*PRS.
*N=90.
FILTER OFF.
USE ALL.
EXECUTE.
RELIABILITY
  /VARIABLES=PRS1_proud PRS2_interest PRS3_listens PRS4_countOn PRS5_realTalk PRS6_comfortable
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*SSBQ.
*SSBQ N=90.
RECODE SSBQ2_UseDrugsIntercourse SSBQ7_SexFirstDate SSBQ13_OralNoCondom SSBQ14_ArousalNoCondom
    SSBQ15_Anal SSBQ20_hardSexTalk SSBQ22_SexWithGay SSBQ23_AnalNoCondom (1=4) (2=3) (3=2) (4=1) INTO
    rSSBQ2_UseDrugsIntercourse rSSBQ7_SexFirstDate rSSBQ13_OralNoCondom rSSBQ14_ArousalNoCondom
    rSSBQ15_Anal rSSBQ20_hardSexTalk rSSBQ22_SexWithGay rSSBQ23_AnalNoCondom.
EXECUTE.
DATASET ACTIVATE DataSet1.

SAVE OUTFILE='/Users/chrysgesualdo/Desktop/DATA/All Students .sav'
  /COMPRESSED.
RELIABILITY
  /VARIABLES=SSBQ1_insistCondom SSBQ3_ForeplayCondom SSBQ4_AskSexualHist SSBQ5_AvoidFuidContact
    SSBQ6_AskHomoPractices SSBQ8_NoHistNoSex SSBQ9_IrritationNoSex SSBQ10_Bringcondom
    SSBQ11_ExamineGenitalsPartner SSBQ12_ExpressSafeSexViews SSBQ16_DrugHistPartn
    SSBQ17_MentalPlanSafeSex SSBQ18_SayNoifNoCond SSBQ19_AvoidBlood SSBQ21_StartSexTalk
    rSSBQ2_UseDrugsIntercourse rSSBQ7_SexFirstDate rSSBQ13_OralNoCondom rSSBQ14_ArousalNoCondom
    rSSBQ15_Anal rSSBQ20_hardSexTalk rSSBQ22_SexWithGay rSSBQ23_AnalNoCondom
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

