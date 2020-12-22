!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: species_database_mod.F90
!
! !DESCRIPTION: Module SPECIES\_DATABASE\_MOD contains routines to set up
!  a database object containing physical properties for each GEOS-Chem
!  species.  This allows us to consolidate all species properties into a
!  single data structure, for convenience.
!\\
!\\
! !INTERFACE:
!
MODULE Species_Database_Mod
!
! !USES:
!
  USE Precision_Mod

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: Init_Species_Database
  PUBLIC  :: Cleanup_Species_Database

#if defined ( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING )
  !-----------------------------------------------------------------
  !         %%%%%%% GEOS-Chem HP (with ESMF & MPI) %%%%%%%
  !
  ! Cleanup routines for restoring the internal state of this
  ! module are exposed, so the DB can be reset from an external
  ! interface to perform multiple initializations of
  ! chemistry states. (hplin, 6/4/18)
  !-----------------------------------------------------------------
  PUBLIC  :: Cleanup_Work_Arrays
#endif
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: TranUc
!
! !REVISION HISTORY:
!  28 Aug 2015 - R. Yantosca - Initial version
!  See https://github.com/geoschem/geos-chem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

  ! Work array to hold the list of species names, which combines the advected
  ! species from input.geos with the KPP species names (and removes duplicates)
  CHARACTER(LEN=31), ALLOCATABLE :: Species_Names(:)

  ! Work array to hold the list of all KPP species indices
  ! (Non-KPP species are given missing values)
  INTEGER,           ALLOCATABLE :: KppSpcId(:)

  ! Work array to hold the list of KPP fixed species indices
  ! (Non-KPP species are given missing values)
  INTEGER,           ALLOCATABLE :: KppFixId(:)

  ! Work array to hold the unique list of KPP variable species indices
  ! (Non-KPP species are given missing values)
  INTEGER,           ALLOCATABLE :: KppVarId(:)

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_Species_Database
!
! !DESCRIPTION: Initializes the GEOS-Chem Species database from
!  YAML file format input.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Init_Species_Database( Input_Opt, SpcData, SpcCount, RC )
!
! !USES:
!
    USE ErrCode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE QFYAML_Mod
    USE Species_Mod
!
! !INPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(IN)    :: Input_Opt    ! Input Options object
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(SpcPtr),   POINTER       :: SpcData(:)   ! Species database object
    TYPE(SpcIndCt), INTENT(INOUT) :: SpcCount     ! Species index counters
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC           ! Success/failure
!
! !REMARKS:
!  Uses the QFYAML parser, see: https://github.com/yantosca/qfyaml
!
! !REVISION HISTORY:
!  23 Apr 2020 - R. Yantosca - Initial version
!  See the subsequent Git history with the gitk browser!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    LOGICAL                     :: prtDebug
    LOGICAL                     :: v_bool
    LOGICAL                     :: wd_kcscalefac_luo_found
    LOGICAL                     :: wd_rainouteff_luo_found
    INTEGER                     :: v_int
    INTEGER                     :: nSpecies
    INTEGER                     :: N
    INTEGER                     :: S
    REAL(f4)                    :: v_real

    ! Strings
    CHARACTER(LEN=14)           :: tag
    CHARACTER(LEN=31)           :: spc
    CHARACTER(LEN=255)          :: v_str
    CHARACTER(LEN=255)          :: key
    CHARACTER(LEN=255)          :: thisLoc
    CHARACTER(LEN=512)          :: errMsg

    ! Arrays
    REAL(f4)                    :: a_real_2(2)
    REAL(f4)                    :: a_real_3(3)
    REAL(f4)                    :: wd_kcscalefac_luo(3)
    REAL(f4)                    :: wd_rainouteff_luo(3)

    ! String arrays
    CHARACTER(LEN=17)           :: tags(41)

    ! Objects
    TYPE(QFYAML_t)              :: yml
    TYPE(Species),    POINTER   :: ThisSpc

    !=======================================================================
    ! Init_Species_Database begins here!
    !=======================================================================

    ! Initialize
    RC         = GC_SUCCESS
    prtDebug   = ( Input_Opt%LPRT .and. Input_Opt%amIRoot )
    errMsg     = ""
    thisLoc    = &
    " -> at Init_Species_Database (in module Headers/species_database_mod.F90"

    ! Zero counters
    SpcCount%nAdvect  = 0
    SpcCount%nAeroSpc = 0
    SpcCount%nDryAlt  = 0
    SpcCount%nDryDep  = 0
    SpcCount%nGasSpc  = 0
    SpcCount%nHygGrth = 0
    SpcCount%nKppVar  = 0
    SpcCount%nKppFix  = 0
    SpcCount%nKppSpc  = 0
    SpcCount%nPhotol  = 0
    SpcCount%nRadNucl = 0
    SpcCount%nWetDep  = 0
    SpcCount%nHg0     = 0
    SpcCount%nHg2     = 0
    SpcCount%nHgP     = 0

    ! Species database tags to match
    tags = (/"Background_VV    ",  &
             "DD_AeroDryDep    ",  &
             "DD_DustDryDep    ",  &
             "DD_DvzAerSnow    ",  &
             "DD_DvzMinVal     ",  &
             "DD_F0            ",  &
             "DD_Hstar         ",  &
             "DD_KOA           ",  &
             "Density          ",  &
             "Formula          ",  &
             "FullName         ",  &
             "Is_Aerosol       ",  &
             "Is_DryAlt        ",  &
             "Is_DryDep        ",  &
             "Is_HygroGrowth   ",  &
             "Is_Gas           ",  &
             "Is_Hg0           ",  &
             "Is_Hg2           ",  &
             "Is_HgP           ",  &
             "Is_Photolysis    ",  &
             "Is_RadioNuclide  ",  &
             "Is_WetDep        ",  &
             "Henry_CR         ",  &
             "Henry_K0         ",  &
             "Henry_pKa        ",  &
             "MP_SizeResAer    ",  &
             "MP_SizeResNum    ",  &
             "MW_g             ",  &
             "Radius           ",  &
             "WD_AerScavEff    ",  &
             "WD_CoarseAer     ",  &
             "WD_ConvFacI2G    ",  &
             "WD_KcScaleFac_Luo",  &
             "WD_KcScaleFac    ",  &
             "WD_Is_H2SO4      ",  &
             "WD_Is_HNO3       ",  &
             "WD_Is_SO2        ",  &
             "WD_LiqAndGas     ",  &
             "WD_RainoutEff_Luo",  &
             "WD_RainoutEff    ",  &
             "WD_RetFactor     "   /)

    !=======================================================================
    ! Store the list unique GEOS-Chem species names in work arrays for use
    ! below. This is the combined list of advected species (from input.geos)
    ! plus KPP species (from SPC_NAMES in gckpp_Monitor.F90), with all
    ! duplicates removed. Also stores the corresponding indices in the
    ! KPP VAR and FIX arrays.  For simulations that do not use KPP, the
    ! unique species list is the list of advected species from input.geos.
    !=======================================================================
    CALL Unique_Species_Names( Input_Opt, nSpecies, RC )
    IF ( RC /= GC_SUCCESS ) THEN
       errMsg = "Could not determine species names!"
       CALL GC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    ! Initialize the species database vector and
    ! set all tags for each species to missing values
    CALL SpcData_Init( Input_Opt, nSpecies, SpcData, RC )
    IF ( RC /= GC_SUCCESS ) THEN
       errMsg = "Could not initialize the species database object!"
       CALL GC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !=======================================================================
    ! Read the species metadata from YAML files into a QFYAML object
    !=======================================================================
    CALL Read_Species_Database( Input_Opt, yml, RC )
    IF ( RC /= GC_SUCCESS ) THEN
       errMsg = 'Error encountered in routine "Read_Species_Database"!'
       CALL GC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF

    !=======================================================================
    ! Extract species metadata and store in the Species Database object
    !=======================================================================

    ! Loop over the number of species
    DO S = 1, nSpecies

       ! Look up this species in the species database
       ThisSpc => SpcData(S)%Info

       !--------------------------------------------------------------------
       ! Set the Name and ModelID tags
       !-------------------------------------------------------------------
       spc             = species_names(S)
       ThisSpc%Name    = TRIM( spc )
       ThisSpc%ModelId = S

       !--------------------------------------------------------------------
       ! Set the Is_Advected tag (check against Input_Opt%AdvecSpc list)
       !-------------------------------------------------------------------
       v_bool = ANY( Input_Opt%AdvectSpc_Name == spc )
       IF ( v_bool ) THEN
          SpcCount%nAdvect    = SpcCount%nAdvect + 1
          ThisSpc%AdvectId    = SpcCount%nAdvect
          ThisSpc%Is_Advected = v_bool
       ENDIF

       !--------------------------------------------------------------------
       ! Set tags for species in the KPP mechanism
       !-------------------------------------------------------------------

       ! Is this species in the KPP mechanism?
       IF ( KppSpcId(S) > 0 ) THEN
          SpcCount%nKppSpc = SpcCount%nKppSpc + 1
          ThisSpc%KppSpcId = KppSpcId(S)
       ENDIF

       ! Is this species an active KPP species?
       IF ( KppVarId(S) > 0 ) THEN
          SpcCount%nKppVar = SpcCount%nKppVar + 1
          ThisSpc%KppVarId = KppVarId(S)
       ENDIF

       ! Is this species a fixed KPP species?
       IF ( KppFixId(S) > 0 ) THEN
          SpcCount%nKppFix = SpcCount%nKppFix + 1
          ThisSpc%KppFixId = KppFixId(S)
       ENDIF

       ! Is the species part of the KPP chemical mechanism?
       ThisSpc%Is_Kpp = ( ThisSpc%KppVarId > 0  .or. ThisSpc%KppFixId > 0 )

       ! Is the species an active or fixed species in the chemical mechanism?
       ThisSpc%Is_ActiveChem = ( ThisSpc%KppVarId >  0 .and.                 &
                                 ThisSpc%KppFixId <= 0                      )
       ThisSpc%Is_FixedChem  = ( ThisSpc%KppFixId >  0                      )

       !--------------------------------------------------------------------
       ! Initialize found flags
       !-------------------------------------------------------------------
       wd_kcscalefac_luo_found = .FALSE.
       wd_rainouteff_luo_found = .FALSE.

       !--------------------------------------------------------------------
       ! Loop over the remaining tags in the species database and
       ! copy values from the QFYAML object to the SpcData object
       !--------------------------------------------------------------------
       DO N = 1, SIZE( tags )

          ! Set intial values to default "missing" values
          ! If the tag isn't found for a given species, then
          ! it will be given the appropriate missing value.
          a_real_2 = MISSING_R4
          a_real_3 = MISSING_R4
          v_bool   = MISSING_BOOL
          v_int    = MISSING_INT
          v_real   = MISSING_R4
          v_str    = MISSING_STR

          ! Create search key for each variable
          key = TRIM( spc ) // '%' // TRIM( tags(N) )

          ! Save into the proper field of the species database
          ! NOTE: Attempt to round off values to 2 decimal places,
          ! unless the values can be either too large or too small
          ! for the roundoff algorithm.
          IF ( INDEX( key, "%Background_VV" ) > 0 ) THEN
             v_real = MISSING_VV
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%BackgroundVV = DBLE( v_real )   ! Don't round off

          ELSE IF ( INDEX( key, "%DD_AeroDryDep" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%DD_AeroDryDep = v_bool

          ELSE IF ( INDEX( key, "%DD_DustDryDep" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%DD_DustDryDep = v_bool

          ELSE IF ( INDEX( key, "%DD_DvzAerSnow" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%DD_DvzAerSnow = Cast_and_RoundOff( v_real )

          ELSE IF ( INDEX( key, "%DD_DvzMinVal" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, a_real_2, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%DD_DvzMinVal(1) = Cast_and_RoundOff( a_real_2(1) )
             ThisSpc%DD_DvzMinVal(2) = Cast_and_RoundOff( a_real_2(2) )

          ELSE IF ( INDEX( key, "%DD_F0" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%DD_F0 = DBLE( v_real )          ! Don't round off

          ELSE IF ( INDEX( key, "%DD_Hstar" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%DD_Hstar = DBLE( v_real )       ! Don't round off

          ELSE IF ( INDEX( key, "%DD_KOA" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%DD_KOA = DBLE( v_real )       ! Don't round off

          ELSE IF ( INDEX( key, "%Density" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%Density = Cast_and_RoundOff( v_real )

          ELSE IF ( INDEX( key, "%Formula" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%Formula = TRIM( v_str )

          ELSE IF ( INDEX( key, "%FullName" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%FullName = TRIM( v_str )

          ELSE IF ( INDEX( key, "%Henry_CR" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%Henry_CR = DBLE( v_real )       ! Don't round off

          ELSE IF ( INDEX( key, "%Henry_K0" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%Henry_K0 = DBLE( v_real )       ! Don't round off

          ELSE IF ( INDEX( key, "%Is_Aerosol" ) > 0  ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nAeroSpc  = SpcCount%nAeroSpc + 1
                ThisSpc%AerosolId  = SpcCount%nAeroSpc
                ThisSpc%Is_Aerosol = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_DryAlt" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nDryAlt  = SpcCount%nDryAlt + 1
                ThisSpc%DryAltId  = SpcCount%nDryAlt
                ThisSpc%Is_DryAlt = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_DryDep" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nDryDep  = SpcCount%nDryDep + 1
                ThisSpc%DryDepId  = SpcCount%nDryDep
                ThisSpc%Is_DryDep = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_HygroGrowth" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nHygGrth      = SpcCount%nHygGrth + 1
                ThisSpc%HygGrthId      = SpcCount%nHygGrth
                ThisSpc%Is_HygroGrowth = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_Gas" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nGasSpc = SpcCount%nGasSpc + 1
                ThisSpc%GasSpcId = SpcCount%nGasSpc
                ThisSpc%Is_Gas   = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_Hg0" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nHg0  = SpcCount%nHg0 + 1
                ThisSpc%Hg_Cat = SpcCount%nHg0
                ThisSpc%Is_Hg0 = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_Hg2" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nHg2  = SpcCount%nHg2 + 1
                ThisSpc%Hg_Cat = SpcCount%nHg2
                ThisSpc%Is_Hg2 = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_HgP" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nHgP  = SpcCount%nHgP + 1
                ThisSpc%Hg_Cat = SpcCount%nHgP
                ThisSpc%Is_HgP = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_Photolysis" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nPhotol      = SpcCount%nPhotol + 1
                ThisSpc%PhotolId      = SpcCount%nPhotol
                ThisSpc%Is_Photolysis = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_RadioNuclide" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nRadNucl       = SpcCount%nRadNucl + 1
                ThisSpc%RadNuclId       = SpcCount%nRadNucl
                ThisSpc%Is_RadioNuclide = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%Is_WetDep" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             IF ( v_bool ) THEN
                SpcCount%nWetDep  = SpcCount%nWetDep + 1
                ThisSpc%WetDepID  = SpcCount%nWetDep
                ThisSpc%Is_WetDep = v_bool
             ENDIF

          ELSE IF ( INDEX( key, "%MP_SizeResAer" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%MP_SizeResAer = v_bool

          ELSE IF ( INDEX( key, "%MP_SizeResNum" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%MP_SizeResNum = v_bool

          ELSE IF ( INDEX( key, "%MW_g" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%MW_g = Cast_and_RoundOff( v_real )

          ELSE IF ( INDEX( key, "%Radius" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%Radius = DBLE( v_real )         ! Don't round off

          ELSE IF ( INDEX( key, "%WD_AerScavEff" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_AerScavEff = Cast_and_RoundOff( v_real )

          ELSE IF ( INDEX( key, "%WD_CoarseAer" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_CoarseAer = v_bool

          ELSE IF ( INDEX( key, "%WD_ConvFacI2G" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_ConvFacI2G = DBLE( v_real )  ! Don't round off

          ELSE IF ( INDEX( key, "%WD_KcScaleFac_Luo" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, a_real_3, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             wd_kcscalefac_luo(1) = Cast_and_RoundOff( a_real_3(1) )
             wd_kcscalefac_luo(2) = Cast_and_RoundOff( a_real_3(2) )
             wd_kcscalefac_luo(3) = Cast_and_RoundOff( a_real_3(3) )
             IF ( wd_kcscalefac_luo(1) /= MISSING_R4 ) THEN
                wd_kcscalefac_luo_found = .TRUE.
             ENDIF

          ELSE IF ( INDEX( key, "%WD_KcScaleFac" ) > 0 .AND. &
                    INDEX( key, "Luo" ) <= 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, a_real_3, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_KcScaleFac(1) = Cast_and_RoundOff( a_real_3(1) )
             ThisSpc%WD_KcScaleFac(2) = Cast_and_RoundOff( a_real_3(2) )
             ThisSpc%WD_KcScaleFac(3) = Cast_and_RoundOff( a_real_3(3) )

          ELSE IF ( INDEX( key, "%WD_Is_H2SO4" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_Is_H2SO4 = v_bool

          ELSE IF ( INDEX( key, "%WD_Is_HNO3" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_Is_HNO3 = v_bool

          ELSE IF ( INDEX( key, "%WD_Is_SO2" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_Is_SO2 = v_bool

          ELSE IF ( INDEX( key, "%WD_LiqAndGas" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_LiqAndGas = v_bool

          ELSE IF ( INDEX( key, "%WD_RainoutEff_Luo" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, a_real_3, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             wd_rainouteff_luo(1) = Cast_and_RoundOff( a_real_3(1) )
             wd_rainouteff_luo(2) = Cast_and_RoundOff( a_real_3(2) )
             wd_rainouteff_luo(3) = Cast_and_RoundOff( a_real_3(3) )
             IF ( wd_rainouteff_luo(1) /= MISSING_R4 ) THEN
                wd_rainouteff_luo_found = .TRUE.
             ENDIF

          ELSE IF ( INDEX( key, "%WD_RainoutEff" ) > 0 .AND. &
                    INDEX( key, "Luo" ) <= 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, a_real_3, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_RainoutEff(1) = Cast_and_RoundOff( a_real_3(1) )
             ThisSpc%WD_RainoutEff(2) = Cast_and_RoundOff( a_real_3(2) )
             ThisSpc%WD_RainoutEff(3) = Cast_and_RoundOff( a_real_3(3) )

          ELSE IF ( INDEX( key, "%WD_RetFactor" ) > 0 ) THEN
             CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
             IF ( RC /= GC_SUCCESS ) GOTO 999
             ThisSpc%WD_RetFactor = Cast_and_RoundOff( v_real )

          ELSE
             ! Pass

          ENDIF

       ENDDO

#ifdef LUO_WETDEP
       ! Overwrite with special values if present in file
       IF ( wd_kcscalefac_luo_found ) THEN
          ThisSpc%WD_KcScaleFac(1) = wd_kcscalefac_luo(1)
          ThisSpc%WD_KcScaleFac(2) = wd_kcscalefac_luo(2)
          ThisSpc%WD_KcScaleFac(3) = wd_kcscalefac_luo(3)
       ENDIF
       IF ( wd_rainouteff_luo_found ) THEN
          ThisSpc%WD_RainoutEff(1) = wd_rainouteff_luo(1)
          ThisSpc%WD_RainoutEff(2) = wd_rainouteff_luo(2)
          ThisSpc%WD_RainoutEff(3) = wd_rainouteff_luo(3)
       ENDIF
#endif

       !--------------------------------------------------------------------
       ! SANITY CHECKS
       !--------------------------------------------------------------------

       ! Is_Gas and Is_Aero tags cannot both be TRUE at the same time
       IF ( ThisSpc%Is_Gas .and. ThisSpc%Is_Aerosol ) THEN
          errMsg = "Is_Gas and Is_Aerosol are both TRUE for species "     // &
                   TRIM( spc ) // "!"
          CALL GC_Error( errMsg, RC, thisLoc )
          RETURN
       ENDIF

       ! Is_Gas and Is_Aero tags cannot both be FALSE at the same time
       IF ( ( .not. ThisSpc%Is_Gas ) .and. ( .not. ThisSpc%Is_Aerosol ) ) THEN
          errMsg = "Is_Gas and Is_Aerosol are both FALSE for species "    // &
                   TRIM( spc ) // "!"
          CALL GC_Error( errMsg, RC, thisLoc )
          RETURN
       ENDIF

       ! If the species is a gas, set all aerosol fields to missing values
       IF ( ThisSpc%Is_Gas ) THEN

          SELECT CASE( TRIM( spc ) )
             CASE( 'H2SO4' )
                ! H2SO4 are gases that wetdep like aerosols,
                ! so keep both all gas and aerosol properties.
             CASE( 'HNO3', 'SO2' )
                ! HNO3 and SO2 drydep like gases but wetdep like fine
                ! aerosols, so set certain fields to missing values.
                ThisSpc%DD_DvzAerSnow = MISSING
                ThisSpc%MP_SizeResAer = MISSING_BOOL
                ThisSpc%MP_SizeResNum = MISSING_BOOL
                ThisSpc%WD_CoarseAer  = MISSING_BOOL
             CASE DEFAULT
                ! For all other gas-phase species, set all
                ! aerosol fields to missing values
                ThisSpc%DD_DvzAerSnow = MISSING
                ThisSpc%MP_SizeResAer = MISSING_BOOL
                ThisSpc%MP_SizeResNum = MISSING_BOOL
                ThisSpc%WD_CoarseAer  = MISSING_BOOL
                ThisSpc%WD_AerScavEff = MISSING
                ThisSpc%WD_KcScaleFac = MISSING
                ThisSpc%WD_RainoutEff = MISSING
          END SELECT
       ENDIF

             ! Halve the Kc (cloud condensate -> precip) rate
             ! for the temperature range 237 K <= T < 258 K.
             KcScale       = (/ 1.0_fp, 0.5_fp, 1.0_fp /)

             ! Turn off rainout only when 237 K <= T < 258K.
             RainEff       = (/ 1.0_fp, 0.0_fp, 1.0_fp /)

             FullName      = 'Coarse sea salt iodine'
             Formula       = 'I'
             MW_g          = 127.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             Density       = 2200.0_fp
             Radius        = Radius
             DD_AeroDryDep = T
             DD_F0         = 0.0_fp
             DD_Hstar_Old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_CoarseAer  = T
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          CASE( 'AERI' )
             ! Mimic SO4 (AERI is essentiall iodine dissolved in aerosol)

             ! Halve the Kc (cloud condensate -> precip) rate
             ! for the temperature range 237 K <= T < 258 K.
             KcScale   = (/ 1.0_fp, 0.5_fp, 1.0_fp /)

             ! Turn off rainout only when 237 K <= T < 258K.
             RainEff   = (/ 1.0_fp, 0.0_fp, 1.0_fp /)

             ! (cf. Mian Chin's GOCART model)
             ! Minimum Vd over snow/ice : 0.01 cm/s
             ! Minimum Vd over land     : 0.01 cm/s
             DvzMinVal = (/ 0.01_fp, 0.01_fp /)

             FullName      = 'iodine on aerosol'
             Formula       = 'I'
             MW_g          = 127.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             DD_DvzAerSnow = 0.03_fp
             DD_DvzMinVal  = DvzMinVal
             DD_F0         = 0.0_fp
             DD_Hstar_Old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          !==================================================================
          ! Species for the Rn-Pb-Be specialty simulation
          !==================================================================

          CASE( 'RN', '222RN', 'RN222' )
             FullName      = 'Radon-222 isotope'
             Formula       = 'Rn'
             MW_g          = 222.0_fp
             Is_Gas        = F
             Is_Drydep     = F
             Is_Wetdep     = F

          CASE( 'PB', '210PB', 'PB210' )

             ! Halve the Kc (cloud condensate -> precip) rate
             ! for the temperature range 237 K <= T < 258 K.
             KcScale       = (/ 1.0_fp, 0.5_fp, 1.0_fp /)

             ! Turn off rainout only when 237 K <= T < 258K.
             RainEff       = (/ 1.0_fp, 0.0_fp, 1.0_fp /)

             FullName      = 'Lead-210 isotope'
             Formula       = 'Pb'
             MW_g          = 210.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             DD_DvzAerSnow = 0.03_fp
             DD_F0         = 0.0_fp
             DD_HStar_old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          CASE( 'PBSTRAT', '210PBSTRAT', 'PB210STRAT' )

             ! Halve the Kc (cloud condensate -> precip) rate
             ! for the temperature range 237 K <= T < 258 K.
             KcScale       = (/ 1.0_fp, 0.5_fp, 1.0_fp /)

             ! Turn off rainout only when 237 K <= T < 258K.
             RainEff       = (/ 1.0_fp, 0.0_fp, 1.0_fp /)

             FullName      = 'Lead-210 isotope in stratosphere'
             Formula       = 'Pb'
             MW_g          = 210.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             DD_DvzAerSnow = 0.03_fp
             DD_F0         = 0.0_fp
             DD_HStar_old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          CASE( 'BE', '7BE', 'BE7' )

             ! Halve the Kc (cloud condensate -> precip) rate
             ! for the temperature range 237 K <= T < 258 K.
             KcScale       = (/ 1.0_fp, 0.5_fp, 1.0_fp /)

             ! Turn off rainout only when 237 K <= T < 258K.
             RainEff       = (/ 1.0_fp, 0.0_fp, 1.0_fp /)

             FullName      = 'Beryllium-7 isotope'
             Formula       = 'Be7'
             MW_g          = 7.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             DD_DvzAerSnow = 0.03_fp
             DD_F0         = 0.0_fp
             DD_HStar_old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          CASE( 'BESTRAT', '7BESTRAT', 'BE7STRAT' )

             ! Halve the Kc (cloud condensate -> precip) rate
             ! for the temperature range 237 K <= T < 258 K.
             KcScale       = (/ 1.0_fp, 0.5_fp, 1.0_fp /)

             ! Turn off rainout only when 237 K <= T < 258K.
             RainEff       = (/ 1.0_fp, 0.0_fp, 1.0_fp /)

             FullName      = 'Beryllium-7 isotope in stratosphere'
             Formula       = 'Be7'
             MW_g          = 7.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             DD_DvzAerSnow = 0.03_fp
             DD_F0         = 0.0_fp
             DD_HStar_old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          CASE( '10BE', 'BE10' )

             ! Halve the Kc (cloud condensate -> precip) rate
             ! for the temperature range 237 K <= T < 258 K.
             KcScale       = (/ 1.0_fp, 0.5_fp, 1.0_fp /)

             ! Turn off rainout only when 237 K <= T < 258K.
             RainEff       = (/ 1.0_fp, 0.0_fp, 1.0_fp /)

             FullName      = 'Beryllium-10 isotope'
             Formula       = 'Be10'
             MW_g          = 10.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             DD_DvzAerSnow = 0.03_fp
             DD_F0         = 0.0_fp
             DD_HStar_old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          CASE( '10BESTRAT', 'BE10STRAT' )

             ! Halve the Kc (cloud condensate -> precip) rate
             ! for the temperature range 237 K <= T < 258 K.
             KcScale       = (/ 1.0_fp, 0.5_fp, 1.0_fp /)

             ! Turn off rainout only when 237 K <= T < 258K.
             RainEff       = (/ 1.0_fp, 0.0_fp, 1.0_fp /)

             FullName      = 'Beryllium-10 isotope in stratosphere'
             Formula       = 'Be10'
             MW_g          = 10.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             DD_DvzAerSnow = 0.03_fp
             DD_F0         = 0.0_fp
             DD_HStar_old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          CASE( 'SF6' ) 
             FullName      = 'Sulfur hexaflouride'
             Formula       = 'SF6'
             MW_g          = 146.0_fp
             Is_Gas        = T
             Is_Drydep     = F
             Is_Wetdep     = F

          CASE( 'AOA' ) 
             FullName      = 'Age of air'
             Formula       = 'AOA'
             MW_g          = 1.0_fp
             Is_Gas        = T
             Is_Drydep     = F
             Is_Wetdep     = F

          CASE( 'E90' ) 
             FullName      = 'Constant emission 90 day tracer'
             Formula       = 'NA'
             MW_g          = 1.0_fp
             Is_Gas        = T
             Is_Drydep     = F
             Is_Wetdep     = F

          CASE( 'ST80_25' )
             FullName      = 'Stratosphere source 25 day tracer'
             Formula       = 'NA'
             MW_g          = 1.0_fp
             Is_Gas        = T
             Is_Drydep     = F
             Is_Wetdep     = F

          CASE( 'CO_50_NA' )
             FullName      = 'Anthropogenic CO North America 50 day tracer'
             Formula       = 'NA'
             MW_g          = 28.0_fp
             Is_Gas        = T
             Is_Drydep     = F
             Is_Wetdep     = F

          !==================================================================
          ! Species for the Hg specialty simulation
          !==================================================================

          CASE( 'HG0',     'HG0_CAN', 'HG0_USA', 'HG0_CAM', 'HG0_SAM',      &
                'HG0_WAF', 'HG0_EAF', 'HG0_SAF', 'HG0_NAF', 'HG0_EUR',      &
                'HG0_EEU', 'HG0_MDE', 'HG0_SOV', 'HG0_SAS', 'HG0_EAS',      &
                'HG0_SEA', 'HG0_JPN', 'HG0_OCE', 'HG0_SO',  'HG0_BB',       &
                'HG0_GEO', 'HG0_ATL', 'HG0_NAT', 'HG0_SAT', 'HG0_NPA',      &
                'HG0_ARC', 'HG0_ANT', 'HG0_OCN', 'HG0_STR'   )

             ! Standardize tagged Hg0 species names
             SELECT CASE( TRIM( Name ) )
                CASE( 'HG0'     )
                   FullName = 'Elemental mercury'
                CASE( 'HG0_CAN' )
                   FullName = 'Elemental mercury from Canada'
                CASE( 'HG0_USA' )
                   FullName = 'Elemental mercury from USA'
                CASE( 'HG0_CAM' )
                   FullName = 'Elemental mercury from Central America'
                CASE( 'HG0_SAM' )
                   FullName = 'Elemental mercury from South America'
                CASE( 'HG0_WAF' )
                   FullName = 'Elemental mercury from West Africa'
                CASE( 'HG0_EAF' )
                   FullName = 'Elemental mercury from East Africa'
                CASE( 'HG0_SAF' )
                   FullName = 'Elemental mercury from South Africa'
                CASE( 'HG0_NAF' )
                   FullName = 'Elemental mercury from North Africa'
                CASE( 'HG0_EUR' )
                   FullName = 'Elemental mercury from OECD Europe'
                CASE( 'HG0_EEU' )
                   FullName = 'Elemental mercury from Eastern Europe'
                CASE( 'HG0_MDE' )
                   FullName = 'Elemental mercury from Middle East'
                CASE( 'HG0_SOV' )
                   FullName = 'Elemental mercury from former USSR'
                CASE( 'HG0_SAS' )
                   FullName = 'Elemental mercury from South Asia'
                CASE( 'HG0_EAS' )
                   FullName = 'Elemental mercury from East Asia'
                CASE( 'HG0_SEA' )
                   FullName = 'Elemental mercury from Southeast Asia'
                CASE( 'HG0_JPN' )
                   FullName = 'Elemental mercury from Japan'
                CASE( 'HG0_OCE' )
                   FullName = 'Elemental mercury from Oceania'
                CASE( 'HG0_SO'  )
                   FullName = 'Elemental mercury from Organic Soil'
                CASE( 'HG0_BB'  )
                   FullName = 'Elemental mercury from Biomass Burning'
                CASE( 'HG0_GEO' )
                   FullName = 'Elemental mercury from Geogenic Sources'
                CASE( 'HG0_ATL' )
                   FullName = 'Elemental mercury from Midatlantic Subsurface Water'
                CASE( 'HG0_NAT' )
                   FullName = 'Elemental mercury from N. Atlantic Subsurface Water'
                CASE( 'HG0_SAT' )
                   FullName = 'Elemental mercury from S. Atlantic Subsurface Water'
                CASE( 'HG0_NPA' )
                   FullName = 'Elemental mercury from N. Pacific Subsurface Water'
                CASE( 'HG0_ARC' )
                   FullName = 'Elemental mercury from Arctic Subsurface Water'
                CASE( 'HG0_ANT' )
                   FullName = 'Elemental mercury from Antarctic Subsurface Water'
                CASE( 'HG0_OCN' )
                   FullName = 'Elemental mercury from Indo-Pacific Subsurface Water'
                CASE( 'HG0_STR' )
                   FullName = 'Elemental mercury from Stratosphere'
             END SELECT

             FullName      = FullName
             Formula       = 'Hg'
             MW_g          = 201.0_fp
             Is_Gas        = T
             Is_Drydep     = T
             Is_Wetdep     = F
             Is_Hg0        = T
             DD_F0         = 1.0e-5_fp
             DD_Hstar_old  = 0.11_fp

          CASE( 'HG2',     'HG2_CAN', 'HG2_USA', 'HG2_CAM', 'HG2_SAM',      &
                'HG2_WAF', 'HG2_EAF', 'HG2_SAF', 'HG2_NAF', 'HG2_EUR',      &
                'HG2_EEU', 'HG2_MDE', 'HG2_SOV', 'HG2_SAS', 'HG2_EAS',      &
                'HG2_SEA', 'HG2_JPN', 'HG2_OCE', 'HG2_SO',  'HG2_BB',       &
                'HG2_GEO', 'HG2_ATL', 'HG2_NAT', 'HG2_SAT', 'HG2_NPA',      &
                'HG2_ARC', 'HG2_ANT', 'HG2_OCN', 'HG2_STR'   )

             ! Standardize tagged Hg0 species names
             SELECT CASE( TRIM( Name ) )
                CASE( 'HG2'     )
                   FullName = 'Divalent mercury'
                CASE( 'HG2_CAN' )
                   FullName = 'Divalent mercury from Canada'
                CASE( 'HG2_USA' )
                   FullName = 'Divalent mercury from USA'
                CASE( 'HG2_CAM' )
                   FullName = 'Divalent mercury from Central America'
                CASE( 'HG2_SAM' )
                   FullName = 'Divalent mercury from South America'
                CASE( 'HG2_WAF' )
                   FullName = 'Divalent mercury from West Africa'
                CASE( 'HG2_EAF' )
                   FullName = 'Divalent mercury from East Africa'
                CASE( 'HG2_SAF' )
                   FullName = 'Divalent mercury from South Africa'
                CASE( 'HG2_NAF' )
                   FullName = 'Divalent mercury from North Africa'
                CASE( 'HG2_EUR' )
                   FullName = 'Divalent mercury from OECD Europe'
                CASE( 'HG2_EEU' )
                   FullName = 'Divalent mercury from Eastern Europe'
                CASE( 'HG2_MDE' )
                   FullName = 'Divalent mercury from Middle East'
                CASE( 'HG2_SOV' )
                   FullName = 'Divalent mercury from former USSR'
                CASE( 'HG2_SAS' )
                   FullName = 'Divalent mercury from South Asia'
                CASE( 'HG2_EAS' )
                   FullName = 'Divalent mercury from East Asia'
                CASE( 'HG2_SEA' )
                   FullName = 'Divalent mercury from Southeast Asia'
                CASE( 'HG2_JPN' )
                   FullName = 'Divalent mercury from Japan'
                CASE( 'HG2_OCE' )
                   FullName = 'Divalent mercury from Oceania'
                CASE( 'HG2_SO'  )
                   FullName = 'Divalent mercury from Organic Soil'
                CASE( 'HG2_BB'  )
                   FullName = 'Divalent mercury from Biomass Burning'
                CASE( 'HG2_GEO' )
                   FullName = 'Divalent mercury from Geogenic Sources'
                CASE( 'HG2_ATL' )
                   FullName = 'Divalent mercury from Midatlantic Subsurface Water'
                CASE( 'HG2_NAT' )
                   FullName = 'Divalent mercury from N. Atlantic Subsurface Water'
                CASE( 'HG2_SAT' )
                   FullName = 'Divalent mercury from S. Atlantic Subsurface Water'
                CASE( 'HG2_NPA' )
                   FullName = 'Divalent mercury from N. Pacific Subsurface Water'
                CASE( 'HG2_ARC' )
                   FullName = 'Divalent mercury from Arctic Subsurface Water'
                CASE( 'HG2_ANT' )
                   FullName = 'Divalent mercury from Antarctic Subsurface Water'
                CASE( 'HG2_OCN' )
                   FullName = 'Divalent mercury from Indo-Pacific Subsurface Water'
                CASE( 'HG2_STR' )
                   FullName = 'Divalent mercury from Stratosphere'
             END SELECT

             FullName      = FullName
             Formula       = 'Hg'
             MW_g          = 201.0_fp
             Is_Gas        = T
             Is_Drydep     = T
             Is_Wetdep     = T
             Is_Hg2        = T
             DD_F0         = 0.0_fp
#ifdef NEW_HENRY_CONSTANTS
             Henry_K0      = 1.40e+4_f8 * To_M_atm
             Henry_CR      = 5300.0_f8
#else
             DD_Hstar_old  = 1.00e+14_fp
             Henry_K0      = 1.40e+6_f8
             Henry_CR      = 8400.0_f8
#endif
             WD_RetFactor  = 1.0_fp

          CASE( 'HGP',     'HGP_CAN', 'HGP_USA', 'HGP_CAM', 'HGP_SAM',      &
                'HGP_WAF', 'HGP_EAF', 'HGP_SAF', 'HGP_NAF', 'HGP_EUR',      &
                'HGP_EEU', 'HGP_MDE', 'HGP_SOV', 'HGP_SAS', 'HGP_EAS',      &
                'HGP_SEA', 'HGP_JPN', 'HGP_OCE', 'HGP_SO',  'HGP_BB',       &
                'HGP_GEO', 'HGP_ATL', 'HGP_NAT', 'HGP_SAT', 'HGP_NPA',      &
                'HGP_ARC', 'HGP_ANT', 'HGP_OCN', 'HGP_STR' )

             ! Standardize tagged HgP species names
             SELECT CASE( TRIM( Name ) )
                 CASE( 'HGP'     )
                   FullName = 'Particulate mercury'
                CASE( 'HGP_CAN' )
                   FullName = 'Particulate mercury from Canada'
                CASE( 'HGP_USA' )
                   FullName = 'Particulate mercury from USA'
                CASE( 'HGP_CAM' )
                   FullName = 'Particulate mercury from Central America'
                CASE( 'HGP_SAM' )
                   FullName = 'Particulate mercury from South America'
                CASE( 'HGP_WAF' )
                   FullName = 'Particulate mercury from West Africa'
                CASE( 'HGP_EAF' )
                   FullName = 'Particulate mercury from East Africa'
                CASE( 'HGP_SAF' )
                   FullName = 'Particulate mercury from South Africa'
                CASE( 'HGP_NAF' )
                   FullName = 'Particulate mercury from North Africa'
                CASE( 'HGP_EUR' )
                   FullName = 'Particulate mercury from OECD Europe'
                CASE( 'HGP_EEU' )
                   FullName = 'Particulate mercury from Eastern Europe'
                CASE( 'HGP_MDE' )
                   FullName = 'Particulate mercury from Middle East'
                CASE( 'HGP_SOV' )
                   FullName = 'Particulate mercury from former USSR'
                CASE( 'HGP_SAS' )
                   FullName = 'Particulate mercury from South Asia'
                CASE( 'HGP_EAS' )
                   FullName = 'Particulate mercury from East Asia'
                CASE( 'HGP_SEA' )
                   FullName = 'Particulate mercury from Southeast Asia'
                CASE( 'HGP_JPN' )
                   FullName = 'Particulate mercury from Japan'
                CASE( 'HGP_OCE' )
                   FullName = 'Particulate mercury from Oceania'
                CASE( 'HGP_SO'  )
                   FullName = 'Particulate mercury from Organic Soil'
                CASE( 'HGP_BB'  )
                   FullName = 'Particulate mercury from Biomass Burning'
                CASE( 'HGP_GEO' )
                   FullName = 'Particulate mercury from Geogenic Sources'
                CASE( 'HGP_ATL' )
                   FullName = 'Particulate mercury from Midatlantic Subsurface Water'
                CASE( 'HGP_NAT' )
                   FullName = 'Particulate mercury from N. Atlantic Subsurface Water'
                CASE( 'HGP_SAT' )
                   FullName = 'Particulate mercury from S. Atlantic Subsurface Water'
                CASE( 'HGP_NPA' )
                   FullName = 'Particulate mercury from N. Pacific Subsurface Water'
                CASE( 'HGP_ARC' )
                   FullName = 'Particulate mercury from Arctic Subsurface Water'
                CASE( 'HGP_ANT' )
                   FullName = 'Particulate mercury from Antarctic Subsurface Water'
                CASE( 'HGP_OCN' )
                   FullName = 'Particulate mercury from Indo-Pacific Subsurface Water'
                CASE( 'HGP_STR' )
                   FullName = 'Particulate mercury from Stratosphere'
             END SELECT

             !%%% NOTE: In the prior code, the rainout fraction for HgP
             !%%% was computed before the shunt that turned off rainout
             !%%% when 237 K <= T < 258 K.  Therefore, in order to
             !%%% replicate the behavior of the prior code, we need to
             !%%% set the rainout efficiency (field WD_RainoutEff) to
             !%%% 1.0 for all temperature regimes.
             !%%%
             !%%% But in the prior code, the Kc rate (rate of conversion
             !%%% of cloud condensate to precipitation) for HgP was
             !%%% multiplied by 0.5 (as is done for most other aerosols)
             !%%% in routine F_AEROSOL.  This is part of the update to
             !%%% allow scavenging by snow that was implemented by Qiaoqiao
             !%%% Wang.
             !%%%
             !%%% Therefore, we have to ask Team Hg if we should allow
             !%%% the rainout for Hg to be turned off AND the Kc rate
             !%%% to be multiplied by 0.5.  They may have intended to
             !%%% not turn off rainout for HgP, but may also have been
             !%%% unaware of the scaling of the Kc rate by 0.5 in the
             !%%% F_AEROSOL routine.
             !%%%
             !%%% For the time being, we shall replicate the behavior of
             !%%% the prior code.  Therefore, we shall allow rainout of
             !%%% HgP to occur for 237 <= T < 258 K AND ALSO multiply
             !%%% Kc by 0.5.
             !%%%
             !%%% (bmy, 9/28/15)

             ! When 237 K <= T < 258 K:
             ! (1) Halve the Kc (cloud condensate -> precip) rate
             ! (2) DON'T TURN OFF RAINOUT! (at least until we talk to Team Hg)
             KcScale       = (/ 1.0_fp, 0.5_fp, 1.0_fp /)
             RainEff       = (/ 1.0_fp, 1.0_fp, 1.0_fp /)

             FullName      = FullName
             Formula       = 'Hg'
             MW_g          = 201.0_fp
             Is_Gas        = F
             Is_Drydep     = T
             Is_Wetdep     = T
             Is_HgP        = T
             DD_DvzAerSnow = 0.03_fp
             DD_F0         = 0.0_fp
             DD_Hstar_old  = 0.0_fp
             WD_AerScavEff = 1.0_fp
             WD_KcScaleFac = KcScale
             WD_RainoutEff = RainEff

          !==================================================================
          ! Species for the POPs specialty simulation
          !==================================================================

          CASE( 'POPG' )

             !----------------------------------------------------------------
             ! Notes for DD_Hstar_old from the v11-01c drydep_mod.F90
             ! (cf. Carey Friedman and Helen Amos
             !----------------------------------------------------------------
             ! HSTAR is Henry's Law in mol/L/atm.
             ! For PHENANTHRENE, log Kaw = -2.76
             !  so unitless Kaw = 1.73*10^-3 and Kwa = 1/Kaw
             !  Divide by R (0.0821 atm/M/K) and T (298 K) and get
             !  HSTAR = 23.5 M/atm
             ! For PYRENE, log Kaw = -3.27
             !  Using the same conversion, HSTAR = 76.1 M/atm
             ! For BENZO[a]PYRENE, log Kaw = -4.51
             !  Using the same conversion, HSTAR = 1.32d3 M/atm
             !  All log Kaws from Ma et al., J Chem Eng Data 2010, 55:819
             !
             !----------------------------------------------------------------
             ! Notes for DD_KOA from the v11-01c drydep_mod.F90
             ! (cf. Carey Friedman and Helen Amos)
             !----------------------------------------------------------------
             ! Adding Koa (octanol-ar partition coefficient) for POPs to
             !  account for accumulation in leaf cuticles
             !  Needs to be in units of mol/liter/atm as with HSTAR
             !  Divide unitless Koa at 298 K by product of R (0.0821 atm/M/K)
             !  and T (298 K)
             ! For PHENANTHRENE, log Koa = 7.64
             !  use same conversion as for HSTAR to get 1.78d6 M/atm
             ! For PYRENE, log Koa = 8.86
             !  use same conversion to get 2.96d7 M/atm
             ! For BENZO[a]PYRENE, log Koa = 11.48
             !  use same conversion to get 1.23d10 M/atm
             ! All log Koas from Ma et al., J Chem Eng Data 2010, 55:819
             ! Now add factor of 0.8 to account for 80% vol content of octanol
             !
             !----------------------------------------------------------------
             ! Notes for Henry_K0, Henry_CR from the v11-01c wetscav_mod.F90
             ! (cf. Carey Friedman and Helen Amos
             !----------------------------------------------------------------
             ! Cocmpute liquid to gas ratio for POPs using
             !  the appropriate parameters for Henry's Law (M/atm, unitless
             !  Kaw divided by R (in atm/M/K, or 8.21d-2) and T (T = 298 K))
             !  as first argument and negative enthalpy of water-air exchange
             !  (kJ/mol) divided by R (in kJ/mol/K, or 8.32d-3) as second
             !  argument.
             ! For PHENANTHRENE, HSTAR = 2.35d1 and del_H = -5.65d3 (HSTAR from
             !  Ma et al, 2010 J. Chem. Eng. Data, and del_H from Scharzenbach
             !  2003, p200)
             ! For PYRENE, HSTAR = 7.61d1 and del_H = -5.17d3 (HSTAR from Ma
             !  et al and del_H from Scharzenbach 2003, p200)
             ! For BENZO[a]PYRENE, HSTAR = 1.32d3 and del_H = -5.65d3
             !  (HSTAR from Ma et al and Del_H the same as pyrene for now)
             !----------------------------------------------------------------
             IF ( .NOT. Present(Input_Opt) ) THEN
                WRITE( 6, '(a)' ) REPEAT( '=', 79 )
                WRITE( 6, * ) 'Error getting info for species ',TRIM(Name)
                WRITE( 6, * ) 'Input_Opt is missing!'
                WRITE( 6, * ) 'In module Headers/species_database_mod.F90!'
                RC = -1
                RETURN
             ENDIF
       ! If the species is an aerosol, set all gas fields to missing values
       IF ( ThisSpc%Is_Aerosol ) THEN
          ThisSpc%WD_ConvFacI2G = MISSING
          ThisSpc%WD_RetFactor  = MISSING
          ThisSpc%WD_LiqAndGas  = MISSING_BOOL
       ENDIF

       ! Debug printout
       IF ( prtDebug ) CALL Spc_Print( Input_Opt, ThisSpc, RC )

       ! Free pointer
       ThisSpc => NULL()
    ENDDO

    ! FORMAT statements
10  FORMAT( a30, " | ", a      )
20  FORMAT( a30, " | ", L10    )
30  FORMAT( a30, " | ", f10.2  )
31  FORMAT( a30, " | ", 2f10.2 )
32  FORMAT( a30, " | ", 3f10.2 )
40  FORMAT( a30, " | ", i10    )

    !=======================================================================
    ! Normal exit
    !=======================================================================

    ! Free objects and arrays, then return
    ThisSpc => NULL()
    CALL QFYAML_CleanUp( yml )
    CALL Cleanup_Work_Arrays()

    !### Uncomment this to stop here when debugging species info
    !STOP

    RETURN

    !=======================================================================
    ! Abnormal exit
    !=======================================================================
999 CONTINUE

    ! Free objects and arrays
    ThisSpc => NULL()
    CALL QFYAML_CleanUp( yml )
    CALL Cleanup_Work_Arrays()

    ! Exit with error
    errMsg = 'Could not read species database variable: ' // TRIM( key )
    CALL GC_Error( errMsg, RC, thisLoc )

  END SUBROUTINE Init_Species_Database
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cast_and_RoundOff
!
! !DESCRIPTION: Casts a 4-byte variable to 8-byte, and then rounds off
!  to 2 decimal places.  Used for species database fields.
!\\
!\\
! !INTERFACE:
!
  FUNCTION Cast_and_RoundOff( v_real ) RESULT( v_dble )
!
! !USES:
!
    USE RoundOff_Mod
    USE Species_Mod, ONLY : ZERO_R4
    USE Species_Mod, ONLY : MISSING
!
! !INPUT PARAMETERS:
!
    REAL(f4), INTENT(IN) :: v_real   ! Input, 4-byte real
!
! !RETURN VALUE:
!
    REAL(f8)             :: v_dble   ! Output, 8-byte real
!
! !REVISION HISTORY:
!  30 Apr 2020 - R. Yantosca - Initial version
!  See the subsequent Git history with the gitk browser!
!EOP
!------------------------------------------------------------------------------
!BOC

    ! If v_real is a missing value, return with 8-byte missing value
    IF ( v_real < ZERO_R4 ) THEN
       v_dble = MISSING
       RETURN
    ENDIF

    ! Cast to real*8 and roundoff (if the number isn't too large)
    v_dble = RoundOff( DBLE( v_real ), 2 )

  END FUNCTION Cast_And_RoundOff
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read_Species_Database
!
! !DESCRIPTION: Reads the metadata for each species into a QFYAML object.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Read_Species_Database( Input_Opt, yml, RC )
!
! !USES:
!
    USE ErrCode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE QFYAML_Mod
!
! !INPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(IN) :: Input_Opt   ! Input options
!
! !OUTPUT PARAMETERS:
!
    TYPE(QFYAML_t), INTENT(OUT) :: yml
    INTEGER,        INTENT(OUT) :: RC
!
! !REVISION HISTORY:
!  28 Apr 2020 - R. Yantosca - Initial version
!  See the subsequent Git history with the gitk browser!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Strings
    CHARACTER(LEN=255) :: fileName
    CHARACTER(LEN=255) :: thisLoc
    CHARACTER(LEN=512) :: errMsg

    ! Objects
    TYPE(QFYAML_t)     :: yml_1
    TYPE(QFYAML_t)     :: yml_2
    TYPE(QFYAML_t)     :: yml_anchored

    !=========================================================================
    ! Read_Species_Database begins here!
    !=========================================================================

    ! Initialize
    RC      = GC_SUCCESS
    errMsg  = ""
    thisLoc = &
    " -> at Read Species Database (in module Headers/species_database_mod.F90)"

    !=======================================================================
    ! Read metadata for GEOS-Chem species
    !========================================================================
    fileName = TRIM( Input_Opt%SpcDatabaseFile )
    CALL QFYAML_Init( fileName, yml, yml_anchored, RC )
    IF ( RC /= GC_SUCCESS ) THEN
       errMsg = "Error reading " // TRIM( fileName )
       CALL GC_Error( errMsg, RC, thisLoc )
       RETURN
    ENDIF
    CALL QFYAML_CleanUp( yml_anchored )

  END SUBROUTINE Read_Species_Database
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleanup_Species_Database
!
! !DESCRIPTION: Finalizes the vector with species information.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Cleanup_Species_Database( SpcData, RC )
!
! !USES:
!
    USE ErrCode_Mod
    USE Species_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(SpcPtr),   POINTER     :: SpcData(:)   ! Species database object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT) :: RC           ! Success or failure?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  22 Jul 2015 - R. Yantosca - Initial version
!  See https://github.com/geoschem/geos-chem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Assume success
    RC = GC_SUCCESS

    ! Deallocate the species database object
    CALL SpcData_Cleanup( SpcData )

  END SUBROUTINE Cleanup_Species_Database
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: TranUc
!
! !DESCRIPTION: Tranlate a character variable to all upper case letters.
!  Non-alphabetic characters are not affected.  The original "text" is
!  destroyed.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TranUc( text )
!
! !INPUT/OUTPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(INOUT) :: text
!
! !AUTHOR:
!  Robert D. Stewart, May 19, 1992 (part of CHARPAK)
!
! !REMARKS:
!  Keep a private shadow copy of this routine here so as not to
!  incur a dependency with GeosUtil/charpak_mod.F90.  This lets us
!  keep species_datbase_mod.F90 in the Headers/ folder together
!  with state_chm_mod.F90 and species_mod.F90.
!
! !REVISION HISTORY:
!  06 Jan 2015 - R. Yantosca - Initial version
!  See https://github.com/geoschem/geos-chem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: iasc, i, ilen

    ilen = LEN(text)
    DO i=1,ilen
       iasc = ICHAR(text(i:i))
       IF ((iasc.GT.96).AND.(iasc.LT.123)) THEN
          text(i:i) = CHAR(iasc-32)
       ENDIF
    ENDDO

  END SUBROUTINE TranUc
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Unique_Species_Names
!
! !DESCRIPTION: Stores the list of unique species names (i.e. removing
!  duplicates from the list of advected species and the the list of KPP
!  species) for later use.  Also computes the corresponding indices for
!  the KPP variable and fixed species arrays (VAR and FIX, respectively).
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Unique_Species_Names( Input_Opt, nSpecies, RC )
!
! !USES:
!
    USE ErrCode_Mod
    USE Input_Opt_Mod,    ONLY : OptInput
    USE GcKpp_Monitor,    ONLY : Spc_Names
    USE GcKpp_Parameters, ONLY : NFIX, NSPEC, NVAR
    USE Species_Mod,      ONLY : MISSING_INT
!
! !INPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(IN)  :: Input_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT) :: nSpecies    ! Number of unique species
    INTEGER,        INTENT(OUT) :: RC          ! Success or failure
!
! !REMARKS:
!  This may not be the fastest search algorithm (because it relies on string
!  comparisons).  But it is only executed at startup so we can live with it.
!  We could make it faster by hashing but that seems like overkill.
!
! !REVISION HISTORY:
!  09 May 2016 - R. Yantosca - Initial version
!  See https://github.com/geoschem/geos-chem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER                        :: nAdvect, K, S

    ! Strings
    CHARACTER(LEN=255)             :: errMsg
    CHARACTER(LEN=255)             :: thisLoc

    ! Arrays
    CHARACTER(LEN=31), ALLOCATABLE :: Tmp(:)
    CHARACTER(LEN=31)              :: SpcName

    !=======================================================================
    ! UNIQUE_SPECIES_NAMES begins here!
    !=======================================================================

    ! Assume success
    RC       = GC_SUCCESS
    errMsg   = ''
    thisLoc  = &
    ' -> at Unique_Species_Names (in module Headers/species_database_mod.F90)'

    ! Number of advected species listed in input.geos
    nAdvect  = Input_Opt%N_Advect

    ! First set the # of species to the # of advected species
    nSpecies = nAdvect

    !=======================================================================
    ! For full-chemistry simulations with KPP, get the list of all of
    ! species names in the KPP mechanism, and their indices
    !=======================================================================
    IF ( Input_Opt%ITS_A_FULLCHEM_SIM ) THEN

       ! Allocate a temporary array large enough to hold all of the
       ! advected species listed in input.geos as well as all of the
       ! KPP species names (listed in SPC_NAMES of gckpp_Monitor.F90)
       ALLOCATE( Tmp( nAdvect + NSPEC ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:Tmp', 0 , RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       Tmp = ''

       !--------------------------------------------------------------------
       ! First determine the unique list of species in the KPP mechanism
       ! (so that we don't duplicate storage for advected & chemical species)
       !--------------------------------------------------------------------

       ! First, store advected species (from input.geos) in the TMP array
       DO S = 1, nSpecies
          Tmp(S) = Input_Opt%AdvectSpc_Name(S)
       ENDDO

       ! Loop over KPP species
       DO K = 1, NSPEC

          ! Skip dummy RR species for prod/loss diagnostic (mps, 8/23/16)
          SpcName = ADJUSTL( Spc_Names(K) )
          IF ( SpcName(1:2) == 'RR' ) CYCLE

          ! Next, add to the TMP array those KPP species that aren't already
          ! listed as advected species.  nSpecies is the # of unique species.
          IF ( .not. ANY( Input_Opt%AdvectSpc_Name == Spc_Names(K) ) ) THEN
             nSpecies      = nSpecies + 1
             Tmp(nSpecies) = Spc_Names(K)
          ENDIF

       ENDDO

       ! Allocate the species names array precisely of length nSpecies
       ALLOCATE( Species_Names( nSpecies ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:Species_Names', 0, RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       Species_Names = Tmp(1:nSpecies )

       ! Free temporary array
       IF ( ALLOCATED( Tmp ) ) DEALLOCATE( Tmp )

       !--------------------------------------------------------------------
       ! Now determine the KPP indices for each unique species name
       !--------------------------------------------------------------------

       ! Work array to hold the list of all KPP species indices
       ALLOCATE( KppSpcId( nSpecies ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:KppSpcId', 0, RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       KppSpcId = MISSING_INT

       ! Work array to hold the list of KPP fixed species indices
       ALLOCATE( KppFixId( nSpecies ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:KppFixId', 0, RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       KppFixId = MISSING_INT

       ! Work array to hold the list of KPP variable species indices
       ALLOCATE( KppVarId( nSpecies ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:KppVarId', 0, RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       KppVarId = MISSING_INT

       ! Loop through the list of unique species names
       DO S = 1, nSpecies

          ! Loop through the list of KPP species (stored in SPC_NAMES)
          DO K = 1, NSPEC

             ! Skip dummy RR species for prod/loss diagnostic (mps, 8/23/16)
             SpcName = ADJUSTL( Spc_Names(K) )
             IF ( SpcName(1:2) == 'RR' ) CYCLE

             ! Test the unique species names (stored in SPECIES_NAMES)
             ! against the list of KPP species (in SPC_NAMES).  The K
             ! index corresponds to the location of the species in the
             ! KPP chemical mechanism:  1..NSPEC = [ 1..NVAR, 1..NFIX].
             IF ( Species_Names(S) == Spc_Names(K) ) THEN

                ! KPP species index (1..NSPEC).  These
                ! are used to index species in the KPP "C" array.
                ! These include both variable and fixed species.
                KppSpcId(S) = K

                IF ( K <= NVAR ) THEN

                   ! KPP variable species index (1..NVAR).  These
                   ! are used to index species in the KPP "C" array
                   ! (as well as the "VAR" array).
                   KppVarId(S) = K

                ELSE

                   ! KPP fixed species also have entries (1..NFIX).  These
                   ! are used to index species in the KPP "FIX" array.
                   KppFixId(S) = K - NVAR

                ENDIF

                ! Skip to next species
                EXIT
             ENDIF
          ENDDO
       ENDDO

    !=======================================================================
    ! For specialty simulations, we do not have KPP species.  Thus, the
    ! of species is just the list of advected species from input.geos
    !=======================================================================
    ELSE

       ! Initialize the species names array from Input_Opt
       ALLOCATE( Species_Names( nSpecies ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:Species_Names', 0, RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       Species_Names = Input_Opt%AdvectSpc_Name(1:nSpecies)

       ! Set KppSpcId to missing value
       ALLOCATE( KppSpcId( nSpecies ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:KppSpcId', 0, RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       KppSpcId = MISSING_INT

       ! Set KppFixId to missing value
       ALLOCATE( KppFixId( nSpecies ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:KppFixId', 0, RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       KppFixId = MISSING_INT

       ! Set KppVarId to missing value
       ALLOCATE( KppVarId( nSpecies ), STAT=RC )
       CALL GC_CheckVar( 'species_database_mod.F90:KppVarId', 0, RC )
       IF ( RC /= GC_SUCCESS ) RETURN
       KppVarId = MISSING_INT

    ENDIF

  END SUBROUTINE Unique_Species_Names
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleanup_Work_Arrays
!
! !DESCRIPTION: Cleans working (temporary) arrays used by this module,
!  restoring them to an unused state. It is called at the end of
!  Init\_Species\_Database or by an external module when needed to
!  reinitialize the species DB.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Cleanup_Work_Arrays()
!
! !REMARKS:
!  This routine allows Species_Database_Mod to be initialized more than once
!  in the same CPU, if called externally before re-initializing a State_Chm
!  derived type object.
!
! !REVISION HISTORY:
!  06 May 2016 - R. Yantosca - Initial version
!  See https://github.com/geoschem/geos-chem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Deallocate arrays
    IF ( ALLOCATED( Species_Names ) ) DEALLOCATE( Species_Names )
    IF ( ALLOCATED( KppFixId      ) ) DEALLOCATE( KppFixId      )
    IF ( ALLOCATED( KppVarId      ) ) DEALLOCATE( KppVarId      )
    IF ( ALLOCATED( KppSpcId      ) ) DEALLOCATE( KppSpcId      )

  END SUBROUTINE Cleanup_Work_Arrays
!EOC
END MODULE Species_Database_Mod
