*&---------------------------------------------------------------------*
*& Report ZPG_BM_CS_COMPANYCODE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZIN_BM_CS_COMPANYCODETOP                .    " Global Data

INCLUDE ZIN_BM_CS_COMPANYCODEO01                .  " PBO-Modules
INCLUDE ZIN_BM_CS_COMPANYCODEI01                .  " PAI-Modules
INCLUDE ZIN_BM_CS_COMPANYCODEF01                .  " FORM-Routines

START-OF-SELECTION.
  PERFORM MAIN_PROCESS.
*  AT SELECTION-SCREEN OUTPUT.
