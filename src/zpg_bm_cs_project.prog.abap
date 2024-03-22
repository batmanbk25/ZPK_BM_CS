*&---------------------------------------------------------------------*
*& Report ZPG_BM_CS_PROJECT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZIN_BM_CS_PROJECTTOP                    .    " Global Data
INCLUDE ZIN_BM_CS_PROJECTO01                    .  " PBO-Modules
INCLUDE ZIN_BM_CS_PROJECTI01                    .  " PAI-Modules
INCLUDE ZIN_BM_CS_PROJECTF01                    .  " FORM-Routines


START-OF-SELECTION.
  PERFORM MAIN_PROCESS.
