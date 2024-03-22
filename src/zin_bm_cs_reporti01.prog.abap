*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_REPORTI01
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN '&F12' OR '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&F03'.
      LEAVE PROGRAM.
    WHEN '&DATA_SAVE'.
      PERFORM SAVE.
    WHEN 'FC_GOTOCS'.
      PERFORM 0100_FC_GOTOCS.
    WHEN 'FC_APPLY'.
      PERFORM 0100_FC_APPLY.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE SY-UCOMM.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      GV_CANCEL = 'X'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
