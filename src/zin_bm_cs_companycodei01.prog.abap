*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_COMPANYCODEI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.
  CASE SY-UCOMM.
    WHEN '&F03' OR '&BACK' OR '&F12' OR '&F15'.
      LEAVE TO SCREEN 0.
    WHEN '&ADDRES'.
      PERFORM ADDRES.
    WHEN '&SAVE_DATA'.
      PERFORM PROCESS_DATA.
      WHEN 'OK'.
        PERFORM LOAD_DATA .
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001 INPUT.
  CASE SY-UCOMM.
    WHEN '&F03' OR '&BACK' OR '&F12' OR '&F15'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE_DATA'.
      PERFORM PROCESS_DATA .
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
