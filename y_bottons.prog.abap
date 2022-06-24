*&---------------------------------------------------------------------*
*&  Include           Y_BOTTONS
*&---------------------------------------------------------------------*
*Criar botões ao lado do executar: new, modify and delete.
TABLES: sscrfields,   "Tabela de campos em img de seleção.
        ykl_plate,
        ykl_request.
TYPE-POOLS: icon.

*buttons
SELECTION-SCREEN: FUNCTION KEY 1.
SELECTION-SCREEN: FUNCTION KEY 2.
SELECTION-SCREEN: FUNCTION KEY 3.

INITIALIZATION.
  PERFORM f_create_botton.

AT SELECTION-SCREEN.
  PERFORM f_capture_event_botton.


FORM f_create_botton.
  DATA: ls_button TYPE smp_dyntxt.

  "btn 1
  ls_button-text = 'Create'.
  ls_button-icon_id = icon_create.
  ls_button-quickinfo = 'Create'.
  sscrfields-functxt_01 = ls_button.

  "btn 2
  ls_button-text = 'Modify'.
  ls_button-icon_id = icon_change.
  ls_button-quickinfo = 'Modify'.
  sscrfields-functxt_02 = ls_button.

  "btn 3
  ls_button-text = 'Delete'.
  ls_button-icon_id = icon_delete.
  ls_button-quickinfo = 'Delete'.
  sscrfields-functxt_03 = ls_button.

ENDFORM. "icon botton

FORM f_capture_event_botton.

  IF sy-ucomm EQ 'FC01'. "create
  ENDIF.

  IF sy-ucomm EQ 'FC02'. "modify
  ENDIF.

  IF sy-ucomm EQ 'FC03'. "delete
  ENDIF.

ENDFORM. "event botton
