*&---------------------------------------------------------------------*
*& Report Y_KL_PROJECT_CASE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_kl_project_case.

TYPE-POOLS: <icon>. "Permite usar todos os tipos de dados e constantes definidos no grupo de tipos <pool>.

TABLES sscrfields. "table system, usado princicipalmente para alterar a tela, ativar e desativar campo.

"Table view (Screen)
TYPES:
  BEGIN OF ly_request_view,
    request_nr    TYPE ykl_request-request_nr,
    plate_type    TYPE ykl_request-plate_type,
    qty           TYPE ykl_request-qty,
    request_value TYPE ykl_request-request_value,
    request_date  TYPE ykl_request-request_date,
    description   TYPE ykl_plate-description,
    unit_value    TYPE ykl_plate-unit_value,
    celltab       TYPE lvc_t_styl,
  END OF ly_request_view.

CLASS lcl_event_grid DEFINITION DEFERRED. " definição adiada

*Declaração de todas as variáveis básicas para construção do ALV. (lo_grid, lv_okcode, lt_fieldcat, ls_layout,ls_variant)
DATA: lo_grid_9000      TYPE REF TO cl_gui_alv_grid,
      lo_container_9000 TYPE REF TO cl_gui_custom_container, "Declaração do container de tela.
      lo_event_grid     TYPE REF TO lcl_event_grid,
      lt_request_view   TYPE TABLE OF ly_request_view,
      lt_fieldcat       TYPE lvc_t_fcat,
      lt_tool_bar       TYPE ui_functions, "Variável responsável em armazenar a lista de botões que serão removidos do GRID.
      ls_layout         TYPE lvc_s_layo,
      ls_variant        TYPE disvariant,
      lv_okcode_9000    TYPE sy-ucomm,
      iv_request        TYPE ykl_request-request_nr.

*Criar opção Maintenance Table ao lado do Executar.
SELECTION-SCREEN: FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_req TYPE ykl_request-request_nr. "OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl01.

SELECTION-SCREEN BEGIN OF BLOCK bl02 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_create RADIOBUTTON GROUP gr1,
            p_modify RADIOBUTTON GROUP gr1,
            p_delete RADIOBUTTON GROUP gr1,
            p_del_it RADIOBUTTON GROUP gr1,
            p_view   RADIOBUTTON GROUP gr1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK bl02.

INCLUDE y_pbo_9000.
INCLUDE y_pai_9000.

CLASS lcl_event_grid DEFINITION.  "Definição da classe event grid
  PUBLIC SECTION.
    METHODS: " Methods uso na definição
      toolbar
          FOR EVENT toolbar
            OF cl_gui_alv_grid IMPORTING e_object e_interactive,

      user_command
          FOR EVENT user_command
            OF cl_gui_alv_grid IMPORTING e_ucomm,

      data_changed
        FOR EVENT data_changed
          OF cl_gui_alv_grid IMPORTING er_data_changed.

ENDCLASS.

CLASS lcl_event_grid IMPLEMENTATION. "Implementação da minha classe de event grid

  METHOD toolbar.
    PERFORM f_toolbar CHANGING e_object e_interactive.
  ENDMETHOD.

  METHOD user_command.
    PERFORM f_user_command CHANGING e_ucomm.
  ENDMETHOD.

  METHOD data_changed.
    PERFORM f_data_changed USING er_data_changed.
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  sscrfields-functxt_01 = 'Maintenance Table Plate'.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      CALL TRANSACTION 'YKL_TRAN_TAB_PLATE'.
  ENDCASE.

START-OF-SELECTION.
  PERFORM f_data_selection.


*&---------------------------------------------------------------------*
*&      Form  F_DATA_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_selection.

  IF NOT p_req BETWEEN 'REQ001' AND 'REQ999' .
    MESSAGE 'Invalid request, check!' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    CASE 'X'.
      WHEN p_create. PERFORM f_create_request.
      WHEN p_modify. PERFORM f_modify_request.
      WHEN p_delete. PERFORM f_delete_request.
      WHEN p_del_it. PERFORM f_delete_request_item.
      WHEN p_view.   PERFORM f_view_request.
    ENDCASE.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_CREATE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_request.

  "If exists
  SELECT SINGLE *
    INTO @DATA(ls_ykl_request)
    FROM ykl_request
    WHERE request_nr EQ @p_req.

  IF sy-subrc EQ 0.
    MESSAGE 'This request already exists! Check.' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    iv_request = p_req.
    CALL SCREEN 9000.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_BUILD_SCREEN_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_build_screen_9000.

  FREE: lt_fieldcat[].

  ls_layout-zebra      = 'X'.
  ls_layout-stylefname = 'CELLTAB'.  "CELLTAB - células

  IF p_view EQ 'X'.
    PERFORM f_build_fieldcat USING:
        'PLATE_TYPE   '  'PLATE_TYPE   '  'YKL_PLATE'    '            '  ' '  ' '  08  CHANGING lt_fieldcat[],
        'DESCRIPTION  '  'DESCRIPTION  '  'YKL_PLATE'    '            '  ' '  ' '  10  CHANGING lt_fieldcat[],
        'QTY          '  'QTY          '  'YKL_REQUEST'  '            '  ' '  ' '  10  CHANGING lt_fieldcat[],
        'REQUEST_VALUE'  'REQUEST_VALUE'  'YKL_REQUEST'  '            '  ' '  'X'  12  CHANGING lt_fieldcat[],
        'REQUEST_DATE '  'REQUEST_DATE '  'YKL_REQUEST'  'Request date'  ' '  ' '  10  CHANGING lt_fieldcat[].
  ELSE.
    PERFORM f_build_fieldcat USING:
        'PLATE_TYPE   '  'PLATE_TYPE   '  'YKL_PLATE'    '            '  'X'  ' '  08  CHANGING lt_fieldcat[],
        'DESCRIPTION  '  'DESCRIPTION  '  'YKL_PLATE'    '            '  'X'  ' '  10  CHANGING lt_fieldcat[],
        'QTY          '  'QTY          '  'YKL_REQUEST'  '            '  'X'  ' '  10  CHANGING lt_fieldcat[],
        'REQUEST_VALUE'  'REQUEST_VALUE'  'YKL_REQUEST'  '            '  'X'  'X'  12  CHANGING lt_fieldcat[],
        'REQUEST_DATE '  'REQUEST_DATE '  'YKL_REQUEST'  'Request date'  'X'  ' '  10  CHANGING lt_fieldcat[].
  ENDIF.

  IF lo_grid_9000 IS INITIAL.
    lo_container_9000 = NEW cl_gui_custom_container( container_name = 'CONTAINER' ).
    lo_grid_9000      = NEW cl_gui_alv_grid( i_parent = lo_container_9000 ).
    lo_event_grid     = NEW lcl_event_grid( ).

    APPEND cl_gui_alv_grid=>mc_fc_excl_all TO lt_tool_bar[].

    SET HANDLER lo_event_grid->toolbar      FOR lo_grid_9000. "SET HANDLER Registra um manipulador de eventos de instância
    SET HANDLER lo_event_grid->user_command FOR lo_grid_9000.

    lo_grid_9000->register_edit_event(
      EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified
    ).

    lo_grid_9000->set_table_for_first_display(
      EXPORTING
        is_variant           = ls_variant
        is_layout            = ls_layout
        i_save               = 'A'
        it_toolbar_excluding = lt_tool_bar[]
      CHANGING
        it_fieldcatalog      = lt_fieldcat[]
        it_outtab            = lt_request_view[]
    ).

    SET HANDLER lo_event_grid->data_changed FOR lo_grid_9000.
  ELSE.
    lo_grid_9000->refresh_table_display( ).
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form F_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_build_fieldcat USING VALUE(p_fieldname) TYPE c
                            VALUE(p_field)     TYPE c
                            VALUE(p_table)     TYPE c
                            VALUE(p_coltext)   TYPE c
                            VALUE(p_edit)      TYPE c
                            VALUE(p_do_sum)    TYPE c
                            VALUE(p_outputlen) TYPE i
                       CHANGING t_fieldcat   TYPE lvc_t_fcat.

  DATA: ls_fieldcat LIKE LINE OF t_fieldcat[].
  ls_fieldcat-fieldname = p_fieldname.
  ls_fieldcat-ref_field = p_field.
  ls_fieldcat-ref_table = p_table.
  ls_fieldcat-coltext   = p_coltext.
  ls_fieldcat-edit      = p_edit.
  ls_fieldcat-do_sum    = p_do_sum.
  ls_fieldcat-outputlen = p_outputlen.
  APPEND ls_fieldcat TO t_fieldcat[].

ENDFORM.                    "f_build_fieldcat


*&---------------------------------------------------------------------*
*&      Form  F_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_OBJECT  text
*      <--P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM f_toolbar CHANGING e_object      TYPE REF TO cl_alv_event_toolbar_set
                        e_interactive TYPE char01.

  DATA: ls_toolbar TYPE stb_button.

  IF p_create EQ 'X' OR p_modify EQ 'X' OR p_del_it EQ 'X'.
    CLEAR: ls_toolbar.
    ls_toolbar-function  = 'SAVE'.
    ls_toolbar-icon      = icon_system_save.
    ls_toolbar-text      = 'Salvar'.
    ls_toolbar-quickinfo = 'Salvar dados'.

    IF p_del_it EQ 'X'.
      ls_toolbar-disabled = 'X'.
    ELSE.
      ls_toolbar-disabled = space.
    ENDIF.

    APPEND ls_toolbar TO e_object->mt_toolbar[].

    "Separador
    CLEAR: ls_toolbar.
    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar[].

    CLEAR: ls_toolbar.
    ls_toolbar-function  = 'NEW'.
    ls_toolbar-icon      = icon_create.
    ls_toolbar-text      = 'Novo'.
    ls_toolbar-quickinfo = 'Novo'.

    IF p_del_it EQ 'X'.
      ls_toolbar-disabled = 'X'.
    ELSE.
      ls_toolbar-disabled = space.
    ENDIF.

    APPEND ls_toolbar TO e_object->mt_toolbar[].

    CLEAR: ls_toolbar.
    ls_toolbar-function  = 'DEL'.
    ls_toolbar-icon      = icon_delete.
    ls_toolbar-text      = 'Remover'.
    ls_toolbar-quickinfo = 'Remover item'.
    ls_toolbar-disabled  = space.
    APPEND ls_toolbar TO e_object->mt_toolbar[].
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM f_user_command CHANGING c_ucomm TYPE sy-ucomm.

  CASE c_ucomm.

    WHEN 'SAVE'.
      PERFORM f_manual_save.

    WHEN 'NEW'.
      PERFORM f_manual_new_line.

    WHEN 'DEL'.
      PERFORM f_manual_remove_line.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_MANUAL_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_manual_save.

  DATA: lt_request_upd TYPE TABLE OF ykl_request.

  MOVE-CORRESPONDING lt_request_view[] TO lt_request_upd[].

  IF lt_request_upd[] IS INITIAL.
    MESSAGE 'Empty table!' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    MODIFY ykl_request FROM TABLE lt_request_upd[].
    COMMIT WORK.
    MESSAGE 'Correct save!' TYPE 'S'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_MANUAL_NEW_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_manual_new_line.

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab LIKE LINE OF lt_celltab[],
        lv_tabix   TYPE sy-tabix. "tabix retorna o número da linha da tabela atual.

  APPEND INITIAL LINE TO lt_request_view[].

  lv_tabix = lines( lt_request_view[] ).

  READ TABLE lt_request_view[] ASSIGNING FIELD-SYMBOL(<fs_request_view>) INDEX lv_tabix.

  IF sy-subrc EQ 0.
    <fs_request_view>-request_nr   = p_req.
    <fs_request_view>-request_date = sy-datum.

    ls_celltab-fieldname = 'DESCRIPTION'.
    ls_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab[].

    ls_celltab-fieldname = 'REQUEST_VALUE'.
    ls_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab[].

    ls_celltab-fieldname = 'REQUEST_DATE'.
    ls_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab[].

    <fs_request_view>-celltab[] = lt_celltab[].
  ENDIF.

  lo_grid_9000->refresh_table_display( ).

  PERFORM f_set_foccus_grid USING lv_tabix
                                  'PLATE_TYPE'
                                  lo_grid_9000.

*  PERFORM f_focar_celula USING lv_tabix 'REQUEST_NR'.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_MANUAL_REMOVE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_manual_remove_line.

  DATA: lt_request_del TYPE TABLE OF ykl_request, "LIKE lt_request[],
        ls_request_del LIKE LINE OF lt_request_del[],
        lt_index_rows  TYPE lvc_t_row,
        lt_row_no	     TYPE lvc_t_roid.

  "Obtém as linhas selecionadas do grid.
  lo_grid_9000->get_selected_rows(
    IMPORTING
      et_index_rows = lt_index_rows[]
      et_row_no     = lt_row_no[]
  ).

  IF lt_row_no[] IS INITIAL.
    MESSAGE 'Select line to data remove!' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    SORT lt_row_no[] BY row_id DESCENDING.

    "Preenche a estrutura com os dados de cópia.
    LOOP AT lt_row_no[] ASSIGNING FIELD-SYMBOL(<fs_row_no>).
      READ TABLE lt_request_view[] ASSIGNING FIELD-SYMBOL(<fs_request_view>) INDEX <fs_row_no>-row_id.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <fs_request_view> TO ls_request_del.
        APPEND ls_request_del TO lt_request_del[].
      ENDIF.

      "Remove from internal table.
      DELETE lt_request_view[] INDEX <fs_row_no>-row_id.
    ENDLOOP.

    "Remove from data table.
    DELETE ykl_request FROM TABLE lt_request_del[].
    COMMIT WORK.

    lo_grid_9000->refresh_table_display( ).
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM f_data_changed USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  LOOP AT er_data_changed->mt_good_cells[] ASSIGNING FIELD-SYMBOL(<fs_good_cells>).
    READ TABLE lt_request_view[] ASSIGNING FIELD-SYMBOL(<fs_request_view>) INDEX <fs_good_cells>-row_id.

    IF sy-subrc EQ 0.
      CASE <fs_good_cells>-fieldname.

        WHEN 'REQUEST_NR'.
          <fs_request_view>-request_nr = <fs_good_cells>-value.

        WHEN 'PLATE_TYPE'.
          <fs_request_view>-plate_type = <fs_good_cells>-value.

          CHECK <fs_request_view>-plate_type IS NOT INITIAL.

          "Fill plate description
          SELECT SINGLE description
            INTO <fs_request_view>-description
            FROM ykl_plate
            WHERE plate_type EQ <fs_request_view>-plate_type.

        WHEN 'QTY'.
          <fs_request_view>-qty = <fs_good_cells>-value.

          CHECK <fs_request_view>-qty IS NOT INITIAL.

          "Fill plate description
          SELECT SINGLE unit_value
            INTO @DATA(lv_unit_value)
            FROM ykl_plate
            WHERE plate_type EQ @<fs_request_view>-plate_type.

          <fs_request_view>-request_value = lv_unit_value * <fs_request_view>-qty.

      ENDCASE.
    ENDIF.
  ENDLOOP.

  lo_grid_9000->refresh_table_display( ).

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_VIEW_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_view_request.

  TYPES:
    BEGIN OF ly_request,
      request_nr    TYPE ykl_request-request_nr,
      plate_type    TYPE ykl_request-plate_type,
      qty           TYPE ykl_request-qty,
      request_value TYPE ykl_request-request_value,
      request_date  TYPE ykl_request-request_date,
      description   TYPE ykl_plate-description,
      unit_value    TYPE ykl_plate-unit_value,
    END OF ly_request.

  DATA: lt_request      TYPE TABLE OF ly_request,
        ls_request_view LIKE LINE OF lt_request_view[].

  FREE: lt_request_view[].

  SELECT
    r~request_nr
    r~plate_type
    r~qty
    r~request_value
    r~request_date
    p~description
    p~unit_value
    INTO TABLE lt_request[]
    FROM ykl_request       AS r
      INNER JOIN ykl_plate AS p ON r~plate_type EQ p~plate_type
    WHERE request_nr EQ p_req.

  IF sy-subrc NE 0.
    MESSAGE 'Nothing data to request!' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    iv_request = p_req.

    LOOP AT lt_request[] ASSIGNING FIELD-SYMBOL(<fs_request>).
      MOVE-CORRESPONDING <fs_request> TO ls_request_view.
      ls_request_view-request_value = ls_request_view-unit_value * ls_request_view-qty.
      APPEND ls_request_view TO lt_request_view[].
    ENDLOOP.

    CALL SCREEN 9000.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_DELETE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_delete_request.

  TYPES:
    BEGIN OF ly_request,
      request_nr    TYPE ykl_request-request_nr,
      plate_type    TYPE ykl_request-plate_type,
      qty           TYPE ykl_request-qty,
      request_value TYPE ykl_request-request_value,
      request_date  TYPE ykl_request-request_date,
      description   TYPE ykl_plate-description,
      unit_value    TYPE ykl_plate-unit_value,
    END OF ly_request.

  DATA: lt_request TYPE TABLE OF ly_request,
        lv_answer  TYPE char1.

  FREE: lt_request_view[].

  SELECT
    r~request_nr
    r~plate_type
    r~qty
    r~request_value
    r~request_date
    p~description
    p~unit_value
    INTO TABLE lt_request[]
    FROM ykl_request       AS r
      INNER JOIN ykl_plate AS p ON r~plate_type EQ p~plate_type
    WHERE request_nr EQ p_req.

  IF sy-subrc NE 0.
    MESSAGE 'Nothing data to request!' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    CALL FUNCTION 'TR_POPUP_TO_CONFIRM'
      EXPORTING
        iv_titlebar           = 'Warning'
        iv_text_question      = 'Would you like data remove?'
        iv_icon_button1       = 'ICON_CHECKED'
        iv_icon_button2       = 'ICON_CANCEL'
        iv_start_column       = 40
        iv_start_row          = 10
        iv_with_cancel_button = space
      IMPORTING
        ev_answer             = lv_answer.

    IF lv_answer EQ '1'.
      DELETE FROM ykl_request WHERE request_nr EQ p_req.
      COMMIT WORK.
      MESSAGE 'Data removed sucessful!' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_DELETE_REQUEST_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_delete_request_item.

  PERFORM f_view_request.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_modify_request.

  TYPES:
    BEGIN OF ly_request,
      request_nr    TYPE ykl_request-request_nr,
      plate_type    TYPE ykl_request-plate_type,
      qty           TYPE ykl_request-qty,
      request_value TYPE ykl_request-request_value,
      request_date  TYPE ykl_request-request_date,
      description   TYPE ykl_plate-description,
      unit_value    TYPE ykl_plate-unit_value,
    END OF ly_request.

  DATA: lt_request      TYPE TABLE OF ly_request,
        lt_celltab      TYPE lvc_t_styl,
        ls_celltab      LIKE LINE OF lt_celltab[],
        ls_request_view LIKE LINE OF lt_request_view[].

  SELECT
    r~request_nr
    r~plate_type
    r~qty
    r~request_value
    r~request_date
    p~description
    p~unit_value
    INTO TABLE lt_request[]
    FROM ykl_request       AS r
      INNER JOIN ykl_plate AS p ON r~plate_type EQ p~plate_type
    WHERE request_nr EQ p_req.

  IF sy-subrc EQ 0.
    iv_request = p_req.

    LOOP AT lt_request[] ASSIGNING FIELD-SYMBOL(<fs_request>).
      FREE: lt_celltab[].

      MOVE-CORRESPONDING <fs_request> TO ls_request_view.
      ls_request_view-request_value = ls_request_view-unit_value * ls_request_view-qty.

      "Block fields
      ls_celltab-fieldname = 'DESCRIPTION'.
      ls_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab[].

      ls_celltab-fieldname = 'REQUEST_VALUE'.
      ls_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab[].

      ls_celltab-fieldname = 'REQUEST_DATE'.
      ls_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab[].

      ls_request_view-celltab[] = lt_celltab[].

      APPEND ls_request_view TO lt_request_view[].
    ENDLOOP.

    CALL SCREEN 9000.
  ELSE.
    MESSAGE 'Empty request!' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_SET_FOCCUS_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_foccus_grid USING u_line_number TYPE i
                             u_fieldname   TYPE string
                             u_grid        TYPE REF TO cl_gui_alv_grid.

  DATA: es_row_id TYPE lvc_s_row,
        es_col_id TYPE lvc_s_col.

  es_row_id-index     = u_line_number.
  es_col_id-fieldname = u_fieldname.

  cl_gui_control=>set_focus(
    EXPORTING
      control = u_grid
  ).

  u_grid->set_current_cell_via_id(
    EXPORTING
      is_row_id    = es_row_id
      is_column_id = es_col_id
  ).

ENDFORM.
