(defvar lookml-mode-syntax-table
  (let ((st (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?_ "w" st)
    st))

(defconst lookml-keywords
  '("view" "explore" "dimension" "dimension_group" "measure"
    "filter" "parameter" "set" "join" "derived_table"
    "connection" "include" "datagroup" "access_grant"
    "named_value_format" "map_layer" "test"
    "project_name" "remote_dependency" "local_dependency"
    "constant" "localization_settings" "visualization"
    "manifest"))

(defconst lookml-properties
  '("type" "sql" "sql_on" "sql_where" "sql_always_where"
    "sql_table_name" "sql_trigger_value"
    "label" "group_label" "description" "hidden"
    "primary_key" "relationship" "from" "to"
    "value_format" "value_format_name"
    "drill_fields" "fields" "timeframes"
    "view_label" "view_name"
    "required_joins" "always_filter" "conditionally_filter"
    "persist_for" "persist_with" "extends"
    "suggestions" "suggest_explore" "suggest_dimension"
    "can_filter" "allow_approximate_optimization"
    "datatype" "convert_tz" "alpha_sort"
    "case_sensitive" "full_suggestions"
    "bypass_suggest_restrictions"
    "fanout_on" "required_access_grants"))

(defconst lookml-types
  '("string" "number" "yesno" "time" "tier" "location" "zipcode"
    "count" "count_distinct" "sum" "sum_distinct"
    "average" "average_distinct" "min" "max" "median"
    "percentile" "percentile_distinct" "list"
    "date" "date_time" "date_raw" "date_week" "date_month"
    "date_quarter" "date_year" "date_day_of_week"
    "left_outer" "inner" "full_outer" "cross"
    "one_to_one" "many_to_one" "one_to_many" "many_to_many"))

(define-derived-mode lookml-mode prog-mode "LookML"
  :syntax-table lookml-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (font-lock-add-keywords
   nil
   `((,(concat (regexp-opt lookml-keywords 'symbols) ":") . font-lock-keyword-face)
     (,(regexp-opt lookml-keywords 'symbols) . font-lock-keyword-face)
     (,(concat (regexp-opt lookml-properties) ":") . font-lock-builtin-face)
     (,(regexp-opt lookml-types 'symbols) . font-lock-type-face)
     ("\\$\\({[^}]+}\\)" 0 font-lock-variable-name-face)
     ("\\b\\(yes\\|no\\|true\\|false\\)\\b" . font-lock-constant-face)
     (";;" . font-lock-comment-delimiter-face))))

(add-to-list 'auto-mode-alist '("\\.lkml\\'" . lookml-mode))
(add-to-list 'auto-mode-alist '("\\.lookml\\'" . lookml-mode))
