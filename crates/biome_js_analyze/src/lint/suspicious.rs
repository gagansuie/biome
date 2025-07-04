//! Generated file, do not edit by hand, see `xtask/codegen`

//! Generated file, do not edit by hand, see `xtask/codegen`

use biome_analyze::declare_lint_group;
pub mod no_alert;
pub mod no_approximative_numeric_constant;
pub mod no_array_index_key;
pub mod no_assign_in_expressions;
pub mod no_async_promise_executor;
pub mod no_catch_assign;
pub mod no_class_assign;
pub mod no_comment_text;
pub mod no_compare_neg_zero;
pub mod no_confusing_labels;
pub mod no_confusing_void_type;
pub mod no_console;
pub mod no_const_enum;
pub mod no_control_characters_in_regex;
pub mod no_debugger;
pub mod no_document_cookie;
pub mod no_document_import_in_page;
pub mod no_double_equals;
pub mod no_duplicate_case;
pub mod no_duplicate_class_members;
pub mod no_duplicate_else_if;
pub mod no_duplicate_jsx_props;
pub mod no_duplicate_object_keys;
pub mod no_duplicate_parameters;
pub mod no_duplicate_test_hooks;
pub mod no_empty_block_statements;
pub mod no_empty_interface;
pub mod no_evolving_types;
pub mod no_explicit_any;
pub mod no_exports_in_test;
pub mod no_extra_non_null_assertion;
pub mod no_fallthrough_switch_clause;
pub mod no_focused_tests;
pub mod no_function_assign;
pub mod no_global_assign;
pub mod no_global_is_finite;
pub mod no_global_is_nan;
pub mod no_head_import_in_document;
pub mod no_implicit_any_let;
pub mod no_import_assign;
pub mod no_irregular_whitespace;
pub mod no_label_var;
pub mod no_misleading_character_class;
pub mod no_misleading_instantiator;
pub mod no_misplaced_assertion;
pub mod no_misrefactored_shorthand_assign;
pub mod no_octal_escape;
pub mod no_prototype_builtins;
pub mod no_react_specific_props;
pub mod no_redeclare;
pub mod no_redundant_use_strict;
pub mod no_self_compare;
pub mod no_shadow_restricted_names;
pub mod no_skipped_tests;
pub mod no_sparse_array;
pub mod no_suspicious_semicolon_in_jsx;
pub mod no_template_curly_in_string;
pub mod no_then_property;
pub mod no_unsafe_declaration_merging;
pub mod no_unsafe_negation;
pub mod no_var;
pub mod no_with;
pub mod use_adjacent_overload_signatures;
pub mod use_await;
pub mod use_default_switch_clause_last;
pub mod use_error_message;
pub mod use_getter_return;
pub mod use_google_font_display;
pub mod use_guard_for_in;
pub mod use_is_array;
pub mod use_namespace_keyword;
pub mod use_number_to_fixed_digits_argument;
pub mod use_strict_mode;
declare_lint_group! { pub Suspicious { name : "suspicious" , rules : [self :: no_alert :: NoAlert , self :: no_approximative_numeric_constant :: NoApproximativeNumericConstant , self :: no_array_index_key :: NoArrayIndexKey , self :: no_assign_in_expressions :: NoAssignInExpressions , self :: no_async_promise_executor :: NoAsyncPromiseExecutor , self :: no_catch_assign :: NoCatchAssign , self :: no_class_assign :: NoClassAssign , self :: no_comment_text :: NoCommentText , self :: no_compare_neg_zero :: NoCompareNegZero , self :: no_confusing_labels :: NoConfusingLabels , self :: no_confusing_void_type :: NoConfusingVoidType , self :: no_console :: NoConsole , self :: no_const_enum :: NoConstEnum , self :: no_control_characters_in_regex :: NoControlCharactersInRegex , self :: no_debugger :: NoDebugger , self :: no_document_cookie :: NoDocumentCookie , self :: no_document_import_in_page :: NoDocumentImportInPage , self :: no_double_equals :: NoDoubleEquals , self :: no_duplicate_case :: NoDuplicateCase , self :: no_duplicate_class_members :: NoDuplicateClassMembers , self :: no_duplicate_else_if :: NoDuplicateElseIf , self :: no_duplicate_jsx_props :: NoDuplicateJsxProps , self :: no_duplicate_object_keys :: NoDuplicateObjectKeys , self :: no_duplicate_parameters :: NoDuplicateParameters , self :: no_duplicate_test_hooks :: NoDuplicateTestHooks , self :: no_empty_block_statements :: NoEmptyBlockStatements , self :: no_empty_interface :: NoEmptyInterface , self :: no_evolving_types :: NoEvolvingTypes , self :: no_explicit_any :: NoExplicitAny , self :: no_exports_in_test :: NoExportsInTest , self :: no_extra_non_null_assertion :: NoExtraNonNullAssertion , self :: no_fallthrough_switch_clause :: NoFallthroughSwitchClause , self :: no_focused_tests :: NoFocusedTests , self :: no_function_assign :: NoFunctionAssign , self :: no_global_assign :: NoGlobalAssign , self :: no_global_is_finite :: NoGlobalIsFinite , self :: no_global_is_nan :: NoGlobalIsNan , self :: no_head_import_in_document :: NoHeadImportInDocument , self :: no_implicit_any_let :: NoImplicitAnyLet , self :: no_import_assign :: NoImportAssign , self :: no_irregular_whitespace :: NoIrregularWhitespace , self :: no_label_var :: NoLabelVar , self :: no_misleading_character_class :: NoMisleadingCharacterClass , self :: no_misleading_instantiator :: NoMisleadingInstantiator , self :: no_misplaced_assertion :: NoMisplacedAssertion , self :: no_misrefactored_shorthand_assign :: NoMisrefactoredShorthandAssign , self :: no_octal_escape :: NoOctalEscape , self :: no_prototype_builtins :: NoPrototypeBuiltins , self :: no_react_specific_props :: NoReactSpecificProps , self :: no_redeclare :: NoRedeclare , self :: no_redundant_use_strict :: NoRedundantUseStrict , self :: no_self_compare :: NoSelfCompare , self :: no_shadow_restricted_names :: NoShadowRestrictedNames , self :: no_skipped_tests :: NoSkippedTests , self :: no_sparse_array :: NoSparseArray , self :: no_suspicious_semicolon_in_jsx :: NoSuspiciousSemicolonInJsx , self :: no_template_curly_in_string :: NoTemplateCurlyInString , self :: no_then_property :: NoThenProperty , self :: no_unsafe_declaration_merging :: NoUnsafeDeclarationMerging , self :: no_unsafe_negation :: NoUnsafeNegation , self :: no_var :: NoVar , self :: no_with :: NoWith , self :: use_adjacent_overload_signatures :: UseAdjacentOverloadSignatures , self :: use_await :: UseAwait , self :: use_default_switch_clause_last :: UseDefaultSwitchClauseLast , self :: use_error_message :: UseErrorMessage , self :: use_getter_return :: UseGetterReturn , self :: use_google_font_display :: UseGoogleFontDisplay , self :: use_guard_for_in :: UseGuardForIn , self :: use_is_array :: UseIsArray , self :: use_namespace_keyword :: UseNamespaceKeyword , self :: use_number_to_fixed_digits_argument :: UseNumberToFixedDigitsArgument , self :: use_strict_mode :: UseStrictMode ,] } }
