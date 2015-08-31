;;; Tests of conditions
(compile-and-load "ANSI-TESTS:AUX;types-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;define-condition-aux.lsp")

(load "conditions/condition.lsp")
(load "conditions/cell-error-name.lsp")
(load "conditions/assert.lsp")
(load "conditions/error.lsp")
(load "conditions/cerror.lsp")
(load "conditions/check-type.lsp")
(load "conditions/warn.lsp")
(load "conditions/invoke-debugger.lsp")
(load "conditions/handler-bind.lsp")
(load "conditions/handler-case.lsp")
(load "conditions/ignore-errors.lsp")
(load "conditions/define-condition.lsp")
(load "conditions/compute-restarts.lsp")
(load "conditions/restart-bind.lsp")
(load "conditions/restart-case.lsp")
(load "conditions/with-condition-restarts.lsp")
(load "conditions/with-simple-restart.lsp")
(load "conditions/abort.lsp")
(load "conditions/muffle-warning.lsp")
(load "conditions/continue.lsp")
(load "conditions/store-value.lsp")
(load "conditions/use-value.lsp")
(load "conditions/make-condition.lsp")
