(in-package :cl-test)

(setf rt::*expected-failures*
      #+linux
      '(GENSYM.ERROR.10
	GENSYM.ERROR.11
	LAMBDA.52
	LAMBDA.53
	LAMBDA.54
	DEFINE-COMPILER-MACRO.8
	DEFMACRO.4
	SHIFTF.7
	CONSTANTLY.ERROR.2
	DEFUN.5
	DEFUN.6
	DEFUN.7
	FLET.62
	FLET.63
	FLET.67
	LABELS.37
	LABELS.38
	LABELS.39
	LABELS.40
	LABELS.41
	LABELS.42
	LABELS.46
	LET.17
	LET.17A
	LET*.17
	LET*.17A
	LET*.18
	PROG.11
	PROG*.11
	DO.16
	DO*.16
	DOTIMES.23
	DOTIMES.23A
	LOOP.1.40
	LOOP.1.41
	LOOP.1.42
	LOOP.1.43
	DEFCLASS.ERROR.9
	DEFCLASS.ERROR.10
	DEFCLASS.ERROR.11
	HANDLER-CASE.29
	RESTART-CASE.36
	MAKE-CONDITION.3
	MAKE-CONDITION.4
	UPGRADED-ARRAY-ELEMENT-TYPE.8
	UPGRADED-ARRAY-ELEMENT-TYPE.NIL.1
	DEFPACKAGE.2B
	EXP.ERROR.8
	EXP.ERROR.9
	EXP.ERROR.10
	EXP.ERROR.11
	EXPT.ERROR.8
	EXPT.ERROR.9
	EXPT.ERROR.10
	EXPT.ERROR.11
	EXPT.29
	RANDOM.ERROR.3
	IMAGPART.4
	UPGRADED-COMPLEX-PART-TYPE.1
	UPGRADED-COMPLEX-PART-TYPE.2
	UPGRADED-COMPLEX-PART-TYPE.9
	MAKE-SEQUENCE.ERROR.15
	MAP.48
	MAP.ERROR.11
	NREVERSE-STRING.5
	FIND.ERROR.4
	FIND-IF.ERROR.4
	FIND-IF-NOT.ERROR.4
	POSITION.ERROR.4
	POSITION-IF.ERROR.4
	POSITION-IF-NOT.ERROR.4
	DEFSTRUCT.ERROR.3
	DEFSTRUCT.ERROR.4
	ALL-STRUCTURE-CLASSES-ARE-SUBTYPES-OF-STRUCTURE-OBJECT.2
	SUBTYPEP.CONS.29
	SUBTYPEP.CONS.30
	SUBTYPEP.CONS.31
	SUBTYPEP.CONS.32
	SUBTYPEP.CONS.33
	SUBTYPEP.CONS.34
	SUBTYPEP.CONS.43
	SUBTYPEP-COMPLEX.8
	DEFTYPE.15
	CHARACTER.1
	CHAR-UPCASE.2
	CHAR-DOWNCASE.2
	BOTH-CASE-P.2
	CHAR-NOT-GREATERP.3
	CHAR-LESSP.3
	CHAR-NOT-LESSP.3
	CHAR-GREATERP.3
	ENSURE-DIRECTORIES-EXIST.8
	RENAME-FILE.1
	RENAME-FILE.2
	RENAME-FILE.6
	RENAME-FILE.7
	READ-SEQUENCE.ERROR.7
	WRITE-SEQUENCE.ERROR.3
	WITH-OPEN-FILE.7
	WITH-OPEN-FILE.8
	WITH-OPEN-FILE.9
	WITH-OPEN-STREAM.10
	LISTEN.ERROR.1
	CLEAR-INPUT.ERROR.1
	CLEAR-INPUT.ERROR.2
	BROADCAST-STREAM-STREAMS.3
	WITH-INPUT-FROM-STRING.20
	WITH-INPUT-FROM-STRING.21
	WITH-OUTPUT-TO-STRING.15
	WITH-OUTPUT-TO-STRING.16
	PRINT.BACKQUOTE.RANDOM.1
	PRINT.BACKQUOTE.RANDOM.2
	PRINT.BACKQUOTE.RANDOM.3
	PRINT.BACKQUOTE.RANDOM.6
	PRINT.BACKQUOTE.RANDOM.7
	PRINT.BACKQUOTE.RANDOM.8
	PRINT.BACKQUOTE.RANDOM.9
	PRINT.BACKQUOTE.RANDOM.12
	PRINT.BACKQUOTE.RANDOM.14
	PRINT.ARRAY.LEVEL.8
	PRINT-UNREADABLE-OBJECT.2
	PRINT-UNREADABLE-OBJECT.3
	PRINT-LEVEL.8
	PRINT-LEVEL.9
	FORMAT.F.5
	FORMAT.F.8
	FORMAT.F.45
	FORMATTER.F.45
	FORMAT.F.46
	FORMATTER.F.46
	FORMAT.F.46B
	FORMATTER.F.46B
	FORMAT.F.47
	FORMATTER.F.47
	FORMAT.E.4
	FORMAT.E.5
	FORMAT.E.6
	FORMAT.E.7
	FORMAT.E.8
	FORMAT.E.9
	FORMAT.E.19
	FORMAT.E.20
	FORMAT.E.26
	READ-SYMBOL.13
	READ-SYMBOL.14
	READTABLE-CASE.ERROR.3
	READTABLE-CASE.ERROR.4
	READTABLE-CASE.ERROR.5
	SYNTAX.ESCAPED.2
	SYNTAX.ESCAPED.3
	SYNTAX.ESCAPED.5
	SYNTAX.ESCAPED.6
	SYNTAX.SHARP-COLON.ERROR.1
	SYNTAX.SHARP-S.1
	SYNTAX.SHARP-S.2
	SYNTAX.SHARP-S.3
	SYNTAX.SHARP-S.4
	SYNTAX.SHARP-S.5
	SYNTAX.SHARP-S.6
	SYNTAX.SHARP-S.7
	SYNTAX.SHARP-S.8
	SYNTAX.SHARP-S.9
	SYNTAX.SHARP-S.10
	COMPILE-FILE.2
	COMPILE-FILE.10
	COMPILE-FILE.19
	APROPOS.1
	APROPOS.2
	APROPOS.3
	APROPOS.4
	APROPOS.5
	APROPOS.6
	APROPOS.7
	APROPOS.8
	APROPOS.9
	APROPOS.10
	APROPOS-LIST.4
	APROPOS-LIST.5
	APROPOS-LIST.6
	APROPOS-LIST.7
	APROPOS-LIST.8
	APROPOS-LIST.9
	ROOM.4
	TRACE.8
	MISC.64
	MISC.78
	MISC.79
	MISC.79A
	MISC.80
	MISC.130
	MISC.131
	MISC.132A
	MISC.133
	MISC.144
	MISC.146
	MISC.150C
	MISC.151A
	MISC.161
	MISC.162
	MISC.166
	MISC.168
	MISC.182
	MISC.183
	MISC.185
	MISC.185A
	MISC.186
	MISC.187
	MISC.194
	MISC.195
	MISC.196
	MISC.197
	MISC.231
	MISC.232
	MISC.233
	MISC.236
	MISC.249
	MISC.250
	MISC.251
	MISC.252
	MISC.252A
	MISC.254
	MISC.255
	MISC.274A
	MISC.289
	MISC.295
	MISC.363
	MISC.383
	MISC.395
	MISC.396
	MISC.397
	MISC.398
	MISC.402
	MISC.403
	MISC.414
	MISC.415
	MISC.421
	MISC.424
	MISC.580
	MISC.598
	MISC.618
	MISC.642
	MISC.643
	MISC.644
	MISC.645
	MISC.646
	MISC.647
	MISC.648)
      #+darwin
      '(GENSYM.ERROR.10
	GENSYM.ERROR.11
	LAMBDA.52
	LAMBDA.53
	LAMBDA.54
	DEFINE-COMPILER-MACRO.8
	DEFMACRO.4
	SHIFTF.7
	CONSTANTLY.ERROR.2
	DEFUN.5
	DEFUN.6
	DEFUN.7
	FLET.62
	FLET.63
	FLET.67
	LABELS.37
	LABELS.38
	LABELS.39
	LABELS.40
	LABELS.41
	LABELS.42
	LABELS.46
	LET.17
	LET.17A
	LET*.17
	LET*.17A
	LET*.18
	PROG.11
	PROG*.11
	DO.16
	DO*.16
	DOTIMES.23
	DOTIMES.23A
	LOOP.1.40
	LOOP.1.41
	LOOP.1.42
	LOOP.1.43
	DEFCLASS.ERROR.9
	DEFCLASS.ERROR.10
	DEFCLASS.ERROR.11
	HANDLER-CASE.29
	RESTART-CASE.36
	MAKE-CONDITION.3
	MAKE-CONDITION.4
	UPGRADED-ARRAY-ELEMENT-TYPE.8
	UPGRADED-ARRAY-ELEMENT-TYPE.NIL.1
	DEFPACKAGE.2B
	EXP.ERROR.8
	EXP.ERROR.9
	EXP.ERROR.10
	EXP.ERROR.11
	EXPT.ERROR.8
	EXPT.ERROR.9
	EXPT.ERROR.10
	EXPT.ERROR.11
	EXPT.29
	RANDOM.ERROR.3
	IMAGPART.4
	UPGRADED-COMPLEX-PART-TYPE.1
	UPGRADED-COMPLEX-PART-TYPE.2
	UPGRADED-COMPLEX-PART-TYPE.9
	MAKE-SEQUENCE.ERROR.15
	MAP.48
	MAP.ERROR.11
	NREVERSE-STRING.5
	FIND.ERROR.4
	FIND-IF.ERROR.4
	FIND-IF-NOT.ERROR.4
	POSITION.ERROR.4
	POSITION-IF.ERROR.4
	POSITION-IF-NOT.ERROR.4
	DEFSTRUCT.ERROR.3
	DEFSTRUCT.ERROR.4
	ALL-STRUCTURE-CLASSES-ARE-SUBTYPES-OF-STRUCTURE-OBJECT.2
	SUBTYPEP.CONS.29
	SUBTYPEP.CONS.30
	SUBTYPEP.CONS.31
	SUBTYPEP.CONS.32
	SUBTYPEP.CONS.33
	SUBTYPEP.CONS.34
	SUBTYPEP.CONS.43
	SUBTYPEP-COMPLEX.8
	DEFTYPE.15
	CHARACTER.1
	CHAR-UPCASE.2
	CHAR-DOWNCASE.2
	BOTH-CASE-P.2
	CHAR-NOT-GREATERP.3
	CHAR-LESSP.3
	CHAR-NOT-LESSP.3
	CHAR-GREATERP.3
	ENSURE-DIRECTORIES-EXIST.8
	READ-SEQUENCE.ERROR.7
	WRITE-SEQUENCE.ERROR.3
	WITH-OPEN-FILE.7
	WITH-OPEN-FILE.8
	WITH-OPEN-FILE.9
	WITH-OPEN-STREAM.10
	LISTEN.ERROR.1
	CLEAR-INPUT.ERROR.1
	CLEAR-INPUT.ERROR.2
	BROADCAST-STREAM-STREAMS.3
	WITH-INPUT-FROM-STRING.20
	WITH-INPUT-FROM-STRING.21
	WITH-OUTPUT-TO-STRING.15
	WITH-OUTPUT-TO-STRING.16
	PRINT.BACKQUOTE.RANDOM.1
	PRINT.BACKQUOTE.RANDOM.2
	PRINT.BACKQUOTE.RANDOM.3
	PRINT.BACKQUOTE.RANDOM.6
	PRINT.BACKQUOTE.RANDOM.7
	PRINT.BACKQUOTE.RANDOM.8
	PRINT.BACKQUOTE.RANDOM.9
	PRINT.BACKQUOTE.RANDOM.12
	PRINT.BACKQUOTE.RANDOM.14
	PRINT.ARRAY.LEVEL.8
	PRINT-UNREADABLE-OBJECT.2
	PRINT-UNREADABLE-OBJECT.3
	PRINT-LEVEL.8
	PRINT-LEVEL.9
	FORMAT.F.5
	FORMAT.F.8
	FORMAT.F.45
	FORMATTER.F.45
	FORMAT.F.46
	FORMATTER.F.46
	FORMAT.F.46B
	FORMATTER.F.46B
	FORMAT.F.47
	FORMATTER.F.47
	FORMAT.E.4
	FORMAT.E.5
	FORMAT.E.6
	FORMAT.E.7
	FORMAT.E.8
	FORMAT.E.9
	FORMAT.E.19
	FORMAT.E.20
	FORMAT.E.26
	READ-SYMBOL.13
	READ-SYMBOL.14
	READTABLE-CASE.ERROR.3
	READTABLE-CASE.ERROR.4
	READTABLE-CASE.ERROR.5
	SYNTAX.ESCAPED.2
	SYNTAX.ESCAPED.3
	SYNTAX.ESCAPED.5
	SYNTAX.ESCAPED.6
	SYNTAX.SHARP-COLON.ERROR.1
	SYNTAX.SHARP-S.1
	SYNTAX.SHARP-S.2
	SYNTAX.SHARP-S.3
	SYNTAX.SHARP-S.4
	SYNTAX.SHARP-S.5
	SYNTAX.SHARP-S.6
	SYNTAX.SHARP-S.7
	SYNTAX.SHARP-S.8
	SYNTAX.SHARP-S.9
	SYNTAX.SHARP-S.10
	COMPILE-FILE.2
	COMPILE-FILE.10
	COMPILE-FILE.19
	APROPOS.1
	APROPOS.2
	APROPOS.3
	APROPOS.4
	APROPOS.5
	APROPOS.6
	APROPOS.7
	APROPOS.8
	APROPOS.9
	APROPOS.10
	APROPOS-LIST.4
	APROPOS-LIST.5
	APROPOS-LIST.6
	APROPOS-LIST.7
	APROPOS-LIST.8
	APROPOS-LIST.9
	ROOM.4
	TRACE.8
	DEFSETF.7A
	FILE-STREAM-CPL
	PATHNAME.3
	PATHNAME.4
	LOGICAL-PATHNAME.ERROR.3
	FILE-NAMESTRING.3
	DIRECTORY-NAMESTRING.3
	HOST-NAMESTRING.3
	ENOUGH-NAMESTRING.4
	PROBE-FILE.2
	PROBE-FILE.3
	PROBE-FILE.4
	PROBE-FILE.5
	ENSURE-DIRECTORIES-EXIST.1
	ENSURE-DIRECTORIES-EXIST.2
	ENSURE-DIRECTORIES-EXIST.3
	ENSURE-DIRECTORIES-EXIST.4
	ENSURE-DIRECTORIES-EXIST.5
	ENSURE-DIRECTORIES-EXIST.6
	ENSURE-DIRECTORIES-EXIST.7
	ENSURE-DIRECTORIES-EXIST.9
	TRUENAME.1
	TRUENAME.2
	TRUENAME.3
	TRUENAME.4
	TRUENAME.6
	FILE-AUTHOR.2
	FILE-AUTHOR.3
	FILE-AUTHOR.4
	FILE-AUTHOR.5
	FILE-AUTHOR.6
	FILE-AUTHOR.7
	FILE-WRITE-DATE.3
	FILE-ERROR-PATHNAME.5
	FILE-ERROR-PATHNAME.6
	INPUT-STREAM-P.3
	OUTPUT-STREAM-P.3
	OPEN-STREAM-P.2
	OPEN-STREAM-P.4
	FILE-LENGTH.ERROR.2
	FILE-LENGTH.ERROR.5
	FILE-LENGTH.ERROR.6
	FILE-LENGTH.ERROR.10
	FILE-LENGTH.1
	FILE-LENGTH.6
	FILE-POSITION.1
	FILE-POSITION.2
	FILE-POSITION.3
	FILE-POSITION.4
	LISTEN.7
	PRINT.SYMBOL.PREFIX.6B
	#:READTABLE-CASE.CASE-UPCASE
	#:READTABLE-CASE.CASE-DOWNCASE
	#:READTABLE-CASE.CASE-PRESERVE
	#:READTABLE-CASE.CASE-INVERT
	COMPILE-FILE.1
	COMPILE-FILE.2A
	COMPILE-FILE.3
	COMPILE-FILE.4
	COMPILE-FILE.5
	COMPILE-FILE.6
	COMPILE-FILE.6A
	COMPILE-FILE.7
	COMPILE-FILE.8
	COMPILE-FILE.9
	COMPILE-FILE.11
	COMPILE-FILE.12
	COMPILE-FILE.13
	COMPILE-FILE.14
	COMPILE-FILE.15
	COMPILE-FILE.16
	COMPILE-FILE.18
	COMPILE-FILE.20
	LOAD.1
	LOAD.2
	LOAD.4
	LOAD.5
	LOAD.6
	LOAD.7
	LOAD.9
	LOAD.10
	LOAD.11
	LOAD.12
	LOAD.17
	LOAD.18
	MODULES.7
	MODULES.8
	MISC.64
	MISC.78
	MISC.79
	MISC.79A
	MISC.80
	MISC.130
	MISC.131
	MISC.132A
	MISC.133
	MISC.144
	MISC.146
	MISC.150C
	MISC.151A
	MISC.161
	MISC.162
	MISC.166
	MISC.168
	MISC.182
	MISC.183
	MISC.185
	MISC.185A
	MISC.186
	MISC.187
	MISC.194
	MISC.195
	MISC.196
	MISC.197
	MISC.231
	MISC.232
	MISC.233
	MISC.236
	MISC.249
	MISC.250
	MISC.251
	MISC.252
	MISC.252A
	MISC.254
	MISC.255
	MISC.274A
	MISC.289
	MISC.295
	MISC.363
	MISC.383
	MISC.395
	MISC.396
	MISC.397
	MISC.398
	MISC.402
	MISC.403
	MISC.414
	MISC.415
	MISC.421
	MISC.424
	MISC.580
	MISC.598
	MISC.618
	MISC.642
	MISC.643
	MISC.644
	MISC.645
	MISC.646
	MISC.647
	MISC.648))
