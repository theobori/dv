EMACS ?= emacs
EMACS_Q_ARG = -Q
EMACS_BATCH = ${EMACS} ${EMACS_Q_ARG} --batch
EMACS_EVAL = ${EMACS_BATCH} --eval
EMACS_USER_DIR != ${EMACS_EVAL} "(princ (string-trim-right user-emacs-directory \"/\"))"

PACKAGE_NAME = dv

INSTALL_DIR = ${EMACS_USER_DIR}/manual-packages/${PACKAGE_NAME}

SOURCE_FILES = dv.el
OBJECT_FILES = ${SOURCE_FILES:.el=.elc}
FILES = ${SOURCE_FILES} ${OBJECT_FILES}
TEST_FILES = test/dv-test.elc

LOAD_FILES_FOR_TEST = ${foreach file, ${OBJECT_FILES} ${TEST_FILES}, --load ${file}}

.PHONY: all
all: build

.PHONY: build
build: ${OBJECT_FILES}

.PHONY: install
install: build
	mkdir -p ${INSTALL_DIR}
	cp -rv ${FILES} ${INSTALL_DIR}

.PHONY: test
test: ${TEST_FILES} ${OBJECT_FILES}
	${EMACS_BATCH} --load ert ${LOAD_FILES_FOR_TEST} -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	${RM} ${OBJECT_FILES} ${TEST_FILES}

.PHONY: nuke
nuke: clean
	${RM} -r ${INSTALL_DIR}

%.elc: %.el
	${EMACS_BATCH} --directory . --eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" $<