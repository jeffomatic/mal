SRC_FILES := $(wildcard *.ml)
STEPS := $(patsubst %.ml,%, $(SRC_FILES))
BUILD_DIR := _build/default/
EXECUTABLES := $(addprefix $(BUILD_DIR), $(SRC_FILES:.ml=.exe))

.PHONY: $(STEPS)

$(EXECUTABLES): $(BUILD_DIR)%.exe: %.ml dune
	dune build $@

$(STEPS): %: $(BUILD_DIR)%.exe
