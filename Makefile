.PHONY: release


ifndef RELEASE_VERSION
$(error You must define RELEASE_VERSION and a tag named vRELEASE_VERSION must exist.)
endif

TARGET_CPU ?= i386
TARGET_OS ?= win32

OUTFILE_NAME := androli-$(RELEASE_VERSION)-$(TARGET_CPU)-$(TARGET_OS)

ifneq (,$(filter $(TARGET_OS),win32 win64))
OUTFILE_EXT = .exe
else
OUTFILE_EXT =
endif

CLONE_DIR := $(TMP)/androli

release:
	rm -rf "$(CLONE_DIR)"
	git clone -q --single-branch -b v$(RELEASE_VERSION) . "$(CLONE_DIR)"
	cd "$(CLONE_DIR)" && lazbuild Androli.lpi --bm=Release --os=$(TARGET_OS) --cpu=$(TARGET_CPU)
	cd "$(CLONE_DIR)" && mv androli$(OUTFILE_EXT) $(OUTFILE_NAME)$(OUTFILE_EXT)
	cd "$(CLONE_DIR)" && zip -r $(OUTFILE_NAME).zip dumps media README.md LICENSE $(OUTFILE_NAME)$(OUTFILE_EXT)
	mv "$(CLONE_DIR)/$(OUTFILE_NAME).zip" ./

