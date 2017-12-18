.PHONY: release

TAG ?= $(shell git tag --points-at HEAD | head -q -n1)
ifeq ($(TAG),)
$(error You must specify TAG with the name of an existing tag)
endif

FPC ?= $(shell which fpc)
ifeq ($(FPC),)
$(error You must specify FPC with full path to compiler)
endif

TARGET_CPU := $(shell $(FPC) -iTP)
TARGET_OS := $(shell $(FPC) -iTO)

OUTFILE := androli-$(TAG)-$(TARGET_CPU)-$(TARGET_OS)

ifneq (,$(filter $(TARGET_OS),win32 win64))
EXT = .exe
else
EXT =
endif

OUTDIR := $(shell mktemp -d)

release:
	rm -rf "$(OUTDIR)"
	git clone -q --single-branch -b $(TAG) --shared . "$(OUTDIR)"
	cd "$(OUTDIR)" && lazbuild Androli.lpi --bm=Release --os=$(TARGET_OS) --cpu=$(TARGET_CPU) --compiler=$(FPC)
	cd "$(OUTDIR)" && mv androli$(EXT) $(OUTFILE)$(EXT)
	cd "$(OUTDIR)" && zip -r $(OUTFILE).zip dumps media README.md LICENSE $(OUTFILE)$(EXT)
	mv -f "$(OUTDIR)/$(OUTFILE).zip" ./

