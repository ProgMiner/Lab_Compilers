
HC = ghc
LD = ghc
HFLAGS =
LDFLAGS =

BUILDPATH = build
SOURCES = Main.hs
TARGET = hw2

OBJECTS = $(SOURCES:%.hs=$(BUILDPATH)/%.o)

.PHONY: all build clean modules
.SUFFIXES:

all: build

clean:
	@rm -vrf $(BUILDPATH) 2> /dev/null; true
	@rm -v $(TARGET) 2> /dev/null; true

build: $(TARGET)

%.hs:

$(OBJECTS): $(BUILDPATH)/%.o : %.hs
	@mkdir -p $(@D)
	$(HC) -c -hidir $(BUILDPATH) -o $@ $< $(HFLAGS)

$(TARGET): $(OBJECTS)
	$(LD) -o $@ $^ $(LDFLAGS)
