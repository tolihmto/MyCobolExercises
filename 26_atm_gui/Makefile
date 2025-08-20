# Project: 26_atm_gui
# Build both the COBOL backend and the C++ SFML GUI

# Tools
CXX ?= g++
COB ?= cobc

# Flags
CXXFLAGS ?= -O2 -std=c++17
# Try to get SFML flags from pkg-config; fallback to manual libs if not available
SFML_CFLAGS ?= $(shell pkg-config --cflags sfml-graphics sfml-window sfml-system 2>/dev/null)
SFML_LIBS   ?= $(shell pkg-config --libs   sfml-graphics sfml-window sfml-system 2>/dev/null)
ifeq ($(strip $(SFML_LIBS)),)
  SFML_LIBS = -lsfml-graphics -lsfml-window -lsfml-system
endif

# Targets
.PHONY: all gui backend run clean

all: backend gui

backend: atm_backend

gui: atm_gui

atm_backend: atm_backend.cbl
	$(COB) -x -o $@ $<

atm_gui: atm_gui.cpp
	$(CXX) $(CXXFLAGS) $(SFML_CFLAGS) $< -o $@ $(SFML_LIBS)

run: all
	./atm_gui

clean:
	rm -f atm_backend atm_gui
