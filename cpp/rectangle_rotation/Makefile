# Compiler and flags
CXX = g++
CXXFLAGS = -std=c++11 -Wall

# Source files
SOURCES = solution.cpp test.cpp
TARGET = test

# Build rule
default: $(TARGET)
	./test
	rm test

$(TARGET): $(SOURCES)
	$(CXX) $(CXXFLAGS) -o $@ $(SOURCES)
